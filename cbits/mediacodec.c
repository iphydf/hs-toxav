#include "mediacodec.h"

#include <dlfcn.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <media/NdkMediaCodec.h>
#include <media/NdkMediaFormat.h>

/* MediaCodecInfo.CodecCapabilities' colour formats that say where each row is: semi-planar
 * (NV12) or planar (I420), at the stride and slice height the encoder asks for. */
enum {
    COLOR_FORMAT_I420 = 19,
    COLOR_FORMAT_NV12 = 21,
};

/* MediaCodec.BUFFER_FLAG_* */
enum {
    FLAG_KEY_FRAME = 1,
    FLAG_CODEC_CONFIG = 2,
};

/* MediaCodecInfo.CodecProfileLevel: constrained baseline, which every H.264 decoder takes, else
 * baseline. */
static const int32_t profiles[] = {0x10000, 1};

/* MediaCodec takes the frame rate as a hint for its bit rate. */
static const int frame_rate = 30;

/* How long to wait for a free input buffer, in µs. */
static const int64_t input_timeout = 10000;

struct Hstoxav_MediaCodec {
    AMediaCodec *codec;
    int hevc;
    int width;
    int height;
    int32_t color_format;
    int32_t stride;
    int32_t slice_height;
    uint8_t *config;
    size_t config_size;
    uint8_t *packet;
    size_t packet_capacity;
    int64_t sent; /* Frames queued. */
    char error[256]; /* What went wrong last. */
};

/* Android 9 on (earlier ones don't say how they want frames laid out). */
typedef AMediaFormat *(*Get_Input_Format)(AMediaCodec *codec);
typedef media_status_t (*Get_Name)(AMediaCodec *codec, char **name);
typedef void (*Release_Name)(AMediaCodec *codec, char *name);

static Get_Input_Format input_format_getter(void)
{
    return (Get_Input_Format)dlsym(RTLD_DEFAULT, "AMediaCodec_getInputFormat");
}

/* Adds to what went wrong, after what's there. */
static void explain(char *why, int size, const char *format, ...)
{
    const size_t used = strlen(why);
    if (used + 3 >= (size_t)size) {
        return;
    }
    const size_t start = used == 0 ? used : used + 2;
    if (used != 0) {
        memcpy(why + used, "; ", 3);
    }
    va_list args;
    va_start(args, format);
    vsnprintf(why + start, size - start, format, args);
    va_end(args);
}

static AMediaFormat *encoder_format(const char *mime, int width, int height, int bit_rate, int gop,
                                    int32_t color_format, int32_t profile)
{
    AMediaFormat *format = AMediaFormat_new();
    AMediaFormat_setString(format, "mime", mime);
    AMediaFormat_setInt32(format, "width", width);
    AMediaFormat_setInt32(format, "height", height);
    AMediaFormat_setInt32(format, "bitrate", bit_rate);
    AMediaFormat_setInt32(format, "color-format", color_format);
    AMediaFormat_setInt32(format, "frame-rate", frame_rate);
    AMediaFormat_setInt32(format, "i-frame-interval", gop / frame_rate > 1 ? gop / frame_rate : 1);
    AMediaFormat_setInt32(format, "max-bframes", 0);
    AMediaFormat_setInt32(format, "priority", 0);
    AMediaFormat_setInt32(format, "latency", 1);
    if (profile != 0) {
        AMediaFormat_setInt32(format, "profile", profile);
    }
    return format;
}

/* Starts an encoder with the colour format and profile, and reads how it wants frames laid out. */
static AMediaCodec *start(Hstoxav_MediaCodec *video, Get_Input_Format get_input_format, const char *mime,
                          int bit_rate, int gop, int32_t color_format, int32_t profile, char *why, int why_size)
{
    const char *tried = color_format == COLOR_FORMAT_NV12 ? "NV12" : "I420";
    AMediaCodec *codec = AMediaCodec_createEncoderByType(mime);
    if (codec == NULL) {
        explain(why, why_size, "no %s encoder", mime);
        return NULL;
    }
    AMediaFormat *format = encoder_format(mime, video->width, video->height, bit_rate, gop, color_format, profile);
    const media_status_t status = AMediaCodec_configure(codec, format, NULL, NULL, AMEDIACODEC_CONFIGURE_FLAG_ENCODE);
    AMediaFormat_delete(format);
    if (status != AMEDIA_OK) {
        explain(why, why_size, "%s with profile %d: configuring failed (%d)", tried, profile, status);
        AMediaCodec_delete(codec);
        return NULL;
    }
    AMediaFormat *input = get_input_format(codec);
    if (input == NULL) {
        explain(why, why_size, "%s: no input format", tried);
        AMediaCodec_delete(codec);
        return NULL;
    }
    int32_t stride = video->width;
    int32_t slice_height = video->height;
    int32_t taken = color_format;
    AMediaFormat_getInt32(input, "stride", &stride);
    AMediaFormat_getInt32(input, "slice-height", &slice_height);
    AMediaFormat_getInt32(input, "color-format", &taken);
    AMediaFormat_delete(input);
    if (taken != color_format || stride < video->width || slice_height < video->height) {
        explain(why, why_size, "%s: took colour format %d, rows of %d, planes of %d", tried, taken, stride,
                slice_height);
        AMediaCodec_delete(codec);
        return NULL;
    }
    const media_status_t started = AMediaCodec_start(codec);
    if (started != AMEDIA_OK) {
        explain(why, why_size, "%s: starting failed (%d)", tried, started);
        AMediaCodec_delete(codec);
        return NULL;
    }
    video->color_format = color_format;
    video->stride = stride;
    video->slice_height = slice_height;
    return codec;
}

Hstoxav_MediaCodec *hstoxav_mediacodec_encoder_new(const char *mime, int width, int height, int bit_rate, int gop,
                                                   char *why, int why_size)
{
    why[0] = '\0';
    Get_Input_Format get_input_format = input_format_getter();
    if (get_input_format == NULL) {
        explain(why, why_size, "Android before 9 doesn't say how to lay out frames");
        return NULL;
    }
    Hstoxav_MediaCodec *video = calloc(1, sizeof(Hstoxav_MediaCodec));
    if (video == NULL) {
        return NULL;
    }
    video->hevc = strcmp(mime, "video/hevc") == 0;
    video->width = width;
    video->height = height;
    const int32_t color_formats[] = {COLOR_FORMAT_NV12, COLOR_FORMAT_I420};
    for (size_t c = 0; c < sizeof color_formats / sizeof color_formats[0] && video->codec == NULL; ++c) {
        if (video->hevc) {
            video->codec = start(video, get_input_format, mime, bit_rate, gop, color_formats[c], 0, why, why_size);
        }
        for (size_t p = 0; p < sizeof profiles / sizeof profiles[0] && !video->hevc && video->codec == NULL; ++p) {
            video->codec = start(video, get_input_format, mime, bit_rate, gop, color_formats[c], profiles[p], why,
                                 why_size);
        }
    }
    if (video->codec == NULL) {
        free(video);
        return NULL;
    }
    return video;
}

void hstoxav_mediacodec_free(Hstoxav_MediaCodec *video)
{
    if (video == NULL) {
        return;
    }
    AMediaCodec_stop(video->codec);
    AMediaCodec_delete(video->codec);
    free(video->config);
    free(video->packet);
    free(video);
}

void hstoxav_mediacodec_describe(const Hstoxav_MediaCodec *video, char *text, int size)
{
    Get_Name get_name = (Get_Name)dlsym(RTLD_DEFAULT, "AMediaCodec_getName");
    Release_Name release_name = (Release_Name)dlsym(RTLD_DEFAULT, "AMediaCodec_releaseName");
    char *name = NULL;
    if (get_name == NULL || release_name == NULL || get_name(video->codec, &name) != AMEDIA_OK) {
        name = NULL;
    }
    snprintf(text, size, "%s, %s %dx%d in rows of %d, planes of %d", name != NULL ? name : "MediaCodec",
             video->color_format == COLOR_FORMAT_NV12 ? "NV12" : "I420", video->width, video->height,
             video->stride, video->slice_height);
    if (name != NULL) {
        release_name(video->codec, name);
    }
}

static int set_parameter(Hstoxav_MediaCodec *video, const char *key, int32_t value)
{
    AMediaFormat *params = AMediaFormat_new();
    AMediaFormat_setInt32(params, key, value);
    const media_status_t status = AMediaCodec_setParameters(video->codec, params);
    AMediaFormat_delete(params);
    return status == AMEDIA_OK ? 0 : -1;
}

int hstoxav_mediacodec_set_bit_rate(Hstoxav_MediaCodec *video, int bit_rate)
{
    return set_parameter(video, "video-bitrate", bit_rate);
}

static void copy_rows(uint8_t *dst, int32_t dst_stride, const uint8_t *src, int width, int height)
{
    for (int row = 0; row < height; ++row) {
        memcpy(dst + row * dst_stride, src + row * width, width);
    }
}

void hstoxav_mediacodec_error(const Hstoxav_MediaCodec *video, char *text, int size)
{
    snprintf(text, size, "%s", video->error);
}

int hstoxav_mediacodec_send_frame(Hstoxav_MediaCodec *video, const uint8_t *y, const uint8_t *u, const uint8_t *v,
                                  int64_t pts, int key)
{
    /* A new encoder starts with a key frame. Asking is a hint, which some refuse. */
    if (key && video->sent > 0) {
        set_parameter(video, "request-sync", 0);
    }
    const ssize_t index = AMediaCodec_dequeueInputBuffer(video->codec, input_timeout);
    if (index == AMEDIACODEC_INFO_TRY_AGAIN_LATER) {
        return 1;
    }
    if (index < 0) {
        snprintf(video->error, sizeof video->error, "no input buffer (%zd)", index);
        return -1;
    }
    size_t capacity = 0;
    uint8_t *buffer = AMediaCodec_getInputBuffer(video->codec, index, &capacity);
    const int chroma_width = (video->width + 1) / 2;
    const int chroma_height = (video->height + 1) / 2;
    const size_t luma = (size_t)video->stride * video->slice_height;
    const int32_t chroma_stride = video->color_format == COLOR_FORMAT_NV12 ? video->stride : (video->stride + 1) / 2;
    const size_t chroma = (size_t)chroma_stride * ((video->slice_height + 1) / 2);
    const size_t size = video->color_format == COLOR_FORMAT_NV12 ? luma + chroma : luma + 2 * chroma;
    /* Past the last row we write, buffers may stop short of a whole plane. */
    const size_t needed = video->color_format == COLOR_FORMAT_NV12
                          ? luma + (size_t)(chroma_height - 1) * chroma_stride + 2 * chroma_width
                          : luma + chroma + (size_t)(chroma_height - 1) * chroma_stride + chroma_width;
    if (buffer == NULL || capacity < needed) {
        snprintf(video->error, sizeof video->error, "input buffer of %zu bytes, %zu needed", capacity, needed);
        return -1;
    }
    copy_rows(buffer, video->stride, y, video->width, video->height);
    if (video->color_format == COLOR_FORMAT_NV12) {
        for (int row = 0; row < chroma_height; ++row) {
            uint8_t *dst = buffer + luma + row * chroma_stride;
            for (int col = 0; col < chroma_width; ++col) {
                dst[2 * col] = u[row * chroma_width + col];
                dst[2 * col + 1] = v[row * chroma_width + col];
            }
        }
    } else {
        copy_rows(buffer + luma, chroma_stride, u, chroma_width, chroma_height);
        copy_rows(buffer + luma + chroma, chroma_stride, v, chroma_width, chroma_height);
    }
    const media_status_t queued =
        AMediaCodec_queueInputBuffer(video->codec, index, 0, size < capacity ? size : capacity, pts * 1000, 0);
    if (queued != AMEDIA_OK) {
        snprintf(video->error, sizeof video->error, "queueing a frame failed (%d)", queued);
        return -1;
    }
    ++video->sent;
    return 0;
}

static int has_parameter_sets(const Hstoxav_MediaCodec *video, const uint8_t *data, size_t size)
{
    const size_t header = size >= 4 && data[2] == 0 ? 4 : 3;
    if (size <= header) {
        return 0;
    }
    const uint8_t nal = data[header];
    return video->hevc ? ((nal >> 1) & 0x3f) == 32 : (nal & 0x1f) == 7;
}

int hstoxav_mediacodec_receive_packet(Hstoxav_MediaCodec *video, const uint8_t **data, int *size, int *key,
                                      int64_t *pts)
{
    for (;;) {
        AMediaCodecBufferInfo info;
        const ssize_t index = AMediaCodec_dequeueOutputBuffer(video->codec, &info, 0);
        if (index == AMEDIACODEC_INFO_TRY_AGAIN_LATER) {
            return 1;
        }
        if (index == AMEDIACODEC_INFO_OUTPUT_FORMAT_CHANGED || index == AMEDIACODEC_INFO_OUTPUT_BUFFERS_CHANGED) {
            continue;
        }
        if (index < 0) {
            snprintf(video->error, sizeof video->error, "no output buffer (%zd)", index);
            return -1;
        }
        size_t capacity = 0;
        const uint8_t *buffer = AMediaCodec_getOutputBuffer(video->codec, index, &capacity);
        if (buffer == NULL || (size_t)info.offset + info.size > capacity) {
            snprintf(video->error, sizeof video->error, "output buffer of %zu bytes for %d at %d", capacity, info.size,
                     info.offset);
            AMediaCodec_releaseOutputBuffer(video->codec, index, false);
            return -1;
        }
        const uint8_t *out = buffer + info.offset;
        if (info.flags & FLAG_CODEC_CONFIG) {
            uint8_t *config = realloc(video->config, info.size);
            if (config != NULL) {
                memcpy(config, out, info.size);
                video->config = config;
                video->config_size = info.size;
            }
            AMediaCodec_releaseOutputBuffer(video->codec, index, false);
            continue;
        }
        const int is_key = (info.flags & FLAG_KEY_FRAME) != 0;
        const size_t prefix = is_key && !has_parameter_sets(video, out, info.size) ? video->config_size : 0;
        const size_t total = prefix + info.size;
        if (total > video->packet_capacity) {
            uint8_t *packet = realloc(video->packet, total);
            if (packet == NULL) {
                snprintf(video->error, sizeof video->error, "out of memory for a frame of %zu bytes", total);
                AMediaCodec_releaseOutputBuffer(video->codec, index, false);
                return -1;
            }
            video->packet = packet;
            video->packet_capacity = total;
        }
        if (prefix > 0) {
            memcpy(video->packet, video->config, prefix);
        }
        memcpy(video->packet + prefix, out, info.size);
        AMediaCodec_releaseOutputBuffer(video->codec, index, false);
        *data = video->packet;
        *size = (int)total;
        *key = is_key;
        *pts = info.presentationTimeUs / 1000;
        return 0;
    }
}
