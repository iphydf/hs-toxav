#include "video.h"

#include <stdlib.h>
#include <string.h>

#include <libavcodec/avcodec.h>
#include <libavutil/frame.h>
#include <libavutil/opt.h>

struct Hstoxav_Video {
    AVCodecContext *ctx;
    AVFrame *frame;
    AVPacket *packet;
};

static Hstoxav_Video *video_new(const AVCodec *codec)
{
    if (codec == NULL) {
        return NULL;
    }
    Hstoxav_Video *video = calloc(1, sizeof(Hstoxav_Video));
    if (video == NULL) {
        return NULL;
    }
    video->ctx = avcodec_alloc_context3(codec);
    video->frame = av_frame_alloc();
    video->packet = av_packet_alloc();
    if (video->ctx == NULL || video->frame == NULL || video->packet == NULL) {
        hstoxav_video_free(video);
        return NULL;
    }
    return video;
}

void hstoxav_video_free(Hstoxav_Video *video)
{
    if (video == NULL) {
        return;
    }
    avcodec_free_context(&video->ctx);
    av_frame_free(&video->frame);
    av_packet_free(&video->packet);
    free(video);
}

Hstoxav_Video *hstoxav_video_encoder_new(const char *name, int width, int height, int64_t bit_rate, int gop)
{
    const AVCodec *codec = avcodec_find_encoder_by_name(name);
    Hstoxav_Video *video = video_new(codec);
    if (video == NULL) {
        return NULL;
    }
    AVCodecContext *ctx = video->ctx;
    ctx->width = width;
    ctx->height = height;
    ctx->pix_fmt = AV_PIX_FMT_YUV420P;
    ctx->bit_rate = bit_rate;
    ctx->time_base = (AVRational) {1, 1000};
    // A guess for encoders that pace their bit rate by it.
    ctx->framerate = (AVRational) {30, 1};
    ctx->gop_size = gop;
    ctx->max_b_frames = 0;
    ctx->thread_count = 4;
    if (strcmp(name, "libvpx") == 0) {
        av_opt_set(ctx->priv_data, "deadline", "realtime", 0);
        av_opt_set_int(ctx->priv_data, "cpu-used", 16, 0);
        av_opt_set_int(ctx->priv_data, "lag-in-frames", 0, 0);
        av_opt_set(ctx->priv_data, "error-resilient", "default+partitions", 0);
        // Phone cameras are noisy, and noise costs bits.
        av_opt_set_int(ctx->priv_data, "noise-sensitivity", 2, 0);
    } else if (strcmp(name, "libx264") == 0 || strcmp(name, "libx265") == 0) {
        av_opt_set(ctx->priv_data, "preset", "ultrafast", 0);
        av_opt_set(ctx->priv_data, "tune", "zerolatency", 0);
    }
    if (strcmp(name, "libx264") == 0) {
        av_opt_set(ctx->priv_data, "profile", "baseline", 0);
    }
    video->frame->format = ctx->pix_fmt;
    video->frame->width = width;
    video->frame->height = height;
    if (avcodec_open2(ctx, codec, NULL) < 0 || av_frame_get_buffer(video->frame, 0) < 0) {
        hstoxav_video_free(video);
        return NULL;
    }
    return video;
}

Hstoxav_Video *hstoxav_video_decoder_new(const char *name, int64_t max_pixels)
{
    const AVCodec *codec = avcodec_find_decoder_by_name(name);
    Hstoxav_Video *video = video_new(codec);
    if (video == NULL) {
        return NULL;
    }
    video->ctx->thread_count = 4;
    // Frame threads would hold pictures back until more frames arrive.
    video->ctx->thread_type = FF_THREAD_SLICE;
    // Larger frames fail before the decoder allocates them.
    video->ctx->max_pixels = max_pixels;
    if (avcodec_open2(video->ctx, codec, NULL) < 0) {
        hstoxav_video_free(video);
        return NULL;
    }
    return video;
}

static void copy_plane(uint8_t *dst, int dst_stride, const uint8_t *src, int width, int height)
{
    for (int row = 0; row < height; ++row) {
        memcpy(dst + row * dst_stride, src + row * width, width);
    }
}

int hstoxav_video_send_frame(Hstoxav_Video *video, const uint8_t *y, const uint8_t *u, const uint8_t *v,
                             int64_t pts, int key)
{
    AVFrame *frame = video->frame;
    int ret = av_frame_make_writable(frame);
    if (ret < 0) {
        return ret;
    }
    const int width = frame->width;
    const int height = frame->height;
    const int chroma_width = (width + 1) / 2;
    const int chroma_height = (height + 1) / 2;
    copy_plane(frame->data[0], frame->linesize[0], y, width, height);
    copy_plane(frame->data[1], frame->linesize[1], u, chroma_width, chroma_height);
    copy_plane(frame->data[2], frame->linesize[2], v, chroma_width, chroma_height);
    frame->pts = pts;
    frame->pict_type = key ? AV_PICTURE_TYPE_I : AV_PICTURE_TYPE_NONE;
#ifdef AV_FRAME_FLAG_KEY
    if (key) {
        frame->flags |= AV_FRAME_FLAG_KEY;
    } else {
        frame->flags &= ~AV_FRAME_FLAG_KEY;
    }
#else
    frame->key_frame = key;
#endif
    return avcodec_send_frame(video->ctx, frame);
}

int hstoxav_video_receive_packet(Hstoxav_Video *video, const uint8_t **data, int *size, int *key, int64_t *pts)
{
    av_packet_unref(video->packet);
    const int ret = avcodec_receive_packet(video->ctx, video->packet);
    if (ret == AVERROR(EAGAIN)) {
        return 1;
    }
    if (ret < 0) {
        return ret;
    }
    *data = video->packet->data;
    *size = video->packet->size;
    *key = (video->packet->flags & AV_PKT_FLAG_KEY) != 0;
    *pts = video->packet->pts;
    return 0;
}

int hstoxav_video_send_packet(Hstoxav_Video *video, const uint8_t *data, int size, int64_t pts)
{
    AVPacket *packet = video->packet;
    av_packet_unref(packet);
    const int ret = av_new_packet(packet, size);
    if (ret < 0) {
        return ret;
    }
    memcpy(packet->data, data, size);
    packet->pts = pts;
    return avcodec_send_packet(video->ctx, packet);
}

int hstoxav_video_receive_frame(Hstoxav_Video *video, int *width, int *height, const uint8_t **y,
                                const uint8_t **u, const uint8_t **v, int *y_stride, int *u_stride,
                                int *v_stride, int64_t *pts)
{
    AVFrame *frame = video->frame;
    av_frame_unref(frame);
    const int ret = avcodec_receive_frame(video->ctx, frame);
    if (ret == AVERROR(EAGAIN)) {
        return 1;
    }
    if (ret < 0) {
        return ret;
    }
    if (frame->format != AV_PIX_FMT_YUV420P && frame->format != AV_PIX_FMT_YUVJ420P) {
        return 2;
    }
    *width = frame->width;
    *height = frame->height;
    *y = frame->data[0];
    *u = frame->data[1];
    *v = frame->data[2];
    *y_stride = frame->linesize[0];
    *u_stride = frame->linesize[1];
    *v_stride = frame->linesize[2];
    *pts = frame->pts;
    return 0;
}
