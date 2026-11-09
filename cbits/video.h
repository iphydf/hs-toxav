#ifndef HS_TOXAV_CBITS_VIDEO_H
#define HS_TOXAV_CBITS_VIDEO_H

#include <stdint.h>

typedef struct Hstoxav_Video Hstoxav_Video;

/* A libavcodec encoder by name, taking I420 frames with timestamps in ms. */
Hstoxav_Video *hstoxav_video_encoder_new(const char *name, int width, int height, int64_t bit_rate, int gop);
/* A libavcodec decoder by name, giving I420 frames. */
Hstoxav_Video *hstoxav_video_decoder_new(const char *name, int64_t max_pixels);
void hstoxav_video_free(Hstoxav_Video *video);

int hstoxav_video_send_frame(Hstoxav_Video *video, const uint8_t *y, const uint8_t *u, const uint8_t *v,
                             int64_t pts, int key);
/* 0 with a packet that stays valid until the next call, 1 for none ready, negative on error. */
int hstoxav_video_receive_packet(Hstoxav_Video *video, const uint8_t **data, int *size, int *key, int64_t *pts);

int hstoxav_video_send_packet(Hstoxav_Video *video, const uint8_t *data, int size, int64_t pts);
/* 0 with a frame whose planes stay valid until the next call, 1 for none ready, 2 for a frame
 * that isn't I420, negative on error. */
int hstoxav_video_receive_frame(Hstoxav_Video *video, int *width, int *height, const uint8_t **y,
                                const uint8_t **u, const uint8_t **v, int *y_stride, int *u_stride,
                                int *v_stride, int64_t *pts);

#endif /* HS_TOXAV_CBITS_VIDEO_H */
