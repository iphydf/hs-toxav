#ifndef HS_TOXAV_CBITS_MEDIACODEC_H
#define HS_TOXAV_CBITS_MEDIACODEC_H

#include <stdint.h>

typedef struct Hstoxav_MediaCodec Hstoxav_MediaCodec;

/* Android's encoder for a MIME type, taking I420 frames with timestamps in ms. NULL with what went
 * wrong when the device has none that works this way: none do before Android 9, which don't say how
 * they want frames laid out. */
Hstoxav_MediaCodec *hstoxav_mediacodec_encoder_new(const char *mime, int width, int height, int bit_rate,
                                                   int gop, char *why, int why_size);
void hstoxav_mediacodec_free(Hstoxav_MediaCodec *codec);

/* The encoder's name, and how it wants frames laid out. */
void hstoxav_mediacodec_describe(const Hstoxav_MediaCodec *codec, char *text, int size);

/* What went wrong when a call last failed. */
void hstoxav_mediacodec_error(const Hstoxav_MediaCodec *codec, char *text, int size);

/* 0 once the encoder sends at the new rate, negative on error. */
int hstoxav_mediacodec_set_bit_rate(Hstoxav_MediaCodec *codec, int bit_rate);

/* 0 once the frame waits to be encoded, 1 when the encoder is busy and skips it, negative on
 * error. */
int hstoxav_mediacodec_send_frame(Hstoxav_MediaCodec *codec, const uint8_t *y, const uint8_t *u, const uint8_t *v,
                                  int64_t pts, int key);
/* 0 with a frame that stays valid until the next call, 1 for none ready, negative on error. Key
 * frames start with the stream's parameter sets. */
int hstoxav_mediacodec_receive_packet(Hstoxav_MediaCodec *codec, const uint8_t **data, int *size, int *key,
                                      int64_t *pts);

#endif /* HS_TOXAV_CBITS_MEDIACODEC_H */
