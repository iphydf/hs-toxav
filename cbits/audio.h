#ifndef HS_TOXAV_CBITS_AUDIO_H
#define HS_TOXAV_CBITS_AUDIO_H

#include <stdint.h>

#include <opus.h>

/* Opus with the settings Tox calls use: VoIP, constant bit rate, in-band FEC for 10% loss. */
OpusEncoder *hstoxav_opus_encoder_new(int32_t rate, int channels, int32_t bit_rate);
int hstoxav_opus_encoder_set_bit_rate(OpusEncoder *enc, int32_t bit_rate);

#endif /* HS_TOXAV_CBITS_AUDIO_H */
