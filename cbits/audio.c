#include "audio.h"

#include <stddef.h>

OpusEncoder *hstoxav_opus_encoder_new(int32_t rate, int channels, int32_t bit_rate)
{
    int err;
    OpusEncoder *enc = opus_encoder_create(rate, channels, OPUS_APPLICATION_VOIP, &err);
    if (err != OPUS_OK) {
        return NULL;
    }
    // A variable bit rate gives away what is said through the packets' sizes
    // (doi:10.1109/SP.2011.34).
    if (opus_encoder_ctl(enc, OPUS_SET_BITRATE(bit_rate)) != OPUS_OK
            || opus_encoder_ctl(enc, OPUS_SET_VBR(0)) != OPUS_OK
            || opus_encoder_ctl(enc, OPUS_SET_INBAND_FEC(1)) != OPUS_OK
            || opus_encoder_ctl(enc, OPUS_SET_PACKET_LOSS_PERC(10)) != OPUS_OK) {
        opus_encoder_destroy(enc);
        return NULL;
    }
    return enc;
}

int hstoxav_opus_encoder_set_bit_rate(OpusEncoder *enc, int32_t bit_rate)
{
    return opus_encoder_ctl(enc, OPUS_SET_BITRATE(bit_rate));
}
