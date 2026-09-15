#include "MfPackDtsBridge.h"

#include <errno.h>
#include <limits.h>
#include <string.h>

#include <libavcodec/avcodec.h>
#include <libavutil/avutil.h>
#include <libavutil/channel_layout.h>
#include <libavutil/error.h>
#include <libavutil/mathematics.h>
#include <libavutil/mem.h>
#include <libavutil/samplefmt.h>
#include <libswresample/swresample.h>

typedef struct MfPackDtsDecoder {
    AVCodecContext *codec;
    AVFrame *frame;
    AVPacket *packet;
    SwrContext *swr;
    AVChannelLayout input_layout;
    AVChannelLayout output_layout;
    enum AVSampleFormat input_format;
    int input_rate;
    int output_rate;
    int output_channels;
    uint8_t *pcm;
    size_t pcm_size;
    size_t pcm_offset;
    size_t pcm_capacity;
} MfPackDtsDecoder;

static int reserve_pcm(MfPackDtsDecoder *d, size_t additional)
{
    size_t remaining = d->pcm_size - d->pcm_offset;
    size_t required;
    size_t capacity;
    uint8_t *replacement;

    if (d->pcm_offset && remaining)
        memmove(d->pcm, d->pcm + d->pcm_offset, remaining);
    d->pcm_size = remaining;
    d->pcm_offset = 0;
    if (additional > SIZE_MAX - remaining)
        return AVERROR(ENOMEM);
    required = remaining + additional;
    if (required <= d->pcm_capacity)
        return 0;
    capacity = d->pcm_capacity ? d->pcm_capacity : 65536u;
    while (capacity < required) {
        if (capacity > SIZE_MAX / 2u) {
            capacity = required;
            break;
        }
        capacity *= 2u;
    }
    replacement = (uint8_t *)av_realloc(d->pcm, capacity);
    if (!replacement)
        return AVERROR(ENOMEM);
    d->pcm = replacement;
    d->pcm_capacity = capacity;
    return 0;
}

static int configure_swr(MfPackDtsDecoder *d, const AVFrame *frame)
{
    int changed = !d->swr || d->input_rate != frame->sample_rate ||
        d->input_format != (enum AVSampleFormat)frame->format ||
        av_channel_layout_compare(&d->input_layout, &frame->ch_layout) != 0;
    int result;

    if (!changed)
        return 0;
    swr_free(&d->swr);
    av_channel_layout_uninit(&d->input_layout);
    result = av_channel_layout_copy(&d->input_layout, &frame->ch_layout);
    if (result < 0)
        return result;
    result = swr_alloc_set_opts2(&d->swr, &d->output_layout,
        AV_SAMPLE_FMT_S16, d->output_rate, &frame->ch_layout,
        (enum AVSampleFormat)frame->format, frame->sample_rate, 0, NULL);
    if (result < 0)
        return result;
    result = swr_init(d->swr);
    if (result < 0) {
        swr_free(&d->swr);
        return result;
    }
    d->input_rate = frame->sample_rate;
    d->input_format = (enum AVSampleFormat)frame->format;
    return 0;
}

static int queue_frame(MfPackDtsDecoder *d, AVFrame *frame)
{
    uint8_t **output = NULL;
    int line_size = 0;
    int capacity;
    int samples;
    int bytes;
    int result = configure_swr(d, frame);

    if (result < 0)
        return result;
    capacity = (int)av_rescale_rnd(
        swr_get_delay(d->swr, frame->sample_rate) + frame->nb_samples,
        d->output_rate, frame->sample_rate, AV_ROUND_UP);
    result = av_samples_alloc_array_and_samples(&output, &line_size,
        d->output_channels, capacity, AV_SAMPLE_FMT_S16, 0);
    if (result < 0)
        return result;
    samples = swr_convert(d->swr, output, capacity,
        (const uint8_t **)frame->extended_data, frame->nb_samples);
    if (samples < 0) {
        result = samples;
        goto done;
    }
    bytes = av_samples_get_buffer_size(NULL, d->output_channels, samples,
                                       AV_SAMPLE_FMT_S16, 1);
    if (bytes < 0) {
        result = bytes;
        goto done;
    }
    result = reserve_pcm(d, (size_t)bytes);
    if (result >= 0) {
        memcpy(d->pcm + d->pcm_size, output[0], (size_t)bytes);
        d->pcm_size += (size_t)bytes;
    }
done:
    if (output) {
        av_freep(&output[0]);
        av_freep(&output);
    }
    return result;
}

static int receive_frames(MfPackDtsDecoder *d)
{
    int result;
    for (;;) {
        result = avcodec_receive_frame(d->codec, d->frame);
        if (result == AVERROR(EAGAIN) || result == AVERROR_EOF)
            return 0;
        if (result < 0)
            return result;
        result = queue_frame(d, d->frame);
        av_frame_unref(d->frame);
        if (result < 0)
            return result;
    }
}

uint32_t MFPACK_DTS_CALL mfpack_dts_bridge_abi(void) { return MFPACK_DTS_BRIDGE_ABI; }
const char *MFPACK_DTS_CALL mfpack_dts_bridge_ffmpeg_version(void) { return av_version_info(); }

int MFPACK_DTS_CALL mfpack_dts_decoder_create(const uint8_t *extra,
    size_t extra_size, int rate, int channels, MfPackDtsDecoderHandle *handle)
{
    const AVCodec *codec;
    MfPackDtsDecoder *d;
    int result;
    if (!handle || rate <= 0 || channels != 2)
        return AVERROR(EINVAL);
    *handle = NULL;
    codec = avcodec_find_decoder(AV_CODEC_ID_DTS);
    if (!codec)
        return AVERROR_DECODER_NOT_FOUND;
    d = (MfPackDtsDecoder *)av_calloc(1, sizeof(*d));
    if (!d)
        return AVERROR(ENOMEM);
    d->output_rate = rate;
    d->output_channels = channels;
    av_channel_layout_default(&d->output_layout, channels);
    d->codec = avcodec_alloc_context3(codec);
    d->frame = av_frame_alloc();
    d->packet = av_packet_alloc();
    if (!d->codec || !d->frame || !d->packet) {
        result = AVERROR(ENOMEM);
        goto failure;
    }
    if (extra && extra_size) {
        if (extra_size > INT_MAX - AV_INPUT_BUFFER_PADDING_SIZE) {
            result = AVERROR(EINVAL);
            goto failure;
        }
        d->codec->extradata = (uint8_t *)av_mallocz(
            extra_size + AV_INPUT_BUFFER_PADDING_SIZE);
        if (!d->codec->extradata) {
            result = AVERROR(ENOMEM);
            goto failure;
        }
        memcpy(d->codec->extradata, extra, extra_size);
        d->codec->extradata_size = (int)extra_size;
    }
    result = avcodec_open2(d->codec, codec, NULL);
    if (result < 0)
        goto failure;
    *handle = d;
    return 0;
failure:
    mfpack_dts_decoder_destroy(d);
    return result;
}

void MFPACK_DTS_CALL mfpack_dts_decoder_destroy(MfPackDtsDecoderHandle handle)
{
    MfPackDtsDecoder *d = (MfPackDtsDecoder *)handle;
    if (!d) return;
    swr_free(&d->swr);
    av_channel_layout_uninit(&d->input_layout);
    av_channel_layout_uninit(&d->output_layout);
    av_packet_free(&d->packet);
    av_frame_free(&d->frame);
    avcodec_free_context(&d->codec);
    av_free(d->pcm);
    av_free(d);
}

int MFPACK_DTS_CALL mfpack_dts_decoder_send(MfPackDtsDecoderHandle handle,
    const uint8_t *data, size_t size)
{
    MfPackDtsDecoder *d = (MfPackDtsDecoder *)handle;
    int result;
    if (!d || !data || !size || size > INT_MAX)
        return AVERROR(EINVAL);
    av_packet_unref(d->packet);
    result = av_new_packet(d->packet, (int)size);
    if (result < 0) return result;
    memcpy(d->packet->data, data, size);
    result = avcodec_send_packet(d->codec, d->packet);
    av_packet_unref(d->packet);
    return result < 0 ? result : receive_frames(d);
}

int MFPACK_DTS_CALL mfpack_dts_decoder_drain(MfPackDtsDecoderHandle handle)
{
    MfPackDtsDecoder *d = (MfPackDtsDecoder *)handle;
    int result;
    if (!d) return AVERROR(EINVAL);
    result = avcodec_send_packet(d->codec, NULL);
    if (result < 0 && result != AVERROR_EOF) return result;
    return receive_frames(d);
}

size_t MFPACK_DTS_CALL mfpack_dts_decoder_available(MfPackDtsDecoderHandle handle)
{
    MfPackDtsDecoder *d = (MfPackDtsDecoder *)handle;
    return d ? d->pcm_size - d->pcm_offset : 0;
}

size_t MFPACK_DTS_CALL mfpack_dts_decoder_read(MfPackDtsDecoderHandle handle,
    uint8_t *destination, size_t size)
{
    MfPackDtsDecoder *d = (MfPackDtsDecoder *)handle;
    size_t available;
    if (!d || !destination || !size) return 0;
    available = d->pcm_size - d->pcm_offset;
    if (size > available) size = available;
    memcpy(destination, d->pcm + d->pcm_offset, size);
    d->pcm_offset += size;
    if (d->pcm_offset == d->pcm_size) d->pcm_offset = d->pcm_size = 0;
    return size;
}

void MFPACK_DTS_CALL mfpack_dts_decoder_flush(MfPackDtsDecoderHandle handle)
{
    MfPackDtsDecoder *d = (MfPackDtsDecoder *)handle;
    if (!d) return;
    avcodec_flush_buffers(d->codec);
    swr_free(&d->swr);
    av_channel_layout_uninit(&d->input_layout);
    d->input_rate = 0;
    d->input_format = AV_SAMPLE_FMT_NONE;
    d->pcm_offset = d->pcm_size = 0;
}
