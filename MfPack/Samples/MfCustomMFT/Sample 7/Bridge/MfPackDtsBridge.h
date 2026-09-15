#ifndef MFPACK_DTS_BRIDGE_H
#define MFPACK_DTS_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

#define MFPACK_DTS_API __declspec(dllexport)
#define MFPACK_DTS_CALL __cdecl
#define MFPACK_DTS_BRIDGE_ABI 1u

#ifdef __cplusplus
extern "C" {
#endif

typedef void *MfPackDtsDecoderHandle;

MFPACK_DTS_API uint32_t MFPACK_DTS_CALL mfpack_dts_bridge_abi(void);
MFPACK_DTS_API const char *MFPACK_DTS_CALL mfpack_dts_bridge_ffmpeg_version(void);
MFPACK_DTS_API int MFPACK_DTS_CALL mfpack_dts_decoder_create(
    const uint8_t *extra_data, size_t extra_data_size,
    int output_sample_rate, int output_channels,
    MfPackDtsDecoderHandle *decoder);
MFPACK_DTS_API void MFPACK_DTS_CALL mfpack_dts_decoder_destroy(MfPackDtsDecoderHandle decoder);
MFPACK_DTS_API int MFPACK_DTS_CALL mfpack_dts_decoder_send(
    MfPackDtsDecoderHandle decoder, const uint8_t *packet_data, size_t packet_size);
MFPACK_DTS_API int MFPACK_DTS_CALL mfpack_dts_decoder_drain(MfPackDtsDecoderHandle decoder);
MFPACK_DTS_API size_t MFPACK_DTS_CALL mfpack_dts_decoder_available(MfPackDtsDecoderHandle decoder);
MFPACK_DTS_API size_t MFPACK_DTS_CALL mfpack_dts_decoder_read(
    MfPackDtsDecoderHandle decoder, uint8_t *destination, size_t destination_size);
MFPACK_DTS_API void MFPACK_DTS_CALL mfpack_dts_decoder_flush(MfPackDtsDecoderHandle decoder);

#ifdef __cplusplus
}
#endif
#endif

