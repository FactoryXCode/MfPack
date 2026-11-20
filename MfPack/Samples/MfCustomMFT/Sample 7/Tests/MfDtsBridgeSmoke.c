#include <stdio.h>
#include <stdint.h>
#include <windows.h>

#include "../Bridge/MfPackDtsBridge.h"

typedef uint32_t (MFPACK_DTS_CALL *bridge_abi_fn)(void);
typedef const char *(MFPACK_DTS_CALL *bridge_version_fn)(void);
typedef int (MFPACK_DTS_CALL *decoder_create_fn)(const uint8_t *, size_t,
                                                 int, int,
                                                 MfPackDtsDecoderHandle *);
typedef void (MFPACK_DTS_CALL *decoder_destroy_fn)(MfPackDtsDecoderHandle);

int main(void)
{
    HMODULE module = LoadLibraryA("MfPackDtsBridge.dll");
    bridge_abi_fn bridge_abi;
    bridge_version_fn bridge_version;
    decoder_create_fn decoder_create;
    decoder_destroy_fn decoder_destroy;
    MfPackDtsDecoderHandle decoder = NULL;
    int result;

    if (!module) {
        fprintf(stderr, "LoadLibrary failed: %lu\n", GetLastError());
        return 1;
    }
    bridge_abi = (bridge_abi_fn)GetProcAddress(module, "mfpack_dts_bridge_abi");
    bridge_version = (bridge_version_fn)GetProcAddress(module,
        "mfpack_dts_bridge_ffmpeg_version");
    decoder_create = (decoder_create_fn)GetProcAddress(module,
        "mfpack_dts_decoder_create");
    decoder_destroy = (decoder_destroy_fn)GetProcAddress(module,
        "mfpack_dts_decoder_destroy");
    if (!bridge_abi || !bridge_version || !decoder_create || !decoder_destroy) {
        fprintf(stderr, "A required bridge export is missing.\n");
        FreeLibrary(module);
        return 2;
    }
    if (bridge_abi() != MFPACK_DTS_BRIDGE_ABI) {
        fprintf(stderr, "Bridge ABI mismatch: %u\n", bridge_abi());
        FreeLibrary(module);
        return 3;
    }
    result = decoder_create(NULL, 0, 48000, 2, &decoder);
    if (result < 0 || !decoder) {
        fprintf(stderr, "DTS decoder creation failed: %d\n", result);
        FreeLibrary(module);
        return 4;
    }
    printf("MfPack DTS bridge ABI %u, FFmpeg %s, decoder ready.\n",
           bridge_abi(), bridge_version());
    decoder_destroy(decoder);
    FreeLibrary(module);
    return 0;
}
