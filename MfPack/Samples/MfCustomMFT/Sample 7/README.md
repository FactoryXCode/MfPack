# Sample 7 - A DTS decoder for Media Foundation

Version: 4.0.0  
  
  
NOTES:  
  
* This release is updated for compiler version 17 up to 35.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.0.0

Windows Media Foundation does not include a native DTS audio decoder. This
sample fills that gap by wrapping FFmpeg's DTS decoder in a Media Foundation
Transform (MFT).

Once the MFT is installed, an MfPack application can ask Media Foundation for
PCM audio in the usual way. Media Foundation finds this decoder, sends DTS
samples to it, and receives ordinary stereo PCM samples. The application does
not need to call FFmpeg directly.

This is mainly an educational sample. It shows how Delphi, a small C bridge,
FFmpeg and Media Foundation can work together without exposing FFmpeg's C
structures to Delphi.

## The parts involved

There are three important layers:

```text
MfPack application / Media Foundation
                 |
                 | IMFTransform calls
                 v
       MfPackDtsMFT.dll (Delphi)
                 |
                 | small exported C interface
                 v
     MfPackDtsBridge.dll (C)
                 |
                 | FFmpeg API
                 v
 avcodec + avutil + swresample DLLs
```

### 1. The Delphi MFT

`Delphi/MfDtsDecoderMFT.pas` implements `IMFTransform`. This is the component
that Media Foundation sees.

It accepts compressed DTS audio and advertises one fixed output format:

- signed 16-bit PCM;
- two interleaved channels (stereo);
- 48,000 samples per second;
- 192,000 bytes per second.

The MFT also preserves sample timing. It copies the timestamp from the DTS
input sample and calculates the duration of the PCM output from the number of
decoded bytes.

`Delphi/MfPackDtsMFT.dpr` is the COM DLL project. Its registration function
registers the component both as a COM server and as a Media Foundation audio
decoder.

### 2. The C bridge

FFmpeg is a C library. Delphi could translate all the FFmpeg declarations, but
that would make this sample dependent on many FFmpeg records, constants and
version-specific details. A new FFmpeg release could then require a large
Delphi translation update.

The bridge gives Delphi a much smaller interface:

```text
create decoder
send a DTS packet
ask how many PCM bytes are ready
read the PCM bytes
flush or drain the decoder
destroy the decoder
```

That interface is declared in `Bridge/MfPackDtsBridge.h` and implemented in
`Bridge/MfPackDtsBridge.c`.

The bridge is simply a translation layer between two programming languages.
It is written in normal C, while `MfDtsBridgeApi.pas` contains matching Delphi
declarations. There is no machine-code programming in this sample.

The C header defines functions such as `mfpack_dts_decoder_create`,
`mfpack_dts_decoder_send` and `mfpack_dts_decoder_read`. The Delphi unit
declares the same functions with matching parameter types and the `cdecl`
calling convention. The C compiler and Delphi compiler take care of the rest.

The exported function `mfpack_dts_bridge_abi` is only a compatibility check.
It returns a version number so the Delphi loader can detect a bridge DLL that
uses a different set of declarations. Most programmers using this decoder do
not need to know anything about that mechanism.

Delphi never sees an `AVCodecContext`, `AVPacket`, `AVFrame` or `SwrContext`.
It receives only an opaque pointer and must pass that pointer back to the
bridge. The C code owns the real FFmpeg objects and uses the official FFmpeg
headers, so their layout stays on the C side of the boundary.

If the exported declarations are changed incompatibly, the bridge version must
also be changed. This prevents a new Delphi DLL from accidentally loading an
older bridge DLL.

### 3. The Delphi bridge loader

`Delphi/MfDtsBridgeApi.pas` is the Delphi side of the bridge. It loads
`MfPackDtsBridge.dll` from the same directory as `MfPackDtsMFT.dll`, resolves
the exported functions with `GetProcAddress`, and checks the ABI version.

Loading the bridge this way has two useful properties:

- Media Foundation can still inspect the MFT when FFmpeg is not installed;
- input-type negotiation fails cleanly with `MF_E_TOPO_CODEC_NOT_FOUND` when
  the required runtime DLLs are missing or incompatible.

## What happens to one DTS sample?

The normal data flow is:

1. Media Foundation calls `ProcessInput` with a compressed DTS sample.
2. The Delphi MFT obtains a contiguous buffer from that sample.
3. It sends the compressed bytes to `MfPackDtsBridge.dll`.
4. The bridge gives the packet to FFmpeg's `dca` decoder.
5. FFmpeg may produce one or more decoded audio frames.
6. `libswresample` converts and downmixes those frames to 48 kHz, 16-bit
   stereo PCM.
7. The bridge stores the PCM bytes in its internal queue.
8. The Delphi MFT reads the available bytes into a pending output buffer.
9. Media Foundation calls `ProcessOutput` and receives a new PCM sample with
   its timestamp and duration.

Not every compressed packet is guaranteed to produce output immediately. The
bridge therefore separates `send`, `available` and `read` instead of assuming
one DTS packet always equals one PCM sample.

When Media Foundation sends `MFT_MESSAGE_COMMAND_FLUSH`, the MFT clears its
pending output and tells FFmpeg to discard buffered decoder state. This is
important after seeking. The bridge also provides a separate drain function
for a client that needs FFmpeg to return buffered audio at end of stream.

## Folder layout

```text
Bridge/       C bridge source and FFmpeg build notes
Delphi/       IMFTransform implementation and COM DLL project
Installer/    Inno Setup script and package build script
Runtime/      Generated Win32 and Win64 DLL sets
Tests/        Bridge and MFT smoke tests
ThirdParty/   FFmpeg source, license and reproducible-build information
```

`Runtime` and `Installer/Output` contain generated files. They are deployment
artifacts rather than source files.

## Building it

Build Win32 and Win64 separately. A 32-bit MFT must load the 32-bit bridge and
FFmpeg DLLs; a 64-bit MFT must load the 64-bit versions. Architectures cannot
be mixed in one process.

The intended order is:

1. Build the reduced FFmpeg shared libraries by following
   `Bridge/build-ffmpeg.md`.
2. Build `MfPackDtsBridge.dll` against the headers and import libraries from
   that same FFmpeg build.
3. Place the bridge and the three FFmpeg runtime DLLs in the matching
   `Runtime/Win32` or `Runtime/Win64` directory.
4. Build `Delphi/MfPackDtsMFT.dproj` for both platforms and place each MFT DLL
   in its matching runtime directory.
5. Run the smoke tests.
6. Run `Installer/Build-Package.ps1` to create the installer.

The package script deliberately stops when a required runtime, source or
license file is missing.

## Testing

`Tests/MfDtsBridgeSmoke.c` tests the C bridge directly. This is useful when the
problem is below Media Foundation, for example a missing FFmpeg DLL or a bridge
ABI mismatch.

`Tests/MfDtsMftSmoke.dpr` loads the Delphi COM DLL, registers its class factory
locally, enumerates it as a Media Foundation decoder and negotiates DTS input
to PCM output. When a media filename is supplied, it also asks a Source Reader
to decode the DTS stream and checks the resulting PCM timing.

Local registration is process-only and is useful for development. The
installer performs normal system registration so other Media Foundation
applications can discover the decoder.

Sample 8 turns this local-registration test pattern into an application-local
deployment. Its client loads the same decoder class factory from DLLs beside
the executable and uses `MFTRegisterLocal` without changing the decoder or
writing COM/MFT registry entries. See [Sample 8](../Sample%208/README.md).

## Runtime files

Each installed architecture needs these files together:

```text
MfPackDtsMFT.dll
MfPackDtsBridge.dll
avcodec-63.dll
avutil-61.dll
swresample-7.dll
```

If the bridge or an FFmpeg dependency is absent, the MFT refuses the DTS media
type instead of pretending to support it and silently selecting another audio
track.

## FFmpeg and licensing

The supplied FFmpeg 9.0.1 libraries are reduced shared-library builds. They
enable the `dca` decoder and parser and do not enable GPL or non-free
components.

`ThirdParty` contains the exact source archive, its signature, the LGPL text,
the configure commands and a declaration of source changes. Keep these files
with any distributed binary package. See `ThirdParty/NOTICE.md` for the short
third-party notice.

## Where can the decoder be used?

The decoder is not tied to Cast V2.1. After registration, it can be used by
MfCastPlayer, MfPlayerX and other MfPack samples—or by another Media Foundation
application that requests PCM output from a supported DTS input stream.
  
Project: Media Foundation - MFPack - Samples  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 02/08/2026  
Final release date: 15/09/2026  
  
Copyright © FactoryX. All rights reserved.
