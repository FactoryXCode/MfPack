# Reproducible FFmpeg runtime

Pinned release: **FFmpeg 9.0.1**.

Download the source archive and signature from `https://ffmpeg.org/releases/`.
The checked runtime was built with w64devkit 2.9.0: its x64 kit produced the
Win64 DLLs and its x86 kit produced the Win32 DLLs. Build shared libraries from
the same source release. The exact common feature set is:

```sh
./configure --target-os=mingw32 --enable-shared --disable-static \
  --disable-programs --disable-doc --disable-network --disable-autodetect \
  --disable-everything --enable-avcodec --enable-avutil \
  --enable-swresample --disable-avformat --disable-avdevice \
  --disable-avfilter --disable-swscale --enable-decoder=dca \
  --enable-parser=dca --disable-x86asm --ln_s=cp
```

Add `--arch=x86_64` for Win64. Add `--arch=x86 --cpu=i686` for Win32.
`--ln_s=cp` is required because normal Windows installations do not grant the
symbolic-link privilege. If the source path contains spaces, map Sample 7 to a
temporary drive letter for the build; FFmpeg rejects such out-of-tree paths.

Do not enable GPL, version-3-only, or non-free components. Record the exact
commands actually used in `../ThirdParty/configure-command.txt`. The current
`../ThirdParty/changes.diff` records that the FFmpeg source was unmodified.
