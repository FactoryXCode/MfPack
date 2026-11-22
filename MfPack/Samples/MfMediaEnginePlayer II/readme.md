# MfMediaEnginePlayer II

Version: 4.1.0

Delphi/VCL Media Foundation player with sidecar WebVTT (`.vtt`) and SubRip
(`.srt`) subtitles through the Timed Text interfaces. Requires Windows 10 or
later and MfPack 4.1.0.

Choose **Extra > Subtitles**:

- **Transparent window** (default): the existing click-through floating layer.
- **Burned into video**: subtitle text is composited into the displayed video
  bitmap, including the F8 clipboard screenshot.

The mode can be changed during playback or while paused. Current text moves
between the two renderers immediately. Expired/cleared cues and Stop remove
text from both modes. Resize and fullscreen use the current video area.
The subtitle file must have the same base name as the media file; `.vtt`
is preferred when both formats exist.

Both modes use Media Engine frame-server output to a WIC bitmap. A clean video
frame is retained separately from the composited frame so mode changes never
leave old text behind. This CPU/GDI sample favors clarity over GPU performance.
Burn-in here applies to playback and screenshots; it does not re-encode or
modify the media file. Subtitle formatting uses the callback font and a
bottom-centered, wrapped layout; advanced WebVTT positioning is not reproduced.

API reference: [TransferVideoFrame](https://learn.microsoft.com/en-us/windows/win32/api/mfmediaengine/nf-mfmediaengine-imfmediaengine-transfervideoframe).

Project: Media Foundation - MfPack - Samples  
[GitHub](https://github.com/FactoryXCode/MfPack) | [SourceForge](https://sourceforge.net/projects/MFPack)

First release: 05-02-2016. Updated: 22-11-2026.  
Copyright FactoryX. All rights reserved.
