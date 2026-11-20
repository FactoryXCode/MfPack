# MfCaptureVideoFromGPU

&#x20; 

Version: 4.1.0

&#x20; 

Description:  
Demonstrates how to capture from screen using your videocard GPU.

&#x20; 

NOTES:

* This release is updated for compiler version 28.0 up to 36.0.
* SDK version: 10.0.28000.2705 (Win 11)
* Requires Windows 10 or later.
* Minimum supported MfPack version: 4.1.0

&#x20; 

\*\*This sample demonstrates how to capture from screen using the GPU of your video card\*\*

This is how it in global works:

1. TCaptureStreamEngine
* Creates a \*\*D3D11\*\* device + immediate context.
* Sets up Desktop Duplication on output 0.

&#x20; 

Each frame:

* AcquireNextFrame → \*\*ID3D11Texture2D\*\* (BGRA desktop).
* Shows it via TPreviewRenderer.
* Maps a \*\*DXGI\_FORMAT\_NV12\*\* staging texture for CPU write.
* Calls TGpuNV12Converter.Convert(BGRA, mappedNV12).
* Packs NV12 into an MF buffer and feeds it into an H.264 sinkwriter.

&#x20; 

2. TGpuNV12Converter
* Has its own Y (R8\_UNORM) and UV (R8G8\_UNORM @ half res) render targets + staging textures.
* Draws a fullscreen quad:

  * Pass 1: BGRA → Y.
  - Pass 2: BGRA → UV (half res).
* Maps the Y/UV staging textures and copies them into the mapped NV12 buffer (Y full res, UV interleaved).

&#x20; 

3. TPreviewRenderer
* Keeps a DXGI swap chain bound to your panel handle.
* For each frame, CopyResource the desktop texture into the backbuffer and Present.

&#x20; 

4. Form
* Creates the engine in FormCreate, hardcoded 1920x1080 @ 60fps.
* Starts/stops capture, logs progress/errors on the UI thread via TThread.Queue.

&#x20; 

The application is kept as simple as possible to show you the workflow and concepts of screen capturing to a MP4 file and preview screen.

&#x20; 

&#x20; 

Project: Media Foundation - MFPack - Samples  

&#x20;   
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack

&#x20; 

First release date: 13/08/2025  
Final release date: 18/11/2026  

&#x20; 

Copyright © FactoryX. All rights reserved.  

&#x20; 

