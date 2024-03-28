# SinkWriterToEncodeVideoSample Sample 3.
Version: X 3.1.6

A VCL-based Delphi-application to encode a series of bitmaps with an audio file to 
video using the Windows Media Foundation API.

The sample shows how to select, arrange and preview bitmap, gif, png or jpeg files and
audio that can be added to a mp4 file.


* Thanks to Renate Schaaf for the initiative sample!
  See: https://github.com/rmesch/Bitmaps2Video-for-Media-Foundation. 

Supported file formats and codecs:

Output:  
Presently only .mp4 with H264 or H265(HEVC)-codec.
Hardware encoding is enabled if supported.
Audio can be encoded to AAC, FLAC or Dolby AC-3.

Input:  
Theoretically anything that Windows has a decoder and encoder for should work as input for audio. 
Exceptions: - Resampling different from the input is not always supported.
            - The AAC coder needs 44.1 or 48 kHz bitrates as input.
 

Usage:
1 - Set the desired video for output (which is currently MP4 only).
2 - Select "Include Audio", this option is not required, and select the audio encoder. 
3 - Select "Create Slideshow".
4 - Select the folder with your imagefiles (Supported are png, gif, jpeg and bitmap files)
5 - Select the files that should be added. 
6 - Set Timing and rendering options.
7 - Eventually run a preview. 
8 - Create SlideShow. 

Delphi-Versions:

Developed under Delphi XE-7 Supported Delphi versions are XE7 and above. 
Records with methods, anonymous procedures, interposer classes, tasks and TWicImage need to be available.

Runtime Requirement:

To be able to use all encoding features, Windows 10 20H2 or higher is required.

Project: Media Foundation - MFPack - Samples
Project location: https://github.com/FactoryXCode/MfPack
                  https://sourceforge.net/projects/MFPack

First release date: 24-02-2024
Final release date: 28-03-2024

Copyright © FactoryX. All rights reserved. 
