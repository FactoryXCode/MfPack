# SinkWriterToEncodeVideo Sample 3.

&#x20; 

&#x20; 

Version: 4.0.0

&#x20; 

A VCL-based Delphi-application to encode a series of images with an audio file to  
video using the Windows Media Foundation API.  

The sample shows how to select, arrange and preview bitmap, gif, png or jpeg files and  
audio that can be added to a mp4 file.  

&#x20; 

* Thanks to Renate Schaaf for the initiative sample!
See: https://github.com/rmesch/Bitmaps2Video-for-Media-Foundation.

&#x20; 

Supported file formats and codecs:  

Output:  
Presently only .mp4 with H264 or H265(HEVC)-codec.  
Hardware encoding is enabled if supported.  
Audio can be encoded to AAC, FLAC or Dolby AC-3.  

&#x20; 

Input:  
Theoretically anything that Windows has a decoder and encoder for should work as input for audio.  
Exceptions: - Resampling different from the input is not always supported.  

* The AAC coder needs 44.1 or 48 kHz bitrates as input.  

&#x20; 

Usage:  
1 - Set the desired video for output (which is currently MP4 only).
2 - Select "Include Audio", this option is not required, and select the audio encoder.
NOTE: With Windows 11 24H2 Dolby AC-3 support will be discontinued.
3 - Select "Create Slideshow".
4 - Select the folder with your imagefiles (Supported are png, gif, jpeg and bitmap files)
5 - Select the files that should be added.
6 - Set Timing and rendering options.
7 - Eventually run a preview.
8 - Create SlideShow.

&#x20; 

NOTES:  

* This release is updated for compiler version 17 up to 34.
* SDK version 10.0.28000.2705 (Win 11)
* Requires Windows 10H2 or later.

&#x20; 

Minimum supported MfPack version: 3.1.7

&#x20; 

Project: Media Foundation - MFPack - Samples  
Project location: https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  

&#x20; 

First release date: 24/02/2024  
Final release date: 05/05/2026  

&#x20; 

Copyright © FactoryX. All rights reserved.

