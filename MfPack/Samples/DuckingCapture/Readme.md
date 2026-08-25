# DuckingCapture Sample
  
Version: 4.0.0  
  
Description:  
  ========================================================================
      WIN32 APPLICATION : Ducking Capture Sample Project Overview
  ========================================================================
  
  This sample implements a simple "Chat" that demonstrates the Windows audio  
  ducking feature. In capture mode, audio from the default communications  
  capture endpoint is routed to the default playback endpoint.  
  Select Capture in the application to hear the routed input.  
  Use headphones while testing to avoid microphone feedback.  
    
  Note that this sample requires Windows 7 or later.  
  
NOTES:  
 - There are 2 versions of the sample. One that uses the function callback and   
   one that uses the window-callback (as used in the original CPP sample).
 - Use headphones while testing microphone routing to avoid acoustic feedback.
 - The Wave transport uses 20 ms PCM buffers. The WASAPI transport uses shared
   mode and lets the audio engine convert the capture format for the playback
   endpoint when necessary.
 - This release is updated for compiler version 17 up to 35.
 - SDK version 10.0.28000.2705 (Win 11)
 - Requires Windows 7 or later.
 - Minimum supported MfPack version: 3.1.2
  
Project: Media Foundation - MFPack - Samples  
Project location: http://sourceforge.net/projects/MFPack  
  
First release date: 05-07-2020  
Final release date: 24/08/2026  
  
Copyright © FactoryX. All rights reserved.
