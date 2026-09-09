## MfRDJ Samples
Version 4.0.0

NOTES:
These releases are updated for compiler version 17 up to 35.
SDK version: 10.0.26100.4654 (Win 11)
Requires Windows 10 or later.
Minimum supported MfPack version: 4.0.0

**MfRDJ Radio Mixer sample**  
This sample demonstrates how to build an audio mixer,  
effects and how to implement IceCast/Caddy for internet broadcasting.  
The Mixer is fully adjustable for audio endpoint assignments, mixer decks and loopback decks.  
All WASAPI sample code comes together in this sample.  
  
**Note:**  
 You have to know the principles of WASAPI, MFT's and audio manipulation.  
 This sample is large and not suitable for beginners!  
 Before using this sample make sure, you have all needed components installed (see instructions).  
  
  
---

**MfRDJ Pro Radio Mixer sample**  
  
MfRDJPro is the extended version of MfRDJ.
  
Where MfRDJ mainly demonstrates DJ-style audio playback and mixing with MfPack,  
MfRDJ Pro adds a complete live broadcast layer around the mixer.  
MfRDJ Pro still provides the familiar RDJ functions: channel decks,  
loopback decks, microphone input, effects, PFL/cue monitoring, playlist editing,  
tag editing, and local recording.  
  
The Pro version expands this into an audio/video streaming application.  
It can combine the live program audio with camera video or a static video source,  
encode the result with Microsoft Media Foundation, and publish it as a browser-playable stream.  
The main technical difference is the broadcast pipeline.  
MfRDJ Pro uses Media Foundation Sink Writer and the MPEG-4 media sink to create fragmented MP4.  
MfRDJ Pro observes the generated MP4 byte stream, extracts and patches fMP4 fragments,  
writes a rolling live.json manifest, and serves the result through FxServe or an alternative proxy server like Caddy.  
Modern browsers can then play the stream using Media Source Extensions.  
MfRDJ Pro also writes now-playing metadata, artwork links, on-air state, and  
listener counts to JSON files for the web interface.  
It includes safeguards for long-running broadcasts, such as bounded queues,  
fragment cleanup, FxServe mirroring, sleep prevention options, and clean shutdown handling.  
MfRDJ Pro supports casting to Cromecast devices on your local network using the  
MfPack Cast V2 protocols.  
  
**Note:**  
You have to know the principles of WASAPI, MFT's, MfPack Cast V2, FxServe and audio manipulation.  
This sample is large and not suitable for beginners!  
Before using this sample make sure, you have all needed components installed (see instructions).

Project: Media Foundation - MFPack - Samples - MFRDJ Samples
Project location: 
https://github.com/FactoryXCode/MfPack
https://sourceforge.net/projects/MFPack
First release date: 02/08/2026
Final release date: 09/09/2026
Copyright © FactoryX. All rights reserved.