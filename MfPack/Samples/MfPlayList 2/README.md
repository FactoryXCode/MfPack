# MfPlayList 2

Version: 4.1.0
  
NOTES:  
This release is updated for compiler version 17 up to 35.  
SDK version 10.0.28000.2705 (Win 11)  
Requires Windows 10 or later.  
Minimum supported MfPack version: 4.1.0  

This is a separate copy of the MfPlaylist sequencer-source learning sample, extended with named playlist files.  

## Using a named playlist

1. Add local media files and arrange them with Up, Down and Remove.
2. Choose **File > Save playlist as...** (Ctrl+Shift+S), for example
   `Evening Music.m3u8`. The filename becomes the name shown in the window title.
3. Click **Build playlist**, wait for Ready, then click **Play**.
4. Choose **File > Save playlist** (Ctrl+S) after changing the list.
5. In another run, choose **File > Open playlist...** (Ctrl+O), then Build and Play.

**File > New playlist** (Ctrl+N) starts a clean untitled list. An asterisk in the
window title indicates unsaved changes. New, Open and closing the window ask
whether to save modified entries. Cancelling keeps the current list.

Click **Release** before editing the entries, creating a new list or opening
another list. Save and Save As are also available while a sequence is built.
Building or releasing playback does not modify the saved playlist.

## File format and portability

The application writes UTF-8 M3U8 text containing `#EXTM3U` and one local mediavpath per line.  
Note that the playlist contains references, not copies of the media.
Entries retain their order and duplicates.  
Paths are relative to the saved playlist when possible; paths on another drive remain absolute.  
Move the playlist and its media together while preserving their relative folder layout.
Save As recalculates relative paths for the new playlist location.

Opening resolves relative paths against the playlist's directory, independently
of the application's working directory.  
UTF-8 BOMs, blank lines and comment or metadata lines such as `#EXTINF` are accepted.  
This sample handles local media paths; network-stream URLs and legacy ANSI M3U encodings are not supported.
Missing media entries remain in the list, with their paths reported in the log.
Correct the list or restore the files before building playback.

Saving first writes and flushes a temporary file in the destination folder, then replaces the destination.  
A failed save retains the current name and unsaved state.  
A failed open leaves the current list intact.  
The chosen folder must be writable.  
Playlists are stored wherever you choose in Save As.  
  
`MfPlaylistFile.pas` implements storage; `frmMfPlaylist.pas` implements the File menu operations.  
The Media Foundation sequencer is recreated from the saved ordered paths on Build; COM sources and topologies are not serialized.
  
## Delphi build  
  
Open `MfPlayList2.dproj` in Delphi XE7 or a compatible later version.  
Choose Win32 or Win64 and Debug or Release.  
Outputs go to `Build/<platform>/<configuration>`.  
The add to the project in the uses clause`MfPack/src`.
  
  
## Default audio artwork  
  
If all artwork candidates are missing or unreadable, the player loads `DefaultArtwork.png` beside its executable.  
Track and album artwork still take priority.  
Video continues to use the video preview.  
  
Copy the default artwork PNG from the sample root into the executable output folder.  
Deploy it beside `MfPlayList2.exe`.  
Replace the source PNG in the sample root to use your own default on future builds.  
If the default file itself is missing or damaged, the player logs the problem and continues without artwork.  
See `Assets/README.md` for the generation prompt and provenance.

## Artist and song title over artwork

The current audio segment displays its artist and song title over both found album artwork and the default PNG.  
Artist text is cyan, the larger title is white, and a smooth translucent dark fade improves readability.  
Long text uses an ellipsis.  
Video segments retain the video preview.  
Captions update when the active track changes and clear when playback is released.  
  
The sample includes the RDJ playlist editor's `RDJ.FilenameParser.pas` and calls 
`TFileNameParser.ResolveArtistTitle(FileName, '', '', Artist, Title)`, matching
`TfrmPlaylistEditor.AddLoadedFileToCurrentPlaylist` for files outside its library.  
It removes numeric track/year/bitrate tokens and uses the editor's fallback `Unknown Artist` where no artist can be identified.  
This is filename-based resolution; this sample does not open the RDJ library database or read tag fields.  
The FPC copy adds only a local equivalent of Delphi's string-splitting helper.  
  
`MfArtworkCaption.pas` uses MfPack's WIC declarations to decode and scale the image, 
then Direct2D and DirectWrite to compose the text onto a WIC bitmap.
Composition happens in memory when the active track changes.  
Artwork files, including `DefaultArtwork.png` beside the executable, are never modified.
The rendered edge is bounded to 1600 pixels while retaining aspect ratio.  
If composition fails, the original artwork remains visible and the error is written to the log.  
No additional runtime package is required on Windows 10+.

The UI tests check filename cleanup, Unicode, rendered text pixels, unchanged
source artwork, long titles, real/default covers, track changes and caption cleanup.  
They also produce `ArtworkCaptionPreview.png` in the test output folder.  
  
Reference: [Microsoft camera companion app requirements](https://learn.microsoft.com/en-us/windows-hardware/drivers/stream/camera-companion-apps).  
  
  
Project: Media Foundation - MFPack - Samples  
  
Project location: https://github.com/FactoryXCode/MfPack  
  
https://sourceforge.net/projects/MFPack
  
First release date: 24/01/2026  
Last updated: 26/11/2026  
  
Copyright © FactoryX. All rights reserved.
