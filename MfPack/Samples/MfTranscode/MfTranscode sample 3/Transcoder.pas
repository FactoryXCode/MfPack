// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Transcoder.pas
// Kind: Pascal Unit
// Release date: 24-06-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: This is a modified class of the Transcoder sample,
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: Transcoding Example
//
// Copyright (c) Microsoft Corporation. All rights reserved .
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit Transcoder;

interface

  // Undefine this when not needed!
  {$DEFINE SAVE_DEBUG_REPORT}
  {$DEFINE SHOW_IN_MESSAGES_IDE}

uses
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.Unknwn,
  {System}
  System.SysUtils,
  System.Classes,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.CodecApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  Common;

const
  WM_PROGRESSNOTIFY = WM_USER + 1;
  WM_STATUSNOTIFY   = WM_USER + 2;

type

  TTranscoder = class(TObject)
  private
    rcHandle: HWnd;         // Handle of the windows that receives the messages.
    pStreamContentsArray: TStreamContentsArray;  // Array with audio and video information.
    m_pSession: IMFMediaSession;
    m_pSource: IMFMediaSource;
    m_pAttributes: IMFAttributes;  // source attributes
    m_pTopology: IMFTopology;
    m_pProfile: IMFTranscodeProfile;
    m_pClock: IMFPresentationClock;  // Presentation Clock to get position
    m_Duration: UInt64;
    m_Position: UInt64;


    function Shutdown(): HResult;
    function Transcode(): HResult;
    function Start(): HResult;
    // usage;  arrayindex := GetMediaFromContArray(mtAudio or mtVideo);
    function GetMediaFromContArray(const mitype: TMediaTypes): Integer;

    procedure SendStatusNotify(aStatusMsg: LPCWSTR);

  public

    constructor Create(clHandle: HWnd);
    destructor Destroy(); override;

    function OpenFile(aURL: TFileName): HResult;

    function ConfigureAudioOutput(const mfAudioFormat: TGuid;
                                  aIndex: Integer = 0): HResult;

    function ConfigureVideoOutput(aVideoMediaFmt: TGuid): HResult;

    function ConfigureContainer(const containerType: TGuid): HResult;

    function EncodeToFile(aURL: TFileName): HResult;
    function Stop(): Boolean;

    // Read only properties
    property Duration: UInt64 read m_Duration;
    property Position: UInt64 read m_Position;
    // Read/write properties
    property StreamContents: TStreamContentsArray read pStreamContentsArray write pStreamContentsArray;
  end;


implementation

//-------------------------------------------------------------------
//  CTranscoder constructor
//-------------------------------------------------------------------
constructor TTranscoder.Create(clHandle: HWnd);
begin
  inherited Create();

  // A Delphi Forms Application will initialize Com by default.
  CoInitializeEx(nil,
                 COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);

  // Check if the current MF version match user's
  if FAILED(MFStartup(MF_VERSION, 0)) then
    begin
      MessageBox(0,
                 LPCWSTR(format('Your computer does not support this Media Foundation API version %d.',[MF_VERSION])),
                 LPCWSTR('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;

  rcHandle := clHandle;
end;

//-------------------------------------------------------------------
//  CTranscoder destructor
//-------------------------------------------------------------------
destructor TTranscoder.Destroy();
begin
  // Shutdown the mediasource and session to release all their objects.
  Shutdown();
  // Shutdown Media Foundation
  MFShutdown();
  CoUninitialize();

  SafeRelease(m_pClock);
  SafeRelease(m_pProfile);
  SafeRelease(m_pTopology);
  SafeRelease(m_pSource);
  SafeRelease(m_pSession);
  SafeRelease(m_pAttributes);

  SetLength(pStreamContentsArray, 0);
  pStreamContentsArray := nil;

  inherited Destroy;
end;


//-------------------------------------------------------------------
//  OpenFile
//
//  1. Creates a media source for the caller specified URL.
//  2. Creates the media session.
//  3. Creates a transcode profile to hold the stream and
//     container attributes.
//
//  sURL: Input file URL.
//-------------------------------------------------------------------

function TTranscoder.OpenFile(aURL: TFileName): HResult;
var
  hr: HResult;
  pPresentationDescriptor: IMFPresentationDescriptor;

begin

  if not FileExists(aURL) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  // Create the media source.
  hr := CreateObjectFromUrl(aURL,
                            m_pSource);

  // Create the media session.
  if SUCCEEDED(hr) then
    hr := MFCreateMediaSession(m_pAttributes,
                               m_pSession);

  // Create an empty transcode profile.
  if SUCCEEDED(hr) then
    hr := MFCreateTranscodeProfile(m_pProfile);

  //
  if SUCCEEDED(hr) then
    hr := m_pSource.CreatePresentationDescriptor(pPresentationDescriptor);

  if SUCCEEDED(hr) then
    hr := GetStreamContents(pPresentationDescriptor,
                            m_pSource,
                            pStreamContentsArray);

  Result := hr;
end;

//-------------------------------------------------------------------
//  ConfigureAudioOutput
//
//  Configures the audio stream attributes.
//  These values are stored in the transcode profile.
//
//-------------------------------------------------------------------

function TTranscoder.ConfigureAudioOutput(const mfAudioFormat: TGuid;
                                          aIndex: Integer = 0): HResult;
var
  hr: HResult;
  dwMTCount: DWORD;
  pAvailableTypes: IMFCollection;
  pUnkAudioType: IUnknown;
  pAudioType: IMFMediaType;
  pAudioAttrs: IMFAttributes;

begin

  {$IFDEF DEBUG}
  assert(m_pProfile <> nil);
  {$ENDIF}

  dwMTCount := 0;

  // Get the list of output formats supported by the Windows Media
  // audio encoder.

  hr := MFTranscodeGetAudioOutputAvailableTypes(mfAudioFormat,
                                                DWord(MFT_ENUM_FLAG_ALL),
                                                nil,
                                                pAvailableTypes);

  // Get the number of elements in the list.
  if SUCCEEDED(hr) then
    begin
      hr := pAvailableTypes.GetElementCount(dwMTCount);

      if (dwMTCount = 0) then
        begin
          hr := E_UNEXPECTED;
        end;
    end;

  // Get the given media type from the collection.
  if SUCCEEDED(hr) then
    hr := pAvailableTypes.GetElement(aIndex,
                                     pUnkAudioType);
  if SUCCEEDED(hr) then
    hr := pUnkAudioType.QueryInterface(IID_IMFMediaType,
                                       pAudioType);
  if FAILED(hr) then
    begin
      DebugMsg('No suitable transform was found to encode or decode the content.', hr);
      Result := hr;
      Exit;
    end;

  {$IFDEF SAVE_DEBUG_REPORT}
  FMediaTypeDebug.LogMediaType(pAudioType);
  FMediaTypeDebug.DebugResults.SaveToFile('AudioProfiles.txt');
  {$ENDIF}



  // Create a copy of the attribute store so that we can modify it safely.
  hr := MFCreateAttributes(pAudioAttrs,
                           0);

  if SUCCEEDED(hr) then
    hr := pAudioType.CopyAllItems(pAudioAttrs);

  // Set the encoder to audio encoder, so that the
  // appropriate MFTs are added to the topology.

  if SUCCEEDED(hr) then
    hr := pAudioAttrs.SetGUID(MF_MT_SUBTYPE,
                              mfAudioFormat);

  // Set the attribute store on the transcode profile.
  if SUCCEEDED(hr) then
    hr := m_pProfile.SetAudioAttributes(pAudioAttrs);

  Result := hr;
end;


//-------------------------------------------------------------------
//  ConfigureVideoOutput
//
//  Configures the Video stream attributes.
//  These values are stored in the transcode profile.
//
//-------------------------------------------------------------------

function TTranscoder.ConfigureVideoOutput(aVideoMediaFmt: TGuid): HResult;
var
  hr: HResult;
  pVideoAttrs: IMFAttributes;
  arIndex: Integer;

label
  done;

begin
  {$IFDEF DEBUG}
  assert(m_pProfile <> nil);
  {$ENDIF}

  // Get the video properties from the array
  arIndex := GetMediaFromContArray(mtVideo);


  // Configure the video stream
  if IsEqualGUID(aVideoMediaFmt,
                 GUID_NULL) then
    begin
      // Create empty attribute store.
      hr := MFCreateAttributes(pVideoAttrs,
                               0);
      goto done;
    end;

  // Create attribute store.
  hr := MFCreateAttributes(pVideoAttrs,
                           5);

  // Set the encoder, so that the appropriate MFTs are added to the topology.
  if SUCCEEDED(hr) then
    begin
      // The H.265 video subtype must be MFVideoFormat_HEVC.
      if IsEqualGUID(aVideoMediaFmt,
                     MFVideoFormat_H265) then
        hr := pVideoAttrs.SetGUID(MF_MT_SUBTYPE,
                                  MFVideoFormat_HEVC)
      else
        hr := pVideoAttrs.SetGUID(MF_MT_SUBTYPE,
                                  aVideoMediaFmt);
    end;

  // Set the frame rate.
  // Framerate is calculated by FrameRateNumerator / FrameRateDenominator
  // For example:  5000000 / 208541 = 23.97 frames per second.
  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pVideoAttrs,
                              MF_MT_AVG_BITRATE,
                              pStreamContentsArray[arIndex].video_FrameRateNumerator,
                              pStreamContentsArray[arIndex].video_FrameRateDenominator);

  // Set the frame size.
  if SUCCEEDED(hr) then
    hr := MFSetAttributeSize(pVideoAttrs,
                             MF_MT_FRAME_SIZE,
                             pStreamContentsArray[arIndex].video_FrameSizeWidth,
                             pStreamContentsArray[arIndex].video_FrameSizeHeigth);

  //Set the pixel aspect ratio
  if SUCCEEDED(hr) then
    hr := MFSetAttributeRatio(pVideoAttrs,
                              MF_MT_PIXEL_ASPECT_RATIO,
                              pStreamContentsArray[arIndex].video_PixelAspectRatioNumerator,
                              pStreamContentsArray[arIndex].video_PixelAspectRatioDenominator);

  // Average Bitrate = FrameRateNumerator.

  if SUCCEEDED(hr) then
    hr := pVideoAttrs.SetUINT32(MF_MT_AVG_BITRATE,
                                pStreamContentsArray[arIndex].video_FrameRateNumerator);

  // Additional settings per mft
  if SUCCEEDED(hr) then
    begin
      if IsEqualGUID(aVideoMediaFmt,
                     MFVideoFormat_H264) then
        begin
          // H.264 encoding profile.
          // Supported values are:
          //   eAVEncH264VProfile_Base (default)
          //   eAVEncH264VProfile_Main
          //   eAVEncH264VProfile_High  etc. see
          hr := pVideoAttrs.SetUINT32(MF_MT_MPEG2_PROFILE,
                                      eAVEncH264VProfile_High);
        end
      else if IsEqualGUID(aVideoMediaFmt,
                          MFVideoFormat_H265) then
        begin
          // H.265 encoding profile.
          // Supported values are:
          //   eAVEncH265VProfile_Main_420_8
          hr := pVideoAttrs.SetUINT32(MF_MT_VIDEO_PROFILE,
                                      eAVEncH265VProfile_Main_420_8);
        end;
    end;
done:

  // Set the attribute store on the transcode profile.
  if SUCCEEDED(hr) then
    hr := m_pProfile.SetVideoAttributes(pVideoAttrs);

  Result := hr;
end;


//-------------------------------------------------------------------
//  ConfigureContainer
//
//  Configures the container attributes.
//  These values are stored in the transcode profile.
//
//  Note: Setting the container type does not insert the required
//  MFT node in the transcode topology. The MFT node is based on the
//  stream settings stored in the transcode profile.
//-------------------------------------------------------------------
function TTranscoder.ConfigureContainer(const containerType: TGuid): HResult;
var
  hr: HResult;
  pContainerAttrs: IMFAttributes;

begin
  {$IFDEF DEBUG}
  assert(m_pProfile <> nil);
  {$ENDIF}

  // Set container attributes
  hr := MFCreateAttributes(pContainerAttrs,
                           2);

  // Set the output container to the correct type
  if SUCCEEDED(hr) then
    hr := pContainerAttrs.SetGUID(MF_TRANSCODE_CONTAINERTYPE,
                                  containerType);

  // Use the default profile setting. Media Foundation will use the stream
  // settings set in ConfigureAudioOutput and ConfigureVideoOutput.

  if SUCCEEDED(hr) then
    hr := pContainerAttrs.SetUINT32(MF_TRANSCODE_ADJUST_PROFILE,
                                    UINT32(MF_TRANSCODE_ADJUST_PROFILE_DEFAULT));

  // Set the attribute store on the transcode profile.
  if SUCCEEDED(hr) then
    hr := m_pProfile.SetContainerAttributes(pContainerAttrs);

  Result := hr;
end;


//-------------------------------------------------------------------
//  EncodeToFile
//
//  Builds the transcode topology based on the input source,
//  configured transcode profile, and the output container settings.
//-------------------------------------------------------------------
function TTranscoder.EncodeToFile(aURL: TFileName): HResult;
var
  hr: HResult;

begin

  {$IFDEF DEBUG}
  assert(m_pSession <> nil);
  assert(m_pSource <> nil);
  assert(m_pProfile <> nil);
  {$ENDIF}

  if aURL = '' then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  // Create the transcode topology
  hr := MFCreateTranscodeTopology(m_pSource,
                                  LPCWSTR(aURL),
                                  m_pProfile,
                                  m_pTopology );

  // Set the topology on the media session.
  if SUCCEEDED(hr) then
    hr := m_pSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE,
                                 m_pTopology);

  // Get media session events. This will start the encoding session.
  if SUCCEEDED(hr) then
    hr := Transcode();

  Result := hr;
end;


//-------------------------------------------------------------------
//  Name: Transcode
//
//  Start the encoding session by controlling the media session.
//
//  The encoding starts when the media session raises the
//  MESessionTopologySet event. The media session is closed after
//  receiving MESessionEnded. The encoded file is finalized after
//  the session is closed.
//
//  For simplicity, this sample uses the synchronous method  for
//  getting media session events.
//-------------------------------------------------------------------

function TTranscoder.Transcode(): HResult;
var
  hr: HResult;
  hrStatus: HResult;  // Event status
  pEvent: IMFMediaEvent;
  meType: MediaEventType;  // Event type
  startUpdateProcessing: Boolean;

begin

  {$IFDEF DEBUG}
  assert(m_pSession <> nil);
  {$ENDIF}

  hr := S_OK;
  meType := MEUnknown;
  startUpdateProcessing := False;

  // Get media session events synchronously
  while (meType <> MESessionClosed) do
    begin

      if startUpdateProcessing then
        begin
          // When using a progressbar, this would be the best place to send a custom progress message.
          // For this you need to implement a message handler in the unit or form where this is going to be processed.
          if (m_pClock <> nil) then
            begin
              {void} m_pClock.GetTime(m_Position);
              SendMessage(rcHandle,
                          WM_PROGRESSNOTIFY,
                          WParam(1),
                          LParam(0));
            end;
          HandleMessages(GetCurrentThread());
        end;

      // This is a synchronised operation, so we change the flag to immediate response, instead of waiting for an event in the queue.
      // Doing so, we prevent that the GetEvent method will not return and leave us with a
      // zombie state app until the whole sourcefile is processed.
      if Assigned(m_pSession) then
        hr := m_pSession.GetEvent(MF_EVENT_FLAG_NO_WAIT,
                                  pEvent)
      else
        Break;

      if FAILED(hr) then
        if hr = MF_E_NO_EVENTS_AVAILABLE then
          begin
            // continue will skip the rest of this loop, so we have to get the progress update before
            // the eventype will be handled.
            hr := S_OK;
            Continue;
          end
        else
          break;


      // Get the event type.
      hr := pEvent.GetType(meType);

      if FAILED(hr) then
        Break;

      hr := pEvent.GetStatus(hrStatus);

      if FAILED(hr) then
        Break;

      if FAILED(hrStatus) then
        begin
          SendStatusNotify(LPCWSTR(Format('Failed. %d error condition triggered this event.', [hrStatus])));

          hr := hrStatus;
          Break;
        end;

      case meType of
        MESessionTopologySet: begin
                                hr := Start();
                                if SUCCEEDED(hr) then
                                  begin
                                   // We use SendMessage to display messages in the mainform's StatusBar.
                                   SendStatusNotify(LPCWSTR('Ready for rendering.'));
                                  end;
                              end;

        MESessionStarted: begin
                            startUpdateProcessing := True;
                            SendStatusNotify(LPCWSTR('Rendering is in progress...'));
                           end;

        MESessionEnded: begin
                          startUpdateProcessing := False;
                          hr := m_pSession.Close();
                          if SUCCEEDED(hr) then
                            begin
                              SendStatusNotify(LPCWSTR('Finished rendering.'));
                            end;
                        end;

        MESessionClosed: begin
                           SendStatusNotify(LPCWSTR('Output file has been succesfully created.'));
                         end;
      end;

      HandleMessages(GetCurrentThread());

      if FAILED(hr) then
        Break;

      pEvent := nil;

    end;

  // Shutdown() has been called. This code happens when the Transcode process is aborted when running.
  // ie: The user closed the application or aborted the operation while Transcoding is in progress.
  if (hr = MF_E_SHUTDOWN) then
    hr := E_ABORT;

  Result := hr;
end;


function TTranscoder.Stop(): Boolean;
begin
  Shutdown();
  Result := True;
end;



//-------------------------------------------------------------------
//  Start
//
//  Starts the encoding session.
//-------------------------------------------------------------------
function TTranscoder.Start(): HResult;
var
  hr: HResult;
  varStart: PROPVARIANT;
  pPD: IMFPresentationDescriptor;
  pClock: IMFClock;

begin
  {$IFDEF DEBUG}
  assert(m_pSession <> nil);
  {$ENDIF}

  if Assigned(m_pClock) then
    m_pClock := Nil;

  // We want the size of the sourcefile
  // Get the presentation descriptor from the topology.
  hr := GetPresentationDescriptorFromTopology(m_pTopology,
                                              pPD);
  // Get the duration from the presentation descriptor.
  // For this we need the clock
  if Succeeded(hr) then
    begin
      hr := pPD.GetUINT64(MF_PD_DURATION, UINT64(m_Duration));
      // Get the presentation clock
      if SUCCEEDED(hr) then
        hr := m_pSession.GetClock(pClock);
      if SUCCEEDED(hr) then
        hr := pClock.QueryInterface(IID_IMFPresentationClock,
                                    m_pClock);
      if Failed(hr) then
        SendStatusNotify(LPCWSTR('Could not find a presentation clock!'));


     end;

  PropVariantInit(varStart);

  hr := m_pSession.Start(GUID_NULL,
                         varStart);

  if FAILED(hr) then
    SendStatusNotify(LPCWSTR('Failed to start the session...'));


  Result := hr;
end;


//-------------------------------------------------------------------
//  Shutdown
//
//  Handler for the MESessionClosed event.
//  Shuts down the media session and the media source.
//-------------------------------------------------------------------
function TTranscoder.Shutdown(): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

  // Shut down the media source
  if Assigned(m_pSource) then
    begin
      hr := m_pSource.Stop;
      if (hr <> MF_E_SHUTDOWN) then
        hr := m_pSource.Shutdown();
    end;

  // Shut down the media session. (Synchronous operation, no events.)
  if SUCCEEDED(hr) then
    begin
      if Assigned(m_pSession) then
        begin
          hr := m_pSession.Stop();
          if Succeeded(hr) then
            hr := m_pSession.Close;
          if hr <> MF_E_SHUTDOWN then
            hr := m_pSession.Shutdown();
        end;
    end;

  if (hr <> S_OK) then
    // On this, nothing special is going on, just continue with no error, unless otherwise.
    // Set the result as aborted.
    if hr = MF_E_SHUTDOWN then
      hr := E_ABORT
    else
      SendStatusNotify(LPCWSTR('Failed to close the session...'));

  Result := hr;
end;


function TTranscoder.GetMediaFromContArray(const mitype: TMediaTypes): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Assigned(pStreamContentsArray) then
    if Length(pStreamContentsArray) > 0 then
      for I := 0 to Length(pStreamContentsArray) -1 do
        if pStreamContentsArray[I].idStreamMediaType = mitype then
          begin
            Result := I;
            Break;
          end;
end;


procedure TTranscoder.SendStatusNotify(aStatusMsg: LPCWSTR);
begin
  SendMessage(rcHandle,
              WM_STATUSNOTIFY,
              WPARAM(0),
              LPARAM(aStatusMsg));
end;

end.
