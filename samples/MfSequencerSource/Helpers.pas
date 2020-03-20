// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: Helpers.pas
// Kind: Pascal Unit
// Release date: 24-01-2020
// Language: ENU
//
// Version: 2.6.3
// Description: Requires Windows 7 or later.
//              Helper methods for the MfSequencerSource.
//
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX)
//
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/01/2020                     Underworld release.
// -----------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: MfPack Samples
// Related projects: MfPackX263
// Known Issues: -
// Compiler version: 23 up to 33
// Todo: -
// SDK version: 10.0.18362.0 (19H1)
// =============================================================================
// Source: MfPack.Additional-MfMetLib.pas
// Copyright (c) FactoryX. All rights reserved
// Source: Transcoder Example, Microsoft Docs
// Copyright (c) Microsoft Corporation. All rights reserved
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit Helpers;

interface

uses
  WinApi.ActiveX,
  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  {MfPack}
  MfPack.MfpTypes,
  MfPack.MfApi,
  MfPack.MfIdl,
  MfPack.PropIdl,
  MfPack.PropSys,
  MfPack.MfObjects,
  MfPack.ComBaseApi,
  MfPack.ObjBase,
  MfPack.MfError,
  MfPack.MMReg,
  MfPack.WmCodecDsp,
  MfPack.Unknwn;

const
  WM_APP_PLAYER_EVENT = WM_APP + 1;
  WM_PROGRESSNOTIFY   = WM_APP + 101;

type

  PlayerState = (Closed = 0,     // No session.
                 Ready,          // Session was created, ready to open a file.
                 OpenPending,    // Session is opening a file.
                 Started,        // Session is playing a file.
                 Paused,         // Session is paused.
                 Stopped,        // Session is stopped (ready to play).
                 Closing         // Application has closed the session, but is waiting for MESessionClosed.
                );

  // CriticalSection
  TMFCritSec = class
    private
    { private fields }
      FCriticalSection: TRTLCriticalSection;
    public
    { public methods }
      constructor Create();
      destructor Destroy(); override;
      procedure Lock();
      procedure Unlock();
   end;


  // Helper methods ////////////////////////////////////////////////////////////

  function CreateObjectFromUrl(const sURL: WideString;
                               out pSource: IMFMediaSource;
                               pStore: IPropertyStore = Nil;
                               const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE): HResult;

  function CreateMediaSource(sURL: PWideChar;   // The URL of the file to open.
                             out ppMediaSource: IMFMediaSource): HResult;

  // Create a playback topology from a media source.
  function CreatePlaybackTopology(pSource: IMFMediaSource;               // Media source.
                                  pPD: IMFPresentationDescriptor;        // Presentation descriptor.
                                  hVideoWnd: HWND;                       // Video window.
                                  var ppTopology: IMFTopology;           // Receives a pointer to the topology.
                                  dwSourceStreams: DWORD = 0): HResult;
  //
  function AddBranchToPartialTopology(pTopology: IMFTopology;
                                      pSource: IMFMediaSource;
                                      pPD: IMFPresentationDescriptor;
                                      iStream: DWord;
                                      hVideoWnd: HWND): HResult;

  // Add a source node to a topology.
  function AddSourceNode(pTopology: IMFTopology;                   // Topology.
                         pSource: IMFMediaSource;                  // Media source.
                         pPD: IMFPresentationDescriptor;           // Presentation descriptor.
                         pSD: IMFStreamDescriptor;                 // Stream descriptor.
                         out ppNode: IMFTopologyNode): HResult;    // Receives the node pointer.

  // Add an output node to a topology.
  function AddOutputNodeA(pTopology: IMFTopology;                 // Topology.
                          pActivate: IMFActivate;                 // Media sink activation object.
                          dwId: DWORD;                            // Identifier of the stream sink.
                          out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.

  // Create an activation object for a renderer, based on the stream media type.
  function CreateMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                   hVideoWnd: HWND;
                                   out ppActivate: IMFActivate): HResult;
  //
  function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                                 out ppPD: IMFPresentationDescriptor): HResult;

  function GetEventObject(pEvent: IMFMediaEvent;
                          out ppObject): HResult;

  function GetDurationFromTopology(pTopology: IMFTopology;
                                   out phnsDuration: LONGLONG): HResult;

  function GetCollectionObject(pCollection: IMFCollection;
                               const dwIndex: DWORD;
                               out ppObject): HResult;

  // Intitialize COM and MF
  function InitMF(): HResult;
  // Close COM and MF
  function CloseMF(): HResult;
  

  // If you don't want to use VCL.Forms Application.processMessages(), this is the alternative.
  // Use function GetCurrentThread() from unit Windows.pas to assign it's value to hThread.
  // Example: HandleMessages(GetCurrentThread(), value in milliseconds); or HandleMessages(GetCurrentThread());
  // or Assign your own thread handle to it.
  procedure HandleMessages(hThread: THandle; cWait: Cardinal = INFINITE);


implementation


// TMFCritSec //////////////////////////////////////////////////////////////////

constructor TMFCritSec.Create();
begin
  InitializeCriticalSection(FcriticalSection);
end;

destructor TMFCritSec.Destroy();
begin
  DeleteCriticalSection(FcriticalSection);
end;

procedure TMFCritSec.Lock();
begin
  EnterCriticalSection(FcriticalSection);
end;

procedure TMFCritSec.Unlock();
begin
  LeaveCriticalSection(FcriticalSection);
end;

// Create a media object from an URL or stream.
// NOTE: This is the replacement for earlier function CreateMediaSourceFromUrl
function CreateObjectFromUrl(const sURL: WideString;
                             out pSource: IMFMediaSource;
                             pStore: IPropertyStore = Nil;
                             const dwFlags: DWord = MF_RESOLUTION_MEDIASOURCE): HResult;
var
  ObjectType: MF_OBJECT_TYPE;
  pSourceResolver: IMFSourceResolver;
  unkSource: IUnknown;
  hr: HResult;

label
  Done;

begin

  ObjectType := MF_OBJECT_INVALID;

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);
  if (Failed(hr)) then
    goto done;

  // Use the source resolver to create the media source.
  // Note: For simplicity this function uses the synchronous method on
  // IMFSourceResolver to create the media source. However, creating a media
  // source can take a noticeable amount of time, especially for a network source.
  // For a more responsive UI, use the asynchronous BeginCreateObjectFromURL method.

  hr := pSourceResolver.CreateObjectFromURL(LPCWSTR(sURL), // URL of the source.
                                            dwFlags,       // Create a source object.
                                            pStore,        // Optional property store.
                                            ObjectType,    // Receives the created object type.
                                            unkSource);    // Receives a pointer to the media source (IUnknown).
  if (Failed(hr)) then
    goto done;



  // Get the IMFMediaSource interface from the media source.
  hr := unkSource.QueryInterface(IUnknown,
                                 pSource);

  // This will work as well: pSource := IMFMediaSource(unkSource);

Done:
  // unlike C/CPP Delphi cleans up all interfaces when going out of scope.
  Result := hr;
end;


///////////////////////////////////////////////////////////////////////
//  CreateMediaSource
//
//  Creates a media source from a URL.
///////////////////////////////////////////////////////////////////////

function CreateMediaSource(sURL: PWideChar;   // The URL of the file to open.
                           out ppMediaSource: IMFMediaSource): HResult;
var
  hr: HResult;
  ObjectType: MF_OBJECT_TYPE;
  pSourceResolver: IMFSourceResolver;
  pUnkSource: IUnknown;

begin

  if Not Assigned(sURL) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  ObjectType := MF_OBJECT_INVALID;

  // Create the source resolver.
  hr := MFCreateSourceResolver(pSourceResolver);

  // Use the source resolver to create the media source.

  if Succeeded(hr) then
    begin
      hr := pSourceResolver.CreateObjectFromURL(sURL,                      // URL of the source.
                                                MF_RESOLUTION_MEDIASOURCE, // Create a source object.
                                                Nil,                       // Optional property store.
                                                ObjectType,                // Receives the created object type.
                                                pUnkSource                 // Receives a pointer to the media source.
                                               );
    end;

  // Get the IMFMediaSource from the IUnknown pointer.
  if Succeeded(hr) then
    hr := pUnkSource.QueryInterface(IID_IMFMediaSourceEx,
                                    ppMediaSource);

  Result := hr;
end;


// Create a playback topology from a media source.
function CreatePlaybackTopology(pSource: IMFMediaSource;               // Media source.
                                pPD: IMFPresentationDescriptor;        // Presentation descriptor.
                                hVideoWnd: HWND;                       // Video window.
                                var ppTopology: IMFTopology;           // Receives a pointer to the topology.
                                dwSourceStreams: DWORD = 0): HResult;
var
  tmpTopology: IMFTopology;
  hr: HResult;
  i: integer;

label
  done;

begin

  hr := MFCreateTopology(tmpTopology);
  if (Failed(hr)) then
    goto done;

  // Get the number of streams in the media source.
  hr := pPD.GetStreamDescriptorCount(dwSourceStreams);
  if (Failed(hr)) then
    goto done;

  // For each stream, create the topology nodes and add them to the topology.
  for i := 0 to dwSourceStreams - 1 do
    begin
      hr := AddBranchToPartialTopology(tmpTopology,
                                       pSource,
                                       pPD,
                                       i,
                                       hVideoWnd);
      if (Failed(hr)) then
        goto done;

      hr := pPD.SelectStream(i);
    end;

  ppTopology := tmpTopology;
  if (Failed(hr)) then
    goto done;

done:
  Result := hr;
end;


//  Add a topology branch for one stream.
//
//  For each stream, this function does the following:
//
//    1. Creates a source node associated with the stream.
//    2. Creates an output node for the renderer.
//    3. Connects the two nodes.
//
//  The media session will add any decoders that are needed.
function AddBranchToPartialTopology(pTopology: IMFTopology;
                                    pSource: IMFMediaSource;
                                    pPD: IMFPresentationDescriptor;
                                    iStream: DWord;
                                    hVideoWnd: HWND): HResult;
var
  pSD: IMFStreamDescriptor;
  pSinkActivate: IMFActivate;
  pSourceNode: IMFTopologyNode;
  pOutputNode: IMFTopologyNode;
  fSelected: BOOL;
  hr: HResult;

label
  Done;

begin
  // Use assertions only for debugging purposes
  assert(pTopology <> Nil);

  // Get the stream descriptor for this stream.
  hr := pPD.GetStreamDescriptorByIndex(iStream,
                                       fSelected,
                                       pSD);
  if (Failed(hr)) then
    goto done;

  // Create the topology branch only if the stream is selected.
  // Otherwise, do nothing.
  if (fSelected) then
    begin
      // create the media sink activation object
      hr := CreateMediaSinkActivate(pSD,
                                    hVideoWnd,
                                    pSinkActivate);
      if (Failed(hr)) then
        goto done;

      // Create a source node for this stream.
      hr := AddSourceNode(pTopology,
                          pSource,
                          pPD,
                          pSD,
                          pSourceNode);
      if (Failed(hr)) then
        goto done;

      // Create the output node for the renderer.
      hr := AddOutPutNodeA(pTopology,
                           pSinkActivate,
                           0,
                           pOutPutNode);
      if (Failed(hr)) then
        goto done;

      // Connect the source node to the output node.
      hr := pSourceNode.ConnectOutput(0,
                                      pOutputNode,
                                      0);
    end;

done:
  Result := hr;
end;


// Add a source node to a topology.
function AddSourceNode(pTopology: IMFTopology;                   // Topology.
                       pSource: IMFMediaSource;                  // Media source.
                       pPD: IMFPresentationDescriptor;           // Presentation descriptor.
                       pSD: IMFStreamDescriptor;                 // Stream descriptor.
                       out ppNode: IMFTopologyNode): HResult;    // Receives the node pointer.

var
  hr: HResult;

label
  done;

begin
  ppNode := Nil;

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE,
                             ppNode);
  if (Failed(hr)) then
    goto done;

  // Set the attributes.
  hr := ppNode.SetUnknown(MF_TOPONODE_SOURCE,
                          pSource);
  if (Failed(hr)) then
    goto done;

  hr := ppNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                          pPD);
  if (Failed(hr)) then
    goto done;

  hr := ppNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR,
                          pSD);
  if (Failed(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(ppNode);

  if (Failed(hr)) then
    goto done;

done:
   Result := hr;
end;


// Add an output node to a topology.
function AddOutputNodeA(pTopology: IMFTopology;                 // Topology.
                        pActivate: IMFActivate;                 // Media sink activation object.
                        dwId: DWORD;                            // Identifier of the stream sink.
                        out ppNode: IMFTopologyNode): HResult;  // Receives the node pointer.
var
  hr: HResult;

label
  done;

begin

  // Create the node.
  hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE,
                             ppNode);
  if (Failed(hr)) then
    goto done;

  // Set the object pointer.
  hr := ppNode.SetObject(pActivate);
  if (Failed(hr)) then
    goto done;

  // Set the stream sink ID attribute.
  hr := ppNode.SetUINT32(MF_TOPONODE_STREAMID,
                         dwId);
  if (Failed(hr)) then
    goto done;

  hr := ppNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE,
                         0);
  if (Failed(hr)) then
    goto done;

  // Add the node to the topology.
  hr := pTopology.AddNode(ppNode);
  if (Failed(hr)) then
    goto done;

done:
  Result := hr;
end;

//  Create an activation object for a renderer, based on the stream media type.
function CreateMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                 hVideoWnd: HWND;
                                 out ppActivate: IMFActivate): HResult;
var
  phandler: IMFMediaTypeHandler;
  pActivate: IMFActivate;
  guidMajorType: TGUID;
  hr: HResult;

label
  Done;

begin
  // Get the media type handler for the stream
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if Failed(hr) then
    goto Done;

  // Get the major media type
  hr := pHandler.GetMajorType(guidMajorType);
  if Failed(hr) then
    goto Done;

  // Create an IMFActivate object for the renderer, based on the media type
  if IsEqualGuid(MFMediaType_Audio,
                guidMajorType) then
    hr := MFCreateAudioRendererActivate(pActivate)
  else if IsEqualGuid(MFMediaType_Video,
                      guidMajorType) then
    hr := MFCreateVideoRendererActivate(hVideoWnd,
                                        pActivate)
  else
    hr := E_FAIL;

  if Failed(hr) then
    goto Done;

  // Return IMFactivate pointer to caller
  ppActivate := pActivate;

done:
  Result := hr;
end;

// Given a topology, returns a pointer to the presentation descriptor.
function GetPresentationDescriptorFromTopology(pTopology: IMFTopology;
                                               out ppPD: IMFPresentationDescriptor): HResult;
var
  hr: HResult;
  pCollection: IMFCollection;
  pUnk: IUnknown;
  pNode: IMFTopologyNode;
  pPD: IMFPresentationDescriptor;

label
  done;

begin
  // Get the collection of source nodes from the topology.
  hr := pTopology.GetSourceNodeCollection(pCollection);

  if Failed(hr) then
    goto done;

  // Any of the source nodes should have the PD, so take the first
  // object in the collection.

  hr := pCollection.GetElement(0,
                               pUnk);
  if Failed(hr) then
    goto done;

  hr := pUnk.QueryInterface(IID_IMFTopologyNode,
                            pNode);

  if Failed(hr) then
    goto done;

  // Get the PD, which is stored as an attribute.
  hr := pNode.GetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         IID_IMFPresentationDescriptor,
                         pPD);
  if Failed(hr) then
    goto done;

  ppPD := pPD;

done:
  Result := hr;
end;


function GetEventObject(pEvent: IMFMediaEvent;
                        out ppObject): HResult;
var
  vVar: mfPROPVARIANT;
  hr: HResult;

begin
  PropVariantInit(vVar);
  hr := pEvent.GetValue(vvar);

  if Succeeded(hr) then
    begin
      if (vvar.vt = WORD(VT_UNKNOWN)) then
        hr := IUnknown(vvar.ppunkVal).QueryInterface(IID_IUnknown,
                                                     ppObject)
      else
        hr := MF_E_INVALIDTYPE;

      PropVariantClear(vVar);
    end;

  Result := hr;
end;


//
function GetDurationFromTopology(pTopology: IMFTopology;
                                 out phnsDuration: LONGLONG): HResult;
var
  pSourceNodes: IMFCollection;
  pNode: IMFTopologyNode;
  pPD: IMFPresentationDescriptor;
  hr: HResult;

label
  done;

begin
  phnsDuration := 0;
  pSourceNodes := Nil;
  pNode := Nil;
  pPD := Nil;

  hr := pTopology.GetSourceNodeCollection(pSourceNodes);
  if Failed(hr) then
    goto done;

  hr := GetCollectionObject(pSourceNodes,
                            0,
                            pNode);
  if Failed(hr) then
    goto done;

  hr := pNode.GetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR,
                         IID_IMFPresentationDescriptor,
                         pPD);
  if Failed(hr) then
    goto done;

  phnsDuration := MFGetAttributeUINT64(pPD,
                                       MF_PD_DURATION,
                                       0);

done:
  Result := hr;
end;


function GetCollectionObject(pCollection: IMFCollection;
                             const dwIndex: DWORD;
                             out ppObject): HResult;
var
  pUnk: IUnknown;
  hr: HResult;

begin
  //ppObject := Nil;   // zero output
  pUnk := Nil;
  hr := pCollection.GetElement(dwIndex,
                               pUnk);
  if Succeeded(hr) then
    begin
      hr := pUnk.QueryInterface(IID_IUnknown,
                                ppObject);
    end;

  Result := hr;
end;


// COM and Mf //////////////////////////////////////////////////////////////////

function InitMF(): HResult;
var
  hr: HResult;

begin
  // Initialize the COM library.
  hr := CoInitializeEx(Nil,
                       COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);

  if Failed(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('COM library initialisation failure.'),
                 LPCWSTR('COM Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;

  // Intialize the Media Foundation platform and
  // check if the current MF version match user's version
  hr := MFStartup(MF_VERSION);

  if Failed(hr) then
    begin
      MessageBox(0,
                 LPCWSTR('Your computer does not support this Media Foundation API version' +
                         IntToStr(MF_VERSION) + '.'),
                 LPCWSTR('MFStartup Failure!'),
                 MB_ICONSTOP);
      Abort();
    end;
  Result := hr;
end;


function CloseMF(): HResult;
begin
  // Shutdown MF
  Result := MFShutdown();
  // Shutdown COM
  CoUninitialize();
end;


procedure HandleMessages(hThread: THandle; cWait: Cardinal = INFINITE);
var
  Msg: TMsg;
begin

  while (MsgWaitForMultipleObjects(1,
                                   hThread,
                                   False,
                                   cWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(Msg,
                  0,
                  0,
                  0,
                  PM_REMOVE);

      if Msg.Message = WM_QUIT then
        Exit;

      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
end;


end.
