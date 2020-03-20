// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Brazil
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: MfMetLib.pas  // Until version 262 this unit was named MfMethods.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 2.6.3
// Description: Requires Windows 7 or later.
//              The sequencer source enables an application to play a collection of
//              media sources sequentially, with seamless transitions between the sources.
//              You can use it to create playlists, or to play streams from multiple sources simultaneously.
//
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues,
//                 (TopPlay)
//
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 29/01/2020                     Underworld release.
// -----------------------------------------------------------------------------
//
// Remarks:
//
// Related objects: MfPack Samples >= 2.6.3
// Related projects: >= MfPackX263
// Known Issues: -
// Compiler version: 23 up to 33
// Todo: -
// SDK version: 10.0.18362.0 (19H1)
// =============================================================================
// Source: Examples from Microsoft Docs.
//         https://docs.microsoft.com/en-us/windows/win32/medfound/sequencer-source
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit MfSequencerSource;

interface

uses
  WinApi.Windows,
  System.Classes,
  MfPack.MfpTypes,
  MfPack.MfApi,
  MfPack.MfIdl,
  MfPack.PropIdl,
  MfPack.MfObjects,
  MfPack.MfError,
  CorePlayerEngine,
  Helpers;

  {$HINTS OFF}

// For this example, there is a hard-coded limit to the number of playlist
// segments, to avoid using a dynamically sized list (in other words: To keep things simple)

const
  MAX_PLAYLIST_SEGMENTS = Integer(128);

type

  // Table of segment IDs and topology IDs.
  TSegments = record
    SegmentID: MFSequencerElementId;
    Topo_ID: TOPOID;
  end;
  SegmentsArray = array[0..MAX_PLAYLIST_SEGMENTS - 1] of TSegments;


  TDPlaylist = class(TDCPlayer)
  private

    m_pSequencerSource: IMFSequencerSource;
    m_pPresentationClock: IMFPresentationClock;
    m_PresentationTimeOffset: MFTIME;
    m_ActiveSegment: Integer;
    m_hnsSegmentDuration: LONGLONG;
    m_segments: SegmentsArray;
    m_count: Integer;

    function LastSegment(): MFSequencerElementId;
    function LookupTopoID(id: TOPOID; var pIndex: Integer): HResult;

    // TDCPlayer implementation
    function OnSessionEvent(pEvent: IMFMediaEvent; meType: MediaEventType): HResult; override;
    function OnTopologyStatus(pEvent: IMFMediaEvent): HResult; override;
    function OnNewPresentation(pEvent: IMFMediaEvent): HResult; override;

    // The AddSegment method adds a new playlist segment.
    // This method performs the following steps:
    //   Creates a playback topology. The code for this step is shown in the topic Creating Playback Topologies.
    //   Calls IMFSequencerSource.AppendTopology to add the topology to the playlist.
    //   On the first segment, gets the value of the MF_PD_DURATION attribute, which contains the playback duration.
    //   Stores the segment ID and topology ID in a lookup table.
    function AddSegment(pszURL: PWideChar): HResult;
    // When you create the first playlist segment, you must queue the segment topology on the Media Session, as follows:
    //   1 - Query the sequencer source for the IMFMediaSourceTopologyProvider interface.
    //   2 - Pass the presentation descriptor to the IMFMediaSourceTopologyProvider.GetMediaSourceTopology method.
    //       This method gets a pointer to the segment topology.
    //       Note that this topology is not exactly the same as the playback topology that you created earliers.
    //       Instead, it is a modified version of that topology. For more information, see About the Sequencer Source.
    //   3 - Queue the topology on the Media Session by calling IMFMediaSession.SetTopology.
    //
    // The following code shows these steps. This same code is also used when the playlist prerolls the next segment.
    function QueueNextSegment(var pPD: IMFPresentationDescriptor): HResult;

    constructor Create(hVideo: HWND;
                       hEvent: HWND;
                       phr: HResult); virtual;
    destructor Destroy(); override;

    function Initialize(): HResult;

  public

    // The TDPlaylist.CreateInstance method creates a new DPlaylist object.
    // Internally, this method calls TDPlaylist.Initialize to initialize the object.
    // The Initialize method calls MFCreateSequencerSource to create the sequence source.
    // It also calls IMFMediaSession.GetClock to get a pointer to the presentation clock.
    class function CreateInstance(hVideo: HWND; hEvents: HWND; out dPlayList: TDPlaylist): HResult;

    function AddToPlaylist(pszURL: PWideChar): HResult;
    // To delete a playlist segment, call IMFSequencerSource.DeleteTopology.
    // The segment is specified by segment ID. (This is why the application should cache the list of segment IDs.)
    function DeleteSegment(index: Integer): HResult;
    function SkipTo(index: Integer): HResult;
    function GetPlaybackTime(out phnsPresentation: MFTIME; out phnsSegment: MFTIME): HResult;

    property NumSegments: Integer read m_count;
    property ActiveSegment: Integer read m_ActiveSegment;
    property SegmentDuration: LONGLONG read m_hnsSegmentDuration;

  end;


implementation


constructor TDPlaylist.Create(hVideo: HWND;
                              hEvent: HWND;
                              phr: HResult);
begin
  inherited Create(hVideo, hEvent);
  m_PresentationTimeOffset := 0;
  m_ActiveSegment := -1;
  m_hnsSegmentDuration := 0;
  m_count := 0;
end;


destructor TDPlaylist.Destroy();
begin
  m_pSequencerSource := Nil;
  m_pPresentationClock := Nil;
  inherited Destroy();
end;


function TDPlaylist.Initialize(): HResult;
var
  pClock: IMFClock;
  hr: HResult;

label
  done;

begin

  hr := CreateSession();
  if Failed(hr) then
    goto done;

  // Create an instance of the sequencer Source.
  hr := MFCreateSequencerSource(Nil, m_pSequencerSource);
  if Failed(hr) then
    goto done;

  // At this point the sequencer doesn't have any source, so we get it when the url is loaded.
  //hr := m_pSequencerSource.QueryInterface(IID_IMFMediaSource,
  //                                        m_Source);
  //if Failed(hr) then
  //  goto done;

  // Get the presentation clock.
  hr := m_Session.GetClock(pClock);
  if Failed(hr) then
    goto done;

  hr := pClock.QueryInterface(IID_IMFPresentationClock,
                              m_PresentationClock);

  hr := m_Session.BeginGetEvent(IMFAsyncCallback(Self),
                                     Nil);
done:
  Result := hr;

end;


class function TDPlaylist.CreateInstance(hVideo: HWND; hEvents: HWND; out dPlayList: TDPlaylist): HResult;
var
  hr: HResult;
  pPlayer: TDPlaylist;

begin
  hr := S_OK;
  dPlayList := Nil;

  pPlayer := TDPlaylist.Create(hVideo, hEvents, hr);

  if Not Assigned(pPlayer) then
    begin
      hr := E_OUTOFMEMORY;
    end;

  if Succeeded(hr) then
    hr := pPlayer.Initialize();

  if Succeeded(hr) then
    dPlayList := pPlayer
  else
    pPlayer.Free;

  Result := hr;

end;


function TDPlaylist.LastSegment(): MFSequencerElementId;
begin
  Result := m_segments[m_count - 1].SegmentID;
end;


function TDPlaylist.LookupTopoID(id: TOPOID; var pIndex: Integer): HResult;
var
  index: Integer;

begin
  for index := 0 to m_count -1 do
    if (m_segments[index].Topo_ID = id) then
      break;

  if (index = m_count) then
    begin
      Result := MF_E_NOT_FOUND;
      Exit;
    end;

  pIndex := index;
  Result := S_OK;
end;


// The AddSegment method is private to CPlaylist, and is called from the following public method:
// Adds a segment to the sequencer.
function TDPlaylist.AddSegment(pszURL: PWideChar): HResult;
var
  hr: HResult;
  pMediaSource: IMFMediaSource;
  pPD: IMFPresentationDescriptor;
  pTopology: IMFTopology;
  SegmentId: MFSequencerElementId;
  TopologyID: TOPOID;

label
  done;

begin

  TopologyID := 0;

  hr := CreateMediaSource(pszURL, pMediaSource);
  if Failed(hr) then
    goto done;

  hr := pMediaSource.CreatePresentationDescriptor(pPD);
  if Failed(hr) then
    goto done;

  hr := CreatePlaybackTopology(pMediaSource,
                               pPD,
                               m_hwndVideo,
                               pTopology);
  if Failed(hr) then
    goto done;

  hr := m_pSequencerSource.AppendTopology(pTopology,
                                          DWord(SequencerTopologyFlags_Last),
                                          SegmentId);

  if Failed(hr) then
    goto done;

  hr := pTopology.GetTopologyID(TopologyID);
  if Failed(hr) then
    goto done;

  // Get the segment duration
  if (m_count = 0) then
    m_hnsSegmentDuration := MFGetAttributeUINT64(pPD,
                                                 MF_PD_DURATION,
                                                 0);

  m_segments[m_count].SegmentID := SegmentId;
  m_segments[m_count].Topo_ID := TopologyID;
  inc(m_count);
done:
  Result := hr;
end;

// Queues the next topology on the session.
function TDPlaylist.QueueNextSegment(var pPD: IMFPresentationDescriptor): HResult;
var
  hr: HResult;
  pTopoProvider: IMFMediaSourceTopologyProvider;
  pTopology: IMFTopology;

label
  done;

begin

  // Get the topology for the presentation descriptor
  hr := m_pSequencerSource.QueryInterface(IID_IMFMediaSourceTopologyProvider,
                                          pTopoProvider);
  if Failed(hr) then
    goto done;

  hr := pTopoProvider.GetMediaSourceTopology(pPD,
                                             pTopology);
  if Failed(hr) then
    goto done;

  // Set the topology on the media session
  m_Session.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE, pTopology);

done:
  Result := hr;
end;



// The AddSegment method is private to CPlaylist, and is called from the following public method:
// Adds a new segment to the playlist.
function TDPlaylist.AddToPlaylist(pszURL: PWideChar): HResult;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;
  bFirstSegment: Boolean;

label
  done;

begin
  if (NumSegments >= MAX_PLAYLIST_SEGMENTS) then
    begin
      hr := MF_E_OUT_OF_RANGE;
      goto done;
    end;

  bFirstSegment := (NumSegments = 0);

  if Not bFirstSegment then
    begin
      // Remove the "last segment" flag from the last segment.
      hr := m_pSequencerSource.UpdateTopologyFlags(LastSegment(), 0);
      if Failed(hr) then
        goto done;
    end;

  // Create the topology and add it to the sequencer.
  hr := AddSegment(pszURL);
  if Failed(hr) then
    goto done;

  // If this is the first segment, queue it on the session.
  if bFirstSegment then
    begin
      hr := m_Source.CreatePresentationDescriptor(pPD);
      if Failed(hr) then
        goto done;

      hr := QueueNextSegment(pPD);
      if Failed(hr) then
        goto done;
    end;

  if (m_state < Started) then
    m_state := OpenPending;

done:
  Result := hr;
end;


// Deletes the corresponding topology from the sequencer source
function TDPlaylist.DeleteSegment(index: Integer): HResult;
var
  hr: HResult;
  LastSegId: MFSequencerElementId;
  SegmentID: MFSequencerElementId;
  I: Integer;

label
  done;

begin
  if (index >= m_count) then
    begin
      hr := E_INVALIDARG;
      goto done;
    end;

  if (index = m_ActiveSegment) then
    begin
      hr := E_INVALIDARG;
      goto done;
    end;

  LastSegId := LastSegment();
  SegmentID := m_segments[index].SegmentID;

  hr := m_pSequencerSource.DeleteTopology(SegmentID);
  if Failed(hr) then
    goto done;

  // Delete the segment entry from the list.

  // Move everything up one slot.
  for I := index to m_count - 1 do
    m_segments[i] := m_segments[I + 1];

  Dec(m_count);

  // Is the deleted topology the last one?
  if (LastSegId = SegmentID) then
    begin
      //Get the new last segment id
      LastSegId := LastSegment();

      //set this topology as the last in the sequencer
      hr := m_pSequencerSource.UpdateTopologyFlags(LastSegId,
                                                   DWord(SequencerTopologyFlags_Last));
    end;

done:
  Result := hr;
end;


function TDPlaylist.SkipTo(index: Integer): HResult;
var
  hr: HResult;
  ID: MFSequencerElementId;
  pvar: PROPVARIANT;

begin
  if (index >= m_count) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  ID := m_segments[index].SegmentID;
  hr := MFCreateSequencerSegmentOffset(ID, 0, pvar);

  if Succeeded(hr) then
    begin
      hr := m_Session.Start(MF_TIME_FORMAT_SEGMENT_OFFSET, pvar);
      PropVariantClear(pvar);
    end;
  Result := hr;
end;


function TDPlaylist.GetPlaybackTime(out phnsPresentation: MFTIME;      // Relative to start of presentation.
                                    out phnsSegment: MFTIME): HResult; // Relative to start of segment.
var
  hr: HResult;
begin
  phnsPresentation := 0;
  phnsSegment := 0;
  hr := m_pPresentationClock.GetTime(phnsPresentation);
  if Succeeded(hr) then
    phnsSegment := (phnsPresentation - m_PresentationTimeOffset);
  Result := hr;
end;


// TDCPlayer implementation
function TDPlaylist.OnSessionEvent(pEvent: IMFMediaEvent; meType: MediaEventType): HResult;
begin
  if (meType = MESessionNotifyPresentationTime) then
    m_PresentationTimeOffset := MFGetAttributeUINT64(pEvent,
                                                     MF_EVENT_PRESENTATION_TIME_OFFSET,
                                                     0);
  Result := S_OK;
  inherited;
end;


function TDPlaylist.OnTopologyStatus(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
  pTopology: IMFTopology;
  uivalue: UINT32;
  SegmentIndex: Integer;
  Topo_ID: TOPOID;

label
  done;

begin
  uivalue := 0;

  hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                         uivalue);
  if Failed(hr) then
    goto done;

  case uivalue of
    DWord(MF_TOPOSTATUS_STARTED_SOURCE):
      begin
        // Get information about the new segment
        hr := GetEventObject(pEvent, pTopology);
        if Failed(hr) then
          begin
            hr := S_OK;
            goto done;
          end;

        hr := pTopology.GetTopologyID(Topo_ID);
        if Failed(hr) then
          goto done;

        hr := LookupTopoID(Topo_ID, SegmentIndex);
        if Failed(hr) then
          goto done;

        m_ActiveSegment := SegmentIndex;
        hr := GetDurationFromTopology(pTopology, m_hnsSegmentDuration);

      end;

    DWord(MF_TOPOSTATUS_ENDED):
      begin
        m_ActiveSegment := -1;
      end;

    DWord(MF_TOPOSTATUS_READY):
      begin
        if (m_state = OpenPending) then
          m_state := Stopped;
      end;
  end;

done:
  Result := hr;
  inherited;
end;


function TDPlaylist.OnNewPresentation(pEvent: IMFMediaEvent): HResult;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;

begin
  hr := GetEventObject(pEvent,
                       pPD);

  if Succeeded(hr) then
    begin
      // Queue the next segment on the media session
      hr := QueueNextSegment(pPD);
    end;
  Result := hr;
  inherited;
end;

end.

