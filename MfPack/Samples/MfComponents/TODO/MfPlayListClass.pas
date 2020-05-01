// FactoryX
//
// Copyright ©2003 - 2018 by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: MfPlayListClass.pas
// Kind: Pascal Unit
// Release date: 24-06-2016
// Language: ENU
//
// Version: 3.4.1
// Description: Requires Windows 7 or later.
//              This is the Playlist player class,
//              containing the necessary methodes to play a sequence of mediafiles
//              For indepth information see: https://msdn.microsoft.com/en-us/library/ee663621
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// ----------------------------------------------------------------------------
//
// Remarks:
//
// Related objects: -
// Related projects: MfPack
// Known Issues: -
// Compiler version: 23 up to 33
// Todo: -
// SDK version: 10.0.16299.15
// =============================================================================
// Source: CPlaylist Class Examples
//
// Copyright (c) 1997-2018 Microsoft Corporation. All rights reserved
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
// Groupname: FactoryX
// The Initial Developers of the Original Code are: Tony Kalf (maXcomX)
//                                                  Peter Larson (ozships)
//                                                  Ramyses De Macedo Rodrigues
// Contributor(s): Tony Kalf (maXcomX),
//                 Peter Larson (ozships),
//                 Ramyses De Macedo Rodrigues.
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit MfPlayListClass;

interface

uses
  {Winapi}
  Winapi.Windows, WinApi.ActiveX,
  {MfPack}
  MfPack.MfpTypes, MfPack.MfIdl, MfPack.MfObjects,
  MfPack.MfpUtils, MfPack.MfApi, MfPack.MfError,
  MfPack.PropIdl,
  {Project}
  MfPlayerClass, MfMethods;


  // For this example, there is a hard-coded limit to the number of playlist
  // segments, to avoid using a dynamically sized list.
const
  MAX_PLAYLIST_SEGMENTS = 127;

type
  TMfPlayList = class;


  m_segment = record
    SegmentID: MFSequencerElementId;
    TopoID: TOPOID;
  end;


  // Descendant of TMfPlayer
  TMfPlayList = class(TMfPlayer)
  private
    {private fields}
    m_PresentationTimeOffset: MFTIME;
    m_ActiveSegment: Integer;
    m_hnsSegmentDuration: LONGLONG;
    m_segments: array[0..MAX_PLAYLIST_SEGMENTS] of m_segment; // Table of segment IDs and topology IDs.
    m_count: Integer;

    function LastSegment(): MFSequencerElementId;
    function LookupTopoID(id: TOPOID;
                          out pIndex: DWORD): HResult;
    function AddSegment(pszURL: PCWSTR): HResult; // Adds a segment to the sequencer.
    function QueueNextSegment(pPD: IMFPresentationDescriptor): HResult; // Queues the next topology on the session.

    // The Initialize method calls MFCreateSequencerSource to create the sequence source.
    // It also calls IMFMediaSession.GetClock to get a pointer to the presentation clock.
    function Initialize(): HResult;


  protected
    {protected fields}

    m_pSequencerSource: IMFSequencerSource;
    m_pPresentationClock: IMFPresentationClock;

    function OnSessionEvent(pEvent: IMFMediaEvent;
                            meType: MediaEventType): HResult;
    function OnTopologyStatus(pEvent: IMFMediaEvent): HResult;
    function OnNewPresentation(pEvent: IMFMediaEvent): HResult;

  public
    {public fields}

    constructor Create(out hres: HResult);
    destructor Destroy; override;
    procedure BeforeDestruction; override;// Handle stuff before reaching Destroy


    function AddToPlaylist(pszURL: PCWSTR): HResult; // Adds a new segment to the playlist.
    function DeleteSegment(const dwindex: Integer): HResult; // Deletes the corresponding topology from the sequencer source.
    function SkipTo(const dwindex: Integer): HResult; // Skips to the specified segment in the sequencer source.

    function GetPlaybackTime(out phnsPresentation: MFTIME;
                             out phnsSegment: MFTIME): HResult;


  published
    // Properties

    property NumSegments: Integer read m_count;
    property ActiveSegment: Integer read m_ActiveSegment;
    property SegmentDuration: LONGLONG read m_hnsSegmentDuration;

  end; // END TMfPlayList class



implementation



function TMfPlayList.LastSegment(): MFSequencerElementId;
begin
  Result:= m_segments[m_count - 1].SegmentID;
end;

//
function TMfPlayList.LookupTopoID(id: TOPOID;
                                  out pIndex: DWORD): HResult;
var
  dwindex: Integer;

begin

  for dwindex:= 0 to m_count - 1 do
    begin
      if (m_segments[dwindex].TopoID = id) then
        begin
          Break;
        end;

      if (dwindex = (m_count -1)) then
        begin
          Result:= MF_E_NOT_FOUND;
          Exit;
        end;
      pIndex:= dwindex;
    end;

  Result:= S_OK;

end;

//
function TMfPlayList.OnSessionEvent(pEvent: IMFMediaEvent;
                                    meType: MediaEventType): HResult;
begin
  if (meType = MESessionNotifyPresentationTime) then
    begin
      m_PresentationTimeOffset:= MFGetAttributeUINT64(pEvent,
                                                      MF_EVENT_PRESENTATION_TIME_OFFSET,
                                                      0);
    end;

  Result:= S_OK;

end;

//
function TMfPlayList.OnTopologyStatus(pEvent: IMFMediaEvent): HResult;
var
  pTopology: IMFTopology;
  pvalue: UINT32;
  SegmentIndex: DWORD;
  pTopoID: TOPOID;
  hr: HResult;

label
  done;

begin
  pTopology:= Nil;
  pvalue:= 0;
  SegmentIndex:= 0;

  hr:= pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS,
                        pvalue);
  if FAILED(hr) then
    goto done;

  case pvalue of
    DWORD(MF_TOPOSTATUS_STARTED_SOURCE):
      begin
        // Get information about the new segment
        hr:= GetEventObject(pEvent,
                            pTopology);
        if FAILED(hr) then
          begin
            hr:= S_OK;
            goto done;
          end;

        hr:= pTopology.GetTopologyID(PTopoID);
        if FAILED(hr) then
          goto done;

        hr:= LookupTopoID(PTopoID,
                          SegmentIndex);
        if FAILED(hr) then
          goto done;

        m_ActiveSegment:= SegmentIndex;
        hr:= GetDurationFromTopology(pTopology,
                                     m_hnsSegmentDuration);
      end;

    DWORD(MF_TOPOSTATUS_ENDED):   // Playback of this topology is complete.
        begin                     // The Media Session might still use the topology internally.
          m_ActiveSegment:= -1;   // The Media Session does not completely release the topology until it sends the next
        end;                      // MF_TOPOSTATUS_STARTED_SOURCE status event or the MESessionEnded event.

    DWORD(MF_TOPOSTATUS_READY):
      begin
        if (State = OpenPending) then
          begin
            State:= Stopped;
          end;
      end;
  end;

done:
  SafeRelease(pTopology);
  Result:= hr;

end;

//
function TMfPlayList.OnNewPresentation(pEvent: IMFMediaEvent): HResult;
var
  pPD: IMFPresentationDescriptor;
  hr: HResult;

begin
  pPD:= Nil;

  hr:= GetEventObject(pEvent,
                      pPD);

  if SUCCEEDED(hr) then
    begin
      // Queue the next segment on the media session
      hr:= QueueNextSegment(pPD);
    end;

  SafeRelease(pPD);
  Result:= hr;

end;

// The AddSegment method adds a new playlist segment.
// The AddSegment method is private to CPlaylist, and is called from function AddToPlaylist
//  This method performs the following steps:
//   1 Creates a playback topology. The code for this step is shown in the topic Creating Playback Topologies.
//   2 Calls IMFSequencerSource.AppendTopology to add the topology to the playlist.
//   3 On the first segment, gets the value of the MF_PD_DURATION attribute, which contains the playback duration.
//   4 Stores the segment ID and topology ID in a lookup table.
function TMfPlayList.AddSegment(pszURL: PCWSTR): HResult;
var
  pMediaSource: IMFMediaSource;
  pPD: IMFPresentationDescriptor;
  pTopology: IMFTopology;
  SegmentId: MFSequencerElementId;
  TopologyID: TOPOID;
  hr: HResult;

label
  done;

begin
  pMediaSource:= Nil;
  pPD:= Nil;
  pTopology:= Nil;
  TopologyID:= 0;

  hr:= CreateMediaSourceFromUrl(pszURL,
                                pMediaSource);
  if FAILED(hr) then
    goto done;


  hr:= pMediaSource.CreatePresentationDescriptor(pPD);
  if FAILED(hr) then
    goto done;


  hr:= CreatePlaybackTopology(pMediaSource,
                              pPD,
                              m_hwndVideo,
                              pTopology);
  if FAILED(hr) then
    goto done;

  hr:= m_pSequencerSource.AppendTopology(pTopology,
                                         DWORD(SequencerTopologyFlags_Last),
                                         SegmentId);
  if FAILED(hr) then
    goto done;

  hr:= pTopology.GetTopologyID(TopologyID);
  if FAILED(hr) then
    goto done;


  if (m_count = 0) then
    begin
      // Get the segment duration
      m_hnsSegmentDuration:= MFGetAttributeUINT64(pPD,
                                                  MF_PD_DURATION,
                                                  0);
    end;

  m_segments[m_count].SegmentID:= SegmentId;
  m_segments[m_count].TopoID:= TopologyID;

  inc(m_count);

done:
  SafeRelease(pMediaSource);
  SafeRelease(pTopology);
  SafeRelease(pPD);
  Result:= hr;
end;

//
// When you create the first playlist segment, you must queue the segment topology
// on the Media Session, as follows:
// 1 Query the sequencer source for the IMFMediaSourceTopologyProvider interface.
// 2 Pass the presentation descriptor to the IMFMediaSourceTopologyProvider.GetMediaSourceTopology method.
//   This method gets a pointer to the segment topology.
//   Note that this topology is not exactly the same as the playback topology that
//   you created earliers. Instead, it is a modified version of that topology.
//   For more information, see About the Sequencer Source.
// 3 Queue the topology on the Media Session by calling IMFMediaSession.SetTopology.
//
function TMfPlayList.QueueNextSegment(pPD: IMFPresentationDescriptor): HResult;
var
  hr: HResult;
  pTopoProvider: IMFMediaSourceTopologyProvider;
  pTopology: IMFTopology;

label
  done;

begin
  pTopoProvider:= Nil;
  pTopology:= Nil;

  //Get the topology for the presentation descriptor
  hr:= m_pSequencerSource.QueryInterface(IID_IMFMediaSourceTopologyProvider,
                                         pTopoProvider);
  if FAILED(hr) then
    goto done;

  hr:= pTopoProvider.GetMediaSourceTopology(pPD,
                                            pTopology);
  if FAILED(hr) then
    goto done;

  //Set the topology on the media session
  m_pSession.SetTopology(0,
                         pTopology);

done:
  SafeRelease(pTopoProvider);
  SafeRelease(pTopology);
  Result:= hr;
end;

//
function TMfPlayList.AddToPlaylist(pszURL: PCWSTR): HResult;
var
  hr: HResult;
  pPD: IMFPresentationDescriptor;
  bFirstSegment: Boolean;

label
  done;

begin
  if (NumSegments >= MAX_PLAYLIST_SEGMENTS) then
    begin
      Result:= MF_E_OUT_OF_RANGE;
      Exit;
    end;

  pPD:= Nil;
  bFirstSegment:= (NumSegments = 0);


  if (bFirstSegment = True) then
    begin
      // Remove the "last segment" flag from the last segment.
      hr:= m_pSequencerSource.UpdateTopologyFlags(LastSegment(),
                                                  0);
      if FAILED(hr) then
        goto done;
    end;

  // Create the topology and add it to the sequencer.
  hr:= AddSegment(pszURL);
  if FAILED(hr) then
    goto done;

  // If this is the first segment, queue it on the session.
  if (bFirstSegment) then
    begin
      hr:= m_pSource.CreatePresentationDescriptor(pPD);
      if FAILED(hr) then
        goto done;

      hr:= QueueNextSegment(pPD);
      if FAILED(hr) then
        goto done;
    end;

  if (State < Started) then
    State:= OpenPending;

done:
    SafeRelease(pPD);
    Result:= hr;
end;

//
// To delete a playlist segment, call IMFSequencerSource.DeleteTopology.
// The segment is specified by segment ID.
// (This is why the application should cache the list of segment IDs.)
//
function TMfPlayList.DeleteSegment(const dwindex: Integer): HResult;
var
  LastSegId: MFSequencerElementId;
  SegmentID: MFSequencerElementId;
  hr: HResult;
  i: integer;

label
  done;

begin
  if (dwindex >= m_count) then
    begin
      Result:= E_INVALIDARG;
      Exit;
    end;

  if (dwindex = m_ActiveSegment) then
    begin
      Result:= E_INVALIDARG;
      Exit;
    end;

  LastSegId:= LastSegment();
  SegmentID:= m_segments[dwindex].SegmentID;

  hr:= m_pSequencerSource.DeleteTopology(SegmentID);
  if FAILED(hr) then
    goto done;

  //Delete the segment entry from the list.

  // Move everything up one slot.
  for i:= dwindex to m_count - 1 do
    begin
      m_segments[i]:= m_segments[i + 1];
    end;

  dec(m_count);

  // Is the deleted topology the last one?
  if (LastSegId = SegmentID) then
    begin
      //Get the new last segment id
      LastSegId:= LastSegment();

      //set this topology as the last in the sequencer
      hr:= m_pSequencerSource.UpdateTopologyFlags(LastSegId,
                                                  DWORD(SequencerTopologyFlags_Last));
    end;

done:
  Result:= hr;

end;

//
function TMfPlayList.SkipTo(const dwindex: Integer): HResult;
var
  ID: MFSequencerElementId;
  pvar: PROPVARIANT;
  hr: HResult;

begin
  if (dwindex >= m_count) then
    begin
      Result:= E_INVALIDARG;
      Exit;
    end;

  ID:= m_segments[dwindex].SegmentID;

  hr:= MFCreateSequencerSegmentOffset(ID,
                                      0,
                                      pvar);
  if SUCCEEDED(hr) then
    begin
      hr:= m_pSession.Start(MF_TIME_FORMAT_SEGMENT_OFFSET,
                            pvar);
      PropVariantClear(pvar);
    end;

  Result:= hr;

end;

//
function TMfPlayList.GetPlaybackTime(out phnsPresentation: MFTIME;
                                     out phnsSegment: MFTIME): HResult;
var
  hr: HResult;

begin
  phnsPresentation:= 0;
  phnsSegment:= 0;

  hr:= m_pPresentationClock.GetTime(phnsPresentation);
  if SUCCEEDED(hr) then
    phnsSegment:= phnsPresentation - m_PresentationTimeOffset;

  Result:= hr;
end;

//
function TMfPlayList.Initialize(): HResult;
var
  pClock: IMFClock;
  hr: HResult;

label
  done;

begin
  pClock:= Nil;

  hr:= CreateSession();
  if FAILED(hr) then
    goto done;

  // Create an instance of the sequencer Source.
  hr:= MFCreateSequencerSource(Nil,
                               m_pSequencerSource);
  if FAILED(hr) then
    goto done;

  hr:= m_pSequencerSource.QueryInterface(IID_IMFMediaSource,
                                         m_pSource);
  if FAILED(hr) then
    goto done;

  // Get the presentation clock.
  hr:= m_pSession.GetClock(pClock);
  if FAILED(hr) then
    goto done;

  hr:= pClock.QueryInterface(IID_IMFPresentationClock,
                             m_pPresentationClock);

done:
  SafeRelease(pClock);
  Result:= hr;
end;


// constructor
constructor TMfPlayList.Create(out hres: HResult);
begin
  inherited Create();

  m_pSequencerSource:= Nil;
  m_pPresentationClock:= Nil;
  m_PresentationTimeOffset:= 0;
  m_ActiveSegment:= -1;
  m_hnsSegmentDuration:= 0;
  m_count:= 0;

  hres:= Initialize();
end;

// destructor
destructor TMfPlayList.Destroy;
begin
  inherited Destroy;
end;

//
procedure TMfPlayList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  SafeRelease(m_pSequencerSource);
  SafeRelease(m_pPresentationClock);
end;

end.
