// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MMStream.pas
// Kind: Pascal / Delphi unit
// Release date: 11-07-2012
// Language: ENU
//
// Revision Version: 3.0.1
// Description: See: Remarks
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//          The interfaces in this header are deprecated. New applications should not use them.
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: mmstream.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//==============================================================================
unit WinApi.MmStream;

  {$HPPEMIT '#include "mmstream.h"'}

interface

uses

  {WinApi}
	WinApi.Windows;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


	function MS_ERROR_CODE(x: Cardinal): Cardinal;
	{$EXTERNALSYM MS_ERROR_CODE}

	function MS_SUCCESS_CODE(x: Cardinal): Cardinal;
	{$EXTERNALSYM MS_SUCCESS_CODE}


const

	MS_S_PENDING                = $00040001;
	{$EXTERNALSYM MS_S_PENDING}
	MS_S_NOUPDATE               = $00040002;
	{$EXTERNALSYM MS_S_NOUPDATE}
	MS_S_ENDOFSTREAM            = $00040003;
	{$EXTERNALSYM MS_S_ENDOFSTREAM}
	MS_E_SAMPLEALLOC            = $80040401;
	{$EXTERNALSYM MS_E_SAMPLEALLOC}
	MS_E_PURPOSEID              = $80040402;
	{$EXTERNALSYM MS_E_PURPOSEID}
  MS_E_NOSTREAM               = $80040403;
  {$EXTERNALSYM MS_E_NOSTREAM}
  MS_E_NOSEEKING              = $80040404;
  {$EXTERNALSYM MS_E_NOSEEKING}
  MS_E_INCOMPATIBLE           = $80040405;
  {$EXTERNALSYM MS_E_INCOMPATIBLE}
  MS_E_BUSY                   = $80040406;
  {$EXTERNALSYM MS_E_BUSY}
  MS_E_NOTINIT                = $80040407;
  {$EXTERNALSYM MS_E_NOTINIT}
  MS_E_SOURCEALREADYDEFINED   = $80040408;
  {$EXTERNALSYM MS_E_SOURCEALREADYDEFINED}
  MS_E_INVALIDSTREAMTYPE      = $80040409;
  {$EXTERNALSYM MS_E_INVALIDSTREAMTYPE}
  MS_E_NOTRUNNING             = $8004040A;
  {$EXTERNALSYM MS_E_NOTRUNNING}


	MSPID_PrimaryVideo                   : TGuid = '{A35FF56A-9FDA-11d0-8FDF-00C04FD9189D}';
	{$EXTERNALSYM MSPID_PrimaryVideo}
  MSPID_PrimaryAudio                   : TGuid = '{A35FF56B-9FDA-11d0-8FDF-00C04FD9189D}';
  {$EXTERNALSYM MSPID_PrimaryAudio}

type

	PAPCFUNC = procedure(dwParam: DWORD); stdcall;
	{$EXTERNALSYM PAPCFUNC}

	PStreamTime = ^STREAM_TIME;
  STREAM_TIME = LONGLONG;
  {$EXTERNALSYM STREAM_TIME}

  PMspid = ^MSPID;
  MSPID = TGUID;

  PRefmspid = ^REFMSPID;
	REFMSPID = TGUID;
	{$EXTERNALSYM REFMSPID}


  PStreamType = ^cwSTREAM_TYPE;
  cwSTREAM_TYPE = (
    STREAMTYPE_READ      = 0,
    STREAMTYPE_WRITE     = 1,
    STREAMTYPE_TRANSFORM = 2
  );
  {$EXTERNALSYM cwSTREAM_TYPE}
  STREAM_TYPE = cwSTREAM_TYPE;
  {$EXTERNALSYM STREAM_TYPE}


  PStreamState = ^cwSTREAMSTATE_STOP;
  cwSTREAMSTATE_STOP = (
    STREAMSTATE_STOP = 0,
    STREAMSTATE_RUN  = 1
  );
  {$EXTERNALSYM cwSTREAMSTATE_STOP}
  STREAM_STATE = cwSTREAMSTATE_STOP;
  {$EXTERNALSYM STREAM_STATE}

type
  PCompletionStatusFlags = ^cwCOMPLETION_STATUS_FLAGS;
  cwCOMPLETION_STATUS_FLAGS = Dword;
  {$EXTERNALSYM cwCOMPLETION_STATUS_FLAGS}
  COMPLETION_STATUS_FLAGS = cwCOMPLETION_STATUS_FLAGS;
  {$EXTERNALSYM COMPLETION_STATUS_FLAGS}
const
    COMPSTAT_NOUPDATEOK = COMPLETION_STATUS_FLAGS($1);
    COMPSTAT_WAIT       = COMPLETION_STATUS_FLAGS($2);
    COMPSTAT_ABORT      = COMPLETION_STATUS_FLAGS($4);

type
  PMMSSF = ^cwMMSSF;
  cwMMSSF = (
    MMSSF_HASCLOCK	    = $1,
	  MMSSF_SUPPORTSEEK	  = $2,
	  MMSSF_ASYNCHRONOUS	= $4
  );
  {$EXTERNALSYM cwMMSSF}
  MMSSF = cwMMSSF;
  {$EXTERNALSYM MMSSF}

  PSSUPDATE = ^cwSSUPDATE;
  cwSSUPDATE = (
    SSUPDATE_ASYNC	    = $1,
	  SSUPDATE_CONTINUOUS	= $2
  );
  {$EXTERNALSYM cwSSUPDATE}
  SSUPDATE = cwSSUPDATE;
  {$EXTERNALSYM SSUPDATE}


type

	// Forward Interface Declarations

	PIMultiMediaStream = ^IMultiMediaStream;
	IMultiMediaStream = interface;

	PIMediaStream = ^IMediaStream;
	IMediaStream = interface;

	PIStreamSample = ^IStreamSample;
	IStreamSample = interface;


  // INTERFACES ////////////////////////////////////////////////////////////////


  // Interface IMultiMediaStream
  // ===========================
  // This interface is deprecated. New applications should not use it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMultiMediaStream);'}
  {$EXTERNALSYM IMultiMediaStream}
  IMultiMediaStream = interface(IUnknown)
  ['{B502D1BC-9A57-11d0-8FDE-00C04FD9189D}']
    function GetInformation(out pdwFlags: DWORD;
                            out pStreamType: STREAM_TYPE): HResult; stdcall;

		function GetMediaStream(const idPurpose: REFMSPID;
                            out pMediaStream: IMediaStream): HResult; stdcall;

		function EnumMediaStreams(Index: Longint;
                              out ppMediaStream: IMediaStream): HResult; stdcall;

		function GetState(out pCurrentState: STREAM_STATE ): HResult; stdcall;

		function SetState(NewState: STREAM_STATE): HResult; stdcall;

    function GetTime(out pCurrentTime: STREAM_TIME): HResult; stdcall;

    function GetDuration(out pDuration: STREAM_TIME): HResult; stdcall;

    function Seek(SeekTime: STREAM_TIME): HResult; stdcall;

    function GetEndOfStreamEventHandle(out phEOS: THandle): HResult; stdcall;

  end;
  IID_IMultiMediaStream = IMultiMediaStream;
  {$EXTERNALSYM IID_IMultiMediaStream}


  // Interface IMediaStream
  // ======================
  // This interface is deprecated. New applications should not use it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMediaStream);'}
  {$EXTERNALSYM IMediaStream}
  IMediaStream = interface(IUnknown)
	['{B502D1BD-9A57-11d0-8FDE-00C04FD9189D}']
		function GetMultiMediaStream(var ppMultiMediaStream: IMultiMediaStream): HResult; stdcall;

    function GetInformation(out pPurposeId: MSPID;
                            out pType: STREAM_TYPE): HResult; stdcall;

		function SetSameFormat(pStreamThatHasDesiredFormat: IMediaStream;
                           dwFlags: DWORD {Must be zero}): HResult; stdcall;

		function AllocateSample(dwFlags: DWORD; // Must be zero.
                            out ppSample: IStreamSample): HResult; stdcall;

    function CreateSharedSample(pExistingSample: IStreamSample;
                                dwFlags: DWORD; // Reserved for flag data. Must be zero.
                                out ppNewSample: IStreamSample): HResult; stdcall;

    function SendEndOfStream(dwFlags: DWORD {Must be zero}): HResult; stdcall;

  end;
  IID_IMediaStream = IMediaStream;
  {$EXTERNALSYM IID_IMediaStream}


  // Interface IStreamSample
  // =======================
  // Note This interface is deprecated. New applications should not use it.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStreamSample);'}
  {$EXTERNALSYM IStreamSample}
	IStreamSample = interface(IUnknown)
  ['{B502D1BE-9A57-11d0-8FDE-00C04FD9189D}']
		function GetMediaStream(ppMediaStream: IMediaStream): HResult; stdcall;

		function GetSampleTimes(out pStartTime: STREAM_TIME;
                            out pEndTime: STREAM_TIME;
                            out pCurrentTime: STREAM_TIME): HResult; stdcall;

		function SetSampleTimes(pStartTime: STREAM_TIME;
                            pEndTime: STREAM_TIME): HResult; stdcall;

		function Update(dwFlags: DWORD;  // Flag that specifies whether the update is synchronous or asynchronous. The SSUPDATE_ASYNC flag specifies an asynchronous update, which you can set if both hEvent and pfnAPC are nil. Use SSUPDATE_CONTINUOUS to continuously update the sample until you call the IStreamSample.CompletionStatus method.
                    hEvent: THANDLE;
                    pfnAPC: PAPCFUNC;
                    dwAPCData: DWORD_PTR): HResult; stdcall;

		function CompletionStatus(dwFlags: DWORD;
                              dwMilliseconds: DWORD): HResult; stdcall;

  end;
  IID_IStreamSample = IStreamSample;
  {$EXTERNALSYM IID_IStreamSample}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


function MS_ERROR_CODE(x: Cardinal): Cardinal;
begin
	// MakeResult = Synonym for MAKE_HRESULT macro
	Result := MakeResult(1, FACILITY_ITF, x + $400);
end;

function MS_SUCCESS_CODE(x: Cardinal): Cardinal;
begin
	// MakeResult = Synonym for MAKE_HRESULT macro
	Result := MakeResult(0, FACILITY_ITF, x);
end;

  // Implement Additional Prototypes here.

end.
