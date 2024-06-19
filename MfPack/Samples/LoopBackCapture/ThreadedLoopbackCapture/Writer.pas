// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Writer.pas
// Kind: Pascal / Delphi unit
// Release date: 15-06-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: A basic WAVfile writer using MMIO.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
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
//==============================================================================
// Source: Microsoft.
//
// Copyright (c) FactoryX. All rights reserved.
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
unit Writer;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.Classes,
  System.SysUtils,
  {WinMM}
  WinApi.WinMM.MMReg,
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMSysCom,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;

const
  MAX_FILE_SIZE = 3900000000;

  // WAV fileheader identifiers.
  // FOURCC_RIFF is declared in WinApi.WinMM.MMiscApi.
  FOURCC_WAVE  = FOURCC(ord('W') or ord('A') shl 8 or (ord('V') shl 16) or (ord('E') shl 24));
  FOURCC_fmt   = FOURCC(ord('f') or ord('m') shl 8 or (ord('t') shl 16) or (ord(' ') shl 24));
  FOURCC_fact  = FOURCC(ord('f') or ord('a') shl 8 or (ord('c') shl 16) or (ord('t') shl 24));
  FOURCC_data  = FOURCC(ord('d') or ord('a') shl 8 or (ord('t') shl 16) or (ord('a') shl 24));

  // User defined MMIOERR.
  MMIOERR_HRFAIL_FILE_CLOSED = (MMIOERR_BASE + 100);

type

  TFileStatus = (fsClosed, fsOpen);
  //
  //  WAV file writer class
  //

  TWavWriter = class(TObject)
  protected
    hmFile: HMMIO;

  private
    pvTotalBytesWritten: Int64;
    pvCkRIFF: MMCKINFO;
    pvCkData: MMCKINFO;
    pvFileStatus: TFileStatus;

  public

    constructor Create();
    destructor Destroy(); override;

    //
    // Usage:
    // 1) CreateFile > will call WriteWaveHeader.
    // 2) WriteData
    // 3) CloseFile
    // Note: CloseFile will also be called, when the source object reports a HResult value <> S_OK.

    function CreateFile(ppfileName: LPWSTR): MMRESULT;

    function WriteWaveHeader(ppwfx: PWAVEFORMATEX;
                             var pckRIFF: MMCKINFO;
                             var pckData: MMCKINFO): MMRESULT;

    function CloseFile(var pckRIFF: MMCKINFO;
                       var pckData: MMCKINFO): MMRESULT;

    function WriteData(pData: PByte;
                       pNumFrames: LongInt;
                       pBlockAlign: Word;
                       out pBytesWritten: LongInt;
                       pObjHResult: HRESULT = S_OK): MMRESULT;

    property FileStatus: TFileStatus read pvFileStatus;
    property TotalBytesWritten: Int64 read pvTotalBytesWritten;
  end;


implementation

uses
  Common;

constructor TWavWriter.Create();
begin

  inherited Create();
end;


destructor TWavWriter.Destroy();
begin

  inherited Destroy();
end;


function TWavWriter.CreateFile(ppfileName: LPWSTR): MMRESULT;
var
  mi: PMMIOINFO;

begin
  pvFileStatus := fsClosed;
  pvTotalBytesWritten := 0;


  // The mmioOpen() function is NOT deprecated as documentation says.

  // Must initialize PMMIOINFO = nil, otherwise mmioOpen wil raise a pointer error.
  mi := nil;
  hmFile := mmioOpen(ppFileName,    // some flags cause mmioOpen write to this buffer
                     mi,            // but not any that we're using.
                     MMIO_WRITE or MMIO_CREATE);

  if (hmFile = 0) then
    Exit(MMIOERR_CANNOTOPEN);

  pvFileStatus := fsOpen;
  Result := MMSYSERR_NOERROR;
end;


function TWavWriter.WriteWaveHeader(ppwfx: PWAVEFORMATEX;
                                    var pckRIFF: MMCKINFO;
                                    var pckData: MMCKINFO): MMRESULT;
var
  mResult: MMRESULT;
  mChunk: MMCKINFO;
  iBytesInWfx: Integer;
  iBytesWritten: Integer;
  dwFrames: DWORD;

begin

  // Format the RIFF/WAVE chunk.

  pckRIFF.ckid := FOURCC_RIFF;
  pckRIFF.fccType := FOURCC_WAVE;

  mResult := mmioCreateChunk(hmFile,
                             @pckRIFF,
                             MMIO_CREATERIFF);
  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  pvCkRIFF := pckRIFF;

  // Create the 'fmt ' chunk (within the RIFF/WAVE chunk).
  mChunk.ckid := FOURCC_fmt;
  mResult := mmioCreateChunk(hmFile,
                             @mChunk,
                             0);
  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  // Write the WAVEFORMATEX data to it.
  iBytesInWfx := SizeOf(WAVEFORMATEX) + ppwfx.cbSize;
  iBytesWritten := mmioWrite(hmFile,
                             PAnsiChar(ppwfx),
                             iBytesInWfx);

  if (iBytesWritten <> iBytesInWfx) then
    Exit(mResult);

  // Ascend from the 'fmt ' chunk.
  mResult := mmioAscend(hmFile,
                        @mChunk,
                        0);
  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  // Create the 'fact' chunk whose data is DWORD(0).
  mChunk.ckid := FOURCC_fact;
  mResult := mmioCreateChunk(hmFile,
                             @mChunk,
                             0);
  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  // Write DWORD(0) to it.
  // This will be cleaned up later.
  dwFrames := 0;
  iBytesWritten := mmioWrite(hmFile,
                             PAnsiChar(@dwFrames),
                             SizeOf(dwFrames));
  if (iBytesWritten <> SizeOf(dwFrames)) then
    Exit(mResult);

  // Ascend from the 'fact' chunk.
  mResult := mmioAscend(hmFile,
                        @mChunk,
                        0);

  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  // Create the 'data' chunk and leave the data pointer there.
  pckData.ckid := FOURCC_data;

  mResult := mmioCreateChunk(hmFile,
                             @pckData,
                             0);
  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  pvCkData := pckData;

  Result := MMSYSERR_NOERROR;
end;


function TWavWriter.CloseFile(var pckRIFF: MMCKINFO;
                              var pckData: MMCKINFO): MMRESULT;
var
  mResult: MMRESULT;

begin

  mResult := mmioAscend(hmFile,
                        @pckData,
                        0);
  if (mResult <> MMSYSERR_NOERROR) then
    Exit(mResult);

  mResult := mmioAscend(hmFile,
                        @pckRIFF,
                        0);
  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  // Close the file handle.
  mResult := mmioClose(hmFile,
                       0);
  if (MMSYSERR_NOERROR <> mResult) then
    Exit(mResult);

  Result := MMSYSERR_NOERROR;
end;


function TWavWriter.WriteData(pData: PByte;
                              pNumFrames: LongInt;
                              pBlockAlign: Word;
                              out pBytesWritten: LongInt;
                              pObjHResult: HRESULT = S_OK): MMRESULT;
var
  bytesToWrite: Integer;
  bytesWritten: Integer;

begin

  if (pNumFrames = 0) then
    Exit(MMIOERR_OUTOFMEMORY);

  if (pvFileStatus = fsClosed) then
    Exit(MMIOERR_CANNOTOPEN);

  if FAILED(pObjHResult) then
    begin
      CloseFile(pvCkRIFF,
                pvCkData);
      Exit(MMIOERR_HRFAIL_FILE_CLOSED);
    end;

  bytesToWrite := (pNumFrames * pBlockAlign);
  bytesWritten := mmioWrite(hmFile,
                            PAnsiChar(pData),
                            bytesToWrite);

  if (bytesToWrite <> bytesWritten) then
    Exit(MMIOERR_CANNOTWRITE);

  // Keep score of written bytes to file.
  Inc(pvTotalBytesWritten,
      bytesWritten);

  pBytesWritten := bytesWritten;

  // For safety on 32bit platforms we have to limit the wav size to < 4 gb.
  // So, we limit the size to 3.9 GB. because we don't know how large the last buffer will be.
  if (pvTotalBytesWritten > MAX_FILE_SIZE) then
    begin
      // Close the file.
      if CloseFile(pvCkRIFF,
                   pvCkData) = 0 then
        pvFileStatus := fsClosed;

      Exit(MMIOERR_CANNOTEXPAND);
    end;
  Result := MMSYSERR_NOERROR;
end;

end.
