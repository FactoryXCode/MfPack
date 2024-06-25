// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Utils.pas
// Kind: Pascal Unit
// Release date: 18-11-2022
// Language: ENU
//
// Revision Version: 3.1.7
//
// Description:
//   Helpers for the MFCaptureEngineVideoCapture.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Ciaran, Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 Rammstein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX317/Samples/MFCaptureEngineVideoCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit Utils;


interface

  // Undefine this when not needed!
  {$DEFINE SAVE_DEBUG_REPORT}

uses
  {WinApi}
  WinApi.Windows,
  WinAPI.Messages,
  WinApi.ComBaseApi,
  {System}
  System.Classes,
  system.Sysutils,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {Vcl}
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  Vcl.Imaging.jpeg,
  {MediaFoundationApi}
  WinApi.WinApiTypes,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfMetLib;


const
  IDTIMEOUT      = 'Unable to set the capture device.';
  ERR_INITIALIZE = 'Unable to initialize the capture engine.';
  ERR_PREVIEW    = 'An error occurred during preview.';
  ERR_RECORD     = 'An error occurred during recording.';
  ERR_CAPTURE    = 'An error occurred during capture.';
  ERR_PHOTO      = 'Unable to capture still photo.';
  ERR_OUTPUT_MEDIATYPE_SET = 'Unable to set the CaptureEngine MediaType output.';
  ERR_SET_DEVICE   = 'Selecting a device failed.';
  ERR_STOP_PREVIEW = 'Stopping preview failed.';

type

  TChooseDeviceParam = record
    ppDevices: PIMFActivate;
    Count: UINT32;
    SelectedDevice: Integer;
    SelectedFormat: Integer;
    public
      procedure Reset();
  end;

  TFrameDataEvent = procedure(AMemoryStream: TMemoryStream) of object;

  TImageType = (itBitmap,
                itPNG,
                itJPG);

  // CriticalSection
  TMFCritSec = class(TObject)
  private
    FCriticalSection: TRTLCriticalSection;

  public
    constructor Create(); overload;
    destructor Destroy(); override;

    procedure Lock();
    procedure Unlock();

  end;

  // To make earlier versions of TBitmap compatible with Seattle and above.
  // CompilerVersion < Delphi 10 Seattle

  TMfpBitmap = class(TBitmap)
  public
    constructor Create(); overload; override;
    destructor Destroy(); override;

    {$IF CompilerVersion < 30}
    class function CanLoadFromStream(Stream: TStream): Boolean;
    {$ENDIF}

  end;

  procedure SaveImage(const pBitmap: TMfpBitmap;
                      const pPath: string;
                      pType: TImageType);

  // Simple error handler
  procedure ErrMsg(pErrMsg: string;
                   pHr: HResult);

  // See also WinApi.MediaFoundationApi.MfUtils/HnsTimeToStr
  function TranslateHnsTimeToStr(hns: MFTIME;
                                 ShowMilliSeconds: Boolean = True;
                                 DelimiterFormat: string = ':'): string; inline;


  function CloneVideoMediaType(pSrcMediaType: IMFMediaType;
                               const guidSubType: REFGUID;
                               out ppNewMediaType: IMFMediaType): HResult;

  function CopyAttribute(pSrc: IMFAttributes;
                         pDest: IMFAttributes;
                         const key: TGUID): HResult; inline;

  function GetActivationObjects(out ppActivate: PIMFActivate;
                                out pCount: UINT32): HResult;


implementation


procedure TChooseDeviceParam.Reset();
var
  i: Integer;

begin
{$POINTERMATH ON}
  for i := 0 to count -1 do
    SafeRelease(ppDevices[i]);
  CoTaskMemFree(ppDevices);

  Count := 0;
  SelectedDevice := -1;
  SelectedFormat := -1;
end;


{ TMFCritSec }

constructor TMFCritSec.Create();
begin
  inherited Create();
  InitializeCriticalSection(FCriticalSection);
end;


destructor TMFCritSec.Destroy();
begin
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy();
end;


procedure TMFCritSec.Lock();
begin
  EnterCriticalSection(FCriticalSection);
end;


procedure TMFCritSec.Unlock();
begin
  LeaveCriticalSection(FCriticalSection);
end;



// TMfpBitmap ==================================================================

constructor TMfpBitmap.Create();
begin
  inherited;

end;


destructor TMfpBitmap.Destroy();
begin

  inherited;
end;

{$IF CompilerVersion < 30}
class function TMfpBitmap.CanLoadFromStream(Stream: TStream): Boolean;
var
  oBitmapFileHeader: TBitmapFileHeader;
  iPosition: Int64;

begin

  iPosition := Stream.Position;

  try
    Result := (Stream.Size - Stream.Position = 0) or
              (Stream.Read(oBitmapFileHeader,
                           SizeOf(oBitmapFileHeader)) = SizeOf(oBitmapFileHeader)) and
              (oBitmapFileHeader.bfType = $4D42 {BM = 19778});
  finally
    Stream.Position := iPosition;
  end;
end;
{$ENDIF}


procedure SaveImage(const pBitmap: TMfpBitmap;
                    const pPath: string;
                    pType: TImageType);
var
  pPng: TPngImage;
  pJpg: TJPEGImage;

begin
  case pType of
    itBitmap :
      begin
         pBitmap.SaveToFile(pPath);
      end;
    itPNG :
      begin
         pPng := TPngImage.Create;
         pPng.Assign(pBitmap);
         pBitmap.SaveToFile(pPath);

         if Assigned(pPng) then
           pPng.Free;
      end;
    itJPG :
      begin
        pJpg := TJPEGImage.Create;
        // Adjust performance, compression etc.
        pJpg.Performance := jpBestQuality;
        pJpg.ProgressiveEncoding := True;
        pJpg.ProgressiveDisplay := True;
        //pJpg.CompressionQuality := 30;
        pJpg.Compress;
        pJpg.Assign(pBitmap);
        pjpg.SaveToFile(pPath);

        if Assigned(pJpg) then
          pJpg.Free();
      end;
  end;
end;


// ErrMsg
procedure ErrMsg(pErrMsg: string;
                 pHr: HResult);
begin
{$IFDEF SAVE_DEBUG_REPORT}
  OutputDebugString(StrToPWideChar(Format('Error: %s (hr = %d)',
                                          [pErrMsg, pHr])))
{$ELSE}

  ShowMessage(format('Error: %s (hr = %d)',
                     [pErrMsg, pHr]));
{$ENDIF}
end;


// Converts Hns to a time string format
function TranslateHnsTimeToStr(hns: MFTIME;
                               ShowMilliSeconds: Boolean = True;
                               DelimiterFormat: string = ':'): string; inline;
var
  hours,
  mins,
  secs,
  millisec: Word;

begin
try
  hours := hns div MFTIME(36000000000);
  hns := hns mod MFTIME(36000000000);

  mins := hns div 600000000;
  hns := hns mod 600000000;

  secs := hns div 10000000;
  hns := hns mod 10000000;

  millisec := hns div 10000;


  if ShowMilliSeconds then
    Result := Format('%2.2d%s%2.2d%s%2.2d,%3.3d', [hours,
                                                   DelimiterFormat,
                                                   mins,
                                                   DelimiterFormat,
                                                   secs,
                                                   DelimiterFormat,
                                                   millisec])
  else
    Result := Format('%2.2d%s%2.2d%s%2.2d', [hours,
                                             DelimiterFormat,
                                             mins,
                                             DelimiterFormat,
                                             secs]);

except
  on exception do
    Result:= '00:00:00,000';
end;
end;


function CloneVideoMediaType(pSrcMediaType: IMFMediaType;
                             const guidSubType: REFGUID;
                             out ppNewMediaType: IMFMediaType): HResult;
var
  hr: HResult;
  pNewMediaType: IMFMediaType;

label
  done;
begin

  hr := MFCreateMediaType(pNewMediaType);
  if FAILED(hr) then
    goto done;

  hr := pNewMediaType.SetGUID(MF_MT_MAJOR_TYPE,
                              MFMediaType_Video);
  if FAILED(hr) then
    goto done;

  hr := pNewMediaType.SetGUID(MF_MT_SUBTYPE,
                              guidSubType);
  if FAILED(hr) then
    goto done;

  hr := CopyAttribute(pSrcMediaType, pNewMediaType, MF_MT_FRAME_SIZE);
  if FAILED(hr) then
    goto done;

  hr := CopyAttribute(pSrcMediaType, pNewMediaType, MF_MT_FRAME_RATE);
  if FAILED(hr) then
    goto done;

  hr := CopyAttribute(pSrcMediaType, pNewMediaType, MF_MT_PIXEL_ASPECT_RATIO);
  if FAILED(hr) then
    goto done;

  hr := CopyAttribute(pSrcMediaType, pNewMediaType, MF_MT_INTERLACE_MODE);
  if FAILED(hr) then
    goto done;

  ppNewMediaType := pNewMediaType;

done:
  SafeRelease(pNewMediaType);
  Result := hr;
end;


function CopyAttribute(pSrc: IMFAttributes;
                       pDest: IMFAttributes;
                       const key: TGUID): HResult;
var
  hr: HResult;
  pVar: PROPVARIANT;
begin
  PropVariantInit(pVar);
  hr := pSrc.GetItem(key,
                     pVar);

  if SUCCEEDED(hr) then
    begin
      hr := pDest.SetItem(key,
                          pVar);
      PropVariantClear(pVar);
    end;
  Result := hr;
end;


function GetActivationObjects(out ppActivate: PIMFActivate;
                              out pCount: UINT32): HResult;
var
  mfAttributes: IMFAttributes;
  pActivate: PIMFActivate;
  hr: HResult;

begin
  hr := MFCreateAttributes(mfAttributes,
                           1);

  if SUCCEEDED(hr) then
    // Ask for source type = video capture devices
    hr := mfAttributes.SetGUID(MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE,
                               MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

  if SUCCEEDED(hr) then
    // Enumerate devices.
    hr := MFEnumDeviceSources(mfAttributes,
                              pActivate,
                              pCount);

  ppActivate := pActivate;
  SafeRelease(pActivate);
  Result := hr;
end;

end.
