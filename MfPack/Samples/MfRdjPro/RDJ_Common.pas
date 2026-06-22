// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ_Common.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Helpers.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 8 or later.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: -
//
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
unit RDJ_Common;

interface

  // Uncomment this when needed!
  //{$DEFINE SAVE_DEBUG_REPORT}

uses

  {WinApi}
  WinApi.Windows,
  WinAPI.Messages,
  WinApi.DwmApi,
  WinApi.CommDlg,
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {System}
  System.Classes,
  System.Sysutils,
  System.Threading,
  System.SyncObjs,
  {Vcl}
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.StdCtrls,
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

  WM_USERINFO = WM_USER + 1;

  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  DWMWA_BORDER_COLOR            = 34;
  DWMWA_CAPTION_COLOR           = 35;
  DWMWA_TEXT_COLOR              = 36;

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

  // Custom hresult constants.
  E_VIDEO_NOT_PRESENT   = HRESULT($A2000001);
  E_AUDIO_NOT_PRESENT   = HRESULT($A2000002);
  E_DEVICE_NOT_SELECTED = HRESULT($A2000003);
  E_PREVIEW_NOT_RUNNING = HRESULT($A2000004);
  E_VIDEOPROCESSOR_NOT_IMPLEMENTED = HRESULT($A2000005);

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

type

  TMsgOptions  = (optIDE,        // Show info in the IDE Messages
                  optShowMsg);   // Return info in a messagebox

  TLockedMdiChildForm = class(TForm)
  private

    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
  end;

  // Simple error handler that reports by default in the IDE Messages screen or
  // shows a messagedialog at runtime.
  procedure InfoMsg(Options: TMsgOptions;
                    pErrMsg: string;
                    pHr: HResult;
                    hwObj: HWND = 0);

  procedure HandleThreadMessages(AThread: THandle;
                                 AWait: Cardinal = INFINITE);

  function EventWait(EventObj: TEvent;
                     Period: Integer = 100): HResult;

  // Delphi MulDiv replacement for Int64 types.
  function _MulDiv64(const aNumber,
                           aNumerator,
                           aDenominator: Int64): Int64; inline;

  function ClampInt(const AValue,
                  AMin,
                  AMax: Integer): Integer; inline;

  function ClampDouble(const AValue,
                     AMin,
                     AMax: Double): Double; inline;

  // (DWM) Dark Windows Mode API utils.
  procedure ApplyDarkWindowFrame(const AWnd: HWND);
  procedure RefreshDarkWindowFrame(const AWnd: HWND);

  // Lightweight OpenFile dialog.
  function BrowseFile(const AOwner: HWND;
                      const AFilter: PWideChar;
                      const AReturnDirOnly: Boolean;
                      out APath: TFileName): Boolean;

  procedure FreeComboObjects(ACombo: TComboBox);


implementation


// Childform locking -----------------------------------------------------------

procedure TLockedMdiChildForm.WMSysCommand(var Msg: TWMSysCommand);
begin

  if ((Msg.CmdType and $FFF0) = SC_MOVE) then
    Exit;

  inherited;
end;


procedure TLockedMdiChildForm.WMNCLButtonDown(var Msg: TWMNCLButtonDown);
begin

  if (Msg.HitTest = HTCAPTION) then
    Exit;

  inherited;
end;


// ErrMsg
procedure InfoMsg(Options: TMsgOptions;
                  pErrMsg: string;
                  pHr: HResult;
                  hwObj: HWND = 0);
begin


  if (Options = optIDE) then
    OutputDebugString(StrToPWideChar(Format('%s (hr = %d)',
                                            [pErrMsg, pHr])));

  if (Options = optShowMsg) then
    ShowMessage(format('%s (hr = %d)',
                       [pErrMsg, pHr]));

  if (hwObj > 0) then
    SendMessage(hwObj,
                WM_USERINFO,
                WPARAM(Pointer(format('%s (hr = %d)',
                                      [pErrMsg, pHr]))),
                LPARAM(0));
end;


procedure HandleThreadMessages(AThread: THandle;
                               AWait: Cardinal = INFINITE);
var
  oMsg: TMsg;

begin

  while (MsgWaitForMultipleObjects(1,
                                   AThread,
                                   False,
                                   AWait,
                                   QS_ALLINPUT) = WAIT_OBJECT_0 + 1) do
    begin
      PeekMessage(oMsg,
                  0,
                  0,
                  0,
                  PM_REMOVE);

      if oMsg.Message = WM_QUIT then
        Exit;

      TranslateMessage(oMsg);
      DispatchMessage(oMsg);
    end;
end;


function EventWait(EventObj: TEvent;
                   Period: Integer = 100): HResult;
var
  hr: HResult;
  wrWaitResult: TWaitResult;

begin

  // Wait for capture to stop
  wrWaitResult := EventObj.WaitFor(Period);

  case wrWaitResult of
    wrSignaled: hr := S_FALSE; // The signal of the event object was set.
    wrTimeout: hr := ERROR_TIMEOUT; // The time specified by the TimeOut parameter elapsed without the signal being set.
    wrAbandoned: hr := ERROR_TIMEOUT;
    wrError: hr := EventObj.LastError;
    wrIOCompletion: hr := S_OK;
    else
      hr := S_OK;
  end;
  Result := hr;
end;


// NOTE:
// MulDiv is 32-bit (Integer in/out). With range checking on, Delphi throws Range check error.
// If working with Int64, use this method, instead of MulDiv.
// Note: In later MfPack versions (> version 3.18), this method will be declared in WinApi.MediaFoundationApi.MfUtils.
function _MulDiv64(const aNumber,
                         aNumerator,
                         aDenominator: Int64): Int64; inline;
begin

  if (aDenominator <= 0) then
    Exit(0);

  // Avoid Int64 overflow: (QpcDelta * 10_000_000) div PerfFreq
  Result := (aNumber div aDenominator) * aNumerator +
            (aNumber mod aDenominator) * aNumerator div aDenominator;
end;


function ClampInt(const AValue,
                  AMin,
                  AMax: Integer): Integer; inline;
begin

  if (AValue < AMin) then
    Result := AMin
  else if (AValue > AMax) then
    Result := AMax
  else
    Result := AValue;
end;


function ClampDouble(const AValue,
                     AMin,
                     AMax: Double): Double; inline;
begin

  if (AValue < AMin) then
    Result := AMin
  else
    if (AValue > AMax) then
      Result := AMax
    else
      Result := AValue;
end;


// DWM API
function IsWindows11OrGreater22000: Boolean;
var
  Info: TOSVersionInfoEx;

begin

  ZeroMemory(@Info,
             SizeOf(Info));
  Info.dwOSVersionInfoSize := SizeOf(Info);

  Result := GetVersionEx(Info) and
            ((Info.dwMajorVersion > 10) or
             ((Info.dwMajorVersion = 10) and (Info.dwBuildNumber >= 22000)));
end;


procedure ApplyBoolDwmAttr(const AWnd: HWND;
                           const AAttr: DWORD;
                           const AValue: BOOL);
begin

  if (AWnd = 0) then
    Exit;

  DwmSetWindowAttribute(AWnd,
                        AAttr,
                        @AValue,
                        SizeOf(AValue));
end;


procedure ApplyColorDwmAttr(const AWnd: HWND;
                            const AAttr: DWORD;
                            const AColorRef: COLORREF);
begin

  if (AWnd = 0) then
    Exit;

  DwmSetWindowAttribute(AWnd,
                        AAttr,
                        @AColorRef,
                        SizeOf(AColorRef));
end;


procedure RefreshDarkWindowFrame(const AWnd: HWND);
begin

  if (AWnd = 0) then
    Exit;

  SetWindowPos(AWnd,
               0,
               0,
               0,
               0,
               0,
               SWP_NOMOVE or
               SWP_NOSIZE or
               SWP_NOZORDER or
               SWP_NOACTIVATE or
               SWP_FRAMECHANGED);

  RedrawWindow(AWnd,
               nil,
               0,
               RDW_INVALIDATE or
               RDW_FRAME or
               RDW_UPDATENOW);
end;


procedure ApplyDarkWindowFrame(const AWnd: HWND);
var
  UseDark: BOOL;
  CaptionColor: COLORREF;
  BorderColor: COLORREF;
  TextColor: COLORREF;

begin

  if (AWnd = 0) then
    Exit;

  UseDark := True;
  ApplyBoolDwmAttr(AWnd,
                   DWMWA_USE_IMMERSIVE_DARK_MODE,
                   UseDark);

  if IsWindows11OrGreater22000 then
    begin

      CaptionColor := RGB(32,
                          32,
                          32);

      BorderColor := RGB(64,
                         64,
                         64);

      TextColor := RGB(240,
                       240,
                       240);

      ApplyColorDwmAttr(AWnd,
                        DWMWA_CAPTION_COLOR,
                        CaptionColor);

      ApplyColorDwmAttr(AWnd,
                        DWMWA_BORDER_COLOR,
                        BorderColor);

      ApplyColorDwmAttr(AWnd,
                        DWMWA_TEXT_COLOR,
                        TextColor);
    end;

  RefreshDarkWindowFrame(AWnd);
end;


function BrowseFile(const AOwner: HWND;
                    const AFilter: PWideChar;
                    const AReturnDirOnly: Boolean;
                    out APath: TFileName): Boolean;
var
  Ofn: OPENFILENAMEW;
  FileBuf: array[0..MAX_PATH - 1] of WideChar;

const
  ALL_FILTER: PWideChar = 'All Files'#0'*.*'#0#0;

begin
  APath := '';

  ZeroMemory(@Ofn, SizeOf(Ofn));
  ZeroMemory(@FileBuf, SizeOf(FileBuf));

  Ofn.lStructSize := SizeOf(Ofn);
  Ofn.hwndOwner := AOwner;

  if (AFilter = nil) or (AFilter^ = #0) then
    Ofn.lpstrFilter := ALL_FILTER
  else
    Ofn.lpstrFilter := AFilter;

  Ofn.lpstrFile := @FileBuf[0];
  Ofn.nMaxFile := Length(FileBuf);
  Ofn.lpstrTitle := 'Open file';
  Ofn.Flags := OFN_FILEMUSTEXIST or
               OFN_PATHMUSTEXIST or
               OFN_HIDEREADONLY;

  Result := GetOpenFileNameW(Ofn);

  if Result then
    begin

      if AReturnDirOnly then
        APath := ExtractFileDir(string(Ofn.lpstrFile))
      else
        APath := string(Ofn.lpstrFile);
    end;
end;

// Use this to free objects from TCombo, to prevent memory leaks.
procedure FreeComboObjects(ACombo: TComboBox);
var
  i: Integer;

begin

  if not Assigned(ACombo) then
    Exit;

  for i := 0 to ACombo.Items.Count - 1 do
    TObject(ACombo.Items.Objects[i]).Free;

  ACombo.Items.Clear;
end;

//
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

  hr := CopyAttribute(pSrcMediaType,
                      pNewMediaType,
                      MF_MT_FRAME_SIZE);
  if FAILED(hr) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      pNewMediaType,
                      MF_MT_FRAME_RATE);
  if FAILED(hr) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      pNewMediaType,
                      MF_MT_PIXEL_ASPECT_RATIO);
  if FAILED(hr) then
    goto done;

  hr := CopyAttribute(pSrcMediaType,
                      pNewMediaType,
                      MF_MT_INTERLACE_MODE);
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
