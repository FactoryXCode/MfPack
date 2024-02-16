// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.StiErr
// Kind: Pascal / Delphi unit
// Release date: 24-11-2023
// Language: ENU
//
// Revision Version: 3.1.6
// Description: Returns code definitions of the WinError32 errors.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/11/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX316
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/debug/error-handling-reference
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.Dbg.StiErr;

interface

uses
  WinApi.Windows,
  WinApi.WinError,
  WinApi.Dbg.WinHResultTools;


  function GetStiErrorDescription(const aHResult: HResult;
                                  out hrStr: string;
                                  out hrDescr: string;
                                  out RegionDescr: string;
                                  out HeaderFile: string;
                                  out Reference: TReferenceArray): HResult;

  function GetStiErrorRegion(aHResult: HResult;
                             out aRegion: string): HResult;


implementation

uses
  System.SysUtils,
  System.StrUtils;


function GetStiErrorDescription(const aHResult: HResult;
                                out hrStr: string;
                                out hrDescr: string;
                                out RegionDescr: string;
                                out HeaderFile: string;
                                out Reference: TReferenceArray): HResult;
var
  hr: HResult;

begin
  hr := S_OK;

  HeaderFile := 'StiErr.h';
  Reference[0] := 'https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes';

  case aHResult of
    LongInt($00000000)  : begin
                            HrStr := 'STI_OK / STI_ERROR_NO_ERROR' ;
                            HrDescr := 'The operation completed successfully.';
                          end;
    LongInt($00000001)  : begin
                            HrStr := 'STI_NOTCONNECTED / STI_CHANGENOEFFECT' ;
                            HrDescr := 'The device exists but not currently attached to the system or' + CRLF +
                                       'The requested change in device mode settings had no effect.';
                          end;
    LongInt($8007047E)  : begin
                            HrStr := 'STIERR_OLD_VERSION' ;
                            HrDescr := 'The application requires newer version.';
                          end;
    LongInt($80070481)  : begin
                            HrStr := 'STIERR_BETA_VERSION' ;
                            HrDescr := 'The application was written for pre-release version of provider DLL.';
                          end;
    LongInt($80070077)  : begin
                            HrStr := 'STIERR_BADDRIVER' ;
                            HrDescr := 'The requested object could not be created due to incompatible or mismatched driver.';
                          end;
    LongInt($80040154)  : begin
                            HrStr := 'STIERR_DEVICENOTREG' ;
                            HrDescr := 'The device is not registered.';
                          end;
    LongInt($80070002)  : begin
                            HrStr := 'STIERR_OBJECTNOTFOUND' ;
                            HrDescr := 'The requested container does not exist.';
                          end;
    LongInt($80070057)  : begin
                            HrStr := 'STIERR_INVALID_PARAM' ;
                            HrDescr := 'An invalid or not state matching parameter was passed to the API.';
                          end;
    LongInt($80004002)  : begin
                            HrStr := 'STIERR_NOINTERFACE' ;
                            HrDescr := 'The specified interface is not supported.';
                          end;
    LongInt($80004005)  : begin
                            HrStr := 'STIERR_GENERIC' ;
                            HrDescr := 'The undetermined error occured.';
                          end;
    LongInt($8007000E)  : begin
                            HrStr := 'STIERR_OUTOFMEMORY' ;
                            HrDescr := 'There is not enough memory to perform requested operation.';
                          end;
    LongInt($80004001)  : begin
                            HrStr := 'STIERR_UNSUPPORTED' ;
                            HrDescr := 'The application called unsupported (at this time) function.';
                          end;
    LongInt($80070015)  : begin
                            HrStr := 'STIERR_NOT_INITIALIZED / STIERR_DEVICE_NOTREADY' ;
                            HrDescr := '- The application requires newer version.' + CRLF +
                                       '- Device appears as not ready.';
                          end;
    LongInt($800704DF)  : begin
                            HrStr := 'STIERR_ALREADY_INITIALIZED' ;
                            HrDescr := 'The application requires newer version.';
                          end;
    LongInt($80070021)  : begin
                            HrStr := 'STIERR_DEVICE_LOCKED' ;
                            HrDescr := 'The operation can not performed while device is locked.';
                          end;
    LongInt($80070005)  : begin
                            HrStr := 'STIERR_READONLY / STIERR_NOTINITIALIZED' ;
                            HrDescr := '- The specified propery can not be changed for this device.' + CRLF +
                                       '- The device already has notification handle associated with it.';
                          end;
    LongInt($8007009E)  : begin
                            HrStr := 'STIERR_NEEDS_LOCK' ;
                            HrDescr := 'The device needs to be locked before attempting this operation.';
                          end;
    LongInt($80070020)  : begin
                            HrStr := 'STIERR_SHARING_VIOLATION' ;
                            HrDescr := 'The device is opened by another application in data mode.';
                          end;
    LongInt($800700B7)  : begin
                            HrStr := 'STIERR_HANDLEEXISTS' ;
                            HrDescr := 'Handle already set for this context.';
                          end;
    LongInt($8007007B)  : begin
                            HrStr := 'STIERR_INVALID_DEVICE_NAME' ;
                            HrDescr := 'Device name is not recognized.';
                          end;
    LongInt($8007000D)  : begin
                            HrStr := 'STIERR_INVALID_HW_TYPE' ;
                            HrDescr := 'Device hardware type is not valid.';
                          end;
    LongInt($80070103)  : begin
                            HrStr := 'STIERR_NOEVENTS' ;
                            HrDescr := 'No events available.';
                          end
    else
      begin
        HrStr := 'Unknown identifier.';
        HrDescr := 'Unknown HResult code.';
        HeaderFile := 'Unknown.';
        FWinHResultCracker.ClearResults();
        hr := aHResult;
      end;
  end;
  Result := hr;
end;


function GetStiErrorRegion(aHResult: HResult;
                           out aRegion: string): HResult;
var
  hr: HResult;
  li: LongInt;
  s: string;

begin
  hr := S_OK;

  // Get the regioncode
  s := IntToHex(aHResult, 8);
  s := '$' + RightStr(s, 4);
  li := StrToInt(s);

  // Find the region.
  if (li >= 0) and (li <= 1000) then
    aRegion := 'Still image APIs error.'
  else
    begin
      hr := ERROR_NOT_FOUND;
      aRegion := 'Unknown Region error.';
    end;
  Result := hr;
end;

end.
