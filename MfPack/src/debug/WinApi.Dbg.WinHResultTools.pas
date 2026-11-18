//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.WinHResultTools.pas
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 4.1.0
// Description: The main unit to explore the Windows HResult codes.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter Larson (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Please read the readme.md about how to use this tool.
//
// Related objects: -
// Related projects: MfPackX410
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
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
unit WinApi.Dbg.WinHResultTools;

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.ComBaseApi,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {System}
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  {DebugApi}
  WinApi.Dbg.WinFacility,
  {Application}
  Tools;

const
  CRLF = #13 + #10;  // DOS carriage return.
  SDK_API_VERSION = '10.0.28000.2705';  // Latest Win 11 API version.
  // When needed, copy newer versions to ..\MfPack\Tools\MicrosoftErrorLookupTool folder and
  // Adjust the file title here.
  WIN_ERROR_LOOKUP_TOOL = 'Err_6.4.5.exe';

  // Run the FactoryX Error Lookup Tool.
  procedure RunFxErrLookUpTool(const aHResult: HResult);

type

  TReferenceArray = array[0..4] of string;

  THrContent = record
    lHResult: HResult;
    hrStr: string;
    hrDescr: string;
    hrRegionDescr: string;
    FacilityVal: LongInt;
    FacilityTag: string;
    FacilityDescr: string;
    SeveretyVal: string;
    SeveretyDescr: string;
    HeaderFile: string;
    Reference: TReferenceArray;
    public
      procedure Clear();
  end;

  TInformationStyle = (isCode,  // Process the result in your code, using the property values.
                       isIDEDebugWindow); // Process the code within the IDE message window.

  TLookupTool = (lutInternal,
                 lutMSELT,
                 lutEMLT);

  TWinHResultCracker = class(TObject)
  private
    rHrContent: THrContent;
    InformationStyle: TInformationStyle;
    LookUpTool: TLookupTool;

  public

    procedure ClearResults();

    function GetHResultDetails(aHResult: HResult): HResult;
    // Delphi uses build-in SysErrorMessage
    function GetSysErrorMessage(aHResult: HResult;
                                UseGetLastError: Boolean = False): string;

    // Runs the Microsoft Error Lookup Tool in the background and returns it's output.
    // See: https://learn.microsoft.com/en-us/windows/win32/debug/system-error-code-lookup-tool.
    function RunWinErrorTool(const CmdLine: string;
                             WorkingDir: string = 'C:\'): string;

    procedure GetSeverityTag(AHresult: HResult);

    property HResultLookUpTool: TLookupTool read LookUpTool write LookUpTool;
    property SelectedOutput: TInformationStyle read InformationStyle write InformationStyle;

    property HResult: HResult read rHrContent.lHResult;
    property HResultAsString: string read rHrContent.hrStr;
    property HResultDescription: string read rHrContent.hrDescr;
    property HResultRegion: string read rHrContent.hrRegionDescr;
    property FacilityValue: LongInt read rHrContent.FacilityVal;
    property FacilityTagAsString: string read rHrContent.FacilityTag;
    property FacilityDescription: string read rHrContent.FacilityDescr;
    property SeveretyValue: string read rHrContent.SeveretyVal;
    property SeveretyDescription: string read rHrContent.SeveretyDescr;
    property HeaderFile: string read rHrContent.HeaderFile;
    property ReferenceUrls: TReferenceArray read rHrContent.Reference;
  end;

var
  FWinHResultCracker: TWinHResultCracker;


implementation

uses
  {WinApi}  {Vcl}
  Vcl.Forms,  {Application}
  HrDlg,
  WinApi.Dbg.WinHResult,
  WinApi.Dbg.WinError32,
  WinApi.Dbg.WinMfError,
  WinApi.Dbg.D3DError,
  WinApi.Dbg.StiErr,
  WinApi.Dbg.XAudio2Err,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;


procedure TWinHResultCracker.ClearResults();
begin
  rHrContent.Clear;
end;

// Please read the Readme.md about how to use this function.
function TWinHResultCracker.GetHResultDetails(aHResult: HResult): HResult;
var
  Hr: LongInt;
  I: Integer;
begin
  rHrContent.Clear;
  Hr := LongInt($80070490);
  if Hr <> S_OK then
    begin
      rHrContent.Clear;
      Hr := GetHResultDescription(aHResult,rHrContent.hrStr,rHrContent.hrDescr,
                       rHrContent.hrRegionDescr,rHrContent.HeaderFile,rHrContent.Reference);
      if Hr = S_OK then
        GetHrRegion(aHResult,rHrContent.hrRegionDescr);
    end;
  if Hr <> S_OK then
    begin
      rHrContent.Clear;
      Hr := GetErr32Description(aHResult,rHrContent.hrStr,rHrContent.hrDescr,
                       rHrContent.hrRegionDescr,rHrContent.HeaderFile,rHrContent.Reference);
      if Hr = S_OK then
        GetErr32Region(aHResult,rHrContent.hrRegionDescr);
    end;
  if Hr <> S_OK then
    begin
      rHrContent.Clear;
      Hr := GetMFHResultDescription(aHResult,rHrContent.hrStr,rHrContent.hrDescr,
                       rHrContent.hrRegionDescr,rHrContent.HeaderFile,rHrContent.Reference);
      if Hr = S_OK then
        GetMFRegion(aHResult,rHrContent.hrRegionDescr);
    end;
  if Hr <> S_OK then
    begin
      rHrContent.Clear;
      Hr := GetD3DErrorDescription(aHResult,rHrContent.hrStr,rHrContent.hrDescr,
                       rHrContent.hrRegionDescr,rHrContent.HeaderFile,rHrContent.Reference);
      if Hr = S_OK then
        GetD3DRegion(aHResult,rHrContent.hrRegionDescr);
    end;
  if Hr <> S_OK then
    begin
      rHrContent.Clear;
      Hr := GetStiErrorDescription(aHResult,rHrContent.hrStr,rHrContent.hrDescr,
                       rHrContent.hrRegionDescr,rHrContent.HeaderFile,rHrContent.Reference);
      if Hr = S_OK then
        GetStiErrorRegion(aHResult,rHrContent.hrRegionDescr);
    end;
  if Hr <> S_OK then
    begin
      rHrContent.Clear;
      Hr := GetXAudio2ErrorDescription(aHResult,rHrContent.hrStr,rHrContent.hrDescr,
                       rHrContent.hrRegionDescr,rHrContent.HeaderFile,rHrContent.Reference);
      if Hr = S_OK then
        GetXAudio2ErrorRegion(aHResult,rHrContent.hrRegionDescr);
    end;
  if Hr <> S_OK then rHrContent.Clear;
  rHrContent.lHResult := aHResult;
  GetFacilityDescription(aHResult,rHrContent.FacilityVal,rHrContent.FacilityTag,rHrContent.FacilityDescr);
  GetSeverityTag(aHResult);
  if InformationStyle = isIDEDebugWindow then
    begin
      OutputDebugStringW(PWideChar('HRESULT: ' + HResultAsString));
      OutputDebugStringW(PWideChar(HResultDescription));
      OutputDebugStringW(PWideChar(HResultRegion));
      OutputDebugStringW(PWideChar(FacilityTagAsString + ': ' + FacilityDescription));
      OutputDebugStringW(PWideChar(SeveretyValue + ': ' + SeveretyDescription));
      OutputDebugStringW(PWideChar(HeaderFile));
      for I := 0 to Length(rHrContent.Reference) - 1 do
        if rHrContent.Reference[I] <> '' then OutputDebugStringW(PWideChar(rHrContent.Reference[I]));
    end;
  Result := Hr;
end;


function TWinHResultCracker.GetSysErrorMessage(aHResult: HResult;
                                               UseGetLastError: Boolean = False): string;
var
  Code: DWORD;
  Buffer: PWideChar;
  Count: DWORD;
begin
  if UseGetLastError then Code := GetLastError else Code := DWORD(aHResult);
  if (Code and $FFFF0000) = $80070000 then Code := Code and $FFFF;
  Buffer := nil;
  Count := FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or
                           FORMAT_MESSAGE_IGNORE_INSERTS,nil,Code,0,PWideChar(@Buffer),0,nil);
  try
    if Count > 0 then
      begin
        SetString(Result,Buffer,Count);
        Result := Trim(Result);
      end
    else
      Result := 'Unknown system error 0x' + UnicodeString(IntToHex(Code,8));
  finally
    if Buffer <> nil then LocalFree(HLOCAL(Buffer));
  end;
end;


function TWinHResultCracker.RunWinErrorTool(const CmdLine: string;
                                            WorkingDir: string = 'C:\'): string;
var
  SecurityAttributes: TSecurityAttributes;
  StartupInfo: TStartupInfoW;
  ProcessInformation: TProcessInformation;
  StdOutPipeRead: THandle;
  StdOutPipeWrite: THandle;
  Buffer: array[0..1023] of AnsiChar;
  BytesRead: DWORD;
  Command: UnicodeString;
  OutputBytes: AnsiString;
  Chunk: AnsiString;
  Count: Integer;
begin
  Result := '';
  StdOutPipeRead := 0;
  StdOutPipeWrite := 0;
  FillChar(ProcessInformation,SizeOf(ProcessInformation),0);
  FillChar(SecurityAttributes,SizeOf(SecurityAttributes),0);
  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  try
    if not CreatePipe(StdOutPipeRead,StdOutPipeWrite,@SecurityAttributes,0) then
      RaiseLastOSError;
    if not SetHandleInformation(StdOutPipeRead,HANDLE_FLAG_INHERIT,0) then
      RaiseLastOSError;
    FillChar(StartupInfo,SizeOf(StartupInfo),0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    StartupInfo.hStdOutput := StdOutPipeWrite;
    StartupInfo.hStdError := StdOutPipeWrite;
    Command := CmdLine;
    UniqueString(Command);
    if not CreateProcessW(nil,PWideChar(Command),nil,nil,True,0,nil,
                          PWideChar(UnicodeString(WorkingDir)),StartupInfo,ProcessInformation) then
      RaiseLastOSError;
    CloseHandle(StdOutPipeWrite);
    StdOutPipeWrite := 0;
    OutputBytes := '';
    repeat
      BytesRead := 0;
      if not ReadFile(StdOutPipeRead,Buffer,SizeOf(Buffer),BytesRead,nil) then
        begin
          if GetLastError <> ERROR_BROKEN_PIPE then
            RaiseLastOSError;
          Break;
        end;
      if BytesRead = 0 then
        Break;
      SetString(Chunk,PAnsiChar(@Buffer[0]),BytesRead);
      OutputBytes := OutputBytes + Chunk;
    until False;
    WaitForSingleObject(ProcessInformation.hProcess,INFINITE);
    Count := MultiByteToWideChar(CP_OEMCP,0,PAnsiChar(OutputBytes),Length(OutputBytes),nil,0);
    SetLength(Result,Count);
    if Count > 0 then
      MultiByteToWideChar(CP_OEMCP,0,PAnsiChar(OutputBytes),Length(OutputBytes),PWideChar(Result),Count);
  finally
    if ProcessInformation.hThread <> 0 then CloseHandle(ProcessInformation.hThread);
    if ProcessInformation.hProcess <> 0 then CloseHandle(ProcessInformation.hProcess);
    if StdOutPipeWrite <> 0 then CloseHandle(StdOutPipeWrite);
    if StdOutPipeRead <> 0 then CloseHandle(StdOutPipeRead);
  end;
end;


procedure TWinHResultCracker.GetSeverityTag(AHresult: HResult);
var
  iSevTag: LongInt;

begin
  iSevTag := HRESULT_SEVERITY(AHresult);
  rHrContent.SeveretyVal := '$' + UnicodeString(IntToHex(iSevTag,1));

  if (iSevTag = $0) then
    rHrContent.SeveretyDescr := 'SUCCESS'
  else if (iSevTag = $1) then
    rHrContent.SeveretyDescr := 'FAILURE'
  else
    rHrContent.SeveretyDescr := 'API BUG' // The code is a wrong HResult in the API.
end;


procedure THrContent.Clear();
var
  i: Integer;

begin
  lHResult := S_OK;
  hrStr := '';
  hrDescr := '';
  hrRegionDescr := '';
  FacilityVal := 0;
  FacilityTag := '';
  FacilityDescr := '';
  SeveretyVal := '';
  SeveretyDescr := '';
  HeaderFile := '';
  for i := 0 to Length(Reference) -1 do
    Reference[i] := '';
end;


procedure RunFxErrLookUpTool(const aHResult: HResult);
begin
  if not Assigned(dlgHrTools) then Application.CreateForm(TdlgHrTools,dlgHrTools);
  dlgHrTools.Show;
  dlgHrTools.edHrHex.Text := '';
  dlgHrTools.edHrDec.Text := IntToStr(aHResult);
  dlgHrTools.rbHLD.Checked := True;
  dlgHrTools.butSearchClick(nil);
end;

end.
