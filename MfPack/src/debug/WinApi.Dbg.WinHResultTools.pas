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
// Revision Version: 3.1.5
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
// 09/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Please read the readme.md about how to use this tool.
//
// Related objects: -
// Related projects: MfPackX315
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
  SDK_API_VERSION = '10.0.22621.0';  // Latest Win 11 API version.
  // When needed, copy newer versions to ../MfPack/Tools/MicrosoftErrorLookupTool folder and
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
  {WinApi}
  WinApi.ShellAPI,
  {Vcl}
  Vcl.Forms,
  Vcl.Clipbrd,
  {Application}
  HrDlg,
  WinApi.Dbg.WinHResult,
  WinApi.Dbg.WinError32,
  WinApi.Dbg.WinMfError,
  WinApi.Dbg.D3DError,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils;


procedure TWinHResultCracker.ClearResults();
begin
  rHrContent.Clear;
end;

// Please read the REadme.md about how to use this function.
function TWinHResultCracker.GetHResultDetails(aHResult: HResult): HResult;
  // Helper
  procedure PrintDbgMsg(msg: string);
  begin
    OutputDebugString(StrToPWideChar(msg));
  end;

var
  hr: Integer;
  i: Integer;

label
  done_HLD;

begin
  hr := S_OK;

  if (InformationStyle = isCode) then
    begin
      // Clear the record
      rHrContent.Clear();

      // Walk through the tables...
      //
      // Start with HResult codes first
      hr := GetHResultDescription(aHResult,
                                  rHrContent.hrStr,
                                  rHrContent.hrDescr,
                                  rHrContent.hrRegionDescr,
                                  rHrContent.HeaderFile,
                                  rHrContent.Reference);
      if (hr = S_OK) then
        begin
          hr := GetHrRegion(aHResult,
                            rHrContent.hrRegionDescr);
          if (hr <> S_OK) then
            rHrContent.hrRegionDescr := 'Unknown Win HResult Region.';
          goto done_HLD;
        end
      else
        rHrContent.Clear();


      // If it's not a positive response code, check if its a win32 error code.
      hr := GetErr32Description(aHResult,
                                rHrContent.hrStr,
                                rHrContent.hrDescr,
                                rHrContent.hrRegionDescr,
                                rHrContent.HeaderFile,
                                rHrContent.Reference);
      if SUCCEEDED(hr) then
        begin
          hr := GetErr32Region(aHResult,
                               rHrContent.hrRegionDescr);
          if (hr <> S_OK) then
            rHrContent.hrRegionDescr := 'Unknown Win32 error region.';
              goto done_HLD;
        end
      else
        rHrContent.Clear();

      // if it's not a wwin32 error, check if it is a Media Foundation error code.
      // Note that the facility code is "Media Server"
      hr := GetMFHResultDescription(aHResult,
                                    rHrContent.hrStr,
                                    rHrContent.hrDescr,
                                    rHrContent.hrRegionDescr,
                                    rHrContent.HeaderFile,
                                    rHrContent.Reference);
      if SUCCEEDED(hr) then
        begin
          hr := GetMFRegion(aHResult,
                            rHrContent.hrRegionDescr);
          if (hr <> S_OK) then
            rHrContent.hrRegionDescr := 'Unknown Media Foundation error region.';
          goto done_HLD;
        end
      else
        rHrContent.Clear();

      hr := GetD3DErrorDescription(aHResult,
                                   rHrContent.hrStr,
                                   rHrContent.hrDescr,
                                   rHrContent.hrRegionDescr,
                                   rHrContent.HeaderFile,
                                   rHrContent.Reference);
      if SUCCEEDED(hr) then
        begin
          hr := GetD3DRegion(aHResult,
                             rHrContent.hrRegionDescr);
          if (hr <> S_OK) then
            rHrContent.hrRegionDescr := 'Unknown D3D error region.';
            goto done_HLD;
        end
      else
        rHrContent.Clear();
    end;

done_HLD:

  if SUCCEEDED(hr) then
    // Get Facility
    GetFacilityDescription(aHResult,
                           rHrContent.FacilityVal,
                           rHrContent.FacilityTag,
                           rHrContent.FacilityDescr);

  // Get the severity code.
  GetSeverityTag(aHResult);

  if (InformationStyle = isCode) then
    begin
      Result := S_OK;
      Exit;
    end

  // Choosen style is print results in the IDE
  else if (InformationStyle = isIDEDebugWindow) then
    begin
      PrintDbgMsg(Format('Error Look Up Results of $s', [FWinHResultCracker.HResultAsString]));
      PrintDbgMsg('=============================================================');
      PrintDbgMsg(Format('Description:          %s', [FWinHResultCracker.HResultDescription]));
      PrintDbgMsg(Format('Region:               %s', [FWinHResultCracker.HResultRegion]));
      PrintDbgMsg('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -');
      PrintDbgMsg(Format('Facility Tag:         %s', [FWinHResultCracker.FacilityTagAsString]));
      PrintDbgMsg(Format('Facility Description: %s', [FWinHResultCracker.FacilityDescription]));
      PrintDbgMsg('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -');
      PrintDbgMsg(Format('Severety Tag:         %s', [FWinHResultCracker.SeveretyValue]));
      PrintDbgMsg(Format('Severety Description: %s', [FWinHResultCracker.SeveretyDescription]));
      PrintDbgMsg('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -');
      PrintDbgMsg(Format('Header file:          %s', [FWinHResultCracker.HeaderFile]));
      PrintDbgMsg('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -');
      PrintDbgMsg('Reference: ');
      for i := 0 to Length(rHrContent.Reference) -1 do
        begin
          if (Length(FWinHResultCracker.ReferenceUrls[i]) > 0) then
            if (FWinHResultCracker.ReferenceUrls[i] <> '') then
              PrintDbgMsg(Format('[%d] %s', [i + 1, FWinHResultCracker.ReferenceUrls[i]]));
        end;
      PrintDbgMsg('=============================================================');
    end;
  Result := hr;
end;


function TWinHResultCracker.GetSysErrorMessage(aHResult: HResult;
                                               UseGetLastError: Boolean = False): string;
begin
  if UseGetLastError then
    begin
      Result := SysErrorMessage(GetLastError);
    end
  else
    begin
      Result := SysErrorMessage(aHResult);
    end;
end;


// TEST
function TWinHResultCracker.RunWinErrorTool(const CmdLine: string;
                                            WorkingDir: string = 'C:\'): string;
var
  SecurityAttributes: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  ProcessInformation: TProcessInformation;
  StdOutPipeRead: THandle;
  StdOutPipeWrite: THandle;
  Buffer: array[0..1024] of AnsiChar;
  dwBytesRead: DWord;
  bReadSuccess: BOOL;
  bHasHandle: Boolean;

begin
  Result := '';

  // Set the security attributes.
  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;

  try

    if not CreatePipe(StdOutPipeRead,
                      StdOutPipeWrite,
                      @SecurityAttributes,
                      0) then
      RaiseLastOSError;

    FillChar(StartupInfo,
             SizeOf(StartupInfo),
             0);

    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    StartupInfo.hStdOutput := StdOutPipeWrite;
    StartupInfo.hStdError := StdOutPipeWrite;

    //WorkDir := WorkingDir;
    bHasHandle := CreateProcess(nil,
                                PChar('cmd.exe /C ' + cmdLine),
                                nil,
                                nil,
                                True,
                                0,
                                nil,
                                PChar(WorkingDir),
                                StartupInfo,
                                ProcessInformation);

    CloseHandle(StdOutPipeWrite);

    if not bHasHandle then
      RaiseLastOSError
    else
      begin
        try
          repeat
            bReadSuccess := ReadFile(StdOutPipeRead,
                                     Buffer,
                                     SizeOf(Buffer),
                                     dwBytesRead,
                                     nil);

            if (dwBytesRead > 0) then
              begin
                Buffer[dwBytesRead] := #0;
                Result := Result + string(Buffer);
              end;
          until (not bReadSuccess) or (dwBytesRead = 0);

          WaitForSingleObject(ProcessInformation.hProcess,
                              INFINITE);

        finally
          CloseHandle(ProcessInformation.hThread);
          CloseHandle(ProcessInformation.hProcess);
        end;
     end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;


procedure TWinHResultCracker.GetSeverityTag(AHresult: HResult);
var
  iSevTag: LongInt;

begin
  iSevTag := HRESULT_SEVERITY(AHresult);
  rHrContent.SeveretyVal := '$' + IntToHex(iSevTag, 1);

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
  if not Assigned(dlgHrTools) then
    Application.CreateForm(TdlgHrTools,
                           dlgHrTools);
    // Clear the clipboard first.
    Clipboard.Clear();
    // Copy the Hresult value to the clipboard.
    ClipBoard.AsText := IntToStr(aHResult);
    // When the dialog is being created, the value from the clipboard will be automaticly processed.
    // Now we just have to use the Onclick procedure of the Searchbutton..
    dlgHrTools.butSearch.OnClick(nil);
end;

end.
