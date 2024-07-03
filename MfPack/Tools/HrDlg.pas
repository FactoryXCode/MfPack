//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: HrDlg.pas
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Visual interface to explore the Windows HResult codes.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
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
unit HrDlg;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Clipbrd,
  {DebugApi}
  WinApi.Dbg.WinHResultTools,
  {Application}
  Tools;

type
  TdlgHrTools = class(TForm)
    edHrHex: TEdit;
    butTransToHex: TButton;
    edHrDec: TEdit;
    butTransToDec: TButton;
    mmoDescription: TMemo;
    butClose: TButton;
    cbxFormStyle: TCheckBox;
    Label2: TLabel;
    butSearch: TButton;
    Bevel1: TBevel;
    cbxAutoPaste: TCheckBox;
    rbHLD: TRadioButton;
    Bevel2: TBevel;
    Bevel3: TBevel;
    rbMSELTDESC: TRadioButton;
    rbSEMLTDLG: TRadioButton;
    Label3: TLabel;
    lbxReference: TListBox;
    Bevel4: TBevel;
    edSeverity: TEdit;
    edFacility: TEdit;
    edErrCode: TEdit;
    butMakeHResult: TButton;
    edHResult: TEdit;
    Bevel5: TBevel;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure butTransToHexClick(Sender: TObject);
    procedure butTransToDecClick(Sender: TObject);
    procedure butSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbxFormStyleClick(Sender: TObject);
    procedure AppOnActivate(Sender: TObject);
    procedure lbxReferenceClick(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
    procedure butSearchMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure butMakeHResultClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgHrTools: TdlgHrTools;

implementation

{$R *.dfm}


procedure TdlgHrTools.AppOnActivate(Sender: TObject);
var
 s1: string;
 s2: string;

begin
  if cbxAutoPaste.Checked then
    begin
      if Clipboard.HasFormat(CF_TEXT) then
        begin
          // We do a little workaround to prevent error e2197.
          s1 := edHrHex.Text;
          s2 := edHrDec.Text;
          // Convert to a hex- or decimal value.
          OxValToHexVal(Clipboard.AsText,
                        s1,
                        s2);
          // ..and back again.
          edHrHex.Text := s1;
          edHrDec.Text := s2;
        end;
    end;
end;


procedure TdlgHrTools.butCloseClick(Sender: TObject);
begin
  Close();
end;


procedure TdlgHrTools.butMakeHResultClick(Sender: TObject);
begin

  edHResult.Text :=  Format('%d', [MAKE_HRESULT(StrToIntDef(edSeverity.Text, SEVERITY_ERROR),
                                                StrToIntDef(edFacility.Text, FACILITY_WIN32),
                                                StrToIntDef(edErrCode.Text, S_OK))]);
end;


procedure TdlgHrTools.butSearchClick(Sender: TObject);
var
  s: string;
  cmdLine: string;
  workDir: string;
  i: Integer;
  HrCode: HResult;
  hr: HResult;

begin
  HrCode := 0;
  mmoDescription.Clear;

  if (edHrHex.Text = '') and (edHrDec.Text = '') then
    begin
      mmoDescription.Text:= 'Please, enter a valid resultcode.';
      Exit;
    end;

  if (edHrHex.Text <> '') then
    HrCode := StrToInt64(edHrHex.Text)
  else if (edHrDec.Text <> '') then
    HrCode := StrToInt64(edHrDec.Text);

  // Use Internal Error Lookup Tool.
  if rbHLD.Checked then
    begin
     // Walk through the tables...
     hr := FWinHResultCracker.GetHResultDetails(HrCode);
     if SUCCEEDED(hr) then
       begin
         s :=     Format('HResult:              %s', [FWinHResultCracker.HResultAsString]) + CRLF ;
         s := s + Format('Description:          %s', [FWinHResultCracker.HResultDescription]) + CRLF;
         s := s + Format('Region:               %s', [FWinHResultCracker.HResultRegion]) + CRLF + CRLF;

         s := s + Format('Facility Tag:         %s', [FWinHResultCracker.FacilityTagAsString]) + CRLF;
         s := s + Format('Facility Description: %s', [FWinHResultCracker.FacilityDescription]) + CRLF + CRLF;

         s := s + Format('Severety Tag:         %s', [FWinHResultCracker.SeveretyValue]) + CRLF;
         s := s + Format('Severety Description: %s', [FWinHResultCracker.SeveretyDescription]) + CRLF + CRLF;

         s := s + Format('Header file:          %s', [FWinHResultCracker.HeaderFile]) + CRLF;
         mmoDescription.Text := s;

         // Add references (url's)
         lbxReference.Clear();
         for i := 0 to Length(FWinHResultCracker.ReferenceUrls) - 1 do
           begin
             if (Length(FWinHResultCracker.ReferenceUrls[i]) > 0) then
               if (FWinHResultCracker.ReferenceUrls[i] <> '') then
                 lbxReference.Items.Append(FWinHResultCracker.ReferenceUrls[i]);
           end;

         if (lbxReference.Items.Count = 0) then
           lbxReference.Items.Append('https://learn.microsoft.com');

       end
     else
       mmoDescription.Text := Format('Unknown HResult value. (%d)', [HrCode]);
    end

  // Use Microsoft Error Lookup Tool Command line to string (hides the command line window).
  else if rbMSELTDESC.Checked then
    begin
      {$IFDEF DEBUG}
      cmdLine := ExpandFileName(ExtractFileDir(Application.ExeName) + '\..\..\MicrosoftErrorLookupTool\' + WIN_ERROR_LOOKUP_TOOL);
      workDir := ExpandFileName(ExtractFileDir(Application.ExeName) + '\..\..\MicrosoftErrorLookupTool\');
      {$ELSE}
      cmdLine := ExpandFileName(ExtractFileDir(Application.ExeName) + 'MicrosoftErrorLookupTool\' + WIN_ERROR_LOOKUP_TOOL);
      workDir := ExpandFileName(ExtractFileDir(Application.ExeName) + 'MicrosoftErrorLookupTool\');
      {$ENDIF}

      mmoDescription.Text := FWinHResultCracker.RunWinErrorTool(Format('%s %d', [cmdLine, HrCode]),
                                                                workDir);

    end
  // Use System Error Messager Lookup Tool.
  // This function will return the HResult text in the native language.
  else if rbSEMLTDLG.Checked then
    begin
      mmoDescription.Text := FWinHResultCracker.GetSysErrorMessage(HrCode);
    end;
end;


procedure TdlgHrTools.butSearchMouseMove(Sender: TObject; Shift: TShiftState; X,
                                         Y: Integer);
begin
   AppOnActivate(Self);
end;


procedure TdlgHrTools.butTransToDecClick(Sender: TObject);
begin
  if (Length(edHrHex.Text) > 0) then
    edHrDec.Text := IntToStr(StrToInt(edHrHex.Text));
end;


procedure TdlgHrTools.butTransToHexClick(Sender: TObject);
begin
  if (Length(edHrDec.Text) > 0) then
    begin
      edHrHex.Text := DecimalToHex(edHrDec.Text);
    end;
end;


procedure TdlgHrTools.cbxFormStyleClick(Sender: TObject);
begin
  if cbxFormStyle.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;


procedure TdlgHrTools.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FWinHResultCracker) then
    FWinHResultCracker.Free;
    FWinHResultCracker := nil;
end;


procedure TdlgHrTools.FormCreate(Sender: TObject);
begin
  FWinHResultCracker := TWinHResultCracker.Create();
  Application.OnActivate := AppOnActivate;
  cbxFormStyle.Checked := True;

end;


procedure TdlgHrTools.lbxReferenceClick(Sender: TObject);
var
  Url: string;

begin
  if (lbxReference.ItemIndex < 0) or (lbxReference.Items.Count = 0) then
    Exit;

  Url := lbxReference.Items.Strings[lbxReference.ItemIndex];

  ShellExecute(Handle,
               'open',
               LPWSTR(Url),
               nil,
               nil,
               SW_SHOWNORMAL);
end;

end.
