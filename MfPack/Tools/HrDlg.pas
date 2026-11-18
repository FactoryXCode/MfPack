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
// Revision Version: 4.1.0
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
// 15/11/2026 All                 Eels release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
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
  Vcl.Menus,
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
    procedure FormCloseQuery(Sender: TObject;
                             var CanClose: Boolean);
    procedure cbxFormStyleClick(Sender: TObject);
    procedure AppOnActivate(Sender: TObject);
    procedure lbxReferenceClick(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
    procedure butSearchMouseMove(Sender: TObject;
                                 Shift: TShiftState;
                                 X,
      Y: Integer);
    procedure butMakeHResultClick(Sender: TObject);

  private
    { Private declarations }
  public
    destructor Destroy; override;
    procedure ShowTransforms(Sender: TObject);
  end;

var
  dlgHrTools: TdlgHrTools;

implementation

uses MediaTransformToolDlg;

{$R *.dfm}


function UiText(const Text: UnicodeString): string;
begin
  Result := {$IFDEF FPC}UTF8Encode(Text){$ELSE}Text{$ENDIF};
end;


function UiValue(const Text: string): UnicodeString;
begin
  Result := {$IFDEF FPC}UTF8Decode(Text){$ELSE}Text{$ENDIF};
end;


procedure TdlgHrTools.AppOnActivate(Sender: TObject);
var
  Value: HResult;
  HexText: UnicodeString;
  DecText: UnicodeString;
begin
  if cbxAutoPaste.Checked and TryParseHResult(UiValue(Clipboard.AsText),Value) then
    begin
      OxValToHexVal(UiValue(Clipboard.AsText),HexText,DecText);
      edHrHex.Text := UiText(HexText);
      edHrDec.Text := UiText(DecText);
    end;
end;


procedure TdlgHrTools.butCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TdlgHrTools.butMakeHResultClick(Sender: TObject);
var
  Severity: HResult;
  Facility: HResult;
  Code: HResult;
begin
  edHResult.Clear;
  if not TryParseHResult(UiValue(edSeverity.Text),Severity) or
     not TryParseHResult(UiValue(edFacility.Text),Facility) or
     not TryParseHResult(UiValue(edErrCode.Text),Code) or
     (Severity < 0) or (Severity > 1) or
     (Facility < 0) or (Facility > $1FFF) or
     (Code < 0) or (Code > $FFFF) then
    begin
      mmoDescription.Text := 'Use severity 0..1, facility 0..8191 and code 0..65535.';
      Exit;
    end;
  edHResult.Text := UiText(UnicodeString(IntToStr(MAKE_HRESULT(Severity,Facility,Code))));
end;


procedure TdlgHrTools.butSearchClick(Sender: TObject);
var
  Text: UnicodeString;
  Command: UnicodeString;
  WorkDir: UnicodeString;
  ToolPath: UnicodeString;
  I: Integer;
  HrCode: HResult;
begin
  mmoDescription.Clear;
  lbxReference.Clear;
  Text := UiValue(edHrHex.Text);
  if Text = '' then Text := UiValue(edHrDec.Text);
  if not TryParseHResult(Text,HrCode) then
    begin
      mmoDescription.Text := 'Please enter a valid 32-bit HRESULT value.';
      Exit;
    end;
  if rbHLD.Checked then
    begin
      if SUCCEEDED(FWinHResultCracker.GetHResultDetails(HrCode)) then
        begin
          Text := 'HResult:              ' + FWinHResultCracker.HResultAsString + CRLF +
                  'Description:          ' + FWinHResultCracker.HResultDescription + CRLF +
                  'Region:               ' + FWinHResultCracker.HResultRegion + CRLF + CRLF +
                  'Facility Tag:         ' + FWinHResultCracker.FacilityTagAsString + CRLF +
                  'Facility Description: ' + FWinHResultCracker.FacilityDescription + CRLF + CRLF +
                  'Severity Tag:         ' + FWinHResultCracker.SeveretyValue + CRLF +
                  'Severity Description: ' + FWinHResultCracker.SeveretyDescription + CRLF + CRLF +
                  'Header file:          ' + FWinHResultCracker.HeaderFile + CRLF;
          mmoDescription.Text := UiText(Text);
          for I := 0 to Length(FWinHResultCracker.ReferenceUrls) - 1 do
            if FWinHResultCracker.ReferenceUrls[I] <> '' then
              lbxReference.Items.Add(UiText(FWinHResultCracker.ReferenceUrls[I]));
        end
      else
        mmoDescription.Text := UiText('Unknown HRESULT value (' + UnicodeString(IntToStr(HrCode)) + ').');
    end
  else if rbMSELTDESC.Checked then
    begin
      WorkDir := IncludeTrailingPathDelimiter(UiValue(ExtractFilePath(Application.ExeName))) + 'MicrosoftErrorLookupTool';
      ToolPath := WorkDir + PathDelim + WIN_ERROR_LOOKUP_TOOL;
      if not FileExists(UiText(ToolPath)) then
        begin
          WorkDir := UiValue(ExpandFileName(ExtractFilePath(Application.ExeName) + '..' + PathDelim + 'MicrosoftErrorLookupTool'));
          ToolPath := WorkDir + PathDelim + WIN_ERROR_LOOKUP_TOOL;
        end;
      if not FileExists(UiText(ToolPath)) then
        begin
          mmoDescription.Text := UiText('Microsoft Error Lookup Tool was not found: ' + ToolPath);
          Exit;
        end;
      Command := '"' + ToolPath + '" ' + UnicodeString(IntToStr(HrCode));
      try
        mmoDescription.Text := UiText(FWinHResultCracker.RunWinErrorTool(Command,WorkDir));
      except
        on E: Exception do mmoDescription.Text := E.Message;
      end;
    end
  else if rbSEMLTDLG.Checked then
    mmoDescription.Text := UiText(FWinHResultCracker.GetSysErrorMessage(HrCode));
end;


procedure TdlgHrTools.butSearchMouseMove(Sender: TObject;
                                         Shift: TShiftState;
                                         X,Y: Integer);
begin
  AppOnActivate(Self);
end;


procedure TdlgHrTools.butTransToDecClick(Sender: TObject);
var
  Value: HResult;
begin
  if TryParseHResult(UiValue(edHrHex.Text),Value) then
    edHrDec.Text := UiText(UnicodeString(IntToStr(Value)));
end;


procedure TdlgHrTools.butTransToHexClick(Sender: TObject);
begin
  edHrHex.Text := UiText(DecimalToHex(UiValue(edHrDec.Text)));
end;


procedure TdlgHrTools.cbxFormStyleClick(Sender: TObject);
begin
  if cbxFormStyle.Checked then FormStyle := fsStayOnTop else FormStyle := fsNormal;
end;


procedure TdlgHrTools.FormCloseQuery(Sender: TObject;
                                     var CanClose: Boolean);
begin
  CanClose := True;
end;


procedure TdlgHrTools.FormCreate(Sender: TObject);
var
  ToolsMenu: TMenuItem;
  TransformItem: TMenuItem;
begin
  Menu := TMainMenu.Create(Self);
  ToolsMenu := TMenuItem.Create(Self);
  ToolsMenu.Caption := 'Tools';
  Menu.Items.Add(ToolsMenu);
  TransformItem := TMenuItem.Create(Self);
  TransformItem.Caption := 'Media Foundation transforms...';
  TransformItem.OnClick := ShowTransforms;
  ToolsMenu.Add(TransformItem);
  if not Assigned(FWinHResultCracker) then FWinHResultCracker := TWinHResultCracker.Create;
  OnActivate := AppOnActivate;
  cbxFormStyle.Checked := True;
  Caption := UiText('HRESULT Lookup Tool (Windows SDK ' + SDK_API_VERSION + ')');
end;


destructor TdlgHrTools.Destroy;
begin
  if dlgHrTools = Self then dlgHrTools := nil;
  FreeAndNil(FWinHResultCracker);
  inherited;
end;


procedure TdlgHrTools.lbxReferenceClick(Sender: TObject);
var
  Url: UnicodeString;
begin
  if (lbxReference.ItemIndex < 0) or (lbxReference.Items.Count = 0) then Exit;
  Url := UiValue(lbxReference.Items[lbxReference.ItemIndex]);
  ShellExecuteW(Handle,'open',PWideChar(Url),nil,nil,SW_SHOWNORMAL);
end;

procedure TdlgHrTools.ShowTransforms(Sender: TObject);
begin
  if not Assigned(dlgMediaTransformTool) then Application.CreateForm(TdlgMediaTransformTool,dlgMediaTransformTool);
  dlgMediaTransformTool.Show;
end;

end.
