// FactoryX
//
// Copyright FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgTimedTextLanguages.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 3.1.3
// Description: Dialog example to select closedcaption languages.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: -
//
// Copyright FactoryX. All rights reserved.
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

unit dlgTimedTextLanguages;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  System.Types,
  {Vcl}
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {Project}
  MfPlayerClassX,
  TimedTextClass,
  FloatingFrm,
  LangTags,
  MfPCXConstants;

type
  TDlgTimedTextLanguages = class(TForm)
    butOk: TButton;
    butCancel: TButton;
    lvTTxtLang: TListView;
    procedure FormShow(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure lvTTxtLangMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  protected
    procedure WMLButtonDown(var Msg: TWMLBUTTONDOWN); message WM_LBUTTONDOWN;

  private
    { Private declarations }

  public
    { Public declarations }

  end;

var
  dlgTimedTextLang: TDlgTimedTextLanguages;

implementation

{$R *.dfm}

uses frmMfPlayer;


procedure TDlgTimedTextLanguages.WMLButtonDown(var Msg: TWMLBUTTONDOWN);
begin
  if lvTTxtLang.GetItemAt(msg.XPos, msg.YPos) <> Nil then
    inherited;
end;


procedure TDlgTimedTextLanguages.butOkClick(Sender: TObject);
begin
  dlgTimedTextLang.Close();
end;


procedure TDlgTimedTextLanguages.FormShow(Sender: TObject);
var
  I: Integer;
  lItem: TListItem;

begin

  lvTTxtLang.Items.Clear;

  if (MfPlayerX.m_hwndFloatingForm = 0) then
    Exit;
  // Read the tag again, because a user may have added or deleted a timedtextfile.
  SetLength(pc_LanguageTags.TimedTxtPropsArray, 0);
  pc_LanguageTags.TimedTxtPropsArray := pc_LanguageTags.ReadFileTags(MfPlayerX.MediaFileName,
                                                                     FloatingForm.PreferredLanguage,
                                                                     0,
                                                                     EXTSUBRIP);

  if Length(pc_LanguageTags.TimedTxtPropsArray) = 0 then
    Exit;

  for I := 0 to Length(pc_LanguageTags.TimedTxtPropsArray) - 1 do
   begin
     // Add a new line
     lItem := lvTTxtLang.Items.Add;
     lItem.Checked := pc_LanguageTags.TimedTxtPropsArray[I].bActiveFile;
     lItem.Caption := pc_LanguageTags.TimedTxtPropsArray[I].sLanguageTag;
     lItem.SubItems.Add(pc_LanguageTags.TimedTxtPropsArray[i].sFriendlyLanguageName);
   end;
end;


procedure TDlgTimedTextLanguages.lvTTxtLangMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ht: THitTests;
  Item: TListItem;
  I: Integer;

begin
  ht := (Sender as TCustomListView).GetHitTestInfoAt(X, Y);

  if (htOnStateIcon in ht) then
    begin
      Item := (Sender as TCustomListView).GetItemAt(X, Y);

      if Assigned(Item) then
        begin
          for I := 0 to lvTTxtLang.Items.Count - 1 do
            if lvTTxtLang.Items[i].Checked and (Item.Index <> I) then
              lvTTxtLang.Items[i].Checked := false;

          // Set the new preffered language
          pc_LanguageTags.TimedTxtPropsArray[Item.Index].bActiveFile := true;
          MfPlayerX.SubtitleLanguage := pc_LanguageTags.TimedTxtPropsArray[Item.Index].sLanguageTag;
          if (MfPlayerX.m_hwndFloatingForm > 0) then
            begin
              fTimedText.TimedTextFile := pc_LanguageTags.TimedTxtPropsArray[Item.Index].sFile;
              FloatingForm.PreferredLanguage := pc_LanguageTags.TimedTxtPropsArray[Item.Index].sLanguageTag;
            end;
        end;
    end;
end;

end.
