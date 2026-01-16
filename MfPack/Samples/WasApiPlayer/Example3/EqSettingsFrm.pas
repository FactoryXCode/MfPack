// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: EqSettingsFrm.pas
// Kind: Pascal Unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 3.1.9
//
// Description: This dialog is used for tuning all MFT parameters and
//              store the values to an ini file.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//          Recommended minimum Delphi version: XE7.
//
// Related objects: -
// Related projects: MfPackX319/Samples/WasApiPlayer/Example3
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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
unit EqSettingsFrm;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  {Application}
  MfAudioHighMidLowTypes;

type

  TEqTuning = record
    LowHz: Single;
    MidHz: Single;
    HighHz: Single;
    MidQ: Single;
    LowShelfSlope: Single;
    HighShelfSlope: Single;
    MidMode: TMfMidMode;
  end;

  TEqApplyEvent = procedure(Sender: TObject;
                            const Tuning: TEqTuning) of object;

  TfrmEqSettings = class(TForm)
    grpLow: TGroupBox;
    lblLowHz: TLabel;
    edtLowHz: TEdit;
    udLowHz: TUpDown;
    lblLowSlope: TLabel;
    edtLowSlope: TEdit;
    udLowSlope: TUpDown;

    grpMid: TGroupBox;
    lblMidHz: TLabel;
    edtMidHz: TEdit;
    udMidHz: TUpDown;
    lblMidQ: TLabel;
    edtMidQ: TEdit;
    udMidQ: TUpDown;
    rgMidMode: TRadioGroup;

    grpHigh: TGroupBox;
    lblHighHz: TLabel;
    edtHighHz: TEdit;
    udHighHz: TUpDown;
    lblHighSlope: TLabel;
    edtHighSlope: TEdit;
    udHighSlope: TUpDown;

    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    btnDefaults: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDefaultsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);

  private

    FLoading: Boolean;
    FIniFileName: string;
    FIniSection: string;
    FDefaults: TEqTuning;

    FOnApply: TEqApplyEvent;
    function ReadTuningFromUi: TEqTuning;
    procedure WriteTuningToUi(const T: TEqTuning);

  public

    procedure ConfigureStorage(const IniFileName: string;
                               const Section: string;
                               const Defaults: TEqTuning);

    procedure LoadFromIni(const IniFileName: string;
                          const Section: string;
                          const Defaults: TEqTuning);

    procedure SaveToIni(const IniFileName: string;
                        const Section: string;
                        const T: TEqTuning);

    procedure SetInitialTuning(const T: TEqTuning);

    property OnApply: TEqApplyEvent read FOnApply write FOnApply;
  end;

var
  frmEqSettings: TfrmEqSettings;


implementation

{$R *.dfm}

uses
  System.Math;

function StrToFloatDefUS(const S: string;
                         const Def: Single): Single;
var
  fs: TFormatSettings;

begin

  fs := TFormatSettings.Create();
  fs.DecimalSeparator := '.';
  Result := StrToFloatDef(StringReplace(S,
                                        ',',
                                        '.',
                                        [rfReplaceAll]),
                                        Def,
                                        fs);
end;


function FloatToStrUS(const V: Single): string;
var
  fs: TFormatSettings;

begin

  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';
  Result := FloatToStr(V,
                       fs);
end;


procedure TfrmEqSettings.FormCreate(Sender: TObject);
begin

  // UpDowns are integer-only; we store scaled ints for slope/Q (x100).
  udLowHz.Min := 10;
  udLowHz.Max := 400;
  udMidHz.Min := 200;
  udMidHz.Max := 6000;
  udHighHz.Min := 2000;
  udHighHz.Max := 22000;

  udMidQ.Min := 30;
  udMidQ.Max := 600;   // 0.30 .. 6.00
  udLowSlope.Min := 10; udLowSlope.Max := 400; // 0.10 .. 4.00
  udHighSlope.Min := 10;
  udHighSlope.Max := 400;


  rgMidMode.Items.Clear();
  rgMidMode.Items.Add('Peaking (Bell)');
  rgMidMode.Items.Add('Notch (Band-stop)');
  rgMidMode.ItemIndex := 0;
end;



procedure TfrmEqSettings.FormShow(Sender: TObject);
begin

  // Lazy defaults for standalone usage.
  if (FIniSection = '') then
    FIniSection := 'EQ';

  if (FIniFileName = '') then
    FIniFileName := ChangeFileExt(Application.ExeName,
                                  '.ini');

  // If caller never provided defaults, use sensible teaching defaults
  if (FDefaults.LowHz = 0) and
     (FDefaults.MidHz = 0) and
     (FDefaults.HighHz = 0) then
    begin

      FDefaults.LowHz := 100.0;
      FDefaults.MidHz := 1000.0;
      FDefaults.HighHz := 10000.0;
      FDefaults.MidQ := 1.0;
      FDefaults.LowShelfSlope := 1.0;
      FDefaults.HighShelfSlope := 1.0;
      FDefaults.MidMode := mmPeaking;
    end;

  // Load persisted values into the UI.
  FLoading := True;

  try
    LoadFromIni(FIniFileName,
                FIniSection,
                FDefaults);
  finally

    FLoading := False;
  end;
end;


procedure TfrmEqSettings.SetInitialTuning(const T: TEqTuning);
begin

  // Used by the main form to seed the dialog with current tuning.
  // We also store this as defaults (for "Defaults" button) if defaults were not set yet.
  if (FDefaults.LowHz = 0) and
     (FDefaults.MidHz = 0) and
     (FDefaults.HighHz = 0) then
    FDefaults := T;

  WriteTuningToUi(T);
end;



procedure TfrmEqSettings.ConfigureStorage(const IniFileName: string; const Section: string; const Defaults: TEqTuning);
begin

  FIniFileName := IniFileName;
  FIniSection := Section;
  FDefaults := Defaults;
end;


procedure TfrmEqSettings.btnApplyClick(Sender: TObject);
var
  T: TEqTuning;

begin

  if FLoading then
    Exit;

  T := ReadTuningFromUi();

  // Persist immediately
  if FIniFileName = '' then
    FIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  if FIniSection = '' then
    FIniSection := 'EQ';

  SaveToIni(FIniFileName,
            FIniSection,
            T);

  if Assigned(FOnApply) then
    FOnApply(Self, T);
end;


procedure TfrmEqSettings.btnDefaultsClick(Sender: TObject);
begin

  // Restore dialog defaults (does not apply until user clicks Apply/OK)
  WriteTuningToUi(FDefaults);
end;


procedure TfrmEqSettings.btnOKClick(Sender: TObject);
begin

  // Apply once on OK for convenience
  btnApplyClick(Sender);
  ModalResult := mrOk;
end;


function TfrmEqSettings.ReadTuningFromUi: TEqTuning;
var
  q100,
  sLow100,
  sHigh100: Integer;

begin

  FillChar(Result,
           SizeOf(Result),
           0);

  Result.LowHz := StrToFloatDef(edtLowHz.Text,
                                100.0);
  Result.MidHz := StrToFloatDef(edtMidHz.Text,
                                1000.0);
  Result.HighHz := StrToFloatDef(edtHighHz.Text,
                                 10000.0);

  // Q and slopes as decimal; we also mirror UpDown (x100)
  Result.MidQ := StrToFloatDefUS(edtMidQ.Text,
                                 1.0);
  Result.LowShelfSlope := StrToFloatDefUS(edtLowSlope.Text,
                                          1.0);
  Result.HighShelfSlope := StrToFloatDefUS(edtHighSlope.Text,
                                           1.0);

  // Notch or Bell
  if (rgMidMode.ItemIndex = 1) then
    Result.MidMode := mmNotch
  else
    Result.MidMode := mmPeaking;

  // Clamp UI values (engine also clamps, but keep UI sane)
  Result.LowHz := EnsureRange(Result.LowHz,
                              10.0,
                              400.0);
  Result.MidHz := EnsureRange(Result.MidHz,
                              200.0,
                              6000.0);
  Result.HighHz := EnsureRange(Result.HighHz,
                               2000.0,
                               22000.0);

  Result.MidQ := EnsureRange(Result.MidQ,
                             0.30,
                             6.00);

  Result.LowShelfSlope := EnsureRange(Result.LowShelfSlope,
                                      0.10,
                                      4.00);

  Result.HighShelfSlope := EnsureRange(Result.HighShelfSlope,
                                       0.10,
                                       4.00);

  // keep UpDowns in sync (integers)
  if not FLoading then
    begin
      udLowHz.Position := Round(Result.LowHz);
  udMidHz.Position := Round(Result.MidHz);
  udHighHz.Position := Round(Result.HighHz);

  q100 := Round(Result.MidQ * 100);
  sLow100 := Round(Result.LowShelfSlope * 100);
  sHigh100 := Round(Result.HighShelfSlope * 100);

  udMidQ.Position := EnsureRange(q100,
                                 udMidQ.Min,
                                 udMidQ.Max);

  udLowSlope.Position := EnsureRange(sLow100,
                                     udLowSlope.Min,
                                     udLowSlope.Max);

  udHighSlope.Position := EnsureRange(sHigh100,
                                      udHighSlope.Min,
                                      udHighSlope.Max);

  // normalize text (dot decimal)
  edtMidQ.Text := FloatToStrUS(Result.MidQ);
  edtLowSlope.Text := FloatToStrUS(Result.LowShelfSlope);
      edtHighSlope.Text := FloatToStrUS(Result.HighShelfSlope);
    end;
end;


procedure TfrmEqSettings.WriteTuningToUi(const T: TEqTuning);
begin

  edtLowHz.Text := IntToStr(Round(EnsureRange(T.LowHz,
                                  10.0,
                                  400.0)));

  edtMidHz.Text := IntToStr(Round(EnsureRange(T.MidHz,
                                              200.0,
                                              6000.0)));

  edtHighHz.Text := IntToStr(Round(EnsureRange(T.HighHz,
                                               2000.0,
                                               22000.0)));

  edtMidQ.Text := FloatToStrUS(EnsureRange(T.MidQ,
                                           0.30,
                                           6.00));

  edtLowSlope.Text := FloatToStrUS(EnsureRange(T.LowShelfSlope,
                                               0.10,
                                               4.00));

  edtHighSlope.Text := FloatToStrUS(EnsureRange(T.HighShelfSlope,
                                                0.10,
                                                4.00));

  udLowHz.Position := StrToIntDef(edtLowHz.Text,
                                  100);

  udMidHz.Position := StrToIntDef(edtMidHz.Text,
                                  1000);

  udHighHz.Position := StrToIntDef(edtHighHz.Text,
                                   10000);

  udMidQ.Position := Round(StrToFloatDefUS(edtMidQ.Text,
                                           1.0) * 100);

  udLowSlope.Position := Round(StrToFloatDefUS(edtLowSlope.Text,
                                               1.0) * 100);

  udHighSlope.Position := Round(StrToFloatDefUS(edtHighSlope.Text,
                                                1.0) * 100);

  if (T.MidMode = mmNotch) then
    rgMidMode.ItemIndex := 1
  else
    rgMidMode.ItemIndex := 0;
end;


procedure TfrmEqSettings.LoadFromIni(const IniFileName: string;
                                     const Section: string;
                                     const Defaults: TEqTuning);
var
  ini: TIniFile;
  t: TEqTuning;

begin

  t := Defaults;

  if not FileExists(IniFileName) then
    begin
      WriteTuningToUi(t);
      Exit;    //ERROR_FILE_NOT_FOUND
    end;

  ini := TIniFile.Create(IniFileName);

  try

    t.LowHz := ini.ReadFloat(Section,
                             'LowHz',
                             t.LowHz);

    t.MidHz := ini.ReadFloat(Section,
                             'MidHz',
                             t.MidHz);

    t.HighHz := ini.ReadFloat(Section,
                              'HighHz',
                              t.HighHz);

    t.MidQ := ini.ReadFloat(Section,
                            'MidQ',
                            t.MidQ);

    t.LowShelfSlope := ini.ReadFloat(Section,
                                     'LowShelfSlope',
                                     t.LowShelfSlope);

    t.HighShelfSlope := ini.ReadFloat(Section,
                                      'HighShelfSlope',
                                      t.HighShelfSlope);

    t.MidMode := TMfMidMode(ini.ReadInteger(Section,
                                            'MidMode',
                                            Ord(t.MidMode)));
  finally

    ini.Free();
  end;

  WriteTuningToUi(t);
end;


procedure TfrmEqSettings.SaveToIni(const IniFileName: string; const Section: string; const T: TEqTuning);
var
  ini: TIniFile;

begin

  ini := TIniFile.Create(IniFileName);

  try

    ini.WriteFloat(Section,
                   'LowHz',
                   T.LowHz);

    ini.WriteFloat(Section,
                   'MidHz',
                   T.MidHz);

    ini.WriteFloat(Section,
                   'HighHz',
                   T.HighHz);

    ini.WriteFloat(Section,
                   'MidQ',
                   T.MidQ);

    ini.WriteFloat(Section,
                   'LowShelfSlope',
                   T.LowShelfSlope);
    ini.WriteFloat(Section,
                   'HighShelfSlope',
                   T.HighShelfSlope);

    ini.WriteInteger(Section,
                     'MidMode',
                     Ord(T.MidMode));
  finally

    ini.Free();
  end;
end;

end.
