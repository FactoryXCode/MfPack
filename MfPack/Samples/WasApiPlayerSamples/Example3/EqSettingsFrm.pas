unit EqSettingsFrm;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
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

  TEqApplyEvent = procedure(Sender: TObject;
                            const Tuning: TEqTuning) of object;

  TfrmEqSettings = class(TForm)
    grpLow: TGroupBox;
    lblLowFreqHz: TLabel;
    edtLowFreqHz: TEdit;
    udLowFreqHz: TUpDown;
    lblLowSlope: TLabel;
    edtLowSlope: TEdit;
    udLowSlope: TUpDown;

    grpMid: TGroupBox;
    lblMidFreqHz: TLabel;
    edtMidFreqHz: TEdit;
    udMidFreqHz: TUpDown;
    lblMidQ: TLabel;
    edtMidQ: TEdit;
    udMidQ: TUpDown;
    rgMidMode: TRadioGroup;

    grpHigh: TGroupBox;
    lblHighFreqHz: TLabel;
    edtHighFreqHz: TEdit;
    udHighFreqHz: TUpDown;
    lblHighSlope: TLabel;
    edtHighSlope: TEdit;
    udHighSlope: TUpDown;

    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    btnDefaults: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDefaultsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);

  private

    FOnApply: TEqApplyEvent;
    FDefaults: TEqTuning;
    FTuning: TEqTuning;
    FIniFileName: string;
    FIniSection: string;

    function ReadTuningFromUi(): TEqTuning;
    procedure WriteTuningToUi(const T: TEqTuning);

  public

    // Optional: if configured, Apply/OK can auto-save to ini.
    procedure ConfigureStorage(const IniFileName: string;
                               const Section: string;
                               const Defaults: TEqTuning);

    procedure LoadFromIni(const IniFileName: string;
                          const Section: string;
                          const Defaults: TEqTuning);

    procedure SaveToIni(const IniFileName: string;
                        const Section: string;
                        const T: TEqTuning);

    property OnApply: TEqApplyEvent read FOnApply write FOnApply;
    procedure SetInitialTuning(const T: TEqTuning);
  end;

var
  frmEqSettings: TfrmEqSettings;


implementation

{$R *.dfm}

uses
  System.Math;


// Helpers ---------------------------------------------------------------------

function TryReadSingle(const S: string; out V: Single): Boolean;
var
  fs: TFormatSettings;
  tmp: string;
  d: Double;

begin

  // Accept both "," and "." as decimal separator.
  tmp := StringReplace(S,
                       ',',
                       '.',
                       [rfReplaceAll]);

  fs := TFormatSettings.Create();
  fs.DecimalSeparator := '.';
  Result := TryStrToFloat(tmp,
                          d,
                          fs);
  if Result then
    V := (d * 1.0);
end;


function StrToFloatDefUS(const S: string;
                         const Def: Single): Single;
var
  fs: TFormatSettings;

begin

  fs := TFormatSettings.Create;
  fs.DecimalSeparator := '.';

  // Accept both "," and "." as decimal separator.
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

  fs := TFormatSettings.Create();
  fs.DecimalSeparator := '.';
  Result := FloatToStr(V, fs);
end;

// -----------------------------------------------------------------------------


procedure TfrmEqSettings.FormCreate(Sender: TObject);
begin

  // UpDowns are integer-only; we store scaled ints for slope/Q (x100).
  udLowFreqHz.Min := 10;
  udLowFreqHz.Max := 400;
  udMidFreqHz.Min := 200;
  udMidFreqHz.Max := 6000;
  udHighFreqHz.Min := 2000;
  udHighFreqHz.Max := 22000;

  udMidQ.Min := 30;
  udMidQ.Max := 600; // 0.30 .. 6.00
  udLowSlope.Min := 10;
  udLowSlope.Max := 400; // 0.10 .. 4.00
  udHighSlope.Min := 10;
  udHighSlope.Max := 400;

  rgMidMode.Items.Clear;
  rgMidMode.Items.Add('Peaking (Bell)');
  rgMidMode.Items.Add('Notch (Band-stop)');
  rgMidMode.ItemIndex := 0;

  // Reasonable built-in defaults (can be overridden by ConfigureStorage)
  FillChar(FDefaults, SizeOf(FDefaults), 0);
  FDefaults.LowFreqHz := 100.0;
  FDefaults.MidFreqHz := 1000.0;
  FDefaults.HighFreqHz := 10000.0;
  FDefaults.MidQ := 1.0;
  FDefaults.LowShelfSlope := 1.0;
  FDefaults.HighShelfSlope := 1.0;
  FDefaults.MidMode := mmPeaking;

  FTuning := FDefaults;
end;


procedure TfrmEqSettings.SetInitialTuning(const T: TEqTuning);
begin

  FTuning := T;
  WriteTuningToUi(FTuning);
end;


procedure TfrmEqSettings.btnApplyClick(Sender: TObject);
var
  T: TEqTuning;

begin

  T := ReadTuningFromUi;
  FTuning := T;
  // Normalize UI to clamped values (but do NOT revert unrelated fields).
  WriteTuningToUi(FTuning);

  if Assigned(FOnApply) then
    FOnApply(Self, T);

  // If storage is configured, save what we just applied.
  if (FIniFileName <> '') and (FIniSection <> '') then
    SaveToIni(FIniFileName, FIniSection, FTuning);
end;


procedure TfrmEqSettings.btnDefaultsClick(Sender: TObject);
begin

  FTuning := FDefaults;
  WriteTuningToUi(FTuning);

  if Assigned(FOnApply) then
    FOnApply(Self, FTuning);

  if (FIniFileName <> '') and (FIniSection <> '') then
    SaveToIni(FIniFileName, FIniSection, FTuning);
end;


procedure TfrmEqSettings.btnOKClick(Sender: TObject);
begin
  // Apply once on OK for convenience
  btnApplyClick(Sender);
  ModalResult := mrOk;
end;


function TfrmEqSettings.ReadTuningFromUi(): TEqTuning;
var
  v: Single;

begin

  // Start from current tuning so a single invalid/empty field doesn't
  // reset everything back to defaults.
  Result := FTuning;

  if TryReadSingle(Trim(edtLowFreqHz.Text),
                        v) then
    Result.LowFreqHz := v;

  if TryReadSingle(Trim(edtMidFreqHz.Text),
                        v) then
    Result.MidFreqHz := v;

  if TryReadSingle(Trim(edtHighFreqHz.Text),
                        v) then
    Result.HighFreqHz := v;

  if TryReadSingle(Trim(edtMidQ.Text),
                        v) then
    Result.MidQ := v;

  if TryReadSingle(Trim(edtLowSlope.Text),
                        v) then
    Result.LowShelfSlope := v;

  if TryReadSingle(Trim(edtHighSlope.Text),
                        v) then
    Result.HighShelfSlope := v;

  // Notch or Bell
  if (rgMidMode.ItemIndex = 1) then
    Result.MidMode := mmNotch
  else
    Result.MidMode := mmPeaking;

  // Clamp UI values (engine also clamps, but keep UI sane)
  Result.LowFreqHz := EnsureRange(Result.LowFreqHz,
                              10.0,
                              400.0);
  Result.MidFreqHz := EnsureRange(Result.MidFreqHz,
                              200.0,
                              6000.0);
  Result.HighFreqHz := EnsureRange(Result.HighFreqHz,
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
end;


procedure TfrmEqSettings.ConfigureStorage(const IniFileName: string;
                                          const Section: string;
                                          const Defaults: TEqTuning);
begin

  FIniFileName := IniFileName;
  FIniSection := Section;
  FDefaults := Defaults;
  // Keep a sane base.
  if (FTuning.LowFreqHz = 0) and
     (FTuning.MidFreqHz = 0) and
     (FTuning.HighFreqHz = 0) then
    FTuning := FDefaults;
end;


procedure TfrmEqSettings.WriteTuningToUi(const T: TEqTuning);
begin

  edtLowFreqHz.Text := IntToStr(Round(EnsureRange(T.LowFreqHz,
                                                  10.0,
                                                  400.0)));

  edtMidFreqHz.Text := IntToStr(Round(EnsureRange(T.MidFreqHz,
                                                  200.0,
                                                  6000.0)));

  edtHighFreqHz.Text := IntToStr(Round(EnsureRange(T.HighFreqHz,
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

  udLowFreqHz.Position := StrToIntDef(edtLowFreqHz.Text,
                                      100);

  udMidFreqHz.Position := StrToIntDef(edtMidFreqHz.Text,
                                      1000);

  udHighFreqHz.Position := StrToIntDef(edtHighFreqHz.Text,
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
      Exit;
    end;

  ini := TIniFile.Create(IniFileName);

  try

    t.LowFreqHz := ini.ReadFloat(Section,
                                 'LowFreqHz',
                                 t.LowFreqHz);

    t.MidFreqHz := ini.ReadFloat(Section,
                                 'MidFreqHz',
                                 t.MidFreqHz);

    t.HighFreqHz := ini.ReadFloat(Section,
                                  'HighFreqHz',
                                  t.HighFreqHz);

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

  // Remember as current
  FTuning := t;
  FDefaults := Defaults;
  FIniFileName := IniFileName;
  FIniSection := Section;
end;


procedure TfrmEqSettings.SaveToIni(const IniFileName: string;
                                   const Section: string;
                                   const T: TEqTuning);
var
  ini: TIniFile;

begin

  ini := TIniFile.Create(IniFileName);

  try
    ini.WriteFloat(Section, 'LowFreqHz', T.LowFreqHz);
    ini.WriteFloat(Section, 'MidFreqHz', T.MidFreqHz);
    ini.WriteFloat(Section, 'HighFreqHz', T.HighFreqHz);
    ini.WriteFloat(Section, 'MidQ', T.MidQ);
    ini.WriteFloat(Section, 'LowShelfSlope', T.LowShelfSlope);
    ini.WriteFloat(Section, 'HighShelfSlope', T.HighShelfSlope);
    ini.WriteInteger(Section, 'MidMode', Ord(T.MidMode));
  finally

    ini.Free();
  end;
end;

end.
