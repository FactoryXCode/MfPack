unit MfPitchTempoComponent;

interface

uses

  {Winapi}
  Winapi.Windows,
  {System}
  System.Classes,
  System.SysUtils,
  System.Math,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfTransform,
  {Application}
  MfPitchTempoMFT,
  MfWasApiFxComponentBase;

type

  TMfPitchTempoEffect = class(TMfWasApiFxComponentBase)
  private

    FSettings: TPitchTempoSettings;
    FMft: IMFTransform;

    function GetEnabled(): LongBool;
    procedure SetEnabled(const Value: LongBool);

    function GetPitchSemitones(): Single;
    procedure SetPitchSemitones(const Value: Single);

    function GetTempoPercent(): Single;
    procedure SetTempoPercent(const Value: Single);

    function GetPreserveFormants(): Boolean;
    procedure SetPreserveFormants(const Value: Boolean);

    function GetWindowSize(): Integer;
    procedure SetWindowSize(const Value: Integer);

    function GetOverlap(): Single;
    procedure SetOverlap(const Value: Single);

    function GetMode: TPitchTempoMode;
    procedure SetMode(const Value: TPitchTempoMode);

    procedure ApplySettingsToMft();

  protected

    // TMfWasApiFxComponentBase
    procedure CheckForMft(); override;
    function GetMftInstance(): IMFTransform; override;

  public

    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

  published

    // Mirrors to the MFT via IMfPitchTempoMft.EnableFX (0/1 LongBool).
    property Enabled: LongBool read GetEnabled write SetEnabled default True;

    property PitchSemitones: Single read GetPitchSemitones write SetPitchSemitones;
    property TempoPercent: Single read GetTempoPercent write SetTempoPercent;
    property PreserveFormants: Boolean read GetPreserveFormants write SetPreserveFormants default False;
    property WindowSize: Integer read GetWindowSize write SetWindowSize default 1024;
    property Overlap: Single read GetOverlap write SetOverlap;
    property Mode: TPitchTempoMode read GetMode write SetMode default ptmClean;
  end;

procedure Register;


implementation


procedure Register;
begin

  RegisterComponents('MfPack Core Audio Samples',
                     [TMfPitchTempoEffect]);
end;

{ TMfPitchTempoEffect }

constructor TMfPitchTempoEffect.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);

  FSettings := PITCH_TEMPO_DEFAULTS;
  // Ensure Enabled is defined even if the defaults constant is older.
  FSettings.Enabled := 1;
  FMft := nil;
end;


destructor TMfPitchTempoEffect.Destroy();
begin

  FMft := nil;

  inherited Destroy();
end;


procedure TMfPitchTempoEffect.CheckForMft();
var
  Ctrl: IMfPitchTempoMft;

begin

  if IsDesigning() then
    Exit;

  if (FMft = nil) then
    begin

      // TMfPitchTempoMFT is an IMFTransform via TMfAudioEffectMFTBase.
      FMft := TMfPitchTempoMFT.Create();
      ApplySettingsToMft();
    end;

  // Ensure settings are applied even if recreated.
  if Supports(FMft,
              IMfPitchTempoMft,
              Ctrl) then
    Ctrl.SetSettings(FSettings);
end;


function TMfPitchTempoEffect.GetMftInstance(): IMFTransform;
begin

  if not IsDesigning() then
    CheckForMft();
  Result := FMft;
end;


procedure TMfPitchTempoEffect.ApplySettingsToMft();
var
  Ctrl: IMfPitchTempoMft;

begin

  if (FMft = nil) then
    Exit;

  if Supports(FMft,
              IMfPitchTempoMft,
              Ctrl) then
    begin

      // Keep enable state in sync (MFT can early-out on the audio thread).
      Ctrl.EnableFX(FSettings.Enabled <> 0);
      Ctrl.SetSettings(FSettings);
    end;
end;


function TMfPitchTempoEffect.GetEnabled(): LongBool;
begin

  Result := Boolean(FSettings.Enabled);
end;


procedure TMfPitchTempoEffect.SetEnabled(const Value: LongBool);
var
  Ctrl: IMfPitchTempoMft;

begin

  if Boolean(FSettings.Enabled) = Value then
    Exit;

  // Delphi LongBool True = -1; normalize to 0/1.
  FSettings.Enabled := Abs(Ord(Value));

  if (FMft <> nil) and Supports(FMft,
                                IMfPitchTempoMft,
                                Ctrl) then
    Ctrl.EnableFX(Value);
end;


function TMfPitchTempoEffect.GetPitchSemitones(): Single;
begin

  Result := FSettings.PitchSemitones;
end;


procedure TMfPitchTempoEffect.SetPitchSemitones(const Value: Single);
begin

  if SameValue(FSettings.PitchSemitones,
               Value) then
    Exit;

  FSettings.PitchSemitones := Value;
  ApplySettingsToMft();
end;


function TMfPitchTempoEffect.GetTempoPercent(): Single;
begin

  Result := FSettings.TempoPercent;
end;


procedure TMfPitchTempoEffect.SetTempoPercent(const Value: Single);
begin

  if SameValue(FSettings.TempoPercent,
               Value) then
    Exit;

  FSettings.TempoPercent := Value;
  ApplySettingsToMft();
end;


function TMfPitchTempoEffect.GetPreserveFormants(): Boolean;
begin

  Result := Boolean(FSettings.PreserveFormants);
end;


procedure TMfPitchTempoEffect.SetPreserveFormants(const Value: Boolean);
begin

  if Boolean(FSettings.PreserveFormants) = Value then
    Exit;

  // Delphi LongBool True = -1; normalize to 0/1.
  FSettings.PreserveFormants := LongBool(Ord(Value));
  ApplySettingsToMft();
end;


function TMfPitchTempoEffect.GetWindowSize(): Integer;
begin
  Result := FSettings.WindowSize;
end;


procedure TMfPitchTempoEffect.SetWindowSize(const Value: Integer);
begin

  if (FSettings.WindowSize = Value) then
    Exit;

  FSettings.WindowSize := Value;
  ApplySettingsToMft();
end;


function TMfPitchTempoEffect.GetOverlap(): Single;
begin

  Result := FSettings.Overlap;
end;


procedure TMfPitchTempoEffect.SetOverlap(const Value: Single);
begin

  if SameValue(FSettings.Overlap,
               Value) then
    Exit;

  FSettings.Overlap := Value;
  ApplySettingsToMft();
end;


function TMfPitchTempoEffect.GetMode: TPitchTempoMode;
begin

  Result := FSettings.Mode;
end;


procedure TMfPitchTempoEffect.SetMode(const Value: TPitchTempoMode);
begin

  if (FSettings.Mode = Value) then
    Exit;

  FSettings.Mode := Value;
  ApplySettingsToMft();
end;

end.
