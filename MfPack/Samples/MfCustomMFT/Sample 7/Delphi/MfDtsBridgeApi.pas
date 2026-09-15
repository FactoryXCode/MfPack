unit MfDtsBridgeApi;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils;

const
  MFPACK_DTS_BRIDGE_ABI = 1;

type

  TMfDtsDecoderHandle = Pointer;

  TMfDtsBridge = class
  private type
    TBridgeAbi = function(): Cardinal; cdecl;
    TBridgeVersion = function(): PAnsiChar; cdecl;
    TDecoderCreate = function(AExtraData: PByte; AExtraDataSize: NativeUInt;
      AOutputRate, AOutputChannels: Integer;
      out ADecoder: TMfDtsDecoderHandle): Integer; cdecl;
    TDecoderDestroy = procedure(ADecoder: TMfDtsDecoderHandle); cdecl;
    TDecoderSend = function(ADecoder: TMfDtsDecoderHandle; AData: PByte;
      ASize: NativeUInt): Integer; cdecl;
    TDecoderDrain = function(ADecoder: TMfDtsDecoderHandle): Integer; cdecl;
    TDecoderAvailable = function(ADecoder: TMfDtsDecoderHandle): NativeUInt; cdecl;
    TDecoderRead = function(ADecoder: TMfDtsDecoderHandle; ADestination: PByte;
      ASize: NativeUInt): NativeUInt; cdecl;
    TDecoderFlush = procedure(ADecoder: TMfDtsDecoderHandle); cdecl;

  private
    FModule: HMODULE;
    FAbi: TBridgeAbi;
    FVersion: TBridgeVersion;
    FCreate: TDecoderCreate;
    FDestroy: TDecoderDestroy;
    FSend: TDecoderSend;
    FDrain: TDecoderDrain;
    FAvailable: TDecoderAvailable;
    FRead: TDecoderRead;
    FFlush: TDecoderFlush;

    function Resolve(const AName: AnsiString): Pointer;

  public

    destructor Destroy(); override;

    function Load(): Boolean;
    function Version(): string;
    function CreateDecoder(out ADecoder: TMfDtsDecoderHandle): Integer;
    procedure DestroyDecoder(var ADecoder: TMfDtsDecoderHandle);

    function Send(ADecoder: TMfDtsDecoderHandle;
                  AData: PByte;
                  ASize: NativeUInt): Integer;

    function Drain(ADecoder: TMfDtsDecoderHandle): Integer;
    function Available(ADecoder: TMfDtsDecoderHandle): NativeUInt;

    function Read(ADecoder: TMfDtsDecoderHandle;
                  ADestination: PByte;
                  ASize: NativeUInt): NativeUInt;

    procedure Flush(ADecoder: TMfDtsDecoderHandle);
  end;


implementation

function TMfDtsBridge.Resolve(const AName: AnsiString): Pointer;
begin

  Result := GetProcAddress(FModule,
                           PAnsiChar(AName));
end;


destructor TMfDtsBridge.Destroy;
begin

  if (FModule <> 0) then
    FreeLibrary(FModule);

  inherited;
end;


function TMfDtsBridge.Load(): Boolean;
var
  ModuleName: array[0..MAX_PATH - 1] of Char;
  BridgeName: string;

begin

  Result := (FModule <> 0);

  if Result then
    Exit;

  if GetModuleFileName(HInstance, ModuleName, Length(ModuleName)) = 0 then Exit(False);
  BridgeName := IncludeTrailingPathDelimiter(ExtractFilePath(ModuleName)) +
                'MfPackDtsBridge.dll';
  FModule := LoadLibraryEx(PChar(BridgeName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if FModule = 0 then Exit(False);
  FAbi := TBridgeAbi(Resolve('mfpack_dts_bridge_abi'));
  FVersion := TBridgeVersion(Resolve('mfpack_dts_bridge_ffmpeg_version'));
  FCreate := TDecoderCreate(Resolve('mfpack_dts_decoder_create'));
  FDestroy := TDecoderDestroy(Resolve('mfpack_dts_decoder_destroy'));
  FSend := TDecoderSend(Resolve('mfpack_dts_decoder_send'));
  FDrain := TDecoderDrain(Resolve('mfpack_dts_decoder_drain'));
  FAvailable := TDecoderAvailable(Resolve('mfpack_dts_decoder_available'));
  FRead := TDecoderRead(Resolve('mfpack_dts_decoder_read'));
  FFlush := TDecoderFlush(Resolve('mfpack_dts_decoder_flush'));
  Result := Assigned(FAbi) and Assigned(FVersion) and Assigned(FCreate) and
    Assigned(FDestroy) and Assigned(FSend) and Assigned(FDrain) and
    Assigned(FAvailable) and Assigned(FRead) and Assigned(FFlush) and
    (FAbi() = MFPACK_DTS_BRIDGE_ABI);
  if not Result then begin FreeLibrary(FModule); FModule := 0; end;
end;

function TMfDtsBridge.Version(): string;
begin
  if (FModule <> 0) and Assigned(FVersion) then Result := string(AnsiString(FVersion()))
  else Result := '';
end;

function TMfDtsBridge.CreateDecoder(out ADecoder: TMfDtsDecoderHandle): Integer;
begin
  ADecoder := nil;
  if not Load() then Exit(-1);
  Result := FCreate(nil, 0, 48000, 2, ADecoder);
end;

procedure TMfDtsBridge.DestroyDecoder(var ADecoder: TMfDtsDecoderHandle);
begin
  if Assigned(ADecoder) and Assigned(FDestroy) then FDestroy(ADecoder);
  ADecoder := nil;
end;

function TMfDtsBridge.Send(ADecoder: TMfDtsDecoderHandle; AData: PByte;
  ASize: NativeUInt): Integer;
begin Result := FSend(ADecoder, AData, ASize); end;

function TMfDtsBridge.Drain(ADecoder: TMfDtsDecoderHandle): Integer;
begin Result := FDrain(ADecoder); end;

function TMfDtsBridge.Available(ADecoder: TMfDtsDecoderHandle): NativeUInt;
begin Result := FAvailable(ADecoder); end;

function TMfDtsBridge.Read(ADecoder: TMfDtsDecoderHandle; ADestination: PByte;
  ASize: NativeUInt): NativeUInt;
begin Result := FRead(ADecoder, ADestination, ASize); end;

procedure TMfDtsBridge.Flush(ADecoder: TMfDtsDecoderHandle);
begin if Assigned(ADecoder) then FFlush(ADecoder); end;

end.
