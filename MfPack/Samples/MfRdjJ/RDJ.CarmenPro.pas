unit RDJ.CarmenPro;

interface

uses

  Winapi.Windows,

  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils;

type

  TCarmenProCameraInfo = record
    FriendlyName: string;
    SymbolicLink: string;
  end;

  TCarmenProCameraList = TArray<TCarmenProCameraInfo>;


type

  TCarmenProController = class
  private

    FObsExePath: string;
    FObsWorkingDir: string;
    FObsProfileName: string;
    FObsSceneCollection: string;
    FMetadataJsonFile: string;
    FEnabled: Boolean;
    FObsProcessHandle: THandle;
    FObsProcessId: Cardinal;

    function BuildObsCommandLine(): string;
    function JsonEscape(const S: string): string;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure AssignConfig(const AObsExePath,
                                 AObsWorkingDir,
                                 AObsProfileName,
                                 AObsSceneCollection,
                                 AMetadataJsonFile: string;
                           const AEnabled: Boolean);

    function IsObsConfigured(): Boolean;
    function IsObsRunning(): Boolean;

    function StartObs(): Boolean;
    function StopObs(): Boolean;

    procedure WriteMetadataJson(const ADjName,
                                      AShowName,
                                      AArtist,
                                      ATitle,
                                      ACoverUrl: string;
                                const AOnAir: Boolean);

    property Enabled: Boolean read FEnabled write FEnabled;
    property ObsExePath: string read FObsExePath write FObsExePath;
    property ObsWorkingDir: string read FObsWorkingDir write FObsWorkingDir;
    property ObsProfileName: string read FObsProfileName write FObsProfileName;
    property ObsSceneCollection: string read FObsSceneCollection write FObsSceneCollection;
    property MetadataJsonFile: string read FMetadataJsonFile write FMetadataJsonFile;
    property ObsProcessId: Cardinal read FObsProcessId;
  end;


implementation


constructor TCarmenProController.Create();
begin
  inherited Create();

  FObsProcessHandle := 0;
  FObsProcessId := 0;
end;


destructor TCarmenProController.Destroy();
begin
  if FObsProcessHandle <> 0 then
    CloseHandle(FObsProcessHandle);

  inherited Destroy();
end;

procedure TCarmenProController.AssignConfig(const AObsExePath,
                                                  AObsWorkingDir,
                                                  AObsProfileName,
                                                  AObsSceneCollection,
                                                  AMetadataJsonFile: string;
                                            const AEnabled: Boolean);
begin

  FObsExePath := Trim(AObsExePath);
  FObsWorkingDir := Trim(AObsWorkingDir);
  FObsProfileName := Trim(AObsProfileName);
  FObsSceneCollection := Trim(AObsSceneCollection);
  FMetadataJsonFile := Trim(AMetadataJsonFile);
  FEnabled := AEnabled;
end;


function TCarmenProController.IsObsConfigured(): Boolean;
begin
  Result := FileExists(FObsExePath);
end;


function TCarmenProController.IsObsRunning(): Boolean;
var
  ExitCode: DWORD;

begin

  Result := False;

  if FObsProcessHandle = 0 then
    Exit;

  if not GetExitCodeProcess(FObsProcessHandle, ExitCode) then
    Exit;

  Result := ExitCode = STILL_ACTIVE;
end;


function TCarmenProController.BuildObsCommandLine(): string;
begin

  // OBS accepts command line switches for selecting a profile and scene collection.
  // This lets CarmenPro start OBS directly in the correct broadcast setup.
  Result := '"' + FObsExePath + '"';

  if FObsProfileName <> '' then
    Result := Result + ' --profile "' + FObsProfileName + '"';

  if FObsSceneCollection <> '' then
    Result := Result + ' --collection "' + FObsSceneCollection + '"';
end;


function TCarmenProController.StartObs(): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Cmd: string;
  WorkDir: string;

begin

  Result := False;

  // Do not start a second OBS instance if the one started by CarmenPro is still running.
  if IsObsRunning() then
    Exit(True);

  // We only need the OBS executable path for the minimum usable setup.
  if not IsObsConfigured() then
    Exit(False);

  ZeroMemory(@SI, SizeOf(SI));
  ZeroMemory(@PI, SizeOf(PI));

  SI.cb := SizeOf(SI);

  Cmd := BuildObsCommandLine();

  // OBS prefers to start from its own installation directory.
  // If no working directory was configured, derive it from the executable path.
  WorkDir := FObsWorkingDir;
  if WorkDir = '' then
    WorkDir := ExtractFileDir(FObsExePath);

  Result := CreateProcess(nil,
                          PChar(Cmd),
                          nil,
                          nil,
                          False,
                          CREATE_NEW_PROCESS_GROUP,
                          nil,
                          PChar(WorkDir),
                          SI,
                          PI);

  if Result then
    begin
      // Keep the process handle so CarmenPro can later check whether OBS is still running.
      if FObsProcessHandle <> 0 then
        CloseHandle(FObsProcessHandle);

      FObsProcessHandle := PI.hProcess;
      FObsProcessId := PI.dwProcessId;

      // The thread handle is not needed after process creation.
      CloseHandle(PI.hThread);
    end;
end;


function TCarmenProController.StopObs(): Boolean;
begin

  Result := False;

  // If OBS was not started by this controller, there is nothing to stop here.
  if not IsObsRunning() then
    Exit(True);

  // Phase 1 uses a hard stop. Later we can replace this with OBS WebSocket
  // so OBS can stop streaming and close gracefully.
  Result := TerminateProcess(FObsProcessHandle, 0);

  CloseHandle(FObsProcessHandle);
  FObsProcessHandle := 0;
  FObsProcessId := 0;
end;


function TCarmenProController.JsonEscape(const S: string): string;
begin

  Result := S;
  Result := Result.Replace('\', '\\');
  Result := Result.Replace('"', '\"');
  Result := Result.Replace(#13#10, '\n');
  Result := Result.Replace(#13, '\n');
  Result := Result.Replace(#10, '\n');
end;


procedure TCarmenProController.WriteMetadataJson(const ADjName,
                                                       AShowName,
                                                       AArtist,
                                                       ATitle,
                                                       ACoverUrl: string;
                                                 const AOnAir: Boolean);
var
  Dir: string;
  Json: TStringBuilder;

begin

  if (FMetadataJsonFile = '') then
    Exit;

  // OBS can read this file with a Browser Source or a text plugin.
  // The website/PWA can also reuse the same metadata if wanted.
  Dir := ExtractFileDir(FMetadataJsonFile);
  if (Dir <> '') and not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Json := TStringBuilder.Create();
  try
    Json.AppendLine('{');
    Json.AppendLine('  "djName": "' + JsonEscape(ADjName) + '",');
    Json.AppendLine('  "showName": "' + JsonEscape(AShowName) + '",');
    Json.AppendLine('  "artist": "' + JsonEscape(AArtist) + '",');
    Json.AppendLine('  "title": "' + JsonEscape(ATitle) + '",');
    Json.AppendLine('  "coverUrl": "' + JsonEscape(ACoverUrl) + '",');

    if AOnAir then
      Json.AppendLine('  "onAir": true')
    else
      Json.AppendLine('  "onAir": false');

    Json.AppendLine('}');

    // Always write UTF-8 so browser overlays and OBS text sources handle accents correctly.
    TFile.WriteAllText(FMetadataJsonFile,
                       Json.ToString,
                       TEncoding.UTF8);
  finally
    Json.Free();
  end;
end;

end.
