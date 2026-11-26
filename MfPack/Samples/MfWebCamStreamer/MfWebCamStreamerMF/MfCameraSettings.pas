// FactoryX
// Project: MfPack - camera sample settings
// Description: Per-camera INI settings beside the executable, separate from Windows defaults.
// License: Mozilla Public License 2.0, https://mozilla.org/MPL/2.0/
unit MfCameraSettings;

{$IFDEF FPC}
{$MODE DELPHIUNICODE}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  Windows, SysUtils, Classes, IniFiles;
{$ELSE}
  Winapi.Windows, System.SysUtils, System.Classes, System.IniFiles;
{$ENDIF}

type
  TMfCameraSettings = class
  private
    class function CameraSection(const ACamera: string): string; static;
    class function ControlKey(const ASet: TGUID; const AId: Cardinal): string; static;
    class function Change(const AFile, ASection, AKey, AValue: string;
                         const ARemove: Boolean): HRESULT; static;
  public
    class function FileName(): string; static;
    class function UsesIni(const AFile: string): Boolean; static;
    class function SetUsesIni(const AFile: string; const AUsesIni: Boolean): HRESULT; static;
    class function Read(const AFile, ACamera: string; const ASet: TGUID;
                        const AId: Cardinal; out AValue: Longint;
                        out AFlags: Cardinal): HRESULT; static;
    class function Write(const AFile, ACamera: string; const ASet: TGUID;
                         const AId: Cardinal; const AValue: Longint;
                         const AFlags: Cardinal): HRESULT; static;
    class function Remove(const AFile, ACamera: string; const ASet: TGUID;
                          const AId: Cardinal): HRESULT; static;
  end;

implementation

const
  SettingsInvalidData = HRESULT($8007000D);
  SettingsFailure = HRESULT($80004005);
  SettingsInvalidArg = HRESULT($80070057);

function SettingsError(const AError: DWORD): HRESULT;
begin
  if AError = 0 then
    Result := SettingsFailure
  else
    Result := HRESULT($80070000 or (AError and $FFFF));
end;

class function TMfCameraSettings.FileName(): string;
begin
  Result := ExtractFilePath(ExpandFileName(ParamStr(0))) + 'CameraSettings.ini';
end;

class function TMfCameraSettings.CameraSection(const ACamera: string): string;
var
  I: Integer;
  Camera: string;
begin
  // Encode the full symbolic link, including INI delimiters and Unicode.
  // This avoids collisions between cameras with the same friendly name.
  Camera := UpperCase(ACamera);
  Result := 'Camera.';
  for I := 1 to Length(Camera) do
    Result := Result + IntToHex(Ord(Camera[I]), 4);
end;

class function TMfCameraSettings.ControlKey(const ASet: TGUID;
                                            const AId: Cardinal): string;
begin
  Result := GUIDToString(ASet) + '.' + IntToStr(AId);
end;

class function TMfCameraSettings.Change(const AFile, ASection, AKey, AValue: string;
                                        const ARemove: Boolean): HRESULT;
var
  Ini: TMemIniFile;
  Lines: TStringList;
  Directory: string;
  Temporary: array[0..MAX_PATH] of Char;
  TemporaryName: string;
begin
  Result := SettingsFailure;
  TemporaryName := '';
  try
    if ARemove and not FileExists(AFile) then
      Exit(S_FALSE);
    Directory := ExtractFilePath(ExpandFileName(AFile));
    if not DirectoryExists(Directory) and not ForceDirectories(Directory) then
      Exit(SettingsError(GetLastError()));
    Ini := TMemIniFile.Create(AFile);
    try
      if ARemove then
        begin
          if not Ini.ValueExists(ASection, AKey) then
            Exit(S_FALSE);
          Ini.DeleteKey(ASection, AKey);
        end
      else
        Ini.WriteString(ASection, AKey, AValue);
      Lines := TStringList.Create();
      try
        Ini.GetStrings(Lines);
        if GetTempFileNameW(PWideChar(Directory), 'mfc', 0, @Temporary[0]) = 0 then
          Exit(SettingsError(GetLastError()));
        TemporaryName := Temporary;
        Lines.SaveToFile(TemporaryName);
        // Publish a complete file; a failed replacement preserves the old settings.
        if not MoveFileExW(PWideChar(TemporaryName), PWideChar(AFile),
                          MOVEFILE_REPLACE_EXISTING or $00000008) then
          Exit(SettingsError(GetLastError()));
        TemporaryName := '';
        Result := S_OK;
      finally
        Lines.Free();
        if TemporaryName <> '' then DeleteFileW(PWideChar(TemporaryName));
      end;
    finally
      Ini.Free();
    end;
  except
    on E: EOSError do Result := SettingsError(E.ErrorCode);
    on E: Exception do Result := SettingsFailure;
  end;
  if TemporaryName <> '' then
    DeleteFileW(PWideChar(TemporaryName));
end;

class function TMfCameraSettings.UsesIni(const AFile: string): Boolean;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(AFile);
  try
    Result := SameText(Ini.ReadString('Storage', 'Backend', 'INI'), 'INI');
  finally
    Ini.Free();
  end;
end;

class function TMfCameraSettings.SetUsesIni(const AFile: string;
                                           const AUsesIni: Boolean): HRESULT;
var
  Backend: string;
begin
  if AUsesIni then Backend := 'INI' else Backend := 'Windows';
  Result := Change(AFile, 'Storage', 'Backend', Backend, False);
end;

class function TMfCameraSettings.Read(const AFile, ACamera: string;
                                     const ASet: TGUID; const AId: Cardinal;
                                     out AValue: Longint;
                                     out AFlags: Cardinal): HRESULT;
var
  Ini: TMemIniFile;
  Text: string;
  Separator: Integer;
  Flags: Integer;
begin
  AValue := 0;
  AFlags := 0;
  if ACamera = '' then Exit(SettingsInvalidArg);
  try
    Ini := TMemIniFile.Create(AFile);
    try
      if not Ini.ValueExists(CameraSection(ACamera), ControlKey(ASet, AId)) then
        Exit(S_FALSE);
      Text := Ini.ReadString(CameraSection(ACamera), ControlKey(ASet, AId), '');
      Separator := Pos(',', Text);
      if (Separator = 0) or
         not TryStrToInt(Copy(Text, 1, Separator - 1), AValue) or
         not TryStrToInt(Copy(Text, Separator + 1, MaxInt), Flags) then
        Exit(SettingsInvalidData);
      if (Flags <> 1) and (Flags <> 2) then Exit(SettingsInvalidData);
      AFlags := Flags;
      Result := S_OK;
    finally
      Ini.Free();
    end;
  except
    on E: EOSError do Result := SettingsError(E.ErrorCode);
    on E: Exception do Result := SettingsFailure;
  end;
end;

class function TMfCameraSettings.Write(const AFile, ACamera: string;
                                      const ASet: TGUID; const AId: Cardinal;
                                      const AValue: Longint;
                                      const AFlags: Cardinal): HRESULT;
begin
  if (ACamera = '') or ((AFlags <> 1) and (AFlags <> 2)) then
    Exit(SettingsInvalidArg);
  Result := Change(AFile, CameraSection(ACamera), ControlKey(ASet, AId),
                   IntToStr(AValue) + ',' + IntToStr(AFlags), False);
end;

class function TMfCameraSettings.Remove(const AFile, ACamera: string;
                                       const ASet: TGUID; const AId: Cardinal): HRESULT;
begin
  if ACamera = '' then Exit(SettingsInvalidArg);
  Result := Change(AFile, CameraSection(ACamera), ControlKey(ASet, AId), '', True);
end;

end.
