library FactoryXAudioDelayMFT;

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.Unknwn,
  {System}
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  {Dll}
  AudioDelayRegistration in 'AudioDelayRegistration.pas',
  RegisteredAudioDelayMFT in 'RegisteredAudioDelayMFT.pas',
  MfAudioDelayMFT in '..\Sample 9\MfAudioDelayMFT.pas';


function SetRegistryString(const AKeyName: UnicodeString;
                           const AValueName: UnicodeString;
                           const AValue: UnicodeString): HResult;
var
  Key: HKEY;
  Disposition: DWORD;
  ErrorCode: LongInt;
  ValueName: PWideChar;

begin

  Key := 0;
  ErrorCode := RegCreateKeyExW(HKEY_LOCAL_MACHINE,
                               PWideChar(AKeyName),
                               0,
                               nil,
                               REG_OPTION_NON_VOLATILE,
                               KEY_WRITE,
                               nil,
                               Key,
                               @Disposition);

  if (ErrorCode = ERROR_SUCCESS) then
    try
      if (AValueName = '') then
        ValueName := nil
      else
        ValueName := PWideChar(AValueName);

      ErrorCode := RegSetValueExW(Key,
                                  ValueName,
                                  0,
                                  REG_SZ,
                                  PByte(PWideChar(AValue)),
                                  (Length(AValue) + 1) * SizeOf(WideChar));
    finally
      RegCloseKey(Key);
    end;

  Result := HRESULT_FROM_WIN32(ErrorCode);
end;


function RegisterComObject(): HResult;
var
  ClassKey: UnicodeString;
  ServerKey: UnicodeString;
  ModuleName: array[0..MAX_PATH - 1] of WideChar;
  ModuleLength: DWORD;

begin

  // A Win32 DLL writes to the 32-bit registry view on 64-bit Windows.
  // COM activation needs the same CLSID that MFTRegister advertises below.
  ClassKey := 'Software\Classes\CLSID\' +
              GUIDToString(CLSID_FactoryXAudioDelayMFT);
  ServerKey := ClassKey + '\InprocServer32';

  ModuleLength := GetModuleFileNameW(HInstance,
                                     ModuleName,
                                     Length(ModuleName));
  if (ModuleLength = 0) then
    Exit(HRESULT_FROM_WIN32(GetLastError));

  if (ModuleLength >= DWORD(Length(ModuleName))) then
    Exit(HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER));

  Result := SetRegistryString(ClassKey,
                              '',
                              FACTORYX_AUDIO_DELAY_MFT_NAME);
  if SUCCEEDED(Result) then
    Result := SetRegistryString(ServerKey,
                                '',
                                ModuleName);

  if SUCCEEDED(Result) then
    Result := SetRegistryString(ServerKey,
                                'ThreadingModel',
                                'Both');
  if FAILED(Result) then
    begin
      RegDeleteKeyW(HKEY_LOCAL_MACHINE,
                    PWideChar(ServerKey));

      RegDeleteKeyW(HKEY_LOCAL_MACHINE,
                    PWideChar(ClassKey));
    end;
end;


function UnregisterComObject(): HResult;
var
  ClassKey: UnicodeString;
  ServerKey: UnicodeString;
  ErrorCode: LongInt;

begin

  ClassKey := 'Software\Classes\CLSID\' + GUIDToString(CLSID_FactoryXAudioDelayMFT);
  ServerKey := ClassKey + '\InprocServer32';

  ErrorCode := RegDeleteKeyW(HKEY_LOCAL_MACHINE,
                             PWideChar(ServerKey));

  if (ErrorCode = ERROR_SUCCESS) or (ErrorCode = ERROR_FILE_NOT_FOUND) then
    ErrorCode := RegDeleteKeyW(HKEY_LOCAL_MACHINE,
                               PWideChar(ClassKey));

  if (ErrorCode = ERROR_FILE_NOT_FOUND) then
    ErrorCode := ERROR_SUCCESS;

  Result := HRESULT_FROM_WIN32(ErrorCode);
end;


function DllGetClassObject(const AClsid, AIid: TGUID;
                           out AObject: Pointer): HResult; stdcall;
var
  Factory: IClassFactory;

begin

  AObject := nil;
  if not IsEqualGUID(AClsid,
                     CLSID_FactoryXAudioDelayMFT) then
    Exit(CLASS_E_CLASSNOTAVAILABLE);

  try
    Factory := CreateAudioDelayClassFactory();
    Result := Factory.QueryInterface(AIid,
                                     AObject);

  except
    on EOutOfMemory do Result := E_OUTOFMEMORY;
    on Exception do Result := E_UNEXPECTED;
  end;
end;


function DllCanUnloadNow(): HResult; stdcall;
begin

  Result := AudioDelayCanUnloadNow();
end;


function DllRegisterServer(): HResult; stdcall;
var
  AudioType: MFT_REGISTER_TYPE_INFO;
  FriendlyName: WideString;

begin

  Result := RegisterComObject();
  if FAILED(Result) then
    Exit;

  // Registration advertises Audio/PCM; SetInputType and SetOutputType in the
  // transform check the complete PCM format, including channels and bit depth.
  AudioType.guidMajorType := MFMediaType_Audio;
  AudioType.guidSubtype := MFAudioFormat_PCM;
  FriendlyName := FACTORYX_AUDIO_DELAY_MFT_NAME;

  Result := MFTRegister(CLSID_FactoryXAudioDelayMFT,
                        MFT_CATEGORY_AUDIO_EFFECT,
                        PWideChar(FriendlyName),
                        UINT32(MFT_ENUM_FLAG_SYNCMFT),
                        1,
                        @AudioType,
                        1,
                        @AudioType,
                        nil);
  if FAILED(Result) then
    UnregisterComObject();
end;


function DllUnregisterServer(): HResult; stdcall;
var
  MftResult: HResult;
  ComResult: HResult;

begin

  // Attempt both removals, even after a partial registration.
  MftResult := MFTUnregister(CLSID_FactoryXAudioDelayMFT);
  ComResult := UnregisterComObject();

  if FAILED(MftResult) then
    Result := MftResult
  else
    Result := ComResult;
end;


exports
  DllGetClassObject name 'DllGetClassObject',
  DllCanUnloadNow name 'DllCanUnloadNow',
  DllRegisterServer name 'DllRegisterServer',
  DllUnregisterServer name 'DllUnregisterServer';

begin
end.
