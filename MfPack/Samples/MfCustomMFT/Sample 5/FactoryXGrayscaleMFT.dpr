library FactoryXGrayscaleMFT;

uses

  WinApi.Windows,
  WinApi.WinError,

  System.SysUtils,
  System.Win.ComServ,

  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,

  MfRegisterApiFix in 'MfRegisterApiFix.pas',
  RegisteredGrayscaleMFT in 'RegisteredGrayscaleMFT.pas';

function SetRegistryString(const AKeyName: UnicodeString;
                           const AValueName: UnicodeString;
                           const AValue: UnicodeString): HRESULT;
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


function RegisterComObject(): HRESULT;
var
  ClsidText: UnicodeString;
  ClassKey: UnicodeString;
  ServerKey: UnicodeString;
  ModuleName: array[0..MAX_PATH - 1] of WideChar;

begin

  ClsidText := GUIDToString(CLSID_FactoryXGrayscaleMFT);
  ClassKey := 'Software\Classes\CLSID\' + ClsidText;
  ServerKey := ClassKey + '\InprocServer32';

  if GetModuleFileNameW(HInstance,
                        ModuleName,
                        Length(ModuleName)) = 0 then
    Exit(HRESULT_FROM_WIN32(GetLastError));

  Result := SetRegistryString(ClassKey, '',
                              FACTORYX_GRAYSCALE_MFT_NAME);
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


function UnregisterComObject: HRESULT;
var
  ClassKey: UnicodeString;
  ServerKey: UnicodeString;
  ErrorCode: LongInt;

begin

  ClassKey := 'Software\Classes\CLSID\' + GUIDToString(CLSID_FactoryXGrayscaleMFT);

  ServerKey := ClassKey + '\InprocServer32';
  ErrorCode := RegDeleteKeyW(HKEY_LOCAL_MACHINE,
                             PWideChar(ServerKey));

  if (ErrorCode = ERROR_SUCCESS) or (ErrorCode = ERROR_FILE_NOT_FOUND) then
    ErrorCode := RegDeleteKeyW(HKEY_LOCAL_MACHINE, PWideChar(ClassKey));

  if (ErrorCode = ERROR_FILE_NOT_FOUND) then
    ErrorCode := ERROR_SUCCESS;

  Result := HRESULT_FROM_WIN32(ErrorCode);
end;


function DllRegisterServer(): HRESULT; stdcall;
var
  InputType: MFT_REGISTER_TYPE_INFO;
  OutputType: MFT_REGISTER_TYPE_INFO;
  FriendlyName: WideString;

begin

  Result := RegisterComObject;

  if FAILED(Result) then
    Exit;

  InputType.guidMajorType := MFMediaType_Video;
  InputType.guidSubtype := MFVideoFormat_RGB32;
  OutputType := InputType;
  FriendlyName := FACTORYX_GRAYSCALE_MFT_NAME;

  Result := MFTRegisterFixed(CLSID_FactoryXGrayscaleMFT,
                             MFT_CATEGORY_VIDEO_EFFECT,
                             PWideChar(FriendlyName),
                             UINT32(MFT_ENUM_FLAG_SYNCMFT),
                             1,
                             @InputType,
                             1,
                             @OutputType,
                             nil);
  if FAILED(Result) then
    UnregisterComObject;
end;


function DllUnregisterServer(): HRESULT; stdcall;
var
  MftResult: HRESULT;
  ComResult: HRESULT;

begin

  // Always attempt both removals so a partial earlier registration is
  // recoverable with one unregister operation.
  MftResult := MFTUnregister(CLSID_FactoryXGrayscaleMFT);
  ComResult := UnregisterComObject;

  if FAILED(MftResult) then
    Result := MftResult
  else
    Result := ComResult;
end;

exports
  System.Win.ComServ.DllGetClassObject name 'DllGetClassObject',
  System.Win.ComServ.DllCanUnloadNow name 'DllCanUnloadNow',
  DllRegisterServer name 'DllRegisterServer',
  DllUnregisterServer name 'DllUnregisterServer';

begin
end.
