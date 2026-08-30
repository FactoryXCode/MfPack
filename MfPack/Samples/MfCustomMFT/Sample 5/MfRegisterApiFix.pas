// FactoryX
//
// Correct XE7 declaration for MFTRegister. The older MfPack declaration lacks
// const on the count and type-info pointer parameters; with Delphi XE7 that
// changes how these arguments are marshalled and causes ERROR_NOACCESS.

unit MfRegisterApiFix;

interface

uses
  {WinApi}
  WinApi.Windows,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects;

function MFTRegisterFixed(clsidMFT: TGUID;
                          guidCategory: TGUID;
                          pszName: LPCWSTR;
                          Flags: UINT32;
                          const cInputTypes: UINT32;
                          const pInputTypes: PMFT_REGISTER_TYPE_INFO;
                          const cOutputTypes: UINT32;
                          const pOutputTypes: PMFT_REGISTER_TYPE_INFO;
                          pAttributes: IMFAttributes): HRESULT; stdcall;

implementation

function MFTRegisterFixed; external 'mfplat.dll' name 'MFTRegister';

end.
