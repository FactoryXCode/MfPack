unit XAudio2_FXMasterLimiter;

interface

uses
  WinApi.Windows,
  System.Classes,
  WinApi.MediaFoundationApi.MfError,
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAPOFx,
  XAudio2_Globals;


type
  TFxMasterLimiter = class(TObject)
  private
    pvMasterLimiterparams: FXMASTERINGLIMITER_PARAMETERS;

  public

    constructor Create();
    destructor Destroy(); override;

    function CreateMasterLimiter(const pMasterLimiterParams: FXMASTERINGLIMITER_PARAMETERS): HResult;

  end;

var
  XAPOMasterLimiter: IUnknown;


implementation


constructor TFxMasterLimiter.Create();
begin
  inherited;

  pvMasterLimiterparams.Release := FXMASTERINGLIMITER_DEFAULT_RELEASE;
  pvMasterLimiterparams.Loudness := FXMASTERINGLIMITER_DEFAULT_LOUDNESS;
end;


destructor TFxMasterLimiter.Destroy();
begin
  if Assigned(XAPOMasterLimiter) then
    XAPOMasterLimiter := nil;
  inherited;
end;


function TFxMasterLimiter.CreateMasterLimiter(const pMasterLimiterParams: FXMASTERINGLIMITER_PARAMETERS): HResult;
var
  hr: HResult;

begin
  // Remove previous XAPO.
  XAPOMasterLimiter := nil;

  hr := CreateFX(IID_FXMasteringLimiter,
                 XAPOMasterLimiter,
                 @pMasterLimiterParams,
                 SizeOf(FXMASTERINGLIMITER_PARAMETERS));
  Result := hr;
end;

end.
