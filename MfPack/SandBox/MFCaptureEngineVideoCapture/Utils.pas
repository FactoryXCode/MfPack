unit Utils;


interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Messages,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  {Vcl}
  Vcl.Graphics,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfCaptureEngine,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils;


const
  IDTIMEOUT      = 'Unable to set the capture device.';
  ERR_INITIALIZE = 'Unable to initialize the capture engine.';
  ERR_PREVIEW    = 'An error occurred during preview.';
  ERR_RECORD     = 'An error occurred during recording.';
  ERR_CAPTURE    = 'An error occurred during capture.';
  ERR_PHOTO      = 'Unable to capture still photo.';
  ERR_OUTPUT_MEDIATYPE_SET = 'Unable to set the CaptureEngine MediaType output.';
  ERR_SET_DEVICE = 'Selecting a device failed.';

type
  // CriticalSection
  TMFCritSec = class
  private
    FCriticalSection: TRTLCriticalSection;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure Lock();
    procedure Unlock();

  end;

  // Simple error handler
  procedure ErrMsg(pErrMsg: string;
                   pHr: HResult);

implementation


{ TMFCritSec }

constructor TMFCritSec.Create();
begin
  InitializeCriticalSection(FCriticalSection);
end;


destructor TMFCritSec.Destroy();
begin
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;


procedure TMFCritSec.Lock();
begin
  EnterCriticalSection(FCriticalSection);
end;


procedure TMFCritSec.Unlock();
begin
  LeaveCriticalSection(FCriticalSection);
end;


// ErrMsg
procedure ErrMsg(pErrMsg: string;
                 pHr: HResult);
begin
{$IFDEF DEBUG}
  OutputDebugString(StrToPWideChar(Format('Error: %s (hr = %d)',
                                          [pErrMsg, pHr])))
{$ELSE}

  ShowMessage(format('Error: %s (hr = %d)',
                     [pErrMsg, pHr]));
{$ENDIF}
end;

end.
