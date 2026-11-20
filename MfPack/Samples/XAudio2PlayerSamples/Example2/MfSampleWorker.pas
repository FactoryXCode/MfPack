unit MfSampleWorker;

{$IFDEF FPC}{$I MfPack.Fpc.inc}{$ENDIF}

interface

uses {$IFDEF FPC}Classes,SysUtils,Math,{$ELSE}System.Classes,System.SysUtils,System.Math,{$ENDIF}
     WinApi.WinApiTypes,WinApi.ComBaseApi,WinApi.ActiveX.ObjBase;

type
  TSampleWorker = class(TThread)
  private
    FRun: TThreadMethod;
  protected
    procedure Execute(); override;
  public
    constructor Create(const Run: TThreadMethod);
  end;

implementation

constructor TSampleWorker.Create(const Run: TThreadMethod);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FRun := Run;
end;

procedure TSampleWorker.Execute();
var
  ComResult: HRESULT;
  SavedMask: TFPUExceptionMask;
begin
  ComResult := CoInitializeEx(nil,COINIT_MULTITHREADED);
  if ComResult < 0 then raise Exception.Create('Worker COM initialization failed');
  SavedMask := GetExceptionMask();
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  try
    FRun();
  finally
    ClearExceptions(False);
    SetExceptionMask(SavedMask);
    CoUninitialize();
  end;
end;

end.
