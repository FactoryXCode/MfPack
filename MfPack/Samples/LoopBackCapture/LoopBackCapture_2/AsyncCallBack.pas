unit AsyncCallBack;

interface

uses
  WinApi.Windows,
  //WinAPI.Messages,
  //WinApi.ComBaseApi,
 // WinApi.WinApiTypes,
  {System}
  System.Classes,
  //System.Sysutils,
  {Vcl}
  //Vcl.Dialogs,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  //WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  //WinApi.MediaFoundationApi.MfIdl,
  {Application}
  LoopBackCapture;

const
  IID_IUnknown = IUnknown; // or IID_IUnknown = TGUID = '{00000000-0000-0000-C000-000000000046}';

type
  TCallbackAsync = class(TInterfacedObject, IMFAsyncCallback)
  private
    _parent: TLoopbackCapture;
    _dwQueueID: DWORD;

  public

    constructor Create(AParent: TLoopbackCapture;
                       AQueueID: DWORD = MFASYNC_CALLBACK_QUEUE_MULTITHREADED);
    //destructor Destroy(); override;

    //
    function AddRef: ULONG; stdcall;
    function Release: ULONG; stdcall;
    function QueryInterface(const IID: TGUID;
                            out Obj): HResult; stdcall;


  {$region IActivateAudioInterfaceCompletionHandler implementation}
    function GetParameters(out pdwFlags: DWord;
                           out pdwQueue: DWord): HResult; stdcall;
    function Invoke(const pResult: IMFAsyncResult): HResult; stdcall;
  {$endregion}

    procedure SetQueueID(dwQueueID: DWORD);
  end;


implementation


constructor TCallbackAsync.Create(AParent: TLoopbackCapture;
                                  AQueueID: DWORD);
begin
  inherited Create;
  _parent := AParent;
  _dwQueueID := AQueueID;
end;


function TCallbackAsync.AddRef: ULONG;
begin
   Result := IMFAsyncCallback(_parent)._AddRef;
end;


function TCallbackAsync.Release: ULONG;
begin
  Result := IMFAsyncCallback(_parent)._Release;
end;


function TCallbackAsync.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if (IsEqualGUID(IID, IID_IMFAsyncCallback) or IsEqualGUID(IID, IID_IUnknown)) then
  begin
    Pointer(Obj) := Self;
    Result := S_OK;
    Exit;
  end;
  Pointer(Obj) := nil;
  Result := E_NOINTERFACE;
end;


function TCallbackAsync.GetParameters(out pdwFlags: DWord;
                                      out pdwQueue: DWord): HResult;
begin
  pdwFlags := 0;
  pdwQueue := _dwQueueID;
  Result := S_OK;
end;


function TCallbackAsync.Invoke(const pResult: IMFAsyncResult): HResult;
begin
  TLoopbackCapture(_parent).pfnCallback(pResult);
  Result := S_OK;
end;


procedure TCallbackAsync.SetQueueID(dwQueueID: DWORD);
begin
  _dwQueueID := dwQueueID;
end;



end.
