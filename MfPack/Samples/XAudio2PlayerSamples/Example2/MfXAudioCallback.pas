unit MfXAudioCallback;

{$IFDEF FPC}{$I MfPack.Fpc.inc}{$ENDIF}

interface

uses WinApi.WinApiTypes,WinApi.DirectX.XAudio2.XAudio2;

type
  TNativeXAudioCallback = class;
  TXAudioCallbackBridge = record
    Native: IXAudio2VoiceCallback;
    Owner: TNativeXAudioCallback;
  end;
  PXAudioCallbackBridge = ^TXAudioCallbackBridge;

  TNativeXAudioCallback = class
  private
    FBridge: TXAudioCallbackBridge;
    function GetCallbackPointer(): PIXAudio2VoiceCallback;
  public
    constructor Create();
    procedure OnVoiceProcessingPassStart(BytesRequired: UINT32); virtual; stdcall;
    procedure OnVoiceProcessingPassEnd(); virtual; stdcall;
    procedure OnStreamEnd(); virtual; stdcall;
    procedure OnBufferStart(pBufferContext: Pointer); virtual; stdcall;
    procedure OnBufferEnd(pBufferContext: Pointer); virtual; stdcall;
    procedure OnLoopEnd(pBufferContext: Pointer); virtual; stdcall;
    procedure OnVoiceError(pBufferContext: Pointer;Error: HRESULT); virtual; stdcall;
    property CallbackPointer: PIXAudio2VoiceCallback read GetCallbackPointer;
  end;

implementation

procedure NativeOnVoiceProcessingPassStart(This: Pointer;BytesRequired: UINT32); stdcall;
begin
  try
    PXAudioCallbackBridge(This).Owner.OnVoiceProcessingPassStart(BytesRequired);
  except
    // Pascal exceptions must not cross an XAudio2 native callback boundary.
  end;
end;

procedure NativeOnVoiceProcessingPassEnd(This: Pointer); stdcall;
begin
  try
    PXAudioCallbackBridge(This).Owner.OnVoiceProcessingPassEnd();
  except
    // Pascal exceptions must not cross an XAudio2 native callback boundary.
  end;
end;

procedure NativeOnStreamEnd(This: Pointer); stdcall;
begin
  try
    PXAudioCallbackBridge(This).Owner.OnStreamEnd();
  except
    // Pascal exceptions must not cross an XAudio2 native callback boundary.
  end;
end;

procedure NativeOnBufferStart(This: Pointer;pBufferContext: Pointer); stdcall;
begin
  try
    PXAudioCallbackBridge(This).Owner.OnBufferStart(pBufferContext);
  except
    // Pascal exceptions must not cross an XAudio2 native callback boundary.
  end;
end;

procedure NativeOnBufferEnd(This: Pointer;pBufferContext: Pointer); stdcall;
begin
  try
    PXAudioCallbackBridge(This).Owner.OnBufferEnd(pBufferContext);
  except
    // Pascal exceptions must not cross an XAudio2 native callback boundary.
  end;
end;

procedure NativeOnLoopEnd(This: Pointer;pBufferContext: Pointer); stdcall;
begin
  try
    PXAudioCallbackBridge(This).Owner.OnLoopEnd(pBufferContext);
  except
    // Pascal exceptions must not cross an XAudio2 native callback boundary.
  end;
end;

procedure NativeOnVoiceError(This: Pointer;pBufferContext: Pointer;Error: HRESULT); stdcall;
begin
  try
    PXAudioCallbackBridge(This).Owner.OnVoiceError(pBufferContext,Error);
  except
    // Pascal exceptions must not cross an XAudio2 native callback boundary.
  end;
end;

var
  CallbackVTable: IXAudio2VoiceCallbackVTable;

constructor TNativeXAudioCallback.Create();
begin
  inherited Create();
  FBridge.Native.lpVtbl := @CallbackVTable;
  FBridge.Owner := Self;
end;

function TNativeXAudioCallback.GetCallbackPointer(): PIXAudio2VoiceCallback;
begin
  Result := @FBridge.Native;
end;

procedure TNativeXAudioCallback.OnVoiceProcessingPassStart(BytesRequired: UINT32);
begin
end;

procedure TNativeXAudioCallback.OnVoiceProcessingPassEnd();
begin
end;

procedure TNativeXAudioCallback.OnStreamEnd();
begin
end;

procedure TNativeXAudioCallback.OnBufferStart(pBufferContext: Pointer);
begin
end;

procedure TNativeXAudioCallback.OnBufferEnd(pBufferContext: Pointer);
begin
end;

procedure TNativeXAudioCallback.OnLoopEnd(pBufferContext: Pointer);
begin
end;

procedure TNativeXAudioCallback.OnVoiceError(pBufferContext: Pointer;Error: HRESULT);
begin
end;

initialization
  CallbackVTable.OnVoiceProcessingPassStart := @NativeOnVoiceProcessingPassStart;
  CallbackVTable.OnVoiceProcessingPassEnd := @NativeOnVoiceProcessingPassEnd;
  CallbackVTable.OnStreamEnd := @NativeOnStreamEnd;
  CallbackVTable.OnBufferStart := @NativeOnBufferStart;
  CallbackVTable.OnBufferEnd := @NativeOnBufferEnd;
  CallbackVTable.OnLoopEnd := @NativeOnLoopEnd;
  CallbackVTable.OnVoiceError := @NativeOnVoiceError;
end.
