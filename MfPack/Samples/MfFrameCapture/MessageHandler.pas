unit MessageHandler;

interface

uses
  {Winapi}
  WinApi.Windows, //
  WinApi.Messages, //
  {System}
  System.Classes;

type
  TOnHandleMessage = reference to procedure(var AMessage : TMessage; var AHandled : boolean);

  TMessageHandler = class(TObject)
  private
    FWinHandle : HWND;
    FOnHandleMessage : TOnHandleMessage;
  protected
    procedure HandleWindowsMessage(var AMessage : TMessage); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AllocateHandle;
    procedure RemoveHandle;

    property Handle : HWND read FWinHandle;
    property OnMessage : TOnHandleMessage read FOnHandleMessage write FOnHandleMessage;
  end;

implementation

{ TMessageHandler }

constructor TMessageHandler.Create;
begin
  inherited;
  FWinHandle := INVALID_HANDLE_VALUE;
  AllocateHandle;
end;

destructor TMessageHandler.Destroy;
begin
  RemoveHandle;
  inherited;
end;

procedure TMessageHandler.AllocateHandle;
begin
  RemoveHandle;
  FWinHandle := AllocateHWnd(HandleWindowsMessage);
end;

procedure TMessageHandler.RemoveHandle;
begin
  DeallocateHWnd(FWinHandle);
  FWinHandle := INVALID_HANDLE_VALUE;
end;

procedure TMessageHandler.HandleWindowsMessage(var AMessage : TMessage);
var
  bHandled : boolean;
begin
  bHandled := False;

  if Assigned(FOnHandleMessage) then
    FOnHandleMessage(AMessage, bHandled);

  if bHandled then
    AMessage.Result := 0
  else
    AMessage.Result := DefWindowProc(FWinHandle, AMessage.Msg, AMessage.WParam, AMessage.LParam);
end;

end.
