unit ScreenActivityPinger;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.ExtCtrls;

type

  TScreenActivityPinger = class
  private

    FPingWnd: HWND; // Native window handle
    FTimer: TTimer;
    FOn: Boolean;
    FTargetFps: Integer;
    FDesktopRect: TRect; // DXGI desktop rectangle (virtual coords)

    FColor: TColor;
    FHorSize: Integer;
    FVertSize: Integer;

    procedure TimerTick(Sender: TObject);
    procedure CreatePingWindow();
    procedure DestroyPingWindow();
    procedure PaintPingWindow(Color: TColor);

  public

    constructor Create(const aDesktopRect: TRect;
                       aFps: Integer;
                       aHorSize: Integer;
                       aVertSize: Integer;
                       aColor: TColor);

    destructor Destroy(); override;

    procedure Start();
    procedure Stop();
  end;

implementation

{-------------------------------------------------------------------------------
  Window Procedure (static callback)
-------------------------------------------------------------------------------}

function PingerWndProc(aHwnd: HWND;
                       aMsg: UINT;
                       awParam: WPARAM;
                       alParam: LPARAM):
                       LRESULT; stdcall;
var
  pPaintStruct: PAINTSTRUCT;
  pHdc: HDC;

begin
  case aMsg of
    WM_PAINT:
      begin
        phdc := BeginPaint(aHwnd,
                           pPaintStruct);

        if (pHdc = 0) then
            begin
              EndPaint(aHwnd,
                       pPaintStruct);
              raise Exception.Create('No display device context is available.');
            end;
      end;
  end;

  Result := DefWindowProc(aHwnd,
                          aMsg,
                          awParam,
                          alParam);
end;

{-------------------------------------------------------------------------------
  TScreenActivityPinger
-------------------------------------------------------------------------------}

constructor TScreenActivityPinger.Create(const aDesktopRect: TRect;
                                         aFps: Integer;
                                         aHorSize: Integer;
                                         aVertSize: Integer;
                                         aColor: TColor);
begin

  inherited Create;

  FDesktopRect := ADesktopRect;

  if (AFps <= 0) then
    FTargetFps := 30
  else
    FTargetFps := AFps;

  FPingWnd := 0;
  FHorSize := aHorSize;
  FVertSize := aVertSize;
  FColor := aColor;

  CreatePingWindow;

  // timer
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerTick;
  FTimer.Interval := Round(1000 / FTargetFps);  // 30 FPS ≈ 33ms, 60 FPS ≈ 17ms
end;


destructor TScreenActivityPinger.Destroy;
begin

  Stop();
  DestroyPingWindow();
  FreeAndNil(FTimer);

  inherited;
end;


{-------------------------------------------------------------------------------
  Create a true independent top-level popup window
-------------------------------------------------------------------------------}

procedure TScreenActivityPinger.CreatePingWindow;
var
  pWndClass: WNDCLASS;
  x,
  y: Integer;

begin
  // register class once per process
  ZeroMemory(@pWndClass,
             SizeOf(pWndClass));

  pWndClass.style := CS_HREDRAW or CS_VREDRAW;
  pWndClass.lpfnWndProc := @PingerWndProc;
  pWndClass.hInstance := hInstance;
  pWndClass.hCursor := 0;
  pWndClass.hbrBackground := 0;
  pWndClass.lpszClassName := 'ScreenActivityPingerWnd';

  RegisterClassW(pWndClass);

  // choose position from DXGI desktop rectangle
  x := FDesktopRect.Right - FHorSize - 2;
  y := FDesktopRect.Bottom - FVertSize - 2;

  FPingWnd := CreateWindowEx(WS_EX_TOPMOST or WS_EX_TOOLWINDOW,
                             'ScreenActivityPingerWnd',
                             nil,
                             WS_POPUP,     // popup top-level window
                             x,
                             y,
                             FHorSize,     // small pixel window horizontal
                             FVertSize,    // small pixel window vertical
                             0,
                             0,
                             hInstance,
                             nil);

  if (FPingWnd = 0) then
    RaiseLastOSError;
end;


procedure TScreenActivityPinger.DestroyPingWindow;
begin

  if (FPingWnd <> 0) then
    begin
      DestroyWindow(FPingWnd);
      FPingWnd := 0;
    end;
end;

{-------------------------------------------------------------------------------
  Drawing the blinking pixel
-------------------------------------------------------------------------------}

procedure TScreenActivityPinger.PaintPingWindow(Color: TColor);
var
  pHdc: HDC;
  pBrush: HBRUSH;
  pRectangle: TRect;

begin

  pHdc := GetDC(FPingWnd);

  try

    pBrush := CreateSolidBrush(ColorToRGB(Color));

    GetClientRect(FPingWnd,
                  pRectangle);

    FillRect(pHdc,
             pRectangle,
             pBrush);

    DeleteObject(pBrush);

  finally

    ReleaseDC(FPingWnd,
              pHdc);
  end;
end;


{-------------------------------------------------------------------------------
  Start / Stop
-------------------------------------------------------------------------------}

procedure TScreenActivityPinger.Start();
begin

  if (FPingWnd = 0) then
    Exit;

  // Show without activating
  ShowWindow(FPingWnd,
             SW_SHOWNOACTIVATE);
  FOn := False;
  FTimer.Enabled := True;
end;


procedure TScreenActivityPinger.Stop;
begin

  FTimer.Enabled := False;

  if (FPingWnd <> 0) then
    ShowWindow(FPingWnd,
               SW_HIDE);
end;


{-------------------------------------------------------------------------------
  Timer Tick: toggle pixel color
-------------------------------------------------------------------------------}

procedure TScreenActivityPinger.TimerTick(Sender: TObject);
var
  pColor: TColor;

begin

  if FOn then
    pColor := clBlack
  else
    pColor := FColor;

  FOn := not FOn;

  PaintPingWindow(pColor);
end;

end.

