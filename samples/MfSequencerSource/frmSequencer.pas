unit frmSequencer;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ClipBrd,
  Helpers,
  MfSequencerSource;

type
  Tfrm_PlayListPlayer = class(TForm)
    pnlControls: TPanel;
    pnlVideo: TPanel;
    butPause: TButton;
    butStop: TButton;
    butPlay: TButton;
    butFullScreen: TButton;
    prbProgress: TProgressBar;
    MainMenu1: TMainMenu;
    muFile: TMenuItem;
    muOpen: TMenuItem;
    muSeparator1: TMenuItem;
    muExit: TMenuItem;
    Extra1: TMenuItem;
    mnuSetPosition: TMenuItem;
    mnuTakeScreenshot: TMenuItem;
    dlgOpenUrl: TOpenDialog;
    trbVolume: TTrackBar;
    procedure muOpenClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure butPauseClick(Sender: TObject);
    procedure butPlayClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure muExitClick(Sender: TObject);
    procedure mnuTakeScreenshotClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure butFullScreenClick(Sender: TObject);
    procedure trbVolumeChange(Sender: TObject);
    procedure prbProgressMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    bFullScreenMode: Boolean;
    DPlaylistPlayer: TDPlaylist;

    function CreatePlayer(): HResult;
    procedure DestroyPlayer();
    procedure RealignVideo();
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;
    procedure SetProgressbarPosition(iPos: int64);
    procedure FullScreen();

  public
    { Public declarations }

  end;

var
  frm_PlayListPlayer: Tfrm_PlayListPlayer;

implementation

{$R *.dfm}


function Tfrm_PlayListPlayer.CreatePlayer(): HResult;
begin
  Result := E_FAIL;
  if not Assigned(DPlaylistPlayer) then
    begin
      DPlaylistPlayer := Nil;
      // We want the video to be played on the VideoPanel, so, we use that handle.
      Result := TDPlaylist.CreateInstance(pnlVideo.Handle,           // The clipping window/control
                                          frm_PlayListPlayer.Handle, // The window or control that receives the (text) messages.
                                          DPlaylistPlayer);          // Must be main form!

      // If you want a different clipping surface, for instance an other TPanel or TForm
      // DPlaylistPlayer.SetVideoSurface := myVideoSurface.Handle;
    end;
end;


procedure Tfrm_PlayListPlayer.DestroyPlayer();
begin
  if Assigned(DPlaylistPlayer) then
    begin
      if Succeeded(DPlaylistPlayer.ShutDown()) then
        begin
          prbProgress.Enabled := False;
          mnuSetPosition.Enabled := False;
          pnlControls.Enabled := false;
          mnuTakeScreenshot.Enabled := False;
          FreeAndNil(DPlaylistPlayer);
        end;
    end;
end;


procedure Tfrm_PlayListPlayer.FormCloseQuery(Sender: TObject;
                                             var CanClose: Boolean);
begin
  CanClose := False;
  DestroyPlayer();
  CanClose := True;
end;


procedure Tfrm_PlayListPlayer.FormCreate(Sender: TObject);
begin
  bFullScreenMode := False;
end;

procedure Tfrm_PlayListPlayer.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SPACE:   if Assigned(DPlaylistPlayer) then
                  begin
                    case DPlaylistPlayer.State of
                      Started: DPlaylistPlayer.Pause();
                      OpenPending, Stopped, Paused: DPlaylistPlayer.Play();
                    end;
                  end;

    VK_END:     if Assigned(DPlaylistPlayer) then
                  begin
                    DPlaylistPlayer.Stop();
                  end;

    VK_F11:     begin
                  if Assigned(DPlaylistPlayer) then
                    DPlaylistPlayer.ShutDown();
                end;

    VK_F12:       muOpenClick(nil);

    // Take a snapshot and copy the bitmap to the clipboard
    VK_F8:        mnuTakeScreenshotClick(Nil);

    // Use left and right arrows to adjust the volume.
    // Since there is a trackbar, this one will have the focus.
    VK_LEFT:      DPlaylistPlayer.Volume := DPlaylistPlayer.Volume - 0.1;

    VK_RIGHT:     DPlaylistPlayer.Volume := DPlaylistPlayer.Volume + 0.1;

    VK_ESCAPE:    FullScreen();

  end;
end;


procedure Tfrm_PlayListPlayer.mnuTakeScreenshotClick(Sender: TObject);
var
  bm: TBitmap;

begin
  bm := TBitmap.Create;
  DPlaylistPlayer.TakeSnapShot(bm);
  // copy the bitmap to the Clipboard
  Clipboard.Assign(bm);
  FreeAndNil(bm);
end;


procedure Tfrm_PlayListPlayer.muExitClick(Sender: TObject);
begin
  Close();
end;


procedure Tfrm_PlayListPlayer.muOpenClick(Sender: TObject);
begin
  // End the previous session
  DestroyPlayer();

  if Succeeded(CreatePlayer()) then
    begin
      if dlgOpenUrl.Execute then
        DPlaylistPlayer.OpenURL(PWideChar(dlgOpenUrl.Filename));
        mnuSetPosition.Enabled := True;
        mnuTakeScreenshot.Enabled := True;
        pnlControls.Enabled := True;
    end
  else
    MessageBox(0,
               lpcwstr('Failed to initialize MfPlayer.'),
               lpcwstr('Initial Failure!'),
               MB_ICONERROR);
end;


procedure Tfrm_PlayListPlayer.prbProgressMouseUp(Sender: TObject;
                                                 Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  fPos: Single;
begin
  if Assigned(DPlaylistPlayer) then
    begin
      if (X <= 0) then
        fPos := 0.0
      else
        fPos := ((X / prbProgress.Width) * DPlaylistPlayer.Duration);

      if DPlaylistPlayer.State in [Started] then
        DPlaylistPlayer.Position := trunc(fPos);

      // set new StartPosition
      if DPlaylistPlayer.State in [Paused] then
        DPlaylistPlayer.Pause();

      prbProgress.Position := X;
    end;
end;


procedure Tfrm_PlayListPlayer.RealignVideo();
begin
  // Adjust the max value from the progressbar
  prbProgress.Max := prbProgress.Width;

  if Assigned(DPlaylistPlayer) then
    begin
      // Set video size
      DPlaylistPlayer.ResizeVideo(pnlVideo.ClientWidth,
                                  pnlVideo.ClientHeight);

      // Adjust size of the peakmeters
      if DPlaylistPlayer.HasVideo = False then
        begin
          //MfPeakMeter1.Height := pnlVideo.Height;
          //MfPeakMeter2.Height := pnlVideo.Height;
          //MfPeakMeter1.Top := pnlVideo.Top;
          //MfPeakMeter2.Top := pnlVideo.Top;
        end;
    end;
end;


procedure Tfrm_PlayListPlayer.WMSize(var Msg: TWMSize);
begin
  Inherited;  // OnResize method will be handled first
  RealignVideo();
end;


procedure Tfrm_PlayListPlayer.WMProgressEvent(var Msg: TMessage);
begin
  // WParam 1 is a subtitle text event
  if (Msg.WParam = 1) then
    if (DPlaylistPlayer.State = Started) then
      SetProgressbarPosition(DPlaylistPlayer.Position);
end;


procedure Tfrm_PlayListPlayer.SetProgressbarPosition(iPos: int64);
var
  uiDuration: UInt64;
begin
  if prbProgress.Enabled then
    begin
      if Succeeded(DPlaylistPlayer.GetDuration(uiDuration)) then
        prbProgress.Position := Trunc((prbProgress.Width / uiDuration) * iPos);
    end;
end;


procedure Tfrm_PlayListPlayer.trbVolumeChange(Sender: TObject);
begin
  if Assigned(DPlaylistPlayer) then
    DPlaylistPlayer.Volume := (trbVolume.Position * 0.1);
end;


procedure Tfrm_PlayListPlayer.butFullScreenClick(Sender: TObject);
begin
  FullScreen();
end;


procedure Tfrm_PlayListPlayer.butPauseClick(Sender: TObject);
begin
  if Assigned(DPlaylistPlayer) then
    DPlaylistPlayer.Pause();
end;


procedure Tfrm_PlayListPlayer.butPlayClick(Sender: TObject);
begin
  if Assigned(DPlaylistPlayer) then
    if DPlaylistPlayer.State in [OpenPending, Stopped, Paused] then
      begin
        prbProgress.Max := 1000000;  // = 1 sec
        prbProgress.Enabled := True;
        if DPlaylistPlayer.State in [Started, Ready, OpenPending] then
          trbVolume.Position := Trunc(DPlaylistPlayer.Volume) * 10;

        pnlVideo.Caption := '';
        DPlaylistPlayer.Play;
        mnuTakeScreenshot.Enabled := True;
      end;
end;


procedure Tfrm_PlayListPlayer.butStopClick(Sender: TObject);
begin
  if Assigned(DPlaylistPlayer) then
    begin
      DPlaylistPlayer.Stop();
      SetProgressbarPosition(0);
      mnuTakeScreenshot.Enabled := False;
    end;
end;

// NOTE: On earlier versions of Windows (Win 7 and probarly Win 8.0/8.1) this procedure wil not work
//       in a convinient way. The samples MfPlayer II and X have a frmFullScreenLayer, to
//       accomplish the same, and works on all Windows versions.

procedure Tfrm_PlayListPlayer.FullScreen();
begin
   if (bFullScreenMode = False) then
     begin
       // Create the Full-Screen form
       pnlControls.Visible := False;
       pnlVideo.Visible := False;
       Menu := Nil;
       BorderStyle := bsNone;
       WindowState := wsMaximized;
       // Let Player know we changed the clipping window (video surface).
       if Assigned(DPlaylistPlayer) then
         begin
           DPlaylistPlayer.VideoSurfaceHandle := Handle;
           DPlaylistPlayer.ResizeVideo(Width, Height);
         end;
       bFullScreenMode := True;
     end
   else  // if FullScreenMode = False, get to the original window state
     begin
       // Keep the form always on top
       pnlControls.Visible := True;
       pnlVideo.Visible := True;
       Menu := MainMenu1;
       BorderStyle := bsSizeable;
       WindowState := wsNormal;
       // Let Player know we changed the clipping window to the panel.
       if Assigned(DPlaylistPlayer) then
         begin
           DPlaylistPlayer.VideoSurfaceHandle := pnlVideo.Handle;
           DPlaylistPlayer.ResizeVideo(pnlVideo.Width, pnlVideo.Height);
         end;
       bFullScreenMode := False;
    end;

  // Prior to SDK version RedStone5, for fullscreen modus you could use this function:
  // IMFVideoDisplayControl.GetFullscreen / IMFVideoDisplayControl.SetFullscreen.
  // However this API is deprecated and not functioning since SDK version RedStone5
  //===============================================================================
end;

end.
