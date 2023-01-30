// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FloatingFrm.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 3.1.3
// Description: Floating form that projects TimedText.
//
//
// Company: FactoryX
// Intiator(s): Ramyses De Macedo Rodrigues, Tony (maXcomX), Peter (OzShips).
// Contributor(s): Ramyses De Macedo Rodrigues, Tony Kalf (maXcomX), Peter Larson (ozships).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: -
//
// Copyright (c) FactoryX. All rights reserved.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit FloatingFrm;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.Types,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfIdl,
  {Project}
  TimedTextClass,
  MfPCXConstants;


type
  // Screen capabillities
  TScrRes = record
    ScrResH: Integer;      // Horizontal width in pixels
    ScrResV: Integer;      // Vertical height in pixels
    LogPixelsX: Integer;   // Logical pixelsinch in X
    LogPixelsY: Integer;   // Logical pixelsinch in Y
  end;

  TFloatingForm = class(TForm)
    lblSubTitle: TLabel;
    Label1: TLabel;
    Label2: TLabel;

    procedure AfterConstruction(); override;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  protected
    procedure CreateParams(var Params: TCreateParams); override;

  private
    { Private declarations }

    crTransparency: TColor;               // transparency key color
    btOpacity: Byte;                      // opacity 0 - 255
    sSubTitleTxt: string;                 // Subtitle string that needs to be displayed
    // To set the margins for the label, use the form's Padding property
    iTextLines: Integer;                  // Number of textlines to be displayed
    fLayerFont: TFont;                    // The subtitlefont  (recommended is the BBC standard Tiresias™, free Font by The Royal National Institute for the Blind)
    CalculatedFontSize: Integer;          // The calculated fontsize when sizing the window.
    bShowCustomText: Boolean;             // Show custom text when true
    fClockProperties: MFCLOCK_PROPERTIES; // Needed to adjust jitter
    hVideoSurface: HWnd;                  // The video area like a TPanel, TForm or other windowed control
    bTimedTextFileLoaded: Boolean;        // True if a timedtextfile is loaded, false if not.
    // TimedText
    fTimedTextFile: WideString;  // TimedText filename
    FPreferredLanguage: string;  // Preferred TimedText language
    bNewTrackAvailable: Boolean; // This boolean will be set if a new track is presented.

    procedure SetShowCustomText(aValue: Boolean);
    procedure SetSubTitleText(aValue: string);
    procedure GetTextLines(aValue: string);  // use this property to get the number of textlines
    procedure SetFormPosition();  // Set form position if auto
    procedure SetFont(aValue: TFont);
    procedure SetPreferredLanguage(aValue: string); // Sets the preferred subtitlelanguage
    procedure SetVideoSurfaceHandle(aValue: HWND);
    function GetScreenResolution(): TScrRes;
    procedure CalculateFontSize();

    // Custom messages to this object
    procedure WMProgressEvent(var Msg: TMessage); message WM_PROGRESSNOTIFY;
    procedure WMTimedTextUpdateEvent(var Msg: TMessage); message WM_TIMEDTEXTNOTIFY_UPDATE;
    procedure WMParentSizeEvent(var Msg: TMessage); message WM_PARENTSIZECHANGED;
    procedure WMParentPositionEvent(var Msg: TMessage); message WM_PARENTPOSCHANGED;
    procedure WMSizeEvent(var Msg: TMessage); message WM_SIZE;
    procedure WMSyscommand(var Msg: TWmSysCommand); message WM_SYSCOMMAND;


  public
    { Public declarations }
    VideoSurfaceRect: TRect;        // dimensions of the videosurface control

    constructor Create(AOwner: TComponent;
                       hwVideoSurface: HWND;
                       TimedTxtFile: WideString;
                       sPreferredLanguage: string;
                       ClockProperties: MFCLOCK_PROPERTIES); reintroduce;

    function OpenTimedTextFile(aValue: WideString): HResult;

  published

    // You need to set FullScreenMode from the caller when running fullscreen
    property Opacity: Byte read btOpacity;
    property ShowCustomText: Boolean read bShowCustomText write SetShowCustomText default True;
    property SubtitleText: string read sSubTitleTxt write SetSubTitleText;
    property TextLines: Integer read iTextLines; // read only
    property LayerFont: TFont read fLayerFont write SetFont;
    //
    property TimedTextFile: WideString read fTimedTextFile;
    property TimedTextFileIsLoaded: Boolean read bTimedTextFileLoaded;
    property PreferredLanguage: string read fPreferredLanguage write SetPreferredLanguage;
    property VideoSurfaceHandle: HWnd read hVideoSurface write SetVideoSurfaceHandle;
  end;

var
  FloatingForm: TFloatingForm;

implementation

{$R *.dfm}

uses
  MfPlayerClassX;  // Needed for clocktiming

const
  User32Lib = 'user32.dll';


constructor TFloatingForm.Create(AOwner: TComponent;
                                 hwVideoSurface: HWND;
                                 TimedTxtFile: WideString;
                                 sPreferredLanguage: string;
                                 ClockProperties: MFCLOCK_PROPERTIES);
begin
  inherited Create(AOwner);

  VideoSurfaceHandle := hwVideoSurface;

  fTimedTextFile := TimedTxtFile;
  fPreferredLanguage := sPreferredLanguage;
  fClockProperties := ClockProperties;
  bNewTrackAvailable := False;
  bTimedTextFileLoaded := False;
  Visible := False;

  // Initial settings
  btOpacity := 255;  // 255 = all controls on the form will be fully visible
  crTransparency := clBlack;
  Padding.Bottom := 20; // default bottom margin

  // Create default Font and set values
  fLayerFont := TFont.Create();
  with fLayerFont do
    begin
      Name        := 'Verdana';
      Charset     := DEFAULT_CHARSET;
      Orientation := 0;
      Size        := 12;
      Orientation := 0;
      Quality     := fqDefault;
      Pitch       := fpDefault;
      Height      := -19;
      Color       := clWhite;
      Style       := [fsBold];
    end;

  // Keep the form always on top
  FormStyle := fsNormal;
  BorderStyle := bsNone;
  Ctl3D := False;
  BorderIcons := [];
  AlphaBlend := True;
  AlphaBlendValue := btOpacity;
  TransparentColor := True;
  TransparentColorValue := clBlack;
end;


procedure TFloatingForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  FreeAndNil(fLayerFont);
  FreeAndNil(fTimedText);
  CanClose := True;
end;


procedure TFloatingForm.AfterConstruction();
var
  hr: HResult;

begin
  // Create the TimedText object
  fTimedText := TMfTimedText.Create(Self.Handle,
                                    fTimedTextFile,
                                    PreferredLanguage);
  if (fTimedText = Nil) then
    Close();

 // Initial text before subtitling starts (Optional)
 // This can be any message
 if (bShowCustomText = true) then
   begin
     SubtitleText:= 'Mediafile ' +
                     fTimedTextFile +
                     ' has been loaded...' + #13 +
                     'Click "Play" or hit the spacebar to start the movie...';
   end;

  // Try to get the TimedText file
  //
  // Enter the media file, the TimedText interface will deal with the formats found.
  // If the returnvalue = ERROR_FILE_NOT_FOUND the interface could not find a valid file
  hr := fTimedText.OpenTimedTextFile(fTimedTextFile);

  // If no timedtextfile is found, then close this form.
  if Succeeded(hr) then
    begin
      if (hr = ERROR_FILE_NOT_FOUND) or (hr = ERROR_INVALID_PARAMETER) then
        bTimedTextFileLoaded := False
      else
        begin
          bTimedTextFileLoaded := True;
          lblSubTitle.Caption := Format('Subtitled movie (%s). Language: %s',
                                        [ExtractFilename(fTimedText.TimedTextFile),
                                         fTimedText.FriendlyLanguage]);
          Show();
        end;
    end
  else
    bTimedTextFileLoaded := False;

  inherited AfterConstruction();
end;


procedure TFloatingForm.SetFormPosition();
begin

  // Note: We replace the top and left for the found position X and Y
  {void} SetWindowPos(Self.Handle,
                      HWND_TOP,
                      VideoSurfaceRect.Left,
                      VideoSurfaceRect.Top,
                      VideoSurfaceRect.Width,
                      VideoSurfaceRect.Height,
                      SWP_SHOWWINDOW);

  // Or use

  //SetBounds(VideoSurfaceRect.Left,
  //          VideoSurfaceRect.Top,
  //          VideoSurfaceRect.Width,
  //          VideoSurfaceRect.Height);

end;


procedure TFloatingForm.SetShowCustomText(aValue: Boolean);
begin
  bShowCustomText := aValue;
end;


procedure TFloatingForm.SetSubTitleText(aValue: string);
begin
  sSubTitleTxt := aValue;
  GetTextLines(aValue);
  lblSubTitle.Caption := sSubTitleTxt;
end;


procedure TFloatingForm.GetTextLines(aValue: string);
var
  i: integer;

begin
  iTextLines := 1; // default value
  // Get the textlines
  for i := 1 to Length(aValue)-1 do
    if aValue[i] = #13 then
      inc(iTextLines);
end;


procedure TFloatingForm.CreateParams(var Params: TCreateParams);
begin
  if not (csDesigning in ComponentState) then
    begin
      // Set the form click-through
      Params.ExStyle := WS_EX_LAYERED or WS_EX_TRANSPARENT;
    end;
  inherited CreateParams(Params);
end;


// Sets the font of the window (the label Parentfont has been set to True)
procedure TFloatingForm.SetFont(aValue: TFont);
begin
  Font := fLayerFont;
  lblSubTitle.Font := fLayerFont;
end;


procedure TFloatingForm.SetPreferredLanguage(aValue: string);
begin
  FPreferredLanguage := aValue;
  fTimedText.PreferredLanguage := aValue;
end;


procedure TFloatingForm.SetVideoSurfaceHandle(aValue: HWND);
begin
  hVideoSurface := aValue;
end;


function TFloatingForm.GetScreenResolution(): TScrRes;
var
  dc: THandle; // the Display Context

begin
  dc := GetDC(Handle);
  Result.ScrResH := GetDeviceCaps(dc,
                                  HORZRES);
  Result.ScrResV := GetDeviceCaps(dc,
                                  VERTRES);
  Result.LogPixelsX := GetDeviceCaps(dc,
                                     LOGPIXELSX);
  Result.LogPixelsY := GetDeviceCaps(dc,
                                     LOGPIXELSY);
  ReleaseDc(Handle, dc);
end;


// EVENT HANDLERS //////////////////////////////////////////////////////////////

procedure TFloatingForm.WMProgressEvent(var Msg: TMessage);
var
  I: Integer;
  txt: string;

begin

  if (MfPlayerX.Position = 0) and (Assigned(fTimedText)) then
    begin
      // Send a message we need the first track.
      SendMessage(fTimedText.Handle,
                  WM_TIMEDTEXTNOTIFY_INIT,
                  0,
                  0);
      Exit;
    end;


  // If a subtitle text is ready to be presented, show it.
  if bNewTrackAvailable then
    begin

      label1.Caption := 'Position : ' + IntToStr(MfPlayerX.Position) +
                        '   Start : ' + IntToStr(fTimedText.Track.Start) +
                        '   Stop : ' + IntToStr(fTimedText.Track.Stop);

      if (length(fTimedText.Track.TrackText) > 0) then
        label2.Caption := 'TrackID: ' + IntToStr(fTimedText.TrackIndex) +
                          '   First textline: ' + fTimedText.Track.TrackText[0].TextLine;


      // Keep the starting of a subtitle within a range, because the clock jitter makes the timer a bit inacurate.
      if InRange(fTimedText.Track.Start,
                 (MfPlayerX.Position - (fClockProperties.dwClockJitter div 1000)),
                 (MfPlayerX.Position + (fClockProperties.dwClockJitter div 1000))) then
        begin
          for I := 0 to Length(fTimedText.Track.TrackText) -1 do
            begin
              if (I > 0) then
                txt := txt + LFEED;
              txt := txt + fTimedText.Track.TrackText[I].TextLine;
              // Since we're using a single TLabel we can only handle one font.
              LayerFont := fTimedText.Track.TrackText[0].TextFont;
              LayerFont.Size := CalculatedFontSize;
              SubtitleText := txt;
            end;
        end;

      // Stop showing subtitle
      if (MfPlayerX.Position >= fTimedText.Track.Stop) then
        begin
          SubtitleText := '';
          bNewTrackAvailable := False;
          // Send a message we processed the track, so we are ready to get a new one.
          SendMessage(fTimedText.Handle,
                      WM_TIMEDTEXTNOTIFY_PROCESSED,
                      0,
                      0);
        end;
    end;

  // We reached the end of the trackarray or something went wrong while decoding
  // the array.
  if (Msg.LParam = -1) then
    SubtitleText := '';
end;


procedure TFloatingForm.WMTimedTextUpdateEvent(var Msg: TMessage);
begin
  bNewTrackAvailable := True;
end;


procedure TFloatingForm.WMParentSizeEvent(var Msg: TMessage);
begin
  if GetWindowRect(hVideoSurface,
                   VideoSurfaceRect) then
    begin
      VideoSurfaceRect.NormalizeRect;
      SetFormPosition();
    end;
end;


procedure TFloatingForm.WMParentPositionEvent(var Msg: TMessage);
begin
  if GetWindowRect(hVideoSurface,
                   VideoSurfaceRect) then
    begin
      VideoSurfaceRect.NormalizeRect;
      SetFormPosition();
      WMParentSizeEvent(Msg);
    end;
end;


procedure TFloatingForm.WMSizeEvent(var Msg: TMessage);
begin
  // Here the fontsize will be adjusted when the formsize is changing.
  if Visible then
    begin
     // Set the bounds of the label
     lblSubTitle.SetBounds(Left, Top, Width, Height);
     CalculateFontSize();
    end;
end;


// Prevents this window on top of all other windows
procedure TFloatingForm.WMSysCommand(var Msg: TWmSysCommand);
begin

  if ((Msg.CmdType and $FFF0) = SC_ICON) then
    begin
      Msg.Result := 0;
      EnableWindow(Handle,
                   True);
    end
  else
    inherited;
end;

// END EVENTHANDLERS ///////////////////////////////////////////////////////////


procedure TFloatingForm.CalculateFontSize();
var
  screenRes: TScrRes; // screen resolution
  formsize: TRect;
  xd, xf: Single;
  perc: Single;

begin
  // Note:
  // Realtime video measurement: aspectratio 2,37 : 1 (which is the cinema format)
  // Calculate the fontsize as a percentage of the videosurface (aspectratio 16:9)
  // If the videosurfacesize is (H, W) 0,0 the percentage = 0
  // If the videosurfacesize is fullscreen the percentage is 100 or some less.
  // So, first thing to do is calculate the maximum fullscreen size
  // For example: screensize = 1680 x 1050
  //              aspectratio = 16:9
  //              videosurfacesize = (1680 x 945), 100%, best readable fontsize will be approx int(40)
  //              videosurfacesize = (840 x 473) = 50%, best readable fontsize will be approx int(20)
  //              videosurfacesize = (420 x 236) = 25%, best readable fontsize will be approx int(9)

  // Get current screensize
  screenRes := GetScreenResolution();
  // Get the form size
  GetWindowRect(Self.Handle,
                formsize);

  xd := (formsize.Width / screenRes.ScrResH) * 100;
  xf := (formsize.Height / screenRes.ScrResV) * 100;
  // Now this a very simple calculation
  perc := (xf + xd) / 2;
  // a better aproach would be calculating the diagonal size of the screen,
  // with this formula:
  // a2 x b2 = c2 (Pythgorean Theorem)
  // So: let's say we have a width of 1680
  //     and a height of 1050,
  //     then the diagonal will be: x2(1680) X x2(1050) = sqrt(3924900) = 1981
  //

  if (perc >= 100) then
    CalculatedFontSize := 38
  else if InRange(perc, 90, 100) then
    CalculatedFontSize := 36
  else if InRange(perc, 80, 90) then
    CalculatedFontSize := 30
  else if InRange(perc, 70, 80) then
    CalculatedFontSize := 24
  else if InRange(perc, 60, 70) then
    CalculatedFontSize := 22
  else if InRange(perc, 50, 60) then
    CalculatedFontSize := 20
  else if InRange(perc, 40, 50) then
    CalculatedFontSize := 16
  else if InRange(perc, 30, 40) then
    CalculatedFontSize := 14
  else if InRange(perc, 20, 30) then
    CalculatedFontSize := 12
  else if InRange(perc, 0, 20) then
    CalculatedFontSize := 10;

  lblSubTitle.Font.Size := CalculatedFontSize;

end;


function TFloatingForm.OpenTimedTextFile(aValue: WideString): HResult;
var
  hr: HResult;

begin
  // input checks will be done in OpenTimedTextFile function

  // Load the TimedText file
  hr := fTimedText.OpenTimedTextFile(aValue);
  if SUCCEEDED(hr) then
    begin
      fTimedTextFile := fTimedText.TimedTextFile;
    end;

  Result := hr;
end;

end.
