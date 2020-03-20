// Copyright: © FactoryX, Netherlands/Australia/Brazil. All rights reserved.
// This was the first MFplayer written and contributed to MfPack-Samples by Ramysés De Macedo Rodrigues (Brazil)
// Date of birth: 2014/06/18

unit fMFPlayer;

interface

uses
  {WinApi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.Variants,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  {Application}
  fMFPlayercore;

type
  TF_MFPlayer = class(TForm)
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    Qrquivo1: TMenuItem;
    Abrir1: TMenuItem;
    Sair1: TMenuItem;
    Sair2: TMenuItem;
    procedure Timer1Timer(Sender: TObject);
    procedure Abrir1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Sair2Click(Sender: TObject);

  private
    { Private declarations }
    FPlayer: TMFPlayer;
    procedure WMResize(var msg: TMessage); message WM_SIZE;
    procedure WMPAINT(var msg: TMessage); message WM_PAINT;
    procedure KEYDOWN (var msg: TMessage); message WM_KEYDOWN;
    procedure UpdateGUI;

  public
    { Public declarations }

    function GetCorePlayer: TMFPlayer;
  end;

var
  F_MFPlayer: TF_MFPlayer;

implementation

{$R *.dfm}

uses
  Vcl.ClipBrd,
  MfPack.MfIDL;

procedure TF_MFPlayer.Timer1Timer(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TF_MFPlayer.TrackBar1Change(Sender: TObject);
var
   dur: Cardinal;
begin
//  if Assigned(Player) then
//     begin
//     Player.GetDuration(dur);
//     Player.SetPosition(trunc(Cardinal(TrackBar1.Position) * dur));
//     end;
end;

procedure TF_MFPlayer.Abrir1Click(Sender: TObject);
begin
  if Opendialog1.execute then
            begin
//            if Assigned(FPlayer) then
//               FPlayer.ShutDown;
//               FreeAndNil(FPlayer);

               FPlayer.OpenURL(Opendialog1.Filename);
           end;
end;

procedure TF_MFPlayer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FPlayer) then
     FreeAndNil(FPlayer);
end;

procedure TF_MFPlayer.FormCreate(Sender: TObject);
begin
  FPlayer := TMFPlayer.Create(Self.Handle, Self.Handle);
end;

function TF_MFPlayer.GetCorePlayer: TMFPlayer;
begin
  Result := FPlayer;
end;

procedure TF_MFPlayer.KEYDOWN(var msg: TMessage);
var
   pos: Cardinal;
   bit: TBitmap;
begin
 case Msg.WParam of
 VK_SPACE: if Assigned(FPlayer) then
           case FPlayer.GetState of
               CmdStart: FPlayer.SendPlayerComand(CmdPause);
               CmdStop, CmdPause: FPlayer.SendPlayerComand(CmdStart);
           end;
 VK_ESCAPE: if Assigned(FPlayer) then FPlayer.SendPlayerComand(CmdStop);
 VK_F12: begin
         Abrir1Click(nil);
         end;
 VK_PRIOR: if Assigned(FPlayer) then
              begin
              FPlayer.StepDown(3000);
              end;
  VK_NEXT: if Assigned(FPlayer) then
              begin
              FPlayer.StepUp(3000);
              end;
  VK_F8:  begin
           bit := TBitmap.Create;
           FPlayer.GetCurrentFrame(bit);
           Clipboard.Assign(bit);  // copia para área de transferênci
           FreeAndNIl(bit);//.FreeImage; // libera o objeto criado dentro do método GetCurrentFrame
           end;
  VK_F5:   if Assigned(FPlayer) then
              FPlayer.SetRate(FPlayer.GetRate + 0.5);

  VK_F6:   if Assigned(FPlayer) then
              FPlayer.SetRate(FPlayer.GetRate - 0.5);

  VK_SUBTRACT: begin
               FPlayer.Volume := FPlayer.Volume - 0.1;
               end;
  VK_ADD:   begin
            FPlayer.Volume := FPlayer.Volume + 0.1;
            end;
  VK_F1: begin
         if Assigned(FPlayer) then FPlayer.SetPosition(0);
         end;
 end;
end;

procedure TF_MFPlayer.Sair2Click(Sender: TObject);
begin
  Close;
end;

procedure TF_MFPlayer.UpdateGUI;
begin
//

end;

procedure TF_MFPlayer.WMPAINT(var msg: TMessage);
var
   ps: PAINTSTRUCT;
   rc: TRECT;
   hHDc: HDC;
begin
        hHDc := BeginPaint(Self.Handle, ps);
        rc := Self.ClientRect;

        if Assigned(FPlayer) then
           FPlayer.Repaint
        else  // The video is not playing, so we must paint the application window.
           FillRect(hHDc, rc, HBRUSH(COLOR_WINDOW));

        EndPaint(Self.Handle, ps);
end;

procedure TF_MFPlayer.WMResize(var msg: TMessage);
begin
  if Assigned(FPlayer) then
     FPlayer.ResizeVideo(Self.ClientWidth, Self.ClientHeight);
end;

end.
