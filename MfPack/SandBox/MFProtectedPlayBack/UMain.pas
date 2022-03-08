unit UMain;

interface

uses
  {System}
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  {FMX}
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.MediaFoundation,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts;

type
  TfrmMain = class(TForm)
    layVideo: TScaledLayout;
    layEPG: TLayout;
    rectEPGBackground: TRectangle;
    layEPGCenter: TLayout;
    lblEPGChannel: TLabel;
    lblEPGNow: TLabel;
    lblEPGNext: TLabel;
    layEPGRight: TLayout;
    lblToday: TLabel;
    lblChannel: TLabel;
    Line1: TLine;
    Line2: TLine;
    tmrUpdate: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    FPlayer: TMediaEngine;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Fill.Color := TAlphaColorRec.Black;
  Fill.Kind := TBrushKind.Solid;

  FPlayer := TMediaEngine.create(Self);
  FPlayer.Parent := Self;
  FPlayer.Align := TAlignLayout.Client;
  FPlayer.PlayAfterLoad := True;
  FPlayer.Filename := 'https://test-videos.co.uk/vids/bigbuckbunny/mp4/h264/1080/Big_Buck_Bunny_1080_10s_30MB.mp4';

  FPlayer.AddObject(layVideo);
end;


procedure TfrmMain.tmrUpdateTimer(Sender: TObject);
begin
  lblToday.Text := FormatDateTime('dd.mm.yyyy hh:nn', Now);
end;

end.
