unit frmVolumeControl;

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
  MfAudioEndPoint,
  WinApi.CoreAudioApi.EndPointVolume, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TVolumeControl = class(TForm)
    MfAudioEndPoint1: TMfAudioEndPoint;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    tbDbVolume: TTrackBar;
    tbScVolume: TTrackBar;
    butClose: TButton;
    procedure MfAudioEndPoint1Notify(Sender: TObject;
      pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA);
    procedure tbScVolumeChange(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure tbDbVolumeChange(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VolumeControl: TVolumeControl;

implementation

{$R *.dfm}

procedure TVolumeControl.butCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TVolumeControl.CheckBox1Click(Sender: TObject);
begin
  MfAudioEndPoint1.Mute := BOOL(CheckBox1.Checked);
end;


procedure TVolumeControl.MfAudioEndPoint1Notify(Sender: TObject;
  pNotify: PAUDIO_VOLUME_NOTIFICATION_DATA);
begin
 // Bogus, just for checking the pointer values.
 if (pNotify = Nil) then
   Exit;

end;


procedure TVolumeControl.tbScVolumeChange(Sender: TObject);
var
  sVal: Single;

begin
  sVal := tbScVolume.Position / 10;
  MfAudioEndPoint1.MasterScalarVolume := sVal;
  Edit2.Text := sVal.ToString();

end;


procedure TVolumeControl.tbDbVolumeChange(Sender: TObject);
var
  sVal: Single;

begin
  sVal := tbDbVolume.Position / 10;
  MfAudioEndPoint1.MasterDBVolume := sVal;
  Edit1.Text := sVal.ToString();
end;

end.
