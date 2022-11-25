unit frmMain;

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
  Vcl.StdCtrls,
  SinkWriterClass;


type
  TMainForm = class(TForm)
    butExecute: TButton;
    lblInfo: TLabel;
    procedure butExecuteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}


procedure TMainForm.butExecuteClick(Sender: TObject);
begin
  FSampleSinkWriter := TSampleSinkWriter.Create();
  if SUCCEEDED(FSampleSinkWriter.RunSinkWriter()) then
    lblInfo.Caption := 'File is succesfully created!'
  else
    lblInfo.Caption := 'Oops, something went wrong :-('
end;





end.
