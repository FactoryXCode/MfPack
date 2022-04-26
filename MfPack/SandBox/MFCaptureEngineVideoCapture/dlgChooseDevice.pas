unit dlgChooseDevice;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Menus,
  CaptureEngine;

type
  TChooseDeviceDlg = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lbxDeviceList: TListBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChooseDeviceDlg: TChooseDeviceDlg;

implementation

{$R *.dfm}

procedure TChooseDeviceDlg.btnCancelClick(Sender: TObject);
begin
  Close();
end;


procedure TChooseDeviceDlg.btnOKClick(Sender: TObject);
begin
  if lbxDeviceList.ItemIndex >= 0 then
    begin
      // Set DeviceParam properties
      DeviceParam.DeviceIndex := lbxDeviceList.ItemIndex;
      DeviceParam.DeviceIsSelected := True;
      ModalResult := 1212;
    end;
end;

end.
