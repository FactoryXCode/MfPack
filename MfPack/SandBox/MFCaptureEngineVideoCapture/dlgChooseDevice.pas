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
      PostMessage(g_pEngine.PreviewHandle,
                  WM_CHOOSE_DEVICEDLG_ITEMINDEX,
                  WPARAM(LPWSTR(lbxDeviceList.Items[lbxDeviceList.ItemIndex])),
                  LPARAM(lbxDeviceList.ItemIndex));
      ModalResult := 1212;
    end
  else
    btnCancelClick(Self);

end;

end.
