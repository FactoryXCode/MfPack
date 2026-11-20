unit frmAdvanced;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TFfrmAdvanced = class(TForm)
    cbxDisableHardwareTransforms: TCheckBox;
    cbxDisableThrottling: TCheckBox;
    Button3: TButton;
    cbxDisableGOPSize: TCheckBox;
    cbxDisableQualityBasedEncoding: TCheckBox;
    Label13: TLabel;
    spedThreadLimit: TSpinEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FfrmAdvanced: TFfrmAdvanced;

implementation

{$R *.dfm}

end.
