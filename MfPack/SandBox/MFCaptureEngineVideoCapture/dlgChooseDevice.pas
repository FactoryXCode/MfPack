unit dlgChooseDevice;

interface

uses
  {WinApi}
  Winapi.Windows,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.Grids,
  {MediaFoundation}
  WinApi.MediaFoundationApi.MfMetLib,
  WinAPI.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError,
  {Application}
  CaptureEngine,
  DeviceExplorer,
  Utils;

type
  TChooseDeviceDlg = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lbxDeviceList: TListBox;
    Label1: TLabel;
    sgResolutions: TStringGrid;
    cbxSupportedFormatsOnly: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbxDeviceListClick(Sender: TObject);
    procedure sgResolutionsClick(Sender: TObject);
    procedure cbxSupportedFormatsOnlyClick(Sender: TObject);
  private
    { Private declarations }

    iSelectedDevice: Integer;
    iSelectedFormat: Integer;

    function Populate(bSupportedFormatsOnly: Boolean): HResult;

  public
    { Public declarations }

    property SelectedDevice: LongInt read iSelectedDevice;
    property SelectedFormat: LongInt read iSelectedFormat;

  end;

var
  ChooseDeviceDlg: TChooseDeviceDlg;

implementation

{$R *.dfm}


// btnCancelClick
procedure TChooseDeviceDlg.btnCancelClick(Sender: TObject);
begin
  Close();
end;


// btnOKClick
procedure TChooseDeviceDlg.btnOKClick(Sender: TObject);
begin
  if (lbxDeviceList.ItemIndex >= 0) and (sgResolutions.Row > 0) then
    begin

      iSelectedDevice := lbxDeviceList.ItemIndex;
      iSelectedFormat := StrToInt(sgResolutions.Cells[4, sgResolutions.Row]);

      if (iSelectedDevice > -1) and (iSelectedFormat > -1) then
        begin
          ModalResult := 1212;
        end
      else
        begin

{$IFDEF DEBUG}
          OutputDebugString(StrToPWideChar(format('Error: %s (hr = %d)',
                                                  [ERR_SET_DEVICE,
                                                   E_FAIL])));
{$ENDIF}
          ModalResult := 0;
        end;
    end
  else
    begin
      MessageBox(Handle,
                 PWideChar('Please select a device and video format.'),
                 PWideChar('Selection incomplete.'),
                 MB_ICONSTOP);
    end;
end;


procedure TChooseDeviceDlg.cbxSupportedFormatsOnlyClick(Sender: TObject);
begin
  Populate(cbxSupportedFormatsOnly.Checked);
end;

// FormCreate
procedure TChooseDeviceDlg.FormCreate(Sender: TObject);
begin
  iSelectedDevice := -1;
  iSelectedFormat := -1;
  populate(True);
end;


procedure TChooseDeviceDlg.lbxDeviceListClick(Sender: TObject);
begin
  btnOK.Enabled := ((lbxDeviceList.ItemIndex > -1) and (sgResolutions.Row > 0));
end;

// Populate the listboxes with camera's and properties found on this system
// ========================================================================
function TChooseDeviceDlg.Populate(bSupportedFormatsOnly: Boolean): HResult;
   // Helper
   procedure AddFormat(iCol: Integer; iDev: Integer; iForm: Integer);
     begin
       {Width and Height}
       sgResolutions.Cells[0, iCol] := Format('%d x %d',
                                              [FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].iVideoWidth,
                                               FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].iVideoHeight]);
       {Framerate}
       sgResolutions.Cells[1, iCol] := Format('%n',
                                              [GetFrameRateFromRatio(FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].iFrameRate,
                                               FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].iFrameRateDenominator)]);
       {Subtype}
       sgResolutions.Cells[2, iCol] := Format('%s',
                                              [GetGUIDNameConst(FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].fSubType)]);

       {Supported by MF}
       sgResolutions.Cells[3, iCol] := Format('%s',
                                              [BoolToStrYesNo(FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].bMFSupported)]);
       {Index}
       sgResolutions.Cells[4, iCol] := Format('%d',
                                              [FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].FormatsIndex]);
     end;

var
  i,
  j,
  rc: Integer;
  hr: HResult;

label
  Done;

begin
  hr := S_OK;
try
try

  if (FDeviceExplorer.DevicesCount = 0) then
    begin
      hr :=  MF_E_NO_CAPTURE_DEVICES_AVAILABLE;
      Exit;
    end;

  lbxDeviceList.Clear;

  sgResolutions.ColCount := 5;
  sgResolutions.RowCount := 1;
  sgResolutions.Cells[0, 0] := 'Height x Width';
  sgResolutions.Cells[1, 0] := 'FPS';
  sgResolutions.Cells[2, 0] := 'Video format';
  sgResolutions.Cells[3, 0] := 'Supported';
  sgResolutions.Cells[4, 0] := 'Formats index';  // This a hidden column.
  rc := 1;

   // Fill the combobox with found capture devices
  for i := 0 to FDeviceExplorer.DevicesCount - 1 do
    begin
      // Append the friendly name to the combobox.
      lbxDeviceList.Items.Append(FDeviceExplorer.DeviceProperties[i].lpDisplayName);

      // List devicecapabilities.

      {$IFDEF ConditionalExpressions}
        {$IF CompilerVersion > 31.0}
          sgResolutions.BeginUpdate();
        {$IFEND}
      {$ENDIF}

      for j := 1 to FDeviceExplorer.NativeFormats -1 do
        begin
          if bSupportedFormatsOnly then
            begin
              if FDeviceExplorer.DeviceProperties[i].aVideoFormats[j].bMFSupported then
                begin
                  AddFormat(rc, i, j);
                  Inc(rc);
                  sgResolutions.RowCount := rc;
                end;
            end
          else   // all
            begin
              AddFormat(rc, i, j);
              Inc(rc);
              sgResolutions.RowCount := rc;
            end;
        end;

      {$IFDEF ConditionalExpressions}
        {$IF CompilerVersion > 31.0}
           sgResolutions.EndUpdate();
        {$IFEND}
      {$ENDIF}
    end;

  // Select the first in the devices list
  lbxDeviceList.ItemIndex := 0;

except
  hr := E_FAIL;
end;

finally
  Result := hr;
end;
end;

procedure TChooseDeviceDlg.sgResolutionsClick(Sender: TObject);
begin
  lbxDeviceListClick(Self);
end;

end.

