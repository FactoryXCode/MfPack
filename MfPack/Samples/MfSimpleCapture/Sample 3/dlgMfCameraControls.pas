// FactoryX
//
// Project: MfPack - MfSimpleCapture Sample 3
// Module: dlgMfCameraControls.pas
// Description: Live MF camera controls, notifications and per-user defaults.
// License: Mozilla Public License 2.0, https://mozilla.org/MPL/2.0/

unit dlgMfCameraControls;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  MfCameraControls;

type

  TMfCameraControlsDialog = class(TForm)
    Pages: TPageControl;
    tsLive: TTabSheet;
    tsExtended: TTabSheet;
    tsDefaults: TTabSheet;
    cbxStorage: TComboBox;
    lblStorage: TLabel;
    btnApplyIni: TButton;
    lstControls: TListBox;
    pnlValue: TPanel;
    lblName: TLabel;
    lblCurrent: TLabel;
    lblRange: TLabel;
    lblCapabilities: TLabel;
    lblValue: TLabel;
    edValue: TEdit;
    chkAuto: TCheckBox;
    btnApply: TButton;
    btnReset: TButton;
    btnRefresh: TButton;
    lblResult: TLabel;
    lblExtended: TLabel;
    cbxExtended: TComboBox;
    lblExtendedInfo: TLabel;
    cbxMode: TComboBox;
    btnApplyExtended: TButton;
    lblExtendedNote: TLabel;
    lblDefaults: TLabel;
    lstDefaults: TListBox;
    btnReadDefaults: TButton;
    btnSaveDefault: TButton;
    btnRemoveDefault: TButton;
    lblDefaultsStatus: TLabel;
    pnlStatus: TPanel;
    lblMonitor: TLabel;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject;
                        var Action: TCloseAction);
    procedure SelectionChanged(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
    procedure ApplyClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure ExtendedChanged(Sender: TObject);
    procedure ApplyExtendedClick(Sender: TObject);
    procedure StorageChanged(Sender: TObject);
    procedure ApplyIniClick(Sender: TObject);
    procedure ReadDefaultsClick(Sender: TObject);
    procedure SaveDefaultClick(Sender: TObject);
    procedure RemoveDefaultClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);

  private
    FControls: TMfCameraControls;
    FLoadingStorage: Boolean;
    FStorageIndex: Integer;
    FNotifyWindow: HWND;
    FModeValues: array of UInt64;

    function UsingIni(): Boolean;
    procedure NotifyWindowProc(var Message: TMessage);
    procedure UpdateSelected();
    procedure UpdateExtended();
    procedure Report(const AAction: string;
                     const AStatus: HRESULT);
    procedure ReportDefaultAction(const AAction: string;
                                  const AStatus: HRESULT);
    procedure AddMode(const ACaption: string;
                      const AFlags: UInt64);

  public
    procedure Bind(const AControls: TMfCameraControls);
    procedure RefreshView();
  end;

implementation

uses
  MfCameraSettings,
  WinApi.WinError,
  WinApi.KsMedia;

{$R *.dfm}

procedure TMfCameraControlsDialog.FormCreate(Sender: TObject);

begin

  FNotifyWindow := AllocateHWnd(NotifyWindowProc);
  FLoadingStorage := True;
  try
    FStorageIndex := 0;
    try
      if not TMfCameraSettings.UsesIni(TMfCameraSettings.FileName()) then
        FStorageIndex := 1;
    except
      on E: Exception do FStorageIndex := 0;
    end;
    cbxStorage.ItemIndex := FStorageIndex;
  finally
    FLoadingStorage := False;
  end;
  Bind(nil);
end;


procedure TMfCameraControlsDialog.FormDestroy(Sender: TObject);

begin

  Bind(nil);
  DeallocateHWnd(FNotifyWindow);
  FNotifyWindow := 0;
end;


procedure TMfCameraControlsDialog.FormClose(Sender: TObject;
                                            var Action: TCloseAction);

begin

  Bind(nil);
  Action := caHide;
end;


procedure TMfCameraControlsDialog.Bind(const AControls: TMfCameraControls);
var
  Status: HRESULT;

begin

  if Assigned(FControls) then
    FControls.StopMonitor();
  FControls := AControls;
  lblResult.Caption := '';
  if Assigned(FControls) then
    begin
      Status := FControls.StartMonitor(FNotifyWindow);
      if SUCCEEDED(Status) then
        lblMonitor.Caption := 'Camera change notifications are active.'
      else
        lblMonitor.Caption := Format('Notifications unavailable ($%.8x). Use Refresh.',
                                     [Cardinal(Status)]);
    end
  else
    lblMonitor.Caption := 'Open a camera to inspect its controls.';
  RefreshView();
  ReadDefaultsClick(nil);
  if Assigned(FControls) and UsingIni() and FAILED(FControls.IniRestoreStatus) then
    Report('Restore INI settings (some controls failed)', FControls.IniRestoreStatus);
end;


procedure TMfCameraControlsDialog.NotifyWindowProc(var Message: TMessage);

begin

  if (Message.Msg = WM_MF_CAMERA_CONTROL_CHANGED) then
    begin
      if Assigned(FControls) and (Message.WParam = FControls.Generation) then
        begin
          if (HRESULT(Message.LParam) < 0) then
            lblMonitor.Caption := Format('Notification monitor error: $%.8x',
                                         [Cardinal(Message.LParam)])
          else
            begin
              FControls.Refresh();
              UpdateSelected();
              UpdateExtended();
            end;
        end;
      Message.Result := 0;
    end
  else
    Message.Result := DefWindowProc(FNotifyWindow,
                                    Message.Msg,
                                    Message.WParam,
                                    Message.LParam);
end;


procedure TMfCameraControlsDialog.RefreshView();
var
  I: Integer;
  Index: Integer;
  Name: string;
  Info: TMfCameraControlInfo;
  Ext: TMfExtendedCameraInfo;

begin

  Index := lstControls.ItemIndex;
  lstControls.Items.BeginUpdate();
  try
    lstControls.Clear();
    if Assigned(FControls) then
      for I := 0 to MF_CAMERA_STANDARD_COUNT - 1 do
        begin
          Info := FControls.Standard[I];
          Name := Info.Name;
          if not Info.Supported then
            Name := Name + ' (unavailable)';
          lstControls.Items.Add(Name);
        end;
    if (Index >= 0) and (Index < lstControls.Count) then
      lstControls.ItemIndex := Index
    else
      if (lstControls.Count > 7) then
        lstControls.ItemIndex := 7;
  finally
    lstControls.Items.EndUpdate();
  end;
  Index := cbxExtended.ItemIndex;
  cbxExtended.Items.Clear();
  if Assigned(FControls) then
    for I := 0 to MF_CAMERA_EXTENDED_COUNT - 1 do
      begin
        Ext := FControls.Extended[I];
        Name := Ext.Name;
        if not Ext.Supported then
          Name := Name + ' (unavailable)';
        cbxExtended.Items.Add(Name);
      end;
  if (Index >= 0) and (Index < cbxExtended.Items.Count) then
    cbxExtended.ItemIndex := Index
  else
    if (cbxExtended.Items.Count > 0) then
      cbxExtended.ItemIndex := 0;
  UpdateSelected();
  UpdateExtended();
end;


procedure TMfCameraControlsDialog.UpdateSelected();
var
  Info: TMfCameraControlInfo;
  Selected: Boolean;

begin

  Selected := Assigned(FControls) and (lstControls.ItemIndex >= 0);
  btnApply.Enabled := False;
  btnReset.Enabled := False;
  edValue.Enabled := False;
  chkAuto.Enabled := False;
  btnSaveDefault.Enabled := False;
  btnRemoveDefault.Enabled := Selected and UsingIni();
  btnApplyIni.Enabled := Assigned(FControls) and UsingIni();
  if UsingIni() then
    begin
      btnReadDefaults.Caption := 'Read INI settings';
      btnSaveDefault.Caption := 'Save selected live control to INI';
      btnRemoveDefault.Caption := 'Remove selected INI setting';
      lblDefaults.Caption := 'INI settings are stored per camera for this sample. ' +
        'They are restored on camera open when INI storage is selected. Use Apply saved INI to restore them now.';
    end
  else
    begin
      btnReadDefaults.Caption := 'Reload saved defaults';
      btnSaveDefault.Caption := 'Save selected live control as default';
      btnRemoveDefault.Caption := 'Remove selected saved default';
      lblDefaults.Caption := 'Windows default writes require a registered camera companion app.';
    end;
  lblName.Caption := 'Select a live control';
  lblCurrent.Caption := '';
  lblRange.Caption := '';
  lblCapabilities.Caption := '';
  if not Selected then
    Exit;
  Info := FControls.Standard[lstControls.ItemIndex];
  lblName.Caption := Info.Name;
  if not Info.Supported then
    begin
      lblCurrent.Caption := Format('Not supported by this camera ($%.8x).',
                                   [Cardinal(Info.Status)]);
      Exit;
    end;
  lblCurrent.Caption := Format('Current: %d   Flags: $%.8x',
                               [Info.Value, Info.Flags]);
  if Info.RangeKnown then
    lblRange.Caption := Format('Minimum: %d   Maximum: %d   Step: %d',
                               [Info.Minimum, Info.Maximum, Info.Step])
  else
    lblRange.Caption := 'The driver did not report a usable numeric range.';
  lblCapabilities.Caption := Format('Manual: %s   Auto: %s',
                                    [BoolToStr((Info.Capabilities and KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL) <> 0,
                                               True),
       BoolToStr((Info.Capabilities and KSPROPERTY_CAMERACONTROL_FLAGS_AUTO) <> 0,
                 True)]);
  edValue.Text := IntToStr(Info.Value);
  edValue.Enabled := Info.RangeKnown and ((Info.Capabilities and KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL) <> 0);
  chkAuto.Checked := ((Info.Flags and KSPROPERTY_CAMERACONTROL_FLAGS_AUTO) <> 0);
  chkAuto.Enabled := ((Info.Capabilities and KSPROPERTY_CAMERACONTROL_FLAGS_AUTO) <> 0);
  btnApply.Enabled := edValue.Enabled or chkAuto.Enabled;
  btnReset.Enabled := Info.DefaultKnown and edValue.Enabled;
  // API availability does not guarantee permission to persist Windows defaults.
  btnSaveDefault.Enabled := UsingIni() or SUCCEEDED(FControls.DefaultsStatus);
  btnRemoveDefault.Enabled := btnSaveDefault.Enabled;
  if not UsingIni() then
    lblDefaults.Caption := 'Selected live control: ' + Info.Name +
      '. Windows default writes require a registered camera companion app. ' +
      'Use Windows Settings > Bluetooth & devices > Cameras for this desktop sample.';
end;


procedure TMfCameraControlsDialog.Report(const AAction: string;
                                         const AStatus: HRESULT);

begin

  if SUCCEEDED(AStatus) then
    lblResult.Caption := AAction + ' succeeded.'
  else
    lblResult.Caption := Format('%s failed ($%.8x).',
                                [AAction, Cardinal(AStatus)]);
end;


procedure TMfCameraControlsDialog.ReportDefaultAction(const AAction: string;
                                                      const AStatus: HRESULT);

begin

  // Reload first so the result of the write remains visible on the defaults tab.
  ReadDefaultsClick(nil);
  Report(AAction,
         AStatus);
  if (AStatus = E_ACCESSDENIED) and not UsingIni() then
    lblDefaultsStatus.Caption := 'Access denied ($80070005). Windows reserves default writes ' +
      'for a registered camera companion app. Use Windows Settings > Bluetooth & devices > Cameras.';
end;


procedure TMfCameraControlsDialog.SelectionChanged(Sender: TObject);

begin

  UpdateSelected();
end;


procedure TMfCameraControlsDialog.RefreshClick(Sender: TObject);

begin

  if Assigned(FControls) then
    FControls.Refresh();
  RefreshView();
end;


procedure TMfCameraControlsDialog.ApplyClick(Sender: TObject);
var
  Value: Integer;
  Flags: ULONG;

begin

  if not Assigned(FControls) or (lstControls.ItemIndex < 0) then
    Exit;
  if chkAuto.Checked then
    begin
      Value := FControls.Standard[lstControls.ItemIndex].Value;
      Flags := KSPROPERTY_CAMERACONTROL_FLAGS_AUTO;
    end
  else
    begin
      if not TryStrToInt(edValue.Text, Value) then
        begin
          lblResult.Caption := 'Enter an integer in the device range, using its step.';
          Exit;
        end;
      Flags := KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL;
    end;
  Report('Apply live control',
         FControls.ApplyStandard(lstControls.ItemIndex,
                                 Value,
                                 Flags));
  UpdateSelected();
end;


procedure TMfCameraControlsDialog.ResetClick(Sender: TObject);
var
  Info: TMfCameraControlInfo;

begin

  if not Assigned(FControls) or (lstControls.ItemIndex < 0) then
    Exit;
  Info := FControls.Standard[lstControls.ItemIndex];
  Report('Apply driver default',
         FControls.ApplyStandard(lstControls.ItemIndex,
                                 Info.DefaultValue,
                                 KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL));
  UpdateSelected();
end;


procedure TMfCameraControlsDialog.AddMode(const ACaption: string;
                                          const AFlags: UInt64);
var
  Info: TMfExtendedCameraInfo;
  Index: Integer;

begin

  Info := FControls.Extended[cbxExtended.ItemIndex];
  if ((Info.Capabilities and AFlags) <> AFlags) then
    Exit;
  Index := Length(FModeValues);
  SetLength(FModeValues,
            Index + 1);
  FModeValues[Index] := AFlags;
  cbxMode.Items.Add(ACaption);
  if (Info.Flags = AFlags) then
    cbxMode.ItemIndex := Index;
end;


procedure TMfCameraControlsDialog.UpdateExtended();
var
  Info: TMfExtendedCameraInfo;

begin

  cbxMode.Items.Clear();
  SetLength(FModeValues,
            0);
  btnApplyExtended.Enabled := False;
  lblExtendedInfo.Caption := '';
  lblExtendedNote.Caption := 'Availability depends on Windows and the camera driver.';
  if not Assigned(FControls) or (cbxExtended.ItemIndex < 0) then
    Exit;
  Info := FControls.Extended[cbxExtended.ItemIndex];
  if not Info.Supported then
    begin
      lblExtendedInfo.Caption := Format('Not available ($%.8x).',
                                        [Cardinal(Info.Status)]);
      Exit;
    end;
  lblExtendedInfo.Caption := 'Flags: $' + IntToHex(Info.Flags,
                                                   16) +
                             '   Capabilities: $' + IntToHex(Info.Capabilities,
                                                             16);
  AddMode('Off',
          0);
  if (cbxExtended.ItemIndex = 3) then
    begin
      AddMode('Background blur',
              KSCAMERA_EXTENDEDPROP_BACKGROUNDSEGMENTATION_BLUR);
      AddMode('Background mask',
              KSCAMERA_EXTENDEDPROP_BACKGROUNDSEGMENTATION_MASK);
    end
  else
    begin
      AddMode('On',
              1);
      if (cbxExtended.ItemIndex = 2) then
        AddMode('Stare correction',
                KSCAMERA_EXTENDEDPROP_EYEGAZECORRECTION_STARE)
      else
        AddMode('Auto',
                2);
    end;
  btnApplyExtended.Enabled := True;
  if (cbxExtended.ItemIndex = 0) and FControls.HasStreamed then
    begin
      btnApplyExtended.Enabled := False;
      lblExtendedNote.Caption := 'HDR is a pre-start control. Reopen with automatic preview unchecked, set HDR, then start preview.';
    end;
end;


procedure TMfCameraControlsDialog.ExtendedChanged(Sender: TObject);

begin

  UpdateExtended();
end;


procedure TMfCameraControlsDialog.ApplyExtendedClick(Sender: TObject);

begin

  if not Assigned(FControls) or (cbxExtended.ItemIndex < 0) or (cbxMode.ItemIndex < 0) then
    Exit;
  Report('Apply extended control',
         FControls.ApplyExtended(cbxExtended.ItemIndex,
                                 FModeValues[cbxMode.ItemIndex]));
  UpdateExtended();
end;


function TMfCameraControlsDialog.UsingIni(): Boolean;
begin
  Result := cbxStorage.ItemIndex = 0;
end;


procedure TMfCameraControlsDialog.StorageChanged(Sender: TObject);
var
  Status: HRESULT;
begin
  if FLoadingStorage then Exit;
  try
    Status := TMfCameraSettings.SetUsesIni(TMfCameraSettings.FileName(), UsingIni());
  except
    on E: Exception do Status := E_FAIL;
  end;
  if FAILED(Status) then
    begin
      FLoadingStorage := True;
      try
        cbxStorage.ItemIndex := FStorageIndex;
      finally
        FLoadingStorage := False;
      end;
      Report('Save storage choice', Status);
    end
  else
    FStorageIndex := cbxStorage.ItemIndex;
  UpdateSelected();
  ReadDefaultsClick(nil);
end;


procedure TMfCameraControlsDialog.ApplyIniClick(Sender: TObject);
begin
  if not Assigned(FControls) or not UsingIni() then Exit;
  ReportDefaultAction('Apply saved INI settings', FControls.ApplyIniSettings());
  UpdateSelected();
end;


procedure TMfCameraControlsDialog.ReadDefaultsClick(Sender: TObject);
var
  Status: HRESULT;

begin

  lstDefaults.Items.Clear();
  lblDefaultsStatus.Hint := '';
  lblDefaultsStatus.ShowHint := False;
  btnReadDefaults.Enabled := Assigned(FControls);
  if not Assigned(FControls) then
    begin
      lblDefaultsStatus.Caption := 'Open a camera first.';
      Exit;
    end;
  if UsingIni() then
    begin
      Status := FControls.DescribeIniSettings(lstDefaults.Items);
      if SUCCEEDED(Status) then
        lblDefaultsStatus.Caption := Format('%d INI settings. File: %s',
          [lstDefaults.Items.Count, TMfCameraSettings.FileName()])
      else
        lblDefaultsStatus.Caption := Format('INI read failed ($%.8x). File: %s',
          [Cardinal(Status), TMfCameraSettings.FileName()]);
      lblDefaultsStatus.Hint := lblDefaultsStatus.Caption;
      lblDefaultsStatus.ShowHint := True;
      Exit;
    end;
  Status := FControls.DescribeDefaults(lstDefaults.Items);
  if SUCCEEDED(Status) then
    lblDefaultsStatus.Caption := Format('%d saved camera defaults.',
                                        [lstDefaults.Items.Count])
  else
    lblDefaultsStatus.Caption := Format('Saved defaults unavailable ($%.8x). Requires Windows 11 build 22621 or later.',
                                        [Cardinal(Status)]);
end;


procedure TMfCameraControlsDialog.SaveDefaultClick(Sender: TObject);

begin

  if not Assigned(FControls) or (lstControls.ItemIndex < 0) then
    Exit;
  if UsingIni() then
    begin
      ReportDefaultAction('Save selected INI setting',
                          FControls.SaveIniSetting(lstControls.ItemIndex));
      Exit;
    end;
  ReportDefaultAction('Save selected camera default',
                      FControls.SaveStandardDefault(lstControls.ItemIndex));
end;


procedure TMfCameraControlsDialog.RemoveDefaultClick(Sender: TObject);

begin

  if not Assigned(FControls) or (lstControls.ItemIndex < 0) then
    Exit;
  if UsingIni() then
    begin
      ReportDefaultAction('Remove selected INI setting',
                          FControls.RemoveIniSetting(lstControls.ItemIndex));
      Exit;
    end;
  ReportDefaultAction('Remove selected camera default',
                      FControls.RemoveStandardDefault(lstControls.ItemIndex));
end;


procedure TMfCameraControlsDialog.CloseClick(Sender: TObject);

begin

  Close();
end;

end.
