// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgChooseDevice.pas
// Kind: Pascal Unit
// Release date: 18-11-2022
// Language: ENU
//
// Revision Version: 3.1.4
//
// Description:
//   Dialog to pick a videodevice and it's (supported) resolutions and samplerates.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX)
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPackX314/Samples/MFCaptureEngineVideoCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: -
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
unit dlgChooseDevice;

interface


  // Undefine this when not needed!
  {$DEFINE SAVE_DEBUG_REPORT}

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
    procedure cbxSupportedFormatsOnlyClick(Sender: TObject);
  private
    { Private declarations }

    iSelectedDevice: Integer;
    iSelectedFormat: Integer;

    procedure PopulateDevices();
    procedure ClearFormatsList();
    function PopulateFormats(bSupportedFormatsOnly: Boolean): HResult;

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

{$IFDEF SAVE_DEBUG_REPORT}
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
  PopulateFormats(cbxSupportedFormatsOnly.Checked);
end;


// FormCreate
procedure TChooseDeviceDlg.FormCreate(Sender: TObject);
begin
  iSelectedDevice := -1;
  iSelectedFormat := -1;
  PopulateDevices();
end;


procedure TChooseDeviceDlg.lbxDeviceListClick(Sender: TObject);
begin
  if (lbxDeviceList.ItemIndex > -1) then
    begin
      iSelectedDevice := lbxDeviceList.ItemIndex;
      PopulateFormats(cbxSupportedFormatsOnly.Checked);
      btnOK.Enabled := True;
    end
  else
    begin
      btnOK.Enabled := False;
      iSelectedDevice := -1;
    end;
end;


procedure TChooseDeviceDlg.PopulateDevices();
var
  i: Integer;

begin
  lbxDeviceList.Clear;

  // Append the friendly name to the combobox.
  for i := 0 to FDeviceExplorer.DevicesCount -1 do
    lbxDeviceList.Items.Append(FDeviceExplorer.DeviceProperties[i].lpDisplayName);

  // Choose first device in the list
  lbxDeviceList.ItemIndex := 0;
  iSelectedDevice := lbxDeviceList.ItemIndex;
  PopulateFormats(cbxSupportedFormatsOnly.Checked);
end;


procedure TChooseDeviceDlg.ClearFormatsList();
var
  i: Integer;

begin
  for i := 0 to sgResolutions.ColCount - 1 do
    sgResolutions.Cols[i].Clear;
  sgResolutions.RowCount := 1;
  sgResolutions.Repaint;
end;


// Populate the listboxes with camera's and properties found on this system
// ========================================================================
function TChooseDeviceDlg.PopulateFormats(bSupportedFormatsOnly: Boolean): HResult;

   // Helper
   procedure AddFormat(iCol: Integer;
                       iDev: Integer;
                       iForm: Integer);
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

       {Supported by MF input but not on output}
       //
       sgResolutions.Cells[3, iCol] := Format('%s',
                                              [BoolToStrYesNo(FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].bMFSupported and (FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].iFrameRate > 29))]);
       {Index}
       sgResolutions.Cells[4, iCol] := Format('%d',
                                              [FDeviceExplorer.DeviceProperties[iDev].aVideoFormats[iForm].FormatsIndex]);
     end;

var
  i,
  rc: Integer;
  hr: HResult;

label
  Done;

begin
  hr := S_OK;

  if (FDeviceExplorer.DevicesCount = 0) then
    begin
      Result := MF_E_NO_CAPTURE_DEVICES_AVAILABLE;
      Exit;
    end;

  if (iSelectedDevice < 0) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;
try
try

  ClearFormatsList();

  sgResolutions.ColCount := 5;
  sgResolutions.RowCount := 1;

  sgResolutions.ColWidths[0] := 80;
  sgResolutions.ColWidths[1] := 60;
  sgResolutions.ColWidths[2] := 120;
  sgResolutions.ColWidths[3] := 120;
  sgResolutions.ColWidths[4] := -1; // Hide last column

  sgResolutions.Width :=  sgResolutions.ColWidths[0] +
                          sgResolutions.ColWidths[1] +
                          sgResolutions.ColWidths[2] +
                          sgResolutions.ColWidths[3] +
                          sgResolutions.ColCount;

  sgResolutions.Cells[0, 0] := 'Height x Width';
  sgResolutions.Cells[1, 0] := 'FPS';
  sgResolutions.Cells[2, 0] := 'Video Format';
  sgResolutions.Cells[3, 0] := 'Supported Format';

  sgResolutions.Cells[4, 0] := 'Formats index';  // This a hidden column.
  rc := 1;


  // List devicecapabilities.

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgResolutions.BeginUpdate();
    {$IFEND}
  {$ENDIF}

  for i := 0 to FDeviceExplorer.NativeFormats -1 do
    begin
      if bSupportedFormatsOnly then
        begin
          if FDeviceExplorer.DeviceProperties[iSelectedDevice].aVideoFormats[i].bMFSupported and (FDeviceExplorer.DeviceProperties[iSelectedDevice].aVideoFormats[i].iFrameRate > 29) then
            begin
              AddFormat(rc,
                        iSelectedDevice,
                        i);
              Inc(rc);
              sgResolutions.RowCount := rc;
            end;
        end
      else   // all
        begin
          AddFormat(rc,
                    iSelectedDevice,
                    i);
          Inc(rc);
          sgResolutions.RowCount := rc;
        end;
    end;

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
       sgResolutions.EndUpdate();
    {$IFEND}
  {$ENDIF}

except
  hr := E_FAIL;
end;

finally
  Result := hr;
end;
end;


end.

