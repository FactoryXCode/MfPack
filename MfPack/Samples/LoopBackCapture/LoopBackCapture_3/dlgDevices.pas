//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgDevices.pas
// Kind: Pascal Unit
// Release date: 23-04-2023
// Language: ENU
//
// Revision Version: 3.1.7
//
// Description:
//   Dialog to choose an rendering or capture device.
//
// Organisation: FactoryX
// Initiator(s): maXcomX
// Contributor(s): Tony (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPack/Samples/LoopbackCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: Tony Kalf / FactoryX
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
unit dlgDevices;

interface

uses
  {Winapi}
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
  Vcl.Grids,
  Vcl.ExtCtrls,
  {CoreAudioApi}
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.MMDevApiUtils;

type
  TDevicesDlg = class(TForm)
    Label1: TLabel;
    butOk: TButton;
    butCancel: TButton;
    butRefresh: TButton;
    sgDevices: TStringGrid;
    Bevel1: TBevel;
    procedure butCancelClick(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure butRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure sgDevicesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);

  private
    { Private declarations }

    prDevices: TEndPointDeviceArray;
    prEndPointDevice: IMMDevice;
    prDataFlow: EDataFlow;
    prDeviceName: LPWSTR;
    prSelectedDeviceRow: Integer;

    procedure InitDeviceList();

  public
    { Public declarations }

    property EndPointDevice: IMMDevice read prEndPointDevice;
    property DataFlow: EDataFlow read prDataFlow write prDataFlow;
    property DeviceName: LPWSTR read prDeviceName;
    property SelectedDevIndex: Integer read prSelectedDeviceRow;

  end;

var
  DevicesDlg: TDevicesDlg;


implementation

{$R *.dfm}


procedure TDevicesDlg.butCancelClick(Sender: TObject);
begin

  prDataFlow := eDataFlow(-1);
  prEndPointDevice := nil;
  prDeviceName := nil;
  ModalResult := mrCancel;
end;


procedure TDevicesDlg.butOkClick(Sender: TObject);
begin

  if (Length(prDevices) > 0) and (sgDevices.Row > 0) then
    begin
      prDataFlow := prDevices[sgDevices.Row - 1].DataFlow;
      prEndPointDevice := prDevices[sgDevices.Row - 1].Device;
      prDeviceName := prDevices[sgDevices.Row - 1].DeviceName;
      ModalResult := mrOk;
    end
  else
    begin
      ShowMessage('No device selected!');
      prDataFlow := eDataFlow(-1);
      prEndPointDevice := nil;
      prDeviceName := nil;
    end;
end;


procedure TDevicesDlg.butRefreshClick(Sender: TObject);

  procedure DoList(aDevices: TEndPointDevice; idx: Integer);
    begin
      sgDevices.Cells[0, idx] := aDevices.DevInterfaceName;
      sgDevices.Cells[1, idx] := aDevices.DeviceName;
      sgDevices.Cells[2, idx] := aDevices.DeviceDesc;
      sgDevices.Cells[3, idx] := aDevices.sState;
      sgDevices.Cells[4, idx] := aDevices.pwszID;
      sgDevices.Cells[5, idx] := aDevices.iID.ToString();
    end;

  procedure populate(devices: TEndPointDeviceArray; StartAt: Integer);
  var
    i: Integer;
  begin
  i := StartAt; // Start at at row 1
  while (i <= Length(devices)) do
    begin
      // Write to new row
      DoList(devices[i -1], i);
      // Add new row
      sgDevices.RowCount := sgDevices.RowCount + 1;
      Inc(i);
    end;
  end;

var
  hr: HResult;
  dwCount: DWord;

begin

  InitDeviceList();

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
      sgDevices.BeginUpdate();
    {$IFEND}
  {$ENDIF}

  hr := GetEndpointDevices(prDataFlow,
                           DEVICE_STATE_ACTIVE or DEVICE_STATE_DISABLED,
                           prDevices,
                           dwCount);
  if FAILED(hr) then
    begin
      ShowMessage(Format('Finding Rendering devices failed with code %d !',[hr]));
    end;

  if (dwCount = 0) then
    InitDeviceList()
  else
    Populate(prDevices,
             1);

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
      sgDevices.EndUpdate();
    {$IFEND}
  {$ENDIF}
end;


procedure TDevicesDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  SetLength(prDevices,
            0);
end;


procedure TDevicesDlg.FormCreate(Sender: TObject);
begin

  prEndPointDevice := nil;
end;


procedure TDevicesDlg.FormShow(Sender: TObject);
begin

  butRefreshClick(Self);
end;


procedure TDevicesDlg.InitDeviceList();
begin

  SetLength(prDevices,
            0);

  sgDevices.RowCount := 1;
  sgDevices.ColCount := 6;

  // For some reason, the methods to dimension TStringGrid changed?
  {$IF CompilerVersion < 31.0}
  sgDevices.ColWidths[0] := 200;
  sgDevices.ColWidths[1] := 200;
  sgDevices.ColWidths[2] := 180;
  sgDevices.ColWidths[3] := 50;
  sgDevices.ColWidths[4] := 340;
  sgDevices.ColWidths[5] := 80;
  {$ELSE}
  sgDevices.ColWidths[0] := 290;
  sgDevices.ColWidths[1] := 290;
  sgDevices.ColWidths[2] := 260;
  sgDevices.ColWidths[3] := 90;
  sgDevices.ColWidths[4] := 390;
  sgDevices.ColWidths[5] := 100;
  {$ENDIF}

  // Calculate width of the stringgrid and dialog
  sgDevices.Width := sgDevices.ColWidths[0] +
                              sgDevices.ColWidths[1] +
                              sgDevices.ColWidths[2] +
                              sgDevices.ColWidths[3] +
                              sgDevices.ColWidths[4] +
                              sgDevices.ColWidths[5] +
                              (sgDevices.BevelWidth * 2) + 10 {scrollbar};
  Width := sgDevices.Width + 60;

  // The header.
  sgDevices.Cells[0, 0] := 'Device Interface Name';
  sgDevices.Cells[1, 0] := 'Device Name';
  sgDevices.Cells[2, 0] := 'Device Description';
  sgDevices.Cells[3, 0] := 'State';
  sgDevices.Cells[4, 0] := 'Internal ID';
  sgDevices.Cells[5, 0] := 'Device Index';
end;


procedure TDevicesDlg.sgDevicesSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin

  prSelectedDeviceRow := ARow;
end;

end.
