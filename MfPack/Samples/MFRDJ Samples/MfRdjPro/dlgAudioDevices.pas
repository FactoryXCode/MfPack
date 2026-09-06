// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgAudioDevices.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 4.0.0
// Description: A GUI dialog to pick audio devices.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
//==============================================================================
// Source: -
//
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
unit dlgAudioDevices;

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
  WinApi.CoreAudioApi.MMDevApiUtils,
  {Application}
  RDJ_Common,
  MPxpButton;

type

  TAudioDevicesDlg = class(TForm)
    pnlCtrls: TPanel;
    pnlButtons: TPanel;
    btnRefresh: TMPxpButton;
    btnOk: TMPxpButton;
    btnCancel: TMPxpButton;
    Bevel1: TBevel;
    Label1: TLabel;
    sgDevices: TStringGrid;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
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
    prDeviceInterfaceName: LPWSTR;
    prSelectedDeviceRow: Integer;

    procedure InitDeviceList();

  public
    { Public declarations }

    property EndPointDevice: IMMDevice read prEndPointDevice;
    property DataFlow: EDataFlow read prDataFlow write prDataFlow;
    property DeviceName: LPWSTR read prDeviceName;
    property DeviceInterfaceName: LPWSTR read prDeviceInterfaceName;
    property SelectedDevIndex: Integer read prSelectedDeviceRow;

  end;

var
  DevicesDlg: TAudioDevicesDlg;


implementation

{$R *.dfm}

const
  RDJ_MASTER_INPUT_DISPLAY_NAME = 'RDJ Master Input';
  RDJ_MASTER_INPUT_DESC = 'Dry mix output (No effects)';
  RDJ_MASTER_INPUT_STATE = 'Active';
  RDJ_MASTER_INPUT_IFACE = 'Internal mixer';


procedure TAudioDevicesDlg.btnCancelClick(Sender: TObject);
begin

  prDataFlow := eDataFlow(-1);
  prEndPointDevice := nil;
  prDeviceName := nil;
  prDeviceInterfaceName := nil;
  ModalResult := mrCancel;
end;


procedure TAudioDevicesDlg.btnOkClick(Sender: TObject);
begin

  if (sgDevices.Row > 0) and (sgDevices.Row < sgDevices.RowCount) then
    begin

      if (sgDevices.Row = sgDevices.RowCount - 1) then
        begin

          prDataFlow := ERender;
          prEndPointDevice := nil;
          prDeviceName := PWideChar(WideString(sgDevices.Cells[1, sgDevices.Row]));
          prDeviceInterfaceName := PWideChar(WideString(sgDevices.Cells[0, sgDevices.Row]));
        end
      else if (Length(prDevices) > 0) and ((sgDevices.Row - 1) <= High(prDevices)) then
        begin

          prDataFlow := prDevices[sgDevices.Row - 1].DataFlow;
          prEndPointDevice := prDevices[sgDevices.Row - 1].Device;
          prDeviceName := prDevices[sgDevices.Row - 1].DeviceName;
          prDeviceInterfaceName := prDevices[sgDevices.Row - 1].DevInterfaceName;
        end
      else
        begin

          ShowMessage('No device selected!');
          prDataFlow := eDataFlow(-1);
          prEndPointDevice := nil;
          prDeviceName := nil;
          prDeviceInterfaceName := nil;
          Exit;
        end;

      ModalResult := mrOk;
    end
  else
    begin

      ShowMessage('No device selected!');
      prDataFlow := eDataFlow(-1);
      prEndPointDevice := nil;
      prDeviceName := nil;
      prDeviceInterfaceName := nil;
    end;
end;


procedure TAudioDevicesDlg.btnRefreshClick(Sender: TObject);

  procedure DoList(aDevices: TEndPointDevice; idx: Integer);
    begin

      sgDevices.Cells[0, idx] := aDevices.DevInterfaceName;
      sgDevices.Cells[1, idx] := aDevices.DeviceName;
      sgDevices.Cells[2, idx] := aDevices.DeviceDesc;
      sgDevices.Cells[3, idx] := aDevices.sState;
      sgDevices.Cells[4, idx] := aDevices.pwszID;
      sgDevices.Cells[5, idx] := aDevices.iID.ToString();
    end;

  procedure Populate(devices: TEndPointDeviceArray; StartAt: Integer);
  var
    i: Integer;
    iAdd: Integer;

  begin

    i := StartAt;

    while (i <= Length(devices)) do
      begin
        DoList(devices[i - 1], i);
        sgDevices.RowCount := sgDevices.RowCount + 1;
        Inc(i);
      end;

    sgDevices.RowCount := sgDevices.RowCount + 1;
    iAdd := sgDevices.RowCount - 1;

    sgDevices.Cells[0, iAdd] := RDJ_MASTER_INPUT_IFACE;
    sgDevices.Cells[1, iAdd] := RDJ_MASTER_INPUT_DISPLAY_NAME;
    sgDevices.Cells[2, iAdd] := RDJ_MASTER_INPUT_DESC;
    sgDevices.Cells[3, iAdd] := RDJ_MASTER_INPUT_STATE;
    sgDevices.Cells[4, iAdd] := '{0.0.0.00000000}.{00000000-0000-0000-0000-000000000000}';
    sgDevices.Cells[5, iAdd] := IntToStr(iAdd - 1);
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
    ShowMessage(Format('Finding Rendering devices failed with code %d !',
                       [hr]));

  if (dwCount = 0) then
    begin
      InitDeviceList();
      sgDevices.RowCount := sgDevices.RowCount + 1;
      sgDevices.Cells[0, 1] := RDJ_MASTER_INPUT_IFACE;
      sgDevices.Cells[1, 1] := RDJ_MASTER_INPUT_DISPLAY_NAME;
      sgDevices.Cells[2, 1] := RDJ_MASTER_INPUT_DESC;
      sgDevices.Cells[3, 1] := RDJ_MASTER_INPUT_STATE;
      sgDevices.Cells[4, 1] := '{0.0.0.00000000}.{00000000-0000-0000-0000-000000000000}';
      sgDevices.Cells[5, 1] := '0';
    end
  else
    Populate(prDevices, 1);

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
      sgDevices.EndUpdate();
    {$IFEND}
  {$ENDIF}
end;


procedure TAudioDevicesDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  Action := caHide;
end;


procedure TAudioDevicesDlg.FormCreate(Sender: TObject);
begin

  prEndPointDevice := nil;
  prDeviceName := nil;
  prDeviceInterfaceName := nil;
  prSelectedDeviceRow := -1;
  prDataFlow := eRender;
  InitDeviceList();
end;


procedure TAudioDevicesDlg.FormShow(Sender: TObject);
begin

  //ApplyDarkWindowFrame(Handle);
  btnRefreshClick(Sender);
end;


procedure TAudioDevicesDlg.InitDeviceList();
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



procedure TAudioDevicesDlg.sgDevicesSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := (ARow > 0);
  if CanSelect then
    prSelectedDeviceRow := ARow
  else
    prSelectedDeviceRow := -1;
end;

end.

