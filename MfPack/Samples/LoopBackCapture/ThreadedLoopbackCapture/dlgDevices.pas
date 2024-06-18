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
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 (2H20) or later.
//
// Related objects: -
// Related projects: MfPack/Samples/LoopbackCapture
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
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
    sgRenderingDevices: TStringGrid;
    Bevel1: TBevel;
    procedure butCancelClick(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure butRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }

    RenderDevices: TEndPointDeviceArray;
    CaptureDevices: TEndPointDeviceArray;
    AllDevices: TEndPointDeviceArray;
    prEndPointDevice: IMMDevice;
    prDataFlow: EDataFlow;
    prDeviceName: LPWSTR;

    procedure InitDeviceList();

  public
    { Public declarations }

    property EndPointDevice: IMMDevice read prEndPointDevice;
    property DataFlow: EDataFlow read prDataFlow;
    property DeviceName: LPWSTR read prDeviceName;
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
  if (Length(RenderDevices) > 0) and (sgRenderingDevices.Row > 0) then
    begin
      prDataFlow := RenderDevices[sgRenderingDevices.Row - 1].DataFlow;
      prEndPointDevice := RenderDevices[sgRenderingDevices.Row - 1].Device;
      prDeviceName := RenderDevices[sgRenderingDevices.Row - 1].DeviceName;
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
      sgRenderingDevices.Cells[0, idx] := aDevices.DevInterfaceName;
      sgRenderingDevices.Cells[1, idx] := aDevices.DeviceName;
      sgRenderingDevices.Cells[2, idx] := aDevices.DeviceDesc;
      sgRenderingDevices.Cells[3, idx] := aDevices.sState;
      sgRenderingDevices.Cells[4, idx] := aDevices.pwszID;
      sgRenderingDevices.Cells[5, idx] := aDevices.iID.ToString();
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
      sgRenderingDevices.RowCount := sgRenderingDevices.RowCount + 1;
      inc(i);
    end;
  end;

var
  hr: HResult;
  dwCount1,
  dwCount2: DWord;
  ptr: Integer;
  i: Integer;

begin

  InitDeviceList();

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
      sgRenderingDevices.BeginUpdate();
    {$IFEND}
  {$ENDIF}
  // Get rendering devices first
  hr := GetEndpointDevices(eRender,
                           DEVICE_STATE_ACTIVE or DEVICE_STATE_DISABLED,
                           RenderDevices,
                           dwCount1);
  if FAILED(hr) then
    begin
      ShowMessage(Format('Finding Rendering devices failed with code %d !',[hr]));
    end;

  // Get capture devices
  hr := GetEndpointDevices(eCapture,
                           DEVICE_STATE_ACTIVE or DEVICE_STATE_DISABLED,
                           CaptureDevices,
                           dwCount2);
  if FAILED(hr) then
    begin
      ShowMessage(Format('Finding Capture devices failed with code %d !',[hr]));
    end;

  if (dwCount1 + dwCount2 > 0) then
    begin
      // Come Together {Beatles}
      SetLength(AllDevices,
                Integer(dwCount1) + Integer(dwCount2));

      ptr := 0;

      for i := 0 to Length(RenderDevices) - 1 do    // Add RenderDevices.
        begin
          AllDevices[ptr] := RenderDevices[i];
          Inc(ptr);
        end;

      for i := 0 to Length(CaptureDevices) - 1 do    // Add CaptureDevices.
        begin
          AllDevices[ptr] := CaptureDevices[i];
          Inc(ptr);
        end;

      Populate(AllDevices,
               1);
    end
  else
    InitDeviceList();

  {$IFDEF ConditionalExpressions}
    {$IF CompilerVersion > 31.0}
      sgRenderingDevices.EndUpdate();
    {$IFEND}
  {$ENDIF}
end;


procedure TDevicesDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetLength(AllDevices,
            0);
end;


procedure TDevicesDlg.FormCreate(Sender: TObject);
begin
  prEndPointDevice := nil;
  prDataFlow := ERender;

end;


procedure TDevicesDlg.FormShow(Sender: TObject);
begin
  butRefreshClick(Self);
end;


procedure TDevicesDlg.InitDeviceList();
begin
  SetLength(AllDevices,
            0);

  sgRenderingDevices.RowCount := 1;
  sgRenderingDevices.ColCount := 6;

  // For some reason, the methods to dimension TStringGrid changed?
  {$IF CompilerVersion < 31.0}
  sgRenderingDevices.ColWidths[0] := 200;
  sgRenderingDevices.ColWidths[1] := 200;
  sgRenderingDevices.ColWidths[2] := 180;
  sgRenderingDevices.ColWidths[3] := 50;
  sgRenderingDevices.ColWidths[4] := 340;
  sgRenderingDevices.ColWidths[5] := 80;
  {$ELSE}
  sgRenderingDevices.ColWidths[0] := 290;
  sgRenderingDevices.ColWidths[1] := 290;
  sgRenderingDevices.ColWidths[2] := 260;
  sgRenderingDevices.ColWidths[3] := 90;
  sgRenderingDevices.ColWidths[4] := 390;
  sgRenderingDevices.ColWidths[5] := 100;
  {$ENDIF}

  // Calculate width of the stringgrid and dialog
  sgRenderingDevices.Width := sgRenderingDevices.ColWidths[0] +
                              sgRenderingDevices.ColWidths[1] +
                              sgRenderingDevices.ColWidths[2] +
                              sgRenderingDevices.ColWidths[3] +
                              sgRenderingDevices.ColWidths[4] +
                              sgRenderingDevices.ColWidths[5] +
                              (sgRenderingDevices.BevelWidth * 2) + 10 {scrollbar};
  Width := sgRenderingDevices.Width + 60;

  // The header
  sgRenderingDevices.Cells[0, 0] := 'Device Interface Name';
  sgRenderingDevices.Cells[1, 0] := 'Device Name';
  sgRenderingDevices.Cells[2, 0] := 'Device Description';
  sgRenderingDevices.Cells[3, 0] := 'State';
  sgRenderingDevices.Cells[4, 0] := 'Internal ID';
  sgRenderingDevices.Cells[5, 0] := 'Device Index';
end;

end.
