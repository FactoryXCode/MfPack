// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX - D3D11
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module:  frmCaptureVideoFromGPU.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2025
// Language: ENU
//
// Revision Version: 4.0.0
// Description: GUI
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Carmen (carmenh), Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
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
// Copyright (c) FactoryX. All rights reserved.
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
unit frmCaptureVideoFromGPU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  CaptureStreamEngine;

type
  TfrmCapture = class(TForm)
    mmoLog  : TMemo;
    pnlPreview: TPanel;
    Panel1: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    cbxResulotuions: TComboBox;
    Label1: TLabel;
    cbxFrameRate: TComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);

  private

    FCaptureWidth,
    FCaptureHeigth: UINT;
    FFrameRate: UINT32;

    FEngine: TCaptureStreamEngine;

    procedure GetRenderSettings();

    procedure CaptureProgress(Sender: TObject;
                              FrameIndex: Int64;
                              Ms: Double);

    procedure CaptureError(Sender: TObject;
                           const Msg: string);

  public

  end;

var
  frmCapture: TfrmCapture;


implementation


{$R *.dfm}

procedure TfrmCapture.FormCreate(Sender: TObject);
begin

  mmoLog.Clear;
  mmoLog.Lines.Add('Initializing...');

  // Get height, width and sample rate.
  GetRenderSettings();

  // Create and prepare the engine for rendering.
  FEngine := TCaptureStreamEngine.Create(pnlPreview.Handle,
                                         FCaptureWidth,
                                         FCaptureHeigth,
                                         FFrameRate);

  FEngine.OnProgress := CaptureProgress;
  FEngine.OnError := CaptureError;

  mmoLog.Lines.Add('Ready.');
end;


procedure TfrmCapture.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEngine);
end;


procedure TfrmCapture.btnStartClick(Sender: TObject);
var
  outFile: string;

begin

  // Get height, width and sample rate if changed inbetween.
  GetRenderSettings();

  FEngine.SetFrameWidth := FCaptureWidth;
  FEngine.SetFrameHeight := FCaptureHeigth;
  FEngine.SetFrameRate := FFrameRate;

  outFile := ExtractFilePath(ParamStr(0)) + 'capture_output.mp4';

  mmoLog.Lines.Add('Starting capture: ' + outFile);
  FEngine.StartCapture(outFile);
end;


procedure TfrmCapture.btnStopClick(Sender: TObject);
begin

  mmoLog.Lines.Add('Stopping capture...');
  FEngine.StopCapture;
  mmoLog.Lines.Add('Capture stopped.');
end;


procedure TfrmCapture.GetRenderSettings();
begin

  // Preview window size = 0
  // 720p (1280 x 720) = 1
  // Full HD (1920 x 1080) = 2
  // 2K (2560 x 1440) = 3
  // 4K (3840 x 2160) = 4

  case cbxResulotuions.ItemIndex of
    0:  begin
          FCaptureWidth := pnlPreview.Width;
          FCaptureHeigth := pnlPreview.Height;
        end;

    1:  begin
          FCaptureWidth := 1280;
          FCaptureHeigth := 720;
        end;

    2:  begin
          FCaptureWidth := 1920;
          FCaptureHeigth := 1080;
        end;

    3:  begin
          FCaptureWidth := 2560;
          FCaptureHeigth := 1440;
        end;

    4:  begin
          FCaptureWidth := 3840;
          FCaptureHeigth := 2160;
        end;
  end;

  case cbxFrameRate.ItemIndex of
    0:  FFrameRate := 30;

    1:  FFrameRate := 60;
  end;
end;


procedure TfrmCapture.CaptureProgress(Sender: TObject;
                                      FrameIndex: Int64;
                                      Ms: Double);
begin

  TThread.Queue(nil,
    procedure
    begin
      mmoLog.Lines.Add(Format('Frame %d captured in Time: %f seconds', [FrameIndex, Ms]));
    end);
end;


procedure TfrmCapture.CaptureError(Sender: TObject;
                                   const Msg: string);
begin

  TThread.Queue(nil,
    procedure
    begin
      mmoLog.Lines.Add('ERROR: ' + Msg);
    end);
end;

end.
