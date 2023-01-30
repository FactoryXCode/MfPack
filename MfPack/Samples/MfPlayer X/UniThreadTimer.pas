// FactoryX
//
// Copyright © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: UniThreadTimer.pas
// Kind: Pascal Unit
// Release date: 24-02-2012
// Language: ENU
//
// Version: 3.0.1
// Description: Universal threaded timer
//              This timer can be used for a variety of tasks where you need a
//              lightweight, precise and threadsave timer.
//
// Organisation: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues
// Contributor(s): Tony Kalf (maXcomX), Ramyses De Macedo Rodrigues.
//
//
// ----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ---------------------------------------------
// 13/08/2020 All                 Enigma release.
// ----------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: MfSamples
// Related projects: MfPack301
// Known Issues: -
// Compiler version: 17 up to 21
// ToDo: -
// SDK version: 10.0.18362.0 (19H1)
// =============================================================================
// Source: -
// =============================================================================
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
unit UniThreadTimer;

interface

uses
  Windows,
  System.Classes,
  System.SysUtils;

type
  // Forwarded class
  TUniThreadedTimer = class;

  TTimerThread = class(TThread)
  private
    { Private declarations }
    FTimer: TUniThreadedTimer;

  protected
    { Protected declarations }
    procedure DoTimer();

  public
    { Public declarations }
    constructor Create(ATimer: TUniThreadedTimer);
    destructor Destroy(); override;
    procedure Execute(); override;

  end;


  TUniThreadedTimer = class(TComponent)
  private
    { Private declarations }
    bEnabled: Boolean;
    dwPeriod: DWord;
    neOnTimer: TNotifyEvent;

    procedure SetEnabled(Value: Boolean);
    procedure SetTimerPeriod(Value: DWord);

  protected
    { Protected declarations }
    thTimerThread: TTimerThread;
    procedure UpdateTimer();

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy(); override;

  published
    { Published declarations}
    property Enabled: Boolean read bEnabled write SetEnabled;
    property Period: DWord read dwPeriod write SetTimerPeriod;
    property OnTimerEvent: TNotifyEvent read neOnTimer write neOnTimer;

  end;

implementation


// TgtTimerThread //////////////////////////////////////////////////////////////

constructor TTimerThread.Create(ATimer: TUniThreadedTimer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTimer := ATimer;
end;


destructor TTimerThread.Destroy();
begin
  inherited Destroy();
end;


procedure TTimerThread.DoTimer();
begin
  if Assigned(FTimer.OnTimerEvent) then
    FTimer.OnTimerEvent(FTimer);
end;


procedure TTimerThread.Execute();
begin
  while (not Self.Terminated) and (FTimer.Enabled) do
  begin
    WaitForSingleObject(Self.Handle,
                        FTimer.Period);
    Synchronize(DoTimer);
  end;
end;


// TgtTimer ////////////////////////////////////////////////////////////////////

constructor TUniThreadedTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  bEnabled := True;
  Period := 100;
end;


destructor TUniThreadedTimer.Destroy();
begin
  inherited Destroy();
end;


procedure TUniThreadedTimer.UpdateTimer();
begin
  if Assigned(thTimerThread) then
    begin
      thTimerThread.Terminate;
      thTimerThread := Nil;
    end;

  if (Enabled = True) then
    begin
      thTimerThread := TTimerThread.Create(Self);
      thTimerThread.Resume;
    end;
end;


procedure TUniThreadedTimer.SetEnabled(Value: Boolean);
begin
  bEnabled := Value;
  UpdateTimer();
end;


procedure TUniThreadedTimer.SetTimerPeriod(Value: DWord);
begin
  if (Value <> dwPeriod) then
  begin
    dwPeriod := Value;
    UpdateTimer();
  end;
end;

end.