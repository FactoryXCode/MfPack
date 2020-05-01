// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: SubTitlePanel.pas
// Kind: Pascal Unit
// Release date: 04-08-2016
// Language: ENU
//
// Version: 2.6.1
// Description: Requires Windows 7 or later.
//              A panel to project subtitles.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2019                     WIN10 May 2019 update (version 1903)
// 03/06/2019                     ISÁK release.
// 18/06/2019                     Prodigy release.
// -----------------------------------------------------------------------------
//
// Remarks: To install the visual components, choose Install in the Project Manager.
//
// Related objects: MfPack Samples 2.6.1
// Related projects: MfPackX261
// Known Issues: -
// Compiler version: 23 up to 33
// Todo: -
// SDK version: 10.0.18362.0 (19H1)
// =============================================================================
// Source: -
//
//
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit FloatingLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;


type
  TFloatingLabel = class(TLabel)
  private

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy(); override;

  published


end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MfPack Samples', [TFloatingLabel]);
end;


constructor TFloatingLabel.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);

end;


destructor TFloatingLabel.Destroy();
begin

  inherited Destroy;
end;


procedure TFloatingLabel.WMSize(var Message: TWMSize);
begin
  inherited;
  BringToFront;
end;


procedure TFloatingLabel.WMMove(var Message: TWMMove);
begin
  inherited;
  BringToFront;
end;


procedure TFloatingLabel.WMPaint(var Message: TWMPaint);
begin
  inherited;
  BringToFront;
end;


end.
