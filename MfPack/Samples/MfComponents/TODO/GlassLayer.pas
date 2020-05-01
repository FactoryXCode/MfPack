// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: TransparentLayer.pas
// Kind: Pascal Unit
// Release date: 03-03-2016
// Language: ENU
//
// Version: 2.6.1
//
// Description: A border- and menuless transparent window
//              that can be used to project anything on top of a clipping window.
//              The current window has 2 components on it as an example.
//
//   When adding this form to a project,
//   choose in your IDE menu:
//   File/New/Other/Inheritable Items/TSubTitleLayer (option Inherited)
//
// Add "TransparentLayer" in the uses clause of your mainform.
// Add "SubTitleLayer: TSubTitleLayer;" in your mainform private section.
// Add "procedure CreateSubTitleLayer(bShowCustomText: Boolean);" in your mainform private section.
//
// Example code:
////////////////////////////////////////////////////////////////////////////////
//procedure TMyMainform.CreateSubTitleLayer(bShowCustomText: Boolean);
//begin
//  if (SubTitleLayer = Nil) then
//    SubTitleLayer:= TSubTitleLayer.Create(Nil);
//
//  with SubTitleLayer do
//    begin
//      FormStyle:= fsStayOnTop;
//      Visible:= True;
//      Parent:= frm_MfPlayer.Handle;
//      FollowTheLeader(pnlControls.Top);
//
//      // Initial text before subtitling starts (Optional)
//      // This can be any message
//      if (bShowCustomText = true) then
//        begin
//          SubtitleText:= 'Mediafile ' +
//                         ExtractFileName(MfPlayer.FileName) +
//                         ' has been loaded...' + #13 +
//                         'Click "Play" or hit the spacebar to start playing...';
//        end;
//    end;
//end;
//
// See for a complete sample: MfPlayer III >= 3.3.4
//
////////////////////////////////////////////////////////////////////////////////
//
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
// -----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2019                     WIN10 May 2019 update (version 1903)
// 03/06/2019                     ISÁK release.
// 18/06/2019                     Prodigy release.
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: MfPack Samples
// Related projects: MfPack 2.6.1
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================

unit GlassLayer;

interface

uses
  {Winapi}
  Winapi.Windows,
  Winapi.Messages,
  {System}
  System.SysUtils,
  System.Classes,
  {Vcl}
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

const
  DELTAFONTHEIGHT = 0.636364;


type
  TGlassLayer = class(TGlassFrame)

  protected
    { Protected declarations }
    //frmParent: TCustomForm;


  private
    { Private declarations }
    iTextLines: integer;

    procedure GetParentRects();
    procedure GetTextLines(Value: string);

    procedure OnMove(var msg: TWMMove); message WM_MOVE;

  public
    { Public declarations }


  end;




procedure Register;

implementation

const
  User32Lib = 'user32.dll';

{$R *.dfm}

// Register as component
procedure Register;
begin
  System.Classes.RegisterComponents('MfPack Samples', [TGlassLayer]);
end;



procedure TGlassLayer.Create(Sender: TObject);
begin
  inherited;


end;




procedure TGlassLayer.GetParentRects();
begin

end;



procedure TGlassLayer.OnMove(var Msg: TWMMove);
begin
  inherited;
  GetParentRects();
  //Top := ParentRect.Top;
  //Left := ParentRect.Left;

end;



procedure TGlassLayer.GetTextLines(Value: string);
var
  i: integer;

begin
  iTextLines := 1; // default value
  // Get the textlines
  for i := 1 to Length(Value)-1 do
    if Value[i] = #13 then
      inc(iTextLines);
end;


end.
