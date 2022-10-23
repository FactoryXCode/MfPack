// FactoryX
//
// Copyright FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: dlgStreamSelect.pas
// Kind: Pascal Unit
// Release date: 05-01-2016
// Language: ENU
//
// Version: 3.1.3
// Description: Dialog example to select streams from a file source.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Peter (OzShips), Ramyses De Macedo Rodrigues.
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ramyses De Macedo Rodrigues.
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX313
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
// =============================================================================
// Source: Parts of CPlayer Examples
//
// Copyright FactoryX. All rights reserved.
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
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//==============================================================================
unit dlgStreamSelect;

interface

uses
  {Winapi}
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
  Vcl.CheckLst,
  Vcl.ComCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfMetLib,
  {Application}
  MfPlayerClassX;

type

  TdlgSelectStreams = class(TForm)
    butClose: TButton;
    lvStreams: TListView;
    procedure FormShow(Sender: TObject);
    procedure butCloseClick(Sender: TObject);
    procedure lvStreamsItemChecked(Sender: TObject; Item: TListItem);

  private
    { Private declarations }
    iCurAudioStream, iCurVideoStream: DWord;

  public
    { Public declarations }

  end;

var
  dlgSelectStreams: TdlgSelectStreams;

implementation

{$R *.dfm}


procedure TdlgSelectStreams.butCloseClick(Sender: TObject);
begin
  dlgSelectStreams.Close;
end;


procedure TdlgSelectStreams.FormShow(Sender: TObject);
var
  i: Integer;
  lItem: TListItem;

begin

  lvStreams.Items.Clear;

 // Get all video streams and their properties
 for i:= 0 to High(MfPlayerX.StreamContents) do
   begin

     lItem:= lvStreams.Items.Add;
     lItem.Caption:= IntToStr(MfPlayerX.StreamContents[i].dwStreamIndex);
     lItem.Checked:= MfPlayerX.StreamContents[i].bSelected;
     lItem.SubItems.Add(MfPlayerX.StreamContents[i].audio_lpStreamName);

     if (MfPlayerX.StreamContents[i].idStreamMediaType = mtVideo) then
       begin
         lItem.SubItems.Add('Video');
         if (MfPlayerX.StreamContents[i].bSelected = True) then
           iCurVideoStream:= MfPlayerX.StreamContents[i].dwStreamIndex;
       end;

     if (MfPlayerX.StreamContents[i].idStreamMediaType = mtAudio) then
       begin
         lItem.SubItems.Add('Audio');
         if (MfPlayerX.StreamContents[i].bSelected = True) then
           iCurAudioStream:= MfPlayerX.StreamContents[i].dwStreamIndex;
       end;

     lItem.SubItems.Add(MfPlayerX.StreamContents[i].audio_lpLangShortName);

   end;
end;



// Check and alter the mediastreams
// Note: Only 1 video and/or 1 audio stream can be selected.
//       Calling MfPlayer.SetActiveStreamType(Streamtype, streamindex) will
//       sets the active stream and update the streaminfo array.
procedure TdlgSelectStreams.lvStreamsItemChecked(Sender: TObject;
                                                 Item: TListItem);
begin
 if (Item.SubItems.Count > 0) and (Item.Checked = True) then
    begin

      if (Item.SubItems[1] = 'Audio') then
        begin
          lvStreams.Items[iCurAudioStream].Checked:= False;
          iCurAudioStream:= StrToInt(Item.Caption);
          MfPlayerX.SetActiveStreamType(mtAudio,
                                        iCurAudioStream);

        end;

      if (Item.SubItems[1] = 'Video') then
        begin
          lvStreams.Items[iCurVideoStream].Checked:= False;
          iCurVideoStream:= StrToInt(Item.Caption);
          MfPlayerX.SetActiveStreamType(mtVideo,
                                        iCurVideoStream);
        end;
    end;
end;


end.

