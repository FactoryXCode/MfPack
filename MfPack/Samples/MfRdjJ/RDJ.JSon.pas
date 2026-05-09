// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - WASAPI
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: RDJ.JSon.pas
// Kind: Pascal / Delphi unit
// Release date: 02-04-2023
// Language: ENU
//
// Revision Version: 3.2.0
// Description: json reader/writer unit.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
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
unit RDJ.JSon;

interface

uses

  {WinApi}
  WinApi.Windows,
  {System}
  System.SysUtils,
  System.JSON,
  System.IOUtils;

type

  TRDJRadioStatusJson = class
  public

    class procedure WriteRadioStatusJson(const AFileName: string;
                                         const ADjName: string;
                                         const AShowName: string;
                                         const AArtist: string;
                                         const ATitle: string;
                                         const ACoverUrl: string = '';
                                         const AListeners: Integer = -1;
                                         const AOnAir: Integer = -1;
                                         const AOnAirLock: string = ''); static;

    class function LoadNowPlayingJson(const AFileName: string): TJSONObject;

  end;


implementation

uses
  {Application}
  RDJ.Setup,
  frmMainMdi;

class procedure TRDJRadioStatusJson.WriteRadioStatusJson(const AFileName: string;
                                                         const ADjName: string;
                                                         const AShowName: string;
                                                         const AArtist: string;
                                                         const ATitle: string;
                                                         const ACoverUrl: string = '';
                                                         const AListeners: Integer = -1;
                                                         const AOnAir: Integer = -1;
                                                         const AOnAirLock: string = '');
var
  Json: TJSONObject;
  JsonValue: TJSONValue;
  ExistingText: string;
  JsonFileName: string;
  TmpFileName: string;
  Setup: TRDJSetup;

  procedure SetJsonInteger(const AName: string;
                           const AValue: Integer);
  var
    Pair: TJSONPair;

  begin

    if (AValue < 0) then
      Exit;

    Pair := Json.RemovePair(AName);
    if Assigned(Pair) then
      Pair.Free;

    Json.AddPair(AName,
                 TJSONNumber.Create(AValue));
  end;


  procedure SetJsonString(const AName: string;
                          const AValue: string);
  begin

    if (AValue = '') then
      Exit;

    Json.RemovePair(AName).Free;
    Json.AddPair(AName,
                 AValue);
  end;

begin

  if (Trim(AFileName) = '') then
    Exit;

  if (ADjName = '') and
     (AShowName = '') and
     (AArtist = '') and
     (ATitle = '') and
     (AListeners = 0) then
    Exit;

  if not Assigned(MainMDIFrm) then
    Exit;

  Setup := MainMDIFrm.Setup;

  JsonFileName := AFileName;

  TmpFileName := JsonFileName + '.tmp';

  Json := nil;
  JsonValue := nil;

  try

    if TFile.Exists(JsonFileName) then
      begin

        ExistingText := TFile.ReadAllText(JsonFileName,
                                          TEncoding.UTF8);

        JsonValue := TJSONObject.ParseJSONValue(ExistingText);

        if JsonValue is TJSONObject then
          begin

            Json := TJSONObject(JsonValue);
            JsonValue := nil;
          end;
      end;

    if not Assigned(Json) then
      Json := TJSONObject.Create();

    SetJsonString('djName',
                  ADjName);

    SetJsonString('show',
                  AShowName);

    SetJsonString('showName',
                  AShowName);

    SetJsonString('artist',
                  AArtist);

    SetJsonString('title',
                  ATitle);

    if (ACoverUrl <> '') then
      SetJsonString('coverUrl',
                    ExtractFileName(ACoverUrl) + '?ts=' + IntToStr(GetTickCount));

    SetJsonInteger('displayListeners',
                   AListeners);

    SetJsonInteger('onAir',
                   AOnAir);

    SetJsonString('onAirLock',
                  AOnAirLock);

    if (AOnAir > 0) then
      SetJsonString('onAirSince',
                    FormatDateTime('yyyy-mm-dd hh:nn:ss',
                                   Now));


    TFile.WriteAllText(TmpFileName,
                       Json.Format(2),
                       TEncoding.UTF8);

    if TFile.Exists(JsonFileName) then
      TFile.Delete(JsonFileName);

    TFile.Move(TmpFileName,
               JsonFileName);

  finally

    JsonValue.Free;
    Json.Free;
  end;
end;


class function TRDJRadioStatusJson.LoadNowPlayingJson(const AFileName: string): TJSONObject;
var
  S: string;

begin

  Result := nil;

  if not FileExists(AFileName) then
    Exit;

  try

    S := TFile.ReadAllText(AFileName,
                           TEncoding.UTF8);
    Result := TJSONObject.ParseJSONValue(S) as TJSONObject;
  except

    Result := nil;
  end;
end;

end.
