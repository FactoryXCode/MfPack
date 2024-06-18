// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfMediaTypeDebug.pas
// Kind: Pascal / Delphi unit
// Release date: 13-08-2022
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Code to view the contents of a media type (IMFMediaType) while debugging.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
// 26/02/2024 Tony                Updated Remarks How to use.
// 11/03/2024 Tony                Changed function GetGUIDNameConst, added parameter majortype.
//------------------------------------------------------------------------------
//
// Remarks: How to use:
//
//            1 Add {$IFDEF DEBUG} WinApi.MediaFoundationApi.MfMediaTypeDebug, {$ENDIF}
//              to your uses clause.
//
//            2 Create the class somewhere in your application code.
//              To get global acces to the class, use the declared FMediaTypeDebug var in this unit
//
//             {$IFDEF DEBUG}
//             FMediaTypeDebug := TMediaTypeDebug.Create();
//             {$ENDIF}
//
//            3 Check the contents and store to file.
//              {$IFDEF DEBUG}
//              FMediaTypeDebug.LogMediaType(pYourMediaType);
//
//              FMediaTypeDebug.SafeAllDebugResultsToOneFile('YourFileName.txt');
//              or
//              SFMediaTypeDebug.SafeDebugResultsToFile('Methodname',
//                                                      'YourFileName',
//                                                      '.FileExtension');
//              {$ENDIF}
//
//            4 When done, destroy the class.
//              {$IFDEF DEBUG}
//              FMediaTypeDebug.Free();
//              {$ENDIF}
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: Microsoft
//
// Copyright (c) Microsoft Corporation. All rights reserved.
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
unit WinApi.MediaFoundationApi.MfMediaTypeDebug;

interface

// {$DEFINE USE_EMBARCADERO_DEF}

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.UuIds,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Classes,
  {ActiveX}
  {$IFDEF USE_EMBARCADERO_DEF}
  WinApi.PropSys,
  WinApi.ActiveX,
  {$ELSE}
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropVarUtil,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  {$ENDIF}
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  {Vcl}
  Vcl.Forms;


type

  TMediaTypeDebug = class(TObject)
  private

    slDebug: TStringlist;
    sDebugDirectory: string;

    function LogAttributeValueByIndex(pAttributes: IMFAttributes;
                                      pIndex: DWord): HRESULT;
    function SpecialCaseAttributeValue(const pGuid: TGUID;
                                       const pPropVar: PROPVARIANT): HRESULT;
    procedure LogUINT32AsUINT64(const pPropVar: PROPVARIANT);
    function LogVideoArea(const pPropVar: PROPVARIANT): HRESULT;

    procedure DebugMsg(pFormat: string);

  public
    arDebug: TStreamContentsArray;


    constructor Create();
    destructor Destroy(); override;

    // Call this method to log.
    function LogMediaType(pType: IMFMediaType;
                          pMediaTypeName: string = ''): HRESULT;

    // Call this method to save all logs to one file.
    procedure SafeAllDebugResultsToOneFile(pFileName: string);

    // Call this method to save each log to a separate file.
    procedure SafeDebugResultsToFile(const pMethodName: string = '';
                                     const pFileName: string = 'MFMediaTypeDebug';
                                     const pExt: string = '.txt');

    property DebugResults: TStringList read slDebug;
    property DebugDirectory: string read sDebugDirectory write sDebugDirectory;
  end;

// Global accessible.
var
  FMediaTypeDebug: TMediaTypeDebug;

implementation


{PUBLIC}
constructor TMediaTypeDebug.Create();
begin
  inherited Create();
  slDebug := TStringlist.Create();
  SetLength(arDebug,
            0);
  sDebugDirectory := ExtractFilePath(Application.ExeName);
end;


destructor TMediaTypeDebug.Destroy();
begin
  if Assigned(arDebug) then
    begin
      SetLength(arDebug,
                0);
      arDebug := nil;
    end;
  if Assigned(slDebug) then
    FreeAndNil(slDebug);
  inherited Destroy();
end;


function TMediaTypeDebug.LogMediaType(pType: IMFMediaType;
                                      pMediaTypeName: string = ''): HRESULT;
var
  hr: HResult;
  unCount: UINT32;
  i: Integer;

label
  Return;

begin

  if not Assigned(pType) then
    begin
      hr := E_POINTER;
      goto Return;
    end;

  hr := pType.GetCount(unCount);
  if FAILED(hr) then
    goto Return;

  if (unCount = 0) then
    begin
        DebugMsg(StrToPWideChar(Format('Empty media type. %d',
                                       [unCount])));
        goto Return;
    end;

  slDebug.BeginUpdate;

  slDebug.Append('== BEGIN =====================================================' + #13);

  DebugMsg(Format('IMFMediaType var name: %s',
                  [pMediaTypeName]));

  for i := 0 to unCount -1 do
   begin
     hr := LogAttributeValueByIndex(pType,
                                    i);
     if FAILED(hr) then
       Break;
   end;

  // Add a separatorline when done.
  slDebug.Append('== END =======================================================' + #13);

  slDebug.EndUpdate;

Return:
  Result := hr;
end;


procedure TMediaTypeDebug.SafeAllDebugResultsToOneFile(pFileName: string);
begin
  slDebug.SaveToFile(sDebugDirectory + pFileName);
end;


procedure TMediaTypeDebug.SafeDebugResultsToFile(const pMethodName: string = '';
                                                 const pFileName: string = 'MFMediaTypeDebug';
                                                 const pExt: string = '.txt');
const
  scFormat = '%s/%s%s(%d)%s';

var
  i: Integer;
  sFileName: string;
  sMetName: string;

  bFileExists: Boolean;

begin
  i := 0;
  bFileExists := True;

  if pMethodName = '' then
    sMetName := ''
  else
    sMetName := Format('_%s',[pMethodName]);

  sFileName := Format(scFormat,
                      [sDebugDirectory,
                       pFileName,
                       sMetName,
                       i,
                       pExt]);

  while (bFileExists = True) do
    begin
      if FileExists(sFileName) then
        begin
          sFileName := Format(scFormat,
                              [sDebugDirectory,
                               pFileName,
                               sMetName,
                               i,
                               pExt]);
          Inc(i);
        end
      else
        bFileExists := False;
    end;

  // When you don't want to safe each item to file,
  // you should call property DebugResults.SafeToFile when done.
  slDebug.SaveToFile(sFileName);
  slDebug.Clear();

end;


{PRIVATE}


//
function TMediaTypeDebug.LogAttributeValueByIndex(pAttributes: IMFAttributes;
                                                  pIndex: DWord): HRESULT;
var
  hr: HResult;
  pwcGuidName: LPWSTR;
  pwcFormatTag: LPWSTR;
  pwcFOURCC: DWord;
  pwcFmtDesc: LPWSTR;

  pwcValGuidName: LPWSTR;
  pwcValFormatTag: LPWSTR;
  pwcValFOURCC: DWord;
  pwcValFmtDesc: LPWSTR;

  gdGuid: TGUID;
  majorGuid: TGUID;
  pvVar: PROPVARIANT;

label
  Return;

begin
  pwcGuidName := '';
  pwcFormatTag := '';
  pwcFOURCC := 0;
  pwcFmtDesc := '';

  pwcValGuidName := '';
  pwcValFormatTag := '';
  pwcValFOURCC := 0;
  pwcValFmtDesc := '';
  gdGuid := GUID_NULL;

  PropVariantInit(pvVar);

  hr := pAttributes.GetItemByIndex(pIndex,
                                   gdGuid,
                                   pvVar);
  if FAILED(hr) then
    goto Return;

  // MFMediaType_Video or MFMediaType_Audio
  hr := pAttributes.GetGUID(MF_MT_MAJOR_TYPE,
                            majorGuid);
  if FAILED(hr) then
    begin
       DebugMsg(Format('Unexpected error in function pAttributes.GetGUID. (HResult = %d)',
                       [hr]));
       goto Return;
    end;


  hr := GetGUIDNameConst(majorGuid,
                         gdGuid,
                         pwcGuidName,
                         pwcFormatTag,
                         pwcFOURCC,
                         pwcFmtDesc);

  if FAILED(hr) then
    begin
       DebugMsg(Format('Unexpected error in function GetGUIDNameConst. (HResult = %d)',
                       [hr]));
       goto Return;
    end;

  DebugMsg(Format('Guidname: %s',
                  [pwcGuidName]));

  hr := SpecialCaseAttributeValue(gdGuid,
                                  pvVar);
  if FAILED(hr) then
    goto Return;

  if (hr = S_FALSE) then
    begin
      case pvVar.vt of

        VT_UI4: DebugMsg(Format('Value: %d',
                                [pvVar.ulVal]));

        VT_UI8: DebugMsg(Format('Value: %d',
                               [pvVar.uhVal.QuadPart]));

        VT_R8:  DebugMsg(Format('Value: %f',
                                [pvVar.dblVal]));

        VT_CLSID: begin
                    hr := GetGUIDNameConst(majorGuid,
                                           pvVar.puuid^,
                                           pwcValGuidName,
                                           pwcValFormatTag,
                                           pwcValFOURCC,
                                           pwcValFmtDesc);
                    if SUCCEEDED(hr) then
                      begin
                        DebugMsg(Format('GuidName: %s',
                                        [pwcValGuidName]));
                        DebugMsg(Format('FormatTag: %s',
                                        [pwcValFormatTag]));
                        DebugMsg(Format('FOURCC: %d',
                                        [pwcValFOURCC]));
                        DebugMsg(Format('Description: %s',
                                        [pwcValFmtDesc]));
                      end;
                  end;

        VT_LPWSTR: DebugMsg(Format('Value: %s',
                                   [PChar(@pvVar.pwszVal)]));

        VT_VECTOR or VT_UI1 : DebugMsg('Value: Byte Array');

        VT_UNKNOWN: DebugMsg('Value: IUnknown');



        else
          DebugMsg(Format('Unexpected attribute type (vt = %d)',
                          [pvVar.vt]));
        end;
    end;

Return:
  PropVariantClear(pvVar);
  Result := hr;
end;


// Handle certain known special cases.
function TMediaTypeDebug.SpecialCaseAttributeValue(const pGuid: TGUID;
                                                   const pPropVar: PROPVARIANT): HRESULT;
var
  hr: HResult;

begin

  hr := S_OK;

  if (isEqualGuid(pGuid,
                  MF_MT_FRAME_RATE) or
      isEqualGuid(pGuid,
                  MF_MT_FRAME_RATE_RANGE_MAX) or
      isEqualGuid(pGuid,
                  MF_MT_FRAME_RATE_RANGE_MIN) or
      isEqualGuid(pGuid,
                  MF_MT_FRAME_SIZE) or
      isEqualGuid(pGuid,
                  MF_MT_PIXEL_ASPECT_RATIO)) then
    begin
      // Attributes that contain two packed 32-bit values.
      LogUINT32AsUINT64(pPropVar);
    end
  else if (isEqualGuid(pGuid,
                       MF_MT_GEOMETRIC_APERTURE) or
           isEqualGuid(pGuid,
                       MF_MT_MINIMUM_DISPLAY_APERTURE) or
           isEqualGuid(pGuid,
                       MF_MT_PAN_SCAN_APERTURE)) then
    begin
      // Attributes that has an MFVideoArea structure.
      hr := LogVideoArea(pPropVar);
    end
  else
    hr := S_FALSE;

  Result := hr;
end;

//
procedure TMediaTypeDebug.LogUINT32AsUINT64(const pPropVar: PROPVARIANT);
var
  uHigh: UINT32;
  uLow: UINT32;

begin
  Unpack2UINT32AsUINT64(pPropVar.uhVal.QuadPart,
                        uHigh,
                        uLow);

  DebugMsg(Format('%d x %d',
                  [uHigh, uLow]));
end;

//
function TMediaTypeDebug.LogVideoArea(const pPropVar: PROPVARIANT): HRESULT;
var
  pArea: MFVideoArea;

begin

  if (pPropVar.caub.cElems < SizeOf(MFVideoArea)) then
    Result := MF_E_BUFFERTOOSMALL
  else
    begin
{$POINTERMATH ON}
      pArea := MakeArea(pPropVar.caub.pElems[0],
                        pPropVar.caub.pElems[1],
                        pPropVar.caub.pElems[2],
                        pPropVar.caub.pElems[3]);
{$POINTERMATH OFF}

      DebugMsg(StrToPWideChar(Format('MFVideoArea: (OffsetX: %f, OffsetY: %f) (cx: %d, cy: %d)',
                                     [MFOffsetToFloat(pArea.OffsetX),
                                      MFOffsetToFloat(pArea.OffsetY),
                                      pArea.Area.cx,
                                      pArea.Area.cy])));
      Result := S_OK;
    end;
end;


//
procedure TMediaTypeDebug.DebugMsg(pFormat: string);
begin
{$IFDEF DEBUG}
  OutputDebugString(StrToPWideChar(Format('%s',
                                          [pFormat])));
{$ENDIF}

  slDebug.Append(Format('%d %s',
                        [slDebug.Count, pFormat]));
end;

end.
