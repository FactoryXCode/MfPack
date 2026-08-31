// FactoryX
//
// MfMftExplorer - beginner-oriented Media Foundation Transform explorer.
//
// This sample calls MFTEnumEx directly. It shows the ownership
// rules which are easy to miss: release every returned IMFActivate interface,
// then free the array itself with CoTaskMemFree.

// FactoryX
//
// Copyright �2003 - 2019 by FactoryX, Netherlands/Australia
//
// Project: Media Foundation - MFPack - Samples
// Project location: http://sourceforge.net/projects/MFPack
// Module: Form.Main.pas
// Kind: Pascal Unit
// Release date: 05-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Beginner-oriented Media Foundation Transform explorer.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or later.
//          Note:
//          This sample calls MFTEnumEx directly. It shows the ownership
//          rules which are easy to miss: release every returned IMFActivate interface,
//          then free the array itself with CoTaskMemFree to prevent memory leaks.
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
// =============================================================================
// Source: Microsoft Learn.
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
unit Form.Main;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Messages,
  WinApi.ComBaseApi,
  {System}
  System.Classes,
  System.SysUtils,
  {Vcl}
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Clipbrd,
  {DirectX}
  WinApi.DirectX.DXGI,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.CodecApi,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfTransform;

type
  TStringArray = array of string;
  TDwordArray = array of DWORD;

  TMftEntry = record
    FriendlyName: string;
    ClassId: TGUID;
    HardwareUrl: string;
    AdapterName: string;
    AdapterLuid: string;
    Activate: IMFActivate;
    InputTypes: TStringArray;
    OutputTypes: TStringArray;
  end;

  TAdapterInfo = record
    Name: string;
    AdapterLuid: LUID;
    IsSoftware: Boolean;
    RawMftCount: Integer;
    UniqueMftCount: Integer;
  end;

  TfrmMain = class(TForm)
    pnlOptions: TPanel;
    lblCategory: TLabel;
    lblScope: TLabel;
    cbCategory: TComboBox;
    cbScope: TComboBox;
    chkSortAndFilter: TCheckBox;
    btnEnumerate: TButton;
    btnInspect: TButton;
    btnCapabilityProbe: TButton;
    pnlBottom: TPanel;
    lblStatus: TLabel;
    btnCopyDetails: TButton;
    lvTransforms: TListView;
    splDetails: TSplitter;
    memDetails: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure btnEnumerateClick(Sender: TObject);
    procedure lvTransformsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnCopyDetailsClick(Sender: TObject);
    procedure btnInspectClick(Sender: TObject);
    procedure btnCapabilityProbeClick(Sender: TObject);

  private
    FEntries: array of TMftEntry;
    FAdapters: array of TAdapterInfo;

    function SelectedCategory(out ACategory: TGUID;
                              out AName: string): Boolean;

    function SelectedFlags(): UINT32;

    function ReadAllocatedString(const AAttributes: IMFAttributes;
                                 const AKey: TGUID): string;

    function ReadRegisteredTypes(const AAttributes: IMFAttributes;
                                 const AKey: TGUID): TStringArray;

    function HasRegisteredType(const AAttributes: IMFAttributes;
                               const AKey, AMajorType, ASubtype: TGUID): Boolean;

    function ActivationIdentity(const AActivate: IMFActivate): string;
    function DescribeRegisteredType(const AType: MFT_REGISTER_TYPE_INFO): string;
    function MajorTypeName(const AGuid: TGUID): string;
    function SubtypeName(const AGuid: TGUID): string;
    function PrintableFourCC(const AValue: Cardinal): string;
    function LuidToString(const ALuid: LUID): string;
    function LoadAdapters(): Boolean;

    procedure AddTransform(const AActivate: IMFActivate;
                           const AAdapterName, AAdapterLuid: string);

    function EnumerateHardwarePerAdapter(const ACategory: TGUID;
                                        const AFlags: UINT32): Integer;

    function EnumerateNormally(const ACategory: TGUID;
                               const AFlags: UINT32): Integer;

    function DescribeMediaType(const AMediaType: IMFMediaType): string;
    function InputStreamFlagsText(const AFlags: DWORD): string;
    function OutputStreamFlagsText(const AFlags: DWORD): string;

    function GetStreamIds(const ATransform: IMFTransform;
                          const AInputCount: DWORD;
                          const AOutputCount: DWORD;
                          out AInputIds: TDwordArray;
                          out AOutputIds: TDwordArray): HResult;

    procedure ListAvailableTypes(const ATransform: IMFTransform;
                                 const AStreamId: DWORD;
                                 const AInput: Boolean;
                                 const ALines: TStrings);

    procedure InspectTransform(const AIndex: Integer);

    function TestHevcProfile(const ATransform: IMFTransform;
                             const AInputStreamId, AOutputStreamId: DWORD;
                             const AInputSubtype: TGUID;
                             const AProfile: UINT32;
                             out AOutputHr: HResult;
                             out AInputHr: HResult): Boolean;

    procedure ProbeActivation(const AEntry: TMftEntry;
                              const ACategoryIndex: Integer;
                              const ALines: TStrings);

    procedure ProbeSelectedCategory();
    procedure EnumerateTransforms();
    procedure ShowDetails(const AIndex: Integer);
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  cbCategory.Items.Add('Video decoders');
  cbCategory.Items.Add('Video encoders');
  cbCategory.Items.Add('Video effects');
  cbCategory.Items.Add('Video processors');
  cbCategory.Items.Add('Video renderer effects');
  cbCategory.Items.Add('Audio decoders');
  cbCategory.Items.Add('Audio encoders');
  cbCategory.Items.Add('Audio effects');
  cbCategory.Items.Add('Demultiplexers');
  cbCategory.Items.Add('Multiplexers');
  cbCategory.Items.Add('Encryptors');
  cbCategory.Items.Add('Other transforms');
  cbCategory.ItemIndex := 0;

  cbScope.Items.Add('All registered MFTs');
  cbScope.Items.Add('Synchronous software MFTs');
  cbScope.Items.Add('Asynchronous software MFTs');
  cbScope.Items.Add('Hardware MFTs');
  cbScope.Items.Add('Field-of-use MFTs');
  cbScope.Items.Add('Local MFTs');
  cbScope.Items.Add('Transcode-only MFTs');
  cbScope.ItemIndex := 0;

  EnumerateTransforms();
end;

function TfrmMain.SelectedCategory(out ACategory: TGUID;
                                   out AName: string): Boolean;
begin

  Result := True;

  AName := cbCategory.Text;
  case cbCategory.ItemIndex of
    0: ACategory := MFT_CATEGORY_VIDEO_DECODER;
    1: ACategory := MFT_CATEGORY_VIDEO_ENCODER;
    2: ACategory := MFT_CATEGORY_VIDEO_EFFECT;
    3: ACategory := MFT_CATEGORY_VIDEO_PROCESSOR;
    4: ACategory := MFT_CATEGORY_VIDEO_RENDERER_EFFECT;
    5: ACategory := MFT_CATEGORY_AUDIO_DECODER;
    6: ACategory := MFT_CATEGORY_AUDIO_ENCODER;
    7: ACategory := MFT_CATEGORY_AUDIO_EFFECT;
    8: ACategory := MFT_CATEGORY_DEMULTIPLEXER;
    9: ACategory := MFT_CATEGORY_MULTIPLEXER;
    10: ACategory := MFT_CATEGORY_ENCRYPTOR;
    11: ACategory := MFT_CATEGORY_OTHER;
  else
    Result := False;
  end;
end;


function TfrmMain.SelectedFlags(): UINT32;
begin

  case cbScope.ItemIndex of
    1: Result := MFT_ENUM_FLAG_SYNCMFT;
    2: Result := MFT_ENUM_FLAG_ASYNCMFT;
    3: Result := MFT_ENUM_FLAG_HARDWARE;
    4: Result := MFT_ENUM_FLAG_FIELDOFUSE;
    5: Result := MFT_ENUM_FLAG_LOCALMFT;
    6: Result := MFT_ENUM_FLAG_TRANSCODE_ONLY;
  else
    Result := MFT_ENUM_FLAG_ALL;
  end;

  if chkSortAndFilter.Checked then
    Result := Result or MFT_ENUM_FLAG_SORTANDFILTER;
end;


function TfrmMain.ReadAllocatedString(const AAttributes: IMFAttributes;
                                      const AKey: TGUID): string;
var
  Value: LPWSTR;
  Length: UINT32;

begin

  Result := '';
  Value := nil;
  Length := 0;

  if Assigned(AAttributes) and
     SUCCEEDED(AAttributes.GetAllocatedString(AKey,
                                              Value,
                                              Length)) then
    try
      if Assigned(Value) then
        Result := string(Value);
    finally
      CoTaskMemFree(Value);
    end;
end;


function TfrmMain.PrintableFourCC(const AValue: Cardinal): string;
var
  C1: Byte;
  C2: Byte;
  C3: Byte;
  C4: Byte;

begin

  C1 := Byte(AValue and $FF);
  C2 := Byte((AValue shr 8) and $FF);
  C3 := Byte((AValue shr 16) and $FF);
  C4 := Byte((AValue shr 24) and $FF);

  if (C1 >= 32) and (C1 <= 126) and
     (C2 >= 32) and (C2 <= 126) and
     (C3 >= 32) and (C3 <= 126) and
     (C4 >= 32) and (C4 <= 126) then
    Result := Char(C1) + Char(C2) + Char(C3) + Char(C4)
  else
    Result := '';
end;


function TfrmMain.LuidToString(const ALuid: LUID): string;
begin

  Result := Format('0x%.8x:%.8x',
                   [Cardinal(ALuid.HighPart), ALuid.LowPart]);
end;


function TfrmMain.LoadAdapters: Boolean;
var
  Factory: IDXGIFactory1;
  FactoryPointer: Pointer;
  Adapter: IDXGIAdapter1;
  Desc: DXGI_ADAPTER_DESC1;
  Index: UINT;
  Hr: HResult;
  ItemIndex: Integer;

begin

  Result := False;
  SetLength(FAdapters,
            0);
  Factory := nil;
  FactoryPointer := nil;

  Hr := CreateDXGIFactory1(IID_IDXGIFactory1, FactoryPointer);
  if FAILED(Hr) then
    Exit;

  // CreateDXGIFactory1 returns one owned interface reference through void**.
  // Transfer that reference into the Delphi interface variable.
  PPointer(@Factory)^ := FactoryPointer;

  Index := 0;

  while True do
    begin
      Adapter := nil;
      Hr := Factory.EnumAdapters1(Index,
                                  Adapter);
      if FAILED(Hr) then
        Break;

      FillChar(Desc,
               SizeOf(Desc),
               0);

      if SUCCEEDED(Adapter.GetDesc1(Desc)) then
        begin
          ItemIndex := Length(FAdapters);
          SetLength(FAdapters, ItemIndex + 1);
          FAdapters[ItemIndex].Name := string(PWideChar(@Desc.Description[0]));
          FAdapters[ItemIndex].AdapterLuid := Desc.AdapterLuid;
          FAdapters[ItemIndex].IsSoftware := ((Desc.Flags and DXGI_ADAPTER_FLAG_SOFTWARE) <> 0);
        end;
      Inc(Index);
    end;

  Result := (Length(FAdapters) > 0);
end;


function TfrmMain.MajorTypeName(const AGuid: TGUID): string;
begin

  if IsEqualGUID(AGuid,
                 MFMediaType_Video) then
    Result := 'Video'
  else
    if IsEqualGUID(AGuid,
                   MFMediaType_Audio) then
      Result := 'Audio'
    else
      Result := GUIDToString(AGuid);
end;


function TfrmMain.SubtypeName(const AGuid: TGUID): string;
const
  MediaSubtypeD4: array[0..7] of Byte = ($80, $00, $00, $AA, $00, $38, $9B, $71);

var
  I: Integer;
  IsMediaSubtype: Boolean;

begin

  IsMediaSubtype := (AGuid.D2 = 0) and (AGuid.D3 = $0010);

  for I := 0 to 7 do
    IsMediaSubtype := IsMediaSubtype and (AGuid.D4[I] = MediaSubtypeD4[I]);

  if IsMediaSubtype then
    begin
      Result := PrintableFourCC(AGuid.D1);
      if (Result <> '') then
        Exit;

      case AGuid.D1 of
        $0001: Result := 'PCM';
        $0003: Result := 'IEEE float';
        $0014: Result := 'RGB24';
        $0015: Result := 'ARGB32';
        $0016: Result := 'RGB32';
        $0017: Result := 'RGB565';
        $0018: Result := 'RGB555';
        $0029: Result := 'RGB8';
        $0055: Result := 'MP3';
        $00FF: Result := 'AAC';
      else
        Result := Format('format tag 0x%.4x', [AGuid.D1]);
      end;
    end
  else
    Result := GUIDToString(AGuid);
end;


function TfrmMain.DescribeRegisteredType(const AType: MFT_REGISTER_TYPE_INFO): string;
begin

  Result := MajorTypeName(AType.guidMajorType) + ' / ' +
            SubtypeName(AType.guidSubtype) + '  ' +
            GUIDToString(AType.guidSubtype);
end;


function TfrmMain.ReadRegisteredTypes(const AAttributes: IMFAttributes;
                                      const AKey: TGUID): TStringArray;
var
  Blob: TBytes;
  BlobSize: UINT32;
  ActualSize: UINT32;
  Count: Integer;
  I: Integer;
  TypeInfo: PMFT_REGISTER_TYPE_INFO;

begin

  SetLength(Result,
            0);
  BlobSize := 0;

  if not Assigned(AAttributes) or
     FAILED(AAttributes.GetBlobSize(AKey,
                                    BlobSize)) or
     (BlobSize = 0) then
    Exit;

  if ((BlobSize mod SizeOf(MFT_REGISTER_TYPE_INFO)) <> 0) then
    Exit;

  SetLength(Blob,
            BlobSize);
  ActualSize := 0;

  if FAILED(AAttributes.GetBlob(AKey,
                                @Blob[0],
                                BlobSize,
                                @ActualSize)) then
    Exit;

  Count := ActualSize div SizeOf(MFT_REGISTER_TYPE_INFO);
  SetLength(Result,
            Count);

  for I := 0 to Count - 1 do
    begin
      TypeInfo := PMFT_REGISTER_TYPE_INFO(
        NativeUInt(@Blob[0]) + NativeUInt(I * SizeOf(MFT_REGISTER_TYPE_INFO)));
      Result[I] := DescribeRegisteredType(TypeInfo^);
    end;
end;


function TfrmMain.HasRegisteredType(const AAttributes: IMFAttributes;
                                    const AKey: TGUID;
                                    const AMajorType: TGUID;
                                    const ASubtype: TGUID): Boolean;
var
  Blob: TBytes;
  BlobSize: UINT32;
  ActualSize: UINT32;
  Count: Integer;
  I: Integer;
  TypeInfo: PMFT_REGISTER_TYPE_INFO;

begin

  Result := False;
  BlobSize := 0;

  if not Assigned(AAttributes) or
     FAILED(AAttributes.GetBlobSize(AKey, BlobSize)) or
     (BlobSize = 0) or
     ((BlobSize mod SizeOf(MFT_REGISTER_TYPE_INFO)) <> 0) then
    Exit;

  SetLength(Blob,
            BlobSize);
  ActualSize := 0;

  if FAILED(AAttributes.GetBlob(AKey,
                                @Blob[0],
                                BlobSize,
                                @ActualSize)) then
    Exit;

  Count := ActualSize div SizeOf(MFT_REGISTER_TYPE_INFO);

  for I := 0 to Count - 1 do
    begin
      TypeInfo := PMFT_REGISTER_TYPE_INFO(NativeUInt(@Blob[0]) + NativeUInt(I * SizeOf(MFT_REGISTER_TYPE_INFO)));
      if IsEqualGUID(TypeInfo^.guidMajorType,
                     AMajorType) and
         IsEqualGUID(TypeInfo^.guidSubtype,
                     ASubtype) then
        Exit(True);
    end;
end;


function TfrmMain.ActivationIdentity(const AActivate: IMFActivate): string;
var
  ClassId: TGUID;
  HardwareUrl: string;

begin

  Result := '';
  if not Assigned(AActivate) then
    Exit;

  FillChar(ClassId,
           SizeOf(ClassId),
           0);

  if FAILED(AActivate.GetGUID(MFT_TRANSFORM_CLSID_Attribute,
                              ClassId)) then
    Exit;

  Result := GUIDToString(ClassId);
  HardwareUrl := ReadAllocatedString(AActivate,
                                     MFT_ENUM_HARDWARE_URL_Attribute);
  // Preserve genuinely distinct hardware registrations that share a CLSID.
  // Software MFTs and drivers without a symbolic link use their CLSID as the
  // stable identity.
  if (HardwareUrl <> '') then
    Result := Result + '|' + HardwareUrl;
end;


procedure TfrmMain.AddTransform(const AActivate: IMFActivate;
                                const AAdapterName, AAdapterLuid: string);
var
  EntryIndex: Integer;
  Item: TListItem;

begin

  EntryIndex := Length(FEntries);
  SetLength(FEntries,
            EntryIndex + 1);

  FEntries[EntryIndex].FriendlyName := ReadAllocatedString(AActivate,
                                                           MFT_FRIENDLY_NAME_Attribute);
  if (FEntries[EntryIndex].FriendlyName = '') then
    FEntries[EntryIndex].FriendlyName := '(unnamed MFT)';

  FillChar(FEntries[EntryIndex].ClassId,
           SizeOf(TGUID),
           0);

  AActivate.GetGUID(MFT_TRANSFORM_CLSID_Attribute,
                    FEntries[EntryIndex].ClassId);

  FEntries[EntryIndex].HardwareUrl := ReadAllocatedString(AActivate,
                                                          MFT_ENUM_HARDWARE_URL_Attribute);

  FEntries[EntryIndex].Activate := AActivate;
  FEntries[EntryIndex].AdapterName := AAdapterName;
  FEntries[EntryIndex].AdapterLuid := AAdapterLuid;

  FEntries[EntryIndex].InputTypes := ReadRegisteredTypes(AActivate,
                                                         MFT_INPUT_TYPES_Attributes);
  FEntries[EntryIndex].OutputTypes := ReadRegisteredTypes(AActivate,
                                                          MFT_OUTPUT_TYPES_Attributes);

  Item := lvTransforms.Items.Add;
  Item.Caption := FEntries[EntryIndex].FriendlyName;
  Item.SubItems.Add(GUIDToString(FEntries[EntryIndex].ClassId));
  Item.SubItems.Add(FEntries[EntryIndex].AdapterName);

  if (FEntries[EntryIndex].HardwareUrl <> '') then
    Item.SubItems.Add('Yes')
  else
    Item.SubItems.Add('No');

  Item.SubItems.Add(IntToStr(Length(FEntries[EntryIndex].InputTypes)));
  Item.SubItems.Add(IntToStr(Length(FEntries[EntryIndex].OutputTypes)));
end;


function TfrmMain.EnumerateNormally(const ACategory: TGUID;
                                    const AFlags: UINT32): Integer;
var
  Activates: PIMFActivateArray;
  Count: UINT32;
  I: Integer;
  Hr: HResult;
  Seen: TStringList;
  Identity: string;

begin

  Result := -1;
  Activates := nil;
  Count := 0;

  Hr := MFTEnumEx(ACategory,
                  AFlags,
                  nil,
                  nil,
                  Activates,
                  Count);
  if FAILED(Hr) then
    begin
      lblStatus.Caption := Format('MFTEnumEx failed: HRESULT 0x%.8x',
                                  [Cardinal(Hr)]);
      Exit;
    end;

  Result := 0;
  Seen := TStringList.Create();

  try
    Seen.CaseSensitive := False;
    Seen.Sorted := True;
    Seen.Duplicates := dupIgnore;

    for I := 0 to Integer(Count) - 1 do
      begin
        Identity := ActivationIdentity(Activates^[I]);
        if (Identity <> '') and (Seen.IndexOf(Identity) >= 0) then
          Continue;
        if (Identity <> '') then
          Seen.Add(Identity);

        AddTransform(Activates^[I],
                     '',
                     '');
        Inc(Result);
      end;

  finally
    Seen.Free;
    if Assigned(Activates) then
      begin
        for I := 0 to Integer(Count) - 1 do
          Activates^[I] := nil;
        CoTaskMemFree(Activates);
      end;
  end;
end;


function TfrmMain.EnumerateHardwarePerAdapter(const ACategory: TGUID;
                                              const AFlags: UINT32): Integer;
var
  Attributes: IMFAttributes;
  Activates: PIMFActivateArray;
  Count: UINT32;
  AdapterIndex: Integer;
  I: Integer;
  Hr: HResult;
  Seen: TStringList;
  Identity: string;

begin

  Result := 0;
  if not LoadAdapters then
    begin
      lblStatus.Caption := 'No DXGI video adapters were found.';
      Exit(-1);
    end;

  Seen := TStringList.Create();

  try
    Seen.CaseSensitive := False;
    Seen.Sorted := True;
    Seen.Duplicates := dupIgnore;
    for AdapterIndex := 0 to Length(FAdapters) - 1 do
      begin
        if FAdapters[AdapterIndex].IsSoftware then
          Continue;

        Seen.Clear;
        FAdapters[AdapterIndex].RawMftCount := 0;
        FAdapters[AdapterIndex].UniqueMftCount := 0;

        Attributes := nil;
        Hr := MFCreateAttributes(Attributes,
                                 1);
        if FAILED(Hr) then
          begin
            lblStatus.Caption := Format('MFCreateAttributes failed: 0x%.8x',
                                        [Cardinal(Hr)]);
            Exit(-1);
          end;

        // MFT_ENUM_ADAPTER_LUID is an LUID-valued blob, not an MF UINT64.
        // The payload is eight bytes in both cases, but the attribute type is
        // significant to MFTEnum2.
        Hr := Attributes.SetBlob(MFT_ENUM_ADAPTER_LUID,
                                 PUINT8(@FAdapters[AdapterIndex].AdapterLuid),
                                 SizeOf(LUID));
        if FAILED(Hr) then
          begin
            lblStatus.Caption := Format('Setting adapter LUID failed: 0x%.8x',
                                        [Cardinal(Hr)]);
            Exit(-1);
          end;

        Activates := nil;
        Count := 0;

        Hr := MFTEnum2(ACategory,
                       AFlags or MFT_ENUM_FLAG_HARDWARE,
                       nil,
                       nil,
                       Attributes,
                       Activates,
                       Count);
        if FAILED(Hr) then
          begin
            lblStatus.Caption := Format('MFTEnum2 failed for %s: 0x%.8x',
                                        [FAdapters[AdapterIndex].Name, Cardinal(Hr)]);
            Exit(-1);
          end;
        FAdapters[AdapterIndex].RawMftCount := Count;

        try
          for I := 0 to Integer(Count) - 1 do
            begin
              Identity := ActivationIdentity(Activates^[I]);
              if (Identity <> '') and (Seen.IndexOf(Identity) >= 0) then
                Continue;
              if (Identity <> '') then
                Seen.Add(Identity);
              AddTransform(Activates^[I],
                           FAdapters[AdapterIndex].Name,
                           LuidToString(FAdapters[AdapterIndex].AdapterLuid));
              Inc(Result);
              Inc(FAdapters[AdapterIndex].UniqueMftCount);
            end;
        finally
          if Assigned(Activates) then
            begin
              for I := 0 to Integer(Count) - 1 do
                Activates^[I] := nil;
              CoTaskMemFree(Activates);
            end;
        end;
      end;
  finally
    Seen.Free;
  end;
end;


function TfrmMain.DescribeMediaType(const AMediaType: IMFMediaType): string;
var
  MajorType: TGUID;
  Subtype: TGUID;
  PackedValue: UINT64;
  Value1: UINT32;
  Value2: UINT32;
  Channels: UINT32;
  SampleRate: UINT32;
  BitsPerSample: UINT32;

begin

  FillChar(MajorType,
           SizeOf(MajorType),
           0);

  FillChar(Subtype,
           SizeOf(Subtype),
           0);

  if FAILED(AMediaType.GetGUID(MF_MT_MAJOR_TYPE, MajorType)) then
    Result := '(major type not set)'
  else
    Result := MajorTypeName(MajorType);

  if SUCCEEDED(AMediaType.GetGUID(MF_MT_SUBTYPE, Subtype)) then
    Result := Result + ' / ' + SubtypeName(Subtype) + '  ' +
              GUIDToString(Subtype)
  else
    Result := Result + ' / (subtype not set)';

  PackedValue := 0;

  if IsEqualGUID(MajorType,
                 MFMediaType_Video) and
     SUCCEEDED(AMediaType.GetUINT64(MF_MT_FRAME_SIZE,
                                    PackedValue)) then
    begin
      Value1 := UINT32(PackedValue shr 32);
      Value2 := UINT32(PackedValue and $FFFFFFFF);
      Result := Result + Format('  %dx%d', [Value1, Value2]);
    end;

  PackedValue := 0;
  if IsEqualGUID(MajorType, MFMediaType_Video) and SUCCEEDED(AMediaType.GetUINT64(MF_MT_FRAME_RATE,
                                                                                  PackedValue)) then
    begin
      Value1 := UINT32(PackedValue shr 32);
      Value2 := UINT32(PackedValue and $FFFFFFFF);
      if (Value2 <> 0) then
        Result := Result + Format('  %d/%d fps', [Value1, Value2]);
    end;

  if IsEqualGUID(MajorType,
                 MFMediaType_Audio) then
    begin
      Channels := 0;
      SampleRate := 0;
      BitsPerSample := 0;

      AMediaType.GetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                           Channels);
      AMediaType.GetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                           SampleRate);
      AMediaType.GetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                           BitsPerSample);

      if (Channels <> 0) then
        Result := Result + Format('  %d channel(s)',
                                 [Channels]);

      if (SampleRate <> 0) then
        Result := Result + Format('  %d Hz',
                                  [SampleRate]);
      if (BitsPerSample <> 0) then
        Result := Result + Format('  %d-bit',
                                  [BitsPerSample]);
    end;
end;


function TfrmMain.InputStreamFlagsText(const AFlags: DWORD): string;
begin

  Result := Format('0x%.8x', [AFlags]);

  if ((AFlags and MFT_INPUT_STREAM_WHOLE_SAMPLES) <> 0) then
    Result := Result + ' WHOLE_SAMPLES';

  if ((AFlags and MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER) <> 0) then
    Result := Result + ' SINGLE_SAMPLE_PER_BUFFER';

  if ((AFlags and MFT_INPUT_STREAM_FIXED_SAMPLE_SIZE) <> 0) then
    Result := Result + ' FIXED_SAMPLE_SIZE';

  if ((AFlags and MFT_INPUT_STREAM_HOLDS_BUFFERS) <> 0) then
    Result := Result + ' HOLDS_BUFFERS';

  if ((AFlags and MFT_INPUT_STREAM_DOES_NOT_ADDREF) <> 0) then
    Result := Result + ' DOES_NOT_ADDREF';

  if ((AFlags and MFT_INPUT_STREAM_REMOVABLE) <> 0) then
    Result := Result + ' REMOVABLE';

  if ((AFlags and MFT_INPUT_STREAM_OPTIONAL) <> 0) then
    Result := Result + ' OPTIONAL';

  if ((AFlags and MFT_INPUT_STREAM_PROCESSES_IN_PLACE) <> 0) then
    Result := Result + ' PROCESSES_IN_PLACE';
end;


function TfrmMain.OutputStreamFlagsText(const AFlags: DWORD): string;
begin

  Result := Format('0x%.8x',
                   [AFlags]);

  if ((AFlags and MFT_OUTPUT_STREAM_WHOLE_SAMPLES) <> 0) then
    Result := Result + ' WHOLE_SAMPLES';

  if ((AFlags and MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER) <> 0) then
    Result := Result + ' SINGLE_SAMPLE_PER_BUFFER';

  if ((AFlags and MFT_OUTPUT_STREAM_FIXED_SAMPLE_SIZE) <> 0) then
    Result := Result + ' FIXED_SAMPLE_SIZE';

  if ((AFlags and MFT_OUTPUT_STREAM_DISCARDABLE) <> 0) then
    Result := Result + ' DISCARDABLE';

  if ((AFlags and MFT_OUTPUT_STREAM_OPTIONAL) <> 0) then
    Result := Result + ' OPTIONAL';

  if ((AFlags and MFT_OUTPUT_STREAM_PROVIDES_SAMPLES) <> 0) then
    Result := Result + ' PROVIDES_SAMPLES';

  if ((AFlags and MFT_OUTPUT_STREAM_CAN_PROVIDE_SAMPLES) <> 0) then
    Result := Result + ' CAN_PROVIDE_SAMPLES';

  if ((AFlags and MFT_OUTPUT_STREAM_LAZY_READ) <> 0) then
    Result := Result + ' LAZY_READ';

  if ((AFlags and MFT_OUTPUT_STREAM_REMOVABLE) <> 0) then
    Result := Result + ' REMOVABLE';
end;


function TfrmMain.GetStreamIds(const ATransform: IMFTransform;
                               const AInputCount, AOutputCount: DWORD;
                               out AInputIds: TDwordArray;
                               out AOutputIds: TDwordArray): HResult;
var
  InputPointer: WinApi.WinApiTypes.PDWORD;
  OutputPointer: WinApi.WinApiTypes.PDWORD;
  I: Integer;

begin

  SetLength(AInputIds,
            AInputCount);

  SetLength(AOutputIds,
            AOutputCount);

  InputPointer := nil;
  OutputPointer := nil;

  if (AInputCount > 0) then
    InputPointer := WinApi.WinApiTypes.PDWORD(@AInputIds[0]);

  if (AOutputCount > 0) then
    OutputPointer := WinApi.WinApiTypes.PDWORD(@AOutputIds[0]);

  Result := ATransform.GetStreamIDs(AInputCount,
                                    InputPointer,
                                    AOutputCount,
                                    OutputPointer);
  if Result = E_NOTIMPL then
    begin
      for I := 0 to Integer(AInputCount) - 1 do
        AInputIds[I] := I;

      for I := 0 to Integer(AOutputCount) - 1 do
        AOutputIds[I] := I;

      Result := S_OK;
    end;
end;


procedure TfrmMain.ListAvailableTypes(const ATransform: IMFTransform;
                                      const AStreamId: DWORD;
                                      const AInput: Boolean;
                                      const ALines: TStrings);
const
  MAX_RUNTIME_TYPES = 256;

var
  Hr: HResult;
  TypeIndex: DWORD;
  MediaType: IMFMediaType;
  Count: Integer;

begin

  Count := 0;

  for TypeIndex := 0 to MAX_RUNTIME_TYPES - 1 do
    begin
      MediaType := nil;

      if AInput then
        Hr := ATransform.GetInputAvailableType(AStreamId,
                                               TypeIndex,
                                               MediaType)
      else
        Hr := ATransform.GetOutputAvailableType(AStreamId,
                                                TypeIndex,
                                                MediaType);

      if (Hr = MF_E_NO_MORE_TYPES) then
        Break;

      if (Hr = MF_E_TRANSFORM_TYPE_NOT_SET) then
        begin
          ALines.Add('      Runtime types require the opposite stream type ' +
                     'to be selected first.');
          Exit;
        end;

      if FAILED(Hr) then
        begin
          ALines.Add(Format('      Query failed: HRESULT 0x%.8x',
                            [Cardinal(Hr)]));
          Exit;
        end;

      ALines.Add(Format('      %d. %s',
                        [TypeIndex + 1, DescribeMediaType(MediaType)]));
      Inc(Count);
    end;

  if (Count = 0) then
    ALines.Add('      (no runtime types returned)')
  else
    if (Count = MAX_RUNTIME_TYPES) then
      ALines.Add('      (list stopped at the 256-type safety limit)');
end;


procedure TfrmMain.InspectTransform(const AIndex: Integer);
var
  Hr: HResult;
  Transform: IMFTransform;
  Attributes: IMFAttributes;
  ShutdownHr: HResult;
  UnlockHr: HResult;
  AsyncValue: UINT32;
  InputMinimum: DWORD;
  InputMaximum: DWORD;
  OutputMinimum: DWORD;
  OutputMaximum: DWORD;
  InputCount: DWORD;
  OutputCount: DWORD;
  InputIds: TDwordArray;
  OutputIds: TDwordArray;
  InputInfo: MFT_INPUT_STREAM_INFO;
  OutputInfo: MFT_OUTPUT_STREAM_INFO;
  I: Integer;

begin

  ShowDetails(AIndex);
  memDetails.Lines.BeginUpdate();

  try
    memDetails.Lines.Add('');
    memDetails.Lines.Add(StringOfChar('=', 72));
    memDetails.Lines.Add('Activated IMFTransform inspection');
    memDetails.Lines.Add(StringOfChar('=', 72));

    if (AIndex < 0) or (AIndex >= Length(FEntries)) or
       not Assigned(FEntries[AIndex].Activate) then
      begin
        memDetails.Lines.Add('Activation object is not available.');
        Exit;
      end;

    Transform := nil;
    Hr := FEntries[AIndex].Activate.ActivateObject(IID_IMFTransform,
                                                   Pointer(Transform));
    if FAILED(Hr) then
      begin
        memDetails.Lines.Add(Format('ActivateObject failed: HRESULT 0x%.8x',
                                    [Cardinal(Hr)]));
        lblStatus.Caption := 'Transform activation failed.';
        Exit;
      end;

    try
      Attributes := nil;
      AsyncValue := 0;

      Hr := Transform.GetAttributes(Attributes);
      if SUCCEEDED(Hr) then
        begin
          Attributes.GetUINT32(MF_TRANSFORM_ASYNC,
                               AsyncValue);
          memDetails.Lines.Add(Format('Asynchronous MFT: %s',
            [BoolToStr(AsyncValue <> 0, True)]));

          if (AsyncValue <> 0) then
            begin
              UnlockHr := Attributes.SetUINT32(MF_TRANSFORM_ASYNC_UNLOCK,
                                               1);
              memDetails.Lines.Add(Format('Async unlock: HRESULT 0x%.8x',
                                          [Cardinal(UnlockHr)]));
            end;
        end
      else
        memDetails.Lines.Add(Format('GetAttributes failed: HRESULT 0x%.8x',
                                    [Cardinal(Hr)]));

      InputMinimum := 0;
      InputMaximum := 0;
      OutputMinimum := 0;
      OutputMaximum := 0;

      Hr := Transform.GetStreamLimits(InputMinimum,
                                      InputMaximum,
                                      OutputMinimum,
                                      OutputMaximum);
      if SUCCEEDED(Hr) then
        memDetails.Lines.Add(Format('Stream limits: input %d..%d, output %d..%d',
                                     [InputMinimum, InputMaximum, OutputMinimum, OutputMaximum]));

      InputCount := 0;
      OutputCount := 0;

      Hr := Transform.GetStreamCount(InputCount,
                                     OutputCount);
      if FAILED(Hr) then
        begin
          memDetails.Lines.Add(Format('GetStreamCount failed: HRESULT 0x%.8x',
                                      [Cardinal(Hr)]));
          Exit;
        end;
      memDetails.Lines.Add(Format('Current streams: %d input, %d output',
                                  [InputCount, OutputCount]));

      Hr := GetStreamIds(Transform,
                         InputCount,
                         OutputCount,
                         InputIds,
                         OutputIds);
      if FAILED(Hr) then
        begin
          memDetails.Lines.Add(Format('GetStreamIDs failed: HRESULT 0x%.8x',
                                      [Cardinal(Hr)]));
          Exit;
        end;

      for I := 0 to Length(InputIds) - 1 do
        begin
          memDetails.Lines.Add('');
          memDetails.Lines.Add(Format('Input stream %d', [InputIds[I]]));

          FillChar(InputInfo,
                   SizeOf(InputInfo),
                   0);

          Hr := Transform.GetInputStreamInfo(InputIds[I],
                                             InputInfo);
          if SUCCEEDED(Hr) then
            begin
              memDetails.Lines.Add('  Flags: ' + InputStreamFlagsText(InputInfo.dwFlags));
              memDetails.Lines.Add(Format('  Buffer: size=%d, lookahead=%d, alignment=%d, latency=%d',
                                          [InputInfo.cbSize,
                                           InputInfo.cbMaxLookahead,
                                           InputInfo.cbAlignment,
                                           InputInfo.hnsMaxLatency]));
            end
          else
            memDetails.Lines.Add(Format('  GetInputStreamInfo failed: HRESULT 0x%.8x',
                                        [Cardinal(Hr)]));
          memDetails.Lines.Add('  Runtime available input types:');

          ListAvailableTypes(Transform,
                             InputIds[I],
                             True,
                             memDetails.Lines);
        end;

      for I := 0 to Length(OutputIds) - 1 do
        begin
          memDetails.Lines.Add('');
          memDetails.Lines.Add(Format('Output stream %d', [OutputIds[I]]));

          FillChar(OutputInfo,
                   SizeOf(OutputInfo),
                   0);

          Hr := Transform.GetOutputStreamInfo(OutputIds[I],
                                              OutputInfo);
          if SUCCEEDED(Hr) then
            begin
              memDetails.Lines.Add('  Flags: ' + OutputStreamFlagsText(OutputInfo.dwFlags));
              memDetails.Lines.Add(Format('  Buffer: size=%d, alignment=%d',
                                          [OutputInfo.cbSize, OutputInfo.cbAlignment]));
            end
          else
            memDetails.Lines.Add(Format('  GetOutputStreamInfo failed: HRESULT 0x%.8x', [Cardinal(Hr)]));

          memDetails.Lines.Add('  Runtime available output types:');

          ListAvailableTypes(Transform,
                             OutputIds[I],
                             False,
                             memDetails.Lines);
        end;

      lblStatus.Caption := 'Transform activated and inspected.';

    finally
      Attributes := nil;
      Transform := nil;
      ShutdownHr := FEntries[AIndex].Activate.ShutdownObject();
      memDetails.Lines.Add('');
      memDetails.Lines.Add(Format('ShutdownObject: HRESULT 0x%.8x',
                                  [Cardinal(ShutdownHr)]));
    end;
  finally
    memDetails.Lines.EndUpdate();
  end;
end;

function TfrmMain.TestHevcProfile(const ATransform: IMFTransform;
                                  const AInputStreamId, AOutputStreamId: DWORD;
                                  const AInputSubtype: TGUID;
                                  const AProfile: UINT32;
                                  out AOutputHr: HResult;
                                  out AInputHr: HResult): Boolean;
const
  PROBE_WIDTH = 1920;
  PROBE_HEIGHT = 1080;
  PROBE_FRAME_RATE = 30;
  PROBE_BIT_RATE = 8000000;

var
  Hr: HResult;
  OutputType: IMFMediaType;
  InputType: IMFMediaType;

begin

  Result := False;
  AOutputHr := E_FAIL;
  AInputHr := E_FAIL;
  OutputType := nil;
  InputType := nil;

  // Each profile probe starts from an unconfigured transform. The output type
  // must be selected first for a video encoder.
  ATransform.SetInputType(AInputStreamId,
                          nil,
                          0);

  ATransform.SetOutputType(AOutputStreamId,
                           nil,
                           0);

  try
    Hr := MFCreateMediaType(OutputType);
    if FAILED(Hr) then
      begin
        AOutputHr := Hr;
        Exit;
      end;

    Hr := OutputType.SetGUID(MF_MT_MAJOR_TYPE,
                             MFMediaType_Video);

    if SUCCEEDED(Hr) then
      Hr := OutputType.SetGUID(MF_MT_SUBTYPE,
                               MFVideoFormat_HEVC);

    if SUCCEEDED(Hr) then
      Hr := OutputType.SetUINT32(MF_MT_AVG_BITRATE,
                                 PROBE_BIT_RATE);

    if SUCCEEDED(Hr) then
      Hr := MFSetAttributeRatio(OutputType,
                                MF_MT_FRAME_RATE,
                                PROBE_FRAME_RATE,
                                1);

    if SUCCEEDED(Hr) then
      Hr := MFSetAttributeSize(OutputType,
                               MF_MT_FRAME_SIZE,
                               PROBE_WIDTH,
                               PROBE_HEIGHT);
    if SUCCEEDED(Hr) then
      Hr := OutputType.SetUINT32(MF_MT_INTERLACE_MODE,
                                 MFVideoInterlace_Progressive);

    if SUCCEEDED(Hr) then
      Hr := OutputType.SetUINT32(MF_MT_VIDEO_PROFILE,
                                 AProfile);

    if SUCCEEDED(Hr) then
      Hr := MFSetAttributeRatio(OutputType,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                1,
                                1);
    if FAILED(Hr) then
      begin
        AOutputHr := Hr;
        Exit;
      end;

    AOutputHr := ATransform.SetOutputType(AOutputStreamId,
                                          OutputType,
                                          0);
    if FAILED(AOutputHr) then
      Exit;

    Hr := MFCreateMediaType(InputType);
    if FAILED(Hr) then
      begin
        AInputHr := Hr;
        Exit;
      end;

    Hr := InputType.SetGUID(MF_MT_MAJOR_TYPE,
                            MFMediaType_Video);
    if SUCCEEDED(Hr) then
      Hr := InputType.SetGUID(MF_MT_SUBTYPE,
                              AInputSubtype);
    if SUCCEEDED(Hr) then
      Hr := InputType.SetUINT32(MF_MT_INTERLACE_MODE,
                                MFVideoInterlace_Progressive);
    if SUCCEEDED(Hr) then
      Hr := MFSetAttributeSize(InputType,
                               MF_MT_FRAME_SIZE,
                               PROBE_WIDTH,
                               PROBE_HEIGHT);
    if SUCCEEDED(Hr) then
      Hr := MFSetAttributeRatio(InputType,
                                MF_MT_FRAME_RATE,
                                PROBE_FRAME_RATE,
                                1);
    if SUCCEEDED(Hr) then
      Hr := MFSetAttributeRatio(InputType,
                                MF_MT_PIXEL_ASPECT_RATIO,
                                1,
                                1);
    if FAILED(Hr) then
      begin
        AInputHr := Hr;
        Exit;
      end;

    // TEST_ONLY verifies negotiation without committing the input type or
    // starting the encoder.
    AInputHr := ATransform.SetInputType(AInputStreamId,
                                        InputType,
                                        MFT_SET_TYPE_TEST_ONLY);
    Result := SUCCEEDED(AInputHr);

  finally
    ATransform.SetInputType(AInputStreamId,
                            nil,
                            0);

    ATransform.SetOutputType(AOutputStreamId,
                             nil,
                             0);
  end;
end;

procedure TfrmMain.ProbeActivation(const AEntry: TMftEntry;
                                   const ACategoryIndex: Integer;
                                   const ALines: TStrings);
var
  Hr: HResult;
  Transform: IMFTransform;
  Attributes: IMFAttributes;
  InputCount: DWORD;
  OutputCount: DWORD;
  InputIds: TDwordArray;
  OutputIds: TDwordArray;
  AsyncValue: UINT32;
  D3d11Aware: UINT32;
  OutputHr: HResult;
  InputHr: HResult;
  MainSupported: Boolean;
  Main10Supported: Boolean;
  I: Integer;

begin

  ALines.Add(AEntry.FriendlyName);
  ALines.Add('  CLSID: ' + GUIDToString(AEntry.ClassId));

  if (AEntry.AdapterName <> '') then
    ALines.Add('  Adapter: ' + AEntry.AdapterName + ' (' + AEntry.AdapterLuid + ')');

  ALines.Add(Format('  Registered types: %d input, %d output',
                    [Length(AEntry.InputTypes), Length(AEntry.OutputTypes)]));

  for I := 0 to Length(AEntry.InputTypes) - 1 do
    ALines.Add(Format('    In %d: %s', [I,
                                        AEntry.InputTypes[I]]));

  for I := 0 to Length(AEntry.OutputTypes) - 1 do
    ALines.Add(Format('    Out %d: %s',
                      [I, AEntry.OutputTypes[I]]));

  Transform := nil;

  Hr := AEntry.Activate.ActivateObject(IID_IMFTransform,
                                       Pointer(Transform));
  if FAILED(Hr) then
    begin
      ALines.Add(Format('  Activation failed: HRESULT 0x%.8x',
                        [Cardinal(Hr)]));
      Exit;
    end;

  try
    Attributes := nil;
    AsyncValue := 0;
    D3d11Aware := 0;

    Hr := Transform.GetAttributes(Attributes);
    if SUCCEEDED(Hr) then
      begin
        Attributes.GetUINT32(MF_TRANSFORM_ASYNC,
                             AsyncValue);
        if (AsyncValue <> 0) then
          Attributes.SetUINT32(MF_TRANSFORM_ASYNC_UNLOCK,
                               1);

        Attributes.GetUINT32(MF_SA_D3D11_AWARE,
                             D3d11Aware);
      end;

    ALines.Add(Format('  Asynchronous: %s',
                      [BoolToStr(AsyncValue <> 0, True)]));
    ALines.Add(Format('  D3D11-aware: %s',
                      [BoolToStr(D3d11Aware <> 0, True)]));

    if (D3d11Aware <> 0) then
      ALines.Add('  Same-adapter zero-copy candidate: Yes')
    else
      ALines.Add('  Same-adapter zero-copy candidate: Not advertised');

    InputCount := 0;
    OutputCount := 0;

    Hr := Transform.GetStreamCount(InputCount,
                                   OutputCount);
    if FAILED(Hr) then
      begin
        ALines.Add(Format('  GetStreamCount failed: HRESULT 0x%.8x',
                          [Cardinal(Hr)]));
        Exit;
      end;

    ALines.Add(Format('  Streams: %d input, %d output',
                      [InputCount, OutputCount]));

    Hr := GetStreamIds(Transform,
                       InputCount,
                       OutputCount,
                       InputIds,
                       OutputIds);
    if FAILED(Hr) then
      begin
        ALines.Add(Format('  Stream discovery failed: HRESULT 0x%.8x',
                          [Cardinal(Hr)]));
        Exit;
      end;

    for I := 0 to Length(InputIds) - 1 do
      begin
        ALines.Add(Format('  Runtime input types, stream %d:',
                          [InputIds[I]]));

        ListAvailableTypes(Transform,
                           InputIds[I],
                           True,
                           ALines);
      end;

    for I := 0 to Length(OutputIds) - 1 do
      begin
        ALines.Add(Format('  Runtime output types, stream %d:',
                          [OutputIds[I]]));
        ListAvailableTypes(Transform,
                           OutputIds[I],
                           False,
                           ALines);
      end;

    // Format-specific negotiation strategies are optional extensions to the
    // generic probe. HEVC encoding is the first one; other codecs retain the
    // complete generic activation and type report above.
    if (ACategoryIndex = 1) and
       (Length(InputIds) > 0) and (Length(OutputIds) > 0) and
       HasRegisteredType(AEntry.Activate,
                         MFT_OUTPUT_TYPES_Attributes,
                         MFMediaType_Video,
                         MFVideoFormat_HEVC) then
      begin
        ALines.Add('  Format-specific strategy: HEVC encoder');

        ALines.Add(Format('    Registered NV12 input: %s',
                          [BoolToStr(HasRegisteredType(AEntry.Activate,
                           MFT_INPUT_TYPES_Attributes,
                           MFMediaType_Video,
                           MFVideoFormat_NV12),
                           True)]));

        ALines.Add(Format('    Registered P010 input: %s',
          [BoolToStr(HasRegisteredType(AEntry.Activate,
                                       MFT_INPUT_TYPES_Attributes,
                                       MFMediaType_Video,
                                       MFVideoFormat_P010),
                                       True)]));

        MainSupported := TestHevcProfile(Transform,
                                         InputIds[0],
                                         OutputIds[0],
                                         MFVideoFormat_NV12,
                                         eAVEncH265VProfile_Main_420_8,
                                         OutputHr,
                                         InputHr);

        ALines.Add(Format('    Main / NV12 1080p30: %s',
                          [BoolToStr(MainSupported, True)]));

        if not MainSupported then
          ALines.Add(Format('      output HRESULT 0x%.8x, input HRESULT 0x%.8x',
                            [Cardinal(OutputHr), Cardinal(InputHr)]));

        Main10Supported := TestHevcProfile(Transform,
                                           InputIds[0],
                                           OutputIds[0],
                                           MFVideoFormat_P010,
                                           eAVEncH265VProfile_Main_420_10,
                                           OutputHr,
                                           InputHr);
        ALines.Add(Format('    Main10 / P010 1080p30: %s',
                          [BoolToStr(Main10Supported, True)]));

        if not Main10Supported then
          ALines.Add(Format('      output HRESULT 0x%.8x, input HRESULT 0x%.8x',
                            [Cardinal(OutputHr), Cardinal(InputHr)]));
      end
    else
      ALines.Add('  Format-specific strategy: generic inspection only');

  finally
    Attributes := nil;
    Transform := nil;
    Hr := AEntry.Activate.ShutdownObject;
    ALines.Add(Format('  ShutdownObject: HRESULT 0x%.8x',
                      [Cardinal(Hr)]));
  end;
end;


procedure TfrmMain.ProbeSelectedCategory();
var
  Category: TGUID;
  CategoryName: string;
  AdapterIndex: Integer;
  I: Integer;
  HardwareVideo: Boolean;

begin

  if not SelectedCategory(Category,
                          CategoryName) then
    Exit;

  // Re-enumerate with the current category, scope, and sort/filter choices so
  // the bulk report always describes exactly what is visible in the list.
  EnumerateTransforms();
  HardwareVideo := (cbScope.ItemIndex = 3) and
                   (cbCategory.ItemIndex >= 0) and
                   (cbCategory.ItemIndex <= 4);

  memDetails.Lines.BeginUpdate;

  try
    memDetails.Clear;
    memDetails.Lines.Add('MFT capability probe');
    memDetails.Lines.Add(StringOfChar('=', 72));
    memDetails.Lines.Add('Category: ' + CategoryName);
    memDetails.Lines.Add('Scope: ' + cbScope.Text);
    memDetails.Lines.Add(Format('Unique transforms selected: %d',
                                [Length(FEntries)]));
    memDetails.Lines.Add('Activation is explicit; no media samples are sent.');
    memDetails.Lines.Add('');

    if HardwareVideo then
      begin
        memDetails.Lines.Add('DXGI adapter query summary');

        for AdapterIndex := 0 to Length(FAdapters) - 1 do
          begin
            memDetails.Lines.Add('  ' + FAdapters[AdapterIndex].Name);
            memDetails.Lines.Add('    LUID ' + LuidToString(FAdapters[AdapterIndex].AdapterLuid));
            if FAdapters[AdapterIndex].IsSoftware then
              memDetails.Lines.Add('    Software adapter; skipped.')
            else
              begin
                memDetails.Lines.Add(Format('    Raw activation objects: %d',
                                            [FAdapters[AdapterIndex].RawMftCount]));

                memDetails.Lines.Add(Format('    Unique transforms: %d',
                                            [FAdapters[AdapterIndex].UniqueMftCount]));

                if (FAdapters[AdapterIndex].RawMftCount > FAdapters[AdapterIndex].UniqueMftCount) then
                  memDetails.Lines.Add(Format('    Duplicate activation objects: %d',
                                              [FAdapters[AdapterIndex].RawMftCount - FAdapters[AdapterIndex].UniqueMftCount]));
              end;
          end;
        memDetails.Lines.Add('');
      end;

    for I := 0 to Length(FEntries) - 1 do
      begin
        memDetails.Lines.Add(Format('Transform %d of %d',
                                    [I + 1, Length(FEntries)]));

        memDetails.Lines.Add(StringOfChar('-', 72));

        ProbeActivation(FEntries[I],
                        cbCategory.ItemIndex,
                        memDetails.Lines);
        memDetails.Lines.Add('');
      end;

    if Length(FEntries) = 0 then
      memDetails.Lines.Add('No transforms matched the selected category and scope.');

    lblStatus.Caption := Format('Capability probe completed: %d transform(s).',
                                [Length(FEntries)]);
  finally
    memDetails.Lines.EndUpdate;
  end;
end;


procedure TfrmMain.EnumerateTransforms;
var
  Category: TGUID;
  CategoryName: string;
  Flags: UINT32;
  Count: Integer;

begin

  if not SelectedCategory(Category,
                          CategoryName) then
    Exit;

  lvTransforms.Items.BeginUpdate();

  try
    lvTransforms.Items.Clear();
    memDetails.Clear;
    btnInspect.Enabled := False;
    SetLength(FEntries,
              0);

    Flags := SelectedFlags();

    // MFTEnum2 can associate hardware video MFTs with a particular DXGI
    // adapter. Audio and other categories are not DXGI-adapter resources and
    // continue to use the system-wide MFTEnumEx path.
    if (cbScope.ItemIndex = 3) and
       (cbCategory.ItemIndex >= 0) and
       (cbCategory.ItemIndex <= 4) then
      Count := EnumerateHardwarePerAdapter(Category,
                                           Flags)
    else
      Count := EnumerateNormally(Category,
                                 Flags);

    if Count < 0 then
      Exit;

    lblStatus.Caption := Format('%d transform(s) in %s.',
                                [Count, CategoryName]);

    if (lvTransforms.Items.Count > 0) then
      begin
        lvTransforms.Items[0].Selected := True;
        lvTransforms.Items[0].Focused := True;
        ShowDetails(0);
      end;

  finally
    lvTransforms.Items.EndUpdate();
  end;
end;


procedure TfrmMain.ShowDetails(const AIndex: Integer);
var
  I: Integer;

begin

  memDetails.Lines.BeginUpdate();

  try
    memDetails.Clear();
    if (AIndex < 0) or (AIndex >= Length(FEntries)) then
      Exit;

    memDetails.Lines.Add('Friendly name');
    memDetails.Lines.Add('  ' + FEntries[AIndex].FriendlyName);
    memDetails.Lines.Add('');
    memDetails.Lines.Add('CLSID');
    memDetails.Lines.Add('  ' + GUIDToString(FEntries[AIndex].ClassId));
    memDetails.Lines.Add('');
    memDetails.Lines.Add('Hardware registration URL');

    if (FEntries[AIndex].HardwareUrl <> '') then
      memDetails.Lines.Add('  ' + FEntries[AIndex].HardwareUrl)
    else
      memDetails.Lines.Add('  (not present)');

    memDetails.Lines.Add('');
    memDetails.Lines.Add('DXGI video adapter');

    if (FEntries[AIndex].AdapterName <> '') then
      begin
        memDetails.Lines.Add('  ' + FEntries[AIndex].AdapterName);
        memDetails.Lines.Add('  LUID ' + FEntries[AIndex].AdapterLuid);
      end
    else
      memDetails.Lines.Add('  (select Hardware MFTs scope for adapter attribution)');

    memDetails.Lines.Add('');
    memDetails.Lines.Add('Registered input types');

    if (Length(FEntries[AIndex].InputTypes) = 0) then
      memDetails.Lines.Add('  (not advertised by the registration)')
    else
      for I := 0 to Length(FEntries[AIndex].InputTypes) - 1 do
        memDetails.Lines.Add(Format('  %d. %s',
                                   [I + 1, FEntries[AIndex].InputTypes[I]]));

    memDetails.Lines.Add('');
    memDetails.Lines.Add('Registered output types');

    if (Length(FEntries[AIndex].OutputTypes) = 0) then
      memDetails.Lines.Add('  (not advertised by the registration)')
    else
      for I := 0 to Length(FEntries[AIndex].OutputTypes) - 1 do
        memDetails.Lines.Add(Format('  %d. %s',
                                   [I + 1, FEntries[AIndex].OutputTypes[I]]));

    memDetails.Lines.Add('');
    memDetails.Lines.Add('Note');
    memDetails.Lines.Add('  These are registration types. Runtime available types can differ');
    memDetails.Lines.Add('  Use Activate and inspect, or probe the selected category.');

  finally
    memDetails.Lines.EndUpdate;
  end;
end;


procedure TfrmMain.btnEnumerateClick(Sender: TObject);
begin

  EnumerateTransforms();
end;


procedure TfrmMain.lvTransformsSelectItem(Sender: TObject;
                                          Item: TListItem;
                                          Selected: Boolean);
begin

  if Selected and Assigned(Item) then
    begin
      btnInspect.Enabled := True;
      ShowDetails(Item.Index);
    end
  else
    if (lvTransforms.Selected = nil) then
      btnInspect.Enabled := False;
end;


procedure TfrmMain.btnInspectClick(Sender: TObject);
begin

  if Assigned(lvTransforms.Selected) then
    InspectTransform(lvTransforms.Selected.Index);
end;


procedure TfrmMain.btnCapabilityProbeClick(Sender: TObject);
begin

  ProbeSelectedCategory();
end;


procedure TfrmMain.btnCopyDetailsClick(Sender: TObject);
begin

  Clipboard.AsText := memDetails.Text;
end;

end.
