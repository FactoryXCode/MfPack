// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfEmbeddedSubtitleReader.pas
// Kind: Pascal Unit
// Release date: 31-07-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Container-neutral reader for textual subtitle streams exposed
//              by a Media Foundation source. The reader imports cues into the
//              existing TMfTimedText model used by local playback and Cast.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
// 02/08/2026 Carmen              Added direct Matroska text-track fallback.
// 01/08/2026 Carmen              Added exact embedded-track selection and end-of-stream safety.
// 01/08/2026 Carmen              Robust stream classification and explicit Source Reader type selection.
// 31/07/2026 Carmen              Embedded SRT/UTF-8 and SSA/ASS subtitle import.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: TMfTimedText, TMfSubtitleCompositor
// Related projects: MfPackX320
//
// Known Issues: Bitmap subtitle streams (PGS/VobSub) are detected but are not
//               decoded. The native Microsoft MP4 source ignores non-audio/
//               video tracks, so MP4 timed-text requires a container adapter.
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
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
unit MfEmbeddedSubtitleReader;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.WinApiTypes,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.ObjBase,
  {System}
  System.SysUtils,
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfIdl,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfReadWrite,
  WinApi.MediaFoundationApi.MfMetLib,
  {Cast/Media}
  MfPCXConstants,
  TimedTextClass,
  MfMatroskaSubtitleReader;

type
  TMfEmbeddedSubtitleSource = (essMediaFoundation,
                               essMatroska);

  TMfEmbeddedSubtitleFormat = (esfUnknown,
                               esfSrt,
                               esfSsaAss,
                               esfGenericText,
                               esfVobSub,
                               esfPgs);

  TMfEmbeddedSubtitleTrackInfo = record
    Source: TMfEmbeddedSubtitleSource;
    StreamIndex: DWORD;
    NativeTypeIndex: DWORD;
    MajorType: TGUID;
    Subtype: TGUID;
    CodecId: string;
    Format: TMfEmbeddedSubtitleFormat;
    Language: string;
    Name: string;
    InitiallySelected: Boolean;
    Supported: Boolean;

    procedure Reset();
  end;

  TMfEmbeddedSubtitleTrackInfoArray = array of TMfEmbeddedSubtitleTrackInfo;

  TMfEmbeddedSubtitleReader = class(TObject)
  strict private
    class function CreateReader(const MediaFileName: WideString;
                                out Reader: IMFSourceReader): HRESULT; static;

    class function EnumerateReaderTracks(const Reader: IMFSourceReader;
                                         out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT; static;

    class function GetStreamAttributeString(const Reader: IMFSourceReader;
                                            const StreamIndex: DWORD;
                                            const AttributeKey: TGUID): string; static;

    class function GetDescriptorAttributeString(const Descriptor: IMFStreamDescriptor;
                                                const AttributeKey: TGUID): string; static;

    class function IsSubtitleMajorType(const MajorType: TGUID): Boolean; static;
    class function ClassifySubtype(const Subtype: TGUID): TMfEmbeddedSubtitleFormat; static;
    class function FormatName(const SubtitleFormat: TMfEmbeddedSubtitleFormat): string; static;
    class function TrackScore(const Track: TMfEmbeddedSubtitleTrackInfo;
                              const PreferredLanguage: string): Integer; static;
    class function ReadSampleBytes(const Sample: IMFSample;
                                   out Data: TBytes): HRESULT; static;

    class function DecodeTextBytes(const Data: TBytes): string; static;
    class function NormalizeSrtText(const Value: string): string; static;
    class function NormalizeSsaAssText(const Value: string): string; static;
    class function StripAssOverrideTags(const Value: string): string; static;
    class function TextAfterCommaCount(const Value: string;
                                       const CommaCount: Integer): string; static;

    class procedure NormalizeCueTimes(var Cues: TMfTimedTextCueArray); static;

    class function ResolveReaderNativeType(const Reader: IMFSourceReader;
                                           const Track: TMfEmbeddedSubtitleTrackInfo;
                                           out NativeType: IMFMediaType;
                                           out NativeTypeIndex: DWORD): HRESULT; static;

    class function ReadTrack(const Reader: IMFSourceReader;
                             const Track: TMfEmbeddedSubtitleTrackInfo;
                             out Cues: TMfTimedTextCueArray): HRESULT; static;

    class function MapMatroskaFormat(const Format: TMfMatroskaSubtitleFormat): TMfEmbeddedSubtitleFormat; static;
    class function EnumerateMatroskaTracks(const MediaFileName: WideString;
                                            out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT; static;
    class function EnumerateMediaSourceTracks(const MediaFileName: WideString;
                                               out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT; static;

    class function ImportMatroskaTextTrack(const MediaFileName: WideString;
                                           const Track: TMfEmbeddedSubtitleTrackInfo;
                                           const TimedText: TMfTimedText;
                                           const CancelEvent: THandle): HRESULT; static;
  public

    class function EnumeratePresentationTracks(const PresentationDescriptor: IMFPresentationDescriptor;
                                               out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT; static;

    class function EnumerateTracks(const MediaFileName: WideString;
                                   out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT; static;

    class function FindBestTextTrackFromList(const PreferredLanguage: string;
                                             const Tracks: TMfEmbeddedSubtitleTrackInfoArray;
                                             out SelectedTrack: TMfEmbeddedSubtitleTrackInfo): HRESULT; static;

    class function ImportTextTrack(const MediaFileName: WideString;
                                   const Track: TMfEmbeddedSubtitleTrackInfo;
                                   const TimedText: TMfTimedText;
                                   const CancelEvent: THandle = 0): HRESULT; static;

    class function LoadMatroskaTextTrackWindow(const MediaFileName: WideString;
                                               const Track: TMfEmbeddedSubtitleTrackInfo;
                                               const StartMs: Int64;
                                               const EndMs: Int64;
                                               out Cues: TMfTimedTextCueArray;
                                               out EndOfTrack: Boolean;
                                               const CancelEvent: THandle = 0): HRESULT; static;

    class function ImportBestTextTrackFromList(const MediaFileName: WideString;
                                               const PreferredLanguage: string;
                                               const Tracks: TMfEmbeddedSubtitleTrackInfoArray;
                                               const TimedText: TMfTimedText;
                                               out SelectedTrack: TMfEmbeddedSubtitleTrackInfo): HRESULT; static;

  end;


implementation

const
  MF_EMBEDDED_SUBTITLE_MAX_STREAMS = 256;
  MF_EMBEDDED_SUBTITLE_DEFAULT_DURATION_MS = 2000;


procedure TMfEmbeddedSubtitleTrackInfo.Reset();
begin

  Source := essMediaFoundation;
  StreamIndex := 0;
  NativeTypeIndex := 0;
  MajorType := GUID_NULL;
  Subtype := GUID_NULL;
  CodecId := '';
  Format := esfUnknown;
  Language := '';
  Name := '';
  InitiallySelected := False;
  Supported := False;
end;


class function TMfEmbeddedSubtitleReader.CreateReader(const MediaFileName: WideString;
                                                      out Reader: IMFSourceReader): HRESULT;
begin

  Reader := nil;

  if (MediaFileName = '') then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Result := MFCreateSourceReaderFromURL(PWideChar(MediaFileName),
                                        nil,
                                        Reader);
end;


class function TMfEmbeddedSubtitleReader.GetStreamAttributeString(const Reader: IMFSourceReader;
                                                                  const StreamIndex: DWORD;
                                                                  const AttributeKey: TGUID): string;
var
  Value: PROPVARIANT;

begin

  Result := '';

  if not Assigned(Reader) then
    Exit;

  PropVariantInit(Value);

  try
    if FAILED(Reader.GetPresentationAttribute(StreamIndex,
                                              AttributeKey,
                                              Value)) then
      Exit;

    case Value.vt of
      VT_LPWSTR: if Assigned(Value.pwszVal) then
                   Result := Value.pwszVal;
      VT_BSTR:   if Assigned(Value.bstrVal) then
                   Result := Value.bstrVal;
      VT_LPSTR:  if Assigned(Value.pszVal) then
                   Result := string(AnsiString(Value.pszVal));
    end;
  finally
    PropVariantClear(Value);
  end;
end;


class function TMfEmbeddedSubtitleReader.GetDescriptorAttributeString(const Descriptor: IMFStreamDescriptor;
                                                                      const AttributeKey: TGUID): string;
var
  Value: PROPVARIANT;

begin

  Result := '';

  if not Assigned(Descriptor) then
    Exit;

  PropVariantInit(Value);

  try
    if FAILED(Descriptor.GetItem(AttributeKey,
                                 Value)) then
      Exit;

    case Value.vt of
      VT_LPWSTR: if Assigned(Value.pwszVal) then
                   Result := Value.pwszVal;
      VT_BSTR:   if Assigned(Value.bstrVal) then
                   Result := Value.bstrVal;
      VT_LPSTR:  if Assigned(Value.pszVal) then
                   Result := string(AnsiString(Value.pszVal));
    end;
  finally
    PropVariantClear(Value);
  end;
end;


class function TMfEmbeddedSubtitleReader.IsSubtitleMajorType(const MajorType: TGUID): Boolean;
begin

  // The Windows Matroska source normally reports MFMediaType_Subtitle, but
  // third-party and older sources can expose textual captions as SAMI or
  // Script. The subtype remains the decisive classifier.
  Result := IsEqualGUID(MajorType,
                        MFMediaType_Subtitle) or
            IsEqualGUID(MajorType,
                        MFMediaType_SAMI) or
            IsEqualGUID(MajorType,
                        MFMediaType_Script);
end;


class function TMfEmbeddedSubtitleReader.ClassifySubtype(const Subtype: TGUID): TMfEmbeddedSubtitleFormat;
begin

  if IsEqualGUID(Subtype,
                 MFSubtitleFormat_SRT) then
    Result := esfSrt
  else
    if IsEqualGUID(Subtype,
                   MFSubtitleFormat_SSA) then
      Result := esfSsaAss
    else
      if IsEqualGUID(Subtype,
                     FSubtitleFormat_VobSub) then
        Result := esfVobSub
      else
        if IsEqualGUID(Subtype,
                       MFSubtitleFormat_PGS) then
          Result := esfPgs
        else
          Result := esfUnknown;
end;


class function TMfEmbeddedSubtitleReader.FormatName(const SubtitleFormat: TMfEmbeddedSubtitleFormat): string;
begin

  case SubtitleFormat of
    esfSrt: Result :=         'SRT/UTF-8';
    esfSsaAss: Result :=      'SSA/ASS';
    esfGenericText: Result := 'Text';
    esfVobSub: Result :=      'VobSub';
    esfPgs: Result :=         'PGS';
  else
    Result :=                 'Unknown';
  end;
end;


class function TMfEmbeddedSubtitleReader.TrackScore(const Track: TMfEmbeddedSubtitleTrackInfo;
                                                    const PreferredLanguage: string): Integer;
var
  Preferred: string;
  Language: string;
  PreferredBase: string;
  LanguageBase: string;
  SeparatorPos: Integer;

begin

  if not Track.Supported then
    begin
      Result := Low(Integer);
      Exit;
    end;

  Result := 10;
  Preferred := LowerCase(StringReplace(Trim(PreferredLanguage),
                                       '_',
                                       '-',
                                       [rfReplaceAll]));

  Language := LowerCase(StringReplace(Trim(Track.Language),
                                      '_',
                                      '-',
                                      [rfReplaceAll]));

  if Track.InitiallySelected then
    Inc(Result,
        20);

  if (Preferred <> '') and (Language <> '') then
    begin

      if SameText(Preferred,
                  Language) then
        Inc(Result, 1000)
      else
        begin
          PreferredBase := Preferred;
          SeparatorPos := Pos('-',
                              PreferredBase);
          if (SeparatorPos > 0) then
            PreferredBase := Copy(PreferredBase,
                                  1,
                                  SeparatorPos - 1);

          LanguageBase := Language;
          SeparatorPos := Pos('-',
                              LanguageBase);

          if (SeparatorPos > 0) then
            LanguageBase := Copy(LanguageBase,
                                 1,
                                 SeparatorPos - 1);

          if (PreferredBase <> '') and SameText(PreferredBase,
                                                LanguageBase) then
            Inc(Result,
                500);
        end;
    end;

  case Track.Format of
    esfSrt: Inc(Result,
                4);
    esfSsaAss: Inc(Result,
                   3);
    esfGenericText: Inc(Result,
                        2);
  end;
end;

class function TMfEmbeddedSubtitleReader.ReadSampleBytes(const Sample: IMFSample;
                                                         out Data: TBytes): HRESULT;
var
  Buffer: IMFMediaBuffer;
  BufferData: PByte;
  MaxLength: DWORD;
  CurrentLength: DWORD;

begin

  SetLength(Data,
            0);
  Buffer := nil;
  BufferData := nil;
  MaxLength := 0;
  CurrentLength := 0;

  if not Assigned(Sample) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  Result := Sample.ConvertToContiguousBuffer(@Buffer);
  if FAILED(Result) then
    Exit;

  Result := Buffer.Lock(BufferData,
                        @MaxLength,
                        @CurrentLength);
  if FAILED(Result) then
    Exit;

  try
    if (CurrentLength > 0) then
      begin
        SetLength(Data,
                  CurrentLength);
        Move(BufferData^,
             Data[0],
             CurrentLength);
      end;
  finally
    Buffer.Unlock();
  end;

  Result := S_OK;
end;


class function TMfEmbeddedSubtitleReader.DecodeTextBytes(const Data: TBytes): string;
var
  StartIndex: Integer;
  ByteCount: Integer;
  LastIndex: Integer;
  I: Integer;
  PairCount: Integer;
  ZeroEven: Integer;
  ZeroOdd: Integer;
  LooksUtf16LE: Boolean;
  LooksUtf16BE: Boolean;

begin

  Result := '';
  if Length(Data) = 0 then
    Exit;

  StartIndex := 0;
  ByteCount := Length(Data);

  LooksUtf16LE := False;
  LooksUtf16BE := False;

  // Some Media Foundation sources return UTF-16 subtitle samples without a
  // byte-order mark. Detect that representation before treating the bytes as
  // UTF-8. Matroska itself stores text as UTF-8, but a media source is allowed
  // to expose a transformed representation.
  if (ByteCount >= 4) and
     not (((Data[0] = $FF) and (Data[1] = $FE)) or
          ((Data[0] = $FE) and (Data[1] = $FF))) then
    begin
      PairCount := ByteCount div 2;
      if (PairCount > 64) then
        PairCount := 64;

      ZeroEven := 0;
      ZeroOdd := 0;

      for I := 0 to PairCount - 1 do
        begin
          if (Data[I * 2] = 0) then
            Inc(ZeroEven);
          if Data[(I * 2) + 1] = 0 then
            Inc(ZeroOdd);
        end;

      LooksUtf16LE := (ZeroOdd >= ((PairCount * 3) div 4)) and
                      (ZeroEven <= (PairCount div 4));

      LooksUtf16BE := (ZeroEven >= ((PairCount * 3) div 4)) and
                      (ZeroOdd <= (PairCount div 4));
    end;

  if (ByteCount >= 2) and (Data[0] = $FF) and (Data[1] = $FE) then
    begin
      StartIndex := 2;
      Dec(ByteCount,
          2);

      if Odd(ByteCount) then
        Dec(ByteCount);

      if (ByteCount > 0) then
        Result := TEncoding.Unicode.GetString(Data,
                                              StartIndex,
                                              ByteCount);
    end
  else
    if (ByteCount >= 2) and (Data[0] = $FE) and (Data[1] = $FF) then
      begin
        StartIndex := 2;
        Dec(ByteCount,
            2);

        if Odd(ByteCount) then
          Dec(ByteCount);

        if (ByteCount > 0) then
          Result := TEncoding.BigEndianUnicode.GetString(Data,
                                                         StartIndex,
                                                         ByteCount);
      end
    else
      if LooksUtf16LE then
        begin
          if Odd(ByteCount) then
            Dec(ByteCount);

          if (ByteCount > 0) then
            Result := TEncoding.Unicode.GetString(Data,
                                                  0,
                                                  ByteCount);
        end
      else
        if LooksUtf16BE then
          begin
            if Odd(ByteCount) then
              Dec(ByteCount);

            if (ByteCount > 0) then
              Result := TEncoding.BigEndianUnicode.GetString(Data,
                                                             0,
                                                             ByteCount);
          end
        else
          begin
            while (ByteCount > 0) and (Data[StartIndex + ByteCount - 1] = 0) do
              Dec(ByteCount);

            if (ByteCount >= 3) and
               (Data[0] = $EF) and
               (Data[1] = $BB) and
               (Data[2] = $BF) then
              begin
                StartIndex := 3;
                Dec(ByteCount,
                    3);
              end;

            if (ByteCount > 0) then
              try
                Result := TEncoding.UTF8.GetString(Data,
                                                   StartIndex,
                                                   ByteCount);
              except
                Result := TEncoding.Default.GetString(Data,
                                                      StartIndex,
                                                      ByteCount);
              end;
          end;

  LastIndex := Length(Result);

  while (LastIndex > 0) and (Result[LastIndex] = #0) do
    Dec(LastIndex);

  if (LastIndex < Length(Result)) then
    SetLength(Result,
              LastIndex);
end;


class function TMfEmbeddedSubtitleReader.NormalizeSrtText(const Value: string): string;
var
  Lines: TStringList;
  Normalized: string;
  FirstLineNumber: Integer;

begin

  Normalized := StringReplace(Value,
                              ULBR,
                              #10, [rfReplaceAll]);

  Normalized := StringReplace(Normalized,
                              LFEED,
                              #10, [rfReplaceAll]);
  Normalized := Trim(Normalized);

  // Matroska text blocks normally contain only cue text, but tolerate a
  // complete SRT cue as well. In that case the sample timestamp remains
  // authoritative and the embedded index/timing lines are removed.
  Lines := TStringList.Create();
  try
    Lines.Text := StringReplace(Normalized,
                                #10,
                                Lines.LineBreak,
                                [rfReplaceAll]);

    if (Lines.Count >= 2) and
       TryStrToInt(Trim(Lines[0]),
                   FirstLineNumber) and
       (Pos('-->',
            Lines[1]) > 0) then
      begin
        Lines.Delete(0);
        Lines.Delete(0);
      end
    else
      if (Lines.Count >= 1) and (Pos('-->', Lines[0]) > 0) then
        Lines.Delete(0);

    Result := Trim(StringReplace(Lines.Text,
                                 Lines.LineBreak,
                                 #10,
                                 [rfReplaceAll]));
  finally
    Lines.Free();
  end;
end;


class function TMfEmbeddedSubtitleReader.TextAfterCommaCount(const Value: string;
                                                             const CommaCount: Integer): string;
var
  I: Integer;
  Found: Integer;

begin

  Result := Value;
  Found := 0;

  for I := 1 to Length(Value) do
    if (Value[I] = ',') then
      begin
        Inc(Found);
        if (Found = CommaCount) then
          begin
            Result := Copy(Value,
                           I + 1,
                           MaxInt);
            Exit;
          end;
      end;
end;


class function TMfEmbeddedSubtitleReader.StripAssOverrideTags(const Value: string): string;
var
  I: Integer;
  InTag: Boolean;

begin

  Result := '';
  InTag := False;

  for I := 1 to Length(Value) do
    begin
      if (Value[I] = '{') then
        InTag := True
      else
        if (Value[I] = '}') then
          InTag := False
        else
          if not InTag then
            Result := Result + Value[I];
    end;
end;


class function TMfEmbeddedSubtitleReader.NormalizeSsaAssText(const Value: string): string;
var
  Text: string;

begin

  Text := Trim(Value);

  if SameText(Copy(Text,
                   1,
                   9),
                   'Dialogue:') then
    begin
      Delete(Text,
             1,
             9);

      Text := TrimLeft(Text);
      Text := TextAfterCommaCount(Text,
                                  9);
    end
  else
    begin

      // Matroska stores ASS packets as:
      // ReadOrder,Layer,Style,Name,MarginL,MarginR,MarginV,Effect,Text
      Text := TextAfterCommaCount(Text,
                                  8);
    end;

  Text := StripAssOverrideTags(Text);
  Text := StringReplace(Text,
                        '\N',
                        #10,
                        [rfReplaceAll]);

  Text := StringReplace(Text,
                        '\n',
                        #10,
                        [rfReplaceAll]);

  Text := StringReplace(Text,
                        '\h',
                        ' ',
                        [rfReplaceAll]);

  Result := NormalizeSrtText(Text);
end;


class procedure TMfEmbeddedSubtitleReader.NormalizeCueTimes(var Cues: TMfTimedTextCueArray);
var
  I: Integer;

begin

  for I := Low(Cues) to High(Cues) do
    begin
      if (Cues[I].StartMs < 0) then
        Cues[I].StartMs := 0;

      if Cues[I].StopMs <= Cues[I].StartMs then
        begin

          if (I < High(Cues)) and (Cues[I + 1].StartMs > Cues[I].StartMs) then
            Cues[I].StopMs := Cues[I + 1].StartMs
          else
            Cues[I].StopMs := Cues[I].StartMs + MF_EMBEDDED_SUBTITLE_DEFAULT_DURATION_MS;
        end;
    end;
end;


class function TMfEmbeddedSubtitleReader.ResolveReaderNativeType(const Reader: IMFSourceReader;
                                                                 const Track: TMfEmbeddedSubtitleTrackInfo;
                                                                 out NativeType: IMFMediaType;
                                                                 out NativeTypeIndex: DWORD): HRESULT;
var
  TypeIndex: DWORD;
  Candidate: IMFMediaType;
  CandidateSubtype: TGUID;
  CandidateFormat: TMfEmbeddedSubtitleFormat;
  FirstType: IMFMediaType;
  FirstTypeIndex: DWORD;
  HrType: HRESULT;

begin

  NativeType := nil;
  NativeTypeIndex := 0;
  FirstType := nil;
  FirstTypeIndex := 0;

  if not Assigned(Reader) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  // A media-type index obtained from IMFMediaTypeHandler belongs to the
  // presentation descriptor. It is not guaranteed to be the same index used
  // by a separately created IMFSourceReader. Resolve the native type again on
  // that reader by subtype/format instead of reusing the descriptor index.
  for TypeIndex := 0 to MF_EMBEDDED_SUBTITLE_MAX_STREAMS - 1 do
    begin
      Candidate := nil;
      HrType := Reader.GetNativeMediaType(Track.StreamIndex,
                                          TypeIndex,
                                          @Candidate);
      if (HrType = MF_E_NO_MORE_TYPES) or (HrType = MF_E_INVALIDSTREAMNUMBER) then
        Break;

      if FAILED(HrType) then
        Continue;

      if not Assigned(FirstType) then
        begin
          FirstType := Candidate;
          FirstTypeIndex := TypeIndex;
        end;

      CandidateSubtype := GUID_NULL;

      if FAILED(Candidate.GetGUID(MF_MT_SUBTYPE,
                                  CandidateSubtype)) then
        CandidateSubtype := GUID_NULL;

      CandidateFormat := ClassifySubtype(CandidateSubtype);

      if ((not IsEqualGUID(Track.Subtype,
                           GUID_NULL)) and
          IsEqualGUID(CandidateSubtype,
                      Track.Subtype)) or
         ((Track.Format <> esfUnknown) and
          (CandidateFormat = Track.Format)) then
        begin
          NativeType := Candidate;
          NativeTypeIndex := TypeIndex;
          Result := S_OK;
          Exit;
        end;
    end;

  // Keep an otherwise readable stream alive when the presentation descriptor
  // did not expose a subtype. For Matroska text streams the first native type
  // is normally the required SRT or SSA media type.
  if Assigned(FirstType) then
    begin
      NativeType := FirstType;
      NativeTypeIndex := FirstTypeIndex;
      Result := S_OK;
    end
  else
    Result := MF_E_INVALIDMEDIATYPE;
end;


class function TMfEmbeddedSubtitleReader.ReadTrack(const Reader: IMFSourceReader;
                                                   const Track: TMfEmbeddedSubtitleTrackInfo;
                                                   out Cues: TMfTimedTextCueArray): HRESULT;
var
  NativeType: IMFMediaType;
  CurrentType: IMFMediaType;
  ReaderNativeTypeIndex: DWORD;
  Sample: IMFSample;
  ActualStreamIndex: DWORD;
  Flags: DWORD;
  SampleTime: LONGLONG;
  SampleDuration: LONGLONG;
  Data: TBytes;
  Text: string;
  CueIndex: Integer;
  SampleCount: Integer;
  EmptySampleCount: Integer;
  EndOfStream: Boolean;
  HrRead: HRESULT;

begin

  SetLength(Cues,
            0);

  if not Assigned(Reader) then
    begin

      Result := E_POINTER;
      Exit;
    end;

  NativeType := nil;
  CurrentType := nil;

  // Configure the selected subtitle stream explicitly. Caption streams are
  // normally deselected by the Matroska source, and an unselected stream is
  // not guaranteed to have a current Source Reader output type.
  ReaderNativeTypeIndex := 0;
  Result := ResolveReaderNativeType(Reader,
                                    Track,
                                    NativeType,
                                    ReaderNativeTypeIndex);
  if FAILED(Result) then
    begin
      OutputDebugString(PChar(Format('Subtitle native type resolution failed stream=%d descriptorType=%d hr=%s',
                                     [Track.StreamIndex, Track.NativeTypeIndex, IntToHex(DWORD(Result), 8)])));
      Exit;
    end;

  Result := Reader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS,
                                      False);
  if FAILED(Result) then
    begin
      OutputDebugString(PChar(Format('Subtitle deselect-all failed hr=%s',
                                     [IntToHex(DWORD(Result), 8)])));
      Exit;
    end;

  Result := Reader.SetStreamSelection(Track.StreamIndex,
                                      True);
  if FAILED(Result) then
    begin
      OutputDebugString(PChar(Format('Subtitle select failed stream=%d hr=%s',
                                     [Track.StreamIndex, IntToHex(DWORD(Result), 8)])));
      Exit;
    end;

  // A native subtitle stream can be read without a conversion MFT. Ask the
  // Source Reader to use the enumerated native type, but do not discard an
  // otherwise readable stream merely because a particular source rejects an
  // explicit SetCurrentMediaType call.
  Result := Reader.SetCurrentMediaType(Track.StreamIndex,
                                       0,
                                       NativeType);
  if FAILED(Result) then
    begin
      OutputDebugString(PChar(Format('Subtitle SetCurrentMediaType was rejected stream=%d hr=%s; trying native current type',
                                     [Track.StreamIndex, IntToHex(DWORD(Result), 8)])));
    end;

  // Force the Source Reader to commit the stream configuration before the
  // first sample request. This also provides a useful diagnostic if the source
  // silently substitutes another media type.
  if SUCCEEDED(Reader.GetCurrentMediaType(Track.StreamIndex,
                                          @CurrentType)) and
     Assigned(CurrentType) then
    OutputDebugString(PChar(Format('Subtitle current type ready stream=%d nativeType=%d',
                                   [Track.StreamIndex, ReaderNativeTypeIndex])));

  SampleCount := 0;
  EmptySampleCount := 0;

  while True do
    begin
      Sample := nil;
      ActualStreamIndex := MF_SOURCE_READER_INVALID_STREAM_INDEX;
      Flags := 0;
      SampleTime := 0;

      HrRead := Reader.ReadSample(Track.StreamIndex,
                                  0,
                                  @ActualStreamIndex,
                                  @Flags,
                                  @SampleTime,
                                  @Sample);
      if FAILED(HrRead) then
        begin
          OutputDebugString(PChar(Format('Subtitle ReadSample failed stream=%d hr=%s samples=%d',
                                         [Track.StreamIndex, IntToHex(DWORD(HrRead), 8), SampleCount])));
          Result := HrRead;
          Exit;
        end;

      EndOfStream := (Flags and MF_SOURCE_READERF_ENDOFSTREAM) <> 0;

      if ((Flags and MF_SOURCE_READERF_ERROR) <> 0) then
        begin
          OutputDebugString(PChar(Format('Subtitle source flagged error stream=%d samples=%d',
                                         [Track.StreamIndex, SampleCount])));
          Result := E_FAIL;
          Exit;
        end;

      // A source is allowed to return the final sample together with the
      // end-of-stream flag. Process any supplied sample before breaking.
      if Assigned(Sample) then
        begin
          Inc(SampleCount);

          if (ActualStreamIndex <> MF_SOURCE_READER_INVALID_STREAM_INDEX) and
             (ActualStreamIndex <> Track.StreamIndex) then
            begin
              OutputDebugString(PChar(Format('Subtitle unexpected stream actual=%d requested=%d',
                                             [ActualStreamIndex, Track.StreamIndex])));
              Sample := nil;
            end;
        end;

      if not Assigned(Sample) then
        begin
          if EndOfStream then
            Break;
          Continue;
        end;

      Result := ReadSampleBytes(Sample,
                                Data);
      if FAILED(Result) then
        Exit;

      if (Length(Data) = 0) then
        begin
          Inc(EmptySampleCount);

          if EndOfStream then
            Break;
          Continue;
        end;

      Text := DecodeTextBytes(Data);
      case Track.Format of
        esfSrt,
        esfGenericText: Text := NormalizeSrtText(Text);
        esfSsaAss: Text := NormalizeSsaAssText(Text);
      else
        Text := '';
      end;

      if (Text = '') then
        begin
          Inc(EmptySampleCount);

          if EndOfStream then
            Break;
          Continue;
        end;

      SampleDuration := 0;

      if FAILED(Sample.GetSampleDuration(@SampleDuration)) then
        SampleDuration := 0;

      CueIndex := Length(Cues);
      SetLength(Cues,
                CueIndex + 1);

      Cues[CueIndex].StartMs := SampleTime div 10000;
      Cues[CueIndex].StopMs := (SampleTime + SampleDuration) div 10000;
      Cues[CueIndex].Text := Text;

      if EndOfStream then
        Break;
    end;

  NormalizeCueTimes(Cues);

  OutputDebugString(PChar(Format('Subtitle read complete stream=%d samples=%d cues=%d empty=%d',
                                 [Track.StreamIndex, SampleCount, Length(Cues), EmptySampleCount])));

  if (Length(Cues) = 0) then
    Result := S_FALSE
  else
    Result := S_OK;
end;


class function TMfEmbeddedSubtitleReader.EnumerateReaderTracks(const Reader: IMFSourceReader;
                                                               out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
var
  MediaType: IMFMediaType;
  MajorType: TGUID;
  Subtype: TGUID;
  StreamIndex: DWORD;
  NativeTypeIndex: DWORD;
  TrackIndex: Integer;
  HrType: HRESULT;
  StreamFinished: Boolean;
  Candidate: TMfEmbeddedSubtitleTrackInfo;
  CandidateFound: Boolean;
  CandidateSupported: Boolean;
  SubtitleFormat: TMfEmbeddedSubtitleFormat;
  IsSelected: Boolean;

begin

  SetLength(Tracks,
            0);

  if not Assigned(Reader) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  for StreamIndex := 0 to MF_EMBEDDED_SUBTITLE_MAX_STREAMS - 1 do
    begin
      NativeTypeIndex := 0;
      StreamFinished := False;
      Candidate.Reset();
      CandidateFound := False;
      CandidateSupported := False;

      while not StreamFinished do
        begin
          MediaType := nil;
          MajorType := GUID_NULL;
          Subtype := GUID_NULL;

          HrType := Reader.GetNativeMediaType(StreamIndex,
                                              NativeTypeIndex,
                                              @MediaType);

          if HrType = MF_E_INVALIDSTREAMNUMBER then
            begin
              // Stream indices are contiguous for the Source Reader.
              if (NativeTypeIndex = 0) then
                begin

                  if Length(Tracks) = 0 then
                    Result := S_FALSE
                  else
                    Result := S_OK;
                  Exit;
                end;

              Break;
            end;

          if (HrType = MF_E_NO_MORE_TYPES) then
            Break;

          if FAILED(HrType) then
            begin
              OutputDebugString(PChar(Format('Stream enumeration failed stream=%d type=%d hr=%s',
                                             [StreamIndex, NativeTypeIndex, IntToHex(DWORD(HrType), 8)])));
              Break;
            end;

          if FAILED(MediaType.GetGUID(MF_MT_MAJOR_TYPE,
                                      MajorType)) then
            MajorType := GUID_NULL;
          if FAILED(MediaType.GetGUID(MF_MT_SUBTYPE,
                                      Subtype)) then
            Subtype := GUID_NULL;

          SubtitleFormat := ClassifySubtype(Subtype);

          if (SubtitleFormat = esfUnknown) and IsSubtitleMajorType(MajorType) then
            SubtitleFormat := esfGenericText;

          OutputDebugString(PChar(Format('Media stream=%d type=%d major=%s subtype=%s',
                                         [StreamIndex, NativeTypeIndex, GUIDToString(MajorType), GUIDToString(Subtype)])));

          // Do not depend exclusively on the major type. The native Microsoft
          // Matroska source normally uses MFMediaType_Subtitle, while older or
          // third-party media sources can expose a known subtitle subtype under
          // SAMI, Script, or another data major type.
          if IsSubtitleMajorType(MajorType) or
             (SubtitleFormat <> esfUnknown) then
            begin
              if (not CandidateFound) or
                 ((SubtitleFormat in [esfSrt, esfSsaAss, esfGenericText]) and
                  (not CandidateSupported)) then
                begin
                  Candidate.Reset();
                  Candidate.StreamIndex := StreamIndex;
                  Candidate.NativeTypeIndex := NativeTypeIndex;
                  Candidate.MajorType := MajorType;
                  Candidate.Subtype := Subtype;
                  Candidate.Format := SubtitleFormat;
                  Candidate.Supported := SubtitleFormat in [esfSrt, esfSsaAss, esfGenericText];
                  CandidateFound := True;
                  CandidateSupported := Candidate.Supported;
                end;
            end;

          Inc(NativeTypeIndex);
        end;

      if CandidateFound then
        begin
          IsSelected := False;

          if SUCCEEDED(Reader.GetStreamSelection(StreamIndex, IsSelected)) then
            Candidate.InitiallySelected := (IsSelected <> False);

          Candidate.Language := GetStreamAttributeString(Reader,
                                                         StreamIndex,
                                                         MF_SD_LANGUAGE);
          Candidate.Name := GetStreamAttributeString(Reader,
                                                     StreamIndex,
                                                     MF_SD_STREAM_NAME);

          TrackIndex := Length(Tracks);
          SetLength(Tracks,
                    TrackIndex + 1);

          Tracks[TrackIndex] := Candidate;

          OutputDebugString(PChar(Format('Embedded subtitle stream=%d type=%d major=%s subtype=%s format=%s lang=%s name=%s supported=%s',
                                         [Candidate.StreamIndex, Candidate.NativeTypeIndex,
                                          GUIDToString(Candidate.MajorType),
                                          GUIDToString(Candidate.Subtype),
                                          FormatName(Candidate.Format),
                                          Candidate.Language,
                                          Candidate.Name,
                                          BoolToStr(Candidate.Supported)])));
        end;
    end;

  if (Length(Tracks) = 0) then
    Result := S_FALSE
  else
    Result := S_OK;
end;


class function TMfEmbeddedSubtitleReader.EnumeratePresentationTracks(const PresentationDescriptor: IMFPresentationDescriptor;
                                                                     out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
var
  StreamCount: DWORD;
  StreamIndex: DWORD;
  TypeCount: DWORD;
  TypeIndex: DWORD;
  TrackIndex: Integer;
  Selected: BOOL;
  StreamDescriptor: IMFStreamDescriptor;
  TypeHandler: IMFMediaTypeHandler;
  MediaType: IMFMediaType;
  HandlerMajorType: TGUID;
  MajorType: TGUID;
  Subtype: TGUID;
  SubtitleFormat: TMfEmbeddedSubtitleFormat;
  Candidate: TMfEmbeddedSubtitleTrackInfo;
  CandidateFound: Boolean;
  CandidateSupported: Boolean;
  Language: string;
  StreamName: string;
  HrType: HRESULT;

  // Helpers
  procedure ConsiderMediaType(const AType: IMFMediaType;
                              const ATypeIndex: DWORD);
  begin

    if not Assigned(AType) then
      Exit;

    MajorType := GUID_NULL;
    Subtype := GUID_NULL;

    if FAILED(AType.GetGUID(MF_MT_MAJOR_TYPE,
                            MajorType)) then
      MajorType := HandlerMajorType;

    if FAILED(AType.GetGUID(MF_MT_SUBTYPE,
                            Subtype)) then
      Subtype := GUID_NULL;

    SubtitleFormat := ClassifySubtype(Subtype);

    if (SubtitleFormat = esfUnknown) and
       IsSubtitleMajorType(MajorType) then
      SubtitleFormat := esfGenericText;

    OutputDebugString(PChar(Format('Descriptor stream=%d selected=%s type=%d major=%s subtype=%s',
                                   [StreamIndex, BoolToStr(Selected <> False),
                                    ATypeIndex, GUIDToString(MajorType),
                                    GUIDToString(Subtype)])));

    if IsSubtitleMajorType(MajorType) or
       (SubtitleFormat <> esfUnknown) then
      begin
        if (not CandidateFound) or
           ((SubtitleFormat in [esfSrt, esfSsaAss, esfGenericText]) and
            (not CandidateSupported)) then
          begin
            Candidate.Reset();
            Candidate.StreamIndex := StreamIndex;
            Candidate.NativeTypeIndex := ATypeIndex;
            Candidate.MajorType := MajorType;
            Candidate.Subtype := Subtype;
            Candidate.Format := SubtitleFormat;
            Candidate.InitiallySelected := (Selected <> False);
            Candidate.Supported := SubtitleFormat in [esfSrt, esfSsaAss, esfGenericText];
            CandidateFound := True;
            CandidateSupported := Candidate.Supported;
          end;
      end;
  end;

begin

  SetLength(Tracks,
            0);

  if not Assigned(PresentationDescriptor) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  StreamCount := 0;
  Result := PresentationDescriptor.GetStreamDescriptorCount(StreamCount);
  if FAILED(Result) then
    Exit;

  OutputDebugString(PChar(Format('Presentation descriptor stream count=%d',
                                 [StreamCount])));

  if (StreamCount = 0) then
    begin
      Result := S_FALSE;
      Exit;
    end;

  for StreamIndex := 0 to StreamCount - 1 do
    begin
      Selected := False;
      StreamDescriptor := nil;
      TypeHandler := nil;
      Candidate.Reset();
      CandidateFound := False;
      CandidateSupported := False;
      HandlerMajorType := GUID_NULL;

      Result := PresentationDescriptor.GetStreamDescriptorByIndex(StreamIndex,
                                                                  Selected,
                                                                  StreamDescriptor);
      if FAILED(Result) then
        Exit;

      Language := GetDescriptorAttributeString(StreamDescriptor,
                                               MF_SD_LANGUAGE);
      StreamName := GetDescriptorAttributeString(StreamDescriptor,
                                                 MF_SD_STREAM_NAME);

      Result := StreamDescriptor.GetMediaTypeHandler(TypeHandler);
      if FAILED(Result) then
        begin
          OutputDebugString(PChar(Format('Descriptor handler failed stream=%d hr=%s',
                                         [StreamIndex, IntToHex(DWORD(Result), 8)])));
          Continue;
        end;

      if FAILED(TypeHandler.GetMajorType(HandlerMajorType)) then
        HandlerMajorType := GUID_NULL;

      // Some sources report no enumerated media types for a deselected caption
      // stream but still expose a current media type. Inspect it first.
      MediaType := nil;
      HrType := TypeHandler.GetCurrentMediaType(MediaType);

      if SUCCEEDED(HrType) and Assigned(MediaType) then
        ConsiderMediaType(MediaType,
                          0)
      else
        OutputDebugString(PChar(Format('Descriptor current media type unavailable stream=%d hr=%s',
                                       [StreamIndex, IntToHex(DWORD(HrType), 8)])));

      TypeCount := 0;
      HrType := TypeHandler.GetMediaTypeCount(TypeCount);
      if FAILED(HrType) then
        begin
          OutputDebugString(PChar(Format('Descriptor media-type count failed stream=%d hr=%s',
                                         [StreamIndex, IntToHex(DWORD(HrType), 8)])));
          TypeCount := 0;
        end;

      if (TypeCount > 0) then
        for TypeIndex := 0 to TypeCount - 1 do
          begin

            MediaType := nil;
            HrType := TypeHandler.GetMediaTypeByIndex(TypeIndex,
                                                      MediaType);
            if FAILED(HrType) then
              begin
                OutputDebugString(PChar(Format('Descriptor media type failed stream=%d type=%d hr=%s',
                                               [StreamIndex, TypeIndex, IntToHex(DWORD(HrType), 8)])));
                Continue;
              end;

            ConsiderMediaType(MediaType,
                              TypeIndex);
          end;

      // Do not hide an unclassified, language-tagged non-A/V stream. This is
      // important for diagnostics and for sources that expose a generic text
      // subtype instead of the documented MFSubtitleFormat_SRT/SSA subtype.
      if (not CandidateFound) and
         (not IsEqualGUID(HandlerMajorType, MFMediaType_Audio)) and
         (not IsEqualGUID(HandlerMajorType, MFMediaType_Video)) and
         ((Language <> '') or (StreamName <> '')) then
        begin
          Candidate.Reset();
          Candidate.StreamIndex := StreamIndex;
          Candidate.NativeTypeIndex := 0;
          Candidate.MajorType := HandlerMajorType;
          Candidate.Format := esfGenericText;
          Candidate.InitiallySelected := (Selected <> False);
          Candidate.Supported := True;
          CandidateFound := True;

          OutputDebugString(PChar(Format('Descriptor unclassified language stream=%d major=%s lang=%s name=%s',
                                         [StreamIndex, GUIDToString(HandlerMajorType),
                                          Language, StreamName])));
        end;

      if CandidateFound then
        begin
          Candidate.Language := Language;
          Candidate.Name := StreamName;

          TrackIndex := Length(Tracks);
          SetLength(Tracks,
                    TrackIndex + 1);

          Tracks[TrackIndex] := Candidate;

          OutputDebugString(PChar(Format('Descriptor subtitle stream=%d type=%d format=%s lang=%s name=%s supported=%s',
                                         [Candidate.StreamIndex, Candidate.NativeTypeIndex,
                                          FormatName(Candidate.Format), Candidate.Language,
                                          Candidate.Name, BoolToStr(Candidate.Supported)])));
        end;
    end;

  if (Length(Tracks) = 0) then
    Result := S_FALSE
  else
    Result := S_OK;
end;


class function TMfEmbeddedSubtitleReader.MapMatroskaFormat(const Format: TMfMatroskaSubtitleFormat): TMfEmbeddedSubtitleFormat;
begin

  case Format of
    msfSrt:    Result := esfSrt;
    msfSsaAss: Result := esfSsaAss;
    msfVobSub: Result := esfVobSub;
    msfPgs:    Result := esfPgs;
  else
    Result := esfUnknown;
  end;
end;


class function TMfEmbeddedSubtitleReader.EnumerateMatroskaTracks(const MediaFileName: WideString;
                                                                 out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
var
  MatroskaTracks: TMfMatroskaSubtitleTrackArray;
  I: Integer;
  TrackNumber: UInt64;

begin

  SetLength(Tracks,
            0);
  SetLength(MatroskaTracks,
            0);

  if not SameText(ExtractFileExt(MediaFileName),
                  '.mkv') and
     not SameText(ExtractFileExt(MediaFileName),
                  '.webm') then
    begin
      Result := S_FALSE;
      Exit;
    end;

  Result := TMfMatroskaSubtitleReader.EnumerateTracks(MediaFileName,
                                                       MatroskaTracks);
  if (Result <> S_OK) then
    Exit;

  SetLength(Tracks,
            Length(MatroskaTracks));

  for I := Low(MatroskaTracks) to High(MatroskaTracks) do
    begin
      Tracks[I].Reset();
      Tracks[I].Source := essMatroska;
      TrackNumber := MatroskaTracks[I].TrackNumber;

      if (TrackNumber > High(DWORD)) then
        begin
          SetLength(Tracks,
                    I);
          Result := E_FAIL;
          Exit;
        end;

      Tracks[I].StreamIndex := DWORD(TrackNumber);
      Tracks[I].NativeTypeIndex := 0;
      Tracks[I].MajorType := MFMediaType_Subtitle;
      Tracks[I].CodecId := MatroskaTracks[I].CodecId;
      Tracks[I].Format := MapMatroskaFormat(MatroskaTracks[I].Format);

      case Tracks[I].Format of
        esfSrt:    Tracks[I].Subtype := MFSubtitleFormat_SRT;
        esfSsaAss: Tracks[I].Subtype := MFSubtitleFormat_SSA;
        esfPgs:    Tracks[I].Subtype := MFSubtitleFormat_PGS;
      else
        Tracks[I].Subtype := GUID_NULL;
      end;

      Tracks[I].Language := MatroskaTracks[I].Language;
      Tracks[I].Name := MatroskaTracks[I].Name;
      Tracks[I].InitiallySelected := MatroskaTracks[I].DefaultTrack or MatroskaTracks[I].ForcedTrack;
      Tracks[I].Supported := MatroskaTracks[I].Supported;

      OutputDebugString(PChar(Format('Direct MKV subtitle track=%d codec=%s format=%s lang=%s name=%s supported=%s',
                                     [Tracks[I].StreamIndex, Tracks[I].CodecId,
                                      FormatName(Tracks[I].Format), Tracks[I].Language,
                                      Tracks[I].Name, BoolToStr(Tracks[I].Supported)])));
    end;
end;


class function TMfEmbeddedSubtitleReader.EnumerateMediaSourceTracks(const MediaFileName: WideString;
                                                                    out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
var
  MediaSource: IMFMediaSource;
  PresentationDescriptor: IMFPresentationDescriptor;

begin

  SetLength(Tracks,
            0);

  MediaSource := nil;
  PresentationDescriptor := nil;

  Result := CreateObjectFromUrl(MediaFileName,
                                MediaSource);
  if FAILED(Result) then
    Exit;

  try
    Result := MediaSource.CreatePresentationDescriptor(
                            PresentationDescriptor);
    if SUCCEEDED(Result) then
      Result := EnumeratePresentationTracks(PresentationDescriptor,
                                            Tracks);
  finally
    MediaSource.Shutdown();
  end;
end;


class function TMfEmbeddedSubtitleReader.ImportMatroskaTextTrack(const MediaFileName: WideString;
                                                                 const Track: TMfEmbeddedSubtitleTrackInfo;
                                                                 const TimedText: TMfTimedText;
                                                                 const CancelEvent: THandle): HRESULT;
var
  MatroskaTrack: TMfMatroskaSubtitleTrack;
  MatroskaCues: TMfMatroskaSubtitleCueArray;
  Cues: TMfTimedTextCueArray;
  I: Integer;
  FriendlyName: string;
  CueText: string;

begin

  SetLength(MatroskaCues,
            0);
  SetLength(Cues,
            0);
  MatroskaTrack.Reset();

  Result := TMfMatroskaSubtitleReader.ReadTrack(MediaFileName,
                                                 UInt64(Track.StreamIndex),
                                                 MatroskaTrack,
                                                 MatroskaCues,
                                                 CancelEvent);
  if Result <> S_OK then
    Exit;

  SetLength(Cues, Length(MatroskaCues));
  for I := Low(MatroskaCues) to High(MatroskaCues) do
    begin
      CueText := MatroskaCues[I].Text;

      case Track.Format of
        esfSsaAss: CueText := NormalizeSsaAssText(CueText);
      else
        CueText :=            NormalizeSrtText(CueText);
      end;

      Cues[I].StartMs := MatroskaCues[I].StartMs;
      Cues[I].StopMs := MatroskaCues[I].StopMs;
      Cues[I].Text := CueText;
    end;

  FriendlyName := Track.Name;
  if (FriendlyName = '') then
    FriendlyName := Track.Language;

  if (FriendlyName = '') then
    FriendlyName := 'Embedded subtitles';

  Result := TimedText.ImportCues(Cues,
                                 MediaFileName,
                                 Track.Language,
                                 FriendlyName);

  OutputDebugString(PChar(Format('Direct MKV subtitle import track=%d cues=%d hr=%s',
                                [Track.StreamIndex, Length(Cues), IntToHex(DWORD(Result), 8)])));
end;


class function TMfEmbeddedSubtitleReader.LoadMatroskaTextTrackWindow(const MediaFileName: WideString;
                                                                     const Track: TMfEmbeddedSubtitleTrackInfo;
                                                                     const StartMs: Int64;
                                                                     const EndMs: Int64;
                                                                     out Cues: TMfTimedTextCueArray;
                                                                     out EndOfTrack: Boolean;
                                                                     const CancelEvent: THandle): HRESULT;
var
  MatroskaTrack: TMfMatroskaSubtitleTrack;
  MatroskaCues: TMfMatroskaSubtitleCueArray;
  I: Integer;
  CueText: string;

begin

  SetLength(Cues,
            0);
  SetLength(MatroskaCues,
            0);

  MatroskaTrack.Reset();
  EndOfTrack := False;

  if (Track.Source <> essMatroska) or
     (StartMs < 0) or
     (EndMs <= StartMs) then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

  Result := TMfMatroskaSubtitleReader.ReadTrackWindow(MediaFileName,
                                                      UInt64(Track.StreamIndex),
                                                      StartMs,
                                                      EndMs,
                                                      MatroskaTrack,
                                                      MatroskaCues,
                                                      EndOfTrack,
                                                      CancelEvent);

  if FAILED(Result) then
    Exit;

  SetLength(Cues,
            Length(MatroskaCues));

  for I := Low(MatroskaCues) to High(MatroskaCues) do
    begin
      CueText := MatroskaCues[I].Text;

      case Track.Format of
        esfSsaAss: CueText := NormalizeSsaAssText(CueText);
      else
        CueText :=            NormalizeSrtText(CueText);
      end;

      Cues[I].StartMs := MatroskaCues[I].StartMs;
      Cues[I].StopMs := MatroskaCues[I].StopMs;
      Cues[I].Text := CueText;
    end;

  OutputDebugString(PChar(Format('Direct MKV subtitle window track=%d start=%d end=%d cues=%d eof=%s hr=%s',
                                 [Track.StreamIndex, StartMs, EndMs, Length(Cues),
                                  BoolToStr(EndOfTrack), IntToHex(DWORD(Result), 8)])));
end;


class function TMfEmbeddedSubtitleReader.EnumerateTracks(const MediaFileName: WideString;
                                                         out Tracks: TMfEmbeddedSubtitleTrackInfoArray): HRESULT;
var
  DescriptorTracks: TMfEmbeddedSubtitleTrackInfoArray;
  I: Integer;
  MergeCount: Integer;
  Reader: IMFSourceReader;
  HrDescriptor: HRESULT;
  HrReader: HRESULT;
  HrMatroska: HRESULT;
  IsMatroska: Boolean;

  function HasUsefulLanguage(const Language: string): Boolean;
  var
    Value: string;

  begin

    Value := LowerCase(Trim(Language));
    Result := (Value <> '') and
              (Value <> 'iv') and
              (Value <> 'und');
  end;

begin

  SetLength(Tracks,
            0);
  SetLength(DescriptorTracks,
            0);
  Reader := nil;

  IsMatroska := SameText(ExtractFileExt(MediaFileName),
                         '.mkv') or
                SameText(ExtractFileExt(MediaFileName),
                         '.webm');

  // Prefer the compact direct EBML metadata pass. Reconcile it with the
  // presentation descriptor because some muxers expose a generic subtitle
  // codec or place the useful language metadata only on that descriptor.
  if IsMatroska then
    begin
      HrMatroska := EnumerateMatroskaTracks(MediaFileName,
                                            Tracks);
      if (HrMatroska = S_OK) then
        begin
          HrDescriptor := EnumerateMediaSourceTracks(MediaFileName,
                                                      DescriptorTracks);
          if (HrDescriptor = S_OK) then
            begin
              MergeCount := Length(Tracks);
              if Length(DescriptorTracks) < MergeCount then
                MergeCount := Length(DescriptorTracks);

              for I := 0 to MergeCount - 1 do
                begin
                  if (not HasUsefulLanguage(Tracks[I].Language)) and
                     HasUsefulLanguage(DescriptorTracks[I].Language) then
                    Tracks[I].Language := DescriptorTracks[I].Language;

                  if (Trim(Tracks[I].Name) = '') then
                    Tracks[I].Name := DescriptorTracks[I].Name;

                  // Retain the direct Matroska track whenever it is readable.
                  // Otherwise use the matching Media Foundation descriptor,
                  // which can expose generic text codecs the EBML reader does
                  // not decode itself.
                  if (not Tracks[I].Supported) and
                     DescriptorTracks[I].Supported then
                    Tracks[I] := DescriptorTracks[I];
                end;

              if Length(DescriptorTracks) > Length(Tracks) then
                begin
                  MergeCount := Length(Tracks);
                  SetLength(Tracks,
                            Length(DescriptorTracks));

                  for I := MergeCount to High(DescriptorTracks) do
                    Tracks[I] := DescriptorTracks[I];
                end;
            end;

          Result := S_OK;
          Exit;
        end;
    end
  else
    begin
      HrMatroska := S_FALSE;
      HrDescriptor := EnumerateMediaSourceTracks(MediaFileName,
                                                  Tracks);
      if (HrDescriptor = S_OK) then
        begin
          Result := S_OK;
          Exit;
        end;
    end;

  HrReader := CreateReader(MediaFileName,
                           Reader);
  if SUCCEEDED(HrReader) then
    HrReader := EnumerateReaderTracks(Reader,
                                      Tracks);

  if (HrReader = S_OK) then
    begin
      Result := S_OK;
      Exit;
    end;

  if not IsMatroska then
    begin
      Result := HrReader;
      Exit;
    end;

  Result := HrMatroska;

  OutputDebugString(PChar(Format('Direct MKV enumeration hr=%s tracks=%d',
                                 [IntToHex(DWORD(Result), 8), Length(Tracks)])));

  if (Result <> S_OK) and FAILED(HrReader) then
    Result := HrReader;
end;


class function TMfEmbeddedSubtitleReader.FindBestTextTrackFromList(const PreferredLanguage: string;
                                                                   const Tracks: TMfEmbeddedSubtitleTrackInfoArray;
                                                                   out SelectedTrack: TMfEmbeddedSubtitleTrackInfo): HRESULT;
var
  I: Integer;
  BestIndex: Integer;
  Score: Integer;
  BestScore: Integer;

begin

  SelectedTrack.Reset();
  BestIndex := -1;
  BestScore := Low(Integer);

  for I := Low(Tracks) to High(Tracks) do
    if Tracks[I].Supported then
      begin
        Score := TrackScore(Tracks[I],
                            PreferredLanguage);
        if (Score > BestScore) then
          begin

            BestScore := Score;
            BestIndex := I;
          end;
      end;

  if (BestIndex < 0) then
    begin
      Result := S_FALSE;
      Exit;
    end;

  SelectedTrack := Tracks[BestIndex];
  Result := S_OK;
end;


class function TMfEmbeddedSubtitleReader.ImportTextTrack(const MediaFileName: WideString;
                                                         const Track: TMfEmbeddedSubtitleTrackInfo;
                                                         const TimedText: TMfTimedText;
                                                         const CancelEvent: THandle): HRESULT;
var
  Reader: IMFSourceReader;
  Cues: TMfTimedTextCueArray;
  FriendlyName: string;

begin

  if not Assigned(TimedText) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if not Track.Supported then
    begin
      Result := MF_E_INVALIDMEDIATYPE;
      Exit;
    end;

  if (Track.Source = essMatroska) then
    begin
      Result := ImportMatroskaTextTrack(MediaFileName,
                                         Track,
                                         TimedText,
                                         CancelEvent);
      Exit;
    end;

  Reader := nil;
  Result := CreateReader(MediaFileName,
                         Reader);
  if FAILED(Result) then
    Exit;

  Result := ReadTrack(Reader,
                      Track,
                      Cues);
  if (Result <> S_OK) then
    Exit;

  FriendlyName := Track.Name;

  if (FriendlyName = '') then
    FriendlyName := Track.Language;

  if (FriendlyName = '') then
    FriendlyName := 'Embedded subtitles';

  Result := TimedText.ImportCues(Cues,
                                 MediaFileName,
                                 Track.Language,
                                 FriendlyName);
end;


class function TMfEmbeddedSubtitleReader.ImportBestTextTrackFromList(const MediaFileName: WideString;
                                                                     const PreferredLanguage: string;
                                                                     const Tracks: TMfEmbeddedSubtitleTrackInfoArray;
                                                                     const TimedText: TMfTimedText;
                                                                     out SelectedTrack: TMfEmbeddedSubtitleTrackInfo): HRESULT;
var
  Tried: array of Boolean;
  CandidateIndex: Integer;
  I: Integer;
  Score: Integer;
  BestScore: Integer;
  HrImport: HRESULT;

begin

  SelectedTrack.Reset();

  if not Assigned(TimedText) then
    begin
      Result := E_POINTER;
      Exit;
    end;

  if (Length(Tracks) = 0) then
    begin
      Result := S_FALSE;
      Exit;
    end;

  SetLength(Tried,
            Length(Tracks));

  while True do
    begin
      CandidateIndex := -1;
      BestScore := Low(Integer);

      for I := Low(Tracks) to High(Tracks) do
        if (not Tried[I]) and Tracks[I].Supported then
          begin
            Score := TrackScore(Tracks[I], PreferredLanguage);
            if (Score > BestScore) then
              begin
                BestScore := Score;
                CandidateIndex := I;
              end;
          end;

      if (CandidateIndex < 0) then
        Break;

      Tried[CandidateIndex] := True;
      HrImport := ImportTextTrack(MediaFileName,
                                  Tracks[CandidateIndex],
                                  TimedText);
      if (HrImport = S_OK) then
        begin

          SelectedTrack := Tracks[CandidateIndex];
          Result := S_OK;
          Exit;
        end;

      OutputDebugString(PChar(Format('Embedded subtitle import rejected stream=%d hr=%s; trying next track',
                                     [Tracks[CandidateIndex].StreamIndex, IntToHex(DWORD(HrImport), 8)])));
    end;

  Result := S_FALSE;
end;

end.
