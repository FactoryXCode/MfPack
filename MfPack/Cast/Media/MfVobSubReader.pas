unit MfVobSubReader;

interface

uses
  WinApi.Windows,
  WinApi.WinError,
  System.Classes,
  System.SysUtils,
  System.StrUtils;

type
  TMfVobSubFrame = record
    CueIndex: Integer;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Pixels: TBytes; // premultiplied BGRA, top-down
    procedure Reset();
  end;

  TMfVobSubCue = record
    StartMs: Int64;
    StopMs: Int64;
    FilePosition: Int64;
    NextFilePosition: Int64;
  end;

  TMfVobSubCueArray = array of TMfVobSubCue;

  TMfVobSubReader = class(TObject)
  private
    FIndexFileName: string;
    FSubFileName: string;
    FLanguage: string;
    FCanvasWidth: Integer;
    FCanvasHeight: Integer;
    FPalette: array[0..15] of Cardinal; // $00RRGGBB
    FCues: TMfVobSubCueArray;
    FCachedFrame: TMfVobSubFrame;
    FSubData: TBytes;
    FSubStream: TFileStream;

    function FindSubFile(const AIndexFileName: string): string;
    function ReadPacket(const ACue: TMfVobSubCue;
                        out APacket: TBytes): HRESULT;
    function DecodePacket(const APacket: TBytes;
                          const ACueIndex: Integer;
                          out AFrame: TMfVobSubFrame;
                          out AStopDelayMs: Int64): HRESULT;
    function ReadNibble(const AData: TBytes;
                        var APosition: Integer;
                        var AHighNibble: Boolean;
                        out AValue: Integer): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    function Open(const AIndexFileName: string): HRESULT;
    function TryGetFrame(const AMediaTimeMs: Int64;
                         out AFrame: TMfVobSubFrame): Boolean;

    property CanvasWidth: Integer read FCanvasWidth;
    property CanvasHeight: Integer read FCanvasHeight;
    property Language: string read FLanguage;
    property IndexFileName: string read FIndexFileName;
  end;

implementation

function ReadBe16(const AData: TBytes; const AOffset: Integer): Integer;
begin
  if (AOffset < 0) or (AOffset + 1 >= Length(AData)) then
    Exit(-1);
  Result := (Integer(AData[AOffset]) shl 8) or AData[AOffset + 1];
end;


procedure AppendBytes(var ADestination: TBytes;
                      const ASource: TBytes;
                      const AOffset: Integer;
                      const ACount: Integer);
var
  OldLength: Integer;
begin
  if (ACount <= 0) or (AOffset < 0) or
     (AOffset + ACount > Length(ASource)) then
    Exit;
  OldLength := Length(ADestination);
  SetLength(ADestination, OldLength + ACount);
  Move(ASource[AOffset], ADestination[OldLength], ACount);
end;


procedure TMfVobSubFrame.Reset();
begin
  CueIndex := -1;
  Left := 0;
  Top := 0;
  Width := 0;
  Height := 0;
  SetLength(Pixels, 0);
end;


constructor TMfVobSubReader.Create();
var
  I: Integer;
begin
  inherited Create();
  FCanvasWidth := 720;
  FCanvasHeight := 576;
  FLanguage := '';
  for I := 0 to 15 do
    FPalette[I] := 0;
  SetLength(FCues, 0);
  SetLength(FSubData, 0);
  FSubStream := nil;
  FCachedFrame.Reset();
end;


destructor TMfVobSubReader.Destroy();
begin
  FSubStream.Free();
  inherited Destroy();
end;


function TMfVobSubReader.FindSubFile(const AIndexFileName: string): string;
var
  BaseName: string;
  Candidate: string;
  CandidateBase: string;
  Dash: Integer;
  SearchRec: TSearchRec;
  LanguageToken: string;

  function HasLanguageSuffix(const AName: string): Boolean;
  var
    Stem: string;
  begin
    Stem := UpperCase(ChangeFileExt(AName, ''));
    Result := EndsText(' ' + LanguageToken, Stem) or
              EndsText('-' + LanguageToken, Stem) or
              EndsText('_' + LanguageToken, Stem);
    if (not Result) and (LanguageToken = 'EN') then
      Result := EndsText(' ENG', Stem) or EndsText('-ENG', Stem) or
                EndsText('_ENG', Stem) or EndsText(' ENGLISH', Stem);
  end;
begin
  Result := ChangeFileExt(AIndexFileName, '.sub');
  if FileExists(Result) then
    Exit;

  // Prefer an explicitly language-tagged bitmap file.  It is common for an
  // IDX and its SUB to have entirely different base names (for example
  // "title-eng.idx" paired with "Movie Name EN.sub").
  LanguageToken := UpperCase(Trim(FLanguage));
  if LanguageToken <> '' then
    begin
      if FindFirst(IncludeTrailingPathDelimiter(ExtractFilePath(AIndexFileName)) +
                   '*.sub', faAnyFile, SearchRec) = 0 then
        try
          repeat
            if ((SearchRec.Attr and faDirectory) = 0) and
               HasLanguageSuffix(SearchRec.Name) then
              begin
                Candidate := IncludeTrailingPathDelimiter(
                  ExtractFilePath(AIndexFileName)) + SearchRec.Name;
                if FileExists(Candidate) then
                  begin
                    Result := Candidate;
                    Exit;
                  end;
              end;
          until FindNext(SearchRec) <> 0;
        finally
          FindClose(SearchRec);
        end;
    end;

  // Some authoring tools append -eng/-de only to the IDX while all language
  // indexes point into one shared SUB file.
  BaseName := ChangeFileExt(AIndexFileName, '');
  Dash := LastDelimiter('-', BaseName);
  if Dash > LastDelimiter('\/', BaseName) then
    begin
      CandidateBase := Copy(BaseName, 1, Dash - 1);
      Candidate := CandidateBase + '.sub';
      if FileExists(Candidate) then
        begin
          Result := Candidate;
          Exit;
        end;
    end;

  Result := '';
end;


function TMfVobSubReader.Open(const AIndexFileName: string): HRESULT;
var
  Lines: TStringList;
  Line: string;
  Value: string;
  Parts: TArray<string>;
  PaletteParts: TArray<string>;
  I: Integer;
  P: Integer;
  H: Integer;
  M: Integer;
  S: Integer;
  Ms: Integer;
  Cue: TMfVobSubCue;
  NewIndex: Integer;
  SubStream: TFileStream;
begin
  SubStream := nil;
  FIndexFileName := '';
  FSubFileName := '';
  FLanguage := '';
  SetLength(FCues, 0);
  SetLength(FSubData, 0);
  FreeAndNil(FSubStream);
  FCachedFrame.Reset();

  if (AIndexFileName = '') or (not FileExists(AIndexFileName)) then
    Exit(HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND));
  Lines := TStringList.Create();
  try
    try
      Lines.LoadFromFile(AIndexFileName);
    except
      Exit(E_FAIL);
    end;

    for I := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[I]);
        if StartsText('size:', Line) then
          begin
            Value := Trim(Copy(Line, 6, MaxInt));
            P := Pos('x', LowerCase(Value));
            if P > 0 then
              begin
                FCanvasWidth := StrToIntDef(Trim(Copy(Value, 1, P - 1)), 720);
                FCanvasHeight := StrToIntDef(Trim(Copy(Value, P + 1, MaxInt)), 576);
              end;
          end
        else if StartsText('palette:', Line) then
          begin
            Value := Trim(Copy(Line, 9, MaxInt));
            PaletteParts := Value.Split([',']);
            for P := 0 to 15 do
              if P < Length(PaletteParts) then
                FPalette[P] := Cardinal(StrToIntDef('$' + Trim(PaletteParts[P]), 0));
          end
        else if StartsText('id:', Line) and (FLanguage = '') then
          begin
            Value := Trim(Copy(Line, 4, MaxInt));
            P := Pos(',', Value);
            if P > 0 then
              Value := Copy(Value, 1, P - 1);
            FLanguage := LowerCase(Trim(Value));
          end
        else if StartsText('timestamp:', Line) then
          begin
            Value := Trim(Copy(Line, 11, MaxInt));
            P := Pos(',', Value);
            if P <= 0 then
              Continue;
            Parts := Trim(Copy(Value, 1, P - 1)).Split([':', ':', ':']);
            if Length(Parts) <> 4 then
              Continue;
            H := StrToIntDef(Parts[0], -1);
            M := StrToIntDef(Parts[1], -1);
            S := StrToIntDef(Parts[2], -1);
            Ms := StrToIntDef(Parts[3], -1);
            if (H < 0) or (M < 0) or (S < 0) or (Ms < 0) then
              Continue;
            P := Pos('filepos:', LowerCase(Value));
            if P <= 0 then
              Continue;

            Cue.StartMs := (((Int64(H) * 60 + M) * 60) + S) * 1000 + Ms;
            Cue.StopMs := Cue.StartMs + 5000;
            Cue.FilePosition := StrToInt64Def('$' +
              Trim(Copy(Value, P + Length('filepos:'), MaxInt)), -1);
            Cue.NextFilePosition := -1;
            if Cue.FilePosition < 0 then
              Continue;
            NewIndex := Length(FCues);
            SetLength(FCues, NewIndex + 1);
            FCues[NewIndex] := Cue;
          end;
      end;
  finally
    Lines.Free();
  end;

  FSubFileName := FindSubFile(AIndexFileName);
  if FSubFileName = '' then
    Exit(HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND));

  // A subtitle cue must never make the video thread reopen and seek another
  // file on the same disk that is feeding the movie. Cache normal-sized SUB
  // files once; retain one open stream for unusually large files.
  try
    SubStream := TFileStream.Create(FSubFileName,
                                    fmOpenRead or fmShareDenyNone);
    if SubStream.Size <= Int64(64 * 1024 * 1024) then
      begin
        SetLength(FSubData, Integer(SubStream.Size));
        if Length(FSubData) > 0 then
          SubStream.ReadBuffer(FSubData[0], Length(FSubData));
        SubStream.Free();
      end
    else
      FSubStream := SubStream;
  except
    on E: Exception do
      begin
        SubStream.Free();
        SetLength(FSubData, 0);
        FreeAndNil(FSubStream);
        Exit(E_FAIL);
      end;
  end;

  if (FCanvasWidth <= 0) or (FCanvasHeight <= 0) or (Length(FCues) = 0) then
    Exit(E_INVALIDARG);
  for I := 0 to High(FCues) - 1 do
    begin
      FCues[I].NextFilePosition := FCues[I + 1].FilePosition;
      if FCues[I].StopMs > FCues[I + 1].StartMs then
        FCues[I].StopMs := FCues[I + 1].StartMs;
    end;
  FIndexFileName := AIndexFileName;
  Result := S_OK;
end;


function TMfVobSubReader.ReadPacket(const ACue: TMfVobSubCue;
                                    out APacket: TBytes): HRESULT;
var
  Data: TBytes;
  ReadLength: Int64;
  SubSize: Int64;
  P: Integer;
  PesLength: Integer;
  Payload: Integer;
  PayloadEnd: Integer;
  PacketSize: Integer;
begin
  SetLength(APacket, 0);
  if Length(FSubData) > 0 then
    SubSize := Length(FSubData)
  else if Assigned(FSubStream) then
    SubSize := FSubStream.Size
  else
    Exit(E_FAIL);

  if ACue.FilePosition >= SubSize then
    Exit(E_INVALIDARG);
    if (ACue.NextFilePosition > ACue.FilePosition) and
       (ACue.NextFilePosition <= SubSize) then
      ReadLength := ACue.NextFilePosition - ACue.FilePosition
    else
      ReadLength := SubSize - ACue.FilePosition;
    if ReadLength > 1024 * 1024 then
      ReadLength := 1024 * 1024;
    SetLength(Data, Integer(ReadLength));
    if Length(Data) > 0 then
      if Length(FSubData) > 0 then
        Move(FSubData[Integer(ACue.FilePosition)], Data[0], Length(Data))
      else
        begin
          FSubStream.Position := ACue.FilePosition;
          FSubStream.ReadBuffer(Data[0], Length(Data));
        end;

  P := 0;
  while P + 9 < Length(Data) do
    begin
      if (Data[P] = 0) and (Data[P + 1] = 0) and
         (Data[P + 2] = 1) and (Data[P + 3] = $BD) then
        begin
          PesLength := (Integer(Data[P + 4]) shl 8) or Data[P + 5];
          PayloadEnd := P + 6 + PesLength;
          if PayloadEnd > Length(Data) then
            PayloadEnd := Length(Data);
          Payload := P + 9 + Data[P + 8];
          if Payload < PayloadEnd then
            Inc(Payload); // private_stream_1 substream id
          if Payload < PayloadEnd then
            AppendBytes(APacket, Data, Payload, PayloadEnd - Payload);
          P := PayloadEnd;
          if Length(APacket) >= 2 then
            begin
              PacketSize := ReadBe16(APacket, 0);
              if (PacketSize > 0) and (Length(APacket) >= PacketSize) then
                begin
                  SetLength(APacket, PacketSize);
                  Break;
                end;
            end;
        end
      else
        Inc(P);
    end;

  if Length(APacket) < 4 then
    Result := E_FAIL
  else
    Result := S_OK;
end;


function TMfVobSubReader.ReadNibble(const AData: TBytes;
                                    var APosition: Integer;
                                    var AHighNibble: Boolean;
                                    out AValue: Integer): Boolean;
begin
  Result := (APosition >= 0) and (APosition < Length(AData));
  if not Result then Exit;
  if AHighNibble then
    begin
      AValue := AData[APosition] shr 4;
      AHighNibble := False;
    end
  else
    begin
      AValue := AData[APosition] and $0F;
      Inc(APosition);
      AHighNibble := True;
    end;
end;


function TMfVobSubReader.DecodePacket(const APacket: TBytes;
                                      const ACueIndex: Integer;
                                      out AFrame: TMfVobSubFrame;
                                      out AStopDelayMs: Int64): HRESULT;
var
  ControlOffset: Integer;
  SequenceOffset: Integer;
  NextSequence: Integer;
  Command: Integer;
  DateValue: Integer;
  PaletteMap: array[0..3] of Integer;
  AlphaMap: array[0..3] of Integer;
  FieldOffset: array[0..1] of Integer;
  LeftPos: Integer;
  RightPos: Integer;
  TopPos: Integer;
  BottomPos: Integer;
  I: Integer;
  Field: Integer;
  X: Integer;
  Y: Integer;
  Code: Integer;
  Nibble: Integer;
  RunLength: Integer;
  ColorCode: Integer;
  BytePosition: Integer;
  HighNibble: Boolean;
  PixelOffset: Integer;
  Rgb: Cardinal;
  Alpha: Integer;
begin
  AFrame.Reset();
  AStopDelayMs := 0;
  if Length(APacket) < 8 then Exit(E_INVALIDARG);
  ControlOffset := ReadBe16(APacket, 2);
  if (ControlOffset < 4) or (ControlOffset + 4 > Length(APacket)) then
    Exit(E_INVALIDARG);

  for I := 0 to 3 do
    begin
      PaletteMap[I] := I;
      AlphaMap[I] := 0;
    end;
  FieldOffset[0] := 4;
  FieldOffset[1] := 4;
  LeftPos := 0;
  RightPos := -1;
  TopPos := 0;
  BottomPos := -1;
  SequenceOffset := ControlOffset;

  while (SequenceOffset >= ControlOffset) and
        (SequenceOffset + 4 <= Length(APacket)) do
    begin
      DateValue := ReadBe16(APacket, SequenceOffset);
      NextSequence := ReadBe16(APacket, SequenceOffset + 2);
      I := SequenceOffset + 4;
      while I < Length(APacket) do
        begin
          Command := APacket[I];
          Inc(I);
          case Command of
            $00, $01: ;
            $02: if AStopDelayMs = 0 then
                   AStopDelayMs := (Int64(DateValue) * 1024 * 1000) div 90000;
            $03:
              if I + 1 < Length(APacket) then
                begin
                  PaletteMap[3] := APacket[I] shr 4;
                  PaletteMap[2] := APacket[I] and $0F;
                  PaletteMap[1] := APacket[I + 1] shr 4;
                  PaletteMap[0] := APacket[I + 1] and $0F;
                  Inc(I, 2);
                end;
            $04:
              if I + 1 < Length(APacket) then
                begin
                  AlphaMap[3] := APacket[I] shr 4;
                  AlphaMap[2] := APacket[I] and $0F;
                  AlphaMap[1] := APacket[I + 1] shr 4;
                  AlphaMap[0] := APacket[I + 1] and $0F;
                  Inc(I, 2);
                end;
            $05:
              if I + 5 < Length(APacket) then
                begin
                  LeftPos := (Integer(APacket[I]) shl 4) or (APacket[I + 1] shr 4);
                  RightPos := ((Integer(APacket[I + 1]) and $0F) shl 8) or APacket[I + 2];
                  TopPos := (Integer(APacket[I + 3]) shl 4) or (APacket[I + 4] shr 4);
                  BottomPos := ((Integer(APacket[I + 4]) and $0F) shl 8) or APacket[I + 5];
                  Inc(I, 6);
                end;
            $06:
              if I + 3 < Length(APacket) then
                begin
                  FieldOffset[0] := ReadBe16(APacket, I);
                  FieldOffset[1] := ReadBe16(APacket, I + 2);
                  Inc(I, 4);
                end;
            $07:
              begin
                if I + 1 >= Length(APacket) then Break;
                Inc(I, ReadBe16(APacket, I));
              end;
            $FF: Break;
          else
            Break;
          end;
          if Command = $FF then Break;
        end;
      if (NextSequence <= SequenceOffset) or
         (NextSequence + 4 > Length(APacket)) then
        Break;
      SequenceOffset := NextSequence;
    end;

  AFrame.Width := RightPos - LeftPos + 1;
  AFrame.Height := BottomPos - TopPos + 1;
  if (AFrame.Width <= 0) or (AFrame.Height <= 0) then
    Exit(E_INVALIDARG);
  AFrame.CueIndex := ACueIndex;
  AFrame.Left := LeftPos;
  AFrame.Top := TopPos;
  SetLength(AFrame.Pixels, AFrame.Width * AFrame.Height * 4);
  FillChar(AFrame.Pixels[0], Length(AFrame.Pixels), 0);

  for Field := 0 to 1 do
    begin
      BytePosition := FieldOffset[Field];
      HighNibble := True;
      Y := Field;
      while Y < AFrame.Height do
        begin
          X := 0;
          while X < AFrame.Width do
            begin
              if not ReadNibble(APacket, BytePosition, HighNibble, Code) then Break;
              if Code < 4 then
                begin
                  if not ReadNibble(APacket, BytePosition, HighNibble, Nibble) then Break;
                  Code := (Code shl 4) or Nibble;
                  if Code < 16 then
                    begin
                      if not ReadNibble(APacket, BytePosition, HighNibble, Nibble) then Break;
                      Code := (Code shl 4) or Nibble;
                      if Code < 64 then
                        begin
                          if not ReadNibble(APacket, BytePosition, HighNibble, Nibble) then Break;
                          Code := (Code shl 4) or Nibble;
                        end;
                    end;
                end;
              RunLength := Code shr 2;
              ColorCode := Code and 3;
              if RunLength = 0 then RunLength := AFrame.Width - X;
              if RunLength > AFrame.Width - X then RunLength := AFrame.Width - X;
              Rgb := FPalette[PaletteMap[ColorCode] and $0F];
              Alpha := (AlphaMap[ColorCode] and $0F) * 17;
              while RunLength > 0 do
                begin
                  PixelOffset := (Y * AFrame.Width + X) * 4;
                  AFrame.Pixels[PixelOffset] := Byte(((Rgb and $FF) * Cardinal(Alpha) + 127) div 255);
                  AFrame.Pixels[PixelOffset + 1] := Byte((((Rgb shr 8) and $FF) * Cardinal(Alpha) + 127) div 255);
                  AFrame.Pixels[PixelOffset + 2] := Byte((((Rgb shr 16) and $FF) * Cardinal(Alpha) + 127) div 255);
                  AFrame.Pixels[PixelOffset + 3] := Byte(Alpha);
                  Inc(X);
                  Dec(RunLength);
                end;
            end;
          if not HighNibble then
            begin
              Inc(BytePosition);
              HighNibble := True;
            end;
          Inc(Y, 2);
        end;
    end;
  Result := S_OK;
end;


function TMfVobSubReader.TryGetFrame(const AMediaTimeMs: Int64;
                                     out AFrame: TMfVobSubFrame): Boolean;
var
  LowIndex: Integer;
  HighIndex: Integer;
  Middle: Integer;
  Index: Integer;
  Packet: TBytes;
  StopDelayMs: Int64;
  PacketHr: HRESULT;
  DecodeHr: HRESULT;
  DecodeStartTick: DWORD;
  DecodeElapsedMs: DWORD;
begin
  AFrame.Reset();
  Result := False;
  LowIndex := 0;
  HighIndex := High(FCues);
  Index := -1;
  while LowIndex <= HighIndex do
    begin
      Middle := LowIndex + (HighIndex - LowIndex) div 2;
      if FCues[Middle].StartMs <= AMediaTimeMs then
        begin
          Index := Middle;
          LowIndex := Middle + 1;
        end
      else
        HighIndex := Middle - 1;
    end;
  if Index < 0 then Exit;
  if AMediaTimeMs >= FCues[Index].StopMs then Exit;

  if FCachedFrame.CueIndex <> Index then
    begin
      DecodeStartTick := GetTickCount();
      PacketHr := ReadPacket(FCues[Index], Packet);
      if FAILED(PacketHr) then
        begin
          OutputDebugString(PChar(Format(
            '[MfCast][VobSub] Cue %d packet read failed: HRESULT $%.8x.',
            [Index, DWORD(PacketHr)])));
          Exit;
        end;
      DecodeHr := DecodePacket(Packet, Index, FCachedFrame, StopDelayMs);
      if FAILED(DecodeHr) then
        begin
          OutputDebugString(PChar(Format(
            '[MfCast][VobSub] Cue %d decode failed: HRESULT $%.8x.',
            [Index, DWORD(DecodeHr)])));
          Exit;
        end;
      DecodeElapsedMs := GetTickCount() - DecodeStartTick;
      if DecodeElapsedMs >= 25 then
        OutputDebugString(PChar(Format(
          '[MfCast][VobSub] Cue %d decoded in %d ms.',
          [Index, DecodeElapsedMs])));
      if StopDelayMs > 0 then
        begin
          FCues[Index].StopMs := FCues[Index].StartMs + StopDelayMs;
          if (Index < High(FCues)) and
             (FCues[Index].StopMs > FCues[Index + 1].StartMs) then
            FCues[Index].StopMs := FCues[Index + 1].StartMs;
        end;
    end;
  if AMediaTimeMs >= FCues[Index].StopMs then Exit;
  AFrame := FCachedFrame;
  Result := Length(AFrame.Pixels) > 0;
end;

end.
