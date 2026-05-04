unit RDJ.TrackQuality;

interface

uses
  System.SysUtils,
  System.IOUtils,
  RDJ.PlaylistTypes;

procedure RDJUpdateTrackQuality(var ATrack: TRDJTrack);

implementation

function RDJCodecFromExt(const AExt: string): string;
var
  Ext: string;

begin

  Ext := LowerCase(AExt);

  if Ext = '.mp3' then
    Result := 'MP3'
  else if Ext = '.flac' then
    Result := 'FLAC'
  else if Ext = '.wav' then
    Result := 'WAV'
  else if (Ext = '.m4a') or (Ext = '.aac') then
    Result := 'AAC'
  else if Ext = '.ogg' then
    Result := 'OGG'
  else if Ext = '.wma' then
    Result := 'WMA'
  else if (Ext = '.aif') or (Ext = '.aiff') then
    Result := 'AIFF'
  else
    Result := 'Audio';
end;


function RDJChannelText(const AChannels: Integer): string;
begin

  case AChannels of
    1: Result := 'Mono';
    2: Result := 'Stereo';
  else
    if AChannels > 0 then
      Result := Format('%d ch', [AChannels])
    else
      Result := '';
  end;
end;


function RDJSampleRateText(const ASampleRate: Integer): string;
var
  KHz: Double;

begin

  Result := '';

  if ASampleRate <= 0 then
    Exit;

  KHz := ASampleRate / 1000.0;

  if Frac(KHz) = 0 then
    Result := Format('%d kHz', [Round(KHz)])
  else
    Result := Format('%.1f kHz', [KHz]);
end;


function RDJEstimateBitrateKbps(const AFileSize: Int64;
                                const ADurationMs: Int64): Integer;
var
  BitsPerSecond: Double;

begin

  Result := 0;

  if (AFileSize <= 0) or (ADurationMs <= 0) then
    Exit;

  BitsPerSecond := (AFileSize * 8.0) / (ADurationMs / 1000.0);
  Result := Round(BitsPerSecond / 1000.0);
end;


procedure RDJUpdateTrackQuality(var ATrack: TRDJTrack);
var
  Parts: string;
  SR: string;
  CH: string;

  procedure AddPart(const S: string);
  begin

    if Trim(S) = '' then
      Exit;

    if Parts = '' then
      Parts := S
    else
      Parts := Parts + ' / ' + S;
  end;

begin

  if Trim(ATrack.FileExt) = '' then
    ATrack.FileExt := LowerCase(ExtractFileExt(ATrack.FullPath));

  if Trim(ATrack.Codec) = '' then
    ATrack.Codec := RDJCodecFromExt(ATrack.FileExt);

  if (ATrack.FileSize <= 0) and FileExists(ATrack.FullPath) then
    begin
      try
        ATrack.FileSize := TFile.GetSize(ATrack.FullPath);
      except
        ATrack.FileSize := 0;
      end;
    end;

  if (ATrack.FileModifiedUtc <= 0) and FileExists(ATrack.FullPath) then
    begin
      try
        ATrack.FileModifiedUtc := TFile.GetLastWriteTime(ATrack.FullPath);
      except
        ATrack.FileModifiedUtc := 0;
      end;
    end;

  if (ATrack.BitrateKbps <= 0) and (ATrack.BitRate > 0) then
    begin
      if ATrack.BitRate > 1000 then
        ATrack.BitrateKbps := ATrack.BitRate div 1000
      else
        ATrack.BitrateKbps := ATrack.BitRate;
    end;

  if (ATrack.BitrateKbps <= 0) then
    ATrack.BitrateKbps := RDJEstimateBitrateKbps(ATrack.FileSize,
                                                 ATrack.DurationMs);

  Parts := '';

  AddPart(ATrack.Codec);

  if SameText(ATrack.Codec, 'MP3') or
     SameText(ATrack.Codec, 'AAC') or
     SameText(ATrack.Codec, 'OGG') or
     SameText(ATrack.Codec, 'WMA') then
    begin
      if ATrack.BitrateKbps > 0 then
        AddPart(Format('%d kbps', [ATrack.BitrateKbps]));
    end
  else
    begin
      SR := RDJSampleRateText(ATrack.SampleRate);
      CH := RDJChannelText(ATrack.Channels);

      AddPart(SR);

      if ATrack.BitsPerSample > 0 then
        AddPart(Format('%d-bit', [ATrack.BitsPerSample]));

      AddPart(CH);
    end;

  ATrack.QualityLabel := Parts;
end;

end.
