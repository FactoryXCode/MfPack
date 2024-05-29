unit AudioMftClass;

interface

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  Profiles;

type

  TAudioProfile = (AAC, MP3);

  TAudioMft = class(TObject)
    private
      pa_AudioFmts: TMFAudioFormatArray;
      pa_AudioProfile: TAudioProfile;
      //FAAC_profiles: TAAC_profiles;
      procedure ResetAudioFormatArray();

    public


      constructor Create();
      destructor Destroy(); override;

      // Get audio
      function GetAudioFormats(const AudioFormat: TGuid;
                               mftEnumFlag: MFT_ENUM_FLAG; {example: MFT_ENUM_FLAG_ALL}
                               out aAudioFmts: TMFAudioFormatArray): HResult;



      property AudioFormats: TMFAudioFormatArray read pa_AudioFmts;
      property AudioProfiles: TAudioProfile read pa_AudioProfile;

  end;


implementation


constructor TAudioMft.Create();
begin
  inherited Create();

end;


destructor TAudioMft.Destroy();
begin

  inherited Destroy;
end;


function TAudioMft.GetAudioFormats(const AudioFormat: TGuid;
                                          mftEnumFlag: MFT_ENUM_FLAG;
                                          out aAudioFmts: TMFAudioFormatArray): HResult;
var
  hr: HResult;
begin
  ResetAudioFormatArray();
  // Get the encoder formats from the selected audioformat.
  hr := GetWinAudioEncoderFormats(AudioFormat,
                                  mftEnumFlag, {MFT_ENUM_FLAG_TRANSCODE_ONLY,}
                                  aAudioFmts);
  Result := hr;
end;





procedure TAudioMft.ResetAudioFormatArray();
var
  i: Integer;

begin
  // Reset the array
  if Length(pa_AudioFmts) > 0 then
    begin
      for i := 0 to Length(pa_AudioFmts) -1 do
        pa_AudioFmts[i].Reset;
      SetLength(pa_AudioFmts,
                0);
    end;
end;

end.
