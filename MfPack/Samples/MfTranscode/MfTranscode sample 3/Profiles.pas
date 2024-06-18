unit Profiles;

interface

uses
  WinApi.Windows,
  WinApi.ComBaseApi,
  {System}
  System.SysUtils,
  System.Classes,
  {ActiveX}
  WinApi.ActiveX.ObjBase,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.CodecApi,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils;



  // VIDEO PROFILES ////////////////////////////////////////////////////////////

  // H.264 profiles

type
  TH264ProfileInfo = record
    profile: UInt32;
    fps: MFRatio;
    frame_size: MFRatio;
    bitrate: UInt32;
  end;

const
  predef_h264_profiles: array[0..6] of TH264ProfileInfo =
                   (
                     (profile: eAVEncH264VProfile_Base;
                      fps: (Numerator: 15; Denominator: 1);
                      frame_size: (Numerator: 176; Denominator: 144);
                      bitrate: 128000),

                     (profile: eAVEncH264VProfile_Base;
                      fps: (Numerator: 15; Denominator: 1);
                      frame_size: (Numerator: 352; Denominator: 288);
                      bitrate: 384000),

                     (profile: eAVEncH264VProfile_Base;
                      fps: (Numerator: 30; Denominator: 1);
                      frame_size: (Numerator: 352; Denominator: 288);
                      bitrate: 384000),

                     (profile: eAVEncH264VProfile_Base;
                      fps: (Numerator: 29970; Denominator: 1000);
                      frame_size: (Numerator: 320; Denominator: 240);
                      bitrate: 528560),

                     (profile: eAVEncH264VProfile_Base;
                      fps: (Numerator: 15; Denominator: 1);
                      frame_size: (Numerator: 720; Denominator: 576);
                      bitrate: 4000000),

                     (profile: eAVEncH264VProfile_Main;
                      fps: (Numerator: 25; Denominator: 1);
                      frame_size: (Numerator: 720; Denominator: 576);
                      bitrate: 10000000),

                     (profile: eAVEncH264VProfile_Main;
                      fps: (Numerator: 30; Denominator: 1);
                      frame_size: (Numerator: 352; Denominator: 288);
                      bitrate: 10000000)
                    );

  // AUDIO

  // AAC profiles
  // Read: https://learn.microsoft.com/en-us/windows/win32/medfound/aac-encoder
type
  TAACProfileInfo = record
    samplesPerSec: UINT32;
    numChannels: UINT32;   // Must match the input type. 1 (mono), 2 (stereo) or 6 (5.1). (see NOTE 2)
    bitsPerSample: UINT32;
    bytesPerSec: UINT32;
    aacProfile: UINT32;
    aacPayloadType: UINT32; // Specifies the payload type of an Advanced Audio Coding (AAC) stream. (see NOTE 1)
  end;

  // NOTE 1:
  //   aacPayloadType.
  //   The following values are possible.
  //   Value	Meaning
  //   0      The stream contains raw_data_block elements only. (<= Win 8)
  //   1      Audio Data Transport Stream (ADTS). The stream contains an adts_sequence, as defined by MPEG-2. (>= Win 8)
  //   2      Audio Data Interchange Format (ADIF). The stream contains an adif_sequence, as defined by MPEG-2. (>= Win 8)
  //   3      The stream contains an MPEG-4 audio transport stream with a synchronization layer (LOAS) and a multiplex layer (LATM). (>= Win 8)

  // NOTE 2:
  //   numChannels.
  //   Support for 6 audio (5.1) channels was introduced with Windows 10 and is not available for earlier versions of Windows.

  // Pre-defined AAC profiles.
  // These profiles are supported by Media Foundation MFT.
  // See: https://learn.microsoft.com/en-us/windows/win32/medfound/aac-encoder
  // The Microsoft Media Foundation AAC encoder is a Media Foundation Transform that
  // encodes Advanced Audio Coding (AAC) Low Complexity (LC) profile, as defined by ISO/IEC 13818-7 (MPEG-2 Audio Part 7).
  // The AAC encoder does not support encoding to any other AAC profiles, such as Main, SSR, or LTP.
const
  predef_aac_profiles: array[0..3] of TAACProfileInfo =
               (
                 (samplesPerSec: 96000;
                  numChannels: 2;
                  bitsPerSample: 16;
                  bytesPerSec: 24000;
                  aacProfile: $29),

                 (samplesPerSec: 48000;
                  numChannels: 2;
                  bitsPerSample: 16;
                  bytesPerSec: 24000;
                  aacProfile: $29),

                 (samplesPerSec: 44100;
                  numChannels: 2;
                  bitsPerSample: 16;
                  bytesPerSec: 16000;
                  aacProfile: $29),

                 (samplesPerSec: 44100;
                  numChannels: 2;
                  bitsPerSample: 16;
                  bytesPerSec: 12000;
                  aacProfile: $29)
               );


type

  H264_profiles = array of TH264ProfileInfo;
  AAC_profiles = array of TAACProfileInfo;


  function AddH264Profile(H264Profile: TH264ProfileInfo;
                          IncludePredefinedProfiles: Boolean = True): Integer;

  function AddAACProfile(AACProfile: TAACProfileInfo;
                         IncludePredefinedProfiles: Boolean = True): Integer;


implementation


function AddH264Profile(H264Profile: TH264ProfileInfo;
                        IncludePredefinedProfiles: Boolean = True): Integer;
begin
//  if IncludePredefinedProfiles then
//    SetLength(H264_profiles,
//              Length(predef_H264_profiles) + Length(H264_profiles) + 1)
//  else
//    SetLength(H264_profiles,
//              Length(H264_profiles) + 1);
//  // Add the extra profile.
//  H264_profiles[Length(H264_profiles)] := H264Profile;
//  Result := Length(H264_profiles);
end;


function AddAACProfile(AACProfile: TAACProfileInfo;
                       IncludePredefinedProfiles: Boolean = True): Integer;
begin
//  if IncludePredefinedProfiles then
//    SetLength(AAC_Profiles,
//                Length(predef_AAC_Profiles) + 1)
//  else
//    SetLength(AAC_Profiles,
//                Length(AAC_Profiles) + 1);
  // Add the extra profile.
//  AAC_profiles[Length(AAC_profiles)] := AACProfile;
//  Result := Length(AAC_profiles);
end;

end.
