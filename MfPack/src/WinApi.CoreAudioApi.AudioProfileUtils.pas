// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - EndpointVolume
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.EndPointVolume.pas
// Kind: Pascal / Delphi unit
// Release date: 22-02-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: Audio tools like profiles, enum audio mft's etc.
//              Supported audio are AAC, MP3, Dolby AC-3.
//
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 30/06/2024 All                 RammStein release  SDK 10.0.26100.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
//
// Related objects: -
// Related projects: MfPackX317
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.0
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/medfound/supported-media-formats-in-media-foundation
//         https://learn.microsoft.com/en-us/windows/win32/medfound/audio-media-types
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
unit WinApi.CoreAudioApi.AudioProfileUtils;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  System.SysUtils,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfError;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

type

  /// Profiles /////////////////////////////////////////////////////////////////

  // PCM profile.
  TPCMProfileInfo = record
    sampleRate: UINT32;
    bitsPerSample: UINT32;
    channels: UINT32;
  end;

  /// AAC profiles.
  /// <see href="https://learn.microsoft.com/en-us/windows/win32/medfound/aac-encoder">[AAC profiles]</see>
  TAACProfileLevel = record
    audioProfileLevelIndication: UINT32;
    ProfileDescription: ShortString;
  end;

  TAACPayloadType = record
    payload: UINT32;
    payloadDescription: ShortString;
  end;

  TAACProfileInfo = record
    samplesPerSec: UINT32;
    channels: UINT32;       // Must match the input type. 1 (mono), 2 (stereo) or 6 (5.1). (see NOTE 2)
    bitsPerSample: UINT32;
    bytesPerSec: UINT32;
    aacProfile: UINT32;
    aacPayloadType: UINT32; // Specifies the payload type of an Advanced Audio Coding (AAC) stream. (see NOTE 1)
  end;

  // Dolby AC3 profile.
  TDolbyAC3ProfileInfo = record
    sampleRate: UINT32;
    bitsPerSample: UINT32;
    channels: UINT32;      // Must match the input type. 1 (mono) or 2 (stereo).
    channelMask: UINT32;
  end;

  // MP3 profile.
  TMP3ProfileInfo = record
    mpegVersion: UINT32;   // 1 (MPEG_1) or 2 (MPEG_2).
    channels: UINT32;      // Must match the input type. 1 (mono) or 2 (stereo).
    sampleRate: UINT32;
    bytesPerSecond: UINT32;
   end;

   TAAC_profiles = array of TAACProfileInfo;
   TAC3_profiles = array of TDolbyAC3ProfileInfo;
   TMP3_profiles = array of TMP3ProfileInfo;


const
  /// <summary>
  /// audioProfileLevelIndication (MF_MT_AAC_PROFILE_LEVEL_INDICATION) values.
  /// </summary>
  AACProfileLevelArray: array[0..12] of TAACProfileLevel =
                        (
                         (audioProfileLevelIndication: $28;
                          ProfileDescription: 'AAC Profile L2'),

                         (audioProfileLevelIndication: $29;
                          ProfileDescription: 'AAC Profile L2'),

                         (audioProfileLevelIndication: $2A;
                          ProfileDescription: 'AAC Profile L4'),

                         (audioProfileLevelIndication: $2B;
                          ProfileDescription: 'AAC Profile L5'),

                         (audioProfileLevelIndication: $2C;
                          ProfileDescription: 'High Efficiency v1 AAC Profile L2'),

                         (audioProfileLevelIndication: $2E;
                          ProfileDescription: 'High Efficiency v1 AAC Profile L4'),

                         (audioProfileLevelIndication: $2F;
                          ProfileDescription: 'High Efficiency v1 AAC Profile L5'),

                         (audioProfileLevelIndication: $30;
                          ProfileDescription: 'High Efficiency v2 AAC Profile L2'),

                         (audioProfileLevelIndication: $31;
                          ProfileDescription: 'High Efficiency v2 AAC Profile L3'),

                         (audioProfileLevelIndication: $32;
                          ProfileDescription: 'High Efficiency v2 AAC Profile L4'),

                         (audioProfileLevelIndication: $33;
                          ProfileDescription: 'High Efficiency v2 AAC Profile L5'),

                         (audioProfileLevelIndication: $50;
                          ProfileDescription: 'High Efficiency v2 AAC Profile for 8 channels (7.1)'),

                         (audioProfileLevelIndication: $52;
                          ProfileDescription: 'High Efficiency v2 AAC Profile for 8 channels (7.1)')
                        );

  /// <summary> aacPayloadType.
  /// The following values are possible.
  ///     Value	 Meaning
  ///     -----  ---------------------------------------------------------------
  ///     0      The stream contains raw_data_block elements only. (<= Win 8)
  ///     1      Audio Data Transport Stream (ADTS). The stream contains an adts_sequence, as defined by MPEG-2. (>= Win 8)
  ///     IMPORTANT NOTE:
  ///       Starting in Windows 8, the value can be 0 (Raw AAC) or 1 (ADTS).
  ///       Other values (2 (ADIF) and 3 (LOAS)) are not supported!
  ///     2      Audio Data Interchange Format (ADIF). The stream contains an adif_sequence, as defined by MPEG-2.
  ///     3      The stream contains an MPEG-4 audio transport stream with a synchronization layer (LOAS) and a multiplex layer (LATM).
  /// </summary>
  AACPayloadTypes: array[0..1] of TAACPayloadType =
                  (
                    (payload: 0;
                     payloadDescription: 'Stream contains raw_data_block elements only (Raw AAC).'),

                    (payload: 1;
                     payloadDescription: 'Audio Data Transport Stream (ADTS). The stream contains an adts_sequence, as defined by MPEG-2.')

                    // Not supported!

                    //(payload: 2;
                    // payloadDescription: 'Audio Data Interchange Format (ADIF). The stream contains an adif_sequence, as defined by MPEG-2.'),

                    //(payload: 3;
                    // payloadDescription: 'Stream contains an MPEG-4 audio transport stream with a synchronization layer (LOAS) and a multiplex layer (LATM).')
                  );

type
  // For now AAC, Dolby AC3 and MP3
  TAudioProfile = (AAC,
                   DOLBY_AC3,
                   MP3);

  TAudioMft = class(TObject)
    private

      pa_AudioFmts: TMFAudioFormatArray;

      /// <summary> Pre-defined AAC profiles.
      /// These profiles are supported by Media Foundation MFT.
      /// The Microsoft Media Foundation AAC encoder is a Media Foundation Transform that
      /// encodes Advanced Audio Coding (AAC) Low Complexity (LC) profile, as defined by ISO/IEC 13818-7 (MPEG-2 Audio Part 7).
      /// The AAC encoder does not support encoding to any other AAC profiles, such as Main, SSR, or LTP.
      /// numChannels.
      /// Support for 6 audio (5.1) channels was introduced with Windows 10 and is not available for earlier versions of Windows.
      /// </summary>
      /// <see href="https://learn.microsoft.com/en-us/windows/win32/medfound/aac-encoder">[AAC Encoder]</see>
      predef_aac_profiles: array of TAACProfileInfo;

      //
      pa_AudioProfile: TAudioProfile;

      // Audio Types Profile array's
      pa_AAC_profiles: TAAC_profiles;
      pa_AC3_profiles: TAC3_profiles;
      pa_MP3_profiles: TMP3_profiles;


      /// <summary>
      /// Initialise the predefined AAC profiles array.
      /// </summary>
      procedure InitialisePreDefAACProfiles();
      /// <summary>
      /// Resets the pa_AudioFmts fields and set the length to zero.
      /// </summary>
      procedure ResetAudioFormatArray();

      /// <summary>
      /// Set pa_AAC_profiles length to zero.
      /// </summary>
      procedure ResetAACProfileInfoArray();
      /// <summary>
      /// Set pa_AC3_profiles length to zero.
      /// </summary>
      procedure ResetAC3ProfileInfoArray();
      /// <summary>
      /// Set pa_MP3_profiles length to zero.
      /// </summary>
      procedure ResetMP3ProfileInfoArray();


    public
      /// <summary>
      /// Creates the TAudioMft class.
      /// </summary>
      constructor Create();
      /// <summary>
      /// Call YourAudioProfileClass.FreeAndNil to destroy this class.
      /// </summary>
      destructor Destroy(); override;

      /// <summary>
      /// Get the supported audio formats by a given AudioFormat guid.
      /// </summary>
      /// <param name="AudioFormat"> AudioFormat_AAC, AudioFormat_MP3 etc.</param>
      /// <param name="mftEnumFlag"> MFT_ENUM_FLAG, MFT_ENUM_FLAG_ALL, (MFT_ENUM_FLAG_ASYNCMFT or MFT_ENUM_FLAG_HARDWARE) etc.</param>
      /// <returns> S_OK when succesful any other HResult is an error.</returns>
      function GetAudioFormats(const AudioFormat: TGuid;
                               mftEnumFlag: MFT_ENUM_FLAG): HResult;


      // Add a new profile to existing profiles. ///////////////////////////////

      /// <summary>
      /// Adds a new profile to the profiles list.
      /// </summary>
      /// <param name="AACProfile"> The new profile. </param>
      /// <param name="IncludePredefinedProfiles"> Includes the predefined profiles. </param>
      /// <returns> The number of profiles.
      /// </returns>
      function AddAACProfile(AACProfile: TAACProfileInfo;
                             IncludePredefinedProfiles: Boolean = True): Integer;

      //
      // Create various audio output types. ////////////////////////////////////
      //

      /// <summary>
      /// Create an uncompessed PCM audio type.
      /// </summary>
      /// <param name="pcmProfile"> PCM profile. </param>
      /// <param name="pcmType"> Mediatype interface. </param>
      /// <returns> S_OK when succesful any other HResult is an error. </returns>
      function CreatePCMAudioType(pcmProfile: TPCMProfileInfo;
                                  out pcmType: IMFMediaType): HRESULT;

      /// <summary>
      /// Create an uncompessed AAC audio type.
      /// </summary>
      /// <param name="profile"> TAACProfile record. </param>
      /// <param name="aacType"> AAC audiotype (IMFMediaType). </param>
      /// <returns> S_OK when succesful any other HResult is an error. </returns>
      function CreateAACAudioType(profile: TAACProfileInfo;
                                  out aacType: IMFMediaType): HRESULT;


      /// <summary>
      /// Create a Dolby AC-3 audio type.
      /// </summary>
      /// <param name="dolbyAC3ProfileInfo"> Dolby AC3 profile. </param>
      /// <param name="ac3Type"> Dolby AC3 audiotype. </param>
      /// <returns> S_OK when succesful any other HResult is an error. </returns>
      function CreateDolbyAC3AudioType(dolbyAC3ProfileInfo: TDolbyAC3ProfileInfo;
                                       out ac3Type: IMFMediaType): HRESULT;

      /// <summary>
      /// Create an MP3 audio type.
      /// </summary>
      /// <param name="mp3ProfileInfo"> MP3 Profile record. </param>
      /// <param name="mp3Type"> MP3 audiotype. </param>
      /// <returns> S_OK when succesful any other HResult is an error. </returns>
      function CreateMP3AudioType(mp3ProfileInfo: TMP3ProfileInfo;
                                  out mp3Type: IMFMediaType): HRESULT;


      //
      // Converters. ///////////////////////////////////////////////////////////
      //

      /// <summary>
      /// Given an audio media type (which might describe a compressed audio.
      /// format), returns a media type that describes the equivalent
      /// uncompressed PCM format.
      /// </summary>
      /// <param name="srcType"> Encoded audio type source. </param>
      /// <param name="pcmType"> PCM audiotype. </param>
      /// <returns> S_OK when succesful any other HResult is an error. </returns>
      function ConvertAudioTypeToPCM(srcType: IMFMediaType;
                                     out pcmType: IMFMediaType): HRESULT;

      /// <summary>
      /// Given an audio media type (which might describe a compressed audio
      /// format), returns a media type that describes the equivalent
      /// uncompressed AAC format.
      /// </summary>
      /// <param name="srcType"> Encoded audio type source. </param>
      /// <param name="aacType"> Converted AAC audio type. </param>
      /// <returns> S_OK when succesful any other HResult is an error. </returns>
      function ConvertAudioTypeToAAC(srcType: IMFMediaType;
                                     out aacType: IMFMediaType): HRESULT;

      /// <summary>
      /// Lists the audioformats from method GetAudioFormats.
      /// </summary>
      property AudioFormats: TMFAudioFormatArray read pa_AudioFmts;
      /// <summary>
      /// Lists audio profiles.
      /// </summary>
      property AudioProfile: TAudioProfile read pa_AudioProfile;

      // AAC payload
      //property AACPayloadTypes: TAACPayloadTypes read pa_AACPayloadTypes;

      // Audio Types Profile array's
      property  AACprofiles: TAAC_profiles read pa_AAC_profiles;
      property  AC3profiles: TAC3_profiles read pa_AC3_profiles;
      property  MP3profiles: TMP3_profiles read pa_MP3_profiles;


  end;


implementation



constructor TAudioMft.Create();
begin
  inherited Create();
  InitialisePreDefAACProfiles();
end;


destructor TAudioMft.Destroy();
begin
  ResetAudioFormatArray();
  ResetAACProfileInfoArray();
  ResetAC3ProfileInfoArray();
  ResetMP3ProfileInfoArray();
  inherited Destroy;
end;


procedure TAudioMft.InitialisePreDefAACProfiles();
begin
  SetLength(predef_aac_profiles,
            4);

  predef_aac_profiles[0].samplesPerSec := 96000;
  predef_aac_profiles[0].channels := 2;
  predef_aac_profiles[0].bitsPerSample := 16;
  predef_aac_profiles[0].bytesPerSec := 24000;
  predef_aac_profiles[0].aacProfile := $29;

  predef_aac_profiles[1].samplesPerSec := 48000;
  predef_aac_profiles[1].channels := 2;
  predef_aac_profiles[1].bitsPerSample := 16;
  predef_aac_profiles[1].bytesPerSec := 24000;
  predef_aac_profiles[1].aacProfile := $29;

  predef_aac_profiles[2].samplesPerSec := 44100;
  predef_aac_profiles[2].channels := 2;
  predef_aac_profiles[2].bitsPerSample := 16;
  predef_aac_profiles[2].bytesPerSec := 16000;
  predef_aac_profiles[2].aacProfile := $29;

  predef_aac_profiles[3].samplesPerSec := 44100;
  predef_aac_profiles[3].channels := 2;
  predef_aac_profiles[3].bitsPerSample := 16;
  predef_aac_profiles[3].bytesPerSec := 12000;
  predef_aac_profiles[3].aacProfile := $29;

end;


function TAudioMft.GetAudioFormats(const AudioFormat: TGuid;
                                   mftEnumFlag: MFT_ENUM_FLAG): HResult;
var
  hr: HResult;

begin
  ResetAudioFormatArray();
  // Get the supported encoder formats from the selected audioformat.
  hr := GetWinAudioEncoderFormats(AudioFormat,
                                  mftEnumFlag,
                                  pa_AudioFmts);
  Result := hr;
end;


function TAudioMft.AddAACProfile(AACProfile: TAACProfileInfo;
                                 IncludePredefinedProfiles: Boolean = True): Integer;
begin
  if IncludePredefinedProfiles then
    SetLength(pa_AAC_profiles,
              Length(predef_AAC_Profiles) + 1)
  else
    SetLength(pa_AAC_profiles,
              Length(pa_AAC_profiles) + 1);

  // Add the extra profile.
  pa_AAC_profiles[Length(pa_AAC_profiles)] := AACProfile;
  Result := Length(pa_AAC_profiles);
end;


function TAudioMft.CreatePCMAudioType(pcmProfile: TPCMProfileInfo;
                                      out pcmType: IMFMediaType): HRESULT;
var
  hr: HRESULT;
  aType: IMFMediaType;
  blockAlign: UINT32;
  bytesPerSecond: UINT32;

label
  done;

begin

  // Calculate derived values.
  blockAlign := pcmProfile.channels * (pcmProfile.bitsPerSample div 8);
  bytesPerSecond := blockAlign * pcmProfile.sampleRate;

  // Create the empty media type.
  hr := MFCreateMediaType(aType);
  if FAILED(hr) then
    goto done;

  // Set attributes on the type.
  hr := aType.SetGUID(MF_MT_MAJOR_TYPE,
                      MFMediaType_Audio);
  if FAILED(hr) then
    goto done;

  hr := aType.SetGUID(MF_MT_SUBTYPE,
                      MFAudioFormat_PCM);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                        pcmProfile.channels);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                        pcmProfile.sampleRate);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                        blockAlign);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                        bytesPerSecond);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                        pcmProfile.bitsPerSample);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUINT32(MF_MT_ALL_SAMPLES_INDEPENDENT,
                        UINT32(TRUE));
  if FAILED(hr) then
    goto done;

  // Return the type to the caller.
  pcmType := aType;

done:
  Result := hr;
end;


//
function TAudioMft.CreateAACAudioType(profile: TAACProfileInfo;
                                      out aacType: IMFMediaType): HRESULT;
var
  hr: HRESULT;
  aType: IMFMediaType;
  blockAlign: UINT32;
  bytesPerSecond: UINT32;
  sampleRateChecked: UINT32;
  channelsChecked: UINT32;
  bytesPerSecondChecked: UINT32;
  payloadTypeChecked: UINT32;

label
  done;

begin

  // What needs to be set on output:
  // - Major type. Must be MFMediaType_Audio.
  // - Sub type. Must be MFAudioFormat_AAC or MEDIASUBTYPE_RAW_AAC1 when using AVI.
  // - Audio profile and level. (When not used or unknown, this value should be $FE ("no audio profile specified")
  // - Bit rate of the encoded AAC stream, in bytes per second. (MF_MT_AUDIO_AVG_BYTES_PER_SECOND)
  // - Payload type. Applies only to MFAudioFormat_AAC.
  //   Note: MF_MT_AAC_PAYLOAD_TYPE is optional.
  //         If this attribute is not specified, the default value 0 is used,
  //         which specifies the stream contains raw_data_block elements only.
  //
  // - Bit depth of the decoded PCM audio. (MF_MT_AUDIO_BITS_PER_SAMPLE)
  // - Assignment of audio channels to speaker positions.
  // - Number of channels, including the low frequency (LFE) channel, if present.
  // - Sample rate, in samples per second.
  // - Additional format data for a media type. (MF_MT_USER_DATA)

  // Calculate the bitrate from derived values.
  blockAlign := profile.Channels * (16 div 8);
  bytesPerSecond := blockAlign * profile.samplesPerSec;

  // Create an empty media type.
  hr := MFCreateMediaType(aType);
  if FAILED(hr) then
    goto done;

  // Set attributes on the type.
  hr := aType.SetGUID(MF_MT_MAJOR_TYPE,
                      MFMediaType_Audio);
  if FAILED(hr) then
    goto done;

  hr := aType.SetGUID(MF_MT_SUBTYPE,
                      MFAudioFormat_AAC);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE,
                        16); // Must be 16!
  if FAILED(hr) then
    goto done;

  // Only 44.1 and 48 khz samplerates are supported by AAC.
  case profile.samplesPerSec of
    44100,
    48000: sampleRateChecked := profile.samplesPerSec;
    else  // Set to default.
      sampleRateChecked := 44100;
  end;

  hr := aType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                        sampleRateChecked);
  if FAILED(hr) then
    goto done;

  // Number of supported channels are 1, 2 and (>= Win 10) 6 (5.1).
  case profile.Channels of   //channelsChecked
    1,
    2,
    6: channelsChecked := profile.Channels;
    else  // Set to default.
      channelsChecked := 2;
  end;

  hr := aType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                        channelsChecked);
  if FAILED(hr) then
    goto done;

  // After the output type is set, the encoder derives the following values and adds them to the media type:

  // The following values are supported: 12000, 16000, 20000 and 24000
  // Note: If using 6 channels, the values are multiplied by 6.
  //       The default value for both mono and stereo is 12000 (96 Kbps).
  //       The default value for 6 channels is 60000.

  // Set default.
  bytesPerSecondChecked := 12000;

  if (channelsChecked = 1) or (channelsChecked = 2) then
    case bytesPerSecond of
      12000,
      16000,
      20000,
      24000: bytesPerSecondChecked := bytesPerSecond;
      else // Set to default.
        bytesPerSecondChecked := 12000;
    end;

  if (channelsChecked = 6) then
    case bytesPerSecond of
      12000,
      16000,
      20000,
      24000: bytesPerSecondChecked := bytesPerSecond * 6;
      else // Set to default.
        bytesPerSecondChecked := 60000;
    end;


  // Bit rate.
  hr := aType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                        bytesPerSecondChecked {profile.bytesPerSec});
  if FAILED(hr) then
    goto done;

  // AAC payload type.
  case profile.aacPayloadType of
    0,
    1,
    2,
    3: payloadTypeChecked := profile.aacPayloadType;
    else // Set to default.
      payloadTypeChecked := 0;
  end;

  hr := aType.SetUINT32(MF_MT_AAC_PAYLOAD_TYPE,
                        payloadTypeChecked);
  if FAILED(hr) then
    goto done;

  hr := aType.SetUInt32(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION,
                        profile.aacProfile);
  if FAILED(hr) then
    goto done;

  aacType := aType;

done:
  Result := hr;
end;


function TAudioMft.CreateDolbyAC3AudioType(dolbyAC3ProfileInfo: TDolbyAC3ProfileInfo;
                                           out ac3Type: IMFMediaType): HRESULT;
var
  hr: HRESULT;
  aType: IMFMediaType;
  sampleRateChecked: UINT32;
  channelsChecked: UINT32;
  channelMaskChecked: UINT32;

label
  done;

begin

  // Create an empty media type.
  hr := MFCreateMediaType(aType);
  if FAILED(hr) then
    goto done;

  // Set attributes on the type.
  hr := aType.SetGUID(MF_MT_MAJOR_TYPE,
                      MFMediaType_Audio);
  if FAILED(hr) then
    goto done;

  hr := aType.SetGUID(MF_MT_SUBTYPE,
                      MFAudioFormat_Dolby_AC3);
  if FAILED(hr) then
    goto done;

  // Only 32, 44.1 and 48 khz sample rates are supported by Media Foundation/DAC3.
  case dolbyAC3ProfileInfo.sampleRate of
    32000,
    44100,
    48000: sampleRateChecked := dolbyAC3ProfileInfo.sampleRate;
    else  // Set to default.
      sampleRateChecked := 44100;
  end;

  hr := aType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                        sampleRateChecked);
  if FAILED(hr) then
    goto done;


  // Dolby AC3 only supports 1 or 2 channels.
  case dolbyAC3ProfileInfo.channels of
    1,
    2: channelsChecked := dolbyAC3ProfileInfo.channels;
    else  // Set to default.
      channelsChecked := 2;
  end;

  hr := aType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                        channelsChecked);
  if FAILED(hr) then
    goto done;

  // Specifie the assignment of audio channels to speaker positions.
  // Optional. If set, the value must be $3 for stereo (front left and right channels) or
  //           $4 for mono (front center channel).
  case dolbyAC3ProfileInfo.channelMask of
    $3,
    $4: channelMaskChecked := dolbyAC3ProfileInfo.channelMask;
    else  // Set to default.
      channelMaskChecked := $3;
  end;


  hr := aType.SetUINT32(MF_MT_AUDIO_CHANNEL_MASK,
                        channelMaskChecked);
  if FAILED(hr) then
    goto done;


done:
  Result := hr;
end;


function TAudioMft.CreateMP3AudioType(mp3ProfileInfo: TMP3ProfileInfo;
                                      out mp3Type: IMFMediaType): HRESULT;
var
  hr: HRESULT;
  aType: IMFMediaType;
  channelsChecked: UINT32;
  sampleRateChecked: UINT32;
  bytesPerSecondChecked: UINT32;

label
  done;

begin
  // Create an empty media type.
  hr := MFCreateMediaType(aType);
  if FAILED(hr) then
    goto done;

  // Set attributes on the type.
  hr := aType.SetGUID(MF_MT_MAJOR_TYPE,
                      MFMediaType_Audio);
  if FAILED(hr) then
    goto done;

  hr := aType.SetGUID(MF_MT_SUBTYPE,
                      MFAudioFormat_MP3);
  if FAILED(hr) then
    goto done;

  // Number of supported channels are 1 or 2.
  case MP3ProfileInfo.channels of
    1,
    2: channelsChecked := MP3ProfileInfo.channels;
    else  // Set to default.
      channelsChecked := 2;
  end;

  hr := aType.SetUINT32(MF_MT_AUDIO_NUM_CHANNELS,
                        channelsChecked);
  if FAILED(hr) then
    goto done;


  // Only 32, 44.1 and 48 khz sample rates are supported by Media Foundation/MP3 MPEG_1.
  if (mp3ProfileInfo.mpegVersion = 1) then
    begin
      case mp3ProfileInfo.sampleRate of
        32000,
        44100,
        48000: sampleRateChecked := mp3ProfileInfo.sampleRate;
        else  // Set to default.
          sampleRateChecked := 44100;
      end;

      hr := aType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            sampleRateChecked);
      if FAILED(hr) then
        goto done;

      // Bit rate of the encoded MP3 MPEG_1 stream, in bytes per second.
      // The encoder supports all bit rates defined by the standard (32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, or 320 kbps).
      // The default bit rates are 128 Kbps for mono and 320 Kbps for stereo.
      // Use this attribute to specify the encoded bit rate.
      case mp3ProfileInfo.bytesPerSecond of
        32,
        40,
        48,
        56,
        64,
        80,
        96,
        112,
        128,
        160,
        192,
        224,
        256,
        320: bytesPerSecondChecked := mp3ProfileInfo.bytesPerSecond;
        else  // Set to defaults.
          if (mp3ProfileInfo.channels = 1) then
            bytesPerSecondChecked := 128
          else // 2 channels
            bytesPerSecondChecked := 320;
      end;

      hr := aType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                              bytesPerSecondChecked);
    end // MPEG_1
  else if (mp3ProfileInfo.mpegVersion = 2) then
    begin
      case mp3ProfileInfo.sampleRate of
        32000,
        44100,
        48000: sampleRateChecked := mp3ProfileInfo.sampleRate;
        else  // Set to default.
          sampleRateChecked := 44100;
      end;

      hr := aType.SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND,
                            sampleRateChecked);
      if FAILED(hr) then
        goto done;

      // Bit rate of the encoded MP3 MPEG_1 stream, in bytes per second.
      // The encoder supports all bit rates defined by the standard (32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, or 320 kbps).
      // The default bit rates are 128 Kbps for mono and 320 Kbps for stereo.
      // Use this attribute to specify the encoded bit rate.
      case mp3ProfileInfo.bytesPerSecond of
        32,
        40,
        48,
        56,
        64,
        80,
        96,
        112,
        128,
        160,
        192,
        224,
        256,
        320: bytesPerSecondChecked := mp3ProfileInfo.bytesPerSecond;
        else  // Set to defaults.
          if (mp3ProfileInfo.channels = 1) then
            bytesPerSecondChecked := 128
          else // 2 channels
            bytesPerSecondChecked := 320;
      end;

      hr := aType.SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                            bytesPerSecondChecked);
    end;

done:
  Result := hr;
end;


//-------------------------------------------------------------------
// ConvertAudioTypeToPCM
//
// Given an audio media type (which might describe a compressed audio
// format), returns a media type that describes the equivalent
// uncompressed PCM format.
//-------------------------------------------------------------------
function TAudioMft.ConvertAudioTypeToPCM(srcType: IMFMediaType;       // Encoded audio type.
                                         out pcmType: IMFMediaType    // Converted audio type.
                                         ): HRESULT;
var
  hr: HRESULT;
  pcmProfile: TPCMProfileInfo;
  majortype: TGUID;
  subtype: TGUID;

label
  done;

begin

  hr := srcType.GetMajorType(majortype);
  if FAILED(hr) then
    goto done;

  // Check majortype
  if not IsEqualGuid(majortype,
                     MFMediaType_Audio) then
    begin
      hr := MF_E_INVALIDMEDIATYPE;
      goto done;
    end;

  // Get the audio subtype.
  hr := srcType.GetGUID(MF_MT_SUBTYPE,
                        subtype);
  if FAILED(hr) then
    goto done;

  if IsEqualGuid(subtype,
                 MFAudioFormat_PCM) then
    begin
      // This is already a PCM audio type. Return the same pointer.
      pcmType := srcType;
      Hr := S_OK;
      goto done;
    end;

  // Get the information from the audio format.

  pcmProfile.channels := MFGetAttributeUINT32(srcType,
                                              MF_MT_AUDIO_NUM_CHANNELS,
                                              2);

  pcmProfile.sampleRate := MFGetAttributeUINT32(srcType,
                                                MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                0);

  // Note: Some encoded audio formats do not contain a value for bits/sample.
  // In that case, use a default value of 16. Most codecs will accept this value.
  pcmProfile.bitsPerSample := MFGetAttributeUINT32(srcType,
                                                   MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                   16);

  if (pcmProfile.channels = 0) or (pcmProfile.bitsPerSample = 0) then
    begin
      pcmProfile.channels := 2;
      pcmProfile.bitsPerSample := 16
    end;

  // Create the corresponding PCM audio type.
  hr := CreatePCMAudioType(pcmProfile,
                           pcmType);
done:
  Result := hr;
end;


//-------------------------------------------------------------------
// ConvertAudioTypeToAAC
//
// Given an audio media type (which might describe a compressed audio
// format), returns a media type that describes the equivalent
// uncompressed AAC format.
//-------------------------------------------------------------------
function TAudioMft.ConvertAudioTypeToAAC(srcType: IMFMediaType;       // Encoded audio type.
                                         out aacType: IMFMediaType    // Converted audio type.
                                         ): HRESULT;
var
  hr: HRESULT;
  majortype: TGUID;
  subtype: TGUID;
  aProfile: TAACProfileInfo;


label
  done;

begin

  hr := srcType.GetMajorType(majortype);
  if FAILED(hr) then
    goto done;

  // Check majortype
  if not IsEqualGuid(majortype,
                     MFMediaType_Audio) then
    begin
      hr := MF_E_INVALIDMEDIATYPE;
      goto done;
    end;

  // Get the audio subtype.
  hr := srcType.GetGUID(MF_MT_SUBTYPE,
                        subtype);
  if FAILED(hr) then
    goto done;

  if IsEqualGuid(subtype,
                 MFAudioFormat_AAC) then
    begin
      // This is already an AAC audio type. Return the same pointer.
      aacType := srcType;
      Hr := S_OK;
      goto done;
    end;

  // Get the sample rate and other information from the audio format needed for AAC.

  aProfile.samplesPerSec := MFGetAttributeUINT32(srcType,
                                                 MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                 0);

  // AAC supports 1, 2 or 6 (5.1) channels only!
  aProfile.channels := MFGetAttributeUINT32(srcType,
                                            MF_MT_AUDIO_NUM_CHANNELS,
                                            0);
  if (aProfile.channels = 0) then
    aProfile.channels := 2;

  aProfile.bitsPerSample := MFGetAttributeUINT32(srcType,
                                                 MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                 16);

  aProfile.bytesPerSec :=  MFGetAttributeUINT32(srcType,
                                                MF_MT_AUDIO_BITS_PER_SAMPLE,
                                                16);

  aProfile.aacProfile := $29;


  aProfile.samplesPerSec := MFGetAttributeUINT32(srcType,
                                                 MF_MT_AUDIO_SAMPLES_PER_SECOND,
                                                 0);
  // Note: Some encoded audio formats do not contain a value for bits/sample.
  // In that case, use a default value of 16. Most codecs will accept this value.
   if (aProfile.samplesPerSec = 0) then
     aProfile.samplesPerSec := 16;

  // Create the corresponding PCM audio type.
  hr := CreateAACAudioType(aProfile,
                           aacType);
done:
  Result := hr;
end;



// Helpers /////////////////////////////////////////////////////////////////////

procedure TAudioMft.ResetAudioFormatArray();
begin
  SetLength(predef_aac_profiles,
            0);
end;


procedure TAudioMft.ResetAACProfileInfoArray();
begin
  SetLength(pa_AAC_profiles,
            0);
end;


procedure TAudioMft.ResetAC3ProfileInfoArray();
begin
  SetLength(pa_AC3_profiles,
            0);
end;


procedure TAudioMft.ResetMP3ProfileInfoArray();
begin
  SetLength(pa_MP3_profiles,
            0);
end;

end.
