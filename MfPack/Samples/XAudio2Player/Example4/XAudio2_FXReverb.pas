// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: XAudio2_FXMasterLimiter.pas
// Kind: Pascal Unit
// Release date: 28-03-2024
// Language: ENU
//
// Revision Version: 3.1.7
// Description: XAudio2 Reverb.
//
// Company: FactoryX
// Intiator(s): Tony Kalf (maXcomX)
// Contributor(s): Tony Kalf (maXcomX)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 19/06/2024 All                 RammStein release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
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
// =============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/xaudio2/xaudio2-audio-effects
//
// Copyright © FacctoryX
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
unit XAudio2_FXReverb;

interface

uses
  {WinApi}
  WinApi.Windows,
  {System}
  System.Classes,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  {XAudio2}
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAudio2Fx;



// Use these setting for testing.
const
  NATIVE_EFFECT_DEFAULT: XAUDIO2FX_REVERB_PARAMETERS = (
      WetDryMix: XAUDIO2FX_REVERB_DEFAULT_WET_DRY_MIX;
      ReflectionsDelay: XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_DELAY;
      ReverbDelay:      XAUDIO2FX_REVERB_DEFAULT_REVERB_DELAY;
      RearDelay: XAUDIO2FX_REVERB_DEFAULT_REAR_DELAY;
      PositionLeft: XAUDIO2FX_REVERB_DEFAULT_POSITION;
      PositionRight: XAUDIO2FX_REVERB_DEFAULT_POSITION;
      PositionMatrixLeft: XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      PositionMatrixRight: XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      EarlyDiffusion: XAUDIO2FX_REVERB_DEFAULT_EARLY_DIFFUSION;
      LateDiffusion: XAUDIO2FX_REVERB_DEFAULT_LATE_DIFFUSION;
      LowEQGain: XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_GAIN;
      LowEQCutoff: XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_CUTOFF;
      HighEQGain: XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_GAIN;
      HighEQCutoff: XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_CUTOFF;
      RoomFilterFreq: XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_FREQ;
      RoomFilterMain: XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_MAIN;
      RoomFilterHF: XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_HF;
      ReflectionsGain: XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_GAIN;
      ReverbGain: XAUDIO2FX_REVERB_DEFAULT_REVERB_GAIN;
      DecayTime: XAUDIO2FX_REVERB_DEFAULT_DECAY_TIME;
      Density: XAUDIO2FX_REVERB_DEFAULT_DENSITY;
      RoomSize: XAUDIO2FX_REVERB_DEFAULT_ROOM_SIZE
     );

type
  // Reverb effect parameters.
  TReverbI3DL2Params = record
    i3dl2Name: string;
    i3dl2Param: XAUDIO2FX_REVERB_I3DL2_PARAMETERS;
    nativeParam: XAUDIO2FX_REVERB_PARAMETERS;
    sevenDotOneReverb: BOOL;
    procedure ConvertToNative(sdOneReverb: BOOL = TRUE);
  end;

  TReverbI3DL2ParamArray = array [0..29] of TReverbI3DL2Params;

  TFxReverb = class(TObject)
  private

    pvFxParameters: XAUDIO2FX_REVERB_PARAMETERS;
    pvReverbEffectEnabled: Boolean;

  public

    constructor Create();
    destructor Destroy(); override;

    /// <summary>Creates a native reverb effect.</summary>
    /// <summary>See also:</summary>  https://learn.microsoft.com/en-us/windows/win32/api/xaudio2fx/
    function CreateNativeReverbEffect(ppVoice: PIXAudio2Voice;
                                      pReverbParam: XAUDIO2FX_REVERB_PARAMETERS;
                                      pOutputChannels: DWord;
                                      pEnableEffect: Boolean): HResult;

    /// <summary>Be careful disabling an effect while the voice that hosts it is running.
    /// This can result in an audible artifact if the effect significantly changes the audio's pitch or volume.</summary>
    function RemoveReverbEffect(ppVoice: PIXAudio2Voice;
                                pEffectIndex: UINT32 = 0): HResult;

    function EnableReverbEffect(ppVoice: PIXAudio2Voice;
                                pEnable: Boolean;
                                pEffectIndex: UINT32 = 0): HResult;

    // Use this property to adjust reverb setting manually.
    property FxReverbNativeParameters: XAUDIO2FX_REVERB_PARAMETERS read pvFxParameters write pvFxParameters;
    property FxReverbEffectEnabled: Boolean read pvReverbEffectEnabled write pvReverbEffectEnabled;

  end;

  /// <summary>Get all predefined effects for I3DL and Native</summary>
  function GetReverbParams(): TReverbI3DL2ParamArray;


var
  XAPOReverbEffect: IInterface;  // effects


implementation


constructor TFxReverb.Create();
begin
  inherited;

end;


destructor TFxReverb.Destroy();
begin
  if Assigned(XAPOReverbEffect) then
    SafeRelease(XAPOReverbEffect);
  inherited;
end;


function GetReverbParams(): TReverbI3DL2ParamArray;
var
  i: Integer;

begin

  Result[0].i3dl2Name := 'Default';
  Result[0].nativeParam := NATIVE_EFFECT_DEFAULT; //Result[0].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_DEFAULT

  Result[1].i3dl2Name := 'Generic';
  Result[1].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_GENERIC;

  Result[2].i3dl2Name := 'Padded cell';
  Result[2].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_PADDEDCELL;

  Result[3].i3dl2Name := 'Room';
  Result[3].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_ROOM;

  Result[4].i3dl2Name := 'Bathroom';
  Result[4].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_BATHROOM;

  Result[5].i3dl2Name := 'Livingroom';
  Result[5].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_LIVINGROOM;

  Result[6].i3dl2Name := 'Stoneroom';
  Result[6].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_STONEROOM;

  Result[7].i3dl2Name := 'Auditorium';
  Result[7].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_AUDITORIUM;

  Result[8].i3dl2Name := 'Concert hall';
  Result[8].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_CONCERTHALL;

  Result[9].i3dl2Name := 'Cave';
  Result[9].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_CAVE;

  Result[10].i3dl2Name := 'Arena';
  Result[10].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_ARENA;

  Result[11].i3dl2Name := 'Hangar';
  Result[11].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_HANGAR;

  Result[12].i3dl2Name := 'Carpeted hallway';
  Result[12].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_CARPETEDHALLWAY;

  Result[13].i3dl2Name := 'Hallway';
  Result[13].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_HALLWAY;

  Result[14].i3dl2Name := 'Stone corridor';
  Result[14].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_STONECORRIDOR;

  Result[15].i3dl2Name := 'Alley';
  Result[15].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_ALLEY;

  Result[16].i3dl2Name := 'Forest';
  Result[16].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_FOREST;

  Result[17].i3dl2Name := 'City';
  Result[17].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_CITY;

  Result[18].i3dl2Name := 'Mountains';
  Result[18].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_MOUNTAINS;

  Result[19].i3dl2Name := 'Quarry';
  Result[19].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_QUARRY;

  Result[20].i3dl2Name := 'Plain';
  Result[20].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_PLAIN;

  Result[21].i3dl2Name := 'Parking lot';
  Result[21].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_PARKINGLOT;

  Result[22].i3dl2Name := 'Sewer pipe';
  Result[22].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_SEWERPIPE;

  Result[23].i3dl2Name := 'Underwater';
  Result[23].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_UNDERWATER;

  Result[24].i3dl2Name := 'Small room';
  Result[24].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_SMALLROOM;

  Result[25].i3dl2Name := 'Medium Room';
  Result[25].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_MEDIUMROOM;

  Result[26].i3dl2Name := 'Large room';
  Result[26].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_LARGEROOM;

  Result[27].i3dl2Name := 'Medium hall';
  Result[27].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_MEDIUMHALL;

  Result[28].i3dl2Name := 'Large hall';
  Result[28].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_LARGEHALL;

  Result[29].i3dl2Name := 'Plate';
  Result[29].i3dl2Param := XAUDIO2FX_I3DL2_PRESET_PLATE;

  // Convert to native ()
  for i := 1 to Length(Result) - 1 do
    begin
      //Result[i].ConvertToNative();
      // or

      ReverbConvertI3DL2ToNative(Result[i].i3dl2Param,
                                 Result[i].nativeParam);

      Result[i].sevenDotOneReverb := False; // Default
      // Check if all parameters are within their boundaries and correct them if they don't.
      Result[i].nativeParam.CheckBoundaries(Result[i].sevenDotOneReverb);
    end;
end;


function TFxReverb.CreateNativeReverbEffect(ppVoice: PIXAudio2Voice;
                                            pReverbParam: XAUDIO2FX_REVERB_PARAMETERS;
                                            pOutputChannels: DWord;
                                            pEnableEffect: Boolean): HResult;
var
  hr: HResult;
  descriptor: XAUDIO2_EFFECT_DESCRIPTOR;
  chain: XAUDIO2_EFFECT_CHAIN;

label
  done;

begin

  //if not Assigned(XAPOReverbEffect) then
    hr := XAudio2CreateReverb(XAPOReverbEffect);

  if FAILED(hr) then
    goto done;

  if not pEnableEffect and pvReverbEffectEnabled then
    begin
      hr := EnableReverbEffect(ppVoice,
                               pEnableEffect);
      goto done;
    end;

  // Populate an XAUDIO2_EFFECT_DESCRIPTOR structure with data.
  descriptor.InitialState := True;
  descriptor.OutputChannels := pOutputChannels;
  descriptor.pEffect := XAPOReverbEffect;

  // Populate an XAUDIO2_EFFECT_CHAIN structure with data.
  chain.EffectCount := 1;
  chain.pEffectDescriptors := @descriptor;

  // Apply the effect chain to a voice with the SetEffectChain function.
  // Note: You can apply effect chains to master voices, source voices, and submix voices.
  hr := ppVoice.SetEffectChain(@chain);
  if FAILED(hr) then
    goto done;

  // Pass the effect parameter structure to the effect by calling the
  // SetEffectParameters function on the voice to which the effect is attached.
  hr := ppVoice.SetEffectParameters(0,
                                    @pReverbParam,
                                    SizeOf(XAUDIO2FX_REVERB_PARAMETERS),
                                    XAUDIO2_COMMIT_NOW);

  if SUCCEEDED(hr) then
    // Disable or enable the effect, whenever appropriate.
    // You can use yourvoice.DisableEffect at any time to turn an effect off.
    // You can turn on an effect again with yourvoice.EnableEffect.
    // Note: The parameters for yourvoice.DisableEffect and yourvoice.EnableEffect specify which
    //       effect in the chain to enable or disable.

    hr := EnableReverbEffect(ppVoice,
                             pEnableEffect);

done:
  Result := hr;
end;


function TFxReverb.RemoveReverbEffect(ppVoice: PIXAudio2Voice;
                                      pEffectIndex: UINT32 = 0): HResult;
begin
  Result := ppVoice.DisableEffect(pEffectIndex,
                                  XAUDIO2_COMMIT_NOW);
end;


function TFxReverb.EnableReverbEffect(ppVoice: PIXAudio2Voice;
                                      pEnable: Boolean;
                                      pEffectIndex: UINT32 = 0): HResult;
begin
  // Disable or enable the effect, whenever appropriate.
  // You can use DisableEffect at any time to turn an effect off.
  // You can turn on an effect again with EnableEffect.
  // Note: The parameters for DisableEffect and EnableEffect specify which
  //       effect in the chain to enable or disable.
  if pEnable then
    Result := ppVoice.EnableEffect(pEffectIndex,
                                   XAUDIO2_COMMIT_NOW)
  else
    Result := ppVoice.DisableEffect(pEffectIndex,
                                    XAUDIO2_COMMIT_NOW);
  pvReverbEffectEnabled := SUCCEEDED(Result);
end;


procedure TReverbI3DL2Params.ConvertToNative(sdOneReverb: BOOL = TRUE);
begin
  ReverbConvertI3DL2ToNative(i3dl2Param,
                             nativeParam,
                             sdOneReverb);
end;

end.
