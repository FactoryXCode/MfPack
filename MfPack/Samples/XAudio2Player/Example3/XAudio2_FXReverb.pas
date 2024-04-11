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
// Revision Version: 3.1.6
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
// 30/01/2024 All                 Morrissey release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX316
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
  WinApi.Windows,
  System.Classes,
  WinApi.MediaFoundationApi.MfError,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.DirectX.XAudio2.XAudio2,
  WinApi.DirectX.XAudio2.XAudio2Fx;

type
  // Reverb effect parameters.
  TReverbParams = (rpDefault,
                   rpMinimum,
                   rpMaximum,
                   rpManual
                  );

  TFxReverb = class(TObject)
  private

    pvFxParameters: XAUDIO2FX_REVERB_PARAMETERS;
    pcReverbEffectEnabled: Boolean;

  public

    constructor Create();
    destructor Destroy(); override;

    /// <summary>Creates a reverb effect.</summary>
    /// <summary>See also:</summary>  https://learn.microsoft.com/en-us/windows/win32/xaudio2/how-to--create-an-effect-chain
    function CreateReverbEffect(ppVoice: PIXAudio2Voice;
                                pReverbParam: TReverbParams;
                                pOutputChannels: DWord;
                                pEnableEffect: Boolean): HResult;

    /// <summary>Be careful disabling an effect while the voice that hosts it is running.
    /// This can result in an audible artifact if the effect significantly changes the audio's pitch or volume.</summary>
    function RemoveReverbEffect(ppVoice: PIXAudio2Voice;
                                pEffectIndex: UINT32): HResult;

    function SetFXParameters(pReverbParam: TReverbParams = rpDefault): XAUDIO2FX_REVERB_PARAMETERS;

    function EnableReverbEffect(const ppVoice: PIXAudio2Voice;
                                pEffectIndex: UINT32;
                                pEnable: Boolean): HResult;

    // Use this property to adjust reverb setting manually.
    property FxReverbParameters: XAUDIO2FX_REVERB_PARAMETERS read pvFxParameters write pvFxParameters;
    property FxReverbEffectEnabled: Boolean read pcReverbEffectEnabled;

  end;

var
  XAPOReverbEffect: IUnknown;  // effects

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


function TFxReverb.CreateReverbEffect(ppVoice: PIXAudio2Voice;
                                      pReverbParam: TReverbParams;
                                      pOutputChannels: DWord;
                                      pEnableEffect: Boolean): HResult;
var
  hr: HResult;
  descriptor: XAUDIO2_EFFECT_DESCRIPTOR;
  chain: XAUDIO2_EFFECT_CHAIN;
  reverbParameters: XAUDIO2FX_REVERB_PARAMETERS;

label
  done;

begin

  if Assigned(XAPOReverbEffect) then
    begin
      hr := MF_E_ALREADY_INITIALIZED;
      goto done;
    end;

  hr := XAudio2CreateReverb(XAPOReverbEffect);
  if FAILED(hr) then
    goto done;

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

  // Populate the parameter structure, if any, associated with the effect.
  // The reverb effect uses an XAUDIO2FX_REVERB_PARAMETERS structure.
  reverbParameters := SetFXParameters(pReverbParam);

  // Pass the effect parameter structure to the effect by calling the
  // SetEffectParameters function on the voice to which the effect is attached.
  hr := ppVoice.SetEffectParameters(0,
                                    @reverbParameters,
                                    SizeOf(XAUDIO2FX_REVERB_PARAMETERS),
                                    XAUDIO2_COMMIT_NOW);

  if SUCCEEDED(hr) then
    // Disable or enable the effect, whenever appropriate.
    // You can use yourvoice.DisableEffect at any time to turn an effect off.
    // You can turn on an effect again with yourvoice.EnableEffect.
    // Note: The parameters for yourvoice.DisableEffect and yourvoice.EnableEffect specify which
    //       effect in the chain to enable or disable.
    hr := EnableReverbEffect(ppVoice,
                             0,
                             pEnableEffect);

done:
  Result := hr;
end;


function TFxReverb.RemoveReverbEffect(ppVoice: PIXAudio2Voice;
                                      pEffectIndex: UINT32): HResult;
begin
  Result := ppVoice.DisableEffect(0,
                                  XAUDIO2_COMMIT_NOW);
end;


function TFxReverb.SetFXParameters(pReverbParam: TReverbParams = rpDefault): XAUDIO2FX_REVERB_PARAMETERS;
begin
  // Minimum settings
  //=================
  if (pReverbParam = rpMinimum) then
    begin
      Result.ReflectionsDelay    := XAUDIO2FX_REVERB_MIN_REFLECTIONS_DELAY;
      Result.ReverbDelay         := XAUDIO2FX_REVERB_MIN_REVERB_DELAY;
      Result.RearDelay           := XAUDIO2FX_REVERB_MIN_REAR_DELAY;
      Result.PositionLeft        := XAUDIO2FX_REVERB_MIN_POSITION;
      Result.PositionRight       := XAUDIO2FX_REVERB_MIN_POSITION;
      Result.PositionMatrixLeft  := XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      Result.PositionMatrixRight := XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      Result.EarlyDiffusion      := XAUDIO2FX_REVERB_DEFAULT_EARLY_DIFFUSION;
      Result.LateDiffusion       := XAUDIO2FX_REVERB_DEFAULT_LATE_DIFFUSION;
      Result.LowEQGain           := XAUDIO2FX_REVERB_MIN_LOW_EQ_GAIN;
      Result.LowEQCutoff         := XAUDIO2FX_REVERB_MIN_LOW_EQ_CUTOFF;
      Result.HighEQGain          := XAUDIO2FX_REVERB_MIN_HIGH_EQ_GAIN;
      Result.HighEQCutoff        := XAUDIO2FX_REVERB_MIN_HIGH_EQ_CUTOFF;
      Result.RoomFilterFreq      := XAUDIO2FX_REVERB_MIN_ROOM_FILTER_FREQ;
      Result.RoomFilterMain      := XAUDIO2FX_REVERB_MIN_ROOM_FILTER_MAIN;
      Result.RoomFilterHF        := XAUDIO2FX_REVERB_MIN_ROOM_FILTER_HF;
      Result.ReflectionsGain     := XAUDIO2FX_REVERB_MIN_REFLECTIONS_GAIN;
      Result.ReverbGain          := XAUDIO2FX_REVERB_MIN_REVERB_GAIN;
      Result.DecayTime           := XAUDIO2FX_REVERB_MIN_DECAY_TIME;
      Result.Density             := XAUDIO2FX_REVERB_MIN_DENSITY;
      Result.RoomSize            := XAUDIO2FX_REVERB_MIN_ROOM_SIZE;
      Result.WetDryMix           := XAUDIO2FX_REVERB_MIN_WET_DRY_MIX;
    end
  else if (pReverbParam = rpDefault) then
    begin
      // Default settings
      //=================
      Result.ReflectionsDelay    := XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_DELAY;
      Result.ReverbDelay         := XAUDIO2FX_REVERB_DEFAULT_REVERB_DELAY;
      Result.RearDelay           := XAUDIO2FX_REVERB_DEFAULT_REAR_DELAY;
      Result.PositionLeft        := XAUDIO2FX_REVERB_DEFAULT_POSITION;
      Result.PositionRight       := XAUDIO2FX_REVERB_DEFAULT_POSITION;
      Result.PositionMatrixLeft  := XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      Result.PositionMatrixRight := XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      Result.EarlyDiffusion      := XAUDIO2FX_REVERB_DEFAULT_EARLY_DIFFUSION;
      Result.LateDiffusion       := XAUDIO2FX_REVERB_DEFAULT_LATE_DIFFUSION;
      Result.LowEQGain           := XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_GAIN;
      Result.LowEQCutoff         := XAUDIO2FX_REVERB_DEFAULT_LOW_EQ_CUTOFF;
      Result.HighEQGain          := XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_GAIN;
      Result.HighEQCutoff        := XAUDIO2FX_REVERB_DEFAULT_HIGH_EQ_CUTOFF;
      Result.RoomFilterFreq      := XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_FREQ;
      Result.RoomFilterMain      := XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_MAIN;
      Result.RoomFilterHF        := XAUDIO2FX_REVERB_DEFAULT_ROOM_FILTER_HF;
      Result.ReflectionsGain     := XAUDIO2FX_REVERB_DEFAULT_REFLECTIONS_GAIN;
      Result.ReverbGain          := XAUDIO2FX_REVERB_DEFAULT_REVERB_GAIN;
      Result.DecayTime           := XAUDIO2FX_REVERB_DEFAULT_DECAY_TIME;
      Result.Density             := XAUDIO2FX_REVERB_DEFAULT_DENSITY;
      Result.RoomSize            := XAUDIO2FX_REVERB_DEFAULT_ROOM_SIZE;
      Result.WetDryMix           := XAUDIO2FX_REVERB_DEFAULT_WET_DRY_MIX;
    end
  else if (pReverbParam = rpMaximum) then
    begin
      // Maximum settings
      //=================
      Result.ReflectionsDelay    := XAUDIO2FX_REVERB_MAX_REFLECTIONS_DELAY;
      Result.ReverbDelay         := XAUDIO2FX_REVERB_MAX_REVERB_DELAY;
      Result.RearDelay           := XAUDIO2FX_REVERB_MAX_REAR_DELAY;
      Result.PositionLeft        := XAUDIO2FX_REVERB_MAX_POSITION;
      Result.PositionRight       := XAUDIO2FX_REVERB_MAX_POSITION;
      Result.PositionMatrixLeft  := XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      Result.PositionMatrixRight := XAUDIO2FX_REVERB_DEFAULT_POSITION_MATRIX;
      Result.EarlyDiffusion      := XAUDIO2FX_REVERB_DEFAULT_EARLY_DIFFUSION;
      Result.LateDiffusion       := XAUDIO2FX_REVERB_DEFAULT_LATE_DIFFUSION;
      Result.LowEQGain           := XAUDIO2FX_REVERB_MAX_LOW_EQ_GAIN;
      Result.LowEQCutoff         := XAUDIO2FX_REVERB_MAX_LOW_EQ_CUTOFF;
      Result.HighEQGain          := XAUDIO2FX_REVERB_MAX_HIGH_EQ_GAIN;
      Result.HighEQCutoff        := XAUDIO2FX_REVERB_MAX_HIGH_EQ_CUTOFF;
      Result.RoomFilterFreq      := XAUDIO2FX_REVERB_MAX_ROOM_FILTER_FREQ;
      Result.RoomFilterMain      := XAUDIO2FX_REVERB_MAX_ROOM_FILTER_MAIN;
      Result.RoomFilterHF        := XAUDIO2FX_REVERB_MAX_ROOM_FILTER_HF;
      Result.ReflectionsGain     := XAUDIO2FX_REVERB_MAX_REFLECTIONS_GAIN;
      Result.ReverbGain          := XAUDIO2FX_REVERB_MAX_REVERB_GAIN;
      Result.DecayTime           := XAUDIO2FX_REVERB_DEFAULT_DECAY_TIME;
      Result.Density             := XAUDIO2FX_REVERB_MAX_DENSITY;
      Result.RoomSize            := XAUDIO2FX_REVERB_MAX_ROOM_SIZE;
      Result.WetDryMix           := XAUDIO2FX_REVERB_MAX_WET_DRY_MIX;
    end
  else if (pReverbParam = rpManual) then
    begin
      Result := pvFxParameters;
    end;
end;


function TFxReverb.EnableReverbEffect(const ppVoice: PIXAudio2Voice;
                                      pEffectIndex: UINT32;
                                      pEnable: Boolean): HResult;
begin
  // Disable or enable the effect, whenever appropriate.
  // You can use DisableEffect at any time to turn an effect off.
  // You can turn on an effect again with EnableEffect.
  // Note: The parameters for DisableEffect and EnableEffect specify which
  //       effect in the chain to enable or disable.
  if pEnable then
    Result := ppVoice.EnableEffect(0, //pEffectIndex,
                                   XAUDIO2_COMMIT_NOW)
  else
    Result := ppVoice.DisableEffect(0, //pEffectIndex,
                                    XAUDIO2_COMMIT_NOW);
  pcReverbEffectEnabled := SUCCEEDED(Result);
end;

end.
