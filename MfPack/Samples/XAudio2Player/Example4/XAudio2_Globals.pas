unit XAudio2_Globals;

interface

//const
  // These settings are not published in XAudio2Fx.
  //XAUDIO2FX_REVERB_MIN_POSITION_MATRIX_LEFT     = 0;
  //XAUDIO2FX_REVERB_MIN_POSITION_MATRIX_RIGHT    = 0;
  //XAUDIO2FX_REVERB_MIN_EARLY_DIFFUSION          = 0;
  //XAUDIO2FX_REVERB_MIN_LATE_DIFFUSION           = 0;
  //XAUDIO2FX_REVERB_MIN_REFLECTIONS_GAIN: Single = -100.0;

  //XAUDIO2FX_REVERB_MAX_POSITION_MATRIX_LEFT     = 30;
  //XAUDIO2FX_REVERB_MAX_POSITION_MATRIX_RIGHT    = 30;
  //XAUDIO2FX_REVERB_MAX_EARLY_DIFFUSION          = 15;
  //XAUDIO2FX_REVERB_MAX_LATE_DIFFUSION           = 15;
  //XAUDIO2FX_REVERB_MAX_DECAY_TIME: Single       = 100.0; // Note that 100 is not the max, permitted range is from 0.1 to infinity seconds.

var
  // Global variable that holds the chaincount for all effects.
  gaEffectChainCount: UINT32;

implementation

end.
