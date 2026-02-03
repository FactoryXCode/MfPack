# WASAPI Player Sample 5 – Unit-by-Unit Architecture Description

This document describes each non-GUI unit, what it does, and how units are connected.

---

## Core Playback & Engine Units

### WasApiEngine.pas
Role: Low-level audio playback engine.

Responsibilities:
- Initializes WASAPI in event-driven shared mode
- Manages the render client and audio buffer loop
- Controls start / stop / pause / shutdown
- Owns the real-time audio thread

Connections:
- Receives decoded audio from MfSourceReaderEngine
- Calls the FX rack (TMfWasApiEffectsRack) before rendering
- Sends audio to the WASAPI render endpoint

Threading:
- Runs the real-time render loop
- Must never allocate, block, or reset state

---

### MfSourceReaderEngine.pas
Role: Audio decoding and source management.

Responsibilities:
- Wraps Media Foundation SourceReader
- Decodes compressed audio files into PCM
- Handles seeking and position tracking

Connections:
- Outputs PCM audio to WasApiEngine
- Supplies format information (sample rate, channels)

---

## FX Rack & Routing

### MfWasApiEffectsRack.pas
Role: Central DSP routing and control unit.

Responsibilities:
- Owns and orders FX slots
- Converts PCM to Float32 and back if needed
- Calls FX MFTs in slot order
- Enforces slot-only bypass policy

Note:
- TMfWasApiFxSlot.Enabled is the only bypass switch

Connections:
- Called by WasApiEngine
- Calls individual FX MFTs

---

### MfWasApiFxSlot.pas
Role: One position in the FX processing chain.

Responsibilities:
- Holds exactly one FX component
- Controls bypass via Enabled
- Does not process audio itself

---

### MfWasApiFxComponentBase.pas
Role: Base class for all FX components.

Responsibilities:
- Defines the contract for FX components
- Exposes IMfWasApiFxProvider
- Connects GUI-visible components to MFTs

---

## DSP / Effect Units

### MfParametricEqMFT.pas
Role: Multi-band parametric equalizer.

Responsibilities:
- Processes Float32 audio
- Applies biquad filters per band
- Smooths coefficient changes

---

### MfFlangerEchoMFT.pas
Role: Modulated delay-based effect.

Responsibilities:
- Implements flanger / echo behavior
- Uses delay line with LFO modulation
- Smooths parameters per-sample

---

### MfCompressorLimiterMFT.pas
Role: Dynamics processor.

Responsibilities:
- RMS compressor
- True-peak limiter with lookahead
- Gain smoothing and envelope tracking

---

## Utility Units

### PcmLib.pas
Role: Audio format conversion utilities.

Responsibilities:
- PCM to Float32 conversion
- Bit-depth handling
- Format inspection helpers

---

## Overall Data Flow

MfSourceReaderEngine -> WasApiEngine -> FX Rack -> WASAPI Render
