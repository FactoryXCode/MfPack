# WASAPI Player Sample 5 – Developer Manual

This document is the **single, authoritative developer manual** for WASAPI Player Sample 5.
It describes the architecture, threading model, unit responsibilities, DSP rules, and
extension guidelines in full detail.

---

## 1. Purpose of This Manual

This manual exists to answer four questions unambiguously:

1. What does each unit do?
2. Who calls whom?
3. On which thread does it run?
4. What must never be done in real-time code?

---

## 2. High-Level Architecture

### 2.1 Audio Data Flow

```
Audio File
   |
   v
Media Foundation SourceReader
   |
   v
Decoded PCM
   |
   v
PCM → Float32 Conversion
   |
   v
___FX Rack___
  |   |   |
  EQ  FL  LIM
   |
   v
Float32 → PCM Conversion
   |
   v
WASAPI Render Client
```

### 2.2 One-Way Flow Guarantee

Audio data always flows **forward**.
No component ever pulls data backward or re-enters a previous stage.

This simplifies:
- Synchronization
- Lifetime management
- Debugging

---

## 3. Threading Model

### 3.1 Threads in the System

| Thread        | Responsibility            |
|---------------|---------------------------|
| GUI Thread    | UI, sliders, menus        |
| Engine Thread | Decoding, scheduling      |
| Audio Thread  | Real-time DSP + rendering |

---

### 3.2 Audio thread guidelines

The audio thread should not:
- Allocate memory.
- Free memory.
- Wait on locks.
- Call ResetState().
- Call GUI code.

The audio thread should:
- Run deterministically.
- Finish before the next buffer deadline.

---

## 4. FX Rack Design

### 4.1 Slot-Based Processing

The FX rack consists of ordered slots.
Each slot contains exactly one effect.

Slots can be reordered, enabled or disabled.


### 4.2 Single bypass

**TMfWasApiFxSlot.Enabled is the ONLY bypass control.**

If a slot is disabled, the rack does not call the effect.
If a slot is enabled, the effect MUST process audio

FX components and MFTs should not implement internal Enabled flags or early-exit based on enable state.
This prevents desynchronization and silent failures.

---

## 5. Core engine units

### 5.1 WasApiEngine.pas

Role:
- Owns the WASAPI render client.
- Runs the real-time render loop.

Calls:
- MfWasApiEffectsRack

Thread:
- Audio thread

Notes:
- No allocations
- No blocking
- No state resets

---

### 5.2 MfSourceReaderEngine.pas

Role:
- Decodes audio files via Media Foundation.

Calls:
- WasApiEngine

Thread:
- Engine thread

Notes:
- Decoding is not real-time critical.

---

## 6. FX Rack Units

### 6.1 MfWasApiEffectsRack.pas

Role:
- Central DSP router.

Responsibilities:
- PCM <-> Float32 conversion.
- Slot ordering.
- Slot-only bypass enforcement.

Thread:
- Audio thread.

---

### 6.2 MfWasApiFxSlot.pas

Role:
- One rack position.

Responsibilities:
- Holds one effect.
- Controls bypass.

Thread:
- Audio thread (read-only).

---

### 6.3 MfWasApiFxComponentBase.pas

Role:
- Base class for FX components.

Responsibilities:
- Exposes IMfWasApiFxProvider.
- Bridges GUI and DSP.

Rule:
- Never decide bypass here.

---

## 7. DSP / Effect Units

### 7.1 Parametric EQ (MfParametricEqMFT.pas)

Processing:
- Biquad filters.
- Float32 only.

Rules:
- Always process when called.
- Smooth coefficient changes.

---

### 7.2 Flanger / Echo (MfFlangerEchoMFT.pas)

Processing:
- Delay line.
- LFO modulation.

Rules:
- Smooth parameters per-sample.
- Never reset delay buffers on UI change.

---

### 7.3 Compressor / Limiter (MfCompressorLimiterMFT.pas)

Processing:
- RMS compressor.
- True-peak limiter.

Rules:
- Double-buffered settings.
- No dynamic allocation.
- No hard resets on parameter change.

---

## 8. Writing Your Own FX

### Step 1: Create the component.
Derive from TMfWasApiFxComponentBase.

### Step 2: Provide an MFT
Implement IMfWasApiFxProvider.

### Step 3: Process unconditionally.
Never early-exit for bypass.

### Step 4: Smooth parameters
All UI-controlled parameters must be smoothed.

---

## 9. Common failures when writing MFT's or .

| Symptom               | Probable cause                       |
|-----------------------|-----------------------------|
| FX silent             | Internal Enabled flag       |
| Clicks                | Hard resets or no smoothing |
| Dropouts              | Allocation in audio thread  |
| Crash on close        | Callbacks after destroy     |
| Noise or distortions  | Wrong WAV format            |


---

## 10. Design philosophy

This project favors:
- Explicit ownership.
- Simple data flow.
- Deterministic execution.

If something feels complicated, it is probably wrong ;-)

---