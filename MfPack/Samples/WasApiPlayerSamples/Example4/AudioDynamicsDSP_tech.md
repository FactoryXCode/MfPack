# AudioDynamicsDSP — Technical Notes (Compressor + True-Peak Limiter)

*Generated: 2026-01-23*

This document explains the design and signal processing performed by `TAudioDynamicsDSP` in the MfPack Sample 4 player, with emphasis on **real-time safety**, **true-peak limiting**, and **format-agnostic processing** (PCM16/24/32-int and Float32).

---

## 1. Purpose and scope

`TAudioDynamicsDSP` provides:

- A **feed-forward compressor** (linked stereo/multi-channel detection)
- A **brickwall limiter** with optional **true-peak** detection (Catmull-Rom interpolated oversampling)
- Optional **RMS detector** for the limiter (vs peak)
- Optional **lookahead delay** for the limiter (to catch fast transients cleanly)
- Lightweight **gain-reduction meters** for compressor and limiter (in dB)

It is designed to be callable from a render loop (e.g., WASAPI event-driven pull) without allocations, locks, or exceptions.

---

## 2. Data model

### 2.1 Interleaved audio buffers

Processing assumes **interleaved** samples:

- Frame index `i` has channel samples: `i*Channels + ch`, for `ch = 0..Channels-1`
- Float samples are normalized approximately to `[-1.0, +1.0)`

### 2.2 Sample formats

A convenience entry point `ProcessInterleaved(Buffer, Frames, Format)` supports:

- `sfFloat32` — processed in place
- `sfInt16 / sfInt24 / sfInt32` — converted to float scratch, processed, converted back

All integer conversion helpers should live in a reusable PCM/DSP helper unit (e.g., `PcmLib.pas`). The engine picks the correct format; callers should not guess.

---

## 3. High-level signal flow

Per render block:

1. Apply pending settings changes safely (block boundary)
2. For each frame:
   1. Measure input detector (linked peak)
   2. Compute compressor target gain
   3. Smooth compressor gain with attack/release
   4. Apply compressor (+ optional makeup gain)
   5. Compute limiter detector (peak or RMS), optionally refined by true-peak interpolation
   6. Compute limiter target gain (soft knee around ceiling)
   7. Apply limiter gain, optionally using lookahead delay
   8. Update history (for true-peak interpolation)

---

## 4. Compressor

### 4.1 Feed-forward, linked detector

Detector uses the maximum absolute sample across channels for each frame:

```
peakIn = max_ch |x[i,ch]|
```

This creates linked behavior: all channels share the same gain.

### 4.2 Threshold and ratio

Configured in dB:

- Threshold: `T` (dBFS)
- Ratio: `R >= 1`

If above threshold:

```
overDb = LinToDb(peakIn) - T
gainReductionDb = overDb * (1 - 1/R)
compTarget = DbToLin(-gainReductionDb)
```

### 4.3 Attack/release smoothing

```
if target < gain:
  gain = atk*gain + (1-atk)*target
else:
  gain = rel*gain + (1-rel)*target
```

Time constants typically map to coefficients:

```
coef = exp(-1 / (timeSeconds * sampleRate))
```

### 4.4 Makeup gain

```
xComp = x * compGain * makeupLin
```

Metering: compressor GR is `-LinToDb(compGain)` (without makeup).

---

## 5. Limiter

### 5.1 Ceiling and knee

Limiter ceiling `C` (dBFS) with soft knee in dB:

```
overDb = LinToDb(det) - C
overSoftDb = SoftKneeOverDb(overDb, kneeDb)
limTarget = DbToLin(-max(0, overSoftDb))

if det > 0 and det*limTarget > ceilLin:
  limTarget = ceilLin / det
```

### 5.2 Release smoothing

Attack is effectively instantaneous; release is smoothed:

```
if limTarget < limGain:
  limGain = limTarget
else:
  limGain = rel*limGain + (1-rel)*limTarget
```

Limiter GR meter: `-LinToDb(limGain)`.

---

## 6. True-peak detection

True-peak detection refines the detector by interpolating between samples using Catmull-Rom. For each channel it uses:

- `a = x[n-2]`, `b = x[n-1]`, `c = x[n]`, `d = x[n+1]`

`TruePeakAbs(a,b,c,d,OS)` returns the max absolute of `|b|`, `|c|`, plus `OS-1` interpolated samples between them.

```
det = max(det, TruePeakAbs(hist0[ch], hist1[ch], hist2[ch], xPostComp, OS))

hist0 = hist1
hist1 = hist2
hist2 = xPostComp
```

Reset history to 0 on seek/stop to avoid false bursts.

---

## 7. Lookahead delay

Lookahead delays audio by `N` frames so the limiter can react before transients reach the output.

```
xd = delayBuf[pos]          // delayed post-comp
delayBuf[pos] = xPostComp   // store current post-comp
out = xd * limGain
pos = (pos+1) mod delaySamples
```

Latency: `delaySamples / sampleRate` seconds.

---

## 8. RMS detector (optional)

```
rmsEnv = rmsCoef*rmsEnv + (1-rmsCoef)*peakComp^2
det = sqrt(rmsEnv)
```

---

## 9. Real-time safety

- No heap allocation in `Process...` (pre-allocate scratch/delay/history)
- No locks; apply settings via atomics at block boundaries
- Never raise exceptions from DSP

GR meters in milli-dB using interlocked writes:

```
InterlockedExchange(FCompGRmDb, Round(-LinToDb(FCompGain) * 1000));
InterlockedExchange(FLimGRmDb,  Round(-LinToDb(FLimGain)  * 1000));
```

---

## 10. WASAPI integration

Single call from render loop when not silent:

```
if ((flags and AUDCLNT_BUFFERFLAGS_SILENT) = 0) then
  FDynamics.ProcessInterleaved(pBufferData, Integer(frames), FSampleFormat);
```

---

## 11. Practical tuning defaults

- Compressor: Threshold -18 dBFS, Ratio 2:1..3:1, Attack 10-30 ms, Release 100-250 ms
- Limiter: Ceiling -1.0 dBFS (true-peak), Knee 0.5-2 dB, Release 50-150 ms, OS 4 or 8, Lookahead 1-5 ms

---

## 12. Common pitfalls

- Avoid array-pointer indexing when range checking is enabled; prefer pointer-walk.
- True-peak must use the same signal that is limited (typically post-compressor).
- Reset state on seek/stop.
- Clamp correctly when converting float back to int PCM.

---

## 13. Future improvements

- Optional stereo-link factor (0..1)
- SIMD acceleration
- Polyphase oversampling
- Dither on float->int16/24 conversion
