# AudioDynamicsDSP

## Overview

`AudioDynamicsDSP` is a real-time **compressor + limiter DSP engine** used in MfPack (WASAPI / Media Foundation–based audio engines).

It is designed for:

* Real-time playback
* Linked stereo processing
* Broadcast-safe limiting
* Sample-accurate processing
* PCM16 / PCM24 / PCM32 / Float32 audio

\---

## Features

* Feed-forward compressor
* Peak or RMS detector
* Soft knee compression
* Lookahead limiter
* True-peak detection (oversampling)
* Gain-reduction meters (compressor + limiter)
* Format-agnostic (int ↔ float handled internally)
* Real-time safe (no heap allocation in audio thread)

\---

## Typical Signal Flow

```
Input → Detector → Gain Computer → Smoothing → Lookahead → Output
```

\---

## Usage

```
FDynamics := TAudioDynamicsDSP.Create(SampleRate, 
                                      Channels);

Settings := TDynamicsSettings.Defaults;
Settings.CompEnabled := True;
Settings.LimEnabled  := True;
Settings.LimTruePeak := True;

FDynamics.SetSettings(Settings);

// In the render loop:
FDynamics.ProcessInterleaved(Buffer, 
                             Frames, 
                             SampleFormat);
```

\---

## Supported Formats

* PCM 16-bit
* PCM 24-bit
* PCM 32-bit
* Float32

All internal DSP processing is performed in **float32**.

\---

## Thread Safety

* Designed for real-time audio threads
* No memory allocation during processing
* Meter values updated atomically for GUI access

\---

# DSP Theory \& Implementation Details

## 1\. Architecture Overview

`AudioDynamicsDSP` implements a **feed-forward dynamics processor** consisting of:

* Linked-channel compressor
* Peak or RMS detector
* Optional soft knee
* Lookahead limiter with delay line
* Optional true-peak detection via oversampling
* Gain-reduction meters

Processing order per sample frame:

```
Input → Detector → Gain Computer → Smoothing → Lookahead → Output
```

\---

## 2\. Envelope Detection

### Peak Detection

```
peak = max(|x\\\[ch]|)
```

### RMS Detection

```
RMS\\\[n] = a · RMS\\\[n-1] + (1 - a) · x²
a = exp(-1 / (τ · Fs))
```

\---

## 3\. Compressor Gain Computer

```
GR = (input\\\_dB - threshold\\\_dB) · (1 - 1/ratio)
gain = 10^( -GR / 20 )
```

\---

## 4\. Soft Knee Processing

Soft knee is applied in the dB domain to smoothly transition into compression near the threshold.

\---

## 5\. Lookahead Limiter

Detector runs ahead of a delayed output signal, allowing safe peak control.

\---

## 6\. True-Peak Detection

True peaks are estimated using oversampling and Catmull–Rom interpolation.

\---

## 7\. Limiter Gain Logic

```
if detected\\\_level > ceiling:
    gain = ceiling / detected\\\_level
```

\---

## 8\. Gain Reduction Metering

```
GR\\\_dB = -20 · log10(gain)
```

\---

## 9\. PCM Format Handling

```
PCM → float → DSP → float → PCM
```

\---

## 10\. Design Decisions

* Feed-forward design for predictability.
* Linked channels for stereo stability.
* Float32 internal processing.
* Recreate DSP on format changes.

\---

## 11\. References

* Udo Zölzer – *DAFX: Digital Audio Effects*
* Will Pirkle – *Designing Audio Effect Plug-Ins in C++*
* Julius O. Smith (CCRMA)
* ITU-R BS.1770
* IEC 60268-17
* TC Electronic – *True Peak Explained*
* JUCE DSP module
* LSP Plugins
* x42 DSP

\---

## 12\. Summary

`AudioDynamicsDSP` follows established professional DSP practices and is therefor suitable for production-grade audio software.

