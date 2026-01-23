## Resources:

**1. Core dynamics processing (compressor / limiter math)**
  **Udo Zölzer – DAFX**: Digital Audio Effects

***This is the golden standard!***

Book: DAFX – Digital Audio Effects, 2nd & 3rd editions

**Chapters:**
- Dynamic Range Control
- Envelope Followers
- Attack / Release coefficient derivation
**Covers:**
- Linear ↔ dB domain math
- Feed-forward vs feed-back compressors
- RMS vs peak detection
- Soft knee equations

*Tip: If you only buy one DSP book: Buy this one.*

**Will Pirkle – Designing Audio Effect Plug-Ins in C++**

Very practical, implementation-oriented.

**Chapters:**
- Dynamics Processors
- Envelope Detectors
- Lookahead limiters
**Covers:**
- Sample-accurate attack/release
- Ballistics
- Gain computer math
- Metering (GR meters)
*Even though it’s C++, the math maps 1:1 to Delphi.*

**Julius O. Smith (CCRMA, Stanford)**
Authoritative academic source.
Website:
https://ccrma.stanford.edu/~jos/
*Key sections:*
- Envelope Detection
- Dynamics Processing
- Audio Signal Processing in Music Applications

*Especially good for:*
- RMS envelope math
- One-pole smoothing filters
- Stability and time constants

**True-peak detection & oversampling**
TU-R BS.1770 (Official loudness + true-peak spec)

**This is the definition of true peak in broadcast audio.**

Spec: Algorithms to measure audio programme loudness and true-peak audio level
Defines:
- 4× oversampling minimum
- Inter-sample peak detection
- Reconstruction filter concept

We did not a need to fully implement BS.1770 — but it explains why Catmull-Rom or polyphase interpolation is used.

**TC Electronic – True Peak Explained**
- Very readable engineering explanation.
- Whitepaper: “True Peak Explained”

***Covers:***
- Why sample peaks are insufficient
- DAC reconstruction overshoot
- Practical oversampling strategies
- This aligns very well with our TruePeakAbs() approach.
