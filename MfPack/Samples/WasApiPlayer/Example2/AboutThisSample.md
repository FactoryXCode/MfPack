## Real-Time Audio Playback with WASAPI and an EQ MFT

**1. Introduction**
  This handout explains the design of a real-time audio playback engine using WASAPI and an equalizer
  implemented as a Media Foundation Transform (MFT). The focus is on architecture, threading, and correct
  real-time behavior.
  The GUI never touches audio processing directly.
  
**2. High-Level Architecture**
  The system is split into clearly separated components to guarantee real-time safety and responsiveness.
  
**3. Threading Model**
  Two threads are involved: the GUI thread and the playback (audio engine) thread. All audio objects are
  owned exclusively by the playback thread.
  
**4. Command Pattern**
  All control actions are represented as commands. This avoids direct cross-thread calls and guarantees
  deterministic behavior.
  
**5. EQ Processing Model**
  The EQ is applied in-place on the audio render buffer just before it is released to WASAPI, guaranteeing
  minimal latency.
  
**6. Clamping and Validation**
  Parameter clamping is performed only inside the EQ MFT. This avoids duplicated logic and ensures
  correctness at the lowest responsible layer.
  
**8. Key Takeaways**
  Use a dedicated playback thread, communicate via commands, process commands during playback, and
  enforce invariants inside the MFT.

## Architecture Overview
**GUI Thread**
↓
Command Queue + Event
↓
Audio Engine Thread
↓
EQ MFT
↓
WASAPI Output

