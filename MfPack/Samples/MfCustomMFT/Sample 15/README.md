# Sample 15 — live camera and microphone delay

Open `MfCameraMicrophoneDelay.dproj` in the Delphi IDE, select **Win32**, and run it. Select a camera and microphone and click **Start live**. The camera appears in the form, while microphone audio passes through the Sample 14 delay MFT to the default audio output device. Move the sliders while it is running to change delay (1–10000 ms) and wet mix (0–100%) immediately. Click **Stop** to release both capture devices.

The sample combines the selected camera and microphone with `MFCreateAggregateSource`, then builds one Media Foundation topology with two source branches:

`camera → EVR video renderer`

`microphone → local TMfAudioDelayMFT → audio renderer`

The Media Session resolves the formats, starts both branches, and reports readiness and errors through `IMFAsyncCallback`. The callback posts messages to the VCL form; it does not change form controls from a Media Foundation worker thread. Closing waits for `MESessionClosed` before shutting down the sources and session. The local MFT does not require registration or administrator rights.

Use headphones when listening to the delayed microphone to avoid acoustic feedback. A physical camera and microphone are needed to test the live paths.
