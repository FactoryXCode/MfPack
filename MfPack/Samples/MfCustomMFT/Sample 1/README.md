# MfCustomMFT

`MfCustomMFT` is a beginner-oriented custom Media Foundation Transform sample.
It implements a synchronous video effect that converts an RGB32 frame to
grayscale.

The application generates a color test frame, wraps its pixels in an
`IMFSample`, passes the sample through `TMfGrayscaleMFT`, and displays the
result. No camera, video file, decoder, or encoder is required.

## What the sample teaches

- implementing the complete `IMFTransform` interface;
- exposing one fixed input stream and one fixed output stream;
- advertising and validating media types;
- supporting `MFT_SET_TYPE_TEST_ONLY` without changing transform state;
- retaining at most one sample between `ProcessInput` and `ProcessOutput`;
- returning `MF_E_NOTACCEPTING` when an input sample is already pending;
- returning `MF_E_TRANSFORM_NEED_MORE_INPUT` when output is requested first;
- flushing retained input through `ProcessMessage`;
- locking an `IMFMediaBuffer` and respecting frame size and stride;
- processing and returning the same sample in place;
- preserving COM interface ownership while a sample is queued.

## Deliberately small format contract

The transform supports only:

```text
Major type:  MFMediaType_Video
Subtype:     MFVideoFormat_RGB32
Layout:      4 bytes per pixel (B, G, R, unused/alpha)
Input type:  must equal the output frame size and stride
```

Supporting one format keeps type negotiation easy to see. A production video
effect would normally accept additional formats such as NV12 and may use D3D11
surfaces instead of a system-memory buffer.

## Synchronous one-sample lifecycle

```text
SetInputType / SetOutputType
              |
              v
       GetInputStatus
              |
              v
       ProcessInput(sample)
              |
      retain one interface
              |
              v
       ProcessOutput(...)
              |
       edit sample in place
              |
              v
       return same sample
```

`ProcessInput` does not process the pixels. It retains the `IMFSample` interface
and returns quickly. `ProcessOutput` performs the grayscale conversion, assigns
the retained sample to `MFT_OUTPUT_DATA_BUFFER.pSample`, and releases the
transform's retained reference.

The stream flags describe this contract:

```text
Input:  WHOLE_SAMPLES | SINGLE_SAMPLE_PER_BUFFER | PROCESSES_IN_PLACE
Output: WHOLE_SAMPLES | SINGLE_SAMPLE_PER_BUFFER | PROVIDES_SAMPLES
```

Because the MFT provides its output sample, the caller passes a nil sample in
the output buffer record. The transform returns the input sample there after
editing it.

## Pixel conversion

RGB32 pixels are stored in memory as B, G, R, and one preserved fourth byte.
The sample uses a fixed-point Rec. 601-style luminance calculation:

```text
gray = (29 * B + 150 * G + 77 * R + 128) shr 8
```

The resulting value is written to B, G, and R. This produces grayscale, not a
two-color thresholded bitmap; grayscale is usually what “black and white” means
for a video effect.

## Why the sample is not registered

The application creates the Delphi class directly:

```pascal
Transform := TMfGrayscaleMFT.Create as IMFTransform;
```

System-wide MFT registration would also require a COM server, class factory,
registration/unregistration, deployment, and architecture-specific registry
handling. Those topics obscure the transform contract and are intentionally
left for a later sample. An application-local MFT does not require installation
or administrator rights.

## Test harness

The form demonstrates the caller side in the same order used by a pipeline:

1. Create a complete RGB32 media type.
2. Test and commit the input type.
3. Test and commit the matching output type.
4. Create an `IMFMediaBuffer` and `IMFSample`.
5. Call `ProcessInput`.
6. Call `ProcessOutput`.
7. Copy the returned sample into the output bitmap.

The log shows the HRESULT from every important call.

## Building

Open `MfCustomMFT.dproj` and build the Win32 Debug configuration. The project
search path points to the MfPack `src` directory and targets the same Delphi
compiler range as MfPack 4.0.0.

## Useful experiments

1. Call `ProcessOutput` before `ProcessInput` and observe
   `MF_E_TRANSFORM_NEED_MORE_INPUT`.
2. Call `ProcessInput` twice without consuming the first sample and observe
   `MF_E_NOTACCEPTING`.
3. Propose NV12 instead of RGB32 and observe `MF_E_INVALIDMEDIATYPE`.
4. Change the output frame size and observe that mismatched in-place types are
   rejected.
5. Replace the grayscale formula with a different per-pixel effect while
   leaving the MFT state machine unchanged.
