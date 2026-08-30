# Sample 6 - Playback through the registered MFT DLL

This sample proves that the DLL built by Sample 5 works outside its own
project. It does not compile or link the grayscale transform implementation.
Instead, it discovers the installed Video Effect with `MFTEnumEx`, activates
the DLL through `IMFActivate`, and places the returned `IMFTransform` in a
Media Session topology.

## Prerequisite

Build Sample 5 and run `Register Win32 as administrator.bat`. Sample 6 is a
Win32 application and therefore uses the 32-bit COM and MFT registrations.

If the transform is not registered, opening a video reports:

```text
FactoryX RGB32 Grayscale MFT is not registered.
Build and register MfCustomMFT Sample 5 first.
```

## Discovery and activation

The application calls `MFTEnumEx` with:

```text
Category: MFT_CATEGORY_VIDEO_EFFECT
Flags:    MFT_ENUM_FLAG_SYNCMFT
Input:    Video / RGB32
Output:   Video / RGB32
```

It searches the returned activation objects for:

```text
{8A761F4D-3E0F-4E37-A291-B6C11D5088A0}
```

The matching `IMFActivate` creates the transform. The activation array and all
of its interfaces are then released before topology construction continues.

## Topology

```text
video media source
    -> decoder/color conversion selected by the topology loader
    -> IMFTransform activated from FactoryXGrayscaleMFT.dll
    -> Enhanced Video Renderer

audio media source
    -> decoder selected by the topology loader when needed
    -> Media Foundation audio renderer
```

The MFT is used only by the video branch. The optional first audio stream is
selected separately and rendered unchanged through the default audio endpoint.

Successful grayscale playback proves all of the following:

- COM registration points to a loadable Win32 DLL;
- Media Foundation registration contains the correct category and types;
- `MFTEnumEx` discovers the transform;
- `IMFActivate` and the DLL class factory create its `IMFTransform`;
- the activated transform negotiates and processes samples in a real topology.

## Run it

1. Register Sample 5.
2. Open `MfRegisteredMFTPlayer.dproj` in Delphi XE7.
3. Build and run **Win32 Debug**.
4. Open a video, wait for the topology-ready message, and click **Play**.
5. Confirm that the EVR output is grayscale.

Close the player and MfMftExplorer before unregistering the DLL, because each
activated transform keeps the in-process server loaded until its final
interface is released.

## Deliberate limitations

- the first video and first audio stream only;
- Win32 client and Win32 registered DLL only;
- RGB32 system-memory processing;
- no fallback to a locally linked transform;
- no automatic registration, because failure when unregistered is part of
  what this sample demonstrates.
