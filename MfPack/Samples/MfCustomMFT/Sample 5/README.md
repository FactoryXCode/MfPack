# Sample 5 - Registered grayscale MFT

This sample packages the RGB32 grayscale transform as a Win32 in-process COM
server and registers it as a synchronous Media Foundation video effect.

The registered identity is:

```text
Friendly name: FactoryX RGB32 Grayscale MFT
CLSID:         {8A761F4D-3E0F-4E37-A291-B6C11D5088A0}
Category:      MFT_CATEGORY_VIDEO_EFFECT
Input:         Video / RGB32
Output:        Video / RGB32
Flags:         MFT_ENUM_FLAG_SYNCMFT
```

## What registration does

`DllRegisterServer` creates the standard machine-wide COM `CLSID` and
`InprocServer32` entries, including the `Both` threading model. It then calls
`MFTRegister` with the same CLSID, category, friendly name, flags, and
registered media types. This explicit code mirrors Microsoft's MFT samples and
makes each required registration step visible.

`MfRegisterApiFix.pas` contains the corrected Delphi XE7 declaration of
`MFTRegister`. For XE7, the category GUID must be passed by value, while the
media-type counts and pointers need their `const` qualifiers. The older
declaration marshals that combination incorrectly and Windows returns
`ERROR_NOACCESS`. The wrapper calls the same `mfplat.dll` export with the
correct ABI and does not replace or wrap any Windows behavior.

Unregistration always attempts both `MFTUnregister` and COM unregistration so
it can also repair a partially completed registration.

The class derives from `TComObject`, and `TComObjectFactory` creates a separate
transform instance for each activation. The transform code and MFT contract
remain the same as the earlier samples.

## Build and register

1. Open `FactoryXGrayscaleMFT.dproj` in Delphi XE7.
2. Build **Win32 Debug**.
3. Run `Register Win32 as administrator.bat`. It requests UAC elevation and
   then runs `Register Win32.cmd` for you.

On 64-bit Windows, a Win32 DLL must be registered with the 32-bit copy of
`regsvr32.exe` in `SysWOW64`. The supplied scripts select it automatically.

Registration stores the absolute path of the current DLL. Do not move or
delete the DLL while it is registered; unregister it first.

## Inspect it with MfMftExplorer

1. Start the Win32 build of `MfMftExplorer` after registration.
2. Select **Video effects**.
3. Select **All registered MFTs** or **Synchronous software MFTs**.
4. Enumerate and select **FactoryX RGB32 Grayscale MFT**.
5. Use **Activate & inspect**.

MfMftExplorer should report the friendly name, CLSID, RGB32 input/output
registration types, one input stream, one output stream, and successful
`IMFTransform` activation.

## Unregister

Close applications that activated the DLL, then run `Unregister Win32.cmd`
**as administrator**. Enumerating Video Effects again should no longer show
the transform.

## Deliberate limitations

- Win32 only; a Win64 host requires a separately compiled and registered
  Win64 DLL with the same implementation;
- RGB32 system-memory samples only;
- no installer, code signing, versioned deployment, or side-by-side servicing;
- registration changes the machine and therefore requires explicit cleanup.

Production deployment should perform the same two registrations in an
installer and remove both during uninstall.
