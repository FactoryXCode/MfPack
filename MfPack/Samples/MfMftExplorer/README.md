# MfMftExplorer
  
Version: 4.0.0  

`MfMftExplorer` is a small teaching sample for discovering Media Foundation
Transforms (MFTs) installed on a Windows system.
  
  
## What the sample demonstrates
  
- Selecting an MFT category such as video decoders or audio encoders;
- Sselecting an enumeration scope such as software, hardware, or all MFTs;
- Calling `MFTEnumEx` without input/output filters;
- Reading the friendly name and CLSID from each `IMFActivate`;
- Detecting a hardware registration URL;
- Associating hardware video MFTs with their DXGI adapter;
- Reading the input and output types advertised during registration;
- Releasing every returned `IMFActivate` before freeing the array.
- Activating a selected MFT only when requested;
- Unlocking asynchronous MFTs before inspecting their streams;
- Reading stream limits, counts, IDs, flags, and buffer requirements;
- Enumerating runtime input and output media types;
- Shutting down the activated object after inspection;
- Probing every transform in the selected category and scope;
- Reporting DXGI adapters for which a hardware video query returns zero MFTs;
- Separating raw activation-object counts from unique hardware transforms;
- Applying generic activation, stream, and runtime-type inspection to every
  MFT category;
- Testing 1080p30 HEVC Main/NV12 and Main10/P010 type negotiation;
- Checking whether a video MFT advertises D3D11 awareness as a prerequisite
  for a same-adapter zero-copy path.
  
# Note:  
The cleanup order is important:

```text
MFTEnumEx
    |
    +-- IMFActivate[0]  -> Release
    +-- IMFActivate[1]  -> Release
    +-- ...
    |
    +-- returned array  -> CoTaskMemFree
```
  
`CoTaskMemFree` frees only the array storage. It does not replace releasing the  
COM interfaces stored in that array.  
  
## Identifying the hardware adapter
  
When `Hardware MFTs` and a video category are selected, the sample enumerates  
the installed DXGI adapters and calls `MFTEnum2` once for each physical  
adapter. The adapter LUID is supplied through an `IMFAttributes` store using  
`MFT_ENUM_ADAPTER_LUID`. The LUID is stored with `SetBlob`; using `SetUINT64`  
has the same eight-byte payload but the wrong Media Foundation attribute type.  
  
Audio and other non-video categories are not DXGI-adapter resources. They use  
the system-wide `MFTEnumEx` path even when the hardware scope is selected.  
  
This is different from ordinary `MFTEnumEx` enumeration, which can report that  
a hardware transform exists without identifying the GPU that provides it.  
  
```text
DXGI adapter -> adapter LUID -> MFTEnum2 -> adapter-specific hardware MFTs  
```
  
The same MFT name may appear once for each GPU. That means Windows reports the  
transform for both adapters rather than exposing one unattributed system-wide  
entry.  
  
An adapter can also return zero transforms. This is a useful result: a DXGI  
graphics adapter is not necessarily equipped with a hardware video encoder or  
decoder. **Probe selected category** lists every physical adapter for hardware  
video categories, including those with zero matches, so "not queried" and  
"queried but unsupported" cannot be confused.  
  
Some drivers return duplicate activation objects for the same hardware MFT.  
The explorer shows the raw activation count, then collapses objects with the  
same adapter, transform CLSID, and hardware symbolic link. Capability counts  
use the unique result; duplicate activation objects must not be interpreted as  
additional hardware engines or concurrent-session capacity.  
  
MfPack's `MFTEnum2` declaration uses `PMFT_REGISTER_TYPE_INFO` for its optional  
input and output filters. This matches the Windows SDK and allows `nil` to mean  
that all registered media types should be considered.  
  
## Registration types versus runtime types  
  
The input and output types shown in this milestone come from the  
`MFT_INPUT_TYPES_Attributes` and `MFT_OUTPUT_TYPES_Attributes` registration  
blobs. They describe what the MFT advertised when it was registered.  
  
They are not necessarily a complete list of the media types returned by an  
activated `IMFTransform`. Some transforms determine their available output  
types only after an input type has been selected.  
  
Use **Activate and inspect** to explore `GetInputAvailableType` and  
`GetOutputAvailableType`. The command is deliberately explicit because  
activation can allocate GPU or codec-driver resources.  
  
Use **Probe selected category** to repeat this generic inspection for every  
transform matching the current category and scope. The report includes  
registered types, activation success, asynchronous state, D3D11 awareness,  
stream counts and IDs, and runtime types. Every activation is shut down before  
the next transform is inspected.  
  
Some MFTs cannot enumerate one side until a type has been selected on the  
other side. In that case the explorer reports `MF_E_TRANSFORM_TYPE_NOT_SET`  
in plain language during general inspection.  
  
Generic inspection cannot safely invent the configuration data required by  
every codec and MFT role. Format-specific negotiation is therefore implemented  
as an extension strategy. The first strategy recognizes an HEVC video encoder,  
builds complete 1920x1080, 30 fps media types, sets the required output type  
first, and tests the input type with `MFT_SET_TYPE_TEST_ONLY`. It performs  
separate Main/NV12 and Main10/P010 tests. Other transforms still receive the  
full generic report and are explicitly marked as generic-only until an  
appropriate strategy is added. No samples are submitted by the probe.  
  
`MF_SA_D3D11_AWARE` is reported as a zero-copy *candidate*, not a guarantee.  
A production pipeline must still create its D3D device on the same adapter,  
provide the corresponding DXGI device manager, and verify actual sample flow.  
  
## Building
  
Open `MfMftExplorer.dproj` in Delphi and build the Win32 Debug configuration.  
The project search path points to the MfPack `src` directory.  
  
  
Project: Media Foundation - MFPack - Samples  
  
Project location:  
https://github.com/FactoryXCode/MfPack  
https://sourceforge.net/projects/MFPack  
  
First release date: 04-08-2026  
Final release date: 24/08/2026  
  
Copyright © FactoryX. All rights reserved.
