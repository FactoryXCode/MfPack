// FactoryX
//
// Copyright: � FactoryX. All rights reserved.
//
// Project: MFPack - XAudio2
// Project location: https://sourceforge.net/projects/MFPack
// Module: MfPack.XApo.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 2.6.4
// Description:   
//  This version of XAudio2 is available only in Windows 8 or later.
//  Use the XAudio2 headers and libraries from the DirectX SDK with
//  applications that target Windows 7 and earlier versions.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/05/2020                     Kraftwerk release. (WIN10 April 2020 update, version 20H1)
//------------------------------------------------------------------------------
//
// Remarks:     
//           
//             1.  Definition of terms:
//                     DSP: Digital Signal Processing.
// 
//                     CBR: Constant BitRate -- DSP that consumes a constant number of
//                          input samples to produce an output sample.
//                          For example, a 22kHz to 44kHz resampler is CBR DSP.
//                          Even though the number of input to output samples differ,
//                          the ratio between input to output rate remains constant.
//                          All user-defined XAPOs are assumed to be CBR as
//                          XAudio2 only allows CBR DSP to be added to an effect chain.
// 
//                     XAPO: Cross-platform Audio Processing Object --
//                           a thin wrapper that manages DSP code, allowing it
//                           to be easily plugged into an XAudio2 effect chain.
// 
//                     Frame: A block of samples, one per channel,
//                            to be played simultaneously.
//                            E.g. a mono stream has one sample per frame.
// 
//                     In-Place: Processing such that the input buffer equals the
//                               output buffer (i.e. input data modified directly).
//                               This form of processing is generally more efficient
//                               than using separate memory for input and output.
//                               However, an XAPO may not perform format conversion
//                               when processing in-place.
// 
//             2.  XAPO member variables are divided into three classifications:
//                     Immutable: Set once via IXAPO.Initialize and remain
//                                constant during the lifespan of the XAPO.
// 
//                     Locked: May change before the XAPO is locked via
//                             IXAPO.LockForProcess but remain constant
//                             until IXAPO.UnlockForProcess is called.
// 
//                     Dynamic: May change from one processing pass to the next,
//                              usually via IXAPOParameters::SetParameters.
//                              XAPOs should assign reasonable defaults to their dynamic
//                              variables during IXAPO.Initialize/LockForProcess so
//                              that calling IXAPOParameters.SetParameters is not
//                              required before processing begins.
// 
//                 When implementing an XAPO, determine the type of each variable and
//                 initialize them in the appropriate method.  Immutable variables are
//                 generally preferable over locked which are preferable over dynamic.
//                 That is, one should strive to minimize XAPO state changes for
//                 best performance, maintainability, and ease of use.
// 
//             3.  To minimize glitches, the realtime audio processing thread must
//                 not block.  XAPO methods called by the realtime thread are commented
//                 as non-blocking and therefore should not use blocking synchronization,
//                 allocate memory, access the disk, etc.  The XAPO interfaces were
//                 designed to allow an effect implementer to move such operations
//                 into other methods called on an application controlled thread.
// 
//             4.  Extending functionality is accomplished through the addition of new
//                 COM interfaces.  For example, if a new member is added to a parameter
//                 structure, a new interface using the new structure should be added,
//                 leaving the original interface unchanged.
//                 This ensures consistent communication between future versions of
//                 XAudio2 and various versions of XAPOs that may exist in an application.
// 
//             5.  All audio data is interleaved in XAudio2.
//                 The default audio format for an effect chain is WAVE_FORMAT_IEEE_FLOAT.
// 
//             6.  User-defined XAPOs should assume all input and output buffers are
//                 16-byte aligned.
// 
//             7.  See XAPOBase.h for an XAPO base class which provides a default
//                 implementation for most of the interface methods defined below.     */
//           
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: xapo.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
// 
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
// 
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
// 
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
// 
//==============================================================================
unit MfPack.XApo;

  {$HPPEMIT '#include "x3daudio.h"'}

interface

uses

  {WinApi}
  //WinApi.ActiveX {opt},
  {MfPack}
  MfPack.MfpTypes,
  MfPack.MmReg,
  MfPack.ComBaseApi; // ActiveX

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'MfPack.inc'}

  // When using pointer array calculations, POINTERMATH should be turned ON
  // {$POINTERMATH ON}


//--------------<D-E-F-I-N-I-T-I-O-N-S>-------------------------------------//


const

  // XAPO error codes
  FACILITY_XAPO                       = $897;
  {$EXTERNALSYM FACILITY_XAPO}
  XAPO_E_FORMAT_UNSUPPORTED           = $88970001;  // requested audio format unsupported
  {$EXTERNALSYM XAPO_E_FORMAT_UNSUPPORTED}

  // supported number of channels (samples per frame) range
  XAPO_MIN_CHANNELS                   = 1;
  {$EXTERNALSYM XAPO_MIN_CHANNELS}
  XAPO_MAX_CHANNELS                   = 64;
  {$EXTERNALSYM XAPO_MAX_CHANNELS}

  // supported framerate range
  XAPO_MIN_FRAMERATE                  = 1000;
  {$EXTERNALSYM XAPO_MIN_FRAMERATE}
  XAPO_MAX_FRAMERATE                  = 200000;
  {$EXTERNALSYM XAPO_MAX_FRAMERATE}

  // unicode string length, including terminator, used with XAPO_REGISTRATION_PROPERTIES
  XAPO_REGISTRATION_STRING_LENGTH     = 256;
  {$EXTERNALSYM XAPO_REGISTRATION_STRING_LENGTH}


  // XAPO property flags, used with XAPO_REGISTRATION_PROPERTIES.Flags:
  // Number of channels of input and output buffers must match,
  // applies to XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.pFormat.
  XAPO_FLAG_CHANNELS_MUST_MATCH       = $00000001;
  {$EXTERNALSYM XAPO_FLAG_CHANNELS_MUST_MATCH}

  // Framerate of input and output buffers must match,
  // applies to XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.pFormat.
  XAPO_FLAG_FRAMERATE_MUST_MATCH      = $00000002;
  {$EXTERNALSYM XAPO_FLAG_FRAMERATE_MUST_MATCH}

  // Bit depth of input and output buffers must match,
  // applies to XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.pFormat.
  // Container size of input and output buffers must also match if
  // XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.pFormat is WAVEFORMATEXTENSIBLE.
  XAPO_FLAG_BITSPERSAMPLE_MUST_MATCH  = $00000004;
  {$EXTERNALSYM XAPO_FLAG_BITSPERSAMPLE_MUST_MATCH}

  // Number of input and output buffers must match,
  // applies to XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.
  //
  // Also, XAPO_REGISTRATION_PROPERTIES.MinInputBufferCount must
  // equal XAPO_REGISTRATION_PROPERTIES.MinOutputBufferCount and
  // XAPO_REGISTRATION_PROPERTIES.MaxInputBufferCount must equal
  // XAPO_REGISTRATION_PROPERTIES.MaxOutputBufferCount when used.
  XAPO_FLAG_BUFFERCOUNT_MUST_MATCH    = $00000008;
  {$EXTERNALSYM XAPO_FLAG_BUFFERCOUNT_MUST_MATCH}

  // XAPO must be run in-place.  Use this flag only if your DSP
  // implementation cannot process separate input and output buffers.
  // If set, the following flags must also be set:
  //     XAPO_FLAG_CHANNELS_MUST_MATCH
  //     XAPO_FLAG_FRAMERATE_MUST_MATCH
  //     XAPO_FLAG_BITSPERSAMPLE_MUST_MATCH
  //     XAPO_FLAG_BUFFERCOUNT_MUST_MATCH
  //     XAPO_FLAG_INPLACE_SUPPORTED
  //
  // Multiple input and output buffers may be used with in-place XAPOs,
  // though the input buffer count must equal the output buffer count.
  // When multiple input/output buffers are used, the XAPO may assume
  // input buffer [N] equals output buffer [N] for in-place processing.
  XAPO_FLAG_INPLACE_REQUIRED          = $00000020;
  {$EXTERNALSYM XAPO_FLAG_INPLACE_REQUIRED}

  // XAPO may be run in-place.  If the XAPO is used in a chain
  // such that the requirements for XAPO_FLAG_INPLACE_REQUIRED are met,
  // XAudio2 will ensure the XAPO is run in-place.  If not met, XAudio2
  // will still run the XAPO albeit with separate input and output buffers.
  //
  // For example, consider an effect which may be ran in stereo->5.1 mode or
  // mono->mono mode.  When set to stereo->5.1, it will be run with separate
  // input and output buffers as format conversion is not permitted in-place.
  // However, if configured to run mono->mono, the same XAPO can be run
  // in-place.  Thus the same implementation may be conveniently reused
  // for various input/output configurations, while taking advantage of
  // in-place processing when possible.
  XAPO_FLAG_INPLACE_SUPPORTED         = $00000010;
  {$EXTERNALSYM XAPO_FLAG_INPLACE_SUPPORTED}


//--------------<D-A-T-A---T-Y-P-E-S>---------------------------------------//
  //#pragma pack(push, 1) // set packing alignment to ensure consistency across arbitrary build environments
  // The default alignment is 8 bytes (quad word) for win64 and 1 byte for win32, unless the project alignment settings are changed.
  // Disable field aligned. All record and class structures will be packed.
  {$ALIGN 1}


type
  // XAPO registration properties, describes general XAPO characteristics, used with IXAPO.GetRegistrationProperties

  PXAPO_REGISTRATION_PROPERTIES = ^XAPO_REGISTRATION_PROPERTIES;
  XAPO_REGISTRATION_PROPERTIES = record
    clsid: CLSID;                                                         // COM class ID, used with CoCreate
    FriendlyName: array[0..XAPO_REGISTRATION_STRING_LENGTH - 1] of WideChar;    // friendly name unicode string
    CopyrightInfo: array[0..XAPO_REGISTRATION_STRING_LENGTH - 1] of WideChar;   // copyright information unicode string
    MajorVersion: UINT32;                                                 // major version
    MinorVersion: UINT32;                                                 // minor version
    Flags: UINT32;                                                        // XAPO property flags, describes supported input/output configuration
    MinInputBufferCount: UINT32;                                          // minimum number of input buffers required for processing, can be 0
    MaxInputBufferCount: UINT32;                                          // maximum number of input buffers supported for processing, must be >= MinInputBufferCount
    MinOutputBufferCount: UINT32;                                         // minimum number of output buffers required for processing, can be 0, must match MinInputBufferCount when XAPO_FLAG_BUFFERCOUNT_MUST_MATCH used
    MaxOutputBufferCount: UINT32;                                         // maximum number of output buffers supported for processing, must be >= MinOutputBufferCount, must match MaxInputBufferCount when XAPO_FLAG_BUFFERCOUNT_MUST_MATCH used
  end;
  {$EXTERNALSYM XAPO_REGISTRATION_PROPERTIES}


  // LockForProcess buffer parameters:
  // Defines buffer parameters that remain constant while an XAPO is locked.
  // Used with IXAPO.LockForProcess.
  //
  // For CBR XAPOs, MaxFrameCount is the only number of frames
  // IXAPO.Process would have to handle for the respective buffer.

  PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS = ^XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
  XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS = record
    pFormat: PWAVEFORMATEX;     // buffer audio format
    MaxFrameCount: UINT32;    // maximum number of frames in respective buffer that IXAPO::Process would have to handle, irrespective of dynamic variable settings, can be 0
  end;
  {$EXTERNALSYM XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS}
  XAPO_LOCKFORPROCESS_PARAMETERS = XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
  {$EXTERNALSYM XAPO_LOCKFORPROCESS_PARAMETERS}
  //
  TXAPOLockForProcessBufferParametersArray = array [0.. 65535] of XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
  {$EXTERNALSYM TXAPOLockForProcessBufferParametersArray}


  // Buffer flags:
  // Describes assumed content of the respective buffer.
  // Used with XAPO_PROCESS_BUFFER_PARAMETERS.BufferFlags.
  //
  // This meta-data can be used by an XAPO to implement
  // optimizations that require knowledge of a buffer's content.
  //
  // For example, XAPOs that always produce silent output from silent input
  // can check the flag on the input buffer to determine if any signal
  // processing is necessary.  If silent, the XAPO may simply set the flag
  // on the output buffer to silent and return, optimizing out the work of
  // processing silent data:  XAPOs that generate silence for any reason may
  // set the buffer's flag accordingly rather than writing out silent
  // frames to the buffer itself.
  //
  // The flags represent what should be assumed is in the respective buffer.
  // The flags may not reflect what is actually stored in memory.

  PXAPO_BUFFER_FLAGS = ^XAPO_BUFFER_FLAGS;
  XAPO_BUFFER_FLAGS = (
    XAPO_BUFFER_SILENT,     // silent data should be assumed, respective memory may be uninitialized
    XAPO_BUFFER_VALID       // arbitrary data should be assumed (may or may not be silent frames), respective memory initialized
  );
  {$EXTERNALSYM XAPO_BUFFER_FLAGS}


  // Process buffer parameters:
  // Defines buffer parameters that may change from one
  // processing pass to the next.  Used with IXAPO.Process.
  //
  // Note the byte size of the respective buffer must be at least:
  //      XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount * XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.pFormat->nBlockAlign
  //
  // Although the audio format and maximum size of the respective
  // buffer is locked (defined by XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS),
  // the actual memory address of the buffer given is permitted to change
  // from one processing pass to the next.
  //
  // For CBR XAPOs, ValidFrameCount is constant while locked and equals
  // the respective XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount.

  PXAPO_PROCESS_BUFFER_PARAMETERS = ^XAPO_PROCESS_BUFFER_PARAMETERS;
  XAPO_PROCESS_BUFFER_PARAMETERS = record
    pBuffer: Pointer;           // audio data buffer, must be non-NULL
    BufferFlags: XAPO_BUFFER_FLAGS;   // describes assumed content of pBuffer, does not affect ValidFrameCount
    ValidFrameCount: UINT32;    // number of frames of valid data, must be within respective [0, XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount], always XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount for CBR/user-defined XAPOs, does not affect BufferFlags
  end;
  {$EXTERNALSYM XAPO_PROCESS_BUFFER_PARAMETERS}
  // If you don't want to use pointers to array's
  TXAPOProcessBufferParametersArray = array [0..65535] of XAPO_PROCESS_BUFFER_PARAMETERS;
  {$EXTERNALSYM TXAPOProcessBufferParametersArray}

//--------------<M-A-C-R-O-S>-----------------------------------------------//
    // Memory allocation macros that allow one module to allocate memory and
    // another to free it, by guaranteeing that the same heap manager is used
    // regardless of differences between build environments of the two modules.
    //
    // Used by IXAPO methods that must allocate arbitrary sized structures
    // such as WAVEFORMATEX that are subsequently returned to the application.
    //
    // function XAPOAlloc
    // Size, in bytes, of the memory block to be allocated.
    function XAPOAlloc(size: DWord): Pointer;
    {$EXTERNALSYM XAPOAlloc}
    procedure XAPOFree(p: Pointer);
    {$EXTERNALSYM XAPOFree}


type
//--------------<I-N-T-E-R-F-A-C-E-S>---------------------------------------//



  // Interface IXAPO
  // ===============
  // The only mandatory XAPO COM interface -- a thin wrapper that manages
  // DSP code, allowing it to be easily plugged into an XAudio2 effect chain.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IXAPO);'}
  {$EXTERNALSYM IXAPO}
  IXAPO = interface(IUnknown)
  ['{A410B984-9839-4819-A0BE-2856AE6B3ADB}']
    // DESCRIPTION:
    //  Allocates a copy of the registration properties of the XAPO.
    //
    // PARAMETERS:
    //  ppRegistrationProperties - [out] receives pointer to copy of registration properties, use XAPOFree to free structure, left untouched on failure
    //
    // RETURN VALUE:
    //  COM error code
    ////
    function GetRegistrationProperties(out ppRegistrationProperties: XAPO_REGISTRATION_PROPERTIES): HRESULT; stdcall;

    ////
    // DESCRIPTION:
    //  Queries if an input/output configuration is supported.
    //
    // REMARKS:
    //  This method allows XAPOs to express dependency of input format
    //  with respect to output format.
    //
    //  If the input/output format pair configuration is unsupported,
    //  this method also determines the nearest input format supported.
    //  Nearest meaning closest bit depth, framerate, and channel count,
    //  in that order of importance.
    //
    //  The behaviour of this method should remain constant after the
    //  XAPO has been initialized.
    //
    // PARAMETERS:
    //  pOutputFormat          - [in]  output format known to be supported
    //  pRequestedInputFormat  - [in]  input format to examine
    //  ppSupportedInputFormat - [out] receives pointer to nearest input format supported if not NULL and input/output configuration unsupported, use XAPOFree to free structure, left untouched on any failure except XAPO_E_FORMAT_UNSUPPORTED
    //
    // RETURN VALUE:
    //  COM error code, including:
    //    S_OK                      - input/output configuration supported, ppSupportedInputFormat left untouched
    //    XAPO_E_FORMAT_UNSUPPORTED - input/output configuration unsupported, ppSupportedInputFormat receives pointer to nearest input format supported if not NULL
    //    E_INVALIDARG              - either audio format invalid, ppSupportedInputFormat left untouched
    ////
    function IsInputFormatSupported(pOutputFormat: WAVEFORMATEX;
                                    pRequestedInputFormat: WAVEFORMATEX;
                                    out ppSupportedInputFormat: WAVEFORMATEX): HRESULT; stdcall;

    ////
    // DESCRIPTION:
    //  Queries if an input/output configuration is supported.
    //
    // REMARKS:
    //  This method allows XAPOs to express dependency of output format
    //  with respect to input format.
    //
    //  If the input/output format pair configuration is unsupported,
    //  this method also determines the nearest output format supported.
    //  Nearest meaning closest bit depth, framerate, and channel count,
    //  in that order of importance.
    //
    //  The behaviour of this method should remain constant after the
    //  XAPO has been initialized.
    //
    // PARAMETERS:
    //  pInputFormat            - [in]  input format known to be supported
    //  pRequestedOutputFormat  - [in]  output format to examine
    //  ppSupportedOutputFormat - [out] receives pointer to nearest output format supported if
    //                                  not Nil and input/output configuration unsupported,
    //                                  use XAPOFree to free structure, left untouched on any failure except XAPO_E_FORMAT_UNSUPPORTED
    //
    // RETURN VALUE:
    //  COM error code, including:
    //    S_OK                      - input/output configuration supported, ppSupportedOutputFormat left untouched
    //    XAPO_E_FORMAT_UNSUPPORTED - input/output configuration unsupported, ppSupportedOutputFormat receives pointer to nearest output format supported if not NULL
    //    E_INVALIDARG              - either audio format invalid, ppSupportedOutputFormat left untouched
    ////
    function IsOutputFormatSupported(pInputFormat: WAVEFORMATEX;
                                     pRequestedOutputFormat: WAVEFORMATEX;
                                     out ppSupportedOutputFormat: WAVEFORMATEX): HRESULT; stdcall;

    ////
    // DESCRIPTION:
    //  Performs any effect-specific initialization if required.
    //
    // REMARKS:
    //  The contents of pData are defined by the XAPO.
    //  Immutable variables (constant during the lifespan of the XAPO)
    //  should be set once via this method.
    //  Once initialized, an XAPO cannot be initialized again.
    //
    //  An XAPO should be initialized before passing it to XAudio2
    //  as part of an effect chain.  XAudio2 will not call this method;
    //  it exists for future content-driven initialization.
    //
    // PARAMETERS:
    //  pData        - [in] effect-specific initialization parameters, may be NIl if DataByteSize = 0
    //  DataByteSize - [in] size of pData in bytes, may be 0 if pData is NIL
    //
    // RETURN VALUE:
    //  COM error code
    ////
    function Initialize(pData: Pointer;
                        DataByteSize: UINT32): HResult; stdcall;

    ////
    // DESCRIPTION:
    //  Resets variables dependent on frame history.
    //
    // REMARKS:
    //  All other variables remain unchanged, including variables set by
    //  IXAPOParameters.SetParameters.
    //
    //  For example, an effect with delay should zero out its delay line
    //  during this method, but should not reallocate anything as the
    //  XAPO remains locked with a constant input/output configuration.
    //
    //  XAudio2 calls this method only if the XAPO is locked.
    //  This method should not block as it is called from the
    //  realtime thread.
    //
    // PARAMETERS:
    //  void
    //
    // RETURN VALUE:
    //  void
    ////
    procedure Reset(); stdcall;

    ////
    // DESCRIPTION:
    //  Locks the XAPO to a specific input/output configuration,
    //  allowing it to do any final initialization before Process
    //  is called on the realtime thread.
    //
    // REMARKS:
    //  Once locked, the input/output configuration and any other locked
    //  variables remain constant until UnlockForProcess is called.
    //
    //  XAPOs should assert the input/output configuration is supported
    //  and that any required effect-specific initialization is complete.
    //  IsInputFormatSupported, IsOutputFormatSupported, and Initialize
    //  should be called as necessary before this method is called.
    //
    //  All internal memory buffers required for Process should be
    //  allocated by the time this method returns successfully
    //  as Process is non-blocking and should not allocate memory.
    //
    //  Once locked, an XAPO cannot be locked again until
    //  UnLockForProcess is called.
    //
    // PARAMETERS:
    //  InputLockedParameterCount  - [in] number of input buffers, must be within [XAPO_REGISTRATION_PROPERTIES.MinInputBufferCount, XAPO_REGISTRATION_PROPERTIES.MaxInputBufferCount]
    //  pInputLockedParameters     - [in] array of input locked buffer parameter structures, may be NIL if InputLockedParameterCount == 0, otherwise must have InputLockedParameterCount elements
    //  OutputLockedParameterCount - [in] number of output buffers, must be within [XAPO_REGISTRATION_PROPERTIES.MinOutputBufferCount, XAPO_REGISTRATION_PROPERTIES.MaxOutputBufferCount], must match InputLockedParameterCount when XAPO_FLAG_BUFFERCOUNT_MUST_MATCH used
    //  pOutputLockedParameters    - [in] array of output locked buffer parameter structures, may be NIL if OutputLockedParameterCount == 0, otherwise must have OutputLockedParameterCount elements
    //
    // RETURN VALUE:
    //  COM error code
    ////
    function LockForProcess(InputLockedParameterCount: UINT32;
                            pInputLockedParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                            OutputLockedParameterCount: UINT32;
                            pOutputLockedParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS): HRESULT; stdcall; overload;

    function LockForProcess(InputLockedParameterCount: UINT32;
                            pInputLockedParameters: TXAPOLockForProcessBufferParametersArray;
                            OutputLockedParameterCount: UINT32;
                            pOutputLockedParameters: TXAPOLockForProcessBufferParametersArray): HRESULT; stdcall; overload;

    ////
    // DESCRIPTION:
    //  Opposite of LockForProcess.  Variables allocated during
    //  LockForProcess should be deallocated by this method.
    //
    // REMARKS:
    //  Unlocking an XAPO allows an XAPO instance to be reused with
    //  different input/output configurations.
    //
    // PARAMETERS:
    //  void
    //
    // RETURN VALUE:
    //  void
    ////
    procedure UnlockForProcess(); stdcall;

    ////
    // DESCRIPTION:
    //  Runs the XAPO's DSP code on the given input/output buffers.
    //
    // REMARKS:
    //  In addition to writing to the output buffers as appropriate,
    //  an XAPO must set the BufferFlags and ValidFrameCount members
    //  of all elements in pOutputProcessParameters accordingly.
    //
    //  ppInputProcessParameters will not necessarily be the same as
    //  ppOutputProcessParameters for in-place processing, rather
    //  the pBuffer members of each will point to the same memory.
    //
    //  Multiple input/output buffers may be used with in-place XAPOs,
    //  though the input buffer count must equal the output buffer count.
    //  When multiple input/output buffers are used with in-place XAPOs,
    //  the XAPO may assume input buffer [N] equals output buffer [N].
    //
    //  When IsEnabled is FALSE, the XAPO should process thru.
    //  Thru processing means an XAPO should not apply its normal
    //  processing to the given input/output buffers during Process.
    //  It should instead pass data from input to output with as little
    //  modification possible.  Effects that perform format conversion
    //  should continue to do so.  The effect must ensure transitions
    //  between normal and thru processing do not introduce
    //  discontinuities into the signal.
    //
    //  XAudio2 calls this method only if the XAPO is locked.
    //  This method should not block as it is called from the
    //  realtime thread.
    //
    // PARAMETERS:
    //  InputProcessParameterCount  - [in]     number of input buffers, matches respective InputLockedParameterCount parameter given to LockForProcess
    //  pInputProcessParameters     - [in]     array of input process buffer parameter structures, may be NIL if InputProcessParameterCount == 0, otherwise must have InputProcessParameterCount elements
    //  OutputProcessParameterCount - [in]     number of output buffers, matches respective OutputLockedParameterCount parameter given to LockForProcess
    //  pOutputProcessParameters    - [in/out] array of output process buffer parameter structures, may be NIL if OutputProcessParameterCount == 0, otherwise must have OutputProcessParameterCount elements
    //  IsEnabled                   - [in]     TRUE to process normally, FALSE to process thru
    //
    // RETURN VALUE:
    //  void
    ////
    procedure Process(InputProcessParameterCount: UINT32;
                      pInputProcessParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                      OutputProcessParameterCount: UINT32;
                      var pOutputProcessParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                      IsEnabled: BOOL); stdcall; overload;

    procedure Process(InputProcessParameterCount: UINT32;
                      pInputProcessParameters: TXAPOProcessBufferParametersArray;
                      OutputProcessParameterCount: UINT32;
                      var pOutputProcessParameters: TXAPOProcessBufferParametersArray;
                      IsEnabled: BOOL); stdcall; overload;

    ////
    // DESCRIPTION:
    //  Returns the number of input frames required to generate the
    //  requested number of output frames.
    //
    // REMARKS:
    //  XAudio2 may call this method to determine how many input frames
    //  an XAPO requires.  This is constant for locked CBR XAPOs;
    //  this method need only be called once while an XAPO is locked.
    //
    //  XAudio2 calls this method only if the XAPO is locked.
    //  This method should not block as it is called from the
    //  realtime thread.
    //
    // PARAMETERS:
    //  OutputFrameCount - [in] requested number of output frames, must be within respective [0, XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount], always XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount for CBR/user-defined XAPOs
    //
    // RETURN VALUE:
    //  number of input frames required
    ////
    function CalcInputFrames(OutputFrameCount: UINT32): UINT32; stdcall;

    ////
    // DESCRIPTION:
    //  Returns the number of output frames generated for the
    //  requested number of input frames.
    //
    // REMARKS:
    //  XAudio2 may call this method to determine how many output frames
    //  an XAPO will generate.  This is constant for locked CBR XAPOs;
    //  this method need only be called once while an XAPO is locked.
    //
    //  XAudio2 calls this method only if the XAPO is locked.
    //  This method should not block as it is called from the
    //  realtime thread.
    //
    // PARAMETERS:
    //  InputFrameCount - [in] requested number of input frames, must be within respective [0, XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount], always XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.MaxFrameCount for CBR/user-defined XAPOs
    //
    // RETURN VALUE:
    //  number of output frames generated
    ////
    function CalcOutputFrames(InputFrameCount: UINT32): UINT32; stdcall;

  end;
  IID_IXAPO = IXAPO;
  {$EXTERNALSYM IID_IXAPO}



  // Interface IXAPOParameters
  // =========================
  // Optional XAPO COM interface that allows an XAPO to use
  // effect-specific parameters.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IXAPOParameters);'}
  {$EXTERNALSYM IXAPOParameters}
  IXAPOParameters = interface(IUnknown)
  ['{26D95C66-80F2-499A-AD54-5AE7F01C6D98}']
    // DESCRIPTION:
    //  Sets effect-specific parameters.
    //
    // REMARKS:
    //  This method may only be called on the realtime thread;
    //  no synchronization between it and IXAPO.Process is necessary.
    //
    //  This method should not block as it is called from the
    //  realtime thread.
    //
    // PARAMETERS:
    //  pParameters       - [in] effect-specific parameter block, must be >= Nil
    //  ParameterByteSize - [in] size of pParameters in bytes, must be > 0
    //
    // RETURN VALUE:
    //  void
    ////
    procedure SetParameters(pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;

    ////
    // DESCRIPTION:
    //  Gets effect-specific parameters.
    //
    // REMARKS:
    //  Unlike SetParameters, XAudio2 does not call this method on the
    //  realtime thread.  Thus, the XAPO must protect variables shared
    //  with SetParameters/Process using appropriate synchronization.
    //
    // PARAMETERS:
    //  pParameters       - [out] receives effect-specific parameter block, must be >= Nil
    //  ParameterByteSize - [in]  size of pParameters in bytes, must be > 0
    //
    // RETURN VALUE:
    //  void
    ////
    procedure GetParameters(out pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;


    // Called by SetParameters() to allow for user-defined parameter validation.
    // SetParameters validates that ParameterByteSize = m_uParameterBlockByteSize
    // so the user may assume/assert ParameterByteSize = m_uParameterBlockByteSize.
    // This method should not block as it is called from the realtime thread.
    procedure OnSetParameters(pParameters: Pointer;
                              ParameterByteSize: UINT32); stdcall;

  end;
  IID_IXAPOParameters = IXAPOParameters;
  {$EXTERNALSYM IID_IXAPOParameters}

  //set allignment back to default
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  // Additional Prototypes for ALL interfaces


  // End of Additional Prototypes

implementation


function XAPOAlloc(size: DWord): Pointer;
begin
  Result:= CoTaskMemAlloc(size);
end;

procedure XAPOFree(p: Pointer);
begin
  CoTaskMemFree(p);
end;

  // Implement Additional functions here.

end.
