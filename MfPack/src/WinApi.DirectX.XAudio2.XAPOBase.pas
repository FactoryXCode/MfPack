// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi.DirectX.XAudio2 - XAudio2
// Project location: http://sourceforge.net/projects/MFPack
// Module: WinApi.DirectX.XAudio2.XAPOBase.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: See XAPO.h for the rules governing XAPO interface behaviour.
// 
// 
//
// Related objects: -
// Related projects: MfPackX300
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19041.0
//
// Todo: -
//
//==============================================================================
// Source: xapobase.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
// 
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
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
unit WinApi.DirectX.XAudio2.XAPOBase;

  {$HPPEMIT '#include "xapobase.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinMM.MMReg,
  {DirectX}
  WinApi.DirectX.XAudio2.XApo;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const

  // default audio format ranges supported, applies to XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS.pFormat
  {$EXTERNALSYM XAPOBASE_DEFAULT_FORMAT_TAG}
  XAPOBASE_DEFAULT_FORMAT_TAG              = WAVE_FORMAT_IEEE_FLOAT;  // 32-bit float only, applies to WAVEFORMATEX.wFormatTag or WAVEFORMATEXTENSIBLE.SubFormat when used
  {$EXTERNALSYM XAPOBASE_DEFAULT_FORMAT_MIN_CHANNELS}
  XAPOBASE_DEFAULT_FORMAT_MIN_CHANNELS     = XAPO_MIN_CHANNELS;       // minimum channel count, applies to WAVEFORMATEX.nChannels
  {$EXTERNALSYM XAPOBASE_DEFAULT_FORMAT_MAX_CHANNELS}
  XAPOBASE_DEFAULT_FORMAT_MAX_CHANNELS     = XAPO_MAX_CHANNELS;       // maximum channel count, applies to WAVEFORMATEX.nChannels
  {$EXTERNALSYM XAPOBASE_DEFAULT_FORMAT_MIN_FRAMERATE}
  XAPOBASE_DEFAULT_FORMAT_MIN_FRAMERATE    = XAPO_MIN_FRAMERATE;      // minimum framerate, applies to WAVEFORMATEX.nSamplesPerSec
  {$EXTERNALSYM XAPOBASE_DEFAULT_FORMAT_MAX_FRAMERATE}
  XAPOBASE_DEFAULT_FORMAT_MAX_FRAMERATE    = XAPO_MAX_FRAMERATE;      // maximum framerate, applies to WAVEFORMATEX.nSamplesPerSec
  {$EXTERNALSYM XAPOBASE_DEFAULT_FORMAT_BITSPERSAMPLE}
  XAPOBASE_DEFAULT_FORMAT_BITSPERSAMPLE    = 32;  // 32-bit float only, applies to WAVEFORMATEX.wBitsPerSample and WAVEFORMATEXTENSIBLE.wValidBitsPerSample when used

  // default XAPO property flags supported, applies to XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS
  {$EXTERNALSYM XAPOBASE_DEFAULT_FLAG}
  XAPOBASE_DEFAULT_FLAG               = (XAPO_FLAG_CHANNELS_MUST_MATCH or
                                         XAPO_FLAG_FRAMERATE_MUST_MATCH or
                                         XAPO_FLAG_BITSPERSAMPLE_MUST_MATCH or
                                         XAPO_FLAG_BUFFERCOUNT_MUST_MATCH or
                                         XAPO_FLAG_INPLACE_SUPPORTED);

  // default number of input and output buffers supported, applies to XAPO_LOCKFORPROCESS_BUFFER_PARAMETERS
  {$EXTERNALSYM XAPOBASE_DEFAULT_BUFFER_COUNT}
  XAPOBASE_DEFAULT_BUFFER_COUNT       = 1;

  //--------------<D-A-T-A---T-Y-P-E-S>---------------------------------------//
  //#pragma pack(push, 8) // set packing alignment to ensure consistency across arbitrary build environments, and ensure synchronization variables used by Interlocked functionality are correctly aligned
  {$ALIGN 8}  // Default for Delphi (Quad Word)


  //////////////////////////////////////////////////////////////////////////////
  // Because Delphi is not capable of (yet) linking with .lib files,
  // both CXAPOBase and CXAPOParametersBase classes are translated with only their interface implementations
  // This should not be a big deal, because the functionality of the IXAPO and
  // IXAPOParameters interfaces will do.
  //////////////////////////////////////////////////////////////////////////////



type

  ////
  // DESCRIPTION:
  //  Default implementation of the IXAPO and IUnknown interface.
  //  Provides overridable implementations for all methods save IXAPO.Process.
  // Platform Requirements
  //  Windows 10 (XAudio2.9);
  //  Windows 8,
  //  Windows Phone 8 (XAudio 2.8);
  //  DirectX SDK (XAudio 2.7)
  ////
  {$EXTERNALSYM CXAPOBase}
  CXAPOBase = class(TInterfacedObject, IXAPO)
  private

    m_pRegistrationProperties: XAPO_REGISTRATION_PROPERTIES;  // pointer to registration properties of the XAPO, set via constructor

    //m_pfnMatrixMixFunction: Pointer;      // optimal matrix function pointer, used for thru processing
    //m_pfl32MatrixCoefficients: FLOAT32;   // matrix coefficient table, used for thru processing
    //m_nSrcFormatType: UINT32;             // input format type, used for thru processing
    //m_fIsScalarMatrix: BOOL;              // TRUE if m_pfl32MatrixCoefficients is diagonal matrix with all main diagonal entries equal, i.e. m_pfnMatrixMixFunction only used for type conversion (no channel conversion), used for thru processing

    m_fIsLocked: BOOL;                      // TRUE if XAPO locked via CXAPOBase.LockForProcess

  protected

    // accessors
    function GetRegistrationPropertiesInternal(): XAPO_REGISTRATION_PROPERTIES; { return m_pRegistrationProperties; }
    function IsLocked(): BOOL; { return m_fIsLocked; }

  public

    // Constructor & destuctor /////////////////////////////////////////////////
    //
    constructor Create(pRegistrationProperties: XAPO_REGISTRATION_PROPERTIES);
    //
    // Destructor
    destructor Destroy(); override;
    //
    ////////////////////////////////////////////////////////////////////////////


    // IXAPO methods:
    ////////////////////////////////////////////////////////////////////////////

    // Allocates a copy of the registration properties of the XAPO.
    // This default implementation returns a copy of the registration
    // properties given to the constructor, allocated via XAPOAlloc.
    function GetRegistrationProperties(out ppRegistrationProperties: XAPO_REGISTRATION_PROPERTIES): HRESULT; stdcall;

    // Queries if a specific input format is supported for a given output format.
    // This default implementation assumes only the format described by the
    // XAPOBASE_DEFAULT_FORMAT values are supported for both input and output.
    function IsInputFormatSupported(pOutputFormat: WAVEFORMATEX;
                                    pRequestedInputFormat: WAVEFORMATEX;
                                    out ppSupportedInputFormat: WAVEFORMATEX): HRESULT; stdcall;

    // Queries if a specific output format is supported for a given input format.
    // This default implementation assumes only the format described by the
    // XAPOBASE_DEFAULT_FORMAT values are supported for both input and output.
    function IsOutputFormatSupported(pInputFormat: WAVEFORMATEX;
                                     pRequestedOutputFormat: WAVEFORMATEX;
                                     out ppSupportedOutputFormat: WAVEFORMATEX): HRESULT; stdcall;

    // Performs any effect-specific initialization.
    // This default implementation is a no-op and only returns S_OK.
    function Initialize(pData: Pointer;
                        DataByteSize: UINT32): HRESULT; stdcall;

    {
        UNREFERENCED_PARAMETER(DataByteSize);
        return S_OK;
    }

    // Resets variables dependent on frame history.
    // This default implementation is a no-op: this base class contains no
    // relevant state to reset.
    procedure Reset(); stdcall;

    // Notifies XAPO of buffer formats Process() will be given.
    // This default implementation performs basic input/output format
    // validation against the XAPO's registration properties.
    // Derived XAPOs should call the base implementation first.
    function LockForProcess(InputLockedParameterCount: UINT32;
                            pInputLockedParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                            OutputLockedParameterCount: UINT32;
                            pOutputLockedParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS): HRESULT; overload; stdcall;


    function LockForProcess(InputLockedParameterCount: UINT32;
                            pInputLockedParameters: TXAPOLockForProcessBufferParametersArray;
                            OutputLockedParameterCount: UINT32;
                            pOutputLockedParameters: TXAPOLockForProcessBufferParametersArray): HRESULT; overload; stdcall;

    // Opposite of LockForProcess.
    // Derived XAPOs should call the base implementation first.
    procedure UnlockForProcess(); stdcall;

    // overloaded methods
    //
    // Need POINTERMATH
    procedure Process(InputProcessParameterCount: UINT32;
                      pInputProcessParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                      OutputProcessParameterCount: UINT32;
                      var pOutputProcessParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                      IsEnabled: BOOL); overload; stdcall;
    // No need for POINTERMATH
    procedure Process(InputProcessParameterCount: UINT32;
                      pInputProcessParameters: TXAPOProcessBufferParametersArray;
                      OutputProcessParameterCount: UINT32;
                      var pOutputProcessParameters: TXAPOProcessBufferParametersArray;
                      IsEnabled: BOOL); overload; stdcall;



    // Returns the number of input frames required to generate the requested number of output frames.
    // By default, this method returns the same number of frames it was passed.
    function CalcInputFrames(OutputFrameCount: UINT32): UINT32; stdcall;

    // Returns the number of output frames generated for the requested number of input frames.
    // By default, this method returns the same number of frames it was passed.
    function CalcOutputFrames(InputFrameCount: UINT32): UINT32; stdcall;

  end;


  //--------------------------------------------------------------------------//

  ////
  // DESCRIPTION:
  //  Extends CXAPOBase, providing a default implementation of the
  //  IXAPOParameters interface with appropriate synchronization to
  //  protect variables shared between IXAPOParameters::GetParameters
  //  and IXAPOParameters.SetParameters/IXAPO.Process.
  //
  //  This class is for parameter blocks whose size is larger than 4 bytes.
  //  For smaller parameter blocks, use atomic operations directly
  //  on the parameters for synchronization.
  ////
  {$EXTERNALSYM CXAPOParametersBase}
  CXAPOParametersBase = class(CXAPOBase, IXAPOParameters)
  private
    m_pParameterBlocks: PByte;              // three contiguous process parameter blocks used for synchronization, user responsible for initialization of parameter blocks before IXAPO::Process/SetParameters/GetParameters called

    // m_pCurrentParameters: PByte;            // pointer to current process parameters, must be aligned for atomic operations
    // m_pCurrentParametersInternal: PByte;    // pointer to current process parameters (temp pointer read by SetParameters/BeginProcess/EndProcess)
    // m_uCurrentParametersIndex: UINT32;      // index of current process parameters

    m_uParameterBlockByteSize: UINT32;      // size of a single parameter block in bytes, must be > 0

    // m_fNewerResultsReady: BOOL;             // TRUE if there exists new processing results not yet picked up by GetParameters(), must be aligned for atomic operations

    m_fProducer: BOOL;                      // TRUE if IXAPO.Process produces data to be returned by GetParameters(), SetParameters() and ParametersChanged() disallowed


  public

    ////
    // PARAMETERS:
    //  pRegistrationProperties - [in] registration properties of the XAPO
    //  pParameterBlocks        - [in] three contiguous process parameter blocks used for synchronization
    //  uParameterBlockByteSize - [in] size of one of the parameter blocks, must be > 0, should be > 4
    //  fProducer               - [in] TRUE if IXAPO.Process produces data to be returned by GetParameters() (SetParameters() and ParametersChanged() disallowed)
    ////
    // CXAPOParametersBase
    // Constructor & destuctor /////////////////////////////////////////////////
    //
    constructor Create(pRegistrationProperties: XAPO_REGISTRATION_PROPERTIES;
                       pParameterBlocks: PByte;
                       uParameterBlockByteSize: UINT32;
                       fProducer: BOOL); virtual;
    //
    // Destructor
    destructor Destroy(); override;
    //
    ////////////////////////////////////////////////////////////////////////////


    // IXAPOParameters methods:
    // Sets effect-specific parameters.
    // This method may only be called on the realtime audio processing thread.
    procedure SetParameters(pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;


    // Gets effect-specific parameters.
    // This method may block and should not be called from the realtime thread.
    // Get the current parameters via BeginProcess.
    procedure GetParameters(out pParameters: Pointer;
                            ParameterByteSize: UINT32); stdcall;


    // Called by SetParameters() to allow for user-defined parameter validation.
    // SetParameters validates that ParameterByteSize = m_uParameterBlockByteSize
    // so the user may assume/assert ParameterByteSize = m_uParameterBlockByteSize.
    // This method should not block as it is called from the realtime thread.
    // Users are expected to use asserts for parameter validation in OnSetParameters.
    //
    // Note: You should not use Asserts when not in debug build configuration.
    // example:
    //    {$IFDEF DEBUG}
    //      Assert(m_uParameterBlockByteSize > 0,
    //             'XAPO ASSERT: Procedure OnSetParameters, m_uParameterBlockByteSize <= 0');
    //      Assert(pParameters > nil,
    //             'XAPO ASSERT: Procedure OnSetParameters, pParameters < nil');
    //      Assert(ParameterByteSize = m_uParameterBlockByteSize,
    //             'XAPO ASSERT: Procedure OnSetParameters, ParameterByteSize <> m_uParameterBlockByteSize');
    //    {$ELSE}
    //       If (m_uParameterBlockByteSize <= 0) or (pParameters = nil) or (ParameterByteSize <> m_uParameterBlockByteSize) then
    //         goto yourerrorhandler
    //    {$END IF}
    //
    //
    procedure OnSetParameters(pParameters: Pointer;
                              ParameterByteSize: UINT32); stdcall;

    ////////////////////////////////////////////////////////////////////////////
    ///
    ///  IMPLEMENTATIONS FOR CXAPOPARAMETERSBASE
    ///
    ////////////////////////////////////////////////////////////////////////////


  end;

  //#pragma pack(pop) // revert packing alignment
  // set back to default
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  //---------------------------------<-EOF->----------------------------------//


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


// Constructor & destuctor /////////////////////////////////////////////////////
//
// Constructor
constructor CXAPOBase.Create(pRegistrationProperties: XAPO_REGISTRATION_PROPERTIES);
begin
  inherited Create();
  m_pRegistrationProperties := pRegistrationProperties;

end;
//
//
// Destructor
destructor CXAPOBase.Destroy();
begin

  inherited Destroy();
end;
//
////////////////////////////////////////////////////////////////////////////////


function CXAPOBase.GetRegistrationPropertiesInternal(): XAPO_REGISTRATION_PROPERTIES;
begin

  Result := m_pRegistrationProperties;

end;


function CXAPOBase.IsLocked(): BOOL;
begin

  Result := m_fIsLocked;

end;


function CXAPOBase.Initialize(pData: Pointer;
                              DataByteSize: UINT32): HRESULT;
begin
  // UNREFERENCED_PARAMETER(DataByteSize);  >> a C++ workaround to surpress compiler W4 warning "Unreferenced Parameter"
  // Just translated this for completeness.
  Result :=  S_OK;
end;



// IXAPO implementations ///////////////////////////////////////////////////////

function CXAPOBase.GetRegistrationProperties(out ppRegistrationProperties: XAPO_REGISTRATION_PROPERTIES): HRESULT;
begin

  Result := E_NOTIMPL; // Do your implementations here, by replacing this line for your code.

end;


function CXAPOBase.IsInputFormatSupported(pOutputFormat: WAVEFORMATEX;
                                          pRequestedInputFormat: WAVEFORMATEX;
                                          out ppSupportedInputFormat: WAVEFORMATEX): HRESULT;
begin

  Result := E_NOTIMPL; // Do your implementations here, by replacing this line for your code.

end;


function CXAPOBase.IsOutputFormatSupported(pInputFormat: WAVEFORMATEX;
                                           pRequestedOutputFormat: WAVEFORMATEX;
                                           out ppSupportedOutputFormat: WAVEFORMATEX): HRESULT;
begin

  Result := E_NOTIMPL; // Do your implementations here, by replacing this line for your code.

end;


procedure CXAPOBase.Reset();
begin

   // Do your implementations here

end;

function CXAPOBase.LockForProcess(InputLockedParameterCount: UINT32;
                                  pInputLockedParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                                  OutputLockedParameterCount: UINT32;
                                  pOutputLockedParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS): HRESULT;
begin

  Result := E_NOTIMPL; // Do your implementations here, by replacing this line for your code.

end;

function CXAPOBase.LockForProcess(InputLockedParameterCount: UINT32;
                                  pInputLockedParameters: TXAPOLockForProcessBufferParametersArray;
                                  OutputLockedParameterCount: UINT32;
                                  pOutputLockedParameters: TXAPOLockForProcessBufferParametersArray): HRESULT;
begin

  Result := E_NOTIMPL; // Do your implementations here, by replacing this line for your code.

end;


procedure CXAPOBase.UnlockForProcess();
begin

   // Do your implementations here

end;


procedure CXAPOBase.Process(InputProcessParameterCount: UINT32;
                            pInputProcessParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                            OutputProcessParameterCount: UINT32;
                            var pOutputProcessParameters: PXAPO_LOCKFORPROCESS_BUFFER_PARAMETERS;
                            IsEnabled: BOOL);
begin

   // Do your implementations here

end;


procedure CXAPOBase.Process(InputProcessParameterCount: UINT32;
                            pInputProcessParameters: TXAPOProcessBufferParametersArray;
                            OutputProcessParameterCount: UINT32;
                            var pOutputProcessParameters: TXAPOProcessBufferParametersArray;
                            IsEnabled: BOOL);
begin

   // Do your implementations here

end;


function CXAPOBase.CalcInputFrames(OutputFrameCount: UINT32): UINT32;
begin

  Result := 0; // Do your implementations here, by replacing this line for your code.

end;


function CXAPOBase.CalcOutputFrames(InputFrameCount: UINT32): UINT32;
begin

  Result := 0; // Do your implementations here, by replacing this line for your code.

end;



// CXAPOParametersBase
// Constructor & destuctor /////////////////////////////////////////////////
//
constructor CXAPOParametersBase.Create(pRegistrationProperties: XAPO_REGISTRATION_PROPERTIES;
                                       pParameterBlocks: PByte;
                                       uParameterBlockByteSize: UINT32;
                                       fProducer: BOOL);
begin
  inherited create(pRegistrationProperties);

  m_pParameterBlocks := pParameterBlocks;
  m_uParameterBlockByteSize := uParameterBlockByteSize;
  m_fProducer := fProducer;


end;
//
// Destructor
destructor CXAPOParametersBase.Destroy();
begin

  inherited Destroy();
end;
//
////////////////////////////////////////////////////////////////////////////


procedure CXAPOParametersBase.SetParameters(pParameters: Pointer;
                                            ParameterByteSize: UINT32);
begin

  // Do your implementations here

end;


procedure CXAPOParametersBase.GetParameters(out pParameters: Pointer;
                                            ParameterByteSize: UINT32);
begin

  // Do your implementations here

end;


procedure CXAPOParametersBase.OnSetParameters(pParameters: Pointer;
                                              ParameterByteSize: UINT32);
begin
{$IFDEF DEBUG}
  Assert(m_uParameterBlockByteSize > 0,
         'XAPO ASSERT: Procedure OnSetParameters, m_uParameterBlockByteSize <= 0');
  Assert(pParameters <> nil,
         'XAPO ASSERT: Procedure OnSetParameters, pParameters = nil');
  Assert(ParameterByteSize = m_uParameterBlockByteSize,
         'XAPO ASSERT: Procedure OnSetParameters, ParameterByteSize <> m_uParameterBlockByteSize');
{$ENDIF}

  // Do your implementations here


end;

//Implement Additional functions here.

end.
