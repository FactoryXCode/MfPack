// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - CoreAudio - DeviceTopology
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.CoreAudioApi.DeviceTopology.pas
// Kind: Pascal / Delphi unit
// Release date: 04-05-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: IAudioMediaType definition.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (TopPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
//------------------------------------------------------------------------------
//
// Remarks: Pay close attention for supported platforms (ie Vista or Win 7/8/8.1/10).
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
// 
//          The interface and type definitions for base APO functionality.
//          Requires Windows Vista or later.
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
// Source: devicetopology.h
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
unit WinApi.CoreAudioApi.DeviceTopology;

  {$HPPEMIT '#include "devicetopology.h"'}
  {$HPPEMIT '#include "ks.h"'}
  {$HPPEMIT '#include "ksmedia.h"'}

interface

uses
  {WinApi}
  WinApi.WinApiTypes,
  WinApi.Ks;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


const

	// Flag for clients of IControlChangeNotify.OnNotify to allow those clients to
	// identify hardware initiated notifications.
	DEVTOPO_HARDWARE_INITIATED_EVENTCONTEXT = ' draH ';
	{$EXTERNALSYM DEVTOPO_HARDWARE_INITIATED_EVENTCONTEXT}
	EVENTCONTEXT_VOLUMESLIDER 			: TGUID = '{E2C2E9DE-09B1-4B04-84E5-07931225EE04}';
	{$EXTERNALSYM EVENTCONTEXT_VOLUMESLIDER}


// Enum Structs
type

	PKSDATAFORMAT = ^KSDATAFORMAT;
	KSDATAFORMAT = record
		FormatSize: ULONG;
		Flags: ULONG;
		SampleSize: ULONG;
    Reserved: ULONG;
		MajorFormat: TGUID;
		SubFormat: TGUID;
		Specifier: TGUID;
  end;
	{$EXTERNALSYM KSDATAFORMAT}


// Is also defined in directShow9.pas
{$IFDEF DEFINE_KSIDENTIFIER}
  __MIDL___MIDL_itf_devicetopology_0000_0000_0001 = record
		case Integer of
      0: (Set_  : TGUID;
          Id    : ULONG;
          Flags : ULONG);
      1: (Alignment : int64);
	end;
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0001}
  KSIDENTIFIER = __MIDL___MIDL_itf_devicetopology_0000_0000_0001
  {$EXTERNALSYM KSIDENTIFIER}
	PKSIDENTIFIER = ^KSIDENTIFIER;
	KSPROPERTY = KSIDENTIFIER;
	{$EXTERNALSYM KSPROPERTY}
	PKSPROPERTY = ^KSPROPERTY;
	KSMETHOD = KSIDENTIFIER;
	{$EXTERNALSYM KSMETHOD}
	PKSMETHOD = ^KSMETHOD;
	KSEVENT = KSIDENTIFIER;
	{$EXTERNALSYM KSEVENT}
	PKSEVENT = ^KSEVENT;
{$ENDIF}

  PEPcxConnectionType = ^TEPcxConnectionType;
  __MIDL___MIDL_itf_devicetopology_0000_0000_0005 = (
    eConnTypeUnknown               = 0,
    eConnType3Point5mm             = (eConnTypeUnknown + 1),
    eConnTypeQuarter               = (eConnType3Point5mm + 1),
    eConnTypeAtapiInternal         = (eConnTypeQuarter + 1),
    eConnTypeRCA                   = (eConnTypeAtapiInternal + 1),
    eConnTypeOptical               = (eConnTypeRCA + 1),
    eConnTypeOtherDigital          = (eConnTypeOptical + 1),
    eConnTypeOtherAnalog           = (eConnTypeOtherDigital + 1),
    eConnTypeMultichannelAnalogDIN = (eConnTypeOtherAnalog + 1),
    eConnTypeXlrProfessional       = (eConnTypeMultichannelAnalogDIN + 1),
    eConnTypeRJ11Modem             = (eConnTypeXlrProfessional + 1),
    eConnTypeCombination           = (eConnTypeRJ11Modem + 1)
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0005}
  EPcxConnectionType = __MIDL___MIDL_itf_devicetopology_0000_0000_0005;
  {$EXTERNALSYM EPcxConnectionType}
  TEPcxConnectionType = __MIDL___MIDL_itf_devicetopology_0000_0000_0005;
  {$NODEFINE _tagEPcxConnectionType}
  _tagEPcxConnectionType = __MIDL___MIDL_itf_devicetopology_0000_0000_0005; // For compatibility with earlier MfPack versions


  PEPcxGeoLocation = ^TEPcxGeoLocation;
  __MIDL___MIDL_itf_devicetopology_0000_0000_0006 = (
    eGeoLocRear             = $1,
    eGeoLocFront            = (eGeoLocRear + 1),
    eGeoLocLeft             = (eGeoLocFront + 1),
    eGeoLocRight            = (eGeoLocLeft + 1),
    eGeoLocTop              = (eGeoLocRight + 1),
    eGeoLocBottom           = (eGeoLocTop + 1),
    eGeoLocRearPanel        = (eGeoLocBottom + 1),
    eGeoLocRiser            = (eGeoLocRearPanel + 1),
    eGeoLocInsideMobileLid  = (eGeoLocRiser + 1),
    eGeoLocDrivebay         = (eGeoLocInsideMobileLid + 1),
    eGeoLocHDMI             = (eGeoLocDrivebay + 1),
    eGeoLocOutsideMobileLid = (eGeoLocHDMI + 1),
    eGeoLocATAPI            = (eGeoLocOutsideMobileLid + 1),
    eGeoLocNotApplicable    = (eGeoLocATAPI + 1),
    eGeoLocReserved6        = (eGeoLocNotApplicable + 1)
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0006}
  EPcxGeoLocation = __MIDL___MIDL_itf_devicetopology_0000_0000_0006;
  {$EXTERNALSYM EPcxGeoLocation}
  TEPcxGeoLocation = __MIDL___MIDL_itf_devicetopology_0000_0000_0006;
  {$NODEFINE _tagEPcxGeoLocation}
  _tagEPcxGeoLocation = __MIDL___MIDL_itf_devicetopology_0000_0000_0006; // For compatibility with earlier MfPack versions

  PEPcxGenLocation = ^TEPcxGenLocation;
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0007}
  __MIDL___MIDL_itf_devicetopology_0000_0000_0007 = (
    eGenLocPrimaryBox = 0,
    eGenLocInternal   = (eGenLocPrimaryBox + 1),
    eGenLocSeparate   = (eGenLocInternal + 1),
    eGenLocOther      = (eGenLocSeparate + 1));
  EPcxGenLocation = __MIDL___MIDL_itf_devicetopology_0000_0000_0007;
  {$EXTERNALSYM EPcxGenLocation}
  TEPcxGenLocation = __MIDL___MIDL_itf_devicetopology_0000_0000_0007;
  {$NODEFINE _tagEPcxGenLocation}
  _tagEPcxGenLocation = __MIDL___MIDL_itf_devicetopology_0000_0000_0007; // For compatibility with earlier MfPack versions


  PEPxcPortConnection = ^TEPxcPortConnection;
  __MIDL___MIDL_itf_devicetopology_0000_0000_0008 = (
    ePortConnJack                  = 0,
    ePortConnIntegratedDevice      = (ePortConnJack + 1),
    ePortConnBothIntegratedAndJack = (ePortConnIntegratedDevice + 1),
    ePortConnUnknown               = (ePortConnBothIntegratedAndJack + 1)
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0008}
  EPxcPortConnection = __MIDL___MIDL_itf_devicetopology_0000_0000_0008;
  {$EXTERNALSYM EPxcPortConnection}
  TEPxcPortConnection = __MIDL___MIDL_itf_devicetopology_0000_0000_0008;
  {$NODEFINE _tagePxcPortConnection}
  _tagePxcPortConnection = __MIDL___MIDL_itf_devicetopology_0000_0000_0008; // For compatibility with earlier MfPack versions

  PKsjackDescription = ^TKsjackDescription;
  __MIDL___MIDL_itf_devicetopology_0000_0000_0009 = record
    ChannelMapping: DWORD;
    Color: COLORREF;  // use RGB() macro to generate these
    ConnectionType: EPcxConnectionType;
    GeoLocation: EPcxGeoLocation;
    GenLocation: EPcxGenLocation;
    PortConnection: EPxcPortConnection;
    IsConnected: BOOL;
  end;
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0009}
  KSJACK_DESCRIPTION = __MIDL___MIDL_itf_devicetopology_0000_0000_0009;
  {$EXTERNALSYM KSJACK_DESCRIPTION}
  TKsjackDescription = __MIDL___MIDL_itf_devicetopology_0000_0000_0009;


	PLuid = ^LUID;
	LUID = record
		LowPart: DWORD;
		HighPart: LONG;
	end;
	{$EXTERNALSYM LUID}


  PKsjackSinkConnectiontype = ^TKsjackSinkConnectiontype;
  __MIDL___MIDL_itf_devicetopology_0000_0000_0010 = (
    KSJACK_SINK_CONNECTIONTYPE_HDMI        = 0, // HDMI
    KSJACK_SINK_CONNECTIONTYPE_DISPLAYPORT = (KSJACK_SINK_CONNECTIONTYPE_HDMI + 1)
  ); // DisplayPort
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0010}
  KSJACK_SINK_CONNECTIONTYPE = __MIDL___MIDL_itf_devicetopology_0000_0000_0010;
  {$EXTERNALSYM KSJACK_SINK_CONNECTIONTYPE}
  TKsjackSinkConnectiontype = __MIDL___MIDL_itf_devicetopology_0000_0000_0010;
  {$NODEFINE _tagKSJACK_SINK_CONNECTIONTYPE}
  _tagKSJACK_SINK_CONNECTIONTYPE = __MIDL___MIDL_itf_devicetopology_0000_0000_0010; // For compatibility with earlier MfPack versions



  PKSJACK_SINK_INFORMATION = ^KSJACK_SINK_INFORMATION;
	_tagKSJACK_SINK_INFORMATION = record
		ConnType: KSJACK_SINK_CONNECTIONTYPE;   // Connection Type
		ManufacturerId: WORD;                   // Sink manufacturer ID
		ProductId: WORD;                        // Sink product ID
		AudioLatency: WORD;                     // Sink audio latency
    HDCPCapable: BOOL;                      // HDCP Support
    AICapable: BOOL;                        // ACP Packet, ISRC1, and ISRC2 Support
    SinkDescriptionLength: UCHAR;           // Monitor/Sink name length
		SinkDescription: array[0..31] of WideChar;  // Monitor/Sink name
		PortId: LUID;                           // Video port identifier
  end;
	{$EXTERNALSYM _tagKSJACK_SINK_INFORMATION}
	KSJACK_SINK_INFORMATION = _tagKSJACK_SINK_INFORMATION;
	{$EXTERNALSYM KSJACK_SINK_INFORMATION}


  // The KSJACK_DESCRIPTION2 structure describes an audio jack.
  // To get the description of an audio jack of a connector,
  // call IKsJackDescription2.GetJackDescription2.
  //
  // NOTE: This structure is also defined in MfPack.KsMedia.pas (ksmedia.h)!
  //
  PKSJACK_DESCRIPTION2 = ^KSJACK_DESCRIPTION2;
	_tagKSJACK_DESCRIPTION2 = record
		DeviceStateInfo: DWORD;
		JackCapabilities: DWORD;
  end;
	{$EXTERNALSYM _tagKSJACK_DESCRIPTION2}
	KSJACK_DESCRIPTION2 = _tagKSJACK_DESCRIPTION2;
	{$EXTERNALSYM KSJACK_DESCRIPTION2}
  // Members
  //  DeviceStateInfo
  //    Reserved for future use.
  //  JackCapabilities
  //    Stores the audio jack's capabilities: jack presence detection capability or
  //    dynamic format changing capability.
  //    The constants that can be stored in this member of the structure are defined in
  //    MfPack.Ksmedia.pas as follows:
  //    - JACKDESC2_PRESENCE_DETECT_CAPABILITY ($00000001)
  //    - JACKDESC2_DYNAMIC_FORMAT_CHANGE_CAPABILITY ($00000002)

  PDataFlow = ^TDataFlow;
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0011}
  __MIDL___MIDL_itf_devicetopology_0000_0000_0011 = (
    _In  = 0,
    _Out = (_In + 1)
  );
  DataFlow = __MIDL___MIDL_itf_devicetopology_0000_0000_0011;
  {$EXTERNALSYM DataFlow}
  TDataFlow = __MIDL___MIDL_itf_devicetopology_0000_0000_0011;
  {$NODEFINE _tagDataFlow}
  _tagDataFlow = __MIDL___MIDL_itf_devicetopology_0000_0000_0011; // For compatibility with earlier MfPack versions


  PPartType = ^TPartType;
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0012}
  __MIDL___MIDL_itf_devicetopology_0000_0000_0012 = (
    Connector = 0,
    Subunit   = (Connector + 1));
  PartType = __MIDL___MIDL_itf_devicetopology_0000_0000_0012;
  {$EXTERNALSYM PartType}
  TPartType = __MIDL___MIDL_itf_devicetopology_0000_0000_0012;
  {$NODEFINE _tagPartType}
  _tagPartType = __MIDL___MIDL_itf_devicetopology_0000_0000_0012; // For compatibility with earlier MfPack versions


  PConnectorType = ^TConnectorType;
  __MIDL___MIDL_itf_devicetopology_0000_0000_0013 = (
    Unknown_Connector = 0,
    Physical_Internal = (Unknown_Connector + 1), // Tangible connector inside the device or PC. i.e. you have to open the case (of the PC or device) to see it
    Physical_External = (Physical_Internal + 1), // Tangible connector external to the device of PC, i.e. a jack
    Software_IO       = (Physical_External + 1), // Connector that you can send/receive data to/from
    Software_Fixed    = (Software_IO + 1),       // Connector that is for topology parsing only.  Is involved in a permanent connection to another Fixed connector.
    Network           = (Software_Fixed + 1)
  );
  {$EXTERNALSYM __MIDL___MIDL_itf_devicetopology_0000_0000_0013}
  ConnectorType = __MIDL___MIDL_itf_devicetopology_0000_0000_0013;
  {$EXTERNALSYM ConnectorType}
  TConnectorType = __MIDL___MIDL_itf_devicetopology_0000_0000_0013;
  {$NODEFINE _tagConnectorType}
  _tagConnectorType = __MIDL___MIDL_itf_devicetopology_0000_0000_0013; // For compatibility with earlier MfPack versions


type

	// Forward declarations of interfaces defined in the type library
	IPart = interface;
	IControlInterface = interface;
  IDeviceTopology = interface;
	IControlChangeNotify = interface;


  // Interfaces Type Library
  // =======================


  // Interface IKsControl
  // ====================
	// This interface is deprecated as all of DirectMusic.
	// Deprecated components are considered obsolete.
	// NOTE: See ks.h, ksproxy.h and DirectShow9.pas
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsControl);'}
	{$EXTERNALSYM IKsControl}
	IKsControl = interface(IUnknown)
	['{28F54685-06FD-11D2-B27A-00A0C9223196}']
	 function KsProperty(ksProperty: PKSPROPERTY;
                       PropertyLength: ULONG;
                       var PropertyData: Pointer;
                       DataLength: UINT;
                       out BytesReturned: ULONG): HRESULT; stdcall;

	 function KsMethod(Method: PKSMETHOD;
                     MethodLength: ULong;
                     var MethodData: Pointer;
                     DataLength: ULong;
                     out BytesReturned: ULong): HRESULT; stdcall;

	 function KsEvent({OPTIONAL}Event: PKSEVENT;
                    EventLength: ULONG;
                    var EventData: Pointer;
                    DataLength: ULONG;
                    out BytesReturned: ULONG): HRESULT; stdcall;
	end;
  IID_IKsControl = IKsControl;
  {$EXTERNALSYM IID_IKsControl}


  // Interface IPerChannelDbLevel
  // ============================
  // Represents a generic subunit control interface that provides per-channel control over the volume level,
	// in decibels, of an audio stream or of a frequency band in an audio stream.
	// A positive volume level represents gain, and a negative value represents attenuation.
	// Clients do not call the methods in this interface directly.
	// Instead, this interface serves as the base interface for the following interfaces, which clients do call directly:
	// IAudioBass Interface, IAudioMidrange Interface, IAudioTreble Interface, IAudioVolumeLevel Interface.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPerChannelDbLevel);'}
	{$EXTERNALSYM IPerChannelDbLevel}
	IPerChannelDbLevel = interface(IUnknown)
	['{85401FD4-6DE4-4b9d-9869-2D6753A82F3C}']
		function GetChannelCount(out pcChannels: UINT): HRESULT; stdcall;

		function GetLevel(nChannel: UINT;
                      out pfLevelDB: FLOAT): HRESULT; stdcall;

		function GetLevelRange(nChannel: UINT;
                           out pfMinLevelDB: FLOAT;
                           out pfMaxLevelDB: FLOAT;
                           out pfStepping: FLOAT): HRESULT; stdcall;

		function SetLevel(nChannel: UINT;
                      fLevelDB: FLOAT;
                      const pguidEventContext: TGUID): HRESULT; stdcall;

		function SetLevelAllChannels(aLevelsDB: PFLOAT;
                                 cChannels: ULONG;
                                 const pguidEventContext: TGUID): HRESULT; stdcall;
    // The SetLevelAllChannels method sets the volume levels, in decibels,
    // of all the channels in the audio stream.
    // Parameters
    //  aLevelsDB
    //  Array of volume levels.
    //  This parameter points to a caller-allocated float array into which the
    //  method writes the new volume levels, in decibels, for all the channels.
    //  cChannels
    //  The number of elements in the aLevelsDB array.
    //  If this parameter does not match the number of channels in the audio stream,
    //  the method fails without modifying the aLevelsDB array.
    //  pguidEventContext
    //  Context value for the IControlChangeNotify::OnNotify method.
    //  This parameter points to an event-context GUID.


		function SetLevelUniform(fLevelDB: FLOAT;
                             const pguidEventContext: TGUID): HRESULT; stdcall;

	end;
  IID_IPerChannelDbLevel = IPerChannelDbLevel;
  {$EXTERNALSYM IID_IPerChannelDbLevel}


  // Interface IAudioVolumeLevel
  // ===========================
	// Inherited members from of IPerChannelDbLevel.
	// Those will only work, if the HARDWARE supports it!
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioVolumeLevel);'}
  {$EXTERNALSYM IAudioVolumeLevel}
  IAudioVolumeLevel = interface(IPerChannelDbLevel)
	['{7FB7B48F-531D-44A2-BCB3-5AD5A134B3DC}']

	end;
  IID_IAudioVolumeLevel = IAudioVolumeLevel;
  {$EXTERNALSYM IID_IAudioVolumeLevel}


  // Interface IAudioChannelConfig
  // =============================
  // Provides access to a hardware channel-configuration control.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioChannelConfig);'}
	{$EXTERNALSYM IAudioChannelConfig}
	IAudioChannelConfig = interface(IUnknown)
	['{BB11C46F-EC28-493C-B88A-5DB88062CE98}']

		function GetChannelConfig(out pdwConfig: DWord): HRESULT; stdcall;

		function SetChannelConfig(dwConfig: DWord;
                              const pguidEventContext: TGUID): HRESULT; stdcall;

	end;
  IID_IAudioChannelConfig = IAudioChannelConfig;
  {$EXTERNALSYM IID_IAudioChannelConfig}


  // Interface IAudioLoudness
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioLoudness);'}
	{$EXTERNALSYM IAudioLoudness}
	IAudioLoudness = interface(IUnknown)
	['{7D8B1437-DD53-4350-9C1B-1EE2890BD938}']
		function GetEnabled(out pbEnabled: BOOL): HRESULT; stdcall;

		function SetEnabled(bEnable: BOOL;
                        const pguidEventContext: TGUID): HRESULT; stdcall;

	end;
  IID_IAudioLoudness = IAudioLoudness;
  {$EXTERNALSYM IID_IAudioLoudness}


  // Interface IAudioInputSelector
  // =============================
  //
  // Provides access to a hardware multiplexer control (input selector)
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioInputSelector);'}
	{$EXTERNALSYM IAudioInputSelector}
	IAudioInputSelector = interface(IUnknown)
	['{4F03DC02-5E6E-4653-8F72-A030C123D598}']
		function GetSelection(out pnIdSelected: UINT): HRESULT; stdcall;

		function SetSelection(nIdSelect: UINT;
                          const pguidEventContext: TGUID): HRESULT; stdcall;

	end;
  IID_IAudioInputSelector = IAudioInputSelector;
  {$EXTERNALSYM IID_IAudioInputSelector}


  // Interface IAudioOutputSelector
  // ==============================
  // Provides access to a hardware de-multiplexer control (output selector)
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioOutputSelector);'}
	{$EXTERNALSYM IAudioOutputSelector}
	IAudioOutputSelector = interface(IUnknown)
	['{82149A85-DBA6-4487-86BB-EA8F7FEFCC71}']
		function GetSelection(out pnIdSelected: UINT): HRESULT; stdcall;

		function SetSelection(nIdSelect: UINT;
                          const pguidEventContext: TGUID): HRESULT; stdcall;

	end;
  IID_IAudioOutputSelector = IAudioOutputSelector;
  {$EXTERNALSYM IID_IAudioOutputSelector}


  // Interface IAudioMute
  // ====================
	// Also implemented in MMDEvApi
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioMute);'}
	{$EXTERNALSYM IAudioMute}
	IAudioMute = interface(IUnknown)
	['{DF45AEEA-B74A-4B6B-AFAD-2366B6AA012E}']
		function SetMute(bMute: BOOL;
                     const pguidEventContext: TGUID): HRESULT; stdcall;

		function GetMute(out pbMute: BOOL): HRESULT; stdcall;

	end;
  IID_IAudioMute = IAudioMute;
  {$EXTERNALSYM IID_IAudioMute}


  // Interface IAudioBass
  // ====================
	// Inherited member from of IPerChannelDbLevel.
	// Those will only work, if the HARDWARE supports it!
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioBass);'}
	{$EXTERNALSYM IAudioBass}
	IAudioBass = interface(IPerChannelDbLevel)
	['{A2B1A1D9-4DB3-425D-A2B2-BD335CB3E2E5}']

	end;
  IID_IAudioBass = IAudioBass;
  {$EXTERNALSYM IID_IAudioBass}


  // Interface IAudioMidrange
  // ========================
  // Inherited member from of IPerChannelDbLevel.
	// Those will only work, if the HARDWARE supports it!
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioMidrange);'}
	{$EXTERNALSYM IAudioMidrange}
	IAudioMidrange = interface(IPerChannelDbLevel)
	['{5E54B6D7-B44B-40D9-9A9E-E691D9CE6EDF}']

	end;
  IID_IAudioMidrange = IAudioMidrange;
  {$EXTERNALSYM IID_IAudioMidrange}


  // Interface IAudioTreble
  // ======================
  // Inherited member from of IPerChannelDbLevel.
	// Those will only work, if the HARDWARE supports it!
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(xxx);'}
	{$EXTERNALSYM IAudioTreble}
	IAudioTreble = interface(IPerChannelDbLevel)
	['{0A717812-694E-4907-B74B-BAFA5CFDCA7B}']

	end;
  IID_IAudioTreble = IAudioTreble;
  {$EXTERNALSYM IID_IAudioTreble}


  // Interface IAudioAutoGainControl
  // ===============================
  // Provides access to a hardware automatic gain control (AGC)
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioAutoGainControl);'}
	{$EXTERNALSYM IAudioAutoGainControl}
	IAudioAutoGainControl = interface(IUnknown)
	['{85401FD4-6DE4-4b9d-9869-2D6753A82F3C}']

		function GetEnabled(out pbEnabled: BOOL): HRESULT; stdcall;

		function SetEnabled(bEnable: BOOL;
                        const pguidEventContext: TGUID): HRESULT; stdcall;

	end;
  IID_IAudioAutoGainControl = IAudioAutoGainControl;
  {$EXTERNALSYM IID_IAudioAutoGainControl}


  // Interface IAudioPeakMeter
  // =========================
  // Provides access to a hardware peak-meter control.
	// When there is no hardware peakmeter, use: MMDevApi.IAudioMeterInformation,
  // that supports both hardware and software.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAudioPeakMeter);'}
	{$EXTERNALSYM IAudioPeakMeter}
	IAudioPeakMeter = interface(IUnknown)
	['{DD79923C-0599-45e0-B8B6-C8DF7DB6E796}']

		function GetChannelCount(out pcChannels: UINT): HRESULT; stdcall;

		function GetLevel(nChannel: UINT;
                      out pfLevel: FLOAT): HRESULT; stdcall;

	end;
  IID_IAudioPeakMeter = IAudioPeakMeter;
  {$EXTERNALSYM IID_IAudioPeakMeter}


  // Interface IDeviceSpecificProperty
  // =================================
  // Provides access to a vendor specific control.
  // Activatable on IPart interface on an KS Filter devicetopology object.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDeviceSpecificProperty);'}
	{$EXTERNALSYM IDeviceSpecificProperty}
	IDeviceSpecificProperty = interface(IUnknown)
	['{3B22BCBF-2586-4af0-8583-205D391B807C}']

    function GetType(out pVType: TVarType): HRESULT; stdcall;

    function GetValue(out pvValue: Pointer;
                      pcbValue: PDword): HRESULT; stdcall;

    function SetValue(pvValue: Pointer;
                      cbValue: Dword;
                      const pguidEventContext: LPCGUID): HRESULT; stdcall;

		function Get4BRange(out plMin: LONG;
                        out plMax: LONG;
                        out plStepping: LONG): HRESULT; stdcall;

	end;
  IID_IDeviceSpecificProperty = IDeviceSpecificProperty;
  {$EXTERNALSYM IID_IDeviceSpecificProperty}


  // Interface IKsFormatSupport
  // ==========================
	// Provides information about the audio data formats that are supported by a
  // software-configured I/O connection
	// (typically a DMA channel) between an audio adapter device and system memory.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsFormatSupport);'}
	{$EXTERNALSYM IKsFormatSupport}
	IKsFormatSupport = interface(IUnknown)
	['{3CB4A69D-BB6F-4D2B-95B7-452D2C155DB5}']

		function GetDevicePreferredFormat(out ppKsFormat: KSDATAFORMAT): HRESULT; stdcall;
    // The GetDevicePreferredFormat method gets the preferred audio stream format for the connection.
		// cbFormat = the size in bytes of the buffer that contains the format specifier.

		function IsFormatSupported(pKsFormat: KSDATAFORMAT;
                               cbFormat: DWord;
                               out pbSupported: BOOL): HRESULT; stdcall;
    // The IsFormatSupported method indicates whether the audio endpoint device supports the specified audio stream format.

	end;
  IID_IKsFormatSupport = IKsFormatSupport;
  {$EXTERNALSYM IID_IKsFormatSupport}


  // Interface IKsJackDescription
  // ============================
	// provides information about the jacks or internal connectors that provide a physical connection between
	// a device on an audio adapter and an external or internal endpoint device (for example, a microphone or CD player).
	// This interface is implemented < Windows 7!
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsJackDescription);'}
	{$EXTERNALSYM IKsJackDescription}
	IKsJackDescription = interface(IUnknown)
	['{82149A85-DBA6-4487-86BB-EA8F7FEFCC71}']

		function GetJackCount(out pcJacks: UINT): HRESULT; stdcall;

		function GetJackDescription(nJack: UINT;
                                out pDescription: KSJACK_DESCRIPTION): HRESULT; stdcall;

	end;
  IID_IKsJackDescription = IKsJackDescription;
  {$EXTERNALSYM IID_IKsJackDescription}


  // Interface IKsJackDescription2
  // =============================
	// provides information about the jacks or internal connectors that provide a physical connection between
	// a device on an audio adapter and an external or internal endpoint device (for example, a microphone or CD player).
	// This interface is implemented >= Windows 7!
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsJackDescription2);'}
	{$EXTERNALSYM IKsJackDescription2}
	IKsJackDescription2 = interface(IUnknown)
	['{478F3A9B-E0C9-4827-9228-6F5505FFE76A}']

		function GetJackCount(out pcJacks: UINT): HRESULT; stdcall;

		function GetJackDescription2(nJack: UINT;
                                 out pDescription2: KSJACK_DESCRIPTION2): HRESULT; stdcall;

	end;
  IID_IKsJackDescription2 = IKsJackDescription2;
  {$EXTERNALSYM IID_IKsJackDescription2}


  // Interface IKsJackSinkInformation
  // ================================
	// Provides access to jack sink information if the jack is supported by the hardware.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsJackSinkInformation);'}
	{$EXTERNALSYM IKsJackSinkInformation}
	IKsJackSinkInformation = interface(IUnknown)
	['{D9BD72ED-290F-4581-9FF3-61027A8FE532}']

		function GetJackSinkInformation(out pJackSinkInformation: KSJACK_SINK_INFORMATION): HRESULT; stdcall;

	end;
  IID_IKsJackSinkInformation = IKsJackSinkInformation;
  {$EXTERNALSYM IID_IKsJackSinkInformation}


  // Interface IKsJackContainerId
  // ============================
  // Provides access to jack container ID if supported by hardware
  // Activatable on:
  //    IPart interface (bridge pin connector) on an KS Filter devicetopology object
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKsJackContainerId);'}
  {$EXTERNALSYM IKsJackContainerId}
  IKsJackContainerId = interface(IUnknown)
	['{C99AF463-D629-4EC4-8C00-E54D68154248}']

    function GetJackContainerId(out pJackContainerId: TGUID): HResult; stdcall;
    // Description:
    //    Retrieves the container ID of the specified Jack.
    // Parameters:
    //    pJackContainerId - [out] The address of a buffer for receiving the container ID.
    // Return values:
    //    S_OK if successful.
  end;
  IID_IKsJackContainerId = IKsJackContainerId;
  {$EXTERNALSYM IID_IKsJackContainerId}


  // Interface IKsJackContainerId
  // ============================
  // Non-Activatable interface
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPartsList);'}
  {$EXTERNALSYM IPartsList}
  IPartsList = interface(IUnknown)
	['{6DAA848C-5EB0-45CC-AEA5-998A2CDA1FFB}']

		function GetCount(out pCount: UINT): HRESULT; stdcall;
    // Description:
    //  Gets the number of Parts in this PartsList
    // Parameters:
    //    pCount - [out] The number of Parts in this PartsList
    // Return values:
    //    S_OK if successful

		function GetPart(nIndex: UINT;
                     out ppPart: IPart): HRESULT; stdcall;
    // Description:
    //    Gets a Part at the specified index from the PartsList
    // Parameters:
    //    nIndex - [in] The index of the Part to retrieve, in the range [0, PartCount - 1]
    //    ppPart - [out] The part at the specified index.
    // Return values:
    //    S_OK if successful
	end;
  IID_IPartsList = IPartsList;
  {$EXTERNALSYM IID_IPartsList}


  // Interface IPart
  // ===============
	// Represents a part (connector or sub-unit) of a device topology.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPart);'}
	{$EXTERNALSYM IPart}
	IPart = interface(IUnknown)
	['{AE2DE0E4-5BCA-4F2D-AA46-5D13F8FDB3A9}']
		// The interface id for the requested control function.
    // The client should set this parameter to one of the following REFIID values:
		// IID_IAudioAutoGainControl, IID_IAudioBass, IID_IAudioChannelConfig, IID_IAudioInputSelector, IID_IAudioLoudness
		// IID_IAudioMidrange, IID_IAudioMute, IID_IAudioOutputSelector, IID_IAudioPeakMeter, IID_IAudioTreble, IID_IAudioVolumeLevel
		// IID_IDeviceSpecificProperty, IID_IKsFormatSupport, IID_IKsJackDescription

    function GetName(out ppwstrName: LPWSTR): HRESULT; stdcall;
    // Description:
    //    Gets the name of this Part.
    // Parameters:
    //    ppwstrName - [out] The name of this Part
    // Remarks:
    //    The returned string must be freed by the caller using CoTaskMemFree.
    // Return values:
    //    S_OK if successful
    //    E_NOTFOUND if the part does not have a name
    //

    function GetLocalId(out pnId: UINT): HRESULT; stdcall;
    // Description:
    //    Gets the local ID of this Part
    // Parameters:
    //    pnId - [out] The local ID of this Part. This ID is only valid in the context of the parent DeviceTopology object.
    // Return values:
    //    S_OK if successful
    //

    function GetGlobalId(out ppwstrGlobalId: LPWSTR): HRESULT; stdcall;
    // Description:
    //    Gets the global ID of this Part
    // Parameters:
    //    ppwstrId - [out] The global ID of this Part.
    //    This ID is globally unique and can be used to get an interface to a Part on
    //    any DeviceTopology object.
    // Remarks:
    //    The returned string must be freed by the caller using CoTaskMemFree.
    // Return values:
    //    S_OK if successful
    //

    function GetPartType(out ppPartType: PartType): HRESULT; stdcall;
    // Description:
    //    Gets the type of this Part (Connector or Subunit)
    // Parameters:
    //    ppPartType - [out] The type of this Part (Connector or Subunit)
    // Return values:
    //    S_OK if successful
    //

    function GetSubType(out pSubType: TGuid): HRESULT; stdcall;

    function GetControlInterfaceCount(out pCount: UINT): HRESULT; stdcall;

    function GetControlInterface(nIndex: UINT;
                                 out ppFunction: IControlInterface): HRESULT; stdcall;

    function EnumPartsIncoming(out ppParts: IPartsList): HRESULT; stdcall;

		function EnumPartsOutgoing(out ppParts: IPartsList): HRESULT; stdcall;

    function GetTopologyObject(out ppTopology: IDeviceTopology): HRESULT; stdcall;

    function Activate(dwClsContext: DWord;
                      const refiid: REFIID;
                      out ppvObject): HRESULT; stdcall;

		function UnregisterControlChangeCallback(pNotify: IControlChangeNotify): HRESULT; stdcall;

    function RegisterControlChangeCallback(const riid: REFGUID;
                                           pNotify: IControlChangeNotify): HRESULT; stdcall;

	end;
  IID_IPart = IPart;
  {$EXTERNALSYM IID_IPart}


  // Interface IConnector
  // ====================
	// Represents a point of connection between components.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IConnector);'}
	{$EXTERNALSYM IConnector}
	IConnector = interface(IUnknown)
	['{9c2c4058-23f5-41de-877a-df3af236a09e}']
    function GetType(out pType: ConnectorType): HRESULT; stdcall;

    function GetDataFlow(out pFlow: DataFlow): HRESULT; stdcall;

    function ConnectTo(pConnectTo: IConnector): HRESULT; stdcall;

    function Disconnect(): HRESULT; stdcall;

    function IsConnected(out pbConnected: BOOL): HRESULT; stdcall;

    function GetConnectedTo(out ppConTo: IConnector): HRESULT; stdcall;

    function GetConnectorIdConnectedTo(out ppwstrConnectorId: LPWSTR): HRESULT; stdcall;

    function IdConnectedTo(out ppwstrDeviceId: LPWSTR): HRESULT; stdcall;

	end;
  IID_IConnector = IConnector;
  {$EXTERNALSYM IID_IConnector}


  // Interface ISubUnit
  // ==================
  // The ISubunit interface inherits from the IUnknown interface but
  // does not have additional members.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISubUnit);'}
	{$EXTERNALSYM ISubUnit}
	ISubUnit = interface(IUnknown)
	['{82149A85-DBA6-4487-86BB-EA8F7FEFCC71}']

	end;
  IID_ISubUnit = ISubUnit;
  {$EXTERNALSYM IID_ISubUnit}


  // Interface IControlInterface
  // ===========================
  // Represents a control interface on a part (connector or subunit) in a device topology.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IControlInterface);'}
	{$EXTERNALSYM IControlInterface}
	IControlInterface = interface(IUnknown)
	['{45d37c3f-5140-444a-ae24-400789f3cbf3}']

		function GetIID(out pIID: TGuid): HRESULT; stdcall;
    // The GetIID method gets the interface ID of the function-specific control interface of the part.
		function GetName(out ppwstrName: LPWSTR): HRESULT; stdcall;
    // The GetName method gets the friendly name for the audio function that the control interface encapsulates.

	end;
  IID_IControlInterface = IControlInterface;
  {$EXTERNALSYM IID_IControlInterface}


	// Interface IControlChangeNotify
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IControlChangeNotify);'}
	{$EXTERNALSYM IControlChangeNotify}
	IControlChangeNotify = interface(IUnknown)
	['{A09513ED-C709-4d21-BD7B-5F34C47F3947}']

		function OnNotify(dwSenderProcessId: DWord;
                      pguidEventContext: LPCGUID = Nil): HRESULT; stdcall;

	end;
  IID_IControlChangeNotify = IControlChangeNotify;
  {$EXTERNALSYM IID_IControlChangeNotify}


  // Interface IDeviceTopology
  // =========================
  // The IDeviceTopology interface provides access to the topology of an audio device.
  // The topology of an audio adapter device consists of the data paths that lead to and
  // from audio endpoint devices and the control points that lie along the paths.
  // An audio endpoint device also has a topology, but it is trivial,
  // as explained in Device Topologies.
  // A client obtains a reference to the IDeviceTopology interface for an
  // audio endpoint device by following these steps:
  //   1 By using one of the techniques described in IMMDevice Interface,
  //     obtain a reference to the IMMDevice interface for an audio endpoint device.
  //   2 Call the IMMDevice.Activate method with parameter refiid set to REFIID IID_IDeviceTopology.
  //
  // After obtaining the IDeviceTopology interface for an audio endpoint device,
  // an application can explore the topologies of the audio adapter devices to which
  // the endpoint device is connected.
  //
  // For code examples that use the IDeviceTopology interface,
  // see the implementations of the GetHardwareDeviceTopology and
  // SelectCaptureDevice functions in DevTopoUtils.pas
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDeviceTopology);'}
	{$EXTERNALSYM IDeviceTopology}
	IDeviceTopology = interface(IUnknown)
	['{2A07407E-6497-4A18-9787-32F79BD0D98F}']

    function GetConnectorCount(out pCount: UINT): HRESULT; stdcall;

		function GetConnector(nIndex: UINT;
                          out ppConnector: IConnector): HRESULT; stdcall;

    function GetSubunitCount(out pCount: UINT): HRESULT; stdcall;

    function GetSubunit(nIndex: UINT;
                        out ppSubunit: ISubUnit): HRESULT; stdcall;

    function GetPartById(nId: UINT;
                         out ppPart: IPart): HRESULT; stdcall;

		function GetDeviceId(out ppwstrDeviceId: LPWSTR): HRESULT; stdcall;

		function GetSignalPath(pIPartFrom: IPart;
                           pIPartTo: IPart;
                           bRejectMixedPaths: BOOL;
                           out ppParts: IPartsList): HRESULT; stdcall;

	end;
  IID_IDeviceTopology = IDeviceTopology;
  {$EXTERNALSYM IID_IDeviceTopology}



  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
