// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MMReg.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Multimedia Registration
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
// Remarks: Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
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
// =============================================================================
// Source: mmreg.h
//
// Copyright (c) Microsoft Corporation. All rights reserved.
// =============================================================================
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
// =============================================================================
unit WinApi.WinMM.MMReg;

  {$HPPEMIT '#include "mmreg.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinMM.MMiscApi,
  WinApi.WinMM.MMeApi;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


// Define the following to skip definitions
//
// NOMMIDS      Multimedia IDs are not defined
// NONEWWAVE    No new waveform types are defined except WAVEFORMATEX
// NONEWRIFF    No new RIFF forms are defined
// NOJPEGDIB    No JPEG DIB definitions
// NONEWIC      No new Image Compressor types are defined
// NOBITMAP     No extended bitmap info header definition


const

  // use version number to verify compatibility
  _INC_MMREG = 158;      // version * 100 + revision
  {$EXTERNALSYM _INC_MMREG}

  // manufacturer IDs
  MM_MICROSOFT                  =  1;  //  Microsoft Corporation
  {$EXTERNALSYM MM_MICROSOFT}
  MM_CREATIVE                   =  2;  //  Creative Labs, Inc
  {$EXTERNALSYM MM_CREATIVE}
  MM_MEDIAVISION                =  3;  //  Media Vision, Inc.
  {$EXTERNALSYM MM_MEDIAVISION}
  MM_FUJITSU                    =  4;  //  Fujitsu Corp.
  {$EXTERNALSYM MM_FUJITSU}
  MM_PRAGMATRAX                 =  5;  // PRAGMATRAX Software
  {$EXTERNALSYM MM_PRAGMATRAX}
  MM_CYRIX                      =  6;  // Cyrix Corporation
  {$EXTERNALSYM MM_CYRIX}
  MM_PHILIPS_SPEECH_PROCESSING  =  7;  // Philips Speech Processing
  {$EXTERNALSYM MM_PHILIPS_SPEECH_PROCESSING}
  MM_NETXL                      =  8;  // NetXL, Inc.
  {$EXTERNALSYM MM_NETXL}
  MM_ZYXEL                      =  9;  // ZyXEL Communications, Inc.
  {$EXTERNALSYM MM_ZYXEL}
  MM_BECUBED                    = 10;  // BeCubed Software Inc.
  {$EXTERNALSYM MM_BECUBED}
  MM_AARDVARK                   = 11;  // Aardvark Computer Systems, Inc.
  {$EXTERNALSYM MM_AARDVARK}
  MM_BINTEC                     = 12;  // Bin Tec Communications GmbH
  {$EXTERNALSYM MM_BINTEC}
  MM_HEWLETT_PACKARD            = 13;  // Hewlett-Packard Company
  {$EXTERNALSYM MM_HEWLETT_PACKARD}
  MM_ACULAB                     = 14;  // Aculab plc
  {$EXTERNALSYM MM_ACULAB}
  MM_FAITH                      = 15;  // Faith,Inc.
  {$EXTERNALSYM MM_FAITH}
  MM_MITEL                      = 16;  // Mitel Corporation
  {$EXTERNALSYM MM_MITEL}
  MM_QUANTUM3D                  = 17;  // Quantum3D, Inc.
  {$EXTERNALSYM MM_QUANTUM3D}
  MM_SNI                        = 18;  // Siemens-Nixdorf
  {$EXTERNALSYM MM_SNI}
  MM_EMU                        = 19;  // E-mu Systems, Inc.
  {$EXTERNALSYM MM_EMU}
  MM_ARTISOFT                   = 20;  //  Artisoft, Inc.
  {$EXTERNALSYM MM_ARTISOFT}
  MM_TURTLE_BEACH               = 21;  //  Turtle Beach, Inc.
  {$EXTERNALSYM MM_TURTLE_BEACH}
  MM_IBM                        = 22;  //  IBM Corporation
  {$EXTERNALSYM MM_IBM}
  MM_VOCALTEC                   = 23;  //  Vocaltec LTD.
  {$EXTERNALSYM MM_VOCALTEC}
  MM_ROLAND                     = 24;  //  Roland
  {$EXTERNALSYM MM_ROLAND}
  MM_DSP_SOLUTIONS              = 25;  //  DSP Solutions, Inc.
  {$EXTERNALSYM MM_DSP_SOLUTIONS}
  MM_NEC                        = 26;  //  NEC
  {$EXTERNALSYM MM_NEC}
  MM_ATI                        = 27;  //  ATI
  {$EXTERNALSYM MM_ATI}
  MM_WANGLABS                   = 28;  //  Wang Laboratories, Inc
  {$EXTERNALSYM MM_WANGLABS}
  MM_TANDY                      = 29;  //  Tandy Corporation
  {$EXTERNALSYM MM_TANDY}
  MM_VOYETRA                    = 30;  //  Voyetra
  {$EXTERNALSYM MM_VOYETRA}
  MM_ANTEX                      = 31;  //  Antex Electronics Corporation
  {$EXTERNALSYM MM_ANTEX}
  MM_ICL_PS                     = 32;  //  ICL Personal Systems
  {$EXTERNALSYM MM_ICL_PS}
  MM_INTEL                      = 33;  //  Intel Corporation
  {$EXTERNALSYM MM_INTEL}
  MM_GRAVIS                     = 34;  //  Advanced Gravis
  {$EXTERNALSYM MM_GRAVIS}
  MM_VAL                        = 35;  //  Video Associates Labs, Inc.
  {$EXTERNALSYM MM_VAL}
  MM_INTERACTIVE                = 36;  //  InterActive Inc
  {$EXTERNALSYM MM_INTERACTIVE}
  MM_YAMAHA                     = 37;  //  Yamaha Corporation of America
  {$EXTERNALSYM MM_YAMAHA}
  MM_EVEREX                     = 38;  //  Everex Systems, Inc
  {$EXTERNALSYM MM_EVEREX}
  MM_ECHO                       = 39;  //  Echo Speech Corporation
  {$EXTERNALSYM MM_ECHO}
  MM_SIERRA                     = 40;  //  Sierra Semiconductor Corp
  {$EXTERNALSYM MM_SIERRA}
  MM_CAT                        = 41;  //  Computer Aided Technologies
  {$EXTERNALSYM MM_CAT}
  MM_APPS                       = 42;  //  APPS Software International
  {$EXTERNALSYM MM_APPS}
  MM_DSP_GROUP                  = 43;  //  DSP Group, Inc
  {$EXTERNALSYM MM_DSP_GROUP}
  MM_MELABS                     = 44;  //  microEngineering Labs
  {$EXTERNALSYM MM_MELABS}
  MM_COMPUTER_FRIENDS           = 45;  //  Computer Friends, Inc.
  {$EXTERNALSYM MM_COMPUTER_FRIENDS}
  MM_ESS                        = 46;  //  ESS Technology
  {$EXTERNALSYM MM_ESS}
  MM_AUDIOFILE                  = 47;  //  Audio, Inc.
  {$EXTERNALSYM MM_AUDIOFILE}
  MM_MOTOROLA                   = 48;  //  Motorola, Inc.
  {$EXTERNALSYM MM_MOTOROLA}
  MM_CANOPUS                    = 49;  //  Canopus, co., Ltd.
  {$EXTERNALSYM MM_CANOPUS}
  MM_EPSON                      = 50;  //  Seiko Epson Corporation
  {$EXTERNALSYM MM_EPSON}
  MM_TRUEVISION                 = 51;  //  Truevision
  {$EXTERNALSYM MM_TRUEVISION}
  MM_AZTECH                     = 52;  //  Aztech Labs, Inc.
  {$EXTERNALSYM MM_AZTECH}
  MM_VIDEOLOGIC                 = 53;  //  Videologic
  {$EXTERNALSYM MM_VIDEOLOGIC}
  MM_SCALACS                    = 54;  //  SCALACS
  {$EXTERNALSYM MM_SCALACS}
  MM_KORG                       = 55;  //  Toshihiko Okuhura, Korg Inc.
  {$EXTERNALSYM MM_KORG}
  MM_APT                        = 56;  //  Audio Processing Technology
  {$EXTERNALSYM MM_APT}
  MM_ICS                        = 57;  //  Integrated Circuit Systems, Inc.
  {$EXTERNALSYM MM_ICS}
  MM_ITERATEDSYS                = 58;  //  Iterated Systems, Inc.
  {$EXTERNALSYM MM_ITERATEDSYS}
  MM_METHEUS                    = 59;  //  Metheus
  {$EXTERNALSYM MM_METHEUS}
  MM_LOGITECH                   = 60;  //  Logitech, Inc.
  {$EXTERNALSYM MM_LOGITECH}
  MM_WINNOV                     = 61;  //  Winnov, Inc.
  {$EXTERNALSYM MM_WINNOV}
  MM_NCR                        = 62;  //  NCR Corporation
  {$EXTERNALSYM MM_NCR}
  MM_EXAN                       = 63;  //  EXAN
  {$EXTERNALSYM MM_EXAN}
  MM_AST                        = 64;  //  AST Research Inc.
  {$EXTERNALSYM MM_AST}
  MM_WILLOWPOND                 = 65;  //  Willow Pond Corporation
  {$EXTERNALSYM MM_WILLOWPOND}
  MM_SONICFOUNDRY               = 66;  //  Sonic Foundry
  {$EXTERNALSYM MM_SONICFOUNDRY}
  MM_VITEC                      = 67;  //  Vitec Multimedia
  {$EXTERNALSYM MM_VITEC}
  MM_MOSCOM                     = 68;  //  MOSCOM Corporation
  {$EXTERNALSYM MM_MOSCOM}
  MM_SILICONSOFT                = 69;  //  Silicon Soft, Inc.
  {$EXTERNALSYM MM_SILICONSOFT}
  MM_TERRATEC                   = 70;  //  TerraTec Electronic GmbH
  {$EXTERNALSYM MM_TERRATEC}
  MM_MEDIASONIC                 = 71;  //  MediaSonic Ltd.
  {$EXTERNALSYM MM_MEDIASONIC}
  MM_SANYO                      = 72;  //  SANYO Electric Co., Ltd.
  {$EXTERNALSYM MM_SANYO}
  MM_SUPERMAC                   = 73;  //  Supermac
  {$EXTERNALSYM MM_SUPERMAC}
  MM_AUDIOPT                    = 74;  //  Audio Processing Technology
  {$EXTERNALSYM MM_AUDIOPT}
  MM_NOGATECH                   = 75;  // NOGATECH Ltd.
  {$EXTERNALSYM MM_NOGATECH}
  MM_SPEECHCOMP                 = 76;  //  Speech Compression
  {$EXTERNALSYM MM_SPEECHCOMP}
  MM_DOLBY                      = 78;  //  Dolby Laboratories
  {$EXTERNALSYM MM_DOLBY}
  MM_OKI                        = 79;  //  OKI
  {$EXTERNALSYM MM_OKI}
  MM_AURAVISION                 = 80;  //  AuraVision Corporation
  {$EXTERNALSYM MM_AURAVISION}
  MM_OLIVETTI                   = 81;  //  Olivetti
  {$EXTERNALSYM MM_OLIVETTI}
  MM_IOMAGIC                    = 82;  //  I/O Magic Corporation
  {$EXTERNALSYM MM_IOMAGIC}
  MM_MATSUSHITA                 = 83;  //  Matsushita Electric Industrial Co., LTD.
  {$EXTERNALSYM MM_MATSUSHITA}
  MM_CONTROLRES                 = 84;  //  Control Resources Limited
  {$EXTERNALSYM MM_CONTROLRES}
  MM_XEBEC                      = 85;  //  Xebec Multimedia Solutions Limited
  {$EXTERNALSYM MM_XEBEC}
  MM_NEWMEDIA                   = 86;  //  New Media Corporation
  {$EXTERNALSYM MM_NEWMEDIA}
  MM_NMS                        = 87;  //  Natural MicroSystems
  {$EXTERNALSYM MM_NMS}
  MM_LYRRUS                     = 88;  //  Lyrrus Inc.
  {$EXTERNALSYM MM_LYRRUS}
  MM_COMPUSIC                   = 89;  //  Compusic
  {$EXTERNALSYM MM_COMPUSIC}
  MM_OPTI                       = 90;  //  OPTi Computers Inc.
  {$EXTERNALSYM MM_OPTI}
  MM_DIALOGIC                   = 93;  //  Dialogic Corporation
  {$EXTERNALSYM MM_DIALOGIC}
  MM_INSOFT                     = 94;  //  InSoft, Inc.
  {$EXTERNALSYM MM_INSOFT}
  MM_MPTUS                      = 95;  //  M.P. Technologies, Inc.
  {$EXTERNALSYM MM_MPTUS}
  MM_WEITEK                     = 96;  //  Weitek
  {$EXTERNALSYM MM_WEITEK}
  MM_LERNOUT_AND_HAUSPIE        = 97;  //  Lernout & Hauspie
  {$EXTERNALSYM MM_LERNOUT_AND_HAUSPIE}
  MM_QCIAR                      = 98;  //  Quanta Computer Inc.
  {$EXTERNALSYM MM_QCIAR}
  MM_APPLE                      = 99;  //  Apple Computer, Inc.
  {$EXTERNALSYM MM_APPLE}
  MM_DIGITAL                    = 100; //  Digital Equipment Corporation
  {$EXTERNALSYM MM_DIGITAL}
  MM_MOTU                       = 101; //  Mark of the Unicorn
  {$EXTERNALSYM MM_MOTU}
  MM_WORKBIT                    = 102; //  Workbit Corporation
  {$EXTERNALSYM MM_WORKBIT}
  MM_OSITECH                    = 103; //  Ositech Communications Inc.
  {$EXTERNALSYM MM_OSITECH}
  MM_MIRO                       = 104; //  miro Computer Products AG
  {$EXTERNALSYM MM_MIRO}
  MM_CIRRUSLOGIC                = 105; //  Cirrus Logic
  {$EXTERNALSYM MM_CIRRUSLOGIC}
  MM_ISOLUTION                  = 106; //  ISOLUTION  B.V.
  {$EXTERNALSYM MM_ISOLUTION}
  MM_HORIZONS                   = 107; //  Horizons Technology, Inc.
  {$EXTERNALSYM MM_HORIZONS}
  MM_CONCEPTS                   = 108; //  Computer Concepts Ltd.
  {$EXTERNALSYM MM_CONCEPTS}
  MM_VTG                        = 109; //  Voice Technologies Group, Inc.
  {$EXTERNALSYM MM_VTG}
  MM_RADIUS                     = 110; //  Radius
  {$EXTERNALSYM MM_RADIUS}
  MM_ROCKWELL                   = 111; //  Rockwell International
  {$EXTERNALSYM MM_ROCKWELL}
  MM_XYZ                        = 112; //  Co. XYZ for testing
  {$EXTERNALSYM MM_XYZ}
  MM_OPCODE                     = 113; //  Opcode Systems
  {$EXTERNALSYM MM_OPCODE}
  MM_VOXWARE                    = 114; //  Voxware Inc.
  {$EXTERNALSYM MM_VOXWARE}
  MM_NORTHERN_TELECOM           = 115; //  Northern Telecom Limited
  {$EXTERNALSYM MM_NORTHERN_TELECOM}
  MM_APICOM                     = 116; //  APICOM
  {$EXTERNALSYM MM_APICOM}
  MM_GRANDE                     = 117; //  Grande Software
  {$EXTERNALSYM MM_GRANDE}
  MM_ADDX                       = 118; //  ADDX
  {$EXTERNALSYM MM_ADDX}
  MM_WILDCAT                    = 119; //  Wildcat Canyon Software
  {$EXTERNALSYM MM_WILDCAT}
  MM_RHETOREX                   = 120; //  Rhetorex Inc.
  {$EXTERNALSYM MM_RHETOREX}
  MM_BROOKTREE                  = 121; //  Brooktree Corporation
  {$EXTERNALSYM MM_BROOKTREE}
  MM_ENSONIQ                    = 125; //  ENSONIQ Corporation
  {$EXTERNALSYM MM_ENSONIQ}
  MM_FAST                       = 126; //  FAST Multimedia AG
  {$EXTERNALSYM MM_FAST}
  MM_NVIDIA                     = 127; //  NVidia Corporation
  {$EXTERNALSYM MM_NVIDIA}
  MM_OKSORI                     = 128; //  OKSORI Co., Ltd.
  {$EXTERNALSYM MM_OKSORI}
  MM_DIACOUSTICS                = 129; //  DiAcoustics, Inc.
  {$EXTERNALSYM MM_DIACOUSTICS}
  MM_GULBRANSEN                 = 130; //  Gulbransen, Inc.
  {$EXTERNALSYM MM_GULBRANSEN}
  MM_KAY_ELEMETRICS             = 131; //  Kay Elemetrics, Inc.
  {$EXTERNALSYM MM_KAY_ELEMETRICS}
  MM_CRYSTAL                    = 132; //  Crystal Semiconductor Corporation
  {$EXTERNALSYM MM_CRYSTAL}
  MM_SPLASH_STUDIOS             = 133; //  Splash Studios
  {$EXTERNALSYM MM_SPLASH_STUDIOS}
  MM_QUARTERDECK                = 134; //  Quarterdeck Corporation
  {$EXTERNALSYM MM_QUARTERDECK}
  MM_TDK                        = 135; //  TDK Corporation
  {$EXTERNALSYM MM_TDK}
  MM_DIGITAL_AUDIO_LABS         = 136; //  Digital Audio Labs, Inc.
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS}
  MM_SEERSYS                    = 137; //  Seer Systems, Inc.
  {$EXTERNALSYM MM_SEERSYS}
  MM_PICTURETEL                 = 138; //  PictureTel Corporation
  {$EXTERNALSYM MM_PICTURETEL}
  MM_ATT_MICROELECTRONICS       = 139; //  AT&T Microelectronics
  {$EXTERNALSYM MM_ATT_MICROELECTRONICS}
  MM_OSPREY                     = 140; //  Osprey Technologies, Inc.
  {$EXTERNALSYM MM_OSPREY}
  MM_MEDIATRIX                  = 141; //  Mediatrix Peripherals
  {$EXTERNALSYM MM_MEDIATRIX}
  MM_SOUNDESIGNS                = 142; //  SounDesignS M.C.S. Ltd.
  {$EXTERNALSYM MM_SOUNDESIGNS}
  MM_ALDIGITAL                  = 143; //  A.L. Digital Ltd.
  {$EXTERNALSYM MM_ALDIGITAL}
  MM_SPECTRUM_SIGNAL_PROCESSING = 144; //  Spectrum Signal Processing, Inc.
  {$EXTERNALSYM MM_SPECTRUM_SIGNAL_PROCESSING}
  MM_ECS                        = 145; //  Electronic Courseware Systems, Inc.
  {$EXTERNALSYM MM_ECS}
  MM_AMD                        = 146; //  AMD
  {$EXTERNALSYM MM_AMD}
  MM_COREDYNAMICS               = 147; //  Core Dynamics
  {$EXTERNALSYM MM_COREDYNAMICS}
  MM_CANAM                      = 148; //  CANAM Computers
  {$EXTERNALSYM MM_CANAM}
  MM_SOFTSOUND                  = 149; //  Softsound, Ltd.
  {$EXTERNALSYM MM_SOFTSOUND}
  MM_NORRIS                     = 150; //  Norris Communications, Inc.
  {$EXTERNALSYM MM_NORRIS}
  MM_DDD                        = 151; //  Danka Data Devices
  {$EXTERNALSYM MM_DDD}
  MM_EUPHONICS                  = 152; //  EuPhonics
  {$EXTERNALSYM MM_EUPHONICS}
  MM_PRECEPT                    = 153; //  Precept Software, Inc.
  {$EXTERNALSYM MM_PRECEPT}
  MM_CRYSTAL_NET                = 154; //  Crystal Net Corporation
  {$EXTERNALSYM MM_CRYSTAL_NET}
  MM_CHROMATIC                  = 155; //  Chromatic Research, Inc.
  {$EXTERNALSYM MM_CHROMATIC}
  MM_VOICEINFO                  = 156; //  Voice Information Systems, Inc.
  {$EXTERNALSYM MM_VOICEINFO}
  MM_VIENNASYS                  = 157; //  Vienna Systems
  {$EXTERNALSYM MM_VIENNASYS}
  MM_CONNECTIX                  = 158; //  Connectix Corporation
  {$EXTERNALSYM MM_CONNECTIX}
  MM_GADGETLABS                 = 159; //  Gadget Labs LLC
  {$EXTERNALSYM MM_GADGETLABS}
  MM_FRONTIER                   = 160; //  Frontier Design Group LLC
  {$EXTERNALSYM MM_FRONTIER}
  MM_VIONA                      = 161; //  Viona Development GmbH
  {$EXTERNALSYM MM_VIONA}
  MM_CASIO                      = 162; //  Casio Computer Co., LTD
  {$EXTERNALSYM MM_CASIO}
  MM_DIAMONDMM                  = 163; //  Diamond Multimedia
  {$EXTERNALSYM MM_DIAMONDMM}
  MM_S3                         = 164; //  S3
  {$EXTERNALSYM MM_S3}
  MM_DVISION                    = 165; //  D-Vision Systems, Inc.
  {$EXTERNALSYM MM_DVISION}
  MM_NETSCAPE                   = 166; //  Netscape Communications
  {$EXTERNALSYM MM_NETSCAPE}
  MM_SOUNDSPACE                 = 167; //  Soundspace Audio
  {$EXTERNALSYM MM_SOUNDSPACE}
  MM_VANKOEVERING               = 168; //  VanKoevering Company
  {$EXTERNALSYM MM_VANKOEVERING}
  MM_QTEAM                      = 169; //  Q-Team
  {$EXTERNALSYM MM_QTEAM}
  MM_ZEFIRO                     = 170; //  Zefiro Acoustics
  {$EXTERNALSYM MM_ZEFIRO}
  MM_STUDER                     = 171; //  Studer Professional Audio AG
  {$EXTERNALSYM MM_STUDER}
  MM_FRAUNHOFER_IIS             = 172; //  Fraunhofer IIS
  {$EXTERNALSYM MM_FRAUNHOFER_IIS}
  MM_QUICKNET                   = 173; //  Quicknet Technologies
  {$EXTERNALSYM MM_QUICKNET}
  MM_ALARIS                     = 174; //  Alaris, Inc.
  {$EXTERNALSYM MM_ALARIS}
  MM_SICRESOURCE                = 175; //  SIC Resource Inc.
  {$EXTERNALSYM MM_SICRESOURCE}
  MM_NEOMAGIC                   = 176; //  NeoMagic Corporation
  {$EXTERNALSYM MM_NEOMAGIC}
  MM_MERGING_TECHNOLOGIES       = 177; //  Merging Technologies S.A.
  {$EXTERNALSYM MM_MERGING_TECHNOLOGIES}
  MM_XIRLINK                    = 178; //  Xirlink, Inc.
  {$EXTERNALSYM MM_XIRLINK}
  MM_COLORGRAPH                 = 179; //  Colorgraph (UK) Ltd
  {$EXTERNALSYM MM_COLORGRAPH}
  MM_OTI                        = 180; //  Oak Technology, Inc.
  {$EXTERNALSYM MM_OTI}
  MM_AUREAL                     = 181; //  Aureal Semiconductor
  {$EXTERNALSYM MM_AUREAL}
  MM_VIVO                       = 182; //  Vivo Software
  {$EXTERNALSYM MM_VIVO}
  MM_SHARP                      = 183; //  Sharp
  {$EXTERNALSYM MM_SHARP}
  MM_LUCENT                     = 184; //  Lucent Technologies
  {$EXTERNALSYM MM_LUCENT}
  MM_ATT                        = 185; //  AT&T Labs, Inc.
  {$EXTERNALSYM MM_ATT}
  MM_SUNCOM                     = 186; //  Sun Communications, Inc.
  {$EXTERNALSYM MM_SUNCOM}
  MM_SORVIS                     = 187; //  Sorenson Vision
  {$EXTERNALSYM MM_SORVIS}
  MM_INVISION                   = 188; //  InVision Interactive
  {$EXTERNALSYM MM_INVISION}
  MM_BERKOM                     = 189; //  Deutsche Telekom Berkom GmbH
  {$EXTERNALSYM MM_BERKOM}
  MM_MARIAN                     = 190; //  Marian GbR Leipzig
  {$EXTERNALSYM MM_MARIAN}
  MM_DPSINC                     = 191; //  Digital Processing Systems, Inc.
  {$EXTERNALSYM MM_DPSINC}
  MM_BCB                        = 192; //  BCB Holdings Inc.
  {$EXTERNALSYM MM_BCB}
  MM_MOTIONPIXELS               = 193; //  Motion Pixels
  {$EXTERNALSYM MM_MOTIONPIXELS}
  MM_QDESIGN                    = 194; //  QDesign Corporation
  {$EXTERNALSYM MM_QDESIGN}
  MM_NMP                        = 195; //  Nokia Mobile Phones
  {$EXTERNALSYM MM_NMP}
  MM_DATAFUSION                 = 196; //  DataFusion Systems (Pty) (Ltd)
  {$EXTERNALSYM MM_DATAFUSION}
  MM_DUCK                       = 197; //  The Duck Corporation
  {$EXTERNALSYM MM_DUCK}
  MM_FTR                        = 198; //  Future Technology Resources Pty Ltd
  {$EXTERNALSYM MM_FTR}
  MM_BERCOS                     = 199; //  BERCOS GmbH
  {$EXTERNALSYM MM_BERCOS}
  MM_ONLIVE                     = 200; //  OnLive! Technologies, Inc.
  {$EXTERNALSYM MM_ONLIVE}
  MM_SIEMENS_SBC                = 201; //  Siemens Business Communications Systems
  {$EXTERNALSYM MM_SIEMENS_SBC}
  MM_TERALOGIC                  = 202; //  TeraLogic, Inc.
  {$EXTERNALSYM MM_TERALOGIC}
  MM_PHONET                     = 203; //  PhoNet Communications Ltd.
  {$EXTERNALSYM MM_PHONET}
  MM_WINBOND                    = 204; //  Winbond Electronics Corp
  {$EXTERNALSYM MM_WINBOND}
  MM_VIRTUALMUSIC               = 205; //  Virtual Music, Inc.
  {$EXTERNALSYM MM_VIRTUALMUSIC}
  MM_ENET                       = 206; //  e-Net, Inc.
  {$EXTERNALSYM MM_ENET}
  MM_GUILLEMOT                  = 207; //  Guillemot International
  {$EXTERNALSYM MM_GUILLEMOT}
  MM_EMAGIC                     = 208; //  Emagic Soft- und Hardware GmbH
  {$EXTERNALSYM MM_EMAGIC}
  MM_MWM                        = 209; //  MWM Acoustics LLC
  {$EXTERNALSYM MM_MWM}
  MM_PACIFICRESEARCH            = 210; //  Pacific Research and Engineering Corporation
  {$EXTERNALSYM MM_PACIFICRESEARCH}
  MM_SIPROLAB                   = 211; //  Sipro Lab Telecom Inc.
  {$EXTERNALSYM MM_SIPROLAB}
  MM_LYNX                       = 212; //  Lynx Studio Technology, Inc.
  {$EXTERNALSYM MM_LYNX}
  MM_SPECTRUM_PRODUCTIONS       = 213; //  Spectrum Productions
  {$EXTERNALSYM MM_SPECTRUM_PRODUCTIONS}
  MM_DICTAPHONE                 = 214; //  Dictaphone Corporation
  {$EXTERNALSYM MM_DICTAPHONE}
  MM_QUALCOMM                   = 215; //  QUALCOMM, Inc.
  {$EXTERNALSYM MM_QUALCOMM}
  MM_RZS                        = 216; //  Ring Zero Systems, Inc
  {$EXTERNALSYM MM_RZS}
  MM_AUDIOSCIENCE               = 217; //  AudioScience Inc.
  {$EXTERNALSYM MM_AUDIOSCIENCE}
  MM_PINNACLE                   = 218; //  Pinnacle Systems, Inc.
  {$EXTERNALSYM MM_PINNACLE}
  MM_EES                        = 219; //  EES Technik für Musik GmbH
  {$EXTERNALSYM MM_EES}
  MM_HAFTMANN                   = 220; //  haftmann#software
  {$EXTERNALSYM MM_HAFTMANN}
  MM_LUCID                      = 221; //  Lucid Technology, Symetrix Inc.
  {$EXTERNALSYM MM_LUCID}
  MM_HEADSPACE                  = 222; //  Headspace, Inc
  {$EXTERNALSYM MM_HEADSPACE}
  MM_UNISYS                     = 223; //  UNISYS CORPORATION
  {$EXTERNALSYM MM_UNISYS}
  MM_LUMINOSITI                 = 224; //  Luminositi, Inc.
  {$EXTERNALSYM MM_LUMINOSITI}
  MM_ACTIVEVOICE                = 225; //  ACTIVE VOICE CORPORATION
  {$EXTERNALSYM MM_ACTIVEVOICE}
  MM_DTS                        = 226; //  Digital Theater Systems, Inc.
  {$EXTERNALSYM MM_DTS}
  MM_DIGIGRAM                   = 227; //  DIGIGRAM
  {$EXTERNALSYM MM_DIGIGRAM}
  MM_SOFTLAB_NSK                = 228; //  Softlab-Nsk
  {$EXTERNALSYM MM_SOFTLAB_NSK}
  MM_FORTEMEDIA                 = 229; //  ForteMedia, Inc
  {$EXTERNALSYM MM_FORTEMEDIA}
  MM_SONORUS                    = 230; //  Sonorus, Inc.
  {$EXTERNALSYM MM_SONORUS}
  MM_ARRAY                      = 231; //  Array Microsystems, Inc.
  {$EXTERNALSYM MM_ARRAY}
  MM_DATARAN                    = 232; //  Data Translation, Inc.
  {$EXTERNALSYM MM_DATARAN}
  MM_I_LINK                     = 233; //  I-link Worldwide
  {$EXTERNALSYM MM_I_LINK}
  MM_SELSIUS_SYSTEMS            = 234; //  Selsius Systems Inc.
  {$EXTERNALSYM MM_SELSIUS_SYSTEMS}
  MM_ADMOS                      = 235; //  AdMOS Technology, Inc.
  {$EXTERNALSYM MM_ADMOS}
  MM_LEXICON                    = 236; //  Lexicon Inc.
  {$EXTERNALSYM MM_LEXICON}
  MM_SGI                        = 237; //  Silicon Graphics Inc.
  {$EXTERNALSYM MM_SGI}
  MM_IPI                        = 238; //  Interactive Product Inc.
  {$EXTERNALSYM MM_IPI}
  MM_ICE                        = 239; //  IC Ensemble, Inc.
  {$EXTERNALSYM MM_ICE}
  MM_VQST                       = 240; //  ViewQuest Technologies Inc.
  {$EXTERNALSYM MM_VQST}
  MM_ETEK                       = 241; //  eTEK Labs Inc.
  {$EXTERNALSYM MM_ETEK}
  MM_CS                         = 242; //  Consistent Software
  {$EXTERNALSYM MM_CS}
  MM_ALESIS                     = 243; //  Alesis Studio Electronics
  {$EXTERNALSYM MM_ALESIS}
  MM_INTERNET                   = 244; //  INTERNET Corporation
  {$EXTERNALSYM MM_INTERNET}
  MM_SONY                       = 245; //  Sony Corporation
  {$EXTERNALSYM MM_SONY}
  MM_HYPERACTIVE                = 246; //  Hyperactive Audio Systems, Inc.
  {$EXTERNALSYM MM_HYPERACTIVE}
  MM_UHER_INFORMATIC            = 247; //  UHER informatic GmbH
  {$EXTERNALSYM MM_UHER_INFORMATIC}
  MM_SYDEC_NV                   = 248; //  Sydec NV
  {$EXTERNALSYM MM_SYDEC_NV}
  MM_FLEXION                    = 249; //  Flexion Systems Ltd.
  {$EXTERNALSYM MM_FLEXION}
  MM_VIA                        = 250; //  Via Technologies, Inc.
  {$EXTERNALSYM MM_VIA}
  MM_MICRONAS                   = 251; //  Micronas Semiconductors, Inc.
  {$EXTERNALSYM MM_MICRONAS}
  MM_ANALOGDEVICES              = 252; //  Analog Devices, Inc.
  {$EXTERNALSYM MM_ANALOGDEVICES}
  MM_HP                         = 253; //  Hewlett Packard Company
  {$EXTERNALSYM MM_HP}
  MM_MATROX_DIV                 = 254; //  Matrox
  {$EXTERNALSYM MM_MATROX_DIV}
  MM_QUICKAUDIO                 = 255; //  Quick Audio, GbR
  {$EXTERNALSYM MM_QUICKAUDIO}
  MM_YOUCOM                     = 256; //  You/Com Audiocommunicatie BV
  {$EXTERNALSYM MM_YOUCOM}
  MM_RICHMOND                   = 257; //  Richmond Sound Design Ltd.
  {$EXTERNALSYM MM_RICHMOND}
  MM_IODD                       = 258; //  I-O Data Device, Inc.
  {$EXTERNALSYM MM_IODD}
  MM_ICCC                       = 259; //  ICCC A/S
  {$EXTERNALSYM MM_ICCC}
  MM_3COM                       = 260; //  3COM Corporation
  {$EXTERNALSYM MM_3COM}
  MM_MALDEN                     = 261; //  Malden Electronics Ltd.
  {$EXTERNALSYM MM_MALDEN}
  MM_3DFX                       = 262; //  3Dfx Interactive, Inc.
  {$EXTERNALSYM MM_3DFX}
  MM_MINDMAKER                  = 263; //  Mindmaker, Inc.
  {$EXTERNALSYM MM_MINDMAKER}
  MM_TELEKOL                    = 264; //  Telekol Corp.
  {$EXTERNALSYM MM_TELEKOL}
  MM_ST_MICROELECTRONICS        = 265; //  ST Microelectronics
  {$EXTERNALSYM MM_ST_MICROELECTRONICS}
  MM_ALGOVISION                 = 266; //  Algo Vision Systems GmbH
  {$EXTERNALSYM MM_ALGOVISION}

  MM_UNMAPPED                   = $ffff; // extensible MID mapping
  {$EXTERNALSYM MM_UNMAPPED}
  MM_PID_UNMAPPED               = MM_UNMAPPED;  // extensible PID mapping
  {$EXTERNALSYM MM_PID_UNMAPPED}


  // {d5a47fa7-6d98-11d1-a21a-00a0c9223196}
  function INIT_MMREG_MID(const guid: TGuid; const id: USHORT): TGUID; inline;
  function EXTRACT_MMREG_MID(const guid: TGUID): USHORT; inline;
  function DEFINE_MMREG_MID_GUID(const id: USHORT): TGUID; inline;
  function IS_COMPATIBLE_MMREG_MID(const guid: TGUID): BOOL; inline;
  // {e36dc2ac-6d9a-11d1-a21a-00a0c9223196}
  function INIT_MMREG_PID(const guid: TGUID; const id: USHORT): TGUID; inline;
  function EXTRACT_MMREG_PID(const guid: TGUID): USHORT; inline;
  function DEFINE_MMREG_PID_GUID(const id: USHORT): TGUID; inline;
  function IS_COMPATIBLE_MMREG_PID(const guid: TGUID): BOOL; inline;


const

  // MM_MICROSOFT product IDs
  // ========================
{$IFNDEF _MM_MIDI_MAPPER_DEFINED}
  MM_MIDI_MAPPER     =  1;  //  Midi Mapper
  {$EXTERNALSYM MM_MIDI_MAPPER}
  MM_WAVE_MAPPER     =  2;  //  Wave Mapper
  {$EXTERNALSYM MM_WAVE_MAPPER}
  MM_SNDBLST_MIDIOUT =  3;  //  Sound Blaster MIDI output port
  {$EXTERNALSYM MM_SNDBLST_MIDIOUT}
  MM_SNDBLST_MIDIIN  =  4;  //  Sound Blaster MIDI input port
  {$EXTERNALSYM MM_SNDBLST_MIDIIN}
  MM_SNDBLST_SYNTH   =  5;  //  Sound Blaster internal synth
  {$EXTERNALSYM MM_SNDBLST_SYNTH}
  MM_SNDBLST_WAVEOUT =  6;  //  Sound Blaster waveform output
  {$EXTERNALSYM MM_SNDBLST_WAVEOUT}
  MM_SNDBLST_WAVEIN  =  7;  //  Sound Blaster waveform input
  {$EXTERNALSYM MM_SNDBLST_WAVEIN}
  MM_ADLIB           =  9;  //  Ad Lib Compatible synth
  {$EXTERNALSYM MM_ADLIB}
  MM_MPU401_MIDIOUT  = 10;  //  MPU 401 compatible MIDI output port
  {$EXTERNALSYM MM_MPU401_MIDIOUT}
  MM_MPU401_MIDIIN   = 11;  //  MPU 401 compatible MIDI input port
  {$EXTERNALSYM MM_MPU401_MIDIIN}
  MM_PC_JOYSTICK     = 12;  //  Joystick adapter
  {$EXTERNALSYM MM_PC_JOYSTICK}
{$DEFINE _MM_MIDI_MAPPER_DEFINED}
{$ENDIF}

  MM_PCSPEAKER_WAVEOUT           = 13;  //  PC speaker waveform output
  {$EXTERNALSYM MM_PCSPEAKER_WAVEOUT}
  MM_MSFT_WSS_WAVEIN             = 14;  //  MS Audio Board waveform input
  {$EXTERNALSYM MM_MSFT_WSS_WAVEIN}
  MM_MSFT_WSS_WAVEOUT            = 15;  //  MS Audio Board waveform output
  {$EXTERNALSYM MM_MSFT_WSS_WAVEOUT}
  MM_MSFT_WSS_FMSYNTH_STEREO     = 16;  //  MS Audio Board  Stereo FM synth
  {$EXTERNALSYM MM_MSFT_WSS_FMSYNTH_STEREO}
  MM_MSFT_WSS_MIXER              = 17;  //  MS Audio Board Mixer Driver
  {$EXTERNALSYM MM_MSFT_WSS_MIXER}
  MM_MSFT_WSS_OEM_WAVEIN         = 18;  //  MS OEM Audio Board waveform input
  {$EXTERNALSYM MM_MSFT_WSS_OEM_WAVEIN}
  MM_MSFT_WSS_OEM_WAVEOUT        = 19;  //  MS OEM Audio Board waveform output
  {$EXTERNALSYM MM_MSFT_WSS_OEM_WAVEOUT}
  MM_MSFT_WSS_OEM_FMSYNTH_STEREO = 20;  //  MS OEM Audio Board Stereo FM Synth
  {$EXTERNALSYM MM_MSFT_WSS_OEM_FMSYNTH_STEREO}
  MM_MSFT_WSS_AUX                = 21;  //  MS Audio Board Aux. Port
  {$EXTERNALSYM MM_MSFT_WSS_AUX}
  MM_MSFT_WSS_OEM_AUX            = 22;  //  MS OEM Audio Aux Port
  {$EXTERNALSYM MM_MSFT_WSS_OEM_AUX}
  MM_MSFT_GENERIC_WAVEIN         = 23;  //  MS Vanilla driver waveform input
  {$EXTERNALSYM MM_MSFT_GENERIC_WAVEIN}
  MM_MSFT_GENERIC_WAVEOUT        = 24;  //  MS Vanilla driver wavefrom output
  {$EXTERNALSYM MM_MSFT_GENERIC_WAVEOUT}
  MM_MSFT_GENERIC_MIDIIN         = 25;  //  MS Vanilla driver MIDI in
  {$EXTERNALSYM MM_MSFT_GENERIC_MIDIIN}
  MM_MSFT_GENERIC_MIDIOUT        = 26;  //  MS Vanilla driver MIDI  external out
  {$EXTERNALSYM MM_MSFT_GENERIC_MIDIOUT}
  MM_MSFT_GENERIC_MIDISYNTH      = 27;  //  MS Vanilla driver MIDI synthesizer
  {$EXTERNALSYM MM_MSFT_GENERIC_MIDISYNTH}
  MM_MSFT_GENERIC_AUX_LINE       = 28;  //  MS Vanilla driver aux (line in)
  {$EXTERNALSYM MM_MSFT_GENERIC_AUX_LINE}
  MM_MSFT_GENERIC_AUX_MIC        = 29;  //  MS Vanilla driver aux (mic)
  {$EXTERNALSYM MM_MSFT_GENERIC_AUX_MIC}
  MM_MSFT_GENERIC_AUX_CD         = 30;  //  MS Vanilla driver aux (CD)
  {$EXTERNALSYM MM_MSFT_GENERIC_AUX_CD}
  MM_MSFT_WSS_OEM_MIXER          = 31;  //  MS OEM Audio Board Mixer Driver
  {$EXTERNALSYM MM_MSFT_WSS_OEM_MIXER}
  MM_MSFT_MSACM                  = 32;  //  MS Audio Compression Manager
  {$EXTERNALSYM MM_MSFT_MSACM}
  MM_MSFT_ACM_MSADPCM            = 33;  //  MS ADPCM Codec
  {$EXTERNALSYM MM_MSFT_ACM_MSADPCM}
  MM_MSFT_ACM_IMAADPCM           = 34;  //  IMA ADPCM Codec
  {$EXTERNALSYM MM_MSFT_ACM_IMAADPCM}
  MM_MSFT_ACM_MSFILTER           = 35;  //  MS Filter
  {$EXTERNALSYM MM_MSFT_ACM_MSFILTER}
  MM_MSFT_ACM_GSM610             = 36;  //  GSM 610 codec
  {$EXTERNALSYM MM_MSFT_ACM_GSM610}
  MM_MSFT_ACM_G711               = 37;  //  G.711 codec
  {$EXTERNALSYM MM_MSFT_ACM_G711}
  MM_MSFT_ACM_PCM                = 38;  //  PCM converter
  {$EXTERNALSYM MM_MSFT_ACM_PCM}


  // Microsoft Windows Sound System drivers
  // ======================================

  MM_WSS_SB16_WAVEIN            = 39;  //  Sound Blaster 16 waveform input
  {$EXTERNALSYM MM_WSS_SB16_WAVEIN}
  MM_WSS_SB16_WAVEOUT           = 40;  //  Sound Blaster 16  waveform output
  {$EXTERNALSYM MM_WSS_SB16_WAVEOUT}
  MM_WSS_SB16_MIDIIN            = 41;  //  Sound Blaster 16 midi-in
  {$EXTERNALSYM MM_WSS_SB16_MIDIIN}
  MM_WSS_SB16_MIDIOUT           = 42;  //  Sound Blaster 16 midi out
  {$EXTERNALSYM MM_WSS_SB16_MIDIOUT}
  MM_WSS_SB16_SYNTH             = 43;  //  Sound Blaster 16 FM Synthesis
  {$EXTERNALSYM MM_WSS_SB16_SYNTH}
  MM_WSS_SB16_AUX_LINE          = 44;  //  Sound Blaster 16 aux (line in)
  {$EXTERNALSYM MM_WSS_SB16_AUX_LINE}
  MM_WSS_SB16_AUX_CD            = 45;  //  Sound Blaster 16 aux (CD)
  {$EXTERNALSYM MM_WSS_SB16_AUX_CD}
  MM_WSS_SB16_MIXER             = 46;  //  Sound Blaster 16 mixer device
  {$EXTERNALSYM MM_WSS_SB16_MIXER}
  MM_WSS_SBPRO_WAVEIN           = 47;  //  Sound Blaster Pro waveform input
  {$EXTERNALSYM MM_WSS_SBPRO_WAVEIN}
  MM_WSS_SBPRO_WAVEOUT          = 48;  //  Sound Blaster Pro waveform output
  {$EXTERNALSYM MM_WSS_SBPRO_WAVEOUT}
  MM_WSS_SBPRO_MIDIIN           = 49;  //  Sound Blaster Pro midi in
  {$EXTERNALSYM MM_WSS_SBPRO_MIDIIN}
  MM_WSS_SBPRO_MIDIOUT          = 50;  //  Sound Blaster Pro midi out
  {$EXTERNALSYM MM_WSS_SBPRO_MIDIOUT}
  MM_WSS_SBPRO_SYNTH            = 51;  //  Sound Blaster Pro FM synthesis
  {$EXTERNALSYM MM_WSS_SBPRO_SYNTH}
  MM_WSS_SBPRO_AUX_LINE         = 52;  //  Sound Blaster Pro aux (line in )
  {$EXTERNALSYM MM_WSS_SBPRO_AUX_LINE}
  MM_WSS_SBPRO_AUX_CD           = 53;  //  Sound Blaster Pro aux (CD)
  {$EXTERNALSYM MM_WSS_SBPRO_AUX_CD}
  MM_WSS_SBPRO_MIXER            = 54;  //  Sound Blaster Pro mixer
  {$EXTERNALSYM MM_WSS_SBPRO_MIXER}
  MM_MSFT_WSS_NT_WAVEIN         = 55;  //  WSS NT wave in
  {$EXTERNALSYM MM_MSFT_WSS_NT_WAVEIN}
  MM_MSFT_WSS_NT_WAVEOUT        = 56;  //  WSS NT wave out
  {$EXTERNALSYM MM_MSFT_WSS_NT_WAVEOUT}
  MM_MSFT_WSS_NT_FMSYNTH_STEREO = 57;  //  WSS NT FM synth
  {$EXTERNALSYM MM_MSFT_WSS_NT_FMSYNTH_STEREO}
  MM_MSFT_WSS_NT_MIXER          = 58;  //  WSS NT mixer
  {$EXTERNALSYM MM_MSFT_WSS_NT_MIXER}
  MM_MSFT_WSS_NT_AUX            = 59;  //  WSS NT aux
  {$EXTERNALSYM MM_MSFT_WSS_NT_AUX}
  MM_MSFT_SB16_WAVEIN           = 60;  //  Sound Blaster 16 waveform input
  {$EXTERNALSYM MM_MSFT_SB16_WAVEIN}
  MM_MSFT_SB16_WAVEOUT          = 61;  //  Sound Blaster 16  waveform output
  {$EXTERNALSYM MM_MSFT_SB16_WAVEOUT}
  MM_MSFT_SB16_MIDIIN           = 62;  //  Sound Blaster 16 midi-in
  {$EXTERNALSYM MM_MSFT_SB16_MIDIIN}
  MM_MSFT_SB16_MIDIOUT          = 63;  //  Sound Blaster 16 midi out
  {$EXTERNALSYM MM_MSFT_SB16_MIDIOUT}
  MM_MSFT_SB16_SYNTH            = 64;  //  Sound Blaster 16 FM Synthesis
  {$EXTERNALSYM MM_MSFT_SB16_SYNTH}
  MM_MSFT_SB16_AUX_LINE         = 65;  //  Sound Blaster 16 aux (line in)
  {$EXTERNALSYM MM_MSFT_SB16_AUX_LINE}
  MM_MSFT_SB16_AUX_CD           = 66;  //  Sound Blaster 16 aux (CD)
  {$EXTERNALSYM MM_MSFT_SB16_AUX_CD}
  MM_MSFT_SB16_MIXER            = 67;  //  Sound Blaster 16 mixer device
  {$EXTERNALSYM MM_MSFT_SB16_MIXER}
  MM_MSFT_SBPRO_WAVEIN          = 68;  //  Sound Blaster Pro waveform input
  {$EXTERNALSYM MM_MSFT_SBPRO_WAVEIN}
  MM_MSFT_SBPRO_WAVEOUT         = 69;  //  Sound Blaster Pro waveform output
  {$EXTERNALSYM MM_MSFT_SBPRO_WAVEOUT}
  MM_MSFT_SBPRO_MIDIIN          = 70;  //  Sound Blaster Pro midi in
  {$EXTERNALSYM MM_MSFT_SBPRO_MIDIIN}
  MM_MSFT_SBPRO_MIDIOUT         = 71;  //  Sound Blaster Pro midi out
  {$EXTERNALSYM MM_MSFT_SBPRO_MIDIOUT}
  MM_MSFT_SBPRO_SYNTH           = 72;  //  Sound Blaster Pro FM synthesis
  {$EXTERNALSYM MM_MSFT_SBPRO_SYNTH}
  MM_MSFT_SBPRO_AUX_LINE        = 73;  //  Sound Blaster Pro aux (line in )
  {$EXTERNALSYM MM_MSFT_SBPRO_AUX_LINE}
  MM_MSFT_SBPRO_AUX_CD          = 74;  //  Sound Blaster Pro aux (CD)
  {$EXTERNALSYM MM_MSFT_SBPRO_AUX_CD}
  MM_MSFT_SBPRO_MIXER           = 75;  //  Sound Blaster Pro mixer
  {$EXTERNALSYM MM_MSFT_SBPRO_MIXER}

  MM_MSFT_MSOPL_SYNTH           = 76;  // Yamaha OPL2/OPL3 compatible FM synthesis
  {$EXTERNALSYM MM_MSFT_MSOPL_SYNTH}

  MM_MSFT_VMDMS_LINE_WAVEIN     = 80;  // Voice Modem Serial Line Wave Input
  {$EXTERNALSYM MM_MSFT_VMDMS_LINE_WAVEIN}
  MM_MSFT_VMDMS_LINE_WAVEOUT    = 81;  // Voice Modem Serial Line Wave Output
  {$EXTERNALSYM MM_MSFT_VMDMS_LINE_WAVEOUT}
  MM_MSFT_VMDMS_HANDSET_WAVEIN  = 82;  // Voice Modem Serial Handset Wave Input
  {$EXTERNALSYM MM_MSFT_VMDMS_HANDSET_WAVEIN}
  MM_MSFT_VMDMS_HANDSET_WAVEOUT = 83;  // Voice Modem Serial Handset Wave Output
  {$EXTERNALSYM MM_MSFT_VMDMS_HANDSET_WAVEOUT}
  MM_MSFT_VMDMW_LINE_WAVEIN     = 84;  // Voice Modem Wrapper Line Wave Input
  {$EXTERNALSYM MM_MSFT_VMDMW_LINE_WAVEIN}
  MM_MSFT_VMDMW_LINE_WAVEOUT    = 85;  // Voice Modem Wrapper Line Wave Output
  {$EXTERNALSYM MM_MSFT_VMDMW_LINE_WAVEOUT}
  MM_MSFT_VMDMW_HANDSET_WAVEIN  = 86;  // Voice Modem Wrapper Handset Wave Input
  {$EXTERNALSYM MM_MSFT_VMDMW_HANDSET_WAVEIN}
  MM_MSFT_VMDMW_HANDSET_WAVEOUT = 87;  // Voice Modem Wrapper Handset Wave Output
  {$EXTERNALSYM MM_MSFT_VMDMW_HANDSET_WAVEOUT}
  MM_MSFT_VMDMW_MIXER           = 88;  // Voice Modem Wrapper Mixer
  {$EXTERNALSYM MM_MSFT_VMDMW_MIXER}
  MM_MSFT_VMDM_GAME_WAVEOUT     = 89;  // Voice Modem Game Compatible Wave Device
  {$EXTERNALSYM MM_MSFT_VMDM_GAME_WAVEOUT}
  MM_MSFT_VMDM_GAME_WAVEIN      = 90;  // Voice Modem Game Compatible Wave Device
  {$EXTERNALSYM MM_MSFT_VMDM_GAME_WAVEIN}

  MM_MSFT_ACM_MSNAUDIO          = 91;  //
  {$EXTERNALSYM MM_MSFT_ACM_MSNAUDIO}
  MM_MSFT_ACM_MSG723            = 92;  //
  {$EXTERNALSYM MM_MSFT_ACM_MSG723}
  MM_MSFT_ACM_MSRT24            = 93;  //
  {$EXTERNALSYM MM_MSFT_ACM_MSRT24}

  MM_MSFT_WDMAUDIO_WAVEOUT      = 100;  // Generic id for WDM Audio drivers
  {$EXTERNALSYM MM_MSFT_WDMAUDIO_WAVEOUT}
  MM_MSFT_WDMAUDIO_WAVEIN       = 101;  // Generic id for WDM Audio drivers
  {$EXTERNALSYM MM_MSFT_WDMAUDIO_WAVEIN}
  MM_MSFT_WDMAUDIO_MIDIOUT      = 102;  // Generic id for WDM Audio drivers
  {$EXTERNALSYM MM_MSFT_WDMAUDIO_MIDIOUT}
  MM_MSFT_WDMAUDIO_MIDIIN       = 103;  // Generic id for WDM Audio drivers
  {$EXTERNALSYM MM_MSFT_WDMAUDIO_MIDIIN}
  MM_MSFT_WDMAUDIO_MIXER        = 104;  // Generic id for WDM Audio drivers
  {$EXTERNALSYM MM_MSFT_WDMAUDIO_MIXER}
  MM_MSFT_WDMAUDIO_AUX          = 105;  // Generic id for WDM Audio drivers
  {$EXTERNALSYM MM_MSFT_WDMAUDIO_AUX}


  // MM_CREATIVE product IDs
  // =======================
  MM_CREATIVE_SB15_WAVEIN             = 1;  //  SB (r) 1.5 waveform input
  {$EXTERNALSYM MM_CREATIVE_SB15_WAVEIN}
  MM_CREATIVE_SB20_WAVEIN             = 2;
  {$EXTERNALSYM MM_CREATIVE_SB20_WAVEIN}
  MM_CREATIVE_SBPRO_WAVEIN            = 3;
  {$EXTERNALSYM MM_CREATIVE_SBPRO_WAVEIN}
  MM_CREATIVE_SBP16_WAVEIN            = 4;
  {$EXTERNALSYM MM_CREATIVE_SBP16_WAVEIN}
  MM_CREATIVE_PHNBLST_WAVEIN          = 5;
  {$EXTERNALSYM MM_CREATIVE_PHNBLST_WAVEIN}
  MM_CREATIVE_SB15_WAVEOUT            = 101;
  {$EXTERNALSYM MM_CREATIVE_SB15_WAVEOUT}
  MM_CREATIVE_SB20_WAVEOUT            = 102;
  {$EXTERNALSYM MM_CREATIVE_SB20_WAVEOUT}
  MM_CREATIVE_SBPRO_WAVEOUT           = 103;
  {$EXTERNALSYM MM_CREATIVE_SBPRO_WAVEOUT}
  MM_CREATIVE_SBP16_WAVEOUT           = 104;
  {$EXTERNALSYM MM_CREATIVE_SBP16_WAVEOUT}
  MM_CREATIVE_PHNBLST_WAVEOUT         = 105;
  {$EXTERNALSYM MM_CREATIVE_PHNBLST_WAVEOUT}
  MM_CREATIVE_MIDIOUT                 = 201;  //  SB (r)
  {$EXTERNALSYM MM_CREATIVE_MIDIOUT}
  MM_CREATIVE_MIDIIN                  = 202;  //  SB (r)
  {$EXTERNALSYM MM_CREATIVE_MIDIIN}
  MM_CREATIVE_FMSYNTH_MONO            = 301;  //  SB (r)
  {$EXTERNALSYM MM_CREATIVE_FMSYNTH_MONO}
  MM_CREATIVE_FMSYNTH_STEREO          = 302;  //  SB Pro (r) stereo synthesizer
  {$EXTERNALSYM MM_CREATIVE_FMSYNTH_STEREO}
  MM_CREATIVE_MIDI_AWE32              = 303;
  {$EXTERNALSYM MM_CREATIVE_MIDI_AWE32}
  MM_CREATIVE_AUX_CD                  = 401;  //  SB Pro (r) aux (CD)
  {$EXTERNALSYM MM_CREATIVE_AUX_CD}
  MM_CREATIVE_AUX_LINE                = 402;  //  SB Pro (r) aux (Line in )
  {$EXTERNALSYM MM_CREATIVE_AUX_LINE}
  MM_CREATIVE_AUX_MIC                 = 403;  //  SB Pro (r) aux (mic)
  {$EXTERNALSYM MM_CREATIVE_AUX_MIC}
  MM_CREATIVE_AUX_MASTER              = 404;
  {$EXTERNALSYM MM_CREATIVE_AUX_MASTER}
  MM_CREATIVE_AUX_PCSPK               = 405;
  {$EXTERNALSYM MM_CREATIVE_AUX_PCSPK}
  MM_CREATIVE_AUX_WAVE                = 406;
  {$EXTERNALSYM MM_CREATIVE_AUX_WAVE}
  MM_CREATIVE_AUX_MIDI                = 407;
  {$EXTERNALSYM MM_CREATIVE_AUX_MIDI}
  MM_CREATIVE_SBPRO_MIXER             = 408;
  {$EXTERNALSYM MM_CREATIVE_SBPRO_MIXER}
  MM_CREATIVE_SB16_MIXER              = 409;
  {$EXTERNALSYM MM_CREATIVE_SB16_MIXER}


  // MM_MEDIAVISION product IDs
  // ==========================

  // Pro Audio Spectrum
  //-------------------
  MM_MEDIAVISION_PROAUDIO             = $10;
  {$EXTERNALSYM MM_MEDIAVISION_PROAUDIO}
  MM_PROAUD_MIDIOUT                   = (MM_MEDIAVISION_PROAUDIO + 1);
  {$EXTERNALSYM MM_PROAUD_MIDIOUT}
  MM_PROAUD_MIDIIN                    = (MM_MEDIAVISION_PROAUDIO + 2);
  {$EXTERNALSYM MM_PROAUD_MIDIIN}
  MM_PROAUD_SYNTH                     = (MM_MEDIAVISION_PROAUDIO + 3);
  {$EXTERNALSYM MM_PROAUD_SYNTH}
  MM_PROAUD_WAVEOUT                   = (MM_MEDIAVISION_PROAUDIO + 4);
  {$EXTERNALSYM MM_PROAUD_WAVEOUT}
  MM_PROAUD_WAVEIN                    = (MM_MEDIAVISION_PROAUDIO + 5);
  {$EXTERNALSYM MM_PROAUD_WAVEIN}
  MM_PROAUD_MIXER                     = (MM_MEDIAVISION_PROAUDIO + 6);
  {$EXTERNALSYM MM_PROAUD_MIXER}
  MM_PROAUD_AUX                       = (MM_MEDIAVISION_PROAUDIO + 7);
  {$EXTERNALSYM MM_PROAUD_AUX}


  // Thunder Board
  // -------------
  MM_MEDIAVISION_THUNDER              = $20;
  {$EXTERNALSYM MM_MEDIAVISION_THUNDER}
  MM_THUNDER_SYNTH                    = (MM_MEDIAVISION_THUNDER + 3);
  {$EXTERNALSYM MM_THUNDER_SYNTH}
  MM_THUNDER_WAVEOUT                  = (MM_MEDIAVISION_THUNDER + 4);
  {$EXTERNALSYM MM_THUNDER_WAVEOUT}
  MM_THUNDER_WAVEIN                   = (MM_MEDIAVISION_THUNDER + 5);
  {$EXTERNALSYM MM_THUNDER_WAVEIN}
  MM_THUNDER_AUX                      = (MM_MEDIAVISION_THUNDER + 7);
  {$EXTERNALSYM MM_THUNDER_AUX}

  // Audio Port
  //-----------
  MM_MEDIAVISION_TPORT                = $40;
  {$EXTERNALSYM MM_MEDIAVISION_TPORT}
  MM_TPORT_WAVEOUT                    = (MM_MEDIAVISION_TPORT + 1);
  {$EXTERNALSYM MM_TPORT_WAVEOUT}
  MM_TPORT_WAVEIN                     = (MM_MEDIAVISION_TPORT + 2);
  {$EXTERNALSYM MM_TPORT_WAVEIN}
  MM_TPORT_SYNTH                      = (MM_MEDIAVISION_TPORT + 3);
  {$EXTERNALSYM MM_TPORT_SYNTH}

  // Pro Audio Spectrum Plus
  //------------------------
  MM_MEDIAVISION_PROAUDIO_PLUS        = $50;
  {$EXTERNALSYM MM_MEDIAVISION_PROAUDIO_PLUS}
  MM_PROAUD_PLUS_MIDIOUT              = (MM_MEDIAVISION_PROAUDIO_PLUS + 1);
  {$EXTERNALSYM MM_PROAUD_PLUS_MIDIOUT}
  MM_PROAUD_PLUS_MIDIIN               = (MM_MEDIAVISION_PROAUDIO_PLUS + 2);
  {$EXTERNALSYM MM_PROAUD_PLUS_MIDIIN}
  MM_PROAUD_PLUS_SYNTH                = (MM_MEDIAVISION_PROAUDIO_PLUS + 3);
  {$EXTERNALSYM MM_PROAUD_PLUS_SYNTH}
  MM_PROAUD_PLUS_WAVEOUT              = (MM_MEDIAVISION_PROAUDIO_PLUS + 4);
  {$EXTERNALSYM MM_PROAUD_PLUS_WAVEOUT}
  MM_PROAUD_PLUS_WAVEIN               = (MM_MEDIAVISION_PROAUDIO_PLUS + 5);
  {$EXTERNALSYM MM_PROAUD_PLUS_WAVEIN}
  MM_PROAUD_PLUS_MIXER                = (MM_MEDIAVISION_PROAUDIO_PLUS + 6);
  {$EXTERNALSYM MM_PROAUD_PLUS_MIXER}
  MM_PROAUD_PLUS_AUX                  = (MM_MEDIAVISION_PROAUDIO_PLUS + 7);
  {$EXTERNALSYM MM_PROAUD_PLUS_AUX}

  // Pro Audio Spectrum 16
  //----------------------
  MM_MEDIAVISION_PROAUDIO_16          = $60;
  {$EXTERNALSYM MM_MEDIAVISION_PROAUDIO_16}
  MM_PROAUD_16_MIDIOUT                = (MM_MEDIAVISION_PROAUDIO_16 + 1);
  {$EXTERNALSYM MM_PROAUD_16_MIDIOUT}
  MM_PROAUD_16_MIDIIN                 = (MM_MEDIAVISION_PROAUDIO_16 + 2);
  {$EXTERNALSYM MM_PROAUD_16_MIDIIN}
  MM_PROAUD_16_SYNTH                  = (MM_MEDIAVISION_PROAUDIO_16 + 3);
  {$EXTERNALSYM MM_PROAUD_16_SYNTH}
  MM_PROAUD_16_WAVEOUT                = (MM_MEDIAVISION_PROAUDIO_16 + 4);
  {$EXTERNALSYM MM_PROAUD_16_WAVEOUT}
  MM_PROAUD_16_WAVEIN                 = (MM_MEDIAVISION_PROAUDIO_16 + 5);
  {$EXTERNALSYM MM_PROAUD_16_WAVEIN}
  MM_PROAUD_16_MIXER                  = (MM_MEDIAVISION_PROAUDIO_16 + 6);
  {$EXTERNALSYM MM_PROAUD_16_MIXER}
  MM_PROAUD_16_AUX                    = (MM_MEDIAVISION_PROAUDIO_16 + 7);
  {$EXTERNALSYM MM_PROAUD_16_AUX}

  // Pro Audio Studio 16
  //--------------------
  MM_MEDIAVISION_PROSTUDIO_16         = $60;
  {$EXTERNALSYM MM_MEDIAVISION_PROSTUDIO_16}
  MM_STUDIO_16_MIDIOUT                = (MM_MEDIAVISION_PROSTUDIO_16 + 1);
  {$EXTERNALSYM MM_STUDIO_16_MIDIOUT}
  MM_STUDIO_16_MIDIIN                 = (MM_MEDIAVISION_PROSTUDIO_16 + 2);
  {$EXTERNALSYM MM_STUDIO_16_MIDIIN}
  MM_STUDIO_16_SYNTH                  = (MM_MEDIAVISION_PROSTUDIO_16 + 3);
  {$EXTERNALSYM MM_STUDIO_16_SYNTH}
  MM_STUDIO_16_WAVEOUT                = (MM_MEDIAVISION_PROSTUDIO_16 + 4);
  {$EXTERNALSYM MM_STUDIO_16_WAVEOUT}
  MM_STUDIO_16_WAVEIN                 = (MM_MEDIAVISION_PROSTUDIO_16 + 5);
  {$EXTERNALSYM MM_STUDIO_16_WAVEIN}
  MM_STUDIO_16_MIXER                  = (MM_MEDIAVISION_PROSTUDIO_16 + 6);
  {$EXTERNALSYM MM_STUDIO_16_MIXER}
  MM_STUDIO_16_AUX                    = (MM_MEDIAVISION_PROSTUDIO_16 + 7);
  {$EXTERNALSYM MM_STUDIO_16_AUX}

  // CDPC
  //-----
  MM_MEDIAVISION_CDPC                 = $70;
  {$EXTERNALSYM MM_MEDIAVISION_CDPC}
  MM_CDPC_MIDIOUT                     = (MM_MEDIAVISION_CDPC + 1);
  {$EXTERNALSYM MM_CDPC_MIDIOUT}
  MM_CDPC_MIDIIN                      = (MM_MEDIAVISION_CDPC + 2);
  {$EXTERNALSYM MM_CDPC_MIDIIN}
  MM_CDPC_SYNTH                       = (MM_MEDIAVISION_CDPC + 3);
  {$EXTERNALSYM MM_CDPC_SYNTH}
  MM_CDPC_WAVEOUT                     = (MM_MEDIAVISION_CDPC + 4);
  {$EXTERNALSYM MM_CDPC_WAVEOUT}
  MM_CDPC_WAVEIN                      = (MM_MEDIAVISION_CDPC + 5);
  {$EXTERNALSYM MM_CDPC_WAVEIN}
  MM_CDPC_MIXER                       = (MM_MEDIAVISION_CDPC + 6);
  {$EXTERNALSYM MM_CDPC_MIXER}
  MM_CDPC_AUX                         = (MM_MEDIAVISION_CDPC + 7);
  {$EXTERNALSYM MM_CDPC_AUX}

  // Opus MV 1208 Chipset
  //---------------------
  MM_MEDIAVISION_OPUS1208             = $80;
  {$EXTERNALSYM MM_MEDIAVISION_OPUS1208}
  MM_OPUS401_MIDIOUT                  = (MM_MEDIAVISION_OPUS1208 + 1);
  {$EXTERNALSYM MM_OPUS401_MIDIOUT}
  MM_OPUS401_MIDIIN                   = (MM_MEDIAVISION_OPUS1208 + 2);
  {$EXTERNALSYM MM_OPUS401_MIDIIN}
  MM_OPUS1208_SYNTH                   = (MM_MEDIAVISION_OPUS1208 + 3);
  {$EXTERNALSYM MM_OPUS1208_SYNTH}
  MM_OPUS1208_WAVEOUT                 = (MM_MEDIAVISION_OPUS1208 + 4);
  {$EXTERNALSYM MM_OPUS1208_WAVEOUT}
  MM_OPUS1208_WAVEIN                  = (MM_MEDIAVISION_OPUS1208 + 5);
  {$EXTERNALSYM MM_OPUS1208_WAVEIN}
  MM_OPUS1208_MIXER                   = (MM_MEDIAVISION_OPUS1208 + 6);
  {$EXTERNALSYM MM_OPUS1208_MIXER}
  MM_OPUS1208_AUX                     = (MM_MEDIAVISION_OPUS1208 + 7);
  {$EXTERNALSYM MM_OPUS1208_AUX}

  // Opus MV 1216 chipset
  //---------------------
  MM_MEDIAVISION_OPUS1216             = $90;
  {$EXTERNALSYM MM_MEDIAVISION_OPUS1216}
  MM_OPUS1216_MIDIOUT                 = (MM_MEDIAVISION_OPUS1216 + 1);
  {$EXTERNALSYM MM_OPUS1216_MIDIOUT}
  MM_OPUS1216_MIDIIN                  = (MM_MEDIAVISION_OPUS1216 + 2);
  {$EXTERNALSYM MM_OPUS1216_MIDIIN}
  MM_OPUS1216_SYNTH                   = (MM_MEDIAVISION_OPUS1216 + 3);
  {$EXTERNALSYM MM_OPUS1216_SYNTH}
  MM_OPUS1216_WAVEOUT                 = (MM_MEDIAVISION_OPUS1216 + 4);
  {$EXTERNALSYM MM_OPUS1216_WAVEOUT}
  MM_OPUS1216_WAVEIN                  = (MM_MEDIAVISION_OPUS1216 + 5);
  {$EXTERNALSYM MM_OPUS1216_WAVEIN}
  MM_OPUS1216_MIXER                   = (MM_MEDIAVISION_OPUS1216 + 6);
  {$EXTERNALSYM MM_OPUS1216_MIXER}
  MM_OPUS1216_AUX                     = (MM_MEDIAVISION_OPUS1216 + 7);
  {$EXTERNALSYM MM_OPUS1216_AUX}


  // MM_CYRIX product IDs
  // ====================
  MM_CYRIX_XASYNTH                    = 1;
  {$EXTERNALSYM MM_CYRIX_XASYNTH}
  MM_CYRIX_XAMIDIIN                   = 2;
  {$EXTERNALSYM MM_CYRIX_XAMIDIIN}
  MM_CYRIX_XAMIDIOUT                  = 3;
  {$EXTERNALSYM MM_CYRIX_XAMIDIOUT}
  MM_CYRIX_XAWAVEIN                   = 4;
  {$EXTERNALSYM MM_CYRIX_XAWAVEIN}
  MM_CYRIX_XAWAVEOUT                  = 5;
  {$EXTERNALSYM MM_CYRIX_XAWAVEOUT}
  MM_CYRIX_XAAUX                      = 6;
  {$EXTERNALSYM MM_CYRIX_XAAUX}
  MM_CYRIX_XAMIXER                    = 7;
  {$EXTERNALSYM MM_CYRIX_XAMIXER}


  // MM_PHILIPS_SPEECH_PROCESSING products IDs
  // =========================================
  MM_PHILIPS_ACM_LPCBB                = 1;
  {$EXTERNALSYM MM_PHILIPS_ACM_LPCBB}


  // MM_NETXL product IDs
  // ====================
  MM_NETXL_XLVIDEO                    = 1;
  {$EXTERNALSYM MM_NETXL_XLVIDEO}


  // MM_ZYXEL product IDs
  // ====================
  MM_ZYXEL_ACM_ADPCM                  = 1;
  {$EXTERNALSYM MM_ZYXEL_ACM_ADPCM}


  // MM_AARDVARK product IDs
  // =======================
  MM_AARDVARK_STUDIO12_WAVEOUT        = 1;
  {$EXTERNALSYM MM_AARDVARK_STUDIO12_WAVEOUT}
  MM_AARDVARK_STUDIO12_WAVEIN         = 2;
  {$EXTERNALSYM MM_AARDVARK_STUDIO12_WAVEIN}
  MM_AARDVARK_STUDIO88_WAVEOUT        = 3;
  {$EXTERNALSYM MM_AARDVARK_STUDIO88_WAVEOUT}
  MM_AARDVARK_STUDIO88_WAVEIN         = 4;
  {$EXTERNALSYM MM_AARDVARK_STUDIO88_WAVEIN}


  // MM_BINTEC product IDs
  // =====================
  MM_BINTEC_TAPI_WAVE                 = 1;
  {$EXTERNALSYM MM_BINTEC_TAPI_WAVE}


  // MM_HEWLETT_PACKARD product IDs
  // ==============================
  MM_HEWLETT_PACKARD_CU_CODEC         = 1;
  {$EXTERNALSYM MM_HEWLETT_PACKARD_CU_CODEC}


  // MM_MITEL product IDs
  // ====================
  MM_MITEL_TALKTO_LINE_WAVEOUT        = 100;
  {$EXTERNALSYM MM_MITEL_TALKTO_LINE_WAVEOUT}
  MM_MITEL_TALKTO_LINE_WAVEIN         = 101;
  {$EXTERNALSYM MM_MITEL_TALKTO_LINE_WAVEIN}
  MM_MITEL_TALKTO_HANDSET_WAVEOUT     = 102;
  {$EXTERNALSYM MM_MITEL_TALKTO_HANDSET_WAVEOUT}
  MM_MITEL_TALKTO_HANDSET_WAVEIN      = 103;
  {$EXTERNALSYM MM_MITEL_TALKTO_HANDSET_WAVEIN}
  MM_MITEL_TALKTO_BRIDGED_WAVEOUT     = 104;
  {$EXTERNALSYM MM_MITEL_TALKTO_BRIDGED_WAVEOUT}
  MM_MITEL_TALKTO_BRIDGED_WAVEIN      = 105;
  {$EXTERNALSYM MM_MITEL_TALKTO_BRIDGED_WAVEIN}
  MM_MITEL_MPA_HANDSET_WAVEOUT        = 200;
  {$EXTERNALSYM MM_MITEL_MPA_HANDSET_WAVEOUT}
  MM_MITEL_MPA_HANDSET_WAVEIN         = 201;
  {$EXTERNALSYM MM_MITEL_MPA_HANDSET_WAVEIN}
  MM_MITEL_MPA_HANDSFREE_WAVEOUT      = 202;
  {$EXTERNALSYM MM_MITEL_MPA_HANDSFREE_WAVEOUT}
  MM_MITEL_MPA_HANDSFREE_WAVEIN       = 203;
  {$EXTERNALSYM MM_MITEL_MPA_HANDSFREE_WAVEIN}
  MM_MITEL_MPA_LINE1_WAVEOUT          = 204;
  {$EXTERNALSYM MM_MITEL_MPA_LINE1_WAVEOUT}
  MM_MITEL_MPA_LINE1_WAVEIN           = 205;
  {$EXTERNALSYM MM_MITEL_MPA_LINE1_WAVEIN}
  MM_MITEL_MPA_LINE2_WAVEOUT          = 206;
  {$EXTERNALSYM MM_MITEL_MPA_LINE2_WAVEOUT}
  MM_MITEL_MPA_LINE2_WAVEIN           = 207;
  {$EXTERNALSYM MM_MITEL_MPA_LINE2_WAVEIN}
  MM_MITEL_MEDIAPATH_WAVEOUT          = 300;
  {$EXTERNALSYM MM_MITEL_MEDIAPATH_WAVEOUT}
  MM_MITEL_MEDIAPATH_WAVEIN           = 301;
  {$EXTERNALSYM MM_MITEL_MEDIAPATH_WAVEIN}

  // MM_SNI product IDs
  // ==================
  MM_SNI_ACM_G721                     = 1;
  {$EXTERNALSYM MM_SNI_ACM_G721}


  // MM_EMU product IDs
  // ==================
  MM_EMU_APSSYNTH                     = 1;
  {$EXTERNALSYM MM_EMU_APSSYNTH}
  MM_EMU_APSMIDIIN                    = 2;
  {$EXTERNALSYM MM_EMU_APSMIDIIN}
  MM_EMU_APSMIDIOUT                   = 3;
  {$EXTERNALSYM MM_EMU_APSMIDIOUT}
  MM_EMU_APSWAVEIN                    = 4;
  {$EXTERNALSYM MM_EMU_APSWAVEIN}
  MM_EMU_APSWAVEOUT                   = 5;
  {$EXTERNALSYM MM_EMU_APSWAVEOUT}


  // MM_ARTISOFT product IDs
  // =======================
  MM_ARTISOFT_SBWAVEIN                = 1;  // Artisoft sounding Board waveform input
  {$EXTERNALSYM MM_ARTISOFT_SBWAVEIN}
  MM_ARTISOFT_SBWAVEOUT               = 2;  // Artisoft sounding Board waveform output
  {$EXTERNALSYM MM_ARTISOFT_SBWAVEOUT}


  // MM_TURTLE_BEACH product IDs
  // ===========================
  MM_TBS_TROPEZ_WAVEIN                = 37;
  {$EXTERNALSYM MM_TBS_TROPEZ_WAVEIN}
  MM_TBS_TROPEZ_WAVEOUT               = 38;
  {$EXTERNALSYM MM_TBS_TROPEZ_WAVEOUT}
  MM_TBS_TROPEZ_AUX1                  = 39;
  {$EXTERNALSYM MM_TBS_TROPEZ_AUX1}
  MM_TBS_TROPEZ_AUX2                  = 40;
  {$EXTERNALSYM MM_TBS_TROPEZ_AUX2}
  MM_TBS_TROPEZ_LINE                  = 41;
  {$EXTERNALSYM MM_TBS_TROPEZ_LINE}


  // MM_IBM product IDs
  // ==================
  MM_MMOTION_WAVEAUX                  = 1;  //  IBM M-Motion Auxiliary Device
  {$EXTERNALSYM MM_MMOTION_WAVEAUX}
  MM_MMOTION_WAVEOUT                  = 2;  //  IBM M-Motion Waveform output
  {$EXTERNALSYM MM_MMOTION_WAVEOUT}
  MM_MMOTION_WAVEIN                   = 3;  //  IBM M-Motion  Waveform Input
  {$EXTERNALSYM MM_MMOTION_WAVEIN}
  MM_IBM_PCMCIA_WAVEIN                = 11; //  IBM waveform input
  {$EXTERNALSYM MM_IBM_PCMCIA_WAVEIN}
  MM_IBM_PCMCIA_WAVEOUT               = 12; //  IBM Waveform output
  {$EXTERNALSYM MM_IBM_PCMCIA_WAVEOUT}
  MM_IBM_PCMCIA_SYNTH                 = 13; //  IBM Midi Synthesis
  {$EXTERNALSYM MM_IBM_PCMCIA_SYNTH}
  MM_IBM_PCMCIA_MIDIIN                = 14; //  IBM external MIDI in
  {$EXTERNALSYM MM_IBM_PCMCIA_MIDIIN}
  MM_IBM_PCMCIA_MIDIOUT               = 15; //  IBM external MIDI out
  {$EXTERNALSYM MM_IBM_PCMCIA_MIDIOUT}
  MM_IBM_PCMCIA_AUX                   = 16; //  IBM auxiliary control
  {$EXTERNALSYM MM_IBM_PCMCIA_AUX}
  MM_IBM_THINKPAD200                  = 17;
  {$EXTERNALSYM MM_IBM_THINKPAD200}
  MM_IBM_MWAVE_WAVEIN                 = 18;
  {$EXTERNALSYM MM_IBM_MWAVE_WAVEIN}
  MM_IBM_MWAVE_WAVEOUT                = 19;
  {$EXTERNALSYM MM_IBM_MWAVE_WAVEOUT}
  MM_IBM_MWAVE_MIXER                  = 20;
  {$EXTERNALSYM MM_IBM_MWAVE_MIXER}
  MM_IBM_MWAVE_MIDIIN                 = 21;
  {$EXTERNALSYM MM_IBM_MWAVE_MIDIIN}
  MM_IBM_MWAVE_MIDIOUT                = 22;
  {$EXTERNALSYM MM_IBM_MWAVE_MIDIOUT}
  MM_IBM_MWAVE_AUX                    = 23;
  {$EXTERNALSYM MM_IBM_MWAVE_AUX}
  MM_IBM_WC_MIDIOUT                   = 30;
  {$EXTERNALSYM MM_IBM_WC_MIDIOUT}
  MM_IBM_WC_WAVEOUT                   = 31;
  {$EXTERNALSYM MM_IBM_WC_WAVEOUT}
  MM_IBM_WC_MIXEROUT                  = 33;
  {$EXTERNALSYM MM_IBM_WC_MIXEROUT}


  // MM_VOCALTEC product IDs
  // =======================
  MM_VOCALTEC_WAVEOUT                 = 1;
  {$EXTERNALSYM MM_VOCALTEC_WAVEOUT}
  MM_VOCALTEC_WAVEIN                  = 2;
  {$EXTERNALSYM MM_VOCALTEC_WAVEIN}


  // MM_ROLAND product IDs
  // =====================
  MM_ROLAND_RAP10_MIDIOUT             = 10;  // MM_ROLAND_RAP10
  {$EXTERNALSYM MM_ROLAND_RAP10_MIDIOUT}
  MM_ROLAND_RAP10_MIDIIN              = 11;  // MM_ROLAND_RAP10
  {$EXTERNALSYM MM_ROLAND_RAP10_MIDIIN}
  MM_ROLAND_RAP10_SYNTH               = 12;  // MM_ROLAND_RAP10
  {$EXTERNALSYM MM_ROLAND_RAP10_SYNTH}
  MM_ROLAND_RAP10_WAVEOUT             = 13;  // MM_ROLAND_RAP10
  {$EXTERNALSYM MM_ROLAND_RAP10_WAVEOUT}
  MM_ROLAND_RAP10_WAVEIN              = 14;  // MM_ROLAND_RAP10
  {$EXTERNALSYM MM_ROLAND_RAP10_WAVEIN}
  MM_ROLAND_MPU401_MIDIOUT            = 15;
  {$EXTERNALSYM MM_ROLAND_MPU401_MIDIOUT}
  MM_ROLAND_MPU401_MIDIIN             = 16;
  {$EXTERNALSYM MM_ROLAND_MPU401_MIDIIN}
  MM_ROLAND_SMPU_MIDIOUTA             = 17;
  {$EXTERNALSYM MM_ROLAND_SMPU_MIDIOUTA}
  MM_ROLAND_SMPU_MIDIOUTB             = 18;
  {$EXTERNALSYM MM_ROLAND_SMPU_MIDIOUTB}
  MM_ROLAND_SMPU_MIDIINA              = 19;
  {$EXTERNALSYM MM_ROLAND_SMPU_MIDIINA}
  MM_ROLAND_SMPU_MIDIINB              = 20;
  {$EXTERNALSYM MM_ROLAND_SMPU_MIDIINB}
  MM_ROLAND_SC7_MIDIOUT               = 21;
  {$EXTERNALSYM MM_ROLAND_SC7_MIDIOUT}
  MM_ROLAND_SC7_MIDIIN                = 22;
  {$EXTERNALSYM MM_ROLAND_SC7_MIDIIN}
  MM_ROLAND_SERIAL_MIDIOUT            = 23;
  {$EXTERNALSYM MM_ROLAND_SERIAL_MIDIOUT}
  MM_ROLAND_SERIAL_MIDIIN             = 24;
  {$EXTERNALSYM MM_ROLAND_SERIAL_MIDIIN}
  MM_ROLAND_SCP_MIDIOUT               = 38;
  {$EXTERNALSYM MM_ROLAND_SCP_MIDIOUT}
  MM_ROLAND_SCP_MIDIIN                = 39;
  {$EXTERNALSYM MM_ROLAND_SCP_MIDIIN}
  MM_ROLAND_SCP_WAVEOUT               = 40;
  {$EXTERNALSYM MM_ROLAND_SCP_WAVEOUT}
  MM_ROLAND_SCP_WAVEIN                = 41;
  {$EXTERNALSYM MM_ROLAND_SCP_WAVEIN}
  MM_ROLAND_SCP_MIXER                 = 42;
  {$EXTERNALSYM MM_ROLAND_SCP_MIXER}
  MM_ROLAND_SCP_AUX                   = 48;
  {$EXTERNALSYM MM_ROLAND_SCP_AUX}


  // MM_DSP_SOLUTIONS product IDs
  // ============================
  MM_DSP_SOLUTIONS_WAVEOUT            = 1;
  {$EXTERNALSYM MM_DSP_SOLUTIONS_WAVEOUT}
  MM_DSP_SOLUTIONS_WAVEIN             = 2;
  {$EXTERNALSYM MM_DSP_SOLUTIONS_WAVEIN}
  MM_DSP_SOLUTIONS_SYNTH              = 3;
  {$EXTERNALSYM MM_DSP_SOLUTIONS_SYNTH}
  MM_DSP_SOLUTIONS_AUX                = 4;
  {$EXTERNALSYM MM_DSP_SOLUTIONS_AUX}


  // MM_NEC product IDs
  // ==================
  MM_NEC_73_86_SYNTH                  = 5;
  {$EXTERNALSYM MM_NEC_73_86_SYNTH}
  MM_NEC_73_86_WAVEOUT                = 6;
  {$EXTERNALSYM MM_NEC_73_86_WAVEOUT}
  MM_NEC_73_86_WAVEIN                 = 7;
  {$EXTERNALSYM MM_NEC_73_86_WAVEIN}
  MM_NEC_26_SYNTH                     = 9;
  {$EXTERNALSYM MM_NEC_26_SYNTH}
  MM_NEC_MPU401_MIDIOUT               = 10;
  {$EXTERNALSYM MM_NEC_MPU401_MIDIOUT}
  MM_NEC_MPU401_MIDIIN                = 11;
  {$EXTERNALSYM MM_NEC_MPU401_MIDIIN}
  MM_NEC_JOYSTICK                     = 12;
  {$EXTERNALSYM MM_NEC_JOYSTICK}


  // MM_WANGLABS product IDs
  // =======================
  MM_WANGLABS_WAVEIN1                 = 1;  //  Input audio wave on CPU board models: Exec 4010, 4030,
  {$EXTERNALSYM MM_WANGLABS_WAVEIN1}
                                            //  3450; PC 251/25c, pc 461/25s , pc 461/33c
  MM_WANGLABS_WAVEOUT1                = 2;
  {$EXTERNALSYM MM_WANGLABS_WAVEOUT1}


  // MM_TANDY product IDs
  // ====================
  MM_TANDY_VISWAVEIN                  = 1;
  {$EXTERNALSYM MM_TANDY_VISWAVEIN}
  MM_TANDY_VISWAVEOUT                 = 2;
  {$EXTERNALSYM MM_TANDY_VISWAVEOUT}
  MM_TANDY_VISBIOSSYNTH               = 3;
  {$EXTERNALSYM MM_TANDY_VISBIOSSYNTH}
  MM_TANDY_SENS_MMAWAVEIN             = 4;
  {$EXTERNALSYM MM_TANDY_SENS_MMAWAVEIN}
  MM_TANDY_SENS_MMAWAVEOUT            = 5;
  {$EXTERNALSYM MM_TANDY_SENS_MMAWAVEOUT}
  MM_TANDY_SENS_MMAMIDIIN             = 6;
  {$EXTERNALSYM MM_TANDY_SENS_MMAMIDIIN}
  MM_TANDY_SENS_MMAMIDIOUT            = 7;
  {$EXTERNALSYM MM_TANDY_SENS_MMAMIDIOUT}
  MM_TANDY_SENS_VISWAVEOUT            = 8;
  {$EXTERNALSYM MM_TANDY_SENS_VISWAVEOUT}
  MM_TANDY_PSSJWAVEIN                 = 9;
  {$EXTERNALSYM MM_TANDY_PSSJWAVEIN}
  MM_TANDY_PSSJWAVEOUT                = 10;
  {$EXTERNALSYM MM_TANDY_PSSJWAVEOUT}


  // MM_ANTEX product IDs
  // ====================
  MM_ANTEX_SX12_WAVEIN                = 1;
  {$EXTERNALSYM MM_ANTEX_SX12_WAVEIN}
  MM_ANTEX_SX12_WAVEOUT               = 2;
  {$EXTERNALSYM MM_ANTEX_SX12_WAVEOUT}
  MM_ANTEX_SX15_WAVEIN                = 3;
  {$EXTERNALSYM MM_ANTEX_SX15_WAVEIN}
  MM_ANTEX_SX15_WAVEOUT               = 4;
  {$EXTERNALSYM MM_ANTEX_SX15_WAVEOUT}
  MM_ANTEX_VP625_WAVEIN               = 5;
  {$EXTERNALSYM MM_ANTEX_VP625_WAVEIN}
  MM_ANTEX_VP625_WAVEOUT              = 6;
  {$EXTERNALSYM MM_ANTEX_VP625_WAVEOUT}
  MM_ANTEX_AUDIOPORT22_WAVEIN         = 7;
  {$EXTERNALSYM MM_ANTEX_AUDIOPORT22_WAVEIN}
  MM_ANTEX_AUDIOPORT22_WAVEOUT        = 8;
  {$EXTERNALSYM MM_ANTEX_AUDIOPORT22_WAVEOUT}
  MM_ANTEX_AUDIOPORT22_FEEDTHRU       = 9;
  {$EXTERNALSYM MM_ANTEX_AUDIOPORT22_FEEDTHRU}


  // MM_INTEL product IDs
  // ====================
  MM_INTELOPD_WAVEIN                  = 1;    // HID2 WaveAudio Driver
  {$EXTERNALSYM MM_INTELOPD_WAVEIN}
  MM_INTELOPD_WAVEOUT                 = 101;  // HID2
  {$EXTERNALSYM MM_INTELOPD_WAVEOUT}
  MM_INTELOPD_AUX                     = 401;  // HID2 for mixing
  {$EXTERNALSYM MM_INTELOPD_AUX}
  MM_INTEL_NSPMODEMLINEIN             = 501;
  {$EXTERNALSYM MM_INTEL_NSPMODEMLINEIN}
  MM_INTEL_NSPMODEMLINEOUT            = 502;
  {$EXTERNALSYM MM_INTEL_NSPMODEMLINEOUT}


  // MM_VAL product IDs
  // ==================
  MM_VAL_MICROKEY_AP_WAVEIN           = 1;
  {$EXTERNALSYM MM_VAL_MICROKEY_AP_WAVEIN}
  MM_VAL_MICROKEY_AP_WAVEOUT          = 2;
  {$EXTERNALSYM MM_VAL_MICROKEY_AP_WAVEOUT}


  // MM_INTERACTIVE product IDs
  // ==========================
  MM_INTERACTIVE_WAVEIN               = $45;
  {$EXTERNALSYM MM_INTERACTIVE_WAVEIN}
  MM_INTERACTIVE_WAVEOUT              = $45;
  {$EXTERNALSYM MM_INTERACTIVE_WAVEOUT}


  // MM_YAMAHA product IDs
  // =====================
  MM_YAMAHA_GSS_SYNTH                 = $01;
  {$EXTERNALSYM MM_YAMAHA_GSS_SYNTH}
  MM_YAMAHA_GSS_WAVEOUT               = $02;
  {$EXTERNALSYM MM_YAMAHA_GSS_WAVEOUT}
  MM_YAMAHA_GSS_WAVEIN                = $03;
  {$EXTERNALSYM MM_YAMAHA_GSS_WAVEIN}
  MM_YAMAHA_GSS_MIDIOUT               = $04;
  {$EXTERNALSYM MM_YAMAHA_GSS_MIDIOUT}
  MM_YAMAHA_GSS_MIDIIN                = $05;
  {$EXTERNALSYM MM_YAMAHA_GSS_MIDIIN}
  MM_YAMAHA_GSS_AUX                   = $06;
  {$EXTERNALSYM MM_YAMAHA_GSS_AUX}
  MM_YAMAHA_SERIAL_MIDIOUT            = $07;
  {$EXTERNALSYM MM_YAMAHA_SERIAL_MIDIOUT}
  MM_YAMAHA_SERIAL_MIDIIN             = $08;
  {$EXTERNALSYM MM_YAMAHA_SERIAL_MIDIIN}
  MM_YAMAHA_OPL3SA_WAVEOUT            = $10;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_WAVEOUT}
  MM_YAMAHA_OPL3SA_WAVEIN             = $11;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_WAVEIN}
  MM_YAMAHA_OPL3SA_FMSYNTH            = $12;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_FMSYNTH}
  MM_YAMAHA_OPL3SA_YSYNTH             = $13;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_YSYNTH}
  MM_YAMAHA_OPL3SA_MIDIOUT            = $14;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_MIDIOUT}
  MM_YAMAHA_OPL3SA_MIDIIN             = $15;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_MIDIIN}
  MM_YAMAHA_OPL3SA_MIXER              = $17;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_MIXER}
  MM_YAMAHA_OPL3SA_JOYSTICK           = $18;
  {$EXTERNALSYM MM_YAMAHA_OPL3SA_JOYSTICK}
  MM_YAMAHA_YMF724LEG_MIDIOUT         = $19;
  {$EXTERNALSYM MM_YAMAHA_YMF724LEG_MIDIOUT}
  MM_YAMAHA_YMF724LEG_MIDIIN          = $1A;
  {$EXTERNALSYM MM_YAMAHA_YMF724LEG_MIDIIN}
  MM_YAMAHA_YMF724_WAVEOUT            = $1B;
  {$EXTERNALSYM MM_YAMAHA_YMF724_WAVEOUT}
  MM_YAMAHA_YMF724_WAVEIN             = $1C;
  {$EXTERNALSYM MM_YAMAHA_YMF724_WAVEIN}
  MM_YAMAHA_YMF724_MIDIOUT            = $1D;
  {$EXTERNALSYM MM_YAMAHA_YMF724_MIDIOUT}
  MM_YAMAHA_YMF724_AUX                = $1E;
  {$EXTERNALSYM MM_YAMAHA_YMF724_AUX}
  MM_YAMAHA_YMF724_MIXER              = $1F;
  {$EXTERNALSYM MM_YAMAHA_YMF724_MIXER}
  MM_YAMAHA_YMF724LEG_FMSYNTH         = $20;
  {$EXTERNALSYM MM_YAMAHA_YMF724LEG_FMSYNTH}
  MM_YAMAHA_YMF724LEG_MIXER           = $21;
  {$EXTERNALSYM MM_YAMAHA_YMF724LEG_MIXER}
  MM_YAMAHA_SXG_MIDIOUT               = $22;
  {$EXTERNALSYM MM_YAMAHA_SXG_MIDIOUT}
  MM_YAMAHA_SXG_WAVEOUT               = $23;
  {$EXTERNALSYM MM_YAMAHA_SXG_WAVEOUT}
  MM_YAMAHA_SXG_MIXER                 = $24;
  {$EXTERNALSYM MM_YAMAHA_SXG_MIXER}
  MM_YAMAHA_ACXG_WAVEIN               = $25;
  {$EXTERNALSYM MM_YAMAHA_ACXG_WAVEIN}
  MM_YAMAHA_ACXG_WAVEOUT              = $26;
  {$EXTERNALSYM MM_YAMAHA_ACXG_WAVEOUT}
  MM_YAMAHA_ACXG_MIDIOUT              = $27;
  {$EXTERNALSYM MM_YAMAHA_ACXG_MIDIOUT}
  MM_YAMAHA_ACXG_MIXER                = $28;
  {$EXTERNALSYM MM_YAMAHA_ACXG_MIXER}
  MM_YAMAHA_ACXG_AUX                  = $29;
  {$EXTERNALSYM MM_YAMAHA_ACXG_AUX}


  // MM_EVEREX product IDs
  // =====================
  MM_EVEREX_CARRIER                   = 1;
  {$EXTERNALSYM MM_EVEREX_CARRIER}


  // MM_ECHO product IDs
  // ===================
  MM_ECHO_SYNTH                       = 1;
  {$EXTERNALSYM MM_ECHO_SYNTH}
  MM_ECHO_WAVEOUT                     = 2;
  {$EXTERNALSYM MM_ECHO_WAVEOUT}
  MM_ECHO_WAVEIN                      = 3;
  {$EXTERNALSYM MM_ECHO_WAVEIN}
  MM_ECHO_MIDIOUT                     = 4;
  {$EXTERNALSYM MM_ECHO_MIDIOUT}
  MM_ECHO_MIDIIN                      = 5;
  {$EXTERNALSYM MM_ECHO_MIDIIN}
  MM_ECHO_AUX                         = 6;
  {$EXTERNALSYM MM_ECHO_AUX}


  // MM_SIERRA product IDs
  // =====================
  MM_SIERRA_ARIA_MIDIOUT              = $14;
  {$EXTERNALSYM MM_SIERRA_ARIA_MIDIOUT}
  MM_SIERRA_ARIA_MIDIIN               = $15;
  {$EXTERNALSYM MM_SIERRA_ARIA_MIDIIN}
  MM_SIERRA_ARIA_SYNTH                = $16;
  {$EXTERNALSYM MM_SIERRA_ARIA_SYNTH}
  MM_SIERRA_ARIA_WAVEOUT              = $17;
  {$EXTERNALSYM MM_SIERRA_ARIA_WAVEOUT}
  MM_SIERRA_ARIA_WAVEIN               = $18;
  {$EXTERNALSYM MM_SIERRA_ARIA_WAVEIN}
  MM_SIERRA_ARIA_AUX                  = $19;
  {$EXTERNALSYM MM_SIERRA_ARIA_AUX}
  MM_SIERRA_ARIA_AUX2                 = $20;
  {$EXTERNALSYM MM_SIERRA_ARIA_AUX2}
  MM_SIERRA_QUARTET_WAVEIN            = $50;
  {$EXTERNALSYM MM_SIERRA_QUARTET_WAVEIN}
  MM_SIERRA_QUARTET_WAVEOUT           = $51;
  {$EXTERNALSYM MM_SIERRA_QUARTET_WAVEOUT}
  MM_SIERRA_QUARTET_MIDIIN            = $52;
  {$EXTERNALSYM MM_SIERRA_QUARTET_MIDIIN}
  MM_SIERRA_QUARTET_MIDIOUT           = $53;
  {$EXTERNALSYM MM_SIERRA_QUARTET_MIDIOUT}
  MM_SIERRA_QUARTET_SYNTH             = $54;
  {$EXTERNALSYM MM_SIERRA_QUARTET_SYNTH}
  MM_SIERRA_QUARTET_AUX_CD            = $55;
  {$EXTERNALSYM MM_SIERRA_QUARTET_AUX_CD}
  MM_SIERRA_QUARTET_AUX_LINE          = $56;
  {$EXTERNALSYM MM_SIERRA_QUARTET_AUX_LINE}
  MM_SIERRA_QUARTET_AUX_MODEM         = $57;
  {$EXTERNALSYM MM_SIERRA_QUARTET_AUX_MODEM}
  MM_SIERRA_QUARTET_MIXER             = $58;
  {$EXTERNALSYM MM_SIERRA_QUARTET_MIXER}


  // MM_CAT product IDs
  // ==================
  MM_CAT_WAVEOUT                      = 1;
  {$EXTERNALSYM MM_CAT_WAVEOUT}


  // MM_DSP_GROUP product IDs
  // ========================
  MM_DSP_GROUP_TRUESPEECH             = 1;
  {$EXTERNALSYM MM_DSP_GROUP_TRUESPEECH}


  // MM_MELABS product IDs
  // =====================
  MM_MELABS_MIDI2GO                   = 1;
  {$EXTERNALSYM MM_MELABS_MIDI2GO}


  // MM_ESS product IDs
  // ==================
  MM_ESS_AMWAVEOUT                    = $01;
  {$EXTERNALSYM MM_ESS_AMWAVEOUT}
  MM_ESS_AMWAVEIN                     = $02;
  {$EXTERNALSYM MM_ESS_AMWAVEIN}
  MM_ESS_AMAUX                        = $03;
  {$EXTERNALSYM MM_ESS_AMAUX}
  MM_ESS_AMSYNTH                      = $04;
  {$EXTERNALSYM MM_ESS_AMSYNTH}
  MM_ESS_AMMIDIOUT                    = $05;
  {$EXTERNALSYM MM_ESS_AMMIDIOUT}
  MM_ESS_AMMIDIIN                     = $06;
  {$EXTERNALSYM MM_ESS_AMMIDIIN}
  MM_ESS_MIXER                        = $07;
  {$EXTERNALSYM MM_ESS_MIXER}
  MM_ESS_AUX_CD                       = $08;
  {$EXTERNALSYM MM_ESS_AUX_CD}
  MM_ESS_MPU401_MIDIOUT               = $09;
  {$EXTERNALSYM MM_ESS_MPU401_MIDIOUT}
  MM_ESS_MPU401_MIDIIN                = $0A;
  {$EXTERNALSYM MM_ESS_MPU401_MIDIIN}
  MM_ESS_ES488_WAVEOUT                = $10;
  {$EXTERNALSYM MM_ESS_ES488_WAVEOUT}
  MM_ESS_ES488_WAVEIN                 = $11;
  {$EXTERNALSYM MM_ESS_ES488_WAVEIN}
  MM_ESS_ES488_MIXER                  = $12;
  {$EXTERNALSYM MM_ESS_ES488_MIXER}
  MM_ESS_ES688_WAVEOUT                = $13;
  {$EXTERNALSYM MM_ESS_ES688_WAVEOUT}
  MM_ESS_ES688_WAVEIN                 = $14;
  {$EXTERNALSYM MM_ESS_ES688_WAVEIN}
  MM_ESS_ES688_MIXER                  = $15;
  {$EXTERNALSYM MM_ESS_ES688_MIXER}
  MM_ESS_ES1488_WAVEOUT               = $16;
  {$EXTERNALSYM MM_ESS_ES1488_WAVEOUT}
  MM_ESS_ES1488_WAVEIN                = $17;
  {$EXTERNALSYM MM_ESS_ES1488_WAVEIN}
  MM_ESS_ES1488_MIXER                 = $18;
  {$EXTERNALSYM MM_ESS_ES1488_MIXER}
  MM_ESS_ES1688_WAVEOUT               = $19;
  {$EXTERNALSYM MM_ESS_ES1688_WAVEOUT}
  MM_ESS_ES1688_WAVEIN                = $1A;
  {$EXTERNALSYM MM_ESS_ES1688_WAVEIN}
  MM_ESS_ES1688_MIXER                 = $1B;
  {$EXTERNALSYM MM_ESS_ES1688_MIXER}
  MM_ESS_ES1788_WAVEOUT               = $1C;
  {$EXTERNALSYM MM_ESS_ES1788_WAVEOUT}
  MM_ESS_ES1788_WAVEIN                = $1D;
  {$EXTERNALSYM MM_ESS_ES1788_WAVEIN}
  MM_ESS_ES1788_MIXER                 = $1E;
  {$EXTERNALSYM MM_ESS_ES1788_MIXER}
  MM_ESS_ES1888_WAVEOUT               = $1F;
  {$EXTERNALSYM MM_ESS_ES1888_WAVEOUT}
  MM_ESS_ES1888_WAVEIN                = $20;
  {$EXTERNALSYM MM_ESS_ES1888_WAVEIN}
  MM_ESS_ES1888_MIXER                 = $21;
  {$EXTERNALSYM MM_ESS_ES1888_MIXER}
  MM_ESS_ES1868_WAVEOUT               = $22;
  {$EXTERNALSYM MM_ESS_ES1868_WAVEOUT}
  MM_ESS_ES1868_WAVEIN                = $23;
  {$EXTERNALSYM MM_ESS_ES1868_WAVEIN}
  MM_ESS_ES1868_MIXER                 = $24;
  {$EXTERNALSYM MM_ESS_ES1868_MIXER}
  MM_ESS_ES1878_WAVEOUT               = $25;
  {$EXTERNALSYM MM_ESS_ES1878_WAVEOUT}
  MM_ESS_ES1878_WAVEIN                = $26;
  {$EXTERNALSYM MM_ESS_ES1878_WAVEIN}
  MM_ESS_ES1878_MIXER                 = $27;
  {$EXTERNALSYM MM_ESS_ES1878_MIXER}


  // MM_CANOPUS product IDs
  // ======================
  MM_CANOPUS_ACM_DVREX                = 1;
  {$EXTERNALSYM MM_CANOPUS_ACM_DVREX}


  // MM_EPSON product IDs
  // ====================
  MM_EPS_FMSND                        = 1;
  {$EXTERNALSYM MM_EPS_FMSND}


  // MM_TRUEVISION product IDs
  // =========================
  MM_TRUEVISION_WAVEIN1               = 1;
  {$EXTERNALSYM MM_TRUEVISION_WAVEIN1}
  MM_TRUEVISION_WAVEOUT1              = 2;
  {$EXTERNALSYM MM_TRUEVISION_WAVEOUT1}


  // MM_AZTECH product IDs
  // =====================
  MM_AZTECH_MIDIOUT                   = 3;
  {$EXTERNALSYM MM_AZTECH_MIDIOUT}
  MM_AZTECH_MIDIIN                    = 4;
  {$EXTERNALSYM MM_AZTECH_MIDIIN}
  MM_AZTECH_WAVEIN                    = 17;
  {$EXTERNALSYM MM_AZTECH_WAVEIN}
  MM_AZTECH_WAVEOUT                   = 18;
  {$EXTERNALSYM MM_AZTECH_WAVEOUT}
  MM_AZTECH_FMSYNTH                   = 20;
  {$EXTERNALSYM MM_AZTECH_FMSYNTH}
  MM_AZTECH_MIXER                     = 21;
  {$EXTERNALSYM MM_AZTECH_MIXER}
  MM_AZTECH_PRO16_WAVEIN              = 33;
  {$EXTERNALSYM MM_AZTECH_PRO16_WAVEIN}
  MM_AZTECH_PRO16_WAVEOUT             = 34;
  {$EXTERNALSYM MM_AZTECH_PRO16_WAVEOUT}
  MM_AZTECH_PRO16_FMSYNTH             = 38;
  {$EXTERNALSYM MM_AZTECH_PRO16_FMSYNTH}
  MM_AZTECH_DSP16_WAVEIN              = 65;
  {$EXTERNALSYM MM_AZTECH_DSP16_WAVEIN}
  MM_AZTECH_DSP16_WAVEOUT             = 66;
  {$EXTERNALSYM MM_AZTECH_DSP16_WAVEOUT}
  MM_AZTECH_DSP16_FMSYNTH             = 68;
  {$EXTERNALSYM MM_AZTECH_DSP16_FMSYNTH}
  MM_AZTECH_DSP16_WAVESYNTH           = 70;
  {$EXTERNALSYM MM_AZTECH_DSP16_WAVESYNTH}
  MM_AZTECH_NOVA16_WAVEIN             = 71;
  {$EXTERNALSYM MM_AZTECH_NOVA16_WAVEIN}
  MM_AZTECH_NOVA16_WAVEOUT            = 72;
  {$EXTERNALSYM MM_AZTECH_NOVA16_WAVEOUT}
  MM_AZTECH_NOVA16_MIXER              = 73;
  {$EXTERNALSYM MM_AZTECH_NOVA16_MIXER}
  MM_AZTECH_WASH16_WAVEIN             = 74;
  {$EXTERNALSYM MM_AZTECH_WASH16_WAVEIN}
  MM_AZTECH_WASH16_WAVEOUT            = 75;
  {$EXTERNALSYM MM_AZTECH_WASH16_WAVEOUT}
  MM_AZTECH_WASH16_MIXER              = 76;
  {$EXTERNALSYM MM_AZTECH_WASH16_MIXER}
  MM_AZTECH_AUX_CD                    = 401;
  {$EXTERNALSYM MM_AZTECH_AUX_CD}
  MM_AZTECH_AUX_LINE                  = 402;
  {$EXTERNALSYM MM_AZTECH_AUX_LINE}
  MM_AZTECH_AUX_MIC                   = 403;
  {$EXTERNALSYM MM_AZTECH_AUX_MIC}
  MM_AZTECH_AUX                       = 404;
  {$EXTERNALSYM MM_AZTECH_AUX}


  // MM_VIDEOLOGIC product IDs
  // =========================
  MM_VIDEOLOGIC_MSWAVEIN              = 1;
  {$EXTERNALSYM MM_VIDEOLOGIC_MSWAVEIN}
  MM_VIDEOLOGIC_MSWAVEOUT             = 2;
  {$EXTERNALSYM MM_VIDEOLOGIC_MSWAVEOUT}


  // MM_KORG product IDs
  // ===================
  MM_KORG_PCIF_MIDIOUT                = 1;
  {$EXTERNALSYM MM_KORG_PCIF_MIDIOUT}
  MM_KORG_PCIF_MIDIIN                 = 2;
  {$EXTERNALSYM MM_KORG_PCIF_MIDIIN}
  MM_KORG_1212IO_MSWAVEIN             = 3;
  {$EXTERNALSYM MM_KORG_1212IO_MSWAVEIN}
  MM_KORG_1212IO_MSWAVEOUT            = 4;
  {$EXTERNALSYM MM_KORG_1212IO_MSWAVEOUT}


  // MM_APT product IDs
  // ==================
  MM_APT_ACE100CD                     = 1;
  {$EXTERNALSYM MM_APT_ACE100CD}


  // MM_ICS product IDs
  // ==================
  MM_ICS_WAVEDECK_WAVEOUT             = 1;  // MS WSS compatible card and driver
  {$EXTERNALSYM MM_ICS_WAVEDECK_WAVEOUT}
  MM_ICS_WAVEDECK_WAVEIN              = 2;
  {$EXTERNALSYM MM_ICS_WAVEDECK_WAVEIN}
  MM_ICS_WAVEDECK_MIXER               = 3;
  {$EXTERNALSYM MM_ICS_WAVEDECK_MIXER}
  MM_ICS_WAVEDECK_AUX                 = 4;
  {$EXTERNALSYM MM_ICS_WAVEDECK_AUX}
  MM_ICS_WAVEDECK_SYNTH               = 5;
  {$EXTERNALSYM MM_ICS_WAVEDECK_SYNTH}
  MM_ICS_WAVEDEC_SB_WAVEOUT           = 6;
  {$EXTERNALSYM MM_ICS_WAVEDEC_SB_WAVEOUT}
  MM_ICS_WAVEDEC_SB_WAVEIN            = 7;
  {$EXTERNALSYM MM_ICS_WAVEDEC_SB_WAVEIN}
  MM_ICS_WAVEDEC_SB_FM_MIDIOUT        = 8;
  {$EXTERNALSYM MM_ICS_WAVEDEC_SB_FM_MIDIOUT}
  MM_ICS_WAVEDEC_SB_MPU401_MIDIOUT    = 9;
  {$EXTERNALSYM MM_ICS_WAVEDEC_SB_MPU401_MIDIOUT}
  MM_ICS_WAVEDEC_SB_MPU401_MIDIIN     = 10;
  {$EXTERNALSYM MM_ICS_WAVEDEC_SB_MPU401_MIDIIN}
  MM_ICS_WAVEDEC_SB_MIXER             = 11;
  {$EXTERNALSYM MM_ICS_WAVEDEC_SB_MIXER}
  MM_ICS_WAVEDEC_SB_AUX               = 12;
  {$EXTERNALSYM MM_ICS_WAVEDEC_SB_AUX}
  MM_ICS_2115_LITE_MIDIOUT            = 13;
  {$EXTERNALSYM MM_ICS_2115_LITE_MIDIOUT}
  MM_ICS_2120_LITE_MIDIOUT            = 14;
  {$EXTERNALSYM MM_ICS_2120_LITE_MIDIOUT}


  // MM_ITERATEDSYS product IDs
  // ==========================
  MM_ITERATEDSYS_FUFCODEC             = 1;
  {$EXTERNALSYM MM_ITERATEDSYS_FUFCODEC}


  // MM_METHEUS product IDs
  // ======================
  MM_METHEUS_ZIPPER                   = 1;
  {$EXTERNALSYM MM_METHEUS_ZIPPER}


  // MM_WINNOV product IDs
  // =====================
  MM_WINNOV_CAVIAR_WAVEIN             = 1;
  {$EXTERNALSYM MM_WINNOV_CAVIAR_WAVEIN}
  MM_WINNOV_CAVIAR_WAVEOUT            = 2;
  {$EXTERNALSYM MM_WINNOV_CAVIAR_WAVEOUT}
  MM_WINNOV_CAVIAR_VIDC               = 3;
  {$EXTERNALSYM MM_WINNOV_CAVIAR_VIDC}
  MM_WINNOV_CAVIAR_CHAMPAGNE          = 4;  // Fourcc is CHAM
  {$EXTERNALSYM MM_WINNOV_CAVIAR_CHAMPAGNE}
  MM_WINNOV_CAVIAR_YUV8               = 5;  // Fourcc is YUV8
  {$EXTERNALSYM MM_WINNOV_CAVIAR_YUV8}


  // MM_NCR product IDs
  // ==================
  MM_NCR_BA_WAVEIN                    = 1;
  {$EXTERNALSYM MM_NCR_BA_WAVEIN}
  MM_NCR_BA_WAVEOUT                   = 2;
  {$EXTERNALSYM MM_NCR_BA_WAVEOUT}
  MM_NCR_BA_SYNTH                     = 3;
  {$EXTERNALSYM MM_NCR_BA_SYNTH}
  MM_NCR_BA_AUX                       = 4;
  {$EXTERNALSYM MM_NCR_BA_AUX}
  MM_NCR_BA_MIXER                     = 5;
  {$EXTERNALSYM MM_NCR_BA_MIXER}


  // MM_AST product IDs
  // ==================
  MM_AST_MODEMWAVE_WAVEIN             = 13;
  {$EXTERNALSYM MM_AST_MODEMWAVE_WAVEIN}
  MM_AST_MODEMWAVE_WAVEOUT            = 14;
  {$EXTERNALSYM MM_AST_MODEMWAVE_WAVEOUT}


  // MM_WILLOWPOND product IDs
  // =========================
  MM_WILLOWPOND_FMSYNTH_STEREO        = 20;
  {$EXTERNALSYM MM_WILLOWPOND_FMSYNTH_STEREO}
  MM_WILLOWPOND_MPU401                = 21;
  {$EXTERNALSYM MM_WILLOWPOND_MPU401}
  MM_WILLOWPOND_SNDPORT_WAVEIN        = 100;
  {$EXTERNALSYM MM_WILLOWPOND_SNDPORT_WAVEIN}
  MM_WILLOWPOND_SNDPORT_WAVEOUT       = 101;
  {$EXTERNALSYM MM_WILLOWPOND_SNDPORT_WAVEOUT}
  MM_WILLOWPOND_SNDPORT_MIXER         = 102;
  {$EXTERNALSYM MM_WILLOWPOND_SNDPORT_MIXER}
  MM_WILLOWPOND_SNDPORT_AUX           = 103;
  {$EXTERNALSYM MM_WILLOWPOND_SNDPORT_AUX}
  MM_WILLOWPOND_PH_WAVEIN             = 104;
  {$EXTERNALSYM MM_WILLOWPOND_PH_WAVEIN}
  MM_WILLOWPOND_PH_WAVEOUT            = 105;
  {$EXTERNALSYM MM_WILLOWPOND_PH_WAVEOUT}
  MM_WILLOWPOND_PH_MIXER              = 106;
  {$EXTERNALSYM MM_WILLOWPOND_PH_MIXER}
  MM_WILLOWPOND_PH_AUX                = 107;
  {$EXTERNALSYM MM_WILLOWPOND_PH_AUX}
  MM_WILLOPOND_SNDCOMM_WAVEIN         = 108;
  {$EXTERNALSYM MM_WILLOPOND_SNDCOMM_WAVEIN}
  MM_WILLOWPOND_SNDCOMM_WAVEOUT       = 109;
  {$EXTERNALSYM MM_WILLOWPOND_SNDCOMM_WAVEOUT}
  MM_WILLOWPOND_SNDCOMM_MIXER         = 110;
  {$EXTERNALSYM MM_WILLOWPOND_SNDCOMM_MIXER}
  MM_WILLOWPOND_SNDCOMM_AUX           = 111;
  {$EXTERNALSYM MM_WILLOWPOND_SNDCOMM_AUX}
  MM_WILLOWPOND_GENERIC_WAVEIN        = 112;
  {$EXTERNALSYM MM_WILLOWPOND_GENERIC_WAVEIN}
  MM_WILLOWPOND_GENERIC_WAVEOUT       = 113;
  {$EXTERNALSYM MM_WILLOWPOND_GENERIC_WAVEOUT}
  MM_WILLOWPOND_GENERIC_MIXER         = 114;
  {$EXTERNALSYM MM_WILLOWPOND_GENERIC_MIXER}
  MM_WILLOWPOND_GENERIC_AUX           = 115;
  {$EXTERNALSYM MM_WILLOWPOND_GENERIC_AUX}


  // MM_VITEC product IDs
  // ====================
  MM_VITEC_VMAKER                     = 1;
  {$EXTERNALSYM MM_VITEC_VMAKER}
  MM_VITEC_VMPRO                      = 2;
  {$EXTERNALSYM MM_VITEC_VMPRO}


  // MM_MOSCOM product IDs
  // =====================
  MM_MOSCOM_VPC2400_IN                = 1;  // Four Port Voice Processing / Voice Recognition Board
  {$EXTERNALSYM MM_MOSCOM_VPC2400_IN}
  MM_MOSCOM_VPC2400_OUT               = 2;  // VPC2400
  {$EXTERNALSYM MM_MOSCOM_VPC2400_OUT}


  // MM_SILICONSOFT product IDs
  // ==========================
  MM_SILICONSOFT_SC1_WAVEIN           = 1;  // Waveform in , high sample rate
  {$EXTERNALSYM MM_SILICONSOFT_SC1_WAVEIN}
  MM_SILICONSOFT_SC1_WAVEOUT          = 2;  // Waveform out , high sample rate
  {$EXTERNALSYM MM_SILICONSOFT_SC1_WAVEOUT}
  MM_SILICONSOFT_SC2_WAVEIN           = 3;  // Waveform in 2 channels, high sample rate
  {$EXTERNALSYM MM_SILICONSOFT_SC2_WAVEIN}
  MM_SILICONSOFT_SC2_WAVEOUT          = 4;  // Waveform out 2 channels, high sample rate
  {$EXTERNALSYM MM_SILICONSOFT_SC2_WAVEOUT}
  MM_SILICONSOFT_SOUNDJR2_WAVEOUT     = 5;  // Waveform out, self powered, efficient
  {$EXTERNALSYM MM_SILICONSOFT_SOUNDJR2_WAVEOUT}
  MM_SILICONSOFT_SOUNDJR2PR_WAVEIN    = 6;  // Waveform in, self powered, efficient
  {$EXTERNALSYM MM_SILICONSOFT_SOUNDJR2PR_WAVEIN}
  MM_SILICONSOFT_SOUNDJR2PR_WAVEOUT   = 7;  // Waveform out 2 channels, self powered, efficient
  {$EXTERNALSYM MM_SILICONSOFT_SOUNDJR2PR_WAVEOUT}
  MM_SILICONSOFT_SOUNDJR3_WAVEOUT     = 8;  // Waveform in 2 channels, self powered, efficient
  {$EXTERNALSYM MM_SILICONSOFT_SOUNDJR3_WAVEOUT}


  // MM_TERRATEC product IDs
  // =======================
  MM_TTEWS_WAVEIN                     = 1;
  {$EXTERNALSYM MM_TTEWS_WAVEIN}
  MM_TTEWS_WAVEOUT                    = 2;
  {$EXTERNALSYM MM_TTEWS_WAVEOUT}
  MM_TTEWS_MIDIIN                     = 3;
  {$EXTERNALSYM MM_TTEWS_MIDIIN}
  MM_TTEWS_MIDIOUT                    = 4;
  {$EXTERNALSYM MM_TTEWS_MIDIOUT}
  MM_TTEWS_MIDISYNTH                  = 5;
  {$EXTERNALSYM MM_TTEWS_MIDISYNTH}
  MM_TTEWS_MIDIMONITOR                = 6;
  {$EXTERNALSYM MM_TTEWS_MIDIMONITOR}
  MM_TTEWS_VMIDIIN                    = 7;
  {$EXTERNALSYM MM_TTEWS_VMIDIIN}
  MM_TTEWS_VMIDIOUT                   = 8;
  {$EXTERNALSYM MM_TTEWS_VMIDIOUT}
  MM_TTEWS_AUX                        = 9;
  {$EXTERNALSYM MM_TTEWS_AUX}
  MM_TTEWS_MIXER                      = 10;
  {$EXTERNALSYM MM_TTEWS_MIXER}


  // MM_MEDIASONIC product IDs
  // =========================
  MM_MEDIASONIC_ACM_G723              = 1;
  {$EXTERNALSYM MM_MEDIASONIC_ACM_G723}
  MM_MEDIASONIC_ICOM                  = 2;
  {$EXTERNALSYM MM_MEDIASONIC_ICOM}
  MM_ICOM_WAVEIN                      = 3;
  {$EXTERNALSYM MM_ICOM_WAVEIN}
  MM_ICOM_WAVEOUT                     = 4;
  {$EXTERNALSYM MM_ICOM_WAVEOUT}
  MM_ICOM_MIXER                       = 5;
  {$EXTERNALSYM MM_ICOM_MIXER}
  MM_ICOM_AUX                         = 6;
  {$EXTERNALSYM MM_ICOM_AUX}
  MM_ICOM_LINE                        = 7;
  {$EXTERNALSYM MM_ICOM_LINE}


  //  MM_SANYO product IDs
  // =====================
  MM_SANYO_ACM_LD_ADPCM               = 1;
  {$EXTERNALSYM MM_SANYO_ACM_LD_ADPCM}


  // MM_AHEAD product IDs
  // ====================
  MM_AHEAD_MULTISOUND                 = 1;
  {$EXTERNALSYM MM_AHEAD_MULTISOUND}
  MM_AHEAD_SOUNDBLASTER               = 2;
  {$EXTERNALSYM MM_AHEAD_SOUNDBLASTER}
  MM_AHEAD_PROAUDIO                   = 3;
  {$EXTERNALSYM MM_AHEAD_PROAUDIO}
  MM_AHEAD_GENERIC                    = 4;
  {$EXTERNALSYM MM_AHEAD_GENERIC}


  // MM_OLIVETTI product IDs
  // =======================
  MM_OLIVETTI_WAVEIN                  = 1;
  {$EXTERNALSYM MM_OLIVETTI_WAVEIN}
  MM_OLIVETTI_WAVEOUT                 = 2;
  {$EXTERNALSYM MM_OLIVETTI_WAVEOUT}
  MM_OLIVETTI_MIXER                   = 3;
  {$EXTERNALSYM MM_OLIVETTI_MIXER}
  MM_OLIVETTI_AUX                     = 4;
  {$EXTERNALSYM MM_OLIVETTI_AUX}
  MM_OLIVETTI_MIDIIN                  = 5;
  {$EXTERNALSYM MM_OLIVETTI_MIDIIN}
  MM_OLIVETTI_MIDIOUT                 = 6;
  {$EXTERNALSYM MM_OLIVETTI_MIDIOUT}
  MM_OLIVETTI_SYNTH                   = 7;
  {$EXTERNALSYM MM_OLIVETTI_SYNTH}
  MM_OLIVETTI_JOYSTICK                = 8;
  {$EXTERNALSYM MM_OLIVETTI_JOYSTICK}
  MM_OLIVETTI_ACM_GSM                 = 9;
  {$EXTERNALSYM MM_OLIVETTI_ACM_GSM}
  MM_OLIVETTI_ACM_ADPCM               = 10;
  {$EXTERNALSYM MM_OLIVETTI_ACM_ADPCM}
  MM_OLIVETTI_ACM_CELP                = 11;
  {$EXTERNALSYM MM_OLIVETTI_ACM_CELP}
  MM_OLIVETTI_ACM_SBC                 = 12;
  {$EXTERNALSYM MM_OLIVETTI_ACM_SBC}
  MM_OLIVETTI_ACM_OPR                 = 13;
  {$EXTERNALSYM MM_OLIVETTI_ACM_OPR}


  // MM_IOMAGIC product IDs
  // ======================
  MM_IOMAGIC_TEMPO_WAVEOUT            = 1;
  {$EXTERNALSYM MM_IOMAGIC_TEMPO_WAVEOUT}
  MM_IOMAGIC_TEMPO_WAVEIN             = 2;
  {$EXTERNALSYM MM_IOMAGIC_TEMPO_WAVEIN}
  MM_IOMAGIC_TEMPO_SYNTH              = 3;
  {$EXTERNALSYM MM_IOMAGIC_TEMPO_SYNTH}
  MM_IOMAGIC_TEMPO_MIDIOUT            = 4;
  {$EXTERNALSYM MM_IOMAGIC_TEMPO_MIDIOUT}
  MM_IOMAGIC_TEMPO_MXDOUT             = 5;
  {$EXTERNALSYM MM_IOMAGIC_TEMPO_MXDOUT}
  MM_IOMAGIC_TEMPO_AUXOUT             = 6;
  {$EXTERNALSYM MM_IOMAGIC_TEMPO_AUXOUT}


  // MM_MATSUSHITA product IDs
  // =========================
  MM_MATSUSHITA_WAVEIN                = 1;
  {$EXTERNALSYM MM_MATSUSHITA_WAVEIN}
  MM_MATSUSHITA_WAVEOUT               = 2;
  {$EXTERNALSYM MM_MATSUSHITA_WAVEOUT}
  MM_MATSUSHITA_FMSYNTH_STEREO        = 3;
  {$EXTERNALSYM MM_MATSUSHITA_FMSYNTH_STEREO}
  MM_MATSUSHITA_MIXER                 = 4;
  {$EXTERNALSYM MM_MATSUSHITA_MIXER}
  MM_MATSUSHITA_AUX                   = 5;
  {$EXTERNALSYM MM_MATSUSHITA_AUX}


  // MM_NEWMEDIA product IDs
  // =======================
  MM_NEWMEDIA_WAVJAMMER               = 1;  // WSS Compatible sound card.
  {$EXTERNALSYM MM_NEWMEDIA_WAVJAMMER}


  // MM_LYRRUS product IDs
  // =====================
  MM_LYRRUS_BRIDGE_GUITAR             = 1;
  {$EXTERNALSYM MM_LYRRUS_BRIDGE_GUITAR}


  // MM_OPTI product IDs
  // ===================
  MM_OPTI_M16_FMSYNTH_STEREO          = $0001;
  {$EXTERNALSYM MM_OPTI_M16_FMSYNTH_STEREO}
  MM_OPTI_M16_MIDIIN                  = $0002;
  {$EXTERNALSYM MM_OPTI_M16_MIDIIN}
  MM_OPTI_M16_MIDIOUT                 = $0003;
  {$EXTERNALSYM MM_OPTI_M16_MIDIOUT}
  MM_OPTI_M16_WAVEIN                  = $0004;
  {$EXTERNALSYM MM_OPTI_M16_WAVEIN}
  MM_OPTI_M16_WAVEOUT                 = $0005;
  {$EXTERNALSYM MM_OPTI_M16_WAVEOUT}
  MM_OPTI_M16_MIXER                   = $0006;
  {$EXTERNALSYM MM_OPTI_M16_MIXER}
  MM_OPTI_M16_AUX                     = $0007;
  {$EXTERNALSYM MM_OPTI_M16_AUX}
  MM_OPTI_P16_FMSYNTH_STEREO          = $0010;
  {$EXTERNALSYM MM_OPTI_P16_FMSYNTH_STEREO}
  MM_OPTI_P16_MIDIIN                  = $0011;
  {$EXTERNALSYM MM_OPTI_P16_MIDIIN}
  MM_OPTI_P16_MIDIOUT                 = $0012;
  {$EXTERNALSYM MM_OPTI_P16_MIDIOUT}
  MM_OPTI_P16_WAVEIN                  = $0013;
  {$EXTERNALSYM MM_OPTI_P16_WAVEIN}
  MM_OPTI_P16_WAVEOUT                 = $0014;
  {$EXTERNALSYM MM_OPTI_P16_WAVEOUT}
  MM_OPTI_P16_MIXER                   = $0015;
  {$EXTERNALSYM MM_OPTI_P16_MIXER}
  MM_OPTI_P16_AUX                     = $0016;
  {$EXTERNALSYM MM_OPTI_P16_AUX}
  MM_OPTI_M32_WAVEIN                  = $0020;
  {$EXTERNALSYM MM_OPTI_M32_WAVEIN}
  MM_OPTI_M32_WAVEOUT                 = $0021;
  {$EXTERNALSYM MM_OPTI_M32_WAVEOUT}
  MM_OPTI_M32_MIDIIN                  = $0022;
  {$EXTERNALSYM MM_OPTI_M32_MIDIIN}
  MM_OPTI_M32_MIDIOUT                 = $0023;
  {$EXTERNALSYM MM_OPTI_M32_MIDIOUT}
  MM_OPTI_M32_SYNTH_STEREO            = $0024;
  {$EXTERNALSYM MM_OPTI_M32_SYNTH_STEREO}
  MM_OPTI_M32_MIXER                   = $0025;
  {$EXTERNALSYM MM_OPTI_M32_MIXER}
  MM_OPTI_M32_AUX                     = $0026;
  {$EXTERNALSYM MM_OPTI_M32_AUX}


  // MM_COMPAQ product IDs
  // =====================
  MM_COMPAQ_BB_WAVEIN                 = 1;
  {$EXTERNALSYM MM_COMPAQ_BB_WAVEIN}
  MM_COMPAQ_BB_WAVEOUT                = 2;
  {$EXTERNALSYM MM_COMPAQ_BB_WAVEOUT}
  MM_COMPAQ_BB_WAVEAUX                = 3;
  {$EXTERNALSYM MM_COMPAQ_BB_WAVEAUX}


  // MM_MPTUS product IDs
  // ====================
  MM_MPTUS_SPWAVEOUT                  = 1;  // Sound Pallette
  {$EXTERNALSYM MM_MPTUS_SPWAVEOUT}


  // MM_LERNOUT_AND_HAUSPIE product IDs
  // ==================================
  MM_LERNOUT_ANDHAUSPIE_LHCODECACM    = 1;
  {$EXTERNALSYM MM_LERNOUT_ANDHAUSPIE_LHCODECACM}


  // MM_DIGITAL product IDs
  // ======================
  MM_DIGITAL_AV320_WAVEIN             = 1;  // Digital Audio Video Compression Board
  {$EXTERNALSYM MM_DIGITAL_AV320_WAVEIN}
  MM_DIGITAL_AV320_WAVEOUT            = 2;  // Digital Audio Video Compression Board
  {$EXTERNALSYM MM_DIGITAL_AV320_WAVEOUT}
  MM_DIGITAL_ACM_G723                 = 3;
  {$EXTERNALSYM MM_DIGITAL_ACM_G723}
  MM_DIGITAL_ICM_H263                 = 4;
  {$EXTERNALSYM MM_DIGITAL_ICM_H263}
  MM_DIGITAL_ICM_H261                 = 5;
  {$EXTERNALSYM MM_DIGITAL_ICM_H261}


  // MM_MOTU product IDs
  // ===================
  MM_MOTU_MTP_MIDIOUT_ALL             = 100;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_ALL}
  MM_MOTU_MTP_MIDIIN_1                = 101;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_1}
  MM_MOTU_MTP_MIDIOUT_1               = 101;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_1}
  MM_MOTU_MTP_MIDIIN_2                = 102;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_2}
  MM_MOTU_MTP_MIDIOUT_2               = 102;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_2}
  MM_MOTU_MTP_MIDIIN_3                = 103;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_3}
  MM_MOTU_MTP_MIDIOUT_3               = 103;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_3}
  MM_MOTU_MTP_MIDIIN_4                = 104;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_4}
  MM_MOTU_MTP_MIDIOUT_4               = 104;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_4}
  MM_MOTU_MTP_MIDIIN_5                = 105;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_5}
  MM_MOTU_MTP_MIDIOUT_5               = 105;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_5}
  MM_MOTU_MTP_MIDIIN_6                = 106;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_6}
  MM_MOTU_MTP_MIDIOUT_6               = 106;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_6}
  MM_MOTU_MTP_MIDIIN_7                = 107;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_7}
  MM_MOTU_MTP_MIDIOUT_7               = 107;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_7}
  MM_MOTU_MTP_MIDIIN_8                = 108;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIIN_8}
  MM_MOTU_MTP_MIDIOUT_8               = 108;
  {$EXTERNALSYM MM_MOTU_MTP_MIDIOUT_8}

  MM_MOTU_MTPII_MIDIOUT_ALL           = 200;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_ALL}
  MM_MOTU_MTPII_MIDIIN_SYNC           = 200;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_SYNC}
  MM_MOTU_MTPII_MIDIIN_1              = 201;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_1}
  MM_MOTU_MTPII_MIDIOUT_1             = 201;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_1}
  MM_MOTU_MTPII_MIDIIN_2              = 202;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_2}
  MM_MOTU_MTPII_MIDIOUT_2             = 202;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_2}
  MM_MOTU_MTPII_MIDIIN_3              = 203;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_3}
  MM_MOTU_MTPII_MIDIOUT_3             = 203;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_3}
  MM_MOTU_MTPII_MIDIIN_4              = 204;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_4}
  MM_MOTU_MTPII_MIDIOUT_4             = 204;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_4}
  MM_MOTU_MTPII_MIDIIN_5              = 205;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_5}
  MM_MOTU_MTPII_MIDIOUT_5             = 205;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_5}
  MM_MOTU_MTPII_MIDIIN_6              = 206;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_6}
  MM_MOTU_MTPII_MIDIOUT_6             = 206;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_6}
  MM_MOTU_MTPII_MIDIIN_7              = 207;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_7}
  MM_MOTU_MTPII_MIDIOUT_7             = 207;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_7}
  MM_MOTU_MTPII_MIDIIN_8              = 208;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIIN_8}
  MM_MOTU_MTPII_MIDIOUT_8             = 208;
  {$EXTERNALSYM MM_MOTU_MTPII_MIDIOUT_8}
  MM_MOTU_MTPII_NET_MIDIIN_1          = 209;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_1}
  MM_MOTU_MTPII_NET_MIDIOUT_1         = 209;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_1}
  MM_MOTU_MTPII_NET_MIDIIN_2          = 210;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_2}
  MM_MOTU_MTPII_NET_MIDIOUT_2         = 210;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_2}
  MM_MOTU_MTPII_NET_MIDIIN_3          = 211;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_3}
  MM_MOTU_MTPII_NET_MIDIOUT_3         = 211;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_3}
  MM_MOTU_MTPII_NET_MIDIIN_4          = 212;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_4}
  MM_MOTU_MTPII_NET_MIDIOUT_4         = 212;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_4}
  MM_MOTU_MTPII_NET_MIDIIN_5          = 213;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_5}
  MM_MOTU_MTPII_NET_MIDIOUT_5         = 213;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_5}
  MM_MOTU_MTPII_NET_MIDIIN_6          = 214;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_6}
  MM_MOTU_MTPII_NET_MIDIOUT_6         = 214;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_6}
  MM_MOTU_MTPII_NET_MIDIIN_7          = 215;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_7}
  MM_MOTU_MTPII_NET_MIDIOUT_7         = 215;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_7}
  MM_MOTU_MTPII_NET_MIDIIN_8          = 216;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIIN_8}
  MM_MOTU_MTPII_NET_MIDIOUT_8         = 216;
  {$EXTERNALSYM MM_MOTU_MTPII_NET_MIDIOUT_8}

  MM_MOTU_MXP_MIDIIN_MIDIOUT_ALL      = 300;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIOUT_ALL}
  MM_MOTU_MXP_MIDIIN_SYNC             = 300;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_SYNC}
  MM_MOTU_MXP_MIDIIN_MIDIIN_1         = 301;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIIN_1}
  MM_MOTU_MXP_MIDIIN_MIDIOUT_1        = 301;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIOUT_1}
  MM_MOTU_MXP_MIDIIN_MIDIIN_2         = 302;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIIN_2}
  MM_MOTU_MXP_MIDIIN_MIDIOUT_2        = 302;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIOUT_2}
  MM_MOTU_MXP_MIDIIN_MIDIIN_3         = 303;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIIN_3}
  MM_MOTU_MXP_MIDIIN_MIDIOUT_3        = 303;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIOUT_3}
  MM_MOTU_MXP_MIDIIN_MIDIIN_4         = 304;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIIN_4}
  MM_MOTU_MXP_MIDIIN_MIDIOUT_4        = 304;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIOUT_4}
  MM_MOTU_MXP_MIDIIN_MIDIIN_5         = 305;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIIN_5}
  MM_MOTU_MXP_MIDIIN_MIDIOUT_5        = 305;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIOUT_5}
  MM_MOTU_MXP_MIDIIN_MIDIIN_6         = 306;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIIN_6}
  MM_MOTU_MXP_MIDIIN_MIDIOUT_6        = 306;
  {$EXTERNALSYM MM_MOTU_MXP_MIDIIN_MIDIOUT_6}

  MM_MOTU_MXPMPU_MIDIOUT_ALL          = 400;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIOUT_ALL}
  MM_MOTU_MXPMPU_MIDIIN_SYNC          = 400;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIIN_SYNC}
  MM_MOTU_MXPMPU_MIDIIN_1             = 401;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIIN_1}
  MM_MOTU_MXPMPU_MIDIOUT_1            = 401;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIOUT_1}
  MM_MOTU_MXPMPU_MIDIIN_2             = 402;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIIN_2}
  MM_MOTU_MXPMPU_MIDIOUT_2            = 402;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIOUT_2}
  MM_MOTU_MXPMPU_MIDIIN_3             = 403;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIIN_3}
  MM_MOTU_MXPMPU_MIDIOUT_3            = 403;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIOUT_3}
  MM_MOTU_MXPMPU_MIDIIN_4             = 404;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIIN_4}
  MM_MOTU_MXPMPU_MIDIOUT_4            = 404;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIOUT_4}
  MM_MOTU_MXPMPU_MIDIIN_5             = 405;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIIN_5}
  MM_MOTU_MXPMPU_MIDIOUT_5            = 405;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIOUT_5}
  MM_MOTU_MXPMPU_MIDIIN_6             = 406;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIIN_6}
  MM_MOTU_MXPMPU_MIDIOUT_6            = 406;
  {$EXTERNALSYM MM_MOTU_MXPMPU_MIDIOUT_6}

  MM_MOTU_MXN_MIDIOUT_ALL             = 500;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIOUT_ALL}
  MM_MOTU_MXN_MIDIIN_SYNC             = 500;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIIN_SYNC}
  MM_MOTU_MXN_MIDIIN_1                = 501;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIIN_1}
  MM_MOTU_MXN_MIDIOUT_1               = 501;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIOUT_1}
  MM_MOTU_MXN_MIDIIN_2                = 502;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIIN_2}
  MM_MOTU_MXN_MIDIOUT_2               = 502;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIOUT_2}
  MM_MOTU_MXN_MIDIIN_3                = 503;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIIN_3}
  MM_MOTU_MXN_MIDIOUT_3               = 503;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIOUT_3}
  MM_MOTU_MXN_MIDIIN_4                = 504;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIIN_4}
  MM_MOTU_MXN_MIDIOUT_4               = 504;
  {$EXTERNALSYM MM_MOTU_MXN_MIDIOUT_4}

  MM_MOTU_FLYER_MIDI_IN_SYNC          = 600;
  {$EXTERNALSYM MM_MOTU_FLYER_MIDI_IN_SYNC}
  MM_MOTU_FLYER_MIDI_IN_A             = 601;
  {$EXTERNALSYM MM_MOTU_FLYER_MIDI_IN_A}
  MM_MOTU_FLYER_MIDI_OUT_A            = 601;
  {$EXTERNALSYM MM_MOTU_FLYER_MIDI_OUT_A}
  MM_MOTU_FLYER_MIDI_IN_B             = 602;
  {$EXTERNALSYM MM_MOTU_FLYER_MIDI_IN_B}
  MM_MOTU_FLYER_MIDI_OUT_B            = 602;
  {$EXTERNALSYM MM_MOTU_FLYER_MIDI_OUT_B}

  MM_MOTU_PKX_MIDI_IN_SYNC            = 700;
  {$EXTERNALSYM MM_MOTU_PKX_MIDI_IN_SYNC}
  MM_MOTU_PKX_MIDI_IN_A               = 701;
  {$EXTERNALSYM MM_MOTU_PKX_MIDI_IN_A}
  MM_MOTU_PKX_MIDI_OUT_A              = 701;
  {$EXTERNALSYM MM_MOTU_PKX_MIDI_OUT_A}
  MM_MOTU_PKX_MIDI_IN_B               = 702;
  {$EXTERNALSYM MM_MOTU_PKX_MIDI_IN_B}
  MM_MOTU_PKX_MIDI_OUT_B              = 702;
  {$EXTERNALSYM MM_MOTU_PKX_MIDI_OUT_B}

  MM_MOTU_DTX_MIDI_IN_SYNC            = 800;
  {$EXTERNALSYM MM_MOTU_DTX_MIDI_IN_SYNC}
  MM_MOTU_DTX_MIDI_IN_A               = 801;
  {$EXTERNALSYM MM_MOTU_DTX_MIDI_IN_A}
  MM_MOTU_DTX_MIDI_OUT_A              = 801;
  {$EXTERNALSYM MM_MOTU_DTX_MIDI_OUT_A}
  MM_MOTU_DTX_MIDI_IN_B               = 802;
  {$EXTERNALSYM MM_MOTU_DTX_MIDI_IN_B}
  MM_MOTU_DTX_MIDI_OUT_B              = 802;
  {$EXTERNALSYM MM_MOTU_DTX_MIDI_OUT_B}

  MM_MOTU_MTPAV_MIDIOUT_ALL           = 900;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_ALL}
  MM_MOTU_MTPAV_MIDIIN_SYNC           = 900;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_SYNC}
  MM_MOTU_MTPAV_MIDIIN_1              = 901;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_1}
  MM_MOTU_MTPAV_MIDIOUT_1             = 901;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_1}
  MM_MOTU_MTPAV_MIDIIN_2              = 902;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_2}
  MM_MOTU_MTPAV_MIDIOUT_2             = 902;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_2}
  MM_MOTU_MTPAV_MIDIIN_3              = 903;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_3}
  MM_MOTU_MTPAV_MIDIOUT_3             = 903;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_3}
  MM_MOTU_MTPAV_MIDIIN_4              = 904;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_4}
  MM_MOTU_MTPAV_MIDIOUT_4             = 904;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_4}
  MM_MOTU_MTPAV_MIDIIN_5              = 905;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_5}
  MM_MOTU_MTPAV_MIDIOUT_5             = 905;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_5}
  MM_MOTU_MTPAV_MIDIIN_6              = 906;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_6}
  MM_MOTU_MTPAV_MIDIOUT_6             = 906;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_6}
  MM_MOTU_MTPAV_MIDIIN_7              = 907;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_7}
  MM_MOTU_MTPAV_MIDIOUT_7             = 907;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_7}
  MM_MOTU_MTPAV_MIDIIN_8              = 908;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_8}
  MM_MOTU_MTPAV_MIDIOUT_8             = 908;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_8}
  MM_MOTU_MTPAV_NET_MIDIIN_1          = 909;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_1}
  MM_MOTU_MTPAV_NET_MIDIOUT_1         = 909;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_1}
  MM_MOTU_MTPAV_NET_MIDIIN_2          = 910;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_2}
  MM_MOTU_MTPAV_NET_MIDIOUT_2         = 910;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_2}
  MM_MOTU_MTPAV_NET_MIDIIN_3          = 911;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_3}
  MM_MOTU_MTPAV_NET_MIDIOUT_3         = 911;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_3}
  MM_MOTU_MTPAV_NET_MIDIIN_4          = 912;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_4}
  MM_MOTU_MTPAV_NET_MIDIOUT_4         = 912;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_4}
  MM_MOTU_MTPAV_NET_MIDIIN_5          = 913;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_5}
  MM_MOTU_MTPAV_NET_MIDIOUT_5         = 913;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_5}
  MM_MOTU_MTPAV_NET_MIDIIN_6          = 914;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_6}
  MM_MOTU_MTPAV_NET_MIDIOUT_6         = 914;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_6}
  MM_MOTU_MTPAV_NET_MIDIIN_7          = 915;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_7}
  MM_MOTU_MTPAV_NET_MIDIOUT_7         = 915;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_7}
  MM_MOTU_MTPAV_NET_MIDIIN_8          = 916;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIIN_8}
  MM_MOTU_MTPAV_NET_MIDIOUT_8         = 916;
  {$EXTERNALSYM MM_MOTU_MTPAV_NET_MIDIOUT_8}
  MM_MOTU_MTPAV_MIDIIN_ADAT           = 917;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIIN_ADAT}
  MM_MOTU_MTPAV_MIDIOUT_ADAT          = 917;
  {$EXTERNALSYM MM_MOTU_MTPAV_MIDIOUT_ADAT}
  MM_MOTU_MXPXT_MIDIIN_SYNC           = 1000;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_SYNC}
  MM_MOTU_MXPXT_MIDIOUT_ALL           = 1000;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_ALL}
  MM_MOTU_MXPXT_MIDIIN_1              = 1001;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_1}
  MM_MOTU_MXPXT_MIDIOUT_1             = 1001;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_1}
  MM_MOTU_MXPXT_MIDIOUT_2             = 1002;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_2}
  MM_MOTU_MXPXT_MIDIIN_2              = 1002;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_2}
  MM_MOTU_MXPXT_MIDIIN_3              = 1003;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_3}
  MM_MOTU_MXPXT_MIDIOUT_3             = 1003;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_3}
  MM_MOTU_MXPXT_MIDIIN_4              = 1004;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_4}
  MM_MOTU_MXPXT_MIDIOUT_4             = 1004;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_4}
  MM_MOTU_MXPXT_MIDIIN_5              = 1005;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_5}
  MM_MOTU_MXPXT_MIDIOUT_5             = 1005;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_5}
  MM_MOTU_MXPXT_MIDIOUT_6             = 1006;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_6}
  MM_MOTU_MXPXT_MIDIIN_6              = 1006;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_6}
  MM_MOTU_MXPXT_MIDIOUT_7             = 1007;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_7}
  MM_MOTU_MXPXT_MIDIIN_7              = 1007;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_7}
  MM_MOTU_MXPXT_MIDIOUT_8             = 1008;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIOUT_8}
  MM_MOTU_MXPXT_MIDIIN_8              = 1008;
  {$EXTERNALSYM MM_MOTU_MXPXT_MIDIIN_8}


  // MM_WORKBIT product IDs
  // ======================
  MM_WORKBIT_MIXER                    = 1;  // Harmony Mixer
  {$EXTERNALSYM MM_WORKBIT_MIXER}
  MM_WORKBIT_WAVEOUT                  = 2;  // Harmony Mixer
  {$EXTERNALSYM MM_WORKBIT_WAVEOUT}
  MM_WORKBIT_WAVEIN                   = 3;  // Harmony Mixer
  {$EXTERNALSYM MM_WORKBIT_WAVEIN}
  MM_WORKBIT_MIDIIN                   = 4;  // Harmony Mixer
  {$EXTERNALSYM MM_WORKBIT_MIDIIN}
  MM_WORKBIT_MIDIOUT                  = 5;  // Harmony Mixer
  {$EXTERNALSYM MM_WORKBIT_MIDIOUT}
  MM_WORKBIT_FMSYNTH                  = 6;  // Harmony Mixer
  {$EXTERNALSYM MM_WORKBIT_FMSYNTH}
  MM_WORKBIT_AUX                      = 7;  // Harmony Mixer
  {$EXTERNALSYM MM_WORKBIT_AUX}
  MM_WORKBIT_JOYSTICK                 = 8;
  {$EXTERNALSYM MM_WORKBIT_JOYSTICK}


  // MM_OSITECH product IDs
  // ======================
  MM_OSITECH_TRUMPCARD                = 1;  // Trumpcard
  {$EXTERNALSYM MM_OSITECH_TRUMPCARD}


  // MM_MIRO product IDs
  // ===================
  MM_MIRO_MOVIEPRO                    = 1;  // miroMOVIE pro
  {$EXTERNALSYM MM_MIRO_MOVIEPRO}
  MM_MIRO_VIDEOD1                     = 2;  // miroVIDEO D1
  {$EXTERNALSYM MM_MIRO_VIDEOD1}
  MM_MIRO_VIDEODC1TV                  = 3;  // miroVIDEO DC1 tv
  {$EXTERNALSYM MM_MIRO_VIDEODC1TV}
  MM_MIRO_VIDEOTD                     = 4;  // miroVIDEO 10/20 TD
  {$EXTERNALSYM MM_MIRO_VIDEOTD}
  MM_MIRO_DC30_WAVEOUT                = 5;
  {$EXTERNALSYM MM_MIRO_DC30_WAVEOUT}
  MM_MIRO_DC30_WAVEIN                 = 6;
  {$EXTERNALSYM MM_MIRO_DC30_WAVEIN}
  MM_MIRO_DC30_MIX                    = 7;
  {$EXTERNALSYM MM_MIRO_DC30_MIX}


  // MM_ISOLUTION product IDs
  // ========================
  MM_ISOLUTION_PASCAL                 = 1;
  {$EXTERNALSYM MM_ISOLUTION_PASCAL}


  // MM_ROCKWELL product IDs
  // =======================
  MM_VOICEMIXER                       = 1;
  {$EXTERNALSYM MM_VOICEMIXER}
  ROCKWELL_WA1_WAVEIN                 = 100;
  {$EXTERNALSYM ROCKWELL_WA1_WAVEIN}
  ROCKWELL_WA1_WAVEOUT                = 101;
  {$EXTERNALSYM ROCKWELL_WA1_WAVEOUT}
  ROCKWELL_WA1_SYNTH                  = 102;
  {$EXTERNALSYM ROCKWELL_WA1_SYNTH}
  ROCKWELL_WA1_MIXER                  = 103;
  {$EXTERNALSYM ROCKWELL_WA1_MIXER}
  ROCKWELL_WA1_MPU401_IN              = 104;
  {$EXTERNALSYM ROCKWELL_WA1_MPU401_IN}
  ROCKWELL_WA1_MPU401_OUT             = 105;
  {$EXTERNALSYM ROCKWELL_WA1_MPU401_OUT}
  ROCKWELL_WA2_WAVEIN                 = 200;
  {$EXTERNALSYM ROCKWELL_WA2_WAVEIN}
  ROCKWELL_WA2_WAVEOUT                = 201;
  {$EXTERNALSYM ROCKWELL_WA2_WAVEOUT}
  ROCKWELL_WA2_SYNTH                  = 202;
  {$EXTERNALSYM ROCKWELL_WA2_SYNTH}
  ROCKWELL_WA2_MIXER                  = 203;
  {$EXTERNALSYM ROCKWELL_WA2_MIXER}
  ROCKWELL_WA2_MPU401_IN              = 204;
  {$EXTERNALSYM ROCKWELL_WA2_MPU401_IN}
  ROCKWELL_WA2_MPU401_OUT             = 205;
  {$EXTERNALSYM ROCKWELL_WA2_MPU401_OUT}


  // MM_VOXWARE product IDs
  // ======================
  MM_VOXWARE_CODEC                    = 1;
  {$EXTERNALSYM MM_VOXWARE_CODEC}


  // MM_NORTHERN_TELECOM product IDs
  // ===============================
  MM_NORTEL_MPXAC_WAVEIN              = 1;  // MPX Audio Card Wave Input Device
  {$EXTERNALSYM MM_NORTEL_MPXAC_WAVEIN}
  MM_NORTEL_MPXAC_WAVEOUT             = 2;  // MPX Audio Card Wave Output Device
  {$EXTERNALSYM MM_NORTEL_MPXAC_WAVEOUT}


  // MM_ADDX product IDs
  // ===================
  MM_ADDX_PCTV_DIGITALMIX             = 1;  // MM_ADDX_PCTV_DIGITALMIX
  {$EXTERNALSYM MM_ADDX_PCTV_DIGITALMIX}
  MM_ADDX_PCTV_WAVEIN                 = 2;  // MM_ADDX_PCTV_WAVEIN
  {$EXTERNALSYM MM_ADDX_PCTV_WAVEIN}
  MM_ADDX_PCTV_WAVEOUT                = 3;  // MM_ADDX_PCTV_WAVEOUT
  {$EXTERNALSYM MM_ADDX_PCTV_WAVEOUT}
  MM_ADDX_PCTV_MIXER                  = 4;  // MM_ADDX_PCTV_MIXER
  {$EXTERNALSYM MM_ADDX_PCTV_MIXER}
  MM_ADDX_PCTV_AUX_CD                 = 5;  // MM_ADDX_PCTV_AUX_CD
  {$EXTERNALSYM MM_ADDX_PCTV_AUX_CD}
  MM_ADDX_PCTV_AUX_LINE               = 6;  // MM_ADDX_PCTV_AUX_LINE
  {$EXTERNALSYM MM_ADDX_PCTV_AUX_LINE}


  // MM_WILDCAT product IDs
  // ======================
  MM_WILDCAT_AUTOSCOREMIDIIN          = 1;  // Autoscore
  {$EXTERNALSYM MM_WILDCAT_AUTOSCOREMIDIIN}


  // MM_RHETOREX product IDs
  // =======================
  MM_RHETOREX_WAVEIN                  = 1;
  {$EXTERNALSYM MM_RHETOREX_WAVEIN}
  MM_RHETOREX_WAVEOUT                 = 2;
  {$EXTERNALSYM MM_RHETOREX_WAVEOUT}


  // MM_BROOKTREE product IDs
  // ========================
  MM_BTV_WAVEIN                       = 1;  // Brooktree PCM Wave Audio In
  {$EXTERNALSYM MM_BTV_WAVEIN}
  MM_BTV_WAVEOUT                      = 2;  // Brooktree PCM Wave Audio Out
  {$EXTERNALSYM MM_BTV_WAVEOUT}
  MM_BTV_MIDIIN                       = 3;  // Brooktree MIDI In
  {$EXTERNALSYM MM_BTV_MIDIIN}
  MM_BTV_MIDIOUT                      = 4;  // Brooktree MIDI out
  {$EXTERNALSYM MM_BTV_MIDIOUT}
  MM_BTV_MIDISYNTH                    = 5;  // Brooktree MIDI FM synth
  {$EXTERNALSYM MM_BTV_MIDISYNTH}
  MM_BTV_AUX_LINE                     = 6;  // Brooktree Line Input
  {$EXTERNALSYM MM_BTV_AUX_LINE}
  MM_BTV_AUX_MIC                      = 7;  // Brooktree Microphone Input
  {$EXTERNALSYM MM_BTV_AUX_MIC}
  MM_BTV_AUX_CD                       = 8;  // Brooktree CD Input
  {$EXTERNALSYM MM_BTV_AUX_CD}
  MM_BTV_DIGITALIN                    = 9;  // Brooktree PCM Wave in with subcode information
  {$EXTERNALSYM MM_BTV_DIGITALIN}
  MM_BTV_DIGITALOUT                   = 10;  // Brooktree PCM Wave out with subcode information
  {$EXTERNALSYM MM_BTV_DIGITALOUT}
  MM_BTV_MIDIWAVESTREAM               = 11;  // Brooktree WaveStream
  {$EXTERNALSYM MM_BTV_MIDIWAVESTREAM}
  MM_BTV_MIXER                        = 12;  // Brooktree WSS Mixer driver
  {$EXTERNALSYM MM_BTV_MIXER}


  // MM_ENSONIQ product IDs
  // ======================
  MM_ENSONIQ_SOUNDSCAPE               = $10;  // ENSONIQ Soundscape
  {$EXTERNALSYM MM_ENSONIQ_SOUNDSCAPE}
  MM_SOUNDSCAPE_WAVEOUT               = MM_ENSONIQ_SOUNDSCAPE + 1;
  {$EXTERNALSYM MM_SOUNDSCAPE_WAVEOUT}
  MM_SOUNDSCAPE_WAVEOUT_AUX           = MM_ENSONIQ_SOUNDSCAPE + 2;
  {$EXTERNALSYM MM_SOUNDSCAPE_WAVEOUT_AUX}
  MM_SOUNDSCAPE_WAVEIN                = MM_ENSONIQ_SOUNDSCAPE + 3;
  {$EXTERNALSYM MM_SOUNDSCAPE_WAVEIN}
  MM_SOUNDSCAPE_MIDIOUT               = MM_ENSONIQ_SOUNDSCAPE + 4;
  {$EXTERNALSYM MM_SOUNDSCAPE_MIDIOUT}
  MM_SOUNDSCAPE_MIDIIN                = MM_ENSONIQ_SOUNDSCAPE + 5;
  {$EXTERNALSYM MM_SOUNDSCAPE_MIDIIN}
  MM_SOUNDSCAPE_SYNTH                 = MM_ENSONIQ_SOUNDSCAPE + 6;
  {$EXTERNALSYM MM_SOUNDSCAPE_SYNTH}
  MM_SOUNDSCAPE_MIXER                 = MM_ENSONIQ_SOUNDSCAPE + 7;
  {$EXTERNALSYM MM_SOUNDSCAPE_MIXER}
  MM_SOUNDSCAPE_AUX                   = MM_ENSONIQ_SOUNDSCAPE + 8;
  {$EXTERNALSYM MM_SOUNDSCAPE_AUX}


  // MM_NVIDIA product IDs
  // =====================
  MM_NVIDIA_WAVEOUT                   = 1;
  {$EXTERNALSYM MM_NVIDIA_WAVEOUT}
  MM_NVIDIA_WAVEIN                    = 2;
  {$EXTERNALSYM MM_NVIDIA_WAVEIN}
  MM_NVIDIA_MIDIOUT                   = 3;
  {$EXTERNALSYM MM_NVIDIA_MIDIOUT}
  MM_NVIDIA_MIDIIN                    = 4;
  {$EXTERNALSYM MM_NVIDIA_MIDIIN}
  MM_NVIDIA_GAMEPORT                  = 5;
  {$EXTERNALSYM MM_NVIDIA_GAMEPORT}
  MM_NVIDIA_MIXER                     = 6;
  {$EXTERNALSYM MM_NVIDIA_MIXER}
  MM_NVIDIA_AUX                       = 7;
  {$EXTERNALSYM MM_NVIDIA_AUX}


  // MM_OKSORI product IDs
  // =====================
  MM_OKSORI_BASE                      = 0;  // Oksori Base
  {$EXTERNALSYM MM_OKSORI_BASE}
  MM_OKSORI_OSR8_WAVEOUT              = MM_OKSORI_BASE + 1;  // Oksori 8bit Wave out
  {$EXTERNALSYM MM_OKSORI_OSR8_WAVEOUT}
  MM_OKSORI_OSR8_WAVEIN               = MM_OKSORI_BASE + 2;  // Oksori 8bit Wave in
  {$EXTERNALSYM MM_OKSORI_OSR8_WAVEIN}
  MM_OKSORI_OSR16_WAVEOUT             = MM_OKSORI_BASE + 3;  // Oksori 16 bit Wave out
  {$EXTERNALSYM MM_OKSORI_OSR16_WAVEOUT}
  MM_OKSORI_OSR16_WAVEIN              = MM_OKSORI_BASE + 4;  // Oksori 16 bit Wave in
  {$EXTERNALSYM MM_OKSORI_OSR16_WAVEIN}
  MM_OKSORI_FM_OPL4                   = MM_OKSORI_BASE + 5;  // Oksori FM Synth Yamaha OPL4
  {$EXTERNALSYM MM_OKSORI_FM_OPL4}
  MM_OKSORI_MIX_MASTER                = MM_OKSORI_BASE + 6;  // Oksori DSP Mixer - Master Volume
  {$EXTERNALSYM MM_OKSORI_MIX_MASTER}
  MM_OKSORI_MIX_WAVE                  = MM_OKSORI_BASE + 7;  // Oksori DSP Mixer - Wave Volume
  {$EXTERNALSYM MM_OKSORI_MIX_WAVE}
  MM_OKSORI_MIX_FM                    = MM_OKSORI_BASE + 8;  // Oksori DSP Mixer - FM Volume
  {$EXTERNALSYM MM_OKSORI_MIX_FM}
  MM_OKSORI_MIX_LINE                  = MM_OKSORI_BASE + 9;  // Oksori DSP Mixer - Line Volume
  {$EXTERNALSYM MM_OKSORI_MIX_LINE}
  MM_OKSORI_MIX_CD                    = MM_OKSORI_BASE + 10; // Oksori DSP Mixer - CD Volume
  {$EXTERNALSYM MM_OKSORI_MIX_CD}
  MM_OKSORI_MIX_MIC                   = MM_OKSORI_BASE + 11; // Oksori DSP Mixer - MIC Volume
  {$EXTERNALSYM MM_OKSORI_MIX_MIC}
  MM_OKSORI_MIX_ECHO                  = MM_OKSORI_BASE + 12; // Oksori DSP Mixer - Echo Volume
  {$EXTERNALSYM MM_OKSORI_MIX_ECHO}
  MM_OKSORI_MIX_AUX1                  = MM_OKSORI_BASE + 13; // Oksori AD1848 - AUX1 Volume
  {$EXTERNALSYM MM_OKSORI_MIX_AUX1}
  MM_OKSORI_MIX_LINE1                 = MM_OKSORI_BASE + 14; // Oksori AD1848 - LINE1 Volume
  {$EXTERNALSYM MM_OKSORI_MIX_LINE1}
  MM_OKSORI_EXT_MIC1                  = MM_OKSORI_BASE + 15; // Oksori External - One Mic Connect
  {$EXTERNALSYM MM_OKSORI_EXT_MIC1}
  MM_OKSORI_EXT_MIC2                  = MM_OKSORI_BASE + 16; // Oksori External - Two Mic Connect
  {$EXTERNALSYM MM_OKSORI_EXT_MIC2}
  MM_OKSORI_MIDIOUT                   = MM_OKSORI_BASE + 17; // Oksori MIDI Out Device
  {$EXTERNALSYM MM_OKSORI_MIDIOUT}
  MM_OKSORI_MIDIIN                    = MM_OKSORI_BASE + 18; // Oksori MIDI In Device
  {$EXTERNALSYM MM_OKSORI_MIDIIN}
  MM_OKSORI_MPEG_CDVISION             = MM_OKSORI_BASE + 19; // Oksori CD-Vision MPEG Decoder
  {$EXTERNALSYM MM_OKSORI_MPEG_CDVISION}


  // MM_DIACOUSTICS product IDs
  // ==========================
  MM_DIACOUSTICS_DRUM_ACTION          = 1;  // Drum Action
  {$EXTERNALSYM MM_DIACOUSTICS_DRUM_ACTION}


  // MM_KAY_ELEMETRICS product IDs
  // =============================
  MM_KAY_ELEMETRICS_CSL               = $4300;
  {$EXTERNALSYM MM_KAY_ELEMETRICS_CSL}
  MM_KAY_ELEMETRICS_CSL_DAT           = $4308;
  {$EXTERNALSYM MM_KAY_ELEMETRICS_CSL_DAT}
  MM_KAY_ELEMETRICS_CSL_4CHANNEL      = $4309;
  {$EXTERNALSYM MM_KAY_ELEMETRICS_CSL_4CHANNEL}

  // MM_CRYSTAL product IDs
  // ======================
  MM_CRYSTAL_CS4232_WAVEIN            = 1;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEIN}
  MM_CRYSTAL_CS4232_WAVEOUT           = 2;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEOUT}
  MM_CRYSTAL_CS4232_WAVEMIXER         = 3;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEMIXER}
  MM_CRYSTAL_CS4232_WAVEAUX_AUX1      = 4;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEAUX_AUX1}
  MM_CRYSTAL_CS4232_WAVEAUX_AUX2      = 5;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEAUX_AUX2}
  MM_CRYSTAL_CS4232_WAVEAUX_LINE      = 6;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEAUX_LINE}
  MM_CRYSTAL_CS4232_WAVEAUX_MONO      = 7;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEAUX_MONO}
  MM_CRYSTAL_CS4232_WAVEAUX_MASTER    = 8;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_WAVEAUX_MASTER}
  MM_CRYSTAL_CS4232_MIDIIN            = 9;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_MIDIIN}
  MM_CRYSTAL_CS4232_MIDIOUT           = 10;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_MIDIOUT}
  MM_CRYSTAL_CS4232_INPUTGAIN_AUX1    = 13;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_INPUTGAIN_AUX1}
  MM_CRYSTAL_CS4232_INPUTGAIN_LOOP    = 14;
  {$EXTERNALSYM MM_CRYSTAL_CS4232_INPUTGAIN_LOOP}
  MM_CRYSTAL_SOUND_FUSION_WAVEIN      = 21;
  {$EXTERNALSYM MM_CRYSTAL_SOUND_FUSION_WAVEIN}
  MM_CRYSTAL_SOUND_FUSION_WAVEOUT     = 22;
  {$EXTERNALSYM MM_CRYSTAL_SOUND_FUSION_WAVEOUT}
  MM_CRYSTAL_SOUND_FUSION_MIXER       = 23;
  {$EXTERNALSYM MM_CRYSTAL_SOUND_FUSION_MIXER}
  MM_CRYSTAL_SOUND_FUSION_MIDIIN      = 24;
  {$EXTERNALSYM MM_CRYSTAL_SOUND_FUSION_MIDIIN}
  MM_CRYSTAL_SOUND_FUSION_MIDIOUT     = 25;
  {$EXTERNALSYM MM_CRYSTAL_SOUND_FUSION_MIDIOUT}
  MM_CRYSTAL_SOUND_FUSION_JOYSTICK    = 26;
  {$EXTERNALSYM MM_CRYSTAL_SOUND_FUSION_JOYSTICK}


  // MM_QUARTERDECK product IDs
  // ==========================
  MM_QUARTERDECK_LHWAVEIN             = 0;   // Quarterdeck L&H Codec Wave In
  {$EXTERNALSYM MM_QUARTERDECK_LHWAVEIN}
  MM_QUARTERDECK_LHWAVEOUT            = 1;   // Quarterdeck L&H Codec Wave Out
  {$EXTERNALSYM MM_QUARTERDECK_LHWAVEOUT}


  // MM_TDK product IDs
  // ==================
  MM_TDK_MW_MIDI_SYNTH                = 1;
  {$EXTERNALSYM MM_TDK_MW_MIDI_SYNTH}
  MM_TDK_MW_MIDI_IN                   = 2;
  {$EXTERNALSYM MM_TDK_MW_MIDI_IN}
  MM_TDK_MW_MIDI_OUT                  = 3;
  {$EXTERNALSYM MM_TDK_MW_MIDI_OUT}
  MM_TDK_MW_WAVE_IN                   = 4;
  {$EXTERNALSYM MM_TDK_MW_WAVE_IN}
  MM_TDK_MW_WAVE_OUT                  = 5;
  {$EXTERNALSYM MM_TDK_MW_WAVE_OUT}
  MM_TDK_MW_AUX                       = 6;
  {$EXTERNALSYM MM_TDK_MW_AUX}
  MM_TDK_MW_MIXER                     = 10;
  {$EXTERNALSYM MM_TDK_MW_MIXER}
  MM_TDK_MW_AUX_MASTER                = 100;
  {$EXTERNALSYM MM_TDK_MW_AUX_MASTER}
  MM_TDK_MW_AUX_BASS                  = 101;
  {$EXTERNALSYM MM_TDK_MW_AUX_BASS}
  MM_TDK_MW_AUX_TREBLE                = 102;
  {$EXTERNALSYM MM_TDK_MW_AUX_TREBLE}
  MM_TDK_MW_AUX_MIDI_VOL              = 103;
  {$EXTERNALSYM MM_TDK_MW_AUX_MIDI_VOL}
  MM_TDK_MW_AUX_WAVE_VOL              = 104;
  {$EXTERNALSYM MM_TDK_MW_AUX_WAVE_VOL}
  MM_TDK_MW_AUX_WAVE_RVB              = 105;
  {$EXTERNALSYM MM_TDK_MW_AUX_WAVE_RVB}
  MM_TDK_MW_AUX_WAVE_CHR              = 106;
  {$EXTERNALSYM MM_TDK_MW_AUX_WAVE_CHR}
  MM_TDK_MW_AUX_VOL                   = 107;
  {$EXTERNALSYM MM_TDK_MW_AUX_VOL}
  MM_TDK_MW_AUX_RVB                   = 108;
  {$EXTERNALSYM MM_TDK_MW_AUX_RVB}
  MM_TDK_MW_AUX_CHR                   = 109;
  {$EXTERNALSYM MM_TDK_MW_AUX_CHR}


  // MM_DIGITAL_AUDIO_LABS product IDs
  // =================================
  MM_DIGITAL_AUDIO_LABS_TC            = $01;
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS_TC}
  MM_DIGITAL_AUDIO_LABS_DOC           = $02;
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS_DOC}
  MM_DIGITAL_AUDIO_LABS_V8            = $10;
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS_V8}
  MM_DIGITAL_AUDIO_LABS_CPRO          = $11;
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS_CPRO}
  MM_DIGITAL_AUDIO_LABS_VP            = $12;
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS_VP}
  MM_DIGITAL_AUDIO_LABS_CDLX          = $13;
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS_CDLX}
  MM_DIGITAL_AUDIO_LABS_CTDIF         = $14;
  {$EXTERNALSYM MM_DIGITAL_AUDIO_LABS_CTDIF}


  // MM_SEERSYS product IDs
  // ======================
  MM_SEERSYS_SEERSYNTH                = 1;
  {$EXTERNALSYM MM_SEERSYS_SEERSYNTH}
  MM_SEERSYS_SEERWAVE                 = 2;
  {$EXTERNALSYM MM_SEERSYS_SEERWAVE}
  MM_SEERSYS_SEERMIX                  = 3;
  {$EXTERNALSYM MM_SEERSYS_SEERMIX}
  MM_SEERSYS_WAVESYNTH                = 4;
  {$EXTERNALSYM MM_SEERSYS_WAVESYNTH}
  MM_SEERSYS_WAVESYNTH_WG             = 5;
  {$EXTERNALSYM MM_SEERSYS_WAVESYNTH_WG}
  MM_SEERSYS_REALITY                  = 6;
  {$EXTERNALSYM MM_SEERSYS_REALITY}


  // MM_OSPREY product IDs
  // ======================
  MM_OSPREY_1000WAVEIN                = 1;
  {$EXTERNALSYM MM_OSPREY_1000WAVEIN}
  MM_OSPREY_1000WAVEOUT               = 2;
  {$EXTERNALSYM MM_OSPREY_1000WAVEOUT}


  // MM_SOUNDESIGNS product IDs
  // ==========================
  MM_SOUNDESIGNS_WAVEIN               = 1;
  {$EXTERNALSYM MM_SOUNDESIGNS_WAVEIN}
  MM_SOUNDESIGNS_WAVEOUT              = 2;
  {$EXTERNALSYM MM_SOUNDESIGNS_WAVEOUT}


  // MM_SPECTRUM_SIGNAL_PROCESSING product IDs
  // =========================================
  MM_SSP_SNDFESWAVEIN                 = 1;  // Sound Festa Wave In Device
  {$EXTERNALSYM MM_SSP_SNDFESWAVEIN}
  MM_SSP_SNDFESWAVEOUT                = 2;  // Sound Festa Wave Out Device
  {$EXTERNALSYM MM_SSP_SNDFESWAVEOUT}
  MM_SSP_SNDFESMIDIIN                 = 3;  // Sound Festa MIDI In Device
  {$EXTERNALSYM MM_SSP_SNDFESMIDIIN}
  MM_SSP_SNDFESMIDIOUT                = 4;  // Sound Festa MIDI Out Device
  {$EXTERNALSYM MM_SSP_SNDFESMIDIOUT}
  MM_SSP_SNDFESSYNTH                  = 5;  // Sound Festa MIDI Synth Device
  {$EXTERNALSYM MM_SSP_SNDFESSYNTH}
  MM_SSP_SNDFESMIX                    = 6;  // Sound Festa Mixer Device
  {$EXTERNALSYM MM_SSP_SNDFESMIX}
  MM_SSP_SNDFESAUX                    = 7;  // Sound Festa Auxilliary Device
  {$EXTERNALSYM MM_SSP_SNDFESAUX}


  // MM_ECS product IDs
  // ==================
  MM_ECS_AADF_MIDI_IN                 = 10;
  {$EXTERNALSYM MM_ECS_AADF_MIDI_IN}
  MM_ECS_AADF_MIDI_OUT                = 11;
  {$EXTERNALSYM MM_ECS_AADF_MIDI_OUT}
  MM_ECS_AADF_WAVE2MIDI_IN            = 12;
  {$EXTERNALSYM MM_ECS_AADF_WAVE2MIDI_IN}


  // MM_AMD product IDs
  // ==================
  MM_AMD_INTERWAVE_WAVEIN             = 1;
  {$EXTERNALSYM MM_AMD_INTERWAVE_WAVEIN}
  MM_AMD_INTERWAVE_WAVEOUT            = 2;
  {$EXTERNALSYM MM_AMD_INTERWAVE_WAVEOUT}
  MM_AMD_INTERWAVE_SYNTH              = 3;
  {$EXTERNALSYM MM_AMD_INTERWAVE_SYNTH}
  MM_AMD_INTERWAVE_MIXER1             = 4;
  {$EXTERNALSYM MM_AMD_INTERWAVE_MIXER1}
  MM_AMD_INTERWAVE_MIXER2             = 5;
  {$EXTERNALSYM MM_AMD_INTERWAVE_MIXER2}
  MM_AMD_INTERWAVE_JOYSTICK           = 6;
  {$EXTERNALSYM MM_AMD_INTERWAVE_JOYSTICK}
  MM_AMD_INTERWAVE_EX_CD              = 7;
  {$EXTERNALSYM MM_AMD_INTERWAVE_EX_CD}
  MM_AMD_INTERWAVE_MIDIIN             = 8;
  {$EXTERNALSYM MM_AMD_INTERWAVE_MIDIIN}
  MM_AMD_INTERWAVE_MIDIOUT            = 9;
  {$EXTERNALSYM MM_AMD_INTERWAVE_MIDIOUT}
  MM_AMD_INTERWAVE_AUX1               = 10;
  {$EXTERNALSYM MM_AMD_INTERWAVE_AUX1}
  MM_AMD_INTERWAVE_AUX2               = 11;
  {$EXTERNALSYM MM_AMD_INTERWAVE_AUX2}
  MM_AMD_INTERWAVE_AUX_MIC            = 12;
  {$EXTERNALSYM MM_AMD_INTERWAVE_AUX_MIC}
  MM_AMD_INTERWAVE_AUX_CD             = 13;
  {$EXTERNALSYM MM_AMD_INTERWAVE_AUX_CD}
  MM_AMD_INTERWAVE_MONO_IN            = 14;
  {$EXTERNALSYM MM_AMD_INTERWAVE_MONO_IN}
  MM_AMD_INTERWAVE_MONO_OUT           = 15;
  {$EXTERNALSYM MM_AMD_INTERWAVE_MONO_OUT}
  MM_AMD_INTERWAVE_EX_TELEPHONY       = 16;
  {$EXTERNALSYM MM_AMD_INTERWAVE_EX_TELEPHONY}
  MM_AMD_INTERWAVE_WAVEOUT_BASE       = 17;
  {$EXTERNALSYM MM_AMD_INTERWAVE_WAVEOUT_BASE}
  MM_AMD_INTERWAVE_WAVEOUT_TREBLE     = 18;
  {$EXTERNALSYM MM_AMD_INTERWAVE_WAVEOUT_TREBLE}
  MM_AMD_INTERWAVE_STEREO_ENHANCED    = 19;
  {$EXTERNALSYM MM_AMD_INTERWAVE_STEREO_ENHANCED}


  // MM_COREDYNAMICS product IDs
  // ===========================
  MM_COREDYNAMICS_DYNAMIXHR           = 1;  // DynaMax Hi-Rez
  {$EXTERNALSYM MM_COREDYNAMICS_DYNAMIXHR}
  MM_COREDYNAMICS_DYNASONIX_SYNTH     = 2;  // DynaSonix
  {$EXTERNALSYM MM_COREDYNAMICS_DYNASONIX_SYNTH}
  MM_COREDYNAMICS_DYNASONIX_MIDI_IN   = 3;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNASONIX_MIDI_IN}
  MM_COREDYNAMICS_DYNASONIX_MIDI_OUT  = 4;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNASONIX_MIDI_OUT}
  MM_COREDYNAMICS_DYNASONIX_WAVE_IN   = 5;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNASONIX_WAVE_IN}
  MM_COREDYNAMICS_DYNASONIX_WAVE_OUT  = 6;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNASONIX_WAVE_OUT}
  MM_COREDYNAMICS_DYNASONIX_AUDIO_IN  = 7;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNASONIX_AUDIO_IN}
  MM_COREDYNAMICS_DYNASONIX_AUDIO_OUT = 8;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNASONIX_AUDIO_OUT}
  MM_COREDYNAMICS_DYNAGRAFX_VGA       = 9;  // DynaGrfx
  {$EXTERNALSYM MM_COREDYNAMICS_DYNAGRAFX_VGA}
  MM_COREDYNAMICS_DYNAGRAFX_WAVE_IN   = 10;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNAGRAFX_WAVE_IN}
  MM_COREDYNAMICS_DYNAGRAFX_WAVE_OUT  = 11;
  {$EXTERNALSYM MM_COREDYNAMICS_DYNAGRAFX_WAVE_OUT}


  // MM_CANAM product IDs //
  MM_CANAM_CBXWAVEOUT                 = 1;
  {$EXTERNALSYM MM_CANAM_CBXWAVEOUT}
  MM_CANAM_CBXWAVEIN                  = 2;
  {$EXTERNALSYM MM_CANAM_CBXWAVEIN}


  // MM_SOFTSOUND product IDs
  // ========================
  MM_SOFTSOUND_CODEC                  = 1;
  {$EXTERNALSYM MM_SOFTSOUND_CODEC}


  // MM_NORRIS product IDs
  // =====================
  MM_NORRIS_VOICELINK                 = 1;
  {$EXTERNALSYM MM_NORRIS_VOICELINK}


  // MM_DDD product IDs
  // ==================
  MM_DDD_MIDILINK_MIDIIN              = 1;
  {$EXTERNALSYM MM_DDD_MIDILINK_MIDIIN}
  MM_DDD_MIDILINK_MIDIOUT             = 2;
  {$EXTERNALSYM MM_DDD_MIDILINK_MIDIOUT}


  // MM_EUPHONICS product IDs
  // ========================
  MM_EUPHONICS_AUX_CD                 = 1;
  {$EXTERNALSYM MM_EUPHONICS_AUX_CD}
  MM_EUPHONICS_AUX_LINE               = 2;
  {$EXTERNALSYM MM_EUPHONICS_AUX_LINE}
  MM_EUPHONICS_AUX_MASTER             = 3;
  {$EXTERNALSYM MM_EUPHONICS_AUX_MASTER}
  MM_EUPHONICS_AUX_MIC                = 4;
  {$EXTERNALSYM MM_EUPHONICS_AUX_MIC}
  MM_EUPHONICS_AUX_MIDI               = 5;
  {$EXTERNALSYM MM_EUPHONICS_AUX_MIDI}
  MM_EUPHONICS_AUX_WAVE               = 6;
  {$EXTERNALSYM MM_EUPHONICS_AUX_WAVE}
  MM_EUPHONICS_FMSYNTH_MONO           = 7;
  {$EXTERNALSYM MM_EUPHONICS_FMSYNTH_MONO}
  MM_EUPHONICS_FMSYNTH_STEREO         = 8;
  {$EXTERNALSYM MM_EUPHONICS_FMSYNTH_STEREO}
  MM_EUPHONICS_MIDIIN                 = 9;
  {$EXTERNALSYM MM_EUPHONICS_MIDIIN}
  MM_EUPHONICS_MIDIOUT                = 10;
  {$EXTERNALSYM MM_EUPHONICS_MIDIOUT}
  MM_EUPHONICS_MIXER                  = 11;
  {$EXTERNALSYM MM_EUPHONICS_MIXER}
  MM_EUPHONICS_WAVEIN                 = 12;
  {$EXTERNALSYM MM_EUPHONICS_WAVEIN}
  MM_EUPHONICS_WAVEOUT                = 13;
  {$EXTERNALSYM MM_EUPHONICS_WAVEOUT}
  MM_EUPHONICS_EUSYNTH                = 14;
  {$EXTERNALSYM MM_EUPHONICS_EUSYNTH}


  // MM_CRYSTAL_NET product IDs
  // ==========================
  CRYSTAL_NET_SFM_CODEC               = 1;
  {$EXTERNALSYM CRYSTAL_NET_SFM_CODEC}


  // MM_CHROMATIC product IDs
  // ========================
  MM_CHROMATIC_M1                     = $0001;
  {$EXTERNALSYM MM_CHROMATIC_M1}
  MM_CHROMATIC_M1_WAVEIN              = $0002;
  {$EXTERNALSYM MM_CHROMATIC_M1_WAVEIN}
  MM_CHROMATIC_M1_WAVEOUT             = $0003;
  {$EXTERNALSYM MM_CHROMATIC_M1_WAVEOUT}
  MM_CHROMATIC_M1_FMSYNTH             = $0004;
  {$EXTERNALSYM MM_CHROMATIC_M1_FMSYNTH}
  MM_CHROMATIC_M1_MIXER               = $0005;
  {$EXTERNALSYM MM_CHROMATIC_M1_MIXER}
  MM_CHROMATIC_M1_AUX                 = $0006;
  {$EXTERNALSYM MM_CHROMATIC_M1_AUX}
  MM_CHROMATIC_M1_AUX_CD              = $0007;
  {$EXTERNALSYM MM_CHROMATIC_M1_AUX_CD}
  MM_CHROMATIC_M1_MIDIIN              = $0008;
  {$EXTERNALSYM MM_CHROMATIC_M1_MIDIIN}
  MM_CHROMATIC_M1_MIDIOUT             = $0009;
  {$EXTERNALSYM MM_CHROMATIC_M1_MIDIOUT}
  MM_CHROMATIC_M1_WTSYNTH             = $0010;
  {$EXTERNALSYM MM_CHROMATIC_M1_WTSYNTH}
  MM_CHROMATIC_M1_MPEGWAVEIN          = $0011;
  {$EXTERNALSYM MM_CHROMATIC_M1_MPEGWAVEIN}
  MM_CHROMATIC_M1_MPEGWAVEOUT         = $0012;
  {$EXTERNALSYM MM_CHROMATIC_M1_MPEGWAVEOUT}
  MM_CHROMATIC_M2                     = $0013;
  {$EXTERNALSYM MM_CHROMATIC_M2}
  MM_CHROMATIC_M2_WAVEIN              = $0014;
  {$EXTERNALSYM MM_CHROMATIC_M2_WAVEIN}
  MM_CHROMATIC_M2_WAVEOUT             = $0015;
  {$EXTERNALSYM MM_CHROMATIC_M2_WAVEOUT}
  MM_CHROMATIC_M2_FMSYNTH             = $0016;
  {$EXTERNALSYM MM_CHROMATIC_M2_FMSYNTH}
  MM_CHROMATIC_M2_MIXER               = $0017;
  {$EXTERNALSYM MM_CHROMATIC_M2_MIXER}
  MM_CHROMATIC_M2_AUX                 = $0018;
  {$EXTERNALSYM MM_CHROMATIC_M2_AUX}
  MM_CHROMATIC_M2_AUX_CD              = $0019;
  {$EXTERNALSYM MM_CHROMATIC_M2_AUX_CD}
  MM_CHROMATIC_M2_MIDIIN              = $0020;
  {$EXTERNALSYM MM_CHROMATIC_M2_MIDIIN}
  MM_CHROMATIC_M2_MIDIOUT             = $0021;
  {$EXTERNALSYM MM_CHROMATIC_M2_MIDIOUT}
  MM_CHROMATIC_M2_WTSYNTH             = $0022;
  {$EXTERNALSYM MM_CHROMATIC_M2_WTSYNTH}
  MM_CHROMATIC_M2_MPEGWAVEIN          = $0023;
  {$EXTERNALSYM MM_CHROMATIC_M2_MPEGWAVEIN}
  MM_CHROMATIC_M2_MPEGWAVEOUT         = $0024;
  {$EXTERNALSYM MM_CHROMATIC_M2_MPEGWAVEOUT}


  // MM_VIENNASYS product IDs
  // ========================
  MM_VIENNASYS_TSP_WAVE_DRIVER        = 1;
  {$EXTERNALSYM MM_VIENNASYS_TSP_WAVE_DRIVER}


  // MM_CONNECTIX product IDs
  // ========================
  MM_CONNECTIX_VIDEC_CODEC            = 1;
  {$EXTERNALSYM MM_CONNECTIX_VIDEC_CODEC}


  // MM_GADGETLABS product IDs
  // =========================
  MM_GADGETLABS_WAVE44_WAVEIN         = 1;
  {$EXTERNALSYM MM_GADGETLABS_WAVE44_WAVEIN}
  MM_GADGETLABS_WAVE44_WAVEOUT        = 2;
  {$EXTERNALSYM MM_GADGETLABS_WAVE44_WAVEOUT}
  MM_GADGETLABS_WAVE42_WAVEIN         = 3;
  {$EXTERNALSYM MM_GADGETLABS_WAVE42_WAVEIN}
  MM_GADGETLABS_WAVE42_WAVEOUT        = 4;
  {$EXTERNALSYM MM_GADGETLABS_WAVE42_WAVEOUT}
  MM_GADGETLABS_WAVE4_MIDIIN          = 5;
  {$EXTERNALSYM MM_GADGETLABS_WAVE4_MIDIIN}
  MM_GADGETLABS_WAVE4_MIDIOUT         = 6;
  {$EXTERNALSYM MM_GADGETLABS_WAVE4_MIDIOUT}


  // MM_FRONTIER product IDs
  // =======================
  MM_FRONTIER_WAVECENTER_MIDIIN       = 1;  // WaveCenter
  {$EXTERNALSYM MM_FRONTIER_WAVECENTER_MIDIIN}
  MM_FRONTIER_WAVECENTER_MIDIOUT      = 2;
  {$EXTERNALSYM MM_FRONTIER_WAVECENTER_MIDIOUT}
  MM_FRONTIER_WAVECENTER_WAVEIN       = 3;
  {$EXTERNALSYM MM_FRONTIER_WAVECENTER_WAVEIN}
  MM_FRONTIER_WAVECENTER_WAVEOUT      = 4;
  {$EXTERNALSYM MM_FRONTIER_WAVECENTER_WAVEOUT}


  // MM_VIONA product IDs
  // ====================
  MM_VIONA_QVINPCI_MIXER              = 1;  // Q-Motion PCI II/Bravado 2000
  {$EXTERNALSYM MM_VIONA_QVINPCI_MIXER}
  MM_VIONA_QVINPCI_WAVEIN             = 2;
  {$EXTERNALSYM MM_VIONA_QVINPCI_WAVEIN}
  MM_VIONAQVINPCI_WAVEOUT             = 3;
  {$EXTERNALSYM MM_VIONAQVINPCI_WAVEOUT}
  MM_VIONA_BUSTER_MIXER               = 4;  // Buster
  {$EXTERNALSYM MM_VIONA_BUSTER_MIXER}
  MM_VIONA_CINEMASTER_MIXER           = 5;  // Cinemaster
  {$EXTERNALSYM MM_VIONA_CINEMASTER_MIXER}
  MM_VIONA_CONCERTO_MIXER             = 6;  // Concerto
  {$EXTERNALSYM MM_VIONA_CONCERTO_MIXER}


  // MM_CASIO product IDs
  // ====================
  MM_CASIO_WP150_MIDIOUT              = 1;  // wp150
  {$EXTERNALSYM MM_CASIO_WP150_MIDIOUT}
  MM_CASIO_WP150_MIDIIN               = 2;
  {$EXTERNALSYM MM_CASIO_WP150_MIDIIN}
  MM_CASIO_LSG_MIDIOUT                = 3;
  {$EXTERNALSYM MM_CASIO_LSG_MIDIOUT}


  // MM_DIAMONDMM product IDs
  // ========================
  MM_DIMD_PLATFORM                    = 0;  // Freedom Audio
  {$EXTERNALSYM MM_DIMD_PLATFORM}
  MM_DIMD_DIRSOUND                    = 1;
  {$EXTERNALSYM MM_DIMD_DIRSOUND}
  MM_DIMD_VIRTMPU                     = 2;
  {$EXTERNALSYM MM_DIMD_VIRTMPU}
  MM_DIMD_VIRTSB                      = 3;
  {$EXTERNALSYM MM_DIMD_VIRTSB}
  MM_DIMD_VIRTJOY                     = 4;
  {$EXTERNALSYM MM_DIMD_VIRTJOY}
  MM_DIMD_WAVEIN                      = 5;
  {$EXTERNALSYM MM_DIMD_WAVEIN}
  MM_DIMD_WAVEOUT                     = 6;
  {$EXTERNALSYM MM_DIMD_WAVEOUT}
  MM_DIMD_MIDIIN                      = 7;
  {$EXTERNALSYM MM_DIMD_MIDIIN}
  MM_DIMD_MIDIOUT                     = 8;
  {$EXTERNALSYM MM_DIMD_MIDIOUT}
  MM_DIMD_AUX_LINE                    = 9;
  {$EXTERNALSYM MM_DIMD_AUX_LINE}
  MM_DIMD_MIXER                       = 10;
  {$EXTERNALSYM MM_DIMD_MIXER}
  MM_DIMD_WSS_WAVEIN                  = 14;
  {$EXTERNALSYM MM_DIMD_WSS_WAVEIN}
  MM_DIMD_WSS_WAVEOUT                 = 15;
  {$EXTERNALSYM MM_DIMD_WSS_WAVEOUT}
  MM_DIMD_WSS_MIXER                   = 17;
  {$EXTERNALSYM MM_DIMD_WSS_MIXER}
  MM_DIMD_WSS_AUX                     = 21;
  {$EXTERNALSYM MM_DIMD_WSS_AUX}
  MM_DIMD_WSS_SYNTH                   = 76;
  {$EXTERNALSYM MM_DIMD_WSS_SYNTH}


  // MM_S3 product IDs
  // =================
  MM_S3_WAVEOUT                       = 1;
  {$EXTERNALSYM MM_S3_WAVEOUT}
  MM_S3_WAVEIN                        = 2;
  {$EXTERNALSYM MM_S3_WAVEIN}
  MM_S3_MIDIOUT                       = 3;
  {$EXTERNALSYM MM_S3_MIDIOUT}
  MM_S3_MIDIIN                        = 4;
  {$EXTERNALSYM MM_S3_MIDIIN}
  MM_S3_FMSYNTH                       = 5;
  {$EXTERNALSYM MM_S3_FMSYNTH}
  MM_S3_MIXER                         = 6;
  {$EXTERNALSYM MM_S3_MIXER}
  MM_S3_AUX                           = 7;
  {$EXTERNALSYM MM_S3_AUX}


  // MM_VANKOEVERING product IDs
  // ===========================
  MM_VKC_MPU401_MIDIIN                = $0100;
  {$EXTERNALSYM MM_VKC_MPU401_MIDIIN}
  MM_VKC_SERIAL_MIDIIN                = $0101;
  {$EXTERNALSYM MM_VKC_SERIAL_MIDIIN}
  MM_VKC_MPU401_MIDIOUT               = $0200;
  {$EXTERNALSYM MM_VKC_MPU401_MIDIOUT}
  MM_VKC_SERIAL_MIDIOUT               = $0201;
  {$EXTERNALSYM MM_VKC_SERIAL_MIDIOUT}


  // MM_ZEFIRO product IDs
  // =====================
  MM_ZEFIRO_ZA2                       = 2;
  {$EXTERNALSYM MM_ZEFIRO_ZA2}


  // MM_FRAUNHOFER_IIS product IDs
  // =============================
  MM_FHGIIS_MPEGLAYER3_DECODE         = 9;
  {$EXTERNALSYM MM_FHGIIS_MPEGLAYER3_DECODE}
  MM_FHGIIS_MPEGLAYER3                = 10;
  {$EXTERNALSYM MM_FHGIIS_MPEGLAYER3}
  MM_FHGIIS_MPEGLAYER3_LITE           = 10;
  {$EXTERNALSYM MM_FHGIIS_MPEGLAYER3_LITE}
  MM_FHGIIS_MPEGLAYER3_BASIC          = 11;
  {$EXTERNALSYM MM_FHGIIS_MPEGLAYER3_BASIC}
  MM_FHGIIS_MPEGLAYER3_ADVANCED       = 12;
  {$EXTERNALSYM MM_FHGIIS_MPEGLAYER3_ADVANCED}
  MM_FHGIIS_MPEGLAYER3_PROFESSIONAL   = 13;
  {$EXTERNALSYM MM_FHGIIS_MPEGLAYER3_PROFESSIONAL}
  MM_FHGIIS_MPEGLAYER3_ADVANCEDPLUS   = 14;
  {$EXTERNALSYM MM_FHGIIS_MPEGLAYER3_ADVANCEDPLUS}


  // MM_QUICKNET product IDs
  // =======================
  MM_QUICKNET_PJWAVEIN                = 1;
  {$EXTERNALSYM MM_QUICKNET_PJWAVEIN}
  MM_QUICKNET_PJWAVEOUT               = 2;
  {$EXTERNALSYM MM_QUICKNET_PJWAVEOUT}


  // MM_SICRESOURCE product IDs
  // ==========================
  MM_SICRESOURCE_SSO3D                = 2;
  {$EXTERNALSYM MM_SICRESOURCE_SSO3D}
  MM_SICRESOURCE_SSOW3DI              = 3;
  {$EXTERNALSYM MM_SICRESOURCE_SSOW3DI}


  // MM_NEOMAGIC product IDs
  // =======================
  MM_NEOMAGIC_SYNTH                   = 1;
  {$EXTERNALSYM MM_NEOMAGIC_SYNTH}
  MM_NEOMAGIC_WAVEOUT                 = 2;
  {$EXTERNALSYM MM_NEOMAGIC_WAVEOUT}
  MM_NEOMAGIC_WAVEIN                  = 3;
  {$EXTERNALSYM MM_NEOMAGIC_WAVEIN}
  MM_NEOMAGIC_MIDIOUT                 = 4;
  {$EXTERNALSYM MM_NEOMAGIC_MIDIOUT}
  MM_NEOMAGIC_MIDIIN                  = 5;
  {$EXTERNALSYM MM_NEOMAGIC_MIDIIN}
  MM_NEOMAGIC_AUX                     = 6;
  {$EXTERNALSYM MM_NEOMAGIC_AUX}
  MM_NEOMAGIC_MW3DX_WAVEOUT           = 10;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_WAVEOUT}
  MM_NEOMAGIC_MW3DX_WAVEIN            = 11;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_WAVEIN}
  MM_NEOMAGIC_MW3DX_MIDIOUT           = 12;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_MIDIOUT}
  MM_NEOMAGIC_MW3DX_MIDIIN            = 13;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_MIDIIN}
  MM_NEOMAGIC_MW3DX_FMSYNTH           = 14;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_FMSYNTH}
  MM_NEOMAGIC_MW3DX_GMSYNTH           = 15;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_GMSYNTH}
  MM_NEOMAGIC_MW3DX_MIXER             = 16;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_MIXER}
  MM_NEOMAGIC_MW3DX_AUX               = 17;
  {$EXTERNALSYM MM_NEOMAGIC_MW3DX_AUX}
  MM_NEOMAGIC_MWAVE_WAVEOUT           = 20;
  {$EXTERNALSYM MM_NEOMAGIC_MWAVE_WAVEOUT}
  MM_NEOMAGIC_MWAVE_WAVEIN            = 21;
  {$EXTERNALSYM MM_NEOMAGIC_MWAVE_WAVEIN}
  MM_NEOMAGIC_MWAVE_MIDIOUT           = 22;
  {$EXTERNALSYM MM_NEOMAGIC_MWAVE_MIDIOUT}
  MM_NEOMAGIC_MWAVE_MIDIIN            = 23;
  {$EXTERNALSYM MM_NEOMAGIC_MWAVE_MIDIIN}
  MM_NEOMAGIC_MWAVE_MIXER             = 24;
  {$EXTERNALSYM MM_NEOMAGIC_MWAVE_MIXER}
  MM_NEOMAGIC_MWAVE_AUX               = 25;
  {$EXTERNALSYM MM_NEOMAGIC_MWAVE_AUX}


  // MM_MERGING_TECHNOLOGIES product IDs
  // ===================================
  MM_MERGING_MPEGL3                   = 1;
  {$EXTERNALSYM MM_MERGING_MPEGL3}


  // MM_XIRLINK product IDs
  // ======================
  MM_XIRLINK_VISIONLINK               = 1;
  {$EXTERNALSYM MM_XIRLINK_VISIONLINK}


  // MM_OTI product IDs
  // ==================
  MM_OTI_611WAVEIN                    = 5;
  {$EXTERNALSYM MM_OTI_611WAVEIN}
  MM_OTI_611WAVEOUT                   = 6;
  {$EXTERNALSYM MM_OTI_611WAVEOUT}
  MM_OTI_611MIXER                     = 7;
  {$EXTERNALSYM MM_OTI_611MIXER}
  MM_OTI_611MIDIN                     = $12;
  {$EXTERNALSYM MM_OTI_611MIDIN}
  MM_OTI_611MIDIOUT                   = $13;
  {$EXTERNALSYM MM_OTI_611MIDIOUT}


  // MM_AUREAL product IDs
  // =====================
  MM_AUREAL_AU8820                    = 16;
  {$EXTERNALSYM MM_AUREAL_AU8820}
  MM_AU8820_SYNTH                     = 17;
  {$EXTERNALSYM MM_AU8820_SYNTH}
  MM_AU8820_WAVEOUT                   = 18;
  {$EXTERNALSYM MM_AU8820_WAVEOUT}
  MM_AU8820_WAVEIN                    = 19;
  {$EXTERNALSYM MM_AU8820_WAVEIN}
  MM_AU8820_MIXER                     = 20;
  {$EXTERNALSYM MM_AU8820_MIXER}
  MM_AU8820_AUX                       = 21;
  {$EXTERNALSYM MM_AU8820_AUX}
  MM_AU8820_MIDIOUT                   = 22;
  {$EXTERNALSYM MM_AU8820_MIDIOUT}
  MM_AU8820_MIDIIN                    = 23;
  {$EXTERNALSYM MM_AU8820_MIDIIN}
  MM_AUREAL_AU8830                    = 32;
  {$EXTERNALSYM MM_AUREAL_AU8830}
  MM_AU8830_SYNTH                     = 33;
  {$EXTERNALSYM MM_AU8830_SYNTH}
  MM_AU8830_WAVEOUT                   = 34;
  {$EXTERNALSYM MM_AU8830_WAVEOUT}
  MM_AU8830_WAVEIN                    = 35;
  {$EXTERNALSYM MM_AU8830_WAVEIN}
  MM_AU8830_MIXER                     = 36;
  {$EXTERNALSYM MM_AU8830_MIXER}
  MM_AU8830_AUX                       = 37;
  {$EXTERNALSYM MM_AU8830_AUX}
  MM_AU8830_MIDIOUT                   = 38;
  {$EXTERNALSYM MM_AU8830_MIDIOUT}
  MM_AU8830_MIDIIN                    = 39;
  {$EXTERNALSYM MM_AU8830_MIDIIN}


  // MM_VIVO product IDs
  // ===================
  MM_VIVO_AUDIO_CODEC                 = 1;
  {$EXTERNALSYM MM_VIVO_AUDIO_CODEC}


  // MM_SHARP product IDs
  // ====================
  MM_SHARP_MDC_MIDI_SYNTH             = 1;
  {$EXTERNALSYM MM_SHARP_MDC_MIDI_SYNTH}
  MM_SHARP_MDC_MIDI_IN                = 2;
  {$EXTERNALSYM MM_SHARP_MDC_MIDI_IN}
  MM_SHARP_MDC_MIDI_OUT               = 3;
  {$EXTERNALSYM MM_SHARP_MDC_MIDI_OUT}
  MM_SHARP_MDC_WAVE_IN                = 4;
  {$EXTERNALSYM MM_SHARP_MDC_WAVE_IN}
  MM_SHARP_MDC_WAVE_OUT               = 5;
  {$EXTERNALSYM MM_SHARP_MDC_WAVE_OUT}
  MM_SHARP_MDC_AUX                    = 6;
  {$EXTERNALSYM MM_SHARP_MDC_AUX}
  MM_SHARP_MDC_MIXER                  = 10;
  {$EXTERNALSYM MM_SHARP_MDC_MIXER}
  MM_SHARP_MDC_AUX_MASTER             = 100;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_MASTER}
  MM_SHARP_MDC_AUX_BASS               = 101;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_BASS}
  MM_SHARP_MDC_AUX_TREBLE             = 102;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_TREBLE}
  MM_SHARP_MDC_AUX_MIDI_VOL           = 103;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_MIDI_VOL}
  MM_SHARP_MDC_AUX_WAVE_VOL           = 104;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_WAVE_VOL}
  MM_SHARP_MDC_AUX_WAVE_RVB           = 105;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_WAVE_RVB}
  MM_SHARP_MDC_AUX_WAVE_CHR           = 106;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_WAVE_CHR}
  MM_SHARP_MDC_AUX_VOL                = 107;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_VOL}
  MM_SHARP_MDC_AUX_RVB                = 108;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_RVB}
  MM_SHARP_MDC_AUX_CHR                = 109;
  {$EXTERNALSYM MM_SHARP_MDC_AUX_CHR}


  // MM_LUCENT product IDs
  // =====================
  MM_LUCENT_ACM_G723                  = 0;
  {$EXTERNALSYM MM_LUCENT_ACM_G723}


  // MM_ATT product IDs
  // ==================
  MM_ATT_G729A                        = 1;
  {$EXTERNALSYM MM_ATT_G729A}


  // MM_MARIAN product IDs
  // =====================
  MM_MARIAN_ARC44WAVEIN               = 1;
  {$EXTERNALSYM MM_MARIAN_ARC44WAVEIN}
  MM_MARIAN_ARC44WAVEOUT              = 2;
  {$EXTERNALSYM MM_MARIAN_ARC44WAVEOUT}
  MM_MARIAN_PRODIF24WAVEIN            = 3;
  {$EXTERNALSYM MM_MARIAN_PRODIF24WAVEIN}
  MM_MARIAN_PRODIF24WAVEOUT           = 4;
  {$EXTERNALSYM MM_MARIAN_PRODIF24WAVEOUT}
  MM_MARIAN_ARC88WAVEIN               = 5;
  {$EXTERNALSYM MM_MARIAN_ARC88WAVEIN}
  MM_MARIAN_ARC88WAVEOUT              = 6;
  {$EXTERNALSYM MM_MARIAN_ARC88WAVEOUT}


  // MM_BCB product IDs
  // ==================
  MM_BCB_NETBOARD_10                  = 1;
  {$EXTERNALSYM MM_BCB_NETBOARD_10}
  MM_BCB_TT75_10                      = 2;
  {$EXTERNALSYM MM_BCB_TT75_10}


  // MM_MOTIONPIXELS product IDs
  // ===========================
  MM_MOTIONPIXELS_MVI2                = 1;
  {$EXTERNALSYM MM_MOTIONPIXELS_MVI2}


  // MM_QDESIGN product IDs
  // ======================
  MM_QDESIGN_ACM_MPEG                 = 1;
  {$EXTERNALSYM MM_QDESIGN_ACM_MPEG}
  MM_QDESIGN_ACM_QDESIGN_MUSIC        = 2;
  {$EXTERNALSYM MM_QDESIGN_ACM_QDESIGN_MUSIC}


  // MM_NMP product IDs
  // ==================
  MM_NMP_CCP_WAVEIN                   = 1;
  {$EXTERNALSYM MM_NMP_CCP_WAVEIN}
  MM_NMP_CCP_WAVEOUT                  = 2;
  {$EXTERNALSYM MM_NMP_CCP_WAVEOUT}
  MM_NMP_ACM_AMR                      = 10;
  {$EXTERNALSYM MM_NMP_ACM_AMR}


  // MM_DATAFUSION product IDs
  // =========================
  MM_DF_ACM_G726                      = 1;
  {$EXTERNALSYM MM_DF_ACM_G726}
  MM_DF_ACM_GSM610                    = 2;
  {$EXTERNALSYM MM_DF_ACM_GSM610}


  // MM_BERCOS product IDs
  // =====================
  MM_BERCOS_WAVEIN                    = 1;
  {$EXTERNALSYM MM_BERCOS_WAVEIN}
  MM_BERCOS_MIXER                     = 2;
  {$EXTERNALSYM MM_BERCOS_MIXER}
  MM_BERCOS_WAVEOUT                   = 3;
  {$EXTERNALSYM MM_BERCOS_WAVEOUT}


  // MM_ONLIVE product IDs
  // =====================
  MM_ONLIVE_MPCODEC                   = 1;
  {$EXTERNALSYM MM_ONLIVE_MPCODEC}


  // MM_PHONET product IDs
  // =====================
  MM_PHONET_PP_WAVEOUT                = 1;
  {$EXTERNALSYM MM_PHONET_PP_WAVEOUT}
  MM_PHONET_PP_WAVEIN                 = 2;
  {$EXTERNALSYM MM_PHONET_PP_WAVEIN}
  MM_PHONET_PP_MIXER                  = 3;
  {$EXTERNALSYM MM_PHONET_PP_MIXER}


  // MM_FTR product IDs
  // ==================
  MM_FTR_ENCODER_WAVEIN               = 1;
  {$EXTERNALSYM MM_FTR_ENCODER_WAVEIN}
  MM_FTR_ACM                          = 2;
  {$EXTERNALSYM MM_FTR_ACM}


  // MM_ENET product IDs
  // ===================
  MM_ENET_T2000_LINEIN                = 1;
  {$EXTERNALSYM MM_ENET_T2000_LINEIN}
  MM_ENET_T2000_LINEOUT               = 2;
  {$EXTERNALSYM MM_ENET_T2000_LINEOUT}
  MM_ENET_T2000_HANDSETIN             = 3;
  {$EXTERNALSYM MM_ENET_T2000_HANDSETIN}
  MM_ENET_T2000_HANDSETOUT            = 4;
  {$EXTERNALSYM MM_ENET_T2000_HANDSETOUT}


  //  MM_EMAGIC product IDs
  // ======================
  MM_EMAGIC_UNITOR8                   = 1;
  {$EXTERNALSYM MM_EMAGIC_UNITOR8}


  //  MM_SIPROLAB product IDs
  // ========================
  MM_SIPROLAB_ACELPNET                = 1;
  {$EXTERNALSYM MM_SIPROLAB_ACELPNET}


  //  MM_DICTAPHONE product IDs
  // ==========================
  MM_DICTAPHONE_G726                  = 1;  // G726 ACM codec (g726pcm.acm)
  {$EXTERNALSYM MM_DICTAPHONE_G726}


  //  MM_RZS product IDs
  // ===================
  MM_RZS_ACM_TUBGSM                   = 1;  // GSM 06.10 CODEC
  {$EXTERNALSYM MM_RZS_ACM_TUBGSM}


  //  MM_EES product IDs
  // ===================
  MM_EES_PCMIDI14                     = 1;
  {$EXTERNALSYM MM_EES_PCMIDI14}
  MM_EES_PCMIDI14_IN                  = 2;
  {$EXTERNALSYM MM_EES_PCMIDI14_IN}
  MM_EES_PCMIDI14_OUT1                = 3;
  {$EXTERNALSYM MM_EES_PCMIDI14_OUT1}
  MM_EES_PCMIDI14_OUT2                = 4;
  {$EXTERNALSYM MM_EES_PCMIDI14_OUT2}
  MM_EES_PCMIDI14_OUT3                = 5;
  {$EXTERNALSYM MM_EES_PCMIDI14_OUT3}
  MM_EES_PCMIDI14_OUT4                = 6;
  {$EXTERNALSYM MM_EES_PCMIDI14_OUT4}


  //  MM_HAFTMANN product IDs
  // ========================
  MM_HAFTMANN_LPTDAC2                 = 1;
  {$EXTERNALSYM MM_HAFTMANN_LPTDAC2}


  //  MM_LUCID product IDs
  // =====================
  MM_LUCID_PCI24WAVEIN                = 1;
  {$EXTERNALSYM MM_LUCID_PCI24WAVEIN}
  MM_LUCID_PCI24WAVEOUT               = 2;
  {$EXTERNALSYM MM_LUCID_PCI24WAVEOUT}


  //  MM_HEADSPACE product IDs
  // =========================
  MM_HEADSPACE_HAESYNTH               = 1;
  {$EXTERNALSYM MM_HEADSPACE_HAESYNTH}
  MM_HEADSPACE_HAEWAVEOUT             = 2;
  {$EXTERNALSYM MM_HEADSPACE_HAEWAVEOUT}
  MM_HEADSPACE_HAEWAVEIN              = 3;
  {$EXTERNALSYM MM_HEADSPACE_HAEWAVEIN}
  MM_HEADSPACE_HAEMIXER               = 4;
  {$EXTERNALSYM MM_HEADSPACE_HAEMIXER}


  //  MM_UNISYS product IDs
  // ======================
  MM_UNISYS_ACM_NAP                   = 1;
  {$EXTERNALSYM MM_UNISYS_ACM_NAP}


  //  MM_LUMINOSITI product IDs
  // ==========================
  MM_LUMINOSITI_SCWAVEIN              = 1;
  {$EXTERNALSYM MM_LUMINOSITI_SCWAVEIN}
  MM_LUMINOSITI_SCWAVEOUT             = 2;
  {$EXTERNALSYM MM_LUMINOSITI_SCWAVEOUT}
  MM_LUMINOSITI_SCWAVEMIX             = 3;
  {$EXTERNALSYM MM_LUMINOSITI_SCWAVEMIX}


  //  MM_ACTIVEVOICE product IDs
  // ===========================
  MM_ACTIVEVOICE_ACM_VOXADPCM         = 1;
  {$EXTERNALSYM MM_ACTIVEVOICE_ACM_VOXADPCM}


  //  MM_DTS product IDs
  // ===================
  MM_DTS_DS                           = 1;
  {$EXTERNALSYM MM_DTS_DS}


  //  MM_SOFTLAB_NSK product IDs
  // ===========================
  MM_SOFTLAB_NSK_FRW_WAVEIN           = 1;
  {$EXTERNALSYM MM_SOFTLAB_NSK_FRW_WAVEIN}
  MM_SOFTLAB_NSK_FRW_WAVEOUT          = 2;
  {$EXTERNALSYM MM_SOFTLAB_NSK_FRW_WAVEOUT}
  MM_SOFTLAB_NSK_FRW_MIXER            = 3;
  {$EXTERNALSYM MM_SOFTLAB_NSK_FRW_MIXER}
  MM_SOFTLAB_NSK_FRW_AUX              = 4;
  {$EXTERNALSYM MM_SOFTLAB_NSK_FRW_AUX}


  //  MM_FORTEMEDIA product IDs
  // ==========================
  MM_FORTEMEDIA_WAVEIN                = 1;
  {$EXTERNALSYM MM_FORTEMEDIA_WAVEIN}
  MM_FORTEMEDIA_WAVEOUT               = 2;
  {$EXTERNALSYM MM_FORTEMEDIA_WAVEOUT}
  MM_FORTEMEDIA_FMSYNC                = 3;
  {$EXTERNALSYM MM_FORTEMEDIA_FMSYNC}
  MM_FORTEMEDIA_MIXER                 = 4;
  {$EXTERNALSYM MM_FORTEMEDIA_MIXER}
  MM_FORTEMEDIA_AUX                   = 5;
  {$EXTERNALSYM MM_FORTEMEDIA_AUX}


  //  MM_SONORUS product IDs
  // =======================
  MM_SONORUS_STUDIO                   = 1;
  {$EXTERNALSYM MM_SONORUS_STUDIO}


  //  MM_I_LINK product IDs
  // ======================
  MM_I_LINK_VOICE_CODER               = 1;
  {$EXTERNALSYM MM_I_LINK_VOICE_CODER}


  //  MM_SELSIUS_SYSTEMS product IDs
  // ===============================
  MM_SELSIUS_SYSTEMS_RTPWAVEOUT       = 1;
  {$EXTERNALSYM MM_SELSIUS_SYSTEMS_RTPWAVEOUT}
  MM_SELSIUS_SYSTEMS_RTPWAVEIN        = 2;
  {$EXTERNALSYM MM_SELSIUS_SYSTEMS_RTPWAVEIN}


  //  MM_ADMOS product IDs
  // =====================
  MM_ADMOS_FM_SYNTH                   = 1;
  {$EXTERNALSYM MM_ADMOS_FM_SYNTH}
  MM_ADMOS_QS3AMIDIOUT                = 2;
  {$EXTERNALSYM MM_ADMOS_QS3AMIDIOUT}
  MM_ADMOS_QS3AMIDIIN                 = 3;
  {$EXTERNALSYM MM_ADMOS_QS3AMIDIIN}
  MM_ADMOS_QS3AWAVEOUT                = 4;
  {$EXTERNALSYM MM_ADMOS_QS3AWAVEOUT}
  MM_ADMOS_QS3AWAVEIN                 = 5;
  {$EXTERNALSYM MM_ADMOS_QS3AWAVEIN}


  // MM_LEXICON product IDs
  // ==================
  MM_LEXICON_STUDIO_WAVE_OUT          = 1;
  {$EXTERNALSYM MM_LEXICON_STUDIO_WAVE_OUT}
  MM_LEXICON_STUDIO_WAVE_IN           = 2;
  {$EXTERNALSYM MM_LEXICON_STUDIO_WAVE_IN}


  // MM_SGI product IDs
  // ==================
  MM_SGI_320_WAVEIN                   = 1;
  {$EXTERNALSYM MM_SGI_320_WAVEIN}
  MM_SGI_320_WAVEOUT                  = 2;
  {$EXTERNALSYM MM_SGI_320_WAVEOUT}
  MM_SGI_320_MIXER                    = 3;
  {$EXTERNALSYM MM_SGI_320_MIXER}
  MM_SGI_540_WAVEIN                   = 4;
  {$EXTERNALSYM MM_SGI_540_WAVEIN}
  MM_SGI_540_WAVEOUT                  = 5;
  {$EXTERNALSYM MM_SGI_540_WAVEOUT}
  MM_SGI_540_MIXER                    = 6;
  {$EXTERNALSYM MM_SGI_540_MIXER}
  MM_SGI_RAD_ADATMONO1_WAVEIN         = 7;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO1_WAVEIN}
  MM_SGI_RAD_ADATMONO2_WAVEIN         = 8;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO2_WAVEIN}
  MM_SGI_RAD_ADATMONO3_WAVEIN         = 9;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO3_WAVEIN}
  MM_SGI_RAD_ADATMONO4_WAVEIN         = 10;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO4_WAVEIN}
  MM_SGI_RAD_ADATMONO5_WAVEIN         = 11;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO5_WAVEIN}
  MM_SGI_RAD_ADATMONO6_WAVEIN         = 12;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO6_WAVEIN}
  MM_SGI_RAD_ADATMONO7_WAVEIN         = 13;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO7_WAVEIN}
  MM_SGI_RAD_ADATMONO8_WAVEIN         = 14;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO8_WAVEIN}
  MM_SGI_RAD_ADATSTEREO12_WAVEIN      = 15;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO12_WAVEIN}
  MM_SGI_RAD_ADATSTEREO34_WAVEIN      = 16;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO34_WAVEIN}
  MM_SGI_RAD_ADATSTEREO56_WAVEIN      = 17;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO56_WAVEIN}
  MM_SGI_RAD_ADATSTEREO78_WAVEIN      = 18;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO78_WAVEIN}
  MM_SGI_RAD_ADAT8CHAN_WAVEIN         = 19;
  {$EXTERNALSYM MM_SGI_RAD_ADAT8CHAN_WAVEIN}
  MM_SGI_RAD_ADATMONO1_WAVEOUT        = 20;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO1_WAVEOUT}
  MM_SGI_RAD_ADATMONO2_WAVEOUT        = 21;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO2_WAVEOUT}
  MM_SGI_RAD_ADATMONO3_WAVEOUT        = 22;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO3_WAVEOUT}
  MM_SGI_RAD_ADATMONO4_WAVEOUT        = 23;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO4_WAVEOUT}
  MM_SGI_RAD_ADATMONO5_WAVEOUT        = 24;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO5_WAVEOUT}
  MM_SGI_RAD_ADATMONO6_WAVEOUT        = 25;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO6_WAVEOUT}
  MM_SGI_RAD_ADATMONO7_WAVEOUT        = 26;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO7_WAVEOUT}
  MM_SGI_RAD_ADATMONO8_WAVEOUT        = 27;
  {$EXTERNALSYM MM_SGI_RAD_ADATMONO8_WAVEOUT}
  MM_SGI_RAD_ADATSTEREO12_WAVEOUT     = 28;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO12_WAVEOUT}
  MM_SGI_RAD_ADATSTEREO32_WAVEOUT     = 29;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO32_WAVEOUT}
  MM_SGI_RAD_ADATSTEREO56_WAVEOUT     = 30;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO56_WAVEOUT}
  MM_SGI_RAD_ADATSTEREO78_WAVEOUT     = 31;
  {$EXTERNALSYM MM_SGI_RAD_ADATSTEREO78_WAVEOUT}
  MM_SGI_RAD_ADAT8CHAN_WAVEOUT        = 32;
  {$EXTERNALSYM MM_SGI_RAD_ADAT8CHAN_WAVEOUT}
  MM_SGI_RAD_AESMONO1_WAVEIN          = 33;
  {$EXTERNALSYM MM_SGI_RAD_AESMONO1_WAVEIN}
  MM_SGI_RAD_AESMONO2_WAVEIN          = 34;
  {$EXTERNALSYM MM_SGI_RAD_AESMONO2_WAVEIN}
  MM_SGI_RAD_AESSTEREO_WAVEIN         = 35;
  {$EXTERNALSYM MM_SGI_RAD_AESSTEREO_WAVEIN}
  MM_SGI_RAD_AESMONO1_WAVEOUT         = 36;
  {$EXTERNALSYM MM_SGI_RAD_AESMONO1_WAVEOUT}
  MM_SGI_RAD_AESMONO2_WAVEOUT         = 37;
  {$EXTERNALSYM MM_SGI_RAD_AESMONO2_WAVEOUT}
  MM_SGI_RAD_AESSTEREO_WAVEOUT        = 38;
  {$EXTERNALSYM MM_SGI_RAD_AESSTEREO_WAVEOUT}


  // MM_IPI product IDs
  // ==================
  MM_IPI_ACM_HSX                      = 1;
  {$EXTERNALSYM MM_IPI_ACM_HSX}
  MM_IPI_ACM_RPELP                    = 2;
  {$EXTERNALSYM MM_IPI_ACM_RPELP}
  MM_IPI_WF_ASSS                      = 3;
  {$EXTERNALSYM MM_IPI_WF_ASSS}
  MM_IPI_AT_WAVEOUT                   = 4;
  {$EXTERNALSYM MM_IPI_AT_WAVEOUT}
  MM_IPI_AT_WAVEIN                    = 5;
  {$EXTERNALSYM MM_IPI_AT_WAVEIN}
  MM_IPI_AT_MIXER                     = 6;
  {$EXTERNALSYM MM_IPI_AT_MIXER}


  // MM_ICE product IDs
  // ==================
  MM_ICE_WAVEOUT                      = 1;
  {$EXTERNALSYM MM_ICE_WAVEOUT}
  MM_ICE_WAVEIN                       = 2;
  {$EXTERNALSYM MM_ICE_WAVEIN}
  MM_ICE_MTWAVEOUT                    = 3;
  {$EXTERNALSYM MM_ICE_MTWAVEOUT}
  MM_ICE_MTWAVEIN                     = 4;
  {$EXTERNALSYM MM_ICE_MTWAVEIN}
  MM_ICE_MIDIOUT1                     = 5;
  {$EXTERNALSYM MM_ICE_MIDIOUT1}
  MM_ICE_MIDIIN1                      = 6;
  {$EXTERNALSYM MM_ICE_MIDIIN1}
  MM_ICE_MIDIOUT2                     = 7;
  {$EXTERNALSYM MM_ICE_MIDIOUT2}
  MM_ICE_MIDIIN2                      = 8;
  {$EXTERNALSYM MM_ICE_MIDIIN2}
  MM_ICE_SYNTH                        = 9;
  {$EXTERNALSYM MM_ICE_SYNTH}
  MM_ICE_MIXER                        = 10;
  {$EXTERNALSYM MM_ICE_MIXER}
  MM_ICE_AUX                          = 11;
  {$EXTERNALSYM MM_ICE_AUX}


  // MM_VQST product IDs
  // ===================
  MM_VQST_VQC1                        = 1;
  {$EXTERNALSYM MM_VQST_VQC1}
  MM_VQST_VQC2                        = 2;
  {$EXTERNALSYM MM_VQST_VQC2}


  // MM_ETEK product IDs
  // ===================
  MM_ETEK_KWIKMIDI_MIDIIN             = 1;
  {$EXTERNALSYM MM_ETEK_KWIKMIDI_MIDIIN}
  MM_ETEK_KWIKMIDI_MIDIOUT            = 2;
  {$EXTERNALSYM MM_ETEK_KWIKMIDI_MIDIOUT}


  // MM_INTERNET product IDs
  // =======================
  MM_INTERNET_SSW_MIDIOUT             = 10;
  {$EXTERNALSYM MM_INTERNET_SSW_MIDIOUT}
  MM_INTERNET_SSW_MIDIIN              = 11;
  {$EXTERNALSYM MM_INTERNET_SSW_MIDIIN}
  MM_INTERNET_SSW_WAVEOUT             = 12;
  {$EXTERNALSYM MM_INTERNET_SSW_WAVEOUT}
  MM_INTERNET_SSW_WAVEIN              = 13;
  {$EXTERNALSYM MM_INTERNET_SSW_WAVEIN}


  // MM_SONY product IDs
  // ===================
  MM_SONY_ACM_SCX                     = 1;
  {$EXTERNALSYM MM_SONY_ACM_SCX}


  // MM_UHER_INFORMATIC product IDs
  // ==============================
  MM_UH_ACM_ADPCM                     = 1;
  {$EXTERNALSYM MM_UH_ACM_ADPCM}


  // MM_SYDEC_NV product IDs
  // =======================
  MM_SYDEC_NV_WAVEIN                  = 1;
  {$EXTERNALSYM MM_SYDEC_NV_WAVEIN}
  MM_SYDEC_NV_WAVEOUT                 = 2;
  {$EXTERNALSYM MM_SYDEC_NV_WAVEOUT}


  // MM_FLEXION product IDs
  // ======================
  MM_FLEXION_X300_WAVEIN              = 1;
  {$EXTERNALSYM MM_FLEXION_X300_WAVEIN}
  MM_FLEXION_X300_WAVEOUT             = 2;
  {$EXTERNALSYM MM_FLEXION_X300_WAVEOUT}


  // MM_VIA product IDs
  // ==================
  MM_VIA_WAVEOUT                      = 1;
  {$EXTERNALSYM MM_VIA_WAVEOUT}
  MM_VIA_WAVEIN                       = 2;
  {$EXTERNALSYM MM_VIA_WAVEIN}
  MM_VIA_MIXER                        = 3;
  {$EXTERNALSYM MM_VIA_MIXER}
  MM_VIA_AUX                          = 4;
  {$EXTERNALSYM MM_VIA_AUX}
  MM_VIA_MPU401_MIDIOUT               = 5;
  {$EXTERNALSYM MM_VIA_MPU401_MIDIOUT}
  MM_VIA_MPU401_MIDIIN                = 6;
  {$EXTERNALSYM MM_VIA_MPU401_MIDIIN}
  MM_VIA_SWFM_SYNTH                   = 7;
  {$EXTERNALSYM MM_VIA_SWFM_SYNTH}
  MM_VIA_WDM_WAVEOUT                  = 8;
  {$EXTERNALSYM MM_VIA_WDM_WAVEOUT}
  MM_VIA_WDM_WAVEIN                   = 9;
  {$EXTERNALSYM MM_VIA_WDM_WAVEIN}
  MM_VIA_WDM_MIXER                    = 10;
  {$EXTERNALSYM MM_VIA_WDM_MIXER}
  MM_VIA_WDM_MPU401_MIDIOUT           = 11;
  {$EXTERNALSYM MM_VIA_WDM_MPU401_MIDIOUT}
  MM_VIA_WDM_MPU401_MIDIIN            = 12;
  {$EXTERNALSYM MM_VIA_WDM_MPU401_MIDIIN}


  // MM_MICRONAS product IDs
  // =======================
  MM_MICRONAS_SC4                     = 1;
  {$EXTERNALSYM MM_MICRONAS_SC4}
  MM_MICRONAS_CLP833                  = 2;
  {$EXTERNALSYM MM_MICRONAS_CLP833}


  // MM_HP product IDs
  // =================
  MM_HP_WAVEOUT                       = 1;
  {$EXTERNALSYM MM_HP_WAVEOUT}
  MM_HP_WAVEIN                        = 2;
  {$EXTERNALSYM MM_HP_WAVEIN}


  // MM_QUICKAUDIO product IDs
  // =========================
  MM_QUICKAUDIO_MINIMIDI              = 1;
  {$EXTERNALSYM MM_QUICKAUDIO_MINIMIDI}
  MM_QUICKAUDIO_MAXIMIDI              = 2;
  {$EXTERNALSYM MM_QUICKAUDIO_MAXIMIDI}


  // MM_ICCC product IDs
  // ===================
  MM_ICCC_UNA3_WAVEIN                 = 1;
  {$EXTERNALSYM MM_ICCC_UNA3_WAVEIN}
  MM_ICCC_UNA3_WAVEOUT                = 2;
  {$EXTERNALSYM MM_ICCC_UNA3_WAVEOUT}
  MM_ICCC_UNA3_AUX                    = 3;
  {$EXTERNALSYM MM_ICCC_UNA3_AUX}
  MM_ICCC_UNA3_MIXER                  = 4;
  {$EXTERNALSYM MM_ICCC_UNA3_MIXER}


  // MM_3COM product IDs
  // ===================
  MM_3COM_CB_MIXER                    = 1;
  {$EXTERNALSYM MM_3COM_CB_MIXER}
  MM_3COM_CB_WAVEIN                   = 2;
  {$EXTERNALSYM MM_3COM_CB_WAVEIN}
  MM_3COM_CB_WAVEOUT                  = 3;
  {$EXTERNALSYM MM_3COM_CB_WAVEOUT}


  // MM_MINDMAKER product IDs
  // ========================
  MM_MINDMAKER_GC_WAVEIN              = 1;
  {$EXTERNALSYM MM_MINDMAKER_GC_WAVEIN}
  MM_MINDMAKER_GC_WAVEOUT             = 2;
  {$EXTERNALSYM MM_MINDMAKER_GC_WAVEOUT}
  MM_MINDMAKER_GC_MIXER               = 3;
  {$EXTERNALSYM MM_MINDMAKER_GC_MIXER}


  // MM_TELEKOL product IDs
  // ======================
  MM_TELEKOL_WAVEOUT                  = 1;
  {$EXTERNALSYM MM_TELEKOL_WAVEOUT}
  MM_TELEKOL_WAVEIN                   = 2;
  {$EXTERNALSYM MM_TELEKOL_WAVEIN}


  // MM_ALGOVISION product IDs
  // =========================
  MM_ALGOVISION_VB80WAVEOUT           = 1;
  {$EXTERNALSYM MM_ALGOVISION_VB80WAVEOUT}
  MM_ALGOVISION_VB80WAVEIN            = 2;
  {$EXTERNALSYM MM_ALGOVISION_VB80WAVEIN}
  MM_ALGOVISION_VB80MIXER             = 3;
  {$EXTERNALSYM MM_ALGOVISION_VB80MIXER}
  MM_ALGOVISION_VB80AUX               = 4;
  {$EXTERNALSYM MM_ALGOVISION_VB80AUX}
  MM_ALGOVISION_VB80AUX2              = 5;
  {$EXTERNALSYM MM_ALGOVISION_VB80AUX2}




  //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  // Vendor media/product ID stuff omitted
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



//------------------------------------------------------------------------------
//
//              INFO LIST CHUNKS (from the Multimedia Programmer's Reference
//                                plus new ones)
//
//------------------------------------------------------------------------------
{$IFNDEF MAKEFOURCC}
  function MAKEFOURCC(const ch0: AnsiChar;
                      const ch1: AnsiChar;
                      const ch2: AnsiChar;
                      const ch3: AnsiChar): FOURCC; inline;
{$DEFINE MAKEFOURCC}
{$ENDIF}

const

  //Tony: To simulate the MAKEFOURCC macro, we directly translate the code.
  //      For example: RIFFINFO_IARL = MAKEFOURCC(FOURCC ('I', 'A', 'R', 'L')) is not possible - generates
  //      E2026 Constant expression expected error -

  RIFFINFO_IARL = ord('I') or (ord('A') shl 8) or (ord('R') shl 16) or (ord('L') shl 24); // Archival location
  {$EXTERNALSYM RIFFINFO_IARL}
  RIFFINFO_IART = ord('I') or (ord('A') shl 8) or (ord('R') shl 16) or (ord('T') shl 24); // Artist
  {$EXTERNALSYM RIFFINFO_IART}
  RIFFINFO_ICMS = ord('I') or (ord('C') shl 8) or (ord('M') shl 16) or (ord('S') shl 24); // Commissioned
  {$EXTERNALSYM RIFFINFO_ICMS}
  RIFFINFO_ICMT = ord('I') or (ord('C') shl 8) or (ord('M') shl 16) or (ord('T') shl 24); // Comments
  {$EXTERNALSYM RIFFINFO_ICMT}
  RIFFINFO_ICOP = ord('I') or (ord('C') shl 8) or (ord('O') shl 16) or (ord('P') shl 24); // Copyright
  {$EXTERNALSYM RIFFINFO_ICOP}
  RIFFINFO_ICRD = ord('I') or (ord('C') shl 8) or (ord('R') shl 16) or (ord('D') shl 24); // Creation date of subject
  {$EXTERNALSYM RIFFINFO_ICRD}
  RIFFINFO_ICRP = ord('I') or (ord('C') shl 8) or (ord('R') shl 16) or (ord('P') shl 24); // Cropped
  {$EXTERNALSYM RIFFINFO_ICRP}
  RIFFINFO_IDIM = ord('I') or (ord('D') shl 8) or (ord('I') shl 16) or (ord('M') shl 24); // Dimensions
  {$EXTERNALSYM RIFFINFO_IDIM}
  RIFFINFO_IDPI = ord('I') or (ord('D') shl 8) or (ord('P') shl 16) or (ord('I') shl 24); // Dots per inch
  {$EXTERNALSYM RIFFINFO_IDPI}
  RIFFINFO_IENG = ord('I') or (ord('E') shl 8) or (ord('N') shl 16) or (ord('G') shl 24); // Engineer
  {$EXTERNALSYM RIFFINFO_IENG}
  RIFFINFO_IGNR = ord('I') or (ord('G') shl 8) or (ord('N') shl 16) or (ord('R') shl 24); // Genre
  {$EXTERNALSYM RIFFINFO_IGNR}
  RIFFINFO_IKEY = ord('I') or (ord('K') shl 8) or (ord('E') shl 16) or (ord('Y') shl 24); // Keywords
  {$EXTERNALSYM RIFFINFO_IKEY}
  RIFFINFO_ILGT = ord('I') or (ord('L') shl 8) or (ord('G') shl 16) or (ord('T') shl 24); // Lightness settings
  {$EXTERNALSYM RIFFINFO_ILGT}
  RIFFINFO_IMED = ord('I') or (ord('M') shl 8) or (ord('E') shl 16) or (ord('D') shl 24); // Medium
  {$EXTERNALSYM RIFFINFO_IMED}
  RIFFINFO_INAM = ord('I') or (ord('N') shl 8) or (ord('A') shl 16) or (ord('M') shl 24); // Name of subject
  {$EXTERNALSYM RIFFINFO_INAM}
  RIFFINFO_IPLT = ord('I') or (ord('P') shl 8) or (ord('L') shl 16) or (ord('T') shl 24); // Palette Settings. No. of colors requested.
  {$EXTERNALSYM RIFFINFO_IPLT}
  RIFFINFO_IPRD = ord('I') or (ord('P') shl 8) or (ord('R') shl 16) or (ord('D') shl 24); // Product
  {$EXTERNALSYM RIFFINFO_IPRD}
  RIFFINFO_ISBJ = ord('I') or (ord('S') shl 8) or (ord('B') shl 16) or (ord('J') shl 24); // Subject description
  {$EXTERNALSYM RIFFINFO_ISBJ}
  RIFFINFO_ISFT = ord('I') or (ord('S') shl 8) or (ord('F') shl 16) or (ord('T') shl 24); // Software. Name of package used to create file.
  {$EXTERNALSYM RIFFINFO_ISFT}
  RIFFINFO_ISHP = ord('I') or (ord('S') shl 8) or (ord('H') shl 16) or (ord('P') shl 24); // Sharpness.
  {$EXTERNALSYM RIFFINFO_ISHP}
  RIFFINFO_ISRC = ord('I') or (ord('S') shl 8) or (ord('R') shl 16) or (ord('C') shl 24); // Source.
  {$EXTERNALSYM RIFFINFO_ISRC}
  RIFFINFO_ISRF = ord('I') or (ord('S') shl 8) or (ord('R') shl 16) or (ord('F') shl 24); // Source Form. ie slide, paper
  {$EXTERNALSYM RIFFINFO_ISRF}
  RIFFINFO_ITCH = ord('I') or (ord('T') shl 8) or (ord('C') shl 16) or (ord('H') shl 24); // Technician who digitized the subject.
  {$EXTERNALSYM RIFFINFO_ITCH}

  // New INFO Chunks as of August 30, 1993:
  //---------------------------------------

  RIFFINFO_ISMP = ord('I') or (ord('S') shl 8) or (ord('M') shl 16) or (ord('P') shl 24); // SMPTE time code
  {$EXTERNALSYM RIFFINFO_ISMP}
  // ISMP: SMPTE time code of digitization start point expressed as a NULL terminated
  //       text string "HH:MM:SS:FF". If performing MCI capture in AVICAP, this
  //       chunk will be automatically set based on the MCI start time.

  RIFFINFO_IDIT = ord('I') or (ord('D') shl 8) or (ord('I') shl 16) or (ord('T') shl 24); // Digitization Time
  {$EXTERNALSYM RIFFINFO_IDIT}
  // IDIT: "Digitization Time" Specifies the time and date that the digitization commenced.
  //       The digitization time is contained in an ASCII string which
  //       contains exactly 26 characters and is in the format
  //       "Wed Jan 02 02:03:55 1990\n\0".
  //       The ctime(), asctime(), functions can be used to create strings
  //       in this format. This chunk is automatically added to the capture
  //       file based on the current system time at the moment capture is initiated.


  RIFFINFO_ITRK = ord('I') or (ord('T') shl 8) or (ord('R') shl 16) or (ord('K') shl 24); // ASCIIZ representation of the 1-based track number of the content.
  {$EXTERNALSYM RIFFINFO_ITRK}
  RIFFINFO_ITOC = ord('T') or (ord('T') shl 8) or (ord('O') shl 16) or (ord('C') shl 24); // A dump of the table of contents from the CD the content originated from.
  {$EXTERNALSYM RIFFINFO_ITOC}


  // Template line for new additions
  // = ord('') or (ord('') shl 8) or (ord('') shl 16) or (ord('');

// -----------------------------------------------------------------------------


const

  // WAVE form wFormatTag IDs
  // ========================

  WAVE_FORMAT_UNKNOWN               = $0000;  // Microsoft Corporation
  {$EXTERNALSYM WAVE_FORMAT_UNKNOWN}
  WAVE_FORMAT_PCM                   = $0001;  // updt 100812
  {$EXTERNALSYM WAVE_FORMAT_PCM}
  WAVE_FORMAT_ADPCM                 = $0002;  // Microsoft Corporation
  {$EXTERNALSYM WAVE_FORMAT_ADPCM}
  WAVE_FORMAT_IEEE_FLOAT            = $0003;  // Microsoft Corporation
  {$EXTERNALSYM WAVE_FORMAT_IEEE_FLOAT}
                                              //  IEEE754: range (+1, -1]
                                              //  32-bit/64-bit format as defined by
                                              //  MSVC++ float/double type
  WAVE_FORMAT_VSELP                 = $0004;  // Compaq Computer Corp. //
  {$EXTERNALSYM WAVE_FORMAT_VSELP}
  WAVE_FORMAT_IBM_CVSD              = $0005;  // IBM Corporation
  {$EXTERNALSYM WAVE_FORMAT_IBM_CVSD}
  WAVE_FORMAT_ALAW                  = $0006;  // Microsoft Corporation
  {$EXTERNALSYM WAVE_FORMAT_ALAW}
  WAVE_FORMAT_MULAW                 = $0007;  // Microsoft Corporation
  {$EXTERNALSYM WAVE_FORMAT_MULAW}
  WAVE_FORMAT_DTS                   = $0008;  // Microsoft Corporation //
  {$EXTERNALSYM WAVE_FORMAT_DTS}
  WAVE_FORMAT_DRM                   = $0009;  // Microsoft Corporation //
  {$EXTERNALSYM WAVE_FORMAT_DRM}
  WAVE_FORMAT_WMAVOICE9             = $000A;  /// Microsoft Corporation ///updt 100812
  {$EXTERNALSYM WAVE_FORMAT_WMAVOICE9}
  WAVE_FORMAT_WMAVOICE10            = $000B;  /// Microsoft Corporation ///updt 100812
  {$EXTERNALSYM WAVE_FORMAT_WMAVOICE10}
  WAVE_FORMAT_OKI_ADPCM             = $0010;  // OKI
  {$EXTERNALSYM WAVE_FORMAT_OKI_ADPCM}
  WAVE_FORMAT_DVI_ADPCM             = $0011;  // Intel Corporation
  {$EXTERNALSYM WAVE_FORMAT_DVI_ADPCM}
  WAVE_FORMAT_IMA_ADPCM             = WAVE_FORMAT_DVI_ADPCM; // Intel Corporation
  {$EXTERNALSYM WAVE_FORMAT_IMA_ADPCM}
  WAVE_FORMAT_MEDIASPACE_ADPCM      = $0012;  // Videologic
  {$EXTERNALSYM WAVE_FORMAT_MEDIASPACE_ADPCM}
  WAVE_FORMAT_SIERRA_ADPCM          = $0013;  // Sierra Semiconductor Corp
  {$EXTERNALSYM WAVE_FORMAT_SIERRA_ADPCM}
  WAVE_FORMAT_G723_ADPCM            = $0014;  // Antex Electronics Corporation
  {$EXTERNALSYM WAVE_FORMAT_G723_ADPCM}
  WAVE_FORMAT_DIGISTD               = $0015;  // DSP Solutions, Inc.
  {$EXTERNALSYM WAVE_FORMAT_DIGISTD}
  WAVE_FORMAT_DIGIFIX               = $0016;  // DSP Solutions, Inc.
  {$EXTERNALSYM WAVE_FORMAT_DIGIFIX}
  WAVE_FORMAT_DIALOGIC_OKI_ADPCM    = $0017;  // Dialogic Corporation
  {$EXTERNALSYM WAVE_FORMAT_DIALOGIC_OKI_ADPCM}
  WAVE_FORMAT_MEDIAVISION_ADPCM     = $0018;  // Media Vision, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_MEDIAVISION_ADPCM}
  WAVE_FORMAT_CU_CODEC              = $0019;  // Hewlett-Packard Company //
  {$EXTERNALSYM WAVE_FORMAT_CU_CODEC}
  WAVE_FORMAT_YAMAHA_ADPCM          = $0020;  // Yamaha Corporation of America
  {$EXTERNALSYM WAVE_FORMAT_YAMAHA_ADPCM}
  WAVE_FORMAT_SONARC                = $0021;  // Speech Compression
  {$EXTERNALSYM WAVE_FORMAT_SONARC}
  WAVE_FORMAT_DSPGROUP_TRUESPEECH   = $0022;  // DSP Group, Inc
  {$EXTERNALSYM WAVE_FORMAT_DSPGROUP_TRUESPEECH}
  WAVE_FORMAT_ECHOSC1               = $0023;  // Echo Speech Corporation
  {$EXTERNALSYM WAVE_FORMAT_ECHOSC1}
  WAVE_FORMAT_AUDIOFILE_AF36        = $0024;  //
  {$EXTERNALSYM WAVE_FORMAT_AUDIOFILE_AF36}
  WAVE_FORMAT_APTX                  = $0025;  // Audio Processing Technology
  {$EXTERNALSYM WAVE_FORMAT_APTX}
  WAVE_FORMAT_AUDIOFILE_AF10        = $0026;  //
  {$EXTERNALSYM WAVE_FORMAT_AUDIOFILE_AF10}
  WAVE_FORMAT_PROSODY_1612          = $0027;  // Aculab plc //
  {$EXTERNALSYM WAVE_FORMAT_PROSODY_1612}
  WAVE_FORMAT_LRC                   = $0028;  // Merging Technologies S.A. //
  {$EXTERNALSYM WAVE_FORMAT_LRC}
  WAVE_FORMAT_DOLBY_AC2             = $0030;  // Dolby Laboratories
  {$EXTERNALSYM WAVE_FORMAT_DOLBY_AC2}
  WAVE_FORMAT_GSM610                = $0031;  // Microsoft Corporation
  {$EXTERNALSYM WAVE_FORMAT_GSM610}
  WAVE_FORMAT_MSNAUDIO              = $0032;  // Microsoft Corporation //
  {$EXTERNALSYM WAVE_FORMAT_MSNAUDIO}
  WAVE_FORMAT_ANTEX_ADPCME          = $0033;  // Antex Electronics Corporation
  {$EXTERNALSYM WAVE_FORMAT_ANTEX_ADPCME}
  WAVE_FORMAT_CONTROL_RES_VQLPC     = $0034;  // Control Resources Limited
  {$EXTERNALSYM WAVE_FORMAT_CONTROL_RES_VQLPC}
  WAVE_FORMAT_DIGIREAL              = $0035;  // DSP Solutions, Inc.
  {$EXTERNALSYM WAVE_FORMAT_DIGIREAL}
  WAVE_FORMAT_DIGIADPCM             = $0036;  // DSP Solutions, Inc.
  {$EXTERNALSYM WAVE_FORMAT_DIGIADPCM}
  WAVE_FORMAT_CONTROL_RES_CR10      = $0037;  // Control Resources Limited
  {$EXTERNALSYM WAVE_FORMAT_CONTROL_RES_CR10}
  WAVE_FORMAT_NMS_VBXADPCM          = $0038;  // Natural MicroSystems
  {$EXTERNALSYM WAVE_FORMAT_NMS_VBXADPCM}
  WAVE_FORMAT_CS_IMAADPCM           = $0039;  // Crystal Semiconductor IMA ADPCM
  {$EXTERNALSYM WAVE_FORMAT_CS_IMAADPCM}
  WAVE_FORMAT_ECHOSC3               = $003A;  // Echo Speech Corporation //
  {$EXTERNALSYM WAVE_FORMAT_ECHOSC3}
  WAVE_FORMAT_ROCKWELL_ADPCM        = $003B;  // Rockwell International //
  {$EXTERNALSYM WAVE_FORMAT_ROCKWELL_ADPCM}
  WAVE_FORMAT_ROCKWELL_DIGITALK     = $003C;  // Rockwell International //
  {$EXTERNALSYM WAVE_FORMAT_ROCKWELL_DIGITALK}
  WAVE_FORMAT_XEBEC                 = $003D;  // Xebec Multimedia Solutions Limited //
  {$EXTERNALSYM WAVE_FORMAT_XEBEC}
  WAVE_FORMAT_G721_ADPCM            = $0040;  // Antex Electronics Corporation
  {$EXTERNALSYM WAVE_FORMAT_G721_ADPCM}
  WAVE_FORMAT_G728_CELP             = $0041;  // Antex Electronics Corporation //
  {$EXTERNALSYM WAVE_FORMAT_G728_CELP}
  WAVE_FORMAT_MSG723                = $0042;  // Microsoft Corporation //
  {$EXTERNALSYM WAVE_FORMAT_MSG723}
  WAVE_FORMAT_MPEG                  = $0050;  // Microsoft Corporation
  {$EXTERNALSYM WAVE_FORMAT_MPEG}
  WAVE_FORMAT_RT24                  = $0052;  // InSoft, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_RT24}
  WAVE_FORMAT_PAC                   = $0053;  // InSoft, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_PAC}
  WAVE_FORMAT_MPEGLAYER3            = $0055;  // ISO/MPEG Layer3 Format Tag //
  {$EXTERNALSYM WAVE_FORMAT_MPEGLAYER3}
  WAVE_FORMAT_LUCENT_G723           = $0059;  // Lucent Technologies //
  {$EXTERNALSYM WAVE_FORMAT_LUCENT_G723}
  WAVE_FORMAT_CIRRUS                = $0060;  // Cirrus Logic //
  {$EXTERNALSYM WAVE_FORMAT_CIRRUS}
  WAVE_FORMAT_ESPCM                 = $0061;  // ESS Technology //
  {$EXTERNALSYM WAVE_FORMAT_ESPCM}
  WAVE_FORMAT_VOXWARE               = $0062;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE}
  WAVE_FORMAT_CANOPUS_ATRAC         = $0063;  // Canopus, co., Ltd. //
  {$EXTERNALSYM WAVE_FORMAT_CANOPUS_ATRAC}
  WAVE_FORMAT_G726_ADPCM            = $0064;  // APICOM //
  {$EXTERNALSYM WAVE_FORMAT_G726_ADPCM}
  WAVE_FORMAT_G722_ADPCM            = $0065;  // APICOM //
  {$EXTERNALSYM WAVE_FORMAT_G722_ADPCM}
  WAVE_FORMAT_DSAT_DISPLAY          = $0067;  // Microsoft Corporation //
  {$EXTERNALSYM WAVE_FORMAT_DSAT_DISPLAY}
  WAVE_FORMAT_VOXWARE_BYTE_ALIGNED  = $0069;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_BYTE_ALIGNED}
  WAVE_FORMAT_VOXWARE_AC8           = $0070;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_AC8}
  WAVE_FORMAT_VOXWARE_AC10          = $0071;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_AC10}
  WAVE_FORMAT_VOXWARE_AC16          = $0072;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_AC16}
  WAVE_FORMAT_VOXWARE_AC20          = $0073;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_AC20}
  WAVE_FORMAT_VOXWARE_RT24          = $0074;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_RT24}
  WAVE_FORMAT_VOXWARE_RT29          = $0075;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_RT29}
  WAVE_FORMAT_VOXWARE_RT29HW        = $0076;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_RT29HW}
  WAVE_FORMAT_VOXWARE_VR12          = $0077;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_VR12}
  WAVE_FORMAT_VOXWARE_VR18          = $0078;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_VR18}
  WAVE_FORMAT_VOXWARE_TQ40          = $0079;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_TQ40}
  WAVE_FORMAT_SOFTSOUND             = $0080;  // Softsound, Ltd. //
  {$EXTERNALSYM WAVE_FORMAT_SOFTSOUND}
  WAVE_FORMAT_VOXWARE_TQ60          = $0081;  // Voxware Inc //
  {$EXTERNALSYM WAVE_FORMAT_VOXWARE_TQ60}
  WAVE_FORMAT_MSRT24                = $0082;  // Microsoft Corporation //
  {$EXTERNALSYM WAVE_FORMAT_MSRT24}
  WAVE_FORMAT_G729A                 = $0083;  // AT&T Labs, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_G729A}
  WAVE_FORMAT_MVI_MVI2              = $0084;  // Motion Pixels //
  {$EXTERNALSYM WAVE_FORMAT_MVI_MVI2}
  WAVE_FORMAT_DF_G726               = $0085;  // DataFusion Systems (Pty) (Ltd) //
  {$EXTERNALSYM WAVE_FORMAT_DF_G726}
  WAVE_FORMAT_DF_GSM610             = $0086;  // DataFusion Systems (Pty) (Ltd) //
  {$EXTERNALSYM WAVE_FORMAT_DF_GSM610}
  WAVE_FORMAT_ISIAUDIO              = $0088;  // Iterated Systems, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_ISIAUDIO}
  WAVE_FORMAT_ONLIVE                = $0089;  // OnLive! Technologies, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_ONLIVE}
  WAVE_FORMAT_SBC24                 = $0091;  // Siemens Business Communications Sys //
  {$EXTERNALSYM WAVE_FORMAT_SBC24}
  WAVE_FORMAT_DOLBY_AC3_SPDIF       = $0092;  // Sonic Foundry //
  {$EXTERNALSYM WAVE_FORMAT_DOLBY_AC3_SPDIF}
  WAVE_FORMAT_MEDIASONIC_G723       = $0093;  // MediaSonic //
  {$EXTERNALSYM WAVE_FORMAT_MEDIASONIC_G723}
  WAVE_FORMAT_PROSODY_8KBPS         = $0094;  // Aculab plc //
  {$EXTERNALSYM WAVE_FORMAT_PROSODY_8KBPS}
  WAVE_FORMAT_ZYXEL_ADPCM           = $0097;  // ZyXEL Communications, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_ZYXEL_ADPCM}
  WAVE_FORMAT_PHILIPS_LPCBB         = $0098;  // Philips Speech Processing //
  {$EXTERNALSYM WAVE_FORMAT_PHILIPS_LPCBB}
  WAVE_FORMAT_PACKED                = $0099;  // Studer Professional Audio AG //
  {$EXTERNALSYM WAVE_FORMAT_PACKED}
  WAVE_FORMAT_MALDEN_PHONYTALK      = $00A0;  // Malden Electronics Ltd. //
  {$EXTERNALSYM WAVE_FORMAT_MALDEN_PHONYTALK}
  WAVE_FORMAT_RHETOREX_ADPCM        = $0100;  // Rhetorex Inc. //
  {$EXTERNALSYM WAVE_FORMAT_RHETOREX_ADPCM}
  WAVE_FORMAT_IRAT                  = $0101;  // BeCubed Software Inc. //
  {$EXTERNALSYM WAVE_FORMAT_IRAT}
  WAVE_FORMAT_VIVO_G723             = $0111;  // Vivo Software //
  {$EXTERNALSYM WAVE_FORMAT_VIVO_G723}
  WAVE_FORMAT_VIVO_SIREN            = $0112;  // Vivo Software //
  {$EXTERNALSYM WAVE_FORMAT_VIVO_SIREN}
  WAVE_FORMAT_DIGITAL_G723          = $0123;  // Digital Equipment Corporation //
  {$EXTERNALSYM WAVE_FORMAT_DIGITAL_G723}
  WAVE_FORMAT_SANYO_LD_ADPCM        = $0125;  // Sanyo Electric Co., Ltd. //
  {$EXTERNALSYM WAVE_FORMAT_SANYO_LD_ADPCM}
  WAVE_FORMAT_SIPROLAB_ACEPLNET     = $0130;  // Sipro Lab Telecom Inc. //
  {$EXTERNALSYM WAVE_FORMAT_SIPROLAB_ACEPLNET}
  WAVE_FORMAT_SIPROLAB_ACELP4800    = $0131;  // Sipro Lab Telecom Inc. //
  {$EXTERNALSYM WAVE_FORMAT_SIPROLAB_ACELP4800}
  WAVE_FORMAT_SIPROLAB_ACELP8V3     = $0132;  // Sipro Lab Telecom Inc. //
  {$EXTERNALSYM WAVE_FORMAT_SIPROLAB_ACELP8V3}
  WAVE_FORMAT_SIPROLAB_G729         = $0133;  // Sipro Lab Telecom Inc. //
  {$EXTERNALSYM WAVE_FORMAT_SIPROLAB_G729}
  WAVE_FORMAT_SIPROLAB_G729A        = $0134;  // Sipro Lab Telecom Inc. //
  {$EXTERNALSYM WAVE_FORMAT_SIPROLAB_G729A}
  WAVE_FORMAT_SIPROLAB_KELVIN       = $0135;  // Sipro Lab Telecom Inc. //
  {$EXTERNALSYM WAVE_FORMAT_SIPROLAB_KELVIN}
  WAVE_FORMAT_G726ADPCM             = $0140;  // Dictaphone Corporation //
  {$EXTERNALSYM WAVE_FORMAT_G726ADPCM}
  WAVE_FORMAT_QUALCOMM_PUREVOICE    = $0150;  // Qualcomm, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_QUALCOMM_PUREVOICE}
  WAVE_FORMAT_QUALCOMM_HALFRATE     = $0151;  // Qualcomm, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_QUALCOMM_HALFRATE}
  WAVE_FORMAT_TUBGSM                = $0155;  // Ring Zero Systems, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_TUBGSM}
  WAVE_FORMAT_MSAUDIO1              = $0160;  // Microsoft Corporation //
  {$EXTERNALSYM WAVE_FORMAT_MSAUDIO1}
  WAVE_FORMAT_WMAUDIO2              = $0161;  /// Microsoft Corporation /// updt 100812
  {$EXTERNALSYM WAVE_FORMAT_WMAUDIO2}
  WAVE_FORMAT_WMAUDIO3              = $0162;  /// Microsoft Corporation /// updt 100812
  {$EXTERNALSYM WAVE_FORMAT_WMAUDIO3}
  WAVE_FORMAT_WMAUDIO_LOSSLESS      = $0163;  /// Microsoft Corporation /// updt 100812
  {$EXTERNALSYM WAVE_FORMAT_WMAUDIO_LOSSLESS}
  WAVE_FORMAT_WMASPDIF              = $0164;  /// Microsoft Corporation /// updt 100812
  {$EXTERNALSYM WAVE_FORMAT_WMASPDIF}
  WAVE_FORMAT_UNISYS_NAP_ADPCM      = $0170;  // Unisys Corp. //
  {$EXTERNALSYM WAVE_FORMAT_UNISYS_NAP_ADPCM}
  WAVE_FORMAT_UNISYS_NAP_ULAW       = $0171;  // Unisys Corp. //
  {$EXTERNALSYM WAVE_FORMAT_UNISYS_NAP_ULAW}
  WAVE_FORMAT_UNISYS_NAP_ALAW       = $0172;  // Unisys Corp. //
  {$EXTERNALSYM WAVE_FORMAT_UNISYS_NAP_ALAW}
  WAVE_FORMAT_UNISYS_NAP_16K        = $0173;  // Unisys Corp. //
  {$EXTERNALSYM WAVE_FORMAT_UNISYS_NAP_16K}
  WAVE_FORMAT_CREATIVE_ADPCM        = $0200;  // Creative Labs, Inc
  {$EXTERNALSYM WAVE_FORMAT_CREATIVE_ADPCM}
  WAVE_FORMAT_CREATIVE_FASTSPEECH8  = $0202;  // Creative Labs, Inc
  {$EXTERNALSYM WAVE_FORMAT_CREATIVE_FASTSPEECH8}
  WAVE_FORMAT_CREATIVE_FASTSPEECH10 = $0203;  // Creative Labs, Inc
  {$EXTERNALSYM WAVE_FORMAT_CREATIVE_FASTSPEECH10}
  WAVE_FORMAT_UHER_ADPCM            = $0210;  // UHER informatic GmbH //
  {$EXTERNALSYM WAVE_FORMAT_UHER_ADPCM}
  WAVE_FORMAT_QUARTERDECK           = $0220;  // Quarterdeck Corporation //
  {$EXTERNALSYM WAVE_FORMAT_QUARTERDECK}
  WAVE_FORMAT_ILINK_VC              = $0230;  // I-link Worldwide //
  {$EXTERNALSYM WAVE_FORMAT_ILINK_VC}
  WAVE_FORMAT_RAW_SPORT             = $0240;  // Aureal Semiconductor //
  {$EXTERNALSYM WAVE_FORMAT_RAW_SPORT}
  WAVE_FORMAT_ESST_AC3              = $0241;  // ESS Technology, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_ESST_AC3}
  WAVE_FORMAT_IPI_HSX               = $0250;  // Interactive Products, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_IPI_HSX}
  WAVE_FORMAT_IPI_RPELP             = $0251;  // Interactive Products, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_IPI_RPELP}
  WAVE_FORMAT_CS2                   = $0260;  // Consistent Software //
  {$EXTERNALSYM WAVE_FORMAT_CS2}
  WAVE_FORMAT_SONY_SCX              = $0270;  // Sony Corp. //
  {$EXTERNALSYM WAVE_FORMAT_SONY_SCX}
  WAVE_FORMAT_FM_TOWNS_SND          = $0300;  // Fujitsu Corp.
  {$EXTERNALSYM WAVE_FORMAT_FM_TOWNS_SND}
  WAVE_FORMAT_BTV_DIGITAL           = $0400;  // Brooktree Corporation //
  {$EXTERNALSYM WAVE_FORMAT_BTV_DIGITAL}
  WAVE_FORMAT_QDESIGN_MUSIC         = $0450;  // QDesign Corporation //
  {$EXTERNALSYM WAVE_FORMAT_QDESIGN_MUSIC}
  WAVE_FORMAT_VME_VMPCM             = $0680;  // AT&T Labs, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_VME_VMPCM}
  WAVE_FORMAT_TPC                   = $0681;  // AT&T Labs, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_TPC}
  WAVE_FORMAT_OLIGSM                = $1000;  // Ing C. Olivetti & C., S.p.A.
  {$EXTERNALSYM WAVE_FORMAT_OLIGSM}
  WAVE_FORMAT_OLIADPCM              = $1001;  // Ing C. Olivetti & C., S.p.A.
  {$EXTERNALSYM WAVE_FORMAT_OLIADPCM}
  WAVE_FORMAT_OLICELP               = $1002;  // Ing C. Olivetti & C., S.p.A.
  {$EXTERNALSYM WAVE_FORMAT_OLICELP}
  WAVE_FORMAT_OLISBC                = $1003;  // Ing C. Olivetti & C., S.p.A.
  {$EXTERNALSYM WAVE_FORMAT_OLISBC}
  WAVE_FORMAT_OLIOPR                = $1004;  // Ing C. Olivetti & C., S.p.A.
  {$EXTERNALSYM WAVE_FORMAT_OLIOPR}
  WAVE_FORMAT_LH_CODEC              = $1100;  // Lernout & Hauspie //
  {$EXTERNALSYM WAVE_FORMAT_LH_CODEC}
  WAVE_FORMAT_NORRIS                = $1400;  // Norris Communications, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_NORRIS}
  WAVE_FORMAT_SOUNDSPACE_MUSICOMPRESS = $1500;  // AT&T Labs, Inc. //
  {$EXTERNALSYM WAVE_FORMAT_SOUNDSPACE_MUSICOMPRESS}
  WAVE_FORMAT_MPEG_ADTS_AAC           = $1600; // Microsoft Corporation ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_MPEG_ADTS_AAC}
  WAVE_FORMAT_MPEG_RAW_AAC            = $1601; // Microsoft Corporation ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_MPEG_RAW_AAC}
  WAVE_FORMAT_MPEG_LOAS               = $1602; // Microsoft Corporation (MPEG-4 Audio Transport Streams (LOAS/LATM) ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_MPEG_LOAS}
  WAVE_FORMAT_NOKIA_MPEG_ADTS_AAC     = $1608; // Microsoft Corporation ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_NOKIA_MPEG_ADTS_AAC}
  WAVE_FORMAT_NOKIA_MPEG_RAW_AAC      = $1609; // Microsoft Corporation ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_NOKIA_MPEG_RAW_AAC}
  WAVE_FORMAT_VODAFONE_MPEG_ADTS_AAC  = $160A; // Microsoft Corporation ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_VODAFONE_MPEG_ADTS_AAC}
  WAVE_FORMAT_VODAFONE_MPEG_RAW_AAC   = $160B; // Microsoft Corporation ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_VODAFONE_MPEG_RAW_AAC}
  WAVE_FORMAT_MPEG_HEAAC              = $1610; // Microsoft Corporation (MPEG-2 AAC or MPEG-4 HE-AAC v1/v2 streams with any payload (ADTS, ADIF, LOAS/LATM, RAW). Format block includes MP4 AudioSpecificConfig() -- see HEAACWAVEFORMAT below ///updt 100812 added
  {$EXTERNALSYM WAVE_FORMAT_MPEG_HEAAC}
  WAVE_FORMAT_DVM                     = $2000;  // FAST Multimedia AG //
  {$EXTERNALSYM WAVE_FORMAT_DVM}

  WAVE_FORMAT_DTS2                        = $2001;
  {$EXTERNALSYM WAVE_FORMAT_DTS2}
  WAVE_FORMAT_MAKEAVIS                    = $3313;
  {$EXTERNALSYM WAVE_FORMAT_MAKEAVIS}
  WAVE_FORMAT_DIVIO_MPEG4_AAC             = $4143;  { Divio, Inc. }
  {$EXTERNALSYM WAVE_FORMAT_DIVIO_MPEG4_AAC}
  WAVE_FORMAT_NOKIA_ADAPTIVE_MULTIRATE    = $4201;  { Nokia }
  {$EXTERNALSYM WAVE_FORMAT_NOKIA_ADAPTIVE_MULTIRATE}
  WAVE_FORMAT_DIVIO_G726                  = $4243;  { Divio, Inc. }
  {$EXTERNALSYM WAVE_FORMAT_DIVIO_G726}
  WAVE_FORMAT_LEAD_SPEECH                 = $434C;  { LEAD Technologies }
  {$EXTERNALSYM WAVE_FORMAT_LEAD_SPEECH}
  WAVE_FORMAT_LEAD_VORBIS                 = $564C;  { LEAD Technologies }
  {$EXTERNALSYM WAVE_FORMAT_LEAD_VORBIS}
  WAVE_FORMAT_WAVPACK_AUDIO               = $5756;  { xiph.org }
  {$EXTERNALSYM WAVE_FORMAT_WAVPACK_AUDIO}
  WAVE_FORMAT_ALAC                        = $6C61;  { Apple Lossless }
  {$EXTERNALSYM WAVE_FORMAT_ALAC}
  WAVE_FORMAT_OGG_VORBIS_MODE_1           = $674F;  { Ogg Vorbis }
  {$EXTERNALSYM WAVE_FORMAT_OGG_VORBIS_MODE_1}
  WAVE_FORMAT_OGG_VORBIS_MODE_2           = $6750;  { Ogg Vorbis }
  {$EXTERNALSYM WAVE_FORMAT_OGG_VORBIS_MODE_2}
  WAVE_FORMAT_OGG_VORBIS_MODE_3           = $6751;  { Ogg Vorbis }
  {$EXTERNALSYM WAVE_FORMAT_OGG_VORBIS_MODE_3}
  WAVE_FORMAT_OGG_VORBIS_MODE_1_PLUS      = $676F;  { Ogg Vorbis }
  {$EXTERNALSYM WAVE_FORMAT_OGG_VORBIS_MODE_1_PLUS}
  WAVE_FORMAT_OGG_VORBIS_MODE_2_PLUS      = $6770;  { Ogg Vorbis }
  {$EXTERNALSYM WAVE_FORMAT_OGG_VORBIS_MODE_2_PLUS}
  WAVE_FORMAT_OGG_VORBIS_MODE_3_PLUS      = $6771;  { Ogg Vorbis }
  {$EXTERNALSYM WAVE_FORMAT_OGG_VORBIS_MODE_3_PLUS}
  WAVE_FORMAT_3COM_NBX                    = $7000;  { 3COM Corp. }
  {$EXTERNALSYM WAVE_FORMAT_3COM_NBX}
  WAVE_FORMAT_OPUS                        = $704F;  { Opus }
  {$EXTERNALSYM WAVE_FORMAT_OPUS}
  WAVE_FORMAT_FAAD_AAC                    = $706D;
  {$EXTERNALSYM WAVE_FORMAT_FAAD_AAC}
  WAVE_FORMAT_AMR_NB                      = $7361;  { AMR Narrowband }
  {$EXTERNALSYM WAVE_FORMAT_AMR_NB}
  WAVE_FORMAT_AMR_WB                      = $7362;  { AMR Wideband }
  {$EXTERNALSYM WAVE_FORMAT_AMR_WB}
  WAVE_FORMAT_AMR_WP                      = $7363;  { AMR Wideband Plus }
  {$EXTERNALSYM WAVE_FORMAT_AMR_WP}
  WAVE_FORMAT_GSM_AMR_CBR                 = $7A21;  { GSMA/3GPP }
  {$EXTERNALSYM WAVE_FORMAT_GSM_AMR_CBR}
  WAVE_FORMAT_GSM_AMR_VBR_SID             = $7A22;  { GSMA/3GPP }
  {$EXTERNALSYM WAVE_FORMAT_GSM_AMR_VBR_SID}
  WAVE_FORMAT_COMVERSE_INFOSYS_G723_1     = $A100;  { Comverse Infosys }
  {$EXTERNALSYM WAVE_FORMAT_COMVERSE_INFOSYS_G723_1}
  WAVE_FORMAT_COMVERSE_INFOSYS_AVQSBC     = $A101;  { Comverse Infosys }
  {$EXTERNALSYM WAVE_FORMAT_COMVERSE_INFOSYS_AVQSBC}
  WAVE_FORMAT_COMVERSE_INFOSYS_SBC        = $A102;  { Comverse Infosys }
  {$EXTERNALSYM WAVE_FORMAT_COMVERSE_INFOSYS_SBC}
  WAVE_FORMAT_SYMBOL_G729_A               = $A103;  { Symbol Technologies }
  {$EXTERNALSYM WAVE_FORMAT_SYMBOL_G729_A}
  WAVE_FORMAT_VOICEAGE_AMR_WB             = $A104;  { VoiceAge Corp. }
  {$EXTERNALSYM WAVE_FORMAT_VOICEAGE_AMR_WB}
  WAVE_FORMAT_INGENIENT_G726              = $A105;  { Ingenient Technologies, Inc. }
  {$EXTERNALSYM WAVE_FORMAT_INGENIENT_G726}
  WAVE_FORMAT_MPEG4_AAC                   = $A106;  { ISO/MPEG-4 }
  {$EXTERNALSYM WAVE_FORMAT_MPEG4_AAC}
  WAVE_FORMAT_ENCORE_G726                 = $A107;  { Encore Software }
  {$EXTERNALSYM WAVE_FORMAT_ENCORE_G726}
  WAVE_FORMAT_ZOLL_ASAO                   = $A108;  { ZOLL Medical Corp. }
  {$EXTERNALSYM WAVE_FORMAT_ZOLL_ASAO}
  WAVE_FORMAT_SPEEX_VOICE                 = $A109;  { xiph.org }
  {$EXTERNALSYM WAVE_FORMAT_SPEEX_VOICE}
  WAVE_FORMAT_VIANIX_MASC                 = $A10A;  { Vianix LLC }
  {$EXTERNALSYM WAVE_FORMAT_VIANIX_MASC}
  WAVE_FORMAT_WM9_SPECTRUM_ANALYZER       = $A10B;  { Microsoft }
  {$EXTERNALSYM WAVE_FORMAT_WM9_SPECTRUM_ANALYZER}
  WAVE_FORMAT_WMF_SPECTRUM_ANAYZER        = $A10C;  { Microsoft }
  {$EXTERNALSYM WAVE_FORMAT_WMF_SPECTRUM_ANAYZER}
  WAVE_FORMAT_GSM_610                     = $A10D;
  {$EXTERNALSYM WAVE_FORMAT_GSM_610}
  WAVE_FORMAT_GSM_620                     = $A10E;
  {$EXTERNALSYM WAVE_FORMAT_GSM_620}
  WAVE_FORMAT_GSM_660                     = $A10F;
  {$EXTERNALSYM WAVE_FORMAT_GSM_660}
  WAVE_FORMAT_GSM_690                     = $A110;
  {$EXTERNALSYM WAVE_FORMAT_GSM_690}
  WAVE_FORMAT_GSM_ADAPTIVE_MULTIRATE_WB   = $A111;
  {$EXTERNALSYM WAVE_FORMAT_GSM_ADAPTIVE_MULTIRATE_WB}
  WAVE_FORMAT_POLYCOM_G722                = $A112;  { Polycom }
  {$EXTERNALSYM WAVE_FORMAT_POLYCOM_G722}
  WAVE_FORMAT_POLYCOM_G728                = $A113;  { Polycom }
  {$EXTERNALSYM WAVE_FORMAT_POLYCOM_G728}
  WAVE_FORMAT_POLYCOM_G729_A              = $A114;  { Polycom }
  {$EXTERNALSYM WAVE_FORMAT_POLYCOM_G729_A}
  WAVE_FORMAT_POLYCOM_SIREN               = $A115;  { Polycom }
  {$EXTERNALSYM WAVE_FORMAT_POLYCOM_SIREN}
  WAVE_FORMAT_GLOBAL_IP_ILBC              = $A116;  { Global IP }
  {$EXTERNALSYM WAVE_FORMAT_GLOBAL_IP_ILBC}
  WAVE_FORMAT_RADIOTIME_TIME_SHIFT_RADIO  = $A117;  { RadioTime }
  {$EXTERNALSYM WAVE_FORMAT_RADIOTIME_TIME_SHIFT_RADIO}
  WAVE_FORMAT_NICE_ACA                    = $A118;  { Nice Systems }
  {$EXTERNALSYM WAVE_FORMAT_NICE_ACA}
  WAVE_FORMAT_NICE_ADPCM                  = $A119;  { Nice Systems }
  {$EXTERNALSYM WAVE_FORMAT_NICE_ADPCM}
  WAVE_FORMAT_VOCORD_G721                 = $A11A;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_G721}
  WAVE_FORMAT_VOCORD_G726                 = $A11B;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_G726}
  WAVE_FORMAT_VOCORD_G722_1               = $A11C;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_G722_1}
  WAVE_FORMAT_VOCORD_G728                 = $A11D;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_G728}
  WAVE_FORMAT_VOCORD_G729                 = $A11E;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_G729}
  WAVE_FORMAT_VOCORD_G729_A               = $A11F;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_G729_A}
  WAVE_FORMAT_VOCORD_G723_1               = $A120;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_G723_1}
  WAVE_FORMAT_VOCORD_LBC                  = $A121;  { Vocord Telecom }
  {$EXTERNALSYM WAVE_FORMAT_VOCORD_LBC}
  WAVE_FORMAT_NICE_G728                   = $A122;  { Nice Systems }
  {$EXTERNALSYM WAVE_FORMAT_NICE_G728}
  WAVE_FORMAT_FRACE_TELECOM_G729          = $A123;  { France Telecom }
  {$EXTERNALSYM WAVE_FORMAT_FRACE_TELECOM_G729}
  WAVE_FORMAT_CODIAN                      = $A124;  { CODIAN }
  {$EXTERNALSYM WAVE_FORMAT_CODIAN}
  WAVE_FORMAT_DOLBY_AC4                   = $AC40;  { Dolby AC-4 }
  {$EXTERNALSYM WAVE_FORMAT_DOLBY_AC4}
  WAVE_FORMAT_DOLBY_AC4_IMS               = $AC41;  { Dolby AC-4 Immersive Stereo }
  {$EXTERNALSYM WAVE_FORMAT_DOLBY_AC4_IMS}
  WAVE_FORMAT_FLAC                        = $F1AC;  { flac.sourceforge.net }
  {$EXTERNALSYM WAVE_FORMAT_FLAC}

  WAVE_FORMAT_EXTENSIBLE                  = $FFFE;  // Microsoft
  {$EXTERNALSYM WAVE_FORMAT_EXTENSIBLE}

  //
  //  New wave format development should be based on the
  //  WAVEFORMATEXTENSIBLE structure. WAVEFORMATEXTENSIBLE allows you to
  //  avoid having to register a new format tag with Microsoft. However, if
  //  you must still define a new format tag, the WAVE_FORMAT_DEVELOPMENT
  //  format tag can be used during the development phase of a new wave
  //  format. Before shipping, you MUST acquire an official format tag from
  //  Microsoft.
  //
  WAVE_FORMAT_DEVELOPMENT                 = $FFFF;
  {$EXTERNALSYM WAVE_FORMAT_DEVELOPMENT}

type

{$IFNDEF _WAVE_FORMAT_PCM_DEFINED}
  // general waveform format structure (information common to all formats)
  PWAVEFORMAT   = ^WAVEFORMAT;
  waveformat_tag = record
    wFormatTag: WORD;                 // format type
    nChannels: WORD;                  // number of channels (i.e. mono, stereo...)
    nSamplesPerSec: DWORD;            // sample rate
    nAvgBytesPerSec: DWORD;           // for buffer estimation
    nBlockAlign: WORD;                // block size of data
  end;
  {$EXTERNALSYM waveformat_tag}
  WAVEFORMAT    = waveformat_tag;
  {$EXTERNALSYM WAVEFORMAT}
  NPWAVEFORMAT  = PWAVEFORMAT;
  {$EXTERNALSYM NPWAVEFORMAT}
  LPWAVEFORMAT  = PWAVEFORMAT;
  {$EXTERNALSYM LPWAVEFORMAT}

  // specific waveform format structure for PCM data
  PPCMWAVEFORMAT = ^PCMWAVEFORMAT;
  NPPCMWAVEFORMAT = ^PCMWAVEFORMAT;
  LPPCMWAVEFORMAT = ^PCMWAVEFORMAT;
  pcmwaveformat_tag = record
    wf: WAVEFORMAT;
    wBitsPerSample: WORD;
  end;
  {$EXTERNALSYM pcmwaveformat_tag}
  PCMWAVEFORMAT = pcmwaveformat_tag;
  {$EXTERNALSYM PCMWAVEFORMAT}
{$DEFINE _WAVE_FORMAT_PCM_DEFINED}
{$DEFINE WAVE_FORMAT_PCM = 1}
{$ENDIF} // WAVE_FORMAT_PCM



  // General extended waveform format structure
  // Use this for all NON PCM formats
  // (information common to all formats)

{$IFNDEF _WAVEFORMATEX_DEFINED}
  PWAVEFORMATEX = ^WAVEFORMATEX;
  tWAVEFORMATEX = record
    wFormatTag: WORD;                { format type }
    nChannels: WORD;                 { number of channels (i.e. mono, stereo...) }
    nSamplesPerSec: DWORD;           { sample rate }
    nAvgBytesPerSec: DWORD;          { for buffer estimation }
    nBlockAlign: WORD;               { block size of data }
    wBitsPerSample: WORD;            { Number of bits per sample of mono data }
    cbSize: WORD;                    { The count in bytes of the size of
                                       extra information (after cbSize) }
  end;
  {$EXTERNALSYM tWAVEFORMATEX}
  WAVEFORMATEX = tWAVEFORMATEX;
  {$EXTERNALSYM WAVEFORMATEX}
  NPWAVEFORMATEX = ^tWAVEFORMATEX;
  {$EXTERNALSYM NPWAVEFORMATEX}
  LPWAVEFORMATEX = ^tWAVEFORMATEX;
  {$EXTERNALSYM LPWAVEFORMATEX}
  LPCWAVEFORMATEX = ^WAVEFORMATEX;
  {$EXTERNALSYM LPCWAVEFORMATEX}{$DEFINE _WAVEFORMATEX_DEFINED}
{$ENDIF} // _WAVEFORMATEX_

const

  KSDATAFORMAT_SUBTYPE_PCM          : TGuid = '{00000001-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM KSDATAFORMAT_SUBTYPE_PCM}
  KSDATAFORMAT_SUBTYPE_IEEE_FLOAT   : TGuid = '{00000003-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM KSDATAFORMAT_SUBTYPE_IEEE_FLOAT}
  KSDATAFORMAT_SUBTYPE_WAVEFORMATEX : TGuid = '{00000000-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM KSDATAFORMAT_SUBTYPE_WAVEFORMATEX}

  //  New wave format development should be based on the
  //  WAVEFORMATEXTENSIBLE structure. WAVEFORMATEXTENSIBLE allows you to
  //  avoid having to register a new format tag with Microsoft. Simply
  //  define a new GUID value for the WAVEFORMATEXTENSIBLE.SubFormat field
  //  and use WAVE_FORMAT_EXTENSIBLE in the
  //  WAVEFORMATEXTENSIBLE.Format.wFormatTag field.

type

{$IFNDEF __WAVEFORMATEXTENSIBLE__DEFINED}

 // WAVEFORMATEXTENSIBLE
 // ====================

{$ALIGN ON}

  PWAVEFORMATEXTENSIBLE = ^WAVEFORMATEXTENSIBLE;
  WAVEFORMATEXTENSIBLE = record
    Format : WAVEFORMATEX;
    Samples : record                      // union part
      case integer of
        0: (wValidBitsPerSample : WORD);  // bits of precision
        1: (wSamplesPerBlock    : WORD);  // valid if wBitsPerSample == 0
        2: (wReserved           : WORD);  // If neither applies, set to zero.
      end;
    dwChannelMask : DWORD;                // which channels are present in stream
    SubFormat :     TGUID;
  end;
  {$EXTERNALSYM WAVEFORMATEXTENSIBLE}

{$DEFINE __WAVEFORMATEXTENSIBLE__DEFINED}
{$ENDIF} // !_WAVEFORMATEXTENSIBLE_


  //  Extended PCM waveform format structure based on WAVEFORMATEXTENSIBLE.
  //  Use this for multiple channel and hi-resolution PCM data
  // ======================================================================

  PWAVEFORMATPCMEX = ^WAVEFORMATPCMEX;
  NPWAVEFORMATPCMEX = ^WAVEFORMATPCMEX;
  {$EXTERNALSYM NPWAVEFORMATPCMEX}
  LPWAVEFORMATPCMEX = ^WAVEFORMATPCMEX;
  {$EXTERNALSYM LPWAVEFORMATPCMEX}
  WAVEFORMATPCMEX = WAVEFORMATEXTENSIBLE;      { Format.cbSize = 22 }
  {$EXTERNALSYM WAVEFORMATPCMEX}


  //  Extended format structure using IEEE Float data and based
  //  on WAVEFORMATEXTENSIBLE.  Use this for multiple channel
  //  and hi-resolution PCM data in IEEE floating point format.
  // ==========================================================


  PWAVEFORMATIEEEFLOATEX = ^WAVEFORMATIEEEFLOATEX;
  NPWAVEFORMATIEEEFLOATEX = ^WAVEFORMATIEEEFLOATEX;
  {$EXTERNALSYM NPWAVEFORMATIEEEFLOATEX}
  LPWAVEFORMATIEEEFLOATEX = ^WAVEFORMATIEEEFLOATEX;
  {$EXTERNALSYM LPWAVEFORMATIEEEFLOATEX}
  WAVEFORMATIEEEFLOATEX = WAVEFORMATEXTENSIBLE;  { Format.cbSize = 22 }
  {$EXTERNALSYM WAVEFORMATIEEEFLOATEX}



{$IFNDEF __SPEAKER_POSITIONS__DEFINED}
const

  // Speaker Positions for dwChannelMask in WAVEFORMATEXTENSIBLE:
  // ============================================================

  SPEAKER_FRONT_LEFT                  = $1;
  {$EXTERNALSYM SPEAKER_FRONT_LEFT}
  SPEAKER_FRONT_RIGHT                 = $2;
  {$EXTERNALSYM SPEAKER_FRONT_RIGHT}
  SPEAKER_FRONT_CENTER                = $4;
  {$EXTERNALSYM SPEAKER_FRONT_CENTER}
  SPEAKER_LOW_FREQUENCY               = $8;
  {$EXTERNALSYM SPEAKER_LOW_FREQUENCY}
  SPEAKER_BACK_LEFT                   = $10;
  {$EXTERNALSYM SPEAKER_BACK_LEFT}
  SPEAKER_BACK_RIGHT                  = $20;
  {$EXTERNALSYM SPEAKER_BACK_RIGHT}
  SPEAKER_FRONT_LEFT_OF_CENTER        = $40;
  {$EXTERNALSYM SPEAKER_FRONT_LEFT_OF_CENTER}
  SPEAKER_FRONT_RIGHT_OF_CENTER       = $80;
  {$EXTERNALSYM SPEAKER_FRONT_RIGHT_OF_CENTER}
  SPEAKER_BACK_CENTER                 = $100;
  {$EXTERNALSYM SPEAKER_BACK_CENTER}
  SPEAKER_SIDE_LEFT                   = $200;
  {$EXTERNALSYM SPEAKER_SIDE_LEFT}
  SPEAKER_SIDE_RIGHT                  = $400;
  {$EXTERNALSYM SPEAKER_SIDE_RIGHT}
  SPEAKER_TOP_CENTER                  = $800;
  {$EXTERNALSYM SPEAKER_TOP_CENTER}
  SPEAKER_TOP_FRONT_LEFT              = $1000;
  {$EXTERNALSYM SPEAKER_TOP_FRONT_LEFT}
  SPEAKER_TOP_FRONT_CENTER            = $2000;
  {$EXTERNALSYM SPEAKER_TOP_FRONT_CENTER}
  SPEAKER_TOP_FRONT_RIGHT             = $4000;
  {$EXTERNALSYM SPEAKER_TOP_FRONT_RIGHT}
  SPEAKER_TOP_BACK_LEFT               = $8000;
  {$EXTERNALSYM SPEAKER_TOP_BACK_LEFT}
  SPEAKER_TOP_BACK_CENTER             = $10000;
  {$EXTERNALSYM SPEAKER_TOP_BACK_CENTER}
  SPEAKER_TOP_BACK_RIGHT              = $20000;
  {$EXTERNALSYM SPEAKER_TOP_BACK_RIGHT}
  // Bit mask locations reserved for future use
  SPEAKER_RESERVED                    = $7FFC0000;
  {$EXTERNALSYM SPEAKER_RESERVED}
  // Used to specify that any possible permutation of speaker configurations
  SPEAKER_ALL                         = $80000000;
  {$EXTERNALSYM SPEAKER_ALL}
{$DEFINE __SPEAKER_POSITIONS__DEFINED}
{$ENDIF} // _SPEAKER_POSITIONS_


type

  // Define data for MS ADPCM

  PADPCMCoefSet = ^ADPCMCoefSet;
  NPADPCMCOEFSET = ^ADPCMCOEFSET;
  LPADPCMCOEFSET = ^ADPCMCOEFSET;
  ADPCMCoefSet = record
    iCoef1: Word;
    iCoef2: Word;
  end;
  {$EXTERNALSYM ADPCMCoefSet}


  PADPCMWaveFormat = ^ADPCMWaveFormat;
  ADPCMWaveFormat = record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
    wNumCoef: Word;
    aCoef: array [0..1] of ADPCMCoefSet;
  end;
  {$EXTERNALSYM ADPCMWaveFormat}


  //  Microsoft's DRM structure definitions

  PDRMWAVEFORMAT = ^DRMWAVEFORMAT;
  NPDRMWAVEFORMAT = ^DRMWAVEFORMAT;
  LPDRMWAVEFORMAT = ^DRMWAVEFORMAT;
  drmwaveformat_tag = record
    wfx: WAVEFORMATEX;
    wReserved: WORD;
    ulContentId: ULONG;
    wfxSecure: WAVEFORMATEX;
  end;
  {$EXTERNALSYM drmwaveformat_tag}
  DRMWAVEFORMAT = drmwaveformat_tag;
  {$EXTERNALSYM DRMWAVEFORMAT}


  //  Intel's DVI ADPCM structure definitions
  //
  //      for WAVE_FORMAT_DVI_ADPCM   ($0011)

  PDVIADPCMWaveFormat = ^DVIADPCMWaveFormat;
  DVIADPCMWaveFormat = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM DVIADPCMWaveFormat}


  //  IMA endorsed ADPCM structure definitions--note that this is exactly
  //  the same format as Intel's DVI ADPCM.
  //
  //      for WAVE_FORMAT_IMA_ADPCM   ($0011)

  PIMAADPCMWaveFormat = ^IMAADPCMWaveFormat;
  IMAADPCMWaveFormat = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM IMAADPCMWaveFormat}


  // VideoLogic's Media Space ADPCM Structure definitions
  // for  WAVE_FORMAT_MEDIASPACE_ADPCM    ($0012)

  PMediaSpaceADPCMWaveFormat = ^MediaSpaceADPCMWaveFormat;
  MediaSpaceADPCMWaveFormat = record
    wfx: WaveFormatEx;
    wRevision: Word;
  end;
  {$EXTERNALSYM MediaSpaceADPCMWaveFormat}


  //  Sierra Semiconductor
  //  for WAVE_FORMAT_SIERRA_ADPCM   ($0013)

  PSierraADPCMWaveFormat = ^SierraADPCMWaveFormat;
  SierraADPCMWaveFormat = record
    wfx: TWaveFormatEx;
    wRevision: Word;
  end;
  {$EXTERNALSYM SierraADPCMWaveFormat}


  //  Antex Electronics  structure definitions
  //      for WAVE_FORMAT_G723_ADPCM   ($0014)

  PG723_ADPCMWaveFormat = ^G723_ADPCMWaveFormat;
  G723_ADPCMWaveFormat = record
    wfx: TWaveFormatEx;
    cbExtraSize: Word;
    nAuxBlockSize: Word;
  end;
  {$EXTERNALSYM G723_ADPCMWaveFormat}


  //  DSP Solutions (formerly DIGISPEECH) structure definitions
  //
  //      for WAVE_FORMAT_DIGISTD   ($0015)

  PDigiStdWaveFormat = ^DigiStdWaveFormat;
  DigiStdWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM DigiStdWaveFormat}


  //  DSP Solutions (formerly DIGISPEECH) structure definitions
  //
  //      for WAVE_FORMAT_DIGIFIX   ($0016)

  PDigiFixWaveFormat = ^DigiFixWaveFormat;
  DigiFixWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM DigiFixWaveFormat}


  //   Dialogic Corporation
  // WAVEFORMAT_DIALOGIC_OKI_ADPCM   ($0017)

  PDialogIkokiADPCMWaveFormat = ^DialogIkokiADPCMWaveFormat;
  DialogIkokiADPCMWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM DialogIkokiADPCMWaveFormat}


  //  Yamaha Compression's ADPCM structure definitions
  //
  //      for WAVE_FORMAT_YAMAHA_ADPCM   ($0020)

  PYamaha_ADPCMWaveFormat = ^Yamaha_ADPCMWaveFormat;
  Yamaha_ADPCMWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM Yamaha_ADPCMWaveFormat}


  //  Speech Compression's Sonarc structure definitions
  //
  //      for WAVE_FORMAT_SONARC   ($0021)

  PSonarcWaveFormat = ^SonarcWaveFormat;
  SonarcWaveFormat = record
    wfx: WaveFormatEx;
    wCompType: Word;
  end;
  {$EXTERNALSYM SonarcWaveFormat}


  //  DSP Groups's TRUESPEECH structure definitions
  //
  //      for WAVE_FORMAT_DSPGROUP_TRUESPEECH   ($0022)

  PTrueSpeechWaveFormat = ^TrueSpeechWaveFormat;
  TrueSpeechWaveFormat = record
    wfx: WaveFormatEx;
    wRevision: Word;
    wSamplesPerBlock: Word;
    abReserved: array [0 .. 27] of Byte;
  end;
  {$EXTERNALSYM TrueSpeechWaveFormat}


  //  Echo Speech Corp structure definitions
  //
  //      for WAVE_FORMAT_ECHOSC1   ($0023)
  PEchoSC1WaveFormat = ^EchoSC1WaveFormat;
  EchoSC1WaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM EchoSC1WaveFormat}


  //  Audiofile Inc.structure definitions
  //
  //      for WAVE_FORMAT_AUDIOFILE_AF36   ($0024)
  PAudioFile_AF36WaveFormat = ^AudioFile_AF36WaveFormat;
  AudioFile_AF36WaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM AudioFile_AF36WaveFormat}


  //  Audio Processing Technology structure definitions
  //
  //      for WAVE_FORMAT_APTX   ($0025)
  PAPTXWaveFormat = ^APTXWaveFormat;
  APTXWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM APTXWaveFormat}


  //  Audiofile Inc.structure definitions
  //
  //      for WAVE_FORMAT_AUDIOFILE_AF10   ($0026)

  PAudioFile_AF10WaveFormat = ^AudioFile_AF10WaveFormat;
  AudioFile_AF10WaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM AudioFile_AF10WaveFormat}


  // Dolby's AC-2 wave format structure definition
  //
  //           WAVE_FORMAT_DOLBY_AC2    ($0030)

  PDolbyAC2WaveFormat = ^DolbyAC2WaveFormat;
  DolbyAC2WaveFormat = record
    wfx: WaveFormatEx;
    nAuxBitsCode: Word;
  end;
  {$EXTERNALSYM DolbyAC2WaveFormat}


  // Microsoft's
  // WAVE_FORMAT_GSM 610           $0031

  PGSM610WaveFormat = ^GSM610WaveFormat;
  GSM610WaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM GSM610WaveFormat}


  //      Antex Electronics Corp
  //
  //      for WAVE_FORMAT_ADPCME                  ($0033)

  PADPCMEWaveFormat = ^ADPCMEWaveFormat;
  ADPCMEWaveFormat = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM ADPCMEWaveFormat}


  // Control Resources Limited
  // WAVE_FORMAT_CONTROL_RES_VQLPC              $0034

  PContresVQLPCWaveFormat = ^ContresVQLPCWaveFormat;
  {$EXTERNALSYM PContresVQLPCWaveFormat}
  ContresVQLPCWaveFormat = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;


  // for WAVE_FORMAT_DIGIREAL                   ($0035)

  PDIGIREALWAVEFORMAT = ^DIGIREALWAVEFORMAT;
  DIGIREALWAVEFORMAT = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM DIGIREALWAVEFORMAT}


  //  DSP Solutions
  //
  //  for WAVE_FORMAT_DIGIADPCM   ($0036)

  PDIGIADPCMWAVEFORMAT = ^DIGIADPCMWAVEFORMAT;
  NPDIGIADPCMWAVEFORMAT = ^DIGIADPCMWAVEFORMAT;
  {$EXTERNALSYM NPDIGIADPCMWAVEFORMAT}
  LPDIGIADPCMWAVEFORMAT = ^DIGIADPCMWAVEFORMAT;
  {$EXTERNALSYM LPDIGIADPCMWAVEFORMAT}
  DIGIADPCMWAVEFORMAT = record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM DIGIADPCMWAVEFORMAT}


  // Control Resources Limited
  // WAVE_FORMAT_CONTROL_RES_CR10          $0037

  PContResCR10WaveFormat = ^ContResCR10WaveFormat;
  ContResCR10WaveFormat = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM ContResCR10WaveFormat}


  //  Natural Microsystems
  //
  //      for WAVE_FORMAT_NMS_VBXADPCM   ($0038)

  PVBXADPCMWaveFormat = ^VBXADPCMWaveFormat;
  VBXADPCMWaveFormat = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM VBXADPCMWaveFormat}


  //  Antex Electronics  structure definitions
  //
  //      for WAVE_FORMAT_G721_ADPCM   ($0040)
  PG721_ADPCMWaveFormat = ^G721_ADPCMWaveFormat;
  G721_ADPCMWaveFormat = record
    wfx: WaveFormatEx;
    wSamplesPerBlock: Word;
  end;
  {$EXTERNALSYM G721_ADPCMWaveFormat}


  // Microsoft MPEG audio WAV definition
  //
  //  MPEG-1 audio wave format (audio layer only).   ($0050)

  PMPEG1WaveFormat = ^MPEG1WaveFormat;
  MPEG1WaveFormat = record
    wfx: WaveFormatEx;
    fwHeadLayer: Word;
    dwHeadBitrate: DWORD;
    fwHeadMode: Word;
    fwHeadModeExt: Word;
    wHeadEmphasis: Word;
    fwHeadFlags: Word;
    dwPTSlow: DWORD;
    dwPTShigh: DWORD;
  end;
  {$EXTERNALSYM MPEG1WaveFormat}
  NPMPEG1WAVEFORMAT = ^MPEG1WAVEFORMAT;
  {$EXTERNALSYM NPMPEG1WAVEFORMAT}
  LPMPEG1WAVEFORMAT = ^MPEG1WAVEFORMAT;
  {$EXTERNALSYM LPMPEG1WAVEFORMAT}


const

  ACM_MPEG_LAYER1             = $0001;
  {$EXTERNALSYM ACM_MPEG_LAYER1}
  ACM_MPEG_LAYER2             = $0002;
  {$EXTERNALSYM ACM_MPEG_LAYER2}
  ACM_MPEG_LAYER3             = $0004;
  {$EXTERNALSYM ACM_MPEG_LAYER3}
  ACM_MPEG_STEREO             = $0001;
  {$EXTERNALSYM ACM_MPEG_STEREO}
  ACM_MPEG_JOINTSTEREO        = $0002;
  {$EXTERNALSYM ACM_MPEG_JOINTSTEREO}
  ACM_MPEG_DUALCHANNEL        = $0004;
  {$EXTERNALSYM ACM_MPEG_DUALCHANNEL}
  ACM_MPEG_SINGLECHANNEL      = $0008;
  {$EXTERNALSYM ACM_MPEG_SINGLECHANNEL}
  ACM_MPEG_PRIVATEBIT         = $0001;
  {$EXTERNALSYM ACM_MPEG_PRIVATEBIT}
  ACM_MPEG_COPYRIGHT          = $0002;
  {$EXTERNALSYM ACM_MPEG_COPYRIGHT}
  ACM_MPEG_ORIGINALHOME       = $0004;
  {$EXTERNALSYM ACM_MPEG_ORIGINALHOME}
  ACM_MPEG_PROTECTIONBIT      = $0008;
  {$EXTERNALSYM ACM_MPEG_PROTECTIONBIT}
  ACM_MPEG_ID_MPEG1           = $0010;
  {$EXTERNALSYM ACM_MPEG_ID_MPEG1}

  // MPEG Layer3 WAVEFORMATEX structure
  // for WAVE_FORMAT_MPEGLAYER3 ($0055)
  // ===================================

  MPEGLAYER3_WFX_EXTRA_BYTES          = 12;
  {$EXTERNALSYM MPEGLAYER3_WFX_EXTRA_BYTES}

type

  // WAVE_FORMAT_MPEGLAYER3 format sructure
  // ======================================

  PMPEGLAYER3WAVEFORMAT = ^MPEGLAYER3WAVEFORMAT;
  NPMPEGLAYER3WAVEFORMAT = ^MPEGLAYER3WAVEFORMAT;
  {$EXTERNALSYM LPMPEGLAYER3WAVEFORMAT}
  LPMPEGLAYER3WAVEFORMAT = ^MPEGLAYER3WAVEFORMAT;
  {$EXTERNALSYM NPMPEGLAYER3WAVEFORMAT}
  mpeglayer3waveformat_tag = record
    wfx: WAVEFORMATEX;
    wID: WORD;
    fdwFlags: DWORD;
    nBlockSize: WORD;
    nFramesPerBlock: WORD;
    nCodecDelay: WORD;
  end;
  {$EXTERNALSYM mpeglayer3waveformat_tag}
  MPEGLAYER3WAVEFORMAT = mpeglayer3waveformat_tag;
  {$EXTERNALSYM MPEGLAYER3WAVEFORMAT}

  // =========================================================================;

const

  MPEGLAYER3_ID_UNKNOWN               = 0;
  {$EXTERNALSYM MPEGLAYER3_ID_UNKNOWN}
  MPEGLAYER3_ID_MPEG                  = 1;
  {$EXTERNALSYM MPEGLAYER3_ID_MPEG}
  MPEGLAYER3_ID_CONSTANTFRAMESIZE     = 2;
  {$EXTERNALSYM MPEGLAYER3_ID_CONSTANTFRAMESIZE}

  MPEGLAYER3_FLAG_PADDING_ISO         = $00000000;
  {$EXTERNALSYM MPEGLAYER3_FLAG_PADDING_ISO}
  MPEGLAYER3_FLAG_PADDING_ON          = $00000001;
  {$EXTERNALSYM MPEGLAYER3_FLAG_PADDING_ON}
  MPEGLAYER3_FLAG_PADDING_OFF         = $00000002;
  {$EXTERNALSYM MPEGLAYER3_FLAG_PADDING_OFF}

  // =========================================================================;

  //      For WAVE_FORMAT_MPEG_HEAAC ($1610)
  //
  //
  // "MPEG-2" in the comments below refers to ISO/IEC 13818-7 (MPEG-2 AAC spec).
  // "MPEG-4" in the comments below refers to ISO/IEC 14496-3 (MPEG-4 Audio spec).
  //
  // The following defines the format block to be used for MPEG-2 AAC or MPEG-4 HE-AAC v1/v2 streams.
  // When setting media type attributes in Media Foundation (MF) objects, this will appear in conjunction with
  // major type MFMediaType_Audio and sub type MFAudioFormat_AAC (=MEDIASUBTYPE_MPEG_HEAAC).
  // The format block structure HEAACWAVEFORMAT is defined below.  It starts with the structure
  // HEAACWAVEINFO (which is an extension of WAVEFORMATEX), followed by AudioSpecificConfig() as
  // defined by ISO/IEC 14496-3 (MPEG-4 audio). Since the length of AudioSpecificConfig() may vary
  // from one stream to another, this is a variable size format block.
  //
  // The WAVEFORMATEX fields describe the properties of the core AAC stream,
  // without SBR/PS extensions (if exists). This is the description of the WAVEFORMATEX fields:
  //
  // wfx.wFormatTag - Set this to WAVE_FORMAT_MPEG_HEAAC ($1610).
  //
  // wfx.nChannels - Total number of channels in core AAC stream (including LFE if exists).
  // This might be different than the decoded number of channels if the MPEG-4 Parametric Stereo (PS)
  // tool exists. If unknown, set to zero.
  //
  // wfx.nSamplesPerSec - Sampling rate of core AAC stream. This will be one of the 12 supported
  // sampling rate between 8000 and 96000 Hz, as defined in MPEG-2.  This might be different than
  // the decoded sampling rate if the MPEG-4 Spectral Band Replication (SBR) tool exists.
  // If not know in advance, the sampling rate can be extracted from:
  // - the 4-bit sampling_frequency_index field in adts_fixed_header(), or
  // - program_config_element() in adif_header for MPEG-2 streams, or
  // - the 4-bit samplingFrequencyIndex field in AudioSpecificConfig() for MPEG-4 streams.
  //
  // wfx.nAvgBytesPerSec - The average bytes per second calculated based on the average bit rate of
  // the compressed stream. This value may be used by parsers to seek into a particular time offset
  // in the stream. It may also be used by applications to determine roughly how much buffer length to allocate.
  // If this is not known in advance, this value can be estimated by parsing some (or all) of the
  // compressed HE-AAC frames and calculating bit rate based on average compressed frame size.
  // If unknown, set to zero.
  //
  // wfx.nBlockAlign - Set this to 1.
  //
  // wfx.wBitsPerSample - Desired bit depth of the decoded PCM. If unknown, set to zero.
  //
  // wfx.cbSize - Set this to 12 (=sizeof(HEAACWAVEINFO)-sizeof(WAVEFORMATEX)) plus the
  // size of AudioSpecificConfig() in bytes.
  //
  // ===========================================================================
  //
  // How do we parse this format block? assume pbBuff is the address of the first
  // byte in the format block. We do the following:
  //
  // HEAACWAVEINFO* pwfInfo = (HEAACWAVEINFO *)pbBuff;
  //
  // if ( 0 == pwfInfo->wStructType)
  // {
  //    HEAACWAVEFORMAT* pwf = (HEAACWAVEFORMAT*)pbBuff;
  //
    // All HEAACWAVEFORMAT fields can now be accessed through pwf
    // ...

    //
    // To parse AudioSpecifiConfig(), write a function such as
    // ParseAudioSpecificConfig(BYTE *pbASC, DWORD dwASCLen),
    // and call:
    //
  //    DWORD dwASCLen = pwf->wfInfo.wfx.cbSize - sizeof(HEAACWAVEINFO) + sizeof(WAVEFORMATEX);
  //
  //    ParseAudioSpecificConfig(pwf->pbAudioSpecificConfig, dwASCLen);
  // }
  // else
  // {
    // Reserved
  // }
  //

type

  PHEAACWAVEINFO = ^HEAACWAVEINFO;
  NPHEAACWAVEINFO = ^HEAACWAVEINFO;
  {$EXTERNALSYM NPHEAACWAVEINFO}
  LPHEAACWAVEINFO = ^HEAACWAVEINFO;
  {$EXTERNALSYM LPHEAACWAVEINFO}
  heaacwaveinfo_tag = record
    // Defines core AAC properties. See description above. WAVEFORMATEX is of size 18 bytes.
    wfx: WAVEFORMATEX;
    // Defines the payload type
    // 0-RAW.  The stream contains raw_data_block() elements only.
    // 1-ADTS. The stream contains an adts_sequence(), as defined by MPEG-2.
    // 2-ADIF. The stream contains an adif_sequence(), as defined by MPEG-2.
    // 3-LOAS. The stream contains an MPEG-4 audio transport stream with a
    //         synchronization layer LOAS and a multiplex layer LATM.
    // All other codes are reserved.
    wPayloadType: WORD;
    // This is the 8-bit field audioProfileLevelIndication available in the
    // MPEG-4 object descriptor.  It is an indication (as defined in MPEG-4 audio)
    // of the audio profile and level required to process the content associated
    // with this stream. For example values $28-$2B correspond to AAC Profile,
    // values $2C-$2F correspond to HE-AAC profile and $30-$33 for HE-AAC v2 profile.
    // If unknown, set to zero or $FE ("no audio profile specified").
    wAudioProfileLevelIndication: WORD;
    // Defines the data that follows this structure. Currently only one data type is supported:
    // 0- AudioSpecificConfig() (as defined by MPEG-4 Audio, ISO/IEC 14496-3) will follow this structure.
    //    wfx.cbSize will indicate the total length including AudioSpecificConfig().
    //    Use HEAACWAVEFORMAT to gain easy access to the address of the first byte of
    //    AudioSpecificConfig() for parsing.
    //    Typical values for the size of AudioSpecificConfig (ASC) are:
    //    - 2 bytes for AAC or HE-AAC v1/v2 with implicit signaling of SBR,
    //    - 5 bytes for HE-AAC v1 with explicit signaling of SBR,
    //    - 7 bytes for HE-AAC v2 with explicit signaling of SBR and PS.
    //    The size may be longer than 7 bytes if the 4-bit channelConfiguration field in ASC is zero,
    //    which means program_config_element() is present in ASC.
    //
    // All other codes are reserved.
    wStructType: WORD;
    // Set these to zero
    wReserved1: WORD;
    dwReserved2: DWORD;
  end;
  {$EXTERNALSYM heaacwaveinfo_tag}
  HEAACWAVEINFO = heaacwaveinfo_tag; // this structure has a size of 30 bytes
  {$EXTERNALSYM HEAACWAVEINFO}


  // This structure describes the format block for wStructType=0
  PHEAACWAVEFORMAT = ^HEAACWAVEFORMAT;
  NPHEAACWAVEFORMAT = ^HEAACWAVEFORMAT;
  {$EXTERNALSYM NPHEAACWAVEFORMAT}
  LPHEAACWAVEFORMAT = ^HEAACWAVEFORMAT;
  {$EXTERNALSYM LPHEAACWAVEFORMAT}
  heaacwaveformat_tag = record
    wfInfo: HEAACWAVEINFO;                        // This structure has a size of 30 bytes
    pbAudioSpecificConfig: array [0..0] of Byte;  // First byte of AudioSpecificConfig()
  end;
  {$EXTERNALSYM heaacwaveformat_tag}
  HEAACWAVEFORMAT = heaacwaveformat_tag;  // This structure has a size of 31 bytes
  {$EXTERNALSYM HEAACWAVEFORMAT}



  // =========================================================================



  //  Windows Media Audio (common)
  // =============================

const

  MM_MSFT_ACM_WMAUDIO                 = 39;
  {$EXTERNALSYM MM_MSFT_ACM_WMAUDIO}

  WMAUDIO_BITS_PER_SAMPLE             = 16;  // just an uncompressed size...
  {$EXTERNALSYM WMAUDIO_BITS_PER_SAMPLE}
  WMAUDIO_MAX_CHANNELS                = 2;
  {$EXTERNALSYM WMAUDIO_MAX_CHANNELS}


  //  Windows Media Audio V1 (a.k.a. "MSAudio")
  //
  //      for WAVE_FORMAT_MSAUDIO1        ($0160)
  // =============================================

  MM_MSFT_ACM_MSAUDIO1                = 39;
  {$EXTERNALSYM MM_MSFT_ACM_MSAUDIO1}

type

  LPMSAUDIO1WAVEFORMAT = ^MSAUDIO1WAVEFORMAT;
  {$EXTERNALSYM LPMSAUDIO1WAVEFORMAT}
  PMSAUDIO1WAVEFORMAT = ^msaudio1waveformat_tag;
  msaudio1waveformat_tag = record
    wfx: WAVEFORMATEX;
    wSamplesPerBlock: WORD;         // only counting "new" samples "= half of what will be used due to overlapping
    wEncodeOptions: WORD;
  end;
  {$EXTERNALSYM msaudio1waveformat_tag}
  MSAUDIO1WAVEFORMAT = msaudio1waveformat_tag;
  {$EXTERNALSYM MSAUDIO1WAVEFORMAT}

const

  MSAUDIO1_BITS_PER_SAMPLE            = WMAUDIO_BITS_PER_SAMPLE;
  {$EXTERNALSYM MSAUDIO1_BITS_PER_SAMPLE}
  MSAUDIO1_MAX_CHANNELS               = WMAUDIO_MAX_CHANNELS;
  {$EXTERNALSYM MSAUDIO1_MAX_CHANNELS}
  MSAUDIO1_WFX_EXTRA_BYTES            = (sizeof (MSAUDIO1WAVEFORMAT)- sizeof (WAVEFORMATEX));
  {$EXTERNALSYM MSAUDIO1_WFX_EXTRA_BYTES}

  //  Windows Media Audio V2
  //
  //      for WAVE_FORMAT_WMAUDIO2        ($0161)
  // =============================================


  MM_MSFT_ACM_WMAUDIO2                = 101;
  {$EXTERNALSYM MM_MSFT_ACM_WMAUDIO2}

type

  LPWMAUDIO2WAVEFORMAT = ^WMAUDIO2WAVEFORMAT;
  {$EXTERNALSYM LPWMAUDIO2WAVEFORMAT}
  PWMAUDIO2WAVEFORMAT = ^wmaudio2waveformat_tag;
  wmaudio2waveformat_tag = record
    wfx: WAVEFORMATEX;
    dwSamplesPerBlock: DWORD;        // only counting "new" samples "= half of what will be used due to overlapping
    wEncodeOptions: WORD;
    dwSuperBlockAlign: DWORD;       // the big size...  should be multiples of wfx.nBlockAlign.
  end;
  {$EXTERNALSYM wmaudio2waveformat_tag}
  WMAUDIO2WAVEFORMAT = wmaudio2waveformat_tag;
  {$EXTERNALSYM WMAUDIO2WAVEFORMAT}


const

  WMAUDIO2_BITS_PER_SAMPLE            = WMAUDIO_BITS_PER_SAMPLE;
  {$EXTERNALSYM WMAUDIO2_BITS_PER_SAMPLE}
  WMAUDIO2_MAX_CHANNELS               = WMAUDIO_MAX_CHANNELS;
  {$EXTERNALSYM WMAUDIO2_MAX_CHANNELS}
  WMAUDIO2_WFX_EXTRA_BYTES            = (sizeof (WMAUDIO2WAVEFORMAT)- sizeof (WAVEFORMATEX));
  {$EXTERNALSYM WMAUDIO2_WFX_EXTRA_BYTES}


  //  Windows Media Audio V3
  //
  //      for WAVE_FORMAT_WMAUDIO3        ($0162)
  // =============================================

type

  LPWMAUDIO3WAVEFORMAT = ^WMAUDIO3WAVEFORMAT;
  {$EXTERNALSYM LPWMAUDIO3WAVEFORMAT}
  PWMAUDIO3WAVEFORMAT = ^wmaudio3waveformat_tag;
  wmaudio3waveformat_tag = record
    wfx: WAVEFORMATEX;
    wValidBitsPerSample: WORD;       // bits of precision
    dwChannelMask: DWORD;            // which channels are present in stream
    dwReserved1: DWORD;
    dwReserved2: DWORD;
    wEncodeOptions: WORD;
    wReserved3: WORD;
  end;
  {$EXTERNALSYM wmaudio3waveformat_tag}
  WMAUDIO3WAVEFORMAT = wmaudio3waveformat_tag;
  {$EXTERNALSYM WMAUDIO3WAVEFORMAT}


const

  WMAUDIO3_WFX_EXTRA_BYTES            = (sizeof (WMAUDIO3WAVEFORMAT)- sizeof (WAVEFORMATEX));
  {$EXTERNALSYM WMAUDIO3_WFX_EXTRA_BYTES}


type

  //  Creative's ADPCM structure definitions
  //  for WAVE_FORMAT_CREATIVE_ADPCM   ($0200)
  PCreativeADPCMWaveFormat = ^CreativeADPCMWaveFormat;
  CreativeADPCMWaveFormat = record
    wfx: WaveFormatEx;
    wRevision: Word;
  end;
  {$EXTERNALSYM CreativeADPCMWaveFormat}


  // Creative FASTSPEECH
  // WAVEFORMAT_CREATIVE_FASTSPEECH8   ($0202)
  PCreativeFastSpeech8WaveFormat = ^CreativeFastSpeech8WaveFormat;
  CreativeFastSpeech8WaveFormat = record
    wfx: WaveFormatEx;
    wRevision: Word;
  end;
  {$EXTERNALSYM CreativeFastSpeech8WaveFormat}


  // Creative FASTSPEECH
  // WAVEFORMAT_CREATIVE_FASTSPEECH10   ($0203)
  PCreativeFastSpeech10WaveFormat = ^CreativeFastSpeech10WaveFormat;
  CreativeFastSpeech10WaveFormat = record
    wfx: WaveFormatEx;
    wRevision: Word;
  end;
  {$EXTERNALSYM CreativeFastSpeech10WaveFormat}


  //  Fujitsu FM Towns 'SND' structure
  //  for WAVE_FORMAT_FMMTOWNS_SND   ($0300)
  PFMTowns_Snd_WaveFormat = ^FMTowns_Snd_WaveFormat;
  FMTowns_Snd_WaveFormat = record
    wfx: WaveFormatEx;
    wRevision: Word;
  end;
  {$EXTERNALSYM FMTowns_Snd_WaveFormat}


  //  Olivetti structure
  //  for WAVE_FORMAT_OLIGSM   ($1000)
  POliGSMWaveFormat = ^OliGSMWaveFormat;
  OliGSMWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM OliGSMWaveFormat}


  //  Olivetti structure
  //  for WAVE_FORMAT_OLIADPCM   ($1001)
  POliADPCMWaveFormat = ^OliADPCMWaveFormat;
  OliADPCMWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM OliADPCMWaveFormat}


  //  Olivetti structure
  //      for WAVE_FORMAT_OLICELP   ($1002)
  POliCelpWaveFormat = ^OliCelpWaveFormat;
  OliCelpWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM OliCelpWaveFormat}


  //  Olivetti structure
  //  for WAVE_FORMAT_OLISBC   ($1003)
 POliSbcWaveFormat = ^OliSbcWaveFormat;
 OliSbcWaveFormat = record
    wfx: WaveFormatEx;
  end;
 {$EXTERNALSYM OliSbcWaveFormat}


  //  Olivetti structure
  //  for WAVE_FORMAT_OLIOPR   ($1004)
  POliOprWaveFormat = ^OliOprWaveFormat;
  {$EXTERNALSYM OliOprWaveFormat}
  OliOprWaveFormat = record
    wfx: WaveFormatEx;
  end;


  //  Crystal Semiconductor IMA ADPCM format
  //  for WAVE_FORMAT_CS_IMAADPCM   ($0039)
  PCSIMAADPCMWaveFormat = ^CSIMAADPCMWaveFormat;
  CSIMAADPCMWaveFormat = record
    wfx: WaveFormatEx;
  end;
  {$EXTERNALSYM CSIMAADPCMWaveFormat}

  // =========================================================================;
  //
  //  ACM Wave Filters
  //
  //
  // =========================================================================;

const

  WAVE_FILTER_UNKNOWN     = $0000;
  {$EXTERNALSYM WAVE_FILTER_UNKNOWN}
  WAVE_FILTER_DEVELOPMENT = $FFFF;
  {$EXTERNALSYM WAVE_FILTER_DEVELOPMENT}

type

  PWAVEFILTER = ^WAVEFILTER;
  NPWAVEFILTER = ^WAVEFILTER;
  {$EXTERNALSYM NPWAVEFILTER}
  LPWAVEFILTER = ^WAVEFILTER;
  {$EXTERNALSYM LPWAVEFILTER}
  WaveFilter_tag = record
    cbStruct: DWORD;                      // Size of the filter in bytes
    dwFilterTag: DWORD;                   // filter type
    fdwFilter: DWORD;                     // Flags for the filter (Universal Dfns)
    dwReserved: array [0 .. 4] of DWORD;  // Reserved for system use
  end;
  {$EXTERNALSYM WaveFilter_tag}
  WAVEFILTER = WaveFilter_tag;
  {$EXTERNALSYM WAVEFILTER}


const

  WAVE_FILTER_VOLUME      = $0001;

type

  PVOLUMEWAVEFILTER = ^VOLUMEWAVEFILTER;
  NPVOLUMEWAVEFILTER = ^VOLUMEWAVEFILTER;
  {$EXTERNALSYM NPVOLUMEWAVEFILTER}
  LPVOLUMEWAVEFILTER = ^VOLUMEWAVEFILTER;
  {$EXTERNALSYM LPVOLUMEWAVEFILTER}
  wavefilter_volume_tag = record
    wfltr: WaveFilter;
    dwVolume: DWORD;
  end;
  {$EXTERNALSYM wavefilter_volume_tag}
  VOLUMEWAVEFILTER = wavefilter_volume_tag;
  {$EXTERNALSYM VOLUMEWAVEFILTER}


const

  WAVE_FILTER_ECHO        = $0002;
  {$EXTERNALSYM WAVE_FILTER_ECHO}

type

  PECHOWAVEFILTER = ^ECHOWAVEFILTER;
  NPECHOWAVEFILTER = ^ECHOWAVEFILTER;
  {$EXTERNALSYM NPECHOWAVEFILTER}
  LPECHOWAVEFILTER = ^ECHOWAVEFILTER;
  {$EXTERNALSYM LPECHOWAVEFILTER}
  wavefilter_echo_tag = record
    wfltr: WaveFilter;
    dwVolume: DWORD;
    dwDelay: DWORD;
  end;
  {$EXTERNALSYM wavefilter_echo_tag}
  ECHOWAVEFILTER = wavefilter_echo_tag;
  {$EXTERNALSYM ECHOWAVEFILTER}


  // New RIFF WAVE Chunks
  // ====================

const

  RIFFWAVE_INST = $74736e69;  // 'inst'
  {$EXTERNALSYM RIFFWAVE_INST}

type

  Ps_RIFFWAVE_INST = ^s_RIFFWAVE_INST;
  s_RIFFWAVE_INST = record
    bUnshiftedNote: Byte;
    chFineTune: ShortInt;
    chGain: ShortInt;
    bLowNote: Byte;
    bHighNote: Byte;
    bLowVelocity: Byte;
    bHighVelocity: Byte;
  end;
  {$EXTERNALSYM s_RIFFWAVE_INST}


  // New RIFF Forms
  // ==============

  // RIFF AVI
  //
  // AVI file format is specified in a seperate file (AVIFMT.H),
  // which is available in the VfW and Win 32 SDK

  // RIFF CPPO

const  //hardcoded

  RIFFCPPO      = $4f505043;   // 'CPPO'
  {$EXTERNALSYM RIFFCPPO}

  RIFFCPPO_objr = $726a626f;   // 'objr'
  {$EXTERNALSYM RIFFCPPO_objr}
  RIFFCPPO_obji = $696a626f;   // 'obji'
  {$EXTERNALSYM RIFFCPPO_obji}

  RIFFCPPO_clsr = $72736c63;   // 'clsr'
  {$EXTERNALSYM RIFFCPPO_clsr}
  RIFFCPPO_clsi = $69736c63;   // 'clsi'
  {$EXTERNALSYM RIFFCPPO_clsi}

  RIFFCPPO_mbr  = $2072626d;   // 'mbr '
  {$EXTERNALSYM RIFFCPPO_mbr}

  RIFFCPPO_char = $72616863;   // 'char'
  {$EXTERNALSYM RIFFCPPO_char}

  RIFFCPPO_byte = $65747962;   // 'byte'
  {$EXTERNALSYM RIFFCPPO_byte}
  RIFFCPPO_int  = $20746e69;   // 'int '
  {$EXTERNALSYM RIFFCPPO_int}
  RIFFCPPO_word = $64726f77;   // 'word'
  {$EXTERNALSYM RIFFCPPO_word}
  RIFFCPPO_long = $676e6f6c;   // 'long'
  {$EXTERNALSYM RIFFCPPO_long}
  RIFFCPPO_dwrd = $64627764;   // 'dwrd'
  {$EXTERNALSYM RIFFCPPO_dwrd}
  RIFFCPPO_flt  = $20746c66;   // 'flt '
  {$EXTERNALSYM RIFFCPPO_flt}
  RIFFCPPO_dbl  = $206c6264;   // 'dbl '
  {$EXTERNALSYM RIFFCPPO_dbl}
  RIFFCPPO_str  = $20727473;   // 'str '
  {$EXTERNALSYM RIFFCPPO_str}


 // DIB Compression Defines
 // =======================

const

  BI_BITFIELDS = 3;
  {$EXTERNALSYM BI_BITFIELDS}

  QUERYDIBSUPPORT = 3073;
  {$EXTERNALSYM QUERYDIBSUPPORT}
  QDI_SETDIBITS   = $0001;
  {$EXTERNALSYM QDI_SETDIBITS}
  QDI_GETDIBITS   = $0002;
  {$EXTERNALSYM QDI_GETDIBITS}
  QDI_DIBTOSCREEN = $0004;
  {$EXTERNALSYM QDI_DIBTOSCREEN}
  QDI_STRETCHDIB  = $0008;
  {$EXTERNALSYM QDI_STRETCHDIB}

type

  // Structure definitions
  PExBmInfoHeader = ^ExBmInfoHeader;
  ExBmInfoHeader = record
    bmi: BitmapInfoHeader;
    // extended BITMAPINFOHEADER fields
    biExtDataOffset: DWORD;
  end;
  {$EXTERNALSYM ExBmInfoHeader}


const

  // New DIB Compression Defines
  BICOMP_IBMULTIMOTION  = $49544c55;  // 'ULTI'
  {$EXTERNALSYM BICOMP_IBMULTIMOTION}
  BICOMP_IBMPHOTOMOTION = $4f4d4850;  // 'PHMO'
  {$EXTERNALSYM BICOMP_IBMPHOTOMOTION}
  BICOMP_CREATIVEYUV    = $56555943;  // 'cyuv'
  {$EXTERNALSYM BICOMP_CREATIVEYUV}

  // New DIB Compression Defines
  JPEG_DIB              = $4745504a;  // 'JPEG' Still image JPEG DIB biCompression
  {$EXTERNALSYM JPEG_DIB}
  MJPG_DIB              = $4745504d;  // 'MJPG' Motion JPEG DIB biCompression
  {$EXTERNALSYM MJPG_DIB}

  // JPEGProcess Definitions
  JPEG_PROCESS_BASELINE = 0;          // Baseline DCT
  {$EXTERNALSYM JPEG_PROCESS_BASELINE}

  // AVI File format extensions
  AVIIF_CONTROLFRAME    = $00000200;  // This is a control frame
  {$EXTERNALSYM AVIIF_CONTROLFRAME}


  // JIF Marker byte pairs in JPEG Interchange Format sequence
  JIFMK_SOF0                          = $FFC0;  // SOF Huff  - Baseline DCT
  {$EXTERNALSYM JIFMK_SOF0}
  JIFMK_SOF1                          = $FFC1;  // SOF Huff  - Extended sequential DCT
  {$EXTERNALSYM JIFMK_SOF1}
  JIFMK_SOF2                          = $FFC2;  // SOF Huff  - Progressive DCT
  {$EXTERNALSYM JIFMK_SOF2}
  JIFMK_SOF3                          = $FFC3;  // SOF Huff  - Spatial (sequential) lossless
  {$EXTERNALSYM JIFMK_SOF3}
  JIFMK_SOF5                          = $FFC5;  // SOF Huff  - Differential sequential DCT
  {$EXTERNALSYM JIFMK_SOF5}
  JIFMK_SOF6                          = $FFC6;  // SOF Huff  - Differential progressive DCT
  {$EXTERNALSYM JIFMK_SOF6}
  JIFMK_SOF7                          = $FFC7;  // SOF Huff  - Differential spatial
  {$EXTERNALSYM JIFMK_SOF7}
  JIFMK_JPG                           = $FFC8;  // SOF Arith - Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG}
  JIFMK_SOF9                          = $FFC9;  // SOF Arith - Extended sequential DCT
  {$EXTERNALSYM JIFMK_SOF9}
  JIFMK_SOF10                         = $FFCA;  // SOF Arith - Progressive DCT
  {$EXTERNALSYM JIFMK_SOF10}
  JIFMK_SOF11                         = $FFCB;  // SOF Arith - Spatial (sequential) lossless
  {$EXTERNALSYM JIFMK_SOF11}
  JIFMK_SOF13                         = $FFCD;  // SOF Arith - Differential sequential DCT
  {$EXTERNALSYM JIFMK_SOF13}
  JIFMK_SOF14                         = $FFCE;  // SOF Arith - Differential progressive DCT
  {$EXTERNALSYM JIFMK_SOF14}
  JIFMK_SOF15                         = $FFCF;  // SOF Arith - Differential spatial
  {$EXTERNALSYM JIFMK_SOF15}
  JIFMK_DHT                           = $FFC4;  // Define Huffman Table(s)
  {$EXTERNALSYM JIFMK_DHT}
  JIFMK_DAC                           = $FFCC;  // Define Arithmetic coding conditioning(s)
  {$EXTERNALSYM JIFMK_DAC}
  JIFMK_RST0                          = $FFD0;  // Restart with modulo 8 count 0
  {$EXTERNALSYM JIFMK_RST0}
  JIFMK_RST1                          = $FFD1;  // Restart with modulo 8 count 1
  {$EXTERNALSYM JIFMK_RST1}
  JIFMK_RST2                          = $FFD2;  // Restart with modulo 8 count 2
  {$EXTERNALSYM JIFMK_RST2}
  JIFMK_RST3                          = $FFD3;  // Restart with modulo 8 count 3
  {$EXTERNALSYM JIFMK_RST3}
  JIFMK_RST4                          = $FFD4;  // Restart with modulo 8 count 4
  {$EXTERNALSYM JIFMK_RST4}
  JIFMK_RST5                          = $FFD5;  // Restart with modulo 8 count 5
  {$EXTERNALSYM JIFMK_RST5}
  JIFMK_RST6                          = $FFD6;  // Restart with modulo 8 count 6
  {$EXTERNALSYM JIFMK_RST6}
  JIFMK_RST7                          = $FFD7;  // Restart with modulo 8 count 7
  {$EXTERNALSYM JIFMK_RST7}
  JIFMK_SOI                           = $FFD8;  // Start of Image
  {$EXTERNALSYM JIFMK_SOI}
  JIFMK_EOI                           = $FFD9;  // End of Image
  {$EXTERNALSYM JIFMK_EOI}
  JIFMK_SOS                           = $FFDA;  // Start of Scan
  {$EXTERNALSYM JIFMK_SOS}
  JIFMK_DQT                           = $FFDB;  // Define quantization Table(s)
  {$EXTERNALSYM JIFMK_DQT}
  JIFMK_DNL                           = $FFDC;  // Define Number of Lines
  {$EXTERNALSYM JIFMK_DNL}
  JIFMK_DRI                           = $FFDD;  // Define Restart Interval
  {$EXTERNALSYM JIFMK_DRI}
  JIFMK_DHP                           = $FFDE;  // Define Hierarchical progression
  {$EXTERNALSYM JIFMK_DHP}
  JIFMK_EXP                           = $FFDF;  // Expand Reference Component(s)
  {$EXTERNALSYM JIFMK_EXP}
  JIFMK_APP0                          = $FFE0;  // Application Field 0
  {$EXTERNALSYM JIFMK_APP0}
  JIFMK_APP1                          = $FFE1;  // Application Field 1
  {$EXTERNALSYM JIFMK_APP1}
  JIFMK_APP2                          = $FFE2;  // Application Field 2
  {$EXTERNALSYM JIFMK_APP2}
  JIFMK_APP3                          = $FFE3;  // Application Field 3
  {$EXTERNALSYM JIFMK_APP3}
  JIFMK_APP4                          = $FFE4;  // Application Field 4
  {$EXTERNALSYM JIFMK_APP4}
  JIFMK_APP5                          = $FFE5;  // Application Field 5
  {$EXTERNALSYM JIFMK_APP5}
  JIFMK_APP6                          = $FFE6;  // Application Field 6
  {$EXTERNALSYM JIFMK_APP6}
  JIFMK_APP7                          = $FFE7;  // Application Field 7
  {$EXTERNALSYM JIFMK_APP7}
  JIFMK_JPG0                          = $FFF0;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG0}
  JIFMK_JPG1                          = $FFF1;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG1}
  JIFMK_JPG2                          = $FFF2;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG2}
  JIFMK_JPG3                          = $FFF3;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG3}
  JIFMK_JPG4                          = $FFF4;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG4}
  JIFMK_JPG5                          = $FFF5;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG5}
  JIFMK_JPG6                          = $FFF6;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG6}
  JIFMK_JPG7                          = $FFF7;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG7}
  JIFMK_JPG8                          = $FFF8;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG8}
  JIFMK_JPG9                          = $FFF9;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG9}
  JIFMK_JPG10                         = $FFFA;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG10}
  JIFMK_JPG11                         = $FFFB;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG11}
  JIFMK_JPG12                         = $FFFC;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG12}
  JIFMK_JPG13                         = $FFFD;  // Reserved for JPEG extensions
  {$EXTERNALSYM JIFMK_JPG13}
  JIFMK_COM                           = $FFFE;  // Comment
  {$EXTERNALSYM JIFMK_COM}
  JIFMK_TEM                           = $FF01;  // for temp private use arith code
  {$EXTERNALSYM JIFMK_TEM}
  JIFMK_RES                           = $FF02;  // Reserved
  {$EXTERNALSYM JIFMK_RES}
  JIFMK_00                            = $FF00;  // Zero stuffed byte - entropy data
  {$EXTERNALSYM JIFMK_00}
  JIFMK_FF                            = $FFFF;  // Fill byte
  {$EXTERNALSYM JIFMK_FF}

  // JPEGColorSpaceID Definitions

  JPEG_Y                              = 1;  // Y only component of YCbCr
  {$EXTERNALSYM JPEG_Y}
  JPEG_YCbCr                          = 2;  // YCbCr as define by CCIR 601
  {$EXTERNALSYM JPEG_YCbCr}
  JPEG_RGB                            = 3;  // 3 component RGB
  {$EXTERNALSYM JPEG_RGB}


type

  // Structure definitions
  PJPEGInfoHeader = ^JPEGInfoHeader;
  JPEGInfoHeader = record
    // compression-specific fields
    // these fields are defined for 'JPEG' and 'MJPG'
    JPEGSize: DWORD;
    JPEGProcess: DWORD;
    // Process specific fields
    JPEGColorSpaceID: DWORD;
    JPEGBitsPerSample: DWORD;
    JPEGHSubSampling: DWORD;
    JPEGVSubSampling: DWORD;
  end;
  {$EXTERNALSYM JPEGInfoHeader}

const

  // Default DHT Segment
  MJPGDHTSeg: array [0 .. $1a3] of byte = (
    // JPEG DHT Segment for YCrCb omitted from MJPG data
    $FF,$C4,$01,$A2,
    $00,$00,$01,$05,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,
    $01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$01,$00,$03,$01,$01,$01,$01,
    $01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$06,$07,
    $08,$09,$0A,$0B,$10,$00,$02,$01,$03,$03,$02,$04,$03,$05,$05,$04,$04,$00,
    $00,$01,$7D,$01,$02,$03,$00,$04,$11,$05,$12,$21,$31,$41,$06,$13,$51,$61,
    $07,$22,$71,$14,$32,$81,$91,$A1,$08,$23,$42,$B1,$C1,$15,$52,$D1,$F0,$24,
    $33,$62,$72,$82,$09,$0A,$16,$17,$18,$19,$1A,$25,$26,$27,$28,$29,$2A,$34,
    $35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,$47,$48,$49,$4A,$53,$54,$55,$56,
    $57,$58,$59,$5A,$63,$64,$65,$66,$67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,
    $79,$7A,$83,$84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,$98,$99,
    $9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,
    $BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,
    $DA,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$F1,$F2,$F3,$F4,$F5,$F6,$F7,
    $F8,$F9,$FA,$11,$00,$02,$01,$02,$04,$04,$03,$04,$07,$05,$04,$04,$00,$01,
    $02,$77,$00,$01,$02,$03,$11,$04,$05,$21,$31,$06,$12,$41,$51,$07,$61,$71,
    $13,$22,$32,$81,$08,$14,$42,$91,$A1,$B1,$C1,$09,$23,$33,$52,$F0,$15,$62,
    $72,$D1,$0A,$16,$24,$34,$E1,$25,$F1,$17,$18,$19,$1A,$26,$27,$28,$29,$2A,
    $35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,$47,$48,$49,$4A,$53,$54,$55,$56,
    $57,$58,$59,$5A,$63,$64,$65,$66,$67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,
    $79,$7A,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,$98,
    $99,$9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,$B4,$B5,$B6,$B7,$B8,
    $B9,$BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,
    $D9,$DA,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$F2,$F3,$F4,$F5,$F6,$F7,$F8,
    $F9,$FA
  );
  {$EXTERNALSYM MJPGDHTSeg}

//------------------------------------------------------------------------------


  // Defined IC types
  // ================

  ICTYPE_VIDEO = ord('v') or ord('i') shl 8 or (ord('d') shl 16) or (ord('c') shl 24);
  {$EXTERNALSYM ICTYPE_VIDEO}
  ICTYPE_AUDIO = ord('a') or ord('u') shl 8 or (ord('d') shl 16) or (ord('c') shl 24);
  {$EXTERNALSYM ICTYPE_AUDIO}

  //template // = ord('') or (ord( '') shl 8) or (ord( '') shl 16) or (ord( '');

const
  // Misc. FOURCC registration

  // Sierra Semiconductor: RDSP- Confidential RIFF file format
  //       for the storage and downloading of DSP
  //       code for Audio and communications devices.
  FOURCC_RDSP  = $50534452;  // 'RDSP'
  {$EXTERNALSYM FOURCC_RDSP}

  MIXERCONTROL_CONTROLTYPE_SRS_MTS         = MIXERCONTROL_CONTROLTYPE_BOOLEAN + 6;
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_SRS_MTS}
  MIXERCONTROL_CONTROLTYPE_SRS_ONOFF       = MIXERCONTROL_CONTROLTYPE_BOOLEAN + 7;
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_SRS_ONOFF}
  MIXERCONTROL_CONTROLTYPE_SRS_SYNTHSELECT = MIXERCONTROL_CONTROLTYPE_BOOLEAN + 8;
  {$EXTERNALSYM MIXERCONTROL_CONTROLTYPE_SRS_SYNTHSELECT}



  // Additional Prototypes for ALL interfaces

  function GETFOURCC(frcc: FOURCC): WideString;

  // End of Additional Prototypes

implementation


// create a FOURCC code
function MAKEFOURCC(const ch0: AnsiChar;
                    const ch1: AnsiChar;
                    const ch2: AnsiChar;
                    const ch3: AnsiChar): FOURCC;
begin
  Result := DWORD(Ord(ch0)) or
           (DWORD(Ord(ch1)) shl 8) or
           (DWORD(Ord(ch2)) shl 16) or
           (DWORD(Ord(ch3)) shl 24);
end;


// Get FOURCC as string
function GETFOURCC(frcc: FOURCC): WideString;
var
  afc : array[0..3] of AnsiChar;

begin
  afc[0] := AnsiChar(ord(frcc));
  afc[1] := AnsiChar(ord(frcc shr 8));
  afc[2] := AnsiChar(ord(frcc shr 16));
  afc[3] := AnsiChar(ord(frcc shr 24));
  Result := WideString(afc);
end;


function INIT_MMREG_MID(const guid: TGuid;
                        const id: USHORT): TGUID;
begin
  Result.D1:= $d5a47fa7 + id;
  Result.D2:= $6d98;
  Result.D3:= $6d98;
  Result.D4[0]:= $a2;
  Result.D4[1]:= $1a;
  Result.D4[2]:= $00;
  Result.D4[3]:= $a0;
  Result.D4[4]:= $c9;
  Result.D4[5]:= $22;
  Result.D4[6]:= $31;
  Result.D4[7]:= $96;
end;


function EXTRACT_MMREG_MID(const guid: TGUID): USHORT;
begin
  Result:= guid.D1 - $d5a47fa7;
end;


function DEFINE_MMREG_MID_GUID(const id: USHORT): TGUID;
begin
  Result.D1:= $d5a47fa7 + id;
  Result.D2:= $6d98;
  Result.D3:= $11d1;
  Result.D4[0]:= $a2;
  Result.D4[1]:= $1a;
  Result.D4[2]:= $00;
  Result.D4[3]:= $a0;
  Result.D4[4]:= $c9;
  Result.D4[5]:= $22;
  Result.D4[6]:= $31;
  Result.D4[7]:= $96;
end;


function IS_COMPATIBLE_MMREG_MID(const guid: TGUID): BOOL;
begin
  Result:= (guid.D1 >= $d5a47fa7) AND
           (guid.D1 < $d5a47fa7 + $ffff) AND
           (guid.D2 = $6d98) AND
           (guid.D3 = $11d1) AND
           (guid.D4[0] = $a2) AND
           (guid.D4[1] = $1a) AND
           (guid.D4[2] = $00) AND
           (guid.D4[3] = $a0) AND
           (guid.D4[4] = $c9) AND
           (guid.D4[5] = $22) AND
           (guid.D4[6] = $31) AND
           (guid.D4[7] = $96);
end;


function INIT_MMREG_PID(const guid: TGUID;
                        const id: USHORT): TGUID;
begin
  Result.D1:= $e36dc2ac + id;
  Result.D2:= $6d9a;
  Result.D3:= $11d1;
  Result.D4[0]:= $a2;
  Result.D4[1]:= $1a;
  Result.D4[2]:= $00;
  Result.D4[3]:= $a0;
  Result.D4[4]:= $c9;
  Result.D4[5]:= $22;
  Result.D4[6]:= $31;
  Result.D4[7]:= $96;
end;


function EXTRACT_MMREG_PID(const guid: TGUID): USHORT;
begin
  Result:= guid.D1 - $e36dc2ac;
end;


function DEFINE_MMREG_PID_GUID(const id: USHORT): TGUID;
begin
  Result.D1:= $e36dc2ac + id;
  Result.D2:= $6d9a;
  Result.D3:= $11d1;
  Result.D4[0]:= $a2;
  Result.D4[1]:= $1a;
  Result.D4[2]:= $00;
  Result.D4[3]:= $a0;
  Result.D4[4]:= $c9;
  Result.D4[5]:= $22;
  Result.D4[6]:= $31;
  Result.D4[7]:= $96;
end;



function IS_COMPATIBLE_MMREG_PID(const guid: TGUID): BOOL;
begin
  Result:= (guid.D1 >= $e36dc2ac) AND
           (guid.D1 < $e36dc2ac + $ffff) AND
           (guid.D2 = $6d9a) AND
           (guid.D3 = $11d1) AND
           (guid.D4[0] = $a2) AND
           (guid.D4[1] = $1a) AND
           (guid.D4[2] = $00) AND
           (guid.D4[3] = $a0) AND
           (guid.D4[4] = $c9) AND
           (guid.D4[5] = $22) AND
           (guid.D4[6] = $31) AND
           (guid.D4[7] = $96);
end;

// Implement Additional Prototypes here.

end.
