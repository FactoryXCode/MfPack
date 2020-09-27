// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MCIApi.pas
// Kind: Pascal / Delphi unit
// Release date: 17-05-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: ApiSet Contract for api-ms-win-mm-mci-l1-1-0.
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
// Remarks:
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
// Source: digitalv.h
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
//==============================================================================
unit WinApi.WinMM.MCIApi;

interface

uses
  WinApi.Windows,
  WinApi.WinMM.MMSysCom,
  WinApi.WinMM.MMiscApi;

  //****************************************************************************
  //
  //                          MCI support
  //
  //****************************************************************************

type

  { MCIERROR is defined in some post 3.1 apps }

  MCIERROR = DWORD;
  {$EXTERNALSYM MCIERROR}

  { error return code, 0 means no error }

  MCIDEVICEID = UINT;
  {$EXTERNALSYM MCIDEVICEID}

  { MCI device ID type }

  YIELDPROC = function(mciId: MCIDEVICEID;
                       dwYieldData: DWORD): UINT; stdcall;
  {$EXTERNALSYM YIELDPROC}

  { MCI function prototypes }

  function mciSendCommandA(mciId: MCIDEVICEID;
                           uMsg: UINT;
                           {_In_opt_} dwParam1: DWORD_PTR;
                           {_In_opt_} dwParam2: DWORD_PTR): MCIERROR; stdcall;
  {$EXTERNALSYM mciSendCommandA}

  function mciSendCommandW(mciId: MCIDEVICEID;
                           uMsg: UINT;
                           {_In_opt_} dwParam1: DWORD_PTR;
                           {_In_opt_} dwParam2: DWORD_PTR): MCIERROR; stdcall;
  {$EXTERNALSYM mciSendCommandW}

  { unicode }
  function mciSendCommand(mciId: MCIDEVICEID;
                          uMsg: UINT;
                          {_In_opt_} dwParam1: DWORD_PTR;
                          {_In_opt_} dwParam2: DWORD_PTR): MCIERROR; stdcall;
  {$EXTERNALSYM mciSendCommand}


  function mciSendStringA(lpstrCommand: PAnsiChar;
                          lpstrReturnString: PAnsiChar;
                          uReturnLength: UINT;
                          {_In_opt_} hwndCallback: HWND): MCIERROR; stdcall;
  {$EXTERNALSYM mciSendStringA}

  function mciSendStringW(lpstrCommand: PWideChar;
                          lpstrReturnString: PWideChar;
                          uReturnLength: UINT;
                          {_In_opt_} hwndCallback: HWND): MCIERROR; stdcall;
  {$EXTERNALSYM mciSendStringW}

  { unicode }
  function mciSendString(lpstrCommand: PWideChar;
                         lpstrReturnString: PWideChar;
                         uReturnLength: UINT;
                         {_In_opt_} hwndCallback: HWND): MCIERROR; stdcall;
  {$EXTERNALSYM mciSendString}


  function mciGetDeviceIDA(pszDevice: PAnsiChar): MCIDEVICEID; stdcall;
  {$EXTERNALSYM mciGetDeviceIDA}

  function mciGetDeviceIDW(pszDevice: PWideChar): MCIDEVICEID; stdcall;
  {$EXTERNALSYM mciGetDeviceIDW}

  { unicode }
  function mciGetDeviceID(pszDevice: PWideChar): MCIDEVICEID; stdcall;
  {$EXTERNALSYM mciGetDeviceID}


  function mciGetDeviceIDFromElementIDA(dwElementID: DWORD;
                                        lpstrType: PAnsiChar): MCIDEVICEID; stdcall;
  {$EXTERNALSYM mciGetDeviceIDFromElementIDA}


  function mciGetDeviceIDFromElementIDW(dwElementID: DWORD;
                                        lpstrType: PWideChar): MCIDEVICEID; stdcall;
  {$EXTERNALSYM mciGetDeviceIDFromElementIDW}

  { unicode }
  function mciGetDeviceIDFromElementID(dwElementID: DWORD;
                                       lpstrType: PWideChar): MCIDEVICEID; stdcall;
  {$EXTERNALSYM mciGetDeviceIDFromElementID}


  function mciGetErrorStringA(mcierr: MCIERROR;
                              {_Out_} pszText: PAnsiChar;
                              cchText: UINT): BOOL; stdcall;
  {$EXTERNALSYM mciGetErrorStringA}

  function mciGetErrorStringW(mcierr: MCIERROR;
                              {_Out_} pszText: PWideChar;
                              cchText: UINT): BOOL; stdcall;
  {$EXTERNALSYM mciGetErrorStringW}

  { unicode }
  function mciGetErrorString(mcierr: MCIERROR;
                             {_Out_} pszText: PWideChar;
                             cchText: UINT): BOOL; stdcall;
  {$EXTERNALSYM mciGetErrorString}


  function mciSetYieldProc(mciId: MCIDEVICEID;
                           {_In_opt_} fpYieldProc: YIELDPROC;
                           dwYieldData: DWORD): BOOL; stdcall;
  {$EXTERNALSYM mciSetYieldProc}


  function mciGetCreatorTask(mciId: MCIDEVICEID): HTASK; stdcall;
  {$EXTERNALSYM mciGetCreatorTask}

  function mciGetYieldProc(mciId: MCIDEVICEID;
                           pdwYieldData: PDWORD): YIELDPROC; stdcall;
  {$EXTERNALSYM mciGetYieldProc}


  { MCI error return values }

const

  MCIERR_INVALID_DEVICE_ID            = (MCIERR_BASE + 1);
  {$EXTERNALSYM MCIERR_INVALID_DEVICE_ID}
  MCIERR_UNRECOGNIZED_KEYWORD         = (MCIERR_BASE + 3);
  {$EXTERNALSYM MCIERR_UNRECOGNIZED_KEYWORD}
  MCIERR_UNRECOGNIZED_COMMAND         = (MCIERR_BASE + 5);
  {$EXTERNALSYM MCIERR_UNRECOGNIZED_COMMAND}
  MCIERR_HARDWARE                     = (MCIERR_BASE + 6);
  {$EXTERNALSYM MCIERR_HARDWARE}
  MCIERR_INVALID_DEVICE_NAME          = (MCIERR_BASE + 7);
  {$EXTERNALSYM MCIERR_INVALID_DEVICE_NAME}
  MCIERR_OUT_OF_MEMORY                = (MCIERR_BASE + 8);
  {$EXTERNALSYM MCIERR_OUT_OF_MEMORY}
  MCIERR_DEVICE_OPEN                  = (MCIERR_BASE + 9);
  {$EXTERNALSYM MCIERR_DEVICE_OPEN}
  MCIERR_CANNOT_LOAD_DRIVER           = (MCIERR_BASE + 10);
  {$EXTERNALSYM MCIERR_CANNOT_LOAD_DRIVER}
  MCIERR_MISSING_COMMAND_STRING       = (MCIERR_BASE + 11);
  {$EXTERNALSYM MCIERR_MISSING_COMMAND_STRING}
  MCIERR_PARAM_OVERFLOW               = (MCIERR_BASE + 12);
  {$EXTERNALSYM MCIERR_PARAM_OVERFLOW}
  MCIERR_MISSING_STRING_ARGUMENT      = (MCIERR_BASE + 13);
  {$EXTERNALSYM MCIERR_MISSING_STRING_ARGUMENT}
  MCIERR_BAD_INTEGER                  = (MCIERR_BASE + 14);
  {$EXTERNALSYM MCIERR_BAD_INTEGER}
  MCIERR_PARSER_INTERNAL              = (MCIERR_BASE + 15);
  {$EXTERNALSYM MCIERR_PARSER_INTERNAL}
  MCIERR_DRIVER_INTERNAL              = (MCIERR_BASE + 16);
  {$EXTERNALSYM MCIERR_DRIVER_INTERNAL}
  MCIERR_MISSING_PARAMETER            = (MCIERR_BASE + 17);
  {$EXTERNALSYM MCIERR_MISSING_PARAMETER}
  MCIERR_UNSUPPORTED_FUNCTION         = (MCIERR_BASE + 18);
  {$EXTERNALSYM MCIERR_UNSUPPORTED_FUNCTION}
  MCIERR_FILE_NOT_FOUND               = (MCIERR_BASE + 19);
  {$EXTERNALSYM MCIERR_FILE_NOT_FOUND}
  MCIERR_DEVICE_NOT_READY             = (MCIERR_BASE + 20);
  {$EXTERNALSYM MCIERR_DEVICE_NOT_READY}
  MCIERR_INTERNAL                     = (MCIERR_BASE + 21);
  {$EXTERNALSYM MCIERR_INTERNAL}
  MCIERR_DRIVER                       = (MCIERR_BASE + 22);
  {$EXTERNALSYM MCIERR_DRIVER}
  MCIERR_CANNOT_USE_ALL               = (MCIERR_BASE + 23);
  {$EXTERNALSYM MCIERR_CANNOT_USE_ALL}
  MCIERR_MULTIPLE                     = (MCIERR_BASE + 24);
  {$EXTERNALSYM MCIERR_MULTIPLE}
  MCIERR_EXTENSION_NOT_FOUND          = (MCIERR_BASE + 25);
  {$EXTERNALSYM MCIERR_EXTENSION_NOT_FOUND}
  MCIERR_OUTOFRANGE                   = (MCIERR_BASE + 26);
  {$EXTERNALSYM MCIERR_OUTOFRANGE}
  MCIERR_FLAGS_NOT_COMPATIBLE         = (MCIERR_BASE + 28);
  {$EXTERNALSYM MCIERR_FLAGS_NOT_COMPATIBLE}
  MCIERR_FILE_NOT_SAVED               = (MCIERR_BASE + 30);
  {$EXTERNALSYM MCIERR_FILE_NOT_SAVED}
  MCIERR_DEVICE_TYPE_REQUIRED         = (MCIERR_BASE + 31);
  {$EXTERNALSYM MCIERR_DEVICE_TYPE_REQUIRED}
  MCIERR_DEVICE_LOCKED                = (MCIERR_BASE + 32);
  {$EXTERNALSYM MCIERR_DEVICE_LOCKED}
  MCIERR_DUPLICATE_ALIAS              = (MCIERR_BASE + 33);
  {$EXTERNALSYM MCIERR_DUPLICATE_ALIAS}
  MCIERR_BAD_CONSTANT                 = (MCIERR_BASE + 34);
  {$EXTERNALSYM MCIERR_BAD_CONSTANT}
  MCIERR_MUST_USE_SHAREABLE           = (MCIERR_BASE + 35);
  {$EXTERNALSYM MCIERR_MUST_USE_SHAREABLE}
  MCIERR_MISSING_DEVICE_NAME          = (MCIERR_BASE + 36);
  {$EXTERNALSYM MCIERR_MISSING_DEVICE_NAME}
  MCIERR_BAD_TIME_FORMAT              = (MCIERR_BASE + 37);
  {$EXTERNALSYM MCIERR_BAD_TIME_FORMAT}
  MCIERR_NO_CLOSING_QUOTE             = (MCIERR_BASE + 38);
  {$EXTERNALSYM MCIERR_NO_CLOSING_QUOTE}
  MCIERR_DUPLICATE_FLAGS              = (MCIERR_BASE + 39);
  {$EXTERNALSYM MCIERR_DUPLICATE_FLAGS}
  MCIERR_INVALID_FILE                 = (MCIERR_BASE + 40);
  {$EXTERNALSYM MCIERR_INVALID_FILE}
  MCIERR_NULL_PARAMETER_BLOCK         = (MCIERR_BASE + 41);
  {$EXTERNALSYM MCIERR_NULL_PARAMETER_BLOCK}
  MCIERR_UNNAMED_RESOURCE             = (MCIERR_BASE + 42);
  {$EXTERNALSYM MCIERR_UNNAMED_RESOURCE}
  MCIERR_NEW_REQUIRES_ALIAS           = (MCIERR_BASE + 43);
  {$EXTERNALSYM MCIERR_NEW_REQUIRES_ALIAS}
  MCIERR_NOTIFY_ON_AUTO_OPEN          = (MCIERR_BASE + 44);
  {$EXTERNALSYM MCIERR_NOTIFY_ON_AUTO_OPEN}
  MCIERR_NO_ELEMENT_ALLOWED           = (MCIERR_BASE + 45);
  {$EXTERNALSYM MCIERR_NO_ELEMENT_ALLOWED}
  MCIERR_NONAPPLICABLE_FUNCTION       = (MCIERR_BASE + 46);
  {$EXTERNALSYM MCIERR_NONAPPLICABLE_FUNCTION}
  MCIERR_ILLEGAL_FOR_AUTO_OPEN        = (MCIERR_BASE + 47);
  {$EXTERNALSYM MCIERR_ILLEGAL_FOR_AUTO_OPEN}
  MCIERR_FILENAME_REQUIRED            = (MCIERR_BASE + 48);
  {$EXTERNALSYM MCIERR_FILENAME_REQUIRED}
  MCIERR_EXTRA_CHARACTERS             = (MCIERR_BASE + 49);
  {$EXTERNALSYM MCIERR_EXTRA_CHARACTERS}
  MCIERR_DEVICE_NOT_INSTALLED         = (MCIERR_BASE + 50);
  {$EXTERNALSYM MCIERR_DEVICE_NOT_INSTALLED}
  MCIERR_GET_CD                       = (MCIERR_BASE + 51);
  {$EXTERNALSYM MCIERR_GET_CD}
  MCIERR_SET_CD                       = (MCIERR_BASE + 52);
  {$EXTERNALSYM MCIERR_SET_CD}
  MCIERR_SET_DRIVE                    = (MCIERR_BASE + 53);
  {$EXTERNALSYM MCIERR_SET_DRIVE}
  MCIERR_DEVICE_LENGTH                = (MCIERR_BASE + 54);
  {$EXTERNALSYM MCIERR_DEVICE_LENGTH}
  MCIERR_DEVICE_ORD_LENGTH            = (MCIERR_BASE + 55);
  {$EXTERNALSYM MCIERR_DEVICE_ORD_LENGTH}
  MCIERR_NO_INTEGER                   = (MCIERR_BASE + 56);
  {$EXTERNALSYM MCIERR_NO_INTEGER}
  MCIERR_WAVE_OUTPUTSINUSE            = (MCIERR_BASE + 64);
  {$EXTERNALSYM MCIERR_WAVE_OUTPUTSINUSE}
  MCIERR_WAVE_SETOUTPUTINUSE          = (MCIERR_BASE + 65);
  {$EXTERNALSYM MCIERR_WAVE_SETOUTPUTINUSE}
  MCIERR_WAVE_INPUTSINUSE             = (MCIERR_BASE + 66);
  {$EXTERNALSYM MCIERR_WAVE_INPUTSINUSE}
  MCIERR_WAVE_SETINPUTINUSE           = (MCIERR_BASE + 67);
  {$EXTERNALSYM MCIERR_WAVE_SETINPUTINUSE}
  MCIERR_WAVE_OUTPUTUNSPECIFIED       = (MCIERR_BASE + 68);
  {$EXTERNALSYM MCIERR_WAVE_OUTPUTUNSPECIFIED}
  MCIERR_WAVE_INPUTUNSPECIFIED        = (MCIERR_BASE + 69);
  {$EXTERNALSYM MCIERR_WAVE_INPUTUNSPECIFIED}
  MCIERR_WAVE_OUTPUTSUNSUITABLE       = (MCIERR_BASE + 70);
  {$EXTERNALSYM MCIERR_WAVE_OUTPUTSUNSUITABLE}
  MCIERR_WAVE_SETOUTPUTUNSUITABLE     = (MCIERR_BASE + 71);
  {$EXTERNALSYM MCIERR_WAVE_SETOUTPUTUNSUITABLE}
  MCIERR_WAVE_INPUTSUNSUITABLE        = (MCIERR_BASE + 72);
  {$EXTERNALSYM MCIERR_WAVE_INPUTSUNSUITABLE}
  MCIERR_WAVE_SETINPUTUNSUITABLE      = (MCIERR_BASE + 73);
  {$EXTERNALSYM MCIERR_WAVE_SETINPUTUNSUITABLE}
  MCIERR_SEQ_DIV_INCOMPATIBLE         = (MCIERR_BASE + 80);
  {$EXTERNALSYM MCIERR_SEQ_DIV_INCOMPATIBLE}
  MCIERR_SEQ_PORT_INUSE               = (MCIERR_BASE + 81);
  {$EXTERNALSYM MCIERR_SEQ_PORT_INUSE}
  MCIERR_SEQ_PORT_NONEXISTENT         = (MCIERR_BASE + 82);
  {$EXTERNALSYM MCIERR_SEQ_PORT_NONEXISTENT}
  MCIERR_SEQ_PORT_MAPNODEVICE         = (MCIERR_BASE + 83);
  {$EXTERNALSYM MCIERR_SEQ_PORT_MAPNODEVICE}
  MCIERR_SEQ_PORT_MISCERROR           = (MCIERR_BASE + 84);
  {$EXTERNALSYM MCIERR_SEQ_PORT_MISCERROR}
  MCIERR_SEQ_TIMER                    = (MCIERR_BASE + 85);
  {$EXTERNALSYM MCIERR_SEQ_TIMER}
  MCIERR_SEQ_PORTUNSPECIFIED          = (MCIERR_BASE + 86);
  {$EXTERNALSYM MCIERR_SEQ_PORTUNSPECIFIED}
  MCIERR_SEQ_NOMIDIPRESENT            = (MCIERR_BASE + 87);
  {$EXTERNALSYM MCIERR_SEQ_NOMIDIPRESENT}
  MCIERR_NO_WINDOW                    = (MCIERR_BASE + 90);
  {$EXTERNALSYM MCIERR_NO_WINDOW}
  MCIERR_CREATEWINDOW                 = (MCIERR_BASE + 91);
  {$EXTERNALSYM MCIERR_CREATEWINDOW}
  MCIERR_FILE_READ                    = (MCIERR_BASE + 92);
  {$EXTERNALSYM MCIERR_FILE_READ}
  MCIERR_FILE_WRITE                   = (MCIERR_BASE + 93);
  {$EXTERNALSYM MCIERR_FILE_WRITE}
  MCIERR_NO_IDENTITY                  = (MCIERR_BASE + 94);
  {$EXTERNALSYM MCIERR_NO_IDENTITY}

  { all custom device driver errors must be >= than this value }
  MCIERR_CUSTOM_DRIVER_BASE           = (MCIERR_BASE + 256);
  {$EXTERNALSYM MCIERR_CUSTOM_DRIVER_BASE}



  { MCI command message identifiers }


  MCI_FIRST                           = DRV_MCI_FIRST;  { $0800 }
  {$EXTERNALSYM MCI_FIRST}
  MCI_OPEN                            = $0803;
  {$EXTERNALSYM MCI_OPEN}
  MCI_CLOSE                           = $0804;
  {$EXTERNALSYM MCI_CLOSE}
  MCI_ESCAPE                          = $0805;
  {$EXTERNALSYM MCI_ESCAPE}
  MCI_PLAY                            = $0806;
  {$EXTERNALSYM MCI_PLAY}
  MCI_SEEK                            = $0807;
  {$EXTERNALSYM MCI_SEEK}
  MCI_STOP                            = $0808;
  {$EXTERNALSYM MCI_STOP}
  MCI_PAUSE                           = $0809;
  {$EXTERNALSYM MCI_PAUSE}
  MCI_INFO                            = $080A;
  {$EXTERNALSYM MCI_INFO}
  MCI_GETDEVCAPS                      = $080B;
  {$EXTERNALSYM MCI_GETDEVCAPS}
  MCI_SPIN                            = $080C;
  {$EXTERNALSYM MCI_SPIN}
  MCI_SET                             = $080D;
  {$EXTERNALSYM MCI_SET}
  MCI_STEP                            = $080E;
  {$EXTERNALSYM MCI_STEP}
  MCI_RECORD                          = $080F;
  {$EXTERNALSYM MCI_RECORD}
  MCI_SYSINFO                         = $0810;
  {$EXTERNALSYM MCI_SYSINFO}
  MCI_BREAK                           = $0811;
  {$EXTERNALSYM MCI_BREAK}
  MCI_SAVE                            = $0813;
  {$EXTERNALSYM MCI_SAVE}
  MCI_STATUS                          = $0814;
  {$EXTERNALSYM MCI_STATUS}
  MCI_CUE                             = $0830;
  {$EXTERNALSYM MCI_CUE}
  MCI_REALIZE                         = $0840;
  {$EXTERNALSYM MCI_REALIZE}
  MCI_WINDOW                          = $0841;
  {$EXTERNALSYM MCI_WINDOW}
  MCI_PUT                             = $0842;
  {$EXTERNALSYM MCI_PUT}
  MCI_WHERE                           = $0843;
  {$EXTERNALSYM MCI_WHERE}
  MCI_FREEZE                          = $0844;
  {$EXTERNALSYM MCI_FREEZE}
  MCI_UNFREEZE                        = $0845;
  {$EXTERNALSYM MCI_UNFREEZE}
  MCI_LOAD                            = $0850;
  {$EXTERNALSYM MCI_LOAD}
  MCI_CUT                             = $0851;
  {$EXTERNALSYM MCI_CUT}
  MCI_COPY                            = $0852;
  {$EXTERNALSYM MCI_COPY}
  MCI_PASTE                           = $0853;
  {$EXTERNALSYM MCI_PASTE}
  MCI_UPDATE                          = $0854;
  {$EXTERNALSYM MCI_UPDATE}
  MCI_RESUME                          = $0855;
  {$EXTERNALSYM MCI_RESUME}
  MCI_DELETE                          = $0856;
  {$EXTERNALSYM MCI_DELETE}


  { all custom MCI command messages must be >= than this value }

  MCI_USER_MESSAGES                   = (DRV_MCI_FIRST + $400);
  {$EXTERNALSYM MCI_USER_MESSAGES}
  MCI_LAST                            = $0FFF;
  {$EXTERNALSYM MCI_LAST}

  { device ID for "all devices" }

  MCI_ALL_DEVICE_ID                   = MCIDEVICEID( - 1);
  {$EXTERNALSYM MCI_ALL_DEVICE_ID}


  { constants for predefined MCI device types }

  MCI_DEVTYPE_VCR                     = 513;  { (MCI_STRING_OFFSET + 1) }
  {$EXTERNALSYM MCI_DEVTYPE_VCR}
  MCI_DEVTYPE_VIDEODISC               = 514;  { (MCI_STRING_OFFSET + 2) }
  {$EXTERNALSYM MCI_DEVTYPE_VIDEODISC}
  MCI_DEVTYPE_OVERLAY                 = 515;  { (MCI_STRING_OFFSET + 3) }
  {$EXTERNALSYM MCI_DEVTYPE_OVERLAY}
  MCI_DEVTYPE_CD_AUDIO                = 516;  { (MCI_STRING_OFFSET + 4) }
  {$EXTERNALSYM MCI_DEVTYPE_CD_AUDIO}
  MCI_DEVTYPE_DAT                     = 517;  { (MCI_STRING_OFFSET + 5) }
  {$EXTERNALSYM MCI_DEVTYPE_DAT}
  MCI_DEVTYPE_SCANNER                 = 518;  { (MCI_STRING_OFFSET + 6) }
  {$EXTERNALSYM MCI_DEVTYPE_SCANNER}
  MCI_DEVTYPE_ANIMATION               = 519;  { (MCI_STRING_OFFSET + 7) }
  {$EXTERNALSYM MCI_DEVTYPE_ANIMATION}
  MCI_DEVTYPE_DIGITAL_VIDEO           = 520;  { (MCI_STRING_OFFSET + 8) }
  {$EXTERNALSYM MCI_DEVTYPE_DIGITAL_VIDEO}
  MCI_DEVTYPE_OTHER                   = 521;  { (MCI_STRING_OFFSET + 9) }
  {$EXTERNALSYM MCI_DEVTYPE_OTHER}
  MCI_DEVTYPE_WAVEFORM_AUDIO          = 522;  { (MCI_STRING_OFFSET + 10) }
  {$EXTERNALSYM MCI_DEVTYPE_WAVEFORM_AUDIO}
  MCI_DEVTYPE_SEQUENCER               = 523;  { (MCI_STRING_OFFSET + 11) }
  {$EXTERNALSYM MCI_DEVTYPE_SEQUENCER}

  MCI_DEVTYPE_FIRST                   = MCI_DEVTYPE_VCR;
  {$EXTERNALSYM MCI_DEVTYPE_FIRST}
  MCI_DEVTYPE_LAST                    = MCI_DEVTYPE_SEQUENCER;
  {$EXTERNALSYM MCI_DEVTYPE_LAST}

  MCI_DEVTYPE_FIRST_USER              = $1000;
  {$EXTERNALSYM MCI_DEVTYPE_FIRST_USER}


  { return values for 'status mode' command }

  MCI_MODE_NOT_READY                  = (MCI_STRING_OFFSET + 12);
  {$EXTERNALSYM MCI_MODE_NOT_READY}
  MCI_MODE_STOP                       = (MCI_STRING_OFFSET + 13);
  {$EXTERNALSYM MCI_MODE_STOP}
  MCI_MODE_PLAY                       = (MCI_STRING_OFFSET + 14);
  {$EXTERNALSYM MCI_MODE_PLAY}
  MCI_MODE_RECORD                     = (MCI_STRING_OFFSET + 15);
  {$EXTERNALSYM MCI_MODE_RECORD}
  MCI_MODE_SEEK                       = (MCI_STRING_OFFSET + 16);
  {$EXTERNALSYM MCI_MODE_SEEK}
  MCI_MODE_PAUSE                      = (MCI_STRING_OFFSET + 17);
  {$EXTERNALSYM MCI_MODE_PAUSE}
  MCI_MODE_OPEN                       = (MCI_STRING_OFFSET + 18);
  {$EXTERNALSYM MCI_MODE_OPEN}


  { constants used in 'set time format' and 'status time format' commands }

  MCI_FORMAT_MILLISECONDS             = 0;
  {$EXTERNALSYM MCI_FORMAT_MILLISECONDS}
  MCI_FORMAT_HMS                      = 1;
  {$EXTERNALSYM MCI_FORMAT_HMS}
  MCI_FORMAT_MSF                      = 2;
  {$EXTERNALSYM MCI_FORMAT_MSF}
  MCI_FORMAT_FRAMES                   = 3;
  {$EXTERNALSYM MCI_FORMAT_FRAMES}
  MCI_FORMAT_SMPTE_24                 = 4;
  {$EXTERNALSYM MCI_FORMAT_SMPTE_24}
  MCI_FORMAT_SMPTE_25                 = 5;
  {$EXTERNALSYM MCI_FORMAT_SMPTE_25}
  MCI_FORMAT_SMPTE_30                 = 6;
  {$EXTERNALSYM MCI_FORMAT_SMPTE_30}
  MCI_FORMAT_SMPTE_30DROP             = 7;
  {$EXTERNALSYM MCI_FORMAT_SMPTE_30DROP}
  MCI_FORMAT_BYTES                    = 8;
  {$EXTERNALSYM MCI_FORMAT_BYTES}
  MCI_FORMAT_SAMPLES                  = 9;
  {$EXTERNALSYM MCI_FORMAT_SAMPLES}
  MCI_FORMAT_TMSF                     = 10;
  {$EXTERNALSYM MCI_FORMAT_TMSF}


  { MCI time format conversion macros }

  function MCI_MSF_MINUTE(msf: DWord): Byte;
  {$EXTERNALSYM MCI_MSF_MINUTE}

  function MCI_MSF_SECOND(msf: DWord): Byte;
  {$EXTERNALSYM MCI_MSF_SECOND}

  function MCI_MSF_FRAME(msf: DWord): Byte;
  {$EXTERNALSYM MCI_MSF_FRAME}

  function MCI_MAKE_MSF(m: Byte;
                        s: Byte;
                        f: Byte): DWord;
  {$EXTERNALSYM MCI_MAKE_MSF}

  function MCI_TMSF_TRACK(tmsf: DWord): Byte;
  {$EXTERNALSYM MCI_TMSF_TRACK}

  function MCI_TMSF_MINUTE(tmsf: DWord): Byte;
  {$EXTERNALSYM MCI_TMSF_MINUTE}

  function MCI_TMSF_SECOND(tmsf: DWord): Byte;
  {$EXTERNALSYM MCI_TMSF_SECOND}

  function MCI_TMSF_FRAME(tmsf: DWord): Byte;
  {$EXTERNALSYM MCI_TMSF_FRAME}

  function MCI_MAKE_TMSF(t: Byte;
                         m: Byte;
                         s: Byte;
                         f: Byte): DWord;
  {$EXTERNALSYM MCI_MAKE_TMSF}

  function MCI_HMS_HOUR(hms: DWord): Byte;
  {$EXTERNALSYM MCI_HMS_HOUR}

  function MCI_HMS_MINUTE(hms: DWord): Byte;
  {$EXTERNALSYM MCI_HMS_MINUTE}

  function MCI_HMS_SECOND(hms: DWord): Byte;
  {$EXTERNALSYM MCI_HMS_SECOND}

  function MCI_MAKE_HMS(h: Byte;
                        m: Byte;
                        s: Byte): DWord;
  {$EXTERNALSYM MCI_MAKE_HMS}


  { flags for wParam of MM_MCINOTIFY message }

const

  {$EXTERNALSYM MCI_NOTIFY_SUCCESSFUL}
  MCI_NOTIFY_SUCCESSFUL               = $0001;
  {$EXTERNALSYM MCI_NOTIFY_SUPERSEDED}
  MCI_NOTIFY_SUPERSEDED               = $0002;
  {$EXTERNALSYM MCI_NOTIFY_ABORTED}
  MCI_NOTIFY_ABORTED                  = $0004;
  {$EXTERNALSYM MCI_NOTIFY_FAILURE}
  MCI_NOTIFY_FAILURE                  = $0008;

  { common flags for dwFlags parameter of MCI command messages }

  {$EXTERNALSYM MCI_NOTIFY}
  MCI_NOTIFY                          = $00000001;
  {$EXTERNALSYM MCI_WAIT}
  MCI_WAIT                            = $00000002;
  {$EXTERNALSYM MCI_FROM}
  MCI_FROM                            = $00000004;
  {$EXTERNALSYM MCI_TO}
  MCI_TO                              = $00000008;
  {$EXTERNALSYM MCI_TRACK}
  MCI_TRACK                           = $00000010;


  { flags for dwFlags parameter of MCI_OPEN command message }

  {$EXTERNALSYM MCI_OPEN_SHAREABLE}
  MCI_OPEN_SHAREABLE                  = $00000100;
  {$EXTERNALSYM MCI_OPEN_ELEMENT}
  MCI_OPEN_ELEMENT                    = $00000200;
  {$EXTERNALSYM MCI_OPEN_ALIAS}
  MCI_OPEN_ALIAS                      = $00000400;
  {$EXTERNALSYM MCI_OPEN_ELEMENT_ID}
  MCI_OPEN_ELEMENT_ID                 = $00000800;
  {$EXTERNALSYM MCI_OPEN_TYPE_ID}
  MCI_OPEN_TYPE_ID                    = $00001000;
  {$EXTERNALSYM MCI_OPEN_TYPE}
  MCI_OPEN_TYPE                       = $00002000;


  { flags for dwFlags parameter of MCI_SEEK command message }

  {$EXTERNALSYM MCI_SEEK_TO_START}
  MCI_SEEK_TO_START                   = $00000100;
  {$EXTERNALSYM MCI_SEEK_TO_END}
  MCI_SEEK_TO_END                     = $00000200;


  { flags for dwFlags parameter of MCI_STATUS command message }

  {$EXTERNALSYM MCI_STATUS_ITEM}
  MCI_STATUS_ITEM                     = $00000100;
  {$EXTERNALSYM MCI_STATUS_START}
  MCI_STATUS_START                    = $00000200;


  { flags for dwItem field of the MCI_STATUS_PARMS parameter block }

  {$EXTERNALSYM MCI_STATUS_LENGTH}
  MCI_STATUS_LENGTH                   = $00000001;
  {$EXTERNALSYM MCI_STATUS_POSITION}
  MCI_STATUS_POSITION                 = $00000002;
  {$EXTERNALSYM MCI_STATUS_NUMBER_OF_TRACKS}
  MCI_STATUS_NUMBER_OF_TRACKS         = $00000003;
  {$EXTERNALSYM MCI_STATUS_MODE}
  MCI_STATUS_MODE                     = $00000004;
  {$EXTERNALSYM MCI_STATUS_MEDIA_PRESENT}
  MCI_STATUS_MEDIA_PRESENT            = $00000005;
  {$EXTERNALSYM MCI_STATUS_TIME_FORMAT}
  MCI_STATUS_TIME_FORMAT              = $00000006;
  {$EXTERNALSYM MCI_STATUS_READY}
  MCI_STATUS_READY                    = $00000007;
  {$EXTERNALSYM MCI_STATUS_CURRENT_TRACK}
  MCI_STATUS_CURRENT_TRACK            = $00000008;


  { flags for dwFlags parameter of MCI_INFO command message }

  {$EXTERNALSYM MCI_INFO_PRODUCT}
  MCI_INFO_PRODUCT                    = $00000100;
  {$EXTERNALSYM MCI_INFO_FILE}
  MCI_INFO_FILE                       = $00000200;
  {$EXTERNALSYM MCI_INFO_MEDIA_UPC}
  MCI_INFO_MEDIA_UPC                  = $00000400;
  {$EXTERNALSYM MCI_INFO_MEDIA_IDENTITY}
  MCI_INFO_MEDIA_IDENTITY             = $00000800;
  {$EXTERNALSYM MCI_INFO_NAME}
  MCI_INFO_NAME                       = $00001000;
  {$EXTERNALSYM MCI_INFO_COPYRIGHT}
  MCI_INFO_COPYRIGHT                  = $00002000;


  { flags for dwFlags parameter of MCI_GETDEVCAPS command message }

  {$EXTERNALSYM MCI_GETDEVCAPS_ITEM}
  MCI_GETDEVCAPS_ITEM                 = $00000100;


  { flags for dwItem field of the MCI_GETDEVCAPS_PARMS parameter block }

  {$EXTERNALSYM MCI_GETDEVCAPS_CAN_RECORD}
  MCI_GETDEVCAPS_CAN_RECORD           = $00000001;
  {$EXTERNALSYM MCI_GETDEVCAPS_HAS_AUDIO}
  MCI_GETDEVCAPS_HAS_AUDIO            = $00000002;
  {$EXTERNALSYM MCI_GETDEVCAPS_HAS_VIDEO}
  MCI_GETDEVCAPS_HAS_VIDEO            = $00000003;
  {$EXTERNALSYM MCI_GETDEVCAPS_DEVICE_TYPE}
  MCI_GETDEVCAPS_DEVICE_TYPE          = $00000004;
  {$EXTERNALSYM MCI_GETDEVCAPS_USES_FILES}
  MCI_GETDEVCAPS_USES_FILES           = $00000005;
  {$EXTERNALSYM MCI_GETDEVCAPS_COMPOUND_DEVICE}
  MCI_GETDEVCAPS_COMPOUND_DEVICE      = $00000006;
  {$EXTERNALSYM MCI_GETDEVCAPS_CAN_EJECT}
  MCI_GETDEVCAPS_CAN_EJECT            = $00000007;
  {$EXTERNALSYM MCI_GETDEVCAPS_CAN_PLAY}
  MCI_GETDEVCAPS_CAN_PLAY             = $00000008;
  {$EXTERNALSYM MCI_GETDEVCAPS_CAN_SAVE}
  MCI_GETDEVCAPS_CAN_SAVE             = $00000009;


  { flags for dwFlags parameter of MCI_SYSINFO command message }

  {$EXTERNALSYM MCI_SYSINFO_QUANTITY}
  MCI_SYSINFO_QUANTITY                = $00000100;
  {$EXTERNALSYM MCI_SYSINFO_OPEN}
  MCI_SYSINFO_OPEN                    = $00000200;
  {$EXTERNALSYM MCI_SYSINFO_NAME}
  MCI_SYSINFO_NAME                    = $00000400;
  {$EXTERNALSYM MCI_SYSINFO_INSTALLNAME}
  MCI_SYSINFO_INSTALLNAME             = $00000800;


  { flags for dwFlags parameter of MCI_SET command message }

  {$EXTERNALSYM MCI_SET_DOOR_OPEN}
  MCI_SET_DOOR_OPEN                   = $00000100;
  {$EXTERNALSYM MCI_SET_DOOR_CLOSED}
  MCI_SET_DOOR_CLOSED                 = $00000200;
  {$EXTERNALSYM MCI_SET_TIME_FORMAT}
  MCI_SET_TIME_FORMAT                 = $00000400;
  {$EXTERNALSYM MCI_SET_AUDIO}
  MCI_SET_AUDIO                       = $00000800;
  {$EXTERNALSYM MCI_SET_VIDEO}
  MCI_SET_VIDEO                       = $00001000;
  {$EXTERNALSYM MCI_SET_ON}
  MCI_SET_ON                          = $00002000;
  {$EXTERNALSYM MCI_SET_OFF}
  MCI_SET_OFF                         = $00004000;


  { flags for dwAudio field of MCI_SET_PARMS or MCI_SEQ_SET_PARMS }

  {$EXTERNALSYM MCI_SET_AUDIO_ALL}
  MCI_SET_AUDIO_ALL                   = $00000000;
  {$EXTERNALSYM MCI_SET_AUDIO_LEFT}
  MCI_SET_AUDIO_LEFT                  = $00000001;
  {$EXTERNALSYM MCI_SET_AUDIO_RIGHT}
  MCI_SET_AUDIO_RIGHT                 = $00000002;


  { flags for dwFlags parameter of MCI_BREAK command message }

  {$EXTERNALSYM MCI_BREAK_KEY}
  MCI_BREAK_KEY                       = $00000100;
  {$EXTERNALSYM MCI_BREAK_HWND}
  MCI_BREAK_HWND                      = $00000200;
  {$EXTERNALSYM MCI_BREAK_OFF}
  MCI_BREAK_OFF                       = $00000400;


  { flags for dwFlags parameter of MCI_RECORD command message }

  {$EXTERNALSYM MCI_RECORD_INSERT}
  MCI_RECORD_INSERT                   = $00000100;
  {$EXTERNALSYM MCI_RECORD_OVERWRITE}
  MCI_RECORD_OVERWRITE                = $00000200;


  { flags for dwFlags parameter of MCI_SAVE command message }

  {$EXTERNALSYM MCI_SAVE_FILE}
  MCI_SAVE_FILE                       = $00000100;


  { flags for dwFlags parameter of MCI_LOAD command message }

  {$EXTERNALSYM MCI_LOAD_FILE}
  MCI_LOAD_FILE                       = $00000100;


 { generic parameter block for MCI command messages with no special parameters }

type

  PMCI_GENERIC_PARMS = ^tagMCI_GENERIC_PARMS;
  {$EXTERNALSYM PMCI_GENERIC_PARMS}
  tagMCI_GENERIC_PARMS = record
    dwCallback: DWORD_PTR;
  end;
  {$EXTERNALSYM tagMCI_GENERIC_PARMS}
  MCI_GENERIC_PARMS = tagMCI_GENERIC_PARMS;
  {$EXTERNALSYM MCI_GENERIC_PARMS}
  LPMCI_GENERIC_PARMS = ^tagMCI_GENERIC_PARMS;
  {$EXTERNALSYM LPMCI_GENERIC_PARMS}


  { parameter block for MCI_OPEN command message }

  PMCI_OPEN_PARMSA = ^MCI_OPEN_PARMSA;
  {$EXTERNALSYM PMCI_OPEN_PARMSA}
  tagMCI_OPEN_PARMSA = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PAnsiChar;
    lpstrElementName: PAnsiChar;
    lpstrAlias: PAnsiChar;
  end;
  {$EXTERNALSYM tagMCI_OPEN_PARMSA}
  MCI_OPEN_PARMSA = tagMCI_OPEN_PARMSA;
  {$EXTERNALSYM MCI_OPEN_PARMSA}
  LPMCI_OPEN_PARMSA = ^tagMCI_OPEN_PARMSA;
  {$EXTERNALSYM LPMCI_OPEN_PARMSA}

  PMCI_OPEN_PARMSW = ^tagMCI_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_OPEN_PARMSW}
  tagMCI_OPEN_PARMSW = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PWideChar;
    lpstrElementName: PWideChar;
    lpstrAlias: PWideChar;
  end;
  {$EXTERNALSYM tagMCI_OPEN_PARMSW}
  MCI_OPEN_PARMSW = tagMCI_OPEN_PARMSW;
  {$EXTERNALSYM MCI_OPEN_PARMSW}
  LPMCI_OPEN_PARMSW = ^tagMCI_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_OPEN_PARMSW}

  { unicode }
  MCI_OPEN_PARMS = tagMCI_OPEN_PARMSW;
  {$EXTERNALSYM MCI_OPEN_PARMS}
  PMCI_OPEN_PARMS = ^tagMCI_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_OPEN_PARMS}
  LPMCI_OPEN_PARMS = ^tagMCI_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_OPEN_PARMS}


  { parameter block for MCI_PLAY command message }

  PMCI_PLAY_PARMS = ^tagMCI_PLAY_PARMS;
  {$EXTERNALSYM PMCI_PLAY_PARMS}
  tagMCI_PLAY_PARMS = record
    dwCallback: DWORD_PTR;
    dwFrom: DWORD;
    dwTo: DWORD;
  end;
  {$EXTERNALSYM tagMCI_PLAY_PARMS}
  MCI_PLAY_PARMS = tagMCI_PLAY_PARMS;
  {$EXTERNALSYM PMCI_PLAY_PARMS}
  LPMCI_PLAY_PARMS = ^tagMCI_PLAY_PARMS;
  {$EXTERNALSYM LPMCI_PLAY_PARMS}


  { parameter block for MCI_SEEK command message }

  PMCI_SEEK_PARMS = ^tagMCI_SEEK_PARMS;
  {$EXTERNALSYM PMCI_SEEK_PARMS}
  tagMCI_SEEK_PARMS = record
    dwCallback: DWORD_PTR;
    dwTo: DWORD;
  end;
  {$EXTERNALSYM tagMCI_SEEK_PARMS}
  MCI_SEEK_PARMS = tagMCI_SEEK_PARMS;
  {$EXTERNALSYM MCI_SEEK_PARMS}
  LPMCI_SEEK_PARMS = ^tagMCI_SEEK_PARMS;
  {$EXTERNALSYM LPMCI_SEEK_PARMS}


  { parameter block for MCI_STATUS command message }

  PMCI_STATUS_PARMS = ^tagMCI_STATUS_PARMS;
  {$EXTERNALSYM PMCI_STATUS_PARMS}
  tagMCI_STATUS_PARMS = record
    dwCallback: DWORD_PTR;
    dwReturn: DWORD_PTR;
    dwItem: DWORD;
    dwTrack: DWORD;
  end;
  {$EXTERNALSYM tagMCI_STATUS_PARMS}
  MCI_STATUS_PARMS = tagMCI_STATUS_PARMS;
  {$EXTERNALSYM MCI_STATUS_PARMS}
  LPMCI_STATUS_PARMS = ^tagMCI_STATUS_PARMS;
  {$EXTERNALSYM LPMCI_STATUS_PARMS}


  { parameter block for MCI_INFO command message }

  PMCI_INFO_PARMSA = ^tagMCI_INFO_PARMSA;
  tagMCI_INFO_PARMSA = record
    dwCallback: DWORD_PTR;
    lpstrReturn: PAnsiChar;
    dwRetSize: DWORD;
  end;
  {$EXTERNALSYM tagMCI_INFO_PARMSA}
  MCI_INFO_PARMSA = tagMCI_INFO_PARMSA;
  {$EXTERNALSYM MCI_INFO_PARMSA}
  LPMCI_INFO_PARMSA = ^tagMCI_INFO_PARMSA;
  {$EXTERNALSYM LPMCI_INFO_PARMSA}

  PMCI_INFO_PARMSW = ^tagMCI_INFO_PARMSW;
  tagMCI_INFO_PARMSW = record
    dwCallback: DWORD_PTR;
    lpstrReturn: PWideChar;
    dwRetSize: DWORD;
  end;
  {$EXTERNALSYM tagMCI_INFO_PARMSW}
  MCI_INFO_PARMSW = tagMCI_INFO_PARMSW;
  {$EXTERNALSYM MCI_INFO_PARMSW}
  LPMCI_INFO_PARMSW = ^tagMCI_INFO_PARMSW;
  {$EXTERNALSYM LPMCI_INFO_PARMSW}


  { parameter block for MCI_GETDEVCAPS command message }

  PMCI_GETDEVCAPS_PARMS = ^tagMCI_GETDEVCAPS_PARMS;
  {$EXTERNALSYM PMCI_GETDEVCAPS_PARMS}
  tagMCI_GETDEVCAPS_PARMS = record
    dwCallback: DWORD_PTR;
    dwReturn: DWORD;
    dwItem: DWORD;
  end;
  {$EXTERNALSYM tagMCI_GETDEVCAPS_PARMS}
  MCI_GETDEVCAPS_PARMS = tagMCI_GETDEVCAPS_PARMS;
  {$EXTERNALSYM MCI_GETDEVCAPS_PARMS}
  LPMCI_GETDEVCAPS_PARMS = ^tagMCI_GETDEVCAPS_PARMS;
  {$EXTERNALSYM LPMCI_GETDEVCAPS_PARMS}


  { parameter block for MCI_SYSINFO command message }

  PMCI_SYSINFO_PARMSA = ^tagMCI_SYSINFO_PARMSA;
  {$EXTERNALSYM PMCI_SYSINFO_PARMSA}
  tagMCI_SYSINFO_PARMSA = record
    dwCallback: DWORD_PTR;
    lpstrReturn: PAnsiChar;
    dwRetSize: DWORD;
    dwNumber: DWORD;
    wDeviceType: UINT;
  end;
  {$EXTERNALSYM tagMCI_SYSINFO_PARMSA}
  MCI_SYSINFO_PARMSA = tagMCI_SYSINFO_PARMSA;
  {$EXTERNALSYM MCI_SYSINFO_PARMSA}
  LPMCI_SYSINFO_PARMSA = ^tagMCI_SYSINFO_PARMSA;
  {$EXTERNALSYM LPMCI_SYSINFO_PARMSA}

  PMCI_SYSINFO_PARMSW = ^tagMCI_SYSINFO_PARMSW;
  {$EXTERNALSYM PMCI_SYSINFO_PARMSW}
  tagMCI_SYSINFO_PARMSW = record
    dwCallback: DWORD_PTR;
    lpstrReturn: PWideChar;
    dwRetSize: DWORD;
    dwNumber: DWORD;
    wDeviceType: UINT;
  end;
  {$EXTERNALSYM tagMCI_SYSINFO_PARMSW}
  MCI_SYSINFO_PARMSW = tagMCI_SYSINFO_PARMSW;
  {$EXTERNALSYM MCI_SYSINFO_PARMSW}
  LPMCI_SYSINFO_PARMSW = ^tagMCI_SYSINFO_PARMSW;
  {$EXTERNALSYM LPMCI_SYSINFO_PARMSW}

  { unicode }
  MCI_SYSINFO_PARMS = tagMCI_SYSINFO_PARMSW;
  {$EXTERNALSYM MCI_SYSINFO_PARMS}
  PMCI_SYSINFO_PARMS = ^tagMCI_SYSINFO_PARMSW;
  {$EXTERNALSYM PMCI_SYSINFO_PARMS}
  LPMCI_SYSINFO_PARMS = ^tagMCI_SYSINFO_PARMSW;
  {$EXTERNALSYM LPMCI_SYSINFO_PARMS}


  { parameter block for MCI_SET command message }

  PMCI_SET_PARMS = ^tagMCI_SET_PARMS;
  {$EXTERNALSYM PMCI_SET_PARMS}
  tagMCI_SET_PARMS = record
    dwCallback: DWORD_PTR;
    dwTimeFormat: DWORD;
    dwAudio: DWORD;
  end;
  {$EXTERNALSYM tagMCI_SET_PARMS}
  MCI_SET_PARMS = tagMCI_SET_PARMS;
  {$EXTERNALSYM MCI_SET_PARMS}
  LPMCI_SET_PARMS = ^tagMCI_SET_PARMS;
  {$EXTERNALSYM LPMCI_SET_PARMS}


  { parameter block for MCI_BREAK command message }

  PMCI_BREAK_PARMS = ^tagMCI_BREAK_PARMS;
  {$EXTERNALSYM PMCI_BREAK_PARMS}
  tagMCI_BREAK_PARMS = record
    dwCallback: DWORD_PTR;
    nVirtKey: Integer;
    hwndBreak: HWND;
  end;
  {$EXTERNALSYM tagMCI_BREAK_PARMS}
  MCI_BREAK_PARMS = tagMCI_BREAK_PARMS;
  {$EXTERNALSYM MCI_BREAK_PARMS}
  LPMCI_BREAK_PARMS = ^tagMCI_BREAK_PARMS;
  {$EXTERNALSYM LPMCI_BREAK_PARMS}


  { parameter block for MCI_SAVE command message }

  PMCI_SAVE_PARMSA = ^tagMCI_SAVE_PARMSA;
  {$EXTERNALSYM PMCI_SAVE_PARMSA}
  tagMCI_SAVE_PARMSA = record
    dwCallback: DWORD_PTR;
    lpfilename: PAnsiChar;
  end;
  {$EXTERNALSYM tagMCI_SAVE_PARMSA}
  MCI_SAVE_PARMSA = tagMCI_SAVE_PARMSA;
  {$EXTERNALSYM MCI_SAVE_PARMSA}
  LPMCI_SAVE_PARMSA = ^tagMCI_SAVE_PARMSA;
  {$EXTERNALSYM LPMCI_SAVE_PARMSA}

  PMCI_SAVE_PARMSW = ^tagMCI_SAVE_PARMSW;
  {$EXTERNALSYM PMCI_SAVE_PARMSW}
  tagMCI_SAVE_PARMSW = record
    dwCallback: DWORD_PTR;
    lpfilename: PWideChar;
  end;
  {$EXTERNALSYM tagMCI_SAVE_PARMSW}
  MCI_SAVE_PARMSW = tagMCI_SAVE_PARMSW;
  {$EXTERNALSYM MCI_SAVE_PARMSW}
  LPMCI_SAVE_PARMSW = ^tagMCI_SAVE_PARMSW;
  {$EXTERNALSYM LPMCI_SAVE_PARMSW}

  { unicode }
  MCI_SAVE_PARMS = tagMCI_SAVE_PARMSW;
  {$EXTERNALSYM MCI_SAVE_PARMS}
  PMCI_SAVE_PARMS = ^tagMCI_SAVE_PARMSW;
  {$EXTERNALSYM PMCI_SAVE_PARMS}
  LPMCI_SAVE_PARMS = ^tagMCI_SAVE_PARMSW;
  {$EXTERNALSYM LPMCI_SAVE_PARMS}


  { parameter block for MCI_LOAD command message }

  PMCI_LOAD_PARMSA = ^tagMCI_LOAD_PARMSA;
  {$EXTERNALSYM PMCI_LOAD_PARMSA}
  tagMCI_LOAD_PARMSA = record
    dwCallback: DWORD_PTR;
    lpfilename: PAnsiChar;
  end;
  {$EXTERNALSYM tagMCI_LOAD_PARMSA}
  MCI_LOAD_PARMSA = tagMCI_LOAD_PARMSA;
  {$EXTERNALSYM MCI_LOAD_PARMSA}
  LPMCI_LOAD_PARMSA = ^tagMCI_LOAD_PARMSA;
  {$EXTERNALSYM LPMCI_LOAD_PARMSA}

  PMCI_LOAD_PARMSW = ^tagMCI_LOAD_PARMSW;
  {$EXTERNALSYM PMCI_LOAD_PARMSW}
  tagMCI_LOAD_PARMSW = record
    dwCallback: DWORD_PTR;
    lpfilename: PWideChar;
  end;
  {$EXTERNALSYM tagMCI_LOAD_PARMSW}
  MCI_LOAD_PARMSW = tagMCI_LOAD_PARMSW;
  {$EXTERNALSYM MCI_LOAD_PARMSW}
  LPMCI_LOAD_PARMSW = ^tagMCI_LOAD_PARMSW;
  {$EXTERNALSYM LPMCI_LOAD_PARMSW}

  { unicode }
  MCI_LOAD_PARMS = tagMCI_LOAD_PARMSW;
  {$EXTERNALSYM MCI_LOAD_PARMS}
  PMCI_LOAD_PARMS = ^tagMCI_LOAD_PARMSW;
  {$EXTERNALSYM PMCI_LOAD_PARMS}
  LPMCI_LOAD_PARMS = ^tagMCI_LOAD_PARMSW;
  {$EXTERNALSYM LPMCI_LOAD_PARMS}


  { parameter block for MCI_RECORD command message }

  PMCI_RECORD_PARMS = ^tagMCI_RECORD_PARMS;
  tagMCI_RECORD_PARMS = record
    dwCallback: DWORD_PTR;
    dwFrom: DWORD;
    dwTo: DWORD;
  end;
  {$EXTERNALSYM tagMCI_RECORD_PARMS}
  MCI_RECORD_PARMS = tagMCI_RECORD_PARMS;
  {$EXTERNALSYM MCI_RECORD_PARMS}
  LPMCI_RECORD_PARMS = ^tagMCI_RECORD_PARMS;
  {$EXTERNALSYM LPMCI_RECORD_PARMS}


  { MCI extensions for videodisc devices }

const

  { flag for dwReturn field of MCI_STATUS_PARMS }
  { MCI_STATUS command, (dwItem == MCI_STATUS_MODE) }

  MCI_VD_MODE_PARK                    = (MCI_VD_OFFSET + 1);
  {$EXTERNALSYM MCI_VD_MODE_PARK}


  { flag for dwReturn field of MCI_STATUS_PARMS }
  { MCI_STATUS command, (dwItem == MCI_VD_STATUS_MEDIA_TYPE) }

  {$EXTERNALSYM MCI_VD_MEDIA_CLV}
  MCI_VD_MEDIA_CLV                    = (MCI_VD_OFFSET + 2);
  {$EXTERNALSYM MCI_VD_MEDIA_CAV}
  MCI_VD_MEDIA_CAV                    = (MCI_VD_OFFSET + 3);
  {$EXTERNALSYM MCI_VD_MEDIA_OTHER}
  MCI_VD_MEDIA_OTHER                  = (MCI_VD_OFFSET + 4);

  MCI_VD_FORMAT_TRACK                 = $4001;
  {$EXTERNALSYM MCI_VD_FORMAT_TRACK}

  { flags for dwFlags parameter of MCI_PLAY command message }

  MCI_VD_PLAY_REVERSE                 = $00010000;
  {$EXTERNALSYM MCI_VD_PLAY_REVERSE}
  MCI_VD_PLAY_FAST                    = $00020000;
  {$EXTERNALSYM MCI_VD_PLAY_FAST}
  MCI_VD_PLAY_SPEED                   = $00040000;
  {$EXTERNALSYM MCI_VD_PLAY_SPEED}
  MCI_VD_PLAY_SCAN                    = $00080000;
  {$EXTERNALSYM MCI_VD_PLAY_SCAN}
  MCI_VD_PLAY_SLOW                    = $00100000;
  {$EXTERNALSYM MCI_VD_PLAY_SLOW}

  { flag for dwFlags parameter of MCI_SEEK command message }

  MCI_VD_SEEK_REVERSE                 = $00010000;
  {$EXTERNALSYM MCI_VD_SEEK_REVERSE}

  { flags for dwItem field of MCI_STATUS_PARMS parameter block }

  MCI_VD_STATUS_SPEED                 = $00004002;
  {$EXTERNALSYM MCI_VD_STATUS_SPEED}
  MCI_VD_STATUS_FORWARD               = $00004003;
  {$EXTERNALSYM MCI_VD_STATUS_FORWARD}
  MCI_VD_STATUS_MEDIA_TYPE            = $00004004;
  {$EXTERNALSYM MCI_VD_STATUS_MEDIA_TYPE}
  MCI_VD_STATUS_SIDE                  = $00004005;
  {$EXTERNALSYM MCI_VD_STATUS_SIDE}
  MCI_VD_STATUS_DISC_SIZE             = $00004006;
  {$EXTERNALSYM MCI_VD_STATUS_DISC_SIZE}

  { flags for dwFlags parameter of MCI_GETDEVCAPS command message }

  MCI_VD_GETDEVCAPS_CLV               = $00010000;
  {$EXTERNALSYM MCI_VD_GETDEVCAPS_CLV}
  MCI_VD_GETDEVCAPS_CAV               = $00020000;
  {$EXTERNALSYM MCI_VD_GETDEVCAPS_CAV}

  MCI_VD_SPIN_UP                      = $00010000;
  {$EXTERNALSYM MCI_VD_SPIN_UP}
  MCI_VD_SPIN_DOWN                    = $00020000;
  {$EXTERNALSYM MCI_VD_SPIN_DOWN}

  { flags for dwItem field of MCI_GETDEVCAPS_PARMS parameter block }

  MCI_VD_GETDEVCAPS_CAN_REVERSE       = $00004002;
  {$EXTERNALSYM MCI_VD_GETDEVCAPS_CAN_REVERSE}
  MCI_VD_GETDEVCAPS_FAST_RATE         = $00004003;
  {$EXTERNALSYM MCI_VD_GETDEVCAPS_FAST_RATE}
  MCI_VD_GETDEVCAPS_SLOW_RATE         = $00004004;
  {$EXTERNALSYM MCI_VD_GETDEVCAPS_SLOW_RATE}
  MCI_VD_GETDEVCAPS_NORMAL_RATE       = $00004005;
  {$EXTERNALSYM MCI_VD_GETDEVCAPS_NORMAL_RATE}

  { flags for the dwFlags parameter of MCI_STEP command message }

  MCI_VD_STEP_FRAMES                  = $00010000;
  {$EXTERNALSYM MCI_VD_STEP_FRAMES}
  MCI_VD_STEP_REVERSE                 = $00020000;
  {$EXTERNALSYM MCI_VD_STEP_REVERSE}

  { flag for the MCI_ESCAPE command message }
  MCI_VD_ESCAPE_STRING                = $00000100;
  {$EXTERNALSYM MCI_VD_ESCAPE_STRING}

type

  { parameter block for MCI_PLAY command message }

  PMCI_VD_PLAY_PARMS = ^tagMCI_VD_PLAY_PARMS;
  {$EXTERNALSYM PMCI_VD_PLAY_PARMS}
  tagMCI_VD_PLAY_PARMS = record
    dwCallback: DWORD_PTR;
    dwFrom: DWORD;
    dwTo: DWORD;
    dwSpeed: DWORD;
  end;
  {$EXTERNALSYM tagMCI_VD_PLAY_PARMS}
  MCI_VD_PLAY_PARMS = tagMCI_VD_PLAY_PARMS;
  {$EXTERNALSYM MCI_VD_PLAY_PARMS}
  LPMCI_VD_PLAY_PARMS = ^tagMCI_VD_PLAY_PARMS;
  {$EXTERNALSYM LPMCI_VD_PLAY_PARMS}


  { parameter block for MCI_STEP command message }

  PMCI_VD_STEP_PARMS = ^tagMCI_VD_STEP_PARMS;
  {$EXTERNALSYM PMCI_VD_STEP_PARMS}
  tagMCI_VD_STEP_PARMS = record
    dwCallback: DWORD_PTR;
    dwFrames: DWORD;
  end;
  {$EXTERNALSYM tagMCI_VD_STEP_PARMS}
  MCI_VD_STEP_PARMS = tagMCI_VD_STEP_PARMS;
  {$EXTERNALSYM MCI_VD_STEP_PARMS}
  LPMCI_VD_STEP_PARMS = ^tagMCI_VD_STEP_PARMS;
  {$EXTERNALSYM LPMCI_VD_STEP_PARMS}


  { parameter block for MCI_ESCAPE command message }

  PMCI_VD_ESCAPE_PARMSA = ^tagMCI_VD_ESCAPE_PARMSA;
  {$EXTERNALSYM PMCI_VD_ESCAPE_PARMSA}
  tagMCI_VD_ESCAPE_PARMSA = record
    dwCallback: DWORD_PTR;
    lpstrCommand: PAnsiChar;
  end;
  {$EXTERNALSYM tagMCI_VD_ESCAPE_PARMSA}
  MCI_VD_ESCAPE_PARMSA = tagMCI_VD_ESCAPE_PARMSA;
  {$EXTERNALSYM MCI_VD_ESCAPE_PARMSA}
  LPMCI_VD_ESCAPE_PARMSA = ^tagMCI_VD_ESCAPE_PARMSA;
  {$EXTERNALSYM LPMCI_VD_ESCAPE_PARMSA}

  PMCI_VD_ESCAPE_PARMSW = ^tagMCI_VD_ESCAPE_PARMSW;
  {$EXTERNALSYM PMCI_VD_ESCAPE_PARMSW}
  tagMCI_VD_ESCAPE_PARMSW = record
    dwCallback: DWORD_PTR;
    lpstrCommand: PWideChar;
  end;
  {$EXTERNALSYM tagMCI_VD_ESCAPE_PARMSW}
  MCI_VD_ESCAPE_PARMSW = tagMCI_VD_ESCAPE_PARMSW;
  {$EXTERNALSYM MCI_VD_ESCAPE_PARMSW}
  LPMCI_VD_ESCAPE_PARMSW = ^tagMCI_VD_ESCAPE_PARMSW;
  {$EXTERNALSYM LPMCI_VD_ESCAPE_PARMSW}

  { unicode }
  MCI_VD_ESCAPE_PARMS = tagMCI_VD_ESCAPE_PARMSW;
  {$EXTERNALSYM MCI_VD_ESCAPE_PARMS}
  PMCI_VD_ESCAPE_PARMS = ^tagMCI_VD_ESCAPE_PARMSW;
  {$EXTERNALSYM PMCI_VD_ESCAPE_PARMS}
  LPMCI_VD_ESCAPE_PARMS = ^tagMCI_VD_ESCAPE_PARMSW;
  {$EXTERNALSYM LPMCI_VD_ESCAPE_PARMS}

  { MCI extensions for CD audio devices }

  { flags for the dwItem field of the MCI_STATUS_PARMS parameter block }

const

  MCI_CDA_STATUS_TYPE_TRACK           = $00004001;
  {$EXTERNALSYM MCI_CDA_STATUS_TYPE_TRACK}

  { flags for the dwReturn field of MCI_STATUS_PARMS parameter block }
  { MCI_STATUS command, (dwItem == MCI_CDA_STATUS_TYPE_TRACK) }

  MCI_CDA_TRACK_AUDIO                 = (MCI_CD_OFFSET + 0);
  {$EXTERNALSYM MCI_CDA_TRACK_AUDIO}
  MCI_CDA_TRACK_OTHER                 = (MCI_CD_OFFSET + 1);
  {$EXTERNALSYM MCI_CDA_TRACK_OTHER}

  { MCI extensions for waveform audio devices }

  MCI_WAVE_PCM                        = (MCI_WAVE_OFFSET + 0);
  {$EXTERNALSYM MCI_WAVE_PCM}
  MCI_WAVE_MAPPER                     = (MCI_WAVE_OFFSET + 1);
  {$EXTERNALSYM MCI_WAVE_MAPPER}

  { flags for the dwFlags parameter of MCI_OPEN command message }

  MCI_WAVE_OPEN_BUFFER                = $00010000;
  {$EXTERNALSYM MCI_WAVE_OPEN_BUFFER}

  { flags for the dwFlags parameter of MCI_SET command message }

  MCI_WAVE_SET_FORMATTAG              = $00010000;
  {$EXTERNALSYM MCI_WAVE_SET_FORMATTAG}
  MCI_WAVE_SET_CHANNELS               = $00020000;
  {$EXTERNALSYM MCI_WAVE_SET_CHANNELS}
  MCI_WAVE_SET_SAMPLESPERSEC          = $00040000;
  {$EXTERNALSYM MCI_WAVE_SET_SAMPLESPERSEC}
  MCI_WAVE_SET_AVGBYTESPERSEC         = $00080000;
  {$EXTERNALSYM MCI_WAVE_SET_AVGBYTESPERSEC}
  MCI_WAVE_SET_BLOCKALIGN             = $00100000;
  {$EXTERNALSYM MCI_WAVE_SET_BLOCKALIGN}
  MCI_WAVE_SET_BITSPERSAMPLE          = $00200000;
  {$EXTERNALSYM MCI_WAVE_SET_BITSPERSAMPLE}

  { flags for the dwFlags parameter of MCI_STATUS, MCI_SET command messages }

  MCI_WAVE_INPUT                      = $00400000;
  {$EXTERNALSYM MCI_WAVE_INPUT}
  MCI_WAVE_OUTPUT                     = $00800000;
  {$EXTERNALSYM MCI_WAVE_OUTPUT}

  { flags for the dwItem field of MCI_STATUS_PARMS parameter block }

  MCI_WAVE_STATUS_FORMATTAG           = $00004001;
  {$EXTERNALSYM MCI_WAVE_STATUS_FORMATTAG}
  MCI_WAVE_STATUS_CHANNELS            = $00004002;
  {$EXTERNALSYM MCI_WAVE_STATUS_CHANNELS}
  MCI_WAVE_STATUS_SAMPLESPERSEC       = $00004003;
  {$EXTERNALSYM MCI_WAVE_STATUS_SAMPLESPERSEC}
  MCI_WAVE_STATUS_AVGBYTESPERSEC      = $00004004;
  {$EXTERNALSYM MCI_WAVE_STATUS_AVGBYTESPERSEC}
  MCI_WAVE_STATUS_BLOCKALIGN          = $00004005;
  {$EXTERNALSYM MCI_WAVE_STATUS_BLOCKALIGN}
  MCI_WAVE_STATUS_BITSPERSAMPLE       = $00004006;
  {$EXTERNALSYM MCI_WAVE_STATUS_BITSPERSAMPLE}
  MCI_WAVE_STATUS_LEVEL               = $00004007;
  {$EXTERNALSYM MCI_WAVE_STATUS_LEVEL}

  { flags for the dwFlags parameter of MCI_SET command message }

  MCI_WAVE_SET_ANYINPUT               = $04000000;
  {$EXTERNALSYM MCI_WAVE_SET_ANYINPUT}
  MCI_WAVE_SET_ANYOUTPUT              = $08000000;
  {$EXTERNALSYM MCI_WAVE_SET_ANYOUTPUT}

  { flags for the dwFlags parameter of MCI_GETDEVCAPS command message }

  MCI_WAVE_GETDEVCAPS_INPUTS          = $00004001;
  {$EXTERNALSYM MCI_WAVE_GETDEVCAPS_INPUTS}
  MCI_WAVE_GETDEVCAPS_OUTPUTS         = $00004002;
  {$EXTERNALSYM MCI_WAVE_GETDEVCAPS_OUTPUTS}


  { parameter block for MCI_OPEN command message }

type

  PMCI_WAVE_OPEN_PARMSA = ^tagMCI_WAVE_OPEN_PARMSA;
  {$EXTERNALSYM PMCI_WAVE_OPEN_PARMSA}
  tagMCI_WAVE_OPEN_PARMSA = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PAnsiChar;
    lpstrElementName: PAnsiChar;
    lpstrAlias: PAnsiChar;
    dwBufferSeconds: DWORD;
  end;
  {$EXTERNALSYM tagMCI_WAVE_OPEN_PARMSA}
  MCI_WAVE_OPEN_PARMSA = tagMCI_WAVE_OPEN_PARMSA;
  {$EXTERNALSYM MCI_WAVE_OPEN_PARMSA}
  LPMCI_WAVE_OPEN_PARMSA = ^tagMCI_WAVE_OPEN_PARMSA;
  {$EXTERNALSYM LPMCI_WAVE_OPEN_PARMSA}

  PMCI_WAVE_OPEN_PARMSW = ^tagMCI_WAVE_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_WAVE_OPEN_PARMSW}
  tagMCI_WAVE_OPEN_PARMSW = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PWideChar;
    lpstrElementName: PWideChar;
    lpstrAlias: PWideChar;
    dwBufferSeconds: DWORD;
  end;
  {$EXTERNALSYM tagMCI_WAVE_OPEN_PARMSW}
  MCI_WAVE_OPEN_PARMSW = tagMCI_WAVE_OPEN_PARMSW;
  {$EXTERNALSYM MCI_WAVE_OPEN_PARMSW}
  LPMCI_WAVE_OPEN_PARMSW = ^tagMCI_WAVE_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_WAVE_OPEN_PARMSW}

  { unicode }
  MCI_WAVE_OPEN_PARMS = tagMCI_WAVE_OPEN_PARMSW;
  {$EXTERNALSYM MCI_WAVE_OPEN_PARMS}
  PMCI_WAVE_OPEN_PARMS = ^tagMCI_WAVE_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_WAVE_OPEN_PARMS}
  LPMCI_WAVE_OPEN_PARMS = ^tagMCI_WAVE_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_WAVE_OPEN_PARMS}

  { parameter block for MCI_DELETE command message }

  PMCI_WAVE_DELETE_PARMS = ^tagMCI_WAVE_DELETE_PARMS;
  {$EXTERNALSYM PMCI_WAVE_DELETE_PARMS}
  tagMCI_WAVE_DELETE_PARMS = record
    dwCallback: DWORD_PTR;
    dwFrom: DWORD;
    dwTo: DWORD;
  end;
  {$EXTERNALSYM tagMCI_WAVE_DELETE_PARMS}
  MCI_WAVE_DELETE_PARMS = tagMCI_WAVE_DELETE_PARMS;
  {$EXTERNALSYM MCI_WAVE_DELETE_PARMS}
  LPMCI_WAVE_DELETE_PARMS = ^tagMCI_WAVE_DELETE_PARMS;
  {$EXTERNALSYM LPMCI_WAVE_DELETE_PARMS}


  { parameter block for MCI_SET command message }

type

  PMCI_WAVE_SET_PARMS = ^tagMCI_WAVE_SET_PARMS;
  {$EXTERNALSYM PMCI_WAVE_SET_PARMS}
  tagMCI_WAVE_SET_PARMS = record
    dwCallback: DWORD_PTR;
    dwTimeFormat: DWORD;
    dwAudio: DWORD;
    wInput: UINT;
    wOutput: UINT;
    wFormatTag: WORD;
    wReserved2: WORD;
    nChannels: WORD;
    wReserved3: WORD;
    nSamplesPerSec: DWORD;
    nAvgBytesPerSec: DWORD;
    nBlockAlign: WORD;
    wReserved4: WORD;
    wBitsPerSample: WORD;
    wReserved5: WORD;
  end;
  {$EXTERNALSYM tagMCI_WAVE_SET_PARMS}
  MCI_WAVE_SET_PARMS = tagMCI_WAVE_SET_PARMS;
  {$EXTERNALSYM MCI_WAVE_SET_PARMS}
  LPMCI_WAVE_SET_PARMS = ^tagMCI_WAVE_SET_PARMS;
  {$EXTERNALSYM LPMCI_WAVE_SET_PARMS}

  { MCI extensions for MIDI sequencer devices }

const

  { flags for the dwReturn field of MCI_STATUS_PARMS parameter block }
  { MCI_STATUS command, (dwItem == MCI_SEQ_STATUS_DIVTYPE) }

  MCI_SEQ_DIV_PPQN                    = (0 + MCI_SEQ_OFFSET);
  {$EXTERNALSYM MCI_SEQ_DIV_PPQN}
  MCI_SEQ_DIV_SMPTE_24                = (1 + MCI_SEQ_OFFSET);
  {$EXTERNALSYM MCI_SEQ_DIV_SMPTE_24}
  MCI_SEQ_DIV_SMPTE_25                = (2 + MCI_SEQ_OFFSET);
  {$EXTERNALSYM MCI_SEQ_DIV_SMPTE_25}
  MCI_SEQ_DIV_SMPTE_30DROP            = (3 + MCI_SEQ_OFFSET);
  {$EXTERNALSYM MCI_SEQ_DIV_SMPTE_30DROP}
  MCI_SEQ_DIV_SMPTE_30                = (4 + MCI_SEQ_OFFSET);
  {$EXTERNALSYM MCI_SEQ_DIV_SMPTE_30}

  { flags for the dwMaster field of MCI_SEQ_SET_PARMS parameter block }
  { MCI_SET command, (dwFlags == MCI_SEQ_SET_MASTER) }

  MCI_SEQ_FORMAT_SONGPTR              = $4001;
  {$EXTERNALSYM MCI_SEQ_FORMAT_SONGPTR}
  MCI_SEQ_FILE                        = $4002;
  {$EXTERNALSYM MCI_SEQ_FILE}
  MCI_SEQ_MIDI                        = $4003;
  {$EXTERNALSYM MCI_SEQ_MIDI}
  MCI_SEQ_SMPTE                       = $4004;
  {$EXTERNALSYM MCI_SEQ_SMPTE}
  MCI_SEQ_NONE                        = 65533;
  {$EXTERNALSYM MCI_SEQ_NONE}
  MCI_SEQ_MAPPER                      = 65535;
  {$EXTERNALSYM MCI_SEQ_MAPPER}

  { flags for the dwItem field of MCI_STATUS_PARMS parameter block }

  MCI_SEQ_STATUS_TEMPO                = $00004002;
  {$EXTERNALSYM MCI_SEQ_STATUS_TEMPO}
  MCI_SEQ_STATUS_PORT                 = $00004003;
  {$EXTERNALSYM MCI_SEQ_STATUS_PORT}
  MCI_SEQ_STATUS_SLAVE                = $00004007;
  {$EXTERNALSYM MCI_SEQ_STATUS_SLAVE}
  MCI_SEQ_STATUS_MASTER               = $00004008;
  {$EXTERNALSYM MCI_SEQ_STATUS_MASTER}
  MCI_SEQ_STATUS_OFFSET               = $00004009;
  {$EXTERNALSYM MCI_SEQ_STATUS_OFFSET}
  MCI_SEQ_STATUS_DIVTYPE              = $0000400A;
  {$EXTERNALSYM MCI_SEQ_STATUS_DIVTYPE}
  MCI_SEQ_STATUS_NAME                 = $0000400B;
  {$EXTERNALSYM MCI_SEQ_STATUS_NAME}
  MCI_SEQ_STATUS_COPYRIGHT            = $0000400C;
  {$EXTERNALSYM MCI_SEQ_STATUS_COPYRIGHT}

  { flags for the dwFlags parameter of MCI_SET command message }

  MCI_SEQ_SET_TEMPO                   = $00010000;
  {$EXTERNALSYM MCI_SEQ_SET_TEMPO}
  MCI_SEQ_SET_PORT                    = $00020000;
  {$EXTERNALSYM MCI_SEQ_SET_PORT}
  MCI_SEQ_SET_SLAVE                   = $00040000;
  {$EXTERNALSYM MCI_SEQ_SET_SLAVE}
  MCI_SEQ_SET_MASTER                  = $00080000;
  {$EXTERNALSYM MCI_SEQ_SET_MASTER}
  MCI_SEQ_SET_OFFSET                  = $01000000;
  {$EXTERNALSYM MCI_SEQ_SET_OFFSET}


  { parameter block for MCI_SET command message }

type

  PMCI_SEQ_SET_PARMS = ^tagMCI_SEQ_SET_PARMS;
  {$EXTERNALSYM PMCI_SEQ_SET_PARMS}
  tagMCI_SEQ_SET_PARMS = record
    dwCallback: DWORD_PTR;
    dwTimeFormat: DWORD;
    dwAudio: DWORD;
    dwTempo: DWORD;
    dwPort: DWORD;
    dwSlave: DWORD;
    dwMaster: DWORD;
    dwOffset: DWORD;
  end;
  {$EXTERNALSYM tagMCI_SEQ_SET_PARMS}
  MCI_SEQ_SET_PARMS = tagMCI_SEQ_SET_PARMS;
  {$EXTERNALSYM MCI_SEQ_SET_PARMS}
  LPMCI_SEQ_SET_PARMS = ^tagMCI_SEQ_SET_PARMS;
  {$EXTERNALSYM LPMCI_SEQ_SET_PARMS}

  { MCI extensions for animation devices }

  { flags for dwFlags parameter of MCI_OPEN command message }

const

  MCI_ANIM_OPEN_WS                    = $00010000;
  {$EXTERNALSYM MCI_ANIM_OPEN_WS}
  MCI_ANIM_OPEN_PARENT                = $00020000;
  {$EXTERNALSYM MCI_ANIM_OPEN_PARENT}
  MCI_ANIM_OPEN_NOSTATIC              = $00040000;
  {$EXTERNALSYM MCI_ANIM_OPEN_NOSTATIC}

  { flags for dwFlags parameter of MCI_PLAY command message }

  MCI_ANIM_PLAY_SPEED                 = $00010000;
  {$EXTERNALSYM MCI_ANIM_PLAY_SPEED}
  MCI_ANIM_PLAY_REVERSE               = $00020000;
  {$EXTERNALSYM MCI_ANIM_PLAY_REVERSE}
  MCI_ANIM_PLAY_FAST                  = $00040000;
  {$EXTERNALSYM MCI_ANIM_PLAY_FAST}
  MCI_ANIM_PLAY_SLOW                  = $00080000;
  {$EXTERNALSYM MCI_ANIM_PLAY_SLOW}
  MCI_ANIM_PLAY_SCAN                  = $00100000;
  {$EXTERNALSYM MCI_ANIM_PLAY_SCAN}

  { flags for dwFlags parameter of MCI_STEP command message }

  MCI_ANIM_STEP_REVERSE               = $00010000;
  {$EXTERNALSYM MCI_ANIM_STEP_REVERSE}
  MCI_ANIM_STEP_FRAMES                = $00020000;
  {$EXTERNALSYM MCI_ANIM_STEP_FRAMES}

  { flags for dwItem field of MCI_STATUS_PARMS parameter block }

  MCI_ANIM_STATUS_SPEED               = $00004001;
  {$EXTERNALSYM MCI_ANIM_STATUS_SPEED}
  MCI_ANIM_STATUS_FORWARD             = $00004002;
  {$EXTERNALSYM MCI_ANIM_STATUS_FORWARD}
  MCI_ANIM_STATUS_HWND                = $00004003;
  {$EXTERNALSYM MCI_ANIM_STATUS_HWND}
  MCI_ANIM_STATUS_HPAL                = $00004004;
  {$EXTERNALSYM MCI_ANIM_STATUS_HPAL}
  MCI_ANIM_STATUS_STRETCH             = $00004005;
  {$EXTERNALSYM MCI_ANIM_STATUS_STRETCH}

  { flags for the dwFlags parameter of MCI_INFO command message }

  MCI_ANIM_INFO_TEXT                  = $00010000;
  {$EXTERNALSYM MCI_ANIM_INFO_TEXT}

  { flags for dwItem field of MCI_GETDEVCAPS_PARMS parameter block }

  MCI_ANIM_GETDEVCAPS_CAN_REVERSE     = $00004001;
  {$EXTERNALSYM MCI_ANIM_GETDEVCAPS_CAN_REVERSE}
  MCI_ANIM_GETDEVCAPS_FAST_RATE       = $00004002;
  {$EXTERNALSYM MCI_ANIM_GETDEVCAPS_FAST_RATE}
  MCI_ANIM_GETDEVCAPS_SLOW_RATE       = $00004003;
  {$EXTERNALSYM MCI_ANIM_GETDEVCAPS_SLOW_RATE}
  MCI_ANIM_GETDEVCAPS_NORMAL_RATE     = $00004004;
  {$EXTERNALSYM MCI_ANIM_GETDEVCAPS_NORMAL_RATE}
  MCI_ANIM_GETDEVCAPS_PALETTES        = $00004006;
  {$EXTERNALSYM MCI_ANIM_GETDEVCAPS_PALETTES}
  MCI_ANIM_GETDEVCAPS_CAN_STRETCH     = $00004007;
  {$EXTERNALSYM MCI_ANIM_GETDEVCAPS_CAN_STRETCH}
  MCI_ANIM_GETDEVCAPS_MAX_WINDOWS     = $00004008;
  {$EXTERNALSYM MCI_ANIM_GETDEVCAPS_MAX_WINDOWS}

  { flags for the MCI_REALIZE command message }

  MCI_ANIM_REALIZE_NORM               = $00010000;
  {$EXTERNALSYM MCI_ANIM_REALIZE_NORM}
  MCI_ANIM_REALIZE_BKGD               = $00020000;
  {$EXTERNALSYM MCI_ANIM_REALIZE_BKGD}

  { flags for dwFlags parameter of MCI_WINDOW command message }

  MCI_ANIM_WINDOW_HWND                = $00010000;
  {$EXTERNALSYM MCI_ANIM_WINDOW_HWND}
  MCI_ANIM_WINDOW_STATE               = $00040000;
  {$EXTERNALSYM MCI_ANIM_WINDOW_STATE}
  MCI_ANIM_WINDOW_TEXT                = $00080000;
  {$EXTERNALSYM MCI_ANIM_WINDOW_TEXT}
  MCI_ANIM_WINDOW_ENABLE_STRETCH      = $00100000;
  {$EXTERNALSYM MCI_ANIM_WINDOW_ENABLE_STRETCH}
  MCI_ANIM_WINDOW_DISABLE_STRETCH     = $00200000;
  {$EXTERNALSYM MCI_ANIM_WINDOW_DISABLE_STRETCH}

  { flags for hWnd field of MCI_ANIM_WINDOW_PARMS parameter block }
  { MCI_WINDOW command message, (dwFlags = MCI_ANIM_WINDOW_HWND) }

  MCI_ANIM_WINDOW_DEFAULT             = $00000000;
  {$EXTERNALSYM MCI_ANIM_WINDOW_DEFAULT}

  { flags for dwFlags parameter of MCI_PUT command message }

  MCI_ANIM_RECT                       = $00010000;
  {$EXTERNALSYM MCI_ANIM_RECT}
  MCI_ANIM_PUT_SOURCE                 = $00020000;
  {$EXTERNALSYM MCI_ANIM_PUT_SOURCE}
  MCI_ANIM_PUT_DESTINATION            = $00040000;
  {$EXTERNALSYM MCI_ANIM_PUT_DESTINATION}

  { flags for dwFlags parameter of MCI_WHERE command message }

  MCI_ANIM_WHERE_SOURCE               = $00020000;
  {$EXTERNALSYM MCI_ANIM_WHERE_SOURCE}
  MCI_ANIM_WHERE_DESTINATION          = $00040000;
  {$EXTERNALSYM MCI_ANIM_WHERE_DESTINATION}

  { flags for dwFlags parameter of MCI_UPDATE command message }

  MCI_ANIM_UPDATE_HDC                 = $00020000;
  {$EXTERNALSYM MCI_ANIM_UPDATE_HDC}


  { parameter block for MCI_OPEN command message }

type

  PMCI_ANIM_OPEN_PARMSA = ^tagMCI_ANIM_OPEN_PARMSA;
  {$EXTERNALSYM PMCI_ANIM_OPEN_PARMSA}
  tagMCI_ANIM_OPEN_PARMSA = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PAnsiChar;
    lpstrElementName: PAnsiChar;
    lpstrAlias: PAnsiChar;
    dwStyle: DWORD;
    hWndParent: HWND;
  end;
  {$EXTERNALSYM tagMCI_ANIM_OPEN_PARMSA}
  MCI_ANIM_OPEN_PARMSA = tagMCI_ANIM_OPEN_PARMSA;
  {$EXTERNALSYM MCI_ANIM_OPEN_PARMSA}
  LPMCI_ANIM_OPEN_PARMSA = ^tagMCI_ANIM_OPEN_PARMSA;
  {$EXTERNALSYM LPMCI_ANIM_OPEN_PARMSA}

  PMCI_ANIM_OPEN_PARMSW = ^tagMCI_ANIM_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_ANIM_OPEN_PARMSW}
  tagMCI_ANIM_OPEN_PARMSW = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PWideChar;
    lpstrElementName: PWideChar;
    lpstrAlias: PWideChar;
    dwStyle: DWORD;
    hWndParent: HWND;
  end;
  {$EXTERNALSYM tagMCI_ANIM_OPEN_PARMSW}
  MCI_ANIM_OPEN_PARMSW = tagMCI_ANIM_OPEN_PARMSW;
  {$EXTERNALSYM MCI_ANIM_OPEN_PARMSW}
  LPMCI_ANIM_OPEN_PARMSW = ^tagMCI_ANIM_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_ANIM_OPEN_PARMSW}

  { unicode }
  MCI_ANIM_OPEN_PARMS = tagMCI_ANIM_OPEN_PARMSW;
  {$EXTERNALSYM MCI_ANIM_OPEN_PARMS}
  PMCI_ANIM_OPEN_PARMS = ^tagMCI_ANIM_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_ANIM_OPEN_PARMS}
  LPMCI_ANIM_OPEN_PARMS = ^tagMCI_ANIM_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_ANIM_OPEN_PARMS}


  { parameter block for MCI_PLAY command message }

  PMCI_ANIM_PLAY_PARMS = ^tagMCI_ANIM_PLAY_PARMS;
  {$EXTERNALSYM PMCI_ANIM_PLAY_PARMS}
  tagMCI_ANIM_PLAY_PARMS = record
    dwCallback: DWORD_PTR;
    dwFrom: DWORD;
    dwTo: DWORD;
    dwSpeed: DWORD;
  end;
  {$EXTERNALSYM tagMCI_ANIM_PLAY_PARMS}
  MCI_ANIM_PLAY_PARMS = tagMCI_ANIM_PLAY_PARMS;
  {$EXTERNALSYM MCI_ANIM_PLAY_PARMS}
  LPMCI_ANIM_PLAY_PARMS = ^tagMCI_ANIM_PLAY_PARMS;
  {$EXTERNALSYM LPMCI_ANIM_PLAY_PARMS}


  { parameter block for MCI_STEP command message }

  PMCI_ANIM_STEP_PARMS = ^tagMCI_ANIM_STEP_PARMS;
  {$EXTERNALSYM PMCI_ANIM_STEP_PARMS}
  tagMCI_ANIM_STEP_PARMS = record
    dwCallback: DWORD_PTR;
    dwFrames: DWORD;
  end;
  {$EXTERNALSYM tagMCI_ANIM_STEP_PARMS}
  MCI_ANIM_STEP_PARMS = tagMCI_ANIM_STEP_PARMS;
  {$EXTERNALSYM MCI_ANIM_STEP_PARMS}
  LPMCI_ANIM_STEP_PARMS = ^tagMCI_ANIM_STEP_PARMS;
  {$EXTERNALSYM LPMCI_ANIM_STEP_PARMS}


  { parameter block for MCI_WINDOW command message }

  PMCI_ANIM_WINDOW_PARMSA = ^tagMCI_ANIM_WINDOW_PARMSA;
  {$EXTERNALSYM PMCI_ANIM_WINDOW_PARMSA}
  tagMCI_ANIM_WINDOW_PARMSA = record
    dwCallback: DWORD_PTR;
    hWnd: HWND;
    nCmdShow: UINT;
    lpstrText: PAnsiChar;
  end;
  {$EXTERNALSYM tagMCI_ANIM_WINDOW_PARMSA}
  MCI_ANIM_WINDOW_PARMSA = tagMCI_ANIM_WINDOW_PARMSA;
  {$EXTERNALSYM MCI_ANIM_WINDOW_PARMSA}
  LPMCI_ANIM_WINDOW_PARMSA = ^tagMCI_ANIM_WINDOW_PARMSA;
  {$EXTERNALSYM LPMCI_ANIM_WINDOW_PARMSA}

  PMCI_ANIM_WINDOW_PARMSW = ^tagMCI_ANIM_WINDOW_PARMSW;
  {$EXTERNALSYM PMCI_ANIM_WINDOW_PARMSW}
  tagMCI_ANIM_WINDOW_PARMSW = record
    dwCallback: DWORD_PTR;
    hWnd: HWND;
    nCmdShow: UINT;
    lpstrText: PWideChar;
  end;
  {$EXTERNALSYM tagMCI_ANIM_WINDOW_PARMSW}
  MCI_ANIM_WINDOW_PARMSW = tagMCI_ANIM_WINDOW_PARMSW;
  {$EXTERNALSYM MCI_ANIM_WINDOW_PARMSW}
  LPMCI_ANIM_WINDOW_PARMSW = ^tagMCI_ANIM_WINDOW_PARMSW;
  {$EXTERNALSYM LPMCI_ANIM_WINDOW_PARMSW}

  { unicode }
  MCI_ANIM_WINDOW_PARMS = tagMCI_ANIM_WINDOW_PARMSW;
  {$EXTERNALSYM MCI_ANIM_WINDOW_PARMS}
  PMCI_ANIM_WINDOW_PARMS = ^tagMCI_ANIM_WINDOW_PARMSW;
  {$EXTERNALSYM PMCI_ANIM_WINDOW_PARMS}
  LPMCI_ANIM_WINDOW_PARMS = ^tagMCI_ANIM_WINDOW_PARMSW;
  {$EXTERNALSYM LPMCI_ANIM_WINDOW_PARMS}


  { parameter block for MCI_PUT, MCI_UPDATE, MCI_WHERE command messages }

  PMCI_ANIM_RECT_PARMS = ^MCI_ANIM_RECT_PARMS;
  {$EXTERNALSYM PMCI_ANIM_RECT_PARMS}
  tagMCI_ANIM_RECT_PARMS = record
    dwCallback: DWORD_PTR;
    rc: TRect;
  end;
  {$EXTERNALSYM tagMCI_ANIM_RECT_PARMS}
  MCI_ANIM_RECT_PARMS = tagMCI_ANIM_RECT_PARMS;
  {$EXTERNALSYM MCI_ANIM_RECT_PARMS}
  LPMCI_ANIM_RECT_PARMS = ^MCI_ANIM_RECT_PARMS;
  {$EXTERNALSYM LPMCI_ANIM_RECT_PARMS}


  { parameter block for MCI_UPDATE PARMS }

  PMCI_ANIM_UPDATE_PARMS = ^tagMCI_ANIM_UPDATE_PARMS;
  {$EXTERNALSYM PMCI_ANIM_UPDATE_PARMS}
  tagMCI_ANIM_UPDATE_PARMS = record
    dwCallback: DWORD_PTR;
    rc: TRect;
    hDC: HDC;
  end;
  {$EXTERNALSYM tagMCI_ANIM_UPDATE_PARMS}
  MCI_ANIM_UPDATE_PARMS = tagMCI_ANIM_UPDATE_PARMS;
  {$EXTERNALSYM MCI_ANIM_UPDATE_PARMS}
  LPMCI_ANIM_UPDATE_PARMS = ^tagMCI_ANIM_UPDATE_PARMS;
  {$EXTERNALSYM LPMCI_ANIM_UPDATE_PARMS}

  { MCI extensions for video overlay devices }

const

  { flags for dwFlags parameter of MCI_OPEN command message }

  MCI_OVLY_OPEN_WS                    = $00010000;
  {$EXTERNALSYM MCI_OVLY_OPEN_WS}
  MCI_OVLY_OPEN_PARENT                = $00020000;
  {$EXTERNALSYM MCI_OVLY_OPEN_PARENT}

  { flags for dwFlags parameter of MCI_STATUS command message }

  MCI_OVLY_STATUS_HWND                = $00004001;
  {$EXTERNALSYM MCI_OVLY_STATUS_HWND}
  MCI_OVLY_STATUS_STRETCH             = $00004002;
  {$EXTERNALSYM MCI_OVLY_STATUS_STRETCH}

  { flags for dwFlags parameter of MCI_INFO command message }

  MCI_OVLY_INFO_TEXT                  = $00010000;
  {$EXTERNALSYM MCI_OVLY_INFO_TEXT}

  { flags for dwItem field of MCI_GETDEVCAPS_PARMS parameter block }

  MCI_OVLY_GETDEVCAPS_CAN_STRETCH     = $00004001;
  {$EXTERNALSYM MCI_OVLY_GETDEVCAPS_CAN_STRETCH}
  MCI_OVLY_GETDEVCAPS_CAN_FREEZE      = $00004002;
  {$EXTERNALSYM MCI_OVLY_GETDEVCAPS_CAN_FREEZE}
  MCI_OVLY_GETDEVCAPS_MAX_WINDOWS     = $00004003;
  {$EXTERNALSYM MCI_OVLY_GETDEVCAPS_MAX_WINDOWS}

  { flags for dwFlags parameter of MCI_WINDOW command message }

  MCI_OVLY_WINDOW_HWND                = $00010000;
  {$EXTERNALSYM MCI_OVLY_WINDOW_HWND}
  MCI_OVLY_WINDOW_STATE               = $00040000;
  {$EXTERNALSYM MCI_OVLY_WINDOW_STATE}
  MCI_OVLY_WINDOW_TEXT                = $00080000;
  {$EXTERNALSYM MCI_OVLY_WINDOW_TEXT}
  MCI_OVLY_WINDOW_ENABLE_STRETCH      = $00100000;
  {$EXTERNALSYM MCI_OVLY_WINDOW_ENABLE_STRETCH}
  MCI_OVLY_WINDOW_DISABLE_STRETCH     = $00200000;
  {$EXTERNALSYM MCI_OVLY_WINDOW_DISABLE_STRETCH}

  { flags for hWnd parameter of MCI_OVLY_WINDOW_PARMS parameter block }

  MCI_OVLY_WINDOW_DEFAULT             = $00000000;
  {$EXTERNALSYM MCI_OVLY_WINDOW_DEFAULT}

  { flags for dwFlags parameter of MCI_PUT command message }

  MCI_OVLY_RECT                       = $00010000;
  {$EXTERNALSYM MCI_OVLY_RECT}
  MCI_OVLY_PUT_SOURCE                 = $00020000;
  {$EXTERNALSYM MCI_OVLY_PUT_SOURCE}
  MCI_OVLY_PUT_DESTINATION            = $00040000;
  {$EXTERNALSYM MCI_OVLY_PUT_DESTINATION}
  MCI_OVLY_PUT_FRAME                  = $00080000;
  {$EXTERNALSYM MCI_OVLY_PUT_FRAME}
  MCI_OVLY_PUT_VIDEO                  = $00100000;
  {$EXTERNALSYM MCI_OVLY_PUT_VIDEO}

  { flags for dwFlags parameter of MCI_WHERE command message }

  MCI_OVLY_WHERE_SOURCE               = $00020000;
  {$EXTERNALSYM MCI_OVLY_WHERE_SOURCE}
  MCI_OVLY_WHERE_DESTINATION          = $00040000;
  {$EXTERNALSYM MCI_OVLY_WHERE_DESTINATION}
  MCI_OVLY_WHERE_FRAME                = $00080000;
  {$EXTERNALSYM MCI_OVLY_WHERE_FRAME}
  MCI_OVLY_WHERE_VIDEO                = $00100000;
  {$EXTERNALSYM MCI_OVLY_WHERE_VIDEO}


  { parameter block for MCI_OPEN command message }

type

  tagMCI_OVLY_OPEN_PARMSA = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PAnsiChar;
    lpstrElementName: PAnsiChar;
    lpstrAlias: PAnsiChar;
    dwStyle: DWORD;
    hWndParent: HWND;
  end;
  {$EXTERNALSYM tagMCI_OVLY_OPEN_PARMSA}
  MCI_OVLY_OPEN_PARMSA = tagMCI_OVLY_OPEN_PARMSA;
  {$EXTERNALSYM MCI_OVLY_OPEN_PARMSA}
  PMCI_OVLY_OPEN_PARMSA = ^tagMCI_OVLY_OPEN_PARMSA;
  {$EXTERNALSYM PMCI_OVLY_OPEN_PARMSA}
  LPMCI_OVLY_OPEN_PARMSA = ^tagMCI_OVLY_OPEN_PARMSA;
  {$EXTERNALSYM LPMCI_OVLY_OPEN_PARMSA}

  tagMCI_OVLY_OPEN_PARMSW = record
    dwCallback: DWORD_PTR;
    wDeviceID: MCIDEVICEID;
    lpstrDeviceType: PWideChar;
    lpstrElementName: PWideChar;
    lpstrAlias: PWideChar;
    dwStyle: DWORD;
    hWndParent: HWND;
  end;
  {$EXTERNALSYM tagMCI_OVLY_OPEN_PARMSW}
  MCI_OVLY_OPEN_PARMSW = tagMCI_OVLY_OPEN_PARMSW;
  {$EXTERNALSYM MCI_OVLY_OPEN_PARMSW}
  PMCI_OVLY_OPEN_PARMSW = ^tagMCI_OVLY_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_OPEN_PARMSW}
  LPMCI_OVLY_OPEN_PARMSW = ^tagMCI_OVLY_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_OPEN_PARMSW}

  { unicode }
  MCI_OVLY_OPEN_PARMS = tagMCI_OVLY_OPEN_PARMSW;
  {$EXTERNALSYM MCI_OVLY_OPEN_PARMS}
  PMCI_OVLY_OPEN_PARMS = ^tagMCI_OVLY_OPEN_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_OPEN_PARMS}
  LPMCI_OVLY_OPEN_PARMS = ^tagMCI_OVLY_OPEN_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_OPEN_PARMS}


  { parameter block for MCI_WINDOW command message }

  tagMCI_OVLY_WINDOW_PARMSA = record
    dwCallback: DWORD_PTR;
    hWnd: HWND;
    nCmdShow: UINT;
    lpstrText: PAnsiChar;
  end;
  {$EXTERNALSYM tagMCI_OVLY_WINDOW_PARMSA}
  MCI_OVLY_WINDOW_PARMSA = tagMCI_OVLY_WINDOW_PARMSA;
  {$EXTERNALSYM MCI_OVLY_WINDOW_PARMSA}
  PMCI_OVLY_WINDOW_PARMSA = ^tagMCI_OVLY_WINDOW_PARMSA;
  {$EXTERNALSYM PMCI_OVLY_WINDOW_PARMSA}
  LPMCI_OVLY_WINDOW_PARMSA = ^tagMCI_OVLY_WINDOW_PARMSA;
  {$EXTERNALSYM LPMCI_OVLY_WINDOW_PARMSA}

  tagMCI_OVLY_WINDOW_PARMSW = record
    dwCallback: DWORD_PTR;
    hWnd: HWND;
    nCmdShow: UINT;
    lpstrText: PWideChar;
  end;
  {$EXTERNALSYM tagMCI_OVLY_WINDOW_PARMSW}
  MCI_OVLY_WINDOW_PARMSW = tagMCI_OVLY_WINDOW_PARMSW;
  {$EXTERNALSYM MCI_OVLY_WINDOW_PARMSW}
  PMCI_OVLY_WINDOW_PARMSW = ^tagMCI_OVLY_WINDOW_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_WINDOW_PARMSW}
  LPMCI_OVLY_WINDOW_PARMSW = ^tagMCI_OVLY_WINDOW_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_WINDOW_PARMSW}

  { unicode }
  MCI_OVLY_WINDOW_PARMS = tagMCI_OVLY_WINDOW_PARMSW;
  {$EXTERNALSYM MCI_OVLY_WINDOW_PARMS}
  PMCI_OVLY_WINDOW_PARMS = ^tagMCI_OVLY_WINDOW_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_WINDOW_PARMS}
  LPMCI_OVLY_WINDOW_PARMS = ^tagMCI_OVLY_WINDOW_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_WINDOW_PARMS}


  { parameter block for MCI_PUT, MCI_UPDATE, and MCI_WHERE command messages }

  PMCI_OVLY_RECT_PARMS = ^tagMCI_OVLY_RECT_PARMS;
  {$EXTERNALSYM PMCI_OVLY_RECT_PARMS}
  tagMCI_OVLY_RECT_PARMS = record
    dwCallback: DWORD_PTR;
    rc: TRect;
  end;
  {$EXTERNALSYM tagMCI_OVLY_RECT_PARMS}
  MCI_OVLY_RECT_PARMS = tagMCI_OVLY_RECT_PARMS;
  {$EXTERNALSYM MCI_OVLY_RECT_PARMS}
  LPMCI_OVLY_RECT_PARMS = ^tagMCI_OVLY_RECT_PARMS;
  {$EXTERNALSYM LPMCI_OVLY_RECT_PARMS}


  { parameter block for MCI_SAVE command message }

  PMCI_OVLY_SAVE_PARMSA = ^tagMCI_OVLY_SAVE_PARMSA;
  {$EXTERNALSYM PMCI_OVLY_SAVE_PARMSA}
  tagMCI_OVLY_SAVE_PARMSA = record
    dwCallback: DWORD_PTR;
    lpfilename: PAnsiChar;
    rc: TRect;
  end;
  {$EXTERNALSYM tagMCI_OVLY_SAVE_PARMSA}
  MCI_OVLY_SAVE_PARMSA = tagMCI_OVLY_SAVE_PARMSA;
  {$EXTERNALSYM MCI_OVLY_SAVE_PARMSA}
  LPMCI_OVLY_SAVE_PARMSA = ^tagMCI_OVLY_SAVE_PARMSA;
  {$EXTERNALSYM LPMCI_OVLY_SAVE_PARMSA}

  PMCI_OVLY_SAVE_PARMSW = ^tagMCI_OVLY_SAVE_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_SAVE_PARMSW}
  tagMCI_OVLY_SAVE_PARMSW = record
    dwCallback: DWORD_PTR;
    lpfilename: PWideChar;
    rc: TRect;
  end;
  {$EXTERNALSYM tagMCI_OVLY_SAVE_PARMSW}
  MCI_OVLY_SAVE_PARMSW = tagMCI_OVLY_SAVE_PARMSW;
  {$EXTERNALSYM MCI_OVLY_SAVE_PARMSW}
  LPMCI_OVLY_SAVE_PARMSW = ^tagMCI_OVLY_SAVE_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_SAVE_PARMSW}

  { unicode }
  MCI_OVLY_SAVE_PARMS = tagMCI_OVLY_SAVE_PARMSW;
  {$EXTERNALSYM MCI_OVLY_SAVE_PARMS}
  PMCI_OVLY_SAVE_PARMS = ^tagMCI_OVLY_SAVE_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_SAVE_PARMS}
  LPMCI_OVLY_SAVE_PARMS = ^tagMCI_OVLY_SAVE_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_SAVE_PARMS}


  { parameter block for MCI_LOAD command message }

  PMCI_OVLY_LOAD_PARMSA = ^tagMCI_OVLY_LOAD_PARMSA;
  {$EXTERNALSYM PMCI_OVLY_LOAD_PARMSA}
  tagMCI_OVLY_LOAD_PARMSA = record
    dwCallback: DWORD_PTR;
    lpfilename: PAnsiChar;
    rc: TRect;
  end;
  {$EXTERNALSYM tagMCI_OVLY_LOAD_PARMSA}
  MCI_OVLY_LOAD_PARMSA = tagMCI_OVLY_LOAD_PARMSA;
  {$EXTERNALSYM MCI_OVLY_LOAD_PARMSA}
  LPMCI_OVLY_LOAD_PARMSA = ^tagMCI_OVLY_LOAD_PARMSA;
  {$EXTERNALSYM LPMCI_OVLY_LOAD_PARMSA}

  PMCI_OVLY_LOAD_PARMSW = ^tagMCI_OVLY_LOAD_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_LOAD_PARMSW}
  tagMCI_OVLY_LOAD_PARMSW = record
    dwCallback: DWORD_PTR;
    lpfilename: PWideChar;
    rc: TRect;
  end;
  {$EXTERNALSYM tagMCI_OVLY_LOAD_PARMSW}
  MCI_OVLY_LOAD_PARMSW = tagMCI_OVLY_LOAD_PARMSW;
  {$EXTERNALSYM MCI_OVLY_LOAD_PARMSW}
  LPMCI_OVLY_LOAD_PARMSW = ^tagMCI_OVLY_LOAD_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_LOAD_PARMSW}

  { unicode }
  MCI_OVLY_LOAD_PARMS = tagMCI_OVLY_LOAD_PARMSW;
  {$EXTERNALSYM MCI_OVLY_LOAD_PARMS}
  PMCI_OVLY_LOAD_PARMS = ^tagMCI_OVLY_LOAD_PARMSW;
  {$EXTERNALSYM PMCI_OVLY_LOAD_PARMS}
  LPMCI_OVLY_LOAD_PARMS = ^tagMCI_OVLY_LOAD_PARMSW;
  {$EXTERNALSYM LPMCI_OVLY_LOAD_PARMSW}


  //
  // APIs moved from mmddk.h - function prototypes for MCI driver functions
  //

  function mciGetDriverData(wDeviceID: MCIDEVICEID): DWORD_PTR; stdcall;
  {$EXTERNALSYM mciGetDriverData}

  function mciLoadCommandResource(hInstance: THandle;
                                  lpResName: PWideChar;
                                  wType: UINT): UINT; stdcall;
  {$EXTERNALSYM mciLoadCommandResource}

  function mciSetDriverData(wDeviceID: MCIDEVICEID;
                            dwData: DWORD_PTR): BOOL; stdcall;
  {$EXTERNALSYM mciSetDriverData}

  function mciDriverYield(wDeviceID: MCIDEVICEID): BOOL; stdcall;
  {$EXTERNALSYM mciDriverYield}

  function mciDriverNotify(hwndCallback: THandle;
                           wDeviceID: MCIDEVICEID;
                           uStatus: UINT): BOOL; stdcall;
  {$EXTERNALSYM mciDriverNotify}

  function mciFreeCommandResource(wTable: UINT): BOOL; stdcall;
  {$EXTERNALSYM mciFreeCommandResource}

  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  MCIApiLib = 'winmm.dll';

function mciSendCommandA; external MCIApiLib name 'mciSendCommandA';
function mciSendCommandW; external MCIApiLib name 'mciSendCommandW';
function mciSendCommand; external MCIApiLib name 'mciSendCommandW';

function mciSendStringA; external MCIApiLib name 'mciSendStringA';
function mciSendStringW; external MCIApiLib name 'mciSendStringW';
function mciSendString; external MCIApiLib name 'mciSendStringW';

function mciGetDeviceIDA; external MCIApiLib name 'mciGetDeviceIDA';
function mciGetDeviceIDW; external MCIApiLib name 'mciGetDeviceIDW';
function mciGetDeviceID; external MCIApiLib name 'mciGetDeviceIDW';

function mciGetDeviceIDFromElementIDA; external MCIApiLib name 'mciGetDeviceIDFromElementIDA';
function mciGetDeviceIDFromElementIDW; external MCIApiLib name 'mciGetDeviceIDFromElementIDW';
function mciGetDeviceIDFromElementID; external MCIApiLib name 'mciGetDeviceIDFromElementIDW';

function mciGetErrorStringA; external MCIApiLib name 'mciGetErrorStringA';
function mciGetErrorStringW; external MCIApiLib name 'mciGetErrorStringW';
function mciGetErrorString; external MCIApiLib name 'mciGetErrorStringW';

function mciSetYieldProc; external MCIApiLib name 'mciSetYieldProc';
function mciGetCreatorTask; external MCIApiLib name 'mciGetCreatorTask';
function mciGetYieldProc; external MCIApiLib name 'mciGetYieldProc';

// APIs moved from mmddk.h
function mciGetDriverData; external MCIApiLib name 'mciGetDriverData';
function mciLoadCommandResource; external MCIApiLib name 'mciLoadCommandResource';
function mciSetDriverData; external MCIApiLib name 'mciSetDriverData';
function mciDriverYield; external MCIApiLib name 'mciDriverYield';
function mciDriverNotify; external MCIApiLib name 'mciDriverNotify';
function mciFreeCommandResource; external MCIApiLib name 'mciFreeCommandResource';


// Converted Macro's

function MCI_MSF_MINUTE(msf: DWord): Byte;
begin
  Result := LoByte(LoWord(msf));
end;

function MCI_MSF_SECOND(msf: DWord): Byte;
begin
  Result := HiByte(LoWord(msf));
end;

function MCI_MSF_FRAME(msf: DWord): Byte;
begin
  Result := LoByte(HiWord(msf));
end;

function MCI_MAKE_MSF(m: Byte;
                      s: Byte;
                      f: Byte): DWord;
begin
  Result := DWord(m or (s shl 8) or (f shl 16));
end;

function MCI_TMSF_TRACK(tmsf: DWord): Byte;
begin
  Result := LoByte(LoWord(tmsf));
end;

function MCI_TMSF_MINUTE(tmsf: DWord): Byte;
begin
  Result := HiByte(LoWord(tmsf));
end;

function MCI_TMSF_SECOND(tmsf: DWord): Byte;
begin
  Result := LoByte(HiWord(tmsf));
end;

function MCI_TMSF_FRAME(tmsf: DWord): Byte;
begin
  Result := HiByte(HiWord(tmsf));
end;

function MCI_MAKE_TMSF(t: Byte;
                       m: Byte;
                       s: Byte;
                       f: Byte): DWord;
begin
  Result := DWord(t or (m shl 8) or (s shl 16) or (f shl 24));
end;

function MCI_HMS_HOUR(hms: DWord): Byte;
begin
  Result := LoByte(LoWord(hms));
end;

function MCI_HMS_MINUTE(hms: DWord): Byte;
begin
  Result := HiByte(LoWord(hms));
end;

function MCI_HMS_SECOND(hms: DWord): Byte;
begin
  Result := LoByte(HiWord(hms));
end;

function MCI_MAKE_HMS(h: Byte;
                      m: Byte;
                      s: Byte): DWord;
begin
  Result := DWord(h or (m shl 8) or (s shl 16));
end;




end.
