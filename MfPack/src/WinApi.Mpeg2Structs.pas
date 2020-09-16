// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Mpeg2Structs.pas
// Kind: Pascal / Delphi unit
// Release date: 16-09-2020
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Definitions for the common structures used in WinApi.Mpeg2Data.pas
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
// Remarks: Requires Windows Vista or later.
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
// Source: mpeg2structs.h
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
unit WinApi.Mpeg2Structs;

interface

uses
  WinApi.WinApiTypes,
  WinApi.Mpeg2Bits;

  {$MINENUMSIZE 4}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


//
// Basic Type Aliases
//

type

  PPID = ^PID;
  PID = WORD;  // program id
  {$EXTERNALSYM PID}

  PTID = ^TID;
  TID = Byte;  // table id
  {$EXTERNALSYM TID}

  PTEID = ^TEID;
  TEID = WORD;  // table extension id
  {$EXTERNALSYM TEID}

  PClientKey = ^ClientKey;
  ClientKey = UINT;
  {$EXTERNALSYM ClientKey}

//
// MPEG-2 Current/Next bit field
//

type

  PMPEG_CURRENT_NEXT_BIT = ^MPEG_CURRENT_NEXT_BIT;
  MPEG_CURRENT_NEXT_BIT     = (
    MPEG_SECTION_IS_NEXT    = 0,
    MPEG_SECTION_IS_CURRENT = 1);
  {$EXTERNALSYM MPEG_CURRENT_NEXT_BIT}

//
// MPEG-2 TID Extension structure
//

  PTID_EXTENSION = ^TID_EXTENSION;
  {$EXTERNALSYM PTID_EXTENSION}
  TID_EXTENSION = record
    wTidExt: WORD;
    wCount: WORD;
  end;
  {$EXTERNALSYM TID_EXTENSION}


//
// MPEG-2 packet "small" header structure
//

type

  PSECTION = ^SECTION;
  {$EXTERNALSYM PSECTION}
  SECTION = record
    TableId: TID;
    Header: record
    case integer of
      0: (S: MPEG_HEADER_BITS_MIDL);
      1: (W: WORD);
    end;
    SectionData: PByte;  // Array size is Header.S.SectionLength
  end;
  {$EXTERNALSYM SECTION}


//
// MPEG-2 packet "long" header structure
//

type

  PLONG_SECTION = ^LONG_SECTION;
  {$EXTERNALSYM PLONG_SECTION}
  LONG_SECTION  = record

    TableId: TID;
    header: record
    case integer of
      0: (S: MPEG_HEADER_BITS_MIDL);
      1: (W: WORD);
    end;

    TableIdExtension: TEID;
    Version: record
    case integer of
      0: (S: MPEG_HEADER_VERSION_BITS_MIDL);
      1: (B: Byte);
    end;

    SectionNumber: Byte;
    LastSectionNumber: Byte;
    RemainingData: PByte;    // Array size is Header.S.SectionLength - 5
  end;
  {$EXTERNALSYM LONG_SECTION}


//
// DSM-CC packet header structure
//

type

  PDSMCC_SECTION = ^DSMCC_SECTION;
  {$EXTERNALSYM PDSMCC_SECTION}
  DSMCC_SECTION = record
    TableId: TID;
    Header: record
    case integer of
      0:  (S: MPEG_HEADER_BITS_MIDL);
      1:  (W: WORD);
    end;

    TableIdExtension: TEID;
    Version: record
    case integer of
      0: (S: MPEG_HEADER_VERSION_BITS_MIDL);
      1: (B: Byte);
    end;

    SectionNumber: Byte;
    LastSectionNumber: Byte;
    ProtocolDiscriminator: Byte;
    DsmccType: Byte;
    MessageId: WORD;
    TransactionId: DWORD;
    Reserved: Byte;
    AdaptationLength: Byte;
    MessageLength: WORD;
    RemainingData: PByte;
  end;
  {$EXTERNALSYM DSMCC_SECTION}


//
// MPEG-2 request/response packets structures
//

type

  PMPEG_RQST_PACKET = ^MPEG_RQST_PACKET;
  {$EXTERNALSYM PMPEG_RQST_PACKET}
  MPEG_RQST_PACKET = record
    dwLength: DWORD;
    pSection: PSECTION;
  end;
  {$EXTERNALSYM MPEG_RQST_PACKET}

  {$EXTERNALSYM PMPEG_PACKET_LIST}
  PMPEG_PACKET_LIST = ^MPEG_PACKET_LIST;
  MPEG_PACKET_LIST = record
    wPacketCount: WORD;
    PacketList: PMPEG_RQST_PACKET;  // pointer to array of MPEG_RQST_PACKET
  end;
  {$EXTERNALSYM MPEG_PACKET_LIST}



//
// DSM-CC request filter options
//

type

  PDSMCC_FILTER_OPTIONS = ^DSMCC_FILTER_OPTIONS;
  DSMCC_FILTER_OPTIONS = record
    fSpecifyProtocol: BOOL;          // If true, Protocol should be set to desired value
    Protocol: Byte;
    fSpecifyType: BOOL;              // If true, Type should be set to desired value
    _Type: Byte;
    fSpecifyMessageId: BOOL;         // If true, MessageId should be set to desired value
    MessageId: WORD;
    fSpecifyTransactionId: BOOL;     // If true, TransactionId (or DownloadId for DDB msgs) should be set to desired value
    fUseTrxIdMessageIdMask: BOOL;    // If false, TransactionId is filtered as is.
                                     // If true, TransactionId is masked to look
                                     // for any version of message with associated
                                     // message identifier. See DVB - Data
                                     // Broadcasting Guidlines 4.6.5. (Assignment
                                     // and use of transactionId values).
    TransactionId: DWORD;
    fSpecifyModuleVersion: BOOL;     // If true, ModuleVersion should be set to the desired value
    ModuleVersion: Byte;
    fSpecifyBlockNumber: BOOL;       // If true, BlockNumber should be set to desired value
    BlockNumber: WORD;
    fGetModuleCall: BOOL;            // If true, NumberOfBlocksInModule should be set
    NumberOfBlocksInModule: WORD;
  end; // 45 BYTES
  {$EXTERNALSYM DSMCC_FILTER_OPTIONS}


//
// ATSC request filter options
//

type

  PATSC_FILTER_OPTIONS = ^ATSC_FILTER_OPTIONS;
  ATSC_FILTER_OPTIONS = record
    fSpecifyEtmId: BOOL;            // If true, EtmId should be set to desired value
    EtmId: DWORD;
  end; // 8 BYTES
  {$EXTERNALSYM ATSC_FILTER_OPTIONS}


//
// DVB/ISDB EIT request filter options
//

type

  PDVB_EIT_FILTER_OPTIONS = ^DVB_EIT_FILTER_OPTIONS;
  DVB_EIT_FILTER_OPTIONS = record
    fSpecifySegment: BOOL;          // If true, bSegment should be set to desired value
    bSegment: Byte;
  end; // 5 BYTES
  {$EXTERNALSYM DVB_EIT_FILTER_OPTIONS}


//
// MPEG-2 request filter structure
//

type

  PMPEG2_FILTER = ^MPEG2_FILTER;
  {$EXTERNALSYM PMPEG2_FILTER}
  MPEG2_FILTER = record
    bVersionNumber: Byte;             // Must be set to 1 or more to match filter definition
    wFilterSize: WORD;                // Size of total filter structure. Version 1 filter is 73 bytes.
    fUseRawFilteringBits: BOOL;       // If true, Filter and Mask fields should be set to desired value, all other
                                      // fields with be ignored.
    Filter: array[0..15] of Byte;     // Bits with values to compare against for a match.
    Mask: array[0..15] of Byte;       // Bits set to 0 are bits that are compared to those in the filter, those
                                      // bits set to 1 are ignored.
    fSpecifyTableIdExtension: BOOL;   // If true, TableIdExtension should be set to desired value (false = don't care)
    TableIdExtension: WORD;
    fSpecifyVersion: BOOL;            // If true, Version should be set to desired value (false = don't care)
    Version: Byte;
    fSpecifySectionNumber: BOOL;      // If true, SectionNumber should be set to desired value (false = don't care)
    SectionNumber: Byte;
    fSpecifyCurrentNext: BOOL;        // If true, fNext should be set to desired value (false = don't care)
    fNext: BOOL;                      // If true, next table is queried. Else, current
    fSpecifyDsmccOptions: BOOL;       // If true, Dsmcc should be set with desired filter options
    Dsmcc: DSMCC_FILTER_OPTIONS;
    fSpecifyAtscOptions: BOOL;        // If true, Atsc should be set with desired filter options
    Atsc: ATSC_FILTER_OPTIONS;
  end; // 124 BYTES
  {$EXTERNALSYM MPEG2_FILTER}


  PMPEG2_FILTER2 = ^MPEG2_FILTER2;
  {$EXTERNALSYM PMPEG2_FILTER2}
  MPEG2_FILTER2 = record
    case integer of

      0: (  bVersionNumber: Byte;
            wFilterSize: WORD;
            fUseRawFilteringBits: BOOL;

            Filter: array [0..15] of Byte;
            Mask: array [0..15] of Byte;

            fSpecifyTableIdExtension: BOOL;
            TableIdExtension: WORD;
            fSpecifyVersion: BOOL;
            Version: Byte;
            fSpecifySectionNumber: BOOL;
            SectionNumber: Byte;
            fSpecifyCurrentNext: BOOL;
            fNext: BOOL;
            fSpecifyDsmccOptions: BOOL;
            Dsmcc: DSMCC_FILTER_OPTIONS;
            fSpecifyAtscOptions: BOOL;
            Atsc: ATSC_FILTER_OPTIONS;

            fSpecifyDvbEitOptions: BOOL;    // Set true for DVB/ISDB EIT. The table is handled segment basis.
                                            // If true, DvbEit should be set with desired filter options
            DvbEit: DVB_EIT_FILTER_OPTIONS
            // Version 2 - 133 bytes
            );
      1:  (  bVersion1Bytes: array [0..123] of Byte
            // Version 1 - 124 bytes
            );
  end;
  {$EXTERNALSYM MPEG2_FILTER2}


const

  MPEG2_FILTER_VERSION_1_SIZE         = 124;
  {$EXTERNALSYM MPEG2_FILTER_VERSION_1_SIZE}
  MPEG2_FILTER_VERSION_2_SIZE         = 133;
  {$EXTERNALSYM MPEG2_FILTER_VERSION_2_SIZE}


//
// Mpeg-2 Stream buffer structure
//

type

  PMPEG_STREAM_BUFFER = ^MPEG_STREAM_BUFFER;
  {$EXTERNALSYM PMPEG_STREAM_BUFFER}
  MPEG_STREAM_BUFFER = record
    hr: HResult;
    dwDataBufferSize: DWORD;
    dwSizeOfDataRead: DWORD;
    pDataBuffer: PByte;  // Pointer to array of byte
  end;
  {$EXTERNALSYM MPEG_STREAM_BUFFER}



//
// MPEG-2 Time and Date structures
//

type
  PMPEG_TIME = ^MPEG_TIME;
  MPEG_TIME = record
    Hours: Byte;                    // Legal Range: 0 to 23
    Minutes: Byte;                  // Legal Range: 0 to 59
    Seconds: Byte;                  // Legal Range: 0 to 59
  end;
  {$EXTERNALSYM MPEG_TIME}

  PMPEG_DURATION = ^MPEG_DURATION;
  MPEG_DURATION = MPEG_TIME;
  {$EXTERNALSYM MPEG_DURATION}

  PMPEG_DATE = ^MPEG_DATE;
  MPEG_DATE = record
    Date: Byte;                     // Legal Range: 1 to 31
    Month: Byte;                    // Legal Range: 1 to 12
    Year: WORD;                     // Legal Range: 1900 to 2100
  end;
  {$EXTERNALSYM MPEG_DATE}

  PMPEG_DATE_AND_TIME = ^MPEG_DATE_AND_TIME;
  MPEG_DATE_AND_TIME = record
    D: MPEG_DATE;
    T: MPEG_TIME;
  end;
  {$EXTERNALSYM MPEG_DATE_AND_TIME}


//
// MPEG-2 API Context structures
//

type

  PMPEG_CONTEXT_TYPE = ^MPEG_CONTEXT_TYPE;
  MPEG_CONTEXT_TYPE = (
    MPEG_CONTEXT_BCS_DEMUX,
    MPEG_CONTEXT_WINSOCK
                      );
  {$EXTERNALSYM MPEG_CONTEXT_TYPE}

  PMPEG_BCS_DEMUX = ^MPEG_BCS_DEMUX;
  MPEG_BCS_DEMUX = record
    AVMGraphId: DWORD;
  end;
  {$EXTERNALSYM MPEG_BCS_DEMUX}

  PMPEG_WINSOCK = ^MPEG_WINSOCK;
  MPEG_WINSOCK = record
    AVMGraphId: DWORD;
  end;
  {$EXTERNALSYM MPEG_WINSOCK}


  PMPEG_CONTEXT = ^MPEG_CONTEXT;
  MPEG_CONTEXT = record

    _Type: MPEG_CONTEXT_TYPE;
    U: record
    case integer of
      0: (Demux: MPEG_BCS_DEMUX);
      1: (Winsock: MPEG_WINSOCK);
    end;
  end;


//
// MPEG-2 Service Request and Responses
//

type
  PMPEG_REQUEST_TYPE = ^MPEG_REQUEST_TYPE;
  MPEG_REQUEST_TYPE   = DWord;
  {$EXTERNALSYM MPEG_REQUEST_TYPE}
const
    MPEG_RQST_UNKNOWN               = MPEG_REQUEST_TYPE(0);
    MPEG_RQST_GET_SECTION           = MPEG_REQUEST_TYPE(1);
    MPEG_RQST_GET_SECTION_ASYNC     = MPEG_REQUEST_TYPE(2);
    MPEG_RQST_GET_TABLE             = MPEG_REQUEST_TYPE(3);
    MPEG_RQST_GET_TABLE_ASYNC       = MPEG_REQUEST_TYPE(4);
    MPEG_RQST_GET_SECTIONS_STREAM   = MPEG_REQUEST_TYPE(5);
    MPEG_RQST_GET_PES_STREAM        = MPEG_REQUEST_TYPE(6);
    MPEG_RQST_GET_TS_STREAM         = MPEG_REQUEST_TYPE(7);
    MPEG_RQST_START_MPE_STREAM      = MPEG_REQUEST_TYPE(8);

type

  PMPEG_SERVICE_REQUEST = ^MPEG_SERVICE_REQUEST;
  {$EXTERNALSYM PMPEG_SERVICE_REQUEST}
  MPEG_SERVICE_REQUEST = record
    _Type: MPEG_REQUEST_TYPE;
    Context: MPEG_CONTEXT;
    Pid: PID;
    TableId: TID;
    Filter: MPEG2_FILTER;
    Flags: DWORD;
  end;
  {$EXTERNALSYM MPEG_SERVICE_REQUEST}

  PMPEG_SERVICE_RESPONSE = ^MPEG_SERVICE_RESPONSE;
  {$EXTERNALSYM PMPEG_SERVICE_RESPONSE}
  MPEG_SERVICE_RESPONSE = record
    IPAddress: DWORD;
    Port: WORD;
  end;
  {$EXTERNALSYM MPEG_SERVICE_RESPONSE}


//
// DSM-CC & MPE Query Results
//

type

  PDSMCC_ELEMENT = ^_DSMCC_ELEMENT;
  {$EXTERNALSYM PDSMCC_ELEMENT}
  P_DSMCC_ELEMENT = PDSMCC_ELEMENT;
  {$EXTERNALSYM P_DSMCC_ELEMENT}
  _DSMCC_ELEMENT = record
    pid: PID;
    bComponentTag: Byte;
    dwCarouselId: DWORD;
    dwTransactionId: DWORD;
    pNext: P_DSMCC_ELEMENT;
  end;
  {$EXTERNALSYM _DSMCC_ELEMENT}
  DSMCC_ELEMENT = _DSMCC_ELEMENT;
  {$EXTERNALSYM DSMCC_ELEMENT}


  PMPE_ELEMENT = ^_MPE_ELEMENT;
  {$EXTERNALSYM PMPE_ELEMENT}
  P_MPE_ELEMENT = PMPE_ELEMENT;
  {$EXTERNALSYM P_MPE_ELEMENT}
  _MPE_ELEMENT = record
    pid: PID;
    bComponentTag: Byte;
    pNext: P_MPE_ELEMENT;
  end;
  {$EXTERNALSYM _MPE_ELEMENT}
  MPE_ELEMENT = _MPE_ELEMENT;
  {$EXTERNALSYM MPE_ELEMENT}


//
// MPEG-2 Stream Filtering Structure
//

type
  PMPEG_STREAM_FILTER = ^_MPEG_STREAM_FILTER;
  _MPEG_STREAM_FILTER = record
    wPidValue: WORD;                    // PID value
    dwFilterSize: DWORD;                // size of filter in bits
    fCrcEnabled: BOOL;                  // enable/disable CRC check
    rgchFilter: array[0..15] of Byte;   // filter data
    rgchMask: array[0..15] of Byte;    // filter mask
  end;
  {$EXTERNALSYM _MPEG_STREAM_FILTER}
  MPEG_STREAM_FILTER = ^_MPEG_STREAM_FILTER;
  {$EXTERNALSYM MPEG_STREAM_FILTER}

//#undef BIG_ENDIAN_MACHINE
//#define LITTLE_ENDIAN_MACHINE

// Leave the macro's for what they are


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
