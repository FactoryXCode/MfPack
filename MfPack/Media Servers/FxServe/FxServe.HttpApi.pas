// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: FxServe.HttpApi.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: HTTP API.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)ws 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Microsoft Learn.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://mozilla.org/MPL/2.0/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit FxServe.HttpApi;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock;

{$Z4}

const

// API constants ===============================================================

  HTTP_INITIALIZE_SERVER                     = $00000001;
  HTTP_SEND_RESPONSE_FLAG_DISCONNECT         = $00000001;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA          = $00000002;
  HTTP_SEND_RESPONSE_FLAG_AUTOMATIC_CHUNKING = $00000200;
  HTTP_NULL_ID                               = UInt64(0);

  HttpDataChunkFromMemory = 0;

  HttpVerbUnknown         = 1;
  HttpVerbOPTIONS         = 3;
  HttpVerbGET             = 4;
  HttpVerbHEAD            = 5;

  HttpHeaderCacheControl    = 0;
  HttpHeaderConnection      = 1;
  HttpHeaderContentLength   = 11;
  HttpHeaderContentType     = 12;
  HttpHeaderContentRange    = 17;
  HttpHeaderLastModified    = 19;
  HttpHeaderAcceptRanges    = 20;
  HttpHeaderEtag            = 22;
  HttpHeaderLocation        = 23;
  HttpHeaderServer          = 26;
  HttpHeaderRequestHost     = 28;
  HttpHeaderRequestRange    = 37;
  HttpHeaderRequestMaximum  = 41;
  HttpHeaderResponseMaximum = 30;


type

// API records =================================================================

  THttpApiVersion = record
    HttpApiMajorVersion: Word;
    HttpApiMinorVersion: Word;
  end;

  THttpVersion = record
    MajorVersion: Word;
    MinorVersion: Word;
  end;


  PHttpKnownHeader = ^THttpKnownHeader;
  THttpKnownHeader = record
    RawValueLength: Word;
    pRawValue: PAnsiChar;
  end;


  PHttpUnknownHeader = ^THttpUnknownHeader;
  THttpUnknownHeader = record
    NameLength: Word;
    RawValueLength: Word;
    pName: PAnsiChar;
    pRawValue: PAnsiChar;
  end;
  THttpUnknownHeaderArray = array[0..0] of THttpUnknownHeader;
  PHttpUnknownHeaderArray = ^THttpUnknownHeaderArray;


  THttpRequestHeaders = record
    UnknownHeaderCount: Word;
    pUnknownHeaders: PHttpUnknownHeader;
    TrailerCount: Word;
    pTrailers: PHttpUnknownHeader;
    KnownHeaders: array[0..HttpHeaderRequestMaximum - 1] of THttpKnownHeader;
  end;

  THttpResponseHeaders = record
    UnknownHeaderCount: Word;
    pUnknownHeaders: PHttpUnknownHeader;
    TrailerCount: Word;
    pTrailers: PHttpUnknownHeader;
    KnownHeaders: array[0..HttpHeaderResponseMaximum - 1] of THttpKnownHeader;
  end;

  THttpCookedUrl = record
    FullUrlLength: Word;
    HostLength: Word;
    AbsPathLength: Word;
    QueryStringLength: Word;
    pFullUrl: PWideChar;
    pHost: PWideChar;
    pAbsPath: PWideChar;
    pQueryString: PWideChar;
  end;

  THttpTransportAddress = record
    pRemoteAddress: PSockAddr;
    pLocalAddress: PSockAddr;
  end;

  PHttpDataChunk = ^THttpDataChunk;
  THttpDataChunk = record
    DataChunkType: Cardinal;
    case Integer of
      0: (
        pBuffer: Pointer;
        BufferLength: Cardinal
      );
      1: (
        ReservedUnion: array[0..2] of UInt64
      );
  end;


  PHttpRequest = ^THttpRequest;
  THttpRequest = record
    Flags: Cardinal;
    ConnectionId: UInt64;
    RequestId: UInt64;
    UrlContext: UInt64;
    Version: THttpVersion;
    Verb: Cardinal;
    UnknownVerbLength: Word;
    RawUrlLength: Word;
    pUnknownVerb: PAnsiChar;
    pRawUrl: PAnsiChar;
    CookedUrl: THttpCookedUrl;
    Address: THttpTransportAddress;
    Headers: THttpRequestHeaders;
    BytesReceived: UInt64;
    EntityChunkCount: Word;
    pEntityChunks: PHttpDataChunk;
    RawConnectionId: UInt64;
    pSslInfo: Pointer;
{$IFDEF WIN32}
    V1AlignmentPadding: Cardinal;
{$ENDIF}
    RequestInfoCount: Word;
    pRequestInfo: Pointer;
  end;


  PHttpResponse = ^THttpResponse;
  THttpResponse = record
    Flags: Cardinal;
    Version: THttpVersion;
    StatusCode: Word;
    ReasonLength: Word;
    pReason: PAnsiChar;
    Headers: THttpResponseHeaders;
    EntityChunkCount: Word;
    pEntityChunks: PHttpDataChunk;
    ResponseInfoCount: Word;
    pResponseInfo: Pointer;
  end;


// API methods =================================================================

  function HttpInitialize(Version: THttpApiVersion;
                          Flags: Cardinal;
                        Reserved: Pointer): Cardinal; stdcall;

  function HttpTerminate(Flags: Cardinal;
                       Reserved: Pointer): Cardinal; stdcall;

  function HttpCreateHttpHandle(out RequestQueueHandle: THandle;
                              Reserved: Cardinal): Cardinal; stdcall;

  function HttpAddUrl(RequestQueueHandle:
                      THandle; FullyQualifiedUrl: PWideChar;
                      Reserved: Pointer): Cardinal; stdcall;

  function HttpRemoveUrl(RequestQueueHandle: THandle;
                         FullyQualifiedUrl: PWideChar): Cardinal; stdcall;

  function HttpReceiveHttpRequest(RequestQueueHandle: THandle;
                                  RequestId: UInt64;
                                  Flags: Cardinal;
                                  RequestBuffer: PHttpRequest;
                                  RequestBufferLength: Cardinal;
                                  BytesReturned: PCardinal;
                                  Overlapped: POverlapped): Cardinal; stdcall;

  function HttpSendHttpResponse(RequestQueueHandle: THandle;
                                RequestId: UInt64;
                                Flags: Cardinal;
                                HttpResponse: PHttpResponse;
                                CachePolicy: Pointer;
                                BytesSent: PCardinal;
                                Reserved1: Pointer;
                                Reserved2: Cardinal;
                                Overlapped: POverlapped;
                                LogData: Pointer): Cardinal; stdcall;

  function HttpSendResponseEntityBody(RequestQueueHandle: THandle;
                                      RequestId: UInt64;
                                      Flags: Cardinal;
                                      EntityChunkCount: Word;
                                      EntityChunks: PHttpDataChunk;
                                      BytesSent: PCardinal;
                                      Reserved1: Pointer;
                                      Reserved2: Cardinal;
                                      Overlapped: POverlapped;
                                      LogData: Pointer): Cardinal; stdcall;


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

const
  HttpApiLib = 'httpapi.dll';

{$WARN SYMBOL_PLATFORM OFF}

 function HttpInitialize; external HttpApiLib name 'HttpInitialize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
 function HttpTerminate;  external HttpApiLib name 'HttpTerminate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
 function HttpCreateHttpHandle; external HttpApiLib name 'HttpCreateHttpHandle' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

 function HttpAddUrl; external HttpApiLib name 'HttpAddUrl' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
 function HttpRemoveUrl; external HttpApiLib name 'HttpRemoveUrl' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

 function HttpReceiveHttpRequest; external HttpApiLib name 'HttpReceiveHttpRequest' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
 function HttpSendHttpResponse; external HttpApiLib name 'HttpSendHttpResponse' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
 function HttpSendResponseEntityBody; external HttpApiLib name 'HttpSendResponseEntityBody' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

// Implement Additional Prototypes here.

end.
