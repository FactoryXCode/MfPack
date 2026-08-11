// FactoryX
//
// Copyright © FactoryX, Netherlands/Australia/Germany. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: MfCastProtocol.pas
// Kind: Pascal Unit
// Release date: 10-08-2026
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Stateless Google Cast V2 protobuf-envelope encoding and decoding.
//              Socket ownership, receiver state, and command policy deliberately
//              remain outside this unit.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
// 10/08/2026 Carmen              Initial protocol separation.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: MfCastChannel.pas
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
// =============================================================================
// Source: Google Cast V2 wire protocol
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
unit MfCastProtocol;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinSock,
  {System}
  System.SysUtils,
  System.Classes;

function MfCastProtocolEncodeFrame(const ASourceId: string;
                                   const ADestinationId: string;
                                   const ANamespace: string;
                                   const AJsonPayload: string): TBytes;

function MfCastProtocolDecodeMessage(const AData: TBytes;
                                     out ANamespace: string;
                                     out AJsonPayload: string): HRESULT;

implementation


procedure AppendByte(var AData: TBytes;
                     const AValue: Byte);
var
  Index: Integer;

begin

  Index := Length(AData);
  SetLength(AData,
            Index + 1);
  AData[Index] := AValue;
end;


procedure AppendBytes(var AData: TBytes;
                      const ABytes: TBytes);
var
  OldLength: Integer;

begin

  if (Length(ABytes) = 0) then
    Exit;

  OldLength := Length(AData);

  SetLength(AData,
            OldLength + Length(ABytes));

  Move(ABytes[0],
       AData[OldLength],
             Length(ABytes));
end;


procedure AppendVarUInt(var AData: TBytes; AValue: Cardinal);
begin

  repeat
    if (AValue >= $80) then
      AppendByte(AData,
                 Byte(AValue or $80))
    else
      AppendByte(AData,
                 Byte(AValue));
    AValue := AValue shr 7;
  until AValue = 0;
end;


procedure AppendProtoString(var AData: TBytes;
                            const AFieldNumber: Cardinal;
                            const AValue: string);
var
  Utf8: TBytes;

begin

  Utf8 := TEncoding.UTF8.GetBytes(AValue);
  AppendVarUInt(AData,
                (AFieldNumber shl 3) or 2);

  AppendVarUInt(AData,
                Length(Utf8));

  AppendBytes(AData,
              Utf8);
end;


procedure AppendProtoVarUInt(var AData: TBytes;
                             const AFieldNumber: Cardinal;
                             const AValue: Cardinal);
begin

  AppendVarUInt(AData,
                AFieldNumber shl 3);

  AppendVarUInt(AData,
                AValue);
end;


function ReadVarUInt(const AData: TBytes;
                     var AIndex: Integer;
                     out AValue: Cardinal): Boolean;
var
  Shift: Integer;
  ValueByte: Byte;

begin

  Result := False;
  AValue := 0;
  Shift := 0;

  while (AIndex < Length(AData)) do
    begin

      ValueByte := AData[AIndex];
      Inc(AIndex);

      AValue := AValue or (Cardinal(ValueByte and $7F) shl Shift);

      if ((ValueByte and $80) = 0) then
        begin
          Result := True;
          Exit;
        end;

      Inc(Shift,
          7);
      if (Shift > 28) then
        Exit;
    end;
end;


function ReadProtoString(const AData: TBytes;
                         var AIndex: Integer;
                         out AValue: string): Boolean;
var
  ValueLength: Cardinal;

begin

  Result := False;
  AValue := '';

  if not ReadVarUInt(AData,
                     AIndex,
                     ValueLength) then
    Exit;

  if (ValueLength > Cardinal(Length(AData) - AIndex)) then
    Exit;

  AValue := TEncoding.UTF8.GetString(AData,
                                     AIndex,
                                     Integer(ValueLength));
  Inc(AIndex,
  Integer(ValueLength));
  Result := True;
end;


function MfCastProtocolEncodeFrame(const ASourceId: string;
                                   const ADestinationId: string;
                                   const ANamespace: string;
                                   const AJsonPayload: string): TBytes;
var
  MessageData: TBytes;
  NetworkLength: Cardinal;

begin

  SetLength(MessageData,
            0);

  AppendProtoVarUInt(MessageData,
                     1,
                     0);
  AppendProtoString(MessageData,
                    2,
                    ASourceId);
  AppendProtoString(MessageData,
                    3,
                    ADestinationId);
  AppendProtoString(MessageData,
                    4,
                    ANamespace);
  AppendProtoVarUInt(MessageData,
                     5,
                     0);
  AppendProtoString(MessageData,
                    6,
                    AJsonPayload);

  SetLength(Result,
            Length(MessageData) + 4);
  NetworkLength := htonl(Cardinal(Length(MessageData)));

  Move(NetworkLength,
       Result[0],
       SizeOf(NetworkLength));

  if (Length(MessageData) > 0) then
    Move(MessageData[0],
         Result[4],
         Length(MessageData));
end;


function MfCastProtocolDecodeMessage(const AData: TBytes;
                                     out ANamespace: string;
                                     out AJsonPayload: string): HRESULT;
var
  FieldKey: Cardinal;
  FieldNumber: Cardinal;
  IgnoredString: string;
  IgnoredValue: Cardinal;
  Index: Integer;
  ValueLength: Cardinal;
  WireType: Cardinal;

begin

  ANamespace := '';
  AJsonPayload := '';
  Index := 0;

  while (Index < Length(AData)) do
    begin
      if not ReadVarUInt(AData,
                         Index,
                         FieldKey) then
        begin
          Result := E_FAIL;
          Exit;
        end;

      FieldNumber := FieldKey shr 3;
      WireType := FieldKey and $07;

      case WireType of
        0: if not ReadVarUInt(AData,
                              Index,
                              IgnoredValue) then
             begin
               Result := E_FAIL;
               Exit;
             end;
        1: Inc(Index, 8);
        2: if FieldNumber in [2, 3, 4, 6] then
             begin
               if not ReadProtoString(AData,
                                      Index,
                                      IgnoredString) then
                 begin
                   Result := E_FAIL;
                   Exit;
                 end;

               if (FieldNumber = 4) then
                 ANamespace := IgnoredString
               else
                 if (FieldNumber = 6) then
                   AJsonPayload := IgnoredString;
             end
           else
             begin
               if not ReadVarUInt(AData, Index, ValueLength) then
                 begin
                   Result := E_FAIL;
                   Exit;
                 end;

               Inc(Index,
                   Integer(ValueLength));
             end;
        5: Inc(Index,
               4);
      else
        Result := E_FAIL;
        Exit;
      end;

      if (Index > Length(AData)) then
        begin
          Result := E_FAIL;
          Exit;
        end;
    end;
  Result := S_OK;
end;

end.
