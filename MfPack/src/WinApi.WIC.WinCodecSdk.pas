// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - WIC
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WIC.WinCodecSdk.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
//
// Description: Used by Windows Imaging Component (WIC) API.
//              This module contains interfaces used by codec and metadata handler authors.
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
// Remarks: Requires Windows Vista or later.
//          This API contains the latest version (see sdk version).
//          It is not recommended to use the rtl versions <= Delphi 10.4
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
// Source: wincodecsdk.h
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
unit WinApi.WIC.WinCodecSdk;

interface

uses

  {WinApi}
  WinApi.Windows,
  {WIC}
  WinApi.WIC.WinCodec,
  {ActiveX}
  WinApi.ActiveX.ObjIdlBase,
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.OcIdl,
  WinApi.ActiveX.PropIdl;

  {$MINENUMSIZE 4}

const

  GUID_MetadataFormatUnknown            : TGUID = '{A45E592F-9078-4A7C-ADB5-4EDC4FD61B1F}';
  {$EXTERNALSYM GUID_MetadataFormatUnknown}
  GUID_MetadataFormatIfd                : TGUID = '{537396C6-2D8A-4BB6-9BF8-2F0A8E2A3ADF}';
  {$EXTERNALSYM GUID_MetadataFormatIfd}
  GUID_MetadataFormatSubIfd             : TGUID = '{58A2E128-2DB9-4E57-BB14-5177891ED331}';
  {$EXTERNALSYM GUID_MetadataFormatSubIfd}
  GUID_MetadataFormatExif               : TGUID = '{1C3C4F9D-B84A-467D-9493-36CFBD59EA57}';
  {$EXTERNALSYM GUID_MetadataFormatExif}
  GUID_MetadataFormatGps                : TGUID = '{7134AB8A-9351-44AD-AF62-448DB6B502EC}';
  {$EXTERNALSYM GUID_MetadataFormatGps}
  GUID_MetadataFormatInterop            : TGUID = '{ED686F8E-681F-4C8B-BD41-A8ADDBF6B3FC}';
  {$EXTERNALSYM GUID_MetadataFormatInterop}
  GUID_MetadataFormatApp0               : TGUID = '{79007028-268D-45d6-A3C2-354E6A504BC9}';
  {$EXTERNALSYM GUID_MetadataFormatApp0}
  GUID_MetadataFormatApp1               : TGUID = '{8FD3DFC3-F951-492B-817F-69C2E6D9A5B0}';
  {$EXTERNALSYM GUID_MetadataFormatApp1}
  GUID_MetadataFormatApp13              : TGUID = '{326556A2-F502-4354-9CC0-8E3F48EAF6B5}';
  {$EXTERNALSYM GUID_MetadataFormatApp13}
  GUID_MetadataFormatIPTC               : TGUID = '{4FAB0914-E129-4087-A1D1-BC812D45A7B5}';
  {$EXTERNALSYM GUID_MetadataFormatIPTC}
  GUID_MetadataFormatIRB                : TGUID = '{16100D66-8570-4BB9-B92D-FDA4B23ECE67}';
  {$EXTERNALSYM GUID_MetadataFormatIRB}
  GUID_MetadataFormat8BIMIPTC           : TGUID = '{0010568c-0852-4e6a-b191-5c33ac5b0430}';
  {$EXTERNALSYM GUID_MetadataFormat8BIMIPTC}
  GUID_MetadataFormat8BIMResolutionInfo : TGUID = '{739F305D-81DB-43CB-AC5E-55013EF9F003}';
  {$EXTERNALSYM GUID_MetadataFormat8BIMResolutionInfo}
  GUID_MetadataFormat8BIMIPTCDigest     : TGUID = '{1CA32285-9CCD-4786-8BD8-79539DB6A006}';
  {$EXTERNALSYM GUID_MetadataFormat8BIMIPTCDigest}
  GUID_MetadataFormatXMP                : TGUID = '{BB5ACC38-F216-4CEC-A6C5-5F6E739763A9}';
  {$EXTERNALSYM GUID_MetadataFormatXMP}
  GUID_MetadataFormatThumbnail          : TGUID = '{243dcee9-8703-40ee-8ef0-22a600b8058c}';
  {$EXTERNALSYM GUID_MetadataFormatThumbnail}
  GUID_MetadataFormatChunktEXt          : TGUID = '{568d8936-c0a9-4923-905d-df2b38238fbc}';
  {$EXTERNALSYM GUID_MetadataFormatChunktEXt}
  GUID_MetadataFormatXMPStruct          : TGUID = '{22383CF1-ED17-4E2E-AF17-D85B8F6B30D0}';
  {$EXTERNALSYM GUID_MetadataFormatXMPStruct}
  GUID_MetadataFormatXMPBag             : TGUID = '{833CCA5F-DCB7-4516-806F-6596AB26DCE4}';
  {$EXTERNALSYM GUID_MetadataFormatXMPBag}
  GUID_MetadataFormatXMPSeq             : TGUID = '{63E8DF02-EB6C-456C-A224-B25E794FD648}';
  {$EXTERNALSYM GUID_MetadataFormatXMPSeq}
  GUID_MetadataFormatXMPAlt             : TGUID = '{7B08A675-91AA-481B-A798-4DA94908613B}';
  {$EXTERNALSYM GUID_MetadataFormatXMPAlt}
  GUID_MetadataFormatLSD                : TGUID = '{E256031E-6299-4929-B98D-5AC884AFBA92}';
  {$EXTERNALSYM GUID_MetadataFormatLSD}
  GUID_MetadataFormatIMD                : TGUID = '{BD2BB086-4D52-48DD-9677-DB483E85AE8F}';
  {$EXTERNALSYM GUID_MetadataFormatIMD}
  GUID_MetadataFormatGCE                : TGUID = '{2A25CAD8-DEEB-4C69-A788-0EC2266DCAFD}';
  {$EXTERNALSYM GUID_MetadataFormatGCE}
  GUID_MetadataFormatAPE                : TGUID = '{2E043DC2-C967-4E05-875E-618BF67E85C3}';
  {$EXTERNALSYM GUID_MetadataFormatAPE}
  GUID_MetadataFormatJpegChrominance    : TGUID = '{F73D0DCF-CEC6-4F85-9B0E-1C3956B1BEF7}';
  {$EXTERNALSYM GUID_MetadataFormatJpegChrominance}
  GUID_MetadataFormatJpegLuminance      : TGUID = '{86908007-EDFC-4860-8D4B-4EE6E83E6058}';
  {$EXTERNALSYM GUID_MetadataFormatJpegLuminance}
  GUID_MetadataFormatJpegComment        : TGUID = '{220E5F33-AFD3-474E-9D31-7D4FE730F557}';
  {$EXTERNALSYM GUID_MetadataFormatJpegComment}
  GUID_MetadataFormatGifComment         : TGUID = '{C4B6E0E0-CFB4-4AD3-AB33-9AAD2355A34A}';
  {$EXTERNALSYM GUID_MetadataFormatGifComment}
  GUID_MetadataFormatChunkgAMA          : TGUID = '{F00935A5-1D5D-4CD1-81B2-9324D7ECA781}';
  {$EXTERNALSYM GUID_MetadataFormatChunkgAMA}
  GUID_MetadataFormatChunkbKGD          : TGUID = '{E14D3571-6B47-4DEA-B60A-87CE0A78DFB7}';
  {$EXTERNALSYM GUID_MetadataFormatChunkbKGD}
  GUID_MetadataFormatChunkiTXt          : TGUID = '{C2BEC729-0B68-4B77-AA0E-6295A6AC1814}';
  {$EXTERNALSYM GUID_MetadataFormatChunkiTXt}
  GUID_MetadataFormatChunkcHRM          : TGUID = '{9DB3655B-2842-44B3-8067-12E9B375556A}';
  {$EXTERNALSYM GUID_MetadataFormatChunkcHRM}
  GUID_MetadataFormatChunkhIST          : TGUID = '{C59A82DA-DB74-48A4-BD6A-B69C4931EF95}';
  {$EXTERNALSYM GUID_MetadataFormatChunkhIST}
  GUID_MetadataFormatChunkiCCP          : TGUID = '{EB4349AB-B685-450F-91B5-E802E892536C}';
  {$EXTERNALSYM GUID_MetadataFormatChunkiCCP}
  GUID_MetadataFormatChunksRGB          : TGUID = '{C115FD36-CC6F-4E3F-8363-524B87C6B0D9}';
  {$EXTERNALSYM GUID_MetadataFormatChunksRGB}
  GUID_MetadataFormatChunktIME          : TGUID = '{6B00AE2D-E24B-460A-98B6-878BD03072FD}';
  {$EXTERNALSYM GUID_MetadataFormatChunktIME}
  GUID_MetadataFormatDds                : TGUID = '{4a064603-8c33-4e60-9c29-136231702d08}';
  {$EXTERNALSYM GUID_MetadataFormatDds}
  GUID_MetadataFormatHeif               : TGUID = '{817EF3E1-1288-45F4-A852-260D9E7CCE83}';
  {$EXTERNALSYM GUID_MetadataFormatHeif}
  GUID_MetadataFormatHeifHDR            : TGUID = '{568b8d8a-1e65-438c-8968-d60e1012beb9}';
  {$EXTERNALSYM GUID_MetadataFormatHeifHDR}
  GUID_MetadataFormatWebpANIM           : TGUID = '{6dc4fda6-78e6-4102-ae35-bcfa1edcc78b}';
  {$EXTERNALSYM GUID_MetadataFormatWebpANIM}
  GUID_MetadataFormatWebpANMF           : TGUID = '{43c105ee-b93b-4abb-b003-a08c0d870471}';
  {$EXTERNALSYM GUID_MetadataFormatWebpANMF}
  CLSID_WICUnknownMetadataReader        : TGUID = '{699745c2-5066-4b82-a8e3-d40478dbec8c}';
  {$EXTERNALSYM CLSID_WICUnknownMetadataReader}
  CLSID_WICUnknownMetadataWriter        : TGUID = '{a09cca86-27ba-4f39-9053-121fa4dc08fc}';
  {$EXTERNALSYM CLSID_WICUnknownMetadataWriter}
  CLSID_WICApp0MetadataWriter           : TGUID = '{F3C633A2-46C8-498e-8FBB-CC6F721BBCDE}';
  {$EXTERNALSYM CLSID_WICApp0MetadataWriter}
  CLSID_WICApp0MetadataReader           : TGUID = '{43324B33-A78F-480f-9111-9638AACCC832}';
  {$EXTERNALSYM CLSID_WICApp0MetadataReader}
  CLSID_WICApp1MetadataWriter           : TGUID = '{ee366069-1832-420f-b381-0479ad066f19}';
  {$EXTERNALSYM CLSID_WICApp1MetadataWriter}
  CLSID_WICApp1MetadataReader           : TGUID = '{dde33513-774e-4bcd-ae79-02f4adfe62fc}';
  {$EXTERNALSYM CLSID_WICApp1MetadataReader}
  CLSID_WICApp13MetadataWriter          : TGUID = '{7B19A919-A9D6-49E5-BD45-02C34E4E4CD5}';
  {$EXTERNALSYM CLSID_WICApp13MetadataWriter}
  CLSID_WICApp13MetadataReader          : TGUID = '{AA7E3C50-864C-4604-BC04-8B0B76E637F6}';
  {$EXTERNALSYM CLSID_WICApp13MetadataReader}
  CLSID_WICIfdMetadataReader            : TGUID = '{8f914656-9d0a-4eb2-9019-0bf96d8a9ee6}';
  {$EXTERNALSYM CLSID_WICIfdMetadataReader}
  CLSID_WICIfdMetadataWriter            : TGUID = '{b1ebfc28-c9bd-47a2-8d33-b948769777a7}';
  {$EXTERNALSYM CLSID_WICIfdMetadataWriter}
  CLSID_WICSubIfdMetadataReader         : TGUID = '{50D42F09-ECD1-4B41-B65D-DA1FDAA75663}';
  {$EXTERNALSYM CLSID_WICSubIfdMetadataReader}
  CLSID_WICSubIfdMetadataWriter         : TGUID = '{8ADE5386-8E9B-4F4C-ACF2-F0008706B238}';
  {$EXTERNALSYM CLSID_WICSubIfdMetadataWriter}
  CLSID_WICExifMetadataReader           : TGUID = '{d9403860-297f-4a49-bf9b-77898150a442}';
  {$EXTERNALSYM CLSID_WICExifMetadataReader}
  CLSID_WICExifMetadataWriter           : TGUID = '{c9a14cda-c339-460b-9078-d4debcfabe91}';
  {$EXTERNALSYM CLSID_WICExifMetadataWriter}
  CLSID_WICGpsMetadataReader            : TGUID = '{3697790B-223B-484E-9925-C4869218F17A}';
  {$EXTERNALSYM CLSID_WICGpsMetadataReader}
  CLSID_WICGpsMetadataWriter            : TGUID = '{CB8C13E4-62B5-4C96-A48B-6BA6ACE39C76}';
  {$EXTERNALSYM CLSID_WICGpsMetadataWriter}
  CLSID_WICInteropMetadataReader        : TGUID = '{B5C8B898-0074-459F-B700-860D4651EA14}';
  {$EXTERNALSYM CLSID_WICInteropMetadataReader}
  CLSID_WICInteropMetadataWriter        : TGUID = '{122EC645-CD7E-44D8-B186-2C8C20C3B50F}';
  {$EXTERNALSYM CLSID_WICInteropMetadataWriter}
  CLSID_WICThumbnailMetadataReader      : TGUID = '{fb012959-f4f6-44d7-9d09-daa087a9db57}';
  {$EXTERNALSYM CLSID_WICThumbnailMetadataReader}
  CLSID_WICThumbnailMetadataWriter      : TGUID = '{d049b20c-5dd0-44fe-b0b3-8f92c8e6d080}';
  {$EXTERNALSYM CLSID_WICThumbnailMetadataWriter}
  CLSID_WICIPTCMetadataReader           : TGUID = '{03012959-f4f6-44d7-9d09-daa087a9db57}';
  {$EXTERNALSYM CLSID_WICIPTCMetadataReader}
  CLSID_WICIPTCMetadataWriter           : TGUID = '{1249b20c-5dd0-44fe-b0b3-8f92c8e6d080}';
  {$EXTERNALSYM CLSID_WICIPTCMetadataWriter}
  CLSID_WICIRBMetadataReader            : TGUID = '{D4DCD3D7-B4C2-47D9-A6BF-B89BA396A4A3}';
  {$EXTERNALSYM CLSID_WICIRBMetadataReader}
  CLSID_WICIRBMetadataWriter            : TGUID = '{5C5C1935-0235-4434-80BC-251BC1EC39C6}';
  {$EXTERNALSYM CLSID_WICIRBMetadataWriter}
  CLSID_WIC8BIMIPTCMetadataReader       : TGUID = '{0010668c-0801-4da6-a4a4-826522b6d28f}';
  {$EXTERNALSYM CLSID_WIC8BIMIPTCMetadataReader}
  CLSID_WIC8BIMIPTCMetadataWriter       : TGUID = '{00108226-ee41-44a2-9e9c-4be4d5b1d2cd}';
  {$EXTERNALSYM CLSID_WIC8BIMIPTCMetadataWriter}
  CLSID_WIC8BIMResolutionInfoMetadataReader : TGUID = '{5805137A-E348-4F7C-B3CC-6DB9965A0599}';
  {$EXTERNALSYM CLSID_WIC8BIMResolutionInfoMetadataReader}
  CLSID_WIC8BIMResolutionInfoMetadataWriter : TGUID = '{4ff2fe0e-e74a-4b71-98c4-ab7dc16707ba}';
  {$EXTERNALSYM CLSID_WIC8BIMResolutionInfoMetadataWriter}
  CLSID_WIC8BIMIPTCDigestMetadataReader : TGUID = '{02805F1E-D5AA-415b-82C5-61C033A988A6}';
  {$EXTERNALSYM CLSID_WIC8BIMIPTCDigestMetadataReader}
  CLSID_WIC8BIMIPTCDigestMetadataWriter : TGUID = '{2DB5E62B-0D67-495f-8F9D-C2F0188647AC}';
  {$EXTERNALSYM CLSID_WIC8BIMIPTCDigestMetadataWriter}
  CLSID_WICPngTextMetadataReader        : TGUID = '{4b59afcc-b8c3-408a-b670-89e5fab6fda7}';
  {$EXTERNALSYM CLSID_WICPngTextMetadataReader}
  CLSID_WICPngTextMetadataWriter        : TGUID = '{b5ebafb9-253e-4a72-a744-0762d2685683}';
  {$EXTERNALSYM CLSID_WICPngTextMetadataWriter}
  CLSID_WICXMPMetadataReader            : TGUID = '{72B624DF-AE11-4948-A65C-351EB0829419}';
  {$EXTERNALSYM CLSID_WICXMPMetadataReader}
  CLSID_WICXMPMetadataWriter            : TGUID = '{1765E14E-1BD4-462E-B6B1-590BF1262AC6}';
  {$EXTERNALSYM CLSID_WICXMPMetadataWriter}
  CLSID_WICXMPStructMetadataReader      : TGUID = '{01B90D9A-8209-47F7-9C52-E1244BF50CED}';
  {$EXTERNALSYM CLSID_WICXMPStructMetadataReader}
  CLSID_WICXMPStructMetadataWriter      : TGUID = '{22C21F93-7DDB-411C-9B17-C5B7BD064ABC}';
  {$EXTERNALSYM CLSID_WICXMPStructMetadataWriter}
  CLSID_WICXMPBagMetadataReader         : TGUID = '{E7E79A30-4F2C-4FAB-8D00-394F2D6BBEBE}';
  {$EXTERNALSYM CLSID_WICXMPBagMetadataReader}
  CLSID_WICXMPBagMetadataWriter         : TGUID = '{ED822C8C-D6BE-4301-A631-0E1416BAD28F}';
  {$EXTERNALSYM CLSID_WICXMPBagMetadataWriter}
  CLSID_WICXMPSeqMetadataReader         : TGUID = '{7F12E753-FC71-43D7-A51D-92F35977ABB5}';
  {$EXTERNALSYM CLSID_WICXMPSeqMetadataReader}
  CLSID_WICXMPSeqMetadataWriter         : TGUID = '{6D68D1DE-D432-4B0F-923A-091183A9BDA7}';
  {$EXTERNALSYM CLSID_WICXMPSeqMetadataWriter}
  CLSID_WICXMPAltMetadataReader         : TGUID = '{AA94DCC2-B8B0-4898-B835-000AABD74393}';
  {$EXTERNALSYM CLSID_WICXMPAltMetadataReader}
  CLSID_WICXMPAltMetadataWriter         : TGUID = '{076C2A6C-F78F-4C46-A723-3583E70876EA}';
  {$EXTERNALSYM CLSID_WICXMPAltMetadataWriter}
  CLSID_WICLSDMetadataReader            : TGUID = '{41070793-59E4-479A-A1F7-954ADC2EF5FC}';
  {$EXTERNALSYM CLSID_WICLSDMetadataReader}
  CLSID_WICLSDMetadataWriter            : TGUID = '{73C037E7-E5D9-4954-876A-6DA81D6E5768}';
  {$EXTERNALSYM CLSID_WICLSDMetadataWriter}
  CLSID_WICGCEMetadataReader            : TGUID = '{B92E345D-F52D-41F3-B562-081BC772E3B9}';
  {$EXTERNALSYM CLSID_WICGCEMetadataReader}
  CLSID_WICGCEMetadataWriter            : TGUID = '{AF95DC76-16B2-47F4-B3EA-3C31796693E7}';
  {$EXTERNALSYM CLSID_WICGCEMetadataWriter}
  CLSID_WICIMDMetadataReader            : TGUID = '{7447A267-0015-42C8-A8F1-FB3B94C68361}';
  {$EXTERNALSYM CLSID_WICIMDMetadataReader}
  CLSID_WICIMDMetadataWriter            : TGUID = '{8C89071F-452E-4E95-9682-9D1024627172}';
  {$EXTERNALSYM CLSID_WICIMDMetadataWriter}
  CLSID_WICAPEMetadataReader            : TGUID = '{1767B93A-B021-44EA-920F-863C11F4F768}';
  {$EXTERNALSYM CLSID_WICAPEMetadataReader}
  CLSID_WICAPEMetadataWriter            : TGUID = '{BD6EDFCA-2890-482F-B233-8D7339A1CF8D}';
  {$EXTERNALSYM CLSID_WICAPEMetadataWriter}
  CLSID_WICJpegChrominanceMetadataReader: TGUID = '{50B1904B-F28F-4574-93F4-0BADE82C69E9}';
  {$EXTERNALSYM CLSID_WICJpegChrominanceMetadataReader}
  CLSID_WICJpegChrominanceMetadataWriter: TGUID = '{3FF566F0-6E6B-49D4-96E6-B78886692C62}';
  {$EXTERNALSYM CLSID_WICJpegChrominanceMetadataWriter}
  CLSID_WICJpegLuminanceMetadataReader  : TGUID = '{356F2F88-05A6-4728-B9A4-1BFBCE04D838}';
  {$EXTERNALSYM CLSID_WICJpegLuminanceMetadataReader}
  CLSID_WICJpegLuminanceMetadataWriter  : TGUID = '{1D583ABC-8A0E-4657-9982-A380CA58FB4B}';
  {$EXTERNALSYM CLSID_WICJpegLuminanceMetadataWriter}
  CLSID_WICJpegCommentMetadataReader    : TGUID = '{9f66347C-60C4-4C4D-AB58-D2358685f607}';
  {$EXTERNALSYM CLSID_WICJpegCommentMetadataReader}
  CLSID_WICJpegCommentMetadataWriter    : TGUID = '{E573236F-55B1-4EDA-81EA-9F65DB0290D3}';
  {$EXTERNALSYM CLSID_WICJpegCommentMetadataWriter}
  CLSID_WICGifCommentMetadataReader     : TGUID = '{32557D3B-69DC-4F95-836E-F5972B2F6159}';
  {$EXTERNALSYM CLSID_WICGifCommentMetadataReader}
  CLSID_WICGifCommentMetadataWriter     : TGUID = '{A02797fC-C4AE-418C-AF95-E637C7EAD2A1}';
  {$EXTERNALSYM CLSID_WICGifCommentMetadataWriter}
  CLSID_WICPngGamaMetadataReader        : TGUID = '{3692CA39-E082-4350-9E1F-3704CB083CD5}';
  {$EXTERNALSYM CLSID_WICPngGamaMetadataReader}
  CLSID_WICPngGamaMetadataWriter        : TGUID = '{FF036D13-5D4B-46DD-B10F-106693D9FE4F}';
  {$EXTERNALSYM CLSID_WICPngGamaMetadataWriter}
  CLSID_WICPngBkgdMetadataReader        : TGUID = '{0CE7A4A6-03E8-4A60-9D15-282EF32EE7DA}';
  {$EXTERNALSYM CLSID_WICPngBkgdMetadataReader}
  CLSID_WICPngBkgdMetadataWriter        : TGUID = '{68E3F2FD-31AE-4441-BB6A-FD7047525F90}';
  {$EXTERNALSYM CLSID_WICPngBkgdMetadataWriter}
  CLSID_WICPngItxtMetadataReader        : TGUID = '{AABFB2FA-3E1E-4A8F-8977-5556FB94EA23}';
  {$EXTERNALSYM CLSID_WICPngItxtMetadataReader}
  CLSID_WICPngItxtMetadataWriter        : TGUID = '{31879719-E751-4DF8-981D-68DFF67704ED}';
  {$EXTERNALSYM CLSID_WICPngItxtMetadataWriter}
  CLSID_WICPngChrmMetadataReader        : TGUID = '{F90B5F36-367B-402A-9DD1-BC0FD59D8F62}';
  {$EXTERNALSYM CLSID_WICPngChrmMetadataReader}
  CLSID_WICPngChrmMetadataWriter        : TGUID = '{E23CE3EB-5608-4E83-BCEF-27B1987E51D7}';
  {$EXTERNALSYM CLSID_WICPngChrmMetadataWriter}
  CLSID_WICPngHistMetadataReader        : TGUID = '{877A0BB7-A313-4491-87B5-2E6D0594F520}';
  {$EXTERNALSYM CLSID_WICPngHistMetadataReader}
  CLSID_WICPngHistMetadataWriter        : TGUID = '{8A03E749-672E-446E-BF1F-2C11D233B6FF}';
  {$EXTERNALSYM CLSID_WICPngHistMetadataWriter}
  CLSID_WICPngIccpMetadataReader        : TGUID = '{F5D3E63B-CB0F-4628-A478-6D8244BE36B1}';
  {$EXTERNALSYM CLSID_WICPngIccpMetadataReader}
  CLSID_WICPngIccpMetadataWriter        : TGUID = '{16671E5F-0CE6-4CC4-9768-E89FE5018ADE}';
  {$EXTERNALSYM CLSID_WICPngIccpMetadataWriter}
  CLSID_WICPngSrgbMetadataReader        : TGUID = '{FB40360C-547E-4956-A3B9-D4418859BA66}';
  {$EXTERNALSYM CLSID_WICPngSrgbMetadataReader}
  CLSID_WICPngSrgbMetadataWriter        : TGUID = '{A6EE35C6-87EC-47DF-9F22-1D5AAD840C82}';
  {$EXTERNALSYM CLSID_WICPngSrgbMetadataWriter}
  CLSID_WICPngTimeMetadataReader        : TGUID = '{D94EDF02-EFE5-4F0D-85C8-F5A68B3000B1}';
  {$EXTERNALSYM CLSID_WICPngTimeMetadataReader}
  CLSID_WICPngTimeMetadataWriter        : TGUID = '{1AB78400-B5A3-4D91-8ACE-33FCD1499BE6}';
  {$EXTERNALSYM CLSID_WICPngTimeMetadataWriter}
  CLSID_WICDdsMetadataReader            : TGUID = '{276c88ca-7533-4a86-b676-66b36080d484}';
  {$EXTERNALSYM CLSID_WICDdsMetadataReader}
  CLSID_WICDdsMetadataWriter            : TGUID = '{fd688bbd-31ed-4db7-a723-934927d38367}';
  {$EXTERNALSYM CLSID_WICDdsMetadataWriter}
  CLSID_WICHeifMetadataReader           : TGUID = '{ACDDFC3F-85EC-41BC-BDEF-1BC262E4DB05}';
  {$EXTERNALSYM CLSID_WICHeifMetadataReader}
  CLSID_WICHeifMetadataWriter           : TGUID = '{3AE45E79-40BC-4401-ACE5-DD3CB16E6AFE}';
  {$EXTERNALSYM CLSID_WICHeifMetadataWriter}
  CLSID_WICHeifHDRMetadataReader        : TGUID = '{2438de3d-94d9-4be8-84a8-4de95a575e75}';
  {$EXTERNALSYM CLSID_WICHeifHDRMetadataReader}
  CLSID_WICWebpAnimMetadataReader       : TGUID = '{076f9911-a348-465c-a807-a252f3f2d3de}';
  {$EXTERNALSYM CLSID_WICWebpAnimMetadataReader}
  CLSID_WICWebpAnmfMetadataReader       : TGUID = '{85a10b03-c9f6-439f-be5e-c0fbef67807c}';
  {$EXTERNALSYM CLSID_WICWebpAnmfMetadataReader}

// Enums =======================================================================

type
  PWICMetadataCreationOptions = ^WICMetadataCreationOptions;
  WICMetadataCreationOptions = DWord;
  {$EXTERNALSYM WICMetadataCreationOptions}
const
  WICMetadataCreationDefault      = WICMetadataCreationOptions(0);
  {$EXTERNALSYM WICMetadataCreationDefault}
  WICMetadataCreationAllowUnknown = WICMetadataCreationDefault;
  {$EXTERNALSYM WICMetadataCreationAllowUnknown}
  WICMetadataCreationFailUnknown  = WICMetadataCreationOptions($10000);
  {$EXTERNALSYM WICMetadataCreationFailUnknown}
  WICMetadataCreationMask         = WICMetadataCreationOptions($FFFF0000);
  {$EXTERNALSYM WICMetadataCreationMask}

type
  PWICPersistOptions = ^WICPersistOptions;
  WICPersistOptions = DWord;
  {$EXTERNALSYM WICPersistOptions}
const
  WICPersistOptionDefault       = WICPersistOptions(0);
  {$EXTERNALSYM WICPersistOptionDefault}
  WICPersistOptionLittleEndian  = WICPersistOptions(0);
  {$EXTERNALSYM WICPersistOptionLittleEndian}
  WICPersistOptionBigEndian     = WICPersistOptions($1);
  {$EXTERNALSYM WICPersistOptionBigEndian}
  WICPersistOptionStrictFormat  = WICPersistOptions($2);
  {$EXTERNALSYM WICPersistOptionStrictFormat}
  WICPersistOptionNoCacheStream = WICPersistOptions($4);
  {$EXTERNALSYM WICPersistOptionNoCacheStream}
  WICPersistOptionPreferUTF8    = WICPersistOptions($8);
  {$EXTERNALSYM WICPersistOptionPreferUTF8}
  WICPersistOptionMask          = WICPersistOptions($FFFF);
  {$EXTERNALSYM WICPersistOptionMask}

// =============================================================================
type

  //* Forward Declarations *//

  PIWICMetadataReader = ^IWICMetadataReader;
  IWICMetadataReader = interface;

  PIWICMetadataWriter = ^IWICMetadataWriter;
  IWICMetadataWriter = interface;

  PIWICMetadataHandlerInfo = ^IWICMetadataHandlerInfo;
  IWICMetadataHandlerInfo = interface;


  // Interface IWICMetadataBlockReader
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataBlockReader);'}
  {$EXTERNALSYM IWICMetadataBlockReader}
  IWICMetadataBlockReader = interface (IUnknown)
  ['{FEAA2A8D-B3F3-43E4-B25C-D1DE990A1AE1}']
    function GetContainerFormat(out pguidContainerFormat: TGuid): HResult; stdcall;

    function GetCount(out pcCount: UINT): HResult; stdcall;

    function GetReaderByIndex(nIndex: UINT;
                              out ppIMetadataReader: IWICMetadataReader): HResult; stdcall;

    function GetEnumerator(out ppIEnumMetadata: IEnumUnknown): HResult; stdcall;
  end;
  IID_IWICMetadataBlockReader = IWICMetadataBlockReader;
  {$EXTERNALSYM IID_IWICMetadataBlockReader}


  // Interface IWICMetadataBlockWriter
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataBlockWriter);'}
  {$EXTERNALSYM IWICMetadataBlockWriter}
  IWICMetadataBlockWriter = interface(IWICMetadataBlockReader)
  ['{08FB9676-B444-41E8-8DBE-6A53A542BFF1}']
    function InitializeFromBlockReader(pIMDBlockReader: IWICMetadataBlockReader): HResult; stdcall;

    function GetWriterByIndex(nIndex: UINT;
                              out ppIMetadataWriter: IWICMetadataWriter): HResult; stdcall;

    function AddWriter(pIMetadataWriter: IWICMetadataWriter): HResult; stdcall;

    function SetWriterByIndex(nIndex: UINT;
                              pIMetadataWriter: IWICMetadataWriter): HResult; stdcall;

    function RemoveWriterByIndex(nIndex: UINT): HResult; stdcall;
  end;
  IID_IWICMetadataBlockWriter = IWICMetadataBlockWriter;
  {$EXTERNALSYM IID_IWICMetadataBlockWriter}


  // Interface IWICMetadataReader
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataReader);'}
  {$EXTERNALSYM IWICMetadataReader}
  IWICMetadataReader = interface (IUnknown)
  ['{9204FE99-D8FC-4FD5-A001-9536B067A899}']
    function GetMetadataFormat(out pguidMetadataFormat: TGuid): HResult; stdcall;

    function GetMetadataHandlerInfo(out ppIHandler: IWICMetadataHandlerInfo): HResult; stdcall;

    function GetCount(out pcCount: UINT): HResult; stdcall;

    function GetValueByIndex(nIndex: UINT;
                             var pvarSchema: PROPVARIANT;
                             var pvarId: PROPVARIANT;
                             var pvarValue: PROPVARIANT): HResult; stdcall;

    function GetValue(const pvarSchema: PROPVARIANT;
                      const pvarId: PROPVARIANT;
                      var pvarValue: PROPVARIANT): HResult; stdcall;

    function GetEnumerator(out ppIEnumMetadata: IWICEnumMetadataItem): HResult; stdcall;
  end;
  IID_IWICMetadataReader = IWICMetadataReader;
  {$EXTERNALSYM IID_IWICMetadataReader}



  // Interface IWICMetadataWriter
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataWriter);'}
  {$EXTERNALSYM IWICMetadataWriter}
  IWICMetadataWriter = interface (IWICMetadataReader)
  ['{F7836E16-3BE0-470B-86BB-160D0AECD7DE}']
    function SetValue(const pvarSchema: PROPVARIANT;
                      const pvarId: PROPVARIANT;
                      const pvarValue: PROPVARIANT): HResult; stdcall;

    function SetValueByIndex(const pvarSchema: PROPVARIANT;
                             const pvarId: PROPVARIANT;
                             const pvarValue: PROPVARIANT): HResult; stdcall;

    function RemoveValue(const pvarSchema: PROPVARIANT;
                         const pvarId: PROPVARIANT): HResult; stdcall;

    function RemoveValueByIndex(nIndex: UINT): HResult; stdcall;
  end;
  IID_IWICMetadataWriter = IWICMetadataWriter;
  {$EXTERNALSYM IID_IWICMetadataWriter}



  // Interface IWICStreamProvider
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICStreamProvider);'}
  {$EXTERNALSYM IWICStreamProvider}
  IWICStreamProvider = interface (IUnknown)
  ['{449494BC-B468-4927-96D7-BA90D31AB505}']
    function GetStream(out ppIStream: IStream): HResult; stdcall;

    function GetPersistOptions(out pdwPersistOptions: DWORD): HResult; stdcall;

    function GetPreferredVendorGUID(out pguidPreferredVendor: TGuid): HResult; stdcall;

    function RefreshStream(): HResult; stdcall;
  end;
  IID_IWICStreamProvider = IWICStreamProvider;
  {$EXTERNALSYM IID_IWICStreamProvider}



  // Interface IWICPersistStream
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPersistStream);'}
  {$EXTERNALSYM IWICPersistStream}
  IWICPersistStream = interface(IPersistStream)
  ['{00675040-6908-45F8-86A3-49C7DFD6D9AD}']
    function LoadEx(pIStream: IStream;
                    const pguidPreferredVendor: TGuid;
                    dwPersistOptions: DWORD): HResult; stdcall;

    function SaveEx(pIStream: IStream;
                    dwPersistOptions: DWORD;
                    fClearDirty: BOOL): HResult; stdcall;
  end;
  IID_IWICPersistStream = IWICPersistStream;
  {$EXTERNALSYM IID_IWICPersistStream}


  // Interface IWICMetadataHandlerInfo
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataHandlerInfo);'}
  {$EXTERNALSYM IWICMetadataHandlerInfo}
  IWICMetadataHandlerInfo = interface(IWICComponentInfo)
  ['{ABA958BF-C672-44D1-8D61-CE6DF2E682C2}']
    function GetMetadataFormat(out pguidMetadataFormat: TGuid): HResult; stdcall;

    function GetContainerFormats(cContainerFormats: UINT;  //The size of the pguidContainerFormats array.
                                 var pguidContainerFormats: PGuid;  // pointer to array of Guids
                                 out pcchActual: UINT  // The actual number of GUIDs added to the array.
                                 ): HResult; stdcall;

    function GetDeviceManufacturer(cchDeviceManufacturer: UINT;
                                   var wzDeviceManufacturer: PWideChar;
                                   out pcchActual: UINT): HResult; stdcall;

    function GetDeviceModels(cchDeviceModels: UINT;
                             var wzDeviceModels: PWideChar;
                             out pcchActual: UINT): HResult; stdcall;

    function DoesRequireFullStream(out pfRequiresFullStream: BOOL): HResult; stdcall;

    function DoesSupportPadding(out pfSupportsPadding: BOOL): HResult; stdcall;

    function DoesRequireFixedSize(out pfFixedSize: BOOL): HResult; stdcall;
  end;
  IID_IWICMetadataHandlerInfo = IWICMetadataHandlerInfo;
  {$EXTERNALSYM IID_IWICMetadataHandlerInfo}


  PWICMetadataPattern = ^WICMetadataPattern;
  WICMetadataPattern = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Pattern: PByte;
    Mask: PByte;
    DataOffset: ULARGE_INTEGER;
  end;
  {$EXTERNALSYM WICMetadataPattern}


  // Interface IWICMetadataReaderInfo
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataReaderInfo);'}
  {$EXTERNALSYM IWICMetadataReaderInfo}
  IWICMetadataReaderInfo = interface(IWICMetadataHandlerInfo)
  ['{EEBF1F5B-07C1-4447-A3AB-22ACAF78A804}']
    function GetPatterns(guidContainerFormat: TGUID;
                         cbSize: UINT;
                         var  pPattern: PWICMetadataPattern;
                         out pcCount: UINT;
                         out pcbActual: UINT): HResult; stdcall;

    function MatchesPattern(guidContainerFormat: TGUID;
                            pIStream: IStream;
                            out pfMatches: BOOL): HResult; stdcall;

    function CreateInstance(out ppIReader: IWICMetadataReader): HResult; stdcall;
  end;
  IID_IWICMetadataReaderInfo = IWICMetadataReaderInfo;
  {$EXTERNALSYM IID_IWICMetadataReaderInfo}



  PWICMetadataHeader = ^WICMetadataHeader;
  WICMetadataHeader = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Header: PByte;
    DataOffset: ULARGE_INTEGER;
  end;
  {$EXTERNALSYM WICMetadataHeader}


  // Interface IWICMetadataWriterInfo
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataWriterInfo);'}
  {$EXTERNALSYM IWICMetadataWriterInfo}
  IWICMetadataWriterInfo = interface(IWICMetadataHandlerInfo)
  ['{B22E3FBA-3925-4323-B5C1-9EBFC430F236}']
    function GetHeader(const guidContainerFormat: TGUID;
                       cbSize: UINT;
                       var pHeader: WICMetadataHeader;
                       var pcbActual: UINT): HResult; stdcall;

    function CreateInstance(out ppIWriter: IWICMetadataWriter): HResult; stdcall;
  end;
  IID_IWICMetadataWriterInfo = IWICMetadataWriterInfo;
  {$EXTERNALSYM IID_IWICMetadataWriterInfo}


  // Interface IWICComponentFactory
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICComponentFactory);'}
  {$EXTERNALSYM IWICComponentFactory}
  IWICComponentFactory = interface(IWICImagingFactory)
  ['{412D0C3A-9650-44FA-AF5B-DD2A06C8E8FB}']
    function CreateMetadataReader(const guidMetadataFormat: TGUID;
                                  const pguidVendor: TGUID;
                                  dwOptions: DWORD;
                                  pIStream: IStream;
                                  out ppIReader: IWICMetadataReader): HResult; stdcall;

    function CreateMetadataReaderFromContainer(const guidContainerFormat: TGUID;
                                               const pguidVendor: TGUID;
                                               dwOptions: DWORD;
                                               pIStream: IStream;
                                               out ppIReader: IWICMetadataReader): HResult; stdcall;

    function CreateMetadataWriter(const guidMetadataFormat: TGUID;
                                  const pguidVendor: TGUID;
                                  dwMetadataOptions: DWORD;
                                  out ppIWriter: IWICMetadataWriter): HResult; stdcall;

    function CreateMetadataWriterFromReader(pIReader: IWICMetadataReader;
                                            const pguidVendor: TGuid;
                                            out ppIWriter: IWICMetadataWriter): HResult; stdcall;

    function CreateQueryReaderFromBlockReader(pIBlockReader: IWICMetadataBlockReader;
                                              out ppIQueryReader: IWICMetadataQueryReader): HResult; stdcall;

    function CreateQueryWriterFromBlockWriter(pIBlockWriter: IWICMetadataBlockWriter;
                                              out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;

    function CreateEncoderPropertyBag(ppropOptions: PPROPBAG2;
                                      cCount: UINT;
                                      out ppIPropertyBag: IPropertyBag2): HResult; stdcall;
  end;
  IID_IWICComponentFactory = IWICComponentFactory;
  {$EXTERNALSYM IID_IWICComponentFactory}



  function WICMatchMetadataContent(const guidContainerFormat: TGUID;
                                   const pguidVendor: TGUID;
                                   pIStream: IStream;
                                   out pguidMetadataFormat: TGUID): HResult; stdcall;
  {$EXTERNALSYM WICMatchMetadataContent}

  function WICSerializeMetadataContent(const guidContainerFormat: TGUID;
                                       pIWriter: IWICMetadataWriter;
                                       dwPersistOptions: DWORD;
                                       pIStream: IStream): HResult; stdcall;
  {$EXTERNALSYM WICSerializeMetadataContent}

  function WICGetMetadataContentSize(const guidContainerFormat: TGUID;
                                     pIWriter: IWICMetadataWriter;
                                     out pcbSize: ULARGE_INTEGER): HResult; stdcall;
  {$EXTERNALSYM WICGetMetadataContentSize}


  //* Additional Prototypes for ALL interfaces *//


  //* end of Additional Prototypes *//


implementation

const
  WinCodecLib = 'Windowscodecs.dll';

{$WARN SYMBOL_PLATFORM OFF}

function WICMatchMetadataContent;      external WinCodecLib name 'WICMatchMetadataContent' delayed;
function WICSerializeMetadataContent;  external WinCodecLib name 'WICSerializeMetadataContent' delayed;
function WICGetMetadataContentSize;    external WinCodecLib name 'WICGetMetadataContentSize' delayed;

{$WARN SYMBOL_PLATFORM ON}

  //Implement Additional functions here.

end.
