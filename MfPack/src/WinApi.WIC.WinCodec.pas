// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MFPack - DirectComposition
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WIC.WinCodec.pas
// Kind: Pascal / Delphi unit
// Release date: 30-04-2019
// Language: ENU
//
// Revision Version: 3.0.0
//
// Description: Windows Imaging Component (WIC) API.
//              This module contains the public data structures and API definitions
//              needed for the Windows still image codecs.
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
//          This API contains the latest version (see SDK version).
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
// Source: wincodec.h
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
unit WinApi.WIC.WinCodec;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.ObjIdlBase,
  WinApi.ActiveX.OcIdl,
  {DirectX}
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.DXGIType,
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1_1,
  WinApi.DirectX.D2DErr;

  {$MINENUMSIZE 4}


const

  WINCODEC_SDK_VERSION1               = $0236;
  {$EXTERNALSYM WINCODEC_SDK_VERSION1}

  WINCODEC_SDK_VERSION2               = $0237;
  {$EXTERNALSYM WINCODEC_SDK_VERSION2}

  CLSID_WICImagingFactory1     : TGUID = '{cacaf262-9370-4615-a13b-9f5539da4c0a}';
  {$EXTERNALSYM CLSID_WICImagingFactory1}
  CLSID_WICImagingFactory2     : TGUID = '{317d06e8-5f24-433d-bdf7-79ce68d8abc2}';
  {$EXTERNALSYM CLSID_WICImagingFactory2}

//#if(_WIN32_WINNT >= _WIN32_WINNT_WIN8) or defined(_WIN7_PLATFORM_UPDATE)
  WINCODEC_SDK_VERSION         = WINCODEC_SDK_VERSION2;

var  CLSID_WICImagingFactory      : TGUID absolute CLSID_WICImagingFactory2;
  {$EXTERNALSYM CLSID_WICImagingFactory}
//#else
// const WINCODEC_SDK_VERSION                = WINCODEC_SDK_VERSION1;
//#endif

const

  GUID_VendorMicrosoft         : TGUID = '{f0e749ca-edef-4589-a73a-ee0e626a2a2b}';
  {$EXTERNALSYM GUID_VendorMicrosoft}
  GUID_VendorMicrosoftBuiltIn  : TGUID = '{257a30fd-06b6-462b-aea4-63f70b86e533}';
  {$EXTERNALSYM GUID_VendorMicrosoftBuiltIn}

  CLSID_WICPngDecoder1         : TGUID = '{389ea17b-5078-4cde-b6ef-25c15175c751}';
  {$EXTERNALSYM CLSID_WICPngDecoder1}
  CLSID_WICPngDecoder2         : TGUID = '{e018945b-aa86-4008-9bd4-6777a1e40c11}';
  {$EXTERNALSYM CLSID_WICPngDecoder2}

//#if(_WIN32_WINNT >= _WIN32_WINNT_WIN8) or defined(_WIN7_PLATFORM_UPDATE)
var CLSID_WICPngDecoder : TGUID absolute CLSID_WICPngDecoder2;
  {$EXTERNALSYM CLSID_WICPngDecoder}
//#endif

const

  CLSID_WICBmpDecoder          : TGUID = '{6b462062-7cbf-400d-9fdb-813dd10f2778}';
  {$EXTERNALSYM CLSID_WICBmpDecoder}
  CLSID_WICIcoDecoder          : TGUID = '{c61bfcdf-2e0f-4aad-a8d7-e06bafebcdfe}';
  {$EXTERNALSYM CLSID_WICIcoDecoder}
  CLSID_WICJpegDecoder         : TGUID = '{9456a480-e88b-43ea-9e73-0b2d9b71b1ca}';
  {$EXTERNALSYM CLSID_WICJpegDecoder}
  CLSID_WICGifDecoder          : TGUID = '{381dda3c-9ce9-4834-a23e-1f98f8fc52be}';
  {$EXTERNALSYM CLSID_WICGifDecoder}
  CLSID_WICTiffDecoder         : TGUID = '{b54e85d9-fe23-499f-8b88-6acea713752b}';
  {$EXTERNALSYM CLSID_WICTiffDecoder}
  CLSID_WICWmpDecoder          : TGUID = '{a26cec36-234c-4950-ae16-e34aace71d0d}';
  {$EXTERNALSYM CLSID_WICWmpDecoder}
  CLSID_WICDdsDecoder          : TGUID = '{9053699f-a341-429d-9e90-ee437cf80c73}';
  {$EXTERNALSYM CLSID_WICDdsDecoder}
  CLSID_WICBmpEncoder          : TGUID = '{69be8bb4-d66d-47c8-865a-ed1589433782}';
  {$EXTERNALSYM CLSID_WICBmpEncoder}
  CLSID_WICPngEncoder          : TGUID = '{27949969-876a-41d7-9447-568f6a35a4dc}';
  {$EXTERNALSYM CLSID_WICPngEncoder}
  CLSID_WICJpegEncoder         : TGUID = '{1a34f5c1-4a5a-46dc-b644-1f4567e7a676}';
  {$EXTERNALSYM CLSID_WICJpegEncoder}
  CLSID_WICGifEncoder          : TGUID = '{114f5598-0b22-40a0-86a1-c83ea495adbd}';
  {$EXTERNALSYM CLSID_WICGifEncoder}
  CLSID_WICTiffEncoder         : TGUID = '{0131be10-2001-4c5f-a9b0-cc88fab64ce8}';
  {$EXTERNALSYM CLSID_WICTiffEncoder}
  CLSID_WICWmpEncoder          : TGUID = '{ac4ce3cb-e1c1-44cd-8215-5a1665509ec2}';
  {$EXTERNALSYM CLSID_WICWmpEncoder}
  CLSID_WICDdsEncoder          : TGUID = '{a61dde94-66ce-4ac1-881b-71680588895e}';
  {$EXTERNALSYM CLSID_WICDdsEncoder}
  CLSID_WICAdngDecoder         : TGUID = '{981d9411-909e-42a7-8f5d-a747ff052edb}';
  {$EXTERNALSYM CLSID_WICAdngDecoder}
  CLSID_WICJpegQualcommPhoneEncoder  : TGUID = '{68ed5c62-f534-4979-b2b3-686a12b2b34c}';
  {$EXTERNALSYM CLSID_WICJpegQualcommPhoneEncoder}
  CLSID_WICHeifDecoder         : TGUID = '{e9A4A80a-44fe-4DE4-8971-7150B10a5199}';
  {$EXTERNALSYM CLSID_WICHeifDecoder}
  CLSID_WICHeifEncoder         : TGUID = '{0dbecec1-9eb3-4860-9c6f-ddbe86634575}';
  {$EXTERNALSYM CLSID_WICHeifEncoder}
  CLSID_WICWebpDecoder         : TGUID = '{7693E886-51C9-4070-8419-9F70738EC8FA}';
  {$EXTERNALSYM CLSID_WICWebpDecoder}
  CLSID_WICRAWDecoder          : TGUID = '{41945702-8302-44A6-9445-AC98E8AFA086}';
  {$EXTERNALSYM CLSID_WICRAWDecoder}

  GUID_ContainerFormatBmp      : TGUID = '{0af1d87e-fcfe-4188-bdeb-a7906471cbe3}';
  {$EXTERNALSYM GUID_ContainerFormatBmp}
  GUID_ContainerFormatPng      : TGUID = '{1b7cfaf4-713f-473c-bbcd-6137425faeaf}';
  {$EXTERNALSYM GUID_ContainerFormatPng}
  GUID_ContainerFormatIco      : TGUID = '{a3a860c4-338f-4c17-919a-fba4b5628f21}';
  {$EXTERNALSYM GUID_ContainerFormatIco}
  GUID_ContainerFormatJpeg     : TGUID = '{19e4a5aa-5662-4fc5-a0c0-1758028e1057}';
  {$EXTERNALSYM GUID_ContainerFormatJpeg}
  GUID_ContainerFormatTiff     : TGUID = '{163bcc30-e2e9-4f0b-961d-a3e9fdb788a3}';
  {$EXTERNALSYM GUID_ContainerFormatTiff}
  GUID_ContainerFormatGif      : TGUID = '{1f8a5601-7d4d-4cbd-9c82-1bc8d4eeb9a5}';
  {$EXTERNALSYM GUID_ContainerFormatGif}
  GUID_ContainerFormatWmp      : TGUID = '{57a37caa-367a-4540-916b-f183c5093a4b}';
  {$EXTERNALSYM GUID_ContainerFormatWmp}
  GUID_ContainerFormatDds      : TGUID = '{9967cb95-2e85-4ac8-8ca2-83d7ccd425c9}';
  {$EXTERNALSYM GUID_ContainerFormatDds}
  GUID_ContainerFormatAdng     : TGUID = '{f3ff6d0d-38c0-41c4-b1fe-1f3824f17b84}';
  {$EXTERNALSYM GUID_ContainerFormatAdng}
  GUID_ContainerFormatHeif     : TGUID = '{e1e62521-6787-405b-a339-500715b5763f}';
  {$EXTERNALSYM GUID_ContainerFormatHeif}
  GUID_ContainerFormatWebp     : TGUID = '{e094b0e2-67f2-45b3-b0ea-115337ca7cf3}';
  {$EXTERNALSYM GUID_ContainerFormatWebp}
  GUID_ContainerFormatRaw      : TGUID = '{fe99ce60-f19c-433c-a3ae-00acefa9ca21}';
  {$EXTERNALSYM GUID_ContainerFormatRaw}

  CLSID_WICImagingCategories   : TGUID = '{fae3d380-fea4-4623-8c75-c6b61110b681}';
  {$EXTERNALSYM CLSID_WICImagingCategories}

  CATID_WICBitmapDecoders      : TGUID = '{7ed96837-96f0-4812-b211-f13c24117ed3}';
  {$EXTERNALSYM CATID_WICBitmapDecoders}
  CATID_WICBitmapEncoders      : TGUID = '{ac757296-3522-4e11-9862-c17be5a1767e}';
  {$EXTERNALSYM CATID_WICBitmapEncoders}
  CATID_WICPixelFormats        : TGUID = '{2b46e70f-cda7-473e-89f6-dc9630a2390b}';
  {$EXTERNALSYM CATID_WICPixelFormats}
  CATID_WICFormatConverters    : TGUID = '{7835eae8-bf14-49d1-93ce-533a407b2248}';
  {$EXTERNALSYM CATID_WICFormatConverters}
  CATID_WICMetadataReader      : TGUID = '{05af94d8-7174-4cd2-be4a-4124b80ee4b8}';
  {$EXTERNALSYM CATID_WICMetadataReader}
  CATID_WICMetadataWriter      : TGUID = '{abe3b9a4-257d-4b97-bd1a-294af496222e}';
  {$EXTERNALSYM CATID_WICMetadataWriter}

  CLSID_WICDefaultFormatConverter    : TGUID = '{1a3f11dc-b514-4b17-8c5f-2154513852f1}';
  {$EXTERNALSYM CLSID_WICDefaultFormatConverter}
  CLSID_WICFormatConverterHighColor  : TGUID = '{ac75d454-9f37-48f8-b972-4e19bc856011}';
  {$EXTERNALSYM CLSID_WICFormatConverterHighColor}
  CLSID_WICFormatConverterNChannel   : TGUID = '{c17cabb2-d4a3-47d7-a557-339b2efbd4f1}';
  {$EXTERNALSYM CLSID_WICFormatConverterNChannel}
  CLSID_WICFormatConverterWMPhoto    : TGUID = '{9cb5172b-d600-46ba-ab77-77bb7e3a00d9}';
  {$EXTERNALSYM CLSID_WICFormatConverterWMPhoto}
  CLSID_WICPlanarFormatConverter     : TGUID = '{184132b8-32f8-4784-9131-dd7224b23438}';
  {$EXTERNALSYM CLSID_WICPlanarFormatConverter}

  WIC_JPEG_MAX_COMPONENT_COUNT        = 4;
  {$EXTERNALSYM WIC_JPEG_MAX_COMPONENT_COUNT}
  WIC_JPEG_MAX_TABLE_INDEX            = 3;
  {$EXTERNALSYM WIC_JPEG_MAX_TABLE_INDEX}

  WIC_JPEG_SAMPLE_FACTORS_ONE         = $11;
  {$EXTERNALSYM WIC_JPEG_SAMPLE_FACTORS_ONE}
  WIC_JPEG_SAMPLE_FACTORS_THREE_420   = $111122;
  {$EXTERNALSYM WIC_JPEG_SAMPLE_FACTORS_THREE_420}
  WIC_JPEG_SAMPLE_FACTORS_THREE_422   = $111121;
  {$EXTERNALSYM WIC_JPEG_SAMPLE_FACTORS_THREE_422}
  WIC_JPEG_SAMPLE_FACTORS_THREE_440   = $111112;
  {$EXTERNALSYM WIC_JPEG_SAMPLE_FACTORS_THREE_440}
  WIC_JPEG_SAMPLE_FACTORS_THREE_444   = $111111;
  {$EXTERNALSYM WIC_JPEG_SAMPLE_FACTORS_THREE_444}

  WIC_JPEG_QUANTIZATION_BASELINE_ONE  = 0;
  {$EXTERNALSYM WIC_JPEG_QUANTIZATION_BASELINE_ONE}
  WIC_JPEG_QUANTIZATION_BASELINE_THREE= $10100;
  {$EXTERNALSYM WIC_JPEG_QUANTIZATION_BASELINE_THREE}

  WIC_JPEG_HUFFMAN_BASELINE_ONE       = 0;
  {$EXTERNALSYM WIC_JPEG_HUFFMAN_BASELINE_ONE}
  WIC_JPEG_HUFFMAN_BASELINE_THREE     = $111100;
  {$EXTERNALSYM WIC_JPEG_HUFFMAN_BASELINE_THREE}


  GUID_WICPixelFormatDontCare         : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc900}';
  {$EXTERNALSYM GUID_WICPixelFormatDontCare}
var GUID_WICPixelFormatUndefined      : TGUID absolute GUID_WICPixelFormatDontCare;
  {$EXTERNALSYM GUID_WICPixelFormatUndefined}

const

  GUID_WICPixelFormat1bppIndexed    : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc901}';
  {$EXTERNALSYM GUID_WICPixelFormat1bppIndexed}
  GUID_WICPixelFormat2bppIndexed    : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc902}';
  {$EXTERNALSYM GUID_WICPixelFormat2bppIndexed}
  GUID_WICPixelFormat4bppIndexed    : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc903}';
  {$EXTERNALSYM GUID_WICPixelFormat4bppIndexed}
  GUID_WICPixelFormat8bppIndexed    : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc904}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppIndexed}
  GUID_WICPixelFormatBlackWhite     : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc905}';
  {$EXTERNALSYM GUID_WICPixelFormatBlackWhite}
  GUID_WICPixelFormat2bppGray       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc906}';
  {$EXTERNALSYM GUID_WICPixelFormat2bppGray}
  GUID_WICPixelFormat4bppGray       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc907}';
  {$EXTERNALSYM GUID_WICPixelFormat4bppGray}
  GUID_WICPixelFormat8bppGray       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc908}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppGray}
  GUID_WICPixelFormat8bppAlpha      : TGUID = '{e6cd0116-eeba-4161-aa85-27dd9fb3a895}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppAlpha}
  GUID_WICPixelFormat16bppBGR555    : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc909}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppBGR555}
  GUID_WICPixelFormat16bppBGR565    : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc90a}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppBGR565}
  GUID_WICPixelFormat16bppBGRA5551  : TGUID = '{05ec7c2b-f1e6-4961-ad46-e1cc810a87d2}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppBGRA5551}
  GUID_WICPixelFormat16bppGray      : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc90b}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppGray}
  GUID_WICPixelFormat24bppBGR       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc90c}';
  {$EXTERNALSYM GUID_WICPixelFormat24bppBGR}
  GUID_WICPixelFormat24bppRGB       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc90d}';
  {$EXTERNALSYM GUID_WICPixelFormat24bppRGB}
  GUID_WICPixelFormat32bppBGR       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc90e}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppBGR}
  GUID_WICPixelFormat32bppBGRA      : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc90f}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppBGRA}
  GUID_WICPixelFormat32bppPBGRA     : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc910}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppPBGRA}
  GUID_WICPixelFormat32bppGrayFloat : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc911}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppGrayFloat}
//#if (_WIN32_WINNT >= _WIN32_WINNT_WIN8) || defined(_WIN7_PLATFORM_UPDATE)
  GUID_WICPixelFormat32bppRGB       : TGUID = '{d98c6b95-3efe-47d6-bb25-eb1748ab0cf1}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGB}
//#endif
  GUID_WICPixelFormat32bppRGBA      : TGUID = '{f5c7ad2d-6a8d-43dd-a7a8-a29935261ae9}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBA}
  GUID_WICPixelFormat32bppPRGBA     : TGUID = '{3cc4a650-a527-4d37-a916-3142c7ebedba}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppPRGBA}
  GUID_WICPixelFormat48bppRGB       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc915}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppRGB}
  GUID_WICPixelFormat48bppBGR       : TGUID = '{e605a384-b468-46ce-bb2e-36f180e64313}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppBGR}
//#if (_WIN32_WINNT >= _WIN32_WINNT_WIN8) || defined(_WIN7_PLATFORM_UPDATE)
  GUID_WICPixelFormat64bppRGB       : TGUID = '{a1182111-186d-4d42-bc6a-9c8303a8dff9}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGB}
//#endif
  GUID_WICPixelFormat64bppRGBA      : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc916}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBA}
  GUID_WICPixelFormat64bppBGRA      : TGUID = '{1562ff7c-d352-46f9-979e-42976b792246}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppBGRA}
  GUID_WICPixelFormat64bppPRGBA     : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc917}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppPRGBA}
  GUID_WICPixelFormat64bppPBGRA     : TGUID = '{8c518e8e-a4ec-468b-ae70-c9a35a9c5530}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppPBGRA}
  GUID_WICPixelFormat16bppGrayFixedPoint  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc913}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppGrayFixedPoint}
  GUID_WICPixelFormat32bppBGR101010       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc914}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppBGR101010}
  GUID_WICPixelFormat48bppRGBFixedPoint   : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc912}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppRGBFixedPoint}
  GUID_WICPixelFormat48bppBGRFixedPoint   : TGUID = '{49ca140e-cab6-493b-9ddf-60187c37532a}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppBGRFixedPoint}
  GUID_WICPixelFormat96bppRGBFixedPoint   : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc918}';
  {$EXTERNALSYM GUID_WICPixelFormat96bppRGBFixedPoint}
//#if (_WIN32_WINNT >= _WIN32_WINNT_WIN8) || defined(_WIN7_PLATFORM_UPDATE)
  GUID_WICPixelFormat96bppRGBFloat        : TGUID = '{e3fed78f-e8db-4acf-84c1-e97f6136b327}';
  {$EXTERNALSYM GUID_WICPixelFormat96bppRGBFloat}
//#endif
  GUID_WICPixelFormat128bppRGBAFloat       : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc919}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBAFloat}
  GUID_WICPixelFormat128bppPRGBAFloat      : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc91a}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppPRGBAFloat}
  GUID_WICPixelFormat128bppRGBFloat        : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc91b}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBFloat}
  GUID_WICPixelFormat32bppCMYK             : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc91c}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppCMYK}
  GUID_WICPixelFormat64bppRGBAFixedPoint   : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc91d}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBAFixedPoint}
  GUID_WICPixelFormat64bppBGRAFixedPoint   : TGUID = '{356de33c-54d2-4a23-bb04-9b7bf9b1d42d}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppBGRAFixedPoint}
  GUID_WICPixelFormat64bppRGBFixedPoint    : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc940}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBFixedPoint}
  GUID_WICPixelFormat128bppRGBAFixedPoint  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc91e}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBAFixedPoint}
  GUID_WICPixelFormat128bppRGBFixedPoint   : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc941}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBFixedPoint}
  GUID_WICPixelFormat64bppRGBAHalf         : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc93a}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBAHalf}
//#if (_WIN32_WINNT >= _WIN32_WINNT_WIN8) || defined(_WIN7_PLATFORM_UPDATE)
  GUID_WICPixelFormat64bppPRGBAHalf        : TGUID = '{58ad26c2-c623-4d9d-b320-387e49f8c442}';
//#endif
  GUID_WICPixelFormat64bppRGBHalf          : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc942}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBHalf}
  GUID_WICPixelFormat48bppRGBHalf          : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc93b}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppRGBHalf}
  GUID_WICPixelFormat32bppRGBE             : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc93d}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBE}
  GUID_WICPixelFormat16bppGrayHalf         : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc93e}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppGrayHalf}
  GUID_WICPixelFormat32bppGrayFixedPoint   : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc93f}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppGrayFixedPoint}
  GUID_WICPixelFormat32bppRGBA1010102      : TGUID = '{25238D72-FCF9-4522-b514-5578e5ad55e0}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBA1010102}
  GUID_WICPixelFormat32bppRGBA1010102XR    : TGUID = '{00DE6B9A-C101-434b-b502-d0165ee1122c}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBA1010102XR}
  GUID_WICPixelFormat32bppR10G10B10A2      : TGUID = '{604e1bb5-8a3c-4b65-b11c-bc0b8dd75b7f}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppR10G10B10A2}
  GUID_WICPixelFormat32bppR10G10B10A2HDR10 : TGUID = '{9c215c5d-1acc-4f0e-a4bc-70fb3ae8fd28}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppR10G10B10A2HDR10}
  GUID_WICPixelFormat64bppCMYK             : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc91f}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppCMYK}
  GUID_WICPixelFormat24bpp3Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc920}';
  {$EXTERNALSYM GUID_WICPixelFormat24bpp3Channels}
  GUID_WICPixelFormat32bpp4Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc921}';
  {$EXTERNALSYM GUID_WICPixelFormat32bpp4Channels}
  GUID_WICPixelFormat40bpp5Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc922}';
  {$EXTERNALSYM GUID_WICPixelFormat40bpp5Channels}
  GUID_WICPixelFormat48bpp6Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc923}';
  {$EXTERNALSYM GUID_WICPixelFormat48bpp6Channels}
  GUID_WICPixelFormat56bpp7Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc924}';
  {$EXTERNALSYM GUID_WICPixelFormat56bpp7Channels}
  GUID_WICPixelFormat64bpp8Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc925}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp8Channels}
  GUID_WICPixelFormat48bpp3Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc926}';
  {$EXTERNALSYM GUID_WICPixelFormat48bpp3Channels}
  GUID_WICPixelFormat64bpp4Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc927}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp4Channels}
  GUID_WICPixelFormat80bpp5Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc928}';
  {$EXTERNALSYM GUID_WICPixelFormat80bpp5Channels}
  GUID_WICPixelFormat96bpp6Channels  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc929}';
  {$EXTERNALSYM GUID_WICPixelFormat96bpp6Channels}
  GUID_WICPixelFormat112bpp7Channels : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc92a}';
  {$EXTERNALSYM GUID_WICPixelFormat112bpp7Channels}
  GUID_WICPixelFormat128bpp8Channels : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc92b}';
  {$EXTERNALSYM GUID_WICPixelFormat128bpp8Channels}
  GUID_WICPixelFormat40bppCMYKAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc92c}';
  {$EXTERNALSYM GUID_WICPixelFormat40bppCMYKAlpha}
  GUID_WICPixelFormat80bppCMYKAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc92d}';
  {$EXTERNALSYM GUID_WICPixelFormat80bppCMYKAlpha}
  GUID_WICPixelFormat32bpp3ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc92e}';
  {$EXTERNALSYM GUID_WICPixelFormat32bpp3ChannelsAlpha}
  GUID_WICPixelFormat40bpp4ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc92f}';
  {$EXTERNALSYM GUID_WICPixelFormat40bpp4ChannelsAlpha}
  GUID_WICPixelFormat48bpp5ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc930}';
  {$EXTERNALSYM GUID_WICPixelFormat48bpp5ChannelsAlpha}
  GUID_WICPixelFormat56bpp6ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc931}';
  {$EXTERNALSYM GUID_WICPixelFormat56bpp6ChannelsAlpha}
  GUID_WICPixelFormat64bpp7ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc932}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp7ChannelsAlpha}
  GUID_WICPixelFormat72bpp8ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc933}';
  {$EXTERNALSYM GUID_WICPixelFormat72bpp8ChannelsAlpha}
  GUID_WICPixelFormat64bpp3ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc934}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp3ChannelsAlpha}
  GUID_WICPixelFormat80bpp4ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc935}';
  {$EXTERNALSYM GUID_WICPixelFormat80bpp4ChannelsAlpha}
  GUID_WICPixelFormat96bpp5ChannelsAlpha  : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc936}';
  {$EXTERNALSYM GUID_WICPixelFormat96bpp5ChannelsAlpha}
  GUID_WICPixelFormat112bpp6ChannelsAlpha : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc937}';
  {$EXTERNALSYM GUID_WICPixelFormat112bpp6ChannelsAlpha}
  GUID_WICPixelFormat128bpp7ChannelsAlpha : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc938}';
  {$EXTERNALSYM GUID_WICPixelFormat128bpp7ChannelsAlpha}
  GUID_WICPixelFormat144bpp8ChannelsAlpha : TGUID = '{6fddc324-4e03-4bfe-b185-3d77768dc939}';
  {$EXTERNALSYM GUID_WICPixelFormat144bpp8ChannelsAlpha}
  GUID_WICPixelFormat8bppY             : TGUID = '{91B4DB54-2DF9-42F0-B449-2909BB3DF88E}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppY}
  GUID_WICPixelFormat8bppCb            : TGUID = '{1339F224-6BFE-4C3E-9302-E4F3A6D0CA2A}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppCb}
  GUID_WICPixelFormat8bppCr            : TGUID = '{B8145053-2116-49F0-8835-ED844B205C51}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppCr}
  GUID_WICPixelFormat16bppCbCr         : TGUID = '{FF95BA6E-11E0-4263-BB45-01721F3460A4}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppCbCr}
  GUID_WICPixelFormat16bppYQuantizedDctCoefficients  : TGUID = '{A355F433-48E8-4A42-84D8-E2AA26CA80A4}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppYQuantizedDctCoefficients}
  GUID_WICPixelFormat16bppCbQuantizedDctCoefficients : TGUID = '{D2C4FF61-56A5-49C2-8B5C-4C1925964837}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppCbQuantizedDctCoefficients}
  GUID_WICPixelFormat16bppCrQuantizedDctCoefficients : TGUID = '{2FE354F0-1680-42D8-9231-E73C0565BFC1}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppCrQuantizedDctCoefficients}

 // From intsafe.h
 // Do not re-introduce, to prevent clashes in later versions where intsafe.h will be included
 // INTSAFE_E_ARITHMETIC_OVERFLOW       = HResult($80070216);  // $216 = 534 = ERROR_ARITHMETIC_OVERFLOW
 // end

  WINCODEC_ERR_GENERIC_ERROR          = E_FAIL;
  {$EXTERNALSYM WINCODEC_ERR_GENERIC_ERROR}
  WINCODEC_ERR_INVALIDPARAMETER       = E_INVALIDARG;
  {$EXTERNALSYM WINCODEC_ERR_INVALIDPARAMETER}
  WINCODEC_ERR_OUTOFMEMORY            = E_OUTOFMEMORY;
  {$EXTERNALSYM WINCODEC_ERR_OUTOFMEMORY}
  WINCODEC_ERR_NOTIMPLEMENTED         = E_NOTIMPL;
  {$EXTERNALSYM WINCODEC_ERR_NOTIMPLEMENTED}
  WINCODEC_ERR_ABORTED                = E_ABORT;
  {$EXTERNALSYM WINCODEC_ERR_ABORTED}
  WINCODEC_ERR_ACCESSDENIED           = E_ACCESSDENIED;
  {$EXTERNALSYM WINCODEC_ERR_ACCESSDENIED}
  WINCODEC_ERR_VALUEOVERFLOW          = HResult($80070216); //INTSAFE_E_ARITHMETIC_OVERFLOW;
  {$EXTERNALSYM WINCODEC_ERR_VALUEOVERFLOW}

const

  FACILITY_WINCODEC_ERR               = $898;
  {$EXTERNALSYM FACILITY_WINCODEC_ERR}
  WINCODEC_ERR_BASE                   = $2000;
  {$EXTERNALSYM WINCODEC_ERR_BASE}

  WICRawChangeNotification_ExposureCompensation    = $00000001;
  {$EXTERNALSYM WICRawChangeNotification_ExposureCompensation}
  WICRawChangeNotification_NamedWhitePoint         = $00000002;
  {$EXTERNALSYM WICRawChangeNotification_NamedWhitePoint}
  WICRawChangeNotification_KelvinWhitePoint        = $00000004;
  {$EXTERNALSYM WICRawChangeNotification_KelvinWhitePoint}
  WICRawChangeNotification_RGBWhitePoint           = $00000008;
  {$EXTERNALSYM WICRawChangeNotification_RGBWhitePoint}
  WICRawChangeNotification_Contrast                = $00000010;
  {$EXTERNALSYM WICRawChangeNotification_Contrast}
  WICRawChangeNotification_Gamma                   = $00000020;
  {$EXTERNALSYM WICRawChangeNotification_Gamma}
  WICRawChangeNotification_Sharpness               = $00000040;
  {$EXTERNALSYM WICRawChangeNotification_Sharpness}
  WICRawChangeNotification_Saturation              = $00000080;
  {$EXTERNALSYM WICRawChangeNotification_Saturation}
  WICRawChangeNotification_Tint                    = $00000100;
  {$EXTERNALSYM WICRawChangeNotification_Tint}
  WICRawChangeNotification_NoiseReduction          = $00000200;
  {$EXTERNALSYM WICRawChangeNotification_NoiseReduction}
  WICRawChangeNotification_DestinationColorContext = $00000400;
  {$EXTERNALSYM WICRawChangeNotification_DestinationColorContext}
  WICRawChangeNotification_ToneCurve               = $00000800;
  {$EXTERNALSYM WICRawChangeNotification_ToneCurve}
  WICRawChangeNotification_Rotation                = $00001000;
  {$EXTERNALSYM WICRawChangeNotification_Rotation}
  WICRawChangeNotification_RenderMode              = $00002000;
  {$EXTERNALSYM WICRawChangeNotification_RenderMode}


// Enums =======================================================================

type
  PWICColorContextType = ^WICColorContextType;
  WICColorContextType = DWord;
  {$EXTERNALSYM WICColorContextType}
const
  WICColorContextUninitialized  = WICColorContextType(0);
  {$EXTERNALSYM WICColorContextUninitialized}
  WICColorContextProfile        = WICColorContextType($1);
  {$EXTERNALSYM WICColorContextProfile}
  WICColorContextExifColorSpace = WICColorContextType($2);
  {$EXTERNALSYM WICColorContextExifColorSpace}


type
  PWICBitmapCreateCacheOption = ^WICBitmapCreateCacheOption;
  WICBitmapCreateCacheOption = DWord;
  {$EXTERNALSYM WICBitmapCreateCacheOption}
const
  WICBitmapNoCache                       = WICBitmapCreateCacheOption(0);
  {$EXTERNALSYM WICBitmapNoCache}
  WICBitmapCacheOnDemand                 = WICBitmapCreateCacheOption($1);
  {$EXTERNALSYM WICBitmapCacheOnDemand}
  WICBitmapCacheOnLoad                   = WICBitmapCreateCacheOption($2);
  {$EXTERNALSYM WICBitmapCacheOnLoad}
  //WICBITMAPCREATECACHEOPTION_FORCE_DWORD = FORCEDWORD;

type
  PWICDecodeOptions = ^WICDecodeOptions;
  WICDecodeOptions = DWord;
  {$EXTERNALSYM WICDecodeOptions}
const
  WICDecodeMetadataCacheOnDemand     = WICDecodeOptions(0);
  {$EXTERNALSYM WICDecodeMetadataCacheOnDemand}
  WICDecodeMetadataCacheOnLoad       = WICDecodeOptions($1);
  {$EXTERNALSYM WICDecodeMetadataCacheOnLoad}
  //WICMETADATACACHEOPTION_FORCE_DWORD = FORCEDWORD;

type
  PWICBitmapEncoderCacheOption = ^WICBitmapEncoderCacheOption;
  WICBitmapEncoderCacheOption = DWord;
  {$EXTERNALSYM WICBitmapEncoderCacheOption}
const
  WICBitmapEncoderCacheInMemory           = WICBitmapEncoderCacheOption(0);
  {$EXTERNALSYM WICBitmapEncoderCacheInMemory}
  WICBitmapEncoderCacheTempFile           = WICBitmapEncoderCacheOption($1);
  {$EXTERNALSYM WICBitmapEncoderCacheTempFile}
  WICBitmapEncoderNoCache                 = WICBitmapEncoderCacheOption($2);
  {$EXTERNALSYM WICBitmapEncoderNoCache}
  //WICBITMAPENCODERCACHEOPTION_FORCE_DWORD = FORCEDWORD;

type
  PWICComponentType = ^WICComponentType;
  WICComponentType = DWord;
  {$EXTERNALSYM WICComponentType}
const
  WICDecoder                   = WICComponentType($1);
  {$EXTERNALSYM WICDecoder}
  WICEncoder                   = WICComponentType($2);
  {$EXTERNALSYM WICEncoder}
  WICPixelFormatConverter      = WICComponentType($4);
  {$EXTERNALSYM WICPixelFormatConverter}
  WICMetadataReader            = WICComponentType($8);
  {$EXTERNALSYM WICMetadataReader}
  WICMetadataWriter            = WICComponentType($10);
  {$EXTERNALSYM WICMetadataWriter}
  WICPixelFormat               = WICComponentType($20);
  {$EXTERNALSYM WICPixelFormat}
  WICAllComponents             = WICComponentType($3F);
  {$EXTERNALSYM WICAllComponents}
  //WICCOMPONENTTYPE_FORCE_DWORD = FORCEDWORD;

type
  PWICComponentEnumerateOptions = ^WICComponentEnumerateOptions;
  WICComponentEnumerateOptions = DWord;
  {$EXTERNALSYM WICComponentEnumerateOptions}
const
  WICComponentEnumerateDefault         = WICComponentEnumerateOptions(0);
  {$EXTERNALSYM WICComponentEnumerateDefault}
  WICComponentEnumerateRefresh         = WICComponentEnumerateOptions($1);
  {$EXTERNALSYM WICComponentEnumerateRefresh}
  WICComponentEnumerateDisabled        = WICComponentEnumerateOptions($80000000);
  {$EXTERNALSYM WICComponentEnumerateDisabled}
  WICComponentEnumerateUnsigned        = WICComponentEnumerateOptions($40000000);
  {$EXTERNALSYM WICComponentEnumerateUnsigned}
  WICComponentEnumerateBuiltInOnly     = WICComponentEnumerateOptions($20000000);
  {$EXTERNALSYM WICComponentEnumerateBuiltInOnly}
  //WICCOMPONENTENUMERATEOPTIONS_FORCE_DWORD = $7FFFFFFF;

type
  PWICBitmapInterpolationMode = ^WICBitmapInterpolationMode;
  WICBitmapInterpolationMode = DWord;
  {$EXTERNALSYM WICBitmapInterpolationMode}
const
  WICBitmapInterpolationModeNearestNeighbor  = WICBitmapInterpolationMode(0);
  {$EXTERNALSYM WICBitmapInterpolationModeNearestNeighbor}
  WICBitmapInterpolationModeLinear           = WICBitmapInterpolationMode($1);
  {$EXTERNALSYM WICBitmapInterpolationModeLinear}
  WICBitmapInterpolationModeCubic            = WICBitmapInterpolationMode($2);
  {$EXTERNALSYM WICBitmapInterpolationModeCubic}
  WICBitmapInterpolationModeFant             = WICBitmapInterpolationMode($3);
  {$EXTERNALSYM WICBitmapInterpolationModeFant}
  WICBitmapInterpolationModeHighQualityCubic = WICBitmapInterpolationMode($4);
  {$EXTERNALSYM WICBitmapInterpolationModeHighQualityCubic}
  //WICBITMAPINTERPOLATIONMODE_FORCE_DWORD   = $7FFFFFFF;

type
  PWICBitmapPaletteType = ^WICBitmapPaletteType;
  WICBitmapPaletteType = DWord;
  {$EXTERNALSYM WICBitmapPaletteType}
const
  WICBitmapPaletteTypeCustom           = WICBitmapPaletteType(0);
  {$EXTERNALSYM WICBitmapPaletteTypeCustom}
  WICBitmapPaletteTypeMedianCut        = WICBitmapPaletteType($1);
  {$EXTERNALSYM WICBitmapPaletteTypeMedianCut}
  WICBitmapPaletteTypeFixedBW          = WICBitmapPaletteType($2);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedBW}
  WICBitmapPaletteTypeFixedHalftone8   = WICBitmapPaletteType($3);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone8}
  WICBitmapPaletteTypeFixedHalftone27  = WICBitmapPaletteType($4);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone27}
  WICBitmapPaletteTypeFixedHalftone64  = WICBitmapPaletteType($5);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone64}
  WICBitmapPaletteTypeFixedHalftone125 = WICBitmapPaletteType($6);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone125}
  WICBitmapPaletteTypeFixedHalftone216 = WICBitmapPaletteType($7);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone216}
  WICBitmapPaletteTypeFixedWebPalette  = WICBitmapPaletteTypeFixedHalftone216;
  {$EXTERNALSYM WICBitmapPaletteTypeFixedWebPalette}
  WICBitmapPaletteTypeFixedHalftone252 = WICBitmapPaletteType($8);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone252}
  WICBitmapPaletteTypeFixedHalftone256 = WICBitmapPaletteType($9);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone256}
  WICBitmapPaletteTypeFixedGray4       = WICBitmapPaletteType($A);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedGray4}
  WICBitmapPaletteTypeFixedGray16      = WICBitmapPaletteType($B);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedGray16}
  WICBitmapPaletteTypeFixedGray256     = WICBitmapPaletteType($C);
  {$EXTERNALSYM WICBitmapPaletteTypeFixedGray256}
  //WICBITMAPPALETTETYPE_FORCE_DWORD   = FORCEDWORD

type
  PWICBitmapDitherType = ^WICBitmapDitherType;
  WICBitmapDitherType = DWord;
  {$EXTERNALSYM WICBitmapDitherType}
const
  WICBitmapDitherTypeNone           = WICBitmapDitherType(0);
  {$EXTERNALSYM WICBitmapDitherTypeNone}
  WICBitmapDitherTypeSolid          = WICBitmapDitherType(0);
  {$EXTERNALSYM WICBitmapDitherTypeSolid}
  WICBitmapDitherTypeOrdered4x4     = WICBitmapDitherType($1);
  {$EXTERNALSYM WICBitmapDitherTypeOrdered4x4}
  WICBitmapDitherTypeOrdered8x8     = WICBitmapDitherType($2);
  {$EXTERNALSYM WICBitmapDitherTypeOrdered8x8}
  WICBitmapDitherTypeOrdered16x16   = WICBitmapDitherType($3);
  {$EXTERNALSYM WICBitmapDitherTypeOrdered16x16}
  WICBitmapDitherTypeSpiral4x4      = WICBitmapDitherType($4);
  {$EXTERNALSYM WICBitmapDitherTypeSpiral4x4}
  WICBitmapDitherTypeSpiral8x8      = WICBitmapDitherType($5);
  {$EXTERNALSYM WICBitmapDitherTypeSpiral8x8}
  WICBitmapDitherTypeDualSpiral4x4  = WICBitmapDitherType($6);
  {$EXTERNALSYM WICBitmapDitherTypeDualSpiral4x4}
  WICBitmapDitherTypeDualSpiral8x8  = WICBitmapDitherType($7);
  {$EXTERNALSYM WICBitmapDitherTypeDualSpiral8x8}
  WICBitmapDitherTypeErrorDiffusion = WICBitmapDitherType($8);
  {$EXTERNALSYM WICBitmapDitherTypeErrorDiffusion}
  //WICBITMAPDITHERTYPE_FORCE_DWORD   = FORCEDWORD;

type
  PWICBitmapAlphaChannelOption = ^WICBitmapAlphaChannelOption;
  WICBitmapAlphaChannelOption = DWord;
  {$EXTERNALSYM WICBitmapAlphaChannelOption}
const
  WICBitmapUseAlpha              = WICBitmapAlphaChannelOption(0);
  {$EXTERNALSYM WICBitmapUseAlpha}
  WICBitmapUsePremultipliedAlpha = WICBitmapAlphaChannelOption($1);
  {$EXTERNALSYM WICBitmapUsePremultipliedAlpha}
  WICBitmapIgnoreAlpha           = WICBitmapAlphaChannelOption($2);
  {$EXTERNALSYM WICBitmapIgnoreAlpha}
  //WICBITMAPALPHACHANNELOPTIONS_FORCE_DWORD = FORCEDWORD;

type
  PWICBitmapTransformOptions = ^WICBitmapTransformOptions;
  WICBitmapTransformOptions = DWord;
  {$EXTERNALSYM WICBitmapTransformOptions}
const
  WICBitmapTransformRotate0         = WICBitmapTransformOptions(0);
  {$EXTERNALSYM WICBitmapTransformRotate0}
  WICBitmapTransformRotate90        = WICBitmapTransformOptions($1);
  {$EXTERNALSYM WICBitmapTransformRotate90}
  WICBitmapTransformRotate180       = WICBitmapTransformOptions($2);
  {$EXTERNALSYM WICBitmapTransformRotate180}
  WICBitmapTransformRotate270       = WICBitmapTransformOptions($3);
  {$EXTERNALSYM WICBitmapTransformRotate270}
  WICBitmapTransformFlipHorizontal  = WICBitmapTransformOptions($8);
  {$EXTERNALSYM WICBitmapTransformFlipHorizontal}
  WICBitmapTransformFlipVertical    = WICBitmapTransformOptions($10);
  {$EXTERNALSYM WICBitmapTransformFlipVertical}
  //WICBITMAPTRANSFORMOPTIONS_FORCE_DWORD = FORCEDWORD;

type
  PWICBitmapLockFlags = ^WICBitmapLockFlags;
  WICBitmapLockFlags = DWord;
  {$EXTERNALSYM WICBitmapLockFlags}
const
  WICBitmapLockRead        = WICBitmapLockFlags($1);
  {$EXTERNALSYM WICBitmapLockRead}
  WICBitmapLockWrite       = WICBitmapLockFlags($2);
  {$EXTERNALSYM WICBitmapLockWrite}
  //WICBITMAPLOCKFLAGS_FORCE_DWORD = FORCEDWORD;

type
  PWICBitmapDecoderCapabilities = ^WICBitmapDecoderCapabilities;
  WICBitmapDecoderCapabilities = DWord;
  {$EXTERNALSYM WICBitmapDecoderCapabilities}
const
  WICBitmapDecoderCapabilitySameEncoder          = WICBitmapDecoderCapabilities($1);
  {$EXTERNALSYM WICBitmapDecoderCapabilitySameEncoder}
  WICBitmapDecoderCapabilityCanDecodeAllImages   = WICBitmapDecoderCapabilities($2);
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanDecodeAllImages}
  WICBitmapDecoderCapabilityCanDecodeSomeImages  = WICBitmapDecoderCapabilities($4);
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanDecodeSomeImages}
  WICBitmapDecoderCapabilityCanEnumerateMetadata = WICBitmapDecoderCapabilities($8);
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanEnumerateMetadata}
  WICBitmapDecoderCapabilityCanDecodeThumbnail   = WICBitmapDecoderCapabilities($10);
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanDecodeThumbnail}
  //WICBITMAPDECODERCAPABILITIES_FORCE_DWORD     = FORCEDWORD;

type
  PWICProgressOperation = ^WICProgressOperation;
  WICProgressOperation = DWord;
  {$EXTERNALSYM WICProgressOperation}
const
  WICProgressOperationCopyPixels   = WICProgressOperation($1);
  {$EXTERNALSYM WICProgressOperationCopyPixels}
  WICProgressOperationWritePixels  = WICProgressOperation($2);
  {$EXTERNALSYM WICProgressOperationWritePixels}
  WICProgressOperationAll          = WICProgressOperation($FFFF);
  {$EXTERNALSYM WICProgressOperationAll}
  //WICPROGRESSOPERATION_FORCE_DWORD = FORCEDWORD;

type
  PWICProgressNotification = ^WICProgressNotification;
  WICProgressNotification = DWord;
  {$EXTERNALSYM WICProgressNotification}
const
  WICProgressNotificationBegin    = WICProgressNotification($10000);
  {$EXTERNALSYM WICProgressNotificationBegin}
  WICProgressNotificationEnd      = WICProgressNotification($20000);
  {$EXTERNALSYM WICProgressNotificationEnd}
  WICProgressNotificationFrequent = WICProgressNotification($40000);
  {$EXTERNALSYM WICProgressNotificationFrequent}
  WICProgressNotificationAll      = WICProgressNotification($FFFF0000);
  {$EXTERNALSYM WICProgressNotificationAll}
  //WICPROGRESSNOTIFICATION_FORCE_DWORD = FORCEDWORD;

type
  PWICComponentSigning = ^WICComponentSigning;
  WICComponentSigning = DWord;
  {$EXTERNALSYM WICComponentSigning}
const
  WICComponentSigned        = WICComponentSigning($1);
  {$EXTERNALSYM WICComponentSigned}
  WICComponentUnsigned      = WICComponentSigning($2);
  {$EXTERNALSYM WICComponentUnsigned}
  WICComponentSafe          = WICComponentSigning($4);
  {$EXTERNALSYM WICComponentSafe}
  WICComponentDisabled      = WICComponentSigning($80000000);
  {$EXTERNALSYM WICComponentDisabled}
  //WICCOMPONENTSIGNING_FORCE_DWORD = FORCEDWORD;

type
  PWICGifLogicalScreenDescriptorProperties = ^WICGifLogicalScreenDescriptorProperties;
  WICGifLogicalScreenDescriptorProperties = DWord;
  {$EXTERNALSYM WICGifLogicalScreenDescriptorProperties}
const
  WICGifLogicalScreenSignature                      = WICGifLogicalScreenDescriptorProperties($1);
  {$EXTERNALSYM WICGifLogicalScreenSignature}
  WICGifLogicalScreenDescriptorWidth                = WICGifLogicalScreenDescriptorProperties($2);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorWidth}
  WICGifLogicalScreenDescriptorHeight               = WICGifLogicalScreenDescriptorProperties($3);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorHeight}
  WICGifLogicalScreenDescriptorGlobalColorTableFlag = WICGifLogicalScreenDescriptorProperties($4);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorGlobalColorTableFlag}
  WICGifLogicalScreenDescriptorColorResolution      = WICGifLogicalScreenDescriptorProperties($5);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorColorResolution}
  WICGifLogicalScreenDescriptorSortFlag             = WICGifLogicalScreenDescriptorProperties($6);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorSortFlag}
  WICGifLogicalScreenDescriptorGlobalColorTableSize = WICGifLogicalScreenDescriptorProperties($7);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorGlobalColorTableSize}
  WICGifLogicalScreenDescriptorBackgroundColorIndex = WICGifLogicalScreenDescriptorProperties($8);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorBackgroundColorIndex}
  WICGifLogicalScreenDescriptorPixelAspectRatio     = WICGifLogicalScreenDescriptorProperties($9);
  {$EXTERNALSYM WICGifLogicalScreenDescriptorPixelAspectRatio}
  //WICGifLogicalScreenDescriptorProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICGifImageDescriptorProperties = ^WICGifImageDescriptorProperties;
  WICGifImageDescriptorProperties = DWord;
  {$EXTERNALSYM WICGifImageDescriptorProperties}
const
  WICGifImageDescriptorLeft                 = WICGifImageDescriptorProperties($1);
  {$EXTERNALSYM WICGifImageDescriptorLeft}
  WICGifImageDescriptorTop                  = WICGifImageDescriptorProperties($2);
  {$EXTERNALSYM WICGifImageDescriptorTop}
  WICGifImageDescriptorWidth                = WICGifImageDescriptorProperties($3);
  {$EXTERNALSYM WICGifImageDescriptorWidth}
  WICGifImageDescriptorHeight               = WICGifImageDescriptorProperties($4);
  {$EXTERNALSYM WICGifImageDescriptorHeight}
  WICGifImageDescriptorLocalColorTableFlag  = WICGifImageDescriptorProperties($5);
  {$EXTERNALSYM WICGifImageDescriptorLocalColorTableFlag}
  WICGifImageDescriptorInterlaceFlag        = WICGifImageDescriptorProperties($6);
  {$EXTERNALSYM WICGifImageDescriptorInterlaceFlag}
  WICGifImageDescriptorSortFlag             = WICGifImageDescriptorProperties($7);
  {$EXTERNALSYM WICGifImageDescriptorSortFlag}
  WICGifImageDescriptorLocalColorTableSize  = WICGifImageDescriptorProperties($8);
  {$EXTERNALSYM WICGifImageDescriptorLocalColorTableSize}
  //WICGifImageDescriptorProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICGifGraphicControlExtensionProperties = ^WICGifGraphicControlExtensionProperties;
  WICGifGraphicControlExtensionProperties = DWord;
  {$EXTERNALSYM WICGifGraphicControlExtensionProperties}
const
  WICGifGraphicControlExtensionDisposal               = WICGifGraphicControlExtensionProperties($1);
  {$EXTERNALSYM WICGifGraphicControlExtensionDisposal}
  WICGifGraphicControlExtensionUserInputFlag          = WICGifGraphicControlExtensionProperties($2);
  {$EXTERNALSYM WICGifGraphicControlExtensionUserInputFlag}
  WICGifGraphicControlExtensionTransparencyFlag       = WICGifGraphicControlExtensionProperties($3);
  {$EXTERNALSYM WICGifGraphicControlExtensionTransparencyFlag}
  WICGifGraphicControlExtensionDelay                  = WICGifGraphicControlExtensionProperties($4);
  {$EXTERNALSYM WICGifGraphicControlExtensionDelay}
  WICGifGraphicControlExtensionTransparentColorIndex  = WICGifGraphicControlExtensionProperties($5);
  {$EXTERNALSYM WICGifGraphicControlExtensionTransparentColorIndex}
  //WICGifGraphicControlExtensionProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICGifApplicationExtensionProperties = ^WICGifApplicationExtensionProperties;
  WICGifApplicationExtensionProperties = DWord;
  {$EXTERNALSYM WICGifApplicationExtensionProperties}
const
  WICGifApplicationExtensionApplication      = WICGifApplicationExtensionProperties($1);
  {$EXTERNALSYM WICGifApplicationExtensionApplication}
  WICGifApplicationExtensionData             = WICGifApplicationExtensionProperties($2);
  {$EXTERNALSYM WICGifApplicationExtensionData}
  //WICGifApplicationExtensionProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICGifCommentExtensionProperties = ^WICGifCommentExtensionProperties;
  WICGifCommentExtensionProperties = DWord;
  {$EXTERNALSYM WICGifCommentExtensionProperties}
const
  WICGifCommentExtensionText           = WICGifCommentExtensionProperties($1);
  {$EXTERNALSYM WICGifCommentExtensionText}
  //WICGifCommentExtensionProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICJpegCommentProperties = ^WICJpegCommentProperties;
  WICJpegCommentProperties = DWord;
  {$EXTERNALSYM WICJpegCommentProperties}
const
  WICJpegCommentText           = WICJpegCommentProperties($1);
  {$EXTERNALSYM WICJpegCommentText}
  //WICJpegCommentProperties_FORCE_DWORD = FORCEDWORD;;

type
  PWICJpegLuminanceProperties = ^WICJpegLuminanceProperties;
  WICJpegLuminanceProperties = DWord;
  {$EXTERNALSYM WICJpegLuminanceProperties}
const
  WICJpegLuminanceTable = WICJpegLuminanceProperties($1);
  {$EXTERNALSYM WICJpegLuminanceTable}
  //WICJpegLuminanceProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICJpegChrominanceProperties = ^WICJpegChrominanceProperties;
  WICJpegChrominanceProperties = DWord;
  {$EXTERNALSYM WICJpegChrominanceProperties}
const
  WICJpegChrominanceTable = WICJpegChrominanceProperties($1);
  {$EXTERNALSYM WICJpegChrominanceTable}
  //WICJpegChrominanceProperties_FORCE_DWORD = FORCEDWORD;

type
  PWIC8BIMIptcProperties = ^WIC8BIMIptcProperties;
  WIC8BIMIptcProperties = DWord;
  {$EXTERNALSYM WIC8BIMIptcProperties}
const
  WIC8BIMIptcPString            = WIC8BIMIptcProperties(0);
  {$EXTERNALSYM WIC8BIMIptcPString}
  WIC8BIMIptcEmbeddedIPTC       = WIC8BIMIptcProperties($1);
  {$EXTERNALSYM WIC8BIMIptcEmbeddedIPTC}
  //WIC8BIMIptcProperties_FORCE_DWORD = FORCEDWORD;

type
  PWIC8BIMResolutionInfoProperties = ^WIC8BIMResolutionInfoProperties;
  WIC8BIMResolutionInfoProperties = DWord;
  {$EXTERNALSYM WIC8BIMResolutionInfoProperties}
const
  WIC8BIMResolutionInfoPString          = WIC8BIMResolutionInfoProperties($1);
  {$EXTERNALSYM WIC8BIMResolutionInfoPString}
  WIC8BIMResolutionInfoHResolution      = WIC8BIMResolutionInfoProperties($2);
  {$EXTERNALSYM WIC8BIMResolutionInfoHResolution}
  WIC8BIMResolutionInfoHResolutionUnit  = WIC8BIMResolutionInfoProperties($3);
  {$EXTERNALSYM WIC8BIMResolutionInfoHResolutionUnit}
  WIC8BIMResolutionInfoWidthUnit        = WIC8BIMResolutionInfoProperties($4);
  {$EXTERNALSYM WIC8BIMResolutionInfoWidthUnit}
  WIC8BIMResolutionInfoVResolution      = WIC8BIMResolutionInfoProperties($5);
  {$EXTERNALSYM WIC8BIMResolutionInfoVResolution}
  WIC8BIMResolutionInfoVResolutionUnit  = WIC8BIMResolutionInfoProperties($6);
  {$EXTERNALSYM WIC8BIMResolutionInfoVResolutionUnit}
  WIC8BIMResolutionInfoHeightUnit       = WIC8BIMResolutionInfoProperties($7);
  {$EXTERNALSYM WIC8BIMResolutionInfoHeightUnit}
  //WIC8BIMResolutionInfoProperties_FORCE_DWORD = FORCEDWORD;

type
  PWIC8BIMIptcDigestProperties = ^WIC8BIMIptcDigestProperties;
  WIC8BIMIptcDigestProperties = DWord;
  {$EXTERNALSYM WIC8BIMIptcDigestProperties}
const
  WIC8BIMIptcDigestPString          = WIC8BIMIptcDigestProperties($1);
  {$EXTERNALSYM WIC8BIMIptcDigestPString}
  WIC8BIMIptcDigestIptcDigest       = WIC8BIMIptcDigestProperties($2);
  {$EXTERNALSYM WIC8BIMIptcDigestIptcDigest}
  //WIC8BIMIptcDigestProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngGamaProperties = ^WICPngGamaProperties;
  WICPngGamaProperties = DWord;
  {$EXTERNALSYM WICPngGamaProperties}
const
  WICPngGamaGamma          = WICPngGamaProperties($1);
  {$EXTERNALSYM WICPngGamaGamma}
  //WICPngGamaProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngBkgdProperties = ^WICPngBkgdProperties;
  WICPngBkgdProperties = DWord;
  {$EXTERNALSYM WICPngBkgdProperties}
const
  WICPngBkgdBackgroundColor    = WICPngBkgdProperties($1);
  {$EXTERNALSYM WICPngBkgdBackgroundColor}
  //WICPngBkgdProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngItxtProperties = ^WICPngItxtProperties;
  WICPngItxtProperties = DWord;
  {$EXTERNALSYM WICPngItxtProperties}
const
  WICPngItxtKeyword           = WICPngItxtProperties($1);
  {$EXTERNALSYM WICPngItxtKeyword}
  WICPngItxtCompressionFlag   = WICPngItxtProperties($2);
  {$EXTERNALSYM WICPngItxtCompressionFlag}
  WICPngItxtLanguageTag       = WICPngItxtProperties($3);
  {$EXTERNALSYM WICPngItxtLanguageTag}
  WICPngItxtTranslatedKeyword = WICPngItxtProperties($4);
  {$EXTERNALSYM WICPngItxtTranslatedKeyword}
  WICPngItxtText              = WICPngItxtProperties($5);
  {$EXTERNALSYM WICPngItxtText}
  //WICPngItxtProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngChrmProperties = ^WICPngChrmProperties;
  WICPngChrmProperties         = DWord;
  {$EXTERNALSYM WICPngChrmProperties}
const
  WICPngChrmWhitePointX    = WICPngChrmProperties($1);
  {$EXTERNALSYM WICPngChrmWhitePointX}
  WICPngChrmWhitePointY    = WICPngChrmProperties($2);
  {$EXTERNALSYM WICPngChrmWhitePointY}
  WICPngChrmRedX           = WICPngChrmProperties($3);
  {$EXTERNALSYM WICPngChrmRedX}
  WICPngChrmRedY           = WICPngChrmProperties($4);
  {$EXTERNALSYM WICPngChrmRedY}
  WICPngChrmGreenX         = WICPngChrmProperties($5);
  {$EXTERNALSYM WICPngChrmGreenX}
  WICPngChrmGreenY         = WICPngChrmProperties($6);
  {$EXTERNALSYM WICPngChrmGreenY}
  WICPngChrmBlueX          = WICPngChrmProperties($7);
  {$EXTERNALSYM WICPngChrmBlueX}
  WICPngChrmBlueY          = WICPngChrmProperties($8);
  {$EXTERNALSYM WICPngChrmBlueY}
  //WICPngChrmProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngHistProperties = ^WICPngHistProperties;
  WICPngHistProperties = DWord;
  {$EXTERNALSYM WICPngHistProperties}
const
  WICPngHistFrequencies      = WICPngHistProperties($1);
  {$EXTERNALSYM WICPngHistFrequencies}
  //WICPngHistProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngIccpProperties = ^WICPngIccpProperties;
  WICPngIccpProperties = DWord;
  {$EXTERNALSYM WICPngIccpProperties}
const
  WICPngIccpProfileName      = WICPngIccpProperties($1);
  {$EXTERNALSYM WICPngIccpProfileName}
  WICPngIccpProfileData      = WICPngIccpProperties($2);
  {$EXTERNALSYM WICPngIccpProfileData}
  //WICPngIccpProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngSrgbProperties = ^WICPngSrgbProperties;
  WICPngSrgbProperties = DWord;
  {$EXTERNALSYM WICPngSrgbProperties}
const
  WICPngSrgbRenderingIntent    = WICPngSrgbProperties($1);
  {$EXTERNALSYM WICPngSrgbRenderingIntent}
  //WICPngSrgbProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICPngTimeProperties = ^WICPngTimeProperties;
  WICPngTimeProperties = DWord;
  {$EXTERNALSYM WICPngTimeProperties}
const
  WICPngTimeYear           = WICPngTimeProperties($1);
  {$EXTERNALSYM WICPngTimeYear}
  WICPngTimeMonth          = WICPngTimeProperties($2);
  {$EXTERNALSYM WICPngTimeMonth}
  WICPngTimeDay          = WICPngTimeProperties($3);
  {$EXTERNALSYM WICPngTimeDay}
  WICPngTimeHour           = WICPngTimeProperties($4);
  {$EXTERNALSYM WICPngTimeHour}
  WICPngTimeMinute         = WICPngTimeProperties($5);
  {$EXTERNALSYM WICPngTimeMinute}
  WICPngTimeSecond         = WICPngTimeProperties($6);
  {$EXTERNALSYM WICPngTimeSecond}
  //WICPngTimeProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICHeifProperties = ^WICHeifProperties;
  WICHeifProperties = DWord;
  {$EXTERNALSYM WICHeifProperties}
const
  WICHeifOrientation      = WICHeifProperties($1);
  {$EXTERNALSYM WICHeifOrientation}
  //WICHeifProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICHeifHdrProperties = ^WICHeifHdrProperties;
  WICHeifHdrProperties = DWord;
  {$EXTERNALSYM WICHeifHdrProperties}
const
  WICHeifHdrMaximumLuminanceLevel                 = WICHeifHdrProperties($1);
  {$EXTERNALSYM WICHeifHdrMaximumLuminanceLevel}
  WICHeifHdrMaximumFrameAverageLuminanceLevel     = WICHeifHdrProperties($2);
  {$EXTERNALSYM WICHeifHdrMaximumFrameAverageLuminanceLevel}
  WICHeifHdrMinimumMasteringDisplayLuminanceLevel = WICHeifHdrProperties($3);
  {$EXTERNALSYM WICHeifHdrMinimumMasteringDisplayLuminanceLevel}
  WICHeifHdrMaximumMasteringDisplayLuminanceLevel = WICHeifHdrProperties($4);
  {$EXTERNALSYM WICHeifHdrMaximumMasteringDisplayLuminanceLevel}
  WICHeifHdrCustomVideoPrimaries                  = WICHeifHdrProperties($5);
  {$EXTERNALSYM WICHeifHdrCustomVideoPrimaries}
  //WICHeifHdrProperties_FORCE_DWORD        = FORCEDWORD;

type
  PWICWebpAnimProperties = ^WICWebpAnimProperties;
  WICWebpAnimProperties = DWord;
  {$EXTERNALSYM WICWebpAnimProperties}
const
  WICWebpAnimLoopCount        = WICWebpAnimProperties($1);
  {$EXTERNALSYM WICWebpAnimLoopCount}
  //WICWebpAnimProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICWebpAnmfProperties = ^WICWebpAnmfProperties;
  WICWebpAnmfProperties = DWord;
  {$EXTERNALSYM WICWebpAnmfProperties}
const
  WICWebpAnmfFrameDuration      = WICWebpAnmfProperties($1);
  {$EXTERNALSYM WICWebpAnmfFrameDuration}
  //WICWebpAnmfProperties_FORCE_DWORD = FORCEDWORD;

type
  PWICSectionAccessLevel = ^WICSectionAccessLevel;
  WICSectionAccessLevel         = DWord;
  {$EXTERNALSYM WICSectionAccessLevel}
const
  WICSectionAccessLevelRead       = WICSectionAccessLevel($1);
  {$EXTERNALSYM WICSectionAccessLevelRead}
  WICSectionAccessLevelReadWrite  = WICSectionAccessLevel($3);
  {$EXTERNALSYM WICSectionAccessLevelReadWrite}
  //WICSectionAccessLevel_FORCE_DWORD = FORCEDWORD;

type
  PWICPixelFormatNumericRepresentation = ^WICPixelFormatNumericRepresentation;
  WICPixelFormatNumericRepresentation = DWord;
  {$EXTERNALSYM WICPixelFormatNumericRepresentation}
const
  WICPixelFormatNumericRepresentationUnspecified     = WICPixelFormatNumericRepresentation(0);
  {$EXTERNALSYM WICPixelFormatNumericRepresentationUnspecified}
  WICPixelFormatNumericRepresentationIndexed         = WICPixelFormatNumericRepresentation($1);
  {$EXTERNALSYM WICPixelFormatNumericRepresentationIndexed}
  WICPixelFormatNumericRepresentationUnsignedInteger = WICPixelFormatNumericRepresentation($2);
  {$EXTERNALSYM WICPixelFormatNumericRepresentationUnsignedInteger}
  WICPixelFormatNumericRepresentationSignedInteger   = WICPixelFormatNumericRepresentation($3);
  {$EXTERNALSYM WICPixelFormatNumericRepresentationSignedInteger}
  WICPixelFormatNumericRepresentationFixed           = WICPixelFormatNumericRepresentation($4);
  {$EXTERNALSYM WICPixelFormatNumericRepresentationFixed}
  WICPixelFormatNumericRepresentationFloat           = WICPixelFormatNumericRepresentation($5);
  {$EXTERNALSYM WICPixelFormatNumericRepresentationFloat}
  //WICPixelFormatNumericRepresentation_FORCE_DWORD  = FORCEDWORD;

type
  PWICPlanarOptions = ^WICPlanarOptions;
  WICPlanarOptions = DWord;
  {$EXTERNALSYM WICPlanarOptions}
const
  WICPlanarOptionsDefault             = WICPlanarOptions(0);
  {$EXTERNALSYM WICPlanarOptionsDefault}
  WICPlanarOptionsPreserveSubsampling = WICPlanarOptions($1);
  {$EXTERNALSYM WICPlanarOptionsPreserveSubsampling}
  //WICPLANAROPTIONS_FORCE_DWORD = FORCEDWORD;

type
  PWICJpegIndexingOptions = ^WICJpegIndexingOptions;
  WICJpegIndexingOptions = DWord;
  {$EXTERNALSYM WICJpegIndexingOptions}
const
  WICJpegIndexingOptionsGenerateOnDemand = WICJpegIndexingOptions(0);
  {$EXTERNALSYM WICJpegIndexingOptionsGenerateOnDemand}
  WICJpegIndexingOptionsGenerateOnLoad   = WICJpegIndexingOptions($1);
  {$EXTERNALSYM WICJpegIndexingOptionsGenerateOnLoad}
  //WICJpegIndexingOptions_FORCE_DWORD     = FORCEDWORD;

type
  PWICJpegTransferMatrix = ^WICJpegTransferMatrix;
  WICJpegTransferMatrix = DWord;
  {$EXTERNALSYM WICJpegTransferMatrix}
const
  WICJpegTransferMatrixIdentity = WICJpegTransferMatrix(0);
  {$EXTERNALSYM WICJpegTransferMatrixIdentity}
  WICJpegTransferMatrixBT601    = WICJpegTransferMatrix($1);
  {$EXTERNALSYM WICJpegTransferMatrixBT601}
  //WICJpegTransferMatrix_FORCE_DWORD = FORCEDWORD;

type
  PWICJpegScanType = ^WICJpegScanType;
  WICJpegScanType = DWord;
  {$EXTERNALSYM WICJpegScanType}
const
  WICJpegScanTypeInterleaved      = WICJpegScanType(0);
  {$EXTERNALSYM WICJpegScanTypeInterleaved}
  WICJpegScanTypePlanarComponents = WICJpegScanType($1);
  {$EXTERNALSYM WICJpegScanTypePlanarComponents}
  WICJpegScanTypeProgressive      = WICJpegScanType($2);
  {$EXTERNALSYM WICJpegScanTypeProgressive}
  //WICJpegScanType_FORCE_DWORD   = FORCEDWORD;

type
  PWICTiffCompressionOption = ^WICTiffCompressionOption;
  WICTiffCompressionOption = DWord;
  {$EXTERNALSYM WICTiffCompressionOption}
const
  WICTiffCompressionDontCare         = WICTiffCompressionOption(0);
  {$EXTERNALSYM WICTiffCompressionDontCare}
  WICTiffCompressionNone             = WICTiffCompressionOption($1);
  {$EXTERNALSYM WICTiffCompressionNone}
  WICTiffCompressionCCITT3           = WICTiffCompressionOption($2);
  {$EXTERNALSYM WICTiffCompressionCCITT3}
  WICTiffCompressionCCITT4           = WICTiffCompressionOption($3);
  {$EXTERNALSYM WICTiffCompressionCCITT4}
  WICTiffCompressionLZW              = WICTiffCompressionOption($4);
  {$EXTERNALSYM WICTiffCompressionLZW}
  WICTiffCompressionRLE              = WICTiffCompressionOption($5);
  {$EXTERNALSYM WICTiffCompressionRLE}
  WICTiffCompressionZIP              = WICTiffCompressionOption($6);
  {$EXTERNALSYM WICTiffCompressionZIP}
  WICTiffCompressionLZWHDifferencing = WICTiffCompressionOption($7);
  {$EXTERNALSYM WICTiffCompressionLZWHDifferencing}
  //WICTIFFCOMPRESSIONOPTION_FORCE_DWORD = FORCEDWORD;

type
  PWICJpegYCrCbSubsamplingOption = ^WICJpegYCrCbSubsamplingOption;
  WICJpegYCrCbSubsamplingOption = DWord;
  {$EXTERNALSYM WICJpegYCrCbSubsamplingOption}
const
  WICJpegYCrCbSubsamplingDefault  = WICJpegYCrCbSubsamplingOption(0);
  {$EXTERNALSYM WICJpegYCrCbSubsamplingDefault}
  WICJpegYCrCbSubsampling420      = WICJpegYCrCbSubsamplingOption($1);
  {$EXTERNALSYM WICJpegYCrCbSubsampling420}
  WICJpegYCrCbSubsampling422      = WICJpegYCrCbSubsamplingOption($2);
  {$EXTERNALSYM WICJpegYCrCbSubsampling422}
  WICJpegYCrCbSubsampling444      = WICJpegYCrCbSubsamplingOption($3);
  {$EXTERNALSYM WICJpegYCrCbSubsampling444}
  WICJpegYCrCbSubsampling440      = WICJpegYCrCbSubsamplingOption($4);
  {$EXTERNALSYM WICJpegYCrCbSubsampling440}
  //WICJPEGYCRCBSUBSAMPLING_FORCE_DWORD = FORCEDWORD;

type
  PWICPngFilterOption = ^WICPngFilterOption;
  WICPngFilterOption = DWord;
  {$EXTERNALSYM WICPngFilterOption}
const
  WICPngFilterUnspecified    = WICPngFilterOption(0);
  {$EXTERNALSYM WICPngFilterUnspecified}
  WICPngFilterNone           = WICPngFilterOption($1);
  {$EXTERNALSYM WICPngFilterNone}
  WICPngFilterSub            = WICPngFilterOption($2);
  {$EXTERNALSYM WICPngFilterSub}
  WICPngFilterUp             = WICPngFilterOption($3);
  {$EXTERNALSYM WICPngFilterUp}
  WICPngFilterAverage        = WICPngFilterOption($4);
  {$EXTERNALSYM WICPngFilterAverage}
  WICPngFilterPaeth          = WICPngFilterOption($5);
  {$EXTERNALSYM WICPngFilterPaeth}
  WICPngFilterAdaptive       = WICPngFilterOption($6);
  {$EXTERNALSYM WICPngFilterAdaptive}
  //WICPNGFILTEROPTION_FORCE_DWORD = FORCEDWORD;

type
  PWICNamedWhitePoint = ^WICNamedWhitePoint;
  WICNamedWhitePoint = DWord;
  {$EXTERNALSYM WICNamedWhitePoint}
const
  WICWhitePointDefault           = WICNamedWhitePoint($1);
  {$EXTERNALSYM WICWhitePointDefault}
  WICWhitePointDaylight          = WICNamedWhitePoint($2);
  {$EXTERNALSYM WICWhitePointDaylight}
  WICWhitePointCloudy            = WICNamedWhitePoint($4);
  {$EXTERNALSYM WICWhitePointCloudy}
  WICWhitePointShade             = WICNamedWhitePoint($8);
  {$EXTERNALSYM WICWhitePointShade}
  WICWhitePointTungsten          = WICNamedWhitePoint($10);
  {$EXTERNALSYM WICWhitePointTungsten}
  WICWhitePointFluorescent       = WICNamedWhitePoint($20);
  {$EXTERNALSYM WICWhitePointFluorescent}
  WICWhitePointFlash             = WICNamedWhitePoint($40);
  {$EXTERNALSYM WICWhitePointFlash}
  WICWhitePointUnderwater        = WICNamedWhitePoint($80);
  {$EXTERNALSYM WICWhitePointUnderwater}
  WICWhitePointCustom            = WICNamedWhitePoint($100);
  {$EXTERNALSYM WICWhitePointCustom}
  WICWhitePointAutoWhiteBalance  = WICNamedWhitePoint($200);
  {$EXTERNALSYM WICWhitePointAutoWhiteBalance}
  WICWhitePointAsShot            = WICWhitePointDefault;
  {$EXTERNALSYM WICWhitePointAsShot}
  //WICNAMEDWHITEPOINT_FORCE_DWORD = FORCEDWORD;

type
  PWICRawCapabilities = ^WICRawCapabilities;
  WICRawCapabilities = DWord;
  {$EXTERNALSYM WICRawCapabilities}
const
  WICRawCapabilityNotSupported   = WICRawCapabilities(0);
  {$EXTERNALSYM WICRawCapabilityNotSupported}
  WICRawCapabilityGetSupported   = WICRawCapabilities($1);
  {$EXTERNALSYM WICRawCapabilityGetSupported}
  WICRawCapabilityFullySupported = WICRawCapabilities($2);
  {$EXTERNALSYM WICRawCapabilityFullySupported}
  //WICRAWCAPABILITIES_FORCE_DWORD = FORCEDWORD;

type
  PWICRawRotationCapabilities = ^WICRawRotationCapabilities;
  WICRawRotationCapabilities = DWord;
  {$EXTERNALSYM WICRawRotationCapabilities}
const
  WICRawRotationCapabilityNotSupported           = WICRawRotationCapabilities(0);
  {$EXTERNALSYM WICRawRotationCapabilityNotSupported}
  WICRawRotationCapabilityGetSupported           = WICRawRotationCapabilities($1);
  {$EXTERNALSYM WICRawRotationCapabilityGetSupported}
  WICRawRotationCapabilityNinetyDegreesSupported = WICRawRotationCapabilities($2);
  {$EXTERNALSYM WICRawRotationCapabilityNinetyDegreesSupported}
  WICRawRotationCapabilityFullySupported         = WICRawRotationCapabilities($3);
  {$EXTERNALSYM WICRawRotationCapabilityFullySupported}
  //WICRAWROTATIONCAPABILITIES_FORCE_DWORD     = FORCEDWORD;

type
  PWICRawParameterSet = ^WICRawParameterSet;
  WICRawParameterSet = DWord;
  {$EXTERNALSYM WICRawParameterSet}
const
  WICAsShotParameterSet          = WICRawParameterSet($1);
  {$EXTERNALSYM WICAsShotParameterSet}
  WICUserAdjustedParameterSet    = WICRawParameterSet($2);
  {$EXTERNALSYM WICUserAdjustedParameterSet}
  WICAutoAdjustedParameterSet    = WICRawParameterSet($3);
  {$EXTERNALSYM WICAutoAdjustedParameterSet}
  //WICRAWPARAMETERSET_FORCE_DWORD = FORCEDWORD;

type
  PWICRawRenderMode = ^WICRawRenderMode;
  WICRawRenderMode = DWord;
  {$EXTERNALSYM WICRawRenderMode}
const
  WICRawRenderModeDraft        = WICRawRenderMode($1);
  {$EXTERNALSYM WICRawRenderModeDraft}
  WICRawRenderModeNormal       = WICRawRenderMode($2);
  {$EXTERNALSYM WICRawRenderModeNormal}
  WICRawRenderModeBestQuality  = WICRawRenderMode($3);
  {$EXTERNALSYM WICRawRenderModeBestQuality}
  //WICRAWRENDERMODE_FORCE_DWORD = FORCEDWORD;


type
  PWICDdsDimension = ^WICDdsDimension;
  WICDdsDimension = DWord;
  {$EXTERNALSYM WICDdsDimension}
const
  WICDdsTexture1D           = WICDdsDimension(0);
  {$EXTERNALSYM WICDdsTexture1D}
  WICDdsTexture2D           = WICDdsDimension($1);
  {$EXTERNALSYM WICDdsTexture2D}
  WICDdsTexture3D           = WICDdsDimension($2);
  {$EXTERNALSYM WICDdsTexture3D}
  WICDdsTextureCube         = WICDdsDimension($3);
  {$EXTERNALSYM WICDdsTextureCube}
  //WICDDSTEXTURE_FORCE_DWORD = FORCEDWORD;

type
  PWICDdsAlphaMode = ^WICDdsAlphaMode;
  WICDdsAlphaMode = DWord;
  {$EXTERNALSYM WICDdsAlphaMode}
const
  WICDdsAlphaModeUnknown       = WICDdsAlphaMode(0);
  {$EXTERNALSYM WICDdsAlphaModeUnknown}
  WICDdsAlphaModeStraight      = WICDdsAlphaMode($1);
  {$EXTERNALSYM WICDdsAlphaModeStraight}
  WICDdsAlphaModePremultiplied = WICDdsAlphaMode($2);
  {$EXTERNALSYM WICDdsAlphaModePremultiplied}
  WICDdsAlphaModeOpaque        = WICDdsAlphaMode($3);
  {$EXTERNALSYM WICDdsAlphaModeOpaque}
  WICDdsAlphaModeCustom        = WICDdsAlphaMode($4);
  {$EXTERNALSYM WICDdsAlphaModeCustom}
  //WICDDSALPHAMODE_FORCE_DWORD  = FORCEDWORD;


// =============================================================================


type

  PREFWICPixelFormatGUID = ^REFWICPixelFormatGUID;
  REFWICPixelFormatGUID = TGUID;
  {$EXTERNALSYM REFWICPixelFormatGUID}

  PWICPixelFormatGUID = ^WICPixelFormatGUID;
  WICPixelFormatGUID = TGUID;
  {$EXTERNALSYM WICPixelFormatGUID}


  //* Forward Interface Declarations *//

  PIWICPalette = ^IWICPalette;
  IWICPalette = interface;

  PIWICBitmapSource = ^IWICBitmapSource;
  IWICBitmapSource = interface;

  PIWICFormatConverter = ^IWICFormatConverter;
  IWICFormatConverter = interface;

  PIWICPlanarFormatConverter = ^IWICPlanarFormatConverter;
  IWICPlanarFormatConverter = interface;

  PIWICBitmapScaler = ^IWICBitmapScaler;
  IWICBitmapScaler = interface;

  PIWICBitmapClipper = ^IWICBitmapClipper;
  IWICBitmapClipper = interface;

  PIWICBitmapFlipRotator = ^IWICBitmapFlipRotator;
  IWICBitmapFlipRotator = interface;

  PIWICBitmapLock = ^IWICBitmapLock;
  IWICBitmapLock = interface;

  PIWICBitmap = ^IWICBitmap;
  IWICBitmap = interface;

  PIWICColorContext = ^IWICColorContext;
  IWICColorContext = interface;

  PIWICColorTransform = ^IWICColorTransform;
  IWICColorTransform = interface;

  PIWICFastMetadataEncoder = ^IWICFastMetadataEncoder;
  IWICFastMetadataEncoder = interface;

  PIWICStream = ^IWICStream;
  IWICStream = interface;

  PIWICEnumMetadataItem = ^IWICEnumMetadataItem;
  IWICEnumMetadataItem = interface;

  PIWICMetadataQueryReader = ^IWICMetadataQueryReader;
  IWICMetadataQueryReader = interface;

  PIWICMetadataQueryWriter = ^IWICMetadataQueryWriter;
  IWICMetadataQueryWriter = interface;

  PIWICBitmapEncoder = ^IWICBitmapEncoder;
  IWICBitmapEncoder = interface;

  PIWICBitmapFrameEncode = ^IWICBitmapFrameEncode;
  IWICBitmapFrameEncode = interface;

  PIWICPlanarBitmapFrameEncode = ^IWICPlanarBitmapFrameEncode;
  IWICPlanarBitmapFrameEncode = interface;

  PIWICImageEncoder = ^IWICImageEncoder;
  IWICImageEncoder = interface;

  PIWICBitmapDecoder = ^IWICBitmapDecoder;
  IWICBitmapDecoder = interface;

  PIWICBitmapSourceTransform = ^IWICBitmapSourceTransform;
  IWICBitmapSourceTransform = interface;

  PIWICPlanarBitmapSourceTransform = ^IWICPlanarBitmapSourceTransform;
  IWICPlanarBitmapSourceTransform = interface;

  PIWICBitmapFrameDecode = ^IWICBitmapFrameDecode;
  IWICBitmapFrameDecode = interface;

  PIWICProgressiveLevelControl = ^IWICProgressiveLevelControl;
  IWICProgressiveLevelControl = interface;

  PIWICProgressCallback = ^IWICProgressCallback;
  IWICProgressCallback = interface;

  PIWICBitmapCodecProgressNotification = ^IWICBitmapCodecProgressNotification;
  IWICBitmapCodecProgressNotification = interface;

  PIWICComponentInfo = ^IWICComponentInfo;
  IWICComponentInfo = interface;

  PIWICFormatConverterInfo = ^IWICFormatConverterInfo;
  IWICFormatConverterInfo = interface;

  PIWICBitmapCodecInfo = ^IWICBitmapCodecInfo;
  IWICBitmapCodecInfo = interface;

  PIWICBitmapEncoderInfo = ^IWICBitmapEncoderInfo;
  IWICBitmapEncoderInfo = interface;

  PIWICBitmapDecoderInfo = ^IWICBitmapDecoderInfo;
  IWICBitmapDecoderInfo = interface;

  PIWICPixelFormatInfo = ^IWICPixelFormatInfo;
  IWICPixelFormatInfo = interface;

  PIWICPixelFormatInfo2 = ^IWICPixelFormatInfo2;
  IWICPixelFormatInfo2 = interface;

  PIWICImagingFactory = ^IWICImagingFactory;
  IWICImagingFactory = interface;

  PIWICImagingFactory2 = ^IWICImagingFactory2;
  IWICImagingFactory2 = interface;


  D2D1_PIXEL_FORMAT = ^DWORD;
  {$EXTERNALSYM D2D1_PIXEL_FORMAT}

  PWICColor = ^WICColor;
  WICColor = UINT32;
  {$EXTERNALSYM WICColor}

  PWICRect = ^WICRect;
  WICRect = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;
  {$EXTERNALSYM WICRect}


  WICInProcPointer = PByte;
  {$EXTERNALSYM WICInProcPointer}
  PWICInProcPointer = Pointer;
  {$EXTERNALSYM PWICInProcPointer}


  PWICBitmapPattern = ^WICBitmapPattern;
  WICBitmapPattern = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Pattern: PByte;
    { [size_is] }
    Mask: PByte;
    { [size_is] }
    EndOfStream: BOOL;
  end;
  {$EXTERNALSYM WICBitmapPattern}


//#if (_WIN32_WINNT >= _WIN32_WINNT_WIN8) or defined(_WIN7_PLATFORM_UPDATE)

  PWICImageParameters = ^WICImageParameters;
  WICImageParameters = record
    PixelFormat: D2D1_PIXEL_FORMAT;
    DpiX: Single;
    DpiY: Single;
    Top: Single;
    Left: Single;
    PixelWidth: UINT32;
    PixelHeight: UINT32;
  end;
  {$EXTERNALSYM WICImageParameters}

//#endif

  PWICBitmapPlaneDescription = ^WICBitmapPlaneDescription;
  WICBitmapPlaneDescription = record
    Format: WICPixelFormatGUID;
    Width: UINT;
    Height: UINT;
  end;
  {$EXTERNALSYM WICBitmapPlaneDescription}

  PWICBitmapPlane = ^WICBitmapPlane;
  WICBitmapPlane = record
    Format: WICPixelFormatGUID;
    pbBuffer: PByte;
    { [size_is] }
    cbStride: UINT;
    cbBufferSize: UINT;
  end;
  {$EXTERNALSYM WICBitmapPlane}

  PWICJpegFrameHeader = ^WICJpegFrameHeader;
  WICJpegFrameHeader = record
    Width: UINT;
    Height: UINT;
    TransferMatrix: WICJpegTransferMatrix;
    ScanType: WICJpegScanType;
    cComponents: UINT;
    { [range] }
    ComponentIdentifiers: DWORD;
    SampleFactors: DWORD;
    QuantizationTableIndices: DWORD;
  end;
  {$EXTERNALSYM WICJpegFrameHeader}

  PWICJpegScanHeader = ^WICJpegScanHeader;
  WICJpegScanHeader = record
    cComponents: UINT;
    { [range] }
    RestartInterval: UINT;
    ComponentSelectors: DWORD;
    HuffmanTableIndices: DWORD;
    StartSpectralSelection: Byte;
    EndSpectralSelection: Byte;
    SuccessiveApproximationHigh: Byte;
    SuccessiveApproximationLow: Byte;
  end;
  {$EXTERNALSYM WICJpegScanHeader}



  // Interface IWICPalette
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPalette);'}
  {$EXTERNALSYM IWICPalette}
  IWICPalette = interface(IUnknown)
  ['{00000040-a8f2-4877-ba0a-fd2b6645fb94}']
    function InitializePredefined(ePaletteType: WICBitmapPaletteType;
                                  fAddTransparentColor: BOOL): HResult; stdcall;

    function InitializeCustom(pColors: PWICColor; // Array
                              cCount: UINT): HResult; stdcall;

    function InitializeFromBitmap(pISurface: IWICBitmapSource;
                                  cCount: UINT;
                                  fAddTransparentColor: BOOL): HResult; stdcall;

    function InitializeFromPalette(pIPalette: IWICPalette): HResult; stdcall;

    function GetType(var pePaletteType: WICBitmapPaletteType): HResult; stdcall;

    function GetColorCount(var pcCount: UINT): HResult; stdcall;

    function GetColors(cCount: UINT;
                       pColors: PWICColor;
                       var pcActualColors: UINT): HResult; stdcall;

    function IsBlackWhite(var pfIsBlackWhite: BOOL): HResult; stdcall;

    function IsGrayscale(var pfIsGrayscale: BOOL): HResult; stdcall;

    function HasAlpha(var pfHasAlpha: BOOL): HResult; stdcall;
  end;
  IID_IWICPalette = IWICPalette;
  {$EXTERNALSYM IID_IWICPalette}



  // Interface IWICBitmapSource
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapSource);'}
  {$EXTERNALSYM IWICBitmapSource}
  IWICBitmapSource = interface(IUnknown)
  ['{00000120-a8f2-4877-ba0a-fd2b6645fb94}']
    function GetSize(var puiWidth: UINT;
                     var puiHeight: UINT): HResult; stdcall;

    function GetPixelFormat(var pPixelFormat: WICPixelFormatGUID): HResult; stdcall;

    function GetResolution(var pDpiX: Double;
                           var pDpiY: Double): HResult; stdcall;

    function CopyPalette(pIPalette: IWICPalette): HResult; stdcall;

    function CopyPixels(prc: PWICRect;
                        cbStride: UINT;
                        cbBufferSize: UINT;
                        pbBuffer: PByte): HResult; stdcall;
  end;
  IID_IWICBitmapSource = IWICBitmapSource;
  {$EXTERNALSYM IID_IWICBitmapSource}



  // Interface IWICFormatConverter
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICFormatConverter);'}
  {$EXTERNALSYM IWICFormatConverter}
  IWICFormatConverter = interface(IWICBitmapSource)
  ['{00000301-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pISource: IWICBitmapSource;
                        const dstFormat: WICPixelFormatGUID;
                        dither: WICBitmapDitherType;
                        const pIPalette: IWICPalette;
                        alphaThresholdPercent: Double;
                        paletteTranslate: WICBitmapPaletteType): HResult; stdcall;

    function CanConvert(srcPixelFormat: REFWICPixelFormatGUID;
                        dstPixelFormat: REFWICPixelFormatGUID;
                        var pfCanConvert: BOOL): HResult; stdcall;
  end;
  IID_IWICFormatConverter = IWICFormatConverter;
  {$EXTERNALSYM IID_IWICFormatConverter}



  // Interface IWICPlanarFormatConverter
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPlanarFormatConverter);'}
  {$EXTERNALSYM IWICPlanarFormatConverter}
  IWICPlanarFormatConverter = interface(IWICBitmapSource)
  ['{BEBEE9CB-83B0-4DCC-8132-B0AAA55EAC96}']

    function Initialize(ppPlanes: PIWICBitmapSource;
                        cPlanes: UINT;
                        dstFormat: REFWICPixelFormatGUID;
                        dither: WICBitmapDitherType;
                        const pIPalette: IWICPalette;
                        alphaThresholdPercent: Double;
                        paletteTranslate: WICBitmapPaletteType): HResult; stdcall;

    function CanConvert(pSrcPixelFormats: PWICPixelFormatGUID;
                        cSrcPlanes: UINT;
                        dstPixelFormat: REFWICPixelFormatGUID;
                        out pfCanConvert: BOOL): HResult; stdcall;
  end;
  IID_IWICPlanarFormatConverter = IWICPlanarFormatConverter;
  {$EXTERNALSYM IID_IWICPlanarFormatConverter}



  // Interface IWICBitmapScaler
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapScaler);'}
  {$EXTERNALSYM IWICBitmapScaler}
  IWICBitmapScaler = interface(IWICBitmapSource)
  ['{00000302-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pISource: IWICBitmapSource;
                        uiWidth: UINT;
                        uiHeight: UINT;
                        mode: WICBitmapInterpolationMode): HResult; stdcall;
  end;
  IID_IWICBitmapScaler = IWICBitmapScaler;
  {$EXTERNALSYM IID_IWICBitmapScaler}



  // Interface IWICBitmapClipper
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapClipper);'}
  {$EXTERNALSYM IWICBitmapClipper}
  IWICBitmapClipper = interface(IWICBitmapSource)
  ['{E4FBCF03-223D-4e81-9333-D635556DD1B5}']
    function Initialize(pISource: IWICBitmapSource;
                        var prc: WICRect): HResult; stdcall;
  end;
  IID_IWICBitmapClipper = IWICBitmapClipper;
  {$EXTERNALSYM IID_IWICBitmapClipper}



  // Interface IWICBitmapFlipRotator
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapFlipRotator);'}
  {$EXTERNALSYM IWICBitmapFlipRotator}
  IWICBitmapFlipRotator = interface(IWICBitmapSource)
  ['{5009834F-2D6A-41ce-9E1B-17C5AFF7A782}']
    function Initialize(pISource: IWICBitmapSource;
                        options: WICBitmapTransformOptions): HResult; stdcall;
  end;
  IID_IWICBitmapFlipRotator = IWICBitmapFlipRotator;
  {$EXTERNALSYM IID_IWICBitmapFlipRotator}



  // Interface IWICBitmapLock
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapLock);'}
  {$EXTERNALSYM IWICBitmapLock}
  IWICBitmapLock = interface(IUnknown)
  ['{00000123-a8f2-4877-ba0a-fd2b6645fb94}']
    function GetSize(var puiWidth: UINT;
                     var puiHeight: UINT): HResult; stdcall;

    function GetStride(var pcbStride: UINT): HResult; stdcall;

    function GetDataPointer(var pcbBufferSize: UINT;
                            var ppbData: WICInProcPointer): HResult; stdcall;

    function GetPixelFormat(var pPixelFormat: WICPixelFormatGUID): HResult; stdcall;
  end;
  IID_IWICBitmapLock = IWICBitmapLock;
  {$EXTERNALSYM IID_IWICBitmapLock}



  // Interface IWICBitmap
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmap);'}
  {$EXTERNALSYM IWICBitmap}
  IWICBitmap = interface(IWICBitmapSource)
  ['{00000121-a8f2-4877-ba0a-fd2b6645fb94}']
    function Lock(const prcLock: WICRect;
                  flags: DWORD;
                  out ppILock: IWICBitmapLock): HResult; stdcall;

    function SetPalette(pIPalette: IWICPalette): HResult; stdcall;

    function SetResolution(dpiX: Double;
                           dpiY: Double): HResult; stdcall;
  end;
  IID_IWICBitmap = IWICBitmap;
  {$EXTERNALSYM IID_IWICBitmap}



  // Interface IWICColorContext
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICColorContext);'}
  {$EXTERNALSYM IWICColorContext}
  IWICColorContext = interface(IUnknown)
  ['{3C613A02-34B2-44ea-9A7C-45AEA9C6FD6D}']
    function InitializeFromFilename(wzFilename: LPCWSTR): HResult; stdcall;

    function InitializeFromMemory(const pbBuffer: PByte;
                                  cbBufferSize: UINT): HResult; stdcall;

    function InitializeFromExifColorSpace(value: UINT): HResult; stdcall;

    function GetType(var pType: WICColorContextType): HResult; stdcall;

    function GetProfileBytes(cbBuffer: UINT;
                             pbBuffer: PBYTE;
                             var pcbActual: UINT): HResult; stdcall;

    function GetExifColorSpace(var pValue: UINT): HResult; stdcall;
  end;
  IID_IWICColorContext = IWICColorContext;
  {$EXTERNALSYM IID_IWICColorContext}



  // Interface IWICColorTransform
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICColorTransform);'}
  {$EXTERNALSYM IWICColorTransform}
  IWICColorTransform = interface(IWICBitmapSource)
  ['{B66F034F-D0E2-40ab-B436-6DE39E321A94}']
    function Initialize(pIBitmapSource: IWICBitmapSource;
                        pIContextSource: IWICColorContext;
                        pIContextDest: IWICColorContext;
                        pixelFmtDest: REFWICPixelFormatGUID): HResult; stdcall;
  end;
  IID_IWICColorTransform = IWICColorTransform;
  {$EXTERNALSYM IID_IWICColorTransform}



  // Interface IWICFastMetadataEncoder
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICFastMetadataEncoder);'}
  {$EXTERNALSYM IWICFastMetadataEncoder}
  IWICFastMetadataEncoder = interface(IUnknown)
  ['{B84E2C09-78C9-4AC4-8BD3-524AE1663A2F}']
    function Commit(): HResult; stdcall;

    function GetMetadataQueryWriter(out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;
  IID_IWICFastMetadataEncoder = IWICFastMetadataEncoder;
  {$EXTERNALSYM IID_IWICFastMetadataEncoder}



  // Interface IWICStream
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICStream);'}
  {$EXTERNALSYM IWICStream}
  IWICStream = interface(IStream)
  ['{135FF860-22B7-4ddf-B0F6-218F4F299A43}']
    function InitializeFromIStream(pIStream: IStream): HResult; stdcall;

    function InitializeFromFilename(wzFileName: LPCWSTR;
                                    dwDesiredAccess: DWORD): HResult; stdcall;

    function InitializeFromMemory(pbBuffer: WICInProcPointer;
                                  cbBufferSize: DWORD): HResult; stdcall;

    function InitializeFromIStreamRegion(pIStream: IStream;
                                         ulOffset: ULARGE_INTEGER;
                                         ulMaxSize: ULARGE_INTEGER): HResult; stdcall;
  end;
  IID_IWICStream = IWICStream;
  {$EXTERNALSYM IID_IWICStream}



  // Interface IWICEnumMetadataItem
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICEnumMetadataItem);'}
  {$EXTERNALSYM IWICEnumMetadataItem}
  IWICEnumMetadataItem = interface(IUnknown)
  ['{DC2BB46D-3F07-481E-8625-220C4AEDBB33}']
    function Next(celt: Cardinal;
                  rgeltSchema: PPROPVARIANT;
                  rgeltID: PPROPVARIANT;
                  rgeltValue: PPROPVARIANT;
                  var pceltFetched: ULONG): HResult; stdcall;

    function Skip(celt: Cardinal): HResult; stdcall;

    function Reset: HResult; stdcall;

    function Clone(out ppIEnumMetadataItem: IWICEnumMetadataItem): HResult; stdcall;
  end;
  IID_IWICEnumMetadataItem = IWICEnumMetadataItem;
  {$EXTERNALSYM IID_IWICEnumMetadataItem}



  // Interface IWICMetadataQueryReader
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataQueryReader);'}
  {$EXTERNALSYM IWICMetadataQueryReader}
  IWICMetadataQueryReader = interface(IUnknown)
  ['{30989668-E1C9-4597-B395-458EEDB808DF}']
    function GetContainerFormat(var pguidContainerFormat: TGuid): HResult; stdcall;

    function GetLocation(cchMaxLength: UINT;
                         wzNamespace: PWCHAR;
                         var pcchActualLength: UINT): HResult; stdcall;

    function GetMetadataByName(wzName: LPCWSTR;
                               var pvarValue: PROPVARIANT): HResult; stdcall;

    function GetEnumerator(out ppIEnumString: IEnumString): HResult; stdcall;
  end;
  IID_IWICMetadataQueryReader = IWICMetadataQueryReader;
  {$EXTERNALSYM IID_IWICMetadataQueryReader}



  // Interface IWICMetadataQueryWriter
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataQueryWriter);'}
  {$EXTERNALSYM IWICMetadataQueryWriter}
  IWICMetadataQueryWriter = interface(IWICMetadataQueryReader)
  ['{A721791A-0DEF-4d06-BD91-2118BF1DB10B}']
    function SetMetadataByName(wzName: LPCWSTR;
                               const pvarValue: PROPVARIANT): HResult; stdcall;

    function RemoveMetadataByName(wzName: LPCWSTR): HResult; stdcall;
  end;
  IID_IWICMetadataQueryWriter = IWICMetadataQueryWriter;
  {$EXTERNALSYM IID_IWICMetadataQueryWriter}



  // Interface IWICBitmapEncoder
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapEncoder);'}
  {$EXTERNALSYM IWICBitmapEncoder}
  IWICBitmapEncoder = interface(IUnknown)
  ['{00000103-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pIStream: IStream;
                        cacheOption: WICBitmapEncoderCacheOption): HResult; stdcall;

    function GetContainerFormat(var pguidContainerFormat: TGuid): HResult; stdcall;

    function GetEncoderInfo(out ppIEncoderInfo: IWICBitmapEncoderInfo): HResult; stdcall;

    function SetColorContexts(cCount: UINT;
                              ppIColorContext: PIWICColorContext): HResult; stdcall;

    function SetPalette(pIPalette: IWICPalette): HResult; stdcall;

    function SetThumbnail(pIThumbnail: IWICBitmapSource): HResult; stdcall;

    function SetPreview(pIPreview: IWICBitmapSource): HResult; stdcall;

    function CreateNewFrame(out ppIFrameEncode: IWICBitmapFrameEncode;
                            var ppIEncoderOptions: IPropertyBag2): HResult; stdcall;

    function Commit(): HResult; stdcall;

    function GetMetadataQueryWriter(out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;
  IID_IWICBitmapEncoder = IWICBitmapEncoder;
  {$EXTERNALSYM IID_IWICBitmapEncoder}


  // Interface IWICBitmapFrameEncode
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapFrameEncode);'}
  {$EXTERNALSYM IWICBitmapFrameEncode}
  IWICBitmapFrameEncode = interface(IUnknown)
  ['{00000105-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pIEncoderOptions: IPropertyBag2): HResult; stdcall;

    function SetSize(uiWidth: UINT;
                     uiHeight: UINT): HResult; stdcall;

    function SetResolution(dpiX: Double;
                           dpiY: Double): HResult; stdcall;

    function SetPixelFormat(var pPixelFormat: WICPixelFormatGUID): HResult; stdcall;

    function SetColorContexts(cCount: UINT;
                              ppIColorContext: PIWICColorContext): HResult; stdcall;

    function SetPalette(pIPalette: PIWICPalette): HResult; stdcall;

    function SetThumbnail(pIThumbnail: PIWICBitmapSource): HResult; stdcall;

    function WritePixels(lineCount: UINT;
                         cbStride: UINT;
                         cbBufferSize: UINT;
                         pbPixels: PByte): HResult; stdcall;

    function WriteSource(pIBitmapSource: PIWICBitmapSource;
                         prc: PWICRect): HResult; stdcall;

    function Commit(): HResult; stdcall;

    function GetMetadataQueryWriter(out ppIMetadataQueryWriter: PIWICMetadataQueryWriter): HResult; stdcall;

  end;
  IID_IWICBitmapFrameEncode = IWICBitmapFrameEncode;
  {$EXTERNALSYM IID_IWICBitmapFrameEncode}



  // Interface IWICPlanarBitmapFrameEncode
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPlanarBitmapFrameEncode);'}
  {$EXTERNALSYM IWICPlanarBitmapFrameEncode}
  IWICPlanarBitmapFrameEncode = interface(IUnknown)
  ['{F928B7B8-2221-40C1-B72E-7E82F1974D1A}']
    function WritePixels(lineCount: UINT;
                         pPlanes: PWICBitmapPlane;  // pointer to array
                         cPlanes: UINT): HResult; stdcall;

    function WriteSource(ppPlanes: PIWICBitmapSource; // pointer to array
                         cPlanes: UINT;
                         prcSource: PWICRect): HResult; stdcall;
  end;
  IID_IWICPlanarBitmapFrameEncode = IWICPlanarBitmapFrameEncode;
  {$EXTERNALSYM IID_IWICPlanarBitmapFrameEncode}


// #if (_WIN32_WINNT >= _WIN32_WINNT_WIN8) || defined(_WIN7_PLATFORM_UPDATE)

  // Interface IWICImageEncoder
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICImageEncoder);'}
  {$EXTERNALSYM IWICImageEncoder}
  IWICImageEncoder = interface(IUnknown)
  ['{04C75BF8-3CE1-473B-ACC5-3CC4F5E94999}']
    function WriteFrame(pImage: ID2D1Image;
                        pFrameEncode: IWICBitmapFrameEncode;
                        const pImageParameters: WICImageParameters): HResult; stdcall;

    function WriteFrameThumbnail(pImage: ID2D1Image;
                                 pFrameEncode: IWICBitmapFrameEncode;
                                 const pImageParameters: WICImageParameters): HResult; stdcall;

    function WriteThumbnail(pImage: ID2D1Image;
                            pEncoder: IWICBitmapEncoder;
                            const pImageParameters: WICImageParameters): HResult; stdcall;
  end;
  IID_IWICImageEncoder = IWICImageEncoder;
  {$EXTERNALSYM IID_IWICImageEncoder}



  // Interface IWICBitmapDecoder
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapDecoder);'}
  {$EXTERNALSYM IWICBitmapDecoder}
  IWICBitmapDecoder = interface(IUnknown)
  ['{9EDDE9E7-8DEE-47ea-99DF-E6FAF2ED44BF}']
    function QueryCapability(pIStream: IStream;
                             out pdwCapability: DWORD): HResult; stdcall;

    function Initialize(pIStream: IStream;
                        cacheOptions: WICDecodeOptions): HResult; stdcall;

    function GetContainerFormat(out pguidContainerFormat: TGuid): HResult; stdcall;

    function GetDecoderInfo(out ppIDecoderInfo: IWICBitmapDecoderInfo): HResult; stdcall;

    function CopyPalette(pIPalette: IWICPalette): HResult; stdcall;

    function GetMetadataQueryReader(out ppIMetadataQueryReader: IWICMetadataQueryReader): HResult; stdcall;

    function GetPreview(out ppIBitmapSource: IWICBitmapSource): HResult; stdcall;

    function GetColorContexts(cCount: UINT;
                              var ppIColorContexts: IWICColorContext;
                              out pcActualCount: UINT): HResult; stdcall;

    function GetThumbnail(out ppIThumbnail: IWICBitmapSource): HResult; stdcall;

    function GetFrameCount(out pCount: UINT): HResult; stdcall;

    function GetFrame(index: UINT;
                      out ppIBitmapFrame: IWICBitmapFrameDecode): HResult; stdcall;
  end;
  IID_IWICBitmapDecoder = IWICBitmapDecoder;
  {$EXTERNALSYM IID_IWICBitmapDecoder}



  // Interface IWICBitmapSourceTransform
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapSourceTransform);'}
  {$EXTERNALSYM IWICBitmapSourceTransform}
  IWICBitmapSourceTransform = interface(IUnknown)
  ['{3B16811B-6A43-4ec9-B713-3D5A0C13B940}']
    function CopyPixels(const prc: WICRect;
                        uiWidth: UINT;
                        uiHeight: UINT;
                        pguidDstFormat: WICPixelFormatGUID;
                        dstTransform: WICBitmapTransformOptions;
                        nStride: UINT;
                        cbBufferSize: UINT;
                        out pbBuffer: PByte): HResult; stdcall;

    function GetClosestSize(var puiWidth: UINT;
                            var puiHeight: UINT): HResult; stdcall;

    function GetClosestPixelFormat(var pguidDstFormat: WICPixelFormatGUID): HResult; stdcall;

    function DoesSupportTransform(const dstTransform: WICBitmapTransformOptions;
                                  out pfIsSupported: PBOOL): HResult; stdcall;
  end;
  IID_IWICBitmapSourceTransform = IWICBitmapSourceTransform;
  {$EXTERNALSYM IID_IWICBitmapSourceTransform}



  // Interface IWICPlanarBitmapSourceTransform
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPlanarBitmapSourceTransform);'}
  {$EXTERNALSYM IWICPlanarBitmapSourceTransform}
  IWICPlanarBitmapSourceTransform = interface(IUnknown)
  ['{3AFF9CCE-BE95-4303-B927-E7D16FF4A613}']
    function DoesSupportTransform(var puiWidth: UINT;
                                  var puiHeight: UINT;
                                  dstTransform: WICBitmapTransformOptions;
                                  dstPlanarOptions: WICPlanarOptions;
                                  pguidDstFormats: PWICPixelFormatGUID;
                                  pPlaneDescriptions: PWICBitmapPlaneDescription;
                                  cPlanes: UINT;
                                  out pfIsSupported: BOOL): HResult; stdcall;

    function CopyPixels(const prcSource: WICRect;
                        uiWidth: UINT;
                        uiHeight: UINT;
                        dstTransform: WICBitmapTransformOptions;
                        dstPlanarOptions: WICPlanarOptions;
                        pDstPlanes: PWICBitmapPlane;
                        cPlanes: UINT): HResult; stdcall;
  end;
  IID_IWICPlanarBitmapSourceTransform = IWICPlanarBitmapSourceTransform;
  {$EXTERNALSYM IID_IWICPlanarBitmapSourceTransform}



  // Interface IWICBitmapFrameDecode
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapFrameDecode);'}
  {$EXTERNALSYM IWICBitmapFrameDecode}
  IWICBitmapFrameDecode = interface(IWICBitmapSource)
  ['{3B16811B-6A43-4ec9-A813-3D930C13B940}']
    function GetMetadataQueryReader(ppIMetadataQueryReader: IWICMetadataQueryReader): HResult; stdcall;

    function GetColorContexts(cCount: UINT;
                              ppIColorContexts: PIWICColorContext;
                              var pcActualCount: UINT): HResult; stdcall;

    function GetThumbnail(var ppIThumbnail: IWICBitmapSource): HResult; stdcall;
  end;
  IID_IWICBitmapFrameDecode = IWICBitmapFrameDecode;
  {$EXTERNALSYM IID_IWICBitmapFrameDecode}



  // Interface IWICProgressiveLevelControl
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICProgressiveLevelControl);'}
  {$EXTERNALSYM IWICProgressiveLevelControl}
  IWICProgressiveLevelControl = interface(IUnknown)
  ['{DAAC296F-7AA5-4dbf-8D15-225C5976F891}']
    function GetLevelCount(out pcLevels: UINT): HResult; stdcall;

    function GetCurrentLevel(out pnLevel: UINT): HResult; stdcall;

    function SetCurrentLevel(nLevel: UINT): HResult; stdcall;
  end;
  IID_IWICProgressiveLevelControl = IWICProgressiveLevelControl;
  {$EXTERNALSYM IID_IWICProgressiveLevelControl}



  // Interface IWICProgressCallback
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICProgressCallback);'}
  {$EXTERNALSYM IWICProgressCallback}
  IWICProgressCallback = interface(IUnknown)
  ['{4776F9CD-9517-45FA-BF24-E89C5EC5C60C}']
    function Notify(uFrameNum: ULONG;
                    operation: WICProgressOperation;
                    dblProgress: Double): HResult; stdcall;
  end;
  IID_IWICProgressCallback = IWICProgressCallback;
  {$EXTERNALSYM IID_IWICProgressCallback}


  // belongs to interface IWICBitmapCodecProgressNotification
  // Application defined callback function called when codec component progress is made.
  FNProgressNotification = function(pvData: Pointer;
                                    uFrameNum: ULONG;
                                    operation: WICProgressOperation;
                                    dblProgress: Double): HResult; stdcall;
  {$EXTERNALSYM FNProgressNotification}


  // Interface IWICBitmapCodecProgressNotification
  // =============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapCodecProgressNotification);'}
  {$EXTERNALSYM IWICBitmapCodecProgressNotification}
  IWICBitmapCodecProgressNotification = interface(IUnknown)
  ['{64C1024E-C3CF-4462-8078-88C2B11C46D9}']
    function RegisterProgressNotification(pfnProgressNotification: FNProgressNotification;
                                          pvData: Pointer;
                                          dwProgressFlags: DWORD): HResult; stdcall;
  end;
  IID_IWICBitmapCodecProgressNotification = IWICBitmapCodecProgressNotification;
  {$EXTERNALSYM IID_IWICBitmapCodecProgressNotification}



  // Interface IWICComponentInfo
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICComponentInfo);'}
  {$EXTERNALSYM IWICComponentInfo}
  IWICComponentInfo = interface(IUnknown)
  ['{23BC3F0A-698B-4357-886B-F24D50671334}']
    function GetComponentType(var pType: WICComponentType): HResult; stdcall;

    function GetCLSID(var pclsid: TGuid): HResult; stdcall;

    function GetSigningStatus(var pStatus: DWORD): HResult; stdcall;

    function GetAuthor(cchAuthor: UINT;
                       wzAuthor: PWCHAR;
                       var pcchActual: UINT): HResult; stdcall;

    function GetVendorGUID(var pguidVendor: TGuid): HResult; stdcall;

    function GetVersion(cchVersion: UINT;
                        wzVersion: PWCHAR;
                        var pcchActual: UINT): HResult; stdcall;

    function GetSpecVersion(cchSpecVersion: UINT;
                            wzSpecVersion: PWCHAR;
                            var pcchActual: UINT): HResult; stdcall;

    function GetFriendlyName(cchFriendlyName: UINT;
                             wzFriendlyName: PWCHAR;
                             var pcchActual: UINT): HResult; stdcall;
  end;
  IID_IWICComponentInfo = IWICComponentInfo;
  {$EXTERNALSYM IID_IWICComponentInfo}



  // Interface IWICFormatConverterInfo
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICFormatConverterInfo);'}
  {$EXTERNALSYM IWICFormatConverterInfo}
  IWICFormatConverterInfo = interface(IWICComponentInfo)
  ['{9F34FB65-13F4-4f15-BC57-3726B5E53D9F}']
    function GetPixelFormats(cFormats: UINT;
                             pPixleFormatGUIDs: PWICPixelFormatGUID;
                             var pcActual: UINT): HResult; stdcall;

    function CreateInstance(out ppIConverter: IWICFormatConverter): HResult; stdcall;
  end;
  IID_IWICFormatConverterInfo = IWICFormatConverterInfo;
  {$EXTERNALSYM IID_IWICFormatConverterInfo}



  // Interface IWICBitmapCodecInfo
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapCodecInfo);'}
  {$EXTERNALSYM IWICBitmapCodecInfo}
  IWICBitmapCodecInfo = interface(IWICComponentInfo)
  ['{E87A44C4-B76E-4c47-8B09-298EB12A2714}']
    function GetContainerFormat(var pguidContainerFormat: TGuid): HResult; stdcall;

    function GetPixelFormats(cFormats: UINT;
                             var guidPixelFormats: PGuid;
                             var pcActual: UINT): HResult; stdcall;

    function GetColorManagementVersion(cchColorManagementVersion: UINT;
                                       wzColorManagementVersion: PWCHAR;
                                       var pcchActual: UINT): HResult; stdcall;

    function GetDeviceManufacturer(cchDeviceManufacturer: UINT;
                                   wzDeviceManufacturer: PWCHAR;
                                   var pcchActual: UINT): HResult; stdcall;

    function GetDeviceModels(cchDeviceModels: UINT;
                             wzDeviceModels: PWCHAR;
                             var pcchActual: UINT): HResult; stdcall;

    function GetMimeTypes(cchMimeTypes: UINT;
                          wzMimeTypes: PWCHAR;
                          var pcchActual: UINT): HResult; stdcall;

    function GetFileExtensions(cchFileExtensions: UINT;
                               wzFileExtensions: PWCHAR;
                               var pcchActual: UINT): HResult; stdcall;

    function DoesSupportAnimation(var pfSupportAnimation: BOOL): HResult; stdcall;

    function DoesSupportChromakey(var pfSupportChromakey: BOOL): HResult; stdcall;

    function DoesSupportLossless(var pfSupportLossless: BOOL): HResult; stdcall;

    function DoesSupportMultiframe(var pfSupportMultiframe: BOOL): HResult; stdcall;

    function MatchesMimeType(wzMimeType: LPCWSTR;
                             var pfMatches: BOOL): HResult; stdcall;
  end;
  IID_IWICBitmapCodecInfo = IWICBitmapCodecInfo;
  {$EXTERNALSYM IID_IWICBitmapCodecInfo}



  // Interface IWICBitmapEncoderInfo
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapEncoderInfo);'}
  {$EXTERNALSYM IWICBitmapEncoderInfo}
  IWICBitmapEncoderInfo = interface(IWICBitmapCodecInfo)
  ['{94C9B4EE-A09F-4f92-8A1E-4A9BCE7E76FB}']
    function CreateInstance(out ppIBitmapEncoder: IWICBitmapEncoder): HResult; stdcall;
  end;
  IID_IWICBitmapEncoderInfo = IWICBitmapEncoderInfo;
  {$EXTERNALSYM IID_IWICBitmapEncoderInfo}


  // Interface IWICBitmapDecoderInfo
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapDecoderInfo);'}
  {$EXTERNALSYM IWICBitmapDecoderInfo}
  IWICBitmapDecoderInfo = interface(IWICBitmapCodecInfo)
  ['{D8CD007F-D08F-4191-9BFC-236EA7F0E4B5}']
    function GetPatterns(cbSizePatterns: UINT;
                         pPatterns: PWICBitmapPattern;   // pointer to array
                         var pcPatterns: UINT;
                         var pcbPatternsActual: UINT): HResult; stdcall;

    function MatchesPattern(pIStream: IStream;
                            var pfMatches: BOOL): HResult; stdcall;

    function CreateInstance(out ppIBitmapDecoder: IWICBitmapDecoder): HResult; stdcall;
  end;
  IID_IWICBitmapDecoderInfo = IWICBitmapDecoderInfo;
  {$EXTERNALSYM IID_IWICBitmapDecoderInfo}


  // Interface IWICPixelFormatInfo
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPixelFormatInfo);'}
  {$EXTERNALSYM IWICPixelFormatInfo}
  IWICPixelFormatInfo = interface(IWICComponentInfo)
  ['{E8EDA601-3D48-431a-AB44-69059BE88BBE}']
    function GetFormatGUID(var pFormat: TGuid): HResult; stdcall;

    function GetColorContext(out ppIColorContext: IWICColorContext): HResult; stdcall;

    function GetBitsPerPixel(var puiBitsPerPixel: UINT): HResult; stdcall;

    function GetChannelCount(var puiChannelCount: UINT): HResult; stdcall;

    function GetChannelMask(uiChannelIndex: UINT;
                            cbMaskBuffer: UINT;
                            pbMaskBuffer: PBYTE;
                            var pcbActual: UINT): HResult; stdcall;
  end;
  IID_IWICPixelFormatInfo = IWICPixelFormatInfo;
  {$EXTERNALSYM IID_IWICPixelFormatInfo}


  // Interface IWICPixelFormatInfo2
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPixelFormatInfo2);'}
  {$EXTERNALSYM IWICPixelFormatInfo2}
  IWICPixelFormatInfo2 = interface(IWICPixelFormatInfo)
  ['{A9DB33A2-AF5F-43C7-B679-74F5984B5AA4}']
    function SupportsTransparency(var pfSupportsTransparency: BOOL): HResult; stdcall;

    function GetNumericRepresentation(var pNumericRepresentation: WICPixelFormatNumericRepresentation): HResult; stdcall;
  end;
  IID_IWICPixelFormatInfo2 = IWICPixelFormatInfo2;
  {$EXTERNALSYM IID_IWICPixelFormatInfo2}


  // Interface IWICImagingFactory
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICImagingFactory);'}
  {$EXTERNALSYM IWICImagingFactory}
  IWICImagingFactory = interface(IUnknown)
  ['{ec5ec8a9-c395-4314-9c77-54d7a935ff70}']
    function CreateDecoderFromFilename(wzFilename: LPCWSTR;
                                       const pguidVendor: TGuid;
                                       dwDesiredAccess: DWORD;
                                       metadataOptions: WICDecodeOptions;
                                       out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;

    function CreateDecoderFromStream(pIStream: IStream;
                                     const pguidVendor: TGuid;
                                     metadataOptions: WICDecodeOptions;
                                     out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;

    function CreateDecoderFromFileHandle(hFile: ULONG_PTR;
                                         const pguidVendor: TGuid;
                                         metadataOptions: WICDecodeOptions;
                                         out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;

    function CreateComponentInfo(const clsidComponent: TGuid;
                                 out ppIInfo: IWICComponentInfo): HResult; stdcall;

    function CreateDecoder(const guidContainerFormat: TGuid;
                           const pguidVendor: TGuid;
                           out ppIDecoder: IWICBitmapDecoder): HResult; stdcall;

    function CreateEncoder(const guidContainerFormat: TGuid;
                           const pguidVendor: TGuid;
                           out ppIEncoder: IWICBitmapEncoder): HResult; stdcall;

    function CreatePalette(out ppIPalette: IWICPalette): HResult; stdcall;

    function CreateFormatConverter(out ppIFormatConverter: IWICFormatConverter): HResult; stdcall;

    function CreateBitmapScaler(out ppIBitmapScaler: IWICBitmapScaler): HResult; stdcall;

    function CreateBitmapClipper(out ppIBitmapClipper: IWICBitmapClipper): HResult; stdcall;

    function CreateBitmapFlipRotator(out ppIBitmapFlipRotator: IWICBitmapFlipRotator): HResult; stdcall;

    function CreateStream(out ppIWICStream: IWICStream): HResult; stdcall;

    function CreateColorContext(out ppIWICColorContext: IWICColorContext): HResult; stdcall;

    function CreateColorTransformer(out ppIWICColorTransform: IWICColorTransform): HResult; stdcall;

    function CreateBitmap(uiWidth: UINT;
                          uiHeight: UINT;
                          pixelFormat: REFWICPixelFormatGUID;
                          option: WICBitmapCreateCacheOption;
                          out ppIBitmap: IWICBitmap): HResult; stdcall;

    function CreateBitmapFromSource(pIBitmapSource: IWICBitmapSource;
                                    option: WICBitmapCreateCacheOption;
                                    out ppIBitmap: IWICBitmap): HResult; stdcall;

    function CreateBitmapFromSourceRect(pIBitmapSource: IWICBitmapSource;
                                        x: UINT;
                                        y: UINT;
                                        width: UINT;
                                        height: UINT;
                                        out ppIBitmap: IWICBitmap): HResult; stdcall;

    function CreateBitmapFromMemory(uiWidth: UINT;
                                    uiHeight: UINT;
                                    const pixelFormat: WICPixelFormatGUID;
                                    cbStride: UINT;
                                    cbBufferSize: UINT;
                                    pbBuffer: PByte;
                                    out ppIBitmap: IWICBitmap): HResult; stdcall;

    function CreateBitmapFromHBITMAP(_hBitmap: HBITMAP;
                                     _hPalette: HPALETTE;
                                     options: WICBitmapAlphaChannelOption;
                                     out ppIBitmap: IWICBitmap): HResult; stdcall;

    function CreateBitmapFromHICON(_hIcon: HICON;
                                   out ppIBitmap: IWICBitmap): HResult; stdcall;

    function CreateComponentEnumerator(componentTypes: DWORD;
                                       options: DWORD;
                                       out ppIEnumUnknown: IEnumUnknown): HResult; stdcall;

    function CreateFastMetadataEncoderFromDecoder(pDecoder: IWICBitmapDecoder;
                                                  out ppIFastEncoder: IWICFastMetadataEncoder): HResult; stdcall;

    function CreateFastMetadataEncoderFromFrameDecode(pFrameDecoder: IWICBitmapFrameDecode;
                                                      out ppIFastEncoder: IWICFastMetadataEncoder): HResult; stdcall;

    function CreateQueryWriter(const guidMetadataFormat: TGuid;
                               const pguidVendor: TGuid;
                               out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;

    function CreateQueryWriterFromReader(pIQueryReader: IWICMetadataQueryReader;
                                         const pguidVendor: TGuid;
                                         out ppIQueryWriter: IWICMetadataQueryWriter): HResult; stdcall;
  end;
  IID_IWICImagingFactory = IWICImagingFactory;
  {$EXTERNALSYM IID_IWICImagingFactory}


//#if (_WIN32_WINNT >= _WIN32_WINNT_WIN8) || defined(_WIN7_PLATFORM_UPDATE)

  // Interface IWICImagingFactory2
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICImagingFactory2);'}
  {$EXTERNALSYM IWICImagingFactory2}
  IWICImagingFactory2 = interface(IWICImagingFactory)
  ['{7B816B45-1996-4476-B132-DE9E247C8AF0}']
    function CreateImageEncoder(pD2DDevice: ID2D1Device;
                                out ppWICImageEncoder: IWICImageEncoder): HResult; stdcall;
  end;
  IID_IWICImagingFactory2 = IWICImagingFactory2;
  {$EXTERNALSYM IID_IWICImagingFactory2}



  function WICConvertBitmapSource(const dstFormat: WICPixelFormatGUID;  // Destination pixel format
                                  pISrc: IWICBitmapSource;  // Source bitmap
                                  out ppIDst: IWICBitmapSource  // Destination bitmap, a copy or addrefed source
                                  ): HResult; stdcall;
  {$EXTERNALSYM WICConvertBitmapSource}


  function WICCreateBitmapFromSection(width: UINT;
                                      height: UINT;
                                      const pixelFormat: WICPixelFormatGUID;
                                      hSection: THandle;
                                      stride: UINT;
                                      offset: UINT;
                                      out ppIBitmap: IWICBitmap): HResult; stdcall;
  {$EXTERNALSYM WICCreateBitmapFromSection}


  function WICCreateBitmapFromSectionEx(width: UINT;
                                        height: UINT;
                                        const pixelFormat: WICPixelFormatGUID;
                                        hSection: THandle;
                                        stride: UINT;
                                        offset: UINT;
                                        desiredAccessLevel: WICSectionAccessLevel;
                                        out ppIBitmap: IWICBitmap): HResult; stdcall;
  {$EXTERNALSYM WICCreateBitmapFromSectionEx}


  function WICMapGuidToShortName(const guid: TGuid;
                                 cchName: UINT;
                                 wzName: PWCHAR;
                                 var pcchActual: UINT): HResult; stdcall;
  {$EXTERNALSYM WICMapGuidToShortName}


  function WICMapShortNameToGuid(wzName: PWCHAR;
                                 var pguid: TGuid): HResult; stdcall;
  {$EXTERNALSYM WICMapShortNameToGuid}


  function WICMapSchemaToName(const guidMetadataFormat: TGuid;
                              pwzSchema: LPWSTR;
                              cchName: UINT;
                              var wzName: WCHAR;
                              var pcchActual: UINT): HResult; stdcall;
  {$EXTERNALSYM WICMapSchemaToName}

type
  PWICRawCapabilitiesInfo = ^WICRawCapabilitiesInfo;
  WICRawCapabilitiesInfo = record
    cbSize: UINT;
    CodecMajorVersion: UINT;
    CodecMinorVersion: UINT;
    ExposureCompensationSupport: WICRawCapabilities;
    ContrastSupport: WICRawCapabilities;
    RGBWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupportMask: UINT;
    KelvinWhitePointSupport: WICRawCapabilities;
    GammaSupport: WICRawCapabilities;
    TintSupport: WICRawCapabilities;
    SaturationSupport: WICRawCapabilities;
    SharpnessSupport: WICRawCapabilities;
    NoiseReductionSupport: WICRawCapabilities;
    DestinationColorProfileSupport: WICRawCapabilities;
    ToneCurveSupport: WICRawCapabilities;
    RotationSupport: WICRawRotationCapabilities;
    RenderModeSupport: WICRawCapabilities;
  end;
  {$EXTERNALSYM WICRawCapabilitiesInfo}


  PWICRawToneCurvePoint = ^WICRawToneCurvePoint;
  WICRawToneCurvePoint = record
    Input: Double;
    Output: Double;
  end;
  {$EXTERNALSYM WICRawToneCurvePoint}

  PWICRawToneCurve = ^WICRawToneCurve;
  WICRawToneCurve = record
    cPoints: UINT;
    aPoints: array[0..0] of WICRawToneCurvePoint;
  end;
  {$EXTERNALSYM WICRawToneCurve}


  PIWICDevelopRawNotificationCallback = ^IWICDevelopRawNotificationCallback;
  IWICDevelopRawNotificationCallback = interface;

  PIWICDevelopRaw = ^IWICDevelopRaw;
  IWICDevelopRaw = interface;

  PIWICDdsDecoder = ^IWICDdsDecoder;
  IWICDdsDecoder = interface;

  PIWICDdsEncoder = ^IWICDdsEncoder;
  IWICDdsEncoder = interface;

  PIWICDdsFrameDecode = ^IWICDdsFrameDecode;
  IWICDdsFrameDecode = interface;

  PIWICJpegFrameDecode = ^IWICJpegFrameDecode;
  IWICJpegFrameDecode = interface;

  PIWICJpegFrameEncode = ^IWICJpegFrameEncode;
  IWICJpegFrameEncode = interface;



  // Interface IWICDevelopRawNotificationCallback
  // ============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICDevelopRawNotificationCallback);'}
  {$EXTERNALSYM IWICDevelopRawNotificationCallback}
  IWICDevelopRawNotificationCallback = interface(IUnknown)
  ['{95c75a6e-3e8c-4ec2-85a8-aebcc551e59b}']
    function Notify(NotificationMask: UINT): HResult; stdcall;
  end;
  IID_IWICDevelopRawNotificationCallback = IWICDevelopRawNotificationCallback;
  {$EXTERNALSYM IID_IWICDevelopRawNotificationCallback}


  // Interface IWICDevelopRaw
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICDevelopRaw);'}
  {$EXTERNALSYM IWICDevelopRaw}
  IWICDevelopRaw = interface(IWICBitmapFrameDecode)
  ['{fbec5e44-f7be-4b65-b7f8-c0c81fef026d}']
    function QueryRawCapabilitiesInfo(var pInfo: WICRawCapabilitiesInfo): HResult; stdcall;

    function LoadParameterSet(ParameterSet: WICRawParameterSet): HResult; stdcall;

    function GetCurrentParameterSet(out ppCurrentParameterSet: IPropertyBag2): HResult; stdcall;

    function SetExposureCompensation(ev: Double): HResult; stdcall;

    function GetExposureCompensation(var pEV: Double): HResult; stdcall;

    function SetWhitePointRGB(Red: UINT;
                              Green: UINT;
                              Blue: UINT): HResult; stdcall;

    function GetWhitePointRGB(var pRed: UINT;
                              var pGreen: UINT;
                              var pBlue: UINT): HResult; stdcall;

    function SetNamedWhitePoint(WhitePoint: WICNamedWhitePoint): HResult; stdcall;

    function GetNamedWhitePoint(var pWhitePoint: WICNamedWhitePoint): HResult; stdcall;

    function SetWhitePointKelvin(WhitePointKelvin: UINT): HResult; stdcall;

    function GetWhitePointKelvin(var pWhitePointKelvin: UINT): HResult; stdcall;

    function GetKelvinRangeInfo(var pMinKelvinTemp: UINT;
                                var pMaxKelvinTemp: UINT;
                                var pKelvinTempStepValue: UINT): HResult; stdcall;

    function SetContrast(Contrast: Double): HResult; stdcall;

    function GetContrast(var pContrast: Double): HResult; stdcall;

    function SetGamma(Gamma: Double): HResult; stdcall;

    function GetGamma(var pGamma: Double): HResult; stdcall;

    function SetSharpness(Sharpness: Double): HResult; stdcall;

    function GetSharpness(var pSharpness: Double): HResult; stdcall;

    function SetSaturation(Saturation: Double): HResult; stdcall;

    function GetSaturation(var pSaturation: Double): HResult; stdcall;

    function SetTint(Tint: Double): HResult; stdcall;

    function GetTint(var pTint: Double): HResult; stdcall;

    function SetNoiseReduction(NoiseReduction: Double): HResult; stdcall;

    function GetNoiseReduction(var pNoiseReduction: Double): HResult; stdcall;

    function SetDestinationColorContext(pColorContext: IWICColorContext): HResult; stdcall;

    function SetToneCurve(cbToneCurveSize: UINT;
                          pToneCurve: PWICRawToneCurve): HResult; stdcall;

    function GetToneCurve(cbToneCurveBufferSize: UINT;
                          pToneCurve: PWICRawToneCurve;
                          var pcbActualToneCurveBufferSize: UINT): HResult; stdcall;

    function SetRotation(Rotation: Double): HResult; stdcall;

    function GetRotation(var pRotation: Double): HResult; stdcall;

    function SetRenderMode(RenderMode: WICRawRenderMode): HResult; stdcall;

    function GetRenderMode(var pRenderMode: WICRawRenderMode): HResult; stdcall;

    function SetNotificationCallback(pCallback: IWICDevelopRawNotificationCallback): HResult; stdcall;
  end;
  IID_IWICDevelopRaw = IWICDevelopRaw;
  {$EXTERNALSYM IID_IWICDevelopRaw}


  PWICDdsParameters = ^WICDdsParameters;
  WICDdsParameters = record
    Width: UINT;
    Height: UINT;
    Depth: UINT;
    MipLevels: UINT;
    ArraySize: UINT;
    DxgiFormat: DXGI_FORMAT;
    Dimension: WICDdsDimension;
    AlphaMode: WICDdsAlphaMode;
  end;
  {$EXTERNALSYM WICDdsParameters}



  // Interface IWICDdsDecoder
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICDdsDecoder);'}
  {$EXTERNALSYM IWICDdsDecoder}
  IWICDdsDecoder = interface(IUnknown)
  ['{409cd537-8532-40cb-9774-e2feb2df4e9c}']
    function GetParameters(out pParameters: WICDdsParameters): HResult; stdcall;

    function GetFrame(arrayIndex: UINT;
                      mipLevel: UINT;
                      sliceIndex: UINT;
                      var ppIBitmapFrame: IWICBitmapFrameDecode): HResult; stdcall;
  end;
  IID_IWICDdsDecoder = IWICDdsDecoder;
  {$EXTERNALSYM IID_IWICDdsDecoder}



  // Interface IWICDdsEncoder
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICDdsEncoder);'}
  {$EXTERNALSYM IWICDdsEncoder}
  IWICDdsEncoder = interface(IUnknown)
  ['{5cacdb4c-407e-41b3-b936-d0f010cd6732}']
    function SetParameters(pParameters: WICDdsParameters): HResult; stdcall;

    function GetParameters(out pParameters: WICDdsParameters): HResult; stdcall;

    function CreateNewFrame(ppIFrameEncode: IWICBitmapFrameEncode;
                            out pArrayIndex: UINT;
                            out pMipLevel: UINT;
                            out pSliceIndex: UINT): HResult; stdcall;
  end;
  IID_IWICDdsEncoder = IWICDdsEncoder;
  {$EXTERNALSYM IID_IWICDdsEncoder}



  PWICDdsFormatInfo = ^TWICDdsFormatInfo;
  WICDdsFormatInfo = record
    DxgiFormat: DXGI_FORMAT;
    BytesPerBlock: UINT;
    BlockWidth: UINT;
    BlockHeight: UINT;
  end;
  {$EXTERNALSYM WICDdsFormatInfo}
  TWICDdsFormatInfo = WICDdsFormatInfo;


  // Interface IWICDdsFrameDecode
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICDdsFrameDecode);'}
  {$EXTERNALSYM IWICDdsFrameDecode}
  IWICDdsFrameDecode = interface(IUnknown)
  ['{3d4c0c61-18a4-41e4-bd80-481a4fc9f464}']
    function GetSizeInBlocks(out pWidthInBlocks: UINT;
                             out pHeightInBlocks: UINT): HResult; stdcall;

    function GetFormatInfo(var pFormatInfo: WICDdsFormatInfo): HResult; stdcall;

    function CopyBlocks(const prcBoundsInBlocks: WICRect;
                        cbStride: UINT;
                        cbBufferSize: UINT;
                        out pbBuffer: PByte): HResult; stdcall;
  end;
  IID_IWICDdsFrameDecode = IWICDdsFrameDecode;
  {$EXTERNALSYM IID_IWICDdsFrameDecode}


  // Interface IWICJpegFrameDecode
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICJpegFrameDecode);'}
  {$EXTERNALSYM IWICJpegFrameDecode}
  IWICJpegFrameDecode = interface(IUnknown)
  ['{8939F66E-C46A-4c21-A9D1-98B327CE1679}']
    function DoesSupportIndexing(out pfIndexingSupported: BOOL): HResult; stdcall;

    function SetIndexing(options: WICJpegIndexingOptions;
                         horizontalIntervalSize: UINT): HResult; stdcall;

    function ClearIndexing(): HResult; stdcall;

    function GetAcHuffmanTable(scanIndex: UINT;
                               tableIndex: UINT;
                               var pAcHuffmanTable: DXGI_JPEG_AC_HUFFMAN_TABLE): HResult; stdcall;

    function GetDcHuffmanTable(scanIndex: UINT;
                               tableIndex: UINT;
                               var pDcHuffmanTable: DXGI_JPEG_DC_HUFFMAN_TABLE): HResult; stdcall;

    function GetQuantizationTable(scanIndex: UINT;
                                  tableIndex: UINT;
                                  var pQuantizationTable: DXGI_JPEG_QUANTIZATION_TABLE): HResult; stdcall;

    function GetFrameHeader(out pFrameHeader: WICJpegFrameHeader): HResult; stdcall;

    function GetScanHeader(scanIndex: UINT;
                           var pScanHeader: PWICJpegScanHeader): HResult; stdcall;

    function CopyScan(scanIndex: UINT;
                      scanOffset: UINT;
                      cbScanData: UINT;
                      out pbScanData: PByte;
                      out pcbScanDataActual: UINT): HResult; stdcall;

    function CopyMinimalStream(streamOffset: UINT;
                               cbStreamData: UINT;
                               out pbStreamData: PByte;
                               out pcbStreamDataActual: UINT): HResult; stdcall;
  end;
  IID_IWICJpegFrameDecode = IWICJpegFrameDecode;
  {$EXTERNALSYM IID_IWICJpegFrameDecode}


  // Interface IWICJpegFrameEncode
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICJpegFrameEncode);'}
  {$EXTERNALSYM IWICJpegFrameEncode}
  IWICJpegFrameEncode = interface(IUnknown)
  ['{2F0C601F-D2C6-468C-ABFA-49495D983ED1}']
    function GetAcHuffmanTable(scanIndex: UINT;
                               tableIndex: UINT;
                               var pAcHuffmanTable: DXGI_JPEG_AC_HUFFMAN_TABLE): HResult; stdcall;

    function GetDcHuffmanTable(scanIndex: UINT;
                               tableIndex: UINT;
                               var pDcHuffmanTable: DXGI_JPEG_DC_HUFFMAN_TABLE): HResult; stdcall;

    function GetQuantizationTable(scanIndex: UINT;
                                  tableIndex: UINT;
                                  var pQuantizationTable: DXGI_JPEG_QUANTIZATION_TABLE): HResult; stdcall;

    function WriteScan(cbScanData: UINT;
                       pbScanData: PByte): HResult; stdcall;
  end;
  IID_IWICJpegFrameEncode = IWICJpegFrameEncode;
  {$EXTERNALSYM IID_IWICJpegFrameEncode}



  function MAKE_WINCODECHR(sev: DWORD;
                           code: DWORD): HResult;

  function MAKE_WINCODECHR_ERR(code: DWORD): HResult;



  //* Additional Prototypes for ALL interfaces *//

  //* end of Additional Prototypes *//


implementation



const
  WincodecLib = 'windowscodecs.dll';


function MAKE_WINCODECHR(sev: DWORD;
                         code: DWORD): HResult;
begin
  Result := MAKE_HRESULT(sev,
                         FACILITY_WINCODEC_ERR,
                         (WINCODEC_ERR_BASE + code));
end;


function MAKE_WINCODECHR_ERR(code: DWORD): HResult;
begin
  Result := MAKE_WINCODECHR(1,
                            code);
end;

{$WARN SYMBOL_PLATFORM OFF}

function WICConvertBitmapSource;       external WincodecLib name 'WICConvertBitmapSource' delayed;
function WICCreateBitmapFromSection;   external WincodecLib name 'WICCreateBitmapFromSection' delayed;
function WICCreateBitmapFromSectionEx; external WincodecLib name 'WICCreateBitmapFromSectionEx' delayed;
function WICMapGuidToShortName;        external WincodecLib name 'WICMapGuidToShortName' delayed;
function WICMapShortNameToGuid;        external WincodecLib name 'WICMapShortNameToGuid' delayed;
function WICMapSchemaToName;           external WincodecLib name 'WICMapSchemaToName' delayed;

{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.

end.
