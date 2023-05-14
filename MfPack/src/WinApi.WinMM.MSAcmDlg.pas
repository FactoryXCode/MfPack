// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinMM.MsAcm.pas
// Kind: Pascal / Delphi unit
// Release date: 25-06-2021
// Language: ENU
//
// Revision Version: 3.1.4
// Description: Audio Compression Manager Common Dialogs Identifiers.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: MSAcmDlg.h
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
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit WinApi.WinMM.MSAcmDlg;

interface

  (*$HPPEMIT '#include <MSAcmDlg.h>' *)

uses
  {WinApi}
  WinApi.Windows;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}
  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}


const

  DLG_ACMFORMATCHOOSE_ID              = 70;
  {$EXTERNALSYM DLG_ACMFORMATCHOOSE_ID}
  IDD_ACMFORMATCHOOSE_BTN_HELP        = 9;
  {$EXTERNALSYM IDD_ACMFORMATCHOOSE_BTN_HELP}
  IDD_ACMFORMATCHOOSE_CMB_CUSTOM      = 100;
  {$EXTERNALSYM IDD_ACMFORMATCHOOSE_CMB_CUSTOM}
  IDD_ACMFORMATCHOOSE_CMB_FORMATTAG   = 101;
  {$EXTERNALSYM IDD_ACMFORMATCHOOSE_CMB_FORMATTAG}
  IDD_ACMFORMATCHOOSE_CMB_FORMAT      = 102;
  {$EXTERNALSYM IDD_ACMFORMATCHOOSE_CMB_FORMAT}
  IDD_ACMFORMATCHOOSE_BTN_SETNAME     = 103;
  {$EXTERNALSYM IDD_ACMFORMATCHOOSE_BTN_SETNAME}
  IDD_ACMFORMATCHOOSE_BTN_DELNAME     = 104;
  {$EXTERNALSYM IDD_ACMFORMATCHOOSE_BTN_DELNAME}


  DLG_ACMFILTERCHOOSE_ID              = 71;
  {$EXTERNALSYM DLG_ACMFILTERCHOOSE_ID}
  IDD_ACMFILTERCHOOSE_BTN_HELP        = 9;
  {$EXTERNALSYM IDD_ACMFILTERCHOOSE_BTN_HELP}
  IDD_ACMFILTERCHOOSE_CMB_CUSTOM      = 100;
  {$EXTERNALSYM IDD_ACMFILTERCHOOSE_CMB_CUSTOM}
  IDD_ACMFILTERCHOOSE_CMB_FILTERTAG   = 101;
  {$EXTERNALSYM IDD_ACMFILTERCHOOSE_CMB_FILTERTAG}
  IDD_ACMFILTERCHOOSE_CMB_FILTER      = 102;
  {$EXTERNALSYM IDD_ACMFILTERCHOOSE_CMB_FILTER}
  IDD_ACMFILTERCHOOSE_BTN_SETNAME     = 103;
  {$EXTERNALSYM IDD_ACMFILTERCHOOSE_BTN_SETNAME}
  IDD_ACMFILTERCHOOSE_BTN_DELNAME     = 104;
  {$EXTERNALSYM IDD_ACMFILTERCHOOSE_BTN_DELNAME}


implementation

end.
