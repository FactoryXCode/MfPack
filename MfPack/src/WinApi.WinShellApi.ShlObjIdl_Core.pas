// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinShellApi.ShlObjIdl_Core.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.2.0
// Description: Shell API.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
//
//
// Related objects: -
// Related projects: MfPackX320
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: ShlObjId_Core.h
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
unit WinApi.WinShellApi.ShlObjIdl_Core;

interface

uses

{WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.WinShellApi.ShTypes,
  WinApi.Unknwn,
  WinApi.ShellAPI,
  WinApi.ComCat,
  WinApi.StructuredQueryCondition_2020,  // alias for WinApi.StructuredQueryCondition
  WinApi.WinShellApi.ObjectArray,
  WinApi.ServProv,
  {ActiveX}
  WinApi.ActiveX.OleIdl,
  WinApi.ActiveX.ObjIdl,
  WinApi.ActiveX.ObjBase,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.OaIdl,
  WinApi.MediaFoundationApi.MfError;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

// This unit was translated from ShObjIdl_core.h.
// External include-file contents are intentionally not expanded.

const

// Keep the following as normal constants. They are #define values in ShObjIdl_core.h, not C enum members:

//  and GUID/string constants.

  // QueryContextMenu uFlags
  CMF_NORMAL                          = $00000000;
  {$EXTERNALSYM CMF_NORMAL}
  CMF_DEFAULTONLY                     = $00000001;
  {$EXTERNALSYM CMF_DEFAULTONLY}
  CMF_VERBSONLY                       = $00000002;
  {$EXTERNALSYM CMF_VERBSONLY}
  CMF_EXPLORE                         = $00000004;
  {$EXTERNALSYM CMF_EXPLORE}
  CMF_NOVERBS                         = $00000008;
  {$EXTERNALSYM CMF_NOVERBS}
  CMF_CANRENAME                       = $00000010;
  {$EXTERNALSYM CMF_CANRENAME}
  CMF_NODEFAULT                       = $00000020;
  {$EXTERNALSYM CMF_NODEFAULT}

  CMF_INCLUDESTATIC                   = $00000040;
  {$EXTERNALSYM CMF_INCLUDESTATIC}
  CMF_ITEMMENU                        = $00000080;
  {$EXTERNALSYM CMF_ITEMMENU}
  CMF_EXTENDEDVERBS                   = $00000100;
  {$EXTERNALSYM CMF_EXTENDEDVERBS}
  CMF_DISABLEDVERBS                   = $00000200;
  {$EXTERNALSYM CMF_DISABLEDVERBS}
  CMF_ASYNCVERBSTATE                  = $00000400;
  {$EXTERNALSYM CMF_ASYNCVERBSTATE}
  CMF_OPTIMIZEFORINVOKE               = $00000800;
  {$EXTERNALSYM CMF_OPTIMIZEFORINVOKE}
  CMF_SYNCCASCADEMENU                 = $00001000;
  {$EXTERNALSYM CMF_SYNCCASCADEMENU}
  CMF_DONOTPICKDEFAULT                = $00002000;
  {$EXTERNALSYM CMF_DONOTPICKDEFAULT}
  CMF_RESERVED                        = $FFFF0000;
  {$EXTERNALSYM CMF_RESERVED}

  // GetCommandString uFlags
  GCS_VERBA                           = $00000000;  // canonical verb
  {$EXTERNALSYM GCS_VERBA}
  GCS_HELPTEXTA                       = $00000001;  // help text (for status bar)
  {$EXTERNALSYM GCS_HELPTEXTA}
  GCS_VALIDATEA                       = $00000002;  // validate command exists
  {$EXTERNALSYM GCS_VALIDATEA}
  GCS_VERBW                           = $00000004;  // canonical verb (unicode)
  {$EXTERNALSYM GCS_VERBW}
  GCS_HELPTEXTW                       = $00000005;  // help text (unicode version)
  {$EXTERNALSYM GCS_HELPTEXTW}
  GCS_VALIDATEW                       = $00000006;  // validate command exists (unicode)
  {$EXTERNALSYM GCS_VALIDATEW}
  GCS_VERBICONW                       = $00000014;  // icon string (unicode)
  {$EXTERNALSYM GCS_VERBICONW}
  GCS_UNICODE                         = $00000004;  // for bit testing - Unicode string
  {$EXTERNALSYM GCS_UNICODE}

{$IFDEF UNICODE}
  GCS_VERB                            = GCS_VERBW;
  {$EXTERNALSYM GCS_VERB}
  GCS_HELPTEXT                        = GCS_HELPTEXTW;
  {$EXTERNALSYM GCS_HELPTEXT}
  GCS_VALIDATE                        = GCS_VALIDATEW;
  {$EXTERNALSYM GCS_VALIDATE}
{$ELSE}
  GCS_VERB                            = GCS_VERBA;
  {$EXTERNALSYM GCS_VERB}
  GCS_HELPTEXT                        = GCS_HELPTEXTA;
  {$EXTERNALSYM GCS_HELPTEXT}
  GCS_VALIDATE                        = GCS_VALIDATEA;
  {$EXTERNALSYM GCS_VALIDATE}
{$ENDIF}

  {$EXTERNALSYM CMDSTR_NEWFOLDERA}
  CMDSTR_NEWFOLDERA                   = 'NewFolder';
  {$EXTERNALSYM CMDSTR_VIEWLISTA}
  CMDSTR_VIEWLISTA                    = 'ViewList';
  {$EXTERNALSYM CMDSTR_VIEWDETAILSA}
  CMDSTR_VIEWDETAILSA                 = 'ViewDetails';
  {$EXTERNALSYM CMDSTR_NEWFOLDERW}
  CMDSTR_NEWFOLDERW                   = 'NewFolder';
  {$EXTERNALSYM CMDSTR_VIEWLISTW}
  CMDSTR_VIEWLISTW                    = 'ViewList';
  {$EXTERNALSYM CMDSTR_VIEWDETAILSW}
  CMDSTR_VIEWDETAILSW                 = 'ViewDetails';

{$IFDEF UNICODE}
  CMDSTR_NEWFOLDER                    = CMDSTR_NEWFOLDERW;
  {$EXTERNALSYM CMDSTR_NEWFOLDER}
  CMDSTR_VIEWLIST                     = CMDSTR_VIEWLISTW;
  {$EXTERNALSYM CMDSTR_VIEWLIST}
  CMDSTR_VIEWDETAILS                  = CMDSTR_VIEWDETAILSW;
  {$EXTERNALSYM CMDSTR_VIEWDETAILS}
{$ELSE}
  CMDSTR_NEWFOLDER                    = CMDSTR_NEWFOLDERA;
  {$EXTERNALSYM CMDSTR_NEWFOLDER}
  CMDSTR_VIEWLIST                     = CMDSTR_VIEWLISTA;
  {$EXTERNALSYM CMDSTR_VIEWLIST}
  CMDSTR_VIEWDETAILS                  = CMDSTR_VIEWDETAILSA;
  {$EXTERNALSYM CMDSTR_VIEWDETAILS}
{$ENDIF}

  {$EXTERNALSYM CMIC_MASK_HOTKEY}
  CMIC_MASK_HOTKEY                    = SEE_MASK_HOTKEY;
  {$EXTERNALSYM CMIC_MASK_ICON}
  CMIC_MASK_ICON                      = SEE_MASK_ICON;
  {$EXTERNALSYM CMIC_MASK_FLAG_NO_UI}
  CMIC_MASK_FLAG_NO_UI                = SEE_MASK_FLAG_NO_UI;
  {$EXTERNALSYM CMIC_MASK_UNICODE}
  CMIC_MASK_UNICODE                   = SEE_MASK_UNICODE;
  {$EXTERNALSYM CMIC_MASK_NO_CONSOLE}
  CMIC_MASK_NO_CONSOLE                = SEE_MASK_NO_CONSOLE;
//#if (NTDDI_VERSION < NTDDI_VISTA)
  {$EXTERNALSYM CMIC_MASK_HASLINKNAME}
  CMIC_MASK_HASLINKNAME               = $00010000; // SEE_MASK_HASLINKNAME
  {$EXTERNALSYM CMIC_MASK_HASTITLE}
  CMIC_MASK_HASTITLE                  = $00080000; // SEE_MASK_HASTITLE
//#endif  // NTDDI_VISTA
  {$EXTERNALSYM CMIC_MASK_FLAG_SEP_VDM}
  CMIC_MASK_FLAG_SEP_VDM              = $00020000; // SEE_MASK_FLAG_SEPVDM
  {$EXTERNALSYM CMIC_MASK_ASYNCOK}
  CMIC_MASK_ASYNCOK                   = SEE_MASK_ASYNCOK;
//#if (NTDDI_VERSION >= NTDDI_VISTA)
  {$EXTERNALSYM CMIC_MASK_NOASYNC}
  CMIC_MASK_NOASYNC                   = SEE_MASK_NOASYNC;
//#endif
  {$EXTERNALSYM CMIC_MASK_SHIFT_DOWN}
  CMIC_MASK_SHIFT_DOWN                = $10000000;
  {$EXTERNALSYM CMIC_MASK_CONTROL_DOWN}
  CMIC_MASK_CONTROL_DOWN              = $40000000;
  {$EXTERNALSYM CMIC_MASK_FLAG_LOG_USAGE}
  CMIC_MASK_FLAG_LOG_USAGE            = SEE_MASK_FLAG_LOG_USAGE;
  {$EXTERNALSYM CMIC_MASK_NOZONECHECKS}
  CMIC_MASK_NOZONECHECKS              = SEE_MASK_NOZONECHECKS;
  {$EXTERNALSYM CMIC_MASK_PTINVOKE}
  CMIC_MASK_PTINVOKE                  = $20000000;

  // IPersistFolder
  IRTIR_TASK_NOT_RUNNING              = 0;
  {$EXTERNALSYM IRTIR_TASK_NOT_RUNNING}
  IRTIR_TASK_RUNNING                  = 1;
  {$EXTERNALSYM IRTIR_TASK_RUNNING}
  IRTIR_TASK_SUSPENDED                = 2;
  {$EXTERNALSYM IRTIR_TASK_SUSPENDED}
  IRTIR_TASK_PENDING                  = 3;
  {$EXTERNALSYM IRTIR_TASK_PENDING}
  IRTIR_TASK_FINISHED                 = 4;
  {$EXTERNALSYM IRTIR_TASK_FINISHED}

  // IShellTaskScheduler
  TOID_NULL                           : TGUID = '{00000000-0000-0000-0000-000000000000}'; // GUID_NULL;
  {$EXTERNALSYM TOID_NULL}
  ITSAT_DEFAULT_LPARAM                = DWORD_PTR(- 1);
  {$EXTERNALSYM ITSAT_DEFAULT_LPARAM}
  ITSAT_DEFAULT_PRIORITY              = $10000000;
  {$EXTERNALSYM ITSAT_DEFAULT_PRIORITY}
  ITSAT_MAX_PRIORITY                  = $7FFFFFFF;
  {$EXTERNALSYM ITSAT_MAX_PRIORITY}
  ITSAT_MIN_PRIORITY                  = $00000000;
  {$EXTERNALSYM ITSAT_MIN_PRIORITY}
  ITSSFLAG_COMPLETE_ON_DESTROY        = $0000;  // wait for the current task to complete before deleting the scheduler
  {$EXTERNALSYM ITSSFLAG_COMPLETE_ON_DESTROY}
  ITSSFLAG_KILL_ON_DESTROY            = $0001;  // kill the current task (if there is one) when the task scheduler is deleted
  {$EXTERNALSYM ITSSFLAG_KILL_ON_DESTROY}
  ITSSFLAG_FLAGS_MASK                 = $0003;
  {$EXTERNALSYM ITSSFLAG_FLAGS_MASK}
  ITSS_THREAD_DESTROY_DEFAULT_TIMEOUT = (10 * 1000);  // default milliseconds until a sleeping worker thread is released
  {$EXTERNALSYM ITSS_THREAD_DESTROY_DEFAULT_TIMEOUT}
  ITSS_THREAD_TERMINATE_TIMEOUT       = (INFINITE);  // set sleeping worker threads to never be released
  {$EXTERNALSYM ITSS_THREAD_TERMINATE_TIMEOUT}
  ITSS_THREAD_TIMEOUT_NO_CHANGE       = (INFINITE - 1);  // no change to the thread timeout
  {$EXTERNALSYM ITSS_THREAD_TIMEOUT_NO_CHANGE}

  // IEnumFullIDList
  SHCIDS_ALLFIELDS                    = $80000000;
  {$EXTERNALSYM SHCIDS_ALLFIELDS}
  SHCIDS_CANONICALONLY                = $10000000;
  {$EXTERNALSYM SHCIDS_CANONICALONLY}
  SHCIDS_BITMASK                      = $FFFF0000;
  {$EXTERNALSYM SHCIDS_BITMASK}
  SHCIDS_COLUMNMASK                   = $0000FFFF;
  {$EXTERNALSYM SHCIDS_COLUMNMASK}
  SFGAO_CANCOPY                       = DROPEFFECT_COPY;  // Objects can be copied    (0x1)
  {$EXTERNALSYM SFGAO_CANCOPY}
  SFGAO_CANMOVE                       = DROPEFFECT_MOVE;  // Objects can be moved     (0x2)
  {$EXTERNALSYM SFGAO_CANMOVE}
  SFGAO_CANLINK                       = DROPEFFECT_LINK;  // Objects can be linked    (0x4)
  {$EXTERNALSYM SFGAO_CANLINK}
  SFGAO_STORAGE                       = $00000008;  // supports BindToObject(IID_IStorage)
  {$EXTERNALSYM SFGAO_STORAGE}
  SFGAO_CANRENAME                     = $00000010;  // Objects can be renamed
  {$EXTERNALSYM SFGAO_CANRENAME}
  SFGAO_CANDELETE                     = $00000020;  // Objects can be deleted
  {$EXTERNALSYM SFGAO_CANDELETE}
  SFGAO_HASPROPSHEET                  = $00000040;  // Objects have property sheets
  {$EXTERNALSYM SFGAO_HASPROPSHEET}
  SFGAO_DROPTARGET                    = $00000100;  // Objects are drop target
  {$EXTERNALSYM SFGAO_DROPTARGET}
  SFGAO_CAPABILITYMASK                = $00000177;
  {$EXTERNALSYM SFGAO_CAPABILITYMASK}
  SFGAO_PLACEHOLDER                   = $00000800;  // File or folder is not fully present and recalled on open or access
  {$EXTERNALSYM SFGAO_PLACEHOLDER}
  SFGAO_SYSTEM                        = $00001000;  // System object
  {$EXTERNALSYM SFGAO_SYSTEM}
  SFGAO_ENCRYPTED                     = $00002000;  // Object is encrypted (use alt color)
  {$EXTERNALSYM SFGAO_ENCRYPTED}
  SFGAO_ISSLOW                        = $00004000;  // 'Slow' object
  {$EXTERNALSYM SFGAO_ISSLOW}
  SFGAO_GHOSTED                       = $00008000;  // Ghosted icon
  {$EXTERNALSYM SFGAO_GHOSTED}
  SFGAO_LINK                          = $00010000;  // Shortcut (link)
  {$EXTERNALSYM SFGAO_LINK}
  SFGAO_SHARE                         = $00020000;  // Shared
  {$EXTERNALSYM SFGAO_SHARE}
  SFGAO_READONLY                      = $00040000;  // Read-only
  {$EXTERNALSYM SFGAO_READONLY}
  SFGAO_HIDDEN                        = $00080000;  // Hidden object
  {$EXTERNALSYM SFGAO_HIDDEN}
  SFGAO_DISPLAYATTRMASK               = $000FC000;
  {$EXTERNALSYM SFGAO_DISPLAYATTRMASK}
  SFGAO_FILESYSANCESTOR               = $10000000;  // May contain children with SFGAO_FILESYSTEM
  {$EXTERNALSYM SFGAO_FILESYSANCESTOR}
  SFGAO_FOLDER                        = $20000000;  // Support BindToObject(IID_IShellFolder)
  {$EXTERNALSYM SFGAO_FOLDER}
  SFGAO_FILESYSTEM                    = $40000000;  // Is a win32 file system object (file/folder/root)
  {$EXTERNALSYM SFGAO_FILESYSTEM}
  SFGAO_HASSUBFOLDER                  = $80000000;  // May contain children with SFGAO_FOLDER (may be slow)
  {$EXTERNALSYM SFGAO_HASSUBFOLDER}
  SFGAO_CONTENTSMASK                  = $80000000;
  {$EXTERNALSYM SFGAO_CONTENTSMASK}
  SFGAO_VALIDATE                      = $01000000;  // Invalidate cached information (may be slow)
  {$EXTERNALSYM SFGAO_VALIDATE}
  SFGAO_REMOVABLE                     = $02000000;  // Is this removeable media?
  {$EXTERNALSYM SFGAO_REMOVABLE}
  SFGAO_COMPRESSED                    = $04000000;  // Object is compressed (use alt color)
  {$EXTERNALSYM SFGAO_COMPRESSED}
  SFGAO_BROWSABLE                     = $08000000;  // Supports IShellFolder, but only implements CreateViewObject() (non-folder view)
  {$EXTERNALSYM SFGAO_BROWSABLE}
  SFGAO_NONENUMERATED                 = $00100000;  // Is a non-enumerated object (should be hidden)
  {$EXTERNALSYM SFGAO_NONENUMERATED}
  SFGAO_NEWCONTENT                    = $00200000;  // Should show bold in explorer tree
  {$EXTERNALSYM SFGAO_NEWCONTENT}
  SFGAO_CANMONIKER                    = $00400000;  // Obsolete
  {$EXTERNALSYM SFGAO_CANMONIKER}
  SFGAO_HASSTORAGE                    = $00400000;  // Obsolete
  {$EXTERNALSYM SFGAO_HASSTORAGE}
  SFGAO_STREAM                        = $00400000;  // Supports BindToObject(IID_IStream)
  {$EXTERNALSYM SFGAO_STREAM}
  SFGAO_STORAGEANCESTOR               = $00800000;  // May contain children with SFGAO_STORAGE or SFGAO_STREAM
  {$EXTERNALSYM SFGAO_STORAGEANCESTOR}
  SFGAO_STORAGECAPMASK                = $70C50008;  // For determining storage capabilities, ie for open/save semantics
  {$EXTERNALSYM SFGAO_STORAGECAPMASK}
  SFGAO_PKEYSFGAOMASK                 = $81044000;  // Attributes that are masked out for PKEY_SFGAOFlags because they are considered to cause slow calculations or lack context (SFGAO_VALIDATE | SFGAO_ISSLOW | SFGAO_HASSUBFOLDER and others)
  {$EXTERNALSYM SFGAO_PKEYSFGAOMASK}

  // IPreviewHandlerVisuals
  CDBOSC_SETFOCUS                     = $00000000;
  {$EXTERNALSYM CDBOSC_SETFOCUS}
  CDBOSC_KILLFOCUS                    = $00000001;
  {$EXTERNALSYM CDBOSC_KILLFOCUS}
  CDBOSC_SELCHANGE                    = $00000002;
  {$EXTERNALSYM CDBOSC_SELCHANGE}
  CDBOSC_RENAME                       = $00000003;
  {$EXTERNALSYM CDBOSC_RENAME}
  CDBOSC_STATECHANGE                  = $00000004;
  {$EXTERNALSYM CDBOSC_STATECHANGE}

  // ICommDlgBrowser
//#if (NTDDI_VERSION >= NTDDI_WIN2K)
  {$EXTERNALSYM CDB2N_CONTEXTMENU_DONE}
  CDB2N_CONTEXTMENU_DONE              = $00000001;
  {$EXTERNALSYM CDB2N_CONTEXTMENU_START}
  CDB2N_CONTEXTMENU_START             = $00000002;
  {$EXTERNALSYM CDB2GVF_SHOWALLFILES}
  CDB2GVF_SHOWALLFILES                = $00000001;
//#if (NTDDI_VERSION >= NTDDI_VISTA)
  {$EXTERNALSYM CDB2GVF_ISFILESAVE}
  CDB2GVF_ISFILESAVE                  = $00000002;  // is file save, else file open
  {$EXTERNALSYM CDB2GVF_ALLOWPREVIEWPANE}
  CDB2GVF_ALLOWPREVIEWPANE            = $00000004;
  {$EXTERNALSYM CDB2GVF_NOSELECTVERB}
  CDB2GVF_NOSELECTVERB                = $00000008;
  {$EXTERNALSYM CDB2GVF_NOINCLUDEITEM}
  CDB2GVF_NOINCLUDEITEM               = $00000010;
  {$EXTERNALSYM CDB2GVF_ISFOLDERPICKER}
  CDB2GVF_ISFOLDERPICKER              = $00000020;
  {$EXTERNALSYM CDB2GVF_ADDSHIELD}
  CDB2GVF_ADDSHIELD                   = $00000040;  // when CDB2GVF_NOSELECTVERB is not specified this flag controls the display of a LUA shield on the Select menu item
//#endif  // NTDDI_VISTA

  // IShellIconVtbl
  SBSP_DEFBROWSER                     = $0000;
  {$EXTERNALSYM SBSP_DEFBROWSER}
  SBSP_SAMEBROWSER                    = $0001;
  {$EXTERNALSYM SBSP_SAMEBROWSER}
  SBSP_NEWBROWSER                     = $0002;
  {$EXTERNALSYM SBSP_NEWBROWSER}
  SBSP_DEFMODE                        = $0000;
  {$EXTERNALSYM SBSP_DEFMODE}
  SBSP_OPENMODE                       = $0010;
  {$EXTERNALSYM SBSP_OPENMODE}
  SBSP_EXPLOREMODE                    = $0020;
  {$EXTERNALSYM SBSP_EXPLOREMODE}
  SBSP_HELPMODE                       = $0040;
  {$EXTERNALSYM SBSP_HELPMODE}
  SBSP_NOTRANSFERHIST                 = $0080;
  {$EXTERNALSYM SBSP_NOTRANSFERHIST}
  SBSP_ABSOLUTE                       = $0000;
  {$EXTERNALSYM SBSP_ABSOLUTE}
  SBSP_RELATIVE                       = $1000;
  {$EXTERNALSYM SBSP_RELATIVE}
  SBSP_PARENT                         = $2000;
  {$EXTERNALSYM SBSP_PARENT}
  SBSP_NAVIGATEBACK                   = $4000;
  {$EXTERNALSYM SBSP_NAVIGATEBACK}
  SBSP_NAVIGATEFORWARD                = $8000;
  {$EXTERNALSYM SBSP_NAVIGATEFORWARD}
  SBSP_ALLOW_AUTONAVIGATE             = $00010000;
  {$EXTERNALSYM SBSP_ALLOW_AUTONAVIGATE}
//#if (NTDDI_VERSION >= NTDDI_VISTA)
  SBSP_KEEPSAMETEMPLATE               = $00020000;
  {$EXTERNALSYM SBSP_KEEPSAMETEMPLATE}
  SBSP_KEEPWORDWHEELTEXT              = $00040000;
  {$EXTERNALSYM SBSP_KEEPWORDWHEELTEXT}
  SBSP_ACTIVATE_NOFOCUS               = $00080000;
  {$EXTERNALSYM SBSP_ACTIVATE_NOFOCUS}
  SBSP_CREATENOHISTORY                = $00100000;
  {$EXTERNALSYM SBSP_CREATENOHISTORY}
  SBSP_PLAYNOSOUND                    = $00200000;
  {$EXTERNALSYM SBSP_PLAYNOSOUND}
//#endif  // (NTDDI_VERSION >= NTDDI_VISTA)
//#if (_WIN32_IE >= _WIN32_IE_IE60SP2)
  SBSP_CALLERUNTRUSTED                = $00800000;
  {$EXTERNALSYM SBSP_CALLERUNTRUSTED}
  SBSP_TRUSTFIRSTDOWNLOAD             = $01000000;
  {$EXTERNALSYM SBSP_TRUSTFIRSTDOWNLOAD}
  SBSP_UNTRUSTEDFORDOWNLOAD           = $02000000;
  {$EXTERNALSYM SBSP_UNTRUSTEDFORDOWNLOAD}
//#endif  // _WIN32_IE_IE60SP2
  SBSP_NOAUTOSELECT                   = $04000000;
  {$EXTERNALSYM SBSP_NOAUTOSELECT}
  SBSP_WRITENOHISTORY                 = $08000000;
  {$EXTERNALSYM SBSP_WRITENOHISTORY}
//#if (_WIN32_IE >= _WIN32_IE_IE60SP2)
  SBSP_TRUSTEDFORACTIVEX              = $10000000;
  {$EXTERNALSYM SBSP_TRUSTEDFORACTIVEX}
//#endif  // _WIN32_IE_IE60SP2
//#if (_WIN32_IE >= _WIN32_IE_IE70)
  SBSP_FEEDNAVIGATION                 = $20000000;
  {$EXTERNALSYM SBSP_FEEDNAVIGATION}
//#endif  // _WIN32_IE_IE70
  SBSP_REDIRECT                       = $40000000;
  {$EXTERNALSYM SBSP_REDIRECT}
  SBSP_INITIATEDBYHLINKFRAME          = $80000000;
  {$EXTERNALSYM SBSP_INITIATEDBYHLINKFRAME}
  FCW_STATUS                          = $0001;
  {$EXTERNALSYM FCW_STATUS}
  FCW_TOOLBAR                         = $0002;
  {$EXTERNALSYM FCW_TOOLBAR}
  FCW_TREE                            = $0003;
  {$EXTERNALSYM FCW_TREE}
  FCW_INTERNETBAR                     = $0006;
  {$EXTERNALSYM FCW_INTERNETBAR}
  FCW_PROGRESS                        = $0008;
  {$EXTERNALSYM FCW_PROGRESS}
//#if (_WIN32_IE >= 0x0700)
//#endif
  FCT_MERGE                           = $0001;
  {$EXTERNALSYM FCT_MERGE}
  FCT_CONFIGABLE                      = $0002;
  {$EXTERNALSYM FCT_CONFIGABLE}
  FCT_ADDTOEND                        = $0004;
  {$EXTERNALSYM FCT_ADDTOEND}
//#ifdef _NEVER_

  // IActionProgress
  ARCONTENT_AUTORUNINF                = $00000002;  // That's the one we have today, and always had
  {$EXTERNALSYM ARCONTENT_AUTORUNINF}
  ARCONTENT_AUDIOCD                   = $00000004;  // Audio CD (not MP3 and the like, the stuff you buy at the store)
  {$EXTERNALSYM ARCONTENT_AUDIOCD}
  ARCONTENT_DVDMOVIE                  = $00000008;  // DVD Movie (not MPEGs, the stuff you buy at the store)
  {$EXTERNALSYM ARCONTENT_DVDMOVIE}
  ARCONTENT_BLANKCD                   = $00000010;  // Blank CD-R/CD-RW)
  {$EXTERNALSYM ARCONTENT_BLANKCD}
  ARCONTENT_BLANKDVD                  = $00000020;  // Blank DVD-R/DVD-RW
  {$EXTERNALSYM ARCONTENT_BLANKDVD}
  ARCONTENT_UNKNOWNCONTENT            = $00000040;  // Whatever files.  Mean that it's formatted.
  {$EXTERNALSYM ARCONTENT_UNKNOWNCONTENT}
  ARCONTENT_AUTOPLAYPIX               = $00000080;  // Any files classified by shell as image. (jpg, bmp, etc.)
  {$EXTERNALSYM ARCONTENT_AUTOPLAYPIX}
  ARCONTENT_AUTOPLAYMUSIC             = $00000100;  // Any files classified by shell as music. (wma, mp3, etc.)
  {$EXTERNALSYM ARCONTENT_AUTOPLAYMUSIC}
  ARCONTENT_AUTOPLAYVIDEO             = $00000200;  // Any files classified by shell as video. (mpg, avi, etc.)
  {$EXTERNALSYM ARCONTENT_AUTOPLAYVIDEO}
//#if (NTDDI_VERSION >= NTDDI_VISTA)
  ARCONTENT_VCD                       = $00000400;  // VCD format
  {$EXTERNALSYM ARCONTENT_VCD}
  ARCONTENT_SVCD                      = $00000800;  // Super-VCD format
  {$EXTERNALSYM ARCONTENT_SVCD}
  ARCONTENT_DVDAUDIO                  = $00001000;  // DVD-Audio
  {$EXTERNALSYM ARCONTENT_DVDAUDIO}
  ARCONTENT_BLANKBD                   = $00002000;  // Blank BD-R/BD-RW
  {$EXTERNALSYM ARCONTENT_BLANKBD}
  ARCONTENT_BLURAY                    = $00004000;  // Blu-ray Disc
  {$EXTERNALSYM ARCONTENT_BLURAY}
  ARCONTENT_CAMERASTORAGE             = $00008000;  // Camera Storage
  {$EXTERNALSYM ARCONTENT_CAMERASTORAGE}
  ARCONTENT_CUSTOMEVENT               = $00010000;  // Custom Event
  {$EXTERNALSYM ARCONTENT_CUSTOMEVENT}
  ARCONTENT_NONE                      = $00000000;  // Empty (but formatted)
  {$EXTERNALSYM ARCONTENT_NONE}
  ARCONTENT_MASK                      = $0001FFFE;  // Bits that denote valid content types
  {$EXTERNALSYM ARCONTENT_MASK}
  ARCONTENT_PHASE_UNKNOWN             = $00000000;  // We can be in any phase.  This is XP behavior.
  {$EXTERNALSYM ARCONTENT_PHASE_UNKNOWN}
  ARCONTENT_PHASE_PRESNIFF            = $10000000;  // These are contents we know w/o scanning the media for complete data (e.g. Audio track, DVD Movie).
  {$EXTERNALSYM ARCONTENT_PHASE_PRESNIFF}
  ARCONTENT_PHASE_SNIFFING            = $20000000;  // We are in the middle of searching the media.  There could be more contents to be found than currently reported.
  {$EXTERNALSYM ARCONTENT_PHASE_SNIFFING}
  ARCONTENT_PHASE_FINAL               = $40000000;  // We have finished searching; contents we report are final.
  {$EXTERNALSYM ARCONTENT_PHASE_FINAL}
  ARCONTENT_PHASE_MASK                = $70000000;  // Bits that denote what phase we are in the Autoplay process.
  {$EXTERNALSYM ARCONTENT_PHASE_MASK}
//#endif  // NTDDI_VISTA

  // IExtractImage
  IEI_PRIORITY_MAX                    = ITSAT_MAX_PRIORITY;
  {$EXTERNALSYM IEI_PRIORITY_MAX}
  IEI_PRIORITY_MIN                    = ITSAT_MIN_PRIORITY;
  {$EXTERNALSYM IEI_PRIORITY_MIN}
  IEIT_PRIORITY_NORMAL                = ITSAT_DEFAULT_PRIORITY;
  {$EXTERNALSYM IEIT_PRIORITY_NORMAL}
  IEIFLAG_ASYNC                       = $0001;  // (deprecated) ask the extractor if it supports ASYNC extract (free threaded)
  {$EXTERNALSYM IEIFLAG_ASYNC}
  IEIFLAG_CACHE                       = $0002;  // returned from the extractor if it does NOT cache the thumbnail
  {$EXTERNALSYM IEIFLAG_CACHE}
  IEIFLAG_ASPECT                      = $0004;  // passed to the extractor to beg it to render to the aspect ratio of the supplied rect
  {$EXTERNALSYM IEIFLAG_ASPECT}
  IEIFLAG_OFFLINE                     = $0008;  // if the extractor shouldn't hit the net to get any content neede for the rendering
  {$EXTERNALSYM IEIFLAG_OFFLINE}
  IEIFLAG_GLEAM                       = $0010;  // does the image have a gleam ? this will be returned if it does
  {$EXTERNALSYM IEIFLAG_GLEAM}
  IEIFLAG_SCREEN                      = $0020;  // render as if for the screen  (this is exlusive with IEIFLAG_ASPECT )
  {$EXTERNALSYM IEIFLAG_SCREEN}
  IEIFLAG_ORIGSIZE                    = $0040;  // render to the approx size passed, but crop if neccessary
  {$EXTERNALSYM IEIFLAG_ORIGSIZE}
  IEIFLAG_NOSTAMP                     = $0080;  // returned from the extractor if it does NOT want an icon stamp on the thumbnail
  {$EXTERNALSYM IEIFLAG_NOSTAMP}
  IEIFLAG_NOBORDER                    = $0100;  // returned from the extractor if it does NOT want an a border around the thumbnail
  {$EXTERNALSYM IEIFLAG_NOBORDER}
  IEIFLAG_QUALITY                     = $0200;  // passed to the Extract method to indicate that a slower, higher quality image is desired, re-compute the thumbnail
  {$EXTERNALSYM IEIFLAG_QUALITY}
  IEIFLAG_REFRESH                     = $0400;  // returned from the extractor if it would like to have Refresh Thumbnail available
  {$EXTERNALSYM IEIFLAG_REFRESH}

  // IDeskBand
  {$EXTERNALSYM DBIM_MINSIZE}
  DBIM_MINSIZE                        = $0001;
  {$EXTERNALSYM DBIM_MAXSIZE}
  DBIM_MAXSIZE                        = $0002;
  {$EXTERNALSYM DBIM_INTEGRAL}
  DBIM_INTEGRAL                       = $0004;
  {$EXTERNALSYM DBIM_ACTUAL}
  DBIM_ACTUAL                         = $0008;
  {$EXTERNALSYM DBIM_TITLE}
  DBIM_TITLE                          = $0010;
  {$EXTERNALSYM DBIM_MODEFLAGS}
  DBIM_MODEFLAGS                      = $0020;
  {$EXTERNALSYM DBIM_BKCOLOR}
  DBIM_BKCOLOR                        = $0040;

  {$EXTERNALSYM DBIMF_NORMAL}
  DBIMF_NORMAL                        = $0000;
  {$EXTERNALSYM DBIMF_FIXED}
  DBIMF_FIXED                         = $0001;
  {$EXTERNALSYM DBIMF_FIXEDBMP}
  DBIMF_FIXEDBMP                      = $0004;  // a fixed background bitmap (if supported)
  {$EXTERNALSYM DBIMF_VARIABLEHEIGHT}
  DBIMF_VARIABLEHEIGHT                = $0008;
  {$EXTERNALSYM DBIMF_UNDELETEABLE}
  DBIMF_UNDELETEABLE                  = $0010;
  {$EXTERNALSYM DBIMF_DEBOSSED}
  DBIMF_DEBOSSED                      = $0020;
  {$EXTERNALSYM DBIMF_BKCOLOR}
  DBIMF_BKCOLOR                       = $0040;
  {$EXTERNALSYM DBIMF_USECHEVRON}
  DBIMF_USECHEVRON                    = $0080;
  {$EXTERNALSYM DBIMF_BREAK}
  DBIMF_BREAK                         = $0100;
  {$EXTERNALSYM DBIMF_ADDTOFRONT}
  DBIMF_ADDTOFRONT                    = $0200;
  {$EXTERNALSYM DBIMF_TOPALIGN}
  DBIMF_TOPALIGN                      = $0400;
//#if (NTDDI_VERSION >= NTDDI_VISTA)
  {$EXTERNALSYM DBIMF_NOGRIPPER}
  DBIMF_NOGRIPPER                     = $0800;
  {$EXTERNALSYM DBIMF_ALWAYSGRIPPER}
  DBIMF_ALWAYSGRIPPER                 = $1000;
  {$EXTERNALSYM DBIMF_NOMARGINS}
  DBIMF_NOMARGINS                     = $2000;
//#endif  // NTDDI_VISTA
  {$EXTERNALSYM DBIF_VIEWMODE_NORMAL}
  DBIF_VIEWMODE_NORMAL                = $0000;
  {$EXTERNALSYM DBIF_VIEWMODE_VERTICAL}
  DBIF_VIEWMODE_VERTICAL              = $0001;
  {$EXTERNALSYM DBIF_VIEWMODE_FLOATING}
  DBIF_VIEWMODE_FLOATING              = $0002;
  {$EXTERNALSYM DBIF_VIEWMODE_TRANSPARENT}
  DBIF_VIEWMODE_TRANSPARENT           = $0004;

  // IFileOperation
  FOFX_NOSKIPJUNCTIONS                = $00010000;  // Don't avoid binding to junctions (like Task folder, Recycle-Bin)
  {$EXTERNALSYM FOFX_NOSKIPJUNCTIONS}
  FOFX_PREFERHARDLINK                 = $00020000;  // Create hard link if possible
  {$EXTERNALSYM FOFX_PREFERHARDLINK}
  FOFX_SHOWELEVATIONPROMPT            = $00040000;  // Show elevation prompts when error UI is disabled (use with FOF_NOERRORUI)
  {$EXTERNALSYM FOFX_SHOWELEVATIONPROMPT}
  FOFX_RECYCLEONDELETE                = $00080000;  // Recycle when deleting, rather than permanently deleting
  {$EXTERNALSYM FOFX_RECYCLEONDELETE}
  FOFX_EARLYFAILURE                   = $00100000;  // Fail operation as soon as a single error occurs rather than trying to process other items (applies only when using FOF_NOERRORUI)
  {$EXTERNALSYM FOFX_EARLYFAILURE}
  FOFX_PRESERVEFILEEXTENSIONS         = $00200000;  // Rename collisions preserve file extns (use with FOF_RENAMEONCOLLISION)
  {$EXTERNALSYM FOFX_PRESERVEFILEEXTENSIONS}
  FOFX_KEEPNEWERFILE                  = $00400000;  // Keep newer file on naming conflicts
  {$EXTERNALSYM FOFX_KEEPNEWERFILE}
  FOFX_NOCOPYHOOKS                    = $00800000;  // Don't use copy hooks
  {$EXTERNALSYM FOFX_NOCOPYHOOKS}
  FOFX_NOMINIMIZEBOX                  = $01000000;  // Don't allow minimizing the progress dialog
  {$EXTERNALSYM FOFX_NOMINIMIZEBOX}
  FOFX_MOVEACLSACROSSVOLUMES          = $02000000;  // Copy security information when performing a cross-volume move operation
  {$EXTERNALSYM FOFX_MOVEACLSACROSSVOLUMES}
  FOFX_DONTDISPLAYSOURCEPATH          = $04000000;  // Don't display the path of source file in progress dialog
  {$EXTERNALSYM FOFX_DONTDISPLAYSOURCEPATH}
  FOFX_DONTDISPLAYDESTPATH            = $08000000;  // Don't display the path of destination file in progress dialog
  {$EXTERNALSYM FOFX_DONTDISPLAYDESTPATH}
  FOFX_REQUIREELEVATION               = $10000000;  // User expects the elevation; don't show a dialog to confirm
  {$EXTERNALSYM FOFX_REQUIREELEVATION}
  FOFX_ADDUNDORECORD                  = $20000000;  // This is a user-invoked operation, and should be placed on the undo stack.  This flag is preferred to FOF_ALLOWUNDO
  {$EXTERNALSYM FOFX_ADDUNDORECORD}
  FOFX_COPYASDOWNLOAD                 = $40000000;  // Show Downloading instead of Copying
  {$EXTERNALSYM FOFX_COPYASDOWNLOAD}
  FOFX_DONTDISPLAYLOCATIONS           = $80000000;  // Hides the locations line in the progress dialog
  {$EXTERNALSYM FOFX_DONTDISPLAYLOCATIONS}

  // IBandSite
  BSIM_STATE                          = $00000001;
  {$EXTERNALSYM BSIM_STATE}
  BSIM_STYLE                          = $00000002;
  {$EXTERNALSYM BSIM_STYLE}
  BSSF_VISIBLE                        = $00000001;
  {$EXTERNALSYM BSSF_VISIBLE}
  BSSF_NOTITLE                        = $00000002;
  {$EXTERNALSYM BSSF_NOTITLE}
  BSSF_UNDELETEABLE                   = $00001000;
  {$EXTERNALSYM BSSF_UNDELETEABLE}
  BSIS_AUTOGRIPPER                    = $00000000;
  {$EXTERNALSYM BSIS_AUTOGRIPPER}
  BSIS_NOGRIPPER                      = $00000001;
  {$EXTERNALSYM BSIS_NOGRIPPER}
  BSIS_ALWAYSGRIPPER                  = $00000002;
  {$EXTERNALSYM BSIS_ALWAYSGRIPPER}
  BSIS_LEFTALIGN                      = $00000004;
  {$EXTERNALSYM BSIS_LEFTALIGN}
  BSIS_SINGLECLICK                    = $00000008;
  {$EXTERNALSYM BSIS_SINGLECLICK}
  BSIS_NOCONTEXTMENU                  = $00000010;
  {$EXTERNALSYM BSIS_NOCONTEXTMENU}
  BSIS_NODROPTARGET                   = $00000020;
  {$EXTERNALSYM BSIS_NODROPTARGET}
  BSIS_NOCAPTION                      = $00000040;
  {$EXTERNALSYM BSIS_NOCAPTION}
  BSIS_PREFERNOLINEBREAK              = $00000080;
  {$EXTERNALSYM BSIS_PREFERNOLINEBREAK}
  BSIS_LOCKED                         = $00000100;
  {$EXTERNALSYM BSIS_LOCKED}
//#if (_WIN32_IE >= _WIN32_IE_IE70)
  BSIS_PRESERVEORDERDURINGLAYOUT      = $00000200;
  {$EXTERNALSYM BSIS_PRESERVEORDERDURINGLAYOUT}
  BSIS_FIXEDORDER                     = $00000400;
  {$EXTERNALSYM BSIS_FIXEDORDER}
//#endif  // _WIN32_IE_IE70
  SID_SBandSite                       : TGUID = '{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}'; // IID_IBandSite
  {$EXTERNALSYM SID_SBandSite}
  CGID_BandSite                       : TGUID = '{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}'; // IID_IBandSite
  {$EXTERNALSYM CGID_BandSite}

  // IFileIsInUse
  OF_CAP_CANSWITCHTO                  = $0001;
  {$EXTERNALSYM OF_CAP_CANSWITCHTO}
  OF_CAP_CANCLOSE                     = $0002;
  {$EXTERNALSYM OF_CAP_CANCLOSE}

  // IShellMenuCallback
  // Mask
  SMDM_SHELLFOLDER                    = $00000001;  // This is for an item in the band
  {$EXTERNALSYM SMDM_SHELLFOLDER}
  SMDM_HMENU                          = $00000002;  // This is for the Band itself
  {$EXTERNALSYM SMDM_HMENU}
  SMDM_TOOLBAR                        = $00000004;  // Plain toolbar, not associated with a shell folder or hmenu
  {$EXTERNALSYM SMDM_TOOLBAR}

  SMC_INITMENU                        = $00000001;  // The callback is called to init a menuband
  {$EXTERNALSYM SMC_INITMENU}
  SMC_CREATE                          = $00000002;
  {$EXTERNALSYM SMC_CREATE}
  SMC_EXITMENU                        = $00000003;  // The callback is called when menu is collapsing
  {$EXTERNALSYM SMC_EXITMENU}
  SMC_GETINFO                         = $00000005;  // The callback is called to return DWORD values
  {$EXTERNALSYM SMC_GETINFO}
  SMC_GETSFINFO                       = $00000006;  // The callback is called to return DWORD values
  {$EXTERNALSYM SMC_GETSFINFO}
  SMC_GETOBJECT                       = $00000007;  // The callback is called to get some object
  {$EXTERNALSYM SMC_GETOBJECT}
  SMC_GETSFOBJECT                     = $00000008;  // The callback is called to get some object
  {$EXTERNALSYM SMC_GETSFOBJECT}
  SMC_SFEXEC                          = $00000009;  // The callback is called to execute an shell folder item
  {$EXTERNALSYM SMC_SFEXEC}
  SMC_SFSELECTITEM                    = $0000000A;  // The callback is called when an item is selected
  {$EXTERNALSYM SMC_SFSELECTITEM}
  SMC_REFRESH                         = $00000010;  // Menus have completely refreshed. Reset your state.
  {$EXTERNALSYM SMC_REFRESH}
  SMC_DEMOTE                          = $00000011;  // Demote an item
  {$EXTERNALSYM SMC_DEMOTE}
  SMC_PROMOTE                         = $00000012;  // Promote an item, wParam = SMINV_* flag
  {$EXTERNALSYM SMC_PROMOTE}
  SMC_DEFAULTICON                     = $00000016;  // Returns Default icon location in wParam, index in lParam
  {$EXTERNALSYM SMC_DEFAULTICON}
  SMC_NEWITEM                         = $00000017;  // Notifies item is not in the order stream.
  {$EXTERNALSYM SMC_NEWITEM}
  SMC_CHEVRONEXPAND                   = $00000019;  // Notifies of a expansion via the chevron
  {$EXTERNALSYM SMC_CHEVRONEXPAND}
  SMC_DISPLAYCHEVRONTIP               = $0000002A;  // S_OK display, S_FALSE not.
  {$EXTERNALSYM SMC_DISPLAYCHEVRONTIP}
  SMC_SETSFOBJECT                     = $0000002D;  // Called to save the passed object
  {$EXTERNALSYM SMC_SETSFOBJECT}
  SMC_SHCHANGENOTIFY                  = $0000002E;  // Called when a Change notify is received. lParam points to SMCSHCHANGENOTIFYSTRUCT
  {$EXTERNALSYM SMC_SHCHANGENOTIFY}
  SMC_CHEVRONGETTIP                   = $0000002F;  // Called to get the chevron tip text. wParam = Tip title, Lparam = TipText Both MAX_PATH
  {$EXTERNALSYM SMC_CHEVRONGETTIP}
  SMC_SFDDRESTRICTED                  = $00000030;  // Called requesting if it's ok to drop. wParam = IDropTarget.
  {$EXTERNALSYM SMC_SFDDRESTRICTED}
//#if (_WIN32_IE >= _WIN32_IE_IE70)
  SMC_SFEXEC_MIDDLE                   = $00000031;  // Same as SFEXEC, but the middle mouse button caused the exec.
  {$EXTERNALSYM SMC_SFEXEC_MIDDLE}
  SMC_GETAUTOEXPANDSTATE              = $00000041;  // callback returns the default autoexpand state lParam = LPDWORD to recieve flags
  {$EXTERNALSYM SMC_GETAUTOEXPANDSTATE}
  SMC_AUTOEXPANDCHANGE                = $00000042;  // Notify that the menu is expanding/contracting
  {$EXTERNALSYM SMC_AUTOEXPANDCHANGE}
  SMC_GETCONTEXTMENUMODIFIER          = $00000043;  // Used to add items to a context menu
  {$EXTERNALSYM SMC_GETCONTEXTMENUMODIFIER}
  SMC_GETBKCONTEXTMENU                = $00000044;  // used to get a context menu to display when user right clicks on the background
  {$EXTERNALSYM SMC_GETBKCONTEXTMENU}
  SMC_OPEN                            = $00000045;  // allows client to overwrite open/explore verb action on an item
  {$EXTERNALSYM SMC_OPEN}
  // Flags for return value from SMC_GETAUTOEXPANDSTATE and SMC_AUTOEXPANDCHANGE:
  SMAE_EXPANDED                       = $00000001;  // The menu is or should start expanded
  {$EXTERNALSYM SMAE_EXPANDED}
  SMAE_CONTRACTED                     = $00000002;  // The menu is or should start contracted
  {$EXTERNALSYM SMAE_CONTRACTED}
                                            // SMAE_EXPANDED and SMAE_CONTRACTED are mutually exclusive
  SMAE_USER                           = $00000004;  // Indicates that the menu expansion/contraction is a reflection
  {$EXTERNALSYM SMAE_USER}
                                            // of user choice
  SMAE_VALID                          = $00000007;
  {$EXTERNALSYM SMAE_VALID}
//#endif  // _WIN32_IE_IE70

  // IShellMenu
  SMINIT_DEFAULT                      = $00000000;  // No Options
  {$EXTERNALSYM SMINIT_DEFAULT}
  SMINIT_RESTRICT_DRAGDROP            = $00000002;  // Don't allow Drag and Drop
  {$EXTERNALSYM SMINIT_RESTRICT_DRAGDROP}
  SMINIT_TOPLEVEL                     = $00000004;  // This is the top band.
  {$EXTERNALSYM SMINIT_TOPLEVEL}
  SMINIT_CACHED                       = $00000010;
  {$EXTERNALSYM SMINIT_CACHED}
//#if (_WIN32_IE >= _WIN32_IE_IE70)
  SMINIT_AUTOEXPAND                   = $00000100;  // automatically expand/contract menu band
  {$EXTERNALSYM SMINIT_AUTOEXPAND}
  SMINIT_AUTOTOOLTIP                  = $00000200;  // regular tooltip support
  {$EXTERNALSYM SMINIT_AUTOTOOLTIP}
  SMINIT_DROPONCONTAINER              = $00000400;  // allow drop on a container
  {$EXTERNALSYM SMINIT_DROPONCONTAINER}
//#endif  // _WIN32_IE_IE70
  SMINIT_VERTICAL                     = $10000000;  // This is a vertical menu
  {$EXTERNALSYM SMINIT_VERTICAL}
  SMINIT_HORIZONTAL                   = $20000000;  // This is a horizontal menu    (does not inherit)
  {$EXTERNALSYM SMINIT_HORIZONTAL}
  ANCESTORDEFAULT                     = UINT(- 1);
  {$EXTERNALSYM ANCESTORDEFAULT}
  SMSET_TOP                           = $10000000;  // Bias this namespace to the top of the menu
  {$EXTERNALSYM SMSET_TOP}
  SMSET_BOTTOM                        = $20000000;  // Bias this namespace to the bottom of the menu
  {$EXTERNALSYM SMSET_BOTTOM}
  SMSET_DONTOWN                       = $00000001;  // The Menuband doesn't own the non-ref counted object
  {$EXTERNALSYM SMSET_DONTOWN}
  SMINV_REFRESH                       = $00000001;
  {$EXTERNALSYM SMINV_REFRESH}
  SMINV_ID                            = $00000008;
  {$EXTERNALSYM SMINV_ID}

  // IBannerNotificationHandler
  ISIOI_ICONFILE                      = $00000001;
  {$EXTERNALSYM ISIOI_ICONFILE}
  ISIOI_ICONINDEX                     = $00000002;
  {$EXTERNALSYM ISIOI_ICONINDEX}

  // ITaskbarList3Vtbl
  THBN_CLICKED                        = $1800;
  {$EXTERNALSYM THBN_CLICKED}

  E_PREVIEWHANDLER_DRM_FAIL           = _HRESULT_TYPEDEF_ ($86420001);
  {$EXTERNALSYM E_PREVIEWHANDLER_DRM_FAIL}
  E_PREVIEWHANDLER_NOAUTH             = _HRESULT_TYPEDEF_ ($86420002);
  {$EXTERNALSYM E_PREVIEWHANDLER_NOAUTH}
  E_PREVIEWHANDLER_NOTFOUND           = _HRESULT_TYPEDEF_ ($86420003);
  {$EXTERNALSYM E_PREVIEWHANDLER_NOTFOUND}
  E_PREVIEWHANDLER_CORRUPT            = _HRESULT_TYPEDEF_ ($86420004);
  {$EXTERNALSYM E_PREVIEWHANDLER_CORRUPT}

// SID =========================================================================

  SID_ExecuteCommandHost              : TGUID = '{4b6832a2-5f04-4c9d-b89d-727a15d103e7}';
  {$EXTERNALSYM SID_ExecuteCommandHost}
  SID_URLExecutionContext             : TGUID = '{FB5F8EBC-BBB6-4D10-A461-777291A09030}';
  {$EXTERNALSYM SID_URLExecutionContext}
  SID_LaunchTargetMonitor             : TGUID = '{8D547FA1-CC45-40C8-B7C1-D48C183F13F3}';
  {$EXTERNALSYM SID_LaunchTargetMonitor}
  SID_LaunchSourceViewSizePreference  : TGUID = '{80605492-67d9-414f-af89-a1cdf1242bc1}';
  {$EXTERNALSYM SID_LaunchSourceViewSizePreference}
  SID_LaunchSourceAppUserModelId      : TGUID = '{2ce78010-74db-48bc-9c6a-10f372495723}';
  {$EXTERNALSYM SID_LaunchSourceAppUserModelId}
  SID_HandlerInfo                     : TGUID = '{31cca04c-04d3-4ea9-90de-97b15e87a532}';
  {$EXTERNALSYM SID_HandlerInfo}
  SID_SHandlerActivationHost          : TGUID = '{35094a87-8bb1-4237-96c6-c417eebdb078}';
  {$EXTERNALSYM SID_SHandlerActivationHost}
  SID_AppActivationUIInfo             : TGUID = '{abad189d-9fa3-4278-b3ca-8ca448a88dcb}';
  {$EXTERNALSYM SID_AppActivationUIInfo}
  SID_ShellExecuteNamedPropertyStore  : TGUID = '{eb84ada2-00ff-4992-8324-ed5ce061cb29}';
  {$EXTERNALSYM SID_ShellExecuteNamedPropertyStore}
  SID_ShellTaskScheduler              : TGUID = '{6CCB7BE0-6807-11d0-B810-00C04FD706EC}';
  {$EXTERNALSYM SID_ShellTaskScheduler}
  SID_SFolderView                     : TGUID = '{cde725b0-ccc9-4519-917e-325d72fab4ce}';
  {$EXTERNALSYM SID_SFolderView}
  SID_SExplorerBrowserFrame           : TGUID = '{000214F1-0000-0000-C000-000000000046}'; // IID_ICommDlgBrowser
  {$EXTERNALSYM SID_SExplorerBrowserFrame}

  SID_SProfferService                 : TGUID = '{cb728b20-f786-11ce-92ad-00aa00a74cd0}';
  {$EXTERNALSYM SID_SProfferService}
  SID_SNewWindowManager               : TGUID = '{D2BC4C84-3F72-4a52-A604-7BCBF3982CBB}';
  {$EXTERNALSYM SID_SNewWindowManager}
  SID_ExecuteCreatingProcess          : TGUID = '{c2b937a9-3110-4398-8a56-f34c6342d244}';
  {$EXTERNALSYM SID_ExecuteCreatingProcess}
  SID_LaunchUIContextProvider         : TGUID = '{0d12c4c8-a3d9-4e24-94c1-0e20c5a956c4}';
  {$EXTERNALSYM SID_LaunchUIContextProvider}
  SID_SNewMenuClient                  : TGUID = '{dcb07fdc-3bb5-451c-90be-966644fed7b0}';
  {$EXTERNALSYM SID_SNewMenuClient}
  SID_SNavigationPane                 : TGUID = '{028212A3-B627-47e9-8856-C14265554E4F}';
  {$EXTERNALSYM SID_SNavigationPane}
  SID_ExplorerPaneVisibility          : TGUID = '{e07010ec-bc17-44c0-97b0-46c7c95b9edc}';
  {$EXTERNALSYM SID_ExplorerPaneVisibility}


// CLSID =======================================================================

  CLSID_DesktopWallpaper              : TGUID = '{C2CF3110-460E-4FC1-B9D0-8A1C0C9CC4BD}';
  {$EXTERNALSYM CLSID_DesktopWallpaper}
  CLSID_ShellDesktop                  : TGUID = '{00021400-0000-0000-C000-000000000046}';
  {$EXTERNALSYM CLSID_ShellDesktop}
  CLSID_ShellFSFolder                 : TGUID = '{F3364BA0-65B9-11CE-A9BA-00AA004AE837}';
  {$EXTERNALSYM CLSID_ShellFSFolder}
  CLSID_NetworkPlaces                 : TGUID = '{208D2C60-3AEA-1069-A2D7-08002B30309D}';
  {$EXTERNALSYM CLSID_NetworkPlaces}
  CLSID_ShellLink                     : TGUID = '{00021401-0000-0000-C000-000000000046}';
  {$EXTERNALSYM CLSID_ShellLink}
  CLSID_DriveSizeCategorizer          : TGUID = '{94357B53-CA29-4B78-83AE-E8FE7409134F}';
  {$EXTERNALSYM CLSID_DriveSizeCategorizer}
  CLSID_DriveTypeCategorizer          : TGUID = '{B0A8F3CF-4333-4BAB-8873-1CCB1CADA48B}';
  {$EXTERNALSYM CLSID_DriveTypeCategorizer}
  CLSID_FreeSpaceCategorizer          : TGUID = '{B5607793-24AC-44C7-82E2-831726AA6CB7}';
  {$EXTERNALSYM CLSID_FreeSpaceCategorizer}
  CLSID_SizeCategorizer               : TGUID = '{55D7B852-F6D1-42F2-AA75-8728A1B2D264}';
  {$EXTERNALSYM CLSID_SizeCategorizer}
  CLSID_PropertiesUI                  : TGUID = '{D912F8CF-0396-4915-884E-FB425D32943B}';
  {$EXTERNALSYM CLSID_PropertiesUI}
  CLSID_UserNotification              : TGUID = '{0010890E-8789-413C-ADBC-48F5B511B3AF}';
  {$EXTERNALSYM CLSID_UserNotification}
  CLSID_TaskbarList                   : TGUID = '{56FDF344-FD6D-11D0-958A-006097C9A090}';
  {$EXTERNALSYM CLSID_TaskbarList}
  CLSID_ShellItem                     : TGUID = '{9AC9FBE1-E0A2-4AD6-B4EE-E212013EA917}';
  {$EXTERNALSYM CLSID_ShellItem}
  CLSID_NamespaceWalker               : TGUID = '{72EB61E0-8672-4303-9175-F2E4C68B2E7C}';
  {$EXTERNALSYM CLSID_NamespaceWalker}
  CLSID_FileOperation                 : TGUID = '{3AD05575-8857-4850-9277-11B85BDB8E09}';
  {$EXTERNALSYM CLSID_FileOperation}
  CLSID_FileOpenDialog                : TGUID = '{DC1C5A9C-E88A-4DDE-A5A1-60F82A20AEF7}';
  {$EXTERNALSYM CLSID_FileOpenDialog}
  CLSID_FileSaveDialog                : TGUID = '{C0B4E2F3-BA21-4773-8DBA-335EC946EB8B}';
  {$EXTERNALSYM CLSID_FileSaveDialog}
  CLSID_KnownFolderManager            : TGUID = '{4DF0C730-DF9D-4AE3-9153-AA6B82E9795A}';
  {$EXTERNALSYM CLSID_KnownFolderManager}
  CLSID_SharingConfigurationManager   : TGUID = '{49F371E1-8C5C-4D9C-9A3B-54A6827F513C}';
  {$EXTERNALSYM CLSID_SharingConfigurationManager}
  CLSID_NetworkConnections            : TGUID = '{7007ACC7-3202-11D1-AAD2-00805FC1270E}';
  {$EXTERNALSYM CLSID_NetworkConnections}
  CLSID_ScheduledTasks                : TGUID = '{D6277990-4C6A-11CF-8D87-00AA0060F5BF}';
  {$EXTERNALSYM CLSID_ScheduledTasks}
  CLSID_ApplicationAssociationRegistration : TGUID = '{591209C7-767B-42B2-9FBA-44EE4615F2C7}';
  {$EXTERNALSYM CLSID_ApplicationAssociationRegistration}
  CLSID_SearchFolderItemFactory       : TGUID = '{14010E02-BBBD-41F0-88E3-EDA371216584}';
  {$EXTERNALSYM CLSID_SearchFolderItemFactory}
  CLSID_OpenControlPanel              : TGUID = '{06622D85-6856-4460-8DE1-A81921B41C4B}';
  {$EXTERNALSYM CLSID_OpenControlPanel}
  CLSID_MailRecipient                 : TGUID = '{9E56BE60-C50F-11CF-9A2C-00A0C90A90CE}';
  {$EXTERNALSYM CLSID_MailRecipient}
  CLSID_NetworkExplorerFolder         : TGUID = '{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}';
  {$EXTERNALSYM CLSID_NetworkExplorerFolder}
  CLSID_DestinationList               : TGUID = '{77F10CF0-3DB5-4966-B520-B7C54FD35ED6}';
  {$EXTERNALSYM CLSID_DestinationList}
  CLSID_ApplicationDestinations       : TGUID = '{86C14003-4D6B-4EF3-A7B4-0506663B2E68}';
  {$EXTERNALSYM CLSID_ApplicationDestinations}
  CLSID_ApplicationDocumentLists      : TGUID = '{86BEC222-30F2-47E0-9F25-60D11CD75C28}';
  {$EXTERNALSYM CLSID_ApplicationDocumentLists}
  CLSID_HomeGroup                     : TGUID = '{DE77BA04-3C92-4D11-A1A5-42352A53E0E3}';
  {$EXTERNALSYM CLSID_HomeGroup}
  CLSID_ShellLibrary                  : TGUID = '{D9B3211D-E57F-4426-AAEF-30A806ADD397}';
  {$EXTERNALSYM CLSID_ShellLibrary}
  CLSID_AppStartupLink                : TGUID = '{273EB5E7-88B0-4843-BFEF-E2C81D43AAE5}';
  {$EXTERNALSYM CLSID_AppStartupLink}
  CLSID_EnumerableObjectCollection    : TGUID = '{2D3468C1-36A7-43B6-AC24-D3F02FD9607A}';
  {$EXTERNALSYM CLSID_EnumerableObjectCollection}
  CLSID_FrameworkInputPane            : TGUID = '{D5120AA3-46BA-44C5-822D-CA8092C1FC72}';
  {$EXTERNALSYM CLSID_FrameworkInputPane}
  CLSID_DefFolderMenu                 : TGUID = '{C63382BE-7933-48D0-9AC8-85FB46BE2FDD}';
  {$EXTERNALSYM CLSID_DefFolderMenu}
  CLSID_AppVisibility                 : TGUID = '{7E5FE3D9-985F-4908-91F9-EE19F9FD1514}';
  {$EXTERNALSYM CLSID_AppVisibility}
  CLSID_AppShellVerbHandler           : TGUID = '{4ED3A719-CEA8-4BD9-910D-E252F997AFC2}';
  {$EXTERNALSYM CLSID_AppShellVerbHandler}
  CLSID_ExecuteUnknown                : TGUID = '{E44E9428-BDBC-4987-A099-40DC8FD255E7}';
  {$EXTERNALSYM CLSID_ExecuteUnknown}
  CLSID_PackageDebugSettings          : TGUID = '{B1AEC16F-2383-4852-B0E9-8F0B1DC66B4D}';
  {$EXTERNALSYM CLSID_PackageDebugSettings}
  CLSID_SuspensionDependencyManager   : TGUID = '{6B273FC5-61FD-4918-95A2-C3B5E9D7F581}';
  {$EXTERNALSYM CLSID_SuspensionDependencyManager}
  CLSID_ApplicationActivationManager  : TGUID = '{45BA127D-10A8-46EA-8AB7-56EA9078943C}';
  {$EXTERNALSYM CLSID_ApplicationActivationManager}
  CLSID_ApplicationDesignModeSettings : TGUID = '{958A6FB5-DCB2-4FAF-AAFD-7FB054AD1A3B}';
  {$EXTERNALSYM CLSID_ApplicationDesignModeSettings}


// ENUMS =======================================================================

type
  PKF_CATEGORY = ^KF_CATEGORY;
  KF_CATEGORY = type Integer;
  {$EXTERNALSYM KF_CATEGORY}
const
  KF_CATEGORY_VIRTUAL = KF_CATEGORY(1);
  {$EXTERNALSYM KF_CATEGORY_VIRTUAL}
  KF_CATEGORY_FIXED   = KF_CATEGORY(2);
  {$EXTERNALSYM KF_CATEGORY_FIXED}
  KF_CATEGORY_COMMON  = KF_CATEGORY(3);
  {$EXTERNALSYM KF_CATEGORY_COMMON}
  KF_CATEGORY_PERUSER = KF_CATEGORY(4);
  {$EXTERNALSYM KF_CATEGORY_PERUSER}

type
  PKF_DEFINITION_FLAGS = ^KF_DEFINITION_FLAGS;
  KF_DEFINITION_FLAGS = type DWORD;
  {$EXTERNALSYM KF_DEFINITION_FLAGS}
const
  KFDF_LOCAL_REDIRECT_ONLY = KF_DEFINITION_FLAGS($2);
  {$EXTERNALSYM KFDF_LOCAL_REDIRECT_ONLY}
  KFDF_ROAMABLE	           = KF_DEFINITION_FLAGS($4);
  {$EXTERNALSYM KFDF_ROAMABLE}
  KFDF_PRECREATE           = KF_DEFINITION_FLAGS($8);
  {$EXTERNALSYM KFDF_PRECREATE}
  KFDF_STREAM	             = KF_DEFINITION_FLAGS($10);
  {$EXTERNALSYM KFDF_STREAM}
  KFDF_UBLISHEXPANDEDPATH	 = KF_DEFINITION_FLAGS($20);
  {$EXTERNALSYM KFDF_UBLISHEXPANDEDPATH}
  KFDF_NO_REDIRECT_UI	     = KF_DEFINITION_FLAGS($40);
  {$EXTERNALSYM KFDF_NO_REDIRECT_UI}

type
  PKF_REDIRECT_FLAGS = ^KF_REDIRECT_FLAGS;
  KF_REDIRECT_FLAGS = type DWORD;
  {$EXTERNALSYM KF_REDIRECT_FLAGS}
const
  KF_REDIRECT_USER_EXCLUSIVE               	= KF_REDIRECT_FLAGS($1);
  {$EXTERNALSYM KF_REDIRECT_USER_EXCLUSIVE}
  KF_REDIRECT_COPY_SOURCE_DACL	            = KF_REDIRECT_FLAGS($2);
  {$EXTERNALSYM KF_REDIRECT_COPY_SOURCE_DACL}
  KF_REDIRECT_OWNER_USER	                  = KF_REDIRECT_FLAGS($4);
  {$EXTERNALSYM KF_REDIRECT_OWNER_USER}
  KF_REDIRECT_SET_OWNER_EXPLICIT	          = KF_REDIRECT_FLAGS($8);
  {$EXTERNALSYM KF_REDIRECT_SET_OWNER_EXPLICIT}
  KF_REDIRECT_CHECK_ONLY        	          = KF_REDIRECT_FLAGS($10);
  {$EXTERNALSYM KF_REDIRECT_CHECK_ONLY}
  KF_REDIRECT_WITH_UI           	          = KF_REDIRECT_FLAGS($20);
  {$EXTERNALSYM KF_REDIRECT_WITH_UI}
  KF_REDIRECT_UNPIN                        	= KF_REDIRECT_FLAGS($40);
  {$EXTERNALSYM KF_REDIRECT_UNPIN}
  KF_REDIRECT_PIN                          	= KF_REDIRECT_FLAGS($80);
  {$EXTERNALSYM KF_REDIRECT_PIN}
  KF_REDIRECT_COPY_CONTENTS	                = KF_REDIRECT_FLAGS($200);
  {$EXTERNALSYM KF_REDIRECT_COPY_CONTENTS}
  KF_REDIRECT_DEL_SOURCE_CONTENTS         	= KF_REDIRECT_FLAGS($400);
  {$EXTERNALSYM KF_REDIRECT_DEL_SOURCE_CONTENTS}
  KF_REDIRECT_EXCLUDE_ALL_KNOWN_SUBFOLDERS	= KF_REDIRECT_FLAGS($800);
  {$EXTERNALSYM KF_REDIRECT_EXCLUDE_ALL_KNOWN_SUBFOLDERS}

type
  PKF_REDIRECTION_CAPABILITIES = ^KF_REDIRECTION_CAPABILITIES;
  KF_REDIRECTION_CAPABILITIES = type DWORD;
  {$EXTERNALSYM KF_REDIRECTION_CAPABILITIES}
const
  KF_REDIRECTION_CAPABILITIES_ALLOW_ALL             	= KF_REDIRECTION_CAPABILITIES($ff);
  {$EXTERNALSYM KF_REDIRECTION_CAPABILITIES_ALLOW_ALL}
  KF_REDIRECTION_CAPABILITIES_REDIRECTABLE          	= KF_REDIRECTION_CAPABILITIES($1);
  {$EXTERNALSYM KF_REDIRECTION_CAPABILITIES_REDIRECTABLE}
  KF_REDIRECTION_CAPABILITIES_DENY_ALL              	= KF_REDIRECTION_CAPABILITIES($fff00);
  {$EXTERNALSYM KF_REDIRECTION_CAPABILITIES_DENY_ALL}
  KF_REDIRECTION_CAPABILITIES_DENY_POLICY_REDIRECTED	= KF_REDIRECTION_CAPABILITIES($100);
  {$EXTERNALSYM KF_REDIRECTION_CAPABILITIES_DENY_POLICY_REDIRECTED}
  KF_REDIRECTION_CAPABILITIES_DENY_POLICY           	= KF_REDIRECTION_CAPABILITIES($200);
  {$EXTERNALSYM KF_REDIRECTION_CAPABILITIES_DENY_POLICY}
  KF_REDIRECTION_CAPABILITIES_DENY_PERMISSIONS	      = KF_REDIRECTION_CAPABILITIES($400);
  {$EXTERNALSYM KF_REDIRECTION_CAPABILITIES_DENY_PERMISSIONS}

type
  PFFFP_MODE = ^FFFP_MODE;
  FFFP_MODE = type DWORD;
  {$EXTERNALSYM FFFP_MODE}
const
  FFFP_EXACTMATCH         = FFFP_MODE(0);
  {$EXTERNALSYM FFFP_EXACTMATCH}
  FFFP_NEARESTPARENTMATCH = FFFP_MODE((FFFP_EXACTMATCH + 1));
  {$EXTERNALSYM FFFP_NEARESTPARENTMATCH}

type
  PSHGDNF = ^SHGDNF;
  _SHGDNF = type DWORD;
  SHGDNF = _SHGDNF;
  {$EXTERNALSYM _SHGDNF}
  {$EXTERNALSYM SHGDNF}
const
  SHGDN_NORMAL        = _SHGDNF(0);
  {$EXTERNALSYM SHGDN_NORMAL}
  SHGDN_INFOLDER      = _SHGDNF($1);
  {$EXTERNALSYM SHGDN_INFOLDER}
  SHGDN_FOREDITING    = _SHGDNF($1000);
  {$EXTERNALSYM SHGDN_FOREDITING}
  SHGDN_FORADDRESSBAR = _SHGDNF($4000);
  {$EXTERNALSYM SHGDN_FORADDRESSBAR}
  SHGDN_FORPARSING    = _SHGDNF($8000);
  {$EXTERNALSYM SHGDN_FORPARSING}

type
  PSHCONTF = ^SHCONTF;
  _SHCONTF = DWORD;
  SHCONTF = _SHCONTF;
  {$EXTERNALSYM _SHCONTF}
  {$EXTERNALSYM SHCONTF}
const
  SHCONTF_CHECKING_FOR_CHILDREN = _SHCONTF($10);
  {$EXTERNALSYM SHCONTF_CHECKING_FOR_CHILDREN}
  SHCONTF_FOLDERS               = _SHCONTF($20);
  {$EXTERNALSYM SHCONTF_FOLDERS}
  SHCONTF_NONFOLDERS            = _SHCONTF($40);
  {$EXTERNALSYM SHCONTF_NONFOLDERS}
  SHCONTF_INCLUDEHIDDEN         = _SHCONTF($80);
  {$EXTERNALSYM SHCONTF_INCLUDEHIDDEN}
  SHCONTF_INIT_ON_FIRST_NEXT    = _SHCONTF($100);
  {$EXTERNALSYM SHCONTF_INIT_ON_FIRST_NEXT}
  SHCONTF_NETPRINTERSRCH        = _SHCONTF($200);
  {$EXTERNALSYM SHCONTF_NETPRINTERSRCH}
  SHCONTF_SHAREABLE             = _SHCONTF($400);
  {$EXTERNALSYM SHCONTF_SHAREABLE}
  SHCONTF_STORAGE               = _SHCONTF($800);
  {$EXTERNALSYM SHCONTF_STORAGE}
  SHCONTF_NAVIGATION_ENUM       = _SHCONTF($1000);
  {$EXTERNALSYM SHCONTF_NAVIGATION_ENUM}
  SHCONTF_FASTITEMS             = _SHCONTF($2000);
  {$EXTERNALSYM SHCONTF_FASTITEMS}
  SHCONTF_FLATLIST              = _SHCONTF($4000);
  {$EXTERNALSYM SHCONTF_FLATLIST}
  SHCONTF_ENABLE_ASYNC          = _SHCONTF($8000);
  {$EXTERNALSYM SHCONTF_ENABLE_ASYNC}
  SHCONTF_INCLUDESUPERHIDDEN    = _SHCONTF($10000);
  {$EXTERNALSYM SHCONTF_INCLUDESUPERHIDDEN}

type
  PSFGAOF = ^SFGAOF;
  SFGAOF = type ULONG;
  {$EXTERNALSYM SFGAOF}

type
  PSYNC_TRANSFER_STATUS = ^SYNC_TRANSFER_STATUS;
  _SYNC_TRANSFER_STATUS = type Integer;
  SYNC_TRANSFER_STATUS = _SYNC_TRANSFER_STATUS;
  {$EXTERNALSYM _SYNC_TRANSFER_STATUS}
  {$EXTERNALSYM SYNC_TRANSFER_STATUS}
const
  STS_NONE                   = _SYNC_TRANSFER_STATUS(0);
  {$EXTERNALSYM STS_NONE}
  STS_NEEDSUPLOAD            = _SYNC_TRANSFER_STATUS($1);
  {$EXTERNALSYM STS_NEEDSUPLOAD}
  STS_NEEDSDOWNLOAD          = _SYNC_TRANSFER_STATUS($2);
  {$EXTERNALSYM STS_NEEDSDOWNLOAD}
  STS_TRANSFERRING           = _SYNC_TRANSFER_STATUS($4);
  {$EXTERNALSYM STS_TRANSFERRING}
  STS_PAUSED                 = _SYNC_TRANSFER_STATUS($8);
  {$EXTERNALSYM STS_PAUSED}
  STS_HASERROR               = _SYNC_TRANSFER_STATUS($10);
  {$EXTERNALSYM STS_HASERROR}
  STS_FETCHING_METADATA      = _SYNC_TRANSFER_STATUS($20);
  {$EXTERNALSYM STS_FETCHING_METADATA}
  STS_USER_REQUESTED_REFRESH = _SYNC_TRANSFER_STATUS($40);
  {$EXTERNALSYM STS_USER_REQUESTED_REFRESH}
  STS_HASWARNING             = _SYNC_TRANSFER_STATUS($80);
  {$EXTERNALSYM STS_HASWARNING}
  STS_EXCLUDED               = _SYNC_TRANSFER_STATUS($100);
  {$EXTERNALSYM STS_EXCLUDED}
  STS_INCOMPLETE             = _SYNC_TRANSFER_STATUS($200);
  {$EXTERNALSYM STS_INCOMPLETE}
  STS_PLACEHOLDER_IFEMPTY    = _SYNC_TRANSFER_STATUS($400);
  {$EXTERNALSYM STS_PLACEHOLDER_IFEMPTY}

type
  PSTORAGE_PROVIDER_FILE_FLAGS = ^STORAGE_PROVIDER_FILE_FLAGS;
  _STORAGE_PROVIDER_FILE_FLAGS = type Integer;
  STORAGE_PROVIDER_FILE_FLAGS = _STORAGE_PROVIDER_FILE_FLAGS;
  {$EXTERNALSYM _STORAGE_PROVIDER_FILE_FLAGS}
  {$EXTERNALSYM STORAGE_PROVIDER_FILE_FLAGS}
const
  SPFF_NONE                   = _STORAGE_PROVIDER_FILE_FLAGS(0);
  {$EXTERNALSYM SPFF_NONE}
  SPFF_DOWNLOAD_BY_DEFAULT    = _STORAGE_PROVIDER_FILE_FLAGS($1);
  {$EXTERNALSYM SPFF_DOWNLOAD_BY_DEFAULT}
  SPFF_CREATED_ON_THIS_DEVICE = _STORAGE_PROVIDER_FILE_FLAGS($2);
  {$EXTERNALSYM SPFF_CREATED_ON_THIS_DEVICE}

type
  PPLACEHOLDER_STATES = ^PLACEHOLDER_STATES;
  _PLACEHOLDER_STATES = type Integer;
  {$EXTERNALSYM _PLACEHOLDER_STATES}
  PLACEHOLDER_STATES = _PLACEHOLDER_STATES;
  {$EXTERNALSYM PLACEHOLDER_STATES}
const
  PS_NONE                            = _PLACEHOLDER_STATES(0);
  {$EXTERNALSYM PS_NONE}
  PS_MARKED_FOR_OFFLINE_AVAILABILITY = _PLACEHOLDER_STATES($1);
  {$EXTERNALSYM PS_MARKED_FOR_OFFLINE_AVAILABILITY}
  PS_FULL_PRIMARY_STREAM_AVAILABLE   = _PLACEHOLDER_STATES($2);
  {$EXTERNALSYM PS_FULL_PRIMARY_STREAM_AVAILABLE}
  PS_CREATE_FILE_ACCESSIBLE          = _PLACEHOLDER_STATES($4);
  {$EXTERNALSYM PS_CREATE_FILE_ACCESSIBLE}
  PS_CLOUDFILE_PLACEHOLDER           = _PLACEHOLDER_STATES($8);
  {$EXTERNALSYM PS_CLOUDFILE_PLACEHOLDER}
  PS_DEFAULT                         = _PLACEHOLDER_STATES(PS_MARKED_FOR_OFFLINE_AVAILABILITY or PS_FULL_PRIMARY_STREAM_AVAILABLE or PS_CREATE_FILE_ACCESSIBLE);
  {$EXTERNALSYM PS_DEFAULT}
  PS_ALL                             = _PLACEHOLDER_STATES(PS_MARKED_FOR_OFFLINE_AVAILABILITY or PS_FULL_PRIMARY_STREAM_AVAILABLE or PS_CREATE_FILE_ACCESSIBLE or PS_CLOUDFILE_PLACEHOLDER);
  {$EXTERNALSYM PS_ALL}

type
  PMERGE_UPDATE_STATUS = ^MERGE_UPDATE_STATUS;
  _MERGE_UPDATE_STATUS = type Integer;
  {$EXTERNALSYM _MERGE_UPDATE_STATUS}
  MERGE_UPDATE_STATUS = _MERGE_UPDATE_STATUS;
  {$EXTERNALSYM MERGE_UPDATE_STATUS}
const
  MUS_COMPLETE        = _MERGE_UPDATE_STATUS(0);
  {$EXTERNALSYM MUS_COMPLETE}
  MUS_USERINPUTNEEDED = _MERGE_UPDATE_STATUS(MUS_COMPLETE + 1);
  {$EXTERNALSYM MUS_USERINPUTNEEDED}
  MUS_FAILED          = _MERGE_UPDATE_STATUS(MUS_USERINPUTNEEDED + 1);
  {$EXTERNALSYM MUS_FAILED}

type
  PFOLDER_ENUM_MODE = ^FOLDER_ENUM_MODE;
  _FOLDER_ENUM_MODE = type Integer;
  {$EXTERNALSYM _FOLDER_ENUM_MODE}
  FOLDER_ENUM_MODE = _FOLDER_ENUM_MODE;
  {$EXTERNALSYM FOLDER_ENUM_MODE}
const
  FEM_VIEWRESULT = _FOLDER_ENUM_MODE(0);
  {$EXTERNALSYM FEM_VIEWRESULT}
  FEM_NAVIGATION = _FOLDER_ENUM_MODE(1);
  {$EXTERNALSYM FEM_NAVIGATION}

type
  PFOLDERFLAGS = ^FOLDERFLAGS;
  FOLDERFLAGS = type Integer;
  {$EXTERNALSYM FOLDERFLAGS}
const
  FWF_NONE                = FOLDERFLAGS(0);
  {$EXTERNALSYM FWF_NONE}
  FWF_AUTOARRANGE         = FOLDERFLAGS($1);
  {$EXTERNALSYM FWF_AUTOARRANGE}
  FWF_ABBREVIATEDNAMES    = FOLDERFLAGS($2);
  {$EXTERNALSYM FWF_ABBREVIATEDNAMES}
  FWF_SNAPTOGRID          = FOLDERFLAGS($4);
  {$EXTERNALSYM FWF_SNAPTOGRID}
  FWF_OWNERDATA           = FOLDERFLAGS($8);
  {$EXTERNALSYM FWF_OWNERDATA}
  FWF_BESTFITWINDOW       = FOLDERFLAGS($10);
  {$EXTERNALSYM FWF_BESTFITWINDOW}
  FWF_DESKTOP             = FOLDERFLAGS($20);
  {$EXTERNALSYM FWF_DESKTOP}
  FWF_SINGLESEL           = FOLDERFLAGS($40);
  {$EXTERNALSYM FWF_SINGLESEL}
  FWF_NOSUBFOLDERS        = FOLDERFLAGS($80);
  {$EXTERNALSYM FWF_NOSUBFOLDERS}
  FWF_TRANSPARENT         = FOLDERFLAGS($100);
  {$EXTERNALSYM FWF_TRANSPARENT}
  FWF_NOCLIENTEDGE        = FOLDERFLAGS($200);
  {$EXTERNALSYM FWF_NOCLIENTEDGE}
  FWF_NOSCROLL            = FOLDERFLAGS($400);
  {$EXTERNALSYM FWF_NOSCROLL}
  FWF_ALIGNLEFT           = FOLDERFLAGS($800);
  {$EXTERNALSYM FWF_ALIGNLEFT}
  FWF_NOICONS             = FOLDERFLAGS($1000);
  {$EXTERNALSYM FWF_NOICONS}
  FWF_SHOWSELALWAYS       = FOLDERFLAGS($2000);
  {$EXTERNALSYM FWF_SHOWSELALWAYS}
  FWF_NOVISIBLE           = FOLDERFLAGS($4000);
  {$EXTERNALSYM FWF_NOVISIBLE}
  FWF_SINGLECLICKACTIVATE = FOLDERFLAGS($8000);
  {$EXTERNALSYM FWF_SINGLECLICKACTIVATE}
  FWF_NOWEBVIEW           = FOLDERFLAGS($10000);
  {$EXTERNALSYM FWF_NOWEBVIEW}
  FWF_HIDEFILENAMES       = FOLDERFLAGS($20000);
  {$EXTERNALSYM FWF_HIDEFILENAMES}
  FWF_CHECKSELECT         = FOLDERFLAGS($40000);
  {$EXTERNALSYM FWF_CHECKSELECT}
  FWF_NOENUMREFRESH       = FOLDERFLAGS($80000);
  {$EXTERNALSYM FWF_NOENUMREFRESH}
  FWF_NOGROUPING          = FOLDERFLAGS($100000);
  {$EXTERNALSYM FWF_NOGROUPING}
  FWF_FULLROWSELECT       = FOLDERFLAGS($200000);
  {$EXTERNALSYM FWF_FULLROWSELECT}
  FWF_NOFILTERS           = FOLDERFLAGS($400000);
  {$EXTERNALSYM FWF_NOFILTERS}
  FWF_NOCOLUMNHEADER      = FOLDERFLAGS($800000);
  {$EXTERNALSYM FWF_NOCOLUMNHEADER}
  FWF_NOHEADERINALLVIEWS  = FOLDERFLAGS($1000000);
  {$EXTERNALSYM FWF_NOHEADERINALLVIEWS}
  FWF_EXTENDEDTILES       = FOLDERFLAGS($2000000);
  {$EXTERNALSYM FWF_EXTENDEDTILES}
  FWF_TRICHECKSELECT      = FOLDERFLAGS($4000000);
  {$EXTERNALSYM FWF_TRICHECKSELECT}
  FWF_AUTOCHECKSELECT     = FOLDERFLAGS($8000000);
  {$EXTERNALSYM FWF_AUTOCHECKSELECT}
  FWF_NOBROWSERVIEWSTATE  = FOLDERFLAGS($10000000);
  {$EXTERNALSYM FWF_NOBROWSERVIEWSTATE}
  FWF_SUBSETGROUPS        = FOLDERFLAGS($20000000);
  {$EXTERNALSYM FWF_SUBSETGROUPS}
  FWF_USESEARCHFOLDER     = FOLDERFLAGS($40000000);
  {$EXTERNALSYM FWF_USESEARCHFOLDER}
  FWF_ALLOWRTLREADING     = FOLDERFLAGS($80000000);
  {$EXTERNALSYM FWF_ALLOWRTLREADING}

type
  PFOLDERVIEWMODE = ^FOLDERVIEWMODE;
  FOLDERVIEWMODE = type Integer;
  {$EXTERNALSYM FOLDERVIEWMODE}
const
  FVM_AUTO       = FOLDERVIEWMODE(-1);
  {$EXTERNALSYM FVM_AUTO}
  FVM_FIRST      = FOLDERVIEWMODE(1);
  {$EXTERNALSYM FVM_FIRST}
  FVM_ICON       = FOLDERVIEWMODE(1);
  {$EXTERNALSYM FVM_ICON}
  FVM_SMALLICON  = FOLDERVIEWMODE(2);
  {$EXTERNALSYM FVM_SMALLICON}
  FVM_LIST       = FOLDERVIEWMODE(3);
  {$EXTERNALSYM FVM_LIST}
  FVM_DETAILS    = FOLDERVIEWMODE(4);
  {$EXTERNALSYM FVM_DETAILS}
  FVM_THUMBNAIL  = FOLDERVIEWMODE(5);
  {$EXTERNALSYM FVM_THUMBNAIL}
  FVM_TILE       = FOLDERVIEWMODE(6);
  {$EXTERNALSYM FVM_TILE}
  FVM_THUMBSTRIP = FOLDERVIEWMODE(7);
  {$EXTERNALSYM FVM_THUMBSTRIP}
  FVM_CONTENT    = FOLDERVIEWMODE(8);
  {$EXTERNALSYM FVM_CONTENT}
  FVM_LAST       = FOLDERVIEWMODE(8);
  {$EXTERNALSYM FVM_LAST}

type
  PFOLDERLOGICALVIEWMODE = ^FOLDERLOGICALVIEWMODE;
  FOLDERLOGICALVIEWMODE = type Integer;
  {$EXTERNALSYM FOLDERLOGICALVIEWMODE}
const
  FLVM_UNSPECIFIED = FOLDERLOGICALVIEWMODE(-1);
  {$EXTERNALSYM FLVM_UNSPECIFIED}
  FLVM_FIRST       = FOLDERLOGICALVIEWMODE(1);
  {$EXTERNALSYM FLVM_FIRST}
  FLVM_DETAILS     = FOLDERLOGICALVIEWMODE(1);
  {$EXTERNALSYM FLVM_DETAILS}
  FLVM_TILES       = FOLDERLOGICALVIEWMODE(2);
  {$EXTERNALSYM FLVM_TILES}
  FLVM_ICONS       = FOLDERLOGICALVIEWMODE(3);
  {$EXTERNALSYM FLVM_ICONS}
  FLVM_LIST        = FOLDERLOGICALVIEWMODE(4);
  {$EXTERNALSYM FLVM_LIST}
  FLVM_CONTENT     = FOLDERLOGICALVIEWMODE(5);
  {$EXTERNALSYM FLVM_CONTENT}
  FLVM_LAST        = FOLDERLOGICALVIEWMODE(5);
  {$EXTERNALSYM FLVM_LAST}

type
  SHELLVIEWID = type TGUID;
  {$EXTERNALSYM SHELLVIEWID}
const
  SV2GV_CURRENTVIEW = UINT(-1);
  {$EXTERNALSYM SV2GV_CURRENTVIEW}
  SV2GV_DEFAULTVIEW = UINT(-2);
  {$EXTERNALSYM SV2GV_DEFAULTVIEW}

type
  PSVSIF = ^SVSIF;
  _SVSIF = type UINT;
  SVSIF = _SVSIF;
  {$EXTERNALSYM _SVSIF}
  {$EXTERNALSYM SVSIF}
const
  SVSI_DESELECT       = _SVSIF(0);
  {$EXTERNALSYM SVSI_DESELECT}
  SVSI_SELECT         = _SVSIF($1);
  {$EXTERNALSYM SVSI_SELECT}
  SVSI_EDIT           = _SVSIF($3);
  {$EXTERNALSYM SVSI_EDIT}
  SVSI_DESELECTOTHERS = _SVSIF($4);
  {$EXTERNALSYM SVSI_DESELECTOTHERS}
  SVSI_ENSUREVISIBLE  = _SVSIF($8);
  {$EXTERNALSYM SVSI_ENSUREVISIBLE}
  SVSI_FOCUSED        = _SVSIF($10);
  {$EXTERNALSYM SVSI_FOCUSED}
  SVSI_TRANSLATEPT    = _SVSIF($20);
  {$EXTERNALSYM SVSI_TRANSLATEPT}
  SVSI_SELECTIONMARK  = _SVSIF($40);
  {$EXTERNALSYM SVSI_SELECTIONMARK}
  SVSI_POSITIONITEM   = _SVSIF($80);
  {$EXTERNALSYM SVSI_POSITIONITEM}
  SVSI_CHECK          = _SVSIF($100);
  {$EXTERNALSYM SVSI_CHECK}
  SVSI_CHECK2         = _SVSIF($200);
  {$EXTERNALSYM SVSI_CHECK2}
  SVSI_KEYBOARDSELECT = _SVSIF($401);
  {$EXTERNALSYM SVSI_KEYBOARDSELECT}
  SVSI_NOTAKEFOCUS    = _SVSIF($40000000);
  {$EXTERNALSYM SVSI_NOTAKEFOCUS}
  SVSI_NOSTATECHANGE  = _SVSIF($80000000);
  {$EXTERNALSYM SVSI_NOSTATECHANGE}

type
  PSVGIO = ^SVGIO;
  _SVGIO = type Integer;
  SVGIO = _SVGIO;
  {$EXTERNALSYM _SVGIO}
  {$EXTERNALSYM SVGIO}
const
  SVGIO_BACKGROUND     = _SVGIO(0);
  {$EXTERNALSYM SVGIO_BACKGROUND}
  SVGIO_SELECTION      = _SVGIO($1);
  {$EXTERNALSYM SVGIO_SELECTION}
  SVGIO_ALLVIEW        = _SVGIO($2);
  {$EXTERNALSYM SVGIO_ALLVIEW}
  SVGIO_CHECKED        = _SVGIO($3);
  {$EXTERNALSYM SVGIO_CHECKED}
  SVGIO_TYPE_MASK      = _SVGIO($F);
  {$EXTERNALSYM SVGIO_TYPE_MASK}
  SVGIO_FLAG_VIEWORDER = _SVGIO($80000000);
  {$EXTERNALSYM SVGIO_FLAG_VIEWORDER}

type
  PSVUIA_STATUS = ^SVUIA_STATUS;
  SVUIA_STATUS = type Integer;
  {$EXTERNALSYM SVUIA_STATUS}
const
  SVUIA_DEACTIVATE       = SVUIA_STATUS(0);
  {$EXTERNALSYM SVUIA_DEACTIVATE}
  SVUIA_ACTIVATE_NOFOCUS = SVUIA_STATUS(1);
  {$EXTERNALSYM SVUIA_ACTIVATE_NOFOCUS}
  SVUIA_ACTIVATE_FOCUS   = SVUIA_STATUS(2);
  {$EXTERNALSYM SVUIA_ACTIVATE_FOCUS}
  SVUIA_INPLACEACTIVATE  = SVUIA_STATUS(3);
  {$EXTERNALSYM SVUIA_INPLACEACTIVATE}

//  this is so that all the implementations still get the function pointer.
//  and midl wont complain about the data type
type
  LPFNSVADDPROPSHEETPAGE = LPARAM;
  {$EXTERNALSYM LPFNSVADDPROPSHEETPAGE}

type
  SORTDIRECTION = type Integer;
  {$EXTERNALSYM SORTDIRECTION}
const
  SORT_DESCENDING = SORTDIRECTION(-1);
  {$EXTERNALSYM SORT_DESCENDING}
  SORT_ASCENDING  = SORTDIRECTION(1);
  {$EXTERNALSYM SORT_ASCENDING}

type
  PFVTEXTTYPE = ^FVTEXTTYPE;
  FVTEXTTYPE = type Integer;
  {$EXTERNALSYM FVTEXTTYPE}
const
  FVST_EMPTYTEXT = FVTEXTTYPE(0);
  {$EXTERNALSYM FVST_EMPTYTEXT}

type
  PCM_MASK = ^CM_MASK;
  CM_MASK = type Integer;
  {$EXTERNALSYM CM_MASK}
const
  CM_MASK_WIDTH        = CM_MASK($1);
  {$EXTERNALSYM CM_MASK_WIDTH}
  CM_MASK_DEFAULTWIDTH = CM_MASK($2);
  {$EXTERNALSYM CM_MASK_DEFAULTWIDTH}
  CM_MASK_IDEALWIDTH   = CM_MASK($4);
  {$EXTERNALSYM CM_MASK_IDEALWIDTH}
  CM_MASK_NAME         = CM_MASK($8);
  {$EXTERNALSYM CM_MASK_NAME}
  CM_MASK_STATE        = CM_MASK($10);
  {$EXTERNALSYM CM_MASK_STATE}

type
  PCM_STATE = ^CM_STATE;
  CM_STATE = type Integer;
  {$EXTERNALSYM CM_STATE}
const
  CM_STATE_NONE                 = CM_STATE(0);
  {$EXTERNALSYM CM_STATE_NONE}
  CM_STATE_VISIBLE              = CM_STATE($1);
  {$EXTERNALSYM CM_STATE_VISIBLE}
  CM_STATE_FIXEDWIDTH           = CM_STATE($2);
  {$EXTERNALSYM CM_STATE_FIXEDWIDTH}
  CM_STATE_NOSORTBYFOLDERNESS   = CM_STATE($4);
  {$EXTERNALSYM CM_STATE_NOSORTBYFOLDERNESS}
  CM_STATE_ALWAYSVISIBLE        = CM_STATE($8);
  {$EXTERNALSYM CM_STATE_ALWAYSVISIBLE}

type
  PCM_ENUM_FLAGS = ^CM_ENUM_FLAGS;
  CM_ENUM_FLAGS = type Integer;
  {$EXTERNALSYM CM_ENUM_FLAGS}
const
  CM_ENUM_ALL     = CM_ENUM_FLAGS($1);
  {$EXTERNALSYM CM_ENUM_ALL}
  CM_ENUM_VISIBLE = CM_ENUM_FLAGS($2);
  {$EXTERNALSYM CM_ENUM_VISIBLE}

type
  PCM_SET_WIDTH_VALUE = ^CM_SET_WIDTH_VALUE;
  CM_SET_WIDTH_VALUE = type Integer;
  {$EXTERNALSYM CM_SET_WIDTH_VALUE}
const
  CM_WIDTH_USEDEFAULT = CM_SET_WIDTH_VALUE(-1);
  {$EXTERNALSYM CM_WIDTH_USEDEFAULT}
  CM_WIDTH_AUTOSIZE   = CM_SET_WIDTH_VALUE(-2);
  {$EXTERNALSYM CM_WIDTH_AUTOSIZE}

type
  PSIGDN = ^SIGDN;
  SIGDN = type Integer;
  {$EXTERNALSYM SIGDN}
const
  SIGDN_NORMALDISPLAY               = SIGDN(0);
  {$EXTERNALSYM SIGDN_NORMALDISPLAY}
  SIGDN_PARENTRELATIVEPARSING       = SIGDN($80018001);
  {$EXTERNALSYM SIGDN_PARENTRELATIVEPARSING}
  SIGDN_DESKTOPABSOLUTEPARSING      = SIGDN($80028000);
  {$EXTERNALSYM SIGDN_DESKTOPABSOLUTEPARSING}
  SIGDN_PARENTRELATIVEEDITING       = SIGDN($80031001);
  {$EXTERNALSYM SIGDN_PARENTRELATIVEEDITING}
  SIGDN_DESKTOPABSOLUTEEDITING      = SIGDN($8004C000);
  {$EXTERNALSYM SIGDN_DESKTOPABSOLUTEEDITING}
  SIGDN_FILESYSPATH                 = SIGDN($80058000);
  {$EXTERNALSYM SIGDN_FILESYSPATH}
  SIGDN_URL                         = SIGDN($80068000);
  {$EXTERNALSYM SIGDN_URL}
  SIGDN_PARENTRELATIVEFORADDRESSBAR = SIGDN($8007C001);
  {$EXTERNALSYM SIGDN_PARENTRELATIVEFORADDRESSBAR}
  SIGDN_PARENTRELATIVE              = SIGDN($80080001);
  {$EXTERNALSYM SIGDN_PARENTRELATIVE}
  SIGDN_PARENTRELATIVEFORUI         = SIGDN($80094001);
  {$EXTERNALSYM SIGDN_PARENTRELATIVEFORUI}

type
  PSICHINTF = ^SICHINTF;
  _SICHINTF = type DWORD;
  SICHINTF = _SICHINTF;
  {$EXTERNALSYM _SICHINTF}
  {$EXTERNALSYM SICHINTF}
const
  SICHINT_DISPLAY                         = _SICHINTF(0);
  {$EXTERNALSYM SICHINT_DISPLAY}
  SICHINT_ALLFIELDS                       = _SICHINTF($80000000);
  {$EXTERNALSYM SICHINT_ALLFIELDS}
  SICHINT_CANONICAL                       = _SICHINTF($10000000);
  {$EXTERNALSYM SICHINT_CANONICAL}
  SICHINT_TEST_FILESYSPATH_IF_NOT_EQUAL   = _SICHINTF($20000000);
  {$EXTERNALSYM SICHINT_TEST_FILESYSPATH_IF_NOT_EQUAL}

type
  PDATAOBJ_GET_ITEM_FLAGS = ^DATAOBJ_GET_ITEM_FLAGS;
  DATAOBJ_GET_ITEM_FLAGS = type Integer;
  {$EXTERNALSYM DATAOBJ_GET_ITEM_FLAGS}
const
  DOGIF_DEFAULT       = DATAOBJ_GET_ITEM_FLAGS(0);
  {$EXTERNALSYM DOGIF_DEFAULT}
  DOGIF_TRAVERSE_LINK = DATAOBJ_GET_ITEM_FLAGS($1);
  {$EXTERNALSYM DOGIF_TRAVERSE_LINK}
  DOGIF_NO_HDROP      = DATAOBJ_GET_ITEM_FLAGS($2);
  {$EXTERNALSYM DOGIF_NO_HDROP}
  DOGIF_NO_URL        = DATAOBJ_GET_ITEM_FLAGS($4);
  {$EXTERNALSYM DOGIF_NO_URL}
  DOGIF_ONLY_IF_ONE   = DATAOBJ_GET_ITEM_FLAGS($8);
  {$EXTERNALSYM DOGIF_ONLY_IF_ONE}

type
  PSIIGBF = ^SIIGBF;
  _SIIGBF = type Integer;
  SIIGBF = _SIIGBF;
  {$EXTERNALSYM _SIIGBF}
  {$EXTERNALSYM SIIGBF}
const
  SIIGBF_RESIZETOFIT   = _SIIGBF(0);
  {$EXTERNALSYM SIIGBF_RESIZETOFIT}
  SIIGBF_BIGGERSIZEOK  = _SIIGBF($1);
  {$EXTERNALSYM SIIGBF_BIGGERSIZEOK}
  SIIGBF_MEMORYONLY    = _SIIGBF($2);
  {$EXTERNALSYM SIIGBF_MEMORYONLY}
  SIIGBF_ICONONLY      = _SIIGBF($4);
  {$EXTERNALSYM SIIGBF_ICONONLY}
  SIIGBF_THUMBNAILONLY = _SIIGBF($8);
  {$EXTERNALSYM SIIGBF_THUMBNAILONLY}
  SIIGBF_INCACHEONLY   = _SIIGBF($10);
  {$EXTERNALSYM SIIGBF_INCACHEONLY}
  SIIGBF_CROPTOSQUARE  = _SIIGBF($20);
  {$EXTERNALSYM SIIGBF_CROPTOSQUARE}
  SIIGBF_WIDETHUMBNAILS = _SIIGBF($40);
  {$EXTERNALSYM SIIGBF_WIDETHUMBNAILS}
  SIIGBF_ICONBACKGROUND = _SIIGBF($80);
  {$EXTERNALSYM SIIGBF_ICONBACKGROUND}
  SIIGBF_SCALEUP       = _SIIGBF($100);
  {$EXTERNALSYM SIIGBF_SCALEUP}

type
  PSTGOP = ^STGOP;
  STGOP = type Integer;
  {$EXTERNALSYM STGOP}
const
  STGOP_MOVE            = STGOP(1);
  {$EXTERNALSYM STGOP_MOVE}
  STGOP_COPY            = STGOP(2);
  {$EXTERNALSYM STGOP_COPY}
  STGOP_SYNC            = STGOP(3);
  {$EXTERNALSYM STGOP_SYNC}
  STGOP_REMOVE          = STGOP(5);
  {$EXTERNALSYM STGOP_REMOVE}
  STGOP_RENAME          = STGOP(6);
  {$EXTERNALSYM STGOP_RENAME}
  STGOP_APPLYPROPERTIES = STGOP(8);
  {$EXTERNALSYM STGOP_APPLYPROPERTIES}
  STGOP_NEW             = STGOP(10);
  {$EXTERNALSYM STGOP_NEW}

type
  PTRANSFER_SOURCE_FLAGS = ^TRANSFER_SOURCE_FLAGS;
  _TRANSFER_SOURCE_FLAGS = type DWORD;
  TRANSFER_SOURCE_FLAGS = _TRANSFER_SOURCE_FLAGS;
  {$EXTERNALSYM _TRANSFER_SOURCE_FLAGS}
  {$EXTERNALSYM TRANSFER_SOURCE_FLAGS}
const
  TSF_NORMAL                     = _TRANSFER_SOURCE_FLAGS(0);
  {$EXTERNALSYM TSF_NORMAL}
  TSF_FAIL_EXIST                 = _TRANSFER_SOURCE_FLAGS(0);
  {$EXTERNALSYM TSF_FAIL_EXIST}
  TSF_RENAME_EXIST               = _TRANSFER_SOURCE_FLAGS($1);
  {$EXTERNALSYM TSF_RENAME_EXIST}
  TSF_OVERWRITE_EXIST            = _TRANSFER_SOURCE_FLAGS($2);
  {$EXTERNALSYM TSF_OVERWRITE_EXIST}
  TSF_ALLOW_DECRYPTION           = _TRANSFER_SOURCE_FLAGS($4);
  {$EXTERNALSYM TSF_ALLOW_DECRYPTION}
  TSF_NO_SECURITY                = _TRANSFER_SOURCE_FLAGS($8);
  {$EXTERNALSYM TSF_NO_SECURITY}
  TSF_COPY_CREATION_TIME         = _TRANSFER_SOURCE_FLAGS($10);
  {$EXTERNALSYM TSF_COPY_CREATION_TIME}
  TSF_COPY_WRITE_TIME            = _TRANSFER_SOURCE_FLAGS($20);
  {$EXTERNALSYM TSF_COPY_WRITE_TIME}
  TSF_USE_FULL_ACCESS            = _TRANSFER_SOURCE_FLAGS($40);
  {$EXTERNALSYM TSF_USE_FULL_ACCESS}
  TSF_DELETE_RECYCLE_IF_POSSIBLE = _TRANSFER_SOURCE_FLAGS($80);
  {$EXTERNALSYM TSF_DELETE_RECYCLE_IF_POSSIBLE}
  TSF_COPY_HARD_LINK             = _TRANSFER_SOURCE_FLAGS($100);
  {$EXTERNALSYM TSF_COPY_HARD_LINK}
  TSF_COPY_LOCALIZED_NAME        = _TRANSFER_SOURCE_FLAGS($200);
  {$EXTERNALSYM TSF_COPY_LOCALIZED_NAME}
  TSF_MOVE_AS_COPY_DELETE        = _TRANSFER_SOURCE_FLAGS($400);
  {$EXTERNALSYM TSF_MOVE_AS_COPY_DELETE}
  TSF_SUSPEND_SHELLEVENTS        = _TRANSFER_SOURCE_FLAGS($800);
  {$EXTERNALSYM TSF_SUSPEND_SHELLEVENTS}

type
  PTRANSFER_ADVISE_STATE = ^TRANSFER_ADVISE_STATE;
  _TRANSFER_ADVISE_STATE = type DWORD;
  TRANSFER_ADVISE_STATE = _TRANSFER_ADVISE_STATE;
  {$EXTERNALSYM _TRANSFER_ADVISE_STATE}
  {$EXTERNALSYM TRANSFER_ADVISE_STATE}
const
  TS_NONE          = _TRANSFER_ADVISE_STATE(0);
  {$EXTERNALSYM TS_NONE}
  TS_PERFORMING    = _TRANSFER_ADVISE_STATE($1);
  {$EXTERNALSYM TS_PERFORMING}
  TS_PREPARING     = _TRANSFER_ADVISE_STATE($2);
  {$EXTERNALSYM TS_PREPARING}
  TS_INDETERMINATE = _TRANSFER_ADVISE_STATE($4);
  {$EXTERNALSYM TS_INDETERMINATE}

type
  PSIATTRIBFLAGS = ^SIATTRIBFLAGS;
  SIATTRIBFLAGS = type Integer;
  {$EXTERNALSYM SIATTRIBFLAGS}
const
  SIATTRIBFLAGS_AND       = SIATTRIBFLAGS($1);
  {$EXTERNALSYM SIATTRIBFLAGS_AND}
  SIATTRIBFLAGS_OR        = SIATTRIBFLAGS($2);
  {$EXTERNALSYM SIATTRIBFLAGS_OR}
  SIATTRIBFLAGS_APPCOMPAT = SIATTRIBFLAGS($3);
  {$EXTERNALSYM SIATTRIBFLAGS_APPCOMPAT}
  SIATTRIBFLAGS_MASK      = SIATTRIBFLAGS($3);
  {$EXTERNALSYM SIATTRIBFLAGS_MASK}
  SIATTRIBFLAGS_ALLITEMS  = SIATTRIBFLAGS($4000);
  {$EXTERNALSYM SIATTRIBFLAGS_ALLITEMS}

type
  PPROPERTYUI_NAME_FLAGS = ^PROPERTYUI_NAME_FLAGS;
  _PROPERTYUI_NAME_FLAGS = type DWORD;
  PROPERTYUI_NAME_FLAGS = _PROPERTYUI_NAME_FLAGS;
  {$EXTERNALSYM _PROPERTYUI_NAME_FLAGS}
  {$EXTERNALSYM PROPERTYUI_NAME_FLAGS}
const
  PUIFNF_DEFAULT  = _PROPERTYUI_NAME_FLAGS(0);
  {$EXTERNALSYM PUIFNF_DEFAULT}
  PUIFNF_MNEMONIC = _PROPERTYUI_NAME_FLAGS($1);
  {$EXTERNALSYM PUIFNF_MNEMONIC}

type
  PPROPERTYUI_FLAGS = ^PROPERTYUI_FLAGS;
  _PROPERTYUI_FLAGS = type DWORD;
  PROPERTYUI_FLAGS = _PROPERTYUI_FLAGS;
  {$EXTERNALSYM _PROPERTYUI_FLAGS}
  {$EXTERNALSYM PROPERTYUI_FLAGS}
const
  PUIF_DEFAULT          = _PROPERTYUI_FLAGS(0);
  {$EXTERNALSYM PUIF_DEFAULT}
  PUIF_RIGHTALIGN       = _PROPERTYUI_FLAGS($1);
  {$EXTERNALSYM PUIF_RIGHTALIGN}
  PUIF_NOLABELININFOTIP = _PROPERTYUI_FLAGS($2);
  {$EXTERNALSYM PUIF_NOLABELININFOTIP}

type
  PPROPERTYUI_FORMAT_FLAGS = ^PROPERTYUI_FORMAT_FLAGS;
  _PROPERTYUI_FORMAT_FLAGS = type DWORD;
  PROPERTYUI_FORMAT_FLAGS = _PROPERTYUI_FORMAT_FLAGS;
  {$EXTERNALSYM _PROPERTYUI_FORMAT_FLAGS}
  {$EXTERNALSYM PROPERTYUI_FORMAT_FLAGS}
const
  PUIFFDF_DEFAULT      = _PROPERTYUI_FORMAT_FLAGS(0);
  {$EXTERNALSYM PUIFFDF_DEFAULT}
  PUIFFDF_RIGHTTOLEFT  = _PROPERTYUI_FORMAT_FLAGS($1);
  {$EXTERNALSYM PUIFFDF_RIGHTTOLEFT}
  PUIFFDF_SHORTFORMAT  = _PROPERTYUI_FORMAT_FLAGS($2);
  {$EXTERNALSYM PUIFFDF_SHORTFORMAT}
  PUIFFDF_NOTIME       = _PROPERTYUI_FORMAT_FLAGS($4);
  {$EXTERNALSYM PUIFFDF_NOTIME}
  PUIFFDF_FRIENDLYDATE = _PROPERTYUI_FORMAT_FLAGS($8);
  {$EXTERNALSYM PUIFFDF_FRIENDLYDATE}

type
  PCATEGORYINFO_FLAGS = ^CATEGORYINFO_FLAGS;
  CATEGORYINFO_FLAGS = type Integer;
  {$EXTERNALSYM CATEGORYINFO_FLAGS}
const
  CATINFO_NORMAL          = CATEGORYINFO_FLAGS(0);
  {$EXTERNALSYM CATINFO_NORMAL}
  CATINFO_COLLAPSED       = CATEGORYINFO_FLAGS($1);
  {$EXTERNALSYM CATINFO_COLLAPSED}
  CATINFO_HIDDEN          = CATEGORYINFO_FLAGS($2);
  {$EXTERNALSYM CATINFO_HIDDEN}
  CATINFO_EXPANDED        = CATEGORYINFO_FLAGS($4);
  {$EXTERNALSYM CATINFO_EXPANDED}
  CATINFO_NOHEADER        = CATEGORYINFO_FLAGS($8);
  {$EXTERNALSYM CATINFO_NOHEADER}
  CATINFO_NOTCOLLAPSIBLE  = CATEGORYINFO_FLAGS($10);
  {$EXTERNALSYM CATINFO_NOTCOLLAPSIBLE}
  CATINFO_NOHEADERCOUNT   = CATEGORYINFO_FLAGS($20);
  {$EXTERNALSYM CATINFO_NOHEADERCOUNT}
  CATINFO_SUBSETTED       = CATEGORYINFO_FLAGS($40);
  {$EXTERNALSYM CATINFO_SUBSETTED}
  CATINFO_SEPARATE_IMAGES = CATEGORYINFO_FLAGS($80);
  {$EXTERNALSYM CATINFO_SEPARATE_IMAGES}
  CATINFO_SHOWEMPTY       = CATEGORYINFO_FLAGS($100);
  {$EXTERNALSYM CATINFO_SHOWEMPTY}

type
  PCATSORT_FLAGS = ^CATSORT_FLAGS;
  CATSORT_FLAGS = type Integer;
  {$EXTERNALSYM CATSORT_FLAGS}
const
  CATSORT_DEFAULT = CATSORT_FLAGS(0);
  {$EXTERNALSYM CATSORT_DEFAULT}
  CATSORT_NAME    = CATSORT_FLAGS($1);
  {$EXTERNALSYM CATSORT_NAME}

type
  PSLR_FLAGS = ^SLR_FLAGS;
  SLR_FLAGS = type Integer;
  {$EXTERNALSYM SLR_FLAGS}
const
  SLR_NONE                       = SLR_FLAGS(0);
  {$EXTERNALSYM SLR_NONE}
  SLR_NO_UI                      = SLR_FLAGS($1);
  {$EXTERNALSYM SLR_NO_UI}
  SLR_ANY_MATCH                  = SLR_FLAGS($2);
  {$EXTERNALSYM SLR_ANY_MATCH}
  SLR_UPDATE                     = SLR_FLAGS($4);
  {$EXTERNALSYM SLR_UPDATE}
  SLR_NOUPDATE                   = SLR_FLAGS($8);
  {$EXTERNALSYM SLR_NOUPDATE}
  SLR_NOSEARCH                   = SLR_FLAGS($10);
  {$EXTERNALSYM SLR_NOSEARCH}
  SLR_NOTRACK                    = SLR_FLAGS($20);
  {$EXTERNALSYM SLR_NOTRACK}
  SLR_NOLINKINFO                 = SLR_FLAGS($40);
  {$EXTERNALSYM SLR_NOLINKINFO}
  SLR_INVOKE_MSI                 = SLR_FLAGS($80);
  {$EXTERNALSYM SLR_INVOKE_MSI}
  SLR_NO_UI_WITH_MSG_PUMP        = SLR_FLAGS($101);
  {$EXTERNALSYM SLR_NO_UI_WITH_MSG_PUMP}
  SLR_OFFER_DELETE_WITHOUT_FILE  = SLR_FLAGS($200);
  {$EXTERNALSYM SLR_OFFER_DELETE_WITHOUT_FILE}
  SLR_KNOWNFOLDER                = SLR_FLAGS($400);
  {$EXTERNALSYM SLR_KNOWNFOLDER}
  SLR_MACHINE_IN_LOCAL_TARGET    = SLR_FLAGS($800);
  {$EXTERNALSYM SLR_MACHINE_IN_LOCAL_TARGET}
  SLR_UPDATE_MACHINE_AND_SID     = SLR_FLAGS($1000);
  {$EXTERNALSYM SLR_UPDATE_MACHINE_AND_SID}
  SLR_NO_OBJECT_ID               = SLR_FLAGS($2000);
  {$EXTERNALSYM SLR_NO_OBJECT_ID}

type
  PSLGP_FLAGS = ^SLGP_FLAGS;
  SLGP_FLAGS = type Integer;
  {$EXTERNALSYM SLGP_FLAGS}
const
  SLGP_SHORTPATH        = SLGP_FLAGS($1);
  {$EXTERNALSYM SLGP_SHORTPATH}
  SLGP_UNCPRIORITY      = SLGP_FLAGS($2);
  {$EXTERNALSYM SLGP_UNCPRIORITY}
  SLGP_RAWPATH          = SLGP_FLAGS($4);
  {$EXTERNALSYM SLGP_RAWPATH}
  SLGP_RELATIVEPRIORITY = SLGP_FLAGS($8);
  {$EXTERNALSYM SLGP_RELATIVEPRIORITY}

type
  PSPINITF = ^SPINITF;
  _SPINITF = type DWORD;
  SPINITF = _SPINITF;
  {$EXTERNALSYM _SPINITF}
  {$EXTERNALSYM SPINITF}
const
  SPINITF_NORMAL     = _SPINITF(0);
  {$EXTERNALSYM SPINITF_NORMAL}
  SPINITF_MODAL      = _SPINITF($1);
  {$EXTERNALSYM SPINITF_MODAL}
  SPINITF_NOMINIMIZE = _SPINITF($8);
  {$EXTERNALSYM SPINITF_NOMINIMIZE}

type
  PSPBEGINF = ^SPBEGINF;
  _SPBEGINF = type DWORD;
  SPBEGINF = _SPBEGINF;
  {$EXTERNALSYM _SPBEGINF}
  {$EXTERNALSYM SPBEGINF}
const
  SPBEGINF_NORMAL          = _SPBEGINF(0);
  {$EXTERNALSYM SPBEGINF_NORMAL}
  SPBEGINF_AUTOTIME        = _SPBEGINF($2);
  {$EXTERNALSYM SPBEGINF_AUTOTIME}
  SPBEGINF_NOPROGRESSBAR   = _SPBEGINF($10);
  {$EXTERNALSYM SPBEGINF_NOPROGRESSBAR}
  SPBEGINF_MARQUEEPROGRESS = _SPBEGINF($20);
  {$EXTERNALSYM SPBEGINF_MARQUEEPROGRESS}
  SPBEGINF_NOCANCELBUTTON  = _SPBEGINF($40);
  {$EXTERNALSYM SPBEGINF_NOCANCELBUTTON}

type
  PSPACTION = ^SPACTION;
  _SPACTION = type Integer;
  {$EXTERNALSYM SPACTION}
  SPACTION = _SPACTION;
const
  SPACTION_NONE               = SPACTION(0);
  {$EXTERNALSYM SPACTION_NONE}
  SPACTION_MOVING             = SPACTION(SPACTION_NONE + 1);
  {$EXTERNALSYM SPACTION_MOVING}
  SPACTION_COPYING            = SPACTION(SPACTION_MOVING + 1);
  {$EXTERNALSYM SPACTION_COPYING}
  SPACTION_RECYCLING          = SPACTION(SPACTION_COPYING + 1);
  {$EXTERNALSYM SPACTION_RECYCLING}
  SPACTION_APPLYINGATTRIBS    = SPACTION(SPACTION_RECYCLING + 1);
  {$EXTERNALSYM SPACTION_APPLYINGATTRIBS}
  SPACTION_DOWNLOADING        = SPACTION(SPACTION_APPLYINGATTRIBS + 1);
  {$EXTERNALSYM SPACTION_DOWNLOADING}
  SPACTION_SEARCHING_INTERNET = SPACTION(SPACTION_DOWNLOADING + 1);
  {$EXTERNALSYM SPACTION_SEARCHING_INTERNET}
  SPACTION_CALCULATING        = SPACTION(SPACTION_SEARCHING_INTERNET + 1);
  {$EXTERNALSYM SPACTION_CALCULATING}
  SPACTION_UPLOADING          = SPACTION(SPACTION_CALCULATING + 1);
  {$EXTERNALSYM SPACTION_UPLOADING}
  SPACTION_SEARCHING_FILES    = SPACTION(SPACTION_UPLOADING + 1);
  {$EXTERNALSYM SPACTION_SEARCHING_FILES}
  SPACTION_DELETING           = SPACTION(SPACTION_SEARCHING_FILES + 1);
  {$EXTERNALSYM SPACTION_DELETING}
  SPACTION_RENAMING           = SPACTION(SPACTION_DELETING + 1);
  {$EXTERNALSYM SPACTION_RENAMING}
  SPACTION_FORMATTING         = SPACTION(SPACTION_RENAMING + 1);
  {$EXTERNALSYM SPACTION_FORMATTING}
  SPACTION_COPY_MOVING        = SPACTION(SPACTION_FORMATTING + 1);
  {$EXTERNALSYM SPACTION_COPY_MOVING}

type
  PSPTEXT = ^SPTEXT;
  SPTEXT = type Integer;
  {$EXTERNALSYM SPTEXT}
const
  SPTEXT_ACTIONDESCRIPTION = SPTEXT(1);
  {$EXTERNALSYM SPTEXT_ACTIONDESCRIPTION}
  SPTEXT_ACTIONDETAIL      = SPTEXT(SPTEXT_ACTIONDESCRIPTION + 1);
  {$EXTERNALSYM SPTEXT_ACTIONDETAIL}

type
  PEXPPS = ^EXPPS;
  EXPPS = UINT;
  {$EXTERNALSYM EXPPS}
const
  EXPPS_FILETYPES = EXPPS($1);
  {$EXTERNALSYM EXPPS_FILETYPES}

type
  PTBPFLAG = ^TBPFLAG;
  TBPFLAG = type DWORD;
  {$EXTERNALSYM TBPFLAG}
const
  TBPF_NOPROGRESS    = TBPFLAG(0);
  {$EXTERNALSYM TBPF_NOPROGRESS}
  TBPF_INDETERMINATE = TBPFLAG($1);
  {$EXTERNALSYM TBPF_INDETERMINATE}
  TBPF_NORMAL        = TBPFLAG($2);
  {$EXTERNALSYM TBPF_NORMAL}
  TBPF_ERROR         = TBPFLAG($4);
  {$EXTERNALSYM TBPF_ERROR}
  TBPF_PAUSED        = TBPFLAG($8);
  {$EXTERNALSYM TBPF_PAUSED}

type
  POPPROGDLGF = ^OPPROGDLGF;
  _OPPROGDLGF = type DWORD;
  OPPROGDLGF = _OPPROGDLGF;
  {$EXTERNALSYM _OPPROGDLGF}
  {$EXTERNALSYM OPPROGDLGF}
const
  OPPROGDLG_DEFAULT                 = _OPPROGDLGF(0);
  {$EXTERNALSYM OPPROGDLG_DEFAULT}
  OPPROGDLG_ENABLEPAUSE             = _OPPROGDLGF($80);
  {$EXTERNALSYM OPPROGDLG_ENABLEPAUSE}
  OPPROGDLG_ALLOWUNDO               = _OPPROGDLGF($100);
  {$EXTERNALSYM OPPROGDLG_ALLOWUNDO}
  OPPROGDLG_DONTDISPLAYSOURCEPATH   = _OPPROGDLGF($200);
  {$EXTERNALSYM OPPROGDLG_DONTDISPLAYSOURCEPATH}
  OPPROGDLG_DONTDISPLAYDESTPATH     = _OPPROGDLGF($400);
  {$EXTERNALSYM OPPROGDLG_DONTDISPLAYDESTPATH}
  OPPROGDLG_NOMULTIDAYESTIMATES     = _OPPROGDLGF($800);
  {$EXTERNALSYM OPPROGDLG_NOMULTIDAYESTIMATES}
  OPPROGDLG_DONTDISPLAYLOCATIONS    = _OPPROGDLGF($1000);
  {$EXTERNALSYM OPPROGDLG_DONTDISPLAYLOCATIONS}

type
  PPDMODE = ^PDMODE;
  _PDMODE = type DWORD;
  PDMODE = _PDMODE;
  {$EXTERNALSYM _PDMODE}
  {$EXTERNALSYM PDMODE}
const
  PDM_DEFAULT        = _PDMODE(0);
  {$EXTERNALSYM PDM_DEFAULT}
  PDM_RUN            = _PDMODE($1);
  {$EXTERNALSYM PDM_RUN}
  PDM_PREFLIGHT      = _PDMODE($2);
  {$EXTERNALSYM PDM_PREFLIGHT}
  PDM_UNDOING        = _PDMODE($4);
  {$EXTERNALSYM PDM_UNDOING}
  PDM_ERRORSBLOCKING = _PDMODE($8);
  {$EXTERNALSYM PDM_ERRORSBLOCKING}
  PDM_INDETERMINATE  = _PDMODE($10);
  {$EXTERNALSYM PDM_INDETERMINATE}

type
  PPDOPSTATUS = ^PDOPSTATUS;
  PDOPSTATUS = type Integer;
  {$EXTERNALSYM PDOPSTATUS}
const
  PDOPS_RUNNING   = PDOPSTATUS(1);
  {$EXTERNALSYM PDOPS_RUNNING}
  PDOPS_PAUSED    = PDOPSTATUS(2);
  {$EXTERNALSYM PDOPS_PAUSED}
  PDOPS_CANCELLED = PDOPSTATUS(3);
  {$EXTERNALSYM PDOPS_CANCELLED}
  PDOPS_STOPPED   = PDOPSTATUS(4);
  {$EXTERNALSYM PDOPS_STOPPED}
  PDOPS_ERRORS    = PDOPSTATUS(5);
  {$EXTERNALSYM PDOPS_ERRORS}

type
  PFILE_OPERATION_FLAGS2 = ^FILE_OPERATION_FLAGS2;
  FILE_OPERATION_FLAGS2 = type Integer;
  {$EXTERNALSYM FILE_OPERATION_FLAGS2}
const
  FOF2_NONE                    = FILE_OPERATION_FLAGS2(0);
  {$EXTERNALSYM FOF2_NONE}
  FOF2_MERGEFOLDERSONCOLLISION = FILE_OPERATION_FLAGS2($1);
  {$EXTERNALSYM FOF2_MERGEFOLDERSONCOLLISION}

type
  PNAMESPACEWALKFLAG = ^NAMESPACEWALKFLAG;
  NAMESPACEWALKFLAG = type Integer;
  {$EXTERNALSYM NAMESPACEWALKFLAG}
  NSWF = NAMESPACEWALKFLAG;
  {$EXTERNALSYM NSWF}
const
  NSWF_DEFAULT                         = NAMESPACEWALKFLAG(0);
  {$EXTERNALSYM NSWF_DEFAULT}
  NSWF_NONE_IMPLIES_ALL                = NAMESPACEWALKFLAG($1);
  {$EXTERNALSYM NSWF_NONE_IMPLIES_ALL}
  NSWF_ONE_IMPLIES_ALL                 = NAMESPACEWALKFLAG($2);
  {$EXTERNALSYM NSWF_ONE_IMPLIES_ALL}
  NSWF_DONT_TRAVERSE_LINKS             = NAMESPACEWALKFLAG($4);
  {$EXTERNALSYM NSWF_DONT_TRAVERSE_LINKS}
  NSWF_DONT_ACCUMULATE_RESULT          = NAMESPACEWALKFLAG($8);
  {$EXTERNALSYM NSWF_DONT_ACCUMULATE_RESULT}
  NSWF_TRAVERSE_STREAM_JUNCTIONS       = NAMESPACEWALKFLAG($10);
  {$EXTERNALSYM NSWF_TRAVERSE_STREAM_JUNCTIONS}
  NSWF_FILESYSTEM_ONLY                 = NAMESPACEWALKFLAG($20);
  {$EXTERNALSYM NSWF_FILESYSTEM_ONLY}
  NSWF_SHOW_PROGRESS                   = NAMESPACEWALKFLAG($40);
  {$EXTERNALSYM NSWF_SHOW_PROGRESS}
  NSWF_FLAG_VIEWORDER                  = NAMESPACEWALKFLAG($80);
  {$EXTERNALSYM NSWF_FLAG_VIEWORDER}
  NSWF_IGNORE_AUTOPLAY_HIDA            = NAMESPACEWALKFLAG($100);
  {$EXTERNALSYM NSWF_IGNORE_AUTOPLAY_HIDA}
  NSWF_ASYNC                           = NAMESPACEWALKFLAG($200);
  {$EXTERNALSYM NSWF_ASYNC}
  NSWF_DONT_RESOLVE_LINKS              = NAMESPACEWALKFLAG($400);
  {$EXTERNALSYM NSWF_DONT_RESOLVE_LINKS}
  NSWF_ACCUMULATE_FOLDERS              = NAMESPACEWALKFLAG($800);
  {$EXTERNALSYM NSWF_ACCUMULATE_FOLDERS}
  NSWF_DONT_SORT                       = NAMESPACEWALKFLAG($1000);
  {$EXTERNALSYM NSWF_DONT_SORT}
  NSWF_USE_TRANSFER_MEDIUM             = NAMESPACEWALKFLAG($2000);
  {$EXTERNALSYM NSWF_USE_TRANSFER_MEDIUM}
  NSWF_DONT_TRAVERSE_STREAM_JUNCTIONS  = NAMESPACEWALKFLAG($4000);
  {$EXTERNALSYM NSWF_DONT_TRAVERSE_STREAM_JUNCTIONS}
  NSWF_ANY_IMPLIES_ALL                 = NAMESPACEWALKFLAG($8000);
  {$EXTERNALSYM NSWF_ANY_IMPLIES_ALL}

type
  PBANDSITECID = ^tagBANDSITECID;
  tagBANDSITECID = type DWORD;
  {$EXTERNALSYM tagBANDSITECID}
  BANDSITECID = tagBANDSITECID;
  {$EXTERNALSYM BANDSITECID}
const
  BSID_BANDADDED    = tagBANDSITECID(0);
  BSID_BANDREMOVED	= tagBANDSITECID((BSID_BANDADDED + 1));

type
  PFILE_USAGE_TYPE = ^FILE_USAGE_TYPE;
  FILE_USAGE_TYPE = type Integer;
  {$EXTERNALSYM FILE_USAGE_TYPE}
const
  FUT_PLAYING = FILE_USAGE_TYPE(0);
  {$EXTERNALSYM FUT_PLAYING}
  FUT_EDITING = FILE_USAGE_TYPE(FUT_PLAYING + 1);
  {$EXTERNALSYM FUT_EDITING}
  FUT_GENERIC = FILE_USAGE_TYPE(FUT_EDITING + 1);
  {$EXTERNALSYM FUT_GENERIC}

type
  PFILEOPENDIALOGOPTIONS = ^_FILEOPENDIALOGOPTIONS;
  _FILEOPENDIALOGOPTIONS = type DWORD;
  FILEOPENDIALOGOPTIONS = _FILEOPENDIALOGOPTIONS;
  {$EXTERNALSYM _FILEOPENDIALOGOPTIONS}
const
  {$EXTERNALSYM FILEOPENDIALOGOPTIONS}
  FOS_OVERWRITEPROMPT          = _FILEOPENDIALOGOPTIONS($00000002); // (on by default in the save dialog)
  {$EXTERNALSYM FOS_OVERWRITEPROMPT}
  FOS_STRICTFILETYPES          = _FILEOPENDIALOGOPTIONS($00000004); // In the save dialog, only allow the user to choose a file that has
  {$EXTERNALSYM FOS_STRICTFILETYPES}
                                                                    // one of the file extensions provided in SetFileTypes.
  FOS_NOCHANGEDIR              = _FILEOPENDIALOGOPTIONS($00000008); // Don't change the current working directory
  {$EXTERNALSYM FOS_NOCHANGEDIR}
  FOS_PICKFOLDERS              = _FILEOPENDIALOGOPTIONS($00000020); // Invoke the open dialog in folder picking mode.
  {$EXTERNALSYM FOS_PICKFOLDERS}
  FOS_FORCEFILESYSTEM          = _FILEOPENDIALOGOPTIONS($00000040); // Ensure that items returned are filesystem items.
  {$EXTERNALSYM FOS_FORCEFILESYSTEM}
  FOS_ALLNONSTORAGEITEMS       = _FILEOPENDIALOGOPTIONS($00000080); // Allow choosing items that have no storage.
  {$EXTERNALSYM FOS_ALLNONSTORAGEITEMS}
  FOS_NOVALIDATE               = _FILEOPENDIALOGOPTIONS($00000100);
  {$EXTERNALSYM FOS_NOVALIDATE}
  FOS_ALLOWMULTISELECT         = _FILEOPENDIALOGOPTIONS($00000200);
  {$EXTERNALSYM FOS_ALLOWMULTISELECT}
  FOS_PATHMUSTEXIST            = _FILEOPENDIALOGOPTIONS($00000800); // (on by default)
  {$EXTERNALSYM FOS_PATHMUSTEXIST}
  FOS_FILEMUSTEXIST            = _FILEOPENDIALOGOPTIONS($00001000); // (on by default in the open dialog and folder picker)
  {$EXTERNALSYM FOS_FILEMUSTEXIST}
  FOS_CREATEPROMPT             = _FILEOPENDIALOGOPTIONS($00002000);
  {$EXTERNALSYM FOS_CREATEPROMPT}
  FOS_SHAREAWARE               = _FILEOPENDIALOGOPTIONS($00004000);
  {$EXTERNALSYM FOS_SHAREAWARE}
  FOS_NOREADONLYRETURN         = _FILEOPENDIALOGOPTIONS($00008000); // (on by default in the save dialog)
  {$EXTERNALSYM FOS_NOREADONLYRETURN}
  FOS_NOTESTFILECREATE         = _FILEOPENDIALOGOPTIONS($00010000); // Avoid testing the creation of the chosen file in the save dialog
  {$EXTERNALSYM FOS_NOTESTFILECREATE}
                                                                    // (specifying this flag will circumvent some useful error handling, such as access denied)
  FOS_HIDEMRUPLACES            = _FILEOPENDIALOGOPTIONS($00020000); // (not used in Win7)
  {$EXTERNALSYM FOS_HIDEMRUPLACES}
  FOS_HIDEPINNEDPLACES         = _FILEOPENDIALOGOPTIONS($00040000); // Don't display the standard namespace locations in the navigation pane.
  {$EXTERNALSYM FOS_HIDEPINNEDPLACES}
                                                                    // (generally used along with AddPlace)
  FOS_NODEREFERENCELINKS       = _FILEOPENDIALOGOPTIONS($00100000); // Don't treat shortcuts as their target files.
  {$EXTERNALSYM FOS_NODEREFERENCELINKS}
  FOS_OKBUTTONNEEDSINTERACTION = _FILEOPENDIALOGOPTIONS($00200000); // Only enable the OK button if the user has done something in the view.
  {$EXTERNALSYM FOS_OKBUTTONNEEDSINTERACTION}
  FOS_DONTADDTORECENT          = _FILEOPENDIALOGOPTIONS($02000000); // Don't add the chosen file to the recent documents list (SHAddToRecentDocs)
  {$EXTERNALSYM FOS_DONTADDTORECENT}
  FOS_FORCESHOWHIDDEN          = _FILEOPENDIALOGOPTIONS($10000000); // Show all files including system and hidden files.
  {$EXTERNALSYM FOS_FORCESHOWHIDDEN}
  FOS_DEFAULTNOMINIMODE        = _FILEOPENDIALOGOPTIONS($20000000); // (not used in Win7)
  {$EXTERNALSYM FOS_DEFAULTNOMINIMODE}
  FOS_FORCEPREVIEWPANEON       = _FILEOPENDIALOGOPTIONS($40000000);
  {$EXTERNALSYM FOS_FORCEPREVIEWPANEON}
  FOS_SUPPORTSTREAMABLEITEMS   = _FILEOPENDIALOGOPTIONS($80000000); // Indicates the caller will use BHID_Stream to open contents, no need to download the file
  {$EXTERNALSYM FOS_SUPPORTSTREAMABLEITEMS}

type
  PASSOCIATIONLEVEL = ^ASSOCIATIONLEVEL;
  ASSOCIATIONLEVEL = type DWORD;
  {$EXTERNALSYM ASSOCIATIONLEVEL}
const
  AL_MACHINE   = ASSOCIATIONLEVEL(0);
  {$EXTERNALSYM AL_MACHINE}
  AL_EFFECTIVE = ASSOCIATIONLEVEL(1);
  {$EXTERNALSYM AL_EFFECTIVE}
  AL_USER      = ASSOCIATIONLEVEL(2);
  {$EXTERNALSYM AL_USER}

type
  PASSOCIATIONTYPE = ^ASSOCIATIONTYPE;
  ASSOCIATIONTYPE = type DWORD;
  {$EXTERNALSYM ASSOCIATIONTYPE}
const
  AT_FILEEXTENSION   = ASSOCIATIONTYPE(0);
  {$EXTERNALSYM AT_FILEEXTENSION}
  AT_URLPROTOCOL     = ASSOCIATIONTYPE(1);
  {$EXTERNALSYM AT_URLPROTOCOL}
  AT_STARTMENUCLIENT = ASSOCIATIONTYPE(2);
  {$EXTERNALSYM AT_STARTMENUCLIENT}
  AT_MIMETYPE        = ASSOCIATIONTYPE(3);
  {$EXTERNALSYM AT_MIMETYPE}

type
  PCDCONTROLSTATEF = ^CDCONTROLSTATEF;
  CDCONTROLSTATEF = type DWORD;
  {$EXTERNALSYM CDCONTROLSTATEF}
const
  CDCS_INACTIVE       = CDCONTROLSTATEF($00000000);
  {$EXTERNALSYM CDCS_INACTIVE}
  CDCS_ENABLED        = CDCONTROLSTATEF($00000001);
  {$EXTERNALSYM CDCS_ENABLED}
  CDCS_VISIBLE        = CDCONTROLSTATEF($00000002);
  {$EXTERNALSYM CDCS_VISIBLE}
  CDCS_ENABLEDVISIBLE = CDCONTROLSTATEF($00000003);
  {$EXTERNALSYM CDCS_ENABLEDVISIBLE}

type
  PBROWSERFRAMEOPTIONS = ^BROWSERFRAMEOPTIONS;
  _BROWSERFRAMEOPTIONS = type DWORD;
  BROWSERFRAMEOPTIONS = _BROWSERFRAMEOPTIONS;
  {$EXTERNALSYM _BROWSERFRAMEOPTIONS}
  {$EXTERNALSYM BROWSERFRAMEOPTIONS}
const
  BFO_NONE                             = _BROWSERFRAMEOPTIONS(0);
  {$EXTERNALSYM BFO_NONE}
  BFO_BROWSER_PERSIST_SETTINGS         = _BROWSERFRAMEOPTIONS($1);
  {$EXTERNALSYM BFO_BROWSER_PERSIST_SETTINGS}
  BFO_RENAME_FOLDER_OPTIONS_TOINTERNET = _BROWSERFRAMEOPTIONS($2);
  {$EXTERNALSYM BFO_RENAME_FOLDER_OPTIONS_TOINTERNET}
  BFO_BOTH_OPTIONS                     = _BROWSERFRAMEOPTIONS($4);
  {$EXTERNALSYM BFO_BOTH_OPTIONS}
  BIF_PREFER_INTERNET_SHORTCUT         = _BROWSERFRAMEOPTIONS($8);
  {$EXTERNALSYM BIF_PREFER_INTERNET_SHORTCUT}
  BFO_BROWSE_NO_IN_NEW_PROCESS         = _BROWSERFRAMEOPTIONS($10);
  {$EXTERNALSYM BFO_BROWSE_NO_IN_NEW_PROCESS}
  BFO_ENABLE_HYPERLINK_TRACKING        = _BROWSERFRAMEOPTIONS($20);
  {$EXTERNALSYM BFO_ENABLE_HYPERLINK_TRACKING}
  BFO_USE_IE_OFFLINE_SUPPORT           = _BROWSERFRAMEOPTIONS($40);
  {$EXTERNALSYM BFO_USE_IE_OFFLINE_SUPPORT}
  BFO_SUBSTITUE_INTERNET_START_PAGE    = _BROWSERFRAMEOPTIONS($80);
  {$EXTERNALSYM BFO_SUBSTITUE_INTERNET_START_PAGE}
  BFO_USE_IE_LOGOBANDING               = _BROWSERFRAMEOPTIONS($100);
  {$EXTERNALSYM BFO_USE_IE_LOGOBANDING}
  BFO_ADD_IE_TOCAPTIONBAR              = _BROWSERFRAMEOPTIONS($200);
  {$EXTERNALSYM BFO_ADD_IE_TOCAPTIONBAR}
  BFO_USE_DIALUP_REF                   = _BROWSERFRAMEOPTIONS($400);
  {$EXTERNALSYM BFO_USE_DIALUP_REF}
  BFO_USE_IE_TOOLBAR                   = _BROWSERFRAMEOPTIONS($800);
  {$EXTERNALSYM BFO_USE_IE_TOOLBAR}
  BFO_NO_PARENT_FOLDER_SUPPORT         = _BROWSERFRAMEOPTIONS($1000);
  {$EXTERNALSYM BFO_NO_PARENT_FOLDER_SUPPORT}
  BFO_NO_REOPEN_NEXT_RESTART           = _BROWSERFRAMEOPTIONS($2000);
  {$EXTERNALSYM BFO_NO_REOPEN_NEXT_RESTART}
  BFO_GO_HOME_PAGE                     = _BROWSERFRAMEOPTIONS($4000);
  {$EXTERNALSYM BFO_GO_HOME_PAGE}
  BFO_PREFER_IEPROCESS                 = _BROWSERFRAMEOPTIONS($8000);
  {$EXTERNALSYM BFO_PREFER_IEPROCESS}
  BFO_SHOW_NAVIGATION_CANCELLED        = _BROWSERFRAMEOPTIONS($10000);
  {$EXTERNALSYM BFO_SHOW_NAVIGATION_CANCELLED}
  BFO_USE_IE_STATUSBAR                 = _BROWSERFRAMEOPTIONS($20000);
  {$EXTERNALSYM BFO_USE_IE_STATUSBAR}
  BFO_QUERY_ALL                        = _BROWSERFRAMEOPTIONS($FFFFFFFF);
  {$EXTERNALSYM BFO_QUERY_ALL}

type
  PMENUPOPUPSELECT = ^tagMENUPOPUPSELECT;
  tagMENUPOPUPSELECT = type DWORD;
  {$EXTERNALSYM tagMENUPOPUPSELECT}
  MENUPOPUPSELECT = tagMENUPOPUPSELECT;
  {$EXTERNALSYM MENUPOPUPSELECT}
const
  MPOS_EXECUTE	      = tagMENUPOPUPSELECT(0);
  {$EXTERNALSYM MPOS_EXECUTE}
  MPOS_FULLCANCEL    	= tagMENUPOPUPSELECT((MPOS_EXECUTE + 1));
  {$EXTERNALSYM MPOS_FULLCANCEL}
  MPOS_CANCELLEVEL	  = tagMENUPOPUPSELECT((MPOS_FULLCANCEL + 1));
  {$EXTERNALSYM MPOS_CANCELLEVEL}
  MPOS_SELECTLEFT	    = tagMENUPOPUPSELECT((MPOS_CANCELLEVEL + 1));
  {$EXTERNALSYM MPOS_SELECTLEFT}
  MPOS_SELECTRIGHT	  = tagMENUPOPUPSELECT((MPOS_SELECTLEFT + 1));
  {$EXTERNALSYM MPOS_SELECTRIGHT}
  MPOS_CHILDTRACKING	= tagMENUPOPUPSELECT((MPOS_SELECTRIGHT + 1));
  {$EXTERNALSYM MPOS_CHILDTRACKING}

type
  PMP_POPUPFLAGS = ^MP_POPUPFLAGS;
  MP_POPUPFLAGS = type Integer;
  {$EXTERNALSYM MP_POPUPFLAGS}
const
  MPPF_SETFOCUS	     = MP_POPUPFLAGS($1);
  {$EXTERNALSYM MPPF_SETFOCUS}
  MPPF_INITIALSELECT = MP_POPUPFLAGS($2);
  {$EXTERNALSYM MPPF_INITIALSELECT}
  MPPF_NOANIMATE	   = MP_POPUPFLAGS($4);
  {$EXTERNALSYM MPPF_NOANIMATE}
  MPPF_KEYBOARD	     = MP_POPUPFLAGS($10);
  {$EXTERNALSYM MPPF_KEYBOARD}
  MPPF_REPOSITION	   = MP_POPUPFLAGS($20);
  {$EXTERNALSYM MPPF_REPOSITION}
  MPPF_FORCEZORDER   = MP_POPUPFLAGS($40);
  {$EXTERNALSYM MPPF_FORCEZORDER}
  MPPF_FINALSELECT	 = MP_POPUPFLAGS($80);
  {$EXTERNALSYM MPPF_FINALSELECT}
  MPPF_TOP	         = MP_POPUPFLAGS($20000000);
  {$EXTERNALSYM MPPF_TOP}
  MPPF_LEFT	         = MP_POPUPFLAGS($40000000);
  {$EXTERNALSYM MPPF_LEFT}
  MPPF_RIGHT	       = MP_POPUPFLAGS($60000000);
  {$EXTERNALSYM MPPF_RIGHT}
  MPPF_BOTTOM	       = MP_POPUPFLAGS($80000000);
  {$EXTERNALSYM MPPF_BOTTOM}
  MPPF_POS_MASK	     = MP_POPUPFLAGS($e0000000);
  {$EXTERNALSYM MPPF_POS_MASK}
  MPPF_ALIGN_LEFT	   = MP_POPUPFLAGS($2000000);
  {$EXTERNALSYM MPPF_ALIGN_LEFT}
  MPPF_ALIGN_RIGHT   = MP_POPUPFLAGS($4000000);
  {$EXTERNALSYM MPPF_ALIGN_RIGHT}


type
  PNWMF = ^NWMF;
  NWMF = type Integer;
  {$EXTERNALSYM NWMF}
const
  NWMF_UNLOADING        = NWMF($1);
  {$EXTERNALSYM NWMF_UNLOADING}
  NWMF_USERINITED       = NWMF($2);
  {$EXTERNALSYM NWMF_USERINITED}
  NWMF_FIRST            = NWMF($4);
  {$EXTERNALSYM NWMF_FIRST}
  NWMF_OVERRIDEKEY      = NWMF($8);
  {$EXTERNALSYM NWMF_OVERRIDEKEY}
  NWMF_SHOWHELP         = NWMF($10);
  {$EXTERNALSYM NWMF_SHOWHELP}
  NWMF_HTMLDIALOG       = NWMF($20);
  {$EXTERNALSYM NWMF_HTMLDIALOG}
  NWMF_FROMDIALOGCHILD  = NWMF($40);
  {$EXTERNALSYM NWMF_FROMDIALOGCHILD}
  NWMF_USERREQUESTED    = NWMF($80);
  {$EXTERNALSYM NWMF_USERREQUESTED}
  NWMF_USERALLOWED      = NWMF($100);
  {$EXTERNALSYM NWMF_USERALLOWED}
  NWMF_FORCEWINDOW      = NWMF($10000);
  {$EXTERNALSYM NWMF_FORCEWINDOW}
  NWMF_FORCETAB         = NWMF($20000);
  {$EXTERNALSYM NWMF_FORCETAB}
  NWMF_SUGGESTWINDOW    = NWMF($40000);
  {$EXTERNALSYM NWMF_SUGGESTWINDOW}
  NWMF_SUGGESTTAB       = NWMF($80000);
  {$EXTERNALSYM NWMF_SUGGESTTAB}
  NWMF_INACTIVETAB      = NWMF($100000);
  {$EXTERNALSYM NWMF_INACTIVETAB}

type
  PATTACHMENT_PROMPT = ^ATTACHMENT_PROMPT;
  ATTACHMENT_PROMPT = type Integer;
  {$EXTERNALSYM ATTACHMENT_PROMPT}
const
  ATTACHMENT_PROMPT_NONE         = ATTACHMENT_PROMPT($0000);
  {$EXTERNALSYM ATTACHMENT_PROMPT_NONE}
  ATTACHMENT_PROMPT_SAVE         = ATTACHMENT_PROMPT($0001);
  {$EXTERNALSYM ATTACHMENT_PROMPT_SAVE}
  ATTACHMENT_PROMPT_EXEC         = ATTACHMENT_PROMPT($0002);
  {$EXTERNALSYM ATTACHMENT_PROMPT_EXEC}
  ATTACHMENT_PROMPT_EXEC_OR_SAVE = ATTACHMENT_PROMPT($0003);
  {$EXTERNALSYM ATTACHMENT_PROMPT_EXEC_OR_SAVE}

type
  PATTACHMENT_ACTION = ^ATTACHMENT_ACTION;
  ATTACHMENT_ACTION = type Integer;
  {$EXTERNALSYM ATTACHMENT_ACTION}
const
  ATTACHMENT_ACTION_CANCEL = ATTACHMENT_ACTION($0000);
  {$EXTERNALSYM ATTACHMENT_ACTION_CANCEL}
  ATTACHMENT_ACTION_SAVE   = ATTACHMENT_ACTION($0001);
  {$EXTERNALSYM ATTACHMENT_ACTION_SAVE}
  ATTACHMENT_ACTION_EXEC   = ATTACHMENT_ACTION($0002);
  {$EXTERNALSYM ATTACHMENT_ACTION_EXEC}

type
  PNSTCSTYLE = ^NSTCSTYLE;
  _NSTCSTYLE = type DWORD;
  NSTCSTYLE = _NSTCSTYLE;
  {$EXTERNALSYM _NSTCSTYLE}
  {$EXTERNALSYM NSTCSTYLE}
const
  NSTCS_HASEXPANDOS              = _NSTCSTYLE($1);
  {$EXTERNALSYM NSTCS_HASEXPANDOS}
  NSTCS_HASLINES                 = _NSTCSTYLE($2);
  {$EXTERNALSYM NSTCS_HASLINES}
  NSTCS_SINGLECLICKEXPAND        = _NSTCSTYLE($4);
  {$EXTERNALSYM NSTCS_SINGLECLICKEXPAND}
  NSTCS_FULLROWSELECT            = _NSTCSTYLE($8);
  {$EXTERNALSYM NSTCS_FULLROWSELECT}
  NSTCS_SPRINGEXPAND             = _NSTCSTYLE($10);
  {$EXTERNALSYM NSTCS_SPRINGEXPAND}
  NSTCS_HORIZONTALSCROLL         = _NSTCSTYLE($20);
  {$EXTERNALSYM NSTCS_HORIZONTALSCROLL}
  NSTCS_ROOTHASEXPANDO           = _NSTCSTYLE($40);
  {$EXTERNALSYM NSTCS_ROOTHASEXPANDO}
  NSTCS_SHOWSELECTIONALWAYS      = _NSTCSTYLE($80);
  {$EXTERNALSYM NSTCS_SHOWSELECTIONALWAYS}
  NSTCS_NOINFOTIP                = _NSTCSTYLE($200);
  {$EXTERNALSYM NSTCS_NOINFOTIP}
  NSTCS_EVENHEIGHT               = _NSTCSTYLE($400);
  {$EXTERNALSYM NSTCS_EVENHEIGHT}
  NSTCS_NOREPLACEOPEN            = _NSTCSTYLE($800);
  {$EXTERNALSYM NSTCS_NOREPLACEOPEN}
  NSTCS_DISABLEDRAGDROP          = _NSTCSTYLE($1000);
  {$EXTERNALSYM NSTCS_DISABLEDRAGDROP}
  NSTCS_NOORDERSTREAM            = _NSTCSTYLE($2000);
  {$EXTERNALSYM NSTCS_NOORDERSTREAM}
  NSTCS_RICHTOOLTIP              = _NSTCSTYLE($4000);
  {$EXTERNALSYM NSTCS_RICHTOOLTIP}
  NSTCS_BORDER                   = _NSTCSTYLE($8000);
  {$EXTERNALSYM NSTCS_BORDER}
  NSTCS_NOEDITLABELS             = _NSTCSTYLE($10000);
  {$EXTERNALSYM NSTCS_NOEDITLABELS}
  NSTCS_TABSTOP                  = _NSTCSTYLE($20000);
  {$EXTERNALSYM NSTCS_TABSTOP}
  NSTCS_FAVORITESMODE            = _NSTCSTYLE($80000);
  {$EXTERNALSYM NSTCS_FAVORITESMODE}
  NSTCS_AUTOHSCROLL              = _NSTCSTYLE($100000);
  {$EXTERNALSYM NSTCS_AUTOHSCROLL}
  NSTCS_FADEINOUTEXPANDOS        = _NSTCSTYLE($200000);
  {$EXTERNALSYM NSTCS_FADEINOUTEXPANDOS}
  NSTCS_EMPTYTEXT                = _NSTCSTYLE($400000);
  {$EXTERNALSYM NSTCS_EMPTYTEXT}
  NSTCS_CHECKBOXES               = _NSTCSTYLE($800000);
  {$EXTERNALSYM NSTCS_CHECKBOXES}
  NSTCS_PARTIALCHECKBOXES        = _NSTCSTYLE($1000000);
  {$EXTERNALSYM NSTCS_PARTIALCHECKBOXES}
  NSTCS_EXCLUSIONCHECKBOXES      = _NSTCSTYLE($2000000);
  {$EXTERNALSYM NSTCS_EXCLUSIONCHECKBOXES}
  NSTCS_DIMMEDCHECKBOXES         = _NSTCSTYLE($4000000);
  {$EXTERNALSYM NSTCS_DIMMEDCHECKBOXES}
  NSTCS_NOINDENTCHECKS           = _NSTCSTYLE($8000000);
  {$EXTERNALSYM NSTCS_NOINDENTCHECKS}
  NSTCS_ALLOWJUNCTIONS           = _NSTCSTYLE($10000000);
  {$EXTERNALSYM NSTCS_ALLOWJUNCTIONS}
  NSTCS_SHOWTABSBUTTON           = _NSTCSTYLE($20000000);
  {$EXTERNALSYM NSTCS_SHOWTABSBUTTON}
  NSTCS_SHOWDELETEBUTTON         = _NSTCSTYLE($40000000);
  {$EXTERNALSYM NSTCS_SHOWDELETEBUTTON}
  NSTCS_SHOWREFRESHBUTTON        = _NSTCSTYLE($80000000);
  {$EXTERNALSYM NSTCS_SHOWREFRESHBUTTON}

type
  PNSTCROOTSTYLE = ^NSTCROOTSTYLE;
  _NSTCROOTSTYLE = type DWORD;
  NSTCROOTSTYLE = _NSTCROOTSTYLE;
  {$EXTERNALSYM _NSTCROOTSTYLE}
  {$EXTERNALSYM NSTCROOTSTYLE}
const
  NSTCRS_VISIBLE  = _NSTCROOTSTYLE(0);
  {$EXTERNALSYM NSTCRS_VISIBLE}
  NSTCRS_HIDDEN   = _NSTCROOTSTYLE($1);
  {$EXTERNALSYM NSTCRS_HIDDEN}
  NSTCRS_EXPANDED = _NSTCROOTSTYLE($2);
  {$EXTERNALSYM NSTCRS_EXPANDED}

type
  PNSTCITEMSTATE = ^NSTCITEMSTATE;
  _NSTCITEMSTATE = type DWORD;
  NSTCITEMSTATE = _NSTCITEMSTATE;
  {$EXTERNALSYM _NSTCITEMSTATE}
  {$EXTERNALSYM NSTCITEMSTATE}
const
  NSTCIS_NONE             = _NSTCITEMSTATE(0);
  {$EXTERNALSYM NSTCIS_NONE}
  NSTCIS_SELECTED         = _NSTCITEMSTATE($1);
  {$EXTERNALSYM NSTCIS_SELECTED}
  NSTCIS_EXPANDED         = _NSTCITEMSTATE($2);
  {$EXTERNALSYM NSTCIS_EXPANDED}
  NSTCIS_BOLD             = _NSTCITEMSTATE($4);
  {$EXTERNALSYM NSTCIS_BOLD}
  NSTCIS_DISABLED         = _NSTCITEMSTATE($8);
  {$EXTERNALSYM NSTCIS_DISABLED}
  NSTCIS_SELECTEDNOEXPAND = _NSTCITEMSTATE($10);
  {$EXTERNALSYM NSTCIS_SELECTEDNOEXPAND}

type
  PNSTCGNI = ^NSTCGNI;
  NSTCGNI = type DWORD;
  {$EXTERNALSYM NSTCGNI}
const
    NSTCGNI_NEXT         = NSTCGNI(0);
    NSTCGNI_NEXTVISIBLE  = NSTCGNI(1);
    NSTCGNI_PREV         = NSTCGNI(2);
    NSTCGNI_PREVVISIBLE  = NSTCGNI(3);
    NSTCGNI_PARENT       = NSTCGNI(4);
    NSTCGNI_CHILD        = NSTCGNI(5);
    NSTCGNI_FIRSTVISIBLE = NSTCGNI(6);
    NSTCGNI_LASTVISIBLE  = NSTCGNI(7);

type
  PEXPLORERPANESTATE = ^EXPLORERPANESTATE;
  _EXPLORERPANESTATE = type DWORD;
  EXPLORERPANESTATE = _EXPLORERPANESTATE;
  {$EXTERNALSYM _EXPLORERPANESTATE}
  {$EXTERNALSYM EXPLORERPANESTATE}
const
  EPS_DONTCARE    = _EXPLORERPANESTATE(0);
  {$EXTERNALSYM EPS_DONTCARE}
  EPS_DEFAULT_ON  = _EXPLORERPANESTATE($1);
  {$EXTERNALSYM EPS_DEFAULT_ON}
  EPS_DEFAULT_OFF = _EXPLORERPANESTATE($2);
  {$EXTERNALSYM EPS_DEFAULT_OFF}
  EPS_STATEMASK   = _EXPLORERPANESTATE($FFFF);
  {$EXTERNALSYM EPS_STATEMASK}
  EPS_INITIALSTATE = _EXPLORERPANESTATE($10000);
  {$EXTERNALSYM EPS_INITIALSTATE}
  EPS_FORCE       = _EXPLORERPANESTATE($20000);
  {$EXTERNALSYM EPS_FORCE}

type
  PEXPCMDSTATE = ^EXPCMDSTATE;
  _EXPCMDSTATE = type DWORD;
  EXPCMDSTATE = _EXPCMDSTATE;
  {$EXTERNALSYM _EXPCMDSTATE}
  {$EXTERNALSYM EXPCMDSTATE}
const
  ECS_ENABLED    = _EXPCMDSTATE(0);
  {$EXTERNALSYM ECS_ENABLED}
  ECS_DISABLED   = _EXPCMDSTATE($1);
  {$EXTERNALSYM ECS_DISABLED}
  ECS_HIDDEN     = _EXPCMDSTATE($2);
  {$EXTERNALSYM ECS_HIDDEN}
  ECS_CHECKBOX   = _EXPCMDSTATE($4);
  {$EXTERNALSYM ECS_CHECKBOX}
  ECS_CHECKED    = _EXPCMDSTATE($8);
  {$EXTERNALSYM ECS_CHECKED}
  ECS_RADIOCHECK = _EXPCMDSTATE($10);
  {$EXTERNALSYM ECS_RADIOCHECK}

type
  PKNOWNDESTCATEGORY = ^KNOWNDESTCATEGORY;
  {$EXTERNALSYM KNOWNDESTCATEGORY}
  KNOWNDESTCATEGORY = type Integer;
const
    KDC_FREQUENT = KNOWNDESTCATEGORY(1);
    KDC_RECENT   = KNOWNDESTCATEGORY((KDC_FREQUENT + 1));

type
  PCPVIEW = ^CPVIEW;
  CPVIEW = type DWORD;
  {$EXTERNALSYM CPVIEW}
const
  CPVIEW_CLASSIC  = CPVIEW(0);
  CPVIEW_ALLITEMS = CPVIEW(CPVIEW_CLASSIC);
  CPVIEW_CATEGORY = CPVIEW(1);
  CPVIEW_HOME     = CPVIEW(CPVIEW_CATEGORY);

type
  PEXPCMDFLAGS = ^EXPCMDFLAGS;
  _EXPCMDFLAGS = type DWORD;
  EXPCMDFLAGS = _EXPCMDFLAGS;
  {$EXTERNALSYM _EXPCMDFLAGS}
  {$EXTERNALSYM EXPCMDFLAGS}
const
  ECF_DEFAULT         = _EXPCMDFLAGS(0);
  {$EXTERNALSYM ECF_DEFAULT}
  ECF_HASSUBCOMMANDS = _EXPCMDFLAGS($1);
  {$EXTERNALSYM ECF_HASSUBCOMMANDS}
  ECF_HASSPLITBUTTON = _EXPCMDFLAGS($2);
  {$EXTERNALSYM ECF_HASSPLITBUTTON}
  ECF_HIDELABEL      = _EXPCMDFLAGS($4);
  {$EXTERNALSYM ECF_HIDELABEL}
  ECF_ISSEPARATOR    = _EXPCMDFLAGS($8);
  {$EXTERNALSYM ECF_ISSEPARATOR}
  ECF_HASLUASHIELD   = _EXPCMDFLAGS($10);
  {$EXTERNALSYM ECF_HASLUASHIELD}
  ECF_SEPARATORBEFORE = _EXPCMDFLAGS($20);
  {$EXTERNALSYM ECF_SEPARATORBEFORE}
  ECF_SEPARATORAFTER = _EXPCMDFLAGS($40);
  {$EXTERNALSYM ECF_SEPARATORAFTER}
  ECF_ISDROPDOWN     = _EXPCMDFLAGS($80);
  {$EXTERNALSYM ECF_ISDROPDOWN}
  ECF_TOGGLEABLE     = _EXPCMDFLAGS($100);
  {$EXTERNALSYM ECF_TOGGLEABLE}
  ECF_AUTOMENUICONS  = _EXPCMDFLAGS($200);
  {$EXTERNALSYM ECF_AUTOMENUICONS}

type
  PBANNER_NOTIFICATION_EVENT = ^BANNER_NOTIFICATION_EVENT;
  BANNER_NOTIFICATION_EVENT = type DWORD;
  {$EXTERNALSYM BANNER_NOTIFICATION_EVENT}
const
    BNE_Rendered       = BANNER_NOTIFICATION_EVENT(0);
    {$EXTERNALSYM BNE_Rendered}
    BNE_Hovered        = BANNER_NOTIFICATION_EVENT(BNE_Rendered + 1);
    {$EXTERNALSYM BNE_Hovered}
    BNE_Closed         = BANNER_NOTIFICATION_EVENT(BNE_Hovered + 1);
    {$EXTERNALSYM BNE_Closed}
    BNE_Dismissed      = BANNER_NOTIFICATION_EVENT(BNE_Closed + 1);
    {$EXTERNALSYM BNE_Dismissed}
    BNE_Button1Clicked = BANNER_NOTIFICATION_EVENT(BNE_Dismissed + 1);
    {$EXTERNALSYM BNE_Button1Clicked}
    BNE_Button2Clicked = BANNER_NOTIFICATION_EVENT(BNE_Button1Clicked + 1);
    {$EXTERNALSYM BNE_Button2Clicked}

type
  PFDE_OVERWRITE_RESPONSE = ^FDE_OVERWRITE_RESPONSE;
  FDE_OVERWRITE_RESPONSE = type DWORD;
  {$EXTERNALSYM FDE_OVERWRITE_RESPONSE}
const
  FDEOR_DEFAULT = FDE_OVERWRITE_RESPONSE(0); // The application has not handled the event, and the dialog should put up UI asking the user
  {$EXTERNALSYM FDEOR_DEFAULT}
                                             // whether or not the file should be overwritten and be returned from the dialog
  FDEOR_ACCEPT  = FDE_OVERWRITE_RESPONSE(1); // The application has decided that the file should be returned from the dialog
  {$EXTERNALSYM FDEOR_ACCEPT}
  FDEOR_REFUSE  = FDE_OVERWRITE_RESPONSE(2); // The application has decided that the file should not be returned from the dialog
  {$EXTERNALSYM FDEOR_REFUSE}

type
  PFDE_SHAREVIOLATION_RESPONSE = ^FDE_SHAREVIOLATION_RESPONSE;
  FDE_SHAREVIOLATION_RESPONSE = type DWORD;
  {$EXTERNALSYM FDE_SHAREVIOLATION_RESPONSE}
const
  FDESVR_DEFAULT = FDE_SHAREVIOLATION_RESPONSE(0); // The application has not handled the event.  The dialog will put up UI indicating the file
  {$EXTERNALSYM FDESVR_DEFAULT}
                                                     // is in use, and a different file must be chosen.
  FDESVR_ACCEPT  = FDE_SHAREVIOLATION_RESPONSE(1); // The application has decided that the file should be returned from the dialog
  {$EXTERNALSYM FDESVR_ACCEPT}
  FDESVR_REFUSE  = FDE_SHAREVIOLATION_RESPONSE(2); // The application has decided that the file should not be returned from the dialog
  {$EXTERNALSYM FDESVR_REFUSE}

type
  PFDAP = ^FDAP;
  FDAP = type DWORD;
  {$EXTERNALSYM FDAP}
const
  FDAP_BOTTOM = FDAP(0);                // The place is added to the bottom of the default list.
  {$EXTERNALSYM FDAP_BOTTOM}
  FDAP_TOP    = FDAP(1);                // The place is added to the top of the default list.
  {$EXTERNALSYM FDAP_TOP}

type
  PSHARE_ROLE = ^SHARE_ROLE;
  SHARE_ROLE = type Integer;
  {$EXTERNALSYM SHARE_ROLE}
const
  SHARE_ROLE_INVALID     = SHARE_ROLE(- 1);
  {$EXTERNALSYM SHARE_ROLE_INVALID}
  SHARE_ROLE_READER      = SHARE_ROLE(0);
  {$EXTERNALSYM SHARE_ROLE_READER}
  SHARE_ROLE_CONTRIBUTOR = SHARE_ROLE(1);
  {$EXTERNALSYM SHARE_ROLE_CONTRIBUTOR}
  SHARE_ROLE_CO_OWNER    = SHARE_ROLE(2);
  {$EXTERNALSYM SHARE_ROLE_CO_OWNER}
  SHARE_ROLE_OWNER       = SHARE_ROLE(3);
  {$EXTERNALSYM SHARE_ROLE_OWNER}
  SHARE_ROLE_CUSTOM      = SHARE_ROLE(4);
  {$EXTERNALSYM SHARE_ROLE_CUSTOM}
  SHARE_ROLE_MIXED       = SHARE_ROLE(5);
  {$EXTERNALSYM SHARE_ROLE_MIXED}

type
  PDEF_SHARE_ID = ^DEF_SHARE_ID;
  DEF_SHARE_ID = type DWORD;
  {$EXTERNALSYM DEF_SHARE_ID}
const
  DEFSHAREID_USERS  = DEF_SHARE_ID(1);
  DEFSHAREID_PUBLIC = DEF_SHARE_ID(2);

type
  PNMCII_FLAGS = ^NMCII_FLAGS;
  NMCII_FLAGS = type Integer;
  {$EXTERNALSYM NMCII_FLAGS}
const
  NMCII_NONE	  = NMCII_FLAGS($0);
  {$EXTERNALSYM NMCII_NONE}
  NMCII_ITEMS	  = NMCII_FLAGS($1);
  {$EXTERNALSYM NMCII_ITEMS}
  NMCII_FOLDERS	= NMCII_FLAGS($2);
  {$EXTERNALSYM NMCII_FOLDERS}

type
  PNMCSAEI_FLAGS = ^NMCSAEI_FLAGS;
  NMCSAEI_FLAGS = type Integer;
  {$EXTERNALSYM NMCSAEI_FLAGS}
const
  NMCSAEI_SELECT	= NMCSAEI_FLAGS($0);
  {$EXTERNALSYM NMCSAEI_SELECT}
  NMCSAEI_EDIT   	= NMCSAEI_FLAGS($1);
  {$EXTERNALSYM NMCSAEI_EDIT}

type
  PNSTCFOLDERCAPABILITIES = ^NSTCFOLDERCAPABILITIES;
  NSTCFOLDERCAPABILITIES = type DWORD;
  {$EXTERNALSYM NSTCFOLDERCAPABILITIES}
const
  NSTCFC_NONE                  = NSTCFOLDERCAPABILITIES($0);
  {$EXTERNALSYM NSTCFC_NONE}
  NSTCFC_PINNEDITEMFILTERING   = NSTCFOLDERCAPABILITIES($1);
  {$EXTERNALSYM NSTCFC_PINNEDITEMFILTERING}
  NSTCFC_DELAY_REGISTER_NOTIFY = NSTCFOLDERCAPABILITIES($2);
  {$EXTERNALSYM NSTCFC_DELAY_REGISTER_NOTIFY}

type
  PAPPDOCLISTTYPE = ^APPDOCLISTTYPE;
  APPDOCLISTTYPE  = type DWORD;
  {$EXTERNALSYM APPDOCLISTTYPE}
const
  ADLT_RECENT   = APPDOCLISTTYPE(0);
  {$EXTERNALSYM ADLT_RECENT}
  ADLT_FREQUENT = APPDOCLISTTYPE((ADLT_RECENT + 1));
  {$EXTERNALSYM ADLT_FREQUENT}

type

  PDESKTOP_SLIDESHOW_OPTIONS = ^DESKTOP_SLIDESHOW_OPTIONS;
  {$EXTERNALSYM DESKTOP_SLIDESHOW_OPTIONS}
  DESKTOP_SLIDESHOW_OPTIONS = type DWORD;
const
  DSO_SHUFFLEIMAGES = DESKTOP_SLIDESHOW_OPTIONS($1);
  {$EXTERNALSYM DSO_SHUFFLEIMAGES}

type
  PDESKTOP_SLIDESHOW_STATE = ^DESKTOP_SLIDESHOW_STATE;
  {$EXTERNALSYM DESKTOP_SLIDESHOW_STATE}
  DESKTOP_SLIDESHOW_STATE = type DWORD;
const
  DSS_ENABLED                    = DESKTOP_SLIDESHOW_STATE($1);
  {$EXTERNALSYM DSS_ENABLED}
  DSS_SLIDESHOW                  = DESKTOP_SLIDESHOW_STATE($2);
  {$EXTERNALSYM DSS_SLIDESHOW}
  DSS_DISABLED_BY_REMOTE_SESSION = DESKTOP_SLIDESHOW_STATE($4);
  {$EXTERNALSYM DSS_DISABLED_BY_REMOTE_SESSION}

type
  PDESKTOP_SLIDESHOW_DIRECTION = ^DESKTOP_SLIDESHOW_DIRECTION;
  {$EXTERNALSYM DESKTOP_SLIDESHOW_DIRECTION}
  DESKTOP_SLIDESHOW_DIRECTION = type DWORD;
const
  DSD_FORWARD  = DESKTOP_SLIDESHOW_DIRECTION(0);
  {$EXTERNALSYM DSD_FORWARD}
  DSD_BACKWARD = DESKTOP_SLIDESHOW_DIRECTION(1);
  {$EXTERNALSYM DSD_BACKWARD}

type
  PDESKTOP_WALLPAPER_POSITION = ^DESKTOP_WALLPAPER_POSITION;
  DESKTOP_WALLPAPER_POSITION = type DWORD;
  {$EXTERNALSYM DESKTOP_WALLPAPER_POSITION}
const
  DWPOS_CENTER  = DESKTOP_WALLPAPER_POSITION(0);
  {$EXTERNALSYM DWPOS_CENTER}
  DWPOS_TILE    = DESKTOP_WALLPAPER_POSITION(1);
  {$EXTERNALSYM DWPOS_TILE}
  DWPOS_STRETCH = DESKTOP_WALLPAPER_POSITION(2);
  {$EXTERNALSYM DWPOS_STRETCH}
  DWPOS_FIT     = DESKTOP_WALLPAPER_POSITION(3);
  {$EXTERNALSYM DWPOS_FIT}
  DWPOS_FILL    = DESKTOP_WALLPAPER_POSITION(4);
  {$EXTERNALSYM DWPOS_FILL}
  DWPOS_SPAN    = DESKTOP_WALLPAPER_POSITION(5);
  {$EXTERNALSYM DWPOS_SPAN}

type
  PHOMEGROUPSHARINGCHOICES = ^HOMEGROUPSHARINGCHOICES;
  HOMEGROUPSHARINGCHOICES = type DWORD;
  {$EXTERNALSYM HOMEGROUPSHARINGCHOICES}
const
  HGSC_NONE             = HOMEGROUPSHARINGCHOICES(0);
  {$EXTERNALSYM HGSC_NONE}
  HGSC_MUSICLIBRARY     = HOMEGROUPSHARINGCHOICES($1);
  {$EXTERNALSYM HGSC_MUSICLIBRARY}
  HGSC_PICTURESLIBRARY  = HOMEGROUPSHARINGCHOICES($2);
  {$EXTERNALSYM HGSC_PICTURESLIBRARY}
  HGSC_VIDEOSLIBRARY    = HOMEGROUPSHARINGCHOICES($4);
  {$EXTERNALSYM HGSC_VIDEOSLIBRARY}
  HGSC_DOCUMENTSLIBRARY = HOMEGROUPSHARINGCHOICES($8);
  {$EXTERNALSYM HGSC_DOCUMENTSLIBRARY}
  HGSC_PRINTERS         = HOMEGROUPSHARINGCHOICES($10);
  {$EXTERNALSYM HGSC_PRINTERS}

type
  PLIBRARYFOLDERFILTER = ^LIBRARYFOLDERFILTER;
  LIBRARYFOLDERFILTER = type Integer;
  {$EXTERNALSYM LIBRARYFOLDERFILTER}

const
  LFF_FORCEFILESYSTEM = LIBRARYFOLDERFILTER(1);
  {$EXTERNALSYM LFF_FORCEFILESYSTEM}
  LFF_STORAGEITEMS    = LIBRARYFOLDERFILTER(2);
  {$EXTERNALSYM LFF_STORAGEITEMS}
  LFF_ALLITEMS        = LIBRARYFOLDERFILTER(3);
  {$EXTERNALSYM LFF_ALLITEMS}

type
  PLIBRARYOPTIONFLAGS = ^LIBRARYOPTIONFLAGS;
  LIBRARYOPTIONFLAGS = type Integer;
  {$EXTERNALSYM LIBRARYOPTIONFLAGS}

const
  LOF_DEFAULT        = LIBRARYOPTIONFLAGS(0);
  {$EXTERNALSYM LOF_DEFAULT}
  LOF_PINNEDTONAVPANE = LIBRARYOPTIONFLAGS($1);
  {$EXTERNALSYM LOF_PINNEDTONAVPANE}
  LOF_MASK_ALL      = LIBRARYOPTIONFLAGS($1);
  {$EXTERNALSYM LOF_MASK_ALL}

type
  PDEFAULTSAVEFOLDERTYPE = ^DEFAULTSAVEFOLDERTYPE;
  DEFAULTSAVEFOLDERTYPE = type Integer;
  {$EXTERNALSYM DEFAULTSAVEFOLDERTYPE}

const
  DSFT_DETECT  = DEFAULTSAVEFOLDERTYPE(1);
  {$EXTERNALSYM DSFT_DETECT}
  DSFT_PRIVATE = DEFAULTSAVEFOLDERTYPE(2);
  {$EXTERNALSYM DSFT_PRIVATE}
  DSFT_PUBLIC  = DEFAULTSAVEFOLDERTYPE(3);
  {$EXTERNALSYM DSFT_PUBLIC}

type
  PLIBRARYSAVEFLAGS = ^LIBRARYSAVEFLAGS;
  LIBRARYSAVEFLAGS = type Integer;
  {$EXTERNALSYM LIBRARYSAVEFLAGS}

const
  LSF_FAILIFTHERE      = LIBRARYSAVEFLAGS(0);
  {$EXTERNALSYM LSF_FAILIFTHERE}
  LSF_OVERRIDEEXISTING = LIBRARYSAVEFLAGS($1);
  {$EXTERNALSYM LSF_OVERRIDEEXISTING}
  LSF_MAKEUNIQUENAME   = LIBRARYSAVEFLAGS($2);
  {$EXTERNALSYM LSF_MAKEUNIQUENAME}

type
  PDEFAULT_FOLDER_MENU_RESTRICTIONS = ^DEFAULT_FOLDER_MENU_RESTRICTIONS;
  DEFAULT_FOLDER_MENU_RESTRICTIONS = type DWORD;
  {$EXTERNALSYM DEFAULT_FOLDER_MENU_RESTRICTIONS}
const
  DFMR_DEFAULT                        = DEFAULT_FOLDER_MENU_RESTRICTIONS(0);
  {$EXTERNALSYM DFMR_DEFAULT}
  DFMR_NO_STATIC_VERBS                = DEFAULT_FOLDER_MENU_RESTRICTIONS($8);
  {$EXTERNALSYM DFMR_NO_STATIC_VERBS}
  DFMR_STATIC_VERBS_ONLY              = DEFAULT_FOLDER_MENU_RESTRICTIONS($10);
  {$EXTERNALSYM DFMR_STATIC_VERBS_ONLY}
  DFMR_NO_RESOURCE_VERBS              = DEFAULT_FOLDER_MENU_RESTRICTIONS($20);
  {$EXTERNALSYM DFMR_NO_RESOURCE_VERBS}
  DFMR_OPTIN_HANDLERS_ONLY            = DEFAULT_FOLDER_MENU_RESTRICTIONS($40);
  {$EXTERNALSYM DFMR_OPTIN_HANDLERS_ONLY}
  DFMR_RESOURCE_AND_FOLDER_VERBS_ONLY = DEFAULT_FOLDER_MENU_RESTRICTIONS($80);
  {$EXTERNALSYM DFMR_RESOURCE_AND_FOLDER_VERBS_ONLY}
  DFMR_USE_SPECIFIED_HANDLERS         = DEFAULT_FOLDER_MENU_RESTRICTIONS($100);
  {$EXTERNALSYM DFMR_USE_SPECIFIED_HANDLERS}
  DFMR_USE_SPECIFIED_VERBS            = DEFAULT_FOLDER_MENU_RESTRICTIONS($200);
  {$EXTERNALSYM DFMR_USE_SPECIFIED_VERBS}
  DFMR_NO_ASYNC_VERBS                 = DEFAULT_FOLDER_MENU_RESTRICTIONS($400);
  {$EXTERNALSYM DFMR_NO_ASYNC_VERBS}
  DFMR_NO_NATIVECPU_VERBS             = DEFAULT_FOLDER_MENU_RESTRICTIONS($800);
  {$EXTERNALSYM DFMR_NO_NATIVECPU_VERBS}
  DFMR_NO_NONWOW_VERBS                = DEFAULT_FOLDER_MENU_RESTRICTIONS($1000);
  {$EXTERNALSYM DFMR_NO_NONWOW_VERBS}

type
  PACTIVATEOPTIONS = ^ACTIVATEOPTIONS;
  ACTIVATEOPTIONS = type Integer;
  {$EXTERNALSYM ACTIVATEOPTIONS}
const
  AO_NONE            = ACTIVATEOPTIONS(0);
  {$EXTERNALSYM AO_NONE}
  AO_DESIGNMODE      = ACTIVATEOPTIONS($1);
  {$EXTERNALSYM AO_DESIGNMODE}
  AO_NOERRORUI       = ACTIVATEOPTIONS($2);
  {$EXTERNALSYM AO_NOERRORUI}
  AO_NOSPLASHSCREEN  = ACTIVATEOPTIONS($4);
  {$EXTERNALSYM AO_NOSPLASHSCREEN}
  AO_PRELAUNCH       = ACTIVATEOPTIONS($02000000);
  {$EXTERNALSYM AO_PRELAUNCH}

type
  PMONITOR_APP_VISIBILITY = ^MONITOR_APP_VISIBILITY;
  MONITOR_APP_VISIBILITY = Integer;
  {$EXTERNALSYM MONITOR_APP_VISIBILITY}
const
  MAV_UNKNOWN        = MONITOR_APP_VISIBILITY(0);
  {$EXTERNALSYM MAV_UNKNOWN}
  MAV_NO_APP_VISIBLE = MONITOR_APP_VISIBILITY(1);
  {$EXTERNALSYM MAV_NO_APP_VISIBLE}
  MAV_APP_VISIBLE    = MONITOR_APP_VISIBILITY(2);
  {$EXTERNALSYM MAV_APP_VISIBLE}

type
  PPACKAGE_EXECUTION_STATE = ^PACKAGE_EXECUTION_STATE;
  PACKAGE_EXECUTION_STATE = type Integer;
  {$EXTERNALSYM PACKAGE_EXECUTION_STATE}
const
  PES_UNKNOWN     = PACKAGE_EXECUTION_STATE(0);
  {$EXTERNALSYM PES_UNKNOWN}
  PES_RUNNING     = PACKAGE_EXECUTION_STATE(1);
  {$EXTERNALSYM PES_RUNNING}
  PES_SUSPENDING  = PACKAGE_EXECUTION_STATE(2);
  {$EXTERNALSYM PES_SUSPENDING}
  PES_SUSPENDED   = PACKAGE_EXECUTION_STATE(3);
  {$EXTERNALSYM PES_SUSPENDED}
  PES_TERMINATED  = PACKAGE_EXECUTION_STATE(4);
  {$EXTERNALSYM PES_TERMINATED}

type
  PAHE_TYPE = ^AHE_TYPE;
  AHE_TYPE = type Integer;
  {$EXTERNALSYM AHE_TYPE}
const
  AHE_DESKTOP   = AHE_TYPE(0);
  {$EXTERNALSYM AHE_DESKTOP}
  AHE_IMMERSIVE = AHE_TYPE(1);
  {$EXTERNALSYM AHE_IMMERSIVE}

type
  PEC_HOST_UI_MODE = ^EC_HOST_UI_MODE;
  EC_HOST_UI_MODE = type Integer;
  {$EXTERNALSYM EC_HOST_UI_MODE}
const
  ECHUIM_DESKTOP         = EC_HOST_UI_MODE(0);
  {$EXTERNALSYM ECHUIM_DESKTOP}
  ECHUIM_IMMERSIVE       = EC_HOST_UI_MODE(1);
  {$EXTERNALSYM ECHUIM_IMMERSIVE}
  ECHUIM_SYSTEM_LAUNCHER = EC_HOST_UI_MODE(2);
  {$EXTERNALSYM ECHUIM_SYSTEM_LAUNCHER}

type
  PAPPLICATION_VIEW_STATE = ^APPLICATION_VIEW_STATE;
  APPLICATION_VIEW_STATE = type Integer;
  {$EXTERNALSYM APPLICATION_VIEW_STATE}
const
  AVS_FULLSCREEN_LANDSCAPE = APPLICATION_VIEW_STATE(0);
  {$EXTERNALSYM AVS_FULLSCREEN_LANDSCAPE}
  AVS_FILLED               = APPLICATION_VIEW_STATE(1);
  {$EXTERNALSYM AVS_FILLED}
  AVS_SNAPPED              = APPLICATION_VIEW_STATE(2);
  {$EXTERNALSYM AVS_SNAPPED}
  AVS_FULLSCREEN_PORTRAIT  = APPLICATION_VIEW_STATE(3);
  {$EXTERNALSYM AVS_FULLSCREEN_PORTRAIT}

type
  PEDGE_GESTURE_KIND = ^EDGE_GESTURE_KIND;
  EDGE_GESTURE_KIND = type Integer;
  {$EXTERNALSYM EDGE_GESTURE_KIND}
const
  EGK_TOUCH    = EDGE_GESTURE_KIND(0);
  {$EXTERNALSYM EGK_TOUCH}
  EGK_KEYBOARD = EDGE_GESTURE_KIND(1);
  {$EXTERNALSYM EGK_KEYBOARD}
  EGK_MOUSE    = EDGE_GESTURE_KIND(2);
  {$EXTERNALSYM EGK_MOUSE}

type
  PNATIVE_DISPLAY_ORIENTATION = ^NATIVE_DISPLAY_ORIENTATION;
  NATIVE_DISPLAY_ORIENTATION = type Integer;
  {$EXTERNALSYM NATIVE_DISPLAY_ORIENTATION}

const
  NDO_LANDSCAPE = NATIVE_DISPLAY_ORIENTATION(0);
  {$EXTERNALSYM NDO_LANDSCAPE}

  NDO_PORTRAIT  = NATIVE_DISPLAY_ORIENTATION(1);
  {$EXTERNALSYM NDO_PORTRAIT}



type
  PAPPLICATION_VIEW_ORIENTATION = ^APPLICATION_VIEW_ORIENTATION;
  APPLICATION_VIEW_ORIENTATION = type Integer;
  {$EXTERNALSYM APPLICATION_VIEW_ORIENTATION}
const
  AVO_LANDSCAPE = APPLICATION_VIEW_ORIENTATION(0);
  {$EXTERNALSYM AVO_LANDSCAPE}
  AVO_PORTRAIT  = APPLICATION_VIEW_ORIENTATION(1);
  {$EXTERNALSYM AVO_PORTRAIT}

type
  PADJACENT_DISPLAY_EDGES = ^ADJACENT_DISPLAY_EDGES;
  ADJACENT_DISPLAY_EDGES = type Integer;
  {$EXTERNALSYM ADJACENT_DISPLAY_EDGES}
const
  ADE_NONE  = ADJACENT_DISPLAY_EDGES(0);
  {$EXTERNALSYM ADE_NONE}
  ADE_LEFT  = ADJACENT_DISPLAY_EDGES($1);
  {$EXTERNALSYM ADE_LEFT}
  ADE_RIGHT = ADJACENT_DISPLAY_EDGES($2);
  {$EXTERNALSYM ADE_RIGHT}

type
  PAPPLICATION_VIEW_MIN_WIDTH = ^APPLICATION_VIEW_MIN_WIDTH;
  APPLICATION_VIEW_MIN_WIDTH = type Integer;
  {$EXTERNALSYM APPLICATION_VIEW_MIN_WIDTH}
const
  AVMW_DEFAULT = APPLICATION_VIEW_MIN_WIDTH(0);
  {$EXTERNALSYM AVMW_DEFAULT}
  AVMW_320     = APPLICATION_VIEW_MIN_WIDTH(1);
  {$EXTERNALSYM AVMW_320}
  AVMW_500     = APPLICATION_VIEW_MIN_WIDTH(2);
  {$EXTERNALSYM AVMW_500}

type
  APPLICATION_VIEW_SIZE_PREFERENCE = type Integer;
  {$EXTERNALSYM APPLICATION_VIEW_SIZE_PREFERENCE}
const
  AVSP_CUSTOM      = APPLICATION_VIEW_SIZE_PREFERENCE(6);
  {$EXTERNALSYM AVSP_DEFAULT}
  AVSP_DEFAULT     = APPLICATION_VIEW_SIZE_PREFERENCE(0);
  {$EXTERNALSYM AVSP_USE_LESS}
  AVSP_USE_LESS    = APPLICATION_VIEW_SIZE_PREFERENCE(1);
  {$EXTERNALSYM AVSP_USE_HALF}
  AVSP_USE_HALF    = APPLICATION_VIEW_SIZE_PREFERENCE(2);
  {$EXTERNALSYM AVSP_USE_MORE}
  AVSP_USE_MORE    = APPLICATION_VIEW_SIZE_PREFERENCE(3);
  {$EXTERNALSYM AVSP_USE_MINIMUM}
  AVSP_USE_MINIMUM = APPLICATION_VIEW_SIZE_PREFERENCE(4);
  {$EXTERNALSYM AVSP_USE_NONE}
  AVSP_USE_NONE    = APPLICATION_VIEW_SIZE_PREFERENCE(5);
  {$EXTERNALSYM AVSP_CUSTOM}

type
  FLYOUT_PLACEMENT = type Integer;
  {$EXTERNALSYM FLYOUT_PLACEMENT}
const
  FP_DEFAULT = FLYOUT_PLACEMENT(0);
  {$EXTERNALSYM FP_DEFAULT}
  FP_ABOVE   = FLYOUT_PLACEMENT(1);
  {$EXTERNALSYM FP_ABOVE}
  FP_BELOW   = FLYOUT_PLACEMENT(2);
  {$EXTERNALSYM FP_BELOW}
  FP_LEFT    = FLYOUT_PLACEMENT(3);
  {$EXTERNALSYM FP_LEFT}
  FP_RIGHT   = FLYOUT_PLACEMENT(4);
  {$EXTERNALSYM FP_RIGHT}

type
  SORT_ORDER_TYPE = type Integer;
  {$EXTERNALSYM SORT_ORDER_TYPE}
const
  SOT_DEFAULT            = SORT_ORDER_TYPE(0);
  {$EXTERNALSYM SOT_DEFAULT}
  SOT_IGNORE_FOLDERNESS  = SORT_ORDER_TYPE(1);
  {$EXTERNALSYM SOT_IGNORE_FOLDERNESS}

type
  PLIBRARYMANAGEDIALOGOPTIONS = ^LIBRARYMANAGEDIALOGOPTIONS;
  LIBRARYMANAGEDIALOGOPTIONS = type Integer;
  {$EXTERNALSYM LIBRARYMANAGEDIALOGOPTIONS}
const
  LMD_DEFAULT                          = LIBRARYMANAGEDIALOGOPTIONS($00000000);
  {$EXTERNALSYM LMD_DEFAULT}
  LMD_ALLOWUNINDEXABLENETWORKLOCATIONS = LIBRARYMANAGEDIALOGOPTIONS($00000001);  // The caller of SHShowManageLibraryUI does not want to show any warning dialogs about network locations that cannot be indexed
  {$EXTERNALSYM LMD_ALLOWUNINDEXABLENETWORKLOCATIONS}


type
  ASSOC_FILTER = type Integer;
  {$EXTERNALSYM ASSOC_FILTER}
const
  ASSOC_FILTER_NONE        = ASSOC_FILTER($0000);
  {$EXTERNALSYM ASSOC_FILTER_NONE}
  ASSOC_FILTER_RECOMMENDED = ASSOC_FILTER($0001);
  {$EXTERNALSYM ASSOC_FILTER_RECOMMENDED}


type
  // IAssocHandlerInvoker
  AHTYPE = type Integer;
  {$EXTERNALSYM AHTYPE}
const
  AHTYPE_UNDEFINED         = AHTYPE($0000);
  {$EXTERNALSYM AHTYPE_UNDEFINED}
  AHTYPE_USER_APPLICATION  = AHTYPE($0008);
  {$EXTERNALSYM AHTYPE_USER_APPLICATION}
  AHTYPE_ANY_APPLICATION   = AHTYPE($0010);
  {$EXTERNALSYM AHTYPE_ANY_APPLICATION}
  AHTYPE_MACHINEDEFAULT    = AHTYPE($0020);
  {$EXTERNALSYM AHTYPE_MACHINEDEFAULT}
  AHTYPE_PROGID            = AHTYPE($0040);
  {$EXTERNALSYM AHTYPE_PROGID}
  AHTYPE_APPLICATION       = AHTYPE($0080);
  {$EXTERNALSYM AHTYPE_APPLICATION}
  AHTYPE_CLASS_APPLICATION = AHTYPE($0100);
  {$EXTERNALSYM AHTYPE_CLASS_APPLICATION}
  AHTYPE_ANY_PROGID        = AHTYPE($0200);
  {$EXTERNALSYM AHTYPE_ANY_PROGID}


type
  PLPTBBUTTONSB = ^LPTBBUTTONSB;
  {$EXTERNALSYM LPTBBUTTONSB}
  LPTBBUTTONSB = LPARAM;



// Records =====================================================================

type
  // IBandSite
  PBANDSITEINFO = ^BANDSITEINFO;
  {$EXTERNALSYM tagBANDSITEINFO}
  tagBANDSITEINFO = record
    dwMask: DWORD;
    dwState: DWORD;
    dwStyle: DWORD;
  end;
  {$EXTERNALSYM BANDSITEINFO}
  BANDSITEINFO = tagBANDSITEINFO;

  // Record CMINVOKECOMMANDINFO
  // ==========================
  CMINVOKECOMMANDINFO = record
    cbSize: DWORD;
    fMask: DWORD;
    hwnd: HWND;
    lpVerb: LPCSTR;
    lpParameters: LPCSTR;
    lpDirectory: LPCSTR;
    nShow: Integer;
    dwHotKey: DWORD;
    hIcon: THANDLE;
  end;
  {$EXTERNALSYM CMINVOKECOMMANDINFO}
  PCMINVOKECOMMANDINFO = ^CMINVOKECOMMANDINFO;
  {$EXTERNALSYM PCMINVOKECOMMANDINFO}

  // Record CMINVOKECOMMANDINFOEX
  // ============================
  CMINVOKECOMMANDINFOEX = record
    cbSize: DWORD;
    fMask: DWORD;
    hwnd: HWND;
    lpVerb: LPCSTR;
    lpParameters: LPCSTR;
    lpDirectory: LPCSTR;
    nShow: Integer;
    dwHotKey: DWORD;
    hIcon: THANDLE;
    lpTitle: LPCSTR;
    lpVerbW: LPCWSTR;
    lpParametersW: LPCWSTR;
    lpDirectoryW: LPCWSTR;
    lpTitleW: LPCWSTR;
    ptInvoke: POINT;
  end;
  {$EXTERNALSYM CMINVOKECOMMANDINFOEX}
  PCMINVOKECOMMANDINFOEX = ^CMINVOKECOMMANDINFOEX;
  {$EXTERNALSYM PCMINVOKECOMMANDINFOEX}

  // Record CMINVOKECOMMANDINFOEX_REMOTE
  // ===================================
  CMINVOKECOMMANDINFOEX_REMOTE = record
    cbSize: DWORD;
    fMask: DWORD;
    hwnd: HWND;
    lpVerbString: LPCSTR;
    lpParameters: LPCSTR;
    lpDirectory: LPCSTR;
    nShow: Integer;
    dwHotKey: DWORD;
    lpTitle: LPCSTR;
    lpVerbWString: LPCWSTR;
    lpParametersW: LPCWSTR;
    lpDirectoryW: LPCWSTR;
    lpTitleW: LPCWSTR;
    ptInvoke: POINT;
    lpVerbInt: UINT;
    lpVerbWInt: UINT;
  end;
  {$EXTERNALSYM CMINVOKECOMMANDINFOEX_REMOTE}
  PCMINVOKECOMMANDINFOEX_REMOTE = ^CMINVOKECOMMANDINFOEX_REMOTE;
  {$EXTERNALSYM PCMINVOKECOMMANDINFOEX_REMOTE}

  // Record PERSIST_FOLDER_TARGET_INFO
  // =================================
  PERSIST_FOLDER_TARGET_INFO = record
    pidlTargetFolder: PIDLIST_ABSOLUTE;
    szTargetParsingName: array[0..260 - 1] of WideChar;
    szNetworkProvider: array[0..260 - 1] of WideChar;
    dwAttributes: DWORD;
    csidl: Integer;
  end;
  {$EXTERNALSYM PERSIST_FOLDER_TARGET_INFO}
  PPERSIST_FOLDER_TARGET_INFO = ^PERSIST_FOLDER_TARGET_INFO;
  {$EXTERNALSYM PPERSIST_FOLDER_TARGET_INFO}

  // Record EXTRASEARCH
  // ==================
  EXTRASEARCH = record
    guidSearch: TGUID;
    wszFriendlyName: array[0..80 - 1] of WideChar;
    wszUrl: array[0..2084 - 1] of WideChar;
  end;
  {$EXTERNALSYM EXTRASEARCH}
  PEXTRASEARCH = ^EXTRASEARCH;
  {$EXTERNALSYM PEXTRASEARCH}

  // Record FOLDERSETTINGS
  // =====================
  FOLDERSETTINGS = record
    ViewMode: UINT;
    fFlags: UINT;
  end;
  {$EXTERNALSYM FOLDERSETTINGS}
  PFOLDERSETTINGS = ^FOLDERSETTINGS;
  {$EXTERNALSYM PFOLDERSETTINGS}
  LPFOLDERSETTINGS = ^FOLDERSETTINGS;
  {$EXTERNALSYM LPFOLDERSETTINGS}
  LPCFOLDERSETTINGS = ^FOLDERSETTINGS;
  {$EXTERNALSYM LPCFOLDERSETTINGS}


  // Record SORTCOLUMN
  // =================
  SORTCOLUMN = record
    propkey: PROPERTYKEY;
    direction: SORTDIRECTION;
  end;
  {$EXTERNALSYM SORTCOLUMN}
  PSORTCOLUMN = ^SORTCOLUMN;
  {$EXTERNALSYM PSORTCOLUMN}

  // Record CM_COLUMNINFO
  // ====================
  CM_COLUMNINFO = record
    cbSize: DWORD;
    dwMask: DWORD;
    dwState: DWORD;
    uWidth: UINT;
    uDefaultWidth: UINT;
    uIdealWidth: UINT;
    wszName: array[0..80 - 1] of WideChar;
  end;
  {$EXTERNALSYM CM_COLUMNINFO}
  PCM_COLUMNINFO = ^CM_COLUMNINFO;
  {$EXTERNALSYM PCM_COLUMNINFO}

  // Record SHELL_ITEM_RESOURCE
  // ==========================
  SHELL_ITEM_RESOURCE = record
    guidType: TGUID;
    szName: array[0..260 - 1] of WideChar;
  end;
  {$EXTERNALSYM SHELL_ITEM_RESOURCE}
  PSHELL_ITEM_RESOURCE = ^SHELL_ITEM_RESOURCE;
  {$EXTERNALSYM PSHELL_ITEM_RESOURCE}

  // Record CATEGORY_INFO
  // ====================
  CATEGORY_INFO = record
    cif: CATEGORYINFO_FLAGS;
    wszName: array[0..260 - 1] of WideChar;
  end;
  {$EXTERNALSYM CATEGORY_INFO}
  PCATEGORY_INFO = ^CATEGORY_INFO;
  {$EXTERNALSYM PCATEGORY_INFO}

  // Record SHDRAGIMAGE
  // ==================
  SHDRAGIMAGE = record
    sizeDragImage: SIZE;
    ptOffset: POINT;
    hbmpDragImage: HBITMAP;
    crColorKey: COLORREF;
  end;
  {$EXTERNALSYM SHDRAGIMAGE}
  PSHDRAGIMAGE = ^SHDRAGIMAGE;
  {$EXTERNALSYM PSHDRAGIMAGE}
  LPSHDRAGIMAGE = PSHDRAGIMAGE;
  {$EXTERNALSYM LPSHDRAGIMAGE}


  // Record DESKBANDINFO
  // ===================
  DESKBANDINFO = record
    dwMask: DWORD;
    ptMinSize: POINTL;
    ptMaxSize: POINTL;
    ptIntegral: POINTL;
    ptActual: POINTL;
    wszTitle: array[0..256 - 1] of WideChar;
    dwModeFlags: DWORD;
    crBkgnd: COLORREF;
  end;
  {$EXTERNALSYM DESKBANDINFO}
  PDESKBANDINFO = ^DESKBANDINFO;
  {$EXTERNALSYM PDESKBANDINFO}


type
  PTHUMBBUTTONFLAGS = ^THUMBBUTTONFLAGS;
  THUMBBUTTONFLAGS      = UINT;
  {$EXTERNALSYM THUMBBUTTONFLAGS}
const
  THBF_ENABLED        = THUMBBUTTONFLAGS(0);
  {$EXTERNALSYM THBF_ENABLED}
  THBF_DISABLED       = THUMBBUTTONFLAGS($1);
  {$EXTERNALSYM THBF_DISABLED}
  THBF_DISMISSONCLICK = THUMBBUTTONFLAGS($2);
  {$EXTERNALSYM THBF_DISMISSONCLICK}
  THBF_NOBACKGROUND   = THUMBBUTTONFLAGS($4);
  {$EXTERNALSYM THBF_NOBACKGROUND}
  THBF_HIDDEN         = THUMBBUTTONFLAGS($8);
  {$EXTERNALSYM THBF_HIDDEN}
  THBF_NONINTERACTIVE = THUMBBUTTONFLAGS($10);
  {$EXTERNALSYM THBF_NONINTERACTIVE}

type
  // DEFINE_ENUM_FLAG_OPERATORS(THUMBBUTTONFLAGS)
  PTHUMBBUTTONMASK = ^THUMBBUTTONMASK;
  THUMBBUTTONMASK =  UINT;
  {$EXTERNALSYM THUMBBUTTONMASK}
const
  THB_BITMAP  = THUMBBUTTONMASK($1);
  {$EXTERNALSYM THB_BITMAP}
  THB_ICON    = THUMBBUTTONMASK($2);
  {$EXTERNALSYM THB_ICON}
  THB_TOOLTIP = THUMBBUTTONMASK($4);
  {$EXTERNALSYM THB_TOOLTIP}
  THB_FLAGS   = THUMBBUTTONMASK($8);
  {$EXTERNALSYM THB_FLAGS}

type
  PSTPFLAG = ^STPFLAG;
  STPFLAG = DWORD;
  {$EXTERNALSYM STPFLAG}
const
  STPF_NONE                      = STPFLAG(0);
  {$EXTERNALSYM STPF_NONE}
  STPF_USEAPPTHUMBNAILALWAYS     = STPFLAG($1);
  {$EXTERNALSYM STPF_USEAPPTHUMBNAILALWAYS}
  STPF_USEAPPTHUMBNAILWHENACTIVE = STPFLAG($2);
  {$EXTERNALSYM STPF_USEAPPTHUMBNAILWHENACTIVE}
  STPF_USEAPPPEEKALWAYS          = STPFLAG($4);
  {$EXTERNALSYM STPF_USEAPPPEEKALWAYS}
  STPF_USEAPPPEEKWHENACTIVE      = STPFLAG($8);
  {$EXTERNALSYM STPF_USEAPPPEEKWHENACTIVE}

//obsolete?
type
  PEXPLORER_BROWSER_OPTIONS = ^EXPLORER_BROWSER_OPTIONS;
  EXPLORER_BROWSER_OPTIONS = DWORD;
  {$EXTERNALSYM EXPLORER_BROWSER_OPTIONS}
const
  EBO_NONE               = EXPLORER_BROWSER_OPTIONS(0);
  {$EXTERNALSYM EBO_NONE}
  EBO_NAVIGATEONCE       = EXPLORER_BROWSER_OPTIONS($1);
  {$EXTERNALSYM EBO_NAVIGATEONCE}
  EBO_SHOWFRAMES         = EXPLORER_BROWSER_OPTIONS($2);
  {$EXTERNALSYM EBO_SHOWFRAMES}
  EBO_ALWAYSNAVIGATE     = EXPLORER_BROWSER_OPTIONS($4);
  {$EXTERNALSYM EBO_ALWAYSNAVIGATE}
  EBO_NOTRAVELLOG        = EXPLORER_BROWSER_OPTIONS($8);
  {$EXTERNALSYM EBO_NOTRAVELLOG}
  EBO_NOWRAPPERWINDOW    = EXPLORER_BROWSER_OPTIONS($10);
  {$EXTERNALSYM EBO_NOWRAPPERWINDOW}
  EBO_HTMLSHAREPOINTVIEW = EXPLORER_BROWSER_OPTIONS($20);
  {$EXTERNALSYM EBO_HTMLSHAREPOINTVIEW}
  EBO_NOBORDER           = EXPLORER_BROWSER_OPTIONS($40);
  {$EXTERNALSYM EBO_NOBORDER}
  EBO_NOPERSISTVIEWSTATE = EXPLORER_BROWSER_OPTIONS($80);
  {$EXTERNALSYM EBO_NOPERSISTVIEWSTATE}

type
  PEXPLORER_BROWSER_FILL_FLAGS = ^EXPLORER_BROWSER_FILL_FLAGS;
  EXPLORER_BROWSER_FILL_FLAGS =  DWORD;
  {$EXTERNALSYM EXPLORER_BROWSER_FILL_FLAGS}
const
  EBF_NONE                 = EXPLORER_BROWSER_FILL_FLAGS(0);
  {$EXTERNALSYM EBF_NONE}
  EBF_SELECTFROMDATAOBJECT = EXPLORER_BROWSER_FILL_FLAGS($100);
  {$EXTERNALSYM EBF_SELECTFROMDATAOBJECT}
  EBF_NODROPTARGET         = EXPLORER_BROWSER_FILL_FLAGS($200);
  {$EXTERNALSYM EBF_NODROPTARGET}


type

  // Record THUMBBUTTON
  // ==================
  THUMBBUTTON = record
    dwMask: THUMBBUTTONMASK;
    iId: UINT;
    iBitmap: UINT;
    hIcon: HICON;
    szTip: array[0..260 - 1] of WideChar;
    dwFlags: THUMBBUTTONFLAGS;
  end;
  {$EXTERNALSYM THUMBBUTTON}
  PTHUMBBUTTON = ^THUMBBUTTON;
  {$EXTERNALSYM PTHUMBBUTTON}
  LPTHUMBBUTTON = PTHUMBBUTTON;
  {$EXTERNALSYM LPTHUMBBUTTON}

  // Record DELEGATEITEMID
  // =====================
  DELEGATEITEMID = record
    cbSize: WORD;
    wOuter: WORD;
    cbInner: WORD;
    rgb: array[0..1 - 1] of BYTE;
  end;
  {$EXTERNALSYM DELEGATEITEMID}
  PDELEGATEITEMID = ^DELEGATEITEMID;
  {$EXTERNALSYM PDELEGATEITEMID}


  // Record SMINFO
  // =============
  SMINFO = record
    dwMask: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    iIcon: Integer;
  end;
  {$EXTERNALSYM SMINFO}
  PSMINFO = ^SMINFO;
  {$EXTERNALSYM PSMINFO}

  // Record SMCSHCHANGENOTIFYSTRUCT
  // ==============================
  SMCSHCHANGENOTIFYSTRUCT = record
    lEvent: long;
    pidl1: PCIDLIST_ABSOLUTE;
    pidl2: PCIDLIST_ABSOLUTE;
  end;
  {$EXTERNALSYM SMCSHCHANGENOTIFYSTRUCT}
  PSMCSHCHANGENOTIFYSTRUCT = ^SMCSHCHANGENOTIFYSTRUCT;
  {$EXTERNALSYM PSMCSHCHANGENOTIFYSTRUCT}

  // Record KNOWNFOLDER_DEFINITION
  // =============================
  KNOWNFOLDER_DEFINITION = record
    category: KF_CATEGORY;
    pszName: LPWSTR;
    pszDescription: LPWSTR;
    fidParent: KNOWNFOLDERID;
    pszRelativePath: LPWSTR;
    pszParsingName: LPWSTR;
    pszTooltip: LPWSTR;
    pszLocalizedName: LPWSTR;
    pszIcon: LPWSTR;
    pszSecurity: LPWSTR;
    dwAttributes: DWORD;
    kfdFlags: KF_DEFINITION_FLAGS;
    ftidType: FOLDERTYPEID;
  end;
  {$EXTERNALSYM KNOWNFOLDER_DEFINITION}
  PKNOWNFOLDER_DEFINITION = ^KNOWNFOLDER_DEFINITION;
  {$EXTERNALSYM PKNOWNFOLDER_DEFINITION}

  // Record PREVIEWHANDLERFRAMEINFO
  // ==============================
  PREVIEWHANDLERFRAMEINFO = record
    haccel: HACCEL;
    cAccelEntries: UINT;
  end;
  {$EXTERNALSYM PREVIEWHANDLERFRAMEINFO}
  PPREVIEWHANDLERFRAMEINFO = ^PREVIEWHANDLERFRAMEINFO;
  {$EXTERNALSYM PPREVIEWHANDLERFRAMEINFO}

  // Record BANNER_NOTIFICATION
  // ==========================
  BANNER_NOTIFICATION = record
    event: BANNER_NOTIFICATION_EVENT;
    providerIdentity: LPCWSTR;
    contentId: LPCWSTR;
  end;
  {$EXTERNALSYM BANNER_NOTIFICATION}
  PBANNER_NOTIFICATION = ^BANNER_NOTIFICATION;
  {$EXTERNALSYM PBANNER_NOTIFICATION}

  LPCMINVOKECOMMANDINFO = PCMINVOKECOMMANDINFO;
  {$EXTERNALSYM LPCMINVOKECOMMANDINFO}
  PCCMINVOKECOMMANDINFO = PCMINVOKECOMMANDINFO;
  {$EXTERNALSYM PCCMINVOKECOMMANDINFO}
  LPCMINVOKECOMMANDINFOEX = PCMINVOKECOMMANDINFOEX;
  {$EXTERNALSYM LPCMINVOKECOMMANDINFOEX}
  PCCMINVOKECOMMANDINFOEX = PCMINVOKECOMMANDINFOEX;
  {$EXTERNALSYM PCCMINVOKECOMMANDINFOEX}

  LPCONTEXTMENU = ^IContextMenu;
  {$EXTERNALSYM LPCONTEXTMENU}
  LPCONTEXTMENU2 = ^IContextMenu2;
  {$EXTERNALSYM LPCONTEXTMENU2}
  LPCONTEXTMENU3 = ^IContextMenu3;
  {$EXTERNALSYM LPCONTEXTMENU3}
  LPPERSISTFOLDER = ^IPersistFolder;
  {$EXTERNALSYM LPPERSISTFOLDER}
  LPENUMIDLIST = ^IEnumIDList;
  {$EXTERNALSYM LPENUMIDLIST}
  LPSHELLFOLDER = ^IShellFolder;
  {$EXTERNALSYM LPSHELLFOLDER}
  LPENUMEXTRASEARCH = ^IEnumExtraSearch;
  {$EXTERNALSYM LPENUMEXTRASEARCH}

  LPCOMMDLGBROWSER = ^ICommDlgBrowser;
  {$EXTERNALSYM LPCOMMDLGBROWSER}
  LPCOMMDLGBROWSER2 = ^ICommDlgBrowser2;
  {$EXTERNALSYM LPCOMMDLGBROWSER2}
  LPSHELLBROWSER = ^IShellBrowser;
  {$EXTERNALSYM LPSHELLBROWSER}
  LPSTGTRANSCONFIRMATION = PGUID;
  {$EXTERNALSYM LPSTGTRANSCONFIRMATION}
  LPSHELLEXTINIT = ^IShellExtInit;
  {$EXTERNALSYM LPSHELLEXTINIT}
  LPSHELLPROPSHEETEXT = ^IShellPropSheetExt;
  {$EXTERNALSYM LPSHELLPROPSHEETEXT}
  LPEXTRACTIMAGE = ^IExtractImage;
  {$EXTERNALSYM LPEXTRACTIMAGE}
  LPEXTRACTIMAGE2 = ^IExtractImage2;
  {$EXTERNALSYM LPEXTRACTIMAGE2}
  HIMAGELIST = PIUnknown;
  {$EXTERNALSYM HIMAGELIST}
  PZZWSTR = PWideChar;
  {$EXTERNALSYM PZZWSTR}


  IContextMenu = interface;
  IContextMenu2 = interface;
  IContextMenu3 = interface;
  IStaticVerbProvider = interface;
  IExecuteCommand = interface;
  IPersistFolder = interface;
  IRunnableTask = interface;
  IShellTaskScheduler = interface;
  IPersistFolder2 = interface;
  IPersistFolder3 = interface;
  IPersistIDList = interface;
  IEnumIDList = interface;
  IEnumFullIDList = interface;
  IFileSyncMergeHandler = interface;
  IObjectWithFolderEnumMode = interface;
  IParseAndCreateItem = interface;
  IShellFolder = interface;
  IShellBrowser = interface;
  IEnumExtraSearch = interface;
  IShellFolder2 = interface;
  IShellView = interface;
  LPSHELLVIEW = ^IShellView;
  {$EXTERNALSYM LPSHELLVIEW}
  IShellView2 = interface;
  IFolderView = interface;
  IFolderView2 = interface;
  IFolderViewSettings = interface;
  IInitializeNetworkFolder = interface;
  INetworkFolderInternal = interface;
  IPreviewHandlerVisuals = interface;
  ICommDlgBrowser = interface;
  ICommDlgBrowser2 = interface;
  IColumnManager = interface;
  IFolderFilterSite = interface;
  IFolderFilter = interface;
  IInputObjectSite = interface;
  IInputObject = interface;
  IInputObject2 = interface;
  IShellIcon = interface;
  //IShellBrowser = interface;
  IProfferService = interface;
  IGetServiceIds = interface;
  IShellItem = interface;
  PIShellItem = ^IShellItem;
  {$EXTERNALSYM PIShellItem}
  IShellItem2 = interface;
  IShellItemImageFactory = interface;
  IEnumShellItems = interface;
  ITransferAdviseSink = interface;
  ITransferSource = interface;
  IEnumResources = interface;
  IShellItemResources = interface;
  ITransferDestination = interface;
  IFileOperationProgressSink = interface;
  IShellItemArray = interface;
  IInitializeWithItem = interface;
  IObjectWithSelection = interface;
  IObjectWithBackReferences = interface;
  IPropertyUI = interface;
  ICategoryProvider = interface;
  ICategorizer = interface;
  IDropTargetHelper = interface;
  IDragSourceHelper = interface;
  IShellLinkA = interface;
  IShellLinkW = interface;
  IShellLinkDataList = interface;
  IResolveShellLink = interface;
  IActionProgressDialog = interface;
  IActionProgress = interface;
  IShellExtInit = interface;
  IShellPropSheetExt = interface;
  IRemoteComputer = interface;
  IQueryContinue = interface;
  IObjectWithCancelEvent = interface;
  IUserNotification = interface;
  IItemNameLimits = interface;
  ISearchFolderItemFactory = interface;
  IExtractImage = interface;
  IExtractImage2 = interface;
  IThumbnailHandlerFactory = interface;
  IParentAndItem = interface;
  IDockingWindow = interface;
  IDeskBand = interface;
  IDeskBandInfo = interface;
  ITaskbarList = interface;
  ITaskbarList2 = interface;
  ITaskbarList3 = interface;
  ITaskbarList4 = interface;
  IExplorerBrowserEvents = interface;
  IExplorerBrowser = interface;
  IEnumObjects = interface;
  IOperationsProgressDialog = interface;
  IIOCancelInformation = interface;
  IFileOperation = interface;
  IFileOperation2 = interface;
  IObjectProvider = interface;
  INamespaceWalkCB = interface;
  INamespaceWalkCB2 = interface;
  INamespaceWalk = interface;
  IBandSite = interface;
  IModalWindow = interface;
  IContextMenuSite = interface;
  IMenuBand = interface;
  IRegTreeItem = interface;
  IDeskBar = interface;
  IMenuPopup = interface;
  IFileIsInUse = interface;
  IFileDialogEvents = interface;
  IFileDialog = interface;
  IFileSaveDialog = interface;
  IFileOpenDialog = interface;
  IFileDialogCustomize = interface;
  IApplicationAssociationRegistration = interface;
  IDelegateFolder = interface;
  IBrowserFrameOptions = interface;
  INewWindowManager = interface;
  IAttachmentExecute = interface;
  IAttachmentExecute2 = interface;
  IShellMenuCallback = interface;
  IShellMenu = interface;
  IKnownFolder = interface;
  IKnownFolderManager = interface;
  ISharingConfigurationManager = interface;
  IRelatedItem = interface;
  IIdentityName = interface;
  IDelegateItem = interface;
  ICurrentItem = interface;
  ITransferMediumItem = interface;
  IDisplayItem = interface;
  IViewStateIdentityItem = interface;
  IPreviewItem = interface;
  IDestinationStreamFactory = interface;
  ICreateProcessInputs = interface;
  ICreatingProcess = interface;
  ILaunchUIContext = interface;
  ILaunchUIContextProvider = interface;
  INewMenuClient = interface;
  IInitializeWithBindCtx = interface;
  IShellItemFilter = interface;
  INameSpaceTreeControl = interface;
  INameSpaceTreeControlFolderCapabilities = interface;
  IPreviewHandler = interface;
  IPreviewHandlerFrame = interface;
  IExplorerPaneVisibility = interface;
  IContextMenuCB = interface;
  IDefaultExtractIconInit = interface;
  IExplorerCommand = interface;
  PIExplorerCommand = ^IExplorerCommand;
  IExplorerCommandState = interface;
  IInitializeCommand = interface;
  IEnumExplorerCommand = interface;
  IExplorerCommandProvider = interface;
  IOpenControlPanel = interface;
  IFileSystemBindData = interface;
  IFileSystemBindData2 = interface;
  ICustomDestinationList = interface;
  IApplicationDestinations = interface;
  IApplicationDocumentLists = interface;
  IObjectWithAppUserModelID = interface;
  IObjectWithPackageFullName = interface;
  IObjectWithProgID = interface;
  IUpdateIDList = interface;
  IDesktopWallpaper = interface;
  IHomeGroup = interface;
  IInitializeWithPropertyStore = interface;
  IOpenSearchSource = interface;
  IShellLibrary = interface;
  IDefaultFolderMenuInitialize = interface;
  IApplicationActivationManager = interface;
  IVirtualDesktopManager = interface;
  IAssocHandlerInvoker = interface;
  IAssocHandler = interface;
  IEnumAssocHandlers = interface;
  IDataObjectProvider = interface;
  IDataTransferManagerInterop = interface;
  IFrameworkInputPaneHandler = interface;
  IFrameworkInputPane = interface;
  IAppVisibilityEvents = interface;
  IAppVisibility = interface;
  IPackageExecutionStateChangeNotification = interface;
  IPackageDebugSettings = interface;
  IPackageDebugSettings2 = interface;
  ISuspensionDependencyManager = interface;
  IExecuteCommandApplicationHostEnvironment = interface;
  IExecuteCommandHost = interface;
  IApplicationDesignModeSettings = interface;
  IApplicationDesignModeSettings2 = interface;
  ILaunchTargetMonitor = interface;
  ILaunchSourceViewSizePreference = interface;
  ILaunchTargetViewSizePreference = interface;
  ILaunchSourceAppUserModelId = interface;
  IInitializeWithWindow = interface;
  IHandlerInfo = interface;
  IHandlerInfo2 = interface;
  IHandlerActivationHost = interface;
  IAppActivationUIInfo = interface;
  IContactManagerInterop = interface;
  IShellIconOverlayIdentifier = interface;
  IBannerNotificationHandler = interface;
  ISortColumnArray = interface;
  IPropertyKeyStore = interface;



  PSV2CVW2_PARAMS = ^SV2CVW2_PARAMS;
  LPSV2CVW2_PARAMS = PSV2CVW2_PARAMS;
  {$EXTERNALSYM LPSV2CVW2_PARAMS}

  SV2CVW2_PARAMS = record
    cbSize: DWORD;
    psvPrev: IShellView;
    pfs: LPCFOLDERSETTINGS;
    psbOwner: IShellBrowser;
    prcView: PRECT;
    pvid: ^SHELLVIEWID;
    hwndView: HWND;
  end;
  {$EXTERNALSYM SV2CVW2_PARAMS}


  // Interface IContextMenu
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IContextMenu);'}
  {$EXTERNALSYM IContextMenu}
  IContextMenu = interface(IUnknown)
    ['{000214E4-0000-0000-C000-000000000046}']

    function QueryContextMenu(hmenu: HMENU;
                     indexMenu: UINT;
                     idCmdFirst: UINT;
                     idCmdLast: UINT;
                     uFlags: UINT): HRESULT; stdcall;

    function InvokeCommand(pici: PCMINVOKECOMMANDINFO): HRESULT; stdcall;

    function GetCommandString(idCmd: UINT_PTR;
                     uType: UINT;
                     pReserved: PUINT;
                     pszName: PAnsiChar;
                     cchMax: UINT): HRESULT; stdcall;
  end;
  IID_IContextMenu = IContextMenu;
  {$EXTERNALSYM IID_IContextMenu}

  // Interface IContextMenu2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IContextMenu2);'}
  {$EXTERNALSYM IContextMenu2}
  IContextMenu2 = interface(IContextMenu)
    ['{000214F4-0000-0000-C000-000000000046}']

    function HandleMenuMsg(uMsg: UINT;
                     wParam: WPARAM;
                     lParam: LPARAM): HRESULT; stdcall;
  end;
  IID_IContextMenu2 = IContextMenu2;
  {$EXTERNALSYM IID_IContextMenu2}

  // Interface IContextMenu3
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IContextMenu3);'}
  {$EXTERNALSYM IContextMenu3}
  IContextMenu3 = interface(IContextMenu2)
    ['{BCFCE0A0-EC17-11D0-8D10-00A0C90F2719}']

    function HandleMenuMsg2(uMsg: UINT;
                            wParam: WPARAM;
                            lParam: LPARAM;
                            plResult: PLRESULT): HRESULT; stdcall;
  end;
  IID_IContextMenu3 = IContextMenu3;
  {$EXTERNALSYM IID_IContextMenu3}

  // Interface IStaticVerbProvider
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStaticVerbProvider);'}
  {$EXTERNALSYM IStaticVerbProvider}
  IStaticVerbProvider = interface(IUnknown)
    ['{4B770DA6-D111-4015-96FD-8C1C56F06C55}']

    function IsVerbSupported(verbName: LPCWSTR;
                             out result: BOOL): HRESULT; stdcall;
  end;
  IID_IStaticVerbProvider = IStaticVerbProvider;
  {$EXTERNALSYM IID_IStaticVerbProvider}

  // Interface IExecuteCommand
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExecuteCommand);'}
  {$EXTERNALSYM IExecuteCommand}
  IExecuteCommand = interface(IUnknown)
    ['{7F9185B0-CB92-43C5-80A9-92277A4F7B54}']

    function SetKeyState(grfKeyState: DWORD): HRESULT; stdcall;

    function SetParameters(pszParameters: LPCWSTR): HRESULT; stdcall;

    function SetPosition(pt: POINT): HRESULT; stdcall;

    function SetShowWindow(nShow: Integer): HRESULT; stdcall;

    function SetNoShowUI(fNoShowUI: BOOL): HRESULT; stdcall;

    function SetDirectory(pszDirectory: LPCWSTR): HRESULT; stdcall;

    function Execute: HRESULT; stdcall;
  end;
  IID_IExecuteCommand = IExecuteCommand;
  {$EXTERNALSYM IID_IExecuteCommand}

  // Interface IPersistFolder
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistFolder);'}
  {$EXTERNALSYM IPersistFolder}
  IPersistFolder = interface(IPersist)
    ['{000214EA-0000-0000-C000-000000000046}']

    function Initialize(pidl: PCIDLIST_ABSOLUTE): HRESULT; stdcall;
  end;
  IID_IPersistFolder = IPersistFolder;
  {$EXTERNALSYM IID_IPersistFolder}


  // Interface IRunnableTask
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRunnableTask);'}
  {$EXTERNALSYM IRunnableTask}
  IRunnableTask = interface(IUnknown)
    ['{85788D00-6807-11D0-B810-00C04FD706EC}']

    function Run: HRESULT; stdcall;

    function Kill(bWait: BOOL): HRESULT; stdcall;

    function Suspend: HRESULT; stdcall;

    function Resume: HRESULT; stdcall;

    function IsRunning: ULONG; stdcall;
  end;
  IID_IRunnableTask = IRunnableTask;
  {$EXTERNALSYM IID_IRunnableTask}


  // Interface IShellTaskScheduler
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellTaskScheduler);'}
  {$EXTERNALSYM IShellTaskScheduler}
  IShellTaskScheduler = interface(IUnknown)
    ['{6CCB7BE0-6807-11D0-B810-00C04FD706EC}']

    function AddTask(prt: IRunnableTask;
                     rtoid: REFTASKOWNERID;
                     lParam: DWORD_PTR;
                     dwPriority: DWORD): HRESULT; stdcall;

    function RemoveTasks(rtoid: REFTASKOWNERID;
                     lParam: DWORD_PTR;
                     bWaitIfRunning: BOOL): HRESULT; stdcall;

    function CountTasks(rtoid: REFTASKOWNERID): UINT; stdcall;

    function Status(dwReleaseStatus: DWORD;
                     dwThreadTimeout: DWORD): HRESULT; stdcall;
  end;
  IID_IShellTaskScheduler = IShellTaskScheduler;
  {$EXTERNALSYM IID_IShellTaskScheduler}


  // Interface IPersistFolder2
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistFolder2);'}
  {$EXTERNALSYM IPersistFolder2}
  IPersistFolder2 = interface(IPersistFolder)
    ['{1AC3D9F0-175C-11D1-95BE-00609797EA4F}']

    function GetCurFolder(ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;
  end;
  IID_IPersistFolder2 = IPersistFolder2;
  {$EXTERNALSYM IID_IPersistFolder2}


  // Interface IPersistFolder3
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistFolder3);'}
  {$EXTERNALSYM IPersistFolder3}
  IPersistFolder3 = interface(IPersistFolder2)
    ['{CEF04FDF-FE72-11D2-87A5-00C04F6837CF}']

    function InitializeEx(pbc: IBindCtx;
                          pidlRoot: PCIDLIST_ABSOLUTE;
                    {out} ppfti: PERSIST_FOLDER_TARGET_INFO): HRESULT; stdcall;

    function GetFolderTargetInfo(ppfti: PPERSIST_FOLDER_TARGET_INFO): HRESULT; stdcall;
  end;
  IID_IPersistFolder3 = IPersistFolder3;
  {$EXTERNALSYM IID_IPersistFolder3}


  // Interface IPersistIDList
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPersistIDList);'}
  {$EXTERNALSYM IPersistIDList}
  IPersistIDList = interface(IPersist)
    ['{1079ACFC-29BD-11D3-8E0D-00C04F6837D5}']

    function SetIDList(pidl: PCIDLIST_ABSOLUTE): HRESULT; stdcall;

    function GetIDList(ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;
  end;
  IID_IPersistIDList = IPersistIDList;
  {$EXTERNALSYM IID_IPersistIDList}

  // Interface IEnumIDList
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumIDList);'}
  {$EXTERNALSYM IEnumIDList}
  IEnumIDList = interface(IUnknown)
    ['{000214F2-0000-0000-C000-000000000046}']

    function Next(celt: ULONG;
                     rgelt: PITEMID_CHILD;
                     pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset: HRESULT; stdcall;

    function Clone(out ppenum: IEnumIDList): HRESULT; stdcall;
  end;
  IID_IEnumIDList = IEnumIDList;
  {$EXTERNALSYM IID_IEnumIDList}

  // Interface IEnumFullIDList
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumFullIDList);'}
  {$EXTERNALSYM IEnumFullIDList}
  IEnumFullIDList = interface(IUnknown)
    ['{D0191542-7954-4908-BC06-B2360BBE45BA}']

    function Next(celt: ULONG;
                     rgelt: PIDLIST_ABSOLUTE;
                     pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset: HRESULT; stdcall;

    function Clone(out ppenum: IEnumFullIDList): HRESULT; stdcall;
  end;
  IID_IEnumFullIDList = IEnumFullIDList;
  {$EXTERNALSYM IID_IEnumFullIDList}

  // Interface IFileSyncMergeHandler
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileSyncMergeHandler);'}
  {$EXTERNALSYM IFileSyncMergeHandler}
  IFileSyncMergeHandler = interface(IUnknown)
    ['{D97B5AAC-C792-433C-975D-35C4EADC7A9D}']

    function Merge(localFilePath: LPCWSTR;
                   serverFilePath: LPCWSTR;
             {out} updateStatus: MERGE_UPDATE_STATUS): HRESULT; stdcall;

    function ShowResolveConflictUIAsync(localFilePath: LPCWSTR;
                                        monitorToDisplayOn: HMONITOR): HRESULT; stdcall;
  end;
  IID_IFileSyncMergeHandler = IFileSyncMergeHandler;
  {$EXTERNALSYM IID_IFileSyncMergeHandler}

  // Interface IObjectWithFolderEnumMode
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithFolderEnumMode);'}
  {$EXTERNALSYM IObjectWithFolderEnumMode}
  IObjectWithFolderEnumMode = interface(IUnknown)
    ['{6A9D9026-0E6E-464C-B000-42ECC07DE673}']

    function SetMode(feMode: FOLDER_ENUM_MODE): HRESULT; stdcall;

    function GetMode(pfeMode: FOLDER_ENUM_MODE): HRESULT; stdcall;
  end;
  IID_IObjectWithFolderEnumMode = IObjectWithFolderEnumMode;
  {$EXTERNALSYM IID_IObjectWithFolderEnumMode}

  // Interface IParseAndCreateItem
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IParseAndCreateItem);'}
  {$EXTERNALSYM IParseAndCreateItem}
  IParseAndCreateItem = interface(IUnknown)
    ['{67EFED0E-E827-4408-B493-78F3982B685C}']

    function SetItem(psi: IShellItem): HRESULT; stdcall;

    function GetItem(const riid: REFIID;
                     out ppv: Pointer): HRESULT; stdcall;
  end;
  IID_IParseAndCreateItem = IParseAndCreateItem;
  {$EXTERNALSYM IID_IParseAndCreateItem}


  // Interface IShellFolder
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellFolder);'}
  {$EXTERNALSYM IShellFolder}
  IShellFolder = interface(IUnknown)
    ['{000214E6-0000-0000-C000-000000000046}']

    function ParseDisplayName(hwnd: HWND;
                              pbc: IBindCtx;
                              pszDisplayName: LPWSTR;
                              pchEaten: PULONG;
                              out ppidl: PIDLIST_RELATIVE;
                              pdwAttributes: PULONG): HRESULT; stdcall;

    function EnumObjects(hwnd: HWND;
                         grfFlags: SHCONTF;
                         out ppenumIDList: IEnumIDList): HRESULT; stdcall;

    // returns an instance of a sub-folder which is specified by the IDList (pidl).
    // IShellFolder or derived interfaces
    function BindToObject(pidl: PCUIDLIST_RELATIVE;
                          pbc: IBindCtx;
                          const riid: TGUID;
                          out ppv): HRESULT; stdcall;

    // produces the same result as BindToObject()
    function BindToStorage(pidl: PCUIDLIST_RELATIVE;
                           pbc: IBindCtx;
                           const riid: TGUID;
                           out ppv): HRESULT; stdcall;

    // compares two IDLists and returns the result. The shell
    // explorer always passes 0 as lParam, which indicates 'sort by name'.
    // It should return 0 (as CODE of the scode), if two id indicates the
    // same object; negative value if pidl1 should be placed before pidl2;
    // positive value if pidl2 should be placed before pidl1.
    // use the macro ResultFromShort() to extract the result comparison
    // it deals with the casting and type conversion issues for you
    function CompareIDs(lParam: LPARAM;
                        pidl1: PCUIDLIST_RELATIVE;
                        pidl2: PCUIDLIST_RELATIVE): HRESULT; stdcall;

    // creates a view object of the folder itself. The view
    // object is a difference instance from the shell folder object.
    // 'hwndOwner' can be used  as the owner window of its dialog box or
    // menu during the lifetime of the view object.
    // This member function should always create a new
    // instance which has only one reference count. The explorer may create
    // more than one instances of view object from one shell folder object
    // and treat them as separate instances.
    // returns IShellView derived interface
    function CreateViewObject(hwndOwner: HWND;
                              const riid: TGUID;
                              out ppv): HRESULT; stdcall;

    // returns the attributes of specified objects in that
    // folder. 'cidl' and 'apidl' specifies objects. 'apidl' contains only
    // simple IDLists. The explorer initializes *prgfInOut with a set of
    // flags to be evaluated. The shell folder may optimize the operation
    // by not returning unspecified flags.
    function GetAttributesOf(cidl: UINT;
                             apidl: PCUITEMID_CHILD_ARRAY;
                             rgfInOut: PSFGAOF): HRESULT; stdcall;

    // creates a UI object to be used for specified objects.
    // The shell explorer passes either IID_IDataObject (for transfer operation)
    // or IID_IContextMenu (for context menu operation) as riid
    // and many other interfaces
    function GetUIObjectOf(hwndOwner: HWND;
                           cidl: UINT;
                           apidl: PCUITEMID_CHILD_ARRAY;
                           const riid: TGUID;
                           rgfReserved: PUINT;
                           out ppv): HRESULT; stdcall;

    // returns the display name of the specified object.
    // If the ID contains the display name (in the locale character set),
    // it returns the offset to the name. Otherwise, it returns a pointer
    // to the display name string (UNICODE), which is allocated by the
    // task allocator, or fills in a buffer.
    // use the helper APIS StrRetToStr() or StrRetToBuf() to deal with the different
    // forms of the STRRET structure
    function GetDisplayNameOf(pidl: PCUITEMID_CHILD;
                              uFlags: SHGDNF;
                              out pName: STRRET): HRESULT; stdcall;

    // sets the display name of the specified object.
    // If it changes the ID as well, it returns the new ID which is
    // alocated by the task allocator.
    function SetNameOf(hwnd: HWND;
                       pidl: PCUITEMID_CHILD;
                       pszName: LPCWSTR;
                       uFlags: SHGDNF;
                       out ppidlOut: PITEMID_CHILD): HRESULT; stdcall;

    function RemoteSetNameOf(hwnd: HWND;
                             pidl: PCUITEMID_CHILD;
                             pszName: LPCWSTR;
                             uFlags: SHGDNF;
                             out ppidlOut: PITEMID_CHILD): HRESULT; stdcall;
  end;
  IID_IShellFolder = IShellFolder;
  {$EXTERNALSYM IID_IShellFolder}


  // Record SMDATA
  // =============
  SMDATA = record
    dwMask: DWORD;
    dwFlags: DWORD;
    hmenu: HMENU;
    hwnd: HWND;
    uId: UINT;
    uIdParent: UINT;
    uIdAncestor: UINT;
    punk: PIUnknown;
    pidlFolder: PIDLIST_ABSOLUTE;
    pidlItem: PUITEMID_CHILD;
    psf: IShellFolder;
    pvUserData: Pointer;
  end;
  {$EXTERNALSYM SMDATA}
  PSMDATA = ^SMDATA;
  {$EXTERNALSYM PSMDATA}
  LPSMDATA = PSMDATA;
  {$EXTERNALSYM LPSMDATA}

  // Interface IEnumExtraSearch
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumExtraSearch);'}
  {$EXTERNALSYM IEnumExtraSearch}
  IEnumExtraSearch = interface(IUnknown)
    ['{0E700BE1-9DB6-11D1-A1CE-00C04FD75D13}']

    function Next(celt: ULONG;
                  out rgelt: EXTRASEARCH;
                  pceltFetched: ULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenum: IEnumExtraSearch): HRESULT; stdcall;
  end;
  IID_IEnumExtraSearch = IEnumExtraSearch;
  {$EXTERNALSYM IID_IEnumExtraSearch}


  // Interface IShellFolder2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellFolder2);'}
  {$EXTERNALSYM IShellFolder2}
  IShellFolder2 = interface(IShellFolder)
    ['{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}']

    // Returns the guid of the search that is to be invoked when user clicks on the search toolbar button
    function GetDefaultSearchGUID(out pguid: TGUID): HRESULT; stdcall;

    // gives an enumerator of the searches to be added to the search menu
    function EnumSearches(out ppenum: IEnumExtraSearch): HRESULT; stdcall;

    function GetDefaultColumn(dwRes: DWORD;
                              out pSort: ULONG;
                              out pDisplay: ULONG): HRESULT; stdcall;

    // return SHCOLSTATE_ values
    function GetDefaultColumnState(iColumn: UINT;
                                   out pcsFlags: SHCOLSTATEF): HRESULT; stdcall;

    // PCUITEMID_CHILD should have been annotated as [in].
    // Changing the annotation will break compatibility, but GetDetailsEx should never be called with a null pidl.
    function GetDetailsEx(pidl: PCUITEMID_CHILD;
                          const pscid: SHCOLUMNID;
                          out pv: VARIANT): HRESULT; stdcall;

    function GetDetailsOf(pidl: PCUITEMID_CHILD;
                          iColumn: UINT;
                          out psd: SHELLDETAILS): HRESULT; stdcall;

    function MapColumnToSCID(iColumn: UINT;
                             out pscid: SHCOLUMNID): HRESULT; stdcall;
  end;
  IID_IShellFolder2 = IShellFolder2;
  {$EXTERNALSYM IID_IShellFolder2}


  // Interface IShellView
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellView);'}
  {$EXTERNALSYM IShellView}
  IShellView = interface(IOleWindow)
    ['{000214E3-0000-0000-C000-000000000046}']

    function TranslateAccelerator(pmsg: PMSG): HRESULT; stdcall;

    function EnableModeless(fEnable: BOOL): HRESULT; stdcall;

    function UIActivate(uState: UINT): HRESULT; stdcall;

    function Refresh(): HRESULT; stdcall;

    function CreateViewWindow(psvPrevious: IShellView;
                              pfs: LPCFOLDERSETTINGS;
                              psb: IShellBrowser;
                              prcView: PRECT;
                              out phWnd: HWND): HRESULT; stdcall;

    function DestroyViewWindow(): HRESULT; stdcall;

    function GetCurrentInfo(pfs: LPFOLDERSETTINGS): HRESULT; stdcall;

    function AddPropertySheetPages(dwReserved: DWORD;
                                   pfn: LPFNSVADDPROPSHEETPAGE;
                                   lparam: LPARAM): HRESULT; stdcall;

    function SaveViewState(): HRESULT; stdcall;

    function SelectItem(pidlItem: PCUITEMID_CHILD;
                        uFlags: SVSIF): HRESULT; stdcall;

    function GetItemObject(uItem: UINT;
                           const riid: TGUID;
                           out ppv): HRESULT; stdcall;
  end;
  IID_IShellView = IShellView;
  {$EXTERNALSYM IID_IShellView}


  // Interface IShellView2
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellView2);'}
  {$EXTERNALSYM IShellView2}
  IShellView2 = interface(IShellView)
    ['{88E39E80-3578-11CF-AE69-08002B2E1262}']

    function GetView(var pvid: SHELLVIEWID;
                     uView: ULONG): HRESULT; stdcall;

    function CreateViewWindow2(lpParams: LPSV2CVW2_PARAMS): HRESULT; stdcall;

    function HandleRename(pidlNew: PCUITEMID_CHILD): HRESULT; stdcall;

    function SelectAndPositionItem(pidlItem: PCUITEMID_CHILD;
                                   uFlags: UINT;
                                   ppt: PPOINT): HRESULT; stdcall;
  end;
  IID_IShellView2 = IShellView2;
  {$EXTERNALSYM IID_IShellView2}

  // Interface IFolderView
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderView);'}
  {$EXTERNALSYM IFolderView}
  IFolderView = interface(IUnknown)
    ['{CDE725B0-CCC9-4519-917E-325D72FAB4CE}']

    function GetCurrentViewMode(pViewMode: UINT): HRESULT; stdcall;

    function SetCurrentViewMode(ViewMode: UINT): HRESULT; stdcall;

    function GetFolder(const riid: REFIID;
                       out ppv: Pointer): HRESULT; stdcall;

    function Item(iItemIndex: Integer;
                     ppidl: PITEMID_CHILD): HRESULT; stdcall;

    function ItemCount(uFlags: UINT;
                       out pcItems: int): HRESULT; stdcall;

    function Items(uFlags: UINT;
                   const riid: REFIID;
                   out ppv: Pointer): HRESULT; stdcall;

    function GetSelectionMarkedItem(piItem: int): HRESULT; stdcall;

    function GetFocusedItem(piItem: int): HRESULT; stdcall;

    function GetItemPosition(pidl: PCUITEMID_CHILD;
                             out ppt: POINT): HRESULT; stdcall;

    function GetSpacing(ppt: POINT): HRESULT; stdcall;

    function GetDefaultSpacing(out ppt: POINT): HRESULT; stdcall;

    function GetAutoArrange(): HRESULT; stdcall;

    function SelectItem(iItem: Integer;
                        dwFlags: DWORD): HRESULT; stdcall;

    function SelectAndPositionItems(cidl: UINT;
                                    apidl: PCUITEMID_CHILD_ARRAY;
                                    apt: POINT;
                                    dwFlags: DWORD): HRESULT; stdcall;
  end;
  IID_IFolderView = IFolderView;
  {$EXTERNALSYM IID_IFolderView}

  // Interface IFolderView2
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderView2);'}
  {$EXTERNALSYM IFolderView2}
  IFolderView2 = interface(IFolderView)
    ['{1AF3A467-214F-4298-908E-06B03E0B39F9}']

    // Sets the group by property and starts a grouping operation
    function SetGroupBy(const key: PROPERTYKEY;
                        fAscending: BOOL): HRESULT; stdcall;

    function GetGroupBy(out pkey: PROPERTYKEY;
                        pfAscending: PBOOL): HRESULT; stdcall;

    function RemoteGetGroupBy(out pkey: PROPERTYKEY;
                              pfAscending: PBOOL): HRESULT; stdcall;

    // Setting and Getting per item view properties is not possible for Libraries or Search results views.
    // Consider using existing item properties instead.
    function SetViewProperty(pidl: PCUITEMID_CHILD;
                             const propkey: PROPERTYKEY;
                             const propvar: PROPVARIANT): HRESULT; stdcall;

    // Setting and Getting per item view properties is not possible for Libraries or Search results views.
    // Consider using existing item properties instead.
    function GetViewProperty(pidl: PCUITEMID_CHILD;
                             const propkey: PROPERTYKEY;
                             ppropvar: PPROPVARIANT): HRESULT; stdcall;

    // Setting per item Tile view properties is not possible for Libraries or Search results views.
    // Consider setting property lists for your item types instead (see PKEY_PropList_TileInfo)
    function SetTileViewProperties(pidl: PCUITEMID_CHILD;
                                   pszPropList: LPCWSTR): HRESULT; stdcall;

    // Setting per item Extended Tile view properties is not possible for Libraries or Search results views.
    // Consider setting property lists for your item types instead (see PKEY_PropList_ExtendedTileInfo)
    function SetExtendedTileViewProperties(pidl: PCUITEMID_CHILD;
                                           pszPropList: LPCWSTR): HRESULT; stdcall;

    function SetText(iType: FVTEXTTYPE;
                     pwszText: LPCWSTR): HRESULT; stdcall;

    function SetCurrentFolderFlags(dwMask: DWORD;
                                   dwFlags: DWORD): HRESULT; stdcall;

    function GetCurrentFolderFlags(out pdwFlags: DWORD): HRESULT; stdcall;

    function GetSortColumnCount(out pcColumns: Integer): HRESULT; stdcall;

    // Sets the sort by property and starts a sort operation
    function SetSortColumns(rgSortColumns: PSORTCOLUMN;
                            cColumns: Integer): HRESULT; stdcall;

    function GetSortColumns(rgSortColumns: PSORTCOLUMN;
                            cColumns: Integer): HRESULT; stdcall;

    // return IShellItem for an item based on its index
    function GetItem(iItem: Integer;
                     const riid: TGUID;
                     out ppv): HRESULT; stdcall;

    function GetVisibleItem(iStart: Integer;
                            fPrevious: BOOL;
                            out piItem: Integer): HRESULT; stdcall;

    function GetSelectedItem(iStart: Integer;
                             out piItem: Integer): HRESULT; stdcall;

    function GetSelection(fNoneImpliesFolder: BOOL;
                          out ppsia: IShellItemArray): HRESULT; stdcall;

    // Gets the selection state including check state.  Same as the flags for IFolderView::SelectAndPositionItems
    function GetSelectionState(pidl: PCUITEMID_CHILD;
                               out pdwFlags: DWORD): HRESULT; stdcall;

    // If pszVerb is NULL, then the default verb is invoked.
    function InvokeVerbOnSelection(pszVerb: LPCSTR): HRESULT; stdcall;

    // Sets default icon size if iImageSize == -1
    function SetViewModeAndIconSize(uViewMode: FOLDERVIEWMODE;
                                    iImageSize: Integer): HRESULT; stdcall;

    function GetViewModeAndIconSize(out puViewMode: FOLDERVIEWMODE;
                                    out piImageSize: Integer): HRESULT; stdcall;

    function SetGroupSubsetCount(cVisibleRows: UINT): HRESULT; stdcall;

    function GetGroupSubsetCount(out pcVisibleRows: UINT): HRESULT; stdcall;

    function SetRedraw(fRedrawOn: BOOL): HRESULT; stdcall;

    // S_OK means this view sourced the current drag drop or cut/paste operation (used by drop target objects)
    function IsMoveInSameFolder(): HRESULT; stdcall;

    function DoRename(): HRESULT; stdcall;
  end;
  IID_IFolderView2 = IFolderView2;
  {$EXTERNALSYM IID_IFolderView2}

  // Interface IFolderViewSettings
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderViewSettings);'}
  {$EXTERNALSYM IFolderViewSettings}
  IFolderViewSettings = interface(IUnknown)
    ['{AE8C987D-8797-4ED3-BE72-2A47DD938DB0}']

    // GetColumnPropertyList - returns IPropertyDescriptionList.  Ordered list of columns that must correspond to column enumerated
    // via ISF::GetDetailsOf. Any column from ISF::GetDetailsOf not included in this list will be marked SHCOLSTATE_SECONDARYUI
    // (or maintain SHCOLSTATE_HIDDEN)
    function GetColumnPropertyList(const riid: TGUID;
                                   out ppv): HRESULT; stdcall;

    function GetGroupByProperty(out pkey: PROPERTYKEY;
                                out pfGroupAscending: BOOL): HRESULT; stdcall;

    function GetViewMode(out plvm: FOLDERLOGICALVIEWMODE): HRESULT; stdcall;

    function GetIconSize(out puIconSize: UINT): HRESULT; stdcall;

    function GetFolderFlags(out pfolderMask: FOLDERFLAGS;
                            out pfolderFlags: FOLDERFLAGS): HRESULT; stdcall;

    function GetSortColumns(rgSortColumns: PSORTCOLUMN;
                            cColumnsIn: UINT;
                            out pcColumnsOut: UINT): HRESULT; stdcall;

    function GetGroupSubsetCount(out pcVisibleRows: UINT): HRESULT; stdcall;
  end;
  IID_IFolderViewSettings = IFolderViewSettings;
  {$EXTERNALSYM IID_IFolderViewSettings}


  // Interface IInitializeNetworkFolder
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeNetworkFolder);'}
  {$EXTERNALSYM IInitializeNetworkFolder}
  IInitializeNetworkFolder = interface(IUnknown)
    ['{6E0F9881-42A8-4F2A-97F8-8AF4E026D92D}']

    function Initialize(pidl: PCIDLIST_ABSOLUTE;
                        pidlTarget: PCIDLIST_ABSOLUTE;
                        uDisplayType: UINT;
                        pszResName: LPCWSTR;
                        pszProvider: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IInitializeNetworkFolder = IInitializeNetworkFolder;
  {$EXTERNALSYM IID_IInitializeNetworkFolder}



  // Interface INetworkFolderInternal
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INetworkFolderInternal);'}
  {$EXTERNALSYM INetworkFolderInternal}
  INetworkFolderInternal = interface(IUnknown)
    ['{CEB38218-C971-47BB-A703-F0BC99CCDB81}']

    function GetResourceDisplayType(out displayType: UINT): HRESULT; stdcall;

    function GetIDList(out idList: PIDLIST_ABSOLUTE): HRESULT; stdcall;

    function GetProvider(itemIdCount: UINT;
                         itemIds: PCUITEMID_CHILD_ARRAY;
                         providerMaxLength: UINT;
                         provider: LPWSTR): HRESULT; stdcall;
  end;
  IID_INetworkFolderInternal = INetworkFolderInternal;
  {$EXTERNALSYM IID_INetworkFolderInternal}


  // Interface IPreviewHandlerVisuals
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPreviewHandlerVisuals);'}
  {$EXTERNALSYM IPreviewHandlerVisuals}
  IPreviewHandlerVisuals = interface(IUnknown)
    ['{196BF9A5-B346-4EF0-AA1E-5DCDB76768B1}']

    function SetBackgroundColor(color: COLORREF): HRESULT; stdcall;

    function SetFont(plf: LOGFONTW): HRESULT; stdcall;

    function SetTextColor(color: COLORREF): HRESULT; stdcall;
  end;
  IID_IPreviewHandlerVisuals = IPreviewHandlerVisuals;
  {$EXTERNALSYM IID_IPreviewHandlerVisuals}


  // Interface ICommDlgBrowser
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICommDlgBrowser);'}
  {$EXTERNALSYM ICommDlgBrowser}
  ICommDlgBrowser = interface(IUnknown)
    ['{000214F1-0000-0000-C000-000000000046}']

    function OnDefaultCommand(ppshv: IShellView): HRESULT; stdcall;

    function OnStateChange(ppshv: IShellView;
                           uChange: ULONG): HRESULT; stdcall;

    function IncludeObject(ppshv: IShellView;
                           pidl: PCUITEMID_CHILD): HRESULT; stdcall;
  end;
  IID_ICommDlgBrowser = ICommDlgBrowser;
  {$EXTERNALSYM IID_ICommDlgBrowser}


  // Interface ICommDlgBrowser2
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICommDlgBrowser2);'}
  {$EXTERNALSYM ICommDlgBrowser2}
  ICommDlgBrowser2 = interface(ICommDlgBrowser)
    ['{10339516-2894-11D2-9039-00C04F8EEB3E}']

    function Notify(ppshv: IShellView;
                    dwNotifyType: DWORD): HRESULT; stdcall;

    function GetDefaultMenuText(ppshv: IShellView;
                                pszText: LPWSTR;
                                cchMax: Integer): HRESULT; stdcall;

    function GetViewFlags(out pdwFlags: DWORD): HRESULT; stdcall;
  end;
  IID_ICommDlgBrowser2 = ICommDlgBrowser2;
  {$EXTERNALSYM IID_ICommDlgBrowser2}


  // Interface IColumnManager
  // ========================
  // IColumnManager is an interfaced provided by defview to
  // allow the manipulation of columns in details view.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IColumnManager);'}
  {$EXTERNALSYM IColumnManager}
  IColumnManager = interface(IUnknown)
    ['{D8EC27BB-3F3B-4042-B10A-4ACFD924D453}']

    // NOTE: These methods only accept physical pixel values.
    function SetColumnInfo(const propkey: PROPERTYKEY;
                           const pcmci: CM_COLUMNINFO): HRESULT; stdcall;

    function GetColumnInfo(const propkey: PROPERTYKEY;
                           pcmci: PCM_COLUMNINFO): HRESULT; stdcall;

    function GetColumnCount(dwFlags: CM_ENUM_FLAGS;
                            out puCount: UINT): HRESULT; stdcall;

    function GetColumns(dwFlags: CM_ENUM_FLAGS;
                        rgkeyOrder: PPROPERTYKEY;
                        cColumns: UINT): HRESULT; stdcall;

    function SetColumns(rgkeyOrder: PPROPERTYKEY;
                        cVisible: UINT): HRESULT; stdcall;
  end;
  IID_IColumnManager = IColumnManager;
  {$EXTERNALSYM IID_IColumnManager}


  // Interface IFolderFilterSite
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderFilterSite);'}
  {$EXTERNALSYM IFolderFilterSite}
  IFolderFilterSite = interface(IUnknown)
    ['{C0A651F5-B48B-11D2-B5ED-006097C686F6}']

    function SetFilter(punk: IUnknown): HRESULT; stdcall;
  end;
  IID_IFolderFilterSite = IFolderFilterSite;
  {$EXTERNALSYM IID_IFolderFilterSite}


  // Interface IFolderFilter
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderFilter);'}
  {$EXTERNALSYM IFolderFilter}
  IFolderFilter = interface(IUnknown)
    ['{9CC22886-DC8E-11D2-B1D0-00C04F8EEB3E}']

    function ShouldShow(psf: IShellFolder;
                        pidlFolder: PCIDLIST_ABSOLUTE;
                        pidlItem: PCUITEMID_CHILD): HRESULT; stdcall;

    function GetEnumFlags(psf: IShellFolder;
                          pidlFolder: PCIDLIST_ABSOLUTE;
                          out phwnd: HWND;
                          pgrfFlags: PDWORD): HRESULT; stdcall;
  end;
  IID_IFolderFilter = IFolderFilter;
  {$EXTERNALSYM IID_IFolderFilter}


  // Interface IInputObjectSite
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInputObjectSite);'}
  {$EXTERNALSYM IInputObjectSite}
  IInputObjectSite = interface(IUnknown)
    ['{F1DB8392-7331-11D0-8C99-00A0C92DBFE8}']

    function OnFocusChangeIS(punkObj: IUnknown;
                             fSetFocus: BOOL): HRESULT; stdcall;

  end;
  IID_IInputObjectSite = IInputObjectSite;
  {$EXTERNALSYM IID_IInputObjectSite}


  // Interface IInputObject
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInputObject);'}
  {$EXTERNALSYM IInputObject}
  IInputObject = interface(IUnknown)
    ['{68284FAA-6A48-11D0-8C78-00C04FD918B4}']

    // Activates or deactivates the object. lpMsg may be NULL.
    // Returns S_OK if the activation succeeded.
    function UIActivateIO(fActivate: BOOL;
                          pMsg: PMSG): HRESULT; stdcall;

    // Returns S_OK if the object has the focus, S_FALSE if not.
    function HasFocusIO(): HRESULT; stdcall;

    // Allow the object to process the message.
    // Returns S_OK if the message was processed (eaten).
    function TranslateAcceleratorIO(pMsg: PMSG): HRESULT; stdcall;
  end;
  IID_IInputObject = IInputObject;
  {$EXTERNALSYM IID_IInputObject}


  // Interface IInputObject2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInputObject2);'}
  {$EXTERNALSYM IInputObject2}
  IInputObject2 = interface(IInputObject)
    ['{6915C085-510B-44CD-94AF-28DFA56CF92B}']

    function TranslateAcceleratorGlobal(pMsg: PMSG): HRESULT; stdcall;
  end;
  IID_IInputObject2 = IInputObject2;
  {$EXTERNALSYM IID_IInputObject2}


  // Interface IShellIcon
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellIcon);'}
  {$EXTERNALSYM IShellIcon}
  IShellIcon = interface(IUnknown)
    ['{000214E5-0000-0000-C000-000000000046}']

    function GetIconOf(pidl: PCUITEMID_CHILD;
                     flags: UINT;
                     out pIconIndex: int): HRESULT; stdcall;
  end;
  IID_IShellIcon = IShellIcon;
  {$EXTERNALSYM IID_IShellIcon}


  // Interface IShellBrowser
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellBrowser);'}
  {$EXTERNALSYM IShellBrowser}
  IShellBrowser = interface(IOleWindow)
    ['{000214E2-0000-0000-C000-000000000046}']

    // same as IOleInPlaceFrame
    function InsertMenusSB(hmenuShared: HMENU;
                           lpMenuWidths: POLEMENUGROUPWIDTHS): HRESULT; stdcall;

    function SetMenuSB(hmenuShared: HMENU;
                       holemenuRes: HOLEMENU;
                       hwndActiveObject: HWND): HRESULT; stdcall;

    function RemoveMenusSB(hmenuShared: HMENU): HRESULT; stdcall;

    function SetStatusTextSB(pszStatusText: LPCWSTR): HRESULT; stdcall;

    function EnableModelessSB(fEnable: BOOL): HRESULT; stdcall;

    function TranslateAcceleratorSB(pmsg: PMSG;
                                    wID: WORD): HRESULT; stdcall;

    // IShellBrowser
    function BrowseObject(pidl: PCUIDLIST_RELATIVE;
                          wFlags: UINT): HRESULT; stdcall;

    function GetViewStateStream(grfMode: DWORD;
                                out ppStrm: IStream): HRESULT; stdcall;

    function GetControlWindow(id: UINT;
                              out phwnd: HWND): HRESULT; stdcall;

    function SendControlMsg(id: UINT;
                            uMsg: UINT;
                            wParam: WPARAM;
                            lParam: LPARAM;
                            pret: PLRESULT): HRESULT; stdcall;

    function QueryActiveShellView(out ppshv: IShellView): HRESULT; stdcall;

    function OnViewWindowActive(pshv: IShellView): HRESULT; stdcall;

    function SetToolbarItems(lpButtons: LPTBBUTTONSB;
                             nButtons: UINT;
                             uFlags: UINT): HRESULT; stdcall;
  end;
  IID_IShellBrowser = IShellBrowser;
  {$EXTERNALSYM IID_IShellBrowser}


  // Interface IProfferService
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IProfferService);'}
  {$EXTERNALSYM IProfferService}
  IProfferService = interface(IUnknown)
    ['{CB728B20-F786-11CE-92AD-00AA00A74CD0}']

    function ProfferService(const serviceId: REFGUID;
                            serviceProvider: IServiceProvider;
                            out cookie: DWORD): HRESULT; stdcall;

    function RevokeService(cookie: DWORD): HRESULT; stdcall;
  end;
  IID_IProfferService = IProfferService;
  {$EXTERNALSYM IID_IProfferService}


  // Interface IGetServiceIds
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGetServiceIds);'}
  {$EXTERNALSYM IGetServiceIds}
  IGetServiceIds = interface(IUnknown)
    ['{4A073526-6103-4E21-B7BC-F519D1524E5D}']

    function GetServiceIds(out serviceIdCount: ULONG;
                           out serviceIds: PGUID): HRESULT; stdcall;
  end;
  IID_IGetServiceIds = IGetServiceIds;
  {$EXTERNALSYM IID_IGetServiceIds}


  // Interface IShellItem
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellItem);'}
  {$EXTERNALSYM IShellItem}
  IShellItem = interface(IUnknown)
    ['{43826D1E-E718-42EE-BC55-A1E261C37BFE}']

    function BindToHandler(pbc: IBindCtx;
                           const bhid: TGUID;
                           const riid: TGUID;
                           out ppv): HRESULT; stdcall;

    function GetParent(out ppsi: IShellItem): HRESULT; stdcall;

    function GetDisplayName(sigdnName: SIGDN;
                            out ppszName: LPWSTR): HRESULT; stdcall;

    function GetAttributes(sfgaoMask: SFGAOF;
                           out psfgaoAttribs: SFGAOF): HRESULT; stdcall;

    function Compare(psi: IShellItem;
                     hint: SICHINTF;
                     out piOrder: Integer): HRESULT; stdcall;
  end;
  IID_IShellItem = IShellItem;
  {$EXTERNALSYM IID_IShellItem}


  // Interface IShellItem2
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellItem2);'}
  {$EXTERNALSYM IShellItem2}
  IShellItem2 = interface(IShellItem)
    ['{7E9FB0D3-919F-4307-AB2E-9B1860310C93}']

    function GetPropertyStore(flags: GETPROPERTYSTOREFLAGS;
                              const riid: TGUID;
                              out ppv): HRESULT; stdcall;

    function GetPropertyStoreWithCreateObject(flags: GETPROPERTYSTOREFLAGS;
                                              punkCreateObject: IUnknown;
                                              const riid: TGUID;
                                              out ppv): HRESULT; stdcall;

    function GetPropertyStoreForKeys(rgKeys: PPROPERTYKEY;
                                     cKeys: UINT;
                                     flags: GETPROPERTYSTOREFLAGS;
                                     const riid: TGUID;
                                     out ppv): HRESULT; stdcall;

    function GetPropertyDescriptionList(const keyType: PROPERTYKEY;
                                        const riid: TGUID;
                                        out ppv): HRESULT; stdcall;

    function Update(pbc: IBindCtx): HRESULT; stdcall;

    function GetProperty(const key: PROPERTYKEY;
                         out ppropvar: PROPVARIANT): HRESULT; stdcall;

    function GetCLSID(const key: PROPERTYKEY;
                      out pclsid: CLSID): HRESULT; stdcall;

    function GetFileTime(const key: PROPERTYKEY;
                         out pft: FILETIME): HRESULT; stdcall;

    function GetInt32(const key: PROPERTYKEY;
                      out pi: Integer): HRESULT; stdcall;

    function GetString(const key: PROPERTYKEY;
                       out ppsz: LPWSTR): HRESULT; stdcall;

    function GetUInt32(const key: PROPERTYKEY;
                       out pui: ULONG): HRESULT; stdcall;

    function GetUInt64(const key: PROPERTYKEY;
                       out pull: ULONGLONG): HRESULT; stdcall;

    function GetBool(const key: PROPERTYKEY;
                     out pf: BOOL): HRESULT; stdcall;
  end;
  IID_IShellItem2 = IShellItem2;
  {$EXTERNALSYM IID_IShellItem2}


  // Interface IShellItemImageFactory
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellItemImageFactory);'}
  {$EXTERNALSYM IShellItemImageFactory}
  IShellItemImageFactory = interface(IUnknown)
    ['{BCC18B79-BA16-442F-80C4-8A59C30C463B}']

    function GetImage(size: SIZE;
                      flags: SIIGBF;
                      out phbm: HBITMAP): HRESULT; stdcall;
  end;
  IID_IShellItemImageFactory = IShellItemImageFactory;
  {$EXTERNALSYM IID_IShellItemImageFactory}


  // Interface IEnumShellItems
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumShellItems);'}
  {$EXTERNALSYM IEnumShellItems}
  IEnumShellItems = interface(IUnknown)
    ['{70629033-E363-4A28-A567-0DB78006E6D7}']

    function Next(celt: ULONG;
                  rgelt: PIShellItem;
                  pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenum: IEnumShellItems): HRESULT; stdcall;
  end;
  IID_IEnumShellItems = IEnumShellItems;
  {$EXTERNALSYM IID_IEnumShellItems}


  // Interface ITransferAdviseSink
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITransferAdviseSink);'}
  {$EXTERNALSYM ITransferAdviseSink}
  ITransferAdviseSink = interface(IUnknown)
    ['{D594D0D8-8DA7-457B-B3B4-CE5DBAAC0B88}']

    function UpdateProgress(ullSizeCurrent: ULONGLONG;
                            ullSizeTotal: ULONGLONG;
                            nFilesCurrent: Integer;
                            nFilesTotal: Integer;
                            nFoldersCurrent: Integer;
                            nFoldersTotal: Integer): HRESULT; stdcall;

    function UpdateTransferState(ts: TRANSFER_ADVISE_STATE): HRESULT; stdcall;

    function ConfirmOverwrite(psiSource: IShellItem;
                              psiDestParent: IShellItem;
                              pszName: LPCWSTR): HRESULT; stdcall;

    function ConfirmEncryptionLoss(psiSource: IShellItem): HRESULT; stdcall;

    function FileFailure(psi: IShellItem;
                         pszItem: LPCWSTR;
                         hrError: HRESULT;
                         pszRename: LPWSTR;
                         cchRename: ULONG): HRESULT; stdcall;

    function SubStreamFailure(psi: IShellItem;
                              pszStreamName: LPCWSTR;
                              hrError: HRESULT): HRESULT; stdcall;

    function PropertyFailure(psi: IShellItem;
                             const pkey: PROPERTYKEY;
                             hrError: HRESULT): HRESULT; stdcall;
  end;
  IID_ITransferAdviseSink = ITransferAdviseSink;
  {$EXTERNALSYM IID_ITransferAdviseSink}


  // Interface ITransferSource
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITransferSource);'}
  {$EXTERNALSYM ITransferSource}
  ITransferSource = interface(IUnknown)
    ['{00ADB003-BDE9-45C6-8E29-D09F9353E108}']

    function Advise(psink: ITransferAdviseSink;
                    out pdwCookie: DWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;

    function SetProperties(pproparray: IPropertyChangeArray): HRESULT; stdcall;

    function OpenItem(psi: IShellItem;
                      flags: TRANSFER_SOURCE_FLAGS;
                      const riid: TGUID;
                      out ppv): HRESULT; stdcall;

    function MoveItem(psi: IShellItem;
                      psiParentDst: IShellItem;
                      pszNameDst: LPCWSTR;
                      flags: TRANSFER_SOURCE_FLAGS;
                      out ppsiNew: IShellItem): HRESULT; stdcall;

    function RecycleItem(psiSource: IShellItem;
                         psiParentDest: IShellItem;
                         flags: TRANSFER_SOURCE_FLAGS;
                         out ppsiNewDest: IShellItem): HRESULT; stdcall;

    function RemoveItem(psiSource: IShellItem;
                        flags: TRANSFER_SOURCE_FLAGS): HRESULT; stdcall;

    function RenameItem(psiSource: IShellItem;
                        pszNewName: LPCWSTR;
                        flags: TRANSFER_SOURCE_FLAGS;
                        out ppsiNewDest: IShellItem): HRESULT; stdcall;

    function LinkItem(psiSource: IShellItem;
                      psiParentDest: IShellItem;
                      pszNewName: LPCWSTR;
                      flags: TRANSFER_SOURCE_FLAGS;
                      out ppsiNewDest: IShellItem): HRESULT; stdcall;

    function ApplyPropertiesToItem(psiSource: IShellItem;
                                   out ppsiNew: IShellItem): HRESULT; stdcall;

    function GetDefaultDestinationName(psiSource: IShellItem;
                                       psiParentDest: IShellItem;
                                       out ppszDestinationName: LPWSTR): HRESULT; stdcall;

    function EnterFolder(psiChildFolderDest: IShellItem): HRESULT; stdcall;

    function LeaveFolder(psiChildFolderDest: IShellItem): HRESULT; stdcall;
  end;
  IID_ITransferSource = ITransferSource;
  {$EXTERNALSYM IID_ITransferSource}

  // Interface IEnumResources
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumResources);'}
  {$EXTERNALSYM IEnumResources}
  IEnumResources = interface(IUnknown)
    ['{2DD81FE3-A83C-4DA9-A330-47249D345BA1}']

    function Next(celt: ULONG;
                  psir: PSHELL_ITEM_RESOURCE;
                  pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenumr: IEnumResources): HRESULT; stdcall;
  end;
  IID_IEnumResources = IEnumResources;
  {$EXTERNALSYM IID_IEnumResources}


  // Interface IShellItemResources
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellItemResources);'}
  {$EXTERNALSYM IShellItemResources}
  IShellItemResources = interface(IUnknown)
    ['{FF5693BE-2CE0-4D48-B5C5-40817D1ACDB9}']

    function GetAttributes(out pdwAttributes: DWORD): HRESULT; stdcall;

    function GetSize(out pullSize: ULONGLONG): HRESULT; stdcall;

    function GetTimes(out pftCreation: FILETIME;
                      out pftWrite: FILETIME;
                      out pftAccess: FILETIME): HRESULT; stdcall;

    function SetTimes(const pftCreation: PFILETIME;
                      const pftWrite: PFILETIME;
                      const pftAccess: PFILETIME): HRESULT; stdcall;

    function GetResourceDescription(const pcsir: PSHELL_ITEM_RESOURCE;
                                    out ppszDescription: LPWSTR): HRESULT; stdcall;

    function EnumResources(out ppenumr: IEnumResources): HRESULT; stdcall;

    function SupportsResource(const pcsir: PSHELL_ITEM_RESOURCE): HRESULT; stdcall;

    function OpenResource(const pcsir: PSHELL_ITEM_RESOURCE;
                          const riid: TGUID;
                          out ppv): HRESULT; stdcall;

    function CreateResource(const pcsir: PSHELL_ITEM_RESOURCE;
                            const riid: TGUID;
                            out ppv): HRESULT; stdcall;

    function MarkForDelete(): HRESULT; stdcall;
  end;
  IID_IShellItemResources = IShellItemResources;
  {$EXTERNALSYM IID_IShellItemResources}


  // Interface ITransferDestination
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITransferDestination);'}
  {$EXTERNALSYM ITransferDestination}
  ITransferDestination = interface(IUnknown)
    ['{48ADDD32-3CA5-4124-ABE3-B5A72531B207}']

    function Advise(psink: ITransferAdviseSink;
                    out pdwCookie: DWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;

    function CreateItem(pszName: LPCWSTR;
                        dwAttributes: DWORD;
                        ullSize: ULONGLONG;
                        flags: TRANSFER_SOURCE_FLAGS;
                        const riidItem: TGUID;
                        out ppvItem;
                        const riidResources: TGUID;
                        out ppvResources): HRESULT; stdcall;
  end;
  IID_ITransferDestination = ITransferDestination;
  {$EXTERNALSYM IID_ITransferDestination}


  // Interface IFileOperationProgressSink
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileOperationProgressSink);'}
  {$EXTERNALSYM IFileOperationProgressSink}
  IFileOperationProgressSink = interface(IUnknown)
    ['{04B0F1A7-9490-44BC-96E1-4296A31252E2}']

    function StartOperations(): HRESULT; stdcall;

    function FinishOperations(hrResult: HRESULT): HRESULT; stdcall;

    function PreRenameItem(dwFlags: DWORD;
                           psiItem: IShellItem;
                           pszNewName: LPCWSTR): HRESULT; stdcall;

    function PostRenameItem(dwFlags: DWORD;
                            psiItem: IShellItem;
                            pszNewName: LPCWSTR;
                            hrRename: HRESULT;
                            psiNewlyCreated: IShellItem): HRESULT; stdcall;

    function PreMoveItem(dwFlags: DWORD;
                         psiItem: IShellItem;
                         psiDestinationFolder: IShellItem;
                         pszNewName: LPCWSTR): HRESULT; stdcall;

    function PostMoveItem(dwFlags: DWORD;
                          psiItem: IShellItem;
                          psiDestinationFolder: IShellItem;
                          pszNewName: LPCWSTR;
                          hrMove: HRESULT;
                          psiNewlyCreated: IShellItem): HRESULT; stdcall;

    function PreCopyItem(dwFlags: DWORD;
                         psiItem: IShellItem;
                         psiDestinationFolder: IShellItem;
                         pszNewName: LPCWSTR): HRESULT; stdcall;

    function PostCopyItem(dwFlags: DWORD;
                          psiItem: IShellItem;
                          psiDestinationFolder: IShellItem;
                          pszNewName: LPCWSTR;
                          hrCopy: HRESULT;
                          psiNewlyCreated: IShellItem): HRESULT; stdcall;

    function PreDeleteItem(dwFlags: DWORD;
                           psiItem: IShellItem): HRESULT; stdcall;

    function PostDeleteItem(dwFlags: DWORD;
                            psiItem: IShellItem;
                            hrDelete: HRESULT;
                            psiNewlyCreated: IShellItem): HRESULT; stdcall;

    function PreNewItem(dwFlags: DWORD;
                        psiDestinationFolder: IShellItem;
                        pszNewName: LPCWSTR): HRESULT; stdcall;

    function PostNewItem(dwFlags: DWORD;
                         psiDestinationFolder: IShellItem;
                         pszNewName: LPCWSTR;
                         pszTemplateName: LPCWSTR;
                         dwFileAttributes: DWORD;
                         hrNew: HRESULT;
                         psiNewItem: IShellItem): HRESULT; stdcall;

    function UpdateProgress(iWorkTotal: UINT;
                            iWorkSoFar: UINT): HRESULT; stdcall;

    function ResetTimer(): HRESULT; stdcall;

    function PauseTimer(): HRESULT; stdcall;

    function ResumeTimer(): HRESULT; stdcall;
  end;
  IID_IFileOperationProgressSink = IFileOperationProgressSink;
  {$EXTERNALSYM IID_IFileOperationProgressSink}


  // Interface IShellItemArray
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellItemArray);'}
  {$EXTERNALSYM IShellItemArray}
  IShellItemArray = interface(IUnknown)
    ['{B63EA76D-1F85-456F-A19C-48159EFA858B}']

    function BindToHandler(pbc: IBindCtx;
                           const bhid: TGUID;
                           const riid: TGUID;
                           out ppvOut): HRESULT; stdcall;

    function GetPropertyStore(flags: GETPROPERTYSTOREFLAGS;
                              const riid: TGUID;
                              out ppv): HRESULT; stdcall;

    function GetPropertyDescriptionList(const keyType: PROPERTYKEY;
                                        const riid: TGUID;
                                        out ppv): HRESULT; stdcall;

    function GetAttributes(AttribFlags: SIATTRIBFLAGS;
                           sfgaoMask: SFGAOF;
                           out psfgaoAttribs: SFGAOF): HRESULT; stdcall;

    function GetCount(out pdwNumItems: DWORD): HRESULT; stdcall;

    function GetItemAt(dwIndex: DWORD;
                       out ppsi: IShellItem): HRESULT; stdcall;

    function EnumItems(out ppenumShellItems: IEnumShellItems): HRESULT; stdcall;
  end;
  IID_IShellItemArray = IShellItemArray;
  {$EXTERNALSYM IID_IShellItemArray}


  // Interface IInitializeWithItem
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeWithItem);'}
  {$EXTERNALSYM IInitializeWithItem}
  IInitializeWithItem = interface(IUnknown)
    ['{7F73BE3F-FB79-493C-A6C7-7EE14E245841}']

    function Initialize(psi: IShellItem;
                        grfMode: DWORD): HRESULT; stdcall;
  end;
  IID_IInitializeWithItem = IInitializeWithItem;
  {$EXTERNALSYM IID_IInitializeWithItem}


  // Interface IObjectWithSelection
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithSelection);'}
  {$EXTERNALSYM IObjectWithSelection}
  IObjectWithSelection = interface(IUnknown)
    ['{1C9CD5BB-98E9-4491-A60F-31AACC72B83C}']

    function SetSelection(psia: IShellItemArray): HRESULT; stdcall;

    function GetSelection(const riid: REFIID;
                          out ppv): HRESULT; stdcall;
  end;
  IID_IObjectWithSelection = IObjectWithSelection;
  {$EXTERNALSYM IID_IObjectWithSelection}


  // Interface IObjectWithBackReferences
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithBackReferences);'}
  {$EXTERNALSYM IObjectWithBackReferences}
  IObjectWithBackReferences = interface(IUnknown)
    ['{321A6A6A-D61F-4BF3-97AE-14BE2986BB36}']

    function RemoveBackReferences(): HRESULT; stdcall;
  end;
  IID_IObjectWithBackReferences = IObjectWithBackReferences;
  {$EXTERNALSYM IID_IObjectWithBackReferences}


  // Interface IPropertyUI
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyUI);'}
  {$EXTERNALSYM IPropertyUI}
  IPropertyUI = interface(IUnknown)
    ['{757A7D9F-919A-4118-99D7-DBB208C8CC66}']

    function ParsePropertyName(pszName: LPCWSTR;
                               out pfmtid: FMTID;
                               out ppid: PROPID;
                               pchEaten: PULONG): HRESULT; stdcall;

    function GetCannonicalName(const fmtid: FMTID;
                               pid: PROPID;
                               pwszText: LPWSTR;
                               cchText: DWORD): HRESULT; stdcall;

    function GetDisplayName(const fmtid: FMTID;
                            pid: PROPID;
                            flags: PROPERTYUI_NAME_FLAGS;
                            pwszText: LPWSTR;
                            cchText: DWORD): HRESULT; stdcall;

    function GetPropertyDescription(const fmtid: FMTID;
                                    pid: PROPID;
                                    pwszText: LPWSTR;
                                    cchText: DWORD): HRESULT; stdcall;

    function GetDefaultWidth(const fmtid: FMTID;
                             pid: PROPID;
                             out pcxChars: ULONG): HRESULT; stdcall;

    function GetFlags(const fmtid: FMTID;
                      pid: PROPID;
                      out pflags: PROPERTYUI_FLAGS): HRESULT; stdcall;

    function FormatForDisplay(const fmtid: FMTID;
                              pid: PROPID;
                              const ppropvar: PPROPVARIANT;
                              puiff: PROPERTYUI_FORMAT_FLAGS;
                              pwszText: LPWSTR;
                              cchText: DWORD): HRESULT; stdcall;

    function GetHelpInfo(const fmtid: FMTID;
                         pid: PROPID;
                         pwszHelpFile: LPWSTR;
                         cch: DWORD;
                         out puHelpID: UINT): HRESULT; stdcall;
  end;
  IID_IPropertyUI = IPropertyUI;
  {$EXTERNALSYM IID_IPropertyUI}


  // Interface ICategoryProvider
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICategoryProvider);'}
  {$EXTERNALSYM ICategoryProvider}
  ICategoryProvider = interface(IUnknown)
    ['{9AF64809-5864-4C26-A720-C1F78C086EE3}']

    function CanCategorizeOnSCID(const pscid: PSHCOLUMNID): HRESULT; stdcall;

    function GetDefaultCategory(out pguid: TGUID;
                                out pscid: SHCOLUMNID): HRESULT; stdcall;

    function GetCategoryForSCID(const pscid: PSHCOLUMNID;
                                out pguid: TGUID): HRESULT; stdcall;

    function EnumCategories(out penum: IEnumGUID): HRESULT; stdcall;

    function GetCategoryName(const pguid: PGUID;
                             pszName: LPWSTR;
                             cch: UINT): HRESULT; stdcall;

    function CreateCategory(const pguid: PGUID;
                            const riid: TGUID;
                            out ppv): HRESULT; stdcall;
  end;
  IID_ICategoryProvider = ICategoryProvider;
  {$EXTERNALSYM IID_ICategoryProvider}


  // Interface ICategorizer
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICategorizer);'}
  {$EXTERNALSYM ICategorizer}
  ICategorizer = interface(IUnknown)
    ['{A3B14589-9174-49A8-89A3-06A1AE2B9BA7}']

    function GetDescription(pszDesc: LPWSTR;
                            cch: UINT): HRESULT; stdcall;

    function GetCategory(cidl: UINT;
                         apidl: PCUITEMID_CHILD_ARRAY;
                         rgCategoryIds: PDWORD): HRESULT; stdcall;

    function GetCategoryInfo(dwCategoryId: DWORD;
                             out pci: CATEGORY_INFO): HRESULT; stdcall;

    function CompareCategory(csfFlags: CATSORT_FLAGS;
                             dwCategoryId1: DWORD;
                             dwCategoryId2: DWORD): HRESULT; stdcall;
  end;
  IID_ICategorizer = ICategorizer;
  {$EXTERNALSYM IID_ICategorizer}


  // Interface IDropTargetHelper
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropTargetHelper);'}
  {$EXTERNALSYM IDropTargetHelper}
  IDropTargetHelper = interface(IUnknown)
    ['{4657278B-411B-11D2-839A-00C04FD918D0}']

    function DragEnter(hwndTarget: HWND;
                       pDataObject: IDataObject;
                       ppt: PPOINT;
                       dwEffect: DWORD): HRESULT; stdcall;

    function DragLeave(): HRESULT; stdcall;

    function DragOver(ppt: PPOINT;
                      dwEffect: DWORD): HRESULT; stdcall;

    function Drop(pDataObject: IDataObject;
                  ppt: PPOINT;
                  dwEffect: DWORD): HRESULT; stdcall;

    function Show(fShow: BOOL): HRESULT; stdcall;
  end;
  IID_IDropTargetHelper = IDropTargetHelper;
  {$EXTERNALSYM IID_IDropTargetHelper}


  // Interface IDragSourceHelper
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDragSourceHelper);'}
  {$EXTERNALSYM IDragSourceHelper}
  IDragSourceHelper = interface(IUnknown)
    ['{DE5BF786-477A-11D2-839D-00C04FD918D0}']

    function InitializeFromBitmap(pshdi: LPSHDRAGIMAGE;
                                  pDataObject: IDataObject): HRESULT; stdcall;

    function InitializeFromWindow(hwnd: HWND;
                                  ppt: PPOINT;
                                  pDataObject: IDataObject): HRESULT; stdcall;
  end;
  IID_IDragSourceHelper = IDragSourceHelper;
  {$EXTERNALSYM IID_IDragSourceHelper}


  // Interface IShellLinkA
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellLinkA);'}
  {$EXTERNALSYM IShellLinkA}
  IShellLinkA = interface(IUnknown)
    ['{000214EE-0000-0000-C000-000000000046}']

    function GetPath(pszFile: LPSTR;
                     cch: Integer;
                     pfd: PWIN32_FIND_DATAA;
                     fFlags: DWORD): HRESULT; stdcall;

    function GetIDList(out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;

    function SetIDList(pidl: PCIDLIST_ABSOLUTE): HRESULT; stdcall;

    function GetDescription(pszName: LPSTR;
                            cch: Integer): HRESULT; stdcall;

    function SetDescription(pszName: LPCSTR): HRESULT; stdcall;

    function GetWorkingDirectory(pszDir: LPSTR;
                                 cch: Integer): HRESULT; stdcall;

    function SetWorkingDirectory(pszDir: LPCSTR): HRESULT; stdcall;

    function GetArguments(pszArgs: LPSTR;
                          cch: Integer): HRESULT; stdcall;

    function SetArguments(pszArgs: LPCSTR): HRESULT; stdcall;

    function GetHotkey(out pwHotkey: WORD): HRESULT; stdcall;

    function SetHotkey(wHotkey: WORD): HRESULT; stdcall;

    function GetShowCmd(out piShowCmd: Integer): HRESULT; stdcall;

    function SetShowCmd(iShowCmd: Integer): HRESULT; stdcall;

    function GetIconLocation(pszIconPath: LPSTR;
                             cch: Integer;
                             out piIcon: Integer): HRESULT; stdcall;

    function SetIconLocation(pszIconPath: LPCSTR;
                             iIcon: Integer): HRESULT; stdcall;

    function SetRelativePath(pszPathRel: LPCSTR;
                             dwReserved: DWORD): HRESULT; stdcall;

    function Resolve(hwnd: HWND;
                     fFlags: DWORD): HRESULT; stdcall;

    function SetPath(pszFile: LPCSTR): HRESULT; stdcall;
  end;
  IID_IShellLinkA = IShellLinkA;
  {$EXTERNALSYM IID_IShellLinkA}


  // Interface IShellLinkW
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellLinkW);'}
  {$EXTERNALSYM IShellLinkW}
  IShellLinkW = interface(IUnknown)
    ['{000214F9-0000-0000-C000-000000000046}']

    function GetPath(pszFile: LPWSTR;
                     cch: Integer;
                     pfd: PWIN32_FIND_DATAW;
                     fFlags: DWORD): HRESULT; stdcall;

    function GetIDList(out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;

    function SetIDList(pidl: PCIDLIST_ABSOLUTE): HRESULT; stdcall;

    function GetDescription(pszName: LPWSTR;
                            cch: Integer): HRESULT; stdcall;

    function SetDescription(pszName: LPCWSTR): HRESULT; stdcall;

    function GetWorkingDirectory(pszDir: LPWSTR;
                                 cch: Integer): HRESULT; stdcall;

    function SetWorkingDirectory(pszDir: LPCWSTR): HRESULT; stdcall;

    function GetArguments(pszArgs: LPWSTR;
                          cch: Integer): HRESULT; stdcall;

    function SetArguments(pszArgs: LPCWSTR): HRESULT; stdcall;

    function GetHotkey(out pwHotkey: WORD): HRESULT; stdcall;

    function SetHotkey(wHotkey: WORD): HRESULT; stdcall;

    function GetShowCmd(out piShowCmd: Integer): HRESULT; stdcall;

    function SetShowCmd(iShowCmd: Integer): HRESULT; stdcall;

    function GetIconLocation(pszIconPath: LPWSTR;
                             cch: Integer;
                             out piIcon: Integer): HRESULT; stdcall;

    function SetIconLocation(pszIconPath: LPCWSTR;
                             iIcon: Integer): HRESULT; stdcall;

    function SetRelativePath(pszPathRel: LPCWSTR;
                             dwReserved: DWORD): HRESULT; stdcall;

    function Resolve(hwnd: HWND;
                     fFlags: DWORD): HRESULT; stdcall;

    function SetPath(pszFile: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IShellLinkW = IShellLinkW;
  {$EXTERNALSYM IID_IShellLinkW}


  // Interface IShellLinkDataList
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellLinkDataList);'}
  {$EXTERNALSYM IShellLinkDataList}
  IShellLinkDataList = interface(IUnknown)
    ['{45E2B4AE-B1C3-11D0-B92F-00A0C90312E1}']

    function AddDataBlock(pDataBlock: Pointer): HRESULT; stdcall;

    function CopyDataBlock(dwSig: DWORD;
                     out ppDataBlock: Pointer): HRESULT; stdcall;

    function RemoveDataBlock(dwSig: DWORD): HRESULT; stdcall;

    function GetFlags(out pdwFlags: DWORD): HRESULT; stdcall;

    function SetFlags(dwFlags: DWORD): HRESULT; stdcall;
  end;
  IID_IShellLinkDataList = IShellLinkDataList;
  {$EXTERNALSYM IID_IShellLinkDataList}


  // Interface IResolveShellLink
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IResolveShellLink);'}
  {$EXTERNALSYM IResolveShellLink}
  IResolveShellLink = interface(IUnknown)
    ['{5CD52983-9449-11D2-963A-00C04F79ADF0}']

    function ResolveShellLink(punkLink: IUnknown;
                              hwnd: HWND;
                              fFlags: DWORD): HRESULT; stdcall;
  end;
  IID_IResolveShellLink = IResolveShellLink;
  {$EXTERNALSYM IID_IResolveShellLink}


  // Interface IActionProgressDialog
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IActionProgressDialog);'}
  {$EXTERNALSYM IActionProgressDialog}
  IActionProgressDialog = interface(IUnknown)
    ['{49FF1172-EADC-446D-9285-156453A6431C}']

    function Initialize(flags: SPINITF;
                     pszTitle: LPCWSTR;
                     pszCancel: LPCWSTR): HRESULT; stdcall;

    function Stop: HRESULT; stdcall;
  end;
  IID_IActionProgressDialog = IActionProgressDialog;
  {$EXTERNALSYM IID_IActionProgressDialog}


  // Interface IActionProgress
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IActionProgress);'}
  {$EXTERNALSYM IActionProgress}
  IActionProgress = interface(IUnknown)
    ['{49FF1173-EADC-446D-9285-156453A6431C}']

    function Begin_(action: SPACTION;
                     flags: SPBEGINF): HRESULT; stdcall;

    function UpdateProgress(ulCompleted: ULONGLONG;
                     ulTotal: ULONGLONG): HRESULT; stdcall;

    function UpdateText(sptext: SPTEXT;
                     pszText: LPCWSTR;
                     fMayCompact: BOOL): HRESULT; stdcall;

    function QueryCancel(out pfCancelled: BOOL): HRESULT; stdcall;

    function ResetCancel(): HRESULT; stdcall;

    function _End(): HRESULT; stdcall;
  end;
  IID_IActionProgress = IActionProgress;
  {$EXTERNALSYM IID_IActionProgress}


  // Interface IShellExtInit
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellExtInit);'}
  {$EXTERNALSYM IShellExtInit}
  IShellExtInit = interface(IUnknown)
    ['{000214E8-0000-0000-C000-000000000046}']

    function Initialize(pidlFolder: PCIDLIST_ABSOLUTE;
                        pdtobj: PIDataObject;
                        hkeyProgID: HKEY): HRESULT; stdcall;
  end;
  IID_IShellExtInit = IShellExtInit;
  {$EXTERNALSYM IID_IShellExtInit}


  // Interface IShellPropSheetExt
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellPropSheetExt);'}
  {$EXTERNALSYM IShellPropSheetExt}
  IShellPropSheetExt = interface(IUnknown)
    ['{000214E9-0000-0000-C000-000000000046}']

    function AddPages(pfnAddPage: LPFNSVADDPROPSHEETPAGE;
                      lParam: LPARAM): HRESULT; stdcall;

    function ReplacePage(uPageID: EXPPS;
                         pfnReplaceWith: LPFNSVADDPROPSHEETPAGE;
                         lParam: LPARAM): HRESULT; stdcall;
  end;
  IID_IShellPropSheetExt = IShellPropSheetExt;
  {$EXTERNALSYM IID_IShellPropSheetExt}


  // Interface IRemoteComputer
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRemoteComputer);'}
  {$EXTERNALSYM IRemoteComputer}
  IRemoteComputer = interface(IUnknown)
    ['{000214FE-0000-0000-C000-000000000046}']

    function Initialize(pszMachine: LPCWSTR;
                        bEnumerating: BOOL): HRESULT; stdcall;
  end;
  IID_IRemoteComputer = IRemoteComputer;
  {$EXTERNALSYM IID_IRemoteComputer}


  // Interface IQueryContinue
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IQueryContinue);'}
  {$EXTERNALSYM IQueryContinue}
  IQueryContinue = interface(IUnknown)
    ['{7307055C-B24A-486B-9F25-163E597A28A9}']

    function QueryContinue(): HRESULT; stdcall;
  end;
  IID_IQueryContinue = IQueryContinue;
  {$EXTERNALSYM IID_IQueryContinue}


  // Interface IObjectWithCancelEvent
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithCancelEvent);'}
  {$EXTERNALSYM IObjectWithCancelEvent}
  IObjectWithCancelEvent = interface(IUnknown)
    ['{F279B885-0AE9-4B85-AC06-DDECF9408941}']

    function GetCancelEvent(phEvent: PHANDLE): HRESULT; stdcall;
  end;
  IID_IObjectWithCancelEvent = IObjectWithCancelEvent;
  {$EXTERNALSYM IID_IObjectWithCancelEvent}


  // Interface IUserNotification
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUserNotification);'}
  {$EXTERNALSYM IUserNotification}
  IUserNotification = interface(IUnknown)
    ['{BA9711BA-5893-4787-A7E1-41277151550B}']

    function SetBalloonInfo(pszTitle: LPCWSTR;
                            pszText: LPCWSTR;
                            dwInfoFlags: DWORD): HRESULT; stdcall;

    function SetBalloonRetry(dwShowTime: DWORD;
                             dwInterval: DWORD;
                             cRetryCount: UINT): HRESULT; stdcall;

    function SetIconInfo(hIcon: HICON;
                         pszToolTip: LPCWSTR): HRESULT; stdcall;

    function Show(pqc: IQueryContinue;
                  dwContinuePollInterval: DWORD): HRESULT; stdcall;

    function PlaySound(pszSoundName: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IUserNotification = IUserNotification;
  {$EXTERNALSYM IID_IUserNotification}


  // Interface IItemNameLimits
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IItemNameLimits);'}
  {$EXTERNALSYM IItemNameLimits}
  IItemNameLimits = interface(IUnknown)
    ['{1DF0D7F1-B267-4D28-8B10-12E23202A5C4}']

    function GetValidCharacters(ppwszValidChars: LPWSTR;
                                ppwszInvalidChars: LPWSTR): HRESULT; stdcall;

    function GetMaxLength(pszName: LPCWSTR;
                          out piMaxNameLen: Int): HRESULT; stdcall;
  end;
  IID_IItemNameLimits = IItemNameLimits;
  {$EXTERNALSYM IID_IItemNameLimits}


  // Interface ISearchFolderItemFactory
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISearchFolderItemFactory);'}
  {$EXTERNALSYM ISearchFolderItemFactory}
  ISearchFolderItemFactory = interface(IUnknown)
    ['{A0FFBC28-5482-4366-BE27-3E81E78E06C2}']

    function SetDisplayName(pszDisplayName: LPCWSTR): HRESULT; stdcall;

    function SetFolderTypeID(ftid: FOLDERTYPEID): HRESULT; stdcall;

    function SetFolderLogicalViewMode(flvm: FOLDERLOGICALVIEWMODE): HRESULT; stdcall;

    function SetIconSize(iIconSize: Integer): HRESULT; stdcall;

    function SetVisibleColumns(cVisibleColumns: UINT;
                               const rgKey: PPROPERTYKEY): HRESULT; stdcall;

    function SetSortColumns(cSortColumns: UINT;
                            rgSortColumns: PSORTCOLUMN): HRESULT; stdcall;

    function SetGroupColumn(const keyGroup: PROPERTYKEY): HRESULT; stdcall;

    function SetStacks(cStackKeys: UINT;
                       rgStackKeys: PPROPERTYKEY): HRESULT; stdcall;

    function SetScope(psiaScope: IShellItemArray): HRESULT; stdcall;

    function SetCondition(pCondition: ICondition): HRESULT; stdcall;

    function GetShellItem(const riid: TGUID;
                          out ppv): HRESULT; stdcall;

    function GetIDList(out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;
  end;
  IID_ISearchFolderItemFactory = ISearchFolderItemFactory;
  {$EXTERNALSYM IID_ISearchFolderItemFactory}


  // Interface IExtractImage
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExtractImage);'}
  {$EXTERNALSYM IExtractImage}
  IExtractImage = interface(IUnknown)
    ['{BB2E617C-0920-11D1-9A0B-00C04FC2D6C1}']

    function GetLocation(pszPathBuffer: LPWSTR;
                         cch: DWORD;
                         pdwPriority: PDWORD;
                         const prgSize: PSIZE;
                         dwRecClrDepth: DWORD;
                         pdwFlags: PDWORD): HRESULT; stdcall;

    function Extract(out phBmpThumbnail: HBITMAP): HRESULT; stdcall;
  end;
  IID_IExtractImage = IExtractImage;
  {$EXTERNALSYM IID_IExtractImage}


  // Interface IExtractImage2
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExtractImage2);'}
  {$EXTERNALSYM IExtractImage2}
  IExtractImage2 = interface(IExtractImage)
    ['{953BB1EE-93B4-11D1-98A3-00C04FB687DA}']

    function GetDateStamp(out pDateStamp: FILETIME): HRESULT; stdcall;
  end;
  IID_IExtractImage2 = IExtractImage2;
  {$EXTERNALSYM IID_IExtractImage2}


  // Interface IThumbnailHandlerFactory
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IThumbnailHandlerFactory);'}
  {$EXTERNALSYM IThumbnailHandlerFactory}
  IThumbnailHandlerFactory = interface(IUnknown)
    ['{E35B4B2E-00DA-4BC1-9F13-38BC11F5D417}']

    function GetThumbnailHandler(pidlChild: PCUITEMID_CHILD;
                                 pbc: IBindCtx;
                                 const riid: REFIID;
                                 out ppv): HRESULT; stdcall;
  end;
  IID_IThumbnailHandlerFactory = IThumbnailHandlerFactory;
  {$EXTERNALSYM IID_IThumbnailHandlerFactory}


  // Interface IParentAndItem
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IParentAndItem);'}
  {$EXTERNALSYM IParentAndItem}
  IParentAndItem = interface(IUnknown)
    ['{B3A4B685-B685-4805-99D9-5DEAD2873236}']

    function SetParentAndItem(pidlParent: PCIDLIST_ABSOLUTE;
                              psf: IShellFolder;
                              pidlChild: PCUITEMID_CHILD): HRESULT; stdcall;

    function GetParentAndItem(out ppidlParent: PIDLIST_ABSOLUTE;
                              out ppsf: IShellFolder;
                              out ppidlChild: PITEMID_CHILD): HRESULT; stdcall;
  end;
  IID_IParentAndItem = IParentAndItem;
  {$EXTERNALSYM IID_IParentAndItem}


  // Interface IDockingWindow
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDockingWindow);'}
  {$EXTERNALSYM IDockingWindow}
  IDockingWindow = interface(IOleWindow)
    ['{012DD920-7B26-11D0-8CA9-00A0C92DBFE8}']

    function ShowDW(fShow: BOOL): HRESULT; stdcall;

    function CloseDW(dwReserved: DWORD): HRESULT; stdcall;

    function ResizeBorderDW(prcBorder: LPCRECT;
                            punkToolbarSite: IUnknown;
                            fReserved: BOOL): HRESULT; stdcall;
  end;
  IID_IDockingWindow = IDockingWindow;
  {$EXTERNALSYM IID_IDockingWindow}


  // Interface IDeskBand
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDeskBand);'}
  {$EXTERNALSYM IDeskBand}
  IDeskBand = interface(IDockingWindow)
    ['{EB0FE172-1A3A-11D0-89B3-00A0C90A90AC}']

    function GetBandInfo(dwBandID: DWORD;
                         dwViewMode: DWORD;
               {in_out}  pdbi: PDESKBANDINFO): HRESULT; stdcall;
  end;
  IID_IDeskBand = IDeskBand;
  {$EXTERNALSYM IID_IDeskBand}


  // Interface IDeskBandInfo
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDeskBandInfo);'}
  {$EXTERNALSYM IDeskBandInfo}
  IDeskBandInfo = interface(IUnknown)
    ['{77E425FC-CBF9-4307-BA6A-BB5727745661}']

    function GetDefaultBandWidth(dwBandID: DWORD;
                                 dwViewMode: DWORD;
                                 out pnWidth: Int): HRESULT; stdcall;
  end;
  IID_IDeskBandInfo = IDeskBandInfo;
  {$EXTERNALSYM IID_IDeskBandInfo}


  // Interface ITaskbarList
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITaskbarList);'}
  {$EXTERNALSYM ITaskbarList}
  ITaskbarList = interface(IUnknown)
    ['{56FDF342-FD6D-11D0-958A-006097C9A090}']

    function HrInit(): HRESULT; stdcall;

    function AddTab(hwnd: HWND): HRESULT; stdcall;

    function DeleteTab(hwnd: HWND): HRESULT; stdcall;

    function ActivateTab(hwnd: HWND): HRESULT; stdcall;

    function SetActiveAlt(hwnd: HWND): HRESULT; stdcall;
  end;
  IID_ITaskbarList = ITaskbarList;
  {$EXTERNALSYM IID_ITaskbarList}


  // Interface ITaskbarList2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITaskbarList2);'}
  {$EXTERNALSYM ITaskbarList2}
  ITaskbarList2 = interface(ITaskbarList)
    ['{602D4995-B13A-429B-A66E-1935E44F4317}']

    function MarkFullscreenWindow(hwnd: HWND;
                                  fFullscreen: BOOL): HRESULT; stdcall;
  end;
  IID_ITaskbarList2 = ITaskbarList2;
  {$EXTERNALSYM IID_ITaskbarList2}


  // Interface ITaskbarList3
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITaskbarList3);'}
  {$EXTERNALSYM ITaskbarList3}
  ITaskbarList3 = interface(ITaskbarList2)
    ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']

    function SetProgressValue(hwnd: HWND;
                              ullCompleted: ULONGLONG;
                              ullTotal: ULONGLONG): HRESULT; stdcall;

    function SetProgressState(hwnd: HWND;
                              tbpFlags: TBPFLAG): HRESULT; stdcall;

    function RegisterTab(hwndTab: HWND;
                         hwndMDI: HWND): HRESULT; stdcall;

    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;

    function SetTabOrder(hwndTab: HWND;
                         hwndInsertBefore: HWND): HRESULT; stdcall;

    function SetTabActive(hwndTab: HWND;
                          hwndMDI: HWND;
                          dwReserved: DWORD): HRESULT; stdcall;

    function ThumbBarAddButtons(hwnd: HWND;
                                cButtons: UINT;
                                pButton: LPTHUMBBUTTON): HRESULT; stdcall;

    function ThumbBarUpdateButtons(hwnd: HWND;
                                   cButtons: UINT;
                                   pButton: LPTHUMBBUTTON): HRESULT; stdcall;

    function ThumbBarSetImageList(hwnd: HWND;
                                  himl: HIMAGELIST): HRESULT; stdcall;

    function SetOverlayIcon(hwnd: HWND;
                            hIcon: HICON;
                            pszDescription: LPCWSTR): HRESULT; stdcall;

    function SetThumbnailTooltip(hwnd: HWND;
                                 pszTip: LPCWSTR): HRESULT; stdcall;

    function SetThumbnailClip(hwnd: HWND;
                              prcClip: PRECT): HRESULT; stdcall;
  end;
  IID_ITaskbarList3 = ITaskbarList3;
  {$EXTERNALSYM IID_ITaskbarList3}


  // Interface ITaskbarList4
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITaskbarList4);'}
  {$EXTERNALSYM ITaskbarList4}
  ITaskbarList4 = interface(ITaskbarList3)
    ['{C43DC798-95D1-4BEA-9030-BB99E2983A1A}']

    function SetTabProperties(hwndTab: HWND;
                              stpFlags: STPFLAG): HRESULT; stdcall;
  end;
  IID_ITaskbarList4 = ITaskbarList4;
  {$EXTERNALSYM IID_ITaskbarList4}


  // Interface IExplorerBrowserEvents
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExplorerBrowserEvents);'}
  {$EXTERNALSYM IExplorerBrowserEvents}
  IExplorerBrowserEvents = interface(IUnknown)
    ['{361BBDC7-E6EE-4E13-BE58-58E2240C810F}']

    function OnNavigationPending(pidlFolder: PCIDLIST_ABSOLUTE): HRESULT; stdcall;

    function OnViewCreated(psv: IShellView): HRESULT; stdcall;

    function OnNavigationComplete(pidlFolder: PCIDLIST_ABSOLUTE): HRESULT; stdcall;

    function OnNavigationFailed(pidlFolder: PCIDLIST_ABSOLUTE): HRESULT; stdcall;
  end;
  IID_IExplorerBrowserEvents = IExplorerBrowserEvents;
  {$EXTERNALSYM IID_IExplorerBrowserEvents}


  // Interface IExplorerBrowser
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExplorerBrowser);'}
  {$EXTERNALSYM IExplorerBrowser}
  IExplorerBrowser = interface(IUnknown)
    ['{DFD3B6B5-C10C-4BE9-85F6-A66969F402F6}']

    function Initialize(hwndParent: HWND;
                        const prc: PRECT;
                        const pfs: PFOLDERSETTINGS): HRESULT; stdcall;

    function Destroy(): HRESULT; stdcall;

    function SetRect(phdwp: PHDWP;
                     rcBrowser: TRECT): HRESULT; stdcall;

    function SetPropertyBag(pszPropertyBag: LPCWSTR): HRESULT; stdcall;

    function SetEmptyText(pszEmptyText: LPCWSTR): HRESULT; stdcall;

    function SetFolderSettings(const pfs: PFOLDERSETTINGS): HRESULT; stdcall;

    function Advise(psbe: IExplorerBrowserEvents;
                    out pdwCookie: DWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;

    function SetOptions(dwFlag: EXPLORER_BROWSER_OPTIONS): HRESULT; stdcall;

    function GetOptions(out pdwFlag: EXPLORER_BROWSER_OPTIONS): HRESULT; stdcall;

    function BrowseToIDList(pidl: PCUIDLIST_RELATIVE;
                            uFlags: UINT): HRESULT; stdcall;

    function BrowseToObject(punk: IUnknown;
                            uFlags: UINT): HRESULT; stdcall;

    function FillFromObject(punk: IUnknown;
                            dwFlags: EXPLORER_BROWSER_FILL_FLAGS): HRESULT; stdcall;

    function RemoveAll(): HRESULT; stdcall;

    function GetCurrentView(const riid: TGUID;
                            out ppv): HRESULT; stdcall;
  end;
  IID_IExplorerBrowser = IExplorerBrowser;
  {$EXTERNALSYM IID_IExplorerBrowser}


  // Interface IEnumObjects
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumObjects);'}
  {$EXTERNALSYM IEnumObjects}
  IEnumObjects = interface(IUnknown)
    ['{2C1C7E2E-2D0E-4059-831E-1E6F82335C2E}']

    function Next(celt: ULONG;
                  const riid: TGUID;
                  rgelt: PPointer;
                  pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenum: IEnumObjects): HRESULT; stdcall;
  end;
  IID_IEnumObjects = IEnumObjects;
  {$EXTERNALSYM IID_IEnumObjects}


  // Interface IOperationsProgressDialog
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOperationsProgressDialog);'}
  {$EXTERNALSYM IOperationsProgressDialog}
  IOperationsProgressDialog = interface(IUnknown)
    ['{0C9FB851-E5C9-43EB-A370-F0677B13874C}']

    function StartProgressDialog(hwndOwner: HWND;
                                 flags: OPPROGDLGF): HRESULT; stdcall;

    function StopProgressDialog(): HRESULT; stdcall;

    function SetOperation(action: SPACTION): HRESULT; stdcall;

    function SetMode(mode: PDMODE): HRESULT; stdcall;

    function UpdateProgress(ullPointsCurrent: ULONGLONG;
                            ullPointsTotal: ULONGLONG;
                            ullSizeCurrent: ULONGLONG;
                            ullSizeTotal: ULONGLONG;
                            ullItemsCurrent: ULONGLONG;
                            ullItemsTotal: ULONGLONG): HRESULT; stdcall;

    function UpdateLocations(psiSource: IShellItem;
                             psiTarget: IShellItem;
                             psiItem: IShellItem): HRESULT; stdcall;

    function ResetTimer(): HRESULT; stdcall;

    function PauseTimer(): HRESULT; stdcall;

    function ResumeTimer(): HRESULT; stdcall;

    function GetMilliseconds(out pullElapsed: ULONGLONG;
                             out pullRemaining: ULONGLONG): HRESULT; stdcall;

    function GetOperationStatus(out popstatus: PDOPSTATUS): HRESULT; stdcall;
  end;
  IID_IOperationsProgressDialog = IOperationsProgressDialog;
  {$EXTERNALSYM IID_IOperationsProgressDialog}


  // Interface IIOCancelInformation
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IIOCancelInformation);'}
  {$EXTERNALSYM IIOCancelInformation}
  IIOCancelInformation = interface(IUnknown)
    ['{F5B0BF81-8CB5-4B1B-9449-1A159E0C733C}']

    function SetCancelInformation(dwThreadID: DWORD;
                                  uMsgCancel: UINT): HRESULT; stdcall;

    function GetCancelInformation(pdwThreadID: PDWORD;
                                  puMsgCancel: PUINT): HRESULT; stdcall;
  end;
  IID_IIOCancelInformation = IIOCancelInformation;
  {$EXTERNALSYM IID_IIOCancelInformation}


  // Interface IFileOperation
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileOperation);'}
  {$EXTERNALSYM IFileOperation}
  IFileOperation = interface(IUnknown)
    ['{947AAB5F-0A5C-4C13-B4D6-4BF7836FC9F8}']

    function Advise(pfops: IFileOperationProgressSink;
                    out pdwCookie: DWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;

    function SetOperationFlags(dwOperationFlags: DWORD): HRESULT; stdcall;

    function SetProgressMessage(pszMessage: LPCWSTR): HRESULT; stdcall;

    function SetProgressDialog(popd: IOperationsProgressDialog): HRESULT; stdcall;

    function SetProperties(pproparray: IPropertyChangeArray): HRESULT; stdcall;

    function SetOwnerWindow(hwndOwner: HWND): HRESULT; stdcall;

    function ApplyPropertiesToItem(psiItem: IShellItem): HRESULT; stdcall;

    function ApplyPropertiesToItems(punkItems: IUnknown): HRESULT; stdcall;

    function RenameItem(psiItem: IShellItem;
                        pszNewName: LPCWSTR;
                        pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;

    function RenameItems(pUnkItems: IUnknown;
                         pszNewName: LPCWSTR): HRESULT; stdcall;

    function MoveItem(psiItem: IShellItem;
                      psiDestinationFolder: IShellItem;
                      pszNewName: LPCWSTR;
                      pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;

    function MoveItems(punkItems: IUnknown;
                       psiDestinationFolder: IShellItem): HRESULT; stdcall;

    function CopyItem(psiItem: IShellItem;
                      psiDestinationFolder: IShellItem;
                      pszCopyName: LPCWSTR;
                      pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;

    function CopyItems(punkItems: IUnknown;
                       psiDestinationFolder: IShellItem): HRESULT; stdcall;

    function DeleteItem(psiItem: IShellItem;
                        pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;

    function DeleteItems(punkItems: IUnknown): HRESULT; stdcall;

    function NewItem(psiDestinationFolder: IShellItem;
                     dwFileAttributes: DWORD;
                     pszName: LPCWSTR;
                     pszTemplateName: LPCWSTR;
                     pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;

    function PerformOperations(): HRESULT; stdcall;

    function GetAnyOperationsAborted(out pfAnyOperationsAborted: BOOL): HRESULT; stdcall;
  end;
  IID_IFileOperation = IFileOperation;
  {$EXTERNALSYM IID_IFileOperation}


  // Interface IFileOperation2
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileOperation2);'}
  {$EXTERNALSYM IFileOperation2}
  IFileOperation2 = interface(IFileOperation)
    ['{CD8F23C1-8F61-4916-909D-55BDD0918753}']

    function SetOperationFlags2(operationFlags2: FILE_OPERATION_FLAGS2): HRESULT; stdcall;
  end;
  IID_IFileOperation2 = IFileOperation2;
  {$EXTERNALSYM IID_IFileOperation2}


  // Interface IObjectProvider
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectProvider);'}
  {$EXTERNALSYM IObjectProvider}
  IObjectProvider = interface(IUnknown)
    ['{A6087428-3BE3-4D73-B308-7C04A540BF1A}']

    function QueryObject(const guidObject: TGUID;
                         const riid: TGUID;
                         out ppvOut): HRESULT; stdcall;
  end;
  IID_IObjectProvider = IObjectProvider;
  {$EXTERNALSYM IID_IObjectProvider}


  // Interface INamespaceWalkCB
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INamespaceWalkCB);'}
  {$EXTERNALSYM INamespaceWalkCB}
  INamespaceWalkCB = interface(IUnknown)
    ['{D92995F8-CF5E-4A76-BF59-EAD39EA2B97E}']

    function FoundItem(psf: IShellFolder;
                       pidl: PCUITEMID_CHILD): HRESULT; stdcall;

    function EnterFolder(psf: IShellFolder;
                         pidl: PCUITEMID_CHILD): HRESULT; stdcall;

    function LeaveFolder(psf: IShellFolder;
                         pidl: PCUITEMID_CHILD): HRESULT; stdcall;

    function InitializeProgressDialog(out ppszTitle: LPWSTR;
                                      out ppszCancel: LPWSTR): HRESULT; stdcall;
  end;
  IID_INamespaceWalkCB = INamespaceWalkCB;
  {$EXTERNALSYM IID_INamespaceWalkCB}


  // Interface INamespaceWalkCB2
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INamespaceWalkCB2);'}
  {$EXTERNALSYM INamespaceWalkCB2}
  INamespaceWalkCB2 = interface(INamespaceWalkCB)
    ['{7AC7492B-C38E-438A-87DB-68737844FF70}']

    function WalkComplete(hr: HRESULT): HRESULT; stdcall;
  end;
  IID_INamespaceWalkCB2 = INamespaceWalkCB2;
  {$EXTERNALSYM IID_INamespaceWalkCB2}


  // Interface INamespaceWalk
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INamespaceWalk);'}
  {$EXTERNALSYM INamespaceWalk}
  INamespaceWalk = interface(IUnknown)
    ['{57CED8A7-3F4A-432C-9350-30F24483F74F}']

    function Walk(punkToWalk: IUnknown;
                  dwFlags: DWORD;
                  cDepth: Integer;
                  pnswcb: INamespaceWalkCB): HRESULT; stdcall;

    function GetIDArrayResult(out pcItems: UINT;
                              out prgpidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;
  end;
  IID_INamespaceWalk = INamespaceWalk;
  {$EXTERNALSYM IID_INamespaceWalk}


  // Interface IBandSite
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBandSite);'}
  {$EXTERNALSYM IBandSite}
  IBandSite = interface(IUnknown)
    ['{4CF504B0-DE96-11D0-8B3F-00A0C911E8E5}']

    function AddBand(punk: IUnknown): HRESULT; stdcall;

    function EnumBands(uBand: UINT;
                       out pdwBandID: DWORD): HRESULT; stdcall;

    function QueryBand(dwBandID: DWORD;
                       out ppstb: IDeskBand;
                       out pdwState: DWORD;
                       pszName: LPWSTR;
                       cchName: Integer): HRESULT; stdcall;

    function SetBandState(dwBandID: DWORD;
                          dwMask: DWORD;
                          dwState: DWORD): HRESULT; stdcall;

    function RemoveBand(dwBandID: DWORD): HRESULT; stdcall;

    function GetBandObject(dwBandID: DWORD;
                           const riid: TGUID;
                           out ppv): HRESULT; stdcall;

    function SetBandSiteInfo(const pbsinfo: BANDSITEINFO): HRESULT; stdcall;

    function GetBandSiteInfo(var pbsinfo: BANDSITEINFO): HRESULT; stdcall;
  end;
  IID_IBandSite = IBandSite;
  {$EXTERNALSYM IID_IBandSite}


  // Interface IModalWindow
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IModalWindow);'}
  {$EXTERNALSYM IModalWindow}
  IModalWindow = interface(IUnknown)
    ['{B4DB1657-70D7-485E-8E3E-6FCB5A5C1802}']

    function Show(hwndOwner: HWND): HRESULT; stdcall;
  end;
  IID_IModalWindow = IModalWindow;
  {$EXTERNALSYM IID_IModalWindow}


  // Interface IContextMenuSite
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IContextMenuSite);'}
  {$EXTERNALSYM IContextMenuSite}
  IContextMenuSite = interface(IUnknown)
    ['{0811AEBE-0B87-4C54-9E72-548CF649016B}']

    function DoContextMenuPopup(punkContextMenu: IUnknown;
                                fFlags: UINT;
                                pt: POINT): HRESULT; stdcall;
  end;
  IID_IContextMenuSite = IContextMenuSite;
  {$EXTERNALSYM IID_IContextMenuSite}


  // Interface IMenuBand
  // ===================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMenuBand);'}
  {$EXTERNALSYM IMenuBand}
  IMenuBand = interface(IUnknown)
    ['{568804CD-CBD7-11D0-9816-00C04FD91972}']

    function IsMenuMessage(pmsg: PMSG): HRESULT; stdcall;

    function TranslateMenuMessage(pmsg: PMSG;
                                  plRet: PLRESULT): HRESULT; stdcall;
  end;
  IID_IMenuBand = IMenuBand;
  {$EXTERNALSYM IID_IMenuBand}


  // Interface IRegTreeItem
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRegTreeItem);'}
  {$EXTERNALSYM IRegTreeItem}
  IRegTreeItem = interface(IUnknown)
    ['{A9521922-0812-4D44-9EC3-7FD38C726F3D}']

    function GetCheckState(pbCheck: PBOOL): HRESULT; stdcall;

    function SetCheckState(bCheck: BOOL): HRESULT; stdcall;
  end;
  IID_IRegTreeItem = IRegTreeItem;
  {$EXTERNALSYM IID_IRegTreeItem}


  // Interface IDeskBar
  // ==================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDeskBar);'}
  {$EXTERNALSYM IDeskBar}
  IDeskBar = interface(IOleWindow)
    ['{EB0FE173-1A3A-11D0-89B3-00A0C90A90AC}']

    function SetClient(punkClient: PIUnknown): HRESULT; stdcall;

    function GetClient(out ppunkClient: PIUnknown): HRESULT; stdcall;

    function OnPosRectChangeDB(prc: PRECT): HRESULT; stdcall;
  end;
  IID_IDeskBar = IDeskBar;
  {$EXTERNALSYM IID_IDeskBar}


  // Interface IMenuPopup
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IMenuPopup);'}
  {$EXTERNALSYM IMenuPopup}
  IMenuPopup = interface(IDeskBar)
    ['{D1E7AFEB-6A2E-11D0-8C78-00C04FD918B4}']

    function Popup(ppt: PPOINTL;
                   prcExclude: PRECTL;
                   dwFlags: MP_POPUPFLAGS): HRESULT; stdcall;

    function OnSelect(dwSelectType: DWORD): HRESULT; stdcall;

    function SetSubMenu(pmp: IMenuPopup;
                        fSet: BOOL): HRESULT; stdcall;
  end;
  IID_IMenuPopup = IMenuPopup;
  {$EXTERNALSYM IID_IMenuPopup}


  // Interface IFileIsInUse
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileIsInUse);'}
  {$EXTERNALSYM IFileIsInUse}
  IFileIsInUse = interface(IUnknown)
    ['{64A1CBF0-3A1A-4461-9158-376969693950}']

    function GetAppName(out ppszName: LPWSTR): HRESULT; stdcall;

    function GetUsage(out pfut: FILE_USAGE_TYPE): HRESULT; stdcall;

    function GetCapabilities(out pdwCapFlags: DWORD): HRESULT; stdcall;

    function GetSwitchToHWND(out phwnd: HWND): HRESULT; stdcall;

    function CloseFile(): HRESULT; stdcall;
  end;
  IID_IFileIsInUse = IFileIsInUse;
  {$EXTERNALSYM IID_IFileIsInUse}


  // Interface IFileDialogEvents
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileDialogEvents);'}
  {$EXTERNALSYM IFileDialogEvents}
  IFileDialogEvents = interface(IUnknown)
    ['{973510DB-7D7F-452B-8975-74A85828D354}']

    function OnFileOk(pfd: IFileDialog): HRESULT; stdcall;

    function OnFolderChanging(pfd: IFileDialog;
                              psiFolder: IShellItem): HRESULT; stdcall;

    function OnFolderChange(pfd: IFileDialog): HRESULT; stdcall;

    function OnSelectionChange(pfd: IFileDialog): HRESULT; stdcall;

    function OnShareViolation(pfd: IFileDialog;
                              psi: IShellItem;
                              out pResponse: FDE_SHAREVIOLATION_RESPONSE): HRESULT; stdcall;

    function OnTypeChange(pfd: IFileDialog): HRESULT; stdcall;

    function OnOverwrite(pfd: IFileDialog;
                         psi: IShellItem;
                         out pResponse: FDE_OVERWRITE_RESPONSE): HRESULT; stdcall;
  end;
  IID_IFileDialogEvents = IFileDialogEvents;
  {$EXTERNALSYM IID_IFileDialogEvents}


  // Interface IFileDialog
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileDialog);'}
  {$EXTERNALSYM IFileDialog}
  IFileDialog = interface(IModalWindow)
    ['{42F85136-DB7E-439C-85F1-E4075D135FC8}']

    function SetFileTypes(cFileTypes: UINT;
                          const rgFilterSpec: PCOMDLG_FILTERSPEC): HRESULT; stdcall;

    function SetFileTypeIndex(iFileType: UINT): HRESULT; stdcall;

    function GetFileTypeIndex(out piFileType: UINT): HRESULT; stdcall;

    function Advise(pfde: IFileDialogEvents;
                    out pdwCookie: DWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;

    function SetOptions(fos: FILEOPENDIALOGOPTIONS): HRESULT; stdcall;

    function GetOptions(out pfos: FILEOPENDIALOGOPTIONS): HRESULT; stdcall;

    function SetDefaultFolder(psi: IShellItem): HRESULT; stdcall;

    function SetFolder(psi: IShellItem): HRESULT; stdcall;

    function GetFolder(out ppsi: IShellItem): HRESULT; stdcall;

    function GetCurrentSelection(out ppsi: IShellItem): HRESULT; stdcall;

    function SetFileName(pszName: LPCWSTR): HRESULT; stdcall;

    function GetFileName(out pszName: LPWSTR): HRESULT; stdcall;

    function SetTitle(pszTitle: LPCWSTR): HRESULT; stdcall;

    function SetOkButtonLabel(pszText: LPCWSTR): HRESULT; stdcall;

    function SetFileNameLabel(pszLabel: LPCWSTR): HRESULT; stdcall;

    function GetResult(out ppsi: IShellItem): HRESULT; stdcall;

    function AddPlace(psi: IShellItem;
                      fdap: FDAP): HRESULT; stdcall;

    function SetDefaultExtension(pszDefaultExtension: LPCWSTR): HRESULT; stdcall;

    function Close(hr: HRESULT): HRESULT; stdcall;

    function SetClientGuid(const guid: TGUID): HRESULT; stdcall;

    function ClearClientData(): HRESULT; stdcall;

    function SetFilter(pFilter: IShellItemFilter): HRESULT; stdcall;
  end;
  IID_IFileDialog = IFileDialog;
  {$EXTERNALSYM IID_IFileDialog}


  // Interface IFileSaveDialog
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileSaveDialog);'}
  {$EXTERNALSYM IFileSaveDialog}
  IFileSaveDialog = interface(IFileDialog)
    ['{84BCCD23-5FDE-4CDB-AEA4-AF64B83D78AB}']

    function SetSaveAsItem(psi: IShellItem): HRESULT; stdcall;

    function SetProperties(pStore: IPropertyStore): HRESULT; stdcall;

    function SetCollectedProperties(pList: IPropertyDescriptionList;
                                    fAppendDefault: BOOL): HRESULT; stdcall;

    function GetProperties(out ppStore: IPropertyStore): HRESULT; stdcall;

    function ApplyProperties(psi: IShellItem;
                             pStore: IPropertyStore;
                             hwnd: HWND;
                             pSink: IFileOperationProgressSink): HRESULT; stdcall;
  end;
  IID_IFileSaveDialog = IFileSaveDialog;
  {$EXTERNALSYM IID_IFileSaveDialog}


  // Interface IFileOpenDialog
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileOpenDialog);'}
  {$EXTERNALSYM IFileOpenDialog}
  IFileOpenDialog = interface(IFileDialog)
    ['{D57C7288-D4AD-4768-BE02-9D969532D960}']

    function GetResults(out ppenum: IShellItemArray): HRESULT; stdcall;

    function GetSelectedItems(out ppsai: IShellItemArray): HRESULT; stdcall;
  end;
  IID_IFileOpenDialog = IFileOpenDialog;
  {$EXTERNALSYM IID_IFileOpenDialog}


  // Interface IFileDialogCustomize
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileDialogCustomize);'}
  {$EXTERNALSYM IFileDialogCustomize}
  IFileDialogCustomize = interface(IUnknown)
    ['{E6FDD21A-163F-4975-9C8C-A69F1BA37034}']

    // Methods for adding or enabling controls. All of these can have their
    // enabled/visible state set, however the default is for them to be enabled and visible,
    // so this parameter has been left off these methods.
    function EnableOpenDropDown(dwIDCtl: DWORD): HRESULT; stdcall;

    function AddMenu(dwIDCtl: DWORD;
                     pszLabel: LPCWSTR): HRESULT; stdcall;

    function AddPushButton(dwIDCtl: DWORD;
                           pszLabel: LPCWSTR): HRESULT; stdcall;

    function AddComboBox(dwIDCtl: DWORD): HRESULT; stdcall;

    function AddRadioButtonList(dwIDCtl: DWORD): HRESULT; stdcall;

    function AddCheckButton(dwIDCtl: DWORD;
                            pszLabel: LPCWSTR;
                            bChecked: BOOL): HRESULT; stdcall;

    function AddEditBox(dwIDCtl: DWORD;
                        pszText: LPCWSTR): HRESULT; stdcall;

    function AddSeparator(dwIDCtl: DWORD): HRESULT; stdcall;

    function AddText(dwIDCtl: DWORD;
                     pszText: LPCWSTR): HRESULT; stdcall;

    // Getting/setting attributes on controls on the fly
    function SetControlLabel(dwIDCtl: DWORD;
                             pszLabel: LPCWSTR): HRESULT; stdcall;

    function GetControlState(dwIDCtl: DWORD;
                             out pdwState: CDCONTROLSTATEF): HRESULT; stdcall;

    function SetControlState(dwIDCtl: DWORD;
                             dwState: CDCONTROLSTATEF): HRESULT; stdcall;

    function GetEditBoxText(dwIDCtl: DWORD;
                            out ppszText: LPWSTR): HRESULT; stdcall;

    function SetEditBoxText(dwIDCtl: DWORD;
                            pszText: LPCWSTR): HRESULT; stdcall;

    function GetCheckButtonState(dwIDCtl: DWORD;
                                 out pbChecked: BOOL): HRESULT; stdcall;

    function SetCheckButtonState(dwIDCtl: DWORD;
                                 bChecked: BOOL): HRESULT; stdcall;

    // Method for adding items to "container controls" (radiogroup, combobox, opendropdown, toolsmenu)
    function AddControlItem(dwIDCtl: DWORD;
                            dwIDItem: DWORD;
                            pszLabel: LPCWSTR): HRESULT; stdcall;

    // Methods for removing items in the "container controls"
    function RemoveControlItem(dwIDCtl: DWORD;
                               dwIDItem: DWORD): HRESULT; stdcall;

    function RemoveAllControlItems(dwIDCtl: DWORD): HRESULT; stdcall;

    // Getting/setting attributes on control items on the fly
    // Items are considered immutable once created, except for their state:
    function GetControlItemState(dwIDCtl: DWORD;
                                 dwIDItem: DWORD;
                                 out pdwState: CDCONTROLSTATEF): HRESULT; stdcall;

    function SetControlItemState(dwIDCtl: DWORD;
                                 dwIDItem: DWORD;
                                 dwState: CDCONTROLSTATEF): HRESULT; stdcall;

    // Methods for some "container controls": OpenDropDown, combobox, radiobuttongroup.
    // These methods don't apply to the tools menu.
    // These methods can be called after the dialog has closed, to determine the users final choice.
    // For comboboxes and radiobuttongroups, these methods may also be called while the
    // dialog is showing (which makes no sense for the OpenDropDown).
    function GetSelectedControlItem(dwIDCtl: DWORD;
                                    out pdwIDItem: DWORD): HRESULT; stdcall;

    function SetSelectedControlItem(dwIDCtl: DWORD;
                                    dwIDItem: DWORD): HRESULT; stdcall; // Not valid for OpenDropDown

    // Controls can be grouped by wrapping their adds in StartVisualGroup/EndVisualGroup
    // Groups have control IDs, and can be disabled/hidden, just like other controls.
    function StartVisualGroup(dwIDCtl: DWORD;
                              pszLabel: LPCWSTR): HRESULT; stdcall;

    function EndVisualGroup(): HRESULT; stdcall;

    // One control may be marked as appearing prominently in the UI
    function MakeProminent(dwIDCtl: DWORD): HRESULT; stdcall;

    // Set the text of a control item (RadioButton, or item in an OpenDropDown or Menu)
    function SetControlItemText(dwIDCtl: DWORD;
                                dwIDItem: DWORD;
                                pszLabel: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IFileDialogCustomize = IFileDialogCustomize;
  {$EXTERNALSYM IID_IFileDialogCustomize}


  // Interface IApplicationAssociationRegistration
  // =============================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IApplicationAssociationRegistration);'}
  {$EXTERNALSYM IApplicationAssociationRegistration}
  IApplicationAssociationRegistration = interface(IUnknown)
    ['{4E530B0A-E611-4C77-A3AC-9031D022281B}']

    function QueryCurrentDefault(pszQuery: LPCWSTR;
                                 atQueryType: ASSOCIATIONTYPE;
                                 alQueryLevel: ASSOCIATIONLEVEL;
                                 out ppszAssociation: LPWSTR): HRESULT; stdcall;

    function QueryAppIsDefault(pszQuery: LPCWSTR;
                               atQueryType: ASSOCIATIONTYPE;
                               alQueryLevel: ASSOCIATIONLEVEL;
                               pszAppRegistryName: LPCWSTR;
                               out pfDefault: BOOL): HRESULT; stdcall;

    function QueryAppIsDefaultAll(alQueryLevel: ASSOCIATIONLEVEL;
                                  pszAppRegistryName: LPCWSTR;
                                  out pfDefault: BOOL): HRESULT; stdcall;

    function SetAppAsDefault(pszAppRegistryName: LPCWSTR;
                             pszSet: LPCWSTR;
                             atSetType: ASSOCIATIONTYPE): HRESULT; stdcall;

    function SetAppAsDefaultAll(pszAppRegistryName: LPCWSTR): HRESULT; stdcall;

    function ClearUserAssociations(): HRESULT; stdcall;
  end;
  IID_IApplicationAssociationRegistration = IApplicationAssociationRegistration;
  {$EXTERNALSYM IID_IApplicationAssociationRegistration}


  // Interface IDelegateFolder
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDelegateFolder);'}
  {$EXTERNALSYM IDelegateFolder}
  IDelegateFolder = interface(IUnknown)
    ['{ADD8BA80-002B-11D0-8F0F-00C04FD7D062}']

    function SetItemAlloc(pmalloc: IMalloc): HRESULT; stdcall;
  end;
  IID_IDelegateFolder = IDelegateFolder;
  {$EXTERNALSYM IID_IDelegateFolder}


  // Interface IBrowserFrameOptions
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBrowserFrameOptions);'}
  {$EXTERNALSYM IBrowserFrameOptions}
  IBrowserFrameOptions = interface(IUnknown)
    ['{10DF43C8-1DBE-11D3-8B34-006097DF5BD4}']

    function GetFrameOptions(dwMask: BROWSERFRAMEOPTIONS;
                             pdwOptions: PBROWSERFRAMEOPTIONS): HRESULT; stdcall;
  end;
  IID_IBrowserFrameOptions = IBrowserFrameOptions;
  {$EXTERNALSYM IID_IBrowserFrameOptions}


  // Interface INewWindowManager
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INewWindowManager);'}
  {$EXTERNALSYM INewWindowManager}
  INewWindowManager = interface(IUnknown)
    ['{D2BC4C84-3F72-4A52-A604-7BCBF3982CBB}']

    function EvaluateNewWindow(pszUrl: LPCWSTR;
                               pszName: LPCWSTR;
                               pszUrlContext: LPCWSTR;
                               pszFeatures: LPCWSTR;
                               fReplace: BOOL;
                               dwFlags: DWORD;
                               dwUserActionTime: DWORD): HRESULT; stdcall;
  end;
  IID_INewWindowManager = INewWindowManager;
  {$EXTERNALSYM IID_INewWindowManager}


  // Interface IAttachmentExecute
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAttachmentExecute);'}
  {$EXTERNALSYM IAttachmentExecute}
  IAttachmentExecute = interface(IUnknown)
    ['{73DB1241-1E85-4581-8E4F-A81E1D0F8C57}']

    //
    //  IAttachmentExecute - COM object designed to help client applications
    //      safely manage saving and opening attachments for users.
    //      clients are assumed to have some policy/settings already
    //      to determine the support and behavior for attachments.
    //      this API assumes that the client is interactive with the user
    //
    //  ClientTitle - (optional) caller specific title for the prompt
    //    if unset, the prompts come with a default title of "File Download"
    function SetClientTitle(pszTitle: LPCWSTR): HRESULT; stdcall;

    //  ClientGuid - (optional) for storing user specific settings
    //      someprompts are allowed to be avoided in the future if the user
    //      chooses.  that choice is stored on per-client basis indexed by the ClientGuid
    //
    //      Specific Example: In the User Trust Prompt there is a check box that is checked
    //      by default, but may be unchecked by the user.  this option is stored under the ClientGuid
    //      based on the file type.
    //
    //      ClearClientState() will reset any user options stored on the clients behalf.
    function SetClientGuid(const guid: TGUID): HRESULT; stdcall;

    //  EVIDENCE properties

    //  LocalPath - (REQUIRED) path that would be passed to ShellExecute()
    //      if FileName was already used for the Check() and Prompt() calls,
    //      and the LocalPath points to a different handler than predicted,
    //      previous trust may be revoked, and the Policy and User trust re-verified.
    function SetLocalPath(pszLocalPath: LPCWSTR): HRESULT; stdcall;

    //  FileName - (optional) proposed name (not path) to be used to construct LocalPath
    //  optionally use this if the caller wants to perform Check() before copying
    //  the file to the LocalPath.  (eg, Check() proposed download)
    function SetFileName(pszFileName: LPCWSTR): HRESULT; stdcall;

    //  Source - (optional) alternate identity path or URL for a file transfer
    //      used as the primary Zone determinant.  if this is NULL default to the Restricted Zone.
    //      may also be used in the Prompt() UI for the "From" field
    //      may also be sent to handlers that can process URLs
    function SetSource(pszSource: LPCWSTR): HRESULT; stdcall;

    //  Referrer - (optional) Zone determinant for container or link types
    //      only used for Zone/Policy
    //      container formats like ZIP and OLE packager use the Referrer to
    //      indicate indirect inheritance and avoid Zone elevation.
    //      Shortcuts can also use it to limit elevation based on parameters
    function SetReferrer(pszReferrer: LPCWSTR): HRESULT; stdcall;

    //  CheckPolicy() - examines available evidence and checks the resultant policy
    //      * requires FileName or LocalPath
    //
    //  Returns S_OK for enable
    //          S_FALSE for prompt
    //          FAILURE for disable
    //
    function CheckPolicy(): HRESULT; stdcall;

    //  Prompt() - application can force UI at an earlier point,
    //      even before the file has been copied to disk
    //      * requires FileName or LocalPath
    function Prompt(hwnd: HWND;
                    prompt: ATTACHMENT_PROMPT;
                    out paction: ATTACHMENT_ACTION): HRESULT; stdcall;

    //  Save() - should always be called if LocalPath is in not in a temp dir
    //      * requires valid LocalPath
    //      * called after the file has been copied to LocalPath
    //      * may run virus scanners or other trust services to validate the file.
    //          these services may delete or alter the file
    //      * may attach evidence to the LocalPath
    function Save(): HRESULT; stdcall;

    //  Execute() - will call Prompt() if necessary, with the EXEC action
    //      * requires valid LocalPath
    //      * called after the file has been copied to LocalPath
    //      * may run virus scanners or other trust services to validate the file.
    //          these services may delete or alter the file
    //      * may attach evidence to the LocalPath
    //
    //      phProcess - if non-NULL Execute() will be synchronous and return an HPROCESS if available
    //                  if null Execute() will be async, implies that you have a message pump and a long lived window
    //
    function Execute(hwnd: HWND;
                     pszVerb: LPCWSTR;
                     phProcess: PHANDLE): HRESULT; stdcall;

    //   SaveWithUI() - superset of Save() that can show modal error UI, but still does not call Prompt()
    //      * requires valid LocalPath
    //      * called after the file has been copied to LocalPath
    //      * may run virus scanners or other trust services to validate the file.
    //          these services may delete or alter the file
    //      * may attach evidence to the LocalPath
    function SaveWithUI(hwnd: HWND): HRESULT; stdcall;

    //  ClearClientState() - removes any state that is stored based on the ClientGuid
    //      * requires SetClientGuid() to be called first
    function ClearClientState(): HRESULT; stdcall;
  end;
  IID_IAttachmentExecute = IAttachmentExecute;
  {$EXTERNALSYM IID_IAttachmentExecute}


  // Interface IAttachmentExecute2
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAttachmentExecute2);'}
  {$EXTERNALSYM IAttachmentExecute2}
  IAttachmentExecute2 = interface(IAttachmentExecute)
    ['{4F2B781F-A608-4543-ABF0-49C246EBBBA9}']

    function SaveNoVirusCheck(): HRESULT; stdcall;

    function SaveWithUINoVirusCheck(hwnd: HWND): HRESULT; stdcall;
  end;
  IID_IAttachmentExecute2 = IAttachmentExecute2;
  {$EXTERNALSYM IID_IAttachmentExecute2}


  // Interface IShellMenuCallback
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellMenuCallback);'}
  {$EXTERNALSYM IShellMenuCallback}
  IShellMenuCallback = interface(IUnknown)
    ['{4CA300A1-9B8D-11D1-8B22-00C04FD918D0}']

    function CallbackSM(psmd: LPSMDATA;
                        uMsg: UINT;
                        wParam: WPARAM;
                        lParam: LPARAM): HRESULT; stdcall;
  end;
  IID_IShellMenuCallback = IShellMenuCallback;
  {$EXTERNALSYM IID_IShellMenuCallback}


  // Interface IShellMenu
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellMenu);'}
  {$EXTERNALSYM IShellMenu}
  IShellMenu = interface(IUnknown)
    ['{EE1F7637-E138-11D1-8379-00C04FD918D0}']

    function Initialize(psmc: IShellMenuCallback;
                        uId: UINT;
                        uIdAncestor: UINT;
                        dwFlags: DWORD): HRESULT; stdcall;

    function GetMenuInfo(out ppsmc: IShellMenuCallback;
                         puId: PUINT;
                         puIdAncestor: PUINT;
                         pdwFlags: PDWORD): HRESULT; stdcall;

    function SetShellFolder(psf: IShellFolder;
                            pidlFolder: PCIDLIST_ABSOLUTE;
                            hKey: HKEY;
                            dwFlags: DWORD): HRESULT; stdcall;

    function GetShellFolder(out pdwFlags: DWORD;
                            out ppidl: PIDLIST_ABSOLUTE;
                            const riid: TGUID;
                            out ppv): HRESULT; stdcall;

    function SetMenu(hmenu: HMENU;
                     hwnd: HWND;
                     dwFlags: DWORD): HRESULT; stdcall;

    function GetMenu(phmenu: HMENU;
                     phwnd: PHWND;
                     pdwFlags: PDWORD): HRESULT; stdcall;

    function InvalidateItem(psmd: LPSMDATA;
                            dwFlags: DWORD): HRESULT; stdcall;

    function GetState(psmd: LPSMDATA): HRESULT; stdcall;

    function SetMenuToolbar(punk: IUnknown;
                            dwFlags: DWORD): HRESULT; stdcall;
  end;
  IID_IShellMenu = IShellMenu;
  {$EXTERNALSYM IID_IShellMenu}


  // Interface IKnownFolder
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKnownFolder);'}
  {$EXTERNALSYM IKnownFolder}
  IKnownFolder = interface(IUnknown)
    ['{3AA7AF7E-9B36-420C-A8E3-F77D4674A488}']

    function GetId(out pkfid: KNOWNFOLDERID): HRESULT; stdcall;

    function GetCategory(out pCategory: KF_CATEGORY): HRESULT; stdcall;

    function GetShellItem(dwFlags: DWORD;
                          const riid: TGUID;
                          out ppv): HRESULT; stdcall;

    function GetPath(dwFlags: DWORD;
                     out ppszPath: LPWSTR): HRESULT; stdcall;

    function SetPath(dwFlags: DWORD;
                     pszPath: LPCWSTR): HRESULT; stdcall;

    function GetIDList(dwFlags: DWORD;
                       out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;

    function GetFolderType(out pftid: FOLDERTYPEID): HRESULT; stdcall;

    function GetRedirectionCapabilities(out pCapabilities: KF_REDIRECTION_CAPABILITIES): HRESULT; stdcall;

    function GetFolderDefinition(out pKFD: KNOWNFOLDER_DEFINITION): HRESULT; stdcall;
  end;
  IID_IKnownFolder = IKnownFolder;
  {$EXTERNALSYM IID_IKnownFolder}


  // Interface IKnownFolderManager
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IKnownFolderManager);'}
  {$EXTERNALSYM IKnownFolderManager}
  IKnownFolderManager = interface(IUnknown)
    ['{8BE2D872-86AA-4D47-B776-32CCA40C7018}']

    function FolderIdFromCsidl(nCsidl: Integer;
                               out pfid: KNOWNFOLDERID): HRESULT; stdcall;

    function FolderIdToCsidl(const rfid: KNOWNFOLDERID;
                             out pnCsidl: Integer): HRESULT; stdcall;

    function GetFolderIds(out ppKFId: PKNOWNFOLDERID;
                          var pCount: UINT): HRESULT; stdcall;

    function GetFolder(const rfid: KNOWNFOLDERID;
                       out ppkf: IKnownFolder): HRESULT; stdcall;

    function GetFolderByName(pszCanonicalName: LPCWSTR;
                             out ppkf: IKnownFolder): HRESULT; stdcall;

    function RegisterFolder(const rfid: KNOWNFOLDERID;
                            const pKFD: PKNOWNFOLDER_DEFINITION): HRESULT; stdcall;

    function UnregisterFolder(const rfid: KNOWNFOLDERID): HRESULT; stdcall;

    function FindFolderFromPath(pszPath: LPCWSTR;
                                mode: FFFP_MODE;
                                out ppkf: IKnownFolder): HRESULT; stdcall;

    function FindFolderFromIDList(pidl: PCIDLIST_ABSOLUTE;
                                  out ppkf: IKnownFolder): HRESULT; stdcall;

    function Redirect(const rfid: KNOWNFOLDERID;
                      hwnd: HWND;
                      flags: KF_REDIRECT_FLAGS;
                      pszTargetPath: LPCWSTR;
                      cFolders: UINT;
                      const pExclusion: PKNOWNFOLDERID;
                      out ppszError: LPWSTR): HRESULT; stdcall;
  end;
  IID_IKnownFolderManager = IKnownFolderManager;
  {$EXTERNALSYM IID_IKnownFolderManager}


  // Interface ISharingConfigurationManager
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISharingConfigurationManager);'}
  {$EXTERNALSYM ISharingConfigurationManager}
  ISharingConfigurationManager = interface(IUnknown)
    ['{B4CD448A-9C86-4466-9201-2E62105B87AE}']

    function CreateShare(dsid: DEF_SHARE_ID;
                         role: SHARE_ROLE): HRESULT; stdcall;

    function DeleteShare(dsid: DEF_SHARE_ID): HRESULT; stdcall;

    function ShareExists(dsid: DEF_SHARE_ID): HRESULT; stdcall;

    function GetSharePermissions(dsid: DEF_SHARE_ID;
                                 out pRole: SHARE_ROLE): HRESULT; stdcall;

    function SharePrinters(): HRESULT; stdcall;

    function StopSharingPrinters(): HRESULT; stdcall;

    function ArePrintersShared(): HRESULT; stdcall;
  end;
  IID_ISharingConfigurationManager = ISharingConfigurationManager;
  {$EXTERNALSYM IID_ISharingConfigurationManager}


  // Interface IRelatedItem
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IRelatedItem);'}
  {$EXTERNALSYM IRelatedItem}
  IRelatedItem = interface(IUnknown)
    ['{A73CE67A-8AB1-44F1-8D43-D2FCBF6B1CD0}']

    function GetItemIDList(out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;

    function GetItem(out ppsi: IShellItem): HRESULT; stdcall;
  end;
  IID_IRelatedItem = IRelatedItem;
  {$EXTERNALSYM IID_IRelatedItem}


  // Interface IIdentityName
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IIdentityName);'}
  {$EXTERNALSYM IIdentityName}
  IIdentityName = interface(IRelatedItem)
    ['{7D903FCA-D6F9-4810-8332-946C0177E247}']
  end;
  IID_IIdentityName = IIdentityName;
  {$EXTERNALSYM IID_IIdentityName}


  // Interface IDelegateItem
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDelegateItem);'}
  {$EXTERNALSYM IDelegateItem}
  IDelegateItem = interface(IRelatedItem)
    ['{3C5A1C94-C951-4CB7-BB6D-3B93F30CCE93}']
  end;
  IID_IDelegateItem = IDelegateItem;
  {$EXTERNALSYM IID_IDelegateItem}


  // Interface ICurrentItem
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICurrentItem);'}
  {$EXTERNALSYM ICurrentItem}
  ICurrentItem = interface(IRelatedItem)
    ['{240A7174-D653-4A1D-A6D3-D4943CFBFE3D}']
  end;
  IID_ICurrentItem = ICurrentItem;
  {$EXTERNALSYM IID_ICurrentItem}


  // Interface ITransferMediumItem
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITransferMediumItem);'}
  {$EXTERNALSYM ITransferMediumItem}
  ITransferMediumItem = interface(IRelatedItem)
    ['{77F295D5-2D6F-4E19-B8AE-322F3E721AB5}']
  end;
  IID_ITransferMediumItem = ITransferMediumItem;
  {$EXTERNALSYM IID_ITransferMediumItem}


  // Interface IDisplayItem
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDisplayItem);'}
  {$EXTERNALSYM IDisplayItem}
  IDisplayItem = interface(IRelatedItem)
    ['{C6FD5997-9F6B-4888-8703-94E80E8CDE3F}']
  end;
  IID_IDisplayItem = IDisplayItem;
  {$EXTERNALSYM IID_IDisplayItem}


  // Interface IViewStateIdentityItem
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IViewStateIdentityItem);'}
  {$EXTERNALSYM IViewStateIdentityItem}
  IViewStateIdentityItem = interface(IRelatedItem)
    ['{9D264146-A94F-4195-9F9F-3BB12CE0C955}']
  end;
  IID_IViewStateIdentityItem = IViewStateIdentityItem;
  {$EXTERNALSYM IID_IViewStateIdentityItem}


  // Interface IPreviewItem
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPreviewItem);'}
  {$EXTERNALSYM IPreviewItem}
  IPreviewItem = interface(IRelatedItem)
    ['{36149969-0A8F-49C8-8B00-4AECB20222FB}']
  end;
  IID_IPreviewItem = IPreviewItem;
  {$EXTERNALSYM IID_IPreviewItem}


  // Interface IDestinationStreamFactory
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDestinationStreamFactory);'}
  {$EXTERNALSYM IDestinationStreamFactory}
  IDestinationStreamFactory = interface(IUnknown)
    ['{8A87781B-39A7-4A1F-AAB3-A39B9C34A7D9}']

    function GetDestinationStream(out ppstm: PIStream): HRESULT; stdcall;
  end;
  IID_IDestinationStreamFactory = IDestinationStreamFactory;
  {$EXTERNALSYM IID_IDestinationStreamFactory}


  // Interface ICreateProcessInputs
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICreateProcessInputs);'}
  {$EXTERNALSYM ICreateProcessInputs}
  ICreateProcessInputs = interface(IUnknown)
    ['{F6EF6140-E26F-4D82-BAC4-E9BA5FD239A8}']

    function GetCreateFlags(pdwCreationFlags: PDWORD): HRESULT; stdcall;

    function SetCreateFlags(dwCreationFlags: DWORD): HRESULT; stdcall;

    function AddCreateFlags(dwCreationFlags: DWORD): HRESULT; stdcall;

    function SetHotKey(wHotKey: WORD): HRESULT; stdcall;

    function AddStartupFlags(dwStartupInfoFlags: DWORD): HRESULT; stdcall;

    function SetTitle(pszTitle: LPCWSTR): HRESULT; stdcall;

    function SetEnvironmentVariable(pszName: LPCWSTR;
                     pszValue: LPCWSTR): HRESULT; stdcall;
  end;
  IID_ICreateProcessInputs = ICreateProcessInputs;
  {$EXTERNALSYM IID_ICreateProcessInputs}


  // Interface ICreatingProcess
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICreatingProcess);'}
  {$EXTERNALSYM ICreatingProcess}
  ICreatingProcess = interface(IUnknown)
    ['{C2B937A9-3110-4398-8A56-F34C6342D244}']

    function OnCreating(pcpi: ICreateProcessInputs): HRESULT; stdcall;
  end;
  IID_ICreatingProcess = ICreatingProcess;
  {$EXTERNALSYM IID_ICreatingProcess}


  // Interface ILaunchUIContext
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ILaunchUIContext);'}
  {$EXTERNALSYM ILaunchUIContext}
  ILaunchUIContext = interface(IUnknown)
    ['{1791E8F6-21C7-4340-882A-A6A93E3FD73B}']

    function SetAssociatedWindow(value: HWND): HRESULT; stdcall;

    function SetTabGroupingPreference(value: DWORD): HRESULT; stdcall;
  end;
  IID_ILaunchUIContext = ILaunchUIContext;
  {$EXTERNALSYM IID_ILaunchUIContext}


  // Interface ILaunchUIContextProvider
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ILaunchUIContextProvider);'}
  {$EXTERNALSYM ILaunchUIContextProvider}
  ILaunchUIContextProvider = interface(IUnknown)
    ['{0D12C4C8-A3D9-4E24-94C1-0E20C5A956C4}']

    function UpdateContext(context: ILaunchUIContext): HRESULT; stdcall;
  end;
  IID_ILaunchUIContextProvider = ILaunchUIContextProvider;
  {$EXTERNALSYM IID_ILaunchUIContextProvider}


  // Interface INewMenuClient
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INewMenuClient);'}
  {$EXTERNALSYM INewMenuClient}
  INewMenuClient = interface(IUnknown)
    ['{DCB07FDC-3BB5-451C-90BE-966644FED7B0}']

    function IncludeItems(out pflags: NMCII_FLAGS): HRESULT; stdcall;

    function SelectAndEditItem(pidlItem: PCIDLIST_ABSOLUTE;
                               flags: NMCSAEI_FLAGS): HRESULT; stdcall;
  end;
  IID_INewMenuClient = INewMenuClient;
  {$EXTERNALSYM IID_INewMenuClient}


  // Interface IInitializeWithBindCtx
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeWithBindCtx);'}
  {$EXTERNALSYM IInitializeWithBindCtx}
  IInitializeWithBindCtx = interface(IUnknown)
    ['{71C0D2BC-726D-45CC-A6C0-2E31C1DB2159}']

    function Initialize(pbc: IBindCtx): HRESULT; stdcall;
  end;
  IID_IInitializeWithBindCtx = IInitializeWithBindCtx;
  {$EXTERNALSYM IID_IInitializeWithBindCtx}


  // Interface IShellItemFilter
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellItemFilter);'}
  {$EXTERNALSYM IShellItemFilter}
  IShellItemFilter = interface(IUnknown)
    ['{2659B475-EEB8-48B7-8F07-B378810F48CF}']

    function IncludeItem(psi: IShellItem): HRESULT; stdcall;

    function GetEnumFlagsForItem(psi: IShellItem;
                                 out pgrfFlags: SHCONTF): HRESULT; stdcall;
  end;
  IID_IShellItemFilter = IShellItemFilter;
  {$EXTERNALSYM IID_IShellItemFilter}


  // Interface INameSpaceTreeControl
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INameSpaceTreeControl);'}
  {$EXTERNALSYM INameSpaceTreeControl}
  INameSpaceTreeControl = interface(IUnknown)
    ['{028212A3-B627-47E9-8856-C14265554E4F}']

    function Initialize(hwndParent: HWND;
                        prc: PRECT;
                        nsctsFlags: NSTCSTYLE): HRESULT; stdcall;

    function TreeAdvise(punk: IUnknown;
                        out pdwCookie: DWORD): HRESULT; stdcall;

    function TreeUnadvise(dwCookie: DWORD): HRESULT; stdcall;

    function AppendRoot(psiRoot: IShellItem;
                        grfEnumFlags: SHCONTF;
                        grfRootStyle: NSTCROOTSTYLE;
                        pif: IShellItemFilter): HRESULT; stdcall;

    function InsertRoot(iIndex: Integer;
                        psiRoot: IShellItem;
                        grfEnumFlags: SHCONTF;
                        grfRootStyle: NSTCROOTSTYLE;
                        pif: IShellItemFilter): HRESULT; stdcall;

    function RemoveRoot(psiRoot: IShellItem): HRESULT; stdcall;

    function RemoveAllRoots(): HRESULT; stdcall;

    function GetRootItems(out ppsiaRootItems: IShellItemArray): HRESULT; stdcall;

    function SetItemState(psi: IShellItem;
                          nstcisMask: NSTCITEMSTATE;
                          nstcisFlags: NSTCITEMSTATE): HRESULT; stdcall;

    function GetItemState(psi: IShellItem;
                          nstcisMask: NSTCITEMSTATE;
                          out pnstcisFlags: NSTCITEMSTATE): HRESULT; stdcall;

    function GetSelectedItems(out psiaItems: IShellItemArray): HRESULT; stdcall;

    function GetItemCustomState(psi: IShellItem;
                                out piStateNumber: Integer): HRESULT; stdcall;

    function SetItemCustomState(psi: IShellItem;
                                iStateNumber: Integer): HRESULT; stdcall;

    function EnsureItemVisible(psi: IShellItem): HRESULT; stdcall;

    function SetTheme(pszTheme: LPCWSTR): HRESULT; stdcall;

    function GetNextItem(psi: IShellItem;
                         nstcgi: NSTCGNI;
                         out ppsiNext: IShellItem): HRESULT; stdcall;

    function HitTest(ppt: PPOINT;
                     out ppsiOut: IShellItem): HRESULT; stdcall;

    function GetItemRect(psi: IShellItem;
                         out prect: TRECT): HRESULT; stdcall;

    function CollapseAll(): HRESULT; stdcall;
  end;
  IID_INameSpaceTreeControl = INameSpaceTreeControl;
  {$EXTERNALSYM IID_INameSpaceTreeControl}


  // Interface INameSpaceTreeControlFolderCapabilities
  // =================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INameSpaceTreeControlFolderCapabilities);'}
  {$EXTERNALSYM INameSpaceTreeControlFolderCapabilities}
  INameSpaceTreeControlFolderCapabilities = interface(IUnknown)
    ['{E9701183-E6B3-4FF2-8568-813615FEC7BE}']

    function GetFolderCapabilities(nfcMask: NSTCFOLDERCAPABILITIES;
                             {out} pnfcValue: PNSTCFOLDERCAPABILITIES): HRESULT; stdcall;
  end;
  IID_INameSpaceTreeControllFolderCapabilities = INameSpaceTreeControlFolderCapabilities;
  {$EXTERNALSYM IID_INameSpaceTreeControllFolderCapabilities}


  // Interface IPreviewHandler
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPreviewHandler);'}
  {$EXTERNALSYM IPreviewHandler}
  IPreviewHandler = interface(IUnknown)
    ['{8895B1C6-B41F-4C1C-A562-0D564250836F}']

    function SetWindow(hwnd: HWND;
                       const prc: PRECT): HRESULT; stdcall;

    function SetRect(const prc: PRECT): HRESULT; stdcall;

    function DoPreview(): HRESULT; stdcall;

    function Unload(): HRESULT; stdcall;

    function SetFocus(): HRESULT; stdcall;

    function QueryFocus(out phwnd: HWND): HRESULT; stdcall;

    function TranslateAccelerator(pmsg: PMSG): HRESULT; stdcall;
  end;
  IID_IPreviewHandler = IPreviewHandler;
  {$EXTERNALSYM IID_IPreviewHandler}


  // Interface IPreviewHandlerFrame
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPreviewHandlerFrame);'}
  {$EXTERNALSYM IPreviewHandlerFrame}
  IPreviewHandlerFrame = interface(IUnknown)
    ['{FEC87AAF-35F9-447A-ADB7-20234491401A}']

    function GetWindowContext(out pinfo: PREVIEWHANDLERFRAMEINFO): HRESULT; stdcall;

    function TranslateAccelerator(pmsg: PMSG): HRESULT; stdcall;
  end;
  IID_IPreviewHandlerFrame = IPreviewHandlerFrame;
  {$EXTERNALSYM IID_IPreviewHandlerFrame}


  // Interface IExplorerPaneVisibility
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExplorerPaneVisibility);'}
  {$EXTERNALSYM IExplorerPaneVisibility}
  IExplorerPaneVisibility = interface(IUnknown)
    ['{E07010EC-BC17-44C0-97B0-46C7C95B9EDC}']

    function GetPaneState(const ep: TGUID;
                          out peps: EXPLORERPANESTATE): HRESULT; stdcall;
  end;
  IID_IExplorerPaneVisibility = IExplorerPaneVisibility;
  {$EXTERNALSYM IID_IExplorerPaneVisibility}


  // Interface IContextMenuCB
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IContextMenuCB);'}
  {$EXTERNALSYM IContextMenuCB}
  IContextMenuCB = interface(IUnknown)
    ['{3409E930-5A39-11D1-83FA-00A0C90DC849}']

    function CallBack(psf: IShellFolder;
                     hwndOwner: HWND;
                     pdtobj: PIDataObject;
                     uMsg: UINT;
                     wParam: WPARAM;
                     lParam: LPARAM): HRESULT; stdcall;
  end;
  IID_IContextMenuCB = IContextMenuCB;
  {$EXTERNALSYM IID_IContextMenuCB}


  // Interface IDefaultExtractIconInit
  // =================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDefaultExtractIconInit);'}
  {$EXTERNALSYM IDefaultExtractIconInit}
  IDefaultExtractIconInit = interface(IUnknown)
    ['{41DED17D-D6B3-4261-997D-88C60E4B1D58}']

    function SetFlags(uFlags: UINT): HRESULT; stdcall;

    function SetKey(hkey: HKEY): HRESULT; stdcall;

    function SetNormalIcon(pszFile: LPCWSTR;
                     iIcon: Integer): HRESULT; stdcall;

    function SetOpenIcon(pszFile: LPCWSTR;
                     iIcon: Integer): HRESULT; stdcall;

    function SetShortcutIcon(pszFile: LPCWSTR;
                     iIcon: Integer): HRESULT; stdcall;

    function SetDefaultIcon(pszFile: LPCWSTR;
                     iIcon: Integer): HRESULT; stdcall;
  end;
  IID_IDefaultExtractIconInit = IDefaultExtractIconInit;
  {$EXTERNALSYM IID_IDefaultExtractIconInit}


  // Interface IExplorerCommand
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExplorerCommand);'}
  {$EXTERNALSYM IExplorerCommand}
  IExplorerCommand = interface(IUnknown)
    ['{A08CE4D0-FA25-44AB-B57C-C7B1C323E0B9}']

    function GetTitle(psiItemArray: IShellItemArray;
                      out ppszName: LPWSTR): HRESULT; stdcall;

    function GetIcon(psiItemArray: IShellItemArray;
                     out ppszIcon: LPWSTR): HRESULT; stdcall;

    function GetToolTip(psiItemArray: IShellItemArray;
                        out ppszInfotip: LPWSTR): HRESULT; stdcall;

    function GetCanonicalName(out pguidCommandName: TGUID): HRESULT; stdcall;

    function GetState(psiItemArray: IShellItemArray;
                      fOkToBeSlow: BOOL;
                      out pCmdState: EXPCMDSTATE): HRESULT; stdcall;

    function Invoke(psiItemArray: IShellItemArray;
                    pbc: IBindCtx): HRESULT; stdcall;

    function GetFlags(out pFlags: EXPCMDFLAGS): HRESULT; stdcall;

    function EnumSubCommands(out ppEnum: IEnumExplorerCommand): HRESULT; stdcall;
  end;
  IID_IExplorerCommand = IExplorerCommand;
  {$EXTERNALSYM IID_IExplorerCommand}


  // Interface IExplorerCommandState
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExplorerCommandState);'}
  {$EXTERNALSYM IExplorerCommandState}
  IExplorerCommandState = interface(IUnknown)
    ['{BDDACB60-7657-47AE-8445-D23E1ACF82AE}']

    function GetState(psiItemArray: IShellItemArray;
                      fOkToBeSlow: BOOL;
                      out pCmdState: EXPCMDSTATE): HRESULT; stdcall;
  end;
  IID_IExplorerCommandState = IExplorerCommandState;
  {$EXTERNALSYM IID_IExplorerCommandState}


  // Interface IInitializeCommand
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeCommand);'}
  {$EXTERNALSYM IInitializeCommand}
  IInitializeCommand = interface(IUnknown)
    ['{85075ACF-231F-40EA-9610-D26B7B58F638}']

    function Initialize(pszCommandName: LPCWSTR;
                        ppb: IPropertyBag): HRESULT; stdcall;
  end;
  IID_IInitializeCommand = IInitializeCommand;
  {$EXTERNALSYM IID_IInitializeCommand}


  // Interface IEnumExplorerCommand
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumExplorerCommand);'}
  {$EXTERNALSYM IEnumExplorerCommand}
  IEnumExplorerCommand = interface(IUnknown)
    ['{A88826F8-186F-4987-AADE-EA0CEF8FBFE8}']

    function Next(celt: ULONG;
                  pUICommand: IExplorerCommand;
                  pceltFetched: PULONG): HRESULT; stdcall;

    function Skip(celt: ULONG): HRESULT; stdcall;

    function Reset(): HRESULT; stdcall;

    function Clone(out ppenum: IEnumExplorerCommand): HRESULT; stdcall;
  end;
  IID_IEnumExplorerCommand = IEnumExplorerCommand;
  {$EXTERNALSYM IID_IEnumExplorerCommand}


  // Interface IExplorerCommandProvider
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExplorerCommandProvider);'}
  {$EXTERNALSYM IExplorerCommandProvider}
  IExplorerCommandProvider = interface(IUnknown)
    ['{64961751-0835-43C0-8FFE-D57686530E64}']

    function GetCommands(punkSite: IUnknown;
                         const riid: TGUID;
                         out ppv): HRESULT; stdcall;

    function GetCommand(const rguidCommandId: TGUID;
                        const riid: TGUID;
                        out ppv): HRESULT; stdcall;
  end;
  IID_IExplorerCommandProvider = IExplorerCommandProvider;
  {$EXTERNALSYM IID_IExplorerCommandProvider}


  // Interface IOpenControlPanel
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOpenControlPanel);'}
  {$EXTERNALSYM IOpenControlPanel}
  IOpenControlPanel = interface(IUnknown)
    ['{D11AD862-66DE-4DF4-BF6C-1F5621996AF1}']

    function Open(pszName: LPCWSTR;
                  pszPage: LPCWSTR;
                  punkSite: IUnknown): HRESULT; stdcall;

    function GetPath(pszName: LPCWSTR;
                     pszPath: LPWSTR;
                     cchPath: UINT): HRESULT; stdcall;

    function GetCurrentView(out pView: CPVIEW): HRESULT; stdcall;
  end;
  IID_IOpenControlPanel = IOpenControlPanel;
  {$EXTERNALSYM IID_IOpenControlPanel}


  // Interface IFileSystemBindData
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileSystemBindData);'}
  {$EXTERNALSYM IFileSystemBindData}
  IFileSystemBindData = interface(IUnknown)
    ['{01E18D10-4D8B-11D2-855D-006008059367}']

    function SetFindData(const pfd: WIN32_FIND_DATAW): HRESULT; stdcall;

    function GetFindData(out pfd: WIN32_FIND_DATAW): HRESULT; stdcall;
  end;
  IID_IFileSystemBindData = IFileSystemBindData;
  {$EXTERNALSYM IID_IFileSystemBindData}


  // Interface IFileSystemBindData2
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileSystemBindData2);'}
  {$EXTERNALSYM IFileSystemBindData2}
  IFileSystemBindData2 = interface(IFileSystemBindData)
    ['{3ACF075F-71DB-4AFA-81F0-3FC4FDF2A5B8}']

    function SetFileID(liFileID: LARGE_INTEGER): HRESULT; stdcall;

    function GetFileID(out pliFileID: LARGE_INTEGER): HRESULT; stdcall;

    function SetJunctionCLSID(const clsid: TGUID): HRESULT; stdcall;

    function GetJunctionCLSID(out pclsid: CLSID): HRESULT; stdcall;
  end;
  IID_IFileSystemBindData2 = IFileSystemBindData2;
  {$EXTERNALSYM IID_IFileSystemBindData2}


  // Interface ICustomDestinationList
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICustomDestinationList);'}
  {$EXTERNALSYM ICustomDestinationList}
  ICustomDestinationList = interface(IUnknown)
    ['{6332DEBF-87B5-4670-90C0-5E57B408A49E}']

    function SetAppID(pszAppID: LPCWSTR): HRESULT; stdcall;

    function BeginList(out pcMinSlots: UINT;
                       const riid: TGUID;
                       out ppv): HRESULT; stdcall;

    function AppendCategory(pszCategory: LPCWSTR;
                            poa: IObjectArray): HRESULT; stdcall;

    function AppendKnownCategory(category: KNOWNDESTCATEGORY): HRESULT; stdcall;

    function AddUserTasks(poa: IObjectArray): HRESULT; stdcall;

    function CommitList(): HRESULT; stdcall;

    function GetRemovedDestinations(const riid: TGUID;
                                    out ppv): HRESULT; stdcall;

    function DeleteList(pszAppID: LPCWSTR): HRESULT; stdcall;

    function AbortList(): HRESULT; stdcall;
  end;
  IID_ICustomDestinationList = ICustomDestinationList;
  {$EXTERNALSYM IID_ICustomDestinationList}


  // Interface IApplicationDestinations
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IApplicationDestinations);'}
  {$EXTERNALSYM IApplicationDestinations}
  IApplicationDestinations = interface(IUnknown)
    ['{12337D35-94C6-48A0-BCE7-6A9C69D4D600}']

    function SetAppID(pszAppID: LPCWSTR): HRESULT; stdcall;

    function RemoveDestination(punk: IUnknown): HRESULT; stdcall;

    function RemoveAllDestinations(): HRESULT; stdcall;
  end;
  IID_IApplicationDestinations = IApplicationDestinations;
  {$EXTERNALSYM IID_IApplicationDestinations}


  // Interface IApplicationDocumentLists
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IApplicationDocumentLists);'}
  {$EXTERNALSYM IApplicationDocumentLists}
  IApplicationDocumentLists = interface(IUnknown)
    ['{3C594F9F-9F30-47A1-979A-C9E83D3D0A06}']

    function SetAppID(pszAppID: LPCWSTR): HRESULT; stdcall;

    function GetList(listtype: APPDOCLISTTYPE;
                     cItemsDesired: UINT;
                     const riid: TGUID;
                     out ppv): HRESULT; stdcall;
  end;
  IID_IApplicationDocumentLists = IApplicationDocumentLists;
  {$EXTERNALSYM IID_IApplicationDocumentLists}


  // Interface IObjectWithAppUserModelID
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithAppUserModelID);'}
  {$EXTERNALSYM IObjectWithAppUserModelID}
  IObjectWithAppUserModelID = interface(IUnknown)
    ['{36DB0196-9665-46D1-9BA7-D3709EECF9ED}']

    function SetAppID(pszAppID: LPCWSTR): HRESULT; stdcall;

    function GetAppID(ppszAppID: LPWSTR): HRESULT; stdcall;
  end;
  IID_IObjectWithAppUserModelID = IObjectWithAppUserModelID;
  {$EXTERNALSYM IID_IObjectWithAppUserModelID}


  // Interface IObjectWithPackageFullName
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithPackageFullName);'}
  {$EXTERNALSYM IObjectWithPackageFullName}
  IObjectWithPackageFullName = interface(IUnknown)
    ['{ED2AA515-602F-469C-A130-CE69FD0FA878}']

    function GetPackageFullName(packageFullName: LPWSTR): HRESULT; stdcall;
  end;
  IID_IObjectWithPackageFullName = IObjectWithPackageFullName;
  {$EXTERNALSYM IID_IObjectWithPackageFullName}


  // Interface IObjectWithProgID
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectWithProgID);'}
  {$EXTERNALSYM IObjectWithProgID}
  IObjectWithProgID = interface(IUnknown)
    ['{71E806FB-8DEE-46FC-BF8C-7748A8A1AE13}']

    function SetProgID(pszProgID: LPCWSTR): HRESULT; stdcall;

    function GetProgID(ppszProgID: LPWSTR): HRESULT; stdcall;
  end;
  IID_IObjectWithProgID = IObjectWithProgID;
  {$EXTERNALSYM IID_IObjectWithProgID}


  // Interface IUpdateIDList
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUpdateIDList);'}
  {$EXTERNALSYM IUpdateIDList}
  IUpdateIDList = interface(IUnknown)
    ['{6589B6D2-5F8D-4B9E-B7E0-23CDD9717D8C}']

    function Update(pbc: PIBindCtx;
                    pidlIn: PCUITEMID_CHILD;
                    ppidlOut: PITEMID_CHILD): HRESULT; stdcall;
  end;
  IID_IUpdateIDList = IUpdateIDList;
  {$EXTERNALSYM IID_IUpdateIDList}


  // Interface IDesktopWallpaper
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDesktopWallpaper);'}
  {$EXTERNALSYM IDesktopWallpaper}
  IDesktopWallpaper = interface(IUnknown)
    ['{B92B56A9-8B55-4E14-9A89-0199BBB6F93B}']

    function SetWallpaper(monitorID: LPCWSTR;
                          wallpaper: LPCWSTR): HRESULT; stdcall;

    function GetWallpaper(monitorID: LPCWSTR;
                          out wallpaper: LPWSTR): HRESULT; stdcall;

    function GetMonitorDevicePathAt(monitorIndex: UINT;
                                    out monitorID: LPWSTR): HRESULT; stdcall;

    function GetMonitorDevicePathCount(out count: UINT): HRESULT; stdcall;

    function GetMonitorRECT(monitorID: LPCWSTR;
                            out displayRect: TRect): HRESULT; stdcall;

    function SetBackgroundColor(color: COLORREF): HRESULT; stdcall;

    function GetBackgroundColor(out color: COLORREF): HRESULT; stdcall;

    function SetPosition(position: DESKTOP_WALLPAPER_POSITION): HRESULT; stdcall;

    function GetPosition(out position: DESKTOP_WALLPAPER_POSITION): HRESULT; stdcall;

    function SetSlideshow(items: IShellItemArray): HRESULT; stdcall;

    function GetSlideshow(out items: IShellItemArray): HRESULT; stdcall;

    function SetSlideshowOptions(options: DESKTOP_SLIDESHOW_OPTIONS;
                                 slideshowTick: UINT): HRESULT; stdcall;

    function GetSlideshowOptions(out options: DESKTOP_SLIDESHOW_OPTIONS;
                                 out slideshowTick: UINT): HRESULT; stdcall;

    function AdvanceSlideshow(monitorID: LPCWSTR;
                              direction: DESKTOP_SLIDESHOW_DIRECTION): HRESULT; stdcall;

    function GetStatus(out state: DESKTOP_SLIDESHOW_STATE): HRESULT; stdcall;

    function Enable(enable: BOOL): HRESULT; stdcall;
  end;
  IID_IDesktopWallpaper = IDesktopWallpaper;
  {$EXTERNALSYM IID_IDesktopWallpaper}


  // Interface IHomeGroup
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHomeGroup);'}
  {$EXTERNALSYM IHomeGroup}
  IHomeGroup = interface(IUnknown)
    ['{7A3BD1D9-35A9-4FB3-A467-F48CAC35E2D0}']

    function IsMember(member: PBOOL): HRESULT; stdcall;

    function ShowSharingWizard(owner: HWND;
                     sharingchoices: PHOMEGROUPSHARINGCHOICES): HRESULT; stdcall;
  end;
  IID_IHomeGroup = IHomeGroup;
  {$EXTERNALSYM IID_IHomeGroup}


  // Interface IInitializeWithPropertyStore
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeWithPropertyStore);'}
  {$EXTERNALSYM IInitializeWithPropertyStore}
  IInitializeWithPropertyStore = interface(IUnknown)
    ['{C3E12EB5-7D8D-44F8-B6DD-0E77B34D6DE4}']

    function Initialize(pps: IPropertyStore): HRESULT; stdcall;
  end;
  IID_IInitializeWithPropertyStore = IInitializeWithPropertyStore;
  {$EXTERNALSYM IID_IInitializeWithPropertyStore}


  // Interface IOpenSearchSource
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOpenSearchSource);'}
  {$EXTERNALSYM IOpenSearchSource}
  IOpenSearchSource = interface(IUnknown)
    ['{F0EE7333-E6FC-479B-9F25-A860C234A38E}']

    function GetResults(hwnd: HWND;
                        pszQuery: LPCWSTR;
                        dwStartIndex: DWORD;
                        dwCount: DWORD;
                        const riid: TGUID;
                        out ppv): HRESULT; stdcall;
  end;
  IID_IOpenSearchSource = IOpenSearchSource;
  {$EXTERNALSYM IID_IOpenSearchSource}


  // Interface IShellLibrary
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellLibrary);'}
  {$EXTERNALSYM IShellLibrary}
  IShellLibrary = interface(IUnknown)
    ['{11A66EFA-382E-451A-9234-1E0E12EF3085}']

    function LoadLibraryFromItem(psiLibrary: IShellItem;
                                 grfMode: DWORD): HRESULT; stdcall;

    function LoadLibraryFromKnownFolder(const kfidLibrary: KNOWNFOLDERID;
                                        grfMode: DWORD): HRESULT; stdcall;

    function AddFolder(psiLocation: IShellItem): HRESULT; stdcall;

    function RemoveFolder(psiLocation: IShellItem): HRESULT; stdcall;

    function GetFolders(lff: LIBRARYFOLDERFILTER;
                        const riid: TGUID;
                        out ppv): HRESULT; stdcall;

    function ResolveFolder(psiFolderToResolve: IShellItem;
                           dwTimeout: DWORD;
                           const riid: TGUID;
                           out ppv): HRESULT; stdcall;

    function GetDefaultSaveFolder(dsft: DEFAULTSAVEFOLDERTYPE;
                                  const riid: TGUID;
                                  out ppv): HRESULT; stdcall;

    function SetDefaultSaveFolder(dsft: DEFAULTSAVEFOLDERTYPE;
                                  psi: IShellItem): HRESULT; stdcall;

    function GetOptions(out plofOptions: LIBRARYOPTIONFLAGS): HRESULT; stdcall;

    function SetOptions(lofMask: LIBRARYOPTIONFLAGS;
                        lofOptions: LIBRARYOPTIONFLAGS): HRESULT; stdcall;

    function GetFolderType(out pftid: FOLDERTYPEID): HRESULT; stdcall;

    function SetFolderType(const ftid: FOLDERTYPEID): HRESULT; stdcall;

    function GetIcon(out ppszIcon: LPWSTR): HRESULT; stdcall;

    function SetIcon(pszIcon: LPCWSTR): HRESULT; stdcall;

    function Commit(): HRESULT; stdcall;

    function Save(psiFolderToSaveIn: IShellItem;
                  pszLibraryName: LPCWSTR;
                  lsf: LIBRARYSAVEFLAGS;
                  out ppsiSavedTo: IShellItem): HRESULT; stdcall;

    function SaveInKnownFolder(const kfidToSaveIn: KNOWNFOLDERID;
                               pszLibraryName: LPCWSTR;
                               lsf: LIBRARYSAVEFLAGS;
                               out ppsiSavedTo: IShellItem): HRESULT; stdcall;
  end;
  IID_IShellLibrary = IShellLibrary;
  {$EXTERNALSYM IID_IShellLibrary}


  // Interface IDefaultFolderMenuInitialize
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDefaultFolderMenuInitialize);'}
  {$EXTERNALSYM IDefaultFolderMenuInitialize}
  IDefaultFolderMenuInitialize = interface(IUnknown)
    ['{7690AA79-F8FC-4615-A327-36F7D18F5D91}']

    function Initialize(hwnd: HWND;
                        pcmcb: IContextMenuCB;
                        pidlFolder: PCIDLIST_ABSOLUTE;
                        psf: IShellFolder;
                        cidl: UINT;
                        apidl: PCUITEMID_CHILD_ARRAY;
                        punkAssociation: IUnknown;
                        cKeys: UINT;
                        aKeys: PHKEY): HRESULT; stdcall;

    function SetMenuRestrictions(dfmrValues: DEFAULT_FOLDER_MENU_RESTRICTIONS): HRESULT; stdcall;

    function GetMenuRestrictions(dfmrMask: DEFAULT_FOLDER_MENU_RESTRICTIONS;
                                 out pdfmrValues: DEFAULT_FOLDER_MENU_RESTRICTIONS): HRESULT; stdcall;

    function SetHandlerClsid(const rclsid: TGUID): HRESULT; stdcall;
  end;
  IID_IDefaultFolderMenuInitialize = IDefaultFolderMenuInitialize;
  {$EXTERNALSYM IID_IDefaultFolderMenuInitialize}


  // Interface IApplicationActivationManager
  // =======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IApplicationActivationManager);'}
  {$EXTERNALSYM IApplicationActivationManager}
  IApplicationActivationManager = interface(IUnknown)
    ['{2E941141-7F97-4756-BA1D-9DECDE894A3D}']

    function ActivateApplication(appUserModelId: LPCWSTR;
                                 arguments: LPCWSTR;
                                 options: ACTIVATEOPTIONS;
                                 out processId: DWORD): HRESULT; stdcall;

    function ActivateForFile(appUserModelId: LPCWSTR;
                             itemArray: IShellItemArray;
                             verb: LPCWSTR;
                             out processId: DWORD): HRESULT; stdcall;

    function ActivateForProtocol(appUserModelId: LPCWSTR;
                                 itemArray: IShellItemArray;
                                 out processId: DWORD): HRESULT; stdcall;
  end;
  IID_IApplicationActivationManager = IApplicationActivationManager;
  {$EXTERNALSYM IID_IApplicationActivationManager}


  // Interface IVirtualDesktopManager
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVirtualDesktopManager);'}
  {$EXTERNALSYM IVirtualDesktopManager}
  IVirtualDesktopManager = interface(IUnknown)
    ['{A5CD92FF-29BE-454C-8D04-D82879FB3F1B}']

    function IsWindowOnCurrentVirtualDesktop(topLevelWindow: HWND;
                                             out onCurrentDesktop: BOOL): HRESULT; stdcall;

    function GetWindowDesktopId(topLevelWindow: HWND;
                                out desktopId: TGUID): HRESULT; stdcall;

    function MoveWindowToDesktop(topLevelWindow: HWND;
                                 const desktopId: TGUID): HRESULT; stdcall;

  end;
  IID_IVirtualDesktopManager = IVirtualDesktopManager;
  {$EXTERNALSYM IID_IVirtualDesktopManager}


  // Interface IAssocHandlerInvoker
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAssocHandlerInvoker);'}
  {$EXTERNALSYM IAssocHandlerInvoker}
  IAssocHandlerInvoker = interface(IUnknown)
    ['{92218CAB-ECAA-4335-8133-807FD234C2EE}']

    function SupportsSelection: HRESULT; stdcall;

    function Invoke: HRESULT; stdcall;
  end;
  IID_IAssocHandlerInvoker = IAssocHandlerInvoker;
  {$EXTERNALSYM IID_IAssocHandlerInvoker}


  // Interface IAssocHandler
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAssocHandler);'}
  {$EXTERNALSYM IAssocHandler}
  IAssocHandler = interface(IUnknown)
    ['{F04061AC-1659-4A3F-A954-775AA57FC083}']

    function GetName(ppsz: LPWSTR): HRESULT; stdcall;

    function GetUIName(ppsz: LPWSTR): HRESULT; stdcall;

    function GetIconLocation(ppszPath: LPWSTR;
                     pIndex: PInteger): HRESULT; stdcall;

    function IsRecommended: HRESULT; stdcall;

    function MakeDefault(pszDescription: LPCWSTR): HRESULT; stdcall;

    function Invoke(pdo: PIDataObject): HRESULT; stdcall;

    function CreateInvoker(pdo: PIDataObject;
                     out ppInvoker: IAssocHandlerInvoker): HRESULT; stdcall;
  end;
  IID_IAssocHandler = IAssocHandler;
  {$EXTERNALSYM IID_IAssocHandler}


  // Interface IEnumAssocHandlers
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumAssocHandlers);'}
  {$EXTERNALSYM IEnumAssocHandlers}
  IEnumAssocHandlers = interface(IUnknown)
    ['{973810AE-9599-4B88-9E4D-6EE98C9552DA}']

    function Next(celt: ULONG;
                     out rgelt: IAssocHandler;
                     pceltFetched: PULONG): HRESULT; stdcall;
  end;
  IID_IEnumAssocHandlers = IEnumAssocHandlers;
  {$EXTERNALSYM IID_IEnumAssocHandlers}


  // Interface IDataObjectProvider
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDataObjectProvider);'}
  {$EXTERNALSYM IDataObjectProvider}
  IDataObjectProvider = interface(IUnknown)
    ['{3D25F6D6-4B2A-433C-9184-7C33AD35D001}']

    function GetDataObject(out dataObject: IDataObject): HRESULT; stdcall;

    function SetDataObject(dataObject: IDataObject): HRESULT; stdcall;
  end;
  IID_IDataObjectProvider = IDataObjectProvider;
  {$EXTERNALSYM IID_IDataObjectProvider}


  // Interface IDataTransferManagerInterop
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDataTransferManagerInterop);'}
  {$EXTERNALSYM IDataTransferManagerInterop}
  IDataTransferManagerInterop = interface(IUnknown)
    ['{3A3DCD6C-3EAB-43DC-BCDE-45671CE800C8}']

    function GetForWindow(appWindow: HWND;
                          const riid: TGUID;
                          out dataTransferManager): HRESULT; stdcall;

    function ShowShareUIForWindow(appWindow: HWND): HRESULT; stdcall;
  end;
  IID_IDataTransferManagerInterop = IDataTransferManagerInterop;
  {$EXTERNALSYM IID_IDataTransferManagerInterop}


  // Interface IFrameworkInputPaneHandler
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFrameworkInputPaneHandler);'}
  {$EXTERNALSYM IFrameworkInputPaneHandler}
  IFrameworkInputPaneHandler = interface(IUnknown)
    ['{226C537B-1E76-4D9E-A760-33DB29922F18}']

    function Showing(prcInputPaneScreenLocation: PRECT;
                     fEnsureFocusedElementInView: BOOL): HRESULT; stdcall;

    function Hiding(fEnsureFocusedElementInView: BOOL): HRESULT; stdcall;
  end;
  IID_IFrameworkInputPaneHandler = IFrameworkInputPaneHandler;
  {$EXTERNALSYM IID_IFrameworkInputPaneHandler}


  // Interface IFrameworkInputPane
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFrameworkInputPane);'}
  {$EXTERNALSYM IFrameworkInputPane}
  IFrameworkInputPane = interface(IUnknown)
    ['{5752238B-24F0-495A-82F1-2FD593056796}']

    function Advise(pWindow: IUnknown;
                    pHandler: IFrameworkInputPaneHandler;
                    out pdwCookie: DWORD): HRESULT; stdcall;

    function AdviseWithHWND(hwnd: HWND;
                            pHandler: IFrameworkInputPaneHandler;
                            out pdwCookie: DWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;

    function Location(out prcInputPaneScreenLocation: TRECT): HRESULT; stdcall;

  end;
  IID_IFrameworkInputPane = IFrameworkInputPane;
  {$EXTERNALSYM IID_IFrameworkInputPane}


  // Interface IAppVisibilityEvents
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAppVisibilityEvents);'}
  {$EXTERNALSYM IAppVisibilityEvents}
  IAppVisibilityEvents = interface(IUnknown)
    ['{6584CE6B-7D82-49C2-89C9-C6BC02BA8C38}']

    function AppVisibilityOnMonitorChanged(hMonitor: HMONITOR;
                                           previousMode: MONITOR_APP_VISIBILITY;
                                           currentMode: MONITOR_APP_VISIBILITY): HRESULT; stdcall;

    function LauncherVisibilityChange(currentVisibleState: BOOL): HRESULT; stdcall;
  end;
  IID_IAppVisibilityEvents = IAppVisibilityEvents;
  {$EXTERNALSYM IID_IAppVisibilityEvents}


  // Interface IAppVisibility
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAppVisibility);'}
  {$EXTERNALSYM IAppVisibility}
  IAppVisibility = interface(IUnknown)
    ['{2246EA2D-CAEA-4444-A3C4-6DE827E44313}']

    function GetAppVisibilityOnMonitor(hMonitor: HMONITOR;
                                       out pMode: MONITOR_APP_VISIBILITY): HRESULT; stdcall;

    function IsLauncherVisible(out pfVisible: BOOL): HRESULT; stdcall;

    function Advise(pCallback: IAppVisibilityEvents;
                    out pdwCookie: DWORD): HRESULT; stdcall;

    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;
  end;
  IID_IAppVisibility = IAppVisibility;
  {$EXTERNALSYM IID_IAppVisibility}


  // Interface IPackageExecutionStateChangeNotification
  // ==================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPackageExecutionStateChangeNotification);'}
  {$EXTERNALSYM IPackageExecutionStateChangeNotification}
  IPackageExecutionStateChangeNotification = interface(IUnknown)
    ['{1BB12A62-2AD8-432B-8CCF-0C2C52AFCD5B}']

    function OnStateChanged(pszPackageFullName: LPCWSTR;
                            pesNewState: PACKAGE_EXECUTION_STATE): HRESULT; stdcall;
  end;
  IID_IPackageExecutionStateChangeNotification =
    IPackageExecutionStateChangeNotification;
  {$EXTERNALSYM IID_IPackageExecutionStateChangeNotification}


  // Interface IPackageDebugSettings
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPackageDebugSettings);'}
  {$EXTERNALSYM IPackageDebugSettings}
  IPackageDebugSettings = interface(IUnknown)
    ['{F27C3930-8029-4AD1-94E3-3DBA417810C1}']

    function EnableDebugging(packageFullName: LPCWSTR;
                             debuggerCommandLine: LPCWSTR;
                             environment: PZZWSTR): HRESULT; stdcall;

    function DisableDebugging(packageFullName: LPCWSTR): HRESULT; stdcall;

    function Suspend(packageFullName: LPCWSTR): HRESULT; stdcall;

    function Resume(packageFullName: LPCWSTR): HRESULT; stdcall;

    function TerminateAllProcesses(packageFullName: LPCWSTR): HRESULT; stdcall;

    function SetTargetSessionId(sessionId: ULONG): HRESULT; stdcall;

    function EnumerateBackgroundTasks(packageFullName: LPCWSTR;
                                      out taskCount: ULONG;
                                      out taskIds: LPCGUID;
                                      out taskNames: LPCWSTR): HRESULT; stdcall;

    function ActivateBackgroundTask(taskId: LPCGUID): HRESULT; stdcall;

    function StartServicing(packageFullName: LPCWSTR): HRESULT; stdcall;

    function StopServicing(packageFullName: LPCWSTR): HRESULT; stdcall;

    function StartSessionRedirection(packageFullName: LPCWSTR;
                                     sessionId: ULONG): HRESULT; stdcall;

    function StopSessionRedirection(packageFullName: LPCWSTR): HRESULT; stdcall;

    function GetPackageExecutionState(packageFullName: LPCWSTR;
                                      out packageExecutionState: PACKAGE_EXECUTION_STATE): HRESULT; stdcall;

    function RegisterForPackageStateChanges(packageFullName: LPCWSTR;
                                            pPackageExecutionStateChangeNotification: IPackageExecutionStateChangeNotification;
                                            out pdwCookie: DWORD): HRESULT; stdcall;

    function UnregisterForPackageStateChanges(dwCookie: DWORD): HRESULT; stdcall;
  end;
  IID_IPackageDebugSettings = IPackageDebugSettings;
  {$EXTERNALSYM IID_IPackageDebugSettings}


  // Interface IPackageDebugSettings2
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPackageDebugSettings2);'}
  {$EXTERNALSYM IPackageDebugSettings2}
  IPackageDebugSettings2 = interface(IPackageDebugSettings)
    ['{6E3194BB-AB82-4D22-93F5-FABDA40E7B16}']

    function EnumerateApps(packageFullName: LPCWSTR;
                           appCount: PULONG;
                           out appUserModelIds: PLPWSTR;
                          out appDisplayNames: PLPWSTR): HRESULT; stdcall;
  end;
  IID_IPackageDebugSettings2 = IPackageDebugSettings2;
  {$EXTERNALSYM IID_IPackageDebugSettings2}


  // Interface ISuspensionDependencyManager
  // ======================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISuspensionDependencyManager);'}
  {$EXTERNALSYM ISuspensionDependencyManager}
  ISuspensionDependencyManager = interface(IUnknown)
    ['{52B83A42-2543-416A-81D9-C0DE7969C8B3}']

    function RegisterAsChild(processHandle: THANDLE): HRESULT; stdcall;

    function GroupChildWithParent(childProcessHandle: THANDLE): HRESULT; stdcall;

    function UngroupChildFromParent(childProcessHandle: THANDLE): HRESULT; stdcall;
  end;
  IID_ISuspensionDependencyManager = ISuspensionDependencyManager;
  {$EXTERNALSYM IID_ISuspensionDependencyManager}


  // Interface IExecuteCommandApplicationHostEnvironment
  // ===================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExecuteCommandApplicationHostEnvironment);'}
  {$EXTERNALSYM IExecuteCommandApplicationHostEnvironment}
  IExecuteCommandApplicationHostEnvironment = interface(IUnknown)
    ['{18B21AA9-E184-4FF0-9F5E-F882D03771B3}']

    function GetValue(out pahe: AHE_TYPE): HRESULT; stdcall;
  end;
  IID_IExecuteCommandApplicationHostEnvironment = IExecuteCommandApplicationHostEnvironment;
  {$EXTERNALSYM IID_IExecuteCommandApplicationHostEnvironment}


  // Interface IExecuteCommandHost
  // =============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IExecuteCommandHost);'}
  {$EXTERNALSYM IExecuteCommandHost}
  IExecuteCommandHost = interface(IUnknown)
    ['{4B6832A2-5F04-4C9D-B89D-727A15D103E7}']

    function GetUIMode(out pUIMode: EC_HOST_UI_MODE): HRESULT; stdcall;
  end;
  IID_IExecuteCommandHost = IExecuteCommandHost;
  {$EXTERNALSYM IID_IExecuteCommandHost}


  // Interface IApplicationDesignModeSettings
  // ========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IApplicationDesignModeSettings);'}
  {$EXTERNALSYM IApplicationDesignModeSettings}
  IApplicationDesignModeSettings = interface(IUnknown)
    ['{2A3DEE9A-E31D-46D6-8508-BCC597DB3557}']

    function SetNativeDisplaySize(nativeDisplaySizePixels: SIZE): HRESULT; stdcall;

    function SetScaleFactor(scaleFactor: DEVICE_SCALE_FACTOR): HRESULT; stdcall;

    function SetApplicationViewState(viewState: APPLICATION_VIEW_STATE): HRESULT; stdcall;

    function ComputeApplicationSize(out applicationSizePixels: SIZE): HRESULT; stdcall;

    function IsApplicationViewStateSupported(viewState: APPLICATION_VIEW_STATE;
                                             nativeDisplaySizePixels: SIZE;
                                             scaleFactor: DEVICE_SCALE_FACTOR;
                                             out supported: BOOL): HRESULT; stdcall;

    function TriggerEdgeGesture(edgeGestureKind: EDGE_GESTURE_KIND): HRESULT; stdcall;
  end;
  IID_IApplicationDesignModeSettings = IApplicationDesignModeSettings;
  {$EXTERNALSYM IID_IApplicationDesignModeSettings}


  // Interface IApplicationDesignModeSettings2
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IApplicationDesignModeSettings2);'}
  {$EXTERNALSYM IApplicationDesignModeSettings2}
  IApplicationDesignModeSettings2 = interface(IApplicationDesignModeSettings)
    ['{490514E1-675A-4D6E-A58D-E54901B4CA2F}']

    function SetNativeDisplayOrientation(nativeDisplayOrientation: NATIVE_DISPLAY_ORIENTATION): HRESULT; stdcall;

    function SetApplicationViewOrientation(viewOrientation: APPLICATION_VIEW_ORIENTATION): HRESULT; stdcall;

    function SetAdjacentDisplayEdges(adjacentDisplayEdges: ADJACENT_DISPLAY_EDGES): HRESULT; stdcall;

    function SetIsOnLockScreen(isOnLockScreen: BOOL): HRESULT; stdcall;

    function SetApplicationViewMinWidth(viewMinWidth: APPLICATION_VIEW_MIN_WIDTH): HRESULT; stdcall;

    function GetApplicationSizeBounds(out minApplicationSizePixels: SIZE;
                                      out maxApplicationSizePixels: SIZE): HRESULT; stdcall;

    function GetApplicationViewOrientation(applicationSizePixels: SIZE;
                                           out viewOrientation: APPLICATION_VIEW_ORIENTATION): HRESULT; stdcall;

  end;
  IID_IApplicationDesignModeSettings2 = IApplicationDesignModeSettings2;
  {$EXTERNALSYM IID_IApplicationDesignModeSettings2}


  // Interface ILaunchTargetMonitor
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ILaunchTargetMonitor);'}
  {$EXTERNALSYM ILaunchTargetMonitor}
  ILaunchTargetMonitor = interface(IUnknown)
    ['{266FBC7E-490D-46ED-A96B-2274DB252003}']

    function GetMonitor(out monitor: HMONITOR): HRESULT; stdcall;
  end;
  IID_ILaunchTargetMonitor = ILaunchTargetMonitor;
  {$EXTERNALSYM IID_ILaunchTargetMonitor}


  // Interface ILaunchSourceViewSizePreference
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ILaunchSourceViewSizePreference);'}
  {$EXTERNALSYM ILaunchSourceViewSizePreference}
  ILaunchSourceViewSizePreference = interface(IUnknown)
    ['{E5AA01F7-1FB8-4830-8720-4E6734CBD5F3}']

    function GetSourceViewToPosition(out hwnd: HWND): HRESULT; stdcall;

    function GetSourceViewSizePreference(out sourceSizeAfterLaunch: APPLICATION_VIEW_SIZE_PREFERENCE): HRESULT; stdcall;

  end;
  IID_ILaunchSourceViewSizePreference = ILaunchSourceViewSizePreference;
  {$EXTERNALSYM IID_ILaunchSourceViewSizePreference}


  // Interface ILaunchTargetViewSizePreference
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ILaunchTargetViewSizePreference);'}
  {$EXTERNALSYM ILaunchTargetViewSizePreference}
  ILaunchTargetViewSizePreference = interface(IUnknown)
    ['{2F0666C6-12F7-4360-B511-A394A0553725}']

    function GetTargetViewSizePreference(out targetSizeOnLaunch: APPLICATION_VIEW_SIZE_PREFERENCE): HRESULT; stdcall;

  end;
  IID_ILaunchTargetViewSizePreference = ILaunchTargetViewSizePreference;
  {$EXTERNALSYM IID_ILaunchTargetViewSizePreference}


  // Interface ILaunchSourceAppUserModelId
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ILaunchSourceAppUserModelId);'}
  {$EXTERNALSYM ILaunchSourceAppUserModelId}
  ILaunchSourceAppUserModelId = interface(IUnknown)
    ['{989191AC-28FF-4CF0-9584-E0D078BC2396}']

    function GetAppUserModelId(
      out launchingApp: LPWSTR): HRESULT; stdcall;

  end;
  IID_ILaunchSourceAppUserModelId = ILaunchSourceAppUserModelId;
  {$EXTERNALSYM IID_ILaunchSourceAppUserModelId}


  // Interface IInitializeWithWindow
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInitializeWithWindow);'}
  {$EXTERNALSYM IInitializeWithWindow}
  IInitializeWithWindow = interface(IUnknown)
    ['{3E68D4BD-7135-4D10-8018-9FB6D9F33FA1}']

    function Initialize(_hwnd: HWND): HRESULT; stdcall;
  end;
  IID_IInitializeWithWindow = IInitializeWithWindow;
  {$EXTERNALSYM IID_IInitializeWithWindow}


  // Interface IHandlerInfo
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHandlerInfo);'}
  {$EXTERNALSYM IHandlerInfo}
  IHandlerInfo = interface(IUnknown)
    ['{997706EF-F880-453B-8118-39E1A2D2655A}']

    function GetApplicationDisplayName(out value: LPWSTR): HRESULT; stdcall;

    function GetApplicationPublisher(out value: LPWSTR): HRESULT; stdcall;

    function GetApplicationIconReference(out value: LPWSTR): HRESULT; stdcall;

  end;
  IID_IHandlerInfo = IHandlerInfo;
  {$EXTERNALSYM IID_IHandlerInfo}


  // Interface IHandlerInfo2
  // =======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHandlerInfo2);'}
  {$EXTERNALSYM IHandlerInfo2}
  IHandlerInfo2 = interface(IHandlerInfo)
    ['{31CCA04C-04D3-4EA9-90DE-97B15E87A532}']

    function GetApplicationId(out value: LPWSTR): HRESULT; stdcall;
  end;
  IID_IHandlerInfo2 = IHandlerInfo2;
  {$EXTERNALSYM IID_IHandlerInfo2}


  // Interface IHandlerActivationHost
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHandlerActivationHost);'}
  {$EXTERNALSYM IHandlerActivationHost}
  IHandlerActivationHost = interface(IUnknown)
    ['{35094A87-8BB1-4237-96C6-C417EEBDB078}']

    function BeforeCoCreateInstance(const clsidHandler: CLSID;
                                    itemsBeingActivated: IShellItemArray;
                                    handlerInfo: IHandlerInfo): HRESULT; stdcall;

    function BeforeCreateProcess(applicationPath: LPCWSTR;
                                 commandLine: LPCWSTR;
                                 handlerInfo: IHandlerInfo): HRESULT; stdcall;

  end;
  IID_IHandlerActivationHost = IHandlerActivationHost;
  {$EXTERNALSYM IID_IHandlerActivationHost}


  // Interface IAppActivationUIInfo
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAppActivationUIInfo);'}
  {$EXTERNALSYM IAppActivationUIInfo}
  IAppActivationUIInfo = interface(IUnknown)
    ['{ABAD189D-9FA3-4278-B3CA-8CA448A88DCB}']

    function GetMonitor(out value: HMONITOR): HRESULT; stdcall;

    function GetInvokePoint(out value: POINT): HRESULT; stdcall;

    function GetShowCommand(out value: Integer): HRESULT; stdcall;

    function GetShowUI(out value: BOOL): HRESULT; stdcall;

    function GetKeyState(out value: DWORD): HRESULT; stdcall;

  end;
  IID_IAppActivationUIInfo = IAppActivationUIInfo;
  {$EXTERNALSYM IID_IAppActivationUIInfo}


  // Interface IContactManagerInterop
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IContactManagerInterop);'}
  {$EXTERNALSYM IContactManagerInterop}
  IContactManagerInterop = interface(IUnknown)
    ['{99EACBA7-E073-43B6-A896-55AFE48A0833}']

    function ShowContactCardForWindow(appWindow: HWND;
                                      contact: IUnknown;
                                      const selection: TRECT;
                                      preferredPlacement: FLYOUT_PLACEMENT): HRESULT; stdcall;

  end;
  IID_IContactManagerInterop = IContactManagerInterop;
  {$EXTERNALSYM IID_IContactManagerInterop}


  // Interface IShellIconOverlayIdentifier
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellIconOverlayIdentifier);'}
  {$EXTERNALSYM IShellIconOverlayIdentifier}
  IShellIconOverlayIdentifier = interface(IUnknown)
    ['{0C6C4200-C589-11D0-999A-00C04FD655E1}']

    function IsMemberOf(pwszPath: LPCWSTR;
                        dwAttrib: DWORD): HRESULT; stdcall;

    function GetOverlayInfo(pwszIconFile: LPWSTR;
                            cchMax: Integer;
                            out pIndex: Integer;
                            out pdwFlags: DWORD): HRESULT; stdcall;

    function GetPriority(out pPriority: Integer): HRESULT; stdcall;

  end;
  IID_IShellIconOverlayIdentifier = IShellIconOverlayIdentifier;
  {$EXTERNALSYM IID_IShellIconOverlayIdentifier}


  // Interface IBannerNotificationHandler
  // ====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBannerNotificationHandler);'}
  {$EXTERNALSYM IBannerNotificationHandler}
  IBannerNotificationHandler = interface(IUnknown)
    ['{8D7B2BA7-DB05-46A8-823C-D2B6DE08EE91}']

    function OnBannerEvent(const notification: BANNER_NOTIFICATION): HRESULT; stdcall;

  end;
  IID_IBannerNotificationHandler = IBannerNotificationHandler;
  {$EXTERNALSYM IID_IBannerNotificationHandler}


  // Interface ISortColumnArray
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISortColumnArray);'}
  {$EXTERNALSYM ISortColumnArray}
  ISortColumnArray = interface(IUnknown)
    ['{6DFC60FB-F2E9-459B-BEB5-288F1A7C7D54}']

    function GetCount(out columnCount: UINT): HRESULT; stdcall;

    function GetAt(index: UINT;
                   out sortcolumn: SORTCOLUMN): HRESULT; stdcall;

    function GetSortType(out AType: SORT_ORDER_TYPE): HRESULT; stdcall;

  end;
  IID_ISortColumnArray = ISortColumnArray;
  {$EXTERNALSYM IID_ISortColumnArray}


  // Interface IPropertyKeyStore
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPropertyKeyStore);'}
  {$EXTERNALSYM IPropertyKeyStore}
  IPropertyKeyStore = interface(IUnknown)
    ['{75BD59AA-F23B-4963-ABA4-0B355752A91B}']

    function GetKeyCount(out keyCount: Integer): HRESULT; stdcall;

    function GetKeyAt(index: Integer;
                      out pkey: PROPERTYKEY): HRESULT; stdcall;

    function AppendKey(const key: PROPERTYKEY): HRESULT; stdcall;

    function DeleteKey(index: Integer): HRESULT; stdcall;

    function IsKeyInStore(const key: PROPERTYKEY): HRESULT; stdcall;

    function RemoveKey(const key: PROPERTYKEY): HRESULT; stdcall;

  end;
  IID_IPropertyKeyStore = IPropertyKeyStore;
  {$EXTERNALSYM IID_IPropertyKeyStore}



// Type aliases that depend on translated interfaces
// ================================================
//
type
  IShellLink = IShellLinkW;
  {$EXTERNALSYM IShellLink}

// Constant aliases that depend on translated interface IIDs
// ========================================================
//
const

  CGID_DeskBand = IID_IDeskBand;
  {$EXTERNALSYM CGID_DeskBand}
  // See above for other declarations.

// Helper function declarations
// ============================
//
procedure FreeIDListArray(ppidls: Pointer;
                          cItems: UINT);
{$EXTERNALSYM FreeIDListArray}
procedure FreeIDListArrayFull(ppidls: Pointer;
                              cItems: UINT);
{$EXTERNALSYM FreeIDListArrayFull}
procedure FreeIDListArrayChild(ppidls: Pointer;
                               cItems: UINT);
{$EXTERNALSYM FreeIDListArrayChild}

// API function declarations
function SHSimpleIDListFromPath(pszPath: LPCWSTR): PIDLIST_ABSOLUTE; stdcall;
{$EXTERNALSYM SHSimpleIDListFromPath}

function SHCreateItemFromIDList(pidl: PCIDLIST_ABSOLUTE;
                                const riid: REFIID;
                                out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHCreateItemFromIDList}

function SHCreateItemFromParsingName(pszPath: LPCWSTR;
                                     pbc: PIBindCtx;
                                     const riid: REFIID;
                                     out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHCreateItemFromParsingName}

function SHCreateItemWithParent(pidlParent: PCIDLIST_ABSOLUTE;
                                psfParent: IShellFolder;
                                pidl: PCUITEMID_CHILD;
                                const riid: REFIID;
                                out ppvItem): HRESULT; stdcall;
{$EXTERNALSYM SHCreateItemWithParent}

function SHCreateItemFromRelativeName(psiParent: IShellItem;
                                      pszName: LPCWSTR;
                                      pbc: PIBindCtx;
                                      const riid: REFIID;
                                      out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHCreateItemFromRelativeName}

function SHCreateItemInKnownFolder(kfid: REFKNOWNFOLDERID;
                                   dwKFFlags: DWORD;
                                   pszItem: LPCWSTR;
                                   const riid: REFIID;
                                   out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHCreateItemInKnownFolder}

function SHGetIDListFromObject(punk: IUnknown;
                               ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;
{$EXTERNALSYM SHGetIDListFromObject}

function SHGetItemFromObject(punk: IUnknown;
                             const riid: REFIID;
                             out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHGetItemFromObject}

function SHGetPropertyStoreFromIDList(pidl: PCIDLIST_ABSOLUTE;
                                      flags: GETPROPERTYSTOREFLAGS;
                                      const riid: REFIID;
                                      out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHGetPropertyStoreFromIDList}

function SHGetPropertyStoreFromParsingName(pszPath: LPCWSTR;
                                           pbc: PIBindCtx;
                                           flags: GETPROPERTYSTOREFLAGS;
                                           const riid: REFIID;
                                           out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHGetPropertyStoreFromParsingName}

function SHGetNameFromIDList(pidl: PCIDLIST_ABSOLUTE;
                             sigdnName: SIGDN;
                             ppszName: LPWSTR): HRESULT; stdcall;
{$EXTERNALSYM SHGetNameFromIDList}

function SHGetItemFromDataObject(pdtobj: IDataObject;
                                 dwFlags: DATAOBJ_GET_ITEM_FLAGS;
                                 const riid: REFIID;
                                 out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHGetItemFromDataObject}

function SHCreateShellItemArray(pidlParent: PCIDLIST_ABSOLUTE;
                                psf: IShellFolder;
                                cidl: UINT;
                                ppidl: PCUITEMID_CHILD_ARRAY;
                                out ppsiItemArray: IShellItemArray): HRESULT; stdcall;
{$EXTERNALSYM SHCreateShellItemArray}

function SHCreateShellItemArrayFromDataObject(pdo: IDataObject;
                                              const riid: REFIID;
                                              out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHCreateShellItemArrayFromDataObject}

function SHCreateShellItemArrayFromIDLists(cidl: UINT;
                                           rgpidl: PCIDLIST_ABSOLUTE_ARRAY;
                                           out ppsiItemArray: IShellItemArray): HRESULT; stdcall;
{$EXTERNALSYM SHCreateShellItemArrayFromIDLists}

function SHCreateShellItemArrayFromShellItem(psi: IShellItem; riid: REFIID; out ppv: Pointer): HRESULT; stdcall;
{$EXTERNALSYM SHCreateShellItemArrayFromShellItem}

function SHCreateAssociationRegistration(riid: REFIID; out ppv: Pointer): HRESULT; stdcall;
{$EXTERNALSYM SHCreateAssociationRegistration}

function SHCreateDefaultExtractIcon(const riid: REFIID;
                                    out ppv): HRESULT; stdcall;
{$EXTERNALSYM SHCreateDefaultExtractIcon}

function SetCurrentProcessExplicitAppUserModelID(AppID: LPCWSTR): HRESULT; stdcall;
{$EXTERNALSYM SetCurrentProcessExplicitAppUserModelID}

function GetCurrentProcessExplicitAppUserModelID(AppID: LPWSTR): HRESULT; stdcall;
{$EXTERNALSYM GetCurrentProcessExplicitAppUserModelID}

function SHGetTemporaryPropertyForItem(psi: IShellItem;
                                       propkey: REFPROPERTYKEY;
                                       ppropvar: PROPVARIANT): HRESULT; stdcall;
{$EXTERNALSYM SHGetTemporaryPropertyForItem}

function SHSetTemporaryPropertyForItem(psi: IShellItem;
                                       propkey: REFPROPERTYKEY;
                                       propvar: REFPROPVARIANT): HRESULT; stdcall;
{$EXTERNALSYM SHSetTemporaryPropertyForItem}

function SHShowManageLibraryUI(psiLibrary: IShellItem;
                               hwndOwner: HWND;
                               pszTitle: LPCWSTR;
                               pszInstruction: LPCWSTR;
                               lmdOptions: LIBRARYMANAGEDIALOGOPTIONS): HRESULT; stdcall;
{$EXTERNALSYM SHShowManageLibraryUI}

function SHResolveLibrary(psiLibrary: IShellItem): HRESULT; stdcall;
{$EXTERNALSYM SHResolveLibrary}

function SHAssocEnumHandlers(pszExtra: LPCWSTR;
                             afFilter: ASSOC_FILTER;
                             out ppEnumHandler: IEnumAssocHandlers): HRESULT; stdcall;
{$EXTERNALSYM SHAssocEnumHandlers}

function SHAssocEnumHandlersForProtocolByApplication(protocol: LPCWSTR;
                                                     const riid: REFIID;
                                                     out enumHandlers: Pointer): HRESULT; stdcall;
{$EXTERNALSYM SHAssocEnumHandlersForProtocolByApplication}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation


const
  Shell32Lib = 'shell32.dll';
  ExplorerFrameLib = 'ExplorerFrame.dll';

{$WARN SYMBOL_PLATFORM OFF}

procedure FreeIDListArray(ppidls: Pointer;
                          cItems: UINT);
var
  I: UINT;
  Items: PPointer;

begin

  Items := PPointer(ppidls);

  for I := 0 to cItems - 1 do
    begin

      CoTaskMemFree(Items^);
      Inc(Items);
    end;

  CoTaskMemFree(ppidls);
end;


procedure FreeIDListArrayFull(ppidls: Pointer;
                              cItems: UINT);
begin

  FreeIDListArray(ppidls,
                  cItems);
end;


procedure FreeIDListArrayChild(ppidls: Pointer;
                               cItems: UINT);
begin

  FreeIDListArray(ppidls,
                  cItems);
end;


{$WARN SYMBOL_PLATFORM OFF}

function SHSimpleIDListFromPath;            external Shell32Lib name 'SHSimpleIDListFromPath' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateItemFromIDList;            external Shell32Lib name 'SHCreateItemFromIDList' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateItemFromParsingName;       external Shell32Lib name 'SHCreateItemFromParsingName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateItemWithParent;            external Shell32Lib name 'SHCreateItemWithParent' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateItemFromRelativeName;      external Shell32Lib name 'SHCreateItemFromRelativeName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateItemInKnownFolder;         external Shell32Lib name 'SHCreateItemInKnownFolder' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHGetIDListFromObject;             external Shell32Lib name 'SHGetIDListFromObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHGetItemFromObject;               external Shell32Lib name 'SHGetItemFromObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHGetPropertyStoreFromIDList;      external Shell32Lib name 'SHGetPropertyStoreFromIDList' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHGetPropertyStoreFromParsingName; external Shell32Lib name 'SHGetPropertyStoreFromParsingName' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHGetNameFromIDList;               external Shell32Lib name 'SHGetNameFromIDList' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHGetItemFromDataObject;           external Shell32Lib name 'SHGetItemFromDataObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateShellItemArray;            external Shell32Lib name 'SHCreateShellItemArray' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateShellItemArrayFromDataObject; external Shell32Lib name 'SHCreateShellItemArrayFromDataObject' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateShellItemArrayFromIDLists; external Shell32Lib name 'SHCreateShellItemArrayFromIDLists' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateShellItemArrayFromShellItem; external Shell32Lib name 'SHCreateShellItemArrayFromShellItem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateAssociationRegistration;   external ExplorerFrameLib name 'SHCreateAssociationRegistration' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHCreateDefaultExtractIcon;        external Shell32Lib name 'SHCreateDefaultExtractIcon' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SetCurrentProcessExplicitAppUserModelID; external Shell32Lib name 'SetCurrentProcessExplicitAppUserModelID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function GetCurrentProcessExplicitAppUserModelID; external Shell32Lib name 'GetCurrentProcessExplicitAppUserModelID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHGetTemporaryPropertyForItem;     external Shell32Lib name 'SHGetTemporaryPropertyForItem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHSetTemporaryPropertyForItem;     external Shell32Lib name 'SHSetTemporaryPropertyForItem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHShowManageLibraryUI;             external Shell32Lib name 'SHShowManageLibraryUI' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHResolveLibrary;                  external Shell32Lib name 'SHResolveLibrary' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHAssocEnumHandlers;               external Shell32Lib name 'SHAssocEnumHandlers' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
function SHAssocEnumHandlersForProtocolByApplication; external Shell32Lib name 'SHAssocEnumHandlersForProtocolByApplication' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

{$WARN SYMBOL_PLATFORM ON}

  // Implement Additional functions here.


end.
