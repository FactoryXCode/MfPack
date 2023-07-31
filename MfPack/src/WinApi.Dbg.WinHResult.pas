//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.WinHResult.pas
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: Return ccde definitions of the Win32 HResults.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 31/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: -
//
// Related objects: -
// Related projects: MfPackX315
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.22621.0
//
// Todo: -
//
//==============================================================================
// Source: https://learn.microsoft.com/en-us/windows/win32/debug/error-handling-reference
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
unit WinApi.Dbg.WinHResult;
// See: https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/705fb797-2175-4a90-b5a3-3918024b10b8
interface

uses
  WinApi.Windows,
  WinApi.WinError,
  WinApi.Dbg.WinHResultTools;


  function GetHResultDescription(const aHResult: HResult;
                                 out hrStr: string;
                                 out hrDescr: string;
                                 out RegionDescr: string;
                                 out HeaderFile: string;
                                 out Reference: TReferenceArray): HResult;

  function GetHrRegion(aHResult: HResult;
                       out aRegion: string): HResult;


implementation

uses
  System.SysUtils,
  System.StrUtils;


function GetHResultDescription(const aHResult: HResult;
                               out hrStr: string;
                               out hrDescr: string;
                               out RegionDescr: string;
                               out HeaderFile: string;
                               out Reference: TReferenceArray): HResult;

var
  hr: HResult;

begin
  hr := S_OK;
  HeaderFile := 'winerror.h';
  Reference[0] := 'https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-erref/705fb797-2175-4a90-b5a3-3918024b10b8';

  case aHResult of
    LongInt($00030200)          : begin
                                    HrStr := 'STG_S_CONVERTED';
                                    HrDescr := 'The underlying file was converted to compound file format.';
                                  end;
    LongInt($00030201)          : begin
                                    HrStr := 'STG_S_BLOCK';
                                    HrDescr := 'The storage operation should block until more data is available.';
                                  end;
    LongInt($00030202)          : begin
                                    HrStr := 'STG_S_RETRYNOW';
                                    HrDescr := 'The storage operation should retry immediately.';
                                  end;
    LongInt($00030203)          : begin
                                    HrStr := 'STG_S_MONITORING';
                                    HrDescr := 'The notified event sink will not influence the storage operation.';
                                  end;
    LongInt($00030204)          : begin
                                    HrStr := 'STG_S_MULTIPLEOPENS';
                                    HrDescr := 'Multiple opens prevent consolidated (commit succeeded).';
                                  end;
    LongInt($00030205)          : begin
                                    HrStr := 'STG_S_CONSOLIDATIONFAILED';
                                    HrDescr := 'Consolidation of the storage file failed (commit succeeded).';
                                  end;
    LongInt($00030206)          : begin
                                    HrStr := 'STG_S_CANNOTCONSOLIDATE';
                                    HrDescr := 'Consolidation of the storage file is inappropriate (commit succeeded).';
                                  end;
    LongInt($00040000)          : begin
                                    HrStr := 'OLE_S_USEREG';
                                    HrDescr := 'Use the registry database to provide the requested information.';
                                  end;
    LongInt($00040001)          : begin
                                    HrStr := 'OLE_S_STATIC';
                                    HrDescr := 'Success, but static.';
                                  end;
    LongInt($00040002)          : begin
                                    HrStr := 'OLE_S_MAC_CLIPFORMAT';
                                    HrDescr := 'Macintosh clipboard format.';
                                  end;
    LongInt($00040100)          : begin
                                    HrStr := 'DRAGDROP_S_DROP';
                                    HrDescr := 'Successful drop took place.';
                                  end;
    LongInt($00040101)          : begin
                                    HrStr := 'DRAGDROP_S_CANCEL';
                                    HrDescr := 'Drag-drop operation canceled.';
                                  end;
    LongInt($00040102)          : begin
                                    HrStr := 'DRAGDROP_S_USEDEFAULTCURSORS';
                                    HrDescr := 'Use the default cursor.';
                                  end;
    LongInt($00040130)          : begin
                                    HrStr := 'DATA_S_SAMEFORMATETC';
                                    HrDescr := 'Data has same FORMATETC.';
                                  end;
    LongInt($00040140)          : begin
                                    HrStr := 'VIEW_S_ALREADY_FROZEN';
                                    HrDescr := 'View is already frozen.';
                                  end;
    LongInt($00040170)          : begin
                                    HrStr := 'CACHE_S_FORMATETC_NOTSUPPORTED';
                                    HrDescr := 'FORMATETC not supported.';
                                  end;
    LongInt($00040171)          : begin
                                    HrStr := 'CACHE_S_SAMECACHE';
                                    HrDescr := 'Same cache.';
                                  end;
    LongInt($00040172)          : begin
                                    HrStr := 'CACHE_S_SOMECACHES_NOTUPDATED';
                                    HrDescr := 'Some caches are not updated.';
                                  end;
    LongInt($00040180)          : begin
                                    HrStr := 'OLEOBJ_S_INVALIDVERB';
                                    HrDescr := 'Invalid verb for OLE object.';
                                  end;
    LongInt($00040181)          : begin
                                    HrStr := 'OLEOBJ_S_CANNOT_DOVERB_NOW';
                                    HrDescr := 'Verb number is valid but verb cannot be done now.';
                                  end;
    LongInt($00040182)          : begin
                                    HrStr := 'OLEOBJ_S_INVALIDHWND';
                                    HrDescr := 'Invalid window handle passed.';
                                  end;
    LongInt($000401A0)          : begin
                                    HrStr := 'INPLACE_S_TRUNCATED';
                                    HrDescr := 'Message is too long; some of it had to be truncated before displaying.';
                                  end;
    LongInt($000401C0)          : begin
                                    HrStr := 'CONVERT10_S_NO_PRESENTATION';
                                    HrDescr := 'Unable to convert OLESTREAM to IStorage.';
                                  end;
    LongInt($000401E2)          : begin
                                    HrStr := 'MK_S_REDUCED_TO_SELF';
                                    HrDescr := 'Moniker reduced to itself.';
                                  end;
    LongInt($000401E4)          : begin
                                    HrStr := 'MK_S_ME';
                                    HrDescr := 'Common prefix is this moniker.';
                                  end;
    LongInt($000401E5)          : begin
                                    HrStr := 'MK_S_HIM';
                                    HrDescr := 'Common prefix is input moniker.';
                                  end;
    LongInt($000401E6)          : begin
                                    HrStr := 'MK_S_US';
                                    HrDescr := 'Common prefix is both monikers.';
                                  end;
    LongInt($000401E7)          : begin
                                    HrStr := 'MK_S_MONIKERALREADYREGISTERED';
                                    HrDescr := 'Moniker is already registered in running object table.';
                                  end;
    LongInt($00040200)          : begin
                                    HrStr := 'EVENT_S_SOME_SUBSCRIBERS_FAILED';
                                    HrDescr := 'An event was able to invoke some, but not all, of the subscribers.';
                                  end;
    LongInt($00040202)          : begin
                                    HrStr := 'EVENT_S_NOSUBSCRIBERS';
                                    HrDescr := 'An event was delivered, but there were no subscribers.';
                                  end;
    LongInt($00041300)          : begin
                                    HrStr := 'SCHED_S_TASK_READY';
                                    HrDescr := 'The task is ready to run at its next scheduled time.';
                                  end;
    LongInt($00041301)          : begin
                                    HrStr := 'SCHED_S_TASK_RUNNING';
                                    HrDescr := 'The task is currently running.';
                                  end;
    LongInt($00041302)          : begin
                                    HrStr := 'SCHED_S_TASK_DISABLED';
                                    HrDescr := 'The task will not run at the scheduled times because it has been disabled.';
                                  end;
    LongInt($00041303)          : begin
                                    HrStr := 'SCHED_S_TASK_HAS_NOT_RUN';
                                    HrDescr := 'The task has not yet run.';
                                  end;
    LongInt($00041304)          : begin
                                    HrStr := 'SCHED_S_TASK_NO_MORE_RUNS';
                                    HrDescr := 'There are no more runs scheduled for this task.';
                                  end;
    LongInt($00041305)          : begin
                                    HrStr := 'SCHED_S_TASK_NOT_SCHEDULED';
                                    HrDescr := 'One or more of the properties that are needed to run this task on a schedule have not been set.';
                                  end;
    LongInt($00041306)          : begin
                                    HrStr := 'SCHED_S_TASK_TERMINATED';
                                    HrDescr := 'The last run of the task was terminated by the user.';
                                  end;
    LongInt($00041307)          : begin
                                    HrStr := 'SCHED_S_TASK_NO_VALID_TRIGGERS';
                                    HrDescr := 'Either the task has no triggers, or the existing triggers are disabled or not set.';
                                  end;
    LongInt($00041308)          : begin
                                    HrStr := 'SCHED_S_EVENT_TRIGGER';
                                    HrDescr := 'Event triggers do not have set run times.';
                                  end;
    LongInt($0004131B)          : begin
                                    HrStr := 'SCHED_S_SOME_TRIGGERS_FAILED';
                                    HrDescr := 'The task is registered, but not all specified triggers will start the task.';
                                  end;
    LongInt($0004131C)          : begin
                                    HrStr := 'SCHED_S_BATCH_LOGON_PROBLEM';
                                    HrDescr := 'The task is registered, but it might fail to start.' +
                                               'Batch logon privilege needs to be enabled for the task principal.';
                                  end;
    LongInt($0004D000)          : begin
                                    HrStr := 'XACT_S_ASYNC';
                                    HrDescr := 'An asynchronous operation was specified.' +
                                               'The operation has begun, but its outcome is not known yet.';
                                  end;
    LongInt($0004D002)          : begin
                                    HrStr := 'XACT_S_READONLY';
                                    HrDescr := 'The method call succeeded because the transaction was read-only.';
                                  end;
    LongInt($0004D003)          : begin
                                    HrStr := 'XACT_S_SOMENORETAIN';
                                    HrDescr := 'The transaction was successfully aborted.' +
                                               'However, this is a coordinated transaction, and a number of enlisted resources were aborted outright because they could not support abort-retaining semantics.';
                                  end;
    LongInt($0004D004)          : begin
                                    HrStr := 'XACT_S_OKINFORM';
                                    HrDescr := 'No changes were made during this call, but the sink wants another chance to look if any other sinks make further changes.';
                                  end;
    LongInt($0004D005)          : begin
                                    HrStr := 'XACT_S_MADECHANGESCONTENT';
                                    HrDescr := 'The sink is content and wants the transaction to proceed.' +
                                               'Changes were made to one or more resources during this call.';
                                  end;
    LongInt($0004D006)          : begin
                                    HrStr := 'XACT_S_MADECHANGESINFORM';
                                    HrDescr := 'The sink is for the moment and wants the transaction to proceed, but if other changes are made following this return by other event sinks, this sink wants another chance to look.';
                                  end;
    LongInt($0004D007)          : begin
                                    HrStr := 'XACT_S_ALLNORETAIN';
                                    HrDescr := 'The transaction was successfully aborted.' +
                                               'However, the abort was nonretaining.';
                                  end;
    LongInt($0004D008)          : begin
                                    HrStr := 'XACT_S_ABORTING';
                                    HrDescr := 'An abort operation was already in progress.';
                                  end;
    LongInt($0004D009)          : begin
                                    HrStr := 'XACT_S_SINGLEPHASE';
                                    HrDescr := 'The resource manager has performed a single-phase commit of the transaction.';
                                  end;
    LongInt($0004D00A)          : begin
                                    HrStr := 'XACT_S_LOCALLY_OK';
                                    HrDescr := 'The local transaction has not aborted.';
                                  end;
    LongInt($0004D010)          : begin
                                    HrStr := 'XACT_S_LASTRESOURCEMANAGER';
                                    HrDescr := 'The resource manager has requested to be the coordinator (last resource manager) for the transaction.';
                                  end;
    LongInt($00080012)          : begin
                                    HrStr := 'CO_S_NOTALLINTERFACES';
                                    HrDescr := 'Not all the requested interfaces were available.';
                                  end;
    LongInt($00080013)          : begin
                                    HrStr := 'CO_S_MACHINENAMENOTFOUND';
                                    HrDescr := 'The specified machine name was not found in the cache.';
                                  end;
    LongInt($00090312)          : begin
                                    HrStr := 'SEC_I_CONTINUE_NEEDED';
                                    HrDescr := 'The function completed successfully, but it must be called again to complete the context.';
                                  end;
    LongInt($00090313)          : begin
                                    HrStr := 'SEC_I_COMPLETE_NEEDED';
                                    HrDescr := 'The function completed successfully, but CompleteToken must be called.';
                                  end;
    LongInt($00090314)          : begin
                                    HrStr := 'SEC_I_COMPLETE_AND_CONTINUE';
                                    HrDescr := 'The function completed successfully, but both CompleteToken and this function must be called to complete the context.';
                                  end;
    LongInt($00090315)          : begin
                                    HrStr := 'SEC_I_LOCAL_LOGON';
                                    HrDescr := 'The logon was completed, but no network authority was available.' +
                                               'The logon was made using locally known information.';
                                  end;
    LongInt($00090317)          : begin
                                    HrStr := 'SEC_I_CONTEXT_EXPIRED';
                                    HrDescr := 'The context has expired and can no longer be used.';
                                  end;
    LongInt($00090320)          : begin
                                    HrStr := 'SEC_I_INCOMPLETE_CREDENTIALS';
                                    HrDescr := 'The credentials supplied were not complete and could not be verified.' +
                                               'Additional information can be returned from the context.';
                                  end;
    LongInt($00090321)          : begin
                                    HrStr := 'SEC_I_RENEGOTIATE';
                                    HrDescr := 'The context data must be renegotiated with the peer.';
                                  end;
    LongInt($00090323)          : begin
                                    HrStr := 'SEC_I_NO_LSA_CONTEXT';
                                    HrDescr := 'There is no LSA mode context associated with this context.';
                                  end;
    LongInt($0009035C)          : begin
                                    HrStr := 'SEC_I_SIGNATURE_NEEDED';
                                    HrDescr := 'A signature operation must be performed before the user can authenticate.';
                                  end;
    LongInt($00091012)          : begin
                                    HrStr := 'CRYPT_I_NEW_PROTECTION_REQUIRED';
                                    HrDescr := 'The protected data needs to be reprotected.';
                                  end;
    LongInt($000D0000)          : begin
                                    HrStr := 'NS_S_CALLPENDING';
                                    HrDescr := 'The requested operation is pending completion.';
                                  end;
    LongInt($000D0001)          : begin
                                    HrStr := 'NS_S_CALLABORTED';
                                    HrDescr := 'The requested operation was aborted by the client.';
                                  end;
    LongInt($000D0002)          : begin
                                    HrStr := 'NS_S_STREAM_TRUNCATED';
                                    HrDescr := 'The stream was purposefully stopped before completion.';
                                  end;
    LongInt($000D0BC8)          : begin
                                    HrStr := 'NS_S_REBUFFERING';
                                    HrDescr := 'The requested operation has caused the source to rebuffer.';
                                  end;
    LongInt($000D0BC9)          : begin
                                    HrStr := 'NS_S_DEGRADING_QUALITY';
                                    HrDescr := 'The requested operation has caused the source to degrade codec quality.';
                                  end;
    LongInt($000D0BDB)          : begin
                                    HrStr := 'NS_S_TRANSCRYPTOR_EOF';
                                    HrDescr := 'The transcryptor object has reached end of file.';
                                  end;
    LongInt($000D0FE8)          : begin
                                    HrStr := 'NS_S_WMP_UI_VERSIONMISMATCH';
                                    HrDescr := 'An upgrade is needed for the theme manager to correctly show this skin.' +
                                               'Skin reports version: %.1f.';
                                  end;
    LongInt($000D0FE9)          : begin
                                    HrStr := 'NS_S_WMP_EXCEPTION';
                                    HrDescr := 'An error occurred in one of the UI components.';
                                  end;
    LongInt($000D1040)          : begin
                                    HrStr := 'NS_S_WMP_LOADED_GIF_IMAGE';
                                    HrDescr := 'Successfully loaded a GIF file.';
                                  end;
    LongInt($000D1041)          : begin
                                    HrStr := 'NS_S_WMP_LOADED_PNG_IMAGE';
                                    HrDescr := 'Successfully loaded a PNG file.';
                                  end;
    LongInt($000D1042)          : begin
                                    HrStr := 'NS_S_WMP_LOADED_BMP_IMAGE';
                                    HrDescr := 'Successfully loaded a BMP file.';
                                  end;
    LongInt($000D1043)          : begin
                                    HrStr := 'NS_S_WMP_LOADED_JPG_IMAGE';
                                    HrDescr := 'Successfully loaded a JPG file.';
                                  end;
    LongInt($000D104F)          : begin
                                    HrStr := 'NS_S_WMG_FORCE_DROP_FRAME';
                                    HrDescr := 'Drop this frame.';
                                  end;
    LongInt($000D105F)          : begin
                                    HrStr := 'NS_S_WMR_ALREADYRENDERED';
                                    HrDescr := 'The specified stream has already been rendered.';
                                  end;
    LongInt($000D1060)          : begin
                                    HrStr := 'NS_S_WMR_PINTYPEPARTIALMATCH';
                                    HrDescr := 'The specified type partially matches this pin type.';
                                  end;
    LongInt($000D1061)          : begin
                                    HrStr := 'NS_S_WMR_PINTYPEFULLMATCH';
                                    HrDescr := 'The specified type fully matches this pin type.';
                                  end;
    LongInt($000D1066)          : begin
                                    HrStr := 'NS_S_WMG_ADVISE_DROP_FRAME';
                                    HrDescr := 'The timestamp is late compared to the current render position.' +
                                               'Advise dropping this frame.';
                                  end;
    LongInt($000D1067)          : begin
                                    HrStr := 'NS_S_WMG_ADVISE_DROP_TO_KEYFRAME';
                                    HrDescr := 'The timestamp is severely late compared to the current render position.' +
                                               'Advise dropping everything up to the next key frame.';
                                  end;
    LongInt($000D10DB)          : begin
                                    HrStr := 'NS_S_NEED_TO_BUY_BURN_RIGHTS';
                                    HrDescr := 'No burn rights.' +
                                               'You will be prompted to buy burn rights when you try to burn this file to an audio CD.';
                                  end;
    LongInt($000D10FE)          : begin
                                    HrStr := 'NS_S_WMPCORE_PLAYLISTCLEARABORT';
                                    HrDescr := 'Failed to clear playlist because it was aborted by user.';
                                  end;
    LongInt($000D10FF)          : begin
                                    HrStr := 'NS_S_WMPCORE_PLAYLISTREMOVEITEMABORT';
                                    HrDescr := 'Failed to remove item in the playlist since it was aborted by user.';
                                  end;
    LongInt($000D1102)          : begin
                                    HrStr := 'NS_S_WMPCORE_PLAYLIST_CREATION_PENDING';
                                    HrDescr := 'Playlist is being generated asynchronously.';
                                  end;
    LongInt($000D1103)          : begin
                                    HrStr := 'NS_S_WMPCORE_MEDIA_VALIDATION_PENDING';
                                    HrDescr := 'Validation of the media is pending.';
                                  end;
    LongInt($000D1104)          : begin
                                    HrStr := 'NS_S_WMPCORE_PLAYLIST_REPEAT_SECONDARY_SEGMENTS_IGNORED';
                                    HrDescr := 'Encountered more than one Repeat block during ASX processing.';
                                  end;
    LongInt($000D1105)          : begin
                                    HrStr := 'NS_S_WMPCORE_COMMAND_NOT_AVAILABLE';
                                    HrDescr := 'Current state of WMP disallows calling this method or property.';
                                  end;
    LongInt($000D1106)          : begin
                                    HrStr := 'NS_S_WMPCORE_PLAYLIST_NAME_AUTO_GENERATED';
                                    HrDescr := 'Name for the playlist has been auto generated.';
                                  end;
    LongInt($000D1107)          : begin
                                    HrStr := 'NS_S_WMPCORE_PLAYLIST_IMPORT_MISSING_ITEMS';
                                    HrDescr := 'The imported playlist does not contain all items from the original.';
                                  end;
    LongInt($000D1108)          : begin
                                    HrStr := 'NS_S_WMPCORE_PLAYLIST_COLLAPSED_TO_SINGLE_MEDIA';
                                    HrDescr := 'The M3U playlist has been ignored because it only contains one item.';
                                  end;
    LongInt($000D1109)          : begin
                                    HrStr := 'NS_S_WMPCORE_MEDIA_CHILD_PLAYLIST_OPEN_PENDING';
                                    HrDescr := 'The open for the child playlist associated with this media is pending.';
                                  end;
    LongInt($000D110A)          : begin
                                    HrStr := 'NS_S_WMPCORE_MORE_NODES_AVAIABLE';
                                    HrDescr := 'More nodes support the interface requested, but the array for returning them is full.';
                                  end;
    LongInt($000D1135)          : begin
                                    HrStr := 'NS_S_WMPBR_SUCCESS';
                                    HrDescr := 'Backup or Restore successful!.';
                                  end;
    LongInt($000D1136)          : begin
                                    HrStr := 'NS_S_WMPBR_PARTIALSUCCESS';
                                    HrDescr := 'Transfer complete with limitations.';
                                  end;
    LongInt($000D1144)          : begin
                                    HrStr := 'NS_S_WMPEFFECT_TRANSPARENT';
                                    HrDescr := 'Request to the effects control to change transparency status to transparent.';
                                  end;
    LongInt($000D1145)          : begin
                                    HrStr := 'NS_S_WMPEFFECT_OPAQUE';
                                    HrDescr := 'Request to the effects control to change transparency status to opaque.';
                                  end;
    LongInt($000D114E)          : begin
                                    HrStr := 'NS_S_OPERATION_PENDING';
                                    HrDescr := 'The requested application pane is performing an operation and will not be released.';
                                  end;
    LongInt($000D1359)          : begin
                                    HrStr := 'NS_S_TRACK_BUY_REQUIRES_ALBUM_PURCHASE';
                                    HrDescr := 'The file is only available for purchase when you buy the entire album.';
                                  end;
    LongInt($000D135E)          : begin
                                    HrStr := 'NS_S_NAVIGATION_COMPLETE_WITH_ERRORS';
                                    HrDescr := 'There were problems completing the requested navigation.' +
                                               'There are identifiers missing in the catalog.';
                                  end;
    LongInt($000D1361)          : begin
                                    HrStr := 'NS_S_TRACK_ALREADY_DOWNLOADED';
                                    HrDescr := 'Track already downloaded.';
                                  end;
    LongInt($000D1519)          : begin
                                    HrStr := 'NS_S_PUBLISHING_POINT_STARTED_WITH_FAILED_SINKS';
                                    HrDescr := 'The publishing point successfully started, but one or more of the requested data writer plug-ins failed.';
                                  end;
    LongInt($000D2726)          : begin
                                    HrStr := 'NS_S_DRM_LICENSE_ACQUIRED';
                                    HrDescr := 'Status message: The license was acquired.';
                                  end;
    LongInt($000D2727)          : begin
                                    HrStr := 'NS_S_DRM_INDIVIDUALIZED';
                                    HrDescr := 'Status message: The security upgrade has been completed.';
                                  end;
    LongInt($000D2746)          : begin
                                    HrStr := 'NS_S_DRM_MONITOR_CANCELLED';
                                    HrDescr := 'Status message: License monitoring has been canceled.';
                                  end;
    LongInt($000D2747)          : begin
                                    HrStr := 'NS_S_DRM_ACQUIRE_CANCELLED';
                                    HrDescr := 'Status message: License acquisition has been canceled.';
                                  end;
    LongInt($000D276E)          : begin
                                    HrStr := 'NS_S_DRM_BURNABLE_TRACK';
                                    HrDescr := 'The track is burnable and had no playlist burn limit.';
                                  end;
    LongInt($000D276F)          : begin
                                    HrStr := 'NS_S_DRM_BURNABLE_TRACK_WITH_PLAYLIST_RESTRICTION';
                                    HrDescr := 'The track is burnable but has a playlist burn limit.';
                                  end;
    LongInt($000D27DE)          : begin
                                    HrStr := 'NS_S_DRM_NEEDS_INDIVIDUALIZATION';
                                    HrDescr := 'A security upgrade is required to perform the operation on this media file.';
                                  end;
    LongInt($000D2AF8)          : begin
                                    HrStr := 'NS_S_REBOOT_RECOMMENDED';
                                    HrDescr := 'Installation was successful; however, some file cleanup is not complete.' +
                                               'For best results, restart your computer.';
                                  end;
    LongInt($000D2AF9)          : begin
                                    HrStr := 'NS_S_REBOOT_REQUIRED';
                                    HrDescr := 'Installation was successful; however, some file cleanup is not complete.' +
                                               'To continue, you must restart your computer.';
                                  end;
    LongInt($000D2F09)          : begin
                                    HrStr := 'NS_S_EOSRECEDING';
                                    HrDescr := 'EOS hit during rewinding.';
                                  end;
    LongInt($000D2F0D)          : begin
                                    HrStr := 'NS_S_CHANGENOTICE';
                                    HrDescr := 'Internal.';
                                  end;
    LongInt($001F0001)          : begin
                                    HrStr := 'ERROR_FLT_IO_COMPLETE';
                                    HrDescr := 'The IO was completed by a filter.';
                                  end;
    LongInt($00262307)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MODE_NOT_PINNED';
                                    HrDescr := 'No mode is pinned on the specified VidPN source or target.';
                                  end;
    LongInt($0026231E)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_PREFERRED_MODE';
                                    HrDescr := 'Specified mode set does not specify preference for one of its modes.';
                                  end;
    LongInt($0026234B)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DATASET_IS_EMPTY';
                                    HrDescr := 'Specified data set (for example, mode set, frequency range set, descriptor set, and topology) is empty.';
                                  end;
    LongInt($0026234C)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_MORE_ELEMENTS_IN_DATASET';
                                    HrDescr := 'Specified data set (for example, mode set, frequency range set, descriptor set, and topology) does not contain any more elements.';
                                  end;
    LongInt($00262351)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PATH_CONTENT_GEOMETRY_TRANSFORMATION_NOT_PINNED';
                                    HrDescr := 'Specified content transformation is not pinned on the specified VidPN present path.';
                                  end;
    LongInt($00300100)          : begin
                                    HrStr := 'PLA_S_PROPERTY_IGNORED';
                                    HrDescr := 'Property value will be ignored.';
                                  end;
    LongInt($00340001)          : begin
                                    HrStr := 'ERROR_NDIS_INDICATION_REQUIRED';
                                    HrDescr := 'The request will be completed later by a Network Driver Interface Specification (NDIS) status indication.';
                                  end;
    LongInt($0DEAD100)          : begin
                                    HrStr := 'TRK_S_OUT_OF_SYNC';
                                    HrDescr := 'The VolumeSequenceNumber of a MOVE_NOTIFICATION request is incorrect.';
                                  end;
    LongInt($0DEAD102)          : begin
                                    HrStr := 'TRK_VOLUME_NOT_FOUND';
                                    HrDescr := 'The VolumeID in a request was not found in the server''s ServerVolumeTable.';
                                  end;
    LongInt($0DEAD103)          : begin
                                    HrStr := 'TRK_VOLUME_NOT_OWNED';
                                    HrDescr := 'A notification was sent to the LnkSvrMessage method, but the RequestMachine for the request was not the VolumeOwner for a VolumeID in the request.';
                                  end;
    LongInt($0DEAD107)          : begin
                                    HrStr := 'TRK_S_NOTIFICATION_QUOTA_EXCEEDED';
                                    HrDescr := 'The server received a MOVE_NOTIFICATION request, but the FileTable size limit has already been reached.';
                                  end;
    LongInt($400D004F)          : begin
                                    HrStr := 'NS_I_TIGER_START';
                                    HrDescr := 'The Title Server %1 is running.';
                                  end;
    LongInt($400D0051)          : begin
                                    HrStr := 'NS_I_CUB_START';
                                    HrDescr := 'Content Server %1 (%2) is starting.';
                                  end;
    LongInt($400D0052)          : begin
                                    HrStr := 'NS_I_CUB_RUNNING';
                                    HrDescr := 'Content Server %1 (%2) is running.';
                                  end;
    LongInt($400D0054)          : begin
                                    HrStr := 'NS_I_DISK_START';
                                    HrDescr := 'Disk %1 ( %2 ) on Content Server %3, is running.';
                                  end;
    LongInt($400D0056)          : begin
                                    HrStr := 'NS_I_DISK_REBUILD_STARTED';
                                    HrDescr := 'Started rebuilding disk %1 ( %2 ) on Content Server %3.';
                                  end;
    LongInt($400D0057)          : begin
                                    HrStr := 'NS_I_DISK_REBUILD_FINISHED';
                                    HrDescr := 'Finished rebuilding disk %1 ( %2 ) on Content Server %3.';
                                  end;
    LongInt($400D0058)          : begin
                                    HrStr := 'NS_I_DISK_REBUILD_ABORTED';
                                    HrDescr := 'Aborted rebuilding disk %1 ( %2 ) on Content Server %3.';
                                  end;
    LongInt($400D0059)          : begin
                                    HrStr := 'NS_I_LIMIT_FUNNELS';
                                    HrDescr := 'A NetShow administrator at network location %1 set the data stream limit to %2 streams.';
                                  end;
    LongInt($400D005A)          : begin
                                    HrStr := 'NS_I_START_DISK';
                                    HrDescr := 'A NetShow administrator at network location %1 started disk %2.';
                                  end;
    LongInt($400D005B)          : begin
                                    HrStr := 'NS_I_STOP_DISK';
                                    HrDescr := 'A NetShow administrator at network location %1 stopped disk %2.';
                                  end;
    LongInt($400D005C)          : begin
                                    HrStr := 'NS_I_STOP_CUB';
                                    HrDescr := 'A NetShow administrator at network location %1 stopped Content Server %2.';
                                  end;
    LongInt($400D005D)          : begin
                                    HrStr := 'NS_I_KILL_USERSESSION';
                                    HrDescr := 'A NetShow administrator at network location %1 aborted user session %2 from the system.';
                                  end;
    LongInt($400D005E)          : begin
                                    HrStr := 'NS_I_KILL_CONNECTION';
                                    HrDescr := 'A NetShow administrator at network location %1 aborted obsolete connection %2 from the system.';
                                  end;
    LongInt($400D005F)          : begin
                                    HrStr := 'NS_I_REBUILD_DISK';
                                    HrDescr := 'A NetShow administrator at network location %1 started rebuilding disk %2.';
                                  end;
    LongInt($400D0069)          : begin
                                    HrStr := 'MCMADM_I_NO_EVENTS';
                                    HrDescr := 'Event initialization failed, there will be no MCM events.';
                                  end;
    LongInt($400D006E)          : begin
                                    HrStr := 'NS_I_LOGGING_FAILED';
                                    HrDescr := 'The logging operation failed.';
                                  end;
    LongInt($400D0070)          : begin
                                    HrStr := 'NS_I_LIMIT_BANDWIDTH';
                                    HrDescr := 'A NetShow administrator at network location %1 set the maximum bandwidth limit to %2 bps.';
                                  end;
    LongInt($400D0191)          : begin
                                    HrStr := 'NS_I_CUB_UNFAIL_LINK';
                                    HrDescr := 'Content Server %1 (%2) has established its link to Content Server %3.';
                                  end;
    LongInt($400D0193)          : begin
                                    HrStr := 'NS_I_RESTRIPE_START';
                                    HrDescr := 'Restripe operation has started.';
                                  end;
    LongInt($400D0194)          : begin
                                    HrStr := 'NS_I_RESTRIPE_DONE';
                                    HrDescr := 'Restripe operation has completed.';
                                  end;
    LongInt($400D0196)          : begin
                                    HrStr := 'NS_I_RESTRIPE_DISK_OUT';
                                    HrDescr := 'Content disk %1 (%2) on Content Server %3 has been restriped out.';
                                  end;
    LongInt($400D0197)          : begin
                                    HrStr := 'NS_I_RESTRIPE_CUB_OUT';
                                    HrDescr := 'Content server %1 (%2) has been restriped out.';
                                  end;
    LongInt($400D0198)          : begin
                                    HrStr := 'NS_I_DISK_STOP';
                                    HrDescr := 'Disk %1 ( %2 ) on Content Server %3, has been offlined.';
                                  end;
    LongInt($400D14BE)          : begin
                                    HrStr := 'NS_I_PLAYLIST_CHANGE_RECEDING';
                                    HrDescr := 'The playlist change occurred while receding.';
                                  end;
    LongInt($400D2EFF)          : begin
                                    HrStr := 'NS_I_RECONNECTED';
                                    HrDescr := 'The client is reconnected.';
                                  end;
    LongInt($400D2F01)          : begin
                                    HrStr := 'NS_I_NOLOG_STOP';
                                    HrDescr := 'Forcing a switch to a pending header on start.';
                                  end;
    LongInt($400D2F03)          : begin
                                    HrStr := 'NS_I_EXISTING_PACKETIZER';
                                    HrDescr := 'There is already an existing packetizer plugin for the stream.';
                                  end;
    LongInt($400D2F04)          : begin
                                    HrStr := 'NS_I_MANUAL_PROXY';
                                    HrDescr := 'The proxy setting is manual.';
                                  end;
    LongInt($40262009)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DRIVER_MISMATCH';
                                    HrDescr := 'The kernel driver detected a version mismatch between it and the user mode driver.';
                                  end;
    LongInt($4026242F)          : begin
                                    HrStr := 'ERROR_GRAPHICS_UNKNOWN_CHILD_STATUS';
                                    HrDescr := 'Child device presence was not reliably detected.';
                                  end;
    LongInt($40262437)          : begin
                                    HrStr := 'ERROR_GRAPHICS_LEADLINK_START_DEFERRED';
                                    HrDescr := 'Starting the lead-link adapter has been deferred temporarily.';
                                  end;
    LongInt($40262439)          : begin
                                    HrStr := 'ERROR_GRAPHICS_POLLING_TOO_FREQUENTLY';
                                    HrDescr := 'The display adapter is being polled for children too frequently at the same polling level.';
                                  end;
    LongInt($4026243A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_START_DEFERRED';
                                    HrDescr := 'Starting the adapter has been deferred temporarily.';
                                  end;
    LongInt($8000000A)          : begin
                                    HrStr := 'E_PENDING';
                                    HrDescr := 'The data necessary to complete this operation is not yet available.';
                                  end;
    LongInt($80004001)          : begin
                                    HrStr := 'E_NOTIMPL';
                                    HrDescr := 'Not implemented.';
                                  end;
    LongInt($80004002)          : begin
                                    HrStr := 'E_NOINTERFACE';
                                    HrDescr := 'No such interface supported.';
                                  end;
    LongInt($80004003)          : begin
                                    HrStr := 'E_POINTER';
                                    HrDescr := 'Invalid pointer.';
                                  end;
    LongInt($80004004)          : begin
                                    HrStr := 'E_ABORT';
                                    HrDescr := 'Operation aborted.';
                                  end;
    LongInt($80004005)          : begin
                                    HrStr := 'E_FAIL';
                                    HrDescr := 'Unspecified error.';
                                  end;
    LongInt($80004006)          : begin
                                    HrStr := 'CO_E_INIT_TLS';
                                    HrDescr := 'Thread local storage failure.';
                                  end;
    LongInt($80004007)          : begin
                                    HrStr := 'CO_E_INIT_SHARED_ALLOCATOR';
                                    HrDescr := 'Get shared memory allocator failure.';
                                  end;
    LongInt($80004008)          : begin
                                    HrStr := 'CO_E_INIT_MEMORY_ALLOCATOR';
                                    HrDescr := 'Get memory allocator failure.';
                                  end;
    LongInt($80004009)          : begin
                                    HrStr := 'CO_E_INIT_CLASS_CACHE';
                                    HrDescr := 'Unable to initialize class cache.';
                                  end;
    LongInt($8000400A)          : begin
                                    HrStr := 'CO_E_INIT_RPC_CHANNEL';
                                    HrDescr := 'Unable to initialize remote procedure call (RPC) services.';
                                  end;
    LongInt($8000400B)          : begin
                                    HrStr := 'CO_E_INIT_TLS_SET_CHANNEL_CONTROL';
                                    HrDescr := 'Cannot set thread local storage channel control.';
                                  end;
    LongInt($8000400C)          : begin
                                    HrStr := 'CO_E_INIT_TLS_CHANNEL_CONTROL';
                                    HrDescr := 'Could not allocate thread local storage channel control.';
                                  end;
    LongInt($8000400D)          : begin
                                    HrStr := 'CO_E_INIT_UNACCEPTED_USER_ALLOCATOR';
                                    HrDescr := 'The user-supplied memory allocator is unacceptable.';
                                  end;
    LongInt($8000400E)          : begin
                                    HrStr := 'CO_E_INIT_SCM_MUTEX_EXISTS';
                                    HrDescr := 'The OLE service mutex already exists.';
                                  end;
    LongInt($8000400F)          : begin
                                    HrStr := 'CO_E_INIT_SCM_FILE_MAPPING_EXISTS';
                                    HrDescr := 'The OLE service file mapping already exists.';
                                  end;
    LongInt($80004010)          : begin
                                    HrStr := 'CO_E_INIT_SCM_MAP_VIEW_OF_FILE';
                                    HrDescr := 'Unable to map view of file for OLE service.';
                                  end;
    LongInt($80004011)          : begin
                                    HrStr := 'CO_E_INIT_SCM_EXEC_FAILURE';
                                    HrDescr := 'Failure attempting to launch OLE service.';
                                  end;
    LongInt($80004012)          : begin
                                    HrStr := 'CO_E_INIT_ONLY_SINGLE_THREADED';
                                    HrDescr := 'There was an attempt to call CoInitialize a second time while single-threaded.';
                                  end;
    LongInt($80004013)          : begin
                                    HrStr := 'CO_E_CANT_REMOTE';
                                    HrDescr := 'A Remote activation was necessary but was not allowed.';
                                  end;
    LongInt($80004014)          : begin
                                    HrStr := 'CO_E_BAD_SERVER_NAME';
                                    HrDescr := 'A Remote activation was necessary, but the server name provided was invalid.';
                                  end;
    LongInt($80004015)          : begin
                                    HrStr := 'CO_E_WRONG_SERVER_IDENTITY';
                                    HrDescr := 'The class is configured to run as a security ID different from the caller.';
                                  end;
    LongInt($80004016)          : begin
                                    HrStr := 'CO_E_OLE1DDE_DISABLED';
                                    HrDescr := 'Use of OLE1 services requiring Dynamic Data Exchange (DDE) Windows is disabled.';
                                  end;
    LongInt($80004017)          : begin
                                    HrStr := 'CO_E_RUNAS_SYNTAX';
                                    HrDescr := 'A RunAs specification must be <domain name>\<user name> or simply <user name>.';
                                  end;
    LongInt($80004018)          : begin
                                    HrStr := 'CO_E_CREATEPROCESS_FAILURE';
                                    HrDescr := 'The server process could not be started.' +
                                               'The path name might be incorrect.';
                                  end;
    LongInt($80004019)          : begin
                                    HrStr := 'CO_E_RUNAS_CREATEPROCESS_FAILURE';
                                    HrDescr := 'The server process could not be started as the configured identity.' +
                                               'The path name might be incorrect or unavailable.';
                                  end;
    LongInt($8000401A)          : begin
                                    HrStr := 'CO_E_RUNAS_LOGON_FAILURE';
                                    HrDescr := 'The server process could not be started because the configured identity is incorrect.' +
                                               'Check the user name and password.';
                                  end;
    LongInt($8000401B)          : begin
                                    HrStr := 'CO_E_LAUNCH_PERMSSION_DENIED';
                                    HrDescr := 'The client is not allowed to launch this server.';
                                  end;
    LongInt($8000401C)          : begin
                                    HrStr := 'CO_E_START_SERVICE_FAILURE';
                                    HrDescr := 'The service providing this server could not be started.';
                                  end;
    LongInt($8000401D)          : begin
                                    HrStr := 'CO_E_REMOTE_COMMUNICATION_FAILURE';
                                    HrDescr := 'This computer was unable to communicate with the computer providing the server.';
                                  end;
    LongInt($8000401E)          : begin
                                    HrStr := 'CO_E_SERVER_START_TIMEOUT';
                                    HrDescr := 'The server did not respond after being launched.';
                                  end;
    LongInt($8000401F)          : begin
                                    HrStr := 'CO_E_CLSREG_INCONSISTENT';
                                    HrDescr := 'The registration information for this server is inconsistent or incomplete.';
                                  end;
    LongInt($80004020)          : begin
                                    HrStr := 'CO_E_IIDREG_INCONSISTENT';
                                    HrDescr := 'The registration information for this interface is inconsistent or incomplete.';
                                  end;
    LongInt($80004021)          : begin
                                    HrStr := 'CO_E_NOT_SUPPORTED';
                                    HrDescr := 'The operation attempted is not supported.';
                                  end;
    LongInt($80004022)          : begin
                                    HrStr := 'CO_E_RELOAD_DLL';
                                    HrDescr := 'A DLL must be loaded.';
                                  end;
    LongInt($80004023)          : begin
                                    HrStr := 'CO_E_MSI_ERROR';
                                    HrDescr := 'A Microsoft Software Installer error was encountered.';
                                  end;
    LongInt($80004024)          : begin
                                    HrStr := 'CO_E_ATTEMPT_TO_CREATE_OUTSIDE_CLIENT_CONTEXT';
                                    HrDescr := 'The specified activation could not occur in the client context as specified.';
                                  end;
    LongInt($80004025)          : begin
                                    HrStr := 'CO_E_SERVER_PAUSED';
                                    HrDescr := 'Activations on the server are paused.';
                                  end;
    LongInt($80004026)          : begin
                                    HrStr := 'CO_E_SERVER_NOT_PAUSED';
                                    HrDescr := 'Activations on the server are not paused.';
                                  end;
    LongInt($80004027)          : begin
                                    HrStr := 'CO_E_CLASS_DISABLED';
                                    HrDescr := 'The component or application containing the component has been disabled.';
                                  end;
    LongInt($80004028)          : begin
                                    HrStr := 'CO_E_CLRNOTAVAILABLE';
                                    HrDescr := 'The common language runtime is not available.';
                                  end;
    LongInt($80004029)          : begin
                                    HrStr := 'CO_E_ASYNC_WORK_REJECTED';
                                    HrDescr := 'The thread-pool rejected the submitted asynchronous work.';
                                  end;
    LongInt($8000402A)          : begin
                                    HrStr := 'CO_E_SERVER_INIT_TIMEOUT';
                                    HrDescr := 'The server started, but it did not finish initializing in a timely fashion.';
                                  end;
    LongInt($8000402B)          : begin
                                    HrStr := 'CO_E_NO_SECCTX_IN_ACTIVATE';
                                    HrDescr := 'Unable to complete the call because there is no COM+ security context inside IObjectControl.Activate.';
                                  end;
    LongInt($80004030)          : begin
                                    HrStr := 'CO_E_TRACKER_CONFIG';
                                    HrDescr := 'The provided tracker configuration is invalid.';
                                  end;
    LongInt($80004031)          : begin
                                    HrStr := 'CO_E_THREADPOOL_CONFIG';
                                    HrDescr := 'The provided thread pool configuration is invalid.';
                                  end;
    LongInt($80004032)          : begin
                                    HrStr := 'CO_E_SXS_CONFIG';
                                    HrDescr := 'The provided side-by-side configuration is invalid.';
                                  end;
    LongInt($80004033)          : begin
                                    HrStr := 'CO_E_MALFORMED_SPN';
                                    HrDescr := 'The server principal name (SPN) obtained during security negotiation is malformed.';
                                  end;
    LongInt($8000FFFF)          : begin
                                    HrStr := 'E_UNEXPECTED';
                                    HrDescr := 'Catastrophic failure.';
                                  end;
    LongInt($80010001)          : begin
                                    HrStr := 'RPC_E_CALL_REJECTED';
                                    HrDescr := 'Call was rejected by callee.';
                                  end;
    LongInt($80010002)          : begin
                                    HrStr := 'RPC_E_CALL_CANCELED';
                                    HrDescr := 'Call was canceled by the message filter.';
                                  end;
    LongInt($80010003)          : begin
                                    HrStr := 'RPC_E_CANTPOST_INSENDCALL';
                                    HrDescr := 'The caller is dispatching an intertask SendMessage call and cannot call out via PostMessage.';
                                  end;
    LongInt($80010004)          : begin
                                    HrStr := 'RPC_E_CANTCALLOUT_INASYNCCALL';
                                    HrDescr := 'The caller is dispatching an asynchronous call and cannot make an outgoing call on behalf of this call.';
                                  end;
    LongInt($80010005)          : begin
                                    HrStr := 'RPC_E_CANTCALLOUT_INEXTERNALCALL';
                                    HrDescr := 'It is illegal to call out while inside message filter.';
                                  end;
    LongInt($80010006)          : begin
                                    HrStr := 'RPC_E_CONNECTION_TERMINATED';
                                    HrDescr := 'The connection terminated or is in a bogus state and can no longer be used.' +
                                               'Other connections are still valid.';
                                  end;
    LongInt($80010007)          : begin
                                    HrStr := 'RPC_E_SERVER_DIED';
                                    HrDescr := 'The callee (the server, not the server application) is not available and disappeared; all connections are invalid.' +
                                               'The call might have executed.';
                                  end;
    LongInt($80010008)          : begin
                                    HrStr := 'RPC_E_CLIENT_DIED';
                                    HrDescr := 'The caller (client) disappeared while the callee (server) was processing a call.';
                                  end;
    LongInt($80010009)          : begin
                                    HrStr := 'RPC_E_INVALID_DATAPACKET';
                                    HrDescr := 'The data packet with the marshaled parameter data is incorrect.';
                                  end;
    LongInt($8001000A)          : begin
                                    HrStr := 'RPC_E_CANTTRANSMIT_CALL';
                                    HrDescr := 'The call was not transmitted properly; the message queue was full and was not emptied after yielding.';
                                  end;
    LongInt($8001000B)          : begin
                                    HrStr := 'RPC_E_CLIENT_CANTMARSHAL_DATA';
                                    HrDescr := 'The client RPC caller cannot marshal the parameter data due to errors (such as low memory).';
                                  end;
    LongInt($8001000C)          : begin
                                    HrStr := 'RPC_E_CLIENT_CANTUNMARSHAL_DATA';
                                    HrDescr := 'The client RPC caller cannot unmarshal the return data due to errors (such as low memory).';
                                  end;
    LongInt($8001000D)          : begin
                                    HrStr := 'RPC_E_SERVER_CANTMARSHAL_DATA';
                                    HrDescr := 'The server RPC callee cannot marshal the return data due to errors (such as low memory).';
                                  end;
    LongInt($8001000E)          : begin
                                    HrStr := 'RPC_E_SERVER_CANTUNMARSHAL_DATA';
                                    HrDescr := 'The server RPC callee cannot unmarshal the parameter data due to errors (such as low memory).';
                                  end;
    LongInt($8001000F)          : begin
                                    HrStr := 'RPC_E_INVALID_DATA';
                                    HrDescr := 'Received data is invalid.' +
                                               'The data might be server or client data.';
                                  end;
    LongInt($80010010)          : begin
                                    HrStr := 'RPC_E_INVALID_PARAMETER';
                                    HrDescr := 'A particular parameter is invalid and cannot be (un)marshaled.';
                                  end;
    LongInt($80010011)          : begin
                                    HrStr := 'RPC_E_CANTCALLOUT_AGAIN';
                                    HrDescr := 'There is no second outgoing call on same channel in DDE conversation.';
                                  end;
    LongInt($80010012)          : begin
                                    HrStr := 'RPC_E_SERVER_DIED_DNE';
                                    HrDescr := 'The callee (the server, not the server application) is not available and disappeared; all connections are invalid.' +
                                               'The call did not execute.';
                                  end;
    LongInt($80010100)          : begin
                                    HrStr := 'RPC_E_SYS_CALL_FAILED';
                                    HrDescr := 'System call failed.';
                                  end;
    LongInt($80010101)          : begin
                                    HrStr := 'RPC_E_OUT_OF_RESOURCES';
                                    HrDescr := 'Could not allocate some required resource (such as memory or events)';
                                  end;
    LongInt($80010102)          : begin
                                    HrStr := 'RPC_E_ATTEMPTED_MULTITHREAD';
                                    HrDescr := 'Attempted to make calls on more than one thread in single-threaded mode.';
                                  end;
    LongInt($80010103)          : begin
                                    HrStr := 'RPC_E_NOT_REGISTERED';
                                    HrDescr := 'The requested interface is not registered on the server object.';
                                  end;
    LongInt($80010104)          : begin
                                    HrStr := 'RPC_E_FAULT';
                                    HrDescr := 'RPC could not call the server or could not return the results of calling the server.';
                                  end;
    LongInt($80010105)          : begin
                                    HrStr := 'RPC_E_SERVERFAULT';
                                    HrDescr := 'The server threw an exception.';
                                  end;
    LongInt($80010106)          : begin
                                    HrStr := 'RPC_E_CHANGED_MODE';
                                    HrDescr := 'Cannot change thread mode after it is set.';
                                  end;
    LongInt($80010107)          : begin
                                    HrStr := 'RPC_E_INVALIDMETHOD';
                                    HrDescr := 'The method called does not exist on the server.';
                                  end;
    LongInt($80010108)          : begin
                                    HrStr := 'RPC_E_DISCONNECTED';
                                    HrDescr := 'The object invoked has disconnected from its clients.';
                                  end;
    LongInt($80010109)          : begin
                                    HrStr := 'RPC_E_RETRY';
                                    HrDescr := 'The object invoked chose not to process the call now.' +
                                               'Try again later.';
                                  end;
    LongInt($8001010A)          : begin
                                    HrStr := 'RPC_E_SERVERCALL_RETRYLATER';
                                    HrDescr := 'The message filter indicated that the application is busy.';
                                  end;
    LongInt($8001010B)          : begin
                                    HrStr := 'RPC_E_SERVERCALL_REJECTED';
                                    HrDescr := 'The message filter rejected the call.';
                                  end;
    LongInt($8001010C)          : begin
                                    HrStr := 'RPC_E_INVALID_CALLDATA';
                                    HrDescr := 'A call control interface was called with invalid data.';
                                  end;
    LongInt($8001010D)          : begin
                                    HrStr := 'RPC_E_CANTCALLOUT_ININPUTSYNCCALL';
                                    HrDescr := 'An outgoing call cannot be made because the application is dispatching an input-synchronous call.';
                                  end;
    LongInt($8001010E)          : begin
                                    HrStr := 'RPC_E_WRONG_THREAD';
                                    HrDescr := 'The application called an interface that was marshaled for a different thread.';
                                  end;
    LongInt($8001010F)          : begin
                                    HrStr := 'RPC_E_THREAD_NOT_INIT';
                                    HrDescr := 'CoInitialize has not been called on the current thread.';
                                  end;
    LongInt($80010110)          : begin
                                    HrStr := 'RPC_E_VERSION_MISMATCH';
                                    HrDescr := 'The version of OLE on the client and server machines does not match.';
                                  end;
    LongInt($80010111)          : begin
                                    HrStr := 'RPC_E_INVALID_HEADER';
                                    HrDescr := 'OLE received a packet with an invalid header.';
                                  end;
    LongInt($80010112)          : begin
                                    HrStr := 'RPC_E_INVALID_EXTENSION';
                                    HrDescr := 'OLE received a packet with an invalid extension.';
                                  end;
    LongInt($80010113)          : begin
                                    HrStr := 'RPC_E_INVALID_IPID';
                                    HrDescr := 'The requested object or interface does not exist.';
                                  end;
    LongInt($80010114)          : begin
                                    HrStr := 'RPC_E_INVALID_OBJECT';
                                    HrDescr := 'The requested object does not exist.';
                                  end;
    LongInt($80010115)          : begin
                                    HrStr := 'RPC_S_CALLPENDING';
                                    HrDescr := 'OLE has sent a request and is waiting for a reply.';
                                  end;
    LongInt($80010116)          : begin
                                    HrStr := 'RPC_S_WAITONTIMER';
                                    HrDescr := 'OLE is waiting before retrying a request.';
                                  end;
    LongInt($80010117)          : begin
                                    HrStr := 'RPC_E_CALL_COMPLETE';
                                    HrDescr := 'Call context cannot be accessed after call completed.';
                                  end;
    LongInt($80010118)          : begin
                                    HrStr := 'RPC_E_UNSECURE_CALL';
                                    HrDescr := 'Impersonate on unsecure calls is not supported.';
                                  end;
    LongInt($80010119)          : begin
                                    HrStr := 'RPC_E_TOO_LATE';
                                    HrDescr := 'Security must be initialized before any interfaces are marshaled or unmarshaled.' +
                                               'It cannot be changed after initialized.';
                                  end;
    LongInt($8001011A)          : begin
                                    HrStr := 'RPC_E_NO_GOOD_SECURITY_PACKAGES';
                                    HrDescr := 'No security packages are installed on this machine, the user is not logged on, or there are no compatible security packages between the client and server.';
                                  end;
    LongInt($8001011B)          : begin
                                    HrStr := 'RPC_E_ACCESS_DENIED';
                                    HrDescr := 'Access is denied.';
                                  end;
    LongInt($8001011C)          : begin
                                    HrStr := 'RPC_E_REMOTE_DISABLED';
                                    HrDescr := 'Remote calls are not allowed for this process.';
                                  end;
    LongInt($8001011D)          : begin
                                    HrStr := 'RPC_E_INVALID_OBJREF';
                                    HrDescr := 'The marshaled interface data packet (OBJREF) has an invalid or unknown format.';
                                  end;
    LongInt($8001011E)          : begin
                                    HrStr := 'RPC_E_NO_CONTEXT';
                                    HrDescr := 'No context is associated with this call.' +
                                               'This happens for some custom marshaled calls and on the client side of the call.';
                                  end;
    LongInt($8001011F)          : begin
                                    HrStr := 'RPC_E_TIMEOUT';
                                    HrDescr := 'This operation returned because the time-out period expired.';
                                  end;
    LongInt($80010120)          : begin
                                    HrStr := 'RPC_E_NO_SYNC';
                                    HrDescr := 'There are no synchronize objects to wait on.';
                                  end;
    LongInt($80010121)          : begin
                                    HrStr := 'RPC_E_FULLSIC_REQUIRED';
                                    HrDescr := 'Full subject issuer chain Secure Sockets Layer (SSL) principal name expected from the server.';
                                  end;
    LongInt($80010122)          : begin
                                    HrStr := 'RPC_E_INVALID_STD_NAME';
                                    HrDescr := 'Principal name is not a valid Microsoft standard (msstd) name.';
                                  end;
    LongInt($80010123)          : begin
                                    HrStr := 'CO_E_FAILEDTOIMPERSONATE';
                                    HrDescr := 'Unable to impersonate DCOM client.';
                                  end;
    LongInt($80010124)          : begin
                                    HrStr := 'CO_E_FAILEDTOGETSECCTX';
                                    HrDescr := 'Unable to obtain server''s security context.';
                                  end;
    LongInt($80010125)          : begin
                                    HrStr := 'CO_E_FAILEDTOOPENTHREADTOKEN';
                                    HrDescr := 'Unable to open the access token of the current thread.';
                                  end;
    LongInt($80010126)          : begin
                                    HrStr := 'CO_E_FAILEDTOGETTOKENINFO';
                                    HrDescr := 'Unable to obtain user information from an access token.';
                                  end;
    LongInt($80010127)          : begin
                                    HrStr := 'CO_E_TRUSTEEDOESNTMATCHCLIENT';
                                    HrDescr := 'The client who called IAccessControl::IsAccessPermitted was not the trustee provided to the method.';
                                  end;
    LongInt($80010128)          : begin
                                    HrStr := 'CO_E_FAILEDTOQUERYCLIENTBLANKET';
                                    HrDescr := 'Unable to obtain the client''s security blanket.';
                                  end;
    LongInt($80010129)          : begin
                                    HrStr := 'CO_E_FAILEDTOSETDACL';
                                    HrDescr := 'Unable to set a discretionary access control list (ACL) into a security descriptor.';
                                  end;
    LongInt($8001012A)          : begin
                                    HrStr := 'CO_E_ACCESSCHECKFAILED';
                                    HrDescr := 'The system function AccessCheck returned false.';
                                  end;
    LongInt($8001012B)          : begin
                                    HrStr := 'CO_E_NETACCESSAPIFAILED';
                                    HrDescr := 'Either NetAccessDel or NetAccessAdd returned an error code.';
                                  end;
    LongInt($8001012C)          : begin
                                    HrStr := 'CO_E_WRONGTRUSTEENAMESYNTAX';
                                    HrDescr := 'One of the trustee strings provided by the user did not conform to the <Domain>\<Name> syntax and it was not the *" string".';
                                  end;
    LongInt($8001012D)          : begin
                                    HrStr := 'CO_E_INVALIDSID';
                                    HrDescr := 'One of the security identifiers provided by the user was invalid.';
                                  end;
    LongInt($8001012E)          : begin
                                    HrStr := 'CO_E_CONVERSIONFAILED';
                                    HrDescr := 'Unable to convert a wide character trustee string to a multiple-byte trustee string.';
                                  end;
    LongInt($8001012F)          : begin
                                    HrStr := 'CO_E_NOMATCHINGSIDFOUND';
                                    HrDescr := 'Unable to find a security identifier that corresponds to a trustee string provided by the user.';
                                  end;
    LongInt($80010130)          : begin
                                    HrStr := 'CO_E_LOOKUPACCSIDFAILED';
                                    HrDescr := 'The system function LookupAccountSID failed.';
                                  end;
    LongInt($80010131)          : begin
                                    HrStr := 'CO_E_NOMATCHINGNAMEFOUND';
                                    HrDescr := 'Unable to find a trustee name that corresponds to a security identifier provided by the user.';
                                  end;
    LongInt($80010132)          : begin
                                    HrStr := 'CO_E_LOOKUPACCNAMEFAILED';
                                    HrDescr := 'The system function LookupAccountName failed.';
                                  end;
    LongInt($80010133)          : begin
                                    HrStr := 'CO_E_SETSERLHNDLFAILED';
                                    HrDescr := 'Unable to set or reset a serialization handle.';
                                  end;
    LongInt($80010134)          : begin
                                    HrStr := 'CO_E_FAILEDTOGETWINDIR';
                                    HrDescr := 'Unable to obtain the Windows directory.';
                                  end;
    LongInt($80010135)          : begin
                                    HrStr := 'CO_E_PATHTOOLONG';
                                    HrDescr := 'Path too long.';
                                  end;
    LongInt($80010136)          : begin
                                    HrStr := 'CO_E_FAILEDTOGENUUID';
                                    HrDescr := 'Unable to generate a UUID.';
                                  end;
    LongInt($80010137)          : begin
                                    HrStr := 'CO_E_FAILEDTOCREATEFILE';
                                    HrDescr := 'Unable to create file.';
                                  end;
    LongInt($80010138)          : begin
                                    HrStr := 'CO_E_FAILEDTOCLOSEHANDLE';
                                    HrDescr := 'Unable to close a serialization handle or a file handle.';
                                  end;
    LongInt($80010139)          : begin
                                    HrStr := 'CO_E_EXCEEDSYSACLLIMIT';
                                    HrDescr := 'The number of access control entries (ACEs) in an ACL exceeds the system limit.';
                                  end;
    LongInt($8001013A)          : begin
                                    HrStr := 'CO_E_ACESINWRONGORDER';
                                    HrDescr := 'Not all the DENY_ACCESS ACEs are arranged in front of the GRANT_ACCESS ACEs in the stream.';
                                  end;
    LongInt($8001013B)          : begin
                                    HrStr := 'CO_E_INCOMPATIBLESTREAMVERSION';
                                    HrDescr := 'The version of ACL format in the stream is not supported by this implementation of IAccessControl.';
                                  end;
    LongInt($8001013C)          : begin
                                    HrStr := 'CO_E_FAILEDTOOPENPROCESSTOKEN';
                                    HrDescr := 'Unable to open the access token of the server process.';
                                  end;
    LongInt($8001013D)          : begin
                                    HrStr := 'CO_E_DECODEFAILED';
                                    HrDescr := 'Unable to decode the ACL in the stream provided by the user.';
                                  end;
    LongInt($8001013F)          : begin
                                    HrStr := 'CO_E_ACNOTINITIALIZED';
                                    HrDescr := 'The COM IAccessControl object is not initialized.';
                                  end;
    LongInt($80010140)          : begin
                                    HrStr := 'CO_E_CANCEL_DISABLED';
                                    HrDescr := 'Call Cancellation is disabled.';
                                  end;
    LongInt($8001FFFF)          : begin
                                    HrStr := 'RPC_E_UNEXPECTED';
                                    HrDescr := 'An internal error occurred.';
                                  end;
    LongInt($80020001)          : begin
                                    HrStr := 'DISP_E_UNKNOWNINTERFACE';
                                    HrDescr := 'Unknown interface.';
                                  end;
    LongInt($80020003)          : begin
                                    HrStr := 'DISP_E_MEMBERNOTFOUND';
                                    HrDescr := 'Member not found.';
                                  end;
    LongInt($80020004)          : begin
                                    HrStr := 'DISP_E_PARAMNOTFOUND';
                                    HrDescr := 'Parameter not found.';
                                  end;
    LongInt($80020005)          : begin
                                    HrStr := 'DISP_E_TYPEMISMATCH';
                                    HrDescr := 'Type mismatch.';
                                  end;
    LongInt($80020006)          : begin
                                    HrStr := 'DISP_E_UNKNOWNNAME';
                                    HrDescr := 'Unknown name.';
                                  end;
    LongInt($80020007)          : begin
                                    HrStr := 'DISP_E_NONAMEDARGS';
                                    HrDescr := 'No named arguments.';
                                  end;
    LongInt($80020008)          : begin
                                    HrStr := 'DISP_E_BADVARTYPE';
                                    HrDescr := 'Bad variable type.';
                                  end;
    LongInt($80020009)          : begin
                                    HrStr := 'DISP_E_EXCEPTION';
                                    HrDescr := 'Exception occurred.';
                                  end;
    LongInt($8002000A)          : begin
                                    HrStr := 'DISP_E_OVERFLOW';
                                    HrDescr := 'Out of present range.';
                                  end;
    LongInt($8002000B)          : begin
                                    HrStr := 'DISP_E_BADINDEX';
                                    HrDescr := 'Invalid index.';
                                  end;
    LongInt($8002000C)          : begin
                                    HrStr := 'DISP_E_UNKNOWNLCID';
                                    HrDescr := 'Unknown language.';
                                  end;
    LongInt($8002000D)          : begin
                                    HrStr := 'DISP_E_ARRAYISLOCKED';
                                    HrDescr := 'Memory is locked.';
                                  end;
    LongInt($8002000E)          : begin
                                    HrStr := 'DISP_E_BADPARAMCOUNT';
                                    HrDescr := 'Invalid number of parameters.';
                                  end;
    LongInt($8002000F)          : begin
                                    HrStr := 'DISP_E_PARAMNOTOPTIONAL';
                                    HrDescr := 'Parameter not optional.';
                                  end;
    LongInt($80020010)          : begin
                                    HrStr := 'DISP_E_BADCALLEE';
                                    HrDescr := 'Invalid callee.';
                                  end;
    LongInt($80020011)          : begin
                                    HrStr := 'DISP_E_NOTACOLLECTION';
                                    HrDescr := 'Does not support a collection.';
                                  end;
    LongInt($80020012)          : begin
                                    HrStr := 'DISP_E_DIVBYZERO';
                                    HrDescr := 'Division by zero.';
                                  end;
    LongInt($80020013)          : begin
                                    HrStr := 'DISP_E_BUFFERTOOSMALL';
                                    HrDescr := 'Buffer too small.';
                                  end;
    LongInt($80028016)          : begin
                                    HrStr := 'TYPE_E_BUFFERTOOSMALL';
                                    HrDescr := 'Buffer too small.';
                                  end;
    LongInt($80028017)          : begin
                                    HrStr := 'TYPE_E_FIELDNOTFOUND';
                                    HrDescr := 'Field name not defined in the record.';
                                  end;
    LongInt($80028018)          : begin
                                    HrStr := 'TYPE_E_INVDATAREAD';
                                    HrDescr := 'Old format or invalid type library.';
                                  end;
    LongInt($80028019)          : begin
                                    HrStr := 'TYPE_E_UNSUPFORMAT';
                                    HrDescr := 'Old format or invalid type library.';
                                  end;
    LongInt($8002801C)          : begin
                                    HrStr := 'TYPE_E_REGISTRYACCESS';
                                    HrDescr := 'Error accessing the OLE registry.';
                                  end;
    LongInt($8002801D)          : begin
                                    HrStr := 'TYPE_E_LIBNOTREGISTERED';
                                    HrDescr := 'Library not registered.';
                                  end;
    LongInt($80028027)          : begin
                                    HrStr := 'TYPE_E_UNDEFINEDTYPE';
                                    HrDescr := 'Bound to unknown type.';
                                  end;
    LongInt($80028028)          : begin
                                    HrStr := 'TYPE_E_QUALIFIEDNAMEDISALLOWED';
                                    HrDescr := 'Qualified name disallowed.';
                                  end;
    LongInt($80028029)          : begin
                                    HrStr := 'TYPE_E_INVALIDSTATE';
                                    HrDescr := 'Invalid forward reference, or reference to uncompiled type.';
                                  end;
    LongInt($8002802A)          : begin
                                    HrStr := 'TYPE_E_WRONGTYPEKIND';
                                    HrDescr := 'Type mismatch.';
                                  end;
    LongInt($8002802B)          : begin
                                    HrStr := 'TYPE_E_ELEMENTNOTFOUND';
                                    HrDescr := 'Element not found.';
                                  end;
    LongInt($8002802C)          : begin
                                    HrStr := 'TYPE_E_AMBIGUOUSNAME';
                                    HrDescr := 'Ambiguous name.';
                                  end;
    LongInt($8002802D)          : begin
                                    HrStr := 'TYPE_E_NAMECONFLICT';
                                    HrDescr := 'Name already exists in the library.';
                                  end;
    LongInt($8002802E)          : begin
                                    HrStr := 'TYPE_E_UNKNOWNLCID';
                                    HrDescr := 'Unknown language code identifier (LCID).';
                                  end;
    LongInt($8002802F)          : begin
                                    HrStr := 'TYPE_E_DLLFUNCTIONNOTFOUND';
                                    HrDescr := 'Function not defined in specified DLL.';
                                  end;
    LongInt($800288BD)          : begin
                                    HrStr := 'TYPE_E_BADMODULEKIND';
                                    HrDescr := 'Wrong module kind for the operation.';
                                  end;
    LongInt($800288C5)          : begin
                                    HrStr := 'TYPE_E_SIZETOOBIG';
                                    HrDescr := 'Size cannot exceed 64 KB.';
                                  end;
    LongInt($800288C6)          : begin
                                    HrStr := 'TYPE_E_DUPLICATEID';
                                    HrDescr := 'Duplicate ID in inheritance hierarchy.';
                                  end;
    LongInt($800288CF)          : begin
                                    HrStr := 'TYPE_E_INVALIDID';
                                    HrDescr := 'Incorrect inheritance depth in standard OLE hmember.';
                                  end;
    LongInt($80028CA0)          : begin
                                    HrStr := 'TYPE_E_TYPEMISMATCH';
                                    HrDescr := 'Type mismatch.';
                                  end;
    LongInt($80028CA1)          : begin
                                    HrStr := 'TYPE_E_OUTOFBOUNDS';
                                    HrDescr := 'Invalid number of arguments.';
                                  end;
    LongInt($80028CA2)          : begin
                                    HrStr := 'TYPE_E_IOERROR';
                                    HrDescr := 'I/O error.';
                                  end;
    LongInt($80028CA3)          : begin
                                    HrStr := 'TYPE_E_CANTCREATETMPFILE';
                                    HrDescr := 'Error creating unique .tmp file.';
                                  end;
    LongInt($80029C4A)          : begin
                                    HrStr := 'TYPE_E_CANTLOADLIBRARY';
                                    HrDescr := 'Error loading type library or DLL.';
                                  end;
    LongInt($80029C83)          : begin
                                    HrStr := 'TYPE_E_INCONSISTENTPROPFUNCS';
                                    HrDescr := 'Inconsistent property functions.';
                                  end;
    LongInt($80029C84)          : begin
                                    HrStr := 'TYPE_E_CIRCULARTYPE';
                                    HrDescr := 'Circular dependency between types and modules.';
                                  end;
    LongInt($80030001)          : begin
                                    HrStr := 'STG_E_INVALIDFUNCTION';
                                    HrDescr := 'Unable to perform requested operation.';
                                  end;
    LongInt($80030002)          : begin
                                    HrStr := 'STG_E_FILENOTFOUND';
                                    HrDescr := '%1 could not be found.';
                                  end;
    LongInt($80030003)          : begin
                                    HrStr := 'STG_E_PATHNOTFOUND';
                                    HrDescr := 'The path %1 could not be found.';
                                  end;
    LongInt($80030004)          : begin
                                    HrStr := 'STG_E_TOOMANYOPENFILES';
                                    HrDescr := 'There are insufficient resources to open another file.';
                                  end;
    LongInt($80030005)          : begin
                                    HrStr := 'STG_E_ACCESSDENIED';
                                    HrDescr := 'Access denied.';
                                  end;
    LongInt($80030006)          : begin
                                    HrStr := 'STG_E_INVALIDHANDLE';
                                    HrDescr := 'Attempted an operation on an invalid object.';
                                  end;
    LongInt($80030008)          : begin
                                    HrStr := 'STG_E_INSUFFICIENTMEMORY';
                                    HrDescr := 'There is insufficient memory available to complete operation.';
                                  end;
    LongInt($80030009)          : begin
                                    HrStr := 'STG_E_INVALIDPOINTER';
                                    HrDescr := 'Invalid pointer error.';
                                  end;
    LongInt($80030012)          : begin
                                    HrStr := 'STG_E_NOMOREFILES';
                                    HrDescr := 'There are no more entries to return.';
                                  end;
    LongInt($80030013)          : begin
                                    HrStr := 'STG_E_DISKISWRITEPROTECTED';
                                    HrDescr := 'Disk is write-protected.';
                                  end;
    LongInt($80030019)          : begin
                                    HrStr := 'STG_E_SEEKERROR';
                                    HrDescr := 'An error occurred during a seek operation.';
                                  end;
    LongInt($8003001D)          : begin
                                    HrStr := 'STG_E_WRITEFAULT';
                                    HrDescr := 'A disk error occurred during a write operation.';
                                  end;
    LongInt($8003001E)          : begin
                                    HrStr := 'STG_E_READFAULT';
                                    HrDescr := 'A disk error occurred during a read operation.';
                                  end;
    LongInt($80030020)          : begin
                                    HrStr := 'STG_E_SHAREVIOLATION';
                                    HrDescr := 'A share violation has occurred.';
                                  end;
    LongInt($80030021)          : begin
                                    HrStr := 'STG_E_LOCKVIOLATION';
                                    HrDescr := 'A lock violation has occurred.';
                                  end;
    LongInt($80030050)          : begin
                                    HrStr := 'STG_E_FILEALREADYEXISTS';
                                    HrDescr := '%1 already exists.';
                                  end;
    LongInt($80030057)          : begin
                                    HrStr := 'STG_E_INVALIDPARAMETER';
                                    HrDescr := 'Invalid parameter error.';
                                  end;
    LongInt($80030070)          : begin
                                    HrStr := 'STG_E_MEDIUMFULL';
                                    HrDescr := 'There is insufficient disk space to complete operation.';
                                  end;
    LongInt($800300F0)          : begin
                                    HrStr := 'STG_E_PROPSETMISMATCHED';
                                    HrDescr := 'Illegal write of non-simple property to simple property set.';
                                  end;
    LongInt($800300FA)          : begin
                                    HrStr := 'STG_E_ABNORMALAPIEXIT';
                                    HrDescr := 'An application programming interface (API) call exited abnormally.';
                                  end;
    LongInt($800300FB)          : begin
                                    HrStr := 'STG_E_INVALIDHEADER';
                                    HrDescr := 'The file %1 is not a valid compound file.';
                                  end;
    LongInt($800300FC)          : begin
                                    HrStr := 'STG_E_INVALIDNAME';
                                    HrDescr := 'The name %1 is not valid.';
                                  end;
    LongInt($800300FD)          : begin
                                    HrStr := 'STG_E_UNKNOWN';
                                    HrDescr := 'An unexpected error occurred.';
                                  end;
    LongInt($800300FE)          : begin
                                    HrStr := 'STG_E_UNIMPLEMENTEDFUNCTION';
                                    HrDescr := 'That function is not implemented.';
                                  end;
    LongInt($800300FF)          : begin
                                    HrStr := 'STG_E_INVALIDFLAG';
                                    HrDescr := 'Invalid flag error.';
                                  end;
    LongInt($80030100)          : begin
                                    HrStr := 'STG_E_INUSE';
                                    HrDescr := 'Attempted to use an object that is busy.';
                                  end;
    LongInt($80030101)          : begin
                                    HrStr := 'STG_E_NOTCURRENT';
                                    HrDescr := 'The storage has been changed since the last commit.';
                                  end;
    LongInt($80030102)          : begin
                                    HrStr := 'STG_E_REVERTED';
                                    HrDescr := 'Attempted to use an object that has ceased to exist.';
                                  end;
    LongInt($80030103)          : begin
                                    HrStr := 'STG_E_CANTSAVE';
                                    HrDescr := 'Cannot save.';
                                  end;
    LongInt($80030104)          : begin
                                    HrStr := 'STG_E_OLDFORMAT';
                                    HrDescr := 'The compound file %1 was produced with an incompatible version of storage.';
                                  end;
    LongInt($80030105)          : begin
                                    HrStr := 'STG_E_OLDDLL';
                                    HrDescr := 'The compound file %1 was produced with a newer version of storage.';
                                  end;
    LongInt($80030106)          : begin
                                    HrStr := 'STG_E_SHAREREQUIRED';
                                    HrDescr := 'Share.exe or equivalent is required for operation.';
                                  end;
    LongInt($80030107)          : begin
                                    HrStr := 'STG_E_NOTFILEBASEDSTORAGE';
                                    HrDescr := 'Illegal operation called on non-file based storage.';
                                  end;
    LongInt($80030108)          : begin
                                    HrStr := 'STG_E_EXTANTMARSHALLINGS';
                                    HrDescr := 'Illegal operation called on object with extant marshalings.';
                                  end;
    LongInt($80030109)          : begin
                                    HrStr := 'STG_E_DOCFILECORRUPT';
                                    HrDescr := 'The docfile has been corrupted.';
                                  end;
    LongInt($80030110)          : begin
                                    HrStr := 'STG_E_BADBASEADDRESS';
                                    HrDescr := 'OLE32.DLL has been loaded at the wrong address.';
                                  end;
    LongInt($80030111)          : begin
                                    HrStr := 'STG_E_DOCFILETOOLARGE';
                                    HrDescr := 'The compound file is too large for the current implementation.';
                                  end;
    LongInt($80030112)          : begin
                                    HrStr := 'STG_E_NOTSIMPLEFORMAT';
                                    HrDescr := 'The compound file was not created with the STGM_SIMPLE flag.';
                                  end;
    LongInt($80030201)          : begin
                                    HrStr := 'STG_E_INCOMPLETE';
                                    HrDescr := 'The file download was aborted abnormally.' +
                                               'The file is incomplete.';
                                  end;
    LongInt($80030202)          : begin
                                    HrStr := 'STG_E_TERMINATED';
                                    HrDescr := 'The file download has been terminated.';
                                  end;
    LongInt($80030305)          : begin
                                    HrStr := 'STG_E_STATUS_COPY_PROTECTION_FAILURE';
                                    HrDescr := 'Generic Copy Protection Error.';
                                  end;
    LongInt($80030306)          : begin
                                    HrStr := 'STG_E_CSS_AUTHENTICATION_FAILURE';
                                    HrDescr := 'Copy Protection Error—DVD CSS Authentication failed.';
                                  end;
    LongInt($80030307)          : begin
                                    HrStr := 'STG_E_CSS_KEY_NOT_PRESENT';
                                    HrDescr := 'Copy Protection Error—The given sector does not have a valid CSS key.';
                                  end;
    LongInt($80030308)          : begin
                                    HrStr := 'STG_E_CSS_KEY_NOT_ESTABLISHED';
                                    HrDescr := 'Copy Protection Error—DVD session key not established.';
                                  end;
    LongInt($80030309)          : begin
                                    HrStr := 'STG_E_CSS_SCRAMBLED_SECTOR';
                                    HrDescr := 'Copy Protection Error—The read failed because the sector is encrypted.';
                                  end;
    LongInt($8003030A)          : begin
                                    HrStr := 'STG_E_CSS_REGION_MISMATCH';
                                    HrDescr := 'Copy Protection Error—The current DVD''s region does not correspond to the region setting of the drive.';
                                  end;
    LongInt($8003030B)          : begin
                                    HrStr := 'STG_E_RESETS_EXHAUSTED';
                                    HrDescr := 'Copy Protection Error—The drive''s region setting might be permanent or the number of user resets has been exhausted.';
                                  end;
    LongInt($80040000)          : begin
                                    HrStr := 'OLE_E_OLEVERB';
                                    HrDescr := 'Invalid OLEVERB structure.';
                                  end;
    LongInt($80040001)          : begin
                                    HrStr := 'OLE_E_ADVF';
                                    HrDescr := 'Invalid advise flags.';
                                  end;
    LongInt($80040002)          : begin
                                    HrStr := 'OLE_E_ENUM_NOMORE';
                                    HrDescr := 'Cannot enumerate any more because the associated data is missing.';
                                  end;
    LongInt($80040003)          : begin
                                    HrStr := 'OLE_E_ADVISENOTSUPPORTED';
                                    HrDescr := 'This implementation does not take advises.';
                                  end;
    LongInt($80040004)          : begin
                                    HrStr := 'OLE_E_NOCONNECTION';
                                    HrDescr := 'There is no connection for this connection ID.';
                                  end;
    LongInt($80040005)          : begin
                                    HrStr := 'OLE_E_NOTRUNNING';
                                    HrDescr := 'Need to run the object to perform this operation.';
                                  end;
    LongInt($80040006)          : begin
                                    HrStr := 'OLE_E_NOCACHE';
                                    HrDescr := 'There is no cache to operate on.';
                                  end;
    LongInt($80040007)          : begin
                                    HrStr := 'OLE_E_BLANK';
                                    HrDescr := 'Uninitialized object.';
                                  end;
    LongInt($80040008)          : begin
                                    HrStr := 'OLE_E_CLASSDIFF';
                                    HrDescr := 'Linked object''s source class has changed.';
                                  end;
    LongInt($80040009)          : begin
                                    HrStr := 'OLE_E_CANT_GETMONIKER';
                                    HrDescr := 'Not able to get the moniker of the object.';
                                  end;
    LongInt($8004000A)          : begin
                                    HrStr := 'OLE_E_CANT_BINDTOSOURCE';
                                    HrDescr := 'Not able to bind to the source.';
                                  end;
    LongInt($8004000B)          : begin
                                    HrStr := 'OLE_E_STATIC';
                                    HrDescr := 'Object is static; operation not allowed.';
                                  end;
    LongInt($8004000C)          : begin
                                    HrStr := 'OLE_E_PROMPTSAVECANCELLED';
                                    HrDescr := 'User canceled out of the Save dialog box.';
                                  end;
    LongInt($8004000D)          : begin
                                    HrStr := 'OLE_E_INVALIDRECT';
                                    HrDescr := 'Invalid rectangle.';
                                  end;
    LongInt($8004000E)          : begin
                                    HrStr := 'OLE_E_WRONGCOMPOBJ';
                                    HrDescr := 'compobj.dll is too old for the ole2.dll initialized.';
                                  end;
    LongInt($8004000F)          : begin
                                    HrStr := 'OLE_E_INVALIDHWND';
                                    HrDescr := 'Invalid window handle.';
                                  end;
    LongInt($80040010)          : begin
                                    HrStr := 'OLE_E_NOT_INPLACEACTIVE';
                                    HrDescr := 'Object is not in any of the inplace active states.';
                                  end;
    LongInt($80040011)          : begin
                                    HrStr := 'OLE_E_CANTCONVERT';
                                    HrDescr := 'Not able to convert object.';
                                  end;
    LongInt($80040012)          : begin
                                    HrStr := 'OLE_E_NOSTORAGE';
                                    HrDescr := 'Not able to perform the operation because object is not given storage yet.';
                                  end;
    LongInt($80040064)          : begin
                                    HrStr := 'DV_E_FORMATETC';
                                    HrDescr := 'Invalid FORMATETC structure.';
                                  end;
    LongInt($80040065)          : begin
                                    HrStr := 'DV_E_DVTARGETDEVICE';
                                    HrDescr := 'Invalid DVTARGETDEVICE structure.';
                                  end;
    LongInt($80040066)          : begin
                                    HrStr := 'DV_E_STGMEDIUM';
                                    HrDescr := 'Invalid STDGMEDIUM structure.';
                                  end;
    LongInt($80040067)          : begin
                                    HrStr := 'DV_E_STATDATA';
                                    HrDescr := 'Invalid STATDATA structure.';
                                  end;
    LongInt($80040068)          : begin
                                    HrStr := 'DV_E_LINDEX';
                                    HrDescr := 'Invalid lindex.';
                                  end;
    LongInt($80040069)          : begin
                                    HrStr := 'DV_E_TYMED';
                                    HrDescr := 'Invalid TYMED structure.';
                                  end;
    LongInt($8004006A)          : begin
                                    HrStr := 'DV_E_CLIPFORMAT';
                                    HrDescr := 'Invalid clipboard format.';
                                  end;
    LongInt($8004006B)          : begin
                                    HrStr := 'DV_E_DVASPECT';
                                    HrDescr := 'Invalid aspects.';
                                  end;
    LongInt($8004006C)          : begin
                                    HrStr := 'DV_E_DVTARGETDEVICE_SIZE';
                                    HrDescr := 'The tdSize parameter of the DVTARGETDEVICE structure is invalid.';
                                  end;
    LongInt($8004006D)          : begin
                                    HrStr := 'DV_E_NOIVIEWOBJECT';
                                    HrDescr := 'Object does not support IViewObject interface.';
                                  end;
    LongInt($80040100)          : begin
                                    HrStr := 'DRAGDROP_E_NOTREGISTERED';
                                    HrDescr := 'Trying to revoke a drop target that has not been registered.';
                                  end;
    LongInt($80040101)          : begin
                                    HrStr := 'DRAGDROP_E_ALREADYREGISTERED';
                                    HrDescr := 'This window has already been registered as a drop target.';
                                  end;
    LongInt($80040102)          : begin
                                    HrStr := 'DRAGDROP_E_INVALIDHWND';
                                    HrDescr := 'Invalid window handle.';
                                  end;
    LongInt($80040110)          : begin
                                    HrStr := 'CLASS_E_NOAGGREGATION';
                                    HrDescr := 'Class does not support aggregation (or class object is remote).';
                                  end;
    LongInt($80040111)          : begin
                                    HrStr := 'CLASS_E_CLASSNOTAVAILABLE';
                                    HrDescr := 'ClassFactory cannot supply requested class.';
                                  end;
    LongInt($80040112)          : begin
                                    HrStr := 'CLASS_E_NOTLICENSED';
                                    HrDescr := 'Class is not licensed for use.';
                                  end;
    LongInt($80040140)          : begin
                                    HrStr := 'VIEW_E_DRAW';
                                    HrDescr := 'Error drawing view.';
                                  end;
    LongInt($80040150)          : begin
                                    HrStr := 'REGDB_E_READREGDB';
                                    HrDescr := 'Could not read key from registry.';
                                  end;
    LongInt($80040151)          : begin
                                    HrStr := 'REGDB_E_WRITEREGDB';
                                    HrDescr := 'Could not write key to registry.';
                                  end;
    LongInt($80040152)          : begin
                                    HrStr := 'REGDB_E_KEYMISSING';
                                    HrDescr := 'Could not find the key in the registry.';
                                  end;
    LongInt($80040153)          : begin
                                    HrStr := 'REGDB_E_INVALIDVALUE';
                                    HrDescr := 'Invalid value for registry.';
                                  end;
    LongInt($80040154)          : begin
                                    HrStr := 'REGDB_E_CLASSNOTREG';
                                    HrDescr := 'Class not registered.';
                                  end;
    LongInt($80040155)          : begin
                                    HrStr := 'REGDB_E_IIDNOTREG';
                                    HrDescr := 'Interface not registered.';
                                  end;
    LongInt($80040156)          : begin
                                    HrStr := 'REGDB_E_BADTHREADINGMODEL';
                                    HrDescr := 'Threading model entry is not valid.';
                                  end;
    LongInt($80040160)          : begin
                                    HrStr := 'CAT_E_CATIDNOEXIST';
                                    HrDescr := 'CATID does not exist.';
                                  end;
    LongInt($80040161)          : begin
                                    HrStr := 'CAT_E_NODESCRIPTION';
                                    HrDescr := 'Description not found.';
                                  end;
    LongInt($80040164)          : begin
                                    HrStr := 'CS_E_PACKAGE_NOTFOUND';
                                    HrDescr := 'No package in the software installation data in Active Directory meets this criteria.';
                                  end;
    LongInt($80040165)          : begin
                                    HrStr := 'CS_E_NOT_DELETABLE';
                                    HrDescr := 'Deleting this will break the referential integrity of the software installation data in Active Directory.';
                                  end;
    LongInt($80040166)          : begin
                                    HrStr := 'CS_E_CLASS_NOTFOUND';
                                    HrDescr := 'The CLSID was not found in the software installation data in Active Directory.';
                                  end;
    LongInt($80040167)          : begin
                                    HrStr := 'CS_E_INVALID_VERSION';
                                    HrDescr := 'The software installation data in Active Directory is corrupt.';
                                  end;
    LongInt($80040168)          : begin
                                    HrStr := 'CS_E_NO_CLASSSTORE';
                                    HrDescr := 'There is no software installation data in Active Directory.';
                                  end;
    LongInt($80040169)          : begin
                                    HrStr := 'CS_E_OBJECT_NOTFOUND';
                                    HrDescr := 'There is no software installation data object in Active Directory.';
                                  end;
    LongInt($8004016A)          : begin
                                    HrStr := 'CS_E_OBJECT_ALREADY_EXISTS';
                                    HrDescr := 'The software installation data object in Active Directory already exists.';
                                  end;
    LongInt($8004016B)          : begin
                                    HrStr := 'CS_E_INVALID_PATH';
                                    HrDescr := 'The path to the software installation data in Active Directory is not correct.';
                                  end;
    LongInt($8004016C)          : begin
                                    HrStr := 'CS_E_NETWORK_ERROR';
                                    HrDescr := 'A network error interrupted the operation.';
                                  end;
    LongInt($8004016D)          : begin
                                    HrStr := 'CS_E_ADMIN_LIMIT_EXCEEDED';
                                    HrDescr := 'The size of this object exceeds the maximum size set by the administrator.';
                                  end;
    LongInt($8004016E)          : begin
                                    HrStr := 'CS_E_SCHEMA_MISMATCH';
                                    HrDescr := 'The schema for the software installation data in Active Directory does not match the required schema.';
                                  end;
    LongInt($8004016F)          : begin
                                    HrStr := 'CS_E_INTERNAL_ERROR';
                                    HrDescr := 'An error occurred in the software installation data in Active Directory.';
                                  end;
    LongInt($80040170)          : begin
                                    HrStr := 'CACHE_E_NOCACHE_UPDATED';
                                    HrDescr := 'Cache not updated.';
                                  end;
    LongInt($80040180)          : begin
                                    HrStr := 'OLEOBJ_E_NOVERBS';
                                    HrDescr := 'No verbs for OLE object.';
                                  end;
    LongInt($80040181)          : begin
                                    HrStr := 'OLEOBJ_E_INVALIDVERB';
                                    HrDescr := 'Invalid verb for OLE object.';
                                  end;
    LongInt($800401A0)          : begin
                                    HrStr := 'INPLACE_E_NOTUNDOABLE';
                                    HrDescr := 'Undo is not available.';
                                  end;
    LongInt($800401A1)          : begin
                                    HrStr := 'INPLACE_E_NOTOOLSPACE';
                                    HrDescr := 'Space for tools is not available.';
                                  end;
    LongInt($800401C0)          : begin
                                    HrStr := 'CONVERT10_E_OLESTREAM_GET';
                                    HrDescr := 'OLESTREAM Get method failed.';
                                  end;
    LongInt($800401C1)          : begin
                                    HrStr := 'CONVERT10_E_OLESTREAM_PUT';
                                    HrDescr := 'OLESTREAM Put method failed.';
                                  end;
    LongInt($800401C2)          : begin
                                    HrStr := 'CONVERT10_E_OLESTREAM_FMT';
                                    HrDescr := 'Contents of the OLESTREAM not in correct format.';
                                  end;
    LongInt($800401C3)          : begin
                                    HrStr := 'CONVERT10_E_OLESTREAM_BITMAP_TO_DIB';
                                    HrDescr := 'There was an error in a Windows GDI call while converting the bitmap to a device-independent bitmap (DIB).';
                                  end;
    LongInt($800401C4)          : begin
                                    HrStr := 'CONVERT10_E_STG_FMT';
                                    HrDescr := 'Contents of the IStorage not in correct format.';
                                  end;
    LongInt($800401C5)          : begin
                                    HrStr := 'CONVERT10_E_STG_NO_STD_STREAM';
                                    HrDescr := 'Contents of IStorage is missing one of the standard streams.';
                                  end;
    LongInt($800401C6)          : begin
                                    HrStr := 'CONVERT10_E_STG_DIB_TO_BITMAP';
                                    HrDescr := 'There was an error in a Windows Graphics Device Interface (GDI) call while converting the DIB to a bitmap.';
                                  end;
    LongInt($800401D0)          : begin
                                    HrStr := 'CLIPBRD_E_CANT_OPEN';
                                    HrDescr := 'OpenClipboard failed.';
                                  end;
    LongInt($800401D1)          : begin
                                    HrStr := 'CLIPBRD_E_CANT_EMPTY';
                                    HrDescr := 'EmptyClipboard failed.';
                                  end;
    LongInt($800401D2)          : begin
                                    HrStr := 'CLIPBRD_E_CANT_SET';
                                    HrDescr := 'SetClipboard failed.';
                                  end;
    LongInt($800401D3)          : begin
                                    HrStr := 'CLIPBRD_E_BAD_DATA';
                                    HrDescr := 'Data on clipboard is invalid.';
                                  end;
    LongInt($800401D4)          : begin
                                    HrStr := 'CLIPBRD_E_CANT_CLOSE';
                                    HrDescr := 'CloseClipboard failed.';
                                  end;
    LongInt($800401E0)          : begin
                                    HrStr := 'MK_E_CONNECTMANUALLY';
                                    HrDescr := 'Moniker needs to be connected manually.';
                                  end;
    LongInt($800401E1)          : begin
                                    HrStr := 'MK_E_EXCEEDEDDEADLINE';
                                    HrDescr := 'Operation exceeded deadline.';
                                  end;
    LongInt($800401E2)          : begin
                                    HrStr := 'MK_E_NEEDGENERIC';
                                    HrDescr := 'Moniker needs to be generic.';
                                  end;
    LongInt($800401E3)          : begin
                                    HrStr := 'MK_E_UNAVAILABLE';
                                    HrDescr := 'Operation unavailable.';
                                  end;
    LongInt($800401E4)          : begin
                                    HrStr := 'MK_E_SYNTAX';
                                    HrDescr := 'Invalid syntax.';
                                  end;
    LongInt($800401E5)          : begin
                                    HrStr := 'MK_E_NOOBJECT';
                                    HrDescr := 'No object for moniker.';
                                  end;
    LongInt($800401E6)          : begin
                                    HrStr := 'MK_E_INVALIDEXTENSION';
                                    HrDescr := 'Bad extension for file.';
                                  end;
    LongInt($800401E7)          : begin
                                    HrStr := 'MK_E_INTERMEDIATEINTERFACENOTSUPPORTED';
                                    HrDescr := 'Intermediate operation failed.';
                                  end;
    LongInt($800401E8)          : begin
                                    HrStr := 'MK_E_NOTBINDABLE';
                                    HrDescr := 'Moniker is not bindable.';
                                  end;
    LongInt($800401E9)          : begin
                                    HrStr := 'MK_E_NOTBOUND';
                                    HrDescr := 'Moniker is not bound.';
                                  end;
    LongInt($800401EA)          : begin
                                    HrStr := 'MK_E_CANTOPENFILE';
                                    HrDescr := 'Moniker cannot open file.';
                                  end;
    LongInt($800401EB)          : begin
                                    HrStr := 'MK_E_MUSTBOTHERUSER';
                                    HrDescr := 'User input required for operation to succeed.';
                                  end;
    LongInt($800401EC)          : begin
                                    HrStr := 'MK_E_NOINVERSE';
                                    HrDescr := 'Moniker class has no inverse.';
                                  end;
    LongInt($800401ED)          : begin
                                    HrStr := 'MK_E_NOSTORAGE';
                                    HrDescr := 'Moniker does not refer to storage.';
                                  end;
    LongInt($800401EE)          : begin
                                    HrStr := 'MK_E_NOPREFIX';
                                    HrDescr := 'No common prefix.';
                                  end;
    LongInt($800401EF)          : begin
                                    HrStr := 'MK_E_ENUMERATION_FAILED';
                                    HrDescr := 'Moniker could not be enumerated.';
                                  end;
    LongInt($800401F0)          : begin
                                    HrStr := 'CO_E_NOTINITIALIZED';
                                    HrDescr := 'CoInitialize has not been called.';
                                  end;
    LongInt($800401F1)          : begin
                                    HrStr := 'CO_E_ALREADYINITIALIZED';
                                    HrDescr := 'CoInitialize has already been called.';
                                  end;
    LongInt($800401F2)          : begin
                                    HrStr := 'CO_E_CANTDETERMINECLASS';
                                    HrDescr := 'Class of object cannot be determined.';
                                  end;
    LongInt($800401F3)          : begin
                                    HrStr := 'CO_E_CLASSSTRING';
                                    HrDescr := 'Invalid class string.';
                                  end;
    LongInt($800401F4)          : begin
                                    HrStr := 'CO_E_IIDSTRING';
                                    HrDescr := 'Invalid interface string.';
                                  end;
    LongInt($800401F5)          : begin
                                    HrStr := 'CO_E_APPNOTFOUND';
                                    HrDescr := 'Application not found.';
                                  end;
    LongInt($800401F6)          : begin
                                    HrStr := 'CO_E_APPSINGLEUSE';
                                    HrDescr := 'Application cannot be run more than once.';
                                  end;
    LongInt($800401F7)          : begin
                                    HrStr := 'CO_E_ERRORINAPP';
                                    HrDescr := 'Some error in application.';
                                  end;
    LongInt($800401F8)          : begin
                                    HrStr := 'CO_E_DLLNOTFOUND';
                                    HrDescr := 'DLL for class not found.';
                                  end;
    LongInt($800401F9)          : begin
                                    HrStr := 'CO_E_ERRORINDLL';
                                    HrDescr := 'Error in the DLL.';
                                  end;
    LongInt($800401FA)          : begin
                                    HrStr := 'CO_E_WRONGOSFORAPP';
                                    HrDescr := 'Wrong operating system or operating system version for application.';
                                  end;
    LongInt($800401FB)          : begin
                                    HrStr := 'CO_E_OBJNOTREG';
                                    HrDescr := 'Object is not registered.';
                                  end;
    LongInt($800401FC)          : begin
                                    HrStr := 'CO_E_OBJISREG';
                                    HrDescr := 'Object is already registered.';
                                  end;
    LongInt($800401FD)          : begin
                                    HrStr := 'CO_E_OBJNOTCONNECTED';
                                    HrDescr := 'Object is not connected to server.';
                                  end;
    LongInt($800401FE)          : begin
                                    HrStr := 'CO_E_APPDIDNTREG';
                                    HrDescr := 'Application was launched, but it did not register a class factory.';
                                  end;
    LongInt($800401FF)          : begin
                                    HrStr := 'CO_E_RELEASED';
                                    HrDescr := 'Object has been released.';
                                  end;
    LongInt($80040201)          : begin
                                    HrStr := 'EVENT_E_ALL_SUBSCRIBERS_FAILED';
                                    HrDescr := 'An event was unable to invoke any of the subscribers.';
                                  end;
    LongInt($80040203)          : begin
                                    HrStr := 'EVENT_E_QUERYSYNTAX';
                                    HrDescr := 'A syntax error occurred trying to evaluate a query string.';
                                  end;
    LongInt($80040204)          : begin
                                    HrStr := 'EVENT_E_QUERYFIELD';
                                    HrDescr := 'An invalid field name was used in a query string.';
                                  end;
    LongInt($80040205)          : begin
                                    HrStr := 'EVENT_E_INTERNALEXCEPTION';
                                    HrDescr := 'An unexpected exception was raised.';
                                  end;
    LongInt($80040206)          : begin
                                    HrStr := 'EVENT_E_INTERNALERROR';
                                    HrDescr := 'An unexpected internal error was detected.';
                                  end;
    LongInt($80040207)          : begin
                                    HrStr := 'EVENT_E_INVALID_PER_USER_SID';
                                    HrDescr := 'The owner security identifier (SID) on a per-user subscription does not exist.';
                                  end;
    LongInt($80040208)          : begin
                                    HrStr := 'EVENT_E_USER_EXCEPTION';
                                    HrDescr := 'A user-supplied component or subscriber raised an exception.';
                                  end;
    LongInt($80040209)          : begin
                                    HrStr := 'EVENT_E_TOO_MANY_METHODS';
                                    HrDescr := 'An interface has too many methods to fire events from.';
                                  end;
    LongInt($8004020A)          : begin
                                    HrStr := 'EVENT_E_MISSING_EVENTCLASS';
                                    HrDescr := 'A subscription cannot be stored unless its event class already exists.';
                                  end;
    LongInt($8004020B)          : begin
                                    HrStr := 'EVENT_E_NOT_ALL_REMOVED';
                                    HrDescr := 'Not all the objects requested could be removed.';
                                  end;
    LongInt($8004020C)          : begin
                                    HrStr := 'EVENT_E_COMPLUS_NOT_INSTALLED';
                                    HrDescr := 'COM+ is required for this operation, but it is not installed.';
                                  end;
    LongInt($8004020D)          : begin
                                    HrStr := 'EVENT_E_CANT_MODIFY_OR_DELETE_UNCONFIGURED_OBJECT';
                                    HrDescr := 'Cannot modify or delete an object that was not added using the COM+ Administrative SDK.';
                                  end;
    LongInt($8004020E)          : begin
                                    HrStr := 'EVENT_E_CANT_MODIFY_OR_DELETE_CONFIGURED_OBJECT';
                                    HrDescr := 'Cannot modify or delete an object that was added using the COM+ Administrative SDK.';
                                  end;
    LongInt($8004020F)          : begin
                                    HrStr := 'EVENT_E_INVALID_EVENT_CLASS_PARTITION';
                                    HrDescr := 'The event class for this subscription is in an invalid partition.';
                                  end;
    LongInt($80040210)          : begin
                                    HrStr := 'EVENT_E_PER_USER_SID_NOT_LOGGED_ON';
                                    HrDescr := 'The owner of the PerUser subscription is not logged on to the system specified.';
                                  end;
    LongInt($80041309)          : begin
                                    HrStr := 'SCHED_E_TRIGGER_NOT_FOUND';
                                    HrDescr := 'Trigger not found.';
                                  end;
    LongInt($8004130A)          : begin
                                    HrStr := 'SCHED_E_TASK_NOT_READY';
                                    HrDescr := 'One or more of the properties that are needed to run this task have not been set.';
                                  end;
    LongInt($8004130B)          : begin
                                    HrStr := 'SCHED_E_TASK_NOT_RUNNING';
                                    HrDescr := 'There is no running instance of the task.';
                                  end;
    LongInt($8004130C)          : begin
                                    HrStr := 'SCHED_E_SERVICE_NOT_INSTALLED';
                                    HrDescr := 'The Task Scheduler service is not installed on this computer.';
                                  end;
    LongInt($8004130D)          : begin
                                    HrStr := 'SCHED_E_CANNOT_OPEN_TASK';
                                    HrDescr := 'The task object could not be opened.';
                                  end;
    LongInt($8004130E)          : begin
                                    HrStr := 'SCHED_E_INVALID_TASK';
                                    HrDescr := 'The object is either an invalid task object or is not a task object.';
                                  end;
    LongInt($8004130F)          : begin
                                    HrStr := 'SCHED_E_ACCOUNT_INFORMATION_NOT_SET';
                                    HrDescr := 'No account information could be found in the Task Scheduler security database for the task indicated.';
                                  end;
    LongInt($80041310)          : begin
                                    HrStr := 'SCHED_E_ACCOUNT_NAME_NOT_FOUND';
                                    HrDescr := 'Unable to establish existence of the account specified.';
                                  end;
    LongInt($80041311)          : begin
                                    HrStr := 'SCHED_E_ACCOUNT_DBASE_CORRUPT';
                                    HrDescr := 'Corruption was detected in the Task Scheduler security database; the database has been reset.';
                                  end;
    LongInt($80041312)          : begin
                                    HrStr := 'SCHED_E_NO_SECURITY_SERVICES';
                                    HrDescr := 'Task Scheduler security services are available only on Windows NT operating system.';
                                  end;
    LongInt($80041313)          : begin
                                    HrStr := 'SCHED_E_UNKNOWN_OBJECT_VERSION';
                                    HrDescr := 'The task object version is either unsupported or invalid.';
                                  end;
    LongInt($80041314)          : begin
                                    HrStr := 'SCHED_E_UNSUPPORTED_ACCOUNT_OPTION';
                                    HrDescr := 'The task has been configured with an unsupported combination of account settings and run-time options.';
                                  end;
    LongInt($80041315)          : begin
                                    HrStr := 'SCHED_E_SERVICE_NOT_RUNNING';
                                    HrDescr := 'The Task Scheduler service is not running.';
                                  end;
    LongInt($80041316)          : begin
                                    HrStr := 'SCHED_E_UNEXPECTEDNODE';
                                    HrDescr := 'The task XML contains an unexpected node.';
                                  end;
    LongInt($80041317)          : begin
                                    HrStr := 'SCHED_E_NAMESPACE';
                                    HrDescr := 'The task XML contains an element or attribute from an unexpected namespace.';
                                  end;
    LongInt($80041318)          : begin
                                    HrStr := 'SCHED_E_INVALIDVALUE';
                                    HrDescr := 'The task XML contains a value that is incorrectly formatted or out of range.';
                                  end;
    LongInt($80041319)          : begin
                                    HrStr := 'SCHED_E_MISSINGNODE';
                                    HrDescr := 'The task XML is missing a required element or attribute.';
                                  end;
    LongInt($8004131A)          : begin
                                    HrStr := 'SCHED_E_MALFORMEDXML';
                                    HrDescr := 'The task XML is malformed.';
                                  end;
    LongInt($8004131D)          : begin
                                    HrStr := 'SCHED_E_TOO_MANY_NODES';
                                    HrDescr := 'The task XML contains too many nodes of the same type.';
                                  end;
    LongInt($8004131E)          : begin
                                    HrStr := 'SCHED_E_PAST_END_BOUNDARY';
                                    HrDescr := 'The task cannot be started after the trigger''s end boundary.';
                                  end;
    LongInt($8004131F)          : begin
                                    HrStr := 'SCHED_E_ALREADY_RUNNING';
                                    HrDescr := 'An instance of this task is already running.';
                                  end;
    LongInt($80041320)          : begin
                                    HrStr := 'SCHED_E_USER_NOT_LOGGED_ON';
                                    HrDescr := 'The task will not run because the user is not logged on.';
                                  end;
    LongInt($80041321)          : begin
                                    HrStr := 'SCHED_E_INVALID_TASK_HASH';
                                    HrDescr := 'The task image is corrupt or has been tampered with.';
                                  end;
    LongInt($80041322)          : begin
                                    HrStr := 'SCHED_E_SERVICE_NOT_AVAILABLE';
                                    HrDescr := 'The Task Scheduler service is not available.';
                                  end;
    LongInt($80041323)          : begin
                                    HrStr := 'SCHED_E_SERVICE_TOO_BUSY';
                                    HrDescr := 'The Task Scheduler service is too busy to handle your request.' +
                                               'Try again later.';
                                  end;
    LongInt($80041324)          : begin
                                    HrStr := 'SCHED_E_TASK_ATTEMPTED';
                                    HrDescr := 'The Task Scheduler service attempted to run the task, but the task did not run due to one of the constraints in the task definition.';
                                  end;
    LongInt($8004D000)          : begin
                                    HrStr := 'XACT_E_ALREADYOTHERSINGLEPHASE';
                                    HrDescr := 'Another single phase resource manager has already been enlisted in this transaction.';
                                  end;
    LongInt($8004D001)          : begin
                                    HrStr := 'XACT_E_CANTRETAIN';
                                    HrDescr := 'A retaining commit or abort is not supported.';
                                  end;
    LongInt($8004D002)          : begin
                                    HrStr := 'XACT_E_COMMITFAILED';
                                    HrDescr := 'The transaction failed to commit for an unknown reason.' +
                                               'The transaction was aborted.';
                                  end;
    LongInt($8004D003)          : begin
                                    HrStr := 'XACT_E_COMMITPREVENTED';
                                    HrDescr := 'Cannot call commit on this transaction object because the calling application did not initiate the transaction.';
                                  end;
    LongInt($8004D004)          : begin
                                    HrStr := 'XACT_E_HEURISTICABORT';
                                    HrDescr := 'Instead of committing, the resource heuristically aborted.';
                                  end;
    LongInt($8004D005)          : begin
                                    HrStr := 'XACT_E_HEURISTICCOMMIT';
                                    HrDescr := 'Instead of aborting, the resource heuristically committed.';
                                  end;
    LongInt($8004D006)          : begin
                                    HrStr := 'XACT_E_HEURISTICDAMAGE';
                                    HrDescr := 'Some of the states of the resource were committed while others were aborted, likely because of heuristic decisions.';
                                  end;
    LongInt($8004D007)          : begin
                                    HrStr := 'XACT_E_HEURISTICDANGER';
                                    HrDescr := 'Some of the states of the resource might have been committed while others were aborted, likely because of heuristic decisions.';
                                  end;
    LongInt($8004D008)          : begin
                                    HrStr := 'XACT_E_ISOLATIONLEVEL';
                                    HrDescr := 'The requested isolation level is not valid or supported.';
                                  end;
    LongInt($8004D009)          : begin
                                    HrStr := 'XACT_E_NOASYNC';
                                    HrDescr := 'The transaction manager does not support an asynchronous operation for this method.';
                                  end;
    LongInt($8004D00A)          : begin
                                    HrStr := 'XACT_E_NOENLIST';
                                    HrDescr := 'Unable to enlist in the transaction.';
                                  end;
    LongInt($8004D00B)          : begin
                                    HrStr := 'XACT_E_NOISORETAIN';
                                    HrDescr := 'The requested semantics of retention of isolation across retaining commit and abort boundaries cannot be supported by this transaction implementation, or isoFlags was not equal to 0.';
                                  end;
    LongInt($8004D00C)          : begin
                                    HrStr := 'XACT_E_NORESOURCE';
                                    HrDescr := 'There is no resource presently associated with this enlistment.';
                                  end;
    LongInt($8004D00D)          : begin
                                    HrStr := 'XACT_E_NOTCURRENT';
                                    HrDescr := 'The transaction failed to commit due to the failure of optimistic concurrency control in at least one of the resource managers.';
                                  end;
    LongInt($8004D00E)          : begin
                                    HrStr := 'XACT_E_NOTRANSACTION';
                                    HrDescr := 'The transaction has already been implicitly or explicitly committed or aborted.';
                                  end;
    LongInt($8004D00F)          : begin
                                    HrStr := 'XACT_E_NOTSUPPORTED';
                                    HrDescr := 'An invalid combination of flags was specified.';
                                  end;
    LongInt($8004D010)          : begin
                                    HrStr := 'XACT_E_UNKNOWNRMGRID';
                                    HrDescr := 'The resource manager ID is not associated with this transaction or the transaction manager.';
                                  end;
    LongInt($8004D011)          : begin
                                    HrStr := 'XACT_E_WRONGSTATE';
                                    HrDescr := 'This method was called in the wrong state.';
                                  end;
    LongInt($8004D012)          : begin
                                    HrStr := 'XACT_E_WRONGUOW';
                                    HrDescr := 'The indicated unit of work does not match the unit of work expected by the resource manager.';
                                  end;
    LongInt($8004D013)          : begin
                                    HrStr := 'XACT_E_XTIONEXISTS';
                                    HrDescr := 'An enlistment in a transaction already exists.';
                                  end;
    LongInt($8004D014)          : begin
                                    HrStr := 'XACT_E_NOIMPORTOBJECT';
                                    HrDescr := 'An import object for the transaction could not be found.';
                                  end;
    LongInt($8004D015)          : begin
                                    HrStr := 'XACT_E_INVALIDCOOKIE';
                                    HrDescr := 'The transaction cookie is invalid.';
                                  end;
    LongInt($8004D016)          : begin
                                    HrStr := 'XACT_E_INDOUBT';
                                    HrDescr := 'The transaction status is in doubt.' +
                                               'A communication failure occurred, or a transaction manager or resource manager has failed.';
                                  end;
    LongInt($8004D017)          : begin
                                    HrStr := 'XACT_E_NOTIMEOUT';
                                    HrDescr := 'A time-out was specified, but time-outs are not supported.';
                                  end;
    LongInt($8004D018)          : begin
                                    HrStr := 'XACT_E_ALREADYINPROGRESS';
                                    HrDescr := 'The requested operation is already in progress for the transaction.';
                                  end;
    LongInt($8004D019)          : begin
                                    HrStr := 'XACT_E_ABORTED';
                                    HrDescr := 'The transaction has already been aborted.';
                                  end;
    LongInt($8004D01A)          : begin
                                    HrStr := 'XACT_E_LOGFULL';
                                    HrDescr := 'The Transaction Manager returned a log full error.';
                                  end;
    LongInt($8004D01B)          : begin
                                    HrStr := 'XACT_E_TMNOTAVAILABLE';
                                    HrDescr := 'The transaction manager is not available.';
                                  end;
    LongInt($8004D01C)          : begin
                                    HrStr := 'XACT_E_CONNECTION_DOWN';
                                    HrDescr := 'A connection with the transaction manager was lost.';
                                  end;
    LongInt($8004D01D)          : begin
                                    HrStr := 'XACT_E_CONNECTION_DENIED';
                                    HrDescr := 'A request to establish a connection with the transaction manager was denied.';
                                  end;
    LongInt($8004D01E)          : begin
                                    HrStr := 'XACT_E_REENLISTTIMEOUT';
                                    HrDescr := 'Resource manager reenlistment to determine transaction status timed out.';
                                  end;
    LongInt($8004D01F)          : begin
                                    HrStr := 'XACT_E_TIP_CONNECT_FAILED';
                                    HrDescr := 'The transaction manager failed to establish a connection with another Transaction Internet Protocol (TIP) transaction manager.';
                                  end;
    LongInt($8004D020)          : begin
                                    HrStr := 'XACT_E_TIP_PROTOCOL_ERROR';
                                    HrDescr := 'The transaction manager encountered a protocol error with another TIP transaction manager.';
                                  end;
    LongInt($8004D021)          : begin
                                    HrStr := 'XACT_E_TIP_PULL_FAILED';
                                    HrDescr := 'The transaction manager could not propagate a transaction from another TIP transaction manager.';
                                  end;
    LongInt($8004D022)          : begin
                                    HrStr := 'XACT_E_DEST_TMNOTAVAILABLE';
                                    HrDescr := 'The transaction manager on the destination machine is not available.';
                                  end;
    LongInt($8004D023)          : begin
                                    HrStr := 'XACT_E_TIP_DISABLED';
                                    HrDescr := 'The transaction manager has disabled its support for TIP.';
                                  end;
    LongInt($8004D024)          : begin
                                    HrStr := 'XACT_E_NETWORK_TX_DISABLED';
                                    HrDescr := 'The transaction manager has disabled its support for remote or network transactions.';
                                  end;
    LongInt($8004D025)          : begin
                                    HrStr := 'XACT_E_PARTNER_NETWORK_TX_DISABLED';
                                    HrDescr := 'The partner transaction manager has disabled its support for remote or network transactions.';
                                  end;
    LongInt($8004D026)          : begin
                                    HrStr := 'XACT_E_XA_TX_DISABLED';
                                    HrDescr := 'The transaction manager has disabled its support for XA transactions.';
                                  end;
    LongInt($8004D027)          : begin
                                    HrStr := 'XACT_E_UNABLE_TO_READ_DTC_CONFIG';
                                    HrDescr := 'Microsoft Distributed Transaction Coordinator (MSDTC) was unable to read its configuration information.';
                                  end;
    LongInt($8004D028)          : begin
                                    HrStr := 'XACT_E_UNABLE_TO_LOAD_DTC_PROXY';
                                    HrDescr := 'MSDTC was unable to load the DTC proxy DLL.';
                                  end;
    LongInt($8004D029)          : begin
                                    HrStr := 'XACT_E_ABORTING';
                                    HrDescr := 'The local transaction has aborted.';
                                  end;
    LongInt($8004D080)          : begin
                                    HrStr := 'XACT_E_CLERKNOTFOUND';
                                    HrDescr := 'The specified CRM clerk was not found.' +
                                               'It might have completed before it could be held.';
                                  end;
    LongInt($8004D081)          : begin
                                    HrStr := 'XACT_E_CLERKEXISTS';
                                    HrDescr := 'The specified CRM clerk does not exist.';
                                  end;
    LongInt($8004D082)          : begin
                                    HrStr := 'XACT_E_RECOVERYINPROGRESS';
                                    HrDescr := 'Recovery of the CRM log file is still in progress.';
                                  end;
    LongInt($8004D083)          : begin
                                    HrStr := 'XACT_E_TRANSACTIONCLOSED';
                                    HrDescr := 'The transaction has completed, and the log records have been discarded from the log file.' +
                                               'They are no longer available.';
                                  end;
    LongInt($8004D084)          : begin
                                    HrStr := 'XACT_E_INVALIDLSN';
                                    HrDescr := 'lsnToRead is outside of the current limits of the log';
                                  end;
    LongInt($8004D085)          : begin
                                    HrStr := 'XACT_E_REPLAYREQUEST';
                                    HrDescr := 'The COM+ Compensating Resource Manager has records it wishes to replay.';
                                  end;
    LongInt($8004D100)          : begin
                                    HrStr := 'XACT_E_CONNECTION_REQUEST_DENIED';
                                    HrDescr := 'The request to connect to the specified transaction coordinator was denied.';
                                  end;
    LongInt($8004D101)          : begin
                                    HrStr := 'XACT_E_TOOMANY_ENLISTMENTS';
                                    HrDescr := 'The maximum number of enlistments for the specified transaction has been reached.';
                                  end;
    LongInt($8004D102)          : begin
                                    HrStr := 'XACT_E_DUPLICATE_GUID';
                                    HrDescr := 'A resource manager with the same identifier is already registered with the specified transaction coordinator.';
                                  end;
    LongInt($8004D103)          : begin
                                    HrStr := 'XACT_E_NOTSINGLEPHASE';
                                    HrDescr := 'The prepare request given was not eligible for single-phase optimizations.';
                                  end;
    LongInt($8004D104)          : begin
                                    HrStr := 'XACT_E_RECOVERYALREADYDONE';
                                    HrDescr := 'RecoveryComplete has already been called for the given resource manager.';
                                  end;
    LongInt($8004D105)          : begin
                                    HrStr := 'XACT_E_PROTOCOL';
                                    HrDescr := 'The interface call made was incorrect for the current state of the protocol.';
                                  end;
    LongInt($8004D106)          : begin
                                    HrStr := 'XACT_E_RM_FAILURE';
                                    HrDescr := 'The xa_open call failed for the XA resource.';
                                  end;
    LongInt($8004D107)          : begin
                                    HrStr := 'XACT_E_RECOVERY_FAILED';
                                    HrDescr := 'The xa_recover call failed for the XA resource.';
                                  end;
    LongInt($8004D108)          : begin
                                    HrStr := 'XACT_E_LU_NOT_FOUND';
                                    HrDescr := 'The logical unit of work specified cannot be found.';
                                  end;
    LongInt($8004D109)          : begin
                                    HrStr := 'XACT_E_DUPLICATE_LU';
                                    HrDescr := 'The specified logical unit of work already exists.';
                                  end;
    LongInt($8004D10A)          : begin
                                    HrStr := 'XACT_E_LU_NOT_CONNECTED';
                                    HrDescr := 'Subordinate creation failed.' +
                                               'The specified logical unit of work was not connected.';
                                  end;
    LongInt($8004D10B)          : begin
                                    HrStr := 'XACT_E_DUPLICATE_TRANSID';
                                    HrDescr := 'A transaction with the given identifier already exists.';
                                  end;
    LongInt($8004D10C)          : begin
                                    HrStr := 'XACT_E_LU_BUSY';
                                    HrDescr := 'The resource is in use.';
                                  end;
    LongInt($8004D10D)          : begin
                                    HrStr := 'XACT_E_LU_NO_RECOVERY_PROCESS';
                                    HrDescr := 'The LU Recovery process is down.';
                                  end;
    LongInt($8004D10E)          : begin
                                    HrStr := 'XACT_E_LU_DOWN';
                                    HrDescr := 'The remote session was lost.';
                                  end;
    LongInt($8004D10F)          : begin
                                    HrStr := 'XACT_E_LU_RECOVERING';
                                    HrDescr := 'The resource is currently recovering.';
                                  end;
    LongInt($8004D110)          : begin
                                    HrStr := 'XACT_E_LU_RECOVERY_MISMATCH';
                                    HrDescr := 'There was a mismatch in driving recovery.';
                                  end;
    LongInt($8004D111)          : begin
                                    HrStr := 'XACT_E_RM_UNAVAILABLE';
                                    HrDescr := 'An error occurred with the XA resource.';
                                  end;
    LongInt($8004E002)          : begin
                                    HrStr := 'CONTEXT_E_ABORTED';
                                    HrDescr := 'The root transaction wanted to commit, but the transaction aborted.';
                                  end;
    LongInt($8004E003)          : begin
                                    HrStr := 'CONTEXT_E_ABORTING';
                                    HrDescr := 'The COM+ component on which the method call was made has a transaction that has already aborted or is in the process of aborting.';
                                  end;
    LongInt($8004E004)          : begin
                                    HrStr := 'CONTEXT_E_NOCONTEXT';
                                    HrDescr := 'There is no Microsoft Transaction Server (MTS) object context.';
                                  end;
    LongInt($8004E005)          : begin
                                    HrStr := 'CONTEXT_E_WOULD_DEADLOCK';
                                    HrDescr := 'The component is configured to use synchronization, and this method call would cause a deadlock to occur.';
                                  end;
    LongInt($8004E006)          : begin
                                    HrStr := 'CONTEXT_E_SYNCH_TIMEOUT';
                                    HrDescr := 'The component is configured to use synchronization, and a thread has timed out waiting to enter the context.';
                                  end;
    LongInt($8004E007)          : begin
                                    HrStr := 'CONTEXT_E_OLDREF';
                                    HrDescr := 'You made a method call on a COM+ component that has a transaction that has already committed or aborted.';
                                  end;
    LongInt($8004E00C)          : begin
                                    HrStr := 'CONTEXT_E_ROLENOTFOUND';
                                    HrDescr := 'The specified role was not configured for the application.';
                                  end;
    LongInt($8004E00F)          : begin
                                    HrStr := 'CONTEXT_E_TMNOTAVAILABLE';
                                    HrDescr := 'COM+ was unable to talk to the MSDTC.';
                                  end;
    LongInt($8004E021)          : begin
                                    HrStr := 'CO_E_ACTIVATIONFAILED';
                                    HrDescr := 'An unexpected error occurred during COM+ activation.';
                                  end;
    LongInt($8004E022)          : begin
                                    HrStr := 'CO_E_ACTIVATIONFAILED_EVENTLOGGED';
                                    HrDescr := 'COM+ activation failed.' +
                                               'Check the event log for more information.';
                                  end;
    LongInt($8004E023)          : begin
                                    HrStr := 'CO_E_ACTIVATIONFAILED_CATALOGERROR';
                                    HrDescr := 'COM+ activation failed due to a catalog or configuration error.';
                                  end;
    LongInt($8004E024)          : begin
                                    HrStr := 'CO_E_ACTIVATIONFAILED_TIMEOUT';
                                    HrDescr := 'COM+ activation failed because the activation could not be completed in the specified amount of time.';
                                  end;
    LongInt($8004E025)          : begin
                                    HrStr := 'CO_E_INITIALIZATIONFAILED';
                                    HrDescr := 'COM+ activation failed because an initialization function failed.' +
                                               'Check the event log for more information.';
                                  end;
    LongInt($8004E026)          : begin
                                    HrStr := 'CONTEXT_E_NOJIT';
                                    HrDescr := 'The requested operation requires that just-in-time (JIT) be in the current context, and it is not.';
                                  end;
    LongInt($8004E027)          : begin
                                    HrStr := 'CONTEXT_E_NOTRANSACTION';
                                    HrDescr := 'The requested operation requires that the current context have a transaction, and it does not.';
                                  end;
    LongInt($8004E028)          : begin
                                    HrStr := 'CO_E_THREADINGMODEL_CHANGED';
                                    HrDescr := 'The components threading model has changed after install into a COM+ application.' +
                                               'Re-install component.';
                                  end;
    LongInt($8004E029)          : begin
                                    HrStr := 'CO_E_NOIISINTRINSICS';
                                    HrDescr := 'Internet Information Services (IIS) intrinsics not available.' +
                                               'Start your work with IIS.';
                                  end;
    LongInt($8004E02A)          : begin
                                    HrStr := 'CO_E_NOCOOKIES';
                                    HrDescr := 'An attempt to write a cookie failed.';
                                  end;
    LongInt($8004E02B)          : begin
                                    HrStr := 'CO_E_DBERROR';
                                    HrDescr := 'An attempt to use a database generated a database-specific error.';
                                  end;
    LongInt($8004E02C)          : begin
                                    HrStr := 'CO_E_NOTPOOLED';
                                    HrDescr := 'The COM+ component you created must use object pooling to work.';
                                  end;
    LongInt($8004E02D)          : begin
                                    HrStr := 'CO_E_NOTCONSTRUCTED';
                                    HrDescr := 'The COM+ component you created must use object construction to work correctly.';
                                  end;
    LongInt($8004E02E)          : begin
                                    HrStr := 'CO_E_NOSYNCHRONIZATION';
                                    HrDescr := 'The COM+ component requires synchronization, and it is not configured for it.';
                                  end;
    LongInt($8004E02F)          : begin
                                    HrStr := 'CO_E_ISOLEVELMISMATCH';
                                    HrDescr := 'The TxIsolation Level property for the COM+ component being created is stronger than the TxIsolationLevel for the root.';
                                  end;
    LongInt($8004E030)          : begin
                                    HrStr := 'CO_E_CALL_OUT_OF_TX_SCOPE_NOT_ALLOWED';
                                    HrDescr := 'The component attempted to make a cross-context call between invocations of EnterTransactionScope and ExitTransactionScope.' +
                                               'This is not allowed.' +
                                               'Cross-context calls cannot be made while inside a transaction scope.';
                                  end;
    LongInt($8004E031)          : begin
                                    HrStr := 'CO_E_EXIT_TRANSACTION_SCOPE_NOT_CALLED';
                                    HrDescr := 'The component made a call to EnterTransactionScope, but did not make a corresponding call to ExitTransactionScope before returning.';
                                  end;
    LongInt($80070005)          : begin
                                    HrStr := 'E_ACCESSDENIED';
                                    HrDescr := 'General access denied error.';
                                  end;
    LongInt($8007000E)          : begin
                                    HrStr := 'E_OUTOFMEMORY';
                                    HrDescr := 'The server does not have enough memory for the new channel.';
                                  end;
    LongInt($80070032)          : begin
                                    HrStr := 'ERROR_NOT_SUPPORTED';
                                    HrDescr := 'The server cannot support a client request for a dynamic virtual channel.';
                                  end;
    LongInt($80070057)          : begin
                                    HrStr := 'E_INVALIDARG';
                                    HrDescr := 'One or more arguments are invalid.';
                                  end;
    LongInt($80070070)          : begin
                                    HrStr := 'ERROR_DISK_FULL';
                                    HrDescr := 'There is not enough space on the disk.';
                                  end;
    LongInt($80080001)          : begin
                                    HrStr := 'CO_E_CLASS_CREATE_FAILED';
                                    HrDescr := 'Attempt to create a class object failed.';
                                  end;
    LongInt($80080002)          : begin
                                    HrStr := 'CO_E_SCM_ERROR';
                                    HrDescr := 'OLE service could not bind object.';
                                  end;
    LongInt($80080003)          : begin
                                    HrStr := 'CO_E_SCM_RPC_FAILURE';
                                    HrDescr := 'RPC communication failed with OLE service.';
                                  end;
    LongInt($80080004)          : begin
                                    HrStr := 'CO_E_BAD_PATH';
                                    HrDescr := 'Bad path to object.';
                                  end;
    LongInt($80080005)          : begin
                                    HrStr := 'CO_E_SERVER_EXEC_FAILURE';
                                    HrDescr := 'Server execution failed.';
                                  end;
    LongInt($80080006)          : begin
                                    HrStr := 'CO_E_OBJSRV_RPC_FAILURE';
                                    HrDescr := 'OLE service could not communicate with the object server.';
                                  end;
    LongInt($80080007)          : begin
                                    HrStr := 'MK_E_NO_NORMALIZED';
                                    HrDescr := 'Moniker path could not be normalized.';
                                  end;
    LongInt($80080008)          : begin
                                    HrStr := 'CO_E_SERVER_STOPPING';
                                    HrDescr := 'Object server is stopping when OLE service contacts it.';
                                  end;
    LongInt($80080009)          : begin
                                    HrStr := 'MEM_E_INVALID_ROOT';
                                    HrDescr := 'An invalid root block pointer was specified.';
                                  end;
    LongInt($80080010)          : begin
                                    HrStr := 'MEM_E_INVALID_LINK';
                                    HrDescr := 'An allocation chain contained an invalid link pointer.';
                                  end;
    LongInt($80080011)          : begin
                                    HrStr := 'MEM_E_INVALID_SIZE';
                                    HrDescr := 'The requested allocation size was too large.';
                                  end;
    LongInt($80080015)          : begin
                                    HrStr := 'CO_E_MISSING_DISPLAYNAME';
                                    HrDescr := 'The activation requires a display name to be present under the class identifier (CLSID) key.';
                                  end;
    LongInt($80080016)          : begin
                                    HrStr := 'CO_E_RUNAS_VALUE_MUST_BE_AAA';
                                    HrDescr := 'The activation requires that the RunAs value for the application is Activate As Activator.';
                                  end;
    LongInt($80080017)          : begin
                                    HrStr := 'CO_E_ELEVATION_DISABLED';
                                    HrDescr := 'The class is not configured to support elevated activation.';
                                  end;
    LongInt($80090001)          : begin
                                    HrStr := 'NTE_BAD_UID';
                                    HrDescr := 'Bad UID.';
                                  end;
    LongInt($80090002)          : begin
                                    HrStr := 'NTE_BAD_HASH';
                                    HrDescr := 'Bad hash.';
                                  end;
    LongInt($80090003)          : begin
                                    HrStr := 'NTE_BAD_KEY';
                                    HrDescr := 'Bad key.';
                                  end;
    LongInt($80090004)          : begin
                                    HrStr := 'NTE_BAD_LEN';
                                    HrDescr := 'Bad length.';
                                  end;
    LongInt($80090005)          : begin
                                    HrStr := 'NTE_BAD_DATA';
                                    HrDescr := 'Bad data.';
                                  end;
    LongInt($80090006)          : begin
                                    HrStr := 'NTE_BAD_SIGNATURE';
                                    HrDescr := 'Invalid signature.';
                                  end;
    LongInt($80090007)          : begin
                                    HrStr := 'NTE_BAD_VER';
                                    HrDescr := 'Bad version of provider.';
                                  end;
    LongInt($80090008)          : begin
                                    HrStr := 'NTE_BAD_ALGID';
                                    HrDescr := 'Invalid algorithm specified.';
                                  end;
    LongInt($80090009)          : begin
                                    HrStr := 'NTE_BAD_FLAGS';
                                    HrDescr := 'Invalid flags specified.';
                                  end;
    LongInt($8009000A)          : begin
                                    HrStr := 'NTE_BAD_TYPE';
                                    HrDescr := 'Invalid type specified.';
                                  end;
    LongInt($8009000B)          : begin
                                    HrStr := 'NTE_BAD_KEY_STATE';
                                    HrDescr := 'Key not valid for use in specified state.';
                                  end;
    LongInt($8009000C)          : begin
                                    HrStr := 'NTE_BAD_HASH_STATE';
                                    HrDescr := 'Hash not valid for use in specified state.';
                                  end;
    LongInt($8009000D)          : begin
                                    HrStr := 'NTE_NO_KEY';
                                    HrDescr := 'Key does not exist.';
                                  end;
    LongInt($8009000E)          : begin
                                    HrStr := 'NTE_NO_MEMORY';
                                    HrDescr := 'Insufficient memory available for the operation.';
                                  end;
    LongInt($8009000F)          : begin
                                    HrStr := 'NTE_EXISTS';
                                    HrDescr := 'Object already exists.';
                                  end;
    LongInt($80090010)          : begin
                                    HrStr := 'NTE_PERM';
                                    HrDescr := 'Access denied.';
                                  end;
    LongInt($80090011)          : begin
                                    HrStr := 'NTE_NOT_FOUND';
                                    HrDescr := 'Object was not found.';
                                  end;
    LongInt($80090012)          : begin
                                    HrStr := 'NTE_DOUBLE_ENCRYPT';
                                    HrDescr := 'Data already encrypted.';
                                  end;
    LongInt($80090013)          : begin
                                    HrStr := 'NTE_BAD_PROVIDER';
                                    HrDescr := 'Invalid provider specified.';
                                  end;
    LongInt($80090014)          : begin
                                    HrStr := 'NTE_BAD_PROV_TYPE';
                                    HrDescr := 'Invalid provider type specified.';
                                  end;
    LongInt($80090015)          : begin
                                    HrStr := 'NTE_BAD_PUBLIC_KEY';
                                    HrDescr := 'Provider''s public key is invalid.';
                                  end;
    LongInt($80090016)          : begin
                                    HrStr := 'NTE_BAD_KEYSET';
                                    HrDescr := 'Key set does not exist.';
                                  end;
    LongInt($80090017)          : begin
                                    HrStr := 'NTE_PROV_TYPE_NOT_DEF';
                                    HrDescr := 'Provider type not defined.';
                                  end;
    LongInt($80090018)          : begin
                                    HrStr := 'NTE_PROV_TYPE_ENTRY_BAD';
                                    HrDescr := 'The provider type, as registered, is invalid.';
                                  end;
    LongInt($80090019)          : begin
                                    HrStr := 'NTE_KEYSET_NOT_DEF';
                                    HrDescr := 'The key set is not defined.';
                                  end;
    LongInt($8009001A)          : begin
                                    HrStr := 'NTE_KEYSET_ENTRY_BAD';
                                    HrDescr := 'The key set, as registered, is invalid.';
                                  end;
    LongInt($8009001B)          : begin
                                    HrStr := 'NTE_PROV_TYPE_NO_MATCH';
                                    HrDescr := 'Provider type does not match registered value.';
                                  end;
    LongInt($8009001C)          : begin
                                    HrStr := 'NTE_SIGNATURE_FILE_BAD';
                                    HrDescr := 'The digital signature file is corrupt.';
                                  end;
    LongInt($8009001D)          : begin
                                    HrStr := 'NTE_PROVIDER_DLL_FAIL';
                                    HrDescr := 'Provider DLL failed to initialize correctly.';
                                  end;
    LongInt($8009001E)          : begin
                                    HrStr := 'NTE_PROV_DLL_NOT_FOUND';
                                    HrDescr := 'Provider DLL could not be found.';
                                  end;
    LongInt($8009001F)          : begin
                                    HrStr := 'NTE_BAD_KEYSET_PARAM';
                                    HrDescr := 'The keyset parameter is invalid.';
                                  end;
    LongInt($80090020)          : begin
                                    HrStr := 'NTE_FAIL';
                                    HrDescr := 'An internal error occurred.';
                                  end;
    LongInt($80090021)          : begin
                                    HrStr := 'NTE_SYS_ERR';
                                    HrDescr := 'A base error occurred.';
                                  end;
    LongInt($80090022)          : begin
                                    HrStr := 'NTE_SILENT_CONTEXT';
                                    HrDescr := 'Provider could not perform the action because the context was acquired as silent.';
                                  end;
    LongInt($80090023)          : begin
                                    HrStr := 'NTE_TOKEN_KEYSET_STORAGE_FULL';
                                    HrDescr := 'The security token does not have storage space available for an additional container.';
                                  end;
    LongInt($80090024)          : begin
                                    HrStr := 'NTE_TEMPORARY_PROFILE';
                                    HrDescr := 'The profile for the user is a temporary profile.';
                                  end;
    LongInt($80090025)          : begin
                                    HrStr := 'NTE_FIXEDPARAMETER';
                                    HrDescr := 'The key parameters could not be set because the configuration service provider (CSP) uses fixed parameters.';
                                  end;
    LongInt($80090026)          : begin
                                    HrStr := 'NTE_INVALID_HANDLE';
                                    HrDescr := 'The supplied handle is invalid.';
                                  end;
    LongInt($80090027)          : begin
                                    HrStr := 'NTE_INVALID_PARAMETER';
                                    HrDescr := 'The parameter is incorrect.';
                                  end;
    LongInt($80090028)          : begin
                                    HrStr := 'NTE_BUFFER_TOO_SMALL';
                                    HrDescr := 'The buffer supplied to a function was too small.';
                                  end;
    LongInt($80090029)          : begin
                                    HrStr := 'NTE_NOT_SUPPORTED';
                                    HrDescr := 'The requested operation is not supported.';
                                  end;
    LongInt($8009002A)          : begin
                                    HrStr := 'NTE_NO_MORE_ITEMS';
                                    HrDescr := 'No more data is available.';
                                  end;
    LongInt($8009002B)          : begin
                                    HrStr := 'NTE_BUFFERS_OVERLAP';
                                    HrDescr := 'The supplied buffers overlap incorrectly.';
                                  end;
    LongInt($8009002C)          : begin
                                    HrStr := 'NTE_DECRYPTION_FAILURE';
                                    HrDescr := 'The specified data could not be decrypted.';
                                  end;
    LongInt($8009002D)          : begin
                                    HrStr := 'NTE_INTERNAL_ERROR';
                                    HrDescr := 'An internal consistency check failed.';
                                  end;
    LongInt($8009002E)          : begin
                                    HrStr := 'NTE_UI_REQUIRED';
                                    HrDescr := 'This operation requires input from the user.';
                                  end;
    LongInt($8009002F)          : begin
                                    HrStr := 'NTE_HMAC_NOT_SUPPORTED';
                                    HrDescr := 'The cryptographic provider does not support Hash Message Authentication Code (HMAC).';
                                  end;
    LongInt($80090300)          : begin
                                    HrStr := 'SEC_E_INSUFFICIENT_MEMORY';
                                    HrDescr := 'Not enough memory is available to complete this request.';
                                  end;
    LongInt($80090301)          : begin
                                    HrStr := 'SEC_E_INVALID_HANDLE';
                                    HrDescr := 'The handle specified is invalid.';
                                  end;
    LongInt($80090302)          : begin
                                    HrStr := 'SEC_E_UNSUPPORTED_FUNCTION';
                                    HrDescr := 'The function requested is not supported.';
                                  end;
    LongInt($80090303)          : begin
                                    HrStr := 'SEC_E_TARGET_UNKNOWN';
                                    HrDescr := 'The specified target is unknown or unreachable.';
                                  end;
    LongInt($80090304)          : begin
                                    HrStr := 'SEC_E_INTERNAL_ERROR';
                                    HrDescr := 'The Local Security Authority (LSA) cannot be contacted.';
                                  end;
    LongInt($80090305)          : begin
                                    HrStr := 'SEC_E_SECPKG_NOT_FOUND';
                                    HrDescr := 'The requested security package does not exist.';
                                  end;
    LongInt($80090306)          : begin
                                    HrStr := 'SEC_E_NOT_OWNER';
                                    HrDescr := 'The caller is not the owner of the desired credentials.';
                                  end;
    LongInt($80090307)          : begin
                                    HrStr := 'SEC_E_CANNOT_INSTALL';
                                    HrDescr := 'The security package failed to initialize and cannot be installed.';
                                  end;
    LongInt($80090308)          : begin
                                    HrStr := 'SEC_E_INVALID_TOKEN';
                                    HrDescr := 'The token supplied to the function is invalid.';
                                  end;
    LongInt($80090309)          : begin
                                    HrStr := 'SEC_E_CANNOT_PACK';
                                    HrDescr := 'The security package is not able to marshal the logon buffer, so the logon attempt has failed.';
                                  end;
    LongInt($8009030A)          : begin
                                    HrStr := 'SEC_E_QOP_NOT_SUPPORTED';
                                    HrDescr := 'The per-message quality of protection is not supported by the security package.';
                                  end;
    LongInt($8009030B)          : begin
                                    HrStr := 'SEC_E_NO_IMPERSONATION';
                                    HrDescr := 'The security context does not allow impersonation of the client.';
                                  end;
    LongInt($8009030C)          : begin
                                    HrStr := 'SEC_E_LOGON_DENIED';
                                    HrDescr := 'The logon attempt failed.';
                                  end;
    LongInt($8009030D)          : begin
                                    HrStr := 'SEC_E_UNKNOWN_CREDENTIALS';
                                    HrDescr := 'The credentials supplied to the package were not recognized.';
                                  end;
    LongInt($8009030E)          : begin
                                    HrStr := 'SEC_E_NO_CREDENTIALS';
                                    HrDescr := 'No credentials are available in the security package.';
                                  end;
    LongInt($8009030F)          : begin
                                    HrStr := 'SEC_E_MESSAGE_ALTERED';
                                    HrDescr := 'The message or signature supplied for verification has been altered.';
                                  end;
    LongInt($80090310)          : begin
                                    HrStr := 'SEC_E_OUT_OF_SEQUENCE';
                                    HrDescr := 'The message supplied for verification is out of sequence.';
                                  end;
    LongInt($80090311)          : begin
                                    HrStr := 'SEC_E_NO_AUTHENTICATING_AUTHORITY';
                                    HrDescr := 'No authority could be contacted for authentication.';
                                  end;
    LongInt($80090316)          : begin
                                    HrStr := 'SEC_E_BAD_PKGID';
                                    HrDescr := 'The requested security package does not exist.';
                                  end;
    LongInt($80090317)          : begin
                                    HrStr := 'SEC_E_CONTEXT_EXPIRED';
                                    HrDescr := 'The context has expired and can no longer be used.';
                                  end;
    LongInt($80090318)          : begin
                                    HrStr := 'SEC_E_INCOMPLETE_MESSAGE';
                                    HrDescr := 'The supplied message is incomplete.' +
                                               'The signature was not verified.';
                                  end;
    LongInt($80090320)          : begin
                                    HrStr := 'SEC_E_INCOMPLETE_CREDENTIALS';
                                    HrDescr := 'The credentials supplied were not complete and could not be verified.' +
                                               'The context could not be initialized.';
                                  end;
    LongInt($80090321)          : begin
                                    HrStr := 'SEC_E_BUFFER_TOO_SMALL';
                                    HrDescr := 'The buffers supplied to a function was too small.';
                                  end;
    LongInt($80090322)          : begin
                                    HrStr := 'SEC_E_WRONG_PRINCIPAL';
                                    HrDescr := 'The target principal name is incorrect.';
                                  end;
    LongInt($80090324)          : begin
                                    HrStr := 'SEC_E_TIME_SKEW';
                                    HrDescr := 'The clocks on the client and server machines are skewed.';
                                  end;
    LongInt($80090325)          : begin
                                    HrStr := 'SEC_E_UNTRUSTED_ROOT';
                                    HrDescr := 'The certificate chain was issued by an authority that is not trusted.';
                                  end;
    LongInt($80090326)          : begin
                                    HrStr := 'SEC_E_ILLEGAL_MESSAGE';
                                    HrDescr := 'The message received was unexpected or badly formatted.';
                                  end;
    LongInt($80090327)          : begin
                                    HrStr := 'SEC_E_CERT_UNKNOWN';
                                    HrDescr := 'An unknown error occurred while processing the certificate.';
                                  end;
    LongInt($80090328)          : begin
                                    HrStr := 'SEC_E_CERT_EXPIRED';
                                    HrDescr := 'The received certificate has expired.';
                                  end;
    LongInt($80090329)          : begin
                                    HrStr := 'SEC_E_ENCRYPT_FAILURE';
                                    HrDescr := 'The specified data could not be encrypted.';
                                  end;
    LongInt($80090330)          : begin
                                    HrStr := 'SEC_E_DECRYPT_FAILURE';
                                    HrDescr := 'The specified data could not be decrypted.';
                                  end;
    LongInt($80090331)          : begin
                                    HrStr := 'SEC_E_ALGORITHM_MISMATCH';
                                    HrDescr := 'The client and server cannot communicate because they do not possess a common algorithm.';
                                  end;
    LongInt($80090332)          : begin
                                    HrStr := 'SEC_E_SECURITY_QOS_FAILED';
                                    HrDescr := 'The security context could not be established due to a failure in the requested quality of service (for example, mutual authentication or delegation).';
                                  end;
    LongInt($80090333)          : begin
                                    HrStr := 'SEC_E_UNFINISHED_CONTEXT_DELETED';
                                    HrDescr := 'A security context was deleted before the context was completed.' +
                                               'This is considered a logon failure.';
                                  end;
    LongInt($80090334)          : begin
                                    HrStr := 'SEC_E_NO_TGT_REPLY';
                                    HrDescr := 'The client is trying to negotiate a context and the server requires user-to-user but did not send a ticket granting ticket (TGT) reply.';
                                  end;
    LongInt($80090335)          : begin
                                    HrStr := 'SEC_E_NO_IP_ADDRESSES';
                                    HrDescr := 'Unable to accomplish the requested task because the local machine does not have an IP addresses.';
                                  end;
    LongInt($80090336)          : begin
                                    HrStr := 'SEC_E_WRONG_CREDENTIAL_HANDLE';
                                    HrDescr := 'The supplied credential handle does not match the credential associated with the security context.';
                                  end;
    LongInt($80090337)          : begin
                                    HrStr := 'SEC_E_CRYPTO_SYSTEM_INVALID';
                                    HrDescr := 'The cryptographic system or checksum function is invalid because a required function is unavailable.';
                                  end;
    LongInt($80090338)          : begin
                                    HrStr := 'SEC_E_MAX_REFERRALS_EXCEEDED';
                                    HrDescr := 'The number of maximum ticket referrals has been exceeded.';
                                  end;
    LongInt($80090339)          : begin
                                    HrStr := 'SEC_E_MUST_BE_KDC';
                                    HrDescr := 'The local machine must be a Kerberos domain controller (KDC), and it is not.';
                                  end;
    LongInt($8009033A)          : begin
                                    HrStr := 'SEC_E_STRONG_CRYPTO_NOT_SUPPORTED';
                                    HrDescr := 'The other end of the security negotiation requires strong cryptographics, but it is not supported on the local machine.';
                                  end;
    LongInt($8009033B)          : begin
                                    HrStr := 'SEC_E_TOO_MANY_PRINCIPALS';
                                    HrDescr := 'The KDC reply contained more than one principal name.';
                                  end;
    LongInt($8009033C)          : begin
                                    HrStr := 'SEC_E_NO_PA_DATA';
                                    HrDescr := 'Expected to find PA data for a hint of what etype to use, but it was not found.';
                                  end;
    LongInt($8009033D)          : begin
                                    HrStr := 'SEC_E_PKINIT_NAME_MISMATCH';
                                    HrDescr := 'The client certificate does not contain a valid user principal name (UPN), or does not match the client name in the logon request.' +
                                               'Contact your administrator.';
                                  end;
    LongInt($8009033E)          : begin
                                    HrStr := 'SEC_E_SMARTCARD_LOGON_REQUIRED';
                                    HrDescr := 'Smart card logon is required and was not used.';
                                  end;
    LongInt($8009033F)          : begin
                                    HrStr := 'SEC_E_SHUTDOWN_IN_PROGRESS';
                                    HrDescr := 'A system shutdown is in progress.';
                                  end;
    LongInt($80090340)          : begin
                                    HrStr := 'SEC_E_KDC_INVALID_REQUEST';
                                    HrDescr := 'An invalid request was sent to the KDC.';
                                  end;
    LongInt($80090341)          : begin
                                    HrStr := 'SEC_E_KDC_UNABLE_TO_REFER';
                                    HrDescr := 'The KDC was unable to generate a referral for the service requested.';
                                  end;
    LongInt($80090342)          : begin
                                    HrStr := 'SEC_E_KDC_UNKNOWN_ETYPE';
                                    HrDescr := 'The encryption type requested is not supported by the KDC.';
                                  end;
    LongInt($80090343)          : begin
                                    HrStr := 'SEC_E_UNSUPPORTED_PREAUTH';
                                    HrDescr := 'An unsupported pre-authentication mechanism was presented to the Kerberos package.';
                                  end;
    LongInt($80090345)          : begin
                                    HrStr := 'SEC_E_DELEGATION_REQUIRED';
                                    HrDescr := 'The requested operation cannot be completed.' +
                                               'The computer must be trusted for delegation, and the current user account must be configured to allow delegation.';
                                  end;
    LongInt($80090346)          : begin
                                    HrStr := 'SEC_E_BAD_BINDINGS';
                                    HrDescr := 'Client''s supplied Security Support Provider Interface (SSPI) channel bindings were incorrect.';
                                  end;
    LongInt($80090347)          : begin
                                    HrStr := 'SEC_E_MULTIPLE_ACCOUNTS';
                                    HrDescr := 'The received certificate was mapped to multiple accounts.';
                                  end;
    LongInt($80090348)          : begin
                                    HrStr := 'SEC_E_NO_KERB_KEY';
                                    HrDescr := 'No Kerberos key was found.';
                                  end;
    LongInt($80090349)          : begin
                                    HrStr := 'SEC_E_CERT_WRONG_USAGE';
                                    HrDescr := 'The certificate is not valid for the requested usage.';
                                  end;
    LongInt($80090350)          : begin
                                    HrStr := 'SEC_E_DOWNGRADE_DETECTED';
                                    HrDescr := 'The system detected a possible attempt to compromise security.' +
                                               'Ensure that you can contact the server that authenticated you.';
                                  end;
    LongInt($80090351)          : begin
                                    HrStr := 'SEC_E_SMARTCARD_CERT_REVOKED';
                                    HrDescr := 'The smart card certificate used for authentication has been revoked.' +
                                               'Contact your system administrator.' +
                                               'The event log might contain additional information.';
                                  end;
    LongInt($80090352)          : begin
                                    HrStr := 'SEC_E_ISSUING_CA_UNTRUSTED';
                                    HrDescr := 'An untrusted certification authority (CA) was detected while processing the smart card certificate used for authentication.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($80090353)          : begin
                                    HrStr := 'SEC_E_REVOCATION_OFFLINE_C';
                                    HrDescr := 'The revocation status of the smart card certificate used for authentication could not be determined.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($80090354)          : begin
                                    HrStr := 'SEC_E_PKINIT_CLIENT_FAILURE';
                                    HrDescr := 'The smart card certificate used for authentication was not trusted.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($80090355)          : begin
                                    HrStr := 'SEC_E_SMARTCARD_CERT_EXPIRED';
                                    HrDescr := 'The smart card certificate used for authentication has expired.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($80090356)          : begin
                                    HrStr := 'SEC_E_NO_S4U_PROT_SUPPORT';
                                    HrDescr := 'The Kerberos subsystem encountered an error.' +
                                               'A service for user protocol requests was made against a domain controller that does not support services for users.';
                                  end;
    LongInt($80090357)          : begin
                                    HrStr := 'SEC_E_CROSSREALM_DELEGATION_FAILURE';
                                    HrDescr := 'An attempt was made by this server to make a Kerberos-constrained delegation request for a target outside the server''s realm.' +
                                               'This is not supported and indicates a misconfiguration on this server''s allowed-to-delegate-to list.' +
                                               'Contact your administrator.';
                                  end;
    LongInt($80090358)          : begin
                                    HrStr := 'SEC_E_REVOCATION_OFFLINE_KDC';
                                    HrDescr := 'The revocation status of the domain controller certificate used for smart card authentication could not be determined.' +
                                               'The system event log contains additional information.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($80090359)          : begin
                                    HrStr := 'SEC_E_ISSUING_CA_UNTRUSTED_KDC';
                                    HrDescr := 'An untrusted CA was detected while processing the domain controller certificate used for authentication.' +
                                               'The system event log contains additional information.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($8009035A)          : begin
                                    HrStr := 'SEC_E_KDC_CERT_EXPIRED';
                                    HrDescr := 'The domain controller certificate used for smart card logon has expired.' +
                                               'Contact your system administrator with the contents of your system event log.';
                                  end;
    LongInt($8009035B)          : begin
                                    HrStr := 'SEC_E_KDC_CERT_REVOKED';
                                    HrDescr := 'The domain controller certificate used for smart card logon has been revoked.' +
                                               'Contact your system administrator with the contents of your system event log.';
                                  end;
    LongInt($8009035D)          : begin
                                    HrStr := 'SEC_E_INVALID_PARAMETER';
                                    HrDescr := 'One or more of the parameters passed to the function were invalid.';
                                  end;
    LongInt($8009035E)          : begin
                                    HrStr := 'SEC_E_DELEGATION_POLICY';
                                    HrDescr := 'The client policy does not allow credential delegation to the target server.';
                                  end;
    LongInt($8009035F)          : begin
                                    HrStr := 'SEC_E_POLICY_NLTM_ONLY';
                                    HrDescr := 'The client policy does not allow credential delegation to the target server with NLTM only authentication.';
                                  end;
    LongInt($80091001)          : begin
                                    HrStr := 'CRYPT_E_MSG_ERROR';
                                    HrDescr := 'An error occurred while performing an operation on a cryptographic message.';
                                  end;
    LongInt($80091002)          : begin
                                    HrStr := 'CRYPT_E_UNKNOWN_ALGO';
                                    HrDescr := 'Unknown cryptographic algorithm.';
                                  end;
    LongInt($80091003)          : begin
                                    HrStr := 'CRYPT_E_OID_FORMAT';
                                    HrDescr := 'The object identifier is poorly formatted.';
                                  end;
    LongInt($80091004)          : begin
                                    HrStr := 'CRYPT_E_INVALID_MSG_TYPE';
                                    HrDescr := 'Invalid cryptographic message type.';
                                  end;
    LongInt($80091005)          : begin
                                    HrStr := 'CRYPT_E_UNEXPECTED_ENCODING';
                                    HrDescr := 'Unexpected cryptographic message encoding.';
                                  end;
    LongInt($80091006)          : begin
                                    HrStr := 'CRYPT_E_AUTH_ATTR_MISSING';
                                    HrDescr := 'The cryptographic message does not contain an expected authenticated attribute.';
                                  end;
    LongInt($80091007)          : begin
                                    HrStr := 'CRYPT_E_HASH_VALUE';
                                    HrDescr := 'The hash value is not correct.';
                                  end;
    LongInt($80091008)          : begin
                                    HrStr := 'CRYPT_E_INVALID_INDEX';
                                    HrDescr := 'The index value is not valid.';
                                  end;
    LongInt($80091009)          : begin
                                    HrStr := 'CRYPT_E_ALREADY_DECRYPTED';
                                    HrDescr := 'The content of the cryptographic message has already been decrypted.';
                                  end;
    LongInt($8009100A)          : begin
                                    HrStr := 'CRYPT_E_NOT_DECRYPTED';
                                    HrDescr := 'The content of the cryptographic message has not been decrypted yet.';
                                  end;
    LongInt($8009100B)          : begin
                                    HrStr := 'CRYPT_E_RECIPIENT_NOT_FOUND';
                                    HrDescr := 'The enveloped-data message does not contain the specified recipient.';
                                  end;
    LongInt($8009100C)          : begin
                                    HrStr := 'CRYPT_E_CONTROL_TYPE';
                                    HrDescr := 'Invalid control type.';
                                  end;
    LongInt($8009100D)          : begin
                                    HrStr := 'CRYPT_E_ISSUER_SERIALNUMBER';
                                    HrDescr := 'Invalid issuer or serial number.';
                                  end;
    LongInt($8009100E)          : begin
                                    HrStr := 'CRYPT_E_SIGNER_NOT_FOUND';
                                    HrDescr := 'Cannot find the original signer.';
                                  end;
    LongInt($8009100F)          : begin
                                    HrStr := 'CRYPT_E_ATTRIBUTES_MISSING';
                                    HrDescr := 'The cryptographic message does not contain all of the requested attributes.';
                                  end;
    LongInt($80091010)          : begin
                                    HrStr := 'CRYPT_E_STREAM_MSG_NOT_READY';
                                    HrDescr := 'The streamed cryptographic message is not ready to return data.';
                                  end;
    LongInt($80091011)          : begin
                                    HrStr := 'CRYPT_E_STREAM_INSUFFICIENT_DATA';
                                    HrDescr := 'The streamed cryptographic message requires more data to complete the decode operation.';
                                  end;
    LongInt($80092001)          : begin
                                    HrStr := 'CRYPT_E_BAD_LEN';
                                    HrDescr := 'The length specified for the output data was insufficient.';
                                  end;
    LongInt($80092002)          : begin
                                    HrStr := 'CRYPT_E_BAD_ENCODE';
                                    HrDescr := 'An error occurred during the encode or decode operation.';
                                  end;
    LongInt($80092003)          : begin
                                    HrStr := 'CRYPT_E_FILE_ERROR';
                                    HrDescr := 'An error occurred while reading or writing to a file.';
                                  end;
    LongInt($80092004)          : begin
                                    HrStr := 'CRYPT_E_NOT_FOUND';
                                    HrDescr := 'Cannot find object or property.';
                                  end;
    LongInt($80092005)          : begin
                                    HrStr := 'CRYPT_E_EXISTS';
                                    HrDescr := 'The object or property already exists.';
                                  end;
    LongInt($80092006)          : begin
                                    HrStr := 'CRYPT_E_NO_PROVIDER';
                                    HrDescr := 'No provider was specified for the store or object.';
                                  end;
    LongInt($80092007)          : begin
                                    HrStr := 'CRYPT_E_SELF_SIGNED';
                                    HrDescr := 'The specified certificate is self-signed.';
                                  end;
    LongInt($80092008)          : begin
                                    HrStr := 'CRYPT_E_DELETED_PREV';
                                    HrDescr := 'The previous certificate or certificate revocation list (CRL) context was deleted.';
                                  end;
    LongInt($80092009)          : begin
                                    HrStr := 'CRYPT_E_NO_MATCH';
                                    HrDescr := 'Cannot find the requested object.';
                                  end;
    LongInt($8009200A)          : begin
                                    HrStr := 'CRYPT_E_UNEXPECTED_MSG_TYPE';
                                    HrDescr := 'The certificate does not have a property that references a private key.';
                                  end;
    LongInt($8009200B)          : begin
                                    HrStr := 'CRYPT_E_NO_KEY_PROPERTY';
                                    HrDescr := 'Cannot find the certificate and private key for decryption.';
                                  end;
    LongInt($8009200C)          : begin
                                    HrStr := 'CRYPT_E_NO_DECRYPT_CERT';
                                    HrDescr := 'Cannot find the certificate and private key to use for decryption.';
                                  end;
    LongInt($8009200D)          : begin
                                    HrStr := 'CRYPT_E_BAD_MSG';
                                    HrDescr := 'Not a cryptographic message or the cryptographic message is not formatted correctly.';
                                  end;
    LongInt($8009200E)          : begin
                                    HrStr := 'CRYPT_E_NO_SIGNER';
                                    HrDescr := 'The signed cryptographic message does not have a signer for the specified signer index.';
                                  end;
    LongInt($8009200F)          : begin
                                    HrStr := 'CRYPT_E_PENDING_CLOSE';
                                    HrDescr := 'Final closure is pending until additional frees or closes.';
                                  end;
    LongInt($80092010)          : begin
                                    HrStr := 'CRYPT_E_REVOKED';
                                    HrDescr := 'The certificate is revoked.';
                                  end;
    LongInt($80092011)          : begin
                                    HrStr := 'CRYPT_E_NO_REVOCATION_DLL';
                                    HrDescr := 'No DLL or exported function was found to verify revocation.';
                                  end;
    LongInt($80092012)          : begin
                                    HrStr := 'CRYPT_E_NO_REVOCATION_CHECK';
                                    HrDescr := 'The revocation function was unable to check revocation for the certificate.';
                                  end;
    LongInt($80092013)          : begin
                                    HrStr := 'CRYPT_E_REVOCATION_OFFLINE';
                                    HrDescr := 'The revocation function was unable to check revocation because the revocation server was offline.';
                                  end;
    LongInt($80092014)          : begin
                                    HrStr := 'CRYPT_E_NOT_IN_REVOCATION_DATABASE';
                                    HrDescr := 'The certificate is not in the revocation server''s database.';
                                  end;
    LongInt($80092020)          : begin
                                    HrStr := 'CRYPT_E_INVALID_NUMERIC_STRING';
                                    HrDescr := 'The string contains a non-numeric character.';
                                  end;
    LongInt($80092021)          : begin
                                    HrStr := 'CRYPT_E_INVALID_PRINTABLE_STRING';
                                    HrDescr := 'The string contains a nonprintable character.';
                                  end;
    LongInt($80092022)          : begin
                                    HrStr := 'CRYPT_E_INVALID_IA5_STRING';
                                    HrDescr := 'The string contains a character not in the 7-bit ASCII character set.';
                                  end;
    LongInt($80092023)          : begin
                                    HrStr := 'CRYPT_E_INVALID_X500_STRING';
                                    HrDescr := 'The string contains an invalid X500 name attribute key, object identifier (OID), value, or delimiter.';
                                  end;
    LongInt($80092024)          : begin
                                    HrStr := 'CRYPT_E_NOT_CHAR_STRING';
                                    HrDescr := 'The dwValueType for the CERT_NAME_VALUE is not one of the character strings.' +
                                               'Most likely it is either a CERT_RDN_ENCODED_BLOB or CERT_TDN_OCTED_STRING.';
                                  end;
    LongInt($80092025)          : begin
                                    HrStr := 'CRYPT_E_FILERESIZED';
                                    HrDescr := 'The Put operation cannot continue.' +
                                               'The file needs to be resized.' +
                                               'However, there is already a signature present.' +
                                               'A complete signing operation must be done.';
                                  end;
    LongInt($80092026)          : begin
                                    HrStr := 'CRYPT_E_SECURITY_SETTINGS';
                                    HrDescr := 'The cryptographic operation failed due to a local security option setting.';
                                  end;
    LongInt($80092027)          : begin
                                    HrStr := 'CRYPT_E_NO_VERIFY_USAGE_DLL';
                                    HrDescr := 'No DLL or exported function was found to verify subject usage.';
                                  end;
    LongInt($80092028)          : begin
                                    HrStr := 'CRYPT_E_NO_VERIFY_USAGE_CHECK';
                                    HrDescr := 'The called function was unable to perform a usage check on the subject.';
                                  end;
    LongInt($80092029)          : begin
                                    HrStr := 'CRYPT_E_VERIFY_USAGE_OFFLINE';
                                    HrDescr := 'The called function was unable to complete the usage check because the server was offline.';
                                  end;
    LongInt($8009202A)          : begin
                                    HrStr := 'CRYPT_E_NOT_IN_CTL';
                                    HrDescr := 'The subject was not found in a certificate trust list (CTL).';
                                  end;
    LongInt($8009202B)          : begin
                                    HrStr := 'CRYPT_E_NO_TRUSTED_SIGNER';
                                    HrDescr := 'None of the signers of the cryptographic message or certificate trust list is trusted.';
                                  end;
    LongInt($8009202C)          : begin
                                    HrStr := 'CRYPT_E_MISSING_PUBKEY_PARA';
                                    HrDescr := 'The public key''s algorithm parameters are missing.';
                                  end;
    LongInt($80093000)          : begin
                                    HrStr := 'CRYPT_E_OSS_ERROR';
                                    HrDescr := 'OSS Certificate encode/decode error code base.';
                                  end;
    LongInt($80093001)          : begin
                                    HrStr := 'OSS_MORE_BUF';
                                    HrDescr := 'OSS ASN.1 Error: Output Buffer is too small.';
                                  end;
    LongInt($80093002)          : begin
                                    HrStr := 'OSS_NEGATIVE_UINTEGER';
                                    HrDescr := 'OSS ASN.1 Error: Signed integer is encoded as a unsigned integer.';
                                  end;
    LongInt($80093003)          : begin
                                    HrStr := 'OSS_PDU_RANGE';
                                    HrDescr := 'OSS ASN.1 Error: Unknown ASN.1 data type.';
                                  end;
    LongInt($80093004)          : begin
                                    HrStr := 'OSS_MORE_INPUT';
                                    HrDescr := 'OSS ASN.1 Error: Output buffer is too small; the decoded data has been truncated.';
                                  end;
    LongInt($80093005)          : begin
                                    HrStr := 'OSS_DATA_ERROR';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($80093006)          : begin
                                    HrStr := 'OSS_BAD_ARG';
                                    HrDescr := 'OSS ASN.1 Error: Invalid argument.';
                                  end;
    LongInt($80093007)          : begin
                                    HrStr := 'OSS_BAD_VERSION';
                                    HrDescr := 'OSS ASN.1 Error: Encode/Decode version mismatch.';
                                  end;
    LongInt($80093008)          : begin
                                    HrStr := 'OSS_OUT_MEMORY';
                                    HrDescr := 'OSS ASN.1 Error: Out of memory.';
                                  end;
    LongInt($80093009)          : begin
                                    HrStr := 'OSS_PDU_MISMATCH';
                                    HrDescr := 'OSS ASN.1 Error: Encode/Decode error.';
                                  end;
    LongInt($8009300A)          : begin
                                    HrStr := 'OSS_LIMITED';
                                    HrDescr := 'OSS ASN.1 Error: Internal error.';
                                  end;
    LongInt($8009300B)          : begin
                                    HrStr := 'OSS_BAD_PTR';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($8009300C)          : begin
                                    HrStr := 'OSS_BAD_TIME';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($8009300D)          : begin
                                    HrStr := 'OSS_INDEFINITE_NOT_SUPPORTED';
                                    HrDescr := 'OSS ASN.1 Error: Unsupported BER indefinite-length encoding.';
                                  end;
    LongInt($8009300E)          : begin
                                    HrStr := 'OSS_MEM_ERROR';
                                    HrDescr := 'OSS ASN.1 Error: Access violation.';
                                  end;
    LongInt($8009300F)          : begin
                                    HrStr := 'OSS_BAD_TABLE';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($80093010)          : begin
                                    HrStr := 'OSS_TOO_LONG';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($80093011)          : begin
                                    HrStr := 'OSS_CONSTRAINT_VIOLATED';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($80093012)          : begin
                                    HrStr := 'OSS_FATAL_ERROR';
                                    HrDescr := 'OSS ASN.1 Error: Internal error.';
                                  end;
    LongInt($80093013)          : begin
                                    HrStr := 'OSS_ACCESS_SERIALIZATION_ERROR';
                                    HrDescr := 'OSS ASN.1 Error: Multithreading conflict.';
                                  end;
    LongInt($80093014)          : begin
                                    HrStr := 'OSS_NULL_TBL';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($80093015)          : begin
                                    HrStr := 'OSS_NULL_FCN';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($80093016)          : begin
                                    HrStr := 'OSS_BAD_ENCRULES';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($80093017)          : begin
                                    HrStr := 'OSS_UNAVAIL_ENCRULES';
                                    HrDescr := 'OSS ASN.1 Error: Encode/Decode function not implemented.';
                                  end;
    LongInt($80093018)          : begin
                                    HrStr := 'OSS_CANT_OPEN_TRACE_WINDOW';
                                    HrDescr := 'OSS ASN.1 Error: Trace file error.';
                                  end;
    LongInt($80093019)          : begin
                                    HrStr := 'OSS_UNIMPLEMENTED';
                                    HrDescr := 'OSS ASN.1 Error: Function not implemented.';
                                  end;
    LongInt($8009301A)          : begin
                                    HrStr := 'OSS_OID_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($8009301B)          : begin
                                    HrStr := 'OSS_CANT_OPEN_TRACE_FILE';
                                    HrDescr := 'OSS ASN.1 Error: Trace file error.';
                                  end;
    LongInt($8009301C)          : begin
                                    HrStr := 'OSS_TRACE_FILE_ALREADY_OPEN';
                                    HrDescr := 'OSS ASN.1 Error: Trace file error.';
                                  end;
    LongInt($8009301D)          : begin
                                    HrStr := 'OSS_TABLE_MISMATCH';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($8009301E)          : begin
                                    HrStr := 'OSS_TYPE_NOT_SUPPORTED';
                                    HrDescr := 'OSS ASN.1 Error: Invalid data.';
                                  end;
    LongInt($8009301F)          : begin
                                    HrStr := 'OSS_REAL_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093020)          : begin
                                    HrStr := 'OSS_REAL_CODE_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093021)          : begin
                                    HrStr := 'OSS_OUT_OF_RANGE';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093022)          : begin
                                    HrStr := 'OSS_COPIER_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093023)          : begin
                                    HrStr := 'OSS_CONSTRAINT_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093024)          : begin
                                    HrStr := 'OSS_COMPARATOR_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093025)          : begin
                                    HrStr := 'OSS_COMPARATOR_CODE_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093026)          : begin
                                    HrStr := 'OSS_MEM_MGR_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093027)          : begin
                                    HrStr := 'OSS_PDV_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093028)          : begin
                                    HrStr := 'OSS_PDV_CODE_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($80093029)          : begin
                                    HrStr := 'OSS_API_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($8009302A)          : begin
                                    HrStr := 'OSS_BERDER_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($8009302B)          : begin
                                    HrStr := 'OSS_PER_DLL_NOT_LINKED';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($8009302C)          : begin
                                    HrStr := 'OSS_OPEN_TYPE_ERROR';
                                    HrDescr := 'OSS ASN.1 Error: Program link error.';
                                  end;
    LongInt($8009302D)          : begin
                                    HrStr := 'OSS_MUTEX_NOT_CREATED';
                                    HrDescr := 'OSS ASN.1 Error: System resource error.';
                                  end;
    LongInt($8009302E)          : begin
                                    HrStr := 'OSS_CANT_CLOSE_TRACE_FILE';
                                    HrDescr := 'OSS ASN.1 Error: Trace file error.';
                                  end;
    LongInt($80093100)          : begin
                                    HrStr := 'CRYPT_E_ASN1_ERROR';
                                    HrDescr := 'ASN1 Certificate encode/decode error code base.';
                                  end;
    LongInt($80093101)          : begin
                                    HrStr := 'CRYPT_E_ASN1_INTERNAL';
                                    HrDescr := 'ASN1 internal encode or decode error.';
                                  end;
    LongInt($80093102)          : begin
                                    HrStr := 'CRYPT_E_ASN1_EOD';
                                    HrDescr := 'ASN1 unexpected end of data.';
                                  end;
    LongInt($80093103)          : begin
                                    HrStr := 'CRYPT_E_ASN1_CORRUPT';
                                    HrDescr := 'ASN1 corrupted data.';
                                  end;
    LongInt($80093104)          : begin
                                    HrStr := 'CRYPT_E_ASN1_LARGE';
                                    HrDescr := 'ASN1 value too large.';
                                  end;
    LongInt($80093105)          : begin
                                    HrStr := 'CRYPT_E_ASN1_CONSTRAINT';
                                    HrDescr := 'ASN1 constraint violated.';
                                  end;
    LongInt($80093106)          : begin
                                    HrStr := 'CRYPT_E_ASN1_MEMORY';
                                    HrDescr := 'ASN1 out of memory.';
                                  end;
    LongInt($80093107)          : begin
                                    HrStr := 'CRYPT_E_ASN1_OVERFLOW';
                                    HrDescr := 'ASN1 buffer overflow.';
                                  end;
    LongInt($80093108)          : begin
                                    HrStr := 'CRYPT_E_ASN1_BADPDU';
                                    HrDescr := 'ASN1 function not supported for this protocol data unit (PDU).';
                                  end;
    LongInt($80093109)          : begin
                                    HrStr := 'CRYPT_E_ASN1_BADARGS';
                                    HrDescr := 'ASN1 bad arguments to function call.';
                                  end;
    LongInt($8009310A)          : begin
                                    HrStr := 'CRYPT_E_ASN1_BADREAL';
                                    HrDescr := 'ASN1 bad real value.';
                                  end;
    LongInt($8009310B)          : begin
                                    HrStr := 'CRYPT_E_ASN1_BADTAG';
                                    HrDescr := 'ASN1 bad tag value met.';
                                  end;
    LongInt($8009310C)          : begin
                                    HrStr := 'CRYPT_E_ASN1_CHOICE';
                                    HrDescr := 'ASN1 bad choice value.';
                                  end;
    LongInt($8009310D)          : begin
                                    HrStr := 'CRYPT_E_ASN1_RULE';
                                    HrDescr := 'ASN1 bad encoding rule.';
                                  end;
    LongInt($8009310E)          : begin
                                    HrStr := 'CRYPT_E_ASN1_UTF8';
                                    HrDescr := 'ASN1 bad Unicode (UTF8).';
                                  end;
    LongInt($80093133)          : begin
                                    HrStr := 'CRYPT_E_ASN1_PDU_TYPE';
                                    HrDescr := 'ASN1 bad PDU type.';
                                  end;
    LongInt($80093134)          : begin
                                    HrStr := 'CRYPT_E_ASN1_NYI';
                                    HrDescr := 'ASN1 not yet implemented.';
                                  end;
    LongInt($80093201)          : begin
                                    HrStr := 'CRYPT_E_ASN1_EXTENDED';
                                    HrDescr := 'ASN1 skipped unknown extensions.';
                                  end;
    LongInt($80093202)          : begin
                                    HrStr := 'CRYPT_E_ASN1_NOEOD';
                                    HrDescr := 'ASN1 end of data expected.';
                                  end;
    LongInt($80094001)          : begin
                                    HrStr := 'CERTSRV_E_BAD_REQUESTSUBJECT';
                                    HrDescr := 'The request subject name is invalid or too long.';
                                  end;
    LongInt($80094002)          : begin
                                    HrStr := 'CERTSRV_E_NO_REQUEST';
                                    HrDescr := 'The request does not exist.';
                                  end;
    LongInt($80094003)          : begin
                                    HrStr := 'CERTSRV_E_BAD_REQUESTSTATUS';
                                    HrDescr := 'The request''s current status does not allow this operation.';
                                  end;
    LongInt($80094004)          : begin
                                    HrStr := 'CERTSRV_E_PROPERTY_EMPTY';
                                    HrDescr := 'The requested property value is empty.';
                                  end;
    LongInt($80094005)          : begin
                                    HrStr := 'CERTSRV_E_INVALID_CA_CERTIFICATE';
                                    HrDescr := 'The CA''s certificate contains invalid data.';
                                  end;
    LongInt($80094006)          : begin
                                    HrStr := 'CERTSRV_E_SERVER_SUSPENDED';
                                    HrDescr := 'Certificate service has been suspended for a database restore operation.';
                                  end;
    LongInt($80094007)          : begin
                                    HrStr := 'CERTSRV_E_ENCODING_LENGTH';
                                    HrDescr := 'The certificate contains an encoded length that is potentially incompatible with older enrollment software.';
                                  end;
    LongInt($80094008)          : begin
                                    HrStr := 'CERTSRV_E_ROLECONFLICT';
                                    HrDescr := 'The operation is denied.' +
                                               'The user has multiple roles assigned, and the CA is configured to enforce role separation.';
                                  end;
    LongInt($80094009)          : begin
                                    HrStr := 'CERTSRV_E_RESTRICTEDOFFICER';
                                    HrDescr := 'The operation is denied.' +
                                               'It can only be performed by a certificate manager that is allowed to manage certificates for the current requester.';
                                  end;
    LongInt($8009400A)          : begin
                                    HrStr := 'CERTSRV_E_KEY_ARCHIVAL_NOT_CONFIGURED';
                                    HrDescr := 'Cannot archive private key.' +
                                               'The CA is not configured for key archival.';
                                  end;
    LongInt($8009400B)          : begin
                                    HrStr := 'CERTSRV_E_NO_VALID_KRA';
                                    HrDescr := 'Cannot archive private key.' +
                                               'The CA could not verify one or more key recovery certificates.';
                                  end;
    LongInt($8009400C)          : begin
                                    HrStr := 'CERTSRV_E_BAD_REQUEST_KEY_ARCHIVAL';
                                    HrDescr := 'The request is incorrectly formatted.' +
                                               'The encrypted private key must be in an unauthenticated attribute in an outermost signature.';
                                  end;
    LongInt($8009400D)          : begin
                                    HrStr := 'CERTSRV_E_NO_CAADMIN_DEFINED';
                                    HrDescr := 'At least one security principal must have the permission to manage this CA.';
                                  end;
    LongInt($8009400E)          : begin
                                    HrStr := 'CERTSRV_E_BAD_RENEWAL_CERT_ATTRIBUTE';
                                    HrDescr := 'The request contains an invalid renewal certificate attribute.';
                                  end;
    LongInt($8009400F)          : begin
                                    HrStr := 'CERTSRV_E_NO_DB_SESSIONS';
                                    HrDescr := 'An attempt was made to open a CA database session, but there are already too many active sessions.' +
                                               'The server needs to be configured to allow additional sessions.';
                                  end;
    LongInt($80094010)          : begin
                                    HrStr := 'CERTSRV_E_ALIGNMENT_FAULT';
                                    HrDescr := 'A memory reference caused a data alignment fault.';
                                  end;
    LongInt($80094011)          : begin
                                    HrStr := 'CERTSRV_E_ENROLL_DENIED';
                                    HrDescr := 'The permissions on this CA do not allow the current user to enroll for certificates.';
                                  end;
    LongInt($80094012)          : begin
                                    HrStr := 'CERTSRV_E_TEMPLATE_DENIED';
                                    HrDescr := 'The permissions on the certificate template do not allow the current user to enroll for this type of certificate.';
                                  end;
    LongInt($80094013)          : begin
                                    HrStr := 'CERTSRV_E_DOWNLEVEL_DC_SSL_OR_UPGRADE';
                                    HrDescr := 'The contacted domain controller cannot support signed Lightweight Directory Access Protocol (LDAP) traffic.' +
                                               'Update the domain controller or configure Certificate Services to use SSL for Active Directory access.';
                                  end;
    LongInt($80094800)          : begin
                                    HrStr := 'CERTSRV_E_UNSUPPORTED_CERT_TYPE';
                                    HrDescr := 'The requested certificate template is not supported by this CA.';
                                  end;
    LongInt($80094801)          : begin
                                    HrStr := 'CERTSRV_E_NO_CERT_TYPE';
                                    HrDescr := 'The request contains no certificate template information.';
                                  end;
    LongInt($80094802)          : begin
                                    HrStr := 'CERTSRV_E_TEMPLATE_CONFLICT';
                                    HrDescr := 'The request contains conflicting template information.';
                                  end;
    LongInt($80094803)          : begin
                                    HrStr := 'CERTSRV_E_SUBJECT_ALT_NAME_REQUIRED';
                                    HrDescr := 'The request is missing a required Subject Alternate name extension.';
                                  end;
    LongInt($80094804)          : begin
                                    HrStr := 'CERTSRV_E_ARCHIVED_KEY_REQUIRED';
                                    HrDescr := 'The request is missing a required private key for archival by the server.';
                                  end;
    LongInt($80094805)          : begin
                                    HrStr := 'CERTSRV_E_SMIME_REQUIRED';
                                    HrDescr := 'The request is missing a required SMIME capabilities extension.';
                                  end;
    LongInt($80094806)          : begin
                                    HrStr := 'CERTSRV_E_BAD_RENEWAL_SUBJECT';
                                    HrDescr := 'The request was made on behalf of a subject other than the caller.' +
                                               'The certificate template must be configured to require at least one signature to authorize the request.';
                                  end;
    LongInt($80094807)          : begin
                                    HrStr := 'CERTSRV_E_BAD_TEMPLATE_VERSION';
                                    HrDescr := 'The request template version is newer than the supported template version.';
                                  end;
    LongInt($80094808)          : begin
                                    HrStr := 'CERTSRV_E_TEMPLATE_POLICY_REQUIRED';
                                    HrDescr := 'The template is missing a required signature policy attribute.';
                                  end;
    LongInt($80094809)          : begin
                                    HrStr := 'CERTSRV_E_SIGNATURE_POLICY_REQUIRED';
                                    HrDescr := 'The request is missing required signature policy information.';
                                  end;
    LongInt($8009480A)          : begin
                                    HrStr := 'CERTSRV_E_SIGNATURE_COUNT';
                                    HrDescr := 'The request is missing one or more required signatures.';
                                  end;
    LongInt($8009480B)          : begin
                                    HrStr := 'CERTSRV_E_SIGNATURE_REJECTED';
                                    HrDescr := 'One or more signatures did not include the required application or issuance policies.' +
                                               'The request is missing one or more required valid signatures.';
                                  end;
    LongInt($8009480C)          : begin
                                    HrStr := 'CERTSRV_E_ISSUANCE_POLICY_REQUIRED';
                                    HrDescr := 'The request is missing one or more required signature issuance policies.';
                                  end;
    LongInt($8009480D)          : begin
                                    HrStr := 'CERTSRV_E_SUBJECT_UPN_REQUIRED';
                                    HrDescr := 'The UPN is unavailable and cannot be added to the Subject Alternate name.';
                                  end;
    LongInt($8009480E)          : begin
                                    HrStr := 'CERTSRV_E_SUBJECT_DIRECTORY_GUID_REQUIRED';
                                    HrDescr := 'The Active Directory GUID is unavailable and cannot be added to the Subject Alternate name.';
                                  end;
    LongInt($8009480F)          : begin
                                    HrStr := 'CERTSRV_E_SUBJECT_DNS_REQUIRED';
                                    HrDescr := 'The Domain Name System (DNS) name is unavailable and cannot be added to the Subject Alternate name.';
                                  end;
    LongInt($80094810)          : begin
                                    HrStr := 'CERTSRV_E_ARCHIVED_KEY_UNEXPECTED';
                                    HrDescr := 'The request includes a private key for archival by the server, but key archival is not enabled for the specified certificate template.';
                                  end;
    LongInt($80094811)          : begin
                                    HrStr := 'CERTSRV_E_KEY_LENGTH';
                                    HrDescr := 'The public key does not meet the minimum size required by the specified certificate template.';
                                  end;
    LongInt($80094812)          : begin
                                    HrStr := 'CERTSRV_E_SUBJECT_EMAIL_REQUIRED';
                                    HrDescr := 'The email name is unavailable and cannot be added to the Subject or Subject Alternate name.';
                                  end;
    LongInt($80094813)          : begin
                                    HrStr := 'CERTSRV_E_UNKNOWN_CERT_TYPE';
                                    HrDescr := 'One or more certificate templates to be enabled on this CA could not be found.';
                                  end;
    LongInt($80094814)          : begin
                                    HrStr := 'CERTSRV_E_CERT_TYPE_OVERLAP';
                                    HrDescr := 'The certificate template renewal period is longer than the certificate validity period.' +
                                               'The template should be reconfigured or the CA certificate renewed.';
                                  end;
    LongInt($80094815)          : begin
                                    HrStr := 'CERTSRV_E_TOO_MANY_SIGNATURES';
                                    HrDescr := 'The certificate template requires too many return authorization (RA) signatures.' +
                                               'Only one RA signature is allowed.';
                                  end;
    LongInt($80094816)          : begin
                                    HrStr := 'CERTSRV_E_RENEWAL_BAD_PUBLIC_KEY';
                                    HrDescr := 'The key used in a renewal request does not match one of the certificates being renewed.';
                                  end;
    LongInt($80094817)          : begin
                                    HrStr := 'CERTSRV_E_INVALID_EK';
                                    HrDescr := 'The endorsement key certificate is not valid.';
                                  end;
    LongInt($8009481A)          : begin
                                    HrStr := 'CERTSRV_E_KEY_ATTESTATION';
                                    HrDescr := 'Key attestation did not succeed.';
                                  end;
    LongInt($80095000)          : begin
                                    HrStr := 'XENROLL_E_KEY_NOT_EXPORTABLE';
                                    HrDescr := 'The key is not exportable.';
                                  end;
    LongInt($80095001)          : begin
                                    HrStr := 'XENROLL_E_CANNOT_ADD_ROOT_CERT';
                                    HrDescr := 'You cannot add the root CA certificate into your local store.';
                                  end;
    LongInt($80095002)          : begin
                                    HrStr := 'XENROLL_E_RESPONSE_KA_HASH_NOT_FOUND';
                                    HrDescr := 'The key archival hash attribute was not found in the response.';
                                  end;
    LongInt($80095003)          : begin
                                    HrStr := 'XENROLL_E_RESPONSE_UNEXPECTED_KA_HASH';
                                    HrDescr := 'An unexpected key archival hash attribute was found in the response.';
                                  end;
    LongInt($80095004)          : begin
                                    HrStr := 'XENROLL_E_RESPONSE_KA_HASH_MISMATCH';
                                    HrDescr := 'There is a key archival hash mismatch between the request and the response.';
                                  end;
    LongInt($80095005)          : begin
                                    HrStr := 'XENROLL_E_KEYSPEC_SMIME_MISMATCH';
                                    HrDescr := 'Signing certificate cannot include SMIME extension.';
                                  end;
    LongInt($80096001)          : begin
                                    HrStr := 'TRUST_E_SYSTEM_ERROR';
                                    HrDescr := 'A system-level error occurred while verifying trust.';
                                  end;
    LongInt($80096002)          : begin
                                    HrStr := 'TRUST_E_NO_SIGNER_CERT';
                                    HrDescr := 'The certificate for the signer of the message is invalid or not found.';
                                  end;
    LongInt($80096003)          : begin
                                    HrStr := 'TRUST_E_COUNTER_SIGNER';
                                    HrDescr := 'One of the counter signatures was invalid.';
                                  end;
    LongInt($80096004)          : begin
                                    HrStr := 'TRUST_E_CERT_SIGNATURE';
                                    HrDescr := 'The signature of the certificate cannot be verified.';
                                  end;
    LongInt($80096005)          : begin
                                    HrStr := 'TRUST_E_TIME_STAMP';
                                    HrDescr := 'The time-stamp signature or certificate could not be verified or is malformed.';
                                  end;
    LongInt($80096010)          : begin
                                    HrStr := 'TRUST_E_BAD_DIGEST';
                                    HrDescr := 'The digital signature of the object did not verify.';
                                  end;
    LongInt($80096019)          : begin
                                    HrStr := 'TRUST_E_BASIC_CONSTRAINTS';
                                    HrDescr := 'A certificate''s basic constraint extension has not been observed.';
                                  end;
    LongInt($8009601E)          : begin
                                    HrStr := 'TRUST_E_FINANCIAL_CRITERIA';
                                    HrDescr := 'The certificate does not meet or contain the Authenticode financial extensions.';
                                  end;
    LongInt($80097001)          : begin
                                    HrStr := 'MSSIPOTF_E_OUTOFMEMRANGE';
                                    HrDescr := 'Tried to reference a part of the file outside the proper range.';
                                  end;
    LongInt($80097002)          : begin
                                    HrStr := 'MSSIPOTF_E_CANTGETOBJECT';
                                    HrDescr := 'Could not retrieve an object from the file.';
                                  end;
    LongInt($80097003)          : begin
                                    HrStr := 'MSSIPOTF_E_NOHEADTABLE';
                                    HrDescr := 'Could not find the head table in the file.';
                                  end;
    LongInt($80097004)          : begin
                                    HrStr := 'MSSIPOTF_E_BAD_MAGICNUMBER';
                                    HrDescr := 'The magic number in the head table is incorrect.';
                                  end;
    LongInt($80097005)          : begin
                                    HrStr := 'MSSIPOTF_E_BAD_OFFSET_TABLE';
                                    HrDescr := 'The offset table has incorrect values.';
                                  end;
    LongInt($80097006)          : begin
                                    HrStr := 'MSSIPOTF_E_TABLE_TAGORDER';
                                    HrDescr := 'Duplicate table tags or the tags are out of alphabetical order.';
                                  end;
    LongInt($80097007)          : begin
                                    HrStr := 'MSSIPOTF_E_TABLE_LONGWORD';
                                    HrDescr := 'A table does not start on a long word boundary.';
                                  end;
    LongInt($80097008)          : begin
                                    HrStr := 'MSSIPOTF_E_BAD_FIRST_TABLE_PLACEMENT';
                                    HrDescr := 'First table does not appear after header information.';
                                  end;
    LongInt($80097009)          : begin
                                    HrStr := 'MSSIPOTF_E_TABLES_OVERLAP';
                                    HrDescr := 'Two or more tables overlap.';
                                  end;
    LongInt($8009700A)          : begin
                                    HrStr := 'MSSIPOTF_E_TABLE_PADBYTES';
                                    HrDescr := 'Too many pad bytes between tables, or pad bytes are not 0.';
                                  end;
    LongInt($8009700B)          : begin
                                    HrStr := 'MSSIPOTF_E_FILETOOSMALL';
                                    HrDescr := 'File is too small to contain the last table.';
                                  end;
    LongInt($8009700C)          : begin
                                    HrStr := 'MSSIPOTF_E_TABLE_CHECKSUM';
                                    HrDescr := 'A table checksum is incorrect.';
                                  end;
    LongInt($8009700D)          : begin
                                    HrStr := 'MSSIPOTF_E_FILE_CHECKSUM';
                                    HrDescr := 'The file checksum is incorrect.';
                                  end;
    LongInt($80097010)          : begin
                                    HrStr := 'MSSIPOTF_E_FAILED_POLICY';
                                    HrDescr := 'The signature does not have the correct attributes for the policy.';
                                  end;
    LongInt($80097011)          : begin
                                    HrStr := 'MSSIPOTF_E_FAILED_HINTS_CHECK';
                                    HrDescr := 'The file did not pass the hints check.';
                                  end;
    LongInt($80097012)          : begin
                                    HrStr := 'MSSIPOTF_E_NOT_OPENTYPE';
                                    HrDescr := 'The file is not an OpenType file.';
                                  end;
    LongInt($80097013)          : begin
                                    HrStr := 'MSSIPOTF_E_FILE';
                                    HrDescr := 'Failed on a file operation (such as open, map, read, or write).';
                                  end;
    LongInt($80097014)          : begin
                                    HrStr := 'MSSIPOTF_E_CRYPT';
                                    HrDescr := 'A call to a CryptoAPI function failed.';
                                  end;
    LongInt($80097015)          : begin
                                    HrStr := 'MSSIPOTF_E_BADVERSION';
                                    HrDescr := 'There is a bad version number in the file.';
                                  end;
    LongInt($80097016)          : begin
                                    HrStr := 'MSSIPOTF_E_DSIG_STRUCTURE';
                                    HrDescr := 'The structure of the DSIG table is incorrect.';
                                  end;
    LongInt($80097017)          : begin
                                    HrStr := 'MSSIPOTF_E_PCONST_CHECK';
                                    HrDescr := 'A check failed in a partially constant table.';
                                  end;
    LongInt($80097018)          : begin
                                    HrStr := 'MSSIPOTF_E_STRUCTURE';
                                    HrDescr := 'Some kind of structural error.';
                                  end;
    LongInt($80097019)          : begin
                                    HrStr := 'ERROR_CRED_REQUIRES_CONFIRMATION';
                                    HrDescr := 'The requested credential requires confirmation.';
                                  end;
    LongInt($800B0001)          : begin
                                    HrStr := 'TRUST_E_PROVIDER_UNKNOWN';
                                    HrDescr := 'Unknown trust provider.';
                                  end;
    LongInt($800B0002)          : begin
                                    HrStr := 'TRUST_E_ACTION_UNKNOWN';
                                    HrDescr := 'The trust verification action specified is not supported by the specified trust provider.';
                                  end;
    LongInt($800B0003)          : begin
                                    HrStr := 'TRUST_E_SUBJECT_FORM_UNKNOWN';
                                    HrDescr := 'The form specified for the subject is not one supported or known by the specified trust provider.';
                                  end;
    LongInt($800B0004)          : begin
                                    HrStr := 'TRUST_E_SUBJECT_NOT_TRUSTED';
                                    HrDescr := 'The subject is not trusted for the specified action.';
                                  end;
    LongInt($800B0005)          : begin
                                    HrStr := 'DIGSIG_E_ENCODE';
                                    HrDescr := 'Error due to problem in ASN.1 encoding process.';
                                  end;
    LongInt($800B0006)          : begin
                                    HrStr := 'DIGSIG_E_DECODE';
                                    HrDescr := 'Error due to problem in ASN.1 decoding process.';
                                  end;
    LongInt($800B0007)          : begin
                                    HrStr := 'DIGSIG_E_EXTENSIBILITY';
                                    HrDescr := 'Reading/writing extensions where attributes are appropriate, and vice versa.';
                                  end;
    LongInt($800B0008)          : begin
                                    HrStr := 'DIGSIG_E_CRYPTO';
                                    HrDescr := 'Unspecified cryptographic failure.';
                                  end;
    LongInt($800B0009)          : begin
                                    HrStr := 'PERSIST_E_SIZEDEFINITE';
                                    HrDescr := 'The size of the data could not be determined.';
                                  end;
    LongInt($800B000A)          : begin
                                    HrStr := 'PERSIST_E_SIZEINDEFINITE';
                                    HrDescr := 'The size of the indefinite-sized data could not be determined.';
                                  end;
    LongInt($800B000B)          : begin
                                    HrStr := 'PERSIST_E_NOTSELFSIZING';
                                    HrDescr := 'This object does not read and write self-sizing data.';
                                  end;
    LongInt($800B0100)          : begin
                                    HrStr := 'TRUST_E_NOSIGNATURE';
                                    HrDescr := 'No signature was present in the subject.';
                                  end;
    LongInt($800B0101)          : begin
                                    HrStr := 'CERT_E_EXPIRED';
                                    HrDescr := 'A required certificate is not within its validity period when verifying against the current system clock or the time stamp in the signed file.';
                                  end;
    LongInt($800B0102)          : begin
                                    HrStr := 'CERT_E_VALIDITYPERIODNESTING';
                                    HrDescr := 'The validity periods of the certification chain do not nest correctly.';
                                  end;
    LongInt($800B0103)          : begin
                                    HrStr := 'CERT_E_ROLE';
                                    HrDescr := 'A certificate that can only be used as an end entity is being used as a CA or vice versa.';
                                  end;
    LongInt($800B0104)          : begin
                                    HrStr := 'CERT_E_PATHLENCONST';
                                    HrDescr := 'A path length constraint in the certification chain has been violated.';
                                  end;
    LongInt($800B0105)          : begin
                                    HrStr := 'CERT_E_CRITICAL';
                                    HrDescr := 'A certificate contains an unknown extension that is marked "critical".';
                                  end;
    LongInt($800B0106)          : begin
                                    HrStr := 'CERT_E_PURPOSE';
                                    HrDescr := 'A certificate is being used for a purpose other than the ones specified by its CA.';
                                  end;
    LongInt($800B0107)          : begin
                                    HrStr := 'CERT_E_ISSUERCHAINING';
                                    HrDescr := 'A parent of a given certificate did not issue that child certificate.';
                                  end;
    LongInt($800B0108)          : begin
                                    HrStr := 'CERT_E_MALFORMED';
                                    HrDescr := 'A certificate is missing or has an empty value for an important field, such as a subject or issuer name.';
                                  end;
    LongInt($800B0109)          : begin
                                    HrStr := 'CERT_E_UNTRUSTEDROOT';
                                    HrDescr := 'A certificate chain processed, but terminated in a root certificate that is not trusted by the trust provider.';
                                  end;
    LongInt($800B010A)          : begin
                                    HrStr := 'CERT_E_CHAINING';
                                    HrDescr := 'A certificate chain could not be built to a trusted root authority.';
                                  end;
    LongInt($800B010B)          : begin
                                    HrStr := 'TRUST_E_FAIL';
                                    HrDescr := 'Generic trust failure.';
                                  end;
    LongInt($800B010C)          : begin
                                    HrStr := 'CERT_E_REVOKED';
                                    HrDescr := 'A certificate was explicitly revoked by its issuer.' +
                                               'If the certificate is Microsoft Windows PCA 2010, then the driver was signed by a certificate no longer recognized by Windows.<3>';
                                  end;
    LongInt($800B010D)          : begin
                                    HrStr := 'CERT_E_UNTRUSTEDTESTROOT';
                                    HrDescr := 'The certification path terminates with the test root that is not trusted with the current policy settings.';
                                  end;
    LongInt($800B010E)          : begin
                                    HrStr := 'CERT_E_REVOCATION_FAILURE';
                                    HrDescr := 'The revocation process could not continue—the certificates could not be checked.';
                                  end;
    LongInt($800B010F)          : begin
                                    HrStr := 'CERT_E_CN_NO_MATCH';
                                    HrDescr := 'The certificate''s CN name does not match the passed value.';
                                  end;
    LongInt($800B0110)          : begin
                                    HrStr := 'CERT_E_WRONG_USAGE';
                                    HrDescr := 'The certificate is not valid for the requested usage.';
                                  end;
    LongInt($800B0111)          : begin
                                    HrStr := 'TRUST_E_EXPLICIT_DISTRUST';
                                    HrDescr := 'The certificate was explicitly marked as untrusted by the user.';
                                  end;
    LongInt($800B0112)          : begin
                                    HrStr := 'CERT_E_UNTRUSTEDCA';
                                    HrDescr := 'A certification chain processed correctly, but one of the CA certificates is not trusted by the policy provider.';
                                  end;
    LongInt($800B0113)          : begin
                                    HrStr := 'CERT_E_INVALID_POLICY';
                                    HrDescr := 'The certificate has invalid policy.';
                                  end;
    LongInt($800B0114)          : begin
                                    HrStr := 'CERT_E_INVALID_NAME';
                                    HrDescr := 'The certificate has an invalid name.' +
                                               'The name is not included in the permitted list or is explicitly excluded.';
                                  end;
    LongInt($800D0003)          : begin
                                    HrStr := 'NS_W_SERVER_BANDWIDTH_LIMIT';
                                    HrDescr := 'The maximum filebitrate value specified is greater than the server''s configured maximum bandwidth.';
                                  end;
    LongInt($800D0004)          : begin
                                    HrStr := 'NS_W_FILE_BANDWIDTH_LIMIT';
                                    HrDescr := 'The maximum bandwidth value specified is less than the maximum filebitrate.';
                                  end;
    LongInt($800D0060)          : begin
                                    HrStr := 'NS_W_UNKNOWN_EVENT';
                                    HrDescr := 'Unknown %1 event encountered.';
                                  end;
    LongInt($800D0199)          : begin
                                    HrStr := 'NS_I_CATATONIC_FAILURE';
                                    HrDescr := 'Disk %1 ( %2 ) on Content Server %3, will be failed because it is catatonic.';
                                  end;
    LongInt($800D019A)          : begin
                                    HrStr := 'NS_I_CATATONIC_AUTO_UNFAIL';
                                    HrDescr := 'Disk %1 ( %2 ) on Content Server %3, auto online from catatonic state.';
                                  end;
    LongInt($800F0000)          : begin
                                    HrStr := 'SPAPI_E_EXPECTED_SECTION_NAME';
                                    HrDescr := 'A non-empty line was encountered in the INF before the start of a section.';
                                  end;
    LongInt($800F0001)          : begin
                                    HrStr := 'SPAPI_E_BAD_SECTION_NAME_LINE';
                                    HrDescr := 'A section name marker in the information file (INF) is not complete or does not exist on a line by itself.';
                                  end;
    LongInt($800F0002)          : begin
                                    HrStr := 'SPAPI_E_SECTION_NAME_TOO_LONG';
                                    HrDescr := 'An INF section was encountered whose name exceeds the maximum section name length.';
                                  end;
    LongInt($800F0003)          : begin
                                    HrStr := 'SPAPI_E_GENERAL_SYNTAX';
                                    HrDescr := 'The syntax of the INF is invalid.';
                                  end;
    LongInt($800F0100)          : begin
                                    HrStr := 'SPAPI_E_WRONG_INF_STYLE';
                                    HrDescr := 'The style of the INF is different than what was requested.';
                                  end;
    LongInt($800F0101)          : begin
                                    HrStr := 'SPAPI_E_SECTION_NOT_FOUND';
                                    HrDescr := 'The required section was not found in the INF.';
                                  end;
    LongInt($800F0102)          : begin
                                    HrStr := 'SPAPI_E_LINE_NOT_FOUND';
                                    HrDescr := 'The required line was not found in the INF.';
                                  end;
    LongInt($800F0103)          : begin
                                    HrStr := 'SPAPI_E_NO_BACKUP';
                                    HrDescr := 'The files affected by the installation of this file queue have not been backed up for uninstall.';
                                  end;
    LongInt($800F0200)          : begin
                                    HrStr := 'SPAPI_E_NO_ASSOCIATED_CLASS';
                                    HrDescr := 'The INF or the device information set or element does not have an associated install class.';
                                  end;
    LongInt($800F0201)          : begin
                                    HrStr := 'SPAPI_E_CLASS_MISMATCH';
                                    HrDescr := 'The INF or the device information set or element does not match the specified install class.';
                                  end;
    LongInt($800F0202)          : begin
                                    HrStr := 'SPAPI_E_DUPLICATE_FOUND';
                                    HrDescr := 'An existing device was found that is a duplicate of the device being manually installed.';
                                  end;
    LongInt($800F0203)          : begin
                                    HrStr := 'SPAPI_E_NO_DRIVER_SELECTED';
                                    HrDescr := 'There is no driver selected for the device information set or element.';
                                  end;
    LongInt($800F0204)          : begin
                                    HrStr := 'SPAPI_E_KEY_DOES_NOT_EXIST';
                                    HrDescr := 'The requested device registry key does not exist.';
                                  end;
    LongInt($800F0205)          : begin
                                    HrStr := 'SPAPI_E_INVALID_DEVINST_NAME';
                                    HrDescr := 'The device instance name is invalid.';
                                  end;
    LongInt($800F0206)          : begin
                                    HrStr := 'SPAPI_E_INVALID_CLASS';
                                    HrDescr := 'The install class is not present or is invalid.';
                                  end;
    LongInt($800F0207)          : begin
                                    HrStr := 'SPAPI_E_DEVINST_ALREADY_EXISTS';
                                    HrDescr := 'The device instance cannot be created because it already exists.';
                                  end;
    LongInt($800F0208)          : begin
                                    HrStr := 'SPAPI_E_DEVINFO_NOT_REGISTERED';
                                    HrDescr := 'The operation cannot be performed on a device information element that has not been registered.';
                                  end;
    LongInt($800F0209)          : begin
                                    HrStr := 'SPAPI_E_INVALID_REG_PROPERTY';
                                    HrDescr := 'The device property code is invalid.';
                                  end;
    LongInt($800F020A)          : begin
                                    HrStr := 'SPAPI_E_NO_INF';
                                    HrDescr := 'The INF from which a driver list is to be built does not exist.';
                                  end;
    LongInt($800F020B)          : begin
                                    HrStr := 'SPAPI_E_NO_SUCH_DEVINST';
                                    HrDescr := 'The device instance does not exist in the hardware tree.';
                                  end;
    LongInt($800F020C)          : begin
                                    HrStr := 'SPAPI_E_CANT_LOAD_CLASS_ICON';
                                    HrDescr := 'The icon representing this install class cannot be loaded.';
                                  end;
    LongInt($800F020D)          : begin
                                    HrStr := 'SPAPI_E_INVALID_CLASS_INSTALLER';
                                    HrDescr := 'The class installer registry entry is invalid.';
                                  end;
    LongInt($800F020E)          : begin
                                    HrStr := 'SPAPI_E_DI_DO_DEFAULT';
                                    HrDescr := 'The class installer has indicated that the default action should be performed for this installation request.';
                                  end;
    LongInt($800F020F)          : begin
                                    HrStr := 'SPAPI_E_DI_NOFILECOPY';
                                    HrDescr := 'The operation does not require any files to be copied.';
                                  end;
    LongInt($800F0210)          : begin
                                    HrStr := 'SPAPI_E_INVALID_HWPROFILE';
                                    HrDescr := 'The specified hardware profile does not exist.';
                                  end;
    LongInt($800F0211)          : begin
                                    HrStr := 'SPAPI_E_NO_DEVICE_SELECTED';
                                    HrDescr := 'There is no device information element currently selected for this device information set.';
                                  end;
    LongInt($800F0212)          : begin
                                    HrStr := 'SPAPI_E_DEVINFO_LIST_LOCKED';
                                    HrDescr := 'The operation cannot be performed because the device information set is locked.';
                                  end;
    LongInt($800F0213)          : begin
                                    HrStr := 'SPAPI_E_DEVINFO_DATA_LOCKED';
                                    HrDescr := 'The operation cannot be performed because the device information element is locked.';
                                  end;
    LongInt($800F0214)          : begin
                                    HrStr := 'SPAPI_E_DI_BAD_PATH';
                                    HrDescr := 'The specified path does not contain any applicable device INFs.';
                                  end;
    LongInt($800F0215)          : begin
                                    HrStr := 'SPAPI_E_NO_CLASSINSTALL_PARAMS';
                                    HrDescr := 'No class installer parameters have been set for the device information set or element.';
                                  end;
    LongInt($800F0216)          : begin
                                    HrStr := 'SPAPI_E_FILEQUEUE_LOCKED';
                                    HrDescr := 'The operation cannot be performed because the file queue is locked.';
                                  end;
    LongInt($800F0217)          : begin
                                    HrStr := 'SPAPI_E_BAD_SERVICE_INSTALLSECT';
                                    HrDescr := 'A service installation section in this INF is invalid.';
                                  end;
    LongInt($800F0218)          : begin
                                    HrStr := 'SPAPI_E_NO_CLASS_DRIVER_LIST';
                                    HrDescr := 'There is no class driver list for the device information element.';
                                  end;
    LongInt($800F0219)          : begin
                                    HrStr := 'SPAPI_E_NO_ASSOCIATED_SERVICE';
                                    HrDescr := 'The installation failed because a function driver was not specified for this device instance.';
                                  end;
    LongInt($800F021A)          : begin
                                    HrStr := 'SPAPI_E_NO_DEFAULT_DEVICE_INTERFACE';
                                    HrDescr := 'There is presently no default device interface designated for this interface class.';
                                  end;
    LongInt($800F021B)          : begin
                                    HrStr := 'SPAPI_E_DEVICE_INTERFACE_ACTIVE';
                                    HrDescr := 'The operation cannot be performed because the device interface is currently active.';
                                  end;
    LongInt($800F021C)          : begin
                                    HrStr := 'SPAPI_E_DEVICE_INTERFACE_REMOVED';
                                    HrDescr := 'The operation cannot be performed because the device interface has been removed from the system.';
                                  end;
    LongInt($800F021D)          : begin
                                    HrStr := 'SPAPI_E_BAD_INTERFACE_INSTALLSECT';
                                    HrDescr := 'An interface installation section in this INF is invalid.';
                                  end;
    LongInt($800F021E)          : begin
                                    HrStr := 'SPAPI_E_NO_SUCH_INTERFACE_CLASS';
                                    HrDescr := 'This interface class does not exist in the system.';
                                  end;
    LongInt($800F021F)          : begin
                                    HrStr := 'SPAPI_E_INVALID_REFERENCE_STRING';
                                    HrDescr := 'The reference string supplied for this interface device is invalid.';
                                  end;
    LongInt($800F0220)          : begin
                                    HrStr := 'SPAPI_E_INVALID_MACHINENAME';
                                    HrDescr := 'The specified machine name does not conform to Universal Naming Convention (UNCs).';
                                  end;
    LongInt($800F0221)          : begin
                                    HrStr := 'SPAPI_E_REMOTE_COMM_FAILURE';
                                    HrDescr := 'A general remote communication error occurred.';
                                  end;
    LongInt($800F0222)          : begin
                                    HrStr := 'SPAPI_E_MACHINE_UNAVAILABLE';
                                    HrDescr := 'The machine selected for remote communication is not available at this time.';
                                  end;
    LongInt($800F0223)          : begin
                                    HrStr := 'SPAPI_E_NO_CONFIGMGR_SERVICES';
                                    HrDescr := 'The Plug and Play service is not available on the remote machine.';
                                  end;
    LongInt($800F0224)          : begin
                                    HrStr := 'SPAPI_E_INVALID_PROPPAGE_PROVIDER';
                                    HrDescr := 'The property page provider registry entry is invalid.';
                                  end;
    LongInt($800F0225)          : begin
                                    HrStr := 'SPAPI_E_NO_SUCH_DEVICE_INTERFACE';
                                    HrDescr := 'The requested device interface is not present in the system.';
                                  end;
    LongInt($800F0226)          : begin
                                    HrStr := 'SPAPI_E_DI_POSTPROCESSING_REQUIRED';
                                    HrDescr := 'The device''s co-installer has additional work to perform after installation is complete.';
                                  end;
    LongInt($800F0227)          : begin
                                    HrStr := 'SPAPI_E_INVALID_COINSTALLER';
                                    HrDescr := 'The device''s co-installer is invalid.';
                                  end;
    LongInt($800F0228)          : begin
                                    HrStr := 'SPAPI_E_NO_COMPAT_DRIVERS';
                                    HrDescr := 'There are no compatible drivers for this device.';
                                  end;
    LongInt($800F0229)          : begin
                                    HrStr := 'SPAPI_E_NO_DEVICE_ICON';
                                    HrDescr := 'There is no icon that represents this device or device type.';
                                  end;
    LongInt($800F022A)          : begin
                                    HrStr := 'SPAPI_E_INVALID_INF_LOGCONFIG';
                                    HrDescr := 'A logical configuration specified in this INF is invalid.';
                                  end;
    LongInt($800F022B)          : begin
                                    HrStr := 'SPAPI_E_DI_DONT_INSTALL';
                                    HrDescr := 'The class installer has denied the request to install or upgrade this device.';
                                  end;
    LongInt($800F022C)          : begin
                                    HrStr := 'SPAPI_E_INVALID_FILTER_DRIVER';
                                    HrDescr := 'One of the filter drivers installed for this device is invalid.';
                                  end;
    LongInt($800F022D)          : begin
                                    HrStr := 'SPAPI_E_NON_WINDOWS_NT_DRIVER';
                                    HrDescr := 'The driver selected for this device does not support Windows XP operating system.';
                                  end;
    LongInt($800F022E)          : begin
                                    HrStr := 'SPAPI_E_NON_WINDOWS_DRIVER';
                                    HrDescr := 'The driver selected for this device does not support Windows.';
                                  end;
    LongInt($800F022F)          : begin
                                    HrStr := 'SPAPI_E_NO_CATALOG_FOR_OEM_INF';
                                    HrDescr := 'The third-party INF does not contain digital signature information.';
                                  end;
    LongInt($800F0230)          : begin
                                    HrStr := 'SPAPI_E_DEVINSTALL_QUEUE_NONNATIVE';
                                    HrDescr := 'An invalid attempt was made to use a device installation file queue for verification of digital signatures relative to other platforms.';
                                  end;
    LongInt($800F0231)          : begin
                                    HrStr := 'SPAPI_E_NOT_DISABLEABLE';
                                    HrDescr := 'The device cannot be disabled.';
                                  end;
    LongInt($800F0232)          : begin
                                    HrStr := 'SPAPI_E_CANT_REMOVE_DEVINST';
                                    HrDescr := 'The device could not be dynamically removed.';
                                  end;
    LongInt($800F0233)          : begin
                                    HrStr := 'SPAPI_E_INVALID_TARGET';
                                    HrDescr := 'Cannot copy to specified target.';
                                  end;
    LongInt($800F0234)          : begin
                                    HrStr := 'SPAPI_E_DRIVER_NONNATIVE';
                                    HrDescr := 'Driver is not intended for this platform.';
                                  end;
    LongInt($800F0235)          : begin
                                    HrStr := 'SPAPI_E_IN_WOW64';
                                    HrDescr := 'Operation not allowed in WOW64.';
                                  end;
    LongInt($800F0236)          : begin
                                    HrStr := 'SPAPI_E_SET_SYSTEM_RESTORE_POINT';
                                    HrDescr := 'The operation involving unsigned file copying was rolled back, so that a system restore point could be set.';
                                  end;
    LongInt($800F0237)          : begin
                                    HrStr := 'SPAPI_E_INCORRECTLY_COPIED_INF';
                                    HrDescr := 'An INF was copied into the Windows INF directory in an improper manner.';
                                  end;
    LongInt($800F0238)          : begin
                                    HrStr := 'SPAPI_E_SCE_DISABLED';
                                    HrDescr := 'The Security Configuration Editor (SCE) APIs have been disabled on this embedded product.';
                                  end;
    LongInt($800F0239)          : begin
                                    HrStr := 'SPAPI_E_UNKNOWN_EXCEPTION';
                                    HrDescr := 'An unknown exception was encountered.';
                                  end;
    LongInt($800F023A)          : begin
                                    HrStr := 'SPAPI_E_PNP_REGISTRY_ERROR';
                                    HrDescr := 'A problem was encountered when accessing the Plug and Play registry database.';
                                  end;
    LongInt($800F023B)          : begin
                                    HrStr := 'SPAPI_E_REMOTE_REQUEST_UNSUPPORTED';
                                    HrDescr := 'The requested operation is not supported for a remote machine.';
                                  end;
    LongInt($800F023C)          : begin
                                    HrStr := 'SPAPI_E_NOT_AN_INSTALLED_OEM_INF';
                                    HrDescr := 'The specified file is not an installed original equipment manufacturer (OEM) INF.';
                                  end;
    LongInt($800F023D)          : begin
                                    HrStr := 'SPAPI_E_INF_IN_USE_BY_DEVICES';
                                    HrDescr := 'One or more devices are presently installed using the specified INF.';
                                  end;
    LongInt($800F023E)          : begin
                                    HrStr := 'SPAPI_E_DI_FUNCTION_OBSOLETE';
                                    HrDescr := 'The requested device install operation is obsolete.';
                                  end;
    LongInt($800F023F)          : begin
                                    HrStr := 'SPAPI_E_NO_AUTHENTICODE_CATALOG';
                                    HrDescr := 'A file could not be verified because it does not have an associated catalog signed via Authenticode.';
                                  end;
    LongInt($800F0240)          : begin
                                    HrStr := 'SPAPI_E_AUTHENTICODE_DISALLOWED';
                                    HrDescr := 'Authenticode signature verification is not supported for the specified INF.';
                                  end;
    LongInt($800F0241)          : begin
                                    HrStr := 'SPAPI_E_AUTHENTICODE_TRUSTED_PUBLISHER';
                                    HrDescr := 'The INF was signed with an Authenticode catalog from a trusted publisher.';
                                  end;
    LongInt($800F0242)          : begin
                                    HrStr := 'SPAPI_E_AUTHENTICODE_TRUST_NOT_ESTABLISHED';
                                    HrDescr := 'The publisher of an Authenticode-signed catalog has not yet been established as trusted.';
                                  end;
    LongInt($800F0243)          : begin
                                    HrStr := 'SPAPI_E_AUTHENTICODE_PUBLISHER_NOT_TRUSTED';
                                    HrDescr := 'The publisher of an Authenticode-signed catalog was not established as trusted.';
                                  end;
    LongInt($800F0244)          : begin
                                    HrStr := 'SPAPI_E_SIGNATURE_OSATTRIBUTE_MISMATCH';
                                    HrDescr := 'The software was tested for compliance with Windows logo requirements on a different version of Windows and might not be compatible with this version.';
                                  end;
    LongInt($800F0245)          : begin
                                    HrStr := 'SPAPI_E_ONLY_VALIDATE_VIA_AUTHENTICODE';
                                    HrDescr := 'The file can be validated only by a catalog signed via Authenticode.';
                                  end;
    LongInt($800F0246)          : begin
                                    HrStr := 'SPAPI_E_DEVICE_INSTALLER_NOT_READY';
                                    HrDescr := 'One of the installers for this device cannot perform the installation at this time.';
                                  end;
    LongInt($800F0247)          : begin
                                    HrStr := 'SPAPI_E_DRIVER_STORE_ADD_FAILED';
                                    HrDescr := 'A problem was encountered while attempting to add the driver to the store.';
                                  end;
    LongInt($800F0248)          : begin
                                    HrStr := 'SPAPI_E_DEVICE_INSTALL_BLOCKED';
                                    HrDescr := 'The installation of this device is forbidden by system policy.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($800F0249)          : begin
                                    HrStr := 'SPAPI_E_DRIVER_INSTALL_BLOCKED';
                                    HrDescr := 'The installation of this driver is forbidden by system policy.' +
                                               'Contact your system administrator.';
                                  end;
    LongInt($800F024A)          : begin
                                    HrStr := 'SPAPI_E_WRONG_INF_TYPE';
                                    HrDescr := 'The specified INF is the wrong type for this operation.';
                                  end;
    LongInt($800F024B)          : begin
                                    HrStr := 'SPAPI_E_FILE_HASH_NOT_IN_CATALOG';
                                    HrDescr := 'The hash for the file is not present in the specified catalog file.' +
                                               'The file is likely corrupt or the victim of tampering.';
                                  end;
    LongInt($800F024C)          : begin
                                    HrStr := 'SPAPI_E_DRIVER_STORE_DELETE_FAILED';
                                    HrDescr := 'A problem was encountered while attempting to delete the driver from the store.';
                                  end;
    LongInt($800F0300)          : begin
                                    HrStr := 'SPAPI_E_UNRECOVERABLE_STACK_OVERFLOW';
                                    HrDescr := 'An unrecoverable stack overflow was encountered.';
                                  end;
    LongInt($800F1000)          : begin
                                    HrStr := 'SPAPI_E_ERROR_NOT_INSTALLED';
                                    HrDescr := 'No installed components were detected.';
                                  end;
    LongInt($80100001)          : begin
                                    HrStr := 'SCARD_F_INTERNAL_ERROR';
                                    HrDescr := 'An internal consistency check failed.';
                                  end;
    LongInt($80100002)          : begin
                                    HrStr := 'SCARD_E_CANCELLED';
                                    HrDescr := 'The action was canceled by an SCardCancel request.';
                                  end;
    LongInt($80100003)          : begin
                                    HrStr := 'SCARD_E_INVALID_HANDLE';
                                    HrDescr := 'The supplied handle was invalid.';
                                  end;
    LongInt($80100004)          : begin
                                    HrStr := 'SCARD_E_INVALID_PARAMETER';
                                    HrDescr := 'One or more of the supplied parameters could not be properly interpreted.';
                                  end;
    LongInt($80100005)          : begin
                                    HrStr := 'SCARD_E_INVALID_TARGET';
                                    HrDescr := 'Registry startup information is missing or invalid.';
                                  end;
    LongInt($80100006)          : begin
                                    HrStr := 'SCARD_E_NO_MEMORY';
                                    HrDescr := 'Not enough memory available to complete this command.';
                                  end;
    LongInt($80100007)          : begin
                                    HrStr := 'SCARD_F_WAITED_TOO_LONG';
                                    HrDescr := 'An internal consistency timer has expired.';
                                  end;
    LongInt($80100008)          : begin
                                    HrStr := 'SCARD_E_INSUFFICIENT_BUFFER';
                                    HrDescr := 'The data buffer to receive returned data is too small for the returned data.';
                                  end;
    LongInt($80100009)          : begin
                                    HrStr := 'SCARD_E_UNKNOWN_READER';
                                    HrDescr := 'The specified reader name is not recognized.';
                                  end;
    LongInt($8010000A)          : begin
                                    HrStr := 'SCARD_E_TIMEOUT';
                                    HrDescr := 'The user-specified time-out value has expired.';
                                  end;
    LongInt($8010000B)          : begin
                                    HrStr := 'SCARD_E_SHARING_VIOLATION';
                                    HrDescr := 'The smart card cannot be accessed because of other connections outstanding.';
                                  end;
    LongInt($8010000C)          : begin
                                    HrStr := 'SCARD_E_NO_SMARTCARD';
                                    HrDescr := 'The operation requires a smart card, but no smart card is currently in the device.';
                                  end;
    LongInt($8010000D)          : begin
                                    HrStr := 'SCARD_E_UNKNOWN_CARD';
                                    HrDescr := 'The specified smart card name is not recognized.';
                                  end;
    LongInt($8010000E)          : begin
                                    HrStr := 'SCARD_E_CANT_DISPOSE';
                                    HrDescr := 'The system could not dispose of the media in the requested manner.';
                                  end;
    LongInt($8010000F)          : begin
                                    HrStr := 'SCARD_E_PROTO_MISMATCH';
                                    HrDescr := 'The requested protocols are incompatible with the protocol currently in use with the smart card.';
                                  end;
    LongInt($80100010)          : begin
                                    HrStr := 'SCARD_E_NOT_READY';
                                    HrDescr := 'The reader or smart card is not ready to accept commands.';
                                  end;
    LongInt($80100011)          : begin
                                    HrStr := 'SCARD_E_INVALID_VALUE';
                                    HrDescr := 'One or more of the supplied parameters values could not be properly interpreted.';
                                  end;
    LongInt($80100012)          : begin
                                    HrStr := 'SCARD_E_SYSTEM_CANCELLED';
                                    HrDescr := 'The action was canceled by the system, presumably to log off or shut down.';
                                  end;
    LongInt($80100013)          : begin
                                    HrStr := 'SCARD_F_COMM_ERROR';
                                    HrDescr := 'An internal communications error has been detected.';
                                  end;
    LongInt($80100014)          : begin
                                    HrStr := 'SCARD_F_UNKNOWN_ERROR';
                                    HrDescr := 'An internal error has been detected, but the source is unknown.';
                                  end;
    LongInt($80100015)          : begin
                                    HrStr := 'SCARD_E_INVALID_ATR';
                                    HrDescr := 'An automatic terminal recognition (ATR) obtained from the registry is not a valid ATR string.';
                                  end;
    LongInt($80100016)          : begin
                                    HrStr := 'SCARD_E_NOT_TRANSACTED';
                                    HrDescr := 'An attempt was made to end a nonexistent transaction.';
                                  end;
    LongInt($80100017)          : begin
                                    HrStr := 'SCARD_E_READER_UNAVAILABLE';
                                    HrDescr := 'The specified reader is not currently available for use.';
                                  end;
    LongInt($80100018)          : begin
                                    HrStr := 'SCARD_P_SHUTDOWN';
                                    HrDescr := 'The operation has been aborted to allow the server application to exit.';
                                  end;
    LongInt($80100019)          : begin
                                    HrStr := 'SCARD_E_PCI_TOO_SMALL';
                                    HrDescr := 'The peripheral component interconnect (PCI) Receive buffer was too small.';
                                  end;
    LongInt($8010001A)          : begin
                                    HrStr := 'SCARD_E_READER_UNSUPPORTED';
                                    HrDescr := 'The reader driver does not meet minimal requirements for support.';
                                  end;
    LongInt($8010001B)          : begin
                                    HrStr := 'SCARD_E_DUPLICATE_READER';
                                    HrDescr := 'The reader driver did not produce a unique reader name.';
                                  end;
    LongInt($8010001C)          : begin
                                    HrStr := 'SCARD_E_CARD_UNSUPPORTED';
                                    HrDescr := 'The smart card does not meet minimal requirements for support.';
                                  end;
    LongInt($8010001D)          : begin
                                    HrStr := 'SCARD_E_NO_SERVICE';
                                    HrDescr := 'The smart card resource manager is not running.';
                                  end;
    LongInt($8010001E)          : begin
                                    HrStr := 'SCARD_E_SERVICE_STOPPED';
                                    HrDescr := 'The smart card resource manager has shut down.';
                                  end;
    LongInt($8010001F)          : begin
                                    HrStr := 'SCARD_E_UNEXPECTED';
                                    HrDescr := 'An unexpected card error has occurred.';
                                  end;
    LongInt($80100020)          : begin
                                    HrStr := 'SCARD_E_ICC_INSTALLATION';
                                    HrDescr := 'No primary provider can be found for the smart card.';
                                  end;
    LongInt($80100021)          : begin
                                    HrStr := 'SCARD_E_ICC_CREATEORDER';
                                    HrDescr := 'The requested order of object creation is not supported.';
                                  end;
    LongInt($80100022)          : begin
                                    HrStr := 'SCARD_E_UNSUPPORTED_FEATURE';
                                    HrDescr := 'This smart card does not support the requested feature.';
                                  end;
    LongInt($80100023)          : begin
                                    HrStr := 'SCARD_E_DIR_NOT_FOUND';
                                    HrDescr := 'The identified directory does not exist in the smart card.';
                                  end;
    LongInt($80100024)          : begin
                                    HrStr := 'SCARD_E_FILE_NOT_FOUND';
                                    HrDescr := 'The identified file does not exist in the smart card.';
                                  end;
    LongInt($80100025)          : begin
                                    HrStr := 'SCARD_E_NO_DIR';
                                    HrDescr := 'The supplied path does not represent a smart card directory.';
                                  end;
    LongInt($80100026)          : begin
                                    HrStr := 'SCARD_E_NO_FILE';
                                    HrDescr := 'The supplied path does not represent a smart card file.';
                                  end;
    LongInt($80100027)          : begin
                                    HrStr := 'SCARD_E_NO_ACCESS';
                                    HrDescr := 'Access is denied to this file.';
                                  end;
    LongInt($80100028)          : begin
                                    HrStr := 'SCARD_E_WRITE_TOO_MANY';
                                    HrDescr := 'The smart card does not have enough memory to store the information.';
                                  end;
    LongInt($80100029)          : begin
                                    HrStr := 'SCARD_E_BAD_SEEK';
                                    HrDescr := 'There was an error trying to set the smart card file object pointer.';
                                  end;
    LongInt($8010002A)          : begin
                                    HrStr := 'SCARD_E_INVALID_CHV';
                                    HrDescr := 'The supplied PIN is incorrect.';
                                  end;
    LongInt($8010002B)          : begin
                                    HrStr := 'SCARD_E_UNKNOWN_RES_MNG';
                                    HrDescr := 'An unrecognized error code was returned from a layered component.';
                                  end;
    LongInt($8010002C)          : begin
                                    HrStr := 'SCARD_E_NO_SUCH_CERTIFICATE';
                                    HrDescr := 'The requested certificate does not exist.';
                                  end;
    LongInt($8010002D)          : begin
                                    HrStr := 'SCARD_E_CERTIFICATE_UNAVAILABLE';
                                    HrDescr := 'The requested certificate could not be obtained.';
                                  end;
    LongInt($8010002E)          : begin
                                    HrStr := 'SCARD_E_NO_READERS_AVAILABLE';
                                    HrDescr := 'Cannot find a smart card reader.';
                                  end;
    LongInt($8010002F)          : begin
                                    HrStr := 'SCARD_E_COMM_DATA_LOST';
                                    HrDescr := 'A communications error with the smart card has been detected.' +
                                               'Retry the operation.';
                                  end;
    LongInt($80100030)          : begin
                                    HrStr := 'SCARD_E_NO_KEY_CONTAINER';
                                    HrDescr := 'The requested key container does not exist on the smart card.';
                                  end;
    LongInt($80100031)          : begin
                                    HrStr := 'SCARD_E_SERVER_TOO_BUSY';
                                    HrDescr := 'The smart card resource manager is too busy to complete this operation.';
                                  end;
    LongInt($80100065)          : begin
                                    HrStr := 'SCARD_W_UNSUPPORTED_CARD';
                                    HrDescr := 'The reader cannot communicate with the smart card, due to ATR configuration conflicts.';
                                  end;
    LongInt($80100066)          : begin
                                    HrStr := 'SCARD_W_UNRESPONSIVE_CARD';
                                    HrDescr := 'The smart card is not responding to a reset.';
                                  end;
    LongInt($80100067)          : begin
                                    HrStr := 'SCARD_W_UNPOWERED_CARD';
                                    HrDescr := 'Power has been removed from the smart card, so that further communication is not possible.';
                                  end;
    LongInt($80100068)          : begin
                                    HrStr := 'SCARD_W_RESET_CARD';
                                    HrDescr := 'The smart card has been reset, so any shared state information is invalid.';
                                  end;
    LongInt($80100069)          : begin
                                    HrStr := 'SCARD_W_REMOVED_CARD';
                                    HrDescr := 'The smart card has been removed, so that further communication is not possible.';
                                  end;
    LongInt($8010006A)          : begin
                                    HrStr := 'SCARD_W_SECURITY_VIOLATION';
                                    HrDescr := 'Access was denied because of a security violation.';
                                  end;
    LongInt($8010006B)          : begin
                                    HrStr := 'SCARD_W_WRONG_CHV';
                                    HrDescr := 'The card cannot be accessed because the wrong PIN was presented.';
                                  end;
    LongInt($8010006C)          : begin
                                    HrStr := 'SCARD_W_CHV_BLOCKED';
                                    HrDescr := 'The card cannot be accessed because the maximum number of PIN entry attempts has been reached.';
                                  end;
    LongInt($8010006D)          : begin
                                    HrStr := 'SCARD_W_EOF';
                                    HrDescr := 'The end of the smart card file has been reached.';
                                  end;
    LongInt($8010006E)          : begin
                                    HrStr := 'SCARD_W_CANCELLED_BY_USER';
                                    HrDescr := 'The action was canceled by the user.';
                                  end;
    LongInt($8010006F)          : begin
                                    HrStr := 'SCARD_W_CARD_NOT_AUTHENTICATED';
                                    HrDescr := 'No PIN was presented to the smart card.';
                                  end;
    LongInt($80110401)          : begin
                                    HrStr := 'COMADMIN_E_OBJECTERRORS';
                                    HrDescr := 'Errors occurred accessing one or more objects—the ErrorInfo collection contains more detail.';
                                  end;
    LongInt($80110402)          : begin
                                    HrStr := 'COMADMIN_E_OBJECTINVALID';
                                    HrDescr := 'One or more of the object''s properties are missing or invalid.';
                                  end;
    LongInt($80110403)          : begin
                                    HrStr := 'COMADMIN_E_KEYMISSING';
                                    HrDescr := 'The object was not found in the catalog.';
                                  end;
    LongInt($80110404)          : begin
                                    HrStr := 'COMADMIN_E_ALREADYINSTALLED';
                                    HrDescr := 'The object is already registered.';
                                  end;
    LongInt($80110407)          : begin
                                    HrStr := 'COMADMIN_E_APP_FILE_WRITEFAIL';
                                    HrDescr := 'An error occurred writing to the application file.';
                                  end;
    LongInt($80110408)          : begin
                                    HrStr := 'COMADMIN_E_APP_FILE_READFAIL';
                                    HrDescr := 'An error occurred reading the application file.';
                                  end;
    LongInt($80110409)          : begin
                                    HrStr := 'COMADMIN_E_APP_FILE_VERSION';
                                    HrDescr := 'Invalid version number in application file.';
                                  end;
    LongInt($8011040A)          : begin
                                    HrStr := 'COMADMIN_E_BADPATH';
                                    HrDescr := 'The file path is invalid.';
                                  end;
    LongInt($8011040B)          : begin
                                    HrStr := 'COMADMIN_E_APPLICATIONEXISTS';
                                    HrDescr := 'The application is already installed.';
                                  end;
    LongInt($8011040C)          : begin
                                    HrStr := 'COMADMIN_E_ROLEEXISTS';
                                    HrDescr := 'The role already exists.';
                                  end;
    LongInt($8011040D)          : begin
                                    HrStr := 'COMADMIN_E_CANTCOPYFILE';
                                    HrDescr := 'An error occurred copying the file.';
                                  end;
    LongInt($8011040F)          : begin
                                    HrStr := 'COMADMIN_E_NOUSER';
                                    HrDescr := 'One or more users are not valid.';
                                  end;
    LongInt($80110410)          : begin
                                    HrStr := 'COMADMIN_E_INVALIDUSERIDS';
                                    HrDescr := 'One or more users in the application file are not valid.';
                                  end;
    LongInt($80110411)          : begin
                                    HrStr := 'COMADMIN_E_NOREGISTRYCLSID';
                                    HrDescr := 'The component''s CLSID is missing or corrupt.';
                                  end;
    LongInt($80110412)          : begin
                                    HrStr := 'COMADMIN_E_BADREGISTRYPROGID';
                                    HrDescr := 'The component''s programmatic ID is missing or corrupt.';
                                  end;
    LongInt($80110413)          : begin
                                    HrStr := 'COMADMIN_E_AUTHENTICATIONLEVEL';
                                    HrDescr := 'Unable to set required authentication level for update request.';
                                  end;
    LongInt($80110414)          : begin
                                    HrStr := 'COMADMIN_E_USERPASSWDNOTVALID';
                                    HrDescr := 'The identity or password set on the application is not valid.';
                                  end;
    LongInt($80110418)          : begin
                                    HrStr := 'COMADMIN_E_CLSIDORIIDMISMATCH';
                                    HrDescr := 'Application file CLSIDs or instance identifiers (IIDs) do not match corresponding DLLs.';
                                  end;
    LongInt($80110419)          : begin
                                    HrStr := 'COMADMIN_E_REMOTEINTERFACE';
                                    HrDescr := 'Interface information is either missing or changed.';
                                  end;
    LongInt($8011041A)          : begin
                                    HrStr := 'COMADMIN_E_DLLREGISTERSERVER';
                                    HrDescr := 'DllRegisterServer failed on component install.';
                                  end;
    LongInt($8011041B)          : begin
                                    HrStr := 'COMADMIN_E_NOSERVERSHARE';
                                    HrDescr := 'No server file share available.';
                                  end;
    LongInt($8011041D)          : begin
                                    HrStr := 'COMADMIN_E_DLLLOADFAILED';
                                    HrDescr := 'DLL could not be loaded.';
                                  end;
    LongInt($8011041E)          : begin
                                    HrStr := 'COMADMIN_E_BADREGISTRYLIBID';
                                    HrDescr := 'The registered TypeLib ID is not valid.';
                                  end;
    LongInt($8011041F)          : begin
                                    HrStr := 'COMADMIN_E_APPDIRNOTFOUND';
                                    HrDescr := 'Application install directory not found.';
                                  end;
    LongInt($80110423)          : begin
                                    HrStr := 'COMADMIN_E_REGISTRARFAILED';
                                    HrDescr := 'Errors occurred while in the component registrar.';
                                  end;
    LongInt($80110424)          : begin
                                    HrStr := 'COMADMIN_E_COMPFILE_DOESNOTEXIST';
                                    HrDescr := 'The file does not exist.';
                                  end;
    LongInt($80110425)          : begin
                                    HrStr := 'COMADMIN_E_COMPFILE_LOADDLLFAIL';
                                    HrDescr := 'The DLL could not be loaded.';
                                  end;
    LongInt($80110426)          : begin
                                    HrStr := 'COMADMIN_E_COMPFILE_GETCLASSOBJ';
                                    HrDescr := 'GetClassObject failed in the DLL.';
                                  end;
    LongInt($80110427)          : begin
                                    HrStr := 'COMADMIN_E_COMPFILE_CLASSNOTAVAIL';
                                    HrDescr := 'The DLL does not support the components listed in the TypeLib.';
                                  end;
    LongInt($80110428)          : begin
                                    HrStr := 'COMADMIN_E_COMPFILE_BADTLB';
                                    HrDescr := 'The TypeLib could not be loaded.';
                                  end;
    LongInt($80110429)          : begin
                                    HrStr := 'COMADMIN_E_COMPFILE_NOTINSTALLABLE';
                                    HrDescr := 'The file does not contain components or component information.';
                                  end;
    LongInt($8011042A)          : begin
                                    HrStr := 'COMADMIN_E_NOTCHANGEABLE';
                                    HrDescr := 'Changes to this object and its subobjects have been disabled.';
                                  end;
    LongInt($8011042B)          : begin
                                    HrStr := 'COMADMIN_E_NOTDELETEABLE';
                                    HrDescr := 'The delete function has been disabled for this object.';
                                  end;
    LongInt($8011042C)          : begin
                                    HrStr := 'COMADMIN_E_SESSION';
                                    HrDescr := 'The server catalog version is not supported.';
                                  end;
    LongInt($8011042D)          : begin
                                    HrStr := 'COMADMIN_E_COMP_MOVE_LOCKED';
                                    HrDescr := 'The component move was disallowed because the source or destination application is either a system application or currently locked against changes.';
                                  end;
    LongInt($8011042E)          : begin
                                    HrStr := 'COMADMIN_E_COMP_MOVE_BAD_DEST';
                                    HrDescr := 'The component move failed because the destination application no longer exists.';
                                  end;
    LongInt($80110430)          : begin
                                    HrStr := 'COMADMIN_E_REGISTERTLB';
                                    HrDescr := 'The system was unable to register the TypeLib.';
                                  end;
    LongInt($80110433)          : begin
                                    HrStr := 'COMADMIN_E_SYSTEMAPP';
                                    HrDescr := 'This operation cannot be performed on the system application.';
                                  end;
    LongInt($80110434)          : begin
                                    HrStr := 'COMADMIN_E_COMPFILE_NOREGISTRAR';
                                    HrDescr := 'The component registrar referenced in this file is not available.';
                                  end;
    LongInt($80110435)          : begin
                                    HrStr := 'COMADMIN_E_COREQCOMPINSTALLED';
                                    HrDescr := 'A component in the same DLL is already installed.';
                                  end;
    LongInt($80110436)          : begin
                                    HrStr := 'COMADMIN_E_SERVICENOTINSTALLED';
                                    HrDescr := 'The service is not installed.';
                                  end;
    LongInt($80110437)          : begin
                                    HrStr := 'COMADMIN_E_PROPERTYSAVEFAILED';
                                    HrDescr := 'One or more property settings are either invalid or in conflict with each other.';
                                  end;
    LongInt($80110438)          : begin
                                    HrStr := 'COMADMIN_E_OBJECTEXISTS';
                                    HrDescr := 'The object you are attempting to add or rename already exists.';
                                  end;
    LongInt($80110439)          : begin
                                    HrStr := 'COMADMIN_E_COMPONENTEXISTS';
                                    HrDescr := 'The component already exists.';
                                  end;
    LongInt($8011043B)          : begin
                                    HrStr := 'COMADMIN_E_REGFILE_CORRUPT';
                                    HrDescr := 'The registration file is corrupt.';
                                  end;
    LongInt($8011043C)          : begin
                                    HrStr := 'COMADMIN_E_PROPERTY_OVERFLOW';
                                    HrDescr := 'The property value is too large.';
                                  end;
    LongInt($8011043E)          : begin
                                    HrStr := 'COMADMIN_E_NOTINREGISTRY';
                                    HrDescr := 'Object was not found in registry.';
                                  end;
    LongInt($8011043F)          : begin
                                    HrStr := 'COMADMIN_E_OBJECTNOTPOOLABLE';
                                    HrDescr := 'This object cannot be pooled.';
                                  end;
    LongInt($80110446)          : begin
                                    HrStr := 'COMADMIN_E_APPLID_MATCHES_CLSID';
                                    HrDescr := 'A CLSID with the same GUID as the new application ID is already installed on this machine.';
                                  end;
    LongInt($80110447)          : begin
                                    HrStr := 'COMADMIN_E_ROLE_DOES_NOT_EXIST';
                                    HrDescr := 'A role assigned to a component, interface, or method did not exist in the application.';
                                  end;
    LongInt($80110448)          : begin
                                    HrStr := 'COMADMIN_E_START_APP_NEEDS_COMPONENTS';
                                    HrDescr := 'You must have components in an application to start the application.';
                                  end;
    LongInt($80110449)          : begin
                                    HrStr := 'COMADMIN_E_REQUIRES_DIFFERENT_PLATFORM';
                                    HrDescr := 'This operation is not enabled on this platform.';
                                  end;
    LongInt($8011044A)          : begin
                                    HrStr := 'COMADMIN_E_CAN_NOT_EXPORT_APP_PROXY';
                                    HrDescr := 'Application proxy is not exportable.';
                                  end;
    LongInt($8011044B)          : begin
                                    HrStr := 'COMADMIN_E_CAN_NOT_START_APP';
                                    HrDescr := 'Failed to start application because it is either a library application or an application proxy.';
                                  end;
    LongInt($8011044C)          : begin
                                    HrStr := 'COMADMIN_E_CAN_NOT_EXPORT_SYS_APP';
                                    HrDescr := 'System application is not exportable.';
                                  end;
    LongInt($8011044D)          : begin
                                    HrStr := 'COMADMIN_E_CANT_SUBSCRIBE_TO_COMPONENT';
                                    HrDescr := 'Cannot subscribe to this component (the component might have been imported).';
                                  end;
    LongInt($8011044E)          : begin
                                    HrStr := 'COMADMIN_E_EVENTCLASS_CANT_BE_SUBSCRIBER';
                                    HrDescr := 'An event class cannot also be a subscriber component.';
                                  end;
    LongInt($8011044F)          : begin
                                    HrStr := 'COMADMIN_E_LIB_APP_PROXY_INCOMPATIBLE';
                                    HrDescr := 'Library applications and application proxies are incompatible.';
                                  end;
    LongInt($80110450)          : begin
                                    HrStr := 'COMADMIN_E_BASE_PARTITION_ONLY';
                                    HrDescr := 'This function is valid for the base partition only.';
                                  end;
    LongInt($80110451)          : begin
                                    HrStr := 'COMADMIN_E_START_APP_DISABLED';
                                    HrDescr := 'You cannot start an application that has been disabled.';
                                  end;
    LongInt($80110457)          : begin
                                    HrStr := 'COMADMIN_E_CAT_DUPLICATE_PARTITION_NAME';
                                    HrDescr := 'The specified partition name is already in use on this computer.';
                                  end;
    LongInt($80110458)          : begin
                                    HrStr := 'COMADMIN_E_CAT_INVALID_PARTITION_NAME';
                                    HrDescr := 'The specified partition name is invalid.' +
                                               'Check that the name contains at least one visible character.';
                                  end;
    LongInt($80110459)          : begin
                                    HrStr := 'COMADMIN_E_CAT_PARTITION_IN_USE';
                                    HrDescr := 'The partition cannot be deleted because it is the default partition for one or more users.';
                                  end;
    LongInt($8011045A)          : begin
                                    HrStr := 'COMADMIN_E_FILE_PARTITION_DUPLICATE_FILES';
                                    HrDescr := 'The partition cannot be exported because one or more components in the partition have the same file name.';
                                  end;
    LongInt($8011045B)          : begin
                                    HrStr := 'COMADMIN_E_CAT_IMPORTED_COMPONENTS_NOT_ALLOWED';
                                    HrDescr := 'Applications that contain one or more imported components cannot be installed into a nonbase partition.';
                                  end;
    LongInt($8011045C)          : begin
                                    HrStr := 'COMADMIN_E_AMBIGUOUS_APPLICATION_NAME';
                                    HrDescr := 'The application name is not unique and cannot be resolved to an application ID.';
                                  end;
    LongInt($8011045D)          : begin
                                    HrStr := 'COMADMIN_E_AMBIGUOUS_PARTITION_NAME';
                                    HrDescr := 'The partition name is not unique and cannot be resolved to a partition ID.';
                                  end;
    LongInt($80110472)          : begin
                                    HrStr := 'COMADMIN_E_REGDB_NOTINITIALIZED';
                                    HrDescr := 'The COM+ registry database has not been initialized.';
                                  end;
    LongInt($80110473)          : begin
                                    HrStr := 'COMADMIN_E_REGDB_NOTOPEN';
                                    HrDescr := 'The COM+ registry database is not open.';
                                  end;
    LongInt($80110474)          : begin
                                    HrStr := 'COMADMIN_E_REGDB_SYSTEMERR';
                                    HrDescr := 'The COM+ registry database detected a system error.';
                                  end;
    LongInt($80110475)          : begin
                                    HrStr := 'COMADMIN_E_REGDB_ALREADYRUNNING';
                                    HrDescr := 'The COM+ registry database is already running.';
                                  end;
    LongInt($80110480)          : begin
                                    HrStr := 'COMADMIN_E_MIG_VERSIONNOTSUPPORTED';
                                    HrDescr := 'This version of the COM+ registry database cannot be migrated.';
                                  end;
    LongInt($80110481)          : begin
                                    HrStr := 'COMADMIN_E_MIG_SCHEMANOTFOUND';
                                    HrDescr := 'The schema version to be migrated could not be found in the COM+ registry database.';
                                  end;
    LongInt($80110482)          : begin
                                    HrStr := 'COMADMIN_E_CAT_BITNESSMISMATCH';
                                    HrDescr := 'There was a type mismatch between binaries.';
                                  end;
    LongInt($80110483)          : begin
                                    HrStr := 'COMADMIN_E_CAT_UNACCEPTABLEBITNESS';
                                    HrDescr := 'A binary of unknown or invalid type was provided.';
                                  end;
    LongInt($80110484)          : begin
                                    HrStr := 'COMADMIN_E_CAT_WRONGAPPBITNESS';
                                    HrDescr := 'There was a type mismatch between a binary and an application.';
                                  end;
    LongInt($80110485)          : begin
                                    HrStr := 'COMADMIN_E_CAT_PAUSE_RESUME_NOT_SUPPORTED';
                                    HrDescr := 'The application cannot be paused or resumed.';
                                  end;
    LongInt($80110486)          : begin
                                    HrStr := 'COMADMIN_E_CAT_SERVERFAULT';
                                    HrDescr := 'The COM+ catalog server threw an exception during execution.';
                                  end;
    LongInt($80110600)          : begin
                                    HrStr := 'COMQC_E_APPLICATION_NOT_QUEUED';
                                    HrDescr := 'Only COM+ applications marked "queued" can be invoked using the "queue" moniker.';
                                  end;
    LongInt($80110601)          : begin
                                    HrStr := 'COMQC_E_NO_QUEUEABLE_INTERFACES';
                                    HrDescr := 'At least one interface must be marked "queued" to create a queued component instance with the "queue" moniker.';
                                  end;
    LongInt($80110602)          : begin
                                    HrStr := 'COMQC_E_QUEUING_SERVICE_NOT_AVAILABLE';
                                    HrDescr := 'Message Queuing is required for the requested operation and is not installed.';
                                  end;
    LongInt($80110603)          : begin
                                    HrStr := 'COMQC_E_NO_IPERSISTSTREAM';
                                    HrDescr := 'Unable to marshal an interface that does not support IPersistStream.';
                                  end;
    LongInt($80110604)          : begin
                                    HrStr := 'COMQC_E_BAD_MESSAGE';
                                    HrDescr := 'The message is improperly formatted or was damaged in transit.';
                                  end;
    LongInt($80110605)          : begin
                                    HrStr := 'COMQC_E_UNAUTHENTICATED';
                                    HrDescr := 'An unauthenticated message was received by an application that accepts only authenticated messages.';
                                  end;
    LongInt($80110606)          : begin
                                    HrStr := 'COMQC_E_UNTRUSTED_ENQUEUER';
                                    HrDescr := 'The message was requeued or moved by a user not in the QC Trusted User "role".';
                                  end;
    LongInt($80110701)          : begin
                                    HrStr := 'MSDTC_E_DUPLICATE_RESOURCE';
                                    HrDescr := 'Cannot create a duplicate resource of type Distributed Transaction Coordinator.';
                                  end;
    LongInt($80110808)          : begin
                                    HrStr := 'COMADMIN_E_OBJECT_PARENT_MISSING';
                                    HrDescr := 'One of the objects being inserted or updated does not belong to a valid parent collection.';
                                  end;
    LongInt($80110809)          : begin
                                    HrStr := 'COMADMIN_E_OBJECT_DOES_NOT_EXIST';
                                    HrDescr := 'One of the specified objects cannot be found.';
                                  end;
    LongInt($8011080A)          : begin
                                    HrStr := 'COMADMIN_E_APP_NOT_RUNNING';
                                    HrDescr := 'The specified application is not currently running.';
                                  end;
    LongInt($8011080B)          : begin
                                    HrStr := 'COMADMIN_E_INVALID_PARTITION';
                                    HrDescr := 'The partitions specified are not valid.';
                                  end;
    LongInt($8011080D)          : begin
                                    HrStr := 'COMADMIN_E_SVCAPP_NOT_POOLABLE_OR_RECYCLABLE';
                                    HrDescr := 'COM+ applications that run as Windows NT service cannot be pooled or recycled.';
                                  end;
    LongInt($8011080E)          : begin
                                    HrStr := 'COMADMIN_E_USER_IN_SET';
                                    HrDescr := 'One or more users are already assigned to a local partition set.';
                                  end;
    LongInt($8011080F)          : begin
                                    HrStr := 'COMADMIN_E_CANTRECYCLELIBRARYAPPS';
                                    HrDescr := 'Library applications cannot be recycled.';
                                  end;
    LongInt($80110811)          : begin
                                    HrStr := 'COMADMIN_E_CANTRECYCLESERVICEAPPS';
                                    HrDescr := 'Applications running as Windows NT services cannot be recycled.';
                                  end;
    LongInt($80110812)          : begin
                                    HrStr := 'COMADMIN_E_PROCESSALREADYRECYCLED';
                                    HrDescr := 'The process has already been recycled.';
                                  end;
    LongInt($80110813)          : begin
                                    HrStr := 'COMADMIN_E_PAUSEDPROCESSMAYNOTBERECYCLED';
                                    HrDescr := 'A paused process cannot be recycled.';
                                  end;
    LongInt($80110814)          : begin
                                    HrStr := 'COMADMIN_E_CANTMAKEINPROCSERVICE';
                                    HrDescr := 'Library applications cannot be Windows NT services.';
                                  end;
    LongInt($80110815)          : begin
                                    HrStr := 'COMADMIN_E_PROGIDINUSEBYCLSID';
                                    HrDescr := 'The ProgID provided to the copy operation is invalid.' +
                                               'The ProgID is in use by another registered CLSID.';
                                  end;
    LongInt($80110816)          : begin
                                    HrStr := 'COMADMIN_E_DEFAULT_PARTITION_NOT_IN_SET';
                                    HrDescr := 'The partition specified as the default is not a member of the partition set.';
                                  end;
    LongInt($80110817)          : begin
                                    HrStr := 'COMADMIN_E_RECYCLEDPROCESSMAYNOTBEPAUSED';
                                    HrDescr := 'A recycled process cannot be paused.';
                                  end;
    LongInt($80110818)          : begin
                                    HrStr := 'COMADMIN_E_PARTITION_ACCESSDENIED';
                                    HrDescr := 'Access to the specified partition is denied.';
                                  end;
    LongInt($80110819)          : begin
                                    HrStr := 'COMADMIN_E_PARTITION_MSI_ONLY';
                                    HrDescr := 'Only application files (*.msi files) can be installed into partitions.';
                                  end;
    LongInt($8011081A)          : begin
                                    HrStr := 'COMADMIN_E_LEGACYCOMPS_NOT_ALLOWED_IN_1_0_FORMAT';
                                    HrDescr := 'Applications containing one or more legacy components cannot be exported to 1.0 format.';
                                  end;
    LongInt($8011081B)          : begin
                                    HrStr := 'COMADMIN_E_LEGACYCOMPS_NOT_ALLOWED_IN_NONBASE_PARTITIONS';
                                    HrDescr := 'Legacy components cannot exist in nonbase partitions.';
                                  end;
    LongInt($8011081C)          : begin
                                    HrStr := 'COMADMIN_E_COMP_MOVE_SOURCE';
                                    HrDescr := 'A component cannot be moved (or copied) from the System Application, an application proxy, or a nonchangeable application.';
                                  end;
    LongInt($8011081D)          : begin
                                    HrStr := 'COMADMIN_E_COMP_MOVE_DEST';
                                    HrDescr := 'A component cannot be moved (or copied) to the System Application, an application proxy or a nonchangeable application.';
                                  end;
    LongInt($8011081E)          : begin
                                    HrStr := 'COMADMIN_E_COMP_MOVE_PRIVATE';
                                    HrDescr := 'A private component cannot be moved (or copied) to a library application or to the base partition.';
                                  end;
    LongInt($8011081F)          : begin
                                    HrStr := 'COMADMIN_E_BASEPARTITION_REQUIRED_IN_SET';
                                    HrDescr := 'The Base Application Partition exists in all partition sets and cannot be removed.';
                                  end;
    LongInt($80110820)          : begin
                                    HrStr := 'COMADMIN_E_CANNOT_ALIAS_EVENTCLASS';
                                    HrDescr := 'Alas, Event Class components cannot be aliased.';
                                  end;
    LongInt($80110821)          : begin
                                    HrStr := 'COMADMIN_E_PRIVATE_ACCESSDENIED';
                                    HrDescr := 'Access is denied because the component is private.';
                                  end;
    LongInt($80110822)          : begin
                                    HrStr := 'COMADMIN_E_SAFERINVALID';
                                    HrDescr := 'The specified SAFER level is invalid.';
                                  end;
    LongInt($80110823)          : begin
                                    HrStr := 'COMADMIN_E_REGISTRY_ACCESSDENIED';
                                    HrDescr := 'The specified user cannot write to the system registry.';
                                  end;
    LongInt($80110824)          : begin
                                    HrStr := 'COMADMIN_E_PARTITIONS_DISABLED';
                                    HrDescr := 'COM+ partitions are currently disabled.';
                                  end;
    LongInt($801F0001)          : begin
                                    HrStr := 'ERROR_FLT_NO_HANDLER_DEFINED';
                                    HrDescr := 'A handler was not defined by the filter for this operation.';
                                  end;
    LongInt($801F0002)          : begin
                                    HrStr := 'ERROR_FLT_CONTEXT_ALREADY_DEFINED';
                                    HrDescr := 'A context is already defined for this object.';
                                  end;
    LongInt($801F0003)          : begin
                                    HrStr := 'ERROR_FLT_INVALID_ASYNCHRONOUS_REQUEST';
                                    HrDescr := 'Asynchronous requests are not valid for this operation.';
                                  end;
    LongInt($801F0004)          : begin
                                    HrStr := 'ERROR_FLT_DISALLOW_FAST_IO';
                                    HrDescr := 'Disallow the Fast IO path for this operation.';
                                  end;
    LongInt($801F0005)          : begin
                                    HrStr := 'ERROR_FLT_INVALID_NAME_REQUEST';
                                    HrDescr := 'An invalid name request was made.' +
                                               'The name requested cannot be retrieved at this time.';
                                  end;
    LongInt($801F0006)          : begin
                                    HrStr := 'ERROR_FLT_NOT_SAFE_TO_POST_OPERATION';
                                    HrDescr := 'Posting this operation to a worker thread for further processing is not safe at this time because it could lead to a system deadlock.';
                                  end;
    LongInt($801F0007)          : begin
                                    HrStr := 'ERROR_FLT_NOT_INITIALIZED';
                                    HrDescr := 'The Filter Manager was not initialized when a filter tried to register.' +
                                               'Be sure that the Filter Manager is being loaded as a driver.';
                                  end;
    LongInt($801F0008)          : begin
                                    HrStr := 'ERROR_FLT_FILTER_NOT_READY';
                                    HrDescr := 'The filter is not ready for attachment to volumes because it has not finished initializing (FltStartFiltering has not been called).';
                                  end;
    LongInt($801F0009)          : begin
                                    HrStr := 'ERROR_FLT_POST_OPERATION_CLEANUP';
                                    HrDescr := 'The filter must clean up any operation-specific context at this time because it is being removed from the system before the operation is completed by the lower drivers.';
                                  end;
    LongInt($801F000A)          : begin
                                    HrStr := 'ERROR_FLT_INTERNAL_ERROR';
                                    HrDescr := 'The Filter Manager had an internal error from which it cannot recover; therefore, the operation has been failed.' +
                                               'This is usually the result of a filter returning an invalid value from a preoperation callback.';
                                  end;
    LongInt($801F000B)          : begin
                                    HrStr := 'ERROR_FLT_DELETING_OBJECT';
                                    HrDescr := 'The object specified for this action is in the process of being deleted; therefore, the action requested cannot be completed at this time.';
                                  end;
    LongInt($801F000C)          : begin
                                    HrStr := 'ERROR_FLT_MUST_BE_NONPAGED_POOL';
                                    HrDescr := 'Nonpaged pool must be used for this type of context.';
                                  end;
    LongInt($801F000D)          : begin
                                    HrStr := 'ERROR_FLT_DUPLICATE_ENTRY';
                                    HrDescr := 'A duplicate handler definition has been provided for an operation.';
                                  end;
    LongInt($801F000E)          : begin
                                    HrStr := 'ERROR_FLT_CBDQ_DISABLED';
                                    HrDescr := 'The callback data queue has been disabled.';
                                  end;
    LongInt($801F000F)          : begin
                                    HrStr := 'ERROR_FLT_DO_NOT_ATTACH';
                                    HrDescr := 'Do not attach the filter to the volume at this time.';
                                  end;
    LongInt($801F0010)          : begin
                                    HrStr := 'ERROR_FLT_DO_NOT_DETACH';
                                    HrDescr := 'Do not detach the filter from the volume at this time.';
                                  end;
    LongInt($801F0011)          : begin
                                    HrStr := 'ERROR_FLT_INSTANCE_ALTITUDE_COLLISION';
                                    HrDescr := 'An instance already exists at this altitude on the volume specified.';
                                  end;
    LongInt($801F0012)          : begin
                                    HrStr := 'ERROR_FLT_INSTANCE_NAME_COLLISION';
                                    HrDescr := 'An instance already exists with this name on the volume specified.';
                                  end;
    LongInt($801F0013)          : begin
                                    HrStr := 'ERROR_FLT_FILTER_NOT_FOUND';
                                    HrDescr := 'The system could not find the filter specified.';
                                  end;
    LongInt($801F0014)          : begin
                                    HrStr := 'ERROR_FLT_VOLUME_NOT_FOUND';
                                    HrDescr := 'The system could not find the volume specified.';
                                  end;
    LongInt($801F0015)          : begin
                                    HrStr := 'ERROR_FLT_INSTANCE_NOT_FOUND';
                                    HrDescr := 'The system could not find the instance specified.';
                                  end;
    LongInt($801F0016)          : begin
                                    HrStr := 'ERROR_FLT_CONTEXT_ALLOCATION_NOT_FOUND';
                                    HrDescr := 'No registered context allocation definition was found for the given request.';
                                  end;
    LongInt($801F0017)          : begin
                                    HrStr := 'ERROR_FLT_INVALID_CONTEXT_REGISTRATION';
                                    HrDescr := 'An invalid parameter was specified during context registration.';
                                  end;
    LongInt($801F0018)          : begin
                                    HrStr := 'ERROR_FLT_NAME_CACHE_MISS';
                                    HrDescr := 'The name requested was not found in the Filter Manager name cache and could not be retrieved from the file system.';
                                  end;
    LongInt($801F0019)          : begin
                                    HrStr := 'ERROR_FLT_NO_DEVICE_OBJECT';
                                    HrDescr := 'The requested device object does not exist for the given volume.';
                                  end;
    LongInt($801F001A)          : begin
                                    HrStr := 'ERROR_FLT_VOLUME_ALREADY_MOUNTED';
                                    HrDescr := 'The specified volume is already mounted.';
                                  end;
    LongInt($801F001B)          : begin
                                    HrStr := 'ERROR_FLT_ALREADY_ENLISTED';
                                    HrDescr := 'The specified Transaction Context is already enlisted in a transaction.';
                                  end;
    LongInt($801F001C)          : begin
                                    HrStr := 'ERROR_FLT_CONTEXT_ALREADY_LINKED';
                                    HrDescr := 'The specified context is already attached to another object.';
                                  end;
    LongInt($801F0020)          : begin
                                    HrStr := 'ERROR_FLT_NO_WAITER_FOR_REPLY';
                                    HrDescr := 'No waiter is present for the filter''s reply to this message.';
                                  end;
    LongInt($80260001)          : begin
                                    HrStr := 'ERROR_HUNG_DISPLAY_DRIVER_THREAD';
                                    HrDescr := '{Display Driver Stopped Responding} The %hs display driver has stopped working normally.' +
                                               'Save your work and reboot the system to restore full display functionality.' +
                                               'The next time you reboot the machine a dialog will be displayed giving you a chance to report this failure to Microsoft.';
                                  end;
    LongInt($80261001)          : begin
                                    HrStr := 'ERROR_MONITOR_NO_DESCRIPTOR';
                                    HrDescr := 'Monitor descriptor could not be obtained.';
                                  end;
    LongInt($80261002)          : begin
                                    HrStr := 'ERROR_MONITOR_UNKNOWN_DESCRIPTOR_FORMAT';
                                    HrDescr := 'Format of the obtained monitor descriptor is not supported by this release.';
                                  end;
    LongInt($80263001)          : begin
                                    HrStr := 'DWM_E_COMPOSITIONDISABLED';
                                    HrDescr := '{Desktop Composition is Disabled} The operation could not be completed because desktop composition is disabled.';
                                  end;
    LongInt($80263002)          : begin
                                    HrStr := 'DWM_E_REMOTING_NOT_SUPPORTED';
                                    HrDescr := '{Some Desktop Composition APIs Are Not Supported While Remoting} Some desktop composition APIs are not supported while remoting.' +
                                               'The operation is not supported while running in a remote session.';
                                  end;
    LongInt($80263003)          : begin
                                    HrStr := 'DWM_E_NO_REDIRECTION_SURFACE_AVAILABLE';
                                    HrDescr := '{No DWM Redirection Surface is Available} The Desktop Window Manager (DWM) was unable to provide a redirection surface to complete the DirectX present.';
                                  end;
    LongInt($80263004)          : begin
                                    HrStr := 'DWM_E_NOT_QUEUING_PRESENTS';
                                    HrDescr := '{DWM Is Not Queuing Presents for the Specified Window} The window specified is not currently using queued presents.';
                                  end;
    LongInt($80280000)          : begin
                                    HrStr := 'TPM_E_ERROR_MASK';
                                    HrDescr := 'This is an error mask to convert Trusted Platform Module (TPM) hardware errors to Win32 errors.';
                                  end;
    LongInt($80280001)          : begin
                                    HrStr := 'TPM_E_AUTHFAIL';
                                    HrDescr := 'Authentication failed.';
                                  end;
    LongInt($80280002)          : begin
                                    HrStr := 'TPM_E_BADINDEX';
                                    HrDescr := 'The index to a Platform Configuration Register (PCR), DIR, or other register is incorrect.';
                                  end;
    LongInt($80280003)          : begin
                                    HrStr := 'TPM_E_BAD_PARAMETER';
                                    HrDescr := 'One or more parameters are bad.';
                                  end;
    LongInt($80280004)          : begin
                                    HrStr := 'TPM_E_AUDITFAILURE';
                                    HrDescr := 'An operation completed successfully but the auditing of that operation failed.';
                                  end;
    LongInt($80280005)          : begin
                                    HrStr := 'TPM_E_CLEAR_DISABLED';
                                    HrDescr := 'The clear disable flag is set and all clear operations now require physical access.';
                                  end;
    LongInt($80280006)          : begin
                                    HrStr := 'TPM_E_DEACTIVATED';
                                    HrDescr := 'The TPM is deactivated.';
                                  end;
    LongInt($80280007)          : begin
                                    HrStr := 'TPM_E_DISABLED';
                                    HrDescr := 'The TPM is disabled.';
                                  end;
    LongInt($80280008)          : begin
                                    HrStr := 'TPM_E_DISABLED_CMD';
                                    HrDescr := 'The target command has been disabled.';
                                  end;
    LongInt($80280009)          : begin
                                    HrStr := 'TPM_E_FAIL';
                                    HrDescr := 'The operation failed.';
                                  end;
    LongInt($8028000A)          : begin
                                    HrStr := 'TPM_E_BAD_ORDINAL';
                                    HrDescr := 'The ordinal was unknown or inconsistent.';
                                  end;
    LongInt($8028000B)          : begin
                                    HrStr := 'TPM_E_INSTALL_DISABLED';
                                    HrDescr := 'The ability to install an owner is disabled.';
                                  end;
    LongInt($8028000C)          : begin
                                    HrStr := 'TPM_E_INVALID_KEYHANDLE';
                                    HrDescr := 'The key handle cannot be interpreted.';
                                  end;
    LongInt($8028000D)          : begin
                                    HrStr := 'TPM_E_KEYNOTFOUND';
                                    HrDescr := 'The key handle points to an invalid key.';
                                  end;
    LongInt($8028000E)          : begin
                                    HrStr := 'TPM_E_INAPPROPRIATE_ENC';
                                    HrDescr := 'Unacceptable encryption scheme.';
                                  end;
    LongInt($8028000F)          : begin
                                    HrStr := 'TPM_E_MIGRATEFAIL';
                                    HrDescr := 'Migration authorization failed.';
                                  end;
    LongInt($80280010)          : begin
                                    HrStr := 'TPM_E_INVALID_PCR_INFO';
                                    HrDescr := 'PCR information could not be interpreted.';
                                  end;
    LongInt($80280011)          : begin
                                    HrStr := 'TPM_E_NOSPACE';
                                    HrDescr := 'No room to load key.';
                                  end;
    LongInt($80280012)          : begin
                                    HrStr := 'TPM_E_NOSRK';
                                    HrDescr := 'There is no storage root key (SRK) set.';
                                  end;
    LongInt($80280013)          : begin
                                    HrStr := 'TPM_E_NOTSEALED_BLOB';
                                    HrDescr := 'An encrypted blob is invalid or was not created by this TPM.';
                                  end;
    LongInt($80280014)          : begin
                                    HrStr := 'TPM_E_OWNER_SET';
                                    HrDescr := 'There is already an owner.';
                                  end;
    LongInt($80280015)          : begin
                                    HrStr := 'TPM_E_RESOURCES';
                                    HrDescr := 'The TPM has insufficient internal resources to perform the requested action.';
                                  end;
    LongInt($80280016)          : begin
                                    HrStr := 'TPM_E_SHORTRANDOM';
                                    HrDescr := 'A random string was too short.';
                                  end;
    LongInt($80280017)          : begin
                                    HrStr := 'TPM_E_SIZE';
                                    HrDescr := 'The TPM does not have the space to perform the operation.';
                                  end;
    LongInt($80280018)          : begin
                                    HrStr := 'TPM_E_WRONGPCRVAL';
                                    HrDescr := 'The named PCR value does not match the current PCR value.';
                                  end;
    LongInt($80280019)          : begin
                                    HrStr := 'TPM_E_BAD_PARAM_SIZE';
                                    HrDescr := 'The paramSize argument to the command has the incorrect value.';
                                  end;
    LongInt($8028001A)          : begin
                                    HrStr := 'TPM_E_SHA_THREAD';
                                    HrDescr := 'There is no existing SHA-1 thread.';
                                  end;
    LongInt($8028001B)          : begin
                                    HrStr := 'TPM_E_SHA_ERROR';
                                    HrDescr := 'The calculation is unable to proceed because the existing SHA-1 thread has already encountered an error.';
                                  end;
    LongInt($8028001C)          : begin
                                    HrStr := 'TPM_E_FAILEDSELFTEST';
                                    HrDescr := 'Self-test has failed and the TPM has shut down.';
                                  end;
    LongInt($8028001D)          : begin
                                    HrStr := 'TPM_E_AUTH2FAIL';
                                    HrDescr := 'The authorization for the second key in a two-key function failed authorization.';
                                  end;
    LongInt($8028001E)          : begin
                                    HrStr := 'TPM_E_BADTAG';
                                    HrDescr := 'The tag value sent to for a command is invalid.';
                                  end;
    LongInt($8028001F)          : begin
                                    HrStr := 'TPM_E_IOERROR';
                                    HrDescr := 'An I/O error occurred transmitting information to the TPM.';
                                  end;
    LongInt($80280020)          : begin
                                    HrStr := 'TPM_E_ENCRYPT_ERROR';
                                    HrDescr := 'The encryption process had a problem.';
                                  end;
    LongInt($80280021)          : begin
                                    HrStr := 'TPM_E_DECRYPT_ERROR';
                                    HrDescr := 'The decryption process did not complete.';
                                  end;
    LongInt($80280022)          : begin
                                    HrStr := 'TPM_E_INVALID_AUTHHANDLE';
                                    HrDescr := 'An invalid handle was used.';
                                  end;
    LongInt($80280023)          : begin
                                    HrStr := 'TPM_E_NO_ENDORSEMENT';
                                    HrDescr := 'The TPM does not have an endorsement key (EK) installed.';
                                  end;
    LongInt($80280024)          : begin
                                    HrStr := 'TPM_E_INVALID_KEYUSAGE';
                                    HrDescr := 'The usage of a key is not allowed.';
                                  end;
    LongInt($80280025)          : begin
                                    HrStr := 'TPM_E_WRONG_ENTITYTYPE';
                                    HrDescr := 'The submitted entity type is not allowed.';
                                  end;
    LongInt($80280026)          : begin
                                    HrStr := 'TPM_E_INVALID_POSTINIT';
                                    HrDescr := 'The command was received in the wrong sequence relative to TPM_Init and a subsequent TPM_Startup.';
                                  end;
    LongInt($80280027)          : begin
                                    HrStr := 'TPM_E_INAPPROPRIATE_SIG';
                                    HrDescr := 'Signed data cannot include additional DER information.';
                                  end;
    LongInt($80280028)          : begin
                                    HrStr := 'TPM_E_BAD_KEY_PROPERTY';
                                    HrDescr := 'The key properties in TPM_KEY_PARMs are not supported by this TPM.';
                                  end;
    LongInt($80280029)          : begin
                                    HrStr := 'TPM_E_BAD_MIGRATION';
                                    HrDescr := 'The migration properties of this key are incorrect.';
                                  end;
    LongInt($8028002A)          : begin
                                    HrStr := 'TPM_E_BAD_SCHEME';
                                    HrDescr := 'The signature or encryption scheme for this key is incorrect or not permitted in this situation.';
                                  end;
    LongInt($8028002B)          : begin
                                    HrStr := 'TPM_E_BAD_DATASIZE';
                                    HrDescr := 'The size of the data (or blob) parameter is bad or inconsistent with the referenced key.';
                                  end;
    LongInt($8028002C)          : begin
                                    HrStr := 'TPM_E_BAD_MODE';
                                    HrDescr := 'A mode parameter is bad, such as capArea or subCapArea for TPM_GetCapability, physicalPresence parameter for TPM_PhysicalPresence, or migrationType for TPM_CreateMigrationBlob.';
                                  end;
    LongInt($8028002D)          : begin
                                    HrStr := 'TPM_E_BAD_PRESENCE';
                                    HrDescr := 'Either the physicalPresence or physicalPresenceLock bits have the wrong value.';
                                  end;
    LongInt($8028002E)          : begin
                                    HrStr := 'TPM_E_BAD_VERSION';
                                    HrDescr := 'The TPM cannot perform this version of the capability.';
                                  end;
    LongInt($8028002F)          : begin
                                    HrStr := 'TPM_E_NO_WRAP_TRANSPORT';
                                    HrDescr := 'The TPM does not allow for wrapped transport sessions.';
                                  end;
    LongInt($80280030)          : begin
                                    HrStr := 'TPM_E_AUDITFAIL_UNSUCCESSFUL';
                                    HrDescr := 'TPM audit construction failed and the underlying command was returning a failure code also.';
                                  end;
    LongInt($80280031)          : begin
                                    HrStr := 'TPM_E_AUDITFAIL_SUCCESSFUL';
                                    HrDescr := 'TPM audit construction failed and the underlying command was returning success.';
                                  end;
    LongInt($80280032)          : begin
                                    HrStr := 'TPM_E_NOTRESETABLE';
                                    HrDescr := 'Attempt to reset a PCR that does not have the resettable attribute.';
                                  end;
    LongInt($80280033)          : begin
                                    HrStr := 'TPM_E_NOTLOCAL';
                                    HrDescr := 'Attempt to reset a PCR register that requires locality and the locality modifier not part of command transport.';
                                  end;
    LongInt($80280034)          : begin
                                    HrStr := 'TPM_E_BAD_TYPE';
                                    HrDescr := 'Make identity blob not properly typed.';
                                  end;
    LongInt($80280035)          : begin
                                    HrStr := 'TPM_E_INVALID_RESOURCE';
                                    HrDescr := 'When saving context identified resource type does not match actual resource.';
                                  end;
    LongInt($80280036)          : begin
                                    HrStr := 'TPM_E_NOTFIPS';
                                    HrDescr := 'The TPM is attempting to execute a command only available when in Federal Information Processing Standards (FIPS) mode.';
                                  end;
    LongInt($80280037)          : begin
                                    HrStr := 'TPM_E_INVALID_FAMILY';
                                    HrDescr := 'The command is attempting to use an invalid family ID.';
                                  end;
    LongInt($80280038)          : begin
                                    HrStr := 'TPM_E_NO_NV_PERMISSION';
                                    HrDescr := 'The permission to manipulate the NV storage is not available.';
                                  end;
    LongInt($80280039)          : begin
                                    HrStr := 'TPM_E_REQUIRES_SIGN';
                                    HrDescr := 'The operation requires a signed command.';
                                  end;
    LongInt($8028003A)          : begin
                                    HrStr := 'TPM_E_KEY_NOTSUPPORTED';
                                    HrDescr := 'Wrong operation to load an NV key.';
                                  end;
    LongInt($8028003B)          : begin
                                    HrStr := 'TPM_E_AUTH_CONFLICT';
                                    HrDescr := 'NV_LoadKey blob requires both owner and blob authorization.';
                                  end;
    LongInt($8028003C)          : begin
                                    HrStr := 'TPM_E_AREA_LOCKED';
                                    HrDescr := 'The NV area is locked and not writable.';
                                  end;
    LongInt($8028003D)          : begin
                                    HrStr := 'TPM_E_BAD_LOCALITY';
                                    HrDescr := 'The locality is incorrect for the attempted operation.';
                                  end;
    LongInt($8028003E)          : begin
                                    HrStr := 'TPM_E_READ_ONLY';
                                    HrDescr := 'The NV area is read-only and cannot be written to.';
                                  end;
    LongInt($8028003F)          : begin
                                    HrStr := 'TPM_E_PER_NOWRITE';
                                    HrDescr := 'There is no protection on the write to the NV area.';
                                  end;
    LongInt($80280040)          : begin
                                    HrStr := 'TPM_E_FAMILYCOUNT';
                                    HrDescr := 'The family count value does not match.';
                                  end;
    LongInt($80280041)          : begin
                                    HrStr := 'TPM_E_WRITE_LOCKED';
                                    HrDescr := 'The NV area has already been written to.';
                                  end;
    LongInt($80280042)          : begin
                                    HrStr := 'TPM_E_BAD_ATTRIBUTES';
                                    HrDescr := 'The NV area attributes conflict.';
                                  end;
    LongInt($80280043)          : begin
                                    HrStr := 'TPM_E_INVALID_STRUCTURE';
                                    HrDescr := 'The structure tag and version are invalid or inconsistent.';
                                  end;
    LongInt($80280044)          : begin
                                    HrStr := 'TPM_E_KEY_OWNER_CONTROL';
                                    HrDescr := 'The key is under control of the TPM owner and can only be evicted by the TPM owner.';
                                  end;
    LongInt($80280045)          : begin
                                    HrStr := 'TPM_E_BAD_COUNTER';
                                    HrDescr := 'The counter handle is incorrect.';
                                  end;
    LongInt($80280046)          : begin
                                    HrStr := 'TPM_E_NOT_FULLWRITE';
                                    HrDescr := 'The write is not a complete write of the area.';
                                  end;
    LongInt($80280047)          : begin
                                    HrStr := 'TPM_E_CONTEXT_GAP';
                                    HrDescr := 'The gap between saved context counts is too large.';
                                  end;
    LongInt($80280048)          : begin
                                    HrStr := 'TPM_E_MAXNVWRITES';
                                    HrDescr := 'The maximum number of NV writes without an owner has been exceeded.';
                                  end;
    LongInt($80280049)          : begin
                                    HrStr := 'TPM_E_NOOPERATOR';
                                    HrDescr := 'No operator AuthData value is set.';
                                  end;
    LongInt($8028004A)          : begin
                                    HrStr := 'TPM_E_RESOURCEMISSING';
                                    HrDescr := 'The resource pointed to by context is not loaded.';
                                  end;
    LongInt($8028004B)          : begin
                                    HrStr := 'TPM_E_DELEGATE_LOCK';
                                    HrDescr := 'The delegate administration is locked.';
                                  end;
    LongInt($8028004C)          : begin
                                    HrStr := 'TPM_E_DELEGATE_FAMILY';
                                    HrDescr := 'Attempt to manage a family other then the delegated family.';
                                  end;
    LongInt($8028004D)          : begin
                                    HrStr := 'TPM_E_DELEGATE_ADMIN';
                                    HrDescr := 'Delegation table management not enabled.';
                                  end;
    LongInt($8028004E)          : begin
                                    HrStr := 'TPM_E_TRANSPORT_NOTEXCLUSIVE';
                                    HrDescr := 'There was a command executed outside an exclusive transport session.';
                                  end;
    LongInt($8028004F)          : begin
                                    HrStr := 'TPM_E_OWNER_CONTROL';
                                    HrDescr := 'Attempt to context save an owner evict controlled key.';
                                  end;
    LongInt($80280050)          : begin
                                    HrStr := 'TPM_E_DAA_RESOURCES';
                                    HrDescr := 'The DAA command has no resources available to execute the command.';
                                  end;
    LongInt($80280051)          : begin
                                    HrStr := 'TPM_E_DAA_INPUT_DATA0';
                                    HrDescr := 'The consistency check on DAA parameter inputData0 has failed.';
                                  end;
    LongInt($80280052)          : begin
                                    HrStr := 'TPM_E_DAA_INPUT_DATA1';
                                    HrDescr := 'The consistency check on DAA parameter inputData1 has failed.';
                                  end;
    LongInt($80280053)          : begin
                                    HrStr := 'TPM_E_DAA_ISSUER_SETTINGS';
                                    HrDescr := 'The consistency check on DAA_issuerSettings has failed.';
                                  end;
    LongInt($80280054)          : begin
                                    HrStr := 'TPM_E_DAA_TPM_SETTINGS';
                                    HrDescr := 'The consistency check on DAA_tpmSpecific has failed.';
                                  end;
    LongInt($80280055)          : begin
                                    HrStr := 'TPM_E_DAA_STAGE';
                                    HrDescr := 'The atomic process indicated by the submitted DAA command is not the expected process.';
                                  end;
    LongInt($80280056)          : begin
                                    HrStr := 'TPM_E_DAA_ISSUER_VALIDITY';
                                    HrDescr := 'The issuer''s validity check has detected an inconsistency.';
                                  end;
    LongInt($80280057)          : begin
                                    HrStr := 'TPM_E_DAA_WRONG_W';
                                    HrDescr := 'The consistency check on w has failed.';
                                  end;
    LongInt($80280058)          : begin
                                    HrStr := 'TPM_E_BAD_HANDLE';
                                    HrDescr := 'The handle is incorrect.';
                                  end;
    LongInt($80280059)          : begin
                                    HrStr := 'TPM_E_BAD_DELEGATE';
                                    HrDescr := 'Delegation is not correct.';
                                  end;
    LongInt($8028005A)          : begin
                                    HrStr := 'TPM_E_BADCONTEXT';
                                    HrDescr := 'The context blob is invalid.';
                                  end;
    LongInt($8028005B)          : begin
                                    HrStr := 'TPM_E_TOOMANYCONTEXTS';
                                    HrDescr := 'Too many contexts held by the TPM.';
                                  end;
    LongInt($8028005C)          : begin
                                    HrStr := 'TPM_E_MA_TICKET_SIGNATURE';
                                    HrDescr := 'Migration authority signature validation failure.';
                                  end;
    LongInt($8028005D)          : begin
                                    HrStr := 'TPM_E_MA_DESTINATION';
                                    HrDescr := 'Migration destination not authenticated.';
                                  end;
    LongInt($8028005E)          : begin
                                    HrStr := 'TPM_E_MA_SOURCE';
                                    HrDescr := 'Migration source incorrect.';
                                  end;
    LongInt($8028005F)          : begin
                                    HrStr := 'TPM_E_MA_AUTHORITY';
                                    HrDescr := 'Incorrect migration authority.';
                                  end;
    LongInt($80280061)          : begin
                                    HrStr := 'TPM_E_PERMANENTEK';
                                    HrDescr := 'Attempt to revoke the EK and the EK is not revocable.';
                                  end;
    LongInt($80280062)          : begin
                                    HrStr := 'TPM_E_BAD_SIGNATURE';
                                    HrDescr := 'Bad signature of CMK ticket.';
                                  end;
    LongInt($80280063)          : begin
                                    HrStr := 'TPM_E_NOCONTEXTSPACE';
                                    HrDescr := 'There is no room in the context list for additional contexts.';
                                  end;
    LongInt($80280400)          : begin
                                    HrStr := 'TPM_E_COMMAND_BLOCKED';
                                    HrDescr := 'The command was blocked.';
                                  end;
    LongInt($80280401)          : begin
                                    HrStr := 'TPM_E_INVALID_HANDLE';
                                    HrDescr := 'The specified handle was not found.';
                                  end;
    LongInt($80280402)          : begin
                                    HrStr := 'TPM_E_DUPLICATE_VHANDLE';
                                    HrDescr := 'The TPM returned a duplicate handle and the command needs to be resubmitted.';
                                  end;
    LongInt($80280403)          : begin
                                    HrStr := 'TPM_E_EMBEDDED_COMMAND_BLOCKED';
                                    HrDescr := 'The command within the transport was blocked.';
                                  end;
    LongInt($80280404)          : begin
                                    HrStr := 'TPM_E_EMBEDDED_COMMAND_UNSUPPORTED';
                                    HrDescr := 'The command within the transport is not supported.';
                                  end;
    LongInt($80280800)          : begin
                                    HrStr := 'TPM_E_RETRY';
                                    HrDescr := 'The TPM is too busy to respond to the command immediately, but the command could be resubmitted at a later time.';
                                  end;
    LongInt($80280801)          : begin
                                    HrStr := 'TPM_E_NEEDS_SELFTEST';
                                    HrDescr := 'SelfTestFull has not been run.';
                                  end;
    LongInt($80280802)          : begin
                                    HrStr := 'TPM_E_DOING_SELFTEST';
                                    HrDescr := 'The TPM is currently executing a full self-test.';
                                  end;
    LongInt($80280803)          : begin
                                    HrStr := 'TPM_E_DEFEND_LOCK_RUNNING';
                                    HrDescr := 'The TPM is defending against dictionary attacks and is in a time-out period.';
                                  end;
    LongInt($80284001)          : begin
                                    HrStr := 'TBS_E_INTERNAL_ERROR';
                                    HrDescr := 'An internal software error has been detected.';
                                  end;
    LongInt($80284002)          : begin
                                    HrStr := 'TBS_E_BAD_PARAMETER';
                                    HrDescr := 'One or more input parameters are bad.';
                                  end;
    LongInt($80284003)          : begin
                                    HrStr := 'TBS_E_INVALID_OUTPUT_POINTER';
                                    HrDescr := 'A specified output pointer is bad.';
                                  end;
    LongInt($80284004)          : begin
                                    HrStr := 'TBS_E_INVALID_CONTEXT';
                                    HrDescr := 'The specified context handle does not refer to a valid context.';
                                  end;
    LongInt($80284005)          : begin
                                    HrStr := 'TBS_E_INSUFFICIENT_BUFFER';
                                    HrDescr := 'A specified output buffer is too small.';
                                  end;
    LongInt($80284006)          : begin
                                    HrStr := 'TBS_E_IOERROR';
                                    HrDescr := 'An error occurred while communicating with the TPM.';
                                  end;
    LongInt($80284007)          : begin
                                    HrStr := 'TBS_E_INVALID_CONTEXT_PARAM';
                                    HrDescr := 'One or more context parameters are invalid.';
                                  end;
    LongInt($80284008)          : begin
                                    HrStr := 'TBS_E_SERVICE_NOT_RUNNING';
                                    HrDescr := 'The TPM Base Services (TBS) is not running and could not be started.';
                                  end;
    LongInt($80284009)          : begin
                                    HrStr := 'TBS_E_TOO_MANY_TBS_CONTEXTS';
                                    HrDescr := 'A new context could not be created because there are too many open contexts.';
                                  end;
    LongInt($8028400A)          : begin
                                    HrStr := 'TBS_E_TOO_MANY_RESOURCES';
                                    HrDescr := 'A new virtual resource could not be created because there are too many open virtual resources.';
                                  end;
    LongInt($8028400B)          : begin
                                    HrStr := 'TBS_E_SERVICE_START_PENDING';
                                    HrDescr := 'The TBS service has been started but is not yet running.';
                                  end;
    LongInt($8028400C)          : begin
                                    HrStr := 'TBS_E_PPI_NOT_SUPPORTED';
                                    HrDescr := 'The physical presence interface is not supported.';
                                  end;
    LongInt($8028400D)          : begin
                                    HrStr := 'TBS_E_COMMAND_CANCELED';
                                    HrDescr := 'The command was canceled.';
                                  end;
    LongInt($8028400E)          : begin
                                    HrStr := 'TBS_E_BUFFER_TOO_LARGE';
                                    HrDescr := 'The input or output buffer is too large.';
                                  end;
    LongInt($80290100)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_STATE';
                                    HrDescr := 'The command buffer is not in the correct state.';
                                  end;
    LongInt($80290101)          : begin
                                    HrStr := 'TPMAPI_E_NOT_ENOUGH_DATA';
                                    HrDescr := 'The command buffer does not contain enough data to satisfy the request.';
                                  end;
    LongInt($80290102)          : begin
                                    HrStr := 'TPMAPI_E_TOO_MUCH_DATA';
                                    HrDescr := 'The command buffer cannot contain any more data.';
                                  end;
    LongInt($80290103)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_OUTPUT_POINTER';
                                    HrDescr := 'One or more output parameters was null or invalid.';
                                  end;
    LongInt($80290104)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_PARAMETER';
                                    HrDescr := 'One or more input parameters are invalid.';
                                  end;
    LongInt($80290105)          : begin
                                    HrStr := 'TPMAPI_E_OUT_OF_MEMORY';
                                    HrDescr := 'Not enough memory was available to satisfy the request.';
                                  end;
    LongInt($80290106)          : begin
                                    HrStr := 'TPMAPI_E_BUFFER_TOO_SMALL';
                                    HrDescr := 'The specified buffer was too small.';
                                  end;
    LongInt($80290107)          : begin
                                    HrStr := 'TPMAPI_E_INTERNAL_ERROR';
                                    HrDescr := 'An internal error was detected.';
                                  end;
    LongInt($80290108)          : begin
                                    HrStr := 'TPMAPI_E_ACCESS_DENIED';
                                    HrDescr := 'The caller does not have the appropriate rights to perform the requested operation.';
                                  end;
    LongInt($80290109)          : begin
                                    HrStr := 'TPMAPI_E_AUTHORIZATION_FAILED';
                                    HrDescr := 'The specified authorization information was invalid.';
                                  end;
    LongInt($8029010A)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_CONTEXT_HANDLE';
                                    HrDescr := 'The specified context handle was not valid.';
                                  end;
    LongInt($8029010B)          : begin
                                    HrStr := 'TPMAPI_E_TBS_COMMUNICATION_ERROR';
                                    HrDescr := 'An error occurred while communicating with the TBS.';
                                  end;
    LongInt($8029010C)          : begin
                                    HrStr := 'TPMAPI_E_TPM_COMMAND_ERROR';
                                    HrDescr := 'The TPM returned an unexpected result.';
                                  end;
    LongInt($8029010D)          : begin
                                    HrStr := 'TPMAPI_E_MESSAGE_TOO_LARGE';
                                    HrDescr := 'The message was too large for the encoding scheme.';
                                  end;
    LongInt($8029010E)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_ENCODING';
                                    HrDescr := 'The encoding in the binary large object (BLOB) was not recognized.';
                                  end;
    LongInt($8029010F)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_KEY_SIZE';
                                    HrDescr := 'The key size is not valid.';
                                  end;
    LongInt($80290110)          : begin
                                    HrStr := 'TPMAPI_E_ENCRYPTION_FAILED';
                                    HrDescr := 'The encryption operation failed.';
                                  end;
    LongInt($80290111)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_KEY_PARAMS';
                                    HrDescr := 'The key parameters structure was not valid.';
                                  end;
    LongInt($80290112)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_MIGRATION_AUTHORIZATION_BLOB';
                                    HrDescr := 'The requested supplied data does not appear to be a valid migration authorization BLOB.';
                                  end;
    LongInt($80290113)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_PCR_INDEX';
                                    HrDescr := 'The specified PCR index was invalid.';
                                  end;
    LongInt($80290114)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_DELEGATE_BLOB';
                                    HrDescr := 'The data given does not appear to be a valid delegate BLOB.';
                                  end;
    LongInt($80290115)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_CONTEXT_PARAMS';
                                    HrDescr := 'One or more of the specified context parameters was not valid.';
                                  end;
    LongInt($80290116)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_KEY_BLOB';
                                    HrDescr := 'The data given does not appear to be a valid key BLOB.';
                                  end;
    LongInt($80290117)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_PCR_DATA';
                                    HrDescr := 'The specified PCR data was invalid.';
                                  end;
    LongInt($80290118)          : begin
                                    HrStr := 'TPMAPI_E_INVALID_OWNER_AUTH';
                                    HrDescr := 'The format of the owner authorization data was invalid.';
                                  end;
    LongInt($80290200)          : begin
                                    HrStr := 'TBSIMP_E_BUFFER_TOO_SMALL';
                                    HrDescr := 'The specified buffer was too small.';
                                  end;
    LongInt($80290201)          : begin
                                    HrStr := 'TBSIMP_E_CLEANUP_FAILED';
                                    HrDescr := 'The context could not be cleaned up.';
                                  end;
    LongInt($80290202)          : begin
                                    HrStr := 'TBSIMP_E_INVALID_CONTEXT_HANDLE';
                                    HrDescr := 'The specified context handle is invalid.';
                                  end;
    LongInt($80290203)          : begin
                                    HrStr := 'TBSIMP_E_INVALID_CONTEXT_PARAM';
                                    HrDescr := 'An invalid context parameter was specified.';
                                  end;
    LongInt($80290204)          : begin
                                    HrStr := 'TBSIMP_E_TPM_ERROR';
                                    HrDescr := 'An error occurred while communicating with the TPM.';
                                  end;
    LongInt($80290205)          : begin
                                    HrStr := 'TBSIMP_E_HASH_BAD_KEY';
                                    HrDescr := 'No entry with the specified key was found.';
                                  end;
    LongInt($80290206)          : begin
                                    HrStr := 'TBSIMP_E_DUPLICATE_VHANDLE';
                                    HrDescr := 'The specified virtual handle matches a virtual handle already in use.';
                                  end;
    LongInt($80290207)          : begin
                                    HrStr := 'TBSIMP_E_INVALID_OUTPUT_POINTER';
                                    HrDescr := 'The pointer to the returned handle location was null or invalid.';
                                  end;
    LongInt($80290208)          : begin
                                    HrStr := 'TBSIMP_E_INVALID_PARAMETER';
                                    HrDescr := 'One or more parameters are invalid.';
                                  end;
    LongInt($80290209)          : begin
                                    HrStr := 'TBSIMP_E_RPC_INIT_FAILED';
                                    HrDescr := 'The RPC subsystem could not be initialized.';
                                  end;
    LongInt($8029020A)          : begin
                                    HrStr := 'TBSIMP_E_SCHEDULER_NOT_RUNNING';
                                    HrDescr := 'The TBS scheduler is not running.';
                                  end;
    LongInt($8029020B)          : begin
                                    HrStr := 'TBSIMP_E_COMMAND_CANCELED';
                                    HrDescr := 'The command was canceled.';
                                  end;
    LongInt($8029020C)          : begin
                                    HrStr := 'TBSIMP_E_OUT_OF_MEMORY';
                                    HrDescr := 'There was not enough memory to fulfill the request.';
                                  end;
    LongInt($8029020D)          : begin
                                    HrStr := 'TBSIMP_E_LIST_NO_MORE_ITEMS';
                                    HrDescr := 'The specified list is empty, or the iteration has reached the end of the list.';
                                  end;
    LongInt($8029020E)          : begin
                                    HrStr := 'TBSIMP_E_LIST_NOT_FOUND';
                                    HrDescr := 'The specified item was not found in the list.';
                                  end;
    LongInt($8029020F)          : begin
                                    HrStr := 'TBSIMP_E_NOT_ENOUGH_SPACE';
                                    HrDescr := 'The TPM does not have enough space to load the requested resource.';
                                  end;
    LongInt($80290210)          : begin
                                    HrStr := 'TBSIMP_E_NOT_ENOUGH_TPM_CONTEXTS';
                                    HrDescr := 'There are too many TPM contexts in use.';
                                  end;
    LongInt($80290211)          : begin
                                    HrStr := 'TBSIMP_E_COMMAND_FAILED';
                                    HrDescr := 'The TPM command failed.';
                                  end;
    LongInt($80290212)          : begin
                                    HrStr := 'TBSIMP_E_UNKNOWN_ORDINAL';
                                    HrDescr := 'The TBS does not recognize the specified ordinal.';
                                  end;
    LongInt($80290213)          : begin
                                    HrStr := 'TBSIMP_E_RESOURCE_EXPIRED';
                                    HrDescr := 'The requested resource is no longer available.';
                                  end;
    LongInt($80290214)          : begin
                                    HrStr := 'TBSIMP_E_INVALID_RESOURCE';
                                    HrDescr := 'The resource type did not match.';
                                  end;
    LongInt($80290215)          : begin
                                    HrStr := 'TBSIMP_E_NOTHING_TO_UNLOAD';
                                    HrDescr := 'No resources can be unloaded.';
                                  end;
    LongInt($80290216)          : begin
                                    HrStr := 'TBSIMP_E_HASH_TABLE_FULL';
                                    HrDescr := 'No new entries can be added to the hash table.';
                                  end;
    LongInt($80290217)          : begin
                                    HrStr := 'TBSIMP_E_TOO_MANY_TBS_CONTEXTS';
                                    HrDescr := 'A new TBS context could not be created because there are too many open contexts.';
                                  end;
    LongInt($80290218)          : begin
                                    HrStr := 'TBSIMP_E_TOO_MANY_RESOURCES';
                                    HrDescr := 'A new virtual resource could not be created because there are too many open virtual resources.';
                                  end;
    LongInt($80290219)          : begin
                                    HrStr := 'TBSIMP_E_PPI_NOT_SUPPORTED';
                                    HrDescr := 'The physical presence interface is not supported.';
                                  end;
    LongInt($8029021A)          : begin
                                    HrStr := 'TBSIMP_E_TPM_INCOMPATIBLE';
                                    HrDescr := 'TBS is not compatible with the version of TPM found on the system.';
                                  end;
    LongInt($80290300)          : begin
                                    HrStr := 'TPM_E_PPI_ACPI_FAILURE';
                                    HrDescr := 'A general error was detected when attempting to acquire the BIOS response to a physical presence command.';
                                  end;
    LongInt($80290301)          : begin
                                    HrStr := 'TPM_E_PPI_USER_ABORT';
                                    HrDescr := 'The user failed to confirm the TPM operation request.';
                                  end;
    LongInt($80290302)          : begin
                                    HrStr := 'TPM_E_PPI_BIOS_FAILURE';
                                    HrDescr := 'The BIOS failure prevented the successful execution of the requested TPM operation (for example, invalid TPM operation request, BIOS communication error with the TPM).';
                                  end;
    LongInt($80290303)          : begin
                                    HrStr := 'TPM_E_PPI_NOT_SUPPORTED';
                                    HrDescr := 'The BIOS does not support the physical presence interface.';
                                  end;
    LongInt($80300002)          : begin
                                    HrStr := 'PLA_E_DCS_NOT_FOUND';
                                    HrDescr := 'A Data Collector Set was not found.';
                                  end;
    LongInt($80300045)          : begin
                                    HrStr := 'PLA_E_TOO_MANY_FOLDERS';
                                    HrDescr := 'Unable to start Data Collector Set because there are too many folders.';
                                  end;
    LongInt($80300070)          : begin
                                    HrStr := 'PLA_E_NO_MIN_DISK';
                                    HrDescr := 'Not enough free disk space to start Data Collector Set.';
                                  end;
    LongInt($803000AA)          : begin
                                    HrStr := 'PLA_E_DCS_IN_USE';
                                    HrDescr := 'Data Collector Set is in use.';
                                  end;
    LongInt($803000B7)          : begin
                                    HrStr := 'PLA_E_DCS_ALREADY_EXISTS';
                                    HrDescr := 'Data Collector Set already exists.';
                                  end;
    LongInt($80300101)          : begin
                                    HrStr := 'PLA_E_PROPERTY_CONFLICT';
                                    HrDescr := 'Property value conflict.';
                                  end;
    LongInt($80300102)          : begin
                                    HrStr := 'PLA_E_DCS_SINGLETON_REQUIRED';
                                    HrDescr := 'The current configuration for this Data Collector Set requires that it contain exactly one Data Collector.';
                                  end;
    LongInt($80300103)          : begin
                                    HrStr := 'PLA_E_CREDENTIALS_REQUIRED';
                                    HrDescr := 'A user account is required to commit the current Data Collector Set properties.';
                                  end;
    LongInt($80300104)          : begin
                                    HrStr := 'PLA_E_DCS_NOT_RUNNING';
                                    HrDescr := 'Data Collector Set is not running.';
                                  end;
    LongInt($80300105)          : begin
                                    HrStr := 'PLA_E_CONFLICT_INCL_EXCL_API';
                                    HrDescr := 'A conflict was detected in the list of include and exclude APIs.' +
                                               'Do not specify the same API in both the include list and the exclude list.';
                                  end;
    LongInt($80300106)          : begin
                                    HrStr := 'PLA_E_NETWORK_EXE_NOT_VALID';
                                    HrDescr := 'The executable path specified refers to a network share or UNC path.';
                                  end;
    LongInt($80300107)          : begin
                                    HrStr := 'PLA_E_EXE_ALREADY_CONFIGURED';
                                    HrDescr := 'The executable path specified is already configured for API tracing.';
                                  end;
    LongInt($80300108)          : begin
                                    HrStr := 'PLA_E_EXE_PATH_NOT_VALID';
                                    HrDescr := 'The executable path specified does not exist.' +
                                               'Verify that the specified path is correct.';
                                  end;
    LongInt($80300109)          : begin
                                    HrStr := 'PLA_E_DC_ALREADY_EXISTS';
                                    HrDescr := 'Data Collector already exists.';
                                  end;
    LongInt($8030010A)          : begin
                                    HrStr := 'PLA_E_DCS_START_WAIT_TIMEOUT';
                                    HrDescr := 'The wait for the Data Collector Set start notification has timed out.';
                                  end;
    LongInt($8030010B)          : begin
                                    HrStr := 'PLA_E_DC_START_WAIT_TIMEOUT';
                                    HrDescr := 'The wait for the Data Collector to start has timed out.';
                                  end;
    LongInt($8030010C)          : begin
                                    HrStr := 'PLA_E_REPORT_WAIT_TIMEOUT';
                                    HrDescr := 'The wait for the report generation tool to finish has timed out.';
                                  end;
    LongInt($8030010D)          : begin
                                    HrStr := 'PLA_E_NO_DUPLICATES';
                                    HrDescr := 'Duplicate items are not allowed.';
                                  end;
    LongInt($8030010E)          : begin
                                    HrStr := 'PLA_E_EXE_FULL_PATH_REQUIRED';
                                    HrDescr := 'When specifying the executable to trace, you must specify a full path to the executable and not just a file name.';
                                  end;
    LongInt($8030010F)          : begin
                                    HrStr := 'PLA_E_INVALID_SESSION_NAME';
                                    HrDescr := 'The session name provided is invalid.';
                                  end;
    LongInt($80300110)          : begin
                                    HrStr := 'PLA_E_PLA_CHANNEL_NOT_ENABLED';
                                    HrDescr := 'The Event Log channel Microsoft-Windows-Diagnosis-PLA/Operational must be enabled to perform this operation.';
                                  end;
    LongInt($80300111)          : begin
                                    HrStr := 'PLA_E_TASKSCHED_CHANNEL_NOT_ENABLED';
                                    HrDescr := 'The Event Log channel Microsoft-Windows-TaskScheduler must be enabled to perform this operation.';
                                  end;
    LongInt($80310000)          : begin
                                    HrStr := 'FVE_E_LOCKED_VOLUME';
                                    HrDescr := 'The volume must be unlocked before it can be used.';
                                  end;
    LongInt($80310001)          : begin
                                    HrStr := 'FVE_E_NOT_ENCRYPTED';
                                    HrDescr := 'The volume is fully decrypted and no key is available.';
                                  end;
    LongInt($80310002)          : begin
                                    HrStr := 'FVE_E_NO_TPM_BIOS';
                                    HrDescr := 'The firmware does not support using a TPM during boot.';
                                  end;
    LongInt($80310003)          : begin
                                    HrStr := 'FVE_E_NO_MBR_METRIC';
                                    HrDescr := 'The firmware does not use a TPM to perform initial program load (IPL) measurement.';
                                  end;
    LongInt($80310004)          : begin
                                    HrStr := 'FVE_E_NO_BOOTSECTOR_METRIC';
                                    HrDescr := 'The master boot record (MBR) is not TPM-aware.';
                                  end;
    LongInt($80310005)          : begin
                                    HrStr := 'FVE_E_NO_BOOTMGR_METRIC';
                                    HrDescr := 'The BOOTMGR is not being measured by the TPM.';
                                  end;
    LongInt($80310006)          : begin
                                    HrStr := 'FVE_E_WRONG_BOOTMGR';
                                    HrDescr := 'The BOOTMGR component does not perform expected TPM measurements.';
                                  end;
    LongInt($80310007)          : begin
                                    HrStr := 'FVE_E_SECURE_KEY_REQUIRED';
                                    HrDescr := 'No secure key protection mechanism has been defined.';
                                  end;
    LongInt($80310008)          : begin
                                    HrStr := 'FVE_E_NOT_ACTIVATED';
                                    HrDescr := 'This volume has not been provisioned for encryption.';
                                  end;
    LongInt($80310009)          : begin
                                    HrStr := 'FVE_E_ACTION_NOT_ALLOWED';
                                    HrDescr := 'Requested action was denied by the full-volume encryption (FVE) control engine.';
                                  end;
    LongInt($8031000A)          : begin
                                    HrStr := 'FVE_E_AD_SCHEMA_NOT_INSTALLED';
                                    HrDescr := 'The Active Directory forest does not contain the required attributes and classes to host FVE or TPM information.';
                                  end;
    LongInt($8031000B)          : begin
                                    HrStr := 'FVE_E_AD_INVALID_DATATYPE';
                                    HrDescr := 'The type of data obtained from Active Directory was not expected.';
                                  end;
    LongInt($8031000C)          : begin
                                    HrStr := 'FVE_E_AD_INVALID_DATASIZE';
                                    HrDescr := 'The size of the data obtained from Active Directory was not expected.';
                                  end;
    LongInt($8031000D)          : begin
                                    HrStr := 'FVE_E_AD_NO_VALUES';
                                    HrDescr := 'The attribute read from Active Directory has no (zero) values.';
                                  end;
    LongInt($8031000E)          : begin
                                    HrStr := 'FVE_E_AD_ATTR_NOT_SET';
                                    HrDescr := 'The attribute was not set.';
                                  end;
    LongInt($8031000F)          : begin
                                    HrStr := 'FVE_E_AD_GUID_NOT_FOUND';
                                    HrDescr := 'The specified GUID could not be found.';
                                  end;
    LongInt($80310010)          : begin
                                    HrStr := 'FVE_E_BAD_INFORMATION';
                                    HrDescr := 'The control block for the encrypted volume is not valid.';
                                  end;
    LongInt($80310011)          : begin
                                    HrStr := 'FVE_E_TOO_SMALL';
                                    HrDescr := 'Not enough free space remaining on volume to allow encryption.';
                                  end;
    LongInt($80310012)          : begin
                                    HrStr := 'FVE_E_SYSTEM_VOLUME';
                                    HrDescr := 'The volume cannot be encrypted because it is required to boot the operating system.';
                                  end;
    LongInt($80310013)          : begin
                                    HrStr := 'FVE_E_FAILED_WRONG_FS';
                                    HrDescr := 'The volume cannot be encrypted because the file system is not supported.';
                                  end;
    LongInt($80310014)          : begin
                                    HrStr := 'FVE_E_FAILED_BAD_FS';
                                    HrDescr := 'The file system is inconsistent.' +
                                               'Run CHKDSK.';
                                  end;
    LongInt($80310015)          : begin
                                    HrStr := 'FVE_E_NOT_SUPPORTED';
                                    HrDescr := 'This volume cannot be encrypted.';
                                  end;
    LongInt($80310016)          : begin
                                    HrStr := 'FVE_E_BAD_DATA';
                                    HrDescr := 'Data supplied is malformed.';
                                  end;
    LongInt($80310017)          : begin
                                    HrStr := 'FVE_E_VOLUME_NOT_BOUND';
                                    HrDescr := 'Volume is not bound to the system.';
                                  end;
    LongInt($80310018)          : begin
                                    HrStr := 'FVE_E_TPM_NOT_OWNED';
                                    HrDescr := 'TPM must be owned before a volume can be bound to it.';
                                  end;
    LongInt($80310019)          : begin
                                    HrStr := 'FVE_E_NOT_DATA_VOLUME';
                                    HrDescr := 'The volume specified is not a data volume.';
                                  end;
    LongInt($8031001A)          : begin
                                    HrStr := 'FVE_E_AD_INSUFFICIENT_BUFFER';
                                    HrDescr := 'The buffer supplied to a function was insufficient to contain the returned data.';
                                  end;
    LongInt($8031001B)          : begin
                                    HrStr := 'FVE_E_CONV_READ';
                                    HrDescr := 'A read operation failed while converting the volume.';
                                  end;
    LongInt($8031001C)          : begin
                                    HrStr := 'FVE_E_CONV_WRITE';
                                    HrDescr := 'A write operation failed while converting the volume.';
                                  end;
    LongInt($8031001D)          : begin
                                    HrStr := 'FVE_E_KEY_REQUIRED';
                                    HrDescr := 'One or more key protection mechanisms are required for this volume.';
                                  end;
    LongInt($8031001E)          : begin
                                    HrStr := 'FVE_E_CLUSTERING_NOT_SUPPORTED';
                                    HrDescr := 'Cluster configurations are not supported.';
                                  end;
    LongInt($8031001F)          : begin
                                    HrStr := 'FVE_E_VOLUME_BOUND_ALREADY';
                                    HrDescr := 'The volume is already bound to the system.';
                                  end;
    LongInt($80310020)          : begin
                                    HrStr := 'FVE_E_OS_NOT_PROTECTED';
                                    HrDescr := 'The boot OS volume is not being protected via FVE.';
                                  end;
    LongInt($80310021)          : begin
                                    HrStr := 'FVE_E_PROTECTION_DISABLED';
                                    HrDescr := 'All protection mechanisms are effectively disabled (clear key exists).';
                                  end;
    LongInt($80310022)          : begin
                                    HrStr := 'FVE_E_RECOVERY_KEY_REQUIRED';
                                    HrDescr := 'A recovery key protection mechanism is required.';
                                  end;
    LongInt($80310023)          : begin
                                    HrStr := 'FVE_E_FOREIGN_VOLUME';
                                    HrDescr := 'This volume cannot be bound to a TPM.';
                                  end;
    LongInt($80310024)          : begin
                                    HrStr := 'FVE_E_OVERLAPPED_UPDATE';
                                    HrDescr := 'The control block for the encrypted volume was updated by another thread.' +
                                               'Try again.';
                                  end;
    LongInt($80310025)          : begin
                                    HrStr := 'FVE_E_TPM_SRK_AUTH_NOT_ZERO';
                                    HrDescr := 'The SRK authentication of the TPM is not zero and, therefore, is not compatible.';
                                  end;
    LongInt($80310026)          : begin
                                    HrStr := 'FVE_E_FAILED_SECTOR_SIZE';
                                    HrDescr := 'The volume encryption algorithm cannot be used on this sector size.';
                                  end;
    LongInt($80310027)          : begin
                                    HrStr := 'FVE_E_FAILED_AUTHENTICATION';
                                    HrDescr := 'BitLocker recovery authentication failed.';
                                  end;
    LongInt($80310028)          : begin
                                    HrStr := 'FVE_E_NOT_OS_VOLUME';
                                    HrDescr := 'The volume specified is not the boot OS volume.';
                                  end;
    LongInt($80310029)          : begin
                                    HrStr := 'FVE_E_AUTOUNLOCK_ENABLED';
                                    HrDescr := 'Auto-unlock information for data volumes is present on the boot OS volume.';
                                  end;
    LongInt($8031002A)          : begin
                                    HrStr := 'FVE_E_WRONG_BOOTSECTOR';
                                    HrDescr := 'The system partition boot sector does not perform TPM measurements.';
                                  end;
    LongInt($8031002B)          : begin
                                    HrStr := 'FVE_E_WRONG_SYSTEM_FS';
                                    HrDescr := 'The system partition file system must be NTFS.';
                                  end;
    LongInt($8031002C)          : begin
                                    HrStr := 'FVE_E_POLICY_PASSWORD_REQUIRED';
                                    HrDescr := 'Group policy requires a recovery password before encryption can begin.';
                                  end;
    LongInt($8031002D)          : begin
                                    HrStr := 'FVE_E_CANNOT_SET_FVEK_ENCRYPTED';
                                    HrDescr := 'The volume encryption algorithm and key cannot be set on an encrypted volume.';
                                  end;
    LongInt($8031002E)          : begin
                                    HrStr := 'FVE_E_CANNOT_ENCRYPT_NO_KEY';
                                    HrDescr := 'A key must be specified before encryption can begin.';
                                  end;
    LongInt($80310030)          : begin
                                    HrStr := 'FVE_E_BOOTABLE_CDDVD';
                                    HrDescr := 'A bootable CD/DVD is in the system.' +
                                               'Remove the CD/DVD and reboot the system.';
                                  end;
    LongInt($80310031)          : begin
                                    HrStr := 'FVE_E_PROTECTOR_EXISTS';
                                    HrDescr := 'An instance of this key protector already exists on the volume.';
                                  end;
    LongInt($80310032)          : begin
                                    HrStr := 'FVE_E_RELATIVE_PATH';
                                    HrDescr := 'The file cannot be saved to a relative path.';
                                  end;
    LongInt($80320001)          : begin
                                    HrStr := 'FWP_E_CALLOUT_NOT_FOUND';
                                    HrDescr := 'The callout does not exist.';
                                  end;
    LongInt($80320002)          : begin
                                    HrStr := 'FWP_E_CONDITION_NOT_FOUND';
                                    HrDescr := 'The filter condition does not exist.';
                                  end;
    LongInt($80320003)          : begin
                                    HrStr := 'FWP_E_FILTER_NOT_FOUND';
                                    HrDescr := 'The filter does not exist.';
                                  end;
    LongInt($80320004)          : begin
                                    HrStr := 'FWP_E_LAYER_NOT_FOUND';
                                    HrDescr := 'The layer does not exist.';
                                  end;
    LongInt($80320005)          : begin
                                    HrStr := 'FWP_E_PROVIDER_NOT_FOUND';
                                    HrDescr := 'The provider does not exist.';
                                  end;
    LongInt($80320006)          : begin
                                    HrStr := 'FWP_E_PROVIDER_CONTEXT_NOT_FOUND';
                                    HrDescr := 'The provider context does not exist.';
                                  end;
    LongInt($80320007)          : begin
                                    HrStr := 'FWP_E_SUBLAYER_NOT_FOUND';
                                    HrDescr := 'The sublayer does not exist.';
                                  end;
    LongInt($80320008)          : begin
                                    HrStr := 'FWP_E_NOT_FOUND';
                                    HrDescr := 'The object does not exist.';
                                  end;
    LongInt($80320009)          : begin
                                    HrStr := 'FWP_E_ALREADY_EXISTS';
                                    HrDescr := 'An object with that GUID or LUID already exists.';
                                  end;
    LongInt($8032000A)          : begin
                                    HrStr := 'FWP_E_IN_USE';
                                    HrDescr := 'The object is referenced by other objects and, therefore, cannot be deleted.';
                                  end;
    LongInt($8032000B)          : begin
                                    HrStr := 'FWP_E_DYNAMIC_SESSION_IN_PROGRESS';
                                    HrDescr := 'The call is not allowed from within a dynamic session.';
                                  end;
    LongInt($8032000C)          : begin
                                    HrStr := 'FWP_E_WRONG_SESSION';
                                    HrDescr := 'The call was made from the wrong session and, therefore, cannot be completed.';
                                  end;
    LongInt($8032000D)          : begin
                                    HrStr := 'FWP_E_NO_TXN_IN_PROGRESS';
                                    HrDescr := 'The call must be made from within an explicit transaction.';
                                  end;
    LongInt($8032000E)          : begin
                                    HrStr := 'FWP_E_TXN_IN_PROGRESS';
                                    HrDescr := 'The call is not allowed from within an explicit transaction.';
                                  end;
    LongInt($8032000F)          : begin
                                    HrStr := 'FWP_E_TXN_ABORTED';
                                    HrDescr := 'The explicit transaction has been forcibly canceled.';
                                  end;
    LongInt($80320010)          : begin
                                    HrStr := 'FWP_E_SESSION_ABORTED';
                                    HrDescr := 'The session has been canceled.';
                                  end;
    LongInt($80320011)          : begin
                                    HrStr := 'FWP_E_INCOMPATIBLE_TXN';
                                    HrDescr := 'The call is not allowed from within a read-only transaction.';
                                  end;
    LongInt($80320012)          : begin
                                    HrStr := 'FWP_E_TIMEOUT';
                                    HrDescr := 'The call timed out while waiting to acquire the transaction lock.';
                                  end;
    LongInt($80320013)          : begin
                                    HrStr := 'FWP_E_NET_EVENTS_DISABLED';
                                    HrDescr := 'Collection of network diagnostic events is disabled.';
                                  end;
    LongInt($80320014)          : begin
                                    HrStr := 'FWP_E_INCOMPATIBLE_LAYER';
                                    HrDescr := 'The operation is not supported by the specified layer.';
                                  end;
    LongInt($80320015)          : begin
                                    HrStr := 'FWP_E_KM_CLIENTS_ONLY';
                                    HrDescr := 'The call is allowed for kernel-mode callers only.';
                                  end;
    LongInt($80320016)          : begin
                                    HrStr := 'FWP_E_LIFETIME_MISMATCH';
                                    HrDescr := 'The call tried to associate two objects with incompatible lifetimes.';
                                  end;
    LongInt($80320017)          : begin
                                    HrStr := 'FWP_E_BUILTIN_OBJECT';
                                    HrDescr := 'The object is built in and, therefore, cannot be deleted.';
                                  end;
    LongInt($80320018)          : begin
                                    HrStr := 'FWP_E_TOO_MANY_BOOTTIME_FILTERS';
                                    HrDescr := 'The maximum number of boot-time filters has been reached.';
                                  end;
    LongInt($80320019)          : begin
                                    HrStr := 'FWP_E_NOTIFICATION_DROPPED';
                                    HrDescr := 'A notification could not be delivered because a message queue is at its maximum capacity.';
                                  end;
    LongInt($8032001A)          : begin
                                    HrStr := 'FWP_E_TRAFFIC_MISMATCH';
                                    HrDescr := 'The traffic parameters do not match those for the security association context.';
                                  end;
    LongInt($8032001B)          : begin
                                    HrStr := 'FWP_E_INCOMPATIBLE_SA_STATE';
                                    HrDescr := 'The call is not allowed for the current security association state.';
                                  end;
    LongInt($8032001C)          : begin
                                    HrStr := 'FWP_E_NULL_POINTER';
                                    HrDescr := 'A required pointer is null.';
                                  end;
    LongInt($8032001D)          : begin
                                    HrStr := 'FWP_E_INVALID_ENUMERATOR';
                                    HrDescr := 'An enumerator is not valid.';
                                  end;
    LongInt($8032001E)          : begin
                                    HrStr := 'FWP_E_INVALID_FLAGS';
                                    HrDescr := 'The flags field contains an invalid value.';
                                  end;
    LongInt($8032001F)          : begin
                                    HrStr := 'FWP_E_INVALID_NET_MASK';
                                    HrDescr := 'A network mask is not valid.';
                                  end;
    LongInt($80320020)          : begin
                                    HrStr := 'FWP_E_INVALID_RANGE';
                                    HrDescr := 'An FWP_RANGE is not valid.';
                                  end;
    LongInt($80320021)          : begin
                                    HrStr := 'FWP_E_INVALID_INTERVAL';
                                    HrDescr := 'The time interval is not valid.';
                                  end;
    LongInt($80320022)          : begin
                                    HrStr := 'FWP_E_ZERO_LENGTH_ARRAY';
                                    HrDescr := 'An array that must contain at least one element that is zero-length.';
                                  end;
    LongInt($80320023)          : begin
                                    HrStr := 'FWP_E_NULL_DISPLAY_NAME';
                                    HrDescr := 'The displayData.name field cannot be null.';
                                  end;
    LongInt($80320024)          : begin
                                    HrStr := 'FWP_E_INVALID_ACTION_TYPE';
                                    HrDescr := 'The action type is not one of the allowed action types for a filter.';
                                  end;
    LongInt($80320025)          : begin
                                    HrStr := 'FWP_E_INVALID_WEIGHT';
                                    HrDescr := 'The filter weight is not valid.';
                                  end;
    LongInt($80320026)          : begin
                                    HrStr := 'FWP_E_MATCH_TYPE_MISMATCH';
                                    HrDescr := 'A filter condition contains a match type that is not compatible with the operands.';
                                  end;
    LongInt($80320027)          : begin
                                    HrStr := 'FWP_E_TYPE_MISMATCH';
                                    HrDescr := 'An FWP_VALUE or FWPM_CONDITION_VALUE is of the wrong type.';
                                  end;
    LongInt($80320028)          : begin
                                    HrStr := 'FWP_E_OUT_OF_BOUNDS';
                                    HrDescr := 'An integer value is outside the allowed range.';
                                  end;
    LongInt($80320029)          : begin
                                    HrStr := 'FWP_E_RESERVED';
                                    HrDescr := 'A reserved field is nonzero.';
                                  end;
    LongInt($8032002A)          : begin
                                    HrStr := 'FWP_E_DUPLICATE_CONDITION';
                                    HrDescr := 'A filter cannot contain multiple conditions operating on a single field.';
                                  end;
    LongInt($8032002B)          : begin
                                    HrStr := 'FWP_E_DUPLICATE_KEYMOD';
                                    HrDescr := 'A policy cannot contain the same keying module more than once.';
                                  end;
    LongInt($8032002C)          : begin
                                    HrStr := 'FWP_E_ACTION_INCOMPATIBLE_WITH_LAYER';
                                    HrDescr := 'The action type is not compatible with the layer.';
                                  end;
    LongInt($8032002D)          : begin
                                    HrStr := 'FWP_E_ACTION_INCOMPATIBLE_WITH_SUBLAYER';
                                    HrDescr := 'The action type is not compatible with the sublayer.';
                                  end;
    LongInt($8032002E)          : begin
                                    HrStr := 'FWP_E_CONTEXT_INCOMPATIBLE_WITH_LAYER';
                                    HrDescr := 'The raw context or the provider context is not compatible with the layer.';
                                  end;
    LongInt($8032002F)          : begin
                                    HrStr := 'FWP_E_CONTEXT_INCOMPATIBLE_WITH_CALLOUT';
                                    HrDescr := 'The raw context or the provider context is not compatible with the callout.';
                                  end;
    LongInt($80320030)          : begin
                                    HrStr := 'FWP_E_INCOMPATIBLE_AUTH_METHOD';
                                    HrDescr := 'The authentication method is not compatible with the policy type.';
                                  end;
    LongInt($80320031)          : begin
                                    HrStr := 'FWP_E_INCOMPATIBLE_DH_GROUP';
                                    HrDescr := 'The Diffie-Hellman group is not compatible with the policy type.';
                                  end;
    LongInt($80320032)          : begin
                                    HrStr := 'FWP_E_EM_NOT_SUPPORTED';
                                    HrDescr := 'An Internet Key Exchange (IKE) policy cannot contain an Extended Mode policy.';
                                  end;
    LongInt($80320033)          : begin
                                    HrStr := 'FWP_E_NEVER_MATCH';
                                    HrDescr := 'The enumeration template or subscription will never match any objects.';
                                  end;
    LongInt($80320034)          : begin
                                    HrStr := 'FWP_E_PROVIDER_CONTEXT_MISMATCH';
                                    HrDescr := 'The provider context is of the wrong type.';
                                  end;
    LongInt($80320035)          : begin
                                    HrStr := 'FWP_E_INVALID_PARAMETER';
                                    HrDescr := 'The parameter is incorrect.';
                                  end;
    LongInt($80320036)          : begin
                                    HrStr := 'FWP_E_TOO_MANY_SUBLAYERS';
                                    HrDescr := 'The maximum number of sublayers has been reached.';
                                  end;
    LongInt($80320037)          : begin
                                    HrStr := 'FWP_E_CALLOUT_NOTIFICATION_FAILED';
                                    HrDescr := 'The notification function for a callout returned an error.';
                                  end;
    LongInt($80320038)          : begin
                                    HrStr := 'FWP_E_INCOMPATIBLE_AUTH_CONFIG';
                                    HrDescr := 'The IPsec authentication configuration is not compatible with the authentication type.';
                                  end;
    LongInt($80320039)          : begin
                                    HrStr := 'FWP_E_INCOMPATIBLE_CIPHER_CONFIG';
                                    HrDescr := 'The IPsec cipher configuration is not compatible with the cipher type.';
                                  end;
    LongInt($80340002)          : begin
                                    HrStr := 'ERROR_NDIS_INTERFACE_CLOSING';
                                    HrDescr := 'The binding to the network interface is being closed.';
                                  end;
    LongInt($80340004)          : begin
                                    HrStr := 'ERROR_NDIS_BAD_VERSION';
                                    HrDescr := 'An invalid version was specified.';
                                  end;
    LongInt($80340005)          : begin
                                    HrStr := 'ERROR_NDIS_BAD_CHARACTERISTICS';
                                    HrDescr := 'An invalid characteristics table was used.';
                                  end;
    LongInt($80340006)          : begin
                                    HrStr := 'ERROR_NDIS_ADAPTER_NOT_FOUND';
                                    HrDescr := 'Failed to find the network interface, or the network interface is not ready.';
                                  end;
    LongInt($80340007)          : begin
                                    HrStr := 'ERROR_NDIS_OPEN_FAILED';
                                    HrDescr := 'Failed to open the network interface.';
                                  end;
    LongInt($80340008)          : begin
                                    HrStr := 'ERROR_NDIS_DEVICE_FAILED';
                                    HrDescr := 'The network interface has encountered an internal unrecoverable failure.';
                                  end;
    LongInt($80340009)          : begin
                                    HrStr := 'ERROR_NDIS_MULTICAST_FULL';
                                    HrDescr := 'The multicast list on the network interface is full.';
                                  end;
    LongInt($8034000A)          : begin
                                    HrStr := 'ERROR_NDIS_MULTICAST_EXISTS';
                                    HrDescr := 'An attempt was made to add a duplicate multicast address to the list.';
                                  end;
    LongInt($8034000B)          : begin
                                    HrStr := 'ERROR_NDIS_MULTICAST_NOT_FOUND';
                                    HrDescr := 'At attempt was made to remove a multicast address that was never added.';
                                  end;
    LongInt($8034000C)          : begin
                                    HrStr := 'ERROR_NDIS_REQUEST_ABORTED';
                                    HrDescr := 'The network interface aborted the request.';
                                  end;
    LongInt($8034000D)          : begin
                                    HrStr := 'ERROR_NDIS_RESET_IN_PROGRESS';
                                    HrDescr := 'The network interface cannot process the request because it is being reset.';
                                  end;
    LongInt($8034000F)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_PACKET';
                                    HrDescr := 'An attempt was made to send an invalid packet on a network interface.';
                                  end;
    LongInt($80340010)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_DEVICE_REQUEST';
                                    HrDescr := 'The specified request is not a valid operation for the target device.';
                                  end;
    LongInt($80340011)          : begin
                                    HrStr := 'ERROR_NDIS_ADAPTER_NOT_READY';
                                    HrDescr := 'The network interface is not ready to complete this operation.';
                                  end;
    LongInt($80340014)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_LENGTH';
                                    HrDescr := 'The length of the buffer submitted for this operation is not valid.';
                                  end;
    LongInt($80340015)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_DATA';
                                    HrDescr := 'The data used for this operation is not valid.';
                                  end;
    LongInt($80340016)          : begin
                                    HrStr := 'ERROR_NDIS_BUFFER_TOO_SHORT';
                                    HrDescr := 'The length of the buffer submitted for this operation is too small.';
                                  end;
    LongInt($80340017)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_OID';
                                    HrDescr := 'The network interface does not support this OID.';
                                  end;
    LongInt($80340018)          : begin
                                    HrStr := 'ERROR_NDIS_ADAPTER_REMOVED';
                                    HrDescr := 'The network interface has been removed.';
                                  end;
    LongInt($80340019)          : begin
                                    HrStr := 'ERROR_NDIS_UNSUPPORTED_MEDIA';
                                    HrDescr := 'The network interface does not support this media type.';
                                  end;
    LongInt($8034001A)          : begin
                                    HrStr := 'ERROR_NDIS_GROUP_ADDRESS_IN_USE';
                                    HrDescr := 'An attempt was made to remove a token ring group address that is in use by other components.';
                                  end;
    LongInt($8034001B)          : begin
                                    HrStr := 'ERROR_NDIS_FILE_NOT_FOUND';
                                    HrDescr := 'An attempt was made to map a file that cannot be found.';
                                  end;
    LongInt($8034001C)          : begin
                                    HrStr := 'ERROR_NDIS_ERROR_READING_FILE';
                                    HrDescr := 'An error occurred while the NDIS tried to map the file.';
                                  end;
    LongInt($8034001D)          : begin
                                    HrStr := 'ERROR_NDIS_ALREADY_MAPPED';
                                    HrDescr := 'An attempt was made to map a file that is already mapped.';
                                  end;
    LongInt($8034001E)          : begin
                                    HrStr := 'ERROR_NDIS_RESOURCE_CONFLICT';
                                    HrDescr := 'An attempt to allocate a hardware resource failed because the resource is used by another component.';
                                  end;
    LongInt($8034001F)          : begin
                                    HrStr := 'ERROR_NDIS_MEDIA_DISCONNECTED';
                                    HrDescr := 'The I/O operation failed because network media is disconnected or the wireless access point is out of range.';
                                  end;
    LongInt($80340022)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_ADDRESS';
                                    HrDescr := 'The network address used in the request is invalid.';
                                  end;
    LongInt($8034002A)          : begin
                                    HrStr := 'ERROR_NDIS_PAUSED';
                                    HrDescr := 'The offload operation on the network interface has been paused.';
                                  end;
    LongInt($8034002B)          : begin
                                    HrStr := 'ERROR_NDIS_INTERFACE_NOT_FOUND';
                                    HrDescr := 'The network interface was not found.';
                                  end;
    LongInt($8034002C)          : begin
                                    HrStr := 'ERROR_NDIS_UNSUPPORTED_REVISION';
                                    HrDescr := 'The revision number specified in the structure is not supported.';
                                  end;
    LongInt($8034002D)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_PORT';
                                    HrDescr := 'The specified port does not exist on this network interface.';
                                  end;
    LongInt($8034002E)          : begin
                                    HrStr := 'ERROR_NDIS_INVALID_PORT_STATE';
                                    HrDescr := 'The current state of the specified port on this network interface does not support the requested operation.';
                                  end;
    LongInt($803400BB)          : begin
                                    HrStr := 'ERROR_NDIS_NOT_SUPPORTED';
                                    HrDescr := 'The network interface does not support this request.';
                                  end;
    LongInt($80342000)          : begin
                                    HrStr := 'ERROR_NDIS_DOT11_AUTO_CONFIG_ENABLED';
                                    HrDescr := 'The wireless local area network (LAN) interface is in auto-configuration mode and does not support the requested parameter change operation.';
                                  end;
    LongInt($80342001)          : begin
                                    HrStr := 'ERROR_NDIS_DOT11_MEDIA_IN_USE';
                                    HrDescr := 'The wireless LAN interface is busy and cannot perform the requested operation.';
                                  end;
    LongInt($80342002)          : begin
                                    HrStr := 'ERROR_NDIS_DOT11_POWER_STATE_INVALID';
                                    HrDescr := 'The wireless LAN interface is shutting down and does not support the requested operation.';
                                  end;
    LongInt($8DEAD01B)          : begin
                                    HrStr := 'TRK_E_NOT_FOUND';
                                    HrDescr := 'A requested object was not found.';
                                  end;
    LongInt($8DEAD01C)          : begin
                                    HrStr := 'TRK_E_VOLUME_QUOTA_EXCEEDED';
                                    HrDescr := 'The server received a CREATE_VOLUME subrequest of a SYNC_VOLUMES request, but the ServerVolumeTable size limit for the RequestMachine has already been reached.';
                                  end;
    LongInt($8DEAD01E)          : begin
                                    HrStr := 'TRK_SERVER_TOO_BUSY';
                                    HrDescr := 'The server is busy, and the client should retry the request at a later time.';
                                  end;
    LongInt($C0090001)          : begin
                                    HrStr := 'ERROR_AUDITING_DISABLED';
                                    HrDescr := 'The specified event is currently not being audited.';
                                  end;
    LongInt($C0090002)          : begin
                                    HrStr := 'ERROR_ALL_SIDS_FILTERED';
                                    HrDescr := 'The SID filtering operation removed all SIDs.';
                                  end;
    LongInt($C0090003)          : begin
                                    HrStr := 'ERROR_BIZRULES_NOT_ENABLED';
                                    HrDescr := 'Business rule scripts are disabled for the calling application.';
                                  end;
    LongInt($C00D0005)          : begin
                                    HrStr := 'NS_E_NOCONNECTION';
                                    HrDescr := 'There is no connection established with the Windows Media server.' +
                                               'The operation failed.';
                                  end;
    LongInt($C00D0006)          : begin
                                    HrStr := 'NS_E_CANNOTCONNECT';
                                    HrDescr := 'Unable to establish a connection to the server.';
                                  end;
    LongInt($C00D0007)          : begin
                                    HrStr := 'NS_E_CANNOTDESTROYTITLE';
                                    HrDescr := 'Unable to destroy the title.';
                                  end;
    LongInt($C00D0008)          : begin
                                    HrStr := 'NS_E_CANNOTRENAMETITLE';
                                    HrDescr := 'Unable to rename the title.';
                                  end;
    LongInt($C00D0009)          : begin
                                    HrStr := 'NS_E_CANNOTOFFLINEDISK';
                                    HrDescr := 'Unable to offline disk.';
                                  end;
    LongInt($C00D000A)          : begin
                                    HrStr := 'NS_E_CANNOTONLINEDISK';
                                    HrDescr := 'Unable to online disk.';
                                  end;
    LongInt($C00D000B)          : begin
                                    HrStr := 'NS_E_NOREGISTEREDWALKER';
                                    HrDescr := 'There is no file parser registered for this type of file.';
                                  end;
    LongInt($C00D000C)          : begin
                                    HrStr := 'NS_E_NOFUNNEL';
                                    HrDescr := 'There is no data connection established.';
                                  end;
    LongInt($C00D000D)          : begin
                                    HrStr := 'NS_E_NO_LOCALPLAY';
                                    HrDescr := 'Failed to load the local play DLL.';
                                  end;
    LongInt($C00D000E)          : begin
                                    HrStr := 'NS_E_NETWORK_BUSY';
                                    HrDescr := 'The network is busy.';
                                  end;
    LongInt($C00D000F)          : begin
                                    HrStr := 'NS_E_TOO_MANY_SESS';
                                    HrDescr := 'The server session limit was exceeded.';
                                  end;
    LongInt($C00D0010)          : begin
                                    HrStr := 'NS_E_ALREADY_CONNECTED';
                                    HrDescr := 'The network connection already exists.';
                                  end;
    LongInt($C00D0011)          : begin
                                    HrStr := 'NS_E_INVALID_INDEX';
                                    HrDescr := 'Index %1 is invalid.';
                                  end;
    LongInt($C00D0012)          : begin
                                    HrStr := 'NS_E_PROTOCOL_MISMATCH';
                                    HrDescr := 'There is no protocol or protocol version supported by both the client and the server.';
                                  end;
    LongInt($C00D0013)          : begin
                                    HrStr := 'NS_E_TIMEOUT';
                                    HrDescr := 'The server, a computer set up to offer multimedia content to other computers, could not handle your request for multimedia content in a timely manner.' +
                                               'Please try again later.';
                                  end;
    LongInt($C00D0014)          : begin
                                    HrStr := 'NS_E_NET_WRITE';
                                    HrDescr := 'Error writing to the network.';
                                  end;
    LongInt($C00D0015)          : begin
                                    HrStr := 'NS_E_NET_READ';
                                    HrDescr := 'Error reading from the network.';
                                  end;
    LongInt($C00D0016)          : begin
                                    HrStr := 'NS_E_DISK_WRITE';
                                    HrDescr := 'Error writing to a disk.';
                                  end;
    LongInt($C00D0017)          : begin
                                    HrStr := 'NS_E_DISK_READ';
                                    HrDescr := 'Error reading from a disk.';
                                  end;
    LongInt($C00D0018)          : begin
                                    HrStr := 'NS_E_FILE_WRITE';
                                    HrDescr := 'Error writing to a file.';
                                  end;
    LongInt($C00D0019)          : begin
                                    HrStr := 'NS_E_FILE_READ';
                                    HrDescr := 'Error reading from a file.';
                                  end;
    LongInt($C00D001A)          : begin
                                    HrStr := 'NS_E_FILE_NOT_FOUND';
                                    HrDescr := 'The system cannot find the file specified.';
                                  end;
    LongInt($C00D001B)          : begin
                                    HrStr := 'NS_E_FILE_EXISTS';
                                    HrDescr := 'The file already exists.';
                                  end;
    LongInt($C00D001C)          : begin
                                    HrStr := 'NS_E_INVALID_NAME';
                                    HrDescr := 'The file name, directory name, or volume label syntax is incorrect.';
                                  end;
    LongInt($C00D001D)          : begin
                                    HrStr := 'NS_E_FILE_OPEN_FAILED';
                                    HrDescr := 'Failed to open a file.';
                                  end;
    LongInt($C00D001E)          : begin
                                    HrStr := 'NS_E_FILE_ALLOCATION_FAILED';
                                    HrDescr := 'Unable to allocate a file.';
                                  end;
    LongInt($C00D001F)          : begin
                                    HrStr := 'NS_E_FILE_INIT_FAILED';
                                    HrDescr := 'Unable to initialize a file.';
                                  end;
    LongInt($C00D0020)          : begin
                                    HrStr := 'NS_E_FILE_PLAY_FAILED';
                                    HrDescr := 'Unable to play a file.';
                                  end;
    LongInt($C00D0021)          : begin
                                    HrStr := 'NS_E_SET_DISK_UID_FAILED';
                                    HrDescr := 'Could not set the disk UID.';
                                  end;
    LongInt($C00D0022)          : begin
                                    HrStr := 'NS_E_INDUCED';
                                    HrDescr := 'An error was induced for testing purposes.';
                                  end;
    LongInt($C00D0023)          : begin
                                    HrStr := 'NS_E_CCLINK_DOWN';
                                    HrDescr := 'Two Content Servers failed to communicate.';
                                  end;
    LongInt($C00D0024)          : begin
                                    HrStr := 'NS_E_INTERNAL';
                                    HrDescr := 'An unknown error occurred.';
                                  end;
    LongInt($C00D0025)          : begin
                                    HrStr := 'NS_E_BUSY';
                                    HrDescr := 'The requested resource is in use.';
                                  end;
    LongInt($C00D0026)          : begin
                                    HrStr := 'NS_E_UNRECOGNIZED_STREAM_TYPE';
                                    HrDescr := 'The specified protocol is not recognized.' +
                                               'Be sure that the file name and syntax, such as slashes, are correct for the protocol.';
                                  end;
    LongInt($C00D0027)          : begin
                                    HrStr := 'NS_E_NETWORK_SERVICE_FAILURE';
                                    HrDescr := 'The network service provider failed.';
                                  end;
    LongInt($C00D0028)          : begin
                                    HrStr := 'NS_E_NETWORK_RESOURCE_FAILURE';
                                    HrDescr := 'An attempt to acquire a network resource failed.';
                                  end;
    LongInt($C00D0029)          : begin
                                    HrStr := 'NS_E_CONNECTION_FAILURE';
                                    HrDescr := 'The network connection has failed.';
                                  end;
    LongInt($C00D002A)          : begin
                                    HrStr := 'NS_E_SHUTDOWN';
                                    HrDescr := 'The session is being terminated locally.';
                                  end;
    LongInt($C00D002B)          : begin
                                    HrStr := 'NS_E_INVALID_REQUEST';
                                    HrDescr := 'The request is invalid in the current state.';
                                  end;
    LongInt($C00D002C)          : begin
                                    HrStr := 'NS_E_INSUFFICIENT_BANDWIDTH';
                                    HrDescr := 'There is insufficient bandwidth available to fulfill the request.';
                                  end;
    LongInt($C00D002D)          : begin
                                    HrStr := 'NS_E_NOT_REBUILDING';
                                    HrDescr := 'The disk is not rebuilding.';
                                  end;
    LongInt($C00D002E)          : begin
                                    HrStr := 'NS_E_LATE_OPERATION';
                                    HrDescr := 'An operation requested for a particular time could not be carried out on schedule.';
                                  end;
    LongInt($C00D002F)          : begin
                                    HrStr := 'NS_E_INVALID_DATA';
                                    HrDescr := 'Invalid or corrupt data was encountered.';
                                  end;
    LongInt($C00D0030)          : begin
                                    HrStr := 'NS_E_FILE_BANDWIDTH_LIMIT';
                                    HrDescr := 'The bandwidth required to stream a file is higher than the maximum file bandwidth allowed on the server.';
                                  end;
    LongInt($C00D0031)          : begin
                                    HrStr := 'NS_E_OPEN_FILE_LIMIT';
                                    HrDescr := 'The client cannot have any more files open simultaneously.';
                                  end;
    LongInt($C00D0032)          : begin
                                    HrStr := 'NS_E_BAD_CONTROL_DATA';
                                    HrDescr := 'The server received invalid data from the client on the control connection.';
                                  end;
    LongInt($C00D0033)          : begin
                                    HrStr := 'NS_E_NO_STREAM';
                                    HrDescr := 'There is no stream available.';
                                  end;
    LongInt($C00D0034)          : begin
                                    HrStr := 'NS_E_STREAM_END';
                                    HrDescr := 'There is no more data in the stream.';
                                  end;
    LongInt($C00D0035)          : begin
                                    HrStr := 'NS_E_SERVER_NOT_FOUND';
                                    HrDescr := 'The specified server could not be found.';
                                  end;
    LongInt($C00D0036)          : begin
                                    HrStr := 'NS_E_DUPLICATE_NAME';
                                    HrDescr := 'The specified name is already in use.';
                                  end;
    LongInt($C00D0037)          : begin
                                    HrStr := 'NS_E_DUPLICATE_ADDRESS';
                                    HrDescr := 'The specified address is already in use.';
                                  end;
    LongInt($C00D0038)          : begin
                                    HrStr := 'NS_E_BAD_MULTICAST_ADDRESS';
                                    HrDescr := 'The specified address is not a valid multicast address.';
                                  end;
    LongInt($C00D0039)          : begin
                                    HrStr := 'NS_E_BAD_ADAPTER_ADDRESS';
                                    HrDescr := 'The specified adapter address is invalid.';
                                  end;
    LongInt($C00D003A)          : begin
                                    HrStr := 'NS_E_BAD_DELIVERY_MODE';
                                    HrDescr := 'The specified delivery mode is invalid.';
                                  end;
    LongInt($C00D003B)          : begin
                                    HrStr := 'NS_E_INVALID_CHANNEL';
                                    HrDescr := 'The specified station does not exist.';
                                  end;
    LongInt($C00D003C)          : begin
                                    HrStr := 'NS_E_INVALID_STREAM';
                                    HrDescr := 'The specified stream does not exist.';
                                  end;
    LongInt($C00D003D)          : begin
                                    HrStr := 'NS_E_INVALID_ARCHIVE';
                                    HrDescr := 'The specified archive could not be opened.';
                                  end;
    LongInt($C00D003E)          : begin
                                    HrStr := 'NS_E_NOTITLES';
                                    HrDescr := 'The system cannot find any titles on the server.';
                                  end;
    LongInt($C00D003F)          : begin
                                    HrStr := 'NS_E_INVALID_CLIENT';
                                    HrDescr := 'The system cannot find the client specified.';
                                  end;
    LongInt($C00D0040)          : begin
                                    HrStr := 'NS_E_INVALID_BLACKHOLE_ADDRESS';
                                    HrDescr := 'The Blackhole Address is not initialized.';
                                  end;
    LongInt($C00D0041)          : begin
                                    HrStr := 'NS_E_INCOMPATIBLE_FORMAT';
                                    HrDescr := 'The station does not support the stream format.';
                                  end;
    LongInt($C00D0042)          : begin
                                    HrStr := 'NS_E_INVALID_KEY';
                                    HrDescr := 'The specified key is not valid.';
                                  end;
    LongInt($C00D0043)          : begin
                                    HrStr := 'NS_E_INVALID_PORT';
                                    HrDescr := 'The specified port is not valid.';
                                  end;
    LongInt($C00D0044)          : begin
                                    HrStr := 'NS_E_INVALID_TTL';
                                    HrDescr := 'The specified TTL is not valid.';
                                  end;
    LongInt($C00D0045)          : begin
                                    HrStr := 'NS_E_STRIDE_REFUSED';
                                    HrDescr := 'The request to fast forward or rewind could not be fulfilled.';
                                  end;
    LongInt($C00D0046)          : begin
                                    HrStr := 'NS_E_MMSAUTOSERVER_CANTFINDWALKER';
                                    HrDescr := 'Unable to load the appropriate file parser.';
                                  end;
    LongInt($C00D0047)          : begin
                                    HrStr := 'NS_E_MAX_BITRATE';
                                    HrDescr := 'Cannot exceed the maximum bandwidth limit.';
                                  end;
    LongInt($C00D0048)          : begin
                                    HrStr := 'NS_E_LOGFILEPERIOD';
                                    HrDescr := 'Invalid value for LogFilePeriod.';
                                  end;
    LongInt($C00D0049)          : begin
                                    HrStr := 'NS_E_MAX_CLIENTS';
                                    HrDescr := 'Cannot exceed the maximum client limit.';
                                  end;
    LongInt($C00D004A)          : begin
                                    HrStr := 'NS_E_LOG_FILE_SIZE';
                                    HrDescr := 'The maximum log file size has been reached.';
                                  end;
    LongInt($C00D004B)          : begin
                                    HrStr := 'NS_E_MAX_FILERATE';
                                    HrDescr := 'Cannot exceed the maximum file rate.';
                                  end;
    LongInt($C00D004C)          : begin
                                    HrStr := 'NS_E_WALKER_UNKNOWN';
                                    HrDescr := 'Unknown file type.';
                                  end;
    LongInt($C00D004D)          : begin
                                    HrStr := 'NS_E_WALKER_SERVER';
                                    HrDescr := 'The specified file, %1, cannot be loaded onto the specified server, %2.';
                                  end;
    LongInt($C00D004E)          : begin
                                    HrStr := 'NS_E_WALKER_USAGE';
                                    HrDescr := 'There was a usage error with file parser.';
                                  end;
    LongInt($C00D0050)          : begin
                                    HrStr := 'NS_E_TIGER_FAIL';
                                    HrDescr := 'The Title Server %1 has failed.';
                                  end;
    LongInt($C00D0053)          : begin
                                    HrStr := 'NS_E_CUB_FAIL';
                                    HrDescr := 'Content Server %1 (%2) has failed.';
                                  end;
    LongInt($C00D0055)          : begin
                                    HrStr := 'NS_E_DISK_FAIL';
                                    HrDescr := 'Disk %1 ( %2 ) on Content Server %3, has failed.';
                                  end;
    LongInt($C00D0060)          : begin
                                    HrStr := 'NS_E_MAX_FUNNELS_ALERT';
                                    HrDescr := 'The NetShow data stream limit of %1 streams was reached.';
                                  end;
    LongInt($C00D0061)          : begin
                                    HrStr := 'NS_E_ALLOCATE_FILE_FAIL';
                                    HrDescr := 'The NetShow Video Server was unable to allocate a %1 block file named %2.';
                                  end;
    LongInt($C00D0062)          : begin
                                    HrStr := 'NS_E_PAGING_ERROR';
                                    HrDescr := 'A Content Server was unable to page a block.';
                                  end;
    LongInt($C00D0063)          : begin
                                    HrStr := 'NS_E_BAD_BLOCK0_VERSION';
                                    HrDescr := 'Disk %1 has unrecognized control block version %2.';
                                  end;
    LongInt($C00D0064)          : begin
                                    HrStr := 'NS_E_BAD_DISK_UID';
                                    HrDescr := 'Disk %1 has incorrect uid %2.';
                                  end;
    LongInt($C00D0065)          : begin
                                    HrStr := 'NS_E_BAD_FSMAJOR_VERSION';
                                    HrDescr := 'Disk %1 has unsupported file system major version %2.';
                                  end;
    LongInt($C00D0066)          : begin
                                    HrStr := 'NS_E_BAD_STAMPNUMBER';
                                    HrDescr := 'Disk %1 has bad stamp number in control block.';
                                  end;
    LongInt($C00D0067)          : begin
                                    HrStr := 'NS_E_PARTIALLY_REBUILT_DISK';
                                    HrDescr := 'Disk %1 is partially reconstructed.';
                                  end;
    LongInt($C00D0068)          : begin
                                    HrStr := 'NS_E_ENACTPLAN_GIVEUP';
                                    HrDescr := 'EnactPlan gives up.';
                                  end;
    LongInt($C00D006A)          : begin
                                    HrStr := 'MCMADM_E_REGKEY_NOT_FOUND';
                                    HrDescr := 'The key was not found in the registry.';
                                  end;
    LongInt($C00D006B)          : begin
                                    HrStr := 'NS_E_NO_FORMATS';
                                    HrDescr := 'The publishing point cannot be started because the server does not have the appropriate stream formats.' +
                                               'Use the Multicast Announcement Wizard to create a new announcement for this publishing point.';
                                  end;
    LongInt($C00D006C)          : begin
                                    HrStr := 'NS_E_NO_REFERENCES';
                                    HrDescr := 'No reference URLs were found in an ASX file.';
                                  end;
    LongInt($C00D006D)          : begin
                                    HrStr := 'NS_E_WAVE_OPEN';
                                    HrDescr := 'Error opening wave device, the device might be in use.';
                                  end;
    LongInt($C00D006F)          : begin
                                    HrStr := 'NS_E_CANNOTCONNECTEVENTS';
                                    HrDescr := 'Unable to establish a connection to the NetShow event monitor service.';
                                  end;
    LongInt($C00D0071)          : begin
                                    HrStr := 'NS_E_NO_DEVICE';
                                    HrDescr := 'No device driver is present on the system.';
                                  end;
    LongInt($C00D0072)          : begin
                                    HrStr := 'NS_E_NO_SPECIFIED_DEVICE';
                                    HrDescr := 'No specified device driver is present.';
                                  end;
    LongInt($C00D00C8)          : begin
                                    HrStr := 'NS_E_MONITOR_GIVEUP';
                                    HrDescr := 'Netshow Events Monitor is not operational and has been disconnected.';
                                  end;
    LongInt($C00D00C9)          : begin
                                    HrStr := 'NS_E_REMIRRORED_DISK';
                                    HrDescr := 'Disk %1 is remirrored.';
                                  end;
    LongInt($C00D00CA)          : begin
                                    HrStr := 'NS_E_INSUFFICIENT_DATA';
                                    HrDescr := 'Insufficient data found.';
                                  end;
    LongInt($C00D00CB)          : begin
                                    HrStr := 'NS_E_ASSERT';
                                    HrDescr := '1 failed in file %2 line %3.';
                                  end;
    LongInt($C00D00CC)          : begin
                                    HrStr := 'NS_E_BAD_ADAPTER_NAME';
                                    HrDescr := 'The specified adapter name is invalid.';
                                  end;
    LongInt($C00D00CD)          : begin
                                    HrStr := 'NS_E_NOT_LICENSED';
                                    HrDescr := 'The application is not licensed for this feature.';
                                  end;
    LongInt($C00D00CE)          : begin
                                    HrStr := 'NS_E_NO_SERVER_CONTACT';
                                    HrDescr := 'Unable to contact the server.';
                                  end;
    LongInt($C00D00CF)          : begin
                                    HrStr := 'NS_E_TOO_MANY_TITLES';
                                    HrDescr := 'Maximum number of titles exceeded.';
                                  end;
    LongInt($C00D00D0)          : begin
                                    HrStr := 'NS_E_TITLE_SIZE_EXCEEDED';
                                    HrDescr := 'Maximum size of a title exceeded.';
                                  end;
    LongInt($C00D00D1)          : begin
                                    HrStr := 'NS_E_UDP_DISABLED';
                                    HrDescr := 'UDP protocol not enabled.' +
                                               'Not trying %1!ls!.';
                                  end;
    LongInt($C00D00D2)          : begin
                                    HrStr := 'NS_E_TCP_DISABLED';
                                    HrDescr := 'TCP protocol not enabled.' +
                                               'Not trying %1!ls!.';
                                  end;
    LongInt($C00D00D3)          : begin
                                    HrStr := 'NS_E_HTTP_DISABLED';
                                    HrDescr := 'HTTP protocol not enabled.' +
                                               'Not trying %1!ls!.';
                                  end;
    LongInt($C00D00D4)          : begin
                                    HrStr := 'NS_E_LICENSE_EXPIRED';
                                    HrDescr := 'The product license has expired.';
                                  end;
    LongInt($C00D00D5)          : begin
                                    HrStr := 'NS_E_TITLE_BITRATE';
                                    HrDescr := 'Source file exceeds the per title maximum bitrate.' +
                                               'See NetShow Theater documentation for more information.';
                                  end;
    LongInt($C00D00D6)          : begin
                                    HrStr := 'NS_E_EMPTY_PROGRAM_NAME';
                                    HrDescr := 'The program name cannot be empty.';
                                  end;
    LongInt($C00D00D7)          : begin
                                    HrStr := 'NS_E_MISSING_CHANNEL';
                                    HrDescr := 'Station %1 does not exist.';
                                  end;
    LongInt($C00D00D8)          : begin
                                    HrStr := 'NS_E_NO_CHANNELS';
                                    HrDescr := 'You need to define at least one station before this operation can complete.';
                                  end;
    LongInt($C00D00D9)          : begin
                                    HrStr := 'NS_E_INVALID_INDEX2';
                                    HrDescr := 'The index specified is invalid.';
                                  end;
    LongInt($C00D0190)          : begin
                                    HrStr := 'NS_E_CUB_FAIL_LINK';
                                    HrDescr := 'Content Server %1 (%2) has failed its link to Content Server %3.';
                                  end;
    LongInt($C00D0192)          : begin
                                    HrStr := 'NS_E_BAD_CUB_UID';
                                    HrDescr := 'Content Server %1 (%2) has incorrect uid %3.';
                                  end;
    LongInt($C00D0195)          : begin
                                    HrStr := 'NS_E_GLITCH_MODE';
                                    HrDescr := 'Server unreliable because multiple components failed.';
                                  end;
    LongInt($C00D019B)          : begin
                                    HrStr := 'NS_E_NO_MEDIA_PROTOCOL';
                                    HrDescr := 'Content Server %1 (%2) is unable to communicate with the Media System Network Protocol.';
                                  end;
    LongInt($C00D07F1)          : begin
                                    HrStr := 'NS_E_NOTHING_TO_DO';
                                    HrDescr := 'Nothing to do.';
                                  end;
    LongInt($C00D07F2)          : begin
                                    HrStr := 'NS_E_NO_MULTICAST';
                                    HrDescr := 'Not receiving data from the server.';
                                  end;
    LongInt($C00D0BB8)          : begin
                                    HrStr := 'NS_E_INVALID_INPUT_FORMAT';
                                    HrDescr := 'The input media format is invalid.';
                                  end;
    LongInt($C00D0BB9)          : begin
                                    HrStr := 'NS_E_MSAUDIO_NOT_INSTALLED';
                                    HrDescr := 'The MSAudio codec is not installed on this system.';
                                  end;
    LongInt($C00D0BBA)          : begin
                                    HrStr := 'NS_E_UNEXPECTED_MSAUDIO_ERROR';
                                    HrDescr := 'An unexpected error occurred with the MSAudio codec.';
                                  end;
    LongInt($C00D0BBB)          : begin
                                    HrStr := 'NS_E_INVALID_OUTPUT_FORMAT';
                                    HrDescr := 'The output media format is invalid.';
                                  end;
    LongInt($C00D0BBC)          : begin
                                    HrStr := 'NS_E_NOT_CONFIGURED';
                                    HrDescr := 'The object must be fully configured before audio samples can be processed.';
                                  end;
    LongInt($C00D0BBD)          : begin
                                    HrStr := 'NS_E_PROTECTED_CONTENT';
                                    HrDescr := 'You need a license to perform the requested operation on this media file.';
                                  end;
    LongInt($C00D0BBE)          : begin
                                    HrStr := 'NS_E_LICENSE_REQUIRED';
                                    HrDescr := 'You need a license to perform the requested operation on this media file.';
                                  end;
    LongInt($C00D0BBF)          : begin
                                    HrStr := 'NS_E_TAMPERED_CONTENT';
                                    HrDescr := 'This media file is corrupted or invalid.' +
                                               'Contact the content provider for a new file.';
                                  end;
    LongInt($C00D0BC0)          : begin
                                    HrStr := 'NS_E_LICENSE_OUTOFDATE';
                                    HrDescr := 'The license for this media file has expired.' +
                                               'Get a new license or contact the content provider for further assistance.';
                                  end;
    LongInt($C00D0BC1)          : begin
                                    HrStr := 'NS_E_LICENSE_INCORRECT_RIGHTS';
                                    HrDescr := 'You are not allowed to open this file.' +
                                               'Contact the content provider for further assistance.';
                                  end;
    LongInt($C00D0BC2)          : begin
                                    HrStr := 'NS_E_AUDIO_CODEC_NOT_INSTALLED';
                                    HrDescr := 'The requested audio codec is not installed on this system.';
                                  end;
    LongInt($C00D0BC3)          : begin
                                    HrStr := 'NS_E_AUDIO_CODEC_ERROR';
                                    HrDescr := 'An unexpected error occurred with the audio codec.';
                                  end;
    LongInt($C00D0BC4)          : begin
                                    HrStr := 'NS_E_VIDEO_CODEC_NOT_INSTALLED';
                                    HrDescr := 'The requested video codec is not installed on this system.';
                                  end;
    LongInt($C00D0BC5)          : begin
                                    HrStr := 'NS_E_VIDEO_CODEC_ERROR';
                                    HrDescr := 'An unexpected error occurred with the video codec.';
                                  end;
    LongInt($C00D0BC6)          : begin
                                    HrStr := 'NS_E_INVALIDPROFILE';
                                    HrDescr := 'The Profile is invalid.';
                                  end;
    LongInt($C00D0BC7)          : begin
                                    HrStr := 'NS_E_INCOMPATIBLE_VERSION';
                                    HrDescr := 'A new version of the SDK is needed to play the requested content.';
                                  end;
    LongInt($C00D0BCA)          : begin
                                    HrStr := 'NS_E_OFFLINE_MODE';
                                    HrDescr := 'The requested URL is not available in offline mode.';
                                  end;
    LongInt($C00D0BCB)          : begin
                                    HrStr := 'NS_E_NOT_CONNECTED';
                                    HrDescr := 'The requested URL cannot be accessed because there is no network connection.';
                                  end;
    LongInt($C00D0BCC)          : begin
                                    HrStr := 'NS_E_TOO_MUCH_DATA';
                                    HrDescr := 'The encoding process was unable to keep up with the amount of supplied data.';
                                  end;
    LongInt($C00D0BCD)          : begin
                                    HrStr := 'NS_E_UNSUPPORTED_PROPERTY';
                                    HrDescr := 'The given property is not supported.';
                                  end;
    LongInt($C00D0BCE)          : begin
                                    HrStr := 'NS_E_8BIT_WAVE_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player cannot copy the files to the CD because they are 8-bit.' +
                                               'Convert the files to 16-bit, 44-kHz stereo files by using Sound Recorder or another audio-processing program, and then try again.';
                                  end;
    LongInt($C00D0BCF)          : begin
                                    HrStr := 'NS_E_NO_MORE_SAMPLES';
                                    HrDescr := 'There are no more samples in the current range.';
                                  end;
    LongInt($C00D0BD0)          : begin
                                    HrStr := 'NS_E_INVALID_SAMPLING_RATE';
                                    HrDescr := 'The given sampling rate is invalid.';
                                  end;
    LongInt($C00D0BD1)          : begin
                                    HrStr := 'NS_E_MAX_PACKET_SIZE_TOO_SMALL';
                                    HrDescr := 'The given maximum packet size is too small to accommodate this profile.)';
                                  end;
    LongInt($C00D0BD2)          : begin
                                    HrStr := 'NS_E_LATE_PACKET';
                                    HrDescr := 'The packet arrived too late to be of use.';
                                  end;
    LongInt($C00D0BD3)          : begin
                                    HrStr := 'NS_E_DUPLICATE_PACKET';
                                    HrDescr := 'The packet is a duplicate of one received before.';
                                  end;
    LongInt($C00D0BD4)          : begin
                                    HrStr := 'NS_E_SDK_BUFFERTOOSMALL';
                                    HrDescr := 'Supplied buffer is too small.';
                                  end;
    LongInt($C00D0BD5)          : begin
                                    HrStr := 'NS_E_INVALID_NUM_PASSES';
                                    HrDescr := 'The wrong number of preprocessing passes was used for the stream''s output type.';
                                  end;
    LongInt($C00D0BD6)          : begin
                                    HrStr := 'NS_E_ATTRIBUTE_READ_ONLY';
                                    HrDescr := 'An attempt was made to add, modify, or delete a read only attribute.';
                                  end;
    LongInt($C00D0BD7)          : begin
                                    HrStr := 'NS_E_ATTRIBUTE_NOT_ALLOWED';
                                    HrDescr := 'An attempt was made to add attribute that is not allowed for the given media type.';
                                  end;
    LongInt($C00D0BD8)          : begin
                                    HrStr := 'NS_E_INVALID_EDL';
                                    HrDescr := 'The EDL provided is invalid.';
                                  end;
    LongInt($C00D0BD9)          : begin
                                    HrStr := 'NS_E_DATA_UNIT_EXTENSION_TOO_LARGE';
                                    HrDescr := 'The Data Unit Extension data was too large to be used.';
                                  end;
    LongInt($C00D0BDA)          : begin
                                    HrStr := 'NS_E_CODEC_DMO_ERROR';
                                    HrDescr := 'An unexpected error occurred with a DMO codec.';
                                  end;
    LongInt($C00D0BDC)          : begin
                                    HrStr := 'NS_E_FEATURE_DISABLED_BY_GROUP_POLICY';
                                    HrDescr := 'This feature has been disabled by group policy.';
                                  end;
    LongInt($C00D0BDD)          : begin
                                    HrStr := 'NS_E_FEATURE_DISABLED_IN_SKU';
                                    HrDescr := 'This feature is disabled in this SKU.';
                                  end;
    LongInt($C00D0FA0)          : begin
                                    HrStr := 'NS_E_NO_CD';
                                    HrDescr := 'There is no CD in the CD drive.' +
                                               'Insert a CD, and then try again.';
                                  end;
    LongInt($C00D0FA1)          : begin
                                    HrStr := 'NS_E_CANT_READ_DIGITAL';
                                    HrDescr := 'Windows Media Player could not use digital playback to play the CD.' +
                                               'To switch to analog playback, on the Tools menu, click Options, and then click the Devices tab.' +
                                               'Double-click the CD drive, and then in the Playback area, click Analog.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FA2)          : begin
                                    HrStr := 'NS_E_DEVICE_DISCONNECTED';
                                    HrDescr := 'Windows Media Player no longer detects a connected portable device.' +
                                               'Reconnect your portable device, and then try synchronizing the file again.';
                                  end;
    LongInt($C00D0FA3)          : begin
                                    HrStr := 'NS_E_DEVICE_NOT_SUPPORT_FORMAT';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'The portable device does not support the specified file type.';
                                  end;
    LongInt($C00D0FA4)          : begin
                                    HrStr := 'NS_E_SLOW_READ_DIGITAL';
                                    HrDescr := 'Windows Media Player could not use digital playback to play the CD.' +
                                               'The Player has automatically switched the CD drive to analog playback.' +
                                               'To switch back to digital CD playback, use the Devices tab.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FA5)          : begin
                                    HrStr := 'NS_E_MIXER_INVALID_LINE';
                                    HrDescr := 'An invalid line error occurred in the mixer.';
                                  end;
    LongInt($C00D0FA6)          : begin
                                    HrStr := 'NS_E_MIXER_INVALID_CONTROL';
                                    HrDescr := 'An invalid control error occurred in the mixer.';
                                  end;
    LongInt($C00D0FA7)          : begin
                                    HrStr := 'NS_E_MIXER_INVALID_VALUE';
                                    HrDescr := 'An invalid value error occurred in the mixer.';
                                  end;
    LongInt($C00D0FA8)          : begin
                                    HrStr := 'NS_E_MIXER_UNKNOWN_MMRESULT';
                                    HrDescr := 'An unrecognized MMRESULT occurred in the mixer.';
                                  end;
    LongInt($C00D0FA9)          : begin
                                    HrStr := 'NS_E_USER_STOP';
                                    HrDescr := 'User has stopped the operation.';
                                  end;
    LongInt($C00D0FAA)          : begin
                                    HrStr := 'NS_E_MP3_FORMAT_NOT_FOUND';
                                    HrDescr := 'Windows Media Player cannot rip the track because a compatible MP3 encoder is not installed on your computer.' +
                                               'Install a compatible MP3 encoder or choose a different format to rip to (such as Windows Media Audio).';
                                  end;
    LongInt($C00D0FAB)          : begin
                                    HrStr := 'NS_E_CD_READ_ERROR_NO_CORRECTION';
                                    HrDescr := 'Windows Media Player cannot read the CD.' +
                                               'The disc might be dirty or damaged.' +
                                               'Turn on error correction, and then try again.';
                                  end;
    LongInt($C00D0FAC)          : begin
                                    HrStr := 'NS_E_CD_READ_ERROR';
                                    HrDescr := 'Windows Media Player cannot read the CD.' +
                                               'The disc might be dirty or damaged or the CD drive might be malfunctioning.';
                                  end;
    LongInt($C00D0FAD)          : begin
                                    HrStr := 'NS_E_CD_SLOW_COPY';
                                    HrDescr := 'For best performance, do not play CD tracks while ripping them.';
                                  end;
    LongInt($C00D0FAE)          : begin
                                    HrStr := 'NS_E_CD_COPYTO_CD';
                                    HrDescr := 'It is not possible to directly burn tracks from one CD to another CD.' +
                                               'You must first rip the tracks from the CD to your computer, and then burn the files to a blank CD.';
                                  end;
    LongInt($C00D0FAF)          : begin
                                    HrStr := 'NS_E_MIXER_NODRIVER';
                                    HrDescr := 'Could not open a sound mixer driver.';
                                  end;
    LongInt($C00D0FB0)          : begin
                                    HrStr := 'NS_E_REDBOOK_ENABLED_WHILE_COPYING';
                                    HrDescr := 'Windows Media Player cannot rip tracks from the CD correctly because the CD drive settings in Device Manager do not match the CD drive settings in the Player.';
                                  end;
    LongInt($C00D0FB1)          : begin
                                    HrStr := 'NS_E_CD_REFRESH';
                                    HrDescr := 'Windows Media Player is busy reading the CD.';
                                  end;
    LongInt($C00D0FB2)          : begin
                                    HrStr := 'NS_E_CD_DRIVER_PROBLEM';
                                    HrDescr := 'Windows Media Player could not use digital playback to play the CD.' +
                                               'The Player has automatically switched the CD drive to analog playback.' +
                                               'To switch back to digital CD playback, use the Devices tab.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FB3)          : begin
                                    HrStr := 'NS_E_WONT_DO_DIGITAL';
                                    HrDescr := 'Windows Media Player could not use digital playback to play the CD.' +
                                               'The Player has automatically switched the CD drive to analog playback.' +
                                               'To switch back to digital CD playback, use the Devices tab.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FB4)          : begin
                                    HrStr := 'NS_E_WMPXML_NOERROR';
                                    HrDescr := 'A call was made to GetParseError on the XML parser but there was no error to retrieve.';
                                  end;
    LongInt($C00D0FB5)          : begin
                                    HrStr := 'NS_E_WMPXML_ENDOFDATA';
                                    HrDescr := 'The XML Parser ran out of data while parsing.';
                                  end;
    LongInt($C00D0FB6)          : begin
                                    HrStr := 'NS_E_WMPXML_PARSEERROR';
                                    HrDescr := 'A generic parse error occurred in the XML parser but no information is available.';
                                  end;
    LongInt($C00D0FB7)          : begin
                                    HrStr := 'NS_E_WMPXML_ATTRIBUTENOTFOUND';
                                    HrDescr := 'A call get GetNamedAttribute or GetNamedAttributeIndex on the XML parser resulted in the index not being found.';
                                  end;
    LongInt($C00D0FB8)          : begin
                                    HrStr := 'NS_E_WMPXML_PINOTFOUND';
                                    HrDescr := 'A call was made go GetNamedPI on the XML parser, but the requested Processing Instruction was not found.';
                                  end;
    LongInt($C00D0FB9)          : begin
                                    HrStr := 'NS_E_WMPXML_EMPTYDOC';
                                    HrDescr := 'Persist was called on the XML parser, but the parser has no data to persist.';
                                  end;
    LongInt($C00D0FBA)          : begin
                                    HrStr := 'NS_E_WMP_PATH_ALREADY_IN_LIBRARY';
                                    HrDescr := 'This file path is already in the library.';
                                  end;
    LongInt($C00D0FBE)          : begin
                                    HrStr := 'NS_E_WMP_FILESCANALREADYSTARTED';
                                    HrDescr := 'Windows Media Player is already searching for files to add to your library.' +
                                               'Wait for the current process to finish before attempting to search again.';
                                  end;
    LongInt($C00D0FBF)          : begin
                                    HrStr := 'NS_E_WMP_HME_INVALIDOBJECTID';
                                    HrDescr := 'Windows Media Player is unable to find the media you are looking for.';
                                  end;
    LongInt($C00D0FC0)          : begin
                                    HrStr := 'NS_E_WMP_MF_CODE_EXPIRED';
                                    HrDescr := 'A component of Windows Media Player is out-of-date.' +
                                               'If you are running a pre-release version of Windows, try upgrading to a more recent version.';
                                  end;
    LongInt($C00D0FC1)          : begin
                                    HrStr := 'NS_E_WMP_HME_NOTSEARCHABLEFORITEMS';
                                    HrDescr := 'This container does not support search on items.';
                                  end;
    LongInt($C00D0FC7)          : begin
                                    HrStr := 'NS_E_WMP_ADDTOLIBRARY_FAILED';
                                    HrDescr := 'Windows Media Player encountered a problem while adding one or more files to the library.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FC8)          : begin
                                    HrStr := 'NS_E_WMP_WINDOWSAPIFAILURE';
                                    HrDescr := 'A Windows API call failed but no error information was available.';
                                  end;
    LongInt($C00D0FC9)          : begin
                                    HrStr := 'NS_E_WMP_RECORDING_NOT_ALLOWED';
                                    HrDescr := 'This file does not have burn rights.' +
                                               'If you obtained this file from an online store, go to the online store to get burn rights.';
                                  end;
    LongInt($C00D0FCA)          : begin
                                    HrStr := 'NS_E_DEVICE_NOT_READY';
                                    HrDescr := 'Windows Media Player no longer detects a connected portable device.' +
                                               'Reconnect your portable device, and then try to sync the file again.';
                                  end;
    LongInt($C00D0FCB)          : begin
                                    HrStr := 'NS_E_DAMAGED_FILE';
                                    HrDescr := 'Windows Media Player cannot play the file because it is corrupted.';
                                  end;
    LongInt($C00D0FCC)          : begin
                                    HrStr := 'NS_E_MPDB_GENERIC';
                                    HrDescr := 'Windows Media Player encountered an error while attempting to access information in the library.' +
                                               'Try restarting the Player.';
                                  end;
    LongInt($C00D0FCD)          : begin
                                    HrStr := 'NS_E_FILE_FAILED_CHECKS';
                                    HrDescr := 'The file cannot be added to the library because it is smaller than the "Skip files smaller than" setting.' +
                                               'To add the file, change the setting on the Library tab.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FCE)          : begin
                                    HrStr := 'NS_E_MEDIA_LIBRARY_FAILED';
                                    HrDescr := 'Windows Media Player cannot create the library.' +
                                               'You must be logged on as an administrator or a member of the Administrators group to install the Player.' +
                                               'For more information, contact your system administrator.';
                                  end;
    LongInt($C00D0FCF)          : begin
                                    HrStr := 'NS_E_SHARING_VIOLATION';
                                    HrDescr := 'The file is already in use.' +
                                               'Close other programs that might be using the file, or stop playing the file, and then try again.';
                                  end;
    LongInt($C00D0FD0)          : begin
                                    HrStr := 'NS_E_NO_ERROR_STRING_FOUND';
                                    HrDescr := 'Windows Media Player has encountered an unknown error.';
                                  end;
    LongInt($C00D0FD1)          : begin
                                    HrStr := 'NS_E_WMPOCX_NO_REMOTE_CORE';
                                    HrDescr := 'The Windows Media Player ActiveX control cannot connect to remote media services, but will continue with local media services.';
                                  end;
    LongInt($C00D0FD2)          : begin
                                    HrStr := 'NS_E_WMPOCX_NO_ACTIVE_CORE';
                                    HrDescr := 'The requested method or property is not available because the Windows Media Player ActiveX control has not been properly activated.';
                                  end;
    LongInt($C00D0FD3)          : begin
                                    HrStr := 'NS_E_WMPOCX_NOT_RUNNING_REMOTELY';
                                    HrDescr := 'The Windows Media Player ActiveX control is not running in remote mode.';
                                  end;
    LongInt($C00D0FD4)          : begin
                                    HrStr := 'NS_E_WMPOCX_NO_REMOTE_WINDOW';
                                    HrDescr := 'An error occurred while trying to get the remote Windows Media Player window.';
                                  end;
    LongInt($C00D0FD5)          : begin
                                    HrStr := 'NS_E_WMPOCX_ERRORMANAGERNOTAVAILABLE';
                                    HrDescr := 'Windows Media Player has encountered an unknown error.';
                                  end;
    LongInt($C00D0FD6)          : begin
                                    HrStr := 'NS_E_PLUGIN_NOTSHUTDOWN';
                                    HrDescr := 'Windows Media Player was not closed properly.' +
                                               'A damaged or incompatible plug-in might have caused the problem to occur.' +
                                               'As a precaution, all optional plug-ins have been disabled.';
                                  end;
    LongInt($C00D0FD7)          : begin
                                    HrStr := 'NS_E_WMP_CANNOT_FIND_FOLDER';
                                    HrDescr := 'Windows Media Player cannot find the specified path.' +
                                               'Verify that the path is typed correctly.' +
                                               'If it is, the path does not exist in the specified location, or the computer where the path is located is not available.';
                                  end;
    LongInt($C00D0FD8)          : begin
                                    HrStr := 'NS_E_WMP_STREAMING_RECORDING_NOT_ALLOWED';
                                    HrDescr := 'Windows Media Player cannot save a file that is being streamed.';
                                  end;
    LongInt($C00D0FD9)          : begin
                                    HrStr := 'NS_E_WMP_PLUGINDLL_NOTFOUND';
                                    HrDescr := 'Windows Media Player cannot find the selected plug-in.' +
                                               'The Player will try to remove it from the menu.' +
                                               'To use this plug-in, install it again.';
                                  end;
    LongInt($C00D0FDA)          : begin
                                    HrStr := 'NS_E_NEED_TO_ASK_USER';
                                    HrDescr := 'Action requires input from the user.';
                                  end;
    LongInt($C00D0FDB)          : begin
                                    HrStr := 'NS_E_WMPOCX_PLAYER_NOT_DOCKED';
                                    HrDescr := 'The Windows Media Player ActiveX control must be in a docked state for this action to be performed.';
                                  end;
    LongInt($C00D0FDC)          : begin
                                    HrStr := 'NS_E_WMP_EXTERNAL_NOTREADY';
                                    HrDescr := 'The Windows Media Player external object is not ready.';
                                  end;
    LongInt($C00D0FDD)          : begin
                                    HrStr := 'NS_E_WMP_MLS_STALE_DATA';
                                    HrDescr := 'Windows Media Player cannot perform the requested action.' +
                                               'Your computer''s time and date might not be set correctly.';
                                  end;
    LongInt($C00D0FDE)          : begin
                                    HrStr := 'NS_E_WMP_UI_SUBCONTROLSNOTSUPPORTED';
                                    HrDescr := 'The control (%s) does not support creation of sub-controls, yet (%d) sub-controls have been specified.';
                                  end;
    LongInt($C00D0FDF)          : begin
                                    HrStr := 'NS_E_WMP_UI_VERSIONMISMATCH';
                                    HrDescr := 'Version mismatch: (%.1f required, %.1f found).';
                                  end;
    LongInt($C00D0FE0)          : begin
                                    HrStr := 'NS_E_WMP_UI_NOTATHEMEFILE';
                                    HrDescr := 'The layout manager was given valid XML that wasn''t a theme file.';
                                  end;
    LongInt($C00D0FE1)          : begin
                                    HrStr := 'NS_E_WMP_UI_SUBELEMENTNOTFOUND';
                                    HrDescr := 'The %s subelement could not be found on the %s object.';
                                  end;
    LongInt($C00D0FE2)          : begin
                                    HrStr := 'NS_E_WMP_UI_VERSIONPARSE';
                                    HrDescr := 'An error occurred parsing the version tag.' +
                                               'Valid version tags are of the form: <?wmp version=''1.0''?>.';
                                  end;
    LongInt($C00D0FE3)          : begin
                                    HrStr := 'NS_E_WMP_UI_VIEWIDNOTFOUND';
                                    HrDescr := 'The view specified in for the ''currentViewID'' property (%s) was not found in this theme file.';
                                  end;
    LongInt($C00D0FE4)          : begin
                                    HrStr := 'NS_E_WMP_UI_PASSTHROUGH';
                                    HrDescr := 'This error used internally for hit testing.';
                                  end;
    LongInt($C00D0FE5)          : begin
                                    HrStr := 'NS_E_WMP_UI_OBJECTNOTFOUND';
                                    HrDescr := 'Attributes were specified for the %s object, but the object was not available to send them to.';
                                  end;
    LongInt($C00D0FE6)          : begin
                                    HrStr := 'NS_E_WMP_UI_SECONDHANDLER';
                                    HrDescr := 'The %s event already has a handler, the second handler was ignored.';
                                  end;
    LongInt($C00D0FE7)          : begin
                                    HrStr := 'NS_E_WMP_UI_NOSKININZIP';
                                    HrDescr := 'No .wms file found in skin archive.';
                                  end;
    LongInt($C00D0FEA)          : begin
                                    HrStr := 'NS_E_WMP_URLDOWNLOADFAILED';
                                    HrDescr := 'Windows Media Player encountered a problem while downloading the file.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FEB)          : begin
                                    HrStr := 'NS_E_WMPOCX_UNABLE_TO_LOAD_SKIN';
                                    HrDescr := 'The Windows Media Player ActiveX control cannot load the requested uiMode and cannot roll back to the existing uiMode.';
                                  end;
    LongInt($C00D0FEC)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_SKIN';
                                    HrDescr := 'Windows Media Player encountered a problem with the skin file.' +
                                               'The skin file might not be valid.';
                                  end;
    LongInt($C00D0FED)          : begin
                                    HrStr := 'NS_E_WMP_SENDMAILFAILED';
                                    HrDescr := 'Windows Media Player cannot send the link because your email program is not responding.' +
                                               'Verify that your email program is configured properly, and then try again.' +
                                               'For more information about email, see Windows Help.';
                                  end;
    LongInt($C00D0FEE)          : begin
                                    HrStr := 'NS_E_WMP_LOCKEDINSKINMODE';
                                    HrDescr := 'Windows Media Player cannot switch to full mode because your computer administrator has locked this skin.';
                                  end;
    LongInt($C00D0FEF)          : begin
                                    HrStr := 'NS_E_WMP_FAILED_TO_SAVE_FILE';
                                    HrDescr := 'Windows Media Player encountered a problem while saving the file.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FF0)          : begin
                                    HrStr := 'NS_E_WMP_SAVEAS_READONLY';
                                    HrDescr := 'Windows Media Player cannot overwrite a read-only file.' +
                                               'Try using a different file name.';
                                  end;
    LongInt($C00D0FF1)          : begin
                                    HrStr := 'NS_E_WMP_FAILED_TO_SAVE_PLAYLIST';
                                    HrDescr := 'Windows Media Player encountered a problem while creating or saving the playlist.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FF2)          : begin
                                    HrStr := 'NS_E_WMP_FAILED_TO_OPEN_WMD';
                                    HrDescr := 'Windows Media Player cannot open the Windows Media Download file.' +
                                               'The file might be damaged.';
                                  end;
    LongInt($C00D0FF3)          : begin
                                    HrStr := 'NS_E_WMP_CANT_PLAY_PROTECTED';
                                    HrDescr := 'The file cannot be added to the library because it is a protected DVR-MS file.' +
                                               'This content cannot be played back by Windows Media Player.';
                                  end;
    LongInt($C00D0FF4)          : begin
                                    HrStr := 'NS_E_SHARING_STATE_OUT_OF_SYNC';
                                    HrDescr := 'Media sharing has been turned off because a required Windows setting or component has changed.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D0FFA)          : begin
                                    HrStr := 'NS_E_WMPOCX_REMOTE_PLAYER_ALREADY_RUNNING';
                                    HrDescr := 'Exclusive Services launch failed because the Windows Media Player is already running.';
                                  end;
    LongInt($C00D1004)          : begin
                                    HrStr := 'NS_E_WMP_RBC_JPGMAPPINGIMAGE';
                                    HrDescr := 'JPG Images are not recommended for use as a mappingImage.';
                                  end;
    LongInt($C00D1005)          : begin
                                    HrStr := 'NS_E_WMP_JPGTRANSPARENCY';
                                    HrDescr := 'JPG Images are not recommended when using a transparencyColor.';
                                  end;
    LongInt($C00D1009)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_MAX_VAL';
                                    HrDescr := 'The Max property cannot be less than Min property.';
                                  end;
    LongInt($C00D100A)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_MIN_VAL';
                                    HrDescr := 'The Min property cannot be greater than Max property.';
                                  end;
    LongInt($C00D100E)          : begin
                                    HrStr := 'NS_E_WMP_CS_JPGPOSITIONIMAGE';
                                    HrDescr := 'JPG Images are not recommended for use as a positionImage.';
                                  end;
    LongInt($C00D100F)          : begin
                                    HrStr := 'NS_E_WMP_CS_NOTEVENLYDIVISIBLE';
                                    HrDescr := 'The (%s) image''s size is not evenly divisible by the positionImage''s size.';
                                  end;
    LongInt($C00D1018)          : begin
                                    HrStr := 'NS_E_WMPZIP_NOTAZIPFILE';
                                    HrDescr := 'The ZIP reader opened a file and its signature did not match that of the ZIP files.';
                                  end;
    LongInt($C00D1019)          : begin
                                    HrStr := 'NS_E_WMPZIP_CORRUPT';
                                    HrDescr := 'The ZIP reader has detected that the file is corrupted.';
                                  end;
    LongInt($C00D101A)          : begin
                                    HrStr := 'NS_E_WMPZIP_FILENOTFOUND';
                                    HrDescr := 'GetFileStream, SaveToFile, or SaveTemp file was called on the ZIP reader with a file name that was not found in the ZIP file.';
                                  end;
    LongInt($C00D1022)          : begin
                                    HrStr := 'NS_E_WMP_IMAGE_FILETYPE_UNSUPPORTED';
                                    HrDescr := 'Image type not supported.';
                                  end;
    LongInt($C00D1023)          : begin
                                    HrStr := 'NS_E_WMP_IMAGE_INVALID_FORMAT';
                                    HrDescr := 'Image file might be corrupt.';
                                  end;
    LongInt($C00D1024)          : begin
                                    HrStr := 'NS_E_WMP_GIF_UNEXPECTED_ENDOFFILE';
                                    HrDescr := 'Unexpected end of file.' +
                                               'GIF file might be corrupt.';
                                  end;
    LongInt($C00D1025)          : begin
                                    HrStr := 'NS_E_WMP_GIF_INVALID_FORMAT';
                                    HrDescr := 'Invalid GIF file.';
                                  end;
    LongInt($C00D1026)          : begin
                                    HrStr := 'NS_E_WMP_GIF_BAD_VERSION_NUMBER';
                                    HrDescr := 'Invalid GIF version.' +
                                               'Only 87a or 89a supported.';
                                  end;
    LongInt($C00D1027)          : begin
                                    HrStr := 'NS_E_WMP_GIF_NO_IMAGE_IN_FILE';
                                    HrDescr := 'No images found in GIF file.';
                                  end;
    LongInt($C00D1028)          : begin
                                    HrStr := 'NS_E_WMP_PNG_INVALIDFORMAT';
                                    HrDescr := 'Invalid PNG image file format.';
                                  end;
    LongInt($C00D1029)          : begin
                                    HrStr := 'NS_E_WMP_PNG_UNSUPPORTED_BITDEPTH';
                                    HrDescr := 'PNG bitdepth not supported.';
                                  end;
    LongInt($C00D102A)          : begin
                                    HrStr := 'NS_E_WMP_PNG_UNSUPPORTED_COMPRESSION';
                                    HrDescr := 'Compression format defined in PNG file not supported,';
                                  end;
    LongInt($C00D102B)          : begin
                                    HrStr := 'NS_E_WMP_PNG_UNSUPPORTED_FILTER';
                                    HrDescr := 'Filter method defined in PNG file not supported.';
                                  end;
    LongInt($C00D102C)          : begin
                                    HrStr := 'NS_E_WMP_PNG_UNSUPPORTED_INTERLACE';
                                    HrDescr := 'Interlace method defined in PNG file not supported.';
                                  end;
    LongInt($C00D102D)          : begin
                                    HrStr := 'NS_E_WMP_PNG_UNSUPPORTED_BAD_CRC';
                                    HrDescr := 'Bad CRC in PNG file.';
                                  end;
    LongInt($C00D102E)          : begin
                                    HrStr := 'NS_E_WMP_BMP_INVALID_BITMASK';
                                    HrDescr := 'Invalid bitmask in BMP file.';
                                  end;
    LongInt($C00D102F)          : begin
                                    HrStr := 'NS_E_WMP_BMP_TOPDOWN_DIB_UNSUPPORTED';
                                    HrDescr := 'Topdown DIB not supported.';
                                  end;
    LongInt($C00D1030)          : begin
                                    HrStr := 'NS_E_WMP_BMP_BITMAP_NOT_CREATED';
                                    HrDescr := 'Bitmap could not be created.';
                                  end;
    LongInt($C00D1031)          : begin
                                    HrStr := 'NS_E_WMP_BMP_COMPRESSION_UNSUPPORTED';
                                    HrDescr := 'Compression format defined in BMP not supported.';
                                  end;
    LongInt($C00D1032)          : begin
                                    HrStr := 'NS_E_WMP_BMP_INVALID_FORMAT';
                                    HrDescr := 'Invalid Bitmap format.';
                                  end;
    LongInt($C00D1033)          : begin
                                    HrStr := 'NS_E_WMP_JPG_JERR_ARITHCODING_NOTIMPL';
                                    HrDescr := 'JPEG Arithmetic coding not supported.';
                                  end;
    LongInt($C00D1034)          : begin
                                    HrStr := 'NS_E_WMP_JPG_INVALID_FORMAT';
                                    HrDescr := 'Invalid JPEG format.';
                                  end;
    LongInt($C00D1035)          : begin
                                    HrStr := 'NS_E_WMP_JPG_BAD_DCTSIZE';
                                    HrDescr := 'Invalid JPEG format.';
                                  end;
    LongInt($C00D1036)          : begin
                                    HrStr := 'NS_E_WMP_JPG_BAD_VERSION_NUMBER';
                                    HrDescr := 'Internal version error.' +
                                               'Unexpected JPEG library version.';
                                  end;
    LongInt($C00D1037)          : begin
                                    HrStr := 'NS_E_WMP_JPG_BAD_PRECISION';
                                    HrDescr := 'Internal JPEG Library error.' +
                                               'Unsupported JPEG data precision.';
                                  end;
    LongInt($C00D1038)          : begin
                                    HrStr := 'NS_E_WMP_JPG_CCIR601_NOTIMPL';
                                    HrDescr := 'JPEG CCIR601 not supported.';
                                  end;
    LongInt($C00D1039)          : begin
                                    HrStr := 'NS_E_WMP_JPG_NO_IMAGE_IN_FILE';
                                    HrDescr := 'No image found in JPEG file.';
                                  end;
    LongInt($C00D103A)          : begin
                                    HrStr := 'NS_E_WMP_JPG_READ_ERROR';
                                    HrDescr := 'Could not read JPEG file.';
                                  end;
    LongInt($C00D103B)          : begin
                                    HrStr := 'NS_E_WMP_JPG_FRACT_SAMPLE_NOTIMPL';
                                    HrDescr := 'JPEG Fractional sampling not supported.';
                                  end;
    LongInt($C00D103C)          : begin
                                    HrStr := 'NS_E_WMP_JPG_IMAGE_TOO_BIG';
                                    HrDescr := 'JPEG image too large.' +
                                               'Maximum image size supported is 65500 X 65500.';
                                  end;
    LongInt($C00D103D)          : begin
                                    HrStr := 'NS_E_WMP_JPG_UNEXPECTED_ENDOFFILE';
                                    HrDescr := 'Unexpected end of file reached in JPEG file.';
                                  end;
    LongInt($C00D103E)          : begin
                                    HrStr := 'NS_E_WMP_JPG_SOF_UNSUPPORTED';
                                    HrDescr := 'Unsupported JPEG SOF marker found.';
                                  end;
    LongInt($C00D103F)          : begin
                                    HrStr := 'NS_E_WMP_JPG_UNKNOWN_MARKER';
                                    HrDescr := 'Unknown JPEG marker found.';
                                  end;
    LongInt($C00D1044)          : begin
                                    HrStr := 'NS_E_WMP_FAILED_TO_OPEN_IMAGE';
                                    HrDescr := 'Windows Media Player cannot display the picture file.' +
                                               'The player either does not support the picture type or the picture is corrupted.';
                                  end;
    LongInt($C00D1049)          : begin
                                    HrStr := 'NS_E_WMP_DAI_SONGTOOSHORT';
                                    HrDescr := 'Windows Media Player cannot compute a Digital Audio Id for the song.' +
                                               'It is too short.';
                                  end;
    LongInt($C00D104A)          : begin
                                    HrStr := 'NS_E_WMG_RATEUNAVAILABLE';
                                    HrDescr := 'Windows Media Player cannot play the file at the requested speed.';
                                  end;
    LongInt($C00D104B)          : begin
                                    HrStr := 'NS_E_WMG_PLUGINUNAVAILABLE';
                                    HrDescr := 'The rendering or digital signal processing plug-in cannot be instantiated.';
                                  end;
    LongInt($C00D104C)          : begin
                                    HrStr := 'NS_E_WMG_CANNOTQUEUE';
                                    HrDescr := 'The file cannot be queued for seamless playback.';
                                  end;
    LongInt($C00D104D)          : begin
                                    HrStr := 'NS_E_WMG_PREROLLLICENSEACQUISITIONNOTALLOWED';
                                    HrDescr := 'Windows Media Player cannot download media usage rights for a file in the playlist.';
                                  end;
    LongInt($C00D104E)          : begin
                                    HrStr := 'NS_E_WMG_UNEXPECTEDPREROLLSTATUS';
                                    HrDescr := 'Windows Media Player encountered an error while trying to queue a file.';
                                  end;
    LongInt($C00D1051)          : begin
                                    HrStr := 'NS_E_WMG_INVALID_COPP_CERTIFICATE';
                                    HrDescr := 'Windows Media Player cannot play the protected file.' +
                                               'The Player cannot verify that the connection to your video card is secure.' +
                                               'Try installing an updated device driver for your video card.';
                                  end;
    LongInt($C00D1052)          : begin
                                    HrStr := 'NS_E_WMG_COPP_SECURITY_INVALID';
                                    HrDescr := 'Windows Media Player cannot play the protected file.' +
                                               'The Player detected that the connection to your hardware might not be secure.';
                                  end;
    LongInt($C00D1053)          : begin
                                    HrStr := 'NS_E_WMG_COPP_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player output link protection is unsupported on this system.';
                                  end;
    LongInt($C00D1054)          : begin
                                    HrStr := 'NS_E_WMG_INVALIDSTATE';
                                    HrDescr := 'Operation attempted in an invalid graph state.';
                                  end;
    LongInt($C00D1055)          : begin
                                    HrStr := 'NS_E_WMG_SINKALREADYEXISTS';
                                    HrDescr := 'A renderer cannot be inserted in a stream while one already exists.';
                                  end;
    LongInt($C00D1056)          : begin
                                    HrStr := 'NS_E_WMG_NOSDKINTERFACE';
                                    HrDescr := 'The Windows Media SDK interface needed to complete the operation does not exist at this time.';
                                  end;
    LongInt($C00D1057)          : begin
                                    HrStr := 'NS_E_WMG_NOTALLOUTPUTSRENDERED';
                                    HrDescr := 'Windows Media Player cannot play a portion of the file because it requires a codec that either could not be downloaded or that is not supported by the Player.';
                                  end;
    LongInt($C00D1058)          : begin
                                    HrStr := 'NS_E_WMG_FILETRANSFERNOTALLOWED';
                                    HrDescr := 'File transfer streams are not allowed in the standalone Player.';
                                  end;
    LongInt($C00D1059)          : begin
                                    HrStr := 'NS_E_WMR_UNSUPPORTEDSTREAM';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'The Player does not support the format you are trying to play.';
                                  end;
    LongInt($C00D105A)          : begin
                                    HrStr := 'NS_E_WMR_PINNOTFOUND';
                                    HrDescr := 'An operation was attempted on a pin that does not exist in the DirectShow filter graph.';
                                  end;
    LongInt($C00D105B)          : begin
                                    HrStr := 'NS_E_WMR_WAITINGONFORMATSWITCH';
                                    HrDescr := 'Specified operation cannot be completed while waiting for a media format change from the SDK.';
                                  end;
    LongInt($C00D105C)          : begin
                                    HrStr := 'NS_E_WMR_NOSOURCEFILTER';
                                    HrDescr := 'Specified operation cannot be completed because the source filter does not exist.';
                                  end;
    LongInt($C00D105D)          : begin
                                    HrStr := 'NS_E_WMR_PINTYPENOMATCH';
                                    HrDescr := 'The specified type does not match this pin.';
                                  end;
    LongInt($C00D105E)          : begin
                                    HrStr := 'NS_E_WMR_NOCALLBACKAVAILABLE';
                                    HrDescr := 'The WMR Source Filter does not have a callback available.';
                                  end;
    LongInt($C00D1062)          : begin
                                    HrStr := 'NS_E_WMR_SAMPLEPROPERTYNOTSET';
                                    HrDescr := 'The specified property has not been set on this sample.';
                                  end;
    LongInt($C00D1063)          : begin
                                    HrStr := 'NS_E_WMR_CANNOT_RENDER_BINARY_STREAM';
                                    HrDescr := 'A plug-in is required to correctly play the file.' +
                                               'To determine if the plug-in is available to download, click Web Help.';
                                  end;
    LongInt($C00D1064)          : begin
                                    HrStr := 'NS_E_WMG_LICENSE_TAMPERED';
                                    HrDescr := 'Windows Media Player cannot play the file because your media usage rights are corrupted.' +
                                               'If you previously backed up your media usage rights, try restoring them.';
                                  end;
    LongInt($C00D1065)          : begin
                                    HrStr := 'NS_E_WMR_WILLNOT_RENDER_BINARY_STREAM';
                                    HrDescr := 'Windows Media Player cannot play protected files that contain binary streams.';
                                  end;
    LongInt($C00D1068)          : begin
                                    HrStr := 'NS_E_WMX_UNRECOGNIZED_PLAYLIST_FORMAT';
                                    HrDescr := 'Windows Media Player cannot play the playlist because it is not valid.';
                                  end;
    LongInt($C00D1069)          : begin
                                    HrStr := 'NS_E_ASX_INVALIDFORMAT';
                                    HrDescr := 'Windows Media Player cannot play the playlist because it is not valid.';
                                  end;
    LongInt($C00D106A)          : begin
                                    HrStr := 'NS_E_ASX_INVALIDVERSION';
                                    HrDescr := 'A later version of Windows Media Player might be required to play this playlist.';
                                  end;
    LongInt($C00D106B)          : begin
                                    HrStr := 'NS_E_ASX_INVALID_REPEAT_BLOCK';
                                    HrDescr := 'The format of a REPEAT loop within the current playlist file is not valid.';
                                  end;
    LongInt($C00D106C)          : begin
                                    HrStr := 'NS_E_ASX_NOTHING_TO_WRITE';
                                    HrDescr := 'Windows Media Player cannot save the playlist because it does not contain any items.';
                                  end;
    LongInt($C00D106D)          : begin
                                    HrStr := 'NS_E_URLLIST_INVALIDFORMAT';
                                    HrDescr := 'Windows Media Player cannot play the playlist because it is not valid.';
                                  end;
    LongInt($C00D106E)          : begin
                                    HrStr := 'NS_E_WMX_ATTRIBUTE_DOES_NOT_EXIST';
                                    HrDescr := 'The specified attribute does not exist.';
                                  end;
    LongInt($C00D106F)          : begin
                                    HrStr := 'NS_E_WMX_ATTRIBUTE_ALREADY_EXISTS';
                                    HrDescr := 'The specified attribute already exists.';
                                  end;
    LongInt($C00D1070)          : begin
                                    HrStr := 'NS_E_WMX_ATTRIBUTE_UNRETRIEVABLE';
                                    HrDescr := 'Cannot retrieve the specified attribute.';
                                  end;
    LongInt($C00D1071)          : begin
                                    HrStr := 'NS_E_WMX_ITEM_DOES_NOT_EXIST';
                                    HrDescr := 'The specified item does not exist in the current playlist.';
                                  end;
    LongInt($C00D1072)          : begin
                                    HrStr := 'NS_E_WMX_ITEM_TYPE_ILLEGAL';
                                    HrDescr := 'Items of the specified type cannot be created within the current playlist.';
                                  end;
    LongInt($C00D1073)          : begin
                                    HrStr := 'NS_E_WMX_ITEM_UNSETTABLE';
                                    HrDescr := 'The specified item cannot be set in the current playlist.';
                                  end;
    LongInt($C00D1074)          : begin
                                    HrStr := 'NS_E_WMX_PLAYLIST_EMPTY';
                                    HrDescr := 'Windows Media Player cannot perform the requested action because the playlist does not contain any items.';
                                  end;
    LongInt($C00D1075)          : begin
                                    HrStr := 'NS_E_MLS_SMARTPLAYLIST_FILTER_NOT_REGISTERED';
                                    HrDescr := 'The specified auto playlist contains a filter type that is either not valid or is not installed on this computer.';
                                  end;
    LongInt($C00D1076)          : begin
                                    HrStr := 'NS_E_WMX_INVALID_FORMAT_OVER_NESTING';
                                    HrDescr := 'Windows Media Player cannot play the file because the associated playlist contains too many nested playlists.';
                                  end;
    LongInt($C00D107C)          : begin
                                    HrStr := 'NS_E_WMPCORE_NOSOURCEURLSTRING';
                                    HrDescr := 'Windows Media Player cannot find the file.' +
                                               'Verify that the path is typed correctly.' +
                                               'If it is, the file might not exist in the specified location, or the computer where the file is stored might not be available.';
                                  end;
    LongInt($C00D107D)          : begin
                                    HrStr := 'NS_E_WMPCORE_COCREATEFAILEDFORGITOBJECT';
                                    HrDescr := 'Failed to create the Global Interface Table.';
                                  end;
    LongInt($C00D107E)          : begin
                                    HrStr := 'NS_E_WMPCORE_FAILEDTOGETMARSHALLEDEVENTHANDLERINTERFACE';
                                    HrDescr := 'Failed to get the marshaled graph event handler interface.';
                                  end;
    LongInt($C00D107F)          : begin
                                    HrStr := 'NS_E_WMPCORE_BUFFERTOOSMALL';
                                    HrDescr := 'Buffer is too small for copying media type.';
                                  end;
    LongInt($C00D1080)          : begin
                                    HrStr := 'NS_E_WMPCORE_UNAVAILABLE';
                                    HrDescr := 'The current state of the Player does not allow this operation.';
                                  end;
    LongInt($C00D1081)          : begin
                                    HrStr := 'NS_E_WMPCORE_INVALIDPLAYLISTMODE';
                                    HrDescr := 'The playlist manager does not understand the current play mode (for example, shuffle or normal).';
                                  end;
    LongInt($C00D1086)          : begin
                                    HrStr := 'NS_E_WMPCORE_ITEMNOTINPLAYLIST';
                                    HrDescr := 'Windows Media Player cannot play the file because it is not in the current playlist.';
                                  end;
    LongInt($C00D1087)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLISTEMPTY';
                                    HrDescr := 'There are no items in the playlist.' +
                                               'Add items to the playlist, and then try again.';
                                  end;
    LongInt($C00D1088)          : begin
                                    HrStr := 'NS_E_WMPCORE_NOBROWSER';
                                    HrDescr := 'The web page cannot be displayed because no web browser is installed on your computer.';
                                  end;
    LongInt($C00D1089)          : begin
                                    HrStr := 'NS_E_WMPCORE_UNRECOGNIZED_MEDIA_URL';
                                    HrDescr := 'Windows Media Player cannot find the specified file.' +
                                               'Verify the path is typed correctly.' +
                                               'If it is, the file does not exist in the specified location, or the computer where the file is stored is not available.';
                                  end;
    LongInt($C00D108A)          : begin
                                    HrStr := 'NS_E_WMPCORE_GRAPH_NOT_IN_LIST';
                                    HrDescr := 'Graph with the specified URL was not found in the prerolled graph list.';
                                  end;
    LongInt($C00D108B)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_EMPTY_OR_SINGLE_MEDIA';
                                    HrDescr := 'Windows Media Player cannot perform the requested operation because there is only one item in the playlist.';
                                  end;
    LongInt($C00D108C)          : begin
                                    HrStr := 'NS_E_WMPCORE_ERRORSINKNOTREGISTERED';
                                    HrDescr := 'An error sink was never registered for the calling object.';
                                  end;
    LongInt($C00D108D)          : begin
                                    HrStr := 'NS_E_WMPCORE_ERRORMANAGERNOTAVAILABLE';
                                    HrDescr := 'The error manager is not available to respond to errors.';
                                  end;
    LongInt($C00D108E)          : begin
                                    HrStr := 'NS_E_WMPCORE_WEBHELPFAILED';
                                    HrDescr := 'The Web Help URL cannot be opened.';
                                  end;
    LongInt($C00D108F)          : begin
                                    HrStr := 'NS_E_WMPCORE_MEDIA_ERROR_RESUME_FAILED';
                                    HrDescr := 'Could not resume playing next item in playlist.';
                                  end;
    LongInt($C00D1090)          : begin
                                    HrStr := 'NS_E_WMPCORE_NO_REF_IN_ENTRY';
                                    HrDescr := 'Windows Media Player cannot play the file because the associated playlist does not contain any items or the playlist is not valid.';
                                  end;
    LongInt($C00D1091)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_EMPTY';
                                    HrDescr := 'An empty string for playlist attribute name was found.';
                                  end;
    LongInt($C00D1092)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_ILLEGAL';
                                    HrDescr := 'A playlist attribute name that is not valid was found.';
                                  end;
    LongInt($C00D1093)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_EMPTY';
                                    HrDescr := 'An empty string for a playlist attribute value was found.';
                                  end;
    LongInt($C00D1094)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_ILLEGAL';
                                    HrDescr := 'An illegal value for a playlist attribute was found.';
                                  end;
    LongInt($C00D1095)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_EMPTY';
                                    HrDescr := 'An empty string for a playlist item attribute name was found.';
                                  end;
    LongInt($C00D1096)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_ILLEGAL';
                                    HrDescr := 'An illegal value for a playlist item attribute name was found.';
                                  end;
    LongInt($C00D1097)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_VALUE_EMPTY';
                                    HrDescr := 'An illegal value for a playlist item attribute was found.';
                                  end;
    LongInt($C00D1098)          : begin
                                    HrStr := 'NS_E_WMPCORE_LIST_ENTRY_NO_REF';
                                    HrDescr := 'The playlist does not contain any items.';
                                  end;
    LongInt($C00D1099)          : begin
                                    HrStr := 'NS_E_WMPCORE_MISNAMED_FILE';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'The file is either corrupted or the Player does not support the format you are trying to play.';
                                  end;
    LongInt($C00D109A)          : begin
                                    HrStr := 'NS_E_WMPCORE_CODEC_NOT_TRUSTED';
                                    HrDescr := 'The codec downloaded for this file does not appear to be properly signed, so it cannot be installed.';
                                  end;
    LongInt($C00D109B)          : begin
                                    HrStr := 'NS_E_WMPCORE_CODEC_NOT_FOUND';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'One or more codecs required to play the file could not be found.';
                                  end;
    LongInt($C00D109C)          : begin
                                    HrStr := 'NS_E_WMPCORE_CODEC_DOWNLOAD_NOT_ALLOWED';
                                    HrDescr := 'Windows Media Player cannot play the file because a required codec is not installed on your computer.' +
                                               'To try downloading the codec, turn on the "Download codecs automatically" option.';
                                  end;
    LongInt($C00D109D)          : begin
                                    HrStr := 'NS_E_WMPCORE_ERROR_DOWNLOADING_PLAYLIST';
                                    HrDescr := 'Windows Media Player encountered a problem while downloading the playlist.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D109E)          : begin
                                    HrStr := 'NS_E_WMPCORE_FAILED_TO_BUILD_PLAYLIST';
                                    HrDescr := 'Failed to build the playlist.';
                                  end;
    LongInt($C00D109F)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NONE';
                                    HrDescr := 'Playlist has no alternates to switch into.';
                                  end;
    LongInt($C00D10A0)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_EXHAUSTED';
                                    HrDescr := 'No more playlist alternates available to switch to.';
                                  end;
    LongInt($C00D10A1)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NAME_NOT_FOUND';
                                    HrDescr := 'Could not find the name of the alternate playlist to switch into.';
                                  end;
    LongInt($C00D10A2)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_MORPH_FAILED';
                                    HrDescr := 'Failed to switch to an alternate for this media.';
                                  end;
    LongInt($C00D10A3)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_INIT_FAILED';
                                    HrDescr := 'Failed to initialize an alternate for the media.';
                                  end;
    LongInt($C00D10A4)          : begin
                                    HrStr := 'NS_E_WMPCORE_MEDIA_ALTERNATE_REF_EMPTY';
                                    HrDescr := 'No URL specified for the roll over Refs in the playlist file.';
                                  end;
    LongInt($C00D10A5)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_NO_EVENT_NAME';
                                    HrDescr := 'Encountered a playlist with no name.';
                                  end;
    LongInt($C00D10A6)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_EVENT_ATTRIBUTE_ABSENT';
                                    HrDescr := 'A required attribute in the event block of the playlist was not found.';
                                  end;
    LongInt($C00D10A7)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_EVENT_EMPTY';
                                    HrDescr := 'No items were found in the event block of the playlist.';
                                  end;
    LongInt($C00D10A8)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_STACK_EMPTY';
                                    HrDescr := 'No playlist was found while returning from a nested playlist.';
                                  end;
    LongInt($C00D10A9)          : begin
                                    HrStr := 'NS_E_WMPCORE_CURRENT_MEDIA_NOT_ACTIVE';
                                    HrDescr := 'The media item is not active currently.';
                                  end;
    LongInt($C00D10AB)          : begin
                                    HrStr := 'NS_E_WMPCORE_USER_CANCEL';
                                    HrDescr := 'Windows Media Player cannot perform the requested action because you chose to cancel it.';
                                  end;
    LongInt($C00D10AC)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_REPEAT_EMPTY';
                                    HrDescr := 'Windows Media Player encountered a problem with the playlist.' +
                                               'The format of the playlist is not valid.';
                                  end;
    LongInt($C00D10AD)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_REPEAT_START_MEDIA_NONE';
                                    HrDescr := 'Media object corresponding to start of a playlist repeat block was not found.';
                                  end;
    LongInt($C00D10AE)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_REPEAT_END_MEDIA_NONE';
                                    HrDescr := 'Media object corresponding to the end of a playlist repeat block was not found.';
                                  end;
    LongInt($C00D10AF)          : begin
                                    HrStr := 'NS_E_WMPCORE_INVALID_PLAYLIST_URL';
                                    HrDescr := 'The playlist URL supplied to the playlist manager is not valid.';
                                  end;
    LongInt($C00D10B0)          : begin
                                    HrStr := 'NS_E_WMPCORE_MISMATCHED_RUNTIME';
                                    HrDescr := 'Windows Media Player cannot play the file because it is corrupted.';
                                  end;
    LongInt($C00D10B1)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_IMPORT_FAILED_NO_ITEMS';
                                    HrDescr := 'Windows Media Player cannot add the playlist to the library because the playlist does not contain any items.';
                                  end;
    LongInt($C00D10B2)          : begin
                                    HrStr := 'NS_E_WMPCORE_VIDEO_TRANSFORM_FILTER_INSERTION';
                                    HrDescr := 'An error has occurred that could prevent the changing of the video contrast on this media.';
                                  end;
    LongInt($C00D10B3)          : begin
                                    HrStr := 'NS_E_WMPCORE_MEDIA_UNAVAILABLE';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'If the file is located on the Internet, connect to the Internet.' +
                                               'If the file is located on a removable storage card, insert the storage card.';
                                  end;
    LongInt($C00D10B4)          : begin
                                    HrStr := 'NS_E_WMPCORE_WMX_ENTRYREF_NO_REF';
                                    HrDescr := 'The playlist contains an ENTRYREF for which no href was parsed.' +
                                               'Check the syntax of playlist file.';
                                  end;
    LongInt($C00D10B5)          : begin
                                    HrStr := 'NS_E_WMPCORE_NO_PLAYABLE_MEDIA_IN_PLAYLIST';
                                    HrDescr := 'Windows Media Player cannot play any items in the playlist.' +
                                               'To find information about the problem, click the Now Playing tab, and then click the icon next to each file in the List pane.';
                                  end;
    LongInt($C00D10B6)          : begin
                                    HrStr := 'NS_E_WMPCORE_PLAYLIST_EMPTY_NESTED_PLAYLIST_SKIPPED_ITEMS';
                                    HrDescr := 'Windows Media Player cannot play some or all of the items in the playlist because the playlist is nested.';
                                  end;
    LongInt($C00D10B7)          : begin
                                    HrStr := 'NS_E_WMPCORE_BUSY';
                                    HrDescr := 'Windows Media Player cannot play the file at this time.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D10B8)          : begin
                                    HrStr := 'NS_E_WMPCORE_MEDIA_CHILD_PLAYLIST_UNAVAILABLE';
                                    HrDescr := 'There is no child playlist available for this media item at this time.';
                                  end;
    LongInt($C00D10B9)          : begin
                                    HrStr := 'NS_E_WMPCORE_MEDIA_NO_CHILD_PLAYLIST';
                                    HrDescr := 'There is no child playlist for this media item.';
                                  end;
    LongInt($C00D10BA)          : begin
                                    HrStr := 'NS_E_WMPCORE_FILE_NOT_FOUND';
                                    HrDescr := 'Windows Media Player cannot find the file.' +
                                               'The link from the item in the library to its associated digital media file might be broken.' +
                                               'To fix the problem, try repairing the link or removing the item from the library.';
                                  end;
    LongInt($C00D10BB)          : begin
                                    HrStr := 'NS_E_WMPCORE_TEMP_FILE_NOT_FOUND';
                                    HrDescr := 'The temporary file was not found.';
                                  end;
    LongInt($C00D10BC)          : begin
                                    HrStr := 'NS_E_WMDM_REVOKED';
                                    HrDescr := 'Windows Media Player cannot sync the file because the device needs to be updated.';
                                  end;
    LongInt($C00D10BD)          : begin
                                    HrStr := 'NS_E_DDRAW_GENERIC';
                                    HrDescr := 'Windows Media Player cannot play the video because there is a problem with your video card.';
                                  end;
    LongInt($C00D10BE)          : begin
                                    HrStr := 'NS_E_DISPLAY_MODE_CHANGE_FAILED';
                                    HrDescr := 'Windows Media Player failed to change the screen mode for full-screen video playback.';
                                  end;
    LongInt($C00D10BF)          : begin
                                    HrStr := 'NS_E_PLAYLIST_CONTAINS_ERRORS';
                                    HrDescr := 'Windows Media Player cannot play one or more files.' +
                                               'For additional information, right-click an item that cannot be played, and then click Error Details.';
                                  end;
    LongInt($C00D10C0)          : begin
                                    HrStr := 'NS_E_CHANGING_PROXY_NAME';
                                    HrDescr := 'Cannot change the proxy name if the proxy setting is not set to custom.';
                                  end;
    LongInt($C00D10C1)          : begin
                                    HrStr := 'NS_E_CHANGING_PROXY_PORT';
                                    HrDescr := 'Cannot change the proxy port if the proxy setting is not set to custom.';
                                  end;
    LongInt($C00D10C2)          : begin
                                    HrStr := 'NS_E_CHANGING_PROXY_EXCEPTIONLIST';
                                    HrDescr := 'Cannot change the proxy exception list if the proxy setting is not set to custom.';
                                  end;
    LongInt($C00D10C3)          : begin
                                    HrStr := 'NS_E_CHANGING_PROXYBYPASS';
                                    HrDescr := 'Cannot change the proxy bypass flag if the proxy setting is not set to custom.';
                                  end;
    LongInt($C00D10C4)          : begin
                                    HrStr := 'NS_E_CHANGING_PROXY_PROTOCOL_NOT_FOUND';
                                    HrDescr := 'Cannot find the specified protocol.';
                                  end;
    LongInt($C00D10C5)          : begin
                                    HrStr := 'NS_E_GRAPH_NOAUDIOLANGUAGE';
                                    HrDescr := 'Cannot change the language settings.' +
                                               'Either the graph has no audio or the audio only supports one language.';
                                  end;
    LongInt($C00D10C6)          : begin
                                    HrStr := 'NS_E_GRAPH_NOAUDIOLANGUAGESELECTED';
                                    HrDescr := 'The graph has no audio language selected.';
                                  end;
    LongInt($C00D10C7)          : begin
                                    HrStr := 'NS_E_CORECD_NOTAMEDIACD';
                                    HrDescr := 'This is not a media CD.';
                                  end;
    LongInt($C00D10C8)          : begin
                                    HrStr := 'NS_E_WMPCORE_MEDIA_URL_TOO_LONG';
                                    HrDescr := 'Windows Media Player cannot play the file because the URL is too long.';
                                  end;
    LongInt($C00D10C9)          : begin
                                    HrStr := 'NS_E_WMPFLASH_CANT_FIND_COM_SERVER';
                                    HrDescr := 'To play the selected item, you must install the Macromedia Flash Player.' +
                                               'To download the Macromedia Flash Player, go to the Adobe website.';
                                  end;
    LongInt($C00D10CA)          : begin
                                    HrStr := 'NS_E_WMPFLASH_INCOMPATIBLEVERSION';
                                    HrDescr := 'To play the selected item, you must install a later version of the Macromedia Flash Player.' +
                                               'To download the Macromedia Flash Player, go to the Adobe website.';
                                  end;
    LongInt($C00D10CB)          : begin
                                    HrStr := 'NS_E_WMPOCXGRAPH_IE_DISALLOWS_ACTIVEX_CONTROLS';
                                    HrDescr := 'Windows Media Player cannot play the file because your Internet security settings prohibit the use of ActiveX controls.';
                                  end;
    LongInt($C00D10CC)          : begin
                                    HrStr := 'NS_E_NEED_CORE_REFERENCE';
                                    HrDescr := 'The use of this method requires an existing reference to the Player object.';
                                  end;
    LongInt($C00D10CD)          : begin
                                    HrStr := 'NS_E_MEDIACD_READ_ERROR';
                                    HrDescr := 'Windows Media Player cannot play the CD.' +
                                               'The disc might be dirty or damaged.';
                                  end;
    LongInt($C00D10CE)          : begin
                                    HrStr := 'NS_E_IE_DISALLOWS_ACTIVEX_CONTROLS';
                                    HrDescr := 'Windows Media Player cannot play the file because your Internet security settings prohibit the use of ActiveX controls.';
                                  end;
    LongInt($C00D10CF)          : begin
                                    HrStr := 'NS_E_FLASH_PLAYBACK_NOT_ALLOWED';
                                    HrDescr := 'Flash playback has been turned off in Windows Media Player.';
                                  end;
    LongInt($C00D10D0)          : begin
                                    HrStr := 'NS_E_UNABLE_TO_CREATE_RIP_LOCATION';
                                    HrDescr := 'Windows Media Player cannot rip the CD because a valid rip location cannot be created.';
                                  end;
    LongInt($C00D10D1)          : begin
                                    HrStr := 'NS_E_WMPCORE_SOME_CODECS_MISSING';
                                    HrDescr := 'Windows Media Player cannot play the file because a required codec is not installed on your computer.';
                                  end;
    LongInt($C00D10D2)          : begin
                                    HrStr := 'NS_E_WMP_RIP_FAILED';
                                    HrDescr := 'Windows Media Player cannot rip one or more tracks from the CD.';
                                  end;
    LongInt($C00D10D3)          : begin
                                    HrStr := 'NS_E_WMP_FAILED_TO_RIP_TRACK';
                                    HrDescr := 'Windows Media Player encountered a problem while ripping the track from the CD.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D10D4)          : begin
                                    HrStr := 'NS_E_WMP_ERASE_FAILED';
                                    HrDescr := 'Windows Media Player encountered a problem while erasing the disc.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D10D5)          : begin
                                    HrStr := 'NS_E_WMP_FORMAT_FAILED';
                                    HrDescr := 'Windows Media Player encountered a problem while formatting the device.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D10D6)          : begin
                                    HrStr := 'NS_E_WMP_CANNOT_BURN_NON_LOCAL_FILE';
                                    HrDescr := 'This file cannot be burned to a CD because it is not located on your computer.';
                                  end;
    LongInt($C00D10D7)          : begin
                                    HrStr := 'NS_E_WMP_FILE_TYPE_CANNOT_BURN_TO_AUDIO_CD';
                                    HrDescr := 'It is not possible to burn this file type to an audio CD.' +
                                               'Windows Media Player can burn the following file types to an audio CD: WMA, MP3, or WAV.';
                                  end;
    LongInt($C00D10D8)          : begin
                                    HrStr := 'NS_E_WMP_FILE_DOES_NOT_FIT_ON_CD';
                                    HrDescr := 'This file is too large to fit on a disc.';
                                  end;
    LongInt($C00D10D9)          : begin
                                    HrStr := 'NS_E_WMP_FILE_NO_DURATION';
                                    HrDescr := 'It is not possible to determine if this file can fit on a disc because Windows Media Player cannot detect the length of the file.' +
                                               'Playing the file before burning might enable the Player to detect the file length.';
                                  end;
    LongInt($C00D10DA)          : begin
                                    HrStr := 'NS_E_PDA_FAILED_TO_BURN';
                                    HrDescr := 'Windows Media Player encountered a problem while burning the file to the disc.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D10DC)          : begin
                                    HrStr := 'NS_E_FAILED_DOWNLOAD_ABORT_BURN';
                                    HrDescr := 'Windows Media Player cannot burn the audio CD because some items in the list that you chose to buy could not be downloaded from the online store.';
                                  end;
    LongInt($C00D10DD)          : begin
                                    HrStr := 'NS_E_WMPCORE_DEVICE_DRIVERS_MISSING';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'Try using Windows Update or Device Manager to update the device drivers for your audio and video cards.' +
                                               'For information about using Windows Update or Device Manager, see Windows Help.';
                                  end;
    LongInt($C00D1126)          : begin
                                    HrStr := 'NS_E_WMPIM_USEROFFLINE';
                                    HrDescr := 'Windows Media Player has detected that you are not connected to the Internet.' +
                                               'Connect to the Internet, and then try again.';
                                  end;
    LongInt($C00D1127)          : begin
                                    HrStr := 'NS_E_WMPIM_USERCANCELED';
                                    HrDescr := 'The attempt to connect to the Internet was canceled.';
                                  end;
    LongInt($C00D1128)          : begin
                                    HrStr := 'NS_E_WMPIM_DIALUPFAILED';
                                    HrDescr := 'The attempt to connect to the Internet failed.';
                                  end;
    LongInt($C00D1129)          : begin
                                    HrStr := 'NS_E_WINSOCK_ERROR_STRING';
                                    HrDescr := 'Windows Media Player has encountered an unknown network error.';
                                  end;
    LongInt($C00D1130)          : begin
                                    HrStr := 'NS_E_WMPBR_NOLISTENER';
                                    HrDescr := 'No window is currently listening to Backup and Restore events.';
                                  end;
    LongInt($C00D1131)          : begin
                                    HrStr := 'NS_E_WMPBR_BACKUPCANCEL';
                                    HrDescr := 'Your media usage rights were not backed up because the backup was canceled.';
                                  end;
    LongInt($C00D1132)          : begin
                                    HrStr := 'NS_E_WMPBR_RESTORECANCEL';
                                    HrDescr := 'Your media usage rights were not restored because the restoration was canceled.';
                                  end;
    LongInt($C00D1133)          : begin
                                    HrStr := 'NS_E_WMPBR_ERRORWITHURL';
                                    HrDescr := 'An error occurred while backing up or restoring your media usage rights.' +
                                               'A required web page cannot be displayed.';
                                  end;
    LongInt($C00D1134)          : begin
                                    HrStr := 'NS_E_WMPBR_NAMECOLLISION';
                                    HrDescr := 'Your media usage rights were not backed up because the backup was canceled.';
                                  end;
    LongInt($C00D1137)          : begin
                                    HrStr := 'NS_E_WMPBR_DRIVE_INVALID';
                                    HrDescr := 'Windows Media Player cannot restore your media usage rights from the specified location.' +
                                               'Choose another location, and then try again.';
                                  end;
    LongInt($C00D1138)          : begin
                                    HrStr := 'NS_E_WMPBR_BACKUPRESTOREFAILED';
                                    HrDescr := 'Windows Media Player cannot backup or restore your media usage rights.';
                                  end;
    LongInt($C00D1158)          : begin
                                    HrStr := 'NS_E_WMP_CONVERT_FILE_FAILED';
                                    HrDescr := 'Windows Media Player cannot add the file to the library.';
                                  end;
    LongInt($C00D1159)          : begin
                                    HrStr := 'NS_E_WMP_CONVERT_NO_RIGHTS_ERRORURL';
                                    HrDescr := 'Windows Media Player cannot add the file to the library because the content provider prohibits it.' +
                                               'For assistance, contact the company that provided the file.';
                                  end;
    LongInt($C00D115A)          : begin
                                    HrStr := 'NS_E_WMP_CONVERT_NO_RIGHTS_NOERRORURL';
                                    HrDescr := 'Windows Media Player cannot add the file to the library because the content provider prohibits it.' +
                                               'For assistance, contact the company that provided the file.';
                                  end;
    LongInt($C00D115B)          : begin
                                    HrStr := 'NS_E_WMP_CONVERT_FILE_CORRUPT';
                                    HrDescr := 'Windows Media Player cannot add the file to the library.' +
                                               'The file might not be valid.';
                                  end;
    LongInt($C00D115C)          : begin
                                    HrStr := 'NS_E_WMP_CONVERT_PLUGIN_UNAVAILABLE_ERRORURL';
                                    HrDescr := 'Windows Media Player cannot add the file to the library.' +
                                               'The plug-in required to add the file is not installed properly.' +
                                               'For assistance, click Web Help to display the website of the company that provided the file.';
                                  end;
    LongInt($C00D115D)          : begin
                                    HrStr := 'NS_E_WMP_CONVERT_PLUGIN_UNAVAILABLE_NOERRORURL';
                                    HrDescr := 'Windows Media Player cannot add the file to the library.' +
                                               'The plug-in required to add the file is not installed properly.' +
                                               'For assistance, contact the company that provided the file.';
                                  end;
    LongInt($C00D115E)          : begin
                                    HrStr := 'NS_E_WMP_CONVERT_PLUGIN_UNKNOWN_FILE_OWNER';
                                    HrDescr := 'Windows Media Player cannot add the file to the library.' +
                                               'The plug-in required to add the file is not installed properly.' +
                                               'For assistance, contact the company that provided the file.';
                                  end;
    LongInt($C00D1160)          : begin
                                    HrStr := 'NS_E_DVD_DISC_COPY_PROTECT_OUTPUT_NS';
                                    HrDescr := 'Windows Media Player cannot play this DVD.' +
                                               'Try installing an updated driver for your video card or obtaining a newer video card.';
                                  end;
    LongInt($C00D1161)          : begin
                                    HrStr := 'NS_E_DVD_DISC_COPY_PROTECT_OUTPUT_FAILED';
                                    HrDescr := 'This DVD''s resolution exceeds the maximum allowed by your component video outputs.' +
                                               'Try reducing your screen resolution to 640 x 480, or turn off analog component outputs and use a VGA connection to your monitor.';
                                  end;
    LongInt($C00D1162)          : begin
                                    HrStr := 'NS_E_DVD_NO_SUBPICTURE_STREAM';
                                    HrDescr := 'Windows Media Player cannot display subtitles or highlights in DVD menus.' +
                                               'Reinstall the DVD decoder or contact the DVD drive manufacturer to obtain an updated decoder.';
                                  end;
    LongInt($C00D1163)          : begin
                                    HrStr := 'NS_E_DVD_COPY_PROTECT';
                                    HrDescr := 'Windows Media Player cannot play this DVD because there is a problem with digital copy protection between your DVD drive, decoder, and video card.' +
                                               'Try installing an updated driver for your video card.';
                                  end;
    LongInt($C00D1164)          : begin
                                    HrStr := 'NS_E_DVD_AUTHORING_PROBLEM';
                                    HrDescr := 'Windows Media Player cannot play the DVD.' +
                                               'The disc was created in a manner that the Player does not support.';
                                  end;
    LongInt($C00D1165)          : begin
                                    HrStr := 'NS_E_DVD_INVALID_DISC_REGION';
                                    HrDescr := 'Windows Media Player cannot play the DVD because the disc prohibits playback in your region of the world.' +
                                               'You must obtain a disc that is intended for your geographic region.';
                                  end;
    LongInt($C00D1166)          : begin
                                    HrStr := 'NS_E_DVD_COMPATIBLE_VIDEO_CARD';
                                    HrDescr := 'Windows Media Player cannot play the DVD because your video card does not support DVD playback.';
                                  end;
    LongInt($C00D1167)          : begin
                                    HrStr := 'NS_E_DVD_MACROVISION';
                                    HrDescr := 'Windows Media Player cannot play this DVD because it is not possible to turn on analog copy protection on the output display.' +
                                               'Try installing an updated driver for your video card.';
                                  end;
    LongInt($C00D1168)          : begin
                                    HrStr := 'NS_E_DVD_SYSTEM_DECODER_REGION';
                                    HrDescr := 'Windows Media Player cannot play the DVD because the region assigned to your DVD drive does not match the region assigned to your DVD decoder.';
                                  end;
    LongInt($C00D1169)          : begin
                                    HrStr := 'NS_E_DVD_DISC_DECODER_REGION';
                                    HrDescr := 'Windows Media Player cannot play the DVD because the disc prohibits playback in your region of the world.' +
                                               'You must obtain a disc that is intended for your geographic region.';
                                  end;
    LongInt($C00D116A)          : begin
                                    HrStr := 'NS_E_DVD_NO_VIDEO_STREAM';
                                    HrDescr := 'Windows Media Player cannot play DVD video.' +
                                               'You might need to adjust your Windows display settings.' +
                                               'Open display settings in Control Panel, and then try lowering your screen resolution and color quality settings.';
                                  end;
    LongInt($C00D116B)          : begin
                                    HrStr := 'NS_E_DVD_NO_AUDIO_STREAM';
                                    HrDescr := 'Windows Media Player cannot play DVD audio.' +
                                               'Verify that your sound card is set up correctly, and then try again.';
                                  end;
    LongInt($C00D116C)          : begin
                                    HrStr := 'NS_E_DVD_GRAPH_BUILDING';
                                    HrDescr := 'Windows Media Player cannot play DVD video.' +
                                               'Close any open files and quit any other programs, and then try again.' +
                                               'If the problem persists, restart your computer.';
                                  end;
    LongInt($C00D116D)          : begin
                                    HrStr := 'NS_E_DVD_NO_DECODER';
                                    HrDescr := 'Windows Media Player cannot play the DVD because a compatible DVD decoder is not installed on your computer.';
                                  end;
    LongInt($C00D116E)          : begin
                                    HrStr := 'NS_E_DVD_PARENTAL';
                                    HrDescr := 'Windows Media Player cannot play the scene because it has a parental rating higher than the rating that you are authorized to view.';
                                  end;
    LongInt($C00D116F)          : begin
                                    HrStr := 'NS_E_DVD_CANNOT_JUMP';
                                    HrDescr := 'Windows Media Player cannot skip to the requested location on the DVD.';
                                  end;
    LongInt($C00D1170)          : begin
                                    HrStr := 'NS_E_DVD_DEVICE_CONTENTION';
                                    HrDescr := 'Windows Media Player cannot play the DVD because it is currently in use by another program.' +
                                               'Quit the other program that is using the DVD, and then try again.';
                                  end;
    LongInt($C00D1171)          : begin
                                    HrStr := 'NS_E_DVD_NO_VIDEO_MEMORY';
                                    HrDescr := 'Windows Media Player cannot play DVD video.' +
                                               'You might need to adjust your Windows display settings.' +
                                               'Open display settings in Control Panel, and then try lowering your screen resolution and color quality settings.';
                                  end;
    LongInt($C00D1172)          : begin
                                    HrStr := 'NS_E_DVD_CANNOT_COPY_PROTECTED';
                                    HrDescr := 'Windows Media Player cannot rip the DVD because it is copy protected.';
                                  end;
    LongInt($C00D1173)          : begin
                                    HrStr := 'NS_E_DVD_REQUIRED_PROPERTY_NOT_SET';
                                    HrDescr := 'One of more of the required properties has not been set.';
                                  end;
    LongInt($C00D1174)          : begin
                                    HrStr := 'NS_E_DVD_INVALID_TITLE_CHAPTER';
                                    HrDescr := 'The specified title and/or chapter number does not exist on this DVD.';
                                  end;
    LongInt($C00D1176)          : begin
                                    HrStr := 'NS_E_NO_CD_BURNER';
                                    HrDescr := 'Windows Media Player cannot burn the files because the Player cannot find a burner.' +
                                               'If the burner is connected properly, try using Windows Update to install the latest device driver.';
                                  end;
    LongInt($C00D1177)          : begin
                                    HrStr := 'NS_E_DEVICE_IS_NOT_READY';
                                    HrDescr := 'Windows Media Player does not detect storage media in the selected device.' +
                                               'Insert storage media into the device, and then try again.';
                                  end;
    LongInt($C00D1178)          : begin
                                    HrStr := 'NS_E_PDA_UNSUPPORTED_FORMAT';
                                    HrDescr := 'Windows Media Player cannot sync this file.' +
                                               'The Player might not support the file type.';
                                  end;
    LongInt($C00D1179)          : begin
                                    HrStr := 'NS_E_NO_PDA';
                                    HrDescr := 'Windows Media Player does not detect a portable device.' +
                                               'Connect your portable device, and then try again.';
                                  end;
    LongInt($C00D117A)          : begin
                                    HrStr := 'NS_E_PDA_UNSPECIFIED_ERROR';
                                    HrDescr := 'Windows Media Player encountered an error while communicating with the device.' +
                                               'The storage card on the device might be full, the device might be turned off, or the device might not allow playlists or folders to be created on it.';
                                  end;
    LongInt($C00D117B)          : begin
                                    HrStr := 'NS_E_MEMSTORAGE_BAD_DATA';
                                    HrDescr := 'Windows Media Player encountered an error while burning a CD.';
                                  end;
    LongInt($C00D117C)          : begin
                                    HrStr := 'NS_E_PDA_FAIL_SELECT_DEVICE';
                                    HrDescr := 'Windows Media Player encountered an error while communicating with a portable device or CD drive.';
                                  end;
    LongInt($C00D117D)          : begin
                                    HrStr := 'NS_E_PDA_FAIL_READ_WAVE_FILE';
                                    HrDescr := 'Windows Media Player cannot open the WAV file.';
                                  end;
    LongInt($C00D117E)          : begin
                                    HrStr := 'NS_E_IMAPI_LOSSOFSTREAMING';
                                    HrDescr := 'Windows Media Player failed to burn all the files to the CD.' +
                                               'Select a slower recording speed, and then try again.';
                                  end;
    LongInt($C00D117F)          : begin
                                    HrStr := 'NS_E_PDA_DEVICE_FULL';
                                    HrDescr := 'There is not enough storage space on the portable device to complete this operation.' +
                                               'Delete some unneeded files on the portable device, and then try again.';
                                  end;
    LongInt($C00D1180)          : begin
                                    HrStr := 'NS_E_FAIL_LAUNCH_ROXIO_PLUGIN';
                                    HrDescr := 'Windows Media Player cannot burn the files.' +
                                               'Verify that your burner is connected properly, and then try again.' +
                                               'If the problem persists, reinstall the Player.';
                                  end;
    LongInt($C00D1181)          : begin
                                    HrStr := 'NS_E_PDA_DEVICE_FULL_IN_SESSION';
                                    HrDescr := 'Windows Media Player did not sync some files to the device because there is not enough storage space on the device.';
                                  end;
    LongInt($C00D1182)          : begin
                                    HrStr := 'NS_E_IMAPI_MEDIUM_INVALIDTYPE';
                                    HrDescr := 'The disc in the burner is not valid.' +
                                               'Insert a blank disc into the burner, and then try again.';
                                  end;
    LongInt($C00D1183)          : begin
                                    HrStr := 'NS_E_PDA_MANUALDEVICE';
                                    HrDescr := 'Windows Media Player cannot perform the requested action because the device does not support sync.';
                                  end;
    LongInt($C00D1184)          : begin
                                    HrStr := 'NS_E_PDA_PARTNERSHIPNOTEXIST';
                                    HrDescr := 'To perform the requested action, you must first set up sync with the device.';
                                  end;
    LongInt($C00D1185)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_CREATE_ADDITIONAL_SYNC_RELATIONSHIP';
                                    HrDescr := 'You have already created sync partnerships with 16 devices.' +
                                               'To create a new sync partnership, you must first end an existing partnership.';
                                  end;
    LongInt($C00D1186)          : begin
                                    HrStr := 'NS_E_PDA_NO_TRANSCODE_OF_DRM';
                                    HrDescr := 'Windows Media Player cannot sync the file because protected files cannot be converted to the required quality level or file format.';
                                  end;
    LongInt($C00D1187)          : begin
                                    HrStr := 'NS_E_PDA_TRANSCODECACHEFULL';
                                    HrDescr := 'The folder that stores converted files is full.' +
                                               'Either empty the folder or increase its size, and then try again.';
                                  end;
    LongInt($C00D1188)          : begin
                                    HrStr := 'NS_E_PDA_TOO_MANY_FILE_COLLISIONS';
                                    HrDescr := 'There are too many files with the same name in the folder on the device.' +
                                               'Change the file name or sync to a different folder.';
                                  end;
    LongInt($C00D1189)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_TRANSCODE';
                                    HrDescr := 'Windows Media Player cannot convert the file to the format required by the device.';
                                  end;
    LongInt($C00D118A)          : begin
                                    HrStr := 'NS_E_PDA_TOO_MANY_FILES_IN_DIRECTORY';
                                    HrDescr := 'You have reached the maximum number of files your device allows in a folder.' +
                                               'If your device supports playback from subfolders, try creating subfolders on the device and storing some files in them.';
                                  end;
    LongInt($C00D118B)          : begin
                                    HrStr := 'NS_E_PROCESSINGSHOWSYNCWIZARD';
                                    HrDescr := 'Windows Media Player is already trying to start the Device Setup Wizard.';
                                  end;
    LongInt($C00D118C)          : begin
                                    HrStr := 'NS_E_PDA_TRANSCODE_NOT_PERMITTED';
                                    HrDescr := 'Windows Media Player cannot convert this file format.' +
                                               'If an updated version of the codec used to compress this file is available, install it and then try to sync the file again.';
                                  end;
    LongInt($C00D118D)          : begin
                                    HrStr := 'NS_E_PDA_INITIALIZINGDEVICES';
                                    HrDescr := 'Windows Media Player is busy setting up devices.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D118E)          : begin
                                    HrStr := 'NS_E_PDA_OBSOLETE_SP';
                                    HrDescr := 'Your device is using an outdated driver that is no longer supported by Windows Media Player.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D118F)          : begin
                                    HrStr := 'NS_E_PDA_TITLE_COLLISION';
                                    HrDescr := 'Windows Media Player cannot sync the file because a file with the same name already exists on the device.' +
                                               'Change the file name or try to sync the file to a different folder.';
                                  end;
    LongInt($C00D1190)          : begin
                                    HrStr := 'NS_E_PDA_DEVICESUPPORTDISABLED';
                                    HrDescr := 'Automatic and manual sync have been turned off temporarily.' +
                                               'To sync to a device, restart Windows Media Player.';
                                  end;
    LongInt($C00D1191)          : begin
                                    HrStr := 'NS_E_PDA_NO_LONGER_AVAILABLE';
                                    HrDescr := 'This device is not available.' +
                                               'Connect the device to the computer, and then try again.';
                                  end;
    LongInt($C00D1192)          : begin
                                    HrStr := 'NS_E_PDA_ENCODER_NOT_RESPONDING';
                                    HrDescr := 'Windows Media Player cannot sync the file because an error occurred while converting the file to another quality level or format.' +
                                               'If the problem persists, remove the file from the list of files to sync.';
                                  end;
    LongInt($C00D1193)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_SYNC_FROM_LOCATION';
                                    HrDescr := 'Windows Media Player cannot sync the file to your device.' +
                                               'The file might be stored in a location that is not supported.' +
                                               'Copy the file from its current location to your hard disk, add it to your library, and then try to sync the file again.';
                                  end;
    LongInt($C00D1194)          : begin
                                    HrStr := 'NS_E_WMP_PROTOCOL_PROBLEM';
                                    HrDescr := 'Windows Media Player cannot open the specified URL.' +
                                               'Verify that the Player is configured to use all available protocols, and then try again.';
                                  end;
    LongInt($C00D1195)          : begin
                                    HrStr := 'NS_E_WMP_NO_DISK_SPACE';
                                    HrDescr := 'Windows Media Player cannot perform the requested action because there is not enough storage space on your computer.' +
                                               'Delete some unneeded files on your hard disk, and then try again.';
                                  end;
    LongInt($C00D1196)          : begin
                                    HrStr := 'NS_E_WMP_LOGON_FAILURE';
                                    HrDescr := 'The server denied access to the file.' +
                                               'Verify that you are using the correct user name and password.';
                                  end;
    LongInt($C00D1197)          : begin
                                    HrStr := 'NS_E_WMP_CANNOT_FIND_FILE';
                                    HrDescr := 'Windows Media Player cannot find the file.' +
                                               'If you are trying to play, burn, or sync an item that is in your library, the item might point to a file that has been moved, renamed, or deleted.';
                                  end;
    LongInt($C00D1198)          : begin
                                    HrStr := 'NS_E_WMP_SERVER_INACCESSIBLE';
                                    HrDescr := 'Windows Media Player cannot connect to the server.' +
                                               'The server name might not be correct, the server might not be available, or your proxy settings might not be correct.';
                                  end;
    LongInt($C00D1199)          : begin
                                    HrStr := 'NS_E_WMP_UNSUPPORTED_FORMAT';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'The Player might not support the file type or might not support the codec that was used to compress the file.';
                                  end;
    LongInt($C00D119A)          : begin
                                    HrStr := 'NS_E_WMP_DSHOW_UNSUPPORTED_FORMAT';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'The Player might not support the file type or a required codec might not be installed on your computer.';
                                  end;
    LongInt($C00D119B)          : begin
                                    HrStr := 'NS_E_WMP_PLAYLIST_EXISTS';
                                    HrDescr := 'Windows Media Player cannot create the playlist because the name already exists.' +
                                               'Type a different playlist name.';
                                  end;
    LongInt($C00D119C)          : begin
                                    HrStr := 'NS_E_WMP_NONMEDIA_FILES';
                                    HrDescr := 'Windows Media Player cannot delete the playlist because it contains items that are not digital media files.' +
                                               'Any digital media files in the playlist were deleted.';
                                  end;
    LongInt($C00D119D)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_ASX';
                                    HrDescr := 'The playlist cannot be opened because it is stored in a shared folder on another computer.' +
                                               'If possible, move the playlist to the playlists folder on your computer.';
                                  end;
    LongInt($C00D119E)          : begin
                                    HrStr := 'NS_E_WMP_ALREADY_IN_USE';
                                    HrDescr := 'Windows Media Player is already in use.' +
                                               'Stop playing any items, close all Player dialog boxes, and then try again.';
                                  end;
    LongInt($C00D119F)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI_FAILURE';
                                    HrDescr := 'Windows Media Player encountered an error while burning.' +
                                               'Verify that the burner is connected properly and that the disc is clean and not damaged.';
                                  end;
    LongInt($C00D11A0)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_FAILURE';
                                    HrDescr := 'Windows Media Player has encountered an unknown error with your portable device.' +
                                               'Reconnect your portable device, and then try again.';
                                  end;
    LongInt($C00D11A1)          : begin
                                    HrStr := 'NS_E_WMP_CODEC_NEEDED_WITH_4CC';
                                    HrDescr := 'A codec is required to play this file.' +
                                               'To determine if this codec is available to download from the web, click Web Help.';
                                  end;
    LongInt($C00D11A2)          : begin
                                    HrStr := 'NS_E_WMP_CODEC_NEEDED_WITH_FORMATTAG';
                                    HrDescr := 'An audio codec is needed to play this file.' +
                                               'To determine if this codec is available to download from the web, click Web Help.';
                                  end;
    LongInt($C00D11A3)          : begin
                                    HrStr := 'NS_E_WMP_MSSAP_NOT_AVAILABLE';
                                    HrDescr := 'To play the file, you must install the latest Windows service pack.' +
                                               'To install the service pack from the Windows Update website, click Web Help.';
                                  end;
    LongInt($C00D11A4)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_INTERFACEDEAD';
                                    HrDescr := 'Windows Media Player no longer detects a portable device.' +
                                               'Reconnect your portable device, and then try again.';
                                  end;
    LongInt($C00D11A5)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_NOTCERTIFIED';
                                    HrDescr := 'Windows Media Player cannot sync the file because the portable device does not support protected files.';
                                  end;
    LongInt($C00D11A6)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_LICENSE_NOTEXIST';
                                    HrDescr := 'This file does not have sync rights.' +
                                               'If you obtained this file from an online store, go to the online store to get sync rights.';
                                  end;
    LongInt($C00D11A7)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_LICENSE_EXPIRED';
                                    HrDescr := 'Windows Media Player cannot sync the file because the sync rights have expired.' +
                                               'Go to the content provider''s online store to get new sync rights.';
                                  end;
    LongInt($C00D11A8)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_BUSY';
                                    HrDescr := 'The portable device is already in use.' +
                                               'Wait until the current task finishes or quit other programs that might be using the portable device, and then try again.';
                                  end;
    LongInt($C00D11A9)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_NORIGHTS';
                                    HrDescr := 'Windows Media Player cannot sync the file because the content provider or device prohibits it.' +
                                               'You might be able to resolve this problem by going to the content provider''s online store to get sync rights.';
                                  end;
    LongInt($C00D11AA)          : begin
                                    HrStr := 'NS_E_WMP_WMDM_INCORRECT_RIGHTS';
                                    HrDescr := 'The content provider has not granted you the right to sync this file.' +
                                               'Go to the content provider''s online store to get sync rights.';
                                  end;
    LongInt($C00D11AB)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI_GENERIC';
                                    HrDescr := 'Windows Media Player cannot burn the files to the CD.' +
                                               'Verify that the disc is clean and not damaged.' +
                                               'If necessary, select a slower recording speed or try a different brand of blank discs.';
                                  end;
    LongInt($C00D11AD)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI_DEVICE_NOTPRESENT';
                                    HrDescr := 'Windows Media Player cannot burn the files.' +
                                               'Verify that the burner is connected properly, and then try again.';
                                  end;
    LongInt($C00D11AE)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI_DEVICE_BUSY';
                                    HrDescr := 'Windows Media Player cannot burn the files.' +
                                               'Verify that the burner is connected properly and that the disc is clean and not damaged.' +
                                               'If the burner is already in use, wait until the current task finishes or quit other programs that might be using the burner.';
                                  end;
    LongInt($C00D11AF)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI_LOSS_OF_STREAMING';
                                    HrDescr := 'Windows Media Player cannot burn the files to the CD.';
                                  end;
    LongInt($C00D11B0)          : begin
                                    HrStr := 'NS_E_WMP_SERVER_UNAVAILABLE';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'The server might not be available or there might be a problem with your network or firewall settings.';
                                  end;
    LongInt($C00D11B1)          : begin
                                    HrStr := 'NS_E_WMP_FILE_OPEN_FAILED';
                                    HrDescr := 'Windows Media Player encountered a problem while playing the file.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D11B2)          : begin
                                    HrStr := 'NS_E_WMP_VERIFY_ONLINE';
                                    HrDescr := 'Windows Media Player must connect to the Internet to verify the file''s media usage rights.' +
                                               'Connect to the Internet, and then try again.';
                                  end;
    LongInt($C00D11B3)          : begin
                                    HrStr := 'NS_E_WMP_SERVER_NOT_RESPONDING';
                                    HrDescr := 'Windows Media Player cannot play the file because a network error occurred.' +
                                               'The server might not be available.' +
                                               'Verify that you are connected to the network and that your proxy settings are correct.';
                                  end;
    LongInt($C00D11B4)          : begin
                                    HrStr := 'NS_E_WMP_DRM_CORRUPT_BACKUP';
                                    HrDescr := 'Windows Media Player cannot restore your media usage rights because it could not find any backed up rights on your computer.';
                                  end;
    LongInt($C00D11B5)          : begin
                                    HrStr := 'NS_E_WMP_DRM_LICENSE_SERVER_UNAVAILABLE';
                                    HrDescr := 'Windows Media Player cannot download media usage rights because the server is not available (for example, the server might be busy or not online).';
                                  end;
    LongInt($C00D11B6)          : begin
                                    HrStr := 'NS_E_WMP_NETWORK_FIREWALL';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'A network firewall might be preventing the Player from opening the file by using the UDP transport protocol.' +
                                               'If you typed a URL in the Open URL dialog box, try using a different transport protocol (for example, "http:").';
                                  end;
    LongInt($C00D11B7)          : begin
                                    HrStr := 'NS_E_WMP_NO_REMOVABLE_MEDIA';
                                    HrDescr := 'Insert the removable media, and then try again.';
                                  end;
    LongInt($C00D11B8)          : begin
                                    HrStr := 'NS_E_WMP_PROXY_CONNECT_TIMEOUT';
                                    HrDescr := 'Windows Media Player cannot play the file because the proxy server is not responding.' +
                                               'The proxy server might be temporarily unavailable or your Player proxy settings might not be valid.';
                                  end;
    LongInt($C00D11B9)          : begin
                                    HrStr := 'NS_E_WMP_NEED_UPGRADE';
                                    HrDescr := 'To play the file, you might need to install a later version of Windows Media Player.' +
                                               'On the Help menu, click Check for Updates, and then follow the instructions.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D11BA)          : begin
                                    HrStr := 'NS_E_WMP_AUDIO_HW_PROBLEM';
                                    HrDescr := 'Windows Media Player cannot play the file because there is a problem with your sound device.' +
                                               'There might not be a sound device installed on your computer, it might be in use by another program, or it might not be functioning properly.';
                                  end;
    LongInt($C00D11BB)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_PROTOCOL';
                                    HrDescr := 'Windows Media Player cannot play the file because the specified protocol is not supported.' +
                                               'If you typed a URL in the Open URL dialog box, try using a different transport protocol (for example, "http:" or "rtsp:").';
                                  end;
    LongInt($C00D11BC)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_LIBRARY_ADD';
                                    HrDescr := 'Windows Media Player cannot add the file to the library because the file format is not supported.';
                                  end;
    LongInt($C00D11BD)          : begin
                                    HrStr := 'NS_E_WMP_MMS_NOT_SUPPORTED';
                                    HrDescr := 'Windows Media Player cannot play the file because the specified protocol is not supported.' +
                                               'If you typed a URL in the Open URL dialog box, try using a different transport protocol (for example, "mms:").';
                                  end;
    LongInt($C00D11BE)          : begin
                                    HrStr := 'NS_E_WMP_NO_PROTOCOLS_SELECTED';
                                    HrDescr := 'Windows Media Player cannot play the file because there are no streaming protocols selected.' +
                                               'Select one or more protocols, and then try again.';
                                  end;
    LongInt($C00D11BF)          : begin
                                    HrStr := 'NS_E_WMP_GOFULLSCREEN_FAILED';
                                    HrDescr := 'Windows Media Player cannot switch to Full Screen.' +
                                               'You might need to adjust your Windows display settings.' +
                                               'Open display settings in Control Panel, and then try setting Hardware acceleration to Full.';
                                  end;
    LongInt($C00D11C0)          : begin
                                    HrStr := 'NS_E_WMP_NETWORK_ERROR';
                                    HrDescr := 'Windows Media Player cannot play the file because a network error occurred.' +
                                               'The server might not be available (for example, the server is busy or not online) or you might not be connected to the network.';
                                  end;
    LongInt($C00D11C1)          : begin
                                    HrStr := 'NS_E_WMP_CONNECT_TIMEOUT';
                                    HrDescr := 'Windows Media Player cannot play the file because the server is not responding.' +
                                               'Verify that you are connected to the network, and then try again later.';
                                  end;
    LongInt($C00D11C2)          : begin
                                    HrStr := 'NS_E_WMP_MULTICAST_DISABLED';
                                    HrDescr := 'Windows Media Player cannot play the file because the multicast protocol is not enabled.' +
                                               'On the Tools menu, click Options, click the Network tab, and then select the Multicast check box.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D11C3)          : begin
                                    HrStr := 'NS_E_WMP_SERVER_DNS_TIMEOUT';
                                    HrDescr := 'Windows Media Player cannot play the file because a network problem occurred.' +
                                               'Verify that you are connected to the network, and then try again later.';
                                  end;
    LongInt($C00D11C4)          : begin
                                    HrStr := 'NS_E_WMP_PROXY_NOT_FOUND';
                                    HrDescr := 'Windows Media Player cannot play the file because the network proxy server cannot be found.' +
                                               'Verify that your proxy settings are correct, and then try again.';
                                  end;
    LongInt($C00D11C5)          : begin
                                    HrStr := 'NS_E_WMP_TAMPERED_CONTENT';
                                    HrDescr := 'Windows Media Player cannot play the file because it is corrupted.';
                                  end;
    LongInt($C00D11C6)          : begin
                                    HrStr := 'NS_E_WMP_OUTOFMEMORY';
                                    HrDescr := 'Your computer is running low on memory.' +
                                               'Quit other programs, and then try again.';
                                  end;
    LongInt($C00D11C7)          : begin
                                    HrStr := 'NS_E_WMP_AUDIO_CODEC_NOT_INSTALLED';
                                    HrDescr := 'Windows Media Player cannot play, burn, rip, or sync the file because a required audio codec is not installed on your computer.';
                                  end;
    LongInt($C00D11C8)          : begin
                                    HrStr := 'NS_E_WMP_VIDEO_CODEC_NOT_INSTALLED';
                                    HrDescr := 'Windows Media Player cannot play the file because the required video codec is not installed on your computer.';
                                  end;
    LongInt($C00D11C9)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI_DEVICE_INVALIDTYPE';
                                    HrDescr := 'Windows Media Player cannot burn the files.' +
                                               'If the burner is busy, wait for the current task to finish.' +
                                               'If necessary, verify that the burner is connected properly and that you have installed the latest device driver.';
                                  end;
    LongInt($C00D11CA)          : begin
                                    HrStr := 'NS_E_WMP_DRM_DRIVER_AUTH_FAILURE';
                                    HrDescr := 'Windows Media Player cannot play the protected file because there is a problem with your sound device.' +
                                               'Try installing a new device driver or use a different sound device.';
                                  end;
    LongInt($C00D11CB)          : begin
                                    HrStr := 'NS_E_WMP_NETWORK_RESOURCE_FAILURE';
                                    HrDescr := 'Windows Media Player encountered a network error.' +
                                               'Restart the Player.';
                                  end;
    LongInt($C00D11CC)          : begin
                                    HrStr := 'NS_E_WMP_UPGRADE_APPLICATION';
                                    HrDescr := 'Windows Media Player is not installed properly.' +
                                               'Reinstall the Player.';
                                  end;
    LongInt($C00D11CD)          : begin
                                    HrStr := 'NS_E_WMP_UNKNOWN_ERROR';
                                    HrDescr := 'Windows Media Player encountered an unknown error.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D11CE)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_KEY';
                                    HrDescr := 'Windows Media Player cannot play the file because the required codec is not valid.';
                                  end;
    LongInt($C00D11CF)          : begin
                                    HrStr := 'NS_E_WMP_CD_ANOTHER_USER';
                                    HrDescr := 'The CD drive is in use by another user.' +
                                               'Wait for the task to complete, and then try again.';
                                  end;
    LongInt($C00D11D0)          : begin
                                    HrStr := 'NS_E_WMP_DRM_NEEDS_AUTHORIZATION';
                                    HrDescr := 'Windows Media Player cannot play, sync, or burn the protected file because a problem occurred with the Windows Media Digital Rights Management (DRM) system.' +
                                               'You might need to connect to the Internet to update your DRM components.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D11D1)          : begin
                                    HrStr := 'NS_E_WMP_BAD_DRIVER';
                                    HrDescr := 'Windows Media Player cannot play the file because there might be a problem with your sound or video device.' +
                                               'Try installing an updated device driver.';
                                  end;
    LongInt($C00D11D2)          : begin
                                    HrStr := 'NS_E_WMP_ACCESS_DENIED';
                                    HrDescr := 'Windows Media Player cannot access the file.' +
                                               'The file might be in use, you might not have access to the computer where the file is stored, or your proxy settings might not be correct.';
                                  end;
    LongInt($C00D11D3)          : begin
                                    HrStr := 'NS_E_WMP_LICENSE_RESTRICTS';
                                    HrDescr := 'The content provider prohibits this action.' +
                                               'Go to the content provider''s online store to get new media usage rights.';
                                  end;
    LongInt($C00D11D4)          : begin
                                    HrStr := 'NS_E_WMP_INVALID_REQUEST';
                                    HrDescr := 'Windows Media Player cannot perform the requested action at this time.';
                                  end;
    LongInt($C00D11D5)          : begin
                                    HrStr := 'NS_E_WMP_CD_STASH_NO_SPACE';
                                    HrDescr := 'Windows Media Player cannot burn the files because there is not enough free disk space to store the temporary files.' +
                                               'Delete some unneeded files on your hard disk, and then try again.';
                                  end;
    LongInt($C00D11D6)          : begin
                                    HrStr := 'NS_E_WMP_DRM_NEW_HARDWARE';
                                    HrDescr := 'Your media usage rights have become corrupted or are no longer valid.' +
                                               'This might happen if you have replaced hardware components in your computer.';
                                  end;
    LongInt($C00D11D7)          : begin
                                    HrStr := 'NS_E_WMP_DRM_INVALID_SIG';
                                    HrDescr := 'The required Windows Media Digital Rights Management (DRM) component cannot be validated.' +
                                               'You might be able resolve the problem by reinstalling the Player.';
                                  end;
    LongInt($C00D11D8)          : begin
                                    HrStr := 'NS_E_WMP_DRM_CANNOT_RESTORE';
                                    HrDescr := 'You have exceeded your restore limit for the day.' +
                                               'Try restoring your media usage rights tomorrow.';
                                  end;
    LongInt($C00D11D9)          : begin
                                    HrStr := 'NS_E_WMP_BURN_DISC_OVERFLOW';
                                    HrDescr := 'Some files might not fit on the CD.' +
                                               'The required space cannot be calculated accurately because some files might be missing duration information.' +
                                               'To ensure the calculation is accurate, play the files that are missing duration information.';
                                  end;
    LongInt($C00D11DA)          : begin
                                    HrStr := 'NS_E_WMP_DRM_GENERIC_LICENSE_FAILURE';
                                    HrDescr := 'Windows Media Player cannot verify the file''s media usage rights.' +
                                               'If you obtained this file from an online store, go to the online store to get the necessary rights.';
                                  end;
    LongInt($C00D11DB)          : begin
                                    HrStr := 'NS_E_WMP_DRM_NO_SECURE_CLOCK';
                                    HrDescr := 'It is not possible to sync because this device''s internal clock is not set correctly.' +
                                               'To set the clock, select the option to set the device clock on the Privacy tab of the Options dialog box, connect to the Internet, and then sync the device again.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D11DC)          : begin
                                    HrStr := 'NS_E_WMP_DRM_NO_RIGHTS';
                                    HrDescr := 'Windows Media Player cannot play, burn, rip, or sync the protected file because you do not have the appropriate rights.';
                                  end;
    LongInt($C00D11DD)          : begin
                                    HrStr := 'NS_E_WMP_DRM_INDIV_FAILED';
                                    HrDescr := 'Windows Media Player encountered an error during upgrade.';
                                  end;
    LongInt($C00D11DE)          : begin
                                    HrStr := 'NS_E_WMP_SERVER_NONEWCONNECTIONS';
                                    HrDescr := 'Windows Media Player cannot connect to the server because it is not accepting any new connections.' +
                                               'This could be because it has reached its maximum connection limit.' +
                                               'Please try again later.';
                                  end;
    LongInt($C00D11DF)          : begin
                                    HrStr := 'NS_E_WMP_MULTIPLE_ERROR_IN_PLAYLIST';
                                    HrDescr := 'A number of queued files cannot be played.' +
                                               'To find information about the problem, click the Now Playing tab, and then click the icon next to each file in the List pane.';
                                  end;
    LongInt($C00D11E0)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI2_ERASE_FAIL';
                                    HrDescr := 'Windows Media Player encountered an error while erasing the rewritable CD or DVD.' +
                                               'Verify that the CD or DVD burner is connected properly and that the disc is clean and not damaged.';
                                  end;
    LongInt($C00D11E1)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI2_ERASE_DEVICE_BUSY';
                                    HrDescr := 'Windows Media Player cannot erase the rewritable CD or DVD.' +
                                               'Verify that the CD or DVD burner is connected properly and that the disc is clean and not damaged.' +
                                               'If the burner is already in use, wait until the current task finishes or quit other programs that might be using the burner.';
                                  end;
    LongInt($C00D11E2)          : begin
                                    HrStr := 'NS_E_WMP_DRM_COMPONENT_FAILURE';
                                    HrDescr := 'A Windows Media Digital Rights Management (DRM) component encountered a problem.' +
                                               'If you are trying to use a file that you obtained from an online store, try going to the online store and getting the appropriate usage rights.';
                                  end;
    LongInt($C00D11E3)          : begin
                                    HrStr := 'NS_E_WMP_DRM_NO_DEVICE_CERT';
                                    HrDescr := 'It is not possible to obtain device''s certificate.' +
                                               'Please contact the device manufacturer for a firmware update or for other steps to resolve this problem.';
                                  end;
    LongInt($C00D11E4)          : begin
                                    HrStr := 'NS_E_WMP_SERVER_SECURITY_ERROR';
                                    HrDescr := 'Windows Media Player encountered an error when connecting to the server.' +
                                               'The security information from the server could not be validated.';
                                  end;
    LongInt($C00D11E5)          : begin
                                    HrStr := 'NS_E_WMP_AUDIO_DEVICE_LOST';
                                    HrDescr := 'An audio device was disconnected or reconfigured.' +
                                               'Verify that the audio device is connected, and then try to play the item again.';
                                  end;
    LongInt($C00D11E6)          : begin
                                    HrStr := 'NS_E_WMP_IMAPI_MEDIA_INCOMPATIBLE';
                                    HrDescr := 'Windows Media Player could not complete burning because the disc is not compatible with your drive.' +
                                               'Try inserting a different kind of recordable media or use a disc that supports a write speed that is compatible with your drive.';
                                  end;
    LongInt($C00D11EE)          : begin
                                    HrStr := 'NS_E_SYNCWIZ_DEVICE_FULL';
                                    HrDescr := 'Windows Media Player cannot save the sync settings because your device is full.' +
                                               'Delete some unneeded files on your device and then try again.';
                                  end;
    LongInt($C00D11EF)          : begin
                                    HrStr := 'NS_E_SYNCWIZ_CANNOT_CHANGE_SETTINGS';
                                    HrDescr := 'It is not possible to change sync settings at this time.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D11F0)          : begin
                                    HrStr := 'NS_E_TRANSCODE_DELETECACHEERROR';
                                    HrDescr := 'Windows Media Player cannot delete these files currently.' +
                                               'If the Player is synchronizing, wait until it is complete and then try again.';
                                  end;
    LongInt($C00D11F8)          : begin
                                    HrStr := 'NS_E_CD_NO_BUFFERS_READ';
                                    HrDescr := 'Windows Media Player could not use digital mode to read the CD.' +
                                               'The Player has automatically switched the CD drive to analog mode.' +
                                               'To switch back to digital mode, use the Devices tab.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D11F9)          : begin
                                    HrStr := 'NS_E_CD_EMPTY_TRACK_QUEUE';
                                    HrDescr := 'No CD track was specified for playback.';
                                  end;
    LongInt($C00D11FA)          : begin
                                    HrStr := 'NS_E_CD_NO_READER';
                                    HrDescr := 'The CD filter was not able to create the CD reader.';
                                  end;
    LongInt($C00D11FB)          : begin
                                    HrStr := 'NS_E_CD_ISRC_INVALID';
                                    HrDescr := 'Invalid ISRC code.';
                                  end;
    LongInt($C00D11FC)          : begin
                                    HrStr := 'NS_E_CD_MEDIA_CATALOG_NUMBER_INVALID';
                                    HrDescr := 'Invalid Media Catalog Number.';
                                  end;
    LongInt($C00D11FD)          : begin
                                    HrStr := 'NS_E_SLOW_READ_DIGITAL_WITH_ERRORCORRECTION';
                                    HrDescr := 'Windows Media Player cannot play audio CDs correctly because the CD drive is slow and error correction is turned on.' +
                                               'To increase performance, turn off playback error correction for this drive.';
                                  end;
    LongInt($C00D11FE)          : begin
                                    HrStr := 'NS_E_CD_SPEEDDETECT_NOT_ENOUGH_READS';
                                    HrDescr := 'Windows Media Player cannot estimate the CD drive''s playback speed because the CD track is too short.';
                                  end;
    LongInt($C00D11FF)          : begin
                                    HrStr := 'NS_E_CD_QUEUEING_DISABLED';
                                    HrDescr := 'Cannot queue the CD track because queuing is not enabled.';
                                  end;
    LongInt($C00D1202)          : begin
                                    HrStr := 'NS_E_WMP_DRM_ACQUIRING_LICENSE';
                                    HrDescr := 'Windows Media Player cannot download additional media usage rights until the current download is complete.';
                                  end;
    LongInt($C00D1203)          : begin
                                    HrStr := 'NS_E_WMP_DRM_LICENSE_EXPIRED';
                                    HrDescr := 'The media usage rights for this file have expired or are no longer valid.' +
                                               'If you obtained the file from an online store, sign in to the store, and then try again.';
                                  end;
    LongInt($C00D1204)          : begin
                                    HrStr := 'NS_E_WMP_DRM_LICENSE_NOTACQUIRED';
                                    HrDescr := 'Windows Media Player cannot download the media usage rights for the file.' +
                                               'If you obtained the file from an online store, sign in to the store, and then try again.';
                                  end;
    LongInt($C00D1205)          : begin
                                    HrStr := 'NS_E_WMP_DRM_LICENSE_NOTENABLED';
                                    HrDescr := 'The media usage rights for this file are not yet valid.' +
                                               'To see when they will become valid, right-click the file in the library, click Properties, and then click the Media Usage Rights tab.';
                                  end;
    LongInt($C00D1206)          : begin
                                    HrStr := 'NS_E_WMP_DRM_LICENSE_UNUSABLE';
                                    HrDescr := 'The media usage rights for this file are not valid.' +
                                               'If you obtained this file from an online store, contact the store for assistance.';
                                  end;
    LongInt($C00D1207)          : begin
                                    HrStr := 'NS_E_WMP_DRM_LICENSE_CONTENT_REVOKED';
                                    HrDescr := 'The content provider has revoked the media usage rights for this file.' +
                                               'If you obtained this file from an online store, ask the store if a new version of the file is available.';
                                  end;
    LongInt($C00D1208)          : begin
                                    HrStr := 'NS_E_WMP_DRM_LICENSE_NOSAP';
                                    HrDescr := 'The media usage rights for this file require a feature that is not supported in your current version of Windows Media Player or your current version of Windows.' +
                                               'Try installing the latest version of the Player.' +
                                               'If you obtained this file from an online store, contact the store for further assistance.';
                                  end;
    LongInt($C00D1209)          : begin
                                    HrStr := 'NS_E_WMP_DRM_UNABLE_TO_ACQUIRE_LICENSE';
                                    HrDescr := 'Windows Media Player cannot download media usage rights at this time.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D120A)          : begin
                                    HrStr := 'NS_E_WMP_LICENSE_REQUIRED';
                                    HrDescr := 'Windows Media Player cannot play, burn, or sync the file because the media usage rights are missing.' +
                                               'If you obtained the file from an online store, sign in to the store, and then try again.';
                                  end;
    LongInt($C00D120B)          : begin
                                    HrStr := 'NS_E_WMP_PROTECTED_CONTENT';
                                    HrDescr := 'Windows Media Player cannot play, burn, or sync the file because the media usage rights are missing.' +
                                               'If you obtained the file from an online store, sign in to the store, and then try again.';
                                  end;
    LongInt($C00D122A)          : begin
                                    HrStr := 'NS_E_WMP_POLICY_VALUE_NOT_CONFIGURED';
                                    HrDescr := 'Windows Media Player cannot read a policy.' +
                                               'This can occur when the policy does not exist in the registry or when the registry cannot be read.';
                                  end;
    LongInt($C00D1234)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_SYNC_FROM_INTERNET';
                                    HrDescr := 'Windows Media Player cannot sync content streamed directly from the Internet.' +
                                               'If possible, download the file to your computer, and then try to sync the file.';
                                  end;
    LongInt($C00D1235)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_SYNC_INVALID_PLAYLIST';
                                    HrDescr := 'This playlist is not valid or is corrupted.' +
                                               'Create a new playlist using Windows Media Player, then sync the new playlist instead.';
                                  end;
    LongInt($C00D1236)          : begin
                                    HrStr := 'NS_E_PDA_FAILED_TO_SYNCHRONIZE_FILE';
                                    HrDescr := 'Windows Media Player encountered a problem while synchronizing the file to the device.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D1237)          : begin
                                    HrStr := 'NS_E_PDA_SYNC_FAILED';
                                    HrDescr := 'Windows Media Player encountered an error while synchronizing to the device.';
                                  end;
    LongInt($C00D1238)          : begin
                                    HrStr := 'NS_E_PDA_DELETE_FAILED';
                                    HrDescr := 'Windows Media Player cannot delete a file from the device.';
                                  end;
    LongInt($C00D1239)          : begin
                                    HrStr := 'NS_E_PDA_FAILED_TO_RETRIEVE_FILE';
                                    HrDescr := 'Windows Media Player cannot copy a file from the device to your library.';
                                  end;
    LongInt($C00D123A)          : begin
                                    HrStr := 'NS_E_PDA_DEVICE_NOT_RESPONDING';
                                    HrDescr := 'Windows Media Player cannot communicate with the device because the device is not responding.' +
                                               'Try reconnecting the device, resetting the device, or contacting the device manufacturer for updated firmware.';
                                  end;
    LongInt($C00D123B)          : begin
                                    HrStr := 'NS_E_PDA_FAILED_TO_TRANSCODE_PHOTO';
                                    HrDescr := 'Windows Media Player cannot sync the picture to the device because a problem occurred while converting the file to another quality level or format.' +
                                               'The original file might be damaged or corrupted.';
                                  end;
    LongInt($C00D123C)          : begin
                                    HrStr := 'NS_E_PDA_FAILED_TO_ENCRYPT_TRANSCODED_FILE';
                                    HrDescr := 'Windows Media Player cannot convert the file.' +
                                               'The file might have been encrypted by the Encrypted File System (EFS).' +
                                               'Try decrypting the file first and then synchronizing it.' +
                                               'For information about how to decrypt a file, see Windows Help and Support.';
                                  end;
    LongInt($C00D123D)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_TRANSCODE_TO_AUDIO';
                                    HrDescr := 'Your device requires that this file be converted in order to play on the device.' +
                                               'However, the device either does not support playing audio, or Windows Media Player cannot convert the file to an audio format that is supported by the device.';
                                  end;
    LongInt($C00D123E)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_TRANSCODE_TO_VIDEO';
                                    HrDescr := 'Your device requires that this file be converted in order to play on the device.' +
                                               'However, the device either does not support playing video, or Windows Media Player cannot convert the file to a video format that is supported by the device.';
                                  end;
    LongInt($C00D123F)          : begin
                                    HrStr := 'NS_E_PDA_CANNOT_TRANSCODE_TO_IMAGE';
                                    HrDescr := 'Your device requires that this file be converted in order to play on the device.' +
                                               'However, the device either does not support displaying pictures, or Windows Media Player cannot convert the file to a picture format that is supported by the device.';
                                  end;
    LongInt($C00D1240)          : begin
                                    HrStr := 'NS_E_PDA_RETRIEVED_FILE_FILENAME_TOO_LONG';
                                    HrDescr := 'Windows Media Player cannot sync the file to your computer because the file name is too long.' +
                                               'Try renaming the file on the device.';
                                  end;
    LongInt($C00D1241)          : begin
                                    HrStr := 'NS_E_PDA_CEWMDM_DRM_ERROR';
                                    HrDescr := 'Windows Media Player cannot sync the file because the device is not responding.' +
                                               'This typically occurs when there is a problem with the device firmware.' +
                                               'For additional assistance, click Web Help.';
                                  end;
    LongInt($C00D1242)          : begin
                                    HrStr := 'NS_E_INCOMPLETE_PLAYLIST';
                                    HrDescr := 'Incomplete playlist.';
                                  end;
    LongInt($C00D1243)          : begin
                                    HrStr := 'NS_E_PDA_SYNC_RUNNING';
                                    HrDescr := 'It is not possible to perform the requested action because sync is in progress.' +
                                               'You can either stop sync or wait for it to complete, and then try again.';
                                  end;
    LongInt($C00D1244)          : begin
                                    HrStr := 'NS_E_PDA_SYNC_LOGIN_ERROR';
                                    HrDescr := 'Windows Media Player cannot sync the subscription content because you are not signed in to the online store that provided it.' +
                                               'Sign in to the online store, and then try again.';
                                  end;
    LongInt($C00D1245)          : begin
                                    HrStr := 'NS_E_PDA_TRANSCODE_CODEC_NOT_FOUND';
                                    HrDescr := 'Windows Media Player cannot convert the file to the format required by the device.' +
                                               'One or more codecs required to convert the file could not be found.';
                                  end;
    LongInt($C00D1246)          : begin
                                    HrStr := 'NS_E_CANNOT_SYNC_DRM_TO_NON_JANUS_DEVICE';
                                    HrDescr := 'It is not possible to sync subscription files to this device.';
                                  end;
    LongInt($C00D1247)          : begin
                                    HrStr := 'NS_E_CANNOT_SYNC_PREVIOUS_SYNC_RUNNING';
                                    HrDescr := 'Your device is operating slowly or is not responding.' +
                                               'Until the device responds, it is not possible to sync again.' +
                                               'To return the device to normal operation, try disconnecting it from the computer or resetting it.';
                                  end;
    LongInt($C00D125C)          : begin
                                    HrStr := 'NS_E_WMP_HWND_NOTFOUND';
                                    HrDescr := 'The Windows Media Player download manager cannot function properly because the Player main window cannot be found.' +
                                               'Try restarting the Player.';
                                  end;
    LongInt($C00D125D)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_WRONG_NO_FILES';
                                    HrDescr := 'Windows Media Player encountered a download that has the wrong number of files.' +
                                               'This might occur if another program is trying to create jobs with the same signature as the Player.';
                                  end;
    LongInt($C00D125E)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_COMPLETECANCELLEDJOB';
                                    HrDescr := 'Windows Media Player tried to complete a download that was already canceled.' +
                                               'The file will not be available.';
                                  end;
    LongInt($C00D125F)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_CANCELCOMPLETEDJOB';
                                    HrDescr := 'Windows Media Player tried to cancel a download that was already completed.' +
                                               'The file will not be removed.';
                                  end;
    LongInt($C00D1260)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_NOJOBPOINTER';
                                    HrDescr := 'Windows Media Player is trying to access a download that is not valid.';
                                  end;
    LongInt($C00D1261)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_INVALIDJOBSIGNATURE';
                                    HrDescr := 'This download was not created by Windows Media Player.';
                                  end;
    LongInt($C00D1262)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_FAILED_TO_CREATE_TEMPFILE';
                                    HrDescr := 'The Windows Media Player download manager cannot create a temporary file name.' +
                                               'This might occur if the path is not valid or if the disk is full.';
                                  end;
    LongInt($C00D1263)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_PLUGIN_FAILEDINITIALIZE';
                                    HrDescr := 'The Windows Media Player download manager plug-in cannot start.' +
                                               'This might occur if the system is out of resources.';
                                  end;
    LongInt($C00D1264)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_PLUGIN_FAILEDTOMOVEFILE';
                                    HrDescr := 'The Windows Media Player download manager cannot move the file.';
                                  end;
    LongInt($C00D1265)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_CALLFUNCFAILED';
                                    HrDescr := 'The Windows Media Player download manager cannot perform a task because the system has no resources to allocate.';
                                  end;
    LongInt($C00D1266)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_CALLFUNCTIMEOUT';
                                    HrDescr := 'The Windows Media Player download manager cannot perform a task because the task took too long to run.';
                                  end;
    LongInt($C00D1267)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_CALLFUNCENDED';
                                    HrDescr := 'The Windows Media Player download manager cannot perform a task because the Player is terminating the service.' +
                                               'The task will be recovered when the Player restarts.';
                                  end;
    LongInt($C00D1268)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_WMDUNPACKFAILED';
                                    HrDescr := 'The Windows Media Player download manager cannot expand a WMD file.' +
                                               'The file will be deleted and the operation will not be completed successfully.';
                                  end;
    LongInt($C00D1269)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_FAILEDINITIALIZE';
                                    HrDescr := 'The Windows Media Player download manager cannot start.' +
                                               'This might occur if the system is out of resources.';
                                  end;
    LongInt($C00D126A)          : begin
                                    HrStr := 'NS_E_INTERFACE_NOT_REGISTERED_IN_GIT';
                                    HrDescr := 'Windows Media Player cannot access a required functionality.' +
                                               'This might occur if the wrong system files or Player DLLs are loaded.';
                                  end;
    LongInt($C00D126B)          : begin
                                    HrStr := 'NS_E_BKGDOWNLOAD_INVALID_FILE_NAME';
                                    HrDescr := 'Windows Media Player cannot get the file name of the requested download.' +
                                               'The requested download will be canceled.';
                                  end;
    LongInt($C00D128E)          : begin
                                    HrStr := 'NS_E_IMAGE_DOWNLOAD_FAILED';
                                    HrDescr := 'Windows Media Player encountered an error while downloading an image.';
                                  end;
    LongInt($C00D12C0)          : begin
                                    HrStr := 'NS_E_WMP_UDRM_NOUSERLIST';
                                    HrDescr := 'Windows Media Player cannot update your media usage rights because the Player cannot verify the list of activated users of this computer.';
                                  end;
    LongInt($C00D12C1)          : begin
                                    HrStr := 'NS_E_WMP_DRM_NOT_ACQUIRING';
                                    HrDescr := 'Windows Media Player is trying to acquire media usage rights for a file that is no longer being used.' +
                                               'Rights acquisition will stop.';
                                  end;
    LongInt($C00D12F2)          : begin
                                    HrStr := 'NS_E_WMP_BSTR_TOO_LONG';
                                    HrDescr := 'The parameter is not valid.';
                                  end;
    LongInt($C00D12FC)          : begin
                                    HrStr := 'NS_E_WMP_AUTOPLAY_INVALID_STATE';
                                    HrDescr := 'The state is not valid for this request.';
                                  end;
    LongInt($C00D1306)          : begin
                                    HrStr := 'NS_E_WMP_COMPONENT_REVOKED';
                                    HrDescr := 'Windows Media Player cannot play this file until you complete the software component upgrade.' +
                                               'After the component has been upgraded, try to play the file again.';
                                  end;
    LongInt($C00D1324)          : begin
                                    HrStr := 'NS_E_CURL_NOTSAFE';
                                    HrDescr := 'The URL is not safe for the operation specified.';
                                  end;
    LongInt($C00D1325)          : begin
                                    HrStr := 'NS_E_CURL_INVALIDCHAR';
                                    HrDescr := 'The URL contains one or more characters that are not valid.';
                                  end;
    LongInt($C00D1326)          : begin
                                    HrStr := 'NS_E_CURL_INVALIDHOSTNAME';
                                    HrDescr := 'The URL contains a host name that is not valid.';
                                  end;
    LongInt($C00D1327)          : begin
                                    HrStr := 'NS_E_CURL_INVALIDPATH';
                                    HrDescr := 'The URL contains a path that is not valid.';
                                  end;
    LongInt($C00D1328)          : begin
                                    HrStr := 'NS_E_CURL_INVALIDSCHEME';
                                    HrDescr := 'The URL contains a scheme that is not valid.';
                                  end;
    LongInt($C00D1329)          : begin
                                    HrStr := 'NS_E_CURL_INVALIDURL';
                                    HrDescr := 'The URL is not valid.';
                                  end;
    LongInt($C00D132B)          : begin
                                    HrStr := 'NS_E_CURL_CANTWALK';
                                    HrDescr := 'Windows Media Player cannot play the file.' +
                                               'If you clicked a link on a web page, the link might not be valid.';
                                  end;
    LongInt($C00D132C)          : begin
                                    HrStr := 'NS_E_CURL_INVALIDPORT';
                                    HrDescr := 'The URL port is not valid.';
                                  end;
    LongInt($C00D132D)          : begin
                                    HrStr := 'NS_E_CURLHELPER_NOTADIRECTORY';
                                    HrDescr := 'The URL is not a directory.';
                                  end;
    LongInt($C00D132E)          : begin
                                    HrStr := 'NS_E_CURLHELPER_NOTAFILE';
                                    HrDescr := 'The URL is not a file.';
                                  end;
    LongInt($C00D132F)          : begin
                                    HrStr := 'NS_E_CURL_CANTDECODE';
                                    HrDescr := 'The URL contains characters that cannot be decoded.' +
                                               'The URL might be truncated or incomplete.';
                                  end;
    LongInt($C00D1330)          : begin
                                    HrStr := 'NS_E_CURLHELPER_NOTRELATIVE';
                                    HrDescr := 'The specified URL is not a relative URL.';
                                  end;
    LongInt($C00D1331)          : begin
                                    HrStr := 'NS_E_CURL_INVALIDBUFFERSIZE';
                                    HrDescr := 'The buffer is smaller than the size specified.';
                                  end;
    LongInt($C00D1356)          : begin
                                    HrStr := 'NS_E_SUBSCRIPTIONSERVICE_PLAYBACK_DISALLOWED';
                                    HrDescr := 'The content provider has not granted you the right to play this file.' +
                                               'Go to the content provider''s online store to get play rights.';
                                  end;
    LongInt($C00D1357)          : begin
                                    HrStr := 'NS_E_CANNOT_BUY_OR_DOWNLOAD_FROM_MULTIPLE_SERVICES';
                                    HrDescr := 'Windows Media Player cannot purchase or download content from multiple online stores.';
                                  end;
    LongInt($C00D1358)          : begin
                                    HrStr := 'NS_E_CANNOT_BUY_OR_DOWNLOAD_CONTENT';
                                    HrDescr := 'The file cannot be purchased or downloaded.' +
                                               'The file might not be available from the online store.';
                                  end;
    LongInt($C00D135A)          : begin
                                    HrStr := 'NS_E_NOT_CONTENT_PARTNER_TRACK';
                                    HrDescr := 'The provider of this file cannot be identified.';
                                  end;
    LongInt($C00D135B)          : begin
                                    HrStr := 'NS_E_TRACK_DOWNLOAD_REQUIRES_ALBUM_PURCHASE';
                                    HrDescr := 'The file is only available for download when you buy the entire album.';
                                  end;
    LongInt($C00D135C)          : begin
                                    HrStr := 'NS_E_TRACK_DOWNLOAD_REQUIRES_PURCHASE';
                                    HrDescr := 'You must buy the file before you can download it.';
                                  end;
    LongInt($C00D135D)          : begin
                                    HrStr := 'NS_E_TRACK_PURCHASE_MAXIMUM_EXCEEDED';
                                    HrDescr := 'You have exceeded the maximum number of files that can be purchased in a single transaction.';
                                  end;
    LongInt($C00D135F)          : begin
                                    HrStr := 'NS_E_SUBSCRIPTIONSERVICE_LOGIN_FAILED';
                                    HrDescr := 'Windows Media Player cannot sign in to the online store.' +
                                               'Verify that you are using the correct user name and password.' +
                                               'If the problem persists, the store might be temporarily unavailable.';
                                  end;
    LongInt($C00D1360)          : begin
                                    HrStr := 'NS_E_SUBSCRIPTIONSERVICE_DOWNLOAD_TIMEOUT';
                                    HrDescr := 'Windows Media Player cannot download this item because the server is not responding.' +
                                               'The server might be temporarily unavailable or the Internet connection might be lost.';
                                  end;
    LongInt($C00D1362)          : begin
                                    HrStr := 'NS_E_CONTENT_PARTNER_STILL_INITIALIZING';
                                    HrDescr := 'Content Partner still initializing.';
                                  end;
    LongInt($C00D1363)          : begin
                                    HrStr := 'NS_E_OPEN_CONTAINING_FOLDER_FAILED';
                                    HrDescr := 'The folder could not be opened.' +
                                               'The folder might have been moved or deleted.';
                                  end;
    LongInt($C00D136A)          : begin
                                    HrStr := 'NS_E_ADVANCEDEDIT_TOO_MANY_PICTURES';
                                    HrDescr := 'Windows Media Player could not add all of the images to the file because the images exceeded the 7 megabyte (MB) limit.';
                                  end;
    LongInt($C00D1388)          : begin
                                    HrStr := 'NS_E_REDIRECT';
                                    HrDescr := 'The client redirected to another server.';
                                  end;
    LongInt($C00D1389)          : begin
                                    HrStr := 'NS_E_STALE_PRESENTATION';
                                    HrDescr := 'The streaming media description is no longer current.';
                                  end;
    LongInt($C00D138A)          : begin
                                    HrStr := 'NS_E_NAMESPACE_WRONG_PERSIST';
                                    HrDescr := 'It is not possible to create a persistent namespace node under a transient parent node.';
                                  end;
    LongInt($C00D138B)          : begin
                                    HrStr := 'NS_E_NAMESPACE_WRONG_TYPE';
                                    HrDescr := 'It is not possible to store a value in a namespace node that has a different value type.';
                                  end;
    LongInt($C00D138C)          : begin
                                    HrStr := 'NS_E_NAMESPACE_NODE_CONFLICT';
                                    HrDescr := 'It is not possible to remove the root namespace node.';
                                  end;
    LongInt($C00D138D)          : begin
                                    HrStr := 'NS_E_NAMESPACE_NODE_NOT_FOUND';
                                    HrDescr := 'The specified namespace node could not be found.';
                                  end;
    LongInt($C00D138E)          : begin
                                    HrStr := 'NS_E_NAMESPACE_BUFFER_TOO_SMALL';
                                    HrDescr := 'The buffer supplied to hold namespace node string is too small.';
                                  end;
    LongInt($C00D138F)          : begin
                                    HrStr := 'NS_E_NAMESPACE_TOO_MANY_CALLBACKS';
                                    HrDescr := 'The callback list on a namespace node is at the maximum size.';
                                  end;
    LongInt($C00D1390)          : begin
                                    HrStr := 'NS_E_NAMESPACE_DUPLICATE_CALLBACK';
                                    HrDescr := 'It is not possible to register an already-registered callback on a namespace node.';
                                  end;
    LongInt($C00D1391)          : begin
                                    HrStr := 'NS_E_NAMESPACE_CALLBACK_NOT_FOUND';
                                    HrDescr := 'Cannot find the callback in the namespace when attempting to remove the callback.';
                                  end;
    LongInt($C00D1392)          : begin
                                    HrStr := 'NS_E_NAMESPACE_NAME_TOO_LONG';
                                    HrDescr := 'The namespace node name exceeds the allowed maximum length.';
                                  end;
    LongInt($C00D1393)          : begin
                                    HrStr := 'NS_E_NAMESPACE_DUPLICATE_NAME';
                                    HrDescr := 'Cannot create a namespace node that already exists.';
                                  end;
    LongInt($C00D1394)          : begin
                                    HrStr := 'NS_E_NAMESPACE_EMPTY_NAME';
                                    HrDescr := 'The namespace node name cannot be a null string.';
                                  end;
    LongInt($C00D1395)          : begin
                                    HrStr := 'NS_E_NAMESPACE_INDEX_TOO_LARGE';
                                    HrDescr := 'Finding a child namespace node by index failed because the index exceeded the number of children.';
                                  end;
    LongInt($C00D1396)          : begin
                                    HrStr := 'NS_E_NAMESPACE_BAD_NAME';
                                    HrDescr := 'The namespace node name is invalid.';
                                  end;
    LongInt($C00D1397)          : begin
                                    HrStr := 'NS_E_NAMESPACE_WRONG_SECURITY';
                                    HrDescr := 'It is not possible to store a value in a namespace node that has a different security type.';
                                  end;
    LongInt($C00D13EC)          : begin
                                    HrStr := 'NS_E_CACHE_ARCHIVE_CONFLICT';
                                    HrDescr := 'The archive request conflicts with other requests in progress.';
                                  end;
    LongInt($C00D13ED)          : begin
                                    HrStr := 'NS_E_CACHE_ORIGIN_SERVER_NOT_FOUND';
                                    HrDescr := 'The specified origin server cannot be found.';
                                  end;
    LongInt($C00D13EE)          : begin
                                    HrStr := 'NS_E_CACHE_ORIGIN_SERVER_TIMEOUT';
                                    HrDescr := 'The specified origin server is not responding.';
                                  end;
    LongInt($C00D13EF)          : begin
                                    HrStr := 'NS_E_CACHE_NOT_BROADCAST';
                                    HrDescr := 'The internal code for HTTP status code 412 Precondition Failed due to not broadcast type.';
                                  end;
    LongInt($C00D13F0)          : begin
                                    HrStr := 'NS_E_CACHE_CANNOT_BE_CACHED';
                                    HrDescr := 'The internal code for HTTP status code 403 Forbidden due to not cacheable.';
                                  end;
    LongInt($C00D13F1)          : begin
                                    HrStr := 'NS_E_CACHE_NOT_MODIFIED';
                                    HrDescr := 'The internal code for HTTP status code 304 Not Modified.';
                                  end;
    LongInt($C00D1450)          : begin
                                    HrStr := 'NS_E_CANNOT_REMOVE_PUBLISHING_POINT';
                                    HrDescr := 'It is not possible to remove a cache or proxy publishing point.';
                                  end;
    LongInt($C00D1451)          : begin
                                    HrStr := 'NS_E_CANNOT_REMOVE_PLUGIN';
                                    HrDescr := 'It is not possible to remove the last instance of a type of plug-in.';
                                  end;
    LongInt($C00D1452)          : begin
                                    HrStr := 'NS_E_WRONG_PUBLISHING_POINT_TYPE';
                                    HrDescr := 'Cache and proxy publishing points do not support this property or method.';
                                  end;
    LongInt($C00D1453)          : begin
                                    HrStr := 'NS_E_UNSUPPORTED_LOAD_TYPE';
                                    HrDescr := 'The plug-in does not support the specified load type.';
                                  end;
    LongInt($C00D1454)          : begin
                                    HrStr := 'NS_E_INVALID_PLUGIN_LOAD_TYPE_CONFIGURATION';
                                    HrDescr := 'The plug-in does not support any load types.' +
                                               'The plug-in must support at least one load type.';
                                  end;
    LongInt($C00D1455)          : begin
                                    HrStr := 'NS_E_INVALID_PUBLISHING_POINT_NAME';
                                    HrDescr := 'The publishing point name is invalid.';
                                  end;
    LongInt($C00D1456)          : begin
                                    HrStr := 'NS_E_TOO_MANY_MULTICAST_SINKS';
                                    HrDescr := 'Only one multicast data writer plug-in can be enabled for a publishing point.';
                                  end;
    LongInt($C00D1457)          : begin
                                    HrStr := 'NS_E_PUBLISHING_POINT_INVALID_REQUEST_WHILE_STARTED';
                                    HrDescr := 'The requested operation cannot be completed while the publishing point is started.';
                                  end;
    LongInt($C00D1458)          : begin
                                    HrStr := 'NS_E_MULTICAST_PLUGIN_NOT_ENABLED';
                                    HrDescr := 'A multicast data writer plug-in must be enabled in order for this operation to be completed.';
                                  end;
    LongInt($C00D1459)          : begin
                                    HrStr := 'NS_E_INVALID_OPERATING_SYSTEM_VERSION';
                                    HrDescr := 'This feature requires Windows Server 2003, Enterprise Edition.';
                                  end;
    LongInt($C00D145A)          : begin
                                    HrStr := 'NS_E_PUBLISHING_POINT_REMOVED';
                                    HrDescr := 'The requested operation cannot be completed because the specified publishing point has been removed.';
                                  end;
    LongInt($C00D145B)          : begin
                                    HrStr := 'NS_E_INVALID_PUSH_PUBLISHING_POINT_START_REQUEST';
                                    HrDescr := 'Push publishing points are started when the encoder starts pushing the stream.' +
                                               'This publishing point cannot be started by the server administrator.';
                                  end;
    LongInt($C00D145C)          : begin
                                    HrStr := 'NS_E_UNSUPPORTED_LANGUAGE';
                                    HrDescr := 'The specified language is not supported.';
                                  end;
    LongInt($C00D145D)          : begin
                                    HrStr := 'NS_E_WRONG_OS_VERSION';
                                    HrDescr := 'Windows Media Services will only run on Windows Server 2003, Standard Edition and Windows Server 2003, Enterprise Edition.';
                                  end;
    LongInt($C00D145E)          : begin
                                    HrStr := 'NS_E_PUBLISHING_POINT_STOPPED';
                                    HrDescr := 'The operation cannot be completed because the publishing point has been stopped.';
                                  end;
    LongInt($C00D14B4)          : begin
                                    HrStr := 'NS_E_PLAYLIST_ENTRY_ALREADY_PLAYING';
                                    HrDescr := 'The playlist entry is already playing.';
                                  end;
    LongInt($C00D14B5)          : begin
                                    HrStr := 'NS_E_EMPTY_PLAYLIST';
                                    HrDescr := 'The playlist or directory you are requesting does not contain content.';
                                  end;
    LongInt($C00D14B6)          : begin
                                    HrStr := 'NS_E_PLAYLIST_PARSE_FAILURE';
                                    HrDescr := 'The server was unable to parse the requested playlist file.';
                                  end;
    LongInt($C00D14B7)          : begin
                                    HrStr := 'NS_E_PLAYLIST_UNSUPPORTED_ENTRY';
                                    HrDescr := 'The requested operation is not supported for this type of playlist entry.';
                                  end;
    LongInt($C00D14B8)          : begin
                                    HrStr := 'NS_E_PLAYLIST_ENTRY_NOT_IN_PLAYLIST';
                                    HrDescr := 'Cannot jump to a playlist entry that is not inserted in the playlist.';
                                  end;
    LongInt($C00D14B9)          : begin
                                    HrStr := 'NS_E_PLAYLIST_ENTRY_SEEK';
                                    HrDescr := 'Cannot seek to the desired playlist entry.';
                                  end;
    LongInt($C00D14BA)          : begin
                                    HrStr := 'NS_E_PLAYLIST_RECURSIVE_PLAYLISTS';
                                    HrDescr := 'Cannot play recursive playlist.';
                                  end;
    LongInt($C00D14BB)          : begin
                                    HrStr := 'NS_E_PLAYLIST_TOO_MANY_NESTED_PLAYLISTS';
                                    HrDescr := 'The number of nested playlists exceeded the limit the server can handle.';
                                  end;
    LongInt($C00D14BC)          : begin
                                    HrStr := 'NS_E_PLAYLIST_SHUTDOWN';
                                    HrDescr := 'Cannot execute the requested operation because the playlist has been shut down by the Media Server.';
                                  end;
    LongInt($C00D14BD)          : begin
                                    HrStr := 'NS_E_PLAYLIST_END_RECEDING';
                                    HrDescr := 'The playlist has ended while receding.';
                                  end;
    LongInt($C00D1518)          : begin
                                    HrStr := 'NS_E_DATAPATH_NO_SINK';
                                    HrDescr := 'The data path does not have an associated data writer plug-in.';
                                  end;
    LongInt($C00D151A)          : begin
                                    HrStr := 'NS_E_INVALID_PUSH_TEMPLATE';
                                    HrDescr := 'The specified push template is invalid.';
                                  end;
    LongInt($C00D151B)          : begin
                                    HrStr := 'NS_E_INVALID_PUSH_PUBLISHING_POINT';
                                    HrDescr := 'The specified push publishing point is invalid.';
                                  end;
    LongInt($C00D151C)          : begin
                                    HrStr := 'NS_E_CRITICAL_ERROR';
                                    HrDescr := 'The requested operation cannot be performed because the server or publishing point is in a critical error state.';
                                  end;
    LongInt($C00D151D)          : begin
                                    HrStr := 'NS_E_NO_NEW_CONNECTIONS';
                                    HrDescr := 'The content cannot be played because the server is not currently accepting connections.' +
                                               'Try connecting at a later time.';
                                  end;
    LongInt($C00D151E)          : begin
                                    HrStr := 'NS_E_WSX_INVALID_VERSION';
                                    HrDescr := 'The version of this playlist is not supported by the server.';
                                  end;
    LongInt($C00D151F)          : begin
                                    HrStr := 'NS_E_HEADER_MISMATCH';
                                    HrDescr := 'The command does not apply to the current media header user by a server component.';
                                  end;
    LongInt($C00D1520)          : begin
                                    HrStr := 'NS_E_PUSH_DUPLICATE_PUBLISHING_POINT_NAME';
                                    HrDescr := 'The specified publishing point name is already in use.';
                                  end;
    LongInt($C00D157C)          : begin
                                    HrStr := 'NS_E_NO_SCRIPT_ENGINE';
                                    HrDescr := 'There is no script engine available for this file.';
                                  end;
    LongInt($C00D157D)          : begin
                                    HrStr := 'NS_E_PLUGIN_ERROR_REPORTED';
                                    HrDescr := 'The plug-in has reported an error.' +
                                               'See the Troubleshooting tab or the NT Application Event Log for details.';
                                  end;
    LongInt($C00D157E)          : begin
                                    HrStr := 'NS_E_SOURCE_PLUGIN_NOT_FOUND';
                                    HrDescr := 'No enabled data source plug-in is available to access the requested content.';
                                  end;
    LongInt($C00D157F)          : begin
                                    HrStr := 'NS_E_PLAYLIST_PLUGIN_NOT_FOUND';
                                    HrDescr := 'No enabled playlist parser plug-in is available to access the requested content.';
                                  end;
    LongInt($C00D1580)          : begin
                                    HrStr := 'NS_E_DATA_SOURCE_ENUMERATION_NOT_SUPPORTED';
                                    HrDescr := 'The data source plug-in does not support enumeration.';
                                  end;
    LongInt($C00D1581)          : begin
                                    HrStr := 'NS_E_MEDIA_PARSER_INVALID_FORMAT';
                                    HrDescr := 'The server cannot stream the selected file because it is either damaged or corrupt.' +
                                               'Select a different file.';
                                  end;
    LongInt($C00D1582)          : begin
                                    HrStr := 'NS_E_SCRIPT_DEBUGGER_NOT_INSTALLED';
                                    HrDescr := 'The plug-in cannot be enabled because a compatible script debugger is not installed on this system.' +
                                               'Install a script debugger, or disable the script debugger option on the general tab of the plug-in''s properties page and try again.';
                                  end;
    LongInt($C00D1583)          : begin
                                    HrStr := 'NS_E_FEATURE_REQUIRES_ENTERPRISE_SERVER';
                                    HrDescr := 'The plug-in cannot be loaded because it requires Windows Server 2003, Enterprise Edition.';
                                  end;
    LongInt($C00D1584)          : begin
                                    HrStr := 'NS_E_WIZARD_RUNNING';
                                    HrDescr := 'Another wizard is currently running.' +
                                               'Please close the other wizard or wait until it finishes before attempting to run this wizard again.';
                                  end;
    LongInt($C00D1585)          : begin
                                    HrStr := 'NS_E_INVALID_LOG_URL';
                                    HrDescr := 'Invalid log URL.' +
                                               'Multicast logging URL must look like "http://servername/isapibackend.dll".';
                                  end;
    LongInt($C00D1586)          : begin
                                    HrStr := 'NS_E_INVALID_MTU_RANGE';
                                    HrDescr := 'Invalid MTU specified.' +
                                               'The valid range for maximum packet size is between 36 and 65507 bytes.';
                                  end;
    LongInt($C00D1587)          : begin
                                    HrStr := 'NS_E_INVALID_PLAY_STATISTICS';
                                    HrDescr := 'Invalid play statistics for logging.';
                                  end;
    LongInt($C00D1588)          : begin
                                    HrStr := 'NS_E_LOG_NEED_TO_BE_SKIPPED';
                                    HrDescr := 'The log needs to be skipped.';
                                  end;
    LongInt($C00D1589)          : begin
                                    HrStr := 'NS_E_HTTP_TEXT_DATACONTAINER_SIZE_LIMIT_EXCEEDED';
                                    HrDescr := 'The size of the data exceeded the limit the WMS HTTP Download Data Source plugin can handle.';
                                  end;
    LongInt($C00D158A)          : begin
                                    HrStr := 'NS_E_PORT_IN_USE';
                                    HrDescr := 'One usage of each socket address (protocol/network address/port) is permitted.' +
                                               'Verify that other services or applications are not attempting to use the same port and then try to enable the plug-in again.';
                                  end;
    LongInt($C00D158B)          : begin
                                    HrStr := 'NS_E_PORT_IN_USE_HTTP';
                                    HrDescr := 'One usage of each socket address (protocol/network address/port) is permitted.' +
                                               'Verify that other services (such as IIS) or applications are not attempting to use the same port and then try to enable the plug-in again.';
                                  end;
    LongInt($C00D158C)          : begin
                                    HrStr := 'NS_E_HTTP_TEXT_DATACONTAINER_INVALID_SERVER_RESPONSE';
                                    HrDescr := 'The WMS HTTP Download Data Source plugin was unable to receive the remote server''s response.';
                                  end;
    LongInt($C00D158D)          : begin
                                    HrStr := 'NS_E_ARCHIVE_REACH_QUOTA';
                                    HrDescr := 'The archive plug-in has reached its quota.';
                                  end;
    LongInt($C00D158E)          : begin
                                    HrStr := 'NS_E_ARCHIVE_ABORT_DUE_TO_BCAST';
                                    HrDescr := 'The archive plug-in aborted because the source was from broadcast.';
                                  end;
    LongInt($C00D158F)          : begin
                                    HrStr := 'NS_E_ARCHIVE_GAP_DETECTED';
                                    HrDescr := 'The archive plug-in detected an interrupt in the source.';
                                  end;
    LongInt($C00D1590)          : begin
                                    HrStr := 'NS_E_AUTHORIZATION_FILE_NOT_FOUND';
                                    HrDescr := 'The system cannot find the file specified.';
                                  end;
    LongInt($C00D1B58)          : begin
                                    HrStr := 'NS_E_BAD_MARKIN';
                                    HrDescr := 'The mark-in time should be greater than 0 and less than the mark-out time.';
                                  end;
    LongInt($C00D1B59)          : begin
                                    HrStr := 'NS_E_BAD_MARKOUT';
                                    HrDescr := 'The mark-out time should be greater than the mark-in time and less than the file duration.';
                                  end;
    LongInt($C00D1B5A)          : begin
                                    HrStr := 'NS_E_NOMATCHING_MEDIASOURCE';
                                    HrDescr := 'No matching media type is found in the source %1.';
                                  end;
    LongInt($C00D1B5B)          : begin
                                    HrStr := 'NS_E_UNSUPPORTED_SOURCETYPE';
                                    HrDescr := 'The specified source type is not supported.';
                                  end;
    LongInt($C00D1B5C)          : begin
                                    HrStr := 'NS_E_TOO_MANY_AUDIO';
                                    HrDescr := 'It is not possible to specify more than one audio input.';
                                  end;
    LongInt($C00D1B5D)          : begin
                                    HrStr := 'NS_E_TOO_MANY_VIDEO';
                                    HrDescr := 'It is not possible to specify more than two video inputs.';
                                  end;
    LongInt($C00D1B5E)          : begin
                                    HrStr := 'NS_E_NOMATCHING_ELEMENT';
                                    HrDescr := 'No matching element is found in the list.';
                                  end;
    LongInt($C00D1B5F)          : begin
                                    HrStr := 'NS_E_MISMATCHED_MEDIACONTENT';
                                    HrDescr := 'The profile''s media types must match the media types defined for the session.';
                                  end;
    LongInt($C00D1B60)          : begin
                                    HrStr := 'NS_E_CANNOT_DELETE_ACTIVE_SOURCEGROUP';
                                    HrDescr := 'It is not possible to remove an active source while encoding.';
                                  end;
    LongInt($C00D1B61)          : begin
                                    HrStr := 'NS_E_AUDIODEVICE_BUSY';
                                    HrDescr := 'It is not possible to open the specified audio capture device because it is currently in use.';
                                  end;
    LongInt($C00D1B62)          : begin
                                    HrStr := 'NS_E_AUDIODEVICE_UNEXPECTED';
                                    HrDescr := 'It is not possible to open the specified audio capture device because an unexpected error has occurred.';
                                  end;
    LongInt($C00D1B63)          : begin
                                    HrStr := 'NS_E_AUDIODEVICE_BADFORMAT';
                                    HrDescr := 'The audio capture device does not support the specified audio format.';
                                  end;
    LongInt($C00D1B64)          : begin
                                    HrStr := 'NS_E_VIDEODEVICE_BUSY';
                                    HrDescr := 'It is not possible to open the specified video capture device because it is currently in use.';
                                  end;
    LongInt($C00D1B65)          : begin
                                    HrStr := 'NS_E_VIDEODEVICE_UNEXPECTED';
                                    HrDescr := 'It is not possible to open the specified video capture device because an unexpected error has occurred.';
                                  end;
    LongInt($C00D1B66)          : begin
                                    HrStr := 'NS_E_INVALIDCALL_WHILE_ENCODER_RUNNING';
                                    HrDescr := 'This operation is not allowed while encoding.';
                                  end;
    LongInt($C00D1B67)          : begin
                                    HrStr := 'NS_E_NO_PROFILE_IN_SOURCEGROUP';
                                    HrDescr := 'No profile is set for the source.';
                                  end;
    LongInt($C00D1B68)          : begin
                                    HrStr := 'NS_E_VIDEODRIVER_UNSTABLE';
                                    HrDescr := 'The video capture driver returned an unrecoverable error.' +
                                               'It is now in an unstable state.';
                                  end;
    LongInt($C00D1B69)          : begin
                                    HrStr := 'NS_E_VIDCAPSTARTFAILED';
                                    HrDescr := 'It was not possible to start the video device.';
                                  end;
    LongInt($C00D1B6A)          : begin
                                    HrStr := 'NS_E_VIDSOURCECOMPRESSION';
                                    HrDescr := 'The video source does not support the requested output format or color depth.';
                                  end;
    LongInt($C00D1B6B)          : begin
                                    HrStr := 'NS_E_VIDSOURCESIZE';
                                    HrDescr := 'The video source does not support the requested capture size.';
                                  end;
    LongInt($C00D1B6C)          : begin
                                    HrStr := 'NS_E_ICMQUERYFORMAT';
                                    HrDescr := 'It was not possible to obtain output information from the video compressor.';
                                  end;
    LongInt($C00D1B6D)          : begin
                                    HrStr := 'NS_E_VIDCAPCREATEWINDOW';
                                    HrDescr := 'It was not possible to create a video capture window.';
                                  end;
    LongInt($C00D1B6E)          : begin
                                    HrStr := 'NS_E_VIDCAPDRVINUSE';
                                    HrDescr := 'There is already a stream active on this video device.';
                                  end;
    LongInt($C00D1B6F)          : begin
                                    HrStr := 'NS_E_NO_MEDIAFORMAT_IN_SOURCE';
                                    HrDescr := 'No media format is set in source.';
                                  end;
    LongInt($C00D1B70)          : begin
                                    HrStr := 'NS_E_NO_VALID_OUTPUT_STREAM';
                                    HrDescr := 'Cannot find a valid output stream from the source.';
                                  end;
    LongInt($C00D1B71)          : begin
                                    HrStr := 'NS_E_NO_VALID_SOURCE_PLUGIN';
                                    HrDescr := 'It was not possible to find a valid source plug-in for the specified source.';
                                  end;
    LongInt($C00D1B72)          : begin
                                    HrStr := 'NS_E_NO_ACTIVE_SOURCEGROUP';
                                    HrDescr := 'No source is currently active.';
                                  end;
    LongInt($C00D1B73)          : begin
                                    HrStr := 'NS_E_NO_SCRIPT_STREAM';
                                    HrDescr := 'No script stream is set in the current source.';
                                  end;
    LongInt($C00D1B74)          : begin
                                    HrStr := 'NS_E_INVALIDCALL_WHILE_ARCHIVAL_RUNNING';
                                    HrDescr := 'This operation is not allowed while archiving.';
                                  end;
    LongInt($C00D1B75)          : begin
                                    HrStr := 'NS_E_INVALIDPACKETSIZE';
                                    HrDescr := 'The setting for the maximum packet size is not valid.';
                                  end;
    LongInt($C00D1B76)          : begin
                                    HrStr := 'NS_E_PLUGIN_CLSID_INVALID';
                                    HrDescr := 'The plug-in CLSID specified is not valid.';
                                  end;
    LongInt($C00D1B77)          : begin
                                    HrStr := 'NS_E_UNSUPPORTED_ARCHIVETYPE';
                                    HrDescr := 'This archive type is not supported.';
                                  end;
    LongInt($C00D1B78)          : begin
                                    HrStr := 'NS_E_UNSUPPORTED_ARCHIVEOPERATION';
                                    HrDescr := 'This archive operation is not supported.';
                                  end;
    LongInt($C00D1B79)          : begin
                                    HrStr := 'NS_E_ARCHIVE_FILENAME_NOTSET';
                                    HrDescr := 'The local archive file name was not set.';
                                  end;
    LongInt($C00D1B7A)          : begin
                                    HrStr := 'NS_E_SOURCEGROUP_NOTPREPARED';
                                    HrDescr := 'The source is not yet prepared.';
                                  end;
    LongInt($C00D1B7B)          : begin
                                    HrStr := 'NS_E_PROFILE_MISMATCH';
                                    HrDescr := 'Profiles on the sources do not match.';
                                  end;
    LongInt($C00D1B7C)          : begin
                                    HrStr := 'NS_E_INCORRECTCLIPSETTINGS';
                                    HrDescr := 'The specified crop values are not valid.';
                                  end;
    LongInt($C00D1B7D)          : begin
                                    HrStr := 'NS_E_NOSTATSAVAILABLE';
                                    HrDescr := 'No statistics are available at this time.';
                                  end;
    LongInt($C00D1B7E)          : begin
                                    HrStr := 'NS_E_NOTARCHIVING';
                                    HrDescr := 'The encoder is not archiving.';
                                  end;
    LongInt($C00D1B7F)          : begin
                                    HrStr := 'NS_E_INVALIDCALL_WHILE_ENCODER_STOPPED';
                                    HrDescr := 'This operation is only allowed during encoding.';
                                  end;
    LongInt($C00D1B80)          : begin
                                    HrStr := 'NS_E_NOSOURCEGROUPS';
                                    HrDescr := 'This SourceGroupCollection doesn''t contain any SourceGroups.';
                                  end;
    LongInt($C00D1B81)          : begin
                                    HrStr := 'NS_E_INVALIDINPUTFPS';
                                    HrDescr := 'This source does not have a frame rate of 30 fps.' +
                                               'Therefore, it is not possible to apply the inverse telecine filter to the source.';
                                  end;
    LongInt($C00D1B82)          : begin
                                    HrStr := 'NS_E_NO_DATAVIEW_SUPPORT';
                                    HrDescr := 'It is not possible to display your source or output video in the Video panel.';
                                  end;
    LongInt($C00D1B83)          : begin
                                    HrStr := 'NS_E_CODEC_UNAVAILABLE';
                                    HrDescr := 'One or more codecs required to open this content could not be found.';
                                  end;
    LongInt($C00D1B84)          : begin
                                    HrStr := 'NS_E_ARCHIVE_SAME_AS_INPUT';
                                    HrDescr := 'The archive file has the same name as an input file.' +
                                               'Change one of the names before continuing.';
                                  end;
    LongInt($C00D1B85)          : begin
                                    HrStr := 'NS_E_SOURCE_NOTSPECIFIED';
                                    HrDescr := 'The source has not been set up completely.';
                                  end;
    LongInt($C00D1B86)          : begin
                                    HrStr := 'NS_E_NO_REALTIME_TIMECOMPRESSION';
                                    HrDescr := 'It is not possible to apply time compression to a broadcast session.';
                                  end;
    LongInt($C00D1B87)          : begin
                                    HrStr := 'NS_E_UNSUPPORTED_ENCODER_DEVICE';
                                    HrDescr := 'It is not possible to open this device.';
                                  end;
    LongInt($C00D1B88)          : begin
                                    HrStr := 'NS_E_UNEXPECTED_DISPLAY_SETTINGS';
                                    HrDescr := 'It is not possible to start encoding because the display size or color has changed since the current session was defined.' +
                                               'Restore the previous settings or create a new session.';
                                  end;
    LongInt($C00D1B89)          : begin
                                    HrStr := 'NS_E_NO_AUDIODATA';
                                    HrDescr := 'No audio data has been received for several seconds.' +
                                               'Check the audio source and restart the encoder.';
                                  end;
    LongInt($C00D1B8A)          : begin
                                    HrStr := 'NS_E_INPUTSOURCE_PROBLEM';
                                    HrDescr := 'One or all of the specified sources are not working properly.' +
                                               'Check that the sources are configured correctly.';
                                  end;
    LongInt($C00D1B8B)          : begin
                                    HrStr := 'NS_E_WME_VERSION_MISMATCH';
                                    HrDescr := 'The supplied configuration file is not supported by this version of the encoder.';
                                  end;
    LongInt($C00D1B8C)          : begin
                                    HrStr := 'NS_E_NO_REALTIME_PREPROCESS';
                                    HrDescr := 'It is not possible to use image preprocessing with live encoding.';
                                  end;
    LongInt($C00D1B8D)          : begin
                                    HrStr := 'NS_E_NO_REPEAT_PREPROCESS';
                                    HrDescr := 'It is not possible to use two-pass encoding when the source is set to loop.';
                                  end;
    LongInt($C00D1B8E)          : begin
                                    HrStr := 'NS_E_CANNOT_PAUSE_LIVEBROADCAST';
                                    HrDescr := 'It is not possible to pause encoding during a broadcast.';
                                  end;
    LongInt($C00D1B8F)          : begin
                                    HrStr := 'NS_E_DRM_PROFILE_NOT_SET';
                                    HrDescr := 'A DRM profile has not been set for the current session.';
                                  end;
    LongInt($C00D1B90)          : begin
                                    HrStr := 'NS_E_DUPLICATE_DRMPROFILE';
                                    HrDescr := 'The profile ID is already used by a DRM profile.' +
                                               'Specify a different profile ID.';
                                  end;
    LongInt($C00D1B91)          : begin
                                    HrStr := 'NS_E_INVALID_DEVICE';
                                    HrDescr := 'The setting of the selected device does not support control for playing back tapes.';
                                  end;
    LongInt($C00D1B92)          : begin
                                    HrStr := 'NS_E_SPEECHEDL_ON_NON_MIXEDMODE';
                                    HrDescr := 'You must specify a mixed voice and audio mode in order to use an optimization definition file.';
                                  end;
    LongInt($C00D1B93)          : begin
                                    HrStr := 'NS_E_DRM_PASSWORD_TOO_LONG';
                                    HrDescr := 'The specified password is too long.' +
                                               'Type a password with fewer than 8 characters.';
                                  end;
    LongInt($C00D1B94)          : begin
                                    HrStr := 'NS_E_DEVCONTROL_FAILED_SEEK';
                                    HrDescr := 'It is not possible to seek to the specified mark-in point.';
                                  end;
    LongInt($C00D1B95)          : begin
                                    HrStr := 'NS_E_INTERLACE_REQUIRE_SAMESIZE';
                                    HrDescr := 'When you choose to maintain the interlacing in your video, the output video size must match the input video size.';
                                  end;
    LongInt($C00D1B96)          : begin
                                    HrStr := 'NS_E_TOO_MANY_DEVICECONTROL';
                                    HrDescr := 'Only one device control plug-in can control a device.';
                                  end;
    LongInt($C00D1B97)          : begin
                                    HrStr := 'NS_E_NO_MULTIPASS_FOR_LIVEDEVICE';
                                    HrDescr := 'You must also enable storing content to hard disk temporarily in order to use two-pass encoding with the input device.';
                                  end;
    LongInt($C00D1B98)          : begin
                                    HrStr := 'NS_E_MISSING_AUDIENCE';
                                    HrDescr := 'An audience is missing from the output stream configuration.';
                                  end;
    LongInt($C00D1B99)          : begin
                                    HrStr := 'NS_E_AUDIENCE_CONTENTTYPE_MISMATCH';
                                    HrDescr := 'All audiences in the output tree must have the same content type.';
                                  end;
    LongInt($C00D1B9A)          : begin
                                    HrStr := 'NS_E_MISSING_SOURCE_INDEX';
                                    HrDescr := 'A source index is missing from the output stream configuration.';
                                  end;
    LongInt($C00D1B9B)          : begin
                                    HrStr := 'NS_E_NUM_LANGUAGE_MISMATCH';
                                    HrDescr := 'The same source index in different audiences should have the same number of languages.';
                                  end;
    LongInt($C00D1B9C)          : begin
                                    HrStr := 'NS_E_LANGUAGE_MISMATCH';
                                    HrDescr := 'The same source index in different audiences should have the same languages.';
                                  end;
    LongInt($C00D1B9D)          : begin
                                    HrStr := 'NS_E_VBRMODE_MISMATCH';
                                    HrDescr := 'The same source index in different audiences should use the same VBR encoding mode.';
                                  end;
    LongInt($C00D1B9E)          : begin
                                    HrStr := 'NS_E_INVALID_INPUT_AUDIENCE_INDEX';
                                    HrDescr := 'The bit rate index specified is not valid.';
                                  end;
    LongInt($C00D1B9F)          : begin
                                    HrStr := 'NS_E_INVALID_INPUT_LANGUAGE';
                                    HrDescr := 'The specified language is not valid.';
                                  end;
    LongInt($C00D1BA0)          : begin
                                    HrStr := 'NS_E_INVALID_INPUT_STREAM';
                                    HrDescr := 'The specified source type is not valid.';
                                  end;
    LongInt($C00D1BA1)          : begin
                                    HrStr := 'NS_E_EXPECT_MONO_WAV_INPUT';
                                    HrDescr := 'The source must be a mono channel .wav file.';
                                  end;
    LongInt($C00D1BA2)          : begin
                                    HrStr := 'NS_E_INPUT_WAVFORMAT_MISMATCH';
                                    HrDescr := 'All the source .wav files must have the same format.';
                                  end;
    LongInt($C00D1BA3)          : begin
                                    HrStr := 'NS_E_RECORDQ_DISK_FULL';
                                    HrDescr := 'The hard disk being used for temporary storage of content has reached the minimum allowed disk space.' +
                                               'Create more space on the hard disk and restart encoding.';
                                  end;
    LongInt($C00D1BA4)          : begin
                                    HrStr := 'NS_E_NO_PAL_INVERSE_TELECINE';
                                    HrDescr := 'It is not possible to apply the inverse telecine feature to PAL content.';
                                  end;
    LongInt($C00D1BA5)          : begin
                                    HrStr := 'NS_E_ACTIVE_SG_DEVICE_DISCONNECTED';
                                    HrDescr := 'A capture device in the current active source is no longer available.';
                                  end;
    LongInt($C00D1BA6)          : begin
                                    HrStr := 'NS_E_ACTIVE_SG_DEVICE_CONTROL_DISCONNECTED';
                                    HrDescr := 'A device used in the current active source for device control is no longer available.';
                                  end;
    LongInt($C00D1BA7)          : begin
                                    HrStr := 'NS_E_NO_FRAMES_SUBMITTED_TO_ANALYZER';
                                    HrDescr := 'No frames have been submitted to the analyzer for analysis.';
                                  end;
    LongInt($C00D1BA8)          : begin
                                    HrStr := 'NS_E_INPUT_DOESNOT_SUPPORT_SMPTE';
                                    HrDescr := 'The source video does not support time codes.';
                                  end;
    LongInt($C00D1BA9)          : begin
                                    HrStr := 'NS_E_NO_SMPTE_WITH_MULTIPLE_SOURCEGROUPS';
                                    HrDescr := 'It is not possible to generate a time code when there are multiple sources in a session.';
                                  end;
    LongInt($C00D1BAA)          : begin
                                    HrStr := 'NS_E_BAD_CONTENTEDL';
                                    HrDescr := 'The voice codec optimization definition file cannot be found or is corrupted.';
                                  end;
    LongInt($C00D1BAB)          : begin
                                    HrStr := 'NS_E_INTERLACEMODE_MISMATCH';
                                    HrDescr := 'The same source index in different audiences should have the same interlace mode.';
                                  end;
    LongInt($C00D1BAC)          : begin
                                    HrStr := 'NS_E_NONSQUAREPIXELMODE_MISMATCH';
                                    HrDescr := 'The same source index in different audiences should have the same nonsquare pixel mode.';
                                  end;
    LongInt($C00D1BAD)          : begin
                                    HrStr := 'NS_E_SMPTEMODE_MISMATCH';
                                    HrDescr := 'The same source index in different audiences should have the same time code mode.';
                                  end;
    LongInt($C00D1BAE)          : begin
                                    HrStr := 'NS_E_END_OF_TAPE';
                                    HrDescr := 'Either the end of the tape has been reached or there is no tape.' +
                                               'Check the device and tape.';
                                  end;
    LongInt($C00D1BAF)          : begin
                                    HrStr := 'NS_E_NO_MEDIA_IN_AUDIENCE';
                                    HrDescr := 'No audio or video input has been specified.';
                                  end;
    LongInt($C00D1BB0)          : begin
                                    HrStr := 'NS_E_NO_AUDIENCES';
                                    HrDescr := 'The profile must contain a bit rate.';
                                  end;
    LongInt($C00D1BB1)          : begin
                                    HrStr := 'NS_E_NO_AUDIO_COMPAT';
                                    HrDescr := 'You must specify at least one audio stream to be compatible with Windows Media Player 7.1.';
                                  end;
    LongInt($C00D1BB2)          : begin
                                    HrStr := 'NS_E_INVALID_VBR_COMPAT';
                                    HrDescr := 'Using a VBR encoding mode is not compatible with Windows Media Player 7.1.';
                                  end;
    LongInt($C00D1BB3)          : begin
                                    HrStr := 'NS_E_NO_PROFILE_NAME';
                                    HrDescr := 'You must specify a profile name.';
                                  end;
    LongInt($C00D1BB4)          : begin
                                    HrStr := 'NS_E_INVALID_VBR_WITH_UNCOMP';
                                    HrDescr := 'It is not possible to use a VBR encoding mode with uncompressed audio or video.';
                                  end;
    LongInt($C00D1BB5)          : begin
                                    HrStr := 'NS_E_MULTIPLE_VBR_AUDIENCES';
                                    HrDescr := 'It is not possible to use MBR encoding with VBR encoding.';
                                  end;
    LongInt($C00D1BB6)          : begin
                                    HrStr := 'NS_E_UNCOMP_COMP_COMBINATION';
                                    HrDescr := 'It is not possible to mix uncompressed and compressed content in a session.';
                                  end;
    LongInt($C00D1BB7)          : begin
                                    HrStr := 'NS_E_MULTIPLE_AUDIO_CODECS';
                                    HrDescr := 'All audiences must use the same audio codec.';
                                  end;
    LongInt($C00D1BB8)          : begin
                                    HrStr := 'NS_E_MULTIPLE_AUDIO_FORMATS';
                                    HrDescr := 'All audiences should use the same audio format to be compatible with Windows Media Player 7.1.';
                                  end;
    LongInt($C00D1BB9)          : begin
                                    HrStr := 'NS_E_AUDIO_BITRATE_STEPDOWN';
                                    HrDescr := 'The audio bit rate for an audience with a higher total bit rate must be greater than one with a lower total bit rate.';
                                  end;
    LongInt($C00D1BBA)          : begin
                                    HrStr := 'NS_E_INVALID_AUDIO_PEAKRATE';
                                    HrDescr := 'The audio peak bit rate setting is not valid.';
                                  end;
    LongInt($C00D1BBB)          : begin
                                    HrStr := 'NS_E_INVALID_AUDIO_PEAKRATE_2';
                                    HrDescr := 'The audio peak bit rate setting must be greater than the audio bit rate setting.';
                                  end;
    LongInt($C00D1BBC)          : begin
                                    HrStr := 'NS_E_INVALID_AUDIO_BUFFERMAX';
                                    HrDescr := 'The setting for the maximum buffer size for audio is not valid.';
                                  end;
    LongInt($C00D1BBD)          : begin
                                    HrStr := 'NS_E_MULTIPLE_VIDEO_CODECS';
                                    HrDescr := 'All audiences must use the same video codec.';
                                  end;
    LongInt($C00D1BBE)          : begin
                                    HrStr := 'NS_E_MULTIPLE_VIDEO_SIZES';
                                    HrDescr := 'All audiences should use the same video size to be compatible with Windows Media Player 7.1.';
                                  end;
    LongInt($C00D1BBF)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_BITRATE';
                                    HrDescr := 'The video bit rate setting is not valid.';
                                  end;
    LongInt($C00D1BC0)          : begin
                                    HrStr := 'NS_E_VIDEO_BITRATE_STEPDOWN';
                                    HrDescr := 'The video bit rate for an audience with a higher total bit rate must be greater than one with a lower total bit rate.';
                                  end;
    LongInt($C00D1BC1)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_PEAKRATE';
                                    HrDescr := 'The video peak bit rate setting is not valid.';
                                  end;
    LongInt($C00D1BC2)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_PEAKRATE_2';
                                    HrDescr := 'The video peak bit rate setting must be greater than the video bit rate setting.';
                                  end;
    LongInt($C00D1BC3)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_WIDTH';
                                    HrDescr := 'The video width setting is not valid.';
                                  end;
    LongInt($C00D1BC4)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_HEIGHT';
                                    HrDescr := 'The video height setting is not valid.';
                                  end;
    LongInt($C00D1BC5)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_FPS';
                                    HrDescr := 'The video frame rate setting is not valid.';
                                  end;
    LongInt($C00D1BC6)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_KEYFRAME';
                                    HrDescr := 'The video key frame setting is not valid.';
                                  end;
    LongInt($C00D1BC7)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_IQUALITY';
                                    HrDescr := 'The video image quality setting is not valid.';
                                  end;
    LongInt($C00D1BC8)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_CQUALITY';
                                    HrDescr := 'The video codec quality setting is not valid.';
                                  end;
    LongInt($C00D1BC9)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_BUFFER';
                                    HrDescr := 'The video buffer setting is not valid.';
                                  end;
    LongInt($C00D1BCA)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_BUFFERMAX';
                                    HrDescr := 'The setting for the maximum buffer size for video is not valid.';
                                  end;
    LongInt($C00D1BCB)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_BUFFERMAX_2';
                                    HrDescr := 'The value of the video maximum buffer size setting must be greater than the video buffer size setting.';
                                  end;
    LongInt($C00D1BCC)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_WIDTH_ALIGN';
                                    HrDescr := 'The alignment of the video width is not valid.';
                                  end;
    LongInt($C00D1BCD)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_HEIGHT_ALIGN';
                                    HrDescr := 'The alignment of the video height is not valid.';
                                  end;
    LongInt($C00D1BCE)          : begin
                                    HrStr := 'NS_E_MULTIPLE_SCRIPT_BITRATES';
                                    HrDescr := 'All bit rates must have the same script bit rate.';
                                  end;
    LongInt($C00D1BCF)          : begin
                                    HrStr := 'NS_E_INVALID_SCRIPT_BITRATE';
                                    HrDescr := 'The script bit rate specified is not valid.';
                                  end;
    LongInt($C00D1BD0)          : begin
                                    HrStr := 'NS_E_MULTIPLE_FILE_BITRATES';
                                    HrDescr := 'All bit rates must have the same file transfer bit rate.';
                                  end;
    LongInt($C00D1BD1)          : begin
                                    HrStr := 'NS_E_INVALID_FILE_BITRATE';
                                    HrDescr := 'The file transfer bit rate is not valid.';
                                  end;
    LongInt($C00D1BD2)          : begin
                                    HrStr := 'NS_E_SAME_AS_INPUT_COMBINATION';
                                    HrDescr := 'All audiences in a profile should either be same as input or have video width and height specified.';
                                  end;
    LongInt($C00D1BD3)          : begin
                                    HrStr := 'NS_E_SOURCE_CANNOT_LOOP';
                                    HrDescr := 'This source type does not support looping.';
                                  end;
    LongInt($C00D1BD4)          : begin
                                    HrStr := 'NS_E_INVALID_FOLDDOWN_COEFFICIENTS';
                                    HrDescr := 'The fold-down value needs to be between -144 and 0.';
                                  end;
    LongInt($C00D1BD5)          : begin
                                    HrStr := 'NS_E_DRMPROFILE_NOTFOUND';
                                    HrDescr := 'The specified DRM profile does not exist in the system.';
                                  end;
    LongInt($C00D1BD6)          : begin
                                    HrStr := 'NS_E_INVALID_TIMECODE';
                                    HrDescr := 'The specified time code is not valid.';
                                  end;
    LongInt($C00D1BD7)          : begin
                                    HrStr := 'NS_E_NO_AUDIO_TIMECOMPRESSION';
                                    HrDescr := 'It is not possible to apply time compression to a video-only session.';
                                  end;
    LongInt($C00D1BD8)          : begin
                                    HrStr := 'NS_E_NO_TWOPASS_TIMECOMPRESSION';
                                    HrDescr := 'It is not possible to apply time compression to a session that is using two-pass encoding.';
                                  end;
    LongInt($C00D1BD9)          : begin
                                    HrStr := 'NS_E_TIMECODE_REQUIRES_VIDEOSTREAM';
                                    HrDescr := 'It is not possible to generate a time code for an audio-only session.';
                                  end;
    LongInt($C00D1BDA)          : begin
                                    HrStr := 'NS_E_NO_MBR_WITH_TIMECODE';
                                    HrDescr := 'It is not possible to generate a time code when you are encoding content at multiple bit rates.';
                                  end;
    LongInt($C00D1BDB)          : begin
                                    HrStr := 'NS_E_INVALID_INTERLACEMODE';
                                    HrDescr := 'The video codec selected does not support maintaining interlacing in video.';
                                  end;
    LongInt($C00D1BDC)          : begin
                                    HrStr := 'NS_E_INVALID_INTERLACE_COMPAT';
                                    HrDescr := 'Maintaining interlacing in video is not compatible with Windows Media Player 7.1.';
                                  end;
    LongInt($C00D1BDD)          : begin
                                    HrStr := 'NS_E_INVALID_NONSQUAREPIXEL_COMPAT';
                                    HrDescr := 'Allowing nonsquare pixel output is not compatible with Windows Media Player 7.1.';
                                  end;
    LongInt($C00D1BDE)          : begin
                                    HrStr := 'NS_E_INVALID_SOURCE_WITH_DEVICE_CONTROL';
                                    HrDescr := 'Only capture devices can be used with device control.';
                                  end;
    LongInt($C00D1BDF)          : begin
                                    HrStr := 'NS_E_CANNOT_GENERATE_BROADCAST_INFO_FOR_QUALITYVBR';
                                    HrDescr := 'It is not possible to generate the stream format file if you are using quality-based VBR encoding for the audio or video stream.' +
                                               'Instead use the Windows Media file generated after encoding to create the announcement file.';
                                  end;
    LongInt($C00D1BE0)          : begin
                                    HrStr := 'NS_E_EXCEED_MAX_DRM_PROFILE_LIMIT';
                                    HrDescr := 'It is not possible to create a DRM profile because the maximum number of profiles has been reached.' +
                                               'You must delete some DRM profiles before creating new ones.';
                                  end;
    LongInt($C00D1BE1)          : begin
                                    HrStr := 'NS_E_DEVICECONTROL_UNSTABLE';
                                    HrDescr := 'The device is in an unstable state.' +
                                               'Check that the device is functioning properly and a tape is in place.';
                                  end;
    LongInt($C00D1BE2)          : begin
                                    HrStr := 'NS_E_INVALID_PIXEL_ASPECT_RATIO';
                                    HrDescr := 'The pixel aspect ratio value must be between 1 and 255.';
                                  end;
    LongInt($C00D1BE3)          : begin
                                    HrStr := 'NS_E_AUDIENCE__LANGUAGE_CONTENTTYPE_MISMATCH';
                                    HrDescr := 'All streams with different languages in the same audience must have same properties.';
                                  end;
    LongInt($C00D1BE4)          : begin
                                    HrStr := 'NS_E_INVALID_PROFILE_CONTENTTYPE';
                                    HrDescr := 'The profile must contain at least one audio or video stream.';
                                  end;
    LongInt($C00D1BE5)          : begin
                                    HrStr := 'NS_E_TRANSFORM_PLUGIN_NOT_FOUND';
                                    HrDescr := 'The transform plug-in could not be found.';
                                  end;
    LongInt($C00D1BE6)          : begin
                                    HrStr := 'NS_E_TRANSFORM_PLUGIN_INVALID';
                                    HrDescr := 'The transform plug-in is not valid.' +
                                               'It might be damaged or you might not have the required permissions to access the plug-in.';
                                  end;
    LongInt($C00D1BE7)          : begin
                                    HrStr := 'NS_E_EDL_REQUIRED_FOR_DEVICE_MULTIPASS';
                                    HrDescr := 'To use two-pass encoding, you must enable device control and setup an edit decision list (EDL) that has at least one entry.';
                                  end;
    LongInt($C00D1BE8)          : begin
                                    HrStr := 'NS_E_INVALID_VIDEO_WIDTH_FOR_INTERLACED_ENCODING';
                                    HrDescr := 'When you choose to maintain the interlacing in your video, the output video size must be a multiple of 4.';
                                  end;
    LongInt($C00D1BE9)          : begin
                                    HrStr := 'NS_E_MARKIN_UNSUPPORTED';
                                    HrDescr := 'Markin/Markout is unsupported with this source type.';
                                  end;
    LongInt($C00D2711)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_APPLICATION';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact product support for this application.';
                                  end;
    LongInt($C00D2712)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_STORE_ERROR';
                                    HrDescr := 'License storage is not working.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2713)          : begin
                                    HrStr := 'NS_E_DRM_SECURE_STORE_ERROR';
                                    HrDescr := 'Secure storage is not working.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2714)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_STORE_SAVE_ERROR';
                                    HrDescr := 'License acquisition did not work.' +
                                               'Acquire a new license or contact the content provider for further assistance.';
                                  end;
    LongInt($C00D2715)          : begin
                                    HrStr := 'NS_E_DRM_SECURE_STORE_UNLOCK_ERROR';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2716)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_CONTENT';
                                    HrDescr := 'The media file is corrupted.' +
                                               'Contact the content provider to get a new file.';
                                  end;
    LongInt($C00D2717)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_OPEN_LICENSE';
                                    HrDescr := 'The license is corrupted.' +
                                               'Acquire a new license.';
                                  end;
    LongInt($C00D2718)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_LICENSE';
                                    HrDescr := 'The license is corrupted or invalid.' +
                                               'Acquire a new license';
                                  end;
    LongInt($C00D2719)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_MACHINE';
                                    HrDescr := 'Licenses cannot be copied from one computer to another.' +
                                               'Use License Management to transfer licenses, or get a new license for the media file.';
                                  end;
    LongInt($C00D271B)          : begin
                                    HrStr := 'NS_E_DRM_ENUM_LICENSE_FAILED';
                                    HrDescr := 'License storage is not working.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D271C)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_LICENSE_REQUEST';
                                    HrDescr := 'The media file is corrupted.' +
                                               'Contact the content provider to get a new file.';
                                  end;
    LongInt($C00D271D)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_INITIALIZE';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D271E)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_ACQUIRE_LICENSE';
                                    HrDescr := 'The license could not be acquired.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D271F)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_LICENSE_ACQUIRED';
                                    HrDescr := 'License acquisition did not work.' +
                                               'Acquire a new license or contact the content provider for further assistance.';
                                  end;
    LongInt($C00D2720)          : begin
                                    HrStr := 'NS_E_DRM_NO_RIGHTS';
                                    HrDescr := 'The requested operation cannot be performed on this file.';
                                  end;
    LongInt($C00D2721)          : begin
                                    HrStr := 'NS_E_DRM_KEY_ERROR';
                                    HrDescr := 'The requested action cannot be performed because a problem occurred with the Windows Media Digital Rights Management (DRM) components on your computer.';
                                  end;
    LongInt($C00D2722)          : begin
                                    HrStr := 'NS_E_DRM_ENCRYPT_ERROR';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2723)          : begin
                                    HrStr := 'NS_E_DRM_DECRYPT_ERROR';
                                    HrDescr := 'The media file is corrupted.' +
                                               'Contact the content provider to get a new file.';
                                  end;
    LongInt($C00D2725)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_INVALID_XML';
                                    HrDescr := 'The license is corrupted.' +
                                               'Acquire a new license.';
                                  end;
    LongInt($C00D2728)          : begin
                                    HrStr := 'NS_E_DRM_NEEDS_INDIVIDUALIZATION';
                                    HrDescr := 'A security upgrade is required to perform the operation on this media file.';
                                  end;
    LongInt($C00D2729)          : begin
                                    HrStr := 'NS_E_DRM_ALREADY_INDIVIDUALIZED';
                                    HrDescr := 'You already have the latest security components.' +
                                               'No upgrade is necessary at this time.';
                                  end;
    LongInt($C00D272A)          : begin
                                    HrStr := 'NS_E_DRM_ACTION_NOT_QUERIED';
                                    HrDescr := 'The application cannot perform this action.' +
                                               'Contact product support for this application.';
                                  end;
    LongInt($C00D272B)          : begin
                                    HrStr := 'NS_E_DRM_ACQUIRING_LICENSE';
                                    HrDescr := 'You cannot begin a new license acquisition process until the current one has been completed.';
                                  end;
    LongInt($C00D272C)          : begin
                                    HrStr := 'NS_E_DRM_INDIVIDUALIZING';
                                    HrDescr := 'You cannot begin a new security upgrade until the current one has been completed.';
                                  end;
    LongInt($C00D272D)          : begin
                                    HrStr := 'NS_E_BACKUP_RESTORE_FAILURE';
                                    HrDescr := 'Failure in Backup-Restore.';
                                  end;
    LongInt($C00D272E)          : begin
                                    HrStr := 'NS_E_BACKUP_RESTORE_BAD_REQUEST_ID';
                                    HrDescr := 'Bad Request ID in Backup-Restore.';
                                  end;
    LongInt($C00D272F)          : begin
                                    HrStr := 'NS_E_DRM_PARAMETERS_MISMATCHED';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2730)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_LICENSE_OBJECT';
                                    HrDescr := 'A license cannot be created for this media file.' +
                                               'Reinstall the application.';
                                  end;
    LongInt($C00D2731)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_INDI_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2732)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_ENCRYPT_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2733)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_DECRYPT_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2734)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_PROPERTIES_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2735)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_BACKUP_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2736)          : begin
                                    HrStr := 'NS_E_DRM_INDIVIDUALIZE_ERROR';
                                    HrDescr := 'The security upgrade failed.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D2737)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_OPEN_ERROR';
                                    HrDescr := 'License storage is not working.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2738)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_CLOSE_ERROR';
                                    HrDescr := 'License storage is not working.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2739)          : begin
                                    HrStr := 'NS_E_DRM_GET_LICENSE_ERROR';
                                    HrDescr := 'License storage is not working.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D273A)          : begin
                                    HrStr := 'NS_E_DRM_QUERY_ERROR';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D273B)          : begin
                                    HrStr := 'NS_E_DRM_REPORT_ERROR';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact product support for this application.';
                                  end;
    LongInt($C00D273C)          : begin
                                    HrStr := 'NS_E_DRM_GET_LICENSESTRING_ERROR';
                                    HrDescr := 'License storage is not working.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D273D)          : begin
                                    HrStr := 'NS_E_DRM_GET_CONTENTSTRING_ERROR';
                                    HrDescr := 'The media file is corrupted.' +
                                               'Contact the content provider to get a new file.';
                                  end;
    LongInt($C00D273E)          : begin
                                    HrStr := 'NS_E_DRM_MONITOR_ERROR';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D273F)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_SET_PARAMETER';
                                    HrDescr := 'The application has made an invalid call to the Digital Rights Management component.' +
                                               'Contact product support for this application.';
                                  end;
    LongInt($C00D2740)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_APPDATA';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2741)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_APPDATA_VERSION';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact product support for this application.';
                                  end;
    LongInt($C00D2742)          : begin
                                    HrStr := 'NS_E_DRM_BACKUP_EXISTS';
                                    HrDescr := 'Licenses are already backed up in this location.';
                                  end;
    LongInt($C00D2743)          : begin
                                    HrStr := 'NS_E_DRM_BACKUP_CORRUPT';
                                    HrDescr := 'One or more backed-up licenses are missing or corrupt.';
                                  end;
    LongInt($C00D2744)          : begin
                                    HrStr := 'NS_E_DRM_BACKUPRESTORE_BUSY';
                                    HrDescr := 'You cannot begin a new backup process until the current process has been completed.';
                                  end;
    LongInt($C00D2745)          : begin
                                    HrStr := 'NS_E_BACKUP_RESTORE_BAD_DATA';
                                    HrDescr := 'Bad Data sent to Backup-Restore.';
                                  end;
    LongInt($C00D2748)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_UNUSABLE';
                                    HrDescr := 'The license is invalid.' +
                                               'Contact the content provider for further assistance.';
                                  end;
    LongInt($C00D2749)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_PROPERTY';
                                    HrDescr := 'A required property was not set by the application.' +
                                               'Contact product support for this application.';
                                  end;
    LongInt($C00D274A)          : begin
                                    HrStr := 'NS_E_DRM_SECURE_STORE_NOT_FOUND';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component of this application.' +
                                               'Try to acquire a license again.';
                                  end;
    LongInt($C00D274B)          : begin
                                    HrStr := 'NS_E_DRM_CACHED_CONTENT_ERROR';
                                    HrDescr := 'A license cannot be found for this media file.' +
                                               'Use License Management to transfer a license for this file from the original computer, or acquire a new license.';
                                  end;
    LongInt($C00D274C)          : begin
                                    HrStr := 'NS_E_DRM_INDIVIDUALIZATION_INCOMPLETE';
                                    HrDescr := 'A problem occurred during the security upgrade.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D274D)          : begin
                                    HrStr := 'NS_E_DRM_DRIVER_AUTH_FAILURE';
                                    HrDescr := 'Certified driver components are required to play this media file.' +
                                               'Contact Windows Update to see whether updated drivers are available for your hardware.';
                                  end;
    LongInt($C00D274E)          : begin
                                    HrStr := 'NS_E_DRM_NEED_UPGRADE_MSSAP';
                                    HrDescr := 'One or more of the Secure Audio Path components were not found or an entry point in those components was not found.';
                                  end;
    LongInt($C00D274F)          : begin
                                    HrStr := 'NS_E_DRM_REOPEN_CONTENT';
                                    HrDescr := 'Status message: Reopen the file.';
                                  end;
    LongInt($C00D2750)          : begin
                                    HrStr := 'NS_E_DRM_DRIVER_DIGIOUT_FAILURE';
                                    HrDescr := 'Certain driver functionality is required to play this media file.' +
                                               'Contact Windows Update to see whether updated drivers are available for your hardware.';
                                  end;
    LongInt($C00D2751)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_SECURESTORE_PASSWORD';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2752)          : begin
                                    HrStr := 'NS_E_DRM_APPCERT_REVOKED';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2753)          : begin
                                    HrStr := 'NS_E_DRM_RESTORE_FRAUD';
                                    HrDescr := 'You cannot restore your license(s).';
                                  end;
    LongInt($C00D2754)          : begin
                                    HrStr := 'NS_E_DRM_HARDWARE_INCONSISTENT';
                                    HrDescr := 'The licenses for your media files are corrupted.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2755)          : begin
                                    HrStr := 'NS_E_DRM_SDMI_TRIGGER';
                                    HrDescr := 'To transfer this media file, you must upgrade the application.';
                                  end;
    LongInt($C00D2756)          : begin
                                    HrStr := 'NS_E_DRM_SDMI_NOMORECOPIES';
                                    HrDescr := 'You cannot make any more copies of this media file.';
                                  end;
    LongInt($C00D2757)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_HEADER_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2758)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_KEYS_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2759)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_NOTACQUIRED';
                                    HrDescr := 'Unable to obtain license.';
                                  end;
    LongInt($C00D275A)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_CODING_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D275B)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_STATE_DATA_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D275C)          : begin
                                    HrStr := 'NS_E_DRM_BUFFER_TOO_SMALL';
                                    HrDescr := 'The buffer supplied is not sufficient.';
                                  end;
    LongInt($C00D275D)          : begin
                                    HrStr := 'NS_E_DRM_UNSUPPORTED_PROPERTY';
                                    HrDescr := 'The property requested is not supported.';
                                  end;
    LongInt($C00D275E)          : begin
                                    HrStr := 'NS_E_DRM_ERROR_BAD_NET_RESP';
                                    HrDescr := 'The specified server cannot perform the requested operation.';
                                  end;
    LongInt($C00D275F)          : begin
                                    HrStr := 'NS_E_DRM_STORE_NOTALLSTORED';
                                    HrDescr := 'Some of the licenses could not be stored.';
                                  end;
    LongInt($C00D2760)          : begin
                                    HrStr := 'NS_E_DRM_SECURITY_COMPONENT_SIGNATURE_INVALID';
                                    HrDescr := 'The Digital Rights Management security upgrade component could not be validated.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2761)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_DATA';
                                    HrDescr := 'Invalid or corrupt data was encountered.';
                                  end;
    LongInt($C00D2762)          : begin
                                    HrStr := 'NS_E_DRM_POLICY_DISABLE_ONLINE';
                                    HrDescr := 'The Windows Media Digital Rights Management system cannot perform the requested action because your computer or network administrator has enabled the group policy Prevent Windows Media DRM Internet Access.' +
                                               'For assistance, contact your administrator.';
                                  end;
    LongInt($C00D2763)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_AUTHENTICATION_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2764)          : begin
                                    HrStr := 'NS_E_DRM_NOT_CONFIGURED';
                                    HrDescr := 'Not all of the necessary properties for DRM have been set.';
                                  end;
    LongInt($C00D2765)          : begin
                                    HrStr := 'NS_E_DRM_DEVICE_ACTIVATION_CANCELED';
                                    HrDescr := 'The portable device does not have the security required to copy protected files to it.' +
                                               'To obtain the additional security, try to copy the file to your portable device again.' +
                                               'When a message appears, click OK.';
                                  end;
    LongInt($C00D2766)          : begin
                                    HrStr := 'NS_E_BACKUP_RESTORE_TOO_MANY_RESETS';
                                    HrDescr := 'Too many resets in Backup-Restore.';
                                  end;
    LongInt($C00D2767)          : begin
                                    HrStr := 'NS_E_DRM_DEBUGGING_NOT_ALLOWED';
                                    HrDescr := 'Running this process under a debugger while using DRM content is not allowed.';
                                  end;
    LongInt($C00D2768)          : begin
                                    HrStr := 'NS_E_DRM_OPERATION_CANCELED';
                                    HrDescr := 'The user canceled the DRM operation.';
                                  end;
    LongInt($C00D2769)          : begin
                                    HrStr := 'NS_E_DRM_RESTRICTIONS_NOT_RETRIEVED';
                                    HrDescr := 'The license you are using has assocaited output restrictions.' +
                                               'This license is unusable until these restrictions are queried.';
                                  end;
    LongInt($C00D276A)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_PLAYLIST_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D276B)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_PLAYLIST_BURN_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D276C)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_DEVICE_REGISTRATION_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D276D)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_METERING_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2770)          : begin
                                    HrStr := 'NS_E_DRM_TRACK_EXCEEDED_PLAYLIST_RESTICTION';
                                    HrDescr := 'The specified track has exceeded it''s specified playlist burn limit in this playlist.';
                                  end;
    LongInt($C00D2771)          : begin
                                    HrStr := 'NS_E_DRM_TRACK_EXCEEDED_TRACKBURN_RESTRICTION';
                                    HrDescr := 'The specified track has exceeded it''s track burn limit.';
                                  end;
    LongInt($C00D2772)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_GET_DEVICE_CERT';
                                    HrDescr := 'A problem has occurred in obtaining the device''s certificate.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2773)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_GET_SECURE_CLOCK';
                                    HrDescr := 'A problem has occurred in obtaining the device''s secure clock.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2774)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_SET_SECURE_CLOCK';
                                    HrDescr := 'A problem has occurred in setting the device''s secure clock.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2775)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_GET_SECURE_CLOCK_FROM_SERVER';
                                    HrDescr := 'A problem has occurred in obtaining the secure clock from server.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2776)          : begin
                                    HrStr := 'NS_E_DRM_POLICY_METERING_DISABLED';
                                    HrDescr := 'This content requires the metering policy to be enabled.';
                                  end;
    LongInt($C00D2777)          : begin
                                    HrStr := 'NS_E_DRM_TRANSFER_CHAINED_LICENSES_UNSUPPORTED';
                                    HrDescr := 'Transfer of chained licenses unsupported.';
                                  end;
    LongInt($C00D2778)          : begin
                                    HrStr := 'NS_E_DRM_SDK_VERSIONMISMATCH';
                                    HrDescr := 'The Digital Rights Management component is not installed properly.' +
                                               'Reinstall the Player.';
                                  end;
    LongInt($C00D2779)          : begin
                                    HrStr := 'NS_E_DRM_LIC_NEEDS_DEVICE_CLOCK_SET';
                                    HrDescr := 'The file could not be transferred because the device clock is not set.';
                                  end;
    LongInt($C00D277A)          : begin
                                    HrStr := 'NS_E_LICENSE_HEADER_MISSING_URL';
                                    HrDescr := 'The content header is missing an acquisition URL.';
                                  end;
    LongInt($C00D277B)          : begin
                                    HrStr := 'NS_E_DEVICE_NOT_WMDRM_DEVICE';
                                    HrDescr := 'The current attached device does not support WMDRM.';
                                  end;
    LongInt($C00D277C)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_APPCERT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D277D)          : begin
                                    HrStr := 'NS_E_DRM_PROTOCOL_FORCEFUL_TERMINATION_ON_PETITION';
                                    HrDescr := 'The client application has been forcefully terminated during a DRM petition.';
                                  end;
    LongInt($C00D277E)          : begin
                                    HrStr := 'NS_E_DRM_PROTOCOL_FORCEFUL_TERMINATION_ON_CHALLENGE';
                                    HrDescr := 'The client application has been forcefully terminated during a DRM challenge.';
                                  end;
    LongInt($C00D277F)          : begin
                                    HrStr := 'NS_E_DRM_CHECKPOINT_FAILED';
                                    HrDescr := 'Secure storage protection error.' +
                                               'Restore your licenses from a previous backup and try again.';
                                  end;
    LongInt($C00D2780)          : begin
                                    HrStr := 'NS_E_DRM_BB_UNABLE_TO_INITIALIZE';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management root of trust.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2781)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_LOAD_HARDWARE_ID';
                                    HrDescr := 'A problem has occurred in retrieving the Digital Rights Management machine identification.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2782)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_OPEN_DATA_STORE';
                                    HrDescr := 'A problem has occurred in opening the Digital Rights Management data storage file.' +
                                               'Contact Microsoft product.';
                                  end;
    LongInt($C00D2783)          : begin
                                    HrStr := 'NS_E_DRM_DATASTORE_CORRUPT';
                                    HrDescr := 'The Digital Rights Management data storage is not functioning properly.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2784)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_INMEMORYSTORE_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2785)          : begin
                                    HrStr := 'NS_E_DRM_STUBLIB_REQUIRED';
                                    HrDescr := 'A secured library is required to access the requested functionality.';
                                  end;
    LongInt($C00D2786)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_CERTIFICATE_OBJECT';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2787)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_TARGET_NOT_ONLINE';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component during license migration.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2788)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_MIGRATION_IMAGE';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component during license migration.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2789)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_TARGET_STATES_CORRUPTED';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component during license migration.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D278A)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_IMPORTER_NOT_AVAILABLE';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component during license migration.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D278B)          : begin
                                    HrStr := 'NS_DRM_E_MIGRATION_UPGRADE_WITH_DIFF_SID';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component during license migration.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D278C)          : begin
                                    HrStr := 'NS_DRM_E_MIGRATION_SOURCE_MACHINE_IN_USE';
                                    HrDescr := 'The Digital Rights Management component is in use during license migration.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D278D)          : begin
                                    HrStr := 'NS_DRM_E_MIGRATION_TARGET_MACHINE_LESS_THAN_LH';
                                    HrDescr := 'Licenses are being migrated to a machine running XP or downlevel OS.' +
                                               'This operation can only be performed on Windows Vista or a later OS.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D278E)          : begin
                                    HrStr := 'NS_DRM_E_MIGRATION_IMAGE_ALREADY_EXISTS';
                                    HrDescr := 'Migration Image already exists.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D278F)          : begin
                                    HrStr := 'NS_E_DRM_HARDWAREID_MISMATCH';
                                    HrDescr := 'The requested action cannot be performed because a hardware configuration change has been detected by the Windows Media Digital Rights Management (DRM) components on your computer.';
                                  end;
    LongInt($C00D2790)          : begin
                                    HrStr := 'NS_E_INVALID_DRMV2CLT_STUBLIB';
                                    HrDescr := 'The wrong stublib has been linked to an application or DLL using drmv2clt.dll.';
                                  end;
    LongInt($C00D2791)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_INVALID_LEGACYV2_DATA';
                                    HrDescr := 'The legacy V2 data being imported is invalid.';
                                  end;
    LongInt($C00D2792)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_LICENSE_ALREADY_EXISTS';
                                    HrDescr := 'The license being imported already exists.';
                                  end;
    LongInt($C00D2793)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_INVALID_LEGACYV2_SST_PASSWORD';
                                    HrDescr := 'The password of the Legacy V2 SST entry being imported is incorrect.';
                                  end;
    LongInt($C00D2794)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_NOT_SUPPORTED';
                                    HrDescr := 'Migration is not supported by the plugin.';
                                  end;
    LongInt($C00D2795)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_CREATE_MIGRATION_IMPORTER_OBJECT';
                                    HrDescr := 'A migration importer cannot be created for this media file.' +
                                               'Reinstall the application.';
                                  end;
    LongInt($C00D2796)          : begin
                                    HrStr := 'NS_E_DRM_CHECKPOINT_MISMATCH';
                                    HrDescr := 'The requested action cannot be performed because a problem occurred with the Windows Media Digital Rights Management (DRM) components on your computer.';
                                  end;
    LongInt($C00D2797)          : begin
                                    HrStr := 'NS_E_DRM_CHECKPOINT_CORRUPT';
                                    HrDescr := 'The requested action cannot be performed because a problem occurred with the Windows Media Digital Rights Management (DRM) components on your computer.';
                                  end;
    LongInt($C00D2798)          : begin
                                    HrStr := 'NS_E_REG_FLUSH_FAILURE';
                                    HrDescr := 'The requested action cannot be performed because a problem occurred with the Windows Media Digital Rights Management (DRM) components on your computer.';
                                  end;
    LongInt($C00D2799)          : begin
                                    HrStr := 'NS_E_HDS_KEY_MISMATCH';
                                    HrDescr := 'The requested action cannot be performed because a problem occurred with the Windows Media Digital Rights Management (DRM) components on your computer.';
                                  end;
    LongInt($C00D279A)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_OPERATION_CANCELLED';
                                    HrDescr := 'Migration was canceled by the user.';
                                  end;
    LongInt($C00D279B)          : begin
                                    HrStr := 'NS_E_DRM_MIGRATION_OBJECT_IN_USE';
                                    HrDescr := 'Migration object is already in use and cannot be called until the current operation completes.';
                                  end;
    LongInt($C00D279C)          : begin
                                    HrStr := 'NS_E_DRM_MALFORMED_CONTENT_HEADER';
                                    HrDescr := 'The content header does not comply with DRM requirements and cannot be used.';
                                  end;
    LongInt($C00D27D8)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_EXPIRED';
                                    HrDescr := 'The license for this file has expired and is no longer valid.' +
                                               'Contact your content provider for further assistance.';
                                  end;
    LongInt($C00D27D9)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_NOTENABLED';
                                    HrDescr := 'The license for this file is not valid yet, but will be at a future date.';
                                  end;
    LongInt($C00D27DA)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_APPSECLOW';
                                    HrDescr := 'The license for this file requires a higher level of security than the player you are currently using has.' +
                                               'Try using a different player or download a newer version of your current player.';
                                  end;
    LongInt($C00D27DB)          : begin
                                    HrStr := 'NS_E_DRM_STORE_NEEDINDI';
                                    HrDescr := 'The license cannot be stored as it requires security upgrade of Digital Rights Management component.';
                                  end;
    LongInt($C00D27DC)          : begin
                                    HrStr := 'NS_E_DRM_STORE_NOTALLOWED';
                                    HrDescr := 'Your machine does not meet the requirements for storing the license.';
                                  end;
    LongInt($C00D27DD)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_APP_NOTALLOWED';
                                    HrDescr := 'The license for this file requires an upgraded version of your player or a different player.';
                                  end;
    LongInt($C00D27DF)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_CERT_EXPIRED';
                                    HrDescr := 'The license server''s certificate expired.' +
                                               'Make sure your system clock is set correctly.' +
                                               'Contact your content provider for further assistance.';
                                  end;
    LongInt($C00D27E0)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_SECLOW';
                                    HrDescr := 'The license for this file requires a higher level of security than the player you are currently using has.' +
                                               'Try using a different player or download a newer version of your current player.';
                                  end;
    LongInt($C00D27E1)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_CONTENT_REVOKED';
                                    HrDescr := 'The content owner for the license you just acquired is no longer supporting their content.' +
                                               'Contact the content owner for a newer version of the content.';
                                  end;
    LongInt($C00D27E2)          : begin
                                    HrStr := 'NS_E_DRM_DEVICE_NOT_REGISTERED';
                                    HrDescr := 'The content owner for the license you just acquired requires your device to register to the current machine.';
                                  end;
    LongInt($C00D280A)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_NOSAP';
                                    HrDescr := 'The license for this file requires a feature that is not supported in your current player or operating system.' +
                                               'You can try with newer version of your current player or contact your content provider for further assistance.';
                                  end;
    LongInt($C00D280B)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_NOSVP';
                                    HrDescr := 'The license for this file requires a feature that is not supported in your current player or operating system.' +
                                               'You can try with newer version of your current player or contact your content provider for further assistance.';
                                  end;
    LongInt($C00D280C)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_NOWDM';
                                    HrDescr := 'The license for this file requires Windows Driver Model (WDM) audio drivers.' +
                                               'Contact your sound card manufacturer for further assistance.';
                                  end;
    LongInt($C00D280D)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_NOTRUSTEDCODEC';
                                    HrDescr := 'The license for this file requires a higher level of security than the player you are currently using has.' +
                                               'Try using a different player or download a newer version of your current player.';
                                  end;
    LongInt($C00D280E)          : begin
                                    HrStr := 'NS_E_DRM_SOURCEID_NOT_SUPPORTED';
                                    HrDescr := 'The license for this file is not supported by your current player.' +
                                               'You can try with newer version of your current player or contact your content provider for further assistance.';
                                  end;
    LongInt($C00D283D)          : begin
                                    HrStr := 'NS_E_DRM_NEEDS_UPGRADE_TEMPFILE';
                                    HrDescr := 'An updated version of your media player is required to play the selected content.';
                                  end;
    LongInt($C00D283E)          : begin
                                    HrStr := 'NS_E_DRM_NEED_UPGRADE_PD';
                                    HrDescr := 'A new version of the Digital Rights Management component is required.' +
                                               'Contact product support for this application to get the latest version.';
                                  end;
    LongInt($C00D283F)          : begin
                                    HrStr := 'NS_E_DRM_SIGNATURE_FAILURE';
                                    HrDescr := 'Failed to either create or verify the content header.';
                                  end;
    LongInt($C00D2840)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_SERVER_INFO_MISSING';
                                    HrDescr := 'Could not read the necessary information from the system registry.';
                                  end;
    LongInt($C00D2841)          : begin
                                    HrStr := 'NS_E_DRM_BUSY';
                                    HrDescr := 'The DRM subsystem is currently locked by another application or user.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D2842)          : begin
                                    HrStr := 'NS_E_DRM_PD_TOO_MANY_DEVICES';
                                    HrDescr := 'There are too many target devices registered on the portable media.';
                                  end;
    LongInt($C00D2843)          : begin
                                    HrStr := 'NS_E_DRM_INDIV_FRAUD';
                                    HrDescr := 'The security upgrade cannot be completed because the allowed number of daily upgrades has been exceeded.' +
                                               'Try again tomorrow.';
                                  end;
    LongInt($C00D2844)          : begin
                                    HrStr := 'NS_E_DRM_INDIV_NO_CABS';
                                    HrDescr := 'The security upgrade cannot be completed because the server is unable to perform the operation.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D2845)          : begin
                                    HrStr := 'NS_E_DRM_INDIV_SERVICE_UNAVAILABLE';
                                    HrDescr := 'The security upgrade cannot be performed because the server is not available.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D2846)          : begin
                                    HrStr := 'NS_E_DRM_RESTORE_SERVICE_UNAVAILABLE';
                                    HrDescr := 'Windows Media Player cannot restore your licenses because the server is not available.' +
                                               'Try again later.';
                                  end;
    LongInt($C00D2847)          : begin
                                    HrStr := 'NS_E_DRM_CLIENT_CODE_EXPIRED';
                                    HrDescr := 'Windows Media Player cannot play the protected file.' +
                                               'Verify that your computer''s date is set correctly.' +
                                               'If it is correct, on the Help menu, click Check for Player Updates to install the latest version of the Player.';
                                  end;
    LongInt($C00D2848)          : begin
                                    HrStr := 'NS_E_DRM_NO_UPLINK_LICENSE';
                                    HrDescr := 'The chained license cannot be created because the referenced uplink license does not exist.';
                                  end;
    LongInt($C00D2849)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_KID';
                                    HrDescr := 'The specified KID is invalid.';
                                  end;
    LongInt($C00D284A)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_INITIALIZATION_ERROR';
                                    HrDescr := 'License initialization did not work.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D284C)          : begin
                                    HrStr := 'NS_E_DRM_CHAIN_TOO_LONG';
                                    HrDescr := 'The uplink license of a chained license cannot itself be a chained license.';
                                  end;
    LongInt($C00D284D)          : begin
                                    HrStr := 'NS_E_DRM_UNSUPPORTED_ALGORITHM';
                                    HrDescr := 'The specified encryption algorithm is unsupported.';
                                  end;
    LongInt($C00D284E)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_DELETION_ERROR';
                                    HrDescr := 'License deletion did not work.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D28A0)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_CERTIFICATE';
                                    HrDescr := 'The client''s certificate is corrupted or the signature cannot be verified.';
                                  end;
    LongInt($C00D28A1)          : begin
                                    HrStr := 'NS_E_DRM_CERTIFICATE_REVOKED';
                                    HrDescr := 'The client''s certificate has been revoked.';
                                  end;
    LongInt($C00D28A2)          : begin
                                    HrStr := 'NS_E_DRM_LICENSE_UNAVAILABLE';
                                    HrDescr := 'There is no license available for the requested action.';
                                  end;
    LongInt($C00D28A3)          : begin
                                    HrStr := 'NS_E_DRM_DEVICE_LIMIT_REACHED';
                                    HrDescr := 'The maximum number of devices in use has been reached.' +
                                               'Unable to open additional devices.';
                                  end;
    LongInt($C00D28A4)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_VERIFY_PROXIMITY';
                                    HrDescr := 'The proximity detection procedure could not confirm that the receiver is near the transmitter in the network.';
                                  end;
    LongInt($C00D28A5)          : begin
                                    HrStr := 'NS_E_DRM_MUST_REGISTER';
                                    HrDescr := 'The client must be registered before executing the intended operation.';
                                  end;
    LongInt($C00D28A6)          : begin
                                    HrStr := 'NS_E_DRM_MUST_APPROVE';
                                    HrDescr := 'The client must be approved before executing the intended operation.';
                                  end;
    LongInt($C00D28A7)          : begin
                                    HrStr := 'NS_E_DRM_MUST_REVALIDATE';
                                    HrDescr := 'The client must be revalidated before executing the intended operation.';
                                  end;
    LongInt($C00D28A8)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_PROXIMITY_RESPONSE';
                                    HrDescr := 'The response to the proximity detection challenge is invalid.';
                                  end;
    LongInt($C00D28A9)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_SESSION';
                                    HrDescr := 'The requested session is invalid.';
                                  end;
    LongInt($C00D28AA)          : begin
                                    HrStr := 'NS_E_DRM_DEVICE_NOT_OPEN';
                                    HrDescr := 'The device must be opened before it can be used to receive content.';
                                  end;
    LongInt($C00D28AB)          : begin
                                    HrStr := 'NS_E_DRM_DEVICE_ALREADY_REGISTERED';
                                    HrDescr := 'Device registration failed because the device is already registered.';
                                  end;
    LongInt($C00D28AC)          : begin
                                    HrStr := 'NS_E_DRM_UNSUPPORTED_PROTOCOL_VERSION';
                                    HrDescr := 'Unsupported WMDRM-ND protocol version.';
                                  end;
    LongInt($C00D28AD)          : begin
                                    HrStr := 'NS_E_DRM_UNSUPPORTED_ACTION';
                                    HrDescr := 'The requested action is not supported.';
                                  end;
    LongInt($C00D28AE)          : begin
                                    HrStr := 'NS_E_DRM_CERTIFICATE_SECURITY_LEVEL_INADEQUATE';
                                    HrDescr := 'The certificate does not have an adequate security level for the requested action.';
                                  end;
    LongInt($C00D28AF)          : begin
                                    HrStr := 'NS_E_DRM_UNABLE_TO_OPEN_PORT';
                                    HrDescr := 'Unable to open the specified port for receiving Proximity messages.';
                                  end;
    LongInt($C00D28B0)          : begin
                                    HrStr := 'NS_E_DRM_BAD_REQUEST';
                                    HrDescr := 'The message format is invalid.';
                                  end;
    LongInt($C00D28B1)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_CRL';
                                    HrDescr := 'The Certificate Revocation List is invalid or corrupted.';
                                  end;
    LongInt($C00D28B2)          : begin
                                    HrStr := 'NS_E_DRM_ATTRIBUTE_TOO_LONG';
                                    HrDescr := 'The length of the attribute name or value is too long.';
                                  end;
    LongInt($C00D28B3)          : begin
                                    HrStr := 'NS_E_DRM_EXPIRED_LICENSEBLOB';
                                    HrDescr := 'The license blob passed in the cardea request is expired.';
                                  end;
    LongInt($C00D28B4)          : begin
                                    HrStr := 'NS_E_DRM_INVALID_LICENSEBLOB';
                                    HrDescr := 'The license blob passed in the cardea request is invalid.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D28B5)          : begin
                                    HrStr := 'NS_E_DRM_INCLUSION_LIST_REQUIRED';
                                    HrDescr := 'The requested operation cannot be performed because the license does not contain an inclusion list.';
                                  end;
    LongInt($C00D28B6)          : begin
                                    HrStr := 'NS_E_DRM_DRMV2CLT_REVOKED';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D28B7)          : begin
                                    HrStr := 'NS_E_DRM_RIV_TOO_SMALL';
                                    HrDescr := 'A problem has occurred in the Digital Rights Management component.' +
                                               'Contact Microsoft product support.';
                                  end;
    LongInt($C00D2904)          : begin
                                    HrStr := 'NS_E_OUTPUT_PROTECTION_LEVEL_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player does not support the level of output protection required by the content.';
                                  end;
    LongInt($C00D2905)          : begin
                                    HrStr := 'NS_E_COMPRESSED_DIGITAL_VIDEO_PROTECTION_LEVEL_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player does not support the level of protection required for compressed digital video.';
                                  end;
    LongInt($C00D2906)          : begin
                                    HrStr := 'NS_E_UNCOMPRESSED_DIGITAL_VIDEO_PROTECTION_LEVEL_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player does not support the level of protection required for uncompressed digital video.';
                                  end;
    LongInt($C00D2907)          : begin
                                    HrStr := 'NS_E_ANALOG_VIDEO_PROTECTION_LEVEL_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player does not support the level of protection required for analog video.';
                                  end;
    LongInt($C00D2908)          : begin
                                    HrStr := 'NS_E_COMPRESSED_DIGITAL_AUDIO_PROTECTION_LEVEL_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player does not support the level of protection required for compressed digital audio.';
                                  end;
    LongInt($C00D2909)          : begin
                                    HrStr := 'NS_E_UNCOMPRESSED_DIGITAL_AUDIO_PROTECTION_LEVEL_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player does not support the level of protection required for uncompressed digital audio.';
                                  end;
    LongInt($C00D290A)          : begin
                                    HrStr := 'NS_E_OUTPUT_PROTECTION_SCHEME_UNSUPPORTED';
                                    HrDescr := 'Windows Media Player does not support the scheme of output protection required by the content.';
                                  end;
    LongInt($C00D2AFA)          : begin
                                    HrStr := 'NS_E_REBOOT_RECOMMENDED';
                                    HrDescr := 'Installation was not successful and some file cleanup is not complete.' +
                                               'For best results, restart your computer.';
                                  end;
    LongInt($C00D2AFB)          : begin
                                    HrStr := 'NS_E_REBOOT_REQUIRED';
                                    HrDescr := 'Installation was not successful.' +
                                               'To continue, you must restart your computer.';
                                  end;
    LongInt($C00D2AFC)          : begin
                                    HrStr := 'NS_E_SETUP_INCOMPLETE';
                                    HrDescr := 'Installation was not successful.';
                                  end;
    LongInt($C00D2AFD)          : begin
                                    HrStr := 'NS_E_SETUP_DRM_MIGRATION_FAILED';
                                    HrDescr := 'Setup cannot migrate the Windows Media Digital Rights Management (DRM) components.';
                                  end;
    LongInt($C00D2AFE)          : begin
                                    HrStr := 'NS_E_SETUP_IGNORABLE_FAILURE';
                                    HrDescr := 'Some skin or playlist components cannot be installed.';
                                  end;
    LongInt($C00D2AFF)          : begin
                                    HrStr := 'NS_E_SETUP_DRM_MIGRATION_FAILED_AND_IGNORABLE_FAILURE';
                                    HrDescr := 'Setup cannot migrate the Windows Media Digital Rights Management (DRM) components.' +
                                               'In addition, some skin or playlist components cannot be installed.';
                                  end;
    LongInt($C00D2B00)          : begin
                                    HrStr := 'NS_E_SETUP_BLOCKED';
                                    HrDescr := 'Installation is blocked because your computer does not meet one or more of the setup requirements.';
                                  end;
    LongInt($C00D2EE0)          : begin
                                    HrStr := 'NS_E_UNKNOWN_PROTOCOL';
                                    HrDescr := 'The specified protocol is not supported.';
                                  end;
    LongInt($C00D2EE1)          : begin
                                    HrStr := 'NS_E_REDIRECT_TO_PROXY';
                                    HrDescr := 'The client is redirected to a proxy server.';
                                  end;
    LongInt($C00D2EE2)          : begin
                                    HrStr := 'NS_E_INTERNAL_SERVER_ERROR';
                                    HrDescr := 'The server encountered an unexpected condition which prevented it from fulfilling the request.';
                                  end;
    LongInt($C00D2EE3)          : begin
                                    HrStr := 'NS_E_BAD_REQUEST';
                                    HrDescr := 'The request could not be understood by the server.';
                                  end;
    LongInt($C00D2EE4)          : begin
                                    HrStr := 'NS_E_ERROR_FROM_PROXY';
                                    HrDescr := 'The proxy experienced an error while attempting to contact the media server.';
                                  end;
    LongInt($C00D2EE5)          : begin
                                    HrStr := 'NS_E_PROXY_TIMEOUT';
                                    HrDescr := 'The proxy did not receive a timely response while attempting to contact the media server.';
                                  end;
    LongInt($C00D2EE6)          : begin
                                    HrStr := 'NS_E_SERVER_UNAVAILABLE';
                                    HrDescr := 'The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.';
                                  end;
    LongInt($C00D2EE7)          : begin
                                    HrStr := 'NS_E_REFUSED_BY_SERVER';
                                    HrDescr := 'The server is refusing to fulfill the requested operation.';
                                  end;
    LongInt($C00D2EE8)          : begin
                                    HrStr := 'NS_E_INCOMPATIBLE_SERVER';
                                    HrDescr := 'The server is not a compatible streaming media server.';
                                  end;
    LongInt($C00D2EE9)          : begin
                                    HrStr := 'NS_E_MULTICAST_DISABLED';
                                    HrDescr := 'The content cannot be streamed because the Multicast protocol has been disabled.';
                                  end;
    LongInt($C00D2EEA)          : begin
                                    HrStr := 'NS_E_INVALID_REDIRECT';
                                    HrDescr := 'The server redirected the player to an invalid location.';
                                  end;
    LongInt($C00D2EEB)          : begin
                                    HrStr := 'NS_E_ALL_PROTOCOLS_DISABLED';
                                    HrDescr := 'The content cannot be streamed because all protocols have been disabled.';
                                  end;
    LongInt($C00D2EEC)          : begin
                                    HrStr := 'NS_E_MSBD_NO_LONGER_SUPPORTED';
                                    HrDescr := 'The MSBD protocol is no longer supported.' +
                                               'Please use HTTP to connect to the Windows Media stream.';
                                  end;
    LongInt($C00D2EED)          : begin
                                    HrStr := 'NS_E_PROXY_NOT_FOUND';
                                    HrDescr := 'The proxy server could not be located.' +
                                               'Please check your proxy server configuration.';
                                  end;
    LongInt($C00D2EEE)          : begin
                                    HrStr := 'NS_E_CANNOT_CONNECT_TO_PROXY';
                                    HrDescr := 'Unable to establish a connection to the proxy server.' +
                                               'Please check your proxy server configuration.';
                                  end;
    LongInt($C00D2EEF)          : begin
                                    HrStr := 'NS_E_SERVER_DNS_TIMEOUT';
                                    HrDescr := 'Unable to locate the media server.' +
                                               'The operation timed out.';
                                  end;
    LongInt($C00D2EF0)          : begin
                                    HrStr := 'NS_E_PROXY_DNS_TIMEOUT';
                                    HrDescr := 'Unable to locate the proxy server.' +
                                               'The operation timed out.';
                                  end;
    LongInt($C00D2EF1)          : begin
                                    HrStr := 'NS_E_CLOSED_ON_SUSPEND';
                                    HrDescr := 'Media closed because Windows was shut down.';
                                  end;
    LongInt($C00D2EF2)          : begin
                                    HrStr := 'NS_E_CANNOT_READ_PLAYLIST_FROM_MEDIASERVER';
                                    HrDescr := 'Unable to read the contents of a playlist file from a media server.';
                                  end;
    LongInt($C00D2EF3)          : begin
                                    HrStr := 'NS_E_SESSION_NOT_FOUND';
                                    HrDescr := 'Session not found.';
                                  end;
    LongInt($C00D2EF4)          : begin
                                    HrStr := 'NS_E_REQUIRE_STREAMING_CLIENT';
                                    HrDescr := 'Content requires a streaming media client.';
                                  end;
    LongInt($C00D2EF5)          : begin
                                    HrStr := 'NS_E_PLAYLIST_ENTRY_HAS_CHANGED';
                                    HrDescr := 'A command applies to a previous playlist entry.';
                                  end;
    LongInt($C00D2EF6)          : begin
                                    HrStr := 'NS_E_PROXY_ACCESSDENIED';
                                    HrDescr := 'The proxy server is denying access.' +
                                               'The username and/or password might be incorrect.';
                                  end;
    LongInt($C00D2EF7)          : begin
                                    HrStr := 'NS_E_PROXY_SOURCE_ACCESSDENIED';
                                    HrDescr := 'The proxy could not provide valid authentication credentials to the media server.';
                                  end;
    LongInt($C00D2EF8)          : begin
                                    HrStr := 'NS_E_NETWORK_SINK_WRITE';
                                    HrDescr := 'The network sink failed to write data to the network.';
                                  end;
    LongInt($C00D2EF9)          : begin
                                    HrStr := 'NS_E_FIREWALL';
                                    HrDescr := 'Packets are not being received from the server.' +
                                               'The packets might be blocked by a filtering device, such as a network firewall.';
                                  end;
    LongInt($C00D2EFA)          : begin
                                    HrStr := 'NS_E_MMS_NOT_SUPPORTED';
                                    HrDescr := 'The MMS protocol is not supported.' +
                                               'Please use HTTP or RTSP to connect to the Windows Media stream.';
                                  end;
    LongInt($C00D2EFB)          : begin
                                    HrStr := 'NS_E_SERVER_ACCESSDENIED';
                                    HrDescr := 'The Windows Media server is denying access.' +
                                               'The username and/or password might be incorrect.';
                                  end;
    LongInt($C00D2EFC)          : begin
                                    HrStr := 'NS_E_RESOURCE_GONE';
                                    HrDescr := 'The Publishing Point or file on the Windows Media Server is no longer available.';
                                  end;
    LongInt($C00D2EFD)          : begin
                                    HrStr := 'NS_E_NO_EXISTING_PACKETIZER';
                                    HrDescr := 'There is no existing packetizer plugin for a stream.';
                                  end;
    LongInt($C00D2EFE)          : begin
                                    HrStr := 'NS_E_BAD_SYNTAX_IN_SERVER_RESPONSE';
                                    HrDescr := 'The response from the media server could not be understood.' +
                                               'This might be caused by an incompatible proxy server or media server.';
                                  end;
    LongInt($C00D2F00)          : begin
                                    HrStr := 'NS_E_RESET_SOCKET_CONNECTION';
                                    HrDescr := 'The Windows Media Server reset the network connection.';
                                  end;
    LongInt($C00D2F02)          : begin
                                    HrStr := 'NS_E_TOO_MANY_HOPS';
                                    HrDescr := 'The request could not reach the media server (too many hops).';
                                  end;
    LongInt($C00D2F05)          : begin
                                    HrStr := 'NS_E_TOO_MUCH_DATA_FROM_SERVER';
                                    HrDescr := 'The server is sending too much data.' +
                                               'The connection has been terminated.';
                                  end;
    LongInt($C00D2F06)          : begin
                                    HrStr := 'NS_E_CONNECT_TIMEOUT';
                                    HrDescr := 'It was not possible to establish a connection to the media server in a timely manner.' +
                                               'The media server might be down for maintenance, or it might be necessary to use a proxy server to access this media server.';
                                  end;
    LongInt($C00D2F07)          : begin
                                    HrStr := 'NS_E_PROXY_CONNECT_TIMEOUT';
                                    HrDescr := 'It was not possible to establish a connection to the proxy server in a timely manner.' +
                                               'Please check your proxy server configuration.';
                                  end;
    LongInt($C00D2F08)          : begin
                                    HrStr := 'NS_E_SESSION_INVALID';
                                    HrDescr := 'Session not found.';
                                  end;
    LongInt($C00D2F0A)          : begin
                                    HrStr := 'NS_E_PACKETSINK_UNKNOWN_FEC_STREAM';
                                    HrDescr := 'Unknown packet sink stream.';
                                  end;
    LongInt($C00D2F0B)          : begin
                                    HrStr := 'NS_E_PUSH_CANNOTCONNECT';
                                    HrDescr := 'Unable to establish a connection to the server.' +
                                               'Ensure Windows Media Services is started and the HTTP Server control protocol is properly enabled.';
                                  end;
    LongInt($C00D2F0C)          : begin
                                    HrStr := 'NS_E_INCOMPATIBLE_PUSH_SERVER';
                                    HrDescr := 'The Server service that received the HTTP push request is not a compatible version of Windows Media Services (WMS).' +
                                               'This error might indicate the push request was received by IIS instead of WMS.' +
                                               'Ensure WMS is started and has the HTTP Server control protocol properly enabled and try again.';
                                  end;
    LongInt($C00D32C8)          : begin
                                    HrStr := 'NS_E_END_OF_PLAYLIST';
                                    HrDescr := 'The playlist has reached its end.';
                                  end;
    LongInt($C00D32C9)          : begin
                                    HrStr := 'NS_E_USE_FILE_SOURCE';
                                    HrDescr := 'Use file source.';
                                  end;
    LongInt($C00D32CA)          : begin
                                    HrStr := 'NS_E_PROPERTY_NOT_FOUND';
                                    HrDescr := 'The property was not found.';
                                  end;
    LongInt($C00D32CC)          : begin
                                    HrStr := 'NS_E_PROPERTY_READ_ONLY';
                                    HrDescr := 'The property is read only.';
                                  end;
    LongInt($C00D32CD)          : begin
                                    HrStr := 'NS_E_TABLE_KEY_NOT_FOUND';
                                    HrDescr := 'The table key was not found.';
                                  end;
    LongInt($C00D32CF)          : begin
                                    HrStr := 'NS_E_INVALID_QUERY_OPERATOR';
                                    HrDescr := 'Invalid query operator.';
                                  end;
    LongInt($C00D32D0)          : begin
                                    HrStr := 'NS_E_INVALID_QUERY_PROPERTY';
                                    HrDescr := 'Invalid query property.';
                                  end;
    LongInt($C00D32D2)          : begin
                                    HrStr := 'NS_E_PROPERTY_NOT_SUPPORTED';
                                    HrDescr := 'The property is not supported.';
                                  end;
    LongInt($C00D32D4)          : begin
                                    HrStr := 'NS_E_SCHEMA_CLASSIFY_FAILURE';
                                    HrDescr := 'Schema classification failure.';
                                  end;
    LongInt($C00D32D5)          : begin
                                    HrStr := 'NS_E_METADATA_FORMAT_NOT_SUPPORTED';
                                    HrDescr := 'The metadata format is not supported.';
                                  end;
    LongInt($C00D32D6)          : begin
                                    HrStr := 'NS_E_METADATA_NO_EDITING_CAPABILITY';
                                    HrDescr := 'Cannot edit the metadata.';
                                  end;
    LongInt($C00D32D7)          : begin
                                    HrStr := 'NS_E_METADATA_CANNOT_SET_LOCALE';
                                    HrDescr := 'Cannot set the locale id.';
                                  end;
    LongInt($C00D32D8)          : begin
                                    HrStr := 'NS_E_METADATA_LANGUAGE_NOT_SUPORTED';
                                    HrDescr := 'The language is not supported in the format.';
                                  end;
    LongInt($C00D32D9)          : begin
                                    HrStr := 'NS_E_METADATA_NO_RFC1766_NAME_FOR_LOCALE';
                                    HrDescr := 'There is no RFC1766 name translation for the supplied locale id.';
                                  end;
    LongInt($C00D32DA)          : begin
                                    HrStr := 'NS_E_METADATA_NOT_AVAILABLE';
                                    HrDescr := 'The metadata (or metadata item) is not available.';
                                  end;
    LongInt($C00D32DB)          : begin
                                    HrStr := 'NS_E_METADATA_CACHE_DATA_NOT_AVAILABLE';
                                    HrDescr := 'The cached metadata (or metadata item) is not available.';
                                  end;
    LongInt($C00D32DC)          : begin
                                    HrStr := 'NS_E_METADATA_INVALID_DOCUMENT_TYPE';
                                    HrDescr := 'The metadata document is invalid.';
                                  end;
    LongInt($C00D32DD)          : begin
                                    HrStr := 'NS_E_METADATA_IDENTIFIER_NOT_AVAILABLE';
                                    HrDescr := 'The metadata content identifier is not available.';
                                  end;
    LongInt($C00D32DE)          : begin
                                    HrStr := 'NS_E_METADATA_CANNOT_RETRIEVE_FROM_OFFLINE_CACHE';
                                    HrDescr := 'Cannot retrieve metadata from the offline metadata cache.';
                                  end;
    LongInt($C0261003)          : begin
                                    HrStr := 'ERROR_MONITOR_INVALID_DESCRIPTOR_CHECKSUM';
                                    HrDescr := 'Checksum of the obtained monitor descriptor is invalid.';
                                  end;
    LongInt($C0261004)          : begin
                                    HrStr := 'ERROR_MONITOR_INVALID_STANDARD_TIMING_BLOCK';
                                    HrDescr := 'Monitor descriptor contains an invalid standard timing block.';
                                  end;
    LongInt($C0261005)          : begin
                                    HrStr := 'ERROR_MONITOR_WMI_DATABLOCK_REGISTRATION_FAILED';
                                    HrDescr := 'Windows Management Instrumentation (WMI) data block registration failed for one of the MSMonitorClass WMI subclasses.';
                                  end;
    LongInt($C0261006)          : begin
                                    HrStr := 'ERROR_MONITOR_INVALID_SERIAL_NUMBER_MONDSC_BLOCK';
                                    HrDescr := 'Provided monitor descriptor block is either corrupted or does not contain the monitor''s detailed serial number.';
                                  end;
    LongInt($C0261007)          : begin
                                    HrStr := 'ERROR_MONITOR_INVALID_USER_FRIENDLY_MONDSC_BLOCK';
                                    HrDescr := 'Provided monitor descriptor block is either corrupted or does not contain the monitor''s user-friendly name.';
                                  end;
    LongInt($C0261008)          : begin
                                    HrStr := 'ERROR_MONITOR_NO_MORE_DESCRIPTOR_DATA';
                                    HrDescr := 'There is no monitor descriptor data at the specified (offset, size) region.';
                                  end;
    LongInt($C0261009)          : begin
                                    HrStr := 'ERROR_MONITOR_INVALID_DETAILED_TIMING_BLOCK';
                                    HrDescr := 'Monitor descriptor contains an invalid detailed timing block.';
                                  end;
    LongInt($C0262000)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NOT_EXCLUSIVE_MODE_OWNER';
                                    HrDescr := 'Exclusive mode ownership is needed to create unmanaged primary allocation.';
                                  end;
    LongInt($C0262001)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INSUFFICIENT_DMA_BUFFER';
                                    HrDescr := 'The driver needs more direct memory access (DMA) buffer space to complete the requested operation.';
                                  end;
    LongInt($C0262002)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_DISPLAY_ADAPTER';
                                    HrDescr := 'Specified display adapter handle is invalid.';
                                  end;
    LongInt($C0262003)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ADAPTER_WAS_RESET';
                                    HrDescr := 'Specified display adapter and all of its state has been reset.';
                                  end;
    LongInt($C0262004)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_DRIVER_MODEL';
                                    HrDescr := 'The driver stack does not match the expected driver model.';
                                  end;
    LongInt($C0262005)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PRESENT_MODE_CHANGED';
                                    HrDescr := 'Present happened but ended up into the changed desktop mode.';
                                  end;
    LongInt($C0262006)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PRESENT_OCCLUDED';
                                    HrDescr := 'Nothing to present due to desktop occlusion.';
                                  end;
    LongInt($C0262007)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PRESENT_DENIED';
                                    HrDescr := 'Not able to present due to denial of desktop access.';
                                  end;
    LongInt($C0262008)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CANNOTCOLORCONVERT';
                                    HrDescr := 'Not able to present with color conversion.';
                                  end;
    LongInt($C0262100)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_VIDEO_MEMORY';
                                    HrDescr := 'Not enough video memory available to complete the operation.';
                                  end;
    LongInt($C0262101)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CANT_LOCK_MEMORY';
                                    HrDescr := 'Could not probe and lock the underlying memory of an allocation.';
                                  end;
    LongInt($C0262102)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ALLOCATION_BUSY';
                                    HrDescr := 'The allocation is currently busy.';
                                  end;
    LongInt($C0262103)          : begin
                                    HrStr := 'ERROR_GRAPHICS_TOO_MANY_REFERENCES';
                                    HrDescr := 'An object being referenced has reach the maximum reference count already and cannot be referenced further.';
                                  end;
    LongInt($C0262104)          : begin
                                    HrStr := 'ERROR_GRAPHICS_TRY_AGAIN_LATER';
                                    HrDescr := 'A problem could not be solved due to some currently existing condition.' +
                                               'The problem should be tried again later.';
                                  end;
    LongInt($C0262105)          : begin
                                    HrStr := 'ERROR_GRAPHICS_TRY_AGAIN_NOW';
                                    HrDescr := 'A problem could not be solved due to some currently existing condition.' +
                                               'The problem should be tried again immediately.';
                                  end;
    LongInt($C0262106)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ALLOCATION_INVALID';
                                    HrDescr := 'The allocation is invalid.';
                                  end;
    LongInt($C0262107)          : begin
                                    HrStr := 'ERROR_GRAPHICS_UNSWIZZLING_APERTURE_UNAVAILABLE';
                                    HrDescr := 'No more unswizzling apertures are currently available.';
                                  end;
    LongInt($C0262108)          : begin
                                    HrStr := 'ERROR_GRAPHICS_UNSWIZZLING_APERTURE_UNSUPPORTED';
                                    HrDescr := 'The current allocation cannot be unswizzled by an aperture.';
                                  end;
    LongInt($C0262109)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CANT_EVICT_PINNED_ALLOCATION';
                                    HrDescr := 'The request failed because a pinned allocation cannot be evicted.';
                                  end;
    LongInt($C0262110)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_ALLOCATION_USAGE';
                                    HrDescr := 'The allocation cannot be used from its current segment location for the specified operation.';
                                  end;
    LongInt($C0262111)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CANT_RENDER_LOCKED_ALLOCATION';
                                    HrDescr := 'A locked allocation cannot be used in the current command buffer.';
                                  end;
    LongInt($C0262112)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ALLOCATION_CLOSED';
                                    HrDescr := 'The allocation being referenced has been closed permanently.';
                                  end;
    LongInt($C0262113)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_ALLOCATION_INSTANCE';
                                    HrDescr := 'An invalid allocation instance is being referenced.';
                                  end;
    LongInt($C0262114)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_ALLOCATION_HANDLE';
                                    HrDescr := 'An invalid allocation handle is being referenced.';
                                  end;
    LongInt($C0262115)          : begin
                                    HrStr := 'ERROR_GRAPHICS_WRONG_ALLOCATION_DEVICE';
                                    HrDescr := 'The allocation being referenced does not belong to the current device.';
                                  end;
    LongInt($C0262116)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ALLOCATION_CONTENT_LOST';
                                    HrDescr := 'The specified allocation lost its content.';
                                  end;
    LongInt($C0262200)          : begin
                                    HrStr := 'ERROR_GRAPHICS_GPU_EXCEPTION_ON_DEVICE';
                                    HrDescr := 'Graphics processing unit (GPU) exception is detected on the given device.' +
                                               'The device is not able to be scheduled.';
                                  end;
    LongInt($C0262300)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDPN_TOPOLOGY';
                                    HrDescr := 'Specified video present network (VidPN) topology is invalid.';
                                  end;
    LongInt($C0262301)          : begin
                                    HrStr := 'ERROR_GRAPHICS_VIDPN_TOPOLOGY_NOT_SUPPORTED';
                                    HrDescr := 'Specified VidPN topology is valid but is not supported by this model of the display adapter.';
                                  end;
    LongInt($C0262302)          : begin
                                    HrStr := 'ERROR_GRAPHICS_VIDPN_TOPOLOGY_CURRENTLY_NOT_SUPPORTED';
                                    HrDescr := 'Specified VidPN topology is valid but is not supported by the display adapter at this time, due to current allocation of its resources.';
                                  end;
    LongInt($C0262303)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDPN';
                                    HrDescr := 'Specified VidPN handle is invalid.';
                                  end;
    LongInt($C0262304)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDEO_PRESENT_SOURCE';
                                    HrDescr := 'Specified video present source is invalid.';
                                  end;
    LongInt($C0262305)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDEO_PRESENT_TARGET';
                                    HrDescr := 'Specified video present target is invalid.';
                                  end;
    LongInt($C0262306)          : begin
                                    HrStr := 'ERROR_GRAPHICS_VIDPN_MODALITY_NOT_SUPPORTED';
                                    HrDescr := 'Specified VidPN modality is not supported (for example, at least two of the pinned modes are not cofunctional).';
                                  end;
    LongInt($C0262308)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDPN_SOURCEMODESET';
                                    HrDescr := 'Specified VidPN source mode set is invalid.';
                                  end;
    LongInt($C0262309)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDPN_TARGETMODESET';
                                    HrDescr := 'Specified VidPN target mode set is invalid.';
                                  end;
    LongInt($C026230A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_FREQUENCY';
                                    HrDescr := 'Specified video signal frequency is invalid.';
                                  end;
    LongInt($C026230B)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_ACTIVE_REGION';
                                    HrDescr := 'Specified video signal active region is invalid.';
                                  end;
    LongInt($C026230C)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_TOTAL_REGION';
                                    HrDescr := 'Specified video signal total region is invalid.';
                                  end;
    LongInt($C0262310)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDEO_PRESENT_SOURCE_MODE';
                                    HrDescr := 'Specified video present source mode is invalid.';
                                  end;
    LongInt($C0262311)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDEO_PRESENT_TARGET_MODE';
                                    HrDescr := 'Specified video present target mode is invalid.';
                                  end;
    LongInt($C0262312)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PINNED_MODE_MUST_REMAIN_IN_SET';
                                    HrDescr := 'Pinned mode must remain in the set on VidPN''s cofunctional modality enumeration.';
                                  end;
    LongInt($C0262313)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PATH_ALREADY_IN_TOPOLOGY';
                                    HrDescr := 'Specified video present path is already in the VidPN topology.';
                                  end;
    LongInt($C0262314)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MODE_ALREADY_IN_MODESET';
                                    HrDescr := 'Specified mode is already in the mode set.';
                                  end;
    LongInt($C0262315)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDEOPRESENTSOURCESET';
                                    HrDescr := 'Specified video present source set is invalid.';
                                  end;
    LongInt($C0262316)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDEOPRESENTTARGETSET';
                                    HrDescr := 'Specified video present target set is invalid.';
                                  end;
    LongInt($C0262317)          : begin
                                    HrStr := 'ERROR_GRAPHICS_SOURCE_ALREADY_IN_SET';
                                    HrDescr := 'Specified video present source is already in the video present source set.';
                                  end;
    LongInt($C0262318)          : begin
                                    HrStr := 'ERROR_GRAPHICS_TARGET_ALREADY_IN_SET';
                                    HrDescr := 'Specified video present target is already in the video present target set.';
                                  end;
    LongInt($C0262319)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDPN_PRESENT_PATH';
                                    HrDescr := 'Specified VidPN present path is invalid.';
                                  end;
    LongInt($C026231A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_RECOMMENDED_VIDPN_TOPOLOGY';
                                    HrDescr := 'Miniport has no recommendation for augmentation of the specified VidPN topology.';
                                  end;
    LongInt($C026231B)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_MONITOR_FREQUENCYRANGESET';
                                    HrDescr := 'Specified monitor frequency range set is invalid.';
                                  end;
    LongInt($C026231C)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_MONITOR_FREQUENCYRANGE';
                                    HrDescr := 'Specified monitor frequency range is invalid.';
                                  end;
    LongInt($C026231D)          : begin
                                    HrStr := 'ERROR_GRAPHICS_FREQUENCYRANGE_NOT_IN_SET';
                                    HrDescr := 'Specified frequency range is not in the specified monitor frequency range set.';
                                  end;
    LongInt($C026231F)          : begin
                                    HrStr := 'ERROR_GRAPHICS_FREQUENCYRANGE_ALREADY_IN_SET';
                                    HrDescr := 'Specified frequency range is already in the specified monitor frequency range set.';
                                  end;
    LongInt($C0262320)          : begin
                                    HrStr := 'ERROR_GRAPHICS_STALE_MODESET';
                                    HrDescr := 'Specified mode set is stale.' +
                                               'Reacquire the new mode set.';
                                  end;
    LongInt($C0262321)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_MONITOR_SOURCEMODESET';
                                    HrDescr := 'Specified monitor source mode set is invalid.';
                                  end;
    LongInt($C0262322)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_MONITOR_SOURCE_MODE';
                                    HrDescr := 'Specified monitor source mode is invalid.';
                                  end;
    LongInt($C0262323)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_RECOMMENDED_FUNCTIONAL_VIDPN';
                                    HrDescr := 'Miniport does not have any recommendation regarding the request to provide a functional VidPN given the current display adapter configuration.';
                                  end;
    LongInt($C0262324)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MODE_ID_MUST_BE_UNIQUE';
                                    HrDescr := 'ID of the specified mode is already used by another mode in the set.';
                                  end;
    LongInt($C0262325)          : begin
                                    HrStr := 'ERROR_GRAPHICS_EMPTY_ADAPTER_MONITOR_MODE_SUPPORT_INTERSECTION';
                                    HrDescr := 'System failed to determine a mode that is supported by both the display adapter and the monitor connected to it.';
                                  end;
    LongInt($C0262326)          : begin
                                    HrStr := 'ERROR_GRAPHICS_VIDEO_PRESENT_TARGETS_LESS_THAN_SOURCES';
                                    HrDescr := 'Number of video present targets must be greater than or equal to the number of video present sources.';
                                  end;
    LongInt($C0262327)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PATH_NOT_IN_TOPOLOGY';
                                    HrDescr := 'Specified present path is not in the VidPN topology.';
                                  end;
    LongInt($C0262328)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ADAPTER_MUST_HAVE_AT_LEAST_ONE_SOURCE';
                                    HrDescr := 'Display adapter must have at least one video present source.';
                                  end;
    LongInt($C0262329)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ADAPTER_MUST_HAVE_AT_LEAST_ONE_TARGET';
                                    HrDescr := 'Display adapter must have at least one video present target.';
                                  end;
    LongInt($C026232A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_MONITORDESCRIPTORSET';
                                    HrDescr := 'Specified monitor descriptor set is invalid.';
                                  end;
    LongInt($C026232B)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_MONITORDESCRIPTOR';
                                    HrDescr := 'Specified monitor descriptor is invalid.';
                                  end;
    LongInt($C026232C)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MONITORDESCRIPTOR_NOT_IN_SET';
                                    HrDescr := 'Specified descriptor is not in the specified monitor descriptor set.';
                                  end;
    LongInt($C026232D)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MONITORDESCRIPTOR_ALREADY_IN_SET';
                                    HrDescr := 'Specified descriptor is already in the specified monitor descriptor set.';
                                  end;
    LongInt($C026232E)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MONITORDESCRIPTOR_ID_MUST_BE_UNIQUE';
                                    HrDescr := 'ID of the specified monitor descriptor is already used by another descriptor in the set.';
                                  end;
    LongInt($C026232F)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDPN_TARGET_SUBSET_TYPE';
                                    HrDescr := 'Specified video present target subset type is invalid.';
                                  end;
    LongInt($C0262330)          : begin
                                    HrStr := 'ERROR_GRAPHICS_RESOURCES_NOT_RELATED';
                                    HrDescr := 'Two or more of the specified resources are not related to each other, as defined by the interface semantics.';
                                  end;
    LongInt($C0262331)          : begin
                                    HrStr := 'ERROR_GRAPHICS_SOURCE_ID_MUST_BE_UNIQUE';
                                    HrDescr := 'ID of the specified video present source is already used by another source in the set.';
                                  end;
    LongInt($C0262332)          : begin
                                    HrStr := 'ERROR_GRAPHICS_TARGET_ID_MUST_BE_UNIQUE';
                                    HrDescr := 'ID of the specified video present target is already used by another target in the set.';
                                  end;
    LongInt($C0262333)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_AVAILABLE_VIDPN_TARGET';
                                    HrDescr := 'Specified VidPN source cannot be used because there is no available VidPN target to connect it to.';
                                  end;
    LongInt($C0262334)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MONITOR_COULD_NOT_BE_ASSOCIATED_WITH_ADAPTER';
                                    HrDescr := 'Newly arrived monitor could not be associated with a display adapter.';
                                  end;
    LongInt($C0262335)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_VIDPNMGR';
                                    HrDescr := 'Display adapter in question does not have an associated VidPN manager.';
                                  end;
    LongInt($C0262336)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_ACTIVE_VIDPN';
                                    HrDescr := 'VidPN manager of the display adapter in question does not have an active VidPN.';
                                  end;
    LongInt($C0262337)          : begin
                                    HrStr := 'ERROR_GRAPHICS_STALE_VIDPN_TOPOLOGY';
                                    HrDescr := 'Specified VidPN topology is stale.' +
                                               'Re-acquire the new topology.';
                                  end;
    LongInt($C0262338)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MONITOR_NOT_CONNECTED';
                                    HrDescr := 'There is no monitor connected on the specified video present target.';
                                  end;
    LongInt($C0262339)          : begin
                                    HrStr := 'ERROR_GRAPHICS_SOURCE_NOT_IN_TOPOLOGY';
                                    HrDescr := 'Specified source is not part of the specified VidPN topology.';
                                  end;
    LongInt($C026233A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_PRIMARYSURFACE_SIZE';
                                    HrDescr := 'Specified primary surface size is invalid.';
                                  end;
    LongInt($C026233B)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VISIBLEREGION_SIZE';
                                    HrDescr := 'Specified visible region size is invalid.';
                                  end;
    LongInt($C026233C)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_STRIDE';
                                    HrDescr := 'Specified stride is invalid.';
                                  end;
    LongInt($C026233D)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_PIXELFORMAT';
                                    HrDescr := 'Specified pixel format is invalid.';
                                  end;
    LongInt($C026233E)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_COLORBASIS';
                                    HrDescr := 'Specified color basis is invalid.';
                                  end;
    LongInt($C026233F)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_PIXELVALUEACCESSMODE';
                                    HrDescr := 'Specified pixel value access mode is invalid.';
                                  end;
    LongInt($C0262340)          : begin
                                    HrStr := 'ERROR_GRAPHICS_TARGET_NOT_IN_TOPOLOGY';
                                    HrDescr := 'Specified target is not part of the specified VidPN topology.';
                                  end;
    LongInt($C0262341)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_DISPLAY_MODE_MANAGEMENT_SUPPORT';
                                    HrDescr := 'Failed to acquire display mode management interface.';
                                  end;
    LongInt($C0262342)          : begin
                                    HrStr := 'ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE';
                                    HrDescr := 'Specified VidPN source is already owned by a display mode manager (DMM) client and cannot be used until that client releases it.';
                                  end;
    LongInt($C0262343)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CANT_ACCESS_ACTIVE_VIDPN';
                                    HrDescr := 'Specified VidPN is active and cannot be accessed.';
                                  end;
    LongInt($C0262344)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_PATH_IMPORTANCE_ORDINAL';
                                    HrDescr := 'Specified VidPN present path importance ordinal is invalid.';
                                  end;
    LongInt($C0262345)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_PATH_CONTENT_GEOMETRY_TRANSFORMATION';
                                    HrDescr := 'Specified VidPN present path content geometry transformation is invalid.';
                                  end;
    LongInt($C0262346)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PATH_CONTENT_GEOMETRY_TRANSFORMATION_NOT_SUPPORTED';
                                    HrDescr := 'Specified content geometry transformation is not supported on the respective VidPN present path.';
                                  end;
    LongInt($C0262347)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_GAMMA_RAMP';
                                    HrDescr := 'Specified gamma ramp is invalid.';
                                  end;
    LongInt($C0262348)          : begin
                                    HrStr := 'ERROR_GRAPHICS_GAMMA_RAMP_NOT_SUPPORTED';
                                    HrDescr := 'Specified gamma ramp is not supported on the respective VidPN present path.';
                                  end;
    LongInt($C0262349)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MULTISAMPLING_NOT_SUPPORTED';
                                    HrDescr := 'Multisampling is not supported on the respective VidPN present path.';
                                  end;
    LongInt($C026234A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MODE_NOT_IN_MODESET';
                                    HrDescr := 'Specified mode is not in the specified mode set.';
                                  end;
    LongInt($C026234D)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_VIDPN_TOPOLOGY_RECOMMENDATION_REASON';
                                    HrDescr := 'Specified VidPN topology recommendation reason is invalid.';
                                  end;
    LongInt($C026234E)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_PATH_CONTENT_TYPE';
                                    HrDescr := 'Specified VidPN present path content type is invalid.';
                                  end;
    LongInt($C026234F)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_COPYPROTECTION_TYPE';
                                    HrDescr := 'Specified VidPN present path copy protection type is invalid.';
                                  end;
    LongInt($C0262350)          : begin
                                    HrStr := 'ERROR_GRAPHICS_UNASSIGNED_MODESET_ALREADY_EXISTS';
                                    HrDescr := 'No more than one unassigned mode set can exist at any given time for a given VidPN source or target.';
                                  end;
    LongInt($C0262352)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_SCANLINE_ORDERING';
                                    HrDescr := 'The specified scan line ordering type is invalid.';
                                  end;
    LongInt($C0262353)          : begin
                                    HrStr := 'ERROR_GRAPHICS_TOPOLOGY_CHANGES_NOT_ALLOWED';
                                    HrDescr := 'Topology changes are not allowed for the specified VidPN.';
                                  end;
    LongInt($C0262354)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NO_AVAILABLE_IMPORTANCE_ORDINALS';
                                    HrDescr := 'All available importance ordinals are already used in the specified topology.';
                                  end;
    LongInt($C0262355)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INCOMPATIBLE_PRIVATE_FORMAT';
                                    HrDescr := 'Specified primary surface has a different private format attribute than the current primary surface.';
                                  end;
    LongInt($C0262356)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INVALID_MODE_PRUNING_ALGORITHM';
                                    HrDescr := 'Specified mode pruning algorithm is invalid.';
                                  end;
    LongInt($C0262400)          : begin
                                    HrStr := 'ERROR_GRAPHICS_SPECIFIED_CHILD_ALREADY_CONNECTED';
                                    HrDescr := 'Specified display adapter child device already has an external device connected to it.';
                                  end;
    LongInt($C0262401)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CHILD_DESCRIPTOR_NOT_SUPPORTED';
                                    HrDescr := 'The display adapter child device does not support reporting a descriptor.';
                                  end;
    LongInt($C0262430)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NOT_A_LINKED_ADAPTER';
                                    HrDescr := 'The display adapter is not linked to any other adapters.';
                                  end;
    LongInt($C0262431)          : begin
                                    HrStr := 'ERROR_GRAPHICS_LEADLINK_NOT_ENUMERATED';
                                    HrDescr := 'Lead adapter in a linked configuration was not enumerated yet.';
                                  end;
    LongInt($C0262432)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CHAINLINKS_NOT_ENUMERATED';
                                    HrDescr := 'Some chain adapters in a linked configuration were not enumerated yet.';
                                  end;
    LongInt($C0262433)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ADAPTER_CHAIN_NOT_READY';
                                    HrDescr := 'The chain of linked adapters is not ready to start because of an unknown failure.';
                                  end;
    LongInt($C0262434)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CHAINLINKS_NOT_STARTED';
                                    HrDescr := 'An attempt was made to start a lead link display adapter when the chain links were not started yet.';
                                  end;
    LongInt($C0262435)          : begin
                                    HrStr := 'ERROR_GRAPHICS_CHAINLINKS_NOT_POWERED_ON';
                                    HrDescr := 'An attempt was made to turn on a lead link display adapter when the chain links were turned off.';
                                  end;
    LongInt($C0262436)          : begin
                                    HrStr := 'ERROR_GRAPHICS_INCONSISTENT_DEVICE_LINK_STATE';
                                    HrDescr := 'The adapter link was found to be in an inconsistent state.' +
                                               'Not all adapters are in an expected PNP or power state.';
                                  end;
    LongInt($C0262438)          : begin
                                    HrStr := 'ERROR_GRAPHICS_NOT_POST_DEVICE_DRIVER';
                                    HrDescr := 'The driver trying to start is not the same as the driver for the posted display adapter.';
                                  end;
    LongInt($C0262500)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_NOT_SUPPORTED';
                                    HrDescr := 'The driver does not support Output Protection Manager (OPM).';
                                  end;
    LongInt($C0262501)          : begin
                                    HrStr := 'ERROR_GRAPHICS_COPP_NOT_SUPPORTED';
                                    HrDescr := 'The driver does not support Certified Output Protection Protocol (COPP).';
                                  end;
    LongInt($C0262502)          : begin
                                    HrStr := 'ERROR_GRAPHICS_UAB_NOT_SUPPORTED';
                                    HrDescr := 'The driver does not support a user-accessible bus (UAB).';
                                  end;
    LongInt($C0262503)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_INVALID_ENCRYPTED_PARAMETERS';
                                    HrDescr := 'The specified encrypted parameters are invalid.';
                                  end;
    LongInt($C0262504)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_PARAMETER_ARRAY_TOO_SMALL';
                                    HrDescr := 'An array passed to a function cannot hold all of the data that the function wants to put in it.';
                                  end;
    LongInt($C0262505)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_NO_VIDEO_OUTPUTS_EXIST';
                                    HrDescr := 'The GDI display device passed to this function does not have any active video outputs.';
                                  end;
    LongInt($C0262506)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PVP_NO_DISPLAY_DEVICE_CORRESPONDS_TO_NAME';
                                    HrDescr := 'The protected video path (PVP) cannot find an actual GDI display device that corresponds to the passed-in GDI display device name.';
                                  end;
    LongInt($C0262507)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PVP_DISPLAY_DEVICE_NOT_ATTACHED_TO_DESKTOP';
                                    HrDescr := 'This function failed because the GDI display device passed to it was not attached to the Windows desktop.';
                                  end;
    LongInt($C0262508)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PVP_MIRRORING_DEVICES_NOT_SUPPORTED';
                                    HrDescr := 'The PVP does not support mirroring display devices because they do not have video outputs.';
                                  end;
    LongInt($C026250A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_INVALID_POINTER';
                                    HrDescr := 'The function failed because an invalid pointer parameter was passed to it.' +
                                               'A pointer parameter is invalid if it is null, it points to an invalid address, it points to a kernel mode address, or it is not correctly aligned.';
                                  end;
    LongInt($C026250B)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_INTERNAL_ERROR';
                                    HrDescr := 'An internal error caused this operation to fail.';
                                  end;
    LongInt($C026250C)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_INVALID_HANDLE';
                                    HrDescr := 'The function failed because the caller passed in an invalid OPM user mode handle.';
                                  end;
    LongInt($C026250D)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PVP_NO_MONITORS_CORRESPOND_TO_DISPLAY_DEVICE';
                                    HrDescr := 'This function failed because the GDI device passed to it did not have any monitors associated with it.';
                                  end;
    LongInt($C026250E)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PVP_INVALID_CERTIFICATE_LENGTH';
                                    HrDescr := 'A certificate could not be returned because the certificate buffer passed to the function was too small.';
                                  end;
    LongInt($C026250F)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_SPANNING_MODE_ENABLED';
                                    HrDescr := 'A video output could not be created because the frame buffer is in spanning mode.';
                                  end;
    LongInt($C0262510)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_THEATER_MODE_ENABLED';
                                    HrDescr := 'A video output could not be created because the frame buffer is in theater mode.';
                                  end;
    LongInt($C0262511)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PVP_HFS_FAILED';
                                    HrDescr := 'The function call failed because the display adapter''s hardware functionality scan failed to validate the graphics hardware.';
                                  end;
    LongInt($C0262512)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_INVALID_SRM';
                                    HrDescr := 'The High-Bandwidth Digital Content Protection (HDCP) System Renewability Message (SRM) passed to this function did not comply with section 5 of the HDCP 1.1 specification.';
                                  end;
    LongInt($C0262513)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_OUTPUT_DOES_NOT_SUPPORT_HDCP';
                                    HrDescr := 'The video output cannot enable the HDCP system because it does not support it.';
                                  end;
    LongInt($C0262514)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_OUTPUT_DOES_NOT_SUPPORT_ACP';
                                    HrDescr := 'The video output cannot enable analog copy protection because it does not support it.';
                                  end;
    LongInt($C0262515)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_OUTPUT_DOES_NOT_SUPPORT_CGMSA';
                                    HrDescr := 'The video output cannot enable the Content Generation Management System Analog (CGMS-A) protection technology because it does not support it.';
                                  end;
    LongInt($C0262516)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_HDCP_SRM_NEVER_SET';
                                    HrDescr := 'IOPMVideoOutput''s GetInformation() method cannot return the version of the SRM being used because the application never successfully passed an SRM to the video output.';
                                  end;
    LongInt($C0262517)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_RESOLUTION_TOO_HIGH';
                                    HrDescr := 'IOPMVideoOutput''s Configure() method cannot enable the specified output protection technology because the output''s screen resolution is too high.';
                                  end;
    LongInt($C0262518)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_ALL_HDCP_HARDWARE_ALREADY_IN_USE';
                                    HrDescr := 'IOPMVideoOutput''s Configure() method cannot enable HDCP because the display adapter''s HDCP hardware is already being used by other physical outputs.';
                                  end;
    LongInt($C0262519)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_VIDEO_OUTPUT_NO_LONGER_EXISTS';
                                    HrDescr := 'The operating system asynchronously destroyed this OPM video output because the operating system''s state changed.' +
                                               'This error typically occurs because the monitor physical device object (PDO) associated with this video output was removed,' +
                                               'the monitor PDO associated with this video output was stopped, the video output''s session became a nonconsole session or the video output''s desktop became an inactive desktop.';
                                  end;
    LongInt($C026251A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_OPM_SESSION_TYPE_CHANGE_IN_PROGRESS';
                                    HrDescr := 'IOPMVideoOutput''s methods cannot be called when a session is changing its type.' +
                                               'There are currently three types of sessions: console, disconnected and remote (remote desktop protocol [RDP] or Independent Computing Architecture [ICA]).';
                                  end;
    LongInt($C0262580)          : begin
                                    HrStr := 'ERROR_GRAPHICS_I2C_NOT_SUPPORTED';
                                    HrDescr := 'The monitor connected to the specified video output does not have an I2C bus.';
                                  end;
    LongInt($C0262581)          : begin
                                    HrStr := 'ERROR_GRAPHICS_I2C_DEVICE_DOES_NOT_EXIST';
                                    HrDescr := 'No device on the I2C bus has the specified address.';
                                  end;
    LongInt($C0262582)          : begin
                                    HrStr := 'ERROR_GRAPHICS_I2C_ERROR_TRANSMITTING_DATA';
                                    HrDescr := 'An error occurred while transmitting data to the device on the I2C bus.';
                                  end;
    LongInt($C0262583)          : begin
                                    HrStr := 'ERROR_GRAPHICS_I2C_ERROR_RECEIVING_DATA';
                                    HrDescr := 'An error occurred while receiving data from the device on the I2C bus.';
                                  end;
    LongInt($C0262584)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DDCCI_VCP_NOT_SUPPORTED';
                                    HrDescr := 'The monitor does not support the specified Virtual Control Panel (VCP) code.';
                                  end;
    LongInt($C0262585)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DDCCI_INVALID_DATA';
                                    HrDescr := 'The data received from the monitor is invalid.';
                                  end;
    LongInt($C0262586)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DDCCI_MONITOR_RETURNED_INVALID_TIMING_STATUS_BYTE';
                                    HrDescr := 'A function call failed because a monitor returned an invalid Timing Status byte when the operating system used the Display Data Channel Command Interface (DDC/CI) Get Timing Report and Timing Message command to get a timing report from a monitor.';
                                  end;
    LongInt($C0262587)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_INVALID_CAPABILITIES_STRING';
                                    HrDescr := 'The monitor returned a DDC/CI capabilities string that did not comply with the ACCESS.bus 3.0, DDC/CI 1.1 or MCCS 2 Revision 1 specification.';
                                  end;
    LongInt($C0262588)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_INTERNAL_ERROR';
                                    HrDescr := 'An internal Monitor Configuration API error occurred.';
                                  end;
    LongInt($C0262589)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DDCCI_INVALID_MESSAGE_COMMAND';
                                    HrDescr := 'An operation failed because a DDC/CI message had an invalid value in its command field.';
                                  end;
    LongInt($C026258A)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DDCCI_INVALID_MESSAGE_LENGTH';
                                    HrDescr := 'This error occurred because a DDC/CI message length field contained an invalid value.';
                                  end;
    LongInt($C026258B)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DDCCI_INVALID_MESSAGE_CHECKSUM';
                                    HrDescr := 'This error occurred because the value in a DDC/CI message checksum field did not match the message''s computed checksum value.' +
                                               'This error implies that the data was corrupted while it was being transmitted from a monitor to a computer.';
                                  end;
    LongInt($C02625D6)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PMEA_INVALID_MONITOR';
                                    HrDescr := 'The HMONITOR no longer exists, is not attached to the desktop, or corresponds to a mirroring device.';
                                  end;
    LongInt($C02625D7)          : begin
                                    HrStr := 'ERROR_GRAPHICS_PMEA_INVALID_D3D_DEVICE';
                                    HrDescr := 'The Direct3D (D3D) device''s GDI display device no longer exists, is not attached to the desktop, or is a mirroring display device.';
                                  end;
    LongInt($C02625D8)          : begin
                                    HrStr := 'ERROR_GRAPHICS_DDCCI_CURRENT_CURRENT_VALUE_GREATER_THAN_MAXIMUM_VALUE';
                                    HrDescr := 'A continuous VCP code''s current value is greater than its maximum value.' +
                                               'This error code indicates that a monitor returned an invalid value.';
                                  end;
    LongInt($C02625D9)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_INVALID_VCP_VERSION';
                                    HrDescr := 'The monitor''s VCP Version ($DF) VCP code returned an invalid version value.';
                                  end;
    LongInt($C02625DA)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_MONITOR_VIOLATES_MCCS_SPECIFICATION';
                                    HrDescr := 'The monitor does not comply with the Monitor Control Command Set (MCCS) specification it claims to support.';
                                  end;
    LongInt($C02625DB)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_MCCS_VERSION_MISMATCH';
                                    HrDescr := 'The MCCS version in a monitor''s mccs_ver capability does not match the MCCS version the monitor reports when the VCP Version ($DF) VCP code is used.';
                                  end;
    LongInt($C02625DC)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_UNSUPPORTED_MCCS_VERSION';
                                    HrDescr := 'The Monitor Configuration API only works with monitors that support the MCCS 1.0 specification, the MCCS 2.0 specification, or the MCCS 2.0 Revision 1 specification.';
                                  end;
    LongInt($C02625DE)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_INVALID_TECHNOLOGY_TYPE_RETURNED';
                                    HrDescr := 'The monitor returned an invalid monitor technology type.' +
                                               'CRT, plasma, and LCD (TFT) are examples of monitor technology types.' +
                                               'This error implies that the monitor violated the MCCS 2.0 or MCCS 2.0 Revision 1 specification.';
                                  end;
    LongInt($C02625DF)          : begin
                                    HrStr := 'ERROR_GRAPHICS_MCA_UNSUPPORTED_COLOR_TEMPERATURE';
                                    HrDescr := 'The SetMonitorColorTemperature() caller passed a color temperature to it that the current monitor did not support.' +
                                               'CRT, plasma, and LCD (TFT) are examples of monitor technology types.' +
                                               'This error implies that the monitor violated the MCCS 2.0 or MCCS 2.0 Revision 1 specification.';
                                  end;
    LongInt($C02625E0)          : begin
                                    HrStr := 'ERROR_GRAPHICS_ONLY_CONSOLE_SESSION_SUPPORTED';
                                    HrDescr := 'This function can be used only if a program is running in the local console session.' +
                                               'It cannot be used if the program is running on a remote desktop session or on a terminal server session.';
                                  end;
    else
      begin
        HrStr := 'Unknown identifier.';
        HrDescr := 'Unknown HResult code.';
        HeaderFile := 'Unknown.';
        FWinHResultCracker.ClearResults();
        hr := ERROR_NOT_FOUND;
      end;
  end;
  Result := hr;
end;


function GetHrRegion(aHResult: HResult;
                     out aRegion: string): HResult;
var
  hr: HResult;
  li: LongInt;
  s: string;
begin
  hr := S_OK;

  // Get the regioncode
  s := IntToHex(aHResult, 8);
  s := '$' + RightStr(s, 4);
  li := StrToInt(s);

  // Find the region.
  if (li <= 499) then
    aRegion := 'General error.'
  else if (li >= 500) and (li <= 999) then
    aRegion := 'Sub-system error.'
  else if (li >= 1000) and (li <= 1299) then
    aRegion := 'Stack error.'
  else if (li >= 1300) and (li <= 1699) then
    aRegion := 'Assigned error.'
  else if (li >= 1700) and (li <= 3999) then
    aRegion := 'String binding error.'
  else if (li >= 4000) and (li <= 5999) then
    aRegion := 'Windows internal error.'
  else if (li >= 6000) and (li <= 8999) then
    aRegion := 'Encryption error.'
  else if (li >= 8200) and (li <= 8199) then
    aRegion := 'Directory service error.'
  else if (li >= 9000) and (li <= 11999) then
    aRegion := 'DNS error.'
  else if (li >= 12000) and (li <= 12175) then
    aRegion := 'Internet error.'
  else if (li >= 14000) and (li <= 14999) then
    aRegion := 'Graphics error.'
  else
    begin
      hr := ERROR_NOT_FOUND;
      aRegion := 'Unknown Region error.';
    end;
  Result := hr;
end;

end.

