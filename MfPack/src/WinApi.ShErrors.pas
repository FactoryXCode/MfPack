// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.ShErrors.pas
// Kind: Pascal / Delphi unit
// Release date: 24-07-2025
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Shell API error code values.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), Ciaran (Ciaran3)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/04/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: When using MfPack, use this unit and not the outdated
//          Errors code definitions from Embarcadero Delphi <= ver 10.4
//
// Related objects: -
// Related projects: MfPackX319
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.26100.4654
//
// Todo: -
//
//==============================================================================
// Source: WinError.h
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
unit WinApi.ShErrors;

  {$HPPEMIT '#include "sherrors.h"'}
  {$HPPEMIT '#include "winerror.h"'}

interface

uses
  WinApi.WinError;

const

  //  COPYENGINE codes use FACILITY_SHELL and 0 in the second lowest byte
  // Success/Informational codes
  COPYENGINE_S_YES                    = HRESULT($00270001);
  {$EXTERNALSYM COPYENGINE_S_YES}
  COPYENGINE_S_NOT_HANDLED            = HRESULT($00270003);
  {$EXTERNALSYM COPYENGINE_S_NOT_HANDLED}
  COPYENGINE_S_USER_RETRY             = HRESULT($00270004);
  {$EXTERNALSYM COPYENGINE_S_USER_RETRY}
  COPYENGINE_S_USER_IGNORED           = HRESULT($00270005);
  {$EXTERNALSYM COPYENGINE_S_USER_IGNORED}
  COPYENGINE_S_MERGE                  = HRESULT($00270006);
  {$EXTERNALSYM COPYENGINE_S_MERGE}
  COPYENGINE_S_DONT_PROCESS_CHILDREN  = HRESULT($00270008);
  {$EXTERNALSYM COPYENGINE_S_DONT_PROCESS_CHILDREN}
  COPYENGINE_S_ALREADY_DONE           = HRESULT($0027000A);
  {$EXTERNALSYM COPYENGINE_S_ALREADY_DONE}
  COPYENGINE_S_PENDING                = HRESULT($0027000B);
  {$EXTERNALSYM COPYENGINE_S_PENDING}
  COPYENGINE_S_KEEP_BOTH              = HRESULT($0027000C);
  {$EXTERNALSYM COPYENGINE_S_KEEP_BOTH}
  COPYENGINE_S_CLOSE_PROGRAM          = HRESULT($0027000D);  // Close the program using the current file
  {$EXTERNALSYM COPYENGINE_S_CLOSE_PROGRAM}
  COPYENGINE_S_COLLISIONRESOLVED      = HRESULT($0027000E);  // Returned by a transfer source when a collision occurs
  {$EXTERNALSYM COPYENGINE_S_COLLISIONRESOLVED}                         // during a file operation but the source resolved the collision
                                                                        // on the users behalf.

  COPYENGINE_S_PROGRESS_PAUSE         = HRESULT($0027000F);
  {$EXTERNALSYM COPYENGINE_S_PENDING_DELETE}
  COPYENGINE_S_PENDING_DELETE         = HRESULT($00270010);  // Returned by initiating async delete operation. It's different
  {$EXTERNALSYM COPYENGINE_S_PROGRESS_PAUSE}                            // from COPYENGINE_S_PENDING which is used for operation queued due to error.
  COPYENGINE_S_PENDING_BATCH_COPY     = HRESULT($00270011);  // Returned by initiating batch copy operation. Like COPYENGINE_S_PENDING_DELETE,
  {$EXTERNALSYM COPYENGINE_S_PENDING_BATCH_COPY}                        // it indicates a non-error queued retry of the operation.

  // Failure/Error codes
  COPYENGINE_E_USER_CANCELLED         = HRESULT($80270000);  // User wants to canceled entire job
  {$EXTERNALSYM COPYENGINE_E_USER_CANCELLED}
  COPYENGINE_E_CANCELLED              = HRESULT($80270001);  // Engine wants to canceled entire job, don't set the CANCELLED bit
  {$EXTERNALSYM COPYENGINE_E_CANCELLED}
  COPYENGINE_E_REQUIRES_ELEVATION     = HRESULT($80270002);  // Need to elevate the process to complete the operation
  {$EXTERNALSYM COPYENGINE_E_REQUIRES_ELEVATION}

  COPYENGINE_E_SAME_FILE              = HRESULT($80270003);  // Source and destination file are the same
  {$EXTERNALSYM COPYENGINE_E_SAME_FILE}
  COPYENGINE_E_DIFF_DIR               = HRESULT($80270004);  // Trying to rename a file into a different location, use move instead
  {$EXTERNALSYM COPYENGINE_E_DIFF_DIR}
  COPYENGINE_E_MANY_SRC_1_DEST        = HRESULT($80270005);  // One source specified, multiple destinations
  {$EXTERNALSYM COPYENGINE_E_MANY_SRC_1_DEST}

  COPYENGINE_E_DEST_SUBTREE           = HRESULT($80270009);  // The destination is a sub-tree of the source
  {$EXTERNALSYM COPYENGINE_E_DEST_SUBTREE}
  COPYENGINE_E_DEST_SAME_TREE         = HRESULT($8027000A);  // The destination is the same folder as the source
  {$EXTERNALSYM COPYENGINE_E_DEST_SAME_TREE}

  COPYENGINE_E_FLD_IS_FILE_DEST       = HRESULT($8027000B);  // Existing destination file with same name as folder
  {$EXTERNALSYM COPYENGINE_E_FLD_IS_FILE_DEST}
  COPYENGINE_E_FILE_IS_FLD_DEST       = HRESULT($8027000C);  // Existing destination folder with same name as file
  {$EXTERNALSYM COPYENGINE_E_FILE_IS_FLD_DEST}

  COPYENGINE_E_FILE_TOO_LARGE         = HRESULT($8027000D);  // File too large for destination file system
  {$EXTERNALSYM COPYENGINE_E_FILE_TOO_LARGE}
  COPYENGINE_E_REMOVABLE_FULL         = HRESULT($8027000E);  // Destination device is full and happens to be removable
  {$EXTERNALSYM COPYENGINE_E_REMOVABLE_FULL}

  COPYENGINE_E_DEST_IS_RO_CD          = HRESULT($8027000F);  // Destination is a Read-Only CDRom, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_DEST_IS_RO_CD}
  COPYENGINE_E_DEST_IS_RW_CD          = HRESULT($80270010);  // Destination is a Read/Write CDRom, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_DEST_IS_RW_CD}
  COPYENGINE_E_DEST_IS_R_CD           = HRESULT($80270011);  // Destination is a Recordable (AudioL) CDRom, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_DEST_IS_R_CD}

  COPYENGINE_E_DEST_IS_RO_DVD         = HRESULT($80270012);  // Destination is a Read-Only DVD, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_DEST_IS_RO_DVD}
  COPYENGINE_E_DEST_IS_RW_DVD         = HRESULT($80270013);  // Destination is a Read/Wrote DVD, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_DEST_IS_RW_DVD}
  COPYENGINE_E_DEST_IS_R_DVD          = HRESULT($80270014);  // Destination is a Recordable (AudioL) DVD, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_DEST_IS_R_DVD}

  COPYENGINE_E_SRC_IS_RO_CD           = HRESULT($80270015);  // Source is a Read-Only CDRom, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_SRC_IS_RO_CD}
  COPYENGINE_E_SRC_IS_RW_CD           = HRESULT($80270016);  // Source is a Read/Write CDRom, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_SRC_IS_RW_CD}
  COPYENGINE_E_SRC_IS_R_CD            = HRESULT($80270017);  // Source is a Recordable (AudioL) CDRom, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_SRC_IS_R_CD}

  COPYENGINE_E_SRC_IS_RO_DVD          = HRESULT($80270018);  // Source is a Read-Only DVD, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_SRC_IS_RO_DVD}
  COPYENGINE_E_SRC_IS_RW_DVD          = HRESULT($80270019);  // Source is a Read/Wrote DVD, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_SRC_IS_RW_DVD}
  COPYENGINE_E_SRC_IS_R_DVD           = HRESULT($8027001A);  // Source is a Recordable (AudioL) DVD, possibly unformatted
  {$EXTERNALSYM COPYENGINE_E_SRC_IS_R_DVD}

  COPYENGINE_E_INVALID_FILES_SRC      = HRESULT($8027001B);  // Invalid source path
  {$EXTERNALSYM COPYENGINE_E_INVALID_FILES_SRC}
  COPYENGINE_E_INVALID_FILES_DEST     = HRESULT($8027001C);  // Invalid destination path
  {$EXTERNALSYM COPYENGINE_E_INVALID_FILES_DEST}
  COPYENGINE_E_PATH_TOO_DEEP_SRC      = HRESULT($8027001D);  // Source Files within folders where the overall path is longer than MAX_PATH
  {$EXTERNALSYM COPYENGINE_E_PATH_TOO_DEEP_SRC}
  COPYENGINE_E_PATH_TOO_DEEP_DEST     = HRESULT($8027001E);  // Destination files would be within folders where the overall path is longer than MAX_PATH
  {$EXTERNALSYM COPYENGINE_E_PATH_TOO_DEEP_DEST}
  COPYENGINE_E_ROOT_DIR_SRC           = HRESULT($8027001F);  // Source is a root directory, cannot be moved or renamed
  {$EXTERNALSYM COPYENGINE_E_ROOT_DIR_SRC}
  COPYENGINE_E_ROOT_DIR_DEST          = HRESULT($80270020);  // Destination is a root directory, cannot be renamed
  {$EXTERNALSYM COPYENGINE_E_ROOT_DIR_DEST}
  COPYENGINE_E_ACCESS_DENIED_SRC      = HRESULT($80270021);  // Security problem on source
  {$EXTERNALSYM COPYENGINE_E_ACCESS_DENIED_SRC}
  COPYENGINE_E_ACCESS_DENIED_DEST     = HRESULT($80270022);  // Security problem on destination
  {$EXTERNALSYM COPYENGINE_E_ACCESS_DENIED_DEST}
  COPYENGINE_E_PATH_NOT_FOUND_SRC     = HRESULT($80270023);  // Source file does not exist, or is unavailable
  {$EXTERNALSYM COPYENGINE_E_PATH_NOT_FOUND_SRC}
  COPYENGINE_E_PATH_NOT_FOUND_DEST    = HRESULT($80270024);  // Destination file does not exist, or is unavailable
  {$EXTERNALSYM COPYENGINE_E_PATH_NOT_FOUND_DEST}
  COPYENGINE_E_NET_DISCONNECT_SRC     = HRESULT($80270025);  // Source file is on a disconnected network location
  {$EXTERNALSYM COPYENGINE_E_NET_DISCONNECT_SRC}
  COPYENGINE_E_NET_DISCONNECT_DEST    = HRESULT($80270026);  // Destination file is on a disconnected network location
  {$EXTERNALSYM COPYENGINE_E_NET_DISCONNECT_DEST}
  COPYENGINE_E_SHARING_VIOLATION_SRC  = HRESULT($80270027);  // Sharing Violation on source
  {$EXTERNALSYM COPYENGINE_E_SHARING_VIOLATION_SRC}
  COPYENGINE_E_SHARING_VIOLATION_DEST = HRESULT($80270028);  // Sharing Violation on destination
  {$EXTERNALSYM COPYENGINE_E_SHARING_VIOLATION_DEST}

  COPYENGINE_E_ALREADY_EXISTS_NORMAL  = HRESULT($80270029);  // Destination exists, cannot replace
  {$EXTERNALSYM COPYENGINE_E_ALREADY_EXISTS_NORMAL}
  COPYENGINE_E_ALREADY_EXISTS_READONLY= HRESULT($8027002A);  // Destination with read-only attribute exists, cannot replace
  {$EXTERNALSYM COPYENGINE_E_ALREADY_EXISTS_READONLY}
  COPYENGINE_E_ALREADY_EXISTS_SYSTEM  = HRESULT($8027002B);  // Destination with system attribute exists, cannot replace
  {$EXTERNALSYM COPYENGINE_E_ALREADY_EXISTS_SYSTEM}
  COPYENGINE_E_ALREADY_EXISTS_FOLDER  = HRESULT($8027002C);  // Destination folder exists, cannot replace
  {$EXTERNALSYM COPYENGINE_E_ALREADY_EXISTS_FOLDER}
  COPYENGINE_E_STREAM_LOSS            = HRESULT($8027002D);  // Secondary Stream information would be lost
  {$EXTERNALSYM COPYENGINE_E_STREAM_LOSS}
  COPYENGINE_E_EA_LOSS                = HRESULT($8027002E);  // Extended Attributes would be lost
  {$EXTERNALSYM COPYENGINE_E_EA_LOSS}
  COPYENGINE_E_PROPERTY_LOSS          = HRESULT($8027002F);  // Property would be lost
  {$EXTERNALSYM COPYENGINE_E_PROPERTY_LOSS}
  COPYENGINE_E_PROPERTIES_LOSS        = HRESULT($80270030);  // Properties would be lost
  {$EXTERNALSYM COPYENGINE_E_PROPERTIES_LOSS}
  COPYENGINE_E_ENCRYPTION_LOSS        = HRESULT($80270031);  // Encryption would be lost
  {$EXTERNALSYM COPYENGINE_E_ENCRYPTION_LOSS}
  COPYENGINE_E_DISK_FULL              = HRESULT($80270032);  // Entire operation likely won't fit
  {$EXTERNALSYM COPYENGINE_E_DISK_FULL}
  COPYENGINE_E_DISK_FULL_CLEAN        = HRESULT($80270033);  // Entire operation likely won't fit, clean-up wizard available
  {$EXTERNALSYM COPYENGINE_E_DISK_FULL_CLEAN}
  COPYENGINE_E_EA_NOT_SUPPORTED       = HRESULT($80270034);  // Volume does not support Extended Attributes
  {$EXTERNALSYM COPYENGINE_E_EA_NOT_SUPPORTED}
  COPYENGINE_E_CANT_REACH_SOURCE      = HRESULT($80270035);  // Can't reach source folder
  {$EXTERNALSYM COPYENGINE_E_CANT_REACH_SOURCE}

  COPYENGINE_E_RECYCLE_UNKNOWN_ERROR  = HRESULT($80270035);  // ???
  {$EXTERNALSYM COPYENGINE_E_RECYCLE_UNKNOWN_ERROR}
  COPYENGINE_E_RECYCLE_FORCE_NUKE     = HRESULT($80270036);  // Recycling not available (usually turned offL)
  {$EXTERNALSYM COPYENGINE_E_RECYCLE_FORCE_NUKE}
  COPYENGINE_E_RECYCLE_SIZE_TOO_BIG   = HRESULT($80270037);  // Item is too large for the recycle-bin
  {$EXTERNALSYM COPYENGINE_E_RECYCLE_SIZE_TOO_BIG}
  COPYENGINE_E_RECYCLE_PATH_TOO_LONG  = HRESULT($80270038);  // Folder is too deep to fit in the recycle-bin
  {$EXTERNALSYM COPYENGINE_E_RECYCLE_PATH_TOO_LONG}
  COPYENGINE_E_RECYCLE_BIN_NOT_FOUND  = HRESULT($8027003A);  // Recycle bin could not be found or is unavailable
  {$EXTERNALSYM COPYENGINE_E_RECYCLE_BIN_NOT_FOUND}
  COPYENGINE_E_NEWFILE_NAME_TOO_LONG  = HRESULT($8027003B);  // Name of the new file being created is too long
  {$EXTERNALSYM COPYENGINE_E_NEWFILE_NAME_TOO_LONG}
  COPYENGINE_E_NEWFOLDER_NAME_TOO_LONG= HRESULT($8027003C);  // Name of the new folder being created is too long
  {$EXTERNALSYM COPYENGINE_E_NEWFOLDER_NAME_TOO_LONG}
  COPYENGINE_E_DIR_NOT_EMPTY          = HRESULT($8027003D);  // The directory being processed is not empty
  {$EXTERNALSYM COPYENGINE_E_DIR_NOT_EMPTY}
  COPYENGINE_E_FAT_MAX_IN_ROOT        = HRESULT($8027003E);  // A Fat drive cannot only store and rename a limited number of items on the root.
  {$EXTERNALSYM COPYENGINE_E_FAT_MAX_IN_ROOT}
  COPYENGINE_E_ACCESSDENIED_READONLY  = HRESULT($8027003F);  // The item cannot be modified because it is set to readonly.
  {$EXTERNALSYM COPYENGINE_E_ACCESSDENIED_READONLY}

  COPYENGINE_E_REDIRECTED_TO_WEBPAGE  = HRESULT($80270040);  // The server redirected the download request to a web page.
  {$EXTERNALSYM COPYENGINE_E_REDIRECTED_TO_WEBPAGE}
  COPYENGINE_E_SERVER_BAD_FILE_TYPE   = HRESULT($80270041);  // The server returned data with an unexpected MIME type or extension.
  {$EXTERNALSYM COPYENGINE_E_SERVER_BAD_FILE_TYPE}

  COPYENGINE_E_INTERNET_ITEM_UNAVAILABLE    = HRESULT($80270042);  // The item is unavailable currently due to no internet connectivity
  {$EXTERNALSYM COPYENGINE_E_INTERNET_ITEM_UNAVAILABLE}

  COPYENGINE_E_CANNOT_MOVE_FROM_RECYCLE_BIN = HRESULT($80270043);  // The item cannot be moved out of the recycle bin, can perform other operations like restored/delete
  {$EXTERNALSYM COPYENGINE_E_CANNOT_MOVE_FROM_RECYCLE_BIN}

  COPYENGINE_E_CANNOT_MOVE_SHARED_FOLDER    = HRESULT($80270044);  // The item cannot be moved from a grouped folder to another shared folder
  {$EXTERNALSYM COPYENGINE_E_CANNOT_MOVE_SHARED_FOLDER}

  COPYENGINE_E_INTERNET_ITEM_STORAGE_PROVIDER_ERROR  = HRESULT($80270045);  // The item is unavailable currently due to storage provider being in error state
  {$EXTERNALSYM COPYENGINE_E_INTERNET_ITEM_STORAGE_PROVIDER_ERROR}
  COPYENGINE_E_INTERNET_ITEM_STORAGE_PROVIDER_PAUSED = HRESULT($80270046);  // The item is unavailable currently due to storage provider being paused
  {$EXTERNALSYM COPYENGINE_E_INTERNET_ITEM_STORAGE_PROVIDER_PAUSED}

  COPYENGINE_E_REQUIRES_EDP_CONSENT   = HRESULT($80270047);  // The file can only be copied if the user consents to override the EDP block
  {$EXTERNALSYM COPYENGINE_E_REQUIRES_EDP_CONSENT}
  COPYENGINE_E_BLOCKED_BY_EDP_POLICY  = HRESULT($80270048);  // The file cannot be copied per EDP policy
  {$EXTERNALSYM COPYENGINE_E_BLOCKED_BY_EDP_POLICY}
  COPYENGINE_E_REQUIRES_EDP_CONSENT_FOR_REMOVABLE_DRIVE     = HRESULT($80270049);  // The file can be copied as personal if the user consents to override the EDP block
  {$EXTERNALSYM COPYENGINE_E_REQUIRES_EDP_CONSENT_FOR_REMOVABLE_DRIVE}
  COPYENGINE_E_BLOCKED_BY_EDP_FOR_REMOVABLE_DRIVE           = HRESULT($8027004A);  // The file can only be copied as work protected per EDP policy
  {$EXTERNALSYM COPYENGINE_E_BLOCKED_BY_EDP_FOR_REMOVABLE_DRIVE}
  COPYENGINE_E_RMS_REQUIRES_EDP_CONSENT_FOR_REMOVABLE_DRIVE = HRESULT($8027004B);  // The file can be copied as personal if the user consents to override the EDP block
  {$EXTERNALSYM COPYENGINE_E_RMS_REQUIRES_EDP_CONSENT_FOR_REMOVABLE_DRIVE}
  COPYENGINE_E_RMS_BLOCKED_BY_EDP_FOR_REMOVABLE_DRIVE       = HRESULT($8027004C);  // The file can only be copied as work protected per EDP policy
  {$EXTERNALSYM COPYENGINE_E_RMS_BLOCKED_BY_EDP_FOR_REMOVABLE_DRIVE}

  COPYENGINE_E_WARNED_BY_DLP_POLICY      = HRESULT($8027004D);  // The file is warned against being copied per DLP policy
  {$EXTERNALSYM COPYENGINE_E_WARNED_BY_DLP_POLICY}
  COPYENGINE_E_BLOCKED_BY_DLP_POLICY     = HRESULT($8027004E);  // The file cannot be copied per DLP policy
  {$EXTERNALSYM COPYENGINE_E_BLOCKED_BY_DLP_POLICY}
  COPYENGINE_E_SILENT_FAIL_BY_DLP_POLICY = HRESULT($8027004F);  // The file cannot be copied per DLP policy, and the caller is requested to silently fail
  {$EXTERNALSYM COPYENGINE_E_SILENT_FAIL_BY_DLP_POLICY}

  COPYENGINE_E_SUPPRESS_DIALOG        = HRESULT($80270050);  // Returned by copy engine to indicate that the shell dialog should not be shown in favor to the cloud provider's or the app's dialog
  {$EXTERNALSYM COPYENGINE_E_SUPPRESS_DIALOG}

  // error codes without a more specific group use FACILITY_SHELL and 0x01 in the second lowest byte.
  NETCACHE_E_NEGATIVE_CACHE           = HRESULT($80270100);  // The item requested is in the negative net parsing cache
  {$EXTERNALSYM NETCACHE_E_NEGATIVE_CACHE}
  EXECUTE_E_LAUNCH_APPLICATION        = HRESULT($80270101);  // for returned by command delegates to indicate that they did no work
  {$EXTERNALSYM EXECUTE_E_LAUNCH_APPLICATION}
  SHELL_E_WRONG_BITDEPTH              = HRESULT($80270102);  // returned when trying to create a thumbnail extractor at too low a bitdepth for high fidelity
  {$EXTERNALSYM SHELL_E_WRONG_BITDEPTH}
  LINK_E_DELETE                       = HRESULT($80270103);  // returned from IShellLink::Resolve when SLR_OFFER_DELETE_WITHOUT_FILE is passed and the user requested to delete the item
  {$EXTERNALSYM LINK_E_DELETE}
  STORE_E_NEWER_VERSION_AVAILABLE     = HRESULT($80270104);  // returned from IAppItemsStateModify to indicate a commit failed because there is newer version already available
  {$EXTERNALSYM STORE_E_NEWER_VERSION_AVAILABLE}

  // File Placeholder success and error codes
  E_FILE_PLACEHOLDER_NOT_INITIALIZED  = HRESULT($80270110);  // The placeholder file or property store isn't initialized.
  {$EXTERNALSYM E_FILE_PLACEHOLDER_NOT_INITIALIZED}
  E_FILE_PLACEHOLDER_VERSION_MISMATCH = HRESULT($80270111);  // The sync engine detected that the local placeholder file's version doesn't match the latest version.
  {$EXTERNALSYM E_FILE_PLACEHOLDER_VERSION_MISMATCH}
  E_FILE_PLACEHOLDER_SERVER_TIMED_OUT = HRESULT($80270112);  // The placeholder platform timed out waiting for a stream resolver call that didn't complete in time.
  {$EXTERNALSYM E_FILE_PLACEHOLDER_SERVER_TIMED_OUT}
  E_FILE_PLACEHOLDER_STORAGEPROVIDER_NOT_FOUND = HRESULT($80270113);  // The placeholder platform cannot find the storage provider for the placeholder.
  {$EXTERNALSYM E_FILE_PLACEHOLDER_STORAGEPROVIDER_NOT_FOUND}

  // Camera Roll error codes
  {$EXTERNALSYM CAMERAROLL_E_NO_DOWNSAMPLING_REQUIRED}
  CAMERAROLL_E_NO_DOWNSAMPLING_REQUIRED= HRESULT($80270120);  // The provided image didn't require downsampling because it was small enough already

  // Error codes for when Shell denies a WinRT app activation request via Window Manager/View Manager.
  // These should be considered subsets of NAV_E_SHELLVALIDATIONFAILED.
  // By the time the activation request reached shell, the scenario was already over and the activation is intentionally denied (not an error).
  E_ACTIVATIONDENIED_USERCLOSE        = HRESULT($80270130);
  {$EXTERNALSYM E_ACTIVATIONDENIED_USERCLOSE}
  // The shell hit a critical error while handling the activation request and cannot process it correctly, so the activation must be denied.
  E_ACTIVATIONDENIED_SHELLERROR       = HRESULT($80270131);
  {$EXTERNALSYM E_ACTIVATIONDENIED_SHELLERROR}
  // The shell restarted while the activation was in-progress, and it is unable to process it properly.
  E_ACTIVATIONDENIED_SHELLRESTART     = HRESULT($80270132);
  {$EXTERNALSYM E_ACTIVATIONDENIED_SHELLRESTART}
  // Placeholder for an unexpected error that we cannot definitively map into one of the above buckets. Not expected to fire in practice.
  E_ACTIVATIONDENIED_UNEXPECTED       = HRESULT($80270133);
  {$EXTERNALSYM E_ACTIVATIONDENIED_UNEXPECTED}
  // The shell was not ready when the activation started, so the activation was aborted immediately.
  E_ACTIVATIONDENIED_SHELLNOTREADY    = HRESULT($80270134);
  {$EXTERNALSYM E_ACTIVATIONDENIED_SHELLNOTREADY}

  // Library error/failure code
  LIBRARY_E_NO_SAVE_LOCATION          = HRESULT($80270200);
  {$EXTERNALSYM LIBRARY_E_NO_SAVE_LOCATION}
  LIBRARY_E_NO_ACCESSIBLE_LOCATION    = HRESULT($80270201);
  {$EXTERNALSYM LIBRARY_E_NO_ACCESSIBLE_LOCATION}


  // User Tile error/failure codes
  E_USERTILE_UNSUPPORTEDFILETYPE   =   HRESULT($80270210);

  // User Tile error codes that map to the WinRT enum Windows.System.UserProfile.SetAccountPictureResult
  E_USERTILE_CHANGEDISABLED           = HRESULT($80270211);
  {$EXTERNALSYM E_USERTILE_CHANGEDISABLED}
  E_USERTILE_LARGEORDYNAMIC           = HRESULT($80270212);
  {$EXTERNALSYM E_USERTILE_LARGEORDYNAMIC}
  E_USERTILE_VIDEOFRAMESIZE           = HRESULT($80270213);
  {$EXTERNALSYM E_USERTILE_VIDEOFRAMESIZE}
  E_USERTILE_FILESIZE                 = HRESULT($80270214);
  {$EXTERNALSYM E_USERTILE_FILESIZE}


  // Immersive Accessibility Docking Service Error Codes
  IMM_ACC_DOCKING_E_INSUFFICIENTHEIGHT= HRESULT($80270230);
  {$EXTERNALSYM IMM_ACC_DOCKING_E_INSUFFICIENTHEIGHT}
  IMM_ACC_DOCKING_E_DOCKOCCUPIED         = HRESULT($80270231);
  {$EXTERNALSYM IMM_ACC_DOCKING_E_DOCKOCCUPIED}

  // Immersive Shell Startup Failure Code
  IMSC_E_SHELL_COMPONENT_STARTUP_FAILURE = HRESULT($80270233);  // An immersive shell component failed to start correctly
  {$EXTERNALSYM IMSC_E_SHELL_COMPONENT_STARTUP_FAILURE}

  // Shell Service Host Startup Failure Code
  SHC_E_SHELL_COMPONENT_STARTUP_FAILURE  = HRESULT($80270234);  // A shell service host component failed to start correctly
  {$EXTERNALSYM SHC_E_SHELL_COMPONENT_STARTUP_FAILURE}

  // Tile notifications - Failure to initialize the notification platform
  E_TILE_NOTIFICATIONS_PLATFORM_FAILURE  = HRESULT($80270249);
  {$EXTERNALSYM E_TILE_NOTIFICATIONS_PLATFORM_FAILURE}

  E_SHELL_EXTENSION_BLOCKED              = HRESULT($80270301);
  {$EXTERNALSYM E_SHELL_EXTENSION_BLOCKED}

  E_IMAGEFEED_CHANGEDISABLED             = HRESULT($80270310);
  {$EXTERNALSYM E_IMAGEFEED_CHANGEDISABLED}


  // Additional Prototypes for ALL interfaces

  // end of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
