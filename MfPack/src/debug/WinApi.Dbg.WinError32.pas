//
// Copyright: © FactoryX. All rights reserved.
//
// Project: WinApi - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Dbg.WinError32
// Kind: Pascal / Delphi unit
// Release date: 09-07-2023
// Language: ENU
//
// Revision Version: 3.1.5
// Description: Returns code definitions of the WinError32 errors.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 09/07/2023 All                 Carmel release  SDK 10.0.22621.0 (Windows 11)
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
unit WinApi.Dbg.WinError32;

interface

uses
  WinApi.Windows,
  WinApi.WinError,
  WinApi.Dbg.WinHResultTools;


  function GetErr32Description(const aHResult: HResult;
                               out hrStr: string;
                               out hrDescr: string;
                               out RegionDescr: string;
                               out HeaderFile: string;
                               out Reference: TReferenceArray): HResult;

  function GetErr32Region(aHResult: HResult;
                          out aRegion: string): HResult;


implementation

uses
  System.SysUtils,
  System.StrUtils;


function GetErr32Description(const aHResult: HResult;
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
  Reference[0] := 'https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes';
  Reference[1] := 'https://learn.microsoft.com/en-us/windows/win32/api/winerror';

  case aHResult of
    LongInt($00000000)  : begin
                            HrStr := 'ERROR_SUCCESS' ;
                            HrDescr := 'The operation completed successfully.';
                          end;
    LongInt($00000001)  : begin
                            HrStr := 'ERROR_INVALID_FUNCTION' ;
                            HrDescr := 'Incorrect function.';
                          end;
    LongInt($00000002)  : begin
                            HrStr := 'ERROR_FILE_NOT_FOUND' ;
                            HrDescr := 'The system cannot find the file specified.';
                          end;
    LongInt($00000003)  : begin
                            HrStr := 'ERROR_PATH_NOT_FOUND' ;
                            HrDescr := 'The system cannot find the path specified.';
                          end;
    LongInt($00000004)  : begin
                            HrStr := 'ERROR_TOO_MANY_OPEN_FILES' ;
                            HrDescr := 'The system cannot open the file.';
                          end;
    LongInt($00000005)  : begin
                            HrStr := 'ERROR_ACCESS_DENIED' ;
                            HrDescr := 'Access is denied.';
                          end;
    LongInt($00000006)  : begin
                            HrStr := 'ERROR_INVALID_HANDLE' ;
                            HrDescr := 'The handle is invalid.';
                          end;
    LongInt($00000007)  : begin
                            HrStr := 'ERROR_ARENA_TRASHED' ;
                            HrDescr := 'The storage control blocks were destroyed.';
                          end;
    LongInt($00000008)  : begin
                            HrStr := 'ERROR_NOT_ENOUGH_MEMORY' ;
                            HrDescr := 'Not enough storage is available to process this command.';
                          end;
    LongInt($00000009)  : begin
                            HrStr := 'ERROR_INVALID_BLOCK' ;
                            HrDescr := 'The storage control block address is invalid.';
                          end;
    LongInt($0000000A)  : begin
                            HrStr := 'ERROR_BAD_ENVIRONMENT' ;
                            HrDescr := 'The environment is incorrect.';
                          end;
    LongInt($0000000B)  : begin
                            HrStr := 'ERROR_BAD_FORMAT' ;
                            HrDescr := 'An attempt was made to load a program with an incorrect format.';
                          end;
    LongInt($0000000C)  : begin
                            HrStr := 'ERROR_INVALID_ACCESS' ;
                            HrDescr := 'The access code is invalid.';
                          end;
    LongInt($0000000D)  : begin
                            HrStr := 'ERROR_INVALID_DATA' ;
                            HrDescr := 'The data is invalid.';
                          end;
    LongInt($0000000E)  : begin
                            HrStr := 'ERROR_OUTOFMEMORY' ;
                            HrDescr := 'Not enough storage is available to complete this operation.';
                          end;
    LongInt($0000000F)  : begin
                            HrStr := 'ERROR_INVALID_DRIVE' ;
                            HrDescr := 'The system cannot find the drive specified.';
                          end;
    LongInt($00000010)  : begin
                            HrStr := 'ERROR_CURRENT_DIRECTORY' ;
                            HrDescr := 'The directory cannot be removed.';
                          end;
    LongInt($00000011)  : begin
                            HrStr := 'ERROR_NOT_SAME_DEVICE' ;
                            HrDescr := 'The system cannot move the file to a different disk drive.';
                          end;
    LongInt($00000012)  : begin
                            HrStr := 'ERROR_NO_MORE_FILES' ;
                            HrDescr := 'There are no more files.';
                          end;
    LongInt($00000013)  : begin
                            HrStr := 'ERROR_WRITE_PROTECT' ;
                            HrDescr := 'The media is write-protected.';
                          end;
    LongInt($00000014)  : begin
                            HrStr := 'ERROR_BAD_UNIT' ;
                            HrDescr := 'The system cannot find the device specified.';
                          end;
    LongInt($00000015)  : begin
                            HrStr := 'ERROR_NOT_READY' ;
                            HrDescr := 'The device is not ready.';
                          end;
    LongInt($00000016)  : begin
                            HrStr := 'ERROR_BAD_COMMAND' ;
                            HrDescr := 'The device does not recognize the command.';
                          end;
    LongInt($00000017)  : begin
                            HrStr := 'ERROR_CRC' ;
                            HrDescr := 'Data error (cyclic redundancy check).';
                          end;
    LongInt($00000018)  : begin
                            HrStr := 'ERROR_BAD_LENGTH' ;
                            HrDescr := 'The program issued a command but the command length is incorrect.';
                          end;
    LongInt($00000019)  : begin
                            HrStr := 'ERROR_SEEK' ;
                            HrDescr := 'The drive cannot locate a specific area or track on the disk.';
                          end;
    LongInt($0000001A)  : begin
                            HrStr := 'ERROR_NOT_DOS_DISK' ;
                            HrDescr := 'The specified disk cannot be accessed.';
                          end;
    LongInt($0000001B)  : begin
                            HrStr := 'ERROR_SECTOR_NOT_FOUND' ;
                            HrDescr := 'The drive cannot find the sector requested.';
                          end;
    LongInt($0000001C)  : begin
                            HrStr := 'ERROR_OUT_OF_PAPER' ;
                            HrDescr := 'The printer is out of paper.';
                          end;
    LongInt($0000001D)  : begin
                            HrStr := 'ERROR_WRITE_FAULT' ;
                            HrDescr := 'The system cannot write to the specified device.';
                          end;
    LongInt($0000001E)  : begin
                            HrStr := 'ERROR_READ_FAULT' ;
                            HrDescr := 'The system cannot read from the specified device.';
                          end;
    LongInt($0000001F)  : begin
                            HrStr := 'ERROR_GEN_FAILURE' ;
                            HrDescr := 'A device attached to the system is not functioning.';
                          end;
    LongInt($00000020)  : begin
                            HrStr := 'ERROR_SHARING_VIOLATION' ;
                            HrDescr := 'The process cannot access the file because it is being used by another process.';
                          end;
    LongInt($00000021)  : begin
                            HrStr := 'ERROR_LOCK_VIOLATION' ;
                            HrDescr := 'The process cannot access the file because another process has locked a portion of the file.';
                          end;
    LongInt($00000022)  : begin
                            HrStr := 'ERROR_WRONG_DISK' ;
                            HrDescr := 'The wrong disk is in the drive.' +
                                       'Insert %2 (Volume Serial Number: %3) into drive %1.';
                          end;
    LongInt($00000024)  : begin
                            HrStr := 'ERROR_SHARING_BUFFER_EXCEEDED' ;
                            HrDescr := 'Too many files opened for sharing.';
                          end;
    LongInt($00000026)  : begin
                            HrStr := 'ERROR_HANDLE_EOF' ;
                            HrDescr := 'Reached the end of the file.';
                          end;
    LongInt($00000027)  : begin
                            HrStr := 'ERROR_HANDLE_DISK_FULL' ;
                            HrDescr := 'The disk is full.';
                          end;
    LongInt($00000032)  : begin
                            HrStr := 'ERROR_NOT_SUPPORTED' ;
                            HrDescr := 'The request is not supported.';

                          end;
    LongInt($00000033)  : begin
                            HrStr := 'ERROR_REM_NOT_LIST' ;
                            HrDescr := 'Windows cannot find the network path.' +
                                       'Verify that the network path is correct and the destination computer is not busy or turned off.' +
                                       'If Windows still cannot find the network path, contact your network administrator.';
                          end;
    LongInt($00000034)  : begin
                            HrStr := 'ERROR_DUP_NAME' ;
                            HrDescr := 'You were not connected because a duplicate name exists on the network.' +
                                       'Go to System in Control Panel to change the computer name, and then try again.';
                          end;
    LongInt($00000035)  : begin
                            HrStr := 'ERROR_BAD_NETPATH' ;
                            HrDescr := 'The network path was not found.';
                          end;
    LongInt($00000036)  : begin
                            HrStr := 'ERROR_NETWORK_BUSY' ;
                            HrDescr := 'The network is busy.';
                          end;
    LongInt($00000037)  : begin
                            HrStr := 'ERROR_DEV_NOT_EXIST' ;
                            HrDescr := 'The specified network resource or device is no longer available.';
                          end;
    LongInt($00000038)  : begin
                            HrStr := 'ERROR_TOO_MANY_CMDS' ;
                            HrDescr := 'The network BIOS command limit has been reached.';
                          end;
    LongInt($00000039)  : begin
                            HrStr := 'ERROR_ADAP_HDW_ERR' ;
                            HrDescr := 'A network adapter hardware error occurred.';
                          end;
    LongInt($0000003A)  : begin
                            HrStr := 'ERROR_BAD_NET_RESP' ;
                            HrDescr := 'The specified server cannot perform the requested operation.';
                          end;
    LongInt($0000003B)  : begin
                            HrStr := 'ERROR_UNEXP_NET_ERR' ;
                            HrDescr := 'An unexpected network error occurred.';
                          end;
    LongInt($0000003C)  : begin
                            HrStr := 'ERROR_BAD_REM_ADAP' ;
                            HrDescr := 'The remote adapter is not compatible.';
                          end;
    LongInt($0000003D)  : begin
                            HrStr := 'ERROR_PRINTQ_FULL' ;
                            HrDescr := 'The print queue is full.';
                          end;
    LongInt($0000003E)  : begin
                            HrStr := 'ERROR_NO_SPOOL_SPACE' ;
                            HrDescr := 'Space to store the file waiting to be printed is not available on the server.';
                          end;
    LongInt($0000003F)  : begin
                            HrStr := 'ERROR_PRINT_CANCELLED' ;
                            HrDescr := 'Your file waiting to be printed was deleted.';
                          end;
    LongInt($00000040)  : begin
                            HrStr := 'ERROR_NETNAME_DELETED' ;
                            HrDescr := 'The specified network name is no longer available.';
                          end;
    LongInt($00000041)  : begin
                            HrStr := 'ERROR_NETWORK_ACCESS_DENIED' ;
                            HrDescr := 'Network access is denied.';
                          end;
    LongInt($00000042)  : begin
                            HrStr := 'ERROR_BAD_DEV_TYPE' ;
                            HrDescr := 'The network resource type is not correct.';
                          end;
    LongInt($00000043)  : begin
                            HrStr := 'ERROR_BAD_NET_NAME' ;
                            HrDescr := 'The network name cannot be found.';
                          end;
    LongInt($00000044)  : begin
                            HrStr := 'ERROR_TOO_MANY_NAMES' ;
                            HrDescr := 'The name limit for the local computer network adapter card was exceeded.';
                          end;
    LongInt($00000045)  : begin
                            HrStr := 'ERROR_TOO_MANY_SESS' ;
                            HrDescr := 'The network BIOS session limit was exceeded.';
                          end;
    LongInt($00000046)  : begin
                            HrStr := 'ERROR_SHARING_PAUSED' ;
                            HrDescr := 'The remote server has been paused or is in the process of being started.';
                          end;
    LongInt($00000047)  : begin
                            HrStr := 'ERROR_REQ_NOT_ACCEP' ;
                            HrDescr := 'No more connections can be made to this remote computer at this time because the computer has accepted the maximum number of connections.';
                          end;
    LongInt($00000048)  : begin
                            HrStr := 'ERROR_REDIR_PAUSED' ;
                            HrDescr := 'The specified printer or disk device has been paused.';
                          end;
    LongInt($00000050)  : begin
                            HrStr := 'ERROR_FILE_EXISTS' ;
                            HrDescr := 'The file exists.';
                          end;
    LongInt($00000052)  : begin
                            HrStr := 'ERROR_CANNOT_MAKE' ;
                            HrDescr := 'The directory or file cannot be created.';
                          end;
    LongInt($00000053)  : begin
                            HrStr := 'ERROR_FAIL_I24' ;
                            HrDescr := 'Fail on INT 24.';
                          end;
    LongInt($00000054)  : begin
                            HrStr := 'ERROR_OUT_OF_STRUCTURES' ;
                            HrDescr := 'Storage to process this request is not available.';
                          end;
    LongInt($00000055)  : begin
                            HrStr := 'ERROR_ALREADY_ASSIGNED' ;
                            HrDescr := 'The local device name is already in use.';
                          end;
    LongInt($00000056)  : begin
                            HrStr := 'ERROR_INVALID_PASSWORD' ;
                            HrDescr := 'The specified network password is not correct.';
                          end;
    LongInt($00000057)  : begin
                            HrStr := 'ERROR_INVALID_PARAMETER' ;
                            HrDescr := 'The parameter is incorrect.';
                          end;
    LongInt($00000058)  : begin
                            HrStr := 'ERROR_NET_WRITE_FAULT' ;
                            HrDescr := 'A write fault occurred on the network.';
                          end;
    LongInt($00000059)  : begin
                            HrStr := 'ERROR_NO_PROC_SLOTS' ;
                            HrDescr := 'The system cannot start another process at this time.';
                          end;
    LongInt($00000064)  : begin
                            HrStr := 'ERROR_TOO_MANY_SEMAPHORES' ;
                            HrDescr := 'Cannot create another system semaphore.';
                          end;
    LongInt($00000065)  : begin
                            HrStr := 'ERROR_EXCL_SEM_ALREADY_OWNED' ;
                            HrDescr := 'The exclusive semaphore is owned by another process.';
                          end;
    LongInt($00000066)  : begin
                            HrStr := 'ERROR_SEM_IS_SET' ;
                            HrDescr := 'The semaphore is set and cannot be closed.';
                          end;
    LongInt($00000067)  : begin
                            HrStr := 'ERROR_TOO_MANY_SEM_REQUESTS' ;
                            HrDescr := 'The semaphore cannot be set again.';
                          end;
    LongInt($00000068)  : begin
                            HrStr := 'ERROR_INVALID_AT_INTERRUPT_TIME' ;
                            HrDescr := 'Cannot request exclusive semaphores at interrupt time.';
                          end;
    LongInt($00000069)  : begin
                            HrStr := 'ERROR_SEM_OWNER_DIED' ;
                            HrDescr := 'The previous ownership of this semaphore has ended.';
                          end;
    LongInt($0000006A)  : begin
                            HrStr := 'ERROR_SEM_USER_LIMIT' ;
                            HrDescr := 'Insert the disk for drive %1.';
                          end;
    LongInt($0000006B)  : begin
                            HrStr := 'ERROR_DISK_CHANGE' ;
                            HrDescr := 'The program stopped because an alternate disk was not inserted.';
                          end;
    LongInt($0000006C)  : begin
                            HrStr := 'ERROR_DRIVE_LOCKED' ;
                            HrDescr := 'The disk is in use or locked by another process.';
                          end;
    LongInt($0000006D)  : begin
                            HrStr := 'ERROR_BROKEN_PIPE' ;
                            HrDescr := 'The pipe has been ended.';
                          end;
    LongInt($0000006E)  : begin
                            HrStr := 'ERROR_OPEN_FAILED' ;
                            HrDescr := 'The system cannot open the device or file specified.';
                          end;
    LongInt($0000006F)  : begin
                            HrStr := 'ERROR_BUFFER_OVERFLOW' ;
                            HrDescr := 'The file name is too long.';
                          end;
    LongInt($00000070)  : begin
                            HrStr := 'ERROR_DISK_FULL' ;
                            HrDescr := 'There is not enough space on the disk.';
                          end;
    LongInt($00000071)  : begin
                            HrStr := 'ERROR_NO_MORE_SEARCH_HANDLES' ;
                            HrDescr := 'No more internal file identifiers are available.';
                          end;
    LongInt($00000072)  : begin
                            HrStr := 'ERROR_INVALID_TARGET_HANDLE' ;
                            HrDescr := 'The target internal file identifier is incorrect.';
                          end;
    LongInt($00000075)  : begin
                            HrStr := 'ERROR_INVALID_CATEGORY' ;
                            HrDescr := 'The Input Output Control (IOCTL) call made by the application program is not correct.';
                          end;
    LongInt($00000076)  : begin
                            HrStr := 'ERROR_INVALID_VERIFY_SWITCH' ;
                            HrDescr := 'The verify-on-write switch parameter value is not correct.';
                          end;
    LongInt($00000077)  : begin
                            HrStr := 'ERROR_BAD_DRIVER_LEVEL' ;
                            HrDescr := 'The system does not support the command requested.';
                          end;
    LongInt($00000078)  : begin
                            HrStr := 'ERROR_CALL_NOT_IMPLEMENTED' ;
                            HrDescr := 'This function is not supported on this system.';
                          end;
    LongInt($00000079)  : begin
                            HrStr := 'ERROR_SEM_TIMEOUT' ;
                            HrDescr := 'The semaphore time-out period has expired.';
                          end;
    LongInt($0000007A)  : begin
                            HrStr := 'ERROR_INSUFFICIENT_BUFFER' ;
                            HrDescr := 'The data area passed to a system call is too small.';
                          end;
    LongInt($0000007B)  : begin
                            HrStr := 'ERROR_INVALID_NAME' ;
                            HrDescr := 'The file name, directory name, or volume label syntax is incorrect.';
                          end;
    LongInt($0000007C)  : begin
                            HrStr := 'ERROR_INVALID_LEVEL' ;
                            HrDescr := 'The system call level is not correct.';
                          end;
    LongInt($0000007D)  : begin
                            HrStr := 'ERROR_NO_VOLUME_LABEL' ;
                            HrDescr := 'The disk has no volume label.';
                          end;
    LongInt($0000007E)  : begin
                            HrStr := 'ERROR_MOD_NOT_FOUND' ;
                            HrDescr := 'The specified module could not be found.';
                          end;
    LongInt($0000007F)  : begin
                            HrStr := 'ERROR_PROC_NOT_FOUND' ;
                            HrDescr := 'The specified procedure could not be found.';
                          end;
    LongInt($00000080)  : begin
                            HrStr := 'ERROR_WAIT_NO_CHILDREN' ;
                            HrDescr := 'There are no child processes to wait for.';
                          end;
    LongInt($00000081)  : begin
                            HrStr := 'ERROR_CHILD_NOT_COMPLETE' ;
                            HrDescr := 'The %1 application cannot be run in Win32 mode.';
                          end;
    LongInt($00000082)  : begin
                            HrStr := 'ERROR_DIRECT_ACCESS_HANDLE' ;
                            HrDescr := 'Attempt to use a file handle to an open disk partition for an operation other than raw disk I/O.';
                          end;
    LongInt($00000083)  : begin
                            HrStr := 'ERROR_NEGATIVE_SEEK' ;
                            HrDescr := 'An attempt was made to move the file pointer before the beginning of the file.';
                          end;
    LongInt($00000084)  : begin
                            HrStr := 'ERROR_SEEK_ON_DEVICE' ;
                            HrDescr := 'The file pointer cannot be set on the specified device or file.';
                          end;
    LongInt($00000085)  : begin
                            HrStr := 'ERROR_IS_JOIN_TARGET' ;
                            HrDescr := 'A JOIN or SUBST command cannot be used for a drive that contains previously joined drives.';
                          end;
    LongInt($00000086)  : begin
                            HrStr := 'ERROR_IS_JOINED' ;
                            HrDescr := 'An attempt was made to use a JOIN or SUBST command on a drive that has already been joined.';
                          end;
    LongInt($00000087)  : begin
                            HrStr := 'ERROR_IS_SUBSTED' ;
                            HrDescr := 'An attempt was made to use a JOIN or SUBST command on a drive that has already been substituted.';
                          end;
    LongInt($00000088)  : begin
                            HrStr := 'ERROR_NOT_JOINED' ;
                            HrDescr := 'The system tried to delete the JOIN of a drive that is not joined.';
                          end;
    LongInt($00000089)  : begin
                            HrStr := 'ERROR_NOT_SUBSTED' ;
                            HrDescr := 'The system tried to delete the substitution of a drive that is not substituted.';
                          end;
    LongInt($0000008A)  : begin
                            HrStr := 'ERROR_JOIN_TO_JOIN' ;
                            HrDescr := 'The system tried to join a drive to a directory on a joined drive.';
                          end;
    LongInt($0000008B)  : begin
                            HrStr := 'ERROR_SUBST_TO_SUBST' ;
                            HrDescr := 'The system tried to substitute a drive to a directory on a substituted drive.';
                          end;
    LongInt($0000008C)  : begin
                            HrStr := 'ERROR_JOIN_TO_SUBST' ;
                            HrDescr := 'The system tried to join a drive to a directory on a substituted drive.';
                          end;
    LongInt($0000008D)  : begin
                            HrStr := 'ERROR_SUBST_TO_JOIN' ;
                            HrDescr := 'The system tried to SUBST a drive to a directory on a joined drive.';
                          end;
    LongInt($0000008E)  : begin
                            HrStr := 'ERROR_BUSY_DRIVE' ;
                            HrDescr := 'The system cannot perform a JOIN or SUBST at this time.';
                          end;
    LongInt($0000008F)  : begin
                            HrStr := 'ERROR_SAME_DRIVE' ;
                            HrDescr := 'The system cannot join or substitute a drive to or for a directory on the same drive.';
                          end;
    LongInt($00000090)  : begin
                            HrStr := 'ERROR_DIR_NOT_ROOT' ;
                            HrDescr := 'The directory is not a subdirectory of the root directory.';
                          end;
    LongInt($00000091)  : begin
                            HrStr := 'ERROR_DIR_NOT_EMPTY' ;
                            HrDescr := 'The directory is not empty.';
                          end;
    LongInt($00000092)  : begin
                            HrStr := 'ERROR_IS_SUBST_PATH' ;
                            HrDescr := 'The path specified is being used in a substitute.';
                          end;
    LongInt($00000093)  : begin
                            HrStr := 'ERROR_IS_JOIN_PATH' ;
                            HrDescr := 'Not enough resources are available to process this command.';
                          end;
    LongInt($00000094)  : begin
                            HrStr := 'ERROR_PATH_BUSY' ;
                            HrDescr := 'The path specified cannot be used at this time.';
                          end;
    LongInt($00000095)  : begin
                            HrStr := 'ERROR_IS_SUBST_TARGET' ;
                            HrDescr := 'An attempt was made to join or substitute a drive for which a directory on the drive is the target of a previous substitute.';
                          end;
    LongInt($00000096)  : begin
                            HrStr := 'ERROR_SYSTEM_TRACE' ;
                            HrDescr := 'System trace information was not specified in your CONFIG.SYS file, or tracing is disallowed.';
                          end;
    LongInt($00000097)  : begin
                            HrStr := 'ERROR_INVALID_EVENT_COUNT' ;
                            HrDescr := 'The number of specified semaphore events for DosMuxSemWait is not correct.';
                          end;
    LongInt($00000098)  : begin
                            HrStr := 'ERROR_TOO_MANY_MUXWAITERS' ;
                            HrDescr := 'DosMuxSemWait did not execute; too many semaphores are already set.';
                          end;
    LongInt($00000099)  : begin
                            HrStr := 'ERROR_INVALID_LIST_FORMAT' ;
                            HrDescr := 'The DosMuxSemWait list is not correct.';
                          end;
    LongInt($0000009A)  : begin
                            HrStr := 'ERROR_LABEL_TOO_LONG' ;
                            HrDescr := 'The volume label you entered exceeds the label character limit of the destination file system.';
                          end;
    LongInt($0000009B)  : begin
                            HrStr := 'ERROR_TOO_MANY_TCBS' ;
                            HrDescr := 'Cannot create another thread.';
                          end;
    LongInt($0000009C)  : begin
                            HrStr := 'ERROR_SIGNAL_REFUSED' ;
                            HrDescr := 'The recipient process has refused the signal.';
                          end;
    LongInt($0000009D)  : begin
                            HrStr := 'ERROR_DISCARDED' ;
                            HrDescr := 'The segment is already discarded and cannot be locked.';
                          end;
    LongInt($0000009E)  : begin
                            HrStr := 'ERROR_NOT_LOCKED' ;
                            HrDescr := 'The segment is already unlocked.';
                          end;
    LongInt($0000009F)  : begin
                            HrStr := 'ERROR_BAD_THREADID_ADDR' ;
                            HrDescr := 'The address for the thread ID is not correct.';
                          end;
    LongInt($000000A0)  : begin
                            HrStr := 'ERROR_BAD_ARGUMENTS' ;
                            HrDescr := 'One or more arguments are not correct.';
                          end;
    LongInt($000000A1)  : begin
                            HrStr := 'ERROR_BAD_PATHNAME' ;
                            HrDescr := 'The specified path is invalid.';
                          end;
    LongInt($000000A2)  : begin
                            HrStr := 'ERROR_SIGNAL_PENDING' ;
                            HrDescr := 'A signal is already pending.';
                          end;
    LongInt($000000A4)  : begin
                            HrStr := 'ERROR_MAX_THRDS_REACHED' ;
                            HrDescr := 'No more threads can be created in the system.';
                          end;
    LongInt($000000A7)  : begin
                            HrStr := 'ERROR_LOCK_FAILED' ;
                            HrDescr := 'Unable to lock a region of a file.';
                          end;
    LongInt($000000AA)  : begin
                            HrStr := 'ERROR_BUSY' ;
                            HrDescr := 'The requested resource is in use.';
                          end;
    LongInt($000000AD)  : begin
                            HrStr := 'ERROR_CANCEL_VIOLATION' ;
                            HrDescr := 'A lock request was not outstanding for the supplied cancel region.';
                          end;
    LongInt($000000AE)  : begin
                            HrStr := 'ERROR_ATOMIC_LOCKS_NOT_SUPPORTED' ;
                            HrDescr := 'The file system does not support atomic changes to the lock type.';
                          end;
    LongInt($000000B4)  : begin
                            HrStr := 'ERROR_INVALID_SEGMENT_NUMBER' ;
                            HrDescr := 'The system detected a segment number that was not correct.';
                          end;
    LongInt($000000B6)  : begin
                            HrStr := 'ERROR_INVALID_ORDINAL' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000B7)  : begin
                            HrStr := 'ERROR_ALREADY_EXISTS' ;
                            HrDescr := 'Cannot create a file when that file already exists.';
                          end;
    LongInt($000000BA)  : begin
                            HrStr := 'ERROR_INVALID_FLAG_NUMBER' ;
                            HrDescr := 'The flag passed is not correct.';
                          end;
    LongInt($000000BB)  : begin
                            HrStr := 'ERROR_SEM_NOT_FOUND' ;
                            HrDescr := 'The specified system semaphore name was not found.';
                          end;
    LongInt($000000BC)  : begin
                            HrStr := 'ERROR_INVALID_STARTING_CODESEG' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000BD)  : begin
                            HrStr := 'ERROR_INVALID_STACKSEG' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000BE)  : begin
                            HrStr := 'ERROR_INVALID_MODULETYPE' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000BF)  : begin
                            HrStr := 'ERROR_INVALID_EXE_SIGNATURE' ;
                            HrDescr := 'Cannot run %1 in Win32 mode.';
                          end;
    LongInt($000000C0)  : begin
                            HrStr := 'ERROR_EXE_MARKED_INVALID' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000C1)  : begin
                            HrStr := 'ERROR_BAD_EXE_FORMAT' ;
                            HrDescr := '%1 is not a valid Win32 application.';
                          end;
    LongInt($000000C2)  : begin
                            HrStr := 'ERROR_ITERATED_DATA_EXCEEDS_64k' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000C3)  : begin
                            HrStr := 'ERROR_INVALID_MINALLOCSIZE' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000C4)  : begin
                            HrStr := 'ERROR_DYNLINK_FROM_INVALID_RING' ;
                            HrDescr := 'The operating system cannot run this application program.';
                          end;
    LongInt($000000C5)  : begin
                            HrStr := 'ERROR_IOPL_NOT_ENABLED' ;
                            HrDescr := 'The operating system is not presently configured to run this application.';
                          end;
    LongInt($000000C6)  : begin
                            HrStr := 'ERROR_INVALID_SEGDPL' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000C7)  : begin
                            HrStr := 'ERROR_AUTODATASEG_EXCEEDS_64k' ;
                            HrDescr := 'The operating system cannot run this application program.';
                          end;
    LongInt($000000C8)  : begin
                            HrStr := 'ERROR_RING2SEG_MUST_BE_MOVABLE' ;
                            HrDescr := 'The code segment cannot be greater than or equal to 64 KB.';
                          end;
    LongInt($000000C9)  : begin
                            HrStr := 'ERROR_RELOC_CHAIN_XEEDS_SEGLIM' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000CA)  : begin
                            HrStr := 'ERROR_INFLOOP_IN_RELOC_CHAIN' ;
                            HrDescr := 'The operating system cannot run %1.';
                          end;
    LongInt($000000CB)  : begin
                            HrStr := 'ERROR_ENVVAR_NOT_FOUND' ;
                            HrDescr := 'The system could not find the environment option that was entered.';
                          end;
    LongInt($000000CD)  : begin
                            HrStr := 'ERROR_NO_SIGNAL_SENT' ;
                            HrDescr := 'No process in the command subtree has a signal handler.';
                          end;
    LongInt($000000CE)  : begin
                            HrStr := 'ERROR_FILENAME_EXCED_RANGE' ;
                            HrDescr := 'The file name or extension is too long.';
                          end;
    LongInt($000000CF)  : begin
                            HrStr := 'ERROR_RING2_STACK_IN_USE' ;
                            HrDescr := 'The ring 2 stack is in use.';
                          end;
    LongInt($000000D0)  : begin
                            HrStr := 'ERROR_META_EXPANSION_TOO_LONG' ;
                            HrDescr := 'The asterisk (*) or question mark (?) global file name characters are entered incorrectly, or too many global file name characters are specified.';
                          end;
    LongInt($000000D1)  : begin
                            HrStr := 'ERROR_INVALID_SIGNAL_NUMBER' ;
                            HrDescr := 'The signal being posted is not correct.';
                          end;
    LongInt($000000D2)  : begin
                            HrStr := 'ERROR_THREAD_1_INACTIVE' ;
                            HrDescr := 'The signal handler cannot be set.';
                          end;
    LongInt($000000D4)  : begin
                            HrStr := 'ERROR_LOCKED' ;
                            HrDescr := 'The segment is locked and cannot be reallocated.';
                          end;
    LongInt($000000D6)  : begin
                            HrStr := 'ERROR_TOO_MANY_MODULES' ;
                            HrDescr := 'Too many dynamic-link modules are attached to this program or dynamic-link module.';
                          end;
    LongInt($000000D7)  : begin
                            HrStr := 'ERROR_NESTING_NOT_ALLOWED' ;
                            HrDescr := 'Cannot nest calls to LoadModule.';
                          end;
    LongInt($000000D8)  : begin
                            HrStr := 'ERROR_EXE_MACHINE_TYPE_MISMATCH' ;
                            HrDescr := 'This version of %1 is not compatible with the version of Windows you''re running.' +
                                       'Check your computer''s system information to see whether you need an x86 (32-bit) or x64 (64-bit) version of the program, and then contact the software publisher.';
                          end;
    LongInt($000000D9)  : begin
                            HrStr := 'ERROR_EXE_CANNOT_MODIFY_SIGNED_BINARY' ;
                            HrDescr := 'The image file %1 is signed, unable to modify.';
                          end;
    LongInt($000000DA)  : begin
                            HrStr := 'ERROR_EXE_CANNOT_MODIFY_STRONG_SIGNED_BINARY' ;
                            HrDescr := 'The image file %1 is strong signed, unable to modify.';
                          end;
    LongInt($000000DC)  : begin
                            HrStr := 'ERROR_FILE_CHECKED_OUT' ;
                            HrDescr := 'This file is checked out or locked for editing by another user.';
                          end;
    LongInt($000000DD)  : begin
                            HrStr := 'ERROR_CHECKOUT_REQUIRED' ;
                            HrDescr := 'The file must be checked out before saving changes.';
                          end;
    LongInt($000000DE)  : begin
                            HrStr := 'ERROR_BAD_FILE_TYPE' ;
                            HrDescr := 'The file type being saved or retrieved has been blocked.';
                          end;
    LongInt($000000DF)  : begin
                            HrStr := 'ERROR_FILE_TOO_LARGE' ;
                            HrDescr := 'The file size exceeds the limit allowed and cannot be saved.';
                          end;
    LongInt($000000E0)  : begin
                            HrStr := 'ERROR_FORMS_AUTH_REQUIRED' ;
                            HrDescr := 'Access denied.' +
                                       'Before opening files in this location, you must first browse to the website and select the option to sign in automatically.';
                          end;
    LongInt($000000E1)  : begin
                            HrStr := 'ERROR_VIRUS_INFECTED' ;
                            HrDescr := 'Operation did not complete successfully because the file contains a virus.';
                          end;
    LongInt($000000E2)  : begin
                            HrStr := 'ERROR_VIRUS_DELETED' ;
                            HrDescr := 'This file contains a virus and cannot be opened.' +
                                       'Due to the nature of this virus, the file has been removed from this location.';
                          end;
    LongInt($000000E5)  : begin
                            HrStr := 'ERROR_PIPE_LOCAL' ;
                            HrDescr := 'The pipe is local.';
                          end;
    LongInt($000000E6)  : begin
                            HrStr := 'ERROR_BAD_PIPE' ;
                            HrDescr := 'The pipe state is invalid.';
                          end;
    LongInt($000000E7)  : begin
                            HrStr := 'ERROR_PIPE_BUSY' ;
                            HrDescr := 'All pipe instances are busy.';
                          end;
    LongInt($000000E8)  : begin
                            HrStr := 'ERROR_NO_DATA' ;
                            HrDescr := 'The pipe is being closed.';
                          end;
    LongInt($000000E9)  : begin
                            HrStr := 'ERROR_PIPE_NOT_CONNECTED' ;
                            HrDescr := 'No process is on the other end of the pipe.';
                          end;
    LongInt($000000EA)  : begin
                            HrStr := 'ERROR_MORE_DATA' ;
                            HrDescr := 'More data is available.';
                          end;
    LongInt($000000F0)  : begin
                            HrStr := 'ERROR_VC_DISCONNECTED' ;
                            HrDescr := 'The session was canceled.';
                          end;
    LongInt($000000FE)  : begin
                            HrStr := 'ERROR_INVALID_EA_NAME' ;
                            HrDescr := 'The specified extended attribute name was invalid.';
                          end;
    LongInt($000000FF)  : begin
                            HrStr := 'ERROR_EA_LIST_INCONSISTENT' ;
                            HrDescr := 'The extended attributes are inconsistent.';
                          end;
    LongInt($00000102)  : begin
                            HrStr := 'WAIT_TIMEOUT' ;
                            HrDescr := 'The wait operation timed out.';
                          end;
    LongInt($00000103)  : begin
                            HrStr := 'ERROR_NO_MORE_ITEMS' ;
                            HrDescr := 'No more data is available.';
                          end;
    LongInt($0000010A)  : begin
                            HrStr := 'ERROR_CANNOT_COPY' ;
                            HrDescr := 'The copy functions cannot be used.';
                          end;
    LongInt($0000010B)  : begin
                            HrStr := 'ERROR_DIRECTORY' ;
                            HrDescr := 'The directory name is invalid.';
                          end;
    LongInt($00000113)  : begin
                            HrStr := 'ERROR_EAS_DIDNT_FIT' ;
                            HrDescr := 'The extended attributes did not fit in the buffer.';
                          end;
    LongInt($00000114)  : begin
                            HrStr := 'ERROR_EA_FILE_CORRUPT' ;
                            HrDescr := 'The extended attribute file on the mounted file system is corrupt.';
                          end;
    LongInt($00000115)  : begin
                            HrStr := 'ERROR_EA_TABLE_FULL' ;
                            HrDescr := 'The extended attribute table file is full.';
                          end;
    LongInt($00000116)  : begin
                            HrStr := 'ERROR_INVALID_EA_HANDLE' ;
                            HrDescr := 'The specified extended attribute handle is invalid.';
                          end;
    LongInt($0000011A)  : begin
                            HrStr := 'ERROR_EAS_NOT_SUPPORTED' ;
                            HrDescr := 'The mounted file system does not support extended attributes.';
                          end;
    LongInt($00000120)  : begin
                            HrStr := 'ERROR_NOT_OWNER' ;
                            HrDescr := 'Attempt to release mutex not owned by caller.';
                          end;
    LongInt($0000012A)  : begin
                            HrStr := 'ERROR_TOO_MANY_POSTS' ;
                            HrDescr := 'Too many posts were made to a semaphore.';
                          end;
    LongInt($0000012B)  : begin
                            HrStr := 'ERROR_PARTIAL_COPY' ;
                            HrDescr := 'Only part of a ReadProcessMemory or WriteProcessMemory request was completed.';
                          end;
    LongInt($0000012C)  : begin
                            HrStr := 'ERROR_OPLOCK_NOT_GRANTED' ;
                            HrDescr := 'The oplock request is denied.';
                          end;
    LongInt($0000012D)  : begin
                            HrStr := 'ERROR_INVALID_OPLOCK_PROTOCOL' ;
                            HrDescr := 'An invalid oplock acknowledgment was received by the system.';
                          end;
    LongInt($0000012E)  : begin
                            HrStr := 'ERROR_DISK_TOO_FRAGMENTED' ;
                            HrDescr := 'The volume is too fragmented to complete this operation.';
                          end;
    LongInt($0000012F)  : begin
                            HrStr := 'ERROR_DELETE_PENDING' ;
                            HrDescr := 'The file cannot be opened because it is in the process of being deleted.';
                          end;
    LongInt($0000013D)  : begin
                            HrStr := 'ERROR_MR_MID_NOT_FOUND' ;
                            HrDescr := 'The system cannot find message text for message number $%1 in the message file for %2.' +
                                       '';
                          end;
    LongInt($0000013E)  : begin
                            HrStr := 'ERROR_SCOPE_NOT_FOUND' ;
                            HrDescr := 'The scope specified was not found.';
                          end;
    LongInt($0000015E)  : begin
                            HrStr := 'ERROR_FAIL_NOACTION_REBOOT' ;
                            HrDescr := 'No action was taken because a system reboot is required.';
                          end;
    LongInt($0000015F)  : begin
                            HrStr := 'ERROR_FAIL_SHUTDOWN' ;
                            HrDescr := 'The shutdown operation failed.';
                          end;
    LongInt($00000160)  : begin
                            HrStr := 'ERROR_FAIL_RESTART' ;
                            HrDescr := 'The restart operation failed.';
                          end;
    LongInt($00000161)  : begin
                            HrStr := 'ERROR_MAX_SESSIONS_REACHED' ;
                            HrDescr := 'The maximum number of sessions has been reached.';
                          end;
    LongInt($00000190)  : begin
                            HrStr := 'ERROR_THREAD_MODE_ALREADY_BACKGROUND' ;
                            HrDescr := 'The thread is already in background processing mode.';
                          end;
    LongInt($00000191)  : begin
                            HrStr := 'ERROR_THREAD_MODE_NOT_BACKGROUND' ;
                            HrDescr := 'The thread is not in background processing mode.';
                          end;
    LongInt($00000192)  : begin
                            HrStr := 'ERROR_PROCESS_MODE_ALREADY_BACKGROUND' ;
                            HrDescr := 'The process is already in background processing mode.';
                          end;
    LongInt($00000193)  : begin
                            HrStr := 'ERROR_PROCESS_MODE_NOT_BACKGROUND' ;
                            HrDescr := 'The process is not in background processing mode.';
                          end;
    LongInt($000001E7)  : begin
                            HrStr := 'ERROR_INVALID_ADDRESS' ;
                            HrDescr := 'Attempt to access invalid address.';
                          end;
    LongInt($000001F4)  : begin
                            HrStr := 'ERROR_USER_PROFILE_LOAD' ;
                            HrDescr := 'User profile cannot be loaded.';
                          end;
    LongInt($00000216)  : begin
                            HrStr := 'ERROR_ARITHMETIC_OVERFLOW' ;
                            HrDescr := 'Arithmetic result exceeded 32 bits.';
                          end;
    LongInt($00000217)  : begin
                            HrStr := 'ERROR_PIPE_CONNECTED' ;
                            HrDescr := 'There is a process on the other end of the pipe.';
                          end;
    LongInt($00000218)  : begin
                            HrStr := 'ERROR_PIPE_LISTENING' ;
                            HrDescr := 'Waiting for a process to open the other end of the pipe.';
                          end;
    LongInt($00000219)  : begin
                            HrStr := 'ERROR_VERIFIER_STOP' ;
                            HrDescr := 'Application verifier has found an error in the current process.';
                          end;
    LongInt($0000021A)  : begin
                            HrStr := 'ERROR_ABIOS_ERROR' ;
                            HrDescr := 'An error occurred in the ABIOS subsystem.';
                          end;
    LongInt($0000021B)  : begin
                            HrStr := 'ERROR_WX86_WARNING' ;
                            HrDescr := 'A warning occurred in the WX86 subsystem.';
                          end;
    LongInt($0000021C)  : begin
                            HrStr := 'ERROR_WX86_ERROR' ;
                            HrDescr := 'An error occurred in the WX86 subsystem.';
                          end;
    LongInt($0000021D)  : begin
                            HrStr := 'ERROR_TIMER_NOT_CANCELED' ;
                            HrDescr := 'An attempt was made to cancel or set a timer that has an associated asynchronous procedure call (APC) and the subject thread is not the thread that originally set the timer with an associated APC routine.';
                          end;
    LongInt($0000021E)  : begin
                            HrStr := 'ERROR_UNWIND' ;
                            HrDescr := 'Unwind exception code.';
                          end;
    LongInt($0000021F)  : begin
                            HrStr := 'ERROR_BAD_STACK' ;
                            HrDescr := 'An invalid or unaligned stack was encountered during an unwind operation.';
                          end;
    LongInt($00000220)  : begin
                            HrStr := 'ERROR_INVALID_UNWIND_TARGET' ;
                            HrDescr := 'An invalid unwind target was encountered during an unwind operation.';
                          end;
    LongInt($00000221)  : begin
                            HrStr := 'ERROR_INVALID_PORT_ATTRIBUTES' ;
                            HrDescr := 'Invalid object attributes specified to NtCreatePort or invalid port attributes specified to NtConnectPort.';
                          end;
    LongInt($00000222)  : begin
                            HrStr := 'ERROR_PORT_MESSAGE_TOO_LONG' ;
                            HrDescr := 'Length of message passed to NtRequestPort or NtRequestWaitReplyPort was longer than the maximum message allowed by the port.';
                          end;
    LongInt($00000223)  : begin
                            HrStr := 'ERROR_INVALID_QUOTA_LOWER' ;
                            HrDescr := 'An attempt was made to lower a quota limit below the current usage.';
                          end;
    LongInt($00000224)  : begin
                            HrStr := 'ERROR_DEVICE_ALREADY_ATTACHED' ;
                            HrDescr := 'An attempt was made to attach to a device that was already attached to another device.';
                          end;
    LongInt($00000225)  : begin
                            HrStr := 'ERROR_INSTRUCTION_MISALIGNMENT' ;
                            HrDescr := 'An attempt was made to execute an instruction at an unaligned address, and the host system does not support unaligned instruction references.';
                          end;
    LongInt($00000226)  : begin
                            HrStr := 'ERROR_PROFILING_NOT_STARTED' ;
                            HrDescr := 'Profiling not started.';
                          end;
    LongInt($00000227)  : begin
                            HrStr := 'ERROR_PROFILING_NOT_STOPPED' ;
                            HrDescr := 'Profiling not stopped.';
                          end;
    LongInt($00000228)  : begin
                            HrStr := 'ERROR_COULD_NOT_INTERPRET' ;
                            HrDescr := 'The passed ACL did not contain the minimum required information.';
                          end;
    LongInt($00000229)  : begin
                            HrStr := 'ERROR_PROFILING_AT_LIMIT' ;
                            HrDescr := 'The number of active profiling objects is at the maximum and no more can be started.';
                          end;
    LongInt($0000022A)  : begin
                            HrStr := 'ERROR_CANT_WAIT' ;
                            HrDescr := 'Used to indicate that an operation cannot continue without blocking for I/O.';
                          end;
    LongInt($0000022B)  : begin
                            HrStr := 'ERROR_CANT_TERMINATE_SELF' ;
                            HrDescr := 'Indicates that a thread attempted to terminate itself by default (called NtTerminateThread with NULL) and it was the last thread in the current process.';
                          end;
    LongInt($0000022C)  : begin
                            HrStr := 'ERROR_UNEXPECTED_MM_CREATE_ERR' ;
                            HrDescr := 'If an MM error is returned that is not defined in the standard FsRtl filter, it is converted to one of the following errors that is guaranteed to be in the filter.' +
                                       'In this case, information is lost; however, the filter correctly handles the exception.';
                          end;
    LongInt($0000022D)  : begin
                            HrStr := 'ERROR_UNEXPECTED_MM_MAP_ERROR' ;
                            HrDescr := 'If an MM error is returned that is not defined in the standard FsRtl filter, it is converted to one of the following errors that is guaranteed to be in the filter.' +
                                       'In this case, information is lost; however, the filter correctly handles the exception.';
                          end;
    LongInt($0000022E)  : begin
                            HrStr := 'ERROR_UNEXPECTED_MM_EXTEND_ERR' ;
                            HrDescr := 'If an MM error is returned that is not defined in the standard FsRtl filter, it is converted to one of the following errors that is guaranteed to be in the filter.' +
                                       'In this case, information is lost; however, the filter correctly handles the exception.';
                          end;
    LongInt($0000022F)  : begin
                            HrStr := 'ERROR_BAD_FUNCTION_TABLE' ;
                            HrDescr := 'A malformed function table was encountered during an unwind operation.';
                          end;
    LongInt($00000230)  : begin
                            HrStr := 'ERROR_NO_GUID_TRANSLATION' ;
                            HrDescr := 'Indicates that an attempt was made to assign protection to a file system file or directory and one of the SIDs in the security descriptor could not be translated into a GUID that could be stored by the file system.' +
                                       'This causes the protection attempt to fail, which might cause a file creation attempt to fail.';
                          end;
    LongInt($00000231)  : begin
                            HrStr := 'ERROR_INVALID_LDT_SIZE' ;
                            HrDescr := 'Indicates that an attempt was made to grow a local domain table (LDT) by setting its size, or that the size was not an even number of selectors.';
                          end;
    LongInt($00000233)  : begin
                            HrStr := 'ERROR_INVALID_LDT_OFFSET' ;
                            HrDescr := 'Indicates that the starting value for the LDT information was not an integral multiple of the selector size.';
                          end;
    LongInt($00000234)  : begin
                            HrStr := 'ERROR_INVALID_LDT_DESCRIPTOR' ;
                            HrDescr := 'Indicates that the user supplied an invalid descriptor when trying to set up LDT descriptors.';
                          end;
    LongInt($00000235)  : begin
                            HrStr := 'ERROR_TOO_MANY_THREADS' ;
                            HrDescr := 'Indicates a process has too many threads to perform the requested action.' +
                                       'For example, assignment of a primary token can be performed only when a process has zero or one threads.';
                          end;
    LongInt($00000236)  : begin
                            HrStr := 'ERROR_THREAD_NOT_IN_PROCESS' ;
                            HrDescr := 'An attempt was made to operate on a thread within a specific process, but the thread specified is not in the process specified.';
                          end;
    LongInt($00000237)  : begin
                            HrStr := 'ERROR_PAGEFILE_QUOTA_EXCEEDED' ;
                            HrDescr := 'Page file quota was exceeded.';
                          end;
    LongInt($00000238)  : begin
                            HrStr := 'ERROR_LOGON_SERVER_CONFLICT' ;
                            HrDescr := 'The Netlogon service cannot start because another Netlogon service running in the domain conflicts with the specified role.';
                          end;
    LongInt($00000239)  : begin
                            HrStr := 'ERROR_SYNCHRONIZATION_REQUIRED' ;
                            HrDescr := 'On applicable Windows Server releases, the Security Accounts Manager (SAM) database is significantly out of synchronization with the copy on the domain controller.' +
                                       'A complete synchronization is required.';
                          end;
    LongInt($0000023A)  : begin
                            HrStr := 'ERROR_NET_OPEN_FAILED' ;
                            HrDescr := 'The NtCreateFile API failed.' +
                                       'This error should never be returned to an application, it is a place holder for the Windows LAN Manager Redirector to use in its internal error mapping routines.';
                          end;
    LongInt($0000023B)  : begin
                            HrStr := 'ERROR_IO_PRIVILEGE_FAILED' ;
                            HrDescr := '{Privilege Failed} The I/O permissions for the process could not be changed.';
                          end;
    LongInt($0000023C)  : begin
                            HrStr := 'ERROR_CONTROL_C_EXIT' ;
                            HrDescr := '{Application Exit by CTRL+C} The application terminated as a result of a CTRL+C.';
                          end;
    LongInt($0000023D)  : begin
                            HrStr := 'ERROR_MISSING_SYSTEMFILE' ;
                            HrDescr := '{Missing System File} The required system file %hs is bad or missing.';
                          end;
    LongInt($0000023E)  : begin
                            HrStr := 'ERROR_UNHANDLED_EXCEPTION' ;
                            HrDescr := '{Application Error} The exception %s ($%08lx) occurred in the application at location $%08lx.';
                          end;
    LongInt($0000023F)  : begin
                            HrStr := 'ERROR_APP_INIT_FAILURE' ;
                            HrDescr := '{Application Error} The application failed to initialize properly ($%lx).' +
                                       'Click OK to terminate the application.';
                          end;
    LongInt($00000240)  : begin
                            HrStr := 'ERROR_PAGEFILE_CREATE_FAILED' ;
                            HrDescr := '{Unable to Create Paging File} The creation of the paging file %hs failed (%lx).' +
                                       'The requested size was %ld.';
                          end;
    LongInt($00000241)  : begin
                            HrStr := 'ERROR_INVALID_IMAGE_HASH' ;
                            HrDescr := 'The hash for the image cannot be found in the system catalogs.' +
                                       'The image is likely corrupt or the victim of tampering.';
                          end;
    LongInt($00000242)  : begin
                            HrStr := 'ERROR_NO_PAGEFILE' ;
                            HrDescr := '{No Paging File Specified} No paging file was specified in the system configuration.';
                          end;
    LongInt($00000243)  : begin
                            HrStr := 'ERROR_ILLEGAL_FLOAT_CONTEXT' ;
                            HrDescr := '{EXCEPTION} A real-mode application issued a floating-point instruction, and floating-point hardware is not present.';
                          end;
    LongInt($00000244)  : begin
                            HrStr := 'ERROR_NO_EVENT_PAIR' ;
                            HrDescr := 'An event pair synchronization operation was performed using the thread-specific client/server event pair object, but no event pair object was associated with the thread.';
                          end;
    LongInt($00000245)  : begin
                            HrStr := 'ERROR_DOMAIN_CTRLR_CONFIG_ERROR' ;
                            HrDescr := 'A domain server has an incorrect configuration.';
                          end;
    LongInt($00000246)  : begin
                            HrStr := 'ERROR_ILLEGAL_CHARACTER' ;
                            HrDescr := 'An illegal character was encountered.' +
                                       'For a multibyte character set, this includes a lead byte without a succeeding trail byte.' +
                                       'For the Unicode character set, this includes the characters $FFFF and $FFFE.';
                          end;
    LongInt($00000247)  : begin
                            HrStr := 'ERROR_UNDEFINED_CHARACTER' ;
                            HrDescr := 'The Unicode character is not defined in the Unicode character set installed on the system.';
                          end;
    LongInt($00000248)  : begin
                            HrStr := 'ERROR_FLOPPY_VOLUME' ;
                            HrDescr := 'The paging file cannot be created on a floppy disk.';
                          end;
    LongInt($00000249)  : begin
                            HrStr := 'ERROR_BIOS_FAILED_TO_CONNECT_INTERRUPT' ;
                            HrDescr := 'The system bios failed to connect a system interrupt to the device or bus for which the device is connected.';
                          end;
    LongInt($0000024A)  : begin
                            HrStr := 'ERROR_BACKUP_CONTROLLER' ;
                            HrDescr := 'This operation is only allowed for the primary domain controller (PDC) of the domain.';
                          end;
    LongInt($0000024B)  : begin
                            HrStr := 'ERROR_MUTANT_LIMIT_EXCEEDED' ;
                            HrDescr := 'An attempt was made to acquire a mutant such that its maximum count would have been exceeded.';
                          end;
    LongInt($0000024C)  : begin
                            HrStr := 'ERROR_FS_DRIVER_REQUIRED' ;
                            HrDescr := 'A volume has been accessed for which a file system driver is required that has not yet been loaded.';
                          end;
    LongInt($0000024D)  : begin
                            HrStr := 'ERROR_CANNOT_LOAD_REGISTRY_FILE' ;
                            HrDescr := '{Registry File Failure} The registry cannot load the hive (file): %hs or its log or alternate.' +
                                       'It is corrupt, absent, or not writable.';
                          end;
    LongInt($0000024E)  : begin
                            HrStr := 'ERROR_DEBUG_ATTACH_FAILED' ;
                            HrDescr := '{Unexpected Failure in DebugActiveProcess} An unexpected failure occurred while processing a DebugActiveProcess API request.' +
                                       'Choosing OK will terminate the process, and choosing Cancel will ignore the error.';
                          end;
    LongInt($0000024F)  : begin
                            HrStr := 'ERROR_SYSTEM_PROCESS_TERMINATED' ;
                            HrDescr := '{Fatal System Error} The %hs system process terminated unexpectedly with a status of $%08x).' +
                                       'The system has been shut down.';
                          end;
    LongInt($00000250)  : begin
                            HrStr := 'ERROR_DATA_NOT_ACCEPTED' ;
                            HrDescr := '{Data Not Accepted} The transport driver interface (TDI) client could not handle the data received during an indication.';
                          end;
    LongInt($00000251)  : begin
                            HrStr := 'ERROR_VDM_HARD_ERROR' ;
                            HrDescr := 'The NT Virtual DOS Machine (NTVDM) encountered a hard error.';
                          end;
    LongInt($00000252)  : begin
                            HrStr := 'ERROR_DRIVER_CANCEL_TIMEOUT' ;
                            HrDescr := '{Cancel Timeout} The driver %hs failed to complete a canceled I/O request in the allotted time.';
                          end;
    LongInt($00000253)  : begin
                            HrStr := 'ERROR_REPLY_MESSAGE_MISMATCH' ;
                            HrDescr := '{Reply Message Mismatch} An attempt was made to reply to a local procedure call (LPC) message, but the thread specified by the client ID in the message was not waiting on that message.';
                          end;
    LongInt($00000254)  : begin
                            HrStr := 'ERROR_LOST_WRITEBEHIND_DATA' ;
                            HrDescr := '{Delayed Write Failed} Windows was unable to save all the data for the file %hs.' +
                                       'The data has been lost.' +
                                       'This error might be caused by a failure of your computer hardware or network connection.' +
                                       'Try to save this file elsewhere.';
                          end;
    LongInt($00000255)  : begin
                            HrStr := 'ERROR_CLIENT_SERVER_PARAMETERS_INVALID' ;
                            HrDescr := 'The parameters passed to the server in the client/server shared memory window were invalid.' +
                                       'Too much data might have been put in the shared memory window.';
                          end;
    LongInt($00000256)  : begin
                            HrStr := 'ERROR_NOT_TINY_STREAM' ;
                            HrDescr := 'The stream is not a tiny stream.';
                          end;
    LongInt($00000257)  : begin
                            HrStr := 'ERROR_STACK_OVERFLOW_READ' ;
                            HrDescr := 'The request must be handled by the stack overflow code.';
                          end;
    LongInt($00000258)  : begin
                            HrStr := 'ERROR_CONVERT_TO_LARGE' ;
                            HrDescr := 'Internal OFS status codes indicating how an allocation operation is handled.' +
                                       'Either it is retried after the containing onode is moved or the extent stream is converted to a large stream.';
                          end;
    LongInt($00000259)  : begin
                            HrStr := 'ERROR_FOUND_OUT_OF_SCOPE' ;
                            HrDescr := 'The attempt to find the object found an object matching by ID on the volume but it is out of the scope of the handle used for the operation.';
                          end;
    LongInt($0000025A)  : begin
                            HrStr := 'ERROR_ALLOCATE_BUCKET' ;
                            HrDescr := 'The bucket array must be grown.' +
                                       'Retry transaction after doing so.';
                          end;
    LongInt($0000025B)  : begin
                            HrStr := 'ERROR_MARSHALL_OVERFLOW' ;
                            HrDescr := 'The user/kernel marshaling buffer has overflowed.';
                          end;
    LongInt($0000025C)  : begin
                            HrStr := 'ERROR_INVALID_VARIANT' ;
                            HrDescr := 'The supplied variant structure contains invalid data.';
                          end;
    LongInt($0000025D)  : begin
                            HrStr := 'ERROR_BAD_COMPRESSION_BUFFER' ;
                            HrDescr := 'The specified buffer contains ill-formed data.';
                          end;
    LongInt($0000025E)  : begin
                            HrStr := 'ERROR_AUDIT_FAILED' ;
                            HrDescr := '{Audit Failed} An attempt to generate a security audit failed.';
                          end;
    LongInt($0000025F)  : begin
                            HrStr := 'ERROR_TIMER_RESOLUTION_NOT_SET' ;
                            HrDescr := 'The timer resolution was not previously set by the current process.';
                          end;
    LongInt($00000260)  : begin
                            HrStr := 'ERROR_INSUFFICIENT_LOGON_INFO' ;
                            HrDescr := 'There is insufficient account information to log you on.';
                          end;
    LongInt($00000261)  : begin
                            HrStr := 'ERROR_BAD_DLL_ENTRYPOINT' ;
                            HrDescr := '{Invalid DLL Entrypoint} The dynamic link library %hs is not written correctly.' +
                                       'The stack pointer has been left in an inconsistent state.' +
                                       'The entry point should be declared as WINAPI or STDCALL.' +
                                       'Select YES to fail the DLL load.' +
                                       'Select NO to continue execution.' +
                                       'Selecting NO can cause the application to operate incorrectly.';
                          end;
    LongInt($00000262)  : begin
                            HrStr := 'ERROR_BAD_SERVICE_ENTRYPOINT' ;
                            HrDescr := '{Invalid Service Callback Entrypoint} The %hs service is not written correctly.' +
                                       'The stack pointer has been left in an inconsistent state.' +
                                       'The callback entry point should be declared as WINAPI or STDCALL.' +
                                       'Selecting OK will cause the service to continue operation.' +
                                       'However, the service process might operate incorrectly.';
                          end;
    LongInt($00000263)  : begin
                            HrStr := 'ERROR_IP_ADDRESS_CONFLICT1' ;
                            HrDescr := 'There is an IP address conflict with another system on the network.';
                          end;
    LongInt($00000264)  : begin
                            HrStr := 'ERROR_IP_ADDRESS_CONFLICT2' ;
                            HrDescr := 'There is an IP address conflict with another system on the network.';
                          end;
    LongInt($00000265)  : begin
                            HrStr := 'ERROR_REGISTRY_QUOTA_LIMIT' ;
                            HrDescr := '{Low On Registry Space} The system has reached the maximum size allowed for the system part of the registry.' +
                                       'Additional storage requests will be ignored.';
                          end;
    LongInt($00000266)  : begin
                            HrStr := 'ERROR_NO_CALLBACK_ACTIVE' ;
                            HrDescr := 'A callback return system service cannot be executed when no callback is active.';
                          end;
    LongInt($00000267)  : begin
                            HrStr := 'ERROR_PWD_TOO_SHORT' ;
                            HrDescr := 'The password provided is too short to meet the policy of your user account.' +
                                       'Choose a longer password.';
                          end;
    LongInt($00000268)  : begin
                            HrStr := 'ERROR_PWD_TOO_RECENT' ;
                            HrDescr := 'The policy of your user account does not allow you to change passwords too frequently.' +
                                       'This is done to prevent users from changing back to a familiar, but potentially discovered, password.' +
                                       'If you feel your password has been compromised, contact your administrator immediately to have a new one assigned.';
                          end;
    LongInt($00000269)  : begin
                            HrStr := 'ERROR_PWD_HISTORY_CONFLICT' ;
                            HrDescr := 'You have attempted to change your password to one that you have used in the past.' +
                                       'The policy of your user account does not allow this.' +
                                       'Select a password that you have not previously used.';
                          end;
    LongInt($0000026A)  : begin
                            HrStr := 'ERROR_UNSUPPORTED_COMPRESSION' ;
                            HrDescr := 'The specified compression format is unsupported.';
                          end;
    LongInt($0000026B)  : begin
                            HrStr := 'ERROR_INVALID_HW_PROFILE' ;
                            HrDescr := 'The specified hardware profile configuration is invalid.';
                          end;
    LongInt($0000026C)  : begin
                            HrStr := 'ERROR_INVALID_PLUGPLAY_DEVICE_PATH' ;
                            HrDescr := 'The specified Plug and Play registry device path is invalid.';
                          end;
    LongInt($0000026D)  : begin
                            HrStr := 'ERROR_QUOTA_LIST_INCONSISTENT' ;
                            HrDescr := 'The specified quota list is internally inconsistent with its descriptor.';
                          end;
    LongInt($0000026E)  : begin
                            HrStr := 'ERROR_EVALUATION_EXPIRATION' ;
                            HrDescr := '{Windows Evaluation Notification} The evaluation period for this installation of Windows has expired.' +
                                       'This system will shut down in 1 hour.' +
                                       'To restore access to this installation of Windows, upgrade this installation using a licensed distribution of this product.';
                          end;
    LongInt($0000026F)  : begin
                            HrStr := 'ERROR_ILLEGAL_DLL_RELOCATION' ;
                            HrDescr := '{Illegal System DLL Relocation} The system DLL %hs was relocated in memory.' +
                                       'The application will not run properly.' +
                                       'The relocation occurred because the DLL %hs occupied an address range reserved for Windows system DLLs.' +
                                       'The vendor supplying the DLL should be contacted for a new DLL.';
                          end;
    LongInt($00000270)  : begin
                            HrStr := 'ERROR_DLL_INIT_FAILED_LOGOFF' ;
                            HrDescr := '{DLL Initialization Failed} The application failed to initialize because the window station is shutting down.';
                          end;
    LongInt($00000271)  : begin
                            HrStr := 'ERROR_VALIDATE_CONTINUE' ;
                            HrDescr := 'The validation process needs to continue on to the next step.';
                          end;
    LongInt($00000272)  : begin
                            HrStr := 'ERROR_NO_MORE_MATCHES' ;
                            HrDescr := 'There are no more matches for the current index enumeration.';
                          end;
    LongInt($00000273)  : begin
                            HrStr := 'ERROR_RANGE_LIST_CONFLICT' ;
                            HrDescr := 'The range could not be added to the range list because of a conflict.';
                          end;
    LongInt($00000274)  : begin
                            HrStr := 'ERROR_SERVER_SID_MISMATCH' ;
                            HrDescr := 'The server process is running under a SID different than that required by the client.';
                          end;
    LongInt($00000275)  : begin
                            HrStr := 'ERROR_CANT_ENABLE_DENY_ONLY' ;
                            HrDescr := 'A group marked use for deny only cannot be enabled.';
                          end;
    LongInt($00000276)  : begin
                            HrStr := 'ERROR_FLOAT_MULTIPLE_FAULTS' ;
                            HrDescr := '{EXCEPTION} Multiple floating point faults.';
                          end;
    LongInt($00000277)  : begin
                            HrStr := 'ERROR_FLOAT_MULTIPLE_TRAPS' ;
                            HrDescr := '{EXCEPTION} Multiple floating point traps.';
                          end;
    LongInt($00000278)  : begin
                            HrStr := 'ERROR_NOINTERFACE' ;
                            HrDescr := 'The requested interface is not supported.';
                          end;
    LongInt($00000279)  : begin
                            HrStr := 'ERROR_DRIVER_FAILED_SLEEP' ;
                            HrDescr := '{System Standby Failed} The driver %hs does not support standby mode.' +
                                       'Updating this driver might allow the system to go to standby mode.';
                          end;
    LongInt($0000027A)  : begin
                            HrStr := 'ERROR_CORRUPT_SYSTEM_FILE' ;
                            HrDescr := 'The system file %1 has become corrupt and has been replaced.';
                          end;
    LongInt($0000027B)  : begin
                            HrStr := 'ERROR_COMMITMENT_MINIMUM' ;
                            HrDescr := '{Virtual Memory Minimum Too Low} Your system is low on virtual memory.' +
                                       'Windows is increasing the size of your virtual memory paging file.' +
                                       'During this process, memory requests for some applications might be denied.' +
                                       'For more information, see Help.';
                          end;
    LongInt($0000027C)  : begin
                            HrStr := 'ERROR_PNP_RESTART_ENUMERATION' ;
                            HrDescr := 'A device was removed so enumeration must be restarted.';
                          end;
    LongInt($0000027D)  : begin
                            HrStr := 'ERROR_SYSTEM_IMAGE_BAD_SIGNATURE' ;
                            HrDescr := '{Fatal System Error} The system image %s is not properly signed.' +
                                       'The file has been replaced with the signed file.' +
                                       'The system has been shut down.';
                          end;
    LongInt($0000027E)  : begin
                            HrStr := 'ERROR_PNP_REBOOT_REQUIRED' ;
                            HrDescr := 'Device will not start without a reboot.';
                          end;
    LongInt($0000027F)  : begin
                            HrStr := 'ERROR_INSUFFICIENT_POWER' ;
                            HrDescr := 'There is not enough power to complete the requested operation.';
                          end;
    LongInt($00000281)  : begin
                            HrStr := 'ERROR_SYSTEM_SHUTDOWN' ;
                            HrDescr := 'The system is in the process of shutting down.';
                          end;
    LongInt($00000282)  : begin
                            HrStr := 'ERROR_PORT_NOT_SET' ;
                            HrDescr := 'An attempt to remove a process DebugPort was made, but a port was not already associated with the process.';
                          end;
    LongInt($00000283)  : begin
                            HrStr := 'ERROR_DS_VERSION_CHECK_FAILURE' ;
                            HrDescr := 'This version of Windows is not compatible with the behavior version of directory forest, domain, or domain controller.';
                          end;
    LongInt($00000284)  : begin
                            HrStr := 'ERROR_RANGE_NOT_FOUND' ;
                            HrDescr := 'The specified range could not be found in the range list.';
                          end;
    LongInt($00000286)  : begin
                            HrStr := 'ERROR_NOT_SAFE_MODE_DRIVER' ;
                            HrDescr := 'The driver was not loaded because the system is booting into safe mode.';
                          end;
    LongInt($00000287)  : begin
                            HrStr := 'ERROR_FAILED_DRIVER_ENTRY' ;
                            HrDescr := 'The driver was not loaded because it failed its initialization call.';
                          end;
    LongInt($00000288)  : begin
                            HrStr := 'ERROR_DEVICE_ENUMERATION_ERROR' ;
                            HrDescr := 'The device encountered an error while applying power or reading the device configuration.' +
                                       'This might be caused by a failure of your hardware or by a poor connection.';
                          end;
    LongInt($00000289)  : begin
                            HrStr := 'ERROR_MOUNT_POINT_NOT_RESOLVED' ;
                            HrDescr := 'The create operation failed because the name contained at least one mount point that resolves to a volume to which the specified device object is not attached.';
                          end;
    LongInt($0000028A)  : begin
                            HrStr := 'ERROR_INVALID_DEVICE_OBJECT_PARAMETER' ;
                            HrDescr := 'The device object parameter is either not a valid device object or is not attached to the volume specified by the file name.';
                          end;
    LongInt($0000028B)  : begin
                            HrStr := 'ERROR_MCA_OCCURED' ;
                            HrDescr := 'A machine check error has occurred.' +
                                       'Check the system event log for additional information.';
                          end;
    LongInt($0000028C)  : begin
                            HrStr := 'ERROR_DRIVER_DATABASE_ERROR' ;
                            HrDescr := 'There was an error [%2] processing the driver database.';
                          end;
    LongInt($0000028D)  : begin
                            HrStr := 'ERROR_SYSTEM_HIVE_TOO_LARGE' ;
                            HrDescr := 'The system hive size has exceeded its limit.';
                          end;
    LongInt($0000028E)  : begin
                            HrStr := 'ERROR_DRIVER_FAILED_PRIOR_UNLOAD' ;
                            HrDescr := 'The driver could not be loaded because a previous version of the driver is still in memory.';
                          end;
    LongInt($0000028F)  : begin
                            HrStr := 'ERROR_VOLSNAP_PREPARE_HIBERNATE' ;
                            HrDescr := '{Volume Shadow Copy Service} Wait while the Volume Shadow Copy Service prepares volume %hs for hibernation.';
                          end;
    LongInt($00000290)  : begin
                            HrStr := 'ERROR_HIBERNATION_FAILURE' ;
                            HrDescr := 'The system has failed to hibernate (the error code is %hs).' +
                                       'Hibernation will be disabled until the system is restarted.';
                          end;
    LongInt($00000299)  : begin
                            HrStr := 'ERROR_FILE_SYSTEM_LIMITATION' ;
                            HrDescr := 'The requested operation could not be completed due to a file system limitation.';
                          end;
    LongInt($0000029C)  : begin
                            HrStr := 'ERROR_ASSERTION_FAILURE' ;
                            HrDescr := 'An assertion failure has occurred.';
                          end;
    LongInt($0000029D)  : begin
                            HrStr := 'ERROR_ACPI_ERROR' ;
                            HrDescr := 'An error occurred in the Advanced Configuration and Power Interface (ACPI) subsystem.';
                          end;
    LongInt($0000029E)  : begin
                            HrStr := 'ERROR_WOW_ASSERTION' ;
                            HrDescr := 'WOW assertion error.';
                          end;
    LongInt($0000029F)  : begin
                            HrStr := 'ERROR_PNP_BAD_MPS_TABLE' ;
                            HrDescr := 'A device is missing in the system BIOS MultiProcessor Specification (MPS) table.' +
                                       'This device will not be used.' +
                                       'Contact your system vendor for system BIOS update.';
                          end;
    LongInt($000002A0)  : begin
                            HrStr := 'ERROR_PNP_TRANSLATION_FAILED' ;
                            HrDescr := 'A translator failed to translate resources.';
                          end;
    LongInt($000002A1)  : begin
                            HrStr := 'ERROR_PNP_IRQ_TRANSLATION_FAILED' ;
                            HrDescr := 'An interrupt request (IRQ) translator failed to translate resources.';
                          end;
    LongInt($000002A2)  : begin
                            HrStr := 'ERROR_PNP_INVALID_ID' ;
                            HrDescr := 'Driver %2 returned invalid ID for a child device (%3).';
                          end;
    LongInt($000002A3)  : begin
                            HrStr := 'ERROR_WAKE_SYSTEM_DEBUGGER' ;
                            HrDescr := '{Kernel Debugger Awakened} the system debugger was awakened by an interrupt.';
                          end;
    LongInt($000002A4)  : begin
                            HrStr := 'ERROR_HANDLES_CLOSED' ;
                            HrDescr := '{Handles Closed} Handles to objects have been automatically closed because of the requested operation.';
                          end;
    LongInt($000002A5)  : begin
                            HrStr := 'ERROR_EXTRANEOUS_INFORMATION' ;
                            HrDescr := '{Too Much Information} The specified ACL contained more information than was expected.';
                          end;
    LongInt($000002A6)  : begin
                            HrStr := 'ERROR_RXACT_COMMIT_NECESSARY' ;
                            HrDescr := 'This warning level status indicates that the transaction state already exists for the registry subtree, but that a transaction commit was previously aborted.' +
                                       'The commit has NOT been completed, but it has not been rolled back either (so it can still be committed if desired).';
                          end;
    LongInt($000002A7)  : begin
                            HrStr := 'ERROR_MEDIA_CHECK' ;
                            HrDescr := '{Media Changed} The media might have changed.';
                          end;
    LongInt($000002A8)  : begin
                            HrStr := 'ERROR_GUID_SUBSTITUTION_MADE' ;
                            HrDescr := '{GUID Substitution} During the translation of a GUID to a Windows SID, no administratively defined GUID prefix was found.' +
                                       'A substitute prefix was used, which will not compromise system security.' +
                                       'However, this might provide more restrictive access than intended.';
                          end;
    LongInt($000002A9)  : begin
                            HrStr := 'ERROR_STOPPED_ON_SYMLINK' ;
                            HrDescr := 'The create operation stopped after reaching a symbolic link.';
                          end;
    LongInt($000002AA)  : begin
                            HrStr := 'ERROR_LONGJUMP' ;
                            HrDescr := 'A long jump has been executed.';
                          end;
    LongInt($000002AB)  : begin
                            HrStr := 'ERROR_PLUGPLAY_QUERY_VETOED' ;
                            HrDescr := 'The Plug and Play query operation was not successful.';
                          end;
    LongInt($000002AC)  : begin
                            HrStr := 'ERROR_UNWIND_CONSOLIDATE' ;
                            HrDescr := 'A frame consolidation has been executed.';
                          end;
    LongInt($000002AD)  : begin
                            HrStr := 'ERROR_REGISTRY_HIVE_RECOVERED' ;
                            HrDescr := '{Registry Hive Recovered} Registry hive (file): %hs was corrupted and it has been recovered.' +
                                       'Some data might have been lost.';
                          end;
    LongInt($000002AE)  : begin
                            HrStr := 'ERROR_DLL_MIGHT_BE_INSECURE' ;
                            HrDescr := 'The application is attempting to run executable code from the module %hs.' +
                                       'This might be insecure.' +
                                       'An alternative, %hs, is available.' +
                                       'Should the application use the secure module %hs?';
                          end;
    LongInt($000002AF)  : begin
                            HrStr := 'ERROR_DLL_MIGHT_BE_INCOMPATIBLE' ;
                            HrDescr := 'The application is loading executable code from the module %hs.' +
                                       'This is secure, but might be incompatible with previous releases of the operating system.' +
                                       'An alternative, %hs, is available.' +
                                       'Should the application use the secure module %hs?';
                          end;
    LongInt($000002B0)  : begin
                            HrStr := 'ERROR_DBG_EXCEPTION_NOT_HANDLED' ;
                            HrDescr := 'Debugger did not handle the exception.';
                          end;
    LongInt($000002B1)  : begin
                            HrStr := 'ERROR_DBG_REPLY_LATER' ;
                            HrDescr := 'Debugger will reply later.';
                          end;
    LongInt($000002B2)  : begin
                            HrStr := 'ERROR_DBG_UNABLE_TO_PROVIDE_HANDLE' ;
                            HrDescr := 'Debugger cannot provide handle.';
                          end;
    LongInt($000002B3)  : begin
                            HrStr := 'ERROR_DBG_TERMINATE_THREAD' ;
                            HrDescr := 'Debugger terminated thread.';
                          end;
    LongInt($000002B4)  : begin
                            HrStr := 'ERROR_DBG_TERMINATE_PROCESS' ;
                            HrDescr := 'Debugger terminated process.';
                          end;
    LongInt($000002B5)  : begin
                            HrStr := 'ERROR_DBG_CONTROL_C' ;
                            HrDescr := 'Debugger got control C.';
                          end;
    LongInt($000002B6)  : begin
                            HrStr := 'ERROR_DBG_PRINTEXCEPTION_C' ;
                            HrDescr := 'Debugger printed exception on control C.';
                          end;
    LongInt($000002B7)  : begin
                            HrStr := 'ERROR_DBG_RIPEXCEPTION' ;
                            HrDescr := 'Debugger received Routing Information Protocol (RIP) exception.';
                          end;
    LongInt($000002B8)  : begin
                            HrStr := 'ERROR_DBG_CONTROL_BREAK' ;
                            HrDescr := 'Debugger received control break.';
                          end;
    LongInt($000002B9)  : begin
                            HrStr := 'ERROR_DBG_COMMAND_EXCEPTION' ;
                            HrDescr := 'Debugger command communication exception.';
                          end;
    LongInt($000002BA)  : begin
                            HrStr := 'ERROR_OBJECT_NAME_EXISTS' ;
                            HrDescr := '{Object Exists} An attempt was made to create an object and the object name already existed.';
                          end;
    LongInt($000002BB)  : begin
                            HrStr := 'ERROR_THREAD_WAS_SUSPENDED' ;
                            HrDescr := '{Thread Suspended} A thread termination occurred while the thread was suspended.' +
                                       'The thread was resumed and termination proceeded.';
                          end;
    LongInt($000002BC)  : begin
                            HrStr := 'ERROR_IMAGE_NOT_AT_BASE' ;
                            HrDescr := '{Image Relocated} An image file could not be mapped at the address specified in the image file.' +
                                       'Local fixes must be performed on this image.';
                          end;
    LongInt($000002BD)  : begin
                            HrStr := 'ERROR_RXACT_STATE_CREATED' ;
                            HrDescr := 'This informational level status indicates that a specified registry subtree transaction state did not yet exist and had to be created.';
                          end;
    LongInt($000002BE)  : begin
                            HrStr := 'ERROR_SEGMENT_NOTIFICATION' ;
                            HrDescr := '{Segment Load} A virtual DOS machine (VDM) is loading, unloading, or moving an MS-DOS or Win16 program segment image.' +
                                       'An exception is raised so a debugger can load, unload, or track symbols and breakpoints within these 16-bit segments.';
                          end;
    LongInt($000002BF)  : begin
                            HrStr := 'ERROR_BAD_CURRENT_DIRECTORY' ;
                            HrDescr := '{Invalid Current Directory} The process cannot switch to the startup current directory %hs.' +
                                       'Select OK to set current directory to %hs, or select CANCEL to exit.';
                          end;
    LongInt($000002C0)  : begin
                            HrStr := 'ERROR_FT_READ_RECOVERY_FROM_BACKUP' ;
                            HrDescr := '{Redundant Read} To satisfy a read request, the NT fault-tolerant file system successfully read the requested data from a redundant copy.' +
                                       'This was done because the file system encountered a failure on a member of the fault-tolerant volume, but it was unable to reassign the failing area of the device.';
                          end;
    LongInt($000002C1)  : begin
                            HrStr := 'ERROR_FT_WRITE_RECOVERY' ;
                            HrDescr := '{Redundant Write} To satisfy a write request, the Windows NT operating system fault-tolerant file system successfully wrote a redundant copy of the information.' +
                                       'This was done because the file system encountered a failure on a member of the fault-tolerant volume, but it was not able to reassign the failing area of the device.';
                          end;
    LongInt($000002C2)  : begin
                            HrStr := 'ERROR_IMAGE_MACHINE_TYPE_MISMATCH' ;
                            HrDescr := '{Machine Type Mismatch} The image file %hs is valid, but is for a machine type other than the current machine.' +
                                       'Select OK to continue, or CANCEL to fail the DLL load.';
                          end;
    LongInt($000002C3)  : begin
                            HrStr := 'ERROR_RECEIVE_PARTIAL' ;
                            HrDescr := '{Partial Data Received} The network transport returned partial data to its client.' +
                                       'The remaining data will be sent later.';
                          end;
    LongInt($000002C4)  : begin
                            HrStr := 'ERROR_RECEIVE_EXPEDITED' ;
                            HrDescr := '{Expedited Data Received} The network transport returned data to its client that was marked as expedited by the remote system.';
                          end;
    LongInt($000002C5)  : begin
                            HrStr := 'ERROR_RECEIVE_PARTIAL_EXPEDITED' ;
                            HrDescr := '{Partial Expedited Data Received} The network transport returned partial data to its client and this data was marked as expedited by the remote system.' +
                                       'The remaining data will be sent later.';
                          end;
    LongInt($000002C6)  : begin
                            HrStr := 'ERROR_EVENT_DONE' ;
                            HrDescr := '{TDI Event Done} The TDI indication has completed successfully.';
                          end;
    LongInt($000002C7)  : begin
                            HrStr := 'ERROR_EVENT_PENDING' ;
                            HrDescr := '{TDI Event Pending} The TDI indication has entered the pending state.';
                          end;
    LongInt($000002C8)  : begin
                            HrStr := 'ERROR_CHECKING_FILE_SYSTEM' ;
                            HrDescr := 'Checking file system on %wZ.';
                          end;
    LongInt($000002C9)  : begin
                            HrStr := 'ERROR_FATAL_APP_EXIT' ;
                            HrDescr := '{Fatal Application Exit} %hs.';
                          end;
    LongInt($000002CA)  : begin
                            HrStr := 'ERROR_PREDEFINED_HANDLE' ;
                            HrDescr := 'The specified registry key is referenced by a predefined handle.';
                          end;
    LongInt($000002CB)  : begin
                            HrStr := 'ERROR_WAS_UNLOCKED' ;
                            HrDescr := '{Page Unlocked} The page protection of a locked page was changed to ''No Access'' and the page was unlocked from memory and from the process.';
                          end;
    LongInt($000002CD)  : begin
                            HrStr := 'ERROR_WAS_LOCKED' ;
                            HrDescr := '{Page Locked} One of the pages to lock was already locked.';
                          end;
    LongInt($000002CF)  : begin
                            HrStr := 'ERROR_ALREADY_WIN32' ;
                            HrDescr := 'The value already corresponds with a Win 32 error code.';
                          end;
    LongInt($000002D0)  : begin
                            HrStr := 'ERROR_IMAGE_MACHINE_TYPE_MISMATCH_EXE' ;
                            HrDescr := '{Machine Type Mismatch} The image file %hs is valid, but is for a machine type other than the current machine.';
                          end;
    LongInt($000002D1)  : begin
                            HrStr := 'ERROR_NO_YIELD_PERFORMED' ;
                            HrDescr := 'A yield execution was performed and no thread was available to run.';
                          end;
    LongInt($000002D2)  : begin
                            HrStr := 'ERROR_TIMER_RESUME_IGNORED' ;
                            HrDescr := 'The resume flag to a timer API was ignored.';
                          end;
    LongInt($000002D3)  : begin
                            HrStr := 'ERROR_ARBITRATION_UNHANDLED' ;
                            HrDescr := 'The arbiter has deferred arbitration of these resources to its parent.';
                          end;
    LongInt($000002D4)  : begin
                            HrStr := 'ERROR_CARDBUS_NOT_SUPPORTED' ;
                            HrDescr := 'The inserted CardBus device cannot be started because of a configuration error on %hs"."';
                          end;
    LongInt($000002D5)  : begin
                            HrStr := 'ERROR_MP_PROCESSOR_MISMATCH' ;
                            HrDescr := 'The CPUs in this multiprocessor system are not all the same revision level.' +
                                       'To use all processors the operating system restricts itself to the features of the least capable processor in the system.' +
                                       'If problems occur with this system, contact the CPU manufacturer to see if this mix of processors is supported.';
                          end;
    LongInt($000002D6)  : begin
                            HrStr := 'ERROR_HIBERNATED' ;
                            HrDescr := 'The system was put into hibernation.';
                          end;
    LongInt($000002D7)  : begin
                            HrStr := 'ERROR_RESUME_HIBERNATION' ;
                            HrDescr := 'The system was resumed from hibernation.';
                          end;
    LongInt($000002D8)  : begin
                            HrStr := 'ERROR_FIRMWARE_UPDATED' ;
                            HrDescr := 'Windows has detected that the system firmware (BIOS) was updated (previous firmware date = %2, current firmware date %3).';
                          end;
    LongInt($000002D9)  : begin
                            HrStr := 'ERROR_DRIVERS_LEAKING_LOCKED_PAGES' ;
                            HrDescr := 'A device driver is leaking locked I/O pages, causing system degradation.' +
                                       'The system has automatically enabled a tracking code to try and catch the culprit.';
                          end;
    LongInt($000002DA)  : begin
                            HrStr := 'ERROR_WAKE_SYSTEM' ;
                            HrDescr := 'The system has awoken.';
                          end;
    LongInt($000002DF)  : begin
                            HrStr := 'ERROR_ABANDONED_WAIT_0' ;
                            HrDescr := 'The call failed because the handle associated with it was closed.';
                          end;
    LongInt($000002E4)  : begin
                            HrStr := 'ERROR_ELEVATION_REQUIRED' ;
                            HrDescr := 'The requested operation requires elevation.';
                          end;
    LongInt($000002E5)  : begin
                            HrStr := 'ERROR_REPARSE' ;
                            HrDescr := 'A reparse should be performed by the object manager because the name of the file resulted in a symbolic link.';
                          end;
    LongInt($000002E6)  : begin
                            HrStr := 'ERROR_OPLOCK_BREAK_IN_PROGRESS' ;
                            HrDescr := 'An open/create operation completed while an oplock break is underway.';
                          end;
    LongInt($000002E7)  : begin
                            HrStr := 'ERROR_VOLUME_MOUNTED' ;
                            HrDescr := 'A new volume has been mounted by a file system.';
                          end;
    LongInt($000002E8)  : begin
                            HrStr := 'ERROR_RXACT_COMMITTED' ;
                            HrDescr := 'This success level status indicates that the transaction state already exists for the registry subtree, but that a transaction commit was previously aborted.' +
                                       'The commit has now been completed.';
                          end;
    LongInt($000002E9)  : begin
                            HrStr := 'ERROR_NOTIFY_CLEANUP' ;
                            HrDescr := 'This indicates that a notify change request has been completed due to closing the handle which made the notify change request.';
                          end;
    LongInt($000002EA)  : begin
                            HrStr := 'ERROR_PRIMARY_TRANSPORT_CONNECT_FAILED' ;
                            HrDescr := '{Connect Failure on Primary Transport} An attempt was made to connect to the remote server %hs on the primary transport, but the connection failed.' +
                                       'The computer was able to connect on a secondary transport.';
                          end;
    LongInt($000002EB)  : begin
                            HrStr := 'ERROR_PAGE_FAULT_TRANSITION' ;
                            HrDescr := 'Page fault was a transition fault.';
                          end;
    LongInt($000002EC)  : begin
                            HrStr := 'ERROR_PAGE_FAULT_DEMAND_ZERO' ;
                            HrDescr := 'Page fault was a demand zero fault.';
                          end;
    LongInt($000002ED)  : begin
                            HrStr := 'ERROR_PAGE_FAULT_COPY_ON_WRITE' ;
                            HrDescr := 'Page fault was a demand zero fault.';
                          end;
    LongInt($000002EE)  : begin
                            HrStr := 'ERROR_PAGE_FAULT_GUARD_PAGE' ;
                            HrDescr := 'Page fault was a demand zero fault.';
                          end;
    LongInt($000002EF)  : begin
                            HrStr := 'ERROR_PAGE_FAULT_PAGING_FILE' ;
                            HrDescr := 'Page fault was satisfied by reading from a secondary storage device.';
                          end;
    LongInt($000002F0)  : begin
                            HrStr := 'ERROR_CACHE_PAGE_LOCKED' ;
                            HrDescr := 'Cached page was locked during operation.';
                          end;
    LongInt($000002F1)  : begin
                            HrStr := 'ERROR_CRASH_DUMP' ;
                            HrDescr := 'Crash dump exists in paging file.';
                          end;
    LongInt($000002F2)  : begin
                            HrStr := 'ERROR_BUFFER_ALL_ZEROS' ;
                            HrDescr := 'Specified buffer contains all zeros.';
                          end;
    LongInt($000002F3)  : begin
                            HrStr := 'ERROR_REPARSE_OBJECT' ;
                            HrDescr := 'A reparse should be performed by the object manager because the name of the file resulted in a symbolic link.';
                          end;
    LongInt($000002F4)  : begin
                            HrStr := 'ERROR_RESOURCE_REQUIREMENTS_CHANGED' ;
                            HrDescr := 'The device has succeeded a query-stop and its resource requirements have changed.';
                          end;
    LongInt($000002F5)  : begin
                            HrStr := 'ERROR_TRANSLATION_COMPLETE' ;
                            HrDescr := 'The translator has translated these resources into the global space and no further translations should be performed.';
                          end;
    LongInt($000002F6)  : begin
                            HrStr := 'ERROR_NOTHING_TO_TERMINATE' ;
                            HrDescr := 'A process being terminated has no threads to terminate.';
                          end;
    LongInt($000002F7)  : begin
                            HrStr := 'ERROR_PROCESS_NOT_IN_JOB' ;
                            HrDescr := 'The specified process is not part of a job.';
                          end;
    LongInt($000002F8)  : begin
                            HrStr := 'ERROR_PROCESS_IN_JOB' ;
                            HrDescr := 'The specified process is part of a job.';
                          end;
    LongInt($000002F9)  : begin
                            HrStr := 'ERROR_VOLSNAP_HIBERNATE_READY' ;
                            HrDescr := '{Volume Shadow Copy Service} The system is now ready for hibernation.';
                          end;
    LongInt($000002FA)  : begin
                            HrStr := 'ERROR_FSFILTER_OP_COMPLETED_SUCCESSFULLY' ;
                            HrDescr := 'A file system or file system filter driver has successfully completed an FsFilter operation.';
                          end;
    LongInt($000002FB)  : begin
                            HrStr := 'ERROR_INTERRUPT_VECTOR_ALREADY_CONNECTED' ;
                            HrDescr := 'The specified interrupt vector was already connected.';
                          end;
    LongInt($000002FC)  : begin
                            HrStr := 'ERROR_INTERRUPT_STILL_CONNECTED' ;
                            HrDescr := 'The specified interrupt vector is still connected.';
                          end;
    LongInt($000002FD)  : begin
                            HrStr := 'ERROR_WAIT_FOR_OPLOCK' ;
                            HrDescr := 'An operation is blocked waiting for an oplock.';
                          end;
    LongInt($000002FE)  : begin
                            HrStr := 'ERROR_DBG_EXCEPTION_HANDLED' ;
                            HrDescr := 'Debugger handled exception.';
                          end;
    LongInt($000002FF)  : begin
                            HrStr := 'ERROR_DBG_CONTINUE' ;
                            HrDescr := 'Debugger continued.';
                          end;
    LongInt($00000300)  : begin
                            HrStr := 'ERROR_CALLBACK_POP_STACK' ;
                            HrDescr := 'An exception occurred in a user mode callback and the kernel callback frame should be removed.';
                          end;
    LongInt($00000301)  : begin
                            HrStr := 'ERROR_COMPRESSION_DISABLED' ;
                            HrDescr := 'Compression is disabled for this volume.';
                          end;
    LongInt($00000302)  : begin
                            HrStr := 'ERROR_CANTFETCHBACKWARDS' ;
                            HrDescr := 'The data provider cannot fetch backward through a result set.';
                          end;
    LongInt($00000303)  : begin
                            HrStr := 'ERROR_CANTSCROLLBACKWARDS' ;
                            HrDescr := 'The data provider cannot scroll backward through a result set.';
                          end;
    LongInt($00000304)  : begin
                            HrStr := 'ERROR_ROWSNOTRELEASED' ;
                            HrDescr := 'The data provider requires that previously fetched data is released before asking for more data.';
                          end;
    LongInt($00000305)  : begin
                            HrStr := 'ERROR_BAD_ACCESSOR_FLAGS' ;
                            HrDescr := 'The data provider was not able to interpret the flags set for a column binding in an accessor.';
                          end;
    LongInt($00000306)  : begin
                            HrStr := 'ERROR_ERRORS_ENCOUNTERED' ;
                            HrDescr := 'One or more errors occurred while processing the request.';
                          end;
    LongInt($00000307)  : begin
                            HrStr := 'ERROR_NOT_CAPABLE' ;
                            HrDescr := 'The implementation is not capable of performing the request.';
                          end;
    LongInt($00000308)  : begin
                            HrStr := 'ERROR_REQUEST_OUT_OF_SEQUENCE' ;
                            HrDescr := 'The client of a component requested an operation that is not valid given the state of the component instance.';
                          end;
    LongInt($00000309)  : begin
                            HrStr := 'ERROR_VERSION_PARSE_ERROR' ;
                            HrDescr := 'A version number could not be parsed.';
                          end;
    LongInt($0000030A)  : begin
                            HrStr := 'ERROR_BADSTARTPOSITION' ;
                            HrDescr := 'The iterator''s start position is invalid.';
                          end;
    LongInt($0000030B)  : begin
                            HrStr := 'ERROR_MEMORY_HARDWARE' ;
                            HrDescr := 'The hardware has reported an uncorrectable memory error.';
                          end;
    LongInt($0000030C)  : begin
                            HrStr := 'ERROR_DISK_REPAIR_DISABLED' ;
                            HrDescr := 'The attempted operation required self-healing to be enabled.';
                          end;
    LongInt($0000030D)  : begin
                            HrStr := 'ERROR_INSUFFICIENT_RESOURCE_FOR_SPECIFIED_SHARED_SECTION_SIZE' ;
                            HrDescr := 'The Desktop heap encountered an error while allocating session memory.' +
                                       'There is more information in the system event log.';
                          end;
    LongInt($0000030E)  : begin
                            HrStr := 'ERROR_SYSTEM_POWERSTATE_TRANSITION' ;
                            HrDescr := 'The system power state is transitioning from %2 to %3.';
                          end;
    LongInt($0000030F)  : begin
                            HrStr := 'ERROR_SYSTEM_POWERSTATE_COMPLEX_TRANSITION' ;
                            HrDescr := 'The system power state is transitioning from %2 to %3 but could enter %4.';
                          end;
    LongInt($00000310)  : begin
                            HrStr := 'ERROR_MCA_EXCEPTION' ;
                            HrDescr := 'A thread is getting dispatched with MCA EXCEPTION because of MCA.';
                          end;
    LongInt($00000311)  : begin
                            HrStr := 'ERROR_ACCESS_AUDIT_BY_POLICY' ;
                            HrDescr := 'Access to %1 is monitored by policy rule %2.';
                          end;
    LongInt($00000312)  : begin
                            HrStr := 'ERROR_ACCESS_DISABLED_NO_SAFER_UI_BY_POLICY' ;
                            HrDescr := 'Access to %1 has been restricted by your administrator by policy rule %2.';
                          end;
    LongInt($00000313)  : begin
                            HrStr := 'ERROR_ABANDON_HIBERFILE' ;
                            HrDescr := 'A valid hibernation file has been invalidated and should be abandoned.';
                          end;
    LongInt($00000314)  : begin
                            HrStr := 'ERROR_LOST_WRITEBEHIND_DATA_NETWORK_DISCONNECTED' ;
                            HrDescr := '{Delayed Write Failed} Windows was unable to save all the data for the file %hs; the data has been lost.' +
                                       'This error can be caused by network connectivity issues.' +
                                       'Try to save this file elsewhere.';
                          end;
    LongInt($00000315)  : begin
                            HrStr := 'ERROR_LOST_WRITEBEHIND_DATA_NETWORK_SERVER_ERROR' ;
                            HrDescr := '{Delayed Write Failed} Windows was unable to save all the data for the file %hs; the data has been lost.' +
                                       'This error was returned by the server on which the file exists.' +
                                       'Try to save this file elsewhere.';
                          end;
    LongInt($00000316)  : begin
                            HrStr := 'ERROR_LOST_WRITEBEHIND_DATA_LOCAL_DISK_ERROR' ;
                            HrDescr := '{Delayed Write Failed} Windows was unable to save all the data for the file %hs; the data has been lost.' +
                                       'This error can be caused if the device has been removed or the media is write-protected.';
                          end;
    LongInt($000003E2)  : begin
                            HrStr := 'ERROR_EA_ACCESS_DENIED' ;
                            HrDescr := 'Access to the extended attribute was denied.';
                          end;
    LongInt($000003E3)  : begin
                            HrStr := 'ERROR_OPERATION_ABORTED' ;
                            HrDescr := 'The I/O operation has been aborted because of either a thread exit or an application request.';
                          end;
    LongInt($000003E4)  : begin
                            HrStr := 'ERROR_IO_INCOMPLETE' ;
                            HrDescr := 'Overlapped I/O event is not in a signaled state.';
                          end;
    LongInt($000003E5)  : begin
                            HrStr := 'ERROR_IO_PENDING' ;
                            HrDescr := 'Overlapped I/O operation is in progress.';
                          end;
    LongInt($000003E6)  : begin
                            HrStr := 'ERROR_NOACCESS' ;
                            HrDescr := 'Invalid access to memory location.';
                          end;
    LongInt($000003E7)  : begin
                            HrStr := 'ERROR_SWAPERROR' ;
                            HrDescr := 'Error performing in-page operation.';
                          end;
    LongInt($000003E9)  : begin
                            HrStr := 'ERROR_STACK_OVERFLOW' ;
                            HrDescr := 'Recursion too deep; the stack overflowed.';
                          end;
    LongInt($000003EA)  : begin
                            HrStr := 'ERROR_INVALID_MESSAGE' ;
                            HrDescr := 'The window cannot act on the sent message.';
                          end;
    LongInt($000003EB)  : begin
                            HrStr := 'ERROR_CAN_NOT_COMPLETE' ;
                            HrDescr := 'Cannot complete this function.';
                          end;
    LongInt($000003EC)  : begin
                            HrStr := 'ERROR_INVALID_FLAGS' ;
                            HrDescr := 'Invalid flags.';
                          end;
    LongInt($000003ED)  : begin
                            HrStr := 'ERROR_UNRECOGNIZED_VOLUME' ;
                            HrDescr := 'The volume does not contain a recognized file system.' +
                                       'Be sure that all required file system drivers are loaded and that the volume is not corrupted.';
                          end;
    LongInt($000003EE)  : begin
                            HrStr := 'ERROR_FILE_INVALID' ;
                            HrDescr := 'The volume for a file has been externally altered so that the opened file is no longer valid.';
                          end;
    LongInt($000003EF)  : begin
                            HrStr := 'ERROR_FULLSCREEN_MODE' ;
                            HrDescr := 'The requested operation cannot be performed in full-screen mode.';
                          end;
    LongInt($000003F0)  : begin
                            HrStr := 'ERROR_NO_TOKEN' ;
                            HrDescr := 'An attempt was made to reference a token that does not exist.';
                          end;
    LongInt($000003F1)  : begin
                            HrStr := 'ERROR_BADDB' ;
                            HrDescr := 'The configuration registry database is corrupt.';
                          end;
    LongInt($000003F2)  : begin
                            HrStr := 'ERROR_BADKEY' ;
                            HrDescr := 'The configuration registry key is invalid.';
                          end;
    LongInt($000003F3)  : begin
                            HrStr := 'ERROR_CANTOPEN' ;
                            HrDescr := 'The configuration registry key could not be opened.';
                          end;
    LongInt($000003F4)  : begin
                            HrStr := 'ERROR_CANTREAD' ;
                            HrDescr := 'The configuration registry key could not be read.';
                          end;
    LongInt($000003F5)  : begin
                            HrStr := 'ERROR_CANTWRITE' ;
                            HrDescr := 'The configuration registry key could not be written.';
                          end;
    LongInt($000003F6)  : begin
                            HrStr := 'ERROR_REGISTRY_RECOVERED' ;
                            HrDescr := 'One of the files in the registry database had to be recovered by use of a log or alternate copy.' +
                                       'The recovery was successful.';
                          end;
    LongInt($000003F7)  : begin
                            HrStr := 'ERROR_REGISTRY_CORRUPT' ;
                            HrDescr := 'The registry is corrupted.' +
                                       'The structure of one of the files containing registry data is corrupted, or the system''s memory image of the file is corrupted, or the file could not be recovered because the alternate copy or log was absent or corrupted.';
                          end;
    LongInt($000003F8)  : begin
                            HrStr := 'ERROR_REGISTRY_IO_FAILED' ;
                            HrDescr := 'An I/O operation initiated by the registry failed and cannot be recovered.' +
                                       'The registry could not read in, write out, or flush one of the files that contain the system''s image of the registry.';
                          end;
    LongInt($000003F9)  : begin
                            HrStr := 'ERROR_NOT_REGISTRY_FILE' ;
                            HrDescr := 'The system attempted to load or restore a file into the registry, but the specified file is not in a registry file format.';
                          end;
    LongInt($000003FA)  : begin
                            HrStr := 'ERROR_KEY_DELETED' ;
                            HrDescr := 'Illegal operation attempted on a registry key that has been marked for deletion.';
                          end;
    LongInt($000003FB)  : begin
                            HrStr := 'ERROR_NO_LOG_SPACE' ;
                            HrDescr := 'System could not allocate the required space in a registry log.';
                          end;
    LongInt($000003FC)  : begin
                            HrStr := 'ERROR_KEY_HAS_CHILDREN' ;
                            HrDescr := 'Cannot create a symbolic link in a registry key that already has subkeys or values.';
                          end;
    LongInt($000003FD)  : begin
                            HrStr := 'ERROR_CHILD_MUST_BE_VOLATILE' ;
                            HrDescr := 'Cannot create a stable subkey under a volatile parent key.';
                          end;
    LongInt($000003FE)  : begin
                            HrStr := 'ERROR_NOTIFY_ENUM_DIR' ;
                            HrDescr := 'A notify change request is being completed and the information is not being returned in the caller''s buffer.' +
                                       'The caller now needs to enumerate the files to find the changes.';
                          end;
    LongInt($0000041B)  : begin
                            HrStr := 'ERROR_DEPENDENT_SERVICES_RUNNING' ;
                            HrDescr := 'A stop control has been sent to a service that other running services are dependent on.';
                          end;
    LongInt($0000041C)  : begin
                            HrStr := 'ERROR_INVALID_SERVICE_CONTROL' ;
                            HrDescr := 'The requested control is not valid for this service.';
                          end;
    LongInt($0000041D)  : begin
                            HrStr := 'ERROR_SERVICE_REQUEST_TIMEOUT' ;
                            HrDescr := 'The service did not respond to the start or control request in a timely fashion.';
                          end;
    LongInt($0000041E)  : begin
                            HrStr := 'ERROR_SERVICE_NO_THREAD' ;
                            HrDescr := 'A thread could not be created for the service.';
                          end;
    LongInt($0000041F)  : begin
                            HrStr := 'ERROR_SERVICE_DATABASE_LOCKED' ;
                            HrDescr := 'The service database is locked.';
                          end;
    LongInt($00000420)  : begin
                            HrStr := 'ERROR_SERVICE_ALREADY_RUNNING' ;
                            HrDescr := 'An instance of the service is already running.';
                          end;
    LongInt($00000421)  : begin
                            HrStr := 'ERROR_INVALID_SERVICE_ACCOUNT' ;
                            HrDescr := 'The account name is invalid or does not exist, or the password is invalid for the account name specified.';
                          end;
    LongInt($00000422)  : begin
                            HrStr := 'ERROR_SERVICE_DISABLED' ;
                            HrDescr := 'The service cannot be started, either because it is disabled or because it has no enabled devices associated with it.';
                          end;
    LongInt($00000423)  : begin
                            HrStr := 'ERROR_CIRCULAR_DEPENDENCY' ;
                            HrDescr := 'Circular service dependency was specified.';
                          end;
    LongInt($00000424)  : begin
                            HrStr := 'ERROR_SERVICE_DOES_NOT_EXIST' ;
                            HrDescr := 'The specified service does not exist as an installed service.';
                          end;
    LongInt($00000425)  : begin
                            HrStr := 'ERROR_SERVICE_CANNOT_ACCEPT_CTRL' ;
                            HrDescr := 'The service cannot accept control messages at this time.';
                          end;
    LongInt($00000426)  : begin
                            HrStr := 'ERROR_SERVICE_NOT_ACTIVE' ;
                            HrDescr := 'The service has not been started.';
                          end;
    LongInt($00000427)  : begin
                            HrStr := 'ERROR_FAILED_SERVICE_CONTROLLER_CONNECT' ;
                            HrDescr := 'The service process could not connect to the service controller.';
                          end;
    LongInt($00000428)  : begin
                            HrStr := 'ERROR_EXCEPTION_IN_SERVICE' ;
                            HrDescr := 'An exception occurred in the service when handling the control request.';
                          end;
    LongInt($00000429)  : begin
                            HrStr := 'ERROR_DATABASE_DOES_NOT_EXIST' ;
                            HrDescr := 'The database specified does not exist.';
                          end;
    LongInt($0000042A)  : begin
                            HrStr := 'ERROR_SERVICE_SPECIFIC_ERROR' ;
                            HrDescr := 'The service has returned a service-specific error code.';
                          end;
    LongInt($0000042B)  : begin
                            HrStr := 'ERROR_PROCESS_ABORTED' ;
                            HrDescr := 'The process terminated unexpectedly.';
                          end;
    LongInt($0000042C)  : begin
                            HrStr := 'ERROR_SERVICE_DEPENDENCY_FAIL' ;
                            HrDescr := 'The dependency service or group failed to start.';
                          end;
    LongInt($0000042D)  : begin
                            HrStr := 'ERROR_SERVICE_LOGON_FAILED' ;
                            HrDescr := 'The service did not start due to a logon failure.';
                          end;
    LongInt($0000042E)  : begin
                            HrStr := 'ERROR_SERVICE_START_HANG' ;
                            HrDescr := 'After starting, the service stopped responding in a start-pending state.';
                          end;
    LongInt($0000042F)  : begin
                            HrStr := 'ERROR_INVALID_SERVICE_LOCK' ;
                            HrDescr := 'The specified service database lock is invalid.';
                          end;
    LongInt($00000430)  : begin
                            HrStr := 'ERROR_SERVICE_MARKED_FOR_DELETE' ;
                            HrDescr := 'The specified service has been marked for deletion.';
                          end;
    LongInt($00000431)  : begin
                            HrStr := 'ERROR_SERVICE_EXISTS' ;
                            HrDescr := 'The specified service already exists.';
                          end;
    LongInt($00000432)  : begin
                            HrStr := 'ERROR_ALREADY_RUNNING_LKG' ;
                            HrDescr := 'The system is currently running with the last-known-good configuration.';
                          end;
    LongInt($00000433)  : begin
                            HrStr := 'ERROR_SERVICE_DEPENDENCY_DELETED' ;
                            HrDescr := 'The dependency service does not exist or has been marked for deletion.';
                          end;
    LongInt($00000434)  : begin
                            HrStr := 'ERROR_BOOT_ALREADY_ACCEPTED' ;
                            HrDescr := 'The current boot has already been accepted for use as the last-known-good control set.';
                          end;
    LongInt($00000435)  : begin
                            HrStr := 'ERROR_SERVICE_NEVER_STARTED' ;
                            HrDescr := 'No attempts to start the service have been made since the last boot.';
                          end;
    LongInt($00000436)  : begin
                            HrStr := 'ERROR_DUPLICATE_SERVICE_NAME' ;
                            HrDescr := 'The name is already in use as either a service name or a service display name.';
                          end;
    LongInt($00000437)  : begin
                            HrStr := 'ERROR_DIFFERENT_SERVICE_ACCOUNT' ;
                            HrDescr := 'The account specified for this service is different from the account specified for other services running in the same process.';
                          end;
    LongInt($00000438)  : begin
                            HrStr := 'ERROR_CANNOT_DETECT_DRIVER_FAILURE' ;
                            HrDescr := 'Failure actions can only be set for Win32 services, not for drivers.';
                          end;
    LongInt($00000439)  : begin
                            HrStr := 'ERROR_CANNOT_DETECT_PROCESS_ABORT' ;
                            HrDescr := 'This service runs in the same process as the service control manager.' +
                                       'Therefore, the service control manager cannot take action if this service''s process terminates unexpectedly.';
                          end;
    LongInt($0000043A)  : begin
                            HrStr := 'ERROR_NO_RECOVERY_PROGRAM' ;
                            HrDescr := 'No recovery program has been configured for this service.';
                          end;
    LongInt($0000043B)  : begin
                            HrStr := 'ERROR_SERVICE_NOT_IN_EXE' ;
                            HrDescr := 'The executable program that this service is configured to run in does not implement the service.';
                          end;
    LongInt($0000043C)  : begin
                            HrStr := 'ERROR_NOT_SAFEBOOT_SERVICE' ;
                            HrDescr := 'This service cannot be started in Safe Mode.';
                          end;
    LongInt($0000044C)  : begin
                            HrStr := 'ERROR_END_OF_MEDIA' ;
                            HrDescr := 'The physical end of the tape has been reached.';
                          end;
    LongInt($0000044D)  : begin
                            HrStr := 'ERROR_FILEMARK_DETECTED' ;
                            HrDescr := 'A tape access reached a filemark.';
                          end;
    LongInt($0000044E)  : begin
                            HrStr := 'ERROR_BEGINNING_OF_MEDIA' ;
                            HrDescr := 'The beginning of the tape or a partition was encountered.';
                          end;
    LongInt($0000044F)  : begin
                            HrStr := 'ERROR_SETMARK_DETECTED' ;
                            HrDescr := 'A tape access reached the end of a set of files.';
                          end;
    LongInt($00000450)  : begin
                            HrStr := 'ERROR_NO_DATA_DETECTED' ;
                            HrDescr := 'No more data is on the tape.';
                          end;
    LongInt($00000451)  : begin
                            HrStr := 'ERROR_PARTITION_FAILURE' ;
                            HrDescr := 'Tape could not be partitioned.';
                          end;
    LongInt($00000452)  : begin
                            HrStr := 'ERROR_INVALID_BLOCK_LENGTH' ;
                            HrDescr := 'When accessing a new tape of a multivolume partition, the current block size is incorrect.';
                          end;
    LongInt($00000453)  : begin
                            HrStr := 'ERROR_DEVICE_NOT_PARTITIONED' ;
                            HrDescr := 'Tape partition information could not be found when loading a tape.';
                          end;
    LongInt($00000454)  : begin
                            HrStr := 'ERROR_UNABLE_TO_LOCK_MEDIA' ;
                            HrDescr := 'Unable to lock the media eject mechanism.';
                          end;
    LongInt($00000455)  : begin
                            HrStr := 'ERROR_UNABLE_TO_UNLOAD_MEDIA' ;
                            HrDescr := 'Unable to unload the media.';
                          end;
    LongInt($00000456)  : begin
                            HrStr := 'ERROR_MEDIA_CHANGED' ;
                            HrDescr := 'The media in the drive might have changed.';
                          end;
    LongInt($00000457)  : begin
                            HrStr := 'ERROR_BUS_RESET' ;
                            HrDescr := 'The I/O bus was reset.';
                          end;
    LongInt($00000458)  : begin
                            HrStr := 'ERROR_NO_MEDIA_IN_DRIVE' ;
                            HrDescr := 'No media in drive.';
                          end;
    LongInt($00000459)  : begin
                            HrStr := 'ERROR_NO_UNICODE_TRANSLATION' ;
                            HrDescr := 'No mapping for the Unicode character exists in the target multibyte code page.';
                          end;
    LongInt($0000045A)  : begin
                            HrStr := 'ERROR_DLL_INIT_FAILED' ;
                            HrDescr := 'A DLL initialization routine failed.';
                          end;
    LongInt($0000045B)  : begin
                            HrStr := 'ERROR_SHUTDOWN_IN_PROGRESS' ;
                            HrDescr := 'A system shutdown is in progress.';
                          end;
    LongInt($0000045C)  : begin
                            HrStr := 'ERROR_NO_SHUTDOWN_IN_PROGRESS' ;
                            HrDescr := 'Unable to abort the system shutdown because no shutdown was in progress.';
                          end;
    LongInt($0000045D)  : begin
                            HrStr := 'ERROR_IO_DEVICE' ;
                            HrDescr := 'The request could not be performed because of an I/O device error.';
                          end;
    LongInt($0000045E)  : begin
                            HrStr := 'ERROR_SERIAL_NO_DEVICE' ;
                            HrDescr := 'No serial device was successfully initialized.' +
                                       'The serial driver will unload.';
                          end;
    LongInt($0000045F)  : begin
                            HrStr := 'ERROR_IRQ_BUSY' ;
                            HrDescr := 'Unable to open a device that was sharing an IRQ with other devices.' +
                                       'At least one other device that uses that IRQ was already opened.';
                          end;
    LongInt($00000460)  : begin
                            HrStr := 'ERROR_MORE_WRITES' ;
                            HrDescr := 'A serial I/O operation was completed by another write to the serial port.' +
                                       '(The IOCTL_SERIAL_XOFF_COUNTER reached zero.)';
                          end;
    LongInt($00000461)  : begin
                            HrStr := 'ERROR_COUNTER_TIMEOUT' ;
                            HrDescr := 'A serial I/O operation completed because the time-out period expired.' +
                                       '(The IOCTL_SERIAL_XOFF_COUNTER did not reach zero.)';
                          end;
    LongInt($00000462)  : begin
                            HrStr := 'ERROR_FLOPPY_ID_MARK_NOT_FOUND' ;
                            HrDescr := 'No ID address mark was found on the floppy disk.';
                          end;
    LongInt($00000463)  : begin
                            HrStr := 'ERROR_FLOPPY_WRONG_CYLINDER' ;
                            HrDescr := 'Mismatch between the floppy disk sector ID field and the floppy disk controller track address.';
                          end;
    LongInt($00000464)  : begin
                            HrStr := 'ERROR_FLOPPY_UNKNOWN_ERROR' ;
                            HrDescr := 'The floppy disk controller reported an error that is not recognized by the floppy disk driver.';
                          end;
    LongInt($00000465)  : begin
                            HrStr := 'ERROR_FLOPPY_BAD_REGISTERS' ;
                            HrDescr := 'The floppy disk controller returned inconsistent results in its registers.';
                          end;
    LongInt($00000466)  : begin
                            HrStr := 'ERROR_DISK_RECALIBRATE_FAILED' ;
                            HrDescr := 'While accessing the hard disk, a recalibrate operation failed, even after retries.';
                          end;
    LongInt($00000467)  : begin
                            HrStr := 'ERROR_DISK_OPERATION_FAILED' ;
                            HrDescr := 'While accessing the hard disk, a disk operation failed even after retries.';
                          end;
    LongInt($00000468)  : begin
                            HrStr := 'ERROR_DISK_RESET_FAILED' ;
                            HrDescr := 'While accessing the hard disk, a disk controller reset was needed, but that also failed.';
                          end;
    LongInt($00000469)  : begin
                            HrStr := 'ERROR_EOM_OVERFLOW' ;
                            HrDescr := 'Physical end of tape encountered.';
                          end;
    LongInt($0000046A)  : begin
                            HrStr := 'ERROR_NOT_ENOUGH_SERVER_MEMORY' ;
                            HrDescr := 'Not enough server storage is available to process this command.';
                          end;
    LongInt($0000046B)  : begin
                            HrStr := 'ERROR_POSSIBLE_DEADLOCK' ;
                            HrDescr := 'A potential deadlock condition has been detected.';
                          end;
    LongInt($0000046C)  : begin
                            HrStr := 'ERROR_MAPPED_ALIGNMENT' ;
                            HrDescr := 'The base address or the file offset specified does not have the proper alignment.';
                          end;
    LongInt($00000474)  : begin
                            HrStr := 'ERROR_SET_POWER_STATE_VETOED' ;
                            HrDescr := 'An attempt to change the system power state was vetoed by another application or driver.';
                          end;
    LongInt($00000475)  : begin
                            HrStr := 'ERROR_SET_POWER_STATE_FAILED' ;
                            HrDescr := 'The system BIOS failed an attempt to change the system power state.';
                          end;
    LongInt($00000476)  : begin
                            HrStr := 'ERROR_TOO_MANY_LINKS' ;
                            HrDescr := 'An attempt was made to create more links on a file than the file system supports.';
                          end;
    LongInt($0000047E)  : begin
                            HrStr := 'ERROR_OLD_WIN_VERSION' ;
                            HrDescr := 'The specified program requires a newer version of Windows.';
                          end;
    LongInt($0000047F)  : begin
                            HrStr := 'ERROR_APP_WRONG_OS' ;
                            HrDescr := 'The specified program is not a Windows or MS-DOS program.';
                          end;
    LongInt($00000480)  : begin
                            HrStr := 'ERROR_SINGLE_INSTANCE_APP' ;
                            HrDescr := 'Cannot start more than one instance of the specified program.';
                          end;
    LongInt($00000481)  : begin
                            HrStr := 'ERROR_RMODE_APP' ;
                            HrDescr := 'The specified program was written for an earlier version of Windows.';
                          end;
    LongInt($00000482)  : begin
                            HrStr := 'ERROR_INVALID_DLL' ;
                            HrDescr := 'One of the library files needed to run this application is damaged.';
                          end;
    LongInt($00000483)  : begin
                            HrStr := 'ERROR_NO_ASSOCIATION' ;
                            HrDescr := 'No application is associated with the specified file for this operation.';
                          end;
    LongInt($00000484)  : begin
                            HrStr := 'ERROR_DDE_FAIL' ;
                            HrDescr := 'An error occurred in sending the command to the application.';
                          end;
    LongInt($00000485)  : begin
                            HrStr := 'ERROR_DLL_NOT_FOUND' ;
                            HrDescr := 'One of the library files needed to run this application cannot be found.';
                          end;
    LongInt($00000486)  : begin
                            HrStr := 'ERROR_NO_MORE_USER_HANDLES' ;
                            HrDescr := 'The current process has used all of its system allowance of handles for Windows manager objects.';
                          end;
    LongInt($00000487)  : begin
                            HrStr := 'ERROR_MESSAGE_SYNC_ONLY' ;
                            HrDescr := 'The message can be used only with synchronous operations.';
                          end;
    LongInt($00000488)  : begin
                            HrStr := 'ERROR_SOURCE_ELEMENT_EMPTY' ;
                            HrDescr := 'The indicated source element has no media.';
                          end;
    LongInt($00000489)  : begin
                            HrStr := 'ERROR_DESTINATION_ELEMENT_FULL' ;
                            HrDescr := 'The indicated destination element already contains media.';
                          end;
    LongInt($0000048A)  : begin
                            HrStr := 'ERROR_ILLEGAL_ELEMENT_ADDRESS' ;
                            HrDescr := 'The indicated element does not exist.';
                          end;
    LongInt($0000048B)  : begin
                            HrStr := 'ERROR_MAGAZINE_NOT_PRESENT' ;
                            HrDescr := 'The indicated element is part of a magazine that is not present.';
                          end;
    LongInt($0000048C)  : begin
                            HrStr := 'ERROR_DEVICE_REINITIALIZATION_NEEDED' ;
                            HrDescr := 'The indicated device requires re-initialization due to hardware errors.';
                          end;
    LongInt($0000048D)  : begin
                            HrStr := 'ERROR_DEVICE_REQUIRES_CLEANING' ;
                            HrDescr := 'The device has indicated that cleaning is required before further operations are attempted.';
                          end;
    LongInt($0000048E)  : begin
                            HrStr := 'ERROR_DEVICE_DOOR_OPEN' ;
                            HrDescr := 'The device has indicated that its door is open.';
                          end;
    LongInt($0000048F)  : begin
                            HrStr := 'ERROR_DEVICE_NOT_CONNECTED' ;
                            HrDescr := 'The device is not connected.';
                          end;
    LongInt($00000490)  : begin
                            HrStr := 'ERROR_NOT_FOUND' ;
                            HrDescr := 'Element not found.';
                          end;
    LongInt($00000491)  : begin
                            HrStr := 'ERROR_NO_MATCH' ;
                            HrDescr := 'There was no match for the specified key in the index.';
                          end;
    LongInt($00000492)  : begin
                            HrStr := 'ERROR_SET_NOT_FOUND' ;
                            HrDescr := 'The property set specified does not exist on the object.';
                          end;
    LongInt($00000493)  : begin
                            HrStr := 'ERROR_POINT_NOT_FOUND' ;
                            HrDescr := 'The point passed to GetMouseMovePoints is not in the buffer.';
                          end;
    LongInt($00000494)  : begin
                            HrStr := 'ERROR_NO_TRACKING_SERVICE' ;
                            HrDescr := 'The tracking (workstation) service is not running.';
                          end;
    LongInt($00000495)  : begin
                            HrStr := 'ERROR_NO_VOLUME_ID' ;
                            HrDescr := 'The volume ID could not be found.';
                          end;
    LongInt($00000497)  : begin
                            HrStr := 'ERROR_UNABLE_TO_REMOVE_REPLACED' ;
                            HrDescr := 'Unable to remove the file to be replaced.';
                          end;
    LongInt($00000498)  : begin
                            HrStr := 'ERROR_UNABLE_TO_MOVE_REPLACEMENT' ;
                            HrDescr := 'Unable to move the replacement file to the file to be replaced.' +
                                       'The file to be replaced has retained its original name.';
                          end;
    LongInt($00000499)  : begin
                            HrStr := 'ERROR_UNABLE_TO_MOVE_REPLACEMENT_2' ;
                            HrDescr := 'Unable to move the replacement file to the file to be replaced.' +
                                       'The file to be replaced has been renamed using the backup name.';
                          end;
    LongInt($0000049A)  : begin
                            HrStr := 'ERROR_JOURNAL_DELETE_IN_PROGRESS' ;
                            HrDescr := 'The volume change journal is being deleted.';
                          end;
    LongInt($0000049B)  : begin
                            HrStr := 'ERROR_JOURNAL_NOT_ACTIVE' ;
                            HrDescr := 'The volume change journal is not active.';
                          end;
    LongInt($0000049C)  : begin
                            HrStr := 'ERROR_POTENTIAL_FILE_FOUND' ;
                            HrDescr := 'A file was found, but it might not be the correct file.';
                          end;
    LongInt($0000049D)  : begin
                            HrStr := 'ERROR_JOURNAL_ENTRY_DELETED' ;
                            HrDescr := 'The journal entry has been deleted from the journal.';
                          end;
    LongInt($000004A6)  : begin
                            HrStr := 'ERROR_SHUTDOWN_IS_SCHEDULED' ;
                            HrDescr := 'A system shutdown has already been scheduled.';
                          end;
    LongInt($000004A7)  : begin
                            HrStr := 'ERROR_SHUTDOWN_USERS_LOGGED_ON' ;
                            HrDescr := 'The system shutdown cannot be initiated because there are other users logged on to the computer.';
                          end;
    LongInt($000004B0)  : begin
                            HrStr := 'ERROR_BAD_DEVICE' ;
                            HrDescr := 'The specified device name is invalid.';
                          end;
    LongInt($000004B1)  : begin
                            HrStr := 'ERROR_CONNECTION_UNAVAIL' ;
                            HrDescr := 'The device is not currently connected but it is a remembered connection.';
                          end;
    LongInt($000004B2)  : begin
                            HrStr := 'ERROR_DEVICE_ALREADY_REMEMBERED' ;
                            HrDescr := 'The local device name has a remembered connection to another network resource.';
                          end;
    LongInt($000004B3)  : begin
                            HrStr := 'ERROR_NO_NET_OR_BAD_PATH' ;
                            HrDescr := 'The network path was either typed incorrectly, does not exist, or the network provider is not currently available.' +
                                       'Try retyping the path or contact your network administrator.';
                          end;
    LongInt($000004B4)  : begin
                            HrStr := 'ERROR_BAD_PROVIDER' ;
                            HrDescr := 'The specified network provider name is invalid.';
                          end;
    LongInt($000004B5)  : begin
                            HrStr := 'ERROR_CANNOT_OPEN_PROFILE' ;
                            HrDescr := 'Unable to open the network connection profile.';
                          end;
    LongInt($000004B6)  : begin
                            HrStr := 'ERROR_BAD_PROFILE' ;
                            HrDescr := 'The network connection profile is corrupted.';
                          end;
    LongInt($000004B7)  : begin
                            HrStr := 'ERROR_NOT_CONTAINER' ;
                            HrDescr := 'Cannot enumerate a noncontainer.';
                          end;
    LongInt($000004B8)  : begin
                            HrStr := 'ERROR_EXTENDED_ERROR' ;
                            HrDescr := 'An extended error has occurred.';
                          end;
    LongInt($000004B9)  : begin
                            HrStr := 'ERROR_INVALID_GROUPNAME' ;
                            HrDescr := 'The format of the specified group name is invalid.';
                          end;
    LongInt($000004BA)  : begin
                            HrStr := 'ERROR_INVALID_COMPUTERNAME' ;
                            HrDescr := 'The format of the specified computer name is invalid.';
                          end;
    LongInt($000004BB)  : begin
                            HrStr := 'ERROR_INVALID_EVENTNAME' ;
                            HrDescr := 'The format of the specified event name is invalid.';
                          end;
    LongInt($000004BC)  : begin
                            HrStr := 'ERROR_INVALID_DOMAINNAME' ;
                            HrDescr := 'The format of the specified domain name is invalid.';
                          end;
    LongInt($000004BD)  : begin
                            HrStr := 'ERROR_INVALID_SERVICENAME' ;
                            HrDescr := 'The format of the specified service name is invalid.';
                          end;
    LongInt($000004BE)  : begin
                            HrStr := 'ERROR_INVALID_NETNAME' ;
                            HrDescr := 'The format of the specified network name is invalid.';
                          end;
    LongInt($000004BF)  : begin
                            HrStr := 'ERROR_INVALID_SHARENAME' ;
                            HrDescr := 'The format of the specified share name is invalid.';
                          end;
    LongInt($000004C0)  : begin
                            HrStr := 'ERROR_INVALID_PASSWORDNAME' ;
                            HrDescr := 'The format of the specified password is invalid.';
                          end;
    LongInt($000004C1)  : begin
                            HrStr := 'ERROR_INVALID_MESSAGENAME' ;
                            HrDescr := 'The format of the specified message name is invalid.';
                          end;
    LongInt($000004C2)  : begin
                            HrStr := 'ERROR_INVALID_MESSAGEDEST' ;
                            HrDescr := 'The format of the specified message destination is invalid.';
                          end;
    LongInt($000004C3)  : begin
                            HrStr := 'ERROR_SESSION_CREDENTIAL_CONFLICT' ;
                            HrDescr := 'Multiple connections to a server or shared resource by the same user, using more than one user name, are not allowed.' +
                                       'Disconnect all previous connections to the server or shared resource and try again.';
                          end;
    LongInt($000004C4)  : begin
                            HrStr := 'ERROR_REMOTE_SESSION_LIMIT_EXCEEDED' ;
                            HrDescr := 'An attempt was made to establish a session to a network server, but there are already too many sessions established to that server.';
                          end;
    LongInt($000004C5)  : begin
                            HrStr := 'ERROR_DUP_DOMAINNAME' ;
                            HrDescr := 'The workgroup or domain name is already in use by another computer on the network.';
                          end;
    LongInt($000004C6)  : begin
                            HrStr := 'ERROR_NO_NETWORK' ;
                            HrDescr := 'The network is not present or not started.';
                          end;
    LongInt($000004C7)  : begin
                            HrStr := 'ERROR_CANCELLED' ;
                            HrDescr := 'The operation was canceled by the user.';
                          end;
    LongInt($000004C8)  : begin
                            HrStr := 'ERROR_USER_MAPPED_FILE' ;
                            HrDescr := 'The requested operation cannot be performed on a file with a user-mapped section open.';
                          end;
    LongInt($000004C9)  : begin
                            HrStr := 'ERROR_CONNECTION_REFUSED' ;
                            HrDescr := 'The remote system refused the network connection.';
                          end;
    LongInt($000004CA)  : begin
                            HrStr := 'ERROR_GRACEFUL_DISCONNECT' ;
                            HrDescr := 'The network connection was gracefully closed.';
                          end;
    LongInt($000004CB)  : begin
                            HrStr := 'ERROR_ADDRESS_ALREADY_ASSOCIATED' ;
                            HrDescr := 'The network transport endpoint already has an address associated with it.';
                          end;
    LongInt($000004CC)  : begin
                            HrStr := 'ERROR_ADDRESS_NOT_ASSOCIATED' ;
                            HrDescr := 'An address has not yet been associated with the network endpoint.';
                          end;
    LongInt($000004CD)  : begin
                            HrStr := 'ERROR_CONNECTION_INVALID' ;
                            HrDescr := 'An operation was attempted on a nonexistent network connection.';
                          end;
    LongInt($000004CE)  : begin
                            HrStr := 'ERROR_CONNECTION_ACTIVE' ;
                            HrDescr := 'An invalid operation was attempted on an active network connection.';
                          end;
    LongInt($000004CF)  : begin
                            HrStr := 'ERROR_NETWORK_UNREACHABLE' ;
                            HrDescr := 'The network location cannot be reached.' +
                                       'For information about network troubleshooting, see Windows Help.';
                          end;
    LongInt($000004D0)  : begin
                            HrStr := 'ERROR_HOST_UNREACHABLE' ;
                            HrDescr := 'The network location cannot be reached.' +
                                       'For information about network troubleshooting, see Windows Help.';
                          end;
    LongInt($000004D1)  : begin
                            HrStr := 'ERROR_PROTOCOL_UNREACHABLE' ;
                            HrDescr := 'The network location cannot be reached.' +
                                       'For information about network troubleshooting, see Windows Help.';
                          end;
    LongInt($000004D2)  : begin
                            HrStr := 'ERROR_PORT_UNREACHABLE' ;
                            HrDescr := 'No service is operating at the destination network endpoint on the remote system.';
                          end;
    LongInt($000004D3)  : begin
                            HrStr := 'ERROR_REQUEST_ABORTED' ;
                            HrDescr := 'The request was aborted.';
                          end;
    LongInt($000004D4)  : begin
                            HrStr := 'ERROR_CONNECTION_ABORTED' ;
                            HrDescr := 'The network connection was aborted by the local system.';
                          end;
    LongInt($000004D5)  : begin
                            HrStr := 'ERROR_RETRY' ;
                            HrDescr := 'The operation could not be completed.' +
                                       'A retry should be performed.';
                          end;
    LongInt($000004D6)  : begin
                            HrStr := 'ERROR_CONNECTION_COUNT_LIMIT' ;
                            HrDescr := 'A connection to the server could not be made because the limit on the number of concurrent connections for this account has been reached.';
                          end;
    LongInt($000004D7)  : begin
                            HrStr := 'ERROR_LOGIN_TIME_RESTRICTION' ;
                            HrDescr := 'Attempting to log on during an unauthorized time of day for this account.';
                          end;
    LongInt($000004D8)  : begin
                            HrStr := 'ERROR_LOGIN_WKSTA_RESTRICTION' ;
                            HrDescr := 'The account is not authorized to log on from this station.';
                          end;
    LongInt($000004D9)  : begin
                            HrStr := 'ERROR_INCORRECT_ADDRESS' ;
                            HrDescr := 'The network address could not be used for the operation requested.';
                          end;
    LongInt($000004DA)  : begin
                            HrStr := 'ERROR_ALREADY_REGISTERED' ;
                            HrDescr := 'The service is already registered.';
                          end;
    LongInt($000004DB)  : begin
                            HrStr := 'ERROR_SERVICE_NOT_FOUND' ;
                            HrDescr := 'The specified service does not exist.';
                          end;
    LongInt($000004DC)  : begin
                            HrStr := 'ERROR_NOT_AUTHENTICATED' ;
                            HrDescr := 'The operation being requested was not performed because the user has not been authenticated.';
                          end;
    LongInt($000004DD)  : begin
                            HrStr := 'ERROR_NOT_LOGGED_ON' ;
                            HrDescr := 'The operation being requested was not performed because the user has not logged on to the network.' +
                                       'The specified service does not exist.';
                          end;
    LongInt($000004DE)  : begin
                            HrStr := 'ERROR_CONTINUE' ;
                            HrDescr := 'Continue with work in progress.';
                          end;
    LongInt($000004DF)  : begin
                            HrStr := 'ERROR_ALREADY_INITIALIZED' ;
                            HrDescr := 'An attempt was made to perform an initialization operation when initialization has already been completed.';
                          end;
    LongInt($000004E0)  : begin
                            HrStr := 'ERROR_NO_MORE_DEVICES' ;
                            HrDescr := 'No more local devices.';
                          end;
    LongInt($000004E1)  : begin
                            HrStr := 'ERROR_NO_SUCH_SITE' ;
                            HrDescr := 'The specified site does not exist.';
                          end;
    LongInt($000004E2)  : begin
                            HrStr := 'ERROR_DOMAIN_CONTROLLER_EXISTS' ;
                            HrDescr := 'A domain controller with the specified name already exists.';
                          end;
    LongInt($000004E3)  : begin
                            HrStr := 'ERROR_ONLY_IF_CONNECTED' ;
                            HrDescr := 'This operation is supported only when you are connected to the server.';
                          end;
    LongInt($000004E4)  : begin
                            HrStr := 'ERROR_OVERRIDE_NOCHANGES' ;
                            HrDescr := 'The group policy framework should call the extension even if there are no changes.';
                          end;
    LongInt($000004E5)  : begin
                            HrStr := 'ERROR_BAD_USER_PROFILE' ;
                            HrDescr := 'The specified user does not have a valid profile.';
                          end;
    LongInt($000004E6)  : begin
                            HrStr := 'ERROR_NOT_SUPPORTED_ON_SBS' ;
                            HrDescr := 'This operation is not supported on a computer running Windows Server 2003 operating system for Small Business Server.';
                          end;
    LongInt($000004E7)  : begin
                            HrStr := 'ERROR_SERVER_SHUTDOWN_IN_PROGRESS' ;
                            HrDescr := 'The server machine is shutting down.';
                          end;
    LongInt($000004E8)  : begin
                            HrStr := 'ERROR_HOST_DOWN' ;
                            HrDescr := 'The remote system is not available.' +
                                       'For information about network troubleshooting, see Windows Help.';
                          end;
    LongInt($000004E9)  : begin
                            HrStr := 'ERROR_NON_ACCOUNT_SID' ;
                            HrDescr := 'The security identifier provided is not from an account domain.';
                          end;
    LongInt($000004EA)  : begin
                            HrStr := 'ERROR_NON_DOMAIN_SID' ;
                            HrDescr := 'The security identifier provided does not have a domain component.';
                          end;
    LongInt($000004EB)  : begin
                            HrStr := 'ERROR_APPHELP_BLOCK' ;
                            HrDescr := 'AppHelp dialog canceled, thus preventing the application from starting.';
                          end;
    LongInt($000004EC)  : begin
                            HrStr := 'ERROR_ACCESS_DISABLED_BY_POLICY' ;
                            HrDescr := 'This program is blocked by Group Policy.' +
                                       'For more information, contact your system administrator.';
                          end;
    LongInt($000004ED)  : begin
                            HrStr := 'ERROR_REG_NAT_CONSUMPTION' ;
                            HrDescr := 'A program attempt to use an invalid register value.' +
                                       'Normally caused by an uninitialized register.' +
                                       'This error is Itanium specific.';
                          end;
    LongInt($000004EE)  : begin
                            HrStr := 'ERROR_CSCSHARE_OFFLINE' ;
                            HrDescr := 'The share is currently offline or does not exist.';
                          end;
    LongInt($000004EF)  : begin
                            HrStr := 'ERROR_PKINIT_FAILURE' ;
                            HrDescr := 'The Kerberos protocol encountered an error while validating the KDC certificate during smartcard logon.' +
                                       'There is more information in the system event log.';
                          end;
    LongInt($000004F0)  : begin
                            HrStr := 'ERROR_SMARTCARD_SUBSYSTEM_FAILURE' ;
                            HrDescr := 'The Kerberos protocol encountered an error while attempting to utilize the smartcard subsystem.';
                          end;
    LongInt($000004F1)  : begin
                            HrStr := 'ERROR_DOWNGRADE_DETECTED' ;
                            HrDescr := 'The system detected a possible attempt to compromise security.' +
                                       'Ensure that you can contact the server that authenticated you.';
                          end;
    LongInt($000004F7)  : begin
                            HrStr := 'ERROR_MACHINE_LOCKED' ;
                            HrDescr := 'The machine is locked and cannot be shut down without the force option.';
                          end;
    LongInt($000004F9)  : begin
                            HrStr := 'ERROR_CALLBACK_SUPPLIED_INVALID_DATA' ;
                            HrDescr := 'An application-defined callback gave invalid data when called.';
                          end;
    LongInt($000004FA)  : begin
                            HrStr := 'ERROR_SYNC_FOREGROUND_REFRESH_REQUIRED' ;
                            HrDescr := 'The Group Policy framework should call the extension in the synchronous foreground policy refresh.';
                          end;
    LongInt($000004FB)  : begin
                            HrStr := 'ERROR_DRIVER_BLOCKED' ;
                            HrDescr := 'This driver has been blocked from loading.';
                          end;
    LongInt($000004FC)  : begin
                            HrStr := 'ERROR_INVALID_IMPORT_OF_NON_DLL' ;
                            HrDescr := 'A DLL referenced a module that was neither a DLL nor the process''s executable image.';
                          end;
    LongInt($000004FD)  : begin
                            HrStr := 'ERROR_ACCESS_DISABLED_WEBBLADE' ;
                            HrDescr := 'Windows cannot open this program because it has been disabled.';
                          end;
    LongInt($000004FE)  : begin
                            HrStr := 'ERROR_ACCESS_DISABLED_WEBBLADE_TAMPER' ;
                            HrDescr := 'Windows cannot open this program because the license enforcement system has been tampered with or become corrupted.';
                          end;
    LongInt($000004FF)  : begin
                            HrStr := 'ERROR_RECOVERY_FAILURE' ;
                            HrDescr := 'A transaction recover failed.';
                          end;
    LongInt($00000500)  : begin
                            HrStr := 'ERROR_ALREADY_FIBER' ;
                            HrDescr := 'The current thread has already been converted to a fiber.';
                          end;
    LongInt($00000501)  : begin
                            HrStr := 'ERROR_ALREADY_THREAD' ;
                            HrDescr := 'The current thread has already been converted from a fiber.';
                          end;
    LongInt($00000502)  : begin
                            HrStr := 'ERROR_STACK_BUFFER_OVERRUN' ;
                            HrDescr := 'The system detected an overrun of a stack-based buffer in this application.' +
                                       'This overrun could potentially allow a malicious user to gain control of this application.';
                          end;
    LongInt($00000503)  : begin
                            HrStr := 'ERROR_PARAMETER_QUOTA_EXCEEDED' ;
                            HrDescr := 'Data present in one of the parameters is more than the function can operate on.';
                          end;
    LongInt($00000504)  : begin
                            HrStr := 'ERROR_DEBUGGER_INACTIVE' ;
                            HrDescr := 'An attempt to perform an operation on a debug object failed because the object is in the process of being deleted.';
                          end;
    LongInt($00000505)  : begin
                            HrStr := 'ERROR_DELAY_LOAD_FAILED' ;
                            HrDescr := 'An attempt to delay-load a .dll or get a function address in a delay-loaded .dll failed.';
                          end;
    LongInt($00000506)  : begin
                            HrStr := 'ERROR_VDM_DISALLOWED' ;
                            HrDescr := '%1 is a 16-bit application.' +
                                       'You do not have permissions to execute 16-bit applications.' +
                                       'Check your permissions with your system administrator.';
                          end;
    LongInt($00000507)  : begin
                            HrStr := 'ERROR_UNIDENTIFIED_ERROR' ;
                            HrDescr := 'Insufficient information exists to identify the cause of failure.';
                          end;
    LongInt($00000508)  : begin
                            HrStr := 'ERROR_INVALID_CRUNTIME_PARAMETER' ;
                            HrDescr := 'The parameter passed to a C runtime function is incorrect.';
                          end;
    LongInt($00000509)  : begin
                            HrStr := 'ERROR_BEYOND_VDL' ;
                            HrDescr := 'The operation occurred beyond the valid data length of the file.';
                          end;
    LongInt($0000050A)  : begin
                            HrStr := 'ERROR_INCOMPATIBLE_SERVICE_SID_TYPE' ;
                            HrDescr := 'The service start failed because one or more services in the same process have an incompatible service SID type setting.' +
                                       'A service with a restricted service SID type can only coexist in the same process with other services with a restricted SID type.';
                          end;
    LongInt($0000050B)  : begin
                            HrStr := 'ERROR_DRIVER_PROCESS_TERMINATED' ;
                            HrDescr := 'The process hosting the driver for this device has been terminated.';
                          end;
    LongInt($0000050C)  : begin
                            HrStr := 'ERROR_IMPLEMENTATION_LIMIT' ;
                            HrDescr := 'An operation attempted to exceed an implementation-defined limit.';
                          end;
    LongInt($0000050D)  : begin
                            HrStr := 'ERROR_PROCESS_IS_PROTECTED' ;
                            HrDescr := 'Either the target process, or the target thread''s containing process, is a protected process.';
                          end;
    LongInt($0000050E)  : begin
                            HrStr := 'ERROR_SERVICE_NOTIFY_CLIENT_LAGGING' ;
                            HrDescr := 'The service notification client is lagging too far behind the current state of services in the machine.';
                          end;
    LongInt($0000050F)  : begin
                            HrStr := 'ERROR_DISK_QUOTA_EXCEEDED' ;
                            HrDescr := 'An operation failed because the storage quota was exceeded.';
                          end;
    LongInt($00000510)  : begin
                            HrStr := 'ERROR_CONTENT_BLOCKED' ;
                            HrDescr := 'An operation failed because the content was blocked.';
                          end;
    LongInt($00000511)  : begin
                            HrStr := 'ERROR_INCOMPATIBLE_SERVICE_PRIVILEGE' ;
                            HrDescr := 'A privilege that the service requires to function properly does not exist in the service account configuration.' +
                                       'The Services Microsoft Management Console (MMC) snap-in (Services.msc) and the Local Security Settings MMC snap-in (Secpol.msc) can be used to view the service configuration and the account configuration.';
                          end;
    LongInt($00000513)  : begin
                            HrStr := 'ERROR_INVALID_LABEL' ;
                            HrDescr := 'Indicates a particular SID cannot be assigned as the label of an object.';
                          end;
    LongInt($00000514)  : begin
                            HrStr := 'ERROR_NOT_ALL_ASSIGNED' ;
                            HrDescr := 'Not all privileges or groups referenced are assigned to the caller.';
                          end;
    LongInt($00000515)  : begin
                            HrStr := 'ERROR_SOME_NOT_MAPPED' ;
                            HrDescr := 'Some mapping between account names and SIDs was not done.';
                          end;
    LongInt($00000516)  : begin
                            HrStr := 'ERROR_NO_QUOTAS_FOR_ACCOUNT' ;
                            HrDescr := 'No system quota limits are specifically set for this account.';
                          end;
    LongInt($00000517)  : begin
                            HrStr := 'ERROR_LOCAL_USER_SESSION_KEY' ;
                            HrDescr := 'No encryption key is available.' +
                                       'A well-known encryption key was returned.';
                          end;
    LongInt($00000518)  : begin
                            HrStr := 'ERROR_NULL_LM_PASSWORD' ;
                            HrDescr := 'The password is too complex to be converted to a LAN Manager password.' +
                                       'The LAN Manager password returned is a null string.';
                          end;
    LongInt($00000519)  : begin
                            HrStr := 'ERROR_UNKNOWN_REVISION' ;
                            HrDescr := 'The revision level is unknown.';
                          end;
    LongInt($0000051A)  : begin
                            HrStr := 'ERROR_REVISION_MISMATCH' ;
                            HrDescr := 'Indicates two revision levels are incompatible.';
                          end;
    LongInt($0000051B)  : begin
                            HrStr := 'ERROR_INVALID_OWNER' ;
                            HrDescr := 'This SID cannot be assigned as the owner of this object.';
                          end;
    LongInt($0000051C)  : begin
                            HrStr := 'ERROR_INVALID_PRIMARY_GROUP' ;
                            HrDescr := 'This SID cannot be assigned as the primary group of an object.';
                          end;
    LongInt($0000051D)  : begin
                            HrStr := 'ERROR_NO_IMPERSONATION_TOKEN' ;
                            HrDescr := 'An attempt has been made to operate on an impersonation token by a thread that is not currently impersonating a client.';
                          end;
    LongInt($0000051E)  : begin
                            HrStr := 'ERROR_CANT_DISABLE_MANDATORY' ;
                            HrDescr := 'The group cannot be disabled.';
                          end;
    LongInt($0000051F)  : begin
                            HrStr := 'ERROR_NO_LOGON_SERVERS' ;
                            HrDescr := 'There are currently no logon servers available to service the logon request.';
                          end;
    LongInt($00000520)  : begin
                            HrStr := 'ERROR_NO_SUCH_LOGON_SESSION' ;
                            HrDescr := 'A specified logon session does not exist.' +
                                       'It might already have been terminated.';
                          end;
    LongInt($00000521)  : begin
                            HrStr := 'ERROR_NO_SUCH_PRIVILEGE' ;
                            HrDescr := 'A specified privilege does not exist.';
                          end;
    LongInt($00000522)  : begin
                            HrStr := 'ERROR_PRIVILEGE_NOT_HELD' ;
                            HrDescr := 'A required privilege is not held by the client.';
                          end;
    LongInt($00000523)  : begin
                            HrStr := 'ERROR_INVALID_ACCOUNT_NAME' ;
                            HrDescr := 'The name provided is not a properly formed account name.';
                          end;
    LongInt($00000524)  : begin
                            HrStr := 'ERROR_USER_EXISTS' ;
                            HrDescr := 'The specified account already exists.';
                          end;
    LongInt($00000525)  : begin
                            HrStr := 'ERROR_NO_SUCH_USER' ;
                            HrDescr := 'The specified account does not exist.';
                          end;
    LongInt($00000526)  : begin
                            HrStr := 'ERROR_GROUP_EXISTS' ;
                            HrDescr := 'The specified group already exists.';
                          end;
    LongInt($00000527)  : begin
                            HrStr := 'ERROR_NO_SUCH_GROUP' ;
                            HrDescr := 'The specified group does not exist.';
                          end;
    LongInt($00000528)  : begin
                            HrStr := 'ERROR_MEMBER_IN_GROUP' ;
                            HrDescr := 'Either the specified user account is already a member of the specified group, or the specified group cannot be deleted because it contains a member.';
                          end;
    LongInt($00000529)  : begin
                            HrStr := 'ERROR_MEMBER_NOT_IN_GROUP' ;
                            HrDescr := 'The specified user account is not a member of the specified group account.';
                          end;
    LongInt($0000052A)  : begin
                            HrStr := 'ERROR_LAST_ADMIN' ;
                            HrDescr := 'The last remaining administration account cannot be disabled or deleted.';
                          end;
    LongInt($0000052B)  : begin
                            HrStr := 'ERROR_WRONG_PASSWORD' ;
                            HrDescr := 'Unable to update the password.' +
                                       'The value provided as the current password is incorrect.';
                          end;
    LongInt($0000052C)  : begin
                            HrStr := 'ERROR_ILL_FORMED_PASSWORD' ;
                            HrDescr := 'Unable to update the password.' +
                                       'The value provided for the new password contains values that are not allowed in passwords.';
                          end;
    LongInt($0000052D)  : begin
                            HrStr := 'ERROR_PASSWORD_RESTRICTION' ;
                            HrDescr := 'Unable to update the password.' +
                                       'The value provided for the new password does not meet the length, complexity, or history requirements of the domain.';
                          end;
    LongInt($0000052E)  : begin
                            HrStr := 'ERROR_LOGON_FAILURE' ;
                            HrDescr := 'Logon failure: Unknown user name or bad password.';
                          end;
    LongInt($0000052F)  : begin
                            HrStr := 'ERROR_ACCOUNT_RESTRICTION' ;
                            HrDescr := 'Logon failure: User account restriction.' +
                                       'Possible reasons are blank passwords not allowed, logon hour restrictions, or a policy restriction has been enforced.';
                          end;
    LongInt($00000530)  : begin
                            HrStr := 'ERROR_INVALID_LOGON_HOURS' ;
                            HrDescr := 'Logon failure: Account logon time restriction violation.';
                          end;
    LongInt($00000531)  : begin
                            HrStr := 'ERROR_INVALID_WORKSTATION' ;
                            HrDescr := 'Logon failure: User not allowed to log on to this computer.';
                          end;
    LongInt($00000532)  : begin
                            HrStr := 'ERROR_PASSWORD_EXPIRED' ;
                            HrDescr := 'Logon failure: The specified account password has expired.';
                          end;
    LongInt($00000533)  : begin
                            HrStr := 'ERROR_ACCOUNT_DISABLED' ;
                            HrDescr := 'Logon failure: Account currently disabled.';
                          end;
    LongInt($00000534)  : begin
                            HrStr := 'ERROR_NONE_MAPPED' ;
                            HrDescr := 'No mapping between account names and SIDs was done.';
                          end;
    LongInt($00000535)  : begin
                            HrStr := 'ERROR_TOO_MANY_LUIDS_REQUESTED' ;
                            HrDescr := 'Too many local user identifiers (LUIDs) were requested at one time.';
                          end;
    LongInt($00000536)  : begin
                            HrStr := 'ERROR_LUIDS_EXHAUSTED' ;
                            HrDescr := 'No more LUIDs are available.';
                          end;
    LongInt($00000537)  : begin
                            HrStr := 'ERROR_INVALID_SUB_AUTHORITY' ;
                            HrDescr := 'The sub-authority part of an SID is invalid for this particular use.';
                          end;
    LongInt($00000538)  : begin
                            HrStr := 'ERROR_INVALID_ACL' ;
                            HrDescr := 'The ACL structure is invalid.';
                          end;
    LongInt($00000539)  : begin
                            HrStr := 'ERROR_INVALID_SID' ;
                            HrDescr := 'The SID structure is invalid.';
                          end;
    LongInt($0000053A)  : begin
                            HrStr := 'ERROR_INVALID_SECURITY_DESCR' ;
                            HrDescr := 'The security descriptor structure is invalid.';
                          end;
    LongInt($0000053C)  : begin
                            HrStr := 'ERROR_BAD_INHERITANCE_ACL' ;
                            HrDescr := 'The inherited ACL or ACE could not be built.';
                          end;
    LongInt($0000053D)  : begin
                            HrStr := 'ERROR_SERVER_DISABLED' ;
                            HrDescr := 'The server is currently disabled.';
                          end;
    LongInt($0000053E)  : begin
                            HrStr := 'ERROR_SERVER_NOT_DISABLED' ;
                            HrDescr := 'The server is currently enabled.';
                          end;
    LongInt($0000053F)  : begin
                            HrStr := 'ERROR_INVALID_ID_AUTHORITY' ;
                            HrDescr := 'The value provided was an invalid value for an identifier authority.';
                          end;
    LongInt($00000540)  : begin
                            HrStr := 'ERROR_ALLOTTED_SPACE_EXCEEDED' ;
                            HrDescr := 'No more memory is available for security information updates.';
                          end;
    LongInt($00000541)  : begin
                            HrStr := 'ERROR_INVALID_GROUP_ATTRIBUTES' ;
                            HrDescr := 'The specified attributes are invalid, or incompatible with the attributes for the group as a whole.';
                          end;
    LongInt($00000542)  : begin
                            HrStr := 'ERROR_BAD_IMPERSONATION_LEVEL' ;
                            HrDescr := 'Either a required impersonation level was not provided, or the provided impersonation level is invalid.';
                          end;
    LongInt($00000543)  : begin
                            HrStr := 'ERROR_CANT_OPEN_ANONYMOUS' ;
                            HrDescr := 'Cannot open an anonymous level security token.';
                          end;
    LongInt($00000544)  : begin
                            HrStr := 'ERROR_BAD_VALIDATION_CLASS' ;
                            HrDescr := 'The validation information class requested was invalid.';
                          end;
    LongInt($00000545)  : begin
                            HrStr := 'ERROR_BAD_TOKEN_TYPE' ;
                            HrDescr := 'The type of the token is inappropriate for its attempted use.';
                          end;
    LongInt($00000546)  : begin
                            HrStr := 'ERROR_NO_SECURITY_ON_OBJECT' ;
                            HrDescr := 'Unable to perform a security operation on an object that has no associated security.';
                          end;
    LongInt($00000547)  : begin
                            HrStr := 'ERROR_CANT_ACCESS_DOMAIN_INFO' ;
                            HrDescr := 'Configuration information could not be read from the domain controller, either because the machine is unavailable, or access has been denied.';
                          end;
    LongInt($00000548)  : begin
                            HrStr := 'ERROR_INVALID_SERVER_STATE' ;
                            HrDescr := 'The SAM or local security authority (LSA) server was in the wrong state to perform the security operation.';
                          end;
    LongInt($00000549)  : begin
                            HrStr := 'ERROR_INVALID_DOMAIN_STATE' ;
                            HrDescr := 'The domain was in the wrong state to perform the security operation.';
                          end;
    LongInt($0000054A)  : begin
                            HrStr := 'ERROR_INVALID_DOMAIN_ROLE' ;
                            HrDescr := 'This operation is only allowed for the PDC of the domain.';
                          end;
    LongInt($0000054B)  : begin
                            HrStr := 'ERROR_NO_SUCH_DOMAIN' ;
                            HrDescr := 'The specified domain either does not exist or could not be contacted.';
                          end;
    LongInt($0000054C)  : begin
                            HrStr := 'ERROR_DOMAIN_EXISTS' ;
                            HrDescr := 'The specified domain already exists.';
                          end;
    LongInt($0000054D)  : begin
                            HrStr := 'ERROR_DOMAIN_LIMIT_EXCEEDED' ;
                            HrDescr := 'An attempt was made to exceed the limit on the number of domains per server.';
                          end;
    LongInt($0000054E)  : begin
                            HrStr := 'ERROR_INTERNAL_DB_CORRUPTION' ;
                            HrDescr := 'Unable to complete the requested operation because of either a catastrophic media failure or a data structure corruption on the disk.';
                          end;
    LongInt($0000054F)  : begin
                            HrStr := 'ERROR_INTERNAL_ERROR' ;
                            HrDescr := 'An internal error occurred.';
                          end;
    LongInt($00000550)  : begin
                            HrStr := 'ERROR_GENERIC_NOT_MAPPED' ;
                            HrDescr := 'Generic access types were contained in an access mask that should already be mapped to nongeneric types.';
                          end;
    LongInt($00000551)  : begin
                            HrStr := 'ERROR_BAD_DESCRIPTOR_FORMAT' ;
                            HrDescr := 'A security descriptor is not in the right format (absolute or self-relative).';
                          end;
    LongInt($00000552)  : begin
                            HrStr := 'ERROR_NOT_LOGON_PROCESS' ;
                            HrDescr := 'The requested action is restricted for use by logon processes only.' +
                                       'The calling process has not registered as a logon process.';
                          end;
    LongInt($00000553)  : begin
                            HrStr := 'ERROR_LOGON_SESSION_EXISTS' ;
                            HrDescr := 'Cannot start a new logon session with an ID that is already in use.';
                          end;
    LongInt($00000554)  : begin
                            HrStr := 'ERROR_NO_SUCH_PACKAGE' ;
                            HrDescr := 'A specified authentication package is unknown.';
                          end;
    LongInt($00000555)  : begin
                            HrStr := 'ERROR_BAD_LOGON_SESSION_STATE' ;
                            HrDescr := 'The logon session is not in a state that is consistent with the requested operation.';
                          end;
    LongInt($00000556)  : begin
                            HrStr := 'ERROR_LOGON_SESSION_COLLISION' ;
                            HrDescr := 'The logon session ID is already in use.';
                          end;
    LongInt($00000557)  : begin
                            HrStr := 'ERROR_INVALID_LOGON_TYPE' ;
                            HrDescr := 'A logon request contained an invalid logon type value.';
                          end;
    LongInt($00000558)  : begin
                            HrStr := 'ERROR_CANNOT_IMPERSONATE' ;
                            HrDescr := 'Unable to impersonate using a named pipe until data has been read from that pipe.';
                          end;
    LongInt($00000559)  : begin
                            HrStr := 'ERROR_RXACT_INVALID_STATE' ;
                            HrDescr := 'The transaction state of a registry subtree is incompatible with the requested operation.';
                          end;
    LongInt($0000055A)  : begin
                            HrStr := 'ERROR_RXACT_COMMIT_FAILURE' ;
                            HrDescr := 'An internal security database corruption has been encountered.';
                          end;
    LongInt($0000055B)  : begin
                            HrStr := 'ERROR_SPECIAL_ACCOUNT' ;
                            HrDescr := 'Cannot perform this operation on built-in accounts.';
                          end;
    LongInt($0000055C)  : begin
                            HrStr := 'ERROR_SPECIAL_GROUP' ;
                            HrDescr := 'Cannot perform this operation on this built-in special group.';
                          end;
    LongInt($0000055D)  : begin
                            HrStr := 'ERROR_SPECIAL_USER' ;
                            HrDescr := 'Cannot perform this operation on this built-in special user.';
                          end;
    LongInt($0000055E)  : begin
                            HrStr := 'ERROR_MEMBERS_PRIMARY_GROUP' ;
                            HrDescr := 'The user cannot be removed from a group because the group is currently the user''s primary group.';
                          end;
    LongInt($0000055F)  : begin
                            HrStr := 'ERROR_TOKEN_ALREADY_IN_USE' ;
                            HrDescr := 'The token is already in use as a primary token.';
                          end;
    LongInt($00000560)  : begin
                            HrStr := 'ERROR_NO_SUCH_ALIAS' ;
                            HrDescr := 'The specified local group does not exist.';
                          end;
    LongInt($00000561)  : begin
                            HrStr := 'ERROR_MEMBER_NOT_IN_ALIAS' ;
                            HrDescr := 'The specified account name is not a member of the group.';
                          end;
    LongInt($00000562)  : begin
                            HrStr := 'ERROR_MEMBER_IN_ALIAS' ;
                            HrDescr := 'The specified account name is already a member of the group.';
                          end;
    LongInt($00000563)  : begin
                            HrStr := 'ERROR_ALIAS_EXISTS' ;
                            HrDescr := 'The specified local group already exists.';
                          end;
    LongInt($00000564)  : begin
                            HrStr := 'ERROR_LOGON_NOT_GRANTED' ;
                            HrDescr := 'Logon failure: The user has not been granted the requested logon type at this computer.';
                          end;
    LongInt($00000565)  : begin
                            HrStr := 'ERROR_TOO_MANY_SECRETS' ;
                            HrDescr := 'The maximum number of secrets that can be stored in a single system has been exceeded.';
                          end;
    LongInt($00000566)  : begin
                            HrStr := 'ERROR_SECRET_TOO_LONG' ;
                            HrDescr := 'The length of a secret exceeds the maximum length allowed.';
                          end;
    LongInt($00000567)  : begin
                            HrStr := 'ERROR_INTERNAL_DB_ERROR' ;
                            HrDescr := 'The local security authority database contains an internal inconsistency.';
                          end;
    LongInt($00000568)  : begin
                            HrStr := 'ERROR_TOO_MANY_CONTEXT_IDS' ;
                            HrDescr := 'During a logon attempt, the user''s security context accumulated too many SIDs.';
                          end;
    LongInt($00000569)  : begin
                            HrStr := 'ERROR_LOGON_TYPE_NOT_GRANTED' ;
                            HrDescr := 'Logon failure: The user has not been granted the requested logon type at this computer.';
                          end;
    LongInt($0000056A)  : begin
                            HrStr := 'ERROR_NT_CROSS_ENCRYPTION_REQUIRED' ;
                            HrDescr := 'A cross-encrypted password is necessary to change a user password.';
                          end;
    LongInt($0000056B)  : begin
                            HrStr := 'ERROR_NO_SUCH_MEMBER' ;
                            HrDescr := 'A member could not be added to or removed from the local group because the member does not exist.';
                          end;
    LongInt($0000056C)  : begin
                            HrStr := 'ERROR_INVALID_MEMBER' ;
                            HrDescr := 'A new member could not be added to a local group because the member has the wrong account type.';
                          end;
    LongInt($0000056D)  : begin
                            HrStr := 'ERROR_TOO_MANY_SIDS' ;
                            HrDescr := 'Too many SIDs have been specified.';
                          end;
    LongInt($0000056E)  : begin
                            HrStr := 'ERROR_LM_CROSS_ENCRYPTION_REQUIRED' ;
                            HrDescr := 'A cross-encrypted password is necessary to change this user password.';
                          end;
    LongInt($0000056F)  : begin
                            HrStr := 'ERROR_NO_INHERITANCE' ;
                            HrDescr := 'Indicates an ACL contains no inheritable components.';
                          end;
    LongInt($00000570)  : begin
                            HrStr := 'ERROR_FILE_CORRUPT' ;
                            HrDescr := 'The file or directory is corrupted and unreadable.';
                          end;
    LongInt($00000571)  : begin
                            HrStr := 'ERROR_DISK_CORRUPT' ;
                            HrDescr := 'The disk structure is corrupted and unreadable.';
                          end;
    LongInt($00000572)  : begin
                            HrStr := 'ERROR_NO_USER_SESSION_KEY' ;
                            HrDescr := 'There is no user session key for the specified logon session.';
                          end;
    LongInt($00000573)  : begin
                            HrStr := 'ERROR_LICENSE_QUOTA_EXCEEDED' ;
                            HrDescr := 'The service being accessed is licensed for a particular number of connections.' +
                                       'No more connections can be made to the service at this time because the service has accepted the maximum number of connections.';
                          end;
    LongInt($00000574)  : begin
                            HrStr := 'ERROR_WRONG_TARGET_NAME' ;
                            HrDescr := 'Logon failure: The target account name is incorrect.';
                          end;
    LongInt($00000575)  : begin
                            HrStr := 'ERROR_MUTUAL_AUTH_FAILED' ;
                            HrDescr := 'Mutual authentication failed.' +
                                       'The server''s password is out of date at the domain controller.';
                          end;
    LongInt($00000576)  : begin
                            HrStr := 'ERROR_TIME_SKEW' ;
                            HrDescr := 'There is a time and/or date difference between the client and server.';
                          end;
    LongInt($00000577)  : begin
                            HrStr := 'ERROR_CURRENT_DOMAIN_NOT_ALLOWED' ;
                            HrDescr := 'This operation cannot be performed on the current domain.';
                          end;
    LongInt($00000578)  : begin
                            HrStr := 'ERROR_INVALID_WINDOW_HANDLE' ;
                            HrDescr := 'Invalid window handle.';
                          end;
    LongInt($00000579)  : begin
                            HrStr := 'ERROR_INVALID_MENU_HANDLE' ;
                            HrDescr := 'Invalid menu handle.';
                          end;
    LongInt($0000057A)  : begin
                            HrStr := 'ERROR_INVALID_CURSOR_HANDLE' ;
                            HrDescr := 'Invalid cursor handle.';
                          end;
    LongInt($0000057B)  : begin
                            HrStr := 'ERROR_INVALID_ACCEL_HANDLE' ;
                            HrDescr := 'Invalid accelerator table handle.';
                          end;
    LongInt($0000057C)  : begin
                            HrStr := 'ERROR_INVALID_HOOK_HANDLE' ;
                            HrDescr := 'Invalid hook handle.';
                          end;
    LongInt($0000057D)  : begin
                            HrStr := 'ERROR_INVALID_DWP_HANDLE' ;
                            HrDescr := 'Invalid handle to a multiple-window position structure.';
                          end;
    LongInt($0000057E)  : begin
                            HrStr := 'ERROR_TLW_WITH_WSCHILD' ;
                            HrDescr := 'Cannot create a top-level child window.';
                          end;
    LongInt($0000057F)  : begin
                            HrStr := 'ERROR_CANNOT_FIND_WND_CLASS' ;
                            HrDescr := 'Cannot find window class.';
                          end;
    LongInt($00000580)  : begin
                            HrStr := 'ERROR_WINDOW_OF_OTHER_THREAD' ;
                            HrDescr := 'Invalid window; it belongs to other thread.';
                          end;
    LongInt($00000581)  : begin
                            HrStr := 'ERROR_HOTKEY_ALREADY_REGISTERED' ;
                            HrDescr := 'Hot key is already registered.';
                          end;
    LongInt($00000582)  : begin
                            HrStr := 'ERROR_CLASS_ALREADY_EXISTS' ;
                            HrDescr := 'Class already exists.';
                          end;
    LongInt($00000583)  : begin
                            HrStr := 'ERROR_CLASS_DOES_NOT_EXIST' ;
                            HrDescr := 'Class does not exist.';
                          end;
    LongInt($00000584)  : begin
                            HrStr := 'ERROR_CLASS_HAS_WINDOWS' ;
                            HrDescr := 'Class still has open windows.';
                          end;
    LongInt($00000585)  : begin
                            HrStr := 'ERROR_INVALID_INDEX' ;
                            HrDescr := 'Invalid index.';
                          end;
    LongInt($00000586)  : begin
                            HrStr := 'ERROR_INVALID_ICON_HANDLE' ;
                            HrDescr := 'Invalid icon handle.';
                          end;
    LongInt($00000587)  : begin
                            HrStr := 'ERROR_PRIVATE_DIALOG_INDEX' ;
                            HrDescr := 'Using private DIALOG window words.';
                          end;
    LongInt($00000588)  : begin
                            HrStr := 'ERROR_LISTBOX_ID_NOT_FOUND' ;
                            HrDescr := 'The list box identifier was not found.';
                          end;
    LongInt($00000589)  : begin
                            HrStr := 'ERROR_NO_WILDCARD_CHARACTERS' ;
                            HrDescr := 'No wildcards were found.';
                          end;
    LongInt($0000058A)  : begin
                            HrStr := 'ERROR_CLIPBOARD_NOT_OPEN' ;
                            HrDescr := 'Thread does not have a clipboard open.';
                          end;
    LongInt($0000058B)  : begin
                            HrStr := 'ERROR_HOTKEY_NOT_REGISTERED' ;
                            HrDescr := 'Hot key is not registered.';
                          end;
    LongInt($0000058C)  : begin
                            HrStr := 'ERROR_WINDOW_NOT_DIALOG' ;
                            HrDescr := 'The window is not a valid dialog window.';
                          end;
    LongInt($0000058D)  : begin
                            HrStr := 'ERROR_CONTROL_ID_NOT_FOUND' ;
                            HrDescr := 'Control ID not found.';
                          end;
    LongInt($0000058E)  : begin
                            HrStr := 'ERROR_INVALID_COMBOBOX_MESSAGE' ;
                            HrDescr := 'Invalid message for a combo box because it does not have an edit control.';
                          end;
    LongInt($0000058F)  : begin
                            HrStr := 'ERROR_WINDOW_NOT_COMBOBOX' ;
                            HrDescr := 'The window is not a combo box.';
                          end;
    LongInt($00000590)  : begin
                            HrStr := 'ERROR_INVALID_EDIT_HEIGHT' ;
                            HrDescr := 'Height must be less than 256.';
                          end;
    LongInt($00000591)  : begin
                            HrStr := 'ERROR_DC_NOT_FOUND' ;
                            HrDescr := 'Invalid device context (DC) handle.';
                          end;
    LongInt($00000592)  : begin
                            HrStr := 'ERROR_INVALID_HOOK_FILTER' ;
                            HrDescr := 'Invalid hook procedure type.';
                          end;
    LongInt($00000593)  : begin
                            HrStr := 'ERROR_INVALID_FILTER_PROC' ;
                            HrDescr := 'Invalid hook procedure.';
                          end;
    LongInt($00000594)  : begin
                            HrStr := 'ERROR_HOOK_NEEDS_HMOD' ;
                            HrDescr := 'Cannot set nonlocal hook without a module handle.';
                          end;
    LongInt($00000595)  : begin
                            HrStr := 'ERROR_GLOBAL_ONLY_HOOK' ;
                            HrDescr := 'This hook procedure can only be set globally.';
                          end;
    LongInt($00000596)  : begin
                            HrStr := 'ERROR_JOURNAL_HOOK_SET' ;
                            HrDescr := 'The journal hook procedure is already installed.';
                          end;
    LongInt($00000597)  : begin
                            HrStr := 'ERROR_HOOK_NOT_INSTALLED' ;
                            HrDescr := 'The hook procedure is not installed.';
                          end;
    LongInt($00000598)  : begin
                            HrStr := 'ERROR_INVALID_LB_MESSAGE' ;
                            HrDescr := 'Invalid message for single-selection list box.';
                          end;
    LongInt($00000599)  : begin
                            HrStr := 'ERROR_SETCOUNT_ON_BAD_LB' ;
                            HrDescr := 'LB_SETCOUNT sent to non-lazy list box.';
                          end;
    LongInt($0000059A)  : begin
                            HrStr := 'ERROR_LB_WITHOUT_TABSTOPS' ;
                            HrDescr := 'This list box does not support tab stops.';
                          end;
    LongInt($0000059B)  : begin
                            HrStr := 'ERROR_DESTROY_OBJECT_OF_OTHER_THREAD' ;
                            HrDescr := 'Cannot destroy object created by another thread.';
                          end;
    LongInt($0000059C)  : begin
                            HrStr := 'ERROR_CHILD_WINDOW_MENU' ;
                            HrDescr := 'Child windows cannot have menus.';
                          end;
    LongInt($0000059D)  : begin
                            HrStr := 'ERROR_NO_SYSTEM_MENU' ;
                            HrDescr := 'The window does not have a system menu.';
                          end;
    LongInt($0000059E)  : begin
                            HrStr := 'ERROR_INVALID_MSGBOX_STYLE' ;
                            HrDescr := 'Invalid message box style.';
                          end;
    LongInt($0000059F)  : begin
                            HrStr := 'ERROR_INVALID_SPI_VALUE' ;
                            HrDescr := 'Invalid system-wide (SPI_*) parameter.';
                          end;
    LongInt($000005A0)  : begin
                            HrStr := 'ERROR_SCREEN_ALREADY_LOCKED' ;
                            HrDescr := 'Screen already locked.';
                          end;
    LongInt($000005A1)  : begin
                            HrStr := 'ERROR_HWNDS_HAVE_DIFF_PARENT' ;
                            HrDescr := 'All handles to windows in a multiple-window position structure must have the same parent.';
                          end;
    LongInt($000005A2)  : begin
                            HrStr := 'ERROR_NOT_CHILD_WINDOW' ;
                            HrDescr := 'The window is not a child window.';
                          end;
    LongInt($000005A3)  : begin
                            HrStr := 'ERROR_INVALID_GW_COMMAND' ;
                            HrDescr := 'Invalid GW_* command.';
                          end;
    LongInt($000005A4)  : begin
                            HrStr := 'ERROR_INVALID_THREAD_ID' ;
                            HrDescr := 'Invalid thread identifier.';
                          end;
    LongInt($000005A5)  : begin
                            HrStr := 'ERROR_NON_MDICHILD_WINDOW' ;
                            HrDescr := 'Cannot process a message from a window that is not a multiple document interface (MDI) window.';
                          end;
    LongInt($000005A6)  : begin
                            HrStr := 'ERROR_POPUP_ALREADY_ACTIVE' ;
                            HrDescr := 'Pop-up menu already active.';
                          end;
    LongInt($000005A7)  : begin
                            HrStr := 'ERROR_NO_SCROLLBARS' ;
                            HrDescr := 'The window does not have scroll bars.';
                          end;
    LongInt($000005A8)  : begin
                            HrStr := 'ERROR_INVALID_SCROLLBAR_RANGE' ;
                            HrDescr := 'Scroll bar range cannot be greater than MAXLONG.';
                          end;
    LongInt($000005A9)  : begin
                            HrStr := 'ERROR_INVALID_SHOWWIN_COMMAND' ;
                            HrDescr := 'Cannot show or remove the window in the way specified.';
                          end;
    LongInt($000005AA)  : begin
                            HrStr := 'ERROR_NO_SYSTEM_RESOURCES' ;
                            HrDescr := 'Insufficient system resources exist to complete the requested service.';
                          end;
    LongInt($000005AB)  : begin
                            HrStr := 'ERROR_NONPAGED_SYSTEM_RESOURCES' ;
                            HrDescr := 'Insufficient system resources exist to complete the requested service.';
                          end;
    LongInt($000005AC)  : begin
                            HrStr := 'ERROR_PAGED_SYSTEM_RESOURCES' ;
                            HrDescr := 'Insufficient system resources exist to complete the requested service.';
                          end;
    LongInt($000005AD)  : begin
                            HrStr := 'ERROR_WORKING_SET_QUOTA' ;
                            HrDescr := 'Insufficient quota to complete the requested service.';
                          end;
    LongInt($000005AE)  : begin
                            HrStr := 'ERROR_PAGEFILE_QUOTA' ;
                            HrDescr := 'Insufficient quota to complete the requested service.';
                          end;
    LongInt($000005AF)  : begin
                            HrStr := 'ERROR_COMMITMENT_LIMIT' ;
                            HrDescr := 'The paging file is too small for this operation to complete.';
                          end;
    LongInt($000005B0)  : begin
                            HrStr := 'ERROR_MENU_ITEM_NOT_FOUND' ;
                            HrDescr := 'A menu item was not found.';
                          end;
    LongInt($000005B1)  : begin
                            HrStr := 'ERROR_INVALID_KEYBOARD_HANDLE' ;
                            HrDescr := 'Invalid keyboard layout handle.';
                          end;
    LongInt($000005B2)  : begin
                            HrStr := 'ERROR_HOOK_TYPE_NOT_ALLOWED' ;
                            HrDescr := 'Hook type not allowed.';
                          end;
    LongInt($000005B3)  : begin
                            HrStr := 'ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION' ;
                            HrDescr := 'This operation requires an interactive window station.';
                          end;
    LongInt($000005B4)  : begin
                            HrStr := 'ERROR_TIMEOUT' ;
                            HrDescr := 'This operation returned because the time-out period expired.';
                          end;
    LongInt($000005B5)  : begin
                            HrStr := 'ERROR_INVALID_MONITOR_HANDLE' ;
                            HrDescr := 'Invalid monitor handle.';
                          end;
    LongInt($000005B6)  : begin
                            HrStr := 'ERROR_INCORRECT_SIZE' ;
                            HrDescr := 'Incorrect size argument.';
                          end;
    LongInt($000005B7)  : begin
                            HrStr := 'ERROR_SYMLINK_CLASS_DISABLED' ;
                            HrDescr := 'The symbolic link cannot be followed because its type is disabled.';
                          end;
    LongInt($000005B8)  : begin
                            HrStr := 'ERROR_SYMLINK_NOT_SUPPORTED' ;
                            HrDescr := 'This application does not support the current operation on symbolic links.';
                          end;
    LongInt($000005DC)  : begin
                            HrStr := 'ERROR_EVENTLOG_FILE_CORRUPT' ;
                            HrDescr := 'The event log file is corrupted.';
                          end;
    LongInt($000005DD)  : begin
                            HrStr := 'ERROR_EVENTLOG_CANT_START' ;
                            HrDescr := 'No event log file could be opened, so the event logging service did not start.';
                          end;
    LongInt($000005DE)  : begin
                            HrStr := 'ERROR_LOG_FILE_FULL' ;
                            HrDescr := 'The event log file is full.';
                          end;
    LongInt($000005DF)  : begin
                            HrStr := 'ERROR_EVENTLOG_FILE_CHANGED' ;
                            HrDescr := 'The event log file has changed between read operations.';
                          end;
    LongInt($0000060E)  : begin
                            HrStr := 'ERROR_INVALID_TASK_NAME' ;
                            HrDescr := 'The specified task name is invalid.';
                          end;
    LongInt($0000060F)  : begin
                            HrStr := 'ERROR_INVALID_TASK_INDEX' ;
                            HrDescr := 'The specified task index is invalid.';
                          end;
    LongInt($00000610)  : begin
                            HrStr := 'ERROR_THREAD_ALREADY_IN_TASK' ;
                            HrDescr := 'The specified thread is already joining a task.';
                          end;
    LongInt($00000641)  : begin
                            HrStr := 'ERROR_INSTALL_SERVICE_FAILURE' ;
                            HrDescr := 'The Windows Installer service could not be accessed.' +
                                       'This can occur if the Windows Installer is not correctly installed.' +
                                       'Contact your support personnel for assistance.';
                          end;
    LongInt($00000642)  : begin
                            HrStr := 'ERROR_INSTALL_USEREXIT' ;
                            HrDescr := 'User canceled installation.';
                          end;
    LongInt($00000643)  : begin
                            HrStr := 'ERROR_INSTALL_FAILURE' ;
                            HrDescr := 'Fatal error during installation.';
                          end;
    LongInt($00000644)  : begin
                            HrStr := 'ERROR_INSTALL_SUSPEND' ;
                            HrDescr := 'Installation suspended, incomplete.';
                          end;
    LongInt($00000645)  : begin
                            HrStr := 'ERROR_UNKNOWN_PRODUCT' ;
                            HrDescr := 'This action is valid only for products that are currently installed.';
                          end;
    LongInt($00000646)  : begin
                            HrStr := 'ERROR_UNKNOWN_FEATURE' ;
                            HrDescr := 'Feature ID not registered.';
                          end;
    LongInt($00000647)  : begin
                            HrStr := 'ERROR_UNKNOWN_COMPONENT' ;
                            HrDescr := 'Component ID not registered.';
                          end;
    LongInt($00000648)  : begin
                            HrStr := 'ERROR_UNKNOWN_PROPERTY' ;
                            HrDescr := 'Unknown property.';
                          end;
    LongInt($00000649)  : begin
                            HrStr := 'ERROR_INVALID_HANDLE_STATE' ;
                            HrDescr := 'Handle is in an invalid state.';
                          end;
    LongInt($0000064A)  : begin
                            HrStr := 'ERROR_BAD_CONFIGURATION' ;
                            HrDescr := 'The configuration data for this product is corrupt.' +
                                       'Contact your support personnel.';
                          end;
    LongInt($0000064B)  : begin
                            HrStr := 'ERROR_INDEX_ABSENT' ;
                            HrDescr := 'Component qualifier not present.';
                          end;
    LongInt($0000064C)  : begin
                            HrStr := 'ERROR_INSTALL_SOURCE_ABSENT' ;
                            HrDescr := 'The installation source for this product is not available.' +
                                       'Verify that the source exists and that you can access it.';
                          end;
    LongInt($0000064D)  : begin
                            HrStr := 'ERROR_INSTALL_PACKAGE_VERSION' ;
                            HrDescr := 'This installation package cannot be installed by the Windows Installer service.' +
                                       'You must install a Windows service pack that contains a newer version of the Windows Installer service.';
                          end;
    LongInt($0000064E)  : begin
                            HrStr := 'ERROR_PRODUCT_UNINSTALLED' ;
                            HrDescr := 'Product is uninstalled.';
                          end;
    LongInt($0000064F)  : begin
                            HrStr := 'ERROR_BAD_QUERY_SYNTAX' ;
                            HrDescr := 'SQL query syntax invalid or unsupported.';
                          end;
    LongInt($00000650)  : begin
                            HrStr := 'ERROR_INVALID_FIELD' ;
                            HrDescr := 'Record field does not exist.';
                          end;
    LongInt($00000651)  : begin
                            HrStr := 'ERROR_DEVICE_REMOVED' ;
                            HrDescr := 'The device has been removed.';
                          end;
    LongInt($00000652)  : begin
                            HrStr := 'ERROR_INSTALL_ALREADY_RUNNING' ;
                            HrDescr := 'Another installation is already in progress.' +
                                       'Complete that installation before proceeding with this install.';
                          end;
    LongInt($00000653)  : begin
                            HrStr := 'ERROR_INSTALL_PACKAGE_OPEN_FAILED' ;
                            HrDescr := 'This installation package could not be opened.' +
                                       'Verify that the package exists and that you can access it, or contact the application vendor to verify that this is a valid Windows Installer package.';
                          end;
    LongInt($00000654)  : begin
                            HrStr := 'ERROR_INSTALL_PACKAGE_INVALID' ;
                            HrDescr := 'This installation package could not be opened.' +
                                       'Contact the application vendor to verify that this is a valid Windows Installer package.';
                          end;
    LongInt($00000655)  : begin
                            HrStr := 'ERROR_INSTALL_UI_FAILURE' ;
                            HrDescr := 'There was an error starting the Windows Installer service user interface.' +
                                       'Contact your support personnel.';
                          end;
    LongInt($00000656)  : begin
                            HrStr := 'ERROR_INSTALL_LOG_FAILURE' ;
                            HrDescr := 'Error opening installation log file.' +
                                       'Verify that the specified log file location exists and that you can write to it.';
                          end;
    LongInt($00000657)  : begin
                            HrStr := 'ERROR_INSTALL_LANGUAGE_UNSUPPORTED' ;
                            HrDescr := 'The language of this installation package is not supported by your system.';
                          end;
    LongInt($00000658)  : begin
                            HrStr := 'ERROR_INSTALL_TRANSFORM_FAILURE' ;
                            HrDescr := 'Error applying transforms.' +
                                       'Verify that the specified transform paths are valid.';
                          end;
    LongInt($00000659)  : begin
                            HrStr := 'ERROR_INSTALL_PACKAGE_REJECTED' ;
                            HrDescr := 'This installation is forbidden by system policy.' +
                                       'Contact your system administrator.';
                          end;
    LongInt($0000065A)  : begin
                            HrStr := 'ERROR_FUNCTION_NOT_CALLED' ;
                            HrDescr := 'Function could not be executed.';
                          end;
    LongInt($0000065B)  : begin
                            HrStr := 'ERROR_FUNCTION_FAILED' ;
                            HrDescr := 'Function failed during execution.';
                          end;
    LongInt($0000065C)  : begin
                            HrStr := 'ERROR_INVALID_TABLE' ;
                            HrDescr := 'Invalid or unknown table specified.';
                          end;
    LongInt($0000065D)  : begin
                            HrStr := 'ERROR_DATATYPE_MISMATCH' ;
                            HrDescr := 'Data supplied is of wrong type.';
                          end;
    LongInt($0000065E)  : begin
                            HrStr := 'ERROR_UNSUPPORTED_TYPE' ;
                            HrDescr := 'Data of this type is not supported.';
                          end;
    LongInt($0000065F)  : begin
                            HrStr := 'ERROR_CREATE_FAILED' ;
                            HrDescr := 'The Windows Installer service failed to start.' +
                                       'Contact your support personnel.';
                          end;
    LongInt($00000660)  : begin
                            HrStr := 'ERROR_INSTALL_TEMP_UNWRITABLE' ;
                            HrDescr := 'The Temp folder is on a drive that is full or is inaccessible.' +
                                       'Free up space on the drive or verify that you have write permission on the Temp folder.';
                          end;
    LongInt($00000661)  : begin
                            HrStr := 'ERROR_INSTALL_PLATFORM_UNSUPPORTED' ;
                            HrDescr := 'This installation package is not supported by this processor type.' +
                                       'Contact your product vendor.';
                          end;
    LongInt($00000662)  : begin
                            HrStr := 'ERROR_INSTALL_NOTUSED' ;
                            HrDescr := 'Component not used on this computer.';
                          end;
    LongInt($00000663)  : begin
                            HrStr := 'ERROR_PATCH_PACKAGE_OPEN_FAILED' ;
                            HrDescr := 'This update package could not be opened.' +
                                       'Verify that the update package exists and that you can access it, or contact the application vendor to verify that this is a valid Windows Installer update package.';
                          end;
    LongInt($00000664)  : begin
                            HrStr := 'ERROR_PATCH_PACKAGE_INVALID' ;
                            HrDescr := 'This update package could not be opened.' +
                                       'Contact the application vendor to verify that this is a valid Windows Installer update package.';
                          end;
    LongInt($00000665)  : begin
                            HrStr := 'ERROR_PATCH_PACKAGE_UNSUPPORTED' ;
                            HrDescr := 'This update package cannot be processed by the Windows Installer service.' +
                                       'You must install a Windows service pack that contains a newer version of the Windows Installer service.';
                          end;
    LongInt($00000666)  : begin
                            HrStr := 'ERROR_PRODUCT_VERSION' ;
                            HrDescr := 'Another version of this product is already installed.' +
                                       'Installation of this version cannot continue.' +
                                       'To configure or remove the existing version of this product, use Add/Remove Programs in Control Panel.';
                          end;
    LongInt($00000667)  : begin
                            HrStr := 'ERROR_INVALID_COMMAND_LINE' ;
                            HrDescr := 'Invalid command-line argument.' +
                                       'Consult the Windows Installer SDK for detailed command line help.';
                          end;
    LongInt($00000668)  : begin
                            HrStr := 'ERROR_INSTALL_REMOTE_DISALLOWED' ;
                            HrDescr := 'Only administrators have permission to add, remove, or configure server software during a Terminal Services remote session.' +
                                       'If you want to install or configure software on the server, contact your network administrator.';
                          end;
    LongInt($00000669)  : begin
                            HrStr := 'ERROR_SUCCESS_REBOOT_INITIATED' ;
                            HrDescr := 'The requested operation completed successfully.' +
                                       'The system will be restarted so the changes can take effect.';
                          end;
    LongInt($0000066A)  : begin
                            HrStr := 'ERROR_PATCH_TARGET_NOT_FOUND' ;
                            HrDescr := 'The upgrade cannot be installed by the Windows Installer service because the program to be upgraded might be missing, or the upgrade might update a different version of the program.' +
                                       'Verify that the program to be upgraded exists on your computer and that you have the correct upgrade.';
                          end;
    LongInt($0000066B)  : begin
                            HrStr := 'ERROR_PATCH_PACKAGE_REJECTED' ;
                            HrDescr := 'The update package is not permitted by a software restriction policy.';
                          end;
    LongInt($0000066C)  : begin
                            HrStr := 'ERROR_INSTALL_TRANSFORM_REJECTED' ;
                            HrDescr := 'One or more customizations are not permitted by a software restriction policy.';
                          end;
    LongInt($0000066D)  : begin
                            HrStr := 'ERROR_INSTALL_REMOTE_PROHIBITED' ;
                            HrDescr := 'The Windows Installer does not permit installation from a Remote Desktop Connection.';
                          end;
    LongInt($0000066E)  : begin
                            HrStr := 'ERROR_PATCH_REMOVAL_UNSUPPORTED' ;
                            HrDescr := 'Uninstallation of the update package is not supported.';
                          end;
    LongInt($0000066F)  : begin
                            HrStr := 'ERROR_UNKNOWN_PATCH' ;
                            HrDescr := 'The update is not applied to this product.';
                          end;
    LongInt($00000670)  : begin
                            HrStr := 'ERROR_PATCH_NO_SEQUENCE' ;
                            HrDescr := 'No valid sequence could be found for the set of updates.';
                          end;
    LongInt($00000671)  : begin
                            HrStr := 'ERROR_PATCH_REMOVAL_DISALLOWED' ;
                            HrDescr := 'Update removal was disallowed by policy.';
                          end;
    LongInt($00000672)  : begin
                            HrStr := 'ERROR_INVALID_PATCH_XML' ;
                            HrDescr := 'The XML update data is invalid.';
                          end;
    LongInt($00000673)  : begin
                            HrStr := 'ERROR_PATCH_MANAGED_ADVERTISED_PRODUCT' ;
                            HrDescr := 'Windows Installer does not permit updating of managed advertised products.' +
                                       'At least one feature of the product must be installed before applying the update.';
                          end;
    LongInt($00000674)  : begin
                            HrStr := 'ERROR_INSTALL_SERVICE_SAFEBOOT' ;
                            HrDescr := 'The Windows Installer service is not accessible in Safe Mode.' +
                                       'Try again when your computer is not in Safe Mode or you can use System Restore to return your machine to a previous good state.';
                          end;
    LongInt($000006A4)  : begin
                            HrStr := 'RPC_S_INVALID_STRING_BINDING' ;
                            HrDescr := 'The string binding is invalid.';
                          end;
    LongInt($000006A5)  : begin
                            HrStr := 'RPC_S_WRONG_KIND_OF_BINDING' ;
                            HrDescr := 'The binding handle is not the correct type.';
                          end;
    LongInt($000006A6)  : begin
                            HrStr := 'RPC_S_INVALID_BINDING' ;
                            HrDescr := 'The binding handle is invalid.';
                          end;
    LongInt($000006A7)  : begin
                            HrStr := 'RPC_S_PROTSEQ_NOT_SUPPORTED' ;
                            HrDescr := 'The RPC protocol sequence is not supported.';
                          end;
    LongInt($000006A8)  : begin
                            HrStr := 'RPC_S_INVALID_RPC_PROTSEQ' ;
                            HrDescr := 'The RPC protocol sequence is invalid.';
                          end;
    LongInt($000006A9)  : begin
                            HrStr := 'RPC_S_INVALID_STRING_UUID' ;
                            HrDescr := 'The string UUID is invalid.';
                          end;
    LongInt($000006AA)  : begin
                            HrStr := 'RPC_S_INVALID_ENDPOINT_FORMAT' ;
                            HrDescr := 'The endpoint format is invalid.';
                          end;
    LongInt($000006AB)  : begin
                            HrStr := 'RPC_S_INVALID_NET_ADDR' ;
                            HrDescr := 'The network address is invalid.';
                          end;
    LongInt($000006AC)  : begin
                            HrStr := 'RPC_S_NO_ENDPOINT_FOUND' ;
                            HrDescr := 'No endpoint was found.';
                          end;
    LongInt($000006AD)  : begin
                            HrStr := 'RPC_S_INVALID_TIMEOUT' ;
                            HrDescr := 'The time-out value is invalid.';
                          end;
    LongInt($000006AE)  : begin
                            HrStr := 'RPC_S_OBJECT_NOT_FOUND' ;
                            HrDescr := 'The object UUID) was not found.';
                          end;
    LongInt($000006AF)  : begin
                            HrStr := 'RPC_S_ALREADY_REGISTERED' ;
                            HrDescr := 'The object UUID) has already been registered.';
                          end;
    LongInt($000006B0)  : begin
                            HrStr := 'RPC_S_TYPE_ALREADY_REGISTERED' ;
                            HrDescr := 'The type UUID has already been registered.';
                          end;
    LongInt($000006B1)  : begin
                            HrStr := 'RPC_S_ALREADY_LISTENING' ;
                            HrDescr := 'The RPC server is already listening.';
                          end;
    LongInt($000006B2)  : begin
                            HrStr := 'RPC_S_NO_PROTSEQS_REGISTERED' ;
                            HrDescr := 'No protocol sequences have been registered.';
                          end;
    LongInt($000006B3)  : begin
                            HrStr := 'RPC_S_NOT_LISTENING' ;
                            HrDescr := 'The RPC server is not listening.';
                          end;
    LongInt($000006B4)  : begin
                            HrStr := 'RPC_S_UNKNOWN_MGR_TYPE' ;
                            HrDescr := 'The manager type is unknown.';
                          end;
    LongInt($000006B5)  : begin
                            HrStr := 'RPC_S_UNKNOWN_IF' ;
                            HrDescr := 'The interface is unknown.';
                          end;
    LongInt($000006B6)  : begin
                            HrStr := 'RPC_S_NO_BINDINGS' ;
                            HrDescr := 'There are no bindings.';
                          end;
    LongInt($000006B7)  : begin
                            HrStr := 'RPC_S_NO_PROTSEQS' ;
                            HrDescr := 'There are no protocol sequences.';
                          end;
    LongInt($000006B8)  : begin
                            HrStr := 'RPC_S_CANT_CREATE_ENDPOINT' ;
                            HrDescr := 'The endpoint cannot be created.';
                          end;
    LongInt($000006B9)  : begin
                            HrStr := 'RPC_S_OUT_OF_RESOURCES' ;
                            HrDescr := 'Not enough resources are available to complete this operation.';
                          end;
    LongInt($000006BA)  : begin
                            HrStr := 'RPC_S_SERVER_UNAVAILABLE' ;
                            HrDescr := 'The RPC server is unavailable.';
                          end;
    LongInt($000006BB)  : begin
                            HrStr := 'RPC_S_SERVER_TOO_BUSY' ;
                            HrDescr := 'The RPC server is too busy to complete this operation.';
                          end;
    LongInt($000006BC)  : begin
                            HrStr := 'RPC_S_INVALID_NETWORK_OPTIONS' ;
                            HrDescr := 'The network options are invalid.';
                          end;
    LongInt($000006BD)  : begin
                            HrStr := 'RPC_S_NO_CALL_ACTIVE' ;
                            HrDescr := 'There are no RPCs active on this thread.';
                          end;
    LongInt($000006BE)  : begin
                            HrStr := 'RPC_S_CALL_FAILED' ;
                            HrDescr := 'The RPC failed.';
                          end;
    LongInt($000006BF)  : begin
                            HrStr := 'RPC_S_CALL_FAILED_DNE' ;
                            HrDescr := 'The RPC failed and did not execute.';
                          end;
    LongInt($000006C0)  : begin
                            HrStr := 'RPC_S_PROTOCOL_ERROR' ;
                            HrDescr := 'An RPC protocol error occurred.';
                          end;
    LongInt($000006C1)  : begin
                            HrStr := 'RPC_S_PROXY_ACCESS_DENIED' ;
                            HrDescr := 'Access to the HTTP proxy is denied.';
                          end;
    LongInt($000006C2)  : begin
                            HrStr := 'RPC_S_UNSUPPORTED_TRANS_SYN' ;
                            HrDescr := 'The transfer syntax is not supported by the RPC server.';
                          end;
    LongInt($000006C4)  : begin
                            HrStr := 'RPC_S_UNSUPPORTED_TYPE' ;
                            HrDescr := 'The UUID type is not supported.';
                          end;
    LongInt($000006C5)  : begin
                            HrStr := 'RPC_S_INVALID_TAG' ;
                            HrDescr := 'The tag is invalid.';
                          end;
    LongInt($000006C6)  : begin
                            HrStr := 'RPC_S_INVALID_BOUND' ;
                            HrDescr := 'The array bounds are invalid.';
                          end;
    LongInt($000006C7)  : begin
                            HrStr := 'RPC_S_NO_ENTRY_NAME' ;
                            HrDescr := 'The binding does not contain an entry name.';
                          end;
    LongInt($000006C8)  : begin
                            HrStr := 'RPC_S_INVALID_NAME_SYNTAX' ;
                            HrDescr := 'The name syntax is invalid.';
                          end;
    LongInt($000006C9)  : begin
                            HrStr := 'RPC_S_UNSUPPORTED_NAME_SYNTAX' ;
                            HrDescr := 'The name syntax is not supported.';
                          end;
    LongInt($000006CB)  : begin
                            HrStr := 'RPC_S_UUID_NO_ADDRESS' ;
                            HrDescr := 'No network address is available to use to construct a UUID.';
                          end;
    LongInt($000006CC)  : begin
                            HrStr := 'RPC_S_DUPLICATE_ENDPOINT' ;
                            HrDescr := 'The endpoint is a duplicate.';
                          end;
    LongInt($000006CD)  : begin
                            HrStr := 'RPC_S_UNKNOWN_AUTHN_TYPE' ;
                            HrDescr := 'The authentication type is unknown.';
                          end;
    LongInt($000006CE)  : begin
                            HrStr := 'RPC_S_MAX_CALLS_TOO_SMALL' ;
                            HrDescr := 'The maximum number of calls is too small.';
                          end;
    LongInt($000006CF)  : begin
                            HrStr := 'RPC_S_STRING_TOO_LONG' ;
                            HrDescr := 'The string is too long.';
                          end;
    LongInt($000006D0)  : begin
                            HrStr := 'RPC_S_PROTSEQ_NOT_FOUND' ;
                            HrDescr := 'The RPC protocol sequence was not found.';
                          end;
    LongInt($000006D1)  : begin
                            HrStr := 'RPC_S_PROCNUM_OUT_OF_RANGE' ;
                            HrDescr := 'The procedure number is out of range.';
                          end;
    LongInt($000006D2)  : begin
                            HrStr := 'RPC_S_BINDING_HAS_NO_AUTH' ;
                            HrDescr := 'The binding does not contain any authentication information.';
                          end;
    LongInt($000006D3)  : begin
                            HrStr := 'RPC_S_UNKNOWN_AUTHN_SERVICE' ;
                            HrDescr := 'The authentication service is unknown.';
                          end;
    LongInt($000006D4)  : begin
                            HrStr := 'RPC_S_UNKNOWN_AUTHN_LEVEL' ;
                            HrDescr := 'The authentication level is unknown.';
                          end;
    LongInt($000006D5)  : begin
                            HrStr := 'RPC_S_INVALID_AUTH_IDENTITY' ;
                            HrDescr := 'The security context is invalid.';
                          end;
    LongInt($000006D6)  : begin
                            HrStr := 'RPC_S_UNKNOWN_AUTHZ_SERVICE' ;
                            HrDescr := 'The authorization service is unknown.';
                          end;
    LongInt($000006D7)  : begin
                            HrStr := 'EPT_S_INVALID_ENTRY' ;
                            HrDescr := 'The entry is invalid.';
                          end;
    LongInt($000006D8)  : begin
                            HrStr := 'EPT_S_CANT_PERFORM_OP' ;
                            HrDescr := 'The server endpoint cannot perform the operation.';
                          end;
    LongInt($000006D9)  : begin
                            HrStr := 'EPT_S_NOT_REGISTERED' ;
                            HrDescr := 'There are no more endpoints available from the endpoint mapper.';
                          end;
    LongInt($000006DA)  : begin
                            HrStr := 'RPC_S_NOTHING_TO_EXPORT' ;
                            HrDescr := 'No interfaces have been exported.';
                          end;
    LongInt($000006DB)  : begin
                            HrStr := 'RPC_S_INCOMPLETE_NAME' ;
                            HrDescr := 'The entry name is incomplete.';
                          end;
    LongInt($000006DC)  : begin
                            HrStr := 'RPC_S_INVALID_VERS_OPTION' ;
                            HrDescr := 'The version option is invalid.';
                          end;
    LongInt($000006DD)  : begin
                            HrStr := 'RPC_S_NO_MORE_MEMBERS' ;
                            HrDescr := 'There are no more members.';
                          end;
    LongInt($000006DE)  : begin
                            HrStr := 'RPC_S_NOT_ALL_OBJS_UNEXPORTED' ;
                            HrDescr := 'There is nothing to unexport.';
                          end;
    LongInt($000006DF)  : begin
                            HrStr := 'RPC_S_INTERFACE_NOT_FOUND' ;
                            HrDescr := 'The interface was not found.';
                          end;
    LongInt($000006E0)  : begin
                            HrStr := 'RPC_S_ENTRY_ALREADY_EXISTS' ;
                            HrDescr := 'The entry already exists.';
                          end;
    LongInt($000006E1)  : begin
                            HrStr := 'RPC_S_ENTRY_NOT_FOUND' ;
                            HrDescr := 'The entry is not found.';
                          end;
    LongInt($000006E2)  : begin
                            HrStr := 'RPC_S_NAME_SERVICE_UNAVAILABLE' ;
                            HrDescr := 'The name service is unavailable.';
                          end;
    LongInt($000006E3)  : begin
                            HrStr := 'RPC_S_INVALID_NAF_ID' ;
                            HrDescr := 'The network address family is invalid.';
                          end;
    LongInt($000006E4)  : begin
                            HrStr := 'RPC_S_CANNOT_SUPPORT' ;
                            HrDescr := 'The requested operation is not supported.';
                          end;
    LongInt($000006E5)  : begin
                            HrStr := 'RPC_S_NO_CONTEXT_AVAILABLE' ;
                            HrDescr := 'No security context is available to allow impersonation.';
                          end;
    LongInt($000006E6)  : begin
                            HrStr := 'RPC_S_INTERNAL_ERROR' ;
                            HrDescr := 'An internal error occurred in an RPC.';
                          end;
    LongInt($000006E7)  : begin
                            HrStr := 'RPC_S_ZERO_DIVIDE' ;
                            HrDescr := 'The RPC server attempted an integer division by zero.';
                          end;
    LongInt($000006E8)  : begin
                            HrStr := 'RPC_S_ADDRESS_ERROR' ;
                            HrDescr := 'An addressing error occurred in the RPC server.';
                          end;
    LongInt($000006E9)  : begin
                            HrStr := 'RPC_S_FP_DIV_ZERO' ;
                            HrDescr := 'A floating-point operation at the RPC server caused a division by zero.';
                          end;
    LongInt($000006EA)  : begin
                            HrStr := 'RPC_S_FP_UNDERFLOW' ;
                            HrDescr := 'A floating-point underflow occurred at the RPC server.';
                          end;
    LongInt($000006EB)  : begin
                            HrStr := 'RPC_S_FP_OVERFLOW' ;
                            HrDescr := 'A floating-point overflow occurred at the RPC server.';
                          end;
    LongInt($000006EC)  : begin
                            HrStr := 'RPC_X_NO_MORE_ENTRIES' ;
                            HrDescr := 'The list of RPC servers available for the binding of auto handles has been exhausted.';
                          end;
    LongInt($000006ED)  : begin
                            HrStr := 'RPC_X_SS_CHAR_TRANS_OPEN_FAIL' ;
                            HrDescr := 'Unable to open the character translation table file.';
                          end;
    LongInt($000006EE)  : begin
                            HrStr := 'RPC_X_SS_CHAR_TRANS_SHORT_FILE' ;
                            HrDescr := 'The file containing the character translation table has fewer than 512 bytes.';
                          end;
    LongInt($000006EF)  : begin
                            HrStr := 'RPC_X_SS_IN_NULL_CONTEXT' ;
                            HrDescr := 'A null context handle was passed from the client to the host during an RPC.';
                          end;
    LongInt($000006F1)  : begin
                            HrStr := 'RPC_X_SS_CONTEXT_DAMAGED' ;
                            HrDescr := 'The context handle changed during an RPC.';
                          end;
    LongInt($000006F2)  : begin
                            HrStr := 'RPC_X_SS_HANDLES_MISMATCH' ;
                            HrDescr := 'The binding handles passed to an RPC do not match.';
                          end;
    LongInt($000006F3)  : begin
                            HrStr := 'RPC_X_SS_CANNOT_GET_CALL_HANDLE' ;
                            HrDescr := 'The stub is unable to get the RPC handle.';
                          end;
    LongInt($000006F4)  : begin
                            HrStr := 'RPC_X_NULL_REF_POINTER' ;
                            HrDescr := 'A null reference pointer was passed to the stub.';
                          end;
    LongInt($000006F5)  : begin
                            HrStr := 'RPC_X_ENUM_VALUE_OUT_OF_RANGE' ;
                            HrDescr := 'The enumeration value is out of range.';
                          end;
    LongInt($000006F6)  : begin
                            HrStr := 'RPC_X_BYTE_COUNT_TOO_SMALL' ;
                            HrDescr := 'The byte count is too small.';
                          end;
    LongInt($000006F7)  : begin
                            HrStr := 'RPC_X_BAD_STUB_DATA' ;
                            HrDescr := 'The stub received bad data.';
                          end;
    LongInt($000006F8)  : begin
                            HrStr := 'ERROR_INVALID_USER_BUFFER' ;
                            HrDescr := 'The supplied user buffer is not valid for the requested operation.';
                          end;
    LongInt($000006F9)  : begin
                            HrStr := 'ERROR_UNRECOGNIZED_MEDIA' ;
                            HrDescr := 'The disk media is not recognized.' +
                                       'It might not be formatted.';
                          end;
    LongInt($000006FA)  : begin
                            HrStr := 'ERROR_NO_TRUST_LSA_SECRET' ;
                            HrDescr := 'The workstation does not have a trust secret.';
                          end;
    LongInt($000006FB)  : begin
                            HrStr := 'ERROR_NO_TRUST_SAM_ACCOUNT' ;
                            HrDescr := 'The security database on the server does not have a computer account for this workstation trust relationship.';
                          end;
    LongInt($000006FC)  : begin
                            HrStr := 'ERROR_TRUSTED_DOMAIN_FAILURE' ;
                            HrDescr := 'The trust relationship between the primary domain and the trusted domain failed.';
                          end;
    LongInt($000006FD)  : begin
                            HrStr := 'ERROR_TRUSTED_RELATIONSHIP_FAILURE' ;
                            HrDescr := 'The trust relationship between this workstation and the primary domain failed.';
                          end;
    LongInt($000006FE)  : begin
                            HrStr := 'ERROR_TRUST_FAILURE' ;
                            HrDescr := 'The network logon failed.';
                          end;
    LongInt($000006FF)  : begin
                            HrStr := 'RPC_S_CALL_IN_PROGRESS' ;
                            HrDescr := 'An RPC is already in progress for this thread.';
                          end;
    LongInt($00000700)  : begin
                            HrStr := 'ERROR_NETLOGON_NOT_STARTED' ;
                            HrDescr := 'An attempt was made to log on, but the network logon service was not started.';
                          end;
    LongInt($00000701)  : begin
                            HrStr := 'ERROR_ACCOUNT_EXPIRED' ;
                            HrDescr := 'The user''s account has expired.';
                          end;
    LongInt($00000702)  : begin
                            HrStr := 'ERROR_REDIRECTOR_HAS_OPEN_HANDLES' ;
                            HrDescr := 'The redirector is in use and cannot be unloaded.';
                          end;
    LongInt($00000703)  : begin
                            HrStr := 'ERROR_PRINTER_DRIVER_ALREADY_INSTALLED' ;
                            HrDescr := 'The specified printer driver is already installed.';
                          end;
    LongInt($00000704)  : begin
                            HrStr := 'ERROR_UNKNOWN_PORT' ;
                            HrDescr := 'The specified port is unknown.';
                          end;
    LongInt($00000705)  : begin
                            HrStr := 'ERROR_UNKNOWN_PRINTER_DRIVER' ;
                            HrDescr := 'The printer driver is unknown.';
                          end;
    LongInt($00000706)  : begin
                            HrStr := 'ERROR_UNKNOWN_PRINTPROCESSOR' ;
                            HrDescr := 'The print processor is unknown.';
                          end;
    LongInt($00000707)  : begin
                            HrStr := 'ERROR_INVALID_SEPARATOR_FILE' ;
                            HrDescr := 'The specified separator file is invalid.';
                          end;
    LongInt($00000708)  : begin
                            HrStr := 'ERROR_INVALID_PRIORITY' ;
                            HrDescr := 'The specified priority is invalid.';
                          end;
    LongInt($00000709)  : begin
                            HrStr := 'ERROR_INVALID_PRINTER_NAME' ;
                            HrDescr := 'The printer name is invalid.';
                          end;
    LongInt($0000070A)  : begin
                            HrStr := 'ERROR_PRINTER_ALREADY_EXISTS' ;
                            HrDescr := 'The printer already exists.';
                          end;
    LongInt($0000070B)  : begin
                            HrStr := 'ERROR_INVALID_PRINTER_COMMAND' ;
                            HrDescr := 'The printer command is invalid.';
                          end;
    LongInt($0000070C)  : begin
                            HrStr := 'ERROR_INVALID_DATATYPE' ;
                            HrDescr := 'The specified data type is invalid.';
                          end;
    LongInt($0000070D)  : begin
                            HrStr := 'ERROR_INVALID_ENVIRONMENT' ;
                            HrDescr := 'The environment specified is invalid.';
                          end;
    LongInt($0000070E)  : begin
                            HrStr := 'RPC_S_NO_MORE_BINDINGS' ;
                            HrDescr := 'There are no more bindings.';
                          end;
    LongInt($0000070F)  : begin
                            HrStr := 'ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT' ;
                            HrDescr := 'The account used is an interdomain trust account.' +
                                       'Use your global user account or local user account to access this server.';
                          end;
    LongInt($00000710)  : begin
                            HrStr := 'ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT' ;
                            HrDescr := 'The account used is a computer account.' +
                                       'Use your global user account or local user account to access this server.';
                          end;
    LongInt($00000711)  : begin
                            HrStr := 'ERROR_NOLOGON_SERVER_TRUST_ACCOUNT' ;
                            HrDescr := 'The account used is a server trust account.' +
                                       'Use your global user account or local user account to access this server.';
                          end;
    LongInt($00000712)  : begin
                            HrStr := 'ERROR_DOMAIN_TRUST_INCONSISTENT' ;
                            HrDescr := 'The name or SID of the domain specified is inconsistent with the trust information for that domain.';
                          end;
    LongInt($00000713)  : begin
                            HrStr := 'ERROR_SERVER_HAS_OPEN_HANDLES' ;
                            HrDescr := 'The server is in use and cannot be unloaded.';
                          end;
    LongInt($00000714)  : begin
                            HrStr := 'ERROR_RESOURCE_DATA_NOT_FOUND' ;
                            HrDescr := 'The specified image file did not contain a resource section.';
                          end;
    LongInt($00000715)  : begin
                            HrStr := 'ERROR_RESOURCE_TYPE_NOT_FOUND' ;
                            HrDescr := 'The specified resource type cannot be found in the image file.';
                          end;
    LongInt($00000716)  : begin
                            HrStr := 'ERROR_RESOURCE_NAME_NOT_FOUND' ;
                            HrDescr := 'The specified resource name cannot be found in the image file.';
                          end;
    LongInt($00000717)  : begin
                            HrStr := 'ERROR_RESOURCE_LANG_NOT_FOUND' ;
                            HrDescr := 'The specified resource language ID cannot be found in the image file.';
                          end;
    LongInt($00000718)  : begin
                            HrStr := 'ERROR_NOT_ENOUGH_QUOTA' ;
                            HrDescr := 'Not enough quota is available to process this command.';
                          end;
    LongInt($00000719)  : begin
                            HrStr := 'RPC_S_NO_INTERFACES' ;
                            HrDescr := 'No interfaces have been registered.';
                          end;
    LongInt($0000071A)  : begin
                            HrStr := 'RPC_S_CALL_CANCELLED' ;
                            HrDescr := 'The RPC was canceled.';
                          end;
    LongInt($0000071B)  : begin
                            HrStr := 'RPC_S_BINDING_INCOMPLETE' ;
                            HrDescr := 'The binding handle does not contain all the required information.';
                          end;
    LongInt($0000071C)  : begin
                            HrStr := 'RPC_S_COMM_FAILURE' ;
                            HrDescr := 'A communications failure occurred during an RPC.';
                          end;
    LongInt($0000071D)  : begin
                            HrStr := 'RPC_S_UNSUPPORTED_AUTHN_LEVEL' ;
                            HrDescr := 'The requested authentication level is not supported.';
                          end;
    LongInt($0000071E)  : begin
                            HrStr := 'RPC_S_NO_PRINC_NAME' ;
                            HrDescr := 'No principal name is registered.';
                          end;
    LongInt($0000071F)  : begin
                            HrStr := 'RPC_S_NOT_RPC_ERROR' ;
                            HrDescr := 'The error specified is not a valid Windows RPC error code.';
                          end;
    LongInt($00000720)  : begin
                            HrStr := 'RPC_S_UUID_LOCAL_ONLY' ;
                            HrDescr := 'A UUID that is valid only on this computer has been allocated.';
                          end;
    LongInt($00000721)  : begin
                            HrStr := 'RPC_S_SEC_PKG_ERROR' ;
                            HrDescr := 'A security package-specific error occurred.';
                          end;
    LongInt($00000722)  : begin
                            HrStr := 'RPC_S_NOT_CANCELLED' ;
                            HrDescr := 'The thread is not canceled.';
                          end;
    LongInt($00000723)  : begin
                            HrStr := 'RPC_X_INVALID_ES_ACTION' ;
                            HrDescr := 'Invalid operation on the encoding/decoding handle.';
                          end;
    LongInt($00000724)  : begin
                            HrStr := 'RPC_X_WRONG_ES_VERSION' ;
                            HrDescr := 'Incompatible version of the serializing package.';
                          end;
    LongInt($00000725)  : begin
                            HrStr := 'RPC_X_WRONG_STUB_VERSION' ;
                            HrDescr := 'Incompatible version of the RPC stub.';
                          end;
    LongInt($00000726)  : begin
                            HrStr := 'RPC_X_INVALID_PIPE_OBJECT' ;
                            HrDescr := 'The RPC pipe object is invalid or corrupted.';
                          end;
    LongInt($00000727)  : begin
                            HrStr := 'RPC_X_WRONG_PIPE_ORDER' ;
                            HrDescr := 'An invalid operation was attempted on an RPC pipe object.';
                          end;
    LongInt($00000728)  : begin
                            HrStr := 'RPC_X_WRONG_PIPE_VERSION' ;
                            HrDescr := 'Unsupported RPC pipe version.';
                          end;
    LongInt($0000076A)  : begin
                            HrStr := 'RPC_S_GROUP_MEMBER_NOT_FOUND' ;
                            HrDescr := 'The group member was not found.';
                          end;
    LongInt($0000076B)  : begin
                            HrStr := 'EPT_S_CANT_CREATE' ;
                            HrDescr := 'The endpoint mapper database entry could not be created.';
                          end;
    LongInt($0000076C)  : begin
                            HrStr := 'RPC_S_INVALID_OBJECT' ;
                            HrDescr := 'The object UUID is the nil UUID.';
                          end;
    LongInt($0000076D)  : begin
                            HrStr := 'ERROR_INVALID_TIME' ;
                            HrDescr := 'The specified time is invalid.';
                          end;
    LongInt($0000076E)  : begin
                            HrStr := 'ERROR_INVALID_FORM_NAME' ;
                            HrDescr := 'The specified form name is invalid.';
                          end;
    LongInt($0000076F)  : begin
                            HrStr := 'ERROR_INVALID_FORM_SIZE' ;
                            HrDescr := 'The specified form size is invalid.';
                          end;
    LongInt($00000770)  : begin
                            HrStr := 'ERROR_ALREADY_WAITING' ;
                            HrDescr := 'The specified printer handle is already being waited on.';
                          end;
    LongInt($00000771)  : begin
                            HrStr := 'ERROR_PRINTER_DELETED' ;
                            HrDescr := 'The specified printer has been deleted.';
                          end;
    LongInt($00000772)  : begin
                            HrStr := 'ERROR_INVALID_PRINTER_STATE' ;
                            HrDescr := 'The state of the printer is invalid.';
                          end;
    LongInt($00000773)  : begin
                            HrStr := 'ERROR_PASSWORD_MUST_CHANGE' ;
                            HrDescr := 'The user''s password must be changed before logging on the first time.';
                          end;
    LongInt($00000774)  : begin
                            HrStr := 'ERROR_DOMAIN_CONTROLLER_NOT_FOUND' ;
                            HrDescr := 'Could not find the domain controller for this domain.';
                          end;
    LongInt($00000775)  : begin
                            HrStr := 'ERROR_ACCOUNT_LOCKED_OUT' ;
                            HrDescr := 'The referenced account is currently locked out and cannot be logged on to.';
                          end;
    LongInt($00000776)  : begin
                            HrStr := 'OR_INVALID_OXID' ;
                            HrDescr := 'The object exporter specified was not found.';
                          end;
    LongInt($00000777)  : begin
                            HrStr := 'OR_INVALID_OID' ;
                            HrDescr := 'The object specified was not found.';
                          end;
    LongInt($00000778)  : begin
                            HrStr := 'OR_INVALID_SET' ;
                            HrDescr := 'The object set specified was not found.';
                          end;
    LongInt($00000779)  : begin
                            HrStr := 'RPC_S_SEND_INCOMPLETE' ;
                            HrDescr := 'Some data remains to be sent in the request buffer.';
                          end;
    LongInt($0000077A)  : begin
                            HrStr := 'RPC_S_INVALID_ASYNC_HANDLE' ;
                            HrDescr := 'Invalid asynchronous RPC handle.';
                          end;
    LongInt($0000077B)  : begin
                            HrStr := 'RPC_S_INVALID_ASYNC_CALL' ;
                            HrDescr := 'Invalid asynchronous RPC call handle for this operation.';
                          end;
    LongInt($0000077C)  : begin
                            HrStr := 'RPC_X_PIPE_CLOSED' ;
                            HrDescr := 'The RPC pipe object has already been closed.';
                          end;
    LongInt($0000077D)  : begin
                            HrStr := 'RPC_X_PIPE_DISCIPLINE_ERROR' ;
                            HrDescr := 'The RPC call completed before all pipes were processed.';
                          end;
    LongInt($0000077E)  : begin
                            HrStr := 'RPC_X_PIPE_EMPTY' ;
                            HrDescr := 'No more data is available from the RPC pipe.';
                          end;
    LongInt($0000077F)  : begin
                            HrStr := 'ERROR_NO_SITENAME' ;
                            HrDescr := 'No site name is available for this machine.';
                          end;
    LongInt($00000780)  : begin
                            HrStr := 'ERROR_CANT_ACCESS_FILE' ;
                            HrDescr := 'The file cannot be accessed by the system.';
                          end;
    LongInt($00000781)  : begin
                            HrStr := 'ERROR_CANT_RESOLVE_FILENAME' ;
                            HrDescr := 'The name of the file cannot be resolved by the system.';
                          end;
    LongInt($00000782)  : begin
                            HrStr := 'RPC_S_ENTRY_TYPE_MISMATCH' ;
                            HrDescr := 'The entry is not of the expected type.';
                          end;
    LongInt($00000783)  : begin
                            HrStr := 'RPC_S_NOT_ALL_OBJS_EXPORTED' ;
                            HrDescr := 'Not all object UUIDs could be exported to the specified entry.';
                          end;
    LongInt($00000784)  : begin
                            HrStr := 'RPC_S_INTERFACE_NOT_EXPORTED' ;
                            HrDescr := 'The interface could not be exported to the specified entry.';
                          end;
    LongInt($00000785)  : begin
                            HrStr := 'RPC_S_PROFILE_NOT_ADDED' ;
                            HrDescr := 'The specified profile entry could not be added.';
                          end;
    LongInt($00000786)  : begin
                            HrStr := 'RPC_S_PRF_ELT_NOT_ADDED' ;
                            HrDescr := 'The specified profile element could not be added.';
                          end;
    LongInt($00000787)  : begin
                            HrStr := 'RPC_S_PRF_ELT_NOT_REMOVED' ;
                            HrDescr := 'The specified profile element could not be removed.';
                          end;
    LongInt($00000788)  : begin
                            HrStr := 'RPC_S_GRP_ELT_NOT_ADDED' ;
                            HrDescr := 'The group element could not be added.';
                          end;
    LongInt($00000789)  : begin
                            HrStr := 'RPC_S_GRP_ELT_NOT_REMOVED' ;
                            HrDescr := 'The group element could not be removed.';
                          end;
    LongInt($0000078A)  : begin
                            HrStr := 'ERROR_KM_DRIVER_BLOCKED' ;
                            HrDescr := 'The printer driver is not compatible with a policy enabled on your computer that blocks Windows NT 4.0 operating system drivers.';
                          end;
    LongInt($0000078B)  : begin
                            HrStr := 'ERROR_CONTEXT_EXPIRED' ;
                            HrDescr := 'The context has expired and can no longer be used.';
                          end;
    LongInt($0000078C)  : begin
                            HrStr := 'ERROR_PER_USER_TRUST_QUOTA_EXCEEDED' ;
                            HrDescr := 'The current user''s delegated trust creation quota has been exceeded.';
                          end;
    LongInt($0000078D)  : begin
                            HrStr := 'ERROR_ALL_USER_TRUST_QUOTA_EXCEEDED' ;
                            HrDescr := 'The total delegated trust creation quota has been exceeded.';
                          end;
    LongInt($0000078E)  : begin
                            HrStr := 'ERROR_USER_DELETE_TRUST_QUOTA_EXCEEDED' ;
                            HrDescr := 'The current user''s delegated trust deletion quota has been exceeded.';
                          end;
    LongInt($0000078F)  : begin
                            HrStr := 'ERROR_AUTHENTICATION_FIREWALL_FAILED' ;
                            HrDescr := 'Logon failure: The machine you are logging on to is protected by an authentication firewall.' +
                                       'The specified account is not allowed to authenticate to the machine.';
                          end;
    LongInt($00000790)  : begin
                            HrStr := 'ERROR_REMOTE_PRINT_CONNECTIONS_BLOCKED' ;
                            HrDescr := 'Remote connections to the Print Spooler are blocked by a policy set on your machine.';
                          end;
    LongInt($000007D0)  : begin
                            HrStr := 'ERROR_INVALID_PIXEL_FORMAT' ;
                            HrDescr := 'The pixel format is invalid.';
                          end;
    LongInt($000007D1)  : begin
                            HrStr := 'ERROR_BAD_DRIVER' ;
                            HrDescr := 'The specified driver is invalid.';
                          end;
    LongInt($000007D2)  : begin
                            HrStr := 'ERROR_INVALID_WINDOW_STYLE' ;
                            HrDescr := 'The window style or class attribute is invalid for this operation.';
                          end;
    LongInt($000007D3)  : begin
                            HrStr := 'ERROR_METAFILE_NOT_SUPPORTED' ;
                            HrDescr := 'The requested metafile operation is not supported.';
                          end;
    LongInt($000007D4)  : begin
                            HrStr := 'ERROR_TRANSFORM_NOT_SUPPORTED' ;
                            HrDescr := 'The requested transformation operation is not supported.';
                          end;
    LongInt($000007D5)  : begin
                            HrStr := 'ERROR_CLIPPING_NOT_SUPPORTED' ;
                            HrDescr := 'The requested clipping operation is not supported.';
                          end;
    LongInt($000007DA)  : begin
                            HrStr := 'ERROR_INVALID_CMM' ;
                            HrDescr := 'The specified color management module is invalid.';
                          end;
    LongInt($000007DB)  : begin
                            HrStr := 'ERROR_INVALID_PROFILE' ;
                            HrDescr := 'The specified color profile is invalid.';
                          end;
    LongInt($000007DC)  : begin
                            HrStr := 'ERROR_TAG_NOT_FOUND' ;
                            HrDescr := 'The specified tag was not found.';
                          end;
    LongInt($000007DD)  : begin
                            HrStr := 'ERROR_TAG_NOT_PRESENT' ;
                            HrDescr := 'A required tag is not present.';
                          end;
    LongInt($000007DE)  : begin
                            HrStr := 'ERROR_DUPLICATE_TAG' ;
                            HrDescr := 'The specified tag is already present.';
                          end;
    LongInt($000007DF)  : begin
                            HrStr := 'ERROR_PROFILE_NOT_ASSOCIATED_WITH_DEVICE' ;
                            HrDescr := 'The specified color profile is not associated with any device.';
                          end;
    LongInt($000007E0)  : begin
                            HrStr := 'ERROR_PROFILE_NOT_FOUND' ;
                            HrDescr := 'The specified color profile was not found.';
                          end;
    LongInt($000007E1)  : begin
                            HrStr := 'ERROR_INVALID_COLORSPACE' ;
                            HrDescr := 'The specified color space is invalid.';
                          end;
    LongInt($000007E2)  : begin
                            HrStr := 'ERROR_ICM_NOT_ENABLED' ;
                            HrDescr := 'Image Color Management is not enabled.';
                          end;
    LongInt($000007E3)  : begin
                            HrStr := 'ERROR_DELETING_ICM_XFORM' ;
                            HrDescr := 'There was an error while deleting the color transform.';
                          end;
    LongInt($000007E4)  : begin
                            HrStr := 'ERROR_INVALID_TRANSFORM' ;
                            HrDescr := 'The specified color transform is invalid.';
                          end;
    LongInt($000007E5)  : begin
                            HrStr := 'ERROR_COLORSPACE_MISMATCH' ;
                            HrDescr := 'The specified transform does not match the bitmap''s color space.';
                          end;
    LongInt($000007E6)  : begin
                            HrStr := 'ERROR_INVALID_COLORINDEX' ;
                            HrDescr := 'The specified named color index is not present in the profile.';
                          end;
    LongInt($000007E7)  : begin
                            HrStr := 'ERROR_PROFILE_DOES_NOT_MATCH_DEVICE' ;
                            HrDescr := 'The specified profile is intended for a device of a different type than the specified device.';
                          end;
    LongInt($00000836)  : begin
                            HrStr := 'NERR_NetNotStarted' ;
                            HrDescr := 'The workstation driver is not installed.';
                          end;
    LongInt($00000837)  : begin
                            HrStr := 'NERR_UnknownServer' ;
                            HrDescr := 'The server could not be located.';
                          end;
    LongInt($00000838)  : begin
                            HrStr := 'NERR_ShareMem' ;
                            HrDescr := 'An internal error occurred.' +
                                       'The network cannot access a shared memory segment.';
                          end;
    LongInt($00000839)  : begin
                            HrStr := 'NERR_NoNetworkResource' ;
                            HrDescr := 'A network resource shortage occurred.';
                          end;
    LongInt($0000083A)  : begin
                            HrStr := 'NERR_RemoteOnly' ;
                            HrDescr := 'This operation is not supported on workstations.';
                          end;
    LongInt($0000083B)  : begin
                            HrStr := 'NERR_DevNotRedirected' ;
                            HrDescr := 'The device is not connected.';
                          end;
    LongInt($0000083C)  : begin
                            HrStr := 'ERROR_CONNECTED_OTHER_PASSWORD' ;
                            HrDescr := 'The network connection was made successfully, but the user had to be prompted for a password other than the one originally specified.';
                          end;
    LongInt($0000083D)  : begin
                            HrStr := 'ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT' ;
                            HrDescr := 'The network connection was made successfully using default credentials.';
                          end;
    LongInt($00000842)  : begin
                            HrStr := 'NERR_ServerNotStarted' ;
                            HrDescr := 'The Server service is not started.';
                          end;
    LongInt($00000843)  : begin
                            HrStr := 'NERR_ItemNotFound' ;
                            HrDescr := 'The queue is empty.';
                          end;
    LongInt($00000844)  : begin
                            HrStr := 'NERR_UnknownDevDir' ;
                            HrDescr := 'The device or directory does not exist.';
                          end;
    LongInt($00000845)  : begin
                            HrStr := 'NERR_RedirectedPath' ;
                            HrDescr := 'The operation is invalid on a redirected resource.';
                          end;
    LongInt($00000846)  : begin
                            HrStr := 'NERR_DuplicateShare' ;
                            HrDescr := 'The name has already been shared.';
                          end;
    LongInt($00000847)  : begin
                            HrStr := 'NERR_NoRoom' ;
                            HrDescr := 'The server is currently out of the requested resource.';
                          end;
    LongInt($00000849)  : begin
                            HrStr := 'NERR_TooManyItems' ;
                            HrDescr := 'Requested addition of items exceeds the maximum allowed.';
                          end;
    LongInt($0000084A)  : begin
                            HrStr := 'NERR_InvalidMaxUsers' ;
                            HrDescr := 'The Peer service supports only two simultaneous users.';
                          end;
    LongInt($0000084B)  : begin
                            HrStr := 'NERR_BufTooSmall' ;
                            HrDescr := 'The API return buffer is too small.';
                          end;
    LongInt($0000084F)  : begin
                            HrStr := 'NERR_RemoteErr' ;
                            HrDescr := 'A remote API error occurred.';
                          end;
    LongInt($00000853)  : begin
                            HrStr := 'NERR_LanmanIniError' ;
                            HrDescr := 'An error occurred when opening or reading the configuration file.';
                          end;
    LongInt($00000858)  : begin
                            HrStr := 'NERR_NetworkError' ;
                            HrDescr := 'A general network error occurred.';
                          end;
    LongInt($00000859)  : begin
                            HrStr := 'NERR_WkstaInconsistentState' ;
                            HrDescr := 'The Workstation service is in an inconsistent state.' +
                                       'Restart the computer before restarting the Workstation service.';
                          end;
    LongInt($0000085A)  : begin
                            HrStr := 'NERR_WkstaNotStarted' ;
                            HrDescr := 'The Workstation service has not been started.';
                          end;
    LongInt($0000085B)  : begin
                            HrStr := 'NERR_BrowserNotStarted' ;
                            HrDescr := 'The requested information is not available.';
                          end;
    LongInt($0000085C)  : begin
                            HrStr := 'NERR_InternalError' ;
                            HrDescr := 'An internal error occurred.';
                          end;
    LongInt($0000085D)  : begin
                            HrStr := 'NERR_BadTransactConfig' ;
                            HrDescr := 'The server is not configured for transactions.';
                          end;
    LongInt($0000085E)  : begin
                            HrStr := 'NERR_InvalidAPI' ;
                            HrDescr := 'The requested API is not supported on the remote server.';
                          end;
    LongInt($0000085F)  : begin
                            HrStr := 'NERR_BadEventName' ;
                            HrDescr := 'The event name is invalid.';
                          end;
    LongInt($00000860)  : begin
                            HrStr := 'NERR_DupNameReboot' ;
                            HrDescr := 'The computer name already exists on the network.' +
                                       'Change it and reboot the computer.';
                          end;
    LongInt($00000862)  : begin
                            HrStr := 'NERR_CfgCompNotFound' ;
                            HrDescr := 'The specified component could not be found in the configuration information.';
                          end;
    LongInt($00000863)  : begin
                            HrStr := 'NERR_CfgParamNotFound' ;
                            HrDescr := 'The specified parameter could not be found in the configuration information.';
                          end;
    LongInt($00000865)  : begin
                            HrStr := 'NERR_LineTooLong' ;
                            HrDescr := 'A line in the configuration file is too long.';
                          end;
    LongInt($00000866)  : begin
                            HrStr := 'NERR_QNotFound' ;
                            HrDescr := 'The printer does not exist.';
                          end;
    LongInt($00000867)  : begin
                            HrStr := 'NERR_JobNotFound' ;
                            HrDescr := 'The print job does not exist.';
                          end;
    LongInt($00000868)  : begin
                            HrStr := 'NERR_DestNotFound' ;
                            HrDescr := 'The printer destination cannot be found.';
                          end;
    LongInt($00000869)  : begin
                            HrStr := 'NERR_DestExists' ;
                            HrDescr := 'The printer destination already exists.';
                          end;
    LongInt($0000086A)  : begin
                            HrStr := 'NERR_QExists' ;
                            HrDescr := 'The print queue already exists.';
                          end;
    LongInt($0000086B)  : begin
                            HrStr := 'NERR_QNoRoom' ;
                            HrDescr := 'No more printers can be added.';
                          end;
    LongInt($0000086C)  : begin
                            HrStr := 'NERR_JobNoRoom' ;
                            HrDescr := 'No more print jobs can be added.';
                          end;
    LongInt($0000086D)  : begin
                            HrStr := 'NERR_DestNoRoom' ;
                            HrDescr := 'No more printer destinations can be added.';
                          end;
    LongInt($0000086E)  : begin
                            HrStr := 'NERR_DestIdle' ;
                            HrDescr := 'This printer destination is idle and cannot accept control operations.';
                          end;
    LongInt($0000086F)  : begin
                            HrStr := 'NERR_DestInvalidOp' ;
                            HrDescr := 'This printer destination request contains an invalid control function.';
                          end;
    LongInt($00000870)  : begin
                            HrStr := 'NERR_ProcNoRespond' ;
                            HrDescr := 'The print processor is not responding.';
                          end;
    LongInt($00000871)  : begin
                            HrStr := 'NERR_SpoolerNotLoaded' ;
                            HrDescr := 'The spooler is not running.';
                          end;
    LongInt($00000872)  : begin
                            HrStr := 'NERR_DestInvalidState' ;
                            HrDescr := 'This operation cannot be performed on the print destination in its current state.';
                          end;
    LongInt($00000873)  : begin
                            HrStr := 'NERR_QinvalidState' ;
                            HrDescr := 'This operation cannot be performed on the print queue in its current state.';
                          end;
    LongInt($00000874)  : begin
                            HrStr := 'NERR_JobInvalidState' ;
                            HrDescr := 'This operation cannot be performed on the print job in its current state.';
                          end;
    LongInt($00000875)  : begin
                            HrStr := 'NERR_SpoolNoMemory' ;
                            HrDescr := 'A spooler memory allocation failure occurred.';
                          end;
    LongInt($00000876)  : begin
                            HrStr := 'NERR_DriverNotFound' ;
                            HrDescr := 'The device driver does not exist.';
                          end;
    LongInt($00000877)  : begin
                            HrStr := 'NERR_DataTypeInvalid' ;
                            HrDescr := 'The data type is not supported by the print processor.';
                          end;
    LongInt($00000878)  : begin
                            HrStr := 'NERR_ProcNotFound' ;
                            HrDescr := 'The print processor is not installed.';
                          end;
    LongInt($00000884)  : begin
                            HrStr := 'NERR_ServiceTableLocked' ;
                            HrDescr := 'The service database is locked.';
                          end;
    LongInt($00000885)  : begin
                            HrStr := 'NERR_ServiceTableFull' ;
                            HrDescr := 'The service table is full.';
                          end;
    LongInt($00000886)  : begin
                            HrStr := 'NERR_ServiceInstalled' ;
                            HrDescr := 'The requested service has already been started.';
                          end;
    LongInt($00000887)  : begin
                            HrStr := 'NERR_ServiceEntryLocked' ;
                            HrDescr := 'The service does not respond to control actions.';
                          end;
    LongInt($00000888)  : begin
                            HrStr := 'NERR_ServiceNotInstalled' ;
                            HrDescr := 'The service has not been started.';
                          end;
    LongInt($00000889)  : begin
                            HrStr := 'NERR_BadServiceName' ;
                            HrDescr := 'The service name is invalid.';
                          end;
    LongInt($0000088A)  : begin
                            HrStr := 'NERR_ServiceCtlTimeout' ;
                            HrDescr := 'The service is not responding to the control function.';
                          end;
    LongInt($0000088B)  : begin
                            HrStr := 'NERR_ServiceCtlBusy' ;
                            HrDescr := 'The service control is busy.';
                          end;
    LongInt($0000088C)  : begin
                            HrStr := 'NERR_BadServiceProgName' ;
                            HrDescr := 'The configuration file contains an invalid service program name.';
                          end;
    LongInt($0000088D)  : begin
                            HrStr := 'NERR_ServiceNotCtrl' ;
                            HrDescr := 'The service could not be controlled in its present state.';
                          end;
    LongInt($0000088E)  : begin
                            HrStr := 'NERR_ServiceKillProc' ;
                            HrDescr := 'The service ended abnormally.';
                          end;
    LongInt($0000088F)  : begin
                            HrStr := 'NERR_ServiceCtlNotValid' ;
                            HrDescr := 'The requested pause or stop is not valid for this service.';
                          end;
    LongInt($00000890)  : begin
                            HrStr := 'NERR_NotInDispatchTbl' ;
                            HrDescr := 'The service control dispatcher could not find the service name in the dispatch table.';
                          end;
    LongInt($00000891)  : begin
                            HrStr := 'NERR_BadControlRecv' ;
                            HrDescr := 'The service control dispatcher pipe read failed.';
                          end;
    LongInt($00000892)  : begin
                            HrStr := 'NERR_ServiceNotStarting' ;
                            HrDescr := 'A thread for the new service could not be created.';
                          end;
    LongInt($00000898)  : begin
                            HrStr := 'NERR_AlreadyLoggedOn' ;
                            HrDescr := 'This workstation is already logged on to the LAN.';
                          end;
    LongInt($00000899)  : begin
                            HrStr := 'NERR_NotLoggedOn' ;
                            HrDescr := 'The workstation is not logged on to the LAN.';
                          end;
    LongInt($0000089A)  : begin
                            HrStr := 'NERR_BadUsername' ;
                            HrDescr := 'The user name or group name parameter is invalid.';
                          end;
    LongInt($0000089B)  : begin
                            HrStr := 'NERR_BadPassword' ;
                            HrDescr := 'The password parameter is invalid.';
                          end;
    LongInt($0000089C)  : begin
                            HrStr := 'NERR_UnableToAddName_W' ;
                            HrDescr := 'The logon processor did not add the message alias.';
                          end;
    LongInt($0000089D)  : begin
                            HrStr := 'NERR_UnableToAddName_F' ;
                            HrDescr := 'The logon processor did not add the message alias.';
                          end;
    LongInt($0000089E)  : begin
                            HrStr := 'NERR_UnableToDelName_W' ;
                            HrDescr := 'The logoff processor did not delete the message alias.';
                          end;
    LongInt($0000089F)  : begin
                            HrStr := 'NERR_UnableToDelName_F' ;
                            HrDescr := 'The logoff processor did not delete the message alias.';
                          end;
    LongInt($000008A1)  : begin
                            HrStr := 'NERR_LogonsPaused' ;
                            HrDescr := 'Network logons are paused.';
                          end;
    LongInt($000008A2)  : begin
                            HrStr := 'NERR_LogonServerConflict' ;
                            HrDescr := 'A centralized logon server conflict occurred.';
                          end;
    LongInt($000008A3)  : begin
                            HrStr := 'NERR_LogonNoUserPath' ;
                            HrDescr := 'The server is configured without a valid user path.';
                          end;
    LongInt($000008A4)  : begin
                            HrStr := 'NERR_LogonScriptError' ;
                            HrDescr := 'An error occurred while loading or running the logon script.';
                          end;
    LongInt($000008A6)  : begin
                            HrStr := 'NERR_StandaloneLogon' ;
                            HrDescr := 'The logon server was not specified.' +
                                       'The computer will be logged on as STANDALONE.';
                          end;
    LongInt($000008A7)  : begin
                            HrStr := 'NERR_LogonServerNotFound' ;
                            HrDescr := 'The logon server could not be found.';
                          end;
    LongInt($000008A8)  : begin
                            HrStr := 'NERR_LogonDomainExists' ;
                            HrDescr := 'There is already a logon domain for this computer.';
                          end;
    LongInt($000008A9)  : begin
                            HrStr := 'NERR_NonValidatedLogon' ;
                            HrDescr := 'The logon server could not validate the logon.';
                          end;
    LongInt($000008AB)  : begin
                            HrStr := 'NERR_ACFNotFound' ;
                            HrDescr := 'The security database could not be found.';
                          end;
    LongInt($000008AC)  : begin
                            HrStr := 'NERR_GroupNotFound' ;
                            HrDescr := 'The group name could not be found.';
                          end;
    LongInt($000008AD)  : begin
                            HrStr := 'NERR_UserNotFound' ;
                            HrDescr := 'The user name could not be found.';
                          end;
    LongInt($000008AE)  : begin
                            HrStr := 'NERR_ResourceNotFound' ;
                            HrDescr := 'The resource name could not be found.';
                          end;
    LongInt($000008AF)  : begin
                            HrStr := 'NERR_GroupExists' ;
                            HrDescr := 'The group already exists.';
                          end;
    LongInt($000008B0)  : begin
                            HrStr := 'NERR_UserExists' ;
                            HrDescr := 'The user account already exists.';
                          end;
    LongInt($000008B1)  : begin
                            HrStr := 'NERR_ResourceExists' ;
                            HrDescr := 'The resource permission list already exists.';
                          end;
    LongInt($000008B2)  : begin
                            HrStr := 'NERR_NotPrimary' ;
                            HrDescr := 'This operation is allowed only on the PDC of the domain.';
                          end;
    LongInt($000008B3)  : begin
                            HrStr := 'NERR_ACFNotLoaded' ;
                            HrDescr := 'The security database has not been started.';
                          end;
    LongInt($000008B4)  : begin
                            HrStr := 'NERR_ACFNoRoom' ;
                            HrDescr := 'There are too many names in the user accounts database.';
                          end;
    LongInt($000008B5)  : begin
                            HrStr := 'NERR_ACFFileIOFail' ;
                            HrDescr := 'A disk I/O failure occurred.';
                          end;
    LongInt($000008B6)  : begin
                            HrStr := 'NERR_ACFTooManyLists' ;
                            HrDescr := 'The limit of 64 entries per resource was exceeded.';
                          end;
    LongInt($000008B7)  : begin
                            HrStr := 'NERR_UserLogon' ;
                            HrDescr := 'Deleting a user with a session is not allowed.';
                          end;
    LongInt($000008B8)  : begin
                            HrStr := 'NERR_ACFNoParent' ;
                            HrDescr := 'The parent directory could not be located.';
                          end;
    LongInt($000008B9)  : begin
                            HrStr := 'NERR_CanNotGrowSegment' ;
                            HrDescr := 'Unable to add to the security database session cache segment.';
                          end;
    LongInt($000008BA)  : begin
                            HrStr := 'NERR_SpeGroupOp' ;
                            HrDescr := 'This operation is not allowed on this special group.';
                          end;
    LongInt($000008BB)  : begin
                            HrStr := 'NERR_NotInCache' ;
                            HrDescr := 'This user is not cached in the user accounts database session cache.';
                          end;
    LongInt($000008BC)  : begin
                            HrStr := 'NERR_UserInGroup' ;
                            HrDescr := 'The user already belongs to this group.';
                          end;
    LongInt($000008BD)  : begin
                            HrStr := 'NERR_UserNotInGroup' ;
                            HrDescr := 'The user does not belong to this group.';
                          end;
    LongInt($000008BE)  : begin
                            HrStr := 'NERR_AccountUndefined' ;
                            HrDescr := 'This user account is undefined.';
                          end;
    LongInt($000008BF)  : begin
                            HrStr := 'NERR_AccountExpired' ;
                            HrDescr := 'This user account has expired.';
                          end;
    LongInt($000008C0)  : begin
                            HrStr := 'NERR_InvalidWorkstation' ;
                            HrDescr := 'The user is not allowed to log on from this workstation.';
                          end;
    LongInt($000008C1)  : begin
                            HrStr := 'NERR_InvalidLogonHours' ;
                            HrDescr := 'The user is not allowed to log on at this time.';
                          end;
    LongInt($000008C2)  : begin
                            HrStr := 'NERR_PasswordExpired' ;
                            HrDescr := 'The password of this user has expired.';
                          end;
    LongInt($000008C3)  : begin
                            HrStr := 'NERR_PasswordCantChange' ;
                            HrDescr := 'The password of this user cannot change.';
                          end;
    LongInt($000008C4)  : begin
                            HrStr := 'NERR_PasswordHistConflict' ;
                            HrDescr := 'This password cannot be used now.';
                          end;
    LongInt($000008C5)  : begin
                            HrStr := 'NERR_PasswordTooShort' ;
                            HrDescr := 'The password does not meet the password policy requirements.' +
                                       'Check the minimum password length, password complexity, and password history requirements.';
                          end;
    LongInt($000008C6)  : begin
                            HrStr := 'NERR_PasswordTooRecent' ;
                            HrDescr := 'The password of this user is too recent to change.';
                          end;
    LongInt($000008C7)  : begin
                            HrStr := 'NERR_InvalidDatabase' ;
                            HrDescr := 'The security database is corrupted.';
                          end;
    LongInt($000008C8)  : begin
                            HrStr := 'NERR_DatabaseUpToDate' ;
                            HrDescr := 'No updates are necessary to this replicant network or local security database.';
                          end;
    LongInt($000008C9)  : begin
                            HrStr := 'NERR_SyncRequired' ;
                            HrDescr := 'This replicant database is outdated; synchronization is required.';
                          end;
    LongInt($000008CA)  : begin
                            HrStr := 'NERR_UseNotFound' ;
                            HrDescr := 'The network connection could not be found.';
                          end;
    LongInt($000008CB)  : begin
                            HrStr := 'NERR_BadAsgType' ;
                            HrDescr := 'This asg_type is invalid.';
                          end;
    LongInt($000008CC)  : begin
                            HrStr := 'NERR_DeviceIsShared' ;
                            HrDescr := 'This device is currently being shared.';
                          end;
    LongInt($000008DE)  : begin
                            HrStr := 'NERR_NoComputerName' ;
                            HrDescr := 'The computer name could not be added as a message alias.' +
                                       'The name might already exist on the network.';
                          end;
    LongInt($000008DF)  : begin
                            HrStr := 'NERR_MsgAlreadyStarted' ;
                            HrDescr := 'The Messenger service is already started.';
                          end;
    LongInt($000008E0)  : begin
                            HrStr := 'NERR_MsgInitFailed' ;
                            HrDescr := 'The Messenger service failed to start.';
                          end;
    LongInt($000008E1)  : begin
                            HrStr := 'NERR_NameNotFound' ;
                            HrDescr := 'The message alias could not be found on the network.';
                          end;
    LongInt($000008E2)  : begin
                            HrStr := 'NERR_AlreadyForwarded' ;
                            HrDescr := 'This message alias has already been forwarded.';
                          end;
    LongInt($000008E3)  : begin
                            HrStr := 'NERR_AddForwarded' ;
                            HrDescr := 'This message alias has been added but is still forwarded.';
                          end;
    LongInt($000008E4)  : begin
                            HrStr := 'NERR_AlreadyExists' ;
                            HrDescr := 'This message alias already exists locally.';
                          end;
    LongInt($000008E5)  : begin
                            HrStr := 'NERR_TooManyNames' ;
                            HrDescr := 'The maximum number of added message aliases has been exceeded.';
                          end;
    LongInt($000008E6)  : begin
                            HrStr := 'NERR_DelComputerName' ;
                            HrDescr := 'The computer name could not be deleted.';
                          end;
    LongInt($000008E7)  : begin
                            HrStr := 'NERR_LocalForward' ;
                            HrDescr := 'Messages cannot be forwarded back to the same workstation.';
                          end;
    LongInt($000008E8)  : begin
                            HrStr := 'NERR_GrpMsgProcessor' ;
                            HrDescr := 'An error occurred in the domain message processor.';
                          end;
    LongInt($000008E9)  : begin
                            HrStr := 'NERR_PausedRemote' ;
                            HrDescr := 'The message was sent, but the recipient has paused the Messenger service.';
                          end;
    LongInt($000008EA)  : begin
                            HrStr := 'NERR_BadReceive' ;
                            HrDescr := 'The message was sent but not received.';
                          end;
    LongInt($000008EB)  : begin
                            HrStr := 'NERR_NameInUse' ;
                            HrDescr := 'The message alias is currently in use.' +
                                       'Try again later.';
                          end;
    LongInt($000008EC)  : begin
                            HrStr := 'NERR_MsgNotStarted' ;
                            HrDescr := 'The Messenger service has not been started.';
                          end;
    LongInt($000008ED)  : begin
                            HrStr := 'NERR_NotLocalName' ;
                            HrDescr := 'The name is not on the local computer.';
                          end;
    LongInt($000008EE)  : begin
                            HrStr := 'NERR_NoForwardName' ;
                            HrDescr := 'The forwarded message alias could not be found on the network.';
                          end;
    LongInt($000008EF)  : begin
                            HrStr := 'NERR_RemoteFull' ;
                            HrDescr := 'The message alias table on the remote station is full.';
                          end;
    LongInt($000008F0)  : begin
                            HrStr := 'NERR_NameNotForwarded' ;
                            HrDescr := 'Messages for this alias are not currently being forwarded.';
                          end;
    LongInt($000008F1)  : begin
                            HrStr := 'NERR_TruncatedBroadcast' ;
                            HrDescr := 'The broadcast message was truncated.';
                          end;
    LongInt($000008F6)  : begin
                            HrStr := 'NERR_InvalidDevice' ;
                            HrDescr := 'This is an invalid device name.';
                          end;
    LongInt($000008F7)  : begin
                            HrStr := 'NERR_WriteFault' ;
                            HrDescr := 'A write fault occurred.';
                          end;
    LongInt($000008F9)  : begin
                            HrStr := 'NERR_DuplicateName' ;
                            HrDescr := 'A duplicate message alias exists on the network.';
                          end;
    LongInt($000008FA)  : begin
                            HrStr := 'NERR_DeleteLater' ;
                            HrDescr := 'This message alias will be deleted later.';
                          end;
    LongInt($000008FB)  : begin
                            HrStr := 'NERR_IncompleteDel' ;
                            HrDescr := 'The message alias was not successfully deleted from all networks.';
                          end;
    LongInt($000008FC)  : begin
                            HrStr := 'NERR_MultipleNets' ;
                            HrDescr := 'This operation is not supported on computers with multiple networks.';
                          end;
    LongInt($00000906)  : begin
                            HrStr := 'NERR_NetNameNotFound' ;
                            HrDescr := 'This shared resource does not exist.';
                          end;
    LongInt($00000907)  : begin
                            HrStr := 'NERR_DeviceNotShared' ;
                            HrDescr := 'This device is not shared.';
                          end;
    LongInt($00000908)  : begin
                            HrStr := 'NERR_ClientNameNotFound' ;
                            HrDescr := 'A session does not exist with that computer name.';
                          end;
    LongInt($0000090A)  : begin
                            HrStr := 'NERR_FileIdNotFound' ;
                            HrDescr := 'There is not an open file with that identification number.';
                          end;
    LongInt($0000090B)  : begin
                            HrStr := 'NERR_ExecFailure' ;
                            HrDescr := 'A failure occurred when executing a remote administration command.';
                          end;
    LongInt($0000090C)  : begin
                            HrStr := 'NERR_TmpFile' ;
                            HrDescr := 'A failure occurred when opening a remote temporary file.';
                          end;
    LongInt($0000090D)  : begin
                            HrStr := 'NERR_TooMuchData' ;
                            HrDescr := 'The data returned from a remote administration command has been truncated to 64 KB.';
                          end;
    LongInt($0000090E)  : begin
                            HrStr := 'NERR_DeviceShareConflict' ;
                            HrDescr := 'This device cannot be shared as both a spooled and a nonspooled resource.';
                          end;
    LongInt($0000090F)  : begin
                            HrStr := 'NERR_BrowserTableIncomplete' ;
                            HrDescr := 'The information in the list of servers might be incorrect.';
                          end;
    LongInt($00000910)  : begin
                            HrStr := 'NERR_NotLocalDomain' ;
                            HrDescr := 'The computer is not active in this domain.';
                          end;
    LongInt($00000911)  : begin
                            HrStr := 'NERR_IsDfsShare' ;
                            HrDescr := 'The share must be removed from the Distributed File System (DFS) before it can be deleted.';
                          end;
    LongInt($0000091B)  : begin
                            HrStr := 'NERR_DevInvalidOpCode' ;
                            HrDescr := 'The operation is invalid for this device.';
                          end;
    LongInt($0000091C)  : begin
                            HrStr := 'NERR_DevNotFound' ;
                            HrDescr := 'This device cannot be shared.';
                          end;
    LongInt($0000091D)  : begin
                            HrStr := 'NERR_DevNotOpen' ;
                            HrDescr := 'This device was not open.';
                          end;
    LongInt($0000091E)  : begin
                            HrStr := 'NERR_BadQueueDevString' ;
                            HrDescr := 'This device name list is invalid.';
                          end;
    LongInt($0000091F)  : begin
                            HrStr := 'NERR_BadQueuePriority' ;
                            HrDescr := 'The queue priority is invalid.';
                          end;
    LongInt($00000921)  : begin
                            HrStr := 'NERR_NoCommDevs' ;
                            HrDescr := 'There are no shared communication devices.';
                          end;
    LongInt($00000922)  : begin
                            HrStr := 'NERR_QueueNotFound' ;
                            HrDescr := 'The queue you specified does not exist.';
                          end;
    LongInt($00000924)  : begin
                            HrStr := 'NERR_BadDevString' ;
                            HrDescr := 'This list of devices is invalid.';
                          end;
    LongInt($00000925)  : begin
                            HrStr := 'NERR_BadDev' ;
                            HrDescr := 'The requested device is invalid.';
                          end;
    LongInt($00000926)  : begin
                            HrStr := 'NERR_InUseBySpooler' ;
                            HrDescr := 'This device is already in use by the spooler.';
                          end;
    LongInt($00000927)  : begin
                            HrStr := 'NERR_CommDevInUse' ;
                            HrDescr := 'This device is already in use as a communication device.';
                          end;
    LongInt($0000092F)  : begin
                            HrStr := 'NERR_InvalidComputer' ;
                            HrDescr := 'This computer name is invalid.';
                          end;
    LongInt($00000932)  : begin
                            HrStr := 'NERR_MaxLenExceeded' ;
                            HrDescr := 'The string and prefix specified are too long.';
                          end;
    LongInt($00000934)  : begin
                            HrStr := 'NERR_BadComponent' ;
                            HrDescr := 'This path component is invalid.';
                          end;
    LongInt($00000935)  : begin
                            HrStr := 'NERR_CantType' ;
                            HrDescr := 'Could not determine the type of input.';
                          end;
    LongInt($0000093A)  : begin
                            HrStr := 'NERR_TooManyEntries' ;
                            HrDescr := 'The buffer for types is not big enough.';
                          end;
    LongInt($00000942)  : begin
                            HrStr := 'NERR_ProfileFileTooBig' ;
                            HrDescr := 'Profile files cannot exceed 64 KB.';
                          end;
    LongInt($00000943)  : begin
                            HrStr := 'NERR_ProfileOffset' ;
                            HrDescr := 'The start offset is out of range.';
                          end;
    LongInt($00000944)  : begin
                            HrStr := 'NERR_ProfileCleanup' ;
                            HrDescr := 'The system cannot delete current connections to network resources.';
                          end;
    LongInt($00000945)  : begin
                            HrStr := 'NERR_ProfileUnknownCmd' ;
                            HrDescr := 'The system was unable to parse the command line in this file.';
                          end;
    LongInt($00000946)  : begin
                            HrStr := 'NERR_ProfileLoadErr' ;
                            HrDescr := 'An error occurred while loading the profile file.';
                          end;
    LongInt($00000947)  : begin
                            HrStr := 'NERR_ProfileSaveErr' ;
                            HrDescr := 'Errors occurred while saving the profile file.' +
                                       'The profile was partially saved.';
                          end;
    LongInt($00000949)  : begin
                            HrStr := 'NERR_LogOverflow' ;
                            HrDescr := 'Log file %1 is full.';
                          end;
    LongInt($0000094A)  : begin
                            HrStr := 'NERR_LogFileChanged' ;
                            HrDescr := 'This log file has changed between reads.';
                          end;
    LongInt($0000094B)  : begin
                            HrStr := 'NERR_LogFileCorrupt' ;
                            HrDescr := 'Log file %1 is corrupt.';
                          end;
    LongInt($0000094C)  : begin
                            HrStr := 'NERR_SourceIsDir' ;
                            HrDescr := 'The source path cannot be a directory.';
                          end;
    LongInt($0000094D)  : begin
                            HrStr := 'NERR_BadSource' ;
                            HrDescr := 'The source path is illegal.';
                          end;
    LongInt($0000094E)  : begin
                            HrStr := 'NERR_BadDest' ;
                            HrDescr := 'The destination path is illegal.';
                          end;
    LongInt($0000094F)  : begin
                            HrStr := 'NERR_DifferentServers' ;
                            HrDescr := 'The source and destination paths are on different servers.';
                          end;
    LongInt($00000951)  : begin
                            HrStr := 'NERR_RunSrvPaused' ;
                            HrDescr := 'The Run server you requested is paused.';
                          end;
    LongInt($00000955)  : begin
                            HrStr := 'NERR_ErrCommRunSrv' ;
                            HrDescr := 'An error occurred when communicating with a Run server.';
                          end;
    LongInt($00000957)  : begin
                            HrStr := 'NERR_ErrorExecingGhost' ;
                            HrDescr := 'An error occurred when starting a background process.';
                          end;
    LongInt($00000958)  : begin
                            HrStr := 'NERR_ShareNotFound' ;
                            HrDescr := 'The shared resource you are connected to could not be found.';
                          end;
    LongInt($00000960)  : begin
                            HrStr := 'NERR_InvalidLana' ;
                            HrDescr := 'The LAN adapter number is invalid.';
                          end;
    LongInt($00000961)  : begin
                            HrStr := 'NERR_OpenFiles' ;
                            HrDescr := 'There are open files on the connection.';
                          end;
    LongInt($00000962)  : begin
                            HrStr := 'NERR_ActiveConns' ;
                            HrDescr := 'Active connections still exist.';
                          end;
    LongInt($00000963)  : begin
                            HrStr := 'NERR_BadPasswordCore' ;
                            HrDescr := 'This share name or password is invalid.';
                          end;
    LongInt($00000964)  : begin
                            HrStr := 'NERR_DevInUse' ;
                            HrDescr := 'The device is being accessed by an active process.';
                          end;
    LongInt($00000965)  : begin
                            HrStr := 'NERR_LocalDrive' ;
                            HrDescr := 'The drive letter is in use locally.';
                          end;
    LongInt($0000097E)  : begin
                            HrStr := 'NERR_AlertExists' ;
                            HrDescr := 'The specified client is already registered for the specified event.';
                          end;
    LongInt($0000097F)  : begin
                            HrStr := 'NERR_TooManyAlerts' ;
                            HrDescr := 'The alert table is full.';
                          end;
    LongInt($00000980)  : begin
                            HrStr := 'NERR_NoSuchAlert' ;
                            HrDescr := 'An invalid or nonexistent alert name was raised.';
                          end;
    LongInt($00000981)  : begin
                            HrStr := 'NERR_BadRecipient' ;
                            HrDescr := 'The alert recipient is invalid.';
                          end;
    LongInt($00000982)  : begin
                            HrStr := 'NERR_AcctLimitExceeded' ;
                            HrDescr := 'A user''s session with this server has been deleted.';
                          end;
    LongInt($00000988)  : begin
                            HrStr := 'NERR_InvalidLogSeek' ;
                            HrDescr := 'The log file does not contain the requested record number.';
                          end;
    LongInt($00000992)  : begin
                            HrStr := 'NERR_BadUasConfig' ;
                            HrDescr := 'The user accounts database is not configured correctly.';
                          end;
    LongInt($00000993)  : begin
                            HrStr := 'NERR_InvalidUASOp' ;
                            HrDescr := 'This operation is not permitted when the Net Logon service is running.';
                          end;
    LongInt($00000994)  : begin
                            HrStr := 'NERR_LastAdmin' ;
                            HrDescr := 'This operation is not allowed on the last administrative account.';
                          end;
    LongInt($00000995)  : begin
                            HrStr := 'NERR_DCNotFound' ;
                            HrDescr := 'Could not find the domain controller for this domain.';
                          end;
    LongInt($00000996)  : begin
                            HrStr := 'NERR_LogonTrackingError' ;
                            HrDescr := 'Could not set logon information for this user.';
                          end;
    LongInt($00000997)  : begin
                            HrStr := 'NERR_NetlogonNotStarted' ;
                            HrDescr := 'The Net Logon service has not been started.';
                          end;
    LongInt($00000998)  : begin
                            HrStr := 'NERR_CanNotGrowUASFile' ;
                            HrDescr := 'Unable to add to the user accounts database.';
                          end;
    LongInt($00000999)  : begin
                            HrStr := 'NERR_TimeDiffAtDC' ;
                            HrDescr := 'This server''s clock is not synchronized with the PDC''s clock.';
                          end;
    LongInt($0000099A)  : begin
                            HrStr := 'NERR_PasswordMismatch' ;
                            HrDescr := 'A password mismatch has been detected.';
                          end;
    LongInt($0000099C)  : begin
                            HrStr := 'NERR_NoSuchServer' ;
                            HrDescr := 'The server identification does not specify a valid server.';
                          end;
    LongInt($0000099D)  : begin
                            HrStr := 'NERR_NoSuchSession' ;
                            HrDescr := 'The session identification does not specify a valid session.';
                          end;
    LongInt($0000099E)  : begin
                            HrStr := 'NERR_NoSuchConnection' ;
                            HrDescr := 'The connection identification does not specify a valid connection.';
                          end;
    LongInt($0000099F)  : begin
                            HrStr := 'NERR_TooManyServers' ;
                            HrDescr := 'There is no space for another entry in the table of available servers.';
                          end;
    LongInt($000009A0)  : begin
                            HrStr := 'NERR_TooManySessions' ;
                            HrDescr := 'The server has reached the maximum number of sessions it supports.';
                          end;
    LongInt($000009A1)  : begin
                            HrStr := 'NERR_TooManyConnections' ;
                            HrDescr := 'The server has reached the maximum number of connections it supports.';
                          end;
    LongInt($000009A2)  : begin
                            HrStr := 'NERR_TooManyFiles' ;
                            HrDescr := 'The server cannot open more files because it has reached its maximum number.';
                          end;
    LongInt($000009A3)  : begin
                            HrStr := 'NERR_NoAlternateServers' ;
                            HrDescr := 'There are no alternate servers registered on this server.';
                          end;
    LongInt($000009A6)  : begin
                            HrStr := 'NERR_TryDownLevel' ;
                            HrDescr := 'Try the down-level (remote admin protocol) version of API instead.';
                          end;
    LongInt($000009B0)  : begin
                            HrStr := 'NERR_UPSDriverNotStarted' ;
                            HrDescr := 'The uninterruptible power supply (UPS) driver could not be accessed by the UPS service.';
                          end;
    LongInt($000009B1)  : begin
                            HrStr := 'NERR_UPSInvalidConfig' ;
                            HrDescr := 'The UPS service is not configured correctly.';
                          end;
    LongInt($000009B2)  : begin
                            HrStr := 'NERR_UPSInvalidCommPort' ;
                            HrDescr := 'The UPS service could not access the specified Comm Port.';
                          end;
    LongInt($000009B3)  : begin
                            HrStr := 'NERR_UPSSignalAsserted' ;
                            HrDescr := 'The UPS indicated a line fail or low battery situation.' +
                                       'Service not started.';
                          end;
    LongInt($000009B4)  : begin
                            HrStr := 'NERR_UPSShutdownFailed' ;
                            HrDescr := 'The UPS service failed to perform a system shut down.';
                          end;
    LongInt($000009C4)  : begin
                            HrStr := 'NERR_BadDosRetCode' ;
                            HrDescr := 'The program below returned an MS-DOS error code.';
                          end;
    LongInt($000009C5)  : begin
                            HrStr := 'NERR_ProgNeedsExtraMem' ;
                            HrDescr := 'The program below needs more memory.';
                          end;
    LongInt($000009C6)  : begin
                            HrStr := 'NERR_BadDosFunction' ;
                            HrDescr := 'The program below called an unsupported MS-DOS function.';
                          end;
    LongInt($000009C7)  : begin
                            HrStr := 'NERR_RemoteBootFailed' ;
                            HrDescr := 'The workstation failed to boot.';
                          end;
    LongInt($000009C8)  : begin
                            HrStr := 'NERR_BadFileCheckSum' ;
                            HrDescr := 'The file below is corrupt.';
                          end;
    LongInt($000009C9)  : begin
                            HrStr := 'NERR_NoRplBootSystem' ;
                            HrDescr := 'No loader is specified in the boot-block definition file.';
                          end;
    LongInt($000009CA)  : begin
                            HrStr := 'NERR_RplLoadrNetBiosErr' ;
                            HrDescr := 'NetBIOS returned an error: The network control blocks (NCBs) and Server Message Block (SMB) are dumped above.';
                          end;
    LongInt($000009CB)  : begin
                            HrStr := 'NERR_RplLoadrDiskErr' ;
                            HrDescr := 'A disk I/O error occurred.';
                          end;
    LongInt($000009CC)  : begin
                            HrStr := 'NERR_ImageParamErr' ;
                            HrDescr := 'Image parameter substitution failed.';
                          end;
    LongInt($000009CD)  : begin
                            HrStr := 'NERR_TooManyImageParams' ;
                            HrDescr := 'Too many image parameters cross disk sector boundaries.';
                          end;
    LongInt($000009CE)  : begin
                            HrStr := 'NERR_NonDosFloppyUsed' ;
                            HrDescr := 'The image was not generated from an MS-DOS disk formatted with /S.';
                          end;
    LongInt($000009CF)  : begin
                            HrStr := 'NERR_RplBootRestart' ;
                            HrDescr := 'Remote boot will be restarted later.';
                          end;
    LongInt($000009D0)  : begin
                            HrStr := 'NERR_RplSrvrCallFailed' ;
                            HrDescr := 'The call to the Remoteboot server failed.';
                          end;
    LongInt($000009D1)  : begin
                            HrStr := 'NERR_CantConnectRplSrvr' ;
                            HrDescr := 'Cannot connect to the Remoteboot server.';
                          end;
    LongInt($000009D2)  : begin
                            HrStr := 'NERR_CantOpenImageFile' ;
                            HrDescr := 'Cannot open image file on the Remoteboot server.';
                          end;
    LongInt($000009D3)  : begin
                            HrStr := 'NERR_CallingRplSrvr' ;
                            HrDescr := 'Connecting to the Remoteboot server.';
                          end;
    LongInt($000009D4)  : begin
                            HrStr := 'NERR_StartingRplBoot' ;
                            HrDescr := 'Connecting to the Remoteboot server.';
                          end;
    LongInt($000009D5)  : begin
                            HrStr := 'NERR_RplBootServiceTerm' ;
                            HrDescr := 'Remote boot service was stopped, check the error log for the cause of the problem.';
                          end;
    LongInt($000009D6)  : begin
                            HrStr := 'NERR_RplBootStartFailed' ;
                            HrDescr := 'Remote boot startup failed; check the error log for the cause of the problem.';
                          end;
    LongInt($000009D7)  : begin
                            HrStr := 'NERR_RPL_CONNECTED' ;
                            HrDescr := 'A second connection to a Remoteboot resource is not allowed.';
                          end;
    LongInt($000009F6)  : begin
                            HrStr := 'NERR_BrowserConfiguredToNotRun' ;
                            HrDescr := 'The browser service was configured with MaintainServerList=No.';
                          end;
    LongInt($00000A32)  : begin
                            HrStr := 'NERR_RplNoAdaptersStarted' ;
                            HrDescr := 'Service failed to start because none of the network adapters started with this service.';
                          end;
    LongInt($00000A33)  : begin
                            HrStr := 'NERR_RplBadRegistry' ;
                            HrDescr := 'Service failed to start due to bad startup information in the registry.';
                          end;
    LongInt($00000A34)  : begin
                            HrStr := 'NERR_RplBadDatabase' ;
                            HrDescr := 'Service failed to start because its database is absent or corrupt.';
                          end;
    LongInt($00000A35)  : begin
                            HrStr := 'NERR_RplRplfilesShare' ;
                            HrDescr := 'Service failed to start because the RPLFILES share is absent.';
                          end;
    LongInt($00000A36)  : begin
                            HrStr := 'NERR_RplNotRplServer' ;
                            HrDescr := 'Service failed to start because the RPLUSER group is absent.';
                          end;
    LongInt($00000A37)  : begin
                            HrStr := 'NERR_RplCannotEnum' ;
                            HrDescr := 'Cannot enumerate service records.';
                          end;
    LongInt($00000A38)  : begin
                            HrStr := 'NERR_RplWkstaInfoCorrupted' ;
                            HrDescr := 'Workstation record information has been corrupted.';
                          end;
    LongInt($00000A39)  : begin
                            HrStr := 'NERR_RplWkstaNotFound' ;
                            HrDescr := 'Workstation record was not found.';
                          end;
    LongInt($00000A3A)  : begin
                            HrStr := 'NERR_RplWkstaNameUnavailable' ;
                            HrDescr := 'Workstation name is in use by some other workstation.';
                          end;
    LongInt($00000A3B)  : begin
                            HrStr := 'NERR_RplProfileInfoCorrupted' ;
                            HrDescr := 'Profile record information has been corrupted.';
                          end;
    LongInt($00000A3C)  : begin
                            HrStr := 'NERR_RplProfileNotFound' ;
                            HrDescr := 'Profile record was not found.';
                          end;
    LongInt($00000A3D)  : begin
                            HrStr := 'NERR_RplProfileNameUnavailable' ;
                            HrDescr := 'Profile name is in use by some other profile.';
                          end;
    LongInt($00000A3E)  : begin
                            HrStr := 'NERR_RplProfileNotEmpty' ;
                            HrDescr := 'There are workstations using this profile.';
                          end;
    LongInt($00000A3F)  : begin
                            HrStr := 'NERR_RplConfigInfoCorrupted' ;
                            HrDescr := 'Configuration record information has been corrupted.';
                          end;
    LongInt($00000A40)  : begin
                            HrStr := 'NERR_RplConfigNotFound' ;
                            HrDescr := 'Configuration record was not found.';
                          end;
    LongInt($00000A41)  : begin
                            HrStr := 'NERR_RplAdapterInfoCorrupted' ;
                            HrDescr := 'Adapter ID record information has been corrupted.';
                          end;
    LongInt($00000A42)  : begin
                            HrStr := 'NERR_RplInternal' ;
                            HrDescr := 'An internal service error has occurred.';
                          end;
    LongInt($00000A43)  : begin
                            HrStr := 'NERR_RplVendorInfoCorrupted' ;
                            HrDescr := 'Vendor ID record information has been corrupted.';
                          end;
    LongInt($00000A44)  : begin
                            HrStr := 'NERR_RplBootInfoCorrupted' ;
                            HrDescr := 'Boot block record information has been corrupted.';
                          end;
    LongInt($00000A45)  : begin
                            HrStr := 'NERR_RplWkstaNeedsUserAcct' ;
                            HrDescr := 'The user account for this workstation record is missing.';
                          end;
    LongInt($00000A46)  : begin
                            HrStr := 'NERR_RplNeedsRPLUSERAcct' ;
                            HrDescr := 'The RPLUSER local group could not be found.';
                          end;
    LongInt($00000A47)  : begin
                            HrStr := 'NERR_RplBootNotFound' ;
                            HrDescr := 'Boot block record was not found.';
                          end;
    LongInt($00000A48)  : begin
                            HrStr := 'NERR_RplIncompatibleProfile' ;
                            HrDescr := 'Chosen profile is incompatible with this workstation.';
                          end;
    LongInt($00000A49)  : begin
                            HrStr := 'NERR_RplAdapterNameUnavailable' ;
                            HrDescr := 'Chosen network adapter ID is in use by some other workstation.';
                          end;
    LongInt($00000A4A)  : begin
                            HrStr := 'NERR_RplConfigNotEmpty' ;
                            HrDescr := 'There are profiles using this configuration.';
                          end;
    LongInt($00000A4B)  : begin
                            HrStr := 'NERR_RplBootInUse' ;
                            HrDescr := 'There are workstations, profiles, or configurations using this boot block.';
                          end;
    LongInt($00000A4C)  : begin
                            HrStr := 'NERR_RplBackupDatabase' ;
                            HrDescr := 'Service failed to back up the Remoteboot database.';
                          end;
    LongInt($00000A4D)  : begin
                            HrStr := 'NERR_RplAdapterNotFound' ;
                            HrDescr := 'Adapter record was not found.';
                          end;
    LongInt($00000A4E)  : begin
                            HrStr := 'NERR_RplVendorNotFound' ;
                            HrDescr := 'Vendor record was not found.';
                          end;
    LongInt($00000A4F)  : begin
                            HrStr := 'NERR_RplVendorNameUnavailable' ;
                            HrDescr := 'Vendor name is in use by some other vendor record.';
                          end;
    LongInt($00000A50)  : begin
                            HrStr := 'NERR_RplBootNameUnavailable' ;
                            HrDescr := 'The boot name or vendor ID is in use by some other boot block record.';
                          end;
    LongInt($00000A51)  : begin
                            HrStr := 'NERR_RplConfigNameUnavailable' ;
                            HrDescr := 'The configuration name is in use by some other configuration.';
                          end;
    LongInt($00000A64)  : begin
                            HrStr := 'NERR_DfsInternalCorruption' ;
                            HrDescr := 'The internal database maintained by the DFS service is corrupt.';
                          end;
    LongInt($00000A65)  : begin
                            HrStr := 'NERR_DfsVolumeDataCorrupt' ;
                            HrDescr := 'One of the records in the internal DFS database is corrupt.';
                          end;
    LongInt($00000A66)  : begin
                            HrStr := 'NERR_DfsNoSuchVolume' ;
                            HrDescr := 'There is no DFS name whose entry path matches the input entry path.';
                          end;
    LongInt($00000A67)  : begin
                            HrStr := 'NERR_DfsVolumeAlreadyExists' ;
                            HrDescr := 'A root or link with the given name already exists.';
                          end;
    LongInt($00000A68)  : begin
                            HrStr := 'NERR_DfsAlreadyShared' ;
                            HrDescr := 'The server share specified is already shared in the DFS.';
                          end;
    LongInt($00000A69)  : begin
                            HrStr := 'NERR_DfsNoSuchShare' ;
                            HrDescr := 'The indicated server share does not support the indicated DFS namespace.';
                          end;
    LongInt($00000A6A)  : begin
                            HrStr := 'NERR_DfsNotALeafVolume' ;
                            HrDescr := 'The operation is not valid in this portion of the namespace.';
                          end;
    LongInt($00000A6B)  : begin
                            HrStr := 'NERR_DfsLeafVolume' ;
                            HrDescr := 'The operation is not valid in this portion of the namespace.';
                          end;
    LongInt($00000A6C)  : begin
                            HrStr := 'NERR_DfsVolumeHasMultipleServers' ;
                            HrDescr := 'The operation is ambiguous because the link has multiple servers.';
                          end;
    LongInt($00000A6D)  : begin
                            HrStr := 'NERR_DfsCantCreateJunctionPoint' ;
                            HrDescr := 'Unable to create a link.';
                          end;
    LongInt($00000A6E)  : begin
                            HrStr := 'NERR_DfsServerNotDfsAware' ;
                            HrDescr := 'The server is not DFS-aware.';
                          end;
    LongInt($00000A6F)  : begin
                            HrStr := 'NERR_DfsBadRenamePath' ;
                            HrDescr := 'The specified rename target path is invalid.';
                          end;
    LongInt($00000A70)  : begin
                            HrStr := 'NERR_DfsVolumeIsOffline' ;
                            HrDescr := 'The specified DFS link is offline.';
                          end;
    LongInt($00000A71)  : begin
                            HrStr := 'NERR_DfsNoSuchServer' ;
                            HrDescr := 'The specified server is not a server for this link.';
                          end;
    LongInt($00000A72)  : begin
                            HrStr := 'NERR_DfsCyclicalName' ;
                            HrDescr := 'A cycle in the DFS name was detected.';
                          end;
    LongInt($00000A73)  : begin
                            HrStr := 'NERR_DfsNotSupportedInServerDfs' ;
                            HrDescr := 'The operation is not supported on a server-based DFS.';
                          end;
    LongInt($00000A74)  : begin
                            HrStr := 'NERR_DfsDuplicateService' ;
                            HrDescr := 'This link is already supported by the specified server share.';
                          end;
    LongInt($00000A75)  : begin
                            HrStr := 'NERR_DfsCantRemoveLastServerShare' ;
                            HrDescr := 'Cannot remove the last server share supporting this root or link.';
                          end;
    LongInt($00000A76)  : begin
                            HrStr := 'NERR_DfsVolumeIsInterDfs' ;
                            HrDescr := 'The operation is not supported for an inter-DFS link.';
                          end;
    LongInt($00000A77)  : begin
                            HrStr := 'NERR_DfsInconsistent' ;
                            HrDescr := 'The internal state of the DFS Service has become inconsistent.';
                          end;
    LongInt($00000A78)  : begin
                            HrStr := 'NERR_DfsServerUpgraded' ;
                            HrDescr := 'The DFS Service has been installed on the specified server.';
                          end;
    LongInt($00000A79)  : begin
                            HrStr := 'NERR_DfsDataIsIdentical' ;
                            HrDescr := 'The DFS data being reconciled is identical.';
                          end;
    LongInt($00000A7A)  : begin
                            HrStr := 'NERR_DfsCantRemoveDfsRoot' ;
                            HrDescr := 'The DFS root cannot be deleted.' +
                                       'Uninstall DFS if required.';
                          end;
    LongInt($00000A7B)  : begin
                            HrStr := 'NERR_DfsChildOrParentInDfs' ;
                            HrDescr := 'A child or parent directory of the share is already in a DFS.';
                          end;
    LongInt($00000A82)  : begin
                            HrStr := 'NERR_DfsInternalError' ;
                            HrDescr := 'DFS internal error.';
                          end;
    LongInt($00000A83)  : begin
                            HrStr := 'NERR_SetupAlreadyJoined' ;
                            HrDescr := 'This machine is already joined to a domain.';
                          end;
    LongInt($00000A84)  : begin
                            HrStr := 'NERR_SetupNotJoined' ;
                            HrDescr := 'This machine is not currently joined to a domain.';
                          end;
    LongInt($00000A85)  : begin
                            HrStr := 'NERR_SetupDomainController' ;
                            HrDescr := 'This machine is a domain controller and cannot be unjoined from a domain.';
                          end;
    LongInt($00000A86)  : begin
                            HrStr := 'NERR_DefaultJoinRequired' ;
                            HrDescr := 'The destination domain controller does not support creating machine accounts in organizational units (OUs).';
                          end;
    LongInt($00000A87)  : begin
                            HrStr := 'NERR_InvalidWorkgroupName' ;
                            HrDescr := 'The specified workgroup name is invalid.';
                          end;
    LongInt($00000A88)  : begin
                            HrStr := 'NERR_NameUsesIncompatibleCodePage' ;
                            HrDescr := 'The specified computer name is incompatible with the default language used on the domain controller.';
                          end;
    LongInt($00000A89)  : begin
                            HrStr := 'NERR_ComputerAccountNotFound' ;
                            HrDescr := 'The specified computer account could not be found.';
                          end;
    LongInt($00000A8A)  : begin
                            HrStr := 'NERR_PersonalSku' ;
                            HrDescr := 'This version of Windows cannot be joined to a domain.';
                          end;
    LongInt($00000A8D)  : begin
                            HrStr := 'NERR_PasswordMustChange' ;
                            HrDescr := 'The password must change at the next logon.';
                          end;
    LongInt($00000A8E)  : begin
                            HrStr := 'NERR_AccountLockedOut' ;
                            HrDescr := 'The account is locked out.';
                          end;
    LongInt($00000A8F)  : begin
                            HrStr := 'NERR_PasswordTooLong' ;
                            HrDescr := 'The password is too long.';
                          end;
    LongInt($00000A90)  : begin
                            HrStr := 'NERR_PasswordNotComplexEnough' ;
                            HrDescr := 'The password does not meet the complexity policy.';
                          end;
    LongInt($00000A91)  : begin
                            HrStr := 'NERR_PasswordFilterError' ;
                            HrDescr := 'The password does not meet the requirements of the password filter DLLs.';
                          end;
    LongInt($00000BB8)  : begin
                            HrStr := 'ERROR_UNKNOWN_PRINT_MONITOR' ;
                            HrDescr := 'The specified print monitor is unknown.';
                          end;
    LongInt($00000BB9)  : begin
                            HrStr := 'ERROR_PRINTER_DRIVER_IN_USE' ;
                            HrDescr := 'The specified printer driver is currently in use.';
                          end;
    LongInt($00000BBA)  : begin
                            HrStr := 'ERROR_SPOOL_FILE_NOT_FOUND' ;
                            HrDescr := 'The spool file was not found.';
                          end;
    LongInt($00000BBB)  : begin
                            HrStr := 'ERROR_SPL_NO_STARTDOC' ;
                            HrDescr := 'A StartDocPrinter call was not issued.';
                          end;
    LongInt($00000BBC)  : begin
                            HrStr := 'ERROR_SPL_NO_ADDJOB' ;
                            HrDescr := 'An AddJob call was not issued.';
                          end;
    LongInt($00000BBD)  : begin
                            HrStr := 'ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED' ;
                            HrDescr := 'The specified print processor has already been installed.';
                          end;
    LongInt($00000BBE)  : begin
                            HrStr := 'ERROR_PRINT_MONITOR_ALREADY_INSTALLED' ;
                            HrDescr := 'The specified print monitor has already been installed.';
                          end;
    LongInt($00000BBF)  : begin
                            HrStr := 'ERROR_INVALID_PRINT_MONITOR' ;
                            HrDescr := 'The specified print monitor does not have the required functions.';
                          end;
    LongInt($00000BC0)  : begin
                            HrStr := 'ERROR_PRINT_MONITOR_IN_USE' ;
                            HrDescr := 'The specified print monitor is currently in use.';
                          end;
    LongInt($00000BC1)  : begin
                            HrStr := 'ERROR_PRINTER_HAS_JOBS_QUEUED' ;
                            HrDescr := 'The requested operation is not allowed when there are jobs queued to the printer.';
                          end;
    LongInt($00000BC2)  : begin
                            HrStr := 'ERROR_SUCCESS_REBOOT_REQUIRED' ;
                            HrDescr := 'The requested operation is successful.' +
                                       'Changes will not be effective until the system is rebooted.';
                          end;
    LongInt($00000BC3)  : begin
                            HrStr := 'ERROR_SUCCESS_RESTART_REQUIRED' ;
                            HrDescr := 'The requested operation is successful.' +
                                       'Changes will not be effective until the service is restarted.';
                          end;
    LongInt($00000BC4)  : begin
                            HrStr := 'ERROR_PRINTER_NOT_FOUND' ;
                            HrDescr := 'No printers were found.';
                          end;
    LongInt($00000BC5)  : begin
                            HrStr := 'ERROR_PRINTER_DRIVER_WARNED' ;
                            HrDescr := 'The printer driver is known to be unreliable.';
                          end;
    LongInt($00000BC6)  : begin
                            HrStr := 'ERROR_PRINTER_DRIVER_BLOCKED' ;
                            HrDescr := 'The printer driver is known to harm the system.';
                          end;
    LongInt($00000BC7)  : begin
                            HrStr := 'ERROR_PRINTER_DRIVER_PACKAGE_IN_USE' ;
                            HrDescr := 'The specified printer driver package is currently in use.';
                          end;
    LongInt($00000BC8)  : begin
                            HrStr := 'ERROR_CORE_DRIVER_PACKAGE_NOT_FOUND' ;
                            HrDescr := 'Unable to find a core driver package that is required by the printer driver package.';
                          end;
    LongInt($00000BC9)  : begin
                            HrStr := 'ERROR_FAIL_REBOOT_REQUIRED' ;
                            HrDescr := 'The requested operation failed.' +
                                       'A system reboot is required to roll back changes made.';
                          end;
    LongInt($00000BCA)  : begin
                            HrStr := 'ERROR_FAIL_REBOOT_INITIATED' ;
                            HrDescr := 'The requested operation failed.' +
                                       'A system reboot has been initiated to roll back changes made.';
                          end;
    LongInt($00000BCB)  : begin
                            HrStr := 'ERROR_PRINTER_DRIVER_DOWNLOAD_NEEDED' ;
                            HrDescr := 'The specified printer driver was not found on the system and needs to be downloaded.';
                          end;
    LongInt($00000BCE)  : begin
                            HrStr := 'ERROR_PRINTER_NOT_SHAREABLE' ;
                            HrDescr := 'The specified printer cannot be shared.';
                          end;
    LongInt($00000F6E)  : begin
                            HrStr := 'ERROR_IO_REISSUE_AS_CACHED' ;
                            HrDescr := 'Reissue the given operation as a cached I/O operation.';
                          end;
    LongInt($00000FA0)  : begin
                            HrStr := 'ERROR_WINS_INTERNAL' ;
                            HrDescr := 'Windows Internet Name Service (WINS) encountered an error while processing the command.';
                          end;
    LongInt($00000FA1)  : begin
                            HrStr := 'ERROR_CAN_NOT_DEL_LOCAL_WINS' ;
                            HrDescr := 'The local WINS cannot be deleted.';
                          end;
    LongInt($00000FA2)  : begin
                            HrStr := 'ERROR_STATIC_INIT' ;
                            HrDescr := 'The importation from the file failed.';
                          end;
    LongInt($00000FA3)  : begin
                            HrStr := 'ERROR_INC_BACKUP' ;
                            HrDescr := 'The backup failed.' +
                                       'Was a full backup done before?';
                          end;
    LongInt($00000FA4)  : begin
                            HrStr := 'ERROR_FULL_BACKUP' ;
                            HrDescr := 'The backup failed.' +
                                       'Check the directory to which you are backing the database.';
                          end;
    LongInt($00000FA5)  : begin
                            HrStr := 'ERROR_REC_NON_EXISTENT' ;
                            HrDescr := 'The name does not exist in the WINS database.';
                          end;
    LongInt($00000FA6)  : begin
                            HrStr := 'ERROR_RPL_NOT_ALLOWED' ;
                            HrDescr := 'Replication with a nonconfigured partner is not allowed.';
                          end;
    LongInt($00000FD2)  : begin
                            HrStr := 'PEERDIST_ERROR_CONTENTINFO_VERSION_UNSUPPORTED' ;
                            HrDescr := 'The version of the supplied content information is not supported.';
                          end;
    LongInt($00000FD3)  : begin
                            HrStr := 'PEERDIST_ERROR_CANNOT_PARSE_CONTENTINFO' ;
                            HrDescr := 'The supplied content information is malformed.';
                          end;
    LongInt($00000FD4)  : begin
                            HrStr := 'PEERDIST_ERROR_MISSING_DATA' ;
                            HrDescr := 'The requested data cannot be found in local or peer caches.';
                          end;
    LongInt($00000FD5)  : begin
                            HrStr := 'PEERDIST_ERROR_NO_MORE' ;
                            HrDescr := 'No more data is available or required.';
                          end;
    LongInt($00000FD6)  : begin
                            HrStr := 'PEERDIST_ERROR_NOT_INITIALIZED' ;
                            HrDescr := 'The supplied object has not been initialized.';
                          end;
    LongInt($00000FD7)  : begin
                            HrStr := 'PEERDIST_ERROR_ALREADY_INITIALIZED' ;
                            HrDescr := 'The supplied object has already been initialized.';
                          end;
    LongInt($00000FD8)  : begin
                            HrStr := 'PEERDIST_ERROR_SHUTDOWN_IN_PROGRESS' ;
                            HrDescr := 'A shutdown operation is already in progress.';
                          end;
    LongInt($00000FD9)  : begin
                            HrStr := 'PEERDIST_ERROR_INVALIDATED' ;
                            HrDescr := 'The supplied object has already been invalidated.';
                          end;
    LongInt($00000FDA)  : begin
                            HrStr := 'PEERDIST_ERROR_ALREADY_EXISTS' ;
                            HrDescr := 'An element already exists and was not replaced.';
                          end;
    LongInt($00000FDB)  : begin
                            HrStr := 'PEERDIST_ERROR_OPERATION_NOTFOUND' ;
                            HrDescr := 'Cannot cancel the requested operation as it has already been completed.';
                          end;
    LongInt($00000FDC)  : begin
                            HrStr := 'PEERDIST_ERROR_ALREADY_COMPLETED' ;
                            HrDescr := 'Cannot perform the requested operation because it has already been carried out.';
                          end;
    LongInt($00000FDD)  : begin
                            HrStr := 'PEERDIST_ERROR_OUT_OF_BOUNDS' ;
                            HrDescr := 'An operation accessed data beyond the bounds of valid data.';
                          end;
    LongInt($00000FDE)  : begin
                            HrStr := 'PEERDIST_ERROR_VERSION_UNSUPPORTED' ;
                            HrDescr := 'The requested version is not supported.';
                          end;
    LongInt($00000FDF)  : begin
                            HrStr := 'PEERDIST_ERROR_INVALID_CONFIGURATION' ;
                            HrDescr := 'A configuration value is invalid.';
                          end;
    LongInt($00000FE0)  : begin
                            HrStr := 'PEERDIST_ERROR_NOT_LICENSED' ;
                            HrDescr := 'The SKU is not licensed.';
                          end;
    LongInt($00000FE1)  : begin
                            HrStr := 'PEERDIST_ERROR_SERVICE_UNAVAILABLE' ;
                            HrDescr := 'PeerDist Service is still initializing and will be available shortly.';
                          end;
    LongInt($00001004)  : begin
                            HrStr := 'ERROR_DHCP_ADDRESS_CONFLICT' ;
                            HrDescr := 'The Dynamic Host Configuration Protocol (DHCP) client has obtained an IP address that is already in use on the network.' +
                                       'The local interface will be disabled until the DHCP client can obtain a new address.';
                          end;
    LongInt($00001068)  : begin
                            HrStr := 'ERROR_WMI_GUID_NOT_FOUND' ;
                            HrDescr := 'The GUID passed was not recognized as valid by a WMI data provider.';
                          end;
    LongInt($00001069)  : begin
                            HrStr := 'ERROR_WMI_INSTANCE_NOT_FOUND' ;
                            HrDescr := 'The instance name passed was not recognized as valid by a WMI data provider.';
                          end;
    LongInt($0000106A)  : begin
                            HrStr := 'ERROR_WMI_ITEMID_NOT_FOUND' ;
                            HrDescr := 'The data item ID passed was not recognized as valid by a WMI data provider.';
                          end;
    LongInt($0000106B)  : begin
                            HrStr := 'ERROR_WMI_TRY_AGAIN' ;
                            HrDescr := 'The WMI request could not be completed and should be retried.';
                          end;
    LongInt($0000106C)  : begin
                            HrStr := 'ERROR_WMI_DP_NOT_FOUND' ;
                            HrDescr := 'The WMI data provider could not be located.';
                          end;
    LongInt($0000106D)  : begin
                            HrStr := 'ERROR_WMI_UNRESOLVED_INSTANCE_REF' ;
                            HrDescr := 'The WMI data provider references an instance set that has not been registered.';
                          end;
    LongInt($0000106E)  : begin
                            HrStr := 'ERROR_WMI_ALREADY_ENABLED' ;
                            HrDescr := 'The WMI data block or event notification has already been enabled.';
                          end;
    LongInt($0000106F)  : begin
                            HrStr := 'ERROR_WMI_GUID_DISCONNECTED' ;
                            HrDescr := 'The WMI data block is no longer available.';
                          end;
    LongInt($00001070)  : begin
                            HrStr := 'ERROR_WMI_SERVER_UNAVAILABLE' ;
                            HrDescr := 'The WMI data service is not available.';
                          end;
    LongInt($00001071)  : begin
                            HrStr := 'ERROR_WMI_DP_FAILED' ;
                            HrDescr := 'The WMI data provider failed to carry out the request.';
                          end;
    LongInt($00001072)  : begin
                            HrStr := 'ERROR_WMI_INVALID_MOF' ;
                            HrDescr := 'The WMI Managed Object Format (MOF) information is not valid.';
                          end;
    LongInt($00001073)  : begin
                            HrStr := 'ERROR_WMI_INVALID_REGINFO' ;
                            HrDescr := 'The WMI registration information is not valid.';
                          end;
    LongInt($00001074)  : begin
                            HrStr := 'ERROR_WMI_ALREADY_DISABLED' ;
                            HrDescr := 'The WMI data block or event notification has already been disabled.';
                          end;
    LongInt($00001075)  : begin
                            HrStr := 'ERROR_WMI_READ_ONLY' ;
                            HrDescr := 'The WMI data item or data block is read-only.';
                          end;
    LongInt($00001076)  : begin
                            HrStr := 'ERROR_WMI_SET_FAILURE' ;
                            HrDescr := 'The WMI data item or data block could not be changed.';
                          end;
    LongInt($000010CC)  : begin
                            HrStr := 'ERROR_INVALID_MEDIA' ;
                            HrDescr := 'The media identifier does not represent a valid medium.';
                          end;
    LongInt($000010CD)  : begin
                            HrStr := 'ERROR_INVALID_LIBRARY' ;
                            HrDescr := 'The library identifier does not represent a valid library.';
                          end;
    LongInt($000010CE)  : begin
                            HrStr := 'ERROR_INVALID_MEDIA_POOL' ;
                            HrDescr := 'The media pool identifier does not represent a valid media pool.';
                          end;
    LongInt($000010CF)  : begin
                            HrStr := 'ERROR_DRIVE_MEDIA_MISMATCH' ;
                            HrDescr := 'The drive and medium are not compatible, or they exist in different libraries.';
                          end;
    LongInt($000010D0)  : begin
                            HrStr := 'ERROR_MEDIA_OFFLINE' ;
                            HrDescr := 'The medium currently exists in an offline library and must be online to perform this operation.';
                          end;
    LongInt($000010D1)  : begin
                            HrStr := 'ERROR_LIBRARY_OFFLINE' ;
                            HrDescr := 'The operation cannot be performed on an offline library.';
                          end;
    LongInt($000010D2)  : begin
                            HrStr := 'ERROR_EMPTY' ;
                            HrDescr := 'The library, drive, or media pool is empty.';
                          end;
    LongInt($000010D3)  : begin
                            HrStr := 'ERROR_NOT_EMPTY' ;
                            HrDescr := 'The library, drive, or media pool must be empty to perform this operation.';
                          end;
    LongInt($000010D4)  : begin
                            HrStr := 'ERROR_MEDIA_UNAVAILABLE' ;
                            HrDescr := 'No media is currently available in this media pool or library.';
                          end;
    LongInt($000010D5)  : begin
                            HrStr := 'ERROR_RESOURCE_DISABLED' ;
                            HrDescr := 'A resource required for this operation is disabled.';
                          end;
    LongInt($000010D6)  : begin
                            HrStr := 'ERROR_INVALID_CLEANER' ;
                            HrDescr := 'The media identifier does not represent a valid cleaner.';
                          end;
    LongInt($000010D7)  : begin
                            HrStr := 'ERROR_UNABLE_TO_CLEAN' ;
                            HrDescr := 'The drive cannot be cleaned or does not support cleaning.';
                          end;
    LongInt($000010D8)  : begin
                            HrStr := 'ERROR_OBJECT_NOT_FOUND' ;
                            HrDescr := 'The object identifier does not represent a valid object.';
                          end;
    LongInt($000010D9)  : begin
                            HrStr := 'ERROR_DATABASE_FAILURE' ;
                            HrDescr := 'Unable to read from or write to the database.';
                          end;
    LongInt($000010DA)  : begin
                            HrStr := 'ERROR_DATABASE_FULL' ;
                            HrDescr := 'The database is full.';
                          end;
    LongInt($000010DB)  : begin
                            HrStr := 'ERROR_MEDIA_INCOMPATIBLE' ;
                            HrDescr := 'The medium is not compatible with the device or media pool.';
                          end;
    LongInt($000010DC)  : begin
                            HrStr := 'ERROR_RESOURCE_NOT_PRESENT' ;
                            HrDescr := 'The resource required for this operation does not exist.';
                          end;
    LongInt($000010DD)  : begin
                            HrStr := 'ERROR_INVALID_OPERATION' ;
                            HrDescr := 'The operation identifier is not valid.';
                          end;
    LongInt($000010DE)  : begin
                            HrStr := 'ERROR_MEDIA_NOT_AVAILABLE' ;
                            HrDescr := 'The media is not mounted or ready for use.';
                          end;
    LongInt($000010DF)  : begin
                            HrStr := 'ERROR_DEVICE_NOT_AVAILABLE' ;
                            HrDescr := 'The device is not ready for use.';
                          end;
    LongInt($000010E0)  : begin
                            HrStr := 'ERROR_REQUEST_REFUSED' ;
                            HrDescr := 'The operator or administrator has refused the request.';
                          end;
    LongInt($000010E1)  : begin
                            HrStr := 'ERROR_INVALID_DRIVE_OBJECT' ;
                            HrDescr := 'The drive identifier does not represent a valid drive.';
                          end;
    LongInt($000010E2)  : begin
                            HrStr := 'ERROR_LIBRARY_FULL' ;
                            HrDescr := 'Library is full.' +
                                       'No slot is available for use.';
                          end;
    LongInt($000010E3)  : begin
                            HrStr := 'ERROR_MEDIUM_NOT_ACCESSIBLE' ;
                            HrDescr := 'The transport cannot access the medium.';
                          end;
    LongInt($000010E4)  : begin
                            HrStr := 'ERROR_UNABLE_TO_LOAD_MEDIUM' ;
                            HrDescr := 'Unable to load the medium into the drive.';
                          end;
    LongInt($000010E5)  : begin
                            HrStr := 'ERROR_UNABLE_TO_INVENTORY_DRIVE' ;
                            HrDescr := 'Unable to retrieve the drive status.';
                          end;
    LongInt($000010E6)  : begin
                            HrStr := 'ERROR_UNABLE_TO_INVENTORY_SLOT' ;
                            HrDescr := 'Unable to retrieve the slot status.';
                          end;
    LongInt($000010E7)  : begin
                            HrStr := 'ERROR_UNABLE_TO_INVENTORY_TRANSPORT' ;
                            HrDescr := 'Unable to retrieve status about the transport.';
                          end;
    LongInt($000010E8)  : begin
                            HrStr := 'ERROR_TRANSPORT_FULL' ;
                            HrDescr := 'Cannot use the transport because it is already in use.';
                          end;
    LongInt($000010E9)  : begin
                            HrStr := 'ERROR_CONTROLLING_IEPORT' ;
                            HrDescr := 'Unable to open or close the inject/eject port.';
                          end;
    LongInt($000010EA)  : begin
                            HrStr := 'ERROR_UNABLE_TO_EJECT_MOUNTED_MEDIA' ;
                            HrDescr := 'Unable to eject the medium because it is in a drive.';
                          end;
    LongInt($000010EB)  : begin
                            HrStr := 'ERROR_CLEANER_SLOT_SET' ;
                            HrDescr := 'A cleaner slot is already reserved.';
                          end;
    LongInt($000010EC)  : begin
                            HrStr := 'ERROR_CLEANER_SLOT_NOT_SET' ;
                            HrDescr := 'A cleaner slot is not reserved.';
                          end;
    LongInt($000010ED)  : begin
                            HrStr := 'ERROR_CLEANER_CARTRIDGE_SPENT' ;
                            HrDescr := 'The cleaner cartridge has performed the maximum number of drive cleanings.';
                          end;
    LongInt($000010EE)  : begin
                            HrStr := 'ERROR_UNEXPECTED_OMID' ;
                            HrDescr := 'Unexpected on-medium identifier.';
                          end;
    LongInt($000010EF)  : begin
                            HrStr := 'ERROR_CANT_DELETE_LAST_ITEM' ;
                            HrDescr := 'The last remaining item in this group or resource cannot be deleted.';
                          end;
    LongInt($000010F0)  : begin
                            HrStr := 'ERROR_MESSAGE_EXCEEDS_MAX_SIZE' ;
                            HrDescr := 'The message provided exceeds the maximum size allowed for this parameter.';
                          end;
    LongInt($000010F1)  : begin
                            HrStr := 'ERROR_VOLUME_CONTAINS_SYS_FILES' ;
                            HrDescr := 'The volume contains system or paging files.';
                          end;
    LongInt($000010F2)  : begin
                            HrStr := 'ERROR_INDIGENOUS_TYPE' ;
                            HrDescr := 'The media type cannot be removed from this library because at least one drive in the library reports it can support this media type.';
                          end;
    LongInt($000010F3)  : begin
                            HrStr := 'ERROR_NO_SUPPORTING_DRIVES' ;
                            HrDescr := 'This offline media cannot be mounted on this system because no enabled drives are present that can be used.';
                          end;
    LongInt($000010F4)  : begin
                            HrStr := 'ERROR_CLEANER_CARTRIDGE_INSTALLED' ;
                            HrDescr := 'A cleaner cartridge is present in the tape library.';
                          end;
    LongInt($000010F5)  : begin
                            HrStr := 'ERROR_IEPORT_FULL' ;
                            HrDescr := 'Cannot use the IEport because it is not empty.';
                          end;
    LongInt($000010FE)  : begin
                            HrStr := 'ERROR_FILE_OFFLINE' ;
                            HrDescr := 'The remote storage service was not able to recall the file.';
                          end;
    LongInt($000010FF)  : begin
                            HrStr := 'ERROR_REMOTE_STORAGE_NOT_ACTIVE' ;
                            HrDescr := 'The remote storage service is not operational at this time.';
                          end;
    LongInt($00001100)  : begin
                            HrStr := 'ERROR_REMOTE_STORAGE_MEDIA_ERROR' ;
                            HrDescr := 'The remote storage service encountered a media error.';
                          end;
    LongInt($00001126)  : begin
                            HrStr := 'ERROR_NOT_A_REPARSE_POINT' ;
                            HrDescr := 'The file or directory is not a reparse point.';
                          end;
    LongInt($00001127)  : begin
                            HrStr := 'ERROR_REPARSE_ATTRIBUTE_CONFLICT' ;
                            HrDescr := 'The reparse point attribute cannot be set because it conflicts with an existing attribute.';
                          end;
    LongInt($00001128)  : begin
                            HrStr := 'ERROR_INVALID_REPARSE_DATA' ;
                            HrDescr := 'The data present in the reparse point buffer is invalid.';
                          end;
    LongInt($00001129)  : begin
                            HrStr := 'ERROR_REPARSE_TAG_INVALID' ;
                            HrDescr := 'The tag present in the reparse point buffer is invalid.';
                          end;
    LongInt($0000112A)  : begin
                            HrStr := 'ERROR_REPARSE_TAG_MISMATCH' ;
                            HrDescr := 'There is a mismatch between the tag specified in the request and the tag present in the reparse point.';
                          end;
    LongInt($00001194)  : begin
                            HrStr := 'ERROR_VOLUME_NOT_SIS_ENABLED' ;
                            HrDescr := 'Single Instance Storage (SIS) is not available on this volume.';
                          end;
    LongInt($00001389)  : begin
                            HrStr := 'ERROR_DEPENDENT_RESOURCE_EXISTS' ;
                            HrDescr := 'The operation cannot be completed because other resources depend on this resource.';
                          end;
    LongInt($0000138A)  : begin
                            HrStr := 'ERROR_DEPENDENCY_NOT_FOUND' ;
                            HrDescr := 'The cluster resource dependency cannot be found.';
                          end;
    LongInt($0000138B)  : begin
                            HrStr := 'ERROR_DEPENDENCY_ALREADY_EXISTS' ;
                            HrDescr := 'The cluster resource cannot be made dependent on the specified resource because it is already dependent.';
                          end;
    LongInt($0000138C)  : begin
                            HrStr := 'ERROR_RESOURCE_NOT_ONLINE' ;
                            HrDescr := 'The cluster resource is not online.';
                          end;
    LongInt($0000138D)  : begin
                            HrStr := 'ERROR_HOST_NODE_NOT_AVAILABLE' ;
                            HrDescr := 'A cluster node is not available for this operation.';
                          end;
    LongInt($0000138E)  : begin
                            HrStr := 'ERROR_RESOURCE_NOT_AVAILABLE' ;
                            HrDescr := 'The cluster resource is not available.';
                          end;
    LongInt($0000138F)  : begin
                            HrStr := 'ERROR_RESOURCE_NOT_FOUND' ;
                            HrDescr := 'The cluster resource could not be found.';
                          end;
    LongInt($00001390)  : begin
                            HrStr := 'ERROR_SHUTDOWN_CLUSTER' ;
                            HrDescr := 'The cluster is being shut down.';
                          end;
    LongInt($00001391)  : begin
                            HrStr := 'ERROR_CANT_EVICT_ACTIVE_NODE' ;
                            HrDescr := 'A cluster node cannot be evicted from the cluster unless the node is down or it is the last node.';
                          end;
    LongInt($00001392)  : begin
                            HrStr := 'ERROR_OBJECT_ALREADY_EXISTS' ;
                            HrDescr := 'The object already exists.';
                          end;
    LongInt($00001393)  : begin
                            HrStr := 'ERROR_OBJECT_IN_LIST' ;
                            HrDescr := 'The object is already in the list.';
                          end;
    LongInt($00001394)  : begin
                            HrStr := 'ERROR_GROUP_NOT_AVAILABLE' ;
                            HrDescr := 'The cluster group is not available for any new requests.';
                          end;
    LongInt($00001395)  : begin
                            HrStr := 'ERROR_GROUP_NOT_FOUND' ;
                            HrDescr := 'The cluster group could not be found.';
                          end;
    LongInt($00001396)  : begin
                            HrStr := 'ERROR_GROUP_NOT_ONLINE' ;
                            HrDescr := 'The operation could not be completed because the cluster group is not online.';
                          end;
    LongInt($00001397)  : begin
                            HrStr := 'ERROR_HOST_NODE_NOT_RESOURCE_OWNER' ;
                            HrDescr := 'The operation failed because either the specified cluster node is not the owner of the resource, or the node is not a possible owner of the resource.';
                          end;
    LongInt($00001398)  : begin
                            HrStr := 'ERROR_HOST_NODE_NOT_GROUP_OWNER' ;
                            HrDescr := 'The operation failed because either the specified cluster node is not the owner of the group, or the node is not a possible owner of the group.';
                          end;
    LongInt($00001399)  : begin
                            HrStr := 'ERROR_RESMON_CREATE_FAILED' ;
                            HrDescr := 'The cluster resource could not be created in the specified resource monitor.';
                          end;
    LongInt($0000139A)  : begin
                            HrStr := 'ERROR_RESMON_ONLINE_FAILED' ;
                            HrDescr := 'The cluster resource could not be brought online by the resource monitor.';
                          end;
    LongInt($0000139B)  : begin
                            HrStr := 'ERROR_RESOURCE_ONLINE' ;
                            HrDescr := 'The operation could not be completed because the cluster resource is online.';
                          end;
    LongInt($0000139C)  : begin
                            HrStr := 'ERROR_QUORUM_RESOURCE' ;
                            HrDescr := 'The cluster resource could not be deleted or brought offline because it is the quorum resource.';
                          end;
    LongInt($0000139D)  : begin
                            HrStr := 'ERROR_NOT_QUORUM_CAPABLE' ;
                            HrDescr := 'The cluster could not make the specified resource a quorum resource because it is not capable of being a quorum resource.';
                          end;
    LongInt($0000139E)  : begin
                            HrStr := 'ERROR_CLUSTER_SHUTTING_DOWN' ;
                            HrDescr := 'The cluster software is shutting down.';
                          end;
    LongInt($0000139F)  : begin
                            HrStr := 'ERROR_INVALID_STATE' ;
                            HrDescr := 'The group or resource is not in the correct state to perform the requested operation.';
                          end;
    LongInt($000013A0)  : begin
                            HrStr := 'ERROR_RESOURCE_PROPERTIES_STORED' ;
                            HrDescr := 'The properties were stored but not all changes will take effect until the next time the resource is brought online.';
                          end;
    LongInt($000013A1)  : begin
                            HrStr := 'ERROR_NOT_QUORUM_CLASS' ;
                            HrDescr := 'The cluster could not make the specified resource a quorum resource because it does not belong to a shared storage class.';
                          end;
    LongInt($000013A2)  : begin
                            HrStr := 'ERROR_CORE_RESOURCE' ;
                            HrDescr := 'The cluster resource could not be deleted because it is a core resource.';
                          end;
    LongInt($000013A3)  : begin
                            HrStr := 'ERROR_QUORUM_RESOURCE_ONLINE_FAILED' ;
                            HrDescr := 'The quorum resource failed to come online.';
                          end;
    LongInt($000013A4)  : begin
                            HrStr := 'ERROR_QUORUMLOG_OPEN_FAILED' ;
                            HrDescr := 'The quorum log could not be created or mounted successfully.';
                          end;
    LongInt($000013A5)  : begin
                            HrStr := 'ERROR_CLUSTERLOG_CORRUPT' ;
                            HrDescr := 'The cluster log is corrupt.';
                          end;
    LongInt($000013A6)  : begin
                            HrStr := 'ERROR_CLUSTERLOG_RECORD_EXCEEDS_MAXSIZE' ;
                            HrDescr := 'The record could not be written to the cluster log because it exceeds the maximum size.';
                          end;
    LongInt($000013A7)  : begin
                            HrStr := 'ERROR_CLUSTERLOG_EXCEEDS_MAXSIZE' ;
                            HrDescr := 'The cluster log exceeds its maximum size.';
                          end;
    LongInt($000013A8)  : begin
                            HrStr := 'ERROR_CLUSTERLOG_CHKPOINT_NOT_FOUND' ;
                            HrDescr := 'No checkpoint record was found in the cluster log.';
                          end;
    LongInt($000013A9)  : begin
                            HrStr := 'ERROR_CLUSTERLOG_NOT_ENOUGH_SPACE' ;
                            HrDescr := 'The minimum required disk space needed for logging is not available.';
                          end;
    LongInt($000013AA)  : begin
                            HrStr := 'ERROR_QUORUM_OWNER_ALIVE' ;
                            HrDescr := 'The cluster node failed to take control of the quorum resource because the resource is owned by another active node.';
                          end;
    LongInt($000013AB)  : begin
                            HrStr := 'ERROR_NETWORK_NOT_AVAILABLE' ;
                            HrDescr := 'A cluster network is not available for this operation.';
                          end;
    LongInt($000013AC)  : begin
                            HrStr := 'ERROR_NODE_NOT_AVAILABLE' ;
                            HrDescr := 'A cluster node is not available for this operation.';
                          end;
    LongInt($000013AD)  : begin
                            HrStr := 'ERROR_ALL_NODES_NOT_AVAILABLE' ;
                            HrDescr := 'All cluster nodes must be running to perform this operation.';
                          end;
    LongInt($000013AE)  : begin
                            HrStr := 'ERROR_RESOURCE_FAILED' ;
                            HrDescr := 'A cluster resource failed.';
                          end;
    LongInt($000013AF)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_NODE' ;
                            HrDescr := 'The cluster node is not valid.';
                          end;
    LongInt($000013B0)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_EXISTS' ;
                            HrDescr := 'The cluster node already exists.';
                          end;
    LongInt($000013B1)  : begin
                            HrStr := 'ERROR_CLUSTER_JOIN_IN_PROGRESS' ;
                            HrDescr := 'A node is in the process of joining the cluster.';
                          end;
    LongInt($000013B2)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_NOT_FOUND' ;
                            HrDescr := 'The cluster node was not found.';
                          end;
    LongInt($000013B3)  : begin
                            HrStr := 'ERROR_CLUSTER_LOCAL_NODE_NOT_FOUND' ;
                            HrDescr := 'The cluster local node information was not found.';
                          end;
    LongInt($000013B4)  : begin
                            HrStr := 'ERROR_CLUSTER_NETWORK_EXISTS' ;
                            HrDescr := 'The cluster network already exists.';
                          end;
    LongInt($000013B5)  : begin
                            HrStr := 'ERROR_CLUSTER_NETWORK_NOT_FOUND' ;
                            HrDescr := 'The cluster network was not found.';
                          end;
    LongInt($000013B6)  : begin
                            HrStr := 'ERROR_CLUSTER_NETINTERFACE_EXISTS' ;
                            HrDescr := 'The cluster network interface already exists.';
                          end;
    LongInt($000013B7)  : begin
                            HrStr := 'ERROR_CLUSTER_NETINTERFACE_NOT_FOUND' ;
                            HrDescr := 'The cluster network interface was not found.';
                          end;
    LongInt($000013B8)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_REQUEST' ;
                            HrDescr := 'The cluster request is not valid for this object.';
                          end;
    LongInt($000013B9)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_NETWORK_PROVIDER' ;
                            HrDescr := 'The cluster network provider is not valid.';
                          end;
    LongInt($000013BA)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_DOWN' ;
                            HrDescr := 'The cluster node is down.';
                          end;
    LongInt($000013BB)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_UNREACHABLE' ;
                            HrDescr := 'The cluster node is not reachable.';
                          end;
    LongInt($000013BC)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_NOT_MEMBER' ;
                            HrDescr := 'The cluster node is not a member of the cluster.';
                          end;
    LongInt($000013BD)  : begin
                            HrStr := 'ERROR_CLUSTER_JOIN_NOT_IN_PROGRESS' ;
                            HrDescr := 'A cluster join operation is not in progress.';
                          end;
    LongInt($000013BE)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_NETWORK' ;
                            HrDescr := 'The cluster network is not valid.';
                          end;
    LongInt($000013C0)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_UP' ;
                            HrDescr := 'The cluster node is up.';
                          end;
    LongInt($000013C1)  : begin
                            HrStr := 'ERROR_CLUSTER_IPADDR_IN_USE' ;
                            HrDescr := 'The cluster IP address is already in use.';
                          end;
    LongInt($000013C2)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_NOT_PAUSED' ;
                            HrDescr := 'The cluster node is not paused.';
                          end;
    LongInt($000013C3)  : begin
                            HrStr := 'ERROR_CLUSTER_NO_SECURITY_CONTEXT' ;
                            HrDescr := 'No cluster security context is available.';
                          end;
    LongInt($000013C4)  : begin
                            HrStr := 'ERROR_CLUSTER_NETWORK_NOT_INTERNAL' ;
                            HrDescr := 'The cluster network is not configured for internal cluster communication.';
                          end;
    LongInt($000013C5)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_ALREADY_UP' ;
                            HrDescr := 'The cluster node is already up.';
                          end;
    LongInt($000013C6)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_ALREADY_DOWN' ;
                            HrDescr := 'The cluster node is already down.';
                          end;
    LongInt($000013C7)  : begin
                            HrStr := 'ERROR_CLUSTER_NETWORK_ALREADY_ONLINE' ;
                            HrDescr := 'The cluster network is already online.';
                          end;
    LongInt($000013C8)  : begin
                            HrStr := 'ERROR_CLUSTER_NETWORK_ALREADY_OFFLINE' ;
                            HrDescr := 'The cluster network is already offline.';
                          end;
    LongInt($000013C9)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_ALREADY_MEMBER' ;
                            HrDescr := 'The cluster node is already a member of the cluster.';
                          end;
    LongInt($000013CA)  : begin
                            HrStr := 'ERROR_CLUSTER_LAST_INTERNAL_NETWORK' ;
                            HrDescr := 'The cluster network is the only one configured for internal cluster communication between two or more active cluster nodes.' +
                                       'The internal communication capability cannot be removed from the network.';
                          end;
    LongInt($000013CB)  : begin
                            HrStr := 'ERROR_CLUSTER_NETWORK_HAS_DEPENDENTS' ;
                            HrDescr := 'One or more cluster resources depend on the network to provide service to clients.' +
                                       'The client access capability cannot be removed from the network.';
                          end;
    LongInt($000013CC)  : begin
                            HrStr := 'ERROR_INVALID_OPERATION_ON_QUORUM' ;
                            HrDescr := 'This operation cannot be performed on the cluster resource because it is the quorum resource.' +
                                       'This quorum resource cannot be brought offline and its possible owners list cannot be modified.';
                          end;
    LongInt($000013CD)  : begin
                            HrStr := 'ERROR_DEPENDENCY_NOT_ALLOWED' ;
                            HrDescr := 'The cluster quorum resource is not allowed to have any dependencies.';
                          end;
    LongInt($000013CE)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_PAUSED' ;
                            HrDescr := 'The cluster node is paused.';
                          end;
    LongInt($000013CF)  : begin
                            HrStr := 'ERROR_NODE_CANT_HOST_RESOURCE' ;
                            HrDescr := 'The cluster resource cannot be brought online.' +
                                       'The owner node cannot run this resource.';
                          end;
    LongInt($000013D0)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_NOT_READY' ;
                            HrDescr := 'The cluster node is not ready to perform the requested operation.';
                          end;
    LongInt($000013D1)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_SHUTTING_DOWN' ;
                            HrDescr := 'The cluster node is shutting down.';
                          end;
    LongInt($000013D2)  : begin
                            HrStr := 'ERROR_CLUSTER_JOIN_ABORTED' ;
                            HrDescr := 'The cluster join operation was aborted.';
                          end;
    LongInt($000013D3)  : begin
                            HrStr := 'ERROR_CLUSTER_INCOMPATIBLE_VERSIONS' ;
                            HrDescr := 'The cluster join operation failed due to incompatible software versions between the joining node and its sponsor.';
                          end;
    LongInt($000013D4)  : begin
                            HrStr := 'ERROR_CLUSTER_MAXNUM_OF_RESOURCES_EXCEEDED' ;
                            HrDescr := 'This resource cannot be created because the cluster has reached the limit on the number of resources it can monitor.';
                          end;
    LongInt($000013D5)  : begin
                            HrStr := 'ERROR_CLUSTER_SYSTEM_CONFIG_CHANGED' ;
                            HrDescr := 'The system configuration changed during the cluster join or form operation.' +
                                       'The join or form operation was aborted.';
                          end;
    LongInt($000013D6)  : begin
                            HrStr := 'ERROR_CLUSTER_RESOURCE_TYPE_NOT_FOUND' ;
                            HrDescr := 'The specified resource type was not found.';
                          end;
    LongInt($000013D7)  : begin
                            HrStr := 'ERROR_CLUSTER_RESTYPE_NOT_SUPPORTED' ;
                            HrDescr := 'The specified node does not support a resource of this type.' +
                                       'This might be due to version inconsistencies or due to the absence of the resource DLL on this node.';
                          end;
    LongInt($000013D8)  : begin
                            HrStr := 'ERROR_CLUSTER_RESNAME_NOT_FOUND' ;
                            HrDescr := 'The specified resource name is not supported by this resource DLL.' +
                                       'This might be due to a bad (or changed) name supplied to the resource DLL.';
                          end;
    LongInt($000013D9)  : begin
                            HrStr := 'ERROR_CLUSTER_NO_RPC_PACKAGES_REGISTERED' ;
                            HrDescr := 'No authentication package could be registered with the RPC server.';
                          end;
    LongInt($000013DA)  : begin
                            HrStr := 'ERROR_CLUSTER_OWNER_NOT_IN_PREFLIST' ;
                            HrDescr := 'You cannot bring the group online because the owner of the group is not in the preferred list for the group.' +
                                       'To change the owner node for the group, move the group.';
                          end;
    LongInt($000013DB)  : begin
                            HrStr := 'ERROR_CLUSTER_DATABASE_SEQMISMATCH' ;
                            HrDescr := 'The join operation failed because the cluster database sequence number has changed or is incompatible with the locker node.' +
                                       'This can happen during a join operation if the cluster database was changing during the join.';
                          end;
    LongInt($000013DC)  : begin
                            HrStr := 'ERROR_RESMON_INVALID_STATE' ;
                            HrDescr := 'The resource monitor will not allow the fail operation to be performed while the resource is in its current state.' +
                                       'This can happen if the resource is in a pending state.';
                          end;
    LongInt($000013DD)  : begin
                            HrStr := 'ERROR_CLUSTER_GUM_NOT_LOCKER' ;
                            HrDescr := 'A non-locker code received a request to reserve the lock for making global updates.';
                          end;
    LongInt($000013DE)  : begin
                            HrStr := 'ERROR_QUORUM_DISK_NOT_FOUND' ;
                            HrDescr := 'The quorum disk could not be located by the cluster service.';
                          end;
    LongInt($000013DF)  : begin
                            HrStr := 'ERROR_DATABASE_BACKUP_CORRUPT' ;
                            HrDescr := 'The backed-up cluster database is possibly corrupt.';
                          end;
    LongInt($000013E0)  : begin
                            HrStr := 'ERROR_CLUSTER_NODE_ALREADY_HAS_DFS_ROOT' ;
                            HrDescr := 'A DFS root already exists in this cluster node.';
                          end;
    LongInt($000013E1)  : begin
                            HrStr := 'ERROR_RESOURCE_PROPERTY_UNCHANGEABLE' ;
                            HrDescr := 'An attempt to modify a resource property failed because it conflicts with another existing property.';
                          end;
    LongInt($00001702)  : begin
                            HrStr := 'ERROR_CLUSTER_MEMBERSHIP_INVALID_STATE' ;
                            HrDescr := 'An operation was attempted that is incompatible with the current membership state of the node.';
                          end;
    LongInt($00001703)  : begin
                            HrStr := 'ERROR_CLUSTER_QUORUMLOG_NOT_FOUND' ;
                            HrDescr := 'The quorum resource does not contain the quorum log.';
                          end;
    LongInt($00001704)  : begin
                            HrStr := 'ERROR_CLUSTER_MEMBERSHIP_HALT' ;
                            HrDescr := 'The membership engine requested shutdown of the cluster service on this node.';
                          end;
    LongInt($00001705)  : begin
                            HrStr := 'ERROR_CLUSTER_INSTANCE_ID_MISMATCH' ;
                            HrDescr := 'The join operation failed because the cluster instance ID of the joining node does not match the cluster instance ID of the sponsor node.';
                          end;
    LongInt($00001706)  : begin
                            HrStr := 'ERROR_CLUSTER_NETWORK_NOT_FOUND_FOR_IP' ;
                            HrDescr := 'A matching cluster network for the specified IP address could not be found.';
                          end;
    LongInt($00001707)  : begin
                            HrStr := 'ERROR_CLUSTER_PROPERTY_DATA_TYPE_MISMATCH' ;
                            HrDescr := 'The actual data type of the property did not match the expected data type of the property.';
                          end;
    LongInt($00001708)  : begin
                            HrStr := 'ERROR_CLUSTER_EVICT_WITHOUT_CLEANUP' ;
                            HrDescr := 'The cluster node was evicted from the cluster successfully, but the node was not cleaned up.' +
                                       'To determine what clean-up steps failed and how to recover, see the Failover Clustering application event log using Event Viewer.';
                          end;
    LongInt($00001709)  : begin
                            HrStr := 'ERROR_CLUSTER_PARAMETER_MISMATCH' ;
                            HrDescr := 'Two or more parameter values specified for a resource''s properties are in conflict.';
                          end;
    LongInt($0000170A)  : begin
                            HrStr := 'ERROR_NODE_CANNOT_BE_CLUSTERED' ;
                            HrDescr := 'This computer cannot be made a member of a cluster.';
                          end;
    LongInt($0000170B)  : begin
                            HrStr := 'ERROR_CLUSTER_WRONG_OS_VERSION' ;
                            HrDescr := 'This computer cannot be made a member of a cluster because it does not have the correct version of Windows installed.';
                          end;
    LongInt($0000170C)  : begin
                            HrStr := 'ERROR_CLUSTER_CANT_CREATE_DUP_CLUSTER_NAME' ;
                            HrDescr := 'A cluster cannot be created with the specified cluster name because that cluster name is already in use.' +
                                       'Specify a different name for the cluster.';
                          end;
    LongInt($0000170D)  : begin
                            HrStr := 'ERROR_CLUSCFG_ALREADY_COMMITTED' ;
                            HrDescr := 'The cluster configuration action has already been committed.';
                          end;
    LongInt($0000170E)  : begin
                            HrStr := 'ERROR_CLUSCFG_ROLLBACK_FAILED' ;
                            HrDescr := 'The cluster configuration action could not be rolled back.';
                          end;
    LongInt($0000170F)  : begin
                            HrStr := 'ERROR_CLUSCFG_SYSTEM_DISK_DRIVE_LETTER_CONFLICT' ;
                            HrDescr := 'The drive letter assigned to a system disk on one node conflicted with the drive letter assigned to a disk on another node.';
                          end;
    LongInt($00001710)  : begin
                            HrStr := 'ERROR_CLUSTER_OLD_VERSION' ;
                            HrDescr := 'One or more nodes in the cluster are running a version of Windows that does not support this operation.';
                          end;
    LongInt($00001711)  : begin
                            HrStr := 'ERROR_CLUSTER_MISMATCHED_COMPUTER_ACCT_NAME' ;
                            HrDescr := 'The name of the corresponding computer account does not match the network name for this resource.';
                          end;
    LongInt($00001712)  : begin
                            HrStr := 'ERROR_CLUSTER_NO_NET_ADAPTERS' ;
                            HrDescr := 'No network adapters are available.';
                          end;
    LongInt($00001713)  : begin
                            HrStr := 'ERROR_CLUSTER_POISONED' ;
                            HrDescr := 'The cluster node has been poisoned.';
                          end;
    LongInt($00001714)  : begin
                            HrStr := 'ERROR_CLUSTER_GROUP_MOVING' ;
                            HrDescr := 'The group is unable to accept the request because it is moving to another node.';
                          end;
    LongInt($00001715)  : begin
                            HrStr := 'ERROR_CLUSTER_RESOURCE_TYPE_BUSY' ;
                            HrDescr := 'The resource type cannot accept the request because it is too busy performing another operation.';
                          end;
    LongInt($00001716)  : begin
                            HrStr := 'ERROR_RESOURCE_CALL_TIMED_OUT' ;
                            HrDescr := 'The call to the cluster resource DLL timed out.';
                          end;
    LongInt($00001717)  : begin
                            HrStr := 'ERROR_INVALID_CLUSTER_IPV6_ADDRESS' ;
                            HrDescr := 'The address is not valid for an IPv6 Address resource.' +
                                       'A global IPv6 address is required, and it must match a cluster network.' +
                                       'Compatibility addresses are not permitted.';
                          end;
    LongInt($00001718)  : begin
                            HrStr := 'ERROR_CLUSTER_INTERNAL_INVALID_FUNCTION' ;
                            HrDescr := 'An internal cluster error occurred.' +
                                       'A call to an invalid function was attempted.';
                          end;
    LongInt($00001719)  : begin
                            HrStr := 'ERROR_CLUSTER_PARAMETER_OUT_OF_BOUNDS' ;
                            HrDescr := 'A parameter value is out of acceptable range.';
                          end;
    LongInt($0000171A)  : begin
                            HrStr := 'ERROR_CLUSTER_PARTIAL_SEND' ;
                            HrDescr := 'A network error occurred while sending data to another node in the cluster.' +
                                       'The number of bytes transmitted was less than required.';
                          end;
    LongInt($0000171B)  : begin
                            HrStr := 'ERROR_CLUSTER_REGISTRY_INVALID_FUNCTION' ;
                            HrDescr := 'An invalid cluster registry operation was attempted.';
                          end;
    LongInt($0000171C)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_STRING_TERMINATION' ;
                            HrDescr := 'An input string of characters is not properly terminated.';
                          end;
    LongInt($0000171D)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_STRING_FORMAT' ;
                            HrDescr := 'An input string of characters is not in a valid format for the data it represents.';
                          end;
    LongInt($0000171E)  : begin
                            HrStr := 'ERROR_CLUSTER_DATABASE_TRANSACTION_IN_PROGRESS' ;
                            HrDescr := 'An internal cluster error occurred.' +
                                       'A cluster database transaction was attempted while a transaction was already in progress.';
                          end;
    LongInt($0000171F)  : begin
                            HrStr := 'ERROR_CLUSTER_DATABASE_TRANSACTION_NOT_IN_PROGRESS' ;
                            HrDescr := 'An internal cluster error occurred.' +
                                       'There was an attempt to commit a cluster database transaction while no transaction was in progress.';
                          end;
    LongInt($00001720)  : begin
                            HrStr := 'ERROR_CLUSTER_NULL_DATA' ;
                            HrDescr := 'An internal cluster error occurred.' +
                                       'Data was not properly initialized.';
                          end;
    LongInt($00001721)  : begin
                            HrStr := 'ERROR_CLUSTER_PARTIAL_READ' ;
                            HrDescr := 'An error occurred while reading from a stream of data.' +
                                       'An unexpected number of bytes was returned.';
                          end;
    LongInt($00001722)  : begin
                            HrStr := 'ERROR_CLUSTER_PARTIAL_WRITE' ;
                            HrDescr := 'An error occurred while writing to a stream of data.' +
                                       'The required number of bytes could not be written.';
                          end;
    LongInt($00001723)  : begin
                            HrStr := 'ERROR_CLUSTER_CANT_DESERIALIZE_DATA' ;
                            HrDescr := 'An error occurred while deserializing a stream of cluster data.';
                          end;
    LongInt($00001724)  : begin
                            HrStr := 'ERROR_DEPENDENT_RESOURCE_PROPERTY_CONFLICT' ;
                            HrDescr := 'One or more property values for this resource are in conflict with one or more property values associated with its dependent resources.';
                          end;
    LongInt($00001725)  : begin
                            HrStr := 'ERROR_CLUSTER_NO_QUORUM' ;
                            HrDescr := 'A quorum of cluster nodes was not present to form a cluster.';
                          end;
    LongInt($00001726)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_IPV6_NETWORK' ;
                            HrDescr := 'The cluster network is not valid for an IPv6 address resource, or it does not match the configured address.';
                          end;
    LongInt($00001727)  : begin
                            HrStr := 'ERROR_CLUSTER_INVALID_IPV6_TUNNEL_NETWORK' ;
                            HrDescr := 'The cluster network is not valid for an IPv6 tunnel resource.' +
                                       'Check the configuration of the IP Address resource on which the IPv6 tunnel resource depends.';
                          end;
    LongInt($00001728)  : begin
                            HrStr := 'ERROR_QUORUM_NOT_ALLOWED_IN_THIS_GROUP' ;
                            HrDescr := 'Quorum resource cannot reside in the available storage group.';
                          end;
    LongInt($00001770)  : begin
                            HrStr := 'ERROR_ENCRYPTION_FAILED' ;
                            HrDescr := 'The specified file could not be encrypted.';
                          end;
    LongInt($00001771)  : begin
                            HrStr := 'ERROR_DECRYPTION_FAILED' ;
                            HrDescr := 'The specified file could not be decrypted.';
                          end;
    LongInt($00001772)  : begin
                            HrStr := 'ERROR_FILE_ENCRYPTED' ;
                            HrDescr := 'The specified file is encrypted and the user does not have the ability to decrypt it.';
                          end;
    LongInt($00001773)  : begin
                            HrStr := 'ERROR_NO_RECOVERY_POLICY' ;
                            HrDescr := 'There is no valid encryption recovery policy configured for this system.';
                          end;
    LongInt($00001774)  : begin
                            HrStr := 'ERROR_NO_EFS' ;
                            HrDescr := 'The required encryption driver is not loaded for this system.';
                          end;
    LongInt($00001775)  : begin
                            HrStr := 'ERROR_WRONG_EFS' ;
                            HrDescr := 'The file was encrypted with a different encryption driver than is currently loaded.';
                          end;
    LongInt($00001776)  : begin
                            HrStr := 'ERROR_NO_USER_KEYS' ;
                            HrDescr := 'There are no Encrypting File System (EFS) keys defined for the user.';
                          end;
    LongInt($00001777)  : begin
                            HrStr := 'ERROR_FILE_NOT_ENCRYPTED' ;
                            HrDescr := 'The specified file is not encrypted.';
                          end;
    LongInt($00001778)  : begin
                            HrStr := 'ERROR_NOT_EXPORT_FORMAT' ;
                            HrDescr := 'The specified file is not in the defined EFS export format.';
                          end;
    LongInt($00001779)  : begin
                            HrStr := 'ERROR_FILE_READ_ONLY' ;
                            HrDescr := 'The specified file is read-only.';
                          end;
    LongInt($0000177A)  : begin
                            HrStr := 'ERROR_DIR_EFS_DISALLOWED' ;
                            HrDescr := 'The directory has been disabled for encryption.';
                          end;
    LongInt($0000177B)  : begin
                            HrStr := 'ERROR_EFS_SERVER_NOT_TRUSTED' ;
                            HrDescr := 'The server is not trusted for remote encryption operation.';
                          end;
    LongInt($0000177C)  : begin
                            HrStr := 'ERROR_BAD_RECOVERY_POLICY' ;
                            HrDescr := 'Recovery policy configured for this system contains invalid recovery certificate.';
                          end;
    LongInt($0000177D)  : begin
                            HrStr := 'ERROR_EFS_ALG_BLOB_TOO_BIG' ;
                            HrDescr := 'The encryption algorithm used on the source file needs a bigger key buffer than the one on the destination file.';
                          end;
    LongInt($0000177E)  : begin
                            HrStr := 'ERROR_VOLUME_NOT_SUPPORT_EFS' ;
                            HrDescr := 'The disk partition does not support file encryption.';
                          end;
    LongInt($0000177F)  : begin
                            HrStr := 'ERROR_EFS_DISABLED' ;
                            HrDescr := 'This machine is disabled for file encryption.';
                          end;
    LongInt($00001780)  : begin
                            HrStr := 'ERROR_EFS_VERSION_NOT_SUPPORT' ;
                            HrDescr := 'A newer system is required to decrypt this encrypted file.';
                          end;
    LongInt($00001781)  : begin
                            HrStr := 'ERROR_CS_ENCRYPTION_INVALID_SERVER_RESPONSE' ;
                            HrDescr := 'The remote server sent an invalid response for a file being opened with client-side encryption.';
                          end;
    LongInt($00001782)  : begin
                            HrStr := 'ERROR_CS_ENCRYPTION_UNSUPPORTED_SERVER' ;
                            HrDescr := 'Client-side encryption is not supported by the remote server even though it claims to support it.';
                          end;
    LongInt($00001783)  : begin
                            HrStr := 'ERROR_CS_ENCRYPTION_EXISTING_ENCRYPTED_FILE' ;
                            HrDescr := 'File is encrypted and should be opened in client-side encryption mode.';
                          end;
    LongInt($00001784)  : begin
                            HrStr := 'ERROR_CS_ENCRYPTION_NEW_ENCRYPTED_FILE' ;
                            HrDescr := 'A new encrypted file is being created and a $EFS needs to be provided.';
                          end;
    LongInt($00001785)  : begin
                            HrStr := 'ERROR_CS_ENCRYPTION_FILE_NOT_CSE' ;
                            HrDescr := 'The SMB client requested a client-side extension (CSE) file system control (FSCTL) on a non-CSE file.';
                          end;
    LongInt($000017E6)  : begin
                            HrStr := 'ERROR_NO_BROWSER_SERVERS_FOUND' ;
                            HrDescr := 'The list of servers for this workgroup is not currently available';
                          end;
    LongInt($00001838)  : begin
                            HrStr := 'SCHED_E_SERVICE_NOT_LOCALSYSTEM' ;
                            HrDescr := 'The Task Scheduler service must be configured to run in the System account to function properly.' +
                                       'Individual tasks can be configured to run in other accounts.';
                          end;
    LongInt($000019C8)  : begin
                            HrStr := 'ERROR_LOG_SECTOR_INVALID' ;
                            HrDescr := 'The log service encountered an invalid log sector.';
                          end;
    LongInt($000019C9)  : begin
                            HrStr := 'ERROR_LOG_SECTOR_PARITY_INVALID' ;
                            HrDescr := 'The log service encountered a log sector with invalid block parity.';
                          end;
    LongInt($000019CA)  : begin
                            HrStr := 'ERROR_LOG_SECTOR_REMAPPED' ;
                            HrDescr := 'The log service encountered a remapped log sector.';
                          end;
    LongInt($000019CB)  : begin
                            HrStr := 'ERROR_LOG_BLOCK_INCOMPLETE' ;
                            HrDescr := 'The log service encountered a partial or incomplete log block.';
                          end;
    LongInt($000019CC)  : begin
                            HrStr := 'ERROR_LOG_INVALID_RANGE' ;
                            HrDescr := 'The log service encountered an attempt to access data outside the active log range.';
                          end;
    LongInt($000019CD)  : begin
                            HrStr := 'ERROR_LOG_BLOCKS_EXHAUSTED' ;
                            HrDescr := 'The log service user marshaling buffers are exhausted.';
                          end;
    LongInt($000019CE)  : begin
                            HrStr := 'ERROR_LOG_READ_CONTEXT_INVALID' ;
                            HrDescr := 'The log service encountered an attempt to read from a marshaling area with an invalid read context.';
                          end;
    LongInt($000019CF)  : begin
                            HrStr := 'ERROR_LOG_RESTART_INVALID' ;
                            HrDescr := 'The log service encountered an invalid log restart area.';
                          end;
    LongInt($000019D0)  : begin
                            HrStr := 'ERROR_LOG_BLOCK_VERSION' ;
                            HrDescr := 'The log service encountered an invalid log block version.';
                          end;
    LongInt($000019D1)  : begin
                            HrStr := 'ERROR_LOG_BLOCK_INVALID' ;
                            HrDescr := 'The log service encountered an invalid log block.';
                          end;
    LongInt($000019D2)  : begin
                            HrStr := 'ERROR_LOG_READ_MODE_INVALID' ;
                            HrDescr := 'The log service encountered an attempt to read the log with an invalid read mode.';
                          end;
    LongInt($000019D3)  : begin
                            HrStr := 'ERROR_LOG_NO_RESTART' ;
                            HrDescr := 'The log service encountered a log stream with no restart area.';
                          end;
    LongInt($000019D4)  : begin
                            HrStr := 'ERROR_LOG_METADATA_CORRUPT' ;
                            HrDescr := 'The log service encountered a corrupted metadata file.';
                          end;
    LongInt($000019D5)  : begin
                            HrStr := 'ERROR_LOG_METADATA_INVALID' ;
                            HrDescr := 'The log service encountered a metadata file that could not be created by the log file system.';
                          end;
    LongInt($000019D6)  : begin
                            HrStr := 'ERROR_LOG_METADATA_INCONSISTENT' ;
                            HrDescr := 'The log service encountered a metadata file with inconsistent data.';
                          end;
    LongInt($000019D7)  : begin
                            HrStr := 'ERROR_LOG_RESERVATION_INVALID' ;
                            HrDescr := 'The log service encountered an attempt to erroneous allocate or dispose reservation space.';
                          end;
    LongInt($000019D8)  : begin
                            HrStr := 'ERROR_LOG_CANT_DELETE' ;
                            HrDescr := 'The log service cannot delete a log file or file system container.';
                          end;
    LongInt($000019D9)  : begin
                            HrStr := 'ERROR_LOG_CONTAINER_LIMIT_EXCEEDED' ;
                            HrDescr := 'The log service has reached the maximum allowable containers allocated to a log file.';
                          end;
    LongInt($000019DA)  : begin
                            HrStr := 'ERROR_LOG_START_OF_LOG' ;
                            HrDescr := 'The log service has attempted to read or write backward past the start of the log.';
                          end;
    LongInt($000019DB)  : begin
                            HrStr := 'ERROR_LOG_POLICY_ALREADY_INSTALLED' ;
                            HrDescr := 'The log policy could not be installed because a policy of the same type is already present.';
                          end;
    LongInt($000019DC)  : begin
                            HrStr := 'ERROR_LOG_POLICY_NOT_INSTALLED' ;
                            HrDescr := 'The log policy in question was not installed at the time of the request.';
                          end;
    LongInt($000019DD)  : begin
                            HrStr := 'ERROR_LOG_POLICY_INVALID' ;
                            HrDescr := 'The installed set of policies on the log is invalid.';
                          end;
    LongInt($000019DE)  : begin
                            HrStr := 'ERROR_LOG_POLICY_CONFLICT' ;
                            HrDescr := 'A policy on the log in question prevented the operation from completing.';
                          end;
    LongInt($000019DF)  : begin
                            HrStr := 'ERROR_LOG_PINNED_ARCHIVE_TAIL' ;
                            HrDescr := 'Log space cannot be reclaimed because the log is pinned by the archive tail.';
                          end;
    LongInt($000019E0)  : begin
                            HrStr := 'ERROR_LOG_RECORD_NONEXISTENT' ;
                            HrDescr := 'The log record is not a record in the log file.';
                          end;
    LongInt($000019E1)  : begin
                            HrStr := 'ERROR_LOG_RECORDS_RESERVED_INVALID' ;
                            HrDescr := 'The number of reserved log records or the adjustment of the number of reserved log records is invalid.';
                          end;
    LongInt($000019E2)  : begin
                            HrStr := 'ERROR_LOG_SPACE_RESERVED_INVALID' ;
                            HrDescr := 'The reserved log space or the adjustment of the log space is invalid.';
                          end;
    LongInt($000019E3)  : begin
                            HrStr := 'ERROR_LOG_TAIL_INVALID' ;
                            HrDescr := 'A new or existing archive tail or base of the active log is invalid.';
                          end;
    LongInt($000019E4)  : begin
                            HrStr := 'ERROR_LOG_FULL' ;
                            HrDescr := 'The log space is exhausted.';
                          end;
    LongInt($000019E5)  : begin
                            HrStr := 'ERROR_COULD_NOT_RESIZE_LOG' ;
                            HrDescr := 'The log could not be set to the requested size.';
                          end;
    LongInt($000019E6)  : begin
                            HrStr := 'ERROR_LOG_MULTIPLEXED' ;
                            HrDescr := 'The log is multiplexed; no direct writes to the physical log are allowed.';
                          end;
    LongInt($000019E7)  : begin
                            HrStr := 'ERROR_LOG_DEDICATED' ;
                            HrDescr := 'The operation failed because the log is a dedicated log.';
                          end;
    LongInt($000019E8)  : begin
                            HrStr := 'ERROR_LOG_ARCHIVE_NOT_IN_PROGRESS' ;
                            HrDescr := 'The operation requires an archive context.';
                          end;
    LongInt($000019E9)  : begin
                            HrStr := 'ERROR_LOG_ARCHIVE_IN_PROGRESS' ;
                            HrDescr := 'Log archival is in progress.';
                          end;
    LongInt($000019EA)  : begin
                            HrStr := 'ERROR_LOG_EPHEMERAL' ;
                            HrDescr := 'The operation requires a non-ephemeral log, but the log is ephemeral.';
                          end;
    LongInt($000019EB)  : begin
                            HrStr := 'ERROR_LOG_NOT_ENOUGH_CONTAINERS' ;
                            HrDescr := 'The log must have at least two containers before it can be read from or written to.';
                          end;
    LongInt($000019EC)  : begin
                            HrStr := 'ERROR_LOG_CLIENT_ALREADY_REGISTERED' ;
                            HrDescr := 'A log client has already registered on the stream.';
                          end;
    LongInt($000019ED)  : begin
                            HrStr := 'ERROR_LOG_CLIENT_NOT_REGISTERED' ;
                            HrDescr := 'A log client has not been registered on the stream.';
                          end;
    LongInt($000019EE)  : begin
                            HrStr := 'ERROR_LOG_FULL_HANDLER_IN_PROGRESS' ;
                            HrDescr := 'A request has already been made to handle the log full condition.';
                          end;
    LongInt($000019EF)  : begin
                            HrStr := 'ERROR_LOG_CONTAINER_READ_FAILED' ;
                            HrDescr := 'The log service encountered an error when attempting to read from a log container.';
                          end;
    LongInt($000019F0)  : begin
                            HrStr := 'ERROR_LOG_CONTAINER_WRITE_FAILED' ;
                            HrDescr := 'The log service encountered an error when attempting to write to a log container.';
                          end;
    LongInt($000019F1)  : begin
                            HrStr := 'ERROR_LOG_CONTAINER_OPEN_FAILED' ;
                            HrDescr := 'The log service encountered an error when attempting to open a log container.';
                          end;
    LongInt($000019F2)  : begin
                            HrStr := 'ERROR_LOG_CONTAINER_STATE_INVALID' ;
                            HrDescr := 'The log service encountered an invalid container state when attempting a requested action.';
                          end;
    LongInt($000019F3)  : begin
                            HrStr := 'ERROR_LOG_STATE_INVALID' ;
                            HrDescr := 'The log service is not in the correct state to perform a requested action.';
                          end;
    LongInt($000019F4)  : begin
                            HrStr := 'ERROR_LOG_PINNED' ;
                            HrDescr := 'The log space cannot be reclaimed because the log is pinned.';
                          end;
    LongInt($000019F5)  : begin
                            HrStr := 'ERROR_LOG_METADATA_FLUSH_FAILED' ;
                            HrDescr := 'The log metadata flush failed.';
                          end;
    LongInt($000019F6)  : begin
                            HrStr := 'ERROR_LOG_INCONSISTENT_SECURITY' ;
                            HrDescr := 'Security on the log and its containers is inconsistent.';
                          end;
    LongInt($000019F7)  : begin
                            HrStr := 'ERROR_LOG_APPENDED_FLUSH_FAILED' ;
                            HrDescr := 'Records were appended to the log or reservation changes were made, but the log could not be flushed.';
                          end;
    LongInt($000019F8)  : begin
                            HrStr := 'ERROR_LOG_PINNED_RESERVATION' ;
                            HrDescr := 'The log is pinned due to reservation consuming most of the log space.' +
                                       'Free some reserved records to make space available.';
                          end;
    LongInt($00001A2C)  : begin
                            HrStr := 'ERROR_INVALID_TRANSACTION' ;
                            HrDescr := 'The transaction handle associated with this operation is not valid.';
                          end;
    LongInt($00001A2D)  : begin
                            HrStr := 'ERROR_TRANSACTION_NOT_ACTIVE' ;
                            HrDescr := 'The requested operation was made in the context of a transaction that is no longer active.';
                          end;
    LongInt($00001A2E)  : begin
                            HrStr := 'ERROR_TRANSACTION_REQUEST_NOT_VALID' ;
                            HrDescr := 'The requested operation is not valid on the transaction object in its current state.';
                          end;
    LongInt($00001A2F)  : begin
                            HrStr := 'ERROR_TRANSACTION_NOT_REQUESTED' ;
                            HrDescr := 'The caller has called a response API, but the response is not expected because the transaction manager did not issue the corresponding request to the caller.';
                          end;
    LongInt($00001A30)  : begin
                            HrStr := 'ERROR_TRANSACTION_ALREADY_ABORTED' ;
                            HrDescr := 'It is too late to perform the requested operation because the transaction has already been aborted.';
                          end;
    LongInt($00001A31)  : begin
                            HrStr := 'ERROR_TRANSACTION_ALREADY_COMMITTED' ;
                            HrDescr := 'It is too late to perform the requested operation because the transaction has already been committed.';
                          end;
    LongInt($00001A32)  : begin
                            HrStr := 'ERROR_TM_INITIALIZATION_FAILED' ;
                            HrDescr := 'The transaction manager was unable to be successfully initialized.' +
                                       'Transacted operations are not supported.';
                          end;
    LongInt($00001A33)  : begin
                            HrStr := 'ERROR_RESOURCEMANAGER_READ_ONLY' ;
                            HrDescr := 'The specified resource manager made no changes or updates to the resource under this transaction.';
                          end;
    LongInt($00001A34)  : begin
                            HrStr := 'ERROR_TRANSACTION_NOT_JOINED' ;
                            HrDescr := 'The resource manager has attempted to prepare a transaction that it has not successfully joined.';
                          end;
    LongInt($00001A35)  : begin
                            HrStr := 'ERROR_TRANSACTION_SUPERIOR_EXISTS' ;
                            HrDescr := 'The transaction object already has a superior enlistment, and the caller attempted an operation that would have created a new superior.' +
                                       'Only a single superior enlistment is allowed.';
                          end;
    LongInt($00001A36)  : begin
                            HrStr := 'ERROR_CRM_PROTOCOL_ALREADY_EXISTS' ;
                            HrDescr := 'The resource manager tried to register a protocol that already exists.';
                          end;
    LongInt($00001A37)  : begin
                            HrStr := 'ERROR_TRANSACTION_PROPAGATION_FAILED' ;
                            HrDescr := 'The attempt to propagate the transaction failed.';
                          end;
    LongInt($00001A38)  : begin
                            HrStr := 'ERROR_CRM_PROTOCOL_NOT_FOUND' ;
                            HrDescr := 'The requested propagation protocol was not registered as a CRM.';
                          end;
    LongInt($00001A39)  : begin
                            HrStr := 'ERROR_TRANSACTION_INVALID_MARSHALL_BUFFER' ;
                            HrDescr := 'The buffer passed in to PushTransaction or PullTransaction is not in a valid format.';
                          end;
    LongInt($00001A3A)  : begin
                            HrStr := 'ERROR_CURRENT_TRANSACTION_NOT_VALID' ;
                            HrDescr := 'The current transaction context associated with the thread is not a valid handle to a transaction object.';
                          end;
    LongInt($00001A3B)  : begin
                            HrStr := 'ERROR_TRANSACTION_NOT_FOUND' ;
                            HrDescr := 'The specified transaction object could not be opened because it was not found.';
                          end;
    LongInt($00001A3C)  : begin
                            HrStr := 'ERROR_RESOURCEMANAGER_NOT_FOUND' ;
                            HrDescr := 'The specified resource manager object could not be opened because it was not found.';
                          end;
    LongInt($00001A3D)  : begin
                            HrStr := 'ERROR_ENLISTMENT_NOT_FOUND' ;
                            HrDescr := 'The specified enlistment object could not be opened because it was not found.';
                          end;
    LongInt($00001A3E)  : begin
                            HrStr := 'ERROR_TRANSACTIONMANAGER_NOT_FOUND' ;
                            HrDescr := 'The specified transaction manager object could not be opened because it was not found.';
                          end;
    LongInt($00001A3F)  : begin
                            HrStr := 'ERROR_TRANSACTIONMANAGER_NOT_ONLINE' ;
                            HrDescr := 'The specified resource manager was unable to create an enlistment because its associated transaction manager is not online.';
                          end;
    LongInt($00001A40)  : begin
                            HrStr := 'ERROR_TRANSACTIONMANAGER_RECOVERY_NAME_COLLISION' ;
                            HrDescr := 'The specified transaction manager was unable to create the objects contained in its log file in the ObjectB namespace.' +
                                       'Therefore, the transaction manager was unable to recover.';
                          end;
    LongInt($00001A90)  : begin
                            HrStr := 'ERROR_TRANSACTIONAL_CONFLICT' ;
                            HrDescr := 'The function attempted to use a name that is reserved for use by another transaction.';
                          end;
    LongInt($00001A91)  : begin
                            HrStr := 'ERROR_RM_NOT_ACTIVE' ;
                            HrDescr := 'Transaction support within the specified file system resource manager is not started or was shut down due to an error.';
                          end;
    LongInt($00001A92)  : begin
                            HrStr := 'ERROR_RM_METADATA_CORRUPT' ;
                            HrDescr := 'The metadata of the resource manager has been corrupted.' +
                                       'The resource manager will not function.';
                          end;
    LongInt($00001A93)  : begin
                            HrStr := 'ERROR_DIRECTORY_NOT_RM' ;
                            HrDescr := 'The specified directory does not contain a resource manager.';
                          end;
    LongInt($00001A95)  : begin
                            HrStr := 'ERROR_TRANSACTIONS_UNSUPPORTED_REMOTE' ;
                            HrDescr := 'The remote server or share does not support transacted file operations.';
                          end;
    LongInt($00001A96)  : begin
                            HrStr := 'ERROR_LOG_RESIZE_INVALID_SIZE' ;
                            HrDescr := 'The requested log size is invalid.';
                          end;
    LongInt($00001A97)  : begin
                            HrStr := 'ERROR_OBJECT_NO_LONGER_EXISTS' ;
                            HrDescr := 'The object (file, stream, link) corresponding to the handle has been deleted by a transaction savepoint rollback.';
                          end;
    LongInt($00001A98)  : begin
                            HrStr := 'ERROR_STREAM_MINIVERSION_NOT_FOUND' ;
                            HrDescr := 'The specified file miniversion was not found for this transacted file open.';
                          end;
    LongInt($00001A99)  : begin
                            HrStr := 'ERROR_STREAM_MINIVERSION_NOT_VALID' ;
                            HrDescr := 'The specified file miniversion was found but has been invalidated.' +
                                       'The most likely cause is a transaction savepoint rollback.';
                          end;
    LongInt($00001A9A)  : begin
                            HrStr := 'ERROR_MINIVERSION_INACCESSIBLE_FROM_SPECIFIED_TRANSACTION' ;
                            HrDescr := 'A miniversion can only be opened in the context of the transaction that created it.';
                          end;
    LongInt($00001A9B)  : begin
                            HrStr := 'ERROR_CANT_OPEN_MINIVERSION_WITH_MODIFY_INTENT' ;
                            HrDescr := 'It is not possible to open a miniversion with modify access.';
                          end;
    LongInt($00001A9C)  : begin
                            HrStr := 'ERROR_CANT_CREATE_MORE_STREAM_MINIVERSIONS' ;
                            HrDescr := 'It is not possible to create any more miniversions for this stream.';
                          end;
    LongInt($00001A9E)  : begin
                            HrStr := 'ERROR_REMOTE_FILE_VERSION_MISMATCH' ;
                            HrDescr := 'The remote server sent mismatching version numbers or FID for a file opened with transactions.';
                          end;
    LongInt($00001A9F)  : begin
                            HrStr := 'ERROR_HANDLE_NO_LONGER_VALID' ;
                            HrDescr := 'The handle has been invalidated by a transaction.' +
                                       'The most likely cause is the presence of memory mapping on a file, or an open handle when the transaction ended or rolled back to savepoint.';
                          end;
    LongInt($00001AA0)  : begin
                            HrStr := 'ERROR_NO_TXF_METADATA' ;
                            HrDescr := 'There is no transaction metadata on the file.';
                          end;
    LongInt($00001AA1)  : begin
                            HrStr := 'ERROR_LOG_CORRUPTION_DETECTED' ;
                            HrDescr := 'The log data is corrupt.';
                          end;
    LongInt($00001AA2)  : begin
                            HrStr := 'ERROR_CANT_RECOVER_WITH_HANDLE_OPEN' ;
                            HrDescr := 'The file cannot be recovered because a handle is still open on it.';
                          end;
    LongInt($00001AA3)  : begin
                            HrStr := 'ERROR_RM_DISCONNECTED' ;
                            HrDescr := 'The transaction outcome is unavailable because the resource manager responsible for it is disconnected.';
                          end;
    LongInt($00001AA4)  : begin
                            HrStr := 'ERROR_ENLISTMENT_NOT_SUPERIOR' ;
                            HrDescr := 'The request was rejected because the enlistment in question is not a superior enlistment.';
                          end;
    LongInt($00001AA5)  : begin
                            HrStr := 'ERROR_RECOVERY_NOT_NEEDED' ;
                            HrDescr := 'The transactional resource manager is already consistent.' +
                                       'Recovery is not needed.';
                          end;
    LongInt($00001AA6)  : begin
                            HrStr := 'ERROR_RM_ALREADY_STARTED' ;
                            HrDescr := 'The transactional resource manager has already been started.';
                          end;
    LongInt($00001AA7)  : begin
                            HrStr := 'ERROR_FILE_IDENTITY_NOT_PERSISTENT' ;
                            HrDescr := 'The file cannot be opened in a transaction because its identity depends on the outcome of an unresolved transaction.';
                          end;
    LongInt($00001AA8)  : begin
                            HrStr := 'ERROR_CANT_BREAK_TRANSACTIONAL_DEPENDENCY' ;
                            HrDescr := 'The operation cannot be performed because another transaction is depending on the fact that this property will not change.';
                          end;
    LongInt($00001AA9)  : begin
                            HrStr := 'ERROR_CANT_CROSS_RM_BOUNDARY' ;
                            HrDescr := 'The operation would involve a single file with two transactional resource managers and is therefore not allowed.';
                          end;
    LongInt($00001AAA)  : begin
                            HrStr := 'ERROR_TXF_DIR_NOT_EMPTY' ;
                            HrDescr := 'The $Txf directory must be empty for this operation to succeed.';
                          end;
    LongInt($00001AAB)  : begin
                            HrStr := 'ERROR_INDOUBT_TRANSACTIONS_EXIST' ;
                            HrDescr := 'The operation would leave a transactional resource manager in an inconsistent state and is, therefore, not allowed.';
                          end;
    LongInt($00001AAC)  : begin
                            HrStr := 'ERROR_TM_VOLATILE' ;
                            HrDescr := 'The operation could not be completed because the transaction manager does not have a log.';
                          end;
    LongInt($00001AAD)  : begin
                            HrStr := 'ERROR_ROLLBACK_TIMER_EXPIRED' ;
                            HrDescr := 'A rollback could not be scheduled because a previously scheduled rollback has already been executed or is queued for execution.';
                          end;
    LongInt($00001AAE)  : begin
                            HrStr := 'ERROR_TXF_ATTRIBUTE_CORRUPT' ;
                            HrDescr := 'The transactional metadata attribute on the file or directory is corrupt and unreadable.';
                          end;
    LongInt($00001AAF)  : begin
                            HrStr := 'ERROR_EFS_NOT_ALLOWED_IN_TRANSACTION' ;
                            HrDescr := 'The encryption operation could not be completed because a transaction is active.';
                          end;
    LongInt($00001AB0)  : begin
                            HrStr := 'ERROR_TRANSACTIONAL_OPEN_NOT_ALLOWED' ;
                            HrDescr := 'This object is not allowed to be opened in a transaction.';
                          end;
    LongInt($00001AB1)  : begin
                            HrStr := 'ERROR_LOG_GROWTH_FAILED' ;
                            HrDescr := 'An attempt to create space in the transactional resource manager''s log failed.' +
                                       'The failure status has been recorded in the event log.';
                          end;
    LongInt($00001AB2)  : begin
                            HrStr := 'ERROR_TRANSACTED_MAPPING_UNSUPPORTED_REMOTE' ;
                            HrDescr := 'Memory mapping (creating a mapped section) to a remote file under a transaction is not supported.';
                          end;
    LongInt($00001AB3)  : begin
                            HrStr := 'ERROR_TXF_METADATA_ALREADY_PRESENT' ;
                            HrDescr := 'Transaction metadata is already present on this file and cannot be superseded.';
                          end;
    LongInt($00001AB4)  : begin
                            HrStr := 'ERROR_TRANSACTION_SCOPE_CALLBACKS_NOT_SET' ;
                            HrDescr := 'A transaction scope could not be entered because the scope handler has not been initialized.';
                          end;
    LongInt($00001AB5)  : begin
                            HrStr := 'ERROR_TRANSACTION_REQUIRED_PROMOTION' ;
                            HrDescr := 'Promotion was required to allow the resource manager to enlist, but the transaction was set to disallow it.';
                          end;
    LongInt($00001AB6)  : begin
                            HrStr := 'ERROR_CANNOT_EXECUTE_FILE_IN_TRANSACTION' ;
                            HrDescr := 'This file is open for modification in an unresolved transaction and can be opened for execution only by a transacted reader.';
                          end;
    LongInt($00001AB7)  : begin
                            HrStr := 'ERROR_TRANSACTIONS_NOT_FROZEN' ;
                            HrDescr := 'The request to thaw frozen transactions was ignored because transactions were not previously frozen.';
                          end;
    LongInt($00001AB8)  : begin
                            HrStr := 'ERROR_TRANSACTION_FREEZE_IN_PROGRESS' ;
                            HrDescr := 'Transactions cannot be frozen because a freeze is already in progress.';
                          end;
    LongInt($00001AB9)  : begin
                            HrStr := 'ERROR_NOT_SNAPSHOT_VOLUME' ;
                            HrDescr := 'The target volume is not a snapshot volume.' +
                                       'This operation is only valid on a volume mounted as a snapshot.';
                          end;
    LongInt($00001ABA)  : begin
                            HrStr := 'ERROR_NO_SAVEPOINT_WITH_OPEN_FILES' ;
                            HrDescr := 'The savepoint operation failed because files are open on the transaction.' +
                                       'This is not permitted.';
                          end;
    LongInt($00001ABB)  : begin
                            HrStr := 'ERROR_DATA_LOST_REPAIR' ;
                            HrDescr := 'Windows has discovered corruption in a file, and that file has since been repaired.' +
                                       'Data loss might have occurred.';
                          end;
    LongInt($00001ABC)  : begin
                            HrStr := 'ERROR_SPARSE_NOT_ALLOWED_IN_TRANSACTION' ;
                            HrDescr := 'The sparse operation could not be completed because a transaction is active on the file.';
                          end;
    LongInt($00001ABD)  : begin
                            HrStr := 'ERROR_TM_IDENTITY_MISMATCH' ;
                            HrDescr := 'The call to create a transaction manager object failed because the Tm Identity stored in the logfile does not match the Tm Identity that was passed in as an argument.';
                          end;
    LongInt($00001ABE)  : begin
                            HrStr := 'ERROR_FLOATED_SECTION' ;
                            HrDescr := 'I/O was attempted on a section object that has been floated as a result of a transaction ending.' +
                                       'There is no valid data.';
                          end;
    LongInt($00001ABF)  : begin
                            HrStr := 'ERROR_CANNOT_ACCEPT_TRANSACTED_WORK' ;
                            HrDescr := 'The transactional resource manager cannot currently accept transacted work due to a transient condition, such as low resources.';
                          end;
    LongInt($00001AC0)  : begin
                            HrStr := 'ERROR_CANNOT_ABORT_TRANSACTIONS' ;
                            HrDescr := 'The transactional resource manager had too many transactions outstanding that could not be aborted.' +
                                       'The transactional resource manager has been shut down.';
                          end;
    LongInt($00001B59)  : begin
                            HrStr := 'ERROR_CTX_WINSTATION_NAME_INVALID' ;
                            HrDescr := 'The specified session name is invalid.';
                          end;
    LongInt($00001B5A)  : begin
                            HrStr := 'ERROR_CTX_INVALID_PD' ;
                            HrDescr := 'The specified protocol driver is invalid.';
                          end;
    LongInt($00001B5B)  : begin
                            HrStr := 'ERROR_CTX_PD_NOT_FOUND' ;
                            HrDescr := 'The specified protocol driver was not found in the system path.';
                          end;
    LongInt($00001B5C)  : begin
                            HrStr := 'ERROR_CTX_WD_NOT_FOUND' ;
                            HrDescr := 'The specified terminal connection driver was not found in the system path.';
                          end;
    LongInt($00001B5D)  : begin
                            HrStr := 'ERROR_CTX_CANNOT_MAKE_EVENTLOG_ENTRY' ;
                            HrDescr := 'A registry key for event logging could not be created for this session.';
                          end;
    LongInt($00001B5E)  : begin
                            HrStr := 'ERROR_CTX_SERVICE_NAME_COLLISION' ;
                            HrDescr := 'A service with the same name already exists on the system.';
                          end;
    LongInt($00001B5F)  : begin
                            HrStr := 'ERROR_CTX_CLOSE_PENDING' ;
                            HrDescr := 'A close operation is pending on the session.';
                          end;
    LongInt($00001B60)  : begin
                            HrStr := 'ERROR_CTX_NO_OUTBUF' ;
                            HrDescr := 'There are no free output buffers available.';
                          end;
    LongInt($00001B61)  : begin
                            HrStr := 'ERROR_CTX_MODEM_INF_NOT_FOUND' ;
                            HrDescr := 'The MODEM.INF file was not found.';
                          end;
    LongInt($00001B62)  : begin
                            HrStr := 'ERROR_CTX_INVALID_MODEMNAME' ;
                            HrDescr := 'The modem name was not found in the MODEM.INF file.';
                          end;
    LongInt($00001B63)  : begin
                            HrStr := 'ERROR_CTX_MODEM_RESPONSE_ERROR' ;
                            HrDescr := 'The modem did not accept the command sent to it.' +
                                       'Verify that the configured modem name matches the attached modem.';
                          end;
    LongInt($00001B64)  : begin
                            HrStr := 'ERROR_CTX_MODEM_RESPONSE_TIMEOUT' ;
                            HrDescr := 'The modem did not respond to the command sent to it.' +
                                       'Verify that the modem is properly cabled and turned on.';
                          end;
    LongInt($00001B65)  : begin
                            HrStr := 'ERROR_CTX_MODEM_RESPONSE_NO_CARRIER' ;
                            HrDescr := 'Carrier detect has failed or carrier has been dropped due to disconnect.';
                          end;
    LongInt($00001B66)  : begin
                            HrStr := 'ERROR_CTX_MODEM_RESPONSE_NO_DIALTONE' ;
                            HrDescr := 'Dial tone not detected within the required time.' +
                                       'Verify that the phone cable is properly attached and functional.';
                          end;
    LongInt($00001B67)  : begin
                            HrStr := 'ERROR_CTX_MODEM_RESPONSE_BUSY' ;
                            HrDescr := 'Busy signal detected at remote site on callback.';
                          end;
    LongInt($00001B68)  : begin
                            HrStr := 'ERROR_CTX_MODEM_RESPONSE_VOICE' ;
                            HrDescr := 'Voice detected at remote site on callback.';
                          end;
    LongInt($00001B69)  : begin
                            HrStr := 'ERROR_CTX_TD_ERROR' ;
                            HrDescr := 'Transport driver error.';
                          end;
    LongInt($00001B6E)  : begin
                            HrStr := 'ERROR_CTX_WINSTATION_NOT_FOUND' ;
                            HrDescr := 'The specified session cannot be found.';
                          end;
    LongInt($00001B6F)  : begin
                            HrStr := 'ERROR_CTX_WINSTATION_ALREADY_EXISTS' ;
                            HrDescr := 'The specified session name is already in use.';
                          end;
    LongInt($00001B70)  : begin
                            HrStr := 'ERROR_CTX_WINSTATION_BUSY' ;
                            HrDescr := 'The requested operation cannot be completed because the terminal connection is currently busy processing a connect, disconnect, reset, or delete operation.';
                          end;
    LongInt($00001B71)  : begin
                            HrStr := 'ERROR_CTX_BAD_VIDEO_MODE' ;
                            HrDescr := 'An attempt has been made to connect to a session whose video mode is not supported by the current client.';
                          end;
    LongInt($00001B7B)  : begin
                            HrStr := 'ERROR_CTX_GRAPHICS_INVALID' ;
                            HrDescr := 'The application attempted to enable DOS graphics mode.' +
                                       'DOS graphics mode is not supported.';
                          end;
    LongInt($00001B7D)  : begin
                            HrStr := 'ERROR_CTX_LOGON_DISABLED' ;
                            HrDescr := 'Your interactive logon privilege has been disabled.' +
                                       'Contact your administrator.';
                          end;
    LongInt($00001B7E)  : begin
                            HrStr := 'ERROR_CTX_NOT_CONSOLE' ;
                            HrDescr := 'The requested operation can be performed only on the system console.' +
                                       'This is most often the result of a driver or system DLL requiring direct console access.';
                          end;
    LongInt($00001B80)  : begin
                            HrStr := 'ERROR_CTX_CLIENT_QUERY_TIMEOUT' ;
                            HrDescr := 'The client failed to respond to the server connect message.';
                          end;
    LongInt($00001B81)  : begin
                            HrStr := 'ERROR_CTX_CONSOLE_DISCONNECT' ;
                            HrDescr := 'Disconnecting the console session is not supported.';
                          end;
    LongInt($00001B82)  : begin
                            HrStr := 'ERROR_CTX_CONSOLE_CONNECT' ;
                            HrDescr := 'Reconnecting a disconnected session to the console is not supported.';
                          end;
    LongInt($00001B84)  : begin
                            HrStr := 'ERROR_CTX_SHADOW_DENIED' ;
                            HrDescr := 'The request to control another session remotely was denied.';
                          end;
    LongInt($00001B85)  : begin
                            HrStr := 'ERROR_CTX_WINSTATION_ACCESS_DENIED' ;
                            HrDescr := 'The requested session access is denied.';
                          end;
    LongInt($00001B89)  : begin
                            HrStr := 'ERROR_CTX_INVALID_WD' ;
                            HrDescr := 'The specified terminal connection driver is invalid.';
                          end;
    LongInt($00001B8A)  : begin
                            HrStr := 'ERROR_CTX_SHADOW_INVALID' ;
                            HrDescr := 'The requested session cannot be controlled remotely.' +
                                       'This might be because the session is disconnected or does not currently have a user logged on.';
                          end;
    LongInt($00001B8B)  : begin
                            HrStr := 'ERROR_CTX_SHADOW_DISABLED' ;
                            HrDescr := 'The requested session is not configured to allow remote control.';
                          end;
    LongInt($00001B8C)  : begin
                            HrStr := 'ERROR_CTX_CLIENT_LICENSE_IN_USE' ;
                            HrDescr := 'Your request to connect to this terminal server has been rejected.' +
                                       'Your terminal server client license number is currently being used by another user.' +
                                       'Call your system administrator to obtain a unique license number.';
                          end;
    LongInt($00001B8D)  : begin
                            HrStr := 'ERROR_CTX_CLIENT_LICENSE_NOT_SET' ;
                            HrDescr := 'Your request to connect to this terminal server has been rejected.' +
                                       'Your terminal server client license number has not been entered for this copy of the terminal server client.' +
                                       'Contact your system administrator.';
                          end;
    LongInt($00001B8E)  : begin
                            HrStr := 'ERROR_CTX_LICENSE_NOT_AVAILABLE' ;
                            HrDescr := 'The number of connections to this computer is limited and all connections are in use right now.' +
                                       'Try connecting later or contact your system administrator.';
                          end;
    LongInt($00001B8F)  : begin
                            HrStr := 'ERROR_CTX_LICENSE_CLIENT_INVALID' ;
                            HrDescr := 'The client you are using is not licensed to use this system.' +
                                       'Your logon request is denied.';
                          end;
    LongInt($00001B90)  : begin
                            HrStr := 'ERROR_CTX_LICENSE_EXPIRED' ;
                            HrDescr := 'The system license has expired.' +
                                       'Your logon request is denied.';
                          end;
    LongInt($00001B91)  : begin
                            HrStr := 'ERROR_CTX_SHADOW_NOT_RUNNING' ;
                            HrDescr := 'Remote control could not be terminated because the specified session is not currently being remotely controlled.';
                          end;
    LongInt($00001B92)  : begin
                            HrStr := 'ERROR_CTX_SHADOW_ENDED_BY_MODE_CHANGE' ;
                            HrDescr := 'The remote control of the console was terminated because the display mode was changed.' +
                                       'Changing the display mode in a remote control session is not supported.';
                          end;
    LongInt($00001B93)  : begin
                            HrStr := 'ERROR_ACTIVATION_COUNT_EXCEEDED' ;
                            HrDescr := 'Activation has already been reset the maximum number of times for this installation.' +
                                       'Your activation timer will not be cleared.';
                          end;
    LongInt($00001B94)  : begin
                            HrStr := 'ERROR_CTX_WINSTATIONS_DISABLED' ;
                            HrDescr := 'Remote logons are currently disabled.';
                          end;
    LongInt($00001B95)  : begin
                            HrStr := 'ERROR_CTX_ENCRYPTION_LEVEL_REQUIRED' ;
                            HrDescr := 'You do not have the proper encryption level to access this session.';
                          end;
    LongInt($00001B96)  : begin
                            HrStr := 'ERROR_CTX_SESSION_IN_USE' ;
                            HrDescr := 'The user %s\\%s is currently logged on to this computer.' +
                                       'Only the current user or an administrator can log on to this computer.';
                          end;
    LongInt($00001B97)  : begin
                            HrStr := 'ERROR_CTX_NO_FORCE_LOGOFF' ;
                            HrDescr := 'The user %s\\%s is already logged on to the console of this computer.' +
                                       'You do not have permission to log in at this time.' +
                                       'To resolve this issue, contact %s\\%s and have them log off.';
                          end;
    LongInt($00001B98)  : begin
                            HrStr := 'ERROR_CTX_ACCOUNT_RESTRICTION' ;
                            HrDescr := 'Unable to log you on because of an account restriction.';
                          end;
    LongInt($00001B99)  : begin
                            HrStr := 'ERROR_RDP_PROTOCOL_ERROR' ;
                            HrDescr := 'The RDP component %2 detected an error in the protocol stream and has disconnected the client.';
                          end;
    LongInt($00001B9A)  : begin
                            HrStr := 'ERROR_CTX_CDM_CONNECT' ;
                            HrDescr := 'The Client Drive Mapping Service has connected on terminal connection.';
                          end;
    LongInt($00001B9B)  : begin
                            HrStr := 'ERROR_CTX_CDM_DISCONNECT' ;
                            HrDescr := 'The Client Drive Mapping Service has disconnected on terminal connection.';
                          end;
    LongInt($00001B9C)  : begin
                            HrStr := 'ERROR_CTX_SECURITY_LAYER_ERROR' ;
                            HrDescr := 'The terminal server security layer detected an error in the protocol stream and has disconnected the client.';
                          end;
    LongInt($00001B9D)  : begin
                            HrStr := 'ERROR_TS_INCOMPATIBLE_SESSIONS' ;
                            HrDescr := 'The target session is incompatible with the current session.';
                          end;
    LongInt($00001F41)  : begin
                            HrStr := 'FRS_ERR_INVALID_API_SEQUENCE' ;
                            HrDescr := 'The file replication service API was called incorrectly.';
                          end;
    LongInt($00001F42)  : begin
                            HrStr := 'FRS_ERR_STARTING_SERVICE' ;
                            HrDescr := 'The file replication service cannot be started.';
                          end;
    LongInt($00001F43)  : begin
                            HrStr := 'FRS_ERR_STOPPING_SERVICE' ;
                            HrDescr := 'The file replication service cannot be stopped.';
                          end;
    LongInt($00001F44)  : begin
                            HrStr := 'FRS_ERR_INTERNAL_API' ;
                            HrDescr := 'The file replication service API terminated the request.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F45)  : begin
                            HrStr := 'FRS_ERR_INTERNAL' ;
                            HrDescr := 'The file replication service terminated the request.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F46)  : begin
                            HrStr := 'FRS_ERR_SERVICE_COMM' ;
                            HrDescr := 'The file replication service cannot be contacted.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F47)  : begin
                            HrStr := 'FRS_ERR_INSUFFICIENT_PRIV' ;
                            HrDescr := 'The file replication service cannot satisfy the request because the user has insufficient privileges.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F48)  : begin
                            HrStr := 'FRS_ERR_AUTHENTICATION' ;
                            HrDescr := 'The file replication service cannot satisfy the request because authenticated RPC is not available.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F49)  : begin
                            HrStr := 'FRS_ERR_PARENT_INSUFFICIENT_PRIV' ;
                            HrDescr := 'The file replication service cannot satisfy the request because the user has insufficient privileges on the domain controller.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F4A)  : begin
                            HrStr := 'FRS_ERR_PARENT_AUTHENTICATION' ;
                            HrDescr := 'The file replication service cannot satisfy the request because authenticated RPC is not available on the domain controller.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F4B)  : begin
                            HrStr := 'FRS_ERR_CHILD_TO_PARENT_COMM' ;
                            HrDescr := 'The file replication service cannot communicate with the file replication service on the domain controller.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F4C)  : begin
                            HrStr := 'FRS_ERR_PARENT_TO_CHILD_COMM' ;
                            HrDescr := 'The file replication service on the domain controller cannot communicate with the file replication service on this computer.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F4D)  : begin
                            HrStr := 'FRS_ERR_SYSVOL_POPULATE' ;
                            HrDescr := 'The file replication service cannot populate the system volume because of an internal error.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F4E)  : begin
                            HrStr := 'FRS_ERR_SYSVOL_POPULATE_TIMEOUT' ;
                            HrDescr := 'The file replication service cannot populate the system volume because of an internal time-out.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F4F)  : begin
                            HrStr := 'FRS_ERR_SYSVOL_IS_BUSY' ;
                            HrDescr := 'The file replication service cannot process the request.' +
                                       'The system volume is busy with a previous request.';
                          end;
    LongInt($00001F50)  : begin
                            HrStr := 'FRS_ERR_SYSVOL_DEMOTE' ;
                            HrDescr := 'The file replication service cannot stop replicating the system volume because of an internal error.' +
                                       'The event log might contain more information.';
                          end;
    LongInt($00001F51)  : begin
                            HrStr := 'FRS_ERR_INVALID_SERVICE_PARAMETER' ;
                            HrDescr := 'The file replication service detected an invalid parameter.';
                          end;
    LongInt($00002008)  : begin
                            HrStr := 'ERROR_DS_NOT_INSTALLED' ;
                            HrDescr := 'An error occurred while installing the directory service.' +
                                       'For more information, see the event log.';
                          end;
    LongInt($00002009)  : begin
                            HrStr := 'ERROR_DS_MEMBERSHIP_EVALUATED_LOCALLY' ;
                            HrDescr := 'The directory service evaluated group memberships locally.';
                          end;
    LongInt($0000200A)  : begin
                            HrStr := 'ERROR_DS_NO_ATTRIBUTE_OR_VALUE' ;
                            HrDescr := 'The specified directory service attribute or value does not exist.';
                          end;
    LongInt($0000200B)  : begin
                            HrStr := 'ERROR_DS_INVALID_ATTRIBUTE_SYNTAX' ;
                            HrDescr := 'The attribute syntax specified to the directory service is invalid.';
                          end;
    LongInt($0000200C)  : begin
                            HrStr := 'ERROR_DS_ATTRIBUTE_TYPE_UNDEFINED' ;
                            HrDescr := 'The attribute type specified to the directory service is not defined.';
                          end;
    LongInt($0000200D)  : begin
                            HrStr := 'ERROR_DS_ATTRIBUTE_OR_VALUE_EXISTS' ;
                            HrDescr := 'The specified directory service attribute or value already exists.';
                          end;
    LongInt($0000200E)  : begin
                            HrStr := 'ERROR_DS_BUSY' ;
                            HrDescr := 'The directory service is busy.';
                          end;
    LongInt($0000200F)  : begin
                            HrStr := 'ERROR_DS_UNAVAILABLE' ;
                            HrDescr := 'The directory service is unavailable.';
                          end;
    LongInt($00002010)  : begin
                            HrStr := 'ERROR_DS_NO_RIDS_ALLOCATED' ;
                            HrDescr := 'The directory service was unable to allocate a relative identifier.';
                          end;
    LongInt($00002011)  : begin
                            HrStr := 'ERROR_DS_NO_MORE_RIDS' ;
                            HrDescr := 'The directory service has exhausted the pool of relative identifiers.';
                          end;
    LongInt($00002012)  : begin
                            HrStr := 'ERROR_DS_INCORRECT_ROLE_OWNER' ;
                            HrDescr := 'The requested operation could not be performed because the directory service is not the master for that type of operation.';
                          end;
    LongInt($00002013)  : begin
                            HrStr := 'ERROR_DS_RIDMGR_INIT_ERROR' ;
                            HrDescr := 'The directory service was unable to initialize the subsystem that allocates relative identifiers.';
                          end;
    LongInt($00002014)  : begin
                            HrStr := 'ERROR_DS_OBJ_CLASS_VIOLATION' ;
                            HrDescr := 'The requested operation did not satisfy one or more constraints associated with the class of the object.';
                          end;
    LongInt($00002015)  : begin
                            HrStr := 'ERROR_DS_CANT_ON_NON_LEAF' ;
                            HrDescr := 'The directory service can perform the requested operation only on a leaf object.';
                          end;
    LongInt($00002016)  : begin
                            HrStr := 'ERROR_DS_CANT_ON_RDN' ;
                            HrDescr := 'The directory service cannot perform the requested operation on the relative distinguished name (RDN) attribute of an object.';
                          end;
    LongInt($00002017)  : begin
                            HrStr := 'ERROR_DS_CANT_MOD_OBJ_CLASS' ;
                            HrDescr := 'The directory service detected an attempt to modify the object class of an object.';
                          end;
    LongInt($00002018)  : begin
                            HrStr := 'ERROR_DS_CROSS_DOM_MOVE_ERROR' ;
                            HrDescr := 'The requested cross-domain move operation could not be performed.';
                          end;
    LongInt($00002019)  : begin
                            HrStr := 'ERROR_DS_GC_NOT_AVAILABLE' ;
                            HrDescr := 'Unable to contact the global catalog (GC) server.';
                          end;
    LongInt($0000201A)  : begin
                            HrStr := 'ERROR_SHARED_POLICY' ;
                            HrDescr := 'The policy object is shared and can only be modified at the root.';
                          end;
    LongInt($0000201B)  : begin
                            HrStr := 'ERROR_POLICY_OBJECT_NOT_FOUND' ;
                            HrDescr := 'The policy object does not exist.';
                          end;
    LongInt($0000201C)  : begin
                            HrStr := 'ERROR_POLICY_ONLY_IN_DS' ;
                            HrDescr := 'The requested policy information is only in the directory service.';
                          end;
    LongInt($0000201D)  : begin
                            HrStr := 'ERROR_PROMOTION_ACTIVE' ;
                            HrDescr := 'A domain controller promotion is currently active.';
                          end;
    LongInt($0000201E)  : begin
                            HrStr := 'ERROR_NO_PROMOTION_ACTIVE' ;
                            HrDescr := 'A domain controller promotion is not currently active.';
                          end;
    LongInt($00002020)  : begin
                            HrStr := 'ERROR_DS_OPERATIONS_ERROR' ;
                            HrDescr := 'An operations error occurred.';
                          end;
    LongInt($00002021)  : begin
                            HrStr := 'ERROR_DS_PROTOCOL_ERROR' ;
                            HrDescr := 'A protocol error occurred.';
                          end;
    LongInt($00002022)  : begin
                            HrStr := 'ERROR_DS_TIMELIMIT_EXCEEDED' ;
                            HrDescr := 'The time limit for this request was exceeded.';
                          end;
    LongInt($00002023)  : begin
                            HrStr := 'ERROR_DS_SIZELIMIT_EXCEEDED' ;
                            HrDescr := 'The size limit for this request was exceeded.';
                          end;
    LongInt($00002024)  : begin
                            HrStr := 'ERROR_DS_ADMIN_LIMIT_EXCEEDED' ;
                            HrDescr := 'The administrative limit for this request was exceeded.';
                          end;
    LongInt($00002025)  : begin
                            HrStr := 'ERROR_DS_COMPARE_FALSE' ;
                            HrDescr := 'The compare response was False.';
                          end;
    LongInt($00002026)  : begin
                            HrStr := 'ERROR_DS_COMPARE_TRUE' ;
                            HrDescr := 'The compare response was True.';
                          end;
    LongInt($00002027)  : begin
                            HrStr := 'ERROR_DS_AUTH_METHOD_NOT_SUPPORTED' ;
                            HrDescr := 'The requested authentication method is not supported by the server.';
                          end;
    LongInt($00002028)  : begin
                            HrStr := 'ERROR_DS_STRONG_AUTH_REQUIRED' ;
                            HrDescr := 'A more secure authentication method is required for this server.';
                          end;
    LongInt($00002029)  : begin
                            HrStr := 'ERROR_DS_INAPPROPRIATE_AUTH' ;
                            HrDescr := 'Inappropriate authentication.';
                          end;
    LongInt($0000202A)  : begin
                            HrStr := 'ERROR_DS_AUTH_UNKNOWN' ;
                            HrDescr := 'The authentication mechanism is unknown.';
                          end;
    LongInt($0000202B)  : begin
                            HrStr := 'ERROR_DS_REFERRAL' ;
                            HrDescr := 'A referral was returned from the server.';
                          end;
    LongInt($0000202C)  : begin
                            HrStr := 'ERROR_DS_UNAVAILABLE_CRIT_EXTENSION' ;
                            HrDescr := 'The server does not support the requested critical extension.';
                          end;
    LongInt($0000202D)  : begin
                            HrStr := 'ERROR_DS_CONFIDENTIALITY_REQUIRED' ;
                            HrDescr := 'This request requires a secure connection.';
                          end;
    LongInt($0000202E)  : begin
                            HrStr := 'ERROR_DS_INAPPROPRIATE_MATCHING' ;
                            HrDescr := 'Inappropriate matching.';
                          end;
    LongInt($0000202F)  : begin
                            HrStr := 'ERROR_DS_CONSTRAINT_VIOLATION' ;
                            HrDescr := 'A constraint violation occurred.';
                          end;
    LongInt($00002030)  : begin
                            HrStr := 'ERROR_DS_NO_SUCH_OBJECT' ;
                            HrDescr := 'There is no such object on the server.';
                          end;
    LongInt($00002031)  : begin
                            HrStr := 'ERROR_DS_ALIAS_PROBLEM' ;
                            HrDescr := 'There is an alias problem.';
                          end;
    LongInt($00002032)  : begin
                            HrStr := 'ERROR_DS_INVALID_DN_SYNTAX' ;
                            HrDescr := 'An invalid dn syntax has been specified.';
                          end;
    LongInt($00002033)  : begin
                            HrStr := 'ERROR_DS_IS_LEAF' ;
                            HrDescr := 'The object is a leaf object.';
                          end;
    LongInt($00002034)  : begin
                            HrStr := 'ERROR_DS_ALIAS_DEREF_PROBLEM' ;
                            HrDescr := 'There is an alias dereferencing problem.';
                          end;
    LongInt($00002035)  : begin
                            HrStr := 'ERROR_DS_UNWILLING_TO_PERFORM' ;
                            HrDescr := 'The server is unwilling to process the request.';
                          end;
    LongInt($00002036)  : begin
                            HrStr := 'ERROR_DS_LOOP_DETECT' ;
                            HrDescr := 'A loop has been detected.';
                          end;
    LongInt($00002037)  : begin
                            HrStr := 'ERROR_DS_NAMING_VIOLATION' ;
                            HrDescr := 'There is a naming violation.';
                          end;
    LongInt($00002038)  : begin
                            HrStr := 'ERROR_DS_OBJECT_RESULTS_TOO_LARGE' ;
                            HrDescr := 'The result set is too large.';
                          end;
    LongInt($00002039)  : begin
                            HrStr := 'ERROR_DS_AFFECTS_MULTIPLE_DSAS' ;
                            HrDescr := 'The operation affects multiple DSAs.';
                          end;
    LongInt($0000203A)  : begin
                            HrStr := 'ERROR_DS_SERVER_DOWN' ;
                            HrDescr := 'The server is not operational.';
                          end;
    LongInt($0000203B)  : begin
                            HrStr := 'ERROR_DS_LOCAL_ERROR' ;
                            HrDescr := 'A local error has occurred.';
                          end;
    LongInt($0000203C)  : begin
                            HrStr := 'ERROR_DS_ENCODING_ERROR' ;
                            HrDescr := 'An encoding error has occurred.';
                          end;
    LongInt($0000203D)  : begin
                            HrStr := 'ERROR_DS_DECODING_ERROR' ;
                            HrDescr := 'A decoding error has occurred.';
                          end;
    LongInt($0000203E)  : begin
                            HrStr := 'ERROR_DS_FILTER_UNKNOWN' ;
                            HrDescr := 'The search filter cannot be recognized.';
                          end;
    LongInt($0000203F)  : begin
                            HrStr := 'ERROR_DS_PARAM_ERROR' ;
                            HrDescr := 'One or more parameters are illegal.';
                          end;
    LongInt($00002040)  : begin
                            HrStr := 'ERROR_DS_NOT_SUPPORTED' ;
                            HrDescr := 'The specified method is not supported.';
                          end;
    LongInt($00002041)  : begin
                            HrStr := 'ERROR_DS_NO_RESULTS_RETURNED' ;
                            HrDescr := 'No results were returned.';
                          end;
    LongInt($00002042)  : begin
                            HrStr := 'ERROR_DS_CONTROL_NOT_FOUND' ;
                            HrDescr := 'The specified control is not supported by the server.';
                          end;
    LongInt($00002043)  : begin
                            HrStr := 'ERROR_DS_CLIENT_LOOP' ;
                            HrDescr := 'A referral loop was detected by the client.';
                          end;
    LongInt($00002044)  : begin
                            HrStr := 'ERROR_DS_REFERRAL_LIMIT_EXCEEDED' ;
                            HrDescr := 'The preset referral limit was exceeded.';
                          end;
    LongInt($00002045)  : begin
                            HrStr := 'ERROR_DS_SORT_CONTROL_MISSING' ;
                            HrDescr := 'The search requires a SORT control.';
                          end;
    LongInt($00002046)  : begin
                            HrStr := 'ERROR_DS_OFFSET_RANGE_ERROR' ;
                            HrDescr := 'The search results exceed the offset range specified.';
                          end;
    LongInt($0000206D)  : begin
                            HrStr := 'ERROR_DS_ROOT_MUST_BE_NC' ;
                            HrDescr := 'The root object must be the head of a naming context.' +
                                       'The root object cannot have an instantiated parent.';
                          end;
    LongInt($0000206E)  : begin
                            HrStr := 'ERROR_DS_ADD_REPLICA_INHIBITED' ;
                            HrDescr := 'The add replica operation cannot be performed.' +
                                       'The naming context must be writable to create the replica.';
                          end;
    LongInt($0000206F)  : begin
                            HrStr := 'ERROR_DS_ATT_NOT_DEF_IN_SCHEMA' ;
                            HrDescr := 'A reference to an attribute that is not defined in the schema occurred.';
                          end;
    LongInt($00002070)  : begin
                            HrStr := 'ERROR_DS_MAX_OBJ_SIZE_EXCEEDED' ;
                            HrDescr := 'The maximum size of an object has been exceeded.';
                          end;
    LongInt($00002071)  : begin
                            HrStr := 'ERROR_DS_OBJ_STRING_NAME_EXISTS' ;
                            HrDescr := 'An attempt was made to add an object to the directory with a name that is already in use.';
                          end;
    LongInt($00002072)  : begin
                            HrStr := 'ERROR_DS_NO_RDN_DEFINED_IN_SCHEMA' ;
                            HrDescr := 'An attempt was made to add an object of a class that does not have an RDN defined in the schema.';
                          end;
    LongInt($00002073)  : begin
                            HrStr := 'ERROR_DS_RDN_DOESNT_MATCH_SCHEMA' ;
                            HrDescr := 'An attempt was made to add an object using an RDN that is not the RDN defined in the schema.';
                          end;
    LongInt($00002074)  : begin
                            HrStr := 'ERROR_DS_NO_REQUESTED_ATTS_FOUND' ;
                            HrDescr := 'None of the requested attributes were found on the objects.';
                          end;
    LongInt($00002075)  : begin
                            HrStr := 'ERROR_DS_USER_BUFFER_TO_SMALL' ;
                            HrDescr := 'The user buffer is too small.';
                          end;
    LongInt($00002076)  : begin
                            HrStr := 'ERROR_DS_ATT_IS_NOT_ON_OBJ' ;
                            HrDescr := 'The attribute specified in the operation is not present on the object.';
                          end;
    LongInt($00002077)  : begin
                            HrStr := 'ERROR_DS_ILLEGAL_MOD_OPERATION' ;
                            HrDescr := 'Illegal modify operation.' +
                                       'Some aspect of the modification is not permitted.';
                          end;
    LongInt($00002078)  : begin
                            HrStr := 'ERROR_DS_OBJ_TOO_LARGE' ;
                            HrDescr := 'The specified object is too large.';
                          end;
    LongInt($00002079)  : begin
                            HrStr := 'ERROR_DS_BAD_INSTANCE_TYPE' ;
                            HrDescr := 'The specified instance type is not valid.';
                          end;
    LongInt($0000207A)  : begin
                            HrStr := 'ERROR_DS_MASTERDSA_REQUIRED' ;
                            HrDescr := 'The operation must be performed at a master DSA.';
                          end;
    LongInt($0000207B)  : begin
                            HrStr := 'ERROR_DS_OBJECT_CLASS_REQUIRED' ;
                            HrDescr := 'The object class attribute must be specified.';
                          end;
    LongInt($0000207C)  : begin
                            HrStr := 'ERROR_DS_MISSING_REQUIRED_ATT' ;
                            HrDescr := 'A required attribute is missing.';
                          end;
    LongInt($0000207D)  : begin
                            HrStr := 'ERROR_DS_ATT_NOT_DEF_FOR_CLASS' ;
                            HrDescr := 'An attempt was made to modify an object to include an attribute that is not legal for its class.';
                          end;
    LongInt($0000207E)  : begin
                            HrStr := 'ERROR_DS_ATT_ALREADY_EXISTS' ;
                            HrDescr := 'The specified attribute is already present on the object.';
                          end;
    LongInt($00002080)  : begin
                            HrStr := 'ERROR_DS_CANT_ADD_ATT_VALUES' ;
                            HrDescr := 'The specified attribute is not present, or has no values.';
                          end;
    LongInt($00002081)  : begin
                            HrStr := 'ERROR_DS_SINGLE_VALUE_CONSTRAINT' ;
                            HrDescr := 'Multiple values were specified for an attribute that can have only one value.';
                          end;
    LongInt($00002082)  : begin
                            HrStr := 'ERROR_DS_RANGE_CONSTRAINT' ;
                            HrDescr := 'A value for the attribute was not in the acceptable range of values.';
                          end;
    LongInt($00002083)  : begin
                            HrStr := 'ERROR_DS_ATT_VAL_ALREADY_EXISTS' ;
                            HrDescr := 'The specified value already exists.';
                          end;
    LongInt($00002084)  : begin
                            HrStr := 'ERROR_DS_CANT_REM_MISSING_ATT' ;
                            HrDescr := 'The attribute cannot be removed because it is not present on the object.';
                          end;
    LongInt($00002085)  : begin
                            HrStr := 'ERROR_DS_CANT_REM_MISSING_ATT_VAL' ;
                            HrDescr := 'The attribute value cannot be removed because it is not present on the object.';
                          end;
    LongInt($00002086)  : begin
                            HrStr := 'ERROR_DS_ROOT_CANT_BE_SUBREF' ;
                            HrDescr := 'The specified root object cannot be a subreference.';
                          end;
    LongInt($00002087)  : begin
                            HrStr := 'ERROR_DS_NO_CHAINING' ;
                            HrDescr := 'Chaining is not permitted.';
                          end;
    LongInt($00002088)  : begin
                            HrStr := 'ERROR_DS_NO_CHAINED_EVAL' ;
                            HrDescr := 'Chained evaluation is not permitted.';
                          end;
    LongInt($00002089)  : begin
                            HrStr := 'ERROR_DS_NO_PARENT_OBJECT' ;
                            HrDescr := 'The operation could not be performed because the object''s parent is either uninstantiated or deleted.';
                          end;
    LongInt($0000208A)  : begin
                            HrStr := 'ERROR_DS_PARENT_IS_AN_ALIAS' ;
                            HrDescr := 'Having a parent that is an alias is not permitted.' +
                                       'Aliases are leaf objects.';
                          end;
    LongInt($0000208B)  : begin
                            HrStr := 'ERROR_DS_CANT_MIX_MASTER_AND_REPS' ;
                            HrDescr := 'The object and parent must be of the same type, either both masters or both replicas.';
                          end;
    LongInt($0000208C)  : begin
                            HrStr := 'ERROR_DS_CHILDREN_EXIST' ;
                            HrDescr := 'The operation cannot be performed because child objects exist.' +
                                       'This operation can only be performed on a leaf object.';
                          end;
    LongInt($0000208D)  : begin
                            HrStr := 'ERROR_DS_OBJ_NOT_FOUND' ;
                            HrDescr := 'Directory object not found.';
                          end;
    LongInt($0000208E)  : begin
                            HrStr := 'ERROR_DS_ALIASED_OBJ_MISSING' ;
                            HrDescr := 'The aliased object is missing.';
                          end;
    LongInt($0000208F)  : begin
                            HrStr := 'ERROR_DS_BAD_NAME_SYNTAX' ;
                            HrDescr := 'The object name has bad syntax.';
                          end;
    LongInt($00002090)  : begin
                            HrStr := 'ERROR_DS_ALIAS_POINTS_TO_ALIAS' ;
                            HrDescr := 'An alias is not permitted to refer to another alias.';
                          end;
    LongInt($00002091)  : begin
                            HrStr := 'ERROR_DS_CANT_DEREF_ALIAS' ;
                            HrDescr := 'The alias cannot be dereferenced.';
                          end;
    LongInt($00002092)  : begin
                            HrStr := 'ERROR_DS_OUT_OF_SCOPE' ;
                            HrDescr := 'The operation is out of scope.';
                          end;
    LongInt($00002093)  : begin
                            HrStr := 'ERROR_DS_OBJECT_BEING_REMOVED' ;
                            HrDescr := 'The operation cannot continue because the object is in the process of being removed.';
                          end;
    LongInt($00002094)  : begin
                            HrStr := 'ERROR_DS_CANT_DELETE_DSA_OBJ' ;
                            HrDescr := 'The DSA object cannot be deleted.';
                          end;
    LongInt($00002095)  : begin
                            HrStr := 'ERROR_DS_GENERIC_ERROR' ;
                            HrDescr := 'A directory service error has occurred.';
                          end;
    LongInt($00002096)  : begin
                            HrStr := 'ERROR_DS_DSA_MUST_BE_INT_MASTER' ;
                            HrDescr := 'The operation can only be performed on an internal master DSA object.';
                          end;
    LongInt($00002097)  : begin
                            HrStr := 'ERROR_DS_CLASS_NOT_DSA' ;
                            HrDescr := 'The object must be of class DSA.';
                          end;
    LongInt($00002098)  : begin
                            HrStr := 'ERROR_DS_INSUFF_ACCESS_RIGHTS' ;
                            HrDescr := 'Insufficient access rights to perform the operation.';
                          end;
    LongInt($00002099)  : begin
                            HrStr := 'ERROR_DS_ILLEGAL_SUPERIOR' ;
                            HrDescr := 'The object cannot be added because the parent is not on the list of possible superiors.';
                          end;
    LongInt($0000209A)  : begin
                            HrStr := 'ERROR_DS_ATTRIBUTE_OWNED_BY_SAM' ;
                            HrDescr := 'Access to the attribute is not permitted because the attribute is owned by the SAM.';
                          end;
    LongInt($0000209B)  : begin
                            HrStr := 'ERROR_DS_NAME_TOO_MANY_PARTS' ;
                            HrDescr := 'The name has too many parts.';
                          end;
    LongInt($0000209C)  : begin
                            HrStr := 'ERROR_DS_NAME_TOO_LONG' ;
                            HrDescr := 'The name is too long.';
                          end;
    LongInt($0000209D)  : begin
                            HrStr := 'ERROR_DS_NAME_VALUE_TOO_LONG' ;
                            HrDescr := 'The name value is too long.';
                          end;
    LongInt($0000209E)  : begin
                            HrStr := 'ERROR_DS_NAME_UNPARSEABLE' ;
                            HrDescr := 'The directory service encountered an error parsing a name.';
                          end;
    LongInt($0000209F)  : begin
                            HrStr := 'ERROR_DS_NAME_TYPE_UNKNOWN' ;
                            HrDescr := 'The directory service cannot get the attribute type for a name.';
                          end;
    LongInt($000020A0)  : begin
                            HrStr := 'ERROR_DS_NOT_AN_OBJECT' ;
                            HrDescr := 'The name does not identify an object; the name identifies a phantom.';
                          end;
    LongInt($000020A1)  : begin
                            HrStr := 'ERROR_DS_SEC_DESC_TOO_SHORT' ;
                            HrDescr := 'The security descriptor is too short.';
                          end;
    LongInt($000020A2)  : begin
                            HrStr := 'ERROR_DS_SEC_DESC_INVALID' ;
                            HrDescr := 'The security descriptor is invalid.';
                          end;
    LongInt($000020A3)  : begin
                            HrStr := 'ERROR_DS_NO_DELETED_NAME' ;
                            HrDescr := 'Failed to create name for deleted object.';
                          end;
    LongInt($000020A4)  : begin
                            HrStr := 'ERROR_DS_SUBREF_MUST_HAVE_PARENT' ;
                            HrDescr := 'The parent of a new subreference must exist.';
                          end;
    LongInt($000020A5)  : begin
                            HrStr := 'ERROR_DS_NCNAME_MUST_BE_NC' ;
                            HrDescr := 'The object must be a naming context.';
                          end;
    LongInt($000020A6)  : begin
                            HrStr := 'ERROR_DS_CANT_ADD_SYSTEM_ONLY' ;
                            HrDescr := 'It is not permitted to add an attribute that is owned by the system.';
                          end;
    LongInt($000020A7)  : begin
                            HrStr := 'ERROR_DS_CLASS_MUST_BE_CONCRETE' ;
                            HrDescr := 'The class of the object must be structural; you cannot instantiate an abstract class.';
                          end;
    LongInt($000020A8)  : begin
                            HrStr := 'ERROR_DS_INVALID_DMD' ;
                            HrDescr := 'The schema object could not be found.';
                          end;
    LongInt($000020A9)  : begin
                            HrStr := 'ERROR_DS_OBJ_GUID_EXISTS' ;
                            HrDescr := 'A local object with this GUID (dead or alive) already exists.';
                          end;
    LongInt($000020AA)  : begin
                            HrStr := 'ERROR_DS_NOT_ON_BACKLINK' ;
                            HrDescr := 'The operation cannot be performed on a back link.';
                          end;
    LongInt($000020AB)  : begin
                            HrStr := 'ERROR_DS_NO_CROSSREF_FOR_NC' ;
                            HrDescr := 'The cross-reference for the specified naming context could not be found.';
                          end;
    LongInt($000020AC)  : begin
                            HrStr := 'ERROR_DS_SHUTTING_DOWN' ;
                            HrDescr := 'The operation could not be performed because the directory service is shutting down.';
                          end;
    LongInt($000020AD)  : begin
                            HrStr := 'ERROR_DS_UNKNOWN_OPERATION' ;
                            HrDescr := 'The directory service request is invalid.';
                          end;
    LongInt($000020AE)  : begin
                            HrStr := 'ERROR_DS_INVALID_ROLE_OWNER' ;
                            HrDescr := 'The role owner attribute could not be read.';
                          end;
    LongInt($000020AF)  : begin
                            HrStr := 'ERROR_DS_COULDNT_CONTACT_FSMO' ;
                            HrDescr := 'The requested Flexible Single Master Operations (FSMO) operation failed.' +
                                       'The current FSMO holder could not be contacted.';
                          end;
    LongInt($000020B0)  : begin
                            HrStr := 'ERROR_DS_CROSS_NC_DN_RENAME' ;
                            HrDescr := 'Modification of a distinguished name across a naming context is not permitted.';
                          end;
    LongInt($000020B1)  : begin
                            HrStr := 'ERROR_DS_CANT_MOD_SYSTEM_ONLY' ;
                            HrDescr := 'The attribute cannot be modified because it is owned by the system.';
                          end;
    LongInt($000020B2)  : begin
                            HrStr := 'ERROR_DS_REPLICATOR_ONLY' ;
                            HrDescr := 'Only the replicator can perform this function.';
                          end;
    LongInt($000020B3)  : begin
                            HrStr := 'ERROR_DS_OBJ_CLASS_NOT_DEFINED' ;
                            HrDescr := 'The specified class is not defined.';
                          end;
    LongInt($000020B4)  : begin
                            HrStr := 'ERROR_DS_OBJ_CLASS_NOT_SUBCLASS' ;
                            HrDescr := 'The specified class is not a subclass.';
                          end;
    LongInt($000020B5)  : begin
                            HrStr := 'ERROR_DS_NAME_REFERENCE_INVALID' ;
                            HrDescr := 'The name reference is invalid.';
                          end;
    LongInt($000020B6)  : begin
                            HrStr := 'ERROR_DS_CROSS_REF_EXISTS' ;
                            HrDescr := 'A cross-reference already exists.';
                          end;
    LongInt($000020B7)  : begin
                            HrStr := 'ERROR_DS_CANT_DEL_MASTER_CROSSREF' ;
                            HrDescr := 'It is not permitted to delete a master cross-reference.';
                          end;
    LongInt($000020B8)  : begin
                            HrStr := 'ERROR_DS_SUBTREE_NOTIFY_NOT_NC_HEAD' ;
                            HrDescr := 'Subtree notifications are only supported on naming context (NC) heads.';
                          end;
    LongInt($000020B9)  : begin
                            HrStr := 'ERROR_DS_NOTIFY_FILTER_TOO_COMPLEX' ;
                            HrDescr := 'Notification filter is too complex.';
                          end;
    LongInt($000020BA)  : begin
                            HrStr := 'ERROR_DS_DUP_RDN' ;
                            HrDescr := 'Schema update failed: Duplicate RDN.';
                          end;
    LongInt($000020BB)  : begin
                            HrStr := 'ERROR_DS_DUP_OID' ;
                            HrDescr := 'Schema update failed: Duplicate OID.';
                          end;
    LongInt($000020BC)  : begin
                            HrStr := 'ERROR_DS_DUP_MAPI_ID' ;
                            HrDescr := 'Schema update failed: Duplicate Message Application Programming Interface (MAPI) identifier.';
                          end;
    LongInt($000020BD)  : begin
                            HrStr := 'ERROR_DS_DUP_SCHEMA_ID_GUID' ;
                            HrDescr := 'Schema update failed: Duplicate schema ID GUID.';
                          end;
    LongInt($000020BE)  : begin
                            HrStr := 'ERROR_DS_DUP_LDAP_DISPLAY_NAME' ;
                            HrDescr := 'Schema update failed: Duplicate LDAP display name.';
                          end;
    LongInt($000020BF)  : begin
                            HrStr := 'ERROR_DS_SEMANTIC_ATT_TEST' ;
                            HrDescr := 'Schema update failed: Range-Lower less than Range-Upper.';
                          end;
    LongInt($000020C0)  : begin
                            HrStr := 'ERROR_DS_SYNTAX_MISMATCH' ;
                            HrDescr := 'Schema update failed: Syntax mismatch.';
                          end;
    LongInt($000020C1)  : begin
                            HrStr := 'ERROR_DS_EXISTS_IN_MUST_HAVE' ;
                            HrDescr := 'Schema deletion failed: Attribute is used in the Must-Contain list.';
                          end;
    LongInt($000020C2)  : begin
                            HrStr := 'ERROR_DS_EXISTS_IN_MAY_HAVE' ;
                            HrDescr := 'Schema deletion failed: Attribute is used in the May-Contain list.';
                          end;
    LongInt($000020C3)  : begin
                            HrStr := 'ERROR_DS_NONEXISTENT_MAY_HAVE' ;
                            HrDescr := 'Schema update failed: Attribute in May-Contain list does not exist.';
                          end;
    LongInt($000020C4)  : begin
                            HrStr := 'ERROR_DS_NONEXISTENT_MUST_HAVE' ;
                            HrDescr := 'Schema update failed: Attribute in the Must-Contain list does not exist.';
                          end;
    LongInt($000020C5)  : begin
                            HrStr := 'ERROR_DS_AUX_CLS_TEST_FAIL' ;
                            HrDescr := 'Schema update failed: Class in the Aux Class list does not exist or is not an auxiliary class.';
                          end;
    LongInt($000020C6)  : begin
                            HrStr := 'ERROR_DS_NONEXISTENT_POSS_SUP' ;
                            HrDescr := 'Schema update failed: Class in the Poss-Superiors list does not exist.';
                          end;
    LongInt($000020C7)  : begin
                            HrStr := 'ERROR_DS_SUB_CLS_TEST_FAIL' ;
                            HrDescr := 'Schema update failed: Class in the subclass of the list does not exist or does not satisfy hierarchy rules.';
                          end;
    LongInt($000020C8)  : begin
                            HrStr := 'ERROR_DS_BAD_RDN_ATT_ID_SYNTAX' ;
                            HrDescr := 'Schema update failed: Rdn-Att-Id has wrong syntax.';
                          end;
    LongInt($000020C9)  : begin
                            HrStr := 'ERROR_DS_EXISTS_IN_AUX_CLS' ;
                            HrDescr := 'Schema deletion failed: Class is used as an auxiliary class.';
                          end;
    LongInt($000020CA)  : begin
                            HrStr := 'ERROR_DS_EXISTS_IN_SUB_CLS' ;
                            HrDescr := 'Schema deletion failed: Class is used as a subclass.';
                          end;
    LongInt($000020CB)  : begin
                            HrStr := 'ERROR_DS_EXISTS_IN_POSS_SUP' ;
                            HrDescr := 'Schema deletion failed: Class is used as a Poss-Superior.';
                          end;
    LongInt($000020CC)  : begin
                            HrStr := 'ERROR_DS_RECALCSCHEMA_FAILED' ;
                            HrDescr := 'Schema update failed in recalculating validation cache.';
                          end;
    LongInt($000020CD)  : begin
                            HrStr := 'ERROR_DS_TREE_DELETE_NOT_FINISHED' ;
                            HrDescr := 'The tree deletion is not finished.' +
                                       'The request must be made again to continue deleting the tree.';
                          end;
    LongInt($000020CE)  : begin
                            HrStr := 'ERROR_DS_CANT_DELETE' ;
                            HrDescr := 'The requested delete operation could not be performed.';
                          end;
    LongInt($000020CF)  : begin
                            HrStr := 'ERROR_DS_ATT_SCHEMA_REQ_ID' ;
                            HrDescr := 'Cannot read the governs class identifier for the schema record.';
                          end;
    LongInt($000020D0)  : begin
                            HrStr := 'ERROR_DS_BAD_ATT_SCHEMA_SYNTAX' ;
                            HrDescr := 'The attribute schema has bad syntax.';
                          end;
    LongInt($000020D1)  : begin
                            HrStr := 'ERROR_DS_CANT_CACHE_ATT' ;
                            HrDescr := 'The attribute could not be cached.';
                          end;
    LongInt($000020D2)  : begin
                            HrStr := 'ERROR_DS_CANT_CACHE_CLASS' ;
                            HrDescr := 'The class could not be cached.';
                          end;
    LongInt($000020D3)  : begin
                            HrStr := 'ERROR_DS_CANT_REMOVE_ATT_CACHE' ;
                            HrDescr := 'The attribute could not be removed from the cache.';
                          end;
    LongInt($000020D4)  : begin
                            HrStr := 'ERROR_DS_CANT_REMOVE_CLASS_CACHE' ;
                            HrDescr := 'The class could not be removed from the cache.';
                          end;
    LongInt($000020D5)  : begin
                            HrStr := 'ERROR_DS_CANT_RETRIEVE_DN' ;
                            HrDescr := 'The distinguished name attribute could not be read.';
                          end;
    LongInt($000020D6)  : begin
                            HrStr := 'ERROR_DS_MISSING_SUPREF' ;
                            HrDescr := 'No superior reference has been configured for the directory service.' +
                                       'The directory service is, therefore, unable to issue referrals to objects outside this forest.';
                          end;
    LongInt($000020D7)  : begin
                            HrStr := 'ERROR_DS_CANT_RETRIEVE_INSTANCE' ;
                            HrDescr := 'The instance type attribute could not be retrieved.';
                          end;
    LongInt($000020D8)  : begin
                            HrStr := 'ERROR_DS_CODE_INCONSISTENCY' ;
                            HrDescr := 'An internal error has occurred.';
                          end;
    LongInt($000020D9)  : begin
                            HrStr := 'ERROR_DS_DATABASE_ERROR' ;
                            HrDescr := 'A database error has occurred.';
                          end;
    LongInt($000020DA)  : begin
                            HrStr := 'ERROR_DS_GOVERNSID_MISSING' ;
                            HrDescr := 'The governsID attribute is missing.';
                          end;
    LongInt($000020DB)  : begin
                            HrStr := 'ERROR_DS_MISSING_EXPECTED_ATT' ;
                            HrDescr := 'An expected attribute is missing.';
                          end;
    LongInt($000020DC)  : begin
                            HrStr := 'ERROR_DS_NCNAME_MISSING_CR_REF' ;
                            HrDescr := 'The specified naming context is missing a cross-reference.';
                          end;
    LongInt($000020DD)  : begin
                            HrStr := 'ERROR_DS_SECURITY_CHECKING_ERROR' ;
                            HrDescr := 'A security checking error has occurred.';
                          end;
    LongInt($000020DE)  : begin
                            HrStr := 'ERROR_DS_SCHEMA_NOT_LOADED' ;
                            HrDescr := 'The schema is not loaded.';
                          end;
    LongInt($000020DF)  : begin
                            HrStr := 'ERROR_DS_SCHEMA_ALLOC_FAILED' ;
                            HrDescr := 'Schema allocation failed.' +
                                       'Check if the machine is running low on memory.';
                          end;
    LongInt($000020E0)  : begin
                            HrStr := 'ERROR_DS_ATT_SCHEMA_REQ_SYNTAX' ;
                            HrDescr := 'Failed to obtain the required syntax for the attribute schema.';
                          end;
    LongInt($000020E1)  : begin
                            HrStr := 'ERROR_DS_GCVERIFY_ERROR' ;
                            HrDescr := 'The GC verification failed.' +
                                       'The GC is not available or does not support the operation.' +
                                       'Some part of the directory is currently not available.';
                          end;
    LongInt($000020E2)  : begin
                            HrStr := 'ERROR_DS_DRA_SCHEMA_MISMATCH' ;
                            HrDescr := 'The replication operation failed because of a schema mismatch between the servers involved.';
                          end;
    LongInt($000020E3)  : begin
                            HrStr := 'ERROR_DS_CANT_FIND_DSA_OBJ' ;
                            HrDescr := 'The DSA object could not be found.';
                          end;
    LongInt($000020E4)  : begin
                            HrStr := 'ERROR_DS_CANT_FIND_EXPECTED_NC' ;
                            HrDescr := 'The naming context could not be found.';
                          end;
    LongInt($000020E5)  : begin
                            HrStr := 'ERROR_DS_CANT_FIND_NC_IN_CACHE' ;
                            HrDescr := 'The naming context could not be found in the cache.';
                          end;
    LongInt($000020E6)  : begin
                            HrStr := 'ERROR_DS_CANT_RETRIEVE_CHILD' ;
                            HrDescr := 'The child object could not be retrieved.';
                          end;
    LongInt($000020E7)  : begin
                            HrStr := 'ERROR_DS_SECURITY_ILLEGAL_MODIFY' ;
                            HrDescr := 'The modification was not permitted for security reasons.';
                          end;
    LongInt($000020E8)  : begin
                            HrStr := 'ERROR_DS_CANT_REPLACE_HIDDEN_REC' ;
                            HrDescr := 'The operation cannot replace the hidden record.';
                          end;
    LongInt($000020E9)  : begin
                            HrStr := 'ERROR_DS_BAD_HIERARCHY_FILE' ;
                            HrDescr := 'The hierarchy file is invalid.';
                          end;
    LongInt($000020EA)  : begin
                            HrStr := 'ERROR_DS_BUILD_HIERARCHY_TABLE_FAILED' ;
                            HrDescr := 'The attempt to build the hierarchy table failed.';
                          end;
    LongInt($000020EB)  : begin
                            HrStr := 'ERROR_DS_CONFIG_PARAM_MISSING' ;
                            HrDescr := 'The directory configuration parameter is missing from the registry.';
                          end;
    LongInt($000020EC)  : begin
                            HrStr := 'ERROR_DS_COUNTING_AB_INDICES_FAILED' ;
                            HrDescr := 'The attempt to count the address book indices failed.';
                          end;
    LongInt($000020ED)  : begin
                            HrStr := 'ERROR_DS_HIERARCHY_TABLE_MALLOC_FAILED' ;
                            HrDescr := 'The allocation of the hierarchy table failed.';
                          end;
    LongInt($000020EE)  : begin
                            HrStr := 'ERROR_DS_INTERNAL_FAILURE' ;
                            HrDescr := 'The directory service encountered an internal failure.';
                          end;
    LongInt($000020EF)  : begin
                            HrStr := 'ERROR_DS_UNKNOWN_ERROR' ;
                            HrDescr := 'The directory service encountered an unknown failure.';
                          end;
    LongInt($000020F0)  : begin
                            HrStr := 'ERROR_DS_ROOT_REQUIRES_CLASS_TOP' ;
                            HrDescr := 'A root object requires a class of "top".';
                          end;
    LongInt($000020F1)  : begin
                            HrStr := 'ERROR_DS_REFUSING_FSMO_ROLES' ;
                            HrDescr := 'This directory server is shutting down, and cannot take ownership of new floating single-master operation roles.';
                          end;
    LongInt($000020F2)  : begin
                            HrStr := 'ERROR_DS_MISSING_FSMO_SETTINGS' ;
                            HrDescr := 'The directory service is missing mandatory configuration information and is unable to determine the ownership of floating single-master operation roles.';
                          end;
    LongInt($000020F3)  : begin
                            HrStr := 'ERROR_DS_UNABLE_TO_SURRENDER_ROLES' ;
                            HrDescr := 'The directory service was unable to transfer ownership of one or more floating single-master operation roles to other servers.';
                          end;
    LongInt($000020F4)  : begin
                            HrStr := 'ERROR_DS_DRA_GENERIC' ;
                            HrDescr := 'The replication operation failed.';
                          end;
    LongInt($000020F5)  : begin
                            HrStr := 'ERROR_DS_DRA_INVALID_PARAMETER' ;
                            HrDescr := 'An invalid parameter was specified for this replication operation.';
                          end;
    LongInt($000020F6)  : begin
                            HrStr := 'ERROR_DS_DRA_BUSY' ;
                            HrDescr := 'The directory service is too busy to complete the replication operation at this time.';
                          end;
    LongInt($000020F7)  : begin
                            HrStr := 'ERROR_DS_DRA_BAD_DN' ;
                            HrDescr := 'The DN specified for this replication operation is invalid.';
                          end;
    LongInt($000020F8)  : begin
                            HrStr := 'ERROR_DS_DRA_BAD_NC' ;
                            HrDescr := 'The naming context specified for this replication operation is invalid.';
                          end;
    LongInt($000020F9)  : begin
                            HrStr := 'ERROR_DS_DRA_DN_EXISTS' ;
                            HrDescr := 'The DN specified for this replication operation already exists.';
                          end;
    LongInt($000020FA)  : begin
                            HrStr := 'ERROR_DS_DRA_INTERNAL_ERROR' ;
                            HrDescr := 'The replication system encountered an internal error.';
                          end;
    LongInt($000020FB)  : begin
                            HrStr := 'ERROR_DS_DRA_INCONSISTENT_DIT' ;
                            HrDescr := 'The replication operation encountered a database inconsistency.';
                          end;
    LongInt($000020FC)  : begin
                            HrStr := 'ERROR_DS_DRA_CONNECTION_FAILED' ;
                            HrDescr := 'The server specified for this replication operation could not be contacted.';
                          end;
    LongInt($000020FD)  : begin
                            HrStr := 'ERROR_DS_DRA_BAD_INSTANCE_TYPE' ;
                            HrDescr := 'The replication operation encountered an object with an invalid instance type.';
                          end;
    LongInt($000020FE)  : begin
                            HrStr := 'ERROR_DS_DRA_OUT_OF_MEM' ;
                            HrDescr := 'The replication operation failed to allocate memory.';
                          end;
    LongInt($000020FF)  : begin
                            HrStr := 'ERROR_DS_DRA_MAIL_PROBLEM' ;
                            HrDescr := 'The replication operation encountered an error with the mail system.';
                          end;
    LongInt($00002100)  : begin
                            HrStr := 'ERROR_DS_DRA_REF_ALREADY_EXISTS' ;
                            HrDescr := 'The replication reference information for the target server already exists.';
                          end;
    LongInt($00002101)  : begin
                            HrStr := 'ERROR_DS_DRA_REF_NOT_FOUND' ;
                            HrDescr := 'The replication reference information for the target server does not exist.';
                          end;
    LongInt($00002102)  : begin
                            HrStr := 'ERROR_DS_DRA_OBJ_IS_REP_SOURCE' ;
                            HrDescr := 'The naming context cannot be removed because it is replicated to another server.';
                          end;
    LongInt($00002103)  : begin
                            HrStr := 'ERROR_DS_DRA_DB_ERROR' ;
                            HrDescr := 'The replication operation encountered a database error.';
                          end;
    LongInt($00002104)  : begin
                            HrStr := 'ERROR_DS_DRA_NO_REPLICA' ;
                            HrDescr := 'The naming context is in the process of being removed or is not replicated from the specified server.';
                          end;
    LongInt($00002105)  : begin
                            HrStr := 'ERROR_DS_DRA_ACCESS_DENIED' ;
                            HrDescr := 'Replication access was denied.';
                          end;
    LongInt($00002106)  : begin
                            HrStr := 'ERROR_DS_DRA_NOT_SUPPORTED' ;
                            HrDescr := 'The requested operation is not supported by this version of the directory service.';
                          end;
    LongInt($00002107)  : begin
                            HrStr := 'ERROR_DS_DRA_RPC_CANCELLED' ;
                            HrDescr := 'The replication RPC was canceled.';
                          end;
    LongInt($00002108)  : begin
                            HrStr := 'ERROR_DS_DRA_SOURCE_DISABLED' ;
                            HrDescr := 'The source server is currently rejecting replication requests.';
                          end;
    LongInt($00002109)  : begin
                            HrStr := 'ERROR_DS_DRA_SINK_DISABLED' ;
                            HrDescr := 'The destination server is currently rejecting replication requests.';
                          end;
    LongInt($0000210A)  : begin
                            HrStr := 'ERROR_DS_DRA_NAME_COLLISION' ;
                            HrDescr := 'The replication operation failed due to a collision of object names.';
                          end;
    LongInt($0000210B)  : begin
                            HrStr := 'ERROR_DS_DRA_SOURCE_REINSTALLED' ;
                            HrDescr := 'The replication source has been reinstalled.';
                          end;
    LongInt($0000210C)  : begin
                            HrStr := 'ERROR_DS_DRA_MISSING_PARENT' ;
                            HrDescr := 'The replication operation failed because a required parent object is missing.';
                          end;
    LongInt($0000210D)  : begin
                            HrStr := 'ERROR_DS_DRA_PREEMPTED' ;
                            HrDescr := 'The replication operation was preempted.';
                          end;
    LongInt($0000210E)  : begin
                            HrStr := 'ERROR_DS_DRA_ABANDON_SYNC' ;
                            HrDescr := 'The replication synchronization attempt was abandoned because of a lack of updates.';
                          end;
    LongInt($0000210F)  : begin
                            HrStr := 'ERROR_DS_DRA_SHUTDOWN' ;
                            HrDescr := 'The replication operation was terminated because the system is shutting down.';
                          end;
    LongInt($00002110)  : begin
                            HrStr := 'ERROR_DS_DRA_INCOMPATIBLE_PARTIAL_SET' ;
                            HrDescr := 'A synchronization attempt failed because the destination DC is currently waiting to synchronize new partial attributes from the source.' +
                                       'This condition is normal if a recent schema change modified the partial attribute set.' +
                                       'The destination partial attribute set is not a subset of the source partial attribute set.';
                          end;
    LongInt($00002111)  : begin
                            HrStr := 'ERROR_DS_DRA_SOURCE_IS_PARTIAL_REPLICA' ;
                            HrDescr := 'The replication synchronization attempt failed because a master replica attempted to sync from a partial replica.';
                          end;
    LongInt($00002112)  : begin
                            HrStr := 'ERROR_DS_DRA_EXTN_CONNECTION_FAILED' ;
                            HrDescr := 'The server specified for this replication operation was contacted, but that server was unable to contact an additional server needed to complete the operation.';
                          end;
    LongInt($00002113)  : begin
                            HrStr := 'ERROR_DS_INSTALL_SCHEMA_MISMATCH' ;
                            HrDescr := 'The version of the directory service schema of the source forest is not compatible with the version of the directory service on this computer.';
                          end;
    LongInt($00002114)  : begin
                            HrStr := 'ERROR_DS_DUP_LINK_ID' ;
                            HrDescr := 'Schema update failed: An attribute with the same link identifier already exists.';
                          end;
    LongInt($00002115)  : begin
                            HrStr := 'ERROR_DS_NAME_ERROR_RESOLVING' ;
                            HrDescr := 'Name translation: Generic processing error.';
                          end;
    LongInt($00002116)  : begin
                            HrStr := 'ERROR_DS_NAME_ERROR_NOT_FOUND' ;
                            HrDescr := 'Name translation: Could not find the name or insufficient right to see name.';
                          end;
    LongInt($00002117)  : begin
                            HrStr := 'ERROR_DS_NAME_ERROR_NOT_UNIQUE' ;
                            HrDescr := 'Name translation: Input name mapped to more than one output name.';
                          end;
    LongInt($00002118)  : begin
                            HrStr := 'ERROR_DS_NAME_ERROR_NO_MAPPING' ;
                            HrDescr := 'Name translation: The input name was found but not the associated output format.';
                          end;
    LongInt($00002119)  : begin
                            HrStr := 'ERROR_DS_NAME_ERROR_DOMAIN_ONLY' ;
                            HrDescr := 'Name translation: Unable to resolve completely, only the domain was found.';
                          end;
    LongInt($0000211A)  : begin
                            HrStr := 'ERROR_DS_NAME_ERROR_NO_SYNTACTICAL_MAPPING' ;
                            HrDescr := 'Name translation: Unable to perform purely syntactical mapping at the client without going out to the wire.';
                          end;
    LongInt($0000211B)  : begin
                            HrStr := 'ERROR_DS_CONSTRUCTED_ATT_MOD' ;
                            HrDescr := 'Modification of a constructed attribute is not allowed.';
                          end;
    LongInt($0000211C)  : begin
                            HrStr := 'ERROR_DS_WRONG_OM_OBJ_CLASS' ;
                            HrDescr := 'The OM-Object-Class specified is incorrect for an attribute with the specified syntax.';
                          end;
    LongInt($0000211D)  : begin
                            HrStr := 'ERROR_DS_DRA_REPL_PENDING' ;
                            HrDescr := 'The replication request has been posted; waiting for a reply.';
                          end;
    LongInt($0000211E)  : begin
                            HrStr := 'ERROR_DS_DS_REQUIRED' ;
                            HrDescr := 'The requested operation requires a directory service, and none was available.';
                          end;
    LongInt($0000211F)  : begin
                            HrStr := 'ERROR_DS_INVALID_LDAP_DISPLAY_NAME' ;
                            HrDescr := 'The LDAP display name of the class or attribute contains non-ASCII characters.';
                          end;
    LongInt($00002120)  : begin
                            HrStr := 'ERROR_DS_NON_BASE_SEARCH' ;
                            HrDescr := 'The requested search operation is only supported for base searches.';
                          end;
    LongInt($00002121)  : begin
                            HrStr := 'ERROR_DS_CANT_RETRIEVE_ATTS' ;
                            HrDescr := 'The search failed to retrieve attributes from the database.';
                          end;
    LongInt($00002122)  : begin
                            HrStr := 'ERROR_DS_BACKLINK_WITHOUT_LINK' ;
                            HrDescr := 'The schema update operation tried to add a backward link attribute that has no corresponding forward link.';
                          end;
    LongInt($00002123)  : begin
                            HrStr := 'ERROR_DS_EPOCH_MISMATCH' ;
                            HrDescr := 'The source and destination of a cross-domain move do not agree on the object''s epoch number.' +
                                       'Either the source or the destination does not have the latest version of the object.';
                          end;
    LongInt($00002124)  : begin
                            HrStr := 'ERROR_DS_SRC_NAME_MISMATCH' ;
                            HrDescr := 'The source and destination of a cross-domain move do not agree on the object''s current name.' +
                                       'Either the source or the destination does not have the latest version of the object.';
                          end;
    LongInt($00002125)  : begin
                            HrStr := 'ERROR_DS_SRC_AND_DST_NC_IDENTICAL' ;
                            HrDescr := 'The source and destination for the cross-domain move operation are identical.' +
                                       'The caller should use a local move operation instead of a cross-domain move operation.';
                          end;
    LongInt($00002126)  : begin
                            HrStr := 'ERROR_DS_DST_NC_MISMATCH' ;
                            HrDescr := 'The source and destination for a cross-domain move do not agree on the naming contexts in the forest.' +
                                       'Either the source or the destination does not have the latest version of the Partitions container.';
                          end;
    LongInt($00002127)  : begin
                            HrStr := 'ERROR_DS_NOT_AUTHORITIVE_FOR_DST_NC' ;
                            HrDescr := 'The destination of a cross-domain move is not authoritative for the destination naming context.';
                          end;
    LongInt($00002128)  : begin
                            HrStr := 'ERROR_DS_SRC_GUID_MISMATCH' ;
                            HrDescr := 'The source and destination of a cross-domain move do not agree on the identity of the source object.' +
                                       'Either the source or the destination does not have the latest version of the source object.';
                          end;
    LongInt($00002129)  : begin
                            HrStr := 'ERROR_DS_CANT_MOVE_DELETED_OBJECT' ;
                            HrDescr := 'The object being moved across domains is already known to be deleted by the destination server.' +
                                       'The source server does not have the latest version of the source object.';
                          end;
    LongInt($0000212A)  : begin
                            HrStr := 'ERROR_DS_PDC_OPERATION_IN_PROGRESS' ;
                            HrDescr := 'Another operation that requires exclusive access to the PDC FSMO is already in progress.';
                          end;
    LongInt($0000212B)  : begin
                            HrStr := 'ERROR_DS_CROSS_DOMAIN_CLEANUP_REQD' ;
                            HrDescr := 'A cross-domain move operation failed because two versions of the moved object existùone each in the source and destination domains.' +
                                       'The destination object needs to be removed to restore the system to a consistent state.';
                          end;
    LongInt($0000212C)  : begin
                            HrStr := 'ERROR_DS_ILLEGAL_XDOM_MOVE_OPERATION' ;
                            HrDescr := 'This object cannot be moved across domain boundaries either because cross-domain moves for this class are not allowed,' +
                                       'or the object has some special characteristics, for example, a trust account or a restricted relative identifier (RID),' +
                                       'that prevent its move.';
                          end;
    LongInt($0000212D)  : begin
                            HrStr := 'ERROR_DS_CANT_WITH_ACCT_GROUP_MEMBERSHPS' ;
                            HrDescr := 'Cannot move objects with memberships across domain boundaries because, once moved, this violates the membership conditions of the account group.' +
                                       'Remove the object from any account group memberships and retry.';
                          end;
    LongInt($0000212E)  : begin
                            HrStr := 'ERROR_DS_NC_MUST_HAVE_NC_PARENT' ;
                            HrDescr := 'A naming context head must be the immediate child of another naming context head, not of an interior node.';
                          end;
    LongInt($0000212F)  : begin
                            HrStr := 'ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE' ;
                            HrDescr := 'The directory cannot validate the proposed naming context name because it does not hold a replica of the naming context above the proposed naming context.' +
                                       'Ensure that the domain naming master role is held by a server that is configured as a GC server, and that the server is up-to-date with its replication partners.' +
                                       '(Applies only to Windows 2000 operating system domain naming masters.)';
                          end;
    LongInt($00002130)  : begin
                            HrStr := 'ERROR_DS_DST_DOMAIN_NOT_NATIVE' ;
                            HrDescr := 'Destination domain must be in native mode.';
                          end;
    LongInt($00002131)  : begin
                            HrStr := 'ERROR_DS_MISSING_INFRASTRUCTURE_CONTAINER' ;
                            HrDescr := 'The operation cannot be performed because the server does not have an infrastructure container in the domain of interest.';
                          end;
    LongInt($00002132)  : begin
                            HrStr := 'ERROR_DS_CANT_MOVE_ACCOUNT_GROUP' ;
                            HrDescr := 'Cross-domain moves of nonempty account groups is not allowed.';
                          end;
    LongInt($00002133)  : begin
                            HrStr := 'ERROR_DS_CANT_MOVE_RESOURCE_GROUP' ;
                            HrDescr := 'Cross-domain moves of nonempty resource groups is not allowed.';
                          end;
    LongInt($00002134)  : begin
                            HrStr := 'ERROR_DS_INVALID_SEARCH_FLAG' ;
                            HrDescr := 'The search flags for the attribute are invalid.' +
                                       'The ambiguous name resolution (ANR) bit is valid only on attributes of Unicode or Teletex strings.';
                          end;
    LongInt($00002135)  : begin
                            HrStr := 'ERROR_DS_NO_TREE_DELETE_ABOVE_NC' ;
                            HrDescr := 'Tree deletions starting at an object that has an NC head as a descendant are not allowed.';
                          end;
    LongInt($00002136)  : begin
                            HrStr := 'ERROR_DS_COULDNT_LOCK_TREE_FOR_DELETE' ;
                            HrDescr := 'The directory service failed to lock a tree in preparation for a tree deletion because the tree was in use.';
                          end;
    LongInt($00002137)  : begin
                            HrStr := 'ERROR_DS_COULDNT_IDENTIFY_OBJECTS_FOR_TREE_DELETE' ;
                            HrDescr := 'The directory service failed to identify the list of objects to delete while attempting a tree deletion.';
                          end;
    LongInt($00002138)  : begin
                            HrStr := 'ERROR_DS_SAM_INIT_FAILURE' ;
                            HrDescr := 'SAM initialization failed because of the following error: %1.' +
                                       'Error Status: $%2.' +
                                       'Click OK to shut down the system and reboot into Directory Services Restore Mode.' +
                                       'Check the event log for detailed information.';
                          end;
    LongInt($00002139)  : begin
                            HrStr := 'ERROR_DS_SENSITIVE_GROUP_VIOLATION' ;
                            HrDescr := 'Only an administrator can modify the membership list of an administrative group.';
                          end;
    LongInt($0000213A)  : begin
                            HrStr := 'ERROR_DS_CANT_MOD_PRIMARYGROUPID' ;
                            HrDescr := 'Cannot change the primary group ID of a domain controller account.';
                          end;
    LongInt($0000213B)  : begin
                            HrStr := 'ERROR_DS_ILLEGAL_BASE_SCHEMA_MOD' ;
                            HrDescr := 'An attempt was made to modify the base schema.';
                          end;
    LongInt($0000213C)  : begin
                            HrStr := 'ERROR_DS_NONSAFE_SCHEMA_CHANGE' ;
                            HrDescr := 'Adding a new mandatory attribute to an existing class, deleting a mandatory attribute from an existing class,' +
                                       'or adding an optional attribute to the special class Top that is not a backlink attribute (directly or through inheritance,' +
                                       'for example, by adding or deleting an auxiliary class) is not allowed.';
                          end;
    LongInt($0000213D)  : begin
                            HrStr := 'ERROR_DS_SCHEMA_UPDATE_DISALLOWED' ;
                            HrDescr := 'Schema update is not allowed on this DC because the DC is not the schema FSMO role owner.';
                          end;
    LongInt($0000213E)  : begin
                            HrStr := 'ERROR_DS_CANT_CREATE_UNDER_SCHEMA' ;
                            HrDescr := 'An object of this class cannot be created under the schema container.' +
                                       'You can only create Attribute-Schema and Class-Schema objects under the schema container.';
                          end;
    LongInt($0000213F)  : begin
                            HrStr := 'ERROR_DS_INSTALL_NO_SRC_SCH_VERSION' ;
                            HrDescr := 'The replica or child install failed to get the objectVersion attribute on the schema container on the source DC.' +
                                       'Either the attribute is missing on the schema container or the credentials supplied do not have permission to read it.';
                          end;
    LongInt($00002140)  : begin
                            HrStr := 'ERROR_DS_INSTALL_NO_SCH_VERSION_IN_INIFILE' ;
                            HrDescr := 'The replica or child install failed to read the objectVersion attribute in the SCHEMA section of the file schema.ini in the System32 directory.';
                          end;
    LongInt($00002141)  : begin
                            HrStr := 'ERROR_DS_INVALID_GROUP_TYPE' ;
                            HrDescr := 'The specified group type is invalid.';
                          end;
    LongInt($00002142)  : begin
                            HrStr := 'ERROR_DS_NO_NEST_GLOBALGROUP_IN_MIXEDDOMAIN' ;
                            HrDescr := 'You cannot nest global groups in a mixed domain if the group is security-enabled.';
                          end;
    LongInt($00002143)  : begin
                            HrStr := 'ERROR_DS_NO_NEST_LOCALGROUP_IN_MIXEDDOMAIN' ;
                            HrDescr := 'You cannot nest local groups in a mixed domain if the group is security-enabled.';
                          end;
    LongInt($00002144)  : begin
                            HrStr := 'ERROR_DS_GLOBAL_CANT_HAVE_LOCAL_MEMBER' ;
                            HrDescr := 'A global group cannot have a local group as a member.';
                          end;
    LongInt($00002145)  : begin
                            HrStr := 'ERROR_DS_GLOBAL_CANT_HAVE_UNIVERSAL_MEMBER' ;
                            HrDescr := 'A global group cannot have a universal group as a member.';
                          end;
    LongInt($00002146)  : begin
                            HrStr := 'ERROR_DS_UNIVERSAL_CANT_HAVE_LOCAL_MEMBER' ;
                            HrDescr := 'A universal group cannot have a local group as a member.';
                          end;
    LongInt($00002147)  : begin
                            HrStr := 'ERROR_DS_GLOBAL_CANT_HAVE_CROSSDOMAIN_MEMBER' ;
                            HrDescr := 'A global group cannot have a cross-domain member.';
                          end;
    LongInt($00002148)  : begin
                            HrStr := 'ERROR_DS_LOCAL_CANT_HAVE_CROSSDOMAIN_LOCAL_MEMBER' ;
                            HrDescr := 'A local group cannot have another cross domain local group as a member.';
                          end;
    LongInt($00002149)  : begin
                            HrStr := 'ERROR_DS_HAVE_PRIMARY_MEMBERS' ;
                            HrDescr := 'A group with primary members cannot change to a security-disabled group.';
                          end;
    LongInt($0000214A)  : begin
                            HrStr := 'ERROR_DS_STRING_SD_CONVERSION_FAILED' ;
                            HrDescr := 'The schema cache load failed to convert the string default security descriptor (SD) on a class-schema object.';
                          end;
    LongInt($0000214B)  : begin
                            HrStr := 'ERROR_DS_NAMING_MASTER_GC' ;
                            HrDescr := 'Only DSAs configured to be GC servers should be allowed to hold the domain naming master FSMO role.' +
                                       '(Applies only to Windows 2000 servers.)';
                          end;
    LongInt($0000214C)  : begin
                            HrStr := 'ERROR_DS_DNS_LOOKUP_FAILURE' ;
                            HrDescr := 'The DSA operation is unable to proceed because of a DNS lookup failure.';
                          end;
    LongInt($0000214D)  : begin
                            HrStr := 'ERROR_DS_COULDNT_UPDATE_SPNS' ;
                            HrDescr := 'While processing a change to the DNS host name for an object, the SPN values could not be kept in sync.';
                          end;
    LongInt($0000214E)  : begin
                            HrStr := 'ERROR_DS_CANT_RETRIEVE_SD' ;
                            HrDescr := 'The Security Descriptor attribute could not be read.';
                          end;
    LongInt($0000214F)  : begin
                            HrStr := 'ERROR_DS_KEY_NOT_UNIQUE' ;
                            HrDescr := 'The object requested was not found, but an object with that key was found.';
                          end;
    LongInt($00002150)  : begin
                            HrStr := 'ERROR_DS_WRONG_LINKED_ATT_SYNTAX' ;
                            HrDescr := 'The syntax of the linked attribute being added is incorrect.' +
                                       'Forward links can only have syntax 2.5.5.1, 2.5.5.7, and 2.5.5.14, and backlinks can only have syntax 2.5.5.1.';
                          end;
    LongInt($00002151)  : begin
                            HrStr := 'ERROR_DS_SAM_NEED_BOOTKEY_PASSWORD' ;
                            HrDescr := 'SAM needs to get the boot password.';
                          end;
    LongInt($00002152)  : begin
                            HrStr := 'ERROR_DS_SAM_NEED_BOOTKEY_FLOPPY' ;
                            HrDescr := 'SAM needs to get the boot key from the floppy disk.';
                          end;
    LongInt($00002153)  : begin
                            HrStr := 'ERROR_DS_CANT_START' ;
                            HrDescr := 'Directory Service cannot start.';
                          end;
    LongInt($00002154)  : begin
                            HrStr := 'ERROR_DS_INIT_FAILURE' ;
                            HrDescr := 'Directory Services could not start.';
                          end;
    LongInt($00002155)  : begin
                            HrStr := 'ERROR_DS_NO_PKT_PRIVACY_ON_CONNECTION' ;
                            HrDescr := 'The connection between client and server requires packet privacy or better.';
                          end;
    LongInt($00002156)  : begin
                            HrStr := 'ERROR_DS_SOURCE_DOMAIN_IN_FOREST' ;
                            HrDescr := 'The source domain cannot be in the same forest as the destination.';
                          end;
    LongInt($00002157)  : begin
                            HrStr := 'ERROR_DS_DESTINATION_DOMAIN_NOT_IN_FOREST' ;
                            HrDescr := 'The destination domain MUST be in the forest.';
                          end;
    LongInt($00002158)  : begin
                            HrStr := 'ERROR_DS_DESTINATION_AUDITING_NOT_ENABLED' ;
                            HrDescr := 'The operation requires that destination domain auditing be enabled.';
                          end;
    LongInt($00002159)  : begin
                            HrStr := 'ERROR_DS_CANT_FIND_DC_FOR_SRC_DOMAIN' ;
                            HrDescr := 'The operation could not locate a DC for the source domain.';
                          end;
    LongInt($0000215A)  : begin
                            HrStr := 'ERROR_DS_SRC_OBJ_NOT_GROUP_OR_USER' ;
                            HrDescr := 'The source object must be a group or user.';
                          end;
    LongInt($0000215B)  : begin
                            HrStr := 'ERROR_DS_SRC_SID_EXISTS_IN_FOREST' ;
                            HrDescr := 'The source object''s SID already exists in the destination forest.';
                          end;
    LongInt($0000215C)  : begin
                            HrStr := 'ERROR_DS_SRC_AND_DST_OBJECT_CLASS_MISMATCH' ;
                            HrDescr := 'The source and destination object must be of the same type.';
                          end;
    LongInt($0000215D)  : begin
                            HrStr := 'ERROR_SAM_INIT_FAILURE' ;
                            HrDescr := 'SAM initialization failed because of the following error: %1.' +
                                       'Error Status: $%2.' +
                                       'Click OK to shut down the system and reboot into Safe Mode.' +
                                       'Check the event log for detailed information.' +
                                       '';
                          end;
    LongInt($0000215E)  : begin
                            HrStr := 'ERROR_DS_DRA_SCHEMA_INFO_SHIP' ;
                            HrDescr := 'Schema information could not be included in the replication request.';
                          end;
    LongInt($0000215F)  : begin
                            HrStr := 'ERROR_DS_DRA_SCHEMA_CONFLICT' ;
                            HrDescr := 'The replication operation could not be completed due to a schema incompatibility.';
                          end;
    LongInt($00002160)  : begin
                            HrStr := 'ERROR_DS_DRA_EARLIER_SCHEMA_CONFLICT' ;
                            HrDescr := 'The replication operation could not be completed due to a previous schema incompatibility.';
                          end;
    LongInt($00002161)  : begin
                            HrStr := 'ERROR_DS_DRA_OBJ_NC_MISMATCH' ;
                            HrDescr := 'The replication update could not be applied because either the source or the destination has not yet received information regarding a recent cross-domain move operation.';
                          end;
    LongInt($00002162)  : begin
                            HrStr := 'ERROR_DS_NC_STILL_HAS_DSAS' ;
                            HrDescr := 'The requested domain could not be deleted because there exist domain controllers that still host this domain.';
                          end;
    LongInt($00002163)  : begin
                            HrStr := 'ERROR_DS_GC_REQUIRED' ;
                            HrDescr := 'The requested operation can be performed only on a GC server.';
                          end;
    LongInt($00002164)  : begin
                            HrStr := 'ERROR_DS_LOCAL_MEMBER_OF_LOCAL_ONLY' ;
                            HrDescr := 'A local group can only be a member of other local groups in the same domain.';
                          end;
    LongInt($00002165)  : begin
                            HrStr := 'ERROR_DS_NO_FPO_IN_UNIVERSAL_GROUPS' ;
                            HrDescr := 'Foreign security principals cannot be members of universal groups.';
                          end;
    LongInt($00002166)  : begin
                            HrStr := 'ERROR_DS_CANT_ADD_TO_GC' ;
                            HrDescr := 'The attribute is not allowed to be replicated to the GC because of security reasons.';
                          end;
    LongInt($00002167)  : begin
                            HrStr := 'ERROR_DS_NO_CHECKPOINT_WITH_PDC' ;
                            HrDescr := 'The checkpoint with the PDC could not be taken because too many modifications are currently being processed.';
                          end;
    LongInt($00002168)  : begin
                            HrStr := 'ERROR_DS_SOURCE_AUDITING_NOT_ENABLED' ;
                            HrDescr := 'The operation requires that source domain auditing be enabled.';
                          end;
    LongInt($00002169)  : begin
                            HrStr := 'ERROR_DS_CANT_CREATE_IN_NONDOMAIN_NC' ;
                            HrDescr := 'Security principal objects can only be created inside domain naming contexts.';
                          end;
    LongInt($0000216A)  : begin
                            HrStr := 'ERROR_DS_INVALID_NAME_FOR_SPN' ;
                            HrDescr := 'An SPN could not be constructed because the provided host name is not in the necessary format.';
                          end;
    LongInt($0000216B)  : begin
                            HrStr := 'ERROR_DS_FILTER_USES_CONTRUCTED_ATTRS' ;
                            HrDescr := 'A filter was passed that uses constructed attributes.';
                          end;
    LongInt($0000216C)  : begin
                            HrStr := 'ERROR_DS_UNICODEPWD_NOT_IN_QUOTES' ;
                            HrDescr := 'The unicodePwd attribute value must be enclosed in quotation marks.';
                          end;
    LongInt($0000216D)  : begin
                            HrStr := 'ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED' ;
                            HrDescr := 'Your computer could not be joined to the domain.' +
                                       'You have exceeded the maximum number of computer accounts you are allowed to create in this domain.' +
                                       'Contact your system administrator to have this limit reset or increased.';
                          end;
    LongInt($0000216E)  : begin
                            HrStr := 'ERROR_DS_MUST_BE_RUN_ON_DST_DC' ;
                            HrDescr := 'For security reasons, the operation must be run on the destination DC.';
                          end;
    LongInt($0000216F)  : begin
                            HrStr := 'ERROR_DS_SRC_DC_MUST_BE_SP4_OR_GREATER' ;
                            HrDescr := 'For security reasons, the source DC must be NT4SP4 or greater.';
                          end;
    LongInt($00002170)  : begin
                            HrStr := 'ERROR_DS_CANT_TREE_DELETE_CRITICAL_OBJ' ;
                            HrDescr := 'Critical directory service system objects cannot be deleted during tree deletion operations.' +
                                       'The tree deletion might have been partially performed.';
                          end;
    LongInt($00002171)  : begin
                            HrStr := 'ERROR_DS_INIT_FAILURE_CONSOLE' ;
                            HrDescr := 'Directory Services could not start because of the following error: %1.' +
                                       'Error Status: $%2.' +
                                       'Click OK to shut down the system.' +
                                       'You can use the Recovery Console to further diagnose the system.' +
                                       '';
                          end;
    LongInt($00002172)  : begin
                            HrStr := 'ERROR_DS_SAM_INIT_FAILURE_CONSOLE' ;
                            HrDescr := 'SAM initialization failed because of the following error: %1.' +
                                       'Error Status: $%2.' +
                                       'Click OK to shut down the system.' +
                                       'You can use the Recovery Console to further diagnose the system.' +
                                       '';
                          end;
    LongInt($00002173)  : begin
                            HrStr := 'ERROR_DS_FOREST_VERSION_TOO_HIGH' ;
                            HrDescr := 'The version of the operating system installed is incompatible with the current forest functional level.' +
                                       'You must upgrade to a new version of the operating system before this server can become a domain controller in this forest.';
                          end;
    LongInt($00002174)  : begin
                            HrStr := 'ERROR_DS_DOMAIN_VERSION_TOO_HIGH' ;
                            HrDescr := 'The version of the operating system installed is incompatible with the current domain functional level.' +
                                       'You must upgrade to a new version of the operating system before this server can become a domain controller in this domain.';
                          end;
    LongInt($00002175)  : begin
                            HrStr := 'ERROR_DS_FOREST_VERSION_TOO_LOW' ;
                            HrDescr := 'The version of the operating system installed on this server no longer supports the current forest functional level.' +
                                       'You must raise the forest functional level before this server can become a domain controller in this forest.';
                          end;
    LongInt($00002176)  : begin
                            HrStr := 'ERROR_DS_DOMAIN_VERSION_TOO_LOW' ;
                            HrDescr := 'The version of the operating system installed on this server no longer supports the current domain functional level.' +
                                       'You must raise the domain functional level before this server can become a domain controller in this domain.';
                          end;
    LongInt($00002177)  : begin
                            HrStr := 'ERROR_DS_INCOMPATIBLE_VERSION' ;
                            HrDescr := 'The version of the operating system installed on this server is incompatible with the functional level of the domain or forest.';
                          end;
    LongInt($00002178)  : begin
                            HrStr := 'ERROR_DS_LOW_DSA_VERSION' ;
                            HrDescr := 'The functional level of the domain (or forest) cannot be raised to the requested value because one or more domain controllers in the domain (or forest) are at a lower, incompatible functional level.';
                          end;
    LongInt($00002179)  : begin
                            HrStr := 'ERROR_DS_NO_BEHAVIOR_VERSION_IN_MIXEDDOMAIN' ;
                            HrDescr := 'The forest functional level cannot be raised to the requested value because one or more domains are still in mixed-domain mode.' +
                                       'All domains in the forest must be in native mode for you to raise the forest functional level.';
                          end;
    LongInt($0000217A)  : begin
                            HrStr := 'ERROR_DS_NOT_SUPPORTED_SORT_ORDER' ;
                            HrDescr := 'The sort order requested is not supported.';
                          end;
    LongInt($0000217B)  : begin
                            HrStr := 'ERROR_DS_NAME_NOT_UNIQUE' ;
                            HrDescr := 'The requested name already exists as a unique identifier.';
                          end;
    LongInt($0000217C)  : begin
                            HrStr := 'ERROR_DS_MACHINE_ACCOUNT_CREATED_PRENT4' ;
                            HrDescr := 'The machine account was created before Windows NT 4.0.' +
                                       'The account needs to be re-created.';
                          end;
    LongInt($0000217D)  : begin
                            HrStr := 'ERROR_DS_OUT_OF_VERSION_STORE' ;
                            HrDescr := 'The database is out of version store.';
                          end;
    LongInt($0000217E)  : begin
                            HrStr := 'ERROR_DS_INCOMPATIBLE_CONTROLS_USED' ;
                            HrDescr := 'Unable to continue operation because multiple conflicting controls were used.';
                          end;
    LongInt($0000217F)  : begin
                            HrStr := 'ERROR_DS_NO_REF_DOMAIN' ;
                            HrDescr := 'Unable to find a valid security descriptor reference domain for this partition.';
                          end;
    LongInt($00002180)  : begin
                            HrStr := 'ERROR_DS_RESERVED_LINK_ID' ;
                            HrDescr := 'Schema update failed: The link identifier is reserved.';
                          end;
    LongInt($00002181)  : begin
                            HrStr := 'ERROR_DS_LINK_ID_NOT_AVAILABLE' ;
                            HrDescr := 'Schema update failed: There are no link identifiers available.';
                          end;
    LongInt($00002182)  : begin
                            HrStr := 'ERROR_DS_AG_CANT_HAVE_UNIVERSAL_MEMBER' ;
                            HrDescr := 'An account group cannot have a universal group as a member.';
                          end;
    LongInt($00002183)  : begin
                            HrStr := 'ERROR_DS_MODIFYDN_DISALLOWED_BY_INSTANCE_TYPE' ;
                            HrDescr := 'Rename or move operations on naming context heads or read-only objects are not allowed.';
                          end;
    LongInt($00002184)  : begin
                            HrStr := 'ERROR_DS_NO_OBJECT_MOVE_IN_SCHEMA_NC' ;
                            HrDescr := 'Move operations on objects in the schema naming context are not allowed.';
                          end;
    LongInt($00002185)  : begin
                            HrStr := 'ERROR_DS_MODIFYDN_DISALLOWED_BY_FLAG' ;
                            HrDescr := 'A system flag has been set on the object that does not allow the object to be moved or renamed.';
                          end;
    LongInt($00002186)  : begin
                            HrStr := 'ERROR_DS_MODIFYDN_WRONG_GRANDPARENT' ;
                            HrDescr := 'This object is not allowed to change its grandparent container.' +
                                       'Moves are not forbidden on this object, but are restricted to sibling containers.';
                          end;
    LongInt($00002187)  : begin
                            HrStr := 'ERROR_DS_NAME_ERROR_TRUST_REFERRAL' ;
                            HrDescr := 'Unable to resolve completely; a referral to another forest was generated.';
                          end;
    LongInt($00002188)  : begin
                            HrStr := 'ERROR_NOT_SUPPORTED_ON_STANDARD_SERVER' ;
                            HrDescr := 'The requested action is not supported on a standard server.';
                          end;
    LongInt($00002189)  : begin
                            HrStr := 'ERROR_DS_CANT_ACCESS_REMOTE_PART_OF_AD' ;
                            HrDescr := 'Could not access a partition of the directory service located on a remote server.' +
                                       'Make sure at least one server is running for the partition in question.';
                          end;
    LongInt($0000218A)  : begin
                            HrStr := 'ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE_V2' ;
                            HrDescr := 'The directory cannot validate the proposed naming context (or partition) name because it does not hold a replica, nor can it contact a replica of the naming context above the proposed naming context.' +
                                       'Ensure that the parent naming context is properly registered in the DNS, and at least one replica of this naming context is reachable by the domain naming master.';
                          end;
    LongInt($0000218B)  : begin
                            HrStr := 'ERROR_DS_THREAD_LIMIT_EXCEEDED' ;
                            HrDescr := 'The thread limit for this request was exceeded.';
                          end;
    LongInt($0000218C)  : begin
                            HrStr := 'ERROR_DS_NOT_CLOSEST' ;
                            HrDescr := 'The GC server is not in the closest site.';
                          end;
    LongInt($0000218D)  : begin
                            HrStr := 'ERROR_DS_CANT_DERIVE_SPN_WITHOUT_SERVER_REF' ;
                            HrDescr := 'The directory service cannot derive an SPN with which to mutually authenticate the target server because the corresponding server object in the local DS database has no serverReference attribute.';
                          end;
    LongInt($0000218E)  : begin
                            HrStr := 'ERROR_DS_SINGLE_USER_MODE_FAILED' ;
                            HrDescr := 'The directory service failed to enter single-user mode.';
                          end;
    LongInt($0000218F)  : begin
                            HrStr := 'ERROR_DS_NTDSCRIPT_SYNTAX_ERROR' ;
                            HrDescr := 'The directory service cannot parse the script because of a syntax error.';
                          end;
    LongInt($00002190)  : begin
                            HrStr := 'ERROR_DS_NTDSCRIPT_PROCESS_ERROR' ;
                            HrDescr := 'The directory service cannot process the script because of an error.';
                          end;
    LongInt($00002191)  : begin
                            HrStr := 'ERROR_DS_DIFFERENT_REPL_EPOCHS' ;
                            HrDescr := 'The directory service cannot perform the requested operation because the servers involved are of different replication epochs (which is usually related to a domain rename that is in progress).';
                          end;
    LongInt($00002192)  : begin
                            HrStr := 'ERROR_DS_DRS_EXTENSIONS_CHANGED' ;
                            HrDescr := 'The directory service binding must be renegotiated due to a change in the server extensions information.';
                          end;
    LongInt($00002193)  : begin
                            HrStr := 'ERROR_DS_REPLICA_SET_CHANGE_NOT_ALLOWED_ON_DISABLED_CR' ;
                            HrDescr := 'The operation is not allowed on a disabled cross-reference.';
                          end;
    LongInt($00002194)  : begin
                            HrStr := 'ERROR_DS_NO_MSDS_INTID' ;
                            HrDescr := 'Schema update failed: No values for msDS-IntId are available.';
                          end;
    LongInt($00002195)  : begin
                            HrStr := 'ERROR_DS_DUP_MSDS_INTID' ;
                            HrDescr := 'Schema update failed: Duplicate msDS-IntId.' +
                                       'Retry the operation.';
                          end;
    LongInt($00002196)  : begin
                            HrStr := 'ERROR_DS_EXISTS_IN_RDNATTID' ;
                            HrDescr := 'Schema deletion failed: Attribute is used in rDNAttID.';
                          end;
    LongInt($00002197)  : begin
                            HrStr := 'ERROR_DS_AUTHORIZATION_FAILED' ;
                            HrDescr := 'The directory service failed to authorize the request.';
                          end;
    LongInt($00002198)  : begin
                            HrStr := 'ERROR_DS_INVALID_SCRIPT' ;
                            HrDescr := 'The directory service cannot process the script because it is invalid.';
                          end;
    LongInt($00002199)  : begin
                            HrStr := 'ERROR_DS_REMOTE_CROSSREF_OP_FAILED' ;
                            HrDescr := 'The remote create cross-reference operation failed on the domain naming master FSMO.' +
                                       'The operation''s error is in the extended data.';
                          end;
    LongInt($0000219A)  : begin
                            HrStr := 'ERROR_DS_CROSS_REF_BUSY' ;
                            HrDescr := 'A cross-reference is in use locally with the same name.';
                          end;
    LongInt($0000219B)  : begin
                            HrStr := 'ERROR_DS_CANT_DERIVE_SPN_FOR_DELETED_DOMAIN' ;
                            HrDescr := 'The directory service cannot derive an SPN with which to mutually authenticate the target server because the server''s domain has been deleted from the forest.';
                          end;
    LongInt($0000219C)  : begin
                            HrStr := 'ERROR_DS_CANT_DEMOTE_WITH_WRITEABLE_NC' ;
                            HrDescr := 'Writable NCs prevent this DC from demoting.';
                          end;
    LongInt($0000219D)  : begin
                            HrStr := 'ERROR_DS_DUPLICATE_ID_FOUND' ;
                            HrDescr := 'The requested object has a nonunique identifier and cannot be retrieved.';
                          end;
    LongInt($0000219E)  : begin
                            HrStr := 'ERROR_DS_INSUFFICIENT_ATTR_TO_CREATE_OBJECT' ;
                            HrDescr := 'Insufficient attributes were given to create an object.' +
                                       'This object might not exist because it might have been deleted and the garbage already collected.';
                          end;
    LongInt($0000219F)  : begin
                            HrStr := 'ERROR_DS_GROUP_CONVERSION_ERROR' ;
                            HrDescr := 'The group cannot be converted due to attribute restrictions on the requested group type.';
                          end;
    LongInt($000021A0)  : begin
                            HrStr := 'ERROR_DS_CANT_MOVE_APP_BASIC_GROUP' ;
                            HrDescr := 'Cross-domain moves of nonempty basic application groups is not allowed.';
                          end;
    LongInt($000021A1)  : begin
                            HrStr := 'ERROR_DS_CANT_MOVE_APP_QUERY_GROUP' ;
                            HrDescr := 'Cross-domain moves of nonempty query-based application groups is not allowed.';
                          end;
    LongInt($000021A2)  : begin
                            HrStr := 'ERROR_DS_ROLE_NOT_VERIFIED' ;
                            HrDescr := 'The FSMO role ownership could not be verified because its directory partition did not replicate successfully with at least one replication partner.';
                          end;
    LongInt($000021A3)  : begin
                            HrStr := 'ERROR_DS_WKO_CONTAINER_CANNOT_BE_SPECIAL' ;
                            HrDescr := 'The target container for a redirection of a well-known object container cannot already be a special container.';
                          end;
    LongInt($000021A4)  : begin
                            HrStr := 'ERROR_DS_DOMAIN_RENAME_IN_PROGRESS' ;
                            HrDescr := 'The directory service cannot perform the requested operation because a domain rename operation is in progress.';
                          end;
    LongInt($000021A5)  : begin
                            HrStr := 'ERROR_DS_EXISTING_AD_CHILD_NC' ;
                            HrDescr := 'The directory service detected a child partition below the requested partition name.' +
                                       'The partition hierarchy must be created in a top down method.';
                          end;
    LongInt($000021A6)  : begin
                            HrStr := 'ERROR_DS_REPL_LIFETIME_EXCEEDED' ;
                            HrDescr := 'The directory service cannot replicate with this server because the time since the last replication with this server has exceeded the tombstone lifetime.';
                          end;
    LongInt($000021A7)  : begin
                            HrStr := 'ERROR_DS_DISALLOWED_IN_SYSTEM_CONTAINER' ;
                            HrDescr := 'The requested operation is not allowed on an object under the system container.';
                          end;
    LongInt($000021A8)  : begin
                            HrStr := 'ERROR_DS_LDAP_SEND_QUEUE_FULL' ;
                            HrDescr := 'The LDAP server''s network send queue has filled up because the client is not processing the results of its requests fast enough.' +
                                       'No more requests will be processed until the client catches up.' +
                                       'If the client does not catch up then it will be disconnected.';
                          end;
    LongInt($000021A9)  : begin
                            HrStr := 'ERROR_DS_DRA_OUT_SCHEDULE_WINDOW' ;
                            HrDescr := 'The scheduled replication did not take place because the system was too busy to execute the request within the schedule window.' +
                                       'The replication queue is overloaded.' +
                                       'Consider reducing the number of partners or decreasing the scheduled replication frequency.';
                          end;
    LongInt($000021AA)  : begin
                            HrStr := 'ERROR_DS_POLICY_NOT_KNOWN' ;
                            HrDescr := 'At this time, it cannot be determined if the branch replication policy is available on the hub domain controller.' +
                                       'Retry at a later time to account for replication latencies.';
                          end;
    LongInt($000021AB)  : begin
                            HrStr := 'ERROR_NO_SITE_SETTINGS_OBJECT' ;
                            HrDescr := 'The site settings object for the specified site does not exist.';
                          end;
    LongInt($000021AC)  : begin
                            HrStr := 'ERROR_NO_SECRETS' ;
                            HrDescr := 'The local account store does not contain secret material for the specified account.';
                          end;
    LongInt($000021AD)  : begin
                            HrStr := 'ERROR_NO_WRITABLE_DC_FOUND' ;
                            HrDescr := 'Could not find a writable domain controller in the domain.';
                          end;
    LongInt($000021AE)  : begin
                            HrStr := 'ERROR_DS_NO_SERVER_OBJECT' ;
                            HrDescr := 'The server object for the domain controller does not exist.';
                          end;
    LongInt($000021AF)  : begin
                            HrStr := 'ERROR_DS_NO_NTDSA_OBJECT' ;
                            HrDescr := 'The NTDS Settings object for the domain controller does not exist.';
                          end;
    LongInt($000021B0)  : begin
                            HrStr := 'ERROR_DS_NON_ASQ_SEARCH' ;
                            HrDescr := 'The requested search operation is not supported for attribute scoped query (ASQ) searches.';
                          end;
    LongInt($000021B1)  : begin
                            HrStr := 'ERROR_DS_AUDIT_FAILURE' ;
                            HrDescr := 'A required audit event could not be generated for the operation.';
                          end;
    LongInt($000021B2)  : begin
                            HrStr := 'ERROR_DS_INVALID_SEARCH_FLAG_SUBTREE' ;
                            HrDescr := 'The search flags for the attribute are invalid.' +
                                       'The subtree index bit is valid only on single-valued attributes.';
                          end;
    LongInt($000021B3)  : begin
                            HrStr := 'ERROR_DS_INVALID_SEARCH_FLAG_TUPLE' ;
                            HrDescr := 'The search flags for the attribute are invalid.' +
                                       'The tuple index bit is valid only on attributes of Unicode strings.';
                          end;
    LongInt($000021BF)  : begin
                            HrStr := 'ERROR_DS_DRA_RECYCLED_TARGET' ;
                            HrDescr := 'The replication operation failed because the target object referenced by a link value is recycled.';
                          end;
    LongInt($000021C2)  : begin
                            HrStr := 'ERROR_DS_HIGH_DSA_VERSION' ;
                            HrDescr := 'The functional level of the domain (or forest) cannot be lowered to the requested value.';
                          end;
    LongInt($000021C7)  : begin
                            HrStr := 'ERROR_DS_SPN_VALUE_NOT_UNIQUE_IN_FOREST' ;
                            HrDescr := 'The operation failed because the SPN value provided for addition/modification is not unique forest-wide.';
                          end;
    LongInt($000021C8)  : begin
                            HrStr := 'ERROR_DS_UPN_VALUE_NOT_UNIQUE_IN_FOREST' ;
                            HrDescr := 'The operation failed because the UPN value provided for addition/modification is not unique forest-wide.';
                          end;
    LongInt($00002329)  : begin
                            HrStr := 'DNS_ERROR_RCODE_FORMAT_ERROR' ;
                            HrDescr := 'DNS server unable to interpret format.';
                          end;
    LongInt($0000232A)  : begin
                            HrStr := 'DNS_ERROR_RCODE_SERVER_FAILURE' ;
                            HrDescr := 'DNS server failure.';
                          end;
    LongInt($0000232B)  : begin
                            HrStr := 'DNS_ERROR_RCODE_NAME_ERROR' ;
                            HrDescr := 'DNS name does not exist.';
                          end;
    LongInt($0000232C)  : begin
                            HrStr := 'DNS_ERROR_RCODE_NOT_IMPLEMENTED' ;
                            HrDescr := 'DNS request not supported by name server.';
                          end;
    LongInt($0000232D)  : begin
                            HrStr := 'DNS_ERROR_RCODE_REFUSED' ;
                            HrDescr := 'DNS operation refused.';
                          end;
    LongInt($0000232E)  : begin
                            HrStr := 'DNS_ERROR_RCODE_YXDOMAIN' ;
                            HrDescr := 'DNS name that should not exist, does exist.';
                          end;
    LongInt($0000232F)  : begin
                            HrStr := 'DNS_ERROR_RCODE_YXRRSET' ;
                            HrDescr := 'DNS resource record (RR) set that should not exist, does exist.';
                          end;
    LongInt($00002330)  : begin
                            HrStr := 'DNS_ERROR_RCODE_NXRRSET' ;
                            HrDescr := 'DNS RR set that should to exist, does not exist.';
                          end;
    LongInt($00002331)  : begin
                            HrStr := 'DNS_ERROR_RCODE_NOTAUTH' ;
                            HrDescr := 'DNS server not authoritative for zone.';
                          end;
    LongInt($00002332)  : begin
                            HrStr := 'DNS_ERROR_RCODE_NOTZONE' ;
                            HrDescr := 'DNS name in update or prereq is not in zone.';
                          end;
    LongInt($00002338)  : begin
                            HrStr := 'DNS_ERROR_RCODE_BADSIG' ;
                            HrDescr := 'DNS signature failed to verify.';
                          end;
    LongInt($00002339)  : begin
                            HrStr := 'DNS_ERROR_RCODE_BADKEY' ;
                            HrDescr := 'DNS bad key.';
                          end;
    LongInt($0000233A)  : begin
                            HrStr := 'DNS_ERROR_RCODE_BADTIME' ;
                            HrDescr := 'DNS signature validity expired.';
                          end;
    LongInt($0000251D)  : begin
                            HrStr := 'DNS_INFO_NO_RECORDS' ;
                            HrDescr := 'No records found for given DNS query.';
                          end;
    LongInt($0000251E)  : begin
                            HrStr := 'DNS_ERROR_BAD_PACKET' ;
                            HrDescr := 'Bad DNS packet.';
                          end;
    LongInt($0000251F)  : begin
                            HrStr := 'DNS_ERROR_NO_PACKET' ;
                            HrDescr := 'No DNS packet.';
                          end;
    LongInt($00002520)  : begin
                            HrStr := 'DNS_ERROR_RCODE' ;
                            HrDescr := 'DNS error, check rcode.';
                          end;
    LongInt($00002521)  : begin
                            HrStr := 'DNS_ERROR_UNSECURE_PACKET' ;
                            HrDescr := 'Unsecured DNS packet.';
                          end;
    LongInt($0000254F)  : begin
                            HrStr := 'DNS_ERROR_INVALID_TYPE' ;
                            HrDescr := 'Invalid DNS type.';
                          end;
    LongInt($00002550)  : begin
                            HrStr := 'DNS_ERROR_INVALID_IP_ADDRESS' ;
                            HrDescr := 'Invalid IP address.';
                          end;
    LongInt($00002551)  : begin
                            HrStr := 'DNS_ERROR_INVALID_PROPERTY' ;
                            HrDescr := 'Invalid property.';
                          end;
    LongInt($00002552)  : begin
                            HrStr := 'DNS_ERROR_TRY_AGAIN_LATER' ;
                            HrDescr := 'Try DNS operation again later.';
                          end;
    LongInt($00002553)  : begin
                            HrStr := 'DNS_ERROR_NOT_UNIQUE' ;
                            HrDescr := 'Record for given name and type is not unique.';
                          end;
    LongInt($00002554)  : begin
                            HrStr := 'DNS_ERROR_NON_RFC_NAME' ;
                            HrDescr := 'DNS name does not comply with RFC specifications.';
                          end;
    LongInt($00002555)  : begin
                            HrStr := 'DNS_STATUS_FQDN' ;
                            HrDescr := 'DNS name is a fully qualified DNS name.';
                          end;
    LongInt($00002556)  : begin
                            HrStr := 'DNS_STATUS_DOTTED_NAME' ;
                            HrDescr := 'DNS name is dotted (multilabel).';
                          end;
    LongInt($00002557)  : begin
                            HrStr := 'DNS_STATUS_SINGLE_PART_NAME' ;
                            HrDescr := 'DNS name is a single-part name.';
                          end;
    LongInt($00002558)  : begin
                            HrStr := 'DNS_ERROR_INVALID_NAME_CHAR' ;
                            HrDescr := 'DNS name contains an invalid character.';
                          end;
    LongInt($00002559)  : begin
                            HrStr := 'DNS_ERROR_NUMERIC_NAME' ;
                            HrDescr := 'DNS name is entirely numeric.';
                          end;
    LongInt($0000255A)  : begin
                            HrStr := 'DNS_ERROR_NOT_ALLOWED_ON_ROOT_SERVER' ;
                            HrDescr := 'The operation requested is not permitted on a DNS root server.';
                          end;
    LongInt($0000255B)  : begin
                            HrStr := 'DNS_ERROR_NOT_ALLOWED_UNDER_DELEGATION' ;
                            HrDescr := 'The record could not be created because this part of the DNS namespace has been delegated to another server.';
                          end;
    LongInt($0000255C)  : begin
                            HrStr := 'DNS_ERROR_CANNOT_FIND_ROOT_HINTS' ;
                            HrDescr := 'The DNS server could not find a set of root hints.';
                          end;
    LongInt($0000255D)  : begin
                            HrStr := 'DNS_ERROR_INCONSISTENT_ROOT_HINTS' ;
                            HrDescr := 'The DNS server found root hints but they were not consistent across all adapters.';
                          end;
    LongInt($0000255E)  : begin
                            HrStr := 'DNS_ERROR_DWORD_VALUE_TOO_SMALL' ;
                            HrDescr := 'The specified value is too small for this parameter.';
                          end;
    LongInt($0000255F)  : begin
                            HrStr := 'DNS_ERROR_DWORD_VALUE_TOO_LARGE' ;
                            HrDescr := 'The specified value is too large for this parameter.';
                          end;
    LongInt($00002560)  : begin
                            HrStr := 'DNS_ERROR_BACKGROUND_LOADING' ;
                            HrDescr := 'This operation is not allowed while the DNS server is loading zones in the background.' +
                                       'Try again later.';
                          end;
    LongInt($00002561)  : begin
                            HrStr := 'DNS_ERROR_NOT_ALLOWED_ON_RODC' ;
                            HrDescr := 'The operation requested is not permitted on against a DNS server running on a read-only DC.';
                          end;
    LongInt($00002581)  : begin
                            HrStr := 'DNS_ERROR_ZONE_DOES_NOT_EXIST' ;
                            HrDescr := 'DNS zone does not exist.';
                          end;
    LongInt($00002582)  : begin
                            HrStr := 'DNS_ERROR_NO_ZONE_INFO' ;
                            HrDescr := 'DNS zone information not available.';
                          end;
    LongInt($00002583)  : begin
                            HrStr := 'DNS_ERROR_INVALID_ZONE_OPERATION' ;
                            HrDescr := 'Invalid operation for DNS zone.';
                          end;
    LongInt($00002584)  : begin
                            HrStr := 'DNS_ERROR_ZONE_CONFIGURATION_ERROR' ;
                            HrDescr := 'Invalid DNS zone configuration.';
                          end;
    LongInt($00002585)  : begin
                            HrStr := 'DNS_ERROR_ZONE_HAS_NO_SOA_RECORD' ;
                            HrDescr := 'DNS zone has no start of authority (SOA) record.';
                          end;
    LongInt($00002586)  : begin
                            HrStr := 'DNS_ERROR_ZONE_HAS_NO_NS_RECORDS' ;
                            HrDescr := 'DNS zone has no Name Server (NS) record.';
                          end;
    LongInt($00002587)  : begin
                            HrStr := 'DNS_ERROR_ZONE_LOCKED' ;
                            HrDescr := 'DNS zone is locked.';
                          end;
    LongInt($00002588)  : begin
                            HrStr := 'DNS_ERROR_ZONE_CREATION_FAILED' ;
                            HrDescr := 'DNS zone creation failed.';
                          end;
    LongInt($00002589)  : begin
                            HrStr := 'DNS_ERROR_ZONE_ALREADY_EXISTS' ;
                            HrDescr := 'DNS zone already exists.';
                          end;
    LongInt($0000258A)  : begin
                            HrStr := 'DNS_ERROR_AUTOZONE_ALREADY_EXISTS' ;
                            HrDescr := 'DNS automatic zone already exists.';
                          end;
    LongInt($0000258B)  : begin
                            HrStr := 'DNS_ERROR_INVALID_ZONE_TYPE' ;
                            HrDescr := 'Invalid DNS zone type.';
                          end;
    LongInt($0000258C)  : begin
                            HrStr := 'DNS_ERROR_SECONDARY_REQUIRES_MASTER_IP' ;
                            HrDescr := 'Secondary DNS zone requires master IP address.';
                          end;
    LongInt($0000258D)  : begin
                            HrStr := 'DNS_ERROR_ZONE_NOT_SECONDARY' ;
                            HrDescr := 'DNS zone not secondary.';
                          end;
    LongInt($0000258E)  : begin
                            HrStr := 'DNS_ERROR_NEED_SECONDARY_ADDRESSES' ;
                            HrDescr := 'Need secondary IP address.';
                          end;
    LongInt($0000258F)  : begin
                            HrStr := 'DNS_ERROR_WINS_INIT_FAILED' ;
                            HrDescr := 'WINS initialization failed.';
                          end;
    LongInt($00002590)  : begin
                            HrStr := 'DNS_ERROR_NEED_WINS_SERVERS' ;
                            HrDescr := 'Need WINS servers.';
                          end;
    LongInt($00002591)  : begin
                            HrStr := 'DNS_ERROR_NBSTAT_INIT_FAILED' ;
                            HrDescr := 'NBTSTAT initialization call failed.';
                          end;
    LongInt($00002592)  : begin
                            HrStr := 'DNS_ERROR_SOA_DELETE_INVALID' ;
                            HrDescr := 'Invalid delete of SOA.';
                          end;
    LongInt($00002593)  : begin
                            HrStr := 'DNS_ERROR_FORWARDER_ALREADY_EXISTS' ;
                            HrDescr := 'A conditional forwarding zone already exists for that name.';
                          end;
    LongInt($00002594)  : begin
                            HrStr := 'DNS_ERROR_ZONE_REQUIRES_MASTER_IP' ;
                            HrDescr := 'This zone must be configured with one or more master DNS server IP addresses.';
                          end;
    LongInt($00002595)  : begin
                            HrStr := 'DNS_ERROR_ZONE_IS_SHUTDOWN' ;
                            HrDescr := 'The operation cannot be performed because this zone is shut down.';
                          end;
    LongInt($000025B3)  : begin
                            HrStr := 'DNS_ERROR_PRIMARY_REQUIRES_DATAFILE' ;
                            HrDescr := 'The primary DNS zone requires a data file.';
                          end;
    LongInt($000025B4)  : begin
                            HrStr := 'DNS_ERROR_INVALID_DATAFILE_NAME' ;
                            HrDescr := 'Invalid data file name for the DNS zone.';
                          end;
    LongInt($000025B5)  : begin
                            HrStr := 'DNS_ERROR_DATAFILE_OPEN_FAILURE' ;
                            HrDescr := 'Failed to open the data file for the DNS zone.';
                          end;
    LongInt($000025B6)  : begin
                            HrStr := 'DNS_ERROR_FILE_WRITEBACK_FAILED' ;
                            HrDescr := 'Failed to write the data file for the DNS zone.';
                          end;
    LongInt($000025B7)  : begin
                            HrStr := 'DNS_ERROR_DATAFILE_PARSING' ;
                            HrDescr := 'Failure while reading datafile for DNS zone.';
                          end;
    LongInt($000025E5)  : begin
                            HrStr := 'DNS_ERROR_RECORD_DOES_NOT_EXIST' ;
                            HrDescr := 'DNS record does not exist.';
                          end;
    LongInt($000025E6)  : begin
                            HrStr := 'DNS_ERROR_RECORD_FORMAT' ;
                            HrDescr := 'DNS record format error.';
                          end;
    LongInt($000025E7)  : begin
                            HrStr := 'DNS_ERROR_NODE_CREATION_FAILED' ;
                            HrDescr := 'Node creation failure in DNS.';
                          end;
    LongInt($000025E8)  : begin
                            HrStr := 'DNS_ERROR_UNKNOWN_RECORD_TYPE' ;
                            HrDescr := 'Unknown DNS record type.';
                          end;
    LongInt($000025E9)  : begin
                            HrStr := 'DNS_ERROR_RECORD_TIMED_OUT' ;
                            HrDescr := 'DNS record timed out.';
                          end;
    LongInt($000025EA)  : begin
                            HrStr := 'DNS_ERROR_NAME_NOT_IN_ZONE' ;
                            HrDescr := 'Name not in DNS zone.';
                          end;
    LongInt($000025EB)  : begin
                            HrStr := 'DNS_ERROR_CNAME_LOOP' ;
                            HrDescr := 'CNAME loop detected.';
                          end;
    LongInt($000025EC)  : begin
                            HrStr := 'DNS_ERROR_NODE_IS_CNAME' ;
                            HrDescr := 'Node is a CNAME DNS record.';
                          end;
    LongInt($000025ED)  : begin
                            HrStr := 'DNS_ERROR_CNAME_COLLISION' ;
                            HrDescr := 'A CNAME record already exists for the given name.';
                          end;
    LongInt($000025EE)  : begin
                            HrStr := 'DNS_ERROR_RECORD_ONLY_AT_ZONE_ROOT' ;
                            HrDescr := 'Record is only at DNS zone root.';
                          end;
    LongInt($000025EF)  : begin
                            HrStr := 'DNS_ERROR_RECORD_ALREADY_EXISTS' ;
                            HrDescr := 'DNS record already exists.';
                          end;
    LongInt($000025F0)  : begin
                            HrStr := 'DNS_ERROR_SECONDARY_DATA' ;
                            HrDescr := 'Secondary DNS zone data error.';
                          end;
    LongInt($000025F1)  : begin
                            HrStr := 'DNS_ERROR_NO_CREATE_CACHE_DATA' ;
                            HrDescr := 'Could not create DNS cache data.';
                          end;
    LongInt($000025F2)  : begin
                            HrStr := 'DNS_ERROR_NAME_DOES_NOT_EXIST' ;
                            HrDescr := 'DNS name does not exist.';
                          end;
    LongInt($000025F3)  : begin
                            HrStr := 'DNS_WARNING_PTR_CREATE_FAILED' ;
                            HrDescr := 'Could not create pointer (PTR) record.';
                          end;
    LongInt($000025F4)  : begin
                            HrStr := 'DNS_WARNING_DOMAIN_UNDELETED' ;
                            HrDescr := 'DNS domain was undeleted.';
                          end;
    LongInt($000025F5)  : begin
                            HrStr := 'DNS_ERROR_DS_UNAVAILABLE' ;
                            HrDescr := 'The directory service is unavailable.';
                          end;
    LongInt($000025F6)  : begin
                            HrStr := 'DNS_ERROR_DS_ZONE_ALREADY_EXISTS' ;
                            HrDescr := 'DNS zone already exists in the directory service.';
                          end;
    LongInt($000025F7)  : begin
                            HrStr := 'DNS_ERROR_NO_BOOTFILE_IF_DS_ZONE' ;
                            HrDescr := 'DNS server not creating or reading the boot file for the directory service integrated DNS zone.';
                          end;
    LongInt($00002617)  : begin
                            HrStr := 'DNS_INFO_AXFR_COMPLETE' ;
                            HrDescr := 'DNS AXFR (zone transfer) complete.';
                          end;
    LongInt($00002618)  : begin
                            HrStr := 'DNS_ERROR_AXFR' ;
                            HrDescr := 'DNS zone transfer failed.';
                          end;
    LongInt($00002619)  : begin
                            HrStr := 'DNS_INFO_ADDED_LOCAL_WINS' ;
                            HrDescr := 'Added local WINS server.';
                          end;
    LongInt($00002649)  : begin
                            HrStr := 'DNS_STATUS_CONTINUE_NEEDED' ;
                            HrDescr := 'Secure update call needs to continue update request.';
                          end;
    LongInt($0000267B)  : begin
                            HrStr := 'DNS_ERROR_NO_TCPIP' ;
                            HrDescr := 'TCP/IP network protocol not installed.';
                          end;
    LongInt($0000267C)  : begin
                            HrStr := 'DNS_ERROR_NO_DNS_SERVERS' ;
                            HrDescr := 'No DNS servers configured for local system.';
                          end;
    LongInt($000026AD)  : begin
                            HrStr := 'DNS_ERROR_DP_DOES_NOT_EXIST' ;
                            HrDescr := 'The specified directory partition does not exist.';
                          end;
    LongInt($000026AE)  : begin
                            HrStr := 'DNS_ERROR_DP_ALREADY_EXISTS' ;
                            HrDescr := 'The specified directory partition already exists.';
                          end;
    LongInt($000026AF)  : begin
                            HrStr := 'DNS_ERROR_DP_NOT_ENLISTED' ;
                            HrDescr := 'This DNS server is not enlisted in the specified directory partition.';
                          end;
    LongInt($000026B0)  : begin
                            HrStr := 'DNS_ERROR_DP_ALREADY_ENLISTED' ;
                            HrDescr := 'This DNS server is already enlisted in the specified directory partition.';
                          end;
    LongInt($000026B1)  : begin
                            HrStr := 'DNS_ERROR_DP_NOT_AVAILABLE' ;
                            HrDescr := 'The directory partition is not available at this time.' +
                                       'Wait a few minutes and try again.';
                          end;
    LongInt($000026B2)  : begin
                            HrStr := 'DNS_ERROR_DP_FSMO_ERROR' ;
                            HrDescr := 'The application directory partition operation failed.' +
                                       'The domain controller holding the domain naming master role is down or unable to service the request or is not running Windows Server 2003.';
                          end;
    LongInt($00002714)  : begin
                            HrStr := 'WSAEINTR' ;
                            HrDescr := 'A blocking operation was interrupted by a call to WSACancelBlockingCall.';
                          end;
    LongInt($00002719)  : begin
                            HrStr := 'WSAEBADF' ;
                            HrDescr := 'The file handle supplied is not valid.';
                          end;
    LongInt($0000271D)  : begin
                            HrStr := 'WSAEACCES' ;
                            HrDescr := 'An attempt was made to access a socket in a way forbidden by its access permissions.';
                          end;
    LongInt($0000271E)  : begin
                            HrStr := 'WSAEFAULT' ;
                            HrDescr := 'The system detected an invalid pointer address in attempting to use a pointer argument in a call.';
                          end;
    LongInt($00002726)  : begin
                            HrStr := 'WSAEINVAL' ;
                            HrDescr := 'An invalid argument was supplied.';
                          end;
    LongInt($00002728)  : begin
                            HrStr := 'WSAEMFILE' ;
                            HrDescr := 'Too many open sockets.';
                          end;
    LongInt($00002733)  : begin
                            HrStr := 'WSAEWOULDBLOCK' ;
                            HrDescr := 'A nonblocking socket operation could not be completed immediately.';
                          end;
    LongInt($00002734)  : begin
                            HrStr := 'WSAEINPROGRESS' ;
                            HrDescr := 'A blocking operation is currently executing.';
                          end;
    LongInt($00002735)  : begin
                            HrStr := 'WSAEALREADY' ;
                            HrDescr := 'An operation was attempted on a nonblocking socket that already had an operation in progress.';
                          end;
    LongInt($00002736)  : begin
                            HrStr := 'WSAENOTSOCK' ;
                            HrDescr := 'An operation was attempted on something that is not a socket.';
                          end;
    LongInt($00002737)  : begin
                            HrStr := 'WSAEDESTADDRREQ' ;
                            HrDescr := 'A required address was omitted from an operation on a socket.';
                          end;
    LongInt($00002738)  : begin
                            HrStr := 'WSAEMSGSIZE' ;
                            HrDescr := 'A message sent on a datagram socket was larger than the internal message buffer or some other network limit, or the buffer used to receive a datagram into was smaller than the datagram itself.';
                          end;
    LongInt($00002739)  : begin
                            HrStr := 'WSAEPROTOTYPE' ;
                            HrDescr := 'A protocol was specified in the socket function call that does not support the semantics of the socket type requested.';
                          end;
    LongInt($0000273A)  : begin
                            HrStr := 'WSAENOPROTOOPT' ;
                            HrDescr := 'An unknown, invalid, or unsupported option or level was specified in a getsockopt or setsockopt call.';
                          end;
    LongInt($0000273B)  : begin
                            HrStr := 'WSAEPROTONOSUPPORT' ;
                            HrDescr := 'The requested protocol has not been configured into the system, or no implementation for it exists.';
                          end;
    LongInt($0000273C)  : begin
                            HrStr := 'WSAESOCKTNOSUPPORT' ;
                            HrDescr := 'The support for the specified socket type does not exist in this address family.';
                          end;
    LongInt($0000273D)  : begin
                            HrStr := 'WSAEOPNOTSUPP' ;
                            HrDescr := 'The attempted operation is not supported for the type of object referenced.';
                          end;
    LongInt($0000273E)  : begin
                            HrStr := 'WSAEPFNOSUPPORT' ;
                            HrDescr := 'The protocol family has not been configured into the system or no implementation for it exists.';
                          end;
    LongInt($0000273F)  : begin
                            HrStr := 'WSAEAFNOSUPPORT' ;
                            HrDescr := 'An address incompatible with the requested protocol was used.';
                          end;
    LongInt($00002740)  : begin
                            HrStr := 'WSAEADDRINUSE' ;
                            HrDescr := 'Only one usage of each socket address (protocol/network address/port) is normally permitted.';
                          end;
    LongInt($00002741)  : begin
                            HrStr := 'WSAEADDRNOTAVAIL' ;
                            HrDescr := 'The requested address is not valid in its context.';
                          end;
    LongInt($00002742)  : begin
                            HrStr := 'WSAENETDOWN' ;
                            HrDescr := 'A socket operation encountered a dead network.';
                          end;
    LongInt($00002743)  : begin
                            HrStr := 'WSAENETUNREACH' ;
                            HrDescr := 'A socket operation was attempted to an unreachable network.';
                          end;
    LongInt($00002744)  : begin
                            HrStr := 'WSAENETRESET' ;
                            HrDescr := 'The connection has been broken due to keep-alive activity detecting a failure while the operation was in progress.';
                          end;
    LongInt($00002745)  : begin
                            HrStr := 'WSAECONNABORTED' ;
                            HrDescr := 'An established connection was aborted by the software in your host machine.';
                          end;
    LongInt($00002746)  : begin
                            HrStr := 'WSAECONNRESET' ;
                            HrDescr := 'An existing connection was forcibly closed by the remote host.';
                          end;
    LongInt($00002747)  : begin
                            HrStr := 'WSAENOBUFS' ;
                            HrDescr := 'An operation on a socket could not be performed because the system lacked sufficient buffer space or because a queue was full.';
                          end;
    LongInt($00002748)  : begin
                            HrStr := 'WSAEISCONN' ;
                            HrDescr := 'A connect request was made on an already connected socket.';
                          end;
    LongInt($00002749)  : begin
                            HrStr := 'WSAENOTCONN' ;
                            HrDescr := 'A request to send or receive data was disallowed because the socket is not connected and (when sending on a datagram socket using a sendto call) no address was supplied.';
                          end;
    LongInt($0000274A)  : begin
                            HrStr := 'WSAESHUTDOWN' ;
                            HrDescr := 'A request to send or receive data was disallowed because the socket had already been shut down in that direction with a previous shutdown call.';
                          end;
    LongInt($0000274B)  : begin
                            HrStr := 'WSAETOOMANYREFS' ;
                            HrDescr := 'Too many references to a kernel object.';
                          end;
    LongInt($0000274C)  : begin
                            HrStr := 'WSAETIMEDOUT' ;
                            HrDescr := 'A connection attempt failed because the connected party did not properly respond after a period of time, or the established connection failed because the connected host failed to respond.';
                          end;
    LongInt($0000274D)  : begin
                            HrStr := 'WSAECONNREFUSED' ;
                            HrDescr := 'No connection could be made because the target machine actively refused it.';
                          end;
    LongInt($0000274E)  : begin
                            HrStr := 'WSAELOOP' ;
                            HrDescr := 'Cannot translate name.';
                          end;
    LongInt($0000274F)  : begin
                            HrStr := 'WSAENAMETOOLONG' ;
                            HrDescr := 'Name or name component was too long.';
                          end;
    LongInt($00002750)  : begin
                            HrStr := 'WSAEHOSTDOWN' ;
                            HrDescr := 'A socket operation failed because the destination host was down.';
                          end;
    LongInt($00002751)  : begin
                            HrStr := 'WSAEHOSTUNREACH' ;
                            HrDescr := 'A socket operation was attempted to an unreachable host.';
                          end;
    LongInt($00002752)  : begin
                            HrStr := 'WSAENOTEMPTY' ;
                            HrDescr := 'Cannot remove a directory that is not empty.';
                          end;
    LongInt($00002753)  : begin
                            HrStr := 'WSAEPROCLIM' ;
                            HrDescr := 'A Windows Sockets implementation might have a limit on the number of applications that can use it simultaneously.';
                          end;
    LongInt($00002754)  : begin
                            HrStr := 'WSAEUSERS' ;
                            HrDescr := 'Ran out of quota.';
                          end;
    LongInt($00002755)  : begin
                            HrStr := 'WSAEDQUOT' ;
                            HrDescr := 'Ran out of disk quota.';
                          end;
    LongInt($00002756)  : begin
                            HrStr := 'WSAESTALE' ;
                            HrDescr := 'File handle reference is no longer available.';
                          end;
    LongInt($00002757)  : begin
                            HrStr := 'WSAEREMOTE' ;
                            HrDescr := 'Item is not available locally.';
                          end;
    LongInt($0000276B)  : begin
                            HrStr := 'WSASYSNOTREADY' ;
                            HrDescr := 'WSAStartup cannot function at this time because the underlying system it uses to provide network services is currently unavailable.';
                          end;
    LongInt($0000276C)  : begin
                            HrStr := 'WSAVERNOTSUPPORTED' ;
                            HrDescr := 'The Windows Sockets version requested is not supported.';
                          end;
    LongInt($0000276D)  : begin
                            HrStr := 'WSANOTINITIALISED' ;
                            HrDescr := 'Either the application has not called WSAStartup, or WSAStartup failed.';
                          end;
    LongInt($00002775)  : begin
                            HrStr := 'WSAEDISCON' ;
                            HrDescr := 'Returned by WSARecv or WSARecvFrom to indicate that the remote party has initiated a graceful shutdown sequence.';
                          end;
    LongInt($00002776)  : begin
                            HrStr := 'WSAENOMORE' ;
                            HrDescr := 'No more results can be returned by WSALookupServiceNext.';
                          end;
    LongInt($00002777)  : begin
                            HrStr := 'WSAECANCELLED' ;
                            HrDescr := 'A call to WSALookupServiceEnd was made while this call was still processing.' +
                                       'The call has been canceled.';
                          end;
    LongInt($00002778)  : begin
                            HrStr := 'WSAEINVALIDPROCTABLE' ;
                            HrDescr := 'The procedure call table is invalid.';
                          end;
    LongInt($00002779)  : begin
                            HrStr := 'WSAEINVALIDPROVIDER' ;
                            HrDescr := 'The requested service provider is invalid.';
                          end;
    LongInt($0000277A)  : begin
                            HrStr := 'WSAEPROVIDERFAILEDINIT' ;
                            HrDescr := 'The requested service provider could not be loaded or initialized.';
                          end;
    LongInt($0000277B)  : begin
                            HrStr := 'WSASYSCALLFAILURE' ;
                            HrDescr := 'A system call that should never fail has failed.';
                          end;
    LongInt($0000277C)  : begin
                            HrStr := 'WSASERVICE_NOT_FOUND' ;
                            HrDescr := 'No such service is known.' +
                                       'The service cannot be found in the specified namespace.';
                          end;
    LongInt($0000277D)  : begin
                            HrStr := 'WSATYPE_NOT_FOUND' ;
                            HrDescr := 'The specified class was not found.';
                          end;
    LongInt($0000277E)  : begin
                            HrStr := 'WSA_E_NO_MORE' ;
                            HrDescr := 'No more results can be returned by WSALookupServiceNext.';
                          end;
    LongInt($0000277F)  : begin
                            HrStr := 'WSA_E_CANCELLED' ;
                            HrDescr := 'A call to WSALookupServiceEnd was made while this call was still processing.' +
                                       'The call has been canceled.';
                          end;
    LongInt($00002780)  : begin
                            HrStr := 'WSAEREFUSED' ;
                            HrDescr := 'A database query failed because it was actively refused.';
                          end;
    LongInt($00002AF9)  : begin
                            HrStr := 'WSAHOST_NOT_FOUND' ;
                            HrDescr := 'No such host is known.';
                          end;
    LongInt($00002AFA)  : begin
                            HrStr := 'WSATRY_AGAIN' ;
                            HrDescr := 'This is usually a temporary error during host name resolution and means that the local server did not receive a response from an authoritative server.';
                          end;
    LongInt($00002AFB)  : begin
                            HrStr := 'WSANO_RECOVERY' ;
                            HrDescr := 'A nonrecoverable error occurred during a database lookup.';
                          end;
    LongInt($00002AFC)  : begin
                            HrStr := 'WSANO_DATA' ;
                            HrDescr := 'The requested name is valid, but no data of the requested type was found.';
                          end;
    LongInt($00002AFD)  : begin
                            HrStr := 'WSA_QOS_RECEIVERS' ;
                            HrDescr := 'At least one reserve has arrived.';
                          end;
    LongInt($00002AFE)  : begin
                            HrStr := 'WSA_QOS_SENDERS' ;
                            HrDescr := 'At least one path has arrived.';
                          end;
    LongInt($00002AFF)  : begin
                            HrStr := 'WSA_QOS_NO_SENDERS' ;
                            HrDescr := 'There are no senders.';
                          end;
    LongInt($00002B00)  : begin
                            HrStr := 'WSA_QOS_NO_RECEIVERS' ;
                            HrDescr := 'There are no receivers.';
                          end;
    LongInt($00002B01)  : begin
                            HrStr := 'WSA_QOS_REQUEST_CONFIRMED' ;
                            HrDescr := 'Reserve has been confirmed.';
                          end;
    LongInt($00002B02)  : begin
                            HrStr := 'WSA_QOS_ADMISSION_FAILURE' ;
                            HrDescr := 'Error due to lack of resources.';
                          end;
    LongInt($00002B03)  : begin
                            HrStr := 'WSA_QOS_POLICY_FAILURE' ;
                            HrDescr := 'Rejected for administrative reasonsùbad credentials.';
                          end;
    LongInt($00002B04)  : begin
                            HrStr := 'WSA_QOS_BAD_STYLE' ;
                            HrDescr := 'Unknown or conflicting style.';
                          end;
    LongInt($00002B05)  : begin
                            HrStr := 'WSA_QOS_BAD_OBJECT' ;
                            HrDescr := 'There is a problem with some part of the filterspec or provider-specific buffer in general.';
                          end;
    LongInt($00002B06)  : begin
                            HrStr := 'WSA_QOS_TRAFFIC_CTRL_ERROR' ;
                            HrDescr := 'There is a problem with some part of the flowspec.';
                          end;
    LongInt($00002B07)  : begin
                            HrStr := 'WSA_QOS_GENERIC_ERROR' ;
                            HrDescr := 'General quality of serve (QOS) error.';
                          end;
    LongInt($00002B08)  : begin
                            HrStr := 'WSA_QOS_ESERVICETYPE' ;
                            HrDescr := 'An invalid or unrecognized service type was found in the flowspec.';
                          end;
    LongInt($00002B09)  : begin
                            HrStr := 'WSA_QOS_EFLOWSPEC' ;
                            HrDescr := 'An invalid or inconsistent flowspec was found in the QOS structure.';
                          end;
    LongInt($00002B0A)  : begin
                            HrStr := 'WSA_QOS_EPROVSPECBUF' ;
                            HrDescr := 'Invalid QOS provider-specific buffer.';
                          end;
    LongInt($00002B0B)  : begin
                            HrStr := 'WSA_QOS_EFILTERSTYLE' ;
                            HrDescr := 'An invalid QOS filter style was used.';
                          end;
    LongInt($00002B0C)  : begin
                            HrStr := 'WSA_QOS_EFILTERTYPE' ;
                            HrDescr := 'An invalid QOS filter type was used.';
                          end;
    LongInt($00002B0D)  : begin
                            HrStr := 'WSA_QOS_EFILTERCOUNT' ;
                            HrDescr := 'An incorrect number of QOS FILTERSPECs were specified in the FLOWDESCRIPTOR.';
                          end;
    LongInt($00002B0E)  : begin
                            HrStr := 'WSA_QOS_EOBJLENGTH' ;
                            HrDescr := 'An object with an invalid ObjectLength field was specified in the QOS provider-specific buffer.';
                          end;
    LongInt($00002B0F)  : begin
                            HrStr := 'WSA_QOS_EFLOWCOUNT' ;
                            HrDescr := 'An incorrect number of flow descriptors was specified in the QOS structure.';
                          end;
    LongInt($00002B10)  : begin
                            HrStr := 'WSA_QOS_EUNKOWNPSOBJ' ;
                            HrDescr := 'An unrecognized object was found in the QOS provider-specific buffer.';
                          end;
    LongInt($00002B11)  : begin
                            HrStr := 'WSA_QOS_EPOLICYOBJ' ;
                            HrDescr := 'An invalid policy object was found in the QOS provider-specific buffer.';
                          end;
    LongInt($00002B12)  : begin
                            HrStr := 'WSA_QOS_EFLOWDESC' ;
                            HrDescr := 'An invalid QOS flow descriptor was found in the flow descriptor list.';
                          end;
    LongInt($00002B13)  : begin
                            HrStr := 'WSA_QOS_EPSFLOWSPEC' ;
                            HrDescr := 'An invalid or inconsistent flowspec was found in the QOS provider-specific buffer.';
                          end;
    LongInt($00002B14)  : begin
                            HrStr := 'WSA_QOS_EPSFILTERSPEC' ;
                            HrDescr := 'An invalid FILTERSPEC was found in the QOS provider-specific buffer.';
                          end;
    LongInt($00002B15)  : begin
                            HrStr := 'WSA_QOS_ESDMODEOBJ' ;
                            HrDescr := 'An invalid shape discard mode object was found in the QOS provider-specific buffer.';
                          end;
    LongInt($00002B16)  : begin
                            HrStr := 'WSA_QOS_ESHAPERATEOBJ' ;
                            HrDescr := 'An invalid shaping rate object was found in the QOS provider-specific buffer.';
                          end;
    LongInt($00002B17)  : begin
                            HrStr := 'WSA_QOS_RESERVED_PETYPE' ;
                            HrDescr := 'A reserved policy element was found in the QOS provider-specific buffer.';
                          end;
    LongInt($000032C8)  : begin
                            HrStr := 'ERROR_IPSEC_QM_POLICY_EXISTS' ;
                            HrDescr := 'The specified quick mode policy already exists.';
                          end;
    LongInt($000032C9)  : begin
                            HrStr := 'ERROR_IPSEC_QM_POLICY_NOT_FOUND' ;
                            HrDescr := 'The specified quick mode policy was not found.';
                          end;
    LongInt($000032CA)  : begin
                            HrStr := 'ERROR_IPSEC_QM_POLICY_IN_USE' ;
                            HrDescr := 'The specified quick mode policy is being used.';
                          end;
    LongInt($000032CB)  : begin
                            HrStr := 'ERROR_IPSEC_MM_POLICY_EXISTS' ;
                            HrDescr := 'The specified main mode policy already exists.';
                          end;
    LongInt($000032CC)  : begin
                            HrStr := 'ERROR_IPSEC_MM_POLICY_NOT_FOUND' ;
                            HrDescr := 'The specified main mode policy was not found.';
                          end;
    LongInt($000032CD)  : begin
                            HrStr := 'ERROR_IPSEC_MM_POLICY_IN_USE' ;
                            HrDescr := 'The specified main mode policy is being used.';
                          end;
    LongInt($000032CE)  : begin
                            HrStr := 'ERROR_IPSEC_MM_FILTER_EXISTS' ;
                            HrDescr := 'The specified main mode filter already exists.';
                          end;
    LongInt($000032CF)  : begin
                            HrStr := 'ERROR_IPSEC_MM_FILTER_NOT_FOUND' ;
                            HrDescr := 'The specified main mode filter was not found.';
                          end;
    LongInt($000032D0)  : begin
                            HrStr := 'ERROR_IPSEC_TRANSPORT_FILTER_EXISTS' ;
                            HrDescr := 'The specified transport mode filter already exists.';
                          end;
    LongInt($000032D1)  : begin
                            HrStr := 'ERROR_IPSEC_TRANSPORT_FILTER_NOT_FOUND' ;
                            HrDescr := 'The specified transport mode filter does not exist.';
                          end;
    LongInt($000032D2)  : begin
                            HrStr := 'ERROR_IPSEC_MM_AUTH_EXISTS' ;
                            HrDescr := 'The specified main mode authentication list exists.';
                          end;
    LongInt($000032D3)  : begin
                            HrStr := 'ERROR_IPSEC_MM_AUTH_NOT_FOUND' ;
                            HrDescr := 'The specified main mode authentication list was not found.';
                          end;
    LongInt($000032D4)  : begin
                            HrStr := 'ERROR_IPSEC_MM_AUTH_IN_USE' ;
                            HrDescr := 'The specified main mode authentication list is being used.';
                          end;
    LongInt($000032D5)  : begin
                            HrStr := 'ERROR_IPSEC_DEFAULT_MM_POLICY_NOT_FOUND' ;
                            HrDescr := 'The specified default main mode policy was not found.';
                          end;
    LongInt($000032D6)  : begin
                            HrStr := 'ERROR_IPSEC_DEFAULT_MM_AUTH_NOT_FOUND' ;
                            HrDescr := 'The specified default main mode authentication list was not found.';
                          end;
    LongInt($000032D7)  : begin
                            HrStr := 'ERROR_IPSEC_DEFAULT_QM_POLICY_NOT_FOUND' ;
                            HrDescr := 'The specified default quick mode policy was not found.';
                          end;
    LongInt($000032D8)  : begin
                            HrStr := 'ERROR_IPSEC_TUNNEL_FILTER_EXISTS' ;
                            HrDescr := 'The specified tunnel mode filter exists.';
                          end;
    LongInt($000032D9)  : begin
                            HrStr := 'ERROR_IPSEC_TUNNEL_FILTER_NOT_FOUND' ;
                            HrDescr := 'The specified tunnel mode filter was not found.';
                          end;
    LongInt($000032DA)  : begin
                            HrStr := 'ERROR_IPSEC_MM_FILTER_PENDING_DELETION' ;
                            HrDescr := 'The main mode filter is pending deletion.';
                          end;
    LongInt($000032DB)  : begin
                            HrStr := 'ERROR_IPSEC_TRANSPORT_FILTER_ENDING_DELETION' ;
                            HrDescr := 'The transport filter is pending deletion.';
                          end;
    LongInt($000032DC)  : begin
                            HrStr := 'ERROR_IPSEC_TUNNEL_FILTER_PENDING_DELETION' ;
                            HrDescr := 'The tunnel filter is pending deletion.';
                          end;
    LongInt($000032DD)  : begin
                            HrStr := 'ERROR_IPSEC_MM_POLICY_PENDING_ELETION' ;
                            HrDescr := 'The main mode policy is pending deletion.';
                          end;
    LongInt($000032DE)  : begin
                            HrStr := 'ERROR_IPSEC_MM_AUTH_PENDING_DELETION' ;
                            HrDescr := 'The main mode authentication bundle is pending deletion.';
                          end;
    LongInt($000032DF)  : begin
                            HrStr := 'ERROR_IPSEC_QM_POLICY_PENDING_DELETION' ;
                            HrDescr := 'The quick mode policy is pending deletion.';
                          end;
    LongInt($000032E0)  : begin
                            HrStr := 'WARNING_IPSEC_MM_POLICY_PRUNED' ;
                            HrDescr := 'The main mode policy was successfully added, but some of the requested offers are not supported.';
                          end;
    LongInt($000032E1)  : begin
                            HrStr := 'WARNING_IPSEC_QM_POLICY_PRUNED' ;
                            HrDescr := 'The quick mode policy was successfully added, but some of the requested offers are not supported.';
                          end;
    LongInt($000035E8)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NEG_STATUS_BEGIN' ;
                            HrDescr := 'Starts the list of frequencies of various IKE Win32 error codes encountered during negotiations.';
                          end;
    LongInt($000035E9)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_AUTH_FAIL' ;
                            HrDescr := 'The IKE authentication credentials are unacceptable.';
                          end;
    LongInt($000035EA)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_ATTRIB_FAIL' ;
                            HrDescr := 'The IKE security attributes are unacceptable.';
                          end;
    LongInt($000035EB)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NEGOTIATION_PENDING' ;
                            HrDescr := 'The IKE negotiation is in progress.';
                          end;
    LongInt($000035EC)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_GENERAL_PROCESSING_ERROR' ;
                            HrDescr := 'General processing error.';
                          end;
    LongInt($000035ED)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_TIMED_OUT' ;
                            HrDescr := 'Negotiation timed out.';
                          end;
    LongInt($000035EE)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NO_CERT' ;
                            HrDescr := 'The IKE failed to find a valid machine certificate.' +
                                       'Contact your network security administrator about installing a valid certificate in the appropriate certificate store.';
                          end;
    LongInt($000035EF)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_SA_DELETED' ;
                            HrDescr := 'The IKE security association (SA) was deleted by a peer before it was completely established.';
                          end;
    LongInt($000035F0)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_SA_REAPED' ;
                            HrDescr := 'The IKE SA was deleted before it was completely established.';
                          end;
    LongInt($000035F1)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_MM_ACQUIRE_DROP' ;
                            HrDescr := 'The negotiation request sat in the queue too long.';
                          end;
    LongInt($000035F2)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_QM_ACQUIRE_DROP' ;
                            HrDescr := 'The negotiation request sat in the queue too long.';
                          end;
    LongInt($000035F3)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_QUEUE_DROP_MM' ;
                            HrDescr := 'The negotiation request sat in the queue too long.';
                          end;
    LongInt($000035F4)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_QUEUE_DROP_NO_MM' ;
                            HrDescr := 'The negotiation request sat in the queue too long.';
                          end;
    LongInt($000035F5)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_DROP_NO_RESPONSE' ;
                            HrDescr := 'There was no response from a peer.';
                          end;
    LongInt($000035F6)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_MM_DELAY_DROP' ;
                            HrDescr := 'The negotiation took too long.';
                          end;
    LongInt($000035F7)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_QM_DELAY_DROP' ;
                            HrDescr := 'The negotiation took too long.';
                          end;
    LongInt($000035F8)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_ERROR' ;
                            HrDescr := 'An unknown error occurred.';
                          end;
    LongInt($000035F9)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_CRL_FAILED' ;
                            HrDescr := 'The certificate revocation check failed.';
                          end;
    LongInt($000035FA)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_KEY_USAGE' ;
                            HrDescr := 'Invalid certificate key usage.';
                          end;
    LongInt($000035FB)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_CERT_TYPE' ;
                            HrDescr := 'Invalid certificate type.';
                          end;
    LongInt($000035FC)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NO_PRIVATE_KEY' ;
                            HrDescr := 'The IKE negotiation failed because the machine certificate used does not have a private key.' +
                                       'IPsec certificates require a private key.' +
                                       'Contact your network security administrator about a certificate that has a private key.';
                          end;
    LongInt($000035FE)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_DH_FAIL' ;
                            HrDescr := 'There was a failure in the Diffie-Hellman computation.';
                          end;
    LongInt($00003600)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_HEADER' ;
                            HrDescr := 'Invalid header.';
                          end;
    LongInt($00003601)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NO_POLICY' ;
                            HrDescr := 'No policy configured.';
                          end;
    LongInt($00003602)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_SIGNATURE' ;
                            HrDescr := 'Failed to verify signature.';
                          end;
    LongInt($00003603)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_KERBEROS_ERROR' ;
                            HrDescr := 'Failed to authenticate using Kerberos.';
                          end;
    LongInt($00003604)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NO_PUBLIC_KEY' ;
                            HrDescr := 'The peer''s certificate did not have a public key.';
                          end;
    LongInt($00003605)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR' ;
                            HrDescr := 'Error processing the error payload.';
                          end;
    LongInt($00003606)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_SA' ;
                            HrDescr := 'Error processing the SA payload.';
                          end;
    LongInt($00003607)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_PROP' ;
                            HrDescr := 'Error processing the proposal payload.';
                          end;
    LongInt($00003608)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_TRANS' ;
                            HrDescr := 'Error processing the transform payload.';
                          end;
    LongInt($00003609)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_KE' ;
                            HrDescr := 'Error processing the key exchange payload.';
                          end;
    LongInt($0000360A)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_ID' ;
                            HrDescr := 'Error processing the ID payload.';
                          end;
    LongInt($0000360B)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_CERT' ;
                            HrDescr := 'Error processing the certification payload.';
                          end;
    LongInt($0000360C)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_CERT_REQ' ;
                            HrDescr := 'Error processing the certificate request payload.';
                          end;
    LongInt($0000360D)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_HASH' ;
                            HrDescr := 'Error processing the hash payload.';
                          end;
    LongInt($0000360E)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_SIG' ;
                            HrDescr := 'Error processing the signature payload.';
                          end;
    LongInt($0000360F)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_NONCE' ;
                            HrDescr := 'Error processing the nonce payload.';
                          end;
    LongInt($00003610)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_NOTIFY' ;
                            HrDescr := 'Error processing the notify payload.';
                          end;
    LongInt($00003611)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_DELETE' ;
                            HrDescr := 'Error processing the delete payload.';
                          end;
    LongInt($00003612)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_VENDOR' ;
                            HrDescr := 'Error processing the VendorId payload.';
                          end;
    LongInt($00003613)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_PAYLOAD' ;
                            HrDescr := 'Invalid payload received.';
                          end;
    LongInt($00003614)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_LOAD_SOFT_SA' ;
                            HrDescr := 'Soft SA loaded.';
                          end;
    LongInt($00003615)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_SOFT_SA_TORN_DOWN' ;
                            HrDescr := 'Soft SA torn down.';
                          end;
    LongInt($00003616)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_COOKIE' ;
                            HrDescr := 'Invalid cookie received.';
                          end;
    LongInt($00003617)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NO_PEER_CERT' ;
                            HrDescr := 'Peer failed to send valid machine certificate.';
                          end;
    LongInt($00003618)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PEER_CRL_FAILED' ;
                            HrDescr := 'Certification revocation check of peer''s certificate failed.';
                          end;
    LongInt($00003619)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_POLICY_CHANGE' ;
                            HrDescr := 'New policy invalidated SAs formed with the old policy.';
                          end;
    LongInt($0000361A)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NO_MM_POLICY' ;
                            HrDescr := 'There is no available main mode IKE policy.';
                          end;
    LongInt($0000361B)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NOTCBPRIV' ;
                            HrDescr := 'Failed to enabled trusted computer base (TCB) privilege.';
                          end;
    LongInt($0000361C)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_SECLOADFAIL' ;
                            HrDescr := 'Failed to load SECURITY.DLL.';
                          end;
    LongInt($0000361D)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_FAILSSPINIT' ;
                            HrDescr := 'Failed to obtain the security function table dispatch address from the SSPI.';
                          end;
    LongInt($0000361E)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_FAILQUERYSSP' ;
                            HrDescr := 'Failed to query the Kerberos package to obtain the max token size.';
                          end;
    LongInt($0000361F)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_SRVACQFAIL' ;
                            HrDescr := 'Failed to obtain the Kerberos server credentials for the Internet Security Association and Key Management Protocol (ISAKMP)/ERROR_IPSEC_IKE service.' +
                                       'Kerberos authentication will not function.' +
                                       'The most likely reason for this is lack of domain membership.' +
                                       'This is normal if your computer is a member of a workgroup.';
                          end;
    LongInt($00003620)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_SRVQUERYCRED' ;
                            HrDescr := 'Failed to determine the SSPI principal name for ISAKMP/ERROR_IPSEC_IKE service (QueryCredentialsAttributes).';
                          end;
    LongInt($00003621)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_GETSPIFAIL' ;
                            HrDescr := 'Failed to obtain a new service provider interface (SPI) for the inbound SA from the IPsec driver.' +
                                       'The most common cause for this is that the driver does not have the correct filter.' +
                                       'Check your policy to verify the filters.';
                          end;
    LongInt($00003622)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_FILTER' ;
                            HrDescr := 'Given filter is invalid.';
                          end;
    LongInt($00003623)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_OUT_OF_MEMORY' ;
                            HrDescr := 'Memory allocation failed.';
                          end;
    LongInt($00003624)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_ADD_UPDATE_KEY_FAILED' ;
                            HrDescr := 'Failed to add an SA to the IPSec driver.' +
                                       'The most common cause for this is if the IKE negotiation took too long to complete.' +
                                       'If the problem persists, reduce the load on the faulting machine.';
                          end;
    LongInt($00003625)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_POLICY' ;
                            HrDescr := 'Invalid policy.';
                          end;
    LongInt($00003626)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_UNKNOWN_DOI' ;
                            HrDescr := 'Invalid digital object identifier (DOI).';
                          end;
    LongInt($00003627)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_SITUATION' ;
                            HrDescr := 'Invalid situation.';
                          end;
    LongInt($00003628)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_DH_FAILURE' ;
                            HrDescr := 'Diffie-Hellman failure.';
                          end;
    LongInt($00003629)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_GROUP' ;
                            HrDescr := 'Invalid Diffie-Hellman group.';
                          end;
    LongInt($0000362A)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_ENCRYPT' ;
                            HrDescr := 'Error encrypting payload.';
                          end;
    LongInt($0000362B)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_DECRYPT' ;
                            HrDescr := 'Error decrypting payload.';
                          end;
    LongInt($0000362C)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_POLICY_MATCH' ;
                            HrDescr := 'Policy match error.';
                          end;
    LongInt($0000362D)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_UNSUPPORTED_ID' ;
                            HrDescr := 'Unsupported ID.';
                          end;
    LongInt($0000362E)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_HASH' ;
                            HrDescr := 'Hash verification failed.';
                          end;
    LongInt($0000362F)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_HASH_ALG' ;
                            HrDescr := 'Invalid hash algorithm.';
                          end;
    LongInt($00003630)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_HASH_SIZE' ;
                            HrDescr := 'Invalid hash size.';
                          end;
    LongInt($00003631)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_ENCRYPT_ALG' ;
                            HrDescr := 'Invalid encryption algorithm.';
                          end;
    LongInt($00003632)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_AUTH_ALG' ;
                            HrDescr := 'Invalid authentication algorithm.';
                          end;
    LongInt($00003633)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_SIG' ;
                            HrDescr := 'Invalid certificate signature.';
                          end;
    LongInt($00003634)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_LOAD_FAILED' ;
                            HrDescr := 'Load failed.';
                          end;
    LongInt($00003635)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_RPC_DELETE' ;
                            HrDescr := 'Deleted by using an RPC call.';
                          end;
    LongInt($00003636)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_BENIGN_REINIT' ;
                            HrDescr := 'A temporary state was created to perform reinitialization.' +
                                       'This is not a real failure.';
                          end;
    LongInt($00003637)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_RESPONDER_LIFETIME_NOTIFY' ;
                            HrDescr := 'The lifetime value received in the Responder Lifetime Notify is below the Windows 2000 configured minimum value.' +
                                       'Fix the policy on the peer machine.';
                          end;
    LongInt($00003639)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_CERT_KEYLEN' ;
                            HrDescr := 'Key length in the certificate is too small for configured security requirements.';
                          end;
    LongInt($0000363A)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_MM_LIMIT' ;
                            HrDescr := 'Maximum number of established MM SAs to peer exceeded.';
                          end;
    LongInt($0000363B)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NEGOTIATION_DISABLED' ;
                            HrDescr := 'The IKE received a policy that disables negotiation.';
                          end;
    LongInt($0000363C)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_QM_LIMIT' ;
                            HrDescr := 'Reached maximum quick mode limit for the main mode.' +
                                       'New main mode will be started.';
                          end;
    LongInt($0000363D)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_MM_EXPIRED' ;
                            HrDescr := 'Main mode SA lifetime expired or the peer sent a main mode delete.';
                          end;
    LongInt($0000363E)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PEER_MM_ASSUMED_INVALID' ;
                            HrDescr := 'Main mode SA assumed to be invalid because peer stopped responding.';
                          end;
    LongInt($0000363F)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_CERT_CHAIN_POLICY_MISMATCH' ;
                            HrDescr := 'Certificate does not chain to a trusted root in IPsec policy.';
                          end;
    LongInt($00003640)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_UNEXPECTED_MESSAGE_ID' ;
                            HrDescr := 'Received unexpected message ID.';
                          end;
    LongInt($00003641)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_UMATTS' ;
                            HrDescr := 'Received invalid AuthIP user mode attributes.';
                          end;
    LongInt($00003642)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_DOS_COOKIE_SENT' ;
                            HrDescr := 'Sent DOS cookie notify to initiator.';
                          end;
    LongInt($00003643)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_SHUTTING_DOWN' ;
                            HrDescr := 'The IKE service is shutting down.';
                          end;
    LongInt($00003644)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_CGA_AUTH_FAILED' ;
                            HrDescr := 'Could not verify the binding between the color graphics adapter (CGA) address and the certificate.';
                          end;
    LongInt($00003645)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_PROCESS_ERR_NATOA' ;
                            HrDescr := 'Error processing the NatOA payload.';
                          end;
    LongInt($00003646)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_INVALID_MM_FOR_QM' ;
                            HrDescr := 'The parameters of the main mode are invalid for this quick mode.';
                          end;
    LongInt($00003647)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_QM_EXPIRED' ;
                            HrDescr := 'The quick mode SA was expired by the IPsec driver.';
                          end;
    LongInt($00003648)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_TOO_MANY_FILTERS' ;
                            HrDescr := 'Too many dynamically added IKEEXT filters were detected.';
                          end;
    LongInt($00003649)  : begin
                            HrStr := 'ERROR_IPSEC_IKE_NEG_STATUS_END' ;
                            HrDescr := 'Ends the list of frequencies of various IKE Win32 error codes encountered during negotiations.';
                          end;
    LongInt($000036B0)  : begin
                            HrStr := 'ERROR_SXS_SECTION_NOT_FOUND' ;
                            HrDescr := 'The requested section was not present in the activation context.';
                          end;
    LongInt($000036B1)  : begin
                            HrStr := 'ERROR_SXS_CANT_GEN_ACTCTX' ;
                            HrDescr := 'The application has failed to start because its side-by-side configuration is incorrect.' +
                                       'See the application event log for more detail.';
                          end;
    LongInt($000036B2)  : begin
                            HrStr := 'ERROR_SXS_INVALID_ACTCTXDATA_FORMAT' ;
                            HrDescr := 'The application binding data format is invalid.';
                          end;
    LongInt($000036B3)  : begin
                            HrStr := 'ERROR_SXS_ASSEMBLY_NOT_FOUND' ;
                            HrDescr := 'The referenced assembly is not installed on your system.';
                          end;
    LongInt($000036B4)  : begin
                            HrStr := 'ERROR_SXS_MANIFEST_FORMAT_ERROR' ;
                            HrDescr := 'The manifest file does not begin with the required tag and format information.';
                          end;
    LongInt($000036B5)  : begin
                            HrStr := 'ERROR_SXS_MANIFEST_PARSE_ERROR' ;
                            HrDescr := 'The manifest file contains one or more syntax errors.';
                          end;
    LongInt($000036B6)  : begin
                            HrStr := 'ERROR_SXS_ACTIVATION_CONTEXT_DISABLED' ;
                            HrDescr := 'The application attempted to activate a disabled activation context.';
                          end;
    LongInt($000036B7)  : begin
                            HrStr := 'ERROR_SXS_KEY_NOT_FOUND' ;
                            HrDescr := 'The requested lookup key was not found in any active activation context.';
                          end;
    LongInt($000036B8)  : begin
                            HrStr := 'ERROR_SXS_VERSION_CONFLICT' ;
                            HrDescr := 'A component version required by the application conflicts with another active component version.';
                          end;
    LongInt($000036B9)  : begin
                            HrStr := 'ERROR_SXS_WRONG_SECTION_TYPE' ;
                            HrDescr := 'The type requested activation context section does not match the query API used.';
                          end;
    LongInt($000036BA)  : begin
                            HrStr := 'ERROR_SXS_THREAD_QUERIES_DISABLED' ;
                            HrDescr := 'Lack of system resources has required isolated activation to be disabled for the current thread of execution.';
                          end;
    LongInt($000036BB)  : begin
                            HrStr := 'ERROR_SXS_PROCESS_DEFAULT_ALREADY_SET' ;
                            HrDescr := 'An attempt to set the process default activation context failed because the process default activation context was already set.';
                          end;
    LongInt($000036BC)  : begin
                            HrStr := 'ERROR_SXS_UNKNOWN_ENCODING_GROUP' ;
                            HrDescr := 'The encoding group identifier specified is not recognized.';
                          end;
    LongInt($000036BD)  : begin
                            HrStr := 'ERROR_SXS_UNKNOWN_ENCODING' ;
                            HrDescr := 'The encoding requested is not recognized.';
                          end;
    LongInt($000036BE)  : begin
                            HrStr := 'ERROR_SXS_INVALID_XML_NAMESPACE_URI' ;
                            HrDescr := 'The manifest contains a reference to an invalid URI.';
                          end;
    LongInt($000036BF)  : begin
                            HrStr := 'ERROR_SXS_ROOT_MANIFEST_DEPENDENCY_OT_INSTALLED' ;
                            HrDescr := 'The application manifest contains a reference to a dependent assembly that is not installed.';
                          end;
    LongInt($000036C0)  : begin
                            HrStr := 'ERROR_SXS_LEAF_MANIFEST_DEPENDENCY_NOT_INSTALLED' ;
                            HrDescr := 'The manifest for an assembly used by the application has a reference to a dependent assembly that is not installed.';
                          end;
    LongInt($000036C1)  : begin
                            HrStr := 'ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE' ;
                            HrDescr := 'The manifest contains an attribute for the assembly identity that is not valid.';
                          end;
    LongInt($000036C2)  : begin
                            HrStr := 'ERROR_SXS_MANIFEST_MISSING_REQUIRED_DEFAULT_NAMESPACE' ;
                            HrDescr := 'The manifest is missing the required default namespace specification on the assembly element.';
                          end;
    LongInt($000036C3)  : begin
                            HrStr := 'ERROR_SXS_MANIFEST_INVALID_REQUIRED_DEFAULT_NAMESPACE' ;
                            HrDescr := 'The manifest has a default namespace specified on the assembly element but its value is not urn:schemas-microsoft-com:asm.v1"."';
                          end;
    LongInt($000036C4)  : begin
                            HrStr := 'ERROR_SXS_PRIVATE_MANIFEST_CROSS_PATH_WITH_REPARSE_POINT' ;
                            HrDescr := 'The private manifest probed has crossed the reparse-point-associated path.';
                          end;
    LongInt($000036C5)  : begin
                            HrStr := 'ERROR_SXS_DUPLICATE_DLL_NAME' ;
                            HrDescr := 'Two or more components referenced directly or indirectly by the application manifest have files by the same name.';
                          end;
    LongInt($000036C6)  : begin
                            HrStr := 'ERROR_SXS_DUPLICATE_WINDOWCLASS_NAME' ;
                            HrDescr := 'Two or more components referenced directly or indirectly by the application manifest have window classes with the same name.';
                          end;
    LongInt($000036C7)  : begin
                            HrStr := 'ERROR_SXS_DUPLICATE_CLSID' ;
                            HrDescr := 'Two or more components referenced directly or indirectly by the application manifest have the same COM server CLSIDs.';
                          end;
    LongInt($000036C8)  : begin
                            HrStr := 'ERROR_SXS_DUPLICATE_IID' ;
                            HrDescr := 'Two or more components referenced directly or indirectly by the application manifest have proxies for the same COM interface IIDs.';
                          end;
    LongInt($000036C9)  : begin
                            HrStr := 'ERROR_SXS_DUPLICATE_TLBID' ;
                            HrDescr := 'Two or more components referenced directly or indirectly by the application manifest have the same COM type library TLBIDs.';
                          end;
    LongInt($000036CA)  : begin
                            HrStr := 'ERROR_SXS_DUPLICATE_PROGID' ;
                            HrDescr := 'Two or more components referenced directly or indirectly by the application manifest have the same COM ProgIDs.';
                          end;
    LongInt($000036CB)  : begin
                            HrStr := 'ERROR_SXS_DUPLICATE_ASSEMBLY_NAME' ;
                            HrDescr := 'Two or more components referenced directly or indirectly by the application manifest are different versions of the same component, which is not permitted.';
                          end;
    LongInt($000036CC)  : begin
                            HrStr := 'ERROR_SXS_FILE_HASH_MISMATCH' ;
                            HrDescr := 'A component''s file does not match the verification information present in the component manifest.';
                          end;
    LongInt($000036CD)  : begin
                            HrStr := 'ERROR_SXS_POLICY_PARSE_ERROR' ;
                            HrDescr := 'The policy manifest contains one or more syntax errors.';
                          end;
    LongInt($000036CE)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MISSINGQUOTE' ;
                            HrDescr := 'Manifest Parse Error: A string literal was expected, but no opening quotation mark was found.';
                          end;
    LongInt($000036CF)  : begin
                            HrStr := 'ERROR_SXS_XML_E_COMMENTSYNTAX' ;
                            HrDescr := 'Manifest Parse Error: Incorrect syntax was used in a comment.';
                          end;
    LongInt($000036D0)  : begin
                            HrStr := 'ERROR_SXS_XML_E_BADSTARTNAMECHAR' ;
                            HrDescr := 'Manifest Parse Error: A name started with an invalid character.';
                          end;
    LongInt($000036D1)  : begin
                            HrStr := 'ERROR_SXS_XML_E_BADNAMECHAR' ;
                            HrDescr := 'Manifest Parse Error: A name contained an invalid character.';
                          end;
    LongInt($000036D2)  : begin
                            HrStr := 'ERROR_SXS_XML_E_BADCHARINSTRING' ;
                            HrDescr := 'Manifest Parse Error: A string literal contained an invalid character.';
                          end;
    LongInt($000036D3)  : begin
                            HrStr := 'ERROR_SXS_XML_E_XMLDECLSYNTAX' ;
                            HrDescr := 'Manifest Parse Error: Invalid syntax for an XML declaration.';
                          end;
    LongInt($000036D4)  : begin
                            HrStr := 'ERROR_SXS_XML_E_BADCHARDATA' ;
                            HrDescr := 'Manifest Parse Error: An Invalid character was found in text content.';
                          end;
    LongInt($000036D5)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MISSINGWHITESPACE' ;
                            HrDescr := 'Manifest Parse Error: Required white space was missing.';
                          end;
    LongInt($000036D6)  : begin
                            HrStr := 'ERROR_SXS_XML_E_EXPECTINGTAGEND' ;
                            HrDescr := 'Manifest Parse Error: The angle bracket (>) character was expected.';
                          end;
    LongInt($000036D7)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MISSINGSEMICOLON' ;
                            HrDescr := 'Manifest Parse Error: A semicolon (;) was expected.';
                          end;
    LongInt($000036D8)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNBALANCEDPAREN' ;
                            HrDescr := 'Manifest Parse Error: Unbalanced parentheses.';
                          end;
    LongInt($000036D9)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INTERNALERROR' ;
                            HrDescr := 'Manifest Parse Error: Internal error.';
                          end;
    LongInt($000036DA)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNEXPECTED_WHITESPACE' ;
                            HrDescr := 'Manifest Parse Error: Whitespace is not allowed at this location.';
                          end;
    LongInt($000036DB)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INCOMPLETE_ENCODING' ;
                            HrDescr := 'Manifest Parse Error: End of file reached in invalid state for current encoding.';
                          end;
    LongInt($000036DC)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MISSING_PAREN' ;
                            HrDescr := 'Manifest Parse Error: Missing parenthesis.';
                          end;
    LongInt($000036DD)  : begin
                            HrStr := 'ERROR_SXS_XML_E_EXPECTINGCLOSEQUOTE' ;
                            HrDescr := 'Manifest Parse Error: A single ('') or double (") quotation mark is missing.';
                          end;
    LongInt($000036DE)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MULTIPLE_COLONS' ;
                            HrDescr := 'Manifest Parse Error: Multiple colons are not allowed in a name.';
                          end;
    LongInt($000036DF)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALID_DECIMAL' ;
                            HrDescr := 'Manifest Parse Error: Invalid character for decimal digit.';
                          end;
    LongInt($000036E0)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALID_HEXIDECIMAL' ;
                            HrDescr := 'Manifest Parse Error: Invalid character for hexadecimal digit.';
                          end;
    LongInt($000036E1)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALID_UNICODE' ;
                            HrDescr := 'Manifest Parse Error: Invalid Unicode character value for this platform.';
                          end;
    LongInt($000036E2)  : begin
                            HrStr := 'ERROR_SXS_XML_E_WHITESPACEORQUESTIONMARK' ;
                            HrDescr := 'Manifest Parse Error: Expecting whitespace or question mark (?).';
                          end;
    LongInt($000036E3)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNEXPECTEDENDTAG' ;
                            HrDescr := 'Manifest Parse Error: End tag was not expected at this location.';
                          end;
    LongInt($000036E4)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNCLOSEDTAG' ;
                            HrDescr := 'Manifest Parse Error: The following tags were not closed: %1.';
                          end;
    LongInt($000036E5)  : begin
                            HrStr := 'ERROR_SXS_XML_E_DUPLICATEATTRIBUTE' ;
                            HrDescr := 'Manifest Parse Error: Duplicate attribute.';
                          end;
    LongInt($000036E6)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MULTIPLEROOTS' ;
                            HrDescr := 'Manifest Parse Error: Only one top-level element is allowed in an XML document.';
                          end;
    LongInt($000036E7)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALIDATROOTLEVEL' ;
                            HrDescr := 'Manifest Parse Error: Invalid at the top level of the document.';
                          end;
    LongInt($000036E8)  : begin
                            HrStr := 'ERROR_SXS_XML_E_BADXMLDECL' ;
                            HrDescr := 'Manifest Parse Error: Invalid XML declaration.';
                          end;
    LongInt($000036E9)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MISSINGROOT' ;
                            HrDescr := 'Manifest Parse Error: XML document must have a top-level element.';
                          end;
    LongInt($000036EA)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNEXPECTEDEOF' ;
                            HrDescr := 'Manifest Parse Error: Unexpected end of file.';
                          end;
    LongInt($000036EB)  : begin
                            HrStr := 'ERROR_SXS_XML_E_BADPEREFINSUBSET' ;
                            HrDescr := 'Manifest Parse Error: Parameter entities cannot be used inside markup declarations in an internal subset.';
                          end;
    LongInt($000036EC)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNCLOSEDSTARTTAG' ;
                            HrDescr := 'Manifest Parse Error: Element was not closed.';
                          end;
    LongInt($000036ED)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNCLOSEDENDTAG' ;
                            HrDescr := 'Manifest Parse Error: End element was missing the angle bracket (>) character.';
                          end;
    LongInt($000036EE)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNCLOSEDSTRING' ;
                            HrDescr := 'Manifest Parse Error: A string literal was not closed.';
                          end;
    LongInt($000036EF)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNCLOSEDCOMMENT' ;
                            HrDescr := 'Manifest Parse Error: A comment was not closed.';
                          end;
    LongInt($000036F0)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNCLOSEDDECL' ;
                            HrDescr := 'Manifest Parse Error: A declaration was not closed.';
                          end;
    LongInt($000036F1)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNCLOSEDCDATA' ;
                            HrDescr := 'Manifest Parse Error: A CDATA section was not closed.';
                          end;
    LongInt($000036F2)  : begin
                            HrStr := 'ERROR_SXS_XML_E_RESERVEDNAMESPACE' ;
                            HrDescr := 'Manifest Parse Error: The namespace prefix is not allowed to start with the reserved string xml"."';
                          end;
    LongInt($000036F3)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALIDENCODING' ;
                            HrDescr := 'Manifest Parse Error: System does not support the specified encoding.';
                          end;
    LongInt($000036F4)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALIDSWITCH' ;
                            HrDescr := 'Manifest Parse Error: Switch from current encoding to specified encoding not supported.';
                          end;
    LongInt($000036F5)  : begin
                            HrStr := 'ERROR_SXS_XML_E_BADXMLCASE' ;
                            HrDescr := 'Manifest Parse Error: The name "xml" is reserved and must be lowercase.';
                          end;
    LongInt($000036F6)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALID_STANDALONE' ;
                            HrDescr := 'Manifest Parse Error: The stand-alone attribute must have the value "yes" or "no".';
                          end;
    LongInt($000036F7)  : begin
                            HrStr := 'ERROR_SXS_XML_E_UNEXPECTED_STANDALONE' ;
                            HrDescr := 'Manifest Parse Error: The stand-alone attribute cannot be used in external entities.';
                          end;
    LongInt($000036F8)  : begin
                            HrStr := 'ERROR_SXS_XML_E_INVALID_VERSION' ;
                            HrDescr := 'Manifest Parse Error: Invalid version number.';
                          end;
    LongInt($000036F9)  : begin
                            HrStr := 'ERROR_SXS_XML_E_MISSINGEQUALS' ;
                            HrDescr := 'Manifest Parse Error: Missing equal sign (=) between the attribute and the attribute value.';
                          end;
    LongInt($000036FA)  : begin
                            HrStr := 'ERROR_SXS_PROTECTION_RECOVERY_FAILED' ;
                            HrDescr := 'Assembly Protection Error: Unable to recover the specified assembly.';
                          end;
    LongInt($000036FB)  : begin
                            HrStr := 'ERROR_SXS_PROTECTION_PUBLIC_KEY_OO_SHORT' ;
                            HrDescr := 'Assembly Protection Error: The public key for an assembly was too short to be allowed.';
                          end;
    LongInt($000036FC)  : begin
                            HrStr := 'ERROR_SXS_PROTECTION_CATALOG_NOT_VALID' ;
                            HrDescr := 'Assembly Protection Error: The catalog for an assembly is not valid, or does not match the assembly''s manifest.';
                          end;
    LongInt($000036FD)  : begin
                            HrStr := 'ERROR_SXS_UNTRANSLATABLE_HRESULT' ;
                            HrDescr := 'An HRESULT could not be translated to a corresponding Win32 error code.';
                          end;
    LongInt($000036FE)  : begin
                            HrStr := 'ERROR_SXS_PROTECTION_CATALOG_FILE_MISSING' ;
                            HrDescr := 'Assembly Protection Error: The catalog for an assembly is missing.';
                          end;
    LongInt($000036FF)  : begin
                            HrStr := 'ERROR_SXS_MISSING_ASSEMBLY_IDENTITY_ATTRIBUTE' ;
                            HrDescr := 'The supplied assembly identity is missing one or more attributes that must be present in this context.';
                          end;
    LongInt($00003700)  : begin
                            HrStr := 'ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE_NAME' ;
                            HrDescr := 'The supplied assembly identity has one or more attribute names that contain characters not permitted in XML names.';
                          end;
    LongInt($00003701)  : begin
                            HrStr := 'ERROR_SXS_ASSEMBLY_MISSING' ;
                            HrDescr := 'The referenced assembly could not be found.';
                          end;
    LongInt($00003702)  : begin
                            HrStr := 'ERROR_SXS_CORRUPT_ACTIVATION_STACK' ;
                            HrDescr := 'The activation context activation stack for the running thread of execution is corrupt.';
                          end;
    LongInt($00003703)  : begin
                            HrStr := 'ERROR_SXS_CORRUPTION' ;
                            HrDescr := 'The application isolation metadata for this process or thread has become corrupt.';
                          end;
    LongInt($00003704)  : begin
                            HrStr := 'ERROR_SXS_EARLY_DEACTIVATION' ;
                            HrDescr := 'The activation context being deactivated is not the most recently activated one.';
                          end;
    LongInt($00003705)  : begin
                            HrStr := 'ERROR_SXS_INVALID_DEACTIVATION' ;
                            HrDescr := 'The activation context being deactivated is not active for the current thread of execution.';
                          end;
    LongInt($00003706)  : begin
                            HrStr := 'ERROR_SXS_MULTIPLE_DEACTIVATION' ;
                            HrDescr := 'The activation context being deactivated has already been deactivated.';
                          end;
    LongInt($00003707)  : begin
                            HrStr := 'ERROR_SXS_PROCESS_TERMINATION_REQUESTED' ;
                            HrDescr := 'A component used by the isolation facility has requested to terminate the process.';
                          end;
    LongInt($00003708)  : begin
                            HrStr := 'ERROR_SXS_RELEASE_ACTIVATION_ONTEXT' ;
                            HrDescr := 'A kernel mode component is releasing a reference on an activation context.';
                          end;
    LongInt($00003709)  : begin
                            HrStr := 'ERROR_SXS_SYSTEM_DEFAULT_ACTIVATION_CONTEXT_EMPTY' ;
                            HrDescr := 'The activation context of the system default assembly could not be generated.';
                          end;
    LongInt($0000370A)  : begin
                            HrStr := 'ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_VALUE' ;
                            HrDescr := 'The value of an attribute in an identity is not within the legal range.';
                          end;
    LongInt($0000370B)  : begin
                            HrStr := 'ERROR_SXS_INVALID_IDENTITY_ATTRIBUTE_NAME' ;
                            HrDescr := 'The name of an attribute in an identity is not within the legal range.';
                          end;
    LongInt($0000370C)  : begin
                            HrStr := 'ERROR_SXS_IDENTITY_DUPLICATE_ATTRIBUTE' ;
                            HrDescr := 'An identity contains two definitions for the same attribute.';
                          end;
    LongInt($0000370D)  : begin
                            HrStr := 'ERROR_SXS_IDENTITY_PARSE_ERROR' ;
                            HrDescr := 'The identity string is malformed.' +
                                       'This might be due to a trailing comma, more than two unnamed attributes, a missing attribute name, or a missing attribute value.';
                          end;
    LongInt($0000370E)  : begin
                            HrStr := 'ERROR_MALFORMED_SUBSTITUTION_STRING' ;
                            HrDescr := 'A string containing localized substitutable content was malformed.' +
                                       'Either a dollar sign ($) was followed by something other than a left parenthesis or another dollar sign, or a substitution''s right parenthesis was not found.';
                          end;
    LongInt($0000370F)  : begin
                            HrStr := 'ERROR_SXS_INCORRECT_PUBLIC_KEY_OKEN' ;
                            HrDescr := 'The public key token does not correspond to the public key specified.';
                          end;
    LongInt($00003710)  : begin
                            HrStr := 'ERROR_UNMAPPED_SUBSTITUTION_STRING' ;
                            HrDescr := 'A substitution string had no mapping.';
                          end;
    LongInt($00003711)  : begin
                            HrStr := 'ERROR_SXS_ASSEMBLY_NOT_LOCKED' ;
                            HrDescr := 'The component must be locked before making the request.';
                          end;
    LongInt($00003712)  : begin
                            HrStr := 'ERROR_SXS_COMPONENT_STORE_CORRUPT' ;
                            HrDescr := 'The component store has been corrupted.';
                          end;
    LongInt($00003713)  : begin
                            HrStr := 'ERROR_ADVANCED_INSTALLER_FAILED' ;
                            HrDescr := 'An advanced installer failed during setup or servicing.';
                          end;
    LongInt($00003714)  : begin
                            HrStr := 'ERROR_XML_ENCODING_MISMATCH' ;
                            HrDescr := 'The character encoding in the XML declaration did not match the encoding used in the document.';
                          end;
    LongInt($00003715)  : begin
                            HrStr := 'ERROR_SXS_MANIFEST_IDENTITY_SAME_BUT_CONTENTS_DIFFERENT' ;
                            HrDescr := 'The identities of the manifests are identical, but the contents are different.';
                          end;
    LongInt($00003716)  : begin
                            HrStr := 'ERROR_SXS_IDENTITIES_DIFFERENT' ;
                            HrDescr := 'The component identities are different.';
                          end;
    LongInt($00003717)  : begin
                            HrStr := 'ERROR_SXS_ASSEMBLY_IS_NOT_A_DEPLOYMENT' ;
                            HrDescr := 'The assembly is not a deployment.';
                          end;
    LongInt($00003718)  : begin
                            HrStr := 'ERROR_SXS_FILE_NOT_PART_OF_ASSEMBLY' ;
                            HrDescr := 'The file is not a part of the assembly.';
                          end;
    LongInt($00003719)  : begin
                            HrStr := 'ERROR_SXS_MANIFEST_TOO_BIG' ;
                            HrDescr := 'The size of the manifest exceeds the maximum allowed.';
                          end;
    LongInt($0000371A)  : begin
                            HrStr := 'ERROR_SXS_SETTING_NOT_REGISTERED' ;
                            HrDescr := 'The setting is not registered.';
                          end;
    LongInt($0000371B)  : begin
                            HrStr := 'ERROR_SXS_TRANSACTION_CLOSURE_INCOMPLETE' ;
                            HrDescr := 'One or more required members of the transaction are not present.';
                          end;
    LongInt($00003A98)  : begin
                            HrStr := 'ERROR_EVT_INVALID_CHANNEL_PATH' ;
                            HrDescr := 'The specified channel path is invalid.';
                          end;
    LongInt($00003A99)  : begin
                            HrStr := 'ERROR_EVT_INVALID_QUERY' ;
                            HrDescr := 'The specified query is invalid.';
                          end;
    LongInt($00003A9A)  : begin
                            HrStr := 'ERROR_EVT_PUBLISHER_METADATA_NOT_FOUND' ;
                            HrDescr := 'The publisher metadata cannot be found in the resource.';
                          end;
    LongInt($00003A9B)  : begin
                            HrStr := 'ERROR_EVT_EVENT_TEMPLATE_NOT_FOUND' ;
                            HrDescr := 'The template for an event definition cannot be found in the resource (error = %1).';
                          end;
    LongInt($00003A9C)  : begin
                            HrStr := 'ERROR_EVT_INVALID_PUBLISHER_NAME' ;
                            HrDescr := 'The specified publisher name is invalid.';
                          end;
    LongInt($00003A9D)  : begin
                            HrStr := 'ERROR_EVT_INVALID_EVENT_DATA' ;
                            HrDescr := 'The event data raised by the publisher is not compatible with the event template definition in the publisher''s manifest.';
                          end;
    LongInt($00003A9F)  : begin
                            HrStr := 'ERROR_EVT_CHANNEL_NOT_FOUND' ;
                            HrDescr := 'The specified channel could not be found.' +
                                       'Check channel configuration.';
                          end;
    LongInt($00003AA0)  : begin
                            HrStr := 'ERROR_EVT_MALFORMED_XML_TEXT' ;
                            HrDescr := 'The specified XML text was not well-formed.' +
                                       'See extended error for more details.';
                          end;
    LongInt($00003AA1)  : begin
                            HrStr := 'ERROR_EVT_SUBSCRIPTION_TO_DIRECT_CHANNEL' ;
                            HrDescr := 'The caller is trying to subscribe to a direct channel which is not allowed.' +
                                       'The events for a direct channel go directly to a log file and cannot be subscribed to.';
                          end;
    LongInt($00003AA2)  : begin
                            HrStr := 'ERROR_EVT_CONFIGURATION_ERROR' ;
                            HrDescr := 'Configuration error.';
                          end;
    LongInt($00003AA3)  : begin
                            HrStr := 'ERROR_EVT_QUERY_RESULT_STALE' ;
                            HrDescr := 'The query result is stale or invalid.' +
                                       'This might be due to the log being cleared or rolling over after the query result was created.' +
                                       'Users should handle this code by releasing the query result object and reissuing the query.';
                          end;
    LongInt($00003AA4)  : begin
                            HrStr := 'ERROR_EVT_QUERY_RESULT_INVALID_POSITION' ;
                            HrDescr := 'Query result is currently at an invalid position.';
                          end;
    LongInt($00003AA5)  : begin
                            HrStr := 'ERROR_EVT_NON_VALIDATING_MSXML' ;
                            HrDescr := 'Registered Microsoft XML (MSXML) does not support validation.';
                          end;
    LongInt($00003AA6)  : begin
                            HrStr := 'ERROR_EVT_FILTER_ALREADYSCOPED' ;
                            HrDescr := 'An expression can only be followed by a change-of-scope operation if it itself evaluates to a node set and is not already part of some other change-of-scope operation.';
                          end;
    LongInt($00003AA7)  : begin
                            HrStr := 'ERROR_EVT_FILTER_NOTELTSET' ;
                            HrDescr := 'Cannot perform a step operation from a term that does not represent an element set.';
                          end;
    LongInt($00003AA8)  : begin
                            HrStr := 'ERROR_EVT_FILTER_INVARG' ;
                            HrDescr := 'Left side arguments to binary operators must be either attributes, nodes, or variables and right side arguments must be constants.';
                          end;
    LongInt($00003AA9)  : begin
                            HrStr := 'ERROR_EVT_FILTER_INVTEST' ;
                            HrDescr := 'A step operation must involve either a node test or, in the case of a predicate, an algebraic expression against which to test each node in the node set identified by the preceding node set can be evaluated.';
                          end;
    LongInt($00003AAA)  : begin
                            HrStr := 'ERROR_EVT_FILTER_INVTYPE' ;
                            HrDescr := 'This data type is currently unsupported.';
                          end;
    LongInt($00003AAB)  : begin
                            HrStr := 'ERROR_EVT_FILTER_PARSEERR' ;
                            HrDescr := 'A syntax error occurred at position %1!d!';
                          end;
    LongInt($00003AAC)  : begin
                            HrStr := 'ERROR_EVT_FILTER_UNSUPPORTEDOP' ;
                            HrDescr := 'This operator is unsupported by this implementation of the filter.';
                          end;
    LongInt($00003AAD)  : begin
                            HrStr := 'ERROR_EVT_FILTER_UNEXPECTEDTOKEN' ;
                            HrDescr := 'The token encountered was unexpected.';
                          end;
    LongInt($00003AAE)  : begin
                            HrStr := 'ERROR_EVT_INVALID_OPERATION_OVER_ENABLED_DIRECT_CHANNEL' ;
                            HrDescr := 'The requested operation cannot be performed over an enabled direct channel.' +
                                       'The channel must first be disabled before performing the requested operation.';
                          end;
    LongInt($00003AAF)  : begin
                            HrStr := 'ERROR_EVT_INVALID_CHANNEL_PROPERTY_VALUE' ;
                            HrDescr := 'Channel property %1!s! contains an invalid value.' +
                                       'The value has an invalid type, is outside the valid range, cannot be updated, or is not supported by this type of channel.';
                          end;
    LongInt($00003AB0)  : begin
                            HrStr := 'ERROR_EVT_INVALID_PUBLISHER_PROPERTY_VALUE' ;
                            HrDescr := 'Publisher property %1!s! contains an invalid value.' +
                                       'The value has an invalid type, is outside the valid range, cannot be updated, or is not supported by this type of publisher.';
                          end;
    LongInt($00003AB1)  : begin
                            HrStr := 'ERROR_EVT_CHANNEL_CANNOT_ACTIVATE' ;
                            HrDescr := 'The channel fails to activate.';
                          end;
    LongInt($00003AB2)  : begin
                            HrStr := 'ERROR_EVT_FILTER_TOO_COMPLEX' ;
                            HrDescr := 'The xpath expression exceeded supported complexity.' +
                                       'Simplify it or split it into two or more simple expressions.';
                          end;
    LongInt($00003AB3)  : begin
                            HrStr := 'ERROR_EVT_MESSAGE_NOT_FOUND' ;
                            HrDescr := 'The message resource is present but the message is not found in the string or message table.';
                          end;
    LongInt($00003AB4)  : begin
                            HrStr := 'ERROR_EVT_MESSAGE_ID_NOT_FOUND' ;
                            HrDescr := 'The message ID for the desired message could not be found.';
                          end;
    LongInt($00003AB5)  : begin
                            HrStr := 'ERROR_EVT_UNRESOLVED_VALUE_INSERT' ;
                            HrDescr := 'The substitution string for the insert index (%1) could not be found.';
                          end;
    LongInt($00003AB6)  : begin
                            HrStr := 'ERROR_EVT_UNRESOLVED_PARAMETER_INSERT' ;
                            HrDescr := 'The description string for the parameter reference (%1) could not be found.';
                          end;
    LongInt($00003AB7)  : begin
                            HrStr := 'ERROR_EVT_MAX_INSERTS_REACHED' ;
                            HrDescr := 'The maximum number of replacements has been reached.';
                          end;
    LongInt($00003AB8)  : begin
                            HrStr := 'ERROR_EVT_EVENT_DEFINITION_NOT_OUND' ;
                            HrDescr := 'The event definition could not be found for the event ID (%1).';
                          end;
    LongInt($00003AB9)  : begin
                            HrStr := 'ERROR_EVT_MESSAGE_LOCALE_NOT_FOUND' ;
                            HrDescr := 'The locale-specific resource for the desired message is not present.';
                          end;
    LongInt($00003ABA)  : begin
                            HrStr := 'ERROR_EVT_VERSION_TOO_OLD' ;
                            HrDescr := 'The resource is too old to be compatible.';
                          end;
    LongInt($00003ABB)  : begin
                            HrStr := 'ERROR_EVT_VERSION_TOO_NEW' ;
                            HrDescr := 'The resource is too new to be compatible.';
                          end;
    LongInt($00003ABC)  : begin
                            HrStr := 'ERROR_EVT_CANNOT_OPEN_CHANNEL_OF_QUERY' ;
                            HrDescr := 'The channel at index %1 of the query cannot be opened.';
                          end;
    LongInt($00003ABD)  : begin
                            HrStr := 'ERROR_EVT_PUBLISHER_DISABLED' ;
                            HrDescr := 'The publisher has been disabled and its resource is not available.' +
                                       'This usually occurs when the publisher is in the process of being uninstalled or upgraded.';
                          end;
    LongInt($00003AE8)  : begin
                            HrStr := 'ERROR_EC_SUBSCRIPTION_CANNOT_ACTIVATE' ;
                            HrDescr := 'The subscription fails to activate.';
                          end;
    LongInt($00003AE9)  : begin
                            HrStr := 'ERROR_EC_LOG_DISABLED' ;
                            HrDescr := 'The log of the subscription is in a disabled state and events cannot be forwarded to it.' +
                                       'The log must first be enabled before the subscription can be activated.';
                          end;
    LongInt($00003AFC)  : begin
                            HrStr := 'ERROR_MUI_FILE_NOT_FOUND' ;
                            HrDescr := 'The resource loader failed to find the Multilingual User Interface (MUI) file.';
                          end;
    LongInt($00003AFD)  : begin
                            HrStr := 'ERROR_MUI_INVALID_FILE' ;
                            HrDescr := 'The resource loader failed to load the MUI file because the file failed to pass validation.';
                          end;
    LongInt($00003AFE)  : begin
                            HrStr := 'ERROR_MUI_INVALID_RC_CONFIG' ;
                            HrDescr := 'The release candidate (RC) manifest is corrupted with garbage data, is an unsupported version, or is missing a required item.';
                          end;
    LongInt($00003AFF)  : begin
                            HrStr := 'ERROR_MUI_INVALID_LOCALE_NAME' ;
                            HrDescr := 'The RC manifest has an invalid culture name.';
                          end;
    LongInt($00003B00)  : begin
                            HrStr := 'ERROR_MUI_INVALID_ULTIMATEFALLBACK_NAME' ;
                            HrDescr := 'The RC Manifest has an invalid ultimate fallback name.';
                          end;
    LongInt($00003B01)  : begin
                            HrStr := 'ERROR_MUI_FILE_NOT_LOADED' ;
                            HrDescr := 'The resource loader cache does not have a loaded MUI entry.';
                          end;
    LongInt($00003B02)  : begin
                            HrStr := 'ERROR_RESOURCE_ENUM_USER_STOP' ;
                            HrDescr := 'The user stopped resource enumeration.';
                          end;
    LongInt($00003B03)  : begin
                            HrStr := 'ERROR_MUI_INTLSETTINGS_UILANG_NOT_INSTALLED' ;
                            HrDescr := 'User interface language installation failed.';
                          end;
    LongInt($00003B04)  : begin
                            HrStr := 'ERROR_MUI_INTLSETTINGS_INVALID_LOCALE_NAME' ;
                            HrDescr := 'Locale installation failed.';
                          end;
    LongInt($00003B60)  : begin
                            HrStr := 'ERROR_MCA_INVALID_CAPABILITIES_STRING' ;
                            HrDescr := 'The monitor returned a DDC/CI capabilities string that did not comply with the ACCESS.bus 3.0, DDC/CI 1.1, or MCCS 2 Revision 1 specification.';
                          end;
    LongInt($00003B61)  : begin
                            HrStr := 'ERROR_MCA_INVALID_VCP_VERSION' ;
                            HrDescr := 'The monitor''s VCP version ($DF) VCP code returned an invalid version value.';
                          end;
    LongInt($00003B62)  : begin
                            HrStr := 'ERROR_MCA_MONITOR_VIOLATES_MCCS_SPECIFICATION' ;
                            HrDescr := 'The monitor does not comply with the MCCS specification it claims to support.';
                          end;
    LongInt($00003B63)  : begin
                            HrStr := 'ERROR_MCA_MCCS_VERSION_MISMATCH' ;
                            HrDescr := 'The MCCS version in a monitor''s mccs_ver capability does not match the MCCS version the monitor reports when the VCP version ($DF) VCP code is used.';
                          end;
    LongInt($00003B64)  : begin
                            HrStr := 'ERROR_MCA_UNSUPPORTED_MCCS_VERSION' ;
                            HrDescr := 'The monitor configuration API works only with monitors that support the MCCS 1.0, MCCS 2.0, or MCCS 2.0 Revision 1 specifications.';
                          end;
    LongInt($00003B65)  : begin
                            HrStr := 'ERROR_MCA_INTERNAL_ERROR' ;
                            HrDescr := 'An internal monitor configuration API error occurred.';
                          end;
    LongInt($00003B66)  : begin
                            HrStr := 'ERROR_MCA_INVALID_TECHNOLOGY_TYPE_RETURNED' ;
                            HrDescr := 'The monitor returned an invalid monitor technology type.' +
                                       'CRT, plasma, and LCD (TFT) are examples of monitor technology types.' +
                                       'This error implies that the monitor violated the MCCS 2.0 or MCCS 2.0 Revision 1 specification.';
                          end;
    LongInt($00003B67)  : begin
                            HrStr := 'ERROR_MCA_UNSUPPORTED_COLOR_TEMPERATURE' ;
                            HrDescr := 'The SetMonitorColorTemperature() caller passed a color temperature to it that the current monitor did not support.' +
                                       'CRT, plasma, and LCD (TFT) are examples of monitor technology types.' +
                                       'This error implies that the monitor violated the MCCS 2.0 or MCCS 2.0 Revision 1 specification.';
                          end;
    LongInt($00003B92)  : begin
                            HrStr := 'ERROR_AMBIGUOUS_SYSTEM_DEVICE' ;
                            HrDescr := 'The requested system device cannot be identified due to multiple indistinguishable devices potentially matching the identification criteria.';
                          end;
    LongInt($00003BC3)  : begin
                            HrStr := 'ERROR_SYSTEM_DEVICE_NOT_FOUND';
                            HrDescr := 'The requested system device cannot be found.';
                          end;
    else
      begin
        HrStr := 'Unknown identifier.';
        HrDescr := 'Unknown HResult code.';
        HeaderFile := 'Unknown.';
        FWinHResultCracker.ClearResults();
        hr := aHResult;
      end;
  end;
  Result := hr;
end;


function GetErr32Region(aHResult: HResult;
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
  if (li >= 0) and (li <= 499) then
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
