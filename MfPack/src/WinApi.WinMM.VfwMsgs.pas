  // FactoryX
  //
  // Copyright: © FactoryX. All rights reserved.
  //
  // Project: MfPack - Shared
  // Project location: https:  //sourceforge.net/projects/MFPack
  //                   https:  //github.com/FactoryXCode/MfPack
  // Module: WinApi.WinMM.VfwMsgs.pas
  // Kind: Pascal / Delphi unit
  // Release date: 17-05-2020
  // Language: ENU
  //
  // Revision Version: 3.0.0
  // Description: Vfw facility codes
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
  // Remarks: -
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
  // Source: vfwmsgs.h
  //
  // Copyright (c) Microsoft Corporation. All rights reserved.
  //==============================================================================
  //
  // LICENSE
  //
  // The contents of this file are subject to the Mozilla Public License
  // Version 2.0 (the "License"); you may not use this file except in
  // compliance with the License. You may obtain a copy of the License at
  // https:  //www.mozilla.org/en-US/MPL/2.0/
  //
  // Software distributed under the License is distributed on an "AS IS"
  // basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  // License for the specific language governing rights and limitations
  // under the License.
  //
  // Users may distribute this source code provided that this header is included
  // in full at the top of the file.
  //==============================================================================
unit WinApi.WinMM.VfwMsgs;

interface

   // no longer used  - but might get
   // our own facility in the future?
   // FacilityNames=(FACILITY_VFW=0x4)
   // To add a message:
   //
   // The MessageId is the number of the message.
   // Accepted severities are 'Success' and 'Warning'.
   //
   // Facility should be FACILITY_ITF (was FACILITY_VFW).
   //
   // The SymbolicName is the name used in the code to identify the message.
   // The text of a message starts the line after 'Language=' and
   // ends before a line with only a '.' in column one.
  //
  //  Values are 32 bit values laid out as follows:
  //
  //   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  //   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  //  +---+-+-+-----------------------+-------------------------------+
  //  |Sev|C|R|     Facility          |               Code            |
  //  +---+-+-+-----------------------+-------------------------------+
  //
  //  where
  //
  //      Sev - is the severity code
  //
  //          00 - Success
  //          01 - Informational
  //          10 - Warning
  //          11 - Error
  //
  //      C - is the Customer code flag
  //
  //      R - is a reserved bit
  //
  //      Facility - is the facility code
  //
  //      Code - is the facility's status code
  //
  //
  // Define the facility codes
  //


  //
  // Define the severity codes
  //

const

  //
  // MessageId: VFW_E_INVALIDMEDIATYPE
  //
  // MessageText:
  //
  // An invalid media type was specified.%0
  //
  VFW_E_INVALIDMEDIATYPE              = HResult($80040200);
  {$EXTERNALSYM VFW_E_INVALIDMEDIATYPE}

  //
  // MessageId: VFW_E_INVALIDSUBTYPE
  //
  // MessageText:
  //
  // An invalid media subtype was specified.%0
  //
  VFW_E_INVALIDSUBTYPE                = HResult($80040201);
  {$EXTERNALSYM VFW_E_INVALIDSUBTYPE}

  //
  // MessageId: VFW_E_NEED_OWNER
  //
  // MessageText:
  //
  // This object can only be created as an aggregated object.%0
  //
  VFW_E_NEED_OWNER                    = HResult($80040202);
  {$EXTERNALSYM VFW_E_NEED_OWNER}

  //
  // MessageId: VFW_E_ENUM_OUT_OF_SYNC
  //
  // MessageText:
  //
  // The enumerator has become invalid.%0
  //
  VFW_E_ENUM_OUT_OF_SYNC              = HResult($80040203);
  {$EXTERNALSYM VFW_E_ENUM_OUT_OF_SYNC}

  //
  // MessageId: VFW_E_ALREADY_CONNECTED
  //
  // MessageText:
  //
  // At least one of the pins involved in the operation is already connected.%0
  //
  VFW_E_ALREADY_CONNECTED             = HResult($80040204);
  {$EXTERNALSYM VFW_E_ALREADY_CONNECTED}

  //
  // MessageId: VFW_E_FILTER_ACTIVE
  //
  // MessageText:
  //
  // This operation cannot be performed because the filter is active.%0
  //
  VFW_E_FILTER_ACTIVE                 = HResult($80040205);
  {$EXTERNALSYM VFW_E_FILTER_ACTIVE}

  //
  // MessageId: VFW_E_NO_TYPES
  //
  // MessageText:
  //
  // One of the specified pins supports no media types.%0
  //
  VFW_E_NO_TYPES                      = HResult($80040206);
  {$EXTERNALSYM VFW_E_NO_TYPES}

  //
  // MessageId: VFW_E_NO_ACCEPTABLE_TYPES
  //
  // MessageText:
  //
  // There is no common media type between these pins.%0
  //
  VFW_E_NO_ACCEPTABLE_TYPES           = HResult($80040207);
  {$EXTERNALSYM VFW_E_NO_ACCEPTABLE_TYPES}

  //
  // MessageId: VFW_E_INVALID_DIRECTION
  //
  // MessageText:
  //
  // Two pins of the same direction cannot be connected together.%0
  //
  VFW_E_INVALID_DIRECTION             = HResult($80040208);
  {$EXTERNALSYM VFW_E_INVALID_DIRECTION}

  //
  // MessageId: VFW_E_NOT_CONNECTED
  //
  // MessageText:
  //
  // The operation cannot be performed because the pins are not connected.%0
  //
  VFW_E_NOT_CONNECTED                 = HResult($80040209);
  {$EXTERNALSYM VFW_E_NOT_CONNECTED}

  //
  // MessageId: VFW_E_NO_ALLOCATOR
  //
  // MessageText:
  //
  // No sample buffer allocator is available.%0
  //
  VFW_E_NO_ALLOCATOR                  = HResult($8004020A);
  {$EXTERNALSYM VFW_E_NO_ALLOCATOR}

  //
  // MessageId: VFW_E_RUNTIME_ERROR
  //
  // MessageText:
  //
  // A run-time error occurred.%0
  //
  VFW_E_RUNTIME_ERROR                 = HResult($8004020B);
  {$EXTERNALSYM VFW_E_RUNTIME_ERROR}

  //
  // MessageId: VFW_E_BUFFER_NOTSET
  //
  // MessageText:
  //
  // No buffer space has been set.%0
  //
  VFW_E_BUFFER_NOTSET                 = HResult($8004020C);
  {$EXTERNALSYM VFW_E_BUFFER_NOTSET}

  //
  // MessageId: VFW_E_BUFFER_OVERFLOW
  //
  // MessageText:
  //
  // The buffer is not big enough.%0
  //
  VFW_E_BUFFER_OVERFLOW               = HResult($8004020D);
  {$EXTERNALSYM VFW_E_BUFFER_OVERFLOW}

  //
  // MessageId: VFW_E_BADALIGN
  //
  // MessageText:
  //
  // An invalid alignment was specified.%0
  //
  VFW_E_BADALIGN                      = HResult($8004020E);
  {$EXTERNALSYM VFW_E_BADALIGN}

  //
  // MessageId: VFW_E_ALREADY_COMMITTED
  //
  // MessageText:
  //
  // Cannot change allocated memory while the filter is active.%0
  //
  VFW_E_ALREADY_COMMITTED             = HResult($8004020F);
  {$EXTERNALSYM VFW_E_ALREADY_COMMITTED}

  //
  // MessageId: VFW_E_BUFFERS_OUTSTANDING
  //
  // MessageText:
  //
  // One or more buffers are still active.%0
  //
  VFW_E_BUFFERS_OUTSTANDING           = HResult($80040210);
  {$EXTERNALSYM VFW_E_BUFFERS_OUTSTANDING}

  //
  // MessageId: VFW_E_NOT_COMMITTED
  //
  // MessageText:
  //
  // Cannot allocate a sample when the allocator is not active.%0
  //
  VFW_E_NOT_COMMITTED                 = HResult($80040211);
  {$EXTERNALSYM VFW_E_NOT_COMMITTED}

  //
  // MessageId: VFW_E_SIZENOTSET
  //
  // MessageText:
  //
  // Cannot allocate memory because no size has been set.%0
  //
  VFW_E_SIZENOTSET                    = HResult($80040212);
  {$EXTERNALSYM VFW_E_SIZENOTSET}

  //
  // MessageId: VFW_E_NO_CLOCK
  //
  // MessageText:
  //
  // Cannot lock for synchronization because no clock has been defined.%0
  //
  VFW_E_NO_CLOCK                      = HResult($80040213);
  {$EXTERNALSYM VFW_E_NO_CLOCK}

  //
  // MessageId: VFW_E_NO_SINK
  //
  // MessageText:
  //
  // Quality messages could not be sent because no quality sink has been defined.%0
  //
  VFW_E_NO_SINK                       = HResult($80040214);
  {$EXTERNALSYM VFW_E_NO_SINK}

  //
  // MessageId: VFW_E_NO_INTERFACE
  //
  // MessageText:
  //
  // A required interface has not been implemented.%0
  //
  VFW_E_NO_INTERFACE                  = HResult($80040215);
  {$EXTERNALSYM VFW_E_NO_INTERFACE}

  //
  // MessageId: VFW_E_NOT_FOUND
  //
  // MessageText:
  //
  // An object or name was not found.%0
  //
  VFW_E_NOT_FOUND                     = HResult($80040216);
  {$EXTERNALSYM VFW_E_NOT_FOUND}

  //
  // MessageId: VFW_E_CANNOT_CONNECT
  //
  // MessageText:
  //
  // No combination of intermediate filters could be found to make the connection.%0
  //
  VFW_E_CANNOT_CONNECT                = HResult($80040217);
  {$EXTERNALSYM VFW_E_CANNOT_CONNECT}

  //
  // MessageId: VFW_E_CANNOT_RENDER
  //
  // MessageText:
  //
  // No combination of filters could be found to render the stream.%0
  //
  VFW_E_CANNOT_RENDER                 = HResult($80040218);
  {$EXTERNALSYM VFW_E_CANNOT_RENDER}

  //
  // MessageId: VFW_E_CHANGING_FORMAT
  //
  // MessageText:
  //
  // Could not change formats dynamically.%0
  //
  VFW_E_CHANGING_FORMAT               = HResult($80040219);
  {$EXTERNALSYM VFW_E_CHANGING_FORMAT}

  //
  // MessageId: VFW_E_NO_COLOR_KEY_SET
  //
  // MessageText:
  //
  // No color key has been set.%0
  //
  VFW_E_NO_COLOR_KEY_SET              = HResult($8004021A);
  {$EXTERNALSYM VFW_E_NO_COLOR_KEY_SET}

  //
  // MessageId: VFW_E_NOT_OVERLAY_CONNECTION
  //
  // MessageText:
  //
  // Current pin connection is not using the IOverlay transport.%0
  //
  VFW_E_NOT_OVERLAY_CONNECTION        = HResult($8004021B);
  {$EXTERNALSYM VFW_E_NOT_OVERLAY_CONNECTION}

  //
  // MessageId: VFW_E_NOT_SAMPLE_CONNECTION
  //
  // MessageText:
  //
  // Current pin connection is not using the IMemInputPin transport.%0
  //
  VFW_E_NOT_SAMPLE_CONNECTION         = HResult($8004021C);
  {$EXTERNALSYM VFW_E_NOT_SAMPLE_CONNECTION}

  //
  // MessageId: VFW_E_PALETTE_SET
  //
  // MessageText:
  //
  // Setting a color key would conflict with the palette already set.%0
  //
  VFW_E_PALETTE_SET                   = HResult($8004021D);
  {$EXTERNALSYM VFW_E_PALETTE_SET}

  //
  // MessageId: VFW_E_COLOR_KEY_SET
  //
  // MessageText:
  //
  // Setting a palette would conflict with the color key already set.%0
  //
  VFW_E_COLOR_KEY_SET                 = HResult($8004021E);
  {$EXTERNALSYM VFW_E_COLOR_KEY_SET}

  //
  // MessageId: VFW_E_NO_COLOR_KEY_FOUND
  //
  // MessageText:
  //
  // No matching color key is available.%0
  //
  VFW_E_NO_COLOR_KEY_FOUND            = HResult($8004021F);
  {$EXTERNALSYM VFW_E_NO_COLOR_KEY_FOUND}

  //
  // MessageId: VFW_E_NO_PALETTE_AVAILABLE
  //
  // MessageText:
  //
  // No palette is available.%0
  //
  VFW_E_NO_PALETTE_AVAILABLE          = HResult($80040220);
  {$EXTERNALSYM VFW_E_NO_PALETTE_AVAILABLE}

  //
  // MessageId: VFW_E_NO_DISPLAY_PALETTE
  //
  // MessageText:
  //
  // Display does not use a palette.%0
  //
  VFW_E_NO_DISPLAY_PALETTE            = HResult($80040221);
  {$EXTERNALSYM VFW_E_NO_DISPLAY_PALETTE}

  //
  // MessageId: VFW_E_TOO_MANY_COLORS
  //
  // MessageText:
  //
  // Too many colors for the current display settings.%0
  //
  VFW_E_TOO_MANY_COLORS               = HResult($80040222);
  {$EXTERNALSYM VFW_E_TOO_MANY_COLORS}

  //
  // MessageId: VFW_E_STATE_CHANGED
  //
  // MessageText:
  //
  // The state changed while waiting to process the sample.%0
  //
  VFW_E_STATE_CHANGED                 = HResult($80040223);
  {$EXTERNALSYM VFW_E_STATE_CHANGED}

  //
  // MessageId: VFW_E_NOT_STOPPED
  //
  // MessageText:
  //
  // The operation could not be performed because the filter is not stopped.%0
  //
  VFW_E_NOT_STOPPED                   = HResult($80040224);
  {$EXTERNALSYM VFW_E_NOT_STOPPED}

  //
  // MessageId: VFW_E_NOT_PAUSED
  //
  // MessageText:
  //
  // The operation could not be performed because the filter is not paused.%0
  //
  VFW_E_NOT_PAUSED                    = HResult($80040225);
  {$EXTERNALSYM VFW_E_NOT_PAUSED}

  //
  // MessageId: VFW_E_NOT_RUNNING
  //
  // MessageText:
  //
  // The operation could not be performed because the filter is not running.%0
  //
  VFW_E_NOT_RUNNING                   = HResult($80040226);
  {$EXTERNALSYM VFW_E_NOT_RUNNING}

  //
  // MessageId: VFW_E_WRONG_STATE
  //
  // MessageText:
  //
  // The operation could not be performed because the filter is in the wrong state.%0
  //
  VFW_E_WRONG_STATE                   = HResult($80040227);
  {$EXTERNALSYM VFW_E_WRONG_STATE}

  //
  // MessageId: VFW_E_START_TIME_AFTER_END
  //
  // MessageText:
  //
  // The sample start time is after the sample end time.%0
  //
  VFW_E_START_TIME_AFTER_END          = HResult($80040228);
  {$EXTERNALSYM VFW_E_START_TIME_AFTER_END}

  //
  // MessageId: VFW_E_INVALID_RECT
  //
  // MessageText:
  //
  // The supplied rectangle is invalid.%0
  //
  VFW_E_INVALID_RECT                  = HResult($80040229);
  {$EXTERNALSYM VFW_E_INVALID_RECT}

  //
  // MessageId: VFW_E_TYPE_NOT_ACCEPTED
  //
  // MessageText:
  //
  // This pin cannot use the supplied media type.%0
  //
  VFW_E_TYPE_NOT_ACCEPTED             = HResult($8004022A);
  {$EXTERNALSYM VFW_E_TYPE_NOT_ACCEPTED}

  //
  // MessageId: VFW_E_SAMPLE_REJECTED
  //
  // MessageText:
  //
  // This sample cannot be rendered.%0
  //
  VFW_E_SAMPLE_REJECTED               = HResult($8004022B);
  {$EXTERNALSYM VFW_E_SAMPLE_REJECTED}

  //
  // MessageId: VFW_E_SAMPLE_REJECTED_EOS
  //
  // MessageText:
  //
  // This sample cannot be rendered because the end of the stream has been reached.%0
  //
  VFW_E_SAMPLE_REJECTED_EOS           = HResult($8004022C);
  {$EXTERNALSYM VFW_E_SAMPLE_REJECTED_EOS}

  //
  // MessageId: VFW_E_DUPLICATE_NAME
  //
  // MessageText:
  //
  // An attempt to add a filter with a duplicate name failed.%0
  //
  VFW_E_DUPLICATE_NAME                = HResult($8004022D);
  {$EXTERNALSYM VFW_E_DUPLICATE_NAME}

  //
  // MessageId: VFW_S_DUPLICATE_NAME
  //
  // MessageText:
  //
  // An attempt to add a filter with a duplicate name succeeded with a modified name.%0
  //
  VFW_S_DUPLICATE_NAME                = HResult($0004022D);
  {$EXTERNALSYM VFW_S_DUPLICATE_NAME}

  //
  // MessageId: VFW_E_TIMEOUT
  //
  // MessageText:
  //
  // A time-out has expired.%0
  //
  VFW_E_TIMEOUT                       = HResult($8004022E);
  {$EXTERNALSYM VFW_E_TIMEOUT}

  //
  // MessageId: VFW_E_INVALID_FILE_FORMAT
  //
  // MessageText:
  //
  // The file format is invalid.%0
  //
  VFW_E_INVALID_FILE_FORMAT           = HResult($8004022F);
  {$EXTERNALSYM VFW_E_INVALID_FILE_FORMAT}

  //
  // MessageId: VFW_E_ENUM_OUT_OF_RANGE
  //
  // MessageText:
  //
  // The list has already been exhausted.%0
  //
  VFW_E_ENUM_OUT_OF_RANGE             = HResult($80040230);
  {$EXTERNALSYM VFW_E_ENUM_OUT_OF_RANGE}

  //
  // MessageId: VFW_E_CIRCULAR_GRAPH
  //
  // MessageText:
  //
  // The filter graph is circular.%0
  //
  VFW_E_CIRCULAR_GRAPH                = HResult($80040231);
  {$EXTERNALSYM VFW_E_CIRCULAR_GRAPH}

  //
  // MessageId: VFW_E_NOT_ALLOWED_TO_SAVE
  //
  // MessageText:
  //
  // Updates are not allowed in this state.%0
  //
  VFW_E_NOT_ALLOWED_TO_SAVE           = HResult($80040232);
  {$EXTERNALSYM VFW_E_NOT_ALLOWED_TO_SAVE}

  //
  // MessageId: VFW_E_TIME_ALREADY_PASSED
  //
  // MessageText:
  //
  // An attempt was made to queue a command for a time in the past.%0
  //
  VFW_E_TIME_ALREADY_PASSED           = HResult($80040233);
  {$EXTERNALSYM VFW_E_TIME_ALREADY_PASSED}

  //
  // MessageId: VFW_E_ALREADY_CANCELLED
  //
  // MessageText:
  //
  // The queued command has already been canceled.%0
  //
  VFW_E_ALREADY_CANCELLED             = HResult($80040234);
  {$EXTERNALSYM VFW_E_ALREADY_CANCELLED}

  //
  // MessageId: VFW_E_CORRUPT_GRAPH_FILE
  //
  // MessageText:
  //
  // Cannot render the file because it is corrupt.%0
  //
  VFW_E_CORRUPT_GRAPH_FILE            = HResult($80040235);
  {$EXTERNALSYM VFW_E_CORRUPT_GRAPH_FILE}

  //
  // MessageId: VFW_E_ADVISE_ALREADY_SET
  //
  // MessageText:
  //
  // An overlay advise link already exists.%0
  //
  VFW_E_ADVISE_ALREADY_SET            = HResult($80040236);
  {$EXTERNALSYM VFW_E_ADVISE_ALREADY_SET}

  //
  // MessageId: VFW_S_STATE_INTERMEDIATE
  //
  // MessageText:
  //
  // The state transition has not completed.%0
  //
  VFW_S_STATE_INTERMEDIATE            = HResult($00040237);
  {$EXTERNALSYM VFW_S_STATE_INTERMEDIATE}

  //
  // MessageId: VFW_E_NO_MODEX_AVAILABLE
  //
  // MessageText:
  //
  // No full-screen modes are available.%0
  //
  VFW_E_NO_MODEX_AVAILABLE            = HResult($80040238);
  {$EXTERNALSYM VFW_E_NO_MODEX_AVAILABLE}

  //
  // MessageId: VFW_E_NO_ADVISE_SET
  //
  // MessageText:
  //
  // This Advise cannot be canceled because it was not successfully set.%0
  //
  VFW_E_NO_ADVISE_SET                 = HResult($80040239);
  {$EXTERNALSYM VFW_E_NO_ADVISE_SET}

  //
  // MessageId: VFW_E_NO_FULLSCREEN
  //
  // MessageText:
  //
  // A full-screen mode is not available.%0
  //
  VFW_E_NO_FULLSCREEN                 = HResult($8004023A);
  {$EXTERNALSYM VFW_E_NO_FULLSCREEN}

  //
  // MessageId: VFW_E_IN_FULLSCREEN_MODE
  //
  // MessageText:
  //
  // Cannot call IVideoWindow methods while in full-screen mode.%0
  //
  VFW_E_IN_FULLSCREEN_MODE            = HResult($8004023B);
  {$EXTERNALSYM VFW_E_IN_FULLSCREEN_MODE}

  //
  // MessageId: VFW_E_UNKNOWN_FILE_TYPE
  //
  // MessageText:
  //
  // The media type of this file is not recognized.%0
  //
  VFW_E_UNKNOWN_FILE_TYPE             = HResult($80040240);
  {$EXTERNALSYM VFW_E_UNKNOWN_FILE_TYPE}

  //
  // MessageId: VFW_E_CANNOT_LOAD_SOURCE_FILTER
  //
  // MessageText:
  //
  // The source filter for this file could not be loaded.%0
  //
  VFW_E_CANNOT_LOAD_SOURCE_FILTER     = HResult($80040241);
  {$EXTERNALSYM VFW_E_CANNOT_LOAD_SOURCE_FILTER}

  //
  // MessageId: VFW_S_PARTIAL_RENDER
  //
  // MessageText:
  //
  // Some of the streams in this movie are in an unsupported format.%0
  //
  VFW_S_PARTIAL_RENDER                = HResult($00040242);
  {$EXTERNALSYM VFW_S_PARTIAL_RENDER}

  //
  // MessageId: VFW_E_FILE_TOO_SHORT
  //
  // MessageText:
  //
  // A file appeared to be incomplete.%0
  //
  VFW_E_FILE_TOO_SHORT                = HResult($80040243);
  {$EXTERNALSYM VFW_E_FILE_TOO_SHORT}

  //
  // MessageId: VFW_E_INVALID_FILE_VERSION
  //
  // MessageText:
  //
  // The version number of the file is invalid.%0
  //
  VFW_E_INVALID_FILE_VERSION          = HResult($80040244);
  {$EXTERNALSYM VFW_E_INVALID_FILE_VERSION}

  //
  // MessageId: VFW_S_SOME_DATA_IGNORED
  //
  // MessageText:
  //
  // The file contained some property settings that were not used.%0
  //
  VFW_S_SOME_DATA_IGNORED             = HResult($00040245);
  {$EXTERNALSYM VFW_S_SOME_DATA_IGNORED}

  //
  // MessageId: VFW_S_CONNECTIONS_DEFERRED
  //
  // MessageText:
  //
  // Some connections have failed and have been deferred.%0
  //
  VFW_S_CONNECTIONS_DEFERRED          = HResult($00040246);
  {$EXTERNALSYM VFW_S_CONNECTIONS_DEFERRED}

  //
  // MessageId: VFW_E_INVALID_CLSID
  //
  // MessageText:
  //
  // This file is corrupt: it contains an invalid class identifier.%0
  //
  VFW_E_INVALID_CLSID                 = HResult($80040247);
  {$EXTERNALSYM VFW_E_INVALID_CLSID}

  //
  // MessageId: VFW_E_INVALID_MEDIA_TYPE
  //
  // MessageText:
  //
  // This file is corrupt: it contains an invalid media type.%0
  //
  VFW_E_INVALID_MEDIA_TYPE            = HResult($80040248);
  {$EXTERNALSYM VFW_E_INVALID_MEDIA_TYPE}

   // Message id from WINWarning.H
  //
  // MessageId: VFW_E_BAD_KEY
  //
  // MessageText:
  //
  // A registry entry is corrupt.%0
  //
  VFW_E_BAD_KEY                       = HResult($800403F2);
  {$EXTERNALSYM VFW_E_BAD_KEY}

   // Message id from WINWarning.H
  //
  // MessageId: VFW_S_NO_MORE_ITEMS
  //
  // MessageText:
  //
  // The end of the list has been reached.%0
  //
  VFW_S_NO_MORE_ITEMS                 = HResult($00040103);
  {$EXTERNALSYM VFW_S_NO_MORE_ITEMS}

  //
  // MessageId: VFW_E_SAMPLE_TIME_NOT_SET
  //
  // MessageText:
  //
  // No time stamp has been set for this sample.%0
  //
  VFW_E_SAMPLE_TIME_NOT_SET           = HResult($80040249);
  {$EXTERNALSYM VFW_E_SAMPLE_TIME_NOT_SET}

  //
  // MessageId: VFW_S_RESOURCE_NOT_NEEDED
  //
  // MessageText:
  //
  // The resource specified is no longer needed.%0
  //
  VFW_S_RESOURCE_NOT_NEEDED           = HResult($00040250);
  {$EXTERNALSYM VFW_S_RESOURCE_NOT_NEEDED}

  //
  // MessageId: VFW_E_MEDIA_TIME_NOT_SET
  //
  // MessageText:
  //
  // No media time stamp has been set for this sample.%0
  //
  VFW_E_MEDIA_TIME_NOT_SET            = HResult($80040251);
  {$EXTERNALSYM VFW_E_MEDIA_TIME_NOT_SET}

  //
  // MessageId: VFW_E_NO_TIME_FORMAT_SET
  //
  // MessageText:
  //
  // No media time format has been selected.%0
  //
  VFW_E_NO_TIME_FORMAT_SET            = HResult($80040252);
  {$EXTERNALSYM VFW_E_NO_TIME_FORMAT_SET}

  //
  // MessageId: VFW_E_MONO_AUDIO_HW
  //
  // MessageText:
  //
  // Cannot change balance because audio device is mono only.%0
  //
  VFW_E_MONO_AUDIO_HW                 = HResult($80040253);
  {$EXTERNALSYM VFW_E_MONO_AUDIO_HW}

  //
  // MessageId: VFW_S_MEDIA_TYPE_IGNORED
  //
  // MessageText:
  //
  // A connection could not be made with the media type in the persistent graph,%0
  // but has been made with a negotiated media type.%0
  //
  VFW_S_MEDIA_TYPE_IGNORED            = HResult($00040254);
  {$EXTERNALSYM VFW_S_MEDIA_TYPE_IGNORED}

  //
  // MessageId: VFW_E_NO_DECOMPRESSOR
  //
  // MessageText:
  //
  // Cannot play back the video stream: no suitable decompressor could be found.%0
  //
  VFW_E_NO_DECOMPRESSOR               = HResult($80040255);
  {$EXTERNALSYM VFW_E_NO_DECOMPRESSOR}

  //
  // MessageId: VFW_E_NO_AUDIO_HARDWARE
  //
  // MessageText:
  //
  // Cannot play back the audio stream: no audio hardware is available, or the hardware is not responding.%0
  //
  VFW_E_NO_AUDIO_HARDWARE             = HResult($80040256);
  {$EXTERNALSYM VFW_E_NO_AUDIO_HARDWARE}

  //
  // MessageId: VFW_S_VIDEO_NOT_RENDERED
  //
  // MessageText:
  //
  // Cannot play back the video stream: no suitable decompressor could be found.%0
  //
  VFW_S_VIDEO_NOT_RENDERED            = HResult($00040257);
  {$EXTERNALSYM VFW_S_VIDEO_NOT_RENDERED}

  //
  // MessageId: VFW_S_AUDIO_NOT_RENDERED
  //
  // MessageText:
  //
  // Cannot play back the audio stream: no audio hardware is available.%0
  //
  VFW_S_AUDIO_NOT_RENDERED            = HResult($00040258);
  {$EXTERNALSYM VFW_S_AUDIO_NOT_RENDERED}

  //
  // MessageId: VFW_E_RPZA
  //
  // MessageText:
  //
  // Cannot play back the video stream: format 'RPZA' is not supported.%0
  //
  VFW_E_RPZA                          = HResult($80040259);
  {$EXTERNALSYM VFW_E_RPZA}

  //
  // MessageId: VFW_S_RPZA
  //
  // MessageText:
  //
  // Cannot play back the video stream: format 'RPZA' is not supported.%0
  //
  VFW_S_RPZA                          = HResult($0004025A);
  {$EXTERNALSYM VFW_S_RPZA}

  //
  // MessageId: VFW_E_PROCESSOR_NOT_SUITABLE
  //
  // MessageText:
  //
  // ActiveMovie cannot play MPEG movies on this processor.%0
  //
  VFW_E_PROCESSOR_NOT_SUITABLE        = HResult($8004025B);
  {$EXTERNALSYM VFW_E_PROCESSOR_NOT_SUITABLE}

  //
  // MessageId: VFW_E_UNSUPPORTED_AUDIO
  //
  // MessageText:
  //
  // Cannot play back the audio stream: the audio format is not supported.%0
  //
  VFW_E_UNSUPPORTED_AUDIO             = HResult($8004025C);
  {$EXTERNALSYM VFW_E_UNSUPPORTED_AUDIO}

  //
  // MessageId: VFW_E_UNSUPPORTED_VIDEO
  //
  // MessageText:
  //
  // Cannot play back the video stream: the video format is not supported.%0
  //
  VFW_E_UNSUPPORTED_VIDEO             = HResult($8004025D);
  {$EXTERNALSYM VFW_E_UNSUPPORTED_VIDEO}

  //
  // MessageId: VFW_E_MPEG_NOT_CONSTRAINED
  //
  // MessageText:
  //
  // ActiveMovie cannot play this video stream because it falls outside the constrained standard.%0
  //
  VFW_E_MPEG_NOT_CONSTRAINED          = HResult($8004025E);
  {$EXTERNALSYM VFW_E_MPEG_NOT_CONSTRAINED}

  //
  // MessageId: VFW_E_NOT_IN_GRAPH
  //
  // MessageText:
  //
  // Cannot perform the requested function on an object that is not in the filter graph.%0
  //
  VFW_E_NOT_IN_GRAPH                  = HResult($8004025F);
  {$EXTERNALSYM VFW_E_NOT_IN_GRAPH}

  //
  // MessageId: VFW_S_ESTIMATED
  //
  // MessageText:
  //
  // The value returned had to be estimated.  It's accuracy can not be guaranteed.%0
  //
  VFW_S_ESTIMATED                     = HResult($00040260);
  {$EXTERNALSYM VFW_S_ESTIMATED}

  //
  // MessageId: VFW_E_NO_TIME_FORMAT
  //
  // MessageText:
  //
  // Cannot get or set time related information on an object that is using a time format of TIME_FORMAT_NONE.%0
  //
  VFW_E_NO_TIME_FORMAT                = HResult($80040261);
  {$EXTERNALSYM VFW_E_NO_TIME_FORMAT}

  //
  // MessageId: VFW_E_READ_ONLY
  //
  // MessageText:
  //
  // The connection cannot be made because the stream is read only and the filter alters the data.%0
  //
  VFW_E_READ_ONLY                     = HResult($80040262);
  {$EXTERNALSYM VFW_E_READ_ONLY}

  //
  // MessageId: VFW_S_RESERVED
  //
  // MessageText:
  //
  // This success code is reserved for internal purposes within ActiveMovie.%0
  //
  VFW_S_RESERVED                      = HResult($00040263);
  {$EXTERNALSYM VFW_S_RESERVED}

  //
  // MessageId: VFW_E_BUFFER_UNDERFLOW
  //
  // MessageText:
  //
  // The buffer is not full enough.%0
  //
  VFW_E_BUFFER_UNDERFLOW              = HResult($80040264);
  {$EXTERNALSYM VFW_E_BUFFER_UNDERFLOW}

  //
  // MessageId: VFW_E_UNSUPPORTED_STREAM
  //
  // MessageText:
  //
  // Cannot play back the file.  The format is not supported.%0
  //
  VFW_E_UNSUPPORTED_STREAM            = HResult($80040265);
  {$EXTERNALSYM VFW_E_UNSUPPORTED_STREAM}

  //
  // MessageId: VFW_E_NO_TRANSPORT
  //
  // MessageText:
  //
  // Pins cannot connect due to not supporting the same transport.%0
  //
  VFW_E_NO_TRANSPORT                  = HResult($80040266);
  {$EXTERNALSYM VFW_E_NO_TRANSPORT}

  //
  // MessageId: VFW_S_STREAM_OFF
  //
  // MessageText:
  //
  // The stream has been turned off.%0
  //
  VFW_S_STREAM_OFF                    = HResult($00040267);
  {$EXTERNALSYM VFW_S_STREAM_OFF}

  //
  // MessageId: VFW_S_CANT_CUE
  //
  // MessageText:
  //
  // The graph can't be cued because of lack of or corrupt data.%0
  //
  VFW_S_CANT_CUE                      = HResult($00040268);
  {$EXTERNALSYM VFW_S_CANT_CUE}

  //
  // MessageId: VFW_E_BAD_VIDEOCD
  //
  // MessageText:
  //
  // The Video CD can't be read correctly by the device or is the data is corrupt.%0
  //
  VFW_E_BAD_VIDEOCD                   = HResult($80040269);
  {$EXTERNALSYM VFW_E_BAD_VIDEOCD}

  //
  // MessageId: VFW_S_NO_STOP_TIME
  //
  // MessageText:
  //
  // The stop time for the sample was not set.%0
  //
  VFW_S_NO_STOP_TIME                  = HResult($00040270);
  {$EXTERNALSYM VFW_S_NO_STOP_TIME}

  //
  // MessageId: VFW_E_OUT_OF_VIDEO_MEMORY
  //
  // MessageText:
  //
  // There is not enough Video Memory at this display resolution and number of colors. Reducing resolution might help.%0
  //
  VFW_E_OUT_OF_VIDEO_MEMORY           = HResult($80040271);
  {$EXTERNALSYM VFW_E_OUT_OF_VIDEO_MEMORY}

  //
  // MessageId: VFW_E_VP_NEGOTIATION_FAILED
  //
  // MessageText:
  //
  // The VideoPort connection negotiation process has failed.%0
  //
  VFW_E_VP_NEGOTIATION_FAILED         = HResult($80040272);
  {$EXTERNALSYM VFW_E_VP_NEGOTIATION_FAILED}

  //
  // MessageId: VFW_E_DDRAW_CAPS_NOT_SUITABLE
  //
  // MessageText:
  //
  // Either DirectDraw has not been installed or the Video Card capabilities are not suitable. Make sure the display is not in 16 color mode or try changing the graphics mode.%0
  //
  VFW_E_DDRAW_CAPS_NOT_SUITABLE       = HResult($80040273);
  {$EXTERNALSYM VFW_E_DDRAW_CAPS_NOT_SUITABLE}

  //
  // MessageId: VFW_E_NO_VP_HARDWARE
  //
  // MessageText:
  //
  // No VideoPort hardware is available, or the hardware is not responding.%0
  //
  VFW_E_NO_VP_HARDWARE                = HResult($80040274);
  {$EXTERNALSYM VFW_E_NO_VP_HARDWARE}

  //
  // MessageId: VFW_E_NO_CAPTURE_HARDWARE
  //
  // MessageText:
  //
  // No Capture hardware is available, or the hardware is not responding.%0
  //
  VFW_E_NO_CAPTURE_HARDWARE           = HResult($80040275);
  {$EXTERNALSYM VFW_E_NO_CAPTURE_HARDWARE}

  //
  // MessageId: VFW_E_DVD_OPERATION_INHIBITED
  //
  // MessageText:
  //
  // This User Operation is inhibited by DVD Content at this time.%0
  //
  VFW_E_DVD_OPERATION_INHIBITED       = HResult($80040276);
  {$EXTERNALSYM VFW_E_DVD_OPERATION_INHIBITED}

  //
  // MessageId: VFW_E_DVD_INVALIDDOMAIN
  //
  // MessageText:
  //
  // This Operation is not permitted in the current domain.%0
  //
  VFW_E_DVD_INVALIDDOMAIN             = HResult($80040277);
  {$EXTERNALSYM VFW_E_DVD_INVALIDDOMAIN}

  //
  // MessageId: VFW_E_DVD_NO_BUTTON
  //
  // MessageText:
  //
  // The specified button is invalid or is not present at the current time, or there is no button present at the specified location.%0
  //
  VFW_E_DVD_NO_BUTTON                 = HResult($80040278);
  {$EXTERNALSYM VFW_E_DVD_NO_BUTTON}

  //
  // MessageId: VFW_E_DVD_GRAPHNOTREADY
  //
  // MessageText:
  //
  // DVD-Video playback graph has not been built yet.%0
  //
  VFW_E_DVD_GRAPHNOTREADY             = HResult($80040279);
  {$EXTERNALSYM VFW_E_DVD_GRAPHNOTREADY}

  //
  // MessageId: VFW_E_DVD_RENDERFAIL
  //
  // MessageText:
  //
  // DVD-Video playback graph building failed.%0
  //
  VFW_E_DVD_RENDERFAIL                = HResult($8004027A);
  {$EXTERNALSYM VFW_E_DVD_RENDERFAIL}

  //
  // MessageId: VFW_E_DVD_DECNOTENOUGH
  //
  // MessageText:
  //
  // DVD-Video playback graph could not be built due to insufficient decoders.%0
  //
  VFW_E_DVD_DECNOTENOUGH              = HResult($8004027B);
  {$EXTERNALSYM VFW_E_DVD_DECNOTENOUGH}

  //
  // MessageId: VFW_E_DDRAW_VERSION_NOT_SUITABLE
  //
  // MessageText:
  //
  // Version number of DirectDraw not suitable. Make sure to install dx5 or higher version.%0
  //
  VFW_E_DDRAW_VERSION_NOT_SUITABLE    = HResult($8004027C);
  {$EXTERNALSYM VFW_E_DDRAW_VERSION_NOT_SUITABLE}

  //
  // MessageId: VFW_E_COPYPROT_FAILED
  //
  // MessageText:
  //
  // Copy protection cannot be enabled. Please make sure any other copy protected content is not being shown now.%0
  //
  VFW_E_COPYPROT_FAILED               = HResult($8004027D);
  {$EXTERNALSYM VFW_E_COPYPROT_FAILED}

  //
  // MessageId: VFW_S_NOPREVIEWPIN
  //
  // MessageText:
  //
  // There was no preview pin available, so the capture pin output is being split to provide both capture and preview.%0
  //
  VFW_S_NOPREVIEWPIN                  = HResult($0004027E);
  {$EXTERNALSYM VFW_S_NOPREVIEWPIN}

  //
  // MessageId: VFW_E_TIME_EXPIRED
  //
  // MessageText:
  //
  // This object cannot be used anymore as its time has expired.%0
  //
  VFW_E_TIME_EXPIRED                  = HResult($8004027F);
  {$EXTERNALSYM VFW_E_TIME_EXPIRED}

  //
  // MessageId: VFW_S_DVD_NON_ONE_SEQUENTIAL
  //
  // MessageText:
  //
  // The current title was not a sequential set of chapters (PGC), and the returned timing information might not be continuous.%0
  //
  VFW_S_DVD_NON_ONE_SEQUENTIAL        = HResult($00040280);
  {$EXTERNALSYM VFW_S_DVD_NON_ONE_SEQUENTIAL}

  //
  // MessageId: VFW_E_DVD_WRONG_SPEED
  //
  // MessageText:
  //
  // The operation cannot be performed at the current playback speed.%0
  //
  VFW_E_DVD_WRONG_SPEED               = HResult($80040281);
  {$EXTERNALSYM VFW_E_DVD_WRONG_SPEED}

  //
  // MessageId: VFW_E_DVD_MENU_DOES_NOT_EXIST
  //
  // MessageText:
  //
  // The specified menu doesn't exist.%0
  //
  VFW_E_DVD_MENU_DOES_NOT_EXIST       = HResult($80040282);
  {$EXTERNALSYM VFW_E_DVD_MENU_DOES_NOT_EXIST}

  //
  // MessageId: VFW_E_DVD_CMD_CANCELLED
  //
  // MessageText:
  //
  // The specified command was either cancelled or no longer exists.%0
  //
  VFW_E_DVD_CMD_CANCELLED             = HResult($80040283);
  {$EXTERNALSYM VFW_E_DVD_CMD_CANCELLED}

  //
  // MessageId: VFW_E_DVD_STATE_WRONG_VERSION
  //
  // MessageText:
  //
  // The data did not contain a recognized version.%0
  //
  VFW_E_DVD_STATE_WRONG_VERSION       = HResult($80040284);
  {$EXTERNALSYM VFW_E_DVD_STATE_WRONG_VERSION}

  //
  // MessageId: VFW_E_DVD_STATE_CORRUPT
  //
  // MessageText:
  //
  // The state data was corrupt.%0
  //
  VFW_E_DVD_STATE_CORRUPT             = HResult($80040285);
  {$EXTERNALSYM VFW_E_DVD_STATE_CORRUPT}

  //
  // MessageId: VFW_E_DVD_STATE_WRONG_DISC
  //
  // MessageText:
  //
  // The state data is from a different disc.%0
  //
  VFW_E_DVD_STATE_WRONG_DISC          = HResult($80040286);
  {$EXTERNALSYM VFW_E_DVD_STATE_WRONG_DISC}

  //
  // MessageId: VFW_E_DVD_INCOMPATIBLE_REGION
  //
  // MessageText:
  //
  // The region was not compatible with the current drive.%0
  //
  VFW_E_DVD_INCOMPATIBLE_REGION       = HResult($80040287);
  {$EXTERNALSYM VFW_E_DVD_INCOMPATIBLE_REGION}

  //
  // MessageId: VFW_E_DVD_NO_ATTRIBUTES
  //
  // MessageText:
  //
  // The requested DVD stream attribute does not exist.%0
  //
  VFW_E_DVD_NO_ATTRIBUTES             = HResult($80040288);
  {$EXTERNALSYM VFW_E_DVD_NO_ATTRIBUTES}

  //
  // MessageId: VFW_E_DVD_NO_GOUP_PGC
  //
  // MessageText:
  //
  // Currently there is no GoUp (Annex J user function) program chain (PGC).%0
  //
  VFW_E_DVD_NO_GOUP_PGC               = HResult($80040289);
  {$EXTERNALSYM VFW_E_DVD_NO_GOUP_PGC}

  //
  // MessageId: VFW_E_DVD_LOW_PARENTAL_LEVEL
  //
  // MessageText:
  //
  // The current parental level was too low.%0
  //
  VFW_E_DVD_LOW_PARENTAL_LEVEL        = HResult($8004028A);
  {$EXTERNALSYM VFW_E_DVD_LOW_PARENTAL_LEVEL}

  //
  // MessageId: VFW_E_DVD_NOT_IN_KARAOKE_MODE
  //
  // MessageText:
  //
  // The current audio is not karaoke content.%0
  //
  VFW_E_DVD_NOT_IN_KARAOKE_MODE       = HResult($8004028B);
  {$EXTERNALSYM VFW_E_DVD_NOT_IN_KARAOKE_MODE}

  //
  // MessageId: VFW_S_DVD_CHANNEL_CONTENTS_NOT_AVAILABLE
  //
  // MessageText:
  //
  // The audio stream did not contain sufficient information to determine the contents of each channel.%0
  //
  VFW_S_DVD_CHANNEL_CONTENTS_NOT_AVAILABLE= HResult($0004028C);
  {$EXTERNALSYM VFW_S_DVD_CHANNEL_CONTENTS_NOT_AVAILABLE}

  //
  // MessageId: VFW_S_DVD_NOT_ACCURATE
  //
  // MessageText:
  //
  // The seek into the movie was not frame accurate.%0
  //
  VFW_S_DVD_NOT_ACCURATE              = HResult($0004028D);
  {$EXTERNALSYM VFW_S_DVD_NOT_ACCURATE}

  //
  // MessageId: VFW_E_FRAME_STEP_UNSUPPORTED
  //
  // MessageText:
  //
  // Frame step is not supported on this configuration.%0
  //
  VFW_E_FRAME_STEP_UNSUPPORTED        = HResult($8004028E);
  {$EXTERNALSYM VFW_E_FRAME_STEP_UNSUPPORTED}

  //
  // MessageId: VFW_E_DVD_STREAM_DISABLED
  //
  // MessageText:
  //
  // The specified stream is disabled and cannot be selected.%0
  //
  VFW_E_DVD_STREAM_DISABLED           = HResult($8004028F);
  {$EXTERNALSYM VFW_E_DVD_STREAM_DISABLED}

  //
  // MessageId: VFW_E_DVD_TITLE_UNKNOWN
  //
  // MessageText:
  //
  // The operation depends on the current title number, however the navigator has not yet entered the VTSM or the title domains,
  // so the 'current' title index is unknown.%0
  //
  VFW_E_DVD_TITLE_UNKNOWN             = HResult($80040290);
  {$EXTERNALSYM VFW_E_DVD_TITLE_UNKNOWN}

  //
  // MessageId: VFW_E_DVD_INVALID_DISC
  //
  // MessageText:
  //
  // The specified path does not point to a valid DVD disc.%0
  //
  VFW_E_DVD_INVALID_DISC              = HResult($80040291);
  {$EXTERNALSYM VFW_E_DVD_INVALID_DISC}

  //
  // MessageId: VFW_E_DVD_NO_RESUME_INFORMATION
  //
  // MessageText:
  //
  // There is currently no resume information.%0
  //
  VFW_E_DVD_NO_RESUME_INFORMATION     = HResult($80040292);
  {$EXTERNALSYM VFW_E_DVD_NO_RESUME_INFORMATION}

  //
  // MessageId: VFW_E_PIN_ALREADY_BLOCKED_ON_THIS_THREAD
  //
  // MessageText:
  //
  // This thread has already blocked this output pin.  There is no need to call IPinFlowControl::Block() again.%0
  //
  VFW_E_PIN_ALREADY_BLOCKED_ON_THIS_THREAD= HResult($80040293);
  {$EXTERNALSYM VFW_E_PIN_ALREADY_BLOCKED_ON_THIS_THREAD}

  //
  // MessageId: VFW_E_PIN_ALREADY_BLOCKED
  //
  // MessageText:
  //
  // IPinFlowControl::Block() has been called on another thread.  The current thread cannot make any assumptions about this pin's block state.%0
  //
  VFW_E_PIN_ALREADY_BLOCKED           = HResult($80040294);
  {$EXTERNALSYM VFW_E_PIN_ALREADY_BLOCKED}

  //
  // MessageId: VFW_E_CERTIFICATION_FAILURE
  //
  // MessageText:
  //
  // An operation failed due to a certification failure.%0
  //
  VFW_E_CERTIFICATION_FAILURE         = HResult($80040295);
  {$EXTERNALSYM VFW_E_CERTIFICATION_FAILURE}

  //
  // MessageId: VFW_E_VMR_NOT_IN_MIXER_MODE
  //
  // MessageText:
  //
  // The VMR has not yet created a mixing component.  That is, IVMRFilterConfig::SetNumberofStreams has not yet been called.%0
  //
  VFW_E_VMR_NOT_IN_MIXER_MODE         = HResult($80040296);
  {$EXTERNALSYM VFW_E_VMR_NOT_IN_MIXER_MODE}

  //
  // MessageId: VFW_E_VMR_NO_AP_SUPPLIED
  //
  // MessageText:
  //
  // The application has not yet provided the VMR filter with a valid allocator-presenter object.%0
  //
  VFW_E_VMR_NO_AP_SUPPLIED            = HResult($80040297);
  {$EXTERNALSYM VFW_E_VMR_NO_AP_SUPPLIED}

  //
  // MessageId: VFW_E_VMR_NO_DEINTERLACE_HW
  //
  // MessageText:
  //
  // The VMR could not find any de-interlacing hardware on the current display device.%0
  //
  VFW_E_VMR_NO_DEINTERLACE_HW         = HResult($80040298);
  {$EXTERNALSYM VFW_E_VMR_NO_DEINTERLACE_HW}

  //
  // MessageId: VFW_E_VMR_NO_PROCAMP_HW
  //
  // MessageText:
  //
  // The VMR could not find any ProcAmp hardware on the current display device.%0
  //
  VFW_E_VMR_NO_PROCAMP_HW             = HResult($80040299);
  {$EXTERNALSYM VFW_E_VMR_NO_PROCAMP_HW}

  //
  // MessageId: VFW_E_DVD_VMR9_INCOMPATIBLEDEC
  //
  // MessageText:
  //
  // VMR9 does not work with VPE-based hardware decoders.%0
  //
  VFW_E_DVD_VMR9_INCOMPATIBLEDEC      = HResult($8004029A);
  {$EXTERNALSYM VFW_E_DVD_VMR9_INCOMPATIBLEDEC}

  //
  // MessageId: VFW_E_NO_COPP_HW
  //
  // MessageText:
  //
  //  The current display device does not support Content Output Protection Protocol (COPP) H/W.%0
  //
  VFW_E_NO_COPP_HW                    = HResult($8004029B);
  {$EXTERNALSYM VFW_E_NO_COPP_HW}

  //
  // MessageId: VFW_E_DVD_NONBLOCKING
  //
  // MessageText:
  //
  //  Nonblocking APIs are enabled and the nav cannot perform the operation at this time (IDVDControl) or partially incomplete information may be returned (DVDInfo).%0
  //
  VFW_E_DVD_NONBLOCKING               = HResult($8004029C);
  {$EXTERNALSYM VFW_E_DVD_NONBLOCKING}

  //
  // MessageId: VFW_E_DVD_TOO_MANY_RENDERERS_IN_FILTER_GRAPH
  //
  // MessageText:
  //
  // The DVD Graph Builder cannot build a DVD playback filter graph if the EVR and another renderer are in the graph builder's filter graph.
  //
  VFW_E_DVD_TOO_MANY_RENDERERS_IN_FILTER_GRAPH= HResult($8004029D);
  {$EXTERNALSYM VFW_E_DVD_TOO_MANY_RENDERERS_IN_FILTER_GRAPH}

  //
  // MessageId: VFW_E_DVD_NON_EVR_RENDERER_IN_FILTER_GRAPH
  //
  // MessageText:
  //
  // AM_DVD_EVR_ONLY cannot be passed to RenderDvdVideoVolume if the graph builder's filter graph contains a renderer which is not the EVR.
  //
  VFW_E_DVD_NON_EVR_RENDERER_IN_FILTER_GRAPH= HResult($8004029E);
  {$EXTERNALSYM VFW_E_DVD_NON_EVR_RENDERER_IN_FILTER_GRAPH}

  //
  // MessageId: VFW_E_DVD_RESOLUTION_ERROR
  //
  // MessageText:
  //
  // DVD Video Output is not at a proper resolution.
  //
  VFW_E_DVD_RESOLUTION_ERROR          = HResult($8004029F);
  {$EXTERNALSYM VFW_E_DVD_RESOLUTION_ERROR}

  //
  //
  // E_PROP_SET_UNSUPPORTED and E_PROP_ID_UNSUPPORTED are added here using
  // HRESULT_FROM_WIN32() because VC5 doesn't have WinNT's new error codes
  // from winerror.h, and because it is more convienent to have them already
  // formed as HRESULTs.  These should correspond to:
  //     HRESULT_FROM_WIN32(ERROR_NOT_FOUND)     == E_PROP_ID_UNSUPPORTED
  //     HRESULT_FROM_WIN32(ERROR_SET_NOT_FOUND) == E_PROP_SET_UNSUPPORTED

  //
  // MessageId: E_PROP_SET_UNSUPPORTED
  //
  // MessageText:
  //
  // The Specified property set is not supported.%0
  //
  E_PROP_SET_UNSUPPORTED              = HResult($80070492);
  {$EXTERNALSYM E_PROP_SET_UNSUPPORTED}

  //
  // MessageId: E_PROP_ID_UNSUPPORTED
  //
  // MessageText:
  //
  // The specified property ID is not supported for the specified property set.%0
  //
  E_PROP_ID_UNSUPPORTED               = HResult($80070490);
  {$EXTERNALSYM E_PROP_ID_UNSUPPORTED}

  //
  // MessageId: VFW_E_CODECAPI_LINEAR_RANGE
  //
  // MessageText:
  //
  // Parameter has linear range.%0
  //
  VFW_E_CODECAPI_LINEAR_RANGE         = HResult($80040310);
  {$EXTERNALSYM VFW_E_CODECAPI_LINEAR_RANGE}

  //
  // MessageId: VFW_E_CODECAPI_ENUMERATED
  //
  // MessageText:
  //
  // Parameter is enumerated. It has no range.%0
  //
  VFW_E_CODECAPI_ENUMERATED           = HResult($80040311);
  {$EXTERNALSYM VFW_E_CODECAPI_ENUMERATED}

  //
  // MessageId: VFW_E_CODECAPI_NO_DEFAULT
  //
  // MessageText:
  //
  // No default value.%0
  //
  VFW_E_CODECAPI_NO_DEFAULT           = HResult($80040313);
  {$EXTERNALSYM VFW_E_CODECAPI_NO_DEFAULT}

  //
  // MessageId: VFW_E_CODECAPI_NO_CURRENT_VALUE
  //
  // MessageText:
  //
  // No current value.%0
  //
  VFW_E_CODECAPI_NO_CURRENT_VALUE     = HResult($80040314);
  {$EXTERNALSYM VFW_E_CODECAPI_NO_CURRENT_VALUE}

  //
  // MessageId: VFW_E_DVD_CHAPTER_DOES_NOT_EXIST
  //
  // MessageText:
  //
  // The operation failed since the chapter does not exist.%0
  //
  VFW_E_DVD_CHAPTER_DOES_NOT_EXIST    = HResult($80040315);
  {$EXTERNALSYM VFW_E_DVD_CHAPTER_DOES_NOT_EXIST}

  //
  // MessageId: VFW_S_DVD_RENDER_STATUS
  //
  // MessageText:
  //
  // The operation succeeded but some streams were not rendered.%0
  //
  VFW_S_DVD_RENDER_STATUS             = HResult($00040320);
  {$EXTERNALSYM VFW_S_DVD_RENDER_STATUS}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional Prototypes here.

end.
