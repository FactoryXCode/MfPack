// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Media Foundation Windows Sonic
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.WinSonic.SpatialAudioMetadata.pas
// Kind: Pascal / Delphi unit
// Release date: 29-05-2018
// Language: ENU
//
// Revision Version: 3.0.0
// Description: Spatial Audio position is a standard Microsoft define metadata command which
// represents volumetric coordinates about the listener in the standard model used by
// SpatialAudioClient, where center of listeners head is 0,0,0 and distance fo 1.0 = 
// 1 meter.
// Shared part of the Core Audio Interfaces
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
// Remarks: Requires Windows 10 RedStone 1 or later.
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
// Source: SpatialAudioMetadata.h
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
//==============================================================================
unit WinApi.CoreAudioApi.SpatialAudioMetadata;

  {$HPPEMIT '#include "SpatialAudioMetadata.h"'}

interface

uses
  {MfPack}
  WinApi.WinApiTypes,
  WinApi.WinMM.MMReg,
  {ActiveX}
  WinApi.ActiveX.PropIdl,
  {CoreAudioApi}
  WinApi.CoreAudioApi.Audiosessiontypes,
  WinApi.CoreAudioApi.SpatialAudioClient;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


type
  // Writer Overflow is defined as attempting to wri e more items into metadataBuffer than
  // maxItems specified when metadataBuffer was created

  PSpatialAudioMetadataWriterOverflowMode = ^SpatialAudioMetadataWriterOverflowMode;
  SpatialAudioMetadataWriterOverflowMode    = (
    SpatialAudioMetadataWriterOverflow_Fail = 0,        // Overflow will fail
    SpatialAudioMetadataWriterOverflow_MergeWithNew,    // Overflow will succeed, will merge overflow item with previous item and adopt frame offset of newest item
    SpatialAudioMetadataWriterOverflow_MergeWithLast    // Overflow will succeed, will merge overflow item with previous item and keep existing frame offset
  );
  {$EXTERNALSYM SpatialAudioMetadataWriterOverflowMode}


  // Copier modes of operation.  Supports full overwrite copy, appending and appending with item overflow merge behaviors
  // When there are more items than the destination can hold (overflow), the merge options allow the overflow items to
  // be merged together, and will take the most recent of duplicate commands.  Can adopt the offset of last or first items merged.
  PSpatialAudioMetadataCopyMode = ^SpatialAudioMetadataCopyMode;
  SpatialAudioMetadataCopyMode         = (
    SpatialAudioMetadataCopy_Overwrite = 0,          // Creates a direct copy of the specfied frameCount in destination buffer, overwrites any previous data
    SpatialAudioMetadataCopy_Append,                 // Normal append - will fail is resulting metadataBuffer has too many items
    SpatialAudioMetadataCopy_AppendMergeWithLast,    // Appends, if overflow occurs, extra items are merged into last item adopting last merged items offset
    SpatialAudioMetadataCopy_AppendMergeWithFirst    // Appends, if overflow occurs, extra items are merged assigning the offset of the first non-overflow item offset
  );
  {$EXTERNALSYM SpatialAudioMetadataCopyMode}

  // provides full set of data available on a SpatialAudioMetadataItems object

  PSpatialAudioMetadataItemsInfo = ^SpatialAudioMetadataItemsInfo;
  SpatialAudioMetadataItemsInfo = record
    FrameCount: UINT16;              // total frame count that defines valid item offsets
    ItemCount: UINT16;               // Current number of items stored
    MaxItemCount: UINT16;            // Max number of items allowed (defined at creation time)
    MaxValueBufferLength: UINT32;    // Size of largest command value defined by metadataFormat
  end;
  {$EXTERNALSYM SpatialAudioMetadataItemsInfo}


  PSpatialAudioObjectRenderStreamForMetadataActivationParams = ^SpatialAudioObjectRenderStreamForMetadataActivationParams;
  SpatialAudioObjectRenderStreamForMetadataActivationParams = record
    ObjectFormat: WAVEFORMATEX;              // Format descriptor for a single spatial audio objects. All objects must have the same format and must be of type WAVEFORMATEX or WAVEFORMATEXTENSIBLE.
    StaticObjectTypeMask: AudioObjectType;   // (static channel bed mask) mask of static audio object type that are allowed
    MinDynamicObjectCount: UINT32;           // Minimum number of dynamic audio objects. If at least this count cannot be granted, no dynamic objects will be granted.
    MaxDynamicObjectCount: UINT32;           // Maximum number of dynamic audio objects that can be activated via ISpatialAudioObjectRenderStreamForMetadata.
    Category: AUDIO_STREAM_CATEGORY;         // Specifies the category of the audio stream and its spatial audio objects.
    EventHandle: THandle;                     // event that will signal the need for more audio data. This handle will be duplicated internally before getting used
    MetadataFormatId: TGUID;                 // Specifies the metadataFormat that for the currently active spatial rendering engine
    MaxMetadataItemCount: UINT16;            // Maximum number of metadata Items Per FrameCount
    MetadataActivationParams: PROPVARIANT;
    NotifyObject: ISpatialAudioObjectRenderStreamNotify;
  end;
  {$EXTERNALSYM SpatialAudioObjectRenderStreamForMetadataActivationParams}


type


  // ISpatialAudioMetadataItems interface
  // ====================================
  //
  // <summary>
  //     The ISpatialAudioMetadataItems interface implements objects used to
  //     store metadata items at frame offsets within a specified range of
  //     of valid frame offset postions defined by frameCount.
  //     To write items into this object use SpatialAudioMetadataWriter
  //     To Read items out of this object use SpatialAudioMetadataReader
  //     To Copy items out of this object use SpatialAudioMetadataCopier
  // </summary>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioMetadataItems);'}
  {$EXTERNALSYM ISpatialAudioMetadataItems}
  ISpatialAudioMetadataItems = interface(IUnknown)
  ['{BCD7C78F-3098-4F22-B547-A2F25A381269}']

    function GetFrameCount(out frameCount: UINT16): HRESULT; stdcall;
    // total frame count that defines valid item offsets

    function GetItemCount(out itemCount: UINT16): HRESULT; stdcall;
    // Current number of items stored

    function GetMaxItemCount(out maxItemCount: UINT16): HRESULT; stdcall;
    // Max number of items allowed (defined at creation time)

    function GetMaxValueBufferLength(out maxValueBufferLength: UINT32): HRESULT; stdcall;
    // Size of largest command value defined by metadataFormat

    function GetInfo(out info: SpatialAudioMetadataItemsInfo): HRESULT; stdcall;
    // provides full set of data available on a SpatialAudioMetadataItems object

  end;
  IID_ISpatialAudioMetadataItems = ISpatialAudioMetadataItems;
  {$EXTERNALSYM IID_ISpatialAudioMetadataItems}


  // ISpatialAudioMetadataWriter interface
  // =====================================
  //
  // <summary>
  //     The ISpatialAudioMetadataWriter interface provides methods for storing metadata items
  //     positioned within a range of corresponding audio frames. Each item has a zero based
  //     offset position within the specified frame. Each item can contain one or more commands
  //     specific to the metadataFormatId provided when SpatialAudioMetadataClient was created.
  //     This object does not allocate storage for the metadata it is provided, the caller is expected
  //     to manage allocation of memory used to store the packed data.
  //
  //     Multiple metadata items can be placed in the SpatialAudioMetadataItems object. For each item,
  //     call WriteNextItem followed by calls to WriteNextItemCommand.
  // </summary>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioMetadataWriter);'}
  {$EXTERNALSYM ISpatialAudioMetadataWriter}
  ISpatialAudioMetadataWriter = interface(IUnknown)
  ['{1B17CA01-2955-444D-A430-537DC589A844}']

    function Open(metadataItems: ISpatialAudioMetadataItems): HRESULT; stdcall;
    // <summary>
    //     The Open() method opens a SpatialAudioMetadataItems object for writing
    // </summary>
    // <param name="metadataItems">
    //     Points to SpatialAudioMetadataItems Object to be written into
    // </param>

    function WriteNextItem(frameOffset: UINT16): HRESULT; stdcall;
    // <summary>
    //     The WriteNextItem() method starts a new metadata item as the specified offset.
    // </summary>
    // <param name="frameOffset">
    //     Specifies the frame offset of the item within the objects frameCount range.
    // </param>
    //
    // <remarks>
    // Can only be called on an open metadata buffer.  Must be called before calling WriteNextItemCommand.  Frame
    // offsets must be greater in value than previous WriteItem offset positions for this
    // metadataBuffer.
    // </remarks>

    function WriteNextItemCommand(commandID: Byte;
                                  valueBuffer: Pointer;
                                  valueBufferLength: UINT32): HRESULT; stdcall;
    // <summary>
    //     The WriteNextItemCommand() method adds metadata commands and value data to the current item.
    // </summary>
    // <param name="commandID">
    //     Specifies a command supported by the metadataFormat of this object.  Each command can
    //     can only be written once per item.  Call will fail if command not defined by metadataFormat.
    // </param>
    // <param name="valueBuffer">
    //     Pointer to a buffer which stores data specific to the command as specified by the
    //     metadataFormat definition.
    // </param>
    // <param name="valueBufferLength">
    //     Specifies the size in bytes of the command valueBuffer.  Size must match definition
    //     as specified by the metadataFormat or call will fail.
    // </param>
    //
    // <remarks>
    //  Valid commands and command value sizes are tied to the metadataFormatId and documented
    //  by the vendor of the spatial audio encoder/decoder.
    // </remarks>

    function Close(): HRESULT; stdcall;
    // <summary>
    //     The Close() method completes any needed operations on the metadataBuffer and will
    //     release the specified SpatialAudioMetadataItems object.
    // </summary>

  end;
  IID_ISpatialAudioMetadataWriter = ISpatialAudioMetadataWriter;
  {$EXTERNALSYM IID_ISpatialAudioMetadataWriter}


  // ISpatialAudioMetadataReader interface
  // =====================================
  //
  // <summary>
  //     The ISpatialAudioMetadataReader interface provides methods for extracting
  //     metadata items and item command value pairs from the specified SpatialAudioMetadataItems object.
  //     The SpatialAudioMetadataItems object, as populated by ISpatialAudioMetadataWriter, has a frameCount
  //     representing the valid range of item offsets.  The object maintains an internal read position.
  // </summary>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioMetadataReader);'}
  {$EXTERNALSYM ISpatialAudioMetadataReader}
  ISpatialAudioMetadataReader = interface(IUnknown)
  ['{B78E86A2-31D9-4C32-94D2-7DF40FC7EBEC}']
    
    function Open(metadataItems: ISpatialAudioMetadataItems): HRESULT; stdcall;
    // <summary>
    //     The Open() method reads metadata commands and value data from specified SpatialAudioMetadataItems object
    // </summary>
    // <param name="metadataItems">
    //     Specifies the SpatialAudioMetadataItems object which contains metadata populated by ISpatialAudioMetadataWriter,
    //     or ISpatialAudioMetadataCopier.
    // </param>
    
    function ReadNextItem(commandCount: UINT8;
                          out frameOffset: UINT16): HRESULT; stdcall;
    // <summary>
    //     The ReadNextItem() method is used to get the number of commands and the sample offset for the current item

    // </summary>
    // <param name="commandCount">
    //     Returns the total number of commands value pairs associated with the current item being read.
    // </param>
    // <param name="frameOffset">
    //     Returns the frame offset position of the current item being read.
    // </param>
    //
    // <remarks>
    //  Must be called before ReadNextItemCommand.
    // </remarks>
    
    function ReadNextItemCommand(out commandID: PByte;
                                 valueBuffer: Pointer;
                                 maxValueBufferLength: UINT32;
                                 out valueBufferLength: UINT32): HRESULT; stdcall;
    // <summary>
    //     The ReadNextItemCommand() method is used to get the commands for thee current item
    // </summary>
    // <param name="commandID">
    //     Returns the command ID for the current command
    // </param>
    // <param name="valueBuffer">
    //     Pointer to a buffer to receive the value data for this command.  Must be at least maxValueLength in size to
    //     ensure all commands can be successfully retrieved.
    // </param>
    // <param name="maxValueBufferLength">
    //     Specifies the maximum size of valueBuffer
    // </param>
    // <param name="valueBufferLength">
    //     returns total bytes copied into valueBuffer.
    // </param>
    //
    // <remarks>
    //  Can only be called after ReadNextItem.
    // </remarks>
    
    function Close(): HRESULT; stdcall;
    // <summary>
    //     The Close() method completes any needed operations on the SpatialAudioMetadataItems and will
    //     release to the SpatialAudioMetadataItems object.
    // </summary>

  end;
  IID_ISpatialAudioMetadataReader = ISpatialAudioMetadataReader;
  {$EXTERNALSYM IID_ISpatialAudioMetadataReader}



  // ISpatialAudioMetadataCopier interface
  // =====================================
  // <summary>
  //     The ISpatialAudioMetadataCopier interface provides methods for copying SpatialAudioMetadataItems.
  //     Callers will be able to copy all or subsets of metadata items from source SpatialAudioMetadataItems
  //     into a destination SpatialAudioMetadataItems.
  //     The SpatialAudioMetadataItems object, as populated by ISpatialAudioMetadata Writer or Copier, has a frameCount
  //     representing the valid range of item offsets.  This objects allows copying of
  //     groups of items within a copyFrameCount of the original specified frameCount.  The object
  //     maintains an internal read position, which is advanced by copyFrameCount on each copy.
  //     This copy object also supports appending the contents of a copy into an existing
  //     metadata buffer, and will adjust metadata item offsets accordingly.
  // </summary>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioMetadataCopier);'}
  {$EXTERNALSYM ISpatialAudioMetadataCopier}
  ISpatialAudioMetadataCopier = interface(IUnknown)
  ['{D224B233-E251-4FD0-9CA2-D5ECF9A68404}']

    function Open(metadataItems: ISpatialAudioMetadataItems): HRESULT; stdcall;
    // <summary>
    //     The Open() method reads metadata commands and value data from specified SpatialAudioMetadataItems object
    // </summary>
    // <param name="metadataItems">
    //     Specifies the SpatialAudioMetadataItems object which contains metadata populated by ISpatialAudioMetadata Writer or Copier.
    // </param>


    function CopyMetadataForFrames(copyFrameCount: UINT16;
                                   copyMode: SpatialAudioMetadataCopyMode;
                                   dstMetadataItems: ISpatialAudioMetadataItems;
                                   out itemsCopied: UINT16): HRESULT; stdcall;
    // <summary>
    //     The CopyMetadataForFrames() copies the metadata items which occur in the specified copyFrameCount from the currently
    //     open SpatialAudioMetadataItems into a destination SpatialAudioMetadataItems.  Each call will advance the internal copy position
    //     by the specified copyFrameCount.
    // </summary>
    // <param name="copyFrameCount">
    //     This specifies the number of frames from the current copy position for which copyFrame information is requested.
    //     A copyFrameCount of zero specifies the entire frame which the source SpatialAudioMetadataItems contains.
    //     After the copy the internal copy position within the source metadataBuffer frame will be advanced
    //     by this copyFrameCount.
    // </param>
    // <param name="copyMode">
    //     Specifies the copy mode as defined by SpatialAudioMetadataCopyMode enum.
    // </param>
    // <param name="dstMetadataItems">
    //    Specifies a destination SpatialAudioMetadataItems to contain the output of the copy
    // </param>
    // <param name="itemsCopied">
    //     Specifies the total number of new items copied.
    // </param>
    //
    // <remarks>
    //  This function merely performs a copy and advances the internal copy position.
    // </remarks>


    function Close(): HRESULT; stdcall;
    // <summary>
    //     The Close() method completes any needed operations on the source SpatialAudioMetadataItems and will
    //     release the specified SpatialAudioMetadataItems.
    // </summary>

  end;
  IID_ISpatialAudioMetadataCopier = ISpatialAudioMetadataCopier;
  {$EXTERNALSYM IID_ISpatialAudioMetadataCopier}


  // ISpatialAudioMetadataItemsBuffer interface
  // ==========================================
  //
  // <summary>
  //     This interface provides methods for attaching buffers to SpatialAudioMetadataItems for inplace storage of data
  //     Buffers can be attached, and it will reset the contents to the empty set of metadata items.  If a previously populated
  //     buffer can be attached again and retain the internal data stored in the buffer.
  // </summary>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioMetadataItemsBuffer);'}
  {$EXTERNALSYM ISpatialAudioMetadataItemsBuffer}
  ISpatialAudioMetadataItemsBuffer = interface(IUnknown)
  ['{42640A16-E1BD-42D9-9FF6-031AB71A2DBA}']

    function AttachToBuffer(buffer: PByte;
                            bufferLength: UINT32): HRESULT; stdcall;
    // <summary>
    //     Used to attach caller provided memory for storage of metadata Items.
    // </summary>
    // <param name="buffer">
    //     pointer to memory to use for storage
    // </param>
    // <param name="bufferLength">
    //     Length of buffer, must match the length required for format and maxitems
    // </param>


    function AttachToPopulatedBuffer(buffer: PByte;
                                     bufferLength: UINT32): HRESULT; stdcall;
    // <summary>
    //     Used to attach to caller provided memory which was previously populated
    // </summary>
    // <param name="buffer">
    //     pointer to memory to use for storage
    // </param>
    // <param name="bufferLength">
    //     Length of buffer, must match the length required for format and maxitems
    // </param>

    function DetachBuffer(): HRESULT; stdcall;
    // <summary>
    //     Detaches the buffer from this Item.  Memory can only be attached to a single metadata item at a time.
    // </summary>

  end;
  IID_ISpatialAudioMetadataItemsBuffer = ISpatialAudioMetadataItemsBuffer;
  {$EXTERNALSYM IID_ISpatialAudioMetadataItemsBuffer}


  // ISpatialAudioMetadataClient interface
  // =====================================
  //
  // <summary>
  //     The SpatialAudioMetadataClient provides a class factory for creating
  //     SpatialAudioMetadataItems, and related writer, reader and copier objects.
  //     When SpatialAudioMetadataItems is activated, a metadataFormatId is specified
  //     which defines the metadata format enforced by all objects created from this factory.
  //     If the metadataFormatId is not available on the current audio render endpoint
  //     the class factory will not activate and returns an error.
  // </summary>
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioMetadataClient);'}
  {$EXTERNALSYM ISpatialAudioMetadataClient}
  ISpatialAudioMetadataClient = interface(IUnknown)
  ['{777D4A3B-F6FF-4A26-85DC-68D7CDEDA1D4}']


    function ActivateSpatialAudioMetadataItems(maxItemCount: UINT16;
                                               frameCount: UINT16;
                                               out metadataItemsBuffer: ISpatialAudioMetadataItemsBuffer;
                                               out metadataItems: ISpatialAudioMetadataItems): HRESULT; stdcall;
    // <summary>
    //     Creates a SpatialAudioMetadataItems object to contain metadata Items.
    // </summary>
    // <param name="maxItemCount">
    //     Specifies the maximum number of metadata items which can be stored in SpatialAudioMetadataItems object
    // </param>
    // <param name="frameCount">
    //     Specifies the valid range of frame offset positions for metadata items stored in SpatialAudioMetadataItems object.
    // </param>
    // <param name="metadataItemsBuffer">
    //     Optionally returns an pointer to SpatialAudioMetadataItemsBuffer interface which provides methods for attaching
    //     caller provided memory for storage of metadata.  If this parameter is NULL, the object will allocate internal storage
    //     for the items.   This interface cannot be obtained via QueryInterface, this allows controlled access of this capablity.
    // </param>
    // <param name="metadataItems">
    //     Returns an instance SpatialAudioMetadataItems object which contains metadata populated by ISpatialAudioMetadata Writer or Copier.
    // </param>

    function GetSpatialAudioMetadataItemsBufferLength(maxItemCount: UINT16;
                                                      out bufferLength: UINT32): HRESULT; stdcall;
    // <summary>
    // provides length of buffer for the number of metadata items specified to support caller provided memory
    // </summary>
    // <param name="maxItemCount">
    //     Specifies the maximum number of metadata items which can be stored in SpatialAudioMetadataItems object
    // </param>
    // <param name="bufferLength">
    //     returns the length of required buffer to store SpatialAudioMetadataItems data with maxItemCount items
    // </param>

    function ActivateSpatialAudioMetadataWriter(overflowMode: SpatialAudioMetadataWriterOverflowMode;
                                                out metadataWriter: ISpatialAudioMetadataWriter): HRESULT; stdcall;
    // <summary>
    //     The ActivateSpatialAudioMetadataWriter() method creates an instance of an ISpatialAudioMetadataWriter.
    // </summary>
    // <param name="mergeOnOverflow">
    //     Accepts SpatialAudioMetadataWriterOverflowMode enum value to define behavior when max item count is exceeded
    //     _Fail - Overflow will fail
    //     _MergeWithNew - Overflow will succeed, will merge overflow item with previous item and adopt frame offset of newest item
    //     _MergeWithLast - Overflow will succeed, will merge overflow item with previous item and keep existing frame offset
    // </param>
    // <param name="metadataWriter">
    //     Returns a pointer to an instance of ISpatialAudioMetadataWriter.
    // </param>

    function ActivateSpatialAudioMetadataCopier(out metadataCopier: ISpatialAudioMetadataCopier): HRESULT; stdcall;
    // <summary>
    //     The ActivateSpatialAudioMetadataCopier() method creates an instance of an ISpatialAudioMetadataCopier.
    //     This object is used to copy all or copyFrames of metadataBuffer items from one metadataBuffer to
    //     another.
    // </summary>
    // <param name="metadataCopy">
    //     Returns a pointer to an instance of ISpatialAudioMetadataCopier.
    // </param>

    function ActivateSpatialAudioMetadataReader(out metadataReader: ISpatialAudioMetadataReader): HRESULT; stdcall;
    // <summary>
    //     The ActivateSpatialAudioMetadataReader() method creates an instance of an ISpatialAudioMetadataReader.
    //     This object is used to extract metadataItems and commands from a SpatialAudioMetadataItems one readFrameCount at a time
    //     or all at once.
    // </summary>
    // <param name="metadataReader">
    //     Returns a pointer to an instance of ISpatialAudioMetadataReader.
    // </param>

  end;
  IID_ISpatialAudioMetadataClient = ISpatialAudioMetadataClient;
  {$EXTERNALSYM IID_ISpatialAudioMetadataClient}


  // ISpatialAudioObjectForMetadataCommands interface
  // ================================================
  //
  // This interface is used to write engine specific metadata commands
  // The data written via this interface must adhere to the format defined by the spatial rendering engine.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectForMetadataCommands);'}
  {$EXTERNALSYM ISpatialAudioObjectForMetadataCommands}
  ISpatialAudioObjectForMetadataCommands = interface(ISpatialAudioObjectBase)
  ['{0DF2C94B-F5F9-472D-AF6B-C46E0AC9CD05}']

    function WriteNextMetadataCommand(commandID: Byte;
                                      valueBuffer: Pointer;
                                      valueBufferLength: UINT32): HRESULT; stdcall;
    // Writes a metadata command to the spatial audio object, individual commands may only be added once
    // per object per processing cycle. Valid commands and value lengths are defined by the metadataFormatId.

  end;
  IID_ISpatialAudioObjectForMetadataCommands = ISpatialAudioObjectForMetadataCommands;
  {$EXTERNALSYM IID_ISpatialAudioObjectForMetadataCommands}



  // ISpatialAudioObjectForMetadataItems interface
  // =============================================
  //
  // This interface is used to write engine specific metadata when multiple metadata items with frame
  // accurate placement per buffer is required.  This is typically used for streaming content via Media Foundation.
  // The data written via this interface must adhere to the format defined by the spatial rendering engine.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectForMetadataItems);'}
  {$EXTERNALSYM ISpatialAudioObjectForMetadataItems}
  ISpatialAudioObjectForMetadataItems = interface(ISpatialAudioObjectBase)
  ['{DDEA49FF-3BC0-4377-8AAD-9FBCFD808566}']

    function GetSpatialAudioMetadataItems(out metadataItems: ISpatialAudioMetadataItems): HRESULT; stdcall;
    // Get a pointer to the ISpatialAudioMetadataItems object to add metadata items, release when done.

  end;
  IID_ISpatialAudioObjectForMetadataItems = ISpatialAudioObjectForMetadataItems;
  {$EXTERNALSYM IID_ISpatialAudioObjectForMetadataItems}


  // ISpatialAudioObjectRenderStreamForMetadata interface
  // ====================================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISpatialAudioObjectRenderStreamForMetadata);'}
  {$EXTERNALSYM ISpatialAudioObjectRenderStreamForMetadata}
  ISpatialAudioObjectRenderStreamForMetadata = interface(ISpatialAudioObjectRenderStreamBase)
  ['{BBC9C907-48D5-4A2E-A0C7-F7F0D67C1FB1}']

    function ActivateSpatialAudioObjectForMetadataCommands(_type: AudioObjectType;
                                                           out audioObject: ISpatialAudioObjectForMetadataCommands): HRESULT; stdcall;
    // Activate an ISpatialAudioObjectForMetadataCommands for rendering, counts against total resources
    // This method will return SPTLAUDCLNT_E_NO_MORE_OBJECTS if all audio objects are
    // being used
    // To avoid this error, call Release() when object life ends
    // and there is no more data to feed or after SetEndOfStream()

    function ActivateSpatialAudioObjectForMetadataItems(_type: AudioObjectType;
                                                        out audioObject: ISpatialAudioObjectForMetadataItems): HRESULT; stdcall;
    // Activate an ISpatialAudioObjectForMetadataItems for rendering, counts against total resources
    // This method will return SPTLAUDCLNT_E_NO_MORE_OBJECTS if all audio objects are
    // being used
    // To avoid this error, call Release() when object life ends
    // and there is no more data to feed or after SetEndOfStream()

  end;
  IID_ISpatialAudioObjectRenderStreamForMetadata = ISpatialAudioObjectRenderStreamForMetadata;
  {$EXTERNALSYM IID_ISpatialAudioObjectRenderStreamForMetadata}


const
  // error codes

  // Since XE2 these needs to be hardcoded.
  // The AUDCLNT_ERR macro is defined in MfPack.AudioClient.pas

  SPTLAUD_MD_CLNT_E_COMMAND_NOT_FOUND             =   $88890200; //   AUDCLNT_ERR($0200);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_COMMAND_NOT_FOUND}
  SPTLAUD_MD_CLNT_E_OBJECT_NOT_INITIALIZED        =   $88890201; //   AUDCLNT_ERR($0201);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_OBJECT_NOT_INITIALIZED}
  SPTLAUD_MD_CLNT_E_INVALID_ARGS                  =   $88890202; //   AUDCLNT_ERR($0202);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_INVALID_ARGS}
  SPTLAUD_MD_CLNT_E_METADATA_FORMAT_NOT_FOUND     =   $88890203; //   AUDCLNT_ERR($0203);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_METADATA_FORMAT_NOT_FOUND}
  SPTLAUD_MD_CLNT_E_VALUE_BUFFER_INCORRECT_SIZE   =   $88890204; //   AUDCLNT_ERR($0204);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_VALUE_BUFFER_INCORRECT_SIZE}
  SPTLAUD_MD_CLNT_E_MEMORY_BOUNDS                 =   $88890205; //   AUDCLNT_ERR($0205);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_MEMORY_BOUNDS}
  SPTLAUD_MD_CLNT_E_NO_MORE_COMMANDS              =   $88890206; //   AUDCLNT_ERR($0206);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_NO_MORE_COMMANDS}
  SPTLAUD_MD_CLNT_E_BUFFER_ALREADY_ATTACHED       =   $88890207; //   AUDCLNT_ERR($0207);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_BUFFER_ALREADY_ATTACHED}
  SPTLAUD_MD_CLNT_E_BUFFER_NOT_ATTACHED           =   $88890208; //   AUDCLNT_ERR($0208);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_BUFFER_NOT_ATTACHED}
  SPTLAUD_MD_CLNT_E_FRAMECOUNT_OUT_OF_RANGE       =   $88890209; //   AUDCLNT_ERR($0209);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_FRAMECOUNT_OUT_OF_RANGE}
  SPTLAUD_MD_CLNT_E_NO_ITEMS_FOUND                =   $88890210; //   AUDCLNT_ERR($0210);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_NO_ITEMS_FOUND}
  SPTLAUD_MD_CLNT_E_ITEM_COPY_OVERFLOW            =   $88890211; //   AUDCLNT_ERR($0211);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_ITEM_COPY_OVERFLOW}
  SPTLAUD_MD_CLNT_E_NO_ITEMS_OPEN                 =   $88890212; //   AUDCLNT_ERR($0212);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_NO_ITEMS_OPEN}
  SPTLAUD_MD_CLNT_E_ITEMS_ALREADY_OPEN            =   $88890213; //   AUDCLNT_ERR($0213);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_ITEMS_ALREADY_OPEN}
  SPTLAUD_MD_CLNT_E_ATTACH_FAILED_INTERNAL_BUFFER =   $88890214; //   AUDCLNT_ERR($0214);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_ATTACH_FAILED_INTERNAL_BUFFER}
  SPTLAUD_MD_CLNT_E_DETACH_FAILED_INTERNAL_BUFFER =   $88890215; //   AUDCLNT_ERR($0215);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_DETACH_FAILED_INTERNAL_BUFFER}
  SPTLAUD_MD_CLNT_E_NO_BUFFER_ATTACHED            =   $88890216; //   AUDCLNT_ERR($0216);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_NO_BUFFER_ATTACHED}
  SPTLAUD_MD_CLNT_E_NO_MORE_ITEMS                 =   $88890217; //   AUDCLNT_ERR($0217);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_NO_MORE_ITEMS}
  SPTLAUD_MD_CLNT_E_FRAMEOFFSET_OUT_OF_RANGE      =   $88890218; //   AUDCLNT_ERR($0218);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_FRAMEOFFSET_OUT_OF_RANGE}
  SPTLAUD_MD_CLNT_E_ITEM_MUST_HAVE_COMMANDS       =   $88890219; //   AUDCLNT_ERR($0219);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_ITEM_MUST_HAVE_COMMANDS}
  SPTLAUD_MD_CLNT_E_NO_ITEMOFFSET_WRITTEN         =   $88890220; //   AUDCLNT_ERR($0220);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_NO_ITEMOFFSET_WRITTEN}
  SPTLAUD_MD_CLNT_E_NO_ITEMS_WRITTEN              =   $88890221; //   AUDCLNT_ERR($0221);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_NO_ITEMS_WRITTEN}
  SPTLAUD_MD_CLNT_E_COMMAND_ALREADY_WRITTEN       =   $88890222; //   AUDCLNT_ERR($0222);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_COMMAND_ALREADY_WRITTEN}
  SPTLAUD_MD_CLNT_E_FORMAT_MISMATCH               =   $88890223; //   AUDCLNT_ERR($0223);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_FORMAT_MISMATCH}
  SPTLAUD_MD_CLNT_E_BUFFER_STILL_ATTACHED         =   $88890224; //   AUDCLNT_ERR($0224);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_BUFFER_STILL_ATTACHED}
  SPTLAUD_MD_CLNT_E_ITEMS_LOCKED_FOR_WRITING      =   $88890225; //   AUDCLNT_ERR($0225);
  {$EXTERNALSYM SPTLAUD_MD_CLNT_E_ITEMS_LOCKED_FOR_WRITING}


  // Additional Prototypes for ALL interfaces

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.

end.
