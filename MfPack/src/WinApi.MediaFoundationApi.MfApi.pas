// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MfPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.MediaFoundationApi.MfApi.pas
// Kind: Pascal / Delphi unit
// Release date: 27-06-2012
// Language: ENU
//
// Revision Version: 3.0.1
// Description: Requires Windows Vista or later.
//              MfApi.pas is the unit containing the APIs for using the MF platform.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships), (TopPlay)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 13/08/2020 All                 Enigma release. New layout and namespaces
// 10/10/2020 Tony                Fixed some issues, see updt 101020
// 26/01/2021 Tony                Fixed MFT Register functions.
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows Vista or later.
// 
//          When reading the original headers (.h) you may see "STDAPI", a macro.
//          "STDAPI" means it uses the "stdcall" calling convention and it returns always a
//          HRESULT,
//          unless it's marked with, for example BOOL, a LongBool is returned.
//          In Delphi it's declared as:
//          [uses Windows;]
//          [function FunctionName(vars: -const, out or var-): HResult; stdcall;]
// 
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
// 
//          Using packed records is not recommended,
//          because it can prevent compatibility with other languages or
//          platforms, it slows data access, and, in the case of a character array,
//          it affects type compatibility.
//          For more information, see Memory management and Implicit Packing of
//          Fields with a Common Type Specification.
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
// Source: mfapi.h
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
unit WinApi.MediaFoundationApi.MfApi;

  {$HPPEMIT '#include "mfapi.h"'}

interface

uses
  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.WinMM.MMReg,
  WinApi.MediaObj,
  WinApi.AmVideo,
  WinApi.DvdMedia,
  WinApi.StrMif,
  WinApi.Unknwn,
  WinApi.ComBaseApi,
  {WinApi.ActiveX}
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.PropIdl,
  {System}
  System.Types,
  {DirectX or use rtl, Clootie Dx}
  WinApi.DirectX.DXGIFormat,
  WinApi.DirectX.D3D9Types,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfObjects;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

type
  MFWORKITEM_KEY = UInt64;

const
  MF_SDK_VERSION                      = $0002;
  {$EXTERNALSYM MF_SDK_VERSION}
  MF_API_VERSION                      = $0070;  // This value is unused in the Win7 release and left at its Vista release value
  {$EXTERNALSYM MF_API_VERSION}
  MF_VERSION                          = (MF_SDK_VERSION shl 16 or MF_API_VERSION);
  {$EXTERNALSYM MF_VERSION}
  MFSTARTUP_NOSOCKET                  = $1;
  {$EXTERNALSYM MFSTARTUP_NOSOCKET}
  MFSTARTUP_LITE                      = (MFSTARTUP_NOSOCKET);
  {$EXTERNALSYM MFSTARTUP_LITE}
  MFSTARTUP_FULL                      = 0; // Default value
  {$EXTERNALSYM MFSTARTUP_FULL}


////////////////////////////////////////////////////////////////////////////////
///////////////////////////////   Startup/Shutdown  ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  // Initializes the platform object.
  // Must be called before using Media Foundation.
  // A matching MFShutdown call must be made when the application is done using
  // Media Foundation.
  // The "Version" parameter should be set to MF_API_VERSION.
  // Application should not call MFStartup / MFShutdown from workqueue threads
  //
  // Default = MFSTARTUP_FULL
  function MFStartup(const Version: ULONG = MF_API_VERSION;
                     const dwFlags: DWORD = MFSTARTUP_FULL): HRESULT; stdcall;
  {$EXTERNALSYM MFStartup}


  // Shuts down the Microsoft Media Foundation platform.
  // Call this function once for every call to MFStartup.
  // Do not call this function from work queue threads.
  function MFShutdown(): HRESULT; stdcall;
  {$EXTERNALSYM MFShutdown}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Platform    ///////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  // These functions can be used to keep the MF platform object in place.
  // Every call to MFLockPlatform should have a matching call to MFUnlockPlatform

  function MFLockPlatform(): HResult; stdcall;
  {$EXTERNALSYM MFUnlockPlatform}
  function MFUnlockPlatform(): HResult; stdcall;
  {$EXTERNALSYM MFLockPlatform}

////////////////////////////////////////////////////////////////////////////////

  function MFPutWorkItem(const dwQueue: DWORD;
                         pCallback: IMFAsyncCallback;
                         pState: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFPutWorkItem}
  // Puts an asynchronous operation on a work queue.
  // Parameters
  // dwQueue [in]
  //    The identifier for the work queue.
  //    This value can specify one of the standard Media Foundation work queues,
  //    or a work queue created by the application.
  //    For list of standard Media Foundation work queues, see Work Queue Identifiers.
  //    To create a new work queue, call MFAllocateWorkQueue or MFAllocateWorkQueueEx.
  // pCallback [in]
  //    A pointer to the IMFAsyncCallback interface.
  //    The caller must implement this interface.
  // pState [in]
  //    A pointer to the IUnknown interface of a state object, defined by the caller.
  //    This parameter can be Nil. You can use this object to hold state information.
  //    The object is returned to the caller when the callback is invoked.

  function MFPutWorkItem2(dwQueue: DWORD;
                          Priority: LONG;
                          pCallback: IMFAsyncCallback;
                          pState: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFPutWorkItem2}
  // Puts an asynchronous operation on a work queue, with a specified priority.
  // Parameters
  // dwQueue [in]
  //    The identifier for the work queue. This value can specify one of the standard Media Foundation work queues, or a work queue created by the application. For list of standard Media Foundation work queues, see Work Queue Identifiers. To create a new work queue, call MFAllocateWorkQueue or MFAllocateWorkQueueEx.
  // Priority [in]
  //    The priority of the work item. Work items are performed in order of priority.
  // pCallback [in]
  //    A pointer to the IMFAsyncCallback interface.
  //    The caller must implement this interface.
  // pState [in]
  //    A pointer to the IUnknown interface of a state object, defined by the caller.
  //    This parameter can be Nil. You can use this object to hold state information. The object is returned to the caller when the callback is invoked.

  function MFPutWorkItemEx(dwQueue: DWORD;
                           pResult: IMFAsyncResult): HResult; stdcall;
  {$EXTERNALSYM MFPutWorkItemEx}
  // Puts an asynchronous operation on a work queue.
  // Parameters
  // dwQueue [in]
  //    The identifier for the work queue.
  //    This value can specify one of the standard Media Foundation work queues,
  //    or a work queue created by the application.
  //    For list of standard Media Foundation work queues, see Work Queue Identifiers.
  //    To create a new work queue, call MFAllocateWorkQueue or MFAllocateWorkQueueEx.
  // pResult [in]
  //    A pointer to the IMFAsyncResult interface of an asynchronous result object.
  //    To create the result object, call MFCreateAsyncResult.

  function MFPutWorkItemEx2(dwQueue: DWORD;
                            Priority: LONG;
                            var pResult: IMFAsyncResult): HResult; stdcall;
  {$EXTERNALSYM MFPutWorkItemEx2}
  // Puts an asynchronous operation on a work queue, with a specified priority.
  // Parameters
  // dwQueue [in]
  //    The identifier for the work queue.
  //    This value can specify one of the standard Media Foundation work queues,
  //    or a work queue created by the application.
  //    For list of standard Media Foundation work queues, see Work Queue Identifiers.
  //    To create a new work queue, call MFAllocateWorkQueue or MFAllocateWorkQueueEx.
  // Priority [in]
  //    The priority of the work item. Work items are performed in order of priority.
  // pResult [in]
  //    A pointer to the IMFAsyncResult interface of an asynchronous result object.
  //    To create the result object, call MFCreateAsyncResult.

  function MFPutWaitingWorkItem(hEvent: THandle;
                                Priority: LONG;
                                pResult: IMFAsyncResult;
                                out pKey: MFWORKITEM_KEY ): HResult; stdcall;
  {$EXTERNALSYM MFPutWaitingWorkItem}
  // Queues a work item that waits for an event to be signaled.
  // Parameters
  // hEvent [in]
  //    A handle to an event object. To create an event object,
  //    call CreateEvent or CreateEventEx.
  // Priority [in]
  //    The priority of the work item. Work items are performed in order of priority.
  // pResult [in]
  //    A pointer to the IMFAsyncResult interface of an asynchronous result object.
  //    To create the result object, call MFCreateAsyncResult.
  // pKey [out]
  //    Receives a key that can be used to cancel the wait.
  //    To cancel the wait, call MFCancelWorkItem and pass this key in the Key parameter.
  //    This parameter can be Nil.

  function MFAllocateSerialWorkQueue(dwWorkQueue: DWORD;
                                     out pdwWorkQueue: DWORD): HResult; stdcall;
  {$EXTERNALSYM MFAllocateSerialWorkQueue}
  // Creates a work queue that is guaranteed to serialize work items.
  // The serial work queue wraps an existing multithreaded work queue.
  // The serial work queue enforces a first-in, first-out (FIFO) execution order.
  // Parameters
  // dwWorkQueue [in]
  //    The identifier of an existing work queue.
  //    This must be either a multithreaded queue or another serial work queue.
  //    Any of the following can be used:
  //    The default work queue (MFASYNC_CALLBACK_QUEUE_STANDARD)
  //    The platform multithreaded queue (MFASYNC_CALLBACK_QUEUE_MULTITHREADED)
  //    A multithreaded queue returned by the MFLockSharedWorkQueue function.
  //    A serial queue created by the MFAllocateSerialWorkQueue function.
  // pdwWorkQueue [out]
  //    Receives an identifier for the new serial work queue.
  //    Use this identifier when queuing work items.

  function MFScheduleWorkItem(pCallback: IMFAsyncCallback;
                              pState: IUnknown;
                              Timeout: INT64;
                              out pKey: MFWORKITEM_KEY): HResult; stdcall;
  {$EXTERNALSYM MFScheduleWorkItem}
  // Schedules an asynchronous operation to be completed after a specified interval.
  // Parameters
  // pCallback [in]
  //    Pointer to the IMFAsyncCallback interface.
  //    The caller must implement this interface.
  // pState [in]
  //    Pointer to the IUnknown interface of a state object, defined by the caller.
  //    This parameter can be Nil. You can use this object to hold state information. The object is returned to the caller when the callback is invoked.
  // Timeout [in]
  //    Time-out interval, in milliseconds. Set this parameter to a negative value.
  //    The callback is invoked after -Timeout milliseconds. For example, if Timeout is -5000, the callback is invoked after 5000 milliseconds.
  // pKey [out]
  //    Receives a key that can be used to cancel the timer.
  //    To cancel the timer, call MFCancelWorkItem and pass this key in the Key parameter.

  function MFScheduleWorkItemEx(pResult: IMFAsyncResult;
                                Timeout: INT64;
                                out pKey: MFWORKITEM_KEY): HResult; stdcall;
  {$EXTERNALSYM MFScheduleWorkItemEx}
  // Schedules an asynchronous operation to be completed after a specified interval.
  // Parameters
  // pResult [in]
  //    Pointer to the IMFAsyncResult interface of an asynchronous result object.
  //    To create the result object, call MFCreateAsyncResult.
  // Timeout [in]
  //    Time-out interval, in milliseconds. Set this parameter to a negative value.
  //    The callback is invoked after -Timeout milliseconds.
  //    For example, if Timeout is -5000, the callback is invoked after 5000 milliseconds.
  // pKey [out]
  //    Receives a key that can be used to cancel the timer.
  //    To cancel the timer, call MFCancelWorkItem and pass this key in the Key parameter.
  //    The CancelWorkItem method is used by objects to cancel scheduled operation
  //    Due to asynchronous nature of timers, application might still get a
  //    timer callback after MFCancelWorkItem has returned.

  {$EXTERNALSYM MFCancelWorkItem}
  function MFCancelWorkItem(Key: MFWORKITEM_KEY): HResult; stdcall;
  // Attempts to cancel an asynchronous operation that was scheduled with
  // MFScheduleWorkItem or MFScheduleWorkItemEx.
  // Parameters
  // Key [in]
  //    The key that was received in the pKey parameter of the MFScheduleWorkItem,
  //    MFScheduleWorkItemEx, or MFPutWaitingWorkItem functions.
  // Return value -
  //    If this function succeeds, it returns S_OK. Otherwise,
  //    it returns an HRESULT error code.
  // Remarks
  //    Because work items are asynchronous, the work-item callback might still be
  //    invoked after MFCancelWorkItem is called.

  ///////////////////////////////////////////////////////////////////////////////



  // MF periodic callbacks
  //======================
  function MFGetTimerPeriodicity(out Periodicity: DWord): HResult; stdcall;
  {$EXTERNALSYM MFGetTimerPeriodicity}
  // Retrieves the timer interval for the MFAddPeriodicCallback function.
  // Parameters
  // Periodicity [out]
  //    Receives the timer interval, in milliseconds.

type
  MFPERIODICCALLBACK = procedure(pContext: IUnknown); stdcall;
  {$EXTERNALSYM MFPERIODICCALLBACK}
  // Callback function for the MFAddPeriodicCallback function.
  // Parameters
  // pContext [in]
  //    Pointer to the IUnknown interface, or Nil.
  //    This pointer is specified by the caller in the MFAddPeriodicCallback function.
  // Return value
  //    This function pointer does not return a value.

  function MFAddPeriodicCallback(Callback: MFPERIODICCALLBACK;
                                 pContext: IUnknown;
                                 out pdwKey: DWord): HResult; stdcall;
  {$EXTERNALSYM MFAddPeriodicCallback}
  // Sets a callback function to be called at a fixed interval.
  // Parameters
  // Callback [in]
  //    Pointer to the callback function, of type MFPERIODICCALLBACK.
  // pContext [in]
  //    Pointer to a caller-provided object that implements IUnknown, or Nil.
  //    This parameter is passed to the callback function.
  // pdwKey [out]
  //    Receives a key that can be used to cancel the callback.
  //    To cancel the callback, call MFRemovePeriodicCallback and pass this key as
  //    the dwKey parameter.
  // Return value
  //    The function returns an HRESULT.
  //    Possible values include, but are not limited to, those in the following table.
  // Return code   Description
  // S_OK          The function succeeded.
  // Remarks
  //    To get the timer interval for the periodic callback, call MFGetTimerPeriodicity.

  function MFRemovePeriodicCallback(dwKey: DWord): HResult; stdcall;
  {$EXTERNALSYM MFRemovePeriodicCallback}
  // Cancels a callback function that was set by the MFAddPeriodicCallback function.
  // Parameters
  // dwKey [in]
  // Key that identifies the callback.
  // This value is retrieved by the MFAddPeriodicCallback function.
  // Return value
  //    The function returns an HRESULT. Possible values include,
  //    but are not limited to, those in the following table.
  // Return code  Description
  // S_OK         The function succeeded.
  // Remarks
  //    The callback is dispatched on another thread,
  //    and this function does not attempt to synchronize with the callback thread.
  //    Therefore, it is possible for the callback to be invoked after this function returns.

///////////////////////////////////////////////////////////////////////////////


  // MF work queues
  /////////////////

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // MFASYNC_WORKQUEUE_TYPE: types of work queue used by MFAllocateWorkQueueEx

type
  PMfasyncWorkqueueType = ^MFASYNC_WORKQUEUE_TYPE;
  PMFASYNC_WORKQUEUE_TYPE = ^MFASYNC_WORKQUEUE_TYPE;
  MFASYNC_WORKQUEUE_TYPE  = Dword;
  {$EXTERNALSYM MFASYNC_WORKQUEUE_TYPE}
const
  MF_STANDARD_WORKQUEUE      = 0;  // MF_STANDARD_WORKQUEUE: Work queue in a thread without Window
  {$EXTERNALSYM MF_STANDARD_WORKQUEUE}
  // message loop.
  MF_WINDOW_WORKQUEUE        = 1;  // MF_WINDOW_WORKQUEUE: Work queue in a thread running Window
  {$EXTERNALSYM MF_WINDOW_WORKQUEUE}
  // Message loop that calls PeekMessage() / DispatchMessage()..
  MF_MULTITHREADED_WORKQUEUE = 2;   // common MT threadpool
  {$EXTERNALSYM MF_MULTITHREADED_WORKQUEUE}


  function MFAllocateWorkQueueEx(WorkQueueType: MFASYNC_WORKQUEUE_TYPE;
                                 out pdwWorkQueue: DWord): HResult; stdcall;
  {$EXTERNALSYM MFAllocateWorkQueueEx}
  // Creates a new work queue.
  // This function extends the capabilities of the MFAllocateWorkQueue function by
  // making it possible to create a work queue that has a message loop.
  // Parameters
  // WorkQueueType [in]
  // A member of the MFASYNC_WORKQUEUE_TYPE enumeration, specifying the type of work queue to create.
  // Value  Meaning
  //    MF_MULTITHREADED_WORKQUEUE
  //      Create a multithreaded work queue.
  //      Generally, applications should not create private multithreaded queues.
  //      Use the platform multithreaded queues instead.
  //      For more information, see Work Queue and Threading Improvements.
  //    MF_STANDARD_WORKQUEUE
  //      Create a work queue without a message loop.
  //      Using this flag is equivalent to calling MFAllocateWorkQueue.
  //    MF_WINDOW_WORKQUEUE
  //      Create a work queue with a message loop.
  //      The thread that dispatches the work items for this queue will also
  //      call PeekMessage and DispatchMessage.
  //      deUse this option if your callback performs any actions that require a message loop.
  // pdwWorkQueue [out]
  //    Receives an identifier for the work queue that was created.

  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  // Allocate a standard work queue. The behaviour is the same with:
  // MFAllocateWorkQueueEx( MF_STANDARD_WORKQUEUE, pdwWorkQueue )

  function MFAllocateWorkQueue(out pdwWorkQueue: DWord): HResult; stdcall;
  {$EXTERNALSYM MFAllocateWorkQueue}
  // Creates a new work queue.
  // Parameters
  // pdwWorkQueue [out]
  //    Receives an identifier for the work queue.

  function MFLockWorkQueue(dwWorkQueue: DWord): HResult; stdcall;
  {$EXTERNALSYM MFLockWorkQueue}
  // Locks a work queue.
  // Parameters
  // dwWorkQueue [in]
  //    The identifier for the work queue.
  //    The identifier is returned by the MFAllocateWorkQueue function.
  // Return value
  //    If this function succeeds, it returns S_OK.
  //    Otherwise, it returns an HRESULT error code.
  // Remarks
  //    This function prevents the MFShutdown function from shutting down the work queue.
  //    Use this function to ensure that asynchronous operations on the
  //    work queue complete gracefully before the platform shuts down.
  //    The MFShutdown function blocks until the work queue is unlocked,
  //    or until a fixed wait period has elapsed. (The wait period is a few seconds.)

  function MFUnlockWorkQueue(const dwWorkQueue: DWord): HResult; stdcall;
  {$EXTERNALSYM MFUnlockWorkQueue}
  // Unlocks a work queue.
  // Parameters
  // dwWorkQueue [in]
  //    Identifier for the work queue to be unlocked.
  //    The identifier is returned by the MFAllocateWorkQueue function.

  function MFBeginRegisterWorkQueueWithMMCSS(dwWorkQueueId: DWord;
                                             wszClass: LPCWSTR;  //updt 090812 > error> DWord): HResult; stdcall;
                                             dwTaskId: DWord;
                                             pDoneCallback: IMFAsyncCallback;
                                             pDoneState: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFBeginRegisterWorkQueueWithMMCSS}
  // Associates a work queue with a Multimedia Class Scheduler Service (MMCSS) task.
  // Parameters
  // dwWorkQueueId [in]
  //    The identifier of the work queue.
  //    For private work queues, the identifier is returned by the MFAllocateWorkQueue function.
  //    For platform work queues, see Work Queue Identifiers.
  // wszClass [in]
  //    The name of the MMCSS task.
  //    For more information, see Multimedia Class Scheduler Service.
  // dwTaskId [in]
  //    The unique task identifier.
  //    To obtain a new task identifier, set this value to zero.
  // pDoneCallback [in]
  //    A pointer to the IMFAsyncCallback interface of a callback object.
  //    The caller must implement this interface.
  // pDoneState [in]
  //    A pointer to the IUnknown interface of a state object, defined by the caller.
  //    This parameter can be Nil. You can use this object to hold state information.
  //    The object is returned to the caller when the callback is invoked.

  function MFBeginRegisterWorkQueueWithMMCSSEx(dwWorkQueueId: DWORD;
                                               wszClass: PWideChar;
                                               dwTaskId: DWORD;
                                               lPriority: LONG;
                                               pDoneCallback: IMFAsyncCallback;
                                               pDoneState: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFBeginRegisterWorkQueueWithMMCSSEx}
  // Associates a work queue with a Multimedia Class Scheduler Service (MMCSS) task.
  // Parameters
  // dwWorkQueueId [in]
  //    The identifier of the work queue.
  //    For private work queues, the identifier is returned by the MFAllocateWorkQueue function.
  //    For platform work queues, see Work Queue Identifiers.
  // wszClass [in]
  //    The name of the MMCSS task.
  //    For more information, see Multimedia Class Scheduler Service.
  // dwTaskId [in]
  //    The unique task identifier.
  //    To obtain a new task identifier, set this value to zero.
  // lPriority [in]
  //    The base relative priority for the work-queue threads.
  //    For more information, see AvSetMmThreadPriority.
  // pDoneCallback [in]
  //    A pointer to the IMFAsyncCallback interface of a callback object.
  //    The caller must implement this interface.
  // pDoneState [in]
  //    A pointer to the IUnknown interface of a state object, defined by the caller.
  //    This parameter can be nil. You can use this object to hold state information.
  //    The object is returned to the caller when the callback is invoked.
  // Remarks
  //    This function extends the MFBeginRegisterWorkQueueWithMMCSS function by adding the lPriority parameter.
  //    This function is asynchronous.
  //    When the operation completes, the callback object's IMFAsyncCallback.Invoke method is called.
  //    At that point, call MFEndRegisterWorkQueueWithMMCSS to complete the asynchronous request.
  //    To unregister the work queue from the MMCSS task, call MFBeginUnregisterWorkQueueWithMMCSS.
  //

  function MFEndRegisterWorkQueueWithMMCSS(pResult: IMFAsyncResult;
                                           pdwTaskId: DWord): HResult; stdcall;
  {$EXTERNALSYM MFEndRegisterWorkQueueWithMMCSS}
  // Completes an asynchronous request to associate a work queue with a Multimedia Class Scheduler Service (MMCSS) task.
  // Parameters
  // pResult [in]
  //    Pointer to the IMFAsyncResult interface.
  //    Pass in the same pointer that your callback object received in the IMFAsyncCallback.Invoke method.
  // pdwTaskId [in]
  //    The unique task identifier.

  function MFBeginUnregisterWorkQueueWithMMCSS(dwWorkQueueId: DWord;
                                               pDoneCallback: IMFAsyncCallback;
                                               pDoneState: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFBeginUnregisterWorkQueueWithMMCSS}
  // Unregisters a work queue from a Multimedia Class Scheduler Service (MMCSS) task.
  // Parameters
  // dwWorkQueueId [in]
  //    The identifier of the work queue.
  //    For private work queues, the identifier is returned by the MFAllocateWorkQueue function.
  //    For platform work queues, see Work Queue Identifiers.
  // pDoneCallback [in]
  //    Pointer to the IMFAsyncCallback interface of a callback object.
  //    The caller must implement this interface.
  // pDoneState [in]
  //    Pointer to the IUnknown interface of a state object, defined by the caller.
  //    This parameter can be nil. You can use this object to hold state information.
  //    The object is returned to the caller when the callback is invoked.

  function MFEndUnregisterWorkQueueWithMMCSS(pResult: IMFAsyncResult): HResult; stdcall;
  {$EXTERNALSYM MFEndUnregisterWorkQueueWithMMCSS}
  // Completes an asynchronous request to unregister a work queue from a Multimedia Class Scheduler Service (MMCSS) task.
  // Parameters
  // pResult [in]
  //    Pointer to the IMFAsyncResult interface.
  //    Pass in the same pointer that your callback object received in the IMFAsyncCallback.Invoke method.
  // Return value
  //    The function returns an HRESULT.
  //    Possible values include, but are not limited to, those in the following table.
  // Return code  Description
  // S_OK         The function succeeded.
  // Remarks
  //    Call this function when the MFBeginUnregisterWorkQueueWithMMCSS function completes asynchronously.

  function MFGetWorkQueueMMCSSClass(dwWorkQueueId: DWord;
                                    out pwszClass: LPWSTR ;
                                    var pcchClass: DWord): HResult; stdcall;
  {$EXTERNALSYM MFGetWorkQueueMMCSSClass}
  // Retrieves the Multimedia Class Scheduler Service (MMCSS) class currently associated with this work queue.
  // Parameters
  // dwWorkQueueId [in]
  //    Identifier for the work queue.
  //    The identifier is retrieved by the MFAllocateWorkQueue function.
  // pwszClass [out]
  //    Pointer to a buffer that receives the name of the MMCSS class.
  //    This parameter can be Nil.
  // pcchClass [in, out]
  //    On input, specifies the size of the pwszClass buffer, in characters.
  //    On output, receives the required size of the buffer, in characters.
  //    The size includes the terminating null character.

  function MFGetWorkQueueMMCSSTaskId(dwWorkQueueId: DWord;
                                     out pdwTaskId: LPDWORD): HResult; stdcall;
  {$EXTERNALSYM MFGetWorkQueueMMCSSTaskId}
  // Retrieves the Multimedia Class Scheduler Service (MMCSS) task identifier currently associated with this work queue.
  // Parameters
  // dwWorkQueueId [in]
  //    Identifier for the work queue.
  //    The identifier is retrieved by the MFAllocateWorkQueue function.
  // pdwTaskId [out]
  //    Receives the task identifier.

  function MFRegisterPlatformWithMMCSS(wszClass: PCWSTR;
                                       var pdwTaskId: DWORD;
                                       const lPriority: LONG): HResult; stdcall;
  {$EXTERNALSYM MFRegisterPlatformWithMMCSS}
  // Registers the standard Microsoft Media Foundation platform work queues with the Multimedia Class Scheduler Service (MMCSS).
  // Parameters
  // wszClass [in]
  //    The name of the MMCSS task.
  // pdwTaskId [in, out]
  //    The MMCSS task identifier.
  //    On input, specify an existing MCCSS task group ID,
  //    or use the value zero to create a new task group.
  //    On output, receives the actual task group ID.
  // lPriority [in]
  //    The base priority of the work-queue threads.
  //

  function MFUnregisterPlatformFromMMCSS(): HResult; stdcall;
  {$EXTERNALSYM MFUnregisterPlatformFromMMCSS}
  // Unregisters the Microsoft Media Foundation platform work queues from a Multimedia Class Scheduler Service (MMCSS) task.
  // Parameters
  //    This function has no parameters.
  //


  function MFLockSharedWorkQueue(wszClass: PCWSTR;
                                 BasePriority: LONG;
                                 var pdwTaskId: DWORD;
                                 out pID: DWORD): HResult; stdcall;
  {$EXTERNALSYM MFLockSharedWorkQueue}
  // Obtains and locks a shared work queue.
  // Parameters
  // wszClass [in]
  //    The name of the MMCSS task.
  // BasePriority [in]
  //    The base priority of the work-queue threads.
  //    If the regular-priority queue is being used (wszClass=""),
  //    then the value 0 must be passed in.
  // pdwTaskId [in, out]
  //    The MMCSS task identifier.
  //    On input, specify an existing MCCSS task group ID ,
  //    or use the value zero to create a new task group.
  //    If the regular priority queue is being used (wszClass=""),
  //    then Nil must be passed in.
  //    On output, receives the actual task group ID.
  // pID [out]
  //    Receives an identifier for the new work queue.
  //    Use this identifier when queuing work items.
  // Remarks See:
  //    https://msdn.microsoft.com/en-us/library/windows/desktop/hh162771
  //

  function MFGetWorkQueueMMCSSPriority(dwWorkQueueId: DWORD;
                                       out lPriority: LONG): HResult; stdcall;
  {$EXTERNALSYM MFGetWorkQueueMMCSSPriority}
  // Gets the relative thread priority of a work queue.
  // Parameters
  // dwWorkQueueId [in]
  //    The identifier of the work queue.
  //    For private work queues, the identifier is returned by the MFAllocateWorkQueue function.
  //    For platform work queues, see Work Queue Identifiers.
  // lPriority [out]
  //    Receives the relative thread priority.
  // Return value
  //    If this function succeeds, it returns S_OK.
  //    Otherwise, it returns an HRESULT error code.
  // Remarks
  //    This function returns the relative thread priority set by the
  //    MFBeginRegisterWorkQueueWithMMCSSEx function.
  //



///////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Async Model //////////////////////////////
///////////////////////////////////////////////////////////////////////////////


  // Instantiates the MF-provided Async Result implementation

  function MFCreateAsyncResult(punkObject: IUnknown;
                               pCallback: IMFAsyncCallback;
                               punkState: IUnknown;
                               out ppAsyncResult: IMFAsyncResult): HResult; stdcall;
  {$EXTERNALSYM MFCreateAsyncResult}

  // Helper for calling IMFAsyncCallback.Invoke
  function MFInvokeCallback(pAsyncResult: IMFAsyncResult): HResult; stdcall;
  {$EXTERNALSYM MFInvokeCallback}


type

  // MFASYNCRESULT struct.
  // Any implementation of IMFAsyncResult must inherit from this struct;
  // the Media Foundation workqueue implementation depends on this.
  //
  PMFASYNCRESULT = ^MFASYNCRESULT;  // updt 101020 corrected wrong naming
  tagMFASYNCRESULT = record
    AsyncResult: IMFAsyncResult;
    overlapped: OVERLAPPED;
    pCallback: IMFAsyncCallback;    // updt 090812 changed pointer to IMFAsyncCallback
    hrStatusResult: HResult;
    dwBytesTransferred: DWORD;
    hEvent: THandle;
  end;
  {$EXTERNALSYM tagMFASYNCRESULT}
  MFASYNCRESULT = tagMFASYNCRESULT; // updt 101020 corrected wrong naming
  {$EXTERNALSYM MFASYNCRESULT}



///////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Files       //////////////////////////////
///////////////////////////////////////////////////////////////////////////////


  // Regardless of the access mode with which the file is opened, the sharing
  // permissions will allow shared reading and deleting.

  function MFCreateFile(AccessMode: MF_FILE_ACCESSMODE;
                        OpenMode: MF_FILE_OPENMODE;
                        fFlags: MF_FILE_FLAGS;
                        pwszFileURL: LPCWSTR;
                        out ppIByteStream: IMFByteStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateFile}

  function MFCreateTempFile(AccessMode: MF_FILE_ACCESSMODE;
                            OpenMode: MF_FILE_OPENMODE;
                            fFlags: MF_FILE_FLAGS;
                            out ppIByteStream: IMFByteStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateTempFile}

  function MFBeginCreateFile(AccessMode: MF_FILE_ACCESSMODE;
                             OpenMode: MF_FILE_OPENMODE;
                             fFlags: MF_FILE_FLAGS;
                             pwszFilePath: LPCWSTR;
                             pCallback: IMFAsyncCallback;
                             pState: IUnknown;
                             out ppCancelCookie: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFBeginCreateFile}

  function MFEndCreateFile(pResult: IMFAsyncResult;
                           out ppFile: IMFByteStream): HResult; stdcall;
  {$EXTERNALSYM MFEndCreateFile}

  function MFCancelCreateFile(const pCancelCookie: IUnknown): HResult; stdcall;
  {$EXTERNALSYM MFCancelCreateFile}


///////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Buffers     //////////////////////////////
///////////////////////////////////////////////////////////////////////////////


  // Creates an IMFMediaBuffer in memory
  function MFCreateMemoryBuffer(cbMaxLength: DWord; // Size of the buffer, in bytes.
                                out ppBuffer: IMFMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateMemoryBuffer}


  // Creates an IMFMediaBuffer wrapper at the given offset and length
  // within an existing IMFMediaBuffer
  function MFCreateMediaBufferWrapper(pBuffer: IMFMediaBuffer;
                                      cbOffset: DWord;
                                      dwLength: DWord;
                                      out ppBuffer: IMFMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateMediaBufferWrapper}

  // Creates a legacy buffer (IMediaBuffer) wrapper at the given offset within
  // an existing IMFMediaBuffer.
  // pSample is optional. It can point to the original IMFSample from which this
  // IMFMediaBuffer came. If provided, then *ppMediaBuffer will succeed
  // QueryInterface for IID_IMFSample, from which the original sample's attributes
  // can be obtained
  function MFCreateLegacyMediaBufferOnMFMediaBuffer(pSample: IMFSample;
                                                    MFMediaBuffer: IMFMediaBuffer;
                                                    cbOffset: DWord;
                                                    out ppMediaBuffer: IMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateLegacyMediaBufferOnMFMediaBuffer}


  // Create a DirectX surface buffer  (DXGI_FORMAT)

  function MFMapDX9FormatToDXGIFormat(dx9: DWORD): DXGI_FORMAT; stdcall;
  {$EXTERNALSYM MFMapDX9FormatToDXGIFormat}
  // Converts a Microsoft Direct3D 9 format identifier to a
  // Microsoft DirectX Graphics Infrastructure (DXGI) format identifier.
  // Parameters
  // dx9 [in]
  //    The D3DFORMAT value or FOURCC code to convert.
  // Return value
  //    Returns a DXGI_FORMAT value.
  //

  function MFMapDXGIFormatToDX9Format(dx11: DXGI_FORMAT): DWORD; stdcall;
  {$EXTERNALSYM MFMapDXGIFormatToDX9Format}
  // Converts a Microsoft DirectX Graphics Infrastructure (DXGI) format identifier
  // to a Microsoft Direct3D 9 format identifier.
  // Parameters
  // dx11 [in]
  //    The DXGI_FORMAT value to convert.
  // Return value
  //    Returns a D3DFORMAT (= DWORD) value or FOURCC code (= DWORD).
  //

  function MFLockDXGIDeviceManager(out pResetToken: UINT;
                                   out ppManager: IMFDXGIDeviceManager): HResult; stdcall;
  {$EXTERNALSYM MFLockDXGIDeviceManager}
  // Locks the shared Microsoft DirectX Graphics Infrastructure (DXGI) Device Manager.
  // Parameters
  // pResetToken [out]
  //    Receives a token that identifies this instance of the DXGI Device Manager.
  //    Use this token when calling IMFDXGIDeviceManager.ResetDevice.
  //    This parameter can be Nil.
  // ppManager [out]
  //    Receives a pointer to the IMFDXGIDeviceManager interface.
  //    The caller must release the interface.

  function MFCreateDXSurfaceBuffer(const riid: REFIID;
                                   punkSurface: IUnknown;
                                   fBottomUpWhenLinear: BOOL;
                                   out ppBuffer: IMFMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateDXSurfaceBuffer}
  // Creates a media buffer object that manages a Direct3D 9 surface.
  // Parameters
  // riid [in]
  //    Identifies the type of Direct3D 9 surface. Currently this value must be IID_IDirect3DSurface9.
  // punkSurface [in]
  //    A pointer to the IUnknown interface of the DirectX surface.
  // fBottomUpWhenLinear [in]
  //    If TRUE, the buffer's IMF2DBuffer.ContiguousCopyTo method copies the buffer into a
  //    bottom-up format. The bottom-up format is compatible with GDI for uncompressed RGB images.
  //    If this parameter is FALSE, the ContiguousCopyTo method copies the buffer into a top-down format,
  //    which is compatible with DirectX.
  //    For more information about top-down versus bottom-up images, see Image Stride.
  // ppBuffer [out]
  //    Receives a pointer to the IMFMediaBuffer interface. The caller must release the buffer.
  //

  function MFCreateWICBitmapBuffer(const riid: REFIID;
                                   punkSurface: IUnknown;
                                   out ppBuffer: IMFMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateWICBitmapBuffer}
  // Creates a media buffer object that manages a Windows Imaging Component (WIC) bitmap.
  // Parameters
  // riid [in]
  //    Set this parameter to __uuidof(IWICBitmap).
  // punkSurface [in]
  //    A pointer to the IUnknown interface of the bitmap surface.
  //    The bitmap surface must be a WIC bitmap that exposes the IWICBitmap interface.
  // ppBuffer [out]
  //    Receives a pointer to the IMFMediaBuffer interface. The caller must release the interface.
  //

  function MFCreateDXGISurfaceBuffer(const riid: REFIID;
                                     punkSurface: IUnknown;
                                     uSubresourceIndex: UINT;
                                     fBottomUpWhenLinear: BOOL;
                                     out ppBuffer: IMFMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateDXGISurfaceBuffer}
  // Creates a media buffer to manage a Microsoft DirectX Graphics Infrastructure (DXGI) surface.
  // Parameters
  //
  // riid [in]
  //    Identifies the type of DXGI surface. This value must be IID_ID3D11Texture2D.
  // punkSurface [in]
  //    A pointer to the IUnknown interface of the DXGI surface.
  // uSubresourceIndex [in]
  //    The zero-based index of a subresource of the surface.
  //    The media buffer object is associated with this subresource.
  // fBottomUpWhenLinear [in]
  //    If TRUE, the buffer's IMF2DBuffer.ContiguousCopyTo method copies the
  //    buffer into a bottom-up format.
  //    The bottom-up format is compatible with GDI for uncompressed RGB images.
  //    If this parameter is FALSE, the ContiguousCopyTo method copies the buffer
  //    into a top-down format, which is compatible with Direct3D.
  //    For more information about top-down versus bottom-up images, see Image Stride.
  // ppBuffer [out]
  //    Receives a pointer to the IMFMediaBuffer interface. The caller must release the buffer.
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.

  function MFCreateVideoSampleAllocatorEx(const riid: REFIID;
                                          out ppSampleAllocator {Expected IUnknown pointer} ): HResult; stdcall;
  {$EXTERNALSYM MFCreateVideoSampleAllocatorEx}
  // Creates an object that allocates video samples that are compatible with Microsoft DirectX Graphics Infrastructure (DXGI).
  // Parameters
  // riid [in]
  //    The identifier of the interface to retrieve. Specify one of the following values.
  //    Value  Meaning:
  //      IID_IUnknown
  //        Retrieve an IUnknown pointer.
  //      IID_IMFVideoSampleAllocator
  //        Retrieve an IMFVideoSampleAllocator pointer.
  //      IID_IMFVideoSampleAllocatorEx
  //        Retrieve an IMFVideoSampleAllocatorEx pointer.
  //      IID_IMFVideoSampleAllocatorCallback
  //        Retrieve an IMFVideoSampleAllocatorCallback pointer.
  //
  //  ppSampleAllocator [out]
  //    Receives a pointer to the requested interface. The caller must release the interface.
  //

  function MFCreateDXGIDeviceManager(out resetToken: UINT;
                                     out ppDeviceManager: IMFDXGIDeviceManager): HResult; stdcall;
  {$EXTERNALSYM MFCreateDXGIDeviceManager}
  // Creates an instance of the Microsoft DirectX Graphics Infrastructure (DXGI) Device Manager.
  // Parameters
  // pResetToken [out]
  //    Receives a token that identifies this instance of the DXGI Device Manager.
  //    Use this token when calling IMFDXGIDeviceManager.ResetDevice.
  // ppDXVAManager [out]
  //    Receives a pointer to the IMFDXGIDeviceManager interface. The caller must release the interface.
  //

const

  // Create an aligned memory buffer.
  // The following constants were chosen for parity with the alignment constants
  // in ntioapi.h

  MF_1_BYTE_ALIGNMENT                 = $00000000;
  {$EXTERNALSYM MF_1_BYTE_ALIGNMENT}
  MF_2_BYTE_ALIGNMENT                 = $00000001;
  {$EXTERNALSYM MF_2_BYTE_ALIGNMENT}
  MF_4_BYTE_ALIGNMENT                 = $00000003;
  {$EXTERNALSYM MF_4_BYTE_ALIGNMENT}
  MF_8_BYTE_ALIGNMENT                 = $00000007;
  {$EXTERNALSYM MF_8_BYTE_ALIGNMENT}
  MF_16_BYTE_ALIGNMENT                = $0000000F;
  {$EXTERNALSYM MF_16_BYTE_ALIGNMENT}
  MF_32_BYTE_ALIGNMENT                = $0000001F;
  {$EXTERNALSYM MF_32_BYTE_ALIGNMENT}
  MF_64_BYTE_ALIGNMENT                = $0000003F;
  {$EXTERNALSYM MF_64_BYTE_ALIGNMENT}
  MF_128_BYTE_ALIGNMENT               = $0000007F;
  {$EXTERNALSYM MF_128_BYTE_ALIGNMENT}
  MF_256_BYTE_ALIGNMENT               = $000000FF;
  {$EXTERNALSYM MF_256_BYTE_ALIGNMENT}
  MF_512_BYTE_ALIGNMENT               = $000001FF;
  {$EXTERNALSYM MF_512_BYTE_ALIGNMENT}
  MF_1024_BYTE_ALIGNMENT              = $000003FF;
  {$EXTERNALSYM MF_1024_BYTE_ALIGNMENT}
  MF_2048_BYTE_ALIGNMENT              = $000007FF;
  {$EXTERNALSYM MF_2048_BYTE_ALIGNMENT}
  MF_4096_BYTE_ALIGNMENT              = $00000FFF;
  {$EXTERNALSYM MF_4096_BYTE_ALIGNMENT}
  MF_8192_BYTE_ALIGNMENT              = $00001FFF;
  {$EXTERNALSYM MF_8192_BYTE_ALIGNMENT}


  //updt 090812 remove: type
  function MFCreateAlignedMemoryBuffer(cbMaxLength: DWord;
                                       cbAligment: DWord;
                                       out ppBuffer: IMFMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateAlignedMemoryBuffer}
  // Allocates system memory with a specified byte alignment and creates a media buffer to manage the memory.
  // Parameters
  // cbMaxLength
  //    Size of the buffer, in bytes.
  //    fAlignmentFlags
  //      Specifies the memory alignment for the buffer.
  //    Use one of the following constants.
  //    Value                                 Meaning
  //    -----------------------------------   --------------------
  //      MF_1_BYTE_ALIGNMENT    = $00000000   Align to 1 bytes.
  //      MF_2_BYTE_ALIGNMENT    = $00000001   Align to 2 bytes.
  //      MF_4_BYTE_ALIGNMENT    = $00000003   Align to 4 bytes.
  //      MF_8_BYTE_ALIGNMENT    = $00000007   Align to 8 bytes.
  //      MF_16_BYTE_ALIGNMENT   = $0000000F   Align to 16 bytes.
  //      MF_32_BYTE_ALIGNMENT   = $0000001F   Align to 32 bytes.
  //      MF_64_BYTE_ALIGNMENT   = $0000003F   Align to 64 bytes.
  //      MF_128_BYTE_ALIGNMENT  = $0000007F   Align to 128 bytes.
  //      MF_256_BYTE_ALIGNMENT  = $000000FF   Align to 256 bytes.
  //      MF_512_BYTE_ALIGNMENT  = $000001FF   Align to 512 bytes.
  //      MF_1024_BYTE_ALIGNMENT = $000003FF   Align to 1024 bytes.
  //      MF_2048_BYTE_ALIGNMENT = $000007FF   Align to 2048 bytes.
  //      MF_4096_BYTE_ALIGNMENT = $00000FFF   Align to 4096 bytes.
  //      MF_8192_BYTE_ALIGNMENT = $00001FFF   Align to 8192 bytes.
  //  ppBuffer
  //    Receives a pointer to the IMFMediaBuffer interface of the media buffer.
  //    The caller must release the interface.

type
  // This GUID is used in IMFGetService.GetService calls to retrieve
  // interfaces from the buffer. Its value is defined in Evr.pas
  MR_BUFFER_SERVICE = TGuid;
  {$EXTERNALSYM MR_BUFFER_SERVICE}



  ///////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////    Events      //////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////


  // Instantiates the MF-provided Media Event implementation.
  function MFCreateMediaEvent(met: MediaEventType;
                              const guidExtendedType: REFGUID;
                              hrStatus: HRESULT;
                              pvValue: PROPVARIANT;
                              out ppEvent: IMFMediaEvent): HResult; stdcall;
  {$EXTERNALSYM MFCreateMediaEvent}


  // Instantiates an object that implements IMFMediaEventQueue.
  // Components that provide an IMFMediaEventGenerator can use this object
  // internally to do their Media Event Generator work for them.
  // IMFMediaEventGenerator calls should be forwarded to the similar call
  // on this object's IMFMediaEventQueue interface (e.g. BeginGetEvent,
  // EndGetEvent), and the various IMFMediaEventQueue::QueueEventXXX methods
  // can be used to queue events that the caller will consume.
  function MFCreateEventQueue(out ppMediaEventQueue: IMFMediaEventQueue): HResult; stdcall;
  {$EXTERNALSYM MFCreateEventQueue}

  // Event attributes
  // Some of the common Media Foundation events have associated attributes
  // that go in their IMFAttributes stores


  // MESessionCapabilitiesChanged attributes

  // MF_EVENT_SESSIONCAPS {7E5EBCD0-11B8-4abe-AFAD-10F6599A7F42}
  // Type: UINT32

const  //updt 090812 replace type
  MF_EVENT_SESSIONCAPS : TGuid =  '{7e5ebcd0-11b8-4abe-afad-10f6599a7f42}';
  {$EXTERNALSYM MF_EVENT_SESSIONCAPS}


  // MF_EVENT_SESSIONCAPS_DELTA {7E5EBCD1-11B8-4abe-AFAD-10F6599A7F42}
  // Type: UINT32
  MF_EVENT_SESSIONCAPS_DELTA : TGuid =  '{7e5ebcd1-11b8-4abe-afad-10f6599a7f42}'; //updt 090812 correct guid value
  {$EXTERNALSYM MF_EVENT_SESSIONCAPS_DELTA}


  // Session capabilities bitflags
  MFSESSIONCAP_START                  = $00000001;
  {$EXTERNALSYM MFSESSIONCAP_START}
  MFSESSIONCAP_SEEK                   = $00000002;
  {$EXTERNALSYM MFSESSIONCAP_SEEK}
  MFSESSIONCAP_PAUSE                  = $00000004;
  {$EXTERNALSYM MFSESSIONCAP_PAUSE}
  MFSESSIONCAP_RATE_FORWARD           = $00000010;
  {$EXTERNALSYM MFSESSIONCAP_RATE_FORWARD}
  MFSESSIONCAP_RATE_REVERSE           = $00000020;
  {$EXTERNALSYM MFSESSIONCAP_RATE_REVERSE}
  MFSESSIONCAP_DOES_NOT_USE_NETWORK   = $00000040;
  {$EXTERNALSYM MFSESSIONCAP_DOES_NOT_USE_NETWORK}

  // MESessionTopologyStatus attributes
  //===================================

  // Possible values for MF_EVENT_TOPOLOGY_STATUS attribute.
  //
  // For a given topology, these status values will arrive via
  // MESessionTopologyStatus in the order below.
  //
  // However, there are no guarantees about how these status values will be
  // ordered between two consecutive topologies. For example,
  // MF_TOPOSTATUS_READY could arrive for topology n+1 before
  // MF_TOPOSTATUS_ENDED arrives for topology n if the application called
  // IMFMediaSession.SetTopology for topology n+1 well enough in advance of the
  // end of topology n. Conversely, if topology n ends before the application
  // calls IMFMediaSession.SetTopology for topology n+1, then
  // MF_TOPOSTATUS_ENDED will arrive for topology n before MF_TOPOSTATUS_READY
  // arrives for topology n+1.


type
  PMF_TOPOSTATUS = ^MF_TOPOSTATUS;
  MF_TOPOSTATUS = (

    MF_TOPOSTATUS_INVALID       = 0,      // MF_TOPOSTATUS_INVALID: Invalid value; will not be sent


    MF_TOPOSTATUS_READY           = 100,  // MF_TOPOSTATUS_READY: The topology has been put in place and is
                                          // ready to start.  All GetService calls to the Media Session will use
                                          // this topology.

    MF_TOPOSTATUS_STARTED_SOURCE  = 200,  // MF_TOPOSTATUS_STARTED_SOURCE: The Media Session has started to read
                                          // and process data from the Media Source(s) in this topology.

    MF_TOPOSTATUS_DYNAMIC_CHANGED = 210,  // MF_TOPOSTATUS_DYNAMIC_CHANGED: The topology has been dynamic changed
                                          // due to the format change.

    MF_TOPOSTATUS_SINK_SWITCHED   = 300,  // MF_TOPOSTATUS_SINK_SWITCHED: The Media Sinks in the pipeline have
                                          // switched from a previous topology to this topology.
                                          // Note that this status does not get sent for the first topology;
                                          // applications can assume that the sinks are playing the first
                                          // topology when they receive MESessionStarted.

    MF_TOPOSTATUS_ENDED           = 400   // MF_TOPOSTATUS_ENDED: Playback of this topology is complete.
                                          // Before deleting this topology, however, the application should wait
                                          // for either MESessionEnded or the MF_TOPOSTATUS_STARTED_SOURCE status
                                          // on the next topology to ensure that the Media Session is no longer
                                          // using this topology.
  );
  {$EXTERNALSYM MF_TOPOSTATUS}


const

  // MF_EVENT_TOPOLOGY_STATUS {30C5018D-9A53-454b-AD9E-6D5F8FA7C43B}
  // Type: UINT32 {MF_TOPOLOGY_STATUS}
  MF_EVENT_TOPOLOGY_STATUS                    : TGuid = '{30c5018d-9a53-454b-ad9e-6d5f8fa7c43b}';
  {$EXTERNALSYM MF_EVENT_TOPOLOGY_STATUS}

  // MESessionNotifyPresentationTime attributes
  //===========================================

  // MF_EVENT_START_PRESENTATION_TIME {5AD914D0-9B45-4a8d-A2C0-81D1E50BFB07}
  // Type: UINT64
  MF_EVENT_START_PRESENTATION_TIME            : TGuid = '{5ad914d0-9b45-4a8d-a2c0-81d1e50bfb07}';
  {$EXTERNALSYM MF_EVENT_START_PRESENTATION_TIME}

  // MF_EVENT_PRESENTATION_TIME_OFFSET {5AD914D1-9B45-4a8d-A2C0-81D1E50BFB07}
  // Type: UINT64
  MF_EVENT_PRESENTATION_TIME_OFFSET           : TGuid = '{5ad914d1-9b45-4a8d-a2c0-81d1e50bfb07}';
  {$EXTERNALSYM MF_EVENT_PRESENTATION_TIME_OFFSET}

  // MF_EVENT_START_PRESENTATION_TIME_AT_OUTPUT {5AD914D2-9B45-4a8d-A2C0-81D1E50BFB07}
  // Type: UINT64
  MF_EVENT_START_PRESENTATION_TIME_AT_OUTPUT  : TGuid = '{5AD914D2-9B45-4a8d-A2C0-81D1E50BFB07}';
  {$EXTERNALSYM MF_EVENT_START_PRESENTATION_TIME_AT_OUTPUT}


  // MESourceStarted attributes
  //===========================

  MF_EVENT_SOURCE_FAKE_START                  : TGuid = '{a8cc55a7-6b31-419f-845d-ffb351a2434b}';
  {$EXTERNALSYM MF_EVENT_SOURCE_FAKE_START}
  MF_EVENT_SOURCE_PROJECTSTART                : TGuid = '{a8cc55a8-6b31-419f-845d-ffb351a2434b}';
  {$EXTERNALSYM MF_EVENT_SOURCE_PROJECTSTART}
  MF_EVENT_SOURCE_ACTUAL_START                : TGuid = '{a8cc55a9-6b31-419f-845d-ffb351a2434b}';
  {$EXTERNALSYM MF_EVENT_SOURCE_ACTUAL_START}


  // MEEndOfPresentationSegment attributes
  //======================================

  MF_EVENT_SOURCE_TOPOLOGY_CANCELED           : TGuid = '{DB62F650-9A5E-4704-ACF3-563BC6A73364}';
  {$EXTERNALSYM MF_EVENT_SOURCE_TOPOLOGY_CANCELED}


  // MESourceCharacteristicsChanged attributes
  //==========================================

  MF_EVENT_SOURCE_CHARACTERISTICS             : TGuid = '{47DB8490-8B22-4f52-AFDA-9CE1B2D3CFA8}';
  {$EXTERNALSYM MF_EVENT_SOURCE_CHARACTERISTICS}
  MF_EVENT_SOURCE_CHARACTERISTICS_OLD         : TGuid = '{47DB8491-8B22-4f52-AFDA-9CE1B2D3CFA8}';
  {$EXTERNALSYM MF_EVENT_SOURCE_CHARACTERISTICS_OLD}


  // MESourceRateChangeRequested attributes
  //=======================================

  MF_EVENT_DO_THINNING                        : TGuid = '{321EA6FB-DAD9-46e4-B31D-D2EAE7090E30}';
  {$EXTERNALSYM MF_EVENT_DO_THINNING}


  // MEStreamSinkScrubSampleComplete attributes
  //===========================================

  MF_EVENT_SCRUBSAMPLE_TIME                   : TGuid = '{9AC712B3-DCB8-44d5-8D0C-37455A2782E3}';
  {$EXTERNALSYM MF_EVENT_SCRUBSAMPLE_TIME}


  // MESinkInvalidated and MESessionStreamSinkFormatChanged attributes
  //==================================================================

  MF_EVENT_OUTPUT_NODE                        : TGuid = '{830f1a8b-c060-46dd-a801-1c95dec9b107}';
  {$EXTERNALSYM MF_EVENT_OUTPUT_NODE}


// #IF WINVER >= _WIN32_WINNT_WIN7

  // METransformNeedInput attributes
  //================================

  MF_EVENT_MFT_INPUT_STREAM_ID                : TGuid = '{F29C2CCA-7AE6-42d2-B284-BF837CC874E2}';
  {$EXTERNALSYM MF_EVENT_MFT_INPUT_STREAM_ID}

  // METransformDrainComplete and METransformMarker attributes
  //==========================================================

  MF_EVENT_MFT_CONTEXT                        : TGuid = '{B7CD31F1-899E-4b41-80C9-26A896D32977}';
  {$EXTERNALSYM MF_EVENT_MFT_CONTEXT}

// #ENDIF} // (WINVER >= _WIN32_WINNT_WIN7)

// #if (WINVER >= _WIN32_WINNT_WINBLUE) //8.1

  // MEContentProtectionMetadata attributes
  //=======================================

  // MF_EVENT_STREAM_METADATA_KEYDATA {CD59A4A1-4A3B-4BBD-8665-72A40FBEA776}
  // Type: BLOB
  // DEFINE_GUID(MF_EVENT_STREAM_METADATA_KEYDATA,
  // 0xcd59a4a1, 0x4a3b, 0x4bbd, 0x86, 0x65, 0x72, 0xa4, 0xf, 0xbe, 0xa7, 0x76);
  MF_EVENT_STREAM_METADATA_KEYDATA            :  TGUID = '{cd59a4a1-4a3b-4bbd-8665-72a40fbea776}';
  {$EXTERNALSYM MF_EVENT_STREAM_METADATA_KEYDATA}

  // MF_EVENT_STREAM_METADATA_CONTENT_KEYIDS {5063449D-CC29-4FC6-A75A-D247B35AF85C}
  // Type: BLOB
  // DEFINE_GUID(MF_EVENT_STREAM_METADATA_CONTENT_KEYIDS,
  // 0x5063449d, 0xcc29, 0x4fc6, 0xa7, 0x5a, 0xd2, 0x47, 0xb3, 0x5a, 0xf8, 0x5c);
  MF_EVENT_STREAM_METADATA_CONTENT_KEYIDS     :  TGUID = '{5063449d-cc29-4fc6-a75a-d247b35af85c}';
  {$EXTERNALSYM MF_EVENT_STREAM_METADATA_CONTENT_KEYIDS}

  // MF_EVENT_STREAM_METADATA_SYSTEMID {1EA2EF64-BA16-4A36-8719-FE7560BA32AD}
  // Type: BLOB
  // DEFINE_GUID(MF_EVENT_STREAM_METADATA_SYSTEMID,
  // 0x1ea2ef64, 0xba16, 0x4a36, 0x87, 0x19, 0xfe, 0x75, 0x60, 0xba, 0x32, 0xad);
  MF_EVENT_STREAM_METADATA_SYSTEMID           :  TGUID = '{1ea2ef64-ba16-4a36-8719-fe7560ba32ad}';
  {$EXTERNALSYM MF_EVENT_STREAM_METADATA_SYSTEMID}


  // #endif // (WINVER >= _WIN32_WINNT_WINBLUE)


  ////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////  Samples  //////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////


  // Creates an instance of the Media Foundation implementation of IMFSample
  function MFCreateSample(out ppIMFSample: IMFSample): HResult; stdcall;
  {$EXTERNALSYM MFCreateSample}


const
  // Sample attributes
  // These are the well-known attributes that can be present on an MF Sample's
  // IMFAttributes store

  //@@MFSampleExtension_MaxDecodeFrameSize
  /// <summary>
  // {D3CC654F-F9F3-4A13-889F-F04EB2B5B957} MFSampleExtension_MaxDecodeFrameSize                {UINT64 (HI32(Width),LO32(Height))}
  // specify the maxiumum resolution of compressed input bitstream,
  // the decoder shall decode any comressed pictures below the specified maximum resolution
  // any input compressed pictures beyond the maximum resolution shall not be decoded and dropped by the decoder
  // the attribute shall be set on input sample
  /// </summary>
  MFSampleExtension_MaxDecodeFrameSize            :  TGUID = '{d3cc654f-f9f3-4a13-889f-f04eb2b5b957}';
  {$EXTERNALSYM MFSampleExtension_MaxDecodeFrameSize}

  //@@MFSampleExtension_AccumulatedNonRefPicPercent
  /// <summary>
  // {79EA74DF-A740-445B-BC98-C9ED1F260EEE} MFSampleExtension_AccumulatedNonRefPicPercent
  // Type: UINT32
  // specify the percentage of accumulated non-reference pictures up to this output sample in decoding order
  // The most common examples are,
  // 1. if the sequence has the GOP structure of IPPPP......,   the value will be 0
  // 2. if the sequence has the GOP structure of IPBPB......,   the percentage will be around 40%~50%. The value is 40~50.
  // 3. if the sequence has the GOP structure of IPBBPBB......, the percentage will be around 50%~66%. The value is 50~60.
  // where B frames are not used for reference.
  // This is some statistic to application or pipeline whether decoder alone can have graceful degradation on quality management
  // In the above example,
  // 1. Decoder alone can't have graceful quality management. Because it can only have full frame rate or 1/15 of full frame rate when GOP size is 15 frames or 1/30 when GOP size is 30 frames
  // 2. Decoder alone can   have quality management. Because it can have full frame rate or 1/2 of full frame rate or 1/GOPSize
  // 2. Decoder alone can   have quality management. Because it can have full frame rate,  or down to 1/3 of full frame rate or 1/GOPSize
  // the attribute could be set on output sample from decoders
  /// </summary>
  // {79EA74DF-A740-445B-BC98-C9ED1F260EEE}
  // 0x79ea74df, 0xa740, 0x445b, 0xbc, 0x98, 0xc9, 0xed, 0x1f, 0x26, 0xe, 0xee
  MFSampleExtension_AccumulatedNonRefPicPercent   :  TGUID = '{79EA74DF-A740-445B-BC98-C9ED1F260EEE}';
  {$EXTERNALSYM MFSampleExtension_AccumulatedNonRefPicPercent}

  ////////////////////////////////////////////////////////////////////////////////
  // Sample extensions for SAMPLE-AES encryption

  // MFSampleExtension_Encryption_ProtectionScheme {D054D096-28BB-45DA-87EC-74F351871406}
  // Type: UINT32
  // Specifies the cipher and mode used to encrypt the content
  MFSampleExtension_Encryption_ProtectionScheme   :  TGUID = '{d054d096-28bb-45da-87ec-74f351871406}';
  {$EXTERNALSYM MFSampleExtension_Encryption_ProtectionScheme}


type
  PMFSampleEncryptionProtectionScheme = ^MFSampleEncryptionProtectionScheme;
  _MFSampleEncryptionProtectionScheme = UINT32;
  {$EXTERNALSYM _MFSampleEncryptionProtectionScheme}
  MFSampleEncryptionProtectionScheme = _MFSampleEncryptionProtectionScheme;
  {$EXTERNALSYM MFSampleEncryptionProtectionScheme}
const
  MF_SAMPLE_ENCRYPTION_PROTECTION_SCHEME_NONE    = MFSampleEncryptionProtectionScheme(0);
  {$EXTERNALSYM MF_SAMPLE_ENCRYPTION_PROTECTION_SCHEME_NONE}
  MF_SAMPLE_ENCRYPTION_PROTECTION_SCHEME_AES_CTR = MFSampleEncryptionProtectionScheme(1);
  {$EXTERNALSYM MF_SAMPLE_ENCRYPTION_PROTECTION_SCHEME_AES_CTR}
  MF_SAMPLE_ENCRYPTION_PROTECTION_SCHEME_AES_CBC = MFSampleEncryptionProtectionScheme(2);
  {$EXTERNALSYM MF_SAMPLE_ENCRYPTION_PROTECTION_SCHEME_AES_CBC}


const

  // MFSampleExtension_Encryption_CryptByteBlock {9D84289B-0C7F-4713-AB95-108AB42AD801}
  // Type: UINT32
  // Represents the number of encrypted blocks in the protection pattern, where each block is 16 bytes.
  MFSampleExtension_Encryption_CryptByteBlock   :  TGUID = '{9d84289b-0c7f-4713-ab95-108ab42ad801}';
  {$EXTERNALSYM MFSampleExtension_Encryption_CryptByteBlock}

  // MFSampleExtension_Encryption_SkipByteBlock {0D550548-8317-4AB1-845F-D06306E293E3}
  // Type: UINT32
  // Represents the number of unencrypted blocks in the protection pattern, where each block is 16 bytes.
  MFSampleExtension_Encryption_SkipByteBlock    :  TGUID = '{0d550548-8317-4ab1-845f-d06306e293e3}';
  {$EXTERNALSYM MFSampleExtension_Encryption_SkipByteBlock}

////////////////////////////////////////////////////////////////////////////////



  // Attributes for HW-DRM support
  //==============================
  //@@MFSampleExtension_Encryption_SubSample_Mapping
  /// <summary>
  /// The data blob associated with this attribute should contain an array of byte
  /// ranges as DWORDs where every two DWORDs make a set. The first DWORD in each set
  /// is the number of clear bytes and the second DWORD of the set is the number of
  /// encrypted bytes.
  /// Note that a pair of 0s is not a valid set (either value can be 0, but not both).
  /// The array of byte ranges that indicate which ranges to decrypt, including the
  /// possibility that the entire sample should NOT be decrypted.
  /// It must be set on an IMFSample using SetBlob
  /// </summary>
  MFSampleExtension_Encryption_SubSample_Mapping  :  TGUID = '{8444F27A-69A1-48DA-BD08-11CEF36830D2}';
  {$EXTERNALSYM MFSampleExtension_Encryption_SubSample_Mapping}

  // MFSampleExtension_Encryption_ClearSliceHeaderData {5509A4F4-320D-4E6C-8D1A-94C66DD20CB0}
  // /*
  // The MF blob should be parsed in the way below defined in SliceHeaderSet, with proper verifications
  //
  // =============================================================================================================
  // Note the slice header data here DO NOT have all bits for all the syntaxes.
  // Some bits are removed on purpose to send out a lossy compressed slice header in order to be 100% secure
  // The partial slice header data here SHALL not include any bits for emulation prevention byte 0x03
  // =============================================================================================================
  //
  // typedef struct SliceHeader_tag {
  // WORD dSliceHeaderLen;                   // indicate the length of the following slice header in byte, it shall not be more than 1024
  // BYTE SliceHeaderBytes[0];               // slice header data, the last byte might contain some bits not used, leave them random
  // } SliceHeader;
  //
  // With dSliceHeaderLen bytes serialized after the SliceHeader struct.
  // And then use an array of these serialized consecutively,
  //
  // typedef struct SliceHeaderSet_tag {
  // WORD dNumHeaders;                       // indicate the number of slice headers in the input sample
  // SliceHeader rgstSliceheader[0];         // cNumHeaders slice header data
  // } SliceHeaderSet;
  // */
  // Type: BLOB
  MFSampleExtension_Encryption_ClearSliceHeaderData: TGUID = '{5509a4f4-320d-4e6c-8d1a-94c66dd20cb0}';
  {$EXTERNALSYM MFSampleExtension_Encryption_ClearSliceHeaderData}

  MFSampleExtension_Encryption_HardwareProtection_KeyInfoID: TGUID = '{8CBFCCEB-94A5-4DE1-8231-A85E47CF81E7}';
  {$EXTERNALSYM MFSampleExtension_Encryption_HardwareProtection_KeyInfoID}
  // MFSampleExtension_Encryption_HardwareProtection_KeyInfoID {8CBFCCEB-94A5-4DE1-8231-A85E47CF81E7}
  // Type: GUID
  // This attribute applies to media samples. The GUID associated with this
  // attribute indicates an identifier (KID/LID) for the hardware protection to be
  // used for the given sample. All hardware protected samples flowing out of the
  // MFT decryptor should have this attribute set with the proper GUID.


  MFSampleExtension_Encryption_HardwareProtection_KeyInfo: TGUID = '{B2372080-455B-4DD7-9989-1A955784B754}';
  {$EXTERNALSYM MFSampleExtension_Encryption_HardwareProtection_KeyInfo}
  // MFSampleExtension_Encryption_HardwareProtection_KeyInfo {B2372080-455B-4DD7-9989-1A955784B754}
  // Type: BLOB
  // This attribute applies to media samples. The data blob associated with this
  // sample has all the information relative to the slot/ID for the hardware
  // protection to be used for the given sample. All hardware protected samples
  // flowing out of the MFT decryptor should have this attribute set with the
  // proper blob.

  MFSampleExtension_Encryption_HardwareProtection_VideoDecryptorContext: TGUID = '{693470C8-E837-47A0-88CB-535B905E3582}';
  {$EXTERNALSYM MFSampleExtension_Encryption_HardwareProtection_VideoDecryptorContext}
  // MFSampleExtension_Encryption_HardwareProtection_VideoDecryptorContext {693470C8-E837-47A0-88CB-535B905E3582}
  // Data type: IUnknown * (IMFContentDecryptorContext)
  // This attribute applies to media samples. It associates a sample with a
  // given IMFContentDecryptorContext which is needed to be able to to
  // decrypt/decode the sample properly when using hardware protection.

  MFSampleExtension_Encryption_Opaque_Data: TGUID = '{224D77E5-1391-4FFB-9F41-B432F68C611D}';
  {$EXTERNALSYM MFSampleExtension_Encryption_Opaque_Data}
  // MFSampleExtension_Encryption_Opaque_Data {224D77E5-1391-4FFB-9F41-B432F68C611D}
  // Data type : BLOB
  // This attribute applies to media samples.The data blob associated with this sample has some private information
  // set by OEM secure environment to be used for the given sample.The hardware protected samples flowing out of the
  // MFT decryptor might have this attribute set with the proper blob.
  // When present, this attribute is set by the decryptor MFT with data that originates from the OEM secure environment.
  // The host decoder may extract this and provide the data to the D3D11 device for VLD decoding through(UINT  PrivateDataSize, void* pPrivateData)
  // of D3D11_VIDEO_DECODER_BEGIN_FRAME_CRYPTO_SESSION data structure in the DecoderBeginFrame() call, when present.

  MFSampleExtension_NALULengthInfo: TGUID = '{19124E7C-AD4B-465F-BB18-20186287B6AF}';
  {$EXTERNALSYM MFSampleExtension_NALULengthInfo}
  // MFSampleExtension_NALULengthInfo. This is an alias of  MF_NALU_LENGTH_INFORMATION
  // Type: BLOB
  // Set MFSampleExtension_NALULengthInfo as a BLOB on the input sample,
  // with one DWORD for each NALU including start code and NALU type in the sample. For example, if
  // there are AUD (9 bytes), SPS (25 bytes), PPS (10 bytes), IDR slice1 (50 k), IDR slice 2 (60 k),
  // then there should be 5 DWORDs with values 9, 25, 10, 50 k, 60 k in the BLOB.

  MFSampleExtension_Encryption_ResumeVideoOutput:  TGUID = '{a435aba5-afde-4cf5-bc1c-f6acaf13949d}';
  {$EXTERNALSYM MFSampleExtension_Encryption_ResumeVideoOutput}
  // MFSampleExtension_Encryption_ResumeVideoOutput. {A435ABA5-AFDE-4CF5-BC1C-F6ACAF13949D}
  // Type: UINT32
  //
  // This attribute shall be used in hardware DRM scenario only
  // it is set on input compressed sample to (H.264/HEVC) video decoder
  //
  // when present, it indicates video output in video render should resume on the first output (uncompressed) sample
  // with the attribute MFSampleExtension_Encryption_ResumeVideoOutput set to true
  //
  // note: (H.264/HEVC) video decoder should buffer the attribute when video decoder
  // detects the attribute set to true on some input sample, which might be dropped since
  // those input sample might not be decode-able because of missing references,
  // and set the attribute to true on the first output sample not dropped in video decoder

  MFSampleExtension_Encryption_NALUTypes  : TGUID = '{B0F067C7-714C-416C-8D59-5F4DDF8913B6}';
  {$EXTERNALSYM MFSampleExtension_Encryption_NALUTypes}
  // MFSampleExtension_Encryption_NALUTypes. {B0F067C7-714C-416C-8D59-5F4DDF8913B6}
  // Type: BLOB
  // The MF blob contains all the NALU type byte for different NALUs in the MF sample.One NALU type is one byte, including the syntaxes forbidden_zero_bit, nal_ref_idc, and nal_unit_type.

  MFSampleExtension_Encryption_SPSPPSData  : TGUID = '{AEDE0FA2-0E0C-453C-B7F3-DE8693364D11}';
  {$EXTERNALSYM MFSampleExtension_Encryption_SPSPPSData}
  // MFSampleExtension_Encryption_SPSPPSData {AEDE0FA2-0E0C-453C-B7F3-DE8693364D11}
  // Type : BLOB
  // When present, the MF blob contains all SPS(s) and / or PPS(s) NALUs inside the MF sample.
  // SPSs and PPSs shall be present in the same order as that in the MF sample and in the format of AvcC,
  // which is DWORD, four - byte length inforamtion for the bytes followed, and NALU data of SPS or PPS, for each NALU.
  // For example, the layout could be 10 in DWORD, 10 bytes data for SPS, 5 in DWORD, and 5 bytes data for PPS.In total, it has 4 + 10 + 4 + 5 = 23 bytes.

  MFSampleExtension_Encryption_SEIData  : TGUID = '{3CF0E972-4542-4687-9999-585F565FBA7D}';
  {$EXTERNALSYM MFSampleExtension_Encryption_SEIData}
  // MFSampleExtension_Encryption_SEIData {3CF0E972-4542-4687-9999-585F565FBA7D}
  // Type : BLOB
  // When present, the MF blob contains all SEI NALUs inside the MF sample. (If there are multiple SEIs in the protected MF sample, all the SEIs shall be present in the blob.)
  // SEIs shall be present in the same order as that in the MF sample and in the format of AvcC,
  // which is DWORD, four - byte length inforamtion for the bytes followed, and NALU data of SEI.
  // For example, the layout could be 10 in DWORD, 10 bytes data for the first SEI, 5 in DWORD, and 5 bytes data for the second SEI.In total, it has 4 + 10 + 4 + 5 = 23 bytes.
  //
  // Some note about  how to process the SEI NALUs in the blob of MFSampleExtension_Encryption_SEIData
  // Decoder should verify every byte of an SEI NALU is clear, not protected, before parsing the SEI NALU
  // otherwise, decoder should treat the SEI NALU as corrupted by encryption and skip the parsing of the SEI NALU

  MFSampleExtension_Encryption_HardwareProtection  : TGUID = '{9a2b2d2b-8270-43e3-8448-994f426e8886}';
  {$EXTERNALSYM MFSampleExtension_Encryption_HardwareProtection}
  // MFSampleExtension_Encryption_HardwareProtection {9A2B2D2B-8270-43E3-8448-994F426E8886}
  // Type: UINT32
  // When present, this UINT32 attribute indicates whether the sample is hardware protected.
  // 0 = not hardware protected, nonzero = hardware protected

  MFSampleExtension_CleanPoint              : TGuid = '{9cdf01d8-a0f0-43ba-b077-eaa06cbd728a}';
  {$EXTERNALSYM MFSampleExtension_CleanPoint}
  // MFSampleExtension_CleanPoint {9cdf01d8-a0f0-43ba-b077-eaa06cbd728a}
  // Type: UINT32
  // If present and nonzero, indicates that the sample is a clean point (key
  // frame), and decoding can begin at this sample.
  //updt 090812 add

  MFSampleExtension_Discontinuity           : TGuid = '{9cdf01d9-a0f0-43ba-b077-eaa06cbd728a}';
  {$EXTERNALSYM MFSampleExtension_Discontinuity}
  // MFSampleExtension_Discontinuity {9cdf01d9-a0f0-43ba-b077-eaa06cbd728a}
  // Type: UINT32
  // If present and nonzero, indicates that the sample data represents the first
  // sample following a discontinuity (gap) in the stream of samples.
  // This can happen, for instance, if the previous sample was lost in
  // transmission.

  MFSampleExtension_Token                   : TGuid = '{8294da66-f328-4805-b551-00deb4c57a61}';
  {$EXTERNALSYM MFSampleExtension_Token}
  // MFSampleExtension_Token {8294da66-f328-4805-b551-00deb4c57a61}
  // Type: IUNKNOWN
  // When an IMFMediaStream delivers a sample via MEMediaStream, this attribute
  // should be set to the IUnknown *pToken argument that was passed with the
  // IMFMediaStream.RequestSample call to which this sample corresponds.

  MFSampleExtension_ClosedCaption_CEA708   :  TGUID = '{26f09068-e744-47dc-aa03-dbf20403bde6}';
  {$EXTERNALSYM MFSampleExtension_ClosedCaption_CEA708}
  // MFSampleExtension_ClosedCaption_CEA708 {26f09068-e744-47dc-aa03-dbf20403bde6}
  // Type: BLOB
  // MF sample attribute contained the closed caption data in CEA-708 format.

  MFSampleExtension_ClosedCaption_CEA708_MAX_SIZE = 256;
  {$EXTERNALSYM MFSampleExtension_ClosedCaption_CEA708_MAX_SIZE}

  MFSampleExtension_DecodeTimestamp        :   TGUID = '{73a954d4-09e2-4861-befc-94bd97c08e6e}';
  {$EXTERNALSYM MFSampleExtension_DecodeTimestamp}
  // MFSampleExtension_DecodeTimestamp {73A954D4-09E2-4861-BEFC-94BD97C08E6E}
  // Type : UINT64
  // If present, contains the DTS (Decoding Time Stamp) of the sample.

  MFSampleExtension_VideoEncodeQP :  TGUID = '{b2efe478-f979-4c66-b95e-ee2b82c82f36}';
  {$EXTERNALSYM MFSampleExtension_VideoEncodeQP}
  // MFSampleExtension_VideoEncodeQP {B2EFE478-F979-4C66-B95E-EE2B82C82F36}
  // Type: UINT64
  // Used by video encoders to specify the QP used to encode the output sample.

  MFSampleExtension_VideoEncodePictureType :  TGUID = '{973704e6-cd14-483c-8f20-c9fc0928bad5}';
  {$EXTERNALSYM MFSampleExtension_VideoEncodePictureType}
  // MFSampleExtension_VideoEncPictureType {973704E6-CD14-483C-8F20-C9FC0928BAD5}
  // Type: UINT32
  // Used by video encoders to specify the output sample's picture type.

  MFSampleExtension_FrameCorruption   :  TGUID = '{b4dd4a8c-0beb-44c4-8b75-b02b913b04f0}';
  {$EXTERNALSYM MFSampleExtension_FrameCorruption}
  // MFSampleExtension_FrameCorruption {B4DD4A8C-0BEB-44C4-8B75-B02B913B04F0}
  // Type: UINT32
  // Indicates whether the frame in the sample has corruption or not
  // value 0 indicates that there is no corruption, or it is unknown
  // Value 1 indicates that some corruption was detected e.g, during decoding

  //#if (WINVER >= _WIN32_WINNT_WINTHRESHOLD)  // Windows 10

  MFSampleExtension_DirtyRects  :  TGUID = '{9ba70225-b342-4e97-9126-0b566ab7ea7e}';
  {$EXTERNALSYM MFSampleExtension_DirtyRects}
  // MFSampleExtension_DirtyRects {9BA70225-B342-4E97-9126-0B566AB7EA7E}
  // Type: BLOB
  // This is a blob containing information about the dirty rectangles within
  // a frame. The blob is a struct of type DIRTYRECT_INFO containing an array
  // of NumDirtyRects number of DirtyRects elements.

  MFSampleExtension_MoveRegions  : TGUID = '{e2a6c693-3a8b-4b8d-95d0-f60281a12fb7}';
  {$EXTERNALSYM MFSampleExtension_MoveRegions}
  // MFSampleExtension_MoveRegions {E2A6C693-3A8B-4B8D-95D0-F60281A12FB7}
  // Type: BLOB
  // This is a blob containing information about the moved regions within
  // a frame. The blob is a struct of type MOVEREGION_INFO containing an array
  // of NumMoveRegions number of MoveRegions elements.


type

  PMoveRect = ^MOVE_RECT;
  PMOVE_RECT = ^MOVE_RECT;
  {$EXTERNALSYM _MOVE_RECT}
  _MOVE_RECT = record
    SourcePoint: TPointF;
    DestRect: TRect;
  end;
  {$EXTERNALSYM MOVE_RECT}
  MOVE_RECT = _MOVE_RECT;
  {$EXTERNALSYM MOVE_RECT}


  PDirtyrectInfo = ^DIRTYRECT_INFO;
  {$EXTERNALSYM _DIRTYRECT_INFO}
  _DIRTYRECT_INFO = record
    FrameNumber: UINT;
    NumDirtyRects: UINT;
    DirtyRects: array [0..0] of TRect;
  end;
  {$EXTERNALSYM DIRTYRECT_INFO}
  DIRTYRECT_INFO = _DIRTYRECT_INFO;
  {$EXTERNALSYM DIRTYRECT_INFO}


  PMoveregionInfo = ^MOVEREGION_INFO;
  {$EXTERNALSYM _MOVEREGION_INFO}
  _MOVEREGION_INFO = record
    FrameNumber: UINT;
    NumMoveRegions: UINT;
    MoveRegions: array [0..0] of MOVE_RECT;
  end;
  {$EXTERNALSYM MOVEREGION_INFO}
  MOVEREGION_INFO = _MOVEREGION_INFO;
  {$EXTERNALSYM MOVEREGION_INFO}


const

  MFSampleExtension_HDCP_OptionalHeader :	TGUID = '{9a2e7390-121f-455f-8376-c97428e0b540}';
  {$EXTERNALSYM MFSampleExtension_HDCP_OptionalHeader}
  // MFSampleExtension_HDCP_OptionalHeader
  // Type: BLOB
  // This blob contains LPCM header in front of LPCM sample in a PES packet. It is
  // encrypted when HDCP 2.x frames are sent, and is needed for decryption.

  MFSampleExtension_HDCP_FrameCounter  :  TGUID = '{9d389c60-f507-4aa6-a40a-71027a02f3de}';
  {$EXTERNALSYM MFSampleExtension_HDCP_FrameCounter}
  // MFSampleExtension_HDCP_FrameCounter
  // Type: BLOB
  // This blob contains the PES_private_data section of a PES packet according to the
  // HDCP 2.2/2.1 specification. This blob should contain the stream counter and
  // input counter.

  MFSampleExtension_HDCP_StreamID :	TGUID = '{177e5d74-c370-4a7a-95a2-36833c01d0af}';
  {$EXTERNALSYM MFSampleExtension_HDCP_StreamID}
  // MFSampleExtension_HDCP_StreamID {177E5D74-C370-4A7A-95A2-36833C01D0AF}
  // Type: UINT32
  // This UINT32 value is provided to the HDCP Encryptor MFT on each input sample.
  // The Stream ID value allows the HDCP Encryptor MFT to support time-multiplexed
  // encryption of multiple independent streams. An example is using 0 for first
  // display video stream, 1 for second display video stream, 2 for first display audio
  // stream, 3 for second display audio stream.
  // Per the HDCP 2.2 specification, this value is referred to as streamCtr. It is called
  // StreamID here to be more intuitive.

  MFSampleExtension_Timestamp    :  TGUID = '{1e436999-69be-4c7a-9369-70068c0260cb}';
  {$EXTERNALSYM MFSampleExtension_Timestamp}
  // MFSampleExtension_Timestamp
  // Type: int64
  // { 1e436999-69be-4c7a-9369-70068c0260cb } MFSampleExtension_Timestamp  {INT64 }
  // The timestamp of a sample
  //

  MFSampleExtension_RepeatFrame    :  TGUID = '{88be738f-0711-4f42-b458-344aed42ec2f}';
  {$EXTERNALSYM MFSampleExtension_RepeatFrame}
  // MFSampleExtension_RepeatFrame {88BE738F-0711-4F42-B458-344AED42EC2F}
  // Type: UINT32
  // This UINT32 when set to 1 indicates that the frame is a repeat of the previous frame

  MFT_ENCODER_ERROR    :  TGUID = '{c8d1eda4-98e4-41d5-9297-44f53852f90e}';
  {$EXTERNALSYM MFT_ENCODER_ERROR}
  // MFT_ENCODER_ERROR {C8D1EDA4-98E4-41D5-9297-44F53852F90E}
  // Type: GUID
  // This is the GUID of a property that caused the encoder MFT to fail initialization
  // MFT_GFX_DRIVER_VERSION_ID_Attribute {F34B9093-05E0-4B16-993D-3E2A2CDE6AD3}

  MFT_GFX_DRIVER_VERSION_ID_Attribute    :  TGUID = '{f34b9093-05e0-4b16-993d-3e2a2cde6ad3}';
  {$EXTERNALSYM MFT_GFX_DRIVER_VERSION_ID_Attribute}
  // Type: WSTR
  // For hardware MFTs, this attribute allows the HMFT to report the graphics driver version.

// #endif  // _WIN32_WINNT_WINTHRESHOLD



  /////////////////////////////////////////////////////////////////////////////
  //
  // The following sample attributes are used for encrypted samples
  //
  /////////////////////////////////////////////////////////////////////////////


  // MFSampleExtension_DescrambleData {43483BE6-4903-4314-B032-2951365936FC}
  // Type: UINT64
  MFSampleExtension_DescrambleData          : TGuid = '{43483be6-4903-4314-b032-2951365936fc}';
  {$EXTERNALSYM MFSampleExtension_DescrambleData}

  // MFSampleExtension_SampleKeyID {9ED713C8-9B87-4B26-8297-A93B0C5A8ACC}
  // Type: UINT32
  MFSampleExtension_SampleKeyID             : TGuid = '{9ed713c8-9b87-4b26-8297-a93b0c5a8acc}';
  {$EXTERNALSYM MFSampleExtension_SampleKeyID}

  // MFSampleExtension_GenKeyFunc {441CA1EE-6B1F-4501-903A-DE87DF42F6ED}
  // Type: UINT64
  MFSampleExtension_GenKeyFunc              : TGuid = '{441ca1ee-6b1f-4501-903a-de87df42f6ed}';
  {$EXTERNALSYM MFSampleExtension_GenKeyFunc}

  // MFSampleExtension_GenKeyCtx {188120CB-D7DA-4B59-9B3E-9252FD37301C}
  // Type: UINT64
  MFSampleExtension_GenKeyCtx               : TGuid = '{188120cb-d7da-4b59-9b3e-9252fd37301c}';
  {$EXTERNALSYM MFSampleExtension_GenKeyCtx}

  // MFSampleExtension_PacketCrossOffsets {2789671D-389F-40BB-90D9-C282F77F9ABD}
  // Type: BLOB
  MFSampleExtension_PacketCrossOffsets      : TGuid = '{2789671d-389f-40bb-90d9-c282f77f9abd}';
  {$EXTERNALSYM MFSampleExtension_PacketCrossOffsets}

  // MFSampleExtension_Encryption_SampleID {6698B84E-0AFA-4330-AEB2-1C0A98D7A44D}
  // Type: BLOB
  MFSampleExtension_Encryption_SampleID     :  TGUID = '{6698b84e-0afa-4330-aeb2-1c0a98d7a44d}';
  {$EXTERNALSYM MFSampleExtension_Encryption_SampleID}

  // MFSampleExtension_Encryption_KeyID {76376591-795F-4DA1-86ED-9D46ECA109A9}
  // Type: BLOB
  MFSampleExtension_Encryption_KeyID        :  TGUID = '{76376591-795f-4da1-86ed-9d46eca109a9}';
  {$EXTERNALSYM MFSampleExtension_Encryption_KeyID}

  // MFSampleExtension_Content_KeyID {C6C7F5B0-ACCA-415B-87D9-10441469EFC6}
  // Type: GUID
  MFSampleExtension_Content_KeyID           :  TGUID = '{c6c7f5b0-acca-415b-87d9-10441469efc6}';
  {$EXTERNALSYM MFSampleExtension_Content_KeyID}

  // MFSampleExtension_Encryption_SubSampleMappingSplit {FE0254B9-2AA5-4EDC-99F7-17E89DBF9174}
  // Type: BLOB
  // Specifies the regions of clear and encrypted bytes in the sample
  MFSampleExtension_Encryption_SubSampleMappingSplit  :  TGUID = '{fe0254b9-2aa5-4edc-99f7-17e89dbf9174}';
  {$EXTERNALSYM MFSampleExtension_Encryption_SubSampleMappingSplit}


  /////////////////////////////////////////////////////////////////////////////
  //
  // MFSample STANDARD EXTENSION ATTRIBUTE GUIDs
  //
  /////////////////////////////////////////////////////////////////////////////

  // {b1d5830a-deb8-40e3-90fa-389943716461}   MFSampleExtension_Interlaced                {UINT32 (BOOL)}
  MFSampleExtension_Interlaced                  : TGuid = '{b1d5830a-deb8-40e3-90fa-389943716461}';
  {$EXTERNALSYM MFSampleExtension_Interlaced}
  // {941ce0a3-6ae3-4dda-9a08-a64298340617}   MFSampleExtension_BottomFieldFirst          {UINT32 (BOOL)}
  MFSampleExtension_BottomFieldFirst            : TGuid = '{941ce0a3-6ae3-4dda-9a08-a64298340617}';
  {$EXTERNALSYM MFSampleExtension_BottomFieldFirst}
  // {304d257c-7493-4fbd-b149-9228de8d9a99}   MFSampleExtension_RepeatFirstField          {UINT32 (BOOL)}
  MFSampleExtension_RepeatFirstField            : TGuid = '{304d257c-7493-4fbd-b149-9228de8d9a99}';
  {$EXTERNALSYM MFSampleExtension_RepeatFirstField}
  // {9d85f816-658b-455a-bde0-9fa7e15ab8f9}   MFSampleExtension_SingleField               {UINT32 (BOOL)}
  MFSampleExtension_SingleField                 : TGuid = '{9d85f816-658b-455a-bde0-9fa7e15ab8f9}';
  {$EXTERNALSYM MFSampleExtension_SingleField}
  // {6852465a-ae1c-4553-8e9b-c3420fcb1637}   MFSampleExtension_DerivedFromTopField       {UINT32 (BOOL)}
  MFSampleExtension_DerivedFromTopField         : TGuid = '{6852465a-ae1c-4553-8e9b-c3420fcb1637}';
  {$EXTERNALSYM MFSampleExtension_DerivedFromTopField}
  MFSampleExtension_MeanAbsoluteDifference      : TGuid = '{1cdbde11-08b4-4311-a6dd-0f9f371907aa}'; // Type: UINT32
  {$EXTERNALSYM MFSampleExtension_MeanAbsoluteDifference}
  MFSampleExtension_LongTermReferenceFrameInfo  : TGuid = '{9154733f-e1bd-41bf-81d3-fcd918f71332}'; // Type: UINT32
  {$EXTERNALSYM MFSampleExtension_LongTermReferenceFrameInfo}
  MFSampleExtension_ROIRectangle                : TGuid = '{3414a438-4998-4d2c-be82-be3ca0b24d43}'; // Type: BLOB
  {$EXTERNALSYM MFSampleExtension_ROIRectangle}
  // MFSampleExtension_LastSlice {2b5d5457-5547-4f07-b8c8-b4a3a9a1daac}
  MFSampleExtension_LastSlice                   : TGuid = '{2b5d5457-5547-4f07-b8c8-b4a3a9a1daac}'; // Type: UINT32
  {$EXTERNALSYM MFSampleExtension_LastSlice}

  // Indicates macroblock is not needed for output and can be skipped
  MACROBLOCK_FLAG_SKIP                = $00000001;
  {$EXTERNALSYM MACROBLOCK_FLAG_SKIP}
  // Indicates macroblock is changed from the previous frame
  MACROBLOCK_FLAG_DIRTY               = $00000002;
  {$EXTERNALSYM MACROBLOCK_FLAG_DIRTY}
  // Indicates macroblock from the previous frame has moved to a new position
  MACROBLOCK_FLAG_MOTION              = $00000004;
  {$EXTERNALSYM MACROBLOCK_FLAG_MOTION}
  // Indicates macroblock contains video playback or other continuous motion, rather than a slower moving screen capture
  MACROBLOCK_FLAG_VIDEO               = $00000008;
  {$EXTERNALSYM MACROBLOCK_FLAG_VIDEO}
  // Indicates that the motion vector values of MACROBLOCK_DATA are valid, and should be used in preference to
  // the encoder's calculated motion vector values
  MACROBLOCK_FLAG_HAS_MOTION_VECTOR   = $00000010;
  {$EXTERNALSYM MACROBLOCK_FLAG_HAS_MOTION_VECTOR}
  // Indicates that the QPDelta value of MACROBLOCK_DATA is valid, and specifies the QP of this macroblock relative
  // to the rest of the frame
  MACROBLOCK_FLAG_HAS_QP              = $00000020;
  {$EXTERNALSYM MACROBLOCK_FLAG_HAS_QP}


type
  PMACROBLOCK_DATA = ^MACROBLOCK_DATA;
  _MACROBLOCK_DATA = record
    flags: UINT32;
    motionVectorX: INT16;
    motionVectorY: INT16;
    QPDelta: INT32;
  end;
  {$EXTERNALSYM _MACROBLOCK_DATA}
  MACROBLOCK_DATA = _MACROBLOCK_DATA;
  {$EXTERNALSYM MACROBLOCK_DATA}

const

  // MFSampleExtension_FeatureMap {a032d165-46fc-400a-b449-49de53e62a6e}
  // Type: BLOB
  // Blob should contain one MACROBLOCK_DATA structure for each macroblock in the
  // input frame.
  MFSampleExtension_FeatureMap :  TGUID = '{a032d165-46fc-400a-b449-49de53e62a6e}';
  {$EXTERNALSYM MFSampleExtension_FeatureMap}


  // MFSampleExtension_ChromaOnly {1eb9179c-a01f-4845-8c04-0e65a26eb04f}
  // Type: BOOL (UINT32)
  // Set to 1 if the input sample is a chroma-only frame
  MFSampleExtension_ChromaOnly :  TGUID = '{1eb9179c-a01f-4845-8c04-0e65a26eb04f}';
  {$EXTERNALSYM MFSampleExtension_ChromaOnly}

type

  PROI_AREA = ^ROI_AREA;
  _ROI_AREA = record
    rect: TRect;
    QPDelta: INT32;
  end;
  {$EXTERNALSYM _ROI_AREA}
  ROI_AREA = _ROI_AREA;
  {$EXTERNALSYM ROI_AREA}


const

  ///////////////////////////////////////////////////////////////////////////////
  /// These are the attribute GUIDs that need to be used by MFT0 to provide
  /// thumbnail support.  We are declaring these in our internal idl first and
  /// once we pass API spec review, we can move it to the public header.
  ///////////////////////////////////////////////////////////////////////////////

  MFSampleExtension_PhotoThumbnail  :  TGUID = '{74BBC85C-C8BB-42DC-B586-DA17FFD35DCC}';
  {$EXTERNALSYM MFSampleExtension_PhotoThumbnail}
  // MFSampleExtension_PhotoThumbnail
  // {74BBC85C-C8BB-42DC-B586DA17FFD35DCC}
  // Type: IUnknown
  // If this attribute is set on the IMFSample provided by the MFT0, this will contain the IMFMediaBuffer which contains
  // the Photo Thumbnail as configured using the KSPROPERTYSETID_ExtendedCameraControl.

  MFSampleExtension_PhotoThumbnailMediaType  :  TGUID = '{61AD5420-EBF8-4143-89AF-6BF25F672DEF}';
  {$EXTERNALSYM MFSampleExtension_PhotoThumbnailMediaType}
  // MFSampleExtension_PhotoThumbnailMediaType
  // {61AD5420-EBF8-4143-89AF6BF25F672DEF}
  // Type: IUnknown
  // This attribute will contain the IMFMediaType which describes the image format type contained in the
  // MFSampleExtension_PhotoThumbnail attribute.  If the MFSampleExtension_PhotoThumbnail attribute
  // is present on the photo sample, the MFSampleExtension_PhotoThumbnailMediaType is required.

  MFSampleExtension_CaptureMetadata  :  TGUID = '{2EBE23A8-FAF5-444A-A6A2-EB810880AB5D}';
  {$EXTERNALSYM MFSampleExtension_CaptureMetadata}
  // MFSampleExtension_CaptureMetadata
  // Type: IUnknown (IMFAttributes)
  // This is the IMFAttributes store for all the metadata related to the capture
  // pipeline.  It can be potentially present on any IMFSample.

  MFSampleExtension_MDLCacheCookie  :  TGUID = '{5F002AF9-D8F9-41A3-B6C3-A2AD43F647AD}';
  {$EXTERNALSYM MFSampleExtension_MDLCacheCookie}
  // MFSampleExtension_MDLCacheCookie
  // Type: IUnknown (IMFAttributes)
  // This is the IMFAttributes stored in the sample if the mini driver
  // desires to cache MDL's. This is used internally by the pipeline.
  // {5F002AF9-D8F9-41A3-B6C3-A2AD43F647AD}


// Put all MF_CAPTURE_METADATA_* here.

  MF_CAPTURE_METADATA_PHOTO_FRAME_FLASH  :  TGUID = '{0F9DD6C6-6003-45D8-BD59-F1F53E3D04E8}';
  {$EXTERNALSYM MF_CAPTURE_METADATA_PHOTO_FRAME_FLASH}
  // {0F9DD6C6-6003-45D8-BD59-F1F53E3D04E8}   MF_CAPTURE_METADATA_PHOTO_FRAME_FLASH       {UINT32}
  // 0 - No flash triggered on this frame.
  // non-0 - Flash triggered on this frame.
  // Do not explicitly check for a value of 1 here, we may overload this to
  // indicate special types of flash going forward (applications should only
  // check for != 0 to indicate flash took place).

  MF_CAPTURE_METADATA_FRAME_RAWSTREAM  :  TGUID = '{9252077B-2680-49B9-AE02-B19075973B70}';
  {$EXTERNALSYM MF_CAPTURE_METADATA_FRAME_RAWSTREAM}
  // The raw IUnknown corresponding to the IMFMediaBuffer that contains the metadata
  // stream as written by the camera driver.  This may be a mix of pre-defined metadata
  // such as photo confirmation, focus notification, or custom metadata that only
  // the MFT0 can parse.

  MF_CAPTURE_METADATA_FOCUSSTATE                  :  TGUID = '{a87ee154-997f-465d-b91f-29d53b982b88}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_FOCUSSTATE}
  MF_CAPTURE_METADATA_REQUESTED_FRAME_SETTING_ID  :  TGUID = '{bb3716d9-8a61-47a4-8197-459c7ff174d5}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_REQUESTED_FRAME_SETTING_ID}
  MF_CAPTURE_METADATA_EXPOSURE_TIME               :  TGUID = '{16b9ae99-cd84-4063-879d-a28c7633729e}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_EXPOSURE_TIME}
  MF_CAPTURE_METADATA_EXPOSURE_COMPENSATION       :  TGUID = '{d198aa75-4b62-4345-abf3-3c31fa12c299}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_EXPOSURE_COMPENSATION}
  MF_CAPTURE_METADATA_ISO_SPEED                   :  TGUID = '{e528a68f-b2e3-44fe-8b65-07bf4b5a13ff}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_ISO_SPEED}
  MF_CAPTURE_METADATA_LENS_POSITION               :  TGUID = '{b5fc8e86-11d1-4e70-819b-723a89fa4520}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_LENS_POSITION}
  MF_CAPTURE_METADATA_SCENE_MODE                  :  TGUID = '{9cc3b54d-5ed3-4bae-b388-7670aef59e13}'; // TYPE: UINT64
  {$EXTERNALSYM MF_CAPTURE_METADATA_SCENE_MODE}
  MF_CAPTURE_METADATA_FLASH                       :  TGUID = '{4A51520B-FB36-446C-9DF2-68171B9A0389}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_FLASH}
  MF_CAPTURE_METADATA_FLASH_POWER                 :  TGUID = '{9C0E0D49-0205-491A-BC9D-2D6E1F4D5684}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_FLASH_POWER}
  MF_CAPTURE_METADATA_WHITEBALANCE                :  TGUID = '{C736FD77-0FB9-4E2E-97A2-FCD490739EE9}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_WHITEBALANCE}
  MF_CAPTURE_METADATA_ZOOMFACTOR                  :  TGUID = '{e50b0b81-e501-42c2-abf2-857ecb13fa5c}'; // TYPE: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_ZOOMFACTOR}
  MF_CAPTURE_METADATA_FACEROIS                    :  TGUID = '{864f25a6-349f-46b1-a30e-54cc22928a47}'; // TYPE: BLOB
  {$EXTERNALSYM MF_CAPTURE_METADATA_FACEROIS}
  MF_CAPTURE_METADATA_FACEROITIMESTAMPS           :  TGUID = '{e94d50cc-3da0-44d4-bb34-83198a741868}'; // TYPE: BLOB
  {$EXTERNALSYM MF_CAPTURE_METADATA_FACEROITIMESTAMPS}
  MF_CAPTURE_METADATA_FACEROICHARACTERIZATIONS    :  TGUID = '{b927a1a8-18ef-46d3-b3af-69372f94d9b2}'; // TYPE: BLOB
  {$EXTERNALSYM MF_CAPTURE_METADATA_FACEROICHARACTERIZATIONS}
  MF_CAPTURE_METADATA_ISO_GAINS                   :  TGUID = '{05802AC9-0E1D-41c7-A8C8-7E7369F84E1E}'; // TYPE: BLOB
  {$EXTERNALSYM MF_CAPTURE_METADATA_ISO_GAINS}
  MF_CAPTURE_METADATA_SENSORFRAMERATE             :  TGUID = '{DB51357E-9D3D-4962-B06D-07CE650D9A0A}'; // TYPE: UINT64
  {$EXTERNALSYM MF_CAPTURE_METADATA_SENSORFRAMERATE}
  MF_CAPTURE_METADATA_WHITEBALANCE_GAINS          :  TGUID = '{e7570c8f-2dcb-4c7c-aace-22ece7cce647}'; // TYPE: BLOB
  {$EXTERNALSYM MF_CAPTURE_METADATA_WHITEBALANCE_GAINS}
  MF_CAPTURE_METADATA_HISTOGRAM                   :  TGUID = '{85358432-2ef6-4ba9-a3fb-06d82974b895}'; // TYPE: BLOB
  {$EXTERNALSYM MF_CAPTURE_METADATA_HISTOGRAM}
  MF_CAPTURE_METADATA_EXIF                        :  TGUID = '{2e9575b8-8c31-4a02-8575-42b197b71592}'; // TYPE: BLOB
  {$EXTERNALSYM MF_CAPTURE_METADATA_EXIF}




  MF_CAPTURE_METADATA_FRAME_ILLUMINATION            :  TGUID = '{6D688FFC-63D3-46FE-BADA-5B947DB0D080}'; // TYPE: UINT64
  {$EXTERNALSYM MF_CAPTURE_METADATA_FRAME_ILLUMINATION}
  // Stores USB Video Class Camera's payload header for user mode components to
  // get the camera timestamps and other header information.
  MF_CAPTURE_METADATA_UVC_PAYLOADHEADER             :  TGUID = '{F9F88A87-E1DD-441E-95CB-42E21A64F1D9}'; // Type: Blob
  {$EXTERNALSYM MF_CAPTURE_METADATA_UVC_PAYLOADHEADER}
  // Minimum reliable depth value in a D16 format depth frame.
  // Default value if the attribute is absent is 1, because 0 represent invalid depth
  MFSampleExtension_Depth_MinReliableDepth          :  TGUID = '{5F8582B2-E36B-47C8-9B87-FEE1CA72C5B0}'; // Type: UINT32
  {$EXTERNALSYM MFSampleExtension_Depth_MinReliableDepth}
  // Maximum reliable depth value in a D16 format depth frame
  // Default value if the attribute is absent is 65535
  MFSampleExtension_Depth_MaxReliableDepth          :  TGUID = '{E45545D1-1F0F-4A32-A8A7-6101A24EA8BE}'; // Type: UINT32
  {$EXTERNALSYM MFSampleExtension_Depth_MaxReliableDepth}
  // Stores value of the start of scan in QPC time
  MF_CAPTURE_METADATA_FIRST_SCANLINE_START_TIME_QPC :  TGUID = '{6a2c49f1-e052-46b6-b2d9-73c1558709af}'; // Type: UINT64
  {$EXTERNALSYM MF_CAPTURE_METADATA_FIRST_SCANLINE_START_TIME_QPC}
  // Stores value of the end of scan in QPC time
  MF_CAPTURE_METADATA_LAST_SCANLINE_END_TIME_QPC    :  TGUID = '{dccadecb-c4d4-400d-b418-10e88525e1f6}'; // Type: UINT64
  {$EXTERNALSYM MF_CAPTURE_METADATA_LAST_SCANLINE_END_TIME_QPC}
  // Stores value of timestamp accuracy in QPC time absolute value
  MF_CAPTURE_METADATA_SCANLINE_TIME_QPC_ACCURACY    :  TGUID = '{4cd79c51-f765-4b09-b1e1-27d1f7ebea09}'; // Type: UINT64
  {$EXTERNALSYM MF_CAPTURE_METADATA_SCANLINE_TIME_QPC_ACCURACY}
  // Bitfield of the way the scan is read. If value is $00, scan is Left to Right, Top to Bottom
  // $00 - Left -> Right
  // $01 - Right -> Left
  // $02  Bottom -> Top
  // $00 - Horizontal Scanline
  // $04 - Vertical Scanline
  MF_CAPTURE_METADATA_SCAN_DIRECTION                :  TGUID = '{6496a3ba-1907-49e6-b0c3-123795f380a9}'; // Type: UINT32
  {$EXTERNALSYM MF_CAPTURE_METADATA_SCAN_DIRECTION}


  MFCAPTURE_METADATA_SCAN_RIGHT_LEFT   =      $00000001;
  {$EXTERNALSYM MFCAPTURE_METADATA_SCAN_RIGHT_LEFT}
  MFCAPTURE_METADATA_SCAN_BOTTOM_TOP   =      $00000002;
  {$EXTERNALSYM MFCAPTURE_METADATA_SCAN_BOTTOM_TOP}
  MFCAPTURE_METADATA_SCANLINE_VERTICAL =      $00000004;
  {$EXTERNALSYM MFCAPTURE_METADATA_SCANLINE_VERTICAL}


type

  PFaceRectInfoBlobHeader = ^FaceRectInfoBlobHeader;
  tagFaceRectInfoBlobHeader = record
    Size: ULONG;                    // Size of this header + all FaceRectInfo following
    Count: ULONG;                   // Number of FaceRectInfo's in the blob
  end;
  {$EXTERNALSYM tagFaceRectInfoBlobHeader}
  FaceRectInfoBlobHeader = tagFaceRectInfoBlobHeader;
  {$EXTERNALSYM FaceRectInfoBlobHeader}

  PFaceRectInfo = ^FaceRectInfo;
  tagFaceRectInfo = record
    Region: TRect;          // Relative coordinates on the frame (Q31 format)
    confidenceLevel: LONG;  // Confidence Level of the region being a face
  end;
  {$EXTERNALSYM tagFaceRectInfo}
  FaceRectInfo = tagFaceRectInfo;
  {$EXTERNALSYM FaceRectInfo}

  PFaceCharacterizationBlobHeader = ^FaceCharacterizationBlobHeader;
  tagFaceCharacterizationBlobHeader = record
    Size: ULONG;                    // Size of this header + all FaceCharacterization following
    Count: ULONG;                   // Number of FaceCharacterization's in the blob. Must match the number of FaceRectInfo's in FaceRectInfoBlobHeader
  end;
  {$EXTERNALSYM tagFaceCharacterizationBlobHeader}
  FaceCharacterizationBlobHeader = tagFaceCharacterizationBlobHeader;
  {$EXTERNALSYM FaceCharacterizationBlobHeader}

  PFaceCharacterization = ^FaceCharacterization;
  tagFaceCharacterization = record
    BlinkScoreLeft: ULONG;          // [0, 100]. 0 indicates no blink for the left eye. 100 indicates definite blink for the left eye
    BlinkScoreRight: ULONG;         // [0, 100]. 0 indicates no blink for the right eye. 100 indicates definite blink for the right eye
    FacialExpression: ULONG;        // Any one of the MF_METADATAFACIALEXPRESSION_XXX defined
    FacialExpressionScore: ULONG;   // [0, 100]. 0 indicates no such facial expression as identified. 100 indicates definite such facial expression as defined
  end;
  {$EXTERNALSYM tagFaceCharacterization}
  FaceCharacterization = tagFaceCharacterization;
  {$EXTERNALSYM FaceCharacterization}


const
  MF_METADATAFACIALEXPRESSION_SMILE   = $00000001;
  {$EXTERNALSYM MF_METADATAFACIALEXPRESSION_SMILE}


type

  PCapturedMetadataExposureCompensation = ^CapturedMetadataExposureCompensation;
  tagCapturedMetadataExposureCompensation = record
    Flags: UINT64;                   // KSCAMERA_EXTENDEDPROP_EVCOMP_XXX step flag
    Value: INT32;                    // EV Compensation value in units of the step
  end;
  {$EXTERNALSYM tagCapturedMetadataExposureCompensation}
  CapturedMetadataExposureCompensation = tagCapturedMetadataExposureCompensation;
  {$EXTERNALSYM CapturedMetadataExposureCompensation}


  PCapturedMetadataISOGains = ^CapturedMetadataISOGains;
  tagCapturedMetadataISOGains = record
    AnalogGain: FLOAT;
    DigitalGain: FLOAT;
  end;
  {$EXTERNALSYM tagCapturedMetadataISOGains}
  CapturedMetadataISOGains = tagCapturedMetadataISOGains;
  {$EXTERNALSYM CapturedMetadataISOGains}


  PCapturedMetadataWhiteBalanceGains = ^CapturedMetadataWhiteBalanceGains;
  tagCapturedMetadataWhiteBalanceGains = record
    R: FLOAT;
    G: FLOAT;
    B: FLOAT;
  end;
  {$EXTERNALSYM tagCapturedMetadataWhiteBalanceGains}
  CapturedMetadataWhiteBalanceGains = tagCapturedMetadataWhiteBalanceGains;
  {$EXTERNALSYM CapturedMetadataWhiteBalanceGains}


  PMetadataTimeStamps = ^MetadataTimeStamps;
  tagMetadataTimeStamps = record
    Flags: ULONG;                    // Bitwise OR of MF_METADATATIMESTAMPS_XXX flags
    Device: LONGLONG;                // QPC time for the sample where the metadata is derived from (in 100ns)
    Presentation: LONGLONG;          // PTS for the sample where the metadata is derived from (in 100ns)
  end;
  {$EXTERNALSYM tagMetadataTimeStamps}
  MetadataTimeStamps = tagMetadataTimeStamps;
  {$EXTERNALSYM MetadataTimeStamps}



const
  MF_METADATATIMESTAMPS_DEVICE        = $00000001;
  {$EXTERNALSYM MF_METADATATIMESTAMPS_DEVICE}
  MF_METADATATIMESTAMPS_PRESENTATION  = $00000002;
  {$EXTERNALSYM MF_METADATATIMESTAMPS_PRESENTATION}



type
  PHistogramGrid = ^HistogramGrid;
  tagHistogramGrid = record
    Width: ULONG;    // Width of the sensor output that histogram is collected from
    Height: ULONG;   // Height of the sensor output that histogram is collected from
    Region: TRect;   // Absolute coordinates of the region on the sensor output that the histogram is collected for
  end;
  {$EXTERNALSYM tagHistogramGrid}
  HistogramGrid = tagHistogramGrid;
  {$EXTERNALSYM HistogramGrid}


  PHistogramBlobHeader = ^HistogramBlobHeader;
  tagHistogramBlobHeader = record
    Size: ULONG;                     // Size of the entire histogram blob in bytes
    Histograms: ULONG;               // Number of histograms in the blob. Each histogram is identified by a HistogramHeader
  end;
  {$EXTERNALSYM tagHistogramBlobHeader}
  HistogramBlobHeader = tagHistogramBlobHeader;
  {$EXTERNALSYM HistogramBlobHeader}


  PHistogramHeader = ^HistogramHeader;
  tagHistogramHeader = record
    Size: ULONG;                     // Size in bytes of this header + (HistogramDataHeader + histogram data following)*number of channels available
    Bins: ULONG;                     // Number of bins in the histogram
    FourCC: ULONG;                   // Color space that the histogram is collected from
    ChannelMasks: ULONG;             // Masks of the color channels that the histogram is collected for
    Grid: HistogramGrid;             // Grid that the histogram is collected from
  end;
  {$EXTERNALSYM tagHistogramHeader}
  HistogramHeader = tagHistogramHeader;
  {$EXTERNALSYM HistogramHeader}


  PHistogramDataHeader = ^HistogramDataHeader;
  tagHistogramDataHeader = record
    Size: ULONG;                     // Size in bytes of this header + histogram data following
    ChannelMask: ULONG;              // Mask of the color channel for the histogram data
    Linear: ULONG;                   // 1, if linear; 0 nonlinear
  end;
  {$EXTERNALSYM tagHistogramDataHeader}
  HistogramDataHeader = tagHistogramDataHeader;
  {$EXTERNALSYM HistogramDataHeader}



const
  MF_HISTOGRAM_CHANNEL_Y              = $00000001;
  {$EXTERNALSYM MF_HISTOGRAM_CHANNEL_Y}
  MF_HISTOGRAM_CHANNEL_R              = $00000002;
  {$EXTERNALSYM MF_HISTOGRAM_CHANNEL_R}
  MF_HISTOGRAM_CHANNEL_G              = $00000004;
  {$EXTERNALSYM MF_HISTOGRAM_CHANNEL_G}
  MF_HISTOGRAM_CHANNEL_B              = $00000008;
  {$EXTERNALSYM MF_HISTOGRAM_CHANNEL_B}
  MF_HISTOGRAM_CHANNEL_Cb             = $00000010;
  {$EXTERNALSYM MF_HISTOGRAM_CHANNEL_Cb}
  MF_HISTOGRAM_CHANNEL_Cr             = $00000020;
  {$EXTERNALSYM MF_HISTOGRAM_CHANNEL_Cr}




  ///////////////////////////////  Attributes //////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  function MFCreateAttributes(out ppMFAttributes: IMFAttributes;
                              cInitialSize: UINT32): HResult; stdcall;
  {$EXTERNALSYM MFCreateAttributes}

  function MFInitAttributesFromBlob(pAttributes: IMFAttributes;
                                    pBuf: UINT8;
                                    cbBufSize: UINT): HResult; stdcall;
  {$EXTERNALSYM MFInitAttributesFromBlob}

  function MFGetAttributesAsBlobSize(pAttributes: IMFAttributes;
                                     out pcbBufSize: UINT32): HResult; stdcall;
  {$EXTERNALSYM MFGetAttributesAsBlobSize}

  function MFGetAttributesAsBlob(pAttributes: IMFAttributes;
                                 out pBuf: UINT8;
                                 cbBufSize: UINT): HResult; stdcall;
  {$EXTERNALSYM MFGetAttributesAsBlob}



  //////////////////////////  MFT Registry categories //////////////////////////
  //////////////////////////////////////////////////////////////////////////////


  //#ifdef MF_INIT_GUIDS
  //#include <initguid.h>
  //#endif


const           // updt 090812 add

  // MFT_CATEGORY
  // The following GUIDs define categories for Media Foundation transforms (MFTs).
  // These categories are used to register and enumerate MFTs.

  MFT_CATEGORY_VIDEO_DECODER          : TGuid = '{d6c02d4b-6833-45b4-971a-05a4b04bab91}';
  {$EXTERNALSYM MFT_CATEGORY_VIDEO_DECODER}
  // {f79eac7d-e545-4387-bdee-d647d7bde42a}   MFT_CATEGORY_VIDEO_ENCODER
  MFT_CATEGORY_VIDEO_ENCODER          : TGuid = '{f79eac7d-e545-4387-bdee-d647d7bde42a}';
  {$EXTERNALSYM MFT_CATEGORY_VIDEO_ENCODER}
  // {12e17c21-532c-4a6e-8a1c-40825a736397}   MFT_CATEGORY_VIDEO_EFFECT
  MFT_CATEGORY_VIDEO_EFFECT           : TGuid = '{12e17c21-532c-4a6e-8a1c-40825a736397}';
  {$EXTERNALSYM MFT_CATEGORY_VIDEO_EFFECT}
  // {059c561e-05ae-4b61-b69d-55b61ee54a7b}   MFT_CATEGORY_MULTIPLEXER
  MFT_CATEGORY_MULTIPLEXER            : TGuid = '{059c561e-05ae-4b61-b69d-55b61ee54a7b}';
  {$EXTERNALSYM MFT_CATEGORY_MULTIPLEXER}
  // {a8700a7a-939b-44c5-99d7-76226b23b3f1}   MFT_CATEGORY_DEMULTIPLEXER
  MFT_CATEGORY_DEMULTIPLEXER          : TGuid = '{a8700a7a-939b-44c5-99d7-76226b23b3f1}';
  {$EXTERNALSYM MFT_CATEGORY_DEMULTIPLEXER}
  // {9ea73fb4-ef7a-4559-8d5d-719d8f0426c7}   MFT_CATEGORY_AUDIO_DECODER
  MFT_CATEGORY_AUDIO_DECODER          : TGuid = '{9ea73fb4-ef7a-4559-8d5d-719d8f0426c7}';
  {$EXTERNALSYM MFT_CATEGORY_AUDIO_DECODER}
  // {91c64bd0-f91e-4d8c-9276-db248279d975}   MFT_CATEGORY_AUDIO_ENCODER
  MFT_CATEGORY_AUDIO_ENCODER          : TGuid = '{91c64bd0-f91e-4d8c-9276-db248279d975}';
  {$EXTERNALSYM MFT_CATEGORY_AUDIO_ENCODER}
  // {11064c48-3648-4ed0-932e-05ce8ac811b7}   MFT_CATEGORY_AUDIO_EFFECT
  MFT_CATEGORY_AUDIO_EFFECT           : TGuid = '{11064c48-3648-4ed0-932e-05ce8ac811b7}';
  {$EXTERNALSYM MFT_CATEGORY_AUDIO_EFFECT}

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // {302EA3FC-AA5F-47f9-9F7A-C2188BB163021}...MFT_CATEGORY_VIDEO_PROCESSOR
  MFT_CATEGORY_VIDEO_PROCESSOR        : TGuid = '{302ea3fc-aa5f-47f9-9f7a-c2188bb16302}'; //updt 090812 correct GUID
  {$EXTERNALSYM MFT_CATEGORY_VIDEO_PROCESSOR}
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  // {90175d57-b7ea-4901-aeb3-933a8747756f}   MFT_CATEGORY_OTHER
  MFT_CATEGORY_OTHER                  : TGuid = '{90175d57-b7ea-4901-aeb3-933a8747756f}';
  {$EXTERNALSYM MFT_CATEGORY_OTHER}

  // #if (WINVER >= _WIN32_WINNT_WIN10_RS1)
  MFT_CATEGORY_ENCRYPTOR              :	TGuid = '{b0c687be-01cd-44b5-b8b2-7c1d7e058b1f}';
  {$EXTERNALSYM MFT_CATEGORY_ENCRYPTOR}
  // #endif
  // TODO: switch to NTDDI_WIN10_RS3 when _NT_TARGET_VERSION is updated to support RS3
  //if NTDDI_VERSION >= NTDDI_WIN10_RS2
  // {145CD8B4-92F4-4b23-8AE7-E0DF06C2DA95}   MFT_CATEGORY_VIDEO_RENDERER_EFFECT
  MFT_CATEGORY_VIDEO_RENDERER_EFFECT  :	TGuid = '{145CD8B4-92F4-4b23-8AE7-E0DF06C2DA95}';
  {$EXTERNALSYM MFT_CATEGORY_VIDEO_RENDERER_EFFECT}
  //endif


type
  //
  // Contains flags for registering and enumeration Media Foundation Transforms (MFTs).
  // These flags are used in the following functions:
  //   MFTEnumEx: These flags control which Media Foundation transforms (MFTs) are enumerated, as well as the enumeration order.
  //   MFTRegister: A subset of these flags are used when registering an MFT.

  PMFT_ENUM_FLAG = ^MFT_ENUM_FLAG;
  _MFT_ENUM_FLAG = UINT32;
  {$EXTERNALSYM _MFT_ENUM_FLAG}
  MFT_ENUM_FLAG = _MFT_ENUM_FLAG;
  {$EXTERNALSYM MFT_ENUM_FLAG}
const
  MFT_ENUM_FLAG_SYNCMFT                         = MFT_ENUM_FLAG($00000001);   // Enumerates V1 MFTs. This is default.
  {$EXTERNALSYM MFT_ENUM_FLAG_SYNCMFT}
  MFT_ENUM_FLAG_ASYNCMFT                        = MFT_ENUM_FLAG($00000002);   // Enumerates only software async MFTs also known as V2 MFTs
  {$EXTERNALSYM MFT_ENUM_FLAG_ASYNCMFT}
  MFT_ENUM_FLAG_HARDWARE                        = MFT_ENUM_FLAG($00000004);   // Enumerates V2 hardware async MFTs
  {$EXTERNALSYM MFT_ENUM_FLAG_HARDWARE}
  MFT_ENUM_FLAG_FIELDOFUSE                      = MFT_ENUM_FLAG($00000008);   // Enumerates MFTs that require unlocking
  {$EXTERNALSYM MFT_ENUM_FLAG_FIELDOFUSE}
  MFT_ENUM_FLAG_LOCALMFT                        = MFT_ENUM_FLAG($00000010);   // Enumerates Locally (in-process) registered MFTs
  {$EXTERNALSYM MFT_ENUM_FLAG_LOCALMFT}
  MFT_ENUM_FLAG_TRANSCODE_ONLY                  = MFT_ENUM_FLAG($00000020);   // Enumerates decoder MFTs used by transcode only
  {$EXTERNALSYM MFT_ENUM_FLAG_TRANSCODE_ONLY}
  MFT_ENUM_FLAG_SORTANDFILTER                   = MFT_ENUM_FLAG($00000040);   // Apply system local, do not use and preferred sorting and filtering
  {$EXTERNALSYM MFT_ENUM_FLAG_SORTANDFILTER}
  MFT_ENUM_FLAG_SORTANDFILTER_APPROVED_ONLY     = MFT_ENUM_FLAG($000000C0);   // Similar to MFT_ENUM_FLAG_SORTANDFILTER, but apply a local policy of: MF_PLUGIN_CONTROL_POLICY_USE_APPROVED_PLUGINS
  {$EXTERNALSYM MFT_ENUM_FLAG_SORTANDFILTER_APPROVED_ONLY}
  MFT_ENUM_FLAG_SORTANDFILTER_WEB_ONLY          = MFT_ENUM_FLAG($00000140);   // Similar to MFT_ENUM_FLAG_SORTANDFILTER, but apply a local policy of: MF_PLUGIN_CONTROL_POLICY_USE_WEB_PLUGINS
  {$EXTERNALSYM MFT_ENUM_FLAG_SORTANDFILTER_WEB_ONLY}
  MFT_ENUM_FLAG_SORTANDFILTER_WEB_ONLY_EDGEMODE = MFT_ENUM_FLAG($00000240);   // Similar to MFT_ENUM_FLAG_SORTANDFILTER, but apply a local policy of: MF_PLUGIN_CONTROL_POLICY_USE_WEB_PLUGINS_EDGEMODE
  {$EXTERNALSYM MFT_ENUM_FLAG_SORTANDFILTER_WEB_ONLY_EDGEMODE}
  MFT_ENUM_FLAG_ALL                             = MFT_ENUM_FLAG($0000003F);   // Enumerates all MFTs including SW and HW MFTs and applies filtering
  {$EXTERNALSYM MFT_ENUM_FLAG_ALL}



  // READ CAREFULLY!
  //================
  //
  // Valid MFT_ENUM_FLAG_ASYNCMFT flags for function MFTRegister
  // ===========================================================
  // The MFT performs asynchronous processing in software. See Asynchronous MFTs.
  // This flag does not apply to hardware transforms.
  // Requires >= Windows 7.
  //
  // MFT_ENUM_FLAG_FIELDOFUSE
  // The application must unlock the MFT in order to use it. See IMFFieldOfUseMFTUnlock.
  // Requires >= Windows 7.
  //
  // MFT_ENUM_FLAG_HARDWARE
  // The MFT performs hardware-based data processing, using either the AVStream driver or a GPU-based proxy MFT. M
  // FTs in this category always process data asynchronously. See Hardware MFTs.
  // Note  This flag applies to video codecs and video processors that perform their work entirely in hardware.
  // It does not apply to software decoders that use DirectX Video Acceleration to assist decoding.
  // Requires >= Windows 7.
  //
  // MFT_ENUM_FLAG_SYNCMFT
  // The MFT performs synchronous processing in software. This flag does not apply to hardware transforms.
  // MFT_ENUM_FLAG_TRANSCODE_ONLY
  // The MFT is optimized for transcoding and should not be used for playback.
  // Requires >= Windows 7.
  //
  function MFTRegister(const clsidMFT: CLSID;  // The CLSID of the MFT. The MFT must also be registered as a COM object using the same CLSID.
                       const guidCategory: TGuid; // GUID that specifies the category of the MFT. For a list of MFT categories, see MFT_CATEGORY .
                       pszName: LPCWSTR;  // Wide-character string that contains the friendly name of the MFT.
                       Flags: UINT32;  // Bitwise OR of zero or more of the following flags from the MFT_ENUM_FLAG enumeration.
                       cInputTypes: UINT32;  // Number of elements in the pInputTypes array.
                       pInputTypes: PMFT_REGISTER_TYPE_INFO; // Pointer to an array of MFT_REGISTER_TYPE_INFO structures. Each member of the array specifies an input format that the MFT supports. This parameter can be Nil.
                       cOutputTypes: UINT32; // Number of elements in the pOutputTypes array.
                       pOutputTypes: PMFT_REGISTER_TYPE_INFO; // Pointer to an array of MFT_REGISTER_TYPE_INFO structures. Each member of the array specifies an input format that the MFT supports. This parameter can be Nil.
                       pAttributes: IMFAttributes): HRESULT; stdcall; // Pointer to the IMFAttributes interface of an attribute store that contains additional registry information. This parameter can be Nil.
  {$EXTERNALSYM MFTRegister}

  // Unregister registered MFT
  function MFTUnregister(clsidMFT: CLSID): HResult; stdcall;

//#if (WINVER >= _WIN32_WINNT_WIN7)

  // Registers a Media Foundation transform (MFT) in the caller's process.

  function MFTRegisterLocal(pClassFactory: IClassFactory; // A pointer to the IClassFactory interface of a class factory object. The class factory creates the MFT.
                            const guidCategory: REFGUID; // A GUID that specifies the category of the MFT. For a list of MFT categories, see MFT_CATEGORY.
                            pszName: LPCWSTR; // A wide-character null-terminated string that contains the friendly name of the MFT.
                            Flags: UINT32; // A bitwise OR of zero or more flags from the MFT_ENUM_FLAG enumeration.
                            cInputTypes: UINT32; // The number of elements in the pInputTypes array.
                            pInputTypes: PMFT_REGISTER_TYPE_INFO; // A pointer to an array of MFT_REGISTER_TYPE_INFO structures. Each member of the array specifies an input format that the MFT supports. This parameter can be Nil if cInputTypes is zero.
                            cOutputTypes: UINT32; // The number of elements in the pOutputTypes array.
                            pOutputTypes: PMFT_REGISTER_TYPE_INFO): HResult; stdcall; // A pointer to an array of MFT_REGISTER_TYPE_INFO structures. Each member of the array defines an output format that the MFT supports. This parameter can be Nil if cOutputTypes is zero.
  {$EXTERNALSYM MFTRegisterLocal}

  // Unregister locally registered MFT
  // If pClassFactory is Nil all local MFTs are unregistered.
  function MFTUnregisterLocal(pClassFactory: IClassFactory): HResult; stdcall;     //updt 090812 pIClassFactory to IClassFactory
  {$EXTERNALSYM MFTUnregisterLocal}



  // Register an MFT class in-process, by CLSID
  function MFTRegisterLocalByCLSID(const clisdMFT: REFCLSID; // The class identifier (CLSID) of the MFT.
                                   const guidCategory: REFGUID; // A GUID that specifies the category of the MFT. For a list of MFT categories, see MFT_CATEGORY.
                                   pszName: LPCWSTR;  // A wide-character null-terminated string that contains the friendly name of the MFT.
                                   Flags: UINT32; // A bitwise OR of zero or more flags from the MFT_ENUM_FLAG enumeration.
                                   cInputTypes: UINT32; // The number of elements in the pInputTypes array.
                                   pInputTypes: PMFT_REGISTER_TYPE_INFO; // A pointer to an array of MFT_REGISTER_TYPE_INFO structures.
                                   cOutputTypes: UINT32; // The number of elements in the pOutputTypes array.
                                   pOutputTypes: PMFT_REGISTER_TYPE_INFO // A pointer to an array of MFT_REGISTER_TYPE_INFO structures.
                                   ): HResult; stdcall;
  {$EXTERNALSYM MFTRegisterLocalByCLSID}

  // Unregister locally registered MFT by CLSID
  function MFTUnregisterLocalByCLSID(clsidMFT: CLSID): HResult; stdcall;
  {$EXTERNALSYM MFTUnregisterLocalByCLSID}

//#endif // (WINVER >= _WIN32_WINNT_WIN7)


  // Starting in Windows 7, applications should use the MFTEnumEx function instead!
  function MFTEnum(const guidCategory: TGUID;   // GUID that specifies the category of MFTs to enumerate.
                   Flags: UINT32;         // Reserved. Must be zero.
                   pInputType: PMFT_REGISTER_TYPE_INFO;  // Pointer to an MFT_REGISTER_TYPE_INFO structure that specifies an input media type to match.
                                                         // This parameter can be Nil. If Nil, all input types are matched.
                   pOutputType: PMFT_REGISTER_TYPE_INFO; // Pointer to an MFT_REGISTER_TYPE_INFO structure that specifies an output media type to match.
                                                         // This parameter can be Nil. If Nil, all output types are matched.
                   pAttributes: PIMFAttributes;    // Reserved. Set to Nil.
             {out} ppclsidMFT: PCLSID;             // Receives an array of CLSIDs. To create an MFT from this list, call CoCreateInstance with one of the CLSIDs.
                                                   // To get information about a particular MFT from its CLSID, call MFTGetInfo.
                                                   // The caller must free the memory for the array by calling CoTaskMemFree.
                                                   // NOTE:  When using pointer array calculations, POINTERMATH should be turned ON.
                                                   //        See WinApiTypes.inc
                   out pcMFTs: UINT32): HResult; stdcall;  // Receives the number of elements in the ppclsidMFT array. The value can be zero.
  {$EXTERNALSYM MFTEnum}

  //#if (WINVER >= _WIN32_WINNT_WIN7)


// TODO: switch to NTDDI_WIN10_RS3 when _NT_TARGET_VERSION is updated to support RS3
//#if (NTDDI_VERSION >= NTDDI_WIN10_RS2)

const
// MFT_ENUM_VIDEO_RENDERER_EXTENSION_PROFILE {62C56928-9A4E-443b-B9DC-CAC830C24100}
// Type: VT_VECTOR | VT_LPWSTR
// MFTEnumEx stores this on the attribute store of the IMFActivate object that
// MFTEnumEx creates for MFTs that have an associated UWP Manifest containing the tag
// VideoRendererExtensionProfiles. This contains a list of all VideoRendererExtensionProfile
// entries in the VideoRendererExtensionProfiles tag.

  MFT_ENUM_VIDEO_RENDERER_EXTENSION_PROFILE : TGUID =  '{62c56928-9a4e-443b-b9dc-cac830c24100}';
  {$EXTERNALSYM MFT_ENUM_VIDEO_RENDERER_EXTENSION_PROFILE}

//#endif // (NTDDI_VERSION >= NTDDI_WIN10_RS2)

  //#if (NTDDI_VERSION >= NTDDI_WIN10_RS1)

  // {1D39518C-E220-4DA8-A07F-BA172552D6B1}   MFT_ENUM_ADAPTER_LUID
  MFT_ENUM_ADAPTER_LUID :	TGUID = '{1d39518c-e220-4da8-a07f-ba172552d6b1}';
  {$EXTERNALSYM MFT_ENUM_ADAPTER_LUID}

  function MFTEnum2(const guidCategory: TGUID; // A GUID that specifies the category of MFTs to enumerate. For a list of MFT categories, see MFT_CATEGORY.
                    Flags: UINT32;  // The bitwise OR of zero or more flags from the _MFT_ENUM_FLAG enumeration.
                    pInputType: MFT_REGISTER_TYPE_INFO;  // A pointer to an MFT_REGISTER_TYPE_INFO structure that specifies an input media type to match. This parameter can be Nil. If Nil, all input types are matched.
                    pOutputType: MFT_REGISTER_TYPE_INFO; // A pointer to an MFT_REGISTER_TYPE_INFO structure that specifies an output media type to match. This parameter can be Nil. If Nil, all input types are matched.
                    pAttributes: IMFAttributes; // A pointer to an IMFAttributes interface that enables access to the standard attribute store.
                    out pppMFTActivate: PIMFActivate;  // Receives a pointer to an array of IMFActivate interface pointers.
                                                       // Each pointer represents an activation object for an MFT that matches the search criteria.
                                                       // The function allocates the memory for the array.
                                                       // The caller must release the pointers and call the CoTaskMemFree function to free the memory for the array.

                    out pnumMFTActivate: UINT32): HResult; stdcall; // Receives the number of elements in the pppMFTActivate array. If no MFTs match the search criteria, this parameter receives the value zero.
  {$EXTERNALSYM MFTEnum2}

  //#endif // (NTDDI_VERSION >= NTDDI_WIN10_RS1)


  // >= Win 7

  function MFTEnumEx(const guidCategory: TGuid;  // A GUID that specifies the category of MFTs to enumerate. For a list of MFT categories, see MFT_CATEGORY.
                     Flags: UINT32;  // The bitwise OR of zero or more flags from the _MFT_ENUM_FLAG enumeration.
                     pInputType: PMFT_REGISTER_TYPE_INFO; // A pointer to an MFT_REGISTER_TYPE_INFO structure that specifies an input media type to match.
                                                          // This parameter can be Nil. If Nil, all input types are matched.
                     pOutputType: PMFT_REGISTER_TYPE_INFO;  // A pointer to an MFT_REGISTER_TYPE_INFO structure that specifies an output media type to match.
                                                            // This parameter can be Nil. If Nil, all output types are matched.
                     out pppMFTActivate: PIMFActivate;  // Receives an array of IMFActivate interface pointers.
                                                        // Each pointer represents an activation object for an MFT that matches the search criteria.
                                                        // The function allocates the memory for the array. The caller must release the pointers and call the
                                                        // CoTaskMemFree function to free the memory for the array.

                     out pnumMFTActivate: UINT32): HResult; stdcall;  // Receives the number of elements in the pppMFTActivate array. If no MFTs match the search criteria, this parameter receives the value zero.
  {$EXTERNALSYM MFTEnumEx}

  // result pppMFTActivate must be freed with CoTaskMemFree. Each IMFActivate pointer inside this
  // buffer should be released.
  //
  // In Delphi XE7 CoTaskMemFree does not free the array at once this will result in a pointer error.
  // You need to itterate the array and call CoTaskMemFree for each interface item!
  //
  // *pppMFTActivate must be released like this when using as global array:
  //
  // Shut down and release the interface pointers.   (Don't use  for _i:= Low() to High() do !!)
  // for i:= 0 to count -1 do   // count is a returned UINT32 from MFEnumDeviceSources
  //   begin
  //     CoTaskMemFree(pppMFTActivate[i]);
  //    or
  //     SafeRelease(pppMFTActivate[i]);
  //   end;
  //
  // SetLength(pppMFTActivate, 0);
  //



//#endif // (WINVER >= _WIN32_WINNT_WIN7)


  function MFTGetInfo(const clsidMFT: CLSID;
                      out pszName: PWideChar;
                      out ppInputTypes: PMFT_REGISTER_TYPE_INFO;
                      out pcInputTypes: UINT32;
                      out ppOutputTypes: PMFT_REGISTER_TYPE_INFO;
                      out pcOutputTypes: UINT32; //updt 090812 typo fix
                      out ppAttributes: IMFAttributes): HResult; stdcall;
  {$EXTERNALSYM MFTGetInfo}
  // results *pszName, *ppInputTypes, and *ppOutputTypes must be freed by caller.



//#if (WINVER >= _WIN32_WINNT_WIN7)


  function MFGetPluginControl(out ppPluginControl: IMFPluginControl): HResult; stdcall;
  // Get the plugin control API


  function MFGetMFTMerit(var pMFT: IUnknown;
                         cbVerifier: UINT32;
                         verifier: Byte;
                         out merit: DWord): HResult; stdcall;
  {$EXTERNALSYM MFGetPluginControl}
  // Get MFT's merit - checking that is has a valid certificate


//#endif // (WINVER >= _WIN32_WINNT_WIN7)


//#if (WINVER >= _WIN32_WINNT_WIN8)

  function MFRegisterLocalSchemeHandler(szScheme: PCWSTR;
                                        pActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFRegisterLocalSchemeHandler}

  function MFRegisterLocalByteStreamHandler(szFileExtension: PCWSTR;
                                            szMimeType: PCWSTR;
                                            pActivate: IMFActivate): HResult; stdcall;
  {$EXTERNALSYM MFRegisterLocalByteStreamHandler}

  function MFCreateMFByteStreamWrapper(pStream: IMFByteStream;
                                       out ppStreamWrapper: IMFByteStream): HResult; stdcall;
  {$EXTERNALSYM MFCreateMFByteStreamWrapper}
  // Wrap a bytestream so that calling Close() on the wrapper
  // closes the wrapper but not the original bytestream.
  // The original bytestream can then be passed to another
  // media source for instance.

  function MFCreateMediaExtensionActivate(szActivatableClassId: PCWSTR;
                                          pConfiguration: IUnknown;
                                          const riid: REFIID;
                                          out ppvObject: Pointer): HResult; stdcall;
  {$EXTERNALSYM MFCreateMediaExtensionActivate}
//#endif // (WINVER >= _WIN32_WINNT_WIN8)



  /////////////////////////  MFT  Attributes GUIDs /////////////////////////////
  //////////////////////////////////////////////////////////////////////////////


const    // updt 090812 add
  MFT_SUPPORT_DYNAMIC_FORMAT_CHANGE        : TGuid = '{53476A11-3F13-49fb-AC42-EE2733C96741}';
  {$EXTERNALSYM MFT_SUPPORT_DYNAMIC_FORMAT_CHANGE}
  // {53476A11-3F13-49fb-AC42-EE2733C96741} MFT_SUPPORT_DYNAMIC_FORMAT_CHANGE {UINT32 (BOOL)}



  //////////////////////////////  Media Type GUIDs ////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  // GUIDs for media types

  // In MF, media types for uncompressed video formats MUST be composed from a FourCC or D3DFORMAT combined with
  // the "base GUID" {00000000-0000-0010-8000-00AA00389B71} by replacing the initial 32 bits with the FourCC/D3DFORMAT
  //
  // Audio media types for types which already have a defined wFormatTag value can be constructed similarly, by
  // putting the wFormatTag (zero-extended to 32 bits) into the first 32 bits of the base GUID.
  //
  // Compressed video or audio can also use any well-known GUID that exists, or can create a new GUID.
  //
  // GUIDs for common media types are defined below.

{
#ifndef FCC
#define FCC(ch4) ((((DWORD)(ch4) & 0xFF) << 24) ||
                  (((DWORD)(ch4) & 0xFF00) << 8) ||
                  (((DWORD)(ch4) & 0xFF0000) >> 8) ||
                  (((DWORD)(ch4) & 0xFF000000) >> 24))
#endif


//
// this macro creates a media type GUID from a FourCC, D3DFMT, or WAVE_FORMAT
//
#ifndef DEFINE_MEDIATYPE_GUID
#define DEFINE_MEDIATYPE_GUID(name, format) \
    DEFINE_GUID(name,                       \
    format, 0x0000, 0x0010, 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71);
#endif

}

// REMARK#1
// To use these converted macro's and/or add other TGuid's,
// you have to use function DefineMediaTypeGuid defined at the Implementations section of this file.
// The following steps should be taken:
// 1 Declare  MFYourAudioOrVideo_Guid as TGuid. (var MFYourAudioOrVideo_Guid : TGuid)
// 2 Assign a valid DWord or FourCC value to get a proper Guid:
//   MFYourAudioOrVideo_Guid := DefineMediaTypeGuid(FourCC or Dword - NOT BOTH! -)
//   NOTE: Default values for both are '' (sFcc) or 0 (dwConst)


  // Peter
type
  // This function is an alias for function MAKEFOURCC defined in WinApi.MmReg.pas and WinApi.MediaFoundationApi.MfMetLib.pas
  tCh4 = array [0..3] of AnsiChar;
  function FCC(ch4: TCh4): DWord; inline;

  // Tony
  function DEFINE_MEDIATYPE_GUID(const format: DWord): TGuid; inline;
  // Parameters
  // name
  //    The name of the GUID constant to be defined.
  // format
  //    A FOURCC code, D3DFORMAT value, or audio format type.
  // Return value
  //    This function returns a TGuid.
  // Remarks
  //    Media formats are often identified by a FOURCC code (such as 'AYUV'),
  //    D3DFORMAT value (such as D3DFMT_X8R8G8B8), or audio format type (such as WAVE_FORMAT_PCM).
  //    The DEFINE_MEDIATYPE_GUID macro defines a new GUID constant from one of these values.
  //    The resulting GUID can be used as a media subtype.
  //
  //    This macro invokes the DEFINE_GUID macro.
  //    The resuling GUID constant is declared extern (MfpTypes.pas (guiddef.h)), so the declaration must have global scope.
  // Example
  //   Declares a GUID named MFVideoFormat_ABCD_Format.
  //     DEFINE_MEDIATYPE_GUID(MFVideoFormat_ABCD_Format, FCC('ABCD'));

// video media types ///////////////////////////////////////////////////////////




  // If no D3D headers have been included yet, define local versions of D3DFMT constants we use.
  // We can't include D3D headers in this header because we need it to be compatible with all versions
  // of D3D.
{$IFNDEF DIRECT3D_VERSION}
const
  D3DFMT_R8G8B8                       = 20;
  {$EXTERNALSYM D3DFMT_R8G8B8}
  D3DFMT_A8R8G8B8                     = 21;
  {$EXTERNALSYM D3DFMT_A8R8G8B8}
  D3DFMT_X8R8G8B8                     = 22;
  {$EXTERNALSYM D3DFMT_X8R8G8B8}
  D3DFMT_R5G6B5                       = 23;
  {$EXTERNALSYM D3DFMT_R5G6B5}
  D3DFMT_X1R5G5B5                     = 24;
  {$EXTERNALSYM D3DFMT_X1R5G5B5}
  D3DFMT_A2B10G10R10                  = 31;
  {$EXTERNALSYM D3DFMT_A2B10G10R10}
  D3DFMT_P8                           = 41;
  {$EXTERNALSYM D3DFMT_P8}
  D3DFMT_L8                           = 50;
  {$EXTERNALSYM D3DFMT_L8}
  D3DFMT_D16                          = 80;
  {$EXTERNALSYM D3DFMT_D16}
  D3DFMT_L16                          = 81;
  {$EXTERNALSYM D3DFMT_L16}
  D3DFMT_A16B16G16R16F                = 113;
  {$EXTERNALSYM D3DFMT_A16B16G16R16F}
  LOCAL_D3DFMT_DEFINES                = 1;
  {$EXTERNALSYM LOCAL_D3DFMT_DEFINES}
{$ENDIF}

const
  // NOTE: All GUIDs containing FourCC's as a subformat have this form: XXXXXXXX-0000-0010-8000-00AA00389B71.

  // Base format for all video media types

  // Delphi note: We had to convert D1 values to hex, to prevent "Incompatible types" issues.
  //              like: D3DFMT_A8R8G8B8 = 21 to it's hex equivalent $00000015
  MFVideoFormat_Base    : TGUID = (D1: $00000000;
                                   D2: $0000;
                                   D3: $0010;
                                   D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_Base}

  MFVideoFormat_RGB32   : TGUID = (D1: $00000016 {D3DFMT_X8R8G8B8};
                                   D2: $0000;
                                   D3: $0010;
                                   D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_RGB32}

  MFVideoFormat_ARGB32  : TGUID = (D1: $00000015 {D3DFMT_A8R8G8B8};
                                   D2: $0000;
                                   D3: $0010;
                                   D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_ARGB32}

  MFVideoFormat_RGB24   : TGUID = (D1: $00000014 {D3DFMT_R8G8B8};
                                   D2: $0000;
                                   D3: $0010;
                                   D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_RGB24}

  MFVideoFormat_RGB555  : TGUID = (D1: $00000018 {D3DFMT_X1R5G5B5};
                                   D2: $0000;
                                   D3: $0010;
                                   D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_RGB555}

  MFVideoFormat_RGB565  : TGUID = (D1: $00000017 {D3DFMT_R5G6B5};
                                   D2: $0000;
                                   D3: $0010;
                                   D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_RGB565}

  MFVideoFormat_RGB8    : TGUID = (D1: $00000029 {D3DFMT_P8};
                                   D2: $0000;
                                   D3: $0010;
                                   D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_RGB8}

  // We could translate with the FCC MACRO, but this is not possible in conjunction with constant expressions in
  // Delphi.
  // So, we do this the alternative way.

  MFVideoFormat_AI44: TGUID = (D1: ord('A') or (ord('I') shl 8) or (ord('4') shl 16) or (ord('4') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_AI44}

  MFVideoFormat_AYUV: TGUID = (D1: Ord('A') or (Ord('Y') shl 8) or (Ord('U') shl 16) or (Ord('V') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_AYUV}

  MFVideoFormat_YUY2: TGUID = (D1: Ord('Y') or (Ord('U') shl 8) or (Ord('Y') shl 16) or (Ord('2') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_YUY2}

  MFVideoFormat_YVYU: TGUID = (D1: Ord('Y') or (Ord('V') shl 8) or (Ord('Y') shl 16) or (Ord('U') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_YVYU}

  MFVideoFormat_YVU9: TGUID = (D1: Ord('Y') or (Ord('V') shl 8) or (Ord('U') shl 16) or (Ord('9') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_YVU9}

  MFVideoFormat_UYVY: TGUID = (D1: Ord('U') or (Ord('Y') shl 8) or (Ord('V') shl 16) or (Ord('Y') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_UYVY}

  MFVideoFormat_NV11: TGUID = (D1: Ord('N') or (Ord('V') shl 8) or (Ord('1') shl 16) or (Ord('1') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_NV11}

  MFVideoFormat_NV12: TGUID = (D1: Ord('N') or (Ord('V') shl 8) or (Ord('1') shl 16) or (Ord('2') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_NV12}

  MFVideoFormat_YV12: TGUID = (D1: Ord('Y') or (Ord('V') shl 8) or (Ord('1') shl 16) or (Ord('2') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
   {$EXTERNALSYM MFVideoFormat_YV12}

  MFVideoFormat_I420: TGUID = (D1: Ord('I') or (Ord('4') shl 8) or (Ord('2') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_I420}

  MFVideoFormat_IYUV: TGUID = (D1: Ord('I') or (Ord('Y') shl 8) or (Ord('U') shl 16) or (Ord('V') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_IYUV}

  MFVideoFormat_Y210: TGUID = (D1: Ord('Y') or (Ord('2') shl 8) or (Ord('1') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_Y210}

  MFVideoFormat_Y216: TGUID = (D1: Ord('Y') or (Ord('2') shl 8) or (Ord('1') shl 16) or (Ord('6') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
   {$EXTERNALSYM MFVideoFormat_Y216}

  MFVideoFormat_Y410: TGUID = (D1: Ord('Y') or (Ord('4') shl 8) or (Ord('1') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_Y410}

  MFVideoFormat_Y416: TGUID = (D1: Ord('Y') or (Ord('4') shl 8) or (Ord('1') shl 16) or (Ord('6') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_Y416}

  MFVideoFormat_Y41P: TGUID = (D1: Ord('Y') or (Ord('4') shl 8) or (Ord('1') shl 16) or (Ord('P') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_Y41P}

  MFVideoFormat_Y41T: TGUID = (D1: Ord('Y') or (Ord('4') shl 8) or (Ord('1') shl 16) or (Ord('T') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_Y41T}

  MFVideoFormat_Y42T: TGUID = (D1: Ord('Y') or (Ord('4') shl 8) or (Ord('2') shl 16) or (Ord('T') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_Y42T}

  MFVideoFormat_P210: TGUID = (D1: Ord('P') or (Ord('2') shl 8) or (Ord('1') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_P210}

  MFVideoFormat_P216: TGUID = (D1: Ord('P') or (Ord('2') shl 8) or (Ord('1') shl 16) or (Ord('6') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_P216}

  MFVideoFormat_P010: TGUID = (D1: Ord('P') or (Ord('0') shl 8) or (Ord('1') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_P010}

  MFVideoFormat_P016: TGUID = (D1: Ord('P') or (Ord('0') shl 8) or (Ord('1') shl 16) or (Ord('6') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
   {$EXTERNALSYM MFVideoFormat_P016}

  MFVideoFormat_v210: TGUID = (D1: Ord('v') or (Ord('2') shl 8) or (Ord('1') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_v210}

  MFVideoFormat_v216: TGUID = (D1: Ord('v') or (Ord('2') shl 8) or (Ord('1') shl 16) or (Ord('6') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_v216}

  MFVideoFormat_v410: TGUID = (D1: Ord('v') or (Ord('4') shl 8) or (Ord('1') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_v410}

  MFVideoFormat_MP43: TGUID = (D1: Ord('M') or (Ord('P') shl 8) or (Ord('4') shl 16) or (Ord('3') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_MP43}

  MFVideoFormat_MP4S: TGUID = (D1: Ord('M') or (Ord('P') shl 8) or (Ord('4') shl 16) or (Ord('S') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_MP4S}

  MFVideoFormat_M4S2: TGUID = (D1: Ord('M') or (Ord('4') shl 8) or (Ord('S') shl 16) or (Ord('2') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_M4S2}

  MFVideoFormat_MP4V: TGUID = (D1: Ord('M') or (Ord('P') shl 8) or (Ord('4') shl 16) or (Ord('V') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_MP4V}

  MFVideoFormat_WMV1: TGUID = (D1: Ord('W') or (Ord('M') shl 8) or (Ord('V') shl 16) or (Ord('1') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_WMV1}

  MFVideoFormat_WMV2: TGUID = (D1: Ord('W') or (Ord('M') shl 8) or (Ord('V') shl 16) or (Ord('2') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_WMV2}

  MFVideoFormat_WMV3: TGUID = (D1: Ord('W') or (Ord('M') shl 8) or (Ord('V') shl 16) or (Ord('3') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_WMV3}

  MFVideoFormat_WVC1: TGUID = (D1: Ord('W') or (Ord('V') shl 8) or (Ord('C') shl 16) or (Ord('1') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_WVC1}

  MFVideoFormat_MSS1: TGUID = (D1: Ord('M') or (Ord('S') shl 8) or (Ord('S') shl 16) or (Ord('1') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_MSS1}

  MFVideoFormat_MSS2: TGUID = (D1: Ord('M') or (Ord('S') shl 8) or (Ord('S') shl 16) or (Ord('2') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_MSS2}

  MFVideoFormat_MPG1: TGUID = (D1: Ord('M') or (Ord('P') shl 8) or (Ord('G') shl 16) or (Ord('1') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_MPG1}

  MFVideoFormat_DVSL: TGUID = (D1: Ord('d') or (Ord('v') shl 8) or (Ord('s') shl 16) or (Ord('l') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_DVSL}

  MFVideoFormat_DVSD: TGUID = (D1: Ord('d') or (Ord('v') shl 8) or (Ord('s') shl 16) or (Ord('d') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_DVSD}

  MFVideoFormat_DVHD: TGUID = (D1: Ord('d') or (Ord('v') shl 8) or (Ord('h') shl 16) or (Ord('d') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_DVHD}

  MFVideoFormat_DV25: TGUID = (D1: Ord('d') or (Ord('v') shl 8) or (Ord('2') shl 16) or (Ord('5') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_DV25}

  MFVideoFormat_DV50: TGUID = (D1: Ord('d') or (Ord('v') shl 8) or (Ord('5') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_DV50}

  MFVideoFormat_DVH1: TGUID = (D1: Ord('d') or (Ord('v') shl 8) or (Ord('h') shl 16) or (Ord('1') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_DVH1}

  MFVideoFormat_DVC: TGUID = (D1: Ord('d') or (Ord('v') shl 8) or (Ord('c') shl 16) or (Ord(' ') shl 24);
                              D2: $0000;
                              D3: $0010;
                              D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
   {$EXTERNALSYM MFVideoFormat_DVC}

  MFVideoFormat_H264: TGUID = (D1: Ord('H') or (Ord('2') shl 8) or (Ord('6') shl 16) or (Ord('4') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
   {$EXTERNALSYM MFVideoFormat_H264}


  // Assume MFVideoFormat_H264 is frame aligned.
  // That is, each input sample has one complete compressed frame (one frame picture,
  // two field pictures or a single unpaired field picture)

  MFVideoFormat_H265: TGUID = (D1: Ord('H') or (Ord('2') shl 8) or (Ord('6') shl 16) or (Ord('5') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_H265}

  MFVideoFormat_MJPG: TGUID = (D1: Ord('M') or (Ord('J') shl 8) or (Ord('P') shl 16) or (Ord('G') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_MJPG}

  MFVideoFormat_420O: TGUID = (D1: Ord('4') or (Ord('2') shl 8) or (Ord('0') shl 16) or (Ord('O') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_420O}

  MFVideoFormat_HEVC: TGUID = (D1: Ord('H') or (Ord('E') shl 8) or (Ord('V') shl 16) or (Ord('C') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_HEVC}

  MFVideoFormat_HEVC_ES: TGUID = (D1: Ord('H') or (Ord('E') shl 8) or (Ord('V') shl 16) or (Ord('S') shl 24);
                                  D2: $0000;
                                  D3: $0010;
                                  D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
   {$EXTERNALSYM MFVideoFormat_HEVC_ES}

  MFVideoFormat_VP80: TGUID = (D1: Ord('V') or (Ord('P') shl 8) or (Ord('8') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_VP80}

  MFVideoFormat_VP90: TGUID = (D1: Ord('V') or (Ord('P') shl 8) or (Ord('9') shl 16) or (Ord('0') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_VP90}

  MFVideoFormat_ORAW: TGUID = (D1: Ord('O') or (Ord('R') shl 8) or (Ord('A') shl 16) or (Ord('W') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_ORAW}

  // #if (WINVER >= _WIN32_WINNT_WIN8)
  MFVideoFormat_H263: TGUID = (D1: Ord('H') or (Ord('2') shl 8) or (Ord('6') shl 16) or (Ord('3') shl 24);
                               D2: $0000;
                               D3: $0010;
                               D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_H263}
  // #endif // (WINVER >= _WIN32_WINNT_WIN8)


  // TODO: switch to RS define once it exists (see: 5312604)
  //#if (WINVER >= _WIN32_WINNT_WIN10)
  MFVideoFormat_A2R10G10B10: TGUID = (D1: Ord('3') or (Ord('1') shl 8) or (Ord(' ') shl 16) or (Ord(' ') shl 24); // = D3DFMT_A2B10G10R10 = 31
                                      D2: $0000;
                                      D3: $0010;
                                      D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_A2R10G10B10}

  MFVideoFormat_A16B16G16R16F: TGUID = (D1: Ord('1') or (Ord('1') shl 8) or (Ord('3') shl 16) or (Ord(' ') shl 24);  // = D3DFMT_A16B16G16R16F = 113
                                        D2: $0000;
                                        D3: $0010;
                                        D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFVideoFormat_A16B16G16R16F}


  //
  // MFSample Perception Date Type-specific attribute GUIDs should be in sync with KSCameraProfileSensorType
  //

type
  PMFFrameSourceTypes = ^_MFFrameSourceTypes;
  _MFFrameSourceTypes = UINT32;
  {$EXTERNALSYM _MFFrameSourceTypes}
  MFFrameSourceTypes = _MFFrameSourceTypes;
  {$EXTERNALSYM MFFrameSourceTypes}
const
  MFFrameSourceTypes_Color    = MFFrameSourceTypes($0001);
  {$EXTERNALSYM MFFrameSourceTypes_Color}
  MFFrameSourceTypes_Infrared = MFFrameSourceTypes($0002);
  {$EXTERNALSYM MFFrameSourceTypes_Infrared}
  MFFrameSourceTypes_Depth    = MFFrameSourceTypes($0004);
  {$EXTERNALSYM MFFrameSourceTypes_Depth}
  MFFrameSourceTypes_Image    = MFFrameSourceTypes($0008);
  {$EXTERNALSYM MFFrameSourceTypes_Image}
  MFFrameSourceTypes_Custom   = MFFrameSourceTypes($0080);
  {$EXTERNALSYM MFFrameSourceTypes_Custom}

//#endif // (WINVER > _WIN32_WINNT_WIN10)

//
// undef the local D3DFMT definitions to avoid later clashes with D3D headers
//
{$IFDEF LOCAL_D3DFMT_DEFINES}
  {$UNDEF D3DFMT_R8G8B8}
  {$UNDEF D3DFMT_A8R8G8B8}
  {$UNDEF D3DFMT_X8R8G8B8}
  {$UNDEF D3DFMT_R5G6B5}
  {$UNDEF D3DFMT_X1R5G5B5}
  {$UNDEF D3DFMT_P8}
  {$UNDEF D3DFMT_A2B10G10R10}
  {$UNDEF D3DFMT_A16B16G16R16F}
  {$UNDEF D3DFMT_L8}
  {$UNDEF D3DFMT_D16}
  {$UNDEF D3DFMT_L16}
  {$UNDEF LOCAL_D3DFMT_DEFINES}
{$ENDIF}

const
  // assume MFVideoFormat_H264_ES may not be frame aligned. that is, each input sample may have one partial frame,
  // multiple frames, some frames plus some partial frame
  // or more general, N.M frames, N is the integer part and M is the fractional part.
  //
  // {3F40F4F0-5622-4FF8-B6D8-A17A584BEE5E}       MFVideoFormat_H264_ES
  MFVideoFormat_H264_ES  :  TGUID = '{3f40f4f0-5622-4ff8-b6d8-a17a584bee5e}';
  {$EXTERNALSYM MFVideoFormat_H264_ES}


  // some legacy formats that don't fit the common pattern ///////////////////////

  // {e06d8026-db46-11cf-b4d1-00805f6cbbea}       MFVideoFormat_MPEG2
  MFVideoFormat_MPEG2   : TGuid = '{e06d8026-db46-11cf-b4d1-00805f6cbbea}';
  {$EXTERNALSYM MFVideoFormat_MPEG2}
  MFVideoFormat_MPG2    : TGuid = '{e06d8026-db46-11cf-b4d1-00805f6cbbea}'; // = MFVideoFormat_MPEG2
  {$EXTERNALSYM MFVideoFormat_MPG2}



  // audio media types
  //==================

  MFAudioFormat_PCM             : TGUID = (D1: WAVE_FORMAT_PCM;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_PCM}

  MFAudioFormat_Float           : TGUID = (D1: WAVE_FORMAT_IEEE_FLOAT;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_Float}


  // MFAudioFormat_DTS is for S/PDIF-encapsulated DTS core streams. It is the same as KSDATAFORMAT_SUBTYPE_IEC61937_DTS in ksmedia.h.
  // Use MEDIASUBTYPE_DTS2 (defined in wmcodecdsp.h) for raw DTS core streams.
  // MFAudioFormat_DTS_RAW (same as MEDIASUBTYPE_DTS) can also be used for raw DTS core streams. While the values for MEDIASUBTYPE_DTS and
  // MEDIASUBTYPE_DTS2 are different, the stream type is the same.
  //
  // If DTS extension substreams may be present, use MFAudioFormat_DTS_HD (same as MEDIASUBTYPE_DTS_HD) for Master Audio,
  // and MEDIASUBTYPE_DTS_HD_HRA for High Resolution Audio and other extension substream variants.
  // (KSDATAFORMAT_SUBTYPE_IEC61937_DTS_HD is the S/PDIF media subtype for MEDIASUBTYPE_DTS_HD and MEDIASUBTYPE_DTS_HD_HRA.)

  MFAudioFormat_DTS             : TGUID = (D1: WAVE_FORMAT_DTS;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_DTS}

  // MFAudioFormat_Dolby_AC3_SPDIF is for S/PDIF-encapsulated AC-3.
  // It is the same as KSDATAFORMAT_SUBTYPE_IEC61937_DOLBY_DIGITAL in ksmedia.h.
  // Use MFAudioFormat_Dolby_AC3 (MEDIASUBTYPE_DOLBY_AC3 in wmcodecdsp.h) for raw AC-3 streams.
  MFAudioFormat_Dolby_AC3_SPDIF : TGUID = (D1: WAVE_FORMAT_DOLBY_AC3_SPDIF;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_Dolby_AC3_SPDIF}

  MFAudioFormat_DRM             : TGUID = (D1: WAVE_FORMAT_DRM;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_DRM}

  MFAudioFormat_WMAudioV8       : TGUID = (D1: WAVE_FORMAT_WMAUDIO2;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_WMAudioV8}

  MFAudioFormat_WMAudioV9       : TGUID = (D1: WAVE_FORMAT_WMAUDIO3;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_WMAudioV9}

  MFAudioFormat_WMAudio_Lossless: TGUID = (D1: WAVE_FORMAT_WMAUDIO_LOSSLESS;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_WMAudio_Lossless}

  MFAudioFormat_WMASPDIF        : TGUID = (D1: WAVE_FORMAT_WMASPDIF;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_WMASPDIF}

  MFAudioFormat_MSP1            : TGUID = (D1: WAVE_FORMAT_WMAVOICE9;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_MSP1}

  MFAudioFormat_MP3             : TGUID = (D1: WAVE_FORMAT_MPEGLAYER3;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_MP3}

  MFAudioFormat_MPEG            : TGUID = (D1: WAVE_FORMAT_MPEG;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_MPEG}

  MFAudioFormat_AAC             : TGUID = (D1: WAVE_FORMAT_MPEG_HEAAC;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_AAC}

  MFAudioFormat_ADTS            : TGUID = (D1: WAVE_FORMAT_MPEG_ADTS_AAC;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_ADTS}

  MFAudioFormat_AMR_NB          : TGUID = (D1: WAVE_FORMAT_AMR_NB;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_AMR_NB}

  MFAudioFormat_AMR_WB          : TGUID = (D1: WAVE_FORMAT_AMR_WB;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_AMR_WB}
  MFAudioFormat_AMR_WP          : TGUID = (D1: WAVE_FORMAT_AMR_WP;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_AMR_WP}

//  #if (WINVER >= _WIN32_WINNT_THRESHOLD)

  MFAudioFormat_FLAC            : TGUID = (D1: WAVE_FORMAT_FLAC;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_FLAC}

  MFAudioFormat_ALAC            : TGUID = (D1: WAVE_FORMAT_ALAC;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_ALAC}

  MFAudioFormat_Opus            : TGUID = (D1: WAVE_FORMAT_OPUS;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));
  {$EXTERNALSYM MFAudioFormat_Opus}
//  #endif

  MFAudioFormat_Dolby_AC4       : TGUID = (D1: WAVE_FORMAT_DOLBY_AC4;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));

  MFAudioFormat_Dolby_AC4_IMS   : TGUID = (D1: WAVE_FORMAT_DOLBY_AC4_IMS;
                                           D2: $0000;
                                           D3: $0010;
                                           D4: ($80, $00, $00, $AA, $00, $38, $9B, $71));



  // These audio types are not derived from an existing wFormatTag
  MFAudioFormat_Dolby_AC3       :  TGUID = '{e06d802c-db46-11cf-b4d1-08005f6cbbea}';
  {$EXTERNALSYM MFAudioFormat_Dolby_AC3}
  // == MEDIASUBTYPE_DOLBY_AC3 defined in ksuuids.h

  MFAudioFormat_Dolby_DDPlus    :  TGUID = '{a7fb87af-2d02-42fb-a4d4-05cd93843bdd}';
  {$EXTERNALSYM MFAudioFormat_Dolby_DDPlus}
  // == MEDIASUBTYPE_DOLBY_DDPLUS defined in wmcodecdsp.h

  MFAudioFormat_Vorbis          :  TGUID = '{8D2FD10B-5841-4a6b-8905-588FEC1ADED9}';
  {$EXTERNALSYM MFAudioFormat_Vorbis}
  // {8D2FD10B-5841-4a6b-8905-588FEC1ADED9}

  MFAudioFormat_DTS_RAW         :  TGUID = '{E06D8033-DB46-11CF-B4D1-00805F6CBBEA}';
  {$EXTERNALSYM MFAudioFormat_DTS_RAW}
  // == MEDIASUBTYPE_DTS defined in ksuuids.h

  MFAudioFormat_DTS_HD          :  TGUID = '{A2E58EB7-0FA9-48BB-A40C-FA0E156D0645}';
  {$EXTERNALSYM MFAudioFormat_DTS_HD}
  // == MEDIASUBTYPE_DTS_HD defined in wmcodecdsp.h

  MFAudioFormat_DTS_XLL         :  TGUID = '{45B37C1B-8C70-4E59-A7BE-A1E42C81C80D}';
  {$EXTERNALSYM MFAudioFormat_DTS_XLL}
  // {45B37C1B-8C70-4E59-A7BE-A1E42C81C80D}

  MFAudioFormat_DTS_LBR         :  TGUID = '{C2FE6F0A-4E3C-4DF1-9B60-50863091E4B9}';
  {$EXTERNALSYM MFAudioFormat_DTS_LBR}
  // {C2FE6F0A-4E3C-4DF1-9B60-50863091E4B9}

  MFAudioFormat_DTS_UHD         :  TGUID = '{87020117-ACE3-42DE-B73E-C656706263F8}';
  {$EXTERNALSYM MFAudioFormat_DTS_UHD}
  // {87020117-ACE3-42DE-B73E-C656706263F8}

  MFAudioFormat_DTS_UHDY        :  TGUID = '{9B9CCA00-91B9-4CCC-883A-8F787AC3CC86}';
  {$EXTERNALSYM MFAudioFormat_DTS_UHDY}
  // {9B9CCA00-91B9-4CCC-883A-8F787AC3CC86}



//#if (NTDDI_VERSION >= NTDDI_WIN10_RS2)
  MFAudioFormat_Float_SpatialObjects  : TGUID =  '{fa39cd94-bc64-4ab1-9b71-dcd09d5a7e7a}';
  {$EXTERNALSYM MFAudioFormat_Float_SpatialObjects}
//#endif // (NTDDI_VERSION >= NTDDI_WIN10_RS2)

//#if (WINVER >= _WIN32_WINNT_THRESHOLD)
  // LPCM audio with headers for encapsulation in an MPEG2 bitstream
  MFAudioFormat_LPCM            :  TGUID = '{e06d8032-db46-11cf-b4d1-00805f6cbbea}';
  {$EXTERNALSYM MFAudioFormat_LPCM}
  // == MEDIASUBTYPE_LPCM defined in ksmedia.h

  MFAudioFormat_PCM_HDCP        :  TGUID = '{a5e7ff01-8411-4acc-a865-5f4941288d80}';
  {$EXTERNALSYM MFAudioFormat_PCM_HDCP}
  MFAudioFormat_Dolby_AC3_HDCP  :  TGUID = '{97663a80-8ffb-4445-a6ba-792d908f497f}';
  {$EXTERNALSYM MFAudioFormat_Dolby_AC3_HDCP}
  MFAudioFormat_AAC_HDCP        :  TGUID = '{419bce76-8b72-400f-adeb-84b57d63484d}';
  {$EXTERNALSYM MFAudioFormat_AAC_HDCP}
  MFAudioFormat_ADTS_HDCP       :  TGUID = '{da4963a3-14d8-4dcf-92b7-193eb84363db}';
  {$EXTERNALSYM MFAudioFormat_ADTS_HDCP}
  MFAudioFormat_Base_HDCP       :  TGUID = '{3884b5bc-e277-43fd-983d-038aa8d9b605}';
  {$EXTERNALSYM MFAudioFormat_Base_HDCP}

  MFVideoFormat_H264_HDCP       :  TGUID = '{5d0ce9dd-9817-49da-bdfd-f5f5b98f18a6}';
  {$EXTERNALSYM MFVideoFormat_H264_HDCP}
  MFVideoFormat_HEVC_HDCP       :	 TGUID = '{3cfe0fe6-05c4-47dc-9d70-4bdb2959720f}';
  {$EXTERNALSYM MFVideoFormat_HEVC_HDCP}
  MFVideoFormat_Base_HDCP       :  TGUID = '{eac3b9d5-bd14-4237-8f1f-bab428e49312}';
  {$EXTERNALSYM MFVideoFormat_Base_HDCP}

//#endif  // _WIN32_WINNT_THRESHOLD


  // MPEG-4 media types
  //===================

  // {00000000-767a-494d-b478-f29d25dc9037}   MFMPEG4Format_Base
  MFMPEG4Format_Base              : TGuid = '{00000000-767a-494d-b478-f29d25dc9037}';
  {$EXTERNALSYM MFMPEG4Format_Base}



  // Subtitle media types
  //=====================
  MFSubtitleFormat_XML            : TGUID = '{2006f94f-29ca-4195-b8db-00ded8ff0c97}';
  {$EXTERNALSYM MFSubtitleFormat_XML}
  MFSubtitleFormat_TTML           : TGUID = '{73e73992-9a10-4356-9557-7194e91e3e54}';
  {$EXTERNALSYM MFSubtitleFormat_TTML}
  MFSubtitleFormat_ATSC           : TGUID = '{7fa7faa3-feae-4e16-aedf-36b9acfbb099}';
  {$EXTERNALSYM MFSubtitleFormat_ATSC}
  MFSubtitleFormat_WebVTT         : TGUID = '{c886d215-f485-40bb-8db6-fadbc619a45d}';
  {$EXTERNALSYM MFSubtitleFormat_WebVTT}
  MFSubtitleFormat_SRT            : TGUID = '{5e467f2e-77ca-4ca5-8391-d142ed4b76c8}';
  {$EXTERNALSYM MFSubtitleFormat_SRT}
  MFSubtitleFormat_SSA            : TGUID = '{57176a1b-1a9e-4eea-abef-c61760198ac4}';
  {$EXTERNALSYM MFSubtitleFormat_SSA}
  MFSubtitleFormat_CustomUserData : TGUID = '{1bb3d849-6614-4d80-8882-ed24aa82da92}';
  {$EXTERNALSYM MFSubtitleFormat_CustomUserData}

//#if (NTDDI_VERSION >= NTDDI_WIN10_VB)
  // {71F40E4A-1278-4442-B30D-39DD1D7722BC}      MFSubtitleFormat_PGS
  MFSubtitleFormat_PGS            : TGUID = '{71F40E4A-1278-4442-B30D-39DD1D7722BC}';
  {$EXTERNALSYM MFSubtitleFormat_PGS}
  // {6B8E40F4-8D2C-4CED-AD91-5960E45B4433}      MFSubtitleFormat_VobSub
  FSubtitleFormat_VobSub          : TGUID = '{6B8E40F4-8D2C-4CED-AD91-5960E45B4433}';
  {$EXTERNALSYM FSubtitleFormat_VobSub}
//#endif // (NTDDI_VERSION >= NTDDI_WIN10_VB)


  //
  // Binary Data MediaTypes
  //

  //#ifndef DEFINE_BINARY_MEDIATYPMF_DEVICESTREAM_ATTRIBUTE_SECURE_CAPABILITYE_GUID
  //#define DEFINE_BINARY_MEDIATYPE_GUID(name, format) \
  //    DEFINE_GUID(name,                       \
  //    format, 0xbf10, 0x48b4, 0xbc, 0x18, 0x59, 0x3d, 0xc1, 0xdb, 0x95, 0xf);
  //#endif

  //DEFINE_BINARY_MEDIATYPE_GUID(MFBinaryFormat_Base, 0x00000000);
  //DEFINE_BINARY_MEDIATYPE_GUID(MFBinaryFormat_GPMD, 'gpmd');


//////////////////////  Media Type Attributes GUIDs ////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// GUIDs for IMFMediaType properties - prefix 'MF_MT_' - basic prop type in {},
// with type to cast to in ().



  // Core info for all types
  //========================

const
  // {48eba18e-f8c9-4687-bf11-0a74c9f96a8f}   MF_MT_MAJOR_TYPE                {GUID}
  MF_MT_MAJOR_TYPE                            : TGuid = '{48eba18e-f8c9-4687-bf11-0a74c9f96a8f}';
  {$EXTERNALSYM MF_MT_MAJOR_TYPE}

  // {f7e34c9a-42e8-4714-b74b-cb29d72c35e5}   MF_MT_SUBTYPE                   {GUID}
  MF_MT_SUBTYPE                               : TGuid = '{f7e34c9a-42e8-4714-b74b-cb29d72c35e5}';
  {$EXTERNALSYM MF_MT_SUBTYPE}

  // {c9173739-5e56-461c-b713-46fb995cb95f}   MF_MT_ALL_SAMPLES_INDEPENDENT   {UINT32 (BOOL)}
  MF_MT_ALL_SAMPLES_INDEPENDENT               : TGuid = '{c9173739-5e56-461c-b713-46fb995cb95f}';
  {$EXTERNALSYM MF_MT_ALL_SAMPLES_INDEPENDENT}

  // {b8ebefaf-b718-4e04-b0a9-116775e3321b}     MF_MT_FIXED_SIZE_SAMPLES        {UINT32 (BOOL)}
  MF_MT_FIXED_SIZE_SAMPLES                    : TGuid = '{b8ebefaf-b718-4e04-b0a9-116775e3321b}';
  {$EXTERNALSYM MF_MT_FIXED_SIZE_SAMPLES}

  // {3afd0cee-18f2-4ba5-a110-8bea502e1f92}     MF_MT_COMPRESSED                {UINT32 (BOOL)}
  MF_MT_COMPRESSED                            : TGuid = '{3afd0cee-18f2-4ba5-a110-8bea502e1f92}';
  {$EXTERNALSYM MF_MT_COMPRESSED}

  // MF_MT_SAMPLE_SIZE is only valid if MF_MT_FIXED_SIZED_SAMPLES is TRUE
  // {dad3ab78-1990-408b-bce2-eba673dacc10}     MF_MT_SAMPLE_SIZE               {UINT32}
  MF_MT_SAMPLE_SIZE                           : TGuid = '{dad3ab78-1990-408b-bce2-eba673dacc10}';
  {$EXTERNALSYM MF_MT_SAMPLE_SIZE}

  // 4d3f7b23-d02f-4e6c-9bee-e4bf2c6c695d       MF_MT_WRAPPED_TYPE              {Blob}
  MF_MT_WRAPPED_TYPE                          : TGuid = '{4d3f7b23-d02f-4e6c-9bee-e4bf2c6c695d}';
  {$EXTERNALSYM MF_MT_WRAPPED_TYPE}

// #if (WINVER >= _WIN32_WINNT_WIN8)


  // Media Type & Sample attributes for 3D Video
  //============================================

  // {CB5E88CF-7B5B-476b-85AA-1CA5AE187555}        MF_MT_VIDEO_3D
  // UINT32 (BOOL)
  MF_MT_VIDEO_3D: TGUID = '{cb5e88cf-7b5b-476b-85aa-1ca5ae187555}';
  {$EXTERNALSYM MF_MT_VIDEO_3D}

type
  // Enum describing the packing for 3D video frames
  PMFVideo3DFormat = ^MFVideo3DFormat;
  _MFVideo3DFormat                         = (
    MFVideo3DSampleFormat_BaseView         = 0,
    MFVideo3DSampleFormat_MultiView        = 1,
    MFVideo3DSampleFormat_Packed_LeftRight = 2,
    MFVideo3DSampleFormat_Packed_TopBottom = 3
  );
  {$EXTERNALSYM _MFVideo3DFormat}
  MFVideo3DFormat = _MFVideo3DFormat;
  {$EXTERNALSYM MFVideo3DFormat}

const
  // UINT32 (anyof MFVideo3DFormat)
  MF_MT_VIDEO_3D_FORMAT         : TGUID = '{5315d8a0-87c5-4697-b793-6606c67c049b}';
  {$EXTERNALSYM MF_MT_VIDEO_3D_FORMAT}
  // UINT32
  MF_MT_VIDEO_3D_NUM_VIEWS      : TGUID = '{bb077e8a-dcbf-42eb-af60-418df98aa495}';
  {$EXTERNALSYM MF_MT_VIDEO_3D_NUM_VIEWS}
  // UINT32
  MF_MT_VIDEO_3D_LEFT_IS_BASE   : TGUID = '{6d4b7bff-5629-4404-948c-c634f4ce26d4}';
  {$EXTERNALSYM MF_MT_VIDEO_3D_LEFT_IS_BASE}
  // UINT32 (BOOL)
  MF_MT_VIDEO_3D_FIRST_IS_LEFT  : TGUID = '{ec298493-0ada-4ea1-a4fe-cbbd36ce9331}';
  {$EXTERNALSYM MF_MT_VIDEO_3D_FIRST_IS_LEFT}

  // MFSampleExtension_3DVideo                    {F86F97A4-DD54-4e2e-9A5E-55FC2D74A005}
  // Type: UINT32
  // If present and nonzero, indicates that the sample contains 3D Video data
  MFSampleExtension_3DVideo     : TGUID = '{f86f97a4-dd54-4e2e-9a5e-55fc2d74a005}';
  {$EXTERNALSYM MFSampleExtension_3DVideo}

type
  // Enum describing the packing for 3D video frames in a sample
  PMFVideo3DSampleFormat = ^MFVideo3DSampleFormat;
  _MFVideo3DSampleFormat                = (
    MFSampleExtension_3DVideo_MultiView = 1,
    MFSampleExtension_3DVideo_Packed    = 0
  );
  {$EXTERNALSYM _MFVideo3DSampleFormat}
  MFVideo3DSampleFormat = _MFVideo3DSampleFormat;
  {$EXTERNALSYM MFVideo3DSampleFormat}

const

  MFSampleExtension_3DVideo_SampleFormat  :  TGUID = '{08671772-e36f-4cff-97b3-d72e20987a48}';
  {$EXTERNALSYM MFSampleExtension_3DVideo_SampleFormat}
  // MFSampleExtension_3DVideo_SampleFormat       {08671772-E36F-4cff-97B3-D72E20987A48}
  // Type: UINT32
  // The value of this attribute is a member of the MFVideo3DSampleFormat enumeration.
  // MFVideo3DSampleFormat enumeration identifies how 3D views are stored in the sample
  //      - in a packed representation, all views are stored in a single buffer
  //      - in a multiview representation, each view is stored in its own buffer

type
  // Describes the rotation of the video image in the counter-clockwise direction.
  PMFVideoRotationFormat = ^_MFVideoRotationFormat;
  _MFVideoRotationFormat      = (
    MFVideoRotationFormat_0   = 0,
    MFVideoRotationFormat_90  = 90,
    MFVideoRotationFormat_180 = 180,
    MFVideoRotationFormat_270 = 270
  );
  {$EXTERNALSYM _MFVideoRotationFormat}
  MFVideoRotationFormat = _MFVideoRotationFormat;
  {$EXTERNALSYM MFVideoRotationFormat}
  // Enum describing the video rotation formats
  // Only the values of 0, 90, 180, and 270 are valid.
  // This enumeration is used with the MF_MT_VIDEO_ROTATION attribute.

const

  MF_MT_VIDEO_ROTATION  :  TGUID = '{c380465d-2271-428c-9b83-ecea3b4a85c1}';
  {$EXTERNALSYM MF_MT_VIDEO_ROTATION}
  // MF_MT_VIDEO_ROTATION      {C380465D-2271-428C-9B83-ECEA3B4A85C1}
  // Type: UINT32
  // Description: MF_MT_VIDEO_ROTATION attribute means the degree that the content
  // has already been rotated in the counter clockwise direction.
  // Currently, only the values of 0, 90, 180, and 270 are valid for MF_MT_VIDEO_ROTATION.
  // For convenience, these currently supported values are enumerated in MFVideoRotationFormat.
  // Example: if the media type has MF_MT_VIDEO_ROTATION set as MFVideoRotationFormat_90,
  // it means the content has been rotated 90 degree in the counter clockwise direction.
  // If the content was actually rotated 90 degree in the clockwise direction, 90 degree in
  // clockwise should be converted into 270 degree in the counter clockwise direction and set
  // the attribute MF_MT_VIDEO_ROTATION as MFVideoRotationFormat_270 accordingly.

//#if (WINVER >= _WIN32_WINNT_WIN10_RS2)
  MF_DEVICESTREAM_MULTIPLEXED_MANAGER   : TGUID =  '{6ea542b0-281f-4231-a464-fe2f5022501c}';
  {$EXTERNALSYM MF_DEVICESTREAM_MULTIPLEXED_MANAGER}
  MF_MEDIATYPE_MULTIPLEXED_MANAGER      : TGUID =  '{13c78fb5-f275-4ea0-bb5f-0249832b0d6e}';
  {$EXTERNALSYM MF_MEDIATYPE_MULTIPLEXED_MANAGER}
  MFSampleExtension_MULTIPLEXED_MANAGER : TGUID =  '{8dcdee79-6b5a-4c45-8db9-20b395f02fcf}';
  {$EXTERNALSYM MFSampleExtension_MULTIPLEXED_MANAGER}



  function MFCreateMuxStreamAttributes(pAttributesToMux: IMFCollection;
                                       out ppMuxAttribs: PIMFAttributes): HResult; stdcall;
  {$EXTERNALSYM MFCreateMuxStreamAttributes}

  function MFCreateMuxStreamMediaType(pMediaTypesToMux: IMFCollection;
                                      out ppMuxMediaType: PIMFMediaType): HResult; stdcall;
  {$EXTERNALSYM MFCreateMuxStreamMediaType}

  function MFCreateMuxStreamSample(pSamplesToMux: IMFCollection;
                                   out ppMuxSample: IMFSample): HResult; stdcall;
  {$EXTERNALSYM MFCreateMuxStreamSample}

//#endif


const

// #if (WINVER >= _WIN32_WINNT_WINTHRESHOLD) Win 10

  MF_MT_SECURE                  :  TGUID = '{c5acc4fd-0304-4ecf-809f-47bc97ff63bd}';
  {$EXTERNALSYM MF_MT_SECURE}
  // MF_MT_SECURE     {c5acc4fd-0304-4ecf-809f-47bc97ff63bd }
  // Type: UINT32 (BOOL)
  // Description: MF_MT_SECURE attribute indicates that the content will be using
  // secure D3D surfaces.  These surfaces can only be accessed by trusted hardware.


  // MF_DEVICESTREAM_ATTRIBUTE_FRAMESOURCE_TYPES {17145FD1-1B2B-423C-8001-2B6833ED3588}
  // Type: UINT32 (enum type defined in MFFrameSourceTypes)
  // Description: The value of this attribute is a enum value, describing the sensor types.
  // For backward compatibility, when this attribute was not defined on in a media type, it is assumed to be MFFrameSourceTypes.Color.
  MF_DEVICESTREAM_ATTRIBUTE_FRAMESOURCE_TYPES :	TGUID = '{17145fd1-1b2b-423c-8001-2b6833ed3588}';
  {$EXTERNALSYM MF_DEVICESTREAM_ATTRIBUTE_FRAMESOURCE_TYPES}

  // MF_MT_ALPHA_MODE {5D959B0D-4CBF-4D04-919F-3F5F7F284211}
  // Type: UINT32
  // Description: To differentiate the usage of alpha channel in such video formats, a new attribute MF_MT_ALPHA_MODE is designed to describe this information.
  // The value of this attribute can be cast to DXGI_ALPHA_MODE.
  // If this attribute is not present, for backward compatibility, the value is DXGI_ALPHA_MODE_STRAIGHT for video format supporting alpha channel,
  // such as ARGB32, or DXGI_ALPHA_MODE_IGNORE for video format without alpha channel, such as RGB32.
  MF_MT_ALPHA_MODE :	TGUID = '{5D959B0D-4CBF-4D04-919F-3F5F7F284211}';
  {$EXTERNALSYM MF_MT_ALPHA_MODE}


type

  PMFDepthMeasurement = ^MFDepthMeasurement;
  _MFDepthMeasurement       = (
    DistanceToFocalPlane    = 0,
    DistanceToOpticalCenter = 1
  );
  {$EXTERNALSYM _MFDepthMeasurement}
  MFDepthMeasurement = _MFDepthMeasurement;
  {$EXTERNALSYM MFDepthMeasurement}


const
  // MF_MT_DEPTH_MEASUREMENT {FD5AC489-0917-4BB6-9D54-3122BF70144B}
  // Type : UINT32  (MFDepthMeasurement)
  // Description: If this attribute is not present, by default it is DistanceToFocalPlane, illustrated by following diagram.
  MF_MT_DEPTH_MEASUREMENT :	TGUID = '{FD5AC489-0917-4BB6-9D54-3122BF70144B}';
  {$EXTERNALSYM MF_MT_DEPTH_MEASUREMENT}

  // MF_MT_DEPTH_VALUE_UNIT    {21a800f5-3189-4797-beba-f13cd9a31a5e}
  // Type : UINT64
  // Description: MF_MT_DEPTH_VALUE_UNIT attribute indicates scale of the depth value in nanometers.
  // For each pixel in depth frame, the actual depth measured in nanometers is the pixel value multiplied by this attribute.
  MF_MT_DEPTH_VALUE_UNIT :	TGUID = '{21a800f5-3189-4797-beba-f13cd9a31a5e}';
  {$EXTERNALSYM MF_MT_DEPTH_VALUE_UNIT}

// #endif  // Win 10



  MF_MT_VIDEO_NO_FRAME_ORDERING :  TGUID = '{3f5b106f-6bc2-4ee3-b7ed-8902c18f5351}';
  {$EXTERNALSYM MF_MT_VIDEO_NO_FRAME_ORDERING}
  // MF_MT_VIDEO_NO_FRAME_ORDERING {3F5B106F-6BC2-4EE3-B7ED-892C18F5351}
  // Type: UINT32
  // Description: MF_MT_VIDEO_NO_FRAME_ORDERING set to non-zero (true) means external users/apps know
  // that input video bitstream has no frame rerodering,
  // that is, the output and display order is the same as the input and decoding order
  // it will overwrite bitstream syntaxes even if bitstream syntaxes do not indicate
  // that the output and display order is the same as the input and decoding order
  //
  // it is an attribute set on input media type

  MF_MT_VIDEO_H264_NO_FMOASO   :  TGUID = '{ed461cd6-ec9f-416a-a8a3-26d7d31018d7}';
  {$EXTERNALSYM MF_MT_VIDEO_H264_NO_FMOASO}

// #endif // (WINVER >= _WIN32_WINNT_WIN8)

// TODO: switch to NTDDI_WIN10_RS3 when _NT_TARGET_VERSION is updated to support RS3
//#if (NTDDI_VERSION >= NTDDI_WIN10_RS2)

//
// Renderer Extensions
//

// MFSampleExtension_ForwardedDecodeUnits {424C754C-97C8-48d6-8777-FC41F7B60879}
// Type: IUnknown
// This is an object of type IMFCollection containing IMFSample objects
//  which contain NALU/SEI forwarded by a decoder.
//  Contains all custom NALU/SEI since previous frame with emulation prevention bytes removed.
// see: MF_MT_FORWARD_CUSTOM_NALU, MF_MT_FORWARD_CUSTOM_SEI
  MFSampleExtension_ForwardedDecodeUnits : TGUID =  '{424C754C-97C8-48d6-8777-FC41F7B60879}';
  {$EXTERNALSYM MFSampleExtension_ForwardedDecodeUnits}


// MFSampleExtension_TargetGlobalLuminance {3F60EF36-31EF-4daf-8360-940397E41EF3}
// Type: UINT32
// Value in Nits that specifies the targeted global backlight luminance for
//  the associated video frame.
  MFSampleExtension_TargetGlobalLuminance : TGUID =  '{3F60EF36-31EF-4daf-8360-940397E41EF3}';
  {$EXTERNALSYM MFSampleExtension_TargetGlobalLuminance}

type

  PMfCustomDecodeUnitType = ^MF_CUSTOM_DECODE_UNIT_TYPE;
  _MF_CUSTOM_DECODE_UNIT_TYPE = (
    MF_DECODE_UNIT_NAL = 0,
    MF_DECODE_UNIT_SEI = 1);
  {$EXTERNALSYM _MF_CUSTOM_DECODE_UNIT_TYPE}
  MF_CUSTOM_DECODE_UNIT_TYPE = _MF_CUSTOM_DECODE_UNIT_TYPE;
  {$EXTERNALSYM MF_CUSTOM_DECODE_UNIT_TYPE}

const

// MFSampleExtension_ForwardedDecodeUnitType {089E57C7-47D3-4a26-BF9C-4B64FAFB5D1E}
// Type: UINT32 (oneof MF_CUSTOM_DECODE_UNIT_TYPE)
// Attached to IMFSample objects in MFSampleExtension_ForwardedDecodeUnits, specifies
//  what type of unit is attached: SEI or NAL
  MFSampleExtension_ForwardedDecodeUnitType : TGUID =  '{089E57C7-47D3-4a26-BF9C-4B64FAFB5D1E}';
  {$EXTERNALSYM MFSampleExtension_ForwardedDecodeUnitType}


// MF_MT_FORWARD_CUSTOM_NALU {ED336EFD-244F-428d-9153-28F399458890}
// Type: UINT32
// Specifies the NAL unit type to forward on output samples of the decoder.
// If the decoder parses the specified NALU then it will not forwarded.
// See: MFSampleExtension_ForwardedDecodeUnits
  MF_MT_FORWARD_CUSTOM_NALU : TGUID =  '{ED336EFD-244F-428d-9153-28F399458890}';
  {$EXTERNALSYM MF_MT_FORWARD_CUSTOM_NALU}


// MF_MT_FORWARD_CUSTOM_SEI {E27362F1-B136-41d1-9594-3A7E4FEBF2D1}
// Type: UINT32
// Specifies the SEI type to forward on output samples of the decoder
// If the decoder parses the specified SEI then it will not be forwarded.
// See: MFSampleExtension_ForwardedDecodeUnits
  MF_MT_FORWARD_CUSTOM_SEI : TGUID =  '{E27362F1-B136-41d1-9594-3A7E4FEBF2D1}';
  {$EXTERNALSYM MF_MT_FORWARD_CUSTOM_SEI}


// MF_MT_VIDEO_RENDERER_EXTENSION_PROFILE {8437D4B9-D448-4fcd-9B6B-839BF96C7798}
// Type: LPCWSTR
// Contains a string that matches an entry in a MediaRendererEffect Manifest's
//  VideoRendererExtensionProfiles list to select which effect to load
  MF_MT_VIDEO_RENDERER_EXTENSION_PROFILE : TGUID =  '{8437D4B9-D448-4fcd-9B6B-839BF96C7798}';
  {$EXTERNALSYM MF_MT_VIDEO_RENDERER_EXTENSION_PROFILE}

//#endif // (NTDDI_VERSION >= NTDDI_WIN10_RS2)


//#if (NTDDI_VERSION >= NTDDI_WIN10_RS4)

// MF_DECODER_FWD_CUSTOM_SEI_DECODE_ORDER {f13bbe3c-36d4-410a-b985-7a951a1e6294}
// Type: UINT32
// Specifies that the SEI unit type to forward on output samples of the decoder
// shall be sent out in decode order (i.e. ahead of time)
// This is required for downstream apps to process the SEI in advance of receiving
// the frame it is meant to be attached to
  MF_DECODER_FWD_CUSTOM_SEI_DECODE_ORDER      : TGUID =  '{F13BBE3C-36D4-410A-B985-7A951A1E6294}';
  {$EXTERNALSYM MF_DECODER_FWD_CUSTOM_SEI_DECODE_ORDER}

//#endif /* (NTDDI_VERSION >= NTDDI_WIN10_RS4) */


// #if (NTDDI_VERSION >= NTDDI_WIN10_VB)
  // {C6052A80-6D9C-40a3-9DB8-F027A25C9AB9}
  // Type: String
  // Name of the App Service, as defined in the AppX manifest of the Package that contains this Video
  // Renderer Effect.
  // This attribute is specified by the Video Renderer Effect to request that the platforms establish
  // a communication channel with the Video Renderer Effect's App Service.
  MF_VIDEO_RENDERER_EFFECT_APP_SERVICE_NAME   : TGUID =  '{C6052A80-6D9C-40a3-9DB8-F027A25C9AB9}';
  {$EXTERNALSYM MF_VIDEO_RENDERER_EFFECT_APP_SERVICE_NAME}
// #endif // (NTDDI_VERSION >= NTDDI_WIN10_VB)


  // AUDIO data
  //===========

  // {37e48bf5-645e-4c5b-89de-ada9e29b696a}   MF_MT_AUDIO_NUM_CHANNELS            {UINT32}
  MF_MT_AUDIO_NUM_CHANNELS                    : TGuid = '{37e48bf5-645e-4c5b-89de-ada9e29b696a}';
  {$EXTERNALSYM MF_MT_AUDIO_NUM_CHANNELS}

  // {5faeeae7-0290-4c31-9e8a-c534f68d9dba}   MF_MT_AUDIO_SAMPLES_PER_SECOND      {UINT32}
  MF_MT_AUDIO_SAMPLES_PER_SECOND              : TGuid = '{5faeeae7-0290-4c31-9e8a-c534f68d9dba}';
  {$EXTERNALSYM MF_MT_AUDIO_SAMPLES_PER_SECOND}

  // {fb3b724a-cfb5-4319-aefe-6e42b2406132}   MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND {double}
  MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND        : TGuid = '{fb3b724a-cfb5-4319-aefe-6e42b2406132}';
  {$EXTERNALSYM MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND}

  // {1aab75c8-cfef-451c-ab95-ac034b8e1731}   MF_MT_AUDIO_AVG_BYTES_PER_SECOND    {UINT32}
  MF_MT_AUDIO_AVG_BYTES_PER_SECOND            : TGuid = '{1aab75c8-cfef-451c-ab95-ac034b8e1731}';
  {$EXTERNALSYM MF_MT_AUDIO_AVG_BYTES_PER_SECOND}

  // {322de230-9eeb-43bd-ab7a-ff412251541d}   MF_MT_AUDIO_BLOCK_ALIGNMENT         {UINT32}
  MF_MT_AUDIO_BLOCK_ALIGNMENT                 : TGuid = '{322de230-9eeb-43bd-ab7a-ff412251541d}';
  {$EXTERNALSYM MF_MT_AUDIO_BLOCK_ALIGNMENT}

  // {f2deb57f-40fa-4764-aa33-ed4f2d1ff669}   MF_MT_AUDIO_BITS_PER_SAMPLE         {UINT32}
  MF_MT_AUDIO_BITS_PER_SAMPLE                 : TGuid = '{f2deb57f-40fa-4764-aa33-ed4f2d1ff669}';
  {$EXTERNALSYM MF_MT_AUDIO_BITS_PER_SAMPLE}

  // {d9bf8d6a-9530-4b7c-9ddf-ff6fd58bbd06}   MF_MT_AUDIO_VALID_BITS_PER_SAMPLE   {UINT32}
  MF_MT_AUDIO_VALID_BITS_PER_SAMPLE           : TGuid = '{d9bf8d6a-9530-4b7c-9ddf-ff6fd58bbd06}';
  {$EXTERNALSYM MF_MT_AUDIO_VALID_BITS_PER_SAMPLE}

  // {aab15aac-e13a-4995-9222-501ea15c6877}   MF_MT_AUDIO_SAMPLES_PER_BLOCK       {UINT32}
  MF_MT_AUDIO_SAMPLES_PER_BLOCK               : TGuid = '{aab15aac-e13a-4995-9222-501ea15c6877}';
  {$EXTERNALSYM MF_MT_AUDIO_SAMPLES_PER_BLOCK}

  // {55fb5765-644a-4caf-8479-938983bb1588}   MF_MT_AUDIO_CHANNEL_MASK            {UINT32}
  MF_MT_AUDIO_CHANNEL_MASK                    : TGuid = '{55fb5765-644a-4caf-8479-938983bb1588}';
  {$EXTERNALSYM MF_MT_AUDIO_CHANNEL_MASK}


// MF_MT_AUDIO_FOLDDOWN_MATRIX stores folddown structure from multichannel to stereo

type
  PMFFOLDDOWN_MATRIX = ^MFFOLDDOWN_MATRIX;
  _MFFOLDDOWN_MATRIX = record
    cbSize: UINT32;
    cSrcChannels: UINT32;            // number of source channels
    cDstChannels: UINT32;            // number of destination channels
    dwChannelMask: UINT32;           // mask
    Coeff: array[0..63] of LONG;
  end;
  {$EXTERNALSYM _MFFOLDDOWN_MATRIX}
  MFFOLDDOWN_MATRIX = _MFFOLDDOWN_MATRIX;
  {$EXTERNALSYM MFFOLDDOWN_MATRIX}


const

  // {9d62927c-36be-4cf2-b5c4-a3926e3e8711}     MF_MT_AUDIO_FOLDDOWN_MATRIX         {BLOB, MFFOLDDOWN_MATRIX}
  MF_MT_AUDIO_FOLDDOWN_MATRIX                   : TGuid = '{9d62927c-36be-4cf2-b5c4-a3926e3e8711}';
  {$EXTERNALSYM MF_MT_AUDIO_FOLDDOWN_MATRIX}

  // {9d62927d-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_PEAKREF         {UINT32}
  MF_MT_AUDIO_WMADRC_PEAKREF                    : TGuid = '{9d62927d-36be-4cf2-b5c4-a3926e3e8711}';
  {$EXTERNALSYM MF_MT_AUDIO_WMADRC_PEAKREF}

  // {9d62927e-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_PEAKTARGET        {UINT32}
  MF_MT_AUDIO_WMADRC_PEAKTARGET                 : TGuid = '{9d62927e-36be-4cf2-b5c4-a3926e3e8711}';
  {$EXTERNALSYM MF_MT_AUDIO_WMADRC_PEAKTARGET}

  // {9d62927f-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_AVGREF         {UINT32}
  MF_MT_AUDIO_WMADRC_AVGREF                     : TGuid = '{9d62927f-36be-4cf2-b5c4-a3926e3e8711}';
  {$EXTERNALSYM MF_MT_AUDIO_WMADRC_AVGREF}

  // {9d629280-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_AVGTARGET      {UINT32}
  MF_MT_AUDIO_WMADRC_AVGTARGET                  : TGuid = '{9d629280-36be-4cf2-b5c4-a3926e3e8711}';
  {$EXTERNALSYM MF_MT_AUDIO_WMADRC_AVGTARGET}



  // MF_MT_AUDIO_PREFER_WAVEFORMATEX tells the converter to prefer a plain WAVEFORMATEX rather than
  // a WAVEFORMATEXTENSIBLE when converting to a legacy type. It is set by the WAVEFORMATEX->IMFMediaType
  // conversion routines when the original format block is a non-extensible WAVEFORMATEX.
  //
  // This preference can be overridden and does not guarantee that the type can be correctly expressed
  // by a non-extensible type.


  // {a901aaba-e037-458a-bdf6-545be2074042}     MF_MT_AUDIO_PREFER_WAVEFORMATEX     {UINT32 (BOOL)}
  MF_MT_AUDIO_PREFER_WAVEFORMATEX               : TGuid = '{a901aaba-e037-458a-bdf6-545be2074042}';
  {$EXTERNALSYM MF_MT_AUDIO_PREFER_WAVEFORMATEX}


//#if (WINVER >= _WIN32_WINNT_WIN7) Windows 7

  // AUDIO - AAC extra data
  //=======================

  // {BFBABE79-7434-4d1c-94F0-72A3B9E17188}     MF_MT_AAC_PAYLOAD_TYPE       {UINT32}
  MF_MT_AAC_PAYLOAD_TYPE                        : TGuid = '{BFBABE79-7434-4d1c-94F0-72A3B9E17188}';
  {$EXTERNALSYM MF_MT_AAC_PAYLOAD_TYPE}

  // {7632F0E6-9538-4d61-ACDA-EA29C8C14456}     MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION       {UINT32}
  MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION      : TGuid = '{7632F0E6-9538-4d61-ACDA-EA29C8C14456}';
  {$EXTERNALSYM MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION}

//#endif // (WINVER >= _WIN32_WINNT_WIN7) Windows 7


  // AUDIO - FLAC extra data
  //========================

  MF_MT_AUDIO_FLAC_MAX_BLOCK_SIZE               :  TGUID = '{8b81adae-4b5a-4d40-8022-f38d09ca3c5c}';
  {$EXTERNALSYM MF_MT_AUDIO_FLAC_MAX_BLOCK_SIZE}

//#endif // (WINVER >= _WIN32_WINNT_WIN10)


//#if (NTDDI_VERSION >= NTDDI_WIN10_RS2)

//
// AUDIO - Spatial Audio Sample extra data
//

  // {DCFBA24A-2609-4240-A721-3FAEA76A4DF9} MF_MT_SPATIAL_AUDIO_MAX_DYNAMIC_OBJECTS     {UINT32}
  MF_MT_SPATIAL_AUDIO_MAX_DYNAMIC_OBJECTS : TGUID =  '{dcfba24a-2609-4240-a721-3faea76a4df9}';
  {$EXTERNALSYM MF_MT_SPATIAL_AUDIO_MAX_DYNAMIC_OBJECTS}

  // {2AB71BC0-6223-4BA7-AD64-7B94B47AE792} MF_MT_SPATIAL_AUDIO_OBJECT_METADATA_FORMAT_ID     {GUID}
  MF_MT_SPATIAL_AUDIO_OBJECT_METADATA_FORMAT_ID : TGUID =  '{2ab71bc0-6223-4ba7-ad64-7b94b47ae792}';
  {$EXTERNALSYM MF_MT_SPATIAL_AUDIO_OBJECT_METADATA_FORMAT_ID}

  // {094BA8BE-D723-489F-92FA-766777B34726} MF_MT_SPATIAL_AUDIO_OBJECT_METADATA_LENGTH  {UINT32}
  MF_MT_SPATIAL_AUDIO_OBJECT_METADATA_LENGTH : TGUID =  '{094ba8be-d723-489f-92fa-766777b34726}';
  {$EXTERNALSYM MF_MT_SPATIAL_AUDIO_OBJECT_METADATA_LENGTH}

  // {11AA80B4-E0DA-47C6-8060-96C1259AE50D} MF_MT_SPATIAL_AUDIO_MAX_METADATA_ITEMS {UINT32}
  MF_MT_SPATIAL_AUDIO_MAX_METADATA_ITEMS : TGUID =  '{11aa80b4-e0da-47c6-8060-96c1259ae50d}';
  {$EXTERNALSYM MF_MT_SPATIAL_AUDIO_MAX_METADATA_ITEMS}

  // {83E96EC9-1184-417E-8254-9F269158FC06} MF_MT_SPATIAL_AUDIO_MIN_METADATA_ITEM_OFFSET_SPACING {UINT32}
  MF_MT_SPATIAL_AUDIO_MIN_METADATA_ITEM_OFFSET_SPACING : TGUID =  '{83e96ec9-1184-417e-8254-9f269158fc06}';
  {$EXTERNALSYM MF_MT_SPATIAL_AUDIO_MIN_METADATA_ITEM_OFFSET_SPACING}

  // {6842F6E7-D43E-4EBB-9C9C-C96F41784863} MF_MT_SPATIAL_AUDIO_DATA_PRESENT {UINT32 (BOOL)}
  MF_MT_SPATIAL_AUDIO_DATA_PRESENT : TGUID =  '{6842f6e7-d43e-4ebb-9c9c-c96f41784863}';
  {$EXTERNALSYM MF_MT_SPATIAL_AUDIO_DATA_PRESENT}

//#endif // (NTDDI_VERSION >= NTDDI_WIN10_RS2)


  // VIDEO core data
  //================

  // {1652c33d-d6b2-4012-b834-72030849a37d}     MF_MT_FRAME_SIZE                {UINT64 (HI32(Width),LO32(Height))}
  MF_MT_FRAME_SIZE                              : TGuid = '{1652c33d-d6b2-4012-b834-72030849a37d}';
  {$EXTERNALSYM MF_MT_FRAME_SIZE}

  // {c459a2e8-3d2c-4e44-b132-fee5156c7bb0}     MF_MT_FRAME_RATE                {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE                              : TGuid = '{c459a2e8-3d2c-4e44-b132-fee5156c7bb0}';
  {$EXTERNALSYM MF_MT_FRAME_RATE}

  // {c6376a1e-8d0a-4027-be45-6d9a0ad39bb6}     MF_MT_PIXEL_ASPECT_RATIO        {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_PIXEL_ASPECT_RATIO                      : TGuid = '{c6376a1e-8d0a-4027-be45-6d9a0ad39bb6}';
  {$EXTERNALSYM MF_MT_PIXEL_ASPECT_RATIO}

  // {8772f323-355a-4cc7-bb78-6d61a048ae82}     MF_MT_DRM_FLAGS                 {UINT32 (anyof MFVideoDRMFlags)}
  MF_MT_DRM_FLAGS                               : TGuid = '{8772f323-355a-4cc7-bb78-6d61a048ae82}';
  {$EXTERNALSYM MF_MT_DRM_FLAGS}


//#if (WINVER >= _WIN32_WINNT_WIN8)

  MF_MT_TIMESTAMP_CAN_BE_DTS  :  TGUID = '{24974215-1b7b-41e4-8625-ac469f2dedaa}';
  {$EXTERNALSYM MF_MT_TIMESTAMP_CAN_BE_DTS}

//#endif // (WINVER >= _WIN32_WINNT_WIN8)


type
  PMFVideoDRMFlags = ^_MFVideoDRMFlags;
  _MFVideoDRMFlags = UINT32;
  {$EXTERNALSYM _MFVideoDRMFlags}
  MFVideoDRMFlags = _MFVideoDRMFlags;
  {$EXTERNALSYM MFVideoDRMFlags}
const
  MFVideoDRMFlag_None               = MFVideoDRMFlags(0);
  {$EXTERNALSYM MFVideoDRMFlag_None}
  MFVideoDRMFlag_AnalogProtected    = MFVideoDRMFlags(1);
  {$EXTERNALSYM MFVideoDRMFlag_AnalogProtected}
  MFVideoDRMFlag_DigitallyProtected = MFVideoDRMFlags(2);
  {$EXTERNALSYM MFVideoDRMFlag_DigitallyProtected}


const

  // {4d0e73e5-80ea-4354-a9d0-1176ceb028ea}     MF_MT_PAD_CONTROL_FLAGS         {UINT32 (oneof MFVideoPadFlags)}
  MF_MT_PAD_CONTROL_FLAGS           : TGuid = '{4d0e73e5-80ea-4354-a9d0-1176ceb028ea}';
  {$EXTERNALSYM MF_MT_PAD_CONTROL_FLAGS}


type
  PMFVideoPadFlags = ^_MFVideoPadFlags;
  _MFVideoPadFlags = UINT32;
  {$EXTERNALSYM _MFVideoPadFlags}
  MFVideoPadFlags = _MFVideoPadFlags;
  {$EXTERNALSYM MFVideoPadFlags}
const
  MFVideoPadFlag_PAD_TO_None = MFVideoPadFlags(0);  // Do not pad the image. (default)
  MFVideoPadFlag_PAD_TO_4x3  = MFVideoPadFlags(1);  // Pad the image so that it can be displayed in a 4×3 area.
  MFVideoPadFlag_PAD_TO_16x9 = MFVideoPadFlags(2);  // Pad the image so that it can be displayed in a 16×9 area.


const

  // {68aca3cc-22d0-44e6-85f8-28167197fa38}     MF_MT_SOURCE_CONTENT_HINT       {UINT32 (oneof MFVideoSrcContentHintFlags)}
  MF_MT_SOURCE_CONTENT_HINT                     : TGuid = '{68aca3cc-22d0-44e6-85f8-28167197fa38}';
  {$EXTERNALSYM MF_MT_SOURCE_CONTENT_HINT}


type
  PMFVideoSrcContentHintFlags = ^_MFVideoSrcContentHintFlags;
  _MFVideoSrcContentHintFlags = UINT32;
  {$EXTERNALSYM _MFVideoSrcContentHintFlags}
  MFVideoSrcContentHintFlags = _MFVideoSrcContentHintFlags;
  {$EXTERNALSYM MFVideoSrcContentHintFlags}
const
  MFVideoSrcContentHintFlag_None  = MFVideoSrcContentHintFlags(0);
  MFVideoSrcContentHintFlag_16x9  = MFVideoSrcContentHintFlags(1);
  MFVideoSrcContentHintFlag_235_1 = MFVideoSrcContentHintFlags(2);


const

  // {65df2370-c773-4c33-aa64-843e068efb0c}     MF_MT_CHROMA_SITING             {UINT32 (anyof MFVideoChromaSubsampling)}
  MF_MT_VIDEO_CHROMA_SITING                     : TGuid = '{65df2370-c773-4c33-aa64-843e068efb0c}';
  {$EXTERNALSYM MF_MT_VIDEO_CHROMA_SITING}

  // {e2724bb8-e676-4806-b4b2-a8d6efb44ccd}     MF_MT_INTERLACE_MODE            {UINT32 (oneof MFVideoInterlaceMode)}
  MF_MT_INTERLACE_MODE                          : TGuid = '{e2724bb8-e676-4806-b4b2-a8d6efb44ccd}';
  {$EXTERNALSYM MF_MT_INTERLACE_MODE}

  // {5fb0fce9-be5c-4935-a811-ec838f8eed93}     MF_MT_TRANSFER_FUNCTION         {UINT32 (oneof MFVideoTransferFunction)}
  MF_MT_TRANSFER_FUNCTION                       : TGuid = '{5fb0fce9-be5c-4935-a811-ec838f8eed93}';
  {$EXTERNALSYM MF_MT_TRANSFER_FUNCTION}

  // {dbfbe4d7-0740-4ee0-8192-850ab0e21935}     MF_MT_VIDEO_PRIMARIES           {UINT32 (oneof MFVideoPrimaries)}
  MF_MT_VIDEO_PRIMARIES                         : TGuid = '{dbfbe4d7-0740-4ee0-8192-850ab0e21935}';
  {$EXTERNALSYM MF_MT_VIDEO_PRIMARIES}

  // TODO: switch to RS define once it exists (see: 5312604)
//#if (WINVER >= _WIN32_WINNT_WIN10)
  //
  // MF_MT_MAX_LUMINANCE_LEVEL specifies the maximum luminance level of the content in Nits.
  // Has the same semantics as MaxCLL as defined in CEA-861.3
  //
  // {50253128-C110-4de4-98AE-46A324FAE6DA}   MF_MT_MAX_LUMINANCE_LEVEL   {UINT32}
  MF_MT_MAX_LUMINANCE_LEVEL                     : TGuid = '{50253128-C110-4de4-98AE-46A324FAE6DA}';
  {$EXTERNALSYM MF_MT_MAX_LUMINANCE_LEVEL}

  //
  // MF_MT_MAX_FRAME_AVERAGE_LUMINANCE_LEVEL specifies the maximum average per-frame
  // luminance level of the content in Nits.
  // Has the same semantics as MaxFALL as defined in CEA-861.3
  //
  // {58D4BF57-6F52-4733-A195-A9E29ECF9E27}   MF_MT_MAX_FRAME_AVERAGE_LUMINANCE_LEVEL  {UINT32}
  MF_MT_MAX_FRAME_AVERAGE_LUMINANCE_LEVEL       : TGuid = '{58D4BF57-6F52-4733-A195-A9E29ECF9E27}';
  {$EXTERNALSYM MF_MT_MAX_FRAME_AVERAGE_LUMINANCE_LEVEL}

  //
  // MF_MT_MAX_MASTERING_LUMINANCE specifies the maximum luminance of the display
  // the content was authored on in Nits.
  // Has the same semantics as max_display_mastering_luminance as defined in ST.2086
  //
  // {D6C6B997-272F-4ca1-8D00-8042111A0FF6} MF_MT_MAX_MASTERING_LUMINANCE {UINT32}
  MF_MT_MAX_MASTERING_LUMINANCE                 : TGuid = '{D6C6B997-272F-4ca1-8D00-8042111A0FF6}';
  {$EXTERNALSYM MF_MT_MAX_MASTERING_LUMINANCE}

  //
  // MF_MT_MIN_MASTERING_LUMINANCE specifies the maximum luminance of the display
  // the content was authored on in 0.0001 Nits.
  // Has the same semantics as min_display_mastering_luminance as defined in ST.2086
  //
  // {839A4460-4E7E-4b4f-AE79-CC08905C7B27} MF_MT_MIN_MASTERING_LUMINANCE {UINT32}
  MF_MT_MIN_MASTERING_LUMINANCE                 : TGuid = '{839A4460-4E7E-4b4f-AE79-CC08905C7B27}';
  {$EXTERNALSYM MF_MT_MIN_MASTERING_LUMINANCE}

  //
  // MF_MT_DECODER_USE_MAX_RESOLUTION hints the decoder should allocate worst
  // case supported resolution whenever possible
  // {4c547c24-af9a-4f38-96ad-978773cf53e7} MF_MT_DECODER_USE_MAX_RESOLUTION {UINT32 (BOOL)}
  MF_MT_DECODER_USE_MAX_RESOLUTION              : TGUID =  '{4C547C24-AF9A-4F38-96AD-978773CF53E7}';
  {$EXTERNALSYM MF_MT_DECODER_USE_MAX_RESOLUTION}

  //
  // MF_MT_DECODER_MAX_DPB_COUNT is a value that hints to the decoder that the current
  // decoding session will never require more than the specified number of decode surfaces
  // {67BE144C-88B7-4CA9-9628-C808D5262217} MF_MT_DECODER_MAX_DPB_COUNT {UINT32}
  MF_MT_DECODER_MAX_DPB_COUNT                   : TGUID =  '{67be144c-88b7-4ca9-9628-c808d5262217}';
  {$EXTERNALSYM MF_MT_DECODER_MAX_DPB_COUNT}

//#endif // (WINVER > _WIN32_WINNT_WIN10)


  // {47537213-8cfb-4722-aa34-fbc9e24d77b8}     MF_MT_CUSTOM_VIDEO_PRIMARIES    {BLOB (MT_CUSTOM_VIDEO_PRIMARIES)}
  MF_MT_CUSTOM_VIDEO_PRIMARIES                  : TGuid = '{47537213-8cfb-4722-aa34-fbc9e24d77b8}';
  {$EXTERNALSYM MF_MT_CUSTOM_VIDEO_PRIMARIES}


type

  PMtCustomVideoPrimaries = ^MT_CUSTOM_VIDEO_PRIMARIES;
  _MT_CUSTOM_VIDEO_PRIMARIES = record
    fRx: Float;
    fRy: Float;
    fGx: Float;
    fGy: Float;
    fBx: Float;
    fBy: Float;
    fWx: Float;
    fWy: Float;
  end;
  {$EXTERNALSYM _MT_CUSTOM_VIDEO_PRIMARIES}
  MT_CUSTOM_VIDEO_PRIMARIES = _MT_CUSTOM_VIDEO_PRIMARIES;
  {$EXTERNALSYM MT_CUSTOM_VIDEO_PRIMARIES}


const

  // {3e23d450-2c75-4d25-a00e-b91670d12327}     MF_MT_YUV_MATRIX                {UINT32 (oneof MFVideoTransferMatrix)}
  MF_MT_YUV_MATRIX                              : TGuid = '{3e23d450-2c75-4d25-a00e-b91670d12327}';
  {$EXTERNALSYM MF_MT_YUV_MATRIX}

  // {53a0529c-890b-4216-8bf9-599367ad6d20}     MF_MT_VIDEO_LIGHTING            {UINT32 (oneof MFVideoLighting)}
  MF_MT_VIDEO_LIGHTING                          : TGuid = '{53a0529c-890b-4216-8bf9-599367ad6d20}';
  {$EXTERNALSYM MF_MT_VIDEO_LIGHTING}

  // {c21b8ee5-b956-4071-8daf-325edf5cab11}     MF_MT_VIDEO_NOMINAL_RANGE       {UINT32 (oneof MFNominalRange)}
  MF_MT_VIDEO_NOMINAL_RANGE                     : TGuid = '{c21b8ee5-b956-4071-8daf-325edf5cab11}';
  {$EXTERNALSYM MF_MT_VIDEO_NOMINAL_RANGE}

  // {66758743-7e5f-400d-980a-aa8596c85696}     MF_MT_GEOMETRIC_APERTURE        {BLOB (MFVideoArea)}
  MF_MT_GEOMETRIC_APERTURE                      : TGuid = '{66758743-7e5f-400d-980a-aa8596c85696}';
  {$EXTERNALSYM MF_MT_GEOMETRIC_APERTURE}

  // {d7388766-18fe-48c6-a177-ee894867c8c4}     MF_MT_MINIMUM_DISPLAY_APERTURE  {BLOB (MFVideoArea)}
  MF_MT_MINIMUM_DISPLAY_APERTURE                : TGuid = '{d7388766-18fe-48c6-a177-ee894867c8c4}';
  {$EXTERNALSYM MF_MT_MINIMUM_DISPLAY_APERTURE}

  // {79614dde-9187-48fb-b8c7-4d52689de649}     MF_MT_PAN_SCAN_APERTURE         {BLOB (MFVideoArea)}
  MF_MT_PAN_SCAN_APERTURE                       : TGuid = '{79614dde-9187-48fb-b8c7-4d52689de649}';
  {$EXTERNALSYM MF_MT_PAN_SCAN_APERTURE}

  // {4b7f6bc3-8b13-40b2-a993-abf630b8204e}     MF_MT_PAN_SCAN_ENABLED          {UINT32 (BOOL)}
  MF_MT_PAN_SCAN_ENABLED                        : TGuid = '{4b7f6bc3-8b13-40b2-a993-abf630b8204e}';
  {$EXTERNALSYM MF_MT_PAN_SCAN_ENABLED}

  // {20332624-fb0d-4d9e-bd0d-cbf6786c102e}     MF_MT_AVG_BITRATE               {UINT32}
  MF_MT_AVG_BITRATE                             : TGuid = '{20332624-fb0d-4d9e-bd0d-cbf6786c102e}';
  {$EXTERNALSYM MF_MT_AVG_BITRATE}

  // {799cabd6-3508-4db4-a3c7-569cd533deb1}     MF_MT_AVG_BIT_ERROR_RATE        {UINT32}
  MF_MT_AVG_BIT_ERROR_RATE                      : TGuid = '{799cabd6-3508-4db4-a3c7-569cd533deb1}';
  {$EXTERNALSYM MF_MT_AVG_BIT_ERROR_RATE}

  // {c16eb52b-73a1-476f-8d62-839d6a020652}     MF_MT_MAX_KEYFRAME_SPACING      {UINT32}
  MF_MT_MAX_KEYFRAME_SPACING                    : TGuid = '{c16eb52b-73a1-476f-8d62-839d6a020652}';
  {$EXTERNALSYM MF_MT_MAX_KEYFRAME_SPACING}

  // {a505d3ac-f930-436e-8ede-93a509ce23b2}     MF_MT_OUTPUT_BUFFER_NUM         {UINT32}
  MF_MT_OUTPUT_BUFFER_NUM                       : TGuid = '{a505d3ac-f930-436e-8ede-93a509ce23b2}';
  {$EXTERNALSYM MF_MT_OUTPUT_BUFFER_NUM}

  // TODO: Fix when GovM has the right ifdef check
//#if (WINVER >= _WIN32_WINNT_WIN10)
  /// {0xbb12d222,0x2bdb,0x425e,0x91,0xec,0x23,0x08,0xe1,0x89,0xa5,0x8f}   MF_MT_REALTIME_CONTENT UINT32 (0 or 1)
  MF_MT_REALTIME_CONTENT                        :	TGuid = '{bb12d222-2bdb-425e-91ec-2308e189a58f}';
  {$EXTERNALSYM MF_MT_REALTIME_CONTENT}

//#endif // (WINVER >= _WIN32_WINNT_WIN10

  // VIDEO - uncompressed format data
  //=================================

  // {644b4e48-1e02-4516-b0eb-c01ca9d49ac6}     MF_MT_DEFAULT_STRIDE            {UINT32 (INT32)} // in bytes
  MF_MT_DEFAULT_STRIDE                          : TGuid = '{644b4e48-1e02-4516-b0eb-c01ca9d49ac6}';
  {$EXTERNALSYM MF_MT_DEFAULT_STRIDE}

  // {6d283f42-9846-4410-afd9-654d503b1a54}     MF_MT_PALETTE                   {BLOB (array of MFPaletteEntry - usually 256)}
  MF_MT_PALETTE                                 : TGuid = '{6d283f42-9846-4410-afd9-654d503b1a54}';
  {$EXTERNALSYM MF_MT_PALETTE}


  // the following is only used for legacy data that was stuck at the end of the format block when
  // the type was converted from a VIDEOINFOHEADER or VIDEOINFOHEADER2 block in an AM_MEDIA_TYPE.

  // {b6bc765f-4c3b-40a4-bd51-2535b66fe09d}     MF_MT_USER_DATA                 {BLOB}
  MF_MT_USER_DATA                               : TGuid = '{b6bc765f-4c3b-40a4-bd51-2535b66fe09d}';
  {$EXTERNALSYM MF_MT_USER_DATA}

  // {73d1072d-1870-4174-a063-29ff4ff6c11e}     MF_MT_AM_FORMAT_TYPE
  MF_MT_AM_FORMAT_TYPE                          : TGuid = '{73d1072d-1870-4174-a063-29ff4ff6c11e}';
  {$EXTERNALSYM MF_MT_AM_FORMAT_TYPE}


  // VIDEO - Generic compressed video extra data
  //============================================

  // {ad76a80b-2d5c-4e0b-b375-64e520137036}   MF_MT_VIDEO_PROFILE             {UINT32}
  MF_MT_VIDEO_PROFILE                           :  TGUID = '{ad76a80b-2d5c-4e0b-b375-64e520137036}';
  {$EXTERNALSYM MF_MT_VIDEO_PROFILE}
  // This is an alias of MF_MT_MPEG2_PROFILE

  // {96f66574-11c5-4015-8666-bff516436da7}   MF_MT_VIDEO_LEVEL               {UINT32}
  MF_MT_VIDEO_LEVEL                             :  TGUID = '{96f66574-11c5-4015-8666-bff516436da7}';
  {$EXTERNALSYM MF_MT_VIDEO_LEVEL}
  // This is an alias of MF_MT_MPEG2_LEVEL



  // VIDEO - MPEG1/2 extra data
  //===========================

  // {91f67885-4333-4280-97cd-bd5a6c03a06e}     MF_MT_MPEG_START_TIME_CODE      {UINT32}
  MF_MT_MPEG_START_TIME_CODE                    : TGuid = '{91f67885-4333-4280-97cd-bd5a6c03a06e}';
  {$EXTERNALSYM MF_MT_MPEG_START_TIME_CODE}

  // {ad76a80b-2d5c-4e0b-b375-64e520137036}     MF_MT_MPEG2_PROFILE             {UINT32 (oneof AM_MPEG2Profile)}
  MF_MT_MPEG2_PROFILE                           : TGuid = '{ad76a80b-2d5c-4e0b-b375-64e520137036}';
  {$EXTERNALSYM MF_MT_MPEG2_PROFILE}

  // {96f66574-11c5-4015-8666-bff516436da7}     MF_MT_MPEG2_LEVEL               {UINT32 (oneof AM_MPEG2Level)}
  MF_MT_MPEG2_LEVEL                             : TGuid = '{96f66574-11c5-4015-8666-bff516436da7}';
  {$EXTERNALSYM MF_MT_MPEG2_LEVEL}

  // {31e3991d-f701-4b2f-b426-8ae3bda9e04b}     MF_MT_MPEG2_FLAGS               {UINT32 (anyof AMMPEG2_xxx flags)}
  MF_MT_MPEG2_FLAGS                             : TGuid = '{31e3991d-f701-4b2f-b426-8ae3bda9e04b}';
  {$EXTERNALSYM MF_MT_MPEG2_FLAGS}

  // {3c036de7-3ad0-4c9e-9216-ee6d6ac21cb3}     MF_MT_MPEG_SEQUENCE_HEADER      {BLOB}
  MF_MT_MPEG_SEQUENCE_HEADER                    : TGuid = '{3c036de7-3ad0-4c9e-9216-ee6d6ac21cb3}';
  {$EXTERNALSYM MF_MT_MPEG_SEQUENCE_HEADER}

  // {A20AF9E8-928A-4B26-AAA9-F05C74CAC47C}   MF_MT_MPEG2_STANDARD            {UINT32 (0 for default MPEG2, 1  to use ATSC standard, 2 to use DVB standard, 3 to use ARIB standard)}
  MF_MT_MPEG2_STANDARD                          :  TGUID = '{a20af9e8-928a-4b26-aaa9-f05c74cac47c}';
  {$EXTERNALSYM MF_MT_MPEG2_STANDARD}

  // {5229BA10-E29D-4F80-A59C-DF4F180207D2}   MF_MT_MPEG2_TIMECODE            {UINT32 (0 for no timecode, 1 to append an 4 byte timecode to the front of each transport packet)}
  MF_MT_MPEG2_TIMECODE                          :  TGUID = '{5229ba10-e29d-4f80-a59c-df4f180207d2}';
  {$EXTERNALSYM MF_MT_MPEG2_TIMECODE}

  // {825D55E4-4F12-4197-9EB3-59B6E4710F06}   MF_MT_MPEG2_CONTENT_PACKET      {UINT32 (0 for no content packet, 1 to append a 14 byte Content Packet header according to the ARIB specification to the beginning a transport packet at 200-1000 ms intervals.)}
  MF_MT_MPEG2_CONTENT_PACKET                    :  TGUID = '{825d55e4-4f12-4197-9eb3-59b6e4710f06}';
  {$EXTERNALSYM MF_MT_MPEG2_CONTENT_PACKET}

  // {91a49eb5-1d20-4b42-ace8-804269bf95ed}   MF_MT_MPEG2_ONE_FRAME_PER_PACKET      {UINT32 (BOOL) -- 0 for default behavior of splitting large video frames into multiple PES packets, 1 for always putting a full frame inside a PES packet, even if that requires setting the PES packet size to undefined (0)}
  MF_MT_MPEG2_ONE_FRAME_PER_PACKET              :  TGUID = '{91a49eb5-1d20-4b42-ace8-804269bf95ed}';
  {$EXTERNALSYM MF_MT_MPEG2_ONE_FRAME_PER_PACKET}

  // {168f1b4a-3e91-450f-aea7-e4baeadae5ba} MF_MT_MPEG2_HDCP  {UINT32 (BOOL) -- 0 for default behavior of clear MPEG2 stream, 1 for adding the HDCP descriptor to the PMT
  MF_MT_MPEG2_HDCP                              :  TGUID = '{168f1b4a-3e91-450f-aea7-e4baeadae5ba}';
  {$EXTERNALSYM MF_MT_MPEG2_HDCP}


  // VIDEO - H264 extra data
  //========================

  // {F5929986-4C45-4FBB-BB49-6CC534D05B9B}  {UINT32, UVC 1.5 H.264 format descriptor: bMaxCodecConfigDelay}
  MF_MT_H264_MAX_CODEC_CONFIG_DELAY             : TGUID = '{f5929986-4c45-4fbb-bb49-6cc534d05b9b}';
  {$EXTERNALSYM MF_MT_H264_MAX_CODEC_CONFIG_DELAY}

  // {C8BE1937-4D64-4549-8343-A8086C0BFDA5} {UINT32, UVC 1.5 H.264 format descriptor: bmSupportedSliceModes}
  MF_MT_H264_SUPPORTED_SLICE_MODES              : TGUID = '{c8be1937-4d64-4549-8343-a8086c0bfda5}';
  {$EXTERNALSYM MF_MT_H264_SUPPORTED_SLICE_MODES}

  // {89A52C01-F282-48D2-B522-22E6AE633199} {UINT32, UVC 1.5 H.264 format descriptor: bmSupportedSyncFrameTypes}
  MF_MT_H264_SUPPORTED_SYNC_FRAME_TYPES         : TGUID = '{89a52c01-f282-48d2-b522-22e6ae633199}';
  {$EXTERNALSYM MF_MT_H264_SUPPORTED_SYNC_FRAME_TYPES}

  // {E3854272-F715-4757-BA90-1B696C773457} {UINT32, UVC 1.5 H.264 format descriptor: bResolutionScaling}
  MF_MT_H264_RESOLUTION_SCALING                 : TGUID = '{e3854272-f715-4757-ba90-1b696c773457}';
  {$EXTERNALSYM MF_MT_H264_RESOLUTION_SCALING}

  // {9EA2D63D-53F0-4A34-B94E-9DE49A078CB3} {UINT32, UVC 1.5 H.264 format descriptor: bSimulcastSupport}
  MF_MT_H264_SIMULCAST_SUPPORT                  : TGUID = '{9ea2d63d-53f0-4a34-b94e-9de49a078cb3}';
  {$EXTERNALSYM MF_MT_H264_SIMULCAST_SUPPORT}

  // {6A8AC47E-519C-4F18-9BB3-7EEAAEA5594D} {UINT32, UVC 1.5 H.264 format descriptor: bmSupportedRateControlModes}
  MF_MT_H264_SUPPORTED_RATE_CONTROL_MODES       : TGUID = '{6a8ac47e-519c-4f18-9bb3-7eeaaea5594d}';
  {$EXTERNALSYM MF_MT_H264_SUPPORTED_RATE_CONTROL_MODES}

  // {45256D30-7215-4576-9336-B0F1BCD59BB2}  {Blob of size 20 * sizeof(WORD), UVC 1.5 H.264 format descriptor: wMaxMBperSec*}
  MF_MT_H264_MAX_MB_PER_SEC                     : TGUID = '{45256d30-7215-4576-9336-b0f1bcd59bb2}';
  {$EXTERNALSYM MF_MT_H264_MAX_MB_PER_SEC}

  // {60B1A998-DC01-40CE-9736-ABA845A2DBDC}         {UINT32, UVC 1.5 H.264 frame descriptor: bmSupportedUsages}
  MF_MT_H264_SUPPORTED_USAGES                   : TGUID = '{60b1a998-dc01-40ce-9736-aba845a2dbdc}';
  {$EXTERNALSYM MF_MT_H264_SUPPORTED_USAGES}

  // {BB3BD508-490A-11E0-99E4-1316DFD72085}         {UINT32, UVC 1.5 H.264 frame descriptor: bmCapabilities}
  MF_MT_H264_CAPABILITIES                       : TGUID = '{bb3bd508-490a-11e0-99e4-1316dfd72085}';
  {$EXTERNALSYM MF_MT_H264_CAPABILITIES}

  // {F8993ABE-D937-4A8F-BBCA-6966FE9E1152}         {UINT32, UVC 1.5 H.264 frame descriptor: bmSVCCapabilities}
  MF_MT_H264_SVC_CAPABILITIES                   : TGUID = '{f8993abe-d937-4a8f-bbca-6966fe9e1152}';
  {$EXTERNALSYM MF_MT_H264_SVC_CAPABILITIES}

  // {359CE3A5-AF00-49CA-A2F4-2AC94CA82B61}         {UINT32, UVC 1.5 H.264 Probe/Commit Control: bUsage}
  MF_MT_H264_USAGE                              : TGUID = '{359ce3a5-af00-49ca-a2f4-2ac94ca82b61}';
  {$EXTERNALSYM MF_MT_H264_USAGE}

  // {705177D8-45CB-11E0-AC7D-B91CE0D72085}          {UINT32, UVC 1.5 H.264 Probe/Commit Control: bmRateControlModes}
  MF_MT_H264_RATE_CONTROL_MODES                 : TGUID = '{705177d8-45cb-11e0-ac7d-b91ce0d72085}';
  {$EXTERNALSYM MF_MT_H264_RATE_CONTROL_MODES}

  // {85E299B2-90E3-4FE8-B2F5-C067E0BFE57A}          {UINT64, UVC 1.5 H.264 Probe/Commit Control: bmLayoutPerStream}
  MF_MT_H264_LAYOUT_PER_STREAM                  : TGUID = '{85e299b2-90e3-4fe8-b2f5-c067e0bfe57a}';
  {$EXTERNALSYM MF_MT_H264_LAYOUT_PER_STREAM}

  // {75DA5090-910B-4A03-896C-7B898FEEA5AF}
  MF_MT_IN_BAND_PARAMETER_SET                   : TGUID = '{75da5090-910b-4a03-896c-7b898feea5af}';
  {$EXTERNALSYM MF_MT_IN_BAND_PARAMETER_SET}
  // According to Mpeg4 spec, SPS and PPS of H.264/HEVC codec could appear in sample data.
  // description box. Mpeg4 sink filters out the SPS and PPS NALU and do not support in band SPS and PPS NALU.
  // This attribute enables support for in band SPS and PPS to appear in the elementary stream.
  // HEVC will have in-band parameter set by default with MP4 recording for broad support.
  // H.264 will have out - of - band parameter set by default for historical reason.

  //{54F486DD-9327-4F6D-80AB-6F709EBB4CCE}          {UINT32, FourCC of the track type in MPEG-4 used for binary streams}
  MF_MT_MPEG4_TRACK_TYPE                        : TGUID = '{54F486DD-9327-4F6D-80AB-6F709EBB4CCE}';
  {$EXTERNALSYM MF_MT_MPEG4_TRACK_TYPE}

  // The speed-up factor of playback. This is a multiplier to the frame rate, and is expressed as a ratio.
  // The upper 32 bits of the attribute value contain the numerator, and the lower 32 bits contain the denominator.
  // Note that this applies to the entirety of the playback.
  // Use MFGetAttributeRatio/MFSetAttributeRatio to get/set value.
  // {83877F5E-0444-4E28-8479-6DB0989B8C09}   {UINT64}
  MF_MT_CONTAINER_RATE_SCALING                  : TGUID = '{83877F5E-0444-4E28-8479-6DB0989B8C09}';
  {$EXTERNALSYM MF_MT_CONTAINER_RATE_SCALING}



  // INTERLEAVED - DV extra data
  //============================

  // {84bd5d88-0fb8-4ac8-be4b-a8848bef98f3}     MF_MT_DV_AAUX_SRC_PACK_0        {UINT32}
  MF_MT_DV_AAUX_SRC_PACK_0                      : TGuid = '{84bd5d88-0fb8-4ac8-be4b-a8848bef98f3}';
  {$EXTERNALSYM MF_MT_DV_AAUX_SRC_PACK_0}

  // {f731004e-1dd1-4515-aabe-f0c06aa536ac}     MF_MT_DV_AAUX_CTRL_PACK_0       {UINT32}
  MF_MT_DV_AAUX_CTRL_PACK_0                     : TGuid = '{f731004e-1dd1-4515-aabe-f0c06aa536ac}';
  {$EXTERNALSYM MF_MT_DV_AAUX_CTRL_PACK_0}

  // {720e6544-0225-4003-a651-0196563a958e}     MF_MT_DV_AAUX_SRC_PACK_1        {UINT32}
  MF_MT_DV_AAUX_SRC_PACK_1                      : TGuid = '{720e6544-0225-4003-a651-0196563a958e}';
  {$EXTERNALSYM MF_MT_DV_AAUX_SRC_PACK_1}

  // {cd1f470d-1f04-4fe0-bfb9-d07ae0386ad8}     MF_MT_DV_AAUX_CTRL_PACK_1       {UINT32}
  MF_MT_DV_AAUX_CTRL_PACK_1                     : TGuid = '{cd1f470d-1f04-4fe0-bfb9-d07ae0386ad8}';
  {$EXTERNALSYM MF_MT_DV_AAUX_CTRL_PACK_1}

  // {41402d9d-7b57-43c6-b129-2cb997f15009}     MF_MT_DV_VAUX_SRC_PACK          {UINT32}
  MF_MT_DV_VAUX_SRC_PACK                        : TGuid = '{41402d9d-7b57-43c6-b129-2cb997f15009}';
  {$EXTERNALSYM MF_MT_DV_VAUX_SRC_PACK}

  // {2f84e1c4-0da1-4788-938e-0dfbfbb34b48}     MF_MT_DV_VAUX_CTRL_PACK         {UINT32}
  MF_MT_DV_VAUX_CTRL_PACK                       : TGuid = '{2f84e1c4-0da1-4788-938e-0dfbfbb34b48}';
  {$EXTERNALSYM MF_MT_DV_VAUX_CTRL_PACK}

  // {9E6BD6F5-0109-4f95-84AC-9309153A19FC}   MF_MT_ARBITRARY_HEADER          {MT_ARBITRARY_HEADER}
  MF_MT_ARBITRARY_HEADER                        : TGUID = '{9e6bd6f5-0109-4f95-84ac-9309153a19fc}';
  {$EXTERNALSYM MF_MT_ARBITRARY_HEADER}

  // {5A75B249-0D7D-49a1-A1C3-E0D87F0CADE5}   MF_MT_ARBITRARY_FORMAT          {Blob}
  MF_MT_ARBITRARY_FORMAT                        : TGUID = '{5a75b249-0d7d-49a1-a1c3-e0d87f0cade5}';
  {$EXTERNALSYM MF_MT_ARBITRARY_FORMAT}


  // IMAGE
  //======

  // {ED062CF4-E34E-4922-BE99-934032133D7C}   MF_MT_IMAGE_LOSS_TOLERANT       {UINT32 (BOOL)}
  MF_MT_IMAGE_LOSS_TOLERANT       : TGUID = '{ed062cf4-e34e-4922-be99-934032133d7c}';
  {$EXTERNALSYM MF_MT_IMAGE_LOSS_TOLERANT}


  // MPEG-4 Media Type Attributes
  //=============================

  // {261E9D83-9529-4B8F-A111-8B9C950A81A9}   MF_MT_MPEG4_SAMPLE_DESCRIPTION   {BLOB}
  MF_MT_MPEG4_SAMPLE_DESCRIPTION   : TGUID = '{261e9d83-9529-4b8f-a111-8b9c950a81a9}';
  {$EXTERNALSYM MF_MT_MPEG4_SAMPLE_DESCRIPTION}

  // {9aa7e155-b64a-4c1d-a500-455d600b6560}   MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY {UINT32}
  MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY : TGUID = '{9aa7e155-b64a-4c1d-a500-455d600b6560}';
  {$EXTERNALSYM MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY}


//#if (NTDDI_VERSION >= NTDDI_WIN10_RS4)
  //
  // Ambisonics Stream Attribute
  // The value of this blob must be AMBISONICS_PARAMS structure defined in AudioClient.h
  //
  // {F715CF3E-A964-4C3F-94AE-9D6BA7264641}   MF_SD_AMBISONICS_SAMPLE3D_DESCRIPTION   {BLOB}
  MF_SD_AMBISONICS_SAMPLE3D_DESCRIPTION : TGUID = '{F715CF3E-A964-4C3F-94AE-9D6BA7264641}';
  {$EXTERNALSYM MF_SD_AMBISONICS_SAMPLE3D_DESCRIPTION}

//#endif (NTDDI_VERSION >= NTDDI_WIN10_RS4)


  // Save original format information for AVI and WAV files
  //=======================================================

  // {d7be3fe0-2bc7-492d-b843-61a1919b70c3}   MF_MT_ORIGINAL_4CC               (UINT32)
  MF_MT_ORIGINAL_4CC: TGUID = '{d7be3fe0-2bc7-492d-b843-61a1919b70c3}';
  {$EXTERNALSYM MF_MT_ORIGINAL_4CC}

  // {8cbbc843-9fd9-49c2-882f-a72586c408ad}   MF_MT_ORIGINAL_WAVE_FORMAT_TAG   (UINT32)
  MF_MT_ORIGINAL_WAVE_FORMAT_TAG : TGUID = '{8cbbc843-9fd9-49c2-882f-a72586c408ad}';
  {$EXTERNALSYM MF_MT_ORIGINAL_WAVE_FORMAT_TAG}


  // Video Capture Media Type Attributes
  //====================================

  // {D2E7558C-DC1F-403f-9A72-D28BB1EB3B5E}   MF_MT_FRAME_RATE_RANGE_MIN      {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE_RANGE_MIN : TGUID = '{d2e7558c-dc1f-403f-9a72-d28bb1eb3b5e}';
  {$EXTERNALSYM MF_MT_FRAME_RATE_RANGE_MIN}

  // {E3371D41-B4CF-4a05-BD4E-20B88BB2C4D6}   MF_MT_FRAME_RATE_RANGE_MAX      {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE_RANGE_MAX : TGUID = '{e3371d41-b4cf-4a05-bd4e-20b88bb2c4d6}';
  {$EXTERNALSYM MF_MT_FRAME_RATE_RANGE_MAX}

  // {9C27891A-ED7A-40e1-88E8-B22727A024EE}   MF_LOW_LATENCY                  {UINT32 (BOOL)}
  MF_LOW_LATENCY: TGUID = '{9c27891a-ed7a-40e1-88e8-b22727a024ee}';
  {$EXTERNALSYM MF_LOW_LATENCY}
  // Same GUID as CODECAPI_AVLowLatencyMode

  // {E3F2E203-D445-4B8C-9211-AE390D3BA017}  {UINT32} Maximum macroblocks per second that can be handled by MFT
  MF_VIDEO_MAX_MB_PER_SEC : TGUID = '{e3f2e203-d445-4b8c-9211-ae390d3ba017}';
  {$EXTERNALSYM MF_VIDEO_MAX_MB_PER_SEC}

  // {7086E16C-49C5-4201-882A-8538F38CF13A} {UINT32 (BOOL)} Enables(0, default)/disables(1) the DXVA decode status queries in decoders. When disabled decoder won't provide MFSampleExtension_FrameCorruption
  MF_DISABLE_FRAME_CORRUPTION_INFO : TGUID = '{7086e16c-49c5-4201-882a-8538f38cf13a}';
  {$EXTERNALSYM MF_DISABLE_FRAME_CORRUPTION_INFO}


  ////////////////////////////////////////////////////////////////////////////////
  // Camera Extrinsics
  ////////////////////////////////////////////////////////////////////////////////

type

  PMfFloat2 = ^MfFloat2;
  _MF_FLOAT2 = record
    x: FLOAT;
    y: FLOAT;
  end;
  {$EXTERNALSYM _MF_FLOAT2}
  MfFloat2 = _MF_FLOAT2;
  {$EXTERNALSYM MfFloat2}


  PMfFloat3 = ^MfFloat3;
  _MF_FLOAT3 = record
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
  end;
  {$EXTERNALSYM _MF_FLOAT3}
  MfFloat3 = _MF_FLOAT3;
  {$EXTERNALSYM MfFloat3}


  PMfQuaternion = ^MfQuaternion;
  _MF_QUATERNION = record
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    w: FLOAT;
  end;
  {$EXTERNALSYM _MF_QUATERNION}
  MF_QUATERNION = _MF_QUATERNION;
  {$EXTERNALSYM MF_QUATERNION}
  MfQuaternion = MF_QUATERNION;
  {$EXTERNALSYM MfQuaternion}


  PMFCameraExtrinsicCalibratedTransform = ^MFCameraExtrinsicCalibratedTransform;
  _MFCameraExtrinsic_CalibratedTransform = record
    CalibrationId: TGUID;
    Position: MfFloat3;
    Orientation: MF_QUATERNION;
  end;
  {$EXTERNALSYM _MFCameraExtrinsic_CalibratedTransform}
  MFCameraExtrinsicCalibratedTransform = _MFCameraExtrinsic_CalibratedTransform;
  {$EXTERNALSYM MFCameraExtrinsicCalibratedTransform}


  PMFCameraExtrinsics = ^MFCameraExtrinsics;
  _MFCameraExtrinsics = record
    TransformCount: UINT32;
    CalibratedTransforms: array[0..0] of MFCameraExtrinsicCalibratedTransform;
  end;
  {$EXTERNALSYM _MFCameraExtrinsics}
  MFCameraExtrinsics = _MFCameraExtrinsics;
  {$EXTERNALSYM MFCameraExtrinsics}

const

  //
  // MFStreamExtension_CameraExtrinsics {686196D0-13E2-41D9-9638-EF032C272A52}
  // Value type: Blob (MFCameraExtrinsics)
  // Stores camera extrinsics data on the stream's attribute store
  //
  MFStreamExtension_CameraExtrinsics : TGUID = '{686196D0-13E2-41D9-9638-EF032C272A52}';
  {$EXTERNALSYM MFStreamExtension_CameraExtrinsics}

  //
  // MFSampleExtension_CameraExtrinsics {6B761658-B7EC-4C3B-8225-8623CABEC31D}
  // Value type: Blob (MFCameraExtrinsics)
  // Stores camera extrinsics data on the sample's (a.k.a frame) attribute store
  //
  MFSampleExtension_CameraExtrinsics : TGUID = '{6b761658-b7ec-4c3b-8225-8623cabec31d}';
  {$EXTERNALSYM MFSampleExtension_CameraExtrinsics}


  ////////////////////////////////////////////////////////////////////////////////
  // Camera Intrinsics
  ////////////////////////////////////////////////////////////////////////////////

type

  PMFCameraIntrinsicPinholeCameraModel = ^MFCameraIntrinsicPinholeCameraModel;
  _MFCameraIntrinsic_PinholeCameraModel = record
    FocalLength: MfFloat2;
    PrincipalPoint: MfFloat2;
  end;
  {$EXTERNALSYM _MFCameraIntrinsic_PinholeCameraModel}
  MFCameraIntrinsicPinholeCameraModel = _MFCameraIntrinsic_PinholeCameraModel;
  {$EXTERNALSYM MFCameraIntrinsicPinholeCameraModel}


  PMFCameraIntrinsicDistortionModel = ^MFCameraIntrinsicDistortionModel;
  _MFCameraIntrinsic_DistortionModel = record
    Radial_k1: FLOAT;
    Radial_k2: FLOAT;
    Radial_k3: FLOAT;
    Tangential_p1: FLOAT;
    Tangential_p2: FLOAT;
  end;
  {$EXTERNALSYM _MFCameraIntrinsic_DistortionModel}
  MFCameraIntrinsicDistortionModel = _MFCameraIntrinsic_DistortionModel;
  {$EXTERNALSYM MFCameraIntrinsicDistortionModel}


  PMFPinholeCameraIntrinsicIntrinsicModel = ^MFPinholeCameraIntrinsicIntrinsicModel;
  _MFPinholeCameraIntrinsic_IntrinsicModel = record
    Width: UINT32;
    Height: UINT32;
    CameraModel: MFCameraIntrinsicPinholeCameraModel;
    DistortionModel: MFCameraIntrinsicDistortionModel;
  end;
  {$EXTERNALSYM _MFPinholeCameraIntrinsic_IntrinsicModel}
  MFPinholeCameraIntrinsicIntrinsicModel = _MFPinholeCameraIntrinsic_IntrinsicModel;
  {$EXTERNALSYM MFPinholeCameraIntrinsicIntrinsicModel}


  PMFPinholeCameraIntrinsics = ^MFPinholeCameraIntrinsics;
  _MFPinholeCameraIntrinsics = record
    IntrinsicModelCount: UINT32;
    IntrinsicModels: array[0..0] of MFPinholeCameraIntrinsicIntrinsicModel;
  end;
  {$EXTERNALSYM _MFPinholeCameraIntrinsics}
  MFPinholeCameraIntrinsics = _MFPinholeCameraIntrinsics;
  {$EXTERNALSYM MFPinholeCameraIntrinsics}


const

  // MFStreamExtension_PinholeCameraIntrinsics {DBAC0455-0EC8-4AEF-9C32-7A3EE3456F53}
  // Value type: Blob (MFPinholeCameraIntrinsics)
  // Stores camera intrinsics data on stream attribute store
  MFStreamExtension_PinholeCameraIntrinsics : TGUID = '{DBAC0455-0EC8-4AEF-9C32-7A3EE3456F53}';
  {$EXTERNALSYM MFStreamExtension_PinholeCameraIntrinsics}

  // MFSampleExtension_PinholeCameraIntrinsics {4EE3B6C5-6A15-4E72-9761-70C1DB8B9FE3}
  // Value type: Blob (MFPinholeCameraIntrinsic_IntrinsicModel)
  // Stores camera intrinsics data on the sample's (a.k.a frame) attribute store
  MFSampleExtension_PinholeCameraIntrinsics : TGUID = '{4EE3B6C5-6A15-4E72-9761-70C1DB8B9FE3}';
  {$EXTERNALSYM MFSampleExtension_PinholeCameraIntrinsics}


  //////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////  Media Type GUIDs ////////////////////////////
  //////////////////////////////////////////////////////////////////////////////


  // Major types
  //============

  MFMediaType_Default                           : TGUID = '{81A412E6-8103-4B06-857F-1862781024AC}';
  {$EXTERNALSYM MFMediaType_Default}
  MFMediaType_Audio                             : TGUID = '{73647561-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM MFMediaType_Audio}
  MFMediaType_Video                             : TGUID = '{73646976-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM MFMediaType_Video}
  MFMediaType_Protected                         : TGUID = '{7b4b6fe6-9d04-4494-be14-7e0bd076c8e4}';
  {$EXTERNALSYM MFMediaType_Protected}
  MFMediaType_SAMI                              : TGUID = '{e69669a0-3dcd-40cb-9e2e-3708387c0616}';
  {$EXTERNALSYM MFMediaType_SAMI}
  MFMediaType_Script                            : TGUID = '{72178C22-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFMediaType_Script}
  MFMediaType_Image                             : TGUID = '{72178C23-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFMediaType_Image}
  MFMediaType_HTML                              : TGUID = '{72178C24-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFMediaType_HTML}
  MFMediaType_Binary                            : TGUID = '{72178C25-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFMediaType_Binary}
  MFMediaType_FileTransfer                      : TGUID = '{72178C26-E45B-11D5-BC2A-00B0D0F3F4AB}';
  {$EXTERNALSYM MFMediaType_FileTransfer}
  MFMediaType_Stream                            : TGUID = '{e436eb83-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM MFMediaType_Stream}
  MFMediaType_MultiplexedFrames                 : TGUID = '{6ea542b0-281f-4231-a464-fe2f5022501c}';
  {$EXTERNALSYM MFMediaType_MultiplexedFrames}
  MFMediaType_Subtitle                          : TGUID = '{a6d13581-ed50-4e65-ae08-26065576aacc}';
  {$EXTERNALSYM MFMediaType_Subtitle}


  // TODO: switch to RS define once it exists (see: 5312604)
  //#if (WINVER >= _WIN32_WINNT_WIN10)
  MFMediaType_Perception                        : TGUID = '{597ff6f9-6ea2-4670-85b4-ea84073fe940}';
  {$EXTERNALSYM MFMediaType_Perception}
  //#endif // (WINVER >= _WIN32_WINNT_WIN10)


  // Image subtypes (MFMediaType_Image major type)
  //==============================================

  // JPEG subtype: same as GUID_ContainerFormatJpeg (defined in (DX12.)WinCodec.pas)
  MFImageFormat_JPEG: TGUID = '{19e4a5aa-5662-4fc5-a0c0-1758028e1057}';
  {$EXTERNALSYM MFImageFormat_JPEG}

  // RGB32 subtype: same as MFVideoFormat_RGB32
  MFImageFormat_RGB32: TGUID = '{00000016-0000-0010-8000-00aa00389b71}';
  {$EXTERNALSYM MFImageFormat_RGB32}


  // MPEG2 Stream subtypes (MFMediaType_Stream major type)
  //======================================================

  MFStreamFormat_MPEG2Transport                 : TGUID = '{e06d8023-db46-11cf-b4d1-00805f6cbbea}';
  {$EXTERNALSYM MFStreamFormat_MPEG2Transport}

  MFStreamFormat_MPEG2Program                   : TGUID = '{263067d1-d330-45dc-b669-34d986e4e3e1}';
  {$EXTERNALSYM MFStreamFormat_MPEG2Program}


  // Representations
  //================

  AM_MEDIA_TYPE_REPRESENTATION                  : TGUID = '{e2e42ad2-132c-491e-a268-3c7c2dca181f}';
  {$EXTERNALSYM AM_MEDIA_TYPE_REPRESENTATION}

  FORMAT_MFVideoFormat                          : TGUID = '{aed4ab2d-7326-43cb-9464-c879cab9c43d}';
  {$EXTERNALSYM FORMAT_MFVideoFormat}


//////////////////////////////////  Media Type functions ///////////////////////
////////////////////////////////////////////////////////////////////////////////


  // Forward declaration
  //====================

  // struct tagVIDEOINFOHEADER;  declared in AmVideo.pas
  // struct tagVIDEOINFOHEADER2; declared in DvdMedia.pas
  // struct tagMPEG1VIDEOINFO; declared in AmVideo.pas
  // struct tagMPEG2VIDEOINFO; declared in DvdMedia.pas
  // struct _AMMediaType;  declared in StrMif.pas

  function MFValidateMediaTypeSize(FormatType: TGUID;
                                   pBlock: UINT8;
                                   cbSize: UINT32): HResult; stdcall;
  {$EXTERNALSYM MFValidateMediaTypeSize}

  function MFCreateMediaType(out ppMFType: IMFMediaType): HResult; stdcall;
  {$EXTERNALSYM MFCreateMediaType}

  function MFCreateMFVideoFormatFromMFMediaType(pMFType: IMFMediaType;
                                                out ppMFVF: MFVIDEOFORMAT;  // must be deleted with CoTaskMemFree
                                                out pcbSize: UINT32): HResult; stdcall;
  {$EXTERNALSYM MFCreateMFVideoFormatFromMFMediaType}


type
  PMFWaveFormatExConvertFlags = ^MFWaveFormatExConvertFlags;
  _MFWaveFormatExConvertFlags = UINT32;
  {$EXTERNALSYM _MFWaveFormatExConvertFlags}
  MFWaveFormatExConvertFlags = _MFWaveFormatExConvertFlags;
  {$EXTERNALSYM MFWaveFormatExConvertFlags}
const
  MFWaveFormatExConvertFlag_Normal          = UINT32(0);
  {$EXTERNALSYM MFWaveFormatExConvertFlag_Normal}
  MFWaveFormatExConvertFlag_ForceExtensible = UINT32(1);
  {$EXTERNALSYM MFWaveFormatExConvertFlag_ForceExtensible}



  // Converts a Media Foundation audio media type to a WAVEFORMATEX structure.
  function MFCreateWaveFormatExFromMFMediaType(pMFType: IMFMediaType; // Pointer to the IMFMediaType interface of the media type.
                                               var ppWF: PWAVEFORMATEX; // Receives a pointer to the WAVEFORMATEX structure. The caller must release the memory allocated for the structure by calling CoTaskMemFree.
                                               out pcbSize: UINT32; // Receives the size of the WAVEFORMATEX structure.
                                               Flags: UINT32 = 0): HResult; stdcall; // Contains a flag from the MFWaveFormatExConvertFlags enumeration.
  {$EXTERNALSYM MFCreateWaveFormatExFromMFMediaType}

  function MFInitMediaTypeFromVideoInfoHeader(pMFType: IMFMediaType;
                                              pVIH: VIDEOINFOHEADER;
                                              const cbBufSize: UINT32;
                                              const pSubtype: TGUID): HResult; stdcall;
  {$EXTERNALSYM MFInitMediaTypeFromVideoInfoHeader}

  function MFInitMediaTypeFromVideoInfoHeader2(pMFType: IMFMediaType;
                                               pVIH2: VIDEOINFOHEADER2;
                                               const cbBufSize: UINT32;
                                               const pSubtype: TGUID): HResult; stdcall;
  {$EXTERNALSYM MFInitMediaTypeFromVideoInfoHeader2}


  function MFInitMediaTypeFromMPEG1VideoInfo(pMFType: IMFMediaType;
                                             pMP1VI: MPEG1VIDEOINFO;
                                             const cbBufSize: UINT32;
                                             const pSubtype: TGUID): HResult; stdcall;
  {$EXTERNALSYM MFInitMediaTypeFromMPEG1VideoInfo}


  function MFInitMediaTypeFromMPEG2VideoInfo(pMFType: IMFMediaType;
                                             pMP2VI: MPEG2VIDEOINFO;
                                             const cbBufSize: UINT32;
                                             const pSubtype: TGUID): HResult; stdcall;
  {$EXTERNALSYM MFInitMediaTypeFromMPEG2VideoInfo}


  function MFCalculateBitmapImageSize(pBMIH: BITMAPINFOHEADER;
                                      const cbBufSize: UINT32;
                                      out pcbImageSize: UINT32;
                                      out pbKnown: Boolean): HResult; stdcall;
  {$EXTERNALSYM MFCalculateBitmapImageSize}

  //////////////////////////////////////////////////////////////////////////////

  function MFCalculateImageSize(const guidSubtype: REFGUID;
                                const unWidth: UINT32;
                                const unHeight: UINT32;
                                out pcbImageSize: UINT32): HRESULT; stdcall;
  {$EXTERNALSYM MFCalculateImageSize}

  // Converts a video frame rate into a frame duration.
  // Remarks
  //   This function is useful for calculating time stamps on a sample, given the frame rate.
  //   Also, average time per frame is used in the older VIDEOINFOHEADER and VIDEOINFOHEADER2 format structures.
  //   This function provides a standard conversion so that all components in the pipeline can use consistent values,
  //   if they need to translate between the older format structures and the media type attributes used in Media Foundation.
  //
  //   For certain common frame rates, the function gets the frame duration from a look-up table:
  //   Frames per second (floating point)     Frames per second (fractional)     Average time per frame
  //   59.94                                  60000/1001                         166833
  //   29.97                                  30000/1001                         333667
  //   23.976                                 24000/1001                         417188
  //   60                                     60/1                               166667
  //   30                                     30/1                               333333
  //   50                                     50/1                               200000
  //   25                                     25/1                               400000
  //   24                                     24/1                               416667

  function MFFrameRateToAverageTimePerFrame(unNumerator: UINT32; // The numerator of the frame rate.
                                            unDenominator: UINT32; // The denominator of the frame rate.
                                            out punAverageTimePerFrame: UINT64): HRESULT; stdcall; // Receives the average duration of a video frame, in 100-nanosecond units.
  {$EXTERNALSYM MFFrameRateToAverageTimePerFrame}

  // Calculates the frame rate, in frames per second, from the average duration of a video frame.
  // Remarks
  //  Average time per frame is used in the older VIDEOINFOHEADER and VIDEOINFOHEADER2 format structures.
  //  This function provides a standard conversion so that all components in the pipeline can use consistent values,
  //  if they need to translate between the older format structures and the media type attributes used in Media Foundation.
  //  This function uses a look-up table for certain common durations.
  //  The table is listed in the Remarks section for the MFFrameRateToAverageTimePerFrame function.
  function MFAverageTimePerFrameToFrameRate(unAverageTimePerFrame: UINT64;  // The average duration of a video frame, in 100-nanosecond units.
                                            out punNumerator: UINT32; // Receives the numerator of the frame rate.
                                            out punDenominator: UINT32): HRESULT; stdcall; // Receives the denominator of the frame rate.
  {$EXTERNALSYM MFAverageTimePerFrameToFrameRate}

  function MFInitMediaTypeFromMFVideoFormat(pMFType: IMFMediaType;
                                            pMFVF: PMFVIDEOFORMAT;
                                            cbBufSize: UINT32): HRESULT; stdcall;
  {$EXTERNALSYM MFInitMediaTypeFromMFVideoFormat}

  function MFInitMediaTypeFromWaveFormatEx(pMFType: IMFMediaType;
                                           pWaveFormat: PWAVEFORMATEX;
                                           cbBufSize: UINT32): HRESULT; stdcall;
  {$EXTERNALSYM MFInitMediaTypeFromWaveFormatEx}

  function MFInitMediaTypeFromAMMediaType(pMFType: IMFMediaType;
                                          pAMType: AM_MEDIA_TYPE): HRESULT; stdcall;
  {$EXTERNALSYM MFInitMediaTypeFromAMMediaType}

  function MFInitAMMediaTypeFromMFMediaType(pMFType: IMFMediaType;
                                            const guidFormatBlockType: TGUID;
                                            var pAMType: AM_MEDIA_TYPE): HRESULT; stdcall;
  {$EXTERNALSYM MFInitAMMediaTypeFromMFMediaType}

  function MFCreateAMMediaTypeFromMFMediaType(pMFType: IMFMediaType;
                                              guidFormatBlockType: TGUID;
                                              ppAMType: PAM_MEDIA_TYPE // delete with DeleteMediaType
                                             ): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateAMMediaTypeFromMFMediaType}

  function MFCompareFullToPartialMediaType(pMFTypeFull: IMFMediaType;
                                           pMFTypePartial: IMFMediaType): BOOL; stdcall;
  {$EXTERNALSYM MFCompareFullToPartialMediaType}
  // This function compares a full media type to a partial media type.
  //
  // A "partial" media type is one that is given out by a component as a possible
  // media type it could accept. Many attributes may be unset, which represents
  // a "don't care" status for that attribute.
  //
  // For example, a video effect may report that it supports YV12,
  // but not want to specify a particular size. It simply creates a media type and sets
  // the major type to MFMediaType_Video and the subtype to MEDIASUBTYPE_YV12.
  //
  // The comparison function succeeds if the partial type contains at least a major type,
  // and all of the attributes in the partial type exist in the full type and are set to
  // the same value.


  function MFWrapMediaType(pOrig: IMFMediaType;
                           const majortype: TGUID;
                           const subtype: TGUID;
                           out ppWrap: IMFMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFWrapMediaType}

  function MFUnwrapMediaType(pWrap: IMFMediaType;
                             out ppOrig: IMFMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFUnwrapMediaType}



  // MFCreateVideoMediaType
  //=======================

  function MFCreateVideoMediaTypeFromVideoInfoHeader(pVideoInfoHeader: VIDEOINFOHEADER;
                                                     cbVideoInfoHeader: DWORD;
                                                     dwPixelAspectRatioX: DWORD;
                                                     dwPixelAspectRatioY: DWORD;
                                                     InterlaceMode: MFVideoInterlaceMode;
                                                     VideoFlags: QWORD;
                                                     const pSubtype: TGUID;
                                                     out ppIVideoMediaType: IMFVideoMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMediaTypeFromVideoInfoHeader}
  // Creates a media type from a KS_VIDEOINFOHEADER structure.
  // Parameters
  // pVideoInfoHeader
  //    Pointer to the KS_VIDEOINFOHEADER structure to convert. (This structure is identical to the DirectShow VIDEOINFOHEADER structure.)
  // cbVideoInfoHeader
  //    Size of the KS_VIDEOINFOHEADER structure in bytes.
  // dwPixelAspectRatioX
  //    The X dimension of the pixel aspect ratio. The pixel aspect ratio is dwPixelAspectRatioX:dwPixelAspectRatioY.
  // dwPixelAspectRatioY
  //    The Y dimension of the pixel aspect ratio.
  // InterlaceMode
  //    Member of the MFVideoInterlaceMode enumeration that specifies how the video is interlaced.
  // VideoFlags
  //    Bitwise OR of flags from the MFVideoFlags enumeration.
  // pSubtype
  //    Pointer to a subtype GUID. This parameter can be NULL. If the subtype GUID is specified, the function uses it to set the media subtype. Otherwise, the function attempts to deduce the subtype from the biCompression field contained in the KS_VIDEOINFOHEADER structure.
  // ppIVideoMediaType
  //    Receives a pointer to the IMFVideoMediaType interface. The caller must release the interface.
  //
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  //
  // Remarks
  //    Note  Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll, and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFCreateVideoMediaTypeFromVideoInfoHeader2(PVIDEOINFOHEADER: VIDEOINFOHEADER2;
                                                      cbVideoInfoHeader: DWORD;
                                                      AdditionalVideoFlags: QWORD;
                                                      const pSubtype: TGUID;
                                                      out ppIVideoMediaType: IMFVideoMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMediaTypeFromVideoInfoHeader2}
  // Creates a media type from a KS_VIDEOINFOHEADER2 structure.
  // Parameters
  // pVideoInfoHeader
  //    Pointer to the KS_VIDEOINFOHEADER2 structure to convert. (This structure is identical to the DirectShow VIDEOINFOHEADER2 structure.)
  // cbVideoInfoHeader
  //    Size of the KS_VIDEOINFOHEADER2 structure in bytes.
  // AdditionalVideoFlags
  //    Bitwise OR of flags from the MFVideoFlags enumeration.
  //    Use this parameter for format information that is not contained in the KS_VIDEOINFOHEADER2 structure.
  // pSubtype
  //    Pointer to a subtype GUID. This parameter can be NULL. If the subtype GUID is specified, the function uses it to set the media subtype.
  //    Otherwise, the function attempts to deduce the subtype from the biCompression field contained in the KS_VIDEOINFOHEADER2 structure.
  // ppIVideoMediaType
  //    Receives a pointer to the IMFVideoMediaType interface. The caller must release the interface.
  //
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  //
  // Remarks
  //    Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll, and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFCreateVideoMediaType(pVideoFormat: MFVIDEOFORMAT;
                                  out ppIVideoMediaType: IMFVideoMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMediaType}
  //
  // [This API is not supported and may be altered or unavailable in the future.
  // Applications should avoid using the MFVIDEOFORMAT structure, and use media type attributes instead.
  // For more information, see Video Media Types.]
  //
  //Creates a video media type from an MFVIDEOFORMAT structure.
  //Parameters
  // pVideoFormat [in]
  //    Pointer to an MFVIDEOFORMAT structure that describes the video format.
  // ppIVideoMediaType [out]
  //    Receives a pointer to the IMFVideoMediaType interface. The caller must release the interface.
  //
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  //
  // Remarks
  //    Instead of using the MFVIDEOFORMAT structure to initialize a video media type,
  //    you can call MFCreateMediaType and set the media type attributes directly.
  //
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll, and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFCreateVideoMediaTypeFromSubtype(const pAMSubtype: TGUID;
                                             out ppIVideoMediaType: IMFVideoMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMediaTypeFromSubtype}
  // Creates a partial video media type with a specified subtype.
  // Parameters
  // pAMSubtype [in]
  //    Pointer to a GUID that specifies the subtype. See Video Subtype GUIDs.
  // ppIVideoMediaType [out]
  //    Receives a pointer to the IMFVideoMediaType interface. The caller must release the interface.
  //
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  //
  // Remarks
  //    This function creates a media type and sets the major type equal to MFMediaType_Video and the subtype equal to the value specified in pAMSubtype.
  //    You can get the same result with the following steps:
  //    1 - Call MFCreateMediaType. This function returns a pointer to the IMFMediaType interface.
  //    2 - Set the MF_MT_MAJOR_TYPE attribute to MFMediaType_Video.
  //    3 - Set the MF_MT_SUBTYPE attribute to the subtype.
  //
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll, and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFIsFormatYUV(const Format: DWORD): BOOL; stdcall;
  {$EXTERNALSYM MFIsFormatYUV}
  // Queries whether a FOURCC code or D3DFORMAT value is a YUV format.
  // Parameters
  // Format [in]
  //    FOURCC code or D3DFORMAT value.
  //
  // Return value
  //  The function returns one of the following values.
  //    Return code     Description
  //    TRUE            The value specifies a YUV format.
  //    FALSE           The value does not specify a recognized YUV format.
  //
  // Remarks
  //    This function checks whether Format specifies a YUV format.
  //    Not every YUV format is recognized by this function. However,
  //    if a YUV format is not recognized by this function,
  //    it is probably not supported for video rendering or DirectX video acceleration (DXVA).


  // These depend on BITMAPINFOHEADER being defined
  //===============================================

  function MFCreateVideoMediaTypeFromBitMapInfoHeader(pbmihBitMapInfoHeader: BITMAPINFOHEADER;
                                                      dwPixelAspectRatioX: DWORD;
                                                      dwPixelAspectRatioY: DWORD;
                                                      InterlaceMode: MFVideoInterlaceMode;
                                                      VideoFlags: QWORD;
                                                      qwFramesPerSecondNumerator: QWORD;
                                                      qwFramesPerSecondDenominator: QWORD;
                                                      dwMaxBitRate: DWORD;
                                                      out ppIVideoMediaType: IMFVideoMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMediaTypeFromBitMapInfoHeader}
  // This function is not implemented.
  //
  // Parameters
  // pbmihBitMapInfoHeader
  //    Reserved.
  // dwPixelAspectRatioX
  //    Reserved.
  // dwPixelAspectRatioY
  //    Reserved.
  // InterlaceMode
  //    Reserved.
  // VideoFlags
  //    Reserved.
  // qwFramesPerSecondNumerator
  //    Reserved.
  // qwFramesPerSecondDenominator
  //    Reserved.
  // dwMaxBitRate
  //    Reserved.
  // ppIVideoMediaType
  //    Reserved.
  //
  // Return value
  //    Returns E_FAIL.
  //
  // Remarks
  //    Note  Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFGetStrideForBitmapInfoHeader(Format: DWORD;
                                          dwWidth: DWORD;
                                          out pStride: LONG): HRESULT; stdcall;
  {$EXTERNALSYM MFGetStrideForBitmapInfoHeader}
  // Calculates the minimum surface stride for a video format.
  // Parameters
  // format [in]
  //    FOURCC code or D3DFORMAT value that specifies the video format. If you have a video subtype GUID, you can use the first DWORD of the subtype.
  // dwWidth [in]
  //    Width of the image, in pixels.
  // pStride [out]
  //    Receives the minimum surface stride, in pixels.
  //
  // Return value
  // If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  //
  // Remarks
  //    This function calculates the minimum stride needed to hold the image in memory.
  //    Use this function if you are allocating buffers in system memory.
  //    Surfaces allocated in video memory might require a larger stride, depending on the graphics card.
  //    If you are working with a DirectX surface buffer, use the IMF2DBuffer.Lock2D method to find the surface stride.
  //    For planar YUV formats, this function returns the stride for the Y plane.
  //    Depending on the format, the chroma planes might have a different stride.
  //
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFGetPlaneSize(Format: DWORD;
                          dwWidth: DWORD;
                          dwHeight: DWORD;
                          out pdwPlaneSize: DWORD): HRESULT; stdcall;
  {$EXTERNALSYM MFGetPlaneSize}
  // Retrieves the image size, in bytes, for an uncompressed video format.
  // Parameters
  // format [in]
  //    FOURCC code or D3DFORMAT value that specifies the video format.
  // dwWidth [in]
  //    Width of the image, in pixels.
  // dwHeight [in]
  //    Height of the image, in pixels.
  // pdwPlaneSize [out]
  //    Receives the size of one frame, in bytes.
  //    If the format is compressed or is not recognized, this value is zero.
  //
  // Remarks
  //    This function is equivalent to the MFCalculateImageSize function.
  //
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.

// #if (WINVER >= _WIN32_WINNT_WIN7)

  // MFCreateVideoMediaTypeFromBitMapInfoHeaderEx
  //=============================================


  function MFCreateVideoMediaTypeFromBitMapInfoHeaderEx(pbmihBitMapInfoHeader: BITMAPINFOHEADER;
                                                        cbBitMapInfoHeader: UINT32;
                                                        dwPixelAspectRatioX: DWORD;
                                                        dwPixelAspectRatioY: DWORD;
                                                        InterlaceMode: MFVideoInterlaceMode;
                                                        VideoFlags: QWORD;
                                                        dwFramesPerSecondNumerator: DWORD;
                                                        dwFramesPerSecondDenominator: DWORD;
                                                        dwMaxBitRate: DWORD;
                                                        out ppIVideoMediaType: IMFVideoMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateVideoMediaTypeFromBitMapInfoHeaderEx}
  // Creates a video media type from a BITMAPINFOHEADER structure.
  // Parameters
  // pbmihBitMapInfoHeader [in]
  //    A pointer to the BITMAPINFOHEADER structure to convert.
  // cbBitMapInfoHeader [in]
  //    The size of the BITMAPINFOHEADER structure in bytes, including the size of any palette entries or color masks that follow the structure.
  // dwPixelAspectRatioX
  //    The X dimension of the pixel aspect ratio.
  // dwPixelAspectRatioY
  //    The Y dimension of the pixel aspect ratio.
  // InterlaceMode
  //    A member of the MFVideoInterlaceMode enumeration, specifying how the video is interlaced.
  // VideoFlags
  //    A bitwise OR of flags from the MFVideoFlags enumeration.
  // dwFramesPerSecondNumerator
  //    The numerator of the frame rate in frames per second.
  // dwFramesPerSecondDenominator
  //    The denominator of the frame rate in frames per second
  // dwMaxBitRate
  //    The approximate data rate of the video stream, in bits per second. If the rate is unknown, set this parameter to zero.
  // ppIVideoMediaType [out]
  //    Receives a pointer to the IMFVideoMediaType interface. The caller must release the interface.

//#endif // (WINVER >= _WIN32_WINNT_WIN7)


  // MFCreateMediaTypeFromRepresentation
  //====================================

  function MFCreateMediaTypeFromRepresentation(const guidRepresentation: TGUID;
                                               pvRepresentation: Pointer;
                                               out ppIMediaType: IMFMediaType): HRESULT; stdcall;
  {$EXTERNALSYM MFCreateMediaTypeFromRepresentation}

  // MFCreateAudioMediaType
  //=======================


  function MFCreateAudioMediaType(pAudioFormat: WAVEFORMATEX;
                                  out ppIAudioMediaType: IMFAudioMediaType): HResult; stdcall;
  {$EXTERNALSYM MFCreateAudioMediaType}

  function MFGetUncompressedVideoFormat(pVideoFormat: MFVIDEOFORMAT): DWORD; stdcall;
  {$EXTERNALSYM MFGetUncompressedVideoFormat}



  function MFInitVideoFormat(var pVideoFormat: MFVIDEOFORMAT;
                             _type: MFStandardVideoFormat): HResult; stdcall;
  {$EXTERNALSYM MFInitVideoFormat}
  // the parameter "type" is a Delphi reserved word, so,
  // this function needs a re-definition.
  // In many cases translations like this are done by adding a leading underscore.
  // This will only work if it's defined in a header like this:
  // _IsSomeThing ... And ended with IsSomeThing = _IsSomeThing. If this is not the case,
  // you better assume it's not implemented. For vars this should not be a problem, because they are passed by value only.
  // If this is the case with a functionname or function thats a part of an interface,
  // it's save to use the Delphi 'name mangling solution' - Exports - :
  // Example:  exports _theFunction name 'theFunction';

  function MFInitVideoFormat_RGB(pVideoFormat: MFVIDEOFORMAT;
                                 dwWidth: DWORD;
                                 dwHeight: DWORD;
                                 D3Dfmt: DWORD): HResult; stdcall;
  {$EXTERNALSYM MFInitVideoFormat_RGB}
  // [This API is not supported and may be altered or unavailable in the future.
  // Applications should avoid using the MFVIDEOFORMAT structure,
  // and use media type attributes instead. For more information, see Video Media Types.]
  // Initializes an MFVIDEOFORMAT structure for an uncompressed RGB video format.
  //Parameters
  // pVideoFormat [in]
  //    A pointer to an MFVIDEOFORMAT structure. The functions fills in the structure members with the format information.
  // dwWidth [in]
  //    The width of the video, in pixels.
  // dwHeight [in]
  //    The height of the video, in pixels.
  // D3Dfmt [in]
  //    A D3DFORMAT value that specifies the RGB format.
  // Remarks
  //    This function fills in some reasonable default values for the specified RGB format.
  //    Developers are encouraged to use media type attributes instead of using the MFVIDEOFORMAT structure.
  //    See Media Type Attributes.
  //    In general, you should avoid calling this function.
  //    If you know all of the format details, you can fill in the MFVIDEOFORMAT structure without this function.
  //    If you do not know all of the format details, attributes are preferable to using the MFVIDEOFORMAT structure.
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.


  function MFConvertColorInfoToDXVA(var pdwToDXVA: DWORD;
                                    var pFromFormat: MFVIDEOFORMAT): HResult; stdcall;
  {$EXTERNALSYM MFConvertColorInfoToDXVA}
  // [This API is not supported and may be altered or unavailable in the future.
  // Applications should avoid using the MFVIDEOFORMAT structure, and use media type attributes instead.
  // For more information, see Extended Color Information.]
  // Converts the extended color information from an MFVIDEOFORMAT to the equivalent
  // DirectX Video Acceleration (DXVA) color information.
  // Parameters
  // pdwToDXVA [out]
  //    Receives the DXVA extended color information. The bitfields in the DWORD are defined in the DXVA2_ExtendedFormat structure.
  // pFromFormat [in]
  //    Pointer to an MFVIDEOFORMAT structure that describes the video format.
  // Remarks
  //  Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFConvertColorInfoFromDXVA(out pToFormat: MFVIDEOFORMAT;
                                      dwFromDXVA: DWORD): HResult; stdcall;
  {$EXTERNALSYM MFConvertColorInfoFromDXVA}
  // [This API is not supported and may be altered or unavailable in the future.
  // Applications should avoid using the MFVIDEOFORMAT structure, and use media type attributes instead.
  // For more information, see Extended Color Information.]
  // Sets the extended color information in a MFVIDEOFORMAT structure.
  // Parameters
  // pToFormat [in, out]
  //    Pointer to an MFVIDEOFORMAT structure. The function fills in the structure members that correspond to the DXVA color information in the dwFromDXVA parameter. The function does not modify the other structure members.
  // dwFromDXVA [in]
  //    DWORD that contains extended color information. The bitfields in the DWORD are defined in the DXVA2_ExtendedFormat structure.
  // Remarks
  //    This function sets the following fields in the MFVIDEOFORMAT structure.
  //      - videoInfo.MFNominalRange
  //      - videoInfo.MFVideoLighting
  //      - videoInfo.MFVideoPrimaries
  //      - videoInfo.MFVideoTransferFunction
  //      - videoInfo.MFVideoTransferMatrix
  //      - videoInfo.SourceChromaSubsampling
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.


  // Optimized stride copy function
  // ==============================

  function MFCopyImage(pDest: PByte;
                       lDestStride: LONG;
                       pSrc: PByte;
                       lSrcStride: LONG;
                       dwWidthInBytes: DWORD;
                       dwLines: DWORD): HRESULT; stdcall;
   {$EXTERNALSYM MFCopyImage}
 // Copies an image or image plane from one buffer to another.
  // Parameters
  // pDest [in]
  //    Pointer to the start of the first row of pixels in the destination buffer.
  // lDestStride [in]
  //    Stride of the destination buffer, in bytes.
  // pSrc [in]
  //    Pointer to the start of the first row of pixels in the source image.
  // lSrcStride [in]
  //    Stride of the source image, in bytes.
  // dwWidthInBytes [in]
  //    Width of the image, in bytes.
  // dwLines [in]
  //    Number of rows of pixels to copy.
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise,
  //    it returns an HRESULT error code.
  // Remarks
  //    This function copies a single plane of the image.
  //    For planar YUV formats, you must call the function once for each plane.
  //    In this case, pDest and pSrc must point to the start of each plane.
  //
  //    This function is optimized if the MMX, SSE, or SSE2 instruction sets are available on the processor.
  //    The function performs a non-temporal store (the data is written to memory directly without polluting the cache).
  //
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.


  function MFConvertFromFP16Array(out pDest: PSingle;
                                  pSrc: PWORD;
                                  dwCount: DWORD): HRESULT; stdcall;
  {$EXTERNALSYM MFConvertFromFP16Array}
  // Converts an array of 16-bit floating-point numbers into an array of 32-bit floating-point numbers.
  // Parameters
  // pDest [in]
  //    Pointer to an array of float values. The array must contain at least dwCount elements.
  // pSrc [in]
  //    Pointer to an array of 16-bit floating-point values, typed as WORD values. The array must contain at least dwCount elements.
  // dwCount [in]
  //    Number of elements in the pSrc array to convert.
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  // Remarks
  //    The function converts dwCount values in the pSrc array and writes them into the pDest array.
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.


  function MFConvertToFP16Array({out} pDest: PWORD;
                                pSrc: PWORD;
                                dwCount: DWORD): HRESULT; stdcall;
  {$EXTERNALSYM MFConvertToFP16Array}
  // Converts an array of 32-bit floating-point numbers into an array of 16-bit floating-point numbers.
  // Parameters
  // pDest [out]
  //    Array of 16-bit floating-point values, typed as WORD values.
  //    The array must contain at least dwCount elements.
  // pSrc [in]
  //    Array of float values. The array must contain at least dwCount elements.
  // dwCount [in]
  //    Number of elements in the pSrc array to convert.
  // Remarks
  //    The function converts the values in the pSrc array and writes them into the pDest array.
  //
  // Note
  //    Prior to Windows 7, this function was exported from evr.dll.
  //    Starting in Windows 7, this function is exported from mfplat.dll,
  //    and evr.dll exports a stub function that calls into mfplat.dll.
  //    For more information, see Library Changes in Windows 7.

  function MFCreate2DMediaBuffer(dwWidth: DWORD;
                                 dwHeight: DWORD;
                                 dwFourCC: DWORD;
                                 fBottomUp: BOOL;
                                 out ppBuffer: IMFMediaBuffer): HRESULT; stdcall;
  {$EXTERNALSYM MFCreate2DMediaBuffer}
  // Creates a system-memory buffer object to hold 2D image data.
  // Parameters
  // dwWidth [in]
  //    Width of the image, in pixels.
  // dwHeight [in]
  //    Height of the image, in pixels.
  // dwFourCC [in]
  //    A FOURCC code or D3DFORMAT value that specifies the video format.
  //    If you have a video subtype GUID, you can use the first DWORD of the subtype.
  // fBottomUp [in]
  //    If TRUE, the buffer's IMF2DBuffer.ContiguousCopyTo method copies the buffer into a bottom-up format.
  //    The bottom-up format is compatible with GDI for uncompressed RGB images.
  //    If this parameter is FALSE, the ContiguousCopyTo method copies the buffer into a top-down format,
  //    which is compatible with DirectX.
  //    For more information about top-down versus bottom-up images, see Image Stride.
  // ppBuffer [out]
  //    Receives a pointer to the IMFMediaBuffer interface.
  // Return value
  //    This function can return one of these values.
  // Return code              Description
  // S_OK                     Success.
  // MF_E_INVALIDMEDIATYPE    Unrecognized video format.
  //
  // Remarks
  //    The returned buffer object also exposes the IMF2DBuffer2 interface.


  function MFCreateMediaBufferFromMediaType(pMediaType: IMFMediaType;
                                            llDuration: LONGLONG;   // Sample Duration, needed for audio
                                            dwMinLength: DWORD;     // 0 means optimized default
                                            dwMinAlignment: DWORD;  // 0 means optimized default
                                            out ppBuffer: IMFMediaBuffer): HResult; stdcall;
  {$EXTERNALSYM MFCreateMediaBufferFromMediaType}
  // Allocates a system-memory buffer that is optimal for a specified media type.
  // Parameters
  // pMediaType [in]
  //    A pointer to the IMFMediaType interface of the media type.
  // llDuration [in]
  //    The sample duration. This value is required for audio formats.
  // dwMinLength [in]
  //    The minimum size of the buffer, in bytes. The actual buffer size might be larger.
  //    Specify zero to allocate the default buffer size for the media type.
  // dwMinAlignment [in]
  //    The minimum memory alignment for the buffer. Specify zero to use the default memory alignment.
  // ppBuffer [out]
  //    Receives a pointer to the IMFMediaBuffer interface. The caller must release the interface.
  // Return value
  //    If this function succeeds, it returns S_OK. Otherwise, it returns an HRESULT error code.
  // Remarks
  //    For video formats, if the format is recognized,
  //    the function creates a 2-D buffer that implements the IMF2DBuffer2 interface.
  //    Otherwise it creates a linear buffer. To get the IMF2DBuffer2 interface,
  //    call QueryInterface on the pointer returned in ppBuffer.
  //    If the QueryInterface method fails, use the IMFMediaBuffer interface to access the buffer memory.
  //    For audio formats, the function allocates a buffer that is large enough to contain llDuration audio samples,
  //    or dwMinLength, whichever is larger.
  //    This function always allocates system memory.
  //    For Direct3D surfaces, use the MFCreateDXGISurfaceBuffer or MFCreateDXSurfaceBuffer function.


/////////////////////  Attributes Utility functions ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  // IMFAttributes inline UTILITY FUNCTIONS - used for IMFMediaType as well
  //=======================================================================


// IMFAttributes inline UTILITY FUNCTIONS - used for IMFMediaType as well //////
// Note: these are internally methodes


  function HI32(unPacked: UINT64): UINT32; inline;
  {$EXTERNALSYM HI32}

  function LO32(unPacked: UINT64): UINT32; inline;
  {$EXTERNALSYM LO32}
  // return (UINT32)unPacked;

  function Pack2UINT32AsUINT64(unHigh: UINT32;
                               unLow: UINT32): UINT64; inline;
  {$EXTERNALSYM Pack2UINT32AsUINT64}
  // Packs two UINT32 values into a UINT64 value.
  // Returns the packed UINT64 value.
  // Parameters
  //==========
  // unHigh [in]
  //    Value to store in the high-order 32 bits of the UINT64 value.
  // unLow [in]
  //    Value to store in the low-order 32 bits of the UINT64 value.
  // Return value
  //    Returns the packed UINT64 value.
  //    ((UINT64)unHigh << 32) | unLow;
  // NOTE: This function stores two 32-bit values in a 64-bit value that is suitable for
  //       the IMFAttributes.SetUINT64 method.


  // Gets the low-order and high-order UINT32 values from a UINT64 value.
  // This procedure does not return a value.

  procedure Unpack2UINT32AsUINT64(unPacked: UINT64;
                                  out punHigh: UINT32;
                                  out punLow: UINT32); inline;
  {$EXTERNALSYM Unpack2UINT32AsUINT64}
  // Parameters
  // ==========
  // unPacked [in]
  //    The value to convert.
  // punHigh [out]
  //    Receives the high-order 32 bits.
  // punLow [out]
  //    Receives the low-order 32 bits.



  // Packs a UINT32 width value and a UINT32 height value into a UINT64 value that represents a size.
  // Returns the packed UINT64 value.
  // NOTE: This function stores two 32-bit values in a 64-bit value that is suitable for
  //       the IMFAttributes.SetUINT64 method.
  function PackSize(unWidth: UINT32;
                    unHeight: UINT32): UINT64; inline;
  {$EXTERNALSYM PackSize}
  //Parameters
  //==========
  // unWidth [in]
  //    Value to store the UINT32 width value.
  // unHeight [in]
  //    Value to store the UINT32 height value.
  // Return value
  //    Returns the packed UINT64 value.



  // Gets the low-order and high-order UINT32 values from a UINT64 value that represnets a size.
  // You can use this function to unpack a UINT64 value that you receive from the IMFAttributes.GetUINT64 method.
  procedure UnpackSize(unPacked: UINT64;
                       out punWidth: UINT32;
                       out punHeight: UINT32); inline;
  {$EXTERNALSYM UnpackSize}
  //Parameters
  //==========
  // unPacked [in]
  //    The value to convert.
  // punWidth [out]
  //    Receives the high-order 32 bits.
  // punHeight [out]
  //    Receives the low-order 32 bits.



  // Packs two UINT32 values, which represent a ratio, into a UINT64 value.
  // Returns the packed UINT64 value.
  // NOTE: This function stores two 32-bit values in a 64-bit value that is suitable for
  //       the IMFAttributes.SetUINT64 method.
  function PackRatio(nNumerator: INT32;
                     unDenominator: UINT32): UINT64; inline;
  {$EXTERNALSYM PackRatio}
  //Parameters
  //==========
  // nNumerator [in]
  //    Value to store the UINT32 numerator value.
  // unDenominator [in]
  //    Value to store the UINT32 denominator value.
  // Return value
  //    Returns the packed UINT64 value.


  // Gets the low-order and high-order UINT32 values from a UINT64 value that represents a ratio.
  // NOTE: You can use this function to unpack a UINT64 value that you receive from
  //       the IMFAttributes.GetUINT64 method.
  procedure UnpackRatio(unPacked: UINT64;
                        out pnNumerator: UINT32;
                        out punDenominator: UINT32); inline;
  {$EXTERNALSYM UnpackRatio}
  // Parameters
  // ==========
  // unPacked [in]
  //    The value to convert.
  // pnNumerator [out]
  //    Receives the high-order 32 bits.
  // punDenominator [out]
  //    Receives the low-order 32 bits.


  // "failsafe" inline get methods - return the stored value or return a default /


  // Returns a UINT32 value from an attribute store, or a default value if the attribute is not present.
  // NOTE: This helper function queries the attribute store for the UINT32 value specified by guidKey.
  //       If the value is not present or does not have type UINT32, the function returns unDefault.
  function MFGetAttributeUINT32(pAttributes: IMFAttributes;
                                guidKey: TGUID;
                                unDefault: UINT32): UINT32; inline;
  {$EXTERNALSYM MFGetAttributeUINT32}
  // Parameters
  // ==========
  // pAttributes [in]
  //    Pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    GUID that identifies which value to retrieve.
  // unDefault [in]
  //    Default value to return if the attribute store does not contain the specified attribute.


  // Returns a UINT64 value from an attribute store, or a default value if the attribute is not present.
  // NOTE: This helper function queries the attribute store for the UINT64 value specified by guidKey.
  //       If the value is not present, the function returns unDefault.
  function MFGetAttributeUINT64(pAttributes: IMFAttributes;
                                guidKey: TGUID;
                                unDefault: UINT64): UINT64; inline;
  {$EXTERNALSYM MFGetAttributeUINT64}
  // Parameters
  // ==========
  // pAttributes [in]
  //    Pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    GUID that identifies which value to retrieve.
  // unDefault [in]
  //    Default value to return if the attribute store does not contain the specified attribute.


  // Returns a double value from an attribute store, or a default value if the attribute is not present.
  // NOTE: This helper function queries the attribute store for the attribute specified by guidKey.
  //       If the attribute is not present or does not have type double, the function returns fDefault.
  //       This function is convenient because it never returns a failure code.
  //       However, if the attribute in question does not have a meaningful default value,
  //       you should call IMFAttributes.GetDouble and check for MF_E_ATTRIBUTENOTFOUND.
  function MFGetAttributeDouble(pAttributes: IMFAttributes;
                                guidKey: TGUID;
                                fDefault: Double ): Double; inline;
  {$EXTERNALSYM MFGetAttributeDouble}
  // Parameters
  // ==========
  // pAttributes [in]
  //    Pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    GUID that identifies which value to retrieve.
  // fDefault [in]
  //    Default value to return if the attribute store does not contain the specified attribute.



  // helpers for getting/setting ratios and sizes //////////////////////////////
  //============================================================================


  // Gets an attribute whose value is two UINT32 values packed into a UINT64.
  // NOTE: Internally, this function calls IMFAttributes.GetUINT64 to get the UINT64 value,
  //       and Unpack2UINT32AsUINT64 to unpack the two 32-bit values.
  function MFGetAttribute2UINT32asUINT64(pAttributes: IMFAttributes;
                                         guidKey: TGUID;
                                         out punHigh32: UINT32;
                                         out punLow32: UINT32): HResult; inline;
  {$EXTERNALSYM MFGetAttribute2UINT32asUINT64}
  //  Unpack2UINT32AsUINT64(unPacked, punHigh32, punLow32);
  //
  //  return hr;
  //}
  // Parameters
  //===========
  // pAttributes [in]
  //    A pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    A GUID that identifies which value to retrieve. The attribute type must be MF_ATTRIBUTE_UINT64.
  // punHigh32 [out]
  //    Receives the high-order 32 bits.
  // punLow32 [out]
  //    Receives the low-order 32 bits.



  // Packs two UINT32 values into a UINT64 attribute value.
  // NOTE: Internally, this functions calls Pack2UINT32AsUINT64 to create the 64-bit value,
  //       and IMFAttributes.SetUINT64 to set the attribute.
  function MFSetAttribute2UINT32asUINT64(pAttributes: IMFAttributes;
                                         guidKey: TGUID;
                                         unHigh32: UINT32;
                                         unLow32: UINT32): HResult; inline;
  {$EXTERNALSYM MFSetAttribute2UINT32asUINT64}
  // Parameters
  //===========
  // pAttributes [in]
  //    A pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    A GUID that identifies the value to set. If this key already exists, the function overwrites the old value.
  // unHigh32 [in]
  //    The value to store in the high-order 32 bits of the UINT64 value.
  // unLow32 [in]
  //    The value to store in the low-order 32 bits of the UINT64 value.


  // Retrieves an attribute whose value is a ratio.
  // NOTE: Some attributes specify a ratio as a packed UINT64 value.
  //       Use this function to get the numerator and denominator as separate 32-bit values.
  function MFGetAttributeRatio(pAttributes: IMFAttributes;
                               guidKey: TGUID;
                               out punNumerator: UINT32;
                               out punDenominator: UINT32): HResult; inline;
  {$EXTERNALSYM MFGetAttributeRatio}
  // Parameters
  // ==========
  // pAttributes [in]
  //    Pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    GUID that identifies which value to retrieve. The attribute type must be MF_ATTRIBUTE_UINT64.
  // pnNumerator [out]
  //    Receives the numerator of the ratio.
  // punDenominator [out]
  //    Receives the denominator of the ratio.


  // Retrieves an attribute whose value is a size, expressed as a width and height.
  // NOTE: Some attributes specify a size as a packed UINT64 value.
  //       Use this function to get the numerator and denominator as separate 32-bit values.
  function MFGetAttributeSize(pAttributes: IMFAttributes;
                              guidKey: TGUID;
                              out punWidth: UINT32;
                              out punHeight: UINT32): HResult; inline;
  {$EXTERNALSYM MFGetAttributeSize}
  // Parameters
  // ==========
  // pAttributes [in]
  //    Pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    GUID that identifies which value to retrieve. The attribute type must be MF_ATTRIBUTE_UINT64.
  // punWidth [out]
  //    Receives the width.
  // punHeight [out]
  //    Receives the height.


  // Sets a ratio as a 64-bit attribute value.
  // NOTE: Some attributes specify a ratio as a packed UINT64 value.
  //       This function packs the numerator and denominator into a single UINT64 value.
  function MFSetAttributeRatio(pAttributes: IMFAttributes;
                               guidKey: TGUID;
                               unNumerator: UINT32;
                               unDenominator: UINT32): HResult; inline;
  {$EXTERNALSYM MFSetAttributeRatio}
  // Parameters
  // ==========
  // pAttributes [in]
  //    A pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    A GUID that identifies the value to set. If this key already exists, the function overwrites the old value.
  // unNumerator [in]
  //    The numerator of the ratio.
  // unDenominator [in]
  //    The denominator of the ratio.


  // Sets width and height as a single 64-bit attribute value.
  function MFSetAttributeSize(pAttributes: IMFAttributes;
                              guidKey: TGUID;
                              unWidth: UINT32;
                              unHeight: UINT32): HResult; inline;
  {$EXTERNALSYM MFSetAttributeSize}
  // Parameters
  // ==========
  // pAttributes [in]
  //    A pointer to the IMFAttributes interface of the attribute store.
  // guidKey [in]
  //    A GUID that identifies the value to set. If this key already exists, the function overwrites the old value.
  // unWidth [in]
  //    The width.
  // unHeight [in]
  //    The height.


  function MFGetAttributeString(pAttributes: IMFAttributes;
                                guidKey: TGUID;
                                out ppsz: LPWSTR): HResult; inline;
  {$EXTERNALSYM MFGetAttributeString}
  // Parameters
  // pAttributes [in]
  //    A pointer to the IMFAttributes interface.
  // guidKey [in]
  //    A GUID that identifies which value to retrieve. The attribute type must be MF_ATTRIBUTE_STRING.
  // ppsz [out]
  //    If the key is found and the value is a string type, this parameter receives a copy of the string.
  //    The caller must free the memory for the string by calling CoTaskMemFree.
  // Remarks
  //    This function is a wrapper for the IMFAttributes::GetAllocatedString method.


///////////////////////////////  Collection  ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


  // Instantiates the MF-provided IMFCollection implementation
  //==========================================================

  // Instantiates the MF-provided IMFCollection implementation
  function MFCreateCollection(out ppIMFCollection: IMFCollection): HResult; stdcall;
  {$EXTERNALSYM MFCreateCollection}
  // Creates an empty collection object.
  // Parameters
  // ppIMFCollection [out]
  //    Receives a pointer to the collection object's IMFCollection interface.
  //    The caller must release the interface.  (Tony: Use procedure SafeRelease declared in MfpUtils.pas)
  // Return value
  //    The function returns an HRESULT.
  //    Possible values include, but are not limited to, those in the following table.
  //    Return code            Description
  //    S_OK                  The method succeeded.


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////  Memory Management ////////////////////////////
////////////////////////////////////////////////////////////////////////////////


type
  // Heap alloc/free
  PEAllocationType = ^EAllocationType;
  _EAllocationType = (
    eAllocationTypeDynamic,
    {$EXTERNALSYM eAllocationTypeDynamic}
    eAllocationTypeRT,
    {$EXTERNALSYM eAllocationTypeRT}
    eAllocationTypePageable,
    {$EXTERNALSYM eAllocationTypePageable}
    eAllocationTypeIgnore
    {$EXTERNALSYM eAllocationTypeIgnore}
  );
  {$EXTERNALSYM _EAllocationType}
  EAllocationType = _EAllocationType;
  {$EXTERNALSYM EAllocationType}



  function MFHeapAlloc(nSize: size_t;
                       dwFlags: ULONG;
                       pszFile: PAnsiChar;
                       line: Integer;
                       eat: EAllocationType ): Pointer; stdcall;
  {$EXTERNALSYM MFHeapAlloc}
  // Allocates a block of memory.
  // Parameters
  // nSize [in]
  //    Number of bytes to allocate.
  // dwFlags [in]
  //    Zero or more flags. For a list of valid flags, see HeapAlloc in the Windows SDK documentation.
  // pszFile [in]
  //    Reserved. Set to Nil.
  // line [in]
  //    Reserved. Set to zero.
  // eat [in]
  //    Reserved. Set to eAllocationTypeIgnore.
  // Return value
  //    If the function succeeds, it returns a pointer to the allocated memory block.
  //    If the function fails, it returns Nil.
  // Remarks
  //    In the current version of Media Foundation,
  //    this function is equivalent to calling the HeapAlloc function and specifying
  //    the heap of the calling process.
  //    To free the allocated memory, call MFHeapFree.

  procedure MFHeapFree(pv: Pointer); stdcall;
  {$EXTERNALSYM MFHeapFree}
  // Frees a block of memory that was allocated by calling the MFHeapAlloc function.
  // Parameters
  // pv [in]
  //    Pointer to the memory to free.
  // Note
  //    This function does not return a value.


//////////////////////////       SourceResolver     ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

const
  CLSID_MFSourceResolver  : TGuid = '{90eab60f-e43a-4188-bcc4-e47fdf04868c}';
  {$EXTERNALSYM CLSID_MFSourceResolver}

//#if (WINVER >= _WIN32_WINNT_WIN7)

  function MFllMulDiv(a: LONGLONG;
                      b: LONGLONG;
                      c: LONGLONG;
                      d: LONGLONG): LONGLONG; stdcall;
  {$EXTERNALSYM MFllMulDiv}
  // Calculates ((a * b) + d) / c, where each term is a 64-bit signed value.
  // Parameters
  //
  // a
  //    A multiplier.
  // b
  //    Another multiplier.
  // c
  //    The divisor.
  // d
  //    The rounding factor.
  // Return value
  //    Returns the result of the calculation.
  //    If numeric overflow occurs, the function returns _I64_MAX (positive overflow) or
  //    LLONG_MIN (negative overflow).
  //    If Mfplat.dll cannot be loaded, the function returns _I64_MAX.
  // Remarks
  //    Note a previous version of this topic onn MSDN described the parameters incorrectly.
  //    The divisor is c and the rounding factor is d.

//#endif // (WINVER >= _WIN32_WINNT_WIN7)


//////////////////////////    Content Protection    ////////////////////////////
////////////////////////////////////////////////////////////////////////////////


  function MFGetContentProtectionSystemCLSID(guidProtectionSystemID: REFGUID;
                                             out pclsid: CLSID): HResult; stdcall;
  {$EXTERNALSYM MFGetContentProtectionSystemCLSID}


////////////////////////////////////////////////////////////////////////////////

const
// MF_DEVICESTREAM_ATTRIBUTE_FACEAUTH_CAPABILITY
// Data type: UINT64
// Represents the Capability field of the KSCAMERA_EXTENDEDPROP_HEADER corresponding to the
// KSPROPERTY_CAMERACONTROL_EXTENDED_FACEAUTH_MODE extended property control.  If this control
// is not supported, this attribute will not be present on the stream.
// The capability advertised will only contain the bitwise OR of the available
// supported modes defined by the Face Auth DDI in ksmedia.h:
//
//      KSCAMERA_EXTENDEDPROP_FACEAUTH_MODE_DISABLED
//      KSCAMERA_EXTENDEDPROP_FACEAUTH_MODE_ALTERNATIVE_FRAME_ILLUMINATION
//      KSCAMERA_EXTENDEDPROP_FACEAUTH_MODE_BACKGROUND_SUBTRACTION

 MF_DEVICESTREAM_ATTRIBUTE_FACEAUTH_CAPABILITY : TGUID = '{CB6FD12A-2248-4E41-AD46-E78BB90AB9FC}';
 {$EXTERNALSYM MF_DEVICESTREAM_ATTRIBUTE_FACEAUTH_CAPABILITY}
// MF_DEVICESTREAM_ATTRIBUTE_SECURE_CAPABILITY
// Data type: UINT64
// Represents the Capability field of the KSCAMERA_EXTENDEDPROP_HEADER corresponding to the
// KSPROPERTY_CAMERACONTROL_EXTENDED_SECURE_MODE extended property control.  If this control
// is not supported, this attribute will not be present on the stream.
// The capability advertised will only contain the bitwise OR of the available
// supported modes defined by the Secure DDI in ksmedia.h:
//
//      KSCAMERA_EXTENDEDPROP_SECURE_MODE_DISABLED
//      KSCAMERA_EXTENDEDPROP_SECURE_MODE_ENABLED

 MF_DEVICESTREAM_ATTRIBUTE_SECURE_CAPABILITY : TGUID = '{CB6FD12A-2248-4E41-AD46-E78BB90AB9FC}';
 {$EXTERNALSYM MF_DEVICESTREAM_ATTRIBUTE_SECURE_CAPABILITY}


// #if (NTDDI_VERSION >= NTDDI_WIN10_VB)

  // MFCombineSamples
  //  pSample - pointer to a sample to append/combine 'pSampleToAdd' to
  //  pSampleToAdd - the sample to append
  //  dwMaxMergedDurationInMS - indicates the maximum duration that the combined sample should be allowed to occupy
  //  pCombined - indicates that pSampleToAdd was successfully added to the base sample
  function MFCombineSamples(pSample: IMFSample;
                            pSampleToAdd: IMFSample;
                            dwMaxMergedDurationInMS: DWORD;
                            out pMerged: BOOL): HResult; stdcall;
  {$EXTERNALSYM MFCombineSamples}

  // MFSplitSample
  //  pSample - a single combined sample that should be split up
  //  pOutputSamples - output array of split samples
  //  dwOutputSampleMaxCount - maximum array size (use the BufferCount on pSample to find out an upper bound)
  //  pdwOutputSampleCount - actual number of output samples produce

  function MFSplitSample(pSample: PIMFSample;
                         pOutputSamples: PIMFSample;  // Out, writes to dwOutputSampleMaxCount and pdwOutputSampleCount
                         dwOutputSampleMaxCount: DWORD;
                         out pdwOutputSampleCount: DWORD): HResult; stdcall;
  {$EXTERNALSYM MFSplitSample}
//#endif // (NTDDI_VERSION >= NTDDI_WIN10_VB)


  // Additional Prototypes for ALL interfaces

  //// Delphi Helpers  /////////////////////////////////////////////////////////

  //See for usage:  function FCC(ch4: TCh4) and function DEFINE_MEDIATYPE_GUID(format: DWord)
  function DefineMediaTypeGuidByFourCC(sFcc: TCh4): TGuid; inline;

  //See for usage:  function FCC(ch4: TCh4) and function DEFINE_MEDIATYPE_GUID(format: DWord)
  function DefineMediaTypeGuidByDWord(dwConst: DWord = 0): TGuid; inline;

  // MOVE_RECT helper
  function MoveRect(const mR: MOVE_RECT): MFVideoNormalizedRect; inline;

  //////////////////////////////////////////////////////////////////////////////


  // End of Additional Prototypes

implementation


uses
  System.SysUtils;

  // Implement Additional Prototypes here.


function HI32(unPacked: UINT64): UINT32;
begin
  Result := unPacked shr 32;
end;

function LO32(unPacked: UINT64): UINT32;
begin
  Result := unPacked and $0ffffffff;
end;

function Pack2UINT32AsUINT64(unHigh: UINT32;
                             unLow: UINT32): UINT64;
begin
  Result := UINT64(unHigh) shl 32 or unLow;
end;

procedure Unpack2UINT32AsUINT64(unPacked: UINT64;
                                out punHigh: UINT32;
                                out punLow: UINT32);
begin
  punHigh := HI32(unPacked);
  punLow := LO32(unPacked);
end;


// Helper function to access the macro translations, mentioned under REMARK#1
function DefineMediaTypeGuidByFourCC(sFcc: TCh4): TGuid;
begin
  Result := DEFINE_MEDIATYPE_GUID(FCC(sFcc));
end;

// Helper function to access the macro translations, mentioned under REMARK#1
function DefineMediaTypeGuidByDWord(dwConst: DWord): TGuid;
begin
  Result := DEFINE_MEDIATYPE_GUID(dwConst);
end;


// MOVE_RECT helper
function MoveRect(const mR: MOVE_RECT): MFVideoNormalizedRect;
begin
  Result.Top := (mR.DestRect.Top * 1.0);
  Result.Bottom := (mR.DestRect.Bottom * 1.0);
  Result.Left := (mR.DestRect.Left * 1.0);
  Result.Right := (mR.DestRect.Right * 1.0);

  OffsetRect(Result,
             mR.SourcePoint.x,
             mR.SourcePoint.y);
end;


//--------------------- Macros converted to functions ------------------------


function FCC(ch4: TCh4): DWord; inline;
begin
  Result :=  DWord(Ord(ch4[0])) or
             DWord(Ord(ch4[1])) shl 8 or
             DWord(Ord(ch4[2])) shl 16 or
             DWord(Ord(ch4[3])) shl 24;
end;


function DEFINE_MEDIATYPE_GUID(const format: DWord): TGuid;
begin
  Result.D1 := format;
  Result.D2 := $0000;
  Result.D3 := $0010;
  Result.D4[0] := $80;
  Result.D4[1] := $00;
  Result.D4[2] := $00;
  Result.D4[3] := $aa;
  Result.D4[4] := $00;
  Result.D4[5] := $38;
  Result.D4[6] := $9b;
  Result.D4[7] := $71;

end;

//--------------------- External definitions ---------------------------------

const
  MfApiLibA   = 'Mfplat.dll';
  MfApiLibB   = '';  // reserved

{$WARN SYMBOL_PLATFORM OFF}
  function MFStartup;                 external MfApiLibA name 'MFStartup' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFShutdown;                external MfApiLibA name 'MFShutdown' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFLockPlatform;            external MfApiLibA name 'MFLockPlatform' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFUnlockPlatform;          external MfApiLibA name 'MFUnlockPlatform' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFPutWorkItem;             external MfApiLibA name 'MFPutWorkItem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFPutWorkItem2;            external MfApiLibA name 'MFPutWorkItem2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFPutWorkItemEx;           external MfApiLibA name 'MFPutWorkItemEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFPutWorkItemEx2;          external MfApiLibA name 'MFPutWorkItemEx2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFPutWaitingWorkItem;      external MfApiLibA name 'MFPutWaitingWorkItem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFAllocateSerialWorkQueue; external MfApiLibA name 'MFAllocateSerialWorkQueue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFScheduleWorkItem;        external MfApiLibA name 'MFScheduleWorkItem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFScheduleWorkItemEx;      external MfApiLibA name 'MFScheduleWorkItemEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCancelWorkItem;          external MfApiLibA name 'MFCancelWorkItem' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetTimerPeriodicity;     external MfApiLibA name 'MFGetTimerPeriodicity' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFAddPeriodicCallback;     external MfApiLibA name 'MFAddPeriodicCallback' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFRemovePeriodicCallback;  external MfApiLibA name 'MFRemovePeriodicCallback' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFAllocateWorkQueueEx;     external MfApiLibA name 'MFAllocateWorkQueueEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFAllocateWorkQueue;       external MfApiLibA name 'MFAllocateWorkQueue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFLockWorkQueue;           external MfApiLibA name 'MFLockWorkQueue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFUnlockWorkQueue;         external MfApiLibA name 'MFUnlockWorkQueue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFBeginRegisterWorkQueueWithMMCSS;   external MfApiLibA name 'MFBeginRegisterWorkQueueWithMMCSS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFBeginRegisterWorkQueueWithMMCSSEx; external MfApiLibA name 'MFBeginRegisterWorkQueueWithMMCSSEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFEndRegisterWorkQueueWithMMCSS;     external MfApiLibA name 'MFEndRegisterWorkQueueWithMMCSS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFBeginUnregisterWorkQueueWithMMCSS; external MfApiLibA name 'MFBeginUnregisterWorkQueueWithMMCSS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFEndUnregisterWorkQueueWithMMCSS;   external MfApiLibA name 'MFEndUnregisterWorkQueueWithMMCSS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFRegisterPlatformWithMMCSS;         external MfApiLibA name 'MFRegisterPlatformWithMMCSS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFUnregisterPlatformFromMMCSS;       external MfApiLibA name 'MFUnregisterPlatformFromMMCSS' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFLockSharedWorkQueue;       external MfApiLibA name 'MFLockSharedWorkQueue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetWorkQueueMMCSSPriority; external MfApiLibA name 'MFGetWorkQueueMMCSSPriority' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetWorkQueueMMCSSClass;    external MfApiLibA name 'MFGetWorkQueueMMCSSClass' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetWorkQueueMMCSSTaskId;   external MfApiLibA name 'MFGetWorkQueueMMCSSTaskId' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAsyncResult;         external MfApiLibA name 'MFCreateAsyncResult' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInvokeCallback;            external MfApiLibA name 'MFInvokeCallback' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateFile;                external MfApiLibA name 'MFCreateFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateTempFile;            external MfApiLibA name 'MFCreateTempFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFBeginCreateFile;           external MfApiLibA name 'MFBeginCreateFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFEndCreateFile;             external MfApiLibA name 'MFEndCreateFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCancelCreateFile;          external MfApiLibA name 'MFCancelCreateFile' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMemoryBuffer;        external MfApiLibA name 'MFCreateMemoryBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMediaBufferWrapper;  external MfApiLibA name 'MFCreateMediaBufferWrapper' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateLegacyMediaBufferOnMFMediaBuffer; external MfApiLibA name 'MFCreateLegacyMediaBufferOnMFMediaBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFMapDX9FormatToDXGIFormat;  external MfApiLibA name 'MFMapDX9FormatToDXGIFormat' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFMapDXGIFormatToDX9Format;  external MfApiLibA name 'MFMapDXGIFormatToDX9Format' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateDXSurfaceBuffer;     external MfApiLibA name 'MFCreateDXSurfaceBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFLockDXGIDeviceManager;     external MfApiLibA name 'MFLockDXGIDeviceManager' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateWICBitmapBuffer;     external MfApiLibA name 'MFCreateWICBitmapBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateDXGISurfaceBuffer;   external MfApiLibA name 'MFCreateDXGISurfaceBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoSampleAllocatorEx; external MfApiLibA name 'MFCreateVideoSampleAllocatorEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateDXGIDeviceManager;   external MfApiLibA name 'MFCreateDXGIDeviceManager' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateAlignedMemoryBuffer; external MfApiLibA name 'MFCreateAlignedMemoryBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMediaEvent;          external MfApiLibA name 'MFCreateMediaEvent' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateEventQueue;          external MfApiLibA name 'MFCreateEventQueue' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateSample;              external MfApiLibA name 'MFCreateSample' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAttributes;          external MfApiLibA name 'MFCreateAttributes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitAttributesFromBlob;    external MfApiLibA name 'MFInitAttributesFromBlob' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetAttributesAsBlobSize;   external MfApiLibA name 'MFGetAttributesAsBlobSize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetAttributesAsBlob;       external MfApiLibA name 'MFGetAttributesAsBlob' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTRegister;                 external MfApiLibA name 'MFTRegister' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTUnregister;               external MfApiLibA name 'MFTUnregister' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTRegisterLocal;            external MfApiLibA name 'MFTRegisterLocal' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTUnregisterLocal;          external MfApiLibA name 'MFTUnregisterLocal' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTRegisterLocalByCLSID;     external MfApiLibA name 'MFTRegisterLocalByCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTUnregisterLocalByCLSID;   external MfApiLibA name 'MFTUnregisterLocalByCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTEnum;                     external MfApiLibA name 'MFTEnum' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTEnum2;                    external MfApiLibA name 'MFTEnum2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTEnumEx;                   external MfApiLibA name 'MFTEnumEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFTGetInfo;                  external MfApiLibA name 'MFTGetInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetPluginControl;          external MfApiLibA name 'MFGetPluginControl' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetMFTMerit;               external MfApiLibA name 'MFGetMFTMerit' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFRegisterLocalSchemeHandler;         external MfApiLibA name 'MFRegisterLocalSchemeHandler' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFRegisterLocalByteStreamHandler;     external MfApiLibA name 'MFRegisterLocalByteStreamHandler' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMFByteStreamWrapper;          external MfApiLibA name 'MFCreateMFByteStreamWrapper' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMediaExtensionActivate;       external MfApiLibA name 'MFCreateMediaExtensionActivate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreateMuxStreamAttributes; external MfApiLibA name 'MFCreateMuxStreamAttributes' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMuxStreamMediaType;  external MfApiLibA name 'MFCreateMuxStreamMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMuxStreamSample;     external MfApiLibA name 'MFCreateMuxStreamSample' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFCreate2DMediaBuffer;                external MfApiLibA name 'MFCreate2DMediaBuffer' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMediaBufferFromMediaType;     external MfApiLibA name 'MFCreateMediaBufferFromMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};

  function MFValidateMediaTypeSize;              external MfApiLibA name 'MFValidateMediaTypeSize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMediaType;                    external MfApiLibA name 'MFCreateMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMFVideoFormatFromMFMediaType; external MfApiLibA name 'MFCreateMFVideoFormatFromMFMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateWaveFormatExFromMFMediaType;  external MfApiLibA name 'MFCreateWaveFormatExFromMFMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitMediaTypeFromVideoInfoHeader;   external MfApiLibA name 'MFInitMediaTypeFromVideoInfoHeader' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitMediaTypeFromVideoInfoHeader2;  external MfApiLibA name 'MFInitMediaTypeFromVideoInfoHeader2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitMediaTypeFromMPEG1VideoInfo;    external MfApiLibA name 'MFInitMediaTypeFromMPEG1VideoInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitMediaTypeFromMPEG2VideoInfo;    external MfApiLibA name 'MFInitMediaTypeFromMPEG2VideoInfo' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCalculateBitmapImageSize;           external MfApiLibA name 'MFCalculateBitmapImageSize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCalculateImageSize;                 external MfApiLibA name 'MFCalculateImageSize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFFrameRateToAverageTimePerFrame;     external MfApiLibA name 'MFFrameRateToAverageTimePerFrame' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFAverageTimePerFrameToFrameRate;     external MfApiLibA name 'MFAverageTimePerFrameToFrameRate' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitMediaTypeFromMFVideoFormat;     external MfApiLibA name 'MFInitMediaTypeFromMFVideoFormat' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitMediaTypeFromWaveFormatEx;      external MfApiLibA name 'MFInitMediaTypeFromWaveFormatEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitMediaTypeFromAMMediaType;       external MfApiLibA name 'MFInitMediaTypeFromAMMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitAMMediaTypeFromMFMediaType;     external MfApiLibA name 'MFInitAMMediaTypeFromMFMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAMMediaTypeFromMFMediaType;   external MfApiLibA name 'MFCreateAMMediaTypeFromMFMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCompareFullToPartialMediaType;      external MfApiLibA name 'MFCompareFullToPartialMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFWrapMediaType;                      external MfApiLibA name 'MFWrapMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFUnwrapMediaType;                    external MfApiLibA name 'MFUnwrapMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMediaTypeFromVideoInfoHeader;  external MfApiLibA name 'MFCreateVideoMediaTypeFromVideoInfoHeader' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMediaTypeFromVideoInfoHeader2; external MfApiLibA name 'MFCreateVideoMediaTypeFromVideoInfoHeader2' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMediaType;               external MfApiLibA name 'MFCreateVideoMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMediaTypeFromSubtype;    external MfApiLibA name 'MFCreateVideoMediaTypeFromSubtype' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFIsFormatYUV;                        external MfApiLibA name 'MFIsFormatYUV' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMediaTypeFromBitMapInfoHeader; external MfApiLibA name 'MFCreateVideoMediaTypeFromBitMapInfoHeader' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetStrideForBitmapInfoHeader;       external MfApiLibA name 'MFGetStrideForBitmapInfoHeader' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetPlaneSize;                       external MfApiLibA name 'MFGetPlaneSize' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateVideoMediaTypeFromBitMapInfoHeaderEx; external MfApiLibA name 'MFCreateVideoMediaTypeFromBitMapInfoHeaderEx' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateMediaTypeFromRepresentation;  external MfApiLibA name 'MFCreateMediaTypeFromRepresentation' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateAudioMediaType;               external MfApiLibA name 'MFCreateAudioMediaType' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitVideoFormat;                    external MfApiLibA name 'MFInitVideoFormat' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFInitVideoFormat_RGB;                external MfApiLibA name 'MFInitVideoFormat_RGB' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFConvertColorInfoToDXVA;             external MfApiLibA name 'MFConvertColorInfoToDXVA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFConvertColorInfoFromDXVA;           external MfApiLibA name 'MFConvertColorInfoFromDXVA' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCopyImage;                          external MfApiLibA name 'MFCopyImage' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFConvertFromFP16Array;               external MfApiLibA name 'MFConvertFromFP16Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFConvertToFP16Array;                 external MfApiLibA name 'MFConvertToFP16Array' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFCreateCollection;                   external MfApiLibA name 'MFCreateCollection' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetUncompressedVideoFormat;         external MfApiLibA name 'MFGetUncompressedVideoFormat' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFHeapAlloc;                          external MfApiLibA name 'MFHeapAlloc' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  procedure MFHeapFree;                          external MfApiLibA name 'MFHeapFree' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFllMulDiv;                           external MfApiLibA name 'MFllMulDiv' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFGetContentProtectionSystemCLSID;    external MfApiLibA name 'MFGetContentProtectionSystemCLSID' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  // Win10_VB  // check lib after MS release
  function MFCombineSamples;                     external MfApiLibA {TODO: check for proper lib} name 'MFCombineSamples' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  function MFSplitSample;                        external MfApiLibA {TODO: check for proper lib} name 'MFSplitSample' {$IF COMPILERVERSION > 20.0} delayed {$ENDIF};
  // end Win10_VB

  {$WARN SYMBOL_PLATFORM ON}

// internal functions converted from MACRO'S
//==========================================
function PackSize(unWidth: UINT32;
                  unHeight: UINT32): UINT64;
begin
  Result := Pack2UINT32AsUINT64(unWidth,
                                unHeight);
end;


procedure UnpackSize(unPacked: UINT64;
                     out punWidth: UINT32;
                     out punHeight: UINT32);
begin
    Unpack2UINT32AsUINT64(unPacked,
                          punWidth,
                          punHeight);
end;


function PackRatio(nNumerator: INT32;
                   unDenominator: UINT32): UINT64;
begin
  Result := Pack2UINT32AsUINT64(nNumerator,
                                unDenominator);
end;


procedure UnpackRatio(unPacked: UINT64;
                      out pnNumerator: UINT32;
                      out punDenominator: UINT32);
begin
  Unpack2UINT32AsUINT64(unPacked,
                        pnNumerator,
                        punDenominator);
end;


function MFGetAttributeUINT32(pAttributes: IMFAttributes;
                              guidKey: TGUID;
                              unDefault: UINT32): UINT32;
var
  unRet : UINT32;

begin
  unRet := 0;

  if (FAILED(pAttributes.GetUINT32(guidKey,
                                   unRet))) then
      unRet := unDefault;

  Result := unRet;
end;


function MFGetAttributeUINT64(pAttributes: IMFAttributes;
                              guidKey: TGUID;
                              unDefault: UINT64): UINT64;
var
  unRet: UINT64;
begin
  unRet := 0;
  if (FAILED(pAttributes.GetUINT64(guidKey,
                                   unRet))) then
    unRet := unDefault;

  Result := unRet;
end;


function MFGetAttributeDouble(pAttributes: IMFAttributes;
                              guidKey: TGUID;
                              fDefault: Double ): Double;
var
  fRet: Double;
begin
  if (FAILED(pAttributes.GetDouble(guidKey,
                                   fRet))) then
    fRet := fDefault;

  Result := fRet;
end;

//
function MFGetAttribute2UINT32asUINT64(pAttributes: IMFAttributes;
                                       guidKey: TGUID;
                                       out punHigh32: UINT32;
                                       out punLow32: UINT32): HResult;
var
  unPacked: UInt64;
  hr: HRESULT;
begin
  hr := pAttributes.GetUINT64(guidKey,
                              unPacked);
  if (FAILED(hr)) then
    begin
      Result := hr;
      Exit;
    end;

  Unpack2UINT32AsUINT64(unPacked,
                        punHigh32,
                        punLow32);
  Result := hr;
end;

//
function MFSetAttribute2UINT32asUINT64(pAttributes: IMFAttributes;
                                       guidKey: TGUID;
                                       unHigh32: UINT32;
                                       unLow32: UINT32): HResult;
begin
  Result := pAttributes.SetUINT64(guidKey,
                                  Pack2UINT32AsUINT64(unHigh32,
                                                      unLow32));
end;

//
function MFGetAttributeRatio(pAttributes: IMFAttributes;
                             guidKey: TGUID;
                             out punNumerator: UINT32;
                             out punDenominator: UINT32): HResult;
begin
  Result := MFGetAttribute2UINT32asUINT64(pAttributes,
                                          guidKey,
                                          punNumerator,
                                          punDenominator);
end;

//
function MFGetAttributeSize(pAttributes: IMFAttributes;
                            guidKey: TGUID;
                            out punWidth: UINT32;
                            out punHeight: UINT32): HResult;
begin
  Result := MFGetAttribute2UINT32asUINT64(pAttributes,
                                          guidKey,
                                          punWidth,
                                          punHeight);
end;

//
function MFSetAttributeRatio(pAttributes: IMFAttributes;
                             guidKey: TGUID;
                             unNumerator: UINT32;
                             unDenominator: UINT32): HResult;
begin
  Result := MFSetAttribute2UINT32asUINT64(pAttributes,
                                          guidKey,
                                          unNumerator,
                                          unDenominator);
end;

//
function MFSetAttributeSize(pAttributes: IMFAttributes;
                            guidKey: TGUID;
                            unWidth: UINT32;
                            unHeight: UINT32): HResult;
begin
  Result := MFSetAttribute2UINT32asUINT64(pAttributes,
                                          guidKey,
                                          unWidth,
                                          unHeight);
end;

//
function MFGetAttributeString(pAttributes: IMFAttributes;
                              guidKey: TGUID;
                              out ppsz: LPWSTR): HResult;
var
  uiLength: UINT32;
  puiLength: PUINT32;
  psz: LPWSTR;
  hr: HRESULT;
  pcb: PSIZE_T;

begin
  psz:= Nil;
  hr:= NOERROR;  //init

try
try
  hr:= pAttributes.GetStringLength(guidKey,
                                   uiLength);
  // add nil to uilength
  if (SUCCEEDED(hr)) then
    hr := UIntAdd(uiLength,
                  UINT32(1),
                  puiLength)  // actually the same as inc(uiLength)
  else
    begin
      hr := E_FAIL;
      Abort;
    end;

  if (SUCCEEDED(hr))  then
    begin
      hr := SizeTMult(SIZE_T(puilength),
                      SIZE_T(sizeof(psz)),
                      pcb);
      if(SUCCEEDED(hr)) then
        begin
          psz := PWideChar(CoTaskMemAlloc(Int(pcb)));
          if (psz = nil ) then
            begin
              hr := E_OUTOFMEMORY;
              Abort;
            end;
        end
      else
        begin
          hr := E_FAIL;
          Abort;
        end;
    end
  else
    begin
      hr := E_FAIL;
      Abort;
    end;

  if (SUCCEEDED(hr)) then
    hr := pAttributes.GetString(guidKey,
                                psz,
                                uiLength,
                                uilength)
  else
    begin
      hr := E_FAIL;
      Abort;
    end;

  if (SUCCEEDED(hr)) then
    ppsz := psz
  else
    begin
      hr := E_FAIL;
      Abort;
    end;
except
  //Do nothing
end;
finally
  Result := hr;
  CoTaskMemFree(psz);
end;
end;  // MFGetAttributeString


end.
