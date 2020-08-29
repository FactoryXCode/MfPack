// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: WinApi.Unknwn.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.0.0
// Description: -
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
// Remarks: Requires Windows 2000 Pro or later.
// 
//          Delphi : The IUnknown entries of functions should be casted like this:
//          IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//          The Iunkown interface is commented out here,
//          use the System.IUnknown interface.
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
// Source: unknwn.h
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
unit WinApi.Unknwn;

  {$HPPEMIT '#include "unknwn.h"'}

interface

uses
  {WinApi}
	WinApi.WinApiTypes;

  {$WEAKPACKAGEUNIT ON}
  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}

const

  IID_IUnknown               : TGUID = '{00000000-0000-0000-C000-000000000046}';

type

  PIUnknown = ^IUnknown;
  PAsyncIUnknown = ^AsyncIUnknown;
  PIClassFactory = ^IClassFactory;
  // Interface IUnknown
  LPUNKNOWN = ^IUnknown;


//////////////////////////////////////////////////////////////////
// IID_IUnknown and all other system IIDs are provided in UUID.LIB
// Link that library in with your proxies, clients and servers
//////////////////////////////////////////////////////////////////


  // Interface IUnknown
  // ==================
  // Enables clients to get pointers to other interfaces on a given object through the QueryInterface method,
  // and manage the existence of the object through the AddRef and Release methods.
  // All other COM interfaces are inherited, directly or indirectly, from IUnknown.
  // Therefore, the three methods in IUnknown are the first entries in the VTable for every interface.
  //
  // You must implement IUnknown as part of every interface.
  // If you are using C++ multiple inheritance to implement multiple interfaces,
  // the various interfaces can share one implementation of IUnknown.
  // If you are using nested classes to implement multiple interfaces,
  // you must implement IUnknown once for each interface you implement.
  //
  // Delphi specific:
  //   The IUnknown entries of functions should be casted like this:
  //   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
  //
  // NOTE: Comment out this inteface, if you run in to clashes with System.IUnknown and
  //       IInterface.
  //
//  {$EXTERNALSYM IUnknown}
//  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUnknown);'}
//  IUnknown = interface
//  ['{00000000-0000-0000-C000-000000000046}']
//    function QueryInterface(const riid: REFIID;
//                            out ppvObject): HResult; stdcall;
    // Parameters
    //  riid [in]
    //    The identifier of the interface being requested.
    //  ppvObject [out]
    //    The address of a pointer variable that receives the interface pointer
    //    requested in the riid parameter.
    //    Upon successful return, *ppvObject contains the requested
    //    interface pointer to the object.
    //    If the object does not support the interface, ppvObject is set to NIL.

//    function AddRef(): ULONG; stdcall;
    // Increments the reference count for an interface on an object.
    // This method should be called for every new copy of a pointer to an interface on an object.
    // Return value
    //  The method returns the new reference count. This value is intended to be used only for test purposes.

//    function Release(): ULONG; stdcall;
    // Decrements the reference count for an interface on an object.
    // Remarks
    //  When the reference count on an object reaches zero,
    //  Release must cause the interface pointer to free itself.
    //  When the released pointer is the only existing reference to
    //  an object (whether the object supports single or multiple interfaces),
    //  the implementation must free the object.
    //  Note that aggregation of objects restricts the ability to recover interface pointers.
    //
    //  Notes to Callers
    //    Call this method when you no longer need to use an interface pointer.
    //    If you are writing a method that takes an in-out parameter,
    //    call Release on the pointer you are passing in before copying the out-value on top of it.
//  end;
//  IID_IUnknown = IUnknown;
//  {$EXTERNALSYM IID_IUnknown}

  // Interface AsyncIUnknown
  // =======================
  // Undocumented
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(AsyncIUnknown);'}
  {$EXTERNALSYM AsyncIUnknown}
  AsyncIUnknown = interface(IUnknown)
  ['{000e0000-0000-0000-C000-000000000046}']
    function Begin_QueryInterface(const riid: REFIID): HResult; stdcall;

    function Finish_QueryInterface(out pvObject: PPointer): HResult; stdcall;

    function Begin_AddRef(): HResult; stdcall;

    function Finish_AddRef(): ULONG; stdcall;

    function Begin_Release(): HResult; stdcall;

    function Finish_Release(): ULONG; stdcall;

  end;
  IID_AsyncIUnknown = AsyncIUnknown;
  {$EXTERNALSYM IID_AsyncIUnknown}


  // Interface IClassFactory
  // =======================
  // Enables a class of objects to be created.
  // You must implement this interface for every class that you register in the system registry and
  // to which you assign a CLSID, so objects of that class can be created.
  //
  // After calling the CoGetClassObject function to get an IClassFactory interface pointer to the class object,
  // call the CreateInstance method of this interface to create an object.
  // It is not, however, always necessary to go through this process to create an object.
  // To create a single object, you can, instead, just call CoCreateInstance.
  // OLE also provides numerous helper functions (with names of the form OleCreateXxx) to create compound document objects.
  //
  // Call the LockServer method to keep the object server in memory and enhance performance only if you
  // intend to create more than one object of the specified class.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IClassFactory);'}
  {$EXTERNALSYM IClassFactory}
  IClassFactory = interface(IUnknown)
  ['{00000001-0000-0000-C000-000000000046}']
    function CreateInstance(const pUnkOuter: IUnknown;
                            const riid: REFIID;
                            out ppvObject): HResult; stdcall;

    function LockServer(fLock: BOOL): HResult; stdcall;

  end;
  IID_IClassFactory = IClassFactory;
  {$EXTERNALSYM IID_IClassFactory}


  // Additional Prototypes for ALL interfaces

type

  // Delphi specific to meet C++ implementations of IUnknown
  TIUnknownObject = class(TObject, IUnknown)
  protected
    FRefCount: Integer;
    //----------------------- IUnknown methods ---------------------------------
    function QueryInterface(const riid: TGUID;
                            out Obj): HResult; stdcall;
    function _AddRef(): Integer; stdcall;
    function _Release(): Integer; stdcall;
    //--------------------------------------------------------------------------

  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    property RefCount: Integer read FRefCount;

  end;

  // End of Additional Prototypes

implementation

  // Implement Additional functions here.


procedure TIUnknownObject.AfterConstruction();
begin
  inherited;
end;

procedure TIUnknownObject.BeforeDestruction();
begin
  inherited;
end;

function TIUnknownObject.QueryInterface(const riid: TGUID;
                                        out Obj): HResult;
begin
  if GetInterface(riid,
                  Obj) then
    Result := 0 // = S_OK
  else
    Result := E_NOINTERFACE;
end;

function TIUnknownObject._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TIUnknownObject._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if (Result = 0) then
    Destroy();
  Result := -1;
end;

end.
