unit WinApi.ObjectArray;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi;

type

  IObjectArray = interface;
  IObjectCollection = interface;



  // Interface IObjectArray
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectArray);'}
  {$EXTERNALSYM IObjectArray}
  IObjectArray = interface(IUnknown)
    ['{92CA9DCD-5622-4BBA-A805-5E9F541BD8C9}']

    function GetCount(out pcObjects: UINT): HRESULT; stdcall;

    function GetAt(uiIndex: UINT;
                   const riid: TGUID;
                   out ppv): HRESULT; stdcall;
  end;
  IID_IObjectArray = IObjectArray;
  {$EXTERNALSYM IID_IObjectArray}



  // Interface IObjectCollection
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IObjectCollection);'}
  {$EXTERNALSYM IObjectCollection}
  IObjectCollection = interface(IObjectArray)
    ['{5632B1A4-E38A-400A-928A-D4CD63230295}']

    function AddObject(punk: IUnknown): HRESULT; stdcall;

    function AddFromArray(poaSource: IObjectArray): HRESULT; stdcall;

    function RemoveObjectAt(uiIndex: UINT): HRESULT; stdcall;

    function Clear(): HRESULT; stdcall;
  end;
  IID_IObjectCollection = IObjectCollection;
  {$EXTERNALSYM IID_IObjectCollection}

implementation

end.
