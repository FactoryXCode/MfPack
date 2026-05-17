unit WinApi.WinShellApi.ShlObjIdl;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  Winapi.CommCtrl,
  Winapi.msxml,
  WinApi.ShTypes,
  {WinShellApi}
  WinApi.WinShellApi.ShlObjId_Core,
  {ActiveX}
  Winapi.ActiveX.ObjBase,
  WinApi.ComBaseApi,
  WinApi.ActiveX.PropIdl,
  WinApi.ActiveX.PropSys,
  WinApi.ActiveX.ObjIdlbase,
  WinApi.ActiveX.ObjIdl;

  {$MINENUMSIZE 4}

  {$IFDEF WIN32}
    {$ALIGN 1}
  {$ELSE}
    {$ALIGN 8} // Win64
  {$ENDIF}

  {$I 'WinApiTypes.inc'}


const

  IDD_WIZEXTN_FIRST = $5000;
  IDD_WIZEXTN_LAST = $5100;
  ACDD_VISIBLE = $0001;

type

  // These values are used by any Sync Engine to expose internal engine states to the PropertyStore's PKEY_StorageProviderState
  // in the File Indexer. To update the key, bind through GPS_EXTRINSICPROPERTIES and use the IPropertyStore interface
  // to set the key to the desired bitmask value.
  // SYNC_ENGINE_STATE_FLAGS = DWORD;
  LPVIEWSETTINGS = Pointer;
  // DEFINE_ENUM_FLAG_OPERATORS(SYNC_ENGINE_STATE_FLAGS)
  // FOLDERSETTINGS is a data structure that explorer passes from one folder
  // view to another, when the user is browsing. It calls ISV::GetCurrentInfo
  // member to get the current settings and pass it to ISV::CreateViewWindow
  // to allow the next folder view 'inherit' it. These settings assumes a
  // particular UI (which the shell's folder view has), and shell extensions
  // may or may not use those settings.

  //
  //  Contents: This interface definition contains shell objects that can be remoted
  //


// Enums =======================================================================

type
  _SV3CVW3_FLAGS = type DWORD;
  {$EXTERNALSYM _SV3CVW3_FLAGS}
  SV3CVW3_FLAGS = _SV3CVW3_FLAGS;
  {$EXTERNALSYM SV3CVW3_FLAGS}
const
  SV3CVW3_DEFAULT          = _SV3CVW3_FLAGS($0000);
  {$EXTERNALSYM SV3CVW3_DEFAULT}
  SV3CVW3_NONINTERACTIVE   = _SV3CVW3_FLAGS($0001);
  {$EXTERNALSYM SV3CVW3_NONINTERACTIVE}
  SV3CVW3_FORCEVIEWMODE    = _SV3CVW3_FLAGS($0002);
  {$EXTERNALSYM SV3CVW3_FORCEVIEWMODE}
  SV3CVW3_FORCEFOLDERFLAGS = _SV3CVW3_FLAGS($0004);
  {$EXTERNALSYM SV3CVW3_FORCEFOLDERFLAGS}

type
  PCDBE_ACTIONS = ^_CDBE_ACTIONS;
  _CDBE_ACTIONS = type DWORD;
  {$EXTERNALSYM _CDBE_ACTIONS}
  CDBE_ACTIONS = _CDBE_ACTIONS;
  {$EXTERNALSYM CDBE_ACTIONS}
const
  CDBE_TYPE_MUSIC = CDBE_ACTIONS($00000001);
  {$EXTERNALSYM CDBE_TYPE_MUSIC}
  CDBE_TYPE_DATA  = CDBE_ACTIONS($00000002);
  {$EXTERNALSYM CDBE_TYPE_DATA}
  CDBE_TYPE_ALL   = CDBE_ACTIONS($FFFFFFFF);
  {$EXTERNALSYM CDBE_TYPE_ALL}

type
  tagCDBURNINGEXTENSIONRET = type DWORD;
  {$EXTERNALSYM tagCDBURNINGEXTENSIONRET}
const
  CDBE_RET_DEFAULT            = tagCDBURNINGEXTENSIONRET(0);
  {$EXTERNALSYM CDBE_RET_DEFAULT}
  CDBE_RET_DONTRUNOTHEREXTS   = tagCDBURNINGEXTENSIONRET($0001);
  {$EXTERNALSYM CDBE_RET_DONTRUNOTHEREXTS}
  CDBE_RET_STOPWIZARD         = tagCDBURNINGEXTENSIONRET($0002);
  {$EXTERNALSYM CDBE_RET_STOPWIZARD}

type
  NSTCEHITTEST = type DWORD;
  {$EXTERNALSYM NSTCEHITTEST}
const
  NSTCEHT_NOWHERE          = NSTCEHITTEST($0001);
  {$EXTERNALSYM NSTCEHT_NOWHERE}
  NSTCEHT_ONITEMICON       = NSTCEHITTEST($0002);
  {$EXTERNALSYM NSTCEHT_ONITEMICON}
  NSTCEHT_ONITEMLABEL      = NSTCEHITTEST($0004);
  {$EXTERNALSYM NSTCEHT_ONITEMLABEL}
  NSTCEHT_ONITEMINDENT     = NSTCEHITTEST($0008);
  {$EXTERNALSYM NSTCEHT_ONITEMINDENT}
  NSTCEHT_ONITEMBUTTON     = NSTCEHITTEST($0010);
  {$EXTERNALSYM NSTCEHT_ONITEMBUTTON}
  NSTCEHT_ONITEMRIGHT      = NSTCEHITTEST($0020);
  {$EXTERNALSYM NSTCEHT_ONITEMRIGHT}
  NSTCEHT_ONITEMSTATEICON  = NSTCEHITTEST($0040);
  {$EXTERNALSYM NSTCEHT_ONITEMSTATEICON}
  NSTCEHT_ONITEM           = NSTCEHITTEST($0046);
  {$EXTERNALSYM NSTCEHT_ONITEM}
  NSTCEHT_ONITEMTABBUTTON  = NSTCEHITTEST($1000);
  {$EXTERNALSYM NSTCEHT_ONITEMTABBUTTON}

type
  NSTCECLICKTYPE = type DWORD;
  {$EXTERNALSYM NSTCECLICKTYPE}
const
  NSTCECT_LBUTTON   = NSTCECLICKTYPE($0001);
  {$EXTERNALSYM NSTCECT_LBUTTON}
  NSTCECT_MBUTTON   = NSTCECLICKTYPE($0002);
  {$EXTERNALSYM NSTCECT_MBUTTON}
  NSTCECT_RBUTTON   = NSTCECLICKTYPE($0003);
  {$EXTERNALSYM NSTCECT_RBUTTON}
  NSTCECT_BUTTON    = NSTCECLICKTYPE($0003);
  {$EXTERNALSYM NSTCECT_BUTTON}
  NSTCECT_DBLCLICK  = NSTCECLICKTYPE($0004);
  {$EXTERNALSYM NSTCECT_DBLCLICK}

type
  FOLDERVIEWOPTIONS = type Integer;
const
  {$EXTERNALSYM FOLDERVIEWOPTIONS}
  FVO_DEFAULT          = FOLDERVIEWOPTIONS($0000);
  FVO_VISTALAYOUT      = FOLDERVIEWOPTIONS($0001);
  {$EXTERNALSYM FVO_DEFAULT}
  FVO_CUSTOMPOSITION   = FOLDERVIEWOPTIONS($0002);
  {$EXTERNALSYM FVO_VISTALAYOUT}
  FVO_CUSTOMORDERING   = FOLDERVIEWOPTIONS($0004);
  {$EXTERNALSYM FVO_CUSTOMPOSITION}
  FVO_SUPPORTHYPERLINKS = FOLDERVIEWOPTIONS($0008);
  {$EXTERNALSYM FVO_CUSTOMORDERING}
  FVO_NOANIMATIONS     = FOLDERVIEWOPTIONS($0010);
  {$EXTERNALSYM FVO_SUPPORTHYPERLINKS}
  FVO_NOSCROLLTIPS     = FOLDERVIEWOPTIONS($0020);
  {$EXTERNALSYM FVO_NOANIMATIONS}

  {$EXTERNALSYM FVO_NOSCROLLTIPS}
type
  SYNC_ENGINE_STATE_FLAGS = type Integer;
const
  {$EXTERNALSYM SYNC_ENGINE_STATE_FLAGS}
  SESF_NONE                           = SYNC_ENGINE_STATE_FLAGS($0000);
  SESF_SERVICE_QUOTA_NEARING_LIMIT    = SYNC_ENGINE_STATE_FLAGS($0001);
  {$EXTERNALSYM SESF_NONE}
  SESF_SERVICE_QUOTA_EXCEEDED_LIMIT   = SYNC_ENGINE_STATE_FLAGS($0002);
  {$EXTERNALSYM SESF_SERVICE_QUOTA_NEARING_LIMIT}
  SESF_AUTHENTICATION_ERROR           = SYNC_ENGINE_STATE_FLAGS($0004);
  {$EXTERNALSYM SESF_SERVICE_QUOTA_EXCEEDED_LIMIT}
  SESF_PAUSED_DUE_TO_METERED_NETWORK  = SYNC_ENGINE_STATE_FLAGS($0008);
  {$EXTERNALSYM SESF_AUTHENTICATION_ERROR}
  SESF_PAUSED_DUE_TO_DISK_SPACE_FULL  = SYNC_ENGINE_STATE_FLAGS($0010);
  {$EXTERNALSYM SESF_PAUSED_DUE_TO_METERED_NETWORK}
  SESF_PAUSED_DUE_TO_CLIENT_POLICY    = SYNC_ENGINE_STATE_FLAGS($0020);
  {$EXTERNALSYM SESF_PAUSED_DUE_TO_DISK_SPACE_FULL}
  SESF_PAUSED_DUE_TO_SERVICE_POLICY   = SYNC_ENGINE_STATE_FLAGS($0040);
  {$EXTERNALSYM SESF_PAUSED_DUE_TO_CLIENT_POLICY}
  SESF_SERVICE_UNAVAILABLE            = SYNC_ENGINE_STATE_FLAGS($0080);
  {$EXTERNALSYM SESF_PAUSED_DUE_TO_SERVICE_POLICY}
  SESF_PAUSED_DUE_TO_USER_REQUEST     = SYNC_ENGINE_STATE_FLAGS($0100);
  {$EXTERNALSYM SESF_SERVICE_UNAVAILABLE}
  SESF_ALL_FLAGS                      = SYNC_ENGINE_STATE_FLAGS($01FF);
  {$EXTERNALSYM SESF_PAUSED_DUE_TO_USER_REQUEST}

  {$EXTERNALSYM SESF_ALL_FLAGS}
type
  VPWATERMARKFLAGS = type Integer;
  {$EXTERNALSYM VPWATERMARKFLAGS}
const
  VPWF_DEFAULT    = VPWATERMARKFLAGS($0000);
  {$EXTERNALSYM VPWF_DEFAULT}
  VPWF_ALPHABLEND = VPWATERMARKFLAGS($0001);
  {$EXTERNALSYM VPWF_ALPHABLEND}

type
  VPCOLORFLAGS = type Integer;
  {$EXTERNALSYM VPCOLORFLAGS}
const
  VPCF_TEXT           = VPCOLORFLAGS(1);
  {$EXTERNALSYM VPCF_TEXT}
  VPCF_BACKGROUND     = VPCOLORFLAGS(2);
  {$EXTERNALSYM VPCF_BACKGROUND}
  VPCF_SORTCOLUMN     = VPCOLORFLAGS(3);
  {$EXTERNALSYM VPCF_SORTCOLUMN}
  VPCF_SUBTEXT        = VPCOLORFLAGS(4);
  {$EXTERNALSYM VPCF_SUBTEXT}
  VPCF_TEXTBACKGROUND = VPCOLORFLAGS(5);
  {$EXTERNALSYM VPCF_TEXTBACKGROUND}

type
  DSH_FLAGS = type Integer;
  {$EXTERNALSYM DSH_FLAGS}
const
  DSH_ALLOWDROPDESCRIPTIONTEXT = DSH_FLAGS($0001);
  {$EXTERNALSYM DSH_ALLOWDROPDESCRIPTIONTEXT}

type
  NSTCSTYLE2 = type Integer;
  {$EXTERNALSYM NSTCSTYLE2}
const
  NSTCS2_DEFAULT                = NSTCSTYLE2($0000);
  {$EXTERNALSYM NSTCS2_DEFAULT}
  NSTCS2_INTERRUPTNOTIFICATIONS = NSTCSTYLE2($0001);
  {$EXTERNALSYM NSTCS2_INTERRUPTNOTIFICATIONS}
  NSTCS2_SHOWNULLSPACEMENU      = NSTCSTYLE2($0002);
  {$EXTERNALSYM NSTCS2_SHOWNULLSPACEMENU}
  NSTCS2_DISPLAYPADDING         = NSTCSTYLE2($0004);
  {$EXTERNALSYM NSTCS2_DISPLAYPADDING}
  NSTCS2_DISPLAYPINNEDONLY      = NSTCSTYLE2($0008);
  {$EXTERNALSYM NSTCS2_DISPLAYPINNEDONLY}
  NTSCS2_NOSINGLETONAUTOEXPAND  = NSTCSTYLE2($0010);
  {$EXTERNALSYM NTSCS2_NOSINGLETONAUTOEXPAND}
  NTSCS2_NEVERINSERTNONENUMERATED = NSTCSTYLE2($0020);
  {$EXTERNALSYM NTSCS2_NEVERINSERTNONENUMERATED}

type
  UNDOCK_REASON = type Integer;
  {$EXTERNALSYM UNDOCK_REASON}
const
  UR_RESOLUTION_CHANGE  = UNDOCK_REASON(0);
  {$EXTERNALSYM UR_RESOLUTION_CHANGE}
  UR_MONITOR_DISCONNECT = UNDOCK_REASON(1);
  {$EXTERNALSYM UR_MONITOR_DISCONNECT}



// Interfaces =====

type

  // Interface IQueryCodePage
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IQueryCodePage);'}
  {$EXTERNALSYM IQueryCodePage}
  IQueryCodePage = interface(IUnknown)
    ['{C7B236CE-EE80-11D0-985F-006008059382}']

    function GetCodePage(out puiCodePage: UINT): HRESULT; stdcall;

    function SetCodePage(uiCodePage: UINT): HRESULT; stdcall;
  end;
  IID_IQueryCodePage = IQueryCodePage;
  {$EXTERNALSYM IID_IQueryCodePage}


  // Interface IFolderViewOptions
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderViewOptions);'}
  {$EXTERNALSYM IFolderViewOptions}
  IFolderViewOptions = interface(IUnknown)
    ['{3CC974D2-B302-4D36-AD3E-06D93F695D3F}']

    // typedef [v1_enum] enum FOLDERVIEWOPTIONS
    // cpp_quote("DEFINE_ENUM_FLAG_OPERATORS(FOLDERVIEWOPTIONS)")
    function SetFolderViewOptions(fvoMask: FOLDERVIEWOPTIONS;
                                  fvoFlags: FOLDERVIEWOPTIONS): HRESULT; stdcall;

    function GetFolderViewOptions(out pfvoFlags: FOLDERVIEWOPTIONS): HRESULT; stdcall;
  end;
  IID_IFolderViewOptions = IFolderViewOptions;
  {$EXTERNALSYM IID_IFolderViewOptions}


// #if (NTDDI_VERSION >= NTDDI_VISTA)

  // Interface IShellView3
  // =====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellView3);'}
  {$EXTERNALSYM IShellView3}
  IShellView3 = interface(IShellView2)
    ['{EC39FA88-F8AF-41C5-8421-38BED28F4673}']

     // [v1_enum] enum _SV3CVW3_FLAGS
    // typedef DWORD SV3CVW3_FLAGS;
    function CreateViewWindow3(psbOwner: IShellBrowser;
                               psvPrev: IShellView;
                               dwViewFlags: SV3CVW3_FLAGS;
                               dwMask: FOLDERFLAGS;
                               dwFlags: FOLDERFLAGS;
                               fvMode: FOLDERVIEWMODE;
                               pvid: Pointer;
                               prcView: PRECT;
                               out phwndView: HWND): HRESULT; stdcall;
  end;
  IID_IShellView3 = IShellView3;
  {$EXTERNALSYM IID_IShellView3}
// #endif  // NTDDI_VISTA

// #if (NTDDI_VERSION >= NTDDI_WIN7)


  // Interface IShellView3
  // =====================
  // To discover the search box, use IServiceProvider.QueryService() using
  // SID_SSearchBoxInfo on a site pointer within the explorer window.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ISearchBoxInfo);'}
  {$EXTERNALSYM ISearchBoxInfo}
  ISearchBoxInfo = interface(IUnknown)
    ['{6AF6E03F-D664-4EF4-9626-F7E0ED36755E}']

    function GetCondition(riid: REFIID;
                          ppv: Pointer): HRESULT; stdcall;

    function GetText(out ppsz: LPWSTR): HRESULT; stdcall;
  end;
  IID_ISearchBoxInfo = ISearchBoxInfo;
  {$EXTERNALSYM IID_ISearchBoxInfo}

// #endif // (NTDDI_VERSION >= NTDDI_WIN7)
// #if (_WIN32_IE >= _WIN32_IE_IE70)

  // Interface IVisualProperties
  // ===========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IVisualProperties);'}
  {$EXTERNALSYM IVisualProperties}
  IVisualProperties = interface(IUnknown)
    ['{E693CF68-D967-4112-8763-99172AEE5E5A}']
    // typedef [v1_enum] enum VPWATERMARKFLAGS
    // cpp_quote("DEFINE_ENUM_FLAG_OPERATORS(VPWATERMARKFLAGS)")
    function SetWatermark(hbmp: HBITMAP; vpwf: VPWATERMARKFLAGS): HRESULT; stdcall;
    // typedef [v1_enum] enum VPCOLORFLAGS
    function SetColor(vpcf: VPCOLORFLAGS; cr: COLORREF): HRESULT; stdcall;
    function GetColor(vpcf: VPCOLORFLAGS; out pcr: COLORREF): HRESULT; stdcall;
    function SetItemHeight(cyItemInPixels: Integer): HRESULT; stdcall;
    function GetItemHeight(cyItemInPixels: Integer): HRESULT; stdcall;
    function SetFont(plf: Pointer; bRedraw: BOOL): HRESULT; stdcall;
    function GetFont(out plf: LOGFONTW): HRESULT; stdcall;
    function SetTheme(pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IVisualProperties = IVisualProperties;
  {$EXTERNALSYM IID_IVisualProperties}

// #endif  // _WIN32_IE_IE70
// #if (_WIN32_IE >= _WIN32_IE_IE70)

  // Interface ICommDlgBrowser3
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICommDlgBrowser3);'}
  {$EXTERNALSYM ICommDlgBrowser3}
  ICommDlgBrowser3 = interface(ICommDlgBrowser2)
    ['{C8AD25A1-3294-41EE-8165-71174BD01C57}']

    function OnColumnClicked(ppshv: IShellView;
                             iColumn: Integer): HRESULT; stdcall;

    function GetCurrentFilter(pszFileSpec: LPWSTR;
                              cchFileSpec: Integer): HRESULT; stdcall;

    function OnPreViewCreated(ppshv: IShellView): HRESULT; stdcall;
  end;
  IID_ICommDlgBrowser3 = ICommDlgBrowser3;
  {$EXTERNALSYM IID_ICommDlgBrowser3}

// #endif  // (_WIN32_IE >= _WIN32_IE_IE70)

  // Interface IUserAccountChangeCallback
  // ====================================
  // Notification callback for changes to user accounts.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUserAccountChangeCallback);'}
  {$EXTERNALSYM IUserAccountChangeCallback}
  IUserAccountChangeCallback = interface(IUnknown)
    ['{A561E69A-B4B8-4113-91A5-64C6BCCA3430}']

    function OnPictureChange(pszUserName: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IUserAccountChangeCallback = IUserAccountChangeCallback;
  {$EXTERNALSYM IID_IUserAccountChangeCallback}


  // Interface IStreamAsync
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStreamAsync);'}
  {$EXTERNALSYM IStreamAsync}
  IStreamAsync = interface(IStream)
    ['{FE0B6665-E0CA-49B9-A178-2B5CB48D92A5}']

    function ReadAsync(pv: Pointer;
                       cb: DWORD;
                       pcbRead: LPDWORD;
                       lpOverlapped: POverlapped): HRESULT; stdcall;

    function WriteAsync(lpBuffer: Pointer;
                        cb: DWORD;
                        pcbWritten: LPDWORD;
                        lpOverlapped: POverlapped): HRESULT; stdcall;

    function OverlappedResult(lpOverlapped: POverlapped;
                              lpNumberOfBytesTransferred: LPDWORD;
                              bWait: BOOL): HRESULT; stdcall;

    function CancelIo(): HRESULT; stdcall;

  end;
  IID_IStreamAsync = IStreamAsync;
  {$EXTERNALSYM IID_IStreamAsync}


  // Interface IStreamUnbufferedInfo
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStreamUnbufferedInfo);'}
  {$EXTERNALSYM IStreamUnbufferedInfo}
  IStreamUnbufferedInfo = interface(IUnknown)
    ['{8A68FDDA-1FDC-4C20-8CEB-416643B5A625}']

    function GetSectorSize(out pcbSectorSize: ULONG): HRESULT; stdcall;
  end;
  IID_IStreamUnbufferedInfo = IStreamUnbufferedInfo;
  {$EXTERNALSYM IID_IStreamUnbufferedInfo}


// #if (NTDDI_VERSION >= NTDDI_WIN2K)
// #if (NTDDI_VERSION >= NTDDI_VISTA)


  // Interface IStreamUnbufferedInfo
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDragSourceHelper2);'}
  {$EXTERNALSYM IDragSourceHelper2}
  IDragSourceHelper2 = interface(IDragSourceHelper)
    ['{83E07D0D-0C5F-4163-BF1A-60B274051E40}']

    function SetFlags(dwFlags: DWORD): HRESULT; stdcall;
  end;
  IID_IDragSourceHelper2 = IDragSourceHelper2;
  {$EXTERNALSYM IID_IDragSourceHelper2}

// #endif  // NTDDI_VISTA
// #endif  // NTDDI_WIN2K

  // Interface IHWEventHandler
  // =========================
  // HW Event Handler Interface
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHWEventHandler);'}
  {$EXTERNALSYM IHWEventHandler}
  IHWEventHandler = interface(IUnknown)
    ['{C1FB73D0-EC3A-4BA2-B512-8CDB9187B6D1}']

    function Initialize(pszParams: LPCWSTR): HRESULT; stdcall;

    function HandleEvent(pszDeviceID: LPCWSTR;
                         pszAltDeviceID: LPCWSTR;
                         pszEventType: LPCWSTR): HRESULT; stdcall;

    function HandleEventWithContent(pszDeviceID: LPCWSTR;
                                    pszAltDeviceID: LPCWSTR;
                                    pszEventType: LPCWSTR;
                                    pszContentTypeHandler: LPCWSTR;
                                    pdataobject: IDataObject): HRESULT; stdcall;
  end;
  IID_IHWEventHandler = IHWEventHandler;
  {$EXTERNALSYM IID_IHWEventHandler}


  // Interface IHWEventHandler2
  // ==========================
  // HW Event Handler Interface 2
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IHWEventHandler2);'}
  {$EXTERNALSYM IHWEventHandler2}
  IHWEventHandler2 = interface(IHWEventHandler)
    ['{CFCC809F-295D-42E8-9FFC-424B33C487E6}']

    function HandleEventWithHWND(pszDeviceID: LPCWSTR;
                                 pszAltDeviceID: LPCWSTR;
                                 pszEventType: LPCWSTR;
                                 hwndOwner: HWND): HRESULT; stdcall;
  end;
  IID_IHWEventHandler2 = IHWEventHandler2;
  {$EXTERNALSYM IID_IHWEventHandler2}


  // Interface IQueryCancelAutoPlay
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IQueryCancelAutoPlay);'}
  {$EXTERNALSYM IQueryCancelAutoPlay}
  IQueryCancelAutoPlay = interface(IUnknown)
    ['{DDEFE873-6997-4E68-BE26-39B633ADBE12}']

    function AllowAutoPlay(pszPath: LPCWSTR;
                           dwContentType: DWORD;
                           pszLabel: LPCWSTR;
                           dwSerialNumber: DWORD): HRESULT; stdcall;
  end;
  IID_IQueryCancelAutoPlay = IQueryCancelAutoPlay;
  {$EXTERNALSYM IID_IQueryCancelAutoPlay}




  // #if (NTDDI_VERSION >= NTDDI_VISTA)
  //
  // IDynamicHWHandler is a new feature in AutoPlay V3 (Vista)
  // A handler can register this interface for show/hide itself from the Autoplay prompt dialog.
  // Additionally, it may specify a different action string than one supplied by the static
  // handler registration under HKLM.
  //
  // params:
  //  [in]  pszDeviceID - DevicePath or drive root (e.g. G:\),
  //  [in]  dwContentType - bitfield of content types detected so far, similar to the param in IQueryCancelAutoPlay
  //  [out] *ppszAction - if not NULL, use this as the Action string.
  //                      if NULL, fallback to default static action string.
  //
  // returns:
  //  S_OK = show, S_FALSE = hide, E_xx = error
  //

  // Interface IDynamicHWHandler
  // ===========================
  // Dynamic HW Handler Interface
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDynamicHWHandler);'}
  {$EXTERNALSYM IDynamicHWHandler}
  IDynamicHWHandler = interface(IUnknown)
    ['{DC2601D7-059E-42FC-A09D-2AFD21B6D5F7}']

    function GetDynamicInfo(pszDeviceID: LPCWSTR;
                            dwContentType: DWORD;
                            out ppszAction: LPWSTR): HRESULT; stdcall;
  end;
  IID_IDynamicHWHandler = IDynamicHWHandler;
  {$EXTERNALSYM IID_IDynamicHWHandler}

// #endif  // NTDDI_VISTA
//cpp_quote("#endif  // NTDDI_WINXP")

  // Interface IUserNotificationCallback
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUserNotificationCallback);'}
  {$EXTERNALSYM IUserNotificationCallback}
  IUserNotificationCallback = interface(IUnknown)
    ['{19108294-0441-4AFF-8013-FA0A730B0BEA}']
    // All of these return S_OK to continue, S_FALSE or other error codes to stop
    // These are all "fire and forget" callbacks. Dont pump messages during these
    // callbacks...
    function OnBalloonUserClick(pt: PPOINT): HRESULT; stdcall;
    function OnLeftClick(pt: PPOINT): HRESULT; stdcall;
    function OnContextMenu(pt: PPOINT): HRESULT; stdcall;
  end;
  IID_IUserNotificationCallback = IUserNotificationCallback;
  {$EXTERNALSYM IID_IUserNotificationCallback}


  // Interface IUserNotification2
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUserNotification2);'}
  {$EXTERNALSYM IUserNotification2}
  IUserNotification2 = interface(IUnknown)
    ['{215913CC-57EB-4FAB-AB5A-E5FA7BEA2A6C}']

    function SetBalloonInfo(pszTitle: LPCWSTR;
                            pszText: LPCWSTR;
                            dwInfoFlags: DWORD): HRESULT; stdcall;

    function SetBalloonRetry(dwShowTime: DWORD;
                             dwInterval: {times in msec} DWORD;
                             cRetryCount: UINT): HRESULT; stdcall;

    function SetIconInfo(hIcon: HICON;
                         pszToolTip: LPCWSTR): HRESULT; stdcall;

    function Show(pqc: IQueryContinue;
                  dwContinuePollInterval: DWORD;
                  pSink: IUserNotificationCallback): HRESULT; stdcall;

    function PlaySound(pszSoundName: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IUserNotification2 = IUserNotification2;
  {$EXTERNALSYM IID_IUserNotification2}


  // #if (NTDDI_VERSION >= NTDDI_VISTA)
  //----------------------------------------------------------------------------
  //
  // IDeskBand2 interface
  //
  //
  // [Member functions]
  //
  // IDeskBand2.CanRenderComposited(pfCanRenderComposited)
  //   Does the deskband support composited rendering (glass/translucent)?
  //
  // IDeskBand2.SetCompositionState(fCompositionEnabled)
  //   Tell the deskband to render with or without composition.
  //
  // IDeskBand2.GetCompositionState(pfCompositionEnabled)
  //   Does the deskband currently render with or without composition?
  //
  //----------------------------------------------------------------------------

  // Interface IDeskBand2
  // ====================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDeskBand2);'}
  {$EXTERNALSYM IDeskBand2}
  IDeskBand2 = interface(IDeskBand)
    ['{79D16DE4-ABEE-4021-8D9D-9169B261D657}']

    function CanRenderComposited(out pfCanRenderComposited: BOOL): HRESULT; stdcall;

    function SetCompositionState(fCompositionEnabled: BOOL): HRESULT; stdcall;

    function GetCompositionState(out pfCompositionEnabled: BOOL): HRESULT; stdcall;
  end;
  IID_IDeskBand2 = IDeskBand2;
  {$EXTERNALSYM IID_IDeskBand2}

// #endif  // NTDDI_VISTA

  // Interface IStartMenuPinnedList
  // ==============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStartMenuPinnedList);'}
  {$EXTERNALSYM IStartMenuPinnedList}
  IStartMenuPinnedList = interface(IUnknown)
    ['{4CD19ADA-25A5-4A32-B3B7-347BEE5BE36B}']

    //  if pitem is not pinned, return S_OK
    //  if pitem is pinned, the pitem is successfully removed from pinned list, return S_OK
    //  if pitem is pinned, the pitem fail to be removed from pinned list, return error HRESULT
    function RemoveFromList(pitem: IShellItem): HRESULT; stdcall;
  end;
  IID_IStartMenuPinnedList = IStartMenuPinnedList;
  {$EXTERNALSYM IID_IStartMenuPinnedList}


  // Interface ICDBurn
  // =================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICDBurn);'}
  {$EXTERNALSYM ICDBurn}
  ICDBurn = interface(IUnknown)
    ['{3D73A659-E5D0-4D42-AFC0-5121BA425C8D}']

    function GetRecorderDriveLetter(pszDrive: LPWSTR;
                                    cch: UINT): HRESULT; stdcall;

    function Burn(hwnd: HWND): HRESULT; stdcall;

    function HasRecordableDrive(out pfHasRecorder: BOOL): HRESULT; stdcall;
  end;
  IID_ICDBurn = ICDBurn;
  {$EXTERNALSYM IID_ICDBurn}


  // Interface IWizardSite
  // =====================
  // Wizard Extension objects.  These interfaces defined methods for extending
  // Win32 wizard in a progromatic way.
  // Range of ID's that extensions can used, these mustn't clash with
  // the existing wizards dialog IDS.  (That enables them to still
  // do PropSheet_SetCurSelByID).
  // This site object is requested via a QueryService of the objects site,
  // it allows the extension to navigate in/out out itself, eg. when the
  // extension has shown all of its pages and wants to navigate to the
  // next page it would call GetNextPage and select the specified HPAGE.
  // Wizard Extension Site
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWizardSite);'}
  {$EXTERNALSYM IWizardSite}
  IWizardSite = interface(IUnknown)
    ['{88960F5B-422F-4E7B-8013-73415381C3C3}']

    function GetPreviousPage(out phpage: HPROPSHEETPAGE): HRESULT; stdcall;

    function GetNextPage(out phpage: HPROPSHEETPAGE): HRESULT; stdcall;

    function GetCancelledPage(out phpage: HPROPSHEETPAGE): HRESULT; stdcall;
  end;
  IID_IWizardSite = IWizardSite;
  {$EXTERNALSYM IID_IWizardSite}

  // Interface IWizardExtension
  // ==========================
  // A wizard extension is implemented using this object, the extension will declare the
  // pages that it supports using the AddPages method, and then when its host needs to navigate
  // into the extenion it will do so via GetFirstPage and selecting that.
  // Wizard Extension
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWizardExtension);'}
  {$EXTERNALSYM IWizardExtension}
  IWizardExtension = interface(IUnknown)
    ['{C02EA696-86CC-491E-9B23-74394A0444A8}']
    function AddPages(out aPages: HPROPSHEETPAGE; cPages: UINT; out pnPagesAdded: UINT): HRESULT; stdcall;
    function GetFirstPage(out phpage: HPROPSHEETPAGE): HRESULT; stdcall;
    function GetLastPage(out phpage: HPROPSHEETPAGE): HRESULT; stdcall;
  end;
  IID_IWizardExtension = IWizardExtension;
  {$EXTERNALSYM IID_IWizardExtension}


  // Interface IWebWizardExtension
  // =============================
  // The Web Wizard is a HTML host for wizard pages, it allows you
  // create a HTML wizard starting at the URL defined via SetInitialURL.
  // Web Wizard Page Extension
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWebWizardExtension);'}
  {$EXTERNALSYM IWebWizardExtension}
  IWebWizardExtension = interface(IWizardExtension)
    ['{0E6B3F66-98D1-48C0-A222-FBDE74E2FBC5}']

    function SetInitialURL(pszURL: LPCWSTR): HRESULT; stdcall;

    function SetErrorURL(pszErrorURL: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IWebWizardExtension = IWebWizardExtension;
  {$EXTERNALSYM IID_IWebWizardExtension}

  // Interface IPublishingWizard
  // ===========================
  // flags for the host to control the publishing wizard
  // Web Publishing Wizard
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPublishingWizard);'}
  {$EXTERNALSYM IPublishingWizard}
  IPublishingWizard = interface(IWizardExtension)
    ['{AA9198BB-CCEC-472D-BEED-19A4F6733F7A}']

    function Initialize(pdo: IDataObject;
                        dwOptions: DWORD;
                        pszServiceScope: LPCWSTR): HRESULT; stdcall;

    function GetTransferManifest(out phrFromTransfer: HRESULT;
                                 out pdocManifest: IXMLDOMDocument): HRESULT; stdcall;
  end;
  IID_IPublishingWizard = IPublishingWizard;
  {$EXTERNALSYM IID_IPublishingWizard}

// #if (NTDDI_VERSION >= NTDDI_WINXP) || (_WIN32_IE >= _WIN32_IE_IE70)

  // Interface IFolderViewHost
  // =========================
  // Object to host an IFolderView in a window.  This is used to build check mark selection
  // UI for files.
  // Shell Folder Host
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderViewHost);'}
  {$EXTERNALSYM IFolderViewHost}
  IFolderViewHost = interface(IUnknown)
    ['{1EA58F02-D55A-411D-B09E-9E65AC21605B}']

    function Initialize(hwndParent: HWND;
                        pdo: IDataObject;
                        prc: PRECT): HRESULT; stdcall;
  end;
  IID_IFolderViewHost = IFolderViewHost;
  {$EXTERNALSYM IID_IFolderViewHost}


// #if (_WIN32_IE >= _WIN32_IE_IE70)

  // Interface IFolderViewHost
  // =========================
  //  This allows you to set the accessible name on COM objects that function as UI elements.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccessibleObject);'}
  {$EXTERNALSYM IAccessibleObject}
  IAccessibleObject = interface(IUnknown)
    ['{95A391C5-9ED4-4C28-8401-AB9E06719E11}']

    function SetAccessibleName(pszName: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IAccessibleObject = IAccessibleObject;
  {$EXTERNALSYM IID_IAccessibleObject}


// #endif  // (_WIN32_IE >= _WIN32_IE_IE70)
// New for Vista, but used by downlevel code
//cpp_quote("#if (NTDDI_VERSION >= NTDDI_VISTA)")

  // Interface IResultsFolder
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IResultsFolder);'}
  {$EXTERNALSYM IResultsFolder}
  IResultsFolder = interface(IUnknown)
    ['{96E5AE6D-6AE1-4B1C-900C-C6480EAA8828}']

    function AddItem(psi: IShellItem): HRESULT; stdcall;

    function AddIDList(pidl: PCIDLIST_ABSOLUTE;
                       out ppidlAdded: PITEMID_CHILD): HRESULT; stdcall;

    function RemoteAddIDList(pidl: PCIDLIST_ABSOLUTE;
                             out ppidlAdded: PITEMID_CHILD): HRESULT; stdcall;

    function RemoveItem(psi: IShellItem): HRESULT; stdcall;

    function RemoveIDList(pidl: PCIDLIST_ABSOLUTE): HRESULT; stdcall;

    function RemoveAll: HRESULT; stdcall;
  end;
  IID_IResultsFolder = IResultsFolder;
  {$EXTERNALSYM IID_IResultsFolder}


//cpp_quote("#endif  // NTDDI_VISTA")
// #if (_WIN32_IE >= _WIN32_IE_IE70)
// #endif  // (_WIN32_IE >= _WIN32_IE_IE70)
// #endif  // NTDDI_WINXP || (_WIN32_IE >= _WIN32_IE_IE70)


  // Interface IAutoCompleteDropDown
  // ===============================
  //  Flags for IAutoCompleteDropDown.GetDropDownStatus
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IResultsFolder);'}
  {$EXTERNALSYM IResultsFolder}
  IAutoCompleteDropDown = interface(IUnknown)
    ['{3CD141F4-3C6A-11D2-BCAA-00C04FD929DB}']

    function GetDropDownStatus(out pdwFlags: DWORD;
                               out ppwszString: LPWSTR): HRESULT; stdcall;

    function ResetEnumerator(): HRESULT; stdcall;
  end;
  IID_IAutoCompleteDropDown = IAutoCompleteDropDown;
  {$EXTERNALSYM IID_IAutoCompleteDropDown}


// #if (NTDDI_VERSION >= NTDDI_WINXP)


  // The cd burning wizard extension sets return codes through a property bag
  // to tell the main wizard whether it should stop or keep going.
  //_CDBE_ACTIONS = DWORD;
  //CDBE_ACTIONS = DWORD;

  // Interface ICDBurnExt
  // ====================
  // add-ons for cd burning
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ICDBurnExt);'}
  {$EXTERNALSYM ICDBurnExt}
  ICDBurnExt = interface(IUnknown)
    ['{2271DCCA-74FC-4414-8FB7-C56B05ACE2D7}']

    function GetSupportedActionTypes(out pdwActions: CDBE_ACTIONS): HRESULT; stdcall;
  end;
  IID_ICDBurnExt = ICDBurnExt;
  {$EXTERNALSYM IID_ICDBurnExt}

// #endif  // NTDDI_WINXP
// New for XP, but used by downlevel code
//cpp_quote("#if (NTDDI_VERSION >= NTDDI_WINXP)")

  // Interface IEnumReadyCallback
  // ============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumReadyCallback);'}
  {$EXTERNALSYM IEnumReadyCallback}
  IEnumReadyCallback = interface(IUnknown)
    ['{61E00D45-8FFF-4E60-924E-6537B61612DD}']

    function EnumReady(): HRESULT; stdcall;
  end;
  IID_IEnumReadyCallback = IEnumReadyCallback;
  {$EXTERNALSYM IID_IEnumReadyCallback}


  // Interface IEnumReadyCallback
  // ============================
  // Interface between CDefView and clients like the NSC which will try to share enumeration with the view.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumerableView);'}
  {$EXTERNALSYM IID_IEnumerableView}
  IEnumerableView = interface(IUnknown)
    ['{8C8BF236-1AEC-495F-9894-91D57C3C686F}']

    function SetEnumReadyCallback(percb: IEnumReadyCallback): HRESULT; stdcall;

    function CreateEnumIDListFromContents(pidlFolder: PCIDLIST_ABSOLUTE;
                                          dwEnumFlags: DWORD;
                                          out ppEnumIDList: IEnumIDList): HRESULT; stdcall;
  end;
  IID_IEnumerableView = IEnumerableView;
  {$EXTERNALSYM IID_IEnumerableView}


  // Interface IInsertItem
  // =====================
  // Supports inserting an item into a folder.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IInsertItem);'}
  {$EXTERNALSYM IInsertItem}
  IInsertItem = interface(IUnknown)
    ['{D2B57227-3D23-4B95-93C0-492BD454C356}']

    function InsertItem(pidl: PCUIDLIST_RELATIVE): HRESULT; stdcall;
  end;
  IID_IInsertItem = IInsertItem;
  {$EXTERNALSYM IID_IInsertItem}


// #if (NTDDI_VERSION >= NTDDI_WINXP)

  // Interface IFolderBandPriv
  // =========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFolderBandPriv);'}
  {$EXTERNALSYM IFolderBandPriv}
  IFolderBandPriv = interface(IUnknown)
    ['{47C01F95-E185-412C-B5C5-4F27DF965AEA}']

    function SetCascade(fCascade: BOOL): HRESULT; stdcall;

    function SetAccelerators(fAccelerators: BOOL): HRESULT; stdcall;

    function SetNoIcons(fNoIcons: BOOL): HRESULT; stdcall;

    function SetNoText(fNoText: BOOL): HRESULT; stdcall;
  end;
  IID_IFolderBandPriv = IFolderBandPriv;
  {$EXTERNALSYM IID_IFolderBandPriv}


  // Interface IImageRecompress
  // ==========================
  // Image recompression object, given the cx, cy and a quality that we need go through the steps
  // of creating a stream that we can give to somebody containing an image that size.
  // If the image is < that size then return S_FALSE.
  // Image Recompression Object
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IImageRecompress);'}
  {$EXTERNALSYM IImageRecompress}
  IImageRecompress = interface(IUnknown)
    ['{505F1513-6B3E-4892-A272-59F8889A4D3E}']

    function RecompressImage(psi: IShellItem;
                             cx: Integer;
                             cy: Integer;
                             iQuality: Integer;
                             pstg: IStorage;
                             out ppstrmOut: IStream): HRESULT; stdcall;
  end;
  IID_IImageRecompress = IImageRecompress;
  {$EXTERNALSYM IID_IImageRecompress}



// #endif  // NTDDI_WINXP
// #endif  // NTDDI_WINXP) || (_WIN32_IE >= _WIN32_IE_IE70)
// #if (NTDDI_VERSION >= NTDDI_VISTA)


  // Interface IFileDialogControlEvents
  // ==================================
  // Event notifications from the controls, handled by an interface
  // optionally implemented by the same IFileDialogEvents object supplied by the app.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileDialogControlEvents);'}
  {$EXTERNALSYM IFileDialogControlEvents}
  IFileDialogControlEvents = interface(IUnknown)
    ['{36116642-D713-4B97-9B83-7484A9D00433}']

    // An item in a combobox, toolsmenu, or radiobutton group was selected.
    // (this notification is *not* sent when an item is chosen from the open dropdown,
    //  as the action here is always the same: close the dialog as if the user clicked open.
    //  At that point, the app will then be able to call GetSelectedItem() for the open dropdown, to
    //  obtain the item that was chosen).
    function OnItemSelected(pfdc: IFileDialogCustomize;
                            dwIDCtl: DWORD;
                            dwIDItem: DWORD): HRESULT; stdcall;

    // A pushbutton was clicked.
    function OnButtonClicked(pfdc: IFileDialogCustomize;
                             dwIDCtl: DWORD): HRESULT; stdcall;

    // A checkbutton was toggled.
    function OnCheckButtonToggled(pfdc: IFileDialogCustomize;
                                  dwIDCtl: DWORD;
                                  bChecked: BOOL): HRESULT; stdcall;

    // A combobox, toolsmenu or open dropdown is about to be "dropped down".  At this point,
    // the application may want to update the contents based on the current state of the dialog.
    function OnControlActivating(pfdc: IFileDialogCustomize;
                                 dwIDCtl: DWORD): HRESULT; stdcall;
  end;
  IID_IFileDialogControlEvents = IFileDialogControlEvents;
  {$EXTERNALSYM IID_IFileDialogControlEvents}


  // Interface IFileDialog2
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IFileDialog2);'}
  {$EXTERNALSYM IFileDialog2}
  IFileDialog2 = interface(IFileDialog)
    ['{61744FC7-85B5-4791-A9B0-272276309B13}']

    // Changing the text on the Cancel button can be useful for a "basket mode" where IFileDialogEvents::OnFileOk
    // is used to accumulate items, and Open/Cancel would be changed to Add/Done for example.
    function SetCancelButtonLabel(pszLabel: LPCWSTR): HRESULT; stdcall;

    // Replaces any items in the navigation pane with this item instead, to guide the user from navigating outside of
    // this part of the namespace.
    function SetNavigationRoot(psi: IShellItem): HRESULT; stdcall;
  end;
  IID_IFileDialog2 = IFileDialog2;
  {$EXTERNALSYM IID_IFileDialog2}

  // Interface IApplicationAssociationRegistrationUI
  // ===============================================
  // Application File Extension and URL Protocol Registration UI
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IApplicationAssociationRegistrationUI);'}
  {$EXTERNALSYM IApplicationAssociationRegistrationUI}
  IApplicationAssociationRegistrationUI = interface(IUnknown)
    ['{1F76A169-F994-40AC-8FC8-0959E8874710}']
    function LaunchAdvancedAssociationUI(pszAppRegistryName: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IApplicationAssociationRegistrationUI = IApplicationAssociationRegistrationUI;
  {$EXTERNALSYM IID_IApplicationAssociationRegistrationUI}


// #endif  // NTDDI_VISTA

  // Interface IShellRunDll
  // ======================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IShellRunDll);'}
  {$EXTERNALSYM IShellRunDll}
  IShellRunDll = interface(IUnknown)
    ['{FCE4BDE0-4B68-4B80-8E9C-7426315A7388}']
    function Run(pszArgs: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IShellRunDll = IShellRunDll;
  {$EXTERNALSYM IID_IShellRunDll}


  // Interface IPreviousVersionsInfo
  // ===============================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPreviousVersionsInfo);'}
  {$EXTERNALSYM IPreviousVersionsInfo}
  IPreviousVersionsInfo = interface(IUnknown)
    ['{76E54780-AD74-48E3-A695-3BA9A0AFF10D}']

    function AreSnapshotsAvailable(pszPath: LPCWSTR;
                                   fOkToBeSlow: BOOL;
                                   out pfAvailable: BOOL): HRESULT; stdcall;
  end;
  IID_IPreviousVersionsInfo = IPreviousVersionsInfo;
  {$EXTERNALSYM IID_IPreviousVersionsInfo}


// #if (NTDDI_VERSION >= NTDDI_VISTA)

  // Interface IUseToBrowseItem
  // ==========================
  // Used to find the item that should be used when browsing to this item (used by pagespace control).
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IUseToBrowseItem);'}
  {$EXTERNALSYM IUseToBrowseItem}
  IUseToBrowseItem = interface(IRelatedItem)
    ['{05EDDA5C-98A3-4717-8ADB-C5E7DA991EB1}']
  end;
  IID_IUseToBrowseItem = IUseToBrowseItem;
  {$EXTERNALSYM IID_IUseToBrowseItem}


  // and many more to come "related items"...
  //
  //  "public partner" - like to SharedPics from MyPics
  //  "private partner" - link to MyPics from SharedPics
  //  "recyle bin root" - where (and if) recycle bin is supported
  //  "machine root" - for "open containing machine"

// #endif  // NTDDI_VISTA

  // SID_SCommandBarState: {B99EAA5C-3850-4400-BC33-2CE534048BF8}
  // DEFINE_GUID(SID_SCommandBarState, 0xB99EAA5C, 0x3850, 0x4400, 0xBC, 0x33, 0x2C, 0xE5, 0x34, 0x04, 0x8B, 0xF8);
  // New for Vista, but used by downlevel code

//cpp_quote("#if (NTDDI_VERSION >= NTDDI_VISTA)")

  // Interface INameSpaceTreeControl2
  // ================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INameSpaceTreeControl2);'}
  {$EXTERNALSYM INameSpaceTreeControl2}
  INameSpaceTreeControl2 = interface(INameSpaceTreeControl)
    ['{7CC7AED8-290E-49BC-8945-C1401CC9306C}']

    // typedef [v1_enum] enum NSTCSTYLE2
    // cpp_quote("DEFINE_ENUM_FLAG_OPERATORS(NSTCSTYLE2)")
    function SetControlStyle(nstcsMask: NSTCSTYLE;
                             nstcsStyle: NSTCSTYLE): HRESULT; stdcall;

    function GetControlStyle(nstcsMask: NSTCSTYLE;
                             out pnstcsStyle: NSTCSTYLE): HRESULT; stdcall;

    function SetControlStyle2(nstcsMask: NSTCSTYLE2;
                              nstcsStyle: NSTCSTYLE2): HRESULT; stdcall;

    function GetControlStyle2(nstcsMask: NSTCSTYLE2;
                              out pnstcsStyle: NSTCSTYLE2): HRESULT; stdcall;
  end;
  IID_INameSpaceTreeControl2 = INameSpaceTreeControl2;
  {$EXTERNALSYM IID_INameSpaceTreeControl2}


  // Interface INameSpaceTreeControlEvents
  // =====================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INameSpaceTreeControlEvents);'}
  {$EXTERNALSYM INameSpaceTreeControlEvents}
  INameSpaceTreeControlEvents = interface(IUnknown)
    ['{93D77985-B3D8-4484-8318-672CDDA002CE}']

    // [v1_enum] enum _NSTCEHITTEST
    // typedef DWORD NSTCEHITTEST;
    // [v1_enum] enum _NSTCECLICKTYPE
    // typedef DWORD NSTCECLICKTYPE;

    function OnItemClick(psi: IShellItem;
                         nstceHitTest: NSTCEHITTEST;
                         nstceClickType: NSTCECLICKTYPE): HRESULT; stdcall;

    function OnPropertyItemCommit(psi: IShellItem): HRESULT; stdcall;

    function OnItemStateChanging(psi: IShellItem;
                                 nstcisMask: NSTCITEMSTATE;
                                 nstcisState: NSTCITEMSTATE): HRESULT; stdcall;

    function OnItemStateChanged(psi: IShellItem;
                                nstcisMask: NSTCITEMSTATE;
                                nstcisState: NSTCITEMSTATE): HRESULT; stdcall;

    function OnSelectionChanged(psiaSelection: IShellItemArray): HRESULT; stdcall;

    function OnKeyboardInput(uMsg: UINT;
                             wParam: WPARAM;
                             lParam: LPARAM): HRESULT; stdcall;

    function OnBeforeExpand(psi: IShellItem): HRESULT; stdcall;

    function OnAfterExpand(psi: IShellItem): HRESULT; stdcall;

    function OnBeginLabelEdit(psi: IShellItem): HRESULT; stdcall;

    function OnEndLabelEdit(psi: IShellItem): HRESULT; stdcall;

    function OnGetToolTip(psi: IShellItem;
                          pszTip: LPWSTR;
                          cchTip: Integer): HRESULT; stdcall;

    function OnBeforeItemDelete(psi: IShellItem): HRESULT; stdcall;

    function OnItemAdded(psi: IShellItem; fIsRoot: BOOL): HRESULT; stdcall;

    function OnItemDeleted(psi: IShellItem; fIsRoot: BOOL): HRESULT; stdcall;

    function OnBeforeContextMenu(psi: IShellItem;
                                 const riid: REFIID;
                                 ppv: Pointer): HRESULT; stdcall;

    // psi can only be NULL if NSTCS2_SHOWNULLSPACEMENU is set
    function OnAfterContextMenu(psi: IShellItem;
                                pcmIn: IContextMenu;
                                const riid: REFIID;
                                ppv: Pointer): HRESULT; stdcall;

    function OnBeforeStateImageChange(psi: IShellItem): HRESULT; stdcall;

    function OnGetDefaultIconIndex(psi: IShellItem;
                                   piDefaultIcon: Integer;
                                   piOpenIcon: Integer): HRESULT; stdcall;
  end;
  IID_INameSpaceTreeControlEvents = INameSpaceTreeControlEvents;
  {$EXTERNALSYM IID_INameSpaceTreeControlEvents}


  // Interface INameSpaceTreeControlDropHandler
  // ==========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INameSpaceTreeControlDropHandler);'}
  {$EXTERNALSYM INameSpaceTreeControlDropHandler}
  INameSpaceTreeControlDropHandler = interface(IUnknown)
    ['{F9C665D6-C2F2-4C19-BF33-8322D7352F51}']

    function OnDragEnter(psiOver: IShellItem;
                         psiaData: IShellItemArray;
                         fOutsideSource: BOOL;
                         grfKeyState: DWORD;
                         pdwEffect: Pointer): HRESULT; stdcall;

    function OnDragOver(psiOver: IShellItem;
                        psiaData: IShellItemArray;
                        grfKeyState: DWORD;
                        pdwEffect: Pointer): HRESULT; stdcall;

    function OnDragPosition(psiOver: IShellItem;
                            psiaData: IShellItemArray;
                            iNewPosition: Integer;
                            iOldPosition: Integer): HRESULT; stdcall;

    function OnDrop(psiOver: IShellItem;
                    psiaData: IShellItemArray;
                    iPosition: Integer;
                    grfKeyState: DWORD;
                    pdwEffect: Pointer): HRESULT; stdcall;

    function OnDropPosition(psiOver: IShellItem;
                            psiaData: IShellItemArray;
                            iNewPosition: Integer;
                            iOldPosition: Integer): HRESULT; stdcall;

    function OnDragLeave(psiOver: IShellItem): HRESULT; stdcall;
  end;
  IID_INameSpaceTreeControlDropHandler = INameSpaceTreeControlDropHandler;
  {$EXTERNALSYM IID_INameSpaceTreeControlDropHandler}


  // Interface INameSpaceTreeAccessible
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INameSpaceTreeAccessible);'}
  {$EXTERNALSYM INameSpaceTreeAccessible}
  INameSpaceTreeAccessible = interface(IUnknown)
    ['{71F312DE-43ED-4190-8477-E9536B82350B}']

    function OnGetDefaultAccessibilityAction(psi: IShellItem;
                                             out pbstrDefaultAction: BSTR): HRESULT; stdcall;

    function OnDoDefaultAccessibilityAction(psi: IShellItem): HRESULT; stdcall;

    function OnGetAccessibilityRole(psi: IShellItem;
                                    out pvarRole: OleVariant): HRESULT; stdcall;
  end;
  IID_INameSpaceTreeAccessible = INameSpaceTreeAccessible;
  {$EXTERNALSYM IID_INameSpaceTreeAccessible}


  // Interface INameSpaceTreeControlCustomDraw
  // =========================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(INameSpaceTreeControlCustomDraw);'}
  {$EXTERNALSYM INameSpaceTreeControlCustomDraw}
  INameSpaceTreeControlCustomDraw = interface(IUnknown)
    ['{2D3BA758-33EE-42D5-BB7B-5F3431D86C78}']

    // typedef struct NSTCCUSTOMDRAW
    // TODO: IShellItem *psi
    // TODO: UINT uItemState; // CDIS_xxx values
    // TODO: NSTCITEMSTATE nstcis
    // TODO: LPCWSTR pszText
    // TODO: int iImage
    // TODO: HIMAGELIST himl
    // TODO: int iLevel
    // TODO: int iIndent
  end;
  IID_INameSpaceTreeControlCustomDraw = INameSpaceTreeControlCustomDraw;
  {$EXTERNALSYM IID_INameSpaceTreeControlCustomDraw}

//cpp_quote("#endif  // NTDDI_VISTA")
// #if (NTDDI_VERSION >= NTDDI_VISTA)


  // Interface ITrayDeskBand
  // =======================
  // for show/hide deskbands
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ITrayDeskBand);'}
  {$EXTERNALSYM ITrayDeskBand}
  ITrayDeskBand = interface(IUnknown)
    ['{6D67E846-5B9C-4DB8-9CBC-DDE12F4254F1}']

    function ShowDeskBand(const clsid: REFCLSID): HRESULT; stdcall;

    function HideDeskBand(const clsid: REFCLSID): HRESULT; stdcall;

    function IsDeskBandShown(const clsid: REFCLSID): HRESULT; stdcall;

    function DeskBandRegistrationChanged(): HRESULT; stdcall;
  end;
  IID_ITrayDeskBand = ITrayDeskBand;
  {$EXTERNALSYM IID_ITrayDeskBand}


  // Interface IBandHost
  // ===================
  // for show/hide deskbands
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IBandHost);'}
  {$EXTERNALSYM IBandHost}
  IBandHost = interface(IUnknown)
    ['{B9075C7C-D48E-403F-AB99-D6C77A1084AC}']

    function CreateBand(rclsidBand: REFCLSID;
                        fAvailable: BOOL;
                        fVisible: BOOL;
                        const riid: REFIID;
                        ppv: Pointer): HRESULT; stdcall;

    function SetBandAvailability(rclsidBand: REFCLSID;
                                 fAvailable: BOOL): HRESULT; stdcall;

    function DestroyBand(const rclsidBand: REFCLSID): HRESULT; stdcall;
  end;
  IID_IBandHost = IBandHost;
  {$EXTERNALSYM IID_IBandHost}

// #endif  // NTDDI_VISTA

  // Interface IComputerInfoChangeNotify
  // ===================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IComputerInfoChangeNotify);'}
  {$EXTERNALSYM IComputerInfoChangeNotify}
  IComputerInfoChangeNotify = interface(IUnknown)
    ['{0DF60D92-6818-46D6-B358-D66170DDE466}']

    function ComputerInfoChanged(): HRESULT; stdcall;
  end;
  IID_IComputerInfoChangeNotify = IComputerInfoChangeNotify;
  {$EXTERNALSYM IID_IComputerInfoChangeNotify}

// #if (NTDDI_VERSION >= NTDDI_WIN7)
// #endif // NTDDI_WIN7

  // Interface IDesktopGadget
  // ========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDesktopGadget);'}
  {$EXTERNALSYM IDesktopGadget}
  IDesktopGadget = interface(IUnknown)
    ['{C1646BC4-F298-4F91-A204-EB2DD1709D1A}']

    function RunGadget(gadgetPath: LPCWSTR): HRESULT; stdcall;
  end;
  IID_IDesktopGadget = IDesktopGadget;
  {$EXTERNALSYM IID_IDesktopGadget}

// #if (NTDDI_VERSION >= NTDDI_WINTHRESHOLD)
// #endif // NTDDI_WINTHRESHOLD

  //
  //  NOTE - this typelib is never registered anywhere
  //  objects that want their typelibs to be registered
  //  in shell32 should add their coclass to shldisp.idl
  //
  // Microsoft Shell Objects
  // CLSID_QueryCancelAutoPlay
  // CLSID_TimeCategorizer
  // CLSID_AlphabeticalCategorizer
  // CLSID_MergedCategorizer
  // CLSID_ImageProperties
  // CLSID_CDBurn
  // CLSID_StartMenuPin
  // For supporting HTML wizard extensions we provide this object, it implements the IWizardExtenion
  // and allows the site to specific via an IPropertyBag an URL which should be displayed.  The property
  // bag is requested from the SID_WebWizardHost, and it used inturn to return parameter information
  // back to the site (eg. any information that the displayed HTML would like to communicate).
  // CLSID_WebWizardHost
  // CLSID_PublishDropTarget
  // CLSID_PublishingWizard
  // CLSID_InternetPrintOrdering
  // CLSID_FolderViewHost
  // CLSID_ExplorerBrowser
  // CLSID_ImageRecompress
  // CLSID_TrayBandSiteService
  // CLSID_TrayDeskBand
  // CLSID_AttachmentServices
  // CLSID_DocPropShellExtension
  // CLSID_FSCopyHandler
  // CLSID_PreviousVersions
  // CLSID_NamespaceTreeControl
  // CLSID_IENamespaceTreeControl
  // CLSID_ApplicationAssociationRegistrationUI
  // CLSID_DesktopGadget
  // CLSID_AccessibilityDockingService
  // CLSID_ExecuteFolder
  // CLSID_VirtualDesktopManager
  // CLSID_StorageProviderBanners
  // helper APIs to simplify initializing an instance of IShellLibrary and working with file system paths
  // #if (NTDDI_VERSION >= NTDDI_WIN7)
  // #if (_WIN32_IE >= _WIN32_IE_IE70)
  // #if defined(__cplusplus) && !defined(CINTERFACE)
  // 
  // These functions properly initialize their _Outptr_ parameters to NULL, and only return NULL
  // on failure, but /analyze can't presently distinguish the failure case from the success case, and
  // throws warning C6387 anyway.  Thus, the warning is disabled to avoid generating noise for code
  // that includes shobjidl.h and compiles with /analyze.
  // #pragma warning(push)
  // #pragma warning(disable:6387)
  // __inline HRESULT SHResolveFolderPathInLibrary(_In_ IShellLibrary *plib, _In_ PCWSTR pszFolderPath, _In_ DWORD dwTimeout, _Outptr_ PWSTR *ppszResolvedPath)
  // {
  //     *ppszResolvedPath = NULL;
  //     PIDLIST_ABSOLUTE pidlFolder = SHSimpleIDListFromPath(pszFolderPath);
  //     HRESULT hr = pidlFolder ? S_OK : E_INVALIDARG;
  //     if (SUCCEEDED(hr))
  //     {
  //         IShellItem *psiFolder;
  //         hr = SHCreateItemFromIDList(pidlFolder, IID_PPV_ARGS(&psiFolder));
  //         if (SUCCEEDED(hr))
  //         {
  //             IShellItem *psiResolved;
  //             hr = plib->ResolveFolder(psiFolder, dwTimeout, IID_PPV_ARGS(&psiResolved));
  //             if (SUCCEEDED(hr))
  //             {
  //                 hr = psiResolved->GetDisplayName(SIGDN_DESKTOPABSOLUTEPARSING, ppszResolvedPath);
  //                 psiResolved->Release();
  //             }
  //             psiFolder->Release();
  //         }
  //         CoTaskMemFree(pidlFolder);
  //     }
  //     return hr;
  // }
  // 
  // #pragma warning(pop)
  // #endif  // __cplusplus && !CINTERFACE
  // #endif  // _WIN32_IE >= _WIN32_IE_IE70
  // #endif  // NTDDI_WIN7
  // 
  // #if (NTDDI_VERSION >= NTDDI_WIN8)
  // Accessibility Docking Service
  // This service allows an accessibility window to dock itself to the bottom of
  // the app monitor. This API ensures that when the docking has completed,
  // the accessibility window is no longer obscuring/overlaying any of the user's applications or other windows
  // A sample scenario:
  // The user has an accessibility keyboard that is currently a floating window. Since it is above all other windows,
  // it's obsuring the user's applications. The user presses the "dock" button on the keyboard, which causes it to dock
  // to bottom of the screen. Tailored experiences and most desktop applications are moved so that they are no
  // longer obscured by keyboard.

  //UNDOCK_REASON = DWORD;


  // Interface IAccessibilityDockingServiceCallback
  // ==============================================
  // Developers who are interested in using the Accessibility Docking Service
  // will need to implement this interface. This is the interface on which apps will receive undock notifications
  // that happen in response to certain system events (listed in the enumeration above)
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccessibilityDockingServiceCallback);'}
  {$EXTERNALSYM IAccessibilityDockingServiceCallback}
  IAccessibilityDockingServiceCallback = interface(IUnknown)
    ['{157733FD-A592-42E5-B594-248468C5A81B}']

    function Undocked(undockReason: UNDOCK_REASON): HRESULT; stdcall;
  end;
  IID_IAccessibilityDockingServiceCallback = IAccessibilityDockingServiceCallback;
  {$EXTERNALSYM IID_IAccessibilityDockingServiceCallback}


  // Interface IAccessibilityDockingService
  // ======================================
  // Developers who are interested in using the Accessibility Docking Service
  // do not need to implement the following interface. An implementation is provided through the CoCreatable Object
  // with the ID CLSID_AccessibilityDockingService
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IAccessibilityDockingService);'}
  {$EXTERNALSYM IAccessibilityDockingService}
  IAccessibilityDockingService = interface(IUnknown)
    ['{8849DC22-CEDF-4C95-998D-051419DD3F76}']

    // This is used to get the available docking size (Fixed Width, Max Height) on a particular monitor
    // The units passed back are in screen pixels
    function GetAvailableSize(hMonitor: HMONITOR;
                              out pcxFixed: UINT;
                              out pcyMax: UINT): HRESULT; stdcall;

    // This is used to dock a window into the bottom of a monitor while the system is in immersive mode
    // The requested height is in screen pixels
    function DockWindow(hwnd: HWND;
                        hMonitor: HMONITOR;
                        cyRequested: UINT;
                        pCallback: IAccessibilityDockingServiceCallback): HRESULT; stdcall;

    // This undocks the passed in window
    function UndockWindow(hwnd: HWND): HRESULT; stdcall;
  end;
  IID_IAccessibilityDockingService = IAccessibilityDockingService;
  {$EXTERNALSYM IID_IAccessibilityDockingService}


// #endif // NTDDI_WIN8
// #if (NTDDI_VERSION >= NTDDI_WIN10_RS4)

  // Interface IAccessibilityDockingService
  // ======================================
  // StorageProviderBanners implements this interface.
  // Interface can obtained by instantiating StorageProviderBanners using CoCreateInstance with CLSID_StorageProviderBanners as an inproc server.
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStorageProviderBanners);'}
  {$EXTERNALSYM IStorageProviderBanners}
  IStorageProviderBanners = interface(IUnknown)
    ['{5EFB46D7-47C0-4B68-ACDA-DED47C90EC91}']

    function SetBanner(providerIdentity: LPCWSTR;
                       subscriptionId: LPCWSTR;
                       contentId: LPCWSTR): HRESULT; stdcall;

    function ClearBanner(providerIdentity: LPCWSTR;
                         subscriptionId: LPCWSTR): HRESULT; stdcall;

    function ClearAllBanners(providerIdentity: LPCWSTR): HRESULT; stdcall;

    function GetBanner(providerIdentity: LPCWSTR;
                       subscriptionId: LPCWSTR;
                       out contentId: LPWSTR): HRESULT; stdcall;
  end;
  IID_IStorageProviderBanners = IStorageProviderBanners;
  {$EXTERNALSYM IID_IStorageProviderBanners}


// #endif // NTDDI_WIN10_RS4

  // Interface IStorageProviderCopyHook
  // ==================================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IStorageProviderCopyHook);'}
  {$EXTERNALSYM IStorageProviderCopyHook}
  IStorageProviderCopyHook = interface(IUnknown)
    ['{7BF992A9-AF7A-4DBA-B2E5-4D080B1ECBC6}']

    function CopyCallback(hwnd: HWND;
                          operation: UINT;
                          flags: UINT;
                          srcFile: LPCWSTR;
                          srcAttribs: DWORD;
                          destFile: LPCWSTR;
                          destAttribs: DWORD;
                          out result: UINT): HRESULT; stdcall;
  end;
  IID_IStorageProviderCopyHook = IStorageProviderCopyHook;
  {$EXTERNALSYM IID_IStorageProviderCopyHook}


  // Additional prototypes for ALL interfaces

  // End of additional prototypes

implementation

  // Implement additional prototypes here.

end.
