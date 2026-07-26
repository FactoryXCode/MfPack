// FactoryX
//
// Copyright: FactoryX. All rights reserved.
//
// Project: Media Foundation - MFPack - Samples
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: frmMainMDI.pas
// Kind: Pascal Unit
// Release date: 24-01-2026
// Language: ENU
//
// Revision Version: 3.1.9
// Description: Main MDI form.
//
// Company: FactoryX
// Intiator(s): Tony (maXcomX), Carmen (carmenh).
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh).
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 01/13/2026 All                 Sineead O'Connor release  SDK 10.0.26100.4654 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//          Please, read documentation carefully!
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
// =============================================================================
// Source: FactoryX.Code.
// =============================================================================
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
unit frmMainMDI;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinApiTypes,
  WinApi.Messages,
  WinApi.WinError,
  {System}
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.IOUtils,
  System.Math,
  System.Types,
  System.UITypes,
  System.Win.ComObj,
  System.JSON,
  System.SyncObjs,
  {Vcl}
  Vcl.Graphics,
  Vcl.Imaging.jpeg,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfObjects,
  WinApi.MediaFoundationApi.MfMetLib,
  WinApi.MediaFoundationApi.MfUtils,
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.AudioPolicy,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  MPxpButton,
  RDJ_Common,
  RDJ.Setup,
  RDJ.InternalMixer,
  RDJ.JSon,
  RDJ.RdjPro.AudioQueue,
  MfWasApiFxComponentBase,
  MfParametricEqComponent,
  MfCompressorLimiterComponent,
  MfLowMidHighEqComponent,
  MfFlangerEchoComponent,
  frmChannelDeck,
  frmMasterDeck,
  frmPlaylistEditor,
  frmSetup,
  frmMasterFxRack,
  frmLoopBackDeck,
  MicrophoneDeckFrm,
  MfWasApiEffectsRack,
  MfWasApiRenderOutputEngine,
  MfAudioRecorder,
  dlgMediaServer;

const

  WM_RDJ_ENDPOINTS_CHANGED = WM_APP + 410;
  RDJ_PBT_APMSUSPEND = $0004;
  RDJ_PBT_APMRESUMECRITICAL = $0006;
  RDJ_PBT_APMRESUMESUSPEND = $0007;
  RDJ_PBT_APMRESUMEAUTOMATIC = $0012;
  RDJ_POWER_ACTION_DO_NOTHING = 0;

  // json file url
  COVER_IMAGE_FILE_URL = 'cover.jpg';
  COVER_DEFAULT_IMAGE_FILE_URL = 'cover_default.jpg';
  COVER_IMAGE_FILE_NAME = 'cover.jpg';
  COVER_DEFAULT_IMAGE_FILE_NAME = 'cover_default.jpg';


type

  // DEBUG: See OnCloseQuery section. Best recommondation is using MadExcept to make things easier to find thread bugs.
  // TCustomFormAccess = class(TCustomForm);

  TAudioEndpointNotificationClient = class(TInterfacedObject, IMMNotificationClient)
  private

    FWnd: HWND;

  public

    constructor Create(const AWnd: HWND);

    function OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                  dwNewState: DWord): HResult; stdcall;
    function OnDeviceAdded(pwstrDeviceId: LPCWSTR): HResult; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HResult; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow;
                                    role: ERole;
                                    pwstrDefaultDeviceId: PWideChar): HResult; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                    const key: PROPERTYKEY): HResult; stdcall;
  end;

  TMainMDIFrm = class(TForm)
    pnlCaption: TPanel;
    btnExit: TMPxpButton;
    btnSetup: TMPxpButton;
    btnPlaylist: TMPxpButton;
    btnMinimize: TMPxpButton;
    btnMaxNormal: TMPxpButton;
    tmrClock: TTimer;
    lblLocalTime: TLabel;
    imgLogo: TImage;
    lblAppTitle: TLabel;
    pnlFooter: TPanel;
    btnEffects: TMPxpButton;
    Panel1: TPanel;
    mmoTextNotes: TMemo;
    btnClearMemo: TMPxpButton;
    btnFooterPnl: TMPxpButton;
    lblDjName: TLabel;
    lblShow: TLabel;
    mmoShow: TMemo;
    mmoDjName: TMemo;
    btnSetDjNameAndShowTitle: TMPxpButton;
    imgDjShowLogo: TImage;
    chkMediaServer: TMPxpButton;
    Label1: TLabel;
    Label2: TLabel;
    mmoEventTitle: TMemo;
    Label3: TLabel;
    mmoActivityTitle: TMemo;
    Label4: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure tmrClockTimer(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnMaxNormalClick(Sender: TObject);
    procedure btnSetupClick(Sender: TObject);
    procedure btnPlaylistClick(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
    procedure pnlCaptionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnEffectsClick(Sender: TObject);
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnFooterPnlClick(Sender: TObject);
    procedure btnSetDjNameAndShowTitleClick(Sender: TObject);
    procedure imgDjShowLogoDblClick(Sender: TObject);
    procedure chkMediaServerClick(Sender: TObject);

  private

    FAppClosing: Boolean;
    FSetupApplied: Boolean;
    FSetupFileName: string;
    FMasterDeck: TMasterDeckFrm;
    FPlayListEditor: TfrmPlaylistEditor;
    FSetup: TRDJSetup;
    FInternalMixer: TRDJInternalMixer;
    FMasterFXRack: TMfWasApiEffectsRack;
    FMasterOut: TMfWasApiRenderOutputEngine;

    FInternalMixRecorderPreFx: TMfInternalMixerRecorder;
    FInternalMixRecorderPostFx: TMfInternalMixerRecorder;

    FCueOut: TMfWasApiRenderOutputEngine;

    FCueBufferLock: TRTLCriticalSection;
    FCueMixQueue: TArray<Single>;
    FCueWorkBuf: TArray<Single>;
    FCueQueueFramesCapacity: Integer;
    FCueQueueReadFrame: Integer;
    FCueQueueWriteFrame: Integer;
    FCueQueueValidFrames: Integer;
    FCueUnavailableWarned: Boolean;
    FUiInitialized: Boolean;
    FAudioGraphRecoveryQueued: Integer;
    FAudioGraphRecoveryDueTick: UInt64;
    FAudioGraphRecoveryReason: string;
    FPowerPolicyScheme: PGUID;
    FLidPolicyOverrideActive: Boolean;
    FLidPolicyWarningShown: Boolean;
    FOriginalLidActionAc: DWORD;
    FOriginalLidActionDc: DWORD;

    // Deviceloss
    FEndpointEnumerator: IMMDeviceEnumerator;
    FEndpointNotifyClient: IMMNotificationClient;
    FEndpointRefreshPending: Boolean;
    FMasterEndpointAvailable: Boolean;
    FPFLEndpointAvailable: Boolean;

    // Microphone deck
    FMicrophoneDeck: TfrmMicrophoneDeck;

    // Loopback capture forms
    FLoopbackDecks: array of TfrmLoopbackDeck;

    // FX ----------------------------------------------------------------------
    FMasterLowMidHighEq: TMfLowMidHighEqEffect;
    FMasterFlangerEcho: TMfFlangerEchoEffect;
    FMasterCompLim: TMfCompressorLimiterEffect;
    FfrmMasterFxRack: TfrmMasterFxRack;
    FMasterFxObjects: array of TMfWasApiFxComponentBase;

    // json
    FRDJRadioStatusJson: TRDJRadioStatusJson;
    FDjName: string;
    FShowName: string;
    FNowPlayingArtist: string;
    FNowPlayingTitle: string;
    FNowPlayingFromChannelDeck: Boolean;
    FCaddyListenerCount: Integer;
    FLastCaddyListenerRefreshTick: UInt64;
    FCoverFileName: string;
    FCoverJpg: string;

    // FX ----------------------------------------------------------------------
    procedure ClearMasterFxObjects();
    function CreateFxByKind(const AFxKind: TRDJFxKind): TMfWasApiFxComponentBase;
    procedure AddMasterRackSlot(AEffect: TMfWasApiFxComponentBase;
                                const AEnabled: Boolean);
    procedure BuildMasterRackFromSetup();
    procedure ProcessMasterFx(const pData: PSingle;
                              const Frames: Integer;
                              const ASampleRate: Integer);

    // RdjPro ------------------------------------------------------------------

    function BuildCoverJsonUrl(const APreferCurrent: Boolean): string;
    function NormalizeNowPlayingMemoText(AMemo: TMemo): string;
    procedure ApplyLoopbackNowPlayingFallback(var AArtist, ATitle: string);
    procedure PublishSelectedCover();
    procedure LoadBroadcastIdentityFromIni();
    procedure SaveBroadcastIdentityToIni();
    function ResolveCaddyLogFileName(): string;
    function ReadCaddyActiveListenerCount(): Integer;
    procedure RefreshCaddyListenerCount(const AForceJsonUpdate: Boolean);
    procedure WriteNowPlayingStatus(const ADjName,
                                    AShowName,
                                    AArtist,
                                    ATitle,
                                    ACoverUrl: string;
                                    const AListeners: Integer = -1;
                                    const AClearEmptyTrackInfo: Boolean = False);

    procedure ConstructEngine();

    procedure EnsureCueWorkBuf(const AFrames: Integer);
    procedure InitCueQueue(const AFramesCapacity: Integer);
    procedure PushCueFrames(const Src: PSingle; const Frames: Integer);
    function PopCueFrames(const Dst: PSingle; const Frames: Integer): Integer;
    procedure ConfigureOutputEndpoints();
    function BuildRenderWaveFormat(out AWfx: TWAVEFORMATEX): HRESULT;

    // Checkers and resolve.
    function EndpointIdExists(const ADeviceId: string): Boolean;
    function ResetInvalidEndpointAssignments(): Boolean;
    procedure ClearAudioEndpointSetup();

    // Endpoints & deciceloss
    procedure SetupEndpointNotifications();
    procedure TeardownEndpointNotifications();
    //procedure QueueEndpointRefresh();
    procedure WMEndpointsChanged(var Msg: TMessage); message WM_RDJ_ENDPOINTS_CHANGED;
    procedure WMPowerBroadcast(var Msg: TMessage); message WM_POWERBROADCAST;

    function IsEndpointUsable(const ADeviceId: string): Boolean;
    function ResolveMasterEndpointAvailable(): Boolean;
    function ResolvePFLEndpointAvailable(): Boolean;
    procedure RefreshEndpointAvailability();
    procedure ApplyRdjExecutionState();
    procedure ApplyLidClosePolicyOverride();
    procedure RestoreLidClosePolicyOverride();
    procedure WarnLidCloseOverrideFailedOnce(const AReason: string);
    procedure AttachAudioOutputHandlers(const AOutput: TMfWasApiRenderOutputEngine);
    procedure AudioRenderError(Sender: TObject;
                               const Hr: HRESULT;
                               const Msg: string);
    procedure QueueAudioGraphRecovery(const AReason: string;
                                      const ADelayMs: Cardinal = 1500);
    procedure ServiceAudioGraphRecovery();

    procedure OpenSetupGUI();
    procedure OpenMediaServerGUI();
    function IsMediaServerBroadcasting(): Boolean;

    procedure ApplySetupOnce();
    procedure RecreateAudioOutputs();
    procedure ResetAudioRuntimeState();
    procedure HardRestartAudioGraph();

    procedure CreateMasterForm();
    procedure CreateMicrophoneForm();
    procedure CreateChannelForms(Count: Integer);
    procedure CreateLoopbackDecks(Count: Integer);
    procedure DestroyDeckForms();
    procedure TileDecks();
    procedure ApplyDarkFrameToAllChildren();

    // Mixer tap point for recorder in channeldeck.
    function GetRecorderTapPoint(): string;

    function IsRecordingPreFx(): Boolean;
    function IsRecordingPostFx(): Boolean;
    function BuildDualRecordFileName(const ABaseFileName: TFileName;
                                     const ASuffix: string): TFileName;

    // Callbacks.
    function MasterOutFillPcm(Sender: TObject;
                              pData: PByte;
                              const ByteCount: DWORD;
                              pwfx: PWAVEFORMATEX;
                              out Flags: DWORD): HRESULT;

    function CueOutFillPcm(Sender: TObject;
                           pData: PByte;
                           const ByteCount: DWORD;
                           pwfx: PWAVEFORMATEX;
                           out Flags: DWORD): HRESULT;

   public

    FChannelDecks: array of TfrmChannelDeck;

    function StartInternalMixerRecording(const ABaseFileName: TFileName;
                                         const ARecordPreFx: Boolean;
                                         const ARecordPostFx: Boolean): Boolean;
    procedure StopInternalMixerRecording();
    function IsInternalMixerRecording(): Boolean;

    procedure SetAudioRecorderRecordPreFx(const AValue: Boolean);
    procedure SetAudioRecorderRecordPostFx(const AValue: Boolean);


    function GetChannelDeck(const AIndex: Integer): TfrmChannelDeck;
    function GetChannelDeckCount(): Integer;
    // audio recorder (main deck)
    procedure SetAudioRecorderTapPoint(const AValue: Integer);
    procedure AlignMasterDeckWithFxRack();

    // Json
    procedure UpdateNowPlaying(const AArtist,
                               ATitle: string);
    procedure ClearNowPlaying();
    function HasActiveLoopbackDeck(): Boolean;
    function CanGoOnAir(): Boolean;
    procedure RefreshMainButtonStates();
    procedure SetMediaServerButtonChecked(const AChecked: Boolean);
        property Caption;

    property Setup: TRDJSetup
      read FSetup;
    property InternalMixer: TRDJInternalMixer
      read FInternalMixer;
    property RecorderTapPoint: string
      read GetRecorderTapPoint;
    property Master: TMasterDeckFrm
      read FMasterDeck;
    property jsonUpdate: TRDJRadioStatusJson
      read FRDJRadioStatusJson;
    property DjName: string
      read FDjName;
    property ShowName: string
      read FShowName;
    property CoverJpg: string
      read FCoverFileName;
  end;

var
  MainMDIFrm: TMainMDIFrm;


implementation

{$R *.dfm}

uses
  System.IniFiles,
  LWFileBrowserExDlg,
  RDJ.PlaylistDb;

const
  // Baseline target "minimum today" resolution.
  BASE_MIN_W = 1920;
  BASE_MIN_H = 1080;
  RDJ_CADDY_LISTENER_REFRESH_MS = 15000;
  RDJ_CADDY_ACTIVE_LISTENER_SECONDS = 180;
  RDJ_CADDY_LOG_TAIL_BYTES = 262144;
  RDJ_BROADCAST_IDENTITY_SECTION = 'BroadcastIdentity';
  RDJ_BROADCAST_IDENTITY_DJ_NAME = 'DjName';
  RDJ_BROADCAST_IDENTITY_SHOW_NAME = 'ShowName';
  RDJ_BROADCAST_IDENTITY_COVER_FILE = 'CoverFileName';

// TAudioEndpointNotificationClient --------------------------------------------

constructor TAudioEndpointNotificationClient.Create(const AWnd: HWND);
begin

  inherited Create;

  FWnd := AWnd;
end;


function TAudioEndpointNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow;
                                                                 role: ERole;
                                                                 pwstrDefaultDeviceId: LPCWSTR): HResult;
begin

  if (FWnd <> 0) then
    PostMessage(FWnd,
                WM_RDJ_ENDPOINTS_CHANGED,
                0,
                0);
  Result := S_OK;
end;


function TAudioEndpointNotificationClient.OnDeviceAdded(pwstrDeviceId: LPCWSTR): HResult;
begin

  if (FWnd <> 0) then
    PostMessage(FWnd,
                WM_RDJ_ENDPOINTS_CHANGED,
                0,
                0);
  Result := S_OK;
end;


function TAudioEndpointNotificationClient.OnDeviceRemoved(pwstrDeviceId: LPCWSTR): HResult;
begin

  if (FWnd <> 0) then
    PostMessage(FWnd,
                WM_RDJ_ENDPOINTS_CHANGED,
                0,
                0);
  Result := S_OK;
end;


function TAudioEndpointNotificationClient.OnDeviceStateChanged(pwstrDeviceId: LPCWSTR;
                                                               dwNewState: DWORD): HResult;
begin

  if (FWnd <> 0) then
    PostMessage(FWnd,
                WM_RDJ_ENDPOINTS_CHANGED,
                0,
                0);
  Result := S_OK;
end;


function TAudioEndpointNotificationClient.OnPropertyValueChanged(pwstrDeviceId: LPCWSTR;
                                                                 const key: PROPERTYKEY): HResult;
begin

  if (FWnd <> 0) then
    PostMessage(FWnd,
                WM_RDJ_ENDPOINTS_CHANGED,
                0,
                0);
  Result := S_OK;
end;

// Endpoint & Deviceloss -------------------------------------------------------

procedure TMainMDIFrm.SetupEndpointNotifications();
begin

  if Assigned(FEndpointEnumerator) then
    Exit;

  FEndpointEnumerator := CreateComObject(CLSID_MMDeviceEnumerator) as IMMDeviceEnumerator;
  FEndpointNotifyClient := TAudioEndpointNotificationClient.Create(Handle);
  FEndpointEnumerator.RegisterEndpointNotificationCallback(FEndpointNotifyClient);
end;


procedure TMainMDIFrm.TeardownEndpointNotifications();
begin

  if Assigned(FEndpointEnumerator) and Assigned(FEndpointNotifyClient) then
    begin

      try

        FEndpointEnumerator.UnregisterEndpointNotificationCallback(FEndpointNotifyClient);
      except
        // Do nothing
      end;
    end;

  FEndpointNotifyClient := nil;
  FEndpointEnumerator := nil;
end;


//procedure TMainMDIFrm.QueueEndpointRefresh();
//begin

//  if FEndpointRefreshPending then
//    Exit;

//  FEndpointRefreshPending := True;
//  PostMessage(Handle,
//              WM_RDJ_ENDPOINTS_CHANGED,
//              0,
//              0);
//end;


procedure TMainMDIFrm.WMEndpointsChanged(var Msg: TMessage);
var
  OldMasterAvailable: Boolean;
  OldPFLAvailable: Boolean;

begin

  FEndpointRefreshPending := False;

  OldMasterAvailable := FMasterEndpointAvailable;
  OldPFLAvailable := FPFLEndpointAvailable;

  RefreshEndpointAvailability();

  if (OldMasterAvailable <> FMasterEndpointAvailable) or
     (OldPFLAvailable <> FPFLEndpointAvailable) then
    HardRestartAudioGraph();
end;


procedure TMainMDIFrm.WMPowerBroadcast(var Msg: TMessage);
var
  EventCode: Cardinal;
  EventName: string;

begin

  EventCode := Cardinal(Msg.WParam);

  case EventCode of
    RDJ_PBT_APMSUSPEND:
      EventName := 'PBT_APMSUSPEND';
    RDJ_PBT_APMRESUMECRITICAL:
      EventName := 'PBT_APMRESUMECRITICAL';
    RDJ_PBT_APMRESUMESUSPEND:
      EventName := 'PBT_APMRESUMESUSPEND';
    RDJ_PBT_APMRESUMEAUTOMATIC:
      EventName := 'PBT_APMRESUMEAUTOMATIC';
  else
    EventName := 'PBT_' + IntToStr(EventCode);
  end;

  OutputDebugString(PChar(Format('RDJ Pro power event: %s (%d)',
                                 [EventName,
                                  EventCode])));

  case EventCode of
    RDJ_PBT_APMRESUMECRITICAL,
    RDJ_PBT_APMRESUMESUSPEND,
    RDJ_PBT_APMRESUMEAUTOMATIC:
      begin
        ApplyRdjExecutionState();
        QueueAudioGraphRecovery(EventName,
                                2500);
      end;
  end;

  Msg.Result := 1;
end;


function TMainMDIFrm.IsEndpointUsable(const ADeviceId: string): Boolean;
begin

  Result := EndpointIdExists(ADeviceId);
end;


function TMainMDIFrm.ResolveMasterEndpointAvailable(): Boolean;
begin

  if (Trim(FSetup.MasterDeviceId) = '') then
    Exit(True);

  Result := IsEndpointUsable(FSetup.MasterDeviceId);
end;


function TMainMDIFrm.ResolvePFLEndpointAvailable(): Boolean;
begin

  if not FSetup.PFLEnabled then
    Exit(False);

  if (Trim(FSetup.PFLDeviceId) = '') then
    Exit(False);

  Result := IsEndpointUsable(FSetup.PFLDeviceId);
end;


procedure TMainMDIFrm.RefreshEndpointAvailability();
begin

  FMasterEndpointAvailable := ResolveMasterEndpointAvailable();
  FPFLEndpointAvailable := ResolvePFLEndpointAvailable();
end;

// Main lifecycle ==============================================================

// DEBUG: let's see what is causing an error on adress 00005000
{procedure DumpFormDestroyHandlers;
var
  I: Integer;
  M: TMethod;
  F: TCustomForm;

begin

  for I := 0 to Screen.FormCount - 1 do
    begin

      F := Screen.Forms[I];

      M := TMethod(TCustomFormAccess(F).OnDestroy);

      OutputDebugString(PChar(Format(
        'FORM %s.%s OnDestroy Code=%p Data=%p',
        [F.ClassName,
         F.Name,
         M.Code,
         M.Data])));
    end;
end;
}

procedure TMainMDIFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  CanClose := False;

  FAppClosing := True;

  tmrClock.Enabled:= False;
  RestoreLidClosePolicyOverride();

  lblLocalTime.Caption := 'Closing RDJ Pro, please wait...';
  lblLocalTime.Repaint;
  Sleep(1000);

  if Assigned(FMediaServer) then
    FMediaServer.Free;

  if Assigned(FPlayListEditor) then
    FreeAndNil(FPlayListEditor);

  RDJShutdownPlaylistDbFireDAC();

  Sleep(1000);

  CanClose := True
end;


procedure TMainMDIFrm.ApplyRdjExecutionState();
var
  NewState: DWORD;
  PrevState: DWORD;

begin

  NewState := ES_CONTINUOUS or
              ES_SYSTEM_REQUIRED or
              ES_AWAYMODE_REQUIRED;

  PrevState := SetThreadExecutionState(NewState);
  if (PrevState <> 0) then
    begin
      OutputDebugString(PChar(Format(
        'RDJ Pro power request active: state=$%.8x previous=$%.8x',
        [Cardinal(NewState),
         Cardinal(PrevState)])));
      Exit;
    end;

  NewState := ES_CONTINUOUS or
              ES_SYSTEM_REQUIRED;

  PrevState := SetThreadExecutionState(NewState);
  if (PrevState = 0) then
    OutputDebugString(PChar('RDJ Pro could not prevent Windows system sleep.'))
  else
    OutputDebugString(PChar(Format(
      'RDJ Pro power request active without away mode: state=$%.8x previous=$%.8x',
      [Cardinal(NewState),
       Cardinal(PrevState)])));
end;


procedure TMainMDIFrm.ApplyLidClosePolicyOverride();
var
  Hr: DWORD;
  Scheme: PGUID;
  AcValue: DWORD;
  DcValue: DWORD;

begin

  if not FSetup.SystemOverrideSleepMode then
    Exit;

  if FLidPolicyOverrideActive then
    Exit;

  Scheme := nil;
  Hr := PowerGetActiveScheme(0,
                             Scheme);
  if (Hr <> ERROR_SUCCESS) or
     (Scheme = nil) then
    begin
      OutputDebugString(PChar(Format('RDJ Pro lid close override failed: PowerGetActiveScheme error=%d',
                                     [Hr])));
      WarnLidCloseOverrideFailedOnce(Format('PowerGetActiveScheme failed: %d',
                                            [Hr]));
      Exit;
    end;

  Hr := PowerReadACValueIndex(0,
                              Scheme,
                              @GUID_SYSTEM_BUTTON_SUBGROUP,
                              @GUID_LIDCLOSE_ACTION,
                              AcValue);
  if Hr <> ERROR_SUCCESS then
    begin
      LocalFree(HLOCAL(Scheme));
      OutputDebugString(PChar(Format('RDJ Pro lid close override failed: PowerReadACValueIndex error=%d',
                                     [Hr])));
      WarnLidCloseOverrideFailedOnce(Format('PowerReadACValueIndex failed: %d',
                                            [Hr]));
      Exit;
    end;

  Hr := PowerReadDCValueIndex(0,
                              Scheme,
                              @GUID_SYSTEM_BUTTON_SUBGROUP,
                              @GUID_LIDCLOSE_ACTION,
                              DcValue);
  if Hr <> ERROR_SUCCESS then
    begin
      LocalFree(HLOCAL(Scheme));
      OutputDebugString(PChar(Format('RDJ Pro lid close override failed: PowerReadDCValueIndex error=%d',
                                     [Hr])));
      WarnLidCloseOverrideFailedOnce(Format('PowerReadDCValueIndex failed: %d',
                                            [Hr]));
      Exit;
    end;

  Hr := PowerWriteACValueIndex(0,
                               Scheme,
                               @GUID_SYSTEM_BUTTON_SUBGROUP,
                               @GUID_LIDCLOSE_ACTION,
                               RDJ_POWER_ACTION_DO_NOTHING);
  if Hr <> ERROR_SUCCESS then
    begin
      LocalFree(HLOCAL(Scheme));
      OutputDebugString(PChar(Format('RDJ Pro lid close override failed: PowerWriteACValueIndex error=%d',
                                     [Hr])));
      WarnLidCloseOverrideFailedOnce(Format('PowerWriteACValueIndex failed: %d',
                                            [Hr]));
      Exit;
    end;

  Hr := PowerWriteDCValueIndex(0,
                               Scheme,
                               @GUID_SYSTEM_BUTTON_SUBGROUP,
                               @GUID_LIDCLOSE_ACTION,
                               RDJ_POWER_ACTION_DO_NOTHING);
  if Hr <> ERROR_SUCCESS then
    begin
      LocalFree(HLOCAL(Scheme));
      OutputDebugString(PChar(Format('RDJ Pro lid close override failed: PowerWriteDCValueIndex error=%d',
                                     [Hr])));
      WarnLidCloseOverrideFailedOnce(Format('PowerWriteDCValueIndex failed: %d',
                                            [Hr]));
      Exit;
    end;

  Hr := PowerSetActiveScheme(0,
                             Scheme);
  if Hr <> ERROR_SUCCESS then
    begin
      LocalFree(HLOCAL(Scheme));
      OutputDebugString(PChar(Format('RDJ Pro lid close override failed: PowerSetActiveScheme error=%d',
                                     [Hr])));
      WarnLidCloseOverrideFailedOnce(Format('PowerSetActiveScheme failed: %d',
                                            [Hr]));
      Exit;
    end;

  FPowerPolicyScheme := Scheme;
  FOriginalLidActionAc := AcValue;
  FOriginalLidActionDc := DcValue;
  FLidPolicyOverrideActive := True;

  OutputDebugString(PChar(Format(
    'RDJ Pro lid close override active: originalAC=%d originalDC=%d newAction=%d',
    [AcValue,
     DcValue,
     RDJ_POWER_ACTION_DO_NOTHING])));
end;


procedure TMainMDIFrm.WarnLidCloseOverrideFailedOnce(const AReason: string);
begin

  if FLidPolicyWarningShown then
    Exit;

  FLidPolicyWarningShown := True;

  MessageDlg('RDJ Pro could not override the Windows lid-close power setting.' + sLineBreak +
             'If the laptop lid is closed, Windows may still sleep, hibernate, or shut down.' + sLineBreak +
             sLineBreak +
             AReason,
             mtWarning,
             [mbOK],
             0);
end;


procedure TMainMDIFrm.RestoreLidClosePolicyOverride();
var
  HrAc: DWORD;
  HrDc: DWORD;
  HrApply: DWORD;

begin

  if not FLidPolicyOverrideActive then
    Exit;

  if (FPowerPolicyScheme = nil) then
    begin

      FLidPolicyOverrideActive := False;
      Exit;
    end;

  HrAc := PowerWriteACValueIndex(0,
                                 FPowerPolicyScheme,
                                 @GUID_SYSTEM_BUTTON_SUBGROUP,
                                 @GUID_LIDCLOSE_ACTION,
                                 FOriginalLidActionAc);

  HrDc := PowerWriteDCValueIndex(0,
                                 FPowerPolicyScheme,
                                 @GUID_SYSTEM_BUTTON_SUBGROUP,
                                 @GUID_LIDCLOSE_ACTION,
                                 FOriginalLidActionDc);

  HrApply := PowerSetActiveScheme(0,
                                  FPowerPolicyScheme);

  OutputDebugString(PChar(Format(
    'RDJ Pro lid close override restore: ac=%d dc=%d apply=%d originalAC=%d originalDC=%d',
    [HrAc,
     HrDc,
     HrApply,
     FOriginalLidActionAc,
     FOriginalLidActionDc])));

  LocalFree(HLOCAL(FPowerPolicyScheme));
  FPowerPolicyScheme := nil;
  FLidPolicyOverrideActive := False;
end;


procedure TMainMDIFrm.AttachAudioOutputHandlers(const AOutput: TMfWasApiRenderOutputEngine);
begin

  if Assigned(AOutput) then
    AOutput.OnError := AudioRenderError;
end;


procedure TMainMDIFrm.AudioRenderError(Sender: TObject;
                                      const Hr: HRESULT;
                                      const Msg: string);
begin

  OutputDebugString(PChar(Format(
    'RDJ Pro audio render error: hr=0x%.8x msg=%s',
    [Cardinal(Hr),
     Msg])));

  QueueAudioGraphRecovery(Format('render error hr=0x%.8x %s',
                                 [Cardinal(Hr),
                                  Msg]),
                          1500);
end;


procedure TMainMDIFrm.QueueAudioGraphRecovery(const AReason: string;
                                             const ADelayMs: Cardinal);
var
  Reason: string;
  QueueProc: TThreadProcedure;

begin

  if FAppClosing then
    Exit;

  Reason := Trim(AReason);
  if (Reason = '') then
    Reason := 'audio graph recovery requested';

  if TInterlocked.CompareExchange(FAudioGraphRecoveryQueued,
                                  1,
                                  0) <> 0 then
    Exit;

  QueueProc := procedure
               begin

                 FAudioGraphRecoveryReason := Reason;
                 FAudioGraphRecoveryDueTick := GetTickCount64() + UInt64(ADelayMs);
                 OutputDebugString(PChar(Format('RDJ Pro audio graph recovery queued: delayMs=%d reason=%s',
                                                [ADelayMs,
                                                 Reason])));
               end;
  TThread.Queue(nil,
                QueueProc);
end;


procedure TMainMDIFrm.ServiceAudioGraphRecovery();
var
  NowTick: UInt64;
  Reason: string;

begin

  if (FAudioGraphRecoveryQueued = 0) then
    Exit;

  if FAppClosing or
     (not FUiInitialized) then
    Exit;

  NowTick := GetTickCount64();
  if (FAudioGraphRecoveryDueTick = 0) or
     (NowTick < FAudioGraphRecoveryDueTick) then
    Exit;

  Reason := FAudioGraphRecoveryReason;

  try

    OutputDebugString(PChar('RDJ Pro audio graph recovery starting: ' + Reason));
    HardRestartAudioGraph();

    if Assigned(FMediaServer) then
      FMediaServer.RecoverBroadcastAfterAudioGraphRestart(Reason);

    OutputDebugString(PChar('RDJ Pro audio graph recovery completed: ' + Reason));

    FAudioGraphRecoveryReason := '';
    FAudioGraphRecoveryDueTick := 0;
    TInterlocked.Exchange(FAudioGraphRecoveryQueued,
                          0);
  except

    on E: Exception do
      begin

        FAudioGraphRecoveryDueTick := NowTick + 5000;
        OutputDebugString(PChar('RDJ Pro audio graph recovery failed, retrying: ' +
                                E.ClassName + ': ' + E.Message));
      end;
  end;
end;


procedure TMainMDIFrm.FormCreate(Sender: TObject);
begin

  FAudioGraphRecoveryQueued := 0;
  FAudioGraphRecoveryDueTick := 0;
  FAudioGraphRecoveryReason := '';
  FCaddyListenerCount := -1;
  FLastCaddyListenerRefreshTick := 0;
  FNowPlayingArtist := '';
  FNowPlayingTitle := '';
  FNowPlayingFromChannelDeck := False;
  FPowerPolicyScheme := nil;
  FLidPolicyOverrideActive := False;
  FLidPolicyWarningShown := False;
  FOriginalLidActionAc := 0;
  FOriginalLidActionDc := 0;

  // Keep audio, video and network broadcasting active while RDJ Pro is running.
  // The display may still switch off according to the Windows power settings.
  ApplyRdjExecutionState();

  // Do not create childforms here!
end;


procedure TMainMDIFrm.FormDestroy(Sender: TObject);
var
  i: Integer;

begin

  SetThreadExecutionState(ES_CONTINUOUS);
  RestoreLidClosePolicyOverride();
  RDJShutdownPlaylistDbFireDAC();

  TeardownEndpointNotifications();

  FreeAndNil(FfrmMasterFxRack);

  StopInternalMixerRecording();
  FreeAndNil(FInternalMixRecorderPreFx);
  FreeAndNil(FInternalMixRecorderPostFx);

  FreeAndNil(FMicrophoneDeck);

  FreeAndNil(FCueOut);
  FreeAndNil(FMasterOut);
  FreeAndNil(FInternalMixer);

  for i := Low(FMasterFxObjects) to High(FMasterFxObjects) do
    FreeAndNil(FMasterFxObjects[i]);

  SetLength(FMasterFxObjects,
            0);

  FreeAndNil(FMasterLowMidHighEq);
  FreeAndNil(FMasterFlangerEcho);
  FreeAndNil(FMasterCompLim);
  FreeAndNil(FMasterFXRack);

  FreeAndNil(FRDJRadioStatusJson);

  DeleteCriticalSection(FCueBufferLock);
end;


// GUI shell behavior:
// - Fixed baseline window size (no user resizing)
// - Optional fullscreen toggle (F11)
// - Horizontal scrollbar to scroll a virtual strip of MDI child decks.
procedure TMainMDIFrm.ConstructEngine();
begin

  KeyPreview := True;

  FSetupFileName := GetDefaultSetupFileName();
  LoadSetupFromIni(FSetupFileName,
                   FSetup);
  ApplyLidClosePolicyOverride();

  // IMPORTANT:
  //  Keep stored cue device + PFLEnabled untouched.
  //  Only master may safely fall back to default by clearing invalid ID.
  ResetInvalidEndpointAssignments();
  SetupEndpointNotifications();
  RefreshEndpointAvailability();

  FMasterFXRack := TMfWasApiEffectsRack.Create(Self);

  FMicrophoneDeck := nil;
  FMasterLowMidHighEq := nil;
  FMasterFlangerEcho := nil;
  FMasterCompLim := nil;
  FfrmMasterFxRack := nil;
  StopInternalMixerRecording();
  FreeAndNil(FInternalMixRecorderPreFx);
  FreeAndNil(FInternalMixRecorderPostFx);

  FInternalMixer := TRDJInternalMixer.Create();
  FInternalMixer.SetFormat(44100,
                           2);

  BuildMasterRackFromSetup();

  FMasterOut := TMfWasApiRenderOutputEngine.Create();
  FMasterOut.OnFillPcm := MasterOutFillPcm;
  AttachAudioOutputHandlers(FMasterOut);

  FCueOut := nil;
  InitializeCriticalSection(FCueBufferLock);
  FCueQueueFramesCapacity := 0;
  FCueQueueReadFrame := 0;
  FCueQueueWriteFrame := 0;
  FCueQueueValidFrames := 0;
end;


procedure TMainMDIFrm.FormResize(Sender: TObject);
begin

  if FAppClosing then
    Exit;

  TileDecks();
  //lblLocalTime.Left := (ClientWidth div 2) - (lblLocalTime.Width div 2);

  if (WindowState = wsMaximized) then
    AlignMasterDeckWithFxRack();

  Invalidate;
end;


procedure TMainMDIFrm.FormShow(Sender: TObject);
var
  hr: HRESULT;
  wfx: TWAVEFORMATEX;

begin

  if FUiInitialized then
    Exit;

  // Windows DWM
  //ApplyDarkWindowFrame(Handle);

  // a better option to create our own form caption.
  // No form caption
  SetWindowLong(Handle,
                GWL_STYLE,
                GetWindowLong(Handle,
                              GWL_STYLE) and not WS_CAPTION);

  Width := BASE_MIN_W;
  Height := BASE_MIN_H;

  FAppClosing := False;

  ConstructEngine();
  FUiInitialized := True;

  if FileExists(FSetupFileName) then
    ApplySetupOnce()
  else
    OpenSetupGUI();

  hr := BuildRenderWaveFormat(wfx);
  if FAILED(hr) then
    raise Exception.CreateFmt('BuildRenderWaveFormat failed: $%.8x',
                              [Cardinal(hr)]);

  RefreshEndpointAvailability();
  ConfigureOutputEndpoints();

  hr := FMasterOut.Prepare(@wfx);
  if FAILED(hr) then
    begin

      ClearAudioEndpointSetup();

      MessageDlg('The stored master audio endpoint is no longer valid or cannot be opened.' + sLineBreak +
                 'The audio endpoint settings have been reset. Please select valid devices in Setup.',
                 mtWarning,
                 [mbOK],
                 0);

      OpenSetupGUI();
      RefreshEndpointAvailability();
      ConfigureOutputEndpoints();

      hr := FMasterOut.Prepare(@wfx);
      if FAILED(hr) then
        raise Exception.CreateFmt('FMasterOut.Prepare failed after endpoint reset: $%.8x',
                                  [Cardinal(hr)]);
    end;

  if Assigned(FCueOut) then
    begin

      hr := FCueOut.Prepare(@wfx);
      if FAILED(hr) then
        begin

          // KEEP the stored cue settings.
          // Runtime failure only; no destructive config mutation.
          FreeAndNil(FCueOut);

          if FSetup.PFLEnabled and (Trim(FSetup.PFLDeviceId) <> '') then
            MessageDlg('The stored cue/PFL audio endpoint is currently unavailable or cannot be opened.' + sLineBreak +
                       'Cue stays disabled for this run, but the stored cue device setting is preserved.',
                       mtWarning,
                       [mbOK],
                       0);
        end;
    end;

  if Assigned(FCueOut) and
     (FMasterOut.BufferFrameCount > 0) and
     (FCueOut.BufferFrameCount > 0) then
    InitCueQueue(Max(Integer(FMasterOut.BufferFrameCount),
                     Integer(FCueOut.BufferFrameCount)))
  else
    InitCueQueue(Integer(FMasterOut.BufferFrameCount));

  EnsureCueWorkBuf(Integer(FMasterOut.BufferFrameCount));
  FInternalMixer.PrepareWorkBuffers(Integer(FMasterOut.BufferFrameCount));

  hr := FMasterOut.Start();
  if FAILED(hr) then
    raise Exception.CreateFmt('FMasterOut.Start failed: $%.8x',
                              [Cardinal(hr)]);

  if Assigned(FCueOut) then
    begin

      hr := FCueOut.Start();
      if FAILED(hr) then
        raise Exception.CreateFmt('FCueOut.Start failed: $%.8x',
                                  [Cardinal(hr)]);
    end;

  FRDJRadioStatusJson := TRDJRadioStatusJson.Create;

  FCoverFileName := '';
  LoadBroadcastIdentityFromIni();
  PublishSelectedCover();
  WriteNowPlayingStatus(FDjName,
                        FShowName,
                        FNowPlayingArtist,
                        FNowPlayingTitle,
                        BuildCoverJsonUrl(True),
                        FCaddyListenerCount,
                        True);
end;



// Setup / deck creation ======================================================

procedure TMainMDIFrm.OpenSetupGUI();
begin

  if TfrmSetup.Execute(FSetup) then
    begin

      SaveSetupToIni(FSetupFileName,
                     FSetup);

      ApplySetupOnce();

      if FUiInitialized then
        HardRestartAudioGraph();
    end;
end;


// Media Server
function TMainMDIFrm.IsMediaServerBroadcasting(): Boolean;
begin

  Result := Assigned(FMediaServer) and
            FMediaServer.IsBroadcasting();
end;


procedure TMainMDIFrm.SetMediaServerButtonChecked(const AChecked: Boolean);
begin

  chkMediaServer.Checked := AChecked;
  if not AChecked then
    btnSetup.Checked := False;
end;


procedure TMainMDIFrm.RefreshMainButtonStates();
var
  MediaServerShown: Boolean;
  Broadcasting: Boolean;
begin

  Broadcasting := IsMediaServerBroadcasting();
  btnSetup.Enabled := not Broadcasting;
  if Broadcasting then
    btnSetup.Checked := False;

  MediaServerShown := Assigned(FMediaServer) and
                      FMediaServer.Visible and
                      (FMediaServer.WindowState <> wsMinimized);
  SetMediaServerButtonChecked(MediaServerShown);
end;


procedure TMainMDIFrm.OpenMediaServerGUI();
begin

  if not assigned(FMediaServer) then
    FMediaServer := TfrmMediaServer.Create(Self);

  FMediaServer.Show;
  FMediaServer.WindowState := wsNormal;
  FMediaServer.BringToFront();
  RefreshMainButtonStates();
end;


// Drag form
procedure TMainMDIFrm.pnlCaptionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;

begin

  if (Button = mbLeft) then
    begin

      ReleaseCapture();
      Perform(WM_SYSCOMMAND,
              SC_DRAGMOVE,
              0);
    end;

end;


// Clear owned FX references.
procedure TMainMDIFrm.ClearMasterFxObjects();
var
  i: Integer;

begin

  if Assigned(FMasterFXRack) then
    FMasterFXRack.Slots.Clear;

  for i := Low(FMasterFxObjects) to High(FMasterFxObjects) do
    FreeAndNil(FMasterFxObjects[i]);

  SetLength(FMasterFxObjects,
            0);
end;

// FX factory.
function TMainMDIFrm.CreateFxByKind(const AFxKind: TRDJFxKind): TMfWasApiFxComponentBase;
begin

  case AFxKind of
    fxkParametricEq:
      Result := TMfParametricEqEffect.Create(nil);

    fxkCompressorLimiter:
      Result := TMfCompressorLimiterEffect.Create(nil);
  else
    Result := nil;
  end;
end;

// Add slot.
procedure TMainMDIFrm.AddMasterRackSlot(AEffect: TMfWasApiFxComponentBase;
                                        const AEnabled: Boolean);
var
  Slot: TMfWasApiFxSlot;
  i: Integer;

begin

  if (AEffect = nil) or
     not Assigned(FMasterFXRack) then
    Exit;

  i := Length(FMasterFxObjects);
  SetLength(FMasterFxObjects,
            i + 1);
  FMasterFxObjects[i] := AEffect;

  Slot := FMasterFXRack.Slots.Add as TMfWasApiFxSlot;
  Slot.Enabled := AEnabled;
  Slot.Effect := AEffect;
end;


// Build the rack from setup.
procedure TMainMDIFrm.BuildMasterRackFromSetup();
var
  i: Integer;
  Fx: TMfWasApiFxComponentBase;
  Slot: TMfWasApiFxSlot;

begin

  if not Assigned(FMasterFXRack) then
    Exit;

  ClearMasterFxObjects();
  FreeAndNil(FMasterLowMidHighEq);
  FreeAndNil(FMasterFlangerEcho);
  FreeAndNil(FMasterCompLim);

  // Always-present master 3-band EQ comes first in the end-FX chain.
  FMasterLowMidHighEq := TMfLowMidHighEqEffect.Create(nil);
  FMasterLowMidHighEq.Enabled := True;

  Slot := FMasterFXRack.Slots.Add as TMfWasApiFxSlot;
  Slot.Enabled := True;
  Slot.Effect := FMasterLowMidHighEq;

  // Always-present master Flanger / Echo comes after the EQ.
  FMasterFlangerEcho := TMfFlangerEchoEffect.Create(nil);
  FMasterFlangerEcho.Enabled := False;   // Keep flanger initial to false.

  Slot := FMasterFXRack.Slots.Add as TMfWasApiFxSlot;
  Slot.Enabled := True;
  Slot.Effect := FMasterFlangerEcho;

  // Always-present master Compressor / Limiter comes after the Flanger / Echo.
  FMasterCompLim := TMfCompressorLimiterEffect.Create(nil);
  FMasterCompLim.Enabled := False;

  Slot := FMasterFXRack.Slots.Add as TMfWasApiFxSlot;
  Slot.Enabled := True;
  Slot.Effect := FMasterCompLim;

  for i := 0 to FSetup.MasterFxRack.Count - 1 do
    begin

      Fx := CreateFxByKind(FSetup.MasterFxRack.Slots[i].FxKind);
      if (Fx <> nil) then
        AddMasterRackSlot(Fx,
                          FSetup.MasterFxRack.Slots[i].Enabled);
    end;

  if Assigned(FfrmMasterFxRack) then
    begin
      FfrmMasterFxRack.BindMasterFx(FMasterFXRack,
                                    FMasterLowMidHighEq,
                                    FMasterFlangerEcho,
                                    FMasterCompLim);
      FfrmMasterFxRack.LoadGuiFromEffects();
    end;
end;


procedure TMainMDIFrm.ProcessMasterFx(const pData: PSingle;
                                      const Frames: Integer;
                                      const ASampleRate: Integer);
begin

  if (pData = nil) or
     (Frames <= 0) or
     (ASampleRate <= 0) then
    Exit;

  if Assigned(FMasterFXRack) then
    FMasterFXRack.ProcessFloat32(pData,
                                 Frames,
                                 2,
                                 ASampleRate);
end;



// Json ------------------------------------------------------------------------


function TMainMDIFrm.ResolveCaddyLogFileName(): string;
var
  BaseDir: string;

begin

  Result := Trim(FSetup.CaddyLogFile);
  if (Result = '') then
    Exit;

  if ((Length(Result) >= 2) and (Result[2] = ':')) or
     (Copy(Result,
           1,
           2) = '\\') then
    Exit;

  BaseDir := Trim(FSetup.CaddyDir);
  if (BaseDir = '') then
    BaseDir := ExtractFilePath(Trim(FSetup.CaddyConfigFile));

  if (BaseDir = '') then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(BaseDir) + Result;
end;


function TMainMDIFrm.ReadCaddyActiveListenerCount(): Integer;
var
  LogFileName: string;
  Stream: TFileStream;
  Bytes: TBytes;
  ReadSize: Integer;
  Text: string;
  Lines: TStringList;
  Clients: TStringList;
  I: Integer;
  Line: string;
  JsonValue: TJSONValue;
  Json: TJSONObject;
  RequestJson: TJSONObject;
  Value: TJSONValue;
  Uri: string;
  ClientIp: string;
  TsText: string;
  LogTs: Double;
  NowUtc: TDateTime;
  NowUnix: Double;

  function JsonText(const AJson: TJSONObject;
                    const AName: string): string;
  var
    V: TJSONValue;

  begin

    Result := '';
    if not Assigned(AJson) then
      Exit;

    V := AJson.GetValue(AName);
    if Assigned(V) then
      Result := V.Value;
  end;

  function RelevantUri(const AUri: string): Boolean;
  var
    S: string;

  begin

    S := LowerCase(Trim(AUri));
    Result := (Copy(S,
                    1,
                    8) = '/stream/') or
              (Copy(S,
                    1,
                    7) = '/video/') or
              (Copy(S,
                    1,
                    5) = '/live') or
              (Copy(S,
                    1,
                    11) = '/video.mjpg');
  end;

  function CleanClientIp(const AValue: string): string;
  var
    P: Integer;
    S: string;

  begin

    S := Trim(AValue);
    if (S = '') then
      Exit('');

    if (Length(S) > 0) and (S[1] = '[') then
      begin
        P := Pos(']',
                 S);
        if P > 0 then
          Exit(Copy(S,
                    2,
                    P - 2));
      end;

    P := Pos(':',
             S);
    if P > 0 then
      S := Copy(S,
                1,
                P - 1);

    Result := S;
  end;

begin

  Result := -1;

  LogFileName := ResolveCaddyLogFileName();
  if (LogFileName = '') or not FileExists(LogFileName) then
    Exit;

  Stream := TFileStream.Create(LogFileName,
                               fmOpenRead or fmShareDenyNone);
  try
    ReadSize := Stream.Size;
    if ReadSize > RDJ_CADDY_LOG_TAIL_BYTES then
      ReadSize := RDJ_CADDY_LOG_TAIL_BYTES;

    if ReadSize <= 0 then
      Exit;

    Stream.Position := Stream.Size - ReadSize;
    SetLength(Bytes,
              ReadSize);
    Stream.ReadBuffer(Bytes[0],
                      ReadSize);
  finally
    Stream.Free;
  end;

  Text := TEncoding.UTF8.GetString(Bytes);

  Lines := TStringList.Create;
  Clients := TStringList.Create;
  try
    Lines.Text := Text;
    Clients.Sorted := True;
    Clients.Duplicates := dupIgnore;

    NowUtc := TTimeZone.Local.ToUniversalTime(Now);
    NowUnix := (NowUtc - EncodeDate(1970,
                                    1,
                                    1)) * SecsPerDay;

    for I := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[I]);
        if (Line = '') or (Line[1] <> '{') then
          Continue;

        JsonValue := TJSONObject.ParseJSONValue(Line);
        try
          if not (JsonValue is TJSONObject) then
            Continue;

          Json := TJSONObject(JsonValue);
          RequestJson := nil;

          Value := Json.GetValue('request');
          if Value is TJSONObject then
            RequestJson := TJSONObject(Value);

          Uri := JsonText(RequestJson,
                          'uri');
          if Uri = '' then
            Uri := JsonText(Json,
                            'uri');

          if not RelevantUri(Uri) then
            Continue;

          TsText := JsonText(Json,
                             'ts');
          LogTs := StrToFloatDef(StringReplace(TsText,
                                               '.',
                                               FormatSettings.DecimalSeparator,
                                               []),
                                 0);

          if (LogTs > 0) and
             ((NowUnix - LogTs) > RDJ_CADDY_ACTIVE_LISTENER_SECONDS) then
            Continue;

          ClientIp := CleanClientIp(JsonText(RequestJson,
                                            'client_ip'));
          if ClientIp = '' then
            ClientIp := CleanClientIp(JsonText(RequestJson,
                                              'remote_ip'));
          if ClientIp = '' then
            ClientIp := CleanClientIp(JsonText(Json,
                                              'remote_ip'));

          if ClientIp <> '' then
            Clients.Add(ClientIp);
        finally
          JsonValue.Free;
        end;
      end;

    Result := Clients.Count;
  finally
    Clients.Free;
    Lines.Free;
  end;
end;


procedure TMainMDIFrm.RefreshCaddyListenerCount(const AForceJsonUpdate: Boolean);
var
  NewCount: Integer;
  OldCount: Integer;

begin

  OldCount := FCaddyListenerCount;
  NewCount := ReadCaddyActiveListenerCount();
  if NewCount < 0 then
    Exit;

  FCaddyListenerCount := NewCount;

  if AForceJsonUpdate and (NewCount <> OldCount) then
    WriteNowPlayingStatus(FDjName,
                          FShowName,
                          FNowPlayingArtist,
                          FNowPlayingTitle,
                          BuildCoverJsonUrl(True),
                          FCaddyListenerCount);
end;



function EncodeBroadcastIdentityText(const AValue: string): string;
begin

  Result := StringReplace(AValue,
                          '\',
                          '\\',
                          [rfReplaceAll]);
  Result := StringReplace(Result,
                          #13#10,
                          '\n',
                          [rfReplaceAll]);
  Result := StringReplace(Result,
                          #13,
                          '\n',
                          [rfReplaceAll]);
  Result := StringReplace(Result,
                          #10,
                          '\n',
                          [rfReplaceAll]);
end;


function DecodeBroadcastIdentityText(const AValue: string): string;
var
  i: Integer;

begin

  Result := '';
  i := 1;
  while i <= Length(AValue) do
    begin

      if (AValue[i] = '\') and
         (i < Length(AValue)) then
        begin

          Inc(i);
          case AValue[i] of
            'n':
              Result := Result + sLineBreak;
            '\':
              Result := Result + '\';
          else
            Result := Result + '\' + AValue[i];
          end;
        end
      else
        Result := Result + AValue[i];

      Inc(i);
    end;
end;


procedure TMainMDIFrm.LoadBroadcastIdentityFromIni();
var
  IniFile: TIniFile;
  Value: string;

begin

  FDjName := Trim(mmoDjName.Text);
  FShowName := Trim(mmoShow.Text);

  if (Trim(FSetupFileName) = '') or
     (not FileExists(FSetupFileName)) then
    Exit;

  IniFile := TIniFile.Create(FSetupFileName);
  try

    if IniFile.ValueExists(RDJ_BROADCAST_IDENTITY_SECTION,
                           RDJ_BROADCAST_IDENTITY_DJ_NAME) then
      begin

        Value := IniFile.ReadString(RDJ_BROADCAST_IDENTITY_SECTION,
                                    RDJ_BROADCAST_IDENTITY_DJ_NAME,
                                    '');
        FDjName := DecodeBroadcastIdentityText(Value);
        mmoDjName.Text := FDjName;
      end;

    if IniFile.ValueExists(RDJ_BROADCAST_IDENTITY_SECTION,
                           RDJ_BROADCAST_IDENTITY_SHOW_NAME) then
      begin

        Value := IniFile.ReadString(RDJ_BROADCAST_IDENTITY_SECTION,
                                    RDJ_BROADCAST_IDENTITY_SHOW_NAME,
                                    '');
        FShowName := DecodeBroadcastIdentityText(Value);
        mmoShow.Text := FShowName;
      end;

    FCoverFileName := Trim(IniFile.ReadString(RDJ_BROADCAST_IDENTITY_SECTION,
                                              RDJ_BROADCAST_IDENTITY_COVER_FILE,
                                              FCoverFileName));
    if (FCoverFileName <> '') then
      begin

        if FileExists(FCoverFileName) then
          begin

            try
              imgDjShowLogo.Picture.LoadFromFile(FCoverFileName);
            except

              on E: Exception do
                begin

                  OutputDebugString(PChar('RDJ Pro could not load stored show logo: ' +
                                          E.ClassName + ': ' + E.Message));
                  FCoverFileName := '';
                end;
            end;
          end
        else
          FCoverFileName := '';
      end;
  finally

    IniFile.Free;
  end;
end;


procedure TMainMDIFrm.SaveBroadcastIdentityToIni();
var
  IniFile: TIniFile;

begin

  if Trim(FSetupFileName) = '' then
    Exit;

  FDjName := Trim(mmoDjName.Text);
  FShowName := Trim(mmoShow.Text);

  IniFile := TIniFile.Create(FSetupFileName);
  try

    IniFile.WriteString(RDJ_BROADCAST_IDENTITY_SECTION,
                        RDJ_BROADCAST_IDENTITY_DJ_NAME,
                        EncodeBroadcastIdentityText(FDjName));
    IniFile.WriteString(RDJ_BROADCAST_IDENTITY_SECTION,
                        RDJ_BROADCAST_IDENTITY_SHOW_NAME,
                        EncodeBroadcastIdentityText(FShowName));
    IniFile.WriteString(RDJ_BROADCAST_IDENTITY_SECTION,
                        RDJ_BROADCAST_IDENTITY_COVER_FILE,
                        Trim(FCoverFileName));
  finally

    IniFile.Free;
  end;
end;
function TMainMDIFrm.BuildCoverJsonUrl(const APreferCurrent: Boolean): string;
var
  BaseUrl: string;
  CaddyPath: string;
  ArtworkPath: string;
  ArtworkRelPath: string;
  FilePart: string;
  QPos: Integer;

  function NormalizeUrlDirectory(const APath: string): string;
  begin

    Result := StringReplace(Trim(APath),
                            '\',
                            '/',
                            [rfReplaceAll]);

    while (Result <> '') and
          (Result[Length(Result)] = '/') do
      Delete(Result,
             Length(Result),
             1);

    if (Result <> '') then
      Result := Result + '/';
  end;

  function LastUrlDirectoryName(const APath: string): string;
  var
    PathValue: string;
    DelimiterPos: Integer;

  begin

    PathValue := NormalizeUrlDirectory(APath);
    if PathValue = '' then
      Exit('');

    Delete(PathValue,
           Length(PathValue),
           1);

    DelimiterPos := LastDelimiter('/',
                                  PathValue);
    if DelimiterPos > 0 then
      Result := Copy(PathValue,
                     DelimiterPos + 1,
                     MaxInt)
    else
      Result := PathValue;
  end;

begin

  BaseUrl := Trim(FCoverJpg);

  if (BaseUrl = '') then
    begin
      if APreferCurrent and
         FileExists(IncludeTrailingPathDelimiter(Setup.CaddyArtworkPath) + COVER_IMAGE_FILE_NAME) then
        BaseUrl := COVER_IMAGE_FILE_NAME
      else
        BaseUrl := COVER_DEFAULT_IMAGE_FILE_NAME;
    end;

  // Keep the URL browser-safe. Never write local or UNC file paths to JSON.
  BaseUrl := StringReplace(BaseUrl,
                           '\',
                            '/',
                            [rfReplaceAll]);

  CaddyPath := NormalizeUrlDirectory(Setup.CaddyDir);
  ArtworkPath := NormalizeUrlDirectory(Setup.CaddyArtworkPath);

  ArtworkRelPath := '';
  if (CaddyPath <> '') and
     (ArtworkPath <> '') and
     SameText(Copy(ArtworkPath,
                   1,
                   Length(CaddyPath)),
              CaddyPath) then
    ArtworkRelPath := Copy(ArtworkPath,
                           Length(CaddyPath) + 1,
                           MaxInt)
  else if ArtworkPath <> '' then
    ArtworkRelPath := LastUrlDirectoryName(ArtworkPath);

  ArtworkRelPath := StringReplace(ArtworkRelPath,
                                  '\',
                                  '/',
                                  [rfReplaceAll]);
  ArtworkRelPath := Trim(ArtworkRelPath);

  if (ArtworkRelPath <> '') and
     (ArtworkRelPath[Length(ArtworkRelPath)] <> '/') then
    ArtworkRelPath := ArtworkRelPath + '/';

  FilePart := ExtractFileName(BaseUrl);

  if SameText(FilePart, COVER_IMAGE_FILE_NAME) or
     (Pos(COVER_IMAGE_FILE_NAME + '?', LowerCase(BaseUrl)) > 0) then
    BaseUrl := ArtworkRelPath + COVER_IMAGE_FILE_NAME
  else
    if SameText(FilePart, COVER_DEFAULT_IMAGE_FILE_NAME) or
       (Pos(COVER_DEFAULT_IMAGE_FILE_NAME + '?', LowerCase(BaseUrl)) > 0) then
      BaseUrl := ArtworkRelPath + COVER_DEFAULT_IMAGE_FILE_NAME
    else
      begin
        QPos := Pos('?', BaseUrl);
        if (QPos > 0) then
          BaseUrl := Copy(BaseUrl,
                          1,
                          QPos - 1);
      end;

  Result := BaseUrl {+ '?ts=' + IntToStr(GetTickCount)};  // Do not add tickcount, that will be done in TRDJRadioStatusJson.WriteRadioStatusJson
end;


procedure TMainMDIFrm.PublishSelectedCover();
var
  SourceCover: TFileName;
  ActiveCover: TFileName;
  PublicCover: TFileName;

  procedure CopyCoverFile(const ASource,
                                ADestination: TFileName);
  begin

    if SameText(ExpandFileName(ASource),
                ExpandFileName(ADestination)) then
      Exit;

    if not CopyFile(PChar(ASource),
                    PChar(ADestination),
                    False) then
      RaiseLastOSError;
  end;

begin

  SourceCover := Trim(FCoverFileName);
  if (SourceCover = '') or
     not FileExists(SourceCover) then
    Exit;

  if not DirectoryExists(Setup.CaddyArtworkPath) then
    Exit;

  ActiveCover := IncludeTrailingPathDelimiter(Setup.CaddyArtworkPath) + COVER_IMAGE_FILE_NAME;
  CopyCoverFile(SourceCover,
                ActiveCover);

  // Keep the root cover in sync for older deployed browser pages that still
  // normalize every cover.jpg URL to /cover.jpg.
  if DirectoryExists(Setup.CaddyDir) then
    begin

      PublicCover := IncludeTrailingPathDelimiter(Setup.CaddyDir) + COVER_IMAGE_FILE_NAME;
      if not SameText(ExpandFileName(ActiveCover),
                      ExpandFileName(PublicCover)) then
        CopyCoverFile(SourceCover,
                      PublicCover);
    end;

  FCoverFileName := ActiveCover;
  FCoverJpg := COVER_IMAGE_FILE_URL;
end;


procedure TMainMDIFrm.WriteNowPlayingStatus(const ADjName,
                                            AShowName,
                                            AArtist,
                                            ATitle,
                                            ACoverUrl: string;
                                            const AListeners: Integer = -1;
                                            const AClearEmptyTrackInfo: Boolean = False);
var
  JsonFile: string;
  ListenerCount: Integer;

begin

  if FAppClosing then
    Exit;

  JsonFile := Trim(Setup.CaddyNowPlayingJsonFile);
  if (JsonFile = '') then
    Exit;

  ListenerCount := AListeners;
  if (ListenerCount < 0) then
    ListenerCount := FCaddyListenerCount;

  // Local mode: JsonFile is normally C:\Caddy\nowplaying.json.
  // Server mode: JsonFile may be a UNC path such as \\Server\Caddy\nowplaying.json.
  // The JSON coverUrl must always be a browser URL, never a disk/UNC path.
  FRDJRadioStatusJson.WriteRadioStatusJson(JsonFile,
                                           ADjName,
                                           AShowName,
                                           AArtist,
                                           ATitle,
                                           ACoverUrl,
                                           ListenerCount,
                                           -1,
                                           '',
                                           AClearEmptyTrackInfo);
end;


function TMainMDIFrm.NormalizeNowPlayingMemoText(AMemo: TMemo): string;
begin

  Result := '';
  if not Assigned(AMemo) then
    Exit;

  Result := Trim(AMemo.Text);
  if SameText(Result,
              'none') then
    Result := '';
end;


procedure TMainMDIFrm.ApplyLoopbackNowPlayingFallback(var AArtist, ATitle: string);
begin

  AArtist := Trim(AArtist);
  ATitle := Trim(ATitle);

  if (AArtist <> '') or
     (ATitle <> '') then
    Exit;

  if not HasActiveLoopbackDeck() then
    Exit;

  AArtist := NormalizeNowPlayingMemoText(mmoEventTitle);
  ATitle := NormalizeNowPlayingMemoText(mmoActivityTitle);
end;


procedure TMainMDIFrm.ClearNowPlaying();
var
  Artist: string;
  Title: string;

begin

  if not Assigned(FRDJRadioStatusJson) then
    Exit;

  FDjName := Trim(mmoDjName.Text);
  FShowName := Trim(mmoShow.Text);
  Artist := '';
  Title := '';
  ApplyLoopbackNowPlayingFallback(Artist,
                                  Title);

  FNowPlayingArtist := Artist;
  FNowPlayingTitle := Title;
  FNowPlayingFromChannelDeck := False;

  WriteNowPlayingStatus(FDjName,
                        FShowName,
                        FNowPlayingArtist,
                        FNowPlayingTitle,
                        BuildCoverJsonUrl(True),
                        FCaddyListenerCount,
                        (FNowPlayingArtist = '') and
                        (FNowPlayingTitle = ''));
end;


procedure TMainMDIFrm.UpdateNowPlaying(const AArtist,
                                       ATitle: string);
var
  Artist: string;
  Title: string;

begin

  if not Assigned(FRDJRadioStatusJson) then
    Exit;

  Artist := Trim(AArtist);
  Title := Trim(ATitle);
  ApplyLoopbackNowPlayingFallback(Artist,
                                  Title);

  if (Artist = '') and
     (Title = '') then
    Exit;

  FDjName := Trim(mmoDjName.Text);
  FShowName := Trim(mmoShow.Text);
  FNowPlayingArtist := Artist;
  FNowPlayingTitle := Title;
  FNowPlayingFromChannelDeck := (Trim(AArtist) <> '') or
                                (Trim(ATitle) <> '');

  WriteNowPlayingStatus(FDjName,
                        FShowName,
                        FNowPlayingArtist,
                        FNowPlayingTitle,
                        BuildCoverJsonUrl(True));
end;

function TMainMDIFrm.CanGoOnAir(): Boolean;
var
  Json: TJSONObject;
  CurrentDj: string;

begin

  Result := True;

  try
  Json := FRDJRadioStatusJson.LoadNowPlayingJson(FSetup.CaddyNowPlayingJsonFile);

  try

    if Assigned(Json) then
      begin

        CurrentDj := '';

        if (Json.GetValue('onAirLock') <> nil) then
          CurrentDj := Json.GetValue('onAirLock').Value;

        if (CurrentDj <> '') and
           (not SameText(CurrentDj,
                         Trim(mmoDjName.Text))) then
          begin

            Result := False;
            ShowMessage('On Air is currently locked by ' + CurrentDj);
          end;
      end;
  finally

    Json.Free;
  end;

  except

  end;
end;
// Json end ====================================================================


procedure TMainMDIFrm.chkMediaServerClick(Sender: TObject);
begin

  if chkMediaServer.Checked then
    OpenMediaServerGUI()
  else
    begin
      if Assigned(FMediaServer) then
        FMediaServer.WindowState := wsMinimized;

      RefreshMainButtonStates();
    end;
end;


procedure TMainMDIFrm.EnsureCueWorkBuf(const AFrames: Integer);
var
  NeedSamples: Integer;

begin

  NeedSamples := Max(0,
                     AFrames * 2);
  if (Length(FCueWorkBuf) < NeedSamples) then
    SetLength(FCueWorkBuf,
              NeedSamples);
end;


// Here we change the default cover.jpg to the Dj's own cover.jpg
procedure TMainMDIFrm.imgDjShowLogoDblClick(Sender: TObject);
var
  FileName: TFileName;

begin

  if not Assigned(DlgLWFileBrowserEx)  then
    DlgLWFileBrowserEx := TLWFileBrowserExDlg.Create(Self);

  DlgLWFileBrowserEx.FileFilter := fbxGraphics;
  DlgLWFileBrowserEx.ShowModal;

  if (DlgLWFileBrowserEx.ModalResult = mrOk) then
    begin

      FileName := DlgLWFileBrowserEx.FileURI;

      if not FileExists(FileName) then
        Exit;

      imgDjShowLogo.Picture.LoadFromFile(FileName);
      FCoverFileName := FileName;
      SaveBroadcastIdentityToIni();
    end;
end;


procedure TMainMDIFrm.InitCueQueue(const AFramesCapacity: Integer);
var
  SafeFrames: Integer;

begin

  SafeFrames := Max(0,
                    AFramesCapacity);

  EnterCriticalSection(FCueBufferLock);

  try

    FCueQueueFramesCapacity := SafeFrames;
    SetLength(FCueMixQueue,
              FCueQueueFramesCapacity * 2);
    FCueQueueReadFrame := 0;
    FCueQueueWriteFrame := 0;
    FCueQueueValidFrames := 0;

    if Length(FCueMixQueue) > 0 then
      FillChar(FCueMixQueue[0],
               Length(FCueMixQueue) * SizeOf(Single),
               0);
  finally

    LeaveCriticalSection(FCueBufferLock);
  end;
end;


procedure TMainMDIFrm.PushCueFrames(const Src: PSingle; const Frames: Integer);
var
  i: Integer;
  WriteFrame: Integer;
  SrcIndex: Integer;
  DstIndex: Integer;

begin

  if (Src = nil) or
     (Frames <= 0) or
     (FCueQueueFramesCapacity <= 0) then
    Exit;

  EnterCriticalSection(FCueBufferLock);

  try

    WriteFrame := FCueQueueWriteFrame;

    for i := 0 to Frames - 1 do
      begin

        if (FCueQueueValidFrames >= FCueQueueFramesCapacity) then
          begin

            Inc(FCueQueueReadFrame);
            if FCueQueueReadFrame >= FCueQueueFramesCapacity then
              FCueQueueReadFrame := 0;
            Dec(FCueQueueValidFrames);
          end;

        SrcIndex := i * 2;
        DstIndex := WriteFrame * 2;

        FCueMixQueue[DstIndex] := PSingle(NativeUInt(Src) + NativeUInt(SrcIndex * SizeOf(Single)))^;
        FCueMixQueue[DstIndex + 1] := PSingle(NativeUInt(Src) + NativeUInt((SrcIndex + 1) * SizeOf(Single)))^;

        Inc(WriteFrame);
        if WriteFrame >= FCueQueueFramesCapacity then
          WriteFrame := 0;

        Inc(FCueQueueValidFrames);
      end;

    FCueQueueWriteFrame := WriteFrame;
  finally
    LeaveCriticalSection(FCueBufferLock);
  end;
end;


function TMainMDIFrm.PopCueFrames(const Dst: PSingle; const Frames: Integer): Integer;
var
  i: Integer;
  ReadFrame: Integer;
  SrcIndex: Integer;
  DstIndex: Integer;

begin

  Result := 0;

  if (Dst = nil) or (Frames <= 0) or (FCueQueueFramesCapacity <= 0) then
    Exit;

  EnterCriticalSection(FCueBufferLock);

  try
    ReadFrame := FCueQueueReadFrame;

    Result := Min(Frames, FCueQueueValidFrames);
    for i := 0 to Result - 1 do
      begin

        SrcIndex := ReadFrame * 2;
        DstIndex := i * 2;

        PSingle(NativeUInt(Dst) + NativeUInt(DstIndex * SizeOf(Single)))^ := FCueMixQueue[SrcIndex];
        PSingle(NativeUInt(Dst) + NativeUInt((DstIndex + 1) * SizeOf(Single)))^ := FCueMixQueue[SrcIndex + 1];

        Inc(ReadFrame);
        if ReadFrame >= FCueQueueFramesCapacity then
          ReadFrame := 0;
      end;

    FCueQueueReadFrame := ReadFrame;
    Dec(FCueQueueValidFrames, Result);
  finally

    LeaveCriticalSection(FCueBufferLock);
  end;
end;


function TMainMDIFrm.BuildRenderWaveFormat(out AWfx: TWAVEFORMATEX): HRESULT;
begin

  ZeroMemory(@AWfx,
             SizeOf(AWfx));

  AWfx.wFormatTag := WAVE_FORMAT_IEEE_FLOAT;
  AWfx.nChannels := 2;
  AWfx.nSamplesPerSec := 44100;
  AWfx.wBitsPerSample := 32;
  AWfx.nBlockAlign := (AWfx.nChannels * AWfx.wBitsPerSample) div 8;
  AWfx.nAvgBytesPerSec := AWfx.nSamplesPerSec * AWfx.nBlockAlign;
  AWfx.cbSize := 0;
  Result := S_OK;
end;


function TMainMDIFrm.EndpointIdExists(const ADeviceId: string): Boolean;
var
  DevEnum: IMMDeviceEnumerator;
  Dev: IMMDevice;

begin

  Result := False;

  if (Trim(ADeviceId) = '') then
    Exit;

  DevEnum := nil;
  Dev := nil;

  try

    DevEnum := CreateComObject(CLSID_MMDeviceEnumerator) as IMMDeviceEnumerator;

    Result := SUCCEEDED(DevEnum.GetDevice(PWideChar(WideString(ADeviceId)),
                                          Dev)) and Assigned(Dev);
  except

    Result := False;
  end;
end;


function TMainMDIFrm.ResetInvalidEndpointAssignments(): Boolean;
begin

  Result := False;

  // Master can safely fall back to default endpoint.
  if (Trim(FSetup.MasterDeviceId) <> '') and
     (not EndpointIdExists(FSetup.MasterDeviceId)) then
    begin

      FSetup.MasterDeviceId := '';
      Result := True;
    end;

  // Microphone input can safely fall back to default capture endpoint.
  if (Trim(FSetup.MicDeviceId) <> '') and
     (not EndpointIdExists(FSetup.MicDeviceId)) then
    begin

      FSetup.MicDeviceId := '';
      Result := True;
    end;

  // IMPORTANT:
  //  Do NOT clear PFLDeviceId.
  //  Do NOT force PFLEnabled := False.
  //  Temporary cue-device absence is runtime state, not setup mutation.

  if Result then
    SaveSetupToIni(FSetupFileName,
                   FSetup);
end;


procedure TMainMDIFrm.ClearAudioEndpointSetup();
begin

  FSetup.MasterDeviceId := '';
  FSetup.MicDeviceId := '';

  // Keep cue settings intact.
  SaveSetupToIni(FSetupFileName,
                 FSetup);
end;


procedure TMainMDIFrm.ConfigureOutputEndpoints();
var
  MasterDeviceId: string;
  PFLDeviceId: string;

begin

  RefreshEndpointAvailability();

  MasterDeviceId := Trim(FSetup.MasterDeviceId);
  PFLDeviceId := Trim(FSetup.PFLDeviceId);

  if FMasterEndpointAvailable and (MasterDeviceId <> '') then
    FMasterOut.SetOutputDeviceId(MasterDeviceId)
  else
    FMasterOut.SetUseDefaultOutputDevice(eMultimedia);

  FreeAndNil(FCueOut);

  if FPFLEndpointAvailable then
    begin

      FCueOut := TMfWasApiRenderOutputEngine.Create();
      FCueOut.OnFillPcm := CueOutFillPcm;
      AttachAudioOutputHandlers(FCueOut);
      FCueOut.SetOutputDeviceId(PFLDeviceId);
      FCueUnavailableWarned := False;
    end
  else
    begin

      if FSetup.PFLEnabled and (PFLDeviceId <> '') and (not FCueUnavailableWarned) then
        begin

          FCueUnavailableWarned := True;
          MessageDlg('The stored cue/PFL audio endpoint is currently unavailable.' + sLineBreak +
                     'Cue will stay disabled for this run until the endpoint becomes available again.',
                     mtWarning,
                     [mbOK],
                     0);
        end;
    end;
end;


procedure TMainMDIFrm.ApplySetupOnce();
begin

  // Freeze setup globally so decks can read it and it cannot change at runtime.
  SetGlobalSetupOnce(FSetup);
  LockGlobalSetup();

  // Disable setup menu after initialization (live safety).
  //mnuSetup.Enabled := False;

  DestroyDeckForms();

  CreateMasterForm();

  if FSetup.MicDeckEnabled then
    CreateMicrophoneForm();

  CreateChannelForms(FSetup.ChannelCount);
  CreateLoopbackDecks(FSetup.LoopbackDeckCount);

  if (FfrmMasterFxRack = nil) then
    FfrmMasterFxRack := TfrmMasterFxRack.Create(Self);

  FfrmMasterFxRack.BindMasterFx(FMasterFXRack,
                                FMasterLowMidHighEq,
                                FMasterFlangerEcho,
                                FMasterCompLim);

  FfrmMasterFxRack.LoadGuiFromEffects();
  FfrmMasterFxRack.Show;

  TileDecks();

  FSetupApplied := True;
end;


procedure TMainMDIFrm.btnMaxNormalClick(Sender: TObject);
begin

  if (WindowState = wsMaximized) then
    WindowState := wsNormal
  else
    if (WindowState = wsNormal) then
      WindowState := wsMaximized;
  Invalidate;
end;


procedure TMainMDIFrm.btnMinimizeClick(Sender: TObject);
begin

  WindowState := wsMinimized;
end;


procedure TMainMDIFrm.btnPlaylistClick(Sender: TObject);
begin

  if (FPlayListEditor = nil) then
    FPlayListEditor := TfrmPlaylistEditor.Create(Self);

  FPlayListEditor.Show;
  FPlayListEditor.Visible := True;
  FPlayListEditor.WindowState := wsNormal;
  FPlayListEditor.BringToFront;
end;


procedure TMainMDIFrm.btnSetDjNameAndShowTitleClick(Sender: TObject);
begin

  if not Assigned (FRDJRadioStatusJson) then
    Exit;

  PublishSelectedCover();

  FDjName := Trim(mmoDjName.Text);
  FShowName := Trim(mmoShow.Text);

  if HasActiveLoopbackDeck() and
     not FNowPlayingFromChannelDeck then
    begin

      FNowPlayingArtist := '';
      FNowPlayingTitle := '';
    end;

  ApplyLoopbackNowPlayingFallback(FNowPlayingArtist,
                                  FNowPlayingTitle);

  WriteNowPlayingStatus(FDjName,
                        FShowName,
                        FNowPlayingArtist,
                        FNowPlayingTitle,
                        BuildCoverJsonUrl(True),
                        FCaddyListenerCount,
                        (FNowPlayingArtist = '') and
                        (FNowPlayingTitle = ''));

  SaveBroadcastIdentityToIni();
end;


procedure TMainMDIFrm.btnSetupClick(Sender: TObject);
begin

  RefreshMainButtonStates();
  if not btnSetup.Enabled then
    Exit;

  OpenSetupGUI();
  btnSetup.Checked := False;
end;


procedure TMainMDIFrm.btnEffectsClick(Sender: TObject);
begin

  if not Assigned(FfrmMasterFxRack) or
     not Assigned(FMasterDeck) then
    Exit;

  if (FfrmMasterFxRack.WindowState = wsMinimized) then
    FfrmMasterFxRack.WindowState := wsNormal
  else
    FfrmMasterFxRack.WindowState := wsMinimized;

  AlignMasterDeckWithFxRack();
end;


procedure TMainMDIFrm.AlignMasterDeckWithFxRack();
begin

  if FAppClosing or
     not Assigned(FfrmMasterFxRack) or
     not Assigned(FMasterDeck) then
    Exit;

  if (FfrmMasterFxRack.WindowState = wsMinimized) then
    FMasterDeck.Left := FfrmMasterFxRack.Left
  else
    FMasterDeck.Left := FfrmMasterFxRack.Left + FfrmMasterFxRack.Width;
end;


procedure TMainMDIFrm.btnExitClick(Sender: TObject);
begin

  Close();
end;


procedure TMainMDIFrm.btnFooterPnlClick(Sender: TObject);
begin

  pnlFooter.Visible := btnFooterPnl.Checked;
end;


procedure TMainMDIFrm.RecreateAudioOutputs();
begin

  FreeAndNil(FCueOut);
  FreeAndNil(FMasterOut);

  FMasterOut := TMfWasApiRenderOutputEngine.Create();
  FMasterOut.OnFillPcm := MasterOutFillPcm;
  AttachAudioOutputHandlers(FMasterOut);
end;


procedure TMainMDIFrm.ResetAudioRuntimeState();
begin

  SetLength(FCueWorkBuf, 0);

  if Assigned(FInternalMixer) then
    FInternalMixer.PrepareWorkBuffers(0);
end;


procedure TMainMDIFrm.HardRestartAudioGraph();
var
  hr: HRESULT;
  wfx: TWAVEFORMATEX;
  Frames: Integer;
begin

  try

    StopInternalMixerRecording();

    if Assigned(FCueOut) then
      FCueOut.Stop();

    if Assigned(FMasterOut) then
      FMasterOut.Stop();

    ResetAudioRuntimeState();
    RecreateAudioOutputs();

    hr := BuildRenderWaveFormat(wfx);
    if FAILED(hr) then
      raise Exception.CreateFmt('BuildRenderWaveFormat failed: $%.8x',
                                [Cardinal(hr)]);

    RefreshEndpointAvailability();
    ConfigureOutputEndpoints();

    hr := FMasterOut.Prepare(@wfx);
    if FAILED(hr) then
      raise Exception.CreateFmt('FMasterOut.Prepare failed: $%.8x',
                                [Cardinal(hr)]);

    if Assigned(FCueOut) then
      begin
        hr := FCueOut.Prepare(@wfx);
        if FAILED(hr) then
          begin
            FreeAndNil(FCueOut);
            // keep stored cue config intact.
          end;
      end;

    Frames := Integer(FMasterOut.BufferFrameCount);
    if Assigned(FCueOut) and (Integer(FCueOut.BufferFrameCount) > Frames) then
      Frames := Integer(FCueOut.BufferFrameCount);

    if (Frames <= 0) then
      Frames := 1024;

    InitCueQueue(Frames);
    EnsureCueWorkBuf(Frames);
    FInternalMixer.PrepareWorkBuffers(Frames);

    hr := FMasterOut.Start();
    if FAILED(hr) then
      raise Exception.CreateFmt('FMasterOut.Start failed: $%.8x',
                                [Cardinal(hr)]);

    if Assigned(FCueOut) then
      begin

        hr := FCueOut.Start();
        if FAILED(hr) then
          raise Exception.CreateFmt('FCueOut.Start failed: $%.8x',
                                    [Cardinal(hr)]);
      end;

    if Assigned(FMasterDeck) then
      FMasterDeck.ApplyCurrentSetup();


  except

    on E: Exception do
      begin

        raise;
      end;
  end;
end;


procedure TMainMDIFrm.DestroyDeckForms();
var
  i: Integer;

begin

  // IMPORTANT:  Graph rebuild instructions
  // 1. Stop MASTER render engine
  // 2. Stop CUE render engine
  // 3. Nil their OnFillPcm callbacks
  // 4. Detach/remove deck engines from the mixer
  // 5. Detach deck engine events from deck forms
  // 6. Free deck forms / deck engines
  // 7. Recreate channels/decks
  // 8. Reattach mixer callbacks
  // 9. Restart MASTER/CUE render engines

  // Microphone deck
  if (FMicrophoneDeck <> nil) then
    FreeAndNil(FMicrophoneDeck);

  // Channel decks
  for i := 0 to Length(FChannelDecks) - 1 do
    begin
      if (FChannelDecks[i] <> nil) then
        FreeAndNil(FChannelDecks[i]);
    end;

  SetLength(FChannelDecks,
            0);

   // Loopback decks
  for i := 0 to Length(FLoopbackDecks) - 1 do
    begin
      if (FLoopbackDecks[i] <> nil) then
        FreeAndNil(FLoopbackDecks[i]);
    end;

  SetLength(FLoopbackDecks,
            0);

  // Master deck
  if (FMasterDeck <> nil) then
    FreeAndNil(FMasterDeck);

  // Master Rack.
  if (FfrmMasterFxRack <> nil) then
    FreeAndNil(FfrmMasterFxRack);
end;


procedure TMainMDIFrm.TileDecks();
var
  i: Integer;
  x: Integer;
  y: Integer;
  deckW: Integer;

begin

  if (FMasterDeck = nil) then
    Exit;

  FMasterDeck.Align := alNone;

  if Assigned(FfrmMasterFxRack) then
    FfrmMasterFxRack.Align := alNone;

  if Assigned(FMicrophoneDeck) then
    FMicrophoneDeck.Align := alNone;

  for i := 0 to Length(FLoopbackDecks) - 1 do
    begin
      if Assigned(FLoopbackDecks[i]) then
        FLoopbackDecks[i].Align := alNone;
    end;

  for i := 0 to Length(FChannelDecks) - 1 do
    begin

      if Assigned(FChannelDecks[i]) then
        FChannelDecks[i].Align := alNone;
    end;

  x := 0;
  y := 0;

  // Microphone deck
  if Assigned(FMicrophoneDeck) then
    begin

      deckW := FMicrophoneDeck.Width;

      FMicrophoneDeck.Top := y;
      FMicrophoneDeck.Left := x;

      Inc(x, deckW);
    end;

  // Normal channel decks
  for i := 0 to Length(FChannelDecks) - 1 do
    begin

      if (FChannelDecks[i] = nil) then
        Continue;

      deckW := FChannelDecks[i].Width;

      FChannelDecks[i].Top := y;
      FChannelDecks[i].Left := x;

      Inc(x, deckW);
    end;

  // Loopback decks
  for i := 0 to Length(FLoopbackDecks) - 1 do
    begin

      if (FLoopbackDecks[i] = nil) then
        Continue;

      deckW := FLoopbackDecks[i].Width;

      FLoopbackDecks[i].Top := y;
      FLoopbackDecks[i].Left := x;

      Inc(x, deckW);
    end;

  // FX rack
  if Assigned(FfrmMasterFxRack) then
    begin

      deckW := FfrmMasterFxRack.Width;

      FfrmMasterFxRack.Top := y;
      FfrmMasterFxRack.Left := x;

      Inc(x, deckW);
    end;

  // Master deck
  //deckW := FMasterDeck.Width;
  FMasterDeck.Top := y;
  FMasterDeck.Left := x;

  //Inc(x, deckW);

  ApplyDarkFrameToAllChildren();
  Invalidate;
end;


procedure TMainMDIFrm.ApplyDarkFrameToAllChildren();
var
  I: Integer;

begin

  //ApplyDarkWindowFrame(Handle);

  for I := 0 to MDIChildCount - 1 do
    if Assigned(MDIChildren[I]) then
      ApplyDarkWindowFrame(MDIChildren[I].Handle);
end;


procedure TMainMDIFrm.tmrClockTimer(Sender: TObject);
var
  NowTick: UInt64;

begin

  lblLocalTime.Caption := TimeToStr(Time);
  ServiceAudioGraphRecovery();

  NowTick := GetTickCount64();
  if (FLastCaddyListenerRefreshTick = 0) or
     (NowTick - FLastCaddyListenerRefreshTick >= RDJ_CADDY_LISTENER_REFRESH_MS) then
    begin
      FLastCaddyListenerRefreshTick := NowTick;
      RefreshCaddyListenerCount(True);
    end;
end;


procedure TMainMDIFrm.CreateMasterForm();
begin

  if (FMasterDeck <> nil) then
    Exit;

  FMasterDeck := TMasterDeckFrm.Create(Self);
  FMasterDeck.Show;
end;


procedure TMainMDIFrm.CreateMicrophoneForm();
begin

  if (FMicrophoneDeck <> nil) then
    Exit;

  FMicrophoneDeck := TfrmMicrophoneDeck.Create(Self);
  FMicrophoneDeck.Show;
end;


procedure TMainMDIFrm.CreateChannelForms(Count: Integer);
var
  i: Integer;

begin

  if (Count < 1) then
    Count := 1;
  if (Count > MAX_CHANNELS) then
    Count := MAX_CHANNELS;

  SetLength(FChannelDecks,
            Count);

  for i := 0 to Count - 1 do
    begin

      FChannelDecks[i] := TfrmChannelDeck.Create(Self);
      FChannelDecks[i].ChannelIndex := i;
      FChannelDecks[i].lblCaption.Caption := Format('Channel %d',
                                                    [i + 1]);

      FChannelDecks[i].Show;
    end;
end;


function TMainMDIFrm.HasActiveLoopbackDeck(): Boolean;
var
  I: Integer;

begin

  Result := False;

  for I := 0 to High(FLoopbackDecks) do
    if Assigned(FLoopbackDecks[I]) and
       FLoopbackDecks[I].IsCapturing() then
      Exit(True);
end;


procedure TMainMDIFrm.CreateLoopbackDecks(Count: Integer);
var
  i: Integer;

begin

  if (Count < 0) then
    Count := 0;

  if (Count > MAX_LOOPBACK_DECKS) then
    Count := MAX_LOOPBACK_DECKS;

  SetLength(FLoopbackDecks,
            Count);

  for i := 0 to Count - 1 do
    begin

      FLoopbackDecks[i] := TfrmLoopbackDeck.Create(Self);
      FLoopbackDecks[i].lblCaption.Caption := Format('Loopback %d',
                                                     [i + 1]);
      FLoopbackDecks[i].Show;
    end;
end;


function TMainMDIFrm.GetRecorderTapPoint(): string;
begin

  if (FSetup.AudioRecorderRecordPreFx) then
    Result := Format('Mixer audio Pre-FX, ID = %d',
                     [0])
  else
    if (FSetup.AudioRecorderRecordPostFx) then
      Result := Format('Mixer audio Post-FX, ID = %d',
                       [1])
    else
      if (FSetup.AudioRecorderRecordPostFx) and (FSetup.AudioRecorderRecordPreFx) then
        Result := Format('Mixer audio Pre-FX (id = ) and Post-FX (ID = %d)',
                         [0, 1])
end;


function TMainMDIFrm.IsRecordingPreFx(): Boolean;
begin

  Result := Assigned(FInternalMixRecorderPreFx) and
            FInternalMixRecorderPreFx.Recording;
end;


function TMainMDIFrm.IsRecordingPostFx(): Boolean;
begin

  Result := Assigned(FInternalMixRecorderPostFx) and
            FInternalMixRecorderPostFx.Recording;
end;


function TMainMDIFrm.BuildDualRecordFileName(const ABaseFileName: TFileName;
                                             const ASuffix: string): TFileName;
var
  DirName: string;
  BaseNoExt: string;
  Ext: string;

begin

  DirName := ExtractFilePath(ABaseFileName);
  BaseNoExt := ChangeFileExt(ExtractFileName(ABaseFileName), '');
  Ext := ExtractFileExt(ABaseFileName);
  Result := DirName + BaseNoExt + ASuffix + Ext;
end;


// Call backs
function TMainMDIFrm.MasterOutFillPcm(Sender: TObject;
                                      pData: PByte;
                                      const ByteCount: DWORD;
                                      pwfx: PWAVEFORMATEX;
                                      out Flags: DWORD): HRESULT;
var
  Frames: Integer;
  OutBuffer: PSingle;

begin

  if not Assigned(FInternalMixer) then
    begin

      FillChar(pData^,
               ByteCount,
               0);

      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  Flags := 0;

  if (pData = nil) or
     (ByteCount = 0) or
     (pwfx = nil) then
    Exit(E_INVALIDARG);

  if (pwfx.nBlockAlign = 0) then
    Exit(E_FAIL);

  Frames := Integer(ByteCount div DWORD(pwfx.nBlockAlign));
  if (Frames <= 0) then
    Exit(E_FAIL);

  OutBuffer := PSingle(Pointer(pData));

  EnsureCueWorkBuf(Frames);

  if Length(FCueWorkBuf) < (Frames * 2) then
    Exit(E_OUTOFMEMORY);

  Result := FInternalMixer.MixBlock(Frames,
                                    OutBuffer,
                                    @FCueWorkBuf[0]);

  if FAILED(Result) then
    begin
      FillChar(pData^,
               ByteCount,
               0);
      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit;
    end;

  // PRE-FX tap for recorder.
  if IsRecordingPreFx() then
    FInternalMixRecorderPreFx.PushFloat32(OutBuffer,
                                          Frames);

  // PRE-FX tap for RDJ Pro MP4/video recording.
  if Assigned(FMediaServer) then
    FMediaServer.RecordTapPreFx(OutBuffer,
                                Frames,
                                pwfx);

  // Apply master FX.
  ProcessMasterFx(OutBuffer,
                  Frames,
                  pwfx.nSamplesPerSec);

  // POST-FX tap for recorder.
  if IsRecordingPostFx() then
    FInternalMixRecorderPostFx.PushFloat32(OutBuffer,
                                           Frames);

  if Assigned(FCueOut) then
    PushCueFrames(@FCueWorkBuf[0],
                  Frames);

  Result := S_OK;
end;


procedure TMainMDIFrm.btnClearMemoClick(Sender: TObject);
begin

  mmoTextNotes.Clear;
end;


function TMainMDIFrm.CueOutFillPcm(Sender: TObject;
                                   pData: PByte;
                                   const ByteCount: DWORD;
                                   pwfx: PWAVEFORMATEX;
                                   out Flags: DWORD): HRESULT;
var
  Frames: Integer;
  FramesPopped: Integer;

begin

  Flags := 0;

  if (pData = nil) or (ByteCount = 0) or (pwfx = nil) then
    Exit(E_INVALIDARG);

  if (pwfx.nBlockAlign = 0) then
    Exit(E_FAIL);

  Frames := Integer(ByteCount div DWORD(pwfx.nBlockAlign));
  if (Frames <= 0) then
    Exit(E_FAIL);

  FillChar(pData^,
           ByteCount,
           0);

  if not Assigned(FCueOut) then
    begin

      Flags := AUDCLNT_BUFFERFLAGS_SILENT;
      Exit(S_OK);
    end;

  FramesPopped := PopCueFrames(PSingle(Pointer(pData)),
                               Frames);
  if (FramesPopped = 0) then
    Flags := AUDCLNT_BUFFERFLAGS_SILENT;

  Result := S_OK;
end;



function TMainMDIFrm.StartInternalMixerRecording(const ABaseFileName: TFileName;
                                                 const ARecordPreFx: Boolean;
                                                 const ARecordPostFx: Boolean): Boolean;
var
  Wfx: TWAVEFORMATEX;
  hr: HRESULT;
  RecPre: Boolean;
  RecPost: Boolean;
  Started: Boolean;
  FileNamePre: TFileName;
  FileNamePost: TFileName;

begin

  Result := False;

  if (Trim(ABaseFileName) = '') then
    Exit;

  RecPre := ARecordPreFx;
  RecPost := ARecordPostFx;

  if (not RecPre) and (not RecPost) then
    Exit;

  hr := BuildRenderWaveFormat(Wfx);
  if FAILED(hr) then
    Exit;

  StopInternalMixerRecording();

  Started := False;

  if RecPre then
    begin

      if not Assigned(FInternalMixRecorderPreFx) then
        FInternalMixRecorderPreFx := TMfInternalMixerRecorder.Create();

      FileNamePre := BuildDualRecordFileName(ABaseFileName,
                                             '_pre');

      if FInternalMixRecorderPreFx.Start(FileNamePre,
                                         Wfx) then
        Started := True;
    end;

  if RecPost then
    begin

      if not Assigned(FInternalMixRecorderPostFx) then
        FInternalMixRecorderPostFx := TMfInternalMixerRecorder.Create();

      FileNamePost := BuildDualRecordFileName(ABaseFileName,
                                              '_post');

      if FInternalMixRecorderPostFx.Start(FileNamePost,
                                          Wfx) then
        Started := True;
    end;

  if not Started then
    StopInternalMixerRecording();

  Result := Started;
end;


procedure TMainMDIFrm.StopInternalMixerRecording();
begin

  if Assigned(FInternalMixRecorderPreFx) then
    FInternalMixRecorderPreFx.Stop();

  if Assigned(FInternalMixRecorderPostFx) then
    FInternalMixRecorderPostFx.Stop();
end;


function TMainMDIFrm.IsInternalMixerRecording(): Boolean;
begin

  Result := IsRecordingPreFx() or
            IsRecordingPostFx();
end;


procedure TMainMDIFrm.SetAudioRecorderRecordPreFx(const AValue: Boolean);
begin

  FSetup.AudioRecorderRecordPreFx := AValue;
  SaveSetupToIni(FSetupFileName,
                 FSetup);
end;


procedure TMainMDIFrm.SetAudioRecorderRecordPostFx(const AValue: Boolean);
begin

  FSetup.AudioRecorderRecordPostFx := AValue;
  SaveSetupToIni(FSetupFileName,
                 FSetup);
end;


function TMainMDIFrm.GetChannelDeck(const AIndex: Integer): TfrmChannelDeck;
begin

  Result := nil;

  if (AIndex < 0) or (AIndex >= Length(FChannelDecks)) then
    Exit;

  Result := FChannelDecks[AIndex];
end;


function TMainMDIFrm.GetChannelDeckCount(): Integer;
begin

  Result := Length(FChannelDecks);
end;


procedure TMainMDIFrm.SetAudioRecorderTapPoint(const AValue: Integer);
begin

  if (AValue < 0) then
    FSetup.AudioRecorderTapPoint := 0
  else
    if (AValue > 1) then
      FSetup.AudioRecorderTapPoint := 1
    else
      FSetup.AudioRecorderTapPoint := AValue;

  SaveSetupToIni(FSetupFileName,
                 FSetup);
end;



// initialization and finalization =============================================


initialization

  if FAILED(MFStartup(MF_VERSION,
                      MFSTARTUP_LITE)) then
      begin
        MessageBox(0,
                   lpcwstr('Your computer does not support this Media Foundation API version.' +
                           IntToStr(MF_VERSION) + '.'),
                   lpcwstr('MFStartup Failure!'),
                           MB_ICONSTOP);
      end;

finalization

  MFShutdown();


end.
