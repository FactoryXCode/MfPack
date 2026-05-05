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
// Revision Version: 3.2.0
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
// 05/05/2026 All                 Bauhaus release  SDK 10.0.26100.4654 (Windows 11)
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
  {System}
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  System.UITypes,
  System.Win.ComObj,
  System.JSON,
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
  {CoreAudioApi}
  WinApi.CoreAudioApi.AudioClient,
  WinApi.CoreAudioApi.MMDeviceApi,
  WinApi.CoreAudioApi.AudioSessionTypes,
  WinApi.CoreAudioApi.AudioPolicy,
  {WinMM}
  WinApi.WinMM.MMeApi,
  WinApi.WinMM.MMReg,
  {Application}
  RDJ_Common,
  frmSetup,
  RDJ.Setup,
  MfWasApiFxComponentBase,
  MfParametricEqComponent,
  MfCompressorLimiterComponent,
  MfLowMidHighEqComponent,
  MfFlangerEchoComponent,
  frmChannelDeck,
  frmMasterDeck,
  frmPlaylistEditor,
  RDJ.InternalMixer,
  MfWasApiEffectsRack,
  MfWasApiRenderOutputEngine,
  frmMasterFxRack,
  frmLoopBackDeck,
  MicrophoneDeckFrm,
  MfAudioRecorder,
  MfIcecastBroadcastEngine,
  MPxpButton,
  dlgMediaServer,
  RDJ.JSon;

const

  WM_RDJ_ENDPOINTS_CHANGED = WM_APP + 300;
  WM_RDJ_ENDPOINTS_CHANGED_COMMIT = WM_APP + 301;
  WM_RDJ_CUE_ENDPOINT_LOST = WM_APP + 302;

  // json file url
  COVER_IMAGE_FILE_URL = 'cover.jpg?ts=';
  COVER_IMAGE_FILE_NAME = 'cover.jpg';


type

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

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

    // Deviceloss
    FEndpointEnumerator: IMMDeviceEnumerator;
    FEndpointNotifyClient: IMMNotificationClient;
    FEndpointRefreshPending: Boolean;
    FAudioGraphRestarting: Boolean;
    FPendingHardRestart: Boolean;
    FCueLostDialogShowing: Boolean;
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

    // Icecast -----------------------------------------------------------------
    FBroadcastEngine: TMfIcecastBroadcastEngine;

    // json
    FRDJRadioStatusJson: TRDJRadioStatusJson;
    FDjName: string;
    FShowName: string;
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

    // Icecast -----------------------------------------------------------------
    procedure BroadcastTapPostFx(const pData: PSingle;
                                 const Frames: Integer;
                                 const pwfx: PWAVEFORMATEX);
    procedure BroadcastTapPreFx(const pData: PSingle;
                                const Frames: Integer;
                                const pwfx: PWAVEFORMATEX);

    procedure CreateBroadcastEngine();
    procedure DestroyBroadcastEngine();
    procedure StartBroadcast();
    procedure StopBroadcast();
    function ValidateBroadcastSetup(const S: TRDJBroadcastSetup;
                                    out ErrMsg: string): Boolean;

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
    procedure QueueEndpointRefresh();
    procedure WMEndpointsChanged(var Msg: TMessage); message WM_RDJ_ENDPOINTS_CHANGED;
    procedure WMEndpointsChangedCommit(var Msg: TMessage); message WM_RDJ_ENDPOINTS_CHANGED_COMMIT;
    procedure WMCueEndpointLost(var Msg: TMessage); message WM_RDJ_CUE_ENDPOINT_LOST;

    function SaveSetupSafely(): Boolean;
    function IsProbablyValidStoredEndpointId(const ADeviceId: string): Boolean;
    procedure NotifyCueEndpointLostAsync();

    function IsEndpointUsable(const ADeviceId: string): Boolean;
    function ResolveMasterEndpointAvailable(): Boolean;
    function ResolvePFLEndpointAvailable(): Boolean;
    procedure RefreshEndpointAvailability();

    procedure OpenSetupGUI();
    procedure OpenMediaServerGUI();

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
    // IceCast
    procedure BroadcastStateChanged(Sender: TObject;
                                    NewState: TMfBroadcastState;
                                    const Msg: string);

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

    procedure SetAudioRecorderTapPoint(const AValue: Integer);
    // Icecast
    procedure SetBroadcastEnabled(const AValue: Boolean);
    procedure RemoveBroadcastEngine();
    // Json
    function CanGoOnAir(): Boolean;
        property Caption;

    property Setup: TRDJSetup
      read FSetup;
    property InternalMixer: TRDJInternalMixer
      read FInternalMixer;
    property RecorderTapPoint: string
      read GetRecorderTapPoint;
    property Master: TMasterDeckFrm
      read FMasterDeck;
    property IceCastEngine: TMfIcecastBroadcastEngine
      read FBroadCastEngine;
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


const
  // Baseline target "minimum today" resolution.
  BASE_MIN_W = 1920;
  BASE_MIN_H = 1080;

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


procedure TMainMDIFrm.QueueEndpointRefresh();
begin

  if FEndpointRefreshPending then
    Exit;

  FEndpointRefreshPending := True;
  PostMessage(Handle,
              WM_RDJ_ENDPOINTS_CHANGED_COMMIT,
              0,
              0);
end;


procedure TMainMDIFrm.WMEndpointsChanged(var Msg: TMessage);
begin

  // Audio endpoint notifications can arrive in bursts, especially after
  // Windows Update, driver restart, USB plug/unplug, or Endpoint Builder reset.
  // Never rebuild the audio graph directly from the notification storm.
  if not FEndpointRefreshPending then
    begin

      FEndpointRefreshPending := True;
      PostMessage(Handle,
                  WM_RDJ_ENDPOINTS_CHANGED_COMMIT,
                  0,
                  0);
    end;
end;


procedure TMainMDIFrm.WMEndpointsChangedCommit(var Msg: TMessage);
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


procedure TMainMDIFrm.NotifyCueEndpointLostAsync();
begin

  if FCueUnavailableWarned then
    Exit;

  FCueUnavailableWarned := True;
  PostMessage(Handle,
              WM_RDJ_CUE_ENDPOINT_LOST,
              0,
              0);
end;


procedure TMainMDIFrm.WMCueEndpointLost(var Msg: TMessage);
begin

  if FCueLostDialogShowing then
    Exit;

  FCueLostDialogShowing := True;
  try

    MessageDlg('The stored cue/PFL audio endpoint is currently unavailable.' + sLineBreak +
               'Cue will stay disabled for this run until the endpoint becomes available again.' + sLineBreak +
               'Open Setup and select the headphone output again if Windows changed the device.',
               mtWarning,
               [mbOK],
               0);
  finally

    FCueLostDialogShowing := False;
  end;
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

procedure TMainMDIFrm.FormCreate(Sender: TObject);
begin

  // Do not create any childforms here!
end;


procedure TMainMDIFrm.FormDestroy(Sender: TObject);
var
  i: Integer;

begin

  tmrClock.Enabled:= False;

  // Icecast will be destroyed in OnCloseQuery.

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

  DeleteCriticalSection(FCueBufferLock);

  TeardownEndpointNotifications();
end;


// GUI shell behavior:
// - Fixed baseline window size (no user resizing)
// - Optional fullscreen toggle (F11)
// - Horizontal scrollbar to scroll a virtual strip of MDI child decks.
procedure TMainMDIFrm.ConstructEngine();
begin

  KeyPreview := True;

  FSetupFileName := GetDefaultSetupFileName();

  try

    LoadSetupFromIni(FSetupFileName,
                     FSetup);
  except

    // If Windows or a crash damaged the INI while RDJ was writing it,
    // keep a copy for inspection and continue with a clean setup record.
    if FileExists(FSetupFileName) then
      MoveFileEx(PChar(FSetupFileName),
                 PChar(FSetupFileName + '.bad.' + IntToStr(GetTickCount)),
                 MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH);

    FSetup := Default(TRDJSetup);
  end;

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

  FCueOut := nil;
  InitializeCriticalSection(FCueBufferLock);
  FCueQueueFramesCapacity := 0;
  FCueQueueReadFrame := 0;
  FCueQueueWriteFrame := 0;
  FCueQueueValidFrames := 0;
end;


procedure TMainMDIFrm.FormResize(Sender: TObject);
begin

  TileDecks();
  //lblLocalTime.Left := (ClientWidth div 2) - (lblLocalTime.Width div 2);

  if Assigned(FMasterDeck) and
     Assigned(FfrmMasterFxRack) and
     (WindowState = wsMaximized) and
     (FfrmMasterFxRack.WindowState = wsMinimized) then
    FMasterDeck.Left := FfrmMasterFxRack.Left;

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
            NotifyCueEndpointLostAsync();
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
        begin

          FreeAndNil(FCueOut);
          NotifyCueEndpointLostAsync();
        end;
    end;

  // Icecast   TODO: Maybe better to do this manually?  T: Yes because Caddy needs to startup first (see frmMasterDeck).
  //if FSetup.Broadcast.Enabled then
  //  StartBroadcast();

  FRDJRadioStatusJson := TRDJRadioStatusJson.Create;

  FDjName := mmoDjName.Text;
  FShowName := mmoShow.Text;
  FCoverFileName := IncludeTrailingPathDelimiter(Setup.IcecastCaddyDir) + 'cover_default.jpg';
  imgDjShowLogo.Picture.LoadFromFile(FCoverFileName);
end;


procedure TMainMDIFrm.FormCloseQuery(Sender: TObject;
                                     var CanClose: Boolean);
begin

  CanClose := False;

  tmrClock.Enabled := False;
  lblLocalTime.Caption := 'Closing RDJ, Please wait...';
  lblLocalTime.Repaint;
  Sleep(2000);

  DestroyBroadcastEngine();

  if Assigned(FMediaServer) then
    FMediaServer.Free;

  if Assigned(FPlayListEditor) then
    FPlayListEditor.Free;

  if Assigned(FRDJRadioStatusJson) then
    FRDJRadioStatusJson.Free;

  Sleep(200);
  CanClose := True;
end;


// Setup / deck creation ======================================================

procedure TMainMDIFrm.OpenSetupGUI();
begin

  if TfrmSetup.Execute(FSetup) then
    begin

      SaveSetupSafely();

      ApplySetupOnce();

      if FUiInitialized then
        HardRestartAudioGraph();
    end;
end;


// IceCast/Caddy
procedure TMainMDIFrm.OpenMediaServerGUI();
begin

  if not assigned(FMediaServer) then
    FMediaServer := TfrmMediaServer.Create(Self);

  FMediaServer.Show;
  FMediaServer.WindowState := wsNormal;
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


procedure TMainMDIFrm.BroadcastTapPostFx(const pData: PSingle;
                                         const Frames: Integer;
                                         const pwfx: PWAVEFORMATEX);
var
  hr: HRESULT;

begin

  if not Assigned(FBroadcastEngine) then
    Exit;

  if not (FBroadcastEngine.State in [bsConnecting,
                                     bsLive,
                                     bsReconnecting]) then
    Exit;

  if (pData = nil) or
     (pwfx = nil) or
     (Frames <= 0) then
    Exit;

  if (FSetup.Broadcast.TapPoint <> btpPostMasterFx) then
    Exit;

  hr := FBroadcastEngine.PushPcmFloat32(pData,
                                        Frames,
                                        pwfx);
  if FAILED(hr) then
    InfoMsg(optShowMsg,
            'PushPcmFloat32 failed',
            hr,
            0);
end;


procedure TMainMDIFrm.BroadcastTapPreFx(const pData: PSingle;
                                        const Frames: Integer;
                                        const pwfx: PWAVEFORMATEX);
var
  HR: HRESULT;

begin

  if not Assigned(FBroadcastEngine) then
    Exit;

  if not (FBroadcastEngine.State in [bsConnecting,
                                     bsLive,
                                     bsReconnecting]) then
    Exit;

  if (pData = nil) or
     (pwfx = nil) or
     (Frames <= 0) then
    Exit;

  if (FSetup.Broadcast.TapPoint <> btpPreMasterFx) then
    Exit;

  HR := FBroadcastEngine.PushPcmFloat32(pData,
                                        Frames,
                                        pwfx);
  if FAILED(hr) then
    InfoMsg(optShowMsg,
            'PushPcmFloat32 failed',
            hr,
            0);
end;


// Icecast ---------------------------------------------------------------------
procedure TMainMDIFrm.CreateBroadcastEngine();
begin

  if Assigned(FBroadcastEngine) then
    Exit;

  FBroadcastEngine := TMfIcecastBroadcastEngine.Create(Self);
  FBroadcastEngine.OnStateChanged := BroadcastStateChanged;
end;


procedure TMainMDIFrm.DestroyBroadcastEngine();
begin

  if Assigned(FBroadcastEngine) then
    begin

      StopBroadcast();
      FreeAndNil(FBroadcastEngine);
      // json
      FreeAndNil(FRDJRadioStatusJson);
    end;
end;


procedure TMainMDIFrm.StartBroadcast();
var
  S: TRDJBroadcastSetup;
  HR: HRESULT;
  ErrMsg: string;

begin

  if not ValidateBroadcastSetup(FSetup.Broadcast,
                                ErrMsg) then
    raise Exception.Create(ErrMsg);

  CreateBroadcastEngine();

  FillChar(S,
           SizeOf(S),
           0);

  S.Host := Trim(FSetup.Broadcast.Host);
  S.Port := FSetup.Broadcast.Port;
  S.Mount := Trim(FSetup.Broadcast.Mount);
  S.Username := FSetup.Broadcast.Username;
  S.Password := FSetup.Broadcast.Password;

  S.StreamName := FSetup.Broadcast.StreamName;
  S.Description := FSetup.Broadcast.Description;
  S.Genre := FSetup.Broadcast.Genre;
  S.Url := FSetup.Broadcast.Url;
  S.PublicStream := FSetup.Broadcast.PublicStream;

  case FSetup.Broadcast.Codec of
    bcMp3: S.Codec := bcMp3;
  else
    S.Codec := bcAac;
  end;

  S.BitrateKbps := FSetup.Broadcast.BitrateKbps;
  S.SampleRate := FSetup.Broadcast.SampleRate;
  S.Channels := FSetup.Broadcast.Channels;

  case FSetup.Broadcast.TapPoint of
    btpPreMasterFx: S.TapPoint := btpPreMasterFx;
  else
    S.TapPoint := btpPostMasterFx;
  end;

  S.AutoReconnect := FSetup.Broadcast.AutoReconnect;
  S.BroadcastGainDb := FSetup.Broadcast.BroadcastGainDb;

  FBroadcastEngine.Settings := S;

  HR := FBroadcastEngine.Start();
  if Failed(HR) then
    OleCheck(HR);
end;


procedure TMainMDIFrm.StopBroadcast();
begin

  if Assigned(FBroadcastEngine) then
    FBroadcastEngine.Stop;
end;


procedure TMainMDIFrm.SetBroadcastEnabled(const AValue: Boolean);
begin

  FSetup.Broadcast.Enabled := AValue;

  if AValue then
    begin

      if Assigned(FBroadcastEngine) and
         (FBroadcastEngine.State in [bsConnecting,
                                     bsLive,
                                     bsReconnecting]) then
        Exit;
    end
  else
    begin
      if Assigned(FBroadcastEngine) and
         (FBroadcastEngine.State = bsStopped) then
        Exit;
    end;

  // Don't save to ini!
  //SaveSetupToIni(FSetupFileName,
  //               FSetup);

  // NOTE: T: Only start broadcast in UI masterdeck.
  if AValue then
    StartBroadcast()
  else
    StopBroadcast();
end;


procedure TMainMDIFrm.RemoveBroadcastEngine();
begin

  DestroyBroadcastEngine();
end;


function TMainMDIFrm.ValidateBroadcastSetup(const S: TRDJBroadcastSetup;
                                            out ErrMsg: string): Boolean;
begin

  Result := False;
  ErrMsg := '';

  if (Trim(S.Host) = '') then
    ErrMsg := 'Broadcast host is empty.'
  else
    if (S.Port <= 0) then
      ErrMsg := 'Broadcast port is invalid.'
    else
      if (Trim(S.Mount) = '') then
        ErrMsg := 'Broadcast mount is empty.'
      else
        if (S.Mount[1] <> '/') then
          ErrMsg := 'Broadcast mount must start with "/".'
        else
          if (Trim(S.Username) = '') then
            ErrMsg := 'Broadcast username is empty.'
          else
            if (Trim(S.Password) = '') then
              ErrMsg := 'Broadcast password is empty.'
            else
              if (S.Channels <> 2) then
                ErrMsg := 'Only stereo broadcast is supported.'
              else
                Result := True;
end;

// Icecast end -----------------------------------------------------------------

// Json ------------------------------------------------------------------------

function TMainMDIFrm.CanGoOnAir(): Boolean;
var
  Json: TJSONObject;
  CurrentDj: string;

begin

  Result := True;

  try
  Json := FRDJRadioStatusJson.LoadNowPlayingJson(FSetup.IcecastNowPlayingJsonFile);

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


procedure TMainMDIFrm.chkMediaServerClick(Sender: TObject);
begin

  OpenMediaServerGUI();
end;

// Json end ====================================================================


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

{
// Here we change the default cover.jpg to the Dj's own cover.jpg
procedure TMainMDIFrm.imgDjShowLogoDblClick(Sender: TObject);
var
  filename: TFileName;
  RDJLogo: TFileName;

begin

  if BrowseAudioFile(Handle,
                     'LogoFile File'#0'*.jpg'#0#0,
                     False,
                     filename) then
    begin

      FCoverFileName := ExtractFileName(filename);
      imgDjShowLogo.Picture.LoadFromFile(FCoverFileName);

      // We replace the Logo.jpg with the new one (after we renamed the original logo).
      RDJLogo := IncludeTrailingPathDelimiter(Setup.IcecastCaddyDir) + 'cover.jpg';

      if FileExists(RDJLogo) then
        ChangeFileExt(RDJLogo, '.rdj')
      else
        RDJLogo := IncludeTrailingPathDelimiter(Setup.IcecastCaddyDir) + 'cover.rdj';

      //FCoverFileName :=

    end
  else
    begin

      FCoverFileName := '';
      imgDjShowLogo.Picture := nil;
    end;
end;}

// Here we change the default cover.jpg to the Dj's own cover.jpg
procedure TMainMDIFrm.imgDjShowLogoDblClick(Sender: TObject);
var
  FileName: TFileName;
  ActiveCover: TFileName;
  DefaultCover: TFileName;

begin

  ActiveCover := IncludeTrailingPathDelimiter(Setup.IcecastCaddyDir) + 'cover.jpg';
  DefaultCover := IncludeTrailingPathDelimiter(Setup.IcecastCaddyDir) + 'cover_default.jpg';

  if BrowseAudioFile(Handle,
                     'JPEG Image'#0'*.jpg;*.jpeg'#0#0,
                     False,
                     FileName) then
    begin

      if not FileExists(FileName) then
        Exit;

      CopyFile(PChar(FileName),
               PChar(ActiveCover),
               False);

      imgDjShowLogo.Picture.LoadFromFile(ActiveCover);

      FCoverFileName := ActiveCover; // disk path, internal only
      FCoverJpg := COVER_IMAGE_FILE_URL + IntToStr(GetTickCount); // web path for JSON

      FRDJRadioStatusJson.WriteRadioStatusJson(Setup.IcecastNowPlayingJsonFile,
                                               Trim(mmoDjName.Text),
                                               Trim(mmoShow.Text),
                                               '',
                                               '',
                                               FCoverJpg);
    end
  else
    begin

      if FileExists(DefaultCover) then
        begin

          CopyFile(PChar(DefaultCover),
                   PChar(ActiveCover),
                   False);

          imgDjShowLogo.Picture.LoadFromFile(ActiveCover);

          FCoverFileName := ActiveCover;
          FCoverJpg := COVER_IMAGE_FILE_URL + IntToStr(GetTickCount);

          FRDJRadioStatusJson.WriteRadioStatusJson(Setup.IcecastNowPlayingJsonFile,
                                                   Trim(mmoDjName.Text),
                                                   Trim(mmoShow.Text),
                                                   '',
                                                   '',
                                                   FCoverJpg);
        end;
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


function TMainMDIFrm.SaveSetupSafely(): Boolean;
var
  TmpFileName: string;
  BakFileName: string;

begin

  Result := False;

  if (Trim(FSetupFileName) = '') then
    Exit;

  TmpFileName := FSetupFileName + '.tmp';
  BakFileName := FSetupFileName + '.bak';

  DeleteFile(PChar(TmpFileName));

  try

    // Write the new setup to a side file first.
    // This prevents a forced Windows reboot from leaving the active INI half-written.
    SaveSetupToIni(TmpFileName,
                   FSetup);

    if FileExists(FSetupFileName) then
      CopyFile(PChar(FSetupFileName),
               PChar(BakFileName),
               False);

    Result := MoveFileEx(PChar(TmpFileName),
                         PChar(FSetupFileName),
                         MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH);

    if not Result then
      begin

        DeleteFile(PChar(FSetupFileName));
        Result := MoveFileEx(PChar(TmpFileName),
                             PChar(FSetupFileName),
                             MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH);
      end;

    if not Result then
      begin

        // Last-resort fallback. It is less safe, but avoids losing setup changes.
        SaveSetupToIni(FSetupFileName,
                       FSetup);
        Result := True;
      end;
  except

    Result := False;
  end;
end;


function TMainMDIFrm.IsProbablyValidStoredEndpointId(const ADeviceId: string): Boolean;
var
  S: string;

begin

  S := Trim(ADeviceId);

  // Normal IMMDevice ids are long strings like:
  // {0.0.0.00000000}.{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
  // Corrupted INI values are often empty, very short, or missing braces.
  Result := (Length(S) >= 16) and
            (Pos('{', S) > 0) and
            (Pos('}', S) > 0);
end;


function TMainMDIFrm.EndpointIdExists(const ADeviceId: string): Boolean;
var
  DevEnum: IMMDeviceEnumerator;
  Dev: IMMDevice;
  State: DWORD;

begin

  Result := False;

  if not IsProbablyValidStoredEndpointId(ADeviceId) then
    Exit;

  DevEnum := nil;
  Dev := nil;
  State := 0;

  try

    DevEnum := CreateComObject(CLSID_MMDeviceEnumerator) as IMMDeviceEnumerator;

    if FAILED(DevEnum.GetDevice(PWideChar(WideString(Trim(ADeviceId))),
                                Dev)) or
       (not Assigned(Dev)) then
      Exit;

    if FAILED(Dev.GetState(State)) then
      Exit;

    Result := (State = DEVICE_STATE_ACTIVE);
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

  // Cue/PFL is special:
  // - Valid-looking but temporarily absent USB headphones are runtime state.
  // - Corrupted/truncated INI strings must be removed, otherwise Windows audio
  //   can loop internally with ERROR_NOT_FOUND after updates/reboots.
  if (Trim(FSetup.PFLDeviceId) <> '') and
     (not IsProbablyValidStoredEndpointId(FSetup.PFLDeviceId)) then
    begin

      FSetup.PFLDeviceId := '';
      FSetup.PFLEnabled := False;
      Result := True;
    end;

  if Result then
    SaveSetupSafely();
end;


procedure TMainMDIFrm.ClearAudioEndpointSetup();
begin

  FSetup.MasterDeviceId := '';
  FSetup.MicDeviceId := '';

  // Keep cue settings intact.
  SaveSetupSafely();
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
      FCueOut.SetOutputDeviceId(PFLDeviceId);
      FCueUnavailableWarned := False;
    end
  else
    begin

      if FSetup.PFLEnabled and (PFLDeviceId <> '') then
        NotifyCueEndpointLostAsync();
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

  FRDJRadioStatusJson.WriteRadioStatusJson(FSetup.IcecastNowPlayingJsonFile,
                                           Trim(mmoDjName.Text),
                                           Trim(mmoShow.Text),
                                           '',
                                           '',
                                           FCoverFileName);
end;


procedure TMainMDIFrm.btnSetupClick(Sender: TObject);
begin

  OpenSetupGUI();
end;


procedure TMainMDIFrm.btnEffectsClick(Sender: TObject);
begin

  if not Assigned(FfrmMasterFxRack) then
    Exit;

  if (FfrmMasterFxRack.WindowState = wsMinimized) then
    begin

      FMasterDeck.Left := FfrmMasterFxRack.Left + FfrmMasterFxRack.Width;
      FfrmMasterFxRack.WindowState := wsNormal;
    end
  else
    begin

      FMasterDeck.Left := FfrmMasterFxRack.Left;
      FfrmMasterFxRack.WindowState := wsMinimized;
    end;
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
  RestartBroadcast: Boolean;

begin

  if FAudioGraphRestarting then
    begin

      FPendingHardRestart := True;
      Exit;
    end;

  FAudioGraphRestarting := True;
  FPendingHardRestart := False;

  RestartBroadcast := True; //FSetup.Broadcast.Enabled;

  if RestartBroadcast then
    StopBroadcast();

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
          begin

            FreeAndNil(FCueOut);
            NotifyCueEndpointLostAsync();
          end;
      end;

    if Assigned(FMasterDeck) then
      FMasterDeck.ApplyCurrentSetup();

    if RestartBroadcast then
      StartBroadcast();

  finally

    FAudioGraphRestarting := False;

    if FPendingHardRestart then
      begin

        FPendingHardRestart := False;
        QueueEndpointRefresh();
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
begin

  lblLocalTime.Caption := TimeToStr(Time);
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

  // PRE-FX tap for Icecast broadcast.
  BroadcastTapPreFx(OutBuffer,
                    Frames,
                    pwfx);

  // Apply master FX.
  ProcessMasterFx(OutBuffer,
                  Frames,
                  pwfx.nSamplesPerSec);

  // POST-FX tap for Icecast broadcast.
  BroadcastTapPostFx(OutBuffer,
                     Frames,
                     pwfx);

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


// Media Server/IceCast
procedure TMainMDIFrm.BroadcastStateChanged(Sender: TObject;
                                            NewState: TMfBroadcastState;
                                            const Msg: string);
begin

  if Assigned(FMediaServer) then
    begin
      case NewState of

        bsStopped: begin

                      FMediaServer.SetBroadcastUiState(False,
                                                       False);
                      MainMdifrm.chkMediaServer.Down := False;
                   end;

        bsConnecting: begin

                        FMediaServer.SetBroadcastUiState(True,
                                                         False);
                        MainMdifrm.chkMediaServer.Down := False;
                      end;

        bsLive: begin

                  FMediaServer.SetBroadcastUiState(True,
                                                   True);
                  MainMdifrm.chkMediaServer.Down := True;
                end;

        bsReconnecting: begin

                          FMediaServer.SetBroadcastUiState(True,
                                                           False);
                          MainMdifrm.chkMediaServer.Down := False;
                        end;

        bsError: begin

                   FMediaServer.SetBroadcastUiState(False,
                                                    False);
                   MainMdifrm.chkMediaServer.Down := False;
                 end;
      end;
    end;
end;
// Media Server/IceCast end

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
  SaveSetupSafely();
end;


procedure TMainMDIFrm.SetAudioRecorderRecordPostFx(const AValue: Boolean);
begin

  FSetup.AudioRecorderRecordPostFx := AValue;
  SaveSetupSafely();
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

  SaveSetupSafely();
end;


// JSON ========================================================================



// JSON end ====================================================================


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
