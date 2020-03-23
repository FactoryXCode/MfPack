// FactoryX
//
// Copyright: © FactoryX, Netherlands/Australia/Brazil. All rights reserved.
//
// Project: MFPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
// Module: fMFPlayerCore.pas
// Kind: Pascal / Delphi unit
// Release date: 07-07-2012
// Language: ENU
//
// Revision Version: 2.6.4
// Description: MfPlayer core engine
//
// Both versions use the same internal objects to render video, and they share many
// of the same interfaces.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Ramysés De Macedo Rodrigues
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 2014/06/18 Ramysés             Birth of the first Delphi MF player
// -----------------------------------------------------------------------------
//
// Remarks: -
//
//
// Related objects: -
// Related projects: MfPackX264
// Known Issues: -
//
// Compiler version: 23 up to 33
// SDK version: 10.0.19569.0
//
// Todo: -
//
//==============================================================================
// Source: MfPlayer example
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
unit fMFPlayerCore;


interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.Messages,
  VCL.Graphics,
  VCL.extctrls,
  System.Types,
  System.SysUtils,
  MfPack.MFApi,
  MfPack.PropIdl,
  MfPack.MFIdl,
  MfPack.EVR9,
  MfPack.EVR,
  MfPack.Mferror,
  MfPack.MfpTypes,
  MfPack.MfpUtils,
  MfPack.MFObjects;

const
  WM_APP_PLAYER_EVENT = WM_APP + 1;    // wparam = IMFMediaEvent*
  IID_IUnknown        : TGUID = '{00000000-0000-0000-C000-000000000046}';   // from MfPack.Unknown.pas

Type
      TCommand = (
                  CmdNone = 0,
                  CmdStop,
                  CmdStart,
                  CmdPause,
                  CmdSeek,
                  CmdClosing,
                  CmdOpenPending,
                  CmdClosed,
                  CmdReady
                  );

   PlayerState = (
                  Closed = 0,     // No session.
                  Ready,          // Session was created, ready to open a file.
                  OpenPending,    // Session is opening a file.
                  Starting,
                  Stoping,
                  Started,        // Session is playing a file.
                  Paused,         // Session is paused.
                  Stopped,        // Session is stopped (ready to play).
                  Closing         // Application has closed the session, but is waiting for MESessionClosed.
                 );

   TSeekState = packed record
                  Command: TCommand;
                  fRate: Double;      // Playback rate
                  bThin: Boolean;      // Thinned playback?
                  hnsStart: MFTIME;   // Start position
                end;

// Classe para manipular seções críticas
   TMFCritSec = class
   private
        FcriticalSection: TRTLCriticalSection;
   public
        constructor Create;
        destructor Destroy; override;
        procedure Lock;
        procedure Unlock;
   end;

Type

  TMFPlayer = class(TObject, IMFAsyncCallback)

  private

    // ponteiros de interfaces
    FpSession:      IMFMediaSession; // Media Session
    FpMediaSource:  IMFMediaSource;   // Media Source
    FpVideoDisplay: IMFVideoDisplayControl; //IUnkNown;   // , do EVR IInterface
    FpClock:        IMFClock;
    FpRateSupport:  IMFRateSupport;
    FpRateControl:  IMFRateControl;

    FCount:         Integer; // variável auxiliar para testes de desenvolvimento

    // objetos internos
    FTimer:         TTimer;
    FCritSec:       TMFCritSec;
    FRefCount:      Integer;

    // propriedades da mídia atual
    FFileName:      string;      // Path da mídia atual
    FdCaps:         DWORD;       // capabilites
    FbCanScrub:     Boolean;
    FtDuration:     UINT64;      // Duração
    FPasso:         Integer;     // valor do passo do stepup e stepDown
    FVolume:        double;      // nível de volume

    // controle de estabilidade
    FRequest:       TSeekState;
    FPending:       Boolean;     // flag de controle de comandos pendentes
    Fstate:         TSeekState;  // Estado do player

    // Outras variáveis membros... handles para funções internas...
    FhwndEvent:     HWND;           // janela que exibe o vídeo
    FhwndVideo:     HWND;           // janela que receberá as mensagens do MediaSession
    FhCloseEvent:   THandle;        // evento para esperar enquanto MediaSession completa operação de fechamento

    //-----------------------IUnknown methods ----------------------------------
    function QueryInterface(const riid: TGUID; out Obj): HResult; stdcall;
    function _AddRef(): Integer; stdcall;
    function _Release(): Integer; stdcall;
    //--------------------------------------------------------------------------

    {Métodos privados}

    //  Creates a media source from a URL.
    function CreateMediaSource(const sURL: string): HRESULT;

    // Create an activation object for a renderer, based on the stream media type
    function CreateMediaSinkActivate( pSourceSD: IMFStreamDescriptor; hVideoWnd: HWND;
                                     out ppActivate: IMFActivate): HRESULT;

    //  Creates a playback topology from the media source.
    //
    //  Pre-condition: The media source must be created already.
    //  Call CreateMediaSource() before calling this method
    function CreateTopologyFromSource(pSource: IMFMediaSource;
                                      pPD: IMFPresentationDescriptor;
                                      hVideoWnd: HWND;
                                      out ppTopology: IMFTopology
                                      ): HRESULT;

    // Add a source node to a topology.
    function AddSourceNode(  pTopology: IMFTopology;           // Topology.
                             pSource: IMFMediaSource;          // Media source.
                             pPD: IMFPresentationDescriptor;   // Presentation descriptor.
                             pSD: IMFStreamDescriptor;         // Stream descriptor.
                             out ppNode: IMFTopologyNode           // Receives the node pointer.
                            ): HRESULT;

     // Add an output node to a topology.
    function AddOutputNode(  pTopology: IMFTopology;       // Topology.
                             pStreamSink: IMFStreamSink;   // Stream sink.
                            out ppNode: IMFTopologyNode       // Receives the node pointer.
                            ): HRESULT; overload;


    // Add an output node to a topology com outros parâmetros
    function AddOutputNode(  pTopology: IMFTopology;      // Topology.
                             pActivate: IMFActivate;      // Media sink activation object.
                            dwId: DWORD;                     // Identifier of the stream sink.
                            out ppNode: IMFTopologyNode      // Receives the node pointer.
                            ): HRESULT; overload;

    //  Adds a topology branch for one stream.
    //
    //  pTopology: Pointer to the topology object.
    //  pSourcePD: The source's presentation descriptor.
    //  iStream: Index of the stream to render.
    //
    //  Pre-conditions: The topology must be created already.
    //
    //  Notes: For each stream, we must do the following:
    //    1. Create a source node associated with the stream.
    //    2. Create an output node for the renderer.
    //    3. Connect the two nodes.
    //  The media session will resolve the topology, so we do not have
    //  to worry about decoders or other transforms.
    function AddBranchToPartialTopology(var pTopology: IMFTopology;
                                        var pSource: IMFMediaSource;
                                        var pPD: IMFPresentationDescriptor;
                                        iStream: DWord;
                                        hVideoWnd: HWND): HRESULT;


    //---------------------------------------------------------------
    // Handlers
    // Handler for OnTopologyReady event (topology pronto para reprodução).
    function OnTopologyReady(pEvent: IMFMediaEvent):  HRESULT;
    // Handler for MEEndOfPresentation event (alcançou o final do registro).
    function OnPresentationEnded(pEvent: IMFMediaEvent): HRESULT;
    function OnStart(pEvent: IMFMediaEvent): HRESULT;  // Comando Start concluído
    function OnPause(pEvent: IMFMediaEvent): HRESULT;  // Comando Pause concluído
    function OnStop(pEvent: IMFMediaEvent): HRESULT;   // Comando Stop concluído
    //  Handler for MENewPresentation event.
    //
    //  This event is sent if the media source has a new presentation, which
    //  requires a new topology.
    function OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;

    //-----------------------------------------------------------------
    // IMFAsyncCallback methods
    // Implementation of this method is optional.
    function GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult; stdcall;
    // função callback chamada pelo MediaSession
    function Invoke(pAsyncResult: IMFAsyncResult): HResult; stdcall;
    // manipulador de eventos inteno
    function HandleEvent(pEventPtr: UINT_PTR ): HRESULT;

    function GetClassID(out classID: TCLSID): HResult; stdcall;

    //---------------- outros métodos ---------------------------------
    // create a new instance of the media session
    function CreateSession: HRESULT;
    // fecha seção
    function CloseSession: HRESULT;
    // inicial reprodução
    //function StartPlayBack: HRESULT;
    // Pode receber comandos Seek?
    function CanSeek(out pbCanSeek: Boolean): HRESULT;
    // função auxilizar para preencher a variável FDuration
    function GetInitDuration(out dur: MFTIME): HRESULT;
    //função auxiliar para atualizar FCurrPosition
    //function GetIntPosition: HRESULT;

   procedure SetVolume(Value: Single);
    function GetVolume: Single;

    // funções para executar o scrubbin
    function CanScrub(out pbCanScrub: BOOLEAN): HRESULT;
    function Scrub(bScrub: BOOLEAN): HRESULT;

    // inicializa plataforma e alguns parâmetros
    function Initialize: HRESULT;

    // funções auxiliares do SetRate.
    function CommitRateChange(fRate: double; bThin: Boolean): HRESULT;
    function GetNominalRate: double;
    function SetInternalPosition(tPos: MFTIME): HRESULT;
    function UpdatePendingCommands(cmd: TCommand): HRESULT;
    procedure UpdateCaption;

    // play, pause, stop internal. Use SendPlayerCmd()
    function Play: HRESULT;
    function Pause: HRESULT;
    function Stop: HRESULT;
    procedure Clear;


     {Métodos públicos}
  public
    constructor Create(hwndVideo, hwndEvent: HWND); overload;
    procedure BeforeDestruction; override;

    // playback
    function OpenURL(const sURL: String): HRESULT;
    function Repaint: HRESULT;
    function SendPlayerComand(cmd: TCommand): HRESULT;
    function StepUp(value: Integer): HRESULT;
    function StepDown(value: Integer): HRESULT;

    function ShutDown: HRESULT;

    // Playback control
    function GetState: TCommand;
    function GetDuration: MFTIME;
    function GetPosition(out posOut: MFTime): HRESULT;
    function SetPosition(posMs: MFTIME): HRESULT;


    // Sets the playback rate.
    function SetRate(frate: Single): HRESULT;
    function GetRate: Single;

    //  Resizes the video rectangle.
    //
    //  The application calls this method if the size of the video window changes;
    //  e.g., when the application receives a WM_SIZE message.
    function ResizeVideo(width: WORD; height: WORD): HRESULT;
    // função auxilizar para ver se tem vídeo na reprodução atual
    function HasVideo: boolean;
    // função callback do timer interno
    procedure TimerTimer(sender: TObject);
    // captura o frame em exibição
    function  GetCurrentFrame(var bit: TBitMap): HRESULT;

    property  FileName: string read FFileName;     // retorna o path do arquivo atualmente em reprodução
    property  IntTimer: TTimer read FTimer write FTimer;
    //property  TimerInterval: Integer read FtInterval write SetInterval default 500;
    property  Passo: Integer read FPasso write FPasso;
    property  Volume: Single read GetVolume write SetVolume;
  end;

  function FormatTime(MSECs: Cardinal): String;

implementation

{funções auxiliares}

function FormatTime(MSECs: Cardinal): String;
var Hor,Min,Sec,MSec: Cardinal;
begin
  if MSECS > 0 then
     begin
     Hor := ((MSECs div 3600000) mod 24); //Divide o TimeDif por 36*10^5 e pega o resto do resultado dividido por 24
     Min := ((MSECs div 60000) mod 60); //Faz a mesma coisa só muda os dados
     Sec := ((MSECs div 1000) mod 60); //Faz a mesma coisa só muda os dados²
     //MSec := (MSECs mod 1000); //Pega o resto de TimeDif dividido por 1000

     Result := Format('%.2d:%.2d:%.2d',[Hor,Min,Sec]); //Formata em HH:MM:SS.sss
     end
  else Result := '0';
end;

function RefTimeToMiliSec(RefTime: int64): Cardinal;
begin
  result := Cardinal(RefTime div 10000);
end;

function MiliSecToRefTime(Milisec: int64): Int64;
begin
  result := Milisec * 10000;
end;

procedure PropVariantInit(var pvar: MfPack.PropIdl.PROPVARIANT);
begin
  ZeroMemory(@pvar, sizeof(MfPack.PropIdl.PROPVARIANT));
end;

procedure PropVariantClear(var pvar: MfPack.PropIdl.PROPVARIANT);
begin
 ZeroMemory(@pvar, sizeof(MfPack.PropIdl.PROPVARIANT));
end;

function GetEventObject(pEvent: IMFMediaEvent; out ppObject): HRESULT;
var
    vVar: MfPack.PropIdl.PROPVARIANT;
    hr: HRESULT;
begin
    hr := pEvent.GetValue(vvar);
    if (SUCCEEDED(hr)) then
        begin
        if (vvar.vt = VT_UNKNOWN) then
            hr := vvar.punkVal.QueryInterface(IID_IUnknown,
                                              PPointer(ppObject))
        else
            hr := MF_E_INVALIDTYPE;

        PropVariantClear(vvar);
        end;
    Result := hr;
end;

function CreateSourceStreamNode(
                                pSource: IMFMediaSource;
                                pSourcePD: IMFPresentationDescriptor;
                                pSourceSD: IMFStreamDescriptor;
                                out ppNode: IMFTopologyNode
                                ): HRESULT;
var
   pNode: IMFTopologyNode;
   hr: HRESULT;

label Done;

begin
    if (not Assigned(pSource) or not Assigned(pSourcePD) or not Assigned(pSourceSD) or not Assigned(ppNode)) then
      begin
      Result := E_POINTER;
      Exit;
      end;

    // Create the source-stream node.
    hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE, pNode);
    if (FAILED(hr)) then goto done;

    // Set attribute: Pointer to the media source.
    hr := pNode.SetUnknown(MF_TOPONODE_SOURCE, pSource);
    if (FAILED(hr)) then goto done;

    // Set attribute: Pointer to the presentation descriptor.
    hr := pNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR, pSourcePD);
    if (FAILED(hr)) then goto done;

    // Set attribute: Pointer to the stream descriptor.
    hr := pNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR, pSourceSD);
    if (FAILED(hr)) then  goto done;

    // Return the IMFTopologyNode pointer to the caller.
    ppNode := pNode;
//    (*ppNode)->AddRef();

done:
    SafeRelease(pNode);
    Result := hr;
end;


function CreateOutputNode(
                          pSourceSD: IMFStreamDescriptor;
                          hwndVideo: HWND;
                          out ppNode: IMFTopologyNode
                          ): HRESULT;
var
    pNode: IMFTopologyNode;
    pHandler:  IMFMediaTypeHandler;
    pRendererActivate:  IMFActivate;
    guidMajorType: TGUID;
    streamID: DWORD;
    hr: HRESULT;

label Done;

begin

    // Get the stream ID.
    streamID := 0;
    pSourceSD.GetStreamIdentifier(streamID); // Just for debugging, ignore any failures.

    // Get the media type handler for the stream.
    hr := pSourceSD.GetMediaTypeHandler(pHandler);
    if (FAILED(hr)) then  goto done;

    // Get the major media type.
    hr := pHandler.GetMajorType(guidMajorType);
    if (FAILED(hr)) then goto done;

    // Create a downstream node.
    hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE, pNode);
    if (FAILED(hr)) then goto done;

    // Create an IMFActivate object for the renderer, based on the media type.
    if MFMediaType_Audio = guidMajorType then
       begin
        // Create the audio renderer.
       hr := MFCreateAudioRendererActivate(pRendererActivate);
       end
    else if MFMediaType_Video = guidMajorType then
        begin
        // Create the video renderer.
        hr := MFCreateVideoRendererActivate(hwndVideo, pRendererActivate);
        end
    else  hr := E_FAIL;

    if (FAILED(hr)) then goto done;

    // Set the IActivate object on the output node.
    hr := pNode.SetObject(pRendererActivate);
    if (FAILED(hr)) then goto done;

    // Return the IMFTopologyNode pointer to the caller.
    ppNode := pNode;
//    (*ppNode)->AddRef();

done:
    SafeRelease(pNode);// := nil;
    SafeRelease(pHandler);// := nil;
    SafeRelease(pRendererActivate);// := nil;
    Result := hr;
end;

{ TMFPlay }

function TMFPlayer.AddBranchToPartialTopology(var pTopology: IMFTopology;
                                              var pSource: IMFMediaSource;
                                              var pPD: IMFPresentationDescriptor;
                                              iStream: DWord;
                                              hVideoWnd: HWND): HRESULT;
var
   pSD:  IMFStreamDescriptor;
   pSinkActivate:  IMFActivate;
   pSourceNode: IMFTopologyNode;
   pOutputNode:  IMFTopologyNode;
   fSelected:    BOOL;
   hr: HRESULT;

label Done;
begin
    assert(pTopology <> nil);

    // Get the stream descriptor for this stream.
    hr := pPD.GetStreamDescriptorByIndex(iStream,
                                         fSelected,
                                         pSD);
    if (FAILED(hr)) then   goto done;

    // Create the topology branch only if the stream is selected.
    // Otherwise, do nothing.
    if (fSelected) then
        begin
        // create the media sink activation object
        hr := CreateMediaSinkActivate(pSD, hVideoWnd, pSinkActivate);
        if (FAILED(hr)) then goto done;

        // Create a source node for this stream.
        hr := AddSourceNode(pTopology, pSource, pPD, pSD, pSourceNode);
        if (FAILED(hr)) then goto done;

        // Create the output node for the renderer.
        hr := AddOutPutNode(pTopology, pSinkActivate, 0, pOutPutNode);
        if (FAILED(hr)) then goto done;

        // Connect the source node to the output node.
        hr := pSourceNode.ConnectOutput(0, pOutputNode, 0);

        end;

done:
    // Clean up.
    SafeRelease(pSD);// := nil;
    SafeRelease(pSinkActivate);//:= nil;
    SafeRelease(pSourceNode);//:= nil;
    SafeRelease(pOutputNode);//:= nil;
    Result := hr;
end;

// Add a source node to a topology.
function TMFPlayer.AddSourceNode(
                                   pTopology: IMFTopology;           // Topology.
                                   pSource: IMFMediaSource;          // Media source.
                                   pPD: IMFPresentationDescriptor;   // Presentation descriptor.
                                   pSD: IMFStreamDescriptor;         // Stream descriptor.
                                  out ppNode: IMFTopologyNode         // Receives the node pointer.
                                  ): HRESULT;
var
  pNode: IMFTopologyNode;
  hr: HRESULT;

label Done;

begin
     pNode := nil;

    // Create the node.
    hr := MFCreateTopologyNode(MF_TOPOLOGY_SOURCESTREAM_NODE, pNode);
    if (FAILED(hr)) then goto done;

    // Set the attributes.
    hr := pNode.SetUnknown(MF_TOPONODE_SOURCE, pSource);
    if (FAILED(hr)) then goto done;

    hr := pNode.SetUnknown(MF_TOPONODE_PRESENTATION_DESCRIPTOR, pPD);
    if (FAILED(hr)) then goto done;

    hr := pNode.SetUnknown(MF_TOPONODE_STREAM_DESCRIPTOR, pSD);
    if (FAILED(hr)) then goto done;

    // Add the node to the topology.
    hr := pTopology.AddNode(pNode);
    if (FAILED(hr)) then goto done;

    // Return the pointer to the caller.
    ppNode := pNode;
//    (*ppNode)->AddRef();

done:
   SafeRelease(pNode);// := nil;
   Result := hr;
end;


procedure TMFPlayer.BeforeDestruction;
begin
  FreeAndNil(FTimer);
  if Assigned(FCritSec) then FreeAndNil(FCritSec);
  // The application must call Shutdown because the media session holds a
  // reference count on the CPlayer object. (This happens when CPlayer calls
  // IMediaEventGenerator::BeginGetEvent on the media session.) As a result,
  // there is a circular reference count between the CPlayer object and the
  // media session. Calling Shutdown breaks the circular reference count.

  // If CreateInstance failed, the application will not call Shutdown. To
  // handle that case, we must call Shutdown() in the destructor. The
  // circular ref-count problem does not occcur if CreateInstance has failed.
  // Also, calling Shutdown twice is harmless.

  if assigned(FpSession) then
     ShutDown;

  SafeRelease(FpSession);
  SafeRelease(FpMediaSource);
  SafeRelease(FpVideoDisplay);
  SafeRelease(FpClock);
  SafeRelease(FpRateSupport);
  SafeRelease(FpRateControl);


  inherited;

end;

function TMFPlayer.AddOutputNode(
                                   pTopology: IMFTopology;     // Topology.
                                   pStreamSink: IMFStreamSink; // Stream sink.
                                  out ppNode: IMFTopologyNode    // Receives the node pointer.
                                  ):   HRESULT;
var
    pNode: IMFTopologyNode;
    hr : HRESULT;

begin
    pNode := nil;
    hr := S_OK;

    // Create the node.
    hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE, pNode);

    // Set the object pointer.
    if (SUCCEEDED(hr)) then
       hr := pNode.SetObject(pStreamSink);

    // Add the node to the topology.
    if (SUCCEEDED(hr)) then
       hr := pTopology.AddNode(pNode);

    if (SUCCEEDED(hr)) then
       hr := pNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE, 1);

    // Return the pointer to the caller.
    if (SUCCEEDED(hr)) then
        ppNode := pNode;
//        (*ppNode)->AddRef();


    SafeRelease(pNode);// := nil;

    Result := hr;
end;

// Add an output node to a topology.
function TMFPlayer.AddOutputNode(
                                   pTopology: IMFTopology;     // Topology.
                                   pActivate: IMFActivate;     // Media sink activation object.
                                  dwId: DWORD;                 // Identifier of the stream sink.
                                  out ppNode: IMFTopologyNode  // Receives the node pointer.
                                   ): HRESULT;

var
   pNode: IMFTopologyNode;
   hr: HRESULT;

label Done;

begin

    // Create the node.
    hr := MFCreateTopologyNode(MF_TOPOLOGY_OUTPUT_NODE, pNode);
    if (FAILED(hr)) then goto done;

    // Set the object pointer.
    hr := pNode.SetObject(pActivate);
    if (FAILED(hr)) then goto done;

    // Set the stream sink ID attribute.
    hr := pNode.SetUINT32(MF_TOPONODE_STREAMID, dwId);
    if (FAILED(hr)) then goto done;

    hr := pNode.SetUINT32(MF_TOPONODE_NOSHUTDOWN_ON_REMOVE, 0);
    if (FAILED(hr)) then goto done;

    // Add the node to the topology.
    hr := pTopology.AddNode(pNode);
    if (FAILED(hr)) then goto done;

    // Return the pointer to the caller.
    ppNode := pNode;
//    (*ppNode)->AddRef();

done:
    SafeRelease(pNode);// := nil;
    Result := hr;
end;

function TMFPlayer.CanScrub(out pbCanScrub: BOOLEAN): HRESULT;
begin
    pbCanScrub := FbCanScrub;
end;

function TMFPlayer.CanSeek(out pbCanSeek: Boolean): HRESULT;
begin
//   if not Assigned(pbCanSeek) then
//     begin
//     Result := E_POINTER;
//     Exit;
//     end;

    // Note: The MFSESSIONCAP_SEEK flag is sufficient for seeking. However, to
    // implement a seek bar, an application also needs the duration (to get
    // the valid range) and a presentation clock (to get the current position).

    pbCanSeek := (
        ((FdCaps and MFSESSIONCAP_SEEK) = MFSESSIONCAP_SEEK) and
        (FtDuration > 0) and (FpClock <> nil));

    Result := S_OK;
end;

procedure TMFPlayer.Clear;
begin
  with FRequest do
     begin
     Command := CmdNone;
     fRate := 1.0;
     bThin := False;
     hnsStart := 0;
     end;

  with Fstate do
     begin
     Command := CmdNone;
     fRate := 1.0;
     bThin := False;
     hnsStart := 0;
     end;
  FtDuration := 0;
  FPending := False;
  FdCaps := 0;
  FFileName := '';

  FpSession := nil;
  FpMediaSource := nil;

end;

function TMFPlayer.CloseSession: HRESULT;
var
   hr: HRESULT;
   dwWaitResult: DWORD;

label Done;
begin
    hr := s_ok;

    // release the video display object
    FpVideoDisplay := nil;
//    SAFE_RELEASE(FpVideoDisplay);

    // First close the media session.
    if Assigned(FpSession) then
       begin
       dwWaitResult := 0;
       Fstate.Command := CmdClosing;

       hr := FpSession.Close; // envia mensagem MESessionClosed quando terminar de fechar
       if (FAILED(hr)) then goto done;

       // Wait for the close operation to complete
       dwWaitResult := WaitForSingleObject(THandle(FhCloseEvent), 2000); //  1000

       if dwWaitResult <> 0 {Wait abandoned} then
          begin
          Fcount := dwWaitResult;
          //goto done;
          end;
       end;


    // Complete shutdown operations.
    // Shut down the media source. (Synchronous operation, no events.)
    if Assigned(FpMediaSource) then
       begin
       hr := FpMediaSource.Shutdown;
       end;

    // Shut down the media session. (Synchronous operation, no events.)
    if Assigned(FpSession) then
       begin
       hr := FpSession.Shutdown;
       end;

//    FpMediaSource  := nil;
//    FpSession := nil;

    SafeRelease(FpMediaSource);
    SafeRelease( FpSession);

    Fstate.Command := CmdClosed;
    hr := S_OK;
done:
    Result := hr;
end;

function TMFPlayer.CommitRateChange(fRate: double; bThin: Boolean): HRESULT;
//var
//   hr: HRESULT;
//   hnsSystemTime: MFTIME;
//   hnsClockTime:  MFTIME;
//   cmdNow: TCommand;
begin
//	assert(!m_bPending);
//
//	// Caller holds the lock.
//
//	hr := S_OK;
//	hnsSystemTime := 0;
//	hnsClockTime := 0;
//
////	Command  = m_state.command;
//
//	IMFClock *pClock = NULL;
//
//	// Allowed rate transitions:
//
//	// Positive <-> negative:   Stopped
//	// Negative <-> zero:       Stopped
//	// Postive <-> zero:        Paused or stopped
//
//	if ((fRate > 0 && m_state.fRate <= 0) || (fRate < 0 && m_state.fRate >= 0))
//	{
//		// Transition to stopped.
//		if (cmdNow == CmdStart)
//		{
//			// Get the current clock position. This will be the restart time.
//			hr = m_pSession->GetClock(&pClock);
//			if (FAILED(hr))
//			{
//				goto done;
//			}
//
//			(void)pClock->GetCorrelatedTime(0, &hnsClockTime, &hnsSystemTime);
//
//			assert(hnsSystemTime != 0);
//
//			// Stop and set the rate
//			hr = Stop();
//			if (FAILED(hr))
//			{
//				goto done;
//			}
//
//			// Cache Request: Restart from stop.
//			m_request.command = CmdSeek;
//			m_request.hnsStart = hnsClockTime;
//		}
//		else if (cmdNow == CmdPause)
//		{
//			// The current state is paused.
//
//			// For this rate change, the session must be stopped. However, the
//			// session cannot transition back from stopped to paused.
//			// Therefore, this rate transition is not supported while paused.
//
//			hr = MF_E_UNSUPPORTED_STATE_TRANSITION;
//			goto done;
//		}
//	}
//	else if (fRate == 0 && m_state.fRate != 0)
//	{
//		if (cmdNow != CmdPause)
//		{
//			// Transition to paused.
//
//			// This transisition requires the paused state.
//
//			// Pause and set the rate.
//			hr = Pause();
//			if (FAILED(hr))
//			{
//				goto done;
//			}
//
//			// Request: Switch back to current state.
//			m_request.command = cmdNow;
//		}
//	}
//
//	// Set the rate.
//	hr = m_pRate->SetRate(bThin, fRate);
//	if (FAILED(hr))
//	{
//		goto done;
//	}
//
//	// Adjust our current rate and requested rate.
//	m_request.fRate = m_state.fRate = fRate;
//
//done:
//	SafeRelease(&pClock);
//	return hr;

end;

constructor TMFPlayer.Create(hwndVideo, hwndEvent: HWND);
var hr: HRESULT;
begin
  FhwndVideo := hwndVideo;
  FhwndEvent := hwndEvent;
  // inicializa algumas variáveis
  Clear;

  // inicializa handle de eventos...
  hr := Initialize;

  if FAILED(hr) then
     begin
     MessageBox(0, pChar('Ocorreu um erro ao criar o sinalizador MESessionClosed'),
                pChar('Erro!'), MB_ICONEXCLAMATION);
     end;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 500;
  FTimer.OnTimer := TimerTimer;

  

  // cria objetos de manipulação de seções críticas
  FCritSec := TMFCritSec.Create;

end;


function TMFPlayer.CreateMediaSinkActivate(pSourceSD: IMFStreamDescriptor;
                                           hVideoWnd: HWND;
                                           out ppActivate: IMFActivate): HRESULT;
var
  phandler: IMFMediaTypeHandler;
  pActivate: IMFActivate;
  guidMajorType: TGUID;
  hr : HRESULT;

label Done;
begin
  // Get the media type handler for the stream
  hr := pSourceSD.GetMediaTypeHandler(pHandler);
  if FAILED(hr) then goto Done;

  // Get the major media type
  hr := pHandler.GetMajorType(guidMajorType);
  if FAILED(hr) then goto Done;

  // Create an IMFActivate object for the renderer, based on the media type
  if (MFMediaType_Audio = guidMajorType) then
     hr := MFCreateAudioRendererActivate(pActivate)
  else
    if (MFMediaType_Video = guidMajorType) then
      hr := MFCreateVideoRendererActivate(hVideoWnd, pActivate)
  else
    hr := E_FAIL;

  if FAILED(hr) then goto Done;

  //return IMFactivate pointer to caller
  ppActivate := pActivate;

done:
  SafeRelease(phandler);
  SafeRelease(pActivate);
  Result := hr;
end;

function TMFPlayer.CreateMediaSource(const sURL: string): HRESULT;
var
    ObjectType: MF_OBJECT_TYPE;
    pSourceResolver: IMFSourceResolver;
    pSource: IInterface;
    hr: HRESULT;

label Done;
begin

    ObjectType := MF_OBJECT_MEDIASOURCE;

    SafeRelease(FpMediaSource);

    // Create the source resolver.
    hr := MFCreateSourceResolver(pSourceResolver);
    if (FAILED(hr)) then goto done;

    // Use the source resolver to create the media source.

    // Note: For simplicity this sample uses the synchronous method on
    // IMFSourceResolver to create the media source. However, creating a media
    // source can take a noticeable amount of time, especially for a network
    // source. For a more responsive UI, use the asynchronous
    // BeginCreateObjectFromURL method.

    hr := pSourceResolver.CreateObjectFromURL(
                                              pWideChar(sURL),            // URL of the source.
                                              MF_RESOLUTION_MEDIASOURCE,  // Create a source object.
                                              nil,                       // Optional property store.
                                              ObjectType,                // Receives the created object type.
                                              pSource                    // Receives a pointer to the media source (IInterface).
                                              );
    if (FAILED(hr)) then goto done;

    // Get the IMFMediaSource interface from the media source.
    hr := pSource.QueryInterface(IID_IMFMediaSource,
                                 FpMediaSource);

Done:
    SafeRelease(pSourceResolver);
    SafeRelease(pSource);
    Result := hr;
end;

function TMFPlayer.CreateSession: HRESULT;
var
   hr: HRESULT;

label Done;
begin
    // Close the old session, if any.
    hr := CloseSession();
    if (FAILED(hr)) then goto done;

    assert(Fstate.Command = CmdClosed);

    // Create the media session.
    hr := MFCreateMediaSession(nil, FpSession);
    if (FAILED(hr)) then goto done;

    Fstate.Command := CmdReady;

    // Start pulling events from the media session
    hr := FpSession.BeginGetEvent(IMFAsyncCallback(Self), nil);
    if (FAILED(hr)) then goto done;

done:
    Result := hr;
end;

function TMFPlayer.CreateTopologyFromSource(pSource: IMFMediaSource;
                                            pPD: IMFPresentationDescriptor;
                                            hVideoWnd: HWND;
                                            out ppTopology: IMFTopology): HRESULT;
var
    pTopology: IMFTopology;
    cSourceStreams : DWORD;
    hr: HRESULT;
    i: Integer;

label Done;
begin
    assert(FpSession <> nil);
    assert(FpMediaSource <> nil);
    cSourceStreams := 0;

    // Create a new topology.
    hr := MFCreateTopology(pTopology);
    if (FAILED(hr)) then goto done;

    // get number of streams in the media source
    hr := pPD.GetStreamDescriptorCount(cSourceStreams);
    if (FAILED(hr)) then goto done;

    // For each stream, create the topology nodes and add them to the topology.
    for i := 0 to cSourceStreams - 1 do
       begin
       hr := AddBranchToPartialTopology(pTopology, pSource, pPD, i, hVideoWnd);
       if (FAILED(hr)) then goto done;
       end;

    // Return the IMFTopology pointer to the caller.
    ppTopology := pTopology;

done:
    SafeRelease(pTopology);
    Result := hr;
end;

function TMFPlayer.GetClassID(out classID: TCLSID): HResult;
begin
  classID := IID_IUnknown;
  Result := S_OK;
end;

function TMFPlayer.GetCurrentFrame(var bit: TBitMap): HRESULT;
var
    buffer, data: PByte;
    bufSize : Winapi.Windows.DWORD;
    i: Integer;
    bmi: BITMAPINFOHEADER;
    pBmi: PBitmapInfoheader;
    timestamp: MFTIME;
    hr: HRESULT;
begin
    Assert(bit <> nil);

    ZeroMemory (@bmi, sizeof(BITMAPINFOHEADER));
    bmi.biSize := sizeof(BITMAPINFOHEADER);
    data := nil;
    bufsize := $0000;
    //pBmi := PBitMapInfoHeader(@bmi);
    Result := E_FAIL;

    if Assigned(FpVideoDisplay) then
       begin
       hr := FpVideoDisplay.GetCurrentImage(Bmi,
                                            buffer,
                                            bufSize,
                                            timestamp);
       if FAILED(hr) then Exit;

       data := buffer;  //data é um ponteiro temporário
       end;

    // carrega no bitmap, que já foi criado pela função chamadora desta.
    if (bmi.biSizeImage > 0) and (data <> nil) then
        begin
        // ajusta tamanho e formato do bitmap de saída
        Bit.PixelFormat := pf32bit;
        Bit.SetSize(abs(bmi.biWidth), abs(bmi.biHeight));
        for i := abs(bmi.biHeight) - 1 downto 0 do
                // (int y = h - 1; y >= 0; --y)
           begin
          CopyMemory(Bit.ScanLine[i], // ponteiro de destino
                     data, // ponteiro do source
                     bmi.biWidth * bmi.biBitCount div 8); // quantidade de bytes

          // data contém o endereço da posição atual do array de bytes que representa a imagem
          Inc(data, bmi.biWidth * bmi.biBitCount div 8);
           end;

       Result := S_OK;
       end;

    data := nil;
    Buffer := nil;
end;

function TMFPlayer.GetDuration: MFTIME;
begin
   Result := FtDuration;
end;

function TMFPlayer.GetInitDuration(out dur: MFTIME): HRESULT;
var
  pPD:  IMFPresentationDescriptor;
  hr: HRESULT;
  mdur: UINT64;

begin
    if Assigned(FpMediaSource) then
       begin
       dur := 0;
       hr := FpMediaSource.CreatePresentationDescriptor(pPD);
       if (SUCCEEDED(hr)) then
           hr := pPD.GetUINT64(MF_PD_DURATION, mdur);
           dur := mdur; // resultado em reftime
           FtDuration := dur;
       end;

    pPD := nil;
    Result := hr;
end;

//function TMFPlayer.GetIntPosition: HRESULT;
//var
//    pMFTime: MFTIME;
//    clkState: MF_CLOCK_STATE;
////    posMs: Cardinal;
//begin
//
//   FCritSec.Lock;
//   try
//       if not Assigned(FpClock) then
//          begin
//          Result := MF_E_NO_CLOCK;
//          exit;
//          end;
//
//    if (FRequest.Command = CmdSeek) then
//        FCurrPosition := RefTimeToMiliSec(FRequest.hnsStart)
//    else if FPending then
//        FCurrPosition := RefTimeToMiliSec(Fstate.hnsStart)
//    else
//         begin
//         if SUCCEEDED(FpClock.GetState(0, clkState)) and not (clkState = MFCLOCK_STATE_INVALID) then
//            begin
//            Result := (FpClock as IMFPresentationClock).GetTime(pMFTime);
//            if SUCCEEDED(Result) {and (pMFTime > 0)} then
//               begin
//               FCurrPosition := RefTimeToMiliSec(pMFTime);
//               end;
//            end;
//          end;
//
//   finally
//        FCritSec.Unlock;
//   end;
//end;

function TMFPlayer.GetNominalRate: double;
begin
  Result := FRequest.fRate;
end;

function TMFPlayer.GetParameters(out pdwFlags, pdwQueue: DWord): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TMFPlayer.GetPosition(out posOut: MFTime): HRESULT;
var
   clkState: MF_CLOCK_STATE;
   pos: Cardinal;
begin

  if not Assigned(FpClock) then exit;
  // Return, in order:
  // 1. Cached seek request (nominal position).
  // 2. Pending seek operation (nominal position).
  // 3. Presentation time (actual position).

  try
    FCritSec.Lock;
    posOut := 0;
  if (Frequest.command = CmdSeek) then
      posOut := Frequest.hnsStart
  else if FPending then
      posOut := Fstate.hnsStart
  else if SUCCEEDED(FpClock.GetState(0, clkState)) and (clkState = MFCLOCK_STATE_RUNNING) then
         Result := (FpClock as IMFPresentationClock).GetTime(posOut)
  finally
    FCritSec.Unlock;
  end;
end;

function TMFPlayer.GetRate: Single;
begin
  Result := FRequest.fRate;
end;

function TMFPlayer.GetState: TCommand;
begin
  Result := FState.Command;
end;

function TMFPlayer.GetVolume: Single;
var
   pVol: IMFAudioStreamVolume;
   nChannels: Cardinal;
   naVolumes: array of Float;
   hr: HRESULT;

begin

   hr := (FpSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE,
                                                 IID_IMFAudioStreamVolume,
                                                 pVol);
   if SUCCEEDED(hr) then
      begin
      hr := pVol.GetChannelCount(nChannels);
      SetLength(naVolumes, nChannels);
      hr := pVol.GetAllVolumes(nChannels, @naVolumes[0]);
      end;
   Result := naVolumes[0]; // return first channel
end;

function GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult;
begin
   Result := E_NOTIMPL;
end;

function TMFPlayer.Initialize: HRESULT;
var h: THandle;
begin
 if FhCloseEvent <> 0 then
    begin
    Result := MF_E_ALREADY_INITIALIZED;
    exit;
    end;

   h := CreateEvent(nil, FALSE, FALSE, nil);
   FhCloseEvent := THandle(h);
//  FhCloseEvent := CreateSemaphore(nil, 0, 1, nil);
  if FhCloseEvent = 0 then
     begin
     Result := GetLastError();
     end;
end;

function TMFPlayer.Invoke(pAsyncResult: IMFAsyncResult): HResult;
var
   pEvent: IMFMediaEvent;
   hr: HRESULT;
   meType: MediaEventType;

label Done;
begin

    if not Assigned(FpSession) then Exit;

    // Get the event from the event queue.
    hr := FpSession.EndGetEvent(pAsyncResult, pEvent);
    if (FAILED(hr)) then goto done;

    // Get the event type.
    hr := pEvent.GetType(meType);
    if (FAILED(hr)) then goto done;

    if meType = MESessionClosed then
        begin
        // If the session is closed, the application is waiting on the event
        // handle. Also, do not request any more events from the session.

        SetEvent(THandle(FhCloseEvent));
        end
    else
        begin
        // For all other events, ask the media session for the
        // next event in the queue.
        hr := FpSession.BeginGetEvent(IMFAsyncCallback(Self), nil);
        if (FAILED(hr)) then goto done;
        end;

//    if meType = MESessionStopped then
//       MessageBox(0, '', '', 0 );


    // For most events, post the event as a private window message to the
    // application. This lets the application process the event on its main
    // thread.

    // However, if a call to IMFMediaSession::Close is pending, it means the
    // application is waiting on the m_hCloseEvent event handle. (Blocking
    // call.) In that case, we simply discard the event.

    // When IMFMediaSession::Close is called, MESessionClosed is NOT
    // necessarily the next event that we will receive. We may receive any
    // number of other events before receiving MESessionClosed.

    if (Fstate.Command <> CmdClosing) then
       begin
       pEvent._AddRef();
       // envia mensagem para o manipulador de eventos
       HandleEvent(UINT_PTR(pEvent));
       end;

done:
    SafeRelease(pEvent);
    Result:= hr; // verifica erros
end;

function TMFPlayer.HandleEvent(pEventPtr: UINT_PTR): HRESULT;
var
  hrStatus: HRESULT;
  TopoStatus: MF_TOPOSTATUS;
  meType: MfPack.MFObjects.MediaEventType;
  pEvent: MfPack.MFObjects.IMFMediaEvent;
  hr: HRESULT;
  pMFT: MFTIME;

label Done;

begin
    hrStatus := S_OK;
    meType := MEUnknown;
    pEvent := IMFMediaEvent(pEventPtr);
    //pEvent := pEventPtr;

    if not Assigned(pEvent) then
       begin
       Result := E_POINTER;
       Exit;
       end;

    // Get the event type.
    hr := pEvent.GetType(meType);
    if (FAILED(hr)) then goto done;

    // Get the event status. If the operation that triggered the event
    // did not succeed, the status is a failure code.
    hr := pEvent.GetStatus(hrStatus);

    // Check if the async operation succeeded.
    if (SUCCEEDED(hr) and FAILED(hrStatus)) then hr := hrStatus;

    if (FAILED(hr)) then goto done;


    if meType = MESessionTopologyStatus then
       begin
       hr := pEvent.GetUINT32(MF_EVENT_TOPOLOGY_STATUS, UINT32(TopoStatus));
       if TopoStatus = MF_TOPOSTATUS_READY then
          begin
          hr := OnTopologyReady(pEvent);
          end;
       end
    else
    if meType = MEEndOfPresentation then // alcançou o final do registro
        hr := OnPresentationEnded(pEvent)
    else if meType = MESessionStopped then
        hr := OnStop(pEvent)
    else if meType = MENewPresentation then   // novo registro
        hr := OnNewPresentation(pEvent)
    else if meType = MESessionStarted then    // iniciou reprodução
        hr := OnStart(pEvent)
    else if meType = MESessionPaused then
        hr := OnPause(pEvent)
    else if meType = MESessionStopped then
        hr := OnStop(pEvent);

done:
    SafeRelease(pEvent);
    Result := hr;

end;

function TMFPlayer.HasVideo: boolean;
begin
  Result := FpVideoDisplay <> nil;
end;

function TMFPlayer.OnTopologyReady(pEvent: IMFMediaEvent):  HRESULT;
var
   rc: TRect;
   iid: IInterface;
begin
        FpVideoDisplay := nil;

        // Get the IMFVideoDisplayControl interface from EVR. This call is
        // expected to fail if the media file does not have a video stream.

        MFGetService(FpSession,
                     MR_VIDEO_RENDER_SERVICE,
                     IID_IMFVideoDisplayControl,
                     iid);

        // ajusta aspect ratio sem alterar tamanho da janela
        if Assigned(iid) then
           begin
           FpVideoDisplay := iid as IMFVideoDisplayControl;
           Winapi.Windows.GetClientRect(FhwndVideo, rc);

           if Assigned(FpVideoDisplay) then
              begin
              FpVideoDisplay.SetAspectRatioMode(DWord(MFVideoARMode_PreservePicture));
              FpVideoDisplay.SetVideoPosition(nil, @rc);
              end;
           end;

        //FTimer.Enabled := true;
        Result := Play();
end;

function TMFPlayer.OnNewPresentation(pEvent: IMFMediaEvent): HRESULT;
begin
  messagebox(0, pchar('Nova reprodução'), pchar(''), MB_OK);
end;

function TMFPlayer.OnPause(pEvent: IMFMediaEvent): HRESULT;
begin
  Fstate.Command := CmdPause;
  UpdatePendingCommands(CmdPause);
  Result := S_OK;
end;

function TMFPlayer.OnPresentationEnded(pEvent: IMFMediaEvent): HRESULT;
begin
    Stop;
    //SetPosition(FtDuration);
    Result := S_OK;
end;

function TMFPlayer.OnStart(pEvent: IMFMediaEvent): HRESULT;
begin
  UpdatePendingCommands(CmdStart);
  Result := S_OK;
end;

function TMFPlayer.OnStop(pEvent: IMFMediaEvent): HRESULT;
begin
 UpdatePendingCommands(CmdStop);
 Result := S_OK;
end;

function TMFPlayer.OpenURL(const sURL: String): HRESULT;
var
   pTopology: IMFTopology;
   pSourcePD: IMFPresentationDescriptor;
   pClock: IMFClock;
   pInt: IUnknown; //IInterface;

   hr: System.HRESULT;

label Done;

begin

    // 1. Create a new media session.
    // 2. Create the media source.
    // 3. Create the topology.
    // 4. Queue the topology [asynchronous]
    // 5. Start playback [asynchronous - does not happen in this method.]

    try

    FFilename := sURL;

//    // Cria Media session, fechando antes a aberta, se houver
    hr := CreateSession();
    if (FAILED(hr)) then goto done;

    // Create the media source.
    hr := CreateMediaSource(sURL);
    if (FAILED(hr)) then goto done;

    // Create the presentation descriptor for the media source
    hr := FpMediaSource.CreatePresentationDescriptor(pSourcePD);
    if (FAILED(hr)) then goto done;

    // Create a partial topology, com branches, source nodes e sink nodes.
    hr := CreateTopologyFromSource(FpMediaSource, pSourcePD, FhwndVideo, pTopology);
    if (FAILED(hr)) then goto done;

    // Set the topology on the media session.
    hr := FpSession.SetTopology(MFSESSION_SETTOPOLOGY_IMMEDIATE, pTopology);
    if (FAILED(hr)) then goto done;

    // If SetTopology succeeds, the media session will queue an
    // MESessionTopologySet event.

    // Get the duration from the presentation descriptor (optional)
    pSourcePD.GetUINT64(MF_PD_DURATION, FtDuration);

    // get Clock
    FpSession.GetClock(pClock);
    hr := pClock.QueryInterface(IID_IMFPresentationClock, FpClock);

    hr := FpSession.GetSessionCapabilities(FdCaps); // obtém capabilities da sessão atual
    if FAILED(hr) then FdCaps := $0001;

    // Get the rate control interface (optional)
    MFGetService(FpSession, MF_RATE_CONTROL_SERVICE, IID_IUnknown, pInt);
    pInt.QueryInterface(IID_IMFRateControl, FpRateControl);
    FpRateSupport := FpSession as IMFRateSupport;

    // Set our state to "open pending"
    Fstate.Command := CmdOpenPending;
done:
    if (FAILED(hr)) then
       begin
       Fstate.Command := CmdClosed;
       Clear;
       end;

    pTopology := nil;
    pSourcePD := nil;
    pClock := nil;
    pInt := nil;
    Result := hr;

    except
       MessageBox(0, pChar('Ocorreu um erro hr: ' + InttoStr(hr)), pChar(''), MB_ICONERROR);

    end;


end;

function TMFPlayer.Pause: HRESULT;
begin

   if (FpSession = nil) or (FpMediaSource = nil) then
       begin
       Result := E_UNEXPECTED;
       Exit;
       end;

   if FPending then
      FRequest.Command := CmdPause
   else
      begin
      Result := FpSession.Pause();

      Fstate.command := CmdPause;
      FPending := true;
      end;
end;

function TMFPlayer.Play: HRESULT;
var
   varStart: MfPack.PropIdl.PROPVARIANT;
   HR: HRESULT;
begin
    if (FpSession = nil) or (FpMediaSource = nil) then
      begin
        Result := E_UNEXPECTED;
        Exit;
      end;

    // If another operation is pending, cache the request.
    // Otherwise, start the media session.
    if FPending then
       FRequest.command := CmdStart
     else
        begin
        PropVariantInit(varStart);
        varStart.vt := VT_EMPTY;
        hr :=  FpSession.Start(GUID_NULL, varStart);
        FState.Command := CmdStart;
        FPending := True;
        PropVariantClear(varStart);
    end;
    Result := hr;
end;


function TMFPlayer.QueryInterface(const riid: TGUID;
  out Obj): HResult;
begin
   if GetInterface(riid, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TMFPlayer.Repaint: HRESULT;
var
   ps: PAINTSTRUCT;
   rc: TRECT;
   hHDc: HDC;

begin
  Result := S_OK;

  if HasVideo then // é porque FpVideoDisplayControl <> nil
     begin
     Result := (FpVideoDisplay as IMFVideoDisplayControl).RepaintVideo();
     end
  else
    Result := S_OK;
end;

function TMFPlayer.ResizeVideo(width, height: WORD): HRESULT;
var rcdest: TRect;
begin
    Result := S_OK;

    if Assigned(FpVideoDisplay) then
       begin
       // Set the destination rectangle.
       // Leave the default source rectangle (0,0,1,1).
       Winapi.Windows.GetClientRect(FhwndVideo, rcDest);
//       TRect.Create(0,0,width, height);
       Result := (FpVideoDisplay as IMFVideoDisplayControl).SetVideoPosition(nil, @rcDest);
       end;
end;

function TMFPlayer.Scrub(bScrub: BOOLEAN): HRESULT;
begin
    // Scrubbing is implemented as rate = 0.

    //FCritSec.Lock;

//    if (FRate)
//    {
//        return MF_E_INVALIDREQUEST;
//    }
    if not FbCanScrub then
       begin
       Result := MF_E_INVALIDREQUEST;
       exit;
       end;

    Result := S_OK;

    if (bScrub) then
       begin
        // Enter scrubbing mode. Cache the current rate.

        if (FRequest.fRate <> 0) then
           {
            m_fPrevRate = m_state.fRate;
           }

        Result := SetRate(0.0);
       end
    else
       begin
        // Leaving scrubbing mode. Restore the old rate.

        if (FRequest.fRate = 0) then
//            Result := SetRate(m_fPrevRate);
       end;
end;

function TMFPlayer.SendPlayerComand(cmd: TCommand): HRESULT;
begin
  case cmd of
    CmdNone: begin end;
    CmdStop: self.Stop;
    CmdStart: self.Play;
    CmdPause: self.Pause;
    CmdSeek: begin end;
  end;
end;

function TMFPlayer.SetInternalPosition(tPos: MFTIME): HRESULT;
var
    varStart:  MfPack.PropIdl.PROPVARIANT;
    hr: HRESULT;
begin
    FCritSec.Lock;
    try
        if (FpSession = nil) then
            begin
            Result := MF_E_INVALIDREQUEST;
            Exit;
            end;

        // preenche variável Propvariant
        varStart.vt := VT_I8;
        varStart.hVal.QuadPart := tPos;

        hr :=  FpSession.Start(GUID_NULL, varStart);

        if (SUCCEEDED(hr)) then
           begin
           // Store the pending state
           Fstate.hnsStart := tPos;
           FState.Command := CmdStart;
           FPending := True;

           UpdateCaption;
           end;

        Result := hr;
    finally
        FCritSec.unlock;
    end;
end;

function TMFPlayer.SetPosition(posMs: MFTIME): HRESULT;
var
  hr: HRESULT;

  vcanSeek: Boolean;
begin
    try
      FCritSec.Lock;

      if FPending then // Currently seeking or changing rates, so cache this request.
         begin
         FRequest.command := CmdSeek;
         FRequest.hnsStart := posMs;
         end
      else
          begin
          // Ajusta faixa de valores possíveis
          if posMs > FtDuration then posMs := FtDuration;
          if posMs <= 0 then posMs := 0;
          SetInternalPosition(PosMs);
          end;

    finally
      FCritSec.Unlock
    end;
end;

function TMFPlayer.SetRate(fRate: Single): HRESULT;
var
   bThin: Boolean;
   bSin: Single;
   hr: HRESULT;
label Done;
begin


    hr := S_OK;
    bThin := FALSE;

    FCritSec.Lock;
    try

    if (fRate = GetNominalRate()) then Goto Done;

    if (FpRateSupport = nil) then
       begin
       hr := MF_E_INVALIDREQUEST;
       Goto done;
       end;

    // Check if this rate is supported. Try non-thinned playback first,
    // then fall back to thinned playback.
    hr := FpRateSupport.IsRateSupported(FALSE, fRate, bSin);

    if (FAILED(hr)) then
       begin
        bThin := TRUE;
        hr := FpRateSupport.IsRateSupported(TRUE, fRate, bSin );
       end;

    // Unsupported rate.
    if (FAILED(hr)) then Goto done;

   	// Set the rate.
	  hr := FpRateControl.SetRate(bThin, fRate);
    if SUCCEEDED(hr) then FRequest.fRate := fRate;

Done:
    Result := hr;

    finally
       FCritSec.Unlock;
    end;
end;

procedure TMFPlayer.SetVolume(Value: Single);
var
   pVol: IMFAudioStreamVolume;
   nChannels: Cardinal;
   hr: HRESULT;
   i: integer;
begin
   if Value > 1 then Value := 1;
   if Value < 0 then Value := 0;

   hr := (FpSession as IMFGetService).GetService(MR_STREAM_VOLUME_SERVICE, IID_IMFAudioStreamVolume, pVol);
   if SUCCEEDED(hr) then
      begin
      hr := pVol.GetChannelCount(nChannels);
      for I := 0 to nChannels - 1 do
          pVol.SetChannelVolume(i, Value);
      end;
end;

function TMFPlayer.ShutDown: HRESULT;
begin
    Result := S_OK;

    // Close the session.
    Result := CloseSession();

    // fechar handle FHCloseEvent somente após CloseSession, pois aquele método
    // utiliza esse handle para aguardar o fechamento da seção.
    if FhCloseEvent <> 0 then
       begin
        CloseHandle(THandle(FhCloseEvent));
        FhCloseEvent := 0;
       end;
end;

function TMFPlayer.StepDown(value: Integer): HRESULT;
var
   mfPos: MFTIME;
   mfValue: MFTIME;
   pos: Cardinal;
begin
   mfValue := MiliSecToRefTime(Value);
   if FAILED(GetPosition(mfPos)) then Exit;
//   pos := RefTimeToMiliSec(mfPos);
//   if pos = 0 then
//     Inc(FCount);

   if mfpos + mfValue < FtDuration then
      Self.SetPosition(mfpos + mfValue)
   else
      Self.SetPosition(FtDuration);
end;

function TMFPlayer.StepUp(value: Integer): HRESULT;
var
   mfPos, mfValue: MFTIME;
   pos: Cardinal;
begin
   mfValue := MiliSecToRefTime(Value);
   if FAILED(GetPosition(mfPos)) then Exit;

   if mfPos - mfValue > 0 then
      Self.SetPosition(mfpos - mfValue)
   else
      Self.SetPosition(0);
end;

function TMFPlayer.Stop: HRESULT;
begin
   if Assigned(FpSession) then

   if FPending then
      FRequest.Command := CmdStop
   else
      begin
      Result := FpSession.Stop;
      Fstate.command := CmdStop;
      FPending := true;
      end;
end;

procedure TMFPlayer.TimerTimer(sender: TObject);
begin
  UpdateCaption;
end;

// Called after an operation completes.
// This method executes any cached requests.

procedure TMFPlayer.UpdateCaption;
var
   mfPos: MFTIME;
   pos, dur: cardinal;
begin

   if Assigned(FpSession) and Assigned(FpMediaSource) and IsWindow(FhwndVideo) then

   GetPosition(mfPos);
   pos := RefTimeToMiliSec(mfPos);
   dur := RefTimeToMiliSec(FtDuration);

   case Self.GetState of
   CmdOpenPending: SetWindowText(FhwndVideo, 'Abrindo mídia... ' +
                                           '"' + ExtractFileName(FFileName) + '"');
   CmdReady:   SetWindowText(FhwndVideo, 'Sessão criada.');
   CmdClosing: SetWindowText(FhwndVideo, 'Fechando sessão...');
   CmdStart: begin // está em reprodução normal
             SetWindowText(FhwndVideo,
                           'RefCount: ' + InttoStr(FRefCount) +
                           '. "' + ExtractFileName(FFileName) + '". ' +
                           FormatTime(pos) + '. Duração: ' + FormatTime(dur));
             end;
   CmdPause:  SetWindowText(FhwndVideo,'Em Pausa. Posição: ' + FormatTime(pos));
   CmdStop:   SetWindowText(FhwndVideo,'Parado.');
   CmdClosed: SetWindowText(FhwndVideo,'Sessão encerrada.');
   end;
end;

function TMFPlayer.UpdatePendingCommands(cmd: TCommand): HRESULT;
var
   hr: HRESULT;
begin
    hr := S_OK;

    try
      FCritSec.Lock;
      if (FPending) and (FState.Command = cmd) then
          begin
          FPending := FALSE;
          // The current pending command has completed.
          // Now look for seek requests.
          if not FPending then
              case FRequest.command of
                 CmdNone: ; // Nothing to do.
                 CmdStart: Play();
                 CmdPause: Pause();
                 CmdStop:  Stop();
                 CmdSeek:  SetInternalPosition(FRequest.hnsStart);
              end;
              FRequest.command := CmdNone;
          end;

    finally
        FCritSec.Unlock;
        Result := hr;
    end;
end;

function TMFPlayer._AddRef: Integer;
begin
 Result := AtomicIncrement(FRefCount);
 //Result := -1;
end;

function TMFPlayer._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
//  if (Result = 0) then
//    Destroy;
//  Result := -1;
end;


{ TMFCritSec }

constructor TMFCritSec.Create;
begin
   InitializeCriticalSection(FcriticalSection);
end;

destructor TMFCritSec.Destroy;
begin
   DeleteCriticalSection(FcriticalSection);
end;

procedure TMFCritSec.Lock;
begin
   EnterCriticalSection(FcriticalSection);
end;

procedure TMFCritSec.Unlock;
begin
   LeaveCriticalSection(FcriticalSection);
end;


initialization
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  if FAILED(MFStartup(MF_VERSION, 0)) then
    MessageBox(0, pChar('Este computador não tem suporte para o Media Foundation'),
               pChar('Erro!'), MB_ICONERROR);

finalization
  CoUninitialize;
  MFShutdown;
end.
