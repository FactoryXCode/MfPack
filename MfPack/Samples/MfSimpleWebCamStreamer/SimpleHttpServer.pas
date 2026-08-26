// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - MediaFoundation
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: SimpleHttpServer.pas
// Kind: Pascal Unit
// Release date: 25-08-2026
// Language: ENU
//
// Revision Version: 4.0.0
// Description: Small HTTP server for MfSimpleWebCamStreamer.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Carmen (carmenh)
// Contributor(s): Tony Kalf (maXcomX), Carmen (carmenh)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 24/08/2026 All                 Moby release  SDK 10.0.28000.2705  (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 10 or higher.
//
// Related objects: -
// Related projects: MfPackX400
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.28000.2705
//
// Todo: -
//
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
unit SimpleHttpServer;

interface

uses

  {WinApi}
  WinApi.Windows,
  WinApi.WinError,
  WinApi.Winsock2,
  {System}
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.SyncObjs;


type

  TSimpleHttpGetBytesEvent = function(out AData: TBytes): Boolean of object;

  TSimpleHttpGetFragmentEvent = function(const ASequence: UInt64;
                                         out AData: TBytes): Boolean of object;

  TSimpleHttpGetStatusEvent = function(out AText: AnsiString): Boolean of object;


  TSimpleHttpServer = class;


  TSimpleHttpServerThread = class(TThread)
  private

    FOwner: TSimpleHttpServer;

  protected

    procedure Execute(); override;

  public

    constructor Create(AOwner: TSimpleHttpServer);
  end;


  TSimpleHttpServer = class
  private

    FSocket: TSocket;
    FThread: TSimpleHttpServerThread;
    FPort: Word;
    FRunning: Boolean;
    FDebugDropRequests: Boolean;
    FWinsockReady: Boolean;
    FLock: TCriticalSection;

    FOnGetInit: TSimpleHttpGetBytesEvent;
    FOnGetFragment: TSimpleHttpGetFragmentEvent;
    FOnGetStatus: TSimpleHttpGetStatusEvent;

    function EnsureWinsock(): Boolean;
    function OpenListenSocket(): Boolean;
    procedure CloseListenSocket();

    function SendAll(ASocket: TSocket;
                     const ABuffer;
                     ASize: Integer): Boolean;

    function SendResponse(ASocket: TSocket;
                          const AStatus: AnsiString;
                          const AContentType: AnsiString;
                          const ABody: TBytes): Boolean;

    function SendTextResponse(ASocket: TSocket;
                              const AStatus: AnsiString;
                              const AText: AnsiString): Boolean;

    function SendHtmlResponse(ASocket: TSocket;
                              const AHtml: string): Boolean;

    function BuildMsePlayerPage(): string;

    procedure HandleClient(ASocket: TSocket);
    procedure ServerLoop();

  public

    constructor Create();
    destructor Destroy(); override;

    function Start(const APort: Word): Boolean;
    procedure Stop();

    property Running: Boolean read FRunning;
    property Port: Word read FPort;

    property DebugDropRequests: Boolean read FDebugDropRequests
                                        write FDebugDropRequests;

    property OnGetInit: TSimpleHttpGetBytesEvent read FOnGetInit write FOnGetInit;
    property OnGetFragment: TSimpleHttpGetFragmentEvent read FOnGetFragment write FOnGetFragment;
    property OnGetStatus: TSimpleHttpGetStatusEvent read FOnGetStatus write FOnGetStatus;
  end;


implementation


const

  HTTP_REQUEST_BUFFER_SIZE = 4096;


// TSimpleHttpServerThread

constructor TSimpleHttpServerThread.Create(AOwner: TSimpleHttpServer);
begin

  inherited Create(True);

  FreeOnTerminate := False;
  FOwner := AOwner;
end;


procedure TSimpleHttpServerThread.Execute();
begin

  if Assigned(FOwner) then
    FOwner.ServerLoop();
end;


// TSimpleHttpServer

constructor TSimpleHttpServer.Create();
begin
  inherited Create();

  FSocket := INVALID_SOCKET;
  FThread := nil;
  FDebugDropRequests := False;
  FPort := 0;
  FRunning := False;
  FWinsockReady := False;

  FLock := TCriticalSection.Create();
end;


destructor TSimpleHttpServer.Destroy();
begin

  Stop();

  if FWinsockReady then
    begin
      WSACleanup();
      FWinsockReady := False;
    end;

  FreeAndNil(FLock);

  inherited Destroy();
end;


function TSimpleHttpServer.EnsureWinsock(): Boolean;
var
  WsaData: TWSAData;

begin

  if FWinsockReady then
    Exit(True);

  Result := WSAStartup($0202,
                       WsaData) = 0;

  FWinsockReady := Result;
end;


function TSimpleHttpServer.OpenListenSocket(): Boolean;
var
  Addr: TSockAddr;
  AddrIn: PSockAddrIn;
  Reuse: Integer;

begin

  Result := False;

  FSocket := socket(AF_INET,
                    SOCK_STREAM,
                    IPPROTO_TCP);

  if (FSocket = INVALID_SOCKET) then
    Exit;

  Reuse := 1;

  setsockopt(FSocket,
             SOL_SOCKET,
             SO_REUSEADDR,
             PAnsiChar(@Reuse),
             SizeOf(Reuse));

  FillChar(Addr,
           SizeOf(Addr),
           0);

  AddrIn := PSockAddrIn(@Addr);

  AddrIn^.sin_family := AF_INET;
  AddrIn^.sin_port := htons(FPort);
  AddrIn^.sin_addr.S_addr := htonl(INADDR_ANY);

  if (bind(FSocket,
          Addr,
          SizeOf(Addr)) = SOCKET_ERROR) then
    begin
      CloseListenSocket();
      Exit;
    end;

  if (listen(FSocket,
            SOMAXCONN) = SOCKET_ERROR) then
    begin
      CloseListenSocket();
      Exit;
    end;

  Result := True;
end;


procedure TSimpleHttpServer.CloseListenSocket();
var
  Socket: TSocket;

begin

  FLock.Enter();

  try
    Socket := FSocket;
    FSocket := INVALID_SOCKET;
  finally
    FLock.Leave();
  end;

  if (Socket <> INVALID_SOCKET) then
    begin
      shutdown(Socket,
               SD_BOTH);

      closesocket(Socket);
    end;
end;


function TSimpleHttpServer.Start(const APort: Word): Boolean;
begin

  if FRunning then
    Exit(True);

  Result := False;

  if not EnsureWinsock() then
    Exit;

  FPort := APort;

  if not OpenListenSocket() then
    Exit;

  FRunning := True;

  FThread := TSimpleHttpServerThread.Create(Self);
  FThread.Start();

  Result := True;
end;


procedure TSimpleHttpServer.Stop();
var
  Thread: TSimpleHttpServerThread;

begin

  if (not FRunning) and
     (not Assigned(FThread)) then
    Exit;

  FRunning := False;
  Thread := FThread;

  if Assigned(Thread) then
    Thread.Terminate();

  CloseListenSocket();

  if Assigned(Thread) then
    begin
      WaitForSingleObject(Thread.Handle,
                          INFINITE);

      FThread := nil;
      Thread.Free();
    end;
end;


function TSimpleHttpServer.SendAll(ASocket: TSocket;
                                   const ABuffer;
                                   ASize: Integer): Boolean;
var
  Buffer: PAnsiChar;
  BytesLeft: Integer;
  BytesSent: Integer;

begin

  Result := False;

  if (ASocket = INVALID_SOCKET) or
     (ASize <= 0) then
    Exit;

  Buffer := @ABuffer;
  BytesLeft := ASize;

  while BytesLeft > 0 do
    begin
      BytesSent := send(ASocket,
                        Buffer^,
                        BytesLeft,
                        0);

      if BytesSent = SOCKET_ERROR then
        Exit;

      Inc(Buffer,
          BytesSent);

      Dec(BytesLeft,
          BytesSent);
    end;

  Result := True;
end;


function TSimpleHttpServer.SendResponse(ASocket: TSocket;
                                        const AStatus: AnsiString;
                                        const AContentType: AnsiString;
                                        const ABody: TBytes): Boolean;
var
  Header: AnsiString;

begin

  Header := 'HTTP/1.1 ' + AStatus + #13#10 +
            'Content-Type: ' + AContentType + #13#10 +
            'Content-Length: ' + AnsiString(IntToStr(Length(ABody))) + #13#10 +
            'Cache-Control: no-store, no-cache, must-revalidate' + #13#10 +
            'Pragma: no-cache' + #13#10 +
            'Access-Control-Allow-Origin: *' + #13#10 +
            'Connection: close' + #13#10 +
            #13#10;

  Result := SendAll(ASocket,
                    PAnsiChar(Header)^,
                    Length(Header));

  if Result and (Length(ABody) > 0) then
    Result := SendAll(ASocket,
                      ABody[0],
                      Length(ABody));
end;


function TSimpleHttpServer.SendTextResponse(ASocket: TSocket;
                                            const AStatus: AnsiString;
                                            const AText: AnsiString): Boolean;
var
  Body: TBytes;

begin

  Body := TEncoding.ASCII.GetBytes(string(AText));

  Result := SendResponse(ASocket,
                         AStatus,
                         'text/plain; charset=us-ascii',
                         Body);
end;


function TSimpleHttpServer.SendHtmlResponse(ASocket: TSocket;
                                            const AHtml: string): Boolean;
var
  Body: TBytes;

begin

  Body := TEncoding.UTF8.GetBytes(AHtml);

  Result := SendResponse(ASocket,
                         '200 OK',
                         'text/html; charset=utf-8',
                         Body);
end;

// This is the GUI you see in your browser.
function TSimpleHttpServer.BuildMsePlayerPage(): string;
begin

  Result :=
    '<!doctype html>'#13#10 +
    '<html>'#13#10 +
    '<head>'#13#10 +
    '  <meta charset="utf-8">'#13#10 +
    '  <title>MfSimpleWebCamStreamer - MSE test</title>'#13#10 +
    '  <style>'#13#10 +
    '    body { font-family: Segoe UI, Arial, sans-serif; margin: 24px; }'#13#10 +
    '    video { width: 800px; max-width: 100%; background: #000; }'#13#10 +
    '    .diag { display: grid; grid-template-columns: repeat(4, minmax(140px, 1fr));'#13#10 +
    '            gap: 8px; margin: 12px 0; max-width: 800px; }'#13#10 +
    '    .diag div { border: 1px solid #bbb; padding: 8px; background: #f7f7f7; }'#13#10 +
    '    .diag b { display: block; font-size: 12px; color: #555; margin-bottom: 3px; }'#13#10 +
    '    pre { height: 280px; overflow: auto; border: 1px solid #bbb; padding: 8px; }'#13#10 +
    '  </style>'#13#10 +
    '</head>'#13#10 +
    '<body>'#13#10 +
    '  <h2>MfSimpleWebCamStreamer</h2>'#13#10 +
    '  <video id="video" controls autoplay muted></video>'#13#10 +
    '  <p><button id="start">Start MSE</button> <span id="state">idle</span></p>'#13#10 +
    '  <p>Connected to: <span id="origin"></span></p>'#13#10 +
    '  <div class="diag">'#13#10 +
    '    <div><b>Generation</b><span id="dGeneration">0</span></div>'#13#10 +
    '    <div><b>Fragment window</b><span id="dWindow">0-0</span></div>'#13#10 +
    '    <div><b>Next sequence</b><span id="dSequence">0</span></div>'#13#10 +
    '    <div><b>Buffered</b><span id="dBuffered">0.000 s</span></div>'#13#10 +
    '    <div><b>Live drift</b><span id="dDrift">0.000 s</span></div>'#13#10 +
    '    <div><b>Retries</b><span id="dRetries">0</span></div>'#13#10 +
    '    <div><b>Approx bitrate</b><span id="dBitrate">0 kbps</span></div>'#13#10 +
    '    <div><b>Health</b><span id="dHealth">idle</span></div>'#13#10 +
    '  </div>'#13#10 +
    '  <pre id="log"></pre>'#13#10 +
    '<script>'#13#10 +
    'const MIME = ''video/mp4; codecs="avc1.42C01F, mp4a.40.2"'';'#13#10 +
    'const video = document.getElementById(''video'');'#13#10 +
    'const logBox = document.getElementById(''log'');'#13#10 +
    'const stateBox = document.getElementById(''state'');'#13#10 +
    'document.getElementById(''origin'').textContent = window.location.origin;'#13#10 +
    'const dGeneration = document.getElementById(''dGeneration'');'#13#10 +
    'const dWindow = document.getElementById(''dWindow'');'#13#10 +
    'const dSequence = document.getElementById(''dSequence'');'#13#10 +
    'const dBuffered = document.getElementById(''dBuffered'');'#13#10 +
    'const dDrift = document.getElementById(''dDrift'');'#13#10 +
    'const dRetries = document.getElementById(''dRetries'');'#13#10 +
    'const dBitrate = document.getElementById(''dBitrate'');'#13#10 +
    'const dHealth = document.getElementById(''dHealth'');'#13#10 +
    'let mediaSource = null;'#13#10 +
    'let sourceBuffer = null;'#13#10 +
    'let nextSequence = 0;'#13#10 +
    'let running = false;'#13#10 +
    'let busy = false;'#13#10 +
    'let playbackStarted = false;'#13#10 +
    'let playRequested = false;'#13#10 +
    'let startupReadyLogged = false;'#13#10 +
    'let playbackStartMs = 0;'#13#10 +
    'let sessionId = 0;'#13#10 +
    'let diagFirst = 0;'#13#10 +
    'let diagLast = 0;'#13#10 +
    'let bitrateBytes = 0;'#13#10 +
    'let bitrateStartedMs = performance.now();'#13#10 +
    'let consecutiveFetchFailures = 0;'#13#10 +
    'let streamGeneration = 0;'#13#10 +
    'let autoRecoveryArmed = false;'#13#10 +
    'let recoveryBusy = false;'#13#10 +
    ''#13#10 +
    'const START_BUFFER_SECONDS = 2.0;'#13#10 +
    'const LIVE_EDGE_TARGET_SECONDS = 2.0;'#13#10 +
    'const LIVE_EDGE_MAX_DRIFT_SECONDS = 4.0;'#13#10 +
    'const LIVE_EDGE_GRACE_MS = 5000;'#13#10 +
    'const KEEP_BEHIND_SECONDS = 30.0;'#13#10 +
    'const PRUNE_MARGIN_SECONDS = 5.0;'#13#10 +
    'const MAX_FETCH_FAILURES = 5;'#13#10 +
    ''#13#10 +
    'function log(s) {'#13#10 +
    '  const t = new Date().toLocaleTimeString();'#13#10 +
    '  logBox.textContent += ''['' + t + ''] '' + s + ''\n'';'#13#10 +
    '  logBox.scrollTop = logBox.scrollHeight;'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function formatKbps(bytes, elapsedMs) {'#13#10 +
    '  if (elapsedMs <= 0) return ''0 kbps'';'#13#10 +
    '  return ((bytes * 8.0) / elapsedMs).toFixed(1) + '' kbps'';'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function updateDiagnostics() {'#13#10 +
    '  dGeneration.textContent = String(streamGeneration || 0);'#13#10 +
    '  dWindow.textContent = String(diagFirst) + ''-'' + String(diagLast);'#13#10 +
    '  dSequence.textContent = String(nextSequence || 0);'#13#10 +
    ''#13#10 +
    '  let buffered = 0;'#13#10 +
    '  let drift = 0;'#13#10 +
    '  if (sourceBuffer && sourceBuffer.buffered.length > 0) {'#13#10 +
    '    const bs = getBufferedStart();'#13#10 +
    '    const be = getBufferedEnd();'#13#10 +
    '    const inRange = video.currentTime >= bs && video.currentTime <= be;'#13#10 +
    '    buffered = inRange ? be - video.currentTime : be - bs;'#13#10 +
    '    drift = inRange ? be - video.currentTime : buffered;'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  dBuffered.textContent = buffered.toFixed(3) + '' s'';'#13#10 +
    '  dDrift.textContent = drift.toFixed(3) + '' s'';'#13#10 +
    '  dRetries.textContent = String(consecutiveFetchFailures);'#13#10 +
    '  dBitrate.textContent = formatKbps(bitrateBytes,'#13#10 +
    '                                      performance.now() - bitrateStartedMs);'#13#10 +
    '  dHealth.textContent = stateBox.textContent;'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function parseStatus(text) {'#13#10 +
    '  const r = {};'#13#10 +
    '  text.split(/\r?\n/).forEach(line => {'#13#10 +
    '    const p = line.indexOf(''='');'#13#10 +
    '    if (p > 0) r[line.substring(0, p)] = line.substring(p + 1);'#13#10 +
    '  });'#13#10 +
    '  return r;'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'async function getBytes(url) {'#13#10 +
    '  const r = await fetch(url, { cache: ''no-store'' });'#13#10 +
    '  if (!r.ok) throw new Error(url + '' HTTP '' + r.status);'#13#10 +
    '  const bytes = new Uint8Array(await r.arrayBuffer());'#13#10 +
    '  bitrateBytes += bytes.byteLength;'#13#10 +
    '  return bytes;'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function appendBytes(bytes, label) {'#13#10 +
    '  return new Promise((resolve, reject) => {'#13#10 +
    '    const done = () => {'#13#10 +
    '      sourceBuffer.removeEventListener(''updateend'', done);'#13#10 +
    '      sourceBuffer.removeEventListener(''error'', fail);'#13#10 +
    '      log(''Appended '' + label + '', bytes='' + bytes.byteLength);'#13#10 +
    '      resolve();'#13#10 +
    '    };'#13#10 +
    '    const fail = () => {'#13#10 +
    '      sourceBuffer.removeEventListener(''updateend'', done);'#13#10 +
    '      sourceBuffer.removeEventListener(''error'', fail);'#13#10 +
    '      reject(new Error(''SourceBuffer error while appending '' + label));'#13#10 +
    '    };'#13#10 +
    '    sourceBuffer.addEventListener(''updateend'', done);'#13#10 +
    '    sourceBuffer.addEventListener(''error'', fail);'#13#10 +
    '    try {'#13#10 +
    '      sourceBuffer.appendBuffer(bytes);'#13#10 +
    '    } catch (e) {'#13#10 +
    '      sourceBuffer.removeEventListener(''updateend'', done);'#13#10 +
    '      sourceBuffer.removeEventListener(''error'', fail);'#13#10 +
    '      reject(e);'#13#10 +
    '    }'#13#10 +
    '  });'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function getBufferedEnd() {'#13#10 +
    '  if (!sourceBuffer || sourceBuffer.buffered.length === 0) return 0;'#13#10 +
    '  return sourceBuffer.buffered.end(sourceBuffer.buffered.length - 1);'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function getBufferedStart() {'#13#10 +
    '  if (!sourceBuffer || sourceBuffer.buffered.length === 0) return 0;'#13#10 +
    '  return sourceBuffer.buffered.start(0);'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function pruneOldBuffer() {'#13#10 +
    '  if (!sourceBuffer || sourceBuffer.updating ||'#13#10 +
    '      sourceBuffer.buffered.length === 0) return;'#13#10 +
    ''#13#10 +
    '  const bufferedStart = sourceBuffer.buffered.start(0);'#13#10 +
    '  const removeTo = video.currentTime - KEEP_BEHIND_SECONDS;'#13#10 +
    ''#13#10 +
    '  if (removeTo > bufferedStart + PRUNE_MARGIN_SECONDS) {'#13#10 +
    '    log(''Prune buffer: '' + bufferedStart.toFixed(3) +'#13#10 +
    '        '' -> '' + removeTo.toFixed(3));'#13#10 +
    '    sourceBuffer.remove(bufferedStart, removeTo);'#13#10 +
    '  }'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function servicePlayback() {'#13#10 +
    '  if (!sourceBuffer || sourceBuffer.buffered.length === 0) return;'#13#10 +
    ''#13#10 +
    '  const bufferedStart = getBufferedStart();'#13#10 +
    '  const bufferedEnd = getBufferedEnd();'#13#10 +
    '  const currentInBuffer ='#13#10 +
    '    video.currentTime >= bufferedStart &&'#13#10 +
    '    video.currentTime <= bufferedEnd;'#13#10 +
    '  const bufferedAhead = currentInBuffer'#13#10 +
    '    ? bufferedEnd - video.currentTime'#13#10 +
    '    : bufferedEnd - bufferedStart;'#13#10 +
    ''#13#10 +
    '  if (!playbackStarted && !playRequested &&'#13#10 +
    '      bufferedAhead < START_BUFFER_SECONDS) {'#13#10 +
    '    stateBox.textContent = ''buffering'';'#13#10 +
    '    return;'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  if (!playbackStarted && !playRequested &&'#13#10 +
    '      bufferedAhead >= START_BUFFER_SECONDS) {'#13#10 +
    '    if (!startupReadyLogged) {'#13#10 +
    '      startupReadyLogged = true;'#13#10 +
    '      log(''Startup buffer ready: '' + bufferedAhead.toFixed(3) + '' s'');'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    const startTarget = Math.max(bufferedStart,'#13#10 +
    '                                 bufferedEnd - LIVE_EDGE_TARGET_SECONDS);'#13#10 +
    ''#13#10 +
    '    if (video.currentTime < bufferedStart ||'#13#10 +
    '        video.currentTime > bufferedEnd) {'#13#10 +
    '      log(''Initial seek: '' + video.currentTime.toFixed(3) +'#13#10 +
    '          '' -> '' + startTarget.toFixed(3) +'#13#10 +
    '          '', buffered='' + bufferedStart.toFixed(3) +'#13#10 +
    '          ''-'' + bufferedEnd.toFixed(3));'#13#10 +
    '      video.currentTime = startTarget;'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    playRequested = true;'#13#10 +
    ''#13#10 +
    '    video.play().then(() => {'#13#10 +
    '      playbackStarted = true;'#13#10 +
    '      playRequested = false;'#13#10 +
    '      playbackStartMs = performance.now();'#13#10 +
    '      stateBox.textContent = ''playing'';'#13#10 +
    '      log(''Playback started, bufferedAhead='' +'#13#10 +
    '          (getBufferedEnd() - video.currentTime).toFixed(3) + '' s'');'#13#10 +
    '    }).catch((e) => {'#13#10 +
    '      playRequested = false;'#13#10 +
    '      log(''play() deferred: '' + (e.message || e));'#13#10 +
    '    });'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  if (playbackStarted) {'#13#10 +
    '    if (running && stateBox.textContent === ''retrying'')'#13#10 +
    '      stateBox.textContent = ''playing'';'#13#10 +
    '    const liveDrift = bufferedEnd - video.currentTime;'#13#10 +
    '    const graceElapsed ='#13#10 +
    '      playbackStartMs > 0 &&'#13#10 +
    '      (performance.now() - playbackStartMs) >= LIVE_EDGE_GRACE_MS;'#13#10 +
    ''#13#10 +
    '    if (graceElapsed &&'#13#10 +
    '        liveDrift > LIVE_EDGE_MAX_DRIFT_SECONDS) {'#13#10 +
    '      const target = Math.max(0, bufferedEnd - LIVE_EDGE_TARGET_SECONDS);'#13#10 +
    '      log(''Live-edge correction: '' +'#13#10 +
    '          video.currentTime.toFixed(3) + '' -> '' + target.toFixed(3) +'#13#10 +
    '          '', drift='' + liveDrift.toFixed(3) + '' s'');'#13#10 +
    '      video.currentTime = target;'#13#10 +
    '    }'#13#10 +
    '  }'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function handleFetchFailure(e) {'#13#10 +
    '  consecutiveFetchFailures++;'#13#10 +
    ''#13#10 +
    '  if (consecutiveFetchFailures < MAX_FETCH_FAILURES) {'#13#10 +
    '    stateBox.textContent = ''retrying'';'#13#10 +
    '    log(''Fetch retry '' + consecutiveFetchFailures +'#13#10 +
    '        ''/'' + MAX_FETCH_FAILURES + '': '' + (e.message || e));'#13#10 +
    '    return true;'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  running = false;'#13#10 +
    '  autoRecoveryArmed = true;'#13#10 +
    '  stateBox.textContent = ''waiting for server'';'#13#10 +
    '  log(''Stream unavailable after '' + consecutiveFetchFailures +'#13#10 +
    '      '' consecutive fetch failures.'');'#13#10 +
    '  log(''Waiting for HTTP recovery or a new stream generation...'');'#13#10 +
    '  return false;'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'async function pump() {'#13#10 +
    '  if (!running || busy || !sourceBuffer || sourceBuffer.updating) return;'#13#10 +
    '  busy = true;'#13#10 +
    '  try {'#13#10 +
    '    const sr = await fetch(''/status'', { cache: ''no-store'' });'#13#10 +
    '    if (!sr.ok) throw new Error(''/status HTTP '' + sr.status);'#13#10 +
    '    const st = parseStatus(await sr.text());'#13#10 +
    '    consecutiveFetchFailures = 0;'#13#10 +
    '    const generation = Number(st.generation || 0);'#13#10 +
    '    const first = Number(st.first || 0);'#13#10 +
    '    const last = Number(st.last || 0);'#13#10 +
    '    diagFirst = first;'#13#10 +
    '    diagLast = last;'#13#10 +
    ''#13#10 +
    '    if (streamGeneration > 0 &&'#13#10 +
    '        generation > 0 &&'#13#10 +
    '        generation !== streamGeneration) {'#13#10 +
    '      log(''New stream generation detected: '' +'#13#10 +
    '          streamGeneration + '' -> '' + generation);'#13#10 +
    '      autoRecoveryArmed = true;'#13#10 +
    '      running = false;'#13#10 +
    '      stateBox.textContent = ''waiting for server'';'#13#10 +
    '      return;'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    if (nextSequence === 0 && last > 0) {'#13#10 +
    '      nextSequence = Math.max(first, last - 3);'#13#10 +
    '      log(''Starting at fragment '' + nextSequence + '', window='' + first + ''-'' + last);'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    if (nextSequence > 0 && nextSequence < first) {'#13#10 +
    '      log(''Fell behind. Jump '' + nextSequence + '' -> '' + first);'#13#10 +
    '      nextSequence = first;'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    if (nextSequence > 0 && nextSequence <= last) {'#13#10 +
    '      const seq = nextSequence;'#13#10 +
    '      const bytes = await getBytes(''/fragment/'' + seq + ''.m4s'');'#13#10 +
    '      consecutiveFetchFailures = 0;'#13#10 +
    '      await appendBytes(bytes, ''fragment '' + seq);'#13#10 +
    '      nextSequence = seq + 1;'#13#10 +
    '      servicePlayback();'#13#10 +
    '      pruneOldBuffer();'#13#10 +
    '    }'#13#10 +
    '  } catch (e) {'#13#10 +
    '    if (e instanceof TypeError ||'#13#10 +
    '        String(e.message || '''').indexOf(''HTTP '') >= 0) {'#13#10 +
    '      handleFetchFailure(e);'#13#10 +
    '    } else {'#13#10 +
    '      running = false;'#13#10 +
    '      stateBox.textContent = ''error'';'#13#10 +
    '      log(''ERROR: '' + (e.stack || e.message || e));'#13#10 +
    '    }'#13#10 +
    '  } finally {'#13#10 +
    '    busy = false;'#13#10 +
    '  }'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'function resetMseSession() {'#13#10 +
    '  running = false;'#13#10 +
    '  busy = false;'#13#10 +
    '  playbackStarted = false;'#13#10 +
    '  playRequested = false;'#13#10 +
    '  startupReadyLogged = false;'#13#10 +
    '  playbackStartMs = 0;'#13#10 +
    '  nextSequence = 0;'#13#10 +
    '  consecutiveFetchFailures = 0;'#13#10 +
    '  diagFirst = 0;'#13#10 +
    '  diagLast = 0;'#13#10 +
    '  bitrateBytes = 0;'#13#10 +
    '  bitrateStartedMs = performance.now();'#13#10 +
    ''#13#10 +
    '  if (video) {'#13#10 +
    '    try { video.pause(); } catch (e) {}'#13#10 +
    '    video.removeAttribute(''src'');'#13#10 +
    '    video.load();'#13#10 +
    '    try { video.currentTime = 0; } catch (e) {}'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  sourceBuffer = null;'#13#10 +
    '  mediaSource = null;'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'async function startMse(autoRestart) {'#13#10 +
    '  sessionId++;'#13#10 +
    '  const mySession = sessionId;'#13#10 +
    ''#13#10 +
    '  resetMseSession();'#13#10 +
    '  if (!autoRestart)'#13#10 +
    '    logBox.textContent = '''';'#13#10 +
    '  else'#13#10 +
    '    log(''Rebuilding MSE session for restarted capture.'');'#13#10 +
    '  stateBox.textContent = ''connecting'';'#13#10 +
    ''#13#10 +
    '  if (!window.MediaSource) {'#13#10 +
    '    log(''ERROR: MediaSource is not available.'');'#13#10 +
    '    return;'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  log(''MIME: '' + MIME);'#13#10 +
    '  log(''isTypeSupported='' + MediaSource.isTypeSupported(MIME));'#13#10 +
    ''#13#10 +
    '  try {'#13#10 +
    '    const sr = await fetch(''/status'', { cache: ''no-store'' });'#13#10 +
    '    if (!sr.ok) throw new Error(''/status HTTP '' + sr.status);'#13#10 +
    '    const st = parseStatus(await sr.text());'#13#10 +
    '    const generation = Number(st.generation || 0);'#13#10 +
    '    const first = Number(st.first || 0);'#13#10 +
    '    const last = Number(st.last || 0);'#13#10 +
    '    diagFirst = first;'#13#10 +
    '    diagLast = last;'#13#10 +
    '    const initBytes = Number(st.initBytes || 0);'#13#10 +
    '    const windowCount = Number(st.windowCount || 0);'#13#10 +
    ''#13#10 +
    '    if (last <= 0 || initBytes <= 0 ||'#13#10 +
    '        (autoRestart && windowCount < 2)) {'#13#10 +
    '      stateBox.textContent = ''waiting for media'';'#13#10 +
    '      if (!autoRestart)'#13#10 +
    '        log(''No media fragments available yet.'');'#13#10 +
    '      return;'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    streamGeneration = generation;'#13#10 +
    '    autoRecoveryArmed = true;'#13#10 +
    '    nextSequence = Math.max(first, last - 3);'#13#10 +
    '    log(''Generation='' + streamGeneration +'#13#10 +
    '        '', window='' + first + ''-'' + last +'#13#10 +
    '        '', starting at '' + nextSequence);'#13#10 +
    '  } catch (e) {'#13#10 +
    '    if (autoRestart) {'#13#10 +
    '      stateBox.textContent = ''waiting for restart'';'#13#10 +
    '      return;'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    stateBox.textContent = ''error'';'#13#10 +
    '    log(''ERROR: '' + (e.stack || e.message || e));'#13#10 +
    '    return;'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  if (mySession !== sessionId) return;'#13#10 +
    ''#13#10 +
    '  mediaSource = new MediaSource();'#13#10 +
    '  video.src = URL.createObjectURL(mediaSource);'#13#10 +
    ''#13#10 +
    '  mediaSource.addEventListener(''sourceopen'', async () => {'#13#10 +
    '    if (mySession !== sessionId) return;'#13#10 +
    ''#13#10 +
    '    try {'#13#10 +
    '      sourceBuffer = mediaSource.addSourceBuffer(MIME);'#13#10 +
    '      sourceBuffer.mode = ''segments'';'#13#10 +
    '      const init = await getBytes(''/init.mp4'');'#13#10 +
    '      await appendBytes(init, ''init.mp4'');'#13#10 +
    '      running = true;'#13#10 +
    '      stateBox.textContent = ''running'';'#13#10 +
    '      log(''MSE started.'');'#13#10 +
    '    } catch (e) {'#13#10 +
    '      stateBox.textContent = ''error'';'#13#10 +
    '      log(''ERROR: '' + (e.stack || e.message || e));'#13#10 +
    '    }'#13#10 +
    '  }, { once: true });'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'video.addEventListener(''playing'', () => {'#13#10 +
    '  stateBox.textContent = ''playing'';'#13#10 +
    '});'#13#10 +
    ''#13#10 +
    'async function watchForRestart() {'#13#10 +
    '  if (!autoRecoveryArmed || running || recoveryBusy) return;'#13#10 +
    '  recoveryBusy = true;'#13#10 +
    ''#13#10 +
    '  try {'#13#10 +
    '    const sr = await fetch(''/status'', { cache: ''no-store'' });'#13#10 +
    '    if (!sr.ok) return;'#13#10 +
    '    const st = parseStatus(await sr.text());'#13#10 +
    '    const generation = Number(st.generation || 0);'#13#10 +
    '    const first = Number(st.first || 0);'#13#10 +
    '    const last = Number(st.last || 0);'#13#10 +
    '    const initBytes = Number(st.initBytes || 0);'#13#10 +
    '    const windowCount = Number(st.windowCount || 0);'#13#10 +
    ''#13#10 +
    '    diagFirst = first;'#13#10 +
    '    diagLast = last;'#13#10 +
    ''#13#10 +
    '    if (generation > 0 &&'#13#10 +
    '        generation === streamGeneration &&'#13#10 +
    '        initBytes > 0 &&'#13#10 +
    '        last > 0) {'#13#10 +
    '      if (nextSequence > 0 && nextSequence < first) {'#13#10 +
    '        log(''HTTP recovered, fell behind. Jump '' +'#13#10 +
    '            nextSequence + '' -> '' + first);'#13#10 +
    '        nextSequence = first;'#13#10 +
    '      } else {'#13#10 +
    '        log(''HTTP recovered on generation '' + generation + ''.'');'#13#10 +
    '      }'#13#10 +
    ''#13#10 +
    '      consecutiveFetchFailures = 0;'#13#10 +
    '      running = true;'#13#10 +
    '      stateBox.textContent = playbackStarted ? ''playing'' : ''running'';'#13#10 +
    '      return;'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    if (generation > 0 &&'#13#10 +
    '        generation !== streamGeneration &&'#13#10 +
    '        initBytes > 0 &&'#13#10 +
    '        windowCount >= 2 &&'#13#10 +
    '        last >= first) {'#13#10 +
    '      log(''Restart detected: generation '' +'#13#10 +
    '          streamGeneration + '' -> '' + generation);'#13#10 +
    '      await startMse(true);'#13#10 +
    '    }'#13#10 +
    '  } catch (e) {'#13#10 +
    '    // Expected while the Delphi HTTP server is unavailable.'#13#10 +
    '  } finally {'#13#10 +
    '    recoveryBusy = false;'#13#10 +
    '  }'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'video.addEventListener(''waiting'', () => {'#13#10 +
    '  if (running && playbackStarted)'#13#10 +
    '    log(''Video waiting for more buffered media.'');'#13#10 +
    '});'#13#10 +
    ''#13#10 +
    'video.addEventListener(''error'', () => {'#13#10 +
    '  const err = video.error;'#13#10 +
    '  if (err) log(''VIDEO ERROR code='' + err.code + '', message='' + (err.message || ''''));'#13#10 +
    '});'#13#10 +
    ''#13#10 +
    'document.getElementById(''start'').addEventListener(''click'', () => startMse(false));'#13#10 +
    'setInterval(pump, 250);'#13#10 +
    'setInterval(watchForRestart, 500);'#13#10 +
    'setInterval(updateDiagnostics, 250);'#13#10 +
    '</script>'#13#10 +
    '</body>'#13#10 +
    '</html>'#13#10;
end;


procedure TSimpleHttpServer.HandleClient(ASocket: TSocket);
var
  Buffer: array[0..HTTP_REQUEST_BUFFER_SIZE - 1] of AnsiChar;
  BytesReceived: Integer;
  Request: AnsiString;
  FirstLine: AnsiString;
  Path: AnsiString;
  StatusText: AnsiString;
  FirstLineEnd: Integer;
  Space1: Integer;
  Space2: Integer;
  SequenceText: string;
  SequenceValue: UInt64;
  Data: TBytes;

begin

  // Milestone 11 localhost retry test hook.
  // Drop the accepted request without sending an HTTP response. Capture,
  // encoding and fragment production continue normally.
  if FDebugDropRequests then
    Exit;


  if (ASocket = INVALID_SOCKET) then
    Exit;

  FillChar(Buffer,
           SizeOf(Buffer),
           0);

  BytesReceived := recv(ASocket,
                        Buffer,
                        SizeOf(Buffer),
                        0);

  if BytesReceived <= 0 then
    Exit;

  SetString(Request,
            PAnsiChar(@Buffer[0]),
            BytesReceived);

  FirstLineEnd := Pos(#13#10,
                      string(Request));

  if (FirstLineEnd > 0) then
    FirstLine := Copy(Request,
                      1,
                      FirstLineEnd - 1)
  else
    FirstLine := Request;

  Space1 := Pos(' ',
                string(FirstLine));

  if Space1 <= 0 then
    begin
      SendTextResponse(ASocket,
                       '400 Bad Request',
                       'Bad Request');
      Exit;
    end;

  Space2 := PosEx(' ',
                  string(FirstLine),
                  Space1 + 1);

  if (Space2 <= Space1) then
    begin
      SendTextResponse(ASocket,
                       '400 Bad Request',
                       'Bad Request');
      Exit;
    end;

  Path := Copy(FirstLine,
               Space1 + 1,
               Space2 - Space1 - 1);

  if (Path = '/') or
     (Path = '/player.html') then
    begin
      SendHtmlResponse(ASocket,
                       BuildMsePlayerPage());
      Exit;
    end;

  if (Path = '/init.mp4') then
    begin
      Data := nil;

      if Assigned(FOnGetInit) and
         FOnGetInit(Data) and
         (Length(Data) > 0) then
        SendResponse(ASocket,
                     '200 OK',
                     'video/mp4',
                     Data)
      else
        SendTextResponse(ASocket,
                         '503 Service Unavailable',
                         'Initialization segment not ready');

      Exit;
    end;

  if (Pos('/fragment/',
         string(Path)) = 1) then
    begin
      SequenceText := Copy(string(Path),
                           Length('/fragment/') + 1,
                           MaxInt);

      if EndsText('.m4s',
                  SequenceText) then
        Delete(SequenceText,
               Length(SequenceText) - Length('.m4s') + 1,
               Length('.m4s'));

      if not TryStrToUInt64(SequenceText,
                            SequenceValue) then
        begin
          SendTextResponse(ASocket,
                           '400 Bad Request',
                           'Invalid fragment sequence');
          Exit;
        end;

      Data := nil;

      if Assigned(FOnGetFragment) and
         FOnGetFragment(SequenceValue,
                        Data) and
         (Length(Data) > 0) then
        SendResponse(ASocket,
                     '200 OK',
                     'video/iso.segment',
                     Data)
      else
        SendTextResponse(ASocket,
                         '404 Not Found',
                         'Fragment not available');

      Exit;
    end;

  if (Path = '/status') then
    begin
      StatusText := 'MfSimpleWebCamStreamer';

      if Assigned(FOnGetStatus) then
        FOnGetStatus(StatusText);

      SendTextResponse(ASocket,
                       '200 OK',
                       StatusText);

      Exit;
    end;

  SendTextResponse(ASocket,
                   '404 Not Found',
                   'Not Found');
end;


procedure TSimpleHttpServer.ServerLoop();
var
  ClientSocket: TSocket;
  ClientAddr: TSockAddr;
  ClientAddrLength: Integer;
  ListenSocket: TSocket;

begin

  while not TThread.CurrentThread.CheckTerminated() do
    begin
      FLock.Enter();

      try
        ListenSocket := FSocket;
      finally
        FLock.Leave();
      end;

      if (ListenSocket = INVALID_SOCKET) then
        Break;

      FillChar(ClientAddr,
               SizeOf(ClientAddr),
               0);

      ClientAddrLength := SizeOf(ClientAddr);

      ClientSocket := accept(ListenSocket,
                             @ClientAddr,
                             @ClientAddrLength);

      if (ClientSocket = INVALID_SOCKET) then
        begin
          if TThread.CurrentThread.CheckTerminated() or
             (not FRunning) then
            Break;

          Continue;
        end;

      try
        HandleClient(ClientSocket);
      finally
        shutdown(ClientSocket,
                 SD_BOTH);

        closesocket(ClientSocket);
      end;
    end;
end;


end.
