unit RDJ.Log;

interface

uses
  System.SysUtils;

function RDJLogFileName(): string;
procedure RDJLog(const ACategory: string; const AMessage: string);
procedure RDJLogFmt(const ACategory: string; const AFormat: string;
                    const AArgs: array of const);

implementation

uses
  Winapi.Windows,
  System.Classes,
  System.IOUtils;

var
  GLogLock: TRTLCriticalSection;
  GLogLockInitialized: Boolean = False;

const
  // Keep the diagnostic hooks in place, but do not produce log output during
  // normal operation. Set this to True temporarily when diagnostics are needed.
  RDJ_LOGGING_ENABLED = False;
  RDJ_LOG_MAX_BYTES = 25 * 1024 * 1024;


function RDJExistingFileSize(const AFileName: string): Int64;
var
  SearchRec: TSearchRec;

begin

  Result := -1;
  if FindFirst(AFileName,
               faAnyFile,
               SearchRec) = 0 then
    try
      Result := SearchRec.Size;
    finally
      System.SysUtils.FindClose(SearchRec);
    end;
end;


function RDJLogFileName(): string;
var
  BaseDir: string;

begin

  BaseDir := Trim(GetEnvironmentVariable('LOCALAPPDATA'));
  if BaseDir = '' then
    BaseDir := ExtractFilePath(ParamStr(0));

  Result := TPath.Combine(BaseDir,
                          'RDJ Pro\Logs\RDJPro.log');
end;


procedure RDJLog(const ACategory: string; const AMessage: string);
var
  FileName: string;
  LogDir: string;
  PreviousFileName: string;
  Line: string;
  Utf8: UTF8String;
  Stream: TFileStream;

begin

  if not RDJ_LOGGING_ENABLED then
    Exit;

  Line := Format('%s | PID=%d TID=%d | %-14s | %s%s',
                 [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
                  GetCurrentProcessId(),
                  GetCurrentThreadId(),
                  Copy(Trim(ACategory), 1, 14),
                  AMessage,
                  sLineBreak]);

  OutputDebugString(PChar(TrimRight(Line)));

  if not GLogLockInitialized then
    Exit;

  EnterCriticalSection(GLogLock);
  try
    try
      FileName := RDJLogFileName();
      LogDir := ExtractFilePath(FileName);
      if not DirectoryExists(LogDir) then
        ForceDirectories(LogDir);

      if FileExists(FileName) and
         (RDJExistingFileSize(FileName) >= RDJ_LOG_MAX_BYTES) then
        begin
          PreviousFileName := TPath.Combine(LogDir,
                                            'RDJPro.previous.log');
          if FileExists(PreviousFileName) then
            DeleteFileW(PWideChar(PreviousFileName));
          MoveFileW(PWideChar(FileName),
                    PWideChar(PreviousFileName));
        end;

      if FileExists(FileName) then
        Stream := TFileStream.Create(FileName,
                                     fmOpenReadWrite or fmShareDenyNone)
      else
        Stream := TFileStream.Create(FileName,
                                     fmCreate or fmShareDenyNone);

      try
        Stream.Seek(0,
                    soFromEnd);
        Utf8 := UTF8String(Line);
        if Length(Utf8) > 0 then
          Stream.WriteBuffer(Utf8[1],
                             Length(Utf8));
      finally
        Stream.Free;
      end;
    except
      // Logging must never prevent RDJ Pro from starting or shutting down.
    end;
  finally
    LeaveCriticalSection(GLogLock);
  end;
end;


procedure RDJLogFmt(const ACategory: string; const AFormat: string;
                    const AArgs: array of const);
begin

  if not RDJ_LOGGING_ENABLED then
    Exit;

  RDJLog(ACategory,
         Format(AFormat,
                AArgs));
end;


initialization
  InitializeCriticalSection(GLogLock);
  GLogLockInitialized := True;

finalization
  GLogLockInitialized := False;
  DeleteCriticalSection(GLogLock);

end.
