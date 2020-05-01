unit InstrumentCOMinit;

interface

uses
  System.SysUtils, WinApi.Windows, System.Win.ComObj, WinApi.ActiveX;

threadvar
  COMinitCount: Integer;

implementation

function CoInitialize(pvReserved: Pointer): HResult; stdcall; external 'ole32.dll';
function CoInitializeEx(pvReserved: Pointer; coInit: Longint): HResult; stdcall; external 'ole32.dll';
procedure CoUninitialize; stdcall; external 'ole32.dll';

function InstrumentedCoInitialize(pvReserved: Pointer): HResult; stdcall;
begin
  Result := CoInitialize(pvReserved);
  if Succeeded(Result) then
    inc(COMinitCount);
end;

function InstrumentedCoInitializeEx(pvReserved: Pointer; coInit: Longint): HResult; stdcall;
begin
  Result := CoInitializeEx(pvReserved, coInit);
  if Succeeded(Result) then
    inc(COMinitCount);
end;

procedure InstrumentedCoUninitialize; stdcall;
begin
  CoUninitialize;
  dec(COMinitCount);
end;

procedure Fail;
begin
  raise EAssertionFailed.Create('Fixup failed.');
end;

procedure PatchCode(Address: Pointer; const NewCode; Size: Integer);
var
  OldProtect: DWORD;
begin
  if not VirtualProtect(Address, Size, PAGE_EXECUTE_READWRITE, OldProtect) then begin
    Fail;
  end;
  Move(NewCode, Address^, Size);
  FlushInstructionCache(GetCurrentProcess, nil, 0);
  if not VirtualProtect(Address, Size, OldProtect, @OldProtect) then begin
    Fail;
  end;
end;

type
  PInstruction = ^TInstruction;
  TInstruction = packed record
    Opcode: Byte;
    Offset: Integer;
  end;

procedure RedirectProcedure(OldAddress, NewAddress: Pointer);
var
  NewCode: TInstruction;
begin
  NewCode.Opcode := $E9;//jump relative
  NewCode.Offset := NativeInt(NewAddress)-NativeInt(OldAddress)-SizeOf(NewCode);
  PatchCode(OldAddress, NewCode, SizeOf(NewCode));
end;

initialization
  RedirectProcedure(@WinApi.ActiveX.CoInitialize, @InstrumentedCoInitialize);
  RedirectProcedure(@WinApi.ActiveX.CoInitializeEx, @InstrumentedCoInitializeEx);
  RedirectProcedure(@WinApi.ActiveX.CoUninitialize, @InstrumentedCoUninitialize);
  System.Win.ComObj.CoInitializeEx := InstrumentedCoInitializeEx;

end.
