unit WinAPI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, windows, dynlibs;

const
  ES_SYSTEM_REQUIRED = DWORD($00000001);
  {$EXTERNALSYM ES_SYSTEM_REQUIRED}
  ES_DISPLAY_REQUIRED = DWORD($00000002);
  {$EXTERNALSYM ES_DISPLAY_REQUIRED}
  ES_USER_PRESENT = DWORD($00000004);
  {$EXTERNALSYM ES_USER_PRESENT}
  ES_CONTINUOUS = DWORD($80000000);
  {$EXTERNALSYM ES_CONTINUOUS}
  ES_AWAYMODE_REQUIRED = DWORD($00000040);
  {$EXTERNALSYM ES_AWAYMODE_REQUIRED}

type
  EXECUTION_STATE = DWORD;

function SetThreadExecutionState(esFlags: EXECUTION_STATE): EXECUTION_STATE; stdcall; external kernel32;

// only available for winvista and later
function ShutdownBlockReasonCreate(Handle: HWND; Msg: LPCWSTR): BOOL;
function ShutdownBlockReasonDestroy(Handle: HWND): BOOL;


implementation

type
  TShutdownBlockReasonCreate = function(Handle: HWND; Msg: LPCWSTR): BOOL; stdcall;
  TShutdownBlockReasonDestroy = function(Handle: HWND): BOOL; stdcall;

var
  user32LibHandle: TLibHandle;
  pShutdownBlockReasonCreate: TShutdownBlockReasonCreate;
  pShutdownBlockReasonDestroy: TShutdownBlockReasonDestroy;

function ShutdownBlockReasonCreate(Handle: HWND; Msg: LPCWSTR): BOOL;
begin
  if pShutdownBlockReasonCreate <> nil then
    Result := pShutdownBlockReasonCreate(Handle, Msg)
  else
    Result := False;
end;

function ShutdownBlockReasonDestroy(Handle: HWND): BOOL;
begin
  if pShutdownBlockReasonDestroy <> nil then
    Result := pShutdownBlockReasonDestroy(Handle)
  else
    Result := False;
end;

procedure doInit;
begin
  user32LibHandle := LoadLibrary(user32);
  if user32LibHandle <> NilHandle then
  begin
    Pointer(pShutdownBlockReasonCreate) := GetProcAddress(user32LibHandle, 'ShutdownBlockReasonCreate');
    Pointer(pShutdownBlockReasonDestroy) := GetProcAddress(user32LibHandle, 'ShutdownBlockReasonDestroy');
  end;
end;

procedure doFinal;
begin
  if user32LibHandle <> NilHandle then
    FreeLibrary(user32LibHandle);
end;

initialization
  doInit;

finalization
  doFinal;

end.
