{ SimpleException Class

  Copyright (C) 2014-2016 Nur Cholif

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit SimpleException;

{$mode objfpc}{$H+}
{$define MULTILOG}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, SimpleExceptionForm
  {$IFDEF WINDOWS}
  , Windows, win32proc
  {$ENDIF}
  {$IFDEF LINUX}
  , elfreader
  {$ENDIF}
  {$IF DEFINED(DARWIN) OR DEFINED(MACOS)}
  , machoreader
  {$ENDIF}
  , fileinfo
  {$IFDEF MULTILOG}
  , MultiLog
  {$ENDIF}
  , LCLVersion;

type

  { TSimpleException }

  TSimpleException = class
  private
    FLogFileHandle: TextFile;
    FLogFileName: String;
    FLogFileOK: Boolean;
    FLogFileStatus: String;
    FLastSender: TObject;
    FLastException: Exception;
    FApplicationInfo,
    FLastReport: String;
    FMaxStackCount: Integer;
    FSimpleCriticalSection: TRTLCriticalSection;
    FDefaultAppFlags: TApplicationFlags;
    FUnhandled: Boolean;
    procedure SetLogFileName(const AValue: String);
    procedure SetMaxStackCount(AMaxStackCount: Integer);
  protected
    function ExceptionHeaderMessage: String;
    function GetStackTraceStr: String;
    procedure CreateExceptionReport;
    procedure SaveLogToFile(const LogMsg: String);
    procedure CallExceptionHandler;
    procedure ExceptionHandler;
    procedure UnhandledException(Obj: TObject; Addr: CodePointer; FrameCount: LongInt;
      Frames: PCodePointer);
  public
    IgnoredExceptionList: TStringList;
    property ApplicationInfo: String read FApplicationInfo;
    property LogFileName: String read FLogFileName write SetLogFileName;
    property LogFileOK: Boolean read FLogFileOK;
    property LogFileStatus: String read FLogFileStatus;
    property MaxStackCount: Integer read FMaxStackCount write SetMaxStackCount;
    property LastSender: TObject read FLastSender;
    property LastException: Exception read FLastException;
    property LastReport: String read FLastReport;
    procedure SimpleExceptionHandler(Sender: TObject; E: Exception);
    procedure SimpleExceptionHandlerSaveLogOnly(Sender: TObject; E: Exception);
    constructor Create(const FileName: String = '');
    destructor Destroy; override;
  end;

function GetIOResultStr(const AIOResult: Word): String;
function GetOSVer: String;
function GetFPCVersion: String;
function GetLCLVersion: String;
function GetWidgetSetName: String;
function GetTargetCPU_OS: String;
function GetBuildTime: String;

function GetApplicationInfo: String;
function AddIgnoredException(const EClassName: String): Boolean;
function RemoveIgnoredClass(const EClassName: String): Boolean;
procedure SetMaxStackCount(const ACount: Integer);
procedure ClearIgnoredException;
procedure ExceptionHandle(Sender: TObject; E: Exception);
procedure ExceptionHandleSaveLogOnly(Sender: TObject; E: Exception);
procedure InitSimpleExceptionHandler(const LogFileName: String = '');
procedure SetLogFileName(const LogFileName: String);
procedure DoneSimpleExceptionHandler;

var
  MainExceptionHandler: TSimpleException;

{$IFDEF MULTILOG}
type
  { TLoggerException }

  TLoggerException = class helper for TLogger
  public
    procedure SendExceptionStr(const AText: String; AExceptionStr: String);
    procedure SendStrings(const AText: String; AValue: String);
    procedure SendWarningStrings(const AText: String; AValue: String);
  end;
{$ENDIF}

resourcestring
  SExceptionDialogTitle = 'Exception Info';
  SExceptionCaption = 'An error occured during program execution:';
  SButtonDetails = '&Show Details';
  SButtonTerminate = '&Terminate';
  SButtonContinue = '&Continue';
  SCheckBoxIgnoreException = 'Ignore this exception for next time';
  SCantHandleException = 'Can''t handle exception';

implementation

uses InterfaceBase {$IF LCL_FULLVERSION >= 1080000}, LCLPlatformDef{$ENDIF};

{$IFDEF MULTILOG}
{ TLoggerException }

procedure TLoggerException.SendExceptionStr(const AText: String; AExceptionStr: String);
begin
  SendBuffer(ltException, AText, AExceptionStr[1], Length(AExceptionStr));
end;

procedure TLoggerException.SendStrings(const AText: String; AValue: String);
begin
  SendBuffer(ltStrings, AText, AValue[1], Length(AValue));
end;

procedure TLoggerException.SendWarningStrings(const AText: String;
  AValue: String);
begin
  SendBuffer(ltWarning, AText, AValue[1], Length(AValue));
end;

{$ENDIF}

procedure SetMaxStackCount(const ACount: Integer);
begin
  if MainExceptionHandler <> nil then
    MainExceptionHandler.MaxStackCount := ACount;
end;

function GetIOResultStr(const AIOResult: Word): String;
begin
  Result := IntToStr(AIOResult) + ': ';
  case AIOResult of
    0  : Result := Result + 'OK.';
    2  : Result := Result + 'File not found.';
    3  : Result := Result + 'Path not found.';
    4  : Result := Result + 'Too many open files.';
    5  : Result := Result + 'Access denied.';
    6  : Result := Result + 'Invalid file handle.';
    12 : Result := Result + 'Invalid file-access mode.';
    15 : Result := Result + 'Invalid disk number.';
    16 : Result := Result + 'Cannot remove current directory.';
    17 : Result := Result + 'Cannot rename across volumes.';
    100: Result := Result + 'Error when reading from disk.';
    101: Result := Result + 'Error when writing to disk.';
    102: Result := Result + 'File not assigned.';
    103: Result := Result + 'File not open.';
    104: Result := Result + 'File not opened for input.';
    105: Result := Result + 'File not opened for output.';
    106: Result := Result + 'Invalid number.';
    150: Result := Result + 'Disk is write protected.';
    151: Result := Result + 'Unknown device.';
    152: Result := Result + 'Drive not ready.';
    153: Result := Result + 'Unknown command.';
    154: Result := Result + 'CRC check failed.';
    155: Result := Result + 'Invalid drive specified..';
    156: Result := Result + 'Seek error on disk.';
    157: Result := Result + 'Invalid media type.';
    158: Result := Result + 'Sector not found.';
    159: Result := Result + 'Printer out of paper.';
    160: Result := Result + 'Error when writing to device.';
    161: Result := Result + 'Error when reading from device.';
    162: Result := Result + 'Hardware failure.';
  end;
end;

function GetOSVer: String;
{$IFDEF WINDOWS}
var
  wdir: array [0..MAX_PATH] of Char;

{$ENDIF}
begin
  {$IFDEF LCLcarbon}
  Result := 'Mac OS X 10.';
  {$ENDIF}
  {$IFDEF Linux}
  Result := 'Linux Kernel ';
  {$ENDIF}
  {$IFDEF UNIX}
  Result := 'Unix ';
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := 'Windows ';
  case WindowsVersion of
    wvUnknown: Result += 'Unknown';
    wv95: Result += '95';
    wvNT4: Result += 'NT 4.0';
    wv98: Result += '98';
    wvMe: Result += 'ME';
    wv2000: Result += '2000';
    wvXP: Result += 'XP';
    wvServer2003: Result += 'Server 2003';
    wvVista: Result += 'Vista';
    wv7: Result += '7';
    wv8: Result += '8';
    wv8_1: Result += '8.1';
    wv10: Result += '10';
    wv11: Result += '11';
    else Result += IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion);
  end;
  FillChar({%H-}wdir, SizeOf(wdir), 0);
  GetWindowsDirectory(PChar(wdir), MAX_PATH);
  if (WindowsVersion <> wv11) and DirectoryExists(wdir + '\SysWOW64') then
    Result += ' 64-bit';
  Result += ' Build ' + IntToStr(Win32BuildNumber);
  {$ENDIF}
end;

function GetFPCVersion: String;
begin
  Result := {$I %FPCVERSION%};
end;

function GetLCLVersion: String;
begin
  Result := LCLVersion.lcl_version;
end;

function GetWidgetSetName: String;
begin
  case WidgetSet.LCLPlatform of
    lpGtk         : Result := 'GTK';
    lpGtk2        : Result := 'GTK2';
    lpGtk3        : Result := 'GTK3';
    lpWin32       : Result := 'Win32/Win64';
    lpWinCE       : Result := 'WinCE';
    lpCarbon      : Result := 'Carbon';
    lpQT          : Result := 'Qt';
    lpfpGUI       : Result := 'fpGUI';
    lpNoGUI       : Result := 'NoGUI';
    lpCocoa       : Result := 'Cocoa';
    lpCustomDrawn : Result := 'Custom Drawn';
    {$IF LCL_FULLVERSION >= 1080000}
    lpQt5         : Result := 'Qt5';
    lpMUI         : Result := 'MUI';
    {$ENDIF}
  end;
end;

function GetTargetCPU_OS: String;
begin
  Result := {$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%};
end;

function GetBuildTime: String;
begin
  Result := {$I %DATE%} + ' ' + {$I %TIME%};
end;

function GetApplicationInfo: String;
begin
  Result := '';
  if Assigned(MainExceptionHandler) then
    Result := MainExceptionHandler.ApplicationInfo;
end;

function AddIgnoredException(const EClassName: String): Boolean;
begin
  Result := False;
  if MainExceptionHandler <> nil then
    if MainExceptionHandler.IgnoredExceptionList.IndexOf(EClassName) < 0 then
    begin
      Result := True;
      MainExceptionHandler.IgnoredExceptionList.Add(EClassName);
    end;
end;

function RemoveIgnoredClass(const EClassName: String): Boolean;
begin
  Result := False;
  if MainExceptionHandler <> nil then
    if MainExceptionHandler.IgnoredExceptionList.IndexOf(EClassName) > -1 then
    begin
      Result := True;
      MainExceptionHandler.IgnoredExceptionList.Delete(
        MainExceptionHandler.IgnoredExceptionList.IndexOf(EClassName));
    end;
end;

procedure ClearIgnoredException;
begin
  if MainExceptionHandler <> nil then
    MainExceptionHandler.IgnoredExceptionList.Clear;
end;

procedure ExceptionHandle(Sender: TObject; E: Exception);
begin
  if not Assigned(MainExceptionHandler) then
    InitSimpleExceptionHandler;
  MainExceptionHandler.SimpleExceptionHandler(Sender, E);
end;

procedure ExceptionHandleSaveLogOnly(Sender: TObject; E: Exception);
begin
  if not Assigned(MainExceptionHandler) then
    InitSimpleExceptionHandler;
  MainExceptionHandler.SimpleExceptionHandlerSaveLogOnly(Sender, E);
end;

procedure InitSimpleExceptionHandler(const LogFileName: String);
begin
  if MainExceptionHandler = nil then
    MainExceptionHandler := TSimpleException.Create(LogFilename);
end;

procedure SetLogFileName(const LogFileName: String);
begin
  if MainExceptionHandler <> nil then
    MainExceptionHandler.LogFileName := LogFileName;
end;

procedure DoneSimpleExceptionHandler;
begin
  if MainExceptionHandler <> nil then
    FreeAndNil(MainExceptionHandler);
end;

{ TSimpleException }

procedure TSimpleException.SetMaxStackCount(AMaxStackCount: Integer);
begin
  if FMaxStackCount <> AMaxStackCount then
  begin
    if AMaxStackCount < 1 then
      FMaxStackCount := 1
    else
    if AMaxStackCount > 255 then
      FMaxStackCount := 255
    else
      FMaxStackCount := AMaxStackCount;
  end;
end;

procedure TSimpleException.SetLogFileName(const AValue: String);
var
  ir: Word;
  fe: Boolean;
begin
  // always check for log file in case something changed at runtime (permission/disk removed)
  FLogFileName := AValue;
  AssignFile(FLogFileHandle, FLogFileName);
  fe := FileExists(FLogFileName);
  if fe then
  {$I-}
    Append(FLogFileHandle)
  else
    Rewrite(FLogFileHandle);
  {$I+}
  ir := IOResult;
  if ir = 0 then
  begin
    CloseFile(FLogFileHandle);
    if not fe then
      Erase(FLogFileHandle);
  end;
  FLogFileOK := ir = 0;
  FLogFileStatus := GetIOResultStr(ir);
end;

procedure TSimpleException.ExceptionHandler;
begin
  if Assigned(FLastException) then
    if (IgnoredExceptionList.IndexOf(FLastException.ClassName) > -1) then
      Exit;
  with TSimpleExceptionForm.Create(nil) do try
      MemoExceptionLog.Lines.Text := FLastReport;
      if Assigned(FLastException) then
        LabelExceptionMessage.Caption := FLastException.Message;
      if FUnhandled then
      begin
        CheckBoxIgnoreException.Visible := False;
        ButtonContinue.Visible := False;
      end;
      if ShowModal = mrIgnore then
        AddIgnoredException(FLastException.ClassName);
    finally
      Free;
    end;
end;

procedure TSimpleException.UnhandledException(Obj: TObject; Addr: CodePointer;
  FrameCount: LongInt; Frames: PCodePointer);
var
  i: Integer;
  S: String;
begin
  EnterCriticalSection(FSimpleCriticalSection);
  try
    FUnhandled := True;
    if Obj is Exception then
    begin
      FLastSender := nil;
      FLastException := Exception(Obj);
      CreateExceptionReport;
      CallExceptionHandler;
    end
    else
    begin
      FLastReport := ExceptionHeaderMessage;
      if Obj is TObject then
        FLastReport := FLastReport +
          'Sender Class      : ' + Obj.ClassName + LineEnding;
      FLastReport := FLastReport +
        'Exception Address : $' + BackTraceStrFunc(Addr) + LineEnding;
      S := '';
      if FrameCount > 0 then
        for i := 0 to FrameCount - 1 do
          S := S + '  ' + BackTraceStrFunc(Frames[i]) + LineEnding;
      FLastReport := FLastReport + S;
      {$IFDEF MULTILOG}
      if Logger.Enabled then
        Logger.SendExceptionStr('Unhandled Exception occured at $' + BackTraceStrFunc(Addr), S)
      else
      {$ENDIF}
        SaveLogToFile(FLastReport);
      CallExceptionHandler;
    end;
  finally
    LeaveCriticalSection(FSimpleCriticalSection);
  end;
end;

function TSimpleException.ExceptionHeaderMessage: String;
var
  i: Integer;
begin
  try
    if FUnhandled then
      Result := 'Unhandled exception!'
    else
      Result := 'Program exception!';
    Result := Result + LineEnding +
      FApplicationInfo + LineEnding +
      'Thread ID         : ' + IntToStr(GetThreadID) + LineEnding +
      'Time              : ' + FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz', Now) + LineEnding;
    if IgnoredExceptionList.Count > 0 then
    begin
      Result := Result + 'Ignored Exception : ' + IgnoredExceptionList[0];
      for i := 1 to IgnoredExceptionList.Count - 1 do
        Result := Result + ', ' + IgnoredExceptionList[i];
      Result := Result + LineEnding;
    end;
  except
    Result := '';
  end;
end;

function TSimpleException.GetStackTraceStr: String;
{$IFDEF DEBUGINFO}
var
  Frames: PPointer;
  i, AMaxStackCount: Integer;
{$ENDIF}
begin
  Result := '';
  {$IFDEF DEBUGINFO}
  try
    Result := BackTraceStrFunc(ExceptAddr);
    Frames := ExceptFrames;
    if ExceptFrameCount > FMaxStackCount then
      AMaxStackCount := FMaxStackCount
    else
      AMaxStackCount := ExceptFrameCount;
    for i := 0 to AMaxStackCount - 1 do
      Result := Result + (LineEnding + BackTraceStrFunc(Frames[i]));
  except
  end;
  {$ENDIF}
end;

procedure TSimpleException.CreateExceptionReport;
var
  S: String;
begin
  S := '';
  FLastReport := ExceptionHeaderMessage;
  if Assigned(FLastSender) then
    FLastReport := FLastReport +
      'Sender Class      : ' + FLastSender.ClassName + LineEnding;
  if Assigned(FLastException) then
  begin
    FLastReport := FLastReport +
      'Exception Object  : ' + HexStr(FLastException) + LineEnding +
      'Exception Class   : ' + FLastException.ClassName + LineEnding +
      'Message           : ' + FLastException.Message + LineEnding;
  end;
  S := GetStackTraceStr;
  FLastReport := FLastReport + S;
  {$IFDEF MULTILOG}
  if Logger.Enabled then
  begin
    if Assigned(FLastException) then
      Logger.SendExceptionStr(HexStr(FLastException) + ' ' + FLastException.ClassName + ' - ' + FLastException.Message, S)
    else
      Logger.SendExceptionStr('Program exception!', S);
  end
  else
  {$ENDIF}
    SaveLogToFile(FLastReport);
end;

procedure TSimpleException.SaveLogToFile(const LogMsg: String);
var
  ir: Word;
begin
  if FLogFileName = '' then Exit;
  if FileExists(FLogFileName) then
  {$I-}
    Append(FLogFileHandle)
  else
    Rewrite(FLogFileHandle);
  {$I+}
  ir := IOResult;
  if ir = 0 then
  begin
    WriteLn(FLogFileHandle, LogMsg);
    CloseFile(FLogFileHandle);
  end
  else
  begin
    FLastReport := 'Failed to write exception message to "' + (FLogFileName) + '"' + LineEnding +
      '  ' + GetIOResultStr(ir) + LineEnding +
      FLastReport;
  end;
end;

procedure TSimpleException.CallExceptionHandler;
begin
  if (ThreadID <> MainThreadID) then
    try
      TThread.Synchronize(TThread.CurrentThread, @ExceptionHandler);
    except
      {$IFDEF MULTILOG}
      if Logger.Enabled then
        Logger.SendError(SCantHandleException)
      else
      {$ENDIF}
        SaveLogToFile(SCantHandleException);
    end
  else
    ExceptionHandler;
end;

procedure TSimpleException.SimpleExceptionHandler(Sender: TObject; E: Exception);
begin
  if E = nil then
    Exit;
  EnterCriticalsection(FSimpleCriticalSection);
  try
    FUnhandled := False;
    FLastSender := Sender;
    FLastException := E;
    CreateExceptionReport;
    CallExceptionHandler;
  finally
    LeaveCriticalsection(FSimpleCriticalSection);
  end;
end;

procedure TSimpleException.SimpleExceptionHandlerSaveLogOnly(Sender: TObject;
  E: Exception);
begin
  if E = nil then
    Exit;
  EnterCriticalsection(FSimpleCriticalSection);
  try
    FUnhandled := False;
    FLastSender := Sender;
    FLastException := E;
    CreateExceptionReport;
  finally
    LeaveCriticalsection(FSimpleCriticalSection);
  end;
end;

procedure CatchUnhandledExcept(Obj: TObject; Addr: CodePointer; FrameCount: LongInt; Frames: PCodePointer);
begin
  if Assigned(MainExceptionHandler) then
    MainExceptionHandler.UnhandledException(Obj, Addr, FrameCount, Frames);
end;

constructor TSimpleException.Create(const FileName: String);
var
  AFileVersion, AProductVersion: String;
begin
  inherited Create;
  FLogFileName := '';
  FLogFileOK := False;
  FLogFileStatus := '';
  if Trim(FileName) <> '' then
    LogFileName := FileName;
  InitCriticalSection(FSimpleCriticalSection);
  IgnoredExceptionList := TStringList.Create;
  FMaxStackCount := 20;
  with TFileVersionInfo.Create(nil) do
    try
      FileName := ParamStr(0);
      if FileName = '' then
        FileName := Application.ExeName;
      try
        Enabled := True;
        if VersionStrings.Count <> 0 then
        begin
          AFileVersion := VersionStrings.Values['fileversion'];
          AProductVersion := VersionStrings.Values['productversion'];
        end;
      except
      end;
    finally
      Free;
    end;
  FApplicationInfo :=
    'Application       : ' + Application.Title + LineEnding;
  if AFileVersion <> '' then
    FApplicationInfo := FApplicationInfo +
      'Version           : ' + AFileVersion + LineEnding;
  if AProductVersion <> '' then
    FApplicationInfo := FApplicationInfo +
      'Product Version   : ' + AProductVersion + LineEnding;
  FApplicationInfo := FApplicationInfo +
    'Host Machine      : ' + GetOSVer + LineEnding +
    'FPC Version       : ' + GetFPCVersion + LineEnding +
    'LCL Version       : ' + GetLCLVersion + LineEnding +
    'WidgetSet         : ' + GetWidgetSetName + LineEnding +
    'Target CPU-OS     : ' + GetTargetCPU_OS + LineEnding +
    'Build Time        : ' + GetBuildTime + LineEnding +
    'Path              : ' + ParamStr(0) + LineEnding +
    'Process ID        : ' + IntToStr(GetProcessID) + LineEnding +
    'MainThread ID     : ' + IntToStr(MainThreadID);
  if Assigned(Application) then
  begin
    FDefaultAppFlags := Application.Flags;
    Application.Flags := Application.Flags + [AppNoExceptionMessages];
    Application.OnException := @SimpleExceptionHandler;
    ExceptProc := @CatchUnhandledExcept;
  end;
end;

destructor TSimpleException.Destroy;
begin
  if Assigned(Application) then
  begin
    Application.OnException := nil;
    Application.Flags := FDefaultAppFlags;
  end;
  IgnoredExceptionList.Free;
  DoneCriticalsection(FSimpleCriticalSection);
  inherited Destroy;
end;

initialization

finalization
  DoneSimpleExceptionHandler;

end.
