unit ImageMagickManager;

{$mode objfpc}{$H+}
{$PACKRECORDS C}
{$z4}

interface

uses
  Classes, SysUtils, synautil, Windows, Registry,
  LazFileUtils, SyncObjs, Dynlibs;

const
  // Makes Windows use the DLL's own directory when resolving its dependencies,
  // instead of the standard PATH-based search. Essential for portable/bundled
  // DLL folders that are not on the system PATH.
  // Value is from the Windows SDK (winbase.h).
  LOAD_WITH_ALTERED_SEARCH_PATH = $00000008;

// LoadLibraryEx is not always declared in FPC's Windows unit, so we declare
// it explicitly. We use the ANSI variant (A suffix) since FMagickPath is an
// AnsiString/String — matching what PChar resolves to in FPC.
function LoadLibraryExA(lpLibFileName: LPCSTR; hFile: THandle; dwFlags: DWORD): HMODULE;
  stdcall; external 'kernel32' name 'LoadLibraryExA';

type
  MagickBooleanType = (MagickFalse = 0, MagickTrue = 1);
  PMagickWand = Pointer;
  PMagickSizeTArray = ^Size_t;
  Size_t = NativeUInt;
  PSize_t = ^Size_t;
  PExceptionType = ^Integer;

  TMagickWandGenesis = procedure; cdecl;
  TMagickWandTerminus = procedure; cdecl;
  TNewMagickWand = function: PMagickWand; cdecl;
  TDestroyMagickWand = function(wand: PMagickWand): PMagickWand; cdecl;
  TMagickReadImage = function(wand: PMagickWand; filename: PAnsiChar): MagickBooleanType; cdecl;
  TMagickReadImageBlob = function(wand: PMagickWand; blob: Pointer; length: Size_t): MagickBooleanType; cdecl;
  TMagickGetImageBlob = function(wand: PMagickWand; length: PSize_t): PByte; cdecl;
  TMagickGetImagesBlob = function(wand: PMagickWand; length: PSize_t): PByte; cdecl;
  TMagickSetFormat = function(wand: PMagickWand; format: PAnsiChar): MagickBooleanType; cdecl;
  TMagickWriteImage = function(wand: PMagickWand; filename: PAnsiChar): MagickBooleanType; cdecl;
  TMagickWriteImages = function(wand: PMagickWand; filename: PAnsiChar; adjoin: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickGetException = function(wand: PMagickWand; severity: PExceptionType): PAnsiChar; cdecl;
  TMagickCoalesceImages = function(wand: PMagickWand): PMagickWand; cdecl;
  TMagickRelinquishMemory = function(ptr: Pointer): Pointer; cdecl;
  TMagickQueryFormats = function(pattern: PAnsiChar; number_formats: PInteger): PPAnsiChar; cdecl;
  TMagickSetImageOption = function(wand: PMagickWand; option: PAnsiChar; value: PAnsiChar): MagickBooleanType; cdecl;
  TMagickSetImageCompressionQuality = function(wand: PMagickWand; quality: Size_t): MagickBooleanType; cdecl;
  TMagickSetImageCompression = function(wand: PMagickWand; compression: Integer): MagickBooleanType; cdecl;
  TMagickResetIterator = procedure(wand: PMagickWand); cdecl;
  TMagickSetFirstIterator = procedure(wand: PMagickWand); cdecl;
  TMagickGetImageWidth = function(wand: PMagickWand): Size_t; cdecl;
  TMagickGetImageHeight = function(wand: PMagickWand): Size_t; cdecl;
  TMagickGetNumberImages = function(wand: PMagickWand): Size_t; cdecl;
  TMagickGetImageFormat = function(wand: PMagickWand): PAnsiChar; cdecl;

var
  pMagickWandGenesis: TMagickWandGenesis = nil;
  pMagickWandTerminus: TMagickWandTerminus = nil;
  pNewMagickWand: TNewMagickWand = nil;
  pDestroyMagickWand: TDestroyMagickWand = nil;
  pMagickReadImage: TMagickReadImage = nil;
  pMagickReadImageBlob: TMagickReadImageBlob = nil;
  pMagickGetImageBlob: TMagickGetImageBlob = nil;
  pMagickGetImagesBlob: TMagickGetImagesBlob = nil;
  pMagickSetFormat: TMagickSetFormat = nil;
  pMagickWriteImage: TMagickWriteImage = nil;
  pMagickWriteImages: TMagickWriteImages = nil;
  pMagickGetException: TMagickGetException = nil;
  pMagickCoalesceImages: TMagickCoalesceImages = nil;
  pMagickRelinquishMemory: TMagickRelinquishMemory = nil;
  pMagickQueryFormats: TMagickQueryFormats = nil;
  pMagickSetImageOption: TMagickSetImageOption = nil;
  pMagickSetImageCompressionQuality: TMagickSetImageCompressionQuality = nil;
  pMagickSetImageCompression: TMagickSetImageCompression = nil;
  pMagickResetIterator: TMagickResetIterator = nil;
  pMagickSetFirstIterator: TMagickSetFirstIterator = nil;
  pMagickGetImageWidth: TMagickGetImageWidth = nil;
  pMagickGetImageHeight: TMagickGetImageHeight = nil;
  pMagickGetNumberImages: TMagickGetNumberImages = nil;
  pMagickGetImageFormat: TMagickGetImageFormat = nil;

type
  { TImageMagickManager }
  TImageMagickManager = class
  private
    class var FInstance: TImageMagickManager;
    class var FInitialized: Boolean;
    class var FLock: TCriticalSection;

    FPathFound: Boolean;
    FEnabled: Boolean;
    FMogrify: Boolean;
    FMagickPath: String;
    FSupportedFormats: TStringList;
    FCompressionTypes: TStringList;
    FSaveAs: String;
    FQuality: Integer;
    FCompression: String;
    FLastError: String;
    FParallelConversions: Integer;
    FSemaphore: THandle;

    FDllHandle: TLibHandle;
    FDependencyHandles: array of TLibHandle;

    function FindMagickBinary: Boolean;
    function LoadMagickDLL: Boolean;
    procedure UnloadMagickDLL;
    procedure CacheSupportedFormats;
    procedure CacheCompressionTypes;
    function GetException(wand: PMagickWand): String;
    constructor CreatePrivate;

    function GetPathFound: Boolean;
    function GetMagickPath: String;
    function GetEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    function GetMogrify: Boolean;
    procedure SetMogrify(AMogrify: Boolean);
    function GetSupportedFormats: TStrings;
    function GetCompressionTypes: TStrings;
{$ifdef USE_LCL}
    procedure SetComboBoxList(AComboBox: TComboBox; AList: TStringList);
{$endif}
    function GetSaveAs: String;
    procedure SetSaveAs(ASaveAs: String);
    function GetQuality: Integer;
    procedure SetQuality(AQuality: Integer);
    function GetCompression: String;
    procedure SetCompression(ACompression: String);
    function GetLastError: String;
    function GetTempPathStr: String;
    function GetParallelConversions: Integer;
    procedure SetParallelConversions(AValue: Integer);

    function WandConvertBlob(Stream: TMemoryStream; const OutputFormat: String; Coalesce: Boolean; const InputFormat: String = ''): TMemoryStream;
  public
    class function Instance: TImageMagickManager;
    class procedure Initialize;
    class procedure Finalize;

    function IsFormatSupported(const Format: String): Boolean;
    function Identify(Stream: TStream; const InputFormat: String = ''): String;
    function ConvertStream(Stream: TStream; const OutputFormat: String; Coalesce: Boolean = False; const InputFormat: String = ''): TMemoryStream;
    function ConvertImage(InputFile, OutputDir: String): Boolean;

    property PathFound: Boolean read GetPathFound;
    property MagickPath: String read GetMagickPath;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Mogrify: Boolean read GetMogrify write SetMogrify;
    property SupportedFormats: TStrings read GetSupportedFormats;
    property CompressionTypes: TStrings read GetCompressionTypes;
    property SaveAs: String read GetSaveAs write SetSaveAs;
    property Quality: Integer read GetQuality write SetQuality;
    property Compression: String read GetCompression write SetCompression;
    property LastError: String read GetLastError;
    property ParallelConversions: Integer read GetParallelConversions write SetParallelConversions;
  end;

resourcestring
  RS_ImageMagickPreferenceHint = 'ImageMagick has been enabled!'#13#10'The following settings have been disabled because ImageMagick has taken preference.';
  RS_ImageMagickNotFoundHint = 'Failed to detect an install of ImageMagick!'#13#10'Please install ImageMagick from www.imagemagick.org if you haven''t already.';

implementation

{$ifdef USE_LCL}
uses
  MultiLog, frmMain;
{$endif}

procedure IMLog(const Msg: String);
begin
  OutputDebugString(PChar('IM: ' + Msg));
end;

{ TImageMagickManager }

class function TImageMagickManager.Instance: TImageMagickManager;
begin
  if not FInitialized then
    Initialize;
  Result := FInstance;
end;

class procedure TImageMagickManager.Initialize;
begin
  if not FInitialized then
  begin
    FLock := TCriticalSection.Create;
    FInitialized := True;
    FPathFound := False;
    FEnabled := True;
    FMogrify := False;
    FInstance := TImageMagickManager.CreatePrivate;
  end;
end;

class procedure TImageMagickManager.Finalize;
begin
  if FInitialized then
  begin
    if Assigned(FInstance) then
    begin
      FInstance.UnloadMagickDLL;
      if FInstance.FSemaphore <> 0 then
      begin
        CloseHandle(FInstance.FSemaphore);
        FInstance.FSemaphore := 0;
      end;
    end;
    FreeAndNil(FInstance);
    FreeAndNil(FLock);
    FInitialized := False;
  end;
end;

{$ifdef USE_LCL}
procedure TImageMagickManager.SetComboBoxList(AComboBox: TComboBox; AList: TStringList);
var
  SelectedString: String;
  SelectedIndex: Integer;
begin
  SelectedIndex := AComboBox.ItemIndex;
  if SelectedIndex >= 0 then
    SelectedString := AComboBox.Items.ValueFromIndex[SelectedIndex]
  else
    SelectedString := '';
  AComboBox.Clear;
  AComboBox.Items.AddStrings(AList);
  AComboBox.ItemIndex := AComboBox.Items.IndexOf(SelectedString);
  if (AComboBox.ItemIndex = -1) and (AComboBox.Items.Count >= 1) then
    AComboBox.ItemIndex := 0;
end;
{$endif}

constructor TImageMagickManager.CreatePrivate;
begin
  inherited Create;
  FSupportedFormats := TStringList.Create;
  FSupportedFormats.CaseSensitive := False;
  FCompressionTypes := TStringList.Create;
  FCompressionTypes.CaseSensitive := False;
  FQuality := 75;
  FCompression := 'None';
  FParallelConversions := 1;
  FSemaphore := CreateSemaphore(nil, FParallelConversions, 32, nil);
  FDllHandle := NilHandle;

  if not FindMagickBinary then
  begin
{$ifdef USE_LCL}
    MainForm.gbImageMagick.Hint := RS_ImageMagickNotFoundHint;
{$endif}
    Exit;
  end;

  if not LoadMagickDLL then
  begin
{$ifdef USE_LCL}
    MainForm.gbImageMagick.Hint := RS_ImageMagickNotFoundHint;
{$endif}
    Exit;
  end;

  FPathFound := True;
  CacheSupportedFormats;
  CacheCompressionTypes;

{$ifdef USE_LCL}
  with MainForm do
  begin
    lbImageMagickHint.Visible := False;
    gbImageMagick.Enabled := True;
    ckImageMagick.Enabled := True;
    cbImageMagickSaveAs.Enabled := True;
    cbImageMagickCompression.Enabled := True;
    seImageMagickQuality.Enabled := True;
    SetComboBoxList(cbImageMagickSaveAs, FSupportedFormats);
    SetComboBoxList(cbImageMagickCompression, FCompressionTypes);
  end;
{$endif}
end;

function TImageMagickManager.GetPathFound: Boolean; begin Result := FPathFound; end;
function TImageMagickManager.GetMagickPath: String; begin Result := FMagickPath; end;
function TImageMagickManager.GetEnabled: Boolean; begin Result := FEnabled; end;
procedure TImageMagickManager.SetEnabled(AEnabled: Boolean); begin FEnabled := AEnabled; end;
function TImageMagickManager.GetMogrify: Boolean; begin Result := FMogrify; end;
procedure TImageMagickManager.SetMogrify(AMogrify: Boolean); begin FMogrify := AMogrify; end;
function TImageMagickManager.GetSupportedFormats: TStrings; begin Result := FSupportedFormats; end;
function TImageMagickManager.GetCompressionTypes: TStrings; begin Result := FCompressionTypes; end;
function TImageMagickManager.GetSaveAs: String; begin Result := FSaveAs; end;
procedure TImageMagickManager.SetSaveAs(ASaveAs: String); begin FSaveAs := LowerCase(ASaveAs); end;
function TImageMagickManager.GetQuality: Integer; begin Result := FQuality; end;
procedure TImageMagickManager.SetQuality(AQuality: Integer); begin FQuality := AQuality; end;
function TImageMagickManager.GetCompression: String; begin Result := FCompression; end;
procedure TImageMagickManager.SetCompression(ACompression: String); begin FCompression := ACompression; end;

function TImageMagickManager.GetParallelConversions: Integer; begin Result := FParallelConversions; end;
procedure TImageMagickManager.SetParallelConversions(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if AValue > 32 then AValue := 32;
  if AValue = FParallelConversions then Exit;
  FParallelConversions := AValue;
  // Recreate the semaphore with the new limit.
  // This is safe to call from the main thread while no conversion is running.
  if FSemaphore <> 0 then CloseHandle(FSemaphore);
  FSemaphore := CreateSemaphore(nil, FParallelConversions, 32, nil);
end;
function TImageMagickManager.GetLastError: String;
begin
  FLock.Acquire;
  try Result := FLastError; finally FLock.Release; end;
end;

function TImageMagickManager.GetTempPathStr: String;
var
  Buf: array[0..MAX_PATH] of Char;
begin
  Windows.GetTempPath(MAX_PATH, Buf);
  Result := Trim(Buf);
  if (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;

function TImageMagickManager.FindMagickBinary: Boolean;
var
  Reg: TRegistry;
  Paths: array of String;
  SearchPath: String;
  PathList: TStringList;
  DllName: String;
  i: Integer;
begin
  Result := False;
  FMagickPath := '';
  FLastError := '';
  DllName := 'CORE_RL_MagickWand_.dll';

  SearchPath := ExtractFilePath(ParamStr(0)) + 'ImageMagick_dependency\';
  if FileExists(SearchPath + DllName) then
  begin
    FMagickPath := SearchPath;
    Result := True;
    Exit;
  end;

  SearchPath := ExtractFilePath(ParamStr(0));
  if FileExists(SearchPath + DllName) then
  begin
    FMagickPath := SearchPath;
    Result := True;
    Exit;
  end;

  Paths := [
    'C:\Program Files\ImageMagick-7.1.2-Q16',
    'C:\Program Files\ImageMagick-7.1.1-Q16',
    'C:\Program Files\ImageMagick-7.1.0-Q16',
    'C:\Program Files\ImageMagick-7.0.11-Q16',
    'C:\Program Files\ImageMagick-7.0.10-Q16',
    'C:\Program Files\ImageMagick-6.9.12-Q16',
    'C:\Program Files (x86)\ImageMagick-7.1.2-Q16',
    'C:\Program Files (x86)\ImageMagick-7.1.1-Q16'
  ];

  for SearchPath in Paths do
  begin
    if FileExists(SearchPath + '\' + DllName) then
    begin
      FMagickPath := SearchPath + '\';
      Result := True;
      Exit;
    end;
  end;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\Imagemagick\7') or Reg.OpenKeyReadOnly('SOFTWARE\Wow6432Node\Imagemagick\7') then
    begin
      try
        SearchPath := Reg.ReadString('LibPath');
        if (SearchPath <> '') and FileExists(SearchPath + '\' + DllName) then
        begin
          if SearchPath[Length(SearchPath)] <> '\' then SearchPath := SearchPath + '\';
          FMagickPath := SearchPath;
          Result := True;
          Exit;
        end;
      except
      end;
    end;
  finally
    Reg.Free;
  end;

  PathList := TStringList.Create;
  try
    SetLength(SearchPath, 32768);
    i := GetEnvironmentVariable('PATH', PChar(SearchPath), Length(SearchPath));
    if i > 0 then
    begin
      SetLength(SearchPath, i);
      PathList.Delimiter := ';';
      PathList.StrictDelimiter := True;
      PathList.DelimitedText := SearchPath;
      for i := 0 to PathList.Count - 1 do
      begin
        SearchPath := PathList[i];
        if (SearchPath <> '') and FileExists(SearchPath + '\' + DllName) then
        begin
          if SearchPath[Length(SearchPath)] <> '\' then SearchPath := SearchPath + '\';
          FMagickPath := SearchPath;
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    PathList.Free;
  end;

  FLastError := 'ImageMagick DLL not found';
end;

function TImageMagickManager.LoadMagickDLL: Boolean;
var
  DllPath: String;
  LastErr: DWORD;
  I: Integer;
  SR: SysUtils.TSearchRec;
  H: TLibHandle;
  DiagLog: String;
begin
  Result := False;
  if FDllHandle <> NilHandle then Exit(True);

  DllPath := FMagickPath + 'CORE_RL_MagickWand_.dll';
  if not FileExists(DllPath) then
  begin
    FLastError := 'DLL not found: ' + DllPath;
    Exit;
  end;

  // Tell ImageMagick where to find its config files and coder modules.
  SetEnvironmentVariable('MAGICK_HOME', PChar(FMagickPath));
  SetEnvironmentVariable('MAGICK_CONFIGURE_PATH', PChar(FMagickPath));
  SetEnvironmentVariable('MAGICK_CODER_MODULE_PATH', PChar(FMagickPath + 'modules\coders'));
  SetEnvironmentVariable('MAGICK_FILTER_MODULE_PATH', PChar(FMagickPath + 'modules\filters'));
  SetEnvironmentVariable('MAGICK_THREAD_LIMIT', '1');

  // Load every DLL in the bundled folder using LOAD_WITH_ALTERED_SEARCH_PATH.
  // This flag makes the Windows loader use each DLL's own directory when
  // resolving *its* dependencies, so the entire CORE_RL_* chain resolves
  // from FMagickPath without needing PATH changes or SetDllDirectory.
  SetLength(FDependencyHandles, 0);
  DiagLog := '';

  // Pass 1: VC++ runtime DLLs first (vcruntime, vcomp, msvcp).
  // If already loaded system-wide, LoadLibraryEx just bumps the ref-count.
  if SysUtils.FindFirst(FMagickPath + 'vcruntime*.dll', faAnyFile, SR) = 0 then
  try
    repeat
      H := TLibHandle(LoadLibraryExA(PChar(FMagickPath + SR.Name), 0, LOAD_WITH_ALTERED_SEARCH_PATH));
      if H <> NilHandle then
      begin
        SetLength(FDependencyHandles, Length(FDependencyHandles) + 1);
        FDependencyHandles[High(FDependencyHandles)] := H;
      end else
        DiagLog := DiagLog + SR.Name + '(err=' + IntToStr(Windows.GetLastError) + ') ';
    until SysUtils.FindNext(SR) <> 0;
  finally
    SysUtils.FindClose(SR);
  end;

  if SysUtils.FindFirst(FMagickPath + 'vcomp*.dll', faAnyFile, SR) = 0 then
  try
    repeat
      H := TLibHandle(LoadLibraryExA(PChar(FMagickPath + SR.Name), 0, LOAD_WITH_ALTERED_SEARCH_PATH));
      if H <> NilHandle then
      begin
        SetLength(FDependencyHandles, Length(FDependencyHandles) + 1);
        FDependencyHandles[High(FDependencyHandles)] := H;
      end else
        DiagLog := DiagLog + SR.Name + '(err=' + IntToStr(Windows.GetLastError) + ') ';
    until SysUtils.FindNext(SR) <> 0;
  finally
    SysUtils.FindClose(SR);
  end;

  if SysUtils.FindFirst(FMagickPath + 'msvcp*.dll', faAnyFile, SR) = 0 then
  try
    repeat
      H := TLibHandle(LoadLibraryExA(PChar(FMagickPath + SR.Name), 0, LOAD_WITH_ALTERED_SEARCH_PATH));
      if H <> NilHandle then
      begin
        SetLength(FDependencyHandles, Length(FDependencyHandles) + 1);
        FDependencyHandles[High(FDependencyHandles)] := H;
      end else
        DiagLog := DiagLog + SR.Name + '(err=' + IntToStr(Windows.GetLastError) + ') ';
    until SysUtils.FindNext(SR) <> 0;
  finally
    SysUtils.FindClose(SR);
  end;

  // Pass 2: all CORE_RL_*.dll except MagickWand (that comes last as FDllHandle).
  if SysUtils.FindFirst(FMagickPath + 'CORE_RL_*.dll', faAnyFile, SR) = 0 then
  try
    repeat
      if CompareText(SR.Name, 'CORE_RL_MagickWand_.dll') = 0 then
        Continue;
      H := TLibHandle(LoadLibraryExA(PChar(FMagickPath + SR.Name), 0, LOAD_WITH_ALTERED_SEARCH_PATH));
      if H <> NilHandle then
      begin
        SetLength(FDependencyHandles, Length(FDependencyHandles) + 1);
        FDependencyHandles[High(FDependencyHandles)] := H;
      end else
        DiagLog := DiagLog + SR.Name + '(err=' + IntToStr(Windows.GetLastError) + ') ';
    until SysUtils.FindNext(SR) <> 0;
  finally
    SysUtils.FindClose(SR);
  end;

  // Final: load MagickWand itself.
  FDllHandle := TLibHandle(LoadLibraryExA(PChar(DllPath), 0, LOAD_WITH_ALTERED_SEARCH_PATH));
  if FDllHandle = NilHandle then
  begin
    LastErr := Windows.GetLastError;
    FLastError := 'Failed to load CORE_RL_MagickWand_.dll (WinError=' + IntToStr(LastErr) + ').' +
                  ' Path: ' + FMagickPath;
    if DiagLog <> '' then
      FLastError := FLastError + ' Failed deps: ' + DiagLog;
    for I := 0 to High(FDependencyHandles) do
      FreeLibrary(FDependencyHandles[I]);
    SetLength(FDependencyHandles, 0);
    Exit;
  end;

  pMagickWandGenesis := TMagickWandGenesis(GetProcAddress(FDllHandle, 'MagickWandGenesis'));
  pMagickWandTerminus := TMagickWandTerminus(GetProcAddress(FDllHandle, 'MagickWandTerminus'));
  pNewMagickWand := TNewMagickWand(GetProcAddress(FDllHandle, 'NewMagickWand'));
  pDestroyMagickWand := TDestroyMagickWand(GetProcAddress(FDllHandle, 'DestroyMagickWand'));
  pMagickReadImage := TMagickReadImage(GetProcAddress(FDllHandle, 'MagickReadImage'));
  pMagickReadImageBlob := TMagickReadImageBlob(GetProcAddress(FDllHandle, 'MagickReadImageBlob'));
  pMagickGetImageBlob := TMagickGetImageBlob(GetProcAddress(FDllHandle, 'MagickGetImageBlob'));
  pMagickGetImagesBlob := TMagickGetImagesBlob(GetProcAddress(FDllHandle, 'MagickGetImagesBlob'));
  pMagickSetFormat := TMagickSetFormat(GetProcAddress(FDllHandle, 'MagickSetFormat'));
  pMagickWriteImage := TMagickWriteImage(GetProcAddress(FDllHandle, 'MagickWriteImage'));
  pMagickWriteImages := TMagickWriteImages(GetProcAddress(FDllHandle, 'MagickWriteImages'));
  pMagickGetException := TMagickGetException(GetProcAddress(FDllHandle, 'MagickGetException'));
  pMagickCoalesceImages := TMagickCoalesceImages(GetProcAddress(FDllHandle, 'MagickCoalesceImages'));
  pMagickRelinquishMemory := TMagickRelinquishMemory(GetProcAddress(FDllHandle, 'MagickRelinquishMemory'));
  pMagickQueryFormats := TMagickQueryFormats(GetProcAddress(FDllHandle, 'MagickQueryFormats'));
  pMagickSetImageOption := TMagickSetImageOption(GetProcAddress(FDllHandle, 'MagickSetImageOption'));
  pMagickSetImageCompressionQuality := TMagickSetImageCompressionQuality(GetProcAddress(FDllHandle, 'MagickSetImageCompressionQuality'));
  pMagickSetImageCompression := TMagickSetImageCompression(GetProcAddress(FDllHandle, 'MagickSetImageCompression'));
  pMagickResetIterator := TMagickResetIterator(GetProcAddress(FDllHandle, 'MagickResetIterator'));
  pMagickSetFirstIterator := TMagickSetFirstIterator(GetProcAddress(FDllHandle, 'MagickSetFirstIterator'));
  pMagickGetImageWidth := TMagickGetImageWidth(GetProcAddress(FDllHandle, 'MagickGetImageWidth'));
  pMagickGetImageHeight := TMagickGetImageHeight(GetProcAddress(FDllHandle, 'MagickGetImageHeight'));
  pMagickGetNumberImages := TMagickGetNumberImages(GetProcAddress(FDllHandle, 'MagickGetNumberImages'));
  pMagickGetImageFormat := TMagickGetImageFormat(GetProcAddress(FDllHandle, 'MagickGetImageFormat'));

  pMagickWandGenesis;
  Result := True;
end;

procedure TImageMagickManager.UnloadMagickDLL;
var
  DepIdx: Integer;
begin
  if FDllHandle <> NilHandle then
  begin
    if Assigned(pMagickWandTerminus) then
      pMagickWandTerminus;
    FreeLibrary(FDllHandle);
    FDllHandle := NilHandle;
    pMagickWandGenesis := nil;
    pMagickWandTerminus := nil;
    pNewMagickWand := nil;
    pDestroyMagickWand := nil;
    pMagickReadImage := nil;
    pMagickReadImageBlob := nil;
    pMagickGetImageBlob := nil;
    pMagickGetImagesBlob := nil;
    pMagickSetFormat := nil;
    pMagickWriteImage := nil;
    pMagickWriteImages := nil;
    pMagickGetException := nil;
    pMagickCoalesceImages := nil;
    pMagickRelinquishMemory := nil;
    pMagickQueryFormats := nil;
    pMagickSetImageOption := nil;
    pMagickSetImageCompressionQuality := nil;
    pMagickSetImageCompression := nil;
    pMagickResetIterator := nil;
    pMagickSetFirstIterator := nil;
    pMagickGetImageWidth := nil;
    pMagickGetImageHeight := nil;
    pMagickGetNumberImages := nil;
    pMagickGetImageFormat := nil;
  end;
  // Release pre-loaded dependency DLLs in reverse load order
  if Length(FDependencyHandles) > 0 then
  begin
    for DepIdx := High(FDependencyHandles) downto 0 do
      if FDependencyHandles[DepIdx] <> NilHandle then
        FreeLibrary(FDependencyHandles[DepIdx]);
    SetLength(FDependencyHandles, 0);
  end;
end;

function TImageMagickManager.GetException(wand: PMagickWand): String;
var
  Severity: Integer;
  Msg: PAnsiChar;
begin
  Result := '';
  if not Assigned(wand) or not Assigned(pMagickGetException) then Exit;
  Msg := pMagickGetException(wand, @Severity);
  if Assigned(Msg) then
  begin
    Result := String(Msg);
    if Assigned(pMagickRelinquishMemory) then
      pMagickRelinquishMemory(Msg);
  end;
end;

function TImageMagickManager.WandConvertBlob(Stream: TMemoryStream; const OutputFormat: String; Coalesce: Boolean; const InputFormat: String = ''): TMemoryStream;
var
  Wand: PMagickWand;
  CoalescedWand: PMagickWand;
  BlobData: PByte;
  BlobLength: Size_t;
  FormatUpper: AnsiString;
  CompressionInt: Integer;
  SavedCW: Word;
begin
  SavedCW := Get8087CW;
  Set8087CW($133F);
  try
    IMLog('WandConvertBlob: Start');
    Result := nil;
    if not Assigned(pNewMagickWand) then begin IMLog('WandConvertBlob: pNewMagickWand nil'); Exit; end;

  IMLog('WandConvertBlob: Creating Wand');
  Wand := pNewMagickWand();
  if Wand = nil then
  begin
    FLastError := 'Failed to create MagickWand';
    IMLog('WandConvertBlob: ' + FLastError);
    Exit;
  end;
  IMLog('WandConvertBlob: Wand created');

  CoalescedWand := nil;
  try
    IMLog('WandConvertBlob: Stream.Size=' + IntToStr(Stream.Size));
    IMLog('WandConvertBlob: Stream.Memory=' + IntToStr(NativeInt(Stream.Memory)));
    Stream.Position := 0;
    IMLog('WandConvertBlob: Calling pMagickReadImageBlob');
    if pMagickReadImageBlob(Wand, Stream.Memory, Stream.Size) <> MagickTrue then
    begin
      FLastError := 'MagickReadImageBlob failed: ' + GetException(Wand);
      IMLog('WandConvertBlob: ' + FLastError);
      Exit;
    end;
    IMLog('WandConvertBlob: ReadImageBlob OK');

    if Coalesce and Assigned(pMagickCoalesceImages) then
    begin
      IMLog('WandConvertBlob: Calling Coalesce');
      CoalescedWand := pMagickCoalesceImages(Wand);
      if CoalescedWand = nil then
      begin
        FLastError := 'MagickCoalesceImages failed: ' + GetException(Wand);
        IMLog('WandConvertBlob: ' + FLastError);
        Exit;
      end;
      pDestroyMagickWand(Wand);
      Wand := CoalescedWand;
      CoalescedWand := nil;
      IMLog('WandConvertBlob: Coalesce OK');
    end;

    if Assigned(pMagickResetIterator) then
      pMagickResetIterator(Wand);

    FormatUpper := AnsiString(UpperCase(OutputFormat));
    if Assigned(pMagickSetImageCompressionQuality) then
      pMagickSetImageCompressionQuality(Wand, FQuality);

    if Assigned(pMagickSetImageCompression) then
    begin
      CompressionInt := 0;
      if FCompression = 'LZW' then CompressionInt := 1
      else if FCompression = 'Zip' then CompressionInt := 2
      else if FCompression = 'JPEG' then CompressionInt := 3;
      pMagickSetImageCompression(Wand, CompressionInt);
    end;

    if pMagickSetFormat(Wand, PAnsiChar(FormatUpper)) <> MagickTrue then
    begin
      FLastError := 'MagickSetFormat (output) failed: ' + GetException(Wand);
      IMLog('WandConvertBlob: ' + FLastError);
      Exit;
    end;
    IMLog('WandConvertBlob: SetFormat=' + String(FormatUpper));

    if Assigned(pMagickGetImagesBlob) then
    begin
      IMLog('WandConvertBlob: Calling pMagickGetImagesBlob');
      BlobData := pMagickGetImagesBlob(Wand, @BlobLength);
      if (BlobData <> nil) and (BlobLength > 0) then
      begin
        IMLog('WandConvertBlob: GetImagesBlob OK (Size=' + IntToStr(BlobLength) + ')');
        Result := TMemoryStream.Create;
        try
          Result.WriteBuffer(BlobData^, BlobLength);
          Result.Position := 0;
        except
          FreeAndNil(Result);
        end;
      end
      else
      begin
        FLastError := 'MagickGetImagesBlob returned no data: ' + GetException(Wand);
        IMLog('WandConvertBlob: ' + FLastError);
      end;
    end
    else if Assigned(pMagickGetImageBlob) then
    begin
      IMLog('WandConvertBlob: Calling pMagickGetImageBlob');
      BlobData := pMagickGetImageBlob(Wand, @BlobLength);
      if (BlobData <> nil) and (BlobLength > 0) then
      begin
        IMLog('WandConvertBlob: GetImageBlob OK (Size=' + IntToStr(BlobLength) + ')');
        Result := TMemoryStream.Create;
        try
          Result.WriteBuffer(BlobData^, BlobLength);
          Result.Position := 0;
        except
          FreeAndNil(Result);
        end;
      end
      else
      begin
        FLastError := 'MagickGetImageBlob returned no data: ' + GetException(Wand);
        IMLog('WandConvertBlob: ' + FLastError);
      end;
    end
    else
    begin
      FLastError := 'No blob output function available';
      IMLog('WandConvertBlob: ' + FLastError);
    end;

    if Assigned(BlobData) and Assigned(pMagickRelinquishMemory) then
    try
      pMagickRelinquishMemory(BlobData);
    except
      IMLog('WandConvertBlob: Exception during RelinquishMemory (ignored)');
    end;
  finally
    try
      if Assigned(Wand) then
        pDestroyMagickWand(Wand);
    except
      IMLog('WandConvertBlob: Exception during Wand cleanup (ignored)');
    end;
    IMLog('WandConvertBlob: Finished');
    Set8087CW(SavedCW);
  end;
  finally
    Set8087CW(SavedCW);
  end;
end;

function TImageMagickManager.Identify(Stream: TStream; const InputFormat: String = ''): String;
var
  Wand: PMagickWand;
  InputMem: TMemoryStream;
  Width, Height, NumImages: Size_t;
  FormatStr: PAnsiChar;
begin
  Result := '';
  if not Assigned(pNewMagickWand) then Exit;
  
  InputMem := TMemoryStream.Create;
  try
    Stream.Position := 0;
    InputMem.CopyFrom(Stream, Stream.Size - Stream.Position);
    InputMem.Position := 0;
    
    Wand := pNewMagickWand();
    if Wand = nil then Exit;
    try
      if pMagickReadImageBlob(Wand, InputMem.Memory, InputMem.Size) <> MagickTrue then
      begin
        FLastError := 'MagickReadImageBlob failed: ' + GetException(Wand);
        Exit;
      end;
      
      Width := 0; Height := 0; NumImages := 0; FormatStr := nil;
      if Assigned(pMagickGetImageWidth) then Width := pMagickGetImageWidth(Wand);
      if Assigned(pMagickGetImageHeight) then Height := pMagickGetImageHeight(Wand);
      if Assigned(pMagickGetNumberImages) then NumImages := pMagickGetNumberImages(Wand);
      if Assigned(pMagickGetImageFormat) then FormatStr := pMagickGetImageFormat(Wand);
      
      Result := Format('Format: %s, Width: %d, Height: %d, Frames: %d', [String(FormatStr), Width, Height, NumImages]);
      if Assigned(FormatStr) and Assigned(pMagickRelinquishMemory) then pMagickRelinquishMemory(FormatStr);
    finally
      pDestroyMagickWand(Wand);
    end;
  finally
    InputMem.Free;
  end;
end;

function TImageMagickManager.ConvertStream(Stream: TStream; const OutputFormat: String; Coalesce: Boolean = False; const InputFormat: String = ''): TMemoryStream;
var
  InputMem: TMemoryStream;
begin
  Result := nil;
  IMLog('ConvertStream: Start');
  FLock.Acquire;
  IMLog('ConvertStream: Lock acquired');
  try
    FLastError := '';
    InputMem := TMemoryStream.Create;
    try
      Stream.Position := 0;
      IMLog('ConvertStream: Stream size=' + IntToStr(Stream.Size));
      InputMem.CopyFrom(Stream, Stream.Size - Stream.Position);
      InputMem.Position := 0;
      IMLog('ConvertStream: Calling WandConvertBlob');
      try
        Result := WandConvertBlob(InputMem, OutputFormat, Coalesce, InputFormat);
        IMLog('ConvertStream: WandConvertBlob returned ' + BoolToStr(Assigned(Result), True));
      except
        on E: Exception do
        begin
          if not Assigned(Result) then
          begin
            FLastError := 'WandConvertBlob exception: ' + E.Message;
            IMLog('ConvertStream: ' + FLastError);
          end
          else
          begin
            IMLog('ConvertStream: WandConvertBlob raised exception but Result is valid: ' + E.Message);
            FLastError := '';
          end;
        end;
      end;
    finally
      InputMem.Free;
    end;
  finally
    FLock.Release;
    IMLog('ConvertStream: Lock released');
  end;
end;

function TImageMagickManager.IsFormatSupported(const Format: String): Boolean;
begin
  Result := FSupportedFormats.IndexOf(LowerCase(Format)) <> -1;
end;

function TImageMagickManager.ConvertImage(InputFile, OutputDir: String): Boolean;
var
  SearchRec: TSearchRec;
  InputPath, OutputFile, Ext: String;
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
begin
  Result := False;
  if not DirectoryExists(OutputDir) then
  begin
    FLastError := 'Output directory not found: ' + OutputDir;
    Exit;
  end;

  // Limit concurrent conversions to FParallelConversions.
  // Each calling TTaskThread blocks here until a slot is available.
  WaitForSingleObject(FSemaphore, INFINITE);
  try
    if Pos('*', InputFile) > 0 then
    begin
      InputPath := ExtractFilePath(InputFile);
      if SysUtils.FindFirst(InputFile, faAnyFile, SearchRec) = 0 then
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            Ext := LowerCase(ExtractFileExt(SearchRec.Name));
            if Ext <> '' then Delete(Ext, 1, 1);
            OutputFile := OutputDir + ChangeFileExt(SearchRec.Name, '.' + FSaveAs);
            InputStream := TFileStream.Create(InputPath + SearchRec.Name, fmOpenRead or fmShareDenyWrite);
            try
              OutputStream := ConvertStream(InputStream, FSaveAs, False, Ext);
              if Assigned(OutputStream) then
              begin
                OutputStream.SaveToFile(OutputFile);
                OutputStream.Free;
              end;
            finally
              InputStream.Free;
            end;
          end;
        until SysUtils.FindNext(SearchRec) <> 0;
      finally
        SysUtils.FindClose(SearchRec);
      end;
      Result := True;
    end
    else if FileExists(InputFile) then
    begin
      Ext := LowerCase(ExtractFileExt(InputFile));
      if Ext <> '' then Delete(Ext, 1, 1);
      OutputFile := OutputDir + ChangeFileExt(ExtractFileName(InputFile), '.' + FSaveAs);
      InputStream := TFileStream.Create(InputFile, fmOpenRead or fmShareDenyWrite);
      try
        OutputStream := ConvertStream(InputStream, FSaveAs, False, Ext);
        if Assigned(OutputStream) then
        begin
          OutputStream.SaveToFile(OutputFile);
          OutputStream.Free;
          Result := True;
        end;
      finally
        InputStream.Free;
      end;
    end
    else
      FLastError := 'Input file not found: ' + InputFile;
  finally
    ReleaseSemaphore(FSemaphore, 1, nil);
  end;
end;

procedure TImageMagickManager.CacheSupportedFormats;
begin
  FSupportedFormats.Clear;
  FSupportedFormats.Delimiter := ',';
  FSupportedFormats.StrictDelimiter := True;
  FSupportedFormats.DelimitedText := 'bmp,gif,jpeg,jpg,png,tiff,webp,avif,heic,heif,jxl,ico,psd,svg,tga,jp2,raw,xcf,pcx,pgm,pbm,ppm,xpm,fax,g3,gif87';
end;

procedure TImageMagickManager.CacheCompressionTypes;
begin
  FCompressionTypes.Clear;
  FCompressionTypes.Add('None=0');
  FCompressionTypes.Add('LZW=1');
  FCompressionTypes.Add('Zip=2');
  FCompressionTypes.Add('JPEG=3');
end;

initialization
  TImageMagickManager.FInitialized := False;

finalization
  TImageMagickManager.Finalize;

end.
