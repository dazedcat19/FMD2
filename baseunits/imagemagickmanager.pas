unit ImageMagickManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synautil, StrUtils, StdCtrls, Process, Windows, Registry,
  LazFileUtils, SyncObjs, MultiLog;

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

    function FindMagickBinary: Boolean;
    function IdentifyCommand(const QueryCommand: String): TStringList;
    procedure CacheSupportedFormats;
    procedure CacheCompressionTypes;
    function ExecuteMagickCommand(const Params: array of String; TimeoutMS: Cardinal = 300000): Boolean;
    function StreamToString(Stream: TMemoryStream): String;
    constructor CreatePrivate;

    function GetPathFound: Boolean;
    function GetEnabled: Boolean;
    procedure SetEnabled(AEnabled: Boolean);
    function GetMogrify: Boolean;
    procedure SetMogrify(AMogrify: Boolean);
    function GetSupportedFormats: TStrings;
    function GetCompressionTypes: TStrings;
    procedure SetComboBoxList(AComboBox: TComboBox; AList: TStringList);
    function GetSaveAs: String;
    procedure SetSaveAs(ASaveAs: String);
    function GetQuality: Integer;
    function GetQualityString: String;
    procedure SetQuality(AQuality: Integer);
    function GetCompression: String;
    procedure SetCompression(ACompression: String);
    function GetLastError: String;
  public
    class function Instance: TImageMagickManager;
    class procedure Initialize;
    class procedure Finalize;

    function IsFormatSupported(const Format: String): Boolean;
    function ConvertImage(const InputFile, OutputDir: String): Boolean;

    property PathFound: Boolean read GetPathFound;
    property Enabled: Boolean read GetEnabled write SetEnabled; 
    property Mogrify: Boolean read GetMogrify write SetMogrify;
    property SupportedFormats: TStrings read GetSupportedFormats;
    property CompressionTypes: TStrings read GetCompressionTypes;
    property SaveAs: String read GetSaveAs write SetSaveAs;
    property Quality: Integer read GetQuality write SetQuality;
    property Compression: String read GetCompression write SetCompression;
    property LastError: String read GetLastError;
  end;

resourcestring
  RS_ImageMagickPreference = 'ImageMagick has been enabled!'#13#10'The following settings have been disabled because ImageMagick has taken preference.';
  RS_ImageMagickNotFoundHint = 'Failed to detect an install of ImageMagick!'#13#10'The following settings have been disabled because an install of ImageMagick was not detected.';

implementation

uses
  frmMain;

{ TImageMagickManager }

class function TImageMagickManager.Instance: TImageMagickManager;
begin
  if not FInitialized then
  begin
    Initialize;
  end;

  Result := FInstance;
end;

class procedure TImageMagickManager.Initialize;
begin
  if not FInitialized then
  begin
    FLock := TCriticalSection.Create;
    FInitialized := True;
    FPathFound := False;
    FEnabled := False;
    FMogrify := False;

    FInstance := TImageMagickManager.CreatePrivate;
  end;
end;

class procedure TImageMagickManager.Finalize;
begin
  if FInitialized then
  begin
    FreeAndNil(FInstance);
    FreeAndNil(FLock);
    FInitialized := False;
  end;
end;

procedure TImageMagickManager.SetComboBoxList(AComboBox: TComboBox; AList: TStringList);
var
  SelectedString: String;
  SelectedIndex: Integer;
begin
  SelectedIndex := AComboBox.ItemIndex;
  SelectedString := AComboBox.Items.ValueFromIndex[SelectedIndex];

  AComboBox.Clear;
  AComboBox.Items.AddStrings(AList);

  AComboBox.ItemIndex := AComboBox.Items.IndexOf(SelectedString);

  if (AComboBox.ItemIndex = -1) and (AComboBox.Items.Count >= 1) then
  begin
    AComboBox.ItemIndex := 0;
  end;
end;

constructor TImageMagickManager.CreatePrivate;
begin
  inherited Create;

  FSupportedFormats := TStringList.Create;
  FSupportedFormats.CaseSensitive := False;
  FCompressionTypes := TStringList.Create;
  FCompressionTypes.CaseSensitive := False;
  FQuality := 75;

  if not FindMagickBinary then
  begin
    MainForm.gbImageMagick.Hint := RS_ImageMagickNotFoundHint;
    Logger.Send(Self.ClassName + ': ImageMagick not found: ' + FLastError);
    Exit;
  end;

  FPathFound := True;
  CacheSupportedFormats;
  CacheCompressionTypes;

  with MainForm do
  begin
    gbImageMagick.Enabled := True;
    ckImageMagick.Enabled := True;
    cbImageMagickSaveAs.Enabled := True;
    cbImageMagickCompression.Enabled := True;
    seImageMagickQuality.Enabled := True;

    SetComboBoxList(cbImageMagickSaveAs, FSupportedFormats);
    SetComboBoxList(cbImageMagickCompression, FCompressionTypes);
  end;
end;

function TImageMagickManager.GetPathFound: Boolean;
begin
  Result := FPathFound;
end;

function TImageMagickManager.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TImageMagickManager.SetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

function TImageMagickManager.GetMogrify: Boolean;
begin
  Result := FMogrify;
end;    

procedure TImageMagickManager.SetMogrify(AMogrify: Boolean);
begin
  FMogrify := AMogrify;
end;

function TImageMagickManager.GetSupportedFormats: TStrings;
begin
  Result := FSupportedFormats;
end;

function TImageMagickManager.GetCompressionTypes: TStrings;
begin
  Result := FCompressionTypes;
end;

function TImageMagickManager.GetSaveAs: String;
begin
  Result := FSaveAs;
end;

procedure TImageMagickManager.SetSaveAs(ASaveAs: String);
begin
  FSaveAs := LowerCase(ASaveAs);
end;

function TImageMagickManager.GetQuality: Integer;
begin
  Result := FQuality;
end;

function TImageMagickManager.GetQualityString: String;
begin
  Result := FQuality.ToString;
end;

procedure TImageMagickManager.SetQuality(AQuality: Integer);
begin
  FQuality := AQuality;
end; 

function TImageMagickManager.GetCompression: String;
begin
  Result := FCompression;
end;

procedure TImageMagickManager.SetCompression(ACompression: String);
begin
  FCompression := ACompression;
end;

function TImageMagickManager.GetLastError: String;
begin
  FLock.Acquire;

  try
    Result := FLastError;
  finally
    FLock.Release;
  end;
end;

function TImageMagickManager.FindMagickBinary: Boolean;
var
  Reg: TRegistry;
  Paths: array of String;
  Path: String;
  Dummy: DWORD;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CommandLine: string;
begin
  Result := False;
  FMagickPath := '';
  FLastError := '';

  // 1. Check common installation paths
  Paths := [
    'C:\Program Files\ImageMagick\',
    'C:\Program Files (x86)\ImageMagick\',
    'C:\Program Files\ImageMagick-6.*.*\',
    'C:\Program Files (x86)\ImageMagick-6.*.*\',
    'C:\Program Files\ImageMagick-7.*.*\',
    'C:\Program Files (x86)\ImageMagick-7.*.*\',
    'C:\imagemagick\',
    GetCurrentDir + '\'
  ];

  for Path in Paths do
  begin
    if FileExists(Path + 'magick.exe') then
    begin
      FMagickPath := Path;
      Exit(True);
    end;
  end;

  // 2. Check registry
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('SOFTWARE\ImageMagick\Current', False) or
       Reg.OpenKey('SOFTWARE\WOW6432Node\ImageMagick\Current', False) then
    begin
      FMagickPath := IncludeTrailingPathDelimiter(Reg.ReadString('BinPath'));

      if FileExists(FMagickPath + 'magick.exe') then
      begin
        Exit(True);
      end;
    end;
  finally
    Reg.Free;
  end;

  // 3. Silent PATH checking using Windows API
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;

  CommandLine := 'magick -version';

  if CreateProcess(nil, PChar(CommandLine), nil, nil, False,
                   CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS,
                   nil, nil, StartupInfo, ProcessInfo) then
  begin
    try
      WaitForSingleObject(ProcessInfo.hProcess, 1000); // 1 second timeout
      GetExitCodeProcess(ProcessInfo.hProcess, Dummy);
      if Dummy = 0 then
      begin
        FMagickPath := ''; // Empty means use system PATH
        Exit(True);
      end;
    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  end;

  FLastError := 'Could not locate ImageMagick installation.';
end;

function TImageMagickManager.IdentifyCommand(const QueryCommand: String): TStringList;
var
  Process: TProcess;
  OutputStream: TMemoryStream;
  BytesRead: LongInt;
  Buffer: array[1..2048] of Byte;
begin
  Process := nil;
  OutputStream := nil;
  Result := TStringList.Create;

  try
    try
      // Set up the process 
      Process := TProcess.Create(nil);
      if FMagickPath <> '' then
      begin
        Process.Executable := FMagickPath + 'magick';
      end
      else
      begin
        Process.Executable := 'magick';
      end;

      Process.Parameters.Add('identify');
      Process.Parameters.Add('-list');
      Process.Parameters.Add(QueryCommand);
      Process.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      Process.ShowWindow := swoHIDE;

      // Execute the process
      Process.Execute;

      // Capture output while process is running
      OutputStream := TMemoryStream.Create;
      while Process.Running do
      begin
        BytesRead := Process.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
        begin
          OutputStream.Write(Buffer, BytesRead);
        end
        else
        begin
          Sleep(100); // Prevent busy waiting
        end;
      end;

      // Read any remaining output after process finishes
      repeat
        BytesRead := Process.Output.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
        begin
          OutputStream.Write(Buffer, BytesRead);
        end;
      until BytesRead <= 0;

      // Convert output to string list
      OutputStream.Position := 0;
      Result.LoadFromStream(OutputStream);
      
    except
      on E: Exception do
      begin
        FreeAndNil(Result);
        Raise;
      end;
    end;
  finally
    Process.Free;
    OutputStream.Free;
  end;
end;

procedure TImageMagickManager.CacheSupportedFormats;
var
  i, j: Integer;
  Line, FormatName, Capabilities: String;
  OutputList: TStringList;
  Parts: array of String;
  IsValid: Boolean;
begin
  FSupportedFormats.Clear;
  OutputList := IdentifyCommand('format');

  try
    for i := 0 to OutputList.Count - 1 do
    begin
      Line := Trim(OutputList[i]);

      // Skip empty lines or description lines (indented)
      if (Line = '') or (Line[1] = ' ') then
      begin
        Continue;
      end;

      // Split line into parts (handles multiple spaces)
      Parts := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
      if Length(Parts) < 3 then
      begin
        Continue;
      end;

      // Check capabilities are exactly 'rw-' or 'rw+'
      Capabilities := Parts[2];
      if (Capabilities <> 'rw-') and (Capabilities <> 'rw+') then
      begin
        Continue;
      end;

      // Get format name (first part), remove * if present
      FormatName := Parts[0];
      if (FormatName <> '') and (FormatName[High(FormatName)] = '*') then
      begin
        FormatName := Copy(FormatName, 1, Length(FormatName) - 1);
      end;

      // Validate format name is all uppercase letters
      IsValid := (Length(FormatName) >= 2);
      for j := 1 to Length(FormatName) do
      begin
        if not (FormatName[j] in ['A'..'Z']) then
        begin
          IsValid := False;
          Break;
        end;
      end;

      if IsValid then
      begin
        FSupportedFormats.Add(FormatName);
      end;
    end;

    // Final cleanup
    FSupportedFormats.Sorted := True;
    FSupportedFormats.Duplicates := dupIgnore;
  finally
    OutputList.Free;
  end;
end;

procedure TImageMagickManager.CacheCompressionTypes;
var
  OutputList: TStringList;
begin
  OutputList := IdentifyCommand('compress');

  try
    FCompressionTypes.Assign(OutputList);

    FCompressionTypes.Sorted := True;
    FCompressionTypes.Duplicates := dupIgnore;
  finally
    OutputList.Free;
  end;
end;

function TImageMagickManager.IsFormatSupported(const Format: String): Boolean;
begin
  FLock.Acquire;

  try
    Result := FSupportedFormats.IndexOf(Format) >= 0;
  finally
    FLock.Release;
  end;
end;

function TImageMagickManager.ExecuteMagickCommand(const Params: array of String; TimeoutMS: Cardinal = 300000): Boolean;
var
  Process: TProcess;
  Param, ErrorStreamOutput: String;
  ErrorStream: TMemoryStream;
  BytesRead: Integer;
  Buffer: array[0..4095] of Byte;
  CommandLine: string;
  StartTime: QWord;
  HasOutput: Boolean;
begin
  Result := False;
  FLock.Acquire;
  try
    Process := TProcess.Create(nil);
    ErrorStream := TMemoryStream.Create;
    try
      // Configure process
      if FMagickPath <> '' then
      begin
        Process.Executable := FMagickPath + 'magick';
      end
      else
      begin
        Process.Executable := 'magick';
      end;

      Process.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      Process.ShowWindow := swoHIDE;
      Process.PipeBufferSize := Length(Buffer);

      // Build command line
      CommandLine := Process.Executable;
      for Param in Params do
      begin
        Process.Parameters.Add(Param);
        CommandLine := CommandLine + ' ' + Param;
      end;

      Logger.Send(Self.ClassName + ': ' + CommandLine);

      try
        // Start process and timing
        Process.Execute;
        StartTime := GetTickCount64;
        HasOutput := False;

        // Monitoring loop
        while Process.Running do
        begin
          // Check for output
          BytesRead := Process.Output.Read(Buffer, SizeOf(Buffer));

          if BytesRead > 0 then
          begin
            ErrorStream.Write(Buffer, BytesRead);
            HasOutput := True;
            StartTime := GetTickCount64; // Reset timeout on activity
          end
          else
          begin
            // Check for timeout (no output for TimeoutMS)
            if GetTickCount64 - StartTime > TimeoutMS then
            begin
              Process.Terminate(255); // Custom timeout exit code
              FLastError := Format(
                'ImageMagick command timed out after %d ms' + sLineBreak +
                'Command: %s',
                [TimeoutMS, CommandLine]
              );

              Exit(False);
            end;
            Sleep(100); // Prevent CPU overload
          end;
        end;

        // Capture any remaining output after process exits
        repeat
          BytesRead := Process.Output.Read(Buffer, SizeOf(Buffer));

          if BytesRead > 0 then
          begin
            ErrorStream.Write(Buffer, BytesRead);
            HasOutput := True;
          end;
        until BytesRead <= 0;

        // Set results
        Result := (Process.ExitStatus = 0);

        if not Result then
        begin
          ErrorStream.Position := 0;
          ErrorStreamOutput := 'No output from process';

          if HasOutput then
          begin
             ErrorStreamOutput := StreamToString(ErrorStream);
          end;

          FLastError := Format(
            'ImageMagick command failed (Code %d)' + sLineBreak +
            'Command: %s' + sLineBreak +
            '%s',
            [Process.ExitStatus,
             CommandLine,
             ErrorStreamOutput]
          );
        end;

      except
        on E: Exception do
        begin
          FLastError := Format(
            'ImageMagick execution failed' + sLineBreak +
            'Command: %s' + sLineBreak +
            'Exception: %s',
            [CommandLine, E.Message]
          );

          Result := False;
        end;
      end;
    finally
      Process.Free;
      ErrorStream.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TImageMagickManager.StreamToString(Stream: TMemoryStream): String;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    Stream.Position := 0;
    StringStream.CopyFrom(Stream, Stream.Size);
    Result := Trim(StringStream.DataString);
  finally
    StringStream.Free;
  end;
end;

function TImageMagickManager.ConvertImage(const InputFile, OutputDir: String): Boolean;
var
  OutputFile, QuoteOutputDir: String;
begin
  FLock.Acquire;

  try
    OutputFile := QuoteStr((OutputDir + '%[filename:name].' + FSaveAs), '"');
    QuoteOutputDir := QuoteStr(ExcludeTrailingPathDelimiter(OutputDir), '"');

    Result := ExecuteMagickCommand([
      IFThen(FMogrify, 'mogrify', InputFile),
      '-quality', GetQualityString,
      '-compress', GetCompression,
      '-format', FSaveAs,
      IFThen(FMogrify, '-path', '+adjoin'),
      IFThen(FMogrify, QuoteOutputDir, '-set filename:name "%t"'),
      IFThen(FMogrify, InputFile, OutputFile)
    ]);
  finally
    FLock.Release;
  end;
end;

initialization
  TImageMagickManager.FInitialized := False;

finalization
  TImageMagickManager.Finalize;

end.
