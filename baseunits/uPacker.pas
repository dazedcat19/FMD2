{
        File: uPacker.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uPacker;

{$mode delphi}

interface

uses
  Classes, Zipper, zstream, SysUtils, uBaseUnit, Img2Pdf, FileUtil,
  LazFileUtils, SimpleException, uEpub, FMDOptions, process, MultiLog;

type
  TPackerFormat = (pfZIP, pfCBZ, pfPDF, pfEPUB);

  { TPacker }

  TPacker = class
  protected
    FSavedFileName, FExt: String;
    FFileList: TStringList;
    procedure FileFound(FileIterator: TFileIterator);
    function DoZipCbz: Boolean;
    function Do7Zip: Boolean;
    procedure DoPdf;
    procedure DoEpub;
  public
    Path,
    FileName: String;
    Format: TPackerFormat;
    CompressionQuality: Cardinal;
    function Execute: Boolean;
    property FileList: TStringList read FFileList;
    property SavedFileName: String read FSavedFileName;
    property Ext: String read FExt;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  frmMain;

procedure TPacker.FileFound(FileIterator: TFileIterator);
begin
  FFileList.Add(FileIterator.Filename);
end;

function TPacker.DoZipCbz: Boolean;
var
  i: Integer;
begin
  Result := False;

  with TZipper.Create do
  begin
    try
      try
        FileName := FSavedFileName;
        for i := 0 to FFileList.Count - 1 do
        begin
          with Entries.AddFileEntry(FFileList[i]) do
          begin
            CompressionLevel := clnone;
            ArchiveFileName := ExtractFileName(FFileList[i]);
          end;
        end;

        ZipAllFiles;
      except
        on E: Exception do
        begin
          E.Message := 'DoZipCbz.Exception'#13#10 + E.Message;
          SimpleException.ExceptionHandleSaveLogOnly(Self, E);
        end;
      end;
    finally
      Free;
    end;
  end;

  Result := FileExists(FSavedFileName);
  if Result then
  begin
    with TUnZipper.Create do
    begin
      try
         FileName := FSavedFileName;
         Examine;
         Result := FileList.Count = Entries.Count;

         if not Result then
         begin
           Logger.SendWarning('Some files failed to be compressed!');
         end;
      finally
        Free;
      end;
    end;
  end;
end;

Function MaybeQuoteIfNotQuoted(Const S : TProcessString) : TProcessString;

begin
  If (Pos(' ', S) <> 0) and (pos('"', S) = 0) then
  begin
    Result := '"' + S + '"';
  end
  else
  begin
     Result := S;
  end;
end;

function TPacker.Do7Zip: Boolean;
var
  p: TProcess;
  exit_status, i: Integer;
  s, sout, serr, fFileName,
  fUniqueTimestampName: string;
begin
  Result := False;
  p := TProcess.Create(nil);
  try
    if FileExists(FSavedFileName) then
    begin
      if not DeleteFile(FSavedFileName) then
      begin
        Logger.SendError(Self.ClassName + '.Do7Zip Error: failed to delete existing file ' + FSavedFileName);
        Exit;
      end;
    end;

    fUniqueTimestampName := StringReplace(ExtractFileName(FSavedFileName), ExtractFileExt(FSavedFileName), '', [rfReplaceAll])+ '_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now);
    fFileName := 'FQDNList_' + fUniqueTimestampName + '.txt';
    with TStringList.Create do
    try
      for i := 0 to FFileList.Count - 1 do
      begin
        FFileList[i] := MainForm.CheckLongNamePaths(FFileList[i]);
        Add(FFileList[i]);
      end;

      try
        SaveToFile(fFileName);
      except
        on E: EFCreateError do
        begin
          // If first attempt fails, try the second path
          try
            fFileName := AppendPathDelim(ExtractFilePath(FSavedFileName)) + fFileName;
            SaveToFile(fFileName);
          except
            on E2: Exception do
            begin
              MainForm.ExceptionHandler(Self, E2);
            end;
          end;
        end;
      end;
    finally
      Free;
    end;

    p.Executable := CURRENT_ZIP_EXE;
    with p.Parameters do begin
      Add('a');
      Add('-tzip');
      Add('-mx0');
      Add('-mmt' + IntToStr(CPUCount));
      Add('-sccUTF-8');
      Add('-scsUTF-8');
      Add('-stl');
      Add('-spd');
      Add('-slt');
      Add('-sse');
      Add('-sdel');
      Add(FSavedFileName);
      Add('@' + fFileName);
    end;

    sout := '';
    serr := '';
    p.ShowWindow := swoHIDE;
    p.RunCommandLoop(sout, serr, exit_status);
    Result := exit_status = 0;
    DeleteFile(fFileName);

    if not Result then
    begin
      serr := '';
      s := IntToStr(exit_status) + ' ';
      case exit_status of
        1: s += 'Warning';
        2: s += 'Fatal error';
        7: s += 'Command line error';
        8: s += 'Not enough memory operation';
        255: s += 'User stopped the process';
        else
          s := 'Unknown';
      end;

      s += ' ' + MaybeQuoteIfNotQuoted(p.Executable);
      for i := 0 to p.Parameters.Count-1 do
      begin
        s += ' ' + MaybeQuoteIfNotQuoted(p.Parameters[i]);
      end;

      Logger.SendError(Self.ClassName + '.Do7zip Error: ' + s);
    end;
  finally
    p.Free;
  end;
end;

procedure TPacker.DoPdf;
var
  pdf: TImg2Pdf;
  i: Cardinal;
  fstream: TFileStream;
begin
  try
    pdf := TImg2Pdf.Create;
    try
      pdf.CompressionQuality := CompressionQuality;
      pdf.Infos.Title := GetLastDir(Path);
      pdf.Infos.Creator := ApplicationName;

      for i := 0 to FFileList.Count - 1 do
      begin
        try
          pdf.AddImage(FFileList[i]);
        except
        end;
      end;

      fstream := TFileStream.Create(FSavedFileName, fmCreate);
      try
        pdf.SaveToStream(fstream);
      finally
        fstream.Free;
      end;
    finally
      pdf.Free;
    end;
  except
    on E: Exception do
    begin
      E.Message := 'DoPdf.Exception'#13#10 + E.Message;
      SimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
end;

procedure TPacker.DoEpub;
var
  epub: TEpubBuilder;
  i: Integer;
  fstream: TFileStream;
begin
  try
    epub := TEpubBuilder.Create;
    try
      epub.Title := GetLastDir(Path);

      for i := 0 to FFileList.Count - 1 do
      begin
        try
          epub.AddImage(FFileList[i]);
        except
        end;
      end;

      fstream := TFileStream.Create(FSavedFileName, fmCreate);
      try
        epub.SaveToStream(fstream);
      finally
        fstream.Free;
      end;
    finally
      epub.Free;
    end;
  except
    on E: Exception do
    begin
      E.Message := 'DoEpub.Exception'#13#10 + E.Message;
      SimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
end;

function TPacker.Execute: Boolean;
var
  i: Integer;
  packResult: Boolean;
begin
  Result := False;
  Path := CorrectPathSys(Path);

  if FFileList.Count = 0 then
  begin
    if not DirectoryExists(Path) then
    begin
      Exit;
    end;

    with TFileSearcher.Create do
      try
        OnFileFound := FileFound;
        Search(Self.Path, '*.jpg;*.png;*.gif;*.webp', False, False);
      finally
        Free;
      end;
  end;

  if FFileList.Count = 0 then
  begin
    Exit;
  end;

  FFileList.CustomSort(NaturalCustomSort);
  case Format of
    pfZIP: FExt := '.zip';
    pfCBZ: FExt := '.cbz';
    pfPDF: FExt := '.pdf';
    pfEPUB: FExt := '.epub';
  end;

  if FileName <> '' then
  begin
    FSavedFileName := FileName + FExt;
  end
  else
  begin
    FSavedFileName := TrimAndExpandFilename(Path) + FExt;
  end;

  if FileExists(FSavedFileName) then
  begin
    if not DeleteFile(FSavedFileName) then
    begin
      Exit;
    end;
  end;

  packResult:=True;
  case Format of
    pfZIP, pfCBZ: packResult:=Do7Zip;
    pfPDF: DoPdf;
    pfEPUB: DoEpub;
  end;

  if not packResult then
  begin
    Exit(False);
  end;

  Result := FileExists(FSavedFileName);
  if Result then
  begin
    if not (Format in [pfZIP, pfCBZ]) then // let 7za delete the files
    begin
      for i := 0 to FFileList.Count - 1 do
      begin
        DeleteFile(FFileList[i]);
      end;
    end;

    if IsDirectoryEmpty(Path) then
    begin
      RemoveDir(Path);
    end;
  end;
end;

constructor TPacker.Create;
begin
  FFileList := TStringList.Create;
  FSavedFileName := '';
  FExt := '';
  Path := '';
  FileName := '';
  Format := pfZIP;
end;

destructor TPacker.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

end.
