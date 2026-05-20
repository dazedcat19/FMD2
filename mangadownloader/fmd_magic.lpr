program fmd_magic;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, ImageMagickManager;

procedure ListFormats;
const
  CommonFormats: array[0..29] of String = (
    'BMP', 'GIF', 'JPEG', 'JPG', 'PNG', 'APNG', 'TIFF', 'TIF',
    'WEBP', 'AVIF', 'JXL', 'HEIC', 'HEIF', 'ICO', 'PSD', 'SVG',
    'TGA', 'JP2', 'RAW', 'XCF', 'PCX', 'PGM', 'PBM', 'PPM',
    'XPM', 'FAX', 'G3', 'GIF87', 'PDF', 'EPS'
  );
var
  i: Integer;
begin
  WriteLn('Common Supported Formats:');
  WriteLn('---------------------------------------------------');
  for i := 0 to High(CommonFormats) do
    WriteLn(CommonFormats[i]);
  WriteLn;
  WriteLn('(Full format list requires magick.exe CLI)');
end;

var
  InputFile, OutputFile, InputExt, OutputFormat: String;
  Stream, Output: TMemoryStream;
  ImgMagick: TImageMagickManager;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: fmd_magic <output_format> <input_file> <output_file>');
    WriteLn('       fmd_magic -identify <input_file>');
    WriteLn('       fmd_magic --support');
    WriteLn('Examples:');
    WriteLn('  fmd_magic png cover.tif cover.png');
    WriteLn('  fmd_magic gif animated.webp animated.gif');
    WriteLn('  fmd_magic -identify cover.tif');
    WriteLn('  fmd_magic --support');
    Exit;
  end;

  TImageMagickManager.Initialize;
  try
    ImgMagick := TImageMagickManager.Instance;
    if not ImgMagick.PathFound then
    begin
      WriteLn('ERROR: ImageMagick not found!');
      Exit;
    end;

    if ParamStr(1) = '--support' then
    begin
      ListFormats;
    end
    else if ParamStr(1) = '-identify' then
    begin
      if ParamCount < 2 then
      begin
        WriteLn('ERROR: Missing input file for -identify');
        Exit;
      end;
      InputFile := ExpandFileName(ParamStr(2));
      if not FileExists(InputFile) then
      begin
        WriteLn('ERROR: Input file not found: ' + InputFile);
        Exit;
      end;

      Stream := TMemoryStream.Create;
      try
        Stream.LoadFromFile(InputFile);
        WriteLn(ImgMagick.Identify(Stream, LowerCase(ExtractFileExt(InputFile))));
      finally
        Stream.Free;
      end;
    end
    else
    begin
      if ParamCount < 3 then
      begin
        WriteLn('ERROR: Missing parameters. Usage: fmd_magic <output_format> <input_file> <output_file>');
        Exit;
      end;

      OutputFormat := ParamStr(1);
      InputFile := ExpandFileName(ParamStr(2));
      OutputFile := ExpandFileName(ParamStr(3));
      InputExt := LowerCase(ExtractFileExt(InputFile));
      if InputExt <> '' then Delete(InputExt, 1, 1);

      WriteLn('Input: ' + InputFile);
      WriteLn('Output: ' + OutputFile);
      WriteLn('Format: ' + OutputFormat);
      WriteLn;

      if not FileExists(InputFile) then
      begin
        WriteLn('ERROR: Input file not found: ' + InputFile);
        Exit;
      end;

      Stream := TMemoryStream.Create;
      try
        Stream.LoadFromFile(InputFile);
        WriteLn('Loaded stream, size=' + IntToStr(Stream.Size));
        WriteLn;

        WriteLn('Calling ConvertStream...');
        Output := ImgMagick.ConvertStream(Stream, OutputFormat, True, InputExt);

        WriteLn;
        if Assigned(Output) and (Output.Size > 0) then
        begin
          Output.SaveToFile(OutputFile);
          WriteLn('SUCCESS! Output: ' + OutputFile);
          WriteLn('Output size: ' + IntToStr(Output.Size) + ' bytes');
          Output.Free;
        end
        else
          WriteLn('FAILED: ' + ImgMagick.LastError);
      finally
        Stream.Free;
      end;
    end;
  finally
    TImageMagickManager.Finalize;
  end;
end.
