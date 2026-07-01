unit webp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MemBitmap, ImageMagickManager;

function IsWebPModuleLoaded: Boolean;
procedure InitWebPModule;
procedure DestroyWebPModule;
function WebPToMemBitmap(webp: TMemoryStream): TMemBitmap;
function WebPGetVersion: String;
function IsAnimatedWebP(webp: TMemoryStream): Boolean;

implementation

function IsWebPModuleLoaded: Boolean;
begin
  Result := TImageMagickManager.Instance.PathFound;
end;

procedure InitWebPModule;
begin
  TImageMagickManager.Initialize;
end;

procedure DestroyWebPModule;
begin
  TImageMagickManager.Finalize;
end;

function WebPToMemBitmap(webp: TMemoryStream): TMemBitmap;
var
  Output: TMemoryStream;
  imgMagick: TImageMagickManager;
begin
  Result := nil;
  if (webp = nil) or (webp.Size = 0) then Exit;
  
  imgMagick := TImageMagickManager.Instance;
  if not imgMagick.PathFound then Exit;
  
  Output := imgMagick.ConvertStream(webp, 'bmp', False, 'webp');
  if Assigned(Output) and (Output.Size > 0) then
  try
    Output.Position := 0;
    Result := TMemBitmap.Create(1, 1);
    try
      Result.LoadFromStream(Output);
    except
      FreeAndNil(Result);
    end;
  finally
    Output.Free;
  end;
end;

function WebPGetVersion: String;
begin
  Result := 'ImageMagick';
end;

function IsAnimatedWebP(webp: TMemoryStream): Boolean;
var
  IdentifyResult: String;
  imgMagick: TImageMagickManager;
  FramesPos: Integer;
begin
  Result := False;
  if (webp = nil) or (webp.Size < 21) then Exit;
  
  imgMagick := TImageMagickManager.Instance;
  if not imgMagick.PathFound then Exit;
  
  IdentifyResult := imgMagick.Identify(webp, 'webp');
  if IdentifyResult <> '' then
  begin
    FramesPos := Pos('Frames: ', IdentifyResult);
    if FramesPos > 0 then
    begin
      Result := StrToIntDef(Copy(IdentifyResult, FramesPos + Length('Frames: '), Length(IdentifyResult)), 1) > 1;
    end;
  end;
end;

initialization
finalization
end.
