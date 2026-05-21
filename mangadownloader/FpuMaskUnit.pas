unit FpuMaskUnit;

{$mode objfpc}{$H+}

interface

implementation

uses
  SysUtils;

var
  Saved8087CW: Word;

initialization
  Saved8087CW := Get8087CW;
  Set8087CW($133F);

finalization
  Set8087CW(Saved8087CW);

end.
