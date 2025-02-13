{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CustomControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  uCustomControls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uCustomControls', @uCustomControls.Register);
end;

initialization
  RegisterPackage('CustomControls', @Register);
end.
