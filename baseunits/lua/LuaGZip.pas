unit LuaGZip;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  GZIPUtils, MultiLog, LuaUtils;

function lua_inflate(L: Plua_State): Integer; cdecl;
var
  data: PAnsiChar;
  len: NativeUInt;
  inStream, outStream: TMemoryStream;
begin
  Result := 0;
  try
    data := lua_tolstring(L, 1, @len);

    inStream := TMemoryStream.Create;
    outStream := TMemoryStream.Create;
    try
      if len > 0 then
        inStream.WriteBuffer(data^, len);

      if unzipStream(inStream, outStream) then
      begin
        lua_pushlstring(L, outStream.Memory, outStream.Size);
        Result := 1;
      end
      else
        Logger.SendError('GZip.Inflate() unzipStream failed');
    finally
      inStream.Free;
      outStream.Free;
    end;
  except
    on E: Exception do
      Logger.SendError('GZip.Inflate() ' + E.Message);
  end;
end;

const
  methods: packed array [0..1] of luaL_Reg = (
    (name: 'Inflate'; func: @lua_inflate),
    (name: nil; func: nil)
    );

function luaopen_gzip(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('gzip', @luaopen_gzip);

end.
