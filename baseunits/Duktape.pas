unit Duktape;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Duktape.Api, FileCache, MultiLog;

function ExecJS(const text: String): String;

var
  DukLibDir: String = 'lua' + DirectorySeparator;

implementation

uses
  uBaseUnit, LazFileUtils;

var
  modules: TFileCache;

function duk_nativeprint(AContext: PDukContext): TDukRet; cdecl;
begin
  { Join all arguments together with spaces between them. }
  duk_push_string(AContext, ' ');
  duk_insert(AContext, 0);
  duk_join(AContext, duk_get_top(AContext) - 1);
  Logger.Send(duk_safe_to_string(AContext, -1));
  Result := 0;
end;

procedure duk_push_cfunction(const AContext: PDukContext; const aname: String;
  const func: TDukCFunction; const nargs: TDukIdx = DUK_VARARGS); inline;
begin
  duk_push_c_function(AContext, func, nargs);
  duk_put_global_string(AContext, PAnsiChar(aname));
end;

function duk_nativemodsearch(ctx: PDukContext): TDukRet; cdecl;
var
  id: String;
  o: TMemoryStream;
begin
  {* Nargs was given as 4 and we get the following stack arguments:
   *   index 0: id
   *   index 1: require
   *   index 2: exports
   *   index 3: module
   *}
  Result := 0;
  id := duk_safe_to_string(ctx, 0);

  o:=TMemoryStream(modules.Find(id));
  if o<>nil then
  try
    //TODO: can't load precompiled module?
    //duk_push_external_buffer(ctx);
    //duk_config_buffer(ctx,-1,o.Memory,o.Size);
    //duk_load_function(ctx);
    //duk_call(ctx,0);
    duk_push_lstring(ctx, o.Memory, o.Size);
    Result:=1;
  except
    on E: Exception do
      SendLogException('modSearch Error', E);
  end;
end;

procedure duk_registermodsearch(const ctx: PDukContext);
begin
  duk_get_global_string(ctx, 'Duktape');
  duk_push_c_function(ctx, @duk_nativemodsearch, 4);
  duk_put_prop_string(ctx, -2, 'modSearch');
  duk_pop(ctx);
end;

function ExecJS(const text: String): String;
var
  ctx: PDukContext;
  r: TDukInt;
  s: String;
begin
  Result := '';
  ctx := duk_create_heap_default;
  try
    if ctx = nil then
      raise Exception.Create('Failed to create a Duktape heap.');
    duk_module_duktape_init(ctx);
    duk_registermodsearch(ctx);
    duk_push_cfunction(ctx, 'print', @duk_nativeprint);

    duk_push_lstring(ctx, PAnsiChar(text), Length(text));
    r := duk_peval(ctx);
    s := duk_safe_to_string(ctx, -1);
    if r <> DUK_ERR_NONE then raise Exception.Create('Duktape error: ' + s)
    else
    begin
      if s <> 'undefined' then
        Result := s;
    end;
  finally
    duk_destroy_heap(ctx);
  end;
end;

function loadModuleFile(const AFileName: String): TObject;
var
  f: String;
  s: TMemoryStream;
  //ctx: PDukContext;
  //r: TDukInt;
  //buf: Pointer;
  //buflen: Integer;
begin
  Result:=nil;
  f:=CleanAndExpandFilename(DukLibDir+AFileName);
  if not FileExists(f) then
  begin
    f+='.js';
    if not FileExists(f) then Exit;
  end;
  s := TMemoryStream.Create;
  //try
    s.LoadFromFile(f);
    //TODO: load precompiled module?
    //ctx := duk_create_heap_default;
    //try
      //duk_push_lstring(ctx, PAnsiChar(f), Length(f));
      //r:=duk_compile_lstring_filename(ctx,DUK_COMPILE_SHEBANG,s.Memory,s.Size);
      //if r <> 0 then
      //  raise Exception.Create('');
      //duk_dump_function(ctx);
      //buf:=duk_require_buffer_data(ctx,-1,@buflen);
      //s.Clear;
      //s.Write(buf, buflen);
      Result := s;
      s := nil;
    //finally
      //duk_destroy_heap(ctx);
    //end;
  //except
  //  on E: Exception do
  //    SendLogException('require error: '+duk_safe_to_string(ctx, -1),E);
  //end;
  //if s<>nil then
  //  s.Free;
end;

initialization
  modules := TFileCache.Create(@loadModuleFile);

finalization
  modules.Free;

end.

