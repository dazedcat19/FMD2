unit QuickJS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, QuickJS.Api, MultiLog;

function ExecJS(const text: String): String;

var
  QJSLibDir: String = 'lua' + DirectorySeparator;

implementation

uses FileCache, uBaseUnit, LazFileUtils;

var
  modules: TFileCache;

function qjs_nativeprint(ctx: JSContext; this_val: JSValueConst; argc: Integer; argv: PJSValueConstArr): JSValue; cdecl;
var
  i: Integer;
  strVal: PChar;
  OutputStr: String;
begin
  OutputStr := '';
  for i := 0 to argc - 1 do
  begin
    strVal := JS_ToCString(ctx, argv^[i]);
    if strVal <> nil then
    begin
      if i > 0 then OutputStr := OutputStr + ' ';
      OutputStr := OutputStr + String(strVal);
      JS_FreeCString(ctx, strVal);
    end;
  end;
  Logger.Send(OutputStr);
  Result := JS_UNDEFINED;
end;

function qjs_module_loader(ctx: JSContext; module_name: PChar; opaque: Pointer): JSModuleDef; cdecl;
var
  id: String;
  o: TMemoryStream;
  func_val: JSValue;
begin
  Result := nil;
  id := String(module_name);
  o := TMemoryStream(modules.Find(id));
  if o <> nil then
  begin
    func_val := JS_Eval(ctx, PChar(o.Memory), o.Size, PChar(id), JS_EVAL_TYPE_MODULE or JS_EVAL_FLAG_COMPILE_ONLY);
    if not JS_IsException(func_val) then
    begin
      Result := JSModuleDef(JS_VALUE_GET_PTR(func_val));
    end
    else
      SendLogException('QuickJS Module Load Error', Exception.Create('Failed to compile module: ' + id));
  end;
end;

function ExecJS(const text: String): String;
var
  rt: JSRuntime;
  ctx: JSContext;
  global_obj, print_func, val, exception_val: JSValue;
  strVal: PChar;
begin
  Result := '';
  rt := JS_NewRuntime();
  if rt = nil then raise Exception.Create('Failed to create QuickJS runtime.');
  ctx := JS_NewContext(rt);
  try
    JS_SetModuleLoaderFunc(rt, nil, PJSModuleLoaderFunc(@qjs_module_loader), nil);
    global_obj := JS_GetGlobalObject(ctx);
    print_func := JS_NewCFunction(ctx, PJSCFunction(@qjs_nativeprint), 'print', 1);
    JS_SetPropertyStr(ctx, global_obj, 'print', print_func);
    JS_FreeValue(ctx, global_obj);
    val := JS_Eval(ctx, PChar(text), Length(text), '<eval>', JS_EVAL_TYPE_GLOBAL);
    if JS_IsException(val) then
    begin
      exception_val := JS_GetException(ctx);
      strVal := JS_ToCString(ctx, exception_val);
      Result := 'QuickJS error: ' + String(strVal);
      JS_FreeCString(ctx, strVal);
      JS_FreeValue(ctx, exception_val);
      raise Exception.Create(Result);
    end
    else if not JS_IsUndefined(val) then
    begin
      strVal := JS_ToCString(ctx, val);
      Result := String(strVal);
      JS_FreeCString(ctx, strVal);
    end;
    JS_FreeValue(ctx, val);
  finally
    JS_FreeContext(ctx);
    JS_FreeRuntime(rt);
  end;
end;

function loadModuleFile(const AFileName: String): TObject;
var
  f: String;
  s: TMemoryStream;
begin
  Result := nil;
  f := CleanAndExpandFilename(QJSLibDir + AFileName);
  if not FileExists(f) then
  begin
    f := f + '.js';
    if not FileExists(f) then Exit;
  end;
  s := TMemoryStream.Create;
  s.LoadFromFile(f);
  Result := s;
end;

initialization
  modules := TFileCache.Create(@loadModuleFile);

finalization
  modules.Free;

end.