{
  FreePascal / Delphi bindings for QuickJS Engine.

  Copyright(c) 2019-2020 Coldzer0 <Coldzer0 [at] protonmail.ch>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

unit QuickJS.Api; // sync with version - "2020-04-12".

{$IfDef FPC}
  {$MODE Delphi}
  {$PackRecords C}
{$EndIf}

{$IfDef FPC}
  {$IfNDef windows}
    {$LinkLib 'libquickjs.a'}
  {$EndIf}
{$EndIf}

{$IfDef FPC}
  {$IfNDef CPU64}
    {$Define JS_NAN_BOXING}
  {$ENDIF}
{$ELSE}
   {$IfNDef CPUX64}
    {$Define JS_NAN_BOXING}
  {$ENDIF}
{$ENDIF}

interface

uses
  math;

{===============================================================================}
{                              QuickJS Constants                                }
{===============================================================================}
const
  QJS_VERSION = '2020-04-12';
const
  { all tags with a reference count are negative }
  JS_TAG_FIRST                = -11; { first negative tag }
  JS_TAG_BIG_DECIMAL          = -11;
  JS_TAG_BIG_INT              = -10;
  JS_TAG_BIG_FLOAT            = -9;
  JS_TAG_SYMBOL               = -8;
  JS_TAG_STRING               = -7;
  JS_TAG_MODULE               = -3; { used internally }
  JS_TAG_FUNCTION_BYTECODE    = -2; { used internally }
  JS_TAG_OBJECT               = -1;

  JS_TAG_INT                  = 0;
  JS_TAG_BOOL                 = 1;
  JS_TAG_NULL                 = 2;
  JS_TAG_UNDEFINED            = 3;
  JS_TAG_UNINITIALIZED        = 4;
  JS_TAG_CATCH_OFFSET         = 5;
  JS_TAG_EXCEPTION            = 6;
  JS_TAG_FLOAT64              = 7;
  { any larger tag is FLOAT64 if JS_NAN_BOXING }

  JS_FLOAT64_NAN = NaN;


const
  { flags for object properties }
  JS_PROP_CONFIGURABLE  = (1 shl 0);
  JS_PROP_WRITABLE      = (1 shl 1);
  JS_PROP_ENUMERABLE    = (1 shl 2);
  JS_PROP_C_W_E         = (JS_PROP_CONFIGURABLE or JS_PROP_WRITABLE or JS_PROP_ENUMERABLE);
  JS_PROP_LENGTH        = (1 shl 3); { used internally in Arrays }
  JS_PROP_TMASK         = (3 shl 4); { mask for NORMAL, GETSET, VARREF, AUTOINIT }
  JS_PROP_NORMAL        = (0 shl 4);
  JS_PROP_GETSET        = (1 shl 4);
  JS_PROP_VARREF        = (2 shl 4); { used internally }
  JS_PROP_AUTOINIT      = (3 shl 4); { used internally }

  { flags for JS_DefineProperty }
  JS_PROP_HAS_SHIFT        = 8;
  JS_PROP_HAS_CONFIGURABLE = (1 shl 8);
  JS_PROP_HAS_WRITABLE     = (1 shl 9);
  JS_PROP_HAS_ENUMERABLE   = (1 shl 10);
  JS_PROP_HAS_GET          = (1 shl 11);
  JS_PROP_HAS_SET          = (1 shl 12);
  JS_PROP_HAS_VALUE        = (1 shl 13);

  { throw an exception if false would be returned /
   (JS_DefineProperty/JS_SetProperty) }
  JS_PROP_THROW            = (1 shl 14);
  { throw an exception if false would be returned in strict mode /
     (JS_SetProperty) }
  JS_PROP_THROW_STRICT     = (1 shl 15);
  JS_PROP_NO_ADD           = (1 shl 16); { internal use }
  JS_PROP_NO_EXOTIC        = (1 shl 17); { internal use }

  JS_DEFAULT_STACK_SIZE    = (256 * 1024);

  { JS_Eval() flags }
  JS_EVAL_TYPE_GLOBAL      = (0 shl 0); { global code (default) }
  JS_EVAL_TYPE_MODULE      = (1 shl 0); { module code }
  JS_EVAL_TYPE_DIRECT      = (2 shl 0); { direct call (internal use) }
  JS_EVAL_TYPE_INDIRECT    = (3 shl 0); { indirect call (internal use) }
  JS_EVAL_TYPE_MASK        = (3 shl 0);

  JS_EVAL_FLAG_STRICT      = (1 shl 3); { force 'strict' mode }
  JS_EVAL_FLAG_STRIP       = (1 shl 4); { force 'strip' mode }
  (*
    compile but do not run. The result is an object with a
     JS_TAG_FUNCTION_BYTECODE or JS_TAG_MODULE tag. It can be executed
     with JS_EvalFunction().
  *)
  JS_EVAL_FLAG_COMPILE_ONLY = (1 shl 5); { internal use }

  { don't include the stack frames before this eval in the Error() backtraces }
  JS_EVAL_FLAG_BACKTRACE_BARRIER = (1 shl 6);

  { Object Writer/Reader (currently only used to handle precompiled code)  }
  JS_WRITE_OBJ_BYTECODE     = (1 shl 0); { allow function/module }
  JS_WRITE_OBJ_BSWAP        = (1 shl 1); { byte swapped output }

  JS_READ_OBJ_BYTECODE      = (1 shl 0); { allow function/module  }
  JS_READ_OBJ_ROM_DATA      = (1 shl 1); { avoid duplicating 'buf' data  }

  { C property definition }
  JS_DEF_CFUNC            = 0;
  JS_DEF_CGETSET          = 1;
  JS_DEF_CGETSET_MAGIC    = 2;
  JS_DEF_PROP_STRING      = 3;
  JS_DEF_PROP_INT32       = 4;
  JS_DEF_PROP_INT64       = 5;
  JS_DEF_PROP_DOUBLE      = 6;
  JS_DEF_PROP_UNDEFINED   = 7;
  JS_DEF_OBJECT           = 8;
  JS_DEF_ALIAS            = 9;


  { C function definition }
  { JSCFunctionEnum }
  JS_CFUNC_generic                   = 0;
  JS_CFUNC_generic_magic             = 1;
  JS_CFUNC_constructor               = 2;
  JS_CFUNC_constructor_magic         = 3;
  JS_CFUNC_constructor_or_func       = 4;
  JS_CFUNC_constructor_or_func_magic = 5;
  JS_CFUNC_f_f                       = 6;
  JS_CFUNC_f_f_f                     = 7;
  JS_CFUNC_getter                    = 8;
  JS_CFUNC_setter                    = 9;
  JS_CFUNC_getter_magic              = 10;
  JS_CFUNC_setter_magic              = 11;
  JS_CFUNC_iterator_next             = 12;

  JS_GPN_STRING_MASK  = (1 shl 0);
  JS_GPN_SYMBOL_MASK  = (1 shl 1);
  JS_GPN_PRIVATE_MASK = (1 shl 2);

  { only include the enumerable properties }
  JS_GPN_ENUM_ONLY = (1 shl 4);
  { set theJSPropertyEnum.is_enumerable field }
  JS_GPN_SET_ENUM  = (1 shl 5);

  { C Call Flags }

  JS_CALL_FLAG_CONSTRUCTOR = (1 shl 0);
{===============================================================================}
{===============================================================================}

type
  {$IFNDEF FPC}
    // Delphi Compatible.
    // Anything under XE4.
  {$IF (CompilerVersion <= 25)}
    PUint32 = ^Uint32; // PUint32 not defined in XE4 - Fix by @edwinyzh
  {$IFEND}
    pUInt8  = PByte;
    pInt8   = PShortInt;
    pInt16  = PSmallint;
    PInt32  = PLongint;
  {$ENDIF}
  {$ifdef cpu64}
    size_t  = QWord;
    psize_t = ^size_t;
  {$else}
    size_t  = Cardinal;
    psize_t = ^size_t;
  {$endif}

  JS_BOOL   = Boolean;
  JSRuntime = Pointer;

  PPJSContext = ^_PJSContext; // Pointer to Pointer.
  _PJSContext = ^_JSContext;
  _JSContext  = record end; // Empty record to mimic the JSContext.
  JSContext   = Pointer;

  JSObject  = Pointer;
  JSClass   = Pointer;

  JSModuleDef = Pointer;

  JSString  = Pointer;

  JSClassID  = UInt32;
  PJSClassID = ^JSClassID;

  JSAtom    = UInt32;

  JSCFunctionEnum = Integer;

  JSGCObjectHeader = Pointer;

type
  PJSRefCountHeader = ^JSRefCountHeader;
  JSRefCountHeader = record
      ref_count : Integer;
  end;

{$If Defined(JS_NAN_BOXING)}
  JSValue          = UInt64;
  PJSValue         = ^JSValue;
  JSValueConst     = JSValue;
  PJSValueConst    = ^JSValueConst;
  JSValueConstArr  = array[0..(MaxInt div SizeOf(JSValueConst))-1] of JSValueConst;
  PJSValueConstArr = ^JSValueConstArr;
const
  JS_FLOAT64_TAG_ADDEND =  $7ff80000 - JS_TAG_FIRST + 1; // quiet NaN encoding
  JS_NAN                = ($7ff8000000000000 - (JS_FLOAT64_TAG_ADDEND shl 32));
{$Else}
type
  JSValueUnion = record
      case byte of
      0 : (&int32 : int32);
      1 : (float64 : Double);
      2 : (Ptr : Pointer);
  end;

  JSValue = record
      u : JSValueUnion;
      tag : Int64;
  end;
  PJSValue         = ^JSValue;
  JSValueConst     = JSValue;
  PJSValueConst    = ^JSValueConst;
  JSValueConstArr  = array[0..(MaxInt div SizeOf(JSValueConst))-1] of JSValueConst;
  PJSValueConstArr = ^JSValueConstArr;
{$ENDIF}
type
  JSMallocState = record
      malloc_count,
      malloc_size,
      malloc_limit : size_t;
      opaque : Pointer;
  end;
  PJSMallocState = ^JSMallocState;

  //c_malloc = function (s : JSMallocState; size : UInt64) : Pointer;
  //Pc_malloc = ^c_malloc;
  // TODO: Check If funcs need to be Pointers or not. ^^^^^
  JSMallocFunctions = record
     js_malloc  : function (s : PJSMallocState; size : size_t) : Pointer; cdecl;
     js_free    : procedure (s : PJSMallocState; Ptr : Pointer); cdecl;
     js_realloc : function (s : PJSMallocState; Ptr : Pointer ; size : size_t) : Pointer; cdecl;
     js_malloc_usable_size : function (Ptr : Pointer) : size_t; cdecl;
  end;
  PJSMallocFunctions = ^JSMallocFunctions;

  PJSMemoryUsage = ^JSMemoryUsage;
  JSMemoryUsage = record
     malloc_size, malloc_limit, memory_used_size,
     malloc_count,
     memory_used_count,
     atom_count, atom_size,
     str_count, str_size,
     obj_count, obj_size,
     prop_count, prop_size,
     shape_count, shape_size,
     js_func_count, js_func_size, js_func_code_size,
     js_func_pc2line_count, js_func_pc2line_size,
     c_func_count, array_count,
     fast_array_count, fast_array_elements,
     binary_object_count, binary_object_size : Int64;
  end;

{===============================================================================}
{                        Native Functions Callbcaks                             }
{===============================================================================}

  PJSCFunction = ^JSCFunction;
  JSCFunction      = function (ctx : JSContext; this_val : JSValueConst;
    argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;

  PJSCFunctionMagic = ^JSCFunctionMagic;
  JSCFunctionMagic = function (ctx : JSContext; this_val : JSValueConst;
    argc : Integer; argv : PJSValueConst; magic : Integer): JSValue; cdecl;


  PJSCFunctionData = ^JSCFunctionData;
  JSCFunctionData  = function (ctx : JSContext; this_val : JSValueConst;
    argc : Integer; argv : PJSValueConst; magic : Integer;
    func_data : PJSValue ): JSValue; cdecl;

{===============================================================================}

  PJS_MarkFunc = ^JS_MarkFunc;
  JS_MarkFunc = procedure (rt : JSRuntime; gp : JSGCObjectHeader); cdecl;

  PJSClassFinalizer = ^JSClassFinalizer;
  JSClassFinalizer  = procedure (rt : JSRuntime; val : JSValue); cdecl;

  PJSClassGCMark   = ^JSClassGCMark;
  JSClassGCMark    = procedure (rt : JSRuntime; val : JSValueConst; mark_func: PJS_MarkFunc); cdecl;

  PJSClassCall     = ^JSClassCall;
  JSClassCall      = function (ctx : JSContext;
                              func_obj : JSValueConst;
                              this_val : JSValueConst;
                              argc : Integer; argv : PJSValueConst;
                              flags : Integer) : JSValue; cdecl;

  PJSFreeArrayBufferDataFunc = ^JSFreeArrayBufferDataFunc;
  JSFreeArrayBufferDataFunc  = procedure(rt : JSRuntime; opaque, Ptr : Pointer); cdecl;

  { return != 0 if the JS code needs to be interrupted }
  PJSInterruptHandler = ^JSInterruptHandler;
  JSInterruptHandler  = function (rt : JSRuntime; opaque : Pointer): integer; cdecl;

  { return the module specifier (allocated with js_malloc()) or NULL if exception }
  PJSModuleNormalizeFunc = ^JSModuleNormalizeFunc;
  JSModuleNormalizeFunc  = function (ctx : JSContext;
                              const module_base_name , module_name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
                              opaque : Pointer): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl;


  PJSModuleLoaderFunc = ^JSModuleLoaderFunc;
  JSModuleLoaderFunc  = function (ctx : JSContext; module_name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; opaque : Pointer) : JSModuleDef; cdecl;

  { JS Job support }
  PJSJobFunc = ^JSJobFunc;
  JSJobFunc  = function (ctx : JSContext; argc : Integer; argv : PJSValueConst): JSValue; cdecl;


  { C module definition }
  PJSModuleInitFunc = ^JSModuleInitFunc;
  JSModuleInitFunc  = function (ctx : JSContext; m : JSModuleDef): Integer; cdecl;

  { Promises RejectionTracker CallBack }

  { is_handled = TRUE means that the rejection is handled  }
  PJSHostPromiseRejectionTracker = ^JSHostPromiseRejectionTracker;
  JSHostPromiseRejectionTracker = procedure(ctx : JSContext;
                              promise, reason :JSValueConst;
                              is_handled : JS_BOOL; opaque : Pointer); cdecl;
{===============================================================================}

  { object class support }
  PPJSPropertyEnum = ^PJSPropertyEnum;
  PJSPropertyEnum = ^JSPropertyEnum;
  JSPropertyEnum = record
     is_enumerable : JS_BOOL;
     atom : JSAtom;
  end;

  PJSPropertyDescriptor = ^JSPropertyDescriptor;
  JSPropertyDescriptor = record
     flags : Integer;
     value,
     getter,
     setter : JSValue;
  end;

  PJSClassExoticMethods = ^JSClassExoticMethods;
  JSClassExoticMethods = record
    { Return -1 if exception (can only happen in case of Proxy object),
       FALSE if the property does not exists, TRUE if it exists. If 1 is
       returned, the property descriptor 'desc' is filled if != NULL. }
    get_own_property : function (ctx: JSContext; desc: PJSPropertyDescriptor; obj:JSValueConst; prop:JSAtom):Integer;cdecl;

    { '*ptab' should hold the '*plen' property keys. Return 0 if OK,
       -1 if exception. The 'is_enumerable' field is ignored. }
    get_own_property_names : function (ctx: JSContext; ptab:PPJSPropertyEnum; plen: pUInt32; obj:JSValueConst):Integer;cdecl;

    { return < 0 if exception, or TRUE/FALSE }
    delete_property : function (ctx: JSContext; obj:JSValueConst; prop:JSAtom):Integer;cdecl;

    { return < 0 if exception or TRUE/FALSE }
    define_own_property : function (ctx: JSContext; this_obj:JSValueConst; prop:JSAtom; val:JSValueConst; getter:JSValueConst;
                 setter:JSValueConst; flags:Integer):Integer;cdecl;

    { The following methods can be emulated with the previous ones,
       so they are usually not needed }

    { return < 0 if exception or TRUE/FALSE }
    has_property : function (ctx: JSContext; obj:JSValueConst; atom:JSAtom):Integer;cdecl;
    get_property : function (ctx: JSContext; obj:JSValueConst; atom:JSAtom; receiver:JSValueConst):JSValue;cdecl;
    set_property : function (ctx: JSContext; obj:JSValueConst; atom:JSAtom; value:JSValueConst; receiver:JSValueConst;
                   flags:Integer):Integer;cdecl;
    end;

  PJSClassDef = ^JSClassDef;
  JSClassDef = record
    class_name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
    finalizer : PJSClassFinalizer;
    gc_mark : PJSClassGCMark;
    {
      if call != NULL, the object is a function. If (flags &
             JS_CALL_FLAG_CONSTRUCTOR) != 0, the function is called as a
             constructor. In this case, 'this_val' is new.target. A
             constructor call only happens if the object constructor bit is
             set (see JS_SetConstructorBit())
    }
    call : PJSClassCall;
    { XXX: suppress this indirection ? It is here only to save memory
       because only a few classes need these methods }
    exotic : PJSClassExoticMethods;
  end;

  { C function definition }

  constructor_magic_func = function (ctx: JSContext; new_target:JSValueConst; argc:Integer; argv:PJSValueConst;
                              magic:Integer):JSValue; cdecl;
  f_f_func    = function (_para1:double):double cdecl;
  f_f_f_func  = function (_para1:double; _para2:double):double; cdecl;
  Getter_func = function (ctx: JSContext; this_val:JSValueConst):JSValue; cdecl;
  Setter_func = function (ctx: JSContext; this_val:JSValueConst; val:JSValueConst):JSValue;cdecl;
  getter_magic_func  = function (ctx: JSContext; this_val:JSValueConst; magic:Integer):JSValue; cdecl;
  setter_magic_func  = function (ctx: JSContext; this_val:JSValueConst; val:JSValueConst; magic:Integer):JSValue; cdecl;
  iterator_next_func = function (ctx: JSContext; this_val:JSValueConst; argc:Integer; argv:PJSValueConst; pdone:PInteger;
                               magic:Integer):JSValue; cdecl;
  JSCFunctionType = record
    case Integer of
      0 : ( generic : JSCFunction );
      1 : ( generic_magic :  JSCFunctionMagic);
      2 : ( &constructor : JSCFunction );
      3 : ( constructor_magic : constructor_magic_func);
      4 : ( constructor_or_func : JSCFunction );
      5 : ( f_f : f_f_func);
      6 : ( f_f_f : f_f_f_func);
      7 : ( getter : Getter_func);
      8 : ( setter : Setter_func);
      9 : ( getter_magic  : getter_magic_func);
      10 : ( setter_magic : setter_magic_func);
      11 : ( iterator_next : iterator_next_func);
  end;
  PJSCFunctionType = ^JSCFunctionType;

  { C property definition }
  JSCFunctionListEntry = record
    name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
    prop_flags : UInt8;
    def_type : UInt8;
    magic : Int16;
    u : record
    case Integer of
      0 : ( func : record
          length : UInt8; { XXX: should move outside union }
          cproto : UInt8; { XXX: should move outside union }
          cfunc : JSCFunctionType;
        end );
      1 : ( getset : record
          get : JSCFunctionType;
          _set : JSCFunctionType;
        end );
      2 : ( alias : record
          name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
          base : Integer;
        end );
      3 : ( prop_list : record
          tab : ^JSCFunctionListEntry;
          len : Integer;
        end );
      4 : ( str : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf} );
      5 : ( i32 : Int32 );
      6 : ( i64 : Int64 );
      7 : ( f64 : double );
    end;
  end;
  PJSCFunctionListEntry = ^JSCFunctionListEntry;

  {$IFDEF mswindows}const QJSDLL = {$IfDef WIN64}'quickjs64.dll'{$Else}'quickjs32.dll'{$EndIf};{$endif}
  { QuickJS external APIs }

  function  JS_NewRuntime : JSRuntime; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  (* info lifetime must exceed that of rt *)
  procedure JS_SetRuntimeInfo(rt : JSRuntime; const info : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_SetMemoryLimit(rt : JSRuntime; limit : size_t); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_SetGCThreshold(rt : JSRuntime; gc_threshold : size_t); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_SetMaxStackSize(ctx: JSContext; stack_size:size_t); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function  JS_NewRuntime2(const mf : PJSMallocFunctions; opaque : Pointer) : JSRuntime; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_FreeRuntime(rt : JSRuntime); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function  JS_GetRuntimeOpaque(rt : JSRuntime) : Pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_SetRuntimeOpaque(rt : JSRuntime; opaque : Pointer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};


  procedure JS_MarkValue(rt:JSRuntime; val:JSValueConst; mark_func:PJS_MarkFunc);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_RunGC(rt:JSRuntime);  cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_IsLiveObject(rt:JSRuntime; obj:JSValueConst):JS_BOOL; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  //{REMOVE}function JS_IsInGCSweep(rt:JSRuntime):JS_BOOL; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewContext(rt:JSRuntime):JSContext; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_FreeContext(s: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DupContext(ctx : JSContext) : JSContext; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetContextOpaque(ctx: JSContext):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_SetContextOpaque(ctx: JSContext; opaque:pointer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetRuntime(ctx: JSContext):JSRuntime; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_SetClassProto(ctx: JSContext; class_id:JSClassID; obj:JSValue); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetClassProto(ctx: JSContext; class_id:JSClassID):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

{ the following functions are used to select the intrinsic object to save memory  }

  function JS_NewContextRaw(rt: JSRuntime): JSContext; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicBaseObjects(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicDate(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicEval(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicStringNormalize(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicRegExpCompiler(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicRegExp(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicJSON(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicProxy(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicMapSet(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicTypedArrays(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicPromise(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  procedure JS_AddIntrinsicBigInt(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicBigFloat(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_AddIntrinsicBigDecimal(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  { enable operator overloading }
  procedure JS_AddIntrinsicOperators(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  { enable "use math" }
  procedure JS_EnableBignumExt(ctx: JSContext; enable : JS_BOOL); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function js_string_codePointRange(ctx: JSContext; this_val:JSValueConst; argc:Integer; argv:PJSValueConst):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function js_malloc_rt(rt: JSRuntime; size:size_t):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_free_rt(rt: JSRuntime; ptr:pointer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_realloc_rt(rt: JSRuntime; ptr:pointer; size:size_t):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_malloc_usable_size_rt(rt: JSRuntime; ptr:pointer):size_t; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_mallocz_rt(rt: JSRuntime; size:size_t):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_malloc(ctx: JSContext; size:size_t):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_free(ctx: JSContext; ptr:pointer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_realloc(ctx: JSContext; ptr:pointer; size:size_t):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_malloc_usable_size(ctx: JSContext; ptr:pointer):size_t; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_realloc2(ctx: JSContext; ptr:pointer; size:size_t; pslack:Psize_t):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_mallocz(ctx: JSContext; size:size_t):pointer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_strdup(ctx: JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function js_strndup(ctx: JSContext; s:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; n:size_t): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  procedure JS_ComputeMemoryUsage(rt: JSRuntime; s:PJSMemoryUsage); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_DumpMemoryUsage(fp: Pointer; s:PJSMemoryUsage; rt: JSRuntime); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { atom support }

  function JS_NewAtomLen(ctx: JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; len:size_t):JSAtom; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewAtom(ctx: JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSAtom; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewAtomUInt32(ctx: JSContext; n:UInt32):JSAtom; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DupAtom(ctx: JSContext; v:JSAtom):JSAtom; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_FreeAtom(ctx: JSContext; v:JSAtom); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_FreeAtomRT(rt: JSRuntime; v:JSAtom); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_AtomToValue(ctx: JSContext; atom:JSAtom):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_AtomToString(ctx: JSContext; atom:JSAtom):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_AtomToCString(ctx: JSContext; atom:JSAtom):{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ValueToAtom(ctx: JSContext; val:JSValueConst) : JSAtom; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { object class support }

  function JS_NewClassID(pclass_id:PJSClassID):JSClassID; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewClass(rt: JSRuntime; class_id:JSClassID; class_def: PJSClassDef):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsRegisteredClass(rt: JSRuntime; class_id:JSClassID):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};


  { JS Numbers }

  function JS_NewBigInt64 (ctx : JSContext; v : Int64): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewBigUint64 (ctx : JSContext; v : UInt64): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};


  function JS_Throw(ctx: JSContext; obj:JSValue):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetException(ctx: JSContext):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsError(ctx: JSContext; val:JSValueConst):JS_BOOL; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_ResetUncatchableError(ctx: JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewError(ctx: JSContext):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ThrowSyntaxError(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ThrowTypeError(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ThrowReferenceError(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ThrowRangeError(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ThrowInternalError(ctx: JSContext; fmt : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; args : Array of Const): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ThrowOutOfMemory(ctx: JSContext): JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  procedure __JS_FreeValue(ctx: JSContext; v : JSValue); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure __JS_FreeValueRT(rt: JSRuntime; v : JSValue); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { JS Values - return -1 for JS_EXCEPTION }

  function JS_ToBool(ctx: JSContext; val:JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ToInt32(ctx: JSContext; pres:pInt32; val:JSValueConst):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_ToInt64(ctx: JSContext; pres:PInt64; val:JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ToIndex(ctx: JSContext; plen:PUInt64; val:JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ToFloat64(ctx: JSContext; pres:PDouble; val:JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  { return an exception if 'val' is a Number }
  function JS_ToBigInt64(ctx: JSContext; pres:PInt64; val:JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  { same as JS_ToInt64() but allow BigInt }
  function JS_ToInt64Ext(ctx: JSContext; pres:PInt64; val:JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_NewStringLen(ctx:JSContext; str1:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; len1: size_t):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewString(ctx:JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewAtomString(ctx:JSContext; str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ToString(ctx:JSContext; val:JSValueConst):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ToPropertyKey(ctx:JSContext; val:JSValueConst):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ToCStringLen2(ctx:JSContext; plen:psize_t; val1:JSValueConst; cesu8:JS_BOOL): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};



  procedure JS_FreeCString(ctx:JSContext; ptr:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewObjectProtoClass(ctx:JSContext; proto:JSValueConst; class_id:JSClassID):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewObjectClass(ctx:JSContext; class_id:JSClassID):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewObjectProto(ctx:JSContext; proto:JSValueConst):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewObject(ctx:JSContext):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsFunction(ctx:JSContext; val:JSValueConst):JS_BOOL; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsConstructor(ctx:JSContext; val:JSValueConst):JS_BOOL; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_SetConstructorBit(ctx:JSContext; func_obj : JSValueConst; val:JS_BOOL):JS_BOOL; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewArray(ctx:JSContext):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsArray(ctx:JSContext; val:JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetPropertyInternal(ctx:JSContext; obj:JSValueConst; prop:JSAtom;
                              receiver:JSValueConst; throw_ref_error:JS_BOOL):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_GetPropertyStr(ctx:JSContext; this_obj:JSValueConst; prop:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetPropertyUint32(ctx:JSContext; this_obj:JSValueConst; idx:UInt32):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_SetPropertyInternal(ctx:JSContext; this_obj:JSValueConst;
                              prop:JSAtom; val:JSValue; flags:Integer):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_SetPropertyUint32(ctx:JSContext; this_obj:JSValueConst; idx:UInt32; val:JSValue):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_SetPropertyInt64(ctx:JSContext; this_obj:JSValueConst; idx:Int64; val:JSValue):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_SetPropertyStr(ctx:JSContext; this_obj:JSValueConst; prop:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val:JSValue):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_HasProperty(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsExtensible(ctx:JSContext; obj:JSValueConst):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_PreventExtensions(ctx:JSContext; obj:JSValueConst):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DeleteProperty(ctx:JSContext; obj:JSValueConst; prop:JSAtom; flags:Integer):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_SetPrototype(ctx:JSContext; obj:JSValueConst; proto_val:JSValueConst):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetPrototype(ctx:JSContext; val:JSValueConst):JSValueConst;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_GetOwnPropertyNames(ctx: JSContext; ptab:PPJSPropertyEnum; plen: pUInt32; obj:JSValueConst; flags : Integer): Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetOwnProperty(ctx: JSContext; desc : PJSPropertyDescriptor; obj : JSValueConst; prop : JSAtom): Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { 'buf' must be zero terminated i.e. buf[buf_len] := #0.  }
  function JS_ParseJSON(ctx:JSContext; buf:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; buf_len:size_t; filename:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_JSONStringify(ctx:JSContext; obj, replacer, space0 : JSValueConst):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_Call(ctx:JSContext; func_obj:JSValueConst; this_obj:JSValueConst; argc:Integer; argv:PJSValueConstArr):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_Invoke(ctx:JSContext; this_val:JSValueConst; atom:JSAtom; argc:Integer; argv:PJSValueConst):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_CallConstructor(ctx:JSContext; func_obj:JSValueConst; argc:Integer; argv:PJSValueConst):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_CallConstructor2(ctx:JSContext; func_obj:JSValueConst; new_target:JSValueConst; argc:Integer; argv:PJSValueConst):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DetectModule(const input:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; input_len : size_t):JS_BOOL;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  { 'input' must be zero terminated i.e. buf[buf_len] := #0.  }
  function JS_Eval(ctx:JSContext; input:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; input_len:size_t; filename:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; eval_flags:Integer):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_EvalFunction(ctx:JSContext; fun_obj : JSValue):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetGlobalObject(ctx:JSContext):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsInstanceOf(ctx:JSContext; val:JSValueConst; obj:JSValueConst):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DefineProperty(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom; val:JSValueConst; getter:JSValueConst;
             setter:JSValueConst; flags:Integer):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_DefinePropertyValue(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom; val:JSValue; flags:Integer):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DefinePropertyValueUint32(ctx:JSContext; this_obj:JSValueConst; idx:UInt32; val:JSValue; flags:Integer):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DefinePropertyValueStr(ctx:JSContext; this_obj:JSValueConst; prop:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val:JSValue; flags:Integer):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_DefinePropertyGetSet(ctx:JSContext; this_obj:JSValueConst; prop:JSAtom; getter:JSValue; setter:JSValue;
             flags:Integer):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  procedure JS_SetOpaque(obj:JSValue; opaque:pointer);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetOpaque(obj:JSValueConst; class_id:JSClassID):pointer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetOpaque2(ctx:JSContext; obj:JSValueConst; class_id:JSClassID):pointer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_NewArrayBuffer(ctx:JSContext; buf:pUInt8; len:size_t; free_func:PJSFreeArrayBufferDataFunc; opaque:pointer;
             is_shared:JS_BOOL):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewArrayBufferCopy(ctx:JSContext; buf:pUInt8; len:size_t):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure JS_DetachArrayBuffer(ctx:JSContext; obj:JSValueConst);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetArrayBuffer(ctx:JSContext; psize:Psize_t; obj:JSValueConst):pUInt8;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_GetTypedArrayBuffer(ctx : JSContext; obj : JSValueConst;
             pbyte_offset, pbyte_length, pbytes_per_element : psize_t):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_NewPromiseCapability(ctx:JSContext; resolving_funcs:PJSValue):JSValue;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  procedure JS_SetHostPromiseRejectionTracker(rt: JSRuntime;
             cb : PJSHostPromiseRejectionTracker; opaque : Pointer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  procedure JS_SetInterruptHandler(rt:JSRuntime; cb:PJSInterruptHandler; opaque:pointer);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { if can_block is TRUE, Atomics.wait() can be used  }
  procedure JS_SetCanBlock(rt:JSRuntime; can_block:JS_BOOL);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { module_normalize = NULL is allowed and invokes the default module
     filename normalizer  }
  procedure JS_SetModuleLoaderFunc(rt:JSRuntime;
             module_normalize:PJSModuleNormalizeFunc;
             module_loader:PJSModuleLoaderFunc; opaque:pointer);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { JS Job support  }

  function JS_EnqueueJob(ctx:JSContext; job_func:PJSJobFunc; argc:Integer; argv:PJSValueConst):Integer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_IsJobPending(rt:JSRuntime):JS_BOOL;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  // TODO: Check pctx if the type is right.
  function JS_ExecutePendingJob(rt:JSRuntime; pctx: PPJSContext):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { Object Writer/Reader (currently only used to handle precompiled code)  }
  { allow function/module  }

  function JS_WriteObject(ctx: JSContext; psize:psize_t; obj:JSValueConst; flags:Integer):pUInt8; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_ReadObject(ctx: JSContext; buf:pUInt8; buf_len:size_t; flags:Integer):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  {
    load the dependencies of the module 'obj'. Useful when JS_ReadObject()
     returns a module.
  }
  function JS_ResolveModule(ctx: JSContext; obj : JSValueConst):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { C function definition }

  procedure JS_SetConstructor(ctx : JSContext; func_obj, proto : JSValueConst);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  function JS_NewCFunction2(ctx: JSContext; func:PJSCFunction; name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length:Integer; cproto:JSCFunctionEnum;
             magic:Integer):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_NewCFunctionData(ctx: JSContext; func:PJSCFunctionData; length:Integer; magic:Integer; data_len:Integer;
             data:PJSValueConst):JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  procedure JS_SetPropertyFunctionList(ctx: JSContext; obj:JSValueConst;
             tab:PJSCFunctionListEntry; len:Integer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { C module definition  }

  function JS_NewCModule(ctx: JSContext; name_str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; func:PJSModuleInitFunc): JSModuleDef; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { can only be called before the module is instantiated  }
  function JS_AddModuleExport(ctx: JSContext; m: JSModuleDef; name_str:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_AddModuleExportList(ctx: JSContext; m: JSModuleDef; tab:PJSCFunctionListEntry; len:Integer):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { can only be called after the module is instantiated  }
  function JS_SetModuleExport(ctx: JSContext; m: JSModuleDef; export_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val:JSValue):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_SetModuleExportList(ctx: JSContext; m: JSModuleDef; tab:PJSCFunctionListEntry; len:Integer):Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { return the import.meta object of a module }
  function JS_GetImportMeta(ctx: JSContext; m: JSModuleDef) : JSValue; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function JS_GetModuleName(ctx: JSContext; m: JSModuleDef) : JSAtom; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

  { QuickJS libc }
  function  js_init_module_std(ctx: JSContext; module_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSModuleDef;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function  js_init_module_os(ctx: JSContext; module_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}):JSModuleDef;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_std_add_helpers(ctx : JSContext; argc : Integer; argv : Pointer);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_std_loop(ctx : JSContext); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_std_free_handlers(rt:JSRuntime);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_std_dump_error(ctx:JSContext);cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function  js_load_file(ctx:JSContext; pbuf_len: psize_t; filename:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}): Pointer;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function  js_module_loader(ctx:JSContext; module_name:{$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; opaque:pointer):JSModuleDef;cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_std_eval_binary(ctx : JSContext; buf : Pointer; buf_len : size_t; flags : Integer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  function  js_module_set_import_meta(ctx : JSContext; func_val : JSValueConst; use_realpath, is_main : JS_BOOL) : Integer; cdecl; external {$IFDEF mswindows}QJSDLL{$endif};
  procedure js_std_promise_rejection_tracker(ctx : JSContext;
             promise, reason : JSValueConst; is_handled : JS_BOOL; opaque : Pointer); cdecl; external {$IFDEF mswindows}QJSDLL{$endif};

{ internal implementations}

function JS_VALUE_GET_TAG(v : JSValue): Int64;
{ same as JS_VALUE_GET_TAG, but return JS_TAG_FLOAT64 with NaN boxing }
function JS_VALUE_GET_NORM_TAG(v : JSValue): Int64;
function JS_VALUE_IS_NAN(v : JSValue) : JS_BOOL; inline;
function JS_VALUE_GET_INT(v : JSValue): Integer;
function JS_VALUE_GET_BOOL(v : JSValue): Boolean;
function JS_VALUE_GET_FLOAT64(v : JSValue): Double;
function JS_VALUE_GET_PTR(v : JSValue): Pointer;
function JS_MKVAL(tag : Int64; val : Int32): JSValue;
function JS_MKPTR(tag : Int64; ptr : Pointer): JSValue;
function JS_TAG_IS_FLOAT64(tag : Int64): Boolean; inline;
{$IfNDef JS_NAN_BOXING}
function JS_NAN : JSValue;
{$EndIf}
function __JS_NewFloat64({%H-}ctx : JSContext; d : Double): JSValue;

function JS_VALUE_IS_BOTH_INT(v1, v2 : JSValue): Boolean;
function JS_VALUE_IS_BOTH_FLOAT(v1, v2 : JSValue): Boolean;
function JS_VALUE_GET_OBJ(v : JSValue): JSObject;
function JS_VALUE_GET_STRING(v : JSValue): JSString;
function JS_VALUE_HAS_REF_COUNT(v : JSValue): Boolean;

{ special values }

function JS_NULL : JSValue;
function JS_UNDEFINED : JSValue;
function JS_FALSE : JSValue;
function JS_TRUE : JSValue;
function JS_EXCEPTION : JSValue;
function JS_UNINITIALIZED : JSValue;

{ value handling }

function JS_NewBool({%H-}ctx : JSContext; val : JS_BOOL): JSValue; inline;
function JS_NewInt32( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
function JS_NewCatchOffset( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
function JS_NewFloat64(ctx : JSContext; d : Double): JSValue;
function JS_IsBigInt(v : JSValueConst): JS_BOOL; inline;
function JS_IsBigFloat(v : JSValueConst): JS_BOOL; inline;
function JS_IsBigDecimal(v : JSValueConst): JS_BOOL; inline;
function JS_IsBool(v : JSValueConst): JS_BOOL; inline;
function JS_IsNull(v : JSValueConst): JS_BOOL; inline;
function JS_IsUndefined(v : JSValueConst): JS_BOOL; inline;
function JS_IsException(v : JSValueConst): JS_BOOL; inline;
function JS_IsUninitialized(v : JSValueConst): JS_BOOL; inline;
function JS_IsString(v : JSValueConst): JS_BOOL; inline;
function JS_IsNumber(v : JSValueConst): JS_BOOL; inline;
function JS_IsSymbol(v : JSValueConst): JS_BOOL; inline;
function JS_IsObject(v : JSValueConst): JS_BOOL; inline;

procedure JS_FreeValue(ctx : JSContext; v : JSValue); inline;
procedure JS_FreeValueRT(rt : JSRuntime; v : JSValue); inline;
function JS_DupValue({%H-}ctx : JSContext; v : JSValueConst) : JSValue; inline;
function JS_DupValueRT({%H-}rt : JSRuntime; v : JSValueConst) : JSValue; inline;

function JS_ToUint32(ctx : JSContext; pres : pUInt32; val : JSValueConst): Integer; inline;
function JS_ToCStringLen(ctx : JSContext; plen : psize_t; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
function JS_ToCString(ctx : JSContext; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
function JS_GetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom): JSValue; inline;
function JS_SetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom; val : JSValue): Integer; inline;

{ C function definition }

function JS_NewCFunction(ctx : JSContext; func : PJSCFunction; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer): JSValue; inline;
function JS_NewCFunctionMagic(ctx : JSContext; func : PJSCFunctionMagic; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum; magic : Integer): JSValue; inline;


{ C property definition }

function JS_CFUNC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; func : JSCFunction) : JSCFunctionListEntry;
function JS_CFUNC_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; func : JSCFunctionMagic; magic : Int16) : JSCFunctionListEntry;
function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; cproto : JSCFunctionEnum ; func : f_f_func) : JSCFunctionListEntry; overload;
function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; cproto : JSCFunctionEnum ; func : f_f_f_func) : JSCFunctionListEntry; overload;
function JS_ITERATOR_NEXT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer; iterator_next : iterator_next_func; magic : Int16) : JSCFunctionListEntry;
function JS_CGETSET_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; fgetter : Getter_func; fsetter : Setter_func) : JSCFunctionListEntry;
function JS_CGETSET_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; fgetter_magic : getter_magic_func; fsetter_magic : setter_magic_func; magic : Int16) : JSCFunctionListEntry;
function JS_PROP_STRING_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_INT32_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : Int32; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_INT64_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : Int64; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_DOUBLE_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; val : Double; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_PROP_UNDEFINED_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_OBJECT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; tab : PJSCFunctionListEntry;  length : Integer; prop_flags : UInt8) : JSCFunctionListEntry;
function JS_ALIAS_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}) : JSCFunctionListEntry;
function JS_ALIAS_BASE_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; base : Integer) : JSCFunctionListEntry;


var
  OldFPUMask : TFPUExceptionMask;

implementation

{$If Defined(JS_NAN_BOXING)}

function JS_VALUE_GET_TAG(v : JSValue): Int64;
begin
  Result := Integer(v shr 32);
end;

function JS_VALUE_GET_INT(v : JSValue): Integer;
begin
  Result := Integer(v);
end;

function JS_VALUE_GET_BOOL(v : JSValue): Boolean;
begin
  Result := Boolean(v);
end;

function JS_VALUE_GET_PTR(v : JSValue): Pointer;
begin
  Result := {%H-}Pointer(v); // TODO: check if this works the right way.
end;

function JS_MKVAL(tag : Int64; val : Int32): JSValue;
begin
  Result := tag shl 32 or val;
end;

function JS_MKPTR(tag : Int64; ptr : Pointer): JSValue;
begin
  Result := JSValue((tag shl 32) or UIntPtr(ptr));
end;

function JS_VALUE_GET_FLOAT64(v : JSValue): Double;
type
  rec = record
    case Byte of
      0 : (v : JSValue);
      1 : (d : Double);
  end;
var
  u : rec;
begin
  u.v := v;
  u.v {$IfDef FPC}+={$Else} := u.v +{$EndIf} UInt64(JS_FLOAT64_TAG_ADDEND shl 32);
  Result := u.d;
end;

function __JS_NewFloat64({%H-}ctx : JSContext; d : Double): JSValue;
type
  rec = record
    case Byte of
      0 : (d : Double);
      1 : (u64 : UInt64);
  end;
var
  u : rec;
  v : JSValue;
begin
  u.d := d;
  { normalize NaN }
  if ((u.u64 and $7fffffffffffffff) > $7ff0000000000000) then
    v := UInt64(JS_NAN)
  else
    v := u.u64 - UInt64(JS_FLOAT64_TAG_ADDEND shl 32);
  Result := v;
end;

function JS_TAG_IS_FLOAT64(tag : Int64): Boolean; inline;
begin
  Result := Boolean( UInt64((tag) - JS_TAG_FIRST) >= (JS_TAG_FLOAT64 - JS_TAG_FIRST) );
end;

{ same as JS_VALUE_GET_TAG, but return JS_TAG_FLOAT64 with NaN boxing }
function JS_VALUE_GET_NORM_TAG(v : JSValue): Int64;
var
  tag : UInt32;
begin
  tag := JS_VALUE_GET_TAG(v);
  if JS_TAG_IS_FLOAT64(tag) then
      Result := JS_TAG_FLOAT64
  else
      Result := tag;
end;

function JS_VALUE_IS_NAN(v : JSValue) : JS_BOOL; inline;
begin
  Result := (JS_VALUE_GET_TAG(v) = (JS_NAN shr 32));
end;

{$else}

function JS_VALUE_GET_TAG(v : JSValue): Int64;
begin
  Result := v.tag;
end;
{ same as JS_VALUE_GET_TAG, but return JS_TAG_FLOAT64 with NaN boxing }
function JS_VALUE_GET_NORM_TAG(v : JSValue): Int64;
begin
  Result := JS_VALUE_GET_TAG(v);
end;

function JS_VALUE_GET_INT(v : JSValue): Integer;
begin
  Result := v.u.&int32;
end;

function JS_VALUE_GET_BOOL(v : JSValue): Boolean;
begin
  Result := Boolean(v.u.&int32);
end;

function JS_VALUE_GET_FLOAT64(v : JSValue): Double;
begin
  Result := v.u.float64;
end;

function JS_VALUE_GET_PTR(v : JSValue): Pointer;
begin
  Result := v.u.Ptr;
end;

function JS_MKVAL(tag : Int64; val : Int32): JSValue;
begin
  Result.u.&int32 := val;
  Result.tag := tag;
end;

function JS_MKPTR(tag : Int64; ptr : Pointer): JSValue;
begin
  Result.u.Ptr := ptr;
  Result.tag := tag;
end;

function JS_TAG_IS_FLOAT64(tag : Int64): Boolean; inline;
begin
  Result := UInt64(tag) = JS_TAG_FLOAT64;
end;

function JS_NAN : JSValue;
begin
  Result.u.float64 := JS_FLOAT64_NAN;
  Result.tag := JS_TAG_FLOAT64;
end;

function __JS_NewFloat64({%H-}ctx : JSContext; d : Double): JSValue;
begin
  Result.u.float64 := d;
  Result.tag := JS_TAG_FLOAT64;
end;

function JS_VALUE_IS_NAN(v : JSValue) : JS_BOOL; inline;
type
  UnionRec = record
    case Byte of
      0 : (d : Double);
      1 : (u64 : UInt64);
  end;
var
  u : UnionRec;
begin
  if (v.tag <> JS_TAG_FLOAT64) then
    Exit(False);
  u.d := v.u.float64;
  Result := (u.u64 and $7fffffffffffffff) > $7ff0000000000000;
end;

{$ENDIF}

function JS_VALUE_IS_BOTH_INT(v1, v2 : JSValue): Boolean;
begin
  Result := ((JS_VALUE_GET_TAG(v1) or JS_VALUE_GET_TAG(v2)) = JS_TAG_INT);
end;

function JS_VALUE_IS_BOTH_FLOAT(v1, v2 : JSValue): Boolean;
begin
  Result := (JS_TAG_IS_FLOAT64(JS_VALUE_GET_TAG(v1)) and JS_TAG_IS_FLOAT64(JS_VALUE_GET_TAG(v2)))
end;

function JS_VALUE_GET_OBJ(v : JSValue): JSObject;
begin
  Result := JS_VALUE_GET_PTR(v);
end;

function JS_VALUE_GET_STRING(v : JSValue): JSString;
begin
  Result := JS_VALUE_GET_PTR(v);
end;

function JS_VALUE_HAS_REF_COUNT(v : JSValue): Boolean;
begin
  Result := UInt64(JS_VALUE_GET_TAG(v)) >= UInt64(JS_TAG_FIRST);
end;

{ special values }

function JS_NULL : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_NULL, 0);
end;

function JS_UNDEFINED : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_UNDEFINED, 0);
end;

function JS_FALSE : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_BOOL, 0);
end;

function JS_TRUE : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_BOOL, 1);
end;

function JS_EXCEPTION : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_EXCEPTION, 0);
end;

function JS_UNINITIALIZED : JSValue;
begin
  Result := JS_MKVAL(JS_TAG_UNINITIALIZED, 0);
end;

{ value handling }

function JS_NewBool({%H-}ctx : JSContext; val : JS_BOOL): JSValue;
begin
  Result := JS_MKVAL(JS_TAG_BOOL, Int32(val));
end;

function JS_NewInt32( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
begin
  Result := JS_MKVAL(JS_TAG_INT, val);
end;

function JS_NewCatchOffset( {%H-}ctx : JSContext; val : Int32): JSValue; inline;
begin
  Result := JS_MKVAL(JS_TAG_CATCH_OFFSET, val);
end;

function JS_NewInt64(ctx : JSContext; val : Int64): JSValue;
begin
  if val = Int32(val) then
    Result := JS_NewInt32(ctx, val)
  else
    Result := __JS_NewFloat64(ctx, val);
end;

function JS_NewUint32(ctx : JSContext; val : UInt32): JSValue;
begin
  if val <= $7fffffff then
    Result := JS_NewInt32(ctx, val)
  else
    Result := __JS_NewFloat64(ctx, val);
end;

function JS_NewFloat64(ctx : JSContext; d : Double): JSValue;
type
  rec = record
    case Byte of
      0 : (d : Double);
      1 : (u : UInt64);
  end;
var
  u,t : rec;
  val : Int32;
begin
  u.d := d;
  val := Int32(Round(d));
  t.d := val;
  { -0 cannot be represented as integer, so we compare the bit representation }
  if u.u = t.u then
    Result := JS_MKVAL(JS_TAG_INT, val)
  else
    Result := __JS_NewFloat64(ctx, d);
end;

function JS_IsBigInt(v : JSValueConst): Boolean; inline;
begin
 Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_BIG_INT);
end;

function JS_IsBigFloat(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_BIG_FLOAT);
end;

function JS_IsBigDecimal(v : JSValueConst): JS_BOOL; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_BIG_DECIMAL);
end;

function JS_IsBool(v : JSValueConst): JS_BOOL; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_BOOL);
end;

function JS_IsNull(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_NULL);
end;

function JS_IsUndefined(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_UNDEFINED);
end;

function JS_IsException(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_EXCEPTION);
end;

function JS_IsUninitialized(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_UNINITIALIZED);
end;

function JS_IsString(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_STRING);
end;

function JS_IsNumber(v: JSValueConst): JS_BOOL; inline;
var
  tag : Integer;
begin
  tag := JS_VALUE_GET_TAG(v);
  Result := (tag = JS_TAG_INT) or JS_TAG_IS_FLOAT64(tag);
end;

function JS_IsSymbol(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_SYMBOL);
end;

function JS_IsObject(v : JSValueConst): Boolean; inline;
begin
  Result := Boolean(JS_VALUE_GET_TAG(v) = JS_TAG_OBJECT);
end;

procedure JS_FreeValue(ctx : JSContext; v : JSValue); inline;
var
  p : PJSRefCountHeader;
begin
  if JS_VALUE_HAS_REF_COUNT(v) then
  begin
    p := PJSRefCountHeader(JS_VALUE_GET_PTR(v));
    Dec(p^.ref_count);
    if (p^.ref_count <= 0) then
      __JS_FreeValue(ctx, v);
  end;
end;

procedure JS_FreeValueRT(rt : JSRuntime; v : JSValue); inline;
var
  p : PJSRefCountHeader;
begin
  if JS_VALUE_HAS_REF_COUNT(v) then
  begin
    p := PJSRefCountHeader(JS_VALUE_GET_PTR(v));
    Dec(p^.ref_count);
    if (p^.ref_count <= 0) then
      __JS_FreeValueRT(rt, v);
  end;
end;

function JS_DupValue({%H-}ctx : JSContext; v : JSValueConst) : JSValue; inline;
var
  p : PJSRefCountHeader;
begin
  if JS_VALUE_HAS_REF_COUNT(v) then
  begin
    p := PJSRefCountHeader(JS_VALUE_GET_PTR(v));
    inc(p^.ref_count);
  end;
  Result := JSValue(v);
end;

function JS_DupValueRT({%H-}rt : JSRuntime; v : JSValueConst) : JSValue; inline;
var
  p : PJSRefCountHeader;
begin
  if JS_VALUE_HAS_REF_COUNT(v) then
  begin
    p := PJSRefCountHeader(JS_VALUE_GET_PTR(v));
    inc(p^.ref_count);
  end;
  Result := JSValue(v);
end;

function JS_ToUint32(ctx : JSContext; pres : pUInt32; val : JSValueConst): Integer; inline;
begin
  Result := JS_ToInt32(ctx, pInt32(pres), val);
end;

function JS_ToCStringLen(ctx : JSContext; plen : psize_t; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
begin
  Result := JS_ToCStringLen2(ctx, plen, val, False);
end;

function JS_ToCString(ctx : JSContext; val : JSValueConst): {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; inline;
begin
  Result := JS_ToCStringLen2(ctx, nil, val, False);
end;

function JS_GetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom): JSValue; inline;
begin
  Result := JS_GetPropertyInternal(ctx, this_obj, prop, this_obj, False);
end;

function JS_SetProperty(ctx : JSContext; this_obj : JSValueConst; prop : JSAtom; val : JSValue): Integer; inline;
begin
  Result := JS_SetPropertyInternal(ctx, this_obj, prop, val, JS_PROP_THROW);
end;

{ C function definition }

function JS_NewCFunction(ctx : JSContext; func : PJSCFunction; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer): JSValue; inline;
begin
  Result := JS_NewCFunction2(ctx, func, name, length, JS_CFUNC_generic, 0);
end;

function JS_NewCFunctionMagic(ctx : JSContext; func : PJSCFunctionMagic; name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum; magic : Integer): JSValue; inline;
begin
  Result := JS_NewCFunction2(ctx, PJSCFunction(func), name, length, cproto, magic);;
end;

{ C property definition }

function JS_CFUNC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           func : JSCFunction) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := 0;
  Result.u.func.length := length;
  Result.u.func.cproto := JS_CFUNC_generic;
  Result.u.func.cfunc.generic := func;
end;

function JS_CFUNC_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           func : JSCFunctionMagic; magic : Int16) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := magic;
  Result.u.func.length := length;
  Result.u.func.cproto := JS_CFUNC_generic_magic;
  Result.u.func.cfunc.generic_magic := func;
end;

function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum ; func : f_f_func) : JSCFunctionListEntry; overload;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := 0;
  Result.u.func.length := length;
  Result.u.func.cproto := cproto;
  Result.u.func.cfunc.f_f := func;
end;

function JS_CFUNC_SPECIAL_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           cproto : JSCFunctionEnum ; func : f_f_f_func) : JSCFunctionListEntry; overload;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := 0;
  Result.u.func.length := length;
  Result.u.func.cproto := cproto;
  Result.u.func.cfunc.f_f_f := func;
end;

function JS_ITERATOR_NEXT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; length : Integer;
           iterator_next : iterator_next_func; magic : Int16) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CFUNC;
  Result.magic := magic;
  Result.u.func.length := length;
  Result.u.func.cproto := JS_CFUNC_iterator_next;
  Result.u.func.cfunc.iterator_next := iterator_next;
end;

function JS_CGETSET_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           fgetter : Getter_func; fsetter : Setter_func ) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CGETSET;
  Result.magic := 0;
  Result.u.getset.get.getter  := fgetter;
  Result.u.getset._set.setter := fsetter;
end;

function JS_CGETSET_MAGIC_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           fgetter_magic : getter_magic_func;
           fsetter_magic : setter_magic_func; magic : Int16) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_CGETSET_MAGIC;
  Result.magic := magic;
  Result.u.getset.get.getter_magic := fgetter_magic;
  Result.u.getset._set.setter_magic := fsetter_magic;
end;

function JS_PROP_STRING_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_STRING;
  Result.magic := 0;
  Result.u.str := val;
end;

function JS_PROP_INT32_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : Int32; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_INT32;
  Result.magic := 0;
  Result.u.i32 := val;
end;

function JS_PROP_INT64_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : Int64; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_INT64;
  Result.magic := 0;
  Result.u.i64 := val;
end;

function JS_PROP_DOUBLE_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           val : Double; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_DOUBLE;
  Result.magic := 0;
  Result.u.f64 := val;
end;

function JS_PROP_UNDEFINED_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_PROP_UNDEFINED;
  Result.magic := 0;
  Result.u.i32 := 0;
end;

function JS_OBJECT_DEF(name : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}; tab : PJSCFunctionListEntry;
           length : Integer; prop_flags : UInt8) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := prop_flags;
  Result.def_type := JS_DEF_OBJECT;
  Result.magic := 0;
  Result.u.prop_list.tab := {$IfDef FPC}tab{$Else}Pointer(tab){$EndIf};
  Result.u.prop_list.len := length;
end;

function JS_ALIAS_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf}) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_ALIAS;
  Result.magic := 0;
  Result.u.alias.name := from;
  Result.u.alias.base := -1;
end;

function JS_ALIAS_BASE_DEF(name, from : {$IFDEF FPC}PChar{$Else}PAnsiChar{$EndIf};
           base : Integer) : JSCFunctionListEntry;
begin
  Result.name := name;
  Result.prop_flags := JS_PROP_WRITABLE or JS_PROP_CONFIGURABLE;
  Result.def_type := JS_DEF_ALIAS;
  Result.magic := 0;
  Result.u.alias.name := from;
  Result.u.alias.base := base;
end;

{ bignum stuff :D }

function c_udivti3(num,den:uint64):uint64; cdecl; public alias: {$ifdef darwin}'___udivti3'{$else} '__udivdi3'{$endif};
begin
 result:=num div den;
end;


initialization
  // fix the Invalid floating point operation .
  OldFPUMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

finalization
   SetExceptionMask(OldFPUMask);

end.

