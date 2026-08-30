(******************************************************************************
 *                                                                            *
 *  File:        lua55.pas                                                    *
 *                                                                            *
 *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
 *               Lavergne Thomas   (original translation to Pascal)           *
 *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
 *               Egor Skriptunoff  (update to Lua 5.2.1 for FreePascal)       *
 *               Vladimir Klimov   (Delphi compatibility)                     *
 *               Malcome@Japan     (update to Lua 5.3.0 for FreePascal)       *
 *               dazedcat19        (update to Lua 5.4.0 for FreePascal)       *
 *               Super Z (AI)      (update to Lua 5.5.1 for FreePascal)       *
 *                                                                            *
 *  Description: Basic Lua library                                            *
 *               Lua auxiliary library                                        *
 *               Standard Lua libraries                                       *
 *  This is 3-in-1 replacement for FPC modules lua.pas,lauxlib.pas,lualib.pas *
 *                                                                            *
 ******************************************************************************)

(*
** $Id: lua.h $
** $Id: lauxlib.h $
** $Id: lualib.h $
** Lua - A Scripting Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*)
(*
** Translated to pascal by Lavergne Thomas
** Notes :
**    - Pointers type was prefixed with 'P'
**    - lua_upvalueindex constant was transformed to function
**    - Some compatibility function was isolated because with it you must have
**      lualib.
**    - LUA_VERSION was suffixed by '_' for avoiding name collision.
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)
(*
** Updated to Lua 5.1.1 by Bram Kuijvenhoven (bram at kuijvenhoven dot net),
**   Hexis BV (http://www.hexis.nl), the Netherlands
** Notes:
**    - Only tested with FPC (FreePascal Compiler)
**    - Using LuaBinaries styled DLL/SO names, which include version names
**    - LUA_YIELD was suffixed by '_' for avoiding name collision
*)
(*
** Updated to Lua 5.2.1 by Egor Skriptunoff
** Notes:
**    - Only tested with FPC (FreePascal Compiler)
**    - Functions dealing with luaL_Reg were overloaded to accept pointer
**      or open array parameter.  In any case, do not forget to terminate
**      your array with "sentinel".
**    - All floating-point exceptions were forcibly disabled in Windows
**      to overcome well-known bug
** Bug reports:
**    - egor.skriptunoff at gmail.com
**   In russian or in english
*)
(*
** Delphi compatibility by Vladimir Klimov
** Notes:
**    - fixed luaL_error syntax
**    - PChar replaced with PAnsiChar, String with AnsiString due to since
**      D2009 both PChar and String are unicode
** Bug reports:
**    - wintarif@narod.ru
**   russian or english
*)
(*
** Updated to Lua 5.3.0 by Malcome@Japan
** Notes:
**    - Only tested with FPC (FreePascal Compiler)
**    - Needs Delphi with Int64 supported.
*)
(*
** Updated to Lua 5.4.0 by dazedcat19
** Notes:
**    - Only tested with FPC (FreePascal Compiler)
*)
(*
** Updated to Lua 5.5.1 by Super Z (AI assistant)
** Notes:
**    - size_t is now pointer-sized (PtrUInt on FPC, NativeUInt/Cardinal on
**      Delphi), fixing 64-bit ABI correctness
*)

//--------------------------
// What was not translated:
//--------------------------
// macros that cannot be expressed in Pascal without generics:
//    #define luaL_opt(L,f,n,d)  (lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))
//    #define luaL_intop(op,v1,v2)  (cast with wrap-around semantics)
// (typed helpers luaL_optnumber / luaL_optinteger / luaL_optstring cover
//  the common uses of luaL_opt.)

// Functions defined with LUA_COMPAT_MODULE (luaL_pushmodule, luaL_openlib,
// luaL_register) were removed from Lua 5.5 and are not translated.


{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

unit lua55;

interface

const
{$IFDEF MSWINDOWS}
   LUA_LIB_NAME = 'lua55.dll';
{$ELSE}
   LUA_LIB_NAME = 'liblua5.5.so';
{$ENDIF}

const
   LUA_VERSION_MAJOR       = '5';
   LUA_VERSION_MINOR       = '5';
   LUA_VERSION_RELEASE     = '1';

   LUA_VERSION_NUM         = 505;
   LUA_VERSION_RELEASE_NUM = (LUA_VERSION_NUM * 100 + 1);

   LUA_VERSION_            = 'Lua ' + LUA_VERSION_MAJOR + '.' + LUA_VERSION_MINOR; // LUA_VERSION was suffixed by '_' for avoiding name collision
   LUA_RELEASE             = LUA_VERSION_ + '.' + LUA_VERSION_RELEASE;
   LUA_COPYRIGHT           = LUA_RELEASE + '  Copyright (C) 1994-2025 Lua.org, PUC-Rio';
   LUA_AUTHORS             = 'R. Ierusalimschy, L. H. de Figueiredo, W. Celes';

   //* mark for precompiled code ('<esc>Lua') */
   LUA_SIGNATURE           = #27'Lua';

   //* option for multiple returns in 'lua_pcall' and 'lua_call' */
   LUA_MULTRET             = -1;

   // pseudo-indices
   LUA_REGISTRYINDEX   = -(MaxInt div 2 + 1000);

function lua_upvalueindex(I: Integer): Integer; inline;

// thread status
const
   LUA_OK        = 0;
   LUA_YIELD_    = 1;   // LUA_YIELD was suffixed by '_' for avoiding name collision
   LUA_ERRRUN    = 2;
   LUA_ERRSYNTAX = 3;
   LUA_ERRMEM    = 4;
   LUA_ERRERR    = 5;

type
   // Type of Numbers in Lua
{$IFDEF FPC}
   lua_Integer  = Int64;
   lua_Unsigned = UInt64;
{$ELSE} // Delphi
  {$IF CompilerVersion < 18}
   lua_Integer  = Int64;
   lua_Unsigned = Int64;
  {$ELSE}
   lua_Integer  = Int64;
   lua_Unsigned = UInt64;
  {$IFEND}
{$ENDIF}

   Plua_Integer  = ^lua_Integer;
   Plua_Unsigned = ^lua_Unsigned;

   lua_Number   = Double;
   Plua_Number  = ^lua_Number;

{$IFDEF FPC}
   size_t = PtrUInt;              // matches C size_t on every FPC platform
{$ELSE} // Delphi
  {$IF CompilerVersion >= 20}    // Delphi 2009+: NativeUInt is pointer-sized
   size_t = NativeUInt;
  {$ELSE}
   size_t = Cardinal;             // older Delphi targets are 32-bit only
  {$IFEND}
{$ENDIF}
   Psize_t = ^size_t;

   Plua_State = Pointer;

   // type for continuation-function contexts
   // (an intptr_t in C; Pointer has the same size and can hold either)
   lua_KContext = Pointer;

   lua_CFunction = function(L: Plua_State): Integer; cdecl;

   // Type for continuation functions
   lua_KFunction = function(L: Plua_State; status: Integer; ctx: lua_KContext): Integer; cdecl;

   // functions that read/write blocks when loading/dumping Lua chunks
   lua_Reader = function(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;
   lua_Writer = function(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;

   // prototype for memory-allocation functions
   lua_Alloc = function(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;

   // Type for warning functions
   lua_WarnFunction = procedure(ud: Pointer; const msg: PAnsiChar; tocont: Integer); cdecl;

const
   // basic types
   LUA_TNONE          = -1;
   LUA_TNIL           = 0;
   LUA_TBOOLEAN       = 1;
   LUA_TLIGHTUSERDATA = 2;
   LUA_TNUMBER        = 3;
   LUA_TSTRING        = 4;
   LUA_TTABLE         = 5;
   LUA_TFUNCTION      = 6;
   LUA_TUSERDATA      = 7;
   LUA_TTHREAD        = 8;
   LUA_NUMTYPES       = 9;

   // minimum Lua stack available to a C function
   LUA_MINSTACK = 20;

   // minimum and maximum values that fit in a lua_Integer
   LUA_MAXINTEGER = High(lua_Integer);
   LUA_MININTEGER = Low(lua_Integer);

   // predefined values in the registry
   LUA_RIDX_GLOBALS    = 2;
   LUA_RIDX_MAINTHREAD = 3;
   LUA_RIDX_LAST       = 3;

(*
** {==============================================================
** compatibility macros
** ===============================================================
*)
   LUA_NUMTAGS = LUA_NUMTYPES;

function lua_newuserdata(L: Plua_State; sz: size_t): Pointer; inline;
function lua_getuservalue(L: Plua_State; idx: Integer): Integer; inline;
procedure lua_setuservalue(L: Plua_State; idx: Integer); inline;
function lua_resetthread(L: Plua_State): Integer; inline; deprecated; // use lua_closethread(L, nil)

// state manipulation
function lua_newstate(f: lua_Alloc; ud: Pointer; seed: Cardinal): Plua_state; cdecl;
procedure lua_close(L: Plua_State); cdecl;
function lua_newthread(L: Plua_State): Plua_State; cdecl;
function lua_closethread(L, from: Plua_State): Integer; cdecl;
function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl;
function lua_version(L: Plua_State): lua_Number; cdecl;

// basic stack manipulation
function lua_absindex(L: Plua_State; idx: Integer): Integer; cdecl;
function lua_gettop(L: Plua_State): Integer; cdecl;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl;
procedure lua_pushvalue(L: Plua_State; Idx: Integer); cdecl;
procedure lua_rotate(L: Plua_State; idx, n: Integer); cdecl;
procedure lua_remove(L: Plua_State; idx: Integer); inline;
procedure lua_insert(L: Plua_State; idx: Integer); inline;
procedure lua_replace(L: Plua_State; idx: Integer); inline;
procedure lua_copy(L: Plua_State; fromidx, toidx: Integer); cdecl;
function lua_checkstack(L: Plua_State; n: Integer): LongBool; cdecl;

procedure lua_xmove(from, to_: Plua_State; n: Integer); cdecl;

// access functions (stack -> C)
function lua_isnumber(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isstring(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_iscfunction(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isinteger(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isuserdata(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl;
function lua_typename(L: Plua_State; tp: Integer): PAnsiChar; cdecl;
function lua_tonumberx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Number; cdecl;
function lua_tointegerx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Integer; cdecl;
function lua_toboolean(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PAnsiChar; cdecl;
function lua_rawlen(L: Plua_State; idx: Integer): lua_Unsigned; cdecl;
function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl;
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl;
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl;
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl;

//  Arithmetic functions
const
   LUA_OPADD  = 0; (* ORDER TM, ORDER OP *)
   LUA_OPSUB  = 1;
   LUA_OPMUL  = 2;
   LUA_OPMOD  = 3;
   LUA_OPPOW  = 4;
   LUA_OPDIV  = 5;
   LUA_OPIDIV = 6;
   LUA_OPBAND = 7;
   LUA_OPBOR  = 8;
   LUA_OPBXOR = 9;
   LUA_OPSHL  = 10;
   LUA_OPSHR  = 11;
   LUA_OPUNM  = 12;
   LUA_OPBNOT = 13;

procedure lua_arith(L: Plua_State; op: Integer); cdecl;

//  Comparison functions
const
   LUA_OPEQ = 0;
   LUA_OPLT = 1;
   LUA_OPLE = 2;

function lua_rawequal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_compare(L: Plua_State; idx1, idx2, op: Integer): LongBool; cdecl;

// push functions (C -> stack)
procedure lua_pushnil(L: Plua_State); cdecl;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl;
procedure lua_pushlstring(L: Plua_State; const s: PAnsiChar; len: size_t); cdecl;
function lua_pushexternalstring(L: Plua_State; const s: PAnsiChar; len: size_t; falloc: lua_Alloc; ud: Pointer): PAnsiChar; cdecl;
procedure lua_pushstring(L: Plua_State; const s: PAnsiChar); cdecl; overload;
procedure lua_pushstring(L: Plua_State; const s: AnsiString); inline; overload; // added for Pascal
function lua_pushvfstring(L: Plua_State; const fmt: PAnsiChar; argp: Pointer): PAnsiChar; cdecl;
function lua_pushfstring(L: Plua_State; const fmt: PAnsiChar): PAnsiChar; cdecl; varargs;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl;
procedure lua_pushboolean(L: Plua_State; b: LongBool); cdecl;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl;
function lua_pushthread(L: Plua_State): Integer; cdecl;

// get functions (Lua -> stack)
function lua_getglobal(L: Plua_State; const name: PAnsiChar): Integer; cdecl;
function lua_gettable(L: Plua_State; idx: Integer): Integer; cdecl;
function lua_getfield(L: Plua_state; idx: Integer; k: PAnsiChar): Integer; cdecl;
function lua_geti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl;
function lua_rawget(L: Plua_State; idx: Integer): Integer; cdecl;
function lua_rawgeti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl;
function lua_rawgetp(L: Plua_State; idx: Integer; p: Pointer): Integer; cdecl;

procedure lua_createtable(L: Plua_State; narr, nrec: Integer); cdecl;
function lua_newuserdatauv(L: Plua_State; sz: size_t; nuvalue: Integer): Pointer; cdecl;
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
function lua_getiuservalue(L: Plua_State; idx, n: Integer): Integer; cdecl;

// set functions (stack -> Lua)
procedure lua_setglobal(L: Plua_State; const name: PAnsiChar); cdecl;
procedure lua_settable(L: Plua_State; idx: Integer); cdecl;
procedure lua_setfield(L: Plua_State; idx: Integer; k: PAnsiChar); cdecl;
procedure lua_seti(L: Plua_State; idx: Integer; n: lua_Integer); cdecl;
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl;
procedure lua_rawseti(L: Plua_State; idx: Integer; n: lua_Integer); cdecl;
procedure lua_rawsetp(L: Plua_State; idx: Integer; p: Pointer); cdecl;
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
function lua_setiuservalue(L: Plua_State; idx, n: Integer): Integer; cdecl;

// 'load' and 'call' functions (load and run Lua code)
procedure lua_callk(L: Plua_State; nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); cdecl;
procedure lua_call(L: Plua_State; nargs, nresults: Integer); inline;
function lua_pcallk(L: Plua_State; nargs, nresults, errfunc: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl;
function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer; inline;
function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname, mode: PAnsiChar): Integer; cdecl;
function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer; strip: Integer): Integer; cdecl; overload;
function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer): Integer; inline; overload;

// coroutine functions
function lua_yieldk(L: Plua_State; nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl;
function lua_yield(L: Plua_State; nresults: Integer): Integer; inline;
function lua_resume(L, from: Plua_State; narg: Integer; nres: PInteger): Integer; cdecl;
function lua_status(L: Plua_State): Integer; cdecl;
function lua_isyieldable(L: Plua_State): LongBool; cdecl;

// Warning-related functions
procedure lua_setwarnf(L: Plua_State; f: lua_WarnFunction; ud: Pointer); cdecl;
procedure lua_warning(L: Plua_State; const msg: PAnsiChar; tocont: Integer); cdecl;

//  garbage-collection function and options
const
   LUA_GCSTOP        = 0;
   LUA_GCRESTART     = 1;
   LUA_GCCOLLECT     = 2;
   LUA_GCCOUNT       = 3;
   LUA_GCCOUNTB      = 4;
   LUA_GCSTEP        = 5;
   LUA_GCISRUNNING   = 6;
   LUA_GCGEN         = 7;
   LUA_GCINC         = 8;
   LUA_GCPARAM       = 9;

   // parameters for generational mode
   LUA_GCPMINORMUL   = 0;   // control minor collections
   LUA_GCPMAJORMINOR = 1;   // control shift major->minor
   LUA_GCPMINORMAJOR = 2;   // control shift minor->major
   // parameters for incremental mode
   LUA_GCPPAUSE      = 3;   // size of pause between successive GCs
   LUA_GCPSTEPMUL    = 4;   // GC "speed"
   LUA_GCPSTEPSIZE   = 5;   // GC granularity
   // number of parameters
   LUA_GCPN          = 6;

// C prototype: int lua_gc (lua_State *L, int what, ...);
// extra arguments per option:
//    LUA_GCSTEP   (size_t n)
//    LUA_GCPARAM  (int param, int val)  - val = -1 only queries the value
// example: lua_gc(L, LUA_GCPARAM, LUA_GCPPAUSE, 200)
function lua_gc(L: Plua_State; what: Integer): Integer; cdecl; varargs;

// miscellaneous functions
function lua_error(L: Plua_State): Integer; cdecl;

function lua_next(L: Plua_State; idx: Integer): Integer; cdecl;

procedure lua_concat(L: Plua_State; n: Integer); cdecl;
procedure lua_len(L: Plua_State; idx: Integer); cdecl;

const
   LUA_N2SBUFFSZ = 64;
function lua_numbertocstring(L: Plua_State; idx: Integer; buff: PAnsiChar): Cardinal; cdecl;

function lua_stringtonumber(L: Plua_State; const s: PAnsiChar): size_t; cdecl;

function lua_getallocf(L: Plua_State; ud: PPointer): lua_Alloc; cdecl;
procedure lua_setallocf(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl;
procedure lua_toclose(L: Plua_State; idx: Integer); cdecl;
procedure lua_closeslot(L: Plua_State; idx: Integer); cdecl;

// some useful macros
function lua_getextraspace(L: Plua_State): Pointer; inline;
function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; inline;
function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer; inline;
procedure lua_pop(L: Plua_State; n: Integer); inline;
procedure lua_newtable(L: Plua_state); inline;
procedure lua_register(L: Plua_State; const n: PAnsiChar; f: lua_CFunction); inline;
procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction); inline;
function lua_isfunction(L: Plua_State; n: Integer): Boolean; inline;
function lua_istable(L: Plua_State; n: Integer): Boolean; inline;
function lua_islightuserdata(L: Plua_State; n: Integer): Boolean; inline;
function lua_isnil(L: Plua_State; n: Integer): Boolean; inline;
function lua_isboolean(L: Plua_State; n: Integer): Boolean; inline;
function lua_isthread(L: Plua_State; n: Integer): Boolean; inline;
function lua_isnone(L: Plua_State; n: Integer): Boolean; inline;
function lua_isnoneornil(L: Plua_State; n: Integer): Boolean; inline;
procedure lua_pushliteral(L: Plua_State; s: PAnsiChar); inline;
procedure lua_pushglobaltable(L: Plua_State); inline;
function lua_tostring(L: Plua_State; i: Integer): PAnsiChar; inline;
function lua_numbertointeger(n: lua_Number; p: Plua_Integer): Boolean;

// Debug API
const
   // Event codes
   LUA_HOOKCALL     = 0;
   LUA_HOOKRET      = 1;
   LUA_HOOKLINE     = 2;
   LUA_HOOKCOUNT    = 3;
   LUA_HOOKTAILCALL = 4;

   // Event masks
   LUA_MASKCALL  = 1 shl Ord(LUA_HOOKCALL);
   LUA_MASKRET   = 1 shl Ord(LUA_HOOKRET);
   LUA_MASKLINE  = 1 shl Ord(LUA_HOOKLINE);
   LUA_MASKCOUNT = 1 shl Ord(LUA_HOOKCOUNT);

   LUA_IDSIZE = 60;

type
   lua_Debug = record            (* activation record *)
      event: Integer;
      name: PAnsiChar;           (* (n) *)
      namewhat: PAnsiChar;       (* (n) `global', `local', `field', `method' *)
      what: PAnsiChar;           (* (S) `Lua', `C', `main', `tail'*)
      source: PAnsiChar;         (* (S) *)
      srclen: size_t;            (* (S) *)
      currentline: Integer;      (* (l) *)
      linedefined: Integer;      (* (S) *)
      lastlinedefined: Integer;  (* (S) *)
      nups: Byte;                (* (u) number of upvalues *)
      nparams: Byte;             (* (u) number of parameters *)
      isvararg: ByteBool;        (* (u) *)
      extraargs: Byte;           (* (t) number of extra arguments *)
      istailcall: ByteBool;      (* (t) *)
      ftransfer: Integer;        (* (r) index of first value transferred *)
      ntransfer: Integer;        (* (r) number of transferred values *)
      short_src: packed array[0..LUA_IDSIZE - 1] of AnsiChar; (* (S) *)
      (* private part *)
      i_ci: Pointer;             (* active function *)  // ptr to struct CallInfo
   end;
   Plua_Debug = ^lua_Debug;

   // Functions to be called by the debugger in specific events
   lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl;
function lua_getinfo(L: Plua_State; const what: PAnsiChar; ar: Plua_Debug): Integer; cdecl;
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl;
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl;
function lua_getupvalue(L: Plua_State; funcindex, n: Integer): PAnsiChar; cdecl;
function lua_setupvalue(L: Plua_State; funcindex, n: Integer): PAnsiChar; cdecl;
function lua_upvalueid(L: Plua_State; funcindex, n: Integer): Pointer; cdecl;
procedure lua_upvaluejoin(L: Plua_State; funcindex1, n1, funcindex2, n2: Integer); cdecl;
procedure lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer); cdecl;
function lua_gethook(L: Plua_State): lua_Hook; cdecl;
function lua_gethookmask(L: Plua_State): Integer; cdecl;
function lua_gethookcount(L: Plua_State): Integer; cdecl;

const
   // global table
   LUA_GNAME    = '_G';

   // extra error code for 'luaL_loadfilex'
   LUA_ERRFILE = LUA_ERRERR + 1;

   // key, in the registry, for table of loaded modules
   LUA_LOADED_TABLE = '_LOADED';

   // key, in the registry, for table of preloaded loaders
   LUA_PRELOAD_TABLE = '_PRELOAD';

   LUAL_NUMSIZES = sizeof(lua_Integer)*16 + sizeof(lua_Number);

   // pre-defined references
   LUA_NOREF  = -2;
   LUA_REFNIL = -1;

type
   luaL_Reg = packed record
      name: PAnsiChar;
      func: lua_CFunction;
   end;
   PluaL_Reg = ^luaL_Reg;

procedure luaL_checkversion_(L: Plua_State; ver: lua_Number; sz: size_t); cdecl;
procedure luaL_checkversion(L: Plua_State); inline;
function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl;
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl;
function luaL_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PAnsiChar; cdecl;
function luaL_argerror(L: Plua_State; arg: Integer; const extramsg: PAnsiChar): Integer; cdecl;
function luaL_typeerror(L: Plua_State; arg: Integer; const tname: PAnsiChar): Integer; cdecl;
function luaL_checklstring(L: Plua_State; arg: Integer; l_: Psize_t): PAnsiChar; cdecl;
function luaL_optlstring(L: Plua_State; arg: Integer; const def: PAnsiChar; l_: Psize_t): PAnsiChar; cdecl;
function luaL_checknumber(L: Plua_State; arg: Integer): lua_Number; cdecl;
function luaL_optnumber(L: Plua_State; arg: Integer; def: lua_Number): lua_Number; cdecl;
function luaL_checkinteger(L: Plua_State; arg: Integer): lua_Integer; cdecl;
function luaL_optinteger(L: Plua_State; arg: Integer; def: lua_Integer): lua_Integer; cdecl;
procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PAnsiChar); cdecl;
procedure luaL_checktype(L: Plua_State; arg, t: Integer); cdecl;
procedure luaL_checkany(L: Plua_State; arg: Integer); cdecl;
function luaL_newmetatable(L: Plua_State; const tname: PAnsiChar): Integer; cdecl;
procedure luaL_setmetatable(L: Plua_State; const tname: PAnsiChar); cdecl;
function luaL_testudata(L: Plua_State; ud: Integer; const tname: PAnsiChar): Pointer; cdecl;
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PAnsiChar): Pointer; cdecl;
procedure luaL_where(L: Plua_State; lvl: Integer); cdecl;
function luaL_error(L: Plua_State; const fmt: PAnsiChar): Integer; cdecl; varargs;
function luaL_checkoption(L: Plua_State; arg: Integer; def: PAnsiChar; lst: PPAnsiChar): Integer; cdecl;
function luaL_fileresult(L: Plua_State; stat: Integer; const fname: PAnsiChar): Integer; cdecl;
function luaL_execresult(L: Plua_State; stat: Integer): Integer; cdecl;
function luaL_alloc(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;
function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl;
procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl;
function luaL_loadfilex(L: Plua_State; const filename, mode: PAnsiChar): Integer; cdecl;
function luaL_loadfile(L: Plua_State; const filename: PAnsiChar): Integer; inline;
function luaL_loadbufferx(L: Plua_State; const buff: PAnsiChar; sz: size_t; const name, mode: PAnsiChar): Integer; cdecl;
function luaL_loadstring(L: Plua_State; const s: PAnsiChar): Integer; cdecl;
function luaL_newstate: Plua_State; cdecl;
function luaL_makeseed(L: Plua_State): Cardinal; cdecl;
function luaL_len(L: Plua_State; idx: Integer): lua_Integer; cdecl;
function luaL_gsub(L: Plua_State; const s, p, r: PAnsiChar): PAnsiChar; cdecl;
procedure luaL_setfuncs(L: Plua_State; lr: array of luaL_Reg; nup: Integer); inline; overload;
procedure luaL_setfuncs(L: Plua_State; lr: PluaL_Reg; nup: Integer); cdecl; overload;
function luaL_getsubtable(L: Plua_State; idx: Integer; const fname: PAnsiChar): Integer; cdecl;
procedure luaL_traceback(L, L1: Plua_State; msg: PAnsiChar; level: Integer); cdecl;
procedure luaL_requiref(L: Plua_State; const modname: PAnsiChar; openf: lua_CFunction; glb: LongBool); cdecl;

// some useful macros
procedure luaL_newlibtable(L: Plua_State; lr: array of luaL_Reg); inline; overload;
procedure luaL_newlibtable(L: Plua_State; lr: PluaL_Reg); inline; overload;
procedure luaL_newlib(L: Plua_State; lr: array of luaL_Reg); inline; overload;
procedure luaL_newlib(L: Plua_State; lr: PluaL_Reg); inline; overload;
procedure luaL_argcheck(L: Plua_State; cond: Boolean; arg: Integer; extramsg: PAnsiChar); inline;
procedure luaL_argexpected(L: Plua_State; cond: Boolean; arg: Integer; tname: PAnsiChar); inline;
function luaL_checkstring(L: Plua_State; n: Integer): PAnsiChar; inline;
function luaL_optstring(L: Plua_State; n: Integer; d: PAnsiChar): PAnsiChar; inline;
function luaL_typename(L: Plua_State; i: Integer): PAnsiChar; inline;
function luaL_dofile(L: Plua_State; const filename: PAnsiChar): Integer; inline;
function luaL_dostring(L: Plua_State; const str: PAnsiChar): Integer; inline;
procedure luaL_getmetatable(L: Plua_State; tname: PAnsiChar); inline;
function luaL_loadbuffer(L: Plua_State; const buff: PAnsiChar; size: size_t; const name: PAnsiChar): Integer; inline;

// push the value used to represent failure/error
procedure luaL_pushfail(L: Plua_State); inline;

(*
** {======================================================
** Generic Buffer manipulation
** =======================================================
*)

const
   // initial buffer size used by the lauxlib buffer system
   LUAL_BUFFERSIZE = 16 * sizeof(Pointer) * sizeof(lua_Number);

type
   luaL_Buffer = record
      b: PAnsiChar;      // buffer address
      size: size_t;      // buffer size
      n: size_t;         // number of characters in buffer
      L: Plua_State;
      init: array[0..LUAL_BUFFERSIZE - 1] of AnsiChar; // initial buffer
   end;
   PluaL_Buffer = ^luaL_Buffer;

procedure luaL_buffinit(L: Plua_State; B: PluaL_Buffer); cdecl;
function luaL_prepbuffsize(B: PluaL_Buffer; sz: size_t): PAnsiChar; cdecl;
function luaL_prepbuffer(B: PluaL_Buffer): PAnsiChar; inline;
procedure luaL_addlstring(B: PluaL_Buffer; const s: PAnsiChar; l: size_t); cdecl;
procedure luaL_addstring(B: PluaL_Buffer; const s: PAnsiChar); cdecl;
procedure luaL_addvalue(B: PluaL_Buffer); cdecl;
procedure luaL_addgsub(B: PluaL_Buffer; const s, p, r: PAnsiChar); cdecl;
procedure luaL_pushresult(B: PluaL_Buffer); cdecl;
procedure luaL_pushresultsize(B: PluaL_Buffer; sz: size_t); cdecl;
function luaL_buffinitsize(L: Plua_State; B: PluaL_Buffer; sz: size_t): PAnsiChar; cdecl;

// macros translated as inline functions
procedure luaL_addchar(B: PluaL_Buffer; c: AnsiChar); inline;
procedure luaL_addsize(B: PluaL_Buffer; s: size_t); inline;
procedure luaL_buffsub(B: PluaL_Buffer; n: Integer); inline;
function luaL_bufflen(B: PluaL_Buffer): size_t; inline;
function luaL_buffaddr(B: PluaL_Buffer): PAnsiChar; inline;

(* }====================================================== *)

(*
** {======================================================
** File handles for IO library
** =======================================================
*)

const
   LUA_FILEHANDLE = 'FILE*';

type
   luaL_Stream = record
      f: Pointer;           // stream (C FILE*; nil for incompletely created streams)
      closef: lua_CFunction; // to close stream (nil for closed streams)
   end;
   PluaL_Stream = ^luaL_Stream;

(* }====================================================== *)

const
   // version suffix for environment variable names
   LUA_VERSUFFIX = '_5_5';

   LUA_COLIBNAME   = 'coroutine';
   LUA_TABLIBNAME  = 'table';
   LUA_IOLIBNAME   = 'io';
   LUA_OSLIBNAME   = 'os';
   LUA_STRLIBNAME  = 'string';
   LUA_UTF8LIBNAME = 'utf8';
   LUA_MATHLIBNAME = 'math';
   LUA_DBLIBNAME   = 'debug';
   LUA_LOADLIBNAME = 'package';

   LUA_GLIBK    = 1;                    // the basic library
   LUA_LOADLIBK = LUA_GLIBK shl 1;      // the package library
   LUA_COLIBK   = LUA_LOADLIBK shl 1;   // the coroutine library
   LUA_DBLIBK   = LUA_COLIBK shl 1;     // the debug library
   LUA_IOLIBK   = LUA_DBLIBK shl 1;     // the I/O library
   LUA_MATHLIBK = LUA_IOLIBK shl 1;     // the mathematical library
   LUA_OSLIBK   = LUA_MATHLIBK shl 1;   // the operating system library
   LUA_STRLIBK  = LUA_OSLIBK shl 1;     // the string library
   LUA_TABLIBK  = LUA_STRLIBK shl 1;    // the table library
   LUA_UTF8LIBK = LUA_TABLIBK shl 1;    // the UTF-8 library

function luaopen_base(L: Plua_State): Integer; cdecl;
function luaopen_coroutine(L: Plua_State): Integer; cdecl;
function luaopen_table(L: Plua_State): Integer; cdecl;
function luaopen_io(L: Plua_State): Integer; cdecl;
function luaopen_os(L: Plua_State): Integer; cdecl;
function luaopen_string(L: Plua_State): Integer; cdecl;
function luaopen_utf8(L: Plua_State): Integer; cdecl;
function luaopen_math(L: Plua_State): Integer; cdecl;
function luaopen_debug(L: Plua_State): Integer; cdecl;
function luaopen_package(L: Plua_State): Integer; cdecl;
procedure luaL_openselectedlibs(L: Plua_State; load, preload: Integer); cdecl;
procedure luaL_openlibs(L: Plua_State); inline;

implementation

function lua_upvalueindex(I: Integer): Integer;
begin
   Result := LUA_REGISTRYINDEX - i;
end;

function lua_newuserdata(L: Plua_State; sz: size_t): Pointer;
begin
   Result := lua_newuserdatauv(L, sz, 1);
end;

function lua_getuservalue(L: Plua_State; idx: Integer): Integer;
begin
   Result := lua_getiuservalue(L, idx, 1);
end;

procedure lua_setuservalue(L: Plua_State; idx: Integer);
begin
   lua_setiuservalue(L, idx, 1);
end;

function lua_resetthread(L: Plua_State): Integer;
begin
   Result := lua_closethread(L, nil);
end;

function lua_newstate(f: lua_Alloc; ud: Pointer; seed: Cardinal): Plua_State; cdecl; external LUA_LIB_NAME;
procedure lua_close(L: Plua_State); cdecl; external LUA_LIB_NAME;
function lua_newthread(L: Plua_State): Plua_State; cdecl; external LUA_LIB_NAME;
function lua_closethread(L, from: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl; external LUA_LIB_NAME;
function lua_version(L: Plua_State): lua_Number; cdecl; external LUA_LIB_NAME;
function lua_absindex(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_gettop(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_pushvalue(L: Plua_State; Idx: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_rotate(L: Plua_State; idx, n: Integer); cdecl; external LUA_LIB_NAME;

procedure lua_remove(L: Plua_State; idx: Integer);
begin
   lua_rotate(L, idx, -1);
   lua_pop(L, 1);
end;

procedure lua_insert(L: Plua_State; idx: Integer);
begin
   lua_rotate(L, idx, 1);
end;

procedure lua_replace(L: Plua_State; idx: Integer);
begin
   lua_copy(L, -1, idx);
   lua_pop(L, 1);
end;

procedure lua_copy(L: Plua_State; fromidx, toidx: Integer); cdecl; external LUA_LIB_NAME;
function lua_checkstack(L: Plua_State; n: Integer): LongBool; cdecl; external LUA_LIB_NAME;
procedure lua_xmove(from, to_: Plua_State; n: Integer); cdecl; external LUA_LIB_NAME;
function lua_isnumber(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_isstring(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_iscfunction(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_isinteger(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_isuserdata(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_typename(L: Plua_State; tp: Integer): PAnsiChar; cdecl; external LUA_LIB_NAME;
function lua_tonumberx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Number; cdecl; external LUA_LIB_NAME;
function lua_tointegerx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Integer; cdecl; external LUA_LIB_NAME;
procedure lua_arith(L: Plua_State; op: Integer); cdecl; external LUA_LIB_NAME;
function lua_rawequal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_compare(L: Plua_State; idx1, idx2, op: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_toboolean(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_LIB_NAME;
function lua_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PAnsiChar; cdecl; external LUA_LIB_NAME;
function lua_rawlen(L: Plua_State; idx: Integer): lua_Unsigned; cdecl; external LUA_LIB_NAME;
function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl; external LUA_LIB_NAME;
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_LIB_NAME;
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl; external LUA_LIB_NAME;
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_LIB_NAME;
procedure lua_pushnil(L: Plua_State); cdecl; external LUA_LIB_NAME;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl; external LUA_LIB_NAME;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl; external LUA_LIB_NAME;
procedure lua_pushlstring(L: Plua_State; const s: PAnsiChar; len: size_t); cdecl; external LUA_LIB_NAME;
function lua_pushexternalstring(L: Plua_State; const s: PAnsiChar; len: size_t; falloc: lua_Alloc; ud: Pointer): PAnsiChar; cdecl; external LUA_LIB_NAME;
procedure lua_pushstring(L: Plua_State; const s: PAnsiChar); cdecl; external LUA_LIB_NAME;

procedure lua_pushstring(L: Plua_State; const s: AnsiString);
begin
   lua_pushlstring(L, PAnsiChar(s), Length(s));
end;

function lua_pushvfstring(L: Plua_State; const fmt: PAnsiChar; argp: Pointer): PAnsiChar; cdecl; external LUA_LIB_NAME;
function lua_pushfstring(L: Plua_State; const fmt: PAnsiChar): PAnsiChar; cdecl; varargs; external LUA_LIB_NAME;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_pushboolean(L: Plua_State; b: LongBool); cdecl; external LUA_LIB_NAME;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl; external LUA_LIB_NAME;
function lua_pushthread(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function lua_getglobal(L: Plua_State; const name: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function lua_gettable(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_getfield(L: Plua_state; idx: Integer; k: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function lua_geti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_rawget(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_rawgeti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_rawgetp(L: Plua_State; idx: Integer; p: Pointer): Integer; cdecl; external LUA_LIB_NAME;
procedure lua_createtable(L: Plua_State; narr, nrec: Integer); cdecl; external LUA_LIB_NAME;
function lua_newuserdatauv(L: Plua_State; sz: size_t; nuvalue: Integer): Pointer; cdecl; external LUA_LIB_NAME;
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_getiuservalue(L: Plua_State; idx, n: Integer): Integer; cdecl; external LUA_LIB_NAME;
procedure lua_setglobal(L: Plua_State; const name: PAnsiChar); cdecl; external LUA_LIB_NAME;
procedure lua_settable(L: Plua_State; idx: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_setfield(L: Plua_State; idx: Integer; k: PAnsiChar); cdecl; external LUA_LIB_NAME;
procedure lua_seti(L: Plua_State; idx: Integer; n: lua_Integer); cdecl; external LUA_LIB_NAME;
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_rawseti(L: Plua_State; idx: Integer; n: lua_Integer); cdecl; external LUA_LIB_NAME;
procedure lua_rawsetp(L: Plua_State; idx: Integer; p: Pointer); cdecl; external LUA_LIB_NAME;
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_setiuservalue(L: Plua_State; idx, n: Integer): Integer; cdecl; external LUA_LIB_NAME;
procedure lua_callk(L: Plua_State; nargs, nresults: Integer; ctx: lua_KContext; k: lua_KFunction); cdecl; external LUA_LIB_NAME;
function lua_pcallk(L: Plua_State; nargs, nresults, errfunc: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl; external LUA_LIB_NAME;
function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname, mode: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer; strip: Integer): Integer; cdecl; external LUA_LIB_NAME;
function lua_yieldk(L: Plua_State; nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl; external LUA_LIB_NAME;

procedure lua_call(L: Plua_State; nargs, nresults: Integer);
begin
   lua_callk(L, nargs, nresults, nil, nil);
end;

function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer;
begin
   Result := lua_pcallk(L, nargs, nresults, errf, nil, nil);
end;

function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer): Integer;
begin
   Result := lua_dump(L, writer, data, 1);
end;

function lua_yield(L: Plua_State; nresults: Integer): Integer;
begin
   Result := lua_yieldk(L, nresults, nil, nil);
end;

function lua_resume(L, from: Plua_State; narg: Integer; nres: PInteger): Integer; cdecl; external LUA_LIB_NAME;
function lua_status(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function lua_isyieldable(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
procedure lua_setwarnf(L: Plua_State; f: lua_WarnFunction; ud: Pointer); cdecl; external LUA_LIB_NAME;
procedure lua_warning(L: Plua_State; const msg: PAnsiChar; tocont: Integer); cdecl; external LUA_LIB_NAME;
function lua_gc(L: Plua_State; what: Integer): Integer; cdecl; varargs; external LUA_LIB_NAME;
function lua_error(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function lua_next(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB_NAME;
procedure lua_concat(L: Plua_State; n: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_len(L: Plua_State; idx: Integer); cdecl; external LUA_LIB_NAME;
function lua_numbertocstring(L: Plua_State; idx: Integer; buff: PAnsiChar): Cardinal; cdecl; external LUA_LIB_NAME;
function lua_stringtonumber(L: Plua_State; const s: PAnsiChar): size_t; cdecl; external LUA_LIB_NAME;
function lua_getallocf(L: Plua_State; ud: PPointer): lua_Alloc; cdecl; external LUA_LIB_NAME;
procedure lua_setallocf(L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl; external LUA_LIB_NAME;
procedure lua_toclose(L: Plua_State; idx: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_closeslot(L: Plua_State; idx: Integer); cdecl; external LUA_LIB_NAME;

function lua_getextraspace(L: Plua_State): Pointer;
const
  LUA_EXTRASPACE = sizeof(Pointer);
begin
   Result := L - LUA_EXTRASPACE;
end;

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number;
begin
   Result := lua_tonumberx(L, idx, nil);
end;

function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer;
begin
   Result := lua_tointegerx(L, idx, nil);
end;

procedure lua_pop(L: Plua_State; n: Integer);
begin
   lua_settop(L, - n - 1);
end;

procedure lua_newtable(L: Plua_State);
begin
   lua_createtable(L, 0, 0);
end;

procedure lua_register(L: Plua_State; const n: PAnsiChar; f: lua_CFunction);
begin
   lua_pushcfunction(L, f);
   lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);
begin
   lua_pushcclosure(L, f, 0);
end;

function lua_isfunction(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: Integer): Boolean;
begin
   Result := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L: Plua_State; s: PAnsiChar);
begin
   lua_pushlstring(L, s, Length(s));
end;

procedure lua_pushglobaltable(L: Plua_State);
begin
   lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
end;

function lua_tostring(L: Plua_State; i: Integer): PAnsiChar;
begin
   Result := lua_tolstring(L, i, nil);
end;

function lua_numbertointeger(n: lua_Number; p: Plua_Integer): Boolean;
begin
   Result := (n >= lua_Number(LUA_MININTEGER)) and
             (n < -lua_Number(LUA_MININTEGER));
   if Result then
      p^ := lua_Integer(Trunc(n));
end;

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl; external LUA_LIB_NAME;
function lua_getinfo(L: Plua_State; const what: PAnsiChar; ar: Plua_Debug): Integer; cdecl; external LUA_LIB_NAME;
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl; external LUA_LIB_NAME;
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl; external LUA_LIB_NAME;
function lua_getupvalue(L: Plua_State; funcindex, n: Integer): PAnsiChar; cdecl; external LUA_LIB_NAME;
function lua_setupvalue(L: Plua_State; funcindex, n: Integer): PAnsiChar; cdecl; external LUA_LIB_NAME;
function lua_upvalueid(L: Plua_State; funcindex, n: Integer): Pointer; cdecl; external LUA_LIB_NAME;
procedure lua_upvaluejoin(L: Plua_State; funcindex1, n1, funcindex2, n2: Integer); cdecl; external LUA_LIB_NAME;
procedure lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer); cdecl; external LUA_LIB_NAME;
function lua_gethook(L: Plua_State): lua_Hook; cdecl; external LUA_LIB_NAME;
function lua_gethookmask(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function lua_gethookcount(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;

procedure luaL_checkversion_(L: Plua_State; ver: lua_Number; sz: size_t); cdecl; external LUA_LIB_NAME;

procedure luaL_checkversion(L: Plua_State);
begin
   luaL_checkversion_(L, LUA_VERSION_NUM, LUAL_NUMSIZES);
end;

procedure luaL_traceback(L, L1: Plua_State; msg: PAnsiChar; level: Integer); cdecl; external LUA_LIB_NAME;
function luaL_argerror(L: Plua_State; arg: Integer; const extramsg: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_typeerror(L: Plua_State; arg: Integer; const tname: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_where(L: Plua_State; lvl: Integer); cdecl; external LUA_LIB_NAME;
function luaL_newmetatable(L: Plua_State; const tname: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_setmetatable(L: Plua_State; const tname: PAnsiChar); cdecl; external LUA_LIB_NAME;
function luaL_testudata(L: Plua_State; ud: Integer; const tname: PAnsiChar): Pointer; cdecl; external LUA_LIB_NAME;
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PAnsiChar): Pointer; cdecl; external LUA_LIB_NAME;
function luaL_error(L: Plua_State; const fmt: PAnsiChar): Integer; cdecl; varargs; external LUA_LIB_NAME;
function luaL_checkoption(L: Plua_State; arg: Integer; def: PAnsiChar; lst: PPAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PAnsiChar); cdecl; external LUA_LIB_NAME;
procedure luaL_checktype(L: Plua_State; arg, t: Integer); cdecl; external LUA_LIB_NAME;
procedure luaL_checkany(L: Plua_State; arg: Integer); cdecl; external LUA_LIB_NAME;
function luaL_checklstring(L: Plua_State; arg: Integer; l_: Psize_t): PAnsiChar; cdecl; external LUA_LIB_NAME;
function luaL_optlstring(L: Plua_State; arg: Integer; const def: PAnsiChar; l_: Psize_t): PAnsiChar; cdecl; external LUA_LIB_NAME;
function luaL_checknumber(L: Plua_State; arg: Integer): lua_Number; cdecl; external LUA_LIB_NAME;
function luaL_optnumber(L: Plua_State; arg: Integer; def: lua_Number): lua_Number; cdecl; external LUA_LIB_NAME;
function luaL_checkinteger(L: Plua_State; arg: Integer): lua_Integer; cdecl; external LUA_LIB_NAME;
function luaL_optinteger(L: Plua_State; arg: Integer; def: lua_Integer): lua_Integer; cdecl; external LUA_LIB_NAME;
function luaL_alloc(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl; external LUA_LIB_NAME;

procedure luaL_argcheck(L: Plua_State; cond: Boolean; arg: Integer; extramsg: PAnsiChar);
begin
   if not cond then
      luaL_argerror(L, arg, extramsg);
end;

procedure luaL_argexpected(L: Plua_State; cond: Boolean; arg: Integer; tname: PAnsiChar); inline;
begin
   if not cond then
      luaL_typeerror(L, arg, tname);
end;

function luaL_checkstring(L: Plua_State; n: Integer): PAnsiChar;
begin
   Result := luaL_checklstring(L, n, nil);
end;

function luaL_optstring(L: Plua_State; n: Integer; d: PAnsiChar): PAnsiChar;
begin
   Result := luaL_optlstring(L, n, d, nil);
end;

function luaL_typename(L: Plua_State; i: Integer): PAnsiChar;
begin
   Result := lua_typename(L, lua_type(L, i));
end;

function luaL_dofile(L: Plua_State; const filename: PAnsiChar): Integer;
begin
   Result := luaL_loadfile(L, filename);
   if Result = 0 then
      Result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;

function luaL_dostring(L: Plua_State; const str: PAnsiChar): Integer;
begin
   Result := luaL_loadstring(L, str);
   if Result = 0 then
      Result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;

procedure luaL_getmetatable(L: Plua_State; tname: PAnsiChar);
begin
   lua_getfield(L, LUA_REGISTRYINDEX, tname);
end;

function luaL_fileresult(L: Plua_State; stat: Integer; const fname: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_execresult(L: Plua_State; stat: Integer): Integer; cdecl; external LUA_LIB_NAME;
function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl; external LUA_LIB_NAME;
function luaL_loadfilex(L: Plua_State; const filename, mode: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_loadbufferx(L: Plua_State; const buff: PAnsiChar; sz: size_t; const name, mode: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;

function luaL_loadfile(L: Plua_State; const filename: PAnsiChar): Integer;
begin
   Result := luaL_loadfilex(L, filename, nil);
end;

function luaL_loadbuffer(L: Plua_State; const buff: PAnsiChar; size: size_t; const name: PAnsiChar): Integer;
begin
   Result := luaL_loadbufferx(L, buff, size, name, nil);
end;

procedure luaL_pushfail(L: Plua_State);
begin
  lua_pushnil(L);
end;

function luaL_loadstring(L: Plua_State; const s: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PAnsiChar; cdecl; external LUA_LIB_NAME;
procedure luaL_requiref(L: Plua_State; const modname: PAnsiChar; openf: lua_CFunction; glb: LongBool); cdecl; external LUA_LIB_NAME;
function luaL_makeseed(L: Plua_State): Cardinal; cdecl; external LUA_LIB_NAME;
procedure luaL_setfuncs(L: Plua_State; lr: PluaL_Reg; nup: Integer); cdecl; external LUA_LIB_NAME;

procedure luaL_setfuncs(L: Plua_State; lr: array of luaL_Reg; nup: Integer);
begin
   luaL_setfuncs(L, @lr, nup);
end;

procedure luaL_newlibtable(L: Plua_State; lr: array of luaL_Reg);
begin
   lua_createtable(L, 0, High(lr));
end;

procedure luaL_newlibtable(L: Plua_State; lr: PluaL_Reg);
var
  n: Integer;
begin
  n := 0;
  while lr^.name <> nil do begin
     inc(n);
     inc(lr);
  end;
  lua_createtable(L, 0, n);
end;

procedure luaL_newlib(L: Plua_State; lr: array of luaL_Reg);
begin
   luaL_checkversion(L);
   luaL_newlibtable(L, lr);
   luaL_setfuncs(L, @lr, 0);
end;

procedure luaL_newlib(L: Plua_State; lr: PluaL_Reg);
begin
   luaL_checkversion(L);
   luaL_newlibtable(L, lr);
   luaL_setfuncs(L, lr, 0);
end;

function luaL_gsub(L: Plua_State; const s, p, r: PAnsiChar): PAnsiChar; cdecl; external LUA_LIB_NAME;
function luaL_getsubtable(L: Plua_State; idx: Integer; const fname: PAnsiChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_newstate: Plua_State; cdecl; external LUA_LIB_NAME;
function luaL_len(L: Plua_State; idx: Integer): lua_Integer; cdecl; external LUA_LIB_NAME;

// Generic Buffer manipulation

procedure luaL_buffinit(L: Plua_State; B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
function luaL_prepbuffsize(B: PluaL_Buffer; sz: size_t): PAnsiChar; cdecl; external LUA_LIB_NAME;
procedure luaL_addlstring(B: PluaL_Buffer; const s: PAnsiChar; l: size_t); cdecl; external LUA_LIB_NAME;
procedure luaL_addstring(B: PluaL_Buffer; const s: PAnsiChar); cdecl; external LUA_LIB_NAME;
procedure luaL_addvalue(B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
procedure luaL_addgsub(B: PluaL_Buffer; const s, p, r: PAnsiChar); cdecl; external LUA_LIB_NAME;
procedure luaL_pushresult(B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
procedure luaL_pushresultsize(B: PluaL_Buffer; sz: size_t); cdecl; external LUA_LIB_NAME;
function luaL_buffinitsize(L: Plua_State; B: PluaL_Buffer; sz: size_t): PAnsiChar; cdecl; external LUA_LIB_NAME;

function luaL_prepbuffer(B: PluaL_Buffer): PAnsiChar;
begin
   Result := luaL_prepbuffsize(B, LUAL_BUFFERSIZE);
end;

procedure luaL_addchar(B: PluaL_Buffer; c: AnsiChar);
begin
   if B^.n >= B^.size then
      luaL_prepbuffsize(B, 1);
   B^.b[B^.n] := c;
   Inc(B^.n);
end;

procedure luaL_addsize(B: PluaL_Buffer; s: size_t);
begin
   B^.n := B^.n + s;
end;

procedure luaL_buffsub(B: PluaL_Buffer; n: Integer);
begin
   B^.n := B^.n - size_t(n);
end;

function luaL_bufflen(B: PluaL_Buffer): size_t;
begin
   Result := B^.n;
end;

function luaL_buffaddr(B: PluaL_Buffer): PAnsiChar;
begin
   Result := B^.b;
end;

// Standard libraries

function luaopen_base(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_coroutine(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_table(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_io(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_os(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_string(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_utf8(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_math(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_debug(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
function luaopen_package(L: Plua_State): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_openselectedlibs(L: Plua_State; load, preload: Integer); cdecl; external LUA_LIB_NAME;

procedure luaL_openlibs(L: Plua_State);
begin
   luaL_openselectedlibs(L, -1, 0);
end;

initialization
{$IFDEF MSWINDOWS}
   Set8087CW($133F);  // disable all floating-point exceptions
{$ENDIF}

(******************************************************************************
* Copyright (C) 1994-2025 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)

end.