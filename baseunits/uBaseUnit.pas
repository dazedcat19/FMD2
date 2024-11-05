{
        File: uBaseUnit.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uBaseUnit;

{$mode objfpc}{$H+}
{$MACRO ON}
{$modeswitch advancedrecords}

interface

uses
  {$ifdef windows}
  Windows,
  {$else}
  UTF8Process,
  {$endif}
  SysUtils, Classes, Graphics, LazFileUtils, LConvEncoding,
  strutils, dateutils, variants, base64, fpjson, jsonparser, jsonscanner,
  fgl, RegExpr, synautil, httpsend,
  synacode, MultiLog, FPimage, GZIPUtils, uMisc, httpsendthread, FMDOptions,
  ImgInfos, NaturalSortUnit,
  MemBitmap, FPWritePNG, zstream, FPReadPNG, VirtualTrees;

const
  LineEnding2 = LineEnding + LineEnding;

  UTF8BOM = #$EF#$BB#$BF;

  DATA_PARAM_LINK       = 0;
  DATA_PARAM_TITLE      = 1;
  DATA_PARAM_AUTHORS    = 2;
  DATA_PARAM_ARTISTS    = 3;
  DATA_PARAM_GENRES     = 4;
  DATA_PARAM_STATUS     = 5;
  DATA_PARAM_SUMMARY    = 6;
  DATA_PARAM_NUMCHAPTER = 7;
  DATA_PARAM_JDN        = 8;

  FILTER_HIDE = 0;
  FILTER_SHOW = 1;

  defaultGenres :array [0..37] of string =
    ('Action',       'Adult',        'Adventure',     'Comedy',
    'Doujinshi',     'Drama',        'Ecchi',         'Fantasy',
    'Gender Bender', 'Harem',        'Hentai',        'Historical',
    'Horror',        'Josei',        'Lolicon',       'Martial Arts',
    'Mature',        'Mecha',        'Musical',       'Mystery',
    'Psychological', 'Romance',      'School Life',   'Sci-fi',
    'Seinen',        'Shotacon',     'Shoujo',        'Shoujo Ai',
    'Shounen',       'Shounen Ai',   'Slice of Life', 'Smut',
    'Sports',        'Supernatural', 'Tragedy',       'Yaoi',
    'Yuri',          'Webtoons');

  Symbols: set of Char =
    ['\', '/', ':', '*', '?', '"', '<', '>', '|', #9, ';'];

  StringFilterChar: array [0..35] of array [0..1] of String = (
    (#10, '\n'),
    (#13, '\r'),
    ('&#x27;', ''''),
    ('&#33;', '!'),
    ('&#36;', '$'),
    ('&#37;', '%'),
    ('&#38;', '&'),
    ('&#39;', ''''),
    ('&#033;', '!'),
    ('&#036;', '$'),
    ('&#037;', '%'),
    ('&#038;', '&'),
    ('&#039;', ''''),
    ('&#8211;', '-'),
    ('&gt;', '>'),
    ('&lt;', '<'),
    ('&amp;', '&'),
    ('&ldquo;', '"'),
    ('&rdquo;', '"'),
    ('&quot;', '"'),
    ('&lsquo;', ''''),
    ('&rsquo;', ''''),
    ('&nbsp;', ' '),
    ('&cent;', '¢'),
    ('&pound;', '£'),
    ('&yen;', '¥'),
    ('&euro;', '©'),
    ('&copy;', '€'),
    ('&reg;', '®'),
    ('［', '['),
    ('］', ']'),
    ('（', '('),
    ('）', ')'),
    ('&frac12;', '½'),
    ('&deg;', '°'),
    ('&sup2;', '²')
    );

  HTMLEntitiesChar: array [0..82] of array [0..1] of String = (
    ('&#171;', '«'),
    ('&#176;', '°'),
    ('&Agrave;', 'À'),
    ('&#192;', 'À'),
    ('&Aacute;', 'Á'),
    ('&#193;', 'Á'),
    ('&Acirc;', 'Â'),
    ('&#194;', 'Â'),
    ('&Atilde;', 'Ã'),
    ('&ccedil;', 'ç'),
    ('&Egrave;', 'È'),
    ('&Eacute;', 'É'),
    ('&Ecirc;', 'Ê'),
    ('&#202;', 'Ê'),
    ('&Etilde;', 'Ẽ'),
    ('&Igrave;', 'Ì'),
    ('&Iacute;', 'Í'),
    ('&Itilde;', 'Ĩ'),
    ('&ETH;', 'Đ'),
    ('&Ograve;', 'Ò'),
    ('&Oacute;', 'Ó'),
    ('&Ocirc;', 'Ô'),
    ('&#212;', 'Ô'),
    ('&Otilde;', 'Õ'),
    ('&Ugrave;', 'Ù'),
    ('&Uacute;', 'Ú'),
    ('&Yacute;', 'Ý'),
    ('&#221;', 'Ý'),
    ('&agrave;', 'à'),
    ('&#224;', 'à'),
    ('&aacute;', 'á'),
    ('&#225;', 'á'),
    ('&acirc;', 'â'),
    ('&#226;', 'â'),
    ('&atilde;', 'ã'),
    ('&#227;', 'ã'),
    ('&#231;', 'ç'),
    ('&egrave;', 'è'),
    ('&#232;', 'è'),
    ('&eacute;', 'é'),
    ('&#233;', 'é'),
    ('&etilde;', 'ẽ'),
    ('&ecirc;', 'ê'),
    ('&#234;', 'ê'),
    ('&igrave;', 'ì'),
    ('&#236;', 'ì'),
    ('&iacute;', 'í'),
    ('&#237;', 'í'),
    ('&itilde;', 'ĩ'),
    ('&#238;', 'î'),
    ('&eth;', 'đ'),
    ('&ograve;', 'ò'),
    ('&#242;', 'ò'),
    ('&oacute;', 'ó'),
    ('&#243;', 'ó'),
    ('&ocirc;', 'ô'),
    ('&#244;', 'ô'),
    ('&otilde;', 'õ'),
    ('&#245;', 'õ'),
    ('&ugrave;', 'ù'),
    ('&#249;', 'ù'),
    ('&uacute;', 'ú'),
    ('&#250;', 'ú'),
    ('&yacute;', 'ý'),
    ('&#253;', 'ý'),
    ('&#8217;', ''''),
    ('&#8220;', '"'),
    ('&#8221;', '"'),
    ('&#8230;', '...'),
    ('&Auml;', 'Ä'),
    ('&auml;', 'ä'),
    ('&Ouml;', 'Ö'),
    ('&ouml;', 'ö'),
    ('&Uuml;', 'Ü'),
    ('&uuml;', 'ü'),
    ('&szlig;', 'ß'),
    ('&mu;', 'μ'),
    ('&#956;', 'μ'),
    ('&raquo;', '»'),
    ('&laquo;', '«'),
    ('&#8216;', '‘'),
    ('&ndash;', '-'),
    ('&gamma;', 'γ')
    );

  OPTION_MANGALIST = 0;
  OPTION_RECONNECT = 1;

  UNKNOWN_ERROR         = -1;
  NO_ERROR              = 0;
  NET_PROBLEM           = 1;
  INFORMATION_NOT_FOUND = 2;

  FMDFormatSettings :TFormatSettings = (
    CurrencyFormat            :1;
    NegCurrFormat             :5;
    ThousandSeparator         :',';
    DecimalSeparator          :'.';
    CurrencyDecimals          :2;
    DateSeparator             :'/';
    TimeSeparator             :':';
    ListSeparator             :',';
    CurrencyString            :'$';
    ShortDateFormat           :'m/d/y';
    LongDateFormat            :'dd" "mmmm" "yyyy';
    TimeAMString              :'AM';
    TimePMString              :'PM';
    ShortTimeFormat           :'hh:nn';
    LongTimeFormat            :'hh:nn:ss';
    ShortMonthNames           :('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames            :('January', 'February', 'March', 'April', 'May',
                                'June', 'July', 'August', 'September', 'October',
                                'November', 'December');
    ShortDayNames             :('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames              :('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                'Thursday', 'Friday', 'Saturday');
    TwoDigitYearCenturyWindow :50;
    );

  HTTPDateTimeFormatStr = 'ddd, dd mmm yyyy hh:nn:ss';

  // common regex to split host/url
  REGEX_HOST = '(?ig)^(\w+://)?([^/]*\.\w+)?(\:\d+)?(/?.*)$';

  ALPHA_LIST    = '#abcdefghijklmnopqrstuvwxyz';
  ALPHA_LIST_UP = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  MangaInfo_StatusCompleted = '0';
  MangaInfo_StatusOngoing = '1';

  FMDSupportedPackedOutputExt: array[0..3] of ShortString = ('.zip', '.cbz', '.pdf', '.epub');
  {$ifdef windows}
  // MAX_PATH = 260
  // MAX_PATH - 12 - 1
  MAX_PATHDIR = 247;
  // fmd max file extension = 4
  // max path + file in windows explorer is 259
  // = MAX_PATH - fmd max file extension - 1
  // 1 is pahtdelim "/"
  FMDMaxImageFilePath = 255;
  // if directory length is max_pathdir, the remaining allowed filename is 7
  // = 259 - fmd max file extension - 1
  {$endif}


  // custom rename
  CR_NUMBERING = '%NUMBERING%';
  CR_CHAPTER   = '%CHAPTER%';
  CR_WEBSITE   = '%WEBSITE%';
  CR_MANGA     = '%MANGA%';
  CR_AUTHOR    = '%AUTHOR%';
  CR_ARTIST    = '%ARTIST%';
  CR_FILENAME  = '%FILENAME%';

var
  Genre: array [0..37] of String;

  Revision: Cardinal;
  currentJDN: Integer;
  isExiting: Boolean = False;
type
  TArrayOfString = array of String;

  TCheckStyleType = (CS_DIRECTORY_COUNT, CS_DIRECTORY_PAGE, CS_INFO);
  TFlagType = (CS_GETPAGENUMBER, CS_GETPAGELINK, CS_DOWNLOAD);

  TFavoriteStatusType = (STATUS_IDLE, STATUS_CHECK, STATUS_CHECKING, STATUS_CHECKED);
  TFavoriteStatusTypes = set of TFavoriteStatusType;

  TMemory = Pointer;

  PMangaListItem = ^TMangaListItem;

  TMangaListItem = packed record
    Text: String;
    JDN: Longint;
  end;

  PSingleItem = ^TSingleItem;

  TSingleItem = packed record
    Text: String;
  end;

  PNamePointerItem = ^TNamePointerItem;

  TNamePointerItem = packed record
    Name: String;
    P: Pointer;
  end;

  PChapterStateItem = ^TChapterStateItem;

  TChapterStateItem = packed record
    Index: Integer;
    Title,
    Link: String;
    Downloaded: Boolean;
  end;

  PBaseMangaInfo = ^TBaseMangaInfo;

  TBaseMangaInfo = packed record
    title,
    authors,
    artists,
    genres,
    status,
    summary: String;
    numChapter: Integer;
  end;

  PMangaInfo = ^TMangaInfo;

  { TMangaInfo }

  TMangaInfo = class
  public
    URL,
    Title,
    Link,
    CoverLink,
    Authors,
    Artists,
    Genres,
    Status,
    Summary: String;
    NumChapter: Integer;
    ChapterNames,
    ChapterLinks: TStringList;
    Module: Pointer;
    constructor Create;
    destructor Destroy; override;
    function ModuleID: String; inline;
    function Website: String; inline;
    procedure Clear;
    function Clone: TMangaInfo;
  end;

  PDownloadInfo = ^TDownloadInfo;

  { TDownloadInfo }

  TDownloadInfo = packed record
    Link,
    Title,
    SaveTo,
    Status,
    Progress,
    TransferRate: String;
    DateAdded,
    DateLastDownloaded: TDateTime;
    iProgress: Integer;
    private
      FModule: Pointer;
      FModuleID: String;
      procedure SetModule(AValue: Pointer);
      procedure SetModuleID(AValue: String);
    public
      property ModuleID: String read FModuleID write SetModuleID;
      property Module: Pointer read FModule write SetModule;
      function Website: String;
  end;

  PFavoriteInfo = ^TFavoriteInfo;

  { TFavoriteInfo }

  TFavoriteInfo = packed record
    Link,
    Title,
    Status,
    SaveTo,
    Numbering,
    DownloadedChapterList,
    CurrentChapter: String;
    DateAdded,
    DateLastChecked,
    DateLastUpdated: TDateTime;
    private
      FModule: Pointer;
      FModuleID: String;
      procedure SetModule(AValue: Pointer);
      procedure SetModuleID(AValue: String);
    public
      property Module: Pointer read FModule write SetModule;
      property ModuleID: String read FModuleID write SetModuleID;
      function Website: String;
  end;

  TCardinalList = specialize TFPGList<Cardinal>;
  TByteList = specialize TFPGList<Byte>;

  TDownloadPageThread = class(TThread)
  protected
    procedure Execute; override;
  public
    isSuccess, isDone: Boolean;
    Retry: Integer;
    URL, Path: String;
    constructor Create(CreateSuspended: Boolean);
  end;

  { THTMLForm }

  THTMLForm = class
  private
    fdata: TStringList;
    fvalueseparator: String;
    fdelimiter: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Put(const AName, AValue: String);
    procedure Remove(const AName: String);
    function GetData: String;
    property ValueSeparator: String read fvalueseparator write fvalueseparator;
    property Delimiter: String read fdelimiter write fdelimiter;
    property Data: TStringList read fdata;
  end;


// graphics
function BlendColor(FG, BG: TColor; T: Byte): TColor;

// VT extras
procedure FilterVST(Tree: TVirtualStringTree; Key: String; Column: Integer = 0);

// Remove Unicode
function ReplaceUnicodeChar(const S, ReplaceStr: String): String;
// Check a directory to see if it's empty (return TRUE) or not
function IsDirectoryEmpty(const ADir: String): Boolean;
function CorrectFilePath(const APath: String): String;
function CorrectURL(const URL: String): String;
procedure CheckPath(const S: String);

// url
function FillURLProtocol(const AProtocol, AURL: String): String;

// modify url
function FillHost(const Host, URL: String): String; overload;
procedure FillHost(const Host: String; const URLs: TStrings); overload;
function MaybeFillHost(const Host, URL: String): String; overload;
procedure MaybeFillHost(const Host: String; const URLs: TStrings); overload;
function GetHostURL(URL: String): String;
function RemoveHostFromURL(URL: String): String;
procedure RemoveHostFromURLs(const URLs: TStringList);
procedure RemoveHostFromURLsPair(const URLs, Names: TStringList);
function EncodeCriticalURLElements(const URL: String): String;

//JSON
procedure ParseJSONArray(const S, Path: String; var OutArray: TStringList);

//convert charset to utf8
function ConvertCharsetToUTF8(S: String): String; overload;
procedure ConvertCharsetToUTF8(S: TStrings); overload;

// basic encrypt/decrypt string
function EncryptString(const s:String):String;
function DecryptString(const s:String):String;

// StringUtils
function PadZero(const S: String; ATotalWidth: Integer = 3;
  PadAll: Boolean = False; StripZero: Boolean = False): String;
procedure PadZeros(const S: TStrings; ATotalWidth: Integer = 3;
  PadAll: Boolean = False; StripZeros: Boolean = False);

// RegExpr
function RegExprGetMatch(const ARegExpr, AInputStr : RegExprString; const AMatchIdx: Integer): String;

// maintain the order of strings by adding serialized number if necessary
procedure SerializeAndMaintainNames(const F: TStrings);

function ShortenString(const S: String; const MaxWidth: Integer;
  const RightLength: Integer = 0; const EllipsisStr: String = '...'): String;

function TitleCase(const S: string): string;
function StringReplaceBrackets(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;
function StreamToString(const Stream: TStream): String;
procedure StringToStream(const S: String; Stream: TStream);
function GetRightValue(const Name, s: String): String;
function QuotedStr(const S: Integer): String; overload; inline;
function QuotedStrD(const S: String): String; overload; inline;
function QuotedStrD(const S: Integer): String; overload; inline;
function BracketStr(const S: String): String; inline;
function RandomString(SLength: Integer; ONumber: Boolean = False;
  OSymbol: Boolean = False; OSpace: Boolean = False): String;
function GetValuesFromString(Str: String; Sepr: Char): String;
procedure TrimStrings(TheStrings: TStrings);
procedure RemoveDuplicateStrings(Strs: array of TStringList; RemIndex: Integer = 0);
function MergeCaseInsensitive(Strs: array of String): String; overload;
function MergeCaseInsensitive(Strs: array of TStrings): String; overload;

function URLDecode(const s: String): String;
function HTMLDecode(const AStr: String): String;

function RemoveSymbols(const input: String): String;
function CorrectPathSys(const Path: String): String;
function RemovePathDelim(const Path: string): string;

function FixWhiteSpace(const S: String): String;
function CleanString(const S: String): String;
function CleanMultilinedString(const S: String; MaxLineEnding: Integer = 1): String;
function CleanAndExpandURL(const URL: String): String;
function CleanURL(const URL: String): String;
function AppendURLDelim(const URL: String): String;
function AppendURLDelimLeft(const URL: String): String;
function RemoveURLDelim(const URL: String): String; inline;
function RemoveURLDelimLeft(const URL: String): String; inline;
function FixURL(const URL: String): String;
function FixPath(const path: String): String;
function GetLastDir(const Dir: String): String;
function StringFilter(const Source: String): String;
function HTMLEntitiesFilter(const Source: String): String;
function CommonStringFilter(const Source: String): String;
function StringBreaks(const Source: String): String;
function BreaksString(const Source: String): String;
function RemoveBreaks(const Source: String): String;
function RemoveStringBreaks(const Source: String): String;
function RemoveDoubleSpace(const Source: String): String;
function TrimChar(const Source: String; const Chars: TSysCharSet): String;
function TrimLeftChar(const Source: String; const Chars: TSysCharSet): String;
function TrimRightChar(const Source: String; const Chars: TSysCharSet): String;

function StringOfString(c: String; l: Integer): String;
function IncStr(const S: String; N: Integer = 1): String; overload;
function IncStr(const I: Integer; N: Integer = 1): String; overload; inline;

//get heaader value from THTTPSend.Headers
function GetHeaderValue(const AHeaders: TStrings; HName: String): String;

// custom rename feature
function CustomRename(const AString, AWebsite, AMangaName, AAuthor, AArtist, AChapter, ANumbering: String;
  const AReplaceUnicode: Boolean;
  const AReplaceUnicodeStr: String;
  const AFileName: String = ''): String;

// Get substring from source
function GetString(const Source, sStart, sEnd: String): String;

function Find(const S: String; var List: TStringList; out index: Integer): Boolean;
function FindStrQuick(const s: String; var AStrings: TStringList): Boolean;

//parse google result urls
function GoogleResultURL(const AURL: String): String;
procedure GoogleResultURLs(const AURLs: TStrings);

// convert webp
function WebPToPNGStream(const AStream: TMemoryStream; const ALevel: Tcompressionlevel = clfastest): Boolean;
function WebPToJPEGStream(const AStream: TMemoryStream; const AQuality: Integer = 80): Boolean;

// convert png
function PNGToJPEGStream(const AStream: TMemoryStream; const AQuality: Integer = 80): Boolean;

// try to save tmemorystream to file, return the saved filename if success, otherwise return empty string
function SaveImageStreamToFile(Stream: TMemoryStream; Path, FileName: String; Age: LongInt = 0): String; overload;
function SaveImageStreamToFile(AHTTP: THTTPSend; Path, FileName: String): String; overload;

// check file exist with known extensions. AFilename is a filename without extensions
function ImageFileExists(const AFileName: String): Boolean;
function FindImageFile(const AFileName: String): String;

// load iamge from file with UTF8 aware
function LoadImageFromFileUTF8(const FileName: String; var Image: TFPCustomImage): Boolean;

// copy image from one image rect to dest point
procedure CopyImageRect(const Source, Dest: TFPCustomImage; const DestX, DestY: Integer; const SourceRect: TRect);

// merge 2 images to one
function Merge2Image(const Directory, ImgName1, ImgName2, FinalName: String; const Landscape: Boolean = False): Boolean;

function GetMimeType(const imgFileName: String): String;

// sort
function NaturalCompareStr(Str1, Str2: String): Integer; inline;
function NaturalCustomSort(List: TStringList; Index1, Index2: Integer): Integer; inline;
procedure QuickSortNaturalPart(var Alist: TStringList; Separator: String;
  PartIndex: Integer);

function GetStringPart(const S, Sep: String; PartIndex: Integer): String;

function DateToJDN(const date: TDate): Integer;
function GetCurrentJDN: Integer;
function JDNToDate(const JDN: Integer): TDate;

{function  ConvertInt32ToStr(const aValue: Cardinal)  : String;
function  ConvertStrToInt32(const aStr  : String): Cardinal;}
procedure TransferMangaInfo(var dest: TMangaInfo; const Source: TMangaInfo);
function MangaInfoStatusIfPos(const SearchStr: String; const OngoingStr: String = 'ongoing';
    const CompletedStr: String = 'complete'): String;

procedure GetBaseMangaInfo(const M: TMangaInfo; var B: TBaseMangaInfo);
// fill empty manga info
procedure FillBaseMangaInfo(const M: TMangaInfo; var B: TBaseMangaInfo);

// cross platform funcs

function fmdGetTempPath: String;
procedure fmdPowerOff;
procedure fmdHibernate;

// logger
{$IFNDEF DEBUGINFO}
type

  { TLoggerException }

  TLoggerException = class helper for TLogger
  public
    procedure SendExceptionStr(const AText: String); inline;
  end;
{$ENDIF}

procedure SendLog(const AText: String); overload; inline;
procedure SendLog(const AText, AValue: String); overload; inline;
procedure SendLog(const AText: String; const AValue: Variant); overload; inline;
procedure SendLog(const AText: String; AValue: TStrings); overload; inline;
procedure SendLogError(const AText: String); overload; inline;
procedure SendLogWarning(const AText: String); overload; inline;
procedure SendLogException(const AText: String; AException: Exception); inline;

implementation

uses
  frmMain, WebsiteModules, webp, DCPrijndael, DCPsha512, FPWriteJPEG;

{$IFDEF WINDOWS}
// thanks Leledumbo for the code
const
  SE_CREATE_TOKEN_NAME = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME = 'SeMachineAccountPrivilege';
  SE_TCB_NAME = 'SeTcbPrivilege';
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME = 'SeBackupPrivilege';
  SE_RESTORE_NAME = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
  SE_DEBUG_NAME = 'SeDebugPrivilege';
  SE_AUDIT_NAME = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME = 'SeManageVolumePrivilege';

function SetSuspendState(hibernate, forcecritical, disablewakeevent: Boolean): Boolean;
  stdcall; external 'powrprof.dll' Name 'SetSuspendState';
function IsHibernateAllowed: Boolean;
  stdcall; external 'powrprof.dll' Name 'IsPwrHibernateAllowed';
function IsPwrSuspendAllowed: Boolean;
  stdcall; external 'powrprof.dll' Name 'IsPwrSuspendAllowed';
function IsPwrShutdownAllowed: Boolean;
  stdcall; external 'powrprof.dll' Name 'IsPwrShutdownAllowed';
function LockWorkStation: Boolean; stdcall; external 'user32.dll' Name 'LockWorkStation';

function NTSetPrivilege(sPrivilege: String; bEnabled: Boolean): Boolean;
var
  hToken: THandle;
  TokenPriv: TOKEN_PRIVILEGES;
  PrevTokenPriv: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
begin
  Result := True;
  // Only for Windows NT/2000/XP and later.
  if not (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;
  Result := False;

  // obtain the processes token
  if OpenProcessToken(GetCurrentProcess(),
    TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  begin
    try
      // Get the locally unique identifier (LUID) .
      if LookupPrivilegeValue(nil, PChar(sPrivilege),
        TokenPriv.Privileges[0].Luid) then
      begin
        TokenPriv.PrivilegeCount := 1; // one privilege to set

        case bEnabled of
          True: TokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          False: TokenPriv.Privileges[0].Attributes := 0;
        end;

        ReturnLength := 0; // replaces a var parameter
        PrevTokenPriv := TokenPriv;

        // enable or disable the privilege

        AdjustTokenPrivileges(hToken, False, TokenPriv, SizeOf(PrevTokenPriv),
          PrevTokenPriv, ReturnLength);
      end;
    finally
      CloseHandle(hToken);
    end;
  end;
  // test the return value of AdjustTokenPrivileges.
  Result := GetLastError = ERROR_SUCCESS;
  if not Result then
    raise Exception.Create(SysErrorMessage(GetLastError));
end;

{$ENDIF}

function BlendColor(FG, BG: TColor; T: Byte): TColor;
  function MixByte(B1, B2: Byte): Byte;
  begin
    Result := Byte(T * (B1 - B2) shr 8 + B2);
  end;
var
  C1, C2: LongInt;
begin
  C1 := ColorToRGB(FG);
  C2 := ColorToRGB(BG);
  Result := (MixByte(Byte(C1 shr 16), Byte(C2 shr 16)) shl 16) +
    (MixByte(Byte(C1 shr 8), Byte(C2 shr 8)) shl 8) +
    MixByte(Byte(C1), Byte(C2));
end;

procedure FilterVST(Tree: TVirtualStringTree; Key: String; Column: Integer);
var
  s: String;
  node, xnode: PVirtualNode;
  v: Boolean;
begin
  if Tree.TotalCount = 0 then
    Exit;
  s := UpperCase(Key);
  Tree.BeginUpdate;
  try
    node := Tree.GetFirst();
    if s <> '' then
    begin
      while node <> nil do
      begin
        v := Pos(s, UpperCase(Tree.Text[node, Column])) <> 0;
        Tree.IsFiltered[node]:=not v;
        if v then
        begin
          xnode := node^.Parent;
          while (xnode <> nil)  and (xnode <> Tree.RootNode) do
          begin
            if (vsFiltered in xnode^.States) then
              Tree.IsFiltered[xnode] := False;
            xnode := xnode^.Parent;
          end;
        end;
        node := Tree.GetNext(node);
      end;
    end
    else
    begin
      while node <> nil do
      begin
        if (vsFiltered in node^.States) then
          Tree.IsFiltered[node] := False;
        node := Tree.GetNext(node);
      end;
    end;
  finally
    Tree.EndUpdate;
  end;
end;

function ReplaceUnicodeChar(const S, ReplaceStr: String): String;
var
  i: Integer;
  s1, s2, sr: UnicodeString;
begin
  Result := S;
  if Result = '' then Exit;
  s1 := UTF8Decode(S);
  s2 := UTF8Decode(ReplaceStr);
  sr := '';
  for i := 1 to Length(s1) do
  begin
    if (Ord(s1[i]) < 31) or (Ord(s1[i]) > 127) then
      sr := sr + s2
    else
      sr := sr + s1[i];
  end;
  Result := UTF8Encode(sr);
end;

function IsDirectoryEmpty(const ADir: String): Boolean;
var
  searchRec: TSearchRec;
begin
  try
    Result := (FindFirst(CorrectPathSys(ADir) + '*.*',
      faAnyFile{$ifdef unix} or faSymLink{$endif unix}, searchRec) = 0) and
      (FindNext(searchRec) = 0) and
      (FindNext(searchRec) <> 0);
  finally
    FindClose(searchRec);
  end;
end;

function CorrectURL(const URL: String): String;
begin
  Result := StringReplace(URL, ' ', '%20', [rfReplaceAll]);
end;

function CorrectFilePath(const APath: String): String;
var
  i: Integer;
begin
  Result := APath;
  if APath = '' then
    Exit('');
  for i := 1 to Length(Result) do
    if Result[i] = '\' then
      Result[i] := '/';
  if Result[Length(Result)] <> '/' then
    Result := Result + '/';
  while system.Pos('//', Result) > 0 do
    Result := StringReplace(Result, '//', '/', []);
end;

// took from an old project - maybe bad code
procedure CheckPath(const S: String);
var
  wS, lcS, lcS2: String;
  i, j: Integer;
begin
  wS := s;
  lcS2 := '';
  if wS[2] <> ':' then
  begin
    {$IFDEF WINDOWS}
    lcS2 := CorrectFilePath(FMD_DIRECTORY);
    {$ELSE}
    lcS2 := '';
    {$ENDIF}
    Insert('/', wS, 1);
  end
  else
  begin
    if Length(wS) = 2 then
      wS := wS + '/';
  end;
  for i := 1 to Length(wS) do
  begin
    lcS2 := lcS2 + wS[i];
    if (wS[i] = '/') and ((wS[i + 1] <> '/') or (wS[i + 1] <> ' ')) and
      (i < Length(wS)) then
    begin
      j := i + 1;
      lcS := '';
      repeat
        lcS := lcS + wS[j];
        if j = Length(wS) then
          Break;
        Inc(j);
      until wS[j] = '/';
      if not DirectoryExists(lcS2 + lcS) then
      begin
        CreateDirUTF8(lcS2 + lcS);
      end;
    end;
  end;
end;

function FillURLProtocol(const AProtocol, AURL: String): String;
begin
  Result := AURL;
  if AURL <> '' then begin
    Result := ReplaceRegExpr('^\w*:?//', AURL, '', False);
    if AProtocol <> '' then
      Result := AProtocol + Result;
  end;
end;

function FillHost(const Host, URL: String): String;
var
  P: String;
begin
  SplitURL(URL,nil,@P);
  Result:={%H-}RemoveURLDelim(Host)+P;
end;

procedure FillHost(const Host: String; const URLs: TStrings);
var
  i: Integer;
begin
  if (URLs=nil) or (URLs.Count=0) then Exit;
  for i:=0 to URLs.Count-1 do
    URLs[i]:=FillHost(Host,URLs[i]);
end;

function MaybeFillHost(const Host, URL: String): String;
var
  H,P: String;
begin
  SplitURL(URL,@H,@P);
  if (H='') and (P<>'') then Result:={%H-}RemoveURLDelim(Host)+P
  else Result:=URL;
end;

procedure MaybeFillHost(const Host: String; const URLs: TStrings);
var
  i: Integer;
begin
  if (URLs=nil) or (URLs.Count=0) then Exit;
  for i:=0 to URLs.Count-1 do
    URLs[i]:=MaybeFillHost(Host,URLs[i]);
end;

function GetHostURL(URL: String): String;
var
  H: String;
begin
  SplitURL(URL,@H,nil);
  Result:=H;
end;

function RemoveHostFromURL(URL: String): String;
begin
  SplitURL(URL,nil,@Result);
end;

procedure RemoveHostFromURLs(const URLs: TStringList);
var
  i: Integer;
begin
  if (URLs=nil) or (URLs.Count=0) then Exit;
  for i:=0 to URLs.Count-1 do
    URLs[i]:=RemoveHostFromURL(URLs[i]);
end;

procedure RemoveHostFromURLsPair(const URLs, Names: TStringList);
var
  i: Integer;
begin
  if (URLs=nil) or (Names=nil) or (URLs.Count<>Names.Count) or (URLs.Count=0) then Exit;
  i:=0;
  while i<URLs.Count do
  begin
    URLs[i]:=RemoveHostFromURL(URLs[i]);
    if URLs[i]<>'' then
      Inc(i)
    else
    begin
      URLs.Delete(i);
      Names.Delete(i);
    end;
  end;
end;

function EncodeCriticalURLElements(const URL: String): String;
var
  H,P: String;
begin
  SplitURL(URL,@H,@P);
  Result:=H+EncodeTriplet(P,'%',URLSpecialChar+URLFullSpecialChar-['/']);
end;

procedure ParseJSONArray(const S, Path: String; var OutArray: TStringList);
var
  P: TJSONParser;
  D: TJSONData;
  O: TJSONObject;
  i: Integer;
begin
  OutArray.BeginUpdate;
  P := TJSONParser.Create(Trim(S), [joUTF8]);
  try
    D := P.Parse;
    try
      if Assigned(D) then
        if (D.JSONType = jtArray) and (D.Count > 0) then
          for i := 0 to D.Count - 1 do
          begin
            O := TJSONObject(D.Items[i]);
            OutArray.Add(O.Strings[Path]);
          end;
    except
    end;
    D.Free;
  finally
    P.Free;
  end;
  OutArray.EndUpdate;
end;

function ConvertCharsetToUTF8(S: String): String;
var
  cs: String;
begin
  Result := S;
  if Trim(S) = '' then Exit;
  with TRegExpr.Create do
    try
      Expression := '(?ig)^.*<meta\s.*charset=([^''";\s]+).*$';
      if Exec(S) then begin
        cs := LowerCase(Replace(S, '$1', True));
        if cs = 'gb2312' then cs := EncodingCP936
        else if (cs = 'big5') or (cs = 'big5-hkscs') then cs := EncodingCP950;
      end
      else cs := GuessEncoding(S);
    finally
      Free;
    end;
  if cs <> '' then Result := ConvertEncoding(S, cs, 'utf8');
end;

procedure ConvertCharsetToUTF8(S: TStrings);
var
  cs: String;
  i: Integer;
begin
  if Trim(S.Text) = '' then Exit;
  cs := '';
  if S.Count > 1 then
  begin
    with TRegExpr.Create do
      try
        Expression := '(?ig)^.*<meta\s.*charset=([^''";\s]+).*$';
        for i := 0 to S.Count - 1 do
          if Pos('/head', S[i]) > 0 then Break
          else if Pos('<meta', S[i]) > 0 then
            if Exec(S[i]) then
            begin
              cs := LowerCase(Replace(S[i], '$1', True));
              if cs = 'gb2312' then cs := EncodingCP936
              else if (cs = 'big5') or (cs = 'big5-hkscs') then cs := EncodingCP950;
              Break;
            end;
      finally
        Free;
      end;
  end;
  if cs = '' then cs := GuessEncoding(S.Text);
  if cs <> '' then S.Text := ConvertEncoding(S.Text, cs, 'utf8');
end;

function StreamToString(const Stream: TStream): String;
Const
  BufSize = 1024;
  MaxGrow = 1 shl 29;
Var
  BytesRead,
  BufLen,
  I,BufDelta : Longint;
  OldPos: Int64;
begin
  Result:='';
  try
    OldPos:=Stream.Position;
    BufLen:=0;
    I:=1;
    Repeat
      BufDelta:=BufSize*I;
      SetLength(Result,BufLen+BufDelta);
      BytesRead:=Stream.Read(Result[BufLen+1],BufDelta);
      inc(BufLen,BufDelta);
      If I<MaxGrow then
        I:=I shl 1;
    Until BytesRead<>BufDelta;
    SetLength(Result,BufLen-BufDelta+BytesRead);
  finally
    Stream.Position:=OldPos;
  end;
end;

procedure StringToStream(const S: String; Stream: TStream);
begin
  Stream.Size := 0;
  Stream.Position := 0;
  Stream.Write(PAnsiChar(S)^, Length(s));
end;

function GetRightValue(const Name, s: String): String;
var
  i: Integer;
begin
  Result := '';
  if s = '' then Exit('');
  if Name = '' then Exit(s);
  i := Pos(Name, s);
  if i > 0 then
    Result := Trim(Copy(s, i + Length(Name), Length(s)));
end;

function QuotedStr(const S: Integer): String;
begin
  Result := AnsiQuotedStr(IntToStr(S), '''');
end;

function QuotedStrD(const S: String): String;
begin
  Result := AnsiQuotedStr(S, '"');
end;

function QuotedStrD(const S: Integer): String;
begin
  Result := AnsiQuotedStr(IntToStr(S), '"');
end;

function BracketStr(const S: String): String;
begin
  Result := '(' + S + ')';
end;

procedure ParseCommandLine(const cmd: String; var Output: TStrings;
  AStripQuotes: Boolean = False);
var
  s, cl: String;
  cq: Integer;
  acl, lq: Boolean;

  procedure Addcl;
  begin
    if cl <> '' then
    begin
      if AStripQuotes and (Length(cl) > 1) then
      begin
        if cl[1] = '"' then
          Delete(cl, 1, 1);
        if cl[Length(cl)] = '"' then
          Delete(cl, Length(cl), 1);
      end;
      Output.Add(cl);
      cl := '';
      acl := False;
    end;
  end;

begin
  if not Assigned(Output) then Exit;
  Output.Clear;
  Output.BeginUpdate;
  try
    s := cmd;
    cl := '';
    cq := 0;
    lq := False;
    while s <> '' do
    begin
      acl := True;
      if s[1] = '"' then
      begin
        Inc(cq);
        lq := True;
      end
      else
      begin
        if s[1] = ' ' then
        begin
          if cq > 0 then
          begin
            if (not odd(cq)) and lq then
            begin
              cq := 0;
              Addcl;
            end;
          end
          else
            Addcl;
        end;
        lq := False;
      end;
      if acl then
        cl := cl + s[1];
      Delete(s, 1, 1);
    end;
    Addcl;
  finally
    Output.EndUpdate;
  end;
end;

function RandomString(SLength: Integer; ONumber: Boolean; OSymbol: Boolean;
  OSpace: Boolean): String;
var
  sgen: String;
const
  alp = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  num = '0123456789';
  sym = '!@#$%^&*()_+[{]}\|;:''",<.>/?';
begin
  Result := '';
  if SLength = 0 then Exit;
  Randomize;
  sgen := alp;
  if ONumber then
    sgen := sgen + num;
  if OSymbol then
    sgen := sgen + sym;
  if OSpace then
    sgen := sgen + #32;
  repeat
    Result := Result + sgen[Random(Length(sgen)) + 1];
  until (Length(Result) = SLength);
end;

function GetValuesFromString(Str: String; Sepr: Char): String;
var
  p: Integer;
  s: String;
begin
  Result := '';
  if Str = '' then Exit;
  p := Pos(Sepr, Str);
  if p > 0 then
  begin
    p := p + Length(Sepr);
    s := Trim(Copy(Str, p, Length(Str)));
    if s <> '' then s := TrimChar(s, ['''', '"', ';', ' ']);
    Result := s;
  end;
end;

function URLDecode(const s: String): String;
var
  sAnsi: String;
  sUtf8: String;
  sWide: WideString;

  i, len: Integer;
  ESC: String[2];
  CharCode: Integer;
  c: Char;
begin
  sAnsi := PChar(s);
  SetLength(sUtf8, Length(sAnsi));
  i := 1;
  len := 1;
  while (i <= Cardinal(Length(sAnsi))) do begin
    if (sAnsi[i] <> '%') then begin
      if (sAnsi[i] = '+') then begin
        c := ' ';
      end else begin
        c := sAnsi[i];
      end;
      sUtf8[len] := c;
      Inc(len);
    end else begin
      Inc(i);
      ESC := Copy(sAnsi, i, 2);
      Inc(i, 1);
      try
        CharCode := StrToInt('$' + ESC);
        c := Char(CharCode);
        sUtf8[len] := c;
        Inc(len);
      except end;
    end;
    Inc(i);
  end;
  Dec(len);
  SetLength(sUtf8, len);

  sWide := UTF8Decode(sUtf8);
  len := Length(sWide);

  Result := {%H-}sWide;
end;

function HTMLDecode(const AStr: String): String;
var
  Sp, Rp, Cp, Tp: PChar;
  S: String;
  I, Code: Integer;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  Cp := Sp;
  try
    while Sp^ <> #0 do
    begin
      case Sp^ of
        '&': begin
          Cp := Sp;
          Inc(Sp);
          case Sp^ of
            'a': if AnsiStrPos(Sp, 'amp;') = Sp then  { do not localize }
              begin
                Inc(Sp, 3);
                Rp^ := '&';
              end;
            'l',
            'g': if (AnsiStrPos(Sp, 'lt;') = Sp) or (AnsiStrPos(Sp, 'gt;') = Sp) then
                { do not localize }
              begin
                Cp := Sp;
                Inc(Sp, 2);
                while (Sp^ <> ';') and (Sp^ <> #0) do
                  Inc(Sp);
                if Cp^ = 'l' then
                  Rp^ := '<'
                else
                  Rp^ := '>';
              end;
            'n': if AnsiStrPos(Sp, 'nbsp;') = Sp then  { do not localize }
              begin
                Inc(Sp, 4);
                Rp^ := ' ';
              end;
            'q': if AnsiStrPos(Sp, 'quot;') = Sp then  { do not localize }
              begin
                Inc(Sp, 4);
                Rp^ := '"';
              end;
            '#': begin
              Tp := Sp;
              Inc(Tp);
              while (Sp^ <> ';') and (Sp^ <> #0) do
                Inc(Sp);
              SetString(S, Tp, Sp - Tp);
              Val(S, I, Code);
              Rp^ := Chr((I));
            end;
            else
              Exit;
          end;
        end
        else
          Rp^ := Sp^;
      end;
      Inc(Rp);
      Inc(Sp);
    end;
  except
  end;
  SetLength(Result, Rp - PChar(Result));
end;

function RemoveSymbols(const input: String): String;
var
  i: Integer;
begin
  Result := input;
  if Result = '' then Exit;
  i := 1;
  while i <= Length(Result) do
    if CharInSet(Result[i], Symbols) then
      Delete(Result, i, 1)
    else
      Inc(i);
end;

procedure TrimStrings(TheStrings: TStrings);
var
  i: Integer;
begin
  if TheStrings = nil then Exit;
  if TheStrings.Count > 0 then
  begin
    i := 0;
    while i < TheStrings.Count do begin
      TheStrings[i] := Trim(TheStrings[i]);
      if TheStrings[i] = '' then TheStrings.Delete(i)
      else Inc(i);
    end;
  end;
end;

procedure RemoveDuplicateStrings(Strs: array of TStringList; RemIndex: Integer);
var
  i, j, k: Integer;
begin
  if Length(Strs) = 0 then
    Exit;
  if RemIndex > High(Strs) then
    Exit;
  i := 0;
  while i < Strs[RemIndex].Count do
  begin
    j := i + 1;
    while j < Strs[RemIndex].Count do
    begin
      if Strs[RemIndex].Strings[i] = Strs[RemIndex].Strings[j] then
      begin
        for k := 0 to High(Strs) do
          Strs[k].Delete(j);
      end
      else
        Inc(j);
    end;
    Inc(i);
  end;
end;

function MergeCaseInsensitive(Strs: array of String): String;
var
  s: TStringList;
  i: Integer;
begin
  if Length(Strs) = 0 then Exit;
  s := TStringList.Create;
  try
    s.CaseSensitive := False;
    s.Duplicates := dupIgnore;
    s.Sorted := True;
    for i := Low(Strs) to High(Strs) do
      s.AddText(Strs[i]);
    Result := s.Text;
  finally
    s.Free;
  end;
end;

function MergeCaseInsensitive(Strs: array of TStrings): String;
var
  s: TStringList;
  i: Integer;
begin
  if Length(Strs) = 0 then Exit;
  s := TStringList.Create;
  try
    s.CaseSensitive := False;
    s.sorted := True;
    s.Duplicates := dupIgnore;
    for i := Low(Strs) to High(Strs) do
      s.AddText(Strs[i].Text);
    Result := s.Text;
  finally
    s.Free;
  end;
end;

function CorrectPathSys(const Path: String): String;
{$IFDEF WINDOWS}
var
  s: UnicodeString;
{$ENDIF}
begin
  Result := FixWhiteSpace(Path);
  {$IFDEF WINDOWS}
  Result := RemovePathDelim(ExpandFileNameUTF8(TrimFilename(GetForcedPathDelims(Result)), FMD_DIRECTORY));
  Result := TrimRightChar(Result, ['.']);
  s := UTF8Decode(Result);
  if Length(s) > MAX_PATHDIR then
  begin
    if MainForm.cbOptionEnableLongNamePaths.Checked then
    begin
      if Pos('\\?\', s) = 0 then
        s := '\\?\' + s;
    end
    else
      SetLength(s, MAX_PATHDIR);
    Result := UTF8Encode(s);
  end;
  {$ELSE}
  Result := ExpandFileNameUTF8(TrimFilename(Path), FMD_DIRECTORY);
  {$ENDIF}
  Result := AppendPathDelim(Trim(Result));
end;

function RemovePathDelim(const Path: string): string;
begin
  Result := TrimRightChar(Path, AllowDirectorySeparators);
end;

function StringOfString(c: String; l: Integer): String;
var
  i: Integer;
begin
  Result := '';
  if c = '' then Exit;
  if l < 1 then Exit;
  for i := 1 to l do
    Result += c;
end;

function IncStr(const S: String; N: Integer): String;
var
  i: Integer;
begin
  Result := S;
  i := StrToIntDef(S, -1);
  if i > -1 then
  begin
    Inc(i, N);
    Result := IntToStr(i);
  end;
end;

function IncStr(const I: Integer; N: Integer): String;
begin
  Result := IntToStr(I + N);
end;

function GetHeaderValue(const AHeaders: TStrings; HName: String): String;
var
  i, p: Integer;
begin
  Result := '';
  if (AHeaders.Count > 0) and (HName <> '') then
  begin
    for i := 0 to AHeaders.Count - 1 do
    begin
      if (Pos(lowercase(HName), lowercase(AHeaders.Strings[i])) > 0) then
      begin
        p := Pos(':', AHeaders.Strings[i]);
        if p > 0 then
          Result := Copy(AHeaders.Strings[i], p + 2,
            Length(AHeaders.Strings[i]) - p - 1);
      end;
    end;
  end;
end;

const
  EncryptKey='B74945FB50E84FD58BF9FEAB8E4BEA6B';

function EncryptString(const s: String): String;
var
  aes: TDCP_rijndael;
begin
  result:='';
  if s='' then exit;
  aes:=TDCP_rijndael.Create(nil);
  try
     aes.InitStr(EncryptKey,TDCP_sha512);
     Result:=aes.EncryptString(s);
  finally
    aes.burn;
    aes.free;
  end;
end;

function DecryptString(const s: String): String;
var
  aes: TDCP_rijndael;
begin
  result:='';
  if s='' then exit;
  aes:=TDCP_rijndael.Create(nil);
  try
     aes.InitStr(EncryptKey,TDCP_sha512);
     result:=aes.DecryptString(s);
  finally
    aes.burn;
    aes.free;
  end;
end;

function PadZero(const S: String; ATotalWidth: Integer; PadAll: Boolean; StripZero: Boolean): String;

  function PadN(const SN: String): String;
  begin
    Result := SN;
    if StripZero and (Length(Result) > ATotalWidth) then
      while (Result[1] = '0') and (Length(Result) > ATotalWidth) do
        Delete(Result, 1, 1);
    if Length(Result) < ATotalWidth then
      Result := StringOfChar('0', ATotalWidth - Length(Result)) + Result;
  end;

var
  ls, i: Integer;
  n: String;
begin
  Result := '';
  if S = '' then Exit;
  ls := Length(S);
  i := 1;
  n := '';
  Result := '';
  repeat
    if S[i] in ['0'..'9'] then
      n := n + s[i]
    else
    begin
      if n <> '' then
      begin
        Result := Result + PadN(n);
        n := '';
        if PadAll then
        begin
          Result := Result + PadZero(Copy(S, i, ls), ATotalWidth, PadAll, StripZero);
          i := ls;
        end;
        Break;
      end;
      Result := Result + S[i];
    end;
    Inc(i);
  until i > ls;
  if n <> '' then
    Result := Result + PadN(n);
  if i < ls then
    Result := Result + Copy(S, i, ls);
end;

procedure PadZeros(const S: TStrings; ATotalWidth: Integer; PadAll: Boolean; StripZeros: Boolean);
var
  i: Integer;
begin
  if S = nil then Exit;
  if S.Count = 0 then Exit;
  S.BeginUpdate;
  try
    for i := 0 to S.Count - 1 do
      S[i] := PadZero(S[i], ATotalWidth, PadAll, StripZeros);
  finally
    S.EndUpdate;
  end;
end;

function RegExprGetMatch(const ARegExpr, AInputStr : RegExprString;
  const AMatchIdx: Integer): String;
begin
  Result := '';
  if AMatchIdx < 0 then Exit;
  with TRegExpr.Create do
    try
      Expression := ARegExpr;
      if Exec(AInputStr) then
        Result := Match[AMatchIdx];
    finally
      Free;
    end;
end;

procedure SerializeAndMaintainNames(const F: TStrings);
var
  s, so: TStringList;
  sameorder: Boolean;
  i, ls: Integer;
  fs: String;

  function identicalstrings(s1, s2: TStrings): Boolean;
  var
    j: Integer;
  begin
    Result := False;
    if s1.Count <> s2.Count then Exit;
    for j := 0 to s1.Count - 1 do
      if s1[j] <> s2[j] then
        Exit;
    Result := True;
  end;

  procedure checksorder;
  begin
    so.Clear;
    so.AddStrings(s);
    so.Sort;
    sameorder := identicalstrings(s, so);
  end;

begin
  if F = nil then Exit;
  if F.Count = 0 then Exit;
  s := TStringList.Create;
  F.BeginUpdate;
  try
    //try sorting it
    s.AddStrings(F);
    s.Sort;
    sameorder := identicalstrings(s, F);

    //try padzero
    if not sameorder then
    begin
      so := TStringList.Create;
      try
        ls := Length(IntToStr(F.Count));
        if ls < 3 then
          ls := 3;
        s.Clear;
        s.AddStrings(F);
        PadZeros(s, ls, False, False);
        checksorder;

        // add serializing number
        if not sameorder then
        begin
          s.Clear;
          s.AddStrings(F);
          fs := '%.' + IntToStr(ls) + 'd_%s';
          for i := 0 to s.Count - 1 do
            s[i] := Format(fs, [i + 1, s[i]]);
          checksorder;
        end;
      finally
        so.Free;
      end;
    end;
    if sameorder then
    begin
      F.Clear;
      F.AddStrings(s);
    end;
  finally
    F.EndUpdate;
    s.Free;
  end;
end;

function ShortenString(const S: String; const MaxWidth: Integer;
  const RightLength: Integer; const EllipsisStr: String): String;
var
  r: String;
begin
  Result := S;
  if Length(Result) > MaxWidth then
  begin
    if RightLength + Length(EllipsisStr) > MaxWidth then
    begin
      Result := RightStr(Result, MaxWidth);
      Exit;
    end;
    r := RightStr(Result, RightLength);
    SetLength(Result, MaxWidth - RightLength - Length(EllipsisStr));
    Result := Result + EllipsisStr + r;
  end;
end;

function TitleCase(const S: string): string;
begin
  Result := AnsiProperCase(S,
    [#9, #10, #13,
     ' ', '.', ',', '-', '+', '_', '=',
     '/', '\', '[', ']', '(', ')', '{', '}', '<', '>']);
end;

function StringReplaceBrackets(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;
var
  b1, b2: Char;
  p, r: String;
  i: Integer;
begin
  Result := Trim(S);
  if OldPattern = '' then Exit;
  p := Trim(OldPattern);
  r := Trim(NewPattern);
  b1 := #0;
  b2 := #0;
  i := Pos(p, Result);
  if i > 0 then begin
    if i > 1 then b1 := Result[i - 1];
    if i + Length(p) <= Length(Result) then b2 := Result[i + Length(p)];
    if b1 in ['(', '[', '{'] then p := b1 + p else b1 := #0;
    if b2 in [')', ']', '}'] then p := p + b2 else b2 := #0;
    if r <> '' then begin
      if b1 <> #0 then r := b1 + r;
      if b2 <> #0 then r := r + b2;
    end;
    Result := StringReplace(Result, p, r, Flags);
  end;
end;

function CustomRename(const AString, AWebsite, AMangaName, AAuthor, AArtist, AChapter,
  ANumbering: String;
  const AReplaceUnicode: Boolean;
  const AReplaceUnicodeStr: String;
  const AFileName: String): String;

  function FixStringLocal(const S: String): String;
  begin
    // fix htmlentities
    Result := CommonStringFilter(S);
    // remove unaccepted character (Windows)
    Result := RemoveSymbols(Result);
    // strip unicode character
    if AReplaceUnicode then
      Result := ReplaceUnicodeChar(Result, AReplaceUnicodeStr);
  end;

var
  fchapter: String;
begin
  Result := AString;

  // for rename chapter only
  if AChapter <> '' then begin
    // numbering/index
    if (Pos(CR_NUMBERING, Result) = 0) and (Pos(CR_CHAPTER, Result) = 0) then
      Result := ANumbering + Result;
    Result := StringReplaceBrackets(Result, CR_NUMBERING, ANumbering, [rfReplaceAll]);

    // pad number
    fchapter := Trim(AChapter);
    if OptionConvertDigitVolume then
    begin
      if OptionConvertDigitChapter then
        VolumeChapterPadZero(fchapter, OptionConvertDigitVolumeLength, OptionConvertDigitChapterLength)
      else
        VolumeChapterPadZero(fchapter, OptionConvertDigitVolumeLength, 0);
    end
    else
    if OptionConvertDigitChapter then
      VolumeChapterPadZero(fchapter, 0, OptionConvertDigitChapterLength);

    fchapter := FixStringLocal(fchapter);

    Result := StringReplaceBrackets(Result, CR_CHAPTER, fchapter, [rfReplaceAll]);

    if Result = '' then
      Result := ANumbering;
  end;

  Result := StringReplaceBrackets(Result, CR_WEBSITE, FixStringLocal(AWebsite), [rfReplaceAll]);
  Result := StringReplaceBrackets(Result, CR_MANGA, FixStringLocal(AMangaName), [rfReplaceAll]);
  Result := StringReplaceBrackets(Result, CR_AUTHOR, FixStringLocal(AAuthor), [rfReplaceAll]);
  Result := StringReplaceBrackets(Result, CR_ARTIST, FixStringLocal(AArtist), [rfReplaceAll]);
  Result := StringReplaceBrackets(Result, CR_FILENAME, FixStringLocal(AFileName), [rfReplaceAll]);
  if Result = '' then Result := FixStringLocal(AMangaName);

  if Result = '' then Exit;

  // remove pathdelim
  Result := TrimChar(Result, AllowDirectorySeparators);
end;

function GetString(const Source, sStart, sEnd: String): String;
var
  l: Integer;
  s: String;
begin
  Result := '';
  if Length(Source) > 0 then
  begin
    l := Pos(sStart, Source);
    if (l <> 0) and (Source[l + Length(sStart)] <> sEnd[1]) then
    begin
      s := RightStr(Source, Length(Source) - l - Length(sStart) + 1);
      l := Pos(sEnd, s);
      if (l <> 0) then
        Result := LeftStr(s, l - 1);
    end;
  end;
end;

function Find(const S: String; var List: TStringList; out index: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  index := -1;
  if List.Count = 0 then
    Exit;
  for i := 0 to List.Count - 1 do
  begin
    if CompareText(S, List.Strings[i]) = 0 then
    begin
      index := i;
      Result := True;
      Break;
    end;
  end;
end;

function FindStrQuick(const s: String; var AStrings: TStringList): Boolean;
var
  p: Integer;
begin
  if AStrings.Count > 0 then
  begin
    if not AStrings.Sorted then
      AStrings.Sorted := True;
    Result := AStrings.Find(s, p);
  end
  else
    Result := False;
end;

function FixWhiteSpace(const S: String): String;
const
  R: array [0..1] of string = (
    #$C2#$A0,     // no-break space  / &nbsp  U+00A0 #160
    #$EF#$BB#$BF  // zero width no-break / BOM U+FEFF
    );
var
  v: String;
begin
  Result := S;
  if Result = '' then Exit;
  for v in R do
    Result := StringReplace(Result, v, '', [rfReplaceAll]);
end;

function CleanString(const S: String): String;
begin
  Result := Trim(S);
  if Result = '' then Exit;
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #9, ' ', [rfReplaceAll]);
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
  Result := Trim(Result);
end;

function CleanMultilinedString(const S: String; MaxLineEnding: Integer): String;
var
  rn, rnp, n, np: String;
begin
  Result := Trim(s);
  if Result = '' then Exit;
  if MaxLineEnding < 1 then MaxLineEnding := 1;

  rn := StringOfString(#13#10, MaxLineEnding);
  rnp := rn + #13#10;
  while Pos(rnp, Result) > 0 do
    Result := StringReplace(Result, rnp, rn, [rfReplaceAll]);

  n := StringOfChar(#10, MaxLineEnding);
  np := n + #10;
  while Pos(np, Result) > 0 do
    Result := StringReplace(Result, np, n, [rfReplaceAll]);
end;

function CleanAndExpandURL(const URL: String): String;
begin
  Result := AppendURLDelim(CleanURL(URL));
end;

function CleanURL(const URL: String): String;
var
  x: Integer;
  p: String;
begin
  Result := Trim(URL);
  if Result = '' then Exit;
  if Pos(':', Result) = 1 then
    Delete(Result, 1, 1);
  if Pos('//', Result) = 1 then
    Delete(Result, 1, 2);
  p := '';
  x := Pos('://', Result);
  if x > 0 then
  begin
    x := x + 2;
    p := Copy(Result, 1, x);
    Delete(Result, 1, x);
    while Pos('/', Result) = 1 do
      Delete(Result, 1, 1);
  end;
  Result := ReplaceRegExpr('([^:])[\/]{2,}', Result, '$1/', True);
  Result := p + Result;
end;

function AppendURLDelim(const URL: String): String;
begin
  Result := URL;
  if (URL <> '') and (URL[Length(URL)] <> '/') then
    Result := URL + '/';
end;

function AppendURLDelimLeft(const URL: String): String;
begin
  Result := URL;
  if (URL <> '') and (URL[1] <> '/') then
    Result := '/' + URL;
end;

function RemoveURLDelim(const URL: String): String;
begin
  Result := TrimRightChar(URL, ['/']);
end;

function RemoveURLDelimLeft(const URL: String): String;
begin
  Result := TrimLeftChar(URL, ['/']);
end;

function FixURL(const URL: String): String;
begin
  Result := URL;
  if Pos(':', Result) or Pos('/', Result) > 0 then
    Result := TrimLeftChar(Result, [':', '/']);
end;

function FixPath(const path: String): String;
var
  i: Integer;
begin
  Result := path;
  if Length(path) = 0 then
    Exit;
  for i := 1 to Length(path) do
  begin
    if Byte(path[i]) >= 128 then
      Result := Result + '_'
    else
      Result := Result + path[i];
  end;
end;

function GetLastDir(const Dir: String): String;
var
  s: String;
  i: Integer;
begin
  Result := '';
  s := Trim(Dir);
  if s = '' then Exit;
  s := TrimRightChar(s, AllowDirectorySeparators);
  if s <> '' then
    for i := Length(s) downto 1 do
      if s[i] in AllowDirectorySeparators then
      begin
        Result := Copy(s, i + 1, Length(s) - i);
        Break;
      end;
  if Result = ''  then
    Result := s;
end;

function StringFilter(const Source: String): String;
var
  i: Integer;
begin
  Result := Source;
  if Length(Source) = 0 then
    Exit;

  for i := Low(StringFilterChar) to High(StringFilterChar) do
  begin
    if Pos(StringFilterChar[i, 0], LowerCase(Result)) > 0 then
      Result := StringReplace(Result, StringFilterChar[i, 0], StringFilterChar[i, 1],
        [rfIgnoreCase, rfReplaceAll]);
  end;

  // broken entities
  for i := Low(StringFilterChar) to High(StringFilterChar) do
  begin
    if Length(StringFilterChar[i, 0]) > 1 then
    begin
      if StringFilterChar[i, 0][Length(StringFilterChar[i, 0])] = ';' then
      begin
        if Pos(LeftStr(StringFilterChar[i, 0], Length(StringFilterChar[i, 0]) - 1),
          LowerCase(Result)) > 0 then
          Result := StringReplace(Result, LeftStr(StringFilterChar[i, 0],
            Length(StringFilterChar[i, 0]) - 1), StringFilterChar[i, 1],
            [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
  end;
end;

function HTMLEntitiesFilter(const Source: String): String;
var
  i: Integer;
begin
  Result := Source;
  if Length(Source) = 0 then
    Exit;

  for i := Low(HTMLEntitiesChar) to High(HTMLEntitiesChar) do
  begin
    if Pos(HTMLEntitiesChar[i, 0], Result) > 0 then
      Result := StringReplace(Result, HTMLEntitiesChar[i, 0], HTMLEntitiesChar[i, 1],
        [rfIgnoreCase, rfReplaceAll]);
  end;

  // broken entities
  for i := Low(HTMLEntitiesChar) to High(HTMLEntitiesChar) do
  begin
    if Length(HTMLEntitiesChar[i, 0]) > 1 then
    begin
      if HTMLEntitiesChar[i, 0][Length(HTMLEntitiesChar[i, 0])] = ';' then
      begin
        if Pos(LeftStr(HTMLEntitiesChar[i, 0], Length(HTMLEntitiesChar[i, 0]) - 1), Result) > 0 then
          Result := StringReplace(Result, LeftStr(HTMLEntitiesChar[i, 0],
            Length(HTMLEntitiesChar[i, 0]) - 1), HTMLEntitiesChar[i, 1],
            [rfIgnoreCase, rfReplaceAll]);
      end;
    end;
  end;
end;

function CommonStringFilter(const Source: String): String;
begin
  Result := Source;
  if Source = '' then Exit;
  Result := Trim(HTMLEntitiesFilter(StringFilter(Trim(Source))));
end;

function StringBreaks(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
end;

function BreaksString(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
end;

function RemoveBreaks(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
end;

function RemoveStringBreaks(const Source: String): String;
begin
  Result := Source;
  if Length(Result) = 0 then
    Exit;
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, '\n', '', [rfReplaceAll]);
  Result := StringReplace(Result, '\r', '', [rfReplaceAll]);
end;

function RemoveDoubleSpace(const Source: String): String;
begin
  Result := Source;
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll, rfIgnoreCase]);
end;

function TrimChar(const Source: String; const Chars: TSysCharSet): String;
begin
  Result := TrimLeftChar(Source, Chars);
  Result := TrimRightChar(Result, Chars);
end;

function TrimLeftChar(const Source: String; const Chars: TSysCharSet): String;
var
  i, j: Longint;
begin
  Result := Source;
  i := Length(Result);
  if i > 0 then
  begin
    j := 1;
    while (j <= i) and (Result[j] in Chars) do
      Inc(j);
    if j > 1 then
      Delete(Result, 1, j - 1);
  end;
end;

function TrimRightChar(const Source: String; const Chars: TSysCharSet): String;
var
  i, j: Longint;
begin
  Result := Source;
  i := Length(Result);
  if i > 0 then
  begin
    j := i;
    while (j > 0) and (Result[j] in Chars) do
      Dec(j);
    if j <> i then
      SetLength(Result, j);
  end;
end;

function GoogleResultURL(const AURL: String): String;
begin
  Result := AURL;
  if Pos('google.', LowerCase(AURL)) = 0 then Exit;
  Result := DecodeURL(ReplaceRegExpr('(?i)^.*google\..*\&url=([^\&]+)\&?.*$', AURL, '$1', True));
end;

procedure GoogleResultURLs(const AURLs: TStrings);
var
  i: Integer;
begin
  if AURLs.Count = 0 then Exit;
  if Pos('google.', LowerCase(AURLs.Text)) = 0 then Exit;
  with TRegExpr.Create('(?i)^.*google\..*\&url=([^\&]+)\&?.*$') do try
    for i := 0 to AURLs.Count - 1 do
      if Pos('google.', LowerCase(AURLs[i])) <> 0 then
        AURLs[i] := DecodeURL(Replace(AURLs[i], '$1', True));
  finally
    Free;
  end;
end;

function WebPToPNGStream(const AStream: TMemoryStream;
  const ALevel: Tcompressionlevel): Boolean;
var
  mem: TMemBitmap;
  writer: TFPWriterPNG;
begin
  Result := False;
  mem := nil;
  try
    mem := WebPToMemBitmap(AStream);
    if Assigned(mem) then
    try
      writer := TFPWriterPNG.create;
      writer.Indexed := False;
      writer.UseAlpha := mem.HasTransparentPixels;
      writer.CompressionLevel := ALevel;
      mem.SaveToStream(AStream, writer);
      Result := True;
    finally
      writer.Free;
    end;
  finally
    if Assigned(mem) then
      mem.Free;
  end;
end;

function WebPToJPEGStream(const AStream: TMemoryStream; const AQuality: Integer
  ): Boolean;
var
  mem: TMemBitmap;
  writer: TFPWriterJPEG;
begin
  Result := False;
  mem := nil;
  try
    mem := WebPToMemBitmap(AStream);
    if Assigned(mem) then
    try
      writer := TFPWriterJPEG.create;
      writer.CompressionQuality := AQuality;
      mem.SaveToStream(AStream, writer);
      Result := True;
    finally
      writer.Free;
    end;
  finally
    if Assigned(mem) then
      mem.Free;
  end;
end;

function PNGToJPEGStream(const AStream: TMemoryStream; const AQuality: Integer): Boolean;
var
  img: TFPCustomImage;
  writer: TFPWriterJPEG;
  reader: TFPReaderPNG;
begin
  Result := False;
  img := TFPMemoryImage.create(0,0);
  reader := TFPReaderPNG.create;
  try
    writer := nil;
    try
      img.LoadFromStream(AStream, reader);
      writer := TFPWriterJPEG.create;
      writer.CompressionQuality := AQuality;
      img.SaveToStream(AStream, writer);
      Result := True;
    except
    end;
    if writer <> nil then
      writer.Free;
  finally
    reader.Free;
    img.Free;
  end;
end;

function SaveImageStreamToFile(Stream: TMemoryStream; Path, FileName: String; Age: LongInt): String;
var
  p, f: String;
  fs: TFileStream;
begin
  Result := '';
  if Stream = nil then Exit;
  if Stream.Size = 0 then Exit;
  p := CorrectPathSys(Path);
  if ForceDirectories(p) then begin
    f := GetImageStreamExt(Stream);
    if f = 'png' then
    begin
      if OptionPNGSaveAsJPEG then
        if PNGToJPEGStream(Stream, OptionJPEGQuality) then f := 'jpg'
    end
    else
    if f = 'webp' then
    begin
      case OptionWebPSaveAs of
        1: if WebPToPNGStream(Stream, Tcompressionlevel(OptionPNGCompressionLevel)) then f := 'png';
        2: if WebPToJPEGStream(Stream, OptionJPEGQuality) then f := 'jpg';
      end;
    end;

    if f = '' then Exit;
    f := p + FileName + '.' + f;
    if MainForm.cbOptionEnableLongNamePaths.Checked then
    begin
      if Pos('\\?\', f) = 0 then
        f := '\\?\' + f;
    end;
    if FileExists(f) then DeleteFile(f);
    try
      fs := TFileStream.Create(f, fmCreate);
      try
        Stream.Position := 0;
        fs.CopyFrom(Stream, Stream.Size);
      finally
        fs.Free;
      end;
    except
      on E: Exception do
        SendLogException('SaveImageStreamToFile.WriteToFile Failed! ' + f, E);
    end;
    if FileExists(f) then
    begin
      Result := f;
      if Age > 0 then
        try
          FileSetDateUTF8(f, Age);
        except
          on E: Exception do
            SendLogException('SaveImageStreamToFile.FileSetDate Error! ' + f, E);
        end;
    end;
  end;
end;

function SaveImageStreamToFile(AHTTP: THTTPSend; Path, FileName: String): String;
var
  s: String;
  lastmodified: LongInt;
begin
  Result := '';
  if AHTTP = nil then Exit;
  s := Trim(AHTTP.Headers.Values['last-modified']);
  lastmodified := 0;
  if s <> '' then
    lastmodified := DateTimeToFileDate(DecodeRfcDateTime(s));
  Result := SaveImageStreamToFile(AHTTP.Document, Path, FileName, lastmodified);
end;

function ImageFileExists(const AFileName: String): Boolean;
begin
  Result := FindImageFile(AFileName) <> '';
end;

function FindImageFile(const AFileName: String): String;
var
  i: TImageHandlerRec;
  s: String;
begin
  for i in ImageHandlerMgr.List do
  begin
    s := AFileName + '.' + i.Ext;
    if FileExists(s) then
      Exit(s);
  end;
  Result := '';
end;

function LoadImageFromFileUTF8(const FileName: String; var Image: TFPCustomImage): Boolean;
var
  fs: TFileStream;
  h: TFPCustomImageReaderClass;
  r: TFPCustomImageReader;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  h := GetImageFileReaderClass(FileName);
  if h = nil then Exit;
  r := h.Create;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Image.LoadFromStream(fs, r);
    Result := True;
  finally
    r.Free;
    fs.Free;
  end;
end;

procedure CopyImageRect(const Source, Dest: TFPCustomImage; const DestX, DestY: Integer; const SourceRect: TRect);
var
  x, y, dx, dy: Integer;
begin
  dx := DestX;
  dy := DestY;
  for y := SourceRect.Top to SourceRect.Bottom -1 do
  begin
    for x := SourceRect.Left to SourceRect.Right - 1 do
    begin
      Dest.Colors[dx, dy] := Source.Colors[x, y];
      Inc(dx);
    end;
    Inc(dy);
  end;
end;

function Merge2Image(const Directory, ImgName1, ImgName2, FinalName: String; const Landscape: Boolean): Boolean;
var
  D, AImgName1, AImgName2, AFinalName: String;
  Img1, Img2, ImgNew: TFPCustomImage;
  newWidth: Integer;
  newHeigth: LongInt;
  h: TFPCustomImageWriterClass;
  w: TFPCustomImageWriter;
  fs: TFileStream;
begin
  Result := False;
  if not DirectoryExists(Directory) then Exit;
  D := CorrectPathSys(Directory);
  AImgName1 := D + ImgName1;
  AImgName2 := D + ImgName2;
  if not (FileExists(AImgName1) and FileExists(AImgName2)) then Exit;
  Img1 := TFPMemoryImage.create(0,0);
  Img2 := TFPMemoryImage.create(0,0);
  try
    if (LoadImageFromFileUTF8(AImgName1, Img1) and LoadImageFromFileUTF8(AImgName2, Img2)) then Exit;
    if Landscape then
    begin
      newWidth := img1.Width + img2.Width;
      newHeigth := max(img1.Height, img2.Height);
    end
    else
    begin
      newWidth := max(img1.Width, img2.Width);
      newHeigth := img1.Height + img2.Height;
    end;

    ImgNew := TFPMemoryImage.create(newWidth, newHeigth);
    try
      CopyImageRect(Img1, ImgNew, 0, 0, Rect(0, 0, Img1.Width, Img1.Height));
      if Landscape then
        CopyImageRect(Img2, ImgNew, Img1.Width + 1, 0, Rect(0, 0, Img2.Width, Img2.Height))
      else
        CopyImageRect(Img2, ImgNew, 0, Img1.Height + 1, Rect(0, 0, Img2.Width, Img2.Height));
      AFinalName := D + FinalName;
      if FileExists(AFinalName) then
        DeleteFile(AFinalName);
      if not FileExists(AFinalName) then
      begin
        h := {%H-}GetImageFileWriterClass(AImgName1);
        if h = nil then Exit;
        try
          w := h.Create;
          fs := TFileStream.Create(AFinalName, fmCreate);
          ImgNew.SaveToStream(fs, w);
          Result := True;
        finally
          w.Free;
          fs.Free;
        end;
        if Result then
        begin
          DeleteFile(AImgName1);
          DeleteFile(AImgName2);
        end;
      end;
    finally
      ImgNew.Free;
    end;
  finally
    Img1.Free;
    Img2.Free;
  end;
end;

function GetMimeType(const imgFileName: String): String;
begin
  case ExtractFileExt(imgFileName) of
    '.jpeg', '.jpg': Result := 'image/jpeg';
    '.png': Result := 'image/png';
    '.gif': Result := 'image/gif';
    '.bmp': Result := 'image/bmp';
    '.webp': Result := 'image/webp';
    else Result := '';
  end;
end;

function NaturalCompareStr(Str1, Str2: String): Integer;
begin
  Result := NaturalSortUnit.UTF8LogicalCompareText(Str1, Str2);
end;

function NaturalCustomSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := NaturalCompareStr(List[Index1], List[Index2]);
end;

procedure QuickSortNaturalPart(var Alist: TStringList; Separator: String;
  PartIndex: Integer);

  function CompareFn(Index1, Index2: Integer): Integer;
  begin
    Result := NaturalCompareStr(getStringPart(Alist[Index1], Separator, PartIndex),
      getStringPart(Alist[Index2], Separator, PartIndex));
  end;

  procedure QSort(L, R: Integer);
  var
    Pivot, vL, vR: Integer;
  begin
    if R - L <= 1 then begin // a little bit of time saver
      if L < R then
        if CompareFn(L, R) > 0 then
          Alist.Exchange(L, R);

      Exit;
    end;

    vL := L;
    vR := R;

    Pivot := L + Random(R - L); // they say random is best

    while vL < vR do begin
      while (vL < Pivot) and (CompareFn(vL, Pivot) <= 0) do
        Inc(vL);

      while (vR > Pivot) and (CompareFn(vR, Pivot) > 0) do
        Dec(vR);

      Alist.Exchange(vL, vR);

      if Pivot = vL then // swap pivot if we just hit it from one side
        Pivot := vR
      else if Pivot = vR then
        Pivot := vL;
    end;

    if Pivot - 1 >= L then
      QSort(L, Pivot - 1);
    if Pivot + 1 <= R then
      QSort(Pivot + 1, R);
  end;

begin
  if Alist.Count < 2 then Exit;
  Alist.BeginUpdate;
  try
    QSort(0, Alist.Count - 1);
  finally
    Alist.EndUpdate;
  end;
end;

function GetStringPart(const S, Sep: String; PartIndex: Integer): String;
var
  i, j, lpos, rpos: Integer;
begin
  lpos := 1;
  rpos := 1;
  Result := '';

  for i := 0 to partIndex do
  begin
    j := PosEx(Sep, S, rpos);
    if (j > 0) then
    begin
      lpos := rpos;
      rpos := j + Length(Sep);
    end
    else
      Break;
  end;
  Result := Copy(S, lpos, rpos - lpos - Length(Sep));
end;

function DateToJDN(const date: TDate): Integer;
var
  day, month, year: Word;
  a, y, m: Integer;
begin
  DecodeDate(date, year, month, day);
  a := (14 - month) div 12;
  y := year + 4800 - a;
  m := month + (12 * a) - 3;
  Result := Round((day + ((153 * m + 2) div 5) + (365 * y) + (y div 4) - (y div 100) +
    (y div 400) - 32045) - 0.5);
end;

function JDNToDate(const JDN: Integer): TDate;
var
  a, b, c, d, e, m: Longint;
  day, month, year: Word;
begin
  a := trunc(JDN + 32044.5);
  b := (4 * a + 3) div 146097;
  c := a - (146097 * b div 4);
  d := (4 * c + 3) div 1461;
  e := c - (1461 * d div 4);
  m := (5 * e + 2) div 153;
  day := e - ((153 * m + 2) div 5) + 1;
  month := m + 3 - 12 * (m div 10);
  year := (100 * b) + d - 4800 + (m div 10);
  Result := EncodeDate(year, month, day);
end;

function GetCurrentJDN: Integer;
begin
  Result := DateToJDN(Now);
end;

procedure TransferMangaInfo(var dest: TMangaInfo; const Source: TMangaInfo);
begin
  dest.URL := Source.URL;
  dest.Title := Source.Title;
  dest.Link := Source.Link;
  dest.Module := Source.Module;
  dest.CoverLink := Source.CoverLink;
  dest.Authors := Source.Authors;
  dest.Artists := Source.Artists;
  dest.Genres := Source.Genres;
  dest.Status := Source.Status;
  dest.Summary := Source.Summary;
  dest.NumChapter := Source.NumChapter;
  dest.ChapterNames.Assign(Source.ChapterNames);
  dest.ChapterLinks.Assign(Source.ChapterLinks);
end;

function MangaInfoStatusIfPos(const SearchStr: String; const OngoingStr: String;
  const CompletedStr: String): String;
var
  s, o, c: String;
  function searchMany(const t: String): Boolean;
  var
    i: String;
  begin
    if Pos('|', t) = 0 then
       Result := Pos(t, s) <> 0
    else
    begin
      for i in t.Split(['|']) do
        if Pos(i, s) <> 0 then
          Exit(True);
      Result := False
    end;
  end;
begin
  Result := '';
  if SearchStr = '' then Exit;
  s := LowerCase(SearchStr);
  o := LowerCase(OngoingStr);
  c := LowerCase(CompletedStr);
  if o <> '' then
  begin
    if searchMany(o) then
      Result := MangaInfo_StatusOngoing
    else if c = '' then
      Result := MangaInfo_StatusCompleted
    else if searchMany(c) then
      Result := MangaInfo_StatusCompleted
  end
  else if c <> '' then
  begin
    if searchMany(c) then
      Result := MangaInfo_StatusCompleted
    else if searchMany(o) then
      Result := MangaInfo_StatusOngoing;
  end;
end;

procedure GetBaseMangaInfo(const M: TMangaInfo; var B: TBaseMangaInfo);
begin
  B.title := M.Title;
  B.authors := M.Authors;
  B.artists := M.Artists;
  B.genres := M.Genres;
  B.status := M.Status;
  B.summary := M.Summary;
  B.numChapter := M.NumChapter;
end;

procedure FillBaseMangaInfo(const M: TMangaInfo; var B: TBaseMangaInfo);
begin
  if Trim(M.Title) = '' then M.Title := B.title;
  if Trim(M.Authors) = '' then M.Authors := B.authors;
  if Trim(M.Artists) = '' then M.Artists := B.artists;
  if Trim(M.Genres) = '' then M.Genres := B.genres;
  if Trim(M.Status) = '' then M.Status := B.status;
  if Trim(M.Summary) = '' then M.Summary := B.summary;
  if M.NumChapter = 0 then M.NumChapter := B.numChapter;
end;

{ TDownloadInfo }

procedure TDownloadInfo.SetModuleID(AValue: String);
begin
  if FModuleID = AValue then Exit;
  FModuleID := AValue;
  FModule := Modules.LocateModule(FModuleID);
end;

procedure TDownloadInfo.SetModule(AValue: Pointer);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
  if Assigned(FModule) then
    FModuleID := TModuleContainer(FModule).ID;
end;

function TDownloadInfo.Website: String;
begin
  if Assigned(FModule) then
    Result := TModuleContainer(FModule).Name
  else
    Result := '';
end;

{ TFavoriteInfo }

procedure TFavoriteInfo.SetModuleID(AValue: String);
begin
  if FModuleID = AValue then Exit;
  FModuleID := AValue;
  FModule := Modules.LocateModule(FModuleID);
end;

procedure TFavoriteInfo.SetModule(AValue: Pointer);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
  if Assigned(FModule) then
    FModuleID := TModuleContainer(FModule).ID
  else
    FModuleID := '';
end;

function TFavoriteInfo.Website: String;
begin
  if Assigned(FModule) then
    Result := TModuleContainer(FModule).Name
  else
    Result := '';
end;

{ THTMLForm }

constructor THTMLForm.Create;
begin
  fdata := TStringList.Create;
  fdata.NameValueSeparator := '=';
  fdata.Delimiter := '&';
  fvalueseparator := '=';
  fdelimiter := '&';
end;

destructor THTMLForm.Destroy;
begin
  fdata.Free;
  inherited Destroy;
end;

procedure THTMLForm.Put(const AName, AValue: String);
begin
  fdata.Values[AName] := AValue;
end;

procedure THTMLForm.Remove(const AName: String);
var
  i: Integer;
begin
  i := fdata.IndexOfName(AName);
  if i > -1 then fdata.Delete(i);
end;

function THTMLForm.GetData: String;
var
  i: Integer;
begin
  Result := '';
  if fdata.Count > 0 then
    for i := 0 to fdata.Count - 1 do begin
      if Result <> '' then Result := Result + fdelimiter;
      Result := Result + fdata.Names[i] + fvalueseparator + EncodeURLElement(fdata.ValueFromIndex[i]);
    end;
end;

{ TMangaInfo }

constructor TMangaInfo.Create;
begin
  inherited Create;
  ChapterNames := TStringList.Create;
  ChapterLinks := TStringList.Create;
end;

destructor TMangaInfo.Destroy;
begin
  ChapterNames.Free;
  ChapterLinks.Free;
  inherited Destroy;
end;

function TMangaInfo.ModuleID: String;
begin
  Result := TModuleContainer(Module).ID;
end;

function TMangaInfo.Website: String;
begin
  Result := TModuleContainer(Module).Name;
end;

procedure TMangaInfo.Clear;
begin
  URL := '';
  Title := '';
  Link := '';
  CoverLink := '';
  Authors := '';
  Artists := '';
  Genres := '';
  Status := '';
  Summary := '';
  NumChapter := 0;
  ChapterNames.Clear;
  ChapterLinks.Clear;
  Module := nil;
end;

function TMangaInfo.Clone: TMangaInfo;
begin
  Result := TMangaInfo.Create;
  Result.URL := URL;
  Result.Title := Title;
  Result.Link := Link;
  Result.CoverLink := CoverLink;
  Result.Authors := Authors;
  Result.Artists := Artists;
  Result.Genres := Genres;
  Result.Status := Status;
  Result.Summary := Summary;
  Result.NumChapter := NumChapter;
  Result.ChapterNames.AddStrings(ChapterNames);
  Result.ChapterLinks.AddStrings(ChapterLinks);
  Result.Module := Module;
end;

constructor TDownloadPageThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  isDone := False;
  FreeOnTerminate := True;
end;

procedure TDownloadPageThread.Execute;
begin
  isDone := True;
  SuspendThread(Self.Handle);
end;

// OS dependent
function fmdGetTempPath: String;
var
  l: Integer;
begin
{$IFDEF WINDOWS}
  SetLength(Result, 4096);
  l := GetTempPath(4096, PChar(Result));
  SetLength(Result, l + 1);
{$ENDIF}
{$IFDEF UNIX}
  Result := GetTempDir(False);
{$ENDIF}
end;

procedure fmdPowerOff;
begin
{$IFDEF WINDOWS}
  if IsPwrShutdownAllowed then
  begin
    NTSetPrivilege(SE_SHUTDOWN_NAME, True);
    ExitWindowsEx(EWX_POWEROFF or EWX_FORCE, 0);
  end;
{$ENDIF}
{$IFDEF UNIX}
  // This process require admin rights in order to execute
  with TProcessUTF8.Create(nil) do try
      CommandLine := 'poweroff';
      Execute;
    finally
      Free;
    end;
{$ENDIF}
end;

procedure fmdHibernate;
begin
  {$IFDEF WINDOWS}
  SetSuspendState(True, False, False);
  {$ENDIF}
end;

{$IFNDEF DEBUGINFO}
procedure TLoggerException.SendExceptionStr(const AText: String);
begin
  SendStream(ltException, AText, nil);
end;
{$ENDIF}
procedure SendLog(const AText: String);
begin
  Logger.Send(AText);
end;

procedure SendLog(const AText, AValue: String);
begin
  Logger.Send(AText, AValue);
end;

procedure SendLog(const AText: String; const AValue: Variant);
begin
  Logger.Send(AText, VarToStr(AValue));
end;

procedure SendLog(const AText: String; AValue: TStrings);
begin
  Logger.Send(AText, AValue);
end;

procedure SendLogError(const AText: String);
begin
  Logger.SendError(AText);
end;

procedure SendLogWarning(const AText: String);
begin
  Logger.SendWarning(AText);
end;

procedure SendLogException(const AText: String; AException: Exception);
begin
  {$ifdef DEBUGINFO}
  Logger.SendException(AText, AException);
  {$else}
  Logger.SendExceptionStr(AText + ' ' + TrimLeft(AException.Message));
  {$endif}
end;

function HeaderByName(const AHeaders: TStrings; const AHeaderName: String): String;
var
  i, p: Integer;
  hn: String;
begin
  Result := '';
  if AHeaders.Count < 1 then
    Exit;
  hn := AHeaderName;
  //if hn[Length(hn)] <> ':' then
  //  hn := hn + ':';
  for i := 0 to AHeaders.Count - 1 do
  begin
    p := Pos(LowerCase(hn), LowerCase(AHeaders.Strings[i]));
    if p > 0 then
    begin
      p := Pos(':', AHeaders.Strings[i]);
      if p > 0 then
      begin
        Result := Copy(AHeaders.Strings[i], p + 2, Length(AHeaders.Strings[i]) - p + 1);
        Break;
      end;
    end;
  end;
end;

end.
