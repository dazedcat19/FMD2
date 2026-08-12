unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fileinfo, jsonini, FileUtil, Forms, Graphics,
  LazFileUtils;

type

  TFMDDo = (DO_NOTHING, DO_EXIT, DO_POWEROFF, DO_HIBERNATE, DO_UPDATE);
  TFMDDoSet = Set of TFMDDo;

  { TGeneral }

  TGeneral = class(TObject)
  private
    FLanguage,
    FSelectedMangaLists,
    FExtProgramPath,
    FExtProgramParam: String;

    FAfterFMDDo,
    FLetFMDDoAfterFinish: TFMDDo;

    FLetFMDDoAfterFinishInt,
    FTheme,
    FMangaDaysNew: Integer;

    FMinOnStart,
    FMinToTray,
    FOneFMDInstance,
    FListLiveSearch,
    FMangaLoadAddToList,
    FHghlghtNewManga,
    FSortChptrListAsc,
    FChptrListHideDwnlded,
    FHghlghtDwnldedChptrs,
    FDelCompltdDLOnClose,
    FAddNewDLAsStopped,
    FSortDLAddNew,
    FDBVacuumExit,
    FLongNamePaths: Boolean;

    procedure SetExtProgramPath(Const AValue: String);
    procedure SetExtProgramParam(Const AValue: String);

    procedure SetAfterFMDDo(Const AValue: TFMDDO);
    procedure SetLetFMDDoAfterFinish(Const AValue: TFMDDO);

    procedure SetLetFMDDoAfterFinishInt(Const AValue: Integer);
    procedure SetTheme(Const AValue: Integer);
    procedure SetMangaDaysNew(Const AValue: Integer);
  public
    property Language: String read FLanguage;
    property SelectedMangaLists: String read FSelectedMangaLists; 
    property ExtProgramPath: String read FExtProgramPath write SetExtProgramPath;
    property ExtProgramParam: String read FExtProgramParam write SetExtProgramParam;

    property AfterFMDDo: TFMDDO read FAfterFMDDo write SetAfterFMDDo;
    property LetFMDDoAfterFinish: TFMDDO read FLetFMDDoAfterFinish write SetLetFMDDoAfterFinish;

    property LetFMDDoAfterFinishInt: Integer read FLetFMDDoAfterFinishInt write SetLetFMDDoAfterFinishInt;
    property Theme: Integer read FTheme write SetTheme;
    property MangaDaysNew: Integer read FMangaDaysNew write FMangaDaysNew;

    property MinOnStart: Boolean read FMinOnStart write FMinOnStart;
    property MinToTray: Boolean read FMinToTray write FMinToTray;
    property OneFMDInstance: Boolean read FOneFMDInstance write FOneFMDInstance;
    property ListLiveSearch: Boolean read FListLiveSearch write FListLiveSearch;
    property MangaLoadAddToList: Boolean read FMangaLoadAddToList write FMangaLoadAddToList;
    property HghlghtNewManga: Boolean read FHghlghtNewManga write FHghlghtNewManga;
    property SortChptrListAsc: Boolean read FSortChptrListAsc write FSortChptrListAsc;
    property ChptrListHideDwnlded: Boolean read FChptrListHideDwnlded write FChptrListHideDwnlded;
    property HghlghtDwnldedChptrs: Boolean read FHghlghtDwnldedChptrs write FHghlghtDwnldedChptrs;
    property DelCompltdDLOnClose: Boolean read FDelCompltdDLOnClose write FDelCompltdDLOnClose;
    property AddNewDLAsStopped: Boolean read FAddNewDLAsStopped write FAddNewDLAsStopped;
    property SortDLAddNew: Boolean read FSortDLAddNew write FSortDLAddNew;
    property DBVacuumExit: Boolean read FDBVacuumExit write FDBVacuumExit;
    property LongNamePaths: Boolean read FLongNamePaths write FLongNamePaths;

    constructor Create;
                             
    procedure SetLanguage;
    procedure SetSelectedMangaLists;
    function CheckAfterFMDDo(Const AValue: TFMDDo): Boolean; overload;
    function CheckAfterFMDDo(Const AValue: TFMDDoSet): Boolean; overload;
    function CheckLetFMDDoAfterFinish(Const AValue: TFMDDo): Boolean; overload;
    function CheckLetFMDDoAfterFinish(Const AValue: TFMDDoSet): Boolean; overload;
  end;

  { TOptions }

  TOptions = class(TObject)
  private
    FGeneral: TGeneral;
    FView: TObject;
    FConnections: TObject;
    FSaveTo: TObject;
    FUpdates: TObject;
    FDialogs: TObject;
    FWebsites: TObject;
    FMisc: TObject;
  public
    property General: TGeneral read FGeneral;
    property View: TObject read FView;
    property Connections: TObject read FConnections;
    property SaveTo: TObject read FSaveTo;
    property Updates: TObject read FUpdates;
    property Dialogs: TObject read FDialogs;
    property Websites: TObject read FWebsites;
    property Misc: TObject read FMisc;

    constructor Create;
    destructor Destroy; override;
  end;


const
  FMD_INSTANCE = '_FreeMangaDownloaderInstance_';
  FMD_TARGETOS  = {$i %FPCTARGETOS%};
  FMD_TARGETCPU = {$i %FPCTARGETCPU%};

  EXPARAM_PATH = '%PATH%';
  EXPARAM_CHAPTER = '%CHAPTER%';
  DEFAULT_EXPARAM = '"' + EXPARAM_PATH + EXPARAM_CHAPTER + '"';

  DEFAULT_MANGA_CUSTOMRENAME = '%MANGA%';
  DEFAULT_CHAPTER_CUSTOMRENAME = '%CHAPTER%';
  DEFAULT_FILENAME_CUSTOMRENAME = '%FILENAME%';

  DATA_EXT = '.dat';
  DBDATA_EXT = '.db';
  DBDATA_SERVER_EXT = '.7z';
  UPDATER_EXE = 'updater.exe';
  OLD_UPDATER_EXE = 'old_' + UPDATER_EXE;
  ZIP_EXE = '7za.exe';
  RUN_EXE = '.run';

  {$IFDEF WINDOWS}
  {$IFDEF WIN32}
  MAX_TASKLIMIT = 16;
  MAX_CONNECTIONPERHOSTLIMIT = 64;
  {$ENDIF}
  {$IFDEF WIN64}
  MAX_TASKLIMIT = 64;
  MAX_CONNECTIONPERHOSTLIMIT = 256;
  {$ENDIF}
  {$ELSE}
  MAX_TASKLIMIT = 8;
  MAX_CONNECTIONPERHOSTLIMIT = 32;
  {$ENDIF}

  BACKUP_FILE_PREFIX = 'fmdbackup_';
  BACKUP_FILE_EXT = '7z';

{$i revision.inc}

var
  FMD_VERSION_NUMBER: TProgramVersion;
  FMD_VERSION_STRING,
  FMD_DIRECTORY,
  FMD_EXENAME,
  CURRENT_UPDATER_EXE,
  OLD_CURRENT_UPDATER_EXE,
  CURRENT_ZIP_EXE,
  APPDATA_DIRECTORY,
  DEFAULT_PATH,
  USERDATA_FOLDER,
  DOWNLOADSDB_FILE,
  DOWNLOADEDCHAPTERSDB_FILE,
  FAVORITES_FILE,
  FAVORITESDB_FILE,
  SETTINGS_FILE,
  CONFIG_FILE,
  ACCOUNTS_FILE,
  MODULES_FILE,
  DATA_FOLDER,
  IMAGE_FOLDER,
  CHANGELOG_FILE,
  DEFAULT_LOG_FILE,
  README_FILE,
  LUA_WEBSITEMODULE_FOLDER,
  LUA_PACKAGES_FOLDER,
  LUA_WEBSITEBYPASS_FOLDER,
  LUA_REPO_FOLDER,
  LUA_REPO_FILE,
  LUA_REPO_WORK_FILE,
  BACKUP_FOLDER: String;

  // program params
  AppParams: TStringList;

  // json files
  settingsfile: TJSONIniFile;

  // base url, should be in base.json
  DEFAULT_SELECTED_WEBSITES: String = '';
  DB_URL: String = '';
  UPDATE_URL: String = '';
  UPDATE_PACKAGE_NAME: String = '';

  currentWebsite: Pointer;

  // saveto
  OptionChangeUnicodeCharacter: Boolean = False;
  OptionChangeUnicodeCharacterStr: String = '_';
  OptionGenerateMangaFolder: Boolean = False;
  OptionMangaCustomRename: String;
  OptionGenerateChapterFolder: Boolean = True;
  OptionChapterCustomRename: String;
  OptionFilenameCustomRename: String;

  OptionConvertDigitVolume: Boolean;
  OptionConvertDigitChapter: Boolean;
  OptionConvertDigitVolumeLength: Integer;
  OptionConvertDigitChapterLength: Integer;

  OptionPDFQuality: Cardinal = 95;

  OptionPNGSaveAsJPEG: Boolean = False;
  OptionWebPSaveAs: Integer = 1;
  OptionPNGCompressionLevel: Integer = 1;
  OptionJPEGQuality: Integer = 80;

  // image properties
  OptionImageServerTime: Boolean = True;

  // connections
  OptionConnectionTimeout: Integer = 30;
  OptionMaxFavoriteThreads: Integer = 1;
  OptionMaxUpdateListThreads: Integer = 1;
  OptionMaxBackgroundLoadThreads: Integer = 1;
  OptionMaxParallel: Integer = 1;
  OptionMaxThreads: Integer = 1;
  OptionMaxRetry: Integer = 5;
  OptionRetryFailedTask: Integer = 1;
  OptionAlwaysStartTaskFromFailedChapters: Boolean = True;
  OptionEnableCloudflareBypass: Boolean = True;
  OptionAutomaticallyDisableCloudflareBypass: Boolean = False;

  // view
  OptionEnableLoadCover: Boolean = False;
  OptionShowBalloonHint: Boolean = True;
  OptionShowFavoritesTabOnNewManga: Boolean = False;
  OptionShowDownloadsTabOnNewTasks: Boolean = True;
  
  // favorites (context menu settings)
  OptionDefaultAction: Integer = 0;

  // updates
  OptionAutoCheckLatestVersion: Boolean = True;
  OptionAutoCheckFavStartup: Boolean = True;
  OptionAutoCheckFavInterval: Boolean = True;
  OptionAutoCheckFavIntervalMinutes: Cardinal = 60;
  OptionJDNNewMangaTime: Integer = MaxInt;
  OptionAutoCheckFavDownload: Boolean = False;
  OptionAutoCheckFavRemoveCompletedManga: Boolean = False;
  OptionUpdateListNoMangaInfo: Boolean = False;
  OptionUpdateListRemoveDuplicateLocalData: Boolean = False;

  // modules
  OptionModulesUpdaterShowUpdateWarning: Boolean = True;
  OptionModulesUpdaterAutoRestart: Boolean = False;

  OptionHTTPUseGzip: Boolean = True;

  OptionRemoveMangaNameFromChapter: Boolean = False;

  OptionRestartFMD: Boolean = False;

  // custom color
  // basiclist
  CL_BSNormalText: TColor = clWindowText;
  CL_BSFocusedSelectionText: TColor = clHighlightText;
  CL_BSUnfocesedSelectionText: TColor = clWindowText;
  CL_BSOdd: TColor = clBtnFace;
  CL_BSEven: TColor = clWindow;
  CL_BSSortedColumn: TColor = $F0F0F0;
  CL_BSEnabledWebsiteSettings: TColor = clYellow;

  // mangalist color
  CL_MNNewManga: TColor = $FDC594;
  CL_MNCompletedManga: TColor = $B8FFB8;

  // favoritelist color
  CL_FVBrokenFavorite: TColor = $8080FF;
  CL_FVChecking: TColor = $80EBFE;
  CL_FVNewChapterFound: TColor = $FDC594;
  CL_FVCompletedManga: TColor = $B8FFB8;
  CL_FVEmptyChapters: TColor = $CCDDFF;

  // chapterlist color
  CL_CHDownloaded: TColor = $B8FFB8;

  // modulelist color
  CL_MDNewUpdate: TColor = $FDC594;

  // custom color darkmode
  // basiclist
  CL_BSSortedColumnDark: TColor = $202020;
  CL_BSEnabledWebsiteSettingsDark: TColor = $009696;

  // mangalist color
  CL_MNNewMangaDark: TColor = $C85A00;
  CL_MNCompletedMangaDark: TColor = $008200;

  // favoritelist color
  CL_FVBrokenFavoriteDark: TColor = $0000C8;
  CL_FVCheckingDark: TColor = $0096AA;
  CL_FVNewChapterFoundDark: TColor = $C85A00;
  CL_FVCompletedMangaDark: TColor = $008200;
  CL_FVEmptyChaptersDark: TColor = $005AC8;

  // chapterlist color
  CL_CHDownloadedDark: TColor = $008200;

  // modulelist color
  CL_MDNewUpdateDark: TColor = $C85A00;

// set base directory
procedure SetFMDdirectory(const ADir: String);
procedure SetAppDataDirectory(const ADir: String);

implementation

uses
  frmMain, WebsiteModules, SimpleTranslator;

{ TGeneral }

constructor TGeneral.Create;
begin
  inherited Create;

  FLanguage := 'en';
  FSelectedMangaLists := DEFAULT_SELECTED_WEBSITES; 
  FExtProgramPath := '';
  FExtProgramParam := DEFAULT_EXPARAM;

  FAfterFMDDo := DO_NOTHING;
  FLetFMDDoAfterFinish := DO_NOTHING;

  FLetFMDDoAfterFinishInt := 0;
  FTheme := 0;
  FMangaDaysNew := 1;

  FMinOnStart := False;
  FMinToTray := False;
  FOneFMDInstance := True;  
  FListLiveSearch := True;
  FMangaLoadAddToList := False;
  FHghlghtNewManga := True;
  FSortChptrListAsc := True;
  FChptrListHideDwnlded := False;
  FHghlghtDwnldedChptrs := True;
  FDelCompltdDLOnClose := False;
  FAddNewDLAsStopped := False;
  FSortDLAddNew := False;
  FDBVacuumExit := False;  
  FLongNamePaths := False;
end;

procedure TGeneral.SetLanguage;
var
  languagesItemIndex: Integer;
begin
  languagesItemIndex := MainForm.cbLanguages.ItemIndex;

  if languagesItemIndex < 0 then
  begin
    FLanguage := 'en';
    Exit;
  end;

  FLanguage := AvailableLanguages.Names[languagesItemIndex];
end;

procedure TGeneral.SetSelectedMangaLists;
var
  lists: String;
  mangaItems: TStrings;
  i: Integer;
  m: TModuleContainer;
begin
  mangaItems := MainForm.cbSelectManga.Items;

  if mangaItems.Count = 0 then
  begin
    Exit;
  end;

  lists := '';
  for i := 0 to mangaItems.Count - 1 do
  begin
    m := TModuleContainer(mangaItems.Objects[i]);
    if m <> nil then
    begin
      lists += m.ID + ',';
    end;
  end;

  FSelectedMangaLists := lists.TrimRight([',']);
end;

procedure TGeneral.SetExtProgramPath(Const AValue: String);
begin
  FExtProgramPath := '';

  if FileExists(AValue) then
  begin
    FExtProgramPath := Trim(AValue);
  end;
end; 

procedure TGeneral.SetExtProgramParam(Const AValue: String);
begin
  FExtProgramParam := Trim(AValue);
end; 

procedure TGeneral.SetAfterFMDDo(Const AValue: TFMDDo);
begin
  if not (AValue in [Low(TFMDDo)..High(TFMDDo)]) then
  begin
    Exit;
  end;

  FAfterFMDDo := AValue;
end;

procedure TGeneral.SetLetFMDDoAfterFinish(Const AValue: TFMDDo);
begin
  if not (AValue in [Low(TFMDDo)..High(TFMDDo)]) then
  begin
    Exit;
  end;

  FLetFMDDoAfterFinish := AValue;
  FLetFMDDoAfterFinishInt := Ord(AValue);
end;

procedure TGeneral.SetLetFMDDoAfterFinishInt(Const AValue: Integer);
begin 
  if (AValue <= Ord(Low(TFMDDo))) or (AValue >= Ord(High(TFMDDo))) then
  begin
    Exit;
  end;

  FLetFMDDoAfterFinishInt := AValue;
  FLetFMDDoAfterFinish := TFMDDo(AValue);
end;

function TGeneral.CheckAfterFMDDo(Const AValue: TFMDDo): Boolean;
begin
  Result := FAfterFMDDo = AValue;
end;

function TGeneral.CheckAfterFMDDo(Const AValue: TFMDDoSet): Boolean;
begin
  Result := FAfterFMDDo in AValue;
end;

function TGeneral.CheckLetFMDDoAfterFinish(Const AValue: TFMDDo): Boolean;
begin
  Result := FLetFMDDoAfterFinish = AValue;
end;

function TGeneral.CheckLetFMDDoAfterFinish(Const AValue: TFMDDoSet): Boolean;
begin
  Result := FLetFMDDoAfterFinish in AValue;
end; 

procedure TGeneral.SetTheme(Const AValue: Integer);
begin
  if (AValue < 0) or (AValue > 2) then
  begin
    Exit;
  end;

  FTheme := AValue;
end;

procedure TGeneral.SetMangaDaysNew(Const AValue: Integer);
begin
  if AValue < 1 then
  begin
    Exit;
  end;

  FMangaDaysNew := AValue;
end;
     
{ TOptions }

constructor TOptions.Create;
begin
  inherited Create;

  FGeneral := TGeneral.Create;
end;

destructor TOptions.Destroy;
begin
  FGeneral.Free;

  inherited Destroy;
end;

procedure FreeNil(var Obj);
begin
  if Pointer(Obj) <> nil then
  begin
    TObject(Obj).Free;
  end;

  Pointer(Obj) := nil;
end;

procedure FreeIniFiles;
begin
  FreeNil(settingsfile);
end;

procedure SetIniFiles;
begin
  FreeIniFiles;
  settingsfile := TJSONIniFile.Create(SETTINGS_FILE);
end;

procedure ReadConfigFile;
begin
  if not FileExists(CONFIG_FILE) then
  begin
    Exit;
  end;

  with TJSONIniFile.Create(CONFIG_FILE) do
  begin
    try
      DEFAULT_SELECTED_WEBSITES := ReadString('config', 'default_selected_websites', DEFAULT_SELECTED_WEBSITES);
      DB_URL := ReadString('config', 'db_url', DB_URL);
      UPDATE_URL := ReadString('config', 'update_url', UPDATE_URL);
      UPDATE_PACKAGE_NAME := ReadString('config', 'update_package_name', UPDATE_PACKAGE_NAME);
    finally
      Free;
    end;
  end;
end;

procedure SetFMDdirectory(const ADir: String);
begin
  FMD_DIRECTORY := CleanAndExpandDirectory(ADir);
  FMD_EXENAME := ExtractFileNameOnly(Application.ExeName);

  CONFIG_FILE := FMD_DIRECTORY + 'config.json';

  IMAGE_FOLDER := FMD_DIRECTORY + 'images' + PathDelim;
  CHANGELOG_FILE := FMD_DIRECTORY + 'changelog.txt';
  README_FILE := FMD_DIRECTORY + 'readme.rtf';
  DEFAULT_LOG_FILE := FMD_EXENAME + '.log';
  CURRENT_UPDATER_EXE := FMD_DIRECTORY + UPDATER_EXE;
  OLD_CURRENT_UPDATER_EXE := FMD_DIRECTORY + OLD_UPDATER_EXE;
  CURRENT_ZIP_EXE := FMD_DIRECTORY + ZIP_EXE;

  BACKUP_FOLDER := FMD_DIRECTORY + 'backup' + PathDelim;

  ReadConfigFile;
end;

procedure SetAppDataDirectory(const ADir: String);
begin
  APPDATA_DIRECTORY := CleanAndExpandDirectory(ADir);

  DEFAULT_PATH := 'downloads' + PathDelim;

  DATA_FOLDER := APPDATA_DIRECTORY + 'data' + PathDelim;
  USERDATA_FOLDER := APPDATA_DIRECTORY + 'userdata' + PathDelim;

  SETTINGS_FILE := USERDATA_FOLDER + 'settings.json';
  ACCOUNTS_FILE := USERDATA_FOLDER + 'accounts.db';
  MODULES_FILE := USERDATA_FOLDER + 'modules.json';
  LUA_REPO_FILE := USERDATA_FOLDER + 'lua.json';
  LUA_REPO_WORK_FILE := USERDATA_FOLDER + 'lua_repo.json';
  DOWNLOADSDB_FILE := USERDATA_FOLDER + 'downloads.db';
  DOWNLOADEDCHAPTERSDB_FILE := USERDATA_FOLDER + 'downloadedchapters.db';
  FAVORITESDB_FILE := USERDATA_FOLDER + 'favorites.db';

  LUA_REPO_FOLDER := FMD_DIRECTORY + 'lua' + PathDelim;
  LUA_PACKAGES_FOLDER := LUA_REPO_FOLDER + 'packages' + PathDelim;
  LUA_WEBSITEMODULE_FOLDER := LUA_REPO_FOLDER + 'modules' + PathDelim;
  LUA_WEBSITEBYPASS_FOLDER := LUA_REPO_FOLDER + 'websitebypass' + PathDelim;

  SetIniFiles;
end;

procedure doInitialization;
var
  i: Integer;
begin
  AppParams := TStringList.Create;
  AppParams.Sorted := False;
  for i := 1 to ParamCount do
  begin
    AppParams.Add(ParamStr(i));
  end;

  GetProgramVersion(FMD_VERSION_NUMBER);
  FMD_VERSION_STRING := ProgramversionToStr(FMD_VERSION_NUMBER);
  SetFMDdirectory(ExtractFilePath(Application.ExeName));
  SetAppDataDirectory(FMD_DIRECTORY);
end;

procedure doFinalization;
begin
  FreeIniFiles;
  AppParams.Free;
end;

initialization
  doInitialization;

finalization
  doFinalization;

end.
