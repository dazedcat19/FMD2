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
    const
      // Default
      FLanguageDefault: String = 'en';
      FExtProgramPathDefault: String = '';
      FThemeDefault: Integer = 0;
      FMangaDaysNewDefault: Integer = 1;

    var
      // Save Key
      FSaveLanguage,
      FSaveSelectedMangaLists,
      FSaveExtProgramPath,
      FSaveExtProgramParam: String;

      FSaveLetFMDDoAfterFinishInt,
      FSaveTheme,
      FSaveMangaDaysNew: String;

      FSaveMinOnStart,
      FSaveMinToTray,
      FSaveOneFMDInstance,
      FSaveListLiveSearch,
      FSaveMangaLoadAddToList,
      FSaveHghlghtNewManga,
      FSaveSortChptrListAsc,
      FSaveChptrListHideDwnlded,
      FSaveHghlghtDwnldedChptrs,
      FSaveDelCompltdDLOnClose,
      FSaveAddNewDLAsStopped,
      FSaveSortDLAddNew,
      FSaveDBVacuumExit,
      FSaveLongNamePaths: String;

      // Option
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

    // Get/Set Method
    function GetSelectedMangaListsDefault: String;

    procedure SetExtProgramPath(const AValue: String);
    procedure SetExtProgramParam(const AValue: String);

    procedure SetAfterFMDDo(const AValue: TFMDDo);
    procedure SetLetFMDDoAfterFinish(const AValue: TFMDDo);

    procedure SetLetFMDDoAfterFinishInt(const AValue: Integer);
    procedure SetTheme(const AValue: Integer);
    procedure SetMangaDaysNew(const AValue: Integer);

  public 
    // TGeneral Save Key
    property SaveLanguage: String read FSaveLanguage;
    property SaveSelectedMangaLists: String read FSaveSelectedMangaLists;
    property SaveExtProgramPath: String read FSaveExtProgramPath;
    property SaveExtProgramParam: String read FSaveExtProgramParam;

    property SaveLetFMDDoAfterFinishInt: String read FSaveLetFMDDoAfterFinishInt;
    property SaveTheme: String read FSaveTheme;
    property SaveMangaDaysNew: String read FSaveMangaDaysNew;

    property SaveMinOnStart: String read FSaveMinOnStart;
    property SaveMinToTray: String read FSaveMinToTray;
    property SaveOneFMDInstance: String read FSaveOneFMDInstance;
    property SaveListLiveSearch: String read FSaveListLiveSearch;
    property SaveMangaLoadAddToList: String read FSaveMangaLoadAddToList;
    property SaveHghlghtNewManga: String read FSaveHghlghtNewManga;
    property SaveSortChptrListAsc: String read FSaveSortChptrListAsc;
    property SaveChptrListHideDwnlded: String read FSaveChptrListHideDwnlded;
    property SaveHghlghtDwnldedChptrs: String read FSaveHghlghtDwnldedChptrs;
    property SaveDelCompltdDLOnClose: String read FSaveDelCompltdDLOnClose;
    property SaveAddNewDLAsStopped: String read FSaveAddNewDLAsStopped;
    property SaveSortDLAddNew: String read FSaveSortDLAddNew;
    property SaveDBVacuumExit: String read FSaveDBVacuumExit;
    property SaveLongNamePaths: String read FSaveLongNamePaths;

    // TGeneral Option
    property Language: String read FLanguage;
    property SelectedMangaLists: String read FSelectedMangaLists; 
    property ExtProgramPath: String read FExtProgramPath write SetExtProgramPath;
    property ExtProgramParam: String read FExtProgramParam write SetExtProgramParam;

    property AfterFMDDo: TFMDDo read FAfterFMDDo write SetAfterFMDDo;
    property LetFMDDoAfterFinish: TFMDDo read FLetFMDDoAfterFinish write SetLetFMDDoAfterFinish;

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

    // TGeneral Get/Set Method
    procedure SetLanguage;
    procedure SetSelectedMangaLists;

    procedure SetSaveHghlghtNewManga(const AValue: Boolean);
    procedure SetSaveSortChptrListAsc(const AValue: Boolean);
    procedure SetSaveChptrListHideDwnlded(const AValue: Boolean);
    procedure SetSaveHghlghtDwnldedChptrs(const AValue: Boolean);
    procedure SetSaveAddNewDLAsStopped(const AValue: Boolean);
    
    // TGeneral Method
    function CheckAfterFMDDo(const AValue: TFMDDo): Boolean; overload;
    function CheckAfterFMDDo(const AValue: TFMDDoSet): Boolean; overload;
    function CheckLetFMDDoAfterFinish(const AValue: TFMDDo): Boolean; overload;
    function CheckLetFMDDoAfterFinish(const AValue: TFMDDoSet): Boolean; overload;
  end;

  { TOptions }

  TOptions = class(TObject)
  private
    class var 
      // Save Ini
      FSaveIni: TJSONIniFile;
                
      // Save Section
      FSaveGeneral: String;

    var
      // Save Section
      FLegacySaveThemeMode: String;

      FSaveForm,
      FSaveLanguages,
      FSaveThemeMode,
      FSaveView,
      FSaveDropTarget,
      FSaveFavorites,
      FSaveConnections,
      FSaveSaveTo,
      FSaveImageMagick,
      FSaveUpdate,
      FSaveModulesUpdater,
      FSaveDialogs,
      FSaveBasicListColors,
      FSaveMangaListColors,
      FSaveFavoriteListColors,
      FSaveChapterListColor,
      FSaveModuleListColor,
      FSaveLogger,
      FSaveDownloadFilter,
      FSaveVTDownload,
      FSaveVTFavorites,
      FSaveVTLuaModulesRepos,
      FSaveVTAccountList,
      FSaveModules: String;

      // Option Class
      FGeneral: TGeneral;
      FView: TObject;
      FConnections: TObject;
      FSaveTo: TObject;
      FUpdate: TObject;
      FDialogs: TObject;
      FWebsites: TObject;
      FMisc: TObject;
  public
    // TOptions Save Section
    property LegacySaveThemeMode: String read FLegacySaveThemeMode;
               
    property SaveForm: String read FSaveForm;
    class property SaveGeneral: String read FSaveGeneral;
    property SaveLanguages: String read FSaveLanguages;
    property SaveThemeMode: String read FSaveThemeMode;
    property SaveView: String read FSaveView;
    property SaveDropTarget: String read FSaveDropTarget;
    property SaveFavorites: String read FSaveFavorites;
    property SaveConnections: String read FSaveConnections;
    property SaveSaveTo: String read FSaveSaveTo;
    property SaveImageMagick: String read FSaveImageMagick;
    property SaveUpdate: String read FSaveUpdate;
    property SaveModulesUpdater: String read FSaveModulesUpdater;
    property SaveDialogs: String read FSaveDialogs;
    property SaveBasicListColors: String read FSaveBasicListColors;
    property SaveMangaListColors: String read FSaveMangaListColors;
    property SaveFavoriteListColors: String read FSaveFavoriteListColors;
    property SaveChapterListColor: String read FSaveChapterListColor;
    property SaveModuleListColor: String read FSaveModuleListColor;
    property SaveLogger: String read FSaveLogger;
    property SaveDownloadFilter: String read FSaveDownloadFilter;
    property SaveVTDownload: String read FSaveVTDownload;
    property SaveVTFavorites: String read FSaveVTFavorites;
    property SaveVTLuaModulesRepos: String read FSaveVTLuaModulesRepos;
    property SaveVTAccountList: String read FSaveVTAccountList;
    property SaveModules: String read FSaveModules;

    // TOptions Option Class
    property General: TGeneral read FGeneral;
    property View: TObject read FView;
    property Connections: TObject read FConnections;
    property SaveTo: TObject read FSaveTo;
    property Update: TObject read FUpdate;
    property Dialogs: TObject read FDialogs;
    property Websites: TObject read FWebsites;
    property Misc: TObject read FMisc;

    constructor Create;
    destructor Destroy; override;

    // TOptions Save Ini Read/Write Method
    class function LegacySaveReadStr(const ALegacySection, ASection, AIdent, ADefaultValue: String): String;
    class function LegacySaveReadInt(const ALegacySection, ASection, AIdent: String; ADefaultValue: Integer): Integer;
    class function LegacySaveReadBool(const ALegacySection, ASection, AIdent: String; ADefaultValue: Boolean): Boolean;
    class function LegacySaveReadDate(const ALegacySection, ASection, AIdent: String; ADefaultValue: TDateTime): TDateTime;

    class function SaveReadStr(const ASection, AIdent, ADefaultValue: String): String;
    class function SaveReadInt(const ASection, AIdent: String; ADefaultValue: Integer): Integer;
    class function SaveReadBool(const ASection, AIdent: String; ADefaultValue: Boolean): Boolean;
    class function SaveReadDate(const ASection, AIdent: String; ADefaultValue: TDateTime): TDateTime;

    class procedure SaveWriteStr(const ASection, AIdent, AValue: String);
    class procedure SaveWriteInt(const ASection, AIdent: String; AValue: Integer);
    class procedure SaveWriteBool(const ASection, AIdent: String; AValue: Boolean);
    class procedure SaveWriteDate(const ASection, AIdent: String; AValue: TDateTime);

    class procedure SaveWriteToDisk;
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
  frmMain, WebsiteModules, SimpleTranslator, MultiLog;

{ Global }

procedure FreeNil(var Obj);
begin
  if Pointer(Obj) <> nil then
  begin
    TObject(Obj).Free;
  end;

  Pointer(Obj) := nil;
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
  AppParams.Free;
end;

{ TGeneral }

constructor TGeneral.Create;
begin
  inherited Create;

  // Save Key
  FSaveLanguage := 'Selected';
  FSaveSelectedMangaLists := 'MangaListSelect';
  FSaveExtProgramPath := 'ExternalProgramPath';
  FSaveExtProgramParam := 'ExternalProgramParams';

  FSaveLetFMDDoAfterFinishInt := 'LetFMDDo';
  FSaveTheme := 'Mode';
  FSaveMangaDaysNew := 'NewMangaTime';

  FSaveMinOnStart := 'MinimizeOnStart';
  FSaveMinToTray := 'MinimizeToTray';
  FSaveOneFMDInstance := 'OneInstanceOnly';
  FSaveListLiveSearch := 'LiveSearch';
  FSaveMangaLoadAddToList := 'MangaLoadAddToList';
  FSaveHghlghtNewManga := 'HighlightNewManga';
  FSaveSortChptrListAsc := 'SortChapterListAscending';
  FSaveChptrListHideDwnlded := 'ChapterListHideDownloaded';
  FSaveHghlghtDwnldedChptrs := 'HighlightDownloadedChapters';
  FSaveDelCompltdDLOnClose := 'DeleteCompletedTasksOnClose';
  FSaveAddNewDLAsStopped := 'AddAsStopped';
  FSaveSortDLAddNew := 'SortDownloadsOnNewTasks';
  FSaveDBVacuumExit := 'VacuumDatabasesOnExit';
  FSaveLongNamePaths := 'EnableLongNamePaths';

  // Option
  FLanguage := FLanguageDefault;
  FSelectedMangaLists := GetSelectedMangaListsDefault;
  FExtProgramPath := FExtProgramPathDefault;
  FExtProgramParam := DEFAULT_EXPARAM;

  FAfterFMDDo := DO_NOTHING;
  FLetFMDDoAfterFinish := DO_NOTHING;

  FLetFMDDoAfterFinishInt := 0;
  FTheme := FThemeDefault;
  FMangaDaysNew := FMangaDaysNewDefault;

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

function TGeneral.GetSelectedMangaListsDefault: String;
begin
  Result := DEFAULT_SELECTED_WEBSITES;
end;

procedure TGeneral.SetLanguage;
var
  languagesItemIndex: Integer;
begin
  languagesItemIndex := MainForm.cbLanguages.ItemIndex;

  if languagesItemIndex < 0 then
  begin
    Logger.SendWarning(Self.ClassName + '.SetLanguage: Invalid language item index');

    FLanguage := FLanguageDefault;
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
    Logger.SendWarning(Self.ClassName + '.SetSelectedMangaLists: No manga list elements remaining');

    FSelectedMangaLists := GetSelectedMangaListsDefault;
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

procedure TGeneral.SetExtProgramPath(const AValue: String);
var
  extProgram: String;
begin
  extProgram := Trim(AValue);

  if not FileExists(extProgram) then
  begin
    if extProgram <> '' then
    begin
      Logger.SendWarning(Self.ClassName + '.SetExtProgramPath: External program path/file invalid or doesn''t exist');
    end;

    FExtProgramPath := FExtProgramPathDefault;
    Exit;
  end;

  FExtProgramPath := extProgram;
end; 

procedure TGeneral.SetExtProgramParam(const AValue: String);
begin
  FExtProgramParam := Trim(AValue);
end; 

procedure TGeneral.SetAfterFMDDo(const AValue: TFMDDo);
begin
  if not (AValue in [Low(TFMDDo)..High(TFMDDo)]) then
  begin                                              
    Logger.SendWarning(Self.ClassName + '.SetAfterFMDDo: Invalid TFMDDo element');

    Exit;
  end;

  FAfterFMDDo := AValue;
end;

procedure TGeneral.SetLetFMDDoAfterFinish(const AValue: TFMDDo);
begin
  if not (AValue in [Low(TFMDDo)..High(TFMDDo)]) then
  begin
    Logger.SendWarning(Self.ClassName + '.SetLetFMDDoAfterFinish: Invalid TFMDDo element');

    Exit;
  end;

  FLetFMDDoAfterFinish := AValue;
  FLetFMDDoAfterFinishInt := Ord(AValue);
end;

procedure TGeneral.SetLetFMDDoAfterFinishInt(const AValue: Integer);
begin
  if (AValue < Ord(Low(TFMDDo))) or (AValue > Ord(High(TFMDDo))) then
  begin                                                                
    Logger.SendWarning(Self.ClassName + '.SetLetFMDDoAfterFinishInt: Invalid index for TFMDDo');

    Exit;
  end;

  FLetFMDDoAfterFinishInt := AValue;
  FLetFMDDoAfterFinish := TFMDDo(AValue);
end;

function TGeneral.CheckAfterFMDDo(const AValue: TFMDDo): Boolean;
begin
  Result := FAfterFMDDo = AValue;
end;

function TGeneral.CheckAfterFMDDo(const AValue: TFMDDoSet): Boolean;
begin
  Result := FAfterFMDDo in AValue;
end;

function TGeneral.CheckLetFMDDoAfterFinish(const AValue: TFMDDo): Boolean;
begin
  Result := FLetFMDDoAfterFinish = AValue;
end;

function TGeneral.CheckLetFMDDoAfterFinish(const AValue: TFMDDoSet): Boolean;
begin
  Result := FLetFMDDoAfterFinish in AValue;
end; 

procedure TGeneral.SetTheme(const AValue: Integer);
begin
  if (AValue < 0) or (AValue > 2) then
  begin
    Logger.SendWarning(Self.ClassName + '.SetTheme: Invalid theme item index');

    FTheme := FThemeDefault;
    Exit;
  end;

  FTheme := AValue;
end;

procedure TGeneral.SetMangaDaysNew(const AValue: Integer);
begin
  if AValue < 1 then
  begin    
    Logger.SendWarning(Self.ClassName + '.SetMangaDaysNew: Invalid days amount value');

    FMangaDaysNew := FMangaDaysNewDefault;
    Exit;
  end;

  FMangaDaysNew := AValue;
end;

procedure TGeneral.SetSaveHghlghtNewManga(const AValue: Boolean);
begin
   FHghlghtNewManga := AValue;

   TOptions.SaveWriteBool(TOptions.SaveGeneral, FSaveHghlghtNewManga, FHghlghtNewManga);
   TOptions.SaveWriteToDisk;
end;

procedure TGeneral.SetSaveSortChptrListAsc(const AValue: Boolean);
begin
   FSortChptrListAsc := AValue;

   TOptions.SaveWriteBool(TOptions.SaveGeneral, FSaveSortChptrListAsc, FSortChptrListAsc);
   TOptions.SaveWriteToDisk;
end;

procedure TGeneral.SetSaveChptrListHideDwnlded(const AValue: Boolean);
begin
   FChptrListHideDwnlded := AValue;

   TOptions.SaveWriteBool(TOptions.SaveGeneral, FSaveChptrListHideDwnlded, FChptrListHideDwnlded);
   TOptions.SaveWriteToDisk;
end;

procedure TGeneral.SetSaveHghlghtDwnldedChptrs(const AValue: Boolean);
begin
   FHghlghtDwnldedChptrs := AValue;

   TOptions.SaveWriteBool(TOptions.SaveGeneral, FSaveHghlghtDwnldedChptrs, FHghlghtDwnldedChptrs);
   TOptions.SaveWriteToDisk;
end;

procedure TGeneral.SetSaveAddNewDLAsStopped(const AValue: Boolean);
begin
   FAddNewDLAsStopped := AValue;

   TOptions.SaveWriteBool(TOptions.SaveGeneral, FSaveAddNewDLAsStopped, FAddNewDLAsStopped);
   TOptions.SaveWriteToDisk;
end;

{ TOptions }

constructor TOptions.Create;
begin
  inherited Create;
   
  FreeNil(FSaveIni);
  FSaveIni := TJSONIniFile.Create(SETTINGS_FILE);

  // Save Section
  FLegacySaveThemeMode:= 'DarkMode';

  FSaveForm := 'Form';
  FSaveGeneral := 'General';
  FSaveLanguages := 'Languages';
  FSaveThemeMode := 'Theme';
  FSaveView := 'View';
  FSaveDropTarget := 'DropTarget';
  FSaveFavorites := 'Favorites';
  FSaveConnections := 'Connections';
  FSaveSaveTo := 'SaveTo';
  FSaveImageMagick := 'ImageMagick';
  FSaveUpdate := 'Update';
  FSaveModulesUpdater := 'ModulesUpdater';
  FSaveDialogs := 'Dialogs';
  FSaveBasicListColors := 'BasicListColors';
  FSaveMangaListColors := 'MangaListColors';
  FSaveFavoriteListColors := 'FavoriteListColors';
  FSaveChapterListColor := 'ChapterListColor';
  FSaveModuleListColor := 'ModuleListColor';
  FSaveLogger := 'Logger';
  FSaveDownloadFilter := 'DownloadFilter';
  FSaveVTDownload := 'VTDownload';
  FSaveVTFavorites := 'VTFavorites';
  FSaveVTLuaModulesRepos := 'VTLuaModulesRepos';
  FSaveVTAccountList := 'VTAccountList';
  FSaveModules := 'Modules';

  FGeneral := TGeneral.Create;
end;

destructor TOptions.Destroy;
begin      
  FreeNil(FSaveIni);

  FGeneral.Free;

  inherited Destroy;
end;

class function TOptions.LegacySaveReadStr(const ALegacySection, ASection, AIdent, ADefaultValue: String): String;
begin
  Result := FSaveIni.ReadString(ASection, AIdent, FSaveIni.ReadString(ALegacySection, AIdent, ADefaultValue));
end;

class function TOptions.LegacySaveReadInt(const ALegacySection, ASection, AIdent: String; ADefaultValue: Integer): Integer;
begin
  Result := FSaveIni.ReadInteger(ASection, AIdent, FSaveIni.ReadInteger(ALegacySection, AIdent, ADefaultValue));
end;

class function TOptions.LegacySaveReadBool(const ALegacySection, ASection, AIdent: String; ADefaultValue: Boolean): Boolean;
begin
  Result := FSaveIni.ReadBool(ASection, AIdent, FSaveIni.ReadBool(ALegacySection, AIdent, ADefaultValue));
end;

class function TOptions.LegacySaveReadDate(const ALegacySection, ASection, AIdent: String; ADefaultValue: TDateTime): TDateTime;
begin
  Result := FSaveIni.ReadDate(ASection, AIdent, FSaveIni.ReadDate(ALegacySection, AIdent, ADefaultValue));
end;

class function TOptions.SaveReadStr(const ASection, AIdent, ADefaultValue: String): String;
begin
  Result := FSaveIni.ReadString(ASection, AIdent, ADefaultValue);
end;

class function TOptions.SaveReadInt(const ASection, AIdent: String; ADefaultValue: Integer): Integer;
begin
  Result := FSaveIni.ReadInteger(ASection, AIdent, ADefaultValue);
end;

class function TOptions.SaveReadBool(const ASection, AIdent: String; ADefaultValue: Boolean): Boolean;
begin
  Result := FSaveIni.ReadBool(ASection, AIdent, ADefaultValue);
end;

class function TOptions.SaveReadDate(const ASection, AIdent: String; ADefaultValue: TDateTime): TDateTime;
begin
  Result := FSaveIni.ReadDate(ASection, AIdent, ADefaultValue);
end;

class procedure TOptions.SaveWriteStr(const ASection, AIdent, AValue: String);
begin
  FSaveIni.WriteString(ASection, AIdent, AValue);
end;

class procedure TOptions.SaveWriteInt(const ASection, AIdent: String; AValue: Integer);
begin
  FSaveIni.WriteInteger(ASection, AIdent, AValue);
end;

class procedure TOptions.SaveWriteBool(const ASection, AIdent: String; AValue: Boolean);
begin
  FSaveIni.WriteBool(ASection, AIdent, AValue);
end;

class procedure TOptions.SaveWriteDate(const ASection, AIdent: String; AValue: TDateTime);
begin
  FSaveIni.WriteDate(ASection, AIdent, AValue);
end;

class procedure TOptions.SaveWriteToDisk;
begin
  FSaveIni.UpdateFile;
end;

initialization
  doInitialization;

finalization
  doFinalization;

end.
