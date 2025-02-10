{
        File: frmMain.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  FMDOptions,
  {$ifdef windows}
  ActiveX, windows,
  {$else}
  FakeActiveX,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  ExtCtrls, ComCtrls, Buttons, Spin, Menus, VirtualTrees, RichMemo, simpleipc, process,
  lclproc, types, LCLIntf, EditBtn, PairSplitter, MultiLog,
  FileChannel, FileUtil, LazStringUtils, TAGraph, TASources, TASeries, TATools,
  AnimatedGif, uBaseUnit, uDownloadsManager, uFavoritesManager,
  uSilentThread, uMisc, uGetMangaInfosThread, frmDropTarget, frmAccountManager,
  frmWebsiteOptionCustom, frmCustomColor, frmLogger, frmTransferFavorites,
  frmLuaModulesUpdater, CheckUpdate, DBDataProcess, uDarkStyleParams,
  SimpleTranslator, httpsendthread, DateUtils, SimpleException;

type

  { TMainForm }

  TMainForm = class(TForm)
    appPropertiesMain: TApplicationProperties;
    btnDownloadFilterCustomDateApply: TBitBtn;
    btOpenLog: TBitBtn;
    btClearLogFile: TBitBtn;
    btAddToFavorites: TBitBtn;
    btCancelFavoritesCheck: TSpeedButton;
    btAbortCheckLatestVersion: TSpeedButton;
    btChecks: TSpeedButton;
    btDonate: TImage;
    btFavoritesImport: TBitBtn;
    btFilter: TBitBtn;
    btFilterReset: TBitBtn;
    btOptionApply: TBitBtn;
    btReadOnline: TBitBtn;
    btRemoveFilter: TSpeedButton;
    btMangaListSearchClear: TSpeedButton;
    btUpdateList: TSpeedButton;
    cbOptionAutoCheckFavStartup: TCheckBox;
    cbOptionAutoCheckFavInterval: TCheckBox;
    cbOptionAutoCheckFavDownload: TCheckBox;
    cbOptionAutoCheckFavRemoveCompletedManga: TCheckBox;
    cbOptionAutoOpenFavStartup: TCheckBox;
    cbOptionDeleteCompletedTasksOnClose: TCheckBox;
    cbOptionSortDownloadsOnNewTasks: TCheckBox;
    cbOptionShowFavoritesTabOnNewManga: TCheckBox;
    cbOptionShowDownloadsTabOnNewTasks: TCheckBox;
    cbOptionEnableLoadCover: TCheckBox;
    cbOptionMinimizeOnStart: TCheckBox;
    cbOptionShowBalloonHint: TCheckBox;
    cbOptionGenerateChapterFolder: TCheckBox;
    cbOptionRemoveMangaNameFromChapter: TCheckBox;
    cbOptionShowDownloadMangalistDialog: TCheckBox;
    cbOptionShowDownloadToolbar: TCheckBox;
    cbOptionShowDownloadToolbarLeft: TCheckBox;
    cbOptionShowDownloadToolbarDeleteAll: TCheckBox;
    cbOptionVacuumDatabasesOnExit: TCheckBox;
    cbOptionEnableLongNamePaths: TCheckBox;
    cbOptionUpdateListNoMangaInfo: TCheckBox;
    cbOptionDigitVolume: TCheckBox;
    cbOptionDigitChapter: TCheckBox;
    cbOptionLiveSearch: TCheckBox;
    cbOptionUpdateListRemoveDuplicateLocalData : TCheckBox;
    cbUseRegExpr: TCheckBox;
    cbOptionProxyType: TComboBox;
    cbOptionOneInstanceOnly: TCheckBox;
    ckPNGSaveAsJPEG: TCheckBox;
    ckOptionsAlwaysStartTaskFromFailedChapters: TCheckBox;
    ckEnableLogging: TCheckBox;
    cbWebPSaveAs: TComboBox;
    cbPNGCompressionLevel: TComboBox;
    deDownloadFilterCustomDateFrom: TDateEdit;
    deDownloadFilterCustomDateTo: TDateEdit;
    edDownloadsSearch: TEditButton;
    edFavoritesSearch: TEditButton;
    edFilterMangaInfoChapters: TEditButton;
    edOptionDefaultUserAgent: TEdit;
    edLogFileName: TEditButton;
    edOptionChangeUnicodeCharacterStr: TEdit;
    edOptionDefaultPath: TEditButton;
    edOptionExternalPath: TEditButton;
    edOptionFilenameCustomRename: TEdit;
    edOptionMangaCustomRename: TEdit;
    edSaveTo: TEditButton;
    edURL: TEditButton;
    edWebsitesSearch: TEditButton;
    gbImageConversion: TGroupBox;
    gbOptionsConnectionsDownloads: TGroupBox;
    gbOptionsConnectiosMiscellaneous: TGroupBox;
    gbOptionsConnectionsGeneral: TGroupBox;
    IconDLLeft: TImageList;
    lbDownloadFilterCustomDateFrom: TLabel;
    lbDownloadFilterCustomDateTo: TLabel;
    lbOptionMaxFavoriteThreads: TLabel;
    lbOptionDefaultUserAgent: TLabel;
    lbOptionMaxUpdateListThreads: TLabel;
    lbOptionMaxBackgroundLoudThreads: TLabel;
    lbPNGCompressionLevel: TLabel;
    lbJPEGQuality: TLabel;
    lbWebPSaveAs: TLabel;
    lbLogFileName: TLabel;
    lbOptionRetryFailedTask: TLabel;
    lbOptionFilenameCustomRenameHint: TLabel;
    lbOptionFilenameCustomRename: TLabel;
    lbOptionMangaCustomRenameHint: TLabel;
    lbOptionMangaCustomRename: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    miFavoritesDefaultAction: TMenuItem;
    miFavoritesDefaultActionOpenFolder: TMenuItem;
    miFavoritesDefaultActionShowInfo: TMenuItem;
    miFavoritesDefaultActionCheckNewChapters: TMenuItem;
    miFavoritesRename: TMenuItem;
    miFavoritesTransferWebsite: TMenuItem;
    miFavoritesEnable: TMenuItem;
    miFavoritesDisable: TMenuItem;
    miChapterListDescending: TMenuItem;
    miChapterListAscending: TMenuItem;
    miMangaListDelete: TMenuItem;
    miDownloadDeleteTaskDataFavorite: TMenuItem;
    miTrayExit: TMenuItem;
    miTrayRestore: TMenuItem;
    miTrayShowDropBox: TMenuItem;
    MenuItem8: TMenuItem;
    miTrayFinishNothing: TMenuItem;
    miTrayFinishExit: TMenuItem;
    miTrayFinishShutdown: TMenuItem;
    MenuItem2: TMenuItem;
    miTrayFinishHibernate: TMenuItem;
    miTrayAfterDownloadFinish: TMenuItem;
    miTrayStopAll: TMenuItem;
    miTrayResumeAll: TMenuItem;
    miDownloadEnable: TMenuItem;
    miDownloadDisable: TMenuItem;
    miChapterListFilter: TMenuItem;
    mnFilterGenreAllIndeterminate: TMenuItem;
    mnFilterGenreAllCheck: TMenuItem;
    mnFilterGenreAllUncheck: TMenuItem;
    miChapterListHideDownloaded: TMenuItem;
    miAbortSilentThread: TMenuItem;
    mmChangelog: TMemo;
    pnDownloadFilterCustomDate: TPanel;
    pnAboutVersion: TPanel;
    pnThumb: TPanel;
    pnInfos: TPanel;
    pnDownloadList: TPanel;
    pnAboutComp: TPanel;
    pcInfo: TPageControl;
    psInfo: TPairSplitter;
    pssInfoList: TPairSplitterSide;
    pssInfo: TPairSplitterSide;
    psDownloads: TPairSplitter;
    pssDownloadsFilter: TPairSplitterSide;
    pssDownloads: TPairSplitterSide;
    pcMisc: TPageControl;
    pcWebsiteOptions: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel8: TPanel;
    pcAbout: TPageControl;
    pmSbMain: TPopupMenu;
    pmFilterGenreAll: TPopupMenu;
    pmTray: TPopupMenu;
    rbFavoritesShowDisabled: TRadioButton;
    rbFavoritesShowEnabled: TRadioButton;
    rbFavoritesShowAll: TRadioButton;
    sbSaveTo: TScrollBox;
    sbWebsiteOptions: TScrollBox;
    btDownloadSplit: TSpeedButton;
    sbGeneralSettings: TScrollBox;
    seOptionMaxFavoriteThreads: TSpinEdit;
    seOptionMaxUpdateListThreads: TSpinEdit;
    seOptionMaxBackgroundLoadThreads: TSpinEdit;
    seOptionRetryFailedTask: TSpinEdit;
    seJPEGQuality: TSpinEdit;
    spThumb: TSplitter;
    tbWebsitesSelectAll: TToolButton;
    tbWebsitesUnselectAll: TToolButton;
    tsAccounts: TTabSheet;
    tsWebsiteModules: TTabSheet;
    ToolBarDownloadLeft: TToolBar;
    tbmiDownloadMoveTop: TToolButton;
    tbmiDownloadMoveUp: TToolButton;
    tbmiDownloadMoveDown: TToolButton;
    tbmiDownloadMoveBottom: TToolButton;
    tsInfoManga: TTabSheet;
    tsinfoFilterAdv: TTabSheet;
    tsCustomColor: TTabSheet;
    tsLog: TTabSheet;
    tmStartup: TTimer;
    tmAnimateMangaInfo: TTimer;
    tmCheckFavorites: TTimer;
    tmRefreshDownloadsInfo: TTimer;
    tsWebsiteAdvanced: TTabSheet;
    tsWebsiteSelection: TTabSheet;
    tsWebsiteOptions: TTabSheet;
    tsAboutText: TTabSheet;
    tsChangelogText: TTabSheet;
    TransferRateToolset: TChartToolset;
    miFavoritesStopCheckNewChapter: TMenuItem;
    miFavoritesCheckNewChapter: TMenuItem;
    pnDownloadToolbarLeft: TPanel;
    pnDownloadToolbar: TPanel;
    TransferRateGraphArea: TAreaSeries;
    TransferRateGraph: TChart;
    ckDropTarget: TCheckBox;
    edOptionExternalParams: TEdit;
    gbDropTarget: TGroupBox;
    gbOptionExternal: TGroupBox;
    IconDL: TImageList;
    IconMed: TImageList;
    IconSmall: TImageList;
    tmExitCommand: TTimer;
    lbDefaultDownloadPath: TLabel;
    lbDropTargetOpacity: TLabel;
    lbOptionExternalParams: TLabel;
    lbOptionConnectionTimeout: TLabel;
    lbSaveTo: TLabel;
    lbOptionProxyType: TLabel;
    lbOptionRenameDigits: TLabel;
    lbFilterHint: TLabel;
    lbOptionExternal: TLabel;
    lbOptionChapterCustomRenameHint: TLabel;
    lbOptionPDFQualityHint: TLabel;
    lbOptionExternalParamsHint: TLabel;
    TransferRateGraphList: TListChartSource;
    medURLCut: TMenuItem;
    medURLCopy: TMenuItem;
    medURLPaste: TMenuItem;
    medURLPasteandgo: TMenuItem;
    medtURLDelete: TMenuItem;
    MenuItem15: TMenuItem;
    medURLSelectAll: TMenuItem;
    miFavoritesDefaultActionRename: TMenuItem;
    medURLUndo: TMenuItem;
    miFavoritesDownloadAll: TMenuItem;
    miDownloadViewMangaInfo: TMenuItem;
    MenuItem9: TMenuItem;
    miDownloadDeleteTask: TMenuItem;
    miDownloadDeleteTaskData: TMenuItem;
    miDownloadOpenWith: TMenuItem;
    miFavoritesOpenWith: TMenuItem;
    pnlWebsitesToolRight: TPanel;
    pnlWebsitesTool: TPanel;
    pnCustomGenre: TPanel;
    pnThumbContainer: TPanel;
    pnMainTop: TPanel;
    btVisitMyBlog: TBitBtn;
    btCheckLatestVersion: TBitBtn;
    btFavoritesCheckNewChapter: TBitBtn;
    btDownload: TBitBtn;
    btRemoveFilterLarge: TBitBtn;
    cbOptionAutoCheckLatestVersion: TCheckBox;
    cbOptionShowDeleteTaskDialog: TCheckBox;
    cbOptionUseProxy: TCheckBox;
    cbSelectManga: TComboBox;
    ckFilterAction: TCheckBox;
    ckFilterHarem: TCheckBox;
    ckFilterHentai: TCheckBox;
    ckFilterHistorical: TCheckBox;
    ckFilterHorror: TCheckBox;
    ckFilterJosei: TCheckBox;
    ckFilterLolicon: TCheckBox;
    ckFilterMartialArts: TCheckBox;
    ckFilterMature: TCheckBox;
    ckFilterMecha: TCheckBox;
    ckFilterMusical: TCheckBox;
    ckFilterAdult: TCheckBox;
    ckFilterMystery: TCheckBox;
    ckFilterPsychological: TCheckBox;
    ckFilterRomance: TCheckBox;
    ckFilterSchoolLife: TCheckBox;
    ckFilterSciFi: TCheckBox;
    ckFilterSeinen: TCheckBox;
    ckFilterShotacon: TCheckBox;
    ckFilterShoujo: TCheckBox;
    ckFilterShoujoAi: TCheckBox;
    ckFilterShounen: TCheckBox;
    ckFilterAdventure: TCheckBox;
    ckFilterShounenAi: TCheckBox;
    ckFilterSliceofLife: TCheckBox;
    ckFilterSmut: TCheckBox;
    ckFilterSports: TCheckBox;
    ckFilterSupernatural: TCheckBox;
    ckFilterTragedy: TCheckBox;
    ckFilterYaoi: TCheckBox;
    ckFilterYuri: TCheckBox;
    ckFilterWeebtons: TCheckBox;
    ckFilterComedy: TCheckBox;
    cbOnlyNew: TCheckBox;
    cbAddAsStopped: TCheckBox;
    cbOptionShowQuitDialog: TCheckBox;
    cbOptionChangeUnicodeCharacter: TCheckBox;
    cbOptionGenerateMangaFolder: TCheckBox;
    cbOptionMinimizeToTray: TCheckBox;
    cbSearchFromAllSites: TCheckBox;
    ckFilterDoujinshi: TCheckBox;
    ckFilterDrama: TCheckBox;
    ckFilterEchi: TCheckBox;
    ckFilterFantasy: TCheckBox;
    ckFilterGenderBender: TCheckBox;
    cbFilterStatus: TComboBox;
    cbLanguages: TComboBox;
    cbOptionLetFMDDo: TComboBox;
    edFilterSummary: TEdit;
    edFilterTitle: TEdit;
    edFilterAuthors: TEdit;
    edFilterArtists: TEdit;
    edCustomGenres: TEdit;
    edOptionChapterCustomRename: TEdit;
    edOptionHost: TEdit;
    edOptionPass: TEdit;
    edOptionPort: TEdit;
    edOptionUser: TEdit;
    edMangaListSearch: TEdit;
    gbDialogs: TGroupBox;
    gbOptionProxy: TGroupBox;
    gbOptionRenaming: TGroupBox;
    gbOptionFavorites: TGroupBox;
    IconList: TImageList;
    imCover: TImage;
    lbOptionChapterCustomRename: TLabel;
    lbOptionPDFQuality: TLabel;
    lbOptionAutoCheckFavIntervalMinutes: TLabel;
    lbOptionLetFMDDo: TLabel;
    lbOptionNewMangaTime: TLabel;
    lbOptionLanguage: TLabel;
    lbFilterCustomGenres: TLabel;
    lbFilterSummary: TLabel;
    lbFilterStatus: TLabel;
    lbFilterTitle: TLabel;
    lbFilterAuthors: TLabel;
    lbFilterArtists: TLabel;
    lbMode: TLabel;
    lbOptionHost: TLabel;
    lbOptionMaxParallel: TLabel;
    lbOptionMaxRetry: TLabel;
    lbOptionMaxThread: TLabel;
    lbOptionPass: TLabel;
    lbOptionPort: TLabel;
    lbOptionUser: TLabel;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miFavoritesViewInfos: TMenuItem;
    miChapterListHighlight: TMenuItem;
    mnDownload1Click: TMenuItem;
    mnUpdate1Click: TMenuItem;
    miMangaListDownloadAll: TMenuItem;
    miMangaListViewInfos: TMenuItem;
    mnUpdateList: TMenuItem;
    mnUpdateDownFromServer: TMenuItem;
    miDownloadMergeCompleted: TMenuItem;
    miFavoritesOpenFolder: TMenuItem;
    miHighlightNewManga: TMenuItem;
    miI2: TMenuItem;
    miDownloadOpenFolder: TMenuItem;
    miFavoritesDelete: TMenuItem;
    miMangaListAddToFavorites: TMenuItem;
    miFavoritesChangeSaveTo: TMenuItem;
    miDownloadDeleteCompleted: TMenuItem;
    miDownloadStop: TMenuItem;
    miChapterListCheckAll: TMenuItem;
    miChapterListUncheckSelected: TMenuItem;
    miChapterListCheckSelected: TMenuItem;
    miI1: TMenuItem;
    miDownloadResume: TMenuItem;
    miDownloadRedownload: TMenuItem;
    miDownloadDelete: TMenuItem;
    miChapterListUncheckAll: TMenuItem;
    pbWait: TPaintBox;
    pmChapterList: TPopupMenu;
    pcOptions: TPageControl;
    pnChapterList: TPanel;
    pnFilter: TPanel;
    pnGenres: TPanel;
    pcMain: TPageControl;
    pnInfomation: TPanel;
    pmDownload: TPopupMenu;
    pmFavorites: TPopupMenu;
    pmMangaList: TPopupMenu;
    pmUpdate: TPopupMenu;
    pmEditURL: TPopupMenu;
    rbOne: TRadioButton;
    rbAll: TRadioButton;
    rgDropTargetMode: TRadioGroup;
    rgOptionCompress: TRadioGroup;
    rmAbout: TRichMemo;
    rmInformation: TRichMemo;
    sbFilter: TScrollBox;
    sbInformation: TScrollBox;
    sbDownloadConnections: TScrollBox;
    dlgSaveTo: TSelectDirectoryDialog;
    seOptionMaxParallel: TSpinEdit;
    seOptionMaxRetry: TSpinEdit;
    seOptionConnectionTimeout: TSpinEdit;
    seOptionMaxThread: TSpinEdit;
    seOptionNewMangaTime: TSpinEdit;
    seOptionAutoCheckFavIntervalMinutes: TSpinEdit;
    seOptionPDFQuality: TSpinEdit;
    seOptionDigitVolume: TSpinEdit;
    seOptionDigitChapter: TSpinEdit;
    spInfos: TSplitter;
    spMainSplitter: TSplitter;
    sbMain: TStatusBar;
    tbDropTargetOpacity: TTrackBar;
    tbWebsitesCollapseAll: TToolButton;
    tbWebsitesExpandAll: TToolButton;
    ToolBarWebsites: TToolBar;
    tsView: TTabSheet;
    ToolBarDownload: TToolBar;
    tbDownloadResumeAll: TToolButton;
    tbDownloadStopAll: TToolButton;
    tbSeparator1: TToolButton;
    tbDownloadDeleteCompleted: TToolButton;
    tvDownloadFilter: TTreeView;
    tsMisc: TTabSheet;
    tsUpdate: TTabSheet;
    tsAbout: TTabSheet;
    tsWebsites: TTabSheet;
    tsDialogs: TTabSheet;
    TrayIcon: TTrayIcon;
    tsGeneral: TTabSheet;
    tsFavorites: TTabSheet;
    tsSaveTo: TTabSheet;
    tsConnections: TTabSheet;
    tsOption: TTabSheet;
    tsInformation: TTabSheet;
    tsDownload: TTabSheet;
    clbChapterList: TVirtualStringTree;
    vtOptionMangaSiteSelection: TVirtualStringTree;
    vtFavorites: TVirtualStringTree;
    vtDownload: TVirtualStringTree;
    vtMangaList: TVirtualStringTree;
    mangaCover: TPicture;

    procedure appPropertiesMainShowHint(var HintStr: String;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure btAddToFavoritesClick(Sender: TObject);
    procedure btAbortCheckLatestVersionClick(Sender: TObject);
    procedure btCancelFavoritesCheckClick(Sender: TObject);
    procedure btChecksClick(Sender: TObject);
    procedure btCheckLatestVersionClick(Sender: TObject);
    procedure btClearLogFileClick(Sender: TObject);
    procedure btDonateClick(Sender: TObject);
    procedure btDownloadSplitClick(Sender: TObject);
    procedure btFavoritesImportClick(Sender: TObject);
    procedure btnDownloadFilterCustomDateApplyClick(Sender: TObject);
    procedure btOpenLogClick(Sender: TObject);
    procedure btReadOnlineClick(Sender: TObject);
    procedure btMangaListSearchClearClick(Sender: TObject);
    procedure btUpdateListClick(Sender: TObject);
    procedure btVisitMyBlogClick(Sender: TObject);
    procedure cbAddAsStoppedChange(Sender: TObject);
    procedure cbOptionAutoCheckFavIntervalChange(Sender: TObject);
    procedure cbOptionAutoCheckFavStartupChange(Sender: TObject);
    procedure cbOptionChangeUnicodeCharacterChange(Sender: TObject);
    procedure cbOptionDigitChapterChange(Sender: TObject);
    procedure cbOptionDigitVolumeChange(Sender: TObject);
    procedure cbOptionGenerateMangaFolderChange(Sender: TObject);
    procedure cbSelectMangaEditingDone(Sender: TObject);
    procedure cbSelectMangaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbSelectMangaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure clbChapterListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure clbChapterListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure clbChapterListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure clbChapterListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edDownloadsSearchButtonClick(Sender: TObject);
    procedure edDownloadsSearchChange(Sender: TObject);
    procedure edFavoritesSearchButtonClick(Sender: TObject);
    procedure edFavoritesSearchChange(Sender: TObject);
    procedure edFilterMangaInfoChaptersButtonClick(Sender: TObject);
    procedure edFilterMangaInfoChaptersChange(Sender: TObject);
    procedure edLogFileNameButtonClick(Sender: TObject);
    procedure edMangaListSearchChange(Sender: TObject);
    procedure edMangaListSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure edOptionDefaultPathButtonClick(Sender: TObject);
    procedure edOptionExternalPathButtonClick(Sender: TObject);
    procedure edSaveToButtonClick(Sender: TObject);
    procedure edURLButtonClick(Sender: TObject);
    procedure edURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edURLKeyPress(Sender: TObject; var Key: Char);
    procedure edWebsitesSearchButtonClick(Sender: TObject);
    procedure edWebsitesSearchChange(Sender: TObject);
    procedure HandleApplicationQueryEndSession(var Cancel: Boolean);
    procedure HandleApplicationEndSession(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure btDownloadClick(Sender: TObject);
    procedure btFavoritesCheckNewChapterClick(Sender: TObject);
    procedure btOptionApplyClick(Sender: TObject);

    procedure btFilterClick(Sender: TObject);
    procedure btFilterResetClick(Sender: TObject);
    procedure btRemoveFilterClick(Sender: TObject);

    procedure cbOptionUseProxyChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miChapterListAscendingClick(Sender: TObject);
    procedure miFavoritesDefaultActionCheckNewChaptersClick(Sender: TObject);
    procedure miFavoritesDefaultActionOpenFolderClick(Sender: TObject);
    procedure miFavoritesDefaultActionRenameClick(Sender: TObject);
    procedure miFavoritesDefaultActionShowInfoClick(Sender: TObject);
    procedure miFavoritesEnableClick(Sender: TObject);
    procedure miFavoritesRenameClick(Sender: TObject);
    procedure miFavoritesTransferWebsiteClick(Sender: TObject);
    procedure rbFavoritesShowAllChange(Sender: TObject);
    procedure rbFavoritesShowDisabledChange(Sender: TObject);
    procedure rbFavoritesShowEnabledChange(Sender: TObject);
    procedure tbmiDownloadMoveBottomClick(Sender: TObject);
    procedure tbmiDownloadMoveDownClick(Sender: TObject);
    procedure tbmiDownloadMoveTopClick(Sender: TObject);
    procedure tbmiDownloadMoveUpClick(Sender: TObject);
    procedure tbWebsitesSelectAllClick(Sender: TObject);
    procedure tbWebsitesUnselectAllClick(Sender: TObject);
    procedure tmAnimateMangaInfoTimer(Sender: TObject);
    procedure tmCheckFavoritesTimer(Sender: TObject);
    procedure tmExitCommandTimer(Sender: TObject);
    procedure tmRefreshDownloadsInfoStartTimer(Sender: TObject);
    procedure tmRefreshDownloadsInfoStopTimer(Sender: TObject);
    procedure tmRefreshDownloadsInfoTimer(Sender: TObject);
    procedure tmStartupTimer(Sender: TObject);
    procedure medURLCutClick(Sender: TObject);
    procedure medURLCopyClick(Sender: TObject);
    procedure medURLPasteClick(Sender: TObject);
    procedure medURLPasteandgoClick(Sender: TObject);
    procedure medtURLDeleteClick(Sender: TObject);
    procedure medURLSelectAllClick(Sender: TObject);
    procedure medURLUndoClick(Sender: TObject);
    procedure miAbortSilentThreadClick(Sender: TObject);
    procedure miChapterListFilterClick(Sender: TObject);
    procedure miChapterListHideDownloadedClick(Sender: TObject);
    procedure miDownloadEnableClick(Sender: TObject);
    procedure miDownloadViewMangaInfoClick(Sender: TObject);
    procedure miChapterListHighlightClick(Sender: TObject);
    procedure miDownloadDeleteTaskClick(Sender: TObject);
    procedure miDownloadMergeCompletedClick(Sender: TObject);
    procedure miFavoritesCheckNewChapterClick(Sender: TObject);
    procedure miFavoritesDownloadAllClick(Sender: TObject);
    procedure miFavoritesStopCheckNewChapterClick(Sender: TObject);
    procedure miFavoritesViewInfosClick(Sender: TObject);
    procedure miHighlightNewMangaClick(Sender: TObject);

    procedure miFavoritesDeleteClick(Sender: TObject);
    procedure miMangaListAddToFavoritesClick(Sender: TObject);
    procedure miFavoritesChangeSaveToClick(Sender: TObject);

    procedure miChapterListCheckSelectedClick(Sender: TObject);
    procedure miChapterListUncheckSelectedClick(Sender: TObject);
    procedure miChapterListCheckAllClick(Sender: TObject);
    procedure miChapterListUncheckAllClick(Sender: TObject);

    procedure miDownloadDeleteCompletedClick(Sender: TObject);
    procedure miDownloadResumeClick(Sender: TObject);
    procedure miDownloadRedownloadClick(Sender: TObject);
    procedure miDownloadStopClick(Sender: TObject);
    procedure miMangaListDeleteClick(Sender: TObject);
    procedure miMangaListDownloadAllClick(Sender: TObject);
    procedure miMangaListViewInfosClick(Sender: TObject);
    procedure miFavoritesOpenFolderClick(Sender: TObject);
    procedure miDownloadOpenFolderClick(Sender: TObject);
    procedure miFavoritesOpenWithClick(Sender: TObject);
    procedure miDownloadOpenWithClick(Sender: TObject);
    procedure miTrayExitClick(Sender: TObject);
    procedure miTrayFinishNothingClick(Sender: TObject);
    procedure miTrayShowDropBoxClick(Sender: TObject);
    procedure mnDownload1ClickClick(Sender: TObject);
    procedure mnFilterGenreAllCheckClick(Sender: TObject);
    procedure mnFilterGenreAllIndeterminateClick(Sender: TObject);
    procedure mnFilterGenreAllUncheckClick(Sender: TObject);
    procedure mnUpdate1ClickClick(Sender: TObject);
    procedure mnUpdateDownFromServerClick(Sender: TObject);
    procedure mnUpdateListClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pmDownloadPopup(Sender: TObject);
    procedure pmEditURLPopup(Sender: TObject);
    procedure pmFavoritesPopup(Sender: TObject);
    procedure pmMangaListPopup(Sender: TObject);
    procedure pmSbMainPopup(Sender: TObject);
    procedure pmTrayPopup(Sender: TObject);
    procedure rgOptionCompressSelectionChanged(Sender: TObject);
    procedure seOptionAutoCheckFavIntervalMinutesChange(Sender: TObject);
    procedure spMainSplitterMoved(Sender: TObject);
    procedure tbDownloadDeleteCompletedClick(Sender: TObject);
    procedure tbDownloadResumeAllClick(Sender: TObject);
    procedure tbDownloadStopAllClick(Sender: TObject);
    procedure tbDropTargetOpacityChange(Sender: TObject);
    procedure tbWebsitesCollapseAllClick(Sender: TObject);
    procedure tbWebsitesExpandAllClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure tvDownloadFilterSelectionChanged(Sender: TObject);
    procedure UniqueInstanceFMDOtherInstance(Sender: TObject;
      ParamCount: Integer; Parameters: array of String);
    procedure vtDownloadAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure vtDownloadColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtDownloadDragAllowed(Sender : TBaseVirtualTree;
      Node : PVirtualNode; Column : TColumnIndex; var Allowed : Boolean);
    procedure vtDownloadDragDrop(Sender : TBaseVirtualTree; Source : TObject;
      DataObject : IDataObject; Formats : TFormatArray; Shift : TShiftState;
      const Pt : TPoint; var Effect : LongWord; Mode : TDropMode);
    procedure vtDownloadDragOver(Sender : TBaseVirtualTree; Source : TObject;
      Shift : TShiftState; State : TDragState; const Pt : TPoint;
      Mode : TDropMode; var Effect : LongWord; var Accept : Boolean);
    procedure vtDownloadFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtDownloadGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtDownloadGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtDownloadGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtDownloadHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtDownloadKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure vtDownloadKeyDown(Sender : TObject; var Key : Word;
      Shift : TShiftState);
    procedure vtDownloadKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vtDownloadPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtFavoritesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtFavoritesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFavoritesColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtFavoritesDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure vtFavoritesDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure vtFavoritesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtFavoritesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtFavoritesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtFavoritesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtFavoritesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vtMangaListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMangaListColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtMangaListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMangaListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtMangaListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtMangaListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtOptionMangaSiteSelectionFreeNode(Sender : TBaseVirtualTree;
      Node : PVirtualNode);
    procedure vtOptionMangaSiteSelectionGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure DisableAddToFavorites(const AModule: Pointer);
  private
    PrevWindowState: TWindowState;
    procedure vtDownloadMoveItems(NextIndex : Cardinal; Mode : TDropMode);
  protected
    procedure FMDInstanceReceiveMsg(Sender: TObject);
    procedure ClearChapterListState;
    procedure DoExternalClose(Sender: TObject);
    procedure DoRestartFMD;
    procedure CloseNow;
  public
    LastSearchStr: String;
    LastSearchWeb: Pointer;
    LastUserPickedSaveTo: String;
    LastViewMangaInfoSender: TObject;

    CurrentFormLeft: Integer;
    CurrentFormTop: Integer;
    CurrentFormWidth: Integer;
    CurrentFormHeight: Integer;

    // state of chapterlist in mangainfo
    ChapterList: array of TChapterStateItem;

    // animation gif
    gifWaiting: TAnimatedGif;
    gifWaitingRect: TRect;

    // timer schedule
    Timer1Hour: TTimer;
    TimerBackup: TTimer;

    procedure OnTimer1Hour(Sender:TObject);
    procedure OnTimerBackup(Sender:TObject);

    // embed form
    procedure EmbedForm(const AForm: TForm; const AParent: TWinControl);

    // generate >> nodes
    procedure GeneratetvDownloadFilterNodes;

    // load about information
    procedure LoadAbout;
    procedure AddToAboutStatus(const ACaption, AValue: String; APanel: TPanel = nil);

    // use ExternalClose to close app in protected mode from thread or auto event
    procedure ExternalClose;
    procedure RestartFMD;

    // en: Too lazy to add it one by one
    procedure InitCheckboxes;

    // download task filters
    procedure FilterDownloadsByJDN(const lowJDN, highJDN: Integer);
    procedure tvDownloadFilterRefresh(const ResourceChanged: Boolean = False);
    procedure vtDownloadUpdateFilters(const RefreshTree: Boolean = True);
    procedure FilterDownloadByCustomDate;
    
    procedure vtFavoritesFilterCountChange;

    procedure AddChapterNameToList;

    // Create silent thread
    procedure AddSilentThread(URL: string; MetaDataType: TMetaDataType); overload;
    procedure AddSilentThread(URL: string); overload;

    // Add text to TRichMemo
    procedure AddTextToInfo(const ATitle, AValue: String);

    // fill edSaveTo with default path
    procedure FillSaveTo;
    procedure OverrideSaveTo(const AModule: Pointer);

    // View manga information
    procedure ViewMangaInfo(const AModule: Pointer; const ALink, ATitle, ASaveTo: String;
      const ASender: TObject; const AMangaListNode: PVirtualNode = nil);

    // Show manga information
    procedure ShowInformation;

    // Load config from config.ini
    procedure LoadOptions;
    procedure SaveOptions(const AShowDialog: Boolean = False);
    procedure ApplyOptions;

    // load supported website from websitemodules
    procedure LoadMangaOptions;

    procedure UpdateVtChapter;
    procedure UpdateVtDownload; inline;
    procedure UpdateVtFavorites;
    procedure UpdateVtMangaListFilterStatus;

    // load form information, like previous position, size, ...
    procedure LoadFormInformation;
    procedure SaveFormInformation;

    // drop target
    procedure ShowDropTarget(const AShow: Boolean);
    procedure SaveDropTargetFormInformation;

    // load language from file
    procedure CollectLanguagesFromFiles;
    procedure ApplyLanguage;

    // openwith
    procedure OpenWithExternalProgramChapters(const Dir: String;
      const Chapters: TStrings = nil);
    procedure OpenWithExternalProgram(const Dir, Filename: String);

    // transfer rate graph
    procedure TransferRateGraphInit(xCount: Integer = 10);
    procedure TransferRateGraphAddItem(TransferRate: Integer);

    // exit counter
    procedure DoExitWaitCounter;
    function ShowExitCounter: Boolean;

    // open db with thread
    procedure OpenDataDB(const AWebsite: String);

    // search db with thread
    procedure SearchDataDB(const ATitle: String);

    // change all filter genre checkbox state
    procedure FilterGenreChangeAllState(const AState: TCheckBoxState);

    // filter chapter list
    procedure FilterChapterList(const SearchStr: String; const HideDownloaded: Boolean);

    // exception handle
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    { public declarations }
  end;

  { TOpenDBThread }

  TOpenDBThread = class(TThread)
  private
    FWebsite: String;
  protected
    procedure SetControlEnabled(const Value: Boolean);
    procedure SyncOpenStart;
    procedure SyncOpenFinish;
    procedure Execute; override;
  public
    constructor Create(const AWebsite: String);
    destructor Destroy; override;
  end;

  { TSearchDBThread }

  TSearchDBThread = class(TThread)
  private
    FSearchStr: String;
    FNewSearch: Boolean;
  protected
    procedure SyncBeforeSearch;
    procedure SyncAfterSearch;
    procedure Execute; override;
  public
    constructor Create(const ASearchStr: String);
    destructor Destroy; override;
    procedure NewSearch(const ASearchStr: String);
  end;

  PMangaInfoData = ^TMangaInfoData;

  TMangaInfoData = record
    Module: Pointer;
    Link,
    Title,
    AltTitles,
    TitleFormat,
    Authors,
    Artists,
    Genres,
    Status,
    Summary: String;
    NumChapter,
    JDN: Integer;
  end;

var
  MainForm: TMainForm;
  TimerBackupInterval: Integer = 10;

const
  CL_HLBlueMarks        = $FDC594;
  CL_HLGreenMarks       = $B8FFB8;
  CL_HLRedMarks         = $8080FF;
  CL_HLYellowMarks      = $80EBFE;

  CL_BarGrayLine        = $bcbcbc;
  CL_BarGray            = $e6e6e6;

  CL_BarGreenLine       = $25b006;
  CL_BarGreen           = $42d932;

  CL_BarOrangeLine       = $00b399;
  CL_BarOrange          = $1870e9;

  CL_BarRedLine         = $1a1ab1;
  CL_BarRed             = $4b4af0;

  CL_BarBlueLine        = $b36b1d;
  CL_BarBlue            = $fab24f;

  CL_BarBlueLightLine   = $eab27c;
  CL_BarBlueLight       = $fed2a3;

  CL_BarYellowLine      = $4a4af0;
  CL_BarYellow          = $80ebfe;

  CL_BarBrownGoldLine   = $5ea2c8;
  CL_BarBrownGold       = $8dd5f0;

  CL_YellowLight        = $eaffff;
  CL_BlueLight          = $f8f8f3;

resourcestring
  RS_FilterStatusItems = 'Completed'#13#10'Ongoing'#13#10'Hiatus'#13#10'Cancelled'#13#10'<none>';
  RS_OptionFMDDoItems = 'Nothing'#13#10'Exit'#13#10'Shutdown'#13#10'Hibernate';
  RS_DropTargetModeItems = 'Download all'#13#10'Add to favorites';
  RS_OptionCompress = 'None'#13#10'ZIP'#13#10'CBZ'#13#10'PDF'#13#10'EPUB';
  RS_WebPConvertTo = 'WebP'#13#10'PNG'#13#10'JPEG';
  RS_WebPPNGLevel = 'None'#13#10'Fastest'#13#10'Default'#13#10'Maximum';

  RS_HintFavoriteProblem = 'There is a problem with this data!'#13#10
                         + 'Removing and re-adding this data may fix the problem.';
  RS_DlgTitleExistInDLlist = 'This title are already in download list.'#13#10
                           + 'Do you want to download it anyway?';
  RS_DlgQuit = 'Are you sure you want to exit?';
  RS_DlgRemoveItem = 'Are you sure you want to delete this item(s)?';
  RS_DlgRemoveTask = 'Are you sure you want to delete the task(s)?';
  RS_DlgRemoveFavorite = 'Are you sure you want to delete the favorite(s)?';
  RS_DlgURLNotSupport = 'URL not supported!';
  RS_DlgUpdaterIsRunning = 'Updater is running!';
  RS_DlgTypeInNewChapter = 'Type in new chapter:';
  RS_DlgTypeInNewSavePath = 'Type in new save path:';
  RS_DlgUpdaterWantToUpdateDB = 'Do you want to download manga list from FMD server?';
  RS_DlgRemoveFinishTasks = 'Are you sure you want to delete all finished tasks?';
  RS_DlgCannotGetMangaInfo = 'Cannot get manga info. Please check your internet connection and try it again.';
  RS_DlgCannotConnectToServer = 'Cannot connect to the server.';
  RS_DlgSplitDownload = 'Split download';
  RS_DlgDownloadCount = 'Download count:';
  RS_WrongInput = 'Invalid input!';
  RS_LblOptionExternalParamsHint = '%s : Path to the manga'#13#10+
                                   '%s : Chapter filename'#13#10+
                                   #13#10+
                                   'Example : "%s%s"';
  RS_LblAutoCheckNewChapterMinute = 'Auto check for new chapter every %d minutes';
  RS_BtnOK = '&OK';
  RS_Loading = 'Loading ...';
  RS_Checking = 'Checking...';
  RS_AllDownloads = 'All downloads';
  RS_InProgress = 'In progress';

  RS_History = 'History';
  RS_Today = 'Today';
  RS_Yesterday = 'Yesterday';
  RS_Last7days = 'Last 7 days';
  RS_ThisMonth = 'This month';
  RS_Last6months = 'Last 6 months';
  RS_OlderThan6months = 'Older than 6 months';
  RS_Custom = 'Custom';

  RS_Import = 'Import';
  RS_Software = 'Software';
  RS_SoftwarePath = 'Path to the software (e.g. C:\MangaDownloader)';
  RS_Cancel = 'Cancel';
  RS_ModeAll = 'Mode: Show all (%d)';
  RS_ModeFiltered = 'Mode: Filtered (%d)';
  RS_Selected = 'Selected: %d';
  RS_InfoWebsite = 'Website:';
  RS_InfoTitle = 'Title:';
  RS_InfoAltTitles = 'Alternative Title(s):';
  RS_InfoAuthors = 'Author(s):';
  RS_InfoArtists = 'Artist(s):';
  RS_InfoGenres = 'Genre(s):';
  RS_InfoStatus = 'Status:';
  RS_InfoStatus_Unknown = 'Unknown';
  RS_InfoSummary = 'Summary:';
  RS_FMDAlreadyRunning = 'Free Manga Downloader already running!';
  RS_ModeSearching = 'Mode: Searching...';
  RS_FavoritesShowAll = 'All';
  RS_FavoritesShowEnabled = 'Enabled';
  RS_FavoritesShowDisabled = 'Disabled';

  RS_Version = 'Version';
  RS_Revision = 'Revision';

implementation

{$R *.lfm}

uses
  {$ifdef windows}
  WinAPI,
  {$endif}
  frmImportFavorites, frmShutdownCounter, frmSelectDirectory,
  frmWebsiteSettings, WebsiteModules, uUpdateThread, FMDVars, RegExpr, sqlite3dyn, Clipbrd,
  ssl_openssl_lib, LazFileUtils, LazUTF8, webp, DBUpdater, pcre2, pcre2lib, dynlibs,
  LuaWebsiteModules, LuaBase, uBackupSettings;

var
  // thread for open db
  OpenDBThread: TOpenDBThread;

  // thread for search db
  SearchDBThread: TSearchDBThread;

  END_SESSION: Boolean = False;

{$ifdef windows}
  // prevent computer sleep counter
  StandbyCounter: Integer;

var
  PrevWndProc: windows.WNDPROC;

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
  if uMsg = WM_DISPLAYCHANGE then
  begin
    Screen.UpdateMonitors;
    Screen.UpdateScreen;
    if Screen.MonitorCount < MainForm.Monitor.MonitorNum then
      MainForm.DefaultMonitor := dmMainForm;
    if (MainForm.Left > Screen.Width) or (MainForm.Top > Screen.Height) then
      MainForm.MoveToDefaultPosition;
  end;
  Result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;
{$endif}

procedure ChangeAllCursor(const ParentControl: TWinControl; const Cur: TCursor);
var
  i: Integer;
begin
  if ParentControl = nil then Exit;
  ParentControl.Cursor := Cur;
  if ParentControl.ControlCount > 0 then
    for i := 0 to ParentControl.ControlCount - 1 do
      ParentControl.Controls[i].Cursor := Cur;
end;

{ TSearchDBThread }

procedure TSearchDBThread.SyncBeforeSearch;
begin
  with MainForm do
  begin
    vtMangaList.Cursor := crHourGlass;
    lbMode.Caption := RS_ModeSearching;
    vtMangaList.RootNodeCount := 0;
  end;
end;

procedure TSearchDBThread.SyncAfterSearch;
begin
  with MainForm do
  begin
    vtMangaList.RootNodeCount := dataProcess.RecordCount;
    UpdateVtMangaListFilterStatus;
    LastSearchWeb := currentWebsite;
    LastSearchStr := UpCase(FSearchStr);
    vtMangaList.Cursor := crDefault;
  end;
end;

procedure TSearchDBThread.Execute;
begin
  if dataProcess <> nil then
  begin
    Synchronize(@SyncBeforeSearch);
    while FNewSearch do
    begin
      FNewSearch := False;
      dataProcess.Search(FSearchStr);
    end;
    if not Terminated then
      Synchronize(@SyncAfterSearch);
  end;
end;

constructor TSearchDBThread.Create(const ASearchStr: String);
begin
  FreeOnTerminate := True;
  FSearchStr := ASearchStr;
  FNewSearch := True;
  inherited Create(False);
end;

destructor TSearchDBThread.Destroy;
begin
  SearchDBThread := nil;
  inherited Destroy;
end;

procedure TSearchDBThread.NewSearch(const ASearchStr: String);
begin
  if ASearchStr <> FSearchStr then
  begin
    FSearchStr := ASearchStr;
    FNewSearch := True;
  end;
end;

{ TOpenDBThread }

procedure TOpenDBThread.SetControlEnabled(const Value: Boolean);
begin
  with MainForm do
  begin
    cbSelectManga.Enabled := Value;
    btUpdateList.Enabled := Value;
    edMangaListSearch.Enabled := Value;
    btMangaListSearchClear.Enabled := Value;
    btRemoveFilter.Enabled := Value;
  end;
end;

procedure TOpenDBThread.SyncOpenStart;
begin
  with MainForm do
  begin
    ChangeAllCursor(pssInfoList, crHourGlass);
    SetControlEnabled(False);
    lbMode.Caption := RS_Loading;
    vtMangaList.Clear;
  end;
end;

procedure TOpenDBThread.SyncOpenFinish;
begin
  with MainForm do
  begin
    LastSearchStr := upcase(edMangaListSearch.Text);
    LastSearchWeb := currentWebsite;
    if dataProcess.Filtered then
      lbMode.Caption := Format(RS_ModeFiltered, [dataProcess.RecordCount])
    else
      lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
    SetControlEnabled(True);
    vtMangaList.RootNodeCount := dataProcess.RecordCount;
    ChangeAllCursor(pssInfoList, crDefault);
  end;
end;

procedure TOpenDBThread.Execute;
begin
  if (FWebsite <> '') and (dataProcess <> nil) then
  begin
    Synchronize(@SyncOpenStart);
    if dataProcess <> nil then
    begin
      dataProcess.Open(FWebsite);
      if FormMain.edMangaListSearch.Text <> '' then
        dataProcess.Search(MainForm.edMangaListSearch.Text);
    end;
    if not Terminated then
      Synchronize(@SyncOpenFinish);
  end;
end;

constructor TOpenDBThread.Create(const AWebsite: String);
begin
  FreeOnTerminate := True;
  FWebsite := AWebsite;
  inherited Create(False);
end;

destructor TOpenDBThread.Destroy;
begin
  OpenDBThread := nil;
  inherited Destroy;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
  FormMain := Self;
  {$ifdef windows}
  PrevWndProc := windows.WNDPROC(windows.GetWindowLongPtr(Self.Handle, GWL_WNDPROC));
  windows.SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback));
  {$endif}
  Application.OnQueryEndSession := @HandleApplicationQueryEndSession;
  Application.OnEndSession := @HandleApplicationEndSession;

  isStartup := True;
  isRunDownloadFilter := False;
  isUpdating := False;
  isPendingExitCounter:=False;
  isNormalExit:=False;
  DoAfterFMD := DO_NOTHING;
  Application.HintHidePause := 10000;

  ForceDirectoriesUTF8(USERDATA_FOLDER);
  ForceDirectoriesUTF8(DATA_FOLDER);

  // load about
  LoadAbout;

  // remove old updater
  if FileExistsUTF8(OLD_CURRENT_UPDATER_EXE) then
  begin
    if FileExistsUTF8(CURRENT_UPDATER_EXE) then
      DeleteFileUTF8(OLD_CURRENT_UPDATER_EXE)
    else
      RenameFileUTF8(OLD_CURRENT_UPDATER_EXE, CURRENT_UPDATER_EXE);
  end;

  // TrayIcon
  TrayIcon.Icon.Assign(Application.Icon);
  PrevWindowState := wsNormal;

  // main dataprocess
  dataProcess := TDBDataProcess.Create;

  // downloadmanager
  DLManager := TDownloadManager.Create;

  // favorites
  FavoriteManager := TFavoriteManager.Create;
  FavoriteManager.DLManager := DLManager;
  FavoriteManager.OnUpdateFavorite := @UpdateVtFavorites;
  FavoriteManager.OnUpdateDownload := @UpdateVtDownload;

  // download all / add to favorites
  SilentThreadManager := TSilentThreadManager.Create;

  // ShowInformation
  mangaInfo := TMangaInfo.Create;

  // generate tvDownloadFilter nodes
  GeneratetvDownloadFilterNodes;

  // set connection limit
  seOptionMaxParallel.MaxValue := MAX_TASKLIMIT;
  seOptionMaxThread.MaxValue := MAX_CONNECTIONPERHOSTLIMIT;

  if cbFilterStatus.Items.Count > 4 then
    cbFilterStatus.ItemIndex := 4;

  InitCheckboxes;

  pcMain.ActivePage := tsDownload;

  TrayIcon.Show;

  currentJDN := GetCurrentJDN;

  // read online
  btDownload.Enabled := False;
  btDownloadSplit.Enabled := btDownload.Enabled;
  btReadOnline.Enabled := False;
  btAddToFavorites.Enabled := False;

  // waiting gif
  if FileExistsUTF8(IMAGE_FOLDER + 'waiting.gif') then
    try
      gifWaiting := TAnimatedGif.Create(IMAGE_FOLDER + 'waiting.gif');
      gifWaiting.EraseColor := Self.Color;
      gifWaiting.BackgroundMode := gbmSaveBackgroundOnce;
      gifWaitingRect.Left := 53;
      gifWaitingRect.Top := 84;
      gifWaitingRect.Right := 101;
      gifWaitingRect.Bottom := 131;
    except
    end;

  mangaCover := TPicture.Create;

  // embed form
  CustomColorForm := TCustomColorForm.Create(Self);
  EmbedForm(CustomColorForm, tsCustomColor);

  AccountManagerForm := TAccountManagerForm.Create(Self);
  EmbedForm(AccountManagerForm, tsAccounts);

  WebsiteOptionCustomForm := TCustomOptionForm.Create(Self);
  EmbedForm(WebsiteOptionCustomForm, sbWebsiteOptions);

  WebsiteSettingsForm := TWebsiteSettingsForm.Create(Self);
  EmbedForm(WebsiteSettingsForm, tsWebsiteAdvanced);

  LuaModulesUpdaterForm := TLuaModulesUpdaterForm.Create(Self);
  EmbedForm(LuaModulesUpdaterForm, tsWebsiteModules);

  // init vt
  vtDownload.NodeDataSize := SizeOf(TDownloadInfo);
  vtFavorites.NodeDataSize := SizeOf(TFavoriteInfo);
  vtMangaList.NodeDataSize := SizeOf(TMangaInfoData);
  vtOptionMangaSiteSelection.NodeDataSize := SizeOf(TNamePointerItem);
  AddVT(Self.vtMangaList);
  AddVT(Self.clbChapterList);
  AddVT(Self.vtDownload);
  AddVT(Self.vtFavorites);
  AddVT(Self.vtOptionMangaSiteSelection);

  // logger
  FormLogger := TFormLogger.Create(Self);

  // hint
  ShowHint := True;
  Application.HintPause := 500;
  Application.HintHidePause := 3000;

  // transfer rate graph
  TransferRateGraphList.DataPoints.NameValueSeparator := '|';
  TransferRateGraph.Visible := False;

  // minimize on start
  if settingsfile.ReadBool('general', 'MinimizeOnStart', False) then
    Application.ShowMainForm := False;

  LoadFormInformation;
  CollectLanguagesFromFiles;
  ApplyLanguage;

  tmStartup:=TTimer.Create(self);
  with tmStartup do
  begin
    OnTimer := @tmStartupTimer;
    Interval := 100;
    Enabled := True;
  end;

  Timer1Hour:=TTimer.Create(Self);
  with Timer1Hour do
  begin
    OnTimer:=@OnTimer1Hour;
    Interval:=1000*60*60;
    Enabled:=True;
  end;

  TimerBackup:=TTimer.Create(Self);
  with Timer1Hour do
  begin
    OnTimer:=@OnTimerBackup;
    Interval:=1000*60*TimerBackupInterval;
    Enabled:=True;
  end;

  Self.Caption := Self.Caption + ' v' + FMD_VERSION_STRING;
  AddToAboutStatus(RS_Version, FMD_VERSION_STRING, pnAboutVersion);
  if REVISION_NUMBER <> '' then
    AddToAboutStatus(RS_Revision, REVISION_NUMBER+' ('+REVISION_SHA+')', pnAboutVersion);

  if AlwaysLoadLuaFromFile then
    Caption := Caption + ' --lua-dofile';
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (END_SESSION=False) and
    (cbOptionShowQuitDialog.Checked and (DoAfterFMD = DO_NOTHING) and (not OptionRestartFMD)) then
  begin
    if MessageDlg('', RS_DlgQuit, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      Logger.Send(Self.ClassName+'.FormClose aborted!');
      CloseAction := caNone;
      Exit;
    end;
  end;
  Logger.Send(Self.ClassName+'.FormClose');
  Hide;
  CloseNow;
  CloseAction := caFree;
end;

procedure TMainForm.CloseNow;
begin
  if OptionDeleteCompletedTasksOnClose then
    miDownloadDeleteCompletedClick(nil);

  isExiting := True;

  {$ifdef windows}
  if Assigned(PrevWndProc) then
    windows.SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(PrevWndProc));
  {$endif}

  if FavoriteManager.isRunning then
    FavoriteManager.StopChekForNewChapter(True);
  if SilentThreadManager.Count > 0 then
    SilentThreadManager.StopAll(True);
  if DLManager.ItemsActiveTask.Count > 0 then
    DLManager.StopAllDownloadTasksForExit;

  //Terminating all threads and wait for it
  if Assigned(CheckUpdateThread) then CheckUpdateThread.Terminate;
  if Assigned(SearchDBThread) then SearchDBThread.Terminate;
  if Assigned(OpenDBThread) then OpenDBThread.Terminate;
  if Assigned(GetInfosThread) then try GetInfosThread.Terminate; except end;
  if isUpdating then updateList.Terminate;
  if Assigned(DBUpdaterThread) then DBUpdaterThread.Terminate;
  if Assigned(SelfUpdaterThread) then SelfUpdaterThread.Terminate;

  if Assigned(CheckUpdateThread) then CheckUpdateThread.WaitFor;
  if Assigned(SearchDBThread) then SearchDBThread.WaitFor;
  if Assigned(OpenDBThread) then OpenDBThread.WaitFor;
  if Assigned(GetInfosThread) then GetInfosThread.WaitFor;
  if isUpdating then updateList.WaitFor;
  if Assigned(DBUpdaterThread) then DBUpdaterThread.WaitFor;
  if Assigned(SelfUpdaterThread) then SelfUpdaterThread.WaitFor;

  Timer1Hour.Enabled := False;
  TimerBackup.Enabled := False;
  tmRefreshDownloadsInfo.Enabled := False;
  tmCheckFavorites.Enabled := False;
  tmAnimateMangaInfo.Enabled := False;
  tmExitCommand.Enabled := False;

  //Backup data
  DLManager.Backup;
  FavoriteManager.Backup;
  SaveOptions;
  SaveFormInformation;

  //embed form
  if Assigned(AccountManagerForm) then
    AccountManagerForm.Close;

  if Assigned(FormDropTarget) then
    FormDropTarget.Close;

  if FMDInstance <> nil then
  begin
    FMDInstance.StopServer;
    FreeAndNil(FMDInstance);
  end;
  isNormalExit:=True;

  Logger.Send('NormalExit', isNormalExit);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SilentThreadManager.Free;
  DLManager.Free;
  FavoriteManager.Free;
  dataProcess.Free;

  SetLength(ChapterList, 0);
  mangaInfo.Free;
  gifWaiting.Free;
  mangaCover.Free;

  if isNormalExit then
    Logger.Send(QuotedStrd(Application.Title)+' exit normally [PID:'+IntToStr(GetProcessID)+']')
  else
    Logger.SendWarning(QuotedStrd(Application.Title)+' doesn''t exit normally [PID:'+IntToStr(GetProcessID)+']');

  if OptionRestartFMD then
    DoRestartFMD;
end;

procedure TMainForm.cbOptionUseProxyChange(Sender: TObject);
begin
  gbOptionProxy.Enabled := cbOptionUseProxy.Checked;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState = wsMinimized) then
  begin
    if cbOptionMinimizeToTray.Checked then
    begin
      ShowInTaskBar := stNever;
      Hide;
      if not TrayIcon.Visible then
        TrayIcon.Show;
    end;
  end
  else
    PrevWindowState := WindowState;
end;

procedure TMainForm.miChapterListAscendingClick(Sender: TObject);
var
  i, j, f: Integer;
  t: TChapterStateItem;
  Node, FNode: PVirtualNode;
  c: array of TCheckState;
begin
  if not (Sender is TMenuItem) then Exit;
  if TMenuItem(Sender).Checked then Exit;
  TMenuItem(Sender).Checked := True;
  settingsfile.WriteBool('general', 'SortChapterListAscending', miChapterListAscending.Checked);
  if Length(ChapterList) <> 0 then
  begin
    // invert chapterlist
    for i := Low(ChapterList) to (High(ChapterList) div 2) do
    begin
      j := High(ChapterList) - i;
      t := ChapterList[i];
      ChapterList[i] := ChapterList[j];
      ChapterList[j] := t;
    end;
    // rearrange checked state and focused
    if (clbChapterList.CheckedCount <> 0 ) or (clbChapterList.SelectedCount <> 0) then
    begin
      FNode := nil;
      if Assigned(clbChapterList.FocusedNode) then
        f := clbChapterList.FocusedNode^.Index
      else
        f := -1;
      SetLength(c, clbChapterList.RootNodeCount);
      Node := clbChapterList.GetFirst();
      while Assigned(Node) do
      begin
        c[Node^.Index] := Node^.CheckState;
        Node := clbChapterList.GetNext(Node);
      end;
      i := Low(c);
      Node := clbChapterList.GetLast();
      while Assigned(Node) do
      begin
        if i = f then
          FNode := Node;
        Node^.CheckState := c[i];
        Inc(i);
        Node := clbChapterList.GetPrevious(Node);
      end;
      SetLength(c, 0);
      if Assigned(FNode) then
        clbChapterList.FocusedNode := FNode
    end;
    clbChapterList.ClearSelection;
    clbChapterList.Repaint;
  end;
end;

procedure TMainForm.miFavoritesDefaultActionCheckNewChaptersClick(Sender: TObject);
begin
  OptionDefaultAction := 3;
  settingsfile.WriteInteger('favorites', 'DefaultAction', 3);
end;

procedure TMainForm.miFavoritesDefaultActionOpenFolderClick(Sender: TObject);
begin
  OptionDefaultAction := 0;
  settingsfile.WriteInteger('favorites', 'DefaultAction', 0);
end;

procedure TMainForm.miFavoritesDefaultActionRenameClick(Sender: TObject);
begin
  OptionDefaultAction := 2;
  settingsfile.WriteInteger('favorites', 'DefaultAction', 2);
end;

procedure TMainForm.miFavoritesDefaultActionShowInfoClick(Sender: TObject);
begin
  OptionDefaultAction := 1;
  settingsfile.WriteInteger('favorites', 'DefaultAction', 1);
end;

procedure TMainForm.miFavoritesEnableClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  FavoriteManager.Lock;
  try
    Node := vtFavorites.GetFirstSelected();
    while Assigned(Node) do
    begin
      if Sender = miFavoritesDisable then
        FavoriteManager.StopChekForNewChapter(False, Node^.Index);
      FavoriteManager[Node^.Index].Enabled := (Sender = miFavoritesEnable);
      Node := vtFavorites.GetNextSelected(Node);
    end;
  finally
    FavoriteManager.UnLock;
  end;
  UpdateVtFavorites;
  vtFavoritesFilterCountChange;
end;

procedure TMainForm.miFavoritesRenameClick(Sender: TObject);
var
  node: PVirtualNode;
  t: TFavoriteContainer;
  tt: String;
begin
  node := vtFavorites.GetFirstSelected();
  if Assigned(node) then
  begin
    t := FavoriteManager.Items[node^.Index];
    tt := t.FavoriteInfo.Title;
    if InputQuery('', RS_InfoTitle, tt) then
    begin
      t.FavoriteInfo.Title := tt;
      t.DBUpdateTitle;
    end;
  end;
end;

procedure TMainForm.miFavoritesTransferWebsiteClick(Sender: TObject);
var
  Node: PVirtualNode;
  sm: Integer;
  Data: PFavContainer;
begin
  with TTransferFavoritesForm.Create(nil) do
  try
    FavoriteManager.isRunning := True;
    sm := mrNone;
    try
      Node := vtFavorites.GetFirstSelected();
      while Assigned(Node) do
      begin
        AddFav(FavoriteManager.Items[Node^.Index]);
        Node := vtFavorites.GetNextSelected(Node);
      end;
      sm := ShowModal;
    finally
      FavoriteManager.isRunning := False;
    end;
    if sm = mrOK then
    begin
      UpdateVtFavorites;
      if ckClearDownloadedChapters.Checked then
      begin
        Node := vtFavs.GetFirst();
        while Assigned(Node) do
        begin
          Data := vtFavs.GetNodeData(Node);
          if Data^.NewLink <> '' then
            FavoriteManager.CheckForNewChapter(FavoriteManager.Items.IndexOf(Data^.Fav));
          Node := vtFavs.GetNext(Node);
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.rbFavoritesShowAllChange(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if rbFavoritesShowAll.Checked = False then Exit;
  if vtFavorites.RootNodeCount = 0 then Exit;
  vtFavorites.BeginUpdate;
  try
    xNode := vtFavorites.GetFirst();
    while Assigned(xNode) do
    begin
      vtFavorites.IsVisible[xNode] := True;
      xNode := vtFavorites.GetNext(xNode);
    end;
  finally
    vtFavorites.EndUpdate;
  end;
end;

procedure TMainForm.rbFavoritesShowDisabledChange(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if rbFavoritesShowDisabled.Checked = False then Exit;
  if vtFavorites.RootNodeCount = 0 then Exit;
  vtFavorites.BeginUpdate;
  try
    xNode := vtFavorites.GetFirst();
    while Assigned(xNode) do
    begin
      vtFavorites.IsVisible[xNode] := not FavoriteManager.Items[xNode^.Index].Enabled;
      xNode := vtFavorites.GetNext(xNode);
    end;
  finally
    vtFavorites.EndUpdate;
  end;
end;

procedure TMainForm.rbFavoritesShowEnabledChange(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if rbFavoritesShowEnabled.Checked = False then Exit;
  if vtFavorites.RootNodeCount = 0 then Exit;
  vtFavorites.BeginUpdate;
  try
    xNode := vtFavorites.GetFirst();
    while Assigned(xNode) do
    begin
      vtFavorites.IsVisible[xNode] := FavoriteManager.Items[xNode^.Index].Enabled;
      xNode := vtFavorites.GetNext(xNode);
    end;
  finally
    vtFavorites.EndUpdate;
  end;
end;

procedure TMainForm.tbmiDownloadMoveTopClick(Sender: TObject);
begin
  if vtDownload.SelectedCount = 0 then Exit;
  vtDownloadMoveItems(0, dmAbove);
end;

procedure TMainForm.tbmiDownloadMoveUpClick(Sender: TObject);
var
  p: Cardinal;
begin
  if vtDownload.SelectedCount = 0 then Exit;
  p := vtDownload.GetFirstSelected()^.Index;
  if p > 0 then
    vtDownloadMoveItems(p - 1, dmAbove);
end;

procedure TMainForm.tbWebsitesSelectAllClick(Sender: TObject);
var
  node: PVirtualNode;
  b: Boolean = False;
begin
  { Check if any top level entries are expanded }
  node := vtOptionMangaSiteSelection.GetFirstVisible();
  while node<>nil do
  begin
    if node^.ChildCount = 0 then
    begin
      b := True;
      // If at least one element without child elements is visible, we don't have to expand all elements:
      break;
    end;
    node := vtOptionMangaSiteSelection.GetNextVisible(node);
  end;

  // If all top level entries are collapsed, expand all:
  if b = False then vtOptionMangaSiteSelection.FullExpand;

  { Set all visible elements to checked state }
  node := vtOptionMangaSiteSelection.GetFirstVisible();
  while node<>nil do
  begin
    if node^.ChildCount = 0 then
    begin
      node^.CheckState := csCheckedNormal;
    end;
    node := vtOptionMangaSiteSelection.GetNextVisible(node);
  end;
  vtOptionMangaSiteSelection.Refresh();
end;

procedure TMainForm.tbWebsitesUnselectAllClick(Sender: TObject);
var
  node: PVirtualNode;
  b: Boolean = False;
begin
  { Check if any top level entries are expanded }
  node := vtOptionMangaSiteSelection.GetFirstVisible();
  while node<>nil do
  begin
    if node^.ChildCount = 0 then
    begin
      b := True;
      // If at least one element without child elements is visible, we don't have to expand all elements:
      break;
    end;
    node := vtOptionMangaSiteSelection.GetNextVisible(node);
  end;

  // If all top level entries are collapsed, expand all:
  if b = False then vtOptionMangaSiteSelection.FullExpand;

  { Set all visible elements to unchecked state }
  node := vtOptionMangaSiteSelection.GetFirstVisible();
  while node<>nil do
  begin
    if node^.ChildCount = 0 then
    begin
      node^.CheckState := csUncheckedNormal;
    end;
    node := vtOptionMangaSiteSelection.GetNextVisible(node);
  end;
  vtOptionMangaSiteSelection.Refresh();
end;

procedure TMainForm.tbmiDownloadMoveDownClick(Sender: TObject);
begin
  if vtDownload.SelectedCount = 0 then Exit;
  vtDownloadMoveItems(vtDownload.GetFirstSelected()^.Index, dmBelow);
end;

procedure TMainForm.tbmiDownloadMoveBottomClick(Sender: TObject);
begin
  if vtDownload.SelectedCount = 0 then Exit;
    vtDownloadMoveItems(vtDownload.RootNodeCount - 1, dmBelow);
end;

procedure TMainForm.tmAnimateMangaInfoTimer(Sender: TObject);
begin
  gifWaiting.Update(pbWait.Canvas, gifWaitingRect);
end;

procedure TMainForm.tmCheckFavoritesTimer(Sender: TObject);
begin
  if IsDlgCounter then Exit;
  tmCheckFavorites.Enabled := False;
  if OptionAutoCheckLatestVersion then
  begin
    btCheckLatestVersionClick(btCheckLatestVersion);
    LuaModulesUpdaterForm.btCheckUpdateClick(LuaModulesUpdaterForm.btCheckUpdate);
  end;
  FavoriteManager.isAuto := True;
  FavoriteManager.CheckForNewChapter;
end;

function TMainForm.ShowExitCounter: Boolean;
begin
  IsDlgCounter := True;
  with TShutdownCounterForm.Create(nil) do try
    case DoAfterFMD of
      DO_POWEROFF:
        begin
          WaitTimeout := 60;
          LabelMessage := RS_LblMessageShutdown;
        end;
      DO_HIBERNATE:
        begin
          WaitTimeout := 30;
          LabelMessage := RS_LblMessageHibernate;
        end;
      DO_EXIT:
        begin
          WaitTimeout := 5;
          LabelMessage := RS_LblMessageExit;
        end;
      else;
    end;
    Result := (ShowModal = mrOK);
  finally
    Free;
  end;
  isPendingExitCounter:=False;
  IsDlgCounter := False;
end;

procedure TMainForm.OpenDataDB(const AWebsite: String);
begin
  if OpenDBThread = nil then
    OpenDBThread := TOpenDBThread.Create(AWebsite);
end;

procedure TMainForm.SearchDataDB(const ATitle: String);
begin
  if SearchDBThread = nil then
    SearchDBThread := TSearchDBThread.Create(ATitle)
  else
  begin
    SearchDBThread.NewSearch(ATitle);
  end;
end;

procedure TMainForm.FilterGenreChangeAllState(const AState: TCheckBoxState);
var
  i: Integer;
begin
  for i := 0 to pnGenres.ControlCount - 1 do
    if pnGenres.Controls[i] is TCheckBox then
      TCheckBox(pnGenres.Controls[i]).State := AState;
end;

procedure TMainForm.FilterChapterList(const SearchStr: String;
  const HideDownloaded: Boolean);
var
  Node: PVirtualNode;
  S: String;
  isShow: Boolean;
begin
  if clbChapterList.RootNodeCount = 0 then Exit;
  with clbChapterList do
    try
      BeginUpdate;
      S := AnsiUpperCase(SearchStr);
      Node := GetFirst();
      while Assigned(Node) do
      begin
        isShow := True;
        if HideDownloaded then
          isShow := not ChapterList[Node^.Index].Downloaded;
        if isShow and (S <> '') then
          isShow := Pos(S, AnsiUpperCase(ChapterList[Node^.Index].Title)) <> 0;
        IsVisible[Node] := isShow;
        Node := GetNext(Node);
      end;
    finally
      EndUpdate;
    end;
end;

procedure TMainForm.tmExitCommandTimer(Sender: TObject);
begin
  tmExitCommand.Enabled := False;
  if DoAfterFMD <> DO_NOTHING then
  begin
    if DoAfterFMD in [DO_POWEROFF, DO_HIBERNATE, DO_EXIT] then
    begin
      if ShowExitCounter then
      begin
        Self.CloseNow;
        if DoAfterFMD = DO_POWEROFF then
          fmdPowerOff
        else
        if DoAfterFMD = DO_HIBERNATE then
          fmdHibernate;
        Self.Close;
      end;
    end
    else
    if DoAfterFMD = DO_UPDATE then
    begin
      Self.CloseNow;
      Self.Close;
    end;
    DoAfterFMD := DO_NOTHING;
  end;
end;

procedure TMainForm.tmRefreshDownloadsInfoStartTimer(Sender: TObject);
begin
  if Assigned(DLManager) then
  begin
    TransferRateGraphInit(round(TransferRateGraph.Width/4)+1);
    TransferRateGraph.Visible := True;
    {$ifdef windows}
    StandbyCounter := 0;
    {$endif}
  end
  else
    tmRefreshDownloadsInfo.Enabled := False;
end;

procedure TMainForm.tmRefreshDownloadsInfoStopTimer(Sender: TObject);
begin
  TransferRateGraph.Visible := False;
  vtDownload.Repaint;
end;

procedure TMainForm.tmRefreshDownloadsInfoTimer(Sender: TObject);
begin
  {$ifdef windows}
  Inc(StandbyCounter, tmRefreshDownloadsInfo.Interval);
  // reset windows sleep counter every 50s, minimum windows sleep is 1 minute
  if StandbyCounter > 50000 then
  begin
    SetThreadExecutionState(ES_SYSTEM_REQUIRED);
    StandbyCounter := 0;
  end;
  {$endif}
  TransferRateGraphAddItem(DLManager.TransferRate);
  vtDownload.Repaint;
end;

procedure DumpLoadedModules;
var
  i: Integer;
  s: string;
begin
  s:=#10;
  for i:=0 to Modules.Count-1 do
    if Modules.Count<>0 then
      s+=Modules[i].ID+' '+Modules[i].Name+#10;
  Logger.Send('loaded modules: '+IntToStr(Modules.Count),s);
end;

procedure TMainForm.tmStartupTimer(Sender: TObject);
begin
  try
    if Sender=tmStartup then
      FreeAndNil(tmStartup);

    //load lua modules
    ScanLuaWebsiteModulesFile;
    AddToAboutStatus('Modules', IntToStr(Modules.Count));
    if AppParams.IndexOf('--dump-loaded-modules') <> -1 then
      DumpLoadedModules;

    Modules.LoadFromFile;
    WebsiteOptionCustomForm.CreateWebsiteOption;
    WebsiteSettingsForm.LoadWebsiteSettings;
    AccountManagerForm.LoadAccounts;

    //restore everything after all modules loaded
    DLManager.Restore;
    FavoriteManager.Restore;

    //load settings_file
    LoadMangaOptions;
    LoadOptions;
    ApplyOptions;
  except
    on E: Exception do
      Logger.SendException('tmStartup Error!', E);
  end;


  if OptionAutoCheckLatestVersion then
  begin
    btCheckLatestVersionClick(btCheckLatestVersion);
    LuaModulesUpdaterForm.btCheckUpdateClick(LuaModulesUpdaterForm.btCheckUpdate);
  end;
  if OptionAutoCheckFavStartup then
  begin
    FavoriteManager.isAuto := True;
    FavoriteManager.CheckForNewChapter;
  end;
  DLManager.CheckAndActiveTaskAtStartup;

  DoBackupToday;

  isStartup := False;
  // effective after startup
  UpdateVtDownload;
  UpdateVtFavorites;

  if cbSelectManga.ItemIndex <> -1 then
    OpenDataDB(TModuleContainer(currentWebsite).ID);
end;

procedure TMainForm.medURLCutClick(Sender: TObject);
begin
  edURL.CutToClipboard;
end;

procedure TMainForm.medURLCopyClick(Sender: TObject);
begin
  edURL.CopyToClipboard;
end;

procedure TMainForm.medURLPasteClick(Sender: TObject);
begin
  edURL.PasteFromClipboard;
end;

procedure TMainForm.medURLPasteandgoClick(Sender: TObject);
begin
  edURL.Clear;
  edURL.PasteFromClipboard;
  edURLButtonClick(edURL);
end;

procedure TMainForm.medtURLDeleteClick(Sender: TObject);
begin
  edURL.ClearSelection;
end;

procedure TMainForm.medURLSelectAllClick(Sender: TObject);
begin
  edURL.SelectAll;
  edURL.SetFocus;
end;

procedure TMainForm.medURLUndoClick(Sender: TObject);
begin
  edURL.Undo;
end;

procedure TMainForm.miAbortSilentThreadClick(Sender: TObject);
begin
  if Assigned(SilentThreadManager) then
    SilentThreadManager.StopAll(False);
end;

procedure TMainForm.miChapterListFilterClick(Sender: TObject);
begin
  edFilterMangaInfoChapters.Visible := miChapterListFilter.Checked;
  if edFilterMangaInfoChapters.Visible then
  begin
    clbChapterList.AnchorSide[akTop].Control := edFilterMangaInfoChapters;
    clbChapterList.AnchorSide[akTop].Side := asrBottom;
    edFilterMangaInfoChapters.SetFocus;
  end
  else
  begin
    edFilterMangaInfoChapters.Clear;
    clbChapterList.AnchorSide[akTop].Control := nil;
    clbChapterList.AnchorSide[akTop].Side := asrTop;
    clbChapterList.Top := 0;
  end;
end;

procedure TMainForm.miChapterListHideDownloadedClick(Sender: TObject);
begin
  if Sender = miChapterListHideDownloaded then
  begin
    miChapterListHideDownloaded.Checked := not miChapterListHideDownloaded.Checked;
    settingsfile.WriteBool('general', 'ChapterListHideDownloaded', miChapterListHideDownloaded.Checked);
  end;

  FilterChapterList(edFilterMangaInfoChapters.Text, miChapterListHideDownloaded.Checked);
end;

procedure TMainForm.miDownloadEnableClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if vtDownload.SelectedCount = 0 then Exit;
  DLManager.Lock;
  try
    Node := vtDownload.GetFirstSelected();
    while Assigned(Node) do
    begin
      if Sender = miDownloadEnable then
        DLManager.EnableTask(Node^.Index)
      else
        DLManager.DisableTask(Node^.Index);
      Node := vtDownload.GetNextSelected(Node);
    end;
  finally
    DLManager.UnLock;
  end;
  UpdateVtDownload;
end;

procedure TMainForm.miDownloadViewMangaInfoClick(Sender: TObject);
begin
  if Assigned(vtDownload.FocusedNode) then
    with DLManager.Items[vtDownload.FocusedNode^.Index].DownloadInfo do
      ViewMangaInfo(Module, Link, Title, SaveTo, miDownloadViewMangaInfo);
end;

procedure TMainForm.miChapterListHighlightClick(Sender: TObject);
begin
  if Sender = miChapterListHighlight then
  begin
    miChapterListHighlight.Checked := not miChapterListHighlight.Checked;
    settingsfile.WriteBool('general', 'HighlightDownloadedChapters', miChapterListHighlight.Checked);
  end;
  if Length(ChapterList) = 0 then Exit;
  if miChapterListHighlight.Checked then
    DLManager.GetDownloadedChaptersState(mangaInfo.ModuleID, mangaInfo.Link,
      ChapterList)
  else
    ClearChapterListState;
  clbChapterList.Repaint;
end;

procedure TMainForm.miDownloadDeleteTaskClick(Sender: TObject);
var
  xNode: PVirtualNode;
  deletedxNodes: array of PVirtualNode;
  i, x: Integer;
  f, d, s: String;
begin
  if vtDownload.SelectedCount = 0 then
  begin
    Exit;
  end;
  if DLManager.Count = 0 then
  begin
    Exit;
  end;
  if cbOptionShowDeleteTaskDialog.Checked then
  begin
    if MessageDlg('', RS_DlgRemoveTask, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      Exit;
    end;
  end;

  vtDownload.BeginUpdate;
  DLManager.Lock;
  try
    // stop selected nodes
    xNode := vtDownload.GetPreviousSelected(nil);
    while Assigned(xNode) do
    begin
      with DLManager.Items[xNode^.Index] do
        if Running then
        begin
          TaskThread.IsForDelete := True;
          TaskThread.Terminate;
        end;
      xNode := vtDownload.GetPreviousSelected(xNode);
    end;

    // cleaning the data
    Setlength(deletedxNodes, vtDownload.SelectedCount);
    x := 0;
    xNode := vtDownload.GetPreviousSelected(nil);
    while Assigned(xNode) do
    begin
      Exclude(xNode^.States, vsSelected);
      with DLManager.Items[xNode^.Index] do
      begin
        if Running then
        begin
          TaskThread.WaitFor;
        end;

        if ((Sender = miDownloadDeleteTaskData) or (Sender = miDownloadDeleteTaskDataFavorite))
          and (ChapterNames.Count > 0) then
        begin
          d := CorrectPathSys(DownloadInfo.SaveTo);
          for i := 0 to ChapterNames.Count - 1 do
          begin
            f := CorrectPathSys(d + ChapterNames[i]);
            if DirectoryExistsUTF8(f) then
            begin
              DeleteDirectory(f, False);
            end;
            f := RemovePathDelim(f);
            for s in FMDSupportedPackedOutputExt do
            begin
              if FileExistsUTF8(f + s) then
              begin
                DeleteFileUTF8(f + s);
              end;
            end;
          end;
          RemoveDirUTF8(d);
        end;

        if (Sender = miDownloadDeleteTaskDataFavorite) and
          (FavoriteManager.Items.Count <> 0) and
          (FavoriteManager.isRunning = False) then
        begin
          try
            FavoriteManager.Lock;
            for i := 0 to FavoriteManager.Count - 1 do
            begin
              if SameText(DLManager[xNode^.Index].DownloadInfo.Link, FavoriteManager[i].FavoriteInfo.Link)
                and SameText(DLManager[xNode^.Index].DownloadInfo.ModuleID, FavoriteManager[i].FavoriteInfo.ModuleID) then
                begin
                  FavoriteManager.Delete(i);
                  Break;
                end;
            end;
          finally
            FavoriteManager.UnLock;
          end;
        end;

        deletedxNodes[x] := xNode;
        Inc(x);
        DLManager.Delete(xNode^.Index);
      end;
      xNode := vtDownload.GetPreviousSelected(xNode);
    end;
  finally
    DLManager.UnLock;
  end;

  try
    for x := 0 to High(deletedxNodes) do
    begin
      vtDownload.DeleteNode(deletedxNodes[x])
    end;

    if edDownloadsSearch.Text <> '' then
    begin
      xNode := vtDownload.GetFirst();
      while Assigned(xNode) do
      begin
        vtDownload.IsFiltered[xNode] := Pos(UpperCase(edDownloadsSearch.Text), UpperCase(DLManager.Items[xNode^.Index].DownloadInfo.Title)) = 0;
        xNode := vtDownload.GetNext(xNode);
      end;
    end;
  finally
  end;

  vtDownload.RootNodeCount := DLManager.Items.Count;
  vtDownload.EndUpdate;
  UpdateVtFavorites;
  UpdateVtDownload;
  DLManager.CheckAndActiveTask();
  Exit;
end;

procedure TMainForm.miDownloadMergeCompletedClick(Sender: TObject);
var
  i, j: Integer;
  ic, jc: TTaskContainer;
  // merge all finished tasks that have same manga name, website and directory
begin
  DLManager.Lock;
  try
    i:=DLManager.Count-1;
    while i>0 do begin
      ic:=DLManager.Items[i];
      if ic.Status=STATUS_FINISH then
      begin
        j:=i-1;
        while j>0 do begin
          jc:=DLManager.Items[j];
          if (i<>j) and
            (jc.Status = STATUS_FINISH) and
            SameText(ic.DownloadInfo.title,jc.DownloadInfo.title) and
            SameText(ic.DownloadInfo.ModuleID,jc.DownloadInfo.ModuleID) and
            SameText(ic.DownloadInfo.saveTo,jc.DownloadInfo.saveTo) then
          begin
            ic.ChapterLinks.Text:=jc.ChapterLinks.Text+ic.ChapterLinks.Text;
            ic.ChapterNames.Text:=jc.ChapterNames.Text+ic.ChapterNames.Text;
            ic.DownloadInfo.DateAdded:=jc.DownloadInfo.DateAdded;
            DLManager.Delete(j);
            Dec(i);
          end;
          Dec(j);
        end;
      end;
      Dec(i);
    end;
  finally
    DLManager.UnLock;
  end;
  UpdateVtDownload;
end;

procedure TMainForm.miFavoritesCheckNewChapterClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount > 0 then
  begin
    xNode := vtFavorites.GetFirstSelected;
    repeat
      if Assigned(xNode) then
      begin
        FavoriteManager.CheckForNewChapter(xNode^.Index);
        xNode := vtFavorites.GetNextSelected(xNode);
      end;
    until xNode = nil;
    vtFavorites.Repaint;
  end;
end;

procedure TMainForm.miFavoritesDownloadAllClick(Sender: TObject);
var
  i: Integer;
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  SilentThreadManager.BeginAdd;
  try
    xNode := vtFavorites.GetFirstSelected;
    for i := 0 to vtFavorites.SelectedCount - 1 do
    begin
      if vtFavorites.Selected[xNode] then
        with FavoriteManager.Items[xNode^.Index].FavoriteInfo do
          SilentThreadManager.Add(MD_DownloadAll, TModuleContainer(Module), Title, Link, SaveTo);
      xNode := vtFavorites.GetNextSelected(xNode);
    end;
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
  SilentThreadManager.EndAdd;
end;

procedure TMainForm.miFavoritesStopCheckNewChapterClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  xNode := vtFavorites.GetFirstSelected;
  while Assigned(xNode) do
  begin
    FavoriteManager.StopChekForNewChapter(False, xNode^.Index);
    xNode := vtFavorites.GetNextSelected(xNode);
  end;
  UpdateVtFavorites;
end;

procedure TMainForm.miFavoritesViewInfosClick(Sender: TObject);
begin
  if Assigned(vtFavorites.FocusedNode) then
    with FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo do
      ViewMangaInfo(Module, Link, Title, SaveTo, miFavoritesViewInfos);
end;

procedure TMainForm.miHighlightNewMangaClick(Sender: TObject);
begin
  miHighlightNewManga.Checked := not miHighlightNewManga.Checked;
  settingsfile.WriteBool('general', 'HighLightNewManga', miHighlightNewManga.Checked);
  vtMangaList.Repaint;
end;

procedure TMainForm.LoadAbout;
var
  fs: TFileStream;
  fp: TFontParams;
  s: String;
  _OpenSSSL_version: function(t: integer): PAnsiChar; cdecl;
begin
  // load readme.rtf
  if FileExistsUTF8(README_FILE) then begin
    fs := TFileStream.Create(README_FILE, fmOpenRead or fmShareDenyNone);
    try
      rmAbout.LoadRichText(fs);
    finally
      fs.free;
    end;
  end;

  if IsDarkModeEnabled then
  begin
    rmAbout.GetTextAttributes(0, fp);
    fp.Color := RGBToColor(245, 245, 245);
    rmAbout.SetTextAttributes(0, -1, fp);
  end;


  // load changelog.txt
  if FileExistsUTF8(CHANGELOG_FILE) then mmChangelog.Lines.LoadFromFile(CHANGELOG_FILE);

  // compiler info
  AddToAboutStatus('FPC Version', GetFPCVersion);
  AddToAboutStatus('LCL Version', GetLCLVersion);
  AddToAboutStatus('WidgetSet', GetWidgetSetName);
  AddToAboutStatus('Target CPU-OS', GetTargetCPU_OS);
  AddToAboutStatus('Build Time', GetBuildTime);
  if SQLiteLibraryHandle = 0 then InitializeSqlite();
  if SQLiteLibraryHandle <> 0 then try AddToAboutStatus('SQLite Version', sqlite3_version()); except end;

  if IsSSLloaded then
  begin
    s := SSLeayversion(0);
    if s = '' then
    begin
      Pointer(_OpenSSSL_version) := GetProcAddress(SSLUtilHandle, 'OpenSSL_version');
      if _OpenSSSL_version <> nil then
      begin
        s := PAnsiChar(_OpenSSSL_version(0));
        _OpenSSSL_version := nil;
      end;
    end;
    AddToAboutStatus('OpenSSL Version', s);
  end;

  if WebPLibHandle = 0 then InitWebPModule;
  if WebPLibHandle <> 0 then try AddToAboutStatus('WebP Version', WebPGetVersion); except end;
  AddToAboutStatus('PCRE Version', pcre2.Version);
end;

procedure TMainForm.AddToAboutStatus(const ACaption, AValue: String;
  APanel: TPanel);

  function addaboutcomplbl(const ACaption: String): TLabel;
  begin
    Result := TLabel.Create(Self);
    if APanel = nil then
      Result.Parent := pnAboutComp
    else
      Result.Parent := APanel;
    Result.Caption := ACaption;
  end;

begin
  addaboutcomplbl(ACaption + ':');
  with addaboutcomplbl(AValue) do
  begin
    Font.Style := [fsBold];
    BorderSpacing.Right := 16;
  end;
end;

procedure TMainForm.DoExternalClose(Sender: TObject);
begin
  Self.Close;
end;

procedure TMainForm.DoRestartFMD;
var
  p: TProcess;
begin
  p := TProcess.Create(nil);
  try
    p.InheritHandles := False;
    p.CurrentDirectory := FMD_DIRECTORY;
    p.Executable := Application.ExeName;
    p.Options := [];
    p.Parameters.AddStrings(AppParams);
    {$ifdef windows}
    p.Parameters.Add('--dorestart-pid=' + IntToStr(GetProcessID));
    {$ifend}
    p.Execute;
  finally
    p.Free;
  end;
end;

procedure TMainForm.ExternalClose;
var
  modalcount, i: Integer;
begin
  modalcount:=0;
  for i:=Screen.FormCount-1 downto 0 do
    if (Screen.Forms[i] <> Application.MainForm) and
      (fsModal in Screen.Forms[i].FormState) then
    begin
      Screen.Forms[i].ModalResult := mrCancel;
      Inc(modalcount);
    end;
  if modalcount=0 then
    Self.Close
  else
    with TTimer.Create(Self) do
    begin
      OnTimer := @DoExternalClose;
      Interval := 1000;
      Enabled := True;
    end;
end;

procedure TMainForm.RestartFMD;
begin
  OptionRestartFMD := True;
  ExternalClose;
end;

procedure TMainForm.GeneratetvDownloadFilterNodes;

  function Add(const ParentNode: TTreeNode; const S: String;
    const ImgIdx: Integer = -1): TTreeNode;
  begin
    if Assigned(ParentNode) then
      Result := tvDownloadFilter.Items.AddChild(ParentNode, S)
    else
      Result := tvDownloadFilter.Items.Add(nil, S);
    with Result do
    begin
      ImageIndex := ImgIdx;
      SelectedIndex := ImgIdx;
      StateIndex := ImgIdx;
    end;
  end;

var
  Node: TTreeNode;

begin
  with tvDownloadFilter do begin
    Items.Clear;

    // download
    Node := Add(nil, RS_AllDownloads, 4);
    Add(Node, RS_Finish, 5);
    Add(Node, RS_InProgress, 6);
    Add(Node, RS_Stopped, 7);
    Add(Node, RS_Failed, 16);
    Add(Node, RS_Disabled, 22);

    // history
    Node := Add(nil, RS_History, 4);
    Add(Node, RS_Today, 8);
    Add(Node, RS_Yesterday, 8);
    Add(Node, RS_Last7days, 8);
    Add(Node, RS_ThisMonth, 8);
    Add(Node, RS_Last6months, 8);
    Add(Node, RS_OlderThan6months, 8);
    Add(Node, RS_Custom, 8);
  end;
end;

procedure TMainForm.btDownloadClick(Sender: TObject);
var
  links, names: TStrings;
  node: PVirtualNode;
  s, sRename: String;
  c, p, r, i, j, k, l: Integer;
begin
  if clbChapterList.CheckedCount = 0 then
  begin
    Exit;
  end;

  links := TStringList.Create;
  names := TStringList.Create;
  try
    node:=clbChapterList.GetFirstChecked();
    while Assigned(node) do
    begin
      if (vsVisible in node^.States) then
      begin
        links.Add(ChapterList[node^.Index].Link);
        s := CustomRename(OptionChapterCustomRename,
          mangaInfo.Website,
          mangaInfo.Title,
          mangaInfo.Authors,
          mangaInfo.Artists,
          ChapterList[node^.Index].Title,
          Format('%.4d',[ChapterList[node^.Index].Index]),
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);
        names.Add(s);
        ChapterList[node^.Index].Downloaded := True;
        clbChapterList.ReinitNode(node, False);
      end;
      node := clbChapterList.GetNextChecked(node);
    end;
    clbChapterList.Repaint;
    if links.Count <> 0 then
    begin
      // save to
      if edSaveTo.Text = '' then
      begin
        FillSaveTo;
        OverrideSaveTo(Modules.LocateModule(mangaInfo.ModuleID));
      end;
      s := edSaveTo.Text;
      if OptionGenerateMangaFolder then
      begin
        sRename := CustomRename(
            OptionMangaCustomRename,
            mangaInfo.Website,
            mangaInfo.Title,
            mangaInfo.Authors,
            mangaInfo.Artists,
            '',
            '',
            OptionChangeUnicodeCharacter,
            OptionChangeUnicodeCharacterStr);

        if Pos(sRename, s) = 0 then
        begin
          s := AppendPathDelim(s) + sRename;
        end;
      end;

      s := ReplaceRegExpr('\.*$', s, '', False);
      c := 1;
      p := links.Count;
      r := 0;
      if btDownload.Tag >= links.Count then
      begin
        c := links.Count;
        p := 1;
      end
      else
      if btDownload.Tag > 1 then
      begin
        c := btDownload.Tag;
        p := links.Count div c;
        r := links.Count mod c;
      end;
      btDownload.Tag:=0;
      k := 0;
      for i := 1 to c do
      begin
        if i <= r then
        begin
          l := p + 1
        end
        else
        if i = c then
        begin
          l := links.Count - k
        end
        else
        begin
          l := p;
        end;
        DLManager.Lock;
        try
          with DLManager.AddTask do
          begin
            for j := 1 to l do
            begin
              ChapterLinks.Add(links[k]);
              ChapterNames.Add(names[k]);
              Inc(k);
            end;
            if cbAddAsStopped.Checked then
            begin
              DownloadInfo.Status := Format('[%d/%d] %s', [0, ChapterLinks.Count, RS_Stopped]);
              Status := STATUS_STOP;
            end
            else
            begin
              DownloadInfo.Status := Format('[%d/%d] %s', [0, ChapterLinks.Count, RS_Waiting]);
              Status := STATUS_WAIT;
            end;
            DownloadInfo.ModuleID := mangaInfo.ModuleID;
            DownloadInfo.Link := mangaInfo.Link;
            DownloadInfo.Title := mangaInfo.Title;
            DownloadInfo.DateAdded := Now;
            DownloadInfo.DateLastDownloaded := Now;
            DownloadInfo.SaveTo := s;
            CurrentDownloadChapterPtr := 0;
            DBInsert;
          end;
        finally
          DLManager.UnLock;
        end;
        if OptionSortDownloadsOnNewTasks then
        begin
          DLManager.Sort(DLManager.SortColumn);
        end;
      end;

      DLManager.DownloadedChapters.Chapters[mangaInfo.ModuleID, mangaInfo.Link] := links.Text;
      FavoriteManager.AddToDownloadedChaptersList(mangaInfo.ModuleID, mangaInfo.Link, links);
      DLManager.CheckAndActiveTask;
      if OptionShowDownloadsTabOnNewTasks then
      begin
        pcMain.ActivePage := tsDownload;
      end;
      UpdateVtDownload;
    end;
  finally
    links.Free;
    names.Free;
  end;
end;

procedure TMainForm.btAddToFavoritesClick(Sender: TObject);
var
  s, sRename: String;
begin
  if mangaInfo.Title <> '' then
  begin
    // save to
    if edSaveTo.Text = '' then
    begin
      FillSaveTo;
      OverrideSaveTo(Modules.LocateModule(mangaInfo.ModuleID));
    end;
    s := edSaveTo.Text;
    if OptionGenerateMangaFolder then
    begin
      sRename := CustomRename(
          OptionMangaCustomRename,
          mangaInfo.Website,
          mangaInfo.Title,
          mangaInfo.Authors,
          mangaInfo.Artists,
          '',
          '',
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);

      if Pos(sRename, s) = 0 then
      begin
        s := AppendPathDelim(s) + sRename;
      end;
    end;

    FavoriteManager.Add(
      mangaInfo.Module,
      mangaInfo.Title,
      MangaInfo.Status,
      IntToStr(mangaInfo.NumChapter),
      mangaInfo.ChapterLinks.Text,
      s,
      mangaInfo.Link);
    UpdateVtFavorites;
    vtFavoritesFilterCountChange;
    btAddToFavorites.Enabled := False;
    if OptionShowFavoritesTabOnNewManga then
    begin
      edFavoritesSearch.Text := mangaInfo.Title;
      pcMain.ActivePage := tsFavorites;
    end;
  end;
end;

procedure TMainForm.btAbortCheckLatestVersionClick(Sender: TObject);
begin
  if Assigned(CheckUpdateThread) then
    CheckUpdateThread.Terminate;
end;

procedure TMainForm.btCancelFavoritesCheckClick(Sender: TObject);
begin
  FavoriteManager.StopChekForNewChapter(False);
end;

procedure TMainForm.appPropertiesMainShowHint(var HintStr: String;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if HintInfo.HintControl = vtMangaList then
  begin
    HintInfo.HintMaxWidth := 500;
    HintInfo.HideTimeout := 300000;
  end;
end;

// -----

procedure TMainForm.btFavoritesCheckNewChapterClick(Sender: TObject);
begin
  FavoriteManager.isAuto := False;
  FavoriteManager.CheckForNewChapter;
end;

// -----

// -----

procedure TMainForm.btUpdateListClick(Sender: TObject);
var
  button: TControl;
  lowerLeft: TPoint;
begin
  pmUpdate.Items[0].Enabled := True;
  pmUpdate.Items[3].Enabled := True;
  if Sender is TControl then
  begin
    button := TControl(Sender);
    lowerLeft := Point(button.Left, button.Top + button.Height * 2 +
      (button.Height div 2));
    lowerLeft := ClientToScreen(lowerLeft);
    pmUpdate.Popup(lowerLeft.X, lowerLeft.Y);
  end;
end;

procedure TMainForm.DisableAddToFavorites(const AModule: Pointer);
begin
  btAddToFavorites.Enabled := Assigned(AModule) and TModuleContainer(AModule).FavoriteAvailable;
end;

procedure TMainForm.FMDInstanceReceiveMsg(Sender: TObject);
begin
  MessageDlg(Application.Title, RS_FMDAlreadyRunning, mtWarning, [mbOK], 0);
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Show;
  BringToFront;
end;

procedure TMainForm.ClearChapterListState;
var
  i: Integer;
begin
  if Length(ChapterList) > 0 then
    for i := Low(ChapterList) to High(ChapterList) do
      ChapterList[i].Downloaded := False;
end;

procedure TMainForm.OnTimer1Hour(Sender: TObject);
begin
  DoBackupToday;
end;

procedure TMainForm.OnTimerBackup(Sender: TObject);
begin
  // todo: optimize backup
  DLManager.Backup;
  FavoriteManager.Backup;
end;

procedure TMainForm.EmbedForm(const AForm: TForm; const AParent: TWinControl);
begin
  with AForm do
  begin
    Parent := AParent;
    BorderStyle := bsNone;
    Align := alClient;
    Show;
    if Screen.PixelsPerInch > 96 then
      AutoAdjustLayout(lapAutoAdjustForDPI, Screen.PixelsPerInch, 96, 0, 0);
  end;
end;

procedure TMainForm.btVisitMyBlogClick(Sender: TObject);
begin
  OpenURL('http://akarink.wordpress.com/');
end;

procedure TMainForm.cbAddAsStoppedChange(Sender: TObject);
begin
  settingsfile.WriteBool('general', 'AddAsStopped', cbAddAsStopped.Checked);
end;

procedure TMainForm.cbOptionAutoCheckFavIntervalChange(Sender: TObject);
begin
  seOptionAutoCheckFavIntervalMinutes.Enabled := cbOptionAutoCheckFavInterval.Checked;
  lbOptionAutoCheckFavIntervalMinutes.Enabled := cbOptionAutoCheckFavInterval.Checked;
end;

procedure TMainForm.cbOptionAutoCheckFavStartupChange(Sender: TObject);
begin
  cbOptionAutoOpenFavStartup.Enabled := cbOptionAutoCheckFavStartup.Checked;
end;

procedure TMainForm.cbOptionChangeUnicodeCharacterChange(Sender: TObject);
begin
  edOptionChangeUnicodeCharacterStr.Enabled := cbOptionChangeUnicodeCharacter.Checked;
end;

procedure TMainForm.cbOptionDigitChapterChange(Sender: TObject);
begin
  seOptionDigitChapter.Enabled := cbOptionDigitChapter.Checked;
end;

procedure TMainForm.cbOptionDigitVolumeChange(Sender: TObject);
begin
  seOptionDigitVolume.Enabled := cbOptionDigitVolume.Checked;
end;

procedure TMainForm.cbOptionGenerateMangaFolderChange(Sender: TObject);
begin
  edOptionMangaCustomRename.Enabled := cbOptionGenerateMangaFolder.Checked;
  lbOptionMangaCustomRename.Enabled := edOptionMangaCustomRename.Enabled;
  lbOptionMangaCustomRenameHint.Enabled := edOptionMangaCustomRename.Enabled;
end;

procedure TMainForm.cbSelectMangaEditingDone(Sender: TObject);
begin
  if cbSelectManga.ItemIndex < 0 then Exit;
  if currentWebsite <> Pointer(cbSelectManga.Items.Objects[cbSelectManga.ItemIndex]) then
  begin
    currentWebsite := cbSelectManga.Items.Objects[cbSelectManga.ItemIndex];
    settingsfile.WriteString('form', 'SelectManga', TModuleContainer(currentWebsite).ID);
    vtMangaList.Clear;
    if dataProcess = nil then
      dataProcess := TDBDataProcess.Create
    else
    if dataProcess.Connected then
      dataProcess.Close;
    lbMode.Caption := Format(RS_ModeAll, [0]);
    if DBDataFileExist(TModuleContainer(currentWebsite).ID) then
      OpenDataDB(TModuleContainer(currentWebsite).ID)
    else
    if cbOptionShowDownloadMangalistDialog.Checked then
      mnUpdateDownFromServerClick(mnUpdateDownFromServer);
  end;
end;

procedure TMainForm.cbSelectMangaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not (Key in [VK_RETURN, VK_TAB]) then
    cbSelectManga.DroppedDown:=True;
end;

procedure TMainForm.cbSelectMangaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbMiddle then
    cbSelectMangaEditingDone(Sender);
end;

procedure TMainForm.btReadOnlineClick(Sender: TObject);
begin
  OpenURL(mangaInfo.URL);
end;

procedure TMainForm.btMangaListSearchClearClick(Sender: TObject);
begin
  edMangaListSearch.Tag := 1;
  edMangaListSearch.Clear;
end;

procedure TMainForm.btCheckLatestVersionClick(Sender: TObject);
begin
  if Assigned(CheckUpdateThread) or Assigned(SelfUpdaterThread) then
    MessageDlg('', RS_DlgUpdaterIsRunning, mtInformation, [mbYes], 0)
  else
    CheckUpdateThread := TCheckUpdateThread.Create;
end;

procedure TMainForm.btClearLogFileClick(Sender: TObject);
var
  F: TextFile;
begin
  if FileExistsUTF8(edLogFileName.Text) then
  begin
    system.Assign(F, edLogFileName.Text);
    Rewrite(F);
    CloseFile(F);
  end;
end;

procedure TMainForm.btDonateClick(Sender: TObject);
begin
  OpenURL('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=akarin.km@gmail.com&item_name=Donation+to+Free+Manga+Downloader');
end;

procedure TMainForm.btDownloadSplitClick(Sender: TObject);
var
  s:String='';
  c:Integer=-1;
begin
  if InputQuery(RS_DlgSplitDownload,RS_DlgDownloadCount,s) and (s<>'') then
  begin
    c:=StrToIntDef(s,-1);
    if c<=0 then
      MessageDlg(RS_WrongInput,mtError,[mbOK],0)
    else
    begin
      btDownload.Tag:=c;
      btDownloadClick(btDownload);
    end;
  end;
end;

procedure TMainForm.btFavoritesImportClick(Sender: TObject);
begin
  with TImportFavorites.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.btnDownloadFilterCustomDateApplyClick(Sender: TObject);
begin
  vtDownload.BeginUpdate;
  try
    FilterDownloadByCustomDate;
  finally
    vtDownload.EndUpdate;
  end;
end;

procedure TMainForm.btOpenLogClick(Sender: TObject);
begin
  FormLogger.Show;
end;

procedure TMainForm.btChecksClick(Sender: TObject);
begin
  if Sender is TControl then
    with TControl(Sender) do begin
      pmChapterList.Alignment := Menus.paRight;
      pmChapterList.PopUp(ControlOrigin.x, ControlOrigin.y);
      pmChapterList.Alignment := Menus.paLeft;
    end;
  clbChapterList.SetFocus;
end;

procedure TMainForm.clbChapterListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if CellPaintMode <> cpmPaint then Exit;
  if Node^.Index>=Length(ChapterList) then Exit;
  if ChapterList[Node^.Index].Downloaded then
  begin
    TargetCanvas.Brush.Color:=CL_CHDownloaded;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TMainForm.clbChapterListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Node^.Index>=Length(ChapterList) then Exit;
  if Length(ChapterList)=1 then
    CellText:=ChapterList[Node^.Index].Title
  else
    CellText:=Format('%.4d - %s',[ChapterList[Node^.Index].Index, ChapterList[Node^.Index].Title]);
end;

procedure TMainForm.clbChapterListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if Assigned(Node) then Node^.CheckType:=ctCheckBox;
end;

procedure TMainForm.clbChapterListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  if (Key = VK_SPACE) and (clbChapterList.SelectedCount > 0) then
  begin
    xNode := clbChapterList.GetFirstSelected;
    for i := 0 to clbChapterList.SelectedCount - 1 do
    begin
      if clbChapterList.Selected[xNode] then
        if xNode^.CheckState = csUncheckedNormal then
          xNode^.CheckState := csCheckedNormal
        else if xNode^.CheckState = csCheckedNormal then
          xNode^.CheckState := csUncheckedNormal;
      clbChapterList.InvalidateNode(xNode);
      xNode := clbChapterList.GetNextSelected(xNode);
    end;
    Key := VK_UNKNOWN;
  end;
end;

procedure TMainForm.FilterDownloadByCustomDate;
begin
  FilterDownloadsByJDN(DateToJDN(deDownloadFilterCustomDateFrom.Date), DateToJDN(deDownloadFilterCustomDateTo.Date));
end;

procedure TMainForm.edDownloadsSearchButtonClick(Sender: TObject);
begin
  edDownloadsSearch.Clear;
end;

procedure TMainForm.edDownloadsSearchChange(Sender: TObject);
var
  xNode: PVirtualNode;
  S: String;
begin
  if vtDownload.RootNodeCount = 0 then Exit;
  vtDownload.BeginUpdate;
  try
    xNode := vtDownload.GetFirst();
    if edDownloadsSearch.Text = '' then
      while Assigned(xNode) do
      begin
        vtDownload.IsFiltered[xNode] := False;
        xNode := vtDownload.GetNext(xNode);
      end
    else
    begin
      S := UpperCase(edDownloadsSearch.Text);
      while Assigned(xNode) do
      begin
        vtDownload.IsFiltered[xNode] := Pos(S, UpperCase(DLManager.Items[xNode^.Index].DownloadInfo.Title)) = 0;
        xNode := vtDownload.GetNext(xNode);
      end;
    end;
  finally
    vtDownload.EndUpdate;
  end;
end;

procedure TMainForm.edFavoritesSearchButtonClick(Sender: TObject);
begin
  edFavoritesSearch.Clear;
end;

procedure TMainForm.edFavoritesSearchChange(Sender: TObject);
var
  xNode: PVirtualNode;
  S: String;
begin
  if vtFavorites.RootNodeCount = 0 then Exit;
  vtFavorites.BeginUpdate;
  try
    xNode := vtFavorites.GetFirst();
    if edFavoritesSearch.Text = '' then
      while Assigned(xNode) do
      begin
        vtFavorites.IsFiltered[xNode] := False;
        xNode := vtFavorites.GetNext(xNode);
      end
    else
    begin
      S := UpperCase(edFavoritesSearch.Text);
      while Assigned(xNode) do
      begin
        vtFavorites.IsFiltered[xNode] := Pos(S, UpperCase(FavoriteManager.Items[xNode^.Index].FavoriteInfo.Title)) = 0;
        xNode := vtFavorites.GetNext(xNode);
      end;
    end;
  finally
    vtFavorites.EndUpdate;
  end;
end;

procedure TMainForm.edFilterMangaInfoChaptersButtonClick(Sender: TObject);
begin
  edFilterMangaInfoChapters.Clear;
end;

procedure TMainForm.edFilterMangaInfoChaptersChange(Sender: TObject);
begin
  FilterChapterList(edFilterMangaInfoChapters.Text, miChapterListHideDownloaded.Checked);
end;

procedure TMainForm.edLogFileNameButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      InitialDir := ExtractFileDir(ExpandFileName(edLogFileName.Text));
      if Execute then
        edLogFileName.Text := FileName;
    finally
      Free;
    end;
end;

procedure TMainForm.edURLKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    edURLButtonClick(edURL);
end;

procedure TMainForm.edWebsitesSearchButtonClick(Sender: TObject);
begin
  edWebsitesSearch.Clear;
end;

procedure TMainForm.edWebsitesSearchChange(Sender: TObject);
begin
  FilterVST(vtOptionMangaSiteSelection, edWebsitesSearch.Text);
end;

procedure TMainForm.HandleApplicationQueryEndSession(var Cancel: Boolean);
begin
  Cancel := False;
  ShutdownBlockReasonCreate(Handle, 'FMD: saving userdata');
end;

procedure TMainForm.HandleApplicationEndSession(Sender: TObject);
begin
  try
    END_SESSION := True;
    DoAfterFMD := DO_NOTHING;
    OptionRestartFMD := False;
    Close;
  finally
    ShutdownBlockReasonDestroy(Handle);
  end;
end;

procedure TMainForm.btRemoveFilterClick(Sender: TObject);
begin
  if dataProcess.Filtered then
  begin
    vtMangaList.Clear;
    Screen.Cursor := crHourGlass;
    try
      dataProcess.RemoveFilter;
      vtMangaList.RootNodeCount := dataProcess.RecordCount;
      lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
      edMangaListSearch.Tag := -1;
      edMangaListSearch.Clear;
    except
      on E: Exception do
        ExceptionHandler(Self, E);
    end;
    Screen.Cursor := crDefault;
  end;
end;

// -----

procedure TMainForm.btFilterClick(Sender: TObject);
var
  checkGenres,
  uncheckGenres: TStringList;
  i: Integer;
  s: String;
begin
  Screen.Cursor := crHourGlass;
  checkGenres := TStringList.Create;
  uncheckGenres := TStringList.Create;
  try
    edCustomGenres.Text := Trim(edCustomGenres.Text);
    if cbUseRegExpr.Checked and (edCustomGenres.Text <> '') then
      checkGenres.Add(edCustomGenres.Text)
    else
    begin
      ExtractStrings([','], [], PChar(edCustomGenres.Text), checkGenres);
      TrimStrings(checkGenres);
      i := 0;
      while i < checkGenres.Count do begin
        s := Trim(checkGenres.Strings[i]);
        if (s <> '') and (s[1] = '-') or (s[1] = '!') then begin
          Delete(s, 1, 1);
          uncheckGenres.Add(s);
          checkGenres.Delete(i);
        end
        else Inc(i);
      end;
    end;

    if pnGenres.ControlCount > 0 then
      for i := 0 to pnGenres.ControlCount - 1 do
        if pnGenres.Controls[i] is TCheckBox then begin
          if TCheckBox(pnGenres.Controls[i]).State = cbChecked then
            checkGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption)
          else
          if TCheckBox(pnGenres.Controls[i]).State = cbUnchecked then
            uncheckGenres.Add(TCheckBox(pnGenres.Controls[i]).Caption);
        end;

    if dataProcess.CanFilter(
      checkGenres,
      uncheckGenres,
      edFilterTitle.Text,
      edFilterAuthors.Text,
      edFilterArtists.Text,
      IntToStr(cbFilterStatus.ItemIndex),
      edFilterSummary.Text,
      OptionNewMangaTime,
      rbAll.Checked,
      cbOnlyNew.Checked) then
    begin
      dataProcess.FilterAllSites := cbSearchFromAllSites.Checked;
      if cbSearchFromAllSites.Checked then
        dataProcess.SitesList.Assign(cbSelectManga.Items);

      edMangaListSearch.Tag := -1;
      edMangaListSearch.Clear;
      vtMangaList.Clear;

      dataProcess.Filter(
        checkGenres,
        uncheckGenres,
        edFilterTitle.Text,
        edFilterAuthors.Text,
        edFilterArtists.Text,
        IntToStr(cbFilterStatus.ItemIndex),
        edFilterSummary.Text,
        OptionNewMangaTime,
        rbAll.Checked,
        cbOnlyNew.Checked,
        cbUseRegExpr.Checked);
    end;
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
  uncheckGenres.Free;
  checkGenres.Free;
  Screen.Cursor := crDefault;

  vtMangaList.RootNodeCount := dataProcess.RecordCount;
  if dataProcess.Filtered then
    lbMode.Caption := Format(RS_ModeFiltered, [vtMangaList.RootNodeCount])
  else
    lbMode.Caption := Format(RS_ModeAll, [vtMangaList.RootNodeCount])
end;

procedure TMainForm.btFilterResetClick(Sender: TObject);
var
  i: Cardinal;
begin
  for i := 0 to 37 do
    TCheckBox(pnGenres.Controls[i]).State := cbGrayed;
  edFilterTitle.Caption := '';
  edFilterAuthors.Caption := '';
  edFilterArtists.Caption := '';
  edFilterSummary.Caption := '';
  cbFilterStatus.ItemIndex := 4;
  edCustomGenres.Caption := '';
end;

// ----- vtMangaList popup menu -----

procedure TMainForm.miMangaListAddToFavoritesClick(Sender: TObject);
var
  xNode: PVirtualNode;
  data: PMangaInfoData;
begin
  if vtMangaList.SelectedCount = 0 then Exit;
  LastUserPickedSaveTo := '';
  FillSaveTo;
  SilentThreadManager.BeginAdd;
  try
    xNode := vtMangaList.GetFirstSelected;
    while Assigned(xNode) do
    begin
      data := vtMangaList.GetNodeData(xNode);
      SilentThreadManager.Add(MD_AddToFavorites, TModuleContainer(data^.Module), data^.Title, data^.Link);
      xNode := vtMangaList.GetNextSelected(xNode);
    end;
  finally
    SilentThreadManager.EndAdd;
  end;
end;

// ----- vtFavorites popup menu -----

procedure TMainForm.miFavoritesDeleteClick(Sender: TObject);
var
  xNode: PVirtualNode;
  deletedxNodes: array of PVirtualNode;
  x: Integer;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbOK], 0);
    Exit;
  end;
  if cbOptionShowDeleteTaskDialog.Checked then
  begin
    if MessageDlg('', RS_DlgRemoveFavorite, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      Exit;
    end;
  end;

  vtFavorites.BeginUpdate;
  Setlength(deletedxNodes, vtFavorites.SelectedCount);
  x := 0;
  FavoriteManager.Lock;
  try
    xNode := vtFavorites.GetLast();
    while Assigned(xNode) do begin
      if vtFavorites.Selected[xNode] then
      begin
        deletedxNodes[x] := xNode;
        Inc(x);
        FavoriteManager.Delete(xNode^.Index);
      end;
      xNode := vtFavorites.GetPreviousSelected(xNode);
    end;
  finally
    FavoriteManager.UnLock;
  end;

  try
    for x := 0 to High(deletedxNodes) do
    begin
      vtFavorites.DeleteNode(deletedxNodes[x])
    end;

    if edFavoritesSearch.Text <> '' then
    begin
      xNode := vtFavorites.GetFirst();
      while Assigned(xNode) do
      begin
        vtFavorites.IsFiltered[xNode] := Pos(UpperCase(edFavoritesSearch.Text), UpperCase(FavoriteManager.Items[xNode^.Index].FavoriteInfo.Title)) = 0;
        xNode := vtFavorites.GetNext(xNode);
      end;
    end;
  finally
  end;

  vtFavorites.EndUpdate;
  UpdateVtFavorites;
  vtFavoritesFilterCountChange;
end;

procedure TMainForm.miFavoritesChangeSaveToClick(Sender: TObject);
var
  s: String;
  s1: String;
  s2: String;
  Node: PVirtualNode;
  F: TFavoriteContainer;
begin
  if vtFavorites.SelectedCount = 0 then Exit;
  if FavoriteManager.isRunning then
  begin
    MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbYes, mbNo], 0);
    Exit;
  end;
  s := '';

  if (vtFavorites.SelectedCount = 1) and Assigned(vtFavorites.FocusedNode) then
  begin
    with TSelectDirectoryForm.Create(Self) do try
      dePath.Directory := FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo;
      if ShowModal = mrOK then
        s := dePath.Directory;
    finally
      Free;
    end;
    
    if s <> '' then
    begin
      FavoriteManager.Lock;
      try
        F:=FavoriteManager.Items[vtFavorites.FocusedNode^.Index];
        F.FavoriteInfo.SaveTo:=s;
        F.DBUpdateSaveTo;
      finally
        FavoriteManager.UnLock;
      end;
      UpdateVtFavorites;
      FavoriteManager.Backup;
    end;
  end
  else if (vtFavorites.SelectedCount > 1) then
  begin
    with TSelectDirectoryForm.Create(Self) do try
      dePath.Directory := '';
      if ShowModal = mrOK then
        s := dePath.Directory;
    finally
      Free;
    end;
    
    if s <> '' then
    begin
      FavoriteManager.Lock;
      try
        Node := vtFavorites.GetFirstSelected();
        while Assigned(Node) do
        begin
          F:=FavoriteManager.Items[Node^.Index];
          s1 := '';
          s2 := '';
          if (length(s) = 2) and (pos(':', s) > 0) then
          begin
            s1 := F.FavoriteInfo.SaveTo;
            s2 := s1;
            if pos(':', s1) > 0 then
            begin
              s1 := Copy(s1, 0, 2);
              F.FavoriteInfo.SaveTo := StringReplace(s2, s1, s, [rfIgnoreCase]);
            end;
          end
          else
          begin
            s1 := Copy(s, length(s), 1);
            s2 := RemoveSymbols(F.FavoriteInfo.Title);
            if s1 = '\' then
              F.FavoriteInfo.SaveTo := s + s2
            else
              F.FavoriteInfo.SaveTo := s + '\' + s2;
          end;
          F.DBUpdateSaveTo;
          Node := vtFavorites.GetNextSelected(Node);
        end;
      finally
        FavoriteManager.UnLock;
      end;
      UpdateVtFavorites;
      FavoriteManager.Backup;
    end;
  end;
end;

// ----- clbChapterList popup menu -----

procedure TMainForm.miChapterListCheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  if clbChapterList.SelectedCount > 0 then
  begin
    xNode := clbChapterList.GetFirstSelected;
    for i := 0 to clbChapterList.SelectedCount - 1 do
    begin
      if clbChapterList.Selected[xNode] then
        xNode^.CheckState := csCheckedNormal;
      clbChapterList.InvalidateNode(xNode);
      xNode := clbChapterList.GetNextSelected(xNode);
    end;
  end;
end;

procedure TMainForm.miChapterListUncheckSelectedClick(Sender: TObject);
var
  i: Cardinal;
  xNode: PVirtualNode;
begin
  if clbChapterList.SelectedCount > 0 then
  begin
    xNode := clbChapterList.GetFirstSelected;
    for i := 0 to clbChapterList.SelectedCount - 1 do
    begin
      if clbChapterList.Selected[xNode] then
        xNode^.CheckState := csUncheckedNormal;
      clbChapterList.InvalidateNode(xNode);
      xNode := clbChapterList.GetNextSelected(xNode);
    end;
  end;
end;

procedure TMainForm.miChapterListCheckAllClick(Sender: TObject);
var
  i: Cardinal;
  Node: PVirtualNode;
begin
  if clbChapterList.RootNodeCount > 0 then
  begin
    Node := clbChapterList.GetFirst;
    for i := 0 to clbChapterList.RootNodeCount - 1 do
    begin
      Node^.CheckState := csCheckedNormal;
      clbChapterList.InvalidateNode(Node);
      Node := clbChapterList.GetNext(Node);
    end;
  end;
end;

procedure TMainForm.miChapterListUncheckAllClick(Sender: TObject);
var
  i: Cardinal;
  Node: PVirtualNode;
begin
  if clbChapterList.RootNodeCount > 0 then
  begin
    Node := clbChapterList.GetFirst;
    for i := 0 to clbChapterList.RootNodeCount - 1 do
    begin
      Node^.CheckState := csUncheckedNormal;
      clbChapterList.InvalidateNode(Node);
      Node := clbChapterList.GetNext(Node);
    end;
  end;
end;

// ----- vtDownload popup menu -----

procedure TMainForm.mnDownload1ClickClick(Sender: TObject);
begin
  if cbSelectManga.Items.Count = 0 then Exit;
  if DBUpdaterThread <> nil then
    DBUpdaterThread.Add(cbSelectManga.Items)
  else
  if MessageDlg('', RS_DlgUpdaterWantToUpdateDB, mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    DBUpdaterThread := TDBUpdaterThread.Create;
    DBUpdaterThread.Items.Assign(cbSelectManga.Items);
    DBUpdaterThread.Start;
  end;
end;

procedure TMainForm.mnFilterGenreAllCheckClick(Sender: TObject);
begin
  FilterGenreChangeAllState(cbChecked);
end;

procedure TMainForm.mnFilterGenreAllIndeterminateClick(Sender: TObject);
begin
  FilterGenreChangeAllState(cbGrayed);
end;

procedure TMainForm.mnFilterGenreAllUncheckClick(Sender: TObject);
begin
  FilterGenreChangeAllState(cbUnchecked);
end;

procedure TMainForm.mnUpdate1ClickClick(Sender: TObject);
var
  m: TModuleContainer;
  i, j, c_list_count, in_list_count: Integer;
  in_list: Boolean;
begin
  if cbSelectManga.Items.Count = 0 then Exit;
  if (not isUpdating) then
  begin
    isUpdating := True;
    updateList := TUpdateListManagerThread.Create;
    for i := 0 to cbSelectManga.Items.Count-1 do
    begin
      m := TModuleContainer(cbSelectManga.Items.Objects[i]);
      updateList.websites.AddObject(m.Name, m);
    end;
    updateList.Start;
  end
  else
  begin
    // look if it's already in the update list
    in_list_count := 0;
    c_list_count := updateList.websites.Count-1;
    for i := 0 to cbSelectManga.Items.Count-1 do
    begin
      in_list := False;
      m := TModuleContainer(cbSelectManga.Items.Objects[i]);
      for j := 0 to c_list_count do
        if m = updateList.websites.Objects[j] then
        begin
          in_list := True;
          Inc(in_list_count);
          Break;
        end;
      if in_list = False then
        updateList.websites.AddObject(m.Name, m);
    end;
    if (in_list_count <> 0) and (in_list_count = cbSelectManga.Items.Count) then
      MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbYes], 0);
  end;
end;

procedure TMainForm.mnUpdateDownFromServerClick(Sender: TObject);
begin
  if cbSelectManga.ItemIndex < 0 then Exit;
  if DBUpdaterThread <> nil then
    DBUpdaterThread.Add(TModuleContainer(cbSelectManga.Items.Objects[cbSelectManga.ItemIndex]))
  else
  if MessageDlg('', RS_DlgUpdaterWantToUpdateDB, mtInformation, [mbYes, mbNo], 0) = mrYes then
  begin
    DBUpdaterThread := TDBUpdaterThread.Create;
    DBUpdaterThread.Add(TModuleContainer(cbSelectManga.Items.Objects[cbSelectManga.ItemIndex]));
    DBUpdaterThread.Start;
  end;
end;

procedure TMainForm.mnUpdateListClick(Sender: TObject);
var
  m: TModuleContainer;
  i: Integer;
begin
  if cbSelectManga.ItemIndex < 0 then Exit;
  m := TModuleContainer(cbSelectManga.Items.Objects[cbSelectManga.ItemIndex]);
  if (not isUpdating) then
  begin
    isUpdating := True;
    updateList := TUpdateListManagerThread.Create;
    updateList.websites.AddObject(m.Name, m);
    updateList.Start;
  end
  else
  begin
    // look if it's already in the update list
    for i := 0 to updateList.websites.Count-1 do
      if m = updateList.websites.Objects[i] then
      begin
        MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbYes], 0);
        Exit;
      end;
    updateList.websites.AddObject(m.Name, m);
  end;
end;

procedure TMainForm.miDownloadDeleteCompletedClick(Sender: TObject);
begin
  if cbOptionShowDeleteTaskDialog.Checked then
    if MessageDlg('', RS_DlgRemoveFinishTasks, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  DLManager.RemoveAllFinishedTasks;
  if Sender <> nil then
  UpdateVtDownload;
  // the reason we put it in here instead of in DLManager because of the size of
  // download list will change during this method
end;

procedure TMainForm.miDownloadResumeClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtDownload.SelectedCount > 0 then begin
    DLManager.Lock;
    try
      xNode := vtDownload.GetFirstSelected();
      while Assigned(xNode) do begin
        DLManager.SetTaskActive(xNode^.Index);
        xNode := vtDownload.GetNextSelected(xNode);
      end;
    finally
      DLManager.UnLock;
    end;
    DLManager.CheckAndActiveTask();
    UpdateVtDownload;
  end;
end;

procedure TMainForm.miDownloadRedownloadClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtDownload.SelectedCount > 0 then begin
    DLManager.Lock;
    try
      xNode := vtDownload.GetFirstSelected();
      while Assigned(xNode) do begin
        DLManager.RedownloadTask(xNode^.Index);
        xNode := vtDownload.GetNextSelected(xNode);
      end;
    finally
      DLManager.UnLock;
    end;
    DLManager.CheckAndActiveTask();
    UpdateVtDownload;
  end;
end;

procedure TMainForm.miDownloadStopClick(Sender: TObject);
var
  xNode: PVirtualNode;
begin
  if vtDownload.SelectedCount = 0 then Exit;
  DLManager.Lock;
  try
    xNode := vtDownload.GetFirstSelected();
    while Assigned(xNode) do begin
      DLManager.StopTask(xNode^.Index, False);
      xNode := vtDownload.GetNextSelected(xNode);
    end;
  finally
    DLManager.UnLock;
  end;
  DLManager.CheckAndActiveTask();
  UpdateVtDownload;
end;

procedure TMainForm.miMangaListDeleteClick(Sender: TObject);
var
  xNode, deleteNode: PVirtualNode;
  deleteCount: Integer;
begin
  if vtMangaList.SelectedCount = 0 then Exit;
  if dataProcess.Table.Active = False then Exit;
  if cbOptionShowDeleteTaskDialog.Checked then
    if MessageDlg('', RS_DlgRemoveItem, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  try
    vtMangaList.BeginUpdate;
    deleteCount := 0;
    xNode := vtMangaList.GetPreviousSelected(nil);
    while Assigned(xNode) do
    begin
      if dataProcess.DeleteData(xNode^.Index) then
        deleteNode := xNode
      else
        deleteNode := nil;
      xNode := vtMangaList.GetPreviousSelected(xNode);
      if Assigned(deleteNode) then
      begin
        vtMangaList.DeleteNode(deleteNode);
        Inc(deleteCount);
      end;
    end;
    if deleteCount > 0 then
    begin
      dataProcess.Table.ApplyUpdates;
      dataProcess.Table.SQLTransaction.CommitRetaining;
      vtMangaList.ClearSelection;
      UpdateVtMangaListFilterStatus
    end;
  finally
    vtMangaList.EndUpdate;
  end;
end;

procedure TMainForm.miMangaListDownloadAllClick(Sender: TObject);
var
  xNode: PVirtualNode;
  AllowedToCreate, YesAll, NoAll : Boolean;
  i: Integer;
  mResult: TModalResult;
  mBtns: TMsgDlgButtons;
  data: PMangaInfoData;
begin
  if vtMangaList.SelectedCount = 0 then Exit;

  SilentThreadManager.BeginAdd;
  try
    YesAll := False;
    NoAll := False;
    if vtMangaList.SelectedCount = 1 then
      mBtns := [mbYes, mbNo]
    else
      mBtns := [mbYes, mbNo, mbYesToAll, mbNoToAll];

    xNode := vtMangaList.GetFirstSelected;
    while Assigned(xNode) do
    begin
      data := vtMangaList.GetNodeData(xNode);
      AllowedToCreate := True;
      if DLManager.Count > 0 then
        for i := 0 to DLManager.Count - 1 do
          if data^.Title = DLManager.Items[i].DownloadInfo.title then
          begin
            if YesAll then
              AllowedToCreate := True
            else if NoAll then
              AllowedToCreate := False
            else
            begin
              if OptionShowDownloadsTabOnNewTasks then
                pcMain.ActivePage := tsDownload;
              mResult := MessageDlg('', DLManager.Items[i].DownloadInfo.title +
                LineEnding + LineEnding + RS_DlgTitleExistInDLlist, mtConfirmation,
                  mBtns, 0);
              case mResult of
                mrYes : AllowedToCreate := True;
                mrNo  : AllowedToCreate := False;
                mrYesToAll :
                  begin
                    YesAll := True;
                    NoAll := False;
                    AllowedToCreate := True;
                  end;
                mrNoToAll :
                  begin
                    YesAll := False;
                    NoAll := True;
                    AllowedToCreate := False;
                  end;
              end;
            end;
            Break;
          end;
		  
      if AllowedToCreate then
        SilentThreadManager.Add(MD_DownloadAll, TModuleContainer(data^.Module), data^.Title, data^.Link);
      xNode := vtMangaList.GetNextSelected(xNode);
    end;
  except
    on E: Exception do
      ExceptionHandler(Self, E);
  end;
  SilentThreadManager.EndAdd;
end;

procedure TMainForm.miMangaListViewInfosClick(Sender: TObject);
begin
  if Assigned(vtMangaList.FocusedNode) then begin
    with PMangaInfoData(vtMangaList.GetNodeData(vtMangaList.FocusedNode))^ do
      ViewMangaInfo(Module, Link, Title, '', miMangaListViewInfos, vtMangaList.FocusedNode);
    if pcInfo.ActivePage <> tsInfoManga then
      pcInfo.ActivePage := tsInfoManga;
  end;
end;

procedure TMainForm.miFavoritesOpenFolderClick(Sender: TObject);
begin
  if Assigned(vtFavorites.FocusedNode) then
    OpenDocument(CorrectPathSys(
      FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo));
end;

procedure TMainForm.miDownloadOpenFolderClick(Sender: TObject);
begin
  if Assigned(vtDownload.FocusedNode) then
    OpenDocument(CorrectPathSys(
      DLManager.Items[vtDownload.FocusedNode^.Index].DownloadInfo.SaveTo));
end;

procedure TMainForm.miFavoritesOpenWithClick(Sender: TObject);
begin
  if Assigned(vtFavorites.FocusedNode) then
     OpenWithExternalProgramChapters(
       FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo);
end;

procedure TMainForm.miDownloadOpenWithClick(Sender: TObject);
begin
  if Assigned(vtDownload.FocusedNode) then
    with DLManager.Items[vtDownload.FocusedNode^.Index] do
      OpenWithExternalProgramChapters(DownloadInfo.SaveTo, ChapterNames);
end;

procedure TMainForm.miTrayExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TMainForm.miTrayFinishNothingClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    OptionLetFMDDo := TFMDDo(TMenuItem(Sender).Tag);
    settingsfile.WriteInteger('general', 'LetFMDDo', Integer(OptionLetFMDDo));
  end;
end;

procedure TMainForm.miTrayShowDropBoxClick(Sender: TObject);
begin
  ShowDropTarget(TMenuItem(Sender).Checked);
end;

procedure TMainForm.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsFavorites then
  begin
    vtFavorites.Repaint;
	if vtFavorites.SelectedCount > 0 then
      sbMain.Panels[0].Text := Format(RS_Selected, [vtFavorites.SelectedCount])
    else
      sbMain.Panels[0].Text := ''
  end
  else if pcMain.ActivePage = tsInformation then
  begin
    if vtMangaList.SelectedCount > 0 then
      sbMain.Panels[0].Text := Format(RS_Selected, [vtMangaList.SelectedCount])
    else
      sbMain.Panels[0].Text := ''
  end
  else if pcMain.ActivePage = tsOption then
    LoadOptions;
end;

procedure TMainForm.pmDownloadPopup(Sender: TObject);
var
  iStop,
  iResume,
  iRedownload,
  iEnable,
  iDisable: Boolean;

  procedure ScanTasks;
  var
    Node: PVirtualNode;
  begin
    iStop := False;
    iResume := False;
    iRedownload := False;
    iEnable := False;
    iDisable := False;
    Node := vtDownload.GetFirstSelected();
    while Assigned(Node) do
    begin
      if DLManager[Node^.Index].Enabled then
      begin
        if not iDisable then
          iDisable := True;
        case DLManager[Node^.Index].Status of
          STATUS_DOWNLOAD,
          STATUS_PREPARE,
          STATUS_WAIT     : if not iStop then iStop := True;
          STATUS_STOP,
          STATUS_FAILED,
          STATUS_PROBLEM  : if not iResume then iResume := True;
          STATUS_FINISH   : if not iRedownload then iRedownload := True;
          else;
        end;
      end
      else if not iEnable then
        iEnable := True;
      if iStop and iResume and iRedownload and iEnable and iDisable then
        Break;
      Node := vtDownload.GetNextSelected(Node);
    end;
  end;

begin
  miDownloadDeleteCompleted.Enabled := DLManager.Count > 0;
  miDownloadMergeCompleted.Enabled := miDownloadDeleteCompleted.Enabled;
  with DLManager do begin
    if (vtDownload.SelectedCount = 0) or (vtDownload.FocusedNode = nil) then
    begin
      miDownloadStop.Enabled := False;
      miDownloadResume.Enabled := False;
      miDownloadRedownload.Enabled := False;
      miDownloadDelete.Enabled := False;
      miDownloadDeleteTask.Enabled := False;
      miDownloadDeleteTaskData.Enabled := False;
      miDownloadViewMangaInfo.Enabled := False;
      miDownloadOpenFolder.Enabled := False;
      miDownloadOpenWith.Enabled := False;
      miDownloadEnable.Enabled := False;
      miDownloadDisable.Enabled := False;
    end
    else
    begin
      ScanTasks;
      miDownloadStop.Enabled := iStop;
      miDownloadResume.Enabled := iResume;
      miDownloadRedownload.Enabled := iRedownload;
      miDownloadDelete.Enabled := True;
      miDownloadDeleteTask.Enabled := True;
      miDownloadDeleteTaskData.Enabled := True;
      miDownloadOpenWith.Enabled := vtDownload.SelectedCount = 1;
      miDownloadOpenFolder.Enabled := miDownloadOpenWith.Enabled;
      miDownloadViewMangaInfo.Enabled := miDownloadOpenFolder.Enabled and
        (DLManager[vtDownload.FocusedNode^.Index].DownloadInfo.Link <> '');
      miDownloadEnable.Enabled := iEnable;
      miDownloadDisable.Enabled := iDisable;
    end;
  end;
end;

procedure TMainForm.pmEditURLPopup(Sender: TObject);
begin
  medURLUndo.Enabled := edURL.CanUndo;
  medURLCut.Enabled := (Length(edURL.SelText) <> 0);
  medURLCopy.Enabled := (Length(edURL.SelText) <> 0);
  medURLPaste.Enabled := (Length(Clipboard.AsText) <> 0);
  medURLPasteandgo.Enabled := (Length(Clipboard.AsText) <> 0);
  medtURLDelete.Enabled := (Length(edURL.SelText) <> 0);
  medURLSelectAll.Enabled := (Length(edURL.Text) <> 0) and
    (edURL.SelLength <> Length(edURL.Text));
end;

procedure TMainForm.pmFavoritesPopup(Sender: TObject);
var
  iCheck,
  iStop,
  iEnable,
  iDisable: Boolean;

  procedure ScanFavs;
  var
    Node: PVirtualNode;
  begin
    iCheck := False;
    iStop := False;
    iEnable := False;
    iDisable := False;
    Node := vtFavorites.GetFirstSelected();
    while Assigned(Node) do
    begin
      if FavoriteManager[Node^.Index].Enabled then
      begin
        if not iDisable then
          iDisable := True;
        case FavoriteManager[Node^.Index].Status of
          STATUS_IDLE      : if not iCheck then iCheck := True;
          STATUS_CHECK,
          STATUS_CHECKING,
          STATUS_CHECKED   : if not iStop then iStop := True;
        end;
      end
      else if not iEnable then
        iEnable := True;
      if iEnable and iDisable and iCheck and iStop then
        Break;
      Node := vtFavorites.GetNextSelected(Node);
    end;
  end;

begin
  if vtFavorites.SelectedCount = 0 then
  begin
    miFavoritesViewInfos.Enabled := False;
    miFavoritesDownloadAll.Enabled := False;
    miFavoritesEnable.Enabled := False;
    miFavoritesDisable.Enabled := False;
    miFavoritesDelete.Enabled := False;
    miFavoritesChangeSaveTo.Enabled := False;
    miFavoritesOpenFolder.Enabled := False;
    miFavoritesOpenWith.Enabled := False;
    miFavoritesTransferWebsite.Enabled := False;
    miFavoritesRename.Enabled := False;
  end
  else
  begin
    ScanFavs;
    miFavoritesCheckNewChapter.Enabled := iCheck;
    miFavoritesStopCheckNewChapter.Enabled := iStop;
    miFavoritesEnable.Enabled := iEnable;
    miFavoritesDisable.Enabled := iDisable;
    miFavoritesTransferWebsite.Enabled := True;
    if (vtFavorites.SelectedCount = 1) and Assigned(vtFavorites.FocusedNode) then
    begin
      miFavoritesViewInfos.Enabled := True;
      miFavoritesDownloadAll.Enabled := (Trim(FavoriteManager[vtFavorites.FocusedNode^.Index].FavoriteInfo.Link) <> '');
      miFavoritesDelete.Enabled := True;
      miFavoritesChangeSaveTo.Enabled := True;
      miFavoritesOpenFolder.Enabled := DirectoryExistsUTF8(FavoriteManager.Items[vtFavorites.FocusedNode^.Index].FavoriteInfo.SaveTo);
      miFavoritesOpenWith.Enabled := miFavoritesOpenFolder.Enabled;
      miFavoritesRename.Enabled := True;
    end
    else
    begin
      miFavoritesViewInfos.Enabled := False;
      miFavoritesDownloadAll.Enabled := True;
      miFavoritesDelete.Enabled := True;
      miFavoritesChangeSaveTo.Enabled := True;
      miFavoritesOpenFolder.Enabled := False;
      miFavoritesOpenWith.Enabled := False;
      miFavoritesRename.Enabled := False;
    end;
  end;
  if FavoriteManager.isRunning then
  begin
    miFavoritesDelete.Enabled := False;
    miFavoritesChangeSaveTo.Enabled := False;
    miFavoritesTransferWebsite.Enabled := False;
  end;
end;

procedure TMainForm.pmMangaListPopup(Sender: TObject);
begin
  if vtMangaList.SelectedCount = 1 then
  begin
    pmMangaList.Items[0].Enabled := True;
    pmMangaList.Items[1].Enabled := True;
    pmMangaList.Items[2].Enabled := True;
  end
  else
  if vtMangaList.SelectedCount > 1 then
  begin
    pmMangaList.Items[0].Enabled := False;
    pmMangaList.Items[1].Enabled := True;
    pmMangaList.Items[2].Enabled := True;
  end;
  pmMangaList.Items[2].Enabled := Assigned(currentWebsite) and TModuleContainer(currentWebsite).FavoriteAvailable;
end;

procedure TMainForm.pmSbMainPopup(Sender: TObject);
begin
  if Assigned(SilentThreadManager) then
  begin
    if SilentThreadManager.Count = 0 then
      Abort;
  end
  else
    Abort;
end;

procedure TMainForm.pmTrayPopup(Sender: TObject);
var
  i: Integer;
begin
  with miTrayAfterDownloadFinish do
    for i := 0 to Count - 1 do
      if Items[i].Tag = Integer(OptionLetFMDDo) then
      begin
        Items[i].Checked := True;
        Break;
      end;
  miTrayShowDropBox.Checked := Assigned(FormDropTarget);
end;

procedure TMainForm.rgOptionCompressSelectionChanged(Sender: TObject);
begin
  seOptionPDFQuality.Enabled:=rgOptionCompress.ItemIndex=3;
  lbOptionPDFQuality.Enabled:=seOptionPDFQuality.Enabled;
  lbOptionPDFQualityHint.Enabled:=seOptionPDFQuality.Enabled;
end;

procedure TMainForm.seOptionAutoCheckFavIntervalMinutesChange(Sender: TObject);
begin
  lbOptionAutoCheckFavIntervalMinutes.Caption :=
    Format(RS_LblAutoCheckNewChapterMinute, [seOptionAutoCheckFavIntervalMinutes.Value]);
end;

procedure TMainForm.spMainSplitterMoved(Sender: TObject);
begin
  sbMain.Panels[0].Width := spMainSplitter.Left;
end;

procedure TMainForm.tbDownloadDeleteCompletedClick(Sender: TObject);
begin
  if DLManager.TaskStatusPresent([STATUS_FINISH]) then
    miDownloadDeleteCompletedClick(miDownloadDeleteCompleted);
end;

procedure TMainForm.tbDownloadResumeAllClick(Sender: TObject);
begin
  DLManager.StartAllTasks;
  UpdateVtDownload;
end;

procedure TMainForm.tbDownloadStopAllClick(Sender: TObject);
begin
  DLManager.StopAllTasks;
  UpdateVtDownload;
end;

procedure TMainForm.tbDropTargetOpacityChange(Sender: TObject);
begin
  frmDropTarget.FAlphaBlendValue := tbDropTargetOpacity.Position;
  if Assigned(FormDropTarget) then
    FormDropTarget.AlphaBlendValue := frmDropTarget.FAlphaBlendValue;
end;

procedure TMainForm.tbWebsitesCollapseAllClick(Sender: TObject);
begin
  vtOptionMangaSiteSelection.FullCollapse;
end;

procedure TMainForm.tbWebsitesExpandAllClick(Sender: TObject);
begin
  vtOptionMangaSiteSelection.FullExpand;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  if (WindowState = wsMinimized) or (Visible = False) then
  begin
    WindowState := PrevWindowState;
    ShowInTaskBar := stDefault;
    Show;
  end
  else
    Application.BringToFront;
end;

procedure TMainForm.tvDownloadFilterSelectionChanged(Sender: TObject);
begin
  vtDownloadUpdateFilters(False);
end;

procedure TMainForm.UniqueInstanceFMDOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Show;
  BringToFront;
end;

procedure TMainForm.vtDownloadAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  BarRect: TRect;
  Percents: double;
  ww, hh: Integer;
begin
  if Column <> 2 then Exit;
  with DLManager.Items[Node^.Index], TargetCanvas do
  begin
    if Status in [STATUS_FINISH, STATUS_COMPRESS, STATUS_FAILED] then
      Percents := 1
    else
    if (DLManager.Items[Node^.Index].DownCounter = 0) or
      (DLManager.Items[Node^.Index].PageNumber = 0) then
      Percents := 0
    else
      Percents := DLManager.Items[Node^.Index].DownCounter / DLManager.Items[Node^.Index].PageNumber;

    // base bar
    BarRect := CellRect;
    BarRect.Inflate(-2,-2);
    BarRect.Right-=1;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    Pen.Color := CL_BarGrayLine;
    Brush.Color := CL_BarGray;
    Rectangle(BarRect);

    // progress bar
    if Percents > 0 then
    begin
      BarRect.Right := round((BarRect.Right - BarRect.Left) * Percents) + BarRect.Left;
      case DLManager.Items[Node^.Index].Status of
        STATUS_STOP,
        STATUS_FAILED  : begin
                           Pen.Color   := CL_BarRedLine;
                           Brush.Color := CL_BarRed;
                         end;
        STATUS_WAIT    : begin
                           Pen.Color   := CL_BarGrayLine;
                           Brush.Color := CL_BarGray;
                         end;
        STATUS_DOWNLOAD: begin
                           Pen.Color   := CL_BarBlueLine;
                           Brush.Color := CL_BarBlue;
                         end;
        STATUS_PROBLEM : begin
                           Pen.Color   := CL_BarYellowLine;
                           Brush.Color := CL_BarYellow;
                         end;
        STATUS_FINISH  : begin
                           Pen.Color   := CL_BarGreenLine;
                           Brush.Color := CL_BarGreen;
                         end;
        else
          begin
            Pen.Color   := CL_BarBrownGoldLine;
            Brush.Color := CL_BarBrownGold;
          end;
      end;
      Frame(BarRect);
      BarRect.Inflate(-2,-2);
      GradientFill(BarRect, BlendColor(Brush.Color,CL_BarGray,128), Brush.Color, gdHorizontal);
    end;
    // text
    if DownloadInfo.Progress <> '' then
    begin
      Font.Color := clBlack;
      Brush.Style := bsClear;
      GetTextSize(DownloadInfo.Progress, ww, hh);
      TextOut(CellRect.Left + ((CellRect.Right - CellRect.Left - ww) div 2),
        CellRect.Top + ((CellRect.Bottom - CellRect.Top - hh) div 2), DownloadInfo.Progress);
    end;
  end;
end;

procedure TMainForm.vtDownloadColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  if Column = 5 then
    miDownloadOpenFolderClick(Sender)
  else
    miDownloadOpenWithClick(Sender);
end;

procedure TMainForm.vtDownloadDragAllowed(Sender : TBaseVirtualTree;
  Node : PVirtualNode; Column : TColumnIndex; var Allowed : Boolean);
begin
  Allowed := True;
end;

procedure TMainForm.vtDownloadMoveItems(NextIndex: Cardinal; Mode: TDropMode);
var
  i, nIndex: Cardinal;
  cNode: PVirtualNode;
  ConTemp: TTaskContainers;
begin
  if vtDownload.SelectedCount=0 then Exit;
  nIndex:=NextIndex;
  vtDownload.BeginUpdate;
  ConTemp:=TTaskContainers.Create;
  DLManager.Lock;
  try
    i:=0;
    cNode:=vtDownload.GetFirstSelected();
    while cNode<>nil do
    begin
      vtDownload.Selected[cNode]:=False;
      ConTemp.Add(DLManager.Items[cNode^.Index-i]);
      DLManager.Items.Delete(cNode^.Index-i);
      if (nIndex>0) and (cNode^.Index<nIndex) then
        Dec(nIndex);
      Inc(i);
      cNode:=vtDownload.GetNextSelected(cNode);
    end;

    for i:=0 to ConTemp.Count-1 do
    begin
      if (i=0) and (Mode in [dmBelow,dmNowhere]) then
        Inc(nIndex)
      else if (i>0) and (nIndex<DLManager.Count) then
        Inc(nIndex);
      if nIndex>DLManager.Count then
        nIndex:=DLManager.Count;
      DLManager.Items.Insert(nIndex, ConTemp[i]);
    end;

    cNode:=vtDownload.GetFirst;
    while cNode^.Index<nIndex do
      cNode:=vtDownload.GetNext(cNode);

    if Mode in [dmBelow,dmNowhere] then
      vtDownload.FocusedNode:=cNode;

    for i:=0 to ConTemp.Count-1 do
    begin
      vtDownload.Selected[cNode]:=True;
      if i<ConTemp.Count-1 then
        cNode:=vtDownload.GetPrevious(cNode);
    end;

    if Mode=dmAbove then
      vtDownload.FocusedNode:=cNode;

    // todo: consider dbupdateorder queue in timer
    DLManager.UpdateOrder;
  finally
    DLManager.UnLock;
  end;
  ConTemp.Free;
  vtDownload.EndUpdate;
  vtDownloadUpdateFilters;
end;

procedure TMainForm.vtDownloadDragDrop(Sender : TBaseVirtualTree;
  Source : TObject; DataObject : IDataObject; Formats : TFormatArray;
  Shift : TShiftState; const Pt : TPoint; var Effect : LongWord;
  Mode : TDropMode);
begin
  if (Source=vtDownload) and (vtDownload.RootNodeCount>1) then
  begin
    if Mode = dmNowhere then
      vtDownloadMoveItems(vtDownload.GetLast^.Index, Mode)
    else
      vtDownloadMoveItems(vtDownload.DropTargetNode^.Index, Mode);
  end
  else
  begin
    AddSilentThread(frmDropTarget.GetDropURLs(DataObject), MD_DownloadAll);
  end;
end;

procedure TMainForm.vtDownloadDragOver(Sender : TBaseVirtualTree;
  Source : TObject; Shift : TShiftState; State : TDragState; const Pt : TPoint;
  Mode : TDropMode; var Effect : LongWord; var Accept : Boolean);
begin
  Accept:=True;
end;

procedure TMainForm.vtDownloadFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  Sender.ScrollIntoView(Node, False, False);
end;

// Download table

procedure TMainForm.vtDownloadGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
var
  l, i: Cardinal;
begin
  with DLManager.Items[Node^.Index],DLManager.Items[Node^.Index].DownloadInfo do
    case Column of
      0: begin
           l := ChapterLinks.Count;
           if l>0 then
           begin
             HintText:='';
             if l<30 then
               for i:=0 to l-1 do begin
                 if HintText<>'' then HintText+=LineEnding;
                 HintText+=ChapterNames.Strings[i]
               end
             else
             begin
               for i:=0 to 14 do begin
                 if HintText<>'' then HintText+=LineEnding;
                 HintText+=ChapterNames.Strings[i]
               end;
               HintText+=LineEnding+'...';
               for i:=l-15 to l-1 do begin
                 if HintText<>'' then HintText+=LineEnding;
                 HintText+=ChapterNames.Strings[i]
               end;
             end;
           end;
         end;
      1: HintText:=Status;
      2: HintText:=Progress;
      4: HintText:=Website;
      5: HintText:=SaveTo;
      6: HintText:=DateTimeToStr(DateAdded);
      7: HintText:=DateTimeToStr(DateLastDownloaded);
    end;
end;

procedure TMainForm.vtDownloadGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if vtDownload.Header.Columns[Column].Position = 0 then
    if not DLManager[Node^.Index].Enabled then
      ImageIndex := 8
    else
      ImageIndex := Integer(DLManager[Node^.Index].Status);
end;

procedure TMainForm.vtDownloadGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  with DLManager[Node^.Index].DownloadInfo do
    case Column of
      0: CellText:=Title;
      1: CellText:=Status;
      2: begin
           if Progress='' then CellText:='Empty'
           else CellText:=Progress;
         end;
      3: CellText:=TransferRate;
      4: CellText:=Website;
      5: CellText:=SaveTo;
      6: CellText:=DateTimeToStr(DateAdded);
      7: CellText:=DateTimeToStr(DateLastDownloaded);
    end;
end;

procedure TMainForm.vtDownloadHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button <> mbLeft then Exit;
  if (HitInfo.Column = 2) or (HitInfo.Column = 3) then Exit;
  if DLManager.SortColumn = HitInfo.Column then
    DLManager.SortDirection := not DLManager.SortDirection;
  DLManager.SortColumn := HitInfo.Column;
  vtDownload.Header.SortDirection := TSortDirection(DLManager.SortDirection);
  vtDownload.Header.SortColumn := HitInfo.Column;
  if DLManager.Count > 1 then
    DLManager.Sort(HitInfo.Column);
  UpdateVtDownload;
end;

procedure TMainForm.vtDownloadKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  if (ssCtrl in Shift) then begin
    if (Sender.SelectedCount>0) and
      (CharCode in [VK_UP,VK_DOWN,VK_HOME,VK_END]) then
      DoDefault:=False;
  end;
end;

procedure TMainForm.vtDownloadKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
var
  p: Cardinal;
begin
  if not (ssCtrl in Shift) then Exit;
  if vtDownload.SelectedCount=0 then Exit;
  p:=vtDownload.GetFirstSelected()^.Index;
  case Key of
    VK_UP   : if p>0 then vtDownloadMoveItems(p-1,dmAbove);
    VK_DOWN : vtDownloadMoveItems(p,dmBelow);
    VK_HOME : vtDownloadMoveItems(0,dmAbove);
    VK_END  : vtDownloadMoveItems(vtDownload.RootNodeCount-1,dmBelow);
  end;
end;

procedure TMainForm.vtDownloadKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    miDownloadDeleteTaskClick(miDownloadDeleteTask);
end;

procedure TMainForm.vtDownloadPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if not DLManager[Node^.Index].Enabled then
    TargetCanvas.Font.Color := TVirtualStringTree(Sender).Colors.DisabledColor;
end;

procedure TMainForm.vtFavoritesBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

var
  C: TColor;
begin
  if CellPaintMode <> cpmPaint then Exit;
  with TargetCanvas, FavoriteManager.Items[Node^.Index] do
  begin
    if not Enabled then Exit;
    C := Brush.Color;
    Brush.Color := clNone;
    if Trim(FavoriteInfo.Link) = '' then
      Brush.Color := CL_FVBrokenFavorite
    else
    begin
      if FavoriteInfo.Status = MangaInfo_StatusCompleted then
        Brush.Color := CL_FVCompletedManga;
      if FavoriteInfo.CurrentChapter = '0' then
        Brush.Color := CL_FVEmptyChapters;
      if Status = STATUS_CHECKING then
        Brush.Color := CL_FVChecking
      else
      if (Status = STATUS_CHECKED) and
        Assigned(NewMangaInfo) then
      begin
        if NewMangaInfoChaptersPos.Count > 0 then
          Brush.Color := CL_FVNewChapterFound;
      end;
    end;
    if Brush.Color <> clNone then
      FillRect(CellRect)
    else
      Brush.Color := C;
  end;
end;

procedure TMainForm.vtFavoritesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if vtFavorites.SelectedCount > 0 then
    sbMain.Panels[0].Text := Format(RS_Selected, [vtFavorites.SelectedCount])
  else
    sbMain.Panels[0].Text := '';
end;

procedure TMainForm.vtFavoritesColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  if Column = 4 then
    miFavoritesOpenFolderClick(Sender)
  else
    case OptionDefaultAction of
      1:
        miFavoritesViewInfosClick(Sender);
      2:
        miFavoritesRenameClick(Sender);
      3:
        miFavoritesCheckNewChapterClick(Sender);
      else
        miFavoritesOpenWithClick(Sender);
    end;
end;

procedure TMainForm.vtFavoritesDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
begin
  AddSilentThread(frmDropTarget.GetDropURLs(DataObject), MD_AddToFavorites);
end;

procedure TMainForm.vtFavoritesDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept:=True;
  Effect:=DROPEFFECT_LINK;
end;

procedure TMainForm.vtFavoritesGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
begin
  if Node^.Index>=FavoriteManager.Count then Exit;
  with FavoriteManager.Items[Node^.Index].FavoriteInfo do
    case Column of
      1: if Trim(Link) = '' then
         begin
           HintText := RS_HintFavoriteProblem;
         end
         else
         begin
           HintText := Title;
         end;
      2: HintText := currentChapter;
      3: HintText := Website;
      4: HintText := cbFilterStatus.Items[StrToIntDef(Status, cbFilterStatus.Items.Count - 1)];
      5: HintText := saveTo;
      6: HintText := DateTimeToStr(DateAdded);
      7: HintText := DateTimeToStr(DateLastChecked);
      8: HintText := DateTimeToStr(DateLastUpdated);
    end;
end;

procedure TMainForm.vtFavoritesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if vtFavorites.Header.Columns[Column].Position<>1 then Exit;
  with FavoriteManager.Items[Node^.Index] do
  begin
    if Trim(FavoriteInfo.Link)='' then
      ImageIndex:=16
    else
      case FavoriteManager.Items[Node^.Index].Status of
        STATUS_CHECK    : ImageIndex:=19;
        STATUS_CHECKING : ImageIndex:=12;
        STATUS_CHECKED  :
          begin
            ImageIndex:=20;
            if Assigned(NewMangaInfo) then
            begin
              if NewMangaInfoChaptersPos.Count>0 then
                ImageIndex:=21
              else
              if NewMangaInfo.Status=MangaInfo_StatusCompleted then
                ImageIndex:=5
            end;
          end;
        else
          ImageIndex:=-1;
      end;
  end;
end;

// vtFavorites

procedure TMainForm.vtFavoritesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Node^.Index>=FavoriteManager.Count then Exit;
  with FavoriteManager.Items[Node^.Index].FavoriteInfo do
    case Column of
      0: CellText:=IntToStr(Node^.Index+1);
      1: CellText:=Title;
      2: CellText:=currentChapter;
      3: CellText:=Website;
      4: CellText:=cbFilterStatus.Items[StrToIntDef(Status, cbFilterStatus.Items.Count - 1)];
      5: CellText:=saveTo;
      6: CellText:=DateTimeToStr(DateAdded);
      7: CellText:=DateTimeToStr(DateLastChecked);
      8: CellText:=DateTimeToStr(DateLastUpdated);
    end;
end;

procedure TMainForm.vtFavoritesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button <> mbLeft then Exit;
  if FavoriteManager.isRunning then Exit;
  if HitInfo.Column = 0 then Exit;
  FavoriteManager.isRunning := True;
  if FavoriteManager.SortColumn = HitInfo.Column then
    FavoriteManager.sortDirection := not FavoriteManager.sortDirection
  else
    FavoriteManager.SortColumn := HitInfo.Column;
  vtFavorites.Header.SortColumn := HitInfo.Column;
  vtFavorites.Header.SortDirection := TSortDirection(FavoriteManager.sortDirection);
  if FavoriteManager.Count > 1 then
    FavoriteManager.Sort(HitInfo.Column);
  UpdateVtFavorites;
  FavoriteManager.isRunning := False;
end;

procedure TMainForm.vtFavoritesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if not FavoriteManager[Node^.Index].Enabled then
    TargetCanvas.Font.Color := TVirtualStringTree(Sender).Colors.DisabledColor;
end;

procedure TMainForm.vtMangaListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  //if (NOT isUpdating) then
  //begin
  if vtMangaList.SelectedCount > 0 then
    sbMain.Panels[0].Text := Format(RS_Selected, [vtMangaList.SelectedCount])
  else
    sbMain.Panels[0].Text := '';
  //end;
end;

procedure TMainForm.vtMangaListColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  miMangaListViewInfosClick(vtMangaList);
end;

// options

procedure TMainForm.btOptionApplyClick(Sender: TObject);
var
  oldOptionMaxParallel: Integer;
begin
  SaveOptions(True);
  oldOptionMaxParallel := OptionMaxParallel;
  ApplyOptions;
  if OptionMaxParallel > oldOptionMaxParallel then
    DLManager.CheckAndActiveTask();
  if not Self.Focused then Self.SetFocus;
end;

// vtMangaList

procedure TMainForm.vtMangaListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  data: PMangaInfoData;
begin
  if CellPaintMode <> cpmPaint then Exit;
  with TargetCanvas do
  begin
    Brush.Color := clNone;
    data := Sender.GetNodeData(Node);
    if data^.Status = MangaInfo_StatusCompleted then
      Brush.Color := CL_MNCompletedManga;
    if miHighlightNewManga.Checked and (data^.JDN > OptionJDNNewMangaTime) then
    begin
      if Brush.Color <> clNone then
        Brush.Color := Brush.Color + CL_MNNewManga
      else
        Brush.Color := CL_MNNewManga;
    end;
    if Brush.Color <> clNone then
      FillRect(CellRect);
  end;
end;

procedure TMainForm.vtMangaListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  data: PMangaInfoData;
begin
  data := Sender.GetNodeData(Node);
  Finalize(data^);
end;

procedure TMainForm.vtMangaListGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
var
  data: PMangaInfoData;
begin
  data := Sender.GetNodeData(Node);
  with data^ do
  begin
    if dataProcess.FilterAllSites then
      HintText += RS_InfoWebsite + LineEnding + TModuleContainer(Module).Name + LineEnding2;
    HintText += RS_InfoTitle + LineEnding + Title;
    if AltTitles <> '' then
      HintText += LineEnding2 + RS_InfoAltTitles + LineEnding + AltTitles;
    if Authors <> '' then
      HintText += LineEnding2 + RS_InfoAuthors + LineEnding + Authors;
    if Artists <> '' then
      HintText += LineEnding2 + RS_InfoArtists + LineEnding + Artists;
    if Genres <> '' then
      HintText += LineEnding2 + RS_InfoGenres + LineEnding + Genres;
    if Status <> '' then
    begin
      HintText += LineEnding2 + RS_InfoStatus + LineEnding;
      if Status = '0' then
      begin
        HintText += cbFilterStatus.Items[0]
      end
      else if Status = '1' then
      begin
        HintText += cbFilterStatus.Items[1]
      end
      else if Status = '2' then
      begin
        HintText += cbFilterStatus.Items[2]
      end
      else if Status = '3' then
      begin
        HintText += cbFilterStatus.Items[3]
      end
      else
      begin
        HintText += Status;
      end;
    end;
    if Summary <> '' then
      HintText += LineEnding2 + RS_InfoSummary + LineEnding + Summary;
  end;
end;

procedure TMainForm.vtMangaListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  data: PMangaInfoData;
begin
  data := Sender.GetNodeData(Node);
  CellText := data^.TitleFormat;
end;

procedure TMainForm.InitCheckboxes;
var
  i: Cardinal;
begin
  for i := 0 to 37 do
    TCheckBox(pnGenres.Controls[i]).State := cbGrayed;
end;

procedure TMainForm.vtFavoritesFilterCountChange;
begin
  with FavoriteManager do
  begin
    rbFavoritesShowAll.Caption := Format('%s (%d)', [RS_FavoritesShowAll, Count]);
    rbFavoritesShowEnabled.Caption := Format('%s (%d)', [RS_FavoritesShowEnabled, EnabledCount]);
    rbFavoritesShowDisabled.Caption := Format('%s (%d)', [RS_FavoritesShowDisabled, DisabledCount]);
  end;
end;

procedure TMainForm.tvDownloadFilterRefresh(const ResourceChanged: Boolean);
begin
  // update download filter treeview
  tvDownloadFilter.BeginUpdate;
  with DLManager, tvDownloadFilter do
    try
      // root
      Items[0].Text := Format('%s (%d)', [RS_AllDownloads, vtDownload.RootNodeCount]);

      // childs
      Items[1].Text := Format('%s (%d)', [RS_Finish, StatusCount[STATUS_FINISH]]);
      Items[2].Text := Format('%s (%d)', [RS_InProgress,
        StatusCount[STATUS_DOWNLOAD] +
        StatusCount[STATUS_PREPARE] +
        StatusCount[STATUS_WAIT]]);
      Items[3].Text := Format('%s (%d)', [RS_Stopped, StatusCount[STATUS_STOP]]);
      Items[4].Text := Format('%s (%d)', [RS_Failed,
        StatusCount[STATUS_PROBLEM] +
        StatusCount[STATUS_FAILED]]);
      Items[5].Text := Format('%s (%d)', [RS_Disabled, DisabledCount]);

      if ResourceChanged then
      begin
        // root
        Items[6].Text := RS_History;

        // childs
        Items[7].Text := RS_Today;
        Items[8].Text := RS_Yesterday;
        Items[9].Text := RS_Last7days;
        Items[10].Text := RS_ThisMonth;
        Items[11].Text := RS_Last6months;
        Items[12].Text := RS_OlderThan6months;
        Items[13].Text := RS_Custom;
      end;
    finally
      tvDownloadFilter.EndUpdate;
    end;
end;

procedure TMainForm.FilterDownloadsByJDN(const lowJDN, highJDN: Integer);
var
  jdn: Integer;
  xNode: PVirtualNode;
begin
  xNode := vtDownload.GetFirst();
  while Assigned(xNode) do
  begin
    with DLManager.Items[xNode^.Index] do
    begin
      jdn := DateToJDN(DownloadInfo.DateAdded);
      vtDownload.IsVisible[xNode] := (jdn >= lowJDN) and (jdn <= highJDN);
    end;
    xNode := vtDownload.GetNext(xNode);
  end;
end;

procedure TMainForm.vtDownloadUpdateFilters(const RefreshTree: Boolean);
var
  ACurrentJDN: Integer;

  procedure ShowTasks(S: TDownloadStatusTypes = []);
  var
    xNode: PVirtualNode;
  begin
    if (S = []) then
    begin
      if vtDownload.VisibleCount <> vtDownload.RootNodeCount then
      begin
        xNode := vtDownload.GetFirst();
        while Assigned(xNode) do
        begin
          vtDownload.IsVisible[xNode] := True;
          xNode := vtDownload.GetNext(xNode);
        end;
      end;
    end
    else
    begin
      xNode := vtDownload.GetFirst();
      while Assigned(xNode) do
      begin
        vtDownload.IsVisible[xNode] := DLManager[xNode^.Index].Status in S;
        xNode := vtDownload.GetNext(xNode);
      end;
    end;
  end;

  procedure ShowDisabled;
  var
    xNode: PVirtualNode;
  begin
    xNode := vtDownload.GetFirst();
    while Assigned(xNode) do
    begin
      vtDownload.IsVisible[xNode] := not DLManager.Items[xNode^.Index].Enabled;
      xNode := vtDownload.GetNext(xNode);
    end;
  end;

begin
  if tvDownloadFilter.Selected = nil then Exit;

  if tvDownloadFilter.Selected.AbsoluteIndex = 13 then // custom date filter
  begin
    if pnDownloadFilterCustomDate.Visible = False then
      pnDownloadFilterCustomDate.Visible := True;
  end
  else
  if pnDownloadFilterCustomDate.Visible then
    pnDownloadFilterCustomDate.Visible := False;

  if isStartup then Exit;

  vtDownload.BeginUpdate;
  try
    if vtDownload.RootNodeCount <> DLManager.Count then
      vtDownload.RootNodeCount := DLManager.Count;
    // filter download list
    case tvDownloadFilter.Selected.AbsoluteIndex of
      0, 6: ShowTasks;
      1: ShowTasks([STATUS_FINISH]);
      2: ShowTasks([STATUS_WAIT, STATUS_PREPARE, STATUS_DOWNLOAD, STATUS_COMPRESS]);
      3: ShowTasks([STATUS_STOP]);
      4: ShowTasks([STATUS_PROBLEM, STATUS_FAILED]);
      5: ShowDisabled;

      7: begin // today
           ACurrentJDN := DateToJDN(Now);
           FilterDownloadsByJDN(ACurrentJDN, ACurrentJDN);
         end;
      8: begin // yesterday
           ACurrentJDN := DateToJDN(Now)-1;
           FilterDownloadsByJDN(ACurrentJDN, ACurrentJDN);
         end;
      9: begin // last 7 days
           ACurrentJDN := DateToJDN(Now);
           FilterDownloadsByJDN(ACurrentJDN - 7, ACurrentJDN);
         end;
      10: FilterDownloadsByJDN(DateToJDN(StartOfTheMonth(Now)), DateToJDN(Now)); // this month
      11: FilterDownloadsByJDN(DateToJDN(IncMonth(Now,-6)), DateToJDN(Now)); // last 6 months
      12: FilterDownloadsByJDN(0, DateToJDN(IncMonth(Now,-6))-1); // older than 6 months
      13: FilterDownloadByCustomDate; // custom date
    end;
  finally
    vtDownload.EndUpdate;
  end;

  if RefreshTree then
    tvDownloadFilterRefresh;
end;

procedure TMainForm.AddChapterNameToList;
begin
  UpdateVtChapter;
end;

procedure TMainForm.AddSilentThread(URL: string; MetaDataType: TMetaDataType);
var
  i: Integer;
  host, link: String;
  URls: TStringList;
  m: TModuleContainer;
begin
  if Trim(URL) = '' then Exit;
  URLs := TStringList.Create;
  try
    URls.Text := URL;
    if URls.Count > 0 then
    begin
      GoogleResultURLs(URls);
      SilentThreadManager.BeginAdd;
      with TRegExpr.Create do
      try
        Expression := REGEX_HOST;
        for i := 0 to URls.Count - 1 do
        begin
          host := '';
          link := '';
          host := LowerCase(Replace(URls[i], '$2', True));
          link := Replace(URls[i], '$4', True);
          m := nil;
          if (host <> '') and (link <> '') then
          begin
            m := Modules.LocateModuleByHost(host);
            if Assigned(m) then
            begin
              if not ((MetaDataType = MD_AddToFavorites) and not(m.FavoriteAvailable)) then
                SilentThreadManager.Add(MetaDataType, m, '', link);
            end;
          end;
        end;
      finally
        Free;
      end;
      SilentThreadManager.EndAdd;
    end;
  finally
    URls.Free;
  end;
end;

procedure TMainForm.AddSilentThread(URL: string);
var
  mt: TMetaDataType;
begin
  if Trim(URL)='' then Exit;
  if rgDropTargetMode.ItemIndex=0 then
    mt:=MD_DownloadAll
  else
    mt:=MD_AddToFavorites;
  AddSilentThread(URL,mt);
end;

procedure TMainForm.AddTextToInfo(const ATitle, AValue: String);
var
  p: Integer;
  fp: TFontParams;
  s: string;
begin
  s := Trim(FixWhiteSpace(AValue));
  if s = '' then Exit;
  if ATitle = RS_InfoSummary then
    s := Trim(StringBreaks(s));
  with rmInformation do
  begin
    if Lines.Count > 0 then
      Lines.Add('');
    p := SelStart;
    GetTextAttributes(p, fp);
    if IsDarkModeEnabled then
    begin
      fp.Color := RGBToColor(245, 245, 245);
    end;
    fp.Style += [fsBold, fsUnderline];
    Inc(fp.Size);
    SetTextAttributes(p, 0, fp);
    Lines.Add(ATitle);
    p := SelStart;
    fp.Style -= [fsBold, fsUnderline];
    Dec(fp.Size);
    SetTextAttributes(p, 0, fp);
    Lines.Add(s);
  end;
end;

procedure TMainForm.FillSaveTo;
begin
  if LastUserPickedSaveTo = '' then
    LastUserPickedSaveTo := Trim(settingsfile.ReadString('saveto', 'SaveTo', DEFAULT_PATH));
  if LastUserPickedSaveTo = '' then
    LastUserPickedSaveTo := DEFAULT_PATH;
  edSaveTo.Text := LastUserPickedSaveTo;
end;

procedure TMainForm.OverrideSaveTo(const AModule: Pointer);
var
  s: String;
begin
  if Assigned(AModule) then
  begin
    s := TModuleContainer(AModule).Settings.OverrideSettings.SaveToPath;
    if s <> '' then
      edSaveTo.Text := s;
  end;
end;

procedure TMainForm.ViewMangaInfo(const AModule: Pointer; const ALink, ATitle,
  ASaveTo: String; const ASender: TObject; const AMangaListNode: PVirtualNode);
var
  fav: TFavoriteContainer;
begin
  if (ALink = '') or (AModule = nil) then Exit;

  // terminate exisiting getmangainfo thread
  if Assigned(GetInfosThread) then
    try
      GetInfosThread.Terminate;
      GetInfosThread.WaitFor;
    except
    end;

  // set the UI
  edURL.Text := FillHost(TModuleContainer(AModule).RootURL, ALink);
  pcMain.ActivePage := tsInformation;
  imCover.Picture.Assign(nil);
  rmInformation.Clear;
  rmInformation.Lines.Add(RS_Loading);
  clbChapterList.Clear;
  if Assigned(gifWaiting) then
  begin
    tmAnimateMangaInfo.Enabled := True;
    pbWait.Visible := True;
  end;
  btDownload.Enabled := False;
  btDownloadSplit.Enabled := btDownload.Enabled;
  btReadOnline.Enabled := True;

  // set saveto
  edSaveTo.Text := ASaveTo;
  LastViewMangaInfoSender := ASender;
  if edSaveTo.Text = '' then
  begin
    FillSaveTo;
    OverrideSaveTo(AModule);
  end;


  DisableAddToFavorites(AModule);
  //check if manga already in FavoriteManager list
  fav := FavoriteManager.LocateMangaByLink(TModuleContainer(AModule).ID, ALink);
  if fav <> nil then
  begin
    btAddToFavorites.Enabled := False;
    if LastViewMangaInfoSender <> miDownloadViewMangaInfo then
    begin
      edSaveTo.Text := fav.FavoriteInfo.SaveTo;
      LastViewMangaInfoSender := miFavoritesViewInfos;
    end;
  end;

  // start the thread
  GetInfosThread := TGetMangaInfosThread.Create(AModule, ALink, AMangaListNode);
  if (ASender = miDownloadViewMangaInfo) or (ASender = miFavoritesViewInfos) then
    GetInfosThread.Title := ''      // retrieve the original title so custom rename can remove them
  else
    GetInfosThread.Title := ATitle;
  GetInfosThread.Start;
end;

procedure TMainForm.ShowInformation;
var
  i, j: Integer;
begin
  pcMain.ActivePage := tsInformation;

  imCover.Picture.Assign(nil);

  with rmInformation do
    try
      Lines.BeginUpdate;
      Lines.Clear;
      edURL.Text := mangaInfo.URL;
      AddTextToInfo(RS_InfoTitle, mangaInfo.Title);
      AddTextToInfo(RS_InfoAltTitles, mangaInfo.AltTitles);
      AddTextToInfo(RS_InfoAuthors, mangaInfo.Authors);
      AddTextToInfo(RS_InfoArtists, mangaInfo.Artists);
      AddTextToInfo(RS_InfoGenres, mangaInfo.Genres);
      i := StrToIntDef(mangaInfo.Status, -1);
      if (i > -1) and (i < cbFilterStatus.Items.Count) then
      begin
        AddTextToInfo(RS_InfoStatus, cbFilterStatus.Items[i]);
      end
      else
      begin
        AddTextToInfo(RS_InfoStatus, mangaInfo.Status);
      end;
      AddTextToInfo(RS_InfoSummary, mangaInfo.Summary);
      CaretPos := Point(0, 0);
    finally
      Lines.EndUpdate;
    end;

  SetLength(ChapterList, mangaInfo.ChapterNames.Count);
  if Length(ChapterList) <> 0 then
  begin
    if miChapterListAscending.Checked then
      j := 0
    else
      j := High(ChapterList);
    for i := low(ChapterList) to High(ChapterList) do
    begin
      ChapterList[i].Index := j + 1;
      ChapterList[i].Title := mangaInfo.ChapterNames[j];
      ChapterList[i].Link := mangaInfo.ChapterLinks[j];
      ChapterList[i].Downloaded := False;
      if miChapterListAscending.Checked then
        Inc(j)
      else
        Dec(j);
    end;
  end;

  miChapterListHighlightClick(nil);
  UpdateVtChapter;
  miChapterListHideDownloadedClick(nil);
  edFilterMangaInfoChaptersChange(nil);
  if (clbChapterList.RootNodeCount <> 0) and miChapterListAscending.Checked then
    clbChapterList.FocusedNode := clbChapterList.GetLast();

  btDownload.Enabled := (clbChapterList.RootNodeCount > 0);
  btDownloadSplit.Enabled := btDownload.Enabled;
  btReadOnline.Enabled := (mangaInfo.Link <> '');
end;

procedure TMainForm.LoadOptions;
begin
  with settingsfile do begin
    // general
    cbOptionOneInstanceOnly.Checked := ReadBool('general', 'OneInstanceOnly', True);
    cbOptionLiveSearch.Checked := ReadBool('general', 'LiveSearch', True);
    cbOptionMinimizeOnStart.Checked := ReadBool('general', 'MinimizeOnStart', False);
    cbOptionMinimizeToTray.Checked := ReadBool('general', 'MinimizeToTray', False);
    cbOptionDeleteCompletedTasksOnClose.Checked := ReadBool('general', 'DeleteCompletedTasksOnClose', OptionDeleteCompletedTasksOnClose);
    cbOptionSortDownloadsOnNewTasks.Checked := ReadBool('general', 'SortDownloadsOnNewTasks', OptionSortDownloadsOnNewTasks);
    cbOptionLetFMDDo.ItemIndex := ReadInteger('general', 'LetFMDDo', 0);
    edOptionExternalPath.Text := ReadString('general', 'ExternalProgramPath', '');
    edOptionExternalParams.Text := ReadString('general', 'ExternalProgramParams', DEFAULT_EXPARAM);
    miChapterListHideDownloaded.Checked := ReadBool('general', 'ChapterListHideDownloaded', False);
    cbAddAsStopped.Checked := ReadBool('general', 'AddAsStopped', False);
    miHighLightNewManga.Checked := ReadBool('general', 'HighlightNewManga', True);
    miChapterListHighlight.Checked := ReadBool('general', 'HighlightDownloadedChapters', True);
    miChapterListAscending.Checked := ReadBool('general', 'SortChapterListAscending', True);
    miChapterListDescending.Checked := not miChapterListAscending.Checked;
    cbOptionVacuumDatabasesOnExit.Checked := ReadBool('general', 'VacuumDatabasesOnExit', False);
    cbOptionEnableLongNamePaths.Checked := ReadBool('general', 'EnableLongNamePaths', False);

    // view
    cbOptionShowDownloadToolbar.Checked := ReadBool('view', 'ShowDownloadsToolbar', True);
    cbOptionShowDownloadToolbarLeft.Checked := ReadBool('view', 'ShowDownloadsToolbarLeft', True);
    cbOptionShowDownloadToolbarDeleteAll.Checked := ReadBool('view', 'ShowDownloadsToolbarDeleteAll', False);
    cbOptionEnableLoadCover.Checked := ReadBool('view', 'LoadMangaCover', True);
    cbOptionShowBalloonHint.Checked := ReadBool('view', 'ShowBalloonHint', OptionShowBalloonHint);
    cbOptionShowFavoritesTabOnNewManga.Checked := ReadBool('view', 'ShowFavoritesTabOnNewManga', OptionShowFavoritesTabOnNewManga);
    cbOptionShowDownloadsTabOnNewTasks.Checked := ReadBool('view', 'ShowDownloadsTabOnNewTasks', OptionShowDownloadsTabOnNewTasks);
    ckDropTarget.Checked := ReadBool('droptarget', 'Show', False);
    frmDropTarget.FWidth := ReadInteger('droptarget', 'Width', frmDropTarget.FWidth);
    frmDropTarget.FHeight := ReadInteger('droptarget', 'Heigth', frmDropTarget.FHeight);
    frmDropTarget.FTop := ReadInteger('droptarget', 'Top', frmDropTarget.FTop);
    frmDropTarget.FLeft := ReadInteger('droptarget', 'Left', frmDropTarget.FLeft);
    rgDropTargetMode.ItemIndex := ReadInteger('droptarget', 'Mode', 0);
    tbDropTargetOpacity.Position := ReadInteger('droptarget', 'Opacity', 255);
    
    // favorites
    OptionDefaultAction := ReadInteger('favorites', 'DefaultAction', OptionDefaultAction);
    case OptionDefaultAction of
      1:
        miFavoritesDefaultActionShowInfo.Checked := True;
      2:
        miFavoritesDefaultActionRename.Checked := True;
      3:
        miFavoritesDefaultActionCheckNewChapters.Checked := True;
      else
        miFavoritesDefaultActionOpenFolder.Checked := True;
    end;

    // connection download
    seOptionMaxParallel.Value := ReadInteger('connections', 'NumberOfTasks', OptionMaxParallel);
    seOptionMaxThread.Value := ReadInteger('connections', 'NumberOfThreadsPerTask', OptionMaxThreads);
    seOptionMaxRetry.Value := ReadInteger('connections', 'Retry', OptionMaxRetry);;
    seOptionRetryFailedTask.Value := ReadInteger('connections', 'NumberOfAutoRetryFailedTask', OptionRetryFailedTask);
    ckOptionsAlwaysStartTaskFromFailedChapters.Checked := ReadBool('connections', 'AlwaysStartFromFailedChapters', OptionAlwaysStartTaskFromFailedChapters);

    // connection misc
    seOptionMaxFavoriteThreads.Value := ReadInteger('connections', 'MaxFavoriteThreads', OptionMaxFavoriteThreads);
    seOptionMaxUpdateListThreads.Value := ReadInteger('connections', 'MaxUpdateListThreads', OptionMaxUpdateListThreads);
    seOptionMaxBackgroundLoadThreads.Value := ReadInteger('connections', 'MaxBackgroundLoadThreads', OptionMaxBackgroundLoadThreads);

    // connection general
    seOptionConnectionTimeout.Value := ReadInteger('connections', 'ConnectionTimeout', OptionConnectionTimeout);
    edOptionDefaultUserAgent.Text := ReadString('connections', 'DefaultUserAgent', DefaultUserAgent);
    if edOptionDefaultUserAgent.Text = '' then
      edOptionDefaultUserAgent.Text := DefaultUserAgent;
    // proxy
    cbOptionUseProxy.Checked := ReadBool('connections', 'UseProxy', False);
    cbOptionProxyType.Text := ReadString('connections', 'ProxyType', 'HTTP');
    edOptionHost.Text := ReadString('connections', 'Host', '');
    edOptionPort.Text := ReadString('connections', 'Port', '');
    edOptionUser.Text := DecryptString(ReadString('connections', 'User', ''));
    edOptionPass.Text := DecryptString(ReadString('connections', 'Pass', ''));

    // saveto
    edOptionDefaultPath.Text := ReadString('saveto', 'SaveTo', DEFAULT_PATH);
    if Trim(edOptionDefaultPath.Text) = '' then
      edOptionDefaultPath.Text := DEFAULT_PATH;
    seOptionPDFQuality.Value := ReadInteger('saveto', 'PDFQuality', 100);
    rgOptionCompress.ItemIndex := ReadInteger('saveto', 'Compress', 0);
    cbOptionChangeUnicodeCharacter.Checked := ReadBool('saveto', 'ChangeUnicodeCharacter', False);
    edOptionChangeUnicodeCharacterStr.Text := ReadString('saveto', 'ChangeUnicodeCharacterStr', OptionChangeUnicodeCharacterStr);
    cbOptionRemoveMangaNameFromChapter.Checked := ReadBool('saveto', 'RemoveMangaNameFromChapter', False);
    cbOptionGenerateMangaFolder.Checked := ReadBool('saveto', 'GenerateMangaFolder', True);
    edOptionMangaCustomRename.Text := ReadString('saveto', 'MangaCustomRename', DEFAULT_MANGA_CUSTOMRENAME);
    if Trim(edOptionMangaCustomRename.Text) = '' then
      edOptionMangaCustomRename.Text := DEFAULT_MANGA_CUSTOMRENAME;
    cbOptionGenerateChapterFolder.Checked := ReadBool('saveto', 'GenerateChapterFolder', True);
    edOptionChapterCustomRename.Text := ReadString('saveto', 'ChapterCustomRename', DEFAULT_CHAPTER_CUSTOMRENAME);
    if Trim(edOptionChapterCustomRename.Text) = '' then
      edOptionChapterCustomRename.Text := DEFAULT_CHAPTER_CUSTOMRENAME;
    cbOptionDigitVolume.Checked := ReadBool('saveto', 'ConvertDigitVolume', True);
    seOptionDigitVolume.Value := ReadInteger('saveto', 'DigitVolumeLength', 2);
    seOptionDigitVolume.Enabled := cbOptionDigitVolume.Checked;
    cbOptionDigitChapter.Checked := ReadBool('saveto', 'ConvertDigitChapter', True);
    seOptionDigitChapter.Value := ReadInteger('saveto', 'DigitChapterLength', 3);
    seOptionDigitChapter.Enabled := cbOptionDigitChapter.Checked;
    edOptionFilenameCustomRename.Text := ReadString('saveto', 'FilenameCustomRename', DEFAULT_FILENAME_CUSTOMRENAME);
    if Trim(edOptionFilenameCustomRename.Text) = '' then
      edOptionFilenameCustomRename.Text := DEFAULT_FILENAME_CUSTOMRENAME;
    ckPNGSaveAsJPEG.Checked := ReadBool('saveto', 'PNGSaveAsJPEG', OptionPNGSaveAsJPEG);
    cbWebPSaveAs.ItemIndex := ReadInteger('saveto', 'ConvertWebP', OptionWebPSaveAs);
    cbPNGCompressionLevel.ItemIndex := ReadInteger('saveto', 'PNGCompressionLevel', OptionPNGCompressionLevel);
    seJPEGQuality.Value := ReadInteger('saveto', 'JPEGQuality', OptionJPEGQuality);

    // update
    cbOptionAutoCheckLatestVersion.Checked := ReadBool('update', 'AutoCheckLatestVersion', True);
    cbOptionAutoCheckFavStartup.Checked := ReadBool('update', 'AutoCheckFavStartup', True);
    cbOptionAutoCheckFavStartupChange(cbOptionAutoCheckFavStartup);
    cbOptionAutoOpenFavStartup.Checked := ReadBool('update', 'AutoOpenFavStartup', False);
    cbOptionAutoCheckFavInterval.Checked := ReadBool('update', 'AutoCheckFavInterval', True);
    seOptionAutoCheckFavIntervalMinutes.Value := ReadInteger('update', 'AutoCheckFavIntervalMinutes', 60);
    lbOptionAutoCheckFavIntervalMinutes.Caption := Format(RS_LblAutoCheckNewChapterMinute, [seOptionAutoCheckFavIntervalMinutes.Value]);
    cbOptionAutoCheckFavIntervalChange(cbOptionAutoCheckFavInterval);
    seOptionNewMangaTime.Value := ReadInteger('update', 'NewMangaTime', 1);
    cbOptionAutoCheckFavDownload.Checked := ReadBool('update', 'AutoCheckFavAutoDownload', False);
    cbOptionAutoCheckFavRemoveCompletedManga.Checked := ReadBool('update', 'AutoCheckFavAutoRemoveCompletedManga', False);
    cbOptionUpdateListNoMangaInfo.Checked := ReadBool('update', 'UpdateListNoMangaInfo', False);
    cbOptionUpdateListRemoveDuplicateLocalData.Checked := ReadBool('update', 'UpdateListRemoveDuplicateLocalData', False);

    // modules updater
    LuaModulesUpdaterForm.ckShowUpdateWarning.Checked := ReadBool('modulesupdater', 'ShowUpdateWarning', OptionModulesUpdaterShowUpdateWarning);
    LuaModulesUpdaterForm.ckAutoRestart.Checked := ReadBool('modulesupdater', 'AutoRestart', OptionModulesUpdaterAutoRestart);

    // dialogs
    cbOptionShowQuitDialog.Checked := ReadBool('dialogs', 'ShowQuitDialog', True);
    cbOptionShowDeleteTaskDialog.Checked := ReadBool('dialogs', 'ShowDeleteDldTaskDialog', True);
    cbOptionShowDownloadMangalistDialog.Checked := ReadBool('dialogs', 'ShowDownloadMangalistDialog', True);

    // misc
    frmCustomColor.LoadFromIniFile(settingsfile);
    ckEnableLogging.Checked := ReadBool('logger', 'Enabled', False);
    edLogFileName.Text := ReadString('logger', 'LogFileName', '');
    if edLogFileName.Text = '' then
      edLogFileName.Text := DEFAULT_LOG_FILE;
  end;
end;

procedure TMainForm.SaveOptions(const AShowDialog: Boolean);
var
  s: String;
  i: Integer;
  m: TModuleContainer;
begin
  with settingsfile do
    try
      // general
      if cbSelectManga.Items.Count <> 0 then
      begin
        s := '';
        for i := 0 to cbSelectManga.Items.Count-1 do
        begin
          m := TModuleContainer(cbSelectManga.Items.Objects[i]);
          if m <> nil then
            s += m.ID + ',';
        end;
        s := s.TrimRight([',']);
        WriteString('general', 'MangaListSelect', s);
      end;
      WriteBool('general', 'LiveSearch', cbOptionLiveSearch.Checked);
      WriteBool('general', 'OneInstanceOnly', cbOptionOneInstanceOnly.Checked);
      if cbLanguages.ItemIndex > -1 then
        WriteString('languages', 'Selected', AvailableLanguages.Names[cbLanguages.ItemIndex]);
      WriteBool('general', 'MinimizeOnStart', cbOptionMinimizeOnStart.Checked);
      WriteBool('general', 'MinimizeToTray', cbOptionMinimizeToTray.Checked);
      WriteBool('general', 'DeleteCompletedTasksOnClose', cbOptionDeleteCompletedTasksOnClose.Checked);
      WriteBool('general', 'SortDownloadsOnNewTasks', cbOptionSortDownloadsOnNewTasks.Checked);
      WriteInteger('general', 'LetFMDDo', cbOptionLetFMDDo.ItemIndex);
      WriteString('general', 'ExternalProgramPath', edOptionExternalPath.Text);
      WriteString('general', 'ExternalProgramParams', edOptionExternalParams.Text);
      WriteBool('general', 'ChapterListHideDownloaded', miChapterListHideDownloaded.Checked);
      WriteBool('general', 'AddAsStopped', cbAddAsStopped.Checked);
      WriteBool('general', 'HighlightNewManga', miHighlightNewManga.Checked);
      WriteBool('general', 'HighlightDownloadedChapters', miChapterListHighlight.Checked);
      WriteBool('general', 'VacuumDatabasesOnExit', cbOptionVacuumDatabasesOnExit.Checked);
      WriteBool('general', 'EnableLongNamePaths', cbOptionEnableLongNamePaths.Checked);

      // view
      WriteBool('view', 'ShowDownloadsToolbar', cbOptionShowDownloadToolbar.Checked);
      WriteBool('view', 'ShowDownloadsToolbarLeft', cbOptionShowDownloadToolbarLeft.Checked);
      WriteBool('view', 'ShowFavoritesTabOnNewManga', cbOptionShowFavoritesTabOnNewManga.Checked);
      WriteBool('view', 'ShowDownloadsTabOnNewTasks', cbOptionShowDownloadsTabOnNewTasks.Checked);
      WriteBool('view', 'ShowDownloadsToolbarDeleteAll', cbOptionShowDownloadToolbarDeleteAll.Checked);
      WriteBool('view', 'LoadMangaCover', cbOptionEnableLoadCover.Checked);
      WriteBool('view', 'ShowBalloonHint', cbOptionShowBalloonHint.Checked);
      if not (isExiting and Assigned(FormDropTarget)) then
        SaveDropTargetFormInformation;

      // connections downloads
      WriteInteger('connections', 'NumberOfTasks', seOptionMaxParallel.Value);
      WriteInteger('connections', 'NumberOfThreadsPerTask', seOptionMaxThread.Value);
      WriteInteger('connections', 'Retry', seOptionMaxRetry.Value);
      WriteInteger('connections', 'NumberOfAutoRetryFailedTask', seOptionRetryFailedTask.Value);
      WriteBool('connections', 'AlwaysRetruFailedChaptersOnStart', ckOptionsAlwaysStartTaskFromFailedChapters.Checked);

      // connections misc
      WriteInteger('connections', 'MaxFavoriteThreads', seOptionMaxFavoriteThreads.Value);
      WriteInteger('connections', 'MaxUpdateListThreads', seOptionMaxUpdateListThreads.Value);
      WriteInteger('connections', 'MaxBackgroundLoadThreads', seOptionMaxBackgroundLoadThreads.Value);

      // connections general
      WriteInteger('connections', 'ConnectionTimeout', seOptionConnectionTimeout.Value);
      if DefaultUserAgent <> UserAgentDefault then
	      WriteString('connections', 'DefaultUserAgent', DefaultUserAgent)
      else
	      WriteString('connections', 'DefaultUserAgent', '');

      // proxy
      WriteBool('connections', 'UseProxy', cbOptionUseProxy.Checked);
      WriteString('connections', 'ProxyType', cbOptionProxyType.Text);
      WriteString('connections', 'Host', edOptionHost.Text);
      WriteString('connections', 'Port', edOptionPort.Text);
      WriteString('connections', 'User', EncryptString(edOptionUser.Text));
      WriteString('connections', 'Pass', EncryptString(edOptionPass.Text));

      // saveto
      if Trim(edOptionDefaultPath.Text) = '' then
        edOptionDefaultPath.Text := DEFAULT_PATH;
      WriteString('saveto', 'SaveTo', edOptionDefaultPath.Text);
      WriteBool('saveto', 'ChangeUnicodeCharacter', cbOptionChangeUnicodeCharacter.Checked);
      WriteString('saveto', 'ChangeUnicodeCharacterStr', edOptionChangeUnicodeCharacterStr.Text);
      WriteBool('saveto', 'GenerateMangaFolder', cbOptionGenerateMangaFolder.Checked);
      if Trim(edOptionMangaCustomRename.Text) = '' then
        edOptionMangaCustomRename.Text := DEFAULT_MANGA_CUSTOMRENAME;
      WriteString('saveto', 'MangaCustomRename', edOptionMangaCustomRename.Text);
      WriteInteger('saveto', 'Compress', rgOptionCompress.ItemIndex);
      WriteInteger('saveto', 'PDFQuality', seOptionPDFQuality.Value);
      WriteBool('saveto', 'RemoveMangaNameFromChapter', cbOptionRemoveMangaNameFromChapter.Checked);
      WriteBool('saveto', 'GenerateChapterFolder', cbOptionGenerateChapterFolder.Checked);
      if Trim(edOptionChapterCustomRename.Text) = '' then
        edOptionChapterCustomRename.Text := DEFAULT_CHAPTER_CUSTOMRENAME;
      WriteString('saveto', 'ChapterCustomRename', edOptionChapterCustomRename.Text);
      WriteBool('saveto', 'ConvertDigitVolume', cbOptionDigitVolume.Checked);
      WriteBool('saveto', 'ConvertDigitChapter', cbOptionDigitChapter.Checked);
      WriteInteger('saveto', 'DigitVolumeLength', seOptionDigitVolume.Value);
      WriteInteger('saveto', 'DigitChapterLength', seOptionDigitChapter.Value);
      if Trim(edOptionFilenameCustomRename.Text) = '' then
        edOptionFilenameCustomRename.Text := DEFAULT_FILENAME_CUSTOMRENAME;
      WriteString('saveto', 'FilenameCustomRename', edOptionFilenameCustomRename.Text);
      WriteBool('saveto', 'PNGSaveAsJPEG', ckPNGSaveAsJPEG.Checked);
      WriteInteger('saveto', 'ConvertWebP', cbWebPSaveAs.ItemIndex);
      WriteInteger('saveto', 'PNGCompressionLevel', cbPNGCompressionLevel.ItemIndex);
      WriteInteger('saveto', 'JPEGQuality', seJPEGQuality.Value);

      // update
      WriteBool('update', 'AutoCheckLatestVersion', cbOptionAutoCheckLatestVersion.Checked);
      WriteBool('update', 'AutoCheckFavStartup', cbOptionAutoCheckFavStartup.Checked);
      WriteBool('update', 'AutoOpenFavStartup', cbOptionAutoOpenFavStartup.Checked);
      WriteBool('update', 'AutoCheckFavInterval', cbOptionAutoCheckFavInterval.Checked);
      WriteInteger('update', 'AutoCheckFavIntervalMinutes', seOptionAutoCheckFavIntervalMinutes.Value);
      WriteInteger('update', 'NewMangaTime', seOptionNewMangaTime.Value);
      WriteBool('update', 'AutoCheckFavAutoDownload', cbOptionAutoCheckFavDownload.Checked);
      WriteBool('update', 'AutoCheckFavAutoRemoveCompletedManga', cbOptionAutoCheckFavRemoveCompletedManga.Checked);
      WriteBool('update', 'UpdateListNoMangaInfo', cbOptionUpdateListNoMangaInfo.Checked);
      WriteBool('update', 'UpdateListRemoveDuplicateLocalData', cbOptionUpdateListRemoveDuplicateLocalData.Checked);

      // modules updater
      WriteBool('modulesupdater', 'ShowUpdateWarning', LuaModulesUpdaterForm.ckShowUpdateWarning.Checked);
      WriteBool('modulesupdater', 'AutoRestart', LuaModulesUpdaterForm.ckAutoRestart.Checked);

      // dialogs
      WriteBool('dialogs', 'ShowQuitDialog', cbOptionShowQuitDialog.Checked);
      WriteBool('dialogs', 'ShowDeleteDldTaskDialog', cbOptionShowDeleteTaskDialog.Checked);
      WriteBool('dialogs', 'ShowDownloadMangalistDialog', cbOptionShowDownloadMangalistDialog.Checked);

      // misc
      frmCustomColor.SaveToIniFile(settingsfile);
      WriteBool('logger', 'Enabled', ckEnableLogging.Checked);
      if edLogFileName.Text = '' then
        edLogFileName.Text := DEFAULT_LOG_FILE;
      WriteString('logger', 'LogFileName', edLogFileName.Text);
    finally
      UpdateFile;
    end;
  Modules.SaveToFile;
end;

procedure TMainForm.ApplyOptions;
var
  i: Integer;
  b: Boolean;
  isStillHaveCurrentWebsite: Boolean;
  node: PVirtualNode;
  m: TNamePointerItem;
begin
  try
    // general
    // selected websites
    cbSelectManga.Clear;
    node := vtOptionMangaSiteSelection.GetFirstChecked();
    while node<>nil do
    begin
      m := PNamePointerItem(vtOptionMangaSiteSelection.GetNodeData(node))^;
      cbSelectManga.Items.AddObject(m.Name, TModuleContainer(m.P));
      node := vtOptionMangaSiteSelection.GetNextChecked(node);
    end;

    isStillHaveCurrentWebsite := False;
    for i := 0 to cbSelectManga.Items.Count - 1 do
    begin
      if currentWebsite = Pointer(cbSelectManga.Items.Objects[i]) then
      begin
        cbSelectManga.ItemIndex := i;
        isStillHaveCurrentWebsite := True;
        Break;
      end;
    end;
    if not isStillHaveCurrentWebsite then
    begin
      if cbSelectManga.Items.Count > 0 then
      begin
        cbSelectManga.ItemIndex := 0;
        // Disable download manga list dialog when applying options:
  	    b := cbOptionShowDownloadMangalistDialog.Checked;
	      cbOptionShowDownloadMangalistDialog.Checked := False;
        cbSelectMangaEditingDone(cbSelectManga);
	      // Revert state back to like it was before:
	      cbOptionShowDownloadMangalistDialog.Checked := b;
      end
      else
      begin
        cbSelectManga.Clear;
        currentWebsite := nil;
        vtMangaList.Clear;
        lbMode.Caption := Format(RS_ModeAll, [0]);
      end;
    end;
    //FMDInstace
    if cbOptionOneInstanceOnly.Checked then
    begin
      if FMDInstance = nil then
      begin
        FMDInstance := TSimpleIPCServer.Create(Self);
        FMDInstance.ServerID := FMD_INSTANCE;
        FMDInstance.Global := True;
        FMDInstance.OnMessage := @FMDInstanceReceiveMsg;
        FMDInstance.StartServer;
      end;
    end
    else
    begin
      if FMDInstance <> nil then
      begin
        FMDInstance.StopServer;
        FreeAndNil(FMDInstance);
      end;
    end;
    OptionLetFMDDo := TFMDDo(cbOptionLetFMDDo.ItemIndex);
    OptionEnableLoadCover := cbOptionEnableLoadCover.Checked;
    OptionDeleteCompletedTasksOnClose := cbOptionDeleteCompletedTasksOnClose.Checked;
    OptionSortDownloadsOnNewTasks := cbOptionSortDownloadsOnNewTasks.Checked;
    DLManager.DB.AutoVacuum:=cbOptionVacuumDatabasesOnExit.Checked;
    FavoriteManager.DB.AutoVacuum:=cbOptionVacuumDatabasesOnExit.Checked;

    //view
    ToolBarDownload.Visible := cbOptionShowDownloadToolbar.Checked;
    ToolBarDownloadLeft.Visible := cbOptionShowDownloadToolbarLeft.Checked;
    tbDownloadDeleteCompleted.Visible := cbOptionShowDownloadToolbarDeleteAll.Checked;
    tbSeparator1.Visible := tbDownloadDeleteCompleted.Visible;
    ShowDropTarget(ckDropTarget.Checked);
    OptionShowBalloonHint := cbOptionShowBalloonHint.Checked;
    OptionShowFavoritesTabOnNewManga := cbOptionShowFavoritesTabOnNewManga.Checked;
    OptionShowDownloadsTabOnNewTasks := cbOptionShowDownloadsTabOnNewTasks.Checked;

    //connection downloads
    OptionMaxParallel := seOptionMaxParallel.Value;
    OptionMaxThreads := seOptionMaxThread.Value;
    OptionMaxRetry := seOptionMaxRetry.Value;
    DLManager.RetryConnect := OptionMaxRetry;
    SetDefaultRetryCountAndApply(OptionMaxRetry);
    SetDefaultTimeoutAndApply(OptionConnectionTimeout * 1000);
    OptionRetryFailedTask := seOptionRetryFailedTask.Value;
    OptionAlwaysStartTaskFromFailedChapters := ckOptionsAlwaysStartTaskFromFailedChapters.Checked;

    // connections misc
    OptionMaxFavoriteThreads := seOptionMaxFavoriteThreads.Value;
    OptionMaxUpdateListThreads := seOptionMaxUpdateListThreads.Value;
    OptionMaxBackgroundLoadThreads := seOptionMaxBackgroundLoadThreads.Value;

    // connections general
    OptionConnectionTimeout := seOptionConnectionTimeout.Value;
    edOptionDefaultUserAgent.Text := Trim(edOptionDefaultUserAgent.Text);
    if edOptionDefaultUserAgent.Text = '' then
      edOptionDefaultUserAgent.Text := UserAgentDefault;
    DefaultUserAgent := edOptionDefaultUserAgent.Text;
    // proxy
    if cbOptionUseProxy.Checked then
      SetDefaultProxyAndApply(cbOptionProxyType.Text, edOptionHost.Text,
        edOptionPort.Text, edOptionUser.Text, edOptionPass.Text)
    else
      SetDefaultProxyAndApply('', '', '' ,'', '');

    //saveto
    OptionPDFQuality := seOptionPDFQuality.Value;
    DLManager.CompressType := rgOptionCompress.ItemIndex;
    OptionChangeUnicodeCharacter := cbOptionChangeUnicodeCharacter.Checked;
    OptionChangeUnicodeCharacterStr := edOptionChangeUnicodeCharacterStr.Text;
    OptionRemoveMangaNameFromChapter := cbOptionRemoveMangaNameFromChapter.Checked;
    OptionGenerateMangaFolder := cbOptionGenerateMangaFolder.Checked;
    OptionMangaCustomRename := edOptionMangaCustomRename.Text;
    OptionGenerateChapterFolder := cbOptionGenerateChapterFolder.Checked;
    OptionChapterCustomRename := edOptionChapterCustomRename.Text;
    OptionFilenameCustomRename := edOptionFilenameCustomRename.Text;
    OptionConvertDigitVolume := cbOptionDigitVolume.Checked;
    OptionConvertDigitVolumeLength := seOptionDigitVolume.Value;
    OptionConvertDigitChapter := cbOptionDigitChapter.Checked;
    OptionConvertDigitChapterLength := seOptionDigitChapter.Value;
    OptionPNGSaveAsJPEG := ckPNGSaveAsJPEG.Checked;
    OptionWebPSaveAs := cbWebPSaveAs.ItemIndex;
    OptionPNGCompressionLevel := cbPNGCompressionLevel.ItemIndex;
    OptionJPEGQuality := seJPEGQuality.Value;

    //update
    OptionAutoCheckLatestVersion := cbOptionAutoCheckLatestVersion.Checked;
    OptionAutoCheckFavStartup := cbOptionAutoCheckFavStartup.Checked;
    OptionAutoCheckFavInterval := cbOptionAutoCheckFavInterval.Checked;
    OptionAutoCheckFavIntervalMinutes := seOptionAutoCheckFavIntervalMinutes.Value;
    OptionNewMangaTime := seOptionNewMangaTime.Value;
    OptionJDNNewMangaTime := currentJDN - OptionNewMangaTime;
    OptionAutoCheckFavDownload := cbOptionAutoCheckFavDownload.Checked;
    OptionAutoCheckFavRemoveCompletedManga := cbOptionAutoCheckFavRemoveCompletedManga.Checked;
    OptionUpdateListNoMangaInfo := cbOptionUpdateListNoMangaInfo.Checked;
    OptionUpdateListRemoveDuplicateLocalData := cbOptionUpdateListRemoveDuplicateLocalData.Checked;
    tmCheckFavorites.Interval := OptionAutoCheckFavIntervalMinutes * 60000;
    tmCheckFavorites.Enabled := OptionAutoCheckFavInterval;

    // modules updater
    OptionModulesUpdaterShowUpdateWarning := LuaModulesUpdaterForm.ckShowUpdateWarning.Checked;
    OptionModulesUpdaterAutoRestart := LuaModulesUpdaterForm.ckAutoRestart.Checked;

    //misc
    frmCustomColor.Apply;
    SimpleException.SetLogFileName(edLogFileName.Text);

    if ckEnableLogging.Checked and (not Logger.Enabled) then
    begin
      Logger.Enabled := True;
      if MainExceptionHandler.LogFileOK then
      begin
        FileLogger := TFileChannel.Create(edLogFileName.Text, [fcoShowHeader, fcoShowPrefix, fcoShowTime]);
        Logger.Channels.Add(FileLogger);
      end
      else
        Logger.SendError('Log file error ' + MainExceptionHandler.LogFileStatus + '"' + edLogFileName.Text + '"');
      Logger.Send(QuotedStrd(Application.Title)+' started [PID:'+IntToStr(GetProcessID)+']');
      Logger.SendStrings('Application info', SimpleException.GetApplicationInfo);
    end
    else
    if (not ckEnableLogging.Checked) and (Logger.Enabled) then
    begin
      if Assigned(FileLogger) then
      begin
        Logger.Channels.Remove(FileLogger);
        FreeAndNil(FileLogger);
      end;
      Logger.Enabled := False;
    end;

    //languages
    ApplyLanguage;
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

procedure TMainForm.LoadMangaOptions;
var
  categories: TStringList;
  categoriesitem: TStringList;
  i, j: Integer;
  s: String;
  module: TModuleContainer;
  node: PVirtualNode;
  nodei: PVirtualNode;
  data: PNamePointerItem;
begin
  if WebsiteModules.Modules.Count <> 0 then
  try
    categories := TStringList.Create;
    // sort all
    categories.OwnsObjects := True;
    categories.Duplicates := dupIgnore;
    categories.Sorted := True;
    for i := 0 to categories.Count - 1 do
    begin
      categoriesitem := TStringList(categories.Objects[i]);
      categoriesitem.Duplicates := dupIgnore;
      categoriesitem.Sorted := True;
    end;

    // read websitemodules
    for i := 0 to WebsiteModules.Modules.Count - 1 do
    begin
      module := WebsiteModules.Modules[i];
      if module.Category <> '' then
      begin
        j := categories.IndexOf(module.Category);
        if j = -1 then
        begin
          categoriesitem := TStringList.Create;
          categoriesitem.OwnsObjects := False;
          categoriesitem.Duplicates := dupIgnore;
          categoriesitem.Sorted := True;
          categories.AddObject(module.Category, categoriesitem);
        end
        else
          categoriesitem := TStringList(categories.Objects[j]);
        categoriesitem.AddObject(module.Name, module);
      end;
    end;

    // add them to vt websites selection and availablewebsites
    vtOptionMangaSiteSelection.BeginUpdate;
    for i := 0 to categories.Count - 1 do
    begin
      node := vtOptionMangaSiteSelection.AddChild(nil, nil);
      vtOptionMangaSiteSelection.ValidateNode(node, False);
      data := vtOptionMangaSiteSelection.GetNodeData(node);
      categoriesitem := TStringList(categories.Objects[i]);
      data^.Name := categories[i];
      for j := 0 to categoriesitem.Count - 1 do
      begin
        s := categoriesitem[j];
        nodei := vtOptionMangaSiteSelection.AddChild(node, nil);
        vtOptionMangaSiteSelection.ValidateNode(nodei, False);
        nodei^.CheckType := ctCheckBox;
        data := vtOptionMangaSiteSelection.GetNodeData(nodei);
        data^.Name := categoriesitem[j];
        data^.P := categoriesitem.Objects[j];
      end;
    end;
    //vtOptionMangaSiteSelection.FullExpand();
    vtOptionMangaSiteSelection.EndUpdate;
  finally
    categories.Free;
  end;

  // load selected websites
  cbSelectManga.Clear;
  s := settingsfile.ReadString('general', 'MangaListSelect', DEFAULT_SELECTED_WEBSITES);
  for s in s.Split([',']) do
  begin
    module := Modules.LocateModule(s);
    if Assigned(module) then //only add available modules
    begin
      // set checked vt websites selection
      node := vtOptionMangaSiteSelection.GetFirst();
      while node <> nil do
      begin
        if node^.ChildCount = 0 then
        begin
          data := vtOptionMangaSiteSelection.GetNodeData(node);
          if TModuleContainer(data^.P).ID = s then
          begin
            node^.CheckState := csCheckedNormal;
            Break;
          end;
        end;
        node := vtOptionMangaSiteSelection.GetNext(node);
      end;
    end;
  end;

  // load last selected webssite
  currentWebsite := Modules.LocateModule(settingsfile.ReadString('form', 'SelectManga', ''));
end;

procedure TMainForm.edMangaListSearchChange(Sender: TObject);
begin
  if edMangaListSearch.Tag = -1 then
  begin
    edMangaListSearch.Tag := 0;
    LastSearchWeb := currentWebsite;
    LastSearchStr := UpCase(edMangaListSearch.Text);
    Exit;
  end;
  if (not cbOptionLiveSearch.Checked) and (edMangaListSearch.Tag = 0) then Exit;
  if edMangaListSearch.Tag <> 0 then
    edMangaListSearch.Tag := 0;
  if (upcase(edMangaListSearch.Text) = LastSearchStr) and (currentWebsite = LastSearchWeb) then
    Exit;

  SearchDataDB(edMangaListSearch.Text);

  //vtMangaList.Clear;
  //dataProcess.Search(edMangaListSearch.Text);
  //vtMangaList.RootNodeCount := dataProcess.RecordCount;
  //if dataProcess.Filtered then
  //  lbMode.Caption := Format(RS_ModeFiltered, [vtMangaList.RootNodeCount])
  //else
  //  lbMode.Caption := Format(RS_ModeAll, [vtMangaList.RootNodeCount]);
end;

procedure TMainForm.edMangaListSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    edMangaListSearch.Tag := 1;
    edMangaListSearchChange(edMangaListSearch);
  end
  else
  if edMangaListSearch.Tag <> 0 then
    edMangaListSearch.Tag := 0;
end;

procedure TMainForm.edOptionDefaultPathButtonClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do
    try
      InitialDir := edOptionDefaultPath.Text;
      if Execute then
        edOptionDefaultPath.Text := FileName;
    finally
      Free;
    end;
end;

procedure TMainForm.edOptionExternalPathButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      InitialDir := ExtractFileDir(edOptionExternalPath.Text);
      if Execute then
        edOptionExternalPath.Text := FileName;
    finally
      Free;
    end;
end;

procedure TMainForm.edSaveToButtonClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(nil) do
    try
      InitialDir := edSaveTo.Text;
      if Execute then
        edSaveTo.Text := FileName;
    finally
      Free;
    end;
end;

procedure TMainForm.edURLButtonClick(Sender: TObject);
var
  host, link: String;
  m: TModuleContainer;
begin
  btDownload.Enabled := False;
  btDownloadSplit.Enabled := btDownload.Enabled;
  btAddToFavorites.Enabled := False;
  btReadOnline.Enabled := False;

  SplitURL(edURL.Text, @host, @link);
  m := nil;

  if (host <> '') and (link <> '') then
  begin
    host := LowerCase(host);
    m := Modules.LocateModuleByHost(Host);
  end;

  if (m = nil) or (link = '') then
  begin
    tmAnimateMangaInfo.Enabled := False;
    pbWait.Visible := False;
    MessageDlg('', RS_DlgURLNotSupport, mtInformation, [mbYes], 0);
    Exit;
  end;

  ViewMangaInfo(m, link, '', '', edURL);
end;

procedure TMainForm.edURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (ssShift in Shift) then
  begin
    if Key = VK_V then
      medURLPasteandgoClick(medURLPasteandgo)
    else
    if Key = VK_C then
      Clipboard.AsText := edURL.Text;
    Key := 0;
  end;
end;

procedure TMainForm.UpdateVtChapter;
begin
  if clbChapterList.RootNodeCount = Length(ChapterList) then
    clbChapterList.Repaint
  else
  begin
    clbChapterList.BeginUpdate;
    clbChapterList.RootNodeCount := Length(ChapterList);
    clbChapterList.EndUpdate;
  end;
end;

procedure TMainForm.UpdateVtDownload;
begin
  vtDownloadUpdateFilters;
end;

procedure TMainForm.UpdateVtFavorites;
begin
  if vtFavorites.RootNodeCount = FavoriteManager.Items.Count then
    vtFavorites.Repaint
  else
  begin
    vtFavorites.BeginUpdate;
    vtFavorites.RootNodeCount := FavoriteManager.Count;
    vtFavorites.EndUpdate;
    vtFavoritesFilterCountChange;
  end;

  if rbFavoritesShowEnabled.Checked then
    rbFavoritesShowEnabledChange(rbFavoritesShowEnabled)
  else if rbFavoritesShowDisabled.Checked then
    rbFavoritesShowDisabledChange(rbFavoritesShowDisabled);
end;

procedure TMainForm.UpdateVtMangaListFilterStatus;
begin
  if dataProcess.Filtered then
    lbMode.Caption := Format(RS_ModeFiltered, [dataProcess.RecordCount])
  else
    lbMode.Caption := Format(RS_ModeAll, [dataProcess.RecordCount]);
end;

procedure TMainForm.LoadFormInformation;

  procedure restorevt(const vt: VirtualTrees.TVirtualStringTree; const name: String);
  var
    i: Integer;
  begin
    with settingsfile, vt.Header do
    begin
      SortColumn := ReadInteger(name, 'SortColumn', SortColumn);
      SortDirection := TSortDirection(ReadInteger(name, 'SortDirection', Integer(SortDirection)));
      for i := 0 to Columns.Count - 1 do
      begin
        Columns[i].Width := ReadInteger(name, 'Column' + IntToStr(i) + 'Width', Columns[i].Width);
        Columns[i].Position := ReadInteger(name, 'Column' + IntToStr(i) + 'Position', Columns[i].Position);
      end;
    end;
  end;

  function IsWithinAnyMonitor(ALeft, ATop, AWidth, AHeight: Integer): Boolean;
  var
    i: Integer;
    MonitorRect: TRect;
  begin
    Result := False;
    for i := 0 to Screen.MonitorCount - 1 do
    begin
      MonitorRect := Screen.Monitors[i].BoundsRect;
      if (ALeft + AWidth > MonitorRect.Left) and (ALeft < MonitorRect.Right) and
         (ATop + AHeight > MonitorRect.Top) and (ATop < MonitorRect.Bottom) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

var
  index: LongInt;
begin
  with settingsfile do
  begin
    psDownloads.Position := ReadInteger('form', 'DownloadsSplitter', psDownloads.Position);
    psInfo.Position := ReadInteger('form', 'MangaInfoSplitter', psInfo.Position);

    if ReadBool('update', 'AutoCheckFavStartup', True) and ReadBool('update', 'AutoOpenFavStartup', False) then
      pcMain.ActivePage := tsFavorites
    else
      pcMain.PageIndex := ReadInteger('form', 'pcMainPageIndex', 0);

    Left := ReadInteger('form', 'MainFormLeft', Left);
    Top := ReadInteger('form', 'MainFormTop', Top);
    Width := ReadInteger('form', 'MainFormWidth', Width);
    Height := ReadInteger('form', 'MainFormHeight', Height);

    if not IsWithinAnyMonitor(Left, Top, Width, Height) then
    begin
      Left := 0;
      Top := 0;
    end;

    CurrentFormLeft := Left;
    CurrentFormTop := Top;
    CurrentFormWidth := Min(Width, Screen.DesktopWidth);
    CurrentFormHeight := Min(Height, Screen.DesktopHeight);

    if Screen.PixelsPerInch > 96 then begin
      Width := ScaleScreenTo96(Width);
      Height := ScaleScreenTo96(Height);
      psDownloads.Position := ScaleScreenTo96(psDownloads.Position);
      psInfo.Position := ScaleScreenTo96(psInfo.Position);
    end;

    if ReadBool('form', 'MainFormMaximized', False) then
      PrevWindowState := wsMaximized
    else
      PrevWindowState := wsNormal;
    WindowState := PrevWindowState;

    ToolBarDownload.Visible := ReadBool('view', 'ShowDownloadsToolbar', True);
    ToolBarDownloadLeft.Visible := ReadBool('view', 'ShowDownloadsToolbarLeft', True);
    tbDownloadDeleteCompleted.Visible := ReadBool('view', 'ShowDownloadsToolbarDeleteAll', False);

    // misc form components
    if tvDownloadFilter.Items.Count>0 then
    begin
      index:=ReadInteger('general', 'DownloadFilterSelect', 0);
      if index>=tvDownloadFilter.Items.Count then
        index:=0;
      tvDownloadFilter.Items[index].Selected := True;
    end;

    deDownloadFilterCustomDateFrom.Date := ReadDate('DownloadFilter', 'CustomFrom', Now);
    deDownloadFilterCustomDateTo.Date := ReadDate('DownloadFilter', 'CustomTo', Now);

    restorevt(vtDownload, 'vtDownload');
    DLManager.SortColumn := vtDownload.Header.SortColumn;
    DLManager.SortDirection := Boolean(vtDownload.Header.SortDirection);

    restorevt(vtFavorites, 'vtFavorites');
    FavoriteManager.SortColumn := vtFavorites.Header.SortColumn;
    FavoriteManager.SortDirection := Boolean(vtFavorites.Header.SortDirection);

    // lua website modules list
    restorevt(LuaModulesUpdaterForm.vtLuaModulesRepos, 'vtLuaModulesRepos');
    LuaModulesUpdaterForm.SortList;

    // account
    restorevt(AccountManagerForm.vtAccountList, 'vtAccountList');
    AccountManagerForm.SortList;
  end;
end;

procedure TMainForm.SaveFormInformation;

  procedure savevt(const vt: VirtualTrees.TVirtualStringTree; const name: String);
  var
    i: Integer;
  begin
    with settingsfile, vt.Header do
    begin
      WriteInteger(name, 'SortColumn', SortColumn);
      WriteInteger(name, 'SortDirection', Integer(SortDirection));
      for i := 0 to Columns.Count - 1 do
      begin
        WriteInteger(name, 'Column' + IntToStr(i) + 'Width', ScaleFontTo96(Columns[i].Width));
        WriteInteger(name, 'Column' + IntToStr(i) + 'Position', Columns[i].Position);
      end;
    end;
  end;

begin
  with settingsfile do
  try
    WriteInteger('form', 'DownloadsSplitter', psDownloads.Position);
    WriteInteger('form', 'MangaInfoSplitter', psInfo.Position);
    WriteInteger('form', 'pcMainPageIndex', pcMain.PageIndex);
    WriteBool('form', 'MainFormMaximized', (WindowState = wsMaximized));
    if WindowState = wsMaximized then
    begin
      WriteInteger('form', 'MainFormLeft', CurrentFormLeft);
      WriteInteger('form', 'MainFormTop', CurrentFormTop);
      WriteInteger('form', 'MainFormWidth', CurrentFormWidth);
      WriteInteger('form', 'MainFormHeight', CurrentFormHeight);
    end
    else
    begin
      WriteInteger('form', 'MainFormLeft', Left);
      WriteInteger('form', 'MainFormTop', Top);
      WriteInteger('form', 'MainFormWidth', Width);
      WriteInteger('form', 'MainFormHeight', Height);
    end;

    // misc form components
    WriteInteger('general', 'DownloadFilterSelect', tvDownloadFilter.Selected.AbsoluteIndex);

    WriteDate('DownloadFilter', 'CustomFrom', deDownloadFilterCustomDateFrom.Date);
    WriteDate('DownloadFilter', 'CustomTo', deDownloadFilterCustomDateTo.Date);

    savevt(vtDownload, 'vtDownload');
    savevt(vtFavorites, 'vtFavorites');

    // lua website modules list
    savevt(LuaModulesUpdaterForm.vtLuaModulesRepos, 'vtLuaModulesRepos');

    // account
    savevt(AccountManagerForm.vtAccountList, 'vtAccountList');
  finally
    UpdateFile;
  end;
end;

procedure TMainForm.ShowDropTarget(const AShow: Boolean);
begin
  ckDropTarget.Checked := AShow;
  settingsfile.WriteBool('droptarget', 'Show', AShow);
  if AShow then
  begin
    if FormDropTarget = nil then
      Application.CreateForm(TFormDropTarget, FormDropTarget);
    frmDropTarget.OnDropChekout := @AddSilentThread;
    frmDropTarget.FAlphaBlendValue := tbDropTargetOpacity.Position;
    FormDropTarget.Show;
  end
  else
  begin
    if Assigned(FormDropTarget) then
      FormDropTarget.Close;
  end;
end;

procedure TMainForm.SaveDropTargetFormInformation;
begin
  with settingsfile do
  begin
    WriteBool('droptarget', 'Show', ckDropTarget.Checked);
    WriteInteger('droptarget', 'Mode', rgDropTargetMode.ItemIndex);
    WriteInteger('droptarget', 'Opacity', frmDropTarget.FAlphaBlendValue);
    WriteInteger('droptarget', 'Width', frmDropTarget.FWidth);
    WriteInteger('droptarget', 'Heigth', frmDropTarget.FHeight);
    WriteInteger('droptarget', 'Top', frmDropTarget.FTop);
    WriteInteger('droptarget', 'Left', frmDropTarget.FLeft);
  end;
end;

procedure TMainForm.CollectLanguagesFromFiles;
var
  i: Integer;
begin
  cbLanguages.Items.Clear;
  SimpleTranslator.LangDir := FMD_DIRECTORY + 'languages';
  SimpleTranslator.LangAppName := 'fmd';
  SimpleTranslator.CollectLanguagesFiles;
  if SimpleTranslator.AvailableLanguages.Count > 0 then
  begin
    for i := 0 to AvailableLanguages.Count - 1 do
      cbLanguages.Items.Add(SimpleTranslator.AvailableLanguages.ValueFromIndex[i]);
    cbLanguages.ItemIndex := SimpleTranslator.AvailableLanguages.IndexOfName(
    settingsfile.ReadString('languages', 'Selected', 'en'));
  end;
end;

procedure TMainForm.ApplyLanguage;
var
  idxSelectManga,
  idxLanguages,
  idxFilterStatus,
  idxOptionLetFMDDo,
  idxOptionProxyType,
  idxDropTargetMode,
  idxOptionCompress,
  idxOptionWebPConvertTo,
  idxOptionWebPPNGLevel: Integer;
begin
  if AvailableLanguages.Count = 0 then Exit;
  if cbLanguages.ItemIndex < 0 then Exit;
  if cbLanguages.ItemIndex >= AvailableLanguages.Count then Exit;
  if SimpleTranslator.LastSelected <> AvailableLanguages.Names[cbLanguages.ItemIndex] then
  begin
    // TCombobox.Items will be cleared upon changing language,
    // and ItemIndex will fall to -1
    // save TComboBox.ItemIndex
    idxSelectManga:=cbSelectManga.ItemIndex;
    idxLanguages := cbLanguages.ItemIndex;
    idxFilterStatus := cbFilterStatus.ItemIndex;
    idxOptionLetFMDDo := cbOptionLetFMDDo.ItemIndex;
    idxOptionProxyType := cbOptionProxyType.ItemIndex;
    idxDropTargetMode := rgDropTargetMode.ItemIndex;
    idxOptionCompress := rgOptionCompress.ItemIndex;
    idxOptionWebPConvertTo := cbWebPSaveAs.ItemIndex;
    idxOptionWebPPNGLevel := cbPNGCompressionLevel.ItemIndex;
    if SimpleTranslator.SetLangByIndex(cbLanguages.ItemIndex) then
    begin
      // assign new value
      lbOptionExternalParamsHint.Hint := Format(RS_LblOptionExternalParamsHint,
        [EXPARAM_PATH, EXPARAM_CHAPTER, EXPARAM_PATH, EXPARAM_CHAPTER]);
      lbOptionPDFQualityHint.Hint:=lbOptionPDFQuality.Hint;

      cbFilterStatus.Items.Text := RS_FilterStatusItems;
      cbOptionLetFMDDo.Items.Text := RS_OptionFMDDoItems;
      rgDropTargetMode.Items.Text := RS_DropTargetModeItems;
      rgOptionCompress.Items.Text := RS_OptionCompress;
      cbWebPSaveAs.Items.Text := RS_WebPConvertTo;
      cbPNGCompressionLevel.Items.Text := RS_WebPPNGLevel;

      // restore ItemIndex
      cbSelectManga.ItemIndex:=idxSelectManga;
      cbLanguages.ItemIndex := idxLanguages;
      cbFilterStatus.ItemIndex := idxFilterStatus;
      cbOptionLetFMDDo.ItemIndex := idxOptionLetFMDDo;
      cbOptionProxyType.ItemIndex := idxOptionProxyType;
      rgDropTargetMode.ItemIndex := idxDropTargetMode;
      rgOptionCompress.ItemIndex := idxOptionCompress;
      cbWebPSaveAs.ItemIndex := idxOptionWebPConvertTo;
      cbPNGCompressionLevel.ItemIndex := idxOptionWebPPNGLevel;
      Self.Repaint;
      vtMangaList.Repaint;
      tvDownloadFilterRefresh(True);

      // refresh custom option
      if not isStartup then
        WebsiteOptionCustomForm.CreateWebsiteOption;
    end;
  end;
end;

procedure TMainForm.OpenWithExternalProgramChapters(const Dir: String;
  const Chapters: TStrings);

  function FindSupportedOutputExt(const Dir, Filename: String): String;
  var
    ADir, SDir, s: String;
  begin
    Result := '';
    if Filename = '' then Exit;
    ADir := CorrectPathSys(Dir);
    if not DirectoryExistsUTF8(ADir) then Exit;
    for s in FMDSupportedPackedOutputExt do
    begin
      SDir := ChompPathDelim(CorrectPathSys(ADir + Filename));
      if FileExistsUTF8(SDir + s) then
      begin
        Result := GetLastDir(SDir) + s;
        Break;
      end;
    end;
    if Result = '' then
    begin
      ADir := CorrectPathSys(ADir + Filename);
      if DirectoryExistsUTF8(ADir) then
      Result := GetLastDir(ADir);
    end;
  end;

var
  ADir, AFilename: String;
  i: Integer;
  FindList: TStringList;
  SearchRec: TSearchRec;
begin
  if Dir = '' then Exit;
  ADir := CorrectPathSys(Dir);
  if Assigned(Chapters) then
    if Chapters.Count > 0 then
      for i := 0 to Chapters.Count - 1 do
      begin
        AFilename := FindSupportedOutputExt(ADir, Chapters[i]);
        if AFilename <> '' then
          Break;
      end;

  if AFilename = '' then
    try
      FindList := TStringList.Create;
      if FindFirstUTF8(ADir + '*', faAnyFile and faDirectory, SearchRec) = 0 then
        repeat
          FindList.Add(SearchRec.Name);
        until FindNextUTF8(SearchRec) <> 0;
      if FindList.Count >= 3 then
        AFilename := FindList.Strings[2]
      else
        AFilename := '';
      FindCloseUTF8(SearchRec);
    finally
      FindList.Free;
    end;
  OpenWithExternalProgram(ADir, AFilename);
end;

procedure TMainForm.OpenWithExternalProgram(const Dir, Filename: String);
var
  ADir, AParam, Exe, Params: String;
begin
  Exe := Trim(settingsfile.ReadString('general', 'ExternalProgramPath', ''));
  Params := Trim(settingsfile.ReadString('general', 'ExternalProgramParams', DEFAULT_EXPARAM));

  ADir := Trim(ChompPathDelim(CorrectPathSys(Dir)));
  AParam := Trim(ChompPathDelim(Filename));

  if Exe <> '' then
  begin
    if (Pos(EXPARAM_PATH + EXPARAM_CHAPTER, Params) <> 0) then
      AParam := PathDelim + AParam;
    Params := StringReplace(Params, EXPARAM_PATH, ADir, [rfIgnoreCase, rfReplaceAll]);
    Params := StringReplace(Params, EXPARAM_CHAPTER, AParam, [rfIgnoreCase, rfReplaceAll]);
    RunExternalProcess(Exe, Params, True, False);
  end
  else
  begin
    if (ADir <> '') and (AParam <> '') then
      AParam := ADir + PathDelim + AParam;
    OpenDocument(AParam);
  end;
end;

procedure TMainForm.TransferRateGraphInit(xCount: Integer);
var
  i: Integer;
begin
  TransferRateGraphList.Clear;
  TransferRateGraphArea.Legend.Format := FormatByteSize(0, True);
  for i:=1 to xCount do
    TransferRateGraphList.DataPoints.Add(IntToStr(i)+'|0|?|');
end;

procedure TMainForm.TransferRateGraphAddItem(TransferRate: Integer);
var
  i: Integer;
begin
  TransferRateGraphArea.Legend.Format := FormatByteSize(TransferRate, True);
  with TransferRateGraphList.DataPoints do
  begin
    for i := 0 to Count - 2 do
      Strings[i] := IntToStr(i+1)+'|'+ValueFromIndex[i+1];
    Strings[Count-1] := IntToStr(Count)+'|'+IntToStr(TransferRate)+'|?|';
  end;
end;

procedure TMainForm.DoExitWaitCounter;
begin
  Logger.Send(Self.ClassName+', Execute exit counter');
  if isUpdating then begin
    Logger.Send(Self.ClassName+', Update thread still exist, pending exit counter');
    isPendingExitCounter:=True
  end
  else tmExitCommand.Enabled:=True;
end;

procedure TMainForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
  SimpleException.ExceptionHandle(Sender, E);
end;

procedure TMainForm.vtMangaListInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  data: PMangaInfoData;
begin
  data := Sender.GetNodeData(Node);
  if dataProcess.GoToRecNo(Node^.Index) then
  with data^ do
  begin
    Link := dataProcess.Value[Node^.Index, DATA_PARAM_LINK];
    Title := dataProcess.Value[Node^.Index, DATA_PARAM_TITLE];
    AltTitles := dataProcess.Value[Node^.Index, DATA_PARAM_ALTTITLES];
    Authors := dataProcess.Value[Node^.Index, DATA_PARAM_AUTHORS];
    Artists := dataProcess.Value[Node^.Index, DATA_PARAM_ARTISTS];
    Genres := dataProcess.Value[Node^.Index, DATA_PARAM_GENRES];
    Status := dataProcess.Value[Node^.Index, DATA_PARAM_STATUS];
    NumChapter := dataProcess.ValueInt[Node^.Index, DATA_PARAM_NUMCHAPTER];
    JDN := dataProcess.ValueInt[Node^.Index, DATA_PARAM_JDN];
    Summary := dataProcess.Value[Node^.Index, DATA_PARAM_SUMMARY];
    TitleFormat := Title + ' (' + IntToStr(NumChapter) + ')';
    if dataProcess.FilterAllSites then
    begin
      Module := dataProcess.GetModule(Node^.Index);
      TitleFormat += ' [' + TModuleContainer(Module).Name + ']';
    end
    else
      Module := dataProcess.Module;
  end;
end;

procedure TMainForm.vtOptionMangaSiteSelectionFreeNode(
  Sender : TBaseVirtualTree; Node : PVirtualNode);
begin
  Finalize(PSingleItem(Sender.GetNodeData(Node))^);
end;

procedure TMainForm.vtOptionMangaSiteSelectionGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  vc: Integer;
  xNode: PVirtualNode;
begin
  if Node^.ChildCount = 0 then
    CellText := PNamePointerItem(vtOptionMangaSiteSelection.GetNodeData(Node))^.Name
  else
  begin
    vc := 0;
    xNode := Node^.FirstChild;
    while Assigned(xNode) do
    begin
      if vsVisible in xNode^.States then
        Inc(vc);
      xNode := xNode^.NextSibling;
    end;
    CellText := PNamePointerItem(vtOptionMangaSiteSelection.GetNodeData(Node))^.Name + ' (' + IntToStr(vc) + ')';
  end;
end;

end.
