unit uVars;

{$mode objfpc}{$H+}

interface

uses
  frmMain, uDownloadsManager, uFavoritesManager, uUpdateThread, DBDataProcess,
  uSilentThread, uBaseUnit, uGetMangaInfosThread, CheckUpdate, uOptions,
  DBUpdater, SelfUpdater, FileChannel, simpleipc;

var
  FormMain: TMainForm;

  isRunDownloadFilter,
  isUpdating,
  isPendingExitCounter,
  isNormalExit: Boolean;
  isStartup: Boolean = True;

  //Instance
  FMDInstance: TSimpleIPCServer;

  // update fmd through main thread
  IsDlgCounter: Boolean = False;

  // file logger
  FileLogger: TFileChannel;

  // download manager
  DLManager: TDownloadManager;

  // favorite manager
  FavoriteManager: TFavoriteManager;

  // main dataprocess for manga list
  dataProcess: TDBDataProcess;

  // update manga list thread manager
  updateList: TUpdateListManagerThread;

  // dbupdater thread
  DBUpdaterThread: TDBUpdaterThread;

  // silent thread for download all or add to favorite
  SilentThreadManager: TSilentThreadManager;

  // get manga info
  mangaInfo: TMangaInfo;
  GetInfosThread: TGetMangaInfosThread;

  // check update thread
  CheckUpdateThread: TCheckUpdateThread;

  // self updater thread
  SelfUpdaterThread: TSelfUpdaterThread;

implementation

end.

