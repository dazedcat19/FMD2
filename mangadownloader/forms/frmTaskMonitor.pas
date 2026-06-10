unit frmTaskMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LCLIntf,
  uDownloadsManager, uBaseUnit;

type

  TChildSnapshot = record
    ThreadNum : Integer;
    PageInfo  : String;
    Status    : String;
  end;
  TChildSnapshots = array of TChildSnapshot;

  TTaskSnapshot = record
    Title           : String;
    Website         : String;
    Status          : TDownloadStatusType;
    StatusText      : String;
    StatusDetail    : String;   // live DownloadInfo.Status string e.g. "[3/12] Converting"
    Progress        : String;
    CurrentChapter  : String;
    ChapterProgress : String;
    SaveTo          : String;
    TransferRate    : String;
    Children        : TChildSnapshots;
  end;
  TTaskSnapshots = array of TTaskSnapshot;

  { TSnapshotThread }

  TSnapshotThread = class(TThread)
  private
    FSnapshots : TTaskSnapshots;
    FCS        : TRTLCriticalSection;
    FHasNew    : Boolean;
  protected
    procedure Execute; override;
  public
    Manager : TDownloadManager;
    constructor Create;
    destructor  Destroy; override;
    function TryGetSnapshots(out ASnapshots: TTaskSnapshots): Boolean;
  end;

  { TFormTaskMonitor }

  TFormTaskMonitor = class(TForm)
    btnClose    : TBitBtn;
    ckStayOnTop : TCheckBox;
    lblNoTasks  : TLabel;
    pnlTop      : TPanel;
    ScrollBox   : TScrollBox;
    tmRefresh   : TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure ckStayOnTopChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure tmRefreshTimer(Sender: TObject);
  private
    FThread     : TSnapshotThread;
    FManager    : TDownloadManager;
    FTaskPanels : array of TPanel;
    procedure RebuildUI(const ASnaps: TTaskSnapshots);
    procedure ClearTaskPanels;
    function  StatusToColor(S: TDownloadStatusType): TColor;
    function  StatusToStr(S: TDownloadStatusType): String;
  public
    procedure StartMonitoring(AManager: TDownloadManager);
    procedure StopMonitoring;
  end;

var
  FormTaskMonitor: TFormTaskMonitor;

implementation

{$R *.lfm}

const
  POLL_MS = 800;

  STATUS_STR : array[TDownloadStatusType] of String = (
    'Stopped', 'Waiting', 'Preparing', 'Downloading',
    'Finished', 'Converting', 'Compressing', 'Problem', 'Failed', '');

{ ------------------------------------------------------------------ }
{ TSnapshotThread                                                     }
{ ------------------------------------------------------------------ }

constructor TSnapshotThread.Create;
begin
  inherited Create(True {suspended});
  InitCriticalSection(FCS);
  FreeOnTerminate := False;
end;

destructor TSnapshotThread.Destroy;
begin
  DoneCriticalSection(FCS);
  inherited;
end;

procedure TSnapshotThread.Execute;
var
  Snap     : TTaskSnapshots;
  i, j, k  : Integer;
  C        : TTaskContainer;
  TT       : TTaskThread;
  DT       : TDownloadThread;
  ChPtr    : Integer;
  NThreads : Integer;
begin
  while not Terminated do
  begin
    SetLength(Snap, 0);

    if Assigned(Manager) then
    begin
      // Use CS_Task to safely walk the full Items list
      EnterCriticalSection(Manager.CS_Task);
      try
        SetLength(Snap, Manager.Items.Count);
        j := 0;
        for i := 0 to Manager.Items.Count - 1 do
        begin
          C := Manager.Items[i];

          // Show all non-finished, non-stopped tasks (or running ones)
          if C.Status in [STATUS_STOP, STATUS_FINISH, STATUS_NONE] then
            Continue;

          Snap[j].Title        := C.DownloadInfo.Title;
          Snap[j].Website      := C.DownloadInfo.Website;
          Snap[j].Status       := C.Status;
          Snap[j].StatusText   := STATUS_STR[C.Status];
          Snap[j].StatusDetail := C.DownloadInfo.Status;
          Snap[j].Progress     := C.DownloadInfo.Progress;
          Snap[j].SaveTo       := C.DownloadInfo.SaveTo;
          Snap[j].TransferRate := C.DownloadInfo.TransferRate;

          // Safe chapter pointer read
          ChPtr := C.CurrentDownloadChapterPtr;
          if (C.ChapterLinks.Count > 0) and (ChPtr >= 0) and
             (ChPtr < C.ChapterNames.Count) then
            Snap[j].CurrentChapter := C.ChapterNames[ChPtr]
          else
            Snap[j].CurrentChapter := '';

          Snap[j].ChapterProgress := Format('%d / %d', [
            ChPtr + 1, C.ChapterLinks.Count]);

          // Child threads — CS_Task is held so TaskThread won't be freed under us.
          // We read Threads.Count speculatively; individual WorkId reads are atomic ints.
          TT := C.TaskThread;
          SetLength(Snap[j].Children, 0);
          if Assigned(TT) then
          begin
            NThreads := TT.Threads.Count;
            if NThreads > 0 then
            begin
              // Active download/link-fetch threads
              SetLength(Snap[j].Children, NThreads);
              for k := 0 to NThreads - 1 do
              begin
                DT := TT.Threads[k];
                Snap[j].Children[k].ThreadNum := k + 1;
                Snap[j].Children[k].PageInfo  := Format('Page %d', [DT.WorkId + 1]);
                // IsConverting is set True by SaveImageStreamToFile while ConvertStream runs
                if DT.IsConverting then
                  Snap[j].Children[k].Status := 'Converting'
                else
                  case TT.Flag of
                    CS_GETPAGENUMBER : Snap[j].Children[k].Status := 'Getting page count';
                    CS_GETPAGELINK   : Snap[j].Children[k].Status := 'Getting link';
                    CS_DOWNLOAD      : Snap[j].Children[k].Status := 'Downloading';
                  else
                    Snap[j].Children[k].Status := STATUS_STR[C.Status];
                  end;
              end;
            end
            else if C.Status in [STATUS_COMPRESS, STATUS_CONVERT] then
            begin
              // No child threads during convert/compress — TTaskThread does it itself.
              // Show one synthetic row so the user sees what phase is running.
              // DownloadInfo.Status contains the live string e.g. "[3/12] Converting (Ch.1)"
              // which already has RS_Converting or RS_Compressing embedded in it.
              SetLength(Snap[j].Children, 1);
              Snap[j].Children[0].ThreadNum := 0;   // 0 = synthetic, not a real thread
              Snap[j].Children[0].PageInfo  := '';
              // Derive phase label from the live status string:
              // RS_Converting / RS_Compressing are localised, but C.Status is reliable.
              if C.Status = STATUS_CONVERT then
                Snap[j].Children[0].Status := 'Converting'
              else
              begin
                // STATUS_COMPRESS covers both ImageMagick conversion (sets "Converting..."
                // in DownloadInfo.Status) and actual compression (sets "Compressing...").
                if Pos('Converting', C.DownloadInfo.Status) > 0 then
                  Snap[j].Children[0].Status := 'Converting (ImageMagick)'
                else
                  Snap[j].Children[0].Status := 'Compressing';
              end;
            end;
          end;

          Inc(j);
        end;
        SetLength(Snap, j);  // trim to actual count
      finally
        LeaveCriticalSection(Manager.CS_Task);
      end;
    end;

    // Publish
    EnterCriticalSection(FCS);
    try
      FSnapshots := Snap;
      FHasNew    := True;
    finally
      LeaveCriticalSection(FCS);
    end;

    Sleep(POLL_MS);
  end;
end;

function TSnapshotThread.TryGetSnapshots(out ASnapshots: TTaskSnapshots): Boolean;
begin
  Result := False;
  if TryEnterCriticalSection(FCS) = 0 then Exit;
  try
    if not FHasNew then Exit;
    ASnapshots := FSnapshots;
    FHasNew    := False;
    Result     := True;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

{ ------------------------------------------------------------------ }
{ TFormTaskMonitor                                                    }
{ ------------------------------------------------------------------ }

procedure TFormTaskMonitor.FormCreate(Sender: TObject);
begin
  FThread  := nil;
  FManager := nil;
  SetLength(FTaskPanels, 0);
end;

procedure TFormTaskMonitor.FormDestroy(Sender: TObject);
begin
  StopMonitoring;
end;

procedure TFormTaskMonitor.FormHide(Sender: TObject);
begin
  StopMonitoring;
end;

procedure TFormTaskMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TFormTaskMonitor.btnCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormTaskMonitor.ckStayOnTopChange(Sender: TObject);
begin
  if ckStayOnTop.Checked then FormStyle := fsStayOnTop
  else                        FormStyle := fsNormal;
end;

procedure TFormTaskMonitor.tmRefreshTimer(Sender: TObject);
var
  Snaps: TTaskSnapshots;
begin
  if not Assigned(FThread) then Exit;
  if FThread.TryGetSnapshots(Snaps) then
    RebuildUI(Snaps);
end;

procedure TFormTaskMonitor.StartMonitoring(AManager: TDownloadManager);
begin
  FManager := AManager;
  // If thread already running (window was just reshown), keep it going
  if Assigned(FThread) then
  begin
    FThread.Manager := AManager;
    tmRefresh.Enabled := True;
    Exit;
  end;
  FThread         := TSnapshotThread.Create;
  FThread.Manager := AManager;
  FThread.Start;
  tmRefresh.Enabled := True;
end;

procedure TFormTaskMonitor.StopMonitoring;
begin
  tmRefresh.Enabled := False;
  if not Assigned(FThread) then Exit;
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

function TFormTaskMonitor.StatusToStr(S: TDownloadStatusType): String;
begin
  Result := STATUS_STR[S];
end;

function TFormTaskMonitor.StatusToColor(S: TDownloadStatusType): TColor;
begin
  case S of
    STATUS_DOWNLOAD            : Result := RGBToColor(0, 102, 204);
    STATUS_CONVERT, STATUS_COMPRESS : Result := RGBToColor(120, 0, 180);
    STATUS_FINISH              : Result := RGBToColor(16, 124, 16);
    STATUS_PROBLEM, STATUS_FAILED   : Result := RGBToColor(196, 43, 28);
    STATUS_WAIT, STATUS_PREPARE     : Result := RGBToColor(90, 90, 90);
  else
    Result := clWindowText;
  end;
end;

procedure TFormTaskMonitor.ClearTaskPanels;
var
  i: Integer;
begin
  for i := 0 to High(FTaskPanels) do
    FreeAndNil(FTaskPanels[i]);
  SetLength(FTaskPanels, 0);
end;

procedure TFormTaskMonitor.RebuildUI(const ASnaps: TTaskSnapshots);
const
  MARGIN  = 6;
  PADDING = 4;
  ROW_H   = 20;
var
  i, j      : Integer;
  TaskPnl   : TPanel;
  Lbl       : TLabel;
  YOff      : Integer;
  PnlH      : Integer;
  CurTop    : Integer;
  PnlW      : Integer;
  ChildStr  : String;
begin
  // Freeze layout while we rebuild
  ScrollBox.DisableAlign;
  try
    ClearTaskPanels;
    SetLength(FTaskPanels, Length(ASnaps));

    lblNoTasks.Visible := Length(ASnaps) = 0;
    if Length(ASnaps) = 0 then Exit;

    CurTop := MARGIN;
    PnlW   := ScrollBox.ClientWidth - MARGIN * 2;
    if PnlW < 100 then PnlW := 100;

    for i := 0 to High(ASnaps) do
    begin
      // Panel height: 3 fixed rows + 1 per child thread + padding
      PnlH := PADDING + (3 + Length(ASnaps[i].Children)) * ROW_H + PADDING;

      TaskPnl            := TPanel.Create(ScrollBox);
      TaskPnl.Parent     := ScrollBox;
      TaskPnl.Left       := MARGIN;
      TaskPnl.Top        := CurTop;
      TaskPnl.Width      := PnlW;
      TaskPnl.Height     := PnlH;
      TaskPnl.BevelOuter := bvRaised;
      TaskPnl.Caption    := '';
      FTaskPanels[i]     := TaskPnl;

      YOff := PADDING;

      // Row 1 — Title (left) + Status (right)
      Lbl             := TLabel.Create(TaskPnl);
      Lbl.Parent      := TaskPnl;
      Lbl.Left        := PADDING;
      Lbl.Top         := YOff;
      Lbl.Width       := PnlW - 130 - PADDING * 2;
      Lbl.Height      := ROW_H;
      Lbl.AutoSize    := False;
      Lbl.Font.Style  := [fsBold];
      Lbl.Font.Color  := StatusToColor(ASnaps[i].Status);
      Lbl.Caption     := ASnaps[i].Title;
      Lbl.ShowHint    := True;
      Lbl.Hint        := ASnaps[i].Title;

      Lbl             := TLabel.Create(TaskPnl);
      Lbl.Parent      := TaskPnl;
      Lbl.Left        := PnlW - 126;
      Lbl.Top         := YOff;
      Lbl.Width       := 120;
      Lbl.Height      := ROW_H;
      Lbl.AutoSize    := False;
      Lbl.Alignment   := taRightJustify;
      Lbl.Font.Style  := [fsBold];
      Lbl.Font.Color  := StatusToColor(ASnaps[i].Status);
      Lbl.Caption     := '[' + ASnaps[i].Website + ']  ' + ASnaps[i].StatusText;

      Inc(YOff, ROW_H);

      // Row 2 — Chapter progress
      Lbl             := TLabel.Create(TaskPnl);
      Lbl.Parent      := TaskPnl;
      Lbl.Left        := PADDING;
      Lbl.Top         := YOff;
      Lbl.Width       := PnlW - PADDING * 2;
      Lbl.Height      := ROW_H;
      Lbl.AutoSize    := False;
      Lbl.Font.Color  := clGrayText;
      if ASnaps[i].CurrentChapter <> '' then
        Lbl.Caption := 'Chapter ' + ASnaps[i].ChapterProgress +
                       '  —  ' + ASnaps[i].CurrentChapter
      else
        Lbl.Caption := 'Chapter ' + ASnaps[i].ChapterProgress;

      Inc(YOff, ROW_H);

      // Row 3 — Pages progress + transfer rate
      Lbl             := TLabel.Create(TaskPnl);
      Lbl.Parent      := TaskPnl;
      Lbl.Left        := PADDING;
      Lbl.Top         := YOff;
      Lbl.Width       := PnlW - PADDING * 2;
      Lbl.Height      := ROW_H;
      Lbl.AutoSize    := False;
      Lbl.Font.Color  := clGrayText;
      Lbl.Caption     := 'Pages: ' + ASnaps[i].Progress +
                         '    ' + ASnaps[i].TransferRate +
                         '    ' + ASnaps[i].SaveTo;
      Lbl.ShowHint    := True;
      Lbl.Hint        := ASnaps[i].SaveTo;

      Inc(YOff, ROW_H);

      // Child thread rows
      for j := 0 to High(ASnaps[i].Children) do
      begin
        // ThreadNum=0 means synthetic row (convert/compress phase, no real threads)
        if ASnaps[i].Children[j].ThreadNum = 0 then
          ChildStr := '  ● ' + ASnaps[i].Children[j].Status
        else if j < High(ASnaps[i].Children) then
          ChildStr := Format('  ├ Thread #%d  —  %s  [%s]',
            [ASnaps[i].Children[j].ThreadNum,
             ASnaps[i].Children[j].PageInfo,
             ASnaps[i].Children[j].Status])
        else
          ChildStr := Format('  └ Thread #%d  —  %s  [%s]',
            [ASnaps[i].Children[j].ThreadNum,
             ASnaps[i].Children[j].PageInfo,
             ASnaps[i].Children[j].Status]);

        Lbl             := TLabel.Create(TaskPnl);
        Lbl.Parent      := TaskPnl;
        Lbl.Left        := PADDING;
        Lbl.Top         := YOff;
        Lbl.Width       := PnlW - PADDING * 2;
        Lbl.Height      := ROW_H;
        Lbl.AutoSize    := False;
        // Color by what the thread/phase is doing
        case ASnaps[i].Children[j].Status of
          'Downloading'              : Lbl.Font.Color := RGBToColor(0, 80, 160);
          'Getting link'             : Lbl.Font.Color := RGBToColor(0, 120, 100);
          'Getting page count'       : Lbl.Font.Color := RGBToColor(90, 90, 90);
          'Converting (ImageMagick)',
          'Converting'               : Lbl.Font.Color := RGBToColor(120, 0, 180);
          'Compressing'              : Lbl.Font.Color := RGBToColor(160, 80, 0);
        else
          Lbl.Font.Color := RGBToColor(0, 80, 160);
        end;
        Lbl.Caption     := ChildStr;

        Inc(YOff, ROW_H);
      end;

      Inc(CurTop, PnlH + MARGIN);
    end;
  finally
    ScrollBox.EnableAlign;
  end;
end;

end.
