unit frmAccountManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, VirtualTrees, WebsiteModules,
  HTTPSendThread, BaseThread, frmAccountSet, SimpleException;

type

  { TAccountManagerForm }

  TAccountManagerForm = class(TForm)
    btRefresh: TBitBtn;
    btEdit: TBitBtn;
    pnBtContainer: TPanel;
    vtAccountList: TVirtualStringTree;
    procedure btEditClick(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtAccountListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtAccountListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
    procedure vtAccountListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtAccountListDblClick(Sender: TObject);
    procedure vtAccountListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vtAccountListHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtAccountListPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private

  public
    procedure LoadAccounts;
    procedure SortList;
  end;

  TAccountCheck = class;

  { TAccountCheckThread }

  TAccountCheckThread = class(TBaseThread)
  private
    fmodule: TModuleContainer;
    fthreadlist: TAccountCheck;
    fhttp: THTTPSendThread;
    procedure SyncStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(const AModule: TModuleContainer; AThreadList: TAccountCheck);
    destructor Destroy; override;
  end;

  TAccountCheck = class(TThreadList)
  public
    destructor Destroy; override;
    procedure StopAll;
  end;

var
  AccountManagerForm: TAccountManagerForm;

const
  CL_HLRedMarks = $008080FF;

resourcestring
  RS_Unknown = 'Unknown';
  RS_Checking = 'Checking';
  RS_Valid = 'OK';
  RS_Invalid = 'Invalid';
  RS_AccountDeleteConfirmation = 'Are you sure you want to delete this account?';

implementation

uses frmCustomColor, math;

var
  AccountThreadList: TAccountCheck;

{$R *.lfm}

{ TAccountCheck }

destructor TAccountCheck.Destroy;
begin
  StopAll;
  while LockList.Count <> 0 do
  begin
    UnlockList;
    Sleep(250);
  end;
  inherited Destroy;
end;

procedure TAccountCheck.StopAll;
var
  i: Integer;
begin
  with LockList do
    try
      for i := 0 to Count - 1 do
        TAccountCheckThread(Items[i]).Terminate;
    finally
      UnlockList;
    end;
end;

{ TAccountCheckThread }

procedure TAccountCheckThread.SyncStatus;
begin
  AccountManagerForm.vtAccountList.Repaint;
end;

procedure TAccountCheckThread.Execute;
begin
  try
    fmodule.Account.Status := asChecking;
    Synchronize(@SyncStatus);
    fmodule.OnLogin(fhttp, fmodule);
    Synchronize(@SyncStatus);
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

constructor TAccountCheckThread.Create(const AModule: TModuleContainer;
  AThreadList: TAccountCheck);
begin
  inherited Create(False);
  FreeOnTerminate:=True;
  fmodule:=AModule;
  fthreadlist:=AThreadList;
  fthreadlist.Add(Self);
  fhttp:=THTTPSendThread.Create(Self);
  fmodule.PrepareHTTP(fhttp);
end;

destructor TAccountCheckThread.Destroy;
begin
  if fthreadlist<>nil then
    fthreadlist.Remove(Self);
  fhttp.Free;
  inherited Destroy;
end;

{ TAccountManagerForm }

procedure TAccountManagerForm.vtAccountListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Node^.CheckState = csUncheckedNormal then
    TargetCanvas.Font.Color := clGrayText;
end;

procedure TAccountManagerForm.LoadAccounts;
var
  i: Integer;
  node: PVirtualNode;
begin
  vtAccountList.BeginUpdate;
  vtAccountList.Clear;
  vtAccountList.NodeDataSize:=SizeOf(TModuleContainer);
  for i:=0 to Modules.Count-1 do
    with Modules[i] do
      if Account<>nil then
      begin
        node:=vtAccountList.AddChild(nil, Modules[i]);
        node^.CheckType:=ctCheckBox;
        if Account.Enabled then
          node^.CheckState:=csCheckedNormal
        else
          node^.CheckState:=csUncheckedNormal;
      end;
  vtAccountList.EndUpdate;
end;

procedure TAccountManagerForm.SortList;
begin
  with vtAccountList do
    Sort(nil, Header.SortColumn, Header.SortDirection, False);
end;

procedure TAccountManagerForm.vtAccountListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  m: TModuleContainer;
begin
  m:=PModuleContainer(Sender.GetNodeData(Node))^;
  case Column of
    1:CellText:=m.Name;
    2:CellText:=m.Account.Username;
    3: case m.Account.Status of
         asUnknown:CellText:=RS_Unknown;
         asChecking:CellText:=RS_Checking;
         asValid:CellText:=RS_Valid;
         asInvalid:CellText:=RS_Invalid;
       end;
  end;
end;

procedure TAccountManagerForm.vtAccountListHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then
    Sender.SortColumn := HitInfo.Column
  else
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  SortList;
end;

procedure TAccountManagerForm.vtAccountListBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if PModuleContainer(Sender.GetNodeData(Node))^.Account.Status = asInvalid then
  begin
    TargetCanvas.Brush.Color:=CL_HLRedMarks;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TAccountManagerForm.FormCreate(Sender: TObject);
begin
  AddVT(vtAccountList);
  AccountThreadList:=TAccountCheck.Create;
end;

procedure TAccountManagerForm.FormDestroy(Sender: TObject);
begin
  if AccountThreadList<>nil then
    AccountThreadList.Free;
  RemoveVT(vtAccountList);
end;

procedure TAccountManagerForm.btEditClick(Sender: TObject);
var
  m: TModuleContainer;
begin
  if vtAccountList.SelectedCount=0 then Exit;
  m:=PModuleContainer(vtAccountList.GetNodeData(vtAccountList.GetFirstSelected))^;
  with TAccountSetForm.Create(Self) do
    try
      Caption:=m.Name;
      with m.Account do
      begin
        edUsername.Text:=Username;
        edPassword.Text:=Password;
        if ShowModal = mrOK then
        begin
          if (edUsername.Text <> Username) or (edPassword.Text <> Password) then
          begin
            Username:=edUsername.Text;
            Password:=edPassword.Text;
            btRefreshClick(btRefresh);
          end;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TAccountManagerForm.btRefreshClick(Sender: TObject);
var
  m: TModuleContainer;
begin
  if vtAccountList.SelectedCount = 0 then Exit;
  m:=PModuleContainer(vtAccountList.GetNodeData(vtAccountList.GetFirstSelected()))^;
  if m.Account.Status=asChecking then Exit;
  TAccountCheckThread.Create(m,AccountThreadList);
end;

procedure TAccountManagerForm.vtAccountListChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  m: TModuleContainer;
begin
  m := PModuleContainer(Sender.GetNodeData(Node))^;
  m.Account.Enabled:=Node^.CheckState=csCheckedNormal;
  if m.OnAccountState<>nil then m.OnAccountState(m);
end;

procedure TAccountManagerForm.vtAccountListCompareNodes(
  Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  m1, m2: TModuleContainer;
begin
  m1:=PModuleContainer(Sender.GetNodeData(Node1))^;
  m2:=PModuleContainer(Sender.GetNodeData(Node2))^;
  case Column of
    0: if m1.Account.Enabled=m2.Account.Enabled then
         Result:=AnsiCompareStr(m1.Name,m2.Name)
       else
         Result:=ifthen(m1.Account.Enabled>m2.Account.Enabled,1,-1);
    1:Result:=AnsiCompareStr(m1.Name,m2.Name);
    2:Result:=AnsiCompareStr(m1.Account.Username,m2.Account.Username);
    3: if m1.Account.Status=m2.Account.Status then
         Result:=AnsiCompareStr(m1.Name,m2.Name)
       else
         Result:=ifthen(m1.Account.Status>m2.Account.Status,1,-1);
  end;
end;

procedure TAccountManagerForm.vtAccountListDblClick(Sender: TObject);
begin
  if vtAccountList.SelectedCount=0 then Exit;
  btEditClick(btEdit);
end;

end.

