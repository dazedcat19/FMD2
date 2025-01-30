unit frmTransferFavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, Menus, ExtCtrls,
  VirtualTrees, uFavoritesManager, DBDataProcess;

type

  { TTransferFavoritesForm }

  TTransferFavoritesForm = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    cbWebsites: TComboBox;
    ckClearDownloadedChapters: TCheckBox;
    imgState: TImage;
    imgsState: TImageList;
    lbTransferTo: TLabel;
    rbAll: TRadioButton;
    rbValid: TRadioButton;
    rbInvalid: TRadioButton;
    vtFavs: TVirtualStringTree;
    procedure btCancelClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure cbWebsitesEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAllChange(Sender: TObject);
    procedure vtFavsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFavsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtFavsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String
      );
  private
    FAllCount,
    FValidCount,
    FInvalidCount,
    FLastFilter,
    FLastWebsiteSelect: Integer;
    procedure UpdateFilterCount;
    procedure FilterState(const AState: Integer = 0);
    procedure FindMatchTitle;
  public
    procedure AddFav(const AFav: TFavoriteContainer);
  end;

  TFavsContainer = record
    Fav: TFavoriteContainer;
    NewModule: Pointer;
    NewLink: String;
    State: Integer;
  end;

  PFavContainer = ^TFavsContainer;

  { TFindMatchDBThread }

  TFindMatchDBThread = class(TThread)
  private
    FOwner: TTransferFavoritesForm;
    FWebsite: String;
    procedure SyncBegin;
    procedure SyncEnd;
  protected
    procedure Execute; override;
  public
    constructor Create(const AWebsite: String; const AOwner: TTransferFavoritesForm);
  end;

var
  TransferFavoritesForm: TTransferFavoritesForm;

resourcestring
  RS_ALL = 'All';
  RS_Valid = 'Valid';
  RS_Invalid = 'Invalid';

implementation

uses
  FMDVars, FMDOptions, WebsiteModules, db, frmCustomColor;

{$R *.lfm}

{ TFindMatchDBThread }

procedure TFindMatchDBThread.SyncBegin;
begin
  FOwner.imgState.Visible := True;
  FOwner.imgsState.GetBitmap(0,FOwner.imgState.Picture.Bitmap);
  FOwner.cbWebsites.Enabled := False;
  FOwner.vtFavs.Enabled := False;
end;

procedure TFindMatchDBThread.SyncEnd;
begin
  FOwner.imgsState.GetBitmap(1,FOwner.imgState.Picture.Bitmap);
  FOwner.cbWebsites.Enabled := True;
  FOwner.vtFavs.Enabled := True;
  FOwner.cbWebsites.SetFocus;

  FOwner.UpdateFilterCount;

  if FOwner.FLastFilter <> 0 then
  begin
    FOwner.FLastFilter := -1;
    FOwner.rbAllChange(nil);
  end;
end;

procedure TFindMatchDBThread.Execute;
var
  db: TDBDataProcess;
  node: PVirtualNode;
  data: PFavContainer;
  module: TModuleContainer;
begin
  Synchronize(@SyncBegin);
  FOwner.FValidCount := 0;
  FOwner.FInvalidCount := 0;
  db := TDBDataProcess.Create;
  try
    if db.Connect(FWebsite) then
    begin
      module := Modules.LocateModule(FWebsite);
      db.Table.ReadOnly := True;
      node := FOwner.vtFavs.GetFirst();
      while Assigned(node) do
      begin
        data := FOwner.vtFavs.GetNodeData(node);
        try
          db.Table.SQL.Text := 'SELECT link FROM ' + AnsiQuotedStr(db.TableName, '"') +
            ' WHERE (title LIKE ' + AnsiQuotedStr(data^.Fav.FavoriteInfo.Title, '"') +
            ' OR LOWER(alttitles) REGEXP LOWER(' + AnsiQuotedStr(db.RegexEscapeAltTitles(data^.Fav.FavoriteInfo.Title), '"') + ')' +
            ') COLLATE NOCASE;';
          db.Table.Open;
          if db.Table.RecNo > 0 then
          begin
            data^.NewModule := module;
            data^.NewLink := db.Table.Fields[0].AsString;
            data^.State := 1;
            Inc(FOwner.FValidCount);
          end
          else
          begin
            data^.NewModule := nil;
            data^.NewLink := '';
            data^.State := 2;
            Inc(FOwner.FInvalidCount);
          end;
        except
        end;
        db.Table.Close;
        node := FOwner.vtFavs.GetNext(node);
      end;
    end;
    db.Connection.Connected := False;
  finally
    db.Free;
  end;
  Synchronize(@SyncEnd);
end;

constructor TFindMatchDBThread.Create(const AWebsite: String;
  const AOwner: TTransferFavoritesForm);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FWebsite := AWebsite;
  FOwner := AOwner;
end;

{ TTransferFavoritesForm }

procedure TTransferFavoritesForm.FormCreate(Sender: TObject);
begin
  frmCustomColor.AddVT(vtFavs);
  cbWebsites.Items.Assign(FormMain.cbSelectManga.Items);
  FLastFilter := 0;
  FAllCount := 0;
  FValidCount := 0;
  FInvalidCount := 0;
  FLastWebsiteSelect := -1;
  vtFavs.NodeDataSize := SizeOf(TFavsContainer);;
end;

procedure TTransferFavoritesForm.FormDestroy(Sender: TObject);
begin
  frmCustomColor.RemoveVT(vtFavs);
end;

procedure TTransferFavoritesForm.FormShow(Sender: TObject);
begin
  UpdateFilterCount;
end;

procedure TTransferFavoritesForm.rbAllChange(Sender: TObject);
begin
  if rbAll.Checked then
    FilterState(0)
  else if rbValid.Checked then
    FilterState(1)
  else if rbInvalid.Checked then
    FilterState(2);
end;

procedure TTransferFavoritesForm.btOKClick(Sender: TObject);
var     
  db: TDBDataProcess;
  Node: PVirtualNode;
  Data: PFavContainer;
  dc: String;
  t: TFavoriteContainer;
  m: TModuleContainer;
begin
  FavoriteManager.Lock;
  try
    db := TDBDataProcess.Create;
    m := TModuleContainer(cbWebsites.Items.Objects[cbWebsites.ItemIndex]);
    try
      if db.Connect(m.ID) then
      begin
        db.Table.ReadOnly := True;
        Node := vtFavs.GetFirst();
        while Assigned(Node) do
        begin
          Data := vtFavs.GetNodeData(Node);
          // add new item and remove the old one
          if Assigned(Data^.NewModule) then
          begin
            with Data^.Fav.FavoriteInfo do
            begin
              if ckClearDownloadedChapters.Checked then
              begin
                dc := ''
              end
              else
              begin
                dc := DownloadedChapterList;
              end;

              if ModuleID <> db.Website then
              begin
                FavoriteManager.Add(
                  Data^.NewModule,
                  Title,
                  Status,
                  CurrentChapter,
                  dc,
                  SaveTo,
                  Data^.NewLink);
              end
              else
              begin
                FavoriteManager.Replace(
                  Data^.Fav.ID,
                  Data^.NewModule,
                  Title,
                  Status,
                  CurrentChapter,
                  dc,
                  SaveTo,
                  Data^.NewLink);
              end;
            end;
            t := FavoriteManager.Items.Last;
            if Data^.Fav.FavoriteInfo.ModuleID <> db.Website then
            begin
              FavoriteManager.Remove(Data^.Fav);
            end
            else
            begin
              FavoriteManager.Items.Remove(Data^.Fav);
              FavoriteManager.UpdateOrder;
            end;
            Data^.Fav := t;
            if ckClearDownloadedChapters.Checked then
            begin
              t.Tag := 100; // get new chapterlist
            end;
          end;
          db.Table.Close;
          Node := vtFavs.GetNext(Node);
        end;
        db.Connection.Connected := False;
        ModalResult := mrOK;
      end;
    finally
      db.Free;
    end;
  finally
    FavoriteManager.UnLock;
  end;
end;

procedure TTransferFavoritesForm.cbWebsitesEditingDone(Sender: TObject);
begin
  FindMatchTitle;
end;

procedure TTransferFavoritesForm.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTransferFavoritesForm.vtFavsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PFavContainer;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure TTransferFavoritesForm.vtFavsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PFavContainer;
begin
  if Column <> 0 then Exit;
  Data := Sender.GetNodeData(Node);
  if Data^.State = 0 then
    ImageIndex := -1
  else
    ImageIndex := Data^.State;
end;

procedure TTransferFavoritesForm.vtFavsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PFavContainer;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    1: CellText := Data^.Fav.FavoriteInfo.Title;
    2: CellText := Data^.Fav.FavoriteInfo.Website;
  end;
end;

procedure TTransferFavoritesForm.UpdateFilterCount;
begin
  rbAll.Caption := RS_ALL + '(' + IntToStr(FAllCount) + ')';
  rbValid.Caption := RS_Valid + '(' + IntToStr(FValidCount) + ')';
  rbInvalid.Caption := RS_Invalid + '(' + IntToStr(FInvalidCount) + ')';
end;

procedure TTransferFavoritesForm.FilterState(const AState: Integer);
var
  Node: PVirtualNode;
  Data: PFavContainer;
begin
  if FLastFilter = AState then Exit;
  try
    vtFavs.BeginUpdate;
    Node := vtFavs.GetFirst();
    while Assigned(Node) do
    begin
      Data := vtFavs.GetNodeData(Node);
      if AState = 0 then
        vtFavs.IsVisible[Node] := True
      else
        vtFavs.IsVisible[Node] := Data^.State = AState;
      Node := vtFavs.GetNext(Node);
    end;
  finally
    vtFavs.EndUpdate;
  end;
  FLastFilter := AState;
end;

procedure TTransferFavoritesForm.FindMatchTitle;
var
  m: TModuleContainer;
begin
  if FLastWebsiteSelect = cbWebsites.ItemIndex then Exit;
  FLastWebsiteSelect := cbWebsites.ItemIndex;
  m := TModuleContainer(cbWebsites.Items.Objects[FLastWebsiteSelect]);
  if FileExists(DBDataFilePath(m.ID)) then
    TFindMatchDBThread.Create(m.ID, Self);
end;

procedure TTransferFavoritesForm.AddFav(const AFav: TFavoriteContainer);
var
  Node: PVirtualNode;
  Data: PFavContainer;
begin
  Node := vtFavs.AddChild(nil);
  Data := vtFavs.GetNodeData(Node);
  Data^.Fav := AFav;
  Data^.NewLink := '';
  Data^.State := 0;
  Inc(FAllCount);
end;

end.

