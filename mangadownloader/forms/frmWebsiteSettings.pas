unit frmWebsiteSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WebsiteModules, VirtualPropertyGrid, frmCustomColor, Forms, LCLType,
  Controls, PairSplitter, EditBtn, VirtualTrees, uBaseUnit, Graphics, uCustomControls;

type

  { TWebsiteSettingsForm }

  TWebsiteSettingsForm = class(TForm)
    edSearch: TCustomEditButton;
    edSearchProperty: TCustomEditButton;
    spMain: TPairSplitter;
    spList: TPairSplitterSide;
    spProps: TPairSplitterSide;
    vtWebsite: TVirtualStringTree;
    procedure edSearchButtonClick(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure edSearchPropertyButtonClick(Sender: TObject);
    procedure edSearchPropertyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtWebsiteBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtWebsiteCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtWebsiteFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtWebsiteGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure SettingsViewPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure SettingsViewKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure SettingsViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private 
    FWebsiteSettingsNode: PVirtualNode;
  public
    SettingsView: TVirtualPropertyGrid;
    procedure LoadWebsiteSettings;
  end;

var
  WebsiteSettingsForm: TWebsiteSettingsForm;

implementation

uses FMDOptions;

{$R *.lfm}

{ TWebsiteSettingsForm }

procedure TWebsiteSettingsForm.FormCreate(Sender: TObject);
begin
  AddVT(vtWebsite);
  SettingsView := TVirtualPropertyGrid.Create(Self);
  with SettingsView do
  begin
    Parent := spProps;
    Align := alClient;
    OnPaintText := @SettingsViewPaintText;
    OnKeyAction := @SettingsViewKeyAction;
    OnMouseUp := @SettingsViewMouseUp;;
    AutoFullExpand := True;
    CleanEnumName := True;
    Header.Columns[0].Width := 300;
  end;
end;

procedure TWebsiteSettingsForm.edSearchChange(Sender: TObject);
begin
  FilterVST(vtWebsite, edSearch.Text);
end;

procedure TWebsiteSettingsForm.edSearchPropertyButtonClick(Sender: TObject);
begin
  edSearchProperty.Clear;
end;

procedure TWebsiteSettingsForm.edSearchPropertyChange(Sender: TObject);
begin
  FilterVST(SettingsView, edSearchProperty.Text);
end;

procedure TWebsiteSettingsForm.edSearchButtonClick(Sender: TObject);
begin
  edSearch.Clear;
end;

procedure TWebsiteSettingsForm.FormDestroy(Sender: TObject);
begin
  RemoveVT(vtWebsite);
end;

procedure TWebsiteSettingsForm.vtWebsiteBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  module: PModuleContainer;
begin
  if not(CellPaintMode = cpmPaint) then Exit;
  module := PModuleContainer(Sender.GetNodeData(Node));
  if Assigned(module) and module^.Settings.Enabled then
  begin
    TargetCanvas.Brush.Color := CL_BSEnabledWebsiteSettings;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TWebsiteSettingsForm.vtWebsiteCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  Result := AnsiCompareStr(PModuleContainer(Sender.GetNodeData(Node1))^.Name,
    PModuleContainer(Sender.GetNodeData(Node2))^.Name);
end;

procedure TWebsiteSettingsForm.SettingsViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  with TargetCanvas.Font do
  begin
    if PModuleContainer(Sender.GetNodeData(FWebsiteSettingsNode))^.Settings.Enabled then
    begin
      Color := clWindowText;
    end
    else
    begin
      Color := clGrayText;
    end;
  end;
end;

procedure TWebsiteSettingsForm.SettingsViewKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  if (CharCode = VK_SPACE) or (CharCode = VK_RETURN) then
  begin
    SettingsView.Invalidate;
  end;
end;

procedure TWebsiteSettingsForm.SettingsViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
begin
  if Button = mbLeft then
  begin
    SettingsView.GetHitTestInfoAt(X, Y, True, HitInfo);

    if (HitInfo.HitNode <> nil) and (HitInfo.HitNode^.CheckType = ctCheckBox) then
    begin
      SettingsView.Invalidate;
    end;
  end;
end;

procedure TWebsiteSettingsForm.vtWebsiteFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FWebsiteSettingsNode := Node;
  SettingsView.TIObject := PModuleContainer(Sender.GetNodeData(Node))^.Settings;
end;

procedure TWebsiteSettingsForm.vtWebsiteGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  CellText := PModuleContainer(Sender.GetNodeData(Node))^.Name;
end;

procedure TWebsiteSettingsForm.LoadWebsiteSettings;
var
  i: Integer;
  items: TStringList;
begin
  items := TStringList.Create;
  try
    items.OwnsObjects := False;
    items.Duplicates := dupIgnore;
    for i := Modules.Count - 1 downto 0 do
      if items.IndexOf(Modules[i].ID) = -1 then
        items.AddObject(Modules[i].ID, Modules[i]);

    vtWebsite.NodeDataSize := SizeOf(TModuleContainer);
    vtWebsite.BeginUpdate;
    for i := 0 to items.Count - 1 do
      vtWebsite.AddChild(nil, items.Objects[i]);
    vtWebsite.Sort(nil, 0, sdAscending, False);
    vtWebsite.EndUpdate;
  finally
    items.Free;
  end;
end;


end.
