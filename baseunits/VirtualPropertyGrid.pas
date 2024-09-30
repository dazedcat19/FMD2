unit VirtualPropertyGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Types, FPCanvas, VirtualTrees, Graphics,
  LMessages, Themes, Controls, LCLIntf;

type

  PObjectPropInfo = ^TObjectPropInfo;

  TObjectPropInfo = record
    Obj: TObject;
    PropInfo: PPropInfo;
  end;

  { TVirtualPropertyGrid }

  TVirtualPropertyGrid = class(TVirtualStringTree)
  private
    FAutoFullExpand: Boolean;
    FAutoSortTree: Boolean;
    FCheckBoxPos: TPoint;
    FCheckBoxSize: TSize;
    FCheckBoxUncheckedDetails: TThemedElementDetails;
    FCheckBoxCheckedDetails: TThemedElementDetails;
    FCleanEnumName: Boolean;
    FFilter: TTypeKinds;
    FHideClassNames: Boolean;
    FTIObject: TObject;
    FIsSplitResize: Boolean;
    procedure BuildProperties;
    procedure SetFilter(AValue: TTypeKinds);
    procedure SetTIObject(AValue: TObject);
  protected
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
      override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure PrepareCell(var PaintInfo: TVTPaintInfo; WindowOrgX, MaxWidth: Integer);
      override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure DoAfterPaint(TargetCanvas: TCanvas); override;
    procedure DoColumnClick(Column: TColumnIndex; Shift: TShiftState); override;
    procedure HandleHotTrack(X, Y: Integer); override;
    procedure HandleMouseDown(var Message: TLMMouse; var HitInfo: THitInfo);
      override;
    procedure HandleMouseUp(Keys: PtrUInt; const HitInfo: THitInfo); override;
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
      override;
    procedure DoScroll(DeltaX, DeltaY: Integer); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoFullExpand: Boolean read FAutoFullExpand write FAutoFullExpand default False;
    property AutoSortTree: Boolean read FAutoSortTree write FAutoSortTree default False;
    property CleanEnumName: Boolean read FCleanEnumName write FCleanEnumName default False;
    property Filter: TTypeKinds read FFilter write SetFilter default tkProperties;
    property HideClassNames: Boolean read FHideClassNames write FHideClassNames default False;
    property TIObject: TObject read FTIObject write SetTIObject;
  end;

implementation

uses VirtualPropertyGridEditLink;

{ TVirtualPropertyGrid }

procedure TVirtualPropertyGrid.SetTIObject(AValue: TObject);
begin
  if FTIObject = AValue then
    Exit;
  FTIObject := AValue;
  BuildProperties;
end;

procedure TVirtualPropertyGrid.BuildProperties;

  procedure BuildProps(AObj: TObject; Parent: PVirtualNode);
  var
    tempproplist: PPropList;
    i: Integer;
    node: PVirtualNode;
    Data: PObjectPropInfo;
  begin
    if AObj = nil then
      Exit;
    try
      GetMem(tempproplist, GetTypeData(AObj.ClassInfo)^.PropCount * SizeOf(Pointer));
      for i := 0 to GetPropList(AObj.ClassInfo, FFilter, tempproplist, False) - 1 do
      begin
        node := AddChild(Parent);
        InitNode(node);
        Data := GetNodeData(node);
        with Data^ do
        begin
          Data^.Obj := AObj;
          PropInfo := tempproplist^[i];
          if PropInfo^.PropType^.Kind = tkBool then
          begin
            node^.CheckType := ctCheckBox;
            if Boolean(GetOrdProp(Obj, Data^.PropInfo)) then
              node^.CheckState := csCheckedNormal;
          end
          else
          if PropInfo^.PropType^.Kind = tkClass then
            BuildProps(GetObjectProp(Obj, PropInfo), node);
        end;
      end;
    finally
      if tempproplist <> nil then
        Freemem(tempproplist);
    end;
  end;

begin
  BeginUpdate;
  try
    Clear;
    BuildProps(FTIObject, RootNode);
    if FAutoSortTree then
      SortTree(0, sdAscending, False);
    if FAutoFullExpand then
      FullExpand();
  finally
    EndUpdate;
  end;
end;

procedure TVirtualPropertyGrid.SetFilter(AValue: TTypeKinds);
begin
  if FFilter = AValue then
    Exit;
  FFilter := AValue;
end;

procedure TVirtualPropertyGrid.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  Allowed := False;
  if Column = 0 then
    Exit;
  if PObjectPropInfo(GetNodeData(Node))^.PropInfo^.PropType^.Kind in [tkBool, tkClass] then
    Exit;
  Allowed := True;
  inherited DoCanEdit(Node, Column, Allowed);
end;

function TVirtualPropertyGrid.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
begin
  if Column = 0 then
    Result := AnsiCompareStr(PObjectPropInfo(GetNodeData(node1))^.PropInfo^.Name,
      PObjectPropInfo(GetNodeData(node2))^.PropInfo^.Name);
  Result := inherited DoCompare(Node1, Node2, Column);
end;

procedure TVirtualPropertyGrid.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
begin
  inherited DoFocusChange(Node, Column);
  EditNode(Node, Column);
end;

procedure TVirtualPropertyGrid.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
begin
  with PObjectPropInfo(GetNodeData(Node))^ do
    case Column of
      0: CellText := PropInfo^.Name;
      1: case PropInfo^.PropType^.Kind of
          tkSString,
          tkLString,
          tkAString,
          tkWString:
            CellText := GetStrProp(Obj, PropInfo);
          tkInteger,
          tkInt64:
            CellText := IntToStr(GetOrdProp(Obj, PropInfo));
          tkBool:
            CellText := '(' + BoolToStr(Boolean(GetOrdProp(Obj, PropInfo)), True) + ')';
          tkEnumeration:
            CellText := TryCleanEnumName(GetEnumProp(Obj, PropInfo), FCleanEnumName);
          tkClass:
            if not FHideClassNames then
              CellText := '(' + GetObjectPropClass(Obj, PropInfo^.Name).ClassName + ')';
        end;
    end;
  inherited DoGetText(Node, Column, TextType, CellText);
end;

procedure TVirtualPropertyGrid.DoFreeNode(Node: PVirtualNode);
begin
  Finalize(PObjectPropInfo(GetNodeData(node))^);
  inherited DoFreeNode(Node);
end;

procedure TVirtualPropertyGrid.PrepareCell(var PaintInfo: TVTPaintInfo;
  WindowOrgX, MaxWidth: Integer);
var
  C: TRect;
begin
  with PaintInfo do
  begin
    C := CellRect;
    if Column = 0 then
    begin
      CellRect := ContentRect;
      CellRect.Left := CellRect.Left + 1;
    end
    else
    begin
      if PObjectPropInfo(GetNodeData(Node))^.PropInfo^.PropType^.Kind = tkBool then
        ContentRect.Left := ContentRect.Left + FCheckBoxSize.Width + TextMargin;
    end;
    inherited PrepareCell(PaintInfo, WindowOrgX, MaxWidth);
    if Column = 0 then
      CellRect := C;
  end;
end;

procedure TVirtualPropertyGrid.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  R: TRect;
begin
  inherited DoPaintNode(PaintInfo);
  with PaintInfo, PObjectPropInfo(GetNodeData(PaintInfo.Node))^ do
    if Column = 1 then
    begin
      if PropInfo^.PropType^.Kind = tkBool then
      begin
        R := CellRect;
        R.Left := R.Left + TextMargin;
        R.Width := FCheckBoxSize.Width;
        FCheckBoxPos.x := R.Left;
        FCheckBoxPos.y := R.Right;
        if Boolean(GetOrdProp(Obj, PropInfo)) then
          ThemeServices.DrawElement(Canvas.Handle, FCheckBoxCheckedDetails, R)
        else
          ThemeServices.DrawElement(Canvas.Handle, FCheckBoxUncheckedDetails, R);
      end;
    end;
end;

procedure TVirtualPropertyGrid.DoAfterPaint(TargetCanvas: TCanvas);
var
  Node, XNode: PVirtualNode;
  R, XR: TRect;
begin
  inherited DoAfterPaint(TargetCanvas);
  R := ClientRect;
  with TargetCanvas, R do
  begin
    Pen.Style := psSolid;
    Pen.Color := Self.Colors.GridLineColor;
    Node := GetFirstVisible();
    while Node <> nil do
    begin
      R := GetDisplayRect(Node, 0, True, False, False);
      Line(Left, Top, Left, Bottom);
      if (GetPreviousVisibleSibling(Node) = nil) and
        (Node^.Parent <> nil) and
        (Node^.Parent <> RootNode) then
        Line(Left, Top, Left - Self.Indent - 1, Top);
      if (GetNextVisibleSibling(Node) = nil) then
      begin
        XNode := GetNextVisible(Node, False);
        if (XNode <> nil) and (XNode^.Parent <> Node) then
        begin
          XR := GetDisplayRect(XNode, 0, True, False, False);
          line(Left, Bottom - 1, XR.Left - 1, Bottom - 1);
        end;
      end;
      Node := GetNextVisible(Node);
    end;
    R := ClientRect;
    Left := Header.Columns[0].Left;
    Right := Left + Header.Columns[0].Width - 1;
    Bottom := Bottom + Header.Height;
    Line(Right, Top, Right, Bottom);
    Node := GetLastVisible(nil, True);
    if Node <> nil then
    begin
      R := GetDisplayRect(Node, 0, True, False, False);
      R.Top := R.Bottom;
      R.Bottom := ClientRect.Bottom + Header.Height;
      Line(Left, Top, Left, Bottom);
    end;
  end;
end;

procedure TVirtualPropertyGrid.HandleHotTrack(X, Y: Integer);
begin
  if FIsSplitResize or ((x >= Header.Columns[1].Left - TextMargin) and
    (x <= Header.Columns[1].Left + TextMargin)) then
  begin
    Cursor := crHSplit;
    if FIsSplitResize then
      Header.Columns[0].Width := x;
  end
  else
    inherited HandleHotTrack(X, Y);
end;

procedure TVirtualPropertyGrid.HandleMouseDown(var Message: TLMMouse; var HitInfo: THitInfo);
begin
  if (Message.XPos >= Header.Columns[1].Left - TextMargin) and
    (Message.XPos <= Header.Columns[1].Left + TextMargin) then
    FIsSplitResize := True
  else
    inherited HandleMouseDown(Message, HitInfo);
end;

procedure TVirtualPropertyGrid.HandleMouseUp(Keys: PtrUInt; const HitInfo: THitInfo);
begin
  if FIsSplitResize then
  begin
    Cursor := crDefault;
    FIsSplitResize := False;
  end
  else
    inherited HandleMouseUp(Keys, HitInfo);
end;

procedure TVirtualPropertyGrid.DoColumnClick(Column: TColumnIndex; Shift: TShiftState);
var
  curpos: TPoint;
  Node: PVirtualNode;
begin
  if Column <> 1 then
    Exit;
  curpos := ScreenToClient(Mouse.CursorPos);
  Node := GetNodeAt(curpos.x, curpos.y);
  if Node = nil then
    Exit;
  if (curpos.x >= FCheckBoxPos.x) and (curpos.x <= FCheckBoxPos.y) then
    with PObjectPropInfo(GetNodeData(Node))^ do
    begin
      if PropInfo^.PropType^.Kind = tkBool then
      begin
        SetOrdProp(Obj, PropInfo, Integer(not (Boolean(GetOrdProp(Obj, PropInfo)))));
        RepaintNode(Node);
      end;
    end;
  inherited DoColumnClick(Column, Shift);
end;

function TVirtualPropertyGrid.DoCreateEditor(Node: PVirtualNode;
  Column: TColumnIndex): IVTEditLink;
var
  L: TVirtualPropertyGridEditLink;
begin
  L := TVirtualPropertyGridEditLink.Create;
  L.CleanEnumName := FCleanEnumName;
  Result := L;
  L := nil;
end;

procedure TVirtualPropertyGrid.DoScroll(DeltaX, DeltaY: Integer);
begin
  if tsEditing in TreeStates then
    EndEditNode;
  inherited DoScroll(DeltaX, DeltaY);
end;

procedure TVirtualPropertyGrid.KeyPress(var Key: Char);
begin
  if (FocusedColumn = 1) and (FocusedNode <> nil) and (Key in [#13, #32]) then
    with PObjectPropInfo(GetNodeData(FocusedNode))^ do
      if PropInfo^.PropType^.Kind = tkBool then
      begin
        SetOrdProp(Obj, PropInfo, Integer(not (Boolean(GetOrdProp(Obj, PropInfo)))));
        RepaintNode(FocusedNode);
      end;
  inherited KeyPress(Key);
end;

constructor TVirtualPropertyGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := tkProperties;
  DoubleBuffered := True;
  NodeDataSize := SizeOf(TObjectPropInfo);
  Color := clBtnFace;
  EditDelay := 0;
  Margin := 0;
  TextMargin := 4;
  Indent := 18;
  Colors.GridLineColor := clBtnShadow;
  DrawSelectionMode := smBlendedRectangle;
  FCheckBoxUncheckedDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  FCheckBoxCheckedDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
  FCheckBoxSize := ThemeServices.GetDetailSize(FCheckBoxUncheckedDetails);
  with TreeOptions do
  begin
    PaintOptions := PaintOptions - [toShowTreeLines] + [toHideFocusRect, toPopupMode];
    SelectionOptions := SelectionOptions + [toExtendedFocus];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions];
  end;
  with Header do
  begin
    Options := Options - [hoVisible] + [hoAutoResize];
    Columns.Add;
    Columns.Add;
    Columns[0].Width := 200;
    AutoSizeIndex := 1;
  end;
end;

destructor TVirtualPropertyGrid.Destroy;
begin
  inherited Destroy;
end;

end.
