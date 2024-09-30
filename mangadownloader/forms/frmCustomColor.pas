unit frmCustomColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Graphics, Dialogs, ColorBox, ComCtrls,
  VirtualTrees, FMDOptions, jsonini;

type
  TColorItem = record
    N: String;
    C: TColor;
  end;

  { TColorItems }

  TColorItems = class
  private
    FColors: array of TColorItem;
    function GetC(Index: Integer): TColor;
    function GetN(Index: Integer): String;
    procedure SetC(Index: Integer; AValue: TColor);
    procedure SetN(Index: Integer; AValue: String);
  public
    destructor Destroy; override;
  public
    function Count: Integer;
    procedure Add(const AName: String; const AColor: TColor);
    property N[Index: Integer]: String read GetN write SetN;
    property C[Index: Integer]: TColor read GetC write SetC; default;
  end;

  { TVirtualStringTree }

  TVirtualStringTree = class(VirtualTrees.TVirtualStringTree)
  private
    FCI: TColorItems;
    procedure SetCI(AValue: TColorItems);
  public
    property CI: TColorItems read FCI write SetCI;
  end;

  { TCustomColorForm }

  TCustomColorForm = class(TForm)
    CBColors: TColorBox;
    btColors: TColorButton;
    pcCustomColorList: TPageControl;
    tsChapterList: TTabSheet;
    tsMangaList: TTabSheet;
    tsFavoriteList: TTabSheet;
    tsBasicList: TTabSheet;
    VTBasicList: TVirtualStringTree;
    VTChapterList: TVirtualStringTree;
    VTMangaList: TVirtualStringTree;
    VTFavoriteList: TVirtualStringTree;
    procedure btColorsColorChanged(Sender: TObject);
    procedure CBColorsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VTBasicListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
      CellRect: TRect; var ContentRect: TRect);
    procedure VTBasicListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VTBasicListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VTBasicListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VTBasicListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
    { private declarations }
    procedure DrawBoxColorText(const TargetCanvas: TCanvas; const BoxColor: TColor;
      const CellText: String; CellRect: TRect);
    procedure SetSelectedColor(const AColor: TColor);
  public
    { public declarations }
  end;

  TVTList = record
    VT: VirtualTrees.TVirtualStringTree;
    PaintText: TVTPaintText;
    BeforeCellPaint: TVTBeforeCellPaintEvent;
    PaintBackground: TVTBackgroundPaintEvent;
  end;

  { TVTApplyList }

  TVTApplyList = class
  private
    FCount: Integer;
    FVTList: array of TVTList;
  private
    procedure VTOnPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VTOnBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VTOnPaintBackground(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; const R: TRect;
      var Handled: Boolean);
  private
    procedure InstallCustomColors(Index: Integer);
    function GetItems(Index: Integer): VirtualTrees.TVirtualStringTree;
    procedure SetItems(Index: Integer; AValue: VirtualTrees.TVirtualStringTree);
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const AVT: VirtualTrees.TVirtualStringTree): Integer;
    procedure Add(const AVT: VirtualTrees.TVirtualStringTree);
    procedure Remove(const AVT: VirtualTrees.TVirtualStringTree);
    property Items[Index: Integer]: VirtualTrees.TVirtualStringTree read GetItems write SetItems; default;
    property Count: Integer read FCount;
  end;

procedure AddVT(const AVT: VirtualTrees.TVirtualStringTree); inline;
procedure RemoveVT(const AVT: VirtualTrees.TVirtualStringTree); inline;
procedure Apply;
procedure LoadFromIniFile(const AIniFile: TJSONIniFile);
procedure SaveToIniFile(const AIniFile: TJSONIniFile);

var
  CustomColorForm: TCustomColorForm;

implementation

const
  TextStyleLeftCenter: TTextStyle = (
    Alignment: taLeftJustify;
    Layout: tlCenter;
    SingleLine: True;
    Clipping: False;
    ExpandTabs: True;
    ShowPrefix: False;
    Wordbreak: False;
    Opaque: False;
    SystemFont: False;
    RightToLeft: False;
    EndEllipsis: True);

var
  // color collection
  BasicListColors,
  MangaListColors,
  FavoriteListColors,
  ChapterListColor: TColorItems;

  // current selected color list
  SelectedColorList: TVirtualStringTree;

  // vt list to apply
  VTApplyList: TVTApplyList;

procedure DoInit;
begin
  BasicListColors := TColorItems.Create;
  with BasicListColors do
  begin
    Add('BackgroundColor', clWindow);
    Add('BorderColor', clBtnFace);
    Add('DisabledColor', clBtnShadow);
    Add('DropMarkColor', clHighlight);
    Add('DropTargetColor', clHighLight);
    Add('DropTargetBorderColor', clHotLight);
    Add('FocusedSelectionColor', clHighLight);
    Add('FocusedSelectionBorderColor', clHotLight);
    Add('GridLineColor', clBtnShadow);
    Add('HeaderHotColor', clBtnShadow);
    Add('HotColor', clWindowText);
    Add('SelectionRectangleBlendColor', clHighlight);
    Add('SelectionRectangleBorderColor', clHotLight);
    Add('TreeLineColor', clBtnShadow);
    Add('UnfocusedSelectionColor', clSilver);
    Add('UnfocusedSelectionBorderColor', clGray);
    Add('NormalTextColor', clWindowText);
    Add('FocusedSelectionTextColor', clHighlightText);
    Add('UnfocusedSelectionTextColor', clWindowText);
    Add('OddColor', CL_BSOdd);
    Add('EvenColor', CL_BSEven);
    Add('SortedColumnColor', CL_BSSortedColumn);
    Add('EnableWebsiteSettings', CL_BSEnabledWebsiteSettings);
  end;

  MangaListColors := TColorItems.Create;
  with MangaListColors do
  begin
    Add('NewMangaColor', CL_MNNewManga);
    Add('CompletedMangaColor', CL_MNCompletedManga);
  end;

  FavoriteListColors := TColorItems.Create;
  with FavoriteListColors do
  begin
    Add('BrokenFavoriteColor', CL_FVBrokenFavorite);
    Add('CheckingColor', CL_FVChecking);
    Add('NewChapterFoundColor', CL_FVNewChapterFound);
    Add('CompletedSeriesColor', CL_FVCompletedManga);
    Add('EmptyChapters', CL_FVEmptyChapters);
  end;

  ChapterListColor := TColorItems.Create;
  with ChapterListColor do
  begin
    Add('DownloadedColor', CL_CHDownloaded);
  end;

  SelectedColorList := nil;
  VTApplyList := TVTApplyList.Create;
end;

procedure DoFinal;
begin
  BasicListColors.Free;
  MangaListColors.Free;
  FavoriteListColors.Free;
  ChapterListColor.Free;
  VTApplyList.Free;
end;

procedure ApplyBasicColorToVT(const AVT: VirtualTrees.TVirtualStringTree);
begin
  with AVT.Colors do
  begin
    AVT.Color := BasicListColors[0];
    BorderColor := BasicListColors[1];
    DisabledColor := BasicListColors[2];
    DropMarkColor := BasicListColors[3];
    DropTargetColor := BasicListColors[4];
    DropTargetBorderColor := BasicListColors[5];
    FocusedSelectionColor := BasicListColors[6];
    FocusedSelectionBorderColor := BasicListColors[7];
    GridLineColor := BasicListColors[8];
    HeaderHotColor := BasicListColors[9];
    HotColor := BasicListColors[10];
    SelectionRectangleBlendColor := BasicListColors[11];
    SelectionRectangleBorderColor := BasicListColors[12];
    TreeLineColor := BasicListColors[13];
    UnfocusedSelectionColor := BasicListColors[14];
    UnfocusedSelectionBorderColor := BasicListColors[15];
    AVT.Repaint;
  end;
end;

procedure AddVT(const AVT: VirtualTrees.TVirtualStringTree);
begin
  VTApplyList.Add(AVT);
end;

procedure RemoveVT(const AVT: VirtualTrees.TVirtualStringTree);
begin
  VTApplyList.Remove(AVT);
end;

procedure ApplyToFMDOptions;
begin
  //basiclist
  CL_BSNormalText := BasicListColors[16];
  CL_BSFocusedSelectionText := BasicListColors[17];
  CL_BSUnfocesedSelectionText := BasicListColors[18];
  CL_BSOdd := BasicListColors[19];
  CL_BSEven := BasicListColors[20];
  CL_BSSortedColumn := BasicListColors[21];
  CL_BSEnabledWebsiteSettings := BasicListColors[22];

  //mangalist
  CL_MNNewManga := MangaListColors[0];
  CL_MNCompletedManga := MangaListColors[1];

  //favoritelist
  CL_FVBrokenFavorite := FavoriteListColors[0];
  CL_FVChecking := FavoriteListColors[1];
  CL_FVNewChapterFound := FavoriteListColors[2];
  CL_FVCompletedManga := FavoriteListColors[3];
  CL_FVEmptyChapters := FavoriteListColors[4];

  //chapterlist
  CL_CHDownloaded := ChapterListColor[0];
end;

procedure Apply;
var
  i: Integer;
begin
  ApplyToFMDOptions;
  if VTApplyList.Count > 0 then
    for i := 0 to VTApplyList.Count - 1 do
      ApplyBasicColorToVT(VTApplyList[i]);
end;

procedure LoadFromIniFile(const AIniFile: TJSONIniFile);
var
  i: Integer;
begin
  with AIniFile do
  begin
    //basiclist
    for i := 0 to BasicListColors.Count - 1 do
      BasicListColors[i] := StringToColor(ReadString('BasicListColors', BasicListColors.N[i],
        ColorToString(BasicListColors[i])));

    //mangalist
    for i := 0 to MangaListColors.Count - 1 do
      MangaListColors[i] := StringToColor(ReadString('MangaListColors', MangaListColors.N[i],
        ColorToString(MangaListColors[i])));

    //favoritelist
    for i := 0 to FavoriteListColors.Count - 1 do
      FavoriteListColors[i] := StringToColor(ReadString('FavoriteListColors', FavoriteListColors.N[i],
        ColorToString(FavoriteListColors[i])));

    //chapterlist
    for i := 0 to ChapterListColor.Count - 1 do
      ChapterListColor[i] := StringToColor(ReadString('ChapterListColor', ChapterListColor.N[i],
        ColorToString(ChapterListColor[i])));

    ApplyToFMDOptions;
  end;
end;

procedure SaveToIniFile(const AIniFile: TJSONIniFile);
var
  i: Integer;
begin
  with AIniFile do
  begin
    //basiclist
    for i := 0 to BasicListColors.Count - 1 do
      WriteString('BasicListColors', BasicListColors.N[i], ColorToString(BasicListColors[i]));

    //mangalist
    for i := 0 to MangaListColors.Count - 1 do
      WriteString('MangaListColors', MangaListColors.N[i], ColorToString(MangaListColors[i]));

    //favoritelist
    for i := 0 to FavoriteListColors.Count - 1 do
      WriteString('FavoriteListColors', FavoriteListColors.N[i], ColorToString(FavoriteListColors[i]));

    //chapterlist
    for i := 0 to ChapterListColor.Count - 1 do
      WriteString('ChapterListColor', ChapterListColor.N[i], ColorToString(ChapterListColor[i]));
  end;
end;

{$R *.lfm}

{ TVTApplyList }

procedure TVTApplyList.VTOnPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if vsSelected in Node^.States then
     if Sender.Focused then
       TargetCanvas.Font.Color:=CL_BSFocusedSelectionText
     else
       TargetCanvas.Font.Color:=CL_BSUnfocesedSelectionText
  else
     TargetCanvas.Font.Color:=CL_BSNormalText;

  if Assigned(FVTList[Sender.Tag].PaintText) then
    FVTList[Sender.Tag].PaintText(Sender, TargetCanvas, Node, Column, TextType);
end;

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

procedure TVTApplyList.VTOnBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  isSortedColumn: Boolean = False;
  CRect: TRect;
begin
  with VirtualTrees.TVirtualStringTree(Sender) do begin
    if (CellPaintMode = cpmPaint) and (Column<>NoColumn) then
    begin
      if odd(Node^.Index) then
        TargetCanvas.Brush.Color := CL_BSEven
      else
        TargetCanvas.Brush.Color := CL_BSOdd;
      isSortedColumn := (Header.SortColumn <> -1) and (Header.SortColumn = Column);
      if (not isSortedColumn) and (TargetCanvas.Brush.Color <> clNone) then
        TargetCanvas.FillRect(CellRect);
    end;

    if Assigned(FVTList[Sender.Tag].BeforeCellPaint) then
      FVTList[Sender.Tag].BeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode,
        CellRect, ContentRect);

    if not (CellPaintMode = cpmPaint) then Exit;

    if toFullRowSelect in TreeOptions.SelectionOptions then
      CRect:=CellRect
    else begin
      CRect:=GetDisplayRect(Node,Column,True);
      CRect.Top:=ContentRect.Top;
      CRect.Bottom:=ContentRect.Bottom;
    end;

    // draw selected
    if vsSelected in Node^.States then begin
      if Sender.Focused then
        TargetCanvas.Brush.Color := Colors.FocusedSelectionColor
      else
        TargetCanvas.Brush.Color := Colors.UnfocusedSelectionColor;
      TargetCanvas.FillRect(CRect);
    end;

    if isSortedColumn and (CL_BSSortedColumn <> clNone) then
    begin
      if vsSelected in Node^.States then
        TargetCanvas.Brush.Color := BlendColor(TargetCanvas.Brush.Color,CL_BSSortedColumn,200)
      else
        TargetCanvas.Brush.Color := BlendColor(TargetCanvas.Brush.Color,CL_BSSortedColumn,SelectionBlendFactor);
      TargetCanvas.FillRect(CellRect);
    end;

    // draw gridline
    if Header.Columns.Count <> 0 then
    begin
      TargetCanvas.Pen.Color := BlendColor(TargetCanvas.Brush.Color,Colors.GridLineColor,SelectionBlendFactor);
      TargetCanvas.Line(CellRect.Right-1,CellRect.Top,CellRect.Right-1,CellRect.Bottom);
    end;

    if Node = HotNode then begin
      if isSortedColumn then
        TargetCanvas.Brush.Color := BlendColor(Colors.FocusedSelectionColor, TargetCanvas.Brush.Color, 100)
      else
        TargetCanvas.Brush.Color := BlendColor(Colors.FocusedSelectionColor, TargetCanvas.Brush.Color, SelectionBlendFactor);
      TargetCanvas.FillRect(CRect);
    end;
  end;
end;

procedure TVTApplyList.VTOnPaintBackground(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; const R: TRect; var Handled: Boolean);
var
  aRect,aColumnRect:TRect;
  i,j,fixedColumnsCount,fixedColumnsWidth:Integer;
  isFixedColumnsRect:Boolean;

  procedure paintVertGridline(const orect:TRect);
  begin
    with VirtualTrees.TVirtualStringTree(Sender) do begin
      TargetCanvas.Pen.Color:=BlendColor(TargetCanvas.Brush.Color,Colors.GridLineColor,SelectionBlendFactor);
      if LineStyle=lsDotted then
        TargetCanvas.Pen.Style:=psDot
      else
        TargetCanvas.Pen.Style:=psSolid;
      TargetCanvas.Line(orect.Right,orect.Top,orect.Right,orect.Bottom);
    end;
  end;

  procedure paintSortedColumn(const orect:TRect);
  begin
    with VirtualTrees.TVirtualStringTree(Sender) do begin
      if CL_BSSortedColumn=clNone then Exit;
      TargetCanvas.Brush.Color:=BlendColor(CL_BSSortedColumn,TargetCanvas.Brush.Color,SelectionBlendFactor);
      TargetCanvas.FillRect(orect);
    end;
  end;

begin
  with VirtualTrees.TVirtualStringTree(Sender) do begin
    if Header.Columns.Count=0 then Exit;

    // draw background
    TargetCanvas.Brush.Style:=bsSolid;
    TargetCanvas.Brush.Color:=Colors.BackGroundColor;
    TargetCanvas.FillRect(R);
    Handled:=True;

    fixedColumnsCount:=0;
    fixedColumnsWidth:=0; // fixed columns width
    for i:=0 to Header.Columns.Count-1 do
      if Header.Columns[I].Options*[coVisible,coFixed]=[coVisible,coFixed] then begin
        Inc(fixedColumnsCount);
        Inc(fixedColumnsWidth,Header.Columns[I].Width);
      end;

    isFixedColumnsRect:=R.Width=fixedColumnsWidth;
    aRect:=R;

    if not isFixedColumnsRect then //non fixed columns
    begin
      // get offset display rect
      aRect.Left:=aRect.Left-(ClientRect.Width-aRect.Width);

      // paint vertgridline for each column
      i:=Header.Columns.GetFirstVisibleColumn();
      while i<>InvalidColumn do
      begin
        aColumnRect:=aRect;
        Inc(aColumnRect.Left,Header.Columns[i].Left);
        aColumnRect.Right:=AColumnRect.Left+(Header.Columns[i].Width-1);
        //if toShowVertGridLines in TreeOptions.PaintOptions then
          paintVertGridline(aColumnRect);
        // paint sorted column
        if i=Header.SortColumn then
          paintSortedColumn(aColumnRect);
        i:=Header.Columns.GetNextVisibleColumn(i);
      end;
    end;

    //if isPaintFixedColumns then
    begin
      // fixed columns always on the left regardless of its column order
      j:=Header.Columns.GetFirstVisibleColumn();
      for i:=0 to fixedColumnsCount-1 do begin
        AColumnRect:=aRect;
        if isFixedColumnsRect then
          aColumnRect.Left:=Header.Columns[i].Left
        else
          Inc(AColumnRect.Left,Header.Columns[i].Left);
        aColumnRect.Right:=aColumnRect.Left+(Header.Columns[i].Width-1);
        //if toShowVertGridLines in TreeOptions.PaintOptions then
          paintVertGridline(aColumnRect);
        // fixed sorted column
        if i=Header.SortColumn then
          paintSortedColumn(aColumnRect);
        j:=Header.Columns.GetNextVisibleColumn(j);
      end;
    end;
  end;

  if Assigned(FVTList[Sender.Tag].PaintBackground) then
    FVTList[Sender.Tag].PaintBackground(Sender,TargetCanvas,R,Handled);
end;

procedure TVTApplyList.InstallCustomColors(Index: Integer);
begin
  with FVTList[Index], VT do
  begin
    // set options
    LineStyle := lsSolid;
    if Color = clDefault then
      Color := clWindow;
    Header.Options:=Header.Options+[hoHotTrack];
    with TreeOptions do begin
      PaintOptions:=PaintOptions-[toUseExplorerTheme,toHotTrack,toShowVertGridLines,toShowHorzGridLines]
        +[toAlwaysHideSelection,toHideFocusRect];
      MiscOptions:=MiscOptions+[toCheckSupport]; //without toHotTrack or toCheckSupport focus not invalidated
    end;

    // save original event
    PaintText := OnPaintText;
    BeforeCellPaint := OnBeforeCellPaint;
    PaintBackground := OnPaintBackground;

    // set custom event
    OnPaintText := @VTOnPaintText;
    OnBeforeCellPaint := @VTOnBeforeCellPaint;
    OnPaintBackground := @VTOnPaintBackground;
  end;
end;

function TVTApplyList.GetItems(Index: Integer): VirtualTrees.TVirtualStringTree;
begin
  Result := FVTList[Index].VT;
end;

procedure TVTApplyList.SetItems(Index: Integer; AValue: VirtualTrees.TVirtualStringTree);
begin
  if FVTList[Index].VT <> AValue then
    FVTList[Index].VT := AValue;
end;

constructor TVTApplyList.Create;
begin
  FCount := 0;
end;

destructor TVTApplyList.Destroy;
begin
  SetLength(FVTList, 0);
  inherited Destroy;
end;

function TVTApplyList.IndexOf(const AVT: VirtualTrees.TVirtualStringTree): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FVTList[Result].VT <> AVT) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TVTApplyList.Add(const AVT: VirtualTrees.TVirtualStringTree);
begin
  if IndexOf(AVT) = -1 then
  begin
    SetLength(FVTList, FCount + 1);
    FVTList[FCount].VT := AVT;
    AVT.Tag := FCount;
    InstallCustomColors(FCount);
    Inc(FCount);
  end;
end;

procedure TVTApplyList.Remove(const AVT: VirtualTrees.TVirtualStringTree);
var
  i: Integer;
begin
  i := IndexOf(AVT);
  if i = -1 then Exit;
  Dec(FCount);
  if i <> FCount then
    FVTList[i] := FVTList[FCount];
  SetLength(FVTList, FCount);
end;

{ TVirtualStringTree }

procedure TVirtualStringTree.SetCI(AValue: TColorItems);
begin
  if FCI = AValue then Exit;
  FCI := AValue;
  RootNodeCount := FCI.Count;
end;

{ TColorItems }

function TColorItems.GetC(Index: Integer): TColor;
begin
  Result := FColors[Index].C;
end;

function TColorItems.GetN(Index: Integer): String;
begin
  Result := FColors[Index].N;
end;

procedure TColorItems.SetC(Index: Integer; AValue: TColor);
begin
  if FColors[Index].C <> AValue then
    FColors[Index].C := AValue;
end;

procedure TColorItems.SetN(Index: Integer; AValue: String);
begin
  if FColors[Index].N <> AValue then
    FColors[Index].N := AValue;
end;

destructor TColorItems.Destroy;
begin
  SetLength(FColors, 0);
  inherited Destroy;
end;

function TColorItems.Count: Integer;
begin
  Result := Length(FColors);
end;

procedure TColorItems.Add(const AName: String; const AColor: TColor);
begin
  SetLength(FColors, Length(FColors) + 1);
  with FColors[High(FColors)] do
  begin
    N := AName;
    C := AColor;
  end;
end;

{ TCustomColorForm }

procedure TCustomColorForm.FormCreate(Sender: TObject);
begin
  AddVT(VTBasicList);
  AddVT(VTMangaList);
  AddVT(VTFavoriteList);
  AddVT(VTChapterList);
  VTBasicList.CI := BasicListColors;
  VTMangaList.CI := MangaListColors;
  VTFavoriteList.CI := FavoriteListColors;
  VTChapterList.CI := ChapterListColor;
end;

procedure TCustomColorForm.VTBasicListBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  with VirtualTrees.TVirtualStringTree(Sender), TargetCanvas do
  begin
    if odd(Node^.Index) then
      Brush.Color := BasicListColors[19]
    else
      Brush.Color := BasicListColors[20];
    FillRect(CellRect);
  end;
end;

procedure TCustomColorForm.CBColorsChange(Sender: TObject);
begin
  btColors.ButtonColor := CBColors.Selected;
  SetSelectedColor(CBColors.Selected);
end;

procedure TCustomColorForm.btColorsColorChanged(Sender: TObject);
begin
  CBColors.Selected := btColors.ButtonColor;
  SetSelectedColor(btColors.ButtonColor);
end;

procedure TCustomColorForm.VTBasicListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := False;
  DrawBoxColorText(TargetCanvas, TVirtualStringTree(Sender).CI[Node^.Index],
    CellText, CellRect);
end;

procedure TCustomColorForm.VTBasicListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  if SelectedColorList <> TVirtualStringTree(Sender) then
    SelectedColorList := TVirtualStringTree(Sender);
  CBColors.Selected := TVirtualStringTree(Sender).CI[Node^.Index];
  btColors.ButtonColor := CBColors.Selected;
end;

procedure TCustomColorForm.VTBasicListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  CellText := TVirtualStringTree(Sender).CI.N[Node^.Index];
end;

procedure TCustomColorForm.VTBasicListPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  with TargetCanvas.Font do
  begin
    if Sender.Selected[Node] then
    begin
      if Sender.Focused then
        Color := BasicListColors[17]
      else
        Color := BasicListColors[18];
    end
    else
      Color := BasicListColors[16];
  end;
end;

procedure TCustomColorForm.DrawBoxColorText(const TargetCanvas: TCanvas; const BoxColor: TColor;
  const CellText: String; CellRect: TRect);
var
  ABoxRect: TRect;
  ATextRect: TRect;
begin
  with TargetCanvas do
  begin
    // box color rect
    ABoxRect := CellRect;
    InflateRect(ABoxRect, -2, -2);
    ABoxRect.Left := CellRect.Left;
    ABoxRect.Right := ABoxRect.Left + (ABoxRect.Bottom - ABoxRect.Top);
    // text rect
    ATextRect := CellRect;
    ATextRect.Left := ABoxRect.Right + 4;
    // box color
    Brush.Style := bsSolid;
    Pen.Color := clGray;
    Brush.Color := BoxColor;
    Rectangle(ABoxRect);
    // extra border
    Brush.Style := bsClear;
    Pen.Color := clWhite;
    InflateRect(ABoxRect, -1, -1);
    Rectangle(ABoxRect);
    // text
    TextRect(ATextRect, ATextRect.Left, 0, CellText, TextStyleLeftCenter);
  end;
end;

procedure TCustomColorForm.SetSelectedColor(const AColor: TColor);
begin
  if (SelectedColorList = nil) or (SelectedColorList.FocusedNode = nil) then Exit;
  if SelectedColorList.CI[SelectedColorList.FocusedNode^.Index] = AColor then Exit;
  SelectedColorList.CI[SelectedColorList.FocusedNode^.Index] := AColor;
  if SelectedColorList = VTBasicList then
  begin
    ApplyBasicColorToVT(VTBasicList);
    ApplyBasicColorToVT(VTMangaList);
    ApplyBasicColorToVT(VTFavoriteList);
    ApplyBasicColorToVT(VTChapterList);
  end
  else
    SelectedColorList.Repaint;
end;

initialization
  DoInit;

finalization
  DoFinal;

end.
