unit VirtualPropertyGridEditLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, Messages, typinfo, VirtualPropertyGrid, VirtualTrees,
  Controls, StdCtrls, LMessages, LCLType, Spin, EditBtn, Forms, StringsPropEditDlg;

type

  TEditLinkKind = (ekString, ekInteger, ekEnum);

  { TVirtualPropertyGridEditLink }

  TVirtualPropertyGridEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FCleanEnumName: Boolean;
    FColumn: TColumnIndex;
    FEdit: TWinControl;
    FKind: TEditLinkKind;
    FNode: PVirtualNode;
    FPObjectProp: PObjectPropInfo;
    FTree: TVirtualStringTree;
  protected
    procedure EditButtonStringClick(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
  public
    destructor Destroy; override;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): Boolean; stdcall;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure SetBounds(R: TRect); stdcall;
    procedure ProcessMessage(var Message: TLMessage); stdcall;
  published
    property CleanEnumName: Boolean read FCleanEnumName write FCleanEnumName default False;
  end;


  { TEditButtonHelper }

  TEditButtonHelper = class helper for TEditButton
  public
    function GetEdit: TEbEdit;
  end;

function TryCleanEnumName(const EnumName: String; Clean: Boolean = True): String;

implementation

function TryCleanEnumName(const EnumName: String; Clean: Boolean): String;
var
  i: Integer;
begin
  if not Clean then
    Exit(EnumName);
  Result := '';
  for i := 1 to Length(EnumName) do
  begin
    if EnumName[i] in ['A'..'Z'] then
    begin
      Result := Copy(EnumName, i, Length(EnumName) - i + 1);
      Break;
    end;
  end;
  if Result = '' then
    Result := EnumName;
end;

{ TEditButtonHelper }

function TEditButtonHelper.GetEdit: TEbEdit;
begin
  Result := Edit;
end;

{ TVirtualPropertyGridEditLink }

procedure TVirtualPropertyGridEditLink.EditButtonStringClick(Sender: TObject);
begin
  with TStringsPropEditorFrm.Create(nil) do
    try
      with TEditButton(FEdit) do
      begin
        Memo.Lines.Text := Text;
        if ShowModal = mrOk then
          Text := Memo.Lines.Text;
      end;
    finally
      Free;
    end;
end;

procedure TVirtualPropertyGridEditLink.EditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
    begin
      FTree.CancelEditNode;
      Key := 0;
    end;
    VK_RETURN:
    begin
      FTree.EndEditNode;
      Key := 0;
    end;
    VK_UP, VK_DOWN:
    begin
      PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
      Key := 0;
    end;
  end;
end;

procedure TVirtualPropertyGridEditLink.EditExit(Sender: TObject);
begin
  FTree.EndEditNode;
end;

destructor TVirtualPropertyGridEditLink.Destroy;
begin
  Application.ReleaseComponent(FEdit);
  inherited Destroy;
end;

function TVirtualPropertyGridEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
var
  i: Integer;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FPObjectProp := FTree.GetNodeData(Node);
  FEdit.Free;
  FEdit := nil;
  with FPObjectProp^ do
    case PropInfo^.PropType^.Kind of
      tkSString, tkLString, tkAString, tkWString:
      begin
        FKind := ekString;
        FEdit := TEditButton.Create(nil);
        with TEditButton(FEdit) do
        begin
          Text := GetStrProp(Obj, PropInfo);
          ButtonCaption := '...';
          OnButtonClick := @EditButtonStringClick;
          GetEdit.OnKeyDown := @EditKeyDown;
        end;
      end;
      tkInteger, tkInt64:
      begin
        FKind := ekInteger;
        FEdit := TSpinEdit.Create(nil);
        with TSpinEdit(FEdit) do
        begin
          MinValue := 0;
          MaxValue := MaxInt;
          Value := GetOrdProp(Obj, PropInfo);
        end;
      end;
      tkEnumeration:
      begin
        FKind := ekEnum;
        FEdit := TComboBox.Create(nil);
        with TComboBox(FEdit) do
        begin
          for i := 0 to GetEnumNameCount(PropInfo^.PropType) - 1 do
            Items.Add(TryCleanEnumName(GetEnumName(PropInfo^.PropType, i), FCleanEnumName));
          ItemIndex := GetOrdProp(Obj, PropInfo);
        end;
      end;
      else
        Result := False;
    end;

  if Result then
    with FEdit do
    begin
      Visible := False;
      Parent := FTree;
      OnKeyDown := @EditKeyDown;
    end;
end;

function TVirtualPropertyGridEditLink.BeginEdit: Boolean; stdcall;
begin
  Result := True;
  FEdit.Visible := True;
  FEdit.SetFocus;
end;

function TVirtualPropertyGridEditLink.CancelEdit: Boolean; stdcall;
begin
  Result := True;
  FEdit.Hide;
end;

function TVirtualPropertyGridEditLink.EndEdit: Boolean; stdcall;
begin
  Result := True;
  with FPObjectProp^ do
    case FKind of
      ekString: SetStrProp(Obj, PropInfo, TEditButton(FEdit).Text);
      ekInteger: SetOrdProp(Obj, PropInfo, TSpinEdit(FEdit).Value);
      ekEnum: SetOrdProp(Obj, PropInfo, TComboBox(FEdit).ItemIndex);
    end;
  FTree.InvalidateNode(FNode);
  FEdit.Hide;
  FTree.SetFocus;
end;

function TVirtualPropertyGridEditLink.GetBounds: TRect; stdcall;
begin
  Result := FEdit.BoundsRect;
end;

procedure TVirtualPropertyGridEditLink.SetBounds(R: TRect); stdcall;
var
  i: Integer;
begin
  FTree.Header.Columns.GetColumnBounds(FColumn, i, R.Right);
  FEdit.BoundsRect := R;
end;

procedure TVirtualPropertyGridEditLink.ProcessMessage(var Message: TLMessage);
  stdcall;
begin
  FEdit.WindowProc(Message);
end;

end.
