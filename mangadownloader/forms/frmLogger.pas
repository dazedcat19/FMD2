unit frmLogger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Buttons, Menus, Clipbrd, ComCtrls, uCustomControlsMultilog;

type

  { TFormLogger }

  TFormLogger = class(TForm)
    btnClearLog: TBitBtn;
    ckStayOnTop: TCheckBox;
    lbLogLimit: TLabel;
    miCopy: TMenuItem;
    pmLog: TPopupMenu;
    seLogLimit: TSpinEdit;
    tmClearLog: TTimer;
    tvLog: TCustomLogTreeView;
    procedure btnClearLogClick(Sender: TObject);
    procedure ckStayOnTopChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure pmLogPopup(Sender: TObject);
    procedure tmClearLogTimer(Sender: TObject);

  private
    procedure SanitizeNode(Node: TTreeNode);
    procedure tvLogDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvLogCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);

  public
    { public declarations }
  end;

var
  FormLogger: TFormLogger;

implementation

uses
  LogTreeView, MultiLog;

{$R *.lfm}

{ TFormLogger }

procedure TFormLogger.SanitizeNode(Node: TTreeNode);
var
  PointerNodeText: PString;
  NodeText: String;
  NodeTextLimit: Integer;
begin
  if Node = nil then
  begin
    Exit;
  end;

  NodeTextLimit := 250;
  if (Node.Data <> nil) or (Length(Node.Text) < NodeTextLimit) then
  begin
    Exit;
  end;

  NodeText := Node.Text;

  New(PointerNodeText);
  PointerNodeText^ := NodeText;
  Node.Data := PointerNodeText;

  Node.Text := Copy(NodeText, 1, NodeTextLimit) + '....';
end;

procedure TFormLogger.tvLogCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  SanitizeNode(Node);
  DefaultDraw := True;
end;

procedure TFormLogger.tvLogDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Node.Data = nil then
  begin
    Exit;
  end;

  Dispose(PString(Node.Data));
  Node.Data := nil;
end;

procedure TFormLogger.tmClearLogTimer(Sender: TObject);
var
  RemoveCount, i: Integer;
  NodeToDelete: TTreeNode;
begin
  RemoveCount := tvLog.Items.TopLvlCount - seLogLimit.Value;
  if RemoveCount <= 0 then
  begin
    Exit;
  end;

  tvLog.BeginUpdate;
  try
    for i := 1 to RemoveCount do
    begin
      NodeToDelete := tvLog.Items.GetFirstNode;
      if not Assigned(NodeToDelete) then
      begin       
        Break;
      end;

      NodeToDelete.Delete;
    end;
  finally
    tvLog.EndUpdate;
  end;
end;

procedure TFormLogger.FormCreate(Sender: TObject);
begin
  tvLog.OnDeletion := @tvLogDeletion;
  tvLog.OnCustomDrawItem := @tvLogCustomDrawItem;
  Logger.Channels.Add(tvLog.Channel);
end;

procedure TFormLogger.ckStayOnTopChange(Sender: TObject);
begin
  if ckStayOnTop.Checked then
  begin
    FormStyle := fsStayOnTop;
  end
  else
  begin
    FormStyle := fsNormal;
  end;
end;

procedure TFormLogger.btnClearLogClick(Sender: TObject);
begin
  tvLog.Clear;
end;

procedure TFormLogger.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TFormLogger.FormDestroy(Sender: TObject);
begin
  Logger.Channels.Remove(tvLog.Channel);
end;

procedure TFormLogger.miCopyClick(Sender: TObject);

  function GetNodeFullText(const ANode: TTreeNode): String;
  begin
    SanitizeNode(ANode);

    if ANode.Data <> nil then
    begin
      Result := PString(ANode.Data)^;
    end
    else
    begin
      Result := ANode.Text;
    end;
  end;

  procedure GetItemsText(const T: TTreeNode; var S: String; const Indent: Integer = 0);
  var
    i: Integer;
  begin
    if S <> '' then
    begin
      S := S + LineEnding;
    end;

    S := S + StringOfChar(' ', Indent) + GetNodeFullText(T);

    if T.Count = 0 then
    begin
      Exit;
    end;

    for i := 0 to T.Count - 1 do
    begin
      GetItemsText(T.Items[i], S, Indent + 2);
    end;
  end;

var
  s: String;
  i: Integer;
begin
  if tvLog.SelectionCount = 0 then
  begin
    Exit;
  end;

  s := '';
  for i := 0 to tvLog.SelectionCount - 1 do
  begin
    GetItemsText(tvLog.Selections[i], s);
  end;

  Clipboard.AsText := s;
end;

procedure TFormLogger.pmLogPopup(Sender: TObject);
begin
  miCopy.Enabled := tvLog.SelectionCount > 0;
end;

end.
