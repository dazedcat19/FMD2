unit frmLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, Buttons, Menus, Clipbrd, ComCtrls, LogTreeView, MultiLog, uCustomControls;

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
    { private declarations }
  public
    { public declarations }
  end;

var
  FormLogger: TFormLogger;

implementation

{$R *.lfm}

{ TFormLogger }

procedure TFormLogger.tmClearLogTimer(Sender: TObject);
begin
  if tvLog.Items.TopLvlCount > seLogLimit.Value then
    try
      tvLog.BeginUpdate;
      while tvLog.Items.TopLvlCount > seLogLimit.Value do
        tvLog.Items.TopLvlItems[0].Delete;
    finally
      tvLog.EndUpdate;
    end;
end;

procedure TFormLogger.FormCreate(Sender: TObject);
begin
  Logger.Channels.Add(tvLog.Channel);
end;

procedure TFormLogger.ckStayOnTopChange(Sender: TObject);
begin
  if ckStayOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
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

  procedure GetItemsText(const T: TTreeNode; var S: String; const Indent: Integer = 0);
  var
    i: Integer;
  begin
    if S <> '' then
      S := S + LineEnding;
    S := S + StringOfChar(' ', Indent) + T.Text;
    if T.Count > 0 then
      for i := 0 to T.Count - 1 do
      begin
        S := S + LineEnding + StringOfChar(' ', Indent + 2) + T.Items[i].Text;
        if T.Items[i].Count > 0 then
          GetItemsText(T.Items[i], S, Indent + 2);
      end;
  end;

var
  s: String;
  i: Integer;
begin
  if tvLog.SelectionCount = 0 then Exit;
  s := '';
  for i := 0 to tvLog.SelectionCount - 1 do
    GetItemsText(tvLog.Selections[i], s);
  Clipboard.AsText := s;
end;

procedure TFormLogger.pmLogPopup(Sender: TObject);
begin
  miCopy.Enabled := tvLog.SelectionCount > 0;
end;

end.
