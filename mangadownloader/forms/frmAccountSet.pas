unit frmAccountSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, uCustomControls;

type

  { TAccountSetForm }

  TAccountSetForm = class(TForm)
    btOk: TBitBtn;
    btCancel: TBitBtn;
    ckShowPassword: TCheckBox;
    edPassword: TCustomEdit;
    edUsername: TCustomEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure btOkClick(Sender: TObject);
    procedure ckShowPasswordEditingDone(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AccountSetForm: TAccountSetForm;

resourcestring
  RS_CantBeEmpty = 'Username or password can''t be empty!';

implementation

{$R *.lfm}

{ TAccountSetForm }

procedure TAccountSetForm.btOkClick(Sender: TObject);
begin
  if (edUsername.Text = '') or (edPassword.Text = '') then begin
    MessageDlg(RS_CantBeEmpty, mtError, [mbOK], 0);
    if edUsername.Text = '' then edUsername.SetFocus
    else edPassword.SetFocus;
  end
  else
    ModalResult := mrOK;
end;

procedure TAccountSetForm.ckShowPasswordEditingDone(Sender: TObject);
begin
  if ckShowPassword.Checked then
    edPassword.PasswordChar:=#0
  else
    edPassword.PasswordChar:='*';
end;

end.

