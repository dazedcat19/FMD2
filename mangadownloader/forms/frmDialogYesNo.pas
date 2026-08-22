unit frmDialogYesNo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmDialogYN }

  TfrmDialogYN = class(TForm)
    btYes: TButton;
    btNo: TButton;
    lbMessage: TLabel;
    mMessages: TMemo;
  private

  public

  end;

var
  frmDialogYN: TfrmDialogYN;

implementation

{$R *.lfm}

end.

