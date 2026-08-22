unit frmWebsiteSelection;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TWebsiteSelectionForm }

  TWebsiteSelectionForm = class(TForm)
    btOk: TBitBtn;
    cbWebsites: TComboBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  WebsiteSelectionForm: TWebsiteSelectionForm;

implementation

{$R *.lfm}

end.

