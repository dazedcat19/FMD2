unit frmShutdownCounter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Forms, Controls, StdCtrls, Buttons, ExtCtrls, LCLType,
  DefaultTranslator;

type

  { TShutdownCounterForm }

  TShutdownCounterForm = class(TForm)
    Bevel1 : TBevel;
    btNow : TBitBtn;
    btAbort: TBitBtn;
    itCounter: TIdleTimer;
    lblMessage: TLabel;
    pnBottom : TPanel;
    procedure btAbortClick(Sender: TObject);
    procedure btNowClick(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState
      );
    procedure FormShow(Sender: TObject);
    procedure itCounterTimer(Sender: TObject);
  private
    WaitCounterOK: Boolean;
    { private declarations }
  public
    WaitTimeout: Integer;
    LabelMessage: String;
    { public declarations }
  end;

var
  ShutdownCounterForm: TShutdownCounterForm;

resourcestring
  RS_LblMessageShutdown = 'System will shutdown in %d second.';
  RS_LblMessageHibernate = 'System will hibernate in %d second.';
  RS_LblMessageExit = 'FMD will exit in %d second.';

implementation

{$R *.lfm}

{ TShutdownCounterForm }

procedure TShutdownCounterForm.btAbortClick(Sender: TObject);
begin
  ModalResult := mrAbort;
end;

procedure TShutdownCounterForm.btNowClick(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TShutdownCounterForm.FormCreate(Sender: TObject);
begin
  WaitTimeout  := 5;
  LabelMessage := RS_LblMessageExit;
end;

procedure TShutdownCounterForm.FormKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrAbort;
end;

procedure TShutdownCounterForm.FormShow(Sender: TObject);
begin
  WaitCounterOK      := False;
  lblMessage.Caption := Format(LabelMessage, [WaitTimeout]);
  itCounter.Enabled  := True;
  btAbort.SetFocus;
end;

procedure TShutdownCounterForm.itCounterTimer(Sender: TObject);
begin
  lblMessage.Caption := Format(LabelMessage, [WaitTimeout]);
  if WaitTimeout = 0 then
  begin
    itCounter.Enabled := False;
    ModalResult := mrOK;
  end
  else
    Dec(WaitTimeout);
end;

end.

