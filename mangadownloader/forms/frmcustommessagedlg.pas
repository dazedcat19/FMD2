unit frmCustomMessageDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DialogRes, Buttons, ExtCtrls, StdCtrls, LCLType;

type

  { TCustomMessageDlg }

  TCustomMessageDlg = class(TForm)
    pnlButtons: TPanel;
    imgIcon: TImage;
    lblMessage: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FHelpContext: Longint;
    procedure CreateButtons(AButtons: TMsgDlgButtons);
    procedure SetIcon(ADlgType: TMsgDlgType);
  public
    class function ShowDlg(const AForm: TForm; const ACaption, AMsg: String; ADlgType: TMsgDlgType;
      AButtons: TMsgDlgButtons; AHelpCtx: Longint = 0): TModalResult;
  end;

function CenteredMessageDlg(const AForm: TForm; const ACaption, AMsg: String; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AHelpCtx: Longint = 0): TModalResult; overload;

function CenteredMessageDlg(const AForm: TForm; const AMsg: String; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AHelpCtx: Longint = 0): TModalResult; overload;

var
  CustomMessageDlg: TCustomMessageDlg;

resourcestring
  RS_DialogWarning = 'Warning';
  RS_DialogError = 'Error';
  RS_DialogInformation = 'Information';
  RS_DialogConfirmation = 'Confirmation';
  RS_DialogCustom = 'Message';

implementation

uses
  frmMain;

{$R *.lfm}

{ Helper Functions }

function CenteredMessageDlg(const AForm: TForm; const ACaption, AMsg: String; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AHelpCtx: Longint = 0): TModalResult; overload;
begin
  Result := TCustomMessageDlg.ShowDlg(AForm, ACaption, AMsg, ADlgType, AButtons, AHelpCtx);
end;

// Overloaded function without caption
function CenteredMessageDlg(const AForm: TForm; const AMsg: String; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AHelpCtx: Longint = 0): TModalResult; overload;
var
  DefaultCaption: string;
begin
  // Generate a default caption based on the dialog type
  case ADlgType of
    mtWarning: DefaultCaption := RS_DialogWarning;
    mtError: DefaultCaption := RS_DialogError;
    mtInformation: DefaultCaption := RS_DialogInformation;
    mtConfirmation: DefaultCaption := RS_DialogConfirmation;
    mtCustom: DefaultCaption := RS_DialogCustom;
  else
    DefaultCaption := RS_DialogCustom;
  end;

  // Call the version with the caption
  Result := CenteredMessageDlg(AForm, DefaultCaption, AMsg, ADlgType, AButtons, AHelpCtx);
end;

function GetButtonCaption(Button: TMsgDlgBtn): String;
begin
  case Button of
    mbYes: Result := 'Yes';
    mbNo: Result := 'No';
    mbOK: Result := 'OK';
    mbCancel: Result := 'Cancel';
    mbAbort: Result := 'Abort';
    mbRetry: Result := 'Retry';
    mbIgnore: Result := 'Ignore';
    mbAll: Result := 'All';
    mbNoToAll: Result := 'No to All';
    mbYesToAll: Result := 'Yes to All';
    mbClose: Result := 'Close';
  else
    Result := '';
  end;
end;

function GetButtonResult(Button: TMsgDlgBtn): TModalResult;
begin
  case Button of
    mbYes: Result := mrYes;
    mbNo: Result := mrNo;
    mbOK: Result := mrOK;
    mbCancel: Result := mrCancel;
    mbAbort: Result := mrAbort;
    mbRetry: Result := mrRetry;
    mbIgnore: Result := mrIgnore;
    mbAll: Result := mrAll;
    mbNoToAll: Result := mrNoToAll;
    mbYesToAll: Result := mrYesToAll;
    mbClose: Result := mrClose;
  else
    Result := mrNone;
  end;
end;

procedure SetDialogGlyphsImage(AImage: TImage; iconIndex: Integer);
begin
  AImage.Images := DialogGlyphs;
  AImage.ImageIndex := DialogGlyphs.DialogIcon[iconIndex]
end;

{ TCustomMessageDlg }

procedure TCustomMessageDlg.FormCreate(Sender: TObject);
begin
  Position := poScreenCenter;
  AutoSize := False;
  Icon := nil;
end;

procedure TCustomMessageDlg.CreateButtons(AButtons: TMsgDlgButtons);
const
  ButtonWidth = 75;
  ButtonHeight = 25;
  ButtonSpacing = 10;
var
  BitBtn: TBitBtn;
  LeftPos, ButtonCount, TotalWidth: Integer;
  BtnKind: TMsgDlgBtn;
begin
  // Calculate the number of buttons and total width
  ButtonCount := 0;
  for BtnKind := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    if BtnKind in AButtons then
    begin
      Inc(ButtonCount);
    end;
  end;

  TotalWidth := (ButtonWidth * ButtonCount) + (ButtonSpacing * (ButtonCount - 1));

  // Calculate the starting position for the buttons
  LeftPos := (pnlButtons.Width - TotalWidth) div 2;

  // Create buttons dynamically based on AButtons
  for BtnKind := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    if BtnKind in AButtons then
    begin
      BitBtn := TBitBtn.Create(Self);
      BitBtn.Parent := pnlButtons;
      BitBtn.Caption := GetButtonCaption(BtnKind);
      BitBtn.ModalResult := GetButtonResult(BtnKind);
      BitBtn.Width := ButtonWidth;
      BitBtn.Height := ButtonHeight;
      BitBtn.Left := LeftPos;
      BitBtn.Top := (pnlButtons.Height - ButtonHeight) div 2;
      BitBtn.Images := MainForm.IconSmall;

      case BtnKind of
        mbYes: BitBtn.ImageIndex := 2;
        mbNo: BitBtn.ImageIndex := 1;
        mbOK: BitBtn.ImageIndex := 2;
        mbCancel: BitBtn.ImageIndex := 1;
        mbAbort: BitBtn.ImageIndex := 1;
        mbRetry: BitBtn.ImageIndex := 2;
        mbIgnore: BitBtn.ImageIndex := 1;
      end;

      LeftPos := LeftPos + ButtonWidth + ButtonSpacing;
    end;
  end;
end;

procedure TCustomMessageDlg.SetIcon(ADlgType: TMsgDlgType);
begin
  case ADlgType of
    mtWarning: SetDialogGlyphsImage(imgIcon, idDialogWarning);
    mtError: SetDialogGlyphsImage(imgIcon, idDialogError);
    mtInformation: SetDialogGlyphsImage(imgIcon, idDialogInfo);
    mtConfirmation: SetDialogGlyphsImage(imgIcon, idDialogConfirm);
  else
    imgIcon.Visible := False;
    Exit;
  end;
end;

class function TCustomMessageDlg.ShowDlg(const AForm: TForm; const ACaption, AMsg: String;
  ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons; AHelpCtx: Longint = 0): TModalResult;
var
  Dlg: TCustomMessageDlg;
begin
  Dlg := TCustomMessageDlg.Create(nil);
  try
    // Set form properties
    Dlg.Caption := ACaption;
    Dlg.SetIcon(ADlgType);
    Dlg.lblMessage.Caption := AMsg;
    Dlg.FHelpContext := AHelpCtx;

    // Create buttons dynamically
    Dlg.CreateButtons(AButtons);

    // Center the dialog on the provided form
    Dlg.Position := poDesigned;
    Dlg.Left := AForm.Left + (AForm.Width - Dlg.Width) div 2;
    Dlg.Top := AForm.Top + (AForm.Height - Dlg.Height) div 2;

    // Show the dialog and return the result
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

end.
