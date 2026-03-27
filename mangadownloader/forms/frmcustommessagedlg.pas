unit frmCustomMessageDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DialogRes, Buttons,
  ExtCtrls, StdCtrls, LCLType, Windows;

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

  RS_DialogButtonYes = 'Yes';
  RS_DialogButtonNo = 'No';
  RS_DialogButtonOK = 'OK';
  RS_DialogButtonCancel = 'Cancel';
  RS_DialogButtonAbort = 'Abort';
  RS_DialogButtonRetry = 'Retry';
  RS_DialogButtonIgnore = 'Ignore';
  RS_DialogButtonAll = 'All';
  RS_DialogButtonNoAll = 'No to All';
  RS_DialogButtonYesAll = 'Yes to All';
  RS_DialogButtonClose = 'Close';

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
    mbYes: Result := RS_DialogButtonYes;
    mbNo: Result := RS_DialogButtonNo;
    mbOK: Result := RS_DialogButtonOK;
    mbCancel: Result := RS_DialogButtonCancel;
    mbAbort: Result := RS_DialogButtonAbort;
    mbRetry: Result := RS_DialogButtonRetry;
    mbIgnore: Result := RS_DialogButtonIgnore;
    mbAll: Result := RS_DialogButtonAll;
    mbNoToAll: Result := RS_DialogButtonNoAll;
    mbYesToAll: Result := RS_DialogButtonYesAll;
    mbClose: Result := RS_DialogButtonClose;
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
  ButtonWidthMin = 70;
  ButtonWidthMax = 125;
  ButtonHeightMin = 25;
  ButtonHeightMax = 30;
  ButtonPadding = 7;
var
  BitBtn: TBitBtn;
  LeftPos, ButtonWidth, ButtonHeight: Integer;
  BtnKind: TMsgDlgBtn;
begin
  ButtonWidth := 0;
  ButtonHeight := 0; 
  pnlButtons.Height := ButtonHeightMax + (ButtonPadding * 2);

  // Starting position for the buttons
  LeftPos := ButtonPadding;

  // Create buttons dynamically based on AButtons
  for BtnKind := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    if not (BtnKind in AButtons) then
    begin
      Continue;
    end;

    BitBtn := TBitBtn.Create(Self);
    BitBtn.AutoSize := True;
    BitBtn.Parent := pnlButtons;
    BitBtn.Caption := GetButtonCaption(BtnKind);
    BitBtn.ModalResult := GetButtonResult(BtnKind);
    BitBtn.Constraints.MinWidth := ButtonWidthMin;
    BitBtn.Constraints.MaxWidth := ButtonWidthMax;
    BitBtn.Constraints.MinHeight := ButtonHeightMin;
    BitBtn.Constraints.MaxHeight := ButtonHeightMax;
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
         
    BitBtn.HandleNeeded;
    BitBtn.GetPreferredSize(ButtonWidth, ButtonHeight);
    BitBtn.Left := LeftPos;
    BitBtn.Top := (pnlButtons.Height - ButtonHeight) div 2;

    LeftPos := LeftPos + ButtonWidth + ButtonPadding;
  end;

  pnlButtons.Width := LeftPos;
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
    Dlg.Width := Max(Dlg.Width, Dlg.pnlButtons.Width);

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
