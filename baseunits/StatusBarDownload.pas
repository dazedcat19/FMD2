unit StatusBarDownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseThread, httpsendthread, blcksock, ExtCtrls, Forms,
  Controls, Buttons, Graphics, ComCtrls;

type

  { TStatusBarDownload }

  TStatusBarDownload = class(TBaseThread)
  private
    FOwnerForm: TForm;
    FImageList: TImageList;
    FStatusBar: TPanel;
    FButtonCancel: TSpeedButton;
    FButtonCancelImageIndex: Integer;

    FControlMargin: Integer;
    FResized: Boolean;
    FProgressBarRect,
    FProgressBarPercentsRect,
    FStatusTextRect: TRect;
    FProgressText,
    FStatusText: String;

    FTimerRepaint: TTimer;
    FNeedRepaint,
    FLoading,
    FLoadingReverse: Boolean;

    FHTTP: THTTPSendThread;
    FTotalSize,
    FCurrentSize,
    FLoadingStep: Integer;
    FPercents: Double;
  protected
    procedure SyncCreate;
    procedure SyncDestroy;
    procedure TimerRepaintTimer(Sender: TObject);
    procedure StatusBarPaint(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure HTTPSockOnStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
    procedure UpdateStatusText(AStatusText: String);
    procedure UpdateProgressBar(AIndex, ACount: Integer);
    procedure LoadingProgressBar;
    procedure LoadingProgressBarMarquee;
  public
    constructor Create(CreateSuspended: Boolean = True; AOwnerForm: TForm = nil;
      AImageList: TImageList = nil; AButtonCancelImageIndex: Integer = -1);
    destructor Destroy; override;
  published
    property HTTP: THTTPSendThread read FHTTP;
    property StatusBar: TPanel read FStatusBar;
  end;

implementation

uses uBaseUnit, Math;

const
  CL_ProgressBarBaseLine = clBtnFace;
  CL_ProgressBarBase     = clWindow;
  CL_ProgressBarLine     = $25b006;
  CL_ProgressBar         = $42d932;

{ TStatusBarDownload }

procedure TStatusBarDownload.SyncCreate;
var
  txtHeight: Integer;
begin
  FControlMargin := FOwnerForm.ScaleFontTo96(2);
  FStatusBar := TPanel.Create(nil);
  with FStatusBar do
  begin
    Parent := FOwnerForm;
    DoubleBuffered := True;
    Align := alBottom;
    AutoSize := False;
    txtHeight := Canvas.GetTextHeight('A');
    Height := txtHeight + (FControlMargin * 4);
    Caption := '';
    Color := clBtnFace;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone;
    BorderSpacing.Top := FControlMargin;
    OnPaint := @StatusBarPaint;
    OnResize := @StatusBarResize;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psSolid;
    FResized := True;
  end;

  FButtonCancel := TSpeedButton.Create(FStatusBar);
  with FButtonCancel do
  begin
    Parent := FStatusBar;
    Align := alNone;
    AutoSize := False;
    Flat := True;
    Anchors := [akTop, akRight, akBottom];
    AnchorSideTop.Control := FStatusBar;
    AnchorSideTop.Side := asrTop;
    BorderSpacing.Top := FControlMargin;
    AnchorSideRight.Control := FStatusBar;
    AnchorSideRight.Side := asrRight;
    BorderSpacing.Right := FControlMargin;
    AnchorSideBottom.Control := FStatusBar;
    AnchorSideBottom.Side := asrBottom;
    BorderSpacing.Bottom := FControlMargin;
    Width := Height;
    OnClick := @ButtonCancelClick;
    if Assigned(FImageList) and (FButtonCancelImageIndex > -1) then
    begin
      Images := FImageList;
      ImageIndex := FButtonCancelImageIndex;
    end;
  end;

  StatusBarResize(FStatusBar);

  FTimerRepaint := TTimer.Create(FStatusBar);
  FTimerRepaint.Interval := 500;
  FTimerRepaint.OnTimer := @TimerRepaintTimer;
  FTimerRepaint.Enabled := True;
  FNeedRepaint := True;
  FLoading := False;
end;

procedure TStatusBarDownload.SyncDestroy;
begin
  FStatusBar.Free;
end;

procedure TStatusBarDownload.TimerRepaintTimer(Sender: TObject);
begin
  if FNeedRepaint then
  begin
    FNeedRepaint := False;
    FStatusBar.Repaint;
  end;
end;

procedure TStatusBarDownload.StatusBarPaint(Sender: TObject);
var
  txtWidth, txtHeight: integer;
begin
  with FStatusBar.Canvas do
  begin
    Pen.Color := clActiveBorder;
    Line(0,0,FStatusBar.ClientRect.Right,0);

    if FResized then
    begin
      FProgressBarRect := FStatusBar.ClientRect;
      FProgressBarRect.Inflate(-FControlMargin, -(FControlMargin * 2));
      FStatusTextRect := FStatusBar.ClientRect;
      FProgressBarRect.Width := GetTextWidth('_999.99 MB/999.99 MB_');
      FStatusTextRect.Left := FProgressBarRect.Right + (FControlMargin * 2);
      FStatusTextRect.Right := FButtonCancel.Left - FControlMargin;
      FResized := False;
    end;

    Brush.Style := bsSolid;
    Pen.Style := psSolid;

    Pen.Color := CL_ProgressBarBaseLine;
    Brush.Color := CL_ProgressBarBase;
    Rectangle(FProgressBarRect);

    if FLoading then
    begin
      FPercents := 1;
      FNeedRepaint := True;
    end;

    if FPercents > 0 then
    begin
      FProgressBarPercentsRect := FProgressBarRect;

      if FLoading then
      begin
        LoadingProgressBarMarquee;
      end
      else
      begin
        FProgressBarPercentsRect.Right :=
          Round((FProgressBarPercentsRect.Right - FProgressBarPercentsRect.Left) * FPercents) + FProgressBarPercentsRect.Left;
      end;

      Pen.Color   := CL_ProgressBarLine;
      Brush.Color := CL_ProgressBar;

      Frame(FProgressBarPercentsRect);
      FProgressBarPercentsRect.Inflate(-2, -2);
      GradientFill(FProgressBarPercentsRect, BlendColor(Brush.Color, CL_ProgressBarBase, 128), Brush.Color, gdHorizontal);

    end;
    Brush.Style := bsClear;
    GetTextSize(FProgressText, txtWidth, txtHeight);
    TextRect(FProgressBarRect, FProgressBarRect.Left + ((FProgressBarRect.Right - FProgressBarRect.Left - txtWidth) div 2),
      FProgressBarRect.Top + ((FProgressBarRect.Bottom - FProgressBarRect.Top - txtHeight) div 2), FProgressText);

    Font.Color := clWindowText;
    Brush.Style := bsClear;
    txtHeight := GetTextHeight(FStatusText);
    TextRect(FStatusTextRect, FStatusTextRect.Left, FStatusTextRect.Top + ((FStatusTextRect.Bottom - FStatusTextRect.Top - txtHeight) div 2), FStatusText);
  end;
end;

procedure TStatusBarDownload.StatusBarResize(Sender: TObject);
begin
  FResized := True;
end;

procedure TStatusBarDownload.ButtonCancelClick(Sender: TObject);
begin
  Self.Terminate;
end;

procedure TStatusBarDownload.HTTPSockOnStatus(Sender: TObject;
  Reason: THookSocketReason; const Value: String);
begin
  if Terminated then
  begin
    Exit;
  end;

  if Reason = HR_ReadCount then
  begin
    FNeedRepaint := True;
    FLoading := False;
    if FTotalSize = 0 then
    begin
      FTotalSize := StrToIntDef(Trim(FHTTP.Headers.Values['Content-Length']), 0);
      FPercents := 0;
    end;

    Inc(FCurrentSize, StrToInt(Value));
    if (FCurrentSize <> 0) then
    begin
      if FTotalSize < FCurrentSize then
      begin
        FPercents := 1;
      end
      else
      begin
        FPercents := FCurrentSize / FTotalSize;
      end;
    end;
    FProgressText := FormatByteSize(FCurrentSize);
    if FTotalSize <> 0 then
    begin
     FProgressText := FProgressText + '/' + FormatByteSize(FTotalSize);
    end;
  end
  else if Reason = HR_Connect then
  begin
    FNeedRepaint := True;
    FLoading := False;
    FCurrentSize := 0;
    FTotalSize := 0;
  end;
end;

procedure TStatusBarDownload.UpdateStatusText(AStatusText: String);
begin
  if AStatusText <> FStatusText then
  begin
    FStatusText := AStatusText;
    FNeedRepaint := True;
  end;
end;  

procedure TStatusBarDownload.UpdateProgressBar(AIndex, ACount: Integer);
begin
  if (AIndex <> FCurrentSize) or (ACount <> FTotalSize) then
  begin
    FCurrentSize := AIndex;
    FTotalSize := Max(1, ACount);

    FPercents := Min(1, FCurrentSize / FTotalSize);
    FProgressText := IntToStr(FCurrentSize) + '/' + IntToStr(FTotalSize);

    FNeedRepaint := True;
    FLoading := False;
  end;
end;

procedure TStatusBarDownload.LoadingProgressBar;
begin
  FProgressText := '';
  FLoading := True;
  FLoadingStep := 0;
  FLoadingReverse := False;
  FNeedRepaint := True;
end;

procedure TStatusBarDownload.LoadingProgressBarMarquee;
var
  marqueeWidth: Integer = 60;
  stepSize: Integer = 3;
  borderRect: Integer = 2;
begin
  FProgressBarPercentsRect.Left := Max(borderRect, FLoadingStep);
  FProgressBarPercentsRect.Right := Min((FProgressBarRect.Width + borderRect), (FProgressBarPercentsRect.Left + marqueeWidth));

  if FProgressBarPercentsRect.Right >= (FProgressBarRect.Width + borderRect) then
  begin
    FLoadingReverse := True;
  end
  else if FLoadingStep <= borderRect then
  begin
    FLoadingReverse := False;
  end;

  if FLoadingReverse then
  begin                
    Dec(FLoadingStep, stepSize);
  end
  else
  begin 
    Inc(FLoadingStep, stepSize);
  end;
end;

constructor TStatusBarDownload.Create(CreateSuspended: Boolean;
  AOwnerForm: TForm; AImageList: TImageList; AButtonCancelImageIndex: Integer);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FOwnerForm := AOwnerForm;
  FImageList := AImageList;
  FButtonCancelImageIndex := AButtonCancelImageIndex;
  FProgressText := '';
  FStatusText := '';

  FHTTP := THTTPSendThread.Create(Self);
  FHTTP.UserAgent := UserAgentCURL;
  FHTTP.Sock.OnStatus := @HTTPSockOnStatus;
  FCurrentSize := 0;
  FTotalSize := 0;
  FPercents := 0;

  Synchronize(@SyncCreate);
end;

destructor TStatusBarDownload.Destroy;
begin
  Synchronize(@SyncDestroy);

  FHTTP.Free;
  inherited Destroy;
end;

end.

