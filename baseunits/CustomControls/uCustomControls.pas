unit uCustomControls;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Messages, LMessages, GroupedEdit, EditBtn,
  Graphics, Types, LCLIntf, LCLType, Windows, ComCtrls, CommCtrl, Controls, LogTreeView;

type

  { TCustomHintWindow }

  TCustomHintWindow = class(THintWindow)
  private
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clWindow;
    property BorderColor: TColor read FBorderColor write FBorderColor default clWindowText;
    property TextColor: TColor read FTextColor write FTextColor default clWindowText;
  end;

  { TCustomEdit }

  TCustomEdit = class(TEdit)
  private
    FTextHintColor: TColor;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property TextHintColor: TColor read FTextHintColor write FTextHintColor default clGrayText;
  end;

  { TCustomGEEdit }
  // Add/Override WMPaint to a subclass of TEditButton that didn't? have it originally

  TCustomGEEdit = class(TGEEdit)
  private
    FTextHintColor: TColor;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property TextHintColor: TColor read FTextHintColor write FTextHintColor default clGrayText;
  end;

  { TCustomEditButton }
  // Override the default Class for the edit box of TEditButton

  TCustomEditButton = class(TEditButton)
  protected
    function GetEditorClassType: TGEEditClass; override;
  end;
  
  { TCustomDirectoryEdit }

  TCustomDirectoryEdit = class(TDirectoryEdit)
  protected
    function GetEditorClassType: TGEEditClass; override;
  end;

  { TCustomLogTreeView }

  TCustomLogTreeView = class(TLogTreeView)
  private
    FLastHintNode: TTreeNode;
    FTooltipBackgroundColor: TColor;
    FTooltipBorderColor: TColor;
    FTooltipTextColor: TColor;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;  
  public
    constructor Create(AOwner: TComponent); override;
    property TooltipBackgroundColor: TColor read FTooltipBackgroundColor write FTooltipBackgroundColor default clWindow;
    property TooltipBorderColor: TColor read FTooltipBorderColor write FTooltipBorderColor default clWindowText;
    property TooltipTextColor: TColor read FTooltipTextColor write FTooltipTextColor default clWindowText;
  end;

  { Register }

  procedure Register;

implementation

  { TCustomHintWindow }

  constructor TCustomHintWindow.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FBackgroundColor := clWindow;
    FBorderColor := clWindowText;
    FTextColor := clWindowText;
  end;

  procedure TCustomHintWindow.Paint;
  var
    R: TRect;
    LogTreeView: TCustomLogTreeView;
  begin
    R := ClientRect;

    // Set custom colors
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Font.Color := FTextColor;

    // Check if the hint is for a TCustomLogTreeView ToolTip
    if (HintControl <> nil) and (HintControl is TCustomLogTreeView) then
    begin
      LogTreeView := TCustomLogTreeView(HintControl);
      Canvas.Brush.Color := LogTreeView.TooltipBackgroundColor;
      Canvas.Pen.Color := LogTreeView.TooltipBorderColor;
      Canvas.Font.Color := LogTreeView.TooltipTextColor;
    end;

    // Draw HintWindow
    Canvas.FillRect(R);
    Canvas.Rectangle(R);
    Canvas.Font.Style := [];
    InflateRect(R, -4, -4); // Add padding
    DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_WORDBREAK or DT_LEFT);
  end;

  { TCustomEdit }

  constructor TCustomEdit.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FTextHintColor := clGrayText;
  end;

  procedure TCustomEdit.WMPaint(var Msg: TWMPaint);
  var
    ACanvas: TCanvas;
    R: TRect;
  begin
    inherited; // Call normal painting of the Edit first

    if (Text = '') and (TextHint <> '') then
    begin
      ACanvas := TCanvas.Create;
      try
        ACanvas.Handle := Msg.DC; // Use the provided device context
        ACanvas.Font.Assign(Self.Font);
        ACanvas.Font.Color := FTextHintColor;
        ACanvas.Brush.Style := bsClear; // Prevent background from being drawn
        R := ClientRect;
        ACanvas.TextOut(R.Left + 1, R.Top + 1, TextHint);
      finally
        ACanvas.Free;
      end;
    end;
  end;

  procedure TCustomEdit.CMTextChanged(var Msg: TMessage);
  begin
    inherited;
    Invalidate; // Repaint when text changes to correctly show/hide hint
  end;

  procedure TCustomEdit.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
  begin
    Msg.Result := 1; // Prevent flickering by skipping default background erasing
  end;

  procedure TCustomEdit.CreateWnd;
  begin
    inherited;
    Invalidate; // Force repaint to ensure hint is drawn correctly
  end;

  { TCustomGEEdit }

  constructor TCustomGEEdit.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FTextHintColor := clGrayText;
  end;

  procedure TCustomGEEdit.WMPaint(var Msg: TWMPaint);
  var
    ACanvas: TCanvas;
    R: TRect;
  begin
    inherited;

    if (Text = '') and (TextHint <> '') then
    begin
      ACanvas := TCanvas.Create;
      try
        ACanvas.Handle := Msg.DC;
        ACanvas.Font.Assign(Self.Font);
        ACanvas.Font.Color := FTextHintColor;
        ACanvas.Brush.Style := bsClear;
        R := ClientRect;
        ACanvas.TextOut(R.Left + 1, R.Top + 1, TextHint);
      finally
        ACanvas.Free;
      end;
    end;
  end;

  procedure TCustomGEEdit.CMTextChanged(var Msg: TMessage);
  begin
    inherited;
    Invalidate;
  end;

  procedure TCustomGEEdit.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
  begin
    Msg.Result := 1;
  end;

  procedure TCustomGEEdit.CreateWnd;
  begin
    inherited;
    Invalidate;
  end;

  { TCustomEditButton }

  function TCustomEditButton.GetEditorClassType: TGEEditClass;
  begin
    Result := TCustomGEEdit;
  end;

  { TCustomDirectoryEdit }

  function TCustomDirectoryEdit.GetEditorClassType: TGEEditClass;
  begin
    Result := TCustomGEEdit;
  end;

  { TCustomLogTreeView }

  procedure TCustomLogTreeView.CMHintShow(var Message: TMessage);
  var
    HintInfo: PHintInfo;
    Node: TTreeNode;
    NodeRect: TRect;
  begin
    HintInfo := PHintInfo(Message.LParam);
    Node := GetNodeAt(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y);
    if Assigned(Node) then
    begin
      NodeRect := Node.DisplayRect(True);

      if Node <> FLastHintNode then
      begin
        FLastHintNode := Node; // Track the new node
        HintInfo^.HintStr := Node.Text;
        HintInfo^.HintControl := Self; // Tell hint window it's for a tooltip
        HintInfo^.HintWindowClass := TCustomHintWindow;
        HintInfo^.HintPos := ClientToScreen(TPoint.Create(NodeRect.Left, NodeRect.Top));
        HintInfo^.ReshowTimeout := 0;
        HintInfo^.HideTimeout := 30000;
        Message.Result := 0;
      end;
    end;
    inherited;
  end;

  procedure TCustomLogTreeView.CMMouseLeave(var Message: TMessage);
  begin
    inherited;
    FLastHintNode := nil;
    Application.CancelHint; // Hide hint immediately
  end;

  procedure TCustomLogTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
  var
    Node: TTreeNode;
  begin
    inherited MouseMove(Shift, X, Y);
    Node := GetNodeAt(X, Y);

    // Force hint update ONLY if the node has changed
    if Node <> FLastHintNode then
    begin
      Application.CancelHint; // Cancel previous hint
      Application.ActivateHint(ClientToScreen(TPoint.Create(X, Y))); // Trigger new hint check
    end;
  end; 

  constructor TCustomLogTreeView.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FTooltipBackgroundColor := clWindow;
    FTooltipBorderColor := clWindowText;
    FTooltipTextColor := clWindowText;
  end;

  { Register }

  procedure Register;
  begin
    RegisterComponents('Custom', [TCustomEdit, TCustomEditButton, TCustomHintWindow, TCustomDirectoryEdit, TCustomLogTreeView]);
  end;
end.
