unit uCustomControls;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Messages, LMessages, GroupedEdit, EditBtn,
  Graphics, Types, LCLIntf, LCLType;

type

  { TCustomHintWindow }

  TCustomHintWindow = class(THintWindow)
  public
    procedure Paint; override;
  end;

  { TCustomEdit }

  TCustomEdit = class(TEdit)
  private
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateWnd; override;
  end;

  { TCustomGEEdit }
  // Add/Override WMPaint to a subclass of TEditButton that didn't? have it originally

  TCustomGEEdit = class(TGEEdit)
  private
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateWnd; override;
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

  { Register }

  procedure Register;

implementation

  { TCustomHintWindow }

  procedure TCustomHintWindow.Paint;
  var
    R: TRect;
  begin
    R := ClientRect;

    // Set background color
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(R);

    // Draw border
    Canvas.Pen.Color := clWindowText;
    Canvas.Rectangle(R);

    // Set text properties
    Canvas.Font.Color := clWindowText;
    Canvas.Font.Style := [];

    // Draw multi-line text with word wrapping
    InflateRect(R, -4, -4); // Add padding
    DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_WORDBREAK or DT_LEFT);
  end;

  { TCustomEdit }

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
        ACanvas.Font.Color := clGrayText;
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
        ACanvas.Font.Color := clGrayText;
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

  { Register }

  procedure Register;
  begin
    RegisterComponents('Custom', [TCustomEdit, TCustomEditButton, TCustomHintWindow, TCustomDirectoryEdit]);
  end;
end.
