unit GX_HotkeyDrawFix;

interface

uses
  Windows, Messages, Themes, ComCtrls;

type
  THotkeyDrawFix = class(THotkey)
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  end;

  TTabControlDrawFix = class(TTabControl)
  public
    procedure DrawTabThemed(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
  end;

procedure PaintHotkey(AHotkey: THotkey);

implementation

uses
  Graphics,
  Controls,
  Menus,
  SysUtils,
  GX_OtaUtils;

{ THotkeyDrawFix }

type
  TWinControlAccess = class(TWinControl);

procedure PaintHotkey(AHotkey: THotkey);
var
  LText      : string;
  LBackColor : TColor;
  LTextColor : TColor;
  LParent    : TWinControl;
  WNd        : HWND;
  DC         : HDC;
  PS         : TPaintStruct;
  OldFont    : HFONT;
begin
  if AHotKey.HandleAllocated then
  begin
    LText := TWinControlAccess(AHotKey).Caption;
    LText := UpperCase(ShortCutToText(AHotKey.HotKey));
    LText := StringReplace(LText, '+', ' + ', [rfReplaceAll]);
    if LText.Length = 0 then LText := 'None';

    LParent := TWinControlAccess(AHotKey).Parent;
    LBackColor := ColorToRGB(GxOtaGetStyleColor(TStyleColor.scEdit, clWindow));
    LTextColor := ColorToRGB(GxOtaGetStyleFontColor(TStyleFont.sfEditBoxTextNormal, clWindowText));

    Wnd := AHotKey.Handle;
    DC := BeginPaint(Wnd, PS);
    if DC <> 0 then
    begin
      OldFont := SelectObject(DC, TWinControlAccess(LParent).Font.Handle);
      SetBkColor(DC, LBackColor);
      SetTextColor(DC, LTextColor);
      TextOut(DC, PS.rcPaint.Left+2, PS.rcPaint.Top+2, PChar(LText), LText.Length);
      if OldFont <> 0 then SelectObject(DC, OldFont);
    end;
    EndPaint(Wnd, PS);
  end;
end;

{ THotkeyDrawFix }

procedure THotkeyDrawFix.WMPaint(var Msg: TWMPaint);
begin
  PaintHotkey(Self);
  Msg.Result := 0;
end;

{ TTabControlDrawFix }

procedure TTabControlDrawFix.DrawTabThemed(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  LCaption   : string;
  LTextRect  : TRect;
  LBackColor : TColor;
  LTextColor : TColor;
begin
  LCaption := TTabControl(Control).Tabs[TabIndex];
  if Active then
  begin
    LBackColor := ColorToRGB(GxOtaGetStyleColor(scButtonHot, clHighlight));// GetThemedColor(clHighlight);
    LTextColor := ColorToRGB(GxOtaGetStyleFontColor(sfButtonTextHot, clHighlightText));//GetThemedColor(clHighlightText)
  end
  else begin
    LBackColor := ColorToRGB(GxOtaGetStyleColor(scButtonNormal, clBtnFace));// GetThemedColor(clHighlight);
    LTextColor := ColorToRGB(GxOtaGetStyleFontColor(sfButtonTextNormal, clBtnText));//GetThemedColor(clHighlightText)
  end;

  Control.Canvas.Brush.Color := LBackColor;
  Control.Canvas.FillRect(Rect);
  Control.Canvas.Font.Color := LTextColor;

  LTextRect := Rect;
  LTextRect.Inflate(-3, -3);
  DrawText(Control.Canvas.Handle, PChar(LCaption), -1, LTextRect, DT_TOP or DT_LEFT);
end;

end.
