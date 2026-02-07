unit GX_DrawFixes;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Messages, Themes, ComCtrls;

type
  THotkeyDrawFix = class(THotkey)
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  end;

  TTabControlDrawFix = class(TTabControl) // currently not in use.
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
    if Length(LText) = 0 then LText := 'None';

    LParent := TWinControlAccess(AHotKey).Parent;
{$IFDEF GX_SUPPORTS_THEMING}
    LBackColor := ColorToRGB(GxOtaGetStyleColor(TStyleColor.scEdit, clWindow));
    LTextColor := ColorToRGB(GxOtaGetStyleFontColor(TStyleFont.sfEditBoxTextNormal, clWindowText));
{$ELSE}
    LBackColor := clWindow;
    LTextColor := clWindowText;
{$ENDIF}
    Wnd := AHotKey.Handle;
    DC := BeginPaint(Wnd, PS);
    if DC <> 0 then
    begin
      OldFont := SelectObject(DC, TWinControlAccess(LParent).Font.Handle);
      SetBkColor(DC, LBackColor);
      SetTextColor(DC, LTextColor);
      TextOut(DC, PS.rcPaint.Left+2, PS.rcPaint.Top+2, PChar(LText), Length(LText));
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
{$IFDEF GX_SUPPORTS_THEMING}
    LBackColor := GxOtaGetThemedColor(clHighlight);
    LTextColor := GxOtaGetThemedColor(clHighlightText);
{$ELSE}
    LBackColor := clHighlight;
    LTextColor := clHighlightText;
{$ENDIF}
  end
  else begin
{$IFDEF GX_SUPPORTS_THEMING}
    LBackColor := GxOtaGetThemedColor(clBtnFace);
    LTextColor := GxOtaGetThemedColor(clBtnText);
{$ELSE}
    LBackColor := clBtnFace;
    LTextColor := clBtnText;
{$ENDIF}
  end;

  Control.Canvas.Brush.Color := LBackColor;
  Control.Canvas.FillRect(Rect);
  Control.Canvas.Font.Color := LTextColor;

  LTextRect := Rect;
  InflateRect(LTextRect, -3, -3); // LTextRect.Inflate(-3, -3);
  DrawText(Control.Canvas.Handle, PChar(LCaption), -1, LTextRect, DT_TOP or DT_LEFT);
end;

Initialization

end.
