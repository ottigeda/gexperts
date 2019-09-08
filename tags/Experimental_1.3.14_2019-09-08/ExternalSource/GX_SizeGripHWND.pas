{
  SizeGripHWND.pas

  Delphi component to add a size grip (like if you use a status bar) to the
  lower right corner of any window control. This is intended for small
  footprint non-VCL delphi applications and paints the size grip just using
  the standard API functions (no layout control, no themes).

  Version 1.2a - always find the most current version at
  http://flocke.vssd.de/prog/code/pascal/sizegrip/

  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  All rights reserved.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

2018-12-21, Achim Kalwa:
- Extended to draw a themed size grip.
- Code cleanup.

2019-05-12, Achim Kalwa:
- adapted to GExperts.
}

unit GX_SizeGripHWND;

interface

uses
  Windows;

procedure GxSetWindowSizeGrip(hWnd: HWND; Enable: Boolean);

implementation

uses
  Messages,
  UxTheme,
  Controls;

const
  SizeGripProp        = 'SizeGrip';
  SizeGripElementName = 'status';

type
  TWndProc = function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

  PGripInfo = ^TGripInfo;
  TGripInfo = record
    OldWndProc : TWndProc;
    Enabled    : Boolean;
    GripRect   : TRect;
  end;

function GET_X_LPARAM(lParam: LPARAM): Integer; inline;
begin
  Result := Smallint(LoWord(lParam));
end;

function GET_Y_LPARAM(lParam: LPARAM): Integer; inline;
begin
  Result := Smallint(HiWord(lParam));
end;

procedure GetGripRect(hWndControl: HWND; var Rect: TRect);
var
  LTheme      : HTHEME;
  GripSize    : TSize;
  ThemedSize  : TSize;
  LControl    : TWinControl;
  DC          : HDC;
begin
  GripSize.cx := GetSystemMetrics(SM_CXVSCROLL);
  GripSize.cy := GetSystemMetrics(SM_CYHSCROLL);

  if (hWndControl <> 0) then
  begin
    DC := GetDC(hWndControl);
    LTheme := OpenThemeData(hWndControl, SizeGripElementName);
    if LTheme <> 0 then
    begin
      if GetThemePartSize(
        LTheme,
        DC,
        SP_GRIPPER,
        0,
        nil,
        TS_DRAW,
        ThemedSize) = S_OK
      then begin
        GripSize := ThemedSize; // use size from theme for grip.
      end;
      CloseThemeData(LTheme);
    end;
    ReleaseDC(hWndControl, DC);
  end;

  LControl := FindControl(hWndControl);
  if Assigned(LControl) then
  begin
    GetClientRect(hWndControl, Rect);
    if LControl.UseRightToLeftScrollBar
    then Rect.Right := Rect.Left  + GripSize.cx
    else Rect.Left  := Rect.Right - GripSize.cx;
    Rect.Top := Rect.Bottom - GripSize.cy;
  end;
end { GetGripRect };

function SizeGripWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Info : PGripInfo;
  pt   : TPoint;

  // Invalidate the current grip rectangle
  procedure InvalidateGrip;
  begin
    with Info^ do
    begin
      if  (GripRect.Right > GripRect.Left)
      and (GripRect.Bottom > GripRect.Top)
      then
        InvalidateRect(hWnd, @GripRect, true);
    end;
  end;

  // Update (and invalidate) the current grip rectangle
  procedure UpdateGrip;
  begin
    with Info^ do
      GetGripRect(hWnd, GripRect);

    InvalidateGrip;
  end;

  function CallOld: LRESULT;
  begin
    Result := CallWindowProc(@Info^.OldWndProc, hWnd, Msg, wParam, lParam);
  end;

  procedure DrawSizeGrip(GripRect: TRect);
  var
    LTheme : HTHEME;
    DC     : HDC;
  begin
    DC := GetDC(hWnd);
    LTheme := OpenThemeData(hWnd, SizeGripElementName);
    if (LTheme <> 0) then
    begin
      DrawThemeBackground(
        LTheme,
        DC,
        SP_GRIPPER,
        0,
        GripRect,
        @GripRect
      );
      CloseThemeData(LTheme);
    end
    else begin
      // draw default (unthemed) grip.
      DrawFrameControl(DC, GripRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
    end;
    ReleaseDC(hWnd, DC);
  end;

begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if Info = nil then
    Result := DefWindowProc(hWnd, Msg, wParam, lParam)
  else if not Info^.Enabled then
    Result := CallOld
  else begin
    case Msg of
      WM_NCDESTROY:
        begin
          Result := CallOld;

          SetWindowLong(hWnd, GWL_WNDPROC, NativeInt(@Info^.OldWndProc));
          RemoveProp(hWnd, SizeGripProp);
          Dispose(Info);
        end;

      WM_PAINT:
        begin
          Result := CallOld;
          if wParam = 0 then
            DrawSizeGrip(Info^.GripRect);
        end;

      WM_NCHITTEST:
        begin
          pt.X := GET_X_LPARAM(lParam);
          pt.Y := GET_Y_LPARAM(lParam);
          ScreenToClient(hWnd, pt);
          if PtInRect(Info^.GripRect, pt) then
            Result := HTBOTTOMRIGHT
          else
            Result := CallOld;
        end;

      WM_SIZE:
        begin
          InvalidateGrip;
          Result := CallOld;
          UpdateGrip;
        end;

      else
        Result := CallOld;
    end; // case Msg of
  end;
end { SizeGripWndProc };

{ Note that SetWindowSizeGrip(..., false) does not really remove the hook -
  it just sets "Enabled" to false. The hook plus all data is removed when
  the window is destroyed.
}
procedure GxSetWindowSizeGrip(hWnd: HWND; Enable: Boolean);
var
  Info: PGripInfo;
begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if (Info = nil) and Enable then
  begin
    New(Info);
    ZeroMemory(Info, SizeOf(TGripInfo));

    with Info^ do
    begin
      Info^.OldWndProc := TWndProc(Pointer(GetWindowLong(hWnd, GWL_WNDPROC)));
      GetGripRect(hWnd, GripRect);
    end;

    SetProp(hWnd, SizeGripProp, NativeUInt(Info));
    SetWindowLong(hWnd, GWL_WNDPROC, NativeInt(@SizeGripWndProc));
  end;

  if (Info <> nil) then
    if Enable <> Info^.Enabled then
      with Info^ do
      begin
        Enabled := Enable;
        if (GripRect.Right > GripRect.Left) and
           (GripRect.Bottom > GripRect.Top) then
          InvalidateRect(hWnd, @GripRect, true);
      end;
end;

end.
