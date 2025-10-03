unit GX_StringGridDrawFix;

{$I 'GX_CondDefine.inc'}

{.$DEFINE DEBUG_GRID_DRAWING}

{$IFOPT D-}
{$UNDEF DEBUG_GRID_DRAWING}
{$ENDIF}

interface

uses
  SysUtils,
  Types,
  Grids;

procedure TStringGrid_DrawCellFixed(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
  _State: TGridDrawState; _Focused: Boolean);

implementation

uses
  Graphics,
  GX_GetIdeVersion,
  GX_DbugIntf,
  u_dzStringUtils;

procedure TStringGrid_DrawCellFixed(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
  _State: TGridDrawState; _Focused: Boolean);

{$IFDEF DEBUG_GRID_DRAWING}
  function StateStr(_State: TGridDrawState): string;
  begin
    Result := '';
    if gdSelected in _State then
      Result := Result + ', gdSelected';
    if gdFocused in _State then
      Result := Result + ', gdFocused';
    if gdFixed in _State then
      Result := Result + ', gdFixed';
{$IF Declared(gdRowSelected)}
    if gdRowSelected in _State then
      Result := Result + ', gdRowSelected';
{$IFEND}
{$IF Declared(gdHotTrack)}
    if gdHotTrack in _State then
      Result := Result + ', gdHotTrack';
{$IFEND}
{$IF Declared(gdPressed)}
    if gdPressed in _State then
      Result := Result + ', gdPressed';
{$IFEND}
    if Result <> '' then
      Result := Copy(Result, 3);
  end;
{$ENDIF DEBUG_GRID_DRAWING}

var
  cnv: TCanvas;
begin
{$IFDEF DEBUG_GRID_DRAWING}
  SendDebugFmt('Drawing grid %s: DefaultRowHeight: %d Rect.Left: %d .Top: %d  .Width: %d .Height: %d .Focused: %s .State: %s',
    [_sg.Name, _sg.DefaultRowHeight, _Rect.Left, _Rect.Top,
      _Rect.Right - _Rect.Left, _Rect.Bottom - _Rect.Top,
      BoolToStr(_Focused), StateStr(_State)]);
{$ENDIF}

  cnv := _sg.Canvas;
  // todo: FillRect might also need to use other offsets apparently since Delphi XE2
  cnv.FillRect(_Rect);
{$IFNDEF GX_DELPHIXE2_UP}
  // Versions before XE2 worked fine with an offset of 2 for both, x and y
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
{$ELSE}
{$IFNDEF GX_DELPHI10_4_UP}
  // Delphi XE2 up to 10.3 required an offset of 2 for x and 1 for y
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 1, _Text);
{$ELSE}
{$IFDEF GX_DELPHI11_UP}
  // Delphi 11 is again broken differently:
  // We still need an offset of 6 for x but again an offset of 2 for y
  // (Maybe that is only the case for Delphi 11.1. I can't check for Delphi 11.0 because
  // I have already updated.)
  cnv.TextRect(_Rect, _Rect.Left + 6, _Rect.Top + 2, _Text)
{$ELSE}
  // so, now to the completely broken versions:
  // In Delphi 10.4, up to patch 2 owner drawn StringGrids were broken
  if GetBorlandIdeVersion in [ideRS104U1, ideRS104U2] then begin
    // in RS 10.4 Update 1 they apparently fixed the fix (or at least made it consistently wrong again)
    cnv.TextRect(_Rect, _Rect.Left + 6, _Rect.Top + 1, _Text)
  end else if GetBorlandIdeVersion in [ideRS104P2] then begin
    // Embarcadero managed to bungle the StringGrid redraw fix in RS 10.4 patch 2.
    // Now we have to check whether the grid is focused and use a different x offset in that case.
    if _Focused then
      cnv.TextRect(_Rect, _Rect.Left + 6, _Rect.Top + 2, _Text)
    else
      cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
  end else begin
    // fix for original bug
    cnv.TextRect(_Rect, _Rect.Left, _Rect.Top, _Text);
  end;
{$ENDIF GX_DELPHI11_UP}
{$ENDIF GX_DELPHI10_4_UP}
{$ENDIF  GX_DELPHIXE2_UP}
end;

end.
