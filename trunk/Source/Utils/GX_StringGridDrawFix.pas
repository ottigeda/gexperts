unit GX_StringGridDrawFix;

{$I 'GX_CondDefine.inc'}

{$IFOPT D+}
{$DEFINE DEBUG_GRID_DRAWING}
{$ELSE}
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
  DbugIntf;

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
    if gdRowSelected in _State then
      Result := Result + ', gdRowSelected';
    if gdHotTrack in _State then
      Result := Result + ', gdHotTrack';
    if gdPressed in _State then
      Result := Result + ', gdPressed';
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
  cnv.FillRect(_Rect);
{$IFDEF STRING_GRID_OWNERDRAW_FIX_ENABLED}
  if GetBorlandIdeVersion in [ideRS104U1, ideRS104U2] then begin
    // in Update 1 they apparently fixed the fix (or at least made it consistently wrong again)
    cnv.TextRect(_Rect, _Rect.Left + 6, _Rect.Top + 2, _Text)
  end else if GetBorlandIdeVersion in [ideRS104P2] then begin
    // Embarcadero managed to bungle the StringGrid redraw fix in patch 2. Now we have to
    // check whether the grid is focused and use a different x offset in that case.
    if _Focused then
      cnv.TextRect(_Rect, _Rect.Left + 6, _Rect.Top + 2, _Text)
    else
      cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
  end else begin
    // fix for original bug
    cnv.TextRect(_Rect, _Rect.Left, _Rect.Top, _Text);
  end;
{$ELSE}
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
{$ENDIF}
end;

end.



