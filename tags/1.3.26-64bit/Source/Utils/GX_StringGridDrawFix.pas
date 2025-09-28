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

procedure DetermineTextOffsets(_Focused: Boolean; out _XOffset, _YOffset: Integer);
begin
  // Embarcdadero managed to break owner drawing for string grids several times, so we need a case
  // statement to determine the offsets (previously this was a rater complex IFDEF construct, which
  // might have been faster but definitely less readable)
  case GetBorlandIdeVersion of
    ideD600..ideRSXE1U1
      : begin
        // Versions before XE2 worked fine with an offset of 2 for both, x and y
        _XOffset := 2;
        _YOffset := 2;
      end;
    ideCSB100, // C# Builder
      ideBCB600, ideBCB601, ideBCB602, ideBCB604, // C++Builder
      ideKylix100, ideKylix200, ideKylix300 // Kylix
      : begin
        // I have never tested these, but I assume that these versions also work with 2 for both
        // If not that probably won't matter anyway because who still uses this old stuff?
        _XOffset := 2;
        _YOffset := 2;
      end;
    ideRSXE2..ideRS103U3
      : begin
        // from XE2 onwards we need a different y offset
          _XOffset := 2;
          _YOffset := 0;
      end;
    ideRS104
      : begin
        // fix for original bug in Delphi 10.4
        _XOffset := 0;
        _YOffset := 0;
      end;
    ideRS104P2
      : begin
        // Embarcadero managed to bungle the StringGrid redraw fix in RS 10.4 patch 2.
        // Now we have to check whether the grid is focused and use a different x offset in that case.
        if _Focused then begin
          _XOffset := 6;
          _YOffset := 2;
        end else begin
          _XOffset := 2;
          _YOffset := 2;
        end;
      end;
    ideRS104U1, ideRS104U2
      : begin
        // in RS 10.4 Update 1 they apparently fixed the fix (or at least made it consistently wrong again)
        _XOffset := 6;
        _YOffset := 2;
      end;
    ideRS11..ideRS12U1
      : begin
        _XOffset := 6;
        _YOffset := 2;
      end;
  else
    // we optimistically assume that they won't break it again in future versions
    _XOffset := 6;
    _YOffset := 2;
  end;
end;

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

procedure TStringGrid_DrawCellFixed(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
  _State: TGridDrawState; _Focused: Boolean);
var
  cnv: TCanvas;
  XOffset: Integer;
  YOffset: Integer;
begin

  cnv := _sg.Canvas;
  // todo: FillRect might also need to use other offsets apparently since Delphi XE2
  cnv.FillRect(_Rect);

  DetermineTextOffsets(_Focused, XOffset, YOffset);
{$IFDEF DEBUG_GRID_DRAWING}
  SendDebugFmt('Drawing grid %s: DefaultRowHeight: %d Rect.Left: %d .Top: %d  .Width: %d .Height: %d .Focused: %s .State: %s XOffs: %d YOffs: %d',
    [_sg.Name, _sg.DefaultRowHeight, _Rect.Left, _Rect.Top,
      _Rect.Right - _Rect.Left, _Rect.Bottom - _Rect.Top,
      BoolToStr(_Focused), StateStr(_State), XOffset, YOffset]);
{$ENDIF}
  cnv.TextRect(_Rect, _Rect.Left + XOffset, _Rect.Top + YOffset, _Text);
end;

end.

