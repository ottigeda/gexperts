unit u_dzTypesUtils;

interface

uses
  Types,
  SysUtils;

///<summary> Returns the Rect's width </summary>
function TRect_Width(const _Rect: TRect): Integer;

///<summary> Returns the Rect's height </summary>
function TRect_Height(const _Rect: TRect): Integer;

///<summary>
/// @returns a TRect generated from Left, Top, Width and Height </summary>
function TRect_FromLTWH(_l, _t, _w, _h: Integer): TRect;

///<summary> returns the center point of the Rect </summary>
function TRect_Center(const _Rect: TRect): TPoint;

///<summary>
/// Check whether a TRect contains a TPoint </summary>
function TRect_Contains(const _Rect: TRect; const _Pnt: TPoint): Boolean; overload;

///<summary>
/// Check whether a TRect contains a point with the given coordinates </summary>
function TRect_Contains(const _Rect: TRect; _x, _y: Integer): Boolean; overload;

implementation

function TRect_Width(const _Rect: TRect): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := _Rect.Right - _Rect.Left;
end;

function TRect_Height(const _Rect: TRect): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := _Rect.Bottom - _Rect.Top;
end;

function TRect_FromLTWH(_l, _t, _w, _h: Integer): TRect;
begin
  Result := Rect(_l, _t, _l + _w, _t + _h);
end;

function TRect_Center(const _Rect: TRect): TPoint;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  // in theory this can lead to an integer overflow, if the rect-coordinates are very large.
  Result := Point((_Rect.Left + _Rect.Right) div 2, (_Rect.Top + _Rect.Bottom) div 2);
end;

function TRect_Contains(const _Rect: TRect; const _Pnt: TPoint): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := TRect_Contains(_Rect, _Pnt.x, _Pnt.y);
end;

function TRect_Contains(const _Rect: TRect; _x, _y: Integer): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := (_Rect.Left <= _x) and (_Rect.Right >= _x)
    and (_Rect.Top <= _y) and (_Rect.Bottom >= _y);
end;

end.
