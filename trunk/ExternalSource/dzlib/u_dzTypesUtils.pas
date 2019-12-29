unit u_dzTypesUtils;

interface

uses
  Types,
  SysUtils;

///<summary> Returns the Rect's width </summary>
function TRect_Width(_Rect: TRect): Integer; inline;

///<summary> Returns the Rect's height </summary>
function TRect_Height(_Rect: TRect): Integer; inline;

///<summary>
/// @returns a TRect generated from Left, Top, Width and Height </summary>
function TRect_FromLTWH(_l, _t, _w, _h: Integer): TRect;

///<summary> returns the center point of the Rect </summary>
function TRect_Center(_Rect: TRect): TPoint; inline;

///<summary>
/// Check whether a TRect contains a TPoint </summary>
function TRect_Contains(_Rect: TRect; _Pnt: TPoint): Boolean; inline; overload;

///<summary>
/// Check whether a TRect contains a point with the given coordinates </summary>
function TRect_Contains(_Rect: TRect; _x, _y: Integer): Boolean; inline; overload;

implementation

function TRect_Width(_Rect: TRect): Integer; inline;
begin
  Result := _Rect.Right - _Rect.Left;
end;

function TRect_Height(_Rect: TRect): Integer; inline;
begin
  Result := _Rect.Bottom - _Rect.Top;
end;

function TRect_FromLTWH(_l, _t, _w, _h: Integer): TRect;
begin
  Result := Rect(_l, _t, _l + _w, _t + _h);
end;

function TRect_Center(_Rect: TRect): TPoint; inline;
begin
  // in theory this can lead to an integer overflow, if the rect-coordinates are very large.
  Result := Point((_Rect.Left + _Rect.Right) div 2, (_Rect.Top + _Rect.Bottom) div 2);
end;

function TRect_Contains(_Rect: TRect; _Pnt: TPoint): Boolean; inline;
begin
  Result := TRect_Contains(_Rect, _Pnt.x, _Pnt.y);
end;

function TRect_Contains(_Rect: TRect; _x, _y: Integer): Boolean; inline; overload;
begin
  Result := (_Rect.Left <= _x) and (_Rect.Right >= _x)
    and (_Rect.Top <= _y) and (_Rect.Bottom >= _y);
end;

end.
