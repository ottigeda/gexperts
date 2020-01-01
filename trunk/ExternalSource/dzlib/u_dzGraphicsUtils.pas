unit u_dzGraphicsUtils;

{$INCLUDE 'dzlib.inc'}

{.$OPTIMIZATION ON}

{$IFOPT O-} // Optimization
{$MESSAGE WARN 'optimization is off, consider turning it on for significantly better performance'}
{$ENDIF}

{.$DEFINE dzUseGraphics32}

interface

uses
  Windows,
  Types,
  SysUtils,
  Graphics,
{$IFDEF dzUseGraphics32}
  GR32, // libs\graphics32\src
{$ENDIF}
  u_dzTranslator,
  u_dzTypesUtils;

type
  TRgbBrightnessChannelEnum = (rcbAverage, rcbFastLuminance, rcbRed, rcbGreen, rcbBlue, rcbLuminance);

const
  // Constant from GraphUtil (implementation section)
  HLSMAX = 240; // H,L, and S vary over 0-HLSMAX

type
{$IFDEF dzUseGraphics32}
  THlsValueType = Single; // 0..1
{$ELSE}
  THlsValueType = Word; // 0..HLSMAX (240)
{$ENDIF}
  THlsRec = record
    Hue: THlsValueType;
    Luminance: THlsValueType;
    Saturation: THlsValueType;
  end;

type
  TdzRgbTriple = packed record
    // do not change the order of the fields, do not add any fields
    Blue: Byte;
    Green: Byte;
    Red: Byte;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    function GetColor: TColor;
    procedure SetColor(_Color: TColor);
    procedure SetGray(_Value: Byte);
    function GetLuminance: Byte;
    function GetFastLuminance: Byte; overload;
    class function GetFastLuminance(_Red, _Green, _Blue: Byte): Byte; overload; static; inline;
    function GetBrightness(_Channel: TRgbBrightnessChannelEnum): Byte;
    procedure SetBrightness(_Value: Byte);
    procedure GetHls(out _Hls: THlsRec);
    procedure SetHls(const _Hls: THlsRec);
{$ENDIF}
  end;

function TdzRgbTriple_GetFastLuminance(const _Triple: TdzRgbTriple): Byte;
procedure TdzRgbTriple_SetColor(var _Triple: TdzRgbTriple; _Color: TColor);

function GetFastLuminance(_Red, _Green, _Blue: Byte): Byte;

type
  TdzRgbTripleArray = packed array[0..MaxInt div SizeOf(TdzRgbTriple) - 1] of TdzRgbTriple;
  PdzRgbTripleArray = ^TdzRgbTripleArray;

type
  TdzRgbQuad = packed record
    // do not change the order of the fields, do not add any fields
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Reserved: Byte;
{$IFDEF SUPPORTS_ENHANCED_RECORDS}
    function GetColor: TColor;
    procedure SetColor(_Color: TColor);
    procedure SetGray(_Value: Byte);
    function GetLuminance: Word;
    function GetFastLuminance: Word;
    function GetBrightness(_Channel: TRgbBrightnessChannelEnum): Word;
    procedure SetBrightness(_Value: Byte);
    procedure GetHls(out _Hue, _Luminance, _Saturation: Word);
    procedure SetHls(_Hue, _Luminance, _Saturation: Word);
{$ENDIF}
  end;

type
  TdzRgbQuadArray = packed array[0..MaxInt div SizeOf(TdzRgbQuad) - 1] of TdzRgbQuad;
  PdzRgbQuadArray = ^TdzRgbQuadArray;

///<summary> Returns the bounding box of the active clipping region </summary>
function TCanvas_GetClipRect(_Canvas: TCanvas): TRect;
///<summary> Sets a clipping rect, returns true, if the region is not empty, false if it is empty </summary>
function TCanvas_SetClipRect(_Canvas: TCanvas; _Rect: TRect): Boolean;

type
  TDrawTextFlags = (
    dtfLeft, dtfRight, dtfCenter, // horizontal alignment
    dtfWordBreak, // Breaks words. Lines are automatically broken between words if a word would
                  // extend past the edge of the rectangle specified by the lpRect parameter.
                  // A carriage return-line feed sequence also breaks the line.
                  // If this is not specified, output is on one line.
    dtfCalcRect, // Determines the width and height of the rectangle. If there are multiple lines
                 // of text, DrawText uses the width of the rectangle pointed to by the lpRect
                 // parameter and extends the base of the rectangle to bound the last line of text.
                 // If the largest word is wider than the rectangle, the width is expanded.
                 // If the text is less than the width of the rectangle, the width is reduced.
                 // If there is only one line of text, DrawText modifies the right side of the
                 // rectangle so that it bounds the last character in the line. In either case,
                 // DrawText returns the height of the formatted text but does not draw the text.
    dtfNoClip); // draw without clipping (slightly faster)
// not implemented:
//    dtfSingleLine, // only print as single line (ignore line breaks)
//    dtfTopSingle, dtfBottomSingle, dtfVCenterSingle, // vertical alignment, only if dtfSingleLine is given
//    dtfPathEllipsis, // replace characters in the middle of the string with ellipses ('...') so that
                     // the result fits in the specified rectangle. If the string contains backslash
                     // (\) characters, preserves as much as possible of the text after the last backslash.
//    dtfEndEllipsis, // if the end of a string does not fit in the rectangle, it is truncated and
                    // ellipses ('...') are added. If a word that is not at the end of the string
                    // goes beyond the limits of the rectangle, it is truncated without ellipses.
                    // (Unless dtfWordEllipsis is also specified.)
//    dtfWordEllipsis, // Truncates any word that does not fit in the rectangle and adds ellipses ('...').
//    dtfModifyStringEllipsis, // if given, together with one of the dtfXxxEllipsis flags, the
                             // string is modified to matcht the output.
//    dtfEditControl,
//    dtfExpandTabs, dtfExternalLeading, dtfHidePrefix, dtfInternal,
//    dtfNoFullWidthCharBreak, dtfNoPrefix,
//    dtfPrefixOnly, dtRtlReading, dtfTabStop,
  TDrawTextFlagSet = set of TDrawTextFlags;

///<summary>
/// Calculates the Rect necessary for drawing the text.
/// @returns the calculated height </summary>
function TCanvas_DrawText(_Canvas: TCanvas; const _Text: string; var _Rect: TRect; _Flags: TDrawTextFlagSet): Integer;

///<summary> calls Windows.SaveDC and returns an interface which will automatically call
///          Windows.RestoreDC when destroyed </summary>
function TCanvas_SaveDC(_Canvas: TCanvas): IInterface;

procedure TCanvas_DrawArrow(_Canvas: TCanvas; _From, _To: TPoint; _ArrowHeadLength: Integer = 15);

///<summary>
/// Draws an isoceles right triangle. The tip is either on the top or bottom, depending
/// on the sign of the Height parameter.
/// @param Canvas is the canvas to draw on
/// @param Tip is the coordinates of the vertex point
/// @param Height is the height of the triangle, if negative, the triangle is painted upside down </summary>
procedure TCanvas_DrawTriangle(_Canvas: TCanvas; _Tip: TPoint; _Height: Integer);

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;

///<summary> abbreviation for StretchBlt that takes TRect </summary>
function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect;
  _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD = SRCCOPY): LongBool; overload;

///<summary> abbreviation for StretchBlt that takes TCanvas and TRect </summary>
function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect;
  _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD = SRCCOPY): LongBool; overload;

///<summary> abbreviation for StretchBlt that takes TRect and TBitmap </summary>
function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect;
  _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;

///<summary> abbreviation for StretchBlt that takes TCanvas, TRect and TBitmap </summary>
function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect;
  _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;

///<summary>
/// Abbreviation for StretchBlt that takes two TBitmap, resizes and keeps the spect ratio,
/// using stretchmode HALFTONE (which usually gives the best quality but is a bit slower).
/// The original stretchmode and the brush origin are preserved.
/// https://msdn.microsoft.com/en-us/library/windows/desktop/dd145089(v=vs.85).aspx </summary>
function dzStretchBlt(_DestBmp, _SrcBmp: TBitmap; _Rop: DWORD = SRCCOPY): LongBool; overload;

///<summary> abbreviation for BitBlt that takes TPoint / TRect and TBitmap parameters </summary>
function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

function dzBitBlt(_DestHandle: Hdc; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

procedure TBitmap_SetSize(_bmp: TBitmap; _Width, _Height: integer);

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
function TBitmap_BitBlt(_DestBmp: TBitmap; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool; overload;
function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap;
  _Rop: DWORD = SRCCOPY): LongBool; overload;

///<summary> load a jpeg file and assign it to the bitmap </summary>
procedure TBitmap_LoadJpg(_bmp: TBitmap; const _JpgFn: string); overload;
{$IF Declared(TBitmap32)}
procedure TBitmap_LoadJpg(_bmp: TBitmap32; const _JpgFn: string); overload;
{$IFEND}

///<summary> save a bitmap as a jpeg file </summary>
procedure TBitmap_SaveJpg(_bmp: TBitmap; const _JpgFn: string);

///<summary>
/// Assign a buffer containg a bitmap in BGR 8 format to the TBitmap </summary>
procedure TBitmap_AssignBgr8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);

///<summary>
/// Assign a buffer containg a bitmap in RGB 8 format to the TBitmap
/// @NOTE: This is much slower than TBitmap_AssignBgr8, so if you have got the choice,
///        go with BGR 8 format. </summary>
procedure TBitmap_AssignRgb8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);

///<summary>
/// Assign a buffer containg a bitmap in Mono 8 format to the TBitmap with 24 bit colors </summary>
procedure TBitmap_AssignMono824(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);

///<summary>
/// Assign a buffer containg a bitmap in Mono 8 format to a 8 bit gray scale TBitmap </summary>
procedure TBitmap_AssignMono8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);

///<summary>
/// converts a pf24bit or pf32bit monochrome bitmap to a pf8bit monochrome bitmap </summary>
function TBitmap_MonoToMono8(_bmp: TBitmap): TBitmap; overload;
procedure TBitmap_MonoToMono8(_InBmp, _OutBmp: TBitmap); overload;

///<summary>
/// Makes the given bitmap pf8Bit grayscale </summary>
procedure TBitmap_MakeMono8(_bmp: TBitmap);

///<summary>
/// Calculates the positive y coordinate for the given x coordinate for an ellipse
/// with horizontal and vertical axes, centered on the coordinate origin (0/0).
/// @param a, b are horizontal and vertical radius values
/// @param x is the x coordinate to calculate the coordinate for
/// @param y returns the y coordinate if it can be calculated
/// @returns true if the x coordinate was inside the ellipse, false if not </summary>
function TryCalcEllipsePoint(_a, _b, _x: Extended; out _y: Extended): Boolean;

///<summary>
/// Calculates both y coordinates for the given x coordinate for an ellipse
/// with horizontal and vertical axes and the given center.
/// @aram x0, y0 are the coordinates of the ellipsis center
/// @param a, b are horizontal and vertical radius values
/// @param x is the x coordinate to calculate the coordinate for
/// @param y1, y2 return the y coordinates if they can be calculated
/// @returns true if the x coordinate was inside the ellipse, false if not </summary>
function TryCalcEllipsePoints(_x0, _y0, _a, _b, _x: Extended; out _y1, _y2: Extended): Boolean;

///<summary>
/// Blurs a rectangular area in the given bitmap.
/// @param bmp is the bitmap to process
/// @param Minx, MaxX, MinY, MaxY describe the area to blur
/// @param passes is the number of blurring passes </summary>
procedure TBitmap_BlurRect(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);

///<summary>
/// Blurs an elliptic area in the given bitmap.
/// @param bmp is the bitmap to process
/// @param Minx, MaxX, MinY, MaxY describe the area to blur
/// @param passes is the number of blurring passes </summary>
procedure TBitmap_BlurEllipse(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);

///<summary>
/// Sharpens a bitmap, pixelformat must be pf24bit
/// @param SrcBmp is the input
/// @param DstBmp is the output (the sharpened picture)
/// @param Alpha is the sharpen factor, must be >=0 and <=5
/// source: https://www.swissdelphicenter.ch/en/showcode.php?id=1948
///         but changed alpha interval from ]1..6[ to [0..5]  </summary>
procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single); overload;
procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single); overload;
procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single); overload;

type
  TSingleMatrix = array of array of Single;

///<summary>
/// Sharpens a bitmap, pixelformat must be pf24bit
/// @param SrcBmp is the input
/// @param DstBmp is the output (the sharpened picture)
/// @param Alpha is a matrix of sharpen factors for each pixel
///        Dimensions must match the bitmap, *remember* *that* *y* *is* *reversed*.
///        Values must be >= 0 and <=5 </summary>
procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix); overload;
procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix); overload;
procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix); overload;

type
  // Note: The bitmap is stored upside down, so the y coordinates are reversed!
  TPixel24FilterCallback = procedure(_x, _y: Integer; var _Pixel: TdzRgbTriple) of object;
  TPixel8FilterCallback = procedure(_x, _y: Integer; var _Pixel: Byte) of object;

///<summary>
/// Calls the given callback procedure for each pixel of the bitmap </summary>
procedure TBitmap24_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel24FilterCallback);
procedure TBitmap8_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel8FilterCallback);

type
  ///<summary>
  /// PixelFilter for cutting off R, G and B values at the given CutOff value
  /// This reduches brightness of white pixels by cutting of brightness at a given value.
  /// Assumes a gray scale bitmap where white means R=255, G=255 and B=255. </summary>
  TPixelFilterCutoff = class
  private
    FCutOff: Byte;
  public
    constructor Create(_CutOff: Byte);
    // Note: The bitmap is stored upside down, so the y coordinates are reversed!
    procedure FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple); overload;
    procedure FilterCallback(_x, _y: Integer; var _Pixel: Byte); overload;
  end;

type
  TPixelFilterStretch = class
  private
    FLowCutOff: Byte;
    FHighCutOff: Byte;
    FFactor: Extended;
    procedure StretchColor(var _Color: Byte);
  public
    constructor Create(_LowCutOff, _HighCutOff: Byte);
    procedure FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple); overload;
    procedure FilterCallback(_x, _y: Integer; var _Pixel: Byte); overload;
  end;

type
  TPixelFilterMove = class
  private
    FMoveBy: Integer;
    procedure MoveColor(var _Color: Byte);
  public
    constructor Create(_MoveBy: Integer);
    procedure FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple); overload;
    procedure FilterCallback(_x, _y: Integer; var _Pixel: Byte); overload;
  end;

type
  TNumColors = 1..256;

function MakeGrayPalette(_NumColors: TNumColors = 256): HPALETTE;

///<summary>
// Calculates the (perceived) brightness of an RGB color value (luminance) </summary>
function ColorBrightness(_Red, _Green, _Blue: Byte): Byte; overload;
///<summary>
// Calculates the (perceived) brightness of a TColor value (luminance) </summary>
function ColorBrightness(_Color: TColor): Byte; overload;

///<summary>
/// @returns clWhite or clBlack depending on the brightness (luminance) of the color </summary>
function BestForegroundForColor(_Red, _Green, _Blue: Byte): TColor; overload;
///<summary>
/// @returns clWhite or clBlack depending on the brightness (luminance) of the color </summary>
function BestForegroundForColor(_Color: TColor): TColor; overload;

function RainbowColor(_Hue: Double): TColor; overload;
function RainbowColor(_MinHue, _MaxHue, _Hue: Integer): TColor; overload;

function TryStr2Color(const _s: string; out _Color: TColor): Boolean;

implementation

uses
  Math,
  GraphUtil,
  jpeg, // if you get a compile error here you might need to add Vcl.Imaging to the unit scope names
  u_dzConvertUtils;

function _(const _s: string): string;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzDGetText(_s, 'dzlib');
end;

function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect; _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := StretchBlt(_DestHandle, _DestRect.Left, _DestRect.Top, TRect_Width(_DestRect), TRect_Height(_DestRect),
    _SrcHandle, _SrcRect.Left, _SrcRect.Top, TRect_Width(_SrcRect), TRect_Height(_SrcRect), _Rop);
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect; _SrcHandle: Hdc; _SrcRect: TRect; _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzStretchBlt(_DestCnv.Handle, _DestRect, _SrcHandle, _SrcRect, _Rop);
end;

function dzStretchBlt(_DestHandle: Hdc; _DestRect: TRect; _Src: TBitmap; _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := StretchBlt(_DestHandle, _DestRect.Left, _DestRect.Top, TRect_Width(_DestRect), TRect_Height(_DestRect),
    _Src.Canvas.Handle, 0, 0, _Src.Width, _Src.Height, _Rop);
end;

function dzStretchBlt(_DestCnv: TCanvas; _DestRect: TRect; _Src: TBitmap; _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzStretchBlt(_DestCnv.Handle, _DestRect, _Src, _Rop);
end;

function dzStretchBlt(_DestBmp, _SrcBmp: TBitmap; _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
var
  DstHandle: Hdc;
  OrigBltMode: Integer;
  OrigBrushOrigin: TPoint;
  wSrc: Integer;
  hSrc: Integer;
  x: Int64;
  y: Integer;
  wDst: Integer;
  hDst: Integer;
begin
  DstHandle := _DestBmp.Canvas.Handle;
  OrigBltMode := GetStretchBltMode(DstHandle);
  try
    SetBrushOrgEx(DstHandle, 0, 0, @OrigBrushOrigin);
    SetStretchBltMode(DstHandle, HALFTONE);
    wDst := _DestBmp.Width;
    hDst := _DestBmp.Height;
    wSrc := _SrcBmp.Width;
    hSrc := _SrcBmp.Height;
    if (hSrc = 0) or (wSrc = 0) then begin
      // SrcBmp is empty, nothing to do
      // todo: Should this clear DestBmp?
      Result := False;
    end else begin
      if hSrc > wSrc then begin
        x := Round((wDst * (hSrc - wSrc)) / 2 / hSrc);
        y := 0;
        wDst := Round(wDst * wSrc / hSrc);
      end else begin
        x := 0;
        y := Round((hDst * (wSrc - hSrc)) / 2 / wSrc);
        hDst := Round(hDst * hSrc / wSrc);
      end;
      Result := dzStretchBlt(DstHandle, Rect(x, y, x + wDst - 1, y + hDst - 1), _SrcBmp);
    end;
  finally
    SetStretchBltMode(DstHandle, OrigBltMode);
    SetBrushOrgEx(DstHandle, OrigBrushOrigin.x, OrigBrushOrigin.y, nil);
  end;
end;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestPos.x, _DestPos.y,
    _Size.x, _Size.y,
    _Src.Canvas.Handle,
    _SrcPos.x, _SrcPos.y, _Rop);
end;

function dzBitBlt(_DestHandle: Hdc; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestRect.Left, _DestRect.Top,
    _DestRect.Right - _DestRect.Left + 1, _DestRect.Bottom - _DestRect.Top + 1,
    _Src.Canvas.Handle,
    _SrcPos.x, _SrcPos.y, _Rop);
end;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestPos.x, _DestPos.y,
    _Src.Width, _Src.Height,
    _Src.Canvas.Handle,
    _SrcPos.x, _SrcPos.y,
    _Rop);
end;

function dzBitBlt(_DestHandle: Hdc; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD): LongBool; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := Windows.BitBlt(
    _DestHandle,
    _DestPos.x, _DestPos.y,
    _Src.Width, _Src.Height,
    _Src.Canvas.Handle,
    0, 0,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestPos,
    _Size,
    _Src,
    _SrcPos,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestRect,
    _Src,
    _SrcPos,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestPos,
    _Src,
    _SrcPos,
    _Rop);
end;

function TCanvas_BitBlt(_Canvas: TCanvas; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := dzBitBlt(
    _Canvas.Handle,
    _DestPos,
    _Src,
    Point(0, 0),
    _Rop);
end;

function TCanvas_GetClipRect(_Canvas: TCanvas): TRect;
var
  RGN: THandle;
  Res: Integer;
begin
  RGN := CreateRectRgn(0, 0, 0, 0);
  if RGN = 0 then
    raise Exception.Create(_('CreateRectRgn failed'));
  try
    Res := GetClipRgn(_Canvas.Handle, RGN);
    if Res = -1 then
      raise Exception.Create(_('GetClipRgn failed'));
    GetRgnBox(RGN, Result);
  finally
    DeleteObject(RGN);
  end;
end;

function TCanvas_SetClipRect(_Canvas: TCanvas; _Rect: TRect): Boolean;
var
  RGN: THandle;
  Res: Integer;
begin
  Result := False;
  RGN := CreateRectRgn(_Rect.Left, _Rect.Top, _Rect.Right, _Rect.Bottom);
  if RGN = 0 then
    raise Exception.Create(_('CreateRectRgn failed'));

  try
    Res := SelectClipRgn(_Canvas.Handle, RGN);
    if Res = Error then
      raise Exception.Create(_('SelectClipRgn failed'));
    Result := (Res <> NULLREGION);
  finally
    DeleteObject(RGN);
  end;
end;

function TCanvas_DrawText(_Canvas: TCanvas; const _Text: string; var _Rect: TRect; _Flags: TDrawTextFlagSet): Integer;
var
  Flags: LongWord;
begin
  Flags := 0;
  if dtfLeft in _Flags then
    Flags := Flags or DT_LEFT;
  if dtfRight in _Flags then
    Flags := Flags or DT_RIGHT;
  if dtfCenter in _Flags then
    Flags := Flags or DT_CENTER;
  if dtfWordBreak in _Flags then
    Flags := Flags or DT_WORDBREAK;
  if dtfNoClip in _Flags then
    Flags := Flags or DT_NOCLIP;
  if dtfCalcRect in _Flags then
    Flags := Flags or DT_CALCRECT;
  Result := Windows.DrawText(_Canvas.Handle, PChar(_Text), -1, _Rect, Flags);
end;

type
  TCanvasSaveDC = class(TInterfacedObject)
  private
    FCanvas: TCanvas;
    FSavedDC: Integer;
  public
    constructor Create(_Canvas: TCanvas; _SavedDC: Integer);
    destructor Destroy; override;
  end;

{ TCanvasSaveDC }

constructor TCanvasSaveDC.Create(_Canvas: TCanvas; _SavedDC: Integer);
begin
  inherited Create;
  FCanvas := _Canvas;
  FSavedDC := _SavedDC;
end;

destructor TCanvasSaveDC.Destroy;
begin
  Windows.RestoreDC(FCanvas.Handle, FSavedDC);
  inherited;
end;

function TCanvas_SaveDC(_Canvas: TCanvas): IInterface;
var
  SavedDC: Integer;
begin
  SavedDC := Windows.SaveDC(_Canvas.Handle);
  Result := TCanvasSaveDC.Create(_Canvas, SavedDC);
end;

procedure TCanvas_DrawArrow(_Canvas: TCanvas; _From, _To: TPoint; _ArrowHeadLength: Integer = 15);
// taken from: http://www.efg2.com/Lab/Library/Delphi/Graphics/Arrow.Txt
var
  xbase: Integer;
  xLineDelta: Integer;
  xLineUnitDelta: Double;
  xNormalDelta: Integer;
  xNormalUnitDelta: Double;
  ybase: Integer;
  yLineDelta: Integer;
  yLineUnitDelta: Double;
  yNormalDelta: Integer;
  yNormalUnitDelta: Double;
begin
  _Canvas.MoveTo(_From.x, _From.y);
  _Canvas.LineTo(_To.x, _To.y);

  xLineDelta := _To.x - _From.x;
  yLineDelta := _To.y - _From.y;

  xLineUnitDelta := xLineDelta / Sqrt(Sqr(xLineDelta) + Sqr(yLineDelta));
  yLineUnitDelta := yLineDelta / Sqrt(Sqr(xLineDelta) + Sqr(yLineDelta));

  // (xBase,yBase) is where arrow line is perpendicular to base of triangle.
  xbase := _To.x - Round(_ArrowHeadLength * xLineUnitDelta);
  ybase := _To.y - Round(_ArrowHeadLength * yLineUnitDelta);

  xNormalDelta := yLineDelta;
  yNormalDelta := -xLineDelta;
  xNormalUnitDelta := xNormalDelta / Sqrt(Sqr(xNormalDelta) + Sqr(yNormalDelta));
  yNormalUnitDelta := yNormalDelta / Sqrt(Sqr(xNormalDelta) + Sqr(yNormalDelta));

  // Draw the arrow tip
  _Canvas.Polygon([_To,
      Point(xbase + Round(_ArrowHeadLength * xNormalUnitDelta),
        ybase + Round(_ArrowHeadLength * yNormalUnitDelta)),
      Point(xbase - Round(_ArrowHeadLength * xNormalUnitDelta),
        ybase - Round(_ArrowHeadLength * yNormalUnitDelta))]);
end;

procedure TCanvas_DrawTriangle(_Canvas: TCanvas; _Tip: TPoint; _Height: Integer);
var
  BaselineY: Integer;
  BaselineLeft: Integer;
  BaselineRight: Integer;
begin
  BaselineY := _Tip.y + _Height;
  BaselineLeft := _Tip.x - Abs(_Height);
  BaselineRight := _Tip.x + Abs(_Height);
  _Canvas.Polygon([_Tip, Point(BaselineLeft, BaselineY), Point(BaselineRight, BaselineY)]);
end;

function TdzRgbTriple_GetFastLuminance(const _Triple: TdzRgbTriple): Byte;
begin
  Result := GetFastLuminance(_Triple.Red, _Triple.Green, _Triple.Blue);
end;

procedure TdzRgbTriple_SetColor(var _Triple: TdzRgbTriple; _Color: TColor);
begin
  _Color := ColorToRGB(_Color);
  _Triple.Red := GetRValue(_Color);
  _Triple.Green := GetGValue(_Color);
  _Triple.Blue := GetBValue(_Color);
end;

function GetFastLuminance(_Red, _Green, _Blue: Byte): Byte;
begin
  Result := Round(0.299 * _Red + 0.587 * _Green + 0.114 * _Blue);
end;

{$IFDEF SUPPORTS_ENHANCED_RECORDS}
{ TdzRgbTriple }

function TdzRgbTriple.GetBrightness(_Channel: TRgbBrightnessChannelEnum): Byte;
begin
  case _Channel of
    rcbAverage: Result := Round((Red + Green + Blue) / 3);
    rcbFastLuminance: Result := GetFastLuminance;
    rcbRed: Result := Red;
    rcbGreen: Result := Green;
    rcbBlue: Result := Blue;
  else //  rcbLuminance: ;
{$IFDEF dzUseGraphics32}
    Result := Round(GetLuminance * HLSMAX);
{$ELSE}
    Result := GetLuminance;
{$ENDIF}
  end;
end;

function TdzRgbTriple.GetColor: TColor;
begin
  Result := RGB(Red, Green, Blue);
end;

procedure TdzRgbTriple.SetColor(_Color: TColor);
begin
  _Color := ColorToRGB(_Color);
  Red := GetRValue(_Color);
  Green := GetGValue(_Color);
  Blue := GetBValue(_Color);
end;

procedure TdzRgbTriple.SetBrightness(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
end;

function TdzRgbTriple.GetFastLuminance: Byte;
begin
  Result := Round(0.299 * Red + 0.587 * Green + 0.114 * Blue);
end;

class function TdzRgbTriple.GetFastLuminance(_Red, _Green, _Blue: Byte): Byte;
begin
  Result := Round(0.299 * _Red + 0.587 * _Green + 0.114 * _Blue);
end;

{$IFDEF dzUseGraphics32}

procedure TdzRgbTriple.GetHls(out _Hls: THlsRec);
begin
  GR32.RGBtoHSL(GR32.Color32(GetColor), _Hls.Hue, _Hls.Saturation, _Hls.Luminance);
end;

procedure TdzRgbTriple.SetHls(const _Hls: THlsRec);
begin
  SetColor(GR32.WinColor(GR32.HSLtoRGB(_Hls.Hue, _Hls.Saturation, _Hls.Luminance)));
end;

{$ELSE}

procedure TdzRgbTriple.GetHls(out _Hls: THlsRec);
begin
  ColorRGBToHLS(GetColor, _Hls.Hue, _Hls.Luminance, _Hls.Saturation);
end;

procedure TdzRgbTriple.SetHls(const _Hls: THlsRec);
begin
  SetColor(ColorHLSToRGB(_Hls.Hue, _Hls.Luminance, _Hls.Saturation));
end;

{$ENDIF}

// untested from http://www.swissdelphicenter.ch/en/showcode.php?id=2349
//function RGB2HSV (R,G,B : Byte) : THSV;
//var
//  Min_, Max_, Delta : Double;
//  H , S , V : Double ;
//begin
//  H := 0.0 ;
//  Min_ := Min (Min( R,G ), B);
//  Max_ := Max (Max( R,G ), B);
//  Delta := ( Max_ - Min_ );
//  V := Max_ ;
//  If ( Max_ <> 0.0 ) then
//    S := 255.0 * Delta / Max_
//  else
//    S := 0.0 ;
//  If (S <> 0.0) then
//    begin
//      If R = Max_ then
//        H := (G - B) / Delta
//      else
//        If G = Max_ then
//          H := 2.0 + (B - R) / Delta
//        else
//          If B = Max_ then
//            H := 4.0 + (R - G) / Delta
//    End
//  else
//    H := -1.0 ;
//  H := H * 60 ;
//  If H < 0.0 then H := H + 360.0;
//  with Result Do
//    begin
//      Hue := H ;             // Hue -> 0..360
//      Sat := S * 100 / 255; // Saturation -> 0..100 %
//      Val := V * 100 / 255; // Value - > 0..100 %
//    end;
//end;

//procedure Swap(var _a, _b: Byte);
//var
//  t: Byte;
//begin
//  t := _a;
//  _a := _b;
//  _b := t;
//end;

// untested: Delphi implmementations of
// http://lolengine.net/blog/2013/01/13/fast-rgb-to-hsv
//procedure RGB2HSV(_r, _g, _b: Byte; out _h, _s, _v: Single);
//var
//  k: Single;
//  chroma: Single;
//begin
//  k := 0;
//
//  if _g < _b then begin
//    Swap(_g, _b);
//    k := -1;
//  end;
//
//  if _r < _g then begin
//    Swap(_r, _g);
//    k := -2 / 6 - k;
//  end;
//
//  chroma := _r - min(_g, _b);
//  _h := Abs(k + (_g - _b) / (6 * chroma + 1e-20));
//  _s := chroma / (_r + 1e-20);
//  _v := _r;
//end;

//procedure RGBtoHSL(_r, _g, _b: Byte; out _h, _s, _l: Single);
//var
//  k: Single;
//  lightness: Integer;
//  chroma: Integer;
//begin
//  k := 0.0;
//  if (_g < _b) then begin
//    Swap(_g, _b);
//    k := 6.0;
//  end;
//  if (_r < _g) then begin
//
//    Swap(_r, _g);
//    k := 2.0 - k;
//  end;
//  lightness := _r + min(_g, _b);
//  chroma := _r - min(_g, _b);
//  if (chroma <> 0) then begin
//    _h := Abs((_g - _b) / chroma - k) * 1.0 / 6.0;
//    _s := chroma / (255 - Abs(lightness - 255));
//  end else begin
//    _h := 0.0;
//    _s := 0.0;
//  end;
//  _l = lightness * 1.0 / 510.0;
//end;

function TdzRgbTriple.GetLuminance: Byte;
var
  Hls: THlsRec;
begin
  GetHls(Hls);
{$IFDEF dzUseGraphics32}
  Result := Round(Hls.Luminance * HLSMAX);
{$ELSE}
  Result := Hls.Luminance;
{$ENDIF}
end;

procedure TdzRgbTriple.SetGray(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
end;

{ tdzRgbQuad }

function TdzRgbQuad.GetBrightness(_Channel: TRgbBrightnessChannelEnum): Word;
begin
  case _Channel of
    rcbAverage: Result := Round((Red + Green + Blue) / 3);
    rcbFastLuminance: Result := GetFastLuminance;
    rcbRed: Result := Red;
    rcbGreen: Result := Green;
    rcbBlue: Result := Blue;
  else //  rcbLuminance: ;
    Result := GetLuminance;
  end;
end;

function TdzRgbQuad.GetColor: TColor;
begin
  Result := RGB(Red, Green, Blue);
end;

function TdzRgbQuad.GetFastLuminance: Word;
begin
  Result := Round(0.299 * Red + 0.587 * Green + 0.114 * Blue);
end;

procedure TdzRgbQuad.GetHls(out _Hue, _Luminance, _Saturation: Word);
begin
  ColorRGBToHLS(GetColor, _Hue, _Luminance, _Saturation)
end;

function TdzRgbQuad.GetLuminance: Word;
var
  Hue: Word;
  Saturation: Word;
begin
  GetHls(Hue, Result, Saturation)
end;

procedure TdzRgbQuad.SetBrightness(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
end;

procedure TdzRgbQuad.SetColor(_Color: TColor);
begin
  _Color := ColorToRGB(_Color);
  Red := GetRValue(_Color);
  Green := GetGValue(_Color);
  Blue := GetBValue(_Color);
end;

procedure TdzRgbQuad.SetGray(_Value: Byte);
begin
  Red := _Value;
  Green := _Value;
  Blue := _Value;
end;

procedure TdzRgbQuad.SetHls(_Hue, _Luminance, _Saturation: Word);
begin
  SetColor(ColorHLSToRGB(_Hue, _Luminance, _Saturation));
end;
{$ENDIF}

procedure TBitmap_SetSize(_bmp: TBitmap; _Width, _Height: integer);
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  _bmp.Width := _Width;
  _bmp.Height := _Height;
end;

{$IF Declared(TBitmap32)}

procedure TBitmap_LoadJpg(_bmp: TBitmap32; const _JpgFn: string);
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    jpg.LoadFromFile(_JpgFn);
    _bmp.Assign(jpg);
  finally
    FreeAndNil(jpg);
  end;
end;
{$IFEND}

procedure TBitmap_LoadJpg(_bmp: TBitmap; const _JpgFn: string);
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    jpg.LoadFromFile(_JpgFn);
    _bmp.Assign(jpg);
  finally
    FreeAndNil(jpg);
  end;
end;

procedure TBitmap_SaveJpg(_bmp: TBitmap; const _JpgFn: string);
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  try
    jpg.Assign(_bmp);
    jpg.SaveToFile(_JpgFn);
  finally
    FreeAndNil(jpg);
  end;
end;

function MakeGrayPalette(_NumColors: TNumColors): HPALETTE;
var
  i: Integer;
  lp: TMaxLogPalette;
  Grey: Byte;
begin
  lp.palVersion := $300;
  lp.palNumEntries := _NumColors;
  for i := 0 to _NumColors - 1 do begin
    Grey := i * 255 div _NumColors;
    lp.palPalEntry[i].peRed := Grey;
    lp.palPalEntry[i].peGreen := Grey;
    lp.palPalEntry[i].peBlue := Grey;
    lp.palPalEntry[i].peFlags := PC_RESERVED;
  end;
  Result := CreatePalette(pLogPalette(@lp)^);
end;

procedure TBitmap_AssignBgr8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
var
  y: Integer;
  ScanLine: PdzRgbTripleArray;
  BytesPerLine: Integer;
//  ms: TMemoryStream;
//  bfh: TBitmapFileHeader;
//  bih: TBitmapInfoHeader;
begin
  Assert(_bmp.PixelFormat = pf24bit, 'unexpected PixelFormat (expected pf24bit)');

  BytesPerLine := 3 * _bmp.Width;

//  bfh.bfType := $4D42; // 'BM'
//  bfh.bfSize := BytesPerLine * _Bmp.Height;
//  bfh.bfReserved1 := 0;
//  bfh.bfReserved2 := 0;
//  bfh.bfOffBits := SizeOf(bfh);

//  bih.biSize := SizeOf(bih);
//  bih.biWidth := _Bmp.Width;
//  bih.biHeight := -_Bmp.Height; // origin is upper left corner -> negative
//  bih.biPlanes := 1;
//  bih.biBitCount := 24;
//  bih.biCompression := BI_RGB;
//  bih.biSizeImage := 0; // The size, in bytes, of the image. This may be set to zero for BI_RGB bitmaps.
//  bih.biXPelsPerMeter := 1000;
//  bih.biYPelsPerMeter := 1000;
//  bih.biClrUsed := 0; // The number of color indexes in the color table that are actually used by the bitmap. If this value is zero, the bitmap uses the maximum number of colors corresponding to the value of the biBitCount member for the compression mode specified by biCompression.
//  bih.biClrImportant := 0;

//  ms := TMemoryStream.Create;
//  ms.WriteBuffer(bfh, SizeOf(bfh));
//  ms.WriteBuffer(_Buffer^, bfh.bfSize);

  // Unfortunately the y coordinates of TBitmap are reversed (the picture is upside down).
  // So we can only copy the whole picture in one go, if the buffer is also upside down
  // (many cameras have this feature). If not, we have to copy it one line at a time.
  if _YIsReversed then begin
    ScanLine := _bmp.ScanLine[_bmp.Height - 1];
    Move(_Buffer^, ScanLine^, _bmp.Height * BytesPerLine);
  end else begin
    // At least with GBR8 the bytes have the right order so we can copy the whole line in one go
    for y := 0 to _bmp.Height - 1 do begin
      ScanLine := _bmp.ScanLine[y];
      Move(_Buffer^, ScanLine^, BytesPerLine);
      Inc(_Buffer, BytesPerLine);
    end;
  end;
end;

procedure TBitmap_AssignRgb8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
var
  y: Integer;
  x: Integer;
  ScanLine: PdzRgbTripleArray;
  h: Integer;
begin
  Assert(_bmp.PixelFormat = pf24bit, 'unexpected PixelFormat (expected pf24bit)');

  h := _bmp.Height;
  for y := 0 to h - 1 do begin
    if _YIsReversed then begin
      ScanLine := _bmp.ScanLine[h - y - 1];
    end else begin
      ScanLine := _bmp.ScanLine[y];
    end;
    for x := 0 to _bmp.Width - 1 do begin
      // unfortunately the bytes in the buffer have a different order (RGB) than in the
      // Bitmap (BGR) so we must copy each byte separately
      ScanLine[x].Red := _Buffer^;
      Inc(_Buffer);
      ScanLine[x].Green := _Buffer^;
      Inc(_Buffer);
      ScanLine[x].Blue := _Buffer^;
      Inc(_Buffer);
    end;
  end;
end;

procedure TBitmap_AssignMono824(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
var
  y: Integer;
  x: Integer;
  ScanLine: PdzRgbTripleArray;
  h: Integer;
  Value: Byte;
begin
  Assert(_bmp.PixelFormat = pf24bit, 'unexpected PixelFormat (expected pf24bit)');
  h := _bmp.Height;
  for y := 0 to h - 1 do begin
    if _YIsReversed then begin
      ScanLine := _bmp.ScanLine[h - y - 1];
    end else begin
      ScanLine := _bmp.ScanLine[y];
    end;
    for x := 0 to _bmp.Width - 1 do begin
      // gray scale: Set all colours to the same value
      Value := _Buffer^;
      ScanLine[x].Red := Value;
      ScanLine[x].Green := Value;
      ScanLine[x].Blue := Value;
      Inc(_Buffer);
    end;
  end;
end;

procedure TBitmap_AssignMono8(_Buffer: PByte; _bmp: TBitmap; _YIsReversed: Boolean);
var
  y: Integer;
  ScanLine: PByte;
begin
  Assert(_bmp.PixelFormat = pf8bit, 'unexpected PixelFormat (expected pf8bit)');

  // Unfortunately the y coordinates of TBitmap are reversed (the picture is upside down).
  // So we can only copy the whole picture in one go, if the buffer is also upside down
  // (many cameras have this feature). If not, we have to copy it one line at a time.
  if _YIsReversed then begin
    ScanLine := _bmp.ScanLine[_bmp.Height - 1];
    Move(_Buffer^, ScanLine^, _bmp.Height * _bmp.Width);
  end else begin
    for y := 0 to _bmp.Height - 1 do begin
      ScanLine := _bmp.ScanLine[y];
      Move(_Buffer^, ScanLine^, _bmp.Width);
      Inc(_Buffer, _bmp.Width);
    end;
  end;
end;

type
  PByteArray = SysUtils.PByteArray;
  TCopyScanline = procedure(_Width: Integer; _SrcLine: Pointer; _DestLine: PByteArray);

procedure Copy24Bit(_Width: Integer; _SrcLine: Pointer; _DestLine: PByteArray);
var
  x: Integer;
  SrcLine: PdzRgbTripleArray absolute _SrcLine;
begin
  for x := 0 to _Width - 1 do begin
    _DestLine[x] := SrcLine[x].Blue;
  end;
end;

procedure Copy32Bit(_Width: Integer; _SrcLine: Pointer; _DestLine: PByteArray);
var
  x: Integer;
  SrcLine: PdzRgbQuadArray absolute _SrcLine;
begin
  for x := 0 to _Width - 1 do begin
    _DestLine[x] := SrcLine[x].Blue;
  end;
end;

procedure TBitmap_MonoToMono8(_InBmp, _OutBmp: TBitmap);
var
  CopyScanLine: TCopyScanline;
  w: Integer;
  h: Integer;
  y: Integer;
begin
  _OutBmp.PixelFormat := pf8bit;
  _OutBmp.Palette := MakeGrayPalette();

  if _InBmp.PixelFormat = pf24bit then begin
    CopyScanLine := Copy24Bit
  end else if _InBmp.PixelFormat = pf32bit then begin
    CopyScanLine := Copy32Bit;
  end else
    raise Exception.Create(_('Only bitmaps with PixelFormat = pf32bit or pf24bit are supported'));

  w := _InBmp.Width;
  h := _InBmp.Height;
  _OutBmp.Width := w;
  _OutBmp.Height := h;
  for y := 0 to h - 1 do begin
    CopyScanLine(w, _InBmp.ScanLine[y], _OutBmp.ScanLine[y]);
  end;
end;

function TBitmap_MonoToMono8(_bmp: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  TBitmap_MonoToMono8(_bmp, Result);
end;

procedure TBitmap_MakeMono8(_bmp: TBitmap);
begin
  _bmp.PixelFormat := pf8bit;
  _bmp.Palette := MakeGrayPalette();
end;

// original source: http://www.delphigeist.com/2009/09/blur-bitmap-algorithm.html
// but heavily modified

type
  TByteMatrix = array of array of Byte;
  TBitMatrix = array of array of Boolean;

procedure BlurBuffer(const _In: TByteMatrix; out _out: TByteMatrix);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
begin
  h := Length(_In);
  w := Length(_In[0]);

  SetLength(_out, h);
  // copy first and last line without changes
  for y := 0 to h - 1 do begin
    SetLength(_out[y], w);
    _out[y][0] := _In[y][0];
    _out[y][w - 1] := _In[y][w - 1];
  end;

  // copy first and last column without changes
  for x := 0 to w - 1 do begin
    _out[0][x] := _In[0][x];
    _out[h - 1][x] := _In[h - 1][x];
  end;

  // blur everything else
  for y := 1 to h - 2 do begin
    for x := 1 to w - 2 do begin
      _out[y][x] := (
        _In[y - 1][x - 1]
        + _In[y - 1][x]
        + _In[y - 1][x + 1]
        + _In[y][x - 1]
        // todo: Check whether the point itself should actually be used, maybe
        //       the blurring effect is better without (and if you remove it,
        //       remember change the divisor to 8)
        + _In[y][x]
        + _In[y][x + 1]
        + _In[y + 1][x - 1]
        + _In[y + 1][x]
        + _In[y + 1][x + 1]) div 9;
    end;
  end;
end;

procedure BlurBufferMask(const _In: TByteMatrix; const _Mask: TBitMatrix; out _out: TByteMatrix);
var
  w: Integer;
  h: Integer;
  x: Integer;
  y: Integer;
begin
  h := Length(_In);
  w := Length(_In[0]);

  Assert(Length(_Mask) = h);
  Assert(Length(_Mask[0]) = w);

  SetLength(_out, h);
  // copy first and last line without changes
  for y := 0 to h - 1 do begin
    SetLength(_out[y], w);
    _out[y][0] := _In[y][0];
    _out[y][w - 1] := _In[y][w - 1];
  end;

  // copy first and last column without changes
  for x := 0 to w - 1 do begin
    _out[0][x] := _In[0][x];
    _out[h - 1][x] := _In[h - 1][x];
  end;

  // blur everything else
  for y := 1 to h - 2 do begin
    for x := 1 to w - 2 do begin
      if _Mask[y][x] then begin
        // if the mask is true for this bit -> blur the point
        _out[y][x] := (
          _In[y - 1][x - 1]
          + _In[y - 1][x]
          + _In[y - 1][x + 1]
          + _In[y][x - 1]
          // todo: Check whether the point itself should actually be used, maybe
          //       the blurring effect is better without (and if you remove it,
          //       remember change the divisor to 8)
          + _In[y][x]
          + _In[y][x + 1]
          + _In[y + 1][x - 1]
          + _In[y + 1][x]
          + _In[y + 1][x + 1]) div 9;
      end else begin
        // if not, copy the point unchanged
        _out[y][x] := _In[y][x];
      end;
    end;
  end;
end;

procedure TBitmap_BlurRect(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);
var
  x, y: Integer;
  Line: PdzRgbTripleArray;
  i: Integer;
  BufW: Integer;
  BufH: Integer;
  Buffer1: TByteMatrix;
  Buffer2: TByteMatrix;
begin
  Assert(Assigned(_bmp));

  if _Left < 1 then
    _Left := 1;
  if _Top < 1 then
    _Top := 1;
  if _Right > _bmp.Width - 2 then
    _Right := _bmp.Width - 2;
  if _Bottom > _bmp.Height - 2 then
    _Bottom := _bmp.Height - 2;

  _bmp.PixelFormat := pf24bit;

  // prepare the working buffer for blurring
  BufH := _Bottom - _Top + 1;
  BufW := _Right - _Left + 1;
  SetLength(Buffer1, BufH);
  for i := 0 to BufH - 1 do
    SetLength(Buffer1[i], BufW);

  // blur blue
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Blue;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBuffer(Buffer1, Buffer2);
    BlurBuffer(Buffer2, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Blue := Buffer1[y - _Top][x - _Left];
  end;

  // blur red
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Red;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBuffer(Buffer1, Buffer2);
    BlurBuffer(Buffer2, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Red := Buffer1[y - _Top][x - _Left];
  end;

  // blur green
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Green;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBuffer(Buffer1, Buffer2);
    BlurBuffer(Buffer2, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Green := Buffer1[y - _Top][x - _Left];
  end;
end;

procedure TBitmap_BlurEllipse(_bmp: TBitmap; _Left, _Top, _Right, _Bottom: Integer; _Passes: Integer);
var
  x, y: Integer;
  Line: PdzRgbTripleArray;
  i: Integer;
  BufW: Integer;
  BufH: Integer;
  Buffer1: TByteMatrix;
  Buffer2: TByteMatrix;
  Mask: TBitMatrix;
  x0: Extended;
  y0: Extended;
  a: Extended;
  b: Extended;
  Y1, Y2: Extended;
begin
  Assert(Assigned(_bmp));

  if _Left < 1 then
    _Left := 1;
  if _Top < 1 then
    _Top := 1;
  if _Right > _bmp.Width - 2 then
    _Right := _bmp.Width - 2;
  if _Bottom > _bmp.Height - 2 then
    _Bottom := _bmp.Height - 2;

  _bmp.PixelFormat := pf24bit;

  // prepare the working buffer for blurring and an empty mask
  BufH := _Bottom - _Top + 1;
  BufW := _Right - _Left + 1;
  SetLength(Buffer1, BufH);
  SetLength(Mask, BufH);
  for y := 0 to BufH - 1 do begin
    SetLength(Buffer1[y], BufW);
    SetLength(Mask[y], BufW);
    for x := 0 to BufW - 1 do begin
      Mask[y][x] := False;
    end;
  end;

  // create an eliptic mask
  x0 := (_Left + _Right) / 2;
  y0 := (_Top + _Bottom) / 2;
  a := (_Right - _Left) / 2;
  b := (_Bottom - _Top) / 2;
  for x := _Left to _Right do begin
    if TryCalcEllipsePoints(x0, y0, a, b, x, Y1, Y2) then begin
      for y := Round(Y1) to Round(Y2) do begin
        Mask[y - _Top][x - _Left] := True;
      end;
    end;
  end;

  // blur blue
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Blue;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBufferMask(Buffer1, Mask, Buffer2);
    BlurBufferMask(Buffer2, Mask, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Blue := Buffer1[y - _Top][x - _Left];
  end;

  // blur red
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Red;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBufferMask(Buffer1, Mask, Buffer2);
    BlurBufferMask(Buffer2, Mask, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Red := Buffer1[y - _Top][x - _Left];
  end;

  // blur green
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Buffer1[y - _Top][x - _Left] := Line^[x].Green;
  end;
  for i := 1 to Ceil(_Passes / 2) do begin
    BlurBufferMask(Buffer1, Mask, Buffer2);
    BlurBufferMask(Buffer2, Mask, Buffer1);
  end;
  for y := _Top to _Bottom do begin
    Line := _bmp.ScanLine[y];
    for x := _Left to _Right do
      Line^[x].Green := Buffer1[y - _Top][x - _Left];
  end;
end;

function TryCalcEllipsePoint(_a, _b, _x: Extended; out _y: Extended): Boolean;
var
  sq: Extended;
begin
  sq := 1 - Sqr(_x / _a);
  Result := (CompareValue(sq, 0) = GreaterThanValue);
  if Result then
    _y := _b * Sqrt(sq);
end;

function TryCalcEllipsePoints(_x0, _y0, _a, _b, _x: Extended; out _y1, _y2: Extended): Boolean;
var
  y: Extended;
begin
  Result := TryCalcEllipsePoint(_a, _b, _x - _x0, y);
  if Result then begin
    _y1 := -y + _y0;
    _y2 := y + _y0;
  end;
end;

procedure TBitmap24_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel24FilterCallback);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PdzRgbTripleArray;
  DstLine: PdzRgbTripleArray;
begin
  Assert(Assigned(_SrcBmp));

  _SrcBmp.PixelFormat := pf24bit;
  _DstBmp.PixelFormat := pf24bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  TBitmap_SetSize(_DstBmp, w, h);

  for y := 0 to h - 1 do begin
    SrcLine := _SrcBmp.ScanLine[y];
    DstLine := _DstBmp.ScanLine[y];
    for x := 0 to w - 1 do begin
      DstLine^[x] := SrcLine^[x];
      _Callback(x, y, DstLine^[x]);
    end;
  end;
end;

procedure TBitmap8_FilterPixels(_SrcBmp, _DstBmp: TBitmap; _Callback: TPixel8FilterCallback);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
  SrcLine: PByteArray;
  DstLine: PByteArray;
begin
  Assert(Assigned(_SrcBmp));

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.PixelFormat := pf8bit;
  w := _SrcBmp.Width;
  h := _SrcBmp.Height;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, w, h);

  for y := 0 to h - 1 do begin
    SrcLine := _SrcBmp.ScanLine[y];
    DstLine := _DstBmp.ScanLine[y];
    for x := 0 to w - 1 do begin
      DstLine^[x] := SrcLine^[x];
      _Callback(x, y, DstLine^[x]);
    end;
  end;
end;

// source: https://www.swissdelphicenter.ch/en/showcode.php?id=1948

procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
begin
  case _SrcBmp.PixelFormat of
    pf8bit: TBitmap8_Sharpen(_SrcBmp, _DstBmp, _Alpha);
  else
    TBitmap24_Sharpen(_SrcBmp, _DstBmp, _Alpha);
  end;
end;

procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
  ForcedPixelFormat = pf8bit;
var
  Row, Column: Integer;
  SrcPixelTop: PPixel;
  SrcPixelLeft: PPixel;
  SrcPixelCenter: PPixel;
  SrcPixelRight: PPixel;
  SrcPixelBottom: PPixel;
  SrcRow: PByte;
  DstRow: PByte;
  DstPixel: PPixel;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  CenterBrightness: Integer;
  AvgBrightness: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((_Alpha >= 0) and (_Alpha <= 5), 'Alpha must be >=0 and <=5');

  // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
  Beta := _Alpha / 5;
  _Alpha := _Alpha + 1;

  // integer scaled alpha and beta calculated only once
  IntBeta := Round(Beta * $10000);
  IntAlpha := Round(_Alpha * $10000);

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.PixelFormat := pf8bit;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
//  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  // Copy first row unchanged
  SrcRow := _SrcBmp.ScanLine[0];
  DstRow := _DstBmp.ScanLine[0];
  SrcPixelCenter := PPixel(SrcRow);
  DstPixel := PPixel(DstRow);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;

  for Row := 1 to WorkAreaHeight do begin
    // ScanLine[0] is the line with the highest memory address, so we decrement it by
    // BytesPerLine to get the next line (which would be ScanLine[1])
    // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
    // of the code due the the function call and some overhead in that code.
    Dec(SrcRow, BytesPerLine);
    Dec(DstRow, BytesPerLine);

    SrcPixelCenter := PPixel(SrcRow);
    DstPixel := PPixel(DstRow);

    // 1st column unchanged
    DstPixel^ := SrcPixelCenter^;

    Inc(DstPixel);

    // remember: Bitmaps are stored upside down, so for the previous line, we must add BytesPerLine
    //           and for the next line we must subtract BytesPerLine
    SrcPixelLeft := SrcPixelCenter;
    Inc(SrcPixelCenter);
    SrcPixelRight := SrcPixelCenter;
    Inc(SrcPixelRight);
    SrcPixelTop := PPixel(Integer(SrcPixelCenter) + BytesPerLine);
    SrcPixelBottom := PPixel(Integer(SrcPixelCenter) - BytesPerLine);
    for Column := 1 to WorkAreaWidth do begin
      CenterBrightness := SrcPixelCenter^;

      // calculate the average brightness weighted by -beta
      AvgBrightness := SrcPixelTop^;
      AvgBrightness := AvgBrightness + SrcPixelLeft^;
      AvgBrightness := AvgBrightness + CenterBrightness;
      AvgBrightness := AvgBrightness + SrcPixelRight^;
      AvgBrightness := AvgBrightness + SrcPixelBottom^;
      AvgBrightness := (IntBeta * AvgBrightness + $7FFF) shr 16;

      // add center pixel weighted by alpha
      AvgBrightness := (IntAlpha * CenterBrightness + $7FFF) shr 16 - AvgBrightness;

      // ensure range
      if AvgBrightness < 0 then
        AvgBrightness := 0
      else if AvgBrightness > 255 then
        AvgBrightness := 255;

      DstPixel^ := AvgBrightness;

      Inc(DstPixel);

      Inc(SrcPixelTop);
      Inc(SrcPixelLeft);
      Inc(SrcPixelCenter);
      Inc(SrcPixelRight);
      Inc(SrcPixelBottom);
    end;

    // copy Last column unchanged
    DstPixel^ := SrcPixelCenter^;
  end;
  Dec(SrcRow, BytesPerLine);

  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);

  // copy last row unchanged
  SrcPixelCenter := PPixel(SrcRow);

  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;
end;

procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; _Alpha: Single);
type
  PPixel = PRGBTriple;
const
  BytesPerPixel = 3;
var
  Row, Column, k: Integer;
  SrcRows: array[0..2] of PByte;
  SrcPixels: array[0..4] of PPixel;
  FirstSrcRow: PByte;
  SrcPixel: PPixel;
  DstRow: PByte;
  DstPixel: PPixel;
  p: PPixel;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  AvgRed, AvgGreen, AvgBlue: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((_Alpha >= 0) and (_Alpha <= 5), 'Alpha must be >=0 and <=5');

  // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
  Beta := _Alpha / 5;
  _Alpha := _Alpha + 1;

  // integer scaled alpha and beta calculated only once
  IntBeta := Round(Beta * $10000);
  IntAlpha := Round(_Alpha * $10000);

  _SrcBmp.PixelFormat := pf24bit;
  _DstBmp.PixelFormat := pf24bit;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  // There is Graphics.BytesPerScanline() which we could call instead of doing the
  // calculation here
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  DstRow := _DstBmp.ScanLine[0];
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  FirstSrcRow := _SrcBmp.ScanLine[0];
  SrcPixel := PPixel(FirstSrcRow);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;

  // ScanLine[0] is the line with the highest memory address, so we decrement it by
  // BytesPerLine to get the next line (which would be ScanLine[1])
  // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
  // of the code due the the function call and some overhead in that code.
  SrcRows[0] := FirstSrcRow;
  SrcRows[1] := PByte(Integer(FirstSrcRow) - BytesPerLine);
  SrcRows[2] := PByte(Integer(SrcRows[1]) - BytesPerLine);
  for Row := 1 to WorkAreaHeight do begin
    Dec(DstRow, BytesPerLine);
    DstPixel := PPixel(DstRow);
    SrcPixels[0] := PPixel(Integer(SrcRows[0]) + 1 * BytesPerPixel); //top
    SrcPixels[1] := PPixel(SrcRows[1]); //left
    SrcPixels[2] := PPixel(Integer(SrcRows[1]) + 1 * BytesPerPixel); //center
    SrcPixels[3] := PPixel(Integer(SrcRows[1]) + 2 * BytesPerPixel); //right
    SrcPixels[4] := PPixel(Integer(SrcRows[2]) + 1 * BytesPerPixel); //bottom
    DstPixel^ := SrcPixels[1]^; //1st col unchanged
    for Column := 1 to WorkAreaWidth do begin
      // calculate average weighted by -beta for each color
      AvgRed := 0;
      AvgGreen := 0;
      AvgBlue := 0;
      for k := 0 to 4 do begin
        AvgRed := AvgRed + SrcPixels[k]^.rgbtRed;
        AvgGreen := AvgGreen + SrcPixels[k]^.rgbtGreen;
        AvgBlue := AvgBlue + SrcPixels[k]^.rgbtBlue;
        Inc(SrcPixels[k]);
      end;
      AvgRed := (IntBeta * AvgRed + $7FFF) shr 16;
      AvgGreen := (IntBeta * AvgGreen + $7FFF) shr 16;
      AvgBlue := (IntBeta * AvgBlue + $7FFF) shr 16;

      // add center pixel weighted by alpha
      p := PPixel(SrcPixels[1]); // after inc, st[1] is at center
      AvgRed := (IntAlpha * p^.rgbtRed + $7FFF) shr 16 - AvgRed;
      AvgGreen := (IntAlpha * p^.rgbtGreen + $7FFF) shr 16 - AvgGreen;
      AvgBlue := (IntAlpha * p^.rgbtBlue + $7FFF) shr 16 - AvgBlue;

      // ensure range (this looks stupid, but avoids function calls)
      if AvgRed < 0 then
        AvgRed := 0
      else if AvgRed > 255 then
        AvgRed := 255;
      if AvgGreen < 0 then
        AvgGreen := 0
      else if AvgGreen > 255 then
        AvgGreen := 255;
      if AvgBlue < 0 then
        AvgBlue := 0
      else if AvgBlue > 255 then
        AvgBlue := 255;

      Inc(DstPixel);
      DstPixel^.rgbtRed := AvgRed;
      DstPixel^.rgbtGreen := AvgGreen;
      DstPixel^.rgbtBlue := AvgBlue;
    end;
    Inc(DstPixel);
    Inc(SrcPixels[1]);

    // copy Last column unchanged
    DstPixel^ := SrcPixels[1]^;

    // prepare for next loop
    SrcRows[0] := SrcRows[1];
    SrcRows[1] := SrcRows[2];
    Dec(SrcRows[2], BytesPerLine);
  end;
  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);

  // copy last row unchanged
  SrcPixel := PPixel(SrcRows[1]);

  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;
end;

procedure TBitmap_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix);
begin
  case _SrcBmp.PixelFormat of
    pf8bit: TBitmap8_Sharpen(_SrcBmp, _DstBmp, _AlphaMap);
  else
    TBitmap24_Sharpen(_SrcBmp, _DstBmp, _AlphaMap);
  end;
end;

procedure TBitmap8_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix);
type
  PPixel = PByte;
const
  BytesPerPixel = 1;
  ForcedPixelFormat = pf8bit;
var
  Row, Column: Integer;
  SrcPixelTop: PPixel;
  SrcPixelLeft: PPixel;
  SrcPixelCenter: PPixel;
  SrcPixelRight: PPixel;
  SrcPixelBottom: PPixel;
  SrcRow: PByte;
  DstRow: PByte;
  DstPixel: PPixel;
  Alpha: Single;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  CenterBrightness: Integer;
  AvgBrightness: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((Length(_AlphaMap) = _SrcBmp.Height),
    Format('Number of lines in AlphaMap (%d) must match bitmap height (%d)', [Length(_AlphaMap), _SrcBmp.Height]));

  _SrcBmp.PixelFormat := pf8bit;
  _DstBmp.PixelFormat := pf8bit;
  _DstBmp.Palette := MakeGrayPalette;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
//  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  SrcRow := _SrcBmp.ScanLine[0];
  DstRow := _DstBmp.ScanLine[0];
  SrcPixelCenter := PPixel(SrcRow);
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;

  for Row := 1 to WorkAreaHeight do begin
    Assert(Length(_AlphaMap[Row]) = _SrcBmp.Width,
      Format('Number of values in AlphaMap[%d] (%d) must match bitmap width (%d)',
      [Row, Length(_AlphaMap[Row]), _SrcBmp.Width]));

    // ScanLine[0] is the line with the highest memory address, so we decrement it by
    // BytesPerLine to get the next line (which would be ScanLine[1])
    // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
    // of the code due the the function call and some overhead in that code.
    Dec(SrcRow, BytesPerLine);
    Dec(DstRow, BytesPerLine);
    SrcPixelCenter := PPixel(SrcRow);
    DstPixel := PPixel(DstRow);

    // 1st column unchanged
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);

    // remember: Bitmaps are stored upside down, so for the previous line, we must add BytesPerLine
    //           and for the next line we must subtract BytesPerLine
    SrcPixelLeft := SrcPixelCenter;
    Inc(SrcPixelCenter);
    SrcPixelRight := SrcPixelCenter;
    Inc(SrcPixelRight);
    SrcPixelTop := PPixel(Integer(SrcPixelCenter) + BytesPerLine);
    SrcPixelBottom := PPixel(Integer(SrcPixelCenter) - BytesPerLine);

    for Column := 1 to WorkAreaWidth do begin
      CenterBrightness := SrcPixelCenter^;

      Alpha := _AlphaMap[Row][Column];
      Assert((Alpha >= 0) and (Alpha <= 5), Format('Alpha[%d][%d] must be >=1 and <=5', [Row, Column]));
      // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
      Beta := Alpha / 5;
      // the original algorithm used 1 < Alpha < 6 so we need to add 1 here
      Alpha := Alpha + 1;
      // integer scaled alpha and beta calculated only once
      IntBeta := Round(Beta * $10000);
      IntAlpha := Round(Alpha * $10000);

      // calculate the average brightness weighted by -beta
      AvgBrightness := SrcPixelTop^;
      AvgBrightness := AvgBrightness + SrcPixelLeft^;
      AvgBrightness := AvgBrightness + CenterBrightness;
      AvgBrightness := AvgBrightness + SrcPixelRight^;
      AvgBrightness := AvgBrightness + SrcPixelBottom^;
      AvgBrightness := (IntBeta * AvgBrightness + $7FFF) shr 16;

      // add center pixel weighted by alpha
      AvgBrightness := (IntAlpha * CenterBrightness + $7FFF) shr 16 - AvgBrightness;

      // ensure range
      if AvgBrightness < 0 then
        AvgBrightness := 0
      else if AvgBrightness > 255 then
        AvgBrightness := 255;

      // write into the target pixel
      DstPixel^ := AvgBrightness;

      Inc(DstPixel);

      Inc(SrcPixelTop);
      Inc(SrcPixelLeft);
      Inc(SrcPixelCenter);
      Inc(SrcPixelRight);
      Inc(SrcPixelBottom);
    end;

    // copy Last column unchanged
    DstPixel^ := SrcPixelCenter^;
  end;
  Dec(SrcRow, BytesPerLine);

  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);

  // copy last row unchanged
  SrcPixelCenter := PPixel(SrcRow);

  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixelCenter^;
    Inc(DstPixel);
    Inc(SrcPixelCenter);
  end;
end;

procedure TBitmap24_Sharpen(_SrcBmp, _DstBmp: TBitmap; const _AlphaMap: TSingleMatrix);
type
  PPixel = PRGBTriple;
const
  BytesPerPixel = 3;
var
  Row, Column, k: Integer;
  SrcRows: array[0..2] of PByte;
  SrcPixels: array[0..4] of PPixel;
  FirstSrcRow: PByte;
  SrcPixel: PPixel;
  DstRow: PByte;
  DstPixel: PPixel;
  p: PPixel;
  Alpha: Single;
  Beta: Single;
  IntAlpha, IntBeta: Integer;
  WorkAreaHeight, WorkAreaWidth: Integer;
  AvgRed, AvgGreen, AvgBlue: Integer;
  BytesPerLine: Integer;
begin
  // sharpening is blending of the current pixel
  // with the average of the surrounding ones,
  // but with a negative weight for the average
  Assert((_SrcBmp.Width > 2) and (_SrcBmp.Height > 2), 'Bitmap must be at least 3x3');
  Assert((Length(_AlphaMap) = _SrcBmp.Height),
    Format('Number of lines in AlphaMap (%d) must match bitmap height (%d)', [Length(_AlphaMap), _SrcBmp.Height]));

  _SrcBmp.PixelFormat := pf24bit;
  _DstBmp.PixelFormat := pf24bit;
  TBitmap_SetSize(_DstBmp, _SrcBmp.Width, _SrcBmp.Height);

  WorkAreaWidth := _SrcBmp.Width - 2;
  WorkAreaHeight := _SrcBmp.Height - 2;
  // There is Graphics.BytesPerScanline() which we could call instead of doing the
  // calculation here
  BytesPerLine := (((WorkAreaWidth + 2) * 8 * BytesPerPixel + 31) and not 31) div 8;
  Assert(BytesPerLine = Graphics.BytesPerScanline(WorkAreaWidth + 2, BytesPerPixel * 8, 32));

  DstRow := _DstBmp.ScanLine[0];
  DstPixel := PPixel(DstRow);

  // Copy first row unchanged
  FirstSrcRow := _SrcBmp.ScanLine[0];
  SrcPixel := PPixel(FirstSrcRow);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;

  // ScanLine[0] is the line with the highest memory address, so we decrement it by
  // BytesPerLine to get the next line (which would be ScanLine[1])
  // We could of course call _SrcBmp.Scanline[], but that would affect the efficiency
  // of the code due the the function call and some overhead in that code.
  SrcRows[0] := FirstSrcRow;
  SrcRows[1] := PByte(Integer(FirstSrcRow) - BytesPerLine);
  SrcRows[2] := PByte(Integer(SrcRows[1]) - BytesPerLine);
  for Row := 1 to WorkAreaHeight do begin
    Assert(Length(_AlphaMap[Row]) = _SrcBmp.Width,
      Format('Number of values in AlphaMap[%d] (%d) must match bitmap width (%d)',
      [Row, Length(_AlphaMap[Row]), _SrcBmp.Width]));

    Dec(DstRow, BytesPerLine);
    DstPixel := PPixel(DstRow);
    SrcPixels[0] := PPixel(Integer(SrcRows[0]) + 1 * BytesPerPixel); //top
    SrcPixels[1] := PPixel(SrcRows[1]); //left
    SrcPixels[2] := PPixel(Integer(SrcRows[1]) + 1 * BytesPerPixel); //center
    SrcPixels[3] := PPixel(Integer(SrcRows[1]) + 2 * BytesPerPixel); //right
    SrcPixels[4] := PPixel(Integer(SrcRows[2]) + 1 * BytesPerPixel); //bottom
    // copy 1st col unchanged ("[1]" is not the pixel index!)
    DstPixel^ := SrcPixels[1]^;
    for Column := 1 to WorkAreaWidth do begin
      Alpha := _AlphaMap[Row][Column];
      Assert((Alpha >= 0) and (Alpha <= 5), Format('Alpha[%d][%d] must be >=1 and <=5', [Row, Column]));
      // since 0 <= Alpha <= 5 we can be sure that 0 <= Beta <= 1
      Beta := Alpha / 5;
      // the original algorithm used 1 < Alpha < 6 so we need to add 1 here
      Alpha := Alpha + 1;
      // integer scaled alpha and beta calculated only once
      IntBeta := Round(Beta * $10000);
      IntAlpha := Round(Alpha * $10000);

      // calculate average weighted by -beta for each color
      AvgRed := 0;
      AvgGreen := 0;
      AvgBlue := 0;
      for k := 0 to 4 do begin
        AvgRed := AvgRed + SrcPixels[k]^.rgbtRed;
        AvgGreen := AvgGreen + SrcPixels[k]^.rgbtGreen;
        AvgBlue := AvgBlue + SrcPixels[k]^.rgbtBlue;
        Inc(SrcPixels[k]);
      end;

      AvgRed := (IntBeta * AvgRed + $7FFF) shr 16;
      AvgGreen := (IntBeta * AvgGreen + $7FFF) shr 16;
      AvgBlue := (IntBeta * AvgBlue + $7FFF) shr 16;

      // add center pixel weighted by alpha
      p := PPixel(SrcPixels[1]); // after inc, SrcPixels[1] is at center
      AvgRed := (IntAlpha * p^.rgbtRed + $7FFF) shr 16 - AvgRed;
      AvgGreen := (IntAlpha * p^.rgbtGreen + $7FFF) shr 16 - AvgGreen;
      AvgBlue := (IntAlpha * p^.rgbtBlue + $7FFF) shr 16 - AvgBlue;

      // ensure range (this looks stupid, but avoids function calls)
      if AvgRed < 0 then
        AvgRed := 0
      else if AvgRed > 255 then
        AvgRed := 255;
      if AvgGreen < 0 then
        AvgGreen := 0
      else if AvgGreen > 255 then
        AvgGreen := 255;
      if AvgBlue < 0 then
        AvgBlue := 0
      else if AvgBlue > 255 then
        AvgBlue := 255;

      // write into the target pixel
      Inc(DstPixel);
      DstPixel^.rgbtRed := AvgRed;
      DstPixel^.rgbtGreen := AvgGreen;
      DstPixel^.rgbtBlue := AvgBlue;
    end;
    Inc(DstPixel);
    Inc(SrcPixels[1]);

    // copy Last column unchanged
    DstPixel^ := SrcPixels[1]^;

    // prepare for next loop
    SrcRows[0] := SrcRows[1];
    SrcRows[1] := SrcRows[2];
    Dec(SrcRows[2], BytesPerLine);
  end;

  // copy last row unchanged
  Dec(DstRow, BytesPerLine);
  DstPixel := PPixel(DstRow);
  SrcPixel := PPixel(SrcRows[1]);
  for Column := 0 to WorkAreaWidth + 1 do begin
    DstPixel^ := SrcPixel^;
    Inc(DstPixel);
    Inc(SrcPixel);
  end;
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Size: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestPos,
    _Size,
    _Src,
    _SrcPos,
    _Rop);
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestRect: TRect; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestRect,
    _Src,
    _SrcPos,
    _Rop);
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap; _SrcPos: TPoint;
  _Rop: DWORD = SRCCOPY): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestPos,
    _Src,
    _SrcPos,
    _Rop);
end;

function TBitmap_BitBlt(_DestBmp: TBitmap; _DestPos: TPoint; _Src: TBitmap; _Rop: DWORD): LongBool;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := TCanvas_BitBlt(
    _DestBmp.Canvas,
    _DestPos,
    _Src,
    Point(0, 0),
    _Rop);
end;

function ColorBrightness(_Red, _Green, _Blue: Byte): Byte;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := GetFastLuminance(_Red, _Green, _Blue);
end;

function ColorBrightness(_Color: TColor): Byte;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
var
  RGB: TdzRgbTriple;
begin
  TdzRgbTriple_SetColor(RGB, _Color);
  Result := TdzRgbTriple_GetFastLuminance(RGB);
end;

function BestForegroundForColor(_Red, _Green, _Blue: Byte): TColor;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  if ColorBrightness(_Red, _Green, _Blue) < 123 then
    Result := clWhite
  else
    Result := clBlack;
end;

function BestForegroundForColor(_Color: TColor): TColor;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  if ColorBrightness(_Color) < 123 then
    Result := clWhite
  else
    Result := clBlack;
end;

function TryStr2Color(const _s: string; out _Color: TColor): Boolean;
var
  c: Integer;
begin
  Result := IdentToColor(_s, c);
  if not Result then
    Result := TryStr2Int(_s, c);
  if Result then
    _Color := c;
end;

{ TPixelFilterCutoff }

constructor TPixelFilterCutoff.Create(_CutOff: Byte);
begin
  inherited Create;
  FCutOff := _CutOff;
end;

procedure TPixelFilterCutoff.FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple);
begin
  if _Pixel.Blue > FCutOff then
    _Pixel.Blue := FCutOff;
  if _Pixel.Red > FCutOff then
    _Pixel.Red := FCutOff;
  if _Pixel.Green > FCutOff then
    _Pixel.Green := FCutOff;
end;

procedure TPixelFilterCutoff.FilterCallback(_x, _y: Integer; var _Pixel: Byte);
begin
  if _Pixel > FCutOff then
    _Pixel := FCutOff;
end;

{ TPixelFilterStretch }

constructor TPixelFilterStretch.Create(_LowCutOff, _HighCutOff: Byte);
begin
  inherited Create;
  FLowCutOff := _LowCutOff;
  FHighCutOff := _HighCutOff;
  FFactor := 256 / (_HighCutOff - _LowCutOff);
end;

procedure TPixelFilterStretch.StretchColor(var _Color: Byte);
var
  Value: Integer;
begin
  Value := _Color;
  if (Value > FLowCutOff) and (Value < FHighCutOff) then begin
    Value := Round((Value - FLowCutOff) * FFactor);
    if Value > 255 then
      Value := 255;
    _Color := Value;
  end;
end;

procedure TPixelFilterStretch.FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple);
begin
  StretchColor(_Pixel.Blue);
  StretchColor(_Pixel.Red);
  StretchColor(_Pixel.Green);
end;

procedure TPixelFilterStretch.FilterCallback(_x, _y: Integer; var _Pixel: Byte);
begin
  StretchColor(_Pixel);
end;

{ TPixelFilterMove }

constructor TPixelFilterMove.Create(_MoveBy: Integer);
begin
  inherited Create;
  if _MoveBy > 200 then
    raise Exception.CreateFmt(_('MoveBy parameter (%d) must not be > 200'), [_MoveBy]);
  if _MoveBy < -200 then
    raise Exception.CreateFmt(_('MoveBy parameter (%d) must not be < -200'), [_MoveBy]);
  FMoveBy := _MoveBy;
end;

procedure TPixelFilterMove.MoveColor(var _Color: Byte);
var
  Value: Integer;
begin
  Value := _Color + FMoveBy;
  if Value < 0 then
    _Color := 0
  else if Value > 255 then
    _Color := 255
  else
    _Color := Value;
end;

procedure TPixelFilterMove.FilterCallback(_x, _y: Integer; var _Pixel: TdzRgbTriple);
begin
  MoveColor(_Pixel.Blue);
  MoveColor(_Pixel.Red);
  MoveColor(_Pixel.Green);
end;

procedure TPixelFilterMove.FilterCallback(_x, _y: Integer; var _Pixel: Byte);
begin
  MoveColor(_Pixel);
end;

function RainbowColor(_Hue: Double): TColor; overload;
// taken from https://stackoverflow.com/a/19719171/49925
begin
  _Hue := EnsureRange(_Hue, 0, 1) * 6;
  case Trunc(_Hue) of
    0: Result := RGB(255, Round(Frac(_Hue) * 255), 0);
    1: Result := RGB(255 - Round(Frac(_Hue) * 255), 255, 0);
    2: Result := RGB(0, 255, Round(Frac(_Hue) * 255));
    3: Result := RGB(0, 255 - Round(Frac(_Hue) * 255), 255);
    4: Result := RGB(Round(Frac(_Hue) * 255), 0, 255);
  else
    Result := RGB(255, 0, 255 - Round(Frac(_Hue) * 255));
  end;
end;

function RainbowColor(_MinHue, _MaxHue, _Hue: Integer): TColor; overload;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
// taken from https://stackoverflow.com/a/19719171/49925
begin
  Result := RainbowColor((_Hue - _MinHue) / (_MaxHue - _MinHue + 1));
end;

end.
