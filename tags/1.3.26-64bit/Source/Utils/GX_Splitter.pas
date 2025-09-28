// A Splitter Control with better visibility than the default VCL TSplitter
unit GX_Splitter;

{$I GX_CondDefine.inc}

interface

{$IFDEF GX_SUPPORTS_THEMING}
uses
  Types,
  Classes,
  Graphics,
  Controls,
  ExtCtrls;

const
  DEFAULT_DOTCOUNT    = 24;
  DEFAULT_DARK_COLOR  = clSilver; // cl3DDkShadow;
  DEFAULT_LIGHT_COLOR = clDkGray; // clWindow;

type
  TGXSplitter = class(TSplitter)
  private
    FDotCount     : Integer;
    FDotHighColor : TColor;
    FDotLowColor  : TColor;
  protected
    procedure DrawDots(R: TRect; dx, dy: Integer);
    procedure Paint; override;
  public
    property DotCount: Integer read FDotCount write FDotCount default DEFAULT_DOTCOUNT;
    property DotHighColor: TColor read FDotHighColor write FDotHighColor default DEFAULT_LIGHT_COLOR;
    property DotLowColor : TColor read FDotLowColor write FDotLowColor default DEFAULT_DARK_COLOR;
    constructor Create(AOwner: TComponent); override;
  end;
  TSplitter = class(TGXSplitter);

Implementation

{ TGXSplitter }

constructor TGXSplitter.Create(AOwner: TComponent);
begin
  inherited;
  Align         := alLeft;
  Width         := 5;
  Beveled       := False;
  FDotCount     := DEFAULT_DOTCOUNT;
  FDotLowColor  := DEFAULT_DARK_COLOR;
  FDotHighColor := DEFAULT_LIGHT_COLOR;
  StyleElements := [];
end;

procedure TGXSplitter.DrawDots(R: TRect; dx, dy: Integer);
var
  i : Integer;
begin
  R.Right := R.Left + 2;
  R.Bottom := R.Top + 2;
  for i := 0 to FDotCount-1 do // draw the dots
  begin
    Canvas.Brush.Color := FDotLowColor;
    Canvas.FillRect(R);

    OffSetRect(R, 1, 1);
    Canvas.Brush.Color := FDotHighColor;
    Canvas.FillRect(R);

    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(Rect(R.Left, R.Top, R.Left+1, R.Top+1));

    OffsetRect(R, dx, dy);
  end; // for i
end;

type
  TWinControlAccess = class(TWinControl);

procedure TGXSplitter.Paint;
var
  R      : TRect;
begin
  // no inherited; we draw own stuff!
  R := ClientRect;

  if ParentColor then
    Canvas.Brush.Color := TWinControlAccess(Parent).Color
  else
    Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(R);

  if Enabled then
  begin
    if (Align in [alLeft, alRight]) then
    begin
      // vertical Splitter
      Inc(R.Top, (R.Height div 2) - (2 * FDotCount));
      Inc(R.Left, (R.Width div 2) - 1);
      DrawDots(R, -1, 3);
    end
    else begin
      // horizontal Splitter
      Inc(R.Left, (R.Width) div 2 - (2 * FDotCount));
      Inc(R.Top, (R.Height) div 2 - 1);
      DrawDots(R, 3, -1);
    end; // if "horizontal"
  end; // if Enabled
end;

{$ELSE}
Implementation
{$ENDIF}

end.

