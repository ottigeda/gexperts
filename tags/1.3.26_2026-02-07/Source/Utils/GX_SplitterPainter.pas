unit GX_SplitterPainter;

interface

uses
  Classes;

const
  GXPainterSuffix = '_GXPainter';

type
  TSplitterPainter = class(TComponent)
  private
    procedure SplitterPaint(_Sender: TObject);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  ExtCtrls, Types, Graphics, Controls;

{ TSplitterPainter }

constructor TSplitterPainter.Create(_Owner: TComponent);
var
  Splitter: TSplitter absolute _Owner;
begin
  inherited;
  Name := Splitter.Name + GXPainterSuffix;
  Splitter.OnPaint := SplitterPaint;
  Splitter.Invalidate;
end;

destructor TSplitterPainter.Destroy;
var
  Splitter: TSplitter;
begin
  Splitter := Owner as TSplitter;
  if Assigned(Splitter) then begin
    Splitter.OnPaint := nil;
    Splitter.Invalidate;
  end;
  inherited;
end;

procedure TSplitterPainter.SplitterPaint(_Sender: TObject);
var
  Splitter: TSplitter absolute _Sender;
  R: TRect;
  cnv: TCanvas;
  Delta: Integer;
begin
  R := Splitter.ClientRect;
  cnv := Splitter.Canvas;
  cnv.Pen.Color := clBtnShadow;
  cnv.Pen.Style := psDash;
  cnv.Pen.Mode := pmCopy;
  cnv.Pen.Width := 0;
  if Splitter.Align in [alLeft, alRight] then begin
    Delta := (R.Right - R.Left) div 2;
    cnv.MoveTo(R.Left + Delta, R.Top);
    cnv.LineTo(R.Left + Delta, R.Bottom);
  end else begin
    Delta := (R.Bottom - R.Top) div 2;
    cnv.MoveTo(R.Left, R.Top + Delta);
    cnv.LineTo(R.Right, R.Top + Delta);
  end;
//  cnv.Brush.Color := clAqua;
//  cnv.FillRect(R);
end;

end.
