unit u_dzDpiScaleUtils;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  Types;

type
  TDpiScaler = record
  private
    FDesignDpi: Integer;
    FCurrentDpi: Integer;
  public
    procedure Init(_frm: TCustomForm); overload; inline;
    procedure Init(_Dpi: Integer); overload; inline;
    procedure Init(_DesignDpi, _CurrentDpi: Integer); overload; inline;
    procedure SetCurrentDpi(_frm: TCustomForm); overload; inline;
    procedure SetCurrentDpi(_Dpi: Integer); overload; inline;
    function Calc(_Value: Integer): Integer; overload; inline;
    function Calc(const _Value: TRect): TRect; overload; inline;
  end;

type
  TCtrlDpiScaler = record
    Ctrl: TControl;
    BoundsRect: TRect;
    FontSize: Integer;
    procedure Assign(_ctrl: TControl);
    procedure ApplyScale(const _Scaler: TDpiScaler);
    procedure ResizeFont(const _Scaler: TDpiScaler);
  end;

  TFormDpiScaler = class
  private
    DesignDPI: Integer;
    Width, Height: Integer;
    MinWidth, MinHeight: Integer;
    MaxWidth, MaxHeight: Integer;
    FontSize: Integer;
    FFrm: TForm;
    CtrlParams: array of TCtrlDpiScaler;
    procedure AddControls(_ctrl: TWinControl);
  public
    constructor Create(_frm: TForm);
    procedure ApplyScale(const _Scaler: TDpiScaler);
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
  end;

implementation

uses
  StdCtrls,
  u_dzAdvancedObject,
  u_dzVclUtils;

{ TDpiScaler }

function TDpiScaler.Calc(_Value: Integer): Integer;
begin
  Result := MulDiv(_Value, FCurrentDpi, FDesignDpi);
end;

function TDpiScaler.Calc(const _Value: TRect): TRect;
begin
  Result.Left := Calc(_Value.Left);
  Result.Top := Calc(_Value.Top);
  Result.Right := Calc(_Value.Right);
  Result.Bottom := Calc(_Value.Bottom);
end;

procedure TDpiScaler.Init(_Dpi: Integer);
begin
  FDesignDpi := _Dpi;
  FCurrentDpi := _Dpi;
end;

procedure TDpiScaler.Init(_DesignDpi, _CurrentDpi: Integer);
begin
  FDesignDpi := _DesignDpi;
  FCurrentDpi := _CurrentDpi;
end;

procedure TDpiScaler.Init(_frm: TCustomForm);
begin
  if not Assigned(_frm) then begin
    FDesignDpi := 96;
    FCurrentDpi := 96;
  end else begin
// todo: adjust as needed
{$IFDEF DELPHIX_TOKYO_UP}
    FDesignDpi := TForm_GetDesignDPI(TForm(_frm));
    FCurrentDpi := TScreen_GetDpiForForm(_frm);
{$ELSE ~DELPHIX_TOKYO_UP}
    FDesignDpi := TForm(_frm).PixelsPerInch;
    FCurrentDpi := TForm(_frm).PixelsPerInch;
{$ENDIF DELPHIX_TOKYO_UP}
  end;
end;

procedure TDpiScaler.SetCurrentDpi(_Dpi: Integer);
begin
  FCurrentDpi := _Dpi;
end;

procedure TDpiScaler.SetCurrentDpi(_frm: TCustomForm);
begin
  if not Assigned(_frm) then begin
    FCurrentDpi := 96;
  end else begin
// todo: adjust as needed
{$IFDEF DELPHIX_TOKYO_UP}
    FCurrentDpi := TScreen_GetDpiForForm(_frm)
{$ELSE ~DELPHIX_TOKYO_UP}
    FCurrentDpi := TForm(_frm).PixelsPerInch;
{$ENDIF DELPHIX_TOKYO_UP}
  end;
end;

{ TCtrlDpiScaler }

function GetFontSize(_fnt: TFont): Integer;
begin
//  Result := _fnt.Size;
  Result := _fnt.Height;
end;

procedure SetFontSize(_fnt: TFont; _Size: Integer);
begin
//  _fnt.Size := _Size;
  _fnt.Height := _Size;
end;

procedure TCtrlDpiScaler.ApplyScale(const _Scaler: TDpiScaler);
var
  br: TRect;
begin
  br := _Scaler.Calc(BoundsRect);
  if (Ctrl is TLabel) and TLabel(Ctrl).AutoSize then begin
    br.Width := Ctrl.Width;
    br.Height := Ctrl.Height;
  end else if (Ctrl is TEdit) and TEdit(Ctrl).AutoSize then begin
    br.Height := Ctrl.Height;
  end;
  Ctrl.BoundsRect := br;
  ResizeFont(_Scaler);
end;

procedure TCtrlDpiScaler.Assign(_ctrl: TControl);
var
  fnt: TFont;
begin
  Ctrl := _ctrl;
  BoundsRect := Ctrl.BoundsRect;
  if not TAdvancedObject.TryGetObjectProperty(_ctrl, 'Font', TObject(fnt)) then begin
    FontSize := 0;
  end else begin
    FontSize := GetFontSize(fnt);
  end;
end;

procedure TCtrlDpiScaler.ResizeFont(const _Scaler: TDpiScaler);
var
  fnt: TFont;
  ParentFontValue: Boolean;
begin
  if TAdvancedObject.TryGetObjectProperty(Ctrl, 'Font', TObject(fnt)) then begin
    if not TAdvancedObject.TryGetBoolProperty(Ctrl, 'ParentFont', ParentFontValue)
      or not ParentFontValue then begin
      Assert(FontSize <> 0);
      SetFontSize(fnt, _Scaler.Calc(FontSize));
    end;
  end;
end;

{ TFormDpiScaler }

procedure TFormDpiScaler.AddControls(_ctrl: TWinControl);
var
  Offset: Integer;
  i: Integer;
  cnt: Integer;
  Ctrl: TControl;
begin
  cnt := _ctrl.ControlCount;
  Offset := Length(CtrlParams);
  SetLength(CtrlParams, Offset + cnt);
  for i := 0 to cnt - 1 do begin
    Ctrl := _ctrl.Controls[i];
    CtrlParams[Offset + i].Assign(Ctrl);
    if Ctrl is TWinControl then
      AddControls(TWinControl(Ctrl));
  end;
end;

procedure TFormDpiScaler.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  Scaler: TDpiScaler;
  br: TRect;
  RedrawLock: IInterface;
begin
  if not Assigned(FFrm) then
    Exit; //==>

  RedrawLock := TWinControl_Lock(FFrm);
  try
    Scaler.Init(DesignDPI, _NewDpi);
    // Disable constraints to assure the new size can be set
    FFrm.Constraints.MinWidth := 0;
    FFrm.Constraints.MinHeight := 0;
    FFrm.Constraints.MaxWidth := 0;
    FFrm.Constraints.MaxHeight := 0;
    if Assigned(_NewBounds) then begin
      FFrm.BoundsRect := _NewBounds^;
    end else begin
      br := FFrm.BoundsRect;
      br.Width := Scaler.Calc(br.Width);
      br.Height := Scaler.Calc(br.Height);
      FFrm.BoundsRect := br;
    end;
    ApplyScale(Scaler);
  finally
    RedrawLock := nil;
  end;
end;

procedure TFormDpiScaler.ApplyScale(const _Scaler: TDpiScaler);
var
  cnt: Integer;
  i: Integer;
begin
  SetFontSize(FFrm.Font, _Scaler.Calc(FontSize));
  cnt := Length(CtrlParams);
  for i := 0 to cnt - 1 do begin
    CtrlParams[i].ApplyScale(_Scaler);
  end;
  FFrm.Constraints.MinWidth := _Scaler.Calc(MinWidth);
  FFrm.Constraints.MinHeight := _Scaler.Calc(MinHeight);
  FFrm.Constraints.MaxWidth := _Scaler.Calc(MaxWidth);
  FFrm.Constraints.MaxHeight := _Scaler.Calc(MaxHeight);
end;

constructor TFormDpiScaler.Create(_frm: TForm);
var
  Scaler: TDpiScaler;
  cnstr: TSizeConstraints;
begin
  inherited Create;
  FFrm := _frm;
  Width := _frm.ClientWidth;
  Height := _frm.ClientHeight;
  cnstr := _frm.Constraints;
  MinWidth := cnstr.MinWidth;
  MinHeight := cnstr.MinHeight;
  MaxWidth := cnstr.MaxWidth;
  MaxHeight := cnstr.MaxHeight;
  FontSize := GetFontSize(_frm.Font);
  DesignDPI := TForm_GetDesignDPI(_frm);
  Scaler.Init(_frm.Font.PixelsPerInch, DesignDPI);
  FontSize := Scaler.Calc(FontSize);
  AddControls(FFrm);
end;

end.



