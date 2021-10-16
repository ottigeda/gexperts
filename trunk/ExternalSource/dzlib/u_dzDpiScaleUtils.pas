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
    function ScaleFactorPercent: Integer;
  end;

type
  TCtrlDpiScaler = record
    Ctrl: TControl;
    BoundsRect: TRect;
    FontSize: Integer;
    procedure Assign(_Ctrl: TControl);
    procedure ApplyScale(const _Scaler: TDpiScaler);
    procedure ResizeFont(const _Scaler: TDpiScaler);
  end;

  TFormDpiScaler = class
  private
    DesignDPI: Integer;
    ClientWidth, ClientHeight: Integer;
    MinWidth, MinHeight: Integer;
    MaxWidth, MaxHeight: Integer;
    FontSize: Integer;
    FFrm: TForm;
    CtrlParams: array of TCtrlDpiScaler;
    procedure AddControls(_Ctrl: TWinControl);
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

{$IFOPT D+}
var
  LogFile: Textfile;
{$ENDIF}

procedure LogStr(const _s: string);
begin
{$IFOPT D+}
  WriteLn(LogFile, _s);
  Flush(LogFile);
{$ENDIF}
end;

procedure LogFmt(const _s: string; _Params: array of const);
begin
  LogStr(Format(_s, _Params));
end;

procedure LogRect(const _Prefix: string; const _Rect: TRect);
begin
  LogFmt('%s: (Left: %d, Top: %d, Right: %d, Bottom: %d, (Width: %d, Height: %d))',
    [_Prefix, _Rect.Left, _Rect.Top, _Rect.Right, _Rect.Bottom, _Rect.Width, _Rect.Height]);
end;

procedure LogConstraints(const _Prefix: string; _Cstr: TSizeConstraints);
begin
  LogFmt('%s: (MinWidth, %d, MinHeight: %d, MaxWidth: %d, MaxHeight: %d)',
    [_Prefix, _Cstr.MinWidth, _Cstr.MinHeight, _Cstr.MaxWidth, _Cstr.MaxHeight]);
end;

procedure LogForm(const _Prefix: string; _frm: TForm);
begin
  LogFmt('%s: ClientWidth: %d, ClientHeight: %d',
    [_Prefix, _frm.ClientWidth, _frm.ClientHeight]);
end;

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

function TDpiScaler.ScaleFactorPercent: Integer;
begin
  Result := MulDiv(100, FCurrentDpi, FDesignDpi);
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

procedure TCtrlDpiScaler.ApplyScale(const _Scaler: TDpiScaler);
var
  br: TRect;
  LabelWasAutoSize: Boolean;
begin
  LogFmt('TCtrlDpiScaler.ApplyScale(%s, %d %%)', [Ctrl.Name, _Scaler.ScaleFactorPercent]);

  ResizeFont(_Scaler);

  br := _Scaler.Calc(BoundsRect);
  LogRect('  br', br);

  LabelWasAutoSize := False;
  if (Ctrl is TLabel) then begin
    LogStr('  Ctrl is TLabel');
    LabelWasAutoSize := TLabel(Ctrl).AutoSize;
    if LabelWasAutoSize then begin
      LogStr('  AutoSize: True');
      TLabel(Ctrl).AutoSize := False;
    end;
    case TLabel(Ctrl).Alignment of
      taRightJustify: begin
          LogStr('  Alignment: taRightJustify');
          br.Left := _Scaler.Calc(BoundsRect.Left + BoundsRect.Width) - br.Width;
          LogFmt('  br: (Left: %d, Top: %d, Width: %d, Height: %d)', [br.Left, br.Top, br.Width, br.Height]);
        end;
      taCenter: begin
          LogStr('  Alignment: taCenter');
          br.Left := _Scaler.Calc(BoundsRect.Left + BoundsRect.Width div 2) - br.Width div 2;
          LogRect('  br', br);
        end;
    end;
  end else if (Ctrl is TEdit) and TEdit(Ctrl).AutoSize then begin
    LogStr('  Ctrl is TEdit Autosie: True');
    br.Height := Ctrl.Height;
    LogRect('  br', br);
  end;

  Ctrl.BoundsRect := br;

  // if we don't do this, the text is truncated on the left
  if LabelWasAutoSize then begin
    TLabel(Ctrl).AutoSize := False;
    TLabel(Ctrl).AutoSize := True;
  end;
end;

procedure TCtrlDpiScaler.Assign(_Ctrl: TControl);
var
  fnt: TFont;
begin
  Ctrl := _Ctrl;
  BoundsRect := Ctrl.BoundsRect;
  if not TAdvancedObject.TryGetObjectProperty(_Ctrl, 'Font', TObject(fnt)) then begin
    FontSize := 0;
  end else begin
    FontSize := GetFontSize(fnt);
  end;

  LogFmt('TCtrlDpiScaler.Assign(%s):', [_Ctrl.Name]);
  LogRect('  BoundsRect', BoundsRect);
end;

procedure TCtrlDpiScaler.ResizeFont(const _Scaler: TDpiScaler);
var
  fnt: TFont;
  ParentFontValue: Boolean;
  OldFontSize: Integer;
begin
  if TAdvancedObject.TryGetObjectProperty(Ctrl, 'Font', TObject(fnt)) then begin
    if not TAdvancedObject.TryGetBoolProperty(Ctrl, 'ParentFont', ParentFontValue)
      or not ParentFontValue then begin
      Assert(FontSize <> 0);

      LogFmt('TCtrlDpiScaler.ResizeFont(%d %%)', [_Scaler.ScaleFactorPercent]);
      OldFontSize := GetFontSize(fnt);
      SetFontSize(fnt, _Scaler.Calc(FontSize));

      LogFmt('  OldFontSize: %d, NewFontSize: %d', [OldFontSize, GetFontSize(fnt)]);
    end;
  end;
end;

{ TFormDpiScaler }

procedure TFormDpiScaler.AddControls(_Ctrl: TWinControl);
var
  Offset: Integer;
  i: Integer;
  cnt: Integer;
  Ctrl: TControl;
begin
  cnt := _Ctrl.ControlCount;
  Offset := Length(CtrlParams);
  SetLength(CtrlParams, Offset + cnt);
  for i := 0 to cnt - 1 do begin
    Ctrl := _Ctrl.Controls[i];
    CtrlParams[Offset + i].Assign(Ctrl);
    if Ctrl is TWinControl then
      AddControls(TWinControl(Ctrl));
  end;
end;

procedure TFormDpiScaler.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  Scaler: TDpiScaler;
  RedrawLock: IInterface;
  ClientRect: TRect;
  NewWidth: Integer;
  NewHeight: Integer;
  BoundsRect: TRect;
begin
  if not Assigned(FFrm) then
    Exit; //==>

  if Assigned(_NewBounds) then
    LogFmt('TFormDpiScaler.ApplyDpi(%s, NewDpi: %d, NewBounds: (Left: %d, Top: %d, Width: %d, Height: %d))',
      [FFrm.Name, _NewDpi, _NewBounds.Left, _NewBounds.Top, _NewBounds.Width, _NewBounds.Height])
  else
    LogFmt('TFormDpiScaler.ApplyDpi(%s, NewDpi: %d, NewBounds: (nil))',
      [FFrm.Name, _NewDpi]);

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
      ClientRect := FFrm.ClientRect;
      NewWidth := Scaler.Calc(ClientRect.Width);
      NewHeight := Scaler.Calc(ClientRect.Height);
      FFrm.ClientWidth := NewWidth;
      FFrm.ClientHeight := NewHeight;

      BoundsRect := FFrm.BoundsRect;
      BoundsRect.Offset(-BoundsRect.Width div 2, -BoundsRect.Height div 2);
      FFrm.BoundsRect := BoundsRect;
    end;
    LogForm('  FFrm', FFrm);

    ApplyScale(Scaler);
  finally
    RedrawLock := nil;
  end;
end;

procedure TFormDpiScaler.ApplyScale(const _Scaler: TDpiScaler);
var
  cnt: Integer;
  i: Integer;
  OldFontSize: Integer;
begin
  LogFmt('TFormDpiScaler.ApplyScale(%s, %d %%)', [FFrm.Name, _Scaler.ScaleFactorPercent]);

  OldFontSize := GetFontSize(FFrm.Font);
  SetFontSize(FFrm.Font, _Scaler.Calc(FontSize));
  LogFmt('  OldFontSize: %d, NewFontSize: %d', [OldFontSize, GetFontSize(FFrm.Font)]);

  cnt := Length(CtrlParams);
  for i := 0 to cnt - 1 do begin
    CtrlParams[i].ApplyScale(_Scaler);
  end;
  FFrm.Constraints.MinWidth := _Scaler.Calc(MinWidth);
  FFrm.Constraints.MinHeight := _Scaler.Calc(MinHeight);
  FFrm.Constraints.MaxWidth := _Scaler.Calc(MaxWidth);
  FFrm.Constraints.MaxHeight := _Scaler.Calc(MaxHeight);

  LogConstraints('  FFrm.Constraints', FFrm.Constraints);
end;

constructor TFormDpiScaler.Create(_frm: TForm);
var
  Scaler: TDpiScaler;
  cnstr: TSizeConstraints;
  CurrFontSize: Integer;
begin
  inherited Create;
  FFrm := _frm;
  ClientWidth := _frm.ClientWidth;
  ClientHeight := _frm.ClientHeight;
  cnstr := _frm.Constraints;
  MinWidth := cnstr.MinWidth;
  MinHeight := cnstr.MinHeight;
  MaxWidth := cnstr.MaxWidth;
  MaxHeight := cnstr.MaxHeight;
  CurrFontSize := GetFontSize(_frm.Font);
  DesignDPI := TForm_GetDesignDPI(_frm);

  LogFmt('TFormDpiScaler.Create(%s)', [FFrm.Name]);
  LogFmt('  ClientWidth: %d, ClientHeight: %d, MinWidth: %d, MinHeight: %d, MaxWdidth: %d MaxHeight: %d CurrFontSize: %d DesignDpi: %d',
    [ClientWidth, ClientHeight, MinWidth, MinHeight, MaxWidth, MaxHeight, CurrFontSize, DesignDPI]);

  // FontSize has already been changed by the VCL, but we need the design font size
  Scaler.Init(_frm.Font.PixelsPerInch, DesignDPI);
  FontSize := Scaler.Calc(CurrFontSize);

  LogFmt('  (FontPixelsPerInc: %d, FontSize: %d',
    [_frm.Font.PixelsPerInch, FontSize]);

  AddControls(FFrm);
end;

{$IFOPT D+}
initialization
  Assignfile(LogFile, 'd:\DpiScaling.log');
  Rewrite(LogFile);
finalization
  CloseFile(LogFile);
{$ENDIF}
end.



