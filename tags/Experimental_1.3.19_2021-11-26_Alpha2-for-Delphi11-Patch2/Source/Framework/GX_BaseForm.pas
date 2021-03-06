unit GX_BaseForm;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
{$IFDEF IDE_IS_HIDPI_AWARE}
  u_dzDpiScaleUtils,
{$ENDIF}
  u_dzVclUtils;

{$IFNDEF IDE_IS_HIDPI_AWARE}
type
  TDummyDpiScaler = class
  public
    function Calc(_Value: Integer): Integer;
  end;
{$ENDIF}

type
  // All forms except docking forms must descend from this class.
  // Changes here must also be made to TfmIdeDockForm, since it must descend
  // from the IDE-internal TDockableForm class.
  TfmBaseForm = class(TForm)
  protected
    procedure Loaded; override;
  protected
{$IFNDEF IDE_IS_HIDPI_AWARE}
    FScaler: TDummyDpiScaler;
{$ELSE}
    FScaler: TFormDpiScaler;
    procedure WMDpiChanged(var _Msg: TWMDpi); message WM_DPICHANGED;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); virtual;
    procedure ArrangeControls; virtual;
{$ENDIF}
  protected
    procedure InitDpiScaler;
  public
    class function Execute(_Owner: TComponent): Boolean; overload; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  GX_GxUtils;

class function TfmBaseForm.Execute(_Owner: TComponent): Boolean;
var
  frm: TfmBaseForm;
begin
  frm := Self.Create(_Owner);
  try
    Result := (frm.ShowModal = mrOk);
  finally
    frm.Free;
  end;
end;

procedure TfmBaseForm.InitDpiScaler;
begin
{$IFNDEF IDE_IS_HIDPI_AWARE}
  FScaler := TDummyDpiScaler.Create;
{$ELSE}
  FScaler := TFormDpiScaler.Create(Self);
  ApplyDpi(TScreen_GetDpiForForm(Self), nil);
{$ENDIF}
end;

{$IFDEF IDE_IS_HIDPI_AWARE}
procedure TfmBaseForm.WMDpiChanged(var _Msg: TWMDpi);
begin
  inherited;
  ApplyDpi(_Msg.YDpi, _Msg.ScaledRect);
  _Msg.Result := 0;
end;

procedure TfmBaseForm.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
begin
  if Assigned(FScaler) then
    FScaler.ApplyDpi(_NewDpi, _NewBounds);
  ArrangeControls;
end;

procedure TfmBaseForm.ArrangeControls;
begin
end;
{$ELSE}
function TDummyDpiScaler.Calc(_Value: Integer): Integer;
begin
  Result := _Value;
end;
{$ENDIF}

procedure TfmBaseForm.Loaded;
begin
  inherited;
  Scaled := False;
end;

constructor TfmBaseForm.Create(AOwner: TComponent);
begin
  inherited;
  GxSetDefaultFont(Self);
end;

destructor TfmBaseForm.Destroy;
begin
{$IFDEF IDE_IS_HIDPI_AWARE}
  FreeAndNil(FScaler);
{$ENDIF}
  inherited;
end;

end.
