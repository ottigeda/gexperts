unit GX_BaseForm;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  // Controls must be listed afer Forms because Forms.THintInfo has been deprecaded in newer
  // Delphi versions, but Controls.THintInfo does not exist in Delphi 6
  Controls,
  Dialogs,
  AppEvnts,
  u_dzDpiScaleUtils,
  u_dzVclUtils,
  GX_HintWindow;

type
  // All forms except docking forms must descend from this class.
  // Changes here must also be made to TfmIdeDockForm, since it must descend
  // from the IDE-internal TDockableForm class.
  TfmBaseForm = class(TForm)
    TheApplicationEvents: TApplicationEvents;
    procedure TheApplicationEventsShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure FormShow(Sender: TObject);
  protected
    procedure Loaded; override;
  protected
    FGxHintCustomDataRec: TGxHintCustomDataRec;
    FScaler: TFormDpiScaler;
    procedure WMDpiChanged(var _Msg: TWMDpi); message WM_DPICHANGED;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); virtual;
    procedure ArrangeControls; virtual;
  protected
    procedure InitDpiScaler;
  public
    class function Execute(_Owner: TComponent): Boolean; overload; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScaleHint(_HintWindow: TGxHintWindow);
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

procedure TfmBaseForm.FormShow(Sender: TObject);
begin
  ApplyDpi(TScreen_GetDpiForForm(Self), nil);
end;

procedure TfmBaseForm.InitDpiScaler;
begin
  FScaler := TFormDpiScaler.Create(Self);
  ArrangeControls;
end;

procedure TfmBaseForm.WMDpiChanged(var _Msg: TWMDpi);
begin
  inherited;
  ApplyDpi(_Msg.YDpi, _Msg.ScaledRect);
  _Msg.Result := 0;
end;

procedure TfmBaseForm.TheApplicationEventsShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  TGxHintWindow.HandleShowHintEvent(TheApplicationEvents, @FGxHintCustomDataRec,
    HintStr, CanShow, HintInfo);
end;

procedure TfmBaseForm.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
begin
  if Assigned(FScaler) then begin
    FScaler.ApplyDpi(_NewDpi, _NewBounds);
  end;
  ArrangeControls;
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
  FCurrentPPI := _NewDpi;
{$ENDIF}
end;

procedure TfmBaseForm.ArrangeControls;
begin
end;

procedure TfmBaseForm.Loaded;
begin
  inherited;
  Scaled := False;
end;

procedure TfmBaseForm.ScaleHint(_HintWindow: TGxHintWindow);
begin
  _HintWindow.Canvas.Font.Size := FScaler.Calc(Screen.HintFont.Size);
end;

constructor TfmBaseForm.Create(AOwner: TComponent);
begin
  inherited;
  FGxHintCustomDataRec.ScaleHint := Self.ScaleHint;
  CopyMemory(@(FGxHintCustomDataRec.ID), @GxHintCustomDataId, SizeOf(FGxHintCustomDataRec.ID));
  GxSetDefaultFont(Self);
  if not Assigned(OnShow) then
    OnShow := FormShow;
end;

destructor TfmBaseForm.Destroy;
begin
  FreeAndNil(FScaler);
  inherited;
end;

end.
