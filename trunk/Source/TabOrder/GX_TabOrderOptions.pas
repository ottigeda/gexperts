unit GX_TabOrderOptions;

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
  StdCtrls,
  ExtCtrls,
  Buttons,
  ActnList,
  Actions,
  GX_BaseForm,
  GX_TabOrder;

type
  TfmTabOrderOptions = class(TfmBaseForm)
    b_Ok: TButton;
    b_Cancel: TButton;
    rg_AutoSort: TGroupBox;
    sb_DoNotSort: TSpeedButton;
    sb_XthenY: TSpeedButton;
    sb_YthenX: TSpeedButton;
    TheActionList: TActionList;
    act_YthenX: TAction;
    act_XthenY: TAction;
    act_DoNotSort: TAction;
    procedure act_YthenXExecute(Sender: TObject);
    procedure act_XthenYExecute(Sender: TObject);
    procedure act_DoNotSortExecute(Sender: TObject);
  private
    procedure SetData(_AutoSort: TTabAutoSortEnum);
    procedure GetData(out _AutoSort: TTabAutoSortEnum);
  public
    class function Execute(_Owner: TWinControl; var _AutoSort: TTabAutoSortEnum): Boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ TfmTabOrderOptions }

procedure TfmTabOrderOptions.act_DoNotSortExecute(Sender: TObject);
begin
  act_DoNotSort.Checked := True;
//  sb_DoNotSort.Down := True;
end;

procedure TfmTabOrderOptions.act_YthenXExecute(Sender: TObject);
begin
  act_YthenX.Checked := True;
//  sb_HorizontalFirst.Down := True;
end;

procedure TfmTabOrderOptions.act_XthenYExecute(Sender: TObject);
begin
  act_XthenY.Checked := True;
//  sb_VertialFirst.Down := True;
end;

class function TfmTabOrderOptions.Execute(_Owner: TWinControl; var _AutoSort: TTabAutoSortEnum): Boolean;
var
  frm: TfmTabOrderOptions;
  Int: IInterface;
begin
  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  Int := TemporarilyDisableHighDpi;
  frm := TfmTabOrderOptions.Create(_Owner);
  try
    frm.TemporarilyDisableHighDpiInterface := Int;
    Int := nil;
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_AutoSort);
    Result := (frm.ShowModal = mrOK);
    if Result then
      frm.GetData(_AutoSort);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmTabOrderOptions.GetData(out _AutoSort: TTabAutoSortEnum);
begin
  if act_YthenX.Checked then
    _AutoSort := tasYthenX
  else if act_XthenY.Checked then
    _AutoSort := tasXthenY
  else
    _AutoSort := tasNone;
end;

procedure TfmTabOrderOptions.SetData(_AutoSort: TTabAutoSortEnum);
begin
  case _AutoSort of
    tasYthenX: act_YthenX.Checked := True;
    tasXthenY: act_XthenY.Checked := True;
  else // tasNone:
    act_DoNotSort.Checked := True;
  end;
end;

end.
