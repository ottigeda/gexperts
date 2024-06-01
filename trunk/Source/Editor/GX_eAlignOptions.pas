unit GX_eAlignOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, StdCtrls, Forms, GX_BaseForm,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TfmAlignOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxTokens: TGroupBox;
    gbxOptions: TGroupBox;
    lblWhitespace: TLabel;
    edtWhitespace: TEdit;
    mmoTokens: TMemo;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

uses
  u_dzVclUtils;

{$R *.dfm}

{ TfmAlignOptions }

constructor TfmAlignOptions.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
  InitDpiScaler;
end;

end.
