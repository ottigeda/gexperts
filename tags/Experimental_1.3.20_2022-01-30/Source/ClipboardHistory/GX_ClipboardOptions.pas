unit GX_ClipboardOptions;

interface

uses
  Classes, StdCtrls, Controls, Forms, GX_BaseForm;

type
  TfmClipboardOptions = class(TfmBaseForm)
    gbxClipboardOptions: TGroupBox;
    lblMaxEntries: TLabel;
    edtMaxClip: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    chkAutoStart: TCheckBox;
    chkAutoClose: TCheckBox;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TfmClipboardOptions }

constructor TfmClipboardOptions.Create(_Owner: TComponent);
begin
  inherited;
  InitDpiScaler;
end;

end.
