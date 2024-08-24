unit GX_CodeSrch;

interface

uses
  Classes, Controls, Forms, StdCtrls, GX_BaseForm;

type
  TfmCodeSearch = class(TfmBaseForm)
    lblFind: TLabel;
    edSearch: TEdit;
    gbxOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbWholeWord: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TfmCodeSearch }

constructor TfmCodeSearch.Create(_Owner: TComponent);
begin
  inherited;
  InitDpiScaler;
end;

end.
