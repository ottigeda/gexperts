unit GX_Progress;

interface

uses
  Classes, Controls, Forms, ComCtrls, GX_BaseForm;

type
  TfmProgress = class(TfmBaseForm)
    Progress: TProgressBar;
  private
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TfmProgress }

constructor TfmProgress.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

end.
