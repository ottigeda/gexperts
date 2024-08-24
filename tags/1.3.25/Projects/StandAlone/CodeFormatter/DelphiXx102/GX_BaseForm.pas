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
  Dialogs;

type
  TfmBaseForm = class(TForm)
  protected
    procedure InitDpiScaler; virtual;
  public
  end;

implementation

{$R *.dfm}

{ TfmBaseForm }

procedure TfmBaseForm.InitDpiScaler;
begin

end;

end.

