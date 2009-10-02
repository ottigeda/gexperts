unit GX_AboutExperimental;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GX_About,
  ExtCtrls,
  StdCtrls;

type
  TfmAboutExperimental = class(TfmAbout)
    l_CreatedBy: TLabel;
    l_DummzeuchDe: TLabel;
  private
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GX_GenericUtils;

{ TfmAboutExperimental }

constructor TfmAboutExperimental.Create(_Owner: TComponent);
begin
  inherited;
  SetFontUnderline(l_DummzeuchDe);
  SetFontColor(l_DummzeuchDe, clBlue);
end;

initialization
  //  TfmAbout.SetCustomBuildDetails('Experimental version by http://www.dummzeuch.de send bug reports to gexperts@dummzeuch.de');
  gblAboutFormClass := TfmAboutExperimental;
end.

