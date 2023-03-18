unit GX_AboutExperimental;

{$I GX_CondDefine.inc}

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
    l_Formatter: TLabel;
  protected
    class function GetVersionStr: string; override;
    class function DoAddToAboutDialog: Integer; override;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  u_dzVclUtils,
  GX_LibrarySource;

{ TfmAboutExperimental }

constructor TfmAboutExperimental.Create(_Owner: TComponent);
begin
  inherited;

  TLabel_MakeUrlLabel(l_DummzeuchDe);
  TLabel_MakeUrlLabel(l_Formatter, 'http://web.archive.org/web/20121115141650/http://www.aew.wur.nl/UK/Delforexp');
end;

class function TfmAboutExperimental.GetVersionStr: string;
begin
  Result := inherited GetVersionStr;
end;

class function TfmAboutExperimental.DoAddToAboutDialog: Integer;
{$IFDEF GX_VER170_up}
// Only Delphi 2005 and up support the about box services
const
  Description = 'GExperts is a free set of tools built to increase the productivity of Delphi and C++Builder'
    + ' programmers by adding several features to the IDE.'
    + ' GExperts is developed as Open Source software and we encourage user contributions to the project.'#13#10
    + '(c) Erik Berry and the GExperts Team';
var
  AboutBoxServices: IOTAAboutBoxServices;
  DupeString: string;
  Desc: string;
begin
  if GExpertsDllMarker = nil then begin
    DupeString := ' (duplicate, inactive)';
    Desc := 'GExperts is listed twice in the IDE Experts list, so this instance is inactive.'#13#10
      + 'Use the Expert Manager to remove the duplicate!';
  end else
    Desc := Description;
  Result := -1;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
    Result := AboutBoxServices.AddPluginInfo(
      'GExperts Experimental' + DupeString,
      Desc + #13#10
      + 'https://gexperts.dummzeuch.de/',
      GetAboutIcon,
      False,
      '', // leave this empty!
      GetVersionStr + DupeString);
  end;
end;
{$ELSE not GX_VER170_up}
begin
  Result := -1;
end;
{$ENDIF not GX_VER170_up}

initialization
  TfmAboutExperimental.SetCustomBuildEmails(
    'https://bugs.dummzeuch.de/',
    'https://features.dummzeuch.de/');
  gblAboutFormClass := TfmAboutExperimental;
end.

