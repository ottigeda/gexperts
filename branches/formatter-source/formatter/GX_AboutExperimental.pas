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
    l_Formatter: TLabel;
  protected
    class function doAddToAboutDialog: integer; override;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  GX_GenericUtils;

{ TfmAboutExperimental }

constructor TfmAboutExperimental.Create(_Owner: TComponent);
begin
  inherited;

  SetFontUnderline(l_DummzeuchDe);
  SetFontColor(l_DummzeuchDe, clBlue);
  SetFontUnderline(l_Formatter);
  SetFontColor(l_Formatter, clBlue);
end;

class function TfmAboutExperimental.doAddToAboutDialog: integer;
var
  bmSplashScreen: HBITMAP;
  AboutBoxServices: IOTAAboutBoxServices;
begin
  Result := -1;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
    bmSplashScreen := LoadBitmap(HInstance, 'SplashScreenBitMap');
    Result := AboutBoxServices.AddPluginInfo(
      'GExperts Experimental',
      'GExperts is a free set of tools built to increase the productivity of Delphi and C++Builder'
      + ' programmers by adding several features to the IDE.'
      + ' GExperts is developed as Open Source software and we encourage user contributions to the project.'#13#10
      + '(c) 2015 by Thomas Mueller'#13#10
      + 'http://blog.dummzeuch.de',
      bmSplashScreen,
      False,
      GetVersionStr + ' experimental', 'Open Source');
  end;
end;

initialization
  TfmAboutExperimental.SetCustomBuildEmails('gexperts@dummzeuch.de', 'gexperts@dummzeuch.de');
  gblAboutFormClass := TfmAboutExperimental;
end.

