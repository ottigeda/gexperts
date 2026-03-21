unit GX_About;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  GX_BaseForm,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TfmAbout = class(TfmBaseForm)
    lblGExperts: TLabel;
    btnClose: TButton;
    lblVersion: TLabel;
    pnlLogo: TPanel;
    imgLogo: TImage;
    btnEmail: TButton;
    lblWebPage: TLabel;
    lblProjectLeader: TLabel;
    lblContributors: TLabel;
    lblProjectLeaderName: TLabel;
    lblWebSite: TLabel;
    lblPreRelease1: TLabel;
    lblPreRelease2: TLabel;
    mmoBuildDetails: TMemo;
    mmoContributors: TMemo;
    tim_Scroll: TTimer;
    lblIdeVersion: TLabel;
    procedure btnEmailClick(Sender: TObject);
    procedure tim_ScrollTimer(Sender: TObject);
  private
    procedure InitFonts;
    procedure InitVersionInfoControls;
  protected
    class function GetFileDescription: string; virtual;
    class function GetVersionStr: string; virtual;
    class function DoAddToAboutDialog: Integer; virtual;
    class function GetAboutIcon: HBITMAP; virtual;
    class function GetSplashIcon: HBITMAP; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // If you release an experimental GExperts, either
    // set gblAboutFormClass to your own descentant of this form or
    // call SetCustomBuildDetails and SetCustomBuildEmails to
    // describe your build and provide feedback email adresses.
    class procedure SetCustomBuildDetails(const Details: string);
    class procedure SetCustomBuildEmails(const ABugEmail, ASuggestionEmail: string);
    class procedure AddToSplashScreen;
    class procedure AddToAboutDialog;
    class procedure RemoveFromAboutDialog;
  end;

type
  TAboutFormClass = class of TfmAbout;

var
  gblAboutFormClass: TAboutFormClass;

implementation

{$R *.dfm}
{$R GX_About.res}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Graphics, ToolsApi, Messages,
  u_dzVclUtils, u_dzVersionInfo,
  GX_GxUtils,
{$IFNDEF GX_STANDALONE}
  GX_GetIdeVersion, GX_DuplicateDllHandler,
{$ENDIF}
  GX_GenericUtils, GX_FeedbackWizard;

const
  DefaultBugEmail = 'https://bugs.dummzeuch.de/';  // Do not localize.
  DefaultSuggestionEmail = 'https://features.dummzeuch.de/'; // Do not localize.
var
  BuildDetails: string = '';
  BugEmail: string = DefaultBugEmail;
  SuggestionEmail: string = DefaultSuggestionEmail;

procedure TfmAbout.btnEmailClick(Sender: TObject);
begin
{$IF DECLARED(TfmFeedbackWizard)}
  TfmFeedbackWizard.Execute(Application, BugEmail, SuggestionEmail);
  Close;
{$IFEND}
end;

constructor TfmAbout.Create(AOwner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
  GxSetDefaultFont(Self);
  TLabel_MakeUrlLabel(lblWebPage);

{$IF DECLARED(TfmFeedbackWizard)}
  btnEmail.Visible := True;
  lblProjectLeaderName.Cursor := crHandPoint;
  lblProjectLeaderName.OnClick := btnEmailClick;
{$ELSE}
  btnEmail.Visible := False;
{$IFEND}

  imgLogo.Picture.Bitmap.LoadFromResourceName(HInstance, 'ABOUT_WIZ');
  InitVersionInfoControls;

  if NotEmpty(BuildDetails) then
  begin
    if (BugEmail = DefaultBugEmail) or (SuggestionEmail = DefaultSuggestionEmail) then
      btnEmail.Visible := False;
    mmoBuildDetails.Visible := True;
    mmoBuildDetails.Lines.Text := BuildDetails;
  end
  else
  begin
    if gblAboutFormClass = TfmAbout then
      btnEmail.Visible := True;
    mmoBuildDetails.Visible := False;
  end;

  InitDpiScaler;
  InitFonts;
end;

procedure TfmAbout.InitFonts;
begin
  SetFontBold(lblContributors);
  SetFontBold(lblProjectLeader);
  SetFontBold(lblWebSite);
  SetFontBold(lblVersion);
  SetFontBold(lblGExperts);
  SetFontColor(lblPreRelease1, clRed);
  SetFontColor(lblPreRelease2, clRed);
  SetFontSize(lblGExperts, +4);
  SetFontSize(lblVersion, +2);
{$IF DECLARED(TfmFeedbackWizard)}
  SetFontUnderline(lblProjectLeaderName);
  SetFontColor(lblProjectLeaderName, clBlue);
{$IFEND}
  SetFontColor(mmoBuildDetails, clRed);
end;

procedure TfmAbout.InitVersionInfoControls;
begin
  lblGExperts.Caption := GetFileDescription;
  lblVersion.Caption := GetVersionStr;
{$IF declared(GetBorlandIdeVersionStr)}
  lblIdeVersion.Caption := 'running in ' + GetBorlandIdeVersionStr;
{$ELSE}
  lblIdeVersion.Visible := False;
{$IFEND}
end;

class procedure TfmAbout.SetCustomBuildDetails(const Details: string);
begin
  BuildDetails := Details;
end;

class procedure TfmAbout.SetCustomBuildEmails(const ABugEmail, ASuggestionEmail: string);
begin
  BugEmail := ABugEmail;
  SuggestionEmail := ASuggestionEmail;
end;

procedure TfmAbout.tim_ScrollTimer(Sender: TObject);
// var
//   Res: Integer;

 function MustRestart: Boolean;
 var
   TopLineIdx,
   VisibleLinesCount: Integer;
 begin
  TopLineIdx := SendMessage(mmoContributors.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  VisibleLinesCount := mmoContributors.Height div (abs(mmoContributors.Font.Height) + 2);
  Result := (TopLineIdx = succ(mmoContributors.Lines.Count - VisibleLinesCount));
 end;

begin
  inherited;

  if mmoContributors.Focused then
    Exit;
    
  // Res :=
  SendMessage(mmoContributors.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
  if MustRestart then begin
    // if Res = 0 then begin - no correct restart!
    // we have reached the end
    SendMessage(mmoContributors.Handle, WM_VSCROLL, SB_TOP, 0);
  end;
end;

class function TfmAbout.GetAboutIcon: HBITMAP;
const
  GX_ABOUT_ICON32 = 'GX32';
begin
  Result := LoadBitmap(HInstance, GX_ABOUT_ICON32);
end;

class function TfmAbout.GetSplashIcon: HBITMAP;
const
  GX_ABOUT_ICON24 = 'GX24';
begin
  Result := LoadBitmap(HInstance, GX_ABOUT_ICON24);
end;

class function TfmAbout.GetVersionStr: string;
resourcestring
  SVersion = 'Version';
  SUnknown = '<unknown>';
var
  Info: IFileInfo;
  Version: TFileVersionRec;
begin
  Info := TDllInfo.Create;
  if Info.HasVersionInfo then begin
    Version := Info.FileVersionRec;
    Result := Info.FileVersionStr();
    if Version.Build <> 0 then
      Result := Result + '  build ' + IntToStr(Version.Build);
  end else
    Result := SUnknown;
{$IFOPT D+}
  Result := SVersion + ' ' + Result + ' - Debug';
{$ELSE}
  Result := SVersion + ' ' + Result;
{$ENDIF}
end;

{$IFOPT D+}
procedure SendDebugComponent(_cmp: TComponent; _Recursive: Boolean = False; _Prefix: string = '');
var
  i: integer;
begin
  SendDebug(_Prefix + _cmp.Name + ': ' + _cmp.ClassName);
  if  _Recursive and (_cmp.ComponentCount > 0) then begin
    SendIndent;
    for i := 0 to _cmp.ComponentCount - 1 do begin
      SendDebugComponent(_cmp.Components[i], _Recursive, '(' + IntToStr(i) + ') ');
    end;
    SendUnIndent;
  end;
end;
{$ENDIF}

procedure AddPluginToSplashScreen(_Icon: HBITMAP; const _Title: string; const _Version: string);
{$IF declared(SplashScreenServices)}
// Only Delphi 2005 and up support the splash screen services
begin
  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap(_Title,
      _Icon, False, _Version);
end;
{$ELSE}
const
  XPOS = 140;
  YPOS = 150;
  PluginLogoStr = 'PluginLogo';
var
  imgLogo: TImage;
  lblTitle: TLabel;
  lblVersion: TLabel;
  i: integer;
  PluginIdx: integer;
  frm: TCustomForm;
begin
{$IFOPT D+} SendDebug('Screen.Forms[]:'); {$ENDIF}
{$IFOPT D+} SendIndent; {$ENDIF}
  for i := 0 to Screen.FormCount - 1 do begin
    frm := Screen.Forms[i];
    if (frm.Name = 'SplashScreen') and frm.ClassNameIs('TForm') then begin
      {$IFOPT D+} SendDebugComponent(frm, True, '(' + IntToStr(i) + ') '); {$ENDIF}
      PluginIdx := 0;
      while frm.FindComponent(PluginLogoStr + IntToStr(PluginIdx)) <> nil do
        Inc(PluginIdx);

      imgLogo := TImage.Create(frm);
      imgLogo.Name := PluginLogoStr + IntToStr(PluginIdx);
      imgLogo.Parent := frm;
      imgLogo.AutoSize := True;
      imgLogo.Picture.Bitmap.Handle := _Icon;
      imgLogo.Left := XPOS;
      imgLogo.Top := YPOS + 32 * PluginIdx;

      lblTitle := TLabel.Create(frm);
      lblTitle.Name := 'PluginTitle' + IntToStr(PluginIdx);
      lblTitle.Parent := frm;
      lblTitle.Caption := _Title;
      lblTitle.Top := imgLogo.Top;
      lblTitle.Left := imgLogo.Left + 32 + 8;
      lblTitle.Transparent := True;
      lblTitle.Font.Color := clWhite;
      lblTitle.Font.Style := [fsbold];

      lblVersion := TLabel.Create(frm);
      lblVersion.Name := 'PluginVersion' + IntToStr(PluginIdx);
      lblVersion.Parent := frm;
      lblVersion.Caption := _Version;
      lblVersion.Top := imgLogo.Top + lblTitle.Height;
      lblVersion.Left := imgLogo.Left + 32 + 20;
      lblVersion.Transparent := True;
      lblVersion.Font.Color := clWhite;
    end else begin
      {$IFOPT D+} SendDebugComponent(frm, False, '(' + IntToStr(i) + ') '); {$ENDIF}
    end;
  end;
{$IFOPT D+} SendUnIndent; {$ENDIF}
end;
{$IFEND declared(SplashScreenServices)}

class procedure TfmAbout.AddToSplashScreen;
var
  VerString: string;
  FileDesc: string;
begin
  FileDesc := GetFileDescription;
  VerString := GetVersionStr;
{$IFNDEF GX_STANDALONE}
  if IsDuplicateGExpertsDll() then
    VerString := VerString + ' (duplicate, inactive)';
{$ENDIF}
  AddPluginToSplashScreen(GetSplashIcon, FileDesc, VerString);
end;

class function TfmAbout.GetFileDescription: string;
var
  Info: IFileInfo;
begin
  Info :=  TDllInfo.Create;
  if Info.HasVersionInfo then
    Result := Info.FileDescription
  else
    Result := 'GExperts';
end;

class function TfmAbout.DoAddToAboutDialog: Integer;
{$IF declared(IOTAAboutBoxServices)}
// Only Delphi 2005 and up support the about box services
const
  Description = 'GExperts is a free set of tools built to increase the productivity of Delphi and C++Builder'
      + ' programmers by adding several features to the IDE.'
      + ' GExperts is developed as Open Source software and we encourage user contributions to the project.'#13#10
      + '(c) Thomas Mueller, Erik Berry and the GExperts Team';
var
  AboutBoxServices: IOTAAboutBoxServices;
  DupeString: string;
  FileDesc: string;
  Desc: string;
  VerString: string;
begin
{$IFNDEF GX_STANDALONE}
  if IsDuplicateGExpertsDll() then begin
    DupeString := ' (duplicate, inactive)';
    Desc := 'GExperts is listed twice in the IDE Experts list, so this instance is inactive.'#13#10
      + 'Use the Expert Manager to remove the duplicate!';
  end else
{$ENDIF GX_STANDALONE}
    Desc := Description;
  Result := -1;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
    FileDesc := GetFileDescription;
    VerString := GetVersionStr;
    Result := AboutBoxServices.AddPluginInfo(
      FileDesc + DupeString,
      Desc + #13#10
      + 'https://gexperts.dummzeuch.de',
      GetAboutIcon,
      False,
      '', // leave this empty!
      VerString);
  end;
end;
{$ELSE}
begin
  Result := -1;
end;
{$IFEND declared(IOTAAboutBoxServices)}

var
  FAboutPluginIndex: Integer;

class procedure TfmAbout.AddToAboutDialog;
begin
  FAboutPluginIndex := DoAddToAboutDialog;
end;

class procedure TfmAbout.RemoveFromAboutDialog;
{$IF declared(IOTAAboutBoxServices)}
// Only Delphi 2005 and up support the about box services
var
  AboutBoxServices: IOTAAboutBoxServices;
begin
  if FAboutPluginIndex <> -1 then
  begin
    if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
      AboutBoxServices.RemovePluginInfo(FAboutPluginIndex);
    FAboutPluginIndex := -1;
  end;
end;
{$ELSE}
begin //fi:W519
end;
{$IFEND declared(IOTAAboutBoxServices)}

initialization
  TfmAbout.AddToSplashScreen;
  gblAboutFormClass := TfmAbout;

finalization
end.

