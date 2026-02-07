unit GX_GxUtils;

interface

uses
  Windows,
  SysUtils,
  Controls;

procedure GxSetDefaultFont(Control: TControl);
procedure GxContextHelp(const HelpOwner: TWinControl; const ContextID: Integer);
procedure GxContextHelpContents(const HelpOwner: TWinControl);

implementation

uses
  GX_GenericUtils,
  u_dzVclUtils;

procedure CallWinHelp(const Command, ContextID: Integer; const HelpOwner: TWinControl);
var
  HelpFn: string;
begin
  HelpFn := TApplication_GetExePathBS + 'GExperts.chm';
  if FileExists(HelpFn) then
    // The 0 allows the help to drop behind the IDE
    HtmlHelp(0, PChar(HelpFn), Command, ContextID)
  else
    raise Exception.Create('The configured help file is missing: ' + HelpFn);
end;

procedure GxContextHelpContents(const HelpOwner: TWinControl);
begin
  CallWinHelp(HH_DISPLAY_INDEX, 0, HelpOwner);
end;

procedure GxContextHelp(const HelpOwner: TWinControl; const ContextID: Integer);
begin
  CallWinHelp(HH_HELP_CONTEXT, ContextID, HelpOwner);
end;

procedure GxSetDefaultFont(Control: TControl);
begin
  SetDefaultFont(Control);
end;

end.
