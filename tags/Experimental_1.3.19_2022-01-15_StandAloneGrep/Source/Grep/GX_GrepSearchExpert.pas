unit GX_GrepSearchExpert;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_Experts,
  GX_GrepOptions,
  GX_GrepExpert;

type
  TGrepSearchExpert = class(TGX_Expert)
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetActionCaption: string; override;
    function GetDefaultShortCut: TShortCut; override;
    ///<summary>
    /// @returns false, because we now group the Grep menu items in a submenu </summary>
    function HasMenuItem: Boolean; override;
    function CanHaveShortCut: boolean; override;
    function GetHelpString: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
    // The call count of the Grep Search expert is included with the Grep Results expert
    // so we return false here.
    function HasCallCount: Boolean; override;
  end;

implementation

uses
  Menus,
  GX_GrepResults,
  GX_GrepBackend,
  GX_ActionBroker;

{ TGrepSearchExpert }

constructor TGrepSearchExpert.Create;
begin
  inherited Create;

{$IFNDEF GX_STANDALONE}
  // since we no longer have a menu entry in the GExperts menu
  // we need to create an action here
  FActionInt := GxActionBroker.RequestAction(GetActionName, GetBitmap);
  FActionInt.OnExecute := Self.Execute;
  FActionInt.Caption := GetActionCaption;
{$ENDIF GX_STANDALONE}
end;

function TGrepSearchExpert.GetActionCaption: string;
resourcestring
  SActionCaption = 'Grep &Search...';
begin
  Result := SActionCaption;
end;

function TGrepSearchExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Word('S'), [ssAlt, ssShift]);
end;

function TGrepSearchExpert.GetHelpString: string;
resourcestring
  SHelpString =
    '  Grep regular expressions allow you to formulate complex searches'#13#10
    + '  that are not possible using a basic text search.'#13#10
    + '  GExperts implements a subset of the Perl regular expression syntax.';
begin
  Result := SHelpString;
end;

class function TGrepSearchExpert.GetName: string;
begin
  Result := 'GrepSearch'; // Do not localize.
end;

function TGrepSearchExpert.HasCallCount: Boolean;
begin
  Result := False;
end;

function TGrepSearchExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

procedure TGrepSearchExpert.Execute(Sender: TObject);
resourcestring
  SGrepResultsNotActive = 'The Grep Results window is not active';
begin
  if Assigned(fmGrepResults) then begin
    fmGrepResults.Execute(gssNormal);
  end else
    raise Exception.Create(SGrepResultsNotActive);
end;

function TGrepSearchExpert.CanHaveShortCut: boolean;
begin
  Result := True;
end;

procedure TGrepSearchExpert.Configure;
var
  UseCurrentIdent: Boolean;
  ExternalEditor: string;
  Params: string;
  AddToBackground: Boolean;
  AddToFolders: Boolean;
begin
  Assert(Assigned(gblGrepExpert), 'gblGrepExpert is not assigned');

  UseCurrentIdent := gblGrepExpert.GrepUseCurrentIdent;
  ExternalEditor := gblGrepExpert.ExternalEditor;
  Params := gblGrepExpert.ExternalEditorParams;
  AddToBackground := gblGrepExpert.ExplorerAddToBackground;
  AddToFolders := gblGrepExpert.ExplorerAddToFolders;
  if TfmGrepOptions.Execute(nil, UseCurrentIdent, ExternalEditor, Params, AddToBackground, AddToFolders) then begin
    gblGrepExpert.GrepUseCurrentIdent := UseCurrentIdent;
    gblGrepExpert.ExternalEditor := ExternalEditor;
    gblGrepExpert.ExternalEditorParams := Params;
    gblGrepExpert.ExplorerAddToBackground := AddToBackground;
    gblGrepExpert.ExplorerAddToFolders := AddToFolders;
  end;
end;

initialization
  RegisterGX_Expert(TGrepSearchExpert);
end.
