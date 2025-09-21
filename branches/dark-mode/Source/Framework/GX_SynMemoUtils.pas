unit GX_SynMemoUtils;

{$I GX_CondDefine.inc}

interface

uses  // If you get errors here, add SynEdit to your library path
  SynEdit,
{$IFDEF GX_SUPPORTS_THEMING}
  SynMemo,
  Vcl.Themes,
  Vcl.Forms,
{$ENDIF}
  GX_GenericUtils, GX_IDEUtils;

function GetGXHighlighterForCurrentSourceEditor: TGXSyntaxHighlighter;

procedure SetSynEditHighlighter(SynEdit: TCustomSynEdit; Highlighter: TGXSyntaxHighlighter);

implementation

uses
  Windows, SysUtils, Classes, Graphics, ToolsAPI,
  SynEditHighlighter, SynHighlighterPas, SynHighlighterCpp, SynHighlighterHtml,
  SynHighlighterSql, SynHighlighterCS, SynHighlighterXML, SynHighlighterGeneral,
  GX_OtaUtils;

procedure GxGetIDEHighLigherSettings(Highlighter: TSynCustomHighlighter; const Prefer: string);
var
  Elements: TStrings;
  PreferIndex: Integer;
begin
  Elements := TStringList.Create;
  try
    Highlighter.EnumUserSettings(Elements);
    PreferIndex := Elements.IndexOf(Prefer);
    if PreferIndex > -1 then
      Highlighter.UseUserSettings(PreferIndex)
    else if Elements.Count > 0 then
      Highlighter.UseUserSettings(Elements.Count - 1);
  finally
    FreeAndNil(Elements);
  end;
end;

procedure SetSynEditHighlighter(SynEdit: TCustomSynEdit; Highlighter: TGXSyntaxHighlighter);
var
  i: Integer;
begin
  Assert(SynEdit <> nil);
  SynEdit.Highlighter.Free;
  SynEdit.Highlighter := nil;

  case Highlighter of
    gxpPAS:  SynEdit.Highlighter := TSynPasSyn.Create(SynEdit);
    gxpCPP:  SynEdit.Highlighter := TSynCppSyn.Create(SynEdit);
    gxpHTML: SynEdit.Highlighter := TSynHtmlSyn.Create(SynEdit);
    gxpCS:   SynEdit.Highlighter := TSynCSSyn.Create(SynEdit);
    gxpXML:  SynEdit.Highlighter := TSynXMLSyn.Create(SynEdit);
    gxpSQL:
      begin 
        SynEdit.Highlighter := TSynSqlSyn.Create(SynEdit);
        for i := 0 to SynEdit.Highlighter.AttrCount - 1 do
        begin
          if SynEdit.Highlighter.Attribute[i].Name = 'Number' then
            SynEdit.Highlighter.Attribute[i].Foreground := clMaroon
          else if SynEdit.Highlighter.Attribute[i].Name = 'String' then
            SynEdit.Highlighter.Attribute[i].Foreground := clTeal
          else if SynEdit.Highlighter.Attribute[i].Name = 'Comment' then
          begin
            SynEdit.Highlighter.Attribute[i].Foreground := clBlue;
            SynEdit.Highlighter.Attribute[i].Style := SynEdit.Highlighter.Attribute[i].Style - [fsItalic];
          end;
        end;
      end;
  else
    SynEdit.Highlighter := TSynGeneralSyn.Create(SynEdit);
  end;
  if Highlighter in [gxpPAS, gxpCPP, gxpSQL, gxpCS] then
  begin
    GxGetIDEHighLigherSettings(SynEdit.Highlighter, GetIDEVersionID);
{$IFDEF GX_SUPPORTS_THEMING}
    var Key: string;
    if GxOtaThemingEnabled
    and IdeStyleIsDark // ColorToGrayscale(GxOtaGetThemedColor(clWindow)) < 127)
    then begin
      // Dark Mode
      Key := GxOtaGetIdeBaseRegistryKey + '\Editor\Highlight';
      for i := 0 to SynEdit.Highlighter.AttrCount-1 do
      begin
        SynEdit.Highlighter.Attribute[i].LoadFromBorlandRegistry(
          HKEY_CURRENT_USER,
          Key,
          SynEdit.Highlighter.Attribute[i].FriendlyName,
          False
        );
      end;
      SynEdit.Highlighter.WhitespaceAttribute.LoadFromBorlandRegistry(
        HKEY_CURRENT_USER,
        Key,
        'Whitespace',
        False
      );
      SynEdit.Color := SynEdit.Highlighter.WhitespaceAttribute.Background;
    end;
{$ENDIF}
  end;
end;

function GetGXHighlighterForCurrentSourceEditor: TGXSyntaxHighlighter;
var
  SourceEditor: IOTASourceEditor;
begin
  Result := gxpNone;

  if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
    Exit;

  Result := GxOtaGetCurrentSyntaxHighlighter(SourceEditor);
end;

{$IFDEF GX_SUPPORTS_THEMING}
Initialization
  TCustomStyleEngine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TSynMemo, TScrollingStyleHook);

Finalization
  TCustomStyleEngine.UnRegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);
  TCustomStyleEngine.UnRegisterStyleHook(TSynMemo, TScrollingStyleHook);
{$ENDIF GX_SUPPORTS_THEMING}

end.

