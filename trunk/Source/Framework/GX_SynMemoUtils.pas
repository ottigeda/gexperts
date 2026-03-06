unit GX_SynMemoUtils;

{$I GX_CondDefine.inc}

interface

uses // If you get errors here, add SynEdit to your unit search path
  SynEdit,
  SynMemo,
{$IFDEF GX_SUPPORTS_THEMING}
  Vcl.Themes,
  Vcl.Forms,
{$ENDIF}
  GX_GenericUtils, GX_IDEUtils, GX_StringList;

type
  TSynMemoUtil = class
  private
    procedure GetSelectedLines(out _StartIdx, _EndIdx: Integer);
  protected
    FMemo: TSynMemo;
    function ProcessLine(const _Line: string): string; virtual; abstract;
  public
    class procedure ProcessSelectedLines(_Memo: TSynMemo); overload; virtual;
    class procedure ProcessAllLines(_Memo: TSynMemo); overload; virtual;
    constructor Create(_Memo: TSynMemo); virtual;
    procedure ProcessSelectedLines; overload;
    procedure ProcessAllLines; overload;
  end;

type
  TSynMemoUtilPrependDots = class(TSynMemoUtil)
  protected
    function ProcessLine(const _Line: string): string; override;
  end;

type
  TSynMemoUtilRemoveDots = class(TSynMemoUtil)
  protected
    function ProcessLine(const _Line: string): string; override;
  end;

type
  TSynMemoUtilMakePathsAbsoluteOrRelative = class(TSynMemoUtil)
  protected
    FProjectDir: string;
  public
    constructor Create(_Memo: TSynMemo); override;
    property ProjectDir: string read FProjectDir;
  end;

type
  TSynMemoUtilMakePathsAbsolute = class(TSynMemoUtilMakePathsAbsoluteOrRelative)
  public
    function ProcessLine(const _Line: string): string; override;
  end;

type
  TSynMemoUtilMakePathsRelative = class(TSynMemoUtilMakePathsAbsoluteOrRelative)
  public
    function ProcessLine(const _Line: string): string; override;
  end;

procedure TSynMemo_UpdateScrollbars(ME: TSynMemo);

procedure TSynMemo_SetDelimiter(_m: TSynMemo);
function TSynMemo_GetDelimitedText(_m: TSynMemo): TGXUnicodeString;
procedure TSynMemo_SetDelimitedText(_m: TSynMemo; const _Value: TGXUnicodeString);

function GetGXHighlighterForCurrentSourceEditor: TGXSyntaxHighlighter;

procedure SetSynEditHighlighter(SynEdit: TCustomSynEdit; Highlighter: TGXSyntaxHighlighter);

implementation

uses
  TypInfo, Windows, SysUtils, Classes, StrUtils, Graphics, ToolsAPI, StdCtrls,
  SynEditHighlighter, SynHighlighterPas, SynHighlighterCpp, SynHighlighterHtml,
  SynHighlighterSql, SynHighlighterCS, SynHighlighterXML, SynHighlighterGeneral,
  SynEditTypes,
  u_dzStringUtils, u_dzFileUtils,
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
    if Ota.ThemingEnabled
    and Ota.IdeStyleIsDark // ColorToGrayscale(GxOtaGetThemedColor(clWindow)) < 127)
    then begin
      // Dark Mode
      Key := Ota.GetIdeBaseRegistryKey + '\Editor\Highlight';
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

  if not Ota.TryGetCurrentSourceEditor(SourceEditor) then
    Exit;

  Result := GxOtaGetCurrentSyntaxHighlighter(SourceEditor);
end;

const
  ///<summary> property types that can be converted to string </summary>
  STRING_PROPERTY_TYPES = [
{$IFDEF FPC}
    TypInfo.tkAString,
    TypInfo.tkUString,
{$ENDIF}
{$IFDEF SUPPORTS_UNICODE}
    TypInfo.tkUString,
{$ENDIF SUPPORTS_UNICODE}
    TypInfo.tkChar, TypInfo.tkString, TypInfo.tkWChar, TypInfo.tkLString, TypInfo.tkWString];

function TrySetStringProperty(_Instance: TObject; const _PropName, _PropValue: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _PropName);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in STRING_PROPERTY_TYPES);
  if Result then
    SetPropValue(_Instance, _PropName, _PropValue);
end;

function TryGetStringProperty(_Instance: TObject; const _PropName: string; out _PropValue: TGXUnicodeString): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(_Instance.ClassInfo, _PropName);
  Result := Assigned(PropInfo) and (PropInfo.PropType^.Kind in STRING_PROPERTY_TYPES);
  if Result then
    _PropValue := GetPropValue(_Instance, _PropName);
end;

procedure TSynMemo_SetDelimiter(_m: TSynMemo);
begin
{$IFDEF UNICODE}
  _m.Lines.Delimiter := ';';
{$ENDIF}
end;

procedure TSynMemo_UpdateScrollbars(ME: TSynMemo);
begin
  ME.Scrollbars := ssNone;
  ME.Scrollbars := ssBoth;
end;

function TSynMemo_GetDelimitedText(_m: TSynMemo): TGXUnicodeString;
{$IFDEF UNICODE}
begin
  Result := _m.Lines.DelimitedText;
end;
{$ELSE}
var
  Lines: TGXUnicodeStrings;
  Line: TGXUnicodeString;
  i: Integer;
begin
  Lines := _m.Lines;
  Result := '';
  for i := 0 to Lines.Count - 1 do begin
    Line := Lines[i];
    if Line <> '' then begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + Line;
    end;
  end;
end;
{$ENDIF}

procedure TSynMemo_SetDelimitedText(_m: TSynMemo; const _Value: TGXUnicodeString);
{$IFDEF UNICODE}
begin
  _m.Lines.DelimitedText := _Value;
end;
{$ELSE}
var
  Lines: TGXUnicodeStrings;
  Line: string;
  s: string;
begin
  Lines := _m.Lines;
  if not TrySetStringProperty(Lines, 'DelimitedText', _Value) then begin
    Lines.Clear;
    s := _Value;
    while ExtractStr(s, ';', Line) do begin
      Lines.Add(Line);
    end;
  end;
end;
{$ENDIF}

{ TSynMemoUtil }

constructor TSynMemoUtil.Create(_Memo: TSynMemo);
begin
  inherited Create;
  FMemo := _Memo;
end;

procedure TSynMemoUtil.GetSelectedLines(out _StartIdx, _EndIdx: Integer);
begin
  _StartIdx := FMemo.BlockBegin.Line - 1;
  _EndIdx := FMemo.BlockEnd.Line - 1;
  if FMemo.BlockEnd.Char = 1 then
    Dec(_EndIdx);
  if _EndIdx < _StartIdx then
    _EndIdx := _StartIdx;
end;

class procedure TSynMemoUtil.ProcessAllLines(_Memo: TSynMemo);
var
  Processor: TSynMemoUtil;
begin
  Processor := Self.Create(_Memo);
  try
    Processor.ProcessAllLines;
  finally
    FreeAndNil(Processor);
  end;
end;

procedure TSynMemoUtil.ProcessAllLines;
var
  i: Integer;
begin
  FMemo.BeginUpdate;
  try
    for i := 0 to FMemo.Lines.Count - 1 do begin
      FMemo.Lines[i] := ProcessLine(FMemo.Lines[i]);
    end;
  finally
    FMemo.EndUpdate;
  end;

  TSynMemo_UpdateScrollbars(FMemo);
end;

class procedure TSynMemoUtil.ProcessSelectedLines(_Memo: TSynMemo);
var
  Processor: TSynMemoUtil;
begin
  Processor := Self.Create(_Memo);
  try
    Processor.ProcessSelectedLines;
  finally
    FreeAndNil(Processor);
  end;
end;

procedure TSynMemoUtil.ProcessSelectedLines;
var
  i: Integer;
  sl: TStringList;
  StartIdx: Integer;
  EndIdx: Integer;
  SelStart: Integer;
  Line: string;
  SelEnd: Integer;
begin
  if FMemo.Lines.Count = 0 then
    Exit; //==>

  GetSelectedLines(StartIdx, EndIdx);
  sl := TStringList.Create;
  try
    sl.Assign(FMemo.Lines);
    for i := StartIdx to EndIdx do begin
      sl[i] := ProcessLine(sl[i]);
    end;
    FMemo.Text := sl.Text;

    SelStart := FMemo.RowColToCharIndex(BufferCoord(1, StartIdx + 1));
    if EndIdx + 1 = FMemo.Lines.Count then begin
      Line := FMemo.Lines[EndIdx];
      SelEnd := FMemo.RowColToCharIndex(BufferCoord(Length(Line) + 1, EndIdx + 1));
    end else
      SelEnd := FMemo.RowColToCharIndex(BufferCoord(1, EndIdx + 2));
    FMemo.SelStart := SelStart;
    FMemo.SelEnd := SelEnd;
  finally
    FreeAndNil(sl);
  end;

  TSynMemo_UpdateScrollbars(FMemo);
end;

{ TSynMemoUtilMakePathsAbsoluteOrRelative }

constructor TSynMemoUtilMakePathsAbsoluteOrRelative.Create(_Memo: TSynMemo);
var
  ProjectFile: string;
begin
  inherited Create(_Memo);

  ProjectFile := Ota.GetCurrentProjectFileName(True);
  if ProjectFile <> '' then
    FProjectDir := ExtractFilePath(ProjectFile)
  else
    FProjectDir := '';
end;

{ TSynMemoUtilMakePathsAbsolute }

function TSynMemoUtilMakePathsAbsolute.ProcessLine(const _Line: string): string;
begin
  if (FProjectDir <> '') and (_Line <> '') and not StartsText('$(BDS)\', _Line) then begin
    Result := TFileSystem.ExpandFileNameRelBaseDir(_Line, FProjectDir);
  end else
    Result := _Line;
end;

{ TSynMemoUtilMakePathsRelative }

function TSynMemoUtilMakePathsRelative.ProcessLine(const _Line: string): string;
begin
  if (FProjectDir <> '') and (_Line <> '') and not StartsText('$(BDS)\', _Line) then begin
    Result := ExtractRelativePath(AddSlash(FProjectDir), _Line);
  end else
    Result := _Line;
end;

{ TSynMemoUtilPrependDots }

function TSynMemoUtilPrependDots.ProcessLine(const _Line: string): string;
begin
  if (_Line <> '') and not StartsText('$(BDS)\', _Line) then begin
    Result := '..\' + _Line;
  end;
end;

{ TSynMemoUtilRemoveDots }

function TSynMemoUtilRemoveDots.ProcessLine(const _Line: string): string;
begin
  if LeftStr(_Line, 3) = '..\' then begin
    Result := Copy(_Line, 4);
  end else
    Result := _Line;
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

