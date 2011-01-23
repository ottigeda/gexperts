// Handles the settings of the Code Formatter
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterSettings;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  GX_CodeFormatterTypes,
  GX_CodeFormatterTokens;

type
  TFeedBegin = (Unchanged, Hanging, NewLine);

  {: how to use the captialization file }
//  TFillMode = (
//    fmUnchanged, {: do not use the capitalization file }
//    fmAddNewWord, {: add new words, but do not use for capitalization }
//    fmUse, {: use for capitalization, but do not add words }
//    fmExceptDirect, {: exclude directives }
//    fmAddUse, {: add new words, and use for capitalization }
//    fmAddUseExcept); {: add new words, but do not use for capitalization, exclude directivs }
  // this could probably be replaced by:
  // TFillModeOptions = (fmAddNew, fmUse, fmExceptDirectives }
  // TFillMode = set of TFillMode
  // which would make it more readable

  TCapfileMode = (cmAddNew, cmUse, cmExceptDirectives);
  TCapfileModeSet = set of TCapfileMode;

type
  {: stores all possible settings for the formatting engine }
  TCodeFormatterEngineSettings = record
    SpaceOperators: TSpaceSet; {: spaces around operators }
    SpaceColon: TSpaceSet; {: spaces around colons ":" }
    SpaceSemiColon: TSpaceSet; {: spaces around semicolons ";" }
    SpaceComma: TSpaceSet; {: spaces around commas "," }
    SpaceLeftBr: TSpaceSet; {: spaces around left brackets "(" }
    SpaceRightBr: TSpaceSet; {: spaces around right brackets ")" }
    SpaceLeftHook: TSpaceSet; {: spaces around left square bracket "[" }
    SpaceRightHook: TSpaceSet; {: spaces around right square bracket "]" }
    SpaceEqualOper: TSpaceSet; {: spaces around equal operator "=" }
    UpperCompDirectives: Boolean; {: uppercase compiler directives }
    UpperNumbers: Boolean; {: uppercase (hex) numbers }
    ReservedCase: TCase; {: case for reserved words }
    StandDirectivesCase: TCase; {: case for standard directives }
    ChangeIndent: Boolean;
    NoIndentElseIf: Boolean;
    IndentBegin: Boolean;
    IndentTry: Boolean;
    IndentTryElse: Boolean;
    IndentCaseElse: Boolean;
    IndentComments: Boolean;
    IndentCompDirectives: Boolean;
    BlankProc: Boolean; {: blank line between main procedures }
    BlankSubProc: Boolean; {: blank line between sub procedures }
    RemoveDoubleBlank: Boolean; {: remove double blank lines }
    SpacePerIndent: Integer; {: number of spaces per indent }
    FeedRoundBegin: TFeedBegin; {: how to handle linefeeds around begin }
    FeedRoundTry: TFeedBegin; {: how to handle linefeeds around try }
    FeedBeforeEnd: Boolean;
    FeedAfterThen: Boolean;
    ExceptSingle: Boolean;
    FeedAfterVar: Boolean;
    FeedEachUnit: Boolean;
    NoFeedBeforeThen: Boolean;
    FeedElseIf: Boolean; {: line feed between else and if }
    FillNewWords: TCapfileModeSet; {: how to use the capitalization file }
    FeedAfterSemiColon: Boolean;
    StartCommentOut: AnsiString; {: special comment to start unformatted section }
    EndCommentOut: AnsiString; {: special comment to end unformatted section }
    CommentFunction: Boolean; {: add a function comment }
    CommentUnit: Boolean; {: add a unit comment }
    WrapLines: Boolean; {: wrap long lines }
    WrapPosition: Byte; {: wrap position for long lines }
    AlignCommentPos: Byte; {: position to align comments }
    AlignComments: Boolean; {: turn on comment alignment }
    AlignVarPos: Byte; {: position to align variant/constant declarations (the colon) }
    AlignVar: Boolean; {: turn on variable/constant alignment }
  end;

type
  TConfigPrecedenceEnum = (cpDirective, cpIniFile, cpMyConfig);
type
  TConfigPrecedenceArr = array[1..3] of TConfigPrecedenceEnum;
  TOneToThree = 1..3;

function IntToConfigPrecedence(_Value: integer): TConfigPrecedenceEnum;

type
  TCodeFormatterSettings = class
  private
    FSettings: TCodeFormatterEngineSettings;
    FCapNames: TStringList;
    FCapFile: AnsiString;
    FUseCapFile: Boolean;
    FShowDoneDialog: Boolean;
    FConfigPrecedence: TConfigPrecedenceArr;
    function GetConfigPrecedence(_Idx: TOneToThree): TConfigPrecedenceEnum;
    procedure SetConfigPrecedence(_Idx: TOneToThree; const _Value: TConfigPrecedenceEnum);
  public
    constructor Create;
    destructor Destroy; override;

//    procedure LoadCapFile(const ACapFile: AnsiString);
//    procedure SaveCapFile(const ACapFile: AnsiString);

    procedure HandleCapitalization(AWord: TPascalToken);

    property Settings: TCodeFormatterEngineSettings read FSettings write FSettings;
    property CapNames: TStringList read FCapNames;
    property ConfigPrecedence[_Idx: TOneToThree]: TConfigPrecedenceEnum read GetConfigPrecedence write SetConfigPrecedence;

    property SpaceOperators: TSpaceSet read FSettings.SpaceOperators;
    property SpaceColon: TSpaceSet read FSettings.SpaceColon;
    property SpaceSemiColon: TSpaceSet read FSettings.SpaceSemiColon;
    property SpaceComma: TSpaceSet read FSettings.SpaceComma;
    property SpaceLeftBr: TSpaceSet read FSettings.SpaceLeftBr;
    property SpaceRightBr: TSpaceSet read FSettings.SpaceRightBr;
    property SpaceLeftHook: TSpaceSet read FSettings.SpaceLeftHook;
    property SpaceRightHook: TSpaceSet read FSettings.SpaceRightHook;
    property SpaceEqualOper: TSpaceSet read FSettings.SpaceEqualOper;
    property UpperCompDirectives: Boolean read FSettings.UpperCompDirectives;
    property UpperNumbers: Boolean read FSettings.UpperNumbers;
    property ReservedCase: TCase read FSettings.ReservedCase;
    property StandDirectivesCase: TCase read FSettings.StandDirectivesCase;
    property ChangeIndent: Boolean read FSettings.ChangeIndent;
    property NoIndentElseIf: Boolean read FSettings.NoIndentElseIf;
    property IndentBegin: Boolean read FSettings.IndentBegin;
    property IndentTry: Boolean read FSettings.IndentTry;
    property IndentTryElse: Boolean read FSettings.IndentTryElse;
    property IndentCaseElse: Boolean read FSettings.IndentCaseElse;
    property IndentComments: Boolean read FSettings.IndentComments;
    property IndentCompDirectives: Boolean read FSettings.IndentCompDirectives;
    property BlankProc: Boolean read FSettings.BlankProc;
    property BlankSubProc: Boolean read FSettings.BlankSubProc;
    property RemoveDoubleBlank: Boolean read FSettings.RemoveDoubleBlank;
    property SpacePerIndent: Integer read FSettings.SpacePerIndent;
    property FeedRoundBegin: TFeedBegin read FSettings.FeedRoundBegin;
    property FeedRoundTry: TFeedBegin read FSettings.FeedRoundTry;
    property FeedBeforeEnd: Boolean read FSettings.FeedBeforeEnd;
    property FeedAfterThen: Boolean read FSettings.FeedAfterThen;
    property ExceptSingle: Boolean read FSettings.ExceptSingle;
    property FeedAfterVar: Boolean read FSettings.FeedAfterVar;
    property FeedEachUnit: Boolean read FSettings.FeedEachUnit;
    property NoFeedBeforeThen: Boolean read FSettings.NoFeedBeforeThen;
    property FeedElseIf: Boolean read FSettings.FeedElseIf;
    property FillNewWords: TCapfileModeSet read FSettings.FillNewWords;
    property FeedAfterSemiColon: Boolean read FSettings.FeedAfterSemiColon;
    property StartCommentOut: AnsiString read FSettings.StartCommentOut;
    property EndCommentOut: AnsiString read FSettings.EndCommentOut;
    property CommentFunction: Boolean read FSettings.CommentFunction;
    property CommentUnit: Boolean read FSettings.CommentUnit;
    property WrapLines: Boolean read FSettings.WrapLines;
    property WrapPosition: Byte read FSettings.WrapPosition;
    property AlignCommentPos: Byte read FSettings.AlignCommentPos;
    property AlignComments: Boolean read FSettings.AlignComments;
    property AlignVarPos: Byte read FSettings.AlignVarPos;
    property AlignVar: Boolean read FSettings.AlignVar;
    // settings for the wizard
    property ShowDoneDialog: Boolean read FShowDoneDialog write FShowDoneDialog;
    property CapitalizationFile: AnsiString read FCapFile write FCapFile;
    property UseCapitalizationFile: Boolean read FUseCapFile write FUseCapFile;
  end;

function IntToCapfileMode(AValue: Integer): TCapfileModeSet;
function CapfileModeToInt(AMode: TCapfileModeSet): Integer;

implementation

uses
  SysUtils,
  GX_CodeFormatterDefaultSettings;

{ TCodeFormatterSettings }

constructor TCodeFormatterSettings.Create;
begin
  inherited;
  FCapNames := TStringList.Create;
  FCapNames.Sorted := True;
  FCapNames.CaseSensitive := False;

  FShowDoneDialog := True;
  FSettings := BorlandDefaults;
  FConfigPrecedence[1] := cpDirective;
  FConfigPrecedence[2] := cpIniFile;
  FConfigPrecedence[3] := cpMyConfig;
end;

destructor TCodeFormatterSettings.Destroy;
begin
  FCapNames.Free;
  inherited;
end;

function TCodeFormatterSettings.GetConfigPrecedence(_Idx: TOneToThree): TConfigPrecedenceEnum;
begin
  Result := FConfigPrecedence[_Idx];
end;

procedure TCodeFormatterSettings.HandleCapitalization(AWord: TPascalToken);
var
  Expression: string;
  Found: Boolean;
  Idx: Integer;
  CommentedIdx: Integer;
begin
  if not (AWord.GetExpression(Expression)) then
    Exit;

  Found := False;
  if (cmAddNew in Settings.FillNewWords)
    and (AWord.ReservedType in (NoReservedTypes - StandardDirectives)) then begin
    // Add new words to CapNames if configured and they aren't there yet
    Found := CapNames.Find(String(Expression), Idx);
    if not Found then begin
      if not CapNames.Find('*' + String(Expression), CommentedIdx) then
        CapNames.Add(String(Expression));
    end;
  end;

  // Use CapNames to set capitalization
  if (cmUse in Settings.FillNewWords)
    or (
    (cmExceptDirectives in Settings.FillNewWords)
    and not (AWord.ReservedType in StandardDirectives)) then begin
    if not Found then
      Found := CapNames.Find(String(Expression), Idx);
    if Found then begin
      AWord.SetExpression(CapNames[Idx]);
      AWord.ExpressionCase := rfUnchanged;
    end;
  end;
end;

procedure TCodeFormatterSettings.SetConfigPrecedence(_Idx: TOneToThree;
  const _Value: TConfigPrecedenceEnum);
begin
  FConfigPrecedence[_Idx] := _Value;
end;

function IntToCapfileMode(AValue: Integer): TCapfileModeSet;
begin
  case AValue of
    1: Result := [cmAddNew];
    2: Result := [cmUse];
    3: Result := [cmUse, cmExceptDirectives];
    4: Result := [cmAddNew, cmUse];
    5: Result := [cmAddNew, cmUse, cmExceptDirectives];
  else
    Result := []; // invalid
  end;
end;

function CapfileModeToInt(AMode: TCapfileModeSet): Integer;
begin
  if AMode = [cmAddNew] then
    Result := 1
  else if AMode = [cmUse] then
    Result := 2
  else if AMode = [cmUse, cmExceptDirectives] then
    Result := 3
  else if AMode = [cmAddNew, cmUse] then
    Result := 4
  else if AMode = [cmAddNew, cmUse, cmExceptDirectives] then
    Result := 5
  else
    Result := 0;
end;

function IntToConfigPrecedence(_Value: integer): TConfigPrecedenceEnum;
begin
  if (_Value < Ord(Low(TConfigPrecedenceEnum))) or (_Value > Ord(High(TConfigPrecedenceEnum))) then
    Result := cpDirective
  else
    Result := TConfigPrecedenceEnum(_Value);
end;

end.

