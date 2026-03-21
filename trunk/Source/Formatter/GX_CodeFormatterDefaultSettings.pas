// Defines several default settings
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
// Contributors:        Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code.

unit GX_CodeFormatterDefaultSettings;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings;

function BorlandDefaults: TCodeFormatterEngineSettings;
function DelForExDefaults: TCodeFormatterEngineSettings;

implementation

function DelForExDefaults: TCodeFormatterEngineSettings;
begin
  Result.AlignCommentPos := 40;
  Result.AlignComments := False;
  Result.AlignVar := 0;
  Result.AlignVarPos := 20;
  Result.AlignAssign := False;
  Result.AlignAssignPos := 30;
  Result.BlankProc := True;
  Result.BlankSubProc := False;
  Result.BlankBeforeUses := False;
  Result.ChangeIndent := True;
  Result.FeedAfterThenExceptSingle := False;
  Result.FeedAfterSemiColon := False;
  Result.FeedAfterThen := False;
  Result.FeedAfterUses:= False;
  Result.FeedAfterVar := False;
  Result.FeedBeforeElse := False;
  Result.FeedBeforeEnd := False;
  Result.FeedEachUnit := False;
  Result.FeedEachUnitBeforeComma := False;
  Result.FeedAfterOpenParenEnum := False;
  Result.FeedEachEnum := False;
  Result.FeedEachEnumBeforeComma := False;
  Result.FeedElseIf := False;
  Result.FeedRoundBegin := Unchanged;
  Result.FeedRoundTry := Unchanged;
  Result.FillNewWords := [];
  Result.IdentifiersCase := rfUnchanged;
  Result.IndentBegin := False;
  Result.IndentCaseElse := False;
  Result.IndentComments := False;
  Result.IndentCompDirectives := icdNone;
  Result.NoIndentUsesComma := False;
  Result.UnindentLabels := False;
  Result.IndentInterfaceImpl := False;
  Result.DoubleIndentIfCondition := False;
  Result.IndentTry := True;
  Result.IndentTryElse := False;
  Result.NoFeedBeforeThen := False;
  Result.NoIndentElseIf := False;
  Result.NoIndentVarDecl := False;
  Result.NoIndentUsesComma := False;
  Result.RemoveDoubleBlank := False;
  Result.ReservedCase := rfLowerCase;
  Result.SpaceColon := [spAfter];
  Result.SpaceComma := [spAfter];
  Result.SpaceEqualOper := spBoth;
  Result.SpaceLeftBr := spNone;
  Result.SpaceLeftHook := spNone;
  Result.SpaceOperators := spBoth;
  Result.SpacePerIndent := 2;
  Result.SpaceRightBr := spNone;
  Result.SpaceRightHook := spNone;
  Result.SpaceSemiColon := [spAfter];
  Result.StandDirectivesCase := rfLowerCase;
  Result.UpperCompDirectives := True;
  Result.UpperNumbers := True;
  Result.WrapLines := False;
  Result.WrapPosition := 81;
  Result.EndCommentOut := '{*)}';
  Result.StartCommentOut := '{(*}';
end;

function BorlandDefaults: TCodeFormatterEngineSettings;
begin
  Result.AlignCommentPos := 40;
  Result.AlignComments := False;
  Result.AlignVar := 0;
  Result.AlignVarPos := 20;
  Result.AlignAssign := False;
  Result.AlignAssignPos := 30;
  Result.BlankProc := True;
  Result.BlankSubProc := False;
  Result.BlankBeforeUses := False;
  Result.ChangeIndent := True;
  Result.FeedAfterThenExceptSingle := False;
  Result.FeedAfterSemiColon := True;
  Result.FeedAfterThen := True;
  Result.FeedAfterUses:= False;
  Result.FeedAfterVar := True;
  Result.FeedBeforeElse := False;
  Result.FeedBeforeEnd := True;
  Result.FeedEachUnit := False;
  Result.FeedEachUnitBeforeComma := False;
  Result.FeedAfterOpenParenEnum := False;
  Result.FeedEachEnum := False;
  Result.FeedEachEnumBeforeComma := False;
  Result.FeedElseIf := False;
  Result.FeedRoundBegin := NewLine;
  Result.FeedRoundTry := NewLine;
  Result.FillNewWords := [];
  Result.IdentifiersCase := rfUnchanged;
  Result.IndentBegin := False;
  Result.IndentCaseElse := False;
  Result.IndentComments := True;
  Result.IndentCompDirectives := icdNone;
  Result.NoIndentUsesComma := False;
  Result.UnindentLabels := False;
  Result.IndentInterfaceImpl := False;
  Result.DoubleIndentIfCondition := False;
  Result.IndentTry := True;
  Result.IndentTryElse := False;
  Result.NoFeedBeforeThen := True;
  Result.NoIndentElseIf := False;
  Result.NoIndentVarDecl := False;
  Result.NoIndentUsesComma := False;
  Result.RemoveDoubleBlank := True;
  Result.ReservedCase := rfLowerCase;
  Result.SpaceColon := [spAfter];
  Result.SpaceComma := [spAfter];
  Result.SpaceEqualOper := spBoth;
  Result.SpaceLeftBr := spNone;
  Result.SpaceLeftHook := spNone;
  Result.SpaceOperators := spBoth;
  Result.SpacePerIndent := 2;
  Result.SpaceRightBr := spNone;
  Result.SpaceRightHook := spNone;
  Result.SpaceSemiColon := [spAfter];
  Result.StandDirectivesCase := rfLowerCase;
  Result.UpperCompDirectives := True;
  Result.UpperNumbers := True;
  Result.WrapLines := True;
  Result.WrapPosition := 81;
  Result.EndCommentOut := '{*)}';
  Result.StartCommentOut := '{(*}';
end;

end.

