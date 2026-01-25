# Formatter Changes and Features

This file documents recent fixes, new features, and implementation details for the GExperts code formatter. For general project information and workflows, see [CLAUDE.md](CLAUDE.md).

## Compiler Directive Indentation (IndentCompDirectives)

The `IndentCompDirectives` setting is a `TIndentCompDirectives` enum with three values:
- `icdNone` (=0) - Place directives at column 1 (default)
- `icdFollowCode` (=1) - Follow surrounding code indentation
- `icdIndependent` (=2) - Indent by directive nesting level, independent of code

**INI backward compatibility:** Old boolean values map correctly (0=icdNone, 1=icdFollowCode).

**Implementation notes** (in `GX_CodeFormatterFormatter.pas`):
- `FCompDirectiveNestLevel` tracks the current directive nesting depth
- Nesting level must be tracked for ALL directives, including inline ones like `{$ELSE}Windows, {$ENDIF}` where `{$ENDIF}` is not at the start of a line
- `FStack.nIndent` is in indent **levels** (0, 1, 2...), not spaces. The `SetIndent` method multiplies by `SpacePerIndent`
- For `rtCompEndif` indentation, use `FCompDirectiveNestLevel - 1` to match the opening `{$IFDEF}`

## Double Indent If Conditions (DoubleIndentIfCondition)

The `DoubleIndentIfCondition` setting aligns multi-line if conditions by using double indentation for continuation lines. When enabled:

- Adds an extra space after the `if` keyword (2 spaces total instead of 1)
- Uses double indent (2 levels) for continuation lines in if conditions

**Example with `DoubleIndentIfCondition := True`:**
```pascal
procedure Example;
begin
  if  Condition1
      and Condition2
      or Condition3 then begin
    doSomething;
  end;
end;
```

Both `Condition1` and `and Condition2` align at the same column (column 7 in this example):
- `if` at column 3 (2 spaces base indent) + 2 spaces after = `Condition1` at column 7
- Continuation line: 2 spaces base + 4 spaces (2 indent levels) = `and` at column 7

**Note:** This feature only applies to `if` statements, not `while` or `repeat/until`, because the longer keywords (`while` = 5 chars, `until` = 5 chars vs `if` = 2 chars) would not align properly with double indentation.

**Implementation notes** (in `GX_CodeFormatterFormatter.pas`):
- In `AdjustSpacing`: When `rtIf` is encountered and setting is enabled, the expression is changed from "if" to "if " (with trailing space)
- In `CheckIndent`: When `FWrapIndent` is true and we're in an if condition (`rtIf` or `rtIfElse` on stack), `_NTmp` is set to 2 instead of 1

**Test file:** `testfile_DoubleIndentIfCondition.pas`

## Delphi 13 Ternary Operator Support

Delphi 13 (RAD Studio 13 Florence) introduced inline if expressions, also called the "ternary operator" or "conditional operator". This feature allows `if-then-else` to be used as an expression that returns a value.

**Syntax**: `if <condition> then <trueValue> else <falseValue>`

**Key characteristics**:
- Short-circuit evaluation (only evaluates the selected branch)
- The `else` part is mandatory (cannot be omitted)
- Can be used anywhere an expression is valid
- Can be nested (parentheses recommended for clarity)
- Can create compile-time constants when all parts are constants

**Formatter implementation**: The formatter keeps inline if expressions on one line with proper spacing:

```pascal
// Simple ternary
X := if Y > 0 then 10 else 20;

// Nested ternary
X := if Y > 0 then (if Y > 10 then 100 else 50) else 0;

// In constants
const MAX_SIZE = if True then 100 else 50;

// In case statement
case if Y > 0 then Y else -Y of
  1: X := 1;
end;

// In for loop
for X := 1 to if Y > 0 then Y else 10 do
  WriteLn(X);
```

**Test cases**:
- `testfile_TernaryOperator.pas` - Comprehensive working test cases (single-line formatting)
- `testfile_TernaryContexts.pas` - Tests inline if in various expression contexts
- `testfile_TernaryDeepNested.pas` - Tests deeply nested inline if expressions
- `testfile_TernaryNotIf.pas` - Tests inline if preceded by `not` operator
- `testfile_TernaryInCondition.pas` - Tests inline if within statement if conditions
- `testfile_TernaryAsBoolCondition.pas` - Tests inline if as boolean condition of statement if

**Implementation details** in `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`:
- `FInlineIfDepth` field tracks ternary operator nesting depth
- `IsInlineIfExpression()` detects if expression context based on previous token
- `CheckIndentIf`, `CheckIndentThen`, `CheckIndentElse` skip line-breaking for ternary operators
- Special handling in `rtOf` and `rtDo` to clean up stack markers for proper indentation

**Known bugs** (documented in `failure_*.pas` test files):

None currently. All ternary operator formatting issues have been fixed.

**Future enhancement**: Add configuration option for vertical alignment style (Phase 2).

**References**:
- [Embarcadero Blog: Coming in RAD Studio 13](https://blogs.embarcadero.com/coming-in-rad-studio-13-a-conditional-ternary-operator-for-the-delphi-language/)
- [Intentional Delphi: The Conditional Ternary Operator](https://wmeyer.tech/2025/10/20/the-conditional-ternary-operator/)
- [Delphi-PRAXiS Discussion](https://en.delphipraxis.net/topic/13907-a-conditional-ternary-operator-for-the-delphi/)

## Recent Fixes

Fixed in `Source/Formatter/Engine/GX_CodeFormatterFormatter.pas`:

1. **Continuation indent after single-line comments** - Continuation indentation is now preserved through sequences of `//` comments in expressions. Previously, `FWrapIndent` was reset for all comments, breaking continuation. The fix only resets `FWrapIndent` for block comments (`{ }`, `(* *)`) or when in case statements (`rtOf` context).

2. **Nested procedure indentation** - Nested procedures inside anonymous procedures now maintain proper indentation instead of resetting to column 1.

3. **Blank line after anonymous procedure with parameters** - Added `AnonymousProcHadParams` function to detect when an anonymous procedure has parameters and insert a blank line before nested content.

4. **Independent compiler directive indentation** - Added `icdIndependent` mode that indents `{$IFDEF}`, `{$IF}`, `{$ELSE}`, `{$ENDIF}`, `{$IFEND}` based on their own nesting level, independent of surrounding code. Inline directives (not at start of line) are tracked for nesting but not indented.

5. **Blank line before uses clauses** - Added `BlankBeforeUses` setting to insert a blank line before `uses` clauses. When comments immediately precede a uses clause, the blank line is inserted before the comments to keep them visually grouped with the uses. Enabled by default in `twm` and `special` configurations.

6. **Nested procedure indentation with BlankSubProc** - Fixed bug where nested procedures were not indented when there was no blank line above them in the input. The formatter now sets indentation before inserting blank lines (previously the blank line insertion changed `FPrevLine`, causing indentation to be applied to the wrong line).

7. **Inline if after `not` operator** - The `not` operator before inline if expressions (e.g., `B := not if Y > 0 then True else False`) is now correctly recognized as an expression context. The fix adds `not` to the list of operators checked in `IsInlineIfExpression`.

8. **Inline if `else` alignment** - Fixed misalignment where `else` had one extra space compared to `then` in vertical inline if formatting. The bug was caused by duplicate handling of `else` in both the `rtLineFeed` case and `CheckIndentElse`. The `rtLineFeed` case did an early `Exit` that skipped updating `FPrevToken`, so when `AdjustSpacing` ran for `else`, it didn't see the line feed as the previous token and didn't clear `spBefore`. The fix removes the redundant `else` handling from the `rtLineFeed` case.

9. **Inline if as statement condition** - When an inline if is used as the boolean condition of a statement if (e.g., `if if A > 0 then True else False then`), the inner inline if is now correctly recognized as an expression. The fix adds `rtIf` to the list of tokens in `IsInlineIfExpression()` that indicate an expression context, so `if` followed by `if` treats the second `if` as starting an inline if expression.
