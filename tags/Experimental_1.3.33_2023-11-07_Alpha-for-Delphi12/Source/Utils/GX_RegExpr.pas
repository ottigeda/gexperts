///<summary>
/// Declares an alternative class TRegExpr which uses the built in regular expression support
/// introduced around Delphi XE. Unfortunately the Replace method doesn't work as I expected
/// so this code is currently disabled by the {$UNDEF} below. </summary>
unit GX_RegExpr;

{$I GX_CondDefine.inc}

{$UNDEF GX_HAS_UNIT_REGULAREXPRESSIONS}

interface

uses
  SysUtils,
{$IFDEF GX_HAS_UNIT_REGULAREXPRESSIONS}
  RegularExpressionsAPI,
  RegularExpressionsCore,
  RegularExpressions,
{$ELSE}
  SynRegExpr,
{$ENDIF}
  Classes;

{$IFDEF GX_HAS_UNIT_REGULAREXPRESSIONS}

type
  ERegExpr = class(Exception)
  private
    FCompilerErrorPos: Integer;
  public
    property CompilerErrorPos: Integer read FCompilerErrorPos;
  end;

type
  TRegExpr = class
  private
    FRegEx: TRegEx;
    FCurrentMatch: TMatch;
    FModifierI: Boolean;
    FExpression: string;
    FModifierG: Boolean;
    function GetMatchPos(_Idx: Integer): Integer;
    function GetMatchLen(_Idx: Integer): Integer;
  public
    constructor Create;
    procedure Compile;
    function Exec(const _Input: string): Boolean;
    function ExecNext: Boolean;
    function Replace(const _Input: string; _ReplaceStr: string; _UseSubstitution: Boolean): string;
    property MatchPos[_Idx: Integer]: Integer read GetMatchPos;
    property MatchLen[_Idx: Integer]: Integer read GetMatchLen;
    property ModifierI: Boolean read FModifierI write FModifierI;
    property ModifierG: Boolean read FModifierG write FModifierG;
    property Expression: string read FExpression write FExpression;
  end;
{$ELSE}
type
  TRegExpr = SynRegExpr.TRegExpr;
  ERegExpr = SynRegExpr.ERegExpr;
{$ENDIF}

function QuoteRegExprMetaChars(const _Str: string): string;
function ExecRegExpr(const _RegExpr, _InputStr: string): Boolean;

implementation
{$IFDEF GX_HAS_UNIT_REGULAREXPRESSIONS}

type
  TPerlRegExHelper = class helper for TPerlRegEx
    procedure SetAdditionalPCREOptions(PCREOptions: Integer);
  end;

procedure TPerlRegExHelper.SetAdditionalPCREOptions(PCREOptions: Integer);
begin
  with Self do
    FPCREOptions := FPCREOptions or PCREOptions;
end;

type
  TRegExHelper = record helper for TRegEx
  public
    procedure SetAdditionalPCREOptions(PCREOptions: Integer);
  end;

procedure TRegExHelper.SetAdditionalPCREOptions(PCREOptions: Integer);
begin
  with Self do
    FRegEx.SetAdditionalPCREOptions(PCREOptions);
end;

function QuoteRegExprMetaChars(const _Str: string): string;
begin
  Result := TRegEx.Escape(_Str);
end;

function ExecRegExpr(const _RegExpr, _InputStr: string): Boolean;
var
  re: TRegExpr;
begin
  re := TRegExpr.Create;
  re.Expression := _RegExpr;
  Result := re.Exec(_InputStr);
end;

{ TRegExpr }

procedure TRegExpr.Compile;
var
  Options: TRegExOptions;
begin
  Options := [roCompiled];
  if FModifierI then
    Options := Options + [roIgnoreCase];
  try
    FRegEx := TRegEx.Create(FExpression, Options);
  except
    on e: Exception do
      raise ERegExpr.Create(e.Message);
  end;
end;

constructor TRegExpr.Create;
begin
  inherited;
end;

function TRegExpr.Exec(const _Input: string): Boolean;
begin
  Compile;
  if FModifierG then
    FRegEx.SetAdditionalPCREOptions(PCRE_UNGREEDY);
  FCurrentMatch := FRegEx.Match(_Input);
  Result := FCurrentMatch.Success
end;

function TRegExpr.ExecNext: Boolean;
begin
  FCurrentMatch := FCurrentMatch.NextMatch;
  Result := FCurrentMatch.Success
end;

function TRegExpr.GetMatchLen(_Idx: Integer): Integer;
begin
  if _Idx <> 0 then
    raise ERegExpr.Create('TRegExpr.GetMatchLen only supports Idx = 0');
  Result := FCurrentMatch.Length;
end;

function TRegExpr.GetMatchPos(_Idx: Integer): Integer;
begin
  if _Idx <> 0 then
    raise ERegExpr.Create('TRegExpr.GetMatchPos only supports Idx = 0');
  Result := FCurrentMatch.Index;
end;

function TRegExpr.Replace(const _Input: string; _ReplaceStr: string; _UseSubstitution: Boolean): string;
begin
  raise ERegExpr.Create('TRegExpr.Replace does not work yet');
  FRegEx.Replace(_Input, FExpression, _ReplaceStr)
end;

{$ELSE}
function QuoteRegExprMetaChars(const _Str: string): string;
begin
  Result := SynRegExpr.QuoteRegExprMetaChars(_Str);
end;

function ExecRegExpr(const _RegExpr, _InputStr: string): Boolean;
begin
  Result := SynRegExpr.ExecRegExpr(_RegExpr, _InputStr);
end;
{$ENDIF}

end.

