// defines TPascalToken and descendants which are the intermediate format after parsing
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterTokens;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  StrUtils,
  TypInfo,
  GX_CodeFormatterTypes;

type
  {: TPascalToken is never instantiated }
  TPascalToken = class
  public
    constructor Create;
    function GetWordType: TWordType; virtual; abstract;
    procedure GetLength(var ALength: Integer); virtual; abstract;
    function Space(ASpace: TSpace): Boolean; virtual;
    function GetReservedType: TReservedType; virtual; abstract;
    procedure SetReservedType(AReservedType: TReservedType); virtual;
    procedure SetSpace(ASpace: TSpaceSet; AState: Boolean); virtual;
    {: @returns the object's content }
    function GetString: string; virtual; abstract;
    (*: changes "// <comment>" to "{//<comment>}" used for preventing a
        begin moved from the next line to become commented out
        @param ACommChar is one of '{', '(' or '/' specifying the new comment type
        @returs true if token is a comment, false otherwise *)
    function ChangeComment(ACommChar: char): Boolean;
    function GetExpression(out AExpression: string): Boolean; virtual;
    procedure SetExpression(const AExpression: string); virtual;
    procedure SetCase(ACase: TCase); virtual;
    function GetCase: TCase; virtual;
    procedure SetOptions(AOptions: TTokenOptions); virtual;
    function GetOptions: TTokenOptions; virtual;
    procedure AddOption(AOption: TTokenOption);
    function HasOption(AOption: TTokenOption): Boolean;

    function GetForDebug: string; virtual;

    property ExpressionCase: TCase read GetCase write SetCase;
    property ReservedType: TReservedType read GetReservedType write SetReservedType;
    property WordType: TWordType read GetWordType;
    property Content: string read GetString;
    property Options: TTokenOptions read GetOptions write SetOptions;
  end;

  TLineFeed = class(TPascalToken)
  private
    FSpacePerIndent: Integer;
  public
    FNoOfSpaces: Integer;
    FOldNoOfSpaces: Integer;
    FWrapped: Boolean;
    constructor Create(AOldnSpaces: Integer; ASpacePerIndent: Integer);
    function GetWordType: TWordType; override;
    procedure SetIndent(Value: Integer);
    procedure IncIndent(Value: Integer);
    procedure GetLength(var ALength: Integer); override;
    function GetReservedType: TReservedType; override;

    function GetForDebug: string; override;

    {: @returns a string with FNoOfSpaces spaces }
    function GetString: string; override;
  end;

  TExpression = class(TPascalToken)
  private
  public
    FExpression: string;
    FWordType: TWordType;
    FSpaceType: TSpaceSet;
    FCaseType: TCase;
    FReservedType: TReservedType;
    FOptions: TTokenOptions;
    constructor Create(AType: TWordType; const AExpression: string);
    procedure CheckReserved;
    procedure SetSpace(ASpace: TSpaceSet; AState: Boolean); override;
    procedure SetReservedType(AReservedType: TReservedType); override;
    function Space(ASpace: TSpace): Boolean; override;
    {: @returns <spaces><the expression><spaces> }
    function GetString: string; override;
    procedure GetLength(var ALength: Integer); override;
    function GetWordType: TWordType; override;
    function GetReservedType: TReservedType; override;
    function GetExpression(out AExpression: string): Boolean; override;
    procedure SetExpression(const AExpression: string); override;
    procedure SetCase(ACase: TCase); override;
    function GetCase: TCase; override;
    procedure SetOptions(AOptions: TTokenOptions); override;
    function GetOptions: TTokenOptions; override;
    function GetForDebug: string; override;
  end;

  TAlignExpression = class(TExpression)
  public
    FAlignPos: Byte;
    FNoOfSpaces: Byte;
    constructor Create(ALike: TExpression; APos: Byte);
    // Note: As a side effect, this adjusts FNoOfspaces
    procedure GetLength(var ALength: Integer); override;
    {: @returns <spaces><the expression><spaces> }
    function GetString: string; override;
  end;

implementation

{$IFDEF GX_VER200_up} // delphi 2009
uses
//  GX_FastcodeAnsiString,
  AnsiStrings;
{$ENDIF}

{ TPascalToken }

constructor TPascalToken.Create;
begin
  inherited Create;
end;

function TPascalToken.GetExpression(out AExpression: string): Boolean;
begin
  Result := False;
end;

function TPascalToken.GetForDebug: string;
begin
  Result := ClassName;
end;

function TPascalToken.GetOptions: TTokenOptions;
begin
  Result := [];
end;

function TPascalToken.HasOption(AOption: TTokenOption): Boolean;
begin
  Result := AOption in Options;
end;

function TPascalToken.Space(ASpace: TSpace): Boolean;
begin
  Result := False;
end;

procedure TPascalToken.SetExpression(const AExpression: string);
begin
end;

procedure TPascalToken.SetOptions(AOptions: TTokenOptions);
begin
end;

procedure TPascalToken.SetCase(ACase: TCase);
begin
end;

function TPascalToken.GetCase: TCase;
begin
  Result := rfUnchanged;
end;

procedure TPascalToken.SetReservedType(AReservedType: TReservedType);
begin
end;

procedure TPascalToken.AddOption(AOption: TTokenOption);
begin
  Options := Options + [AOption];
end;

function TPascalToken.ChangeComment(ACommChar: char): Boolean;
var
  s: string;
begin
  Result := (Reservedtype = rtComment);
  if not Result then
    Exit;

  GetExpression(s);
  if Pos(ACommChar, s) = 0 then begin
    case ACommChar of
      '{':
        if Pos('}', s) = 0 then begin
          s := '{' + s + '}';
        end;
      '(':
        if Pos('*)', s) = 0 then begin
          s := '(*' + s + '*)';
        end;
      '/':
        if Pos('//', s) <> 1 then begin
          s := '//' + s;
        end;
    end;
    SetExpression(s);
  end;
end;

procedure TPascalToken.SetSpace(ASpace: TSpaceSet; AState: Boolean);
begin
  // do nothing
end;

{ TExpression }

constructor TExpression.Create(AType: TWordType; const AExpression: string);
begin
  inherited Create;
  FWordType := AType;
  FSpaceType := [];
  FCaseType := rfUnchanged;
  FReservedType := rtNothing;
  FExpression := AExpression;
  CheckReserved;
end;

procedure TExpression.CheckReserved;
var
  Expr: string;
  Directive: string;
  ResvdType: TReservedType;
begin
  SetReservedType(rtNothing);
  case WordType of
    wtCompDirective: begin
        GetExpression(Expr);
        if Copy(Expr, 1, 1) = '{' then
          Directive := Copy(Expr, 3, 999999)
        else if Copy(Expr, 1, 2) = '(*' then
          Directive := Copy(Expr, 4, 999999);
        Directive := LowerCase(Directive);

        if AnsiStartsStr('endif', Directive) then
          SetReservedType(rtCompEndif)
        else if AnsiStartsStr('else', Directive) then
          SetReservedType(rtCompElse)
        else if AnsiStartsStr('ifdef', Directive) then
          SetReservedType(rtCompIf)
        else if AnsiStartsStr('ifopt', Directive) then
          SetReservedType(rtCompIf)
        else if AnsiStartsStr('if ', Directive) then
          SetReservedType(rtCompIf)
        else if AnsiStartsStr('ifndef', Directive) then
          SetReservedType(rtCompIf);
      end;
    wtFullComment, wtFullOutComment, wtHalfStarComment,
      wtHalfOutComment, wtHalfComment:
      SetReservedType(rtComment);
    wtWord: begin
        GetExpression(Expr);
        Expr := LowerCase(Expr);
        if ReservedWordList.FindWord(Expr, ResvdType) then begin
          SetReservedType(ResvdType);
          Exit;
        end;
      end;
    wtOperator: begin
        if GetExpression(Expr) then begin
          if Length(Expr) = 1 then begin
            case Expr[1] of
              ':': SetReservedType(rtColon);
              '.': SetReservedType(rtDot);
              ';': SetReservedType(rtSemiColon);
              ',': SetReservedType(rtComma);
              ')': SetReservedType(rtRightBr);
              '(': SetReservedType(rtLeftBr);
              ']': SetReservedType(rtRightHook);
              '[': SetReservedType(rtLeftHook);
              '-': SetReservedType(rtMinus);
              '+': SetReservedType(rtPlus);
              '=': SetReservedType(rtEquals);
              '/', '*': SetReservedType(rtMathOper);
              '<', '>': SetReservedType(rtLogOper);
            end;
          end else if Length(Expr) = 2 then begin
            if Expr = '.)' then
              SetReservedType(rtRightHook)
            else if Expr = '(.' then
              SetReservedType(rtLeftHook)
            else if Expr = '..' then
              SetReservedType(rtDotDot)
            else if Expr = ':=' then
              SetReservedType(rtAssignOper)
            else if (Expr[1] = '<') or (Expr[1] = '>') then
            {if p in > < <> >=  <= =}
              SetReservedType(rtLogOper);
          end;
        end;
      end;
  end;
end;

procedure TExpression.SetExpression(const AExpression: string);
begin
  FExpression := AExpression
end;

procedure TExpression.SetOptions(AOptions: TTokenOptions);
begin
  FOptions := AOptions;
end;

function TExpression.GetWordType: TWordType;
begin
  Result := FWordType;
end;

function TExpression.GetExpression(out AExpression: string): Boolean;
begin
  AExpression := FExpression;
  Result := True;
end;

function TExpression.GetForDebug: string;

  function SpaceTypeToStr(_SpaceType: TSpaceSet): string;
  begin
    if (spBefore in _SpaceType) then
      Result := 'spBefore'
    else
      Result := '';
    if spAfter in _SpaceType then begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + 'spAfter';
    end;
  end;

var
  s: string;
begin
  s := 'Expression: ''' + FExpression + '''';
  s := s + ', WordType: ' + GetEnumName(TypeInfo(TWordType), Integer(FWordType));
  s := s + ', SpaceType: [' + SpaceTypeToStr(FSpaceType) + ']';
  s := s + ', CaseType: ' + GetEnumName(TypeInfo(TCase), Integer(FCaseType));
  s := s + ', ReservedType: ' + GetEnumName(TypeInfo(TReservedType), Integer(FReservedType));
  s := s + ', Options: [' + IfThen(FOptions = [], '', 'toFeedNewLine') + ']';
  Result := inherited GetForDebug + '(' + s + ')';
end;

function TExpression.Space(ASpace: TSpace): Boolean;
begin
  Result := (ASpace in FSpaceType);
end;

function TExpression.GetReservedType: TReservedType;
begin
  Result := FReservedType;
end;

procedure TExpression.SetSpace(ASpace: TSpaceSet; AState: Boolean);
begin
  if AState then
    FSpaceType := FSpaceType + ASpace
  else
    FSpaceType := FSpaceType - ASpace
end;

procedure TExpression.SetCase(ACase: TCase);
begin
  FCaseType := ACase;
end;

function TExpression.GetCase: TCase;
begin
  Result := FCaseType;
end;

procedure TExpression.SetReservedType(AReservedType: TReservedType);
begin
  FReservedType := AReservedType;
end;

function TExpression.GetString: string;
begin
  if Space(spBefore) then
    Result := ' '
  else
    Result := '';
  Result := Result + FExpression;
  Result := AdjustCase(Result, ExpressionCase);
  if Space(spAfter) then
    Result := Result + ' ';
end;

procedure TExpression.GetLength(var ALength: Integer);
begin
  ALength := ALength + Length(FExpression);
  if Space(spBefore) then
    inc(ALength);
  if Space(spAfter) then
    inc(ALength);
end;

function TExpression.GetOptions: TTokenOptions;
begin
  Result := FOptions;
end;

function strSpaceE(Dest: PAnsiChar; n: Integer): PAnsiChar;
var
  I: Integer;
begin
  Result := Dest;
  for I := 0 to n - 1 do begin
    Result^ := ' ';
    inc(Result);
  end;
  Result^ := #0;
end;

{ TLineFeed }

function TLineFeed.GetString: string;
var
  Len: Integer;
begin
  if FWrapped then
    Len := FNoOfSpaces + FSpacePerIndent
  else
    Len := FNoOfSpaces;
  if (Len > 0) then
    Result := StringOfChar(' ', Len)
  else
    Result := '';
end;

function TLineFeed.GetForDebug: string;
var
  s: string;
begin
  s := 'SpacePerIndent: ' + IntTostr(FSpacePerIndent);
  s := s + ', NoOfSpaces: ' + IntToStr(FNoOfSpaces);
  s := s + ', OldNoOfSpaces: ' + IntToStr(FOldNoOfSpaces);
  s := s + ', Wrapped: ' + IfThen(FWrapped, 'true', 'false');

  Result := inherited GetForDebug + '(' + s + ')';
end;

procedure TLineFeed.GetLength(var ALength: Integer);
begin
  if FNoOfSpaces > 0 then
    ALength := FNoOfSpaces
  else
    ALength := 0;
end;

function TLineFeed.GetWordType: TWordType;
begin
  Result := wtLineFeed;
end;

function TLineFeed.GetReservedType: TReservedType;
begin
  Result := rtLineFeed;
end;

constructor TLineFeed.Create(AOldnSpaces: Integer; ASpacePerIndent: Integer);
begin
  inherited Create;
  FOldNoOfSpaces := AOldnSpaces;
  FSpacePerIndent := ASpacePerIndent;
  FWrapped := False;
  FNoOfSpaces := AOldnSpaces; {default not changed indent}
end;

procedure TLineFeed.IncIndent(Value: Integer);
begin
  inc(FNoOfSpaces, Value * FSpacePerIndent);
end;

procedure TLineFeed.SetIndent(Value: Integer);
begin
  FNoOfSpaces := Value * FSpacePerIndent;
end;

{ TAlignExpression }

constructor TAlignExpression.Create(ALike: TExpression; APos: Byte);
var
  s: string;
begin
  ALike.GetExpression(s);
  inherited Create(ALike.FWordType, s);
  FAlignPos := APos;
  FNoOfSpaces := 0;
  FSpaceType := ALike.FSpaceType;
  FCaseType := ALike.FCaseType;
  FReservedType := ALike.FReservedType;
end;

procedure TAlignExpression.GetLength(var ALength: Integer);
begin
  if ALength < FAlignPos then begin
    FNoOfSpaces := FAlignPos - ALength;
    ALength := FAlignPos;
    SetSpace([spBefore], False);
  end else
    FNoOfSpaces := 0;
  inherited GetLength(ALength);
end;

function TAlignExpression.GetString: string;
begin
  if (FNoOfSpaces > 0) then
    Result := StringOfChar(' ', FNoOfSpaces)
  else
    Result := '';
  Result := Result + inherited GetString;
end;

end.

