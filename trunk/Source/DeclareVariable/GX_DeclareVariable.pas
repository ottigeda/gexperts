/// <summary>
/// Core logic for the Declare Variable editor expert.
/// This unit has no OTA dependency and operates on plain strings,
/// making it fully testable with DUnit. </summary>
unit GX_DeclareVariable;

{$I GX_CondDefine.inc}

interface

uses
  GX_PascalTokenAnalyzer,
  mPasLex, mwPasParserTypes;

type
  TExternalLookupKind = (elkFuncReturn, elkParamType, elkIdentType);

  /// <summary>
  /// Callback for resolving types from external units (UCM, intrinsics, etc.).
  /// @param Name the identifier to look up
  /// @param Kind indicates what type of lookup
  /// @param ParamIndex 0-based parameter position (only for elkParamType)
  /// @returns the resolved type name, or '' if not found </summary>
  TExternalTypeLookup = function(const Name: string;
    Kind: TExternalLookupKind; ParamIndex: Integer): string of object;

  TDeclKind = (dkLocalVar, dkField, dkProperty);

  TInsertionPoint = record
    Description: string;        // "var section in DoSomething", "private of TMyClass"
    Kind: TDeclKind;
    CharPos: Integer;           // 1-based position in source where to insert
    Indent: string;             // indentation prefix for the declaration
    NeedsVarKeyword: Boolean;   // True if 'var' keyword must be added first
    SectionHeader: string;      // e.g. '  private' — inserted as new line before declaration
  end;
  TInsertionPointArray = array of TInsertionPoint;

  TTypeAlternatives = array of string;

  TDeclareInfo = record
    IdentName: string;
    InferredType: string;       // guessed from context, empty if unknown
    InferenceDesc: string;      // "from := 42", "from parameter of WriteLn"
    ExistingDecl: string;       // non-empty if already declared
    IsReservedWord: Boolean;
    InsertionPoints: TInsertionPointArray;
    AlternativeTypes: TTypeAlternatives;  // alternative type suggestions for overloaded functions
    DefaultKindIndex: Integer;  // index into InsertionPoints for default
    ErrorMsg: string;
  end;

function IsReservedWord(const Name: string): Boolean;

function AnalyzeForDeclaration(const Source: string; CursorPos: Integer;
  out Info: TDeclareInfo; ExternalLookup: TExternalTypeLookup = nil): Boolean;

function InferTypeFromContext(const Tokens: TTokenArray; IdentTokenIdx: Integer;
  const IdentName: string; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray;
  ExternalLookup: TExternalTypeLookup = nil): string; overload;

function InferTypeFromContext(const Tokens: TTokenArray; IdentTokenIdx: Integer;
  const IdentName: string; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray;
  ExternalLookup: TExternalTypeLookup;
  out AlternativeTypes: TTypeAlternatives): string; overload;

function FindInsertionPoints(const Source: string; const Tokens: TTokenArray;
  const Procs: TProcScopeArray; const Classes: TClassScopeArray;
  CursorTokenIdx: Integer; const IdentName: string): TInsertionPointArray;

function BuildDeclarationText(const IdentName, TypeName: string;
  Kind: TDeclKind; const Indent: string;
  const PropertyRead, PropertyWrite: string): string;

function InsertDeclaration(const Source: string;
  const InsertionPt: TInsertionPoint;
  const DeclText: string): string;

implementation

uses
  SysUtils;

function IsReservedWord(const Name: string): Boolean;
var
  Lexer: TmwPasLex;
begin
  if Name = '' then begin
    Result := True;
    Exit;
  end;
  Lexer := TmwPasLex.Create;
  try
    Lexer.Origin := PChar(Name + ';');
    Result := not (Lexer.TokenID in IdentDirect) and (Lexer.TokenID <> tkIdentifier);
  finally
    Lexer.Free;
  end;
end;

/// <summary>
/// Finds the line start position (1-based) for a given 1-based position in source. </summary>
function FindLineStart(const Source: string; Pos: Integer): Integer;
begin
  Result := Pos;
  while (Result > 1) and (Source[Result - 1] <> #10) do
    Dec(Result);
end;

/// <summary>
/// Extracts whitespace indentation at line containing Pos (1-based). </summary>
function GetIndentAtPos(const Source: string; Pos: Integer): string;
var
  LineStart, I: Integer;
begin
  LineStart := FindLineStart(Source, Pos);
  I := LineStart;
  while (I <= Length(Source)) and ((Source[I] = ' ') or (Source[I] = #9)) do
    Inc(I);
  Result := Copy(Source, LineStart, I - LineStart);
end;

/// <summary>
/// Finds position after end of line at given 1-based position. </summary>
function FindEndOfLine(const Source: string; Pos: Integer): Integer;
begin
  Result := Pos;
  while (Result <= Length(Source)) and (Source[Result] <> #13) and (Source[Result] <> #10) do
    Inc(Result);
  // Skip CRLF
  if (Result <= Length(Source)) and (Source[Result] = #13) then
    Inc(Result);
  if (Result <= Length(Source)) and (Source[Result] = #10) then
    Inc(Result);
end;

/// <summary>
/// Checks whether a numeric expression starting at RhsIdx contains a '/'
/// operator or a float literal, which would make the result floating-point.
/// In Delphi, '/' always returns Extended (floating-point), while 'div' is
/// integer division. Scans until semicolon/end-of-tokens. </summary>
function ExpressionIsFloat(const Tokens: TTokenArray; RhsIdx: Integer): Boolean;
var
  J, ParenLevel: Integer;
begin
  Result := False;
  ParenLevel := 0;
  J := RhsIdx;
  while J <= High(Tokens) do begin
    case Tokens[J].Kind of
      tkRoundOpen: Inc(ParenLevel);
      tkRoundClose: begin
        Dec(ParenLevel);
        if ParenLevel < 0 then
          Break;
      end;
      tkSemiColon: begin
        if ParenLevel <= 0 then
          Break;
      end;
      tkSlash: begin
        // '/' operator → float result
        Result := True;
        Exit;
      end;
      tkFloat: begin
        Result := True;
        Exit;
      end;
      tkInteger, tkNumber: begin
        if (Pos('.', Tokens[J].Text) > 0) or
           (Pos('e', LowerCase(Tokens[J].Text)) > 0) then begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Inc(J);
  end;
end;

/// <summary>
/// Looks up the return type of a compiler intrinsic function.
/// @param FuncName the function name to look up
/// @returns the return type, or '' if not a known intrinsic </summary>
function LookupIntrinsicReturnType(const FuncName: string): string;
begin
  Result := '';
  if SameText(FuncName, 'Length') then Result := 'Integer'
  else if SameText(FuncName, 'High') then Result := 'Integer'
  else if SameText(FuncName, 'Low') then Result := 'Integer'
  else if SameText(FuncName, 'Ord') then Result := 'Integer'
  else if SameText(FuncName, 'Chr') then Result := 'Char'
  else if SameText(FuncName, 'SizeOf') then Result := 'Integer'
  else if SameText(FuncName, 'Assigned') then Result := 'Boolean'
  else if SameText(FuncName, 'Copy') then Result := 'string'
  else if SameText(FuncName, 'Pos') then Result := 'Integer'
  else if SameText(FuncName, 'Round') then Result := 'Int64'
  else if SameText(FuncName, 'Trunc') then Result := 'Int64'
  // Pred, Succ, Abs, Sqr omitted — return type depends on argument type
  else if SameText(FuncName, 'Odd') then Result := 'Boolean'
  else if SameText(FuncName, 'Swap') then Result := 'Integer'
  else if SameText(FuncName, 'Lo') then Result := 'Byte'
  else if SameText(FuncName, 'Hi') then Result := 'Byte'
  else if SameText(FuncName, 'StringOfChar') then Result := 'string'
  else if SameText(FuncName, 'Concat') then Result := 'string';
end;

function IsIntegerType(const TypeName: string): Boolean;
begin
  Result := SameText(TypeName, 'Integer') or SameText(TypeName, 'SmallInt') or
    SameText(TypeName, 'ShortInt') or SameText(TypeName, 'Word') or
    SameText(TypeName, 'Byte') or SameText(TypeName, 'Cardinal') or
    SameText(TypeName, 'LongInt') or SameText(TypeName, 'LongWord') or
    SameText(TypeName, 'NativeInt') or SameText(TypeName, 'NativeUInt');
end;

function IsInt64Type(const TypeName: string): Boolean;
begin
  Result := SameText(TypeName, 'Int64') or SameText(TypeName, 'UInt64');
end;

function IsFloatType(const TypeName: string): Boolean;
begin
  Result := SameText(TypeName, 'Extended') or SameText(TypeName, 'Double') or
    SameText(TypeName, 'Single') or SameText(TypeName, 'Real') or
    SameText(TypeName, 'Currency') or SameText(TypeName, 'Comp');
end;

/// <summary>
/// Infers the type of the first argument in a function call.
/// @param Tokens the token array
/// @param OpenParenIdx index of the '(' token after the function name
/// @param Procs procedure scopes for variable lookup
/// @param Classes class scopes for variable lookup
/// @param ContextIdx token index providing scope context for lookups
/// @param ExternalLookup optional callback for external type resolution
/// @returns the inferred argument type, or '' if unknown </summary>
function InferArgumentType(const Tokens: TTokenArray; OpenParenIdx: Integer;
  const Procs: TProcScopeArray; const Classes: TClassScopeArray;
  ContextIdx: Integer;
  ExternalLookup: TExternalTypeLookup): string;
var
  ArgIdx: Integer;
begin
  Result := '';
  ArgIdx := NextNonJunk(Tokens, OpenParenIdx);
  if ArgIdx > High(Tokens) then Exit;
  if Tokens[ArgIdx].Kind = tkRoundClose then Exit;

  // Skip unary minus/plus
  if (Tokens[ArgIdx].Kind in [tkMinus, tkPlus]) then begin
    ArgIdx := NextNonJunk(Tokens, ArgIdx);
    if ArgIdx > High(Tokens) then Exit;
  end;

  case Tokens[ArgIdx].Kind of
    tkInteger, tkNumber: begin
      if ExpressionIsFloat(Tokens, ArgIdx) then
        Result := 'Double'
      else
        Result := 'Integer';
    end;
    tkFloat: Result := 'Double';
    tkString: begin
      if (Length(Tokens[ArgIdx].Text) = 3) and (Tokens[ArgIdx].Text[1] = '''') then
        Result := 'Char'
      else
        Result := 'string';
    end;
  else
    if Tokens[ArgIdx].Kind in IdentDirect then begin
      if SameText(Tokens[ArgIdx].Text, 'True') or SameText(Tokens[ArgIdx].Text, 'False') then
        Result := 'Boolean'
      else begin
        Result := LookupVarType(Tokens, Tokens[ArgIdx].Text, Procs, Classes, ContextIdx);
        if Result = '' then begin
          Result := LookupFuncReturnType(Tokens, Tokens[ArgIdx].Text, '', Procs);
          if Result = '' then
            Result := LookupIntrinsicReturnType(Tokens[ArgIdx].Text);
        end;
      end;
    end;
  end;
end;

/// <summary>
/// Looks up the return type of an overloaded compiler intrinsic (Abs, Sqr, Pred, Succ)
/// based on the argument type.
/// @param FuncName the intrinsic function name
/// @param ArgType the determined argument type ('' if unknown)
/// @param Alternatives receives alternative type suggestions when ArgType is unknown
/// @returns the inferred return type, or '' if FuncName is not an overloaded intrinsic </summary>
function LookupOverloadedIntrinsicType(const FuncName, ArgType: string;
  out Alternatives: TTypeAlternatives): string;
begin
  Result := '';
  Alternatives := nil;

  if SameText(FuncName, 'Abs') or SameText(FuncName, 'Sqr') then begin
    if IsInt64Type(ArgType) then
      Result := 'Int64'
    else if IsFloatType(ArgType) then
      Result := ArgType
    else
      Result := 'Integer';
    // Always offer alternatives for Abs/Sqr since the user may want a different type
    SetLength(Alternatives, 0);
    if not SameText(Result, 'Integer') then begin
      SetLength(Alternatives, Length(Alternatives) + 1);
      Alternatives[High(Alternatives)] := 'Integer';
    end;
    if not SameText(Result, 'Cardinal') then begin
      SetLength(Alternatives, Length(Alternatives) + 1);
      Alternatives[High(Alternatives)] := 'Cardinal';
    end;
    if not IsInt64Type(Result) then begin
      SetLength(Alternatives, Length(Alternatives) + 1);
      Alternatives[High(Alternatives)] := 'Int64';
    end;
    if not SameText(Result, 'UInt64') then begin
      SetLength(Alternatives, Length(Alternatives) + 1);
      Alternatives[High(Alternatives)] := 'UInt64';
    end;
    if not IsFloatType(Result) then begin
      SetLength(Alternatives, Length(Alternatives) + 1);
      Alternatives[High(Alternatives)] := 'Extended';
    end;
  end else if SameText(FuncName, 'Pred') or SameText(FuncName, 'Succ') then begin
    if ArgType <> '' then
      Result := ArgType
    else begin
      Result := 'Integer';
      SetLength(Alternatives, 2);
      Alternatives[0] := 'Char';
      Alternatives[1] := 'Byte';
    end;
  end;
end;

function InferTypeFromContext(const Tokens: TTokenArray; IdentTokenIdx: Integer;
  const IdentName: string; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray;
  ExternalLookup: TExternalTypeLookup;
  out AlternativeTypes: TTypeAlternatives): string;
var
  I, NI, PI, AssignIdx, RhsIdx, LhsIdx, FuncIdx: Integer;
  ParenLevel, ParamIndex: Integer;
  FuncName, ArgType: string;
begin
  Result := '';
  AlternativeTypes := nil;

  // Scan all occurrences of the identifier to find assignment context
  for I := 0 to High(Tokens) do begin
    if not ((Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName)) then
      Continue;

    NI := NextNonJunk(Tokens, I);

    // Pattern: Ident := <value>
    if (NI <= High(Tokens)) and (Tokens[NI].Kind = tkAssign) then begin
      RhsIdx := NextNonJunk(Tokens, NI);
      if RhsIdx > High(Tokens) then
        Continue;

      case Tokens[RhsIdx].Kind of
        tkInteger, tkNumber: begin
          // Check the entire expression for '/' or float literals
          if ExpressionIsFloat(Tokens, RhsIdx) then
            Result := 'Double'
          else
            Result := 'Integer';
          Exit;
        end;
        tkFloat: begin
          Result := 'Double';
          Exit;
        end;
        tkString: begin
          // Single char string -> Char, multi-char -> string
          if (Length(Tokens[RhsIdx].Text) = 3) and (Tokens[RhsIdx].Text[1] = '''') then
            Result := 'Char'
          else
            Result := 'string';
          Exit;
        end;
      end;

      // Check for True/False -> Boolean
      if (Tokens[RhsIdx].Kind in IdentDirect) then begin
        if SameText(Tokens[RhsIdx].Text, 'True') or SameText(Tokens[RhsIdx].Text, 'False') then begin
          Result := 'Boolean';
          Exit;
        end;
        if SameText(Tokens[RhsIdx].Text, 'nil') then begin
          Result := 'Pointer';
          Exit;
        end;

        // Pattern: Ident := TFoo.Create -> TFoo
        AssignIdx := NextNonJunk(Tokens, RhsIdx);
        if (AssignIdx <= High(Tokens)) and (Tokens[AssignIdx].Kind = tkPoint) then begin
          AssignIdx := NextNonJunk(Tokens, AssignIdx);
          if (AssignIdx <= High(Tokens)) and (Tokens[AssignIdx].Kind in IdentDirect) and
             SameText(Tokens[AssignIdx].Text, 'Create') then begin
            Result := Tokens[RhsIdx].Text;
            Exit;
          end;
        end;

        // Pattern: Ident := Func(...) -> lookup function return type
        AssignIdx := NextNonJunk(Tokens, RhsIdx);
        if (AssignIdx <= High(Tokens)) and (Tokens[AssignIdx].Kind = tkPoint) then begin
          // Qualified call: Ident := Obj.Method or Ident := Obj.Method(...)
          AssignIdx := NextNonJunk(Tokens, AssignIdx);
          if (AssignIdx <= High(Tokens)) and (Tokens[AssignIdx].Kind in IdentDirect) then begin
            // Try exact class match first (e.g. TMyObj.GetValue)
            Result := LookupFuncReturnType(Tokens, Tokens[AssignIdx].Text,
              Tokens[RhsIdx].Text, Procs);
            if Result = '' then begin
              // Qualifier may be a variable, not a class — look up its type
              FuncName := LookupVarType(Tokens, Tokens[RhsIdx].Text, Procs, Classes, I);
              if FuncName <> '' then
                Result := LookupFuncReturnType(Tokens, Tokens[AssignIdx].Text,
                  FuncName, Procs);
            end;
            // Fallback: try external lookup for Obj.Method
            if (Result = '') and Assigned(ExternalLookup) then
              Result := ExternalLookup(Tokens[AssignIdx].Text, elkFuncReturn, -1);
            if Result <> '' then
              Exit;
          end;
        end else if (AssignIdx <= High(Tokens)) and (Tokens[AssignIdx].Kind = tkRoundOpen) then begin
          // Plain call: Ident := Func(...)
          Result := LookupFuncReturnType(Tokens, Tokens[RhsIdx].Text, '', Procs);
          if Result = '' then
            Result := LookupIntrinsicReturnType(Tokens[RhsIdx].Text);
          if Result = '' then begin
            // Try overloaded intrinsics (Abs, Sqr, Pred, Succ) with argument type analysis
            ArgType := InferArgumentType(Tokens, AssignIdx, Procs, Classes, I, ExternalLookup);
            Result := LookupOverloadedIntrinsicType(Tokens[RhsIdx].Text, ArgType, AlternativeTypes);
          end;
          if (Result = '') and Assigned(ExternalLookup) then
            Result := ExternalLookup(Tokens[RhsIdx].Text, elkFuncReturn, -1);
          if Result <> '' then
            Exit;
        end else begin
          // Plain identifier or call without parens: Ident := Func
          Result := LookupFuncReturnType(Tokens, Tokens[RhsIdx].Text, '', Procs);
          if Result = '' then
            Result := LookupIntrinsicReturnType(Tokens[RhsIdx].Text);
          if (Result = '') and Assigned(ExternalLookup) then
            Result := ExternalLookup(Tokens[RhsIdx].Text, elkFuncReturn, -1);
          if Result <> '' then
            Exit;
        end;

        // Pattern: Ident := SomeVar -> lookup SomeVar's type
        if not SameText(Tokens[RhsIdx].Text, IdentName) then begin
          Result := LookupVarType(Tokens, Tokens[RhsIdx].Text, Procs, Classes, I);
          if Result <> '' then
            Exit;
          // Try external lookup for enum values, external constants, etc.
          if Assigned(ExternalLookup) then begin
            Result := ExternalLookup(Tokens[RhsIdx].Text, elkIdentType, -1);
            if Result <> '' then
              Exit;
          end;
        end;

        // Fallback: check if expression contains '/' (always float in Delphi)
        if ExpressionIsFloat(Tokens, RhsIdx) then begin
          Result := 'Double';
          Exit;
        end;
      end;
    end;

    // Pattern: SomeVar := Ident (look up SomeVar's type)
    PI := PrevNonJunk(Tokens, I);
    if (PI >= 0) and (Tokens[PI].Kind = tkAssign) then begin
      LhsIdx := PrevNonJunk(Tokens, PI);
      if (LhsIdx >= 0) and (Tokens[LhsIdx].Kind in IdentDirect) and
         not SameText(Tokens[LhsIdx].Text, IdentName) then begin
        Result := LookupVarType(Tokens, Tokens[LhsIdx].Text, Procs, Classes, I);
        if Result <> '' then
          Exit;
      end;
    end;

    // Pattern: Func(... Ident ...) -> lookup parameter type by position
    // Walk backward from Ident, counting commas to determine parameter index,
    // until we find the opening '(' and the function name before it.
    if (PI >= 0) and (Tokens[PI].Kind in [tkRoundOpen, tkComma]) then begin
      ParamIndex := 0;
      ParenLevel := 0;
      FuncIdx := PI;
      // Count commas backward at the same paren level to get parameter index
      while FuncIdx >= 0 do begin
        case Tokens[FuncIdx].Kind of
          tkRoundClose: Inc(ParenLevel);
          tkRoundOpen: begin
            if ParenLevel > 0 then
              Dec(ParenLevel)
            else begin
              // Found our opening paren — function name is before it
              FuncIdx := PrevNonJunk(Tokens, FuncIdx);
              if (FuncIdx >= 0) and (Tokens[FuncIdx].Kind in IdentDirect) then begin
                FuncName := Tokens[FuncIdx].Text;
                Result := LookupParamType(Tokens, FuncName, ParamIndex, Procs);
                if (Result = '') and Assigned(ExternalLookup) then
                  Result := ExternalLookup(FuncName, elkParamType, ParamIndex);
                if Result <> '' then
                  Exit;
              end;
              Break;
            end;
          end;
          tkComma: begin
            if ParenLevel = 0 then
              Inc(ParamIndex);
          end;
        end;
        Dec(FuncIdx);
      end;
    end;
  end;
end;

function InferTypeFromContext(const Tokens: TTokenArray; IdentTokenIdx: Integer;
  const IdentName: string; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray;
  ExternalLookup: TExternalTypeLookup = nil): string;
var
  Alts: TTypeAlternatives;
begin
  Result := InferTypeFromContext(Tokens, IdentTokenIdx, IdentName, Procs, Classes,
    ExternalLookup, Alts);
end;

type
  TVisibilityKind = (vkImplicit, vkPrivate, vkStrictPrivate,
    vkProtected, vkStrictProtected, vkPublic, vkPublished);

  TVisSectionRec = record
    Exists: Boolean;
    LastFieldEnd: Integer;    // token index of last field's semicolon, or -1
    KeywordIdx: Integer;      // token index of the visibility keyword
    VisIndent: string;        // indent of the visibility keyword
  end;

function VisibilityName(V: TVisibilityKind): string;
begin
  case V of
    vkImplicit: Result := 'default (implicit private)';
    vkPrivate: Result := 'private';
    vkStrictPrivate: Result := 'strict private';
    vkProtected: Result := 'protected';
    vkStrictProtected: Result := 'strict protected';
    vkPublic: Result := 'public';
    vkPublished: Result := 'published';
  end;
end;

procedure AddInsertionPoint(var Arr: TInsertionPointArray; var Count, Capacity: Integer;
  const Pt: TInsertionPoint);
begin
  if Count >= Capacity then begin
    Capacity := Capacity * 2;
    SetLength(Arr, Capacity);
  end;
  Arr[Count] := Pt;
  Inc(Count);
end;

function FindInsertionPoints(const Source: string; const Tokens: TTokenArray;
  const Procs: TProcScopeArray; const Classes: TClassScopeArray;
  CursorTokenIdx: Integer; const IdentName: string): TInsertionPointArray;
var
  EnclosingProcs: TProcScopeArray;
  I, J, K, NI, PI, Count, Capacity, SemiIdx: Integer;
  Pt: TInsertionPoint;
  InVarSection: Boolean;
  LastVarDeclEnd: Integer;
  BeginPos: Integer;
  Indent: string;
  Cls: TClassScope;
  CurrentVis, V: TVisibilityKind;
  NestDepth: Integer;
  ParenLevel: Integer;
  EncProc: TProcScope;
  Sections: array[TVisibilityKind] of TVisSectionRec;
  HasAnySections: Boolean;
  EndCharPos: Integer;
  VisIndent: string;
begin
  Capacity := 8;
  SetLength(Result, Capacity);
  Count := 0;
  Pt.SectionHeader := '';

  EnclosingProcs := FindEnclosingProcs(Procs, CursorTokenIdx);

  // --- Local variable insertion points ---
  for K := 0 to High(EnclosingProcs) do begin
    EncProc := EnclosingProcs[K];
    if EncProc.BodyStart < 0 then
      Continue;

    // Scan from header to body start for var sections
    InVarSection := False;
    LastVarDeclEnd := -1;
    I := EncProc.HeaderStart;

    // Skip past parameter list
    ParenLevel := 0;
    while (I <= High(Tokens)) and (I < EncProc.BodyStart) do begin
      if Tokens[I].Kind = tkRoundOpen then
        Inc(ParenLevel)
      else if Tokens[I].Kind = tkRoundClose then
        Dec(ParenLevel)
      else if (Tokens[I].Kind = tkSemiColon) and (ParenLevel = 0) then begin
        Inc(I);
        Break;
      end;
      Inc(I);
    end;

    // Now scan the local declarations section
    while (I <= High(Tokens)) and (I < EncProc.BodyStart) do begin
      case Tokens[I].Kind of
        tkVar: begin
          InVarSection := True;
          LastVarDeclEnd := -1;
        end;
        tkConst, tkType, tkLabel: begin
          // If we were in a var section and found another section keyword,
          // the var section ended — record the last declaration end
          if InVarSection and (LastVarDeclEnd > 0) then begin
            Pt.Description := 'var section in ' + EncProc.Name;
            Pt.Kind := dkLocalVar;
            // Insert after the last var declaration's semicolon + EOL
            Pt.CharPos := FindEndOfLine(Source, LastVarDeclEnd + 1);
            Pt.Indent := GetIndentAtPos(Source, Tokens[LastVarDeclEnd].Pos + 1);
            Pt.NeedsVarKeyword := False;
            AddInsertionPoint(Result, Count, Capacity, Pt);
          end;
          InVarSection := False;
        end;
        tkSemiColon: begin
          if InVarSection then
            LastVarDeclEnd := I;
        end;
        tkBegin, tkAsm:
          Break;
      end;
      Inc(I);
    end;

    // If still in a var section at the end
    if InVarSection and (LastVarDeclEnd > 0) then begin
      Pt.Description := 'var section in ' + EncProc.Name;
      Pt.Kind := dkLocalVar;
      Pt.CharPos := FindEndOfLine(Source, Tokens[LastVarDeclEnd].Pos + 1 + Tokens[LastVarDeclEnd].Len);
      Pt.Indent := GetIndentAtPos(Source, Tokens[LastVarDeclEnd].Pos + 1);
      Pt.NeedsVarKeyword := False;
      AddInsertionPoint(Result, Count, Capacity, Pt);
    end;

    // Option: create new var section before begin
    if EncProc.BodyStart >= 0 then begin
      BeginPos := Tokens[EncProc.BodyStart].Pos + 1; // 1-based
      Indent := GetIndentAtPos(Source, BeginPos);
      Pt.Description := 'new var section in ' + EncProc.Name;
      Pt.Kind := dkLocalVar;
      Pt.CharPos := FindLineStart(Source, BeginPos);
      Pt.Indent := Indent;
      Pt.NeedsVarKeyword := True;
      AddInsertionPoint(Result, Count, Capacity, Pt);
    end;
  end;

  // --- Class field / property insertion points ---
  // Find enclosing class for the cursor
  for I := 0 to High(Classes) do begin
    Cls := Classes[I];
    // Check if cursor is in a method of this class
    for K := 0 to High(EnclosingProcs) do begin
      if SameText(EnclosingProcs[K].ClassName, Cls.Name) then begin
        // Scan class to collect visibility section info
        CurrentVis := vkImplicit;
        NestDepth := 0;
        for V := Low(TVisibilityKind) to High(TVisibilityKind) do begin
          Sections[V].Exists := False;
          Sections[V].LastFieldEnd := -1;
          Sections[V].KeywordIdx := -1;
          Sections[V].VisIndent := '';
        end;

        J := Cls.DeclStart;
        while J <= Cls.DeclEnd do begin
          if Tokens[J].Kind in [tkRoundOpen, tkSquareOpen] then
            Inc(NestDepth)
          else if Tokens[J].Kind in [tkRoundClose, tkSquareClose] then
            Dec(NestDepth);

          // Detect visibility keywords
          if Tokens[J].Kind in [tkPrivate, tkProtected, tkPublic, tkPublished] then begin
            // Save current section's last field end before switching
            Sections[CurrentVis].LastFieldEnd :=
              Sections[CurrentVis].LastFieldEnd; // already tracked below

            PI := PrevNonJunk(Tokens, J);
            if (PI >= Cls.DeclStart) and (Tokens[PI].Kind = tkStrict) then begin
              if Tokens[J].Kind = tkPrivate then
                CurrentVis := vkStrictPrivate
              else
                CurrentVis := vkStrictProtected;
              Sections[CurrentVis].VisIndent := GetIndentAtPos(Source, Tokens[PI].Pos + 1);
            end else begin
              case Tokens[J].Kind of
                tkPrivate: CurrentVis := vkPrivate;
                tkProtected: CurrentVis := vkProtected;
                tkPublic: CurrentVis := vkPublic;
                tkPublished: CurrentVis := vkPublished;
              end;
              Sections[CurrentVis].VisIndent := GetIndentAtPos(Source, Tokens[J].Pos + 1);
            end;
            Sections[CurrentVis].Exists := True;
            Sections[CurrentVis].KeywordIdx := J;
          end;

          // Track field declarations (Ident: Type;)
          // Only at top level (not inside method parameter lists or property indexers)
          // and only if the identifier is not a method/property name
          if (NestDepth = 0) and (Tokens[J].Kind in IdentDirect) then begin
            PI := PrevNonJunk(Tokens, J);
            if (PI < Cls.DeclStart) or not (Tokens[PI].Kind in
                [tkProcedure, tkFunction, tkConstructor, tkDestructor, tkProperty]) then begin
              NI := NextNonJunk(Tokens, J);
              if (NI <= Cls.DeclEnd) and (Tokens[NI].Kind in [tkColon, tkComma]) then begin
                SemiIdx := NI;
                while (SemiIdx <= Cls.DeclEnd) and (Tokens[SemiIdx].Kind <> tkSemiColon) do
                  Inc(SemiIdx);
                if SemiIdx <= Cls.DeclEnd then
                  Sections[CurrentVis].LastFieldEnd := SemiIdx;
              end;
            end;
          end;

          Inc(J);
        end;

        // Determine indent for visibility keywords (from any existing section)
        VisIndent := '';
        HasAnySections := False;
        for V := vkPrivate to vkPublished do begin
          if Sections[V].Exists then begin
            HasAnySections := True;
            if VisIndent = '' then
              VisIndent := Sections[V].VisIndent;
          end;
        end;
        if VisIndent = '' then
          VisIndent := GetIndentAtPos(Source, Tokens[Cls.DeclEnd].Pos + 1);
        EndCharPos := FindLineStart(Source, Tokens[Cls.DeclEnd].Pos + 1);

        // Emit insertion points in fixed order: private, strict private,
        // protected, strict protected, public, published
        if HasAnySections then begin
          for V := vkPrivate to vkPublished do begin
            Pt.Kind := dkField;
            Pt.NeedsVarKeyword := False;
            if Sections[V].Exists then begin
              // Existing section
              Pt.SectionHeader := '';
              if Sections[V].LastFieldEnd > 0 then begin
                // Insert after last field in this section
                Pt.Description := VisibilityName(V) + ' of ' + Cls.Name;
                Pt.CharPos := FindEndOfLine(Source,
                  Tokens[Sections[V].LastFieldEnd].Pos + 1 +
                  Tokens[Sections[V].LastFieldEnd].Len);
                Pt.Indent := GetIndentAtPos(Source,
                  Tokens[Sections[V].LastFieldEnd].Pos + 1);
              end else begin
                // Section has no fields — insert right after visibility keyword
                NI := NextNonJunk(Tokens, Sections[V].KeywordIdx);
                if NI > Cls.DeclEnd then
                  Continue;
                Pt.Description := VisibilityName(V) + ' of ' + Cls.Name;
                Pt.CharPos := FindLineStart(Source, Tokens[NI].Pos + 1);
                Pt.Indent := GetIndentAtPos(Source, Tokens[NI].Pos + 1);
              end;
              AddInsertionPoint(Result, Count, Capacity, Pt);
            end else begin
              // New section — insert before 'end' with section header
              Pt.Description := VisibilityName(V) + ' of ' + Cls.Name + ' (new)';
              Pt.CharPos := EndCharPos;
              Pt.Indent := VisIndent + '  ';
              Pt.SectionHeader := VisIndent + VisibilityName(V);
              AddInsertionPoint(Result, Count, Capacity, Pt);
            end;
          end;
          Pt.SectionHeader := '';
        end else begin
          // Fallback for classes without any visibility keywords
          NI := NextNonJunk(Tokens, Cls.DeclStart);
          // Skip optional (TParent) or (TParent, IInterface)
          if (NI <= Cls.DeclEnd) and (Tokens[NI].Kind = tkRoundOpen) then begin
            ParenLevel := 1;
            Inc(NI);
            while (NI <= Cls.DeclEnd) and (ParenLevel > 0) do begin
              if Tokens[NI].Kind = tkRoundOpen then Inc(ParenLevel)
              else if Tokens[NI].Kind = tkRoundClose then Dec(ParenLevel);
              Inc(NI);
            end;
          end;
          if NI <= Cls.DeclEnd then begin
            if Sections[vkImplicit].LastFieldEnd > 0 then begin
              Pt.Description := Cls.Name;
              Pt.Kind := dkField;
              Pt.CharPos := FindEndOfLine(Source,
                Tokens[Sections[vkImplicit].LastFieldEnd].Pos + 1 +
                Tokens[Sections[vkImplicit].LastFieldEnd].Len);
              Pt.Indent := GetIndentAtPos(Source,
                Tokens[Sections[vkImplicit].LastFieldEnd].Pos + 1);
            end else begin
              Pt.Description := Cls.Name;
              Pt.Kind := dkField;
              Pt.CharPos := FindLineStart(Source, Tokens[NI].Pos + 1);
              Pt.Indent := GetIndentAtPos(Source, Tokens[NI].Pos + 1);
            end;
            Pt.NeedsVarKeyword := False;
            AddInsertionPoint(Result, Count, Capacity, Pt);
          end;
        end;

        Break; // Only handle the first matching class
      end;
    end;
  end;

  SetLength(Result, Count);
end;

function BuildDeclarationText(const IdentName, TypeName: string;
  Kind: TDeclKind; const Indent: string;
  const PropertyRead, PropertyWrite: string): string;
begin
  case Kind of
    dkLocalVar:
      Result := Indent + IdentName + ': ' + TypeName + ';' + #13#10;
    dkField:
      Result := Indent + IdentName + ': ' + TypeName + ';' + #13#10;
    dkProperty: begin
      Result := Indent + 'property ' + IdentName + ': ' + TypeName;
      if PropertyRead <> '' then
        Result := Result + ' read ' + PropertyRead;
      if PropertyWrite <> '' then
        Result := Result + ' write ' + PropertyWrite;
      Result := Result + ';' + #13#10;
    end;
  end;
end;

function InsertDeclaration(const Source: string;
  const InsertionPt: TInsertionPoint;
  const DeclText: string): string;
var
  VarLine, IndentedDecl, Prefix: string;
begin
  Prefix := '';
  if InsertionPt.SectionHeader <> '' then
    Prefix := InsertionPt.SectionHeader + #13#10;

  if InsertionPt.NeedsVarKeyword then begin
    VarLine := InsertionPt.Indent + 'var' + #13#10;
    // Add 2 extra spaces to the declaration text relative to var keyword
    IndentedDecl := InsertionPt.Indent + '  ' +
      Copy(DeclText, Length(InsertionPt.Indent) + 1,
           Length(DeclText) - Length(InsertionPt.Indent));
    Result := Copy(Source, 1, InsertionPt.CharPos - 1)
      + Prefix + VarLine + IndentedDecl
      + Copy(Source, InsertionPt.CharPos, Length(Source) - InsertionPt.CharPos + 1);
  end else begin
    Result := Copy(Source, 1, InsertionPt.CharPos - 1)
      + Prefix + DeclText
      + Copy(Source, InsertionPt.CharPos, Length(Source) - InsertionPt.CharPos + 1);
  end;
end;

function AnalyzeForDeclaration(const Source: string; CursorPos: Integer;
  out Info: TDeclareInfo; ExternalLookup: TExternalTypeLookup = nil): Boolean;
var
  Tokens: TTokenArray;
  Procs: TProcScopeArray;
  Classes: TClassScopeArray;
  CursorTokenIdx: Integer;
  Scope: TRenameScope;
  IdentInfo: TIdentifierInfo;
  I: Integer;
begin
  Result := False;
  Info.IdentName := '';
  Info.InferredType := '';
  Info.InferenceDesc := '';
  Info.ExistingDecl := '';
  Info.IsReservedWord := False;
  Info.AlternativeTypes := nil;
  Info.InsertionPoints := nil;
  Info.DefaultKindIndex := 0;
  Info.ErrorMsg := '';

  Tokens := TokenizeSource(Source);
  if Length(Tokens) = 0 then begin
    Info.ErrorMsg := 'No tokens found in source';
    Exit;
  end;

  CursorTokenIdx := FindTokenAtCursor(Tokens, CursorPos - 1);
  if CursorTokenIdx < 0 then begin
    Info.ErrorMsg := 'No identifier at cursor position';
    Exit;
  end;

  Info.IdentName := Tokens[CursorTokenIdx].Text;

  // Check reserved word
  if IsReservedWord(Info.IdentName) then begin
    Info.IsReservedWord := True;
    Info.ErrorMsg := '''' + Info.IdentName + ''' is a reserved word';
    Result := True;
    Exit;
  end;

  // Build scopes
  Classes := BuildClassScopes(Tokens);
  Procs := BuildProcScopes(Tokens, Classes);

  // Check existing declaration
  Scope := DetectScope(Tokens, CursorTokenIdx, Info.IdentName, Procs, Classes);
  IdentInfo := DetectIdentifierKind(Tokens, Scope, Procs, Classes,
    Info.IdentName, CursorTokenIdx);
  if IdentInfo.Kind <> ikUnknown then begin
    if IdentInfo.Kind <> ikExternalIdent then
      Info.ExistingDecl := IdentInfo.KindDesc + ': ' + IdentInfo.TypeName;
  end;

  // Infer type from context
  Info.InferredType := InferTypeFromContext(Tokens, CursorTokenIdx,
    Info.IdentName, Procs, Classes, ExternalLookup, Info.AlternativeTypes);

  // Find insertion points
  Info.InsertionPoints := FindInsertionPoints(Source, Tokens, Procs, Classes,
    CursorTokenIdx, Info.IdentName);

  if Length(Info.InsertionPoints) = 0 then begin
    Info.ErrorMsg := 'No suitable insertion point found';
    Result := True;
    Exit;
  end;

  // Determine default index: prefer local var if available, else first field
  Info.DefaultKindIndex := 0;
  if (Length(Info.IdentName) > 1) and (UpCase(Info.IdentName[1]) = 'F')
     and (Info.IdentName[2] >= 'A') and (Info.IdentName[2] <= 'Z') then begin
    // Identifier starts with F -> default to field
    for I := 0 to High(Info.InsertionPoints) do begin
      if Info.InsertionPoints[I].Kind = dkField then begin
        Info.DefaultKindIndex := I;
        Break;
      end;
    end;
  end;

  Result := True;
end;

end.
