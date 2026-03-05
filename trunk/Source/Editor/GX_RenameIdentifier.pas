/// Core logic for the Rename Identifier expert.
/// This unit has no OTA dependency and operates on plain strings,
/// making it fully testable with DUnit.
unit GX_RenameIdentifier;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  GX_PascalTokenAnalyzer,
  mPasLex, mwPasParserTypes;

/// Renames the identifier at CursorPos in Source to NewName.
/// CursorPos is 1-based character position in Source.
/// If NewName is empty, runs in "preview" mode: returns scope info and
/// occurrence count without applying any changes.
/// Returns True on success.
function RenameIdentifierInSource(
  const Source: string;
  CursorPos: Integer;
  const NewName: string;
  out RenamedSource: string;
  out OccurrenceCount: Integer;
  out OldName: string;
  out ScopeDesc: string;
  out IdentInfo: TIdentifierInfo;
  out ErrorMsg: string
): Boolean;

/// Checks if NewName conflicts with any visible identifier at the cursor position.
/// Returns True if a conflict is found, with a description in ConflictDesc.
function CheckNameConflict(
  const Source: string;
  CursorPos: Integer;
  const NewName: string;
  out ConflictDesc: string
): Boolean;

/// Renames all occurrences of OldName to NewName in Source using lexer-based
/// tokenization. Only identifier tokens are replaced (not strings, comments,
/// or compiler directives). Matching is case-insensitive.
/// Returns the modified source and the number of replacements made.
function RenameAllTokenOccurrences(const Source, OldName, NewName: string;
  out OccurrenceCount: Integer): string;

implementation

uses
  SysUtils;

function IsTokenShadowedByInnerProc(const Tokens: TTokenArray;
  const Procs: TProcScopeArray; OuterProcIdx, TokenIdx: Integer;
  const IdentName: string): Boolean;
var
  J: Integer;
begin
  Result := False;
  for J := 0 to High(Procs) do begin
    if J = OuterProcIdx then
      Continue;
    if (Procs[J].BodyEnd >= 0) and
       (Procs[J].HeaderStart > Procs[OuterProcIdx].HeaderStart) and
       (Procs[J].BodyEnd <= Procs[OuterProcIdx].BodyEnd) and
       (TokenIdx >= Procs[J].HeaderStart) and
       (TokenIdx <= Procs[J].BodyEnd) and
       IsLocalDeclaration(Tokens, Procs[J], IdentName) then begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure CollectMethodParamOccurrences(const Tokens: TTokenArray;
  const Cls: TClassScope; const MethodName, IdentName: string;
  Occurrences: TList);
// Scan the class declaration for the named method's parameter list and
// add matching identifier occurrences to the list.
var
  I, NameI, ParenLevel: Integer;
begin
  I := Cls.DeclStart;
  while I <= Cls.DeclEnd do begin
    if IsProcKeyword(Tokens[I].Kind) then begin
      NameI := NextNonJunk(Tokens, I);
      if (NameI <= Cls.DeclEnd) and (Tokens[NameI].Kind in IdentDirect) and
         SameText(Tokens[NameI].Text, MethodName) then begin
        I := NextNonJunk(Tokens, NameI);
        if (I <= Cls.DeclEnd) and (Tokens[I].Kind = tkRoundOpen) then begin
          ParenLevel := 1;
          Inc(I);
          while (I <= Cls.DeclEnd) and (ParenLevel > 0) do begin
            if Tokens[I].Kind = tkRoundOpen then
              Inc(ParenLevel)
            else if Tokens[I].Kind = tkRoundClose then begin
              Dec(ParenLevel);
              if ParenLevel = 0 then
                Break;
            end;
            if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName) then
              Occurrences.Add(Pointer(I));
            Inc(I);
          end;
        end;
        Exit;
      end;
    end;
    Inc(I);
  end;
end;

function IsAssignmentTarget(const Tokens: TTokenArray; TokenIdx: Integer): Boolean;
// Returns True if the token at TokenIdx is followed by ':=' (assignment operator),
// i.e. it's used as the left-hand side of an assignment.
var
  NI: Integer;
begin
  NI := NextNonJunk(Tokens, TokenIdx);
  Result := (NI <= High(Tokens)) and (Tokens[NI].Kind = tkAssign);
end;

function CollectFuncNameAsResultOccurrences(const Tokens: TTokenArray;
  const FuncName: string; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray; const Scope: TRenameScope): TList;
// Collect only assignment-target occurrences of a function name inside
// its own body. Used when renaming a function name to 'Result'.
// Skips the function declaration, call sites, and recursive calls.
var
  I, ProcIdx: Integer;
begin
  Result := TList.Create;

  // Find all procs that match this function name
  for ProcIdx := 0 to High(Procs) do begin
    if not SameText(Procs[ProcIdx].Name, FuncName) then
      Continue;
    if not Procs[ProcIdx].IsFunction then
      Continue;
    if Procs[ProcIdx].BodyStart < 0 then
      Continue;

    // For class-scoped rename, only process methods of that class
    if (Scope.Kind = skClass) and (Scope.ClassIndex >= 0) then begin
      if not SameText(Procs[ProcIdx].ClassName, Classes[Scope.ClassIndex].Name) then
        Continue;
    end;

    // Scan the body (between begin and end) for assignment targets
    for I := Procs[ProcIdx].BodyStart to Procs[ProcIdx].BodyEnd do begin
      if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, FuncName) then begin
        // Skip if inside a nested proc that has the same name
        // (unlikely but defensive)
        if IsAssignmentTarget(Tokens, I) then
          Result.Add(Pointer(I));
      end;
    end;
  end;
end;

function CollectOccurrences(const Tokens: TTokenArray; const IdentName: string;
  const Scope: TRenameScope; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray): TList;
var
  I, J: Integer;
  Shadowed: Boolean;
  ProcIdx: Integer;
begin
  Result := TList.Create;

  case Scope.Kind of
    skProcedure: begin
      ProcIdx := Scope.ProcIndex;
      if (ProcIdx >= 0) and (ProcIdx <= High(Procs)) then begin
        // For class methods, also collect from the class declaration's method param list
        if Procs[ProcIdx].ClassName <> '' then begin
          for J := 0 to High(Classes) do begin
            if SameText(Classes[J].Name, Procs[ProcIdx].ClassName) then begin
              CollectMethodParamOccurrences(Tokens, Classes[J],
                Procs[ProcIdx].Name, IdentName, Result);
              Break;
            end;
          end;
        end;
        // Collect from the implementation proc's range
        for I := Procs[ProcIdx].HeaderStart to Procs[ProcIdx].BodyEnd do begin
          if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName) then
            Result.Add(Pointer(I));
        end;
      end;
    end;

    skClass: begin
      if (Scope.ClassIndex >= 0) and (Scope.ClassIndex <= High(Classes)) then begin
        // In class declaration
        for I := Classes[Scope.ClassIndex].DeclStart to Classes[Scope.ClassIndex].DeclEnd do begin
          if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName) then
            Result.Add(Pointer(I));
        end;
        // In method implementations
        for J := 0 to High(Procs) do begin
          if SameText(Procs[J].ClassName, Classes[Scope.ClassIndex].Name) and
             (Procs[J].BodyEnd >= 0) then begin
            // Skip method if it declares a local of the same name (shadowing)
            if IsLocalDeclaration(Tokens, Procs[J], IdentName) then
              Continue;
            for I := Procs[J].HeaderStart to Procs[J].BodyEnd do begin
              if (I >= Classes[Scope.ClassIndex].DeclStart) and
                 (I <= Classes[Scope.ClassIndex].DeclEnd) then
                Continue;
              if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName) then begin
                Shadowed := IsTokenShadowedByInnerProc(Tokens, Procs, J, I, IdentName);
                if not Shadowed then
                  Result.Add(Pointer(I));
              end;
            end;
          end;
        end;
      end;
    end;

    skUnit: begin
      for I := 0 to High(Tokens) do begin
        if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName) then begin
          Shadowed := False;
          for J := 0 to High(Procs) do begin
            if (Procs[J].BodyEnd >= 0) and
               (I >= Procs[J].HeaderStart) and (I <= Procs[J].BodyEnd) and
               IsLocalDeclaration(Tokens, Procs[J], IdentName) then begin
              Shadowed := True;
              Break;
            end;
          end;
          if not Shadowed then
            Result.Add(Pointer(I));
        end;
      end;
    end;
  end;
end;

function FindIdentInScope(const Tokens: TTokenArray; const NewName: string;
  const Scope: TRenameScope; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray; CursorTokenIdx: Integer;
  out ConflictDesc: string): Boolean;
var
  I, NI, ParenLevel, ProcIdx: Integer;
  InVarSection, InConstSection, InTypeSection, InParamList: Boolean;
  Proc: TProcScope;
  Cls: TClassScope;
  J, PrevI: Integer;
begin
  Result := False;
  ConflictDesc := '';

  case Scope.Kind of
    skProcedure: begin
      ProcIdx := Scope.ProcIndex;
      if (ProcIdx < 0) or (ProcIdx > High(Procs)) then
        Exit;
      Proc := Procs[ProcIdx];

      // Check proc locals/params
      InVarSection := False;
      InConstSection := False;
      InTypeSection := False;
      InParamList := False;
      ParenLevel := 0;

      I := Proc.HeaderStart;
      while (I <= High(Tokens)) and ((Proc.BodyStart < 0) or (I < Proc.BodyStart)) do begin
        case Tokens[I].Kind of
          tkRoundOpen: begin
            if ParenLevel = 0 then InParamList := True;
            Inc(ParenLevel);
          end;
          tkRoundClose: begin
            Dec(ParenLevel);
            if ParenLevel = 0 then InParamList := False;
          end;
          tkVar: if not InParamList then begin InVarSection := True; InConstSection := False; InTypeSection := False; end;
          tkConst: if not InParamList then begin InConstSection := True; InVarSection := False; InTypeSection := False; end;
          tkType: if not InParamList then begin InTypeSection := True; InVarSection := False; InConstSection := False; end;
          tkBegin, tkAsm: Break;
        end;

        if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, NewName) then begin
          NI := NextNonJunk(Tokens, I);
          if InParamList and (NI <= High(Tokens)) and
             (Tokens[NI].Kind in [tkColon, tkComma, tkSemiColon, tkRoundClose]) then begin
            ConflictDesc := '''' + NewName + ''' already exists as a parameter';
            Result := True;
            Exit;
          end;
          if InVarSection and (NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkComma]) then begin
            ConflictDesc := '''' + NewName + ''' already exists as a local variable';
            Result := True;
            Exit;
          end;
          if InConstSection and (NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkEqual]) then begin
            ConflictDesc := '''' + NewName + ''' already exists as a local constant';
            Result := True;
            Exit;
          end;
          if InTypeSection and (NI <= High(Tokens)) and (Tokens[NI].Kind = tkEqual) then begin
            ConflictDesc := '''' + NewName + ''' already exists as a local type';
            Result := True;
            Exit;
          end;
        end;
        Inc(I);
      end;

      // Check class members if method
      if Proc.ClassName <> '' then begin
        for J := 0 to High(Classes) do begin
          if SameText(Classes[J].Name, Proc.ClassName) then begin
            for I := Classes[J].DeclStart to Classes[J].DeclEnd do begin
              if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, NewName) then begin
                NI := NextNonJunk(Tokens, I);
                PrevI := PrevNonJunk(Tokens, I);
                if ((NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkComma, tkEqual, tkRoundOpen, tkSemiColon])) or
                   ((PrevI >= 0) and (IsProcKeyword(Tokens[PrevI].Kind) or (Tokens[PrevI].Kind = tkProperty))) then begin
                  ConflictDesc := '''' + NewName + ''' already exists as a member of ' + Classes[J].Name;
                  Result := True;
                  Exit;
                end;
              end;
            end;
            Break;
          end;
        end;
      end;

      // Check unit-level identifiers
      for I := 0 to High(Tokens) do begin
        if IsTokenInsideAnyClass(I, Classes) then Continue;
        if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, NewName) then begin
          NI := NextNonJunk(Tokens, I);
          PrevI := PrevNonJunk(Tokens, I);
          // Skip if inside any proc
          for J := 0 to High(Procs) do begin
            if (Procs[J].BodyEnd >= 0) and (I >= Procs[J].HeaderStart) and (I <= Procs[J].BodyEnd) then begin
              // Inside a proc — skip it
              NI := -1; // mark as skipped
              Break;
            end;
          end;
          if NI = -1 then Continue;
          if ((NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkComma, tkEqual])) or
             ((PrevI >= 0) and IsProcKeyword(Tokens[PrevI].Kind)) then begin
            ConflictDesc := '''' + NewName + ''' already exists at unit level';
            Result := True;
            Exit;
          end;
        end;
      end;
    end;

    skClass: begin
      if (Scope.ClassIndex < 0) or (Scope.ClassIndex > High(Classes)) then
        Exit;
      Cls := Classes[Scope.ClassIndex];
      // Check other members
      for I := Cls.DeclStart to Cls.DeclEnd do begin
        if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, NewName) then begin
          NI := NextNonJunk(Tokens, I);
          PrevI := PrevNonJunk(Tokens, I);
          if ((NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkComma, tkEqual, tkRoundOpen, tkSemiColon])) or
             ((PrevI >= 0) and (IsProcKeyword(Tokens[PrevI].Kind) or (Tokens[PrevI].Kind = tkProperty))) then begin
            ConflictDesc := '''' + NewName + ''' already exists as a member of ' + Cls.Name;
            Result := True;
            Exit;
          end;
        end;
      end;
    end;

    skUnit: begin
      for I := 0 to High(Tokens) do begin
        if IsTokenInsideAnyClass(I, Classes) then Continue;
        if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, NewName) then begin
          NI := NextNonJunk(Tokens, I);
          PrevI := PrevNonJunk(Tokens, I);
          // Skip if inside any proc body
          for J := 0 to High(Procs) do begin
            if (Procs[J].BodyEnd >= 0) and (I >= Procs[J].HeaderStart) and (I <= Procs[J].BodyEnd) then begin
              NI := -1;
              Break;
            end;
          end;
          if NI = -1 then Continue;
          if ((NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkComma, tkEqual])) or
             ((PrevI >= 0) and IsProcKeyword(Tokens[PrevI].Kind)) then begin
            ConflictDesc := '''' + NewName + ''' already exists at unit level';
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function ApplyReplacements(const Source: string; const Tokens: TTokenArray;
  Occurrences: TList; const NewName: string): string;
var
  I, TokenIdx: Integer;
  Parts: TStringList;
  LastPos, TokStart, TokEnd: Integer;
begin
  if Occurrences.Count = 0 then begin
    Result := Source;
    Exit;
  end;

  Parts := TStringList.Create;
  try
    LastPos := 1;
    for I := 0 to Occurrences.Count - 1 do begin
      TokenIdx := Integer(Occurrences[I]);
      TokStart := Tokens[TokenIdx].Pos + 1;
      TokEnd := TokStart + Tokens[TokenIdx].Len;

      if TokStart > LastPos then
        Parts.Add(Copy(Source, LastPos, TokStart - LastPos));

      Parts.Add(NewName);
      LastPos := TokEnd;
    end;

    if LastPos <= Length(Source) then
      Parts.Add(Copy(Source, LastPos, Length(Source) - LastPos + 1));

    Result := '';
    for I := 0 to Parts.Count - 1 do
      Result := Result + Parts[I];
  finally
    Parts.Free;
  end;
end;

function RenameAllTokenOccurrences(const Source, OldName, NewName: string;
  out OccurrenceCount: Integer): string;
var
  Lexer: TmwPasLex;
  Parts: TStringList;
  LastPos, TokPos, TokLen: Integer;
begin
  OccurrenceCount := 0;
  if SameText(OldName, NewName) and (OldName = NewName) then begin
    Result := Source;
    Exit;
  end;

  Lexer := TmwPasLex.Create;
  Parts := TStringList.Create;
  try
    LastPos := 1;
    Lexer.Origin := PChar(Source);
    while Lexer.TokenID <> tkNull do begin
      if (Lexer.TokenID in IdentDirect) and SameText(Lexer.Token, OldName) then begin
        TokPos := Lexer.TokenPos + 1; // 1-based
        TokLen := Lexer.RunPos - Lexer.TokenPos;
        if TokPos > LastPos then
          Parts.Add(Copy(Source, LastPos, TokPos - LastPos));
        Parts.Add(NewName);
        LastPos := TokPos + TokLen;
        Inc(OccurrenceCount);
      end;
      Lexer.Next;
    end;
    if LastPos <= Length(Source) then
      Parts.Add(Copy(Source, LastPos, Length(Source) - LastPos + 1));

    Result := '';
    if OccurrenceCount > 0 then begin
      for TokLen := 0 to Parts.Count - 1 do
        Result := Result + Parts[TokLen];
    end else
      Result := Source;
  finally
    Parts.Free;
    Lexer.Free;
  end;
end;

function RenameIdentifierInSource(
  const Source: string;
  CursorPos: Integer;
  const NewName: string;
  out RenamedSource: string;
  out OccurrenceCount: Integer;
  out OldName: string;
  out ScopeDesc: string;
  out IdentInfo: TIdentifierInfo;
  out ErrorMsg: string
): Boolean;
var
  Tokens: TTokenArray;
  Procs: TProcScopeArray;
  Classes: TClassScopeArray;
  CursorTokenIdx: Integer;
  Scope: TRenameScope;
  Occurrences: TList;
begin
  Result := False;
  RenamedSource := Source;
  OccurrenceCount := 0;
  OldName := '';
  ScopeDesc := '';
  IdentInfo.Kind := ikUnknown;
  IdentInfo.TypeName := '';
  IdentInfo.KindDesc := '';
  IdentInfo.ScopeDesc := '';
  ErrorMsg := '';

  Tokens := TokenizeSource(Source);
  if Length(Tokens) = 0 then begin
    ErrorMsg := 'No tokens found in source';
    Exit;
  end;

  CursorTokenIdx := FindTokenAtCursor(Tokens, CursorPos - 1);
  if CursorTokenIdx < 0 then begin
    ErrorMsg := 'No identifier at cursor position';
    Exit;
  end;

  OldName := Tokens[CursorTokenIdx].Text;

  if (NewName <> '') and SameText(NewName, OldName) and (NewName = OldName) then begin
    ErrorMsg := 'New name is the same as the old name';
    Exit;
  end;

  // Build class scopes first — needed by BuildProcScopes to skip class interiors
  Classes := BuildClassScopes(Tokens);
  Procs := BuildProcScopes(Tokens, Classes);

  Scope := DetectScope(Tokens, CursorTokenIdx, OldName, Procs, Classes);
  ScopeDesc := Scope.Description;

  IdentInfo := DetectIdentifierKind(Tokens, Scope, Procs, Classes, OldName, CursorTokenIdx);

  // Special case: renaming a function/method name to 'Result' should only
  // rename assignment-target occurrences inside the function body, not
  // the declaration, class member, or recursive call sites.
  if (NewName <> '') and SameText(NewName, 'Result') and (IdentInfo.Kind = ikProcedure) then
    Occurrences := CollectFuncNameAsResultOccurrences(Tokens, OldName, Procs, Classes, Scope)
  else
    Occurrences := CollectOccurrences(Tokens, OldName, Scope, Procs, Classes);
  try
    OccurrenceCount := Occurrences.Count;

    if OccurrenceCount = 0 then begin
      ErrorMsg := 'No occurrences found';
      Exit;
    end;

    if NewName = '' then begin
      Result := True;
      Exit;
    end;

    RenamedSource := ApplyReplacements(Source, Tokens, Occurrences, NewName);
    Result := True;
  finally
    Occurrences.Free;
  end;
end;

function CheckNameConflict(
  const Source: string;
  CursorPos: Integer;
  const NewName: string;
  out ConflictDesc: string
): Boolean;
var
  Tokens: TTokenArray;
  Procs: TProcScopeArray;
  Classes: TClassScopeArray;
  CursorTokenIdx: Integer;
  OldName: string;
  Scope: TRenameScope;
begin
  Result := False;
  ConflictDesc := '';

  if NewName = '' then
    Exit;

  Tokens := TokenizeSource(Source);
  if Length(Tokens) = 0 then
    Exit;

  CursorTokenIdx := FindTokenAtCursor(Tokens, CursorPos - 1);
  if CursorTokenIdx < 0 then
    Exit;

  OldName := Tokens[CursorTokenIdx].Text;
  if SameText(NewName, OldName) then
    Exit;

  Classes := BuildClassScopes(Tokens);
  Procs := BuildProcScopes(Tokens, Classes);
  Scope := DetectScope(Tokens, CursorTokenIdx, OldName, Procs, Classes);

  Result := FindIdentInScope(Tokens, NewName, Scope, Procs, Classes, CursorTokenIdx, ConflictDesc);
end;

end.
