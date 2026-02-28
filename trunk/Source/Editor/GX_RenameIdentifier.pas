/// Core logic for the Rename Identifier expert.
/// This unit has no OTA dependency and operates on plain strings,
/// making it fully testable with DUnit.
unit GX_RenameIdentifier;

{$I GX_CondDefine.inc}

interface

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
  out ErrorMsg: string
): Boolean;

implementation

uses
  SysUtils, Classes,
  mPasLex, mwPasParserTypes;

type
  TTokenRec = record
    Kind: TTokenKind;
    Pos: Integer;   // 0-based offset in source
    Len: Integer;
    Text: string;   // stored for identifiers; empty for non-identifiers
  end;
  TTokenArray = array of TTokenRec;

  TScopeKind = (skUnit, skClass, skProcedure);

  TProcScope = record
    Name: string;
    ClassName: string;
    HeaderStart: Integer;  // token index
    BodyStart: Integer;    // token index of 'begin' (or -1)
    BodyEnd: Integer;      // token index of matching 'end' (or -1)
  end;
  TProcScopeArray = array of TProcScope;

  TClassScope = record
    Name: string;
    DeclStart: Integer;    // token index of 'class'/'record' keyword
    DeclEnd: Integer;      // token index of matching 'end'
  end;
  TClassScopeArray = array of TClassScope;

  TRenameScope = record
    Kind: TScopeKind;
    Description: string;
    ProcIndex: Integer;
    ClassIndex: Integer;
  end;

function TokenizeSource(const Source: string): TTokenArray;
var
  Lexer: TmwPasLex;
  Count, Capacity: Integer;
  Tok: TTokenRec;
begin
  Capacity := 1024;
  SetLength(Result, Capacity);
  Count := 0;

  Lexer := TmwPasLex.Create;
  try
    Lexer.Origin := PChar(Source);
    while Lexer.TokenID <> tkNull do begin
      Tok.Kind := Lexer.TokenID;
      Tok.Pos := Lexer.TokenPos;
      Tok.Len := Lexer.RunPos - Lexer.TokenPos;
      if Tok.Kind in IdentDirect then
        Tok.Text := Lexer.Token
      else
        Tok.Text := '';

      if Count >= Capacity then begin
        Capacity := Capacity * 2;
        SetLength(Result, Capacity);
      end;
      Result[Count] := Tok;
      Inc(Count);

      Lexer.Next;
    end;
  finally
    Lexer.Free;
  end;
  SetLength(Result, Count);
end;

function FindTokenAtCursor(const Tokens: TTokenArray; CursorPos0: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Tokens) do begin
    if (CursorPos0 >= Tokens[I].Pos) and (CursorPos0 < Tokens[I].Pos + Tokens[I].Len) then begin
      if Tokens[I].Kind in IdentDirect then begin
        Result := I;
        Exit;
      end;
      Result := -1;
      Exit;
    end;
  end;
  Result := -1;
end;

function IsStructKeyword(Kind: TTokenKind): Boolean;
begin
  Result := Kind in [tkClass, tkRecord, tkObject, tkInterface, tkDispinterface];
end;

function IsProcKeyword(Kind: TTokenKind): Boolean;
begin
  Result := Kind in [tkProcedure, tkFunction, tkConstructor, tkDestructor];
end;

function PrevNonJunk(const Tokens: TTokenArray; I: Integer): Integer;
begin
  Result := I - 1;
  while (Result >= 0) and (Tokens[Result].Kind in [tkSpace, tkCRLF, tkCRLFCo,
    tkAnsiComment, tkBorComment, tkSlashesComment, tkCompDirect]) do
    Dec(Result);
end;

function NextNonJunk(const Tokens: TTokenArray; I: Integer): Integer;
begin
  Result := I + 1;
  while (Result <= High(Tokens)) and (Tokens[Result].Kind in [tkSpace, tkCRLF, tkCRLFCo,
    tkAnsiComment, tkBorComment, tkSlashesComment, tkCompDirect]) do
    Inc(Result);
end;

function BuildClassScopes(const Tokens: TTokenArray): TClassScopeArray;
var
  I, J, NestLevel, Count, Capacity: Integer;
  Cls: TClassScope;
  PrevI, NameI, NextI: Integer;
begin
  Capacity := 16;
  SetLength(Result, Capacity);
  Count := 0;

  I := 0;
  while I <= High(Tokens) do begin
    if IsStructKeyword(Tokens[I].Kind) then begin
      PrevI := PrevNonJunk(Tokens, I);
      if (PrevI < 0) or (Tokens[PrevI].Kind <> tkEqual) then begin
        Inc(I);
        Continue;
      end;
      NameI := PrevNonJunk(Tokens, PrevI);
      if (NameI < 0) or not (Tokens[NameI].Kind in IdentDirect) then begin
        Inc(I);
        Continue;
      end;

      NextI := NextNonJunk(Tokens, I);
      // Forward declaration: class;
      if (NextI <= High(Tokens)) and (Tokens[NextI].Kind = tkSemiColon) then begin
        Inc(I);
        Continue;
      end;
      // Class reference: class of
      if (NextI <= High(Tokens)) and (Tokens[NextI].Kind = tkOf) then begin
        Inc(I);
        Continue;
      end;

      Cls.Name := Tokens[NameI].Text;
      Cls.DeclStart := I;

      // Find matching 'end'
      NestLevel := 1;
      J := I + 1;
      while (J <= High(Tokens)) and (NestLevel > 0) do begin
        if IsStructKeyword(Tokens[J].Kind) then begin
          PrevI := PrevNonJunk(Tokens, J);
          if (PrevI >= 0) and (Tokens[PrevI].Kind = tkEqual) then begin
            // Check not forward
            NextI := NextNonJunk(Tokens, J);
            if not ((NextI <= High(Tokens)) and (Tokens[NextI].Kind = tkSemiColon)) then
              Inc(NestLevel);
          end;
        end else if Tokens[J].Kind = tkEnd then
          Dec(NestLevel);
        if NestLevel > 0 then
          Inc(J);
      end;
      Cls.DeclEnd := J;

      if Count >= Capacity then begin
        Capacity := Capacity * 2;
        SetLength(Result, Capacity);
      end;
      Result[Count] := Cls;
      Inc(Count);

      I := J + 1;
    end else
      Inc(I);
  end;
  SetLength(Result, Count);
end;

function IsTokenInsideAnyClass(TokenIndex: Integer; const Classes: TClassScopeArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Classes) do begin
    if (TokenIndex >= Classes[I].DeclStart) and (TokenIndex <= Classes[I].DeclEnd) then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function MatchBeginEnd(const Tokens: TTokenArray; BeginIdx: Integer): Integer;
// Given the index of a 'begin' token, find its matching 'end'.
// Returns the index of the matching 'end', or High(Tokens)+1 if not found.
var
  J, NestLevel, PrevI: Integer;
begin
  NestLevel := 1;
  J := BeginIdx + 1;
  while (J <= High(Tokens)) and (NestLevel > 0) do begin
    case Tokens[J].Kind of
      tkBegin, tkCase, tkTry, tkAsm: Inc(NestLevel);
      tkRecord: begin
        PrevI := PrevNonJunk(Tokens, J);
        if (PrevI >= 0) and (Tokens[PrevI].Kind = tkEqual) then
          Inc(NestLevel);
      end;
      tkEnd: Dec(NestLevel);
    end;
    if NestLevel > 0 then
      Inc(J);
  end;
  Result := J;
end;

procedure ScanProcBody(const Tokens: TTokenArray; const Classes: TClassScopeArray;
  StartIdx: Integer; out BodyStart, BodyEnd: Integer;
  var ProcList: TProcScopeArray; var ProcCount, ProcCapacity: Integer);
// Scan forward from StartIdx (which is right after the header semicolon)
// looking for the proc body 'begin..end', skipping nested procs recursively.
// Nested procs are recorded in ProcList.
var
  J, NestLevel, NameJ: Integer;
  NestedBodyStart, NestedBodyEnd: Integer;
  NestedProc: TProcScope;
begin
  BodyStart := -1;
  BodyEnd := -1;
  J := StartIdx;
  while J <= High(Tokens) do begin
    if Tokens[J].Kind = tkForward then
      Exit;
    if Tokens[J].Kind = tkExternal then
      Exit;
    if IsProcKeyword(Tokens[J].Kind) and not IsTokenInsideAnyClass(J, Classes) then begin
      // Nested procedure — record it and skip its header and body
      NestedProc.HeaderStart := J;
      NestedProc.BodyStart := -1;
      NestedProc.BodyEnd := -1;
      NestedProc.Name := '';
      NestedProc.ClassName := '';

      // Parse nested proc name
      NameJ := NextNonJunk(Tokens, J);
      if (NameJ <= High(Tokens)) and (Tokens[NameJ].Kind in IdentDirect) then begin
        NestedProc.Name := Tokens[NameJ].Text;
        NameJ := NextNonJunk(Tokens, NameJ);
        if (NameJ <= High(Tokens)) and (Tokens[NameJ].Kind = tkPoint) then begin
          NameJ := NextNonJunk(Tokens, NameJ);
          if (NameJ <= High(Tokens)) and (Tokens[NameJ].Kind in IdentDirect) then begin
            NestedProc.ClassName := NestedProc.Name;
            NestedProc.Name := Tokens[NameJ].Text;
          end;
        end;
      end;

      // Skip to header semicolon
      NestLevel := 0;
      Inc(J);
      while J <= High(Tokens) do begin
        if Tokens[J].Kind = tkRoundOpen then
          Inc(NestLevel)
        else if Tokens[J].Kind = tkRoundClose then
          Dec(NestLevel)
        else if (Tokens[J].Kind = tkSemiColon) and (NestLevel = 0) then
          Break;
        Inc(J);
      end;
      Inc(J); // past semicolon

      // Recursively find the nested proc's body (and any deeper nested procs)
      ScanProcBody(Tokens, Classes, J, NestedBodyStart, NestedBodyEnd,
        ProcList, ProcCount, ProcCapacity);
      NestedProc.BodyStart := NestedBodyStart;
      NestedProc.BodyEnd := NestedBodyEnd;

      // Add nested proc to the list
      if ProcCount >= ProcCapacity then begin
        ProcCapacity := ProcCapacity * 2;
        SetLength(ProcList, ProcCapacity);
      end;
      ProcList[ProcCount] := NestedProc;
      Inc(ProcCount);

      if NestedBodyEnd >= 0 then
        J := NestedBodyEnd + 1
      else begin
        // Forward/external nested proc — skip to next semicolon
        while (J <= High(Tokens)) and (Tokens[J].Kind <> tkSemiColon) do
          Inc(J);
        Inc(J);
      end;
      Continue;
    end;
    if Tokens[J].Kind = tkBegin then begin
      BodyStart := J;
      BodyEnd := MatchBeginEnd(Tokens, J);
      Exit;
    end;
    Inc(J);
  end;
end;

function BuildProcScopes(const Tokens: TTokenArray; const Classes: TClassScopeArray): TProcScopeArray;
var
  I, J, NestLevel, Count, Capacity: Integer;
  Proc: TProcScope;
begin
  Capacity := 32;
  SetLength(Result, Capacity);
  Count := 0;

  I := 0;
  while I <= High(Tokens) do begin
    if IsProcKeyword(Tokens[I].Kind) and not IsTokenInsideAnyClass(I, Classes) then begin
      Proc.HeaderStart := I;
      Proc.BodyStart := -1;
      Proc.BodyEnd := -1;
      Proc.Name := '';
      Proc.ClassName := '';

      // Find the name
      J := NextNonJunk(Tokens, I);
      if (J <= High(Tokens)) and (Tokens[J].Kind in IdentDirect) then begin
        Proc.Name := Tokens[J].Text;
        J := NextNonJunk(Tokens, J);
        if (J <= High(Tokens)) and (Tokens[J].Kind = tkPoint) then begin
          J := NextNonJunk(Tokens, J);
          if (J <= High(Tokens)) and (Tokens[J].Kind in IdentDirect) then begin
            Proc.ClassName := Proc.Name;
            Proc.Name := Tokens[J].Text;
          end;
        end;
      end;

      // Skip to the semicolon after the header
      J := I + 1;
      NestLevel := 0;
      while J <= High(Tokens) do begin
        if Tokens[J].Kind = tkRoundOpen then
          Inc(NestLevel)
        else if Tokens[J].Kind = tkRoundClose then
          Dec(NestLevel)
        else if (Tokens[J].Kind = tkSemiColon) and (NestLevel = 0) then
          Break;
        Inc(J);
      end;

      // Find the body — ScanProcBody also records nested procs
      ScanProcBody(Tokens, Classes, J + 1, Proc.BodyStart, Proc.BodyEnd,
        Result, Count, Capacity);

      if Count >= Capacity then begin
        Capacity := Capacity * 2;
        SetLength(Result, Capacity);
      end;
      Result[Count] := Proc;
      Inc(Count);

      if Proc.BodyEnd >= 0 then
        I := Proc.BodyEnd + 1
      else begin
        // Skip to next semicolon (for forward/external)
        I := J + 1;
        while (I <= High(Tokens)) and (Tokens[I].Kind <> tkSemiColon) do
          Inc(I);
        Inc(I);
      end;
    end else
      Inc(I);
  end;
  SetLength(Result, Count);
end;

function FindEnclosingProcs(const Procs: TProcScopeArray; TokenIndex: Integer): TProcScopeArray;
var
  I, J, Count, Capacity: Integer;
  Temp: TProcScope;
  RangeI, RangeJ: Integer;
begin
  Capacity := 4;
  SetLength(Result, Capacity);
  Count := 0;

  for I := 0 to High(Procs) do begin
    if (Procs[I].BodyEnd >= 0) and
       (TokenIndex >= Procs[I].HeaderStart) and
       (TokenIndex <= Procs[I].BodyEnd) then begin
      if Count >= Capacity then begin
        Capacity := Capacity * 2;
        SetLength(Result, Capacity);
      end;
      Result[Count] := Procs[I];
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);

  // Sort by range size (smallest first = innermost)
  for I := 0 to Count - 2 do
    for J := I + 1 to Count - 1 do begin
      RangeI := Result[I].BodyEnd - Result[I].HeaderStart;
      RangeJ := Result[J].BodyEnd - Result[J].HeaderStart;
      if RangeI > RangeJ then begin
        Temp := Result[I];
        Result[I] := Result[J];
        Result[J] := Temp;
      end;
    end;
end;

function IsLocalDeclaration(const Tokens: TTokenArray; const Proc: TProcScope;
  const IdentName: string): Boolean;
var
  I, ParenLevel, NI: Integer;
  InVarSection, InConstSection, InTypeSection, InParamList: Boolean;
begin
  Result := False;
  InVarSection := False;
  InConstSection := False;
  InTypeSection := False;
  InParamList := False;
  ParenLevel := 0;

  I := Proc.HeaderStart;
  while (I <= High(Tokens)) and ((Proc.BodyStart < 0) or (I < Proc.BodyStart)) do begin
    case Tokens[I].Kind of
      tkRoundOpen: begin
        if ParenLevel = 0 then
          InParamList := True;
        Inc(ParenLevel);
      end;
      tkRoundClose: begin
        Dec(ParenLevel);
        if ParenLevel = 0 then
          InParamList := False;
      end;
      tkVar: begin
        if not InParamList then
          InVarSection := True;
        InConstSection := False;
        InTypeSection := False;
      end;
      tkConst: begin
        if not InParamList then begin
          InConstSection := True;
          InVarSection := False;
          InTypeSection := False;
        end;
      end;
      tkType: begin
        if not InParamList then begin
          InTypeSection := True;
          InVarSection := False;
          InConstSection := False;
        end;
      end;
      tkBegin, tkAsm:
        Break;
    end;

    if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName) then begin
      NI := NextNonJunk(Tokens, I);
      if InParamList then begin
        if (NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkComma, tkSemiColon, tkRoundClose]) then begin
          Result := True;
          Exit;
        end;
      end else if InVarSection or InConstSection then begin
        if (NI <= High(Tokens)) and (Tokens[NI].Kind in [tkColon, tkEqual, tkComma]) then begin
          Result := True;
          Exit;
        end;
      end else if InTypeSection then begin
        if (NI <= High(Tokens)) and (Tokens[NI].Kind = tkEqual) then begin
          Result := True;
          Exit;
        end;
      end;
    end;

    Inc(I);
  end;
end;

function IsClassMemberDecl(const Tokens: TTokenArray; const Cls: TClassScope;
  const IdentName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Cls.DeclStart to Cls.DeclEnd do begin
    if (Tokens[I].Kind in IdentDirect) and SameText(Tokens[I].Text, IdentName) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function DetectScope(const Tokens: TTokenArray; CursorTokenIdx: Integer;
  const IdentName: string; const Procs: TProcScopeArray;
  const Classes: TClassScopeArray): TRenameScope;
var
  EnclosingProcs: TProcScopeArray;
  I, J: Integer;
begin
  Result.Kind := skUnit;
  Result.Description := 'unit-level';
  Result.ProcIndex := -1;
  Result.ClassIndex := -1;

  // 1. Find all enclosing procedures (innermost first)
  EnclosingProcs := FindEnclosingProcs(Procs, CursorTokenIdx);

  // 2. Check local declarations in enclosing procedures
  for I := 0 to High(EnclosingProcs) do begin
    if IsLocalDeclaration(Tokens, EnclosingProcs[I], IdentName) then begin
      Result.Kind := skProcedure;
      for J := 0 to High(Procs) do
        if Procs[J].HeaderStart = EnclosingProcs[I].HeaderStart then begin
          Result.ProcIndex := J;
          Break;
        end;
      if EnclosingProcs[I].ClassName <> '' then
        Result.Description := 'local in ' + EnclosingProcs[I].ClassName + '.' + EnclosingProcs[I].Name
      else
        Result.Description := 'local in ' + EnclosingProcs[I].Name;
      Exit;
    end;
  end;

  // 3. Check class membership
  if Length(EnclosingProcs) > 0 then begin
    for I := 0 to High(EnclosingProcs) do begin
      if EnclosingProcs[I].ClassName <> '' then begin
        for J := 0 to High(Classes) do begin
          if SameText(Classes[J].Name, EnclosingProcs[I].ClassName) then begin
            if IsClassMemberDecl(Tokens, Classes[J], IdentName) then begin
              Result.Kind := skClass;
              Result.ClassIndex := J;
              Result.Description := 'member of ' + Classes[J].Name;
              Exit;
            end;
            Break;
          end;
        end;
      end;
    end;
  end;

  // 4. Check if cursor is directly inside a class declaration
  for I := 0 to High(Classes) do begin
    if (CursorTokenIdx >= Classes[I].DeclStart) and (CursorTokenIdx <= Classes[I].DeclEnd) then begin
      Result.Kind := skClass;
      Result.ClassIndex := I;
      Result.Description := 'member of ' + Classes[I].Name;
      Exit;
    end;
  end;
end;

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

function RenameIdentifierInSource(
  const Source: string;
  CursorPos: Integer;
  const NewName: string;
  out RenamedSource: string;
  out OccurrenceCount: Integer;
  out OldName: string;
  out ScopeDesc: string;
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

end.
