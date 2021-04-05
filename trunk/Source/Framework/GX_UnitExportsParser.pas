unit GX_UnitExportsParser;

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzErrorThread,
  u_dzCompilerAndRtlVersions,
  mwPasParserTypes,
  mPasLex,
  GX_UnitExportList;

{$DEFINE DEBUG_TIMING}

{$IFOPT D-}
{$UNDEF DEBUG_TIMING}
{$ENDIF}

type
  TSkipToElseOrEndifResult = (seeElse, seeEndif, seeElseIf, seeNull);

type
  TPasLexEx = class(TmwPasLex)
  private
    FSymbols: TStringList;
    FIfdefStack: TStringList;
    procedure Push(const _Condition: string);
    procedure Pop(out _Condition: string); overload;
    procedure Pop; overload;
    function TopOfStack: string;
    function GetSymbols: TStrings;
    procedure SetSymbols(const _Value: TStrings);
    function SkipToElseOrEndif(out _Expression: string): TSkipToElseOrEndifResult;
    ///<summary>
    /// @returns true, if the current token is {$ELSE}
    /// (allows additional text in the comment, so {$ENDIF bla} is also detected) </summary>
    function TokenIsElse: Boolean;
    ///<summary>
    /// @returns true, if the current token is either {$ENDIF} or {$IFEND}
    /// (allows additional text in the comment, so {$ENDIF bla} is also detected) </summary>
    function TokenIsEndif: Boolean;
    ///<summary>
    /// Checks for {$IFDEF <symbol>} or {$IF <expression>}
    /// @param Expression will return either be the defined(<symbol>) (for {$IFDEF <symbol>}
    ///        or the whole expression if the expression is more conplex
    /// @returns true, if the current tooken is {$IFDEF <symbol>} or {$IF <expression>}
    /// </summary>
    function TokenIsIfdef(out _Expression: string): Boolean;
    function TokenIsIfndef(out _Expression: string): Boolean;
    function TokenIsElseIf(out _Expression: string): Boolean;
    ///<summary>
    /// @returns true, if last token is <> tkNull </summary>
    function SkipToEndif: Boolean;
    function CheckExpression(const _Expression: string): Boolean;
    function Content: string;
  public
    constructor Create;
    destructor Destroy; override;
    function NextNoJunkEx: Boolean;
    property Symbols: TStrings read GetSymbols write SetSymbols;
  end;

type
  ///<summary>
  /// Simple parser that collects all identifiers declared in the interface of a unit. </summary>
  TUnitExportsParser = class
  private
    FFilename: string;
    FParser: TPasLexEx;
    FProcedures: TStrings;
    FFunctions: TStrings;
    FConstants: TStrings;
    FTypes: TStrings;
    FVariables: TStrings;
    FIdentifiers: TStrings;
    FIdentifierList: TUnitIdentifierList;
    FSymbols: TStrings;
    procedure AddToIdentifiers(const _Identifier: string; _IdType: TUnitIdentifierTypes; _LineNo: Integer);
    procedure AddToConsts(const _Token: string; _LineNo: Integer);
    procedure AddToFunctions(const _Token: string; _LineNo: Integer);
    procedure AddToProcedures(const _Token: string; _LineNo: Integer);
    procedure AddToTypes(const _Token: string; _LineNo: Integer);
    procedure AddToVars(const _Token: string; _LineNo: Integer);
    procedure SkipClassOrRecord;
    procedure SkipConstDeclaration;
    procedure SkipFunctionDeclaration;
    procedure SkipProcedureDeclaration;
    procedure SkipToClosingDelimiter(_OpeningDel, _ClosingDel: TTokenKind);
    procedure HandleTypeDeclaration;
    procedure SkipVarDeclaration;
    procedure SkipDirectives;
    function GetIdentifiers: TStrings;
  public
    ///<summary>
    /// adds the following symbols to the list as applicable:
    /// * VERxxx
    /// * UNICODE
    /// * NATIVECODE
    /// * DELPHILANGUAGE
    /// * BORLAND
    /// * BCB or DELPHI
    /// * DELPHIxx
    /// * DELPHICOMPILERxx
    /// * COMPILERxx
    /// * DELPHIxx_UP
    /// * DELPHICOMPILERxx_UP
    /// * COMPILERxx_UP
    /// * RTLxx
    /// * RTLxx_UP </summary>
    class procedure AddDefaultSymbols(_Symbols: TStrings);
    ///<summary>
    /// @returns the name of the given identifier type </summary>
    class function IdentfierTypeNames(_IdType: TUnitIdentifierTypes): string;
    ///<summary>
    /// constructs a TUnitExportsParser instance for the given file </summary>
    constructor Create(const _Filename: string);
    destructor Destroy; override;
    ///<summary>
    /// Parses the file and fills the various properties
    /// may raise an exception if the file cannot be opened </summary>
    procedure Execute;
    ///<summary>
    /// Sorted list of all exported procedures </summary>
    property Procedures: TStrings read FProcedures;
    ///<summary>
    /// Sorted list of all exported functions </summary>
    property Functions: TStrings read FFunctions;
    ///<summary>
    /// Sorted list of all exported constants </summary>
    property Constants: TStrings read FConstants;
    ///<summary>
    /// Sorted list of all exported variables </summary>
    property Variables: TStrings read FVariables;
    ///<summary>
    /// Sorted list of all exported types </summary>
    property Types: TStrings read FTypes;
    ///<summary>
    /// Sorted list of all exported identifiers, not containing any duplicates </summary>
    property Identifiers: TStrings read GetIdentifiers;
    ///<summary>
    /// Unsorted list of all exported identifiers as a TIdentifier record which
    /// in addition to the name contains the identifiert type. Note that this
    /// List can contain duplicates e.g. for overloaded functions. </summary>
    property IdentifierList: TUnitIdentifierList read FIdentifierList;
    ///<summary>
    /// Add any conditional symbols here </summary>
    property Symbols: TStrings read FSymbols;
  end;

type
  TUnitExportParserThread = class(TErrorThread)
  private
    FUnitFiles: TStringList;
    FFiles: TStringList;
    FIdentifierList: TUnitExportlist;
    FPaths: TStringList;
    FCacheDirBS: string;
    FParsedUnitsCount: Integer;
    FLoadedUnitsCount: Integer;
    FFoundUnitsCount: Integer;
    FSymbols: TStringList;
    procedure GetAllFilesInPath(_sl: TStringList);
    procedure GetAllFilesInDir(_dir: string; _sl: TStringList);
  protected
    procedure doExecute; override;
  public
    ///<summary>
    /// @param Files is a list of unit names, without path and extension, which are to be parsed.
    ///              Can be NIL, in which case all files in the search path will be parsed.
    ///              We could exclude the System unit here, but that won't save much time or memory
    ///              and would prevent the user from opening that unit to inspect the identifier
    ///              declaration.
    /// @param Paths is a list of possible search paths
    /// @param CacheDir is a directory to cache the identifier lists </summary>
    constructor Create(const _Files: TStrings; _Paths: TStrings; const _CacheDir: string; const _Symbols: TStrings);
    destructor Destroy; override;
    function DetachIdentifiers: TUnitExportlist;
    ///<summary>
    /// After execution Identifiers contains a sorted list of all identfiers. </summary>
    property Identifiers: TUnitExportlist read FIdentifierList;
    property FoundUnitsCount: Integer read FFoundUnitsCount;
    property ParsedUnitsCount: Integer read FParsedUnitsCount;
    property LoadedUnitsCount: Integer read FLoadedUnitsCount;
  end;

implementation

uses
  StrUtils,
  u_dzStringUtils,
{$IFDEF DEBUG_TIMING}
  u_dzStopwatch,
{$ENDIF}
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF}
  GX_GenericUtils;

{$IF not declared(TStopwatch)}
// TStopwatch requires enhanced records, so it's not available in Delphi < 2007)
{$UNDEF DEBUG_TIME_IDENTIFIER_TAB}
{$IFEND}

{ TPasLexEx }

constructor TPasLexEx.Create;
begin
  inherited Create;
  FSymbols := TStringList.Create;
  FSymbols.Sorted := True;
  FSymbols.Duplicates := dupIgnore;
  FSymbols.CaseSensitive := False;
  FIfdefStack := TStringList.Create;

  // call the following functions so they don't get elimiated by the linker
  TopOfStack;
  Content;
end;

destructor TPasLexEx.Destroy;
begin
  FreeAndNil(FIfdefStack);
  FreeAndNil(FSymbols);
  inherited;
end;

function TPasLexEx.GetSymbols: TStrings;
begin
  Result := FSymbols;
end;

procedure TPasLexEx.SetSymbols(const _Value: TStrings);
begin
  FSymbols.Assign(_Value);
end;

function TPasLexEx.CheckExpression(const _Expression: string): Boolean;
var
  s: string;
  Idx: Integer;
begin
  if StartsText('defined(', _Expression) then begin
    s := Trim(Copy(_Expression, 9, Length(_Expression) - 9));
    if (Pos('(', s) > 0) or (Pos(' ', s) > 0) then begin
      // we don't handle more complex expressions (yet)
      Result := False;
    end else begin
      // it's a simple defined(<symbol>), we can handle that
      Result := FSymbols.Find(s, Idx);
    end;
  end else begin
    // we don't handle more complex expressions (yet)
    Result := False;
  end;
end;

function TPasLexEx.TokenIsElse: Boolean;
var
  TheToken: string;
begin
  TheToken := Token;
  Result := SameText('{$else}', TheToken) or StartsText('{$else ', TheToken);
end;

function TPasLexEx.TokenIsElseIf(out _Expression: string): Boolean;
var
  TheToken: string;
begin
  TheToken := Token;
  Result := StartsText('{$elseif ', TheToken);
  if not Result then
    Exit; //==>

  _Expression := Trim(Copy(TheToken, 9, Length(TheToken) - 9));
end;

function TPasLexEx.TokenIsEndif: Boolean;
var
  TheToken: string;
begin
  TheToken := Token;
  Result := SameText('{$endif}', TheToken) or StartsText('{$endif ', TheToken)
    or SameText('{$ifend}', TheToken) or StartsText('{$ifend ', TheToken);
end;

function TPasLexEx.TokenIsIfdef(out _Expression: string): Boolean;
var
  TheToken: string;
  p: Integer;
begin
  TheToken := Token;
  Result := StartsText('{$ifdef ', TheToken);
  if Result then begin
    _Expression := Trim(Copy(TheToken, 9, Length(TheToken) - 9));
    p := Pos(' ', _Expression);
    if p > 0 then
      _Expression := Copy(_Expression, 1, p - 1);
    _Expression := 'defined(' + _Expression + ')';
    Exit; //==>
  end;

  if StartsText('{$if ', TheToken) then begin
    _Expression := Trim(Copy(TheToken, 5, Length(TheToken) - 5));
    Result := True;
  end;
end;

function TPasLexEx.TokenIsIfndef(out _Expression: string): Boolean;
var
  TheToken: string;
  p: Integer;
begin
  TheToken := Token;
  Result := StartsText('{$ifndef ', TheToken);
  if Result then begin
    _Expression := Trim(Copy(TheToken, 10, Length(TheToken) - 10));
    p := Pos(' ', _Expression);
    if p > 0 then
      _Expression := Copy(_Expression, 1, p - 1);
    _Expression := 'defined(' + _Expression + ')';
  end;
end;

function TPasLexEx.TopOfStack: string;
var
  cnt: Integer;
begin
  cnt := FIfdefStack.Count;
  if cnt > 0 then
    Result := FIfdefStack[cnt - 1]
  else
    Result := '<empty>';
end;

function TPasLexEx.Content: string;
begin
  if Assigned(Origin) then
    Result := Origin + RunPos
  else
    Result := '';
end;

function TPasLexEx.NextNoJunkEx: Boolean;
var
  Expression: string;
  ElseifExp: string;
  Res: TSkipToElseOrEndifResult;
begin
  Result := NextNoJunk;
  if not Result or (Tokenid <> tkCompDirect) then
    Exit; //==>

  if TokenIsIfdef(Expression) then begin
    if CheckExpression(Expression) then begin
      Push(Expression);
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end else begin
      Res := SkipToElseOrEndif(ElseifExp);
      while Res = seeElseIf do begin
        Pop;
        Push(ElseifExp);
        if CheckExpression(ElseifExp) then begin
          // it's possible that the next token again is a compiler directive, so recurse
          Result := NextNoJunkEx;
          Exit; //==>
        end;
        Res := SkipToElseOrEndif(ElseifExp);
      end;
      case Res of
        seeElse: begin
            Push('not ' + Expression);
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
        seeEndif: begin
            Pop;
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
      else // seeNull
        Result := False;
      end;
    end;
    Exit; //==>
  end;

  if TokenIsIfndef(Expression) then begin
    if not CheckExpression(Expression) then begin
      Push('not ' + Expression);
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end else begin
      Res := SkipToElseOrEndif(ElseifExp);
      while Res = seeElseIf do begin
        Pop;
        Push(ElseifExp);
        if CheckExpression(ElseifExp) then begin
          // it's possible that the next token again is a compiler directive, so recurse
          Result := NextNoJunkEx;
          Exit; //==>
        end;
        Res := SkipToElseOrEndif(ElseifExp);
      end;
      case Res of
        seeElse: begin
            Push(Expression);
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
        seeEndif: begin
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
      else // seeNull
        Result := False;
      end;
    end;
    Exit; //==>
  end;

  if TokenIsElse then begin
    // we have already parsed the if branch, so skip  to endif
    Result := SkipToEndif;
    if Result then begin
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end;
    Exit; //==>
  end;

  if TokenIsElseIf(Expression) then begin
    // if we get here, we have already parsed the if branch, so skip to endif
    Pop;
    Push(Expression);
    // we have already parsed the if branch, so skip  to endif
    Result := SkipToEndif;
    if Result then begin
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end;
    Exit; //==>
  end;

  if TokenIsEndif then begin
    // we have parsed the else branch, so we remove the symbol from the stack and continue
    Pop;
    // it's possible that the next token again is a compiler directive, so recurse
    Result := NextNoJunkEx;
    Exit; //==>
  end;
end;

procedure TPasLexEx.Pop(out _Condition: string);
var
  cnt: Integer;
begin
  cnt := FIfdefStack.Count;
  if cnt > 0 then begin
    _Condition := FIfdefStack[cnt - 1];
    FIfdefStack.Delete(cnt - 1);
  end;
end;

procedure TPasLexEx.Pop;
var
  Condition: string;
begin
  Pop(Condition);
end;

procedure TPasLexEx.Push(const _Condition: string);
begin
  FIfdefStack.Add(_Condition);
end;

function TPasLexEx.SkipToEndif: Boolean;
var
  Expression: string;
begin
  Result := True;
  while NextNoJunk do begin
    if Tokenid = tkCompDirect then begin
      if TokenIsEndif then
        Exit; //==>
      if TokenIsIfdef(Expression) then begin
        // nested $if
        SkipToEndif;
      end;
    end;
  end;
  Result := False;
end;

function TPasLexEx.SkipToElseOrEndif(out _Expression: string): TSkipToElseOrEndifResult;
var
  Expression: string;
begin
  while NextNoJunk do begin
    if Tokenid = tkCompDirect then begin
      if TokenIsElse then begin
        Result := seeElse;
        Exit; //==>
      end;
      if TokenIsEndif then begin
        Result := seeEndif;
        Exit; //==>
      end;
      if TokenIsElseIf(_Expression) then begin
        Result := seeElseIf;
        Exit; //==>
      end;

      if TokenIsIfdef(Expression) or TokenIsIfndef(Expression) then begin
        // nested $if
        SkipToEndif;
      end;
    end;
  end;
  Result := seeNull;
end;

{ TUnitExportsParser }

class function TUnitExportsParser.IdentfierTypeNames(_IdType: TUnitIdentifierTypes): string;
begin
  case _IdType of
    itConst: Result := 'const';
    itType: Result := 'type';
    itVar: Result := 'var';
    itProcedure: Result := 'procedure';
    itFunction: Result := 'function';
  else // itUnknown:
    Result := 'unknown';
  end;
end;

procedure CreateSortedStringsNoDuplicates(var _st: TStrings);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Sorted := True;
  sl.Duplicates := dupIgnore;
  _st := sl;
end;

constructor TUnitExportsParser.Create(const _Filename: string);
begin
  inherited Create;
  FFilename := _Filename;
  CreateSortedStringsNoDuplicates(FProcedures);
  CreateSortedStringsNoDuplicates(FFunctions);
  CreateSortedStringsNoDuplicates(FConstants);
  CreateSortedStringsNoDuplicates(FVariables);
  CreateSortedStringsNoDuplicates(FTypes);
  FIdentifierList := TUnitIdentifierList.Create(500);
  CreateSortedStringsNoDuplicates(FSymbols);
end;

destructor TUnitExportsParser.Destroy;
begin
  FreeAndNil(FSymbols);
  FreeAndNil(FProcedures);
  FreeAndNil(FFunctions);
  FreeAndNil(FConstants);
  FreeAndNil(FVariables);
  FreeAndNil(FTypes);
  FreeAndNil(FIdentifiers);
  FreeAndNil(FIdentifierList);
  inherited;
end;

type
  TDeclarationType = (dtNone, dtConst, dtType, dtVar, dtFunction, dtProcedure);

procedure TUnitExportsParser.Execute;
var
  sl: TStringList;
  s: string;
  DeclarationType: TDeclarationType;
begin
  sl := nil;
  FParser := TPasLexEx.Create;
  try
    FParser.Symbols := FSymbols;
    sl := TStringList.Create;
    sl.LoadFromFile(FFilename);
    s := sl.Text;
    if s <> '' then begin
      FParser.Origin := @s[1];
      DeclarationType := dtNone;
      while FParser.Tokenid <> tkNull do begin
        if FParser.Tokenid = tkImplementation then
          Exit;
        case FParser.Tokenid of
          tkSquareOpen: begin
              // attribute declarations
              SkipToClosingDelimiter(tkSquareOpen, tkSquareClose);
            end;
          tkConst: begin
              DeclarationType := dtConst;
            end;
          tkVar: begin
              DeclarationType := dtVar;
            end;
          tkType: begin
              DeclarationType := dtType;
            end;
          tkIdentifier: begin
              case DeclarationType of
                dtNone: ;
                dtConst: begin
                    AddToConsts(FParser.Token, FParser.LineNumber);
                    SkipConstDeclaration;
                  end;
                dtType: begin
                    AddToTypes(FParser.Token, FParser.LineNumber);
                    HandleTypeDeclaration;
                  end;
                dtVar: begin
                    AddToVars(FParser.Token, FParser.LineNumber);
                    SkipVarDeclaration;
                  end;
                dtFunction: begin
                    AddToFunctions(FParser.Token, FParser.LineNumber);
                    SkipFunctionDeclaration;
                  end;
                dtProcedure: begin
                    AddToProcedures(FParser.Token, FParser.LineNumber);
                    SkipProcedureDeclaration
                  end;
              end;
            end;
          tkProcedure: begin
              DeclarationType := dtProcedure;
            end;
          tkFunction: begin
              DeclarationType := dtFunction;
            end;
        end;
        FParser.NextNoJunkEx;
      end;
    end;
  finally
    FreeAndNil(sl);
    FreeAndNil(FParser);
  end;
end;

function TUnitExportsParser.GetIdentifiers: TStrings;
var
  i: Integer;
begin
  if not Assigned(FIdentifiers) then begin
    CreateSortedStringsNoDuplicates(FIdentifiers);
    for i := 0 to FIdentifierList.Count - 1 do
      FIdentifiers.Add(FIdentifierList[i].Identifier);
  end;
  Result := FIdentifiers;
end;

procedure TUnitExportsParser.AddToIdentifiers(const _Identifier: string; _IdType: TUnitIdentifierTypes; _LineNo: Integer);
begin
  FIdentifierList.Add(_Identifier, _LineNo);
end;

class procedure TUnitExportsParser.AddDefaultSymbols(_Symbols: TStrings);
var
  CompVer: Integer;
  DelphiVer: Integer;
  RtlVer: Integer;
begin
  // the usual VERxxx symbol
  _Symbols.Add(Format('VER%.0f', [CompilerVersion * 10]));
  _Symbols.Add('CONDITIONALEXPRESSIONS');
  _Symbols.Add('NATIVECODE');

  // add commonly used symbols
  // 1. jedi.inc
  // todo: Maybe check if jedi.inc is included?
  _Symbols.Add('DELPHILANGUAGE');
  _Symbols.Add('BORLAND');
{$IFDEF BCB}
  Symbols.Add('BCB');
{$ELSE}
  _Symbols.Add('DELPHI');
{$ENDIF}

  DelphiVer := Round(CompilerVersion) - CompilerVersionDelphi6 + 6;
  _Symbols.Add(Format('DELPHI%d', [DelphiVer]));
  _Symbols.Add(Format('DELPHICOMPILER%d', [DelphiVer]));
  _Symbols.Add(Format('COMPILER%d', [DelphiVer]));

  for CompVer := CompilerVersionDelphi6 to Round(CompilerVersion) do begin
    DelphiVer := CompVer - CompilerVersionDelphi6 + 6;
    _Symbols.Add(Format('DELPHI%d_UP', [DelphiVer]));
    _Symbols.Add(Format('DELPHICOMPILER%d_UP', [DelphiVer]));
    _Symbols.Add(Format('COMPILER%d_UP', [DelphiVer]));
  end;

  if (CompilerVersion >= CompilerVersionDelphi2005) and (CompilerVersion <= CompilerVersionDelphi2010) then begin
    DelphiVer := Round(CompilerVersion) - CompilerVersionDelphi2005 + 2005;
    _Symbols.Add(Format('DELPHI%d', [DelphiVer]));
  end;
  for CompVer := CompilerVersionDelphi2005 to CompilerVersionDelphi2007Net do begin
    if CompilerVersion >= CompVer then begin
      DelphiVer := Round(CompVer) - CompilerVersionDelphi2005 + 2005;
      _Symbols.Add(Format('DELPHI%d_UP', [DelphiVer]));
    end;
  end;
  if CompilerVersion = CompilerVersionDelphi2007 then begin
    _Symbols.Add('DELPHI2007');
  end;
  if CompilerVersion >= CompilerVersionDelphi2007 then begin
    _Symbols.Add('DELPHI2007_UP');
  end;

  for CompVer := CompilerVersionDelphi2009 to CompilerVersionDelphi2010 do begin
    if CompilerVersion >= CompVer then begin
      DelphiVer := Round(CompVer) - CompilerVersionDelphi2009 + 2009;
      _Symbols.Add(Format('DELPHI%d_UP', [DelphiVer]));
    end;
  end;

  if CompilerVersion = CompilerVersionDelphiXE then begin
    _Symbols.Add('DELPHIXE');
  end;
  if CompilerVersion >= CompilerVersionDelphiXE then begin
    _Symbols.Add('DELPHIXE_UP');
  end;

  if (CompilerVersion >= CompilerVersionDelphiXE2) and (CompilerVersion <= CompilerVersionDelphiXE8) then begin
    DelphiVer := Round(CompilerVersion) - CompilerVersionDelphiXE2 + 2;
    _Symbols.Add(Format('DELPHIXE%d', [DelphiVer]));
  end;
  for CompVer := CompilerVersionDelphiXE2 to CompilerVersionDelphiXE8 do begin
    if CompilerVersion >= CompVer then begin
      DelphiVer := Round(CompVer) - CompilerVersionDelphiXE2 + 2;
      _Symbols.Add(Format('DELPHIXE%d_UP', [DelphiVer]));
    end;
  end;

  _Symbols.Add(Format('RTL%d', [Round(RTLVersion)]));
  for RtlVer := RtlVersionDelphi6 to Round(RTLVersion) do begin
    _Symbols.Add(Format('RTL%D_UP', [RtlVer]));
  end;

{$IFDEF UNICODE}
  // from
  // http://docwiki.embarcadero.com/RADStudio/Sydney/en/Conditional_compilation_(Delphi)
  // I gather that all target platforms of Unicode aware versions of Delphi are also Unicode aware
  _Symbols.Add('UNICODE');
{$ENDIF}
  // todo: Handle the symbols defined by the target somehow
  // {$ifdef WINDOWS}
  //  _Symbols.Add('WINDOWS');
  // {$ENDIF}
  // todo: add symbol WIN32 or WIN64 and possibly MS_WINDOWS or LINUX or IOS, ANDROID
end;

procedure TUnitExportsParser.AddToConsts(const _Token: string; _LineNo: Integer);
begin
  FConstants.Add(_Token);
  AddToIdentifiers(_Token, itConst, _LineNo);
end;

procedure TUnitExportsParser.AddToVars(const _Token: string; _LineNo: Integer);
begin
  FVariables.Add(_Token);
  AddToIdentifiers(_Token, itVar, _LineNo);
end;

procedure TUnitExportsParser.AddToTypes(const _Token: string; _LineNo: Integer);
begin
  FTypes.Add(_Token);
  AddToIdentifiers(_Token, itType, _LineNo);
end;

procedure TUnitExportsParser.AddToFunctions(const _Token: string; _LineNo: Integer);
begin
  FFunctions.Add(_Token);
  AddToIdentifiers(_Token, itFunction, _LineNo);
end;

procedure TUnitExportsParser.AddToProcedures(const _Token: string; _LineNo: Integer);
begin
  FProcedures.Add(_Token);
  AddToIdentifiers(_Token, itProcedure, _LineNo);
end;

procedure TUnitExportsParser.SkipToClosingDelimiter(_OpeningDel, _ClosingDel: TTokenKind);
begin
  while FParser.NextNoJunkEx do begin
    if FParser.Tokenid = _ClosingDel then begin
      // we have found the closing delimiter
      Exit; //==>
    end;
    if FParser.Tokenid = _OpeningDel then
      // we found another opening delimiter
      SkipToClosingDelimiter(_OpeningDel, _ClosingDel);
  end;
end;

procedure TUnitExportsParser.SkipProcedureDeclaration;
begin
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkRoundOpen then begin
    SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    FParser.NextNoJunkEx;
  end;
  while FParser.Tokenid <> tkNull do begin
    if FParser.Tokenid = tkSemiColon then begin
      SkipDirectives;
      Exit; //==>
    end;
    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.SkipClassOrRecord;
begin
  while FParser.Tokenid <> tkNull do begin
    case FParser.Tokenid of
      tkEnd:
        Exit; //==>
      tkLower: begin
          SkipToClosingDelimiter(tkLower, tkGreater);
        end;
      tkClass: begin
          FParser.NextNoJunkEx;
          if not (FParser.Tokenid in [tkFunction, tkProcedure, tkConstructor, tkDestructor, tkOperator,
              tkVar, tkThreadvar, tkProperty, tkOf, tkSemiColon]) then begin
            // nested class declaration
            if FParser.Tokenid = tkRoundOpen then begin
              // class declaration with ancestor
              SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
              FParser.NextNoJunkEx;
              if FParser.Tokenid = tkSemiColon then
                // type class bla(blub);
              else
                SkipClassOrRecord;
            end else
              SkipClassOrRecord;
          end;
        end;
      tkRecord,
        tkInterface: begin
          FParser.NextNoJunkEx;
          // nested record/interface declaration
          SkipClassOrRecord;
        end;
    end;
    // todo: handle more complex declarations
    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.HandleTypeDeclaration;
begin
  // this mostly just skips the type declaration but adds enum identifiers to the identifier list
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkLower then begin
    // type bla<tresult>
    SkipToClosingDelimiter(tkLower, tkGreater);
    FParser.NextNoJunkEx;
  end;
  if FParser.Tokenid <> tkEqual then begin
    // todo: this is an error -> handle it gracefully somehow
    Exit; //==>
  end;
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkRoundOpen then begin
    // enum declaration -> add the enum identifiers
    FParser.NextNoJunkEx;
    while FParser.Tokenid <> tkRoundClose do begin
      if FParser.Tokenid = tkNull then
        Exit; //==>
      if FParser.Tokenid = tkIdentifier then begin
        AddToIdentifiers(FParser.Token, itConst, FParser.LineNumber);
      end;
      FParser.NextNoJunkEx;
    end;
  end;
  if FParser.Tokenid = tkPacked then
    FParser.NextNoJunkEx;
  while FParser.Tokenid <> tkNull do begin
    case FParser.Tokenid of
      tkSemiColon: begin
          // we have reached the end of the type declaration
          Exit; //==>
        end;
      tkInterface, tkDispinterface, tkClass: begin
          FParser.NextNoJunkEx;
          if FParser.Tokenid = tkAbstract then begin
            FParser.NextNoJunkEx;
          end;
          if FParser.Tokenid = tkSemiColon then begin
            // forward declaration: type Tbla = class;
            //                  or: type Tbla = interface;
            Exit; //==>
          end else if FParser.Tokenid = tkRoundOpen then begin
            SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
            FParser.NextNoJunkEx;
            if FParser.Tokenid = tkSquareOpen then begin
              // interface(bla)[guid]
              SkipToClosingDelimiter(tkSquareOpen, tkSquareClose);
              FParser.NextNoJunkEx;
            end;
            if FParser.Tokenid = tkSemiColon then begin
              // simple declaration: type Tbla = class(Tblub); as used for e.g. exceptions or forward declarations
              Exit; //==>
            end;
          end else if FParser.Tokenid = tkSquareOpen then begin
            // interface[guid]
            SkipToClosingDelimiter(tkSquareOpen, tkSquareClose);
            FParser.NextNoJunkEx;
            if FParser.Tokenid = tkSemiColon then begin
              // simple declaration: type Tbla = class(Tblub); as used for e.g. exceptions or forward declarations
              Exit; //==>
            end;
          end else if FParser.Tokenid = tkOf then begin
            // TBla = class of Tblub;
            FParser.NextNoJunkEx;
            while FParser.Tokenid <> tkNull do begin
              if FParser.Tokenid = tkSemiColon then
                Exit; //==>
              FParser.NextNoJunkEx;
            end;
            // should never happen
            Exit; //==>
          end;
          SkipClassOrRecord;
          Exit; //==>
        end;
      tkRecord: begin
          FParser.NextNoJunkEx;
          if FParser.Tokenid = tkSemiColon then begin
            // forward declaration: type Tbla = record;
            Exit; //==>
          end;
          SkipClassOrRecord;
          Exit; //==>
        end;
      tkProcedure: begin
          // type bla = procedure(...);
          // type bla = procedure(...) of object;
          SkipProcedureDeclaration;
          Exit; //==>
        end;
      tkFunction: begin
          // type bla = function(...): Sometype;
          // type bla = function(...): Sometype of object;
          SkipFunctionDeclaration;
          Exit; //==>
        end;
    end;

    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.SkipConstDeclaration;
begin
  FParser.NextNoJunkEx;
  while FParser.Tokenid <> tkNull do begin
    case FParser.Tokenid of
      tkSquareOpen: begin
          SkipToClosingDelimiter(tkSquareOpen, tkSquareClose);
        end;
      tkRoundOpen: begin
          SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
        end;
      tkSemiColon: begin
          // found the end
          Exit; //==>
        end;
    end;
    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.SkipVarDeclaration;
begin
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkComma then begin
    // multiple variables in one declaration
    Exit; //==>
  end;
  while FParser.Tokenid <> tkNull do begin
    if FParser.Tokenid = tkRoundOpen then begin
      SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    end else if FParser.Tokenid = tkSemiColon then
      Exit; //==>
    FParser.NextNoJunkEx;
  end;
end;

// skip function or procedure declaration directives:
// * calling convention: Register, Pascal, cdecl, StdCall, Savecall
// * varargs
// * external with optionally a string (constant or literal)
//   * optionally followed by name and and an string (constant or literal) -- or --
//   * optionally followed by index and a number
//   * optionally followed by delayed

procedure TUnitExportsParser.SkipDirectives;
const
  CallingConventions = [tkRegister, tkPascal, tkCdecl, tkStdcall, tkSafecall];
var
  ParserState: TmwPasLexState;

  function SkipToSemicolon: Boolean;
  begin
    Result := False;
    while FParser.NextNoJunkEx do begin
      Result := (FParser.Tokenid = tkSemiColon);
      if Result then begin
        FParser.GetParsingState(ParserState);
        Exit; //==>
      end;
    end;
  end;

begin
  FParser.GetParsingState(ParserState);
  if not FParser.NextNoJunkEx then begin
    // todo: this is an error -> handle it gracefully somehow
    Exit; //==>
  end;
  if FParser.Tokenid in CallingConventions then begin
    if not SkipToSemicolon then begin
      // todo: this is an error -> handle it gracefully somehow
      Exit; //==>
    end;
    if not FParser.NextNoJunkEx then begin
      // todo: this is an error -> handle it gracefully somehow
      Exit; //==>
    end;
  end;
  if FParser.Tokenid = tkExternal then begin
    if not SkipToSemicolon then begin
      // todo: this is an error -> handle it gracefully somehow
      Exit; //==>
    end;
  end else begin
    FParser.SetParsingState(ParserState);
    Exit; //==>
  end;
//  if FParser.TokenID in [tkvarargs] then
end;

procedure TUnitExportsParser.SkipFunctionDeclaration;
begin
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkRoundOpen then begin
    SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    FParser.NextNoJunkEx;
  end;
  if FParser.Tokenid <> tkColon then begin
    // todo: this is an error -> handle it gracefully somehow
    Exit; //==>
  end;
  while FParser.Tokenid <> tkNull do begin
    if FParser.Tokenid = tkSemiColon then begin
      SkipDirectives;
      Exit; //==>
    end;
    FParser.NextNoJunkEx;
  end;
end;

{ TUnitExportParserThread }

constructor TUnitExportParserThread.Create(const _Files: TStrings; _Paths: TStrings;
  const _CacheDir: string; const _Symbols: TStrings);
var
  i: Integer;
  s: string;
begin
  if _CacheDir = '' then
    FCacheDirBS := ''
  else begin
    FCacheDirBS := IncludeTrailingPathDelimiter(_CacheDir);
    UniqueString(FCacheDirBS);
  end;

  FIdentifierList := TUnitExportlist.Create(5000);
  FUnitFiles := TStringList.Create;

  if Assigned(_Files) then begin
    FFiles := TStringList.Create;
    for i := 0 to _Files.Count - 1 do begin
      s := _Files[i];
      UniqueString(s);
      FFiles.Add(s);
    end;
  end else
    FFiles := nil;

  FPaths := TStringList.Create;
  FPaths.Duplicates := dupIgnore;
  FPaths.Sorted := True;
  for i := 0 to _Paths.Count - 1 do begin
    s := AddSlash(_Paths[i]);
    UniqueString(s);
    FPaths.Add(s);
  end;

  FSymbols := TStringList.Create;
  for i := 0 to _Symbols.Count - 1 do begin
    s := _Symbols[i];
    UniqueString(s);
    FSymbols.Add(s);
  end;

  inherited Create(False);
end;

destructor TUnitExportParserThread.Destroy;
begin
  OnTerminate := nil;
  inherited;
  FreeAndNil(FSymbols);
  FreeAndNil(FPaths);
  FreeAndNil(FFiles);
  FreeAndNil(FIdentifierList);
  FreeAndNil(FUnitFiles);
end;

function TUnitExportParserThread.DetachIdentifiers: TUnitExportlist;
begin
  Result := FIdentifierList;
  FIdentifierList := nil;
end;

function FileTimeToDosTime(_ft: _FILETIME): GXNativeUInt;
var
  DosDate: Word;
  DosTime: Word;
begin
  if FileTimeToDosDateTime(_ft, DosDate, DosTime) then begin
    Result := DosDate shl 16 + DosTime;
  end else begin
    // If converting the file date/time to DOS date/time fails, we don't really care, since the
    // worst thing that can happen is that we continue using an outdated cache file
    // (if it fails for a unit in the sarch path) or reparse a unit (if it fails for a cache file).
    Result := 0;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

{$IF not declared(faTemporary)}
const
  faTemporary = $00000100;
  faSymLink = $00000400;
{$IFEND}

procedure TUnitExportParserThread.GetAllFilesInDir(_dir: string; _sl: TStringList);
var
  sr: TSearchRec;
begin
  _dir := IncludeTrailingPathDelimiter(_dir);
  if SysUtils.FindFirst(_dir + '*.pas', faAnyFile, sr) = 0 then begin
    try
      repeat
        if Terminated then
          Exit; //==>
        if (sr.Attr and (faDirectory or faHidden or faSysFile or faTemporary or faSymLink)) = 0 then begin
          _sl.AddObject(_dir + sr.Name, Pointer(FileTimeToDosTime(sr.FindData.ftLastWriteTime)));
        end;
      until SysUtils.FindNext(sr) <> 0;
    finally
      SysUtils.FindClose(sr);
    end;
  end;
end;

procedure TUnitExportParserThread.GetAllFilesInPath(_sl: TStringList);
var
  i: Integer;
begin
  for i := 0 to FPaths.Count - 1 do begin
    if Terminated then
      Exit; //==>
    GetAllFilesInDir(FPaths[i], _sl);
  end;
end;

procedure TUnitExportParserThread.doExecute;

  function GxTryGetFileAge(const _fn: string; out _DosTime: GXNativeUInt): Boolean;
  var
    FileInformation: TWin32FileAttributeData;
  begin
    Result := GetFileAttributesEx(PChar(_fn), GetFileExInfoStandard, @FileInformation);
    if Result then begin
      _DosTime := FileTimeToDosTime(FileInformation.ftLastWriteTime);
    end;
  end;

  function TryFindPathToFile(const _fn: string; _FilesInPath: TStringList;
    out _FoundFn: string; out _FileAge: GXNativeUInt): Boolean;
  var
    i: Integer;
    fno: string;
    fn: string;
  begin
    Result := False;
    for i := 0 to _FilesInPath.Count - 1 do begin
      if Terminated then
        Exit; //==>
      fn := _FilesInPath[i];
      fno := ExtractFileName(fn);
      if SameText(fno, _fn) then begin
        Result := True;
        _FoundFn := fn;
        _FileAge := GXNativeUInt(_FilesInPath.Objects[i]);
        Exit; //==>
      end;
    end;
  end;

var
  FileIdx: Integer;
  fn: string;
  Parser: TUnitExportsParser;
  FilesFound: TStringList;
  FilesInPath: TStringList;
  Item: TUnitIdentifier;
  IdentIdx: Integer;
  ParsedIdentifiers: TUnitIdentifierList;
  LoadedIdentifiers: TUnitIdentifierList;
  WasLoadedFromCache: Boolean;
  UnitName: string;
  CacheFn: string;
  UnitTime: UInt32;
  CacheTime: UInt32;
{$IFDEF  DEBUG_TIMING}
  Loading: TStopwatch;
  Inserting: TStopwatch;
  Sorting: TStopwatch;
  Processing: TStopwatch;
  Total: TStopwatch;
  Searching: TStopwatch;
  Parsing: TStopwatch;
{$ENDIF}
begin
  inherited;

{$IFDEF  DEBUG_TIMING}
  Loading := TStopwatch.Create;
  Inserting := TStopwatch.Create;
  Sorting := TStopwatch.Create;
  Processing := TStopwatch.Create;
  Total := TStopwatch.Create;
  Searching := TStopwatch.Create;
  Parsing := TStopwatch.Create;

  Total.Start;
  Searching.Start;
{$ENDIF}

  FilesFound := nil;
  FilesInPath := TStringList.Create;
  try
    FilesFound := TStringList.Create;
    GetAllFilesInPath(FilesInPath);
    if Terminated then
      Exit; //==>

    FFoundUnitsCount := FilesInPath.Count;

    if Assigned(FFiles) then begin
      for FileIdx := 0 to FFiles.Count - 1 do begin
        if Terminated then
          Exit; //==>
        if TryFindPathToFile(FFiles[FileIdx] + '.pas', FilesInPath, fn, UnitTime) then
          FilesFound.AddObject(fn, Pointer(UnitTime));
      end;
      if FilesFound.Count = 0 then
        Exit; //==>
      FFiles.Assign(FilesFound);
    end else begin
      FFiles := FilesInPath;
      FilesInPath := nil;
    end;
  finally
    FreeAndNil(FilesFound);
    FreeAndNil(FilesInPath);
  end;
{$IFDEF DEBUG_TIMING}
  Searching.Stop;
{$ENDIF}

  if FCacheDirBS <> '' then
    ForceDirectories(FCacheDirBS);

{$IFDEF DEBUG_TIMING}
  Processing.Start;
{$ENDIF}
  for FileIdx := 0 to FFiles.Count - 1 do begin
    if Terminated then
      Exit; //==>
    fn := FFiles[FileIdx];
    UnitTime := UInt32(FFiles.Objects[FileIdx]);
    UnitName := ExtractFileName(fn);
    UnitName := ChangeFileExt(UnitName, '');
    if UnitName = 'u_dzBeep' then
      asm nop end;
    if FCacheDirBS = '' then
      CacheFn := ''
    else begin
      CacheFn := MangleFilename(fn);
      CacheFn := FCacheDirBS + CacheFn;
    end;
    if (CacheFn <> '') and GxTryGetFileAge(CacheFn, CacheTime) and (UnitTime < CacheTime) then begin
      LoadedIdentifiers := TUnitIdentifierList.Create(500);
      try
{$IFDEF DEBUG_TIMING}
        Loading.Start;
{$ENDIF}
        WasLoadedFromCache := LoadedIdentifiers.LoadFromFile(CacheFn);
{$IFDEF DEBUG_TIMING}
        Loading.Stop;
{$ENDIF}
        if WasLoadedFromCache then begin
          Inc(FLoadedUnitsCount);

          FUnitFiles.Add(fn);
          // the unit name is also an identifier
          // we save it with line number 1, even if the unit statement maight be further down
          // but personally I want to jump to the fist line of a unit if I open it for the unit name
          FIdentifierList.Add(UnitName, fn, 1);

{$IFDEF DEBUG_TIMING}
          Inserting.Start;
{$ENDIF}
          for IdentIdx := 0 to LoadedIdentifiers.Count - 1 do begin
            Item := LoadedIdentifiers[IdentIdx];
            FIdentifierList.Add(Item.Identifier, fn, Item.LineNo);
          end;
{$IFDEF DEBUG_TIMING}
          Inserting.Stop;
{$ENDIF}
        end;
      finally
        FreeAndNil(LoadedIdentifiers);
      end;
    end else begin
      WasLoadedFromCache := False;
    end;

    if not WasLoadedFromCache then begin
      Inc(FParsedUnitsCount);
{$IFDEF DEBUG_TIMING}
      Parsing.Start;
{$ENDIF}
      Parser := TUnitExportsParser.Create(fn);
      try
        Parser.Symbols.Assign(FSymbols);
        Parser.Execute;
        if Terminated then
          Exit; //==>
        FUnitFiles.Add(fn);
        ParsedIdentifiers := Parser.IdentifierList;
        if CacheFn <> '' then
          ParsedIdentifiers.SaveToFile(CacheFn);
        // the unit name is also an identifier
        // we save it with line number 1, even if the unit statement maight be further down
        // but personally I want to jump to the fist line of a unit if I open it for the unit name
        FIdentifierList.Add(UnitName, fn, 0);
        for IdentIdx := 0 to ParsedIdentifiers.Count - 1 do begin
          Item := ParsedIdentifiers[IdentIdx];
          FIdentifierList.Add(Item.Identifier, fn, Item.LineNo);
        end;
      finally
        FreeAndNil(Parser);
      end;
{$IFDEF DEBUG_TIMING}
      Parsing.Stop;
{$ENDIF}
    end;
  end;
  if Terminated then
    Exit; //==>

{$IFDEF DEBUG_TIMING}
  Processing.Stop;

  Sorting.Start;
{$ENDIF}
  FIdentifierList.Sort;
{$IFDEF DEBUG_TIMING}
  Sorting.Stop;

  Total.Stop;
{$ENDIF}

{$IF Declared(SendDebug)}
  SendDebugFmt('UnitExportParser finished, found %d identifiers', [FIdentifierList.Count]);
  SendDebugFmt('UnitExportParser found %d units', [FFoundUnitsCount]);
  SendDebugFmt('UnitExportParser loaded %d units', [FLoadedUnitsCount]);
  SendDebugFmt('UnitExportParser parsed %d units', [FParsedUnitsCount]);

{$IFDEF DEBUG_TIMING}
  SendDebugFmt('UnitExportParser searching time %d ms', [Searching.ElapsedMilliseconds]);
  SendDebugFmt('UnitExportParser loading time %d ms', [Loading.ElapsedMilliseconds]);
  SendDebugFmt('UnitExportParser inserting time %d ms', [Inserting.ElapsedMilliseconds]);
  SendDebugFmt('UnitExportParser parsing time %d ms', [Parsing.ElapsedMilliseconds]);
  SendDebugFmt('UnitExportParser processing time %d ms', [Processing.ElapsedMilliseconds]);
  SendDebugFmt('UnitExportParser sorting time %d ms', [Sorting.ElapsedMilliseconds]);
  SendDebugFmt('UnitExportParser total time %d ms', [Total.ElapsedMilliseconds]);
{$ENDIF}
{$IFEND}
end;

end.

