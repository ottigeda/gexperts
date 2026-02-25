unit GX_GenericUtils;

// Minimal stub providing only what GX_FileScanner.pas, mwBCBTokenList.pas,
// and mPasLex.pas need from GX_GenericUtils.

interface

uses
  Classes;

function CompressWhiteSpace(const Str: string): string;
procedure DecomposeClassMethodString(const S: string; var MethodName, ClassName, NestedClasses: string);
procedure AnsiStrTok(const Source, Delimiter: string; Dest: TStrings);
function IsDpr(const FileName: string): Boolean;
function IsInc(const FileName: string): Boolean;
function IsCppSourceModule(const FileName: string): Boolean;
function StrContains(const SubStr, Str: string; CaseSensitive: Boolean = True): Boolean;
function IsCharWhiteSpaceOrNull(C: Char): Boolean;
function IsCharWhiteSpace(C: Char): Boolean;
function IsCharNumeric(C: Char): Boolean;
function IsCharSymbol(C: Char): Boolean;
function IsCharIdentifierStart(C: Char): Boolean;
function IsCharLineEndingOrNull(C: Char): Boolean;

implementation

uses
  SysUtils, Character;

function IsCharWhiteSpace(C: Char): Boolean;
begin
  Result := C.IsWhiteSpace;
end;

function IsCharWhiteSpaceOrNull(C: Char): Boolean;
begin
  Result := (C = #0) or IsCharWhiteSpace(C);
end;

function CompressWhiteSpace(const Str: string): string;
var
  i: Integer;
  Len: Integer;
  NextResultChar: Integer;
  CheckChar: Char;
  NextChar: Char;
begin
  Len := Length(Str);
  NextResultChar := 1;
  SetLength(Result, Len);

  for i := 1 to Len do
  begin
    CheckChar := Str[i];
    {$RANGECHECKS OFF}
    NextChar := Str[i + 1];
    {$RANGECHECKS ON}
    case CheckChar of
      #9, #10, #13, #32:
        begin
          if IsCharWhiteSpaceOrNull(NextChar) or (NextResultChar = 1) then
            Continue
          else
          begin
            Result[NextResultChar] := #32;
            Inc(NextResultChar);
          end;
        end;
      else
        begin
          Result[NextResultChar] := Str[i];
          Inc(NextResultChar);
        end;
    end;
  end;
  if Len = 0 then
    Exit;
  SetLength(Result, NextResultChar - 1);
end;

procedure AnsiStrTok(const Source, Delimiter: string; Dest: TStrings);
var
  i: Integer;
  SubString: string;
  Temp: string;
begin
  if (Source = '') or (Delimiter = '') or (Dest = nil) then
    Exit;

  Dest.Clear;
  SubString := Source;
  repeat
    i := Pos(Delimiter, SubString);

    if i = 0 then
      Temp := SubString
    else
      Temp := Copy(SubString, 1, i - 1);

    if Temp <> '' then
      Dest.Add(Temp);

    SubString := Copy(SubString, i + Length(Delimiter), Length(SubString) - i);
  until i = 0;
end;

procedure DecomposeClassMethodString(const S: string; var MethodName, ClassName, NestedClasses: string);
var
  TempStr: string;
  Identifiers: TStringList;
  AssignedCount: Integer;
  i: Integer;
begin
  TempStr := Trim(S);
  MethodName := '';
  ClassName := '';
  NestedClasses := '';

  Identifiers := TStringList.Create;
  try
    AnsiStrTok(TempStr, '.', Identifiers);
    AssignedCount := 0;
    for i := Identifiers.Count - 1 downto 0 do
    begin
      if AssignedCount = 0 then
      begin
        MethodName := Identifiers[i];
        Inc(AssignedCount);
      end
      else if AssignedCount = 1 then
      begin
        ClassName := Identifiers[i];
        Inc(AssignedCount);
      end
      else begin
        if NestedClasses = '' then
          NestedClasses := Identifiers[i]
        else
          NestedClasses := Identifiers[i] + '.' + NestedClasses;
      end;
    end;
  finally
    Identifiers.Free;
  end;
end;

function IsDpr(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), '.dpr');
end;

function IsInc(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), '.inc');
end;

function IsCppSourceModule(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.cpp') or (Ext = '.c') or (Ext = '.hpp') or (Ext = '.h')
    or (Ext = '.cxx') or (Ext = '.cc') or (Ext = '.hxx') or (Ext = '.hh')
    or (Ext = '.asm') or (Ext = '.bpf');
end;

function StrContains(const SubStr, Str: string; CaseSensitive: Boolean): Boolean;
begin
  if CaseSensitive then
    Result := Pos(SubStr, Str) > 0
  else
    Result := Pos(LowerCase(SubStr), LowerCase(Str)) > 0;
end;

function IsCharNumeric(C: Char): Boolean;
begin
  Result := C.IsDigit;
end;

function IsCharSymbol(C: Char): Boolean;
begin
  Result := C.IsSymbol or C.IsPunctuation;
end;

function IsCharIdentifierStart(C: Char): Boolean;
begin
  Result := C.IsLetter or (C = '_');
end;

function IsCharLineEndingOrNull(C: Char): Boolean;
begin
  Result := CharInSet(C, [#0, #10, #13]);
end;

end.
