// the main code formatter engine, combines the parser and formatter do do the work
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterEngine;

{$I GX_CondDefine.inc}

interface

uses
{$IFDEF debug}
  // NOTE: including DbugIntf adds around 300 k to the dll
  DbugIntf,
{$ENDIF}
  SysUtils,
  Classes,
  GX_PascalTokenList,
  GX_CodeFormatterTypes,
  GX_CodeFormatterTokens,
  GX_CodeFormatterSettings;

(*
WISHLIST:
- suppress read-only file message
- read capitalization from var const type blocks
- sorting methods
- Is it possible to insert a "user customisable" line or group of lines before each
function/procedure, to allow the developer to comment it. Ex :

{------------Name of the proc------------------------}  (Simple)

{***************************
 * ...Comment ...
 * ...Comment ...
 ***************************/ (A few lines)}

 *)

type
  TCodeFormatterEngine = class(TObject)
  private
    FSettings: TCodeFormatterSettings;
    function GetLine(Tokens: TPascalTokenList; var TokenNo: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    {: @returns true if the formatting succeeded and the source code has changed }
    function Execute(SourceCode: TStrings): Boolean;
    property Settings: TCodeFormatterSettings read FSettings write FSettings;
  end;

implementation

uses
  StrUtils,
  GX_CodeFormatterFormatter,
  GX_CodeFormatterParser;

constructor TCodeFormatterEngine.Create;
begin
  inherited;
  FSettings := TCodeFormatterSettings.Create;
end;

destructor TCodeFormatterEngine.Destroy;
begin
  FreeAndNil(FSettings);
  inherited;
end;

function TCodeFormatterEngine.Execute(SourceCode: TStrings): Boolean;
var
  Line: string;
  TokenNo: Integer;
  OrigSource: string;
  Tokens: TPascalTokenList;
  s: string;
  OrigLen: Integer;
  NewLen: Integer;
begin
  try
    SourceCode.BeginUpdate;
    OrigSource := SourceCode.Text;

    Tokens := TCodeFormatterParser.Execute(SourceCode, FSettings);
    try
      TCodeFormatterFormatter.Execute(Tokens, FSettings);

      SourceCode.Clear;
      if Assigned(Tokens) then begin
        TokenNo := 0;
        while TokenNo < Tokens.Count do begin
          Line := GetLine(Tokens, TokenNo);
          SourceCode.Add(Line);
        end;
      end;
    finally
      Tokens.Free;
    end;

    // this checks for the case that either the formatter or the IDE
    // removed or added a CRLF at the end of the file
    // checking the length first prevents costly multiple
    // comparisons of potentially large strings
    s := SourceCode.Text;
    OrigLen := Length(OrigSource);
    NewLen := Length(s);

    if OrigLen = NewLen then
      Result := (OrigSource <> s)
    else if OrigLen = NewLen + 2 then
      Result := (OrigSource <> s + LineBreak)
    else if OrigLen + 2 = NewLen then
      Result := (OrigSource + LineBreak <> s)
    else
      Result := True;
  except
    on E: Exception do begin
      Result := False;
      { ShowMessage('Error occurred, cannot format'); }
    end;
  end;

  SourceCode.EndUpdate;
end;

function TCodeFormatterEngine.GetLine(Tokens: TPascalTokenList; var TokenNo: Integer): string;
var
  Token: TPascalToken;
  i: Integer;
begin
  Result := '';

  if not Assigned(Tokens) then
    Exit;

  if (TokenNo < 0) or (TokenNo >= Tokens.Count) then
    Exit;

  Token := Tokens[TokenNo];

  repeat
    Result := Result + Token.GetString;
    Inc(TokenNo);

    if TokenNo >= Tokens.Count then
      break;

    Token := Tokens[TokenNo];
  until Token.ReservedType = rtLineFeed;

  // remove spaces and tabs at the end
  i := Length(Result);

  while (i > 0) and ((Result[i] = Space) or (Result[i] = Tab)) do
    Dec(i);
  SetLength(Result, i);
end;

end.

