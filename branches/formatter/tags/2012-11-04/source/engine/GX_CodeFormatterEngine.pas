// the main code formatter engine, combines the parser and formatter do do the work
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

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
  GX_CodeFormatterTokenList,
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
    function GetLine(_Tokens: TPascalTokenList; var _TokenNo: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    {: @returns true if the formatting succeeded and the source code has changed }
    function Execute(_SourceCode: TStrings): Boolean;
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

function TCodeFormatterEngine.Execute(_SourceCode: TStrings): Boolean;
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
    _SourceCode.BeginUpdate;
    OrigSource := _SourceCode.Text;

    Tokens := TCodeFormatterParser.Execute(_SourceCode, FSettings);
    try
      TCodeFormatterFormatter.Execute(Tokens, FSettings);

      _SourceCode.Clear;
      if Assigned(Tokens) then begin
        TokenNo := 0;
        while TokenNo < Tokens.Count do begin
          Line := GetLine(Tokens, TokenNo);
          _SourceCode.Add(Line);
        end;
      end;
    finally
      Tokens.Free;
    end;

    // this checks for the case that either the formatter or the IDE
    // removed or added a CRLF at the end of the file
    // checking the length first prevents costly multiple
    // comparisons of potentially large strings
    s := _SourceCode.Text;
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

  _SourceCode.EndUpdate;
end;

function TCodeFormatterEngine.GetLine(_Tokens: TPascalTokenList; var _TokenNo: Integer): string;
var
  Token: TPascalToken;
  i: Integer;
begin
  Result := '';

  if not Assigned(_Tokens) then
    Exit;

  if (_TokenNo < 0) or (_TokenNo >= _Tokens.Count) then
    Exit;

  Token := _Tokens[_TokenNo];

  repeat
    Result := Result + Token.GetString;
    Inc(_TokenNo);

    if _TokenNo >= _Tokens.Count then
      break;

    Token := _Tokens[_TokenNo];
  until Token.ReservedType = rtLineFeed;

  // remove spaces and tabs at the end
  i := Length(Result);

  while (i > 0) and ((Result[i] = Space) or (Result[i] = Tab)) do
    Dec(i);
  SetLength(Result, i);
end;

end.

