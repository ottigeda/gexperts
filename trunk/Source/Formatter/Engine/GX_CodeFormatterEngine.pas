// the main code formatter engine, combines the parser and formatter do do the work
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (https://blog.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

unit GX_CodeFormatterEngine;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_GenericUtils,
  GX_StringList,
  GX_Logging,
  GX_CodeFormatterTokenList,
  GX_CodeFormatterTypes,
  GX_CodeFormatterTokens,
  GX_CodeFormatterSettings;

type
  TCodeFormatterEngine = class(TObject)
  private
    Logger: IGxLogger;
    FSettings: TCodeFormatterSettings;
    function GetLine(_Tokens: TPascalTokenList; var _TokenNo: Integer): TGXUnicodeString;
  public
    constructor Create;
    destructor Destroy; override;
    {: @returns true if the formatting succeeded and the source code has changed }
    function Execute(_SourceCode: TGXUnicodeStringList): Boolean;
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
  Logger := CreateModuleLogger('Formatter');
end;

destructor TCodeFormatterEngine.Destroy;
begin
  FreeAndNil(FSettings);

  inherited;
end;

function TCodeFormatterEngine.Execute(_SourceCode: TGXUnicodeStringList): Boolean;
var
  Line: TGXUnicodeString;
  TokenNo: Integer;
  OrigSource: TGXUnicodeString;
  Tokens: TPascalTokenList;
  NewSource: TGXUnicodeString;
begin
  Logger.MethodEnter('TCodeFormatterEngine.Execute', [_SourceCode.Count]);
  _SourceCode.BeginUpdate;
  try
    try
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
        end else begin
//          Logger.Info('Tokens not assigned');
        end;
      finally
        Tokens.Free;
      end;

      NewSource := _SourceCode.Text;
      Result := SourceDifferentApartFromTrailingLineBreak(OrigSource, NewSource);
    except
      on E: Exception do begin
        Logger.Exception(E);
        Result := False;
      end;
    end;
  finally
    _SourceCode.EndUpdate;
  end;

end;

function TCodeFormatterEngine.GetLine(_Tokens: TPascalTokenList; var _TokenNo: Integer): TGXUnicodeString;
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
    Result := Result + Token.GetContent;
    Inc(_TokenNo);

    if _TokenNo >= _Tokens.Count then
      Break;

    Token := _Tokens[_TokenNo];
  until Token.ReservedType = rtLineFeed;

  // remove spaces and tabs at the end
  i := Length(Result);

  while (i > 0) and ((Result[i] = Space) or (Result[i] = Tab)) do
    Dec(i);
  SetLength(Result, i);
end;

end.
