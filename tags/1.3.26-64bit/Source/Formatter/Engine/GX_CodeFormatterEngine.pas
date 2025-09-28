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
    procedure GetLine(_Tokens: TPascalTokenList; var _TokenNo: Integer;
      out _Line: TGXUnicodeString; out _OrigLineNo: Integer);
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
  OrigLineNo: Integer;
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
            GetLine(Tokens, TokenNo, Line, OrigLineNo);

            // Delphi 6 does not allow adding NIL pointers to a TStringList, so we
            // must make sure that this never happens. Also, negative values should
            // not be used.
            if OrigLineNo < 0 then
              OrigLineNo := -1;
            Inc(OrigLineNo);
            if OrigLineNo > 0 then
              _SourceCode.AddObject(Line, Pointer(OrigLineNo))
            else
            _SourceCode.Add(Line);
          end;
        end else begin
//          Logger.Info('Tokens not assigned');
        end;
      finally
        Tokens.Free;
      end;

      // We now have got a string list with the formatted code where the object pointer
      // is the line number from the original source code. NIL means we don't have the
      // original line number.
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

procedure TCodeFormatterEngine.GetLine(_Tokens: TPascalTokenList; var _TokenNo: Integer;
  out _Line: TGXUnicodeString; out _OrigLineNo: Integer);
var
  Token: TPascalToken;
  i: Integer;
begin
  _Line := '';
  _OrigLineNo := -1;

  if not Assigned(_Tokens) then
    Exit;

  if (_TokenNo < 0) or (_TokenNo >= _Tokens.Count) then
    Exit;

  Token := _Tokens[_TokenNo];
  repeat
    if Token.GetWordType in [wtMultilineString, wtMultilineStringEnd] then
      _Line := Token.GetContent
    else
      _Line := _Line + Token.GetContent;

    if (_OrigLineNo = -1) and (Token.ReservedType <> rtLineFeed) then begin
      // LineFeed tokens don't really work correctly here as they contain the line number
      // of the previous line
      _OrigLineNo := Token.OrigLineNo;
    end;

    Inc(_TokenNo);
    if _TokenNo >= _Tokens.Count then
      Break;

    Token := _Tokens[_TokenNo];
  until Token.ReservedType = rtLineFeed;

  // remove spaces and tabs at the end
  i := Length(_Line);

  while (i > 0) and ((_Line[i] = Space) or (_Line[i] = Tab)) do
    Dec(i);
  SetLength(_Line, i);
end;

end.
