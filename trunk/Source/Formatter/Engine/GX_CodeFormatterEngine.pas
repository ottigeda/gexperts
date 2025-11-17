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
    function GetSettingsName(const AFileName: string; FullText: TGXUnicodeString): string;
    function FormatFile(const _FileName: string): Boolean;
  end;

implementation

uses
  IniFiles,
  StrUtils,
  GX_CodeFormatterFormatter,
  GX_CodeFormatterParser,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterGXConfigWrapper;

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

function FileNameMatches(const FileName, Pattern: string): Boolean;
var
  sr: TSearchRec;
begin
  Result := False;
  if FindFirst(Pattern, 0, sr) = 0 then begin
    repeat
      if SameText(FileName, sr.Name) then begin
        Result := True;
        Break;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

function TCodeFormatterEngine.GetSettingsName(const AFileName: string; FullText: TGXUnicodeString): string;

  function GetFromConfigFile: string;
  var
    ConfigFileName, ConfiguredFileName, Path: string;
    IniFile: TIniFile;
    ConfiguredFileNames: TStringList;
    i: Integer;
  begin
    Result := '';
    Path := AddSlash(ExtractFilePath(ExpandFileName(AFileName)));
    ConfigFileName := Path + 'GXFormatter.ini'; // Do not localize.
    IniFile := TIniFile.Create(ConfigFileName);
    try
      ConfiguredFileNames := TStringList.Create;
      try
        // Read section manually to preserve order:
        IniFile.ReadSection('FileSettings', ConfiguredFileNames);
        for i := 0 to Pred(ConfiguredFileNames.Count) do begin
          ConfiguredFileName := ConfiguredFileNames[i];
          if FileNameMatches(ExtractFileName(AFileName), Path + ConfiguredFileName) then begin
            Result := IniFile.ReadString('FileSettings', ConfiguredFileName, '');
            Logger.InfoFmt('Settings "%s" from rule %s in %s', [Result, ConfiguredFileName, ConfigFileName]);
            Break;
          end;
        end;
      finally
        FreeAndNil(ConfiguredFileNames);
      end;
    finally
      FreeAndNil(IniFile);
    end;
  end;

  function GetFromSource: string;
  const
    cConfigDirective = '{GXFormatter.config='; // Do not localize.
  var
    FirstLine: string;
  begin
    FirstLine := Trim(FullText);
    if SameText(Copy(FirstLine, 1, Length(cConfigDirective)), cConfigDirective) and
      (FirstLine[Length(FirstLine)] = '}') then begin
      Result := Trim(Copy(FirstLine, Length(cConfigDirective) + 1, Length(FirstLine) - Length(cConfigDirective) - 1));
      Logger.InfoFmt('Settings "%s" from in-source directive', [Result]);
    end else
      Result := '';
  end;

  function GetSettingNameFor(_Precedence: TConfigPrecedenceEnum; out _Name: string): Boolean;
  begin
    case _Precedence of
      cpDirective: begin
          _Name := GetFromSource;
          Result := _Name <> '';
        end;
      cpIniFile: begin
          _Name := GetFromConfigFile;
          Result := _Name <> '';
        end;
      cpMyConfig: begin
          _Name := '';
          Result := True;
        end else
      Result := False;
    end;
  end;

var
  i: Integer;
begin
  for i := Low(TOneToThree) to High(TOneToThree) do
    if GetSettingNameFor(Settings.ConfigPrecedence[i], Result) then
      Exit;
  Result := '';
end;

function TCodeFormatterEngine.FormatFile(const _FileName: string): Boolean;
resourcestring
  str_UnsupportedFileTypeS = 'Unsupported file type: %s';
var
  FullText: TGXUnicodeStringList;
  TempSettings: TCodeFormatterSettings;
  OrigSettings: TCodeFormatterEngineSettings;
  SettingsName: string;
begin
  Result := False;
  if not (IsPascalSourceFile(_FileName) or IsDelphiPackage(_FileName) or FileMatchesExtension(_FileName, '.tpl')) then
    raise ECodeFormatter.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(_FileName)]);

  Logger.InfoFmt('Formatting requested for "%s"', [_FileName]);
  TempSettings := nil;
  FullText := TGXUnicodeStringList.Create;
  try
    FullText.LoadFromFile(_FileName);

    SettingsName := Trim(GetSettingsName(_FileName, FullText.Text));
    if SettingsName <> '-' then begin
      if SettingsName <> '' then begin
        Logger.InfoFmt('Use settings "%s"', [SettingsName]);
        TempSettings := TCodeFormatterSettings.Create;
        if TCodeFormatterConfigHandler.GetDefaultConfig(SettingsName, TempSettings,
          GetDefaultCapitalizationFilename) then begin
          OrigSettings := Settings.Settings;
          Settings.Settings := TempSettings.Settings;
        end else
          FreeAndNil(TempSettings);
      end else
        Logger.Info('Use default settings');

      Result := Execute(FullText);
      if Result then begin
        Logger.Info('Saving changed file');
        // add an empty line to be compatible with running in the IDE
        FullText.Add('');
        FullText.SaveToFile(_FileName);
      end else begin
        Logger.Info('Nothing changed');
      end;
    end else
      Logger.Info('Ignoring request, no settings name available');
  finally
    FreeAndNil(FullText);
    if Assigned(TempSettings) then begin
      FreeAndNil(TempSettings);
      Settings.Settings := OrigSettings;
    end;
  end;
end;

end.

