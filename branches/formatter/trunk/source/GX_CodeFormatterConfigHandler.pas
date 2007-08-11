// handles reading and writing of the code formatter settings to an ini file or
// any other implementation of the IConfigReader and IConfigWriter interface
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterConfigHandler;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  IniFiles,
  GX_CodeFormatterSettings,
  GX_CodeFormatterTypes;

const
  FORMATTER_CONFIG_PREFIX = 'FormatterSettings-';

type
  IConfigReader = interface
    function ReadBool(const AName: string; ADefault: Boolean): Boolean;
    function ReadInteger(const AName: string; ADefault: Integer): Integer;
    function ReadString(const AName, ADefault: string): string;
    procedure ReadStrings(const ASection: string; const AList: TStrings);
  end;

type
  IConfigWriter = interface
    procedure WriteBool(const AName: string; AValue: Boolean);
    procedure WriteInteger(const AName: string; AValue: Integer);
    procedure WriteString(const AName: string; const AValue: string);
    procedure WriteStrings(const ASection: string; const AList: TStrings);
  end;

type
  TIniFileWrapper = class(TInterfacedObject, IConfigReader, IConfigWriter)
  protected
    FIni: TCustomIniFile;
  protected // implementation of IConfigReader
    function ReadBool(const AName: string; ADefault: Boolean): Boolean;
    function ReadInteger(const AName: string; ADefault: Integer): Integer;
    function ReadString(const AName, ADefault: string): string;
    procedure ReadStrings(const ASection: string; const AList: TStrings);
  protected // implementatio of IConfigWriter
    procedure WriteBool(const AName: string; AValue: Boolean);
    procedure WriteInteger(const AName: string; AValue: Integer);
    procedure WriteString(const AName: string; const AValue: string);
    procedure WriteStrings(const ASection: string; const AList: TStrings);
  public
    constructor Create(AIniFile: TCustomIniFile);
  end;

type
  TCodeFormatterConfigHandler = class
  public
    class procedure WriteSettings(AWriter: IConfigWriter; ASettings: TCodeFormatterSettings);
    class procedure ReadSettings(AReader: IConfigReader; ASettings: TCodeFormatterSettings);
    class procedure ExportToFile(const AFilename: string; ASettings: TCodeFormatterSettings);
    class procedure ImportFromFile(const AFilename: string; ASettings: TCodeFormatterSettings);
    class procedure GetDefaultsList(ADefaults: TStrings);
    class function GetDefaultConfig(const AName: string; ASettings: TCodeFormatterSettings): Boolean;
  end;

implementation

{ TIniFileWrapper }

constructor TIniFileWrapper.Create(AIniFile: TCustomIniFile);
begin
  inherited Create;
  FIni := AIniFile;
end;

function TIniFileWrapper.ReadBool(const AName: string; ADefault: Boolean): Boolean;
begin
  Result := FIni.ReadBool('settings', AName, ADefault)
end;

function TIniFileWrapper.ReadInteger(const AName: string; ADefault: Integer): Integer;
begin
  Result := FIni.ReadInteger('settings', AName, ADefault)
end;

function TIniFileWrapper.ReadString(const AName, ADefault: string): string;
begin
  Result := FIni.ReadString('settings', AName, ADefault)
end;

procedure TIniFileWrapper.ReadStrings(const ASection: string; const AList: TStrings);
var
  Cnt: Integer;
  i: Integer;
  s: string;
begin
  AList.Clear;
  Cnt := FIni.ReadInteger(ASection, 'Count', 0);
  for i := 0 to Cnt - 1 do begin
    s := FIni.ReadString(ASection, Format('Item%.4d', [i]), '');
    if s <> '' then
      AList.Add(s);
  end;
end;

procedure TIniFileWrapper.WriteBool(const AName: string; AValue: Boolean);
begin
  FIni.WriteInteger('settings', AName, Ord(AValue));
end;

procedure TIniFileWrapper.WriteInteger(const AName: string; AValue: Integer);
begin
  FIni.WriteInteger('settings', AName, AValue);
end;

procedure TIniFileWrapper.WriteString(const AName, AValue: string);
begin
  FIni.WriteString('settings', AName, AValue);
end;

procedure TIniFileWrapper.WriteStrings(const ASection: string; const AList: TStrings);
var
  i: Integer;
begin
  FIni.WriteInteger(ASection, 'Count', AList.Count);
  for i := 0 to AList.Count - 1 do begin
    FIni.WriteString(ASection, Format('Items%.4d', [i]), AList[i]);
  end;
end;

{ TCodeFormatterConfigHandler }

class procedure TCodeFormatterConfigHandler.ReadSettings(AReader: IConfigReader; ASettings: TCodeFormatterSettings);

  function ReadSpaceSet(const AName: string; _Default: TSpaceSet): TSpaceSet;
  begin
    Result := IntToSpaceSet(AReader.ReadInteger(AName, SpaceSetToInt(_Default)));
  end;

var
  ES: TCodeFormatterEngineSettings;
  cps: set of TConfigPrecedenceEnum;
  cp: TConfigPrecedenceEnum;
begin
  ASettings.ShowDoneDialog := AReader.ReadBool('ShowDoneDialog', True);
  ASettings.UseCapitalizationFile := AReader.ReadBool('UseCapitalizationFile', False);
  ASettings.CapitalizationFile := AReader.ReadString('CapitalizationFile', '');
  ASettings.CapNames.Clear;
  if ASettings.UseCapitalizationFile and (ASettings.CapitalizationFile <> '') and FileExists(ASettings.CapitalizationFile) then
    ASettings.CapNames.LoadFromFile(ASettings.CapitalizationFile)
  else
    AReader.ReadStrings('Capitalization', ASettings.CapNames);

  ASettings.ConfigPrecedence[1] := IntToConfigPrecedence(AReader.ReadInteger('Precedence1', Ord(cpDirective)));
  ASettings.ConfigPrecedence[2] := IntToConfigPrecedence(AReader.ReadInteger('Precedence2', Ord(cpIniFile)));
  ASettings.ConfigPrecedence[3] := IntToConfigPrecedence(AReader.ReadInteger('Precedence3', Ord(cpMyConfig)));

  // make sure the setting is valid
  cps := [cpDirective, cpIniFile, cpMyConfig];
  Exclude(cps, ASettings.ConfigPrecedence[1]);
  if not (ASettings.ConfigPrecedence[2] in cps) then begin
    ASettings.ConfigPrecedence[2] := ASettings.ConfigPrecedence[3];
    if not (ASettings.ConfigPrecedence[2] in cps) then begin
      for cp := Low(TConfigPrecedenceEnum) to High(TConfigPrecedenceEnum) do begin
        if cp in cps then begin
          ASettings.ConfigPrecedence[2] := cp;
          break;
        end;
      end;
    end;
  end;
  Exclude(cps, ASettings.ConfigPrecedence[2]);
  if not (ASettings.ConfigPrecedence[3] in cps) then begin
    for cp := Low(TConfigPrecedenceEnum) to High(TConfigPrecedenceEnum) do begin
      if cp in cps then begin
        ASettings.ConfigPrecedence[3] := cp;
        break;
      end;
    end;
  end;

  ES := ASettings.Settings;
  ES.SpaceOperators := ReadSpaceSet('SpaceOperators', ES.SpaceOperators);
  ES.SpaceColon := ReadSpaceSet('SpaceColon', ES.SpaceColon);
  ES.SpaceSemiColon := ReadSpaceSet('SpaceSemiColon', ES.SpaceSemiColon);
  ES.SpaceComma := ReadSpaceSet('SpaceComma', ES.SpaceComma);
  ES.SpaceLeftBr := ReadSpaceSet('SpaceLeftBr', ES.SpaceLeftBr);
  ES.SpaceRightBr := ReadSpaceSet('SpaceRightBr', ES.SpaceRightBr);
  ES.SpaceLeftHook := ReadSpaceSet('SpaceLeftHook', ES.SpaceLeftHook);
  ES.SpaceRightHook := ReadSpaceSet('SpaceRightHook', ES.SpaceRightHook);
  ES.SpaceEqualOper := ReadSpaceSet('SpaceEqualOper', ES.SpaceEqualOper);
  ES.UpperCompDirectives := AReader.ReadBool('UpperCompDirectives', ES.UpperCompDirectives); //: Boolean;
  ES.UpperNumbers := AReader.ReadBool('UpperNumbers', ES.UpperNumbers); //: Boolean;
  ES.ReservedCase := TCase(AReader.ReadInteger('ReservedCase', Ord(ES.ReservedCase))); //: TCase;
  ES.StandDirectivesCase := TCase(AReader.ReadInteger('StandDirectivesCase', Ord(ES.StandDirectivesCase))); //: TCase;
  ES.ChangeIndent := AReader.ReadBool('ChangeIndent', ES.ChangeIndent); //: Boolean;
  ES.NoIndentElseIf := AReader.ReadBool('NoIndentElseIf', ES.NoIndentElseIf); //: Boolean;
  ES.IndentBegin := AReader.ReadBool('indentBegin', ES.IndentBegin); //: Boolean;
  ES.IndentTry := AReader.ReadBool('IndentTry', ES.IndentTry); //: Boolean;
  ES.IndentTryElse := AReader.ReadBool('IndentTryElse', ES.IndentTryElse); //: Boolean;
  ES.IndentCaseElse := AReader.ReadBool('IndentCaseElse', ES.IndentCaseElse); //: Boolean;
  ES.IndentComments := AReader.ReadBool('IndentComments', ES.IndentComments); //: Boolean;
  ES.IndentCompDirectives := AReader.ReadBool('IndentCompDirectives', ES.IndentCompDirectives); //: Boolean;
  ES.BlankProc := AReader.ReadBool('BlankProc', ES.BlankProc); //: Boolean;
  ES.BlankSubProc := AReader.ReadBool('BlankSubProc', ES.BlankSubProc); //: Boolean;
  ES.RemoveDoubleBlank := AReader.ReadBool('RemoveDoubleBlank', ES.RemoveDoubleBlank); //: Boolean;
  ES.SpacePerIndent := AReader.ReadInteger('SpacePerIndent', ES.SpacePerIndent); //: Integer;
  ES.FeedRoundBegin := TFeedBegin(AReader.ReadInteger('FeedRoundBegin', Ord(ES.FeedRoundBegin)));
  ES.FeedRoundTry := TFeedBegin(AReader.ReadInteger('FeedRoundTry', Ord(ES.FeedRoundTry)));
  ES.FeedBeforeEnd := AReader.ReadBool('FeedBeforeEnd', ES.FeedBeforeEnd); //: Boolean;
  ES.FeedAfterThen := AReader.ReadBool('FeedAfterThen', ES.FeedAfterThen); //: Boolean;
  ES.ExceptSingle := AReader.ReadBool('ExceptSingle', ES.ExceptSingle); //: Boolean;
  ES.FeedAfterVar := AReader.ReadBool('FeedAfterVar', ES.FeedAfterVar); //: Boolean;
  ES.FeedEachUnit := AReader.ReadBool('FeedEachUnit', ES.FeedEachUnit); //: Boolean;
  ES.NoFeedBeforeThen := AReader.ReadBool('NoFeedBeforeThen', ES.NoFeedBeforeThen); //: Boolean;
  ES.FeedElseIf := AReader.ReadBool('FeedElseIf', ES.FeedElseIf); //: Boolean;
  ES.FillNewWords := IntToCapfileMode(AReader.ReadInteger('FillNewWords', CapfileModeToInt(ES.FillNewWords)));
  ES.FeedAfterSemiColon := AReader.ReadBool('FeedAfterSemiColon', ES.FeedAfterSemiColon); //: Boolean;
  ES.StartCommentOut := AReader.ReadString('StartCommentOut', ES.StartCommentOut);
  ES.EndCommentOut := AReader.ReadString('EndCommentOut', ES.EndCommentOut);
  ES.CommentFunction := AReader.ReadBool('CommentFunction', ES.CommentFunction); //: Boolean;
  ES.CommentUnit := AReader.ReadBool('CommentUnit', ES.CommentUnit); //: Boolean;
  ES.WrapLines := AReader.ReadBool('WrapLines', ES.WrapLines); //: Boolean;
  ES.WrapPosition := AReader.ReadInteger('WrapPosition', ES.WrapPosition); //: Byte;
  ES.AlignCommentPos := AReader.ReadInteger('AlignCommentPos', ES.AlignCommentPos); //: Byte;
  ES.AlignComments := AReader.ReadBool('AlignComments', ES.AlignComments); //: Boolean;
  ES.AlignVarPos := AReader.ReadInteger('AlignVarPos', ES.AlignVarPos); //: Byte;
  ES.AlignVar := AReader.ReadBool('AlignVar', ES.AlignVar); //: Boolean;
  ASettings.Settings := ES;
end;

class procedure TCodeFormatterConfigHandler.WriteSettings(AWriter: IConfigWriter; ASettings: TCodeFormatterSettings);

  procedure WriteSpaceSet(const AName: string; AValue: TSpaceSet);
  begin
    AWriter.WriteInteger(AName, SpaceSetToInt(AValue));
  end;

begin
  WriteSpaceSet('SpaceOperators', ASettings.SpaceOperators);
  WriteSpaceSet('SpaceColon', ASettings.SpaceColon);
  WriteSpaceSet('SpaceSemiColon', ASettings.SpaceSemiColon);
  WriteSpaceSet('SpaceComma', ASettings.SpaceComma);
  WriteSpaceSet('SpaceLeftBr', ASettings.SpaceLeftBr);
  WriteSpaceSet('SpaceRightBr', ASettings.SpaceRightBr);
  WriteSpaceSet('SpaceLeftHook', ASettings.SpaceLeftHook);
  WriteSpaceSet('SpaceRightHook', ASettings.SpaceRightHook);
  WriteSpaceSet('SpaceEqualOper', ASettings.SpaceEqualOper);
  AWriter.WriteBool('UpperCompDirectives', ASettings.UpperCompDirectives); //: Boolean;
  AWriter.WriteBool('UpperNumbers', ASettings.UpperNumbers); //: Boolean;
  AWriter.WriteInteger('ReservedCase', Ord(ASettings.ReservedCase)); //: TCase;
  AWriter.WriteInteger('StandDirectivesCase', Ord(ASettings.StandDirectivesCase)); //: TCase;
  AWriter.WriteBool('ChangeIndent', ASettings.ChangeIndent); //: Boolean;
  AWriter.WriteBool('NoIndentElseIf', ASettings.NoIndentElseIf); //: Boolean;
  AWriter.WriteBool('indentBegin', ASettings.IndentBegin); //: Boolean;
  AWriter.WriteBool('IndentTry', ASettings.IndentTry); //: Boolean;
  AWriter.WriteBool('IndentTryElse', ASettings.IndentTryElse); //: Boolean;
  AWriter.WriteBool('IndentCaseElse', ASettings.IndentCaseElse); //: Boolean;
  AWriter.WriteBool('IndentComments', ASettings.IndentComments); //: Boolean;
  AWriter.WriteBool('IndentCompDirectives', ASettings.IndentCompDirectives); //: Boolean;
  AWriter.WriteBool('BlankProc', ASettings.BlankProc); //: Boolean;
  AWriter.WriteBool('BlankSubProc', ASettings.BlankSubProc); //: Boolean;
  AWriter.WriteBool('RemoveDoubleBlank', ASettings.RemoveDoubleBlank); //: Boolean;
  AWriter.WriteInteger('SpacePerIndent', ASettings.SpacePerIndent); //: Integer;
  AWriter.WriteInteger('FeedRoundBegin', Ord(ASettings.FeedRoundBegin)); //: TFeedBegin;
  AWriter.WriteInteger('FeedRoundTry', Ord(ASettings.FeedRoundTry)); //: TFeedBegin;
  AWriter.WriteBool('FeedBeforeEnd', ASettings.FeedBeforeEnd); //: Boolean;
  AWriter.WriteBool('FeedAfterThen', ASettings.FeedAfterThen); //: Boolean;
  AWriter.WriteBool('ExceptSingle', ASettings.ExceptSingle); //: Boolean;
  AWriter.WriteBool('FeedAfterVar', ASettings.FeedAfterVar); //: Boolean;
  AWriter.WriteBool('FeedEachUnit', ASettings.FeedEachUnit); //: Boolean;
  AWriter.WriteBool('NoFeedBeforeThen', ASettings.NoFeedBeforeThen); //: Boolean;
  AWriter.WriteBool('FeedElseIf', ASettings.FeedElseIf); //: Boolean;
  AWriter.WriteInteger('FillNewWords', CapfileModeToInt(ASettings.FillNewWords));
  AWriter.WriteBool('FeedAfterSemiColon', ASettings.FeedAfterSemiColon); //: Boolean;
  AWriter.WriteString('StartCommentOut', ASettings.StartCommentOut); //: TCommentArray;
  AWriter.WriteString('EndCommentOut', ASettings.EndCommentOut); //: TCommentArray;
  AWriter.WriteBool('CommentFunction', ASettings.CommentFunction); //: Boolean;
  AWriter.WriteBool('CommentUnit', ASettings.CommentUnit); //: Boolean;
  AWriter.WriteBool('WrapLines', ASettings.WrapLines); //: Boolean;
  AWriter.WriteInteger('WrapPosition', ASettings.WrapPosition); //: Byte;
  AWriter.WriteInteger('AlignCommentPos', ASettings.AlignCommentPos); //: Byte;
  AWriter.WriteBool('AlignComments', ASettings.AlignComments); //: Boolean;
  AWriter.WriteInteger('AlignVarPos', ASettings.AlignVarPos); //: Byte;
  AWriter.WriteBool('AlignVar', ASettings.AlignVar); //: Boolean;

  AWriter.WriteInteger('Precedence1', Ord(ASettings.ConfigPrecedence[1]));
  AWriter.WriteInteger('Precedence2', Ord(ASettings.ConfigPrecedence[2]));
  AWriter.WriteInteger('Precedence3', Ord(ASettings.ConfigPrecedence[3]));

  AWriter.WriteBool('ShowDoneDialog', ASettings.ShowDoneDialog);
  AWriter.WriteBool('UseCapitalizationFile', ASettings.UseCapitalizationFile);
  AWriter.WriteString('CapitalizationFile', ASettings.CapitalizationFile);

  if ASettings.UseCapitalizationFile and (ASettings.CapitalizationFile <> '') then begin
    try
      ASettings.CapNames.SaveToFile(ASettings.CapitalizationFile);
    except
        // ignore, file might be readonly
    end;
  end else
    AWriter.WriteStrings('Capitalization', ASettings.CapNames);
end;

class procedure TCodeFormatterConfigHandler.ExportToFile(const AFilename: string; ASettings: TCodeFormatterSettings);
var
  Writer: IConfigWriter;
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(AFilename);
  try
    Writer := TIniFileWrapper.Create(Ini);
    WriteSettings(Writer, ASettings);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function GetModulePath: string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

class function TCodeFormatterConfigHandler.GetDefaultConfig(const AName: string; ASettings: TCodeFormatterSettings): Boolean;
var
  Filename: string;
begin
  Filename := GetModulePath + FORMATTER_CONFIG_PREFIX + AName + '.ini';
  Result := FileExists(Filename);
  if Result then
    ImportFromFile(Filename, ASettings);
end;

class procedure TCodeFormatterConfigHandler.GetDefaultsList(ADefaults: TStrings);
var
  Path: string;
  sr: TSearchRec;
  s: string;
begin
  Path := GetModulePath;
  if 0 = FindFirst(Path + FORMATTER_CONFIG_PREFIX + '*.ini', faAnyFile, sr) then begin
    try
      repeat
        s := ChangeFileExt(sr.Name, '');
        Delete(s, 1, Length(FORMATTER_CONFIG_PREFIX));
        ADefaults.Add(s);
      until 0 <> FindNext(sr);
    finally
      FindClose(sr);
    end;
  end;
end;

class procedure TCodeFormatterConfigHandler.ImportFromFile(const AFilename: string; ASettings: TCodeFormatterSettings);
var
  Reader: IConfigReader;
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(AFilename);
  try
    Reader := TIniFileWrapper.Create(Ini);
    ReadSettings(Reader, ASettings);
  finally
    Ini.Free;
  end;
end;

end.

