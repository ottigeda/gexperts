unit GX_UnitExportsParserTest;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework,
  u_dzCompilerAndRtlVersions,
  Windows,
  u_dzErrorThread,
  GX_UnitExportsParser,
  Classes,
  SysUtils,
  mPasLex,
  mwPasParserTypes;

type
  TTestUnitExportsParser = class(TTestCase)
  private
  protected
  published
    procedure TestudzBeep;
    procedure TestMagickWand;
    procedure TestIdGlobalProtocols;
    procedure TestToolsApi;
  end;

implementation

{ TTestUnitExportsParser }

procedure TTestUnitExportsParser.TestIdGlobalProtocols;
const
  Identifiers: array[0..120] of string = (
    'ABNFToText',
    'BinNumbers',
    'BinStrToInt',
    'BreakApart',
    'CharRange',
    'CharsetToEncoding',
    'CommaSeparatedToStringList',
    'CompareDateTime',
    'ContentTypeToEncoding',
    'CookieStrToLocalDateTime',
    'CopyBytesToHostLongWord',
    'CopyBytesToHostUInt16',
    'CopyBytesToHostUInt32',
    'CopyBytesToHostWord',
    'CopyFileTo',
    'CopyTIdNetworkLongWord',
    'CopyTIdNetworkUInt16',
    'CopyTIdNetworkUInt32',
    'CopyTIdNetworkWord',
    'DateTimeToUnix',
    'DomainName',
    'EIdExtensionAlreadyExists',
    'EnsureMsgIDBrackets',
    'ExtractHeaderItem',
    'ExtractHeaderMediaSubType',
    'ExtractHeaderMediaType',
    'ExtractHeaderSubItem',
    'FileSizeByName',
    'FindFirstNotOf',
    'FindFirstOf',
    'FTPGMTDateTimeToMLS',
    'FTPLocalDateTimeToMLS',
    'FTPMLSToGMTDateTime',
    'FTPMLSToLocalDateTime',
    'GetClockValue',
    'GetGMTDateByName',
    'GetMIMEDefaultFileExt',
    'GetMIMETypeFromFile',
    'GetUniqueFileName',
    'GIdEncodingNeeded',
    'GmtOffsetStrToDateTime',
    'GMTToLocalDateTime',
    'HexNumbers',
    'IdGetDefaultCharSet',
    'IndyComputerName',
    'IndyCurrentYear',
    'IndyFalseBoolStrs',
    'IndySetLocalTime',
    'IndyStrToBool',
    'IndyTrueBoolStrs',
    'IndyWrapText',
    'IntToBin',
    'IsBinary',
    'IsDomain',
    'IsFQDN',
    'IsHeaderMediaType',
    'IsHeaderMediaTypes',
    'IsHeaderValue',
    'IsHex',
    'IsHostname',
    'IsTopDomain',
    'IsValidIP',
    'LongWordToFourChar',
    'LongWordToOrdFourByte',
    'LWS',
    'MakeTempFilename',
    'monthnames',
    'OrdFourByteToLongWord',
    'OrdFourByteToUInt32',
    'PadString',
    'ParseMetaHTTPEquiv',
    'PIdFileNameChar',
    'ProcessPath',
    'QuoteHTTP',
    'QuoteMIME',
    'QuotePlain',
    'QuoteRFC822',
    'ReadStringAsCharset',
    'ReadStringAsContentType',
    'ReadStringsAsCharset',
    'ReadStringsAsContentType',
    'RemoveHeaderEntries',
    'RemoveHeaderEntry',
    'ReplaceHeaderSubItem',
    'RightStr',
    'ROL',
    'ROR',
    'RPos',
    'StartsWith',
    'StrInternetToDateTime',
    'StrToDay',
    'StrToMonth',
    'StrToWord',
    'TIdEncodingNeededEvent',
    'TIdFileName',
    'TIdHeaderQuotingType',
    'TIdInterfacedObject',
    'TIdMimeTable',
    'TIdReadLnFunction',
    'TIME_BASEDATE',
    'TimeZoneBias',
    'TrimAllOf',
    'TStringEvent',
    'TwoCharToUInt16',
    'TwoCharToWord',
    'UInt16ToStr',
    'UInt16ToTwoBytes',
    'UInt32ToFourChar',
    'UInt32ToOrdFourByte',
    'UnixDateTimeToDelphiDateTime',
    'UNIXSTARTDATE',
    'UnquotedStr',
    'UpCaseFirst',
    'UpCaseFirstWord',
    'wdays',
    'WordToStr',
    'WordToTwoBytes',
    'WriteStringAsCharset',
    'WriteStringAsContentType',
    'WriteStringsAsCharset',
    'WriteStringsAsContentType');
var
  Parser: TUnitExportsParser;
  fn: string;
  i: Integer;
begin
  fn := IncludeTrailingPathDelimiter(ExtractFileDir(ParamSTr(0))) + 'testcases\IdGlobalProtocols.pas';
  Parser := TUnitExportsParser.Create(fn);
  try
    TUnitExportsParser.AddDefaultSymbols(Parser.Symbols);
    Parser.Execute;

    CheckEquals(Length(Identifiers), Parser.Identifiers.Count);
    for i := 0 to Parser.Identifiers.Count - 1 do
      CheckEquals(Identifiers[i], Parser.Identifiers[i]);
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TTestUnitExportsParser.TestMagickWand;
const
  Identifiers: array[0..9] of string = (
    'bla',
    'blablub',
    'blub',
    'DllGetDataSnapClassObject',
    'eins',
    'ExternalMethod',
    'IsMagickWand',
    'MagickClearException',
    'MessageBox',
    'Wandblablub');
var
  Parser: TUnitExportsParser;
  fn: string;
  i: Integer;
begin
  fn := IncludeTrailingPathDelimiter(ExtractFileDir(ParamSTr(0))) + 'testcases\magick_wand.pas';
  Parser := TUnitExportsParser.Create(fn);
  try
    TUnitExportsParser.AddDefaultSymbols(Parser.Symbols);
    Parser.Execute;

    CheckEquals(Length(Identifiers), Parser.Identifiers.Count);
    for i := 0 to Parser.Identifiers.Count - 1 do
      CheckEquals(Identifiers[i], Parser.Identifiers[i]);
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TTestUnitExportsParser.TestToolsApi;
const
  Identifiers: array[0..1] of string = (
    'IOTAProcess90',
    'TGetSrcLinesFunc');
var
  Parser: TUnitExportsParser;
  fn: string;
  i: Integer;
begin
  fn := IncludeTrailingPathDelimiter(ExtractFileDir(ParamSTr(0))) + 'testcases\Toolsapi.pas';
  Parser := TUnitExportsParser.Create(fn);
  try
    TUnitExportsParser.AddDefaultSymbols(Parser.Symbols);
    Parser.Symbols.Add('MSWINDOWS');
    Parser.Execute;

    CheckEquals(Length(Identifiers), Parser.Identifiers.Count);
    for i := 0 to Parser.Identifiers.Count - 1 do
      CheckEquals(Identifiers[i], Parser.Identifiers[i]);
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TTestUnitExportsParser.TestudzBeep;
const
  Identifiers: array[0..3] of string = (
    'Beeper',
    'TBeeper',
    'TBeepSequenceEntry',
    'TBeepSequenceList');
var
  Parser: TUnitExportsParser;
  fn: string;
  i: Integer;
begin
  fn := IncludeTrailingPathDelimiter(ExtractFileDir(ParamSTr(0))) + 'testcases\u_dzBeep.pas';
  Parser := TUnitExportsParser.Create(fn);
  try
    TUnitExportsParser.AddDefaultSymbols(Parser.Symbols);
    Parser.Execute;

    CheckEquals(Length(Identifiers), Parser.Identifiers.Count);
    for i := 0 to Parser.Identifiers.Count - 1 do
      CheckEquals(Identifiers[i], Parser.Identifiers[i]);
  finally
    FreeAndNil(Parser);
  end;
end;

initialization
  RegisterTest(TTestUnitExportsParser.Suite);

end.

