unit GX_CodeFormatterSourceLineList;

interface

uses
  SysUtils,
  Classes,
  GX_StringList;

type
  TSourceLineList = class(TGXUnicodeStringList)
  private
  protected
    FFirstLine: Integer;
    FLastLine: Integer;
    FFormattedBlockStart: string;
    FFormattedBlockEnd: string;
  public
    ///<summary>
    /// @param FirstLine and LastLine are the line numbers of the block that was formatted. </summary>
    constructor Create(const _Content: TGXUnicodeString; _FirstLine, _LastLine: Integer);
    ///<summary>
    /// Checks whether FirstLine <= OldLineNo <= LastLine and returns the new line number
    /// @param OldLineNo is the line number to check
    /// @param NewLineNo is the new line number after the formatting
    /// @returns True, if line is within the formatted block, False otherwise. </summary>
    function FindNewLineNo(_OldLineNo: Integer; out _NewLineNo: Integer): Boolean;
    function GetFormattedBlock: TGXUnicodeString;
  end;

implementation

uses
  GX_GenericUtils,
  u_dzStringUtils;

constructor TSourceLineList.Create(const _Content: TGXUnicodeString; _FirstLine, _LastLine: Integer);
const
  FORMATTED_BLOCK_START = '****formatted block start****';
  FORMATTED_BLOCK_END = '****formatted block end****';
begin
  inherited Create;

  Self.Text := _Content;
  FFirstLine := _FirstLine;
  if _LastLine = -1 then
    FLastLine := Self.Count + 1
  else begin
    FLastLine := _LastLine;

    FFormattedBlockStart := FORMATTED_BLOCK_START;
    FFormattedBlockEnd := FORMATTED_BLOCK_END;
    while Pos(FFormattedBlockStart, _Content) <> 0 do
      FFormattedBlockStart := '*' + FFormattedBlockStart + '*';
    while Pos(FFormattedBlockEnd, _Content) <> 0 do
      FFormattedBlockEnd := '*' + FFormattedBlockEnd + '*';
    FFormattedBlockStart := '{' + FFormattedBlockStart + '}';
    FFormattedBlockEnd := '{' + FFormattedBlockEnd + '}';

    Self.Insert(FLastLine - 1, FFormattedBlockEnd);
    Self.Insert(FFirstLine - 1, FFormattedBlockStart);
  end;

end;

function TSourceLineList.FindNewLineNo(_OldLineNo: Integer; out _NewLineNo: Integer): Boolean;
var
  i: Integer;
  OldLineNoOfCode: Integer;
begin
  _NewLineNo := _OldLineNo;
  Result := (FFirstLine <= _OldLineNo) and (_OldLineNo <= FLastLine);
  if not Result then
    Exit; //==>

  for i := 0 to Self.Count - 1 do begin
    // stored old line numbers were incremented by 1
    OldLineNoOfCode := GXNativeInt(Self.Objects[i]) - 1;
    if _OldLineNo = OldLineNoOfCode then begin
      // line numbers are one based
      _NewLineNo := i + 1;
      Exit; //==>
    end;
    if _OldLineNo < OldLineNoOfCode then begin
      // we passed the line we're searching for -> take the previous line
      _NewLineNo := i;
      Exit; //==>
    end;
  end;
end;

function TSourceLineList.GetFormattedBlock: TGXUnicodeString;
var
  Position: Integer;
begin
  Result := Self.Text;
  Position := Pos(FFormattedBlockEnd, Result);
  Result := Copy(Result, 1, Position - 1);
  Position := Pos(FFormattedBlockStart, Result);
  Result := Copy(Result, Position + Length(FFormattedBlockStart));
  if Copy(Result, 1, 2) = CRLF then
    Result := Copy(Result, 3);
end;

end.

