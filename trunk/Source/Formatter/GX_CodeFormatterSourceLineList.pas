unit GX_CodeFormatterSourceLineList;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_StringList;

type
  TSourceLineList = class(TGXUnicodeStringList)
  private
    FFirstLine: Integer;
    FLastLine: Integer;
    FFormattedBlockStart: string;
    FFormattedBlockEnd: string;
    ///<summary>
    /// Set to 0 when formatting the whole unit
    /// Set to -1 initially when formatting the selected block only
    /// Set to the new line number of the first line of the selected block in the first
    /// call to FindNewLineNo to calculate the offset. </summary>
    FFirstLineNew: Integer;
    function FindNewLineNoGlobal(_OldLineNo: Integer): Integer;
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
  u_dzStringUtils,
{$IFOPT D+} GX_DbugIntf, {$ENDIF}
  GX_GenericUtils;

constructor TSourceLineList.Create(const _Content: TGXUnicodeString; _FirstLine, _LastLine: Integer);
const
  FORMATTED_BLOCK_START = '****formatted block start****';
  FORMATTED_BLOCK_END = '****formatted block end****';
begin
  inherited Create;

  Self.Text := _Content;
  FFirstLine := _FirstLine;
  if _LastLine = -1 then begin
    // formatting the whole unit
    FFirstLineNew := 0;
    FLastLine := Self.Count + 1;
  end else begin
    // formatting the selected block (-1 is a flag)
    FFirstLineNew := -1;

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

function TSourceLineList.FindNewLineNoGlobal(_OldLineNo: Integer): Integer;
var
  i: Integer;
  OldLineNoOfCode: GXNativeInt;
begin
  for i := 0 to Self.Count - 1 do begin
    // stored old line numbers were incremented by 1
    OldLineNoOfCode := GXNativeInt(Self.Objects[i]) - 1;
    if _OldLineNo = OldLineNoOfCode then begin
      // line numbers are one based
      Result := i + 1;
      Exit; //==>
    end;
    if _OldLineNo < OldLineNoOfCode then begin
      // we passed the line we're searching for -> take the previous line
      Result := i;
      Exit; //==>
    end;
  end;
  // this should not happen but if it does, don't change the line no
{$IFDEF DEBUG} SendDebugFmt('Could not determine new line number for line %d', [_OldlineNo]); {$ENDIF}
  Result := _OldLineNo;
end;

function TSourceLineList.FindNewLineNo(_OldLineNo: Integer; out _NewLineNo: Integer): Boolean;
begin
  Result := (FFirstLine <= _OldLineNo) and (_OldLineNo <= FLastLine);
  if not Result then begin
    // outside the formatted block, don't change it
    _NewLineNo := _OldLineNo;
    Exit; //==>
  end;

  if FFirstLineNew = -1 then begin
    // We are formatting the selected block and this is the first call to FindNewLineNo
    // We need the new line of the first line in the block to be used as an offset
    FFirstLineNew := FindNewLineNoGlobal(FFirstLine);
  end;

  _NewLineNo := FindNewLineNoGlobal(_OldLineNo);
  // adjust for the block offset (FFirstLine and FFirstLineNew are 0 if there is no block)
  _NewLineNo := _NewLineNo - FFirstLineNew + FFirstLine;
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

