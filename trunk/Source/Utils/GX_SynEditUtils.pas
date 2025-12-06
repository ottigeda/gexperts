unit GX_SynEditUtils;

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  SynMemo;

type
  TSynMemoUtil = class
  private
    procedure GetSelectedLines(out _StartIdx, _EndIdx: Integer);
  protected
    FMemo: TSynMemo;
    function ProcessLine(const _Line: string): string; virtual; abstract;
  public
    class procedure ProcessSelectedLines(_Memo: TSynMemo); overload; virtual;
    class procedure ProcessAllLines(_Memo: TSynMemo); overload; virtual;
    constructor Create(_Memo: TSynMemo); virtual;
    procedure ProcessSelectedLines; overload;
    procedure ProcessAllLines; overload;
  end;

type
  TSynMemoUtilPrependDots = class(TSynMemoUtil)
  protected
    function ProcessLine(const _Line: string): string; override;
  end;

type
  TSynMemoUtilRemoveDots = class(TSynMemoUtil)
  protected
    function ProcessLine(const _Line: string): string; override;
  end;

type
  TSynMemoUtilMakePathsAbsoluteOrRelative = class(TSynMemoUtil)
  protected
    FProjectDir: string;
  public
    constructor Create(_Memo: TSynMemo); override;
  end;

type
  TSynMemoUtilMakePathsAbsolute = class(TSynMemoUtilMakePathsAbsoluteOrRelative)
  protected
    function ProcessLine(const _Line: string): string; override;
  end;

type
  TSynMemoUtilMakePathsRelative = class(TSynMemoUtilMakePathsAbsoluteOrRelative)
  protected
    function ProcessLine(const _Line: string): string; override;
  end;

procedure TSynMemo_UpdateScrollbars(ME: TSynMemo);

implementation

uses
  StrUtils,
  u_dzFileUtils,
  u_dzStringUtils,
  SynEditTypes,
  GX_OtaUtils,
  GX_GenericUtils;

procedure TSynMemo_UpdateScrollbars(ME: TSynMemo);
begin
  ME.Scrollbars := ssNone;
  ME.Scrollbars := ssBoth;
end;

constructor TSynMemoUtil.Create(_Memo: TSynMemo);
begin
  inherited Create;
  FMemo := _Memo;
end;

procedure TSynMemoUtil.GetSelectedLines(out _StartIdx, _EndIdx: Integer);
begin
  _StartIdx := FMemo.BlockBegin.Line - 1;
  _EndIdx := FMemo.BlockEnd.Line - 1;
  if FMemo.BlockEnd.Char = 1 then
    Dec(_EndIdx);
  if _EndIdx < _StartIdx then
    _EndIdx := _StartIdx;
end;

class procedure TSynMemoUtil.ProcessAllLines(_Memo: TSynMemo);
var
  Processor: TSynMemoUtil;
begin
  Processor := Self.Create(_Memo);
  try
    Processor.ProcessAllLines;
  finally
    FreeAndNil(Processor);
  end;
end;

procedure TSynMemoUtil.ProcessAllLines;
var
  i: Integer;
begin
  FMemo.BeginUpdate;
  try
    for i := 0 to FMemo.Lines.Count - 1 do begin
      FMemo.Lines[i] := ProcessLine(FMemo.Lines[i]);
    end;
  finally
    FMemo.EndUpdate;
  end;

  TSynMemo_UpdateScrollbars(FMemo);
end;

class procedure TSynMemoUtil.ProcessSelectedLines(_Memo: TSynMemo);
var
  Processor: TSynMemoUtil;
begin
  Processor := Self.Create(_Memo);
  try
    Processor.ProcessSelectedLines;
  finally
    FreeAndNil(Processor);
  end;
end;

procedure TSynMemoUtil.ProcessSelectedLines;
var
  i: Integer;
  sl: TStringList;
  StartIdx: Integer;
  EndIdx: Integer;
  SelStart: Integer;
  Line: string;
  SelEnd: Integer;
begin
  if FMemo.Lines.Count = 0 then
    Exit; //==>

  GetSelectedLines(StartIdx, EndIdx);
  sl := TStringList.Create;
  try
    sl.Assign(FMemo.Lines);
    for i := StartIdx to EndIdx do begin
      sl[i] := ProcessLine(sl[i]);
    end;
    FMemo.Text := sl.Text;

    SelStart := FMemo.RowColToCharIndex(BufferCoord(1, StartIdx + 1));
    if EndIdx + 1 = FMemo.Lines.Count then begin
      Line := FMemo.Lines[EndIdx];
      SelEnd := FMemo.RowColToCharIndex(BufferCoord(Length(Line) + 1, EndIdx + 1));
    end else
      SelEnd := FMemo.RowColToCharIndex(BufferCoord(1, EndIdx + 2));
    FMemo.SelStart := SelStart;
    FMemo.SelEnd := SelEnd;
  finally
    FreeAndNil(sl);
  end;

  TSynMemo_UpdateScrollbars(FMemo);
end;

{ TSynMemoUtilMakePathsAbsoluteOrRelative }

constructor TSynMemoUtilMakePathsAbsoluteOrRelative.Create(_Memo: TSynMemo);
var
  ProjectFile: string;
begin
  inherited Create(_Memo);

  ProjectFile := GxOtaGetCurrentProjectFileName(True);
  if ProjectFile <> '' then
    FProjectDir := ExtractFilePath(ProjectFile)
  else
    FProjectDir := '';
end;

{ TSynMemoUtilMakePathsAbsolute }

function TSynMemoUtilMakePathsAbsolute.ProcessLine(const _Line: string): string;
begin
  if (FProjectDir <> '') and (_Line <> '') and not StartsText('$(BDS)\', _Line) then begin
    Result := TFileSystem.ExpandFileNameRelBaseDir(_Line, FProjectDir);
  end else
    Result := _Line;
end;

{ TSynMemoUtilMakePathsRelative }

function TSynMemoUtilMakePathsRelative.ProcessLine(const _Line: string): string;
begin
  if (FProjectDir <> '') and (_Line <> '') and not StartsText('$(BDS)\', _Line) then begin
    Result := ExtractRelativePath(AddSlash(FProjectDir), _Line);
  end else
    Result := _Line;
end;

{ TSynMemoUtilPrependDots }

function TSynMemoUtilPrependDots.ProcessLine(const _Line: string): string;
begin
  if (_Line <> '') and not StartsText('$(BDS)\', _Line) then begin
    Result := '..\' + _Line;
  end;
end;

{ TSynMemoUtilRemoveDots }

function TSynMemoUtilRemoveDots.ProcessLine(const _Line: string): string;
begin
  if LeftStr(_Line, 3) = '..\' then begin
    Result := Copy(_Line, 4);
  end else
    Result := _Line;
end;

end.

