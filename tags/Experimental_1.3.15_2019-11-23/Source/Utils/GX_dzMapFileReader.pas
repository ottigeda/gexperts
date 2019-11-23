unit GX_dzMapFileReader;

// This is u_dzMapFileReader in dzlib http://blog.dummzeuch.de/dzlib/
// @author(Thomas Mueller http://www.dummzeuch.de)

interface

uses
  SysUtils,
  Classes;

type
  ///<summary>
  /// Reads and interprets a .map file generated by Delphi
  /// For now only creates a list of units from the "Detailed map of segments" which
  /// is available even in the smallest variant of that file. </summary>
  TMapFileReader = class
  private
    FFilename: string;
    FContent: TStringList;
    FUnits: TStringList;
    procedure Parse;
    procedure ParseSegments(_StartIdx: Integer);
  public
    constructor Create(const _Filename: string);
    destructor Destroy; override;
    property Filename: string read FFilename;
    property Content: TStringList read FContent;
    property Units: TStringList read FUnits;
  end;

implementation

uses
  GX_GenericUtils;

{ TMapFileReader }

constructor TMapFileReader.Create(const _Filename: string);
begin
  inherited Create;
  FFilename := _Filename;
  FUnits := TStringList.Create;
  FUnits.Sorted := True;
  FContent := TStringList.Create;
  FContent.LoadFromFile(FFilename);
  Parse;
end;

destructor TMapFileReader.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FUnits);
  inherited;
end;

procedure TMapFileReader.Parse;
var
  i: Integer;
  s: string;
begin
  for i := 0 to FContent.Count - 1 do begin
    s := FContent[i];
    if s = 'Detailed map of segments' then begin
      ParseSegments(i + 2);
      Exit;
    end;
  end;
end;

function TryCutAt(const _SubStr: string; var _Str: string): Boolean;
var
  StartPos: Integer;
  p: Integer;
begin
  StartPos := Pos(_SubStr, _Str);
  Result := (StartPos > 0);
  if Result then begin
    p := StartPos + Length(_SubStr);
    _Str := Copy(_Str, p, Length(_Str) - p + 1);
  end;
end;

procedure TMapFileReader.ParseSegments(_StartIdx: Integer);
const
  StrACBP = ' ACBP=';
  StrALIGN = ' ALIGN=';
var
  i: Integer;
  s: string;
  p: Integer;
  AlignStr: string;
begin
  if StrContains(StrACBP, FContent.Text) then begin
    // 32-bit platform
    AlignStr := StrACBP;
  end else if StrContains(StrALIGN, FContent.Text) then begin
    // 64-bit platform
    AlignStr := StrALIGN;
  end else
    Exit; //==>

  for i := _StartIdx to FContent.Count - 1 do begin
    s := FContent[i];
    if s = '' then
      Exit; //==>
    if TryCutAt(' M=', s) then begin
      p := Pos(AlignStr, s);
      if p > 0 then begin
        s := Copy(s, 1, p);
        s := Trim(s);
        FUnits.Add(s);
      end;
    end;
  end;
end;

end.
