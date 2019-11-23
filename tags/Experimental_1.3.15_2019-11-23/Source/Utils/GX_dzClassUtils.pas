/// <summary>
/// This is an extract from u_dzClassUtils in dzlib http://blog.dummzeuch.de/dzlib/ </summary>
unit GX_dzClassUtils;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  IniFiles,
  GX_GenericUtils;

///<summary>
/// Assigns st to sl and sorts it.
/// sl.Objects contains the index into st+1
///            so st[Integer(ls.Objects[i]-1)] = sl[i] </summary>
procedure TStrings_GetAsSortedList(_st: TStrings; _sl: TStringList; _Duplicates: TDuplicates = dupAccept);

procedure TStrings_FreeWithObjects(_List: TStrings);

/// <summary>
/// Frees all objects stored in the TStrings intance and returns the instance,
/// meant to be called like
/// @code( TStrings_FreeAllObjects(sl).Free; ) or
/// @code( TStrings_FreeAllObjects(sl).Clear; ) </summary>
function TStrings_FreeAllObjects(_Strings: TStrings): TStrings;

///<summary>
/// assign the current index to the Objects property and then sort the list </summary>
procedure TStringList_MakeIndex(_sl: TStringList);

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);

function TStrings_ValueFromIndex(_st: TStrings; _Idx: Integer): string;

function TStringList_CreateSorted(_Duplicates: TDuplicates = dupError): TStringList;

///<summary>
/// Like TComponent.FindComponent but optionally search recursively because in the IDE
/// sometimes not all controls are owned by the form (e.g. in the Project Options dialog).
/// This will do a depth first search.
/// Name can be an empty string, which will return the first component with an empty name.</sumamry>
function TComponent_FindComponent(_Owner: TComponent; const _Name: string; _Recursive: Boolean;
  out _Found: TComponent; _CmpClass: TComponentClass = nil): Boolean;

///<summary>
/// Checks both, the code and data pointer of a Method and returns true, if both are equal </summary>
function IsSameMethod(_Method1, _Method2: TNotifyEvent): Boolean;

procedure TList_FreeWithObjects(_List: TList);

type
  TIniSection = class
  private
    FIni: TMemIniFile;
    FSection: string;
  public
    constructor Create(const _Filename: string; const _Section: string);
    destructor Destroy; override;
    procedure Clear;
    function SectionExists: Boolean;
    function ReadBool(const _Ident: string; _Default: Boolean = False): Boolean;
    function ReadInteger(const _Ident: string; _Default: Integer = -1): Integer;
    function ReadString(const _Ident: string; const _Default: string = ''): string;
    procedure WriteBool(const _Ident: string; _Value: Boolean);
    procedure WriteInteger(const _Ident: string; _Value: Integer);
    procedure WriteString(const _Ident: string; const _Value: string);
    procedure UpdateFile;
  end;

implementation

procedure TList_FreeWithObjects(_List: TList);
var
  i: Integer;
begin
  if Assigned(_List) then begin
    for i := 0 to _List.Count - 1 do
      TObject(_List[i]).Free;
    FreeAndNil(_List);
  end;
end;

procedure TStrings_FreeWithObjects(_List: TStrings);
var
  i: Integer;
begin
  if Assigned(_List) then
    for i := 0 to _List.Count - 1 do begin
      _List.Objects[i].Free;
      _List.Objects[i] := nil;
    end;
  _List.Free;
end;

function TStrings_FreeAllObjects(_Strings: TStrings): TStrings;
var
  i: Integer;
begin
  Result := _Strings;
  if not Assigned(_Strings) then
    Exit; //==>
  for i := 0 to _Strings.Count - 1 do begin
    _Strings.Objects[i].Free;
    _Strings.Objects[i] := nil;
  end;
end;

function IsSameMethod(_Method1, _Method2: TNotifyEvent): Boolean;
begin
  Result := (TMethod(_Method1).Code = TMethod(_Method2).Code)
    and (TMethod(_Method1).Data = TMethod(_Method2).Data);
end;

function TComponent_FindComponent(_Owner: TComponent; const _Name: string; _Recursive: Boolean;
  out _Found: TComponent; _CmpClass: TComponentClass = nil): Boolean;
var
  i: Integer;
  comp: TComponent;
begin
  Result := False;
  if (_Owner = nil) then
    Exit;

  for i := 0 to _Owner.ComponentCount - 1 do begin
    comp := _Owner.Components[i];
    if SameText(comp.Name, _Name) then begin
      Result := not Assigned(_CmpClass) or (comp is _CmpClass);
      if Result then begin
        _Found := comp;
        Exit;
      end;
    end;
    if _Recursive then begin
      Result := TComponent_FindComponent(comp, _Name, _Recursive, _Found, _CmpClass);
      if Result then
        Exit;
    end;
  end;
end;

procedure TStrings_GetAsSortedList(_st: TStrings; _sl: TStringList; _Duplicates: TDuplicates = dupAccept);
begin
  Assert(Assigned(_st));
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  _sl.Assign(_st);
  TStringList_MakeIndex(_sl);
end;

function TStrings_ValueFromIndex(_st: TStrings; _Idx: Integer): string;
var
  Name: string;
begin
  Assert(Assigned(_st));

  Name := _st.Names[_Idx];
  Result := _st.Values[Name];
end;

procedure TStringList_MakeIndex(_sl: TStringList);
var
  i: Integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := Pointer(i + 1);
  _sl.Sorted := True;
end;

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);
var
  i: Integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := Pointer(i + 1);
  _sl.Sorted := True;
end;

function TStringList_CreateSorted(_Duplicates: TDuplicates): TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := _Duplicates;
end;

{ TIniSection }

procedure TIniSection.Clear;
begin
  FIni.EraseSection(FSection);
end;

constructor TIniSection.Create(const _Filename, _Section: string);
begin
  FIni := TMemIniFile.Create(_Filename);
  FSection := _Section;
end;

destructor TIniSection.Destroy;
begin
  if Assigned(FIni) then begin
    FIni.UpdateFile;
    FreeAndNil(FIni);
  end;
  inherited;
end;

function TIniSection.ReadBool(const _Ident: string; _Default: Boolean = False): Boolean;
var
  s: string;
  IntValue: Integer;
begin
  s := FIni.ReadString(FSection, _Ident, '');
  if TryStrToInt(s, IntValue) then
    Result := (IntValue <> 0)
  else begin
    Result := SameText(s, 'TRUE') or SameText(s, 'T') or SameText(s, 'Yes') or SameText(s, 'Y');
  end;
end;

function TIniSection.ReadInteger(const _Ident: string; _Default: Integer): Integer;
begin
  Result := FIni.ReadInteger(FSection, _Ident, _Default);
end;

function TIniSection.ReadString(const _Ident, _Default: string): string;
begin
  Result := FIni.ReadString(FSection, _Ident, _Default);
end;

function TIniSection.SectionExists: Boolean;
begin
  Result := FIni.SectionExists(FSection);
end;

procedure TIniSection.UpdateFile;
begin
  FIni.UpdateFile;
end;

function Bool2Str(_Value: Boolean): string;
begin
  if _Value then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TIniSection.WriteBool(const _Ident: string; _Value: Boolean);
begin
  // -> True  / False
  FIni.WriteString(FSection, _Ident, Bool2Str(_Value));
end;

procedure TIniSection.WriteInteger(const _Ident: string; _Value: Integer);
begin
  FIni.WriteInteger(FSection, _Ident, _Value);
end;

procedure TIniSection.WriteString(const _Ident, _Value: string);
begin
  FIni.WriteString(FSection, _Ident, _Value);
end;

end.
