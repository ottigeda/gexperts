unit GX_UsesManager;

{$I GX_CondDefine.inc}
{$I dzlib.inc}

// Add to, remove from, and query the uses clauses of Object Pascal units
// Orginal Author: Krzysztof Jez - krzysztofj@bms.com.pl
// Modified by Erik Berry to remove external dependencies and cleanup
// These routines do not support C++ source

// note: Even though TUsesManager.Create takes a parameter specifying the unit to work on
//       this is only used in the parser. Any of the methodes manipulating the uses list
//       always work on the current source editor.

// note: In theory we should be using TGXUnicodeString and TGXUnicodeStringList instad of
//       string and TStringList/TStrings so non-UNICODE Delphi versions (< Delphi 2009) can
//       handle file and unit names with unicode characters.
//       Unfortunately this would break a lot of code and I have just decided that it is not
//       worth the offort. These > 10 years old Delphi versions will continue to be broken
//       the way they have been for years. As far as I know, nobody has complained about this.
//       -- 2025-05-14 twm

interface

uses Classes, Contnrs, ToolsAPI, GX_GenericUtils, mPasLex;

type
  TUsesItem = class(TObject)
    Name: string;
    BeginPos: Longint;
    EndPos: Longint; // Position at the end of the unit name
    CommaBeforePos: Longint; // Position of ',' before unit name
    CommaAfterPos: Longint;  // Position of ',' after unit name
  end;

  TUsesList = class(TObjectList)
  private
    function GetItem(AIndex: GXNativeInt): TUsesItem;
    procedure SetItem(AIndex: GXNativeInt; const Value: TUsesItem);
  public
    function Add: TUsesItem;
    procedure AssignTo(List: TStrings);
    function IndexOf(const AUnitName: string): Integer;
    property Items[AIndex: GXNativeInt]: TUsesItem read GetItem write SetItem;
  end;

  TUsesStatus = (usNonExisting, usInterface, usImplementation, usInsideUnit);
  TPosInUsesList = (puInterface, puImplementation, puNo);
  TWhichUsesList = puInterface..puImplementation;

  TUsesClause = class
  public
    // Position of the last char of the "interface" and the "implementation" keyword
    SECTION: Integer;
    // Position of first char of "uses" keyword
    BegOfUses: Integer;
    // Position of the semicolon which ends uses clause
    EndOfUses: Integer;
    Units: TUsesList;
    constructor Create;
    destructor Destroy; override;
  end;

  TUsesManager = class(TObject)
  private
    FFileContent: string;
    FUsesClause: array[TWhichUsesList] of TUsesClause;
    FFileName: string;
    procedure BuildUsesList;
    function GetCurrentUnitName: string;
    function UsesLineWouldBeTooLong(InsertPos, InsertLength: Integer): Boolean;
    procedure InternalInit;
    procedure InternalReplaceInUses(UsesList: TUsesList; const AOldUnitName, ANewUnitName: string);
    function InternalAddToUsesSection(const AUnitName: string; _Which: TWhichUsesList): Boolean;
    procedure InternalRemoveFromUses(_Which: TWhichUsesList; const AUnitName: string);
    function GetUsesList(_Which: TWhichUsesList): TUsesList;
    class procedure InternalGetUnits(Units: TStrings; _Which: TWhichUsesList);
  public
    class function GetIDERightMargin: Integer;
    // These act on the current source editor
    class function UseUnitInImplementation(const AUnitName: string): boolean;
    class function UseUnitInInterface(const AUnitName: string): Boolean;
    class function GetUnitUsesStatus(const AUnitName: string): TUsesStatus;
    class procedure GetImplementationUnits(Units: TStrings);
    class procedure GetInterfaceUnits(Units: TStrings);
    class procedure RemoveUnitFromImplementation(const AUnitName: string);
    class procedure RemoveUnitFromInterface(const AUnitName: string);
    class procedure ReplaceUnitInImplementation(const AOldUnitName, ANewUnitName: string);
    class procedure ReplaceUnitInInterface(const AOldUnitName, ANewUnitName: string);
    ///<summary>
    /// Deletes the existing uses clause and inserts NewUsesText at the same position.
    /// NewUsesText must include the 'uses' keyword through trailing ';'. </summary>
    class procedure RewriteUsesClause(_Which: TWhichUsesList; const _NewUsesText: string);
  public
    constructor Create(const SourceEditor: IOTASourceEditor); overload;
    constructor Create(const FileName: string); overload;
    destructor Destroy; override;
    function GetUsesStatus(const AUnitName: string): TUsesStatus;
    function AddToImpSection(const AUnitName: string): Boolean;
    function AddToIntSection(const AUnitName: string): Boolean;
    function IsPositionBeforeImplementation(Pos: Integer): Boolean;
    function IsPositionInUsesList(Pos: integer): TPosInUsesList;
    procedure AddUnits(AUnits: TStrings; _Which: TWhichUsesList);
    procedure RemoveFromImplUses(const AUnitName: string);
    procedure RemoveImplementationUses;
    procedure RemoveFromIntfUses(const AUnitName: string);
    procedure RemoveInterfaceUses;
    procedure ReplaceInImplUses(const AOldUnitName, ANewUnitName: string);
    procedure ReplaceInIntUses(const AOldUnitName, ANewUnitName: string);
    property ImplementationUses: TUsesList index puImplementation read GetUsesList;
    property InterfaceUses: TUsesList index puInterface read GetUsesList;
  end;

implementation


uses
  SysUtils,
{$IFDEF HAS_UNIT_CHARACTER}
  Character,
{$ENDIF}
  GX_OtaUtils,
  mwPasParserTypes;

class function TUsesManager.UseUnitInImplementation(const AUnitName: string): boolean;
var
  um: TUsesManager;
begin
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    Result := um.AddToImpSection(AUnitName);
  finally
    um.free;
  end;
end;

class function TUsesManager.UseUnitInInterface(const AUnitName: string): Boolean;
var
  um: TUsesManager;
begin
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    Result := um.AddToIntSection(AUnitName);
  finally
    um.Free;
  end;
end;

class function TUsesManager.GetUnitUsesStatus(const AUnitName: string): TUsesStatus;
var
  um: TUsesManager;
begin
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    Result := um.GetUsesStatus(AUnitName);
  finally
    um.Free;
  end;
end;

class procedure TUsesManager.InternalGetUnits(Units: TStrings; _Which: TWhichUsesList);
var
  UsesList: TUsesList;
  um: TUsesManager;
begin
  Assert(Assigned(Units));
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    UsesList := um.GetUsesList(_Which);
    UsesList.AssignTo(Units);
  finally
    um.Free;
  end;
end;

class procedure TUsesManager.GetImplementationUnits(Units: TStrings);
begin
  InternalGetUnits(Units, puImplementation);
end;

class procedure TUsesManager.GetInterfaceUnits(Units: TStrings);
begin
  InternalGetUnits(Units, puInterface);
end;

class procedure TUsesManager.RemoveUnitFromImplementation(const AUnitName: string);
var
  um: TUsesManager;
begin
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    um.RemoveFromImplUses(AUnitName);
  finally
    um.Free;
  end;
end;

class procedure TUsesManager.RemoveUnitFromInterface(const AUnitName: string);
var
  um: TUsesManager;
begin
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    um.RemoveFromIntfUses(AUnitName);
  finally
    um.Free;
  end;
end;

class procedure TUsesManager.ReplaceUnitInImplementation(const AOldUnitName, ANewUnitName: string);
var
  um: TUsesManager;
begin
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    um.ReplaceInImplUses(AOldUnitName, ANewUnitName);
  finally
    um.Free;
  end;
end;

class procedure TUsesManager.ReplaceUnitInInterface(const AOldUnitName, ANewUnitName: string);
var
  um: TUsesManager;
begin
  um := TUsesManager.Create(Ota.GetCurrentSourceEditor);
  try
    um.ReplaceInIntUses(AOldUnitName, ANewUnitName);
  finally
    um.Free;
  end;
end;

class procedure TUsesManager.RewriteUsesClause(_Which: TWhichUsesList; const _NewUsesText: string);
var
  um: TUsesManager;
  BegPos, EndPos, DeleteCount: Integer;
  SourceEditor: IOTASourceEditor;
begin
{$IFNDEF GX_DELPHI10_2_UP}
  // Suppress W1036 "Variable might not have been initialized" in Delphi 10.1 and earlier.
  // Delphi 10.2+ sees through the try/finally and doesn't warn, but emits H2077 instead.
  DeleteCount := 0;
{$ENDIF}
  SourceEditor := Ota.GetCurrentSourceEditor;
  um := TUsesManager.Create(SourceEditor);
  try
    BegPos := um.FUsesClause[_Which].BegOfUses;
    EndPos := um.FUsesClause[_Which].EndOfUses;
    if (BegPos = 0) or (EndPos = 0) then
      raise Exception.Create('RewriteUsesClause: Uses clause not found');
    DeleteCount := EndPos - BegPos;
  finally
    um.Free;
  end;
  // Delete old uses clause and insert new text
  GxOtaDeleteTextFromPos(BegPos, DeleteCount, SourceEditor);
  Ota.InsertTextIntoEditorAtCharPos(_NewUsesText, BegPos, SourceEditor);
end;

constructor TUsesManager.Create(const SourceEditor: IOTASourceEditor);
begin
  inherited Create;

  Assert(Assigned(SourceEditor));
  FFileName := SourceEditor.FileName;
  FFileContent := Ota.ReadEditorTextToString(SourceEditor.CreateReader);
  InternalInit;
end;

constructor TUsesManager.Create(const FileName: string);
var
  sl: TStringList;
begin
  inherited Create;

  FFileName := FileName;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FFileName);
    FFileContent := sl.Text;
  finally
    FreeAndNil(sl);
  end;
  InternalInit;
end;

procedure TUsesManager.InternalInit;
begin
  FUsesClause[puInterface] := TUsesClause.Create;
  FUsesClause[puImplementation] := TUsesClause.Create;

  BuildUsesList;
end;

destructor TUsesManager.Destroy;
begin
  FreeAndNil(FUsesClause[puInterface]);
  FreeAndNil(FUsesClause[puImplementation]);

  inherited Destroy;
end;

function TUsesManager.AddToImpSection(const AUnitName: string): Boolean;
begin
  Result := InternalAddToUsesSection(AUnitName, puImplementation);
end;

function TUsesManager.AddToIntSection(const AUnitName: string): Boolean;
begin
  Result := InternalAddToUsesSection(AUnitName, puInterface);
end;

// Add a unit to a uses clause (may require removing it from the other clause)
function TUsesManager.InternalAddToUsesSection(const AUnitName: string; _Which: TWhichUsesList): Boolean;
var
  InsertPosition: Integer;
  LastUses: TUsesItem;
  InsertString: string;
  Status: TUsesStatus;
  UsesItems: TUsesList;
  UsesPos: Integer;
begin
  Result := False;
  Status := GetUsesStatus(AUnitName);
  if Status = usInsideUnit then
    Exit; //==>

  case _Which of
    puInterface: begin
        if Status = usInterface then
          Exit; //==>
        if Status = usImplementation then
          RemoveFromImplUses(AUnitName);
        UsesPos := FUsesClause[puInterface].Section;
        UsesItems := InterfaceUses;
      end;
    puImplementation: begin
        if Status in [usInterface, usImplementation] then
          Exit;
        UsesPos := FUsesClause[puImplementation].Section;
        UsesItems := ImplementationUses;
      end;
  else
    Exit; //==>
  end;

  if UsesPos = 0 then
    Exit;

  // If a uses item exists
  if UsesItems.Count > 0 then
  begin
    // Retrieve the position after the last uses item
    LastUses := UsesItems.Items[UsesItems.Count - 1];
    InsertPosition := LastUses.EndPos;
    InsertString := ', ' + AUnitName;
    if UsesLineWouldBeTooLong(InsertPosition, Length(InsertString)) then
      InsertString := ',' + sLineBreak + '  ' + AUnitName;
    // Insert the new unit name into the uses clause
    Ota.InsertTextIntoEditorAtCharPos(InsertString, InsertPosition);
  end
  else // The uses clause does not exist
  begin
    InsertString := sLineBreak + sLineBreak +'uses'+ sLineBreak +'  '+ AUnitName +';';
    Ota.InsertTextIntoEditorAtCharPos(InsertString, UsesPos);
  end;

  // This needs to be done last since it changes the implementation offsets
  if _Which = puImplementation then
  begin
    if Status = usInterface then
      RemoveFromIntfUses(AUnitName);
  end;

  Result := True;
end;

procedure TUsesManager.AddUnits(AUnits: TStrings; _Which: TWhichUsesList);
var
  UnitName: string;
  i: Integer;
begin
  for i := 0 to AUnits.Count - 1 do
  begin
    UnitName := AUnits.Strings[i];
    case _Which of
      puInterface:
        AddToIntSection(UnitName);
      puImplementation:
        AddToImpSection(UnitName);
    end;
  end;
end;

procedure TUsesManager.BuildUsesList;
var
  Section: (sImplementation, sInterface);
  InUses: Boolean;
  UsesItem: TUsesItem;
  LastCommaPos: Integer;
  Parser: TmwPasLex;
begin
  Parser := TmwPasLex.Create;
  try
    Parser.Origin := @FFileContent[1];
    Section := sInterface;
    InUses := False;
    FUsesClause[puImplementation].BegOfUses := 0;
    FUsesClause[puImplementation].Section := 0;

    FUsesClause[puInterface].Section := 0;
    FUsesClause[puInterface].EndOfUses := 0;
    FUsesClause[puInterface].BegOfUses := 0;

    UsesItem := nil;
    LastCommaPos := 0;

    Parser.NextNoJunk;
    while Parser.TokenID <> tkNull do
    begin
      case Parser.TokenID of
        tkInterface:
          begin
            Section := sInterface;
            FUsesClause[puInterface].Section := Parser.RunPos;
            InUses := False;
            LastCommaPos := 0;
          end;
        tkImplementation:
          begin
            Section := sImplementation;
            FUsesClause[puImplementation].Section := Parser.RunPos;
            InUses := False;
            LastCommaPos := 0;
          end;
        tkUses:
          begin
            InUses := True;
            if Section = sImplementation then
              FUsesClause[puImplementation].BegOfUses := Parser.RunPos - Length('uses');
            if Section = sInterface then
              FUsesClause[puInterface].BegOfUses := Parser.RunPos - Length('uses');
            LastCommaPos := 0;
          end;
      else
        // If it is after the unit identifier
        if InUses and not (Parser.TokenID in [tkCompDirect, tkIn, tkString]) then
        begin
          if Parser.TokenID = tkIdentifier then
          begin
            if Section = sInterface then
              UsesItem := InterfaceUses.Add
            else // Section = sImplementation
              UsesItem := ImplementationUses.Add;
            {$IFOPT D+} Assert(UsesItem <> nil); {$ENDIF}

            UsesItem.Name := Parser.GetDottedIdentifierAtPos(True);
            UsesItem.EndPos := Parser.RunPos;
            UsesItem.BeginPos := UsesItem.EndPos - Length(UsesItem.Name);

            if LastCommaPos <> 0 then
              UsesItem.CommaBeforePos := LastCommaPos - 1;

            UsesItem.CommaAfterPos := 0;
          end // tkIdentifier
          else if Parser.TokenID = tkComma then
          begin
            LastCommaPos := Parser.RunPos;
            if UsesItem <> nil then
            begin
              UsesItem.CommaAfterPos := LastCommaPos - 1;
            end;
          end
          else // FParser.TokenID <> tkComma
          begin
            InUses := False;
            if Section = sImplementation then
            begin
              FUsesClause[puImplementation].EndOfUses := Parser.RunPos;
              Break; // End of parsing
            end;
            if Section = sInterface then
              FUsesClause[puInterface].EndOfUses := Parser.RunPos;
          end; // Not comma
        end; // UsesFlag
      end;
      Parser.NextNoJunk;
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

function TUsesManager.GetCurrentUnitName: string;
begin
  Result := Trim(ExtractPureFileName(FFileName));
end;

function TUsesManager.GetUsesList(_Which: TWhichUsesList): TUsesList;
begin
  Result := FUsesClause[_Which].Units;
end;

function TUsesManager.GetUsesStatus(const AUnitName: string): TUsesStatus;
begin
  if SameFileName(GetCurrentUnitName, Trim(AUnitName)) then
    Result := usInsideUnit
  else if InterfaceUses.IndexOf(AUnitName) > -1 then
    Result := usInterface
  else if ImplementationUses.IndexOf(AUnitName) > -1 then
    Result := usImplementation
  else
    Result := usNonExisting;
end;

{
  Remove a unit from a uses clause
  Algorithm to remove Unit1:
  If the unit at the start of the uses clause:
    uses Unit1;  // Only unit in the uses clause
     -> Remove the whole uses clause
    uses Unit1, Unit2; // There is a unit after Unit1
     -> Remove the unit name and trailing comma
  If the unit is the last one in the uses clause:
    uses Unit2, Unit1;
    -> Remove the unit name and comma before it
  If the unit the middle of the uses clause:
    uses Unit2, Unit1, Unit3; // Comma directly after the unit name
    -> The Unit name and trailing comma are deleted
    uses Unit2 ,Unit1 ,Unit3; // Comma not directly after the unit name
    -> The Unit name and preceeding comma are deleted
}
procedure TUsesManager.InternalRemoveFromUses(_Which: TWhichUsesList; const AUnitName: string);

{$IFDEF DEPRECATED_TCHARACTER}
  function IsWhiteSpace(const _c: Char): Boolean;
  begin
    Result := _c.IsWhiteSpace;
  end;
{$ENDIF}

{$IF not declared(IsWhiteSpace)}
  function IsWhiteSpace(const _c: Char): Boolean;
  begin
  // SPACE, TAB,LF,LINE-TAB,FF,CR
    Result := CharInSet(_c, [#$20, #$09..#$0D]);
  end;
{$IFEND}

var
  DeletedUnit: TUsesItem;
  UnitIndex: Integer;
  BegPos, EndPos: Integer;
  UsesList: TUsesList;
begin
  UsesList := FUsesClause[_Which].Units;
  UnitIndex := UsesList.IndexOf(AUnitName);
  if UnitIndex < 0 then
    Exit; //==>

  if UsesList.Count = 1 then
  begin
    // This is the only unit in the list -> we delete the whole clause
    case _Which of
      puInterface:
        RemoveInterfaceUses;
      puImplementation:
        RemoveImplementationUses;
    end;
    Exit; //==>
  end;

  DeletedUnit := UsesList.Items[UnitIndex];
  if UnitIndex = 0 then begin
    // First in the uses clause
    if DeletedUnit.CommaAfterPos <> 0 then
      EndPos := DeletedUnit.CommaAfterPos + 1
    else
      EndPos := DeletedUnit.EndPos;
    BegPos := DeletedUnit.BeginPos;
  end else if UnitIndex = UsesList.Count-1 then begin
    // Last in the uses clause
    EndPos := DeletedUnit.EndPos;
    if DeletedUnit.CommaBeforePos <> 0 then
      BegPos := DeletedUnit.CommaBeforePos
    else
      BegPos := DeletedUnit.BeginPos;
  end else begin
    // In the middle of the uses clause
    if DeletedUnit.CommaAfterPos = DeletedUnit.EndPos then begin
      // Comma directly after unit
      BegPos := DeletedUnit.BeginPos;
      EndPos := DeletedUnit.CommaAfterPos + 1;
    end else begin
      // Comma before unit
      if DeletedUnit.CommaBeforePos <> 0 then
        BegPos := DeletedUnit.CommaBeforePos
      else
        BegPos := DeletedUnit.BeginPos;
      EndPos := DeletedUnit.EndPos;
    end;
  end;

  while IsWhiteSpace(FFileContent[EndPos + 1]) do
    Inc(EndPos);

  GxOtaDeleteTextFromPos(BegPos, EndPos - BegPos);
end;

// Remove the whole implementation uses clause
procedure TUsesManager.RemoveImplementationUses;
var
  BegIndex, Count: Integer;
begin
  if (FUsesClause[puImplementation].BegOfUses = 0) or (FUsesClause[puImplementation].EndOfUses = 0) then
    raise Exception.Create('RemoveImplementationUses: Begin or End of uses clause is not available!');

  BegIndex := FUsesClause[puImplementation].BegOfUses;
  Count := FUsesClause[puImplementation].EndOfUses - BegIndex;
  GxOtaDeleteTextFromPos(BegIndex, Count);
end;

procedure TUsesManager.RemoveFromImplUses(const AUnitName: string);
begin
  InternalRemoveFromUses(puImplementation, AUnitName);
end;

procedure TUsesManager.RemoveFromIntfUses(const AUnitName: string);
begin
  InternalRemoveFromUses(puInterface, AUnitName);
end;

// Remove the whole interface uses clause
procedure TUsesManager.RemoveInterfaceUses;
var
  BegIndex, Count: Integer;
begin
  if (FUsesClause[puInterface].BegOfUses = 0) or (FUsesClause[puInterface].EndOfUses = 0) then
    raise Exception.Create('RemoveInterfaceUses: Begin or End of uses clause is not available!');

  BegIndex := FUsesClause[puInterface].BegOfUses;
  Count := FUsesClause[puInterface].EndOfUses - BegIndex;
  GxOtaDeleteTextFromPos(BegIndex, Count);
end;

procedure TUsesManager.InternalReplaceInUses(UsesList: TUsesList; const AOldUnitName, ANewUnitName: string);
var
  ReplaceUnit: TUsesItem;
  UnitIndex: Integer;
  BegPos, EndPos: Integer;
  SourceEditor: IOTASourceEditor;
begin
  UnitIndex := UsesList.IndexOf(AOldUnitName);
  if UnitIndex = -1 then
    exit;

  ReplaceUnit := UsesList.Items[UnitIndex];
  SourceEditor := Ota.GetCurrentSourceEditor;

  EndPos := ReplaceUnit.EndPos;
  BegPos := ReplaceUnit.BeginPos;
  GxOtaDeleteTextFromPos(BegPos, EndPos - BegPos, SourceEditor);
  Ota.InsertTextIntoEditorAtCharPos(ANewUnitName, BegPos, SourceEditor);
end;

procedure TUsesManager.ReplaceInImplUses(const AOldUnitName, ANewUnitName: string);
begin
  InternalReplaceInUses(ImplementationUses, AOldUnitName, ANewUnitName);
end;

procedure TUsesManager.ReplaceInIntUses(const AOldUnitName, ANewUnitName: string);
begin
  InternalReplaceInUses(InterfaceUses, AOldUnitName, ANewUnitName);
end;

function TUsesList.Add: TUsesItem;
begin
  Result := TUsesItem.Create;

  inherited Add(Result);
end;

function TUsesList.IndexOf(const AUnitName: string): Integer;
begin
  Result := Count - 1;
  while Result >= 0 do
  begin
    if SameText(Items[Result].Name, AUnitName) then
      Break;
    Dec(Result);
  end;
end;

procedure TUsesList.AssignTo(List: TStrings);
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to Count - 1 do
    List.Add(Items[i].Name);
end;

function TUsesList.GetItem(AIndex: GXNativeInt): TUsesItem;
begin
  Result := TUsesItem(Get(AIndex));
end;

procedure TUsesList.SetItem(AIndex: GXNativeInt; const Value: TUsesItem);
begin
  Put(AIndex, Value);
end;

class function TUsesManager.GetIDERightMargin: Integer;
var
  EditBuffer: IOTAEditBuffer;
begin
  Result := 80; // fallback
  if Ota.TryGetTopMostEditBuffer(EditBuffer) and Assigned(EditBuffer.BufferOptions) then
    Result := EditBuffer.BufferOptions.RightMargin;
  if Result <= 0 then
    Result := 80;
end;

function TUsesManager.UsesLineWouldBeTooLong(InsertPos, InsertLength: Integer): Boolean;
var
  EditView: IOTAEditView;
  InsertCharPos: TOTACharPos;
  InsertEditPos: TOTAEditPos;
begin
  EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView));
  InsertCharPos := Ota.GetCharPosFromPos(InsertPos, EditView);
  EditView.ConvertPos(False, InsertEditPos, InsertCharPos);
  Result := (InsertEditPos.Col + InsertLength) > GetIDERightMargin;
end;

function TUsesManager.IsPositionBeforeImplementation(Pos: Integer): Boolean;
begin
  Result := FUsesClause[puImplementation].Section > Pos;
end;

function TUsesManager.IsPositionInUsesList(Pos: Integer): TPosInUsesList;
begin
  if (FUsesClause[puInterface].BegOfUses + Length('uses') < Pos) and (Pos < FUsesClause[puInterface].EndOfUses) then
    Result := puInterface
  else if (FUsesClause[puImplementation].BegOfUses + Length('uses') < Pos) and (Pos < FUsesClause[puImplementation].EndOfUses) then
    Result := puImplementation
  else
    Result := puNo;
end;

{ TPositionClass }

constructor TUsesClause.Create;
begin
  inherited Create;
  Units := TUsesList.Create;
end;

destructor TUsesClause.Destroy;
begin
  FreeAndNil(Units);
  inherited;
end;

end.

