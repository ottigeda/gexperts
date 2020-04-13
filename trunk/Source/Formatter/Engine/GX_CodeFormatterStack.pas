///<summary>
/// defines a stack and a stackstack for the code formatter based on a pseudo template
/// Original Author: Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
/// Contributors:    Thomas Mueller (http://www.dummzeuch.de)
///                  Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features
///</summary>

unit GX_CodeFormatterStack;

{$I GX_CondDefine.inc}

// This unit uses Assert(False, 'some text') for trace logging (for the line numbers)
// Therefore we must turn off Assertions by default.
{$C-}
{$IFDEF ASSERT_TRACING}
// Assertions should only be turned on in the unit tests
{$C+}
{$ENDIF}

interface

uses
  GX_CodeFormatterTypes;

type
  PStackRec = ^TStackRec;
  TStackRec = record
    RT: TReservedType;
    nInd: Integer;
  end;

const
  MaxStack = 150;
type
  TStackArray = array[0..MaxStack] of TStackRec;

type
  TCodeFormatterSegment = class
  private
    FStackPtr: Integer;
    FNIndent: Integer;
    FProcLevel: Integer;
    FStack: TStackArray;
    function GetTopRec: PStackRec;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>
    /// @returns the topmost item from the stack without removing it
    ///          if the stack is empty, it returns rtNothing. </summary>
    function GetTopType: TReservedType;{$IFDEF SupportsInline} inline; {$ENDIF}
    function GetTopIndent: Integer;{$IFDEF SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// like GetTopType, but takes an index, Idx = 0 is equivalent to GetTopType,
    /// Idx=1 returns the next etc. </summary>
    function GetType(_Idx: Integer): TReservedType;{$IFDEF SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// Check whether _Type is somewhere on the stack </summary>
    function HasType(_Type: TReservedType): Boolean;{$IFDEF SupportsInline} inline; {$ENDIF}
    function Pop: TReservedType;
    procedure Push(_Type: TReservedType; _IncIndent: Integer);
    ///<summary>
    /// @returns True if the stack is empty </summary>
    function IsEmpty: Boolean;{$IFDEF SupportsInline} inline; {$ENDIF}
    ///<summary>
    /// clears the stack and returns the number of items that were left </summary>
    function Clear: Integer;{$IFDEF SupportsInline} inline; {$ENDIF}
    function Depth: Integer;{$IFDEF SupportsInline} inline; {$ENDIF}
    ///<sumamry>
    /// Creates a new stack and copies all elements to it </summary>
    function Clone: TCodeFormatterSegment;
    property NIndent: Integer read FNIndent write FNIndent;
    property ProcLevel: Integer read FProcLevel write FProcLevel;
  end;

{$DEFINE STACK_TEMPLATE}
type
  _STACK_ITEM_ = TCodeFormatterSegment;
const
  _MAX_DEPTH_ = 150;
{$INCLUDE DelforStackTemplate.tpl}

type
  TCodeFormatterStack = class(_STACK_)
  end;

implementation

uses
  TypInfo,
  SysUtils;

{ TCodeFormatterSegment }

constructor TCodeFormatterSegment.Create;
begin
  inherited Create;
  FStackPtr := -1;
  FNIndent := 0;
  FProcLevel := 0;
end;

destructor TCodeFormatterSegment.Destroy;
begin
  inherited;
end;

function TCodeFormatterSegment.GetTopType: TReservedType;
begin
  if FStackPtr >= 0 then
    Result := GetTopRec.RT
  else
    Result := rtNothing;
end;

function TCodeFormatterSegment.GetType(_Idx: Integer): TReservedType;
begin
  if FStackPtr >= _Idx then
    Result := FStack[FStackPtr - _Idx].RT
  else
    Result := rtNothing;
end;

procedure TCodeFormatterSegment.Push(_Type: TReservedType; _IncIndent: Integer);
var
  tr: PStackRec;
begin
  Assert(False, 'Stack.Push: Type: ' + GetEnumname(TypeInfo(TReservedType), Ord(_Type))
    + ' IncIndent: ' + IntToStr(_IncIndent));

  if FStackPtr >= MaxStack then
    raise EFormatException.Create('Stack overflow');

  Inc(FStackPtr);
  tr := GetTopRec;
  tr.RT := _Type;
  tr.nInd := FNIndent;
  FNIndent := FNIndent + _IncIndent;
end;

function TCodeFormatterSegment.HasType(_Type: TReservedType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FStackPtr do
    if FStack[i].RT = _Type then begin
      Result := True;
      Exit;
    end;
end;

function TCodeFormatterSegment.Pop: TReservedType;
var
  tr: PStackRec;
begin
  if FStackPtr >= 0 then begin
    tr := GetTopRec;
    Assert(False, 'Stack.Pop: ' + GetEnumname(TypeInfo(TReservedType), Ord(tr.RT)));
    FNIndent := tr.nInd;
    if (tr.RT = rtProcedure) and (FProcLevel > 0) then
      Dec(FProcLevel);

    Result := tr^.RT;
    Dec(FStackPtr);
  end else begin
    Assert(False, 'Stack.Pop: empty stack');
    FNIndent := 0;
    FProcLevel := 0;
    Result := rtNothing;
  end;
end;

function TCodeFormatterSegment.GetTopRec: PStackRec;
begin
  Result := @FStack[FStackPtr];
end;

function TCodeFormatterSegment.GetTopIndent: Integer;
begin
  if not IsEmpty then begin
    Result := GetTopRec.nInd;
  end else
    Result := NIndent;
end;

function TCodeFormatterSegment.IsEmpty: Boolean;
begin
  Result := FStackPtr < 0;
end;

function TCodeFormatterSegment.Clear: Integer;
begin
  Result := Depth;
  FStackPtr := -1;
  FNIndent := 0;
  FProcLevel := 0;
end;

function TCodeFormatterSegment.Depth: Integer;
begin
  Result := FStackPtr + 1;
end;

function TCodeFormatterSegment.Clone: TCodeFormatterSegment;
begin
  Result := TCodeFormatterSegment.Create;
  Result.FStack := FStack;
  Result.FStackPtr := FStackPtr;
  Result.FNIndent := FNIndent;
  Result.FProcLevel := FProcLevel;
end;

{ TCodeFormatterStack }

{$INCLUDE DelforStackTemplate.tpl}

end.
