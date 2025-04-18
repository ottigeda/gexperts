// stores the breakpoints while the source code is being formatted
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterBreakpoints;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  IniFiles,
  ToolsApi,
  GX_StringList,
  GX_CodeFormatterSourceLineList;

type
  TSavedBreakpoint = class
  private
    // IOTABreakpoint120
    FThreadCondition: string;
    // IOTABreakpoint120
    FStackFramesToLog: Integer;
    // IOTABreakpoint80
    FDoHandleExceptions: Boolean;
    FDoIgnoreExceptions: Boolean;
    // IOTABreakpoint50
    FGroupName: string;
    FDoBreak: Boolean;
    FLogMessage: string;
    FEvalExpression: string;
    FLogResult: Boolean;
    FEnableGroup: string;
    FDisableGroup: string;
    // IOTABreakpoint40
    FEnabled: Boolean;
    FExpression: string;
    FFileName: string;
    FLineNumber: Integer;
    FPassCount: Integer;
  public
    constructor Create(_SrcBrkPt: IOTABreakpoint);
    procedure AssignTo(_SrcBrkPt: IOTABreakpoint);
    procedure Dump(_INI: TCustomINifile; _Idx: Integer);
  end;

type
  TSavedBreakpointList = class
  private
    FList: TList;
    function GetItems(_Idx: Integer): TSavedBreakpoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TSavedBreakpoint);
    procedure Delete(_Idx: Integer);
    function Count: Integer;
    procedure Clear;
    function Find(const _Filename: string; _LineNumber: Integer; out _Idx: Integer): Boolean;
    property Items[_Idx: Integer]: TSavedBreakpoint read GetItems; default;
  end;

type
  TBreakpointHandler = class
  private
    FSavedBreakpoints: TSavedBreakpointList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>
    /// @param NewCode has a method FindNewLineNo for calculating the new line numbers for the
    ///                bookmarks. </summary>
    procedure RestoreItems(_NewCode: TSourceLineList);
    procedure SaveItems;
    procedure Dump(const _Filename: string);
  end;

implementation

uses
  Math,
  GX_GenericUtils;

{ TSavedBreakpoint }

constructor TSavedBreakpoint.Create(_SrcBrkPt: IOTABreakpoint);
begin
  inherited Create;

{$IFDEF GX_VER210_up}
  FThreadCondition := _SrcBrkPt.ThreadCondition;
{$ENDIF}
{$IFDEF GX_VER170_up}
  FStackFramesToLog := _SrcBrkPt.StackFramesToLog;
{$ENDIF}
{$IFDEF GX_VER160_up}
  FDoHandleExceptions := _SrcBrkPt.DoHandleExceptions;
  FDoIgnoreExceptions := _SrcBrkPt.DoIgnoreExceptions;
{$ENDIF}
  FGroupName := _SrcBrkPt.GroupName;
  FDoBreak := _SrcBrkPt.DoBreak;
  FLogMessage := _SrcBrkPt.LogMessage;
  FEvalExpression := _SrcBrkPt.EvalExpression;
  FLogResult := _SrcBrkPt.LogResult;
  FEnableGroup := _SrcBrkPt.EnableGroup;
  FDisableGroup := _SrcBrkPt.DisableGroup;
  FEnabled := _SrcBrkPt.Enabled;
  FExpression := _SrcBrkPt.Expression;
  FFileName := _SrcBrkPt.FileName;
  FLineNumber := _SrcBrkPt.LineNumber;
  FPassCount := _SrcBrkPt.PassCount;
end;

procedure TSavedBreakpoint.AssignTo(_SrcBrkPt: IOTABreakpoint);
begin
{$IFDEF GX_VER210_up}
  _SrcBrkPt.ThreadCondition := FThreadCondition;
{$ENDIF}
{$IFDEF GX_VER170_up}
  _SrcBrkPt.StackFramesToLog := FStackFramesToLog;
{$ENDIF}
{$IFDEF GX_VER160_up}
  _SrcBrkPt.DoHandleExceptions := FDoHandleExceptions;
  _SrcBrkPt.DoIgnoreExceptions := FDoIgnoreExceptions;
{$ENDIF}
  _SrcBrkPt.GroupName := FGroupName;
  _SrcBrkPt.DoBreak := FDoBreak;
  _SrcBrkPt.LogMessage := FLogMessage;
  _SrcBrkPt.EvalExpression := FEvalExpression;
  _SrcBrkPt.LogResult := FLogResult;
  _SrcBrkPt.EnableGroup := FEnableGroup;
  _SrcBrkPt.DisableGroup := FDisableGroup;
  _SrcBrkPt.Enabled := FEnabled;
  _SrcBrkPt.Expression := FExpression;
  _SrcBrkPt.FileName := FFileName;
  _SrcBrkPt.LineNumber := FLineNumber;
  _SrcBrkPt.PassCount := FPassCount;
end;

procedure TSavedBreakpoint.Dump(_INI: TCustomINifile; _Idx: Integer);
begin
  _INI.WriteString('Breakpoints', 'Breakpoint' + IntToStr(_Idx),
    '''' + FFileName + '''' + ','
    + IntToStr(FLineNumber) + ','
    + '''' + FExpression + '''' + ','
    + IntToStr(FPassCount) + ','
    + IntToStr(IfThen(FEnabled, 1)) + ','
    + '''' + FGroupName + '''' + ','
    + IntToStr(IfThen(FDoBreak, 1)) + ','
    + IntToStr(IfThen(FDoIgnoreExceptions, 1)) + ','
    + IntToStr(IfThen(FDoHandleExceptions, 1)) + ','
    + '''' + FLogMessage + '''' + ','
    + IntToStr(IfThen(FLogResult, 1)) + ','
    + '''' + FEvalExpression + '''' + ','
    + '''' + FEnableGroup + '''' + ','
    + '''' + FDisableGroup + '''' + ','
    + IntToStr(FStackFramesToLog) + ','
    + '''' + FThreadCondition + ''''
    );
end;

{ TSavedBreakpointList }

constructor TSavedBreakpointList.Create;
begin
  inherited;
  FList := TList.Create;
end;

procedure TSavedBreakpointList.Delete(_Idx: Integer);
begin
  FList.Delete(_Idx);
end;

destructor TSavedBreakpointList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TSavedBreakpoint(FList[i]).Free;
  FreeAndNil(FList);
  inherited;
end;

function TSavedBreakpointList.Find(const _Filename: string; _LineNumber: Integer;
  out _Idx: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    if SameText(Items[i].FFileName, _Filename) and (Items[i].FLineNumber = _LineNumber) then begin
      _Idx := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TSavedBreakpointList.GetItems(_Idx: Integer): TSavedBreakpoint;
begin
  Assert((_Idx > -1) and (_Idx < FList.Count));
  Result := FList[_Idx];
end;

procedure TSavedBreakpointList.Add(Item: TSavedBreakpoint);
begin
  Assert(Assigned(FList));
  FList.Add(Item);
end;

function TSavedBreakpointList.Count: Integer;
begin
  Assert(Assigned(FList));
  Result := FList.Count;
end;

procedure TSavedBreakpointList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TSavedBreakpoint(FList[i]).Free;
  FList.Clear;
end;

{ TBreakpointHandler }

constructor TBreakpointHandler.Create;
begin
  inherited;
  FSavedBreakpoints := TSavedBreakpointList.Create;
end;

destructor TBreakpointHandler.Destroy;
begin
  FreeAndNil(FSavedBreakpoints);
  inherited;
end;

procedure TBreakpointHandler.RestoreItems(_NewCode: TSourceLineList);
var
  DebuggerServices: IOTADebuggerServices;
  i: Integer;
  SrcBrkPtInt: IOTABreakpoint;
  SavedBreakpoint: TSavedBreakpoint;
  Idx: Integer;
  NewLineNo: Integer;
begin
  if BorlandIDEServices.QueryInterface(IOTADebuggerServices, DebuggerServices) <> S_OK then
    Exit; //==>

  for i := 0 to DebuggerServices.SourceBkptCount - 1 do begin
    SrcBrkPtInt := DebuggerServices.SourceBkpts[i];
    // todo: Is this correct? We are looking for any breakpoint at the new line number.
    //       Shouldn't we look for
    if FSavedBreakpoints.Find(SrcBrkPtInt.FileName, SrcBrkPtInt.LineNumber, Idx) then
      FSavedBreakpoints.Delete(Idx);
  end;

  for i := 0 to FSavedBreakpoints.Count - 1 do begin
    SavedBreakpoint := FSavedBreakpoints[i];
    try
      _NewCode.FindNewLineNo(SavedBreakpoint.FLineNumber, NewLineNo);
      SavedBreakpoint.FLineNumber := NewLineNo;
      SrcBrkPtInt := DebuggerServices.NewSourceBreakpoint(
        SavedBreakpoint.FFileName, SavedBreakpoint.FLineNumber,
        DebuggerServices.CurrentProcess);
      SavedBreakpoint.AssignTo(SrcBrkPtInt);
    except
      Break; // FIXME: Delphi 6/7 do not support recreation of source breakpoints at designtime (EListError: List index out of bounds (-1))
    end;
  end;
end;

procedure TBreakpointHandler.SaveItems;
var
  DebuggerServices: IOTADebuggerServices;
  i: Integer;
  SrcBrkPtInt: IOTABreakpoint;
  SavedBreakpoint: TSavedBreakpoint;
begin
  FSavedBreakpoints.Clear;

  if BorlandIDEServices.QueryInterface(IOTADebuggerServices, DebuggerServices) <> S_OK then
    Exit; //==>

  for i := 0 to DebuggerServices.SourceBkptCount - 1 do begin
    SrcBrkPtInt := DebuggerServices.SourceBkpts[i];
    SavedBreakpoint := TSavedBreakpoint.Create(SrcBrkPtInt);
    FSavedBreakpoints.Add(SavedBreakpoint);
  end;
//  Dump('d:\breakpoints.ini');
end;

procedure TBreakpointHandler.Dump(const _Filename: string);
var
  INI: TIniFile;
  i: Integer;
  SavedBreakpoint: TSavedBreakpoint;
begin
  INI := TIniFile.Create(_Filename);
  try
    for i := 0 to FSavedBreakpoints.Count - 1 do begin
      SavedBreakpoint := FSavedBreakpoints[i];
      SavedBreakpoint.Dump(INI, i);
    end;
  finally
    FreeAndNil(INI);
  end;
end;

end.

