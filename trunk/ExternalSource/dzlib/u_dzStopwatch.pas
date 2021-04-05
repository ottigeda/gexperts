unit u_dzStopwatch;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  Windows,
  Classes,
  u_dzNullableTimespan,
  u_dzTypes;

type
  TStopwatch = record
  strict private
    FElapsedTicks: Int64;
    FIsRunning: Boolean;
    FStartTicks: Int64;
    function GetElapsedDateTimeTicks: Int64;
  public
    class function Create: TStopwatch; static;
    class function GetTimeStamp: Int64; static;
    procedure Reset;
    procedure Start;
    class function StartNew: TStopwatch; static;
    procedure Stop;
    function Elapsed: TNullableTimespan;
    function ElapsedMilliseconds: Int64;
    ///<summary>
    /// Elapsed Milliseconds as UInt32, cut off at MaxUInt32 </summary>
    function ElapsedMilliseconds32: UInt32;
    function ElapsedTicks: Int64;
    class function Frequency: Int64; static;
    class function IsHighResolution: Boolean; static;
    property IsRunning: Boolean read FIsRunning;
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  u_dzConvertUtils;

const
  TicksPerMillisecond = 10000;
  TicksPerSecond = TicksPerMillisecond * 1000;

var
  gblFrequency: Int64;
  gblIsHighResolution: Boolean;
  gblTickFrequency: Double;

{ TStopwatch }

class function TStopwatch.Create: TStopwatch;
begin
  Result.Reset;
end;

class function TStopwatch.Frequency: Int64;
begin
  Result := gblFrequency;
end;

function TStopwatch.Elapsed: TNullableTimespan;
begin
  Result.AssignMilliseconds(GetElapsedDateTimeTicks / TicksPerMillisecond);
end;

function TStopwatch.GetElapsedDateTimeTicks: Int64;
begin
  Result := ElapsedTicks;
  if gblIsHighResolution then
    Result := Trunc(Result * gblTickFrequency);
end;

function TStopwatch.ElapsedMilliseconds: Int64;
begin
  Result := GetElapsedDateTimeTicks div Int64(TicksPerMillisecond);
end;

function TStopwatch.ElapsedMilliseconds32: UInt32;
begin
  Result := ReduceToUInt32(ElapsedMilliseconds);
end;

function TStopwatch.ElapsedTicks: Int64;
begin
  Result := FElapsedTicks;
  if FIsRunning then
    Result := Result + GetTimeStamp - FStartTicks;
end;

class function TStopwatch.GetTimeStamp: Int64;
begin
  if gblIsHighResolution then
    QueryPerformanceCounter(Result)
  else
    Result := GetTickCount * Int64(TicksPerMillisecond);
end;

class function TStopwatch.IsHighResolution: Boolean;
begin
  Result := gblIsHighResolution;
end;

procedure TStopwatch.Reset;
begin
  FElapsedTicks := 0;
  FIsRunning := False;
  FStartTicks := 0;
end;

procedure TStopwatch.Start;
begin
  if not FIsRunning then begin
    FStartTicks := GetTimeStamp;
    FIsRunning := True;
  end;
end;

class function TStopwatch.StartNew: TStopwatch;
begin
  Result.Reset;
  Result.Start;
end;

procedure TStopwatch.Stop;
begin
  if FIsRunning then begin
    FElapsedTicks := FElapsedTicks + GetTimeStamp - FStartTicks;
    FIsRunning := False;
  end;
end;

procedure InitVariables;
begin
  if not QueryPerformanceFrequency(gblFrequency) then begin
    // no high performance timer available, use WinAPI GetTickCount
    gblIsHighResolution := False;
    gblFrequency := TicksPerSecond;
    gblTickFrequency := 1.0;
  end else begin
    gblIsHighResolution := True;
    gblTickFrequency := TicksPerSecond / gblFrequency;
  end;
end;

procedure Test;
var
  Stopwatch: TStopwatch;
begin
  Stopwatch.Reset;
  Stopwatch.Start;
  sleep(1000);
  Stopwatch.Stop;
  Stopwatch.Elapsed.InMicroseconds;
end;

initialization
  InitVariables;
//  Test;
{$ENDIF DELPHI2007_UP}
end.
