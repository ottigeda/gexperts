unit testfile_NoReturnDirective;

interface

procedure FatalError(const Msg: string); noreturn;
procedure RaiseAbort; noreturn;
procedure ExitApplication(Code: Integer); noreturn;

// Combined with other directives
procedure CriticalFailure(const Msg: string); stdcall; noreturn;
procedure ExternalAbort; cdecl; noreturn; external 'somelib';

type
  TErrorHandler = class
    class procedure Fatal(const Msg: string); static; noreturn;
    procedure InstanceFatal; virtual; noreturn;
  end;

implementation

procedure FatalError(const Msg: string);
begin
  WriteLn('Fatal: ', Msg);
  Halt(1);
end;

procedure RaiseAbort;
begin
  raise EAbort.Create('Aborted');
end;

procedure ExitApplication(Code: Integer);
begin
  Halt(Code);
end;

procedure CriticalFailure(const Msg: string);
begin
  raise Exception.Create('Critical: ' + Msg);
end;

class procedure TErrorHandler.Fatal(const Msg: string);
begin
  raise Exception.Create('Class Fatal: ' + Msg);
end;

procedure TErrorHandler.InstanceFatal;
begin
  raise Exception.Create('Instance fatal');
end;

// Local procedure not exported from unit
procedure LocalNoReturn; noreturn;
begin
  raise Exception.Create('Local error');
end;

procedure AnotherLocalNoReturn(Code: Integer); noreturn;
begin
  Halt(Code);
end;

procedure TestNestedNoReturn;

  procedure NestedFatal; noreturn;
  begin
    raise Exception.Create('Nested fatal');
  end;

  procedure DeeplyNested;

    procedure InnerNoReturn(const Msg: string); noreturn;
    begin
      raise EInvalidOperation.Create(Msg);
    end;

  begin
    InnerNoReturn('Deep error');
  end;

begin
  if  ParamCount > 0 then
    NestedFatal;
  DeeplyNested;
end;

procedure MultipleNestedNoReturn;
var
  SomeVar: Integer;

  procedure FirstNested; noreturn;
  begin
    raise Exception.Create('First');
  end;

  procedure SecondNested(Value: Integer); noreturn;
  begin
    Halt(Value);
  end;

begin
  SomeVar := 1;
  if  SomeVar = 1 then
    FirstNested
  else
    SecondNested(SomeVar);
end;

end.
