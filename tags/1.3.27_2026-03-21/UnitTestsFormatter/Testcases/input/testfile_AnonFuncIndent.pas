unit testfile_AnonFuncIndent;

interface

implementation

procedure TestSetup;
begin
  FDateTimeFactory := TYockDateTimeFactory.Create;
  TYockDateTimeFactory(FDateTimeFactory).OnCurrentDateTime :=
    function: TDateTime
    begin
      Result := EncodeDateTime(2023, 01, 26, 15, 26, 16, 0);
    end;
end;

procedure TestSimpleAssignment;
begin
  Func :=
    function(X: Integer): Integer
    begin
      Result := X * 2;
    end;
end;

procedure TestDirectAssignment;
begin
  Proc := procedure
    begin
      DoSomething;
    end;
end;

procedure TestAsParameter;
begin
  SomeProc(
    procedure
    begin
      DoSomething;
    end);
end;

procedure TestMultipleAnon;
begin
  SomeProc(
    function: Integer
    begin
      Result := 1;
    end,
    procedure(X: Integer)
    begin
      DoSomething(X);
    end);
end;

end.
