unit testfile_GenericProcTypes

interface

type
  TMyProc<T> = procedure(Param: T);
  TMyProc2<Y> = procedure(Param1, Param2: Y) of object;

type
  TFoo = class
    procedure Test;
    procedure MyProc(X, Y: Integer);
  end;

implementation

procedure Sample(Param: Integer);
begin
  Writeln(Param);
end;

procedure TFoo.MyProc(X, Y: Integer);
begin
  Writeln('X:', X, ', Y:', Y);
end;

procedure TFoo.Test;
var
  X: TMyProc<Integer>;
  Y: TMyProc2<Integer>;
begin
  X := Sample;
  X(10);
  Y := MyProc;
  Y(20, 30);
end;

var
  F: TFoo;
begin
  F := TFoo.Create;
  F.Test;
  F.Free;
end.