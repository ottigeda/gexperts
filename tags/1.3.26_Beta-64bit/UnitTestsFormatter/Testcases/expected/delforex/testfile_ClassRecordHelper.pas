unit ClassRecordHelper;

interface


type
  TMyClass = class
    procedure MyProc;
    function MyFunc: Integer;
  end;

type
  TMyRecord = record
    procedure MyProc;
    function MyFunc: Integer;
  end;

type
  TMyClassHelper = class helper for TMyClass
    procedure HelloWorld;
    function MyFunc: Integer;
  end;

type
  TMyRecordHelper = record helper for TMyRecord
    procedure HelloWorld;
    function MyFunc: Integer;
  end;

implementation

procedure TMyClass.MyProc;
var X: Integer;
begin
  X := MyFunc;
end;

function TMyClass.MyFunc: Integer;
begin
      // ...
end;

procedure TMyClassHelper.HelloWorld;
begin
  Writeln(Self.ClassName); // Self refers to TMyClass type, not TMyClassHelper
end;

function TMyClassHelper.MyFunc: Integer;
begin
  ...
end;

procedure TMyRecord.MyProc;
var X: Integer;
begin
  X := MyFunc;
end;

function TMyRecord.MyFunc: Integer;
begin
      // ...
end;

procedure TMyRecordHelper.HelloWorld;
begin
  Writeln(Self.ClassName); // Self refers to TMyRecord type, not TMyRecordHelper
end;

function TMyRecordHelper.MyFunc: Integer;
begin
      // ...
end;


var
  X: TMyClass;
  r: TMyRecord;
begin
  X := TMyClass.Create;
  X.MyProc; // Calls TMyClass.MyProc
  X.HelloWorld; // Calls TMyClassHelper.HelloWorld
  X.MyFunc; // Calls TMyClassHelper.MyFunc

  r.MyProc; // Calls TMyRecord.MyProc
  r.HelloWorld; // Calls TMyRecordHelper.HelloWorld
  r.MyFunc; // Calls TMyRecordHelper.MyFunc
end.