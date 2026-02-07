unit testfile_ReadWrite;

interface

type
  TMyClass = class
  private
    FField: Integer;
    procedure Read;
    procedure Write;
    function Default: Integer;
    function Stored: Boolean;
  published
    property SomeField: Integer read FField write FField stored False default 0;
  end;

implementation

procedure TMyClass.Read;
begin
end;

procedure TMyClass.Write;
begin
end;

function TMyClass.Default: Integer;
begin
  Result := 0;
end;

function TMyClass.Stored: Boolean;
begin
  Result := False;
end;

end.
