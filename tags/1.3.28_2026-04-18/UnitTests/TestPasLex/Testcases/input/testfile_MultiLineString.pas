unit MultiLineString;

interface

procedure Method1;
procedure Method2;
procedure Method3;

implementation

procedure Method1;
begin
end;

procedure Method2;
var
  SQL: string;
begin
  SQL := '''
    select count(*) as CNT
    from SOME_TABLE
    ''';
end;

procedure Method3;
begin
end;

end.
