unit testfile_AnonymousLast;

interface

implementation

procedure TForm2.FormCreate(Sender: TObject);
begin
  Execute(
    procedure()
    begin
      a := a + 1;
    end
    );
end;

end.
