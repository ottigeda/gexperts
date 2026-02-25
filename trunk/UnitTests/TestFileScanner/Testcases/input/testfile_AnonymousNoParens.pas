unit testfile_AnonymousNoParens;

interface

implementation

procedure TForm2.FormDestroy(Sender: TObject);
begin
  Execute(
    procedure
    begin
      a := a + 1;
    end
    );
end;

procedure TForm2.FormHide(Sender: TObject);
begin
end;

end.
