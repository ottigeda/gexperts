unit testfile_RecordFieldsWithComma;

interface

type
  TSomeRecord = record
    a,
      b, // bla
      c,
      d: integer;
  end;

implementation

end.