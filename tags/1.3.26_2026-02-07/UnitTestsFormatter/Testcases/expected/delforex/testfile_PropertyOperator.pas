unit testfile_PropertyOperator;

interface

type
  TSomeRec = record
  private
    FOperator: string;
  public
    class operator Add(_a, _b: integer): TSomeRec;
    property Operator: string read FOperator write FOperator;
  end;

implementation

end.