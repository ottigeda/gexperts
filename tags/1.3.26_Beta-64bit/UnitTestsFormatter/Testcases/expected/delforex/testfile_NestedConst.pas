unit testfile_NestedClass;

interface


type
  TClass = class(TObject)
  private const
    CConst = 1;
  private
    FSomeField: integer;
  private const const2 = 5;
    FAnotherField: string;
  end;

  TSomeConsts = class
  public const
    Const1 = 'Const1';
    Const2 = 2;
  end;

const
  GConst = 1;

implementation

end.
