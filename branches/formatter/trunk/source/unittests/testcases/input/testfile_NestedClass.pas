unit testfile_NestedClass;

interface

type
  TOuterClass = class
  strict private
    myField: Integer;
  public
  type
    TInnerClass = class
    public
      myInnerField: Integer;
      procedure innerProc;
    end;
    procedure outerProc;
  end;

implementation

{ TOuterClass }

procedure TOuterClass.outerProc;
begin

end;

{ TOuterClass.TInnerClass }

procedure TOuterClass.TInnerClass.innerProc;
begin

end;

end.

