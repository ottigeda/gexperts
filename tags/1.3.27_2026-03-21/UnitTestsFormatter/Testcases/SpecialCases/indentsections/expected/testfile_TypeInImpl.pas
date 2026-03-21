unit testfile_TypeInImpl;

interface

  type
    TMyClass = class
    private
      FValue: Integer;
    public
      property Value: Integer read FValue write FValue;
    end;

implementation

  type
    THelperRec = record
      X: Integer;
      Y: Integer;
    end;

  var
    GlobalHelper: THelperRec;

  procedure DoSomething;
  begin
    GlobalHelper.X := 1;
  end;

  function GetValue: Integer;
  begin
    Result := GlobalHelper.Y;
  end;

end.
