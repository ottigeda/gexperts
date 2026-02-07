unit testfile_SimpleUnit;

interface

  type
    TMyClass = class
    private
      FValue: Integer;
    public
      property Value: Integer read FValue write FValue;
    end;

implementation

  procedure LocalProc;
  begin
    // local procedure
  end;

end.
