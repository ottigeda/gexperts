unit u_dzDpiScaleUtilsDummy;

interface

type
  TDummyDpiScaler = class
  public
    function Calc(_Value: Integer): Integer;
  end;

implementation

{ TDummyDpiScaler }

function TDummyDpiScaler.Calc(_Value: Integer): Integer;
begin
  Result := _Value;
end;

end.
