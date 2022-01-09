unit u_dzDpiScaleUtilsDummy;

interface

uses
  SysUtils,
  Forms;

type
  TFormDpiScaler = class
  public
    constructor Create(_Form: TForm);
    function Calc(_Value: Integer): Integer;
  end;

implementation

{ TFormDpiScaler }

constructor TFormDpiScaler.Create(_Form: TForm);
begin
  inherited Create;
end;

function TFormDpiScaler.Calc(_Value: Integer): Integer;
begin
  Result := _Value;
end;

end.
