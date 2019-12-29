unit u_dzTypes;

interface

uses
  SysUtils;

type
  EdzException = class(Exception)
  end;

type
  TErrorHandlingEnum = (ehReturnFalse, ehRaiseException);

type
  TStringArray = array of string;
  TIntegerArray = array of Integer;
  TSingleArray = array of Single;
  TDoubleArray = array of Double;
  TExtendedArray = array of Extended;

implementation

end.

