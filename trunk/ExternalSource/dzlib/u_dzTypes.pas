unit u_dzTypes;

{$INCLUDE 'dzlib.inc'}

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
{$IF not Declared(TBytes)}
  TBytes = array of Byte;
{$IFEND}

implementation

end.

