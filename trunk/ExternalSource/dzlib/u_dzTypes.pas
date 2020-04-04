unit u_dzTypes;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Types; // for $IF Declared(TBytes)

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
{$IF not Declared(RawByteString)}
  RawByteString = AnsiString;
{$IFEND}

type
  TByteMatrix = array of array of Byte;
  TBitMatrix = array of array of Boolean;

  TSingleMatrix = array of array of Single;
  TDoubleMatrix = array of array of Double;

implementation

end.

