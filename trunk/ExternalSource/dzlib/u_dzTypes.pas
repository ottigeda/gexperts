unit u_dzTypes;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  Types; // for $IF Declared(TBytes) and TStringDynArray

type
  EdzException = class(Exception)
  end;

type
  TErrorHandlingEnum = (ehReturnFalse, ehRaiseException);

{$IF not Declared(RawByteString)}
type
  RawByteString = AnsiString;
{$IFEND}

type
{$IF not declared(TStringDynArray)}
  TStringDynArray = array of string;
{$IFEND}
  TStringArray = TStringDynArray;
  TRawByteStringArray = array of RawByteString;
  TIntegerArray = array of Integer;
  TSingleArray = array of Single;
  TDoubleArray = array of Double;
  TExtendedArray = array of Extended;
{$IF not Declared(TBytes)}
  TBytes = array of Byte;
{$IFEND}

type
  TByteMatrix = array of array of Byte;
  TBitMatrix = array of array of Boolean;

  TSingleMatrix = array of array of Single;
  TDoubleMatrix = array of array of Double;

type
  TMethodPointer = procedure of object;

implementation

end.
