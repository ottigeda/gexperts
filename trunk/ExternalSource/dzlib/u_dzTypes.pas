unit u_dzTypes;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
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

type
  TMethodPointer = procedure of object;

function TStringArray_FromStrings(_sl: TStrings): TStringArray;

implementation

function TStringArray_FromStrings(_sl: TStrings): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, _sl.count);
  for i := 0 to _sl.count - 1 do
    Result[i] := _sl[i];
end;

end.

