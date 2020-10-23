unit u_dzTypes;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes,
  Types; // for $IF Declared(TBytes) and TStringDynArray

type
  // Fixed size signed and unsigned integer types
{$IF not declared(Int8)}
  Int8 = Shortint;
{$IFEND}
{$IF not declared(UInt8)}
  UInt8 = Byte;
{$IFEND}
{$IF not declared(Int16)}
  Int16 = Smallint;
{$IFEND}
{$IF not declared(UInt16)}
  UInt16 = Word;
{$IFEND}
{$IF not declared(Int32)}
  Int32 = Integer;
{$IFEND}
{$IF not declared(UInt32)}
  UInt32 = Cardinal;
{$IFEND}
  // Int64 is predefined
{$IF not declared(UInt64)}
  UInt64 = Int64;
{$IFEND}
  // UInt64 is predefined

{$IF not declared(PInt8)}
  PInt8 = ^Int8;
{$IFEND}
{$IF not declared(PUInt8)}
  PUInt8 = ^UInt8;
{$IFEND}
{$IF not declared(PInt16)}
  PInt16 = ^Int16;
{$IFEND}
{$IF not declared(PUInt16)}
  PUInt16 = ^UInt16;
{$IFEND}
{$IF not declared(PInt32)}
  PInt32 = ^Int32;
{$IFEND}
{$IF not declared(PUInt32)}
  PUInt32 = ^UInt32;
{$IFEND}
{$IF not declared(PInt64)}
  PInt64 = ^Int64;
{$IFEND}
{$IF not declared(PUInt64)}
  PUInt64 = ^UInt64;
{$IFEND}

{$IF not declared(NativeUInt)}
  NativeUInt = UInt32;
{$IFEND}

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
  TUInt16Array = array of UInt16;

type
  TByteMatrix = array of array of Byte;
  TBitMatrix = array of array of Boolean;

  TSingleMatrix = array of array of Single;
  TDoubleMatrix = array of array of Double;

type
  TMethodPointer = procedure of object;

implementation

end.
