unit u_dzTypeInfoUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  TypInfo;

function SafeGetEnumName(_TypeInfo: PTypeInfo; _Value: Integer): string;

implementation

uses
  u_dzTypes;

function SafeGetEnumName(_TypeInfo: PTypeInfo; _Value: Integer): string;

  procedure IncPtr(var _P: Pointer);
  begin
    _P := Pointer(IntPtr(_P) + 1);
  end;

  // P must point to the length field (that is the first byte) of a ShortString
  function AfterShortString(const _P: Pointer): Pointer;
  begin
    Result := Pointer(IntPtr(_P) + PByte(_P)^ + 1);
  end;

{$IFDEF ENUM_NAMES_ARE_UTF8}

{$IFNDEF HAS_UTF8TOWIDESTRING}
  function UTF8ToWideString(const _Utf8: RawByteString): WideString;
  begin
    Result := UTF8Decode(_Utf8);
  end;
{$ENDIF}

// Older Unicode Delphi versions did not have a UTF8ToString overload for a pointer parameter
{$IFNDEF DELPHIX_ATHENS_UP}
  function _UTF8ToString(_P: Pointer): WideString;
  var
    Len: Byte;
    Buf: UTF8String;
  begin
    Result := '';
    Len := PByte(_P)^;
    if Len <> 0 then begin
      SetLength(Buf, Len);
      IncPtr(_P);
      Move(PByte(_P)^, Buf[1], Len);
      Result := UTF8ToWideString(Buf);
    end;
  end;
{$ENDIF}
{$ENDIF}

var
  P: Pointer;
  T: PTypeData;
begin
  if _TypeInfo^.Kind = tkInteger then begin
    Result := IntToStr(_Value);
    Exit;
  end;
  T := GetTypeData(GetTypeData(_TypeInfo)^.BaseType^);
  if (_TypeInfo = System.TypeInfo(Boolean)) or (T^.MinValue < 0) then begin
    { LongBool/WordBool/ByteBool have MinValue < 0 and arbitrary
      content in Value; Boolean has Value in [0, 1] }
    Result := BooleanIdents[_Value <> 0];
    if SameText(HexDisplayPrefix, '0x') then
      Result := LowerCase(Result);
  end else begin
    if (_Value < T.MinValue) or (_Value > T.MaxValue) then
      Result := Format('OutOfBounds(%d)', [_Value])
    else begin
      P := @T^.NameList;
      while _Value <> 0 do begin
        P := AfterShortString(P);
        Dec(_Value);
      end;
{$IFDEF ENUM_NAMES_ARE_UTF8}
      Result := _UTF8ToString(P);
{$ELSE}
      Result := PShortString(P)^;
{$ENDIF}
    end;
  end;
end;

end.
