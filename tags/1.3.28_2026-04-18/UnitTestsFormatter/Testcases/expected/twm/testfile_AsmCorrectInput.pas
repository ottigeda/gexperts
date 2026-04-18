unit testfile_AsmCorrectInput;
// Working as designed: The formatter preserves asm block indentation as-is.
// This test shows that correctly indented asm code remains correctly indented.
// The nop instruction is indented one level deeper than the asm keyword.

interface

type
  MInteger = array[0..1] of Byte;

  TInt16 = record
    FValue: Smallint;
    procedure AssignMotorola(_Value: MInteger);
  end;

implementation

function Swap16(w: Word): Word;
begin
  Result := w;
end;

procedure TInt16.AssignMotorola(_Value: MInteger);
begin
  if _Value[1] and $F0 > 0 then
    asm
      nop
    end;
  FValue := Smallint(Swap16(Word(_Value)));
end;

end.
