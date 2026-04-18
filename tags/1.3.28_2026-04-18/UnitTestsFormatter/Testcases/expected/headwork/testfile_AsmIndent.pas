unit testfile_AsmIndent;
// Working as designed: The formatter preserves asm block indentation as-is.
// It does not attempt to fix or normalize indentation inside asm..end blocks.
// If the input asm code is incorrectly indented, it will remain so in the output.
// See testfile_AsmCorrectInput.pas for an example with correct input indentation.

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
