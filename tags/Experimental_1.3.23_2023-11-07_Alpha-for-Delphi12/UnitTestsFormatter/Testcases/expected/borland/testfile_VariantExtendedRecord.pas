unit testfile_VariantExtendedRecord;

interface

type
  // this doens't
  TExtendedVariantRec = record
    procedure Test;
    case Byte of
      1: (CoOrd: array[0..2] of Double);
      2: (X: Double;
          Y: Double;
          z: Double);
      3: (
          a: Double;
          b: Double;
          c: Double);
      4:
        (
          r: Double;
          s: Double;
          t: Double);
      5:
        (
          u, v: Double;
          w: Double
          );
  end;

var
  SomeVar: Integer;

implementation

procedure TSomeRec.Test;
begin
end;

end.
