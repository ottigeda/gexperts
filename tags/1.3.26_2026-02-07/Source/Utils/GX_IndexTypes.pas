unit GX_IndexTypes;

interface

type
{$IFDEF WIN64}
  TGXListIndex = NativeInt;
{$ELSE}
  TGXListIndex = Integer;
{$ENDIF}

implementation

end.
