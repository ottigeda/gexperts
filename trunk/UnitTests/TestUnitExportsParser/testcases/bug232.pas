unit bug232;

// Test case for bug #232:
// UnitExportsParser did not handle nested IFNDEF / ELSE correctly.
// When DELPHI2007_UP is defined, FooInElseBranch must be found.

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}
procedure FooInElseBranch;
{$ENDIF}

implementation

end.
