This is the JExperts source taken from
https://sourceforge.net/projects/jexperts/
plus those parts of the GExperts source that it requires.
The original ZIP file is also included for reference, just in case
it gets removed from sourceforge.

I have partly ported it from Delphi 5/6 to compile with Delphi 2007.
Note: It requires the JCL/JVCL to be installed!
And many of the components it originally used have long been deprecated.
I have replaced most of them with the ones from the current JVCL but
not all could be simple renamed, so some functionality got lost.

Apparently there is a DLL that is supposed to be loaded into the Delphi IDE
by a package.

The DLL compiles (but I have commented out lots of code), the package doesn't.
I could never get it to work.

Some of the functionality overlaps with the GExperts, there might be intersting
stuff for additional functions, but it turned out to be too much work for me
so I lost interest.

But maybe somebody else has more luck + time + interest and wants to pick up here,
so I put the code into the GExperts repository.
-- 2017-02-05 twm
