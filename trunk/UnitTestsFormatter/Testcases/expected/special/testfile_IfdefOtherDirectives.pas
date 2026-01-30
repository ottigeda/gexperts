unit testfile_IfdefOtherDirectives;
// Working as designed: When IndentCompDirectives=icdIndependent, ALL directives
// inside IFDEF blocks are indented based on nesting level, not just conditionals.
// This is intentional because directives like {$L}, {$R}, {$I} can be conditionally
// included/excluded, so showing them indented helps visualize the nesting structure.

interface

{$IFDEF WIN32}
uses
  SysUtils;

  {$L adler32.obj}
  {$R myresource.res}
  {$I myinclude.inc}

{$ENDIF}

implementation

end.
