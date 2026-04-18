unit testfile_useswithcomment;

interface

uses
{$IFDEF bla}  Sysutils, {$ENDIF}
  Classes; // comment 2
  
implementation

uses
Types, {$if True} Windows,{$ifend} Controls;

end.