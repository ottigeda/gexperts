unit GrepMain;

interface

procedure Main;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  GX_GrepExpert;

procedure Main;
var
  Dir: AnsiString;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  try
    SetErrorMode(SEM_FAILCRITICALERRORS);
    if ParamCount > 0 then begin
      Dir := AnsiString(ParamStr(1));
      ShowGrepEx(0, 0, PAnsiChar(Dir), SW_SHOWNORMAL);
    end else begin
      ShowGrep;
    end;
    // it worked, halt the program
    Halt(0);
  except
    on e: Exception do begin
      Application.ShowException(e);
    end;
  end;
end;

end.
