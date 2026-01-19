unit testfile_BlankBeforeUsesWithComment;
interface

// Comment about interface uses
uses
  System.SysUtils,
  System.Classes;

type
  TMyClass = class
  public
    procedure DoSomething;
  end;

implementation

{ Multi-line comment
  about implementation uses }
uses
  System.Math;

{ TMyClass }

procedure TMyClass.DoSomething;
begin
  // Something
end;

end.
