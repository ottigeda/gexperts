unit testfile_BlankBeforeUses;
interface

uses
  System.SysUtils,
  System.Classes;

type
  TMyClass = class
  public
    procedure DoSomething;
  end;

implementation

uses
  System.Math;

{ TMyClass }

procedure TMyClass.DoSomething;
begin
  // Something
end;

end.
