unit previewcapitalization;
{ Demonstrates capitalization options }

interface

uses
  sysutils, classes;

type
  tdemo = class(tobject)
  private
    fvalue: integer;
  protected
    procedure setvvalue(avalue: integer); virtual;
  public
    property value: integer read fvalue write setvalue;
  end;

implementation

{$r *.dfm}
{$ifdef debug}
{$define logging}
{$endif}

procedure tdemo.setvalue(avalue: integer);
const
  maxvalue = $ff;
  hexnum = $abcdef;
begin
  if avalue <= maxvalue then
    fvalue := avalue
  else
    fvalue := maxvalue;
end;

function createinstance: tdemo;
begin
  result := tdemo.create;
  result.value := 42;
end;

end.
