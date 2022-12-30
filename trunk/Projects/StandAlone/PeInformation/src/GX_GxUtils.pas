unit GX_GxUtils;

interface

uses
  Controls;

procedure GxSetDefaultFont(Control: TControl);

implementation

uses
  GX_GenericUtils;

procedure GxSetDefaultFont(Control: TControl);
begin
  SetDefaultFont(Control);
end;

end.
