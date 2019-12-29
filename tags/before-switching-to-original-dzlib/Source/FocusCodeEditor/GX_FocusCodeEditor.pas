unit GX_FocusCodeEditor;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  GX_Experts;

type
  TFocusCodeEditorExpert = class(TGX_Expert)
  public
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function GetHelpString: string; override;
  end;

implementation

uses
  Menus,
  Forms,
  Controls,
  GX_dzVclUtils;

{ TFocusCodeEditorExpert }

procedure TFocusCodeEditorExpert.Execute(Sender: TObject);
var
  i: Integer;
  frm: TForm;
  j: Integer;
  cmp: TComponent;
begin
  for i := 0 to Screen.FormCount - 1 do begin
    frm := Screen.Forms[i];
    if frm.Name = 'EditWindow_0' then begin
      frm.BringToFront;
      for j := 0 to frm.ComponentCount - 1 do begin
        cmp := frm.Components[j];
        if cmp.ClassNameIs('TEditControl') then begin
          TWinControl_SetFocus(TWinControl(cmp));
          Exit; //==>
        end;
      end;
    end;
  end;
end;

function TFocusCodeEditorExpert.GetActionCaption: string;
begin
  Result := 'Focus Code Editor';
end;

function TFocusCodeEditorExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(VK_F6, [ssShift]);
end;

function TFocusCodeEditorExpert.GetHelpString: string;
begin
  Result := 'Brings the code editor window to the front and gives it the input focus.';
end;

class function TFocusCodeEditorExpert.GetName: string;
begin
  Result := 'FocusCodeEditor';
end;

function TFocusCodeEditorExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TFocusCodeEditorExpert);
end.

