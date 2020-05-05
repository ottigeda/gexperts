unit GX_HideNonVisualComps;

interface

uses
  ActnList,
  GX_Experts;

implementation

uses
  GX_OtaUtils, Forms, SysUtils, Windows, GX_GenericUtils, Controls, Classes;

type
  THideNonVisualCompsExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure ToggleNonVisualVisible(Form: TCustomForm);
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    class function ConfigurationKey: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
  end;

{ THideNonVisualCompsExpert }

procedure THideNonVisualCompsExpert.Execute(Sender: TObject);
var
  DesignForm: TCustomForm;
begin
  inherited;
  DesignForm := GxOtaGetCurrentDesignForm;
  if not Assigned(DesignForm) then
    raise Exception.Create('No VCL form designer selected');
  if (DesignForm.ClassName = 'TDataModuleForm') then
    raise Exception.Create('Please select a VCL form');
  ToggleNonVisualVisible(DesignForm);
end;

class function THideNonVisualCompsExpert.ConfigurationKey: string;
begin
  Result := 'HideNonVisualComps';
end;

function THideNonVisualCompsExpert.GetActionCaption: string;
begin
  Result := 'Hide/Show Non-Visual';
end;

class function THideNonVisualCompsExpert.GetName: string;
begin
  Result := 'HideComponents';
end;

function THideNonVisualCompsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function THideNonVisualCompsExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

procedure THideNonVisualCompsExpert.ToggleNonVisualVisible(Form: TCustomForm);
const
  NonVisualClassName = 'TContainer';
var
  VisibleState: Boolean;
  FormHandle: THandle;
  CompHandle: THandle;
  WindowClass: string;
  FirstCompFound: Boolean;
  WinControl: TWinControl;
  ChildControl: TWinControl;
  i: Integer;
begin
  Assert(Assigned(Form));
  Assert(Form.Handle > 0);
  FirstCompFound := False;
  WinControl := Form;
  if InheritsFromClass(WinControl.ClassType, 'TWinControlForm') then
  begin
    for i := WinControl.ComponentCount - 1 downto 0 do
    begin
      if WinControl.Controls[i] is TWinControl then
      begin
        ChildControl := WinControl.Controls[i] as TWinControl;
        if InheritsFromClass(ChildControl.ClassType, 'TCustomFrame') then
        begin
          WinControl := ChildControl;
          Break;
        end;
      end;
    end;
  end;

  FormHandle := GetWindow(WinControl.Handle, GW_CHILD);
  CompHandle := GetWindow(FormHandle, GW_HWNDLAST);
  VisibleState := False;
  GxOtaClearSelectionOnCurrentForm;

  while (CompHandle <> 0) do
  begin
    WindowClass := GetWindowClassName(CompHandle);
    if AnsiSameText(WindowClass, NonVisualClassName) then
    begin
      if not FirstCompFound then
      begin
        VisibleState := not IsWindowVisible(CompHandle);
        FirstCompFound := True;
      end;
      if VisibleState then
        ShowWindow(CompHandle, SW_SHOW)
      else
        ShowWindow(CompHandle, SW_HIDE);
    end;
    CompHandle := GetWindow(CompHandle, GW_HWNDPREV);
  end;
end;

procedure THideNonVisualCompsExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaCurrentlyEditingForm;
end;

initialization
  RegisterGX_Expert(THideNonVisualCompsExpert);

end.
