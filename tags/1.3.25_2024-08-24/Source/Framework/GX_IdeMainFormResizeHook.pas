unit GX_IdeMainFormResizeHook;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  GX_EventHook;

type
  ///<summary>
  /// This class hooks the OnResize event of the Ide's main form.
  /// I'm not sure whether I will need this this eventually, but since I just tried it and it
  /// worked, I don't want to throw it away.
  /// -- 2019-12-22 twm </summary>
  TIdeMainFormResizeHook = class(TSecureNotifyEventHook)
  private
    class function GetIdeMainForm: TForm;
  protected
    class function GetEvent: TMethod; override;
    class procedure SetEvent(_Value: TMethod); override;
  end;

implementation

uses
  GX_IdeUtils;

{ TIdeMainFormResizeHook }

class function TIdeMainFormResizeHook.GetIdeMainForm: TForm;
begin
  Result := TForm(GX_IdeUtils.GetIdeMainForm);
end;

class function TIdeMainFormResizeHook.GetEvent: TMethod;
var
  IdeMainForm: TForm;
begin
  IdeMainForm := GetIdeMainForm;
  if Assigned(IdeMainForm) then
    Result := TMethod(IdeMainForm.OnResize)
  else
    ZeroMemory(@Result, SizeOf(Result));
end;

class procedure TIdeMainFormResizeHook.SetEvent(_Value: TMethod);
var
  IdeMainForm: TForm;
begin
  IdeMainForm := GetIdeMainForm;
  if Assigned(IdeMainForm) then
    IdeMainForm.OnResize := TNotifyEvent(_Value);
end;

end.
