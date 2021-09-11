unit GX_IdeApplicationSettingsEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes;

type
  TGxIdeApplicationSettingsEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  Controls,
  Menus,
  StdCtrls,
  Forms,
  Dialogs,
  Types,
  ComCtrls,
  ExtCtrls,
  u_dzClassUtils,
  GX_VerDepConst,
  GX_IdeDialogEnhancer;

type
  TIdeAppSettingsEnhancer = class(TIdeDialogEnhancer)
  private
{$IFDEF GX_DELPHI_SYDNEY_UP}
    // from Delphi 10.4 onwards this is a TComboBox
    FSuffixCmb: TComboBox;
{$ELSE}
    // in older versions is was a TEdit
    FSuffixEdit: TEdit;
{$ENDIF}
    procedure btnSetSuffixClick(_Sender: TObject);
  protected
    procedure EnhanceForm(_Form: TForm); override;
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
  public
  end;

var
  TheAppSettingsEnhancer: TIdeAppSettingsEnhancer = nil;

{ TIdeAppSettingsEnhancer }

procedure TIdeAppSettingsEnhancer.EnhanceForm(_Form: TForm);
const
  SuffixBtnName = 'GXSOSuffixBtn';
var
  Cmp: TComponent;
  btn: TButton;
  s: string;
  WinCtrl: TWinControl;
begin
  if TComponent_FindComponent(_Form, SuffixBtnName, True, Cmp) then
    Exit; //==>
  if not TComponent_FindComponent(_Form, 'ecSOSuffix', True, Cmp) then
    Exit; //==>

{$IFDEF GX_DELPHI_SYDNEY_UP}
  if not (Cmp is TComboBox) then
    Exit; //==>
  FSuffixCmb := TComboBox(Cmp);
{$ELSE}
  if not (Cmp is TEdit) then
    Exit; //==>
  FSuffixEdit := TEdit(Cmp);
{$ENDIF}
  WinCtrl := TWinControl(Cmp);

  btn := TButton.Create(_Form);
  btn.Parent := WinCtrl.Parent;
  btn.Name := SuffixBtnName;
  btn.Top := WinCtrl.Top;
  btn.Height := WinCtrl.Height;
  s := MajorVersionNumberChar + '0';
  btn.Width := _Form.Canvas.TextWidth(s) + 8;
  WinCtrl.Width := WinCtrl.Width - btn.Width;
  btn.Left := WinCtrl.Left + WinCtrl.Width;
  btn.Anchors := [akTop, akRight];
  btn.Caption := s;
  btn.OnClick := btnSetSuffixClick;
  btn.TabOrder := WinCtrl.TabOrder + 1;
end;

procedure TIdeAppSettingsEnhancer.btnSetSuffixClick(_Sender: TObject);
begin
{$IFDEF GX_DELPHI_SYDNEY_UP}
  FSuffixCmb.Text := (_Sender as TButton).Caption;
{$ELSE}
  FSuffixEdit.Text := (_Sender as TButton).Caption;
{$ENDIF}
end;

function TIdeAppSettingsEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
const
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
  DIALOG_CLASS = 'TProjectOptionsDialog';
  DIALOG_NAME = 'ProjectOptionsDialog';
{$ELSE}
  DIALOG_CLASS = 'TDelphiProjectOptionsDialog';
  DIALOG_NAME = 'DelphiProjectOptionsDialog';
{$ENDIF}
begin
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
end;

{ TGxIdeApplicationSettingsEnhancer }

class function TGxIdeApplicationSettingsEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheAppSettingsEnhancer);
end;

class procedure TGxIdeApplicationSettingsEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheAppSettingsEnhancer) then
      TheAppSettingsEnhancer := TIdeAppSettingsEnhancer.Create
  end else
    FreeAndNil(TheAppSettingsEnhancer);
end;

initialization
finalization
  TGxIdeApplicationSettingsEnhancer.SetEnabled(False);
end.

