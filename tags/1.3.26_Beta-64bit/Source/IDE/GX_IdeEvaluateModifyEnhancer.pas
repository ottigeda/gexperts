unit GX_IdeEvaluateModifyEnhancer;

{$I GX_CondDefine.inc}

interface

type
  TGxIdeEvaluateModifyEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  GX_IdeDialogEnhancer,
  GX_ConfigurationInfo,
  GX_GenericUtils;

type
  TEvaluateModifyEnhancer = class(TIdeDialogEnhancer)
  private
    FOrigOnDestroy: TNotifyEvent;
    procedure DoLoadFormState(_Form: TForm);
    function GetRegistryKey: string;
    procedure DoSaveFormState(_Form: TForm);
    procedure DoHookOnFormDestroy(_Form: TForm);
    procedure HandleFormDestroy(Sender: TObject);
    procedure FixFormErrors(_Form: TForm);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); overload; override;
  public
  end;

{ TEvaluateModifyEnhancer }

function TEvaluateModifyEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
const
  DIALOG_CLASS = 'TEvalDialog';
  DIALOG_NAME = 'EvalDialog';
begin
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
end;

function TryFindComponent(_frm: TForm; const _Name: string; out _Ctrl: TControl): Boolean;
begin
  _Ctrl := _frm.FindComponent(_Name) as TControl;
  Result := Assigned(_Ctrl);
end;

procedure TEvaluateModifyEnhancer.FixFormErrors(_Form: TForm);
{$IFDEF GX_EVAL_MODIFY_FIX}
var
  Panel1: TControl;
  ResultLine: TControl;
  Expression: TControl;
  NewValue: TControl;
  NewValueLabel: TControl;
begin
  if not TryFindComponent(_Form, 'Panel1', Panel1) then
    Exit; //==>
  if not TryFindComponent(_Form, 'Expression', Expression) then
    Exit; //==>
  if not TryFindComponent(_Form, 'ResultLine', ResultLine) then
    Exit; //==>
  if not TryFindComponent(_Form, 'NewValueLabel', NewValueLabel) then
    Exit; //==>
  if not TryFindComponent(_Form, 'NewValue', NewValue) then
    Exit; //==>

  Panel1.Width := _Form.Width - 2 * Panel1.Left;
  Panel1.Height := _Form.Height - 2 * Panel1.Left;
  Expression.Width := Panel1.Width - 2 * Expression.Left;
  NewValue.Width := Panel1.Width - 2 * NewValue.Left;
  NewValueLabel.Top := Panel1.Height - NewValue.Height - NewValueLabel.Height - NewValue.Left;
  NewValue.Top := Panel1.Height - NewValue.Left - NewValue.Height;
  ResultLine.Width := Panel1.Width - 2 * ResultLine.Left;
  ResultLine.Height := NewValueLabel.Top - ResultLine.Left - ResultLine.Top;
end;
{$ELSE}
begin
end;
{$ENDIF GX_EVAL_MODIFY_FIX}

procedure TEvaluateModifyEnhancer.EnhanceForm(_Form: TForm);
begin
  DoLoadFormState(_Form);
  DoHookOnFormDestroy(_Form);

  FixFormErrors(_Form);
end;

procedure TEvaluateModifyEnhancer.DoHookOnFormDestroy(_Form: TForm);
begin
  FOrigOnDestroy := _Form.OnDestroy;
  _Form.OnDestroy := HandleFormDestroy;
end;

function TEvaluateModifyEnhancer.GetRegistryKey: string;
begin
  Result := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + 'IDEForms';
end;

procedure TEvaluateModifyEnhancer.DoLoadFormState(_Form: TForm);
var
  Settings: TGExpertsSettings;
  SECTION: string;
  Flags: TFormSaveFlags;
begin
  Assert(Assigned(_Form));

  if _Form.Parent <> nil then begin
    // form is docked
    Exit; //==>
  end;

  Settings := TGExpertsSettings.Create(GetRegistryKey);
  try
    SECTION := _Form.ClassName;
    Flags := [];
    Include(Flags, fsSize);
    Include(Flags, fsPosition);
    if Flags <> [] then begin
      Settings.LoadForm(_Form, SECTION, Flags);
      EnsureFormVisible(_Form);
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TEvaluateModifyEnhancer.DoSaveFormState(_Form: TForm);
var
  Settings: TGExpertsSettings;
  SECTION: string;
begin
  Assert(Assigned(_Form));

  if _Form.Parent <> nil then begin
    // form is docked
    Exit; //==>
  end;

  Settings := TGExpertsSettings.Create(GetRegistryKey);
  try
    SECTION := _Form.ClassName;
    Settings.SaveForm(_Form, SECTION);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TEvaluateModifyEnhancer.HandleFormDestroy(Sender: TObject);
var
  OrigOnDestroy: TNotifyEvent;
  frm: TForm;
begin
  frm := Sender as TForm;

  // restore the original OnDestroy event
  // It's not being called, but there might be code in other plugins
  // (e.g. IdeFixpack or cnpack) that checks for it.
  frm.OnDestroy := FOrigOnDestroy;
  // save it for later (see below)
  OrigOnDestroy := FOrigOnDestroy;
  // just in case, set FOrigOnDestroy to nil
  FOrigOnDestroy := nil;

  DoSaveFormState(frm);

  if Assigned(OrigOnDestroy) then begin
    // Restoring the FForm.OnDestroy to its original value does not call OrigOnDestroy.
    // The reason is that HandleFormDestroy gets called from OnDestroy;
    // OnDestroy is called from TCustomForm.DoDestroy which will never call OnDestroy restored from OrigOnDestroy.
    // So we need to explicitly call the old one:
    OrigOnDestroy(Sender);
    // Note we still restore the old OnDestroy value, just in case there is a check
    // for the value (some interceptors actively perform that check).
  end;
end;

{ TGxIdeEvaluateModifyEnhancer }

var
  TheEvaluateModifyEnhancer: TEvaluateModifyEnhancer;

class function TGxIdeEvaluateModifyEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheEvaluateModifyEnhancer);
end;

class procedure TGxIdeEvaluateModifyEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheEvaluateModifyEnhancer) then
      TheEvaluateModifyEnhancer := TEvaluateModifyEnhancer.Create
  end else
    FreeAndNil(TheEvaluateModifyEnhancer);
end;

end.

