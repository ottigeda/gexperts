unit GX_IdeMessageAutoClose;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils;

type
  TGxMessageAutoClose = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const _Value, _IgnoreHints, _IgnoreWarnings, _TransparencyDisabled: Boolean);
  end;

implementation

uses
{$IFOPT D+}GX_DbugIntf,{$ENDIF}
  Windows,
  Messages,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes,
  Graphics,
  u_dzClassUtils,
  GX_IdeDialogEnhancer,
  GX_GExperts;

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
// this expert does not work for Delphi 6 and 7, it causes access violations
type
  TMessageAutoClose = class(TIdeDialogEnhancer)
  private
    FAutoCloseEnabled: Boolean;
    FIgnoreHints: Boolean;
    FIgnoreWarnings: Boolean;
    FTransparencyDisabled: Boolean;
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
    constructor Create(_Enabled, _IgnoreHints, _IgnoreWarnings: Boolean; _TransparencyDisabled: Boolean);
  end;
{$ENDIF GX_VER170_up} // Delphi 9/2005 (BDS 2)

{$IF declared(TMessageAutoClose)}
var
  TheMessageAutoClose: TMessageAutoClose = nil;

type
  TMessageAutoCloseComponent = class(TComponent)
  private
    FHintCount: TLabel;
    FWarningCount: TLabel;
    FErrorCount: TLabel;
    FIgnoreHints: Boolean;
    FIgnoreWarnings: Boolean;
  public
    constructor Create(_Owner: TComponent; _HintCount, _WarningCount, _ErrorCount: TLabel;
      _IgnoreHints, _IgnoreWarnings: Boolean); reintroduce;
    procedure BeforeDestruction; override;
  end;

{ TMessageAutoCloseComponent }

constructor TMessageAutoCloseComponent.Create(_Owner: TComponent;
  _HintCount, _WarningCount, _ErrorCount: TLabel;
  _IgnoreHints, _IgnoreWarnings: Boolean);
begin
  inherited Create(_Owner);
  FHintCount := _HintCount;
  FWarningCount := _WarningCount;
  FErrorCount := _ErrorCount;
  FIgnoreHints := _IgnoreHints;
  FIgnoreWarnings := _IgnoreWarnings;
end;

procedure TMessageAutoCloseComponent.BeforeDestruction;
begin
  inherited;
  try
    if FErrorCount.Caption <> '0' then
      Exit; //==>
    if not FIgnoreWarnings and (FWarningCount.Caption <> '0') then
      Exit; //==>
    if not FIgnoreHints and (FHintCount.Caption <> '0') then
      Exit; //==>
    GExpertsInst.TimedCloseMessageView;
  except
    on e: Exception do begin
{$IF declared(SendDebugError)}
      SendDebugError(e.Message + ' in TMessageAutoCloseComponent.BeforeDestruction');
{$IFEND}
    end;
  end;
end;
{$IFEND}

{ TGxMessageAutoClose }

class function TGxMessageAutoClose.GetEnabled: Boolean;
begin
{$IF declared(TMessageAutoClose)}
  Result := Assigned(TheMessageAutoClose) and TheMessageAutoClose.FAutoCloseEnabled;
{$ELSE}
  Result := False;
{$IFEND}
end;

class procedure TGxMessageAutoClose.SetEnabled(const _Value, _IgnoreHints, _IgnoreWarnings, _TransparencyDisabled: Boolean);
begin
{$IF declared(TMessageAutoClose)}
  if Assigned(TheMessageAutoClose) then begin
    TheMessageAutoClose.FAutoCloseEnabled := _Value;
    TheMessageAutoClose.FIgnoreHints := _IgnoreHints;
    TheMessageAutoClose.FIgnoreWarnings := _IgnoreWarnings;
    TheMessageAutoClose.FTransparencyDisabled := _TransparencyDisabled;
  end else
    TheMessageAutoClose := TMessageAutoClose.Create(_Value, _IgnoreHints, _IgnoreWarnings, _TransparencyDisabled);
{$IFEND}
end;

{$IF declared(TMessageAutoClose)}

{ TMessageAutoClose }

constructor TMessageAutoClose.Create(_Enabled, _IgnoreHints, _IgnoreWarnings: Boolean; _TransparencyDisabled: Boolean);
begin
  inherited Create;
  FAutoCloseEnabled := _Enabled;
  FIgnoreHints := _IgnoreHints;
  FIgnoreWarnings := _IgnoreWarnings;
  FTransparencyDisabled := _TransparencyDisabled;
end;

function TMessageAutoClose.IsDesiredForm(_Form: TCustomForm): Boolean;
const
  DIALOG_CLASS = 'TProgressForm';
  DIALOG_NAME = 'ProgressForm';
begin
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
end;

procedure TMessageAutoClose.EnhanceForm(_Form: TForm);
const
  GxMessageAutoCloseComponent = 'GxMessageAutoCloseComponent';
var
  cmp: TComponent;
  HintCount: TLabel;
  WarningCount: TLabel;
  ErrorCount: TLabel;
(*
  AutoCloseChk: TCheckBox;
*)
begin
  if TComponent_FindComponent(_Form, GxMessageAutoCloseComponent, True, cmp) then begin
    // our AutoClose component already exists, nothing to do
    Exit; //==>
  end;

(*
  // disable the auto-close checkbox
  if TComponent_FindComponent(_Form, 'AutoCloseChk', True, TComponent(AutoCloseChk), TCheckBox) then
  begin
    AutoCloseChk.Checked := False;
    AutoCloseChk.Enabled := False;
  end;
*)

  if FTransparencyDisabled then begin
{$IFDEF GX_FORM_HAS_ALPHABLEND}
    _Form.AlphaBlend := False;
{$ENDIF}
  end;

  if not TComponent_FindComponent(_Form, 'HintCount', False, TComponent(HintCount), TLabel) then begin
    // no HintCount component -> There is some error
    Exit;
  end;
  if not TComponent_FindComponent(_Form, 'WarningCount', False, TComponent(WarningCount), TLabel) then begin
    // no WarningCount component -> There is some error
    Exit;
  end;
  if not TComponent_FindComponent(_Form, 'ErrorCount', False, TComponent(ErrorCount), TLabel) then begin
    // no ErrorCount component -> There is some error
    Exit;
  end;

  if FAutoCloseEnabled then begin
    cmp := TMessageAutoCloseComponent.Create(_Form, HintCount, WarningCount, ErrorCount, FIgnoreHints, FIgnoreWarnings);
    cmp.Name := GxMessageAutoCloseComponent;
  end;
end;
{$IFEND}

end.
