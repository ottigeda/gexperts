unit GX_IdeMessageAutoClose;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils;

type
  TGxMessageAutoClose = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean; _IgnoreHints, _IgnoreWarnings: Boolean); //static;
  end;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
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
    FIgnoreHints: Boolean;
    FIgnoreWarnings: Boolean;
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
    constructor Create(_IgnoreHints, _IgnoreWarnings: Boolean);
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
  Result := Assigned(TheMessageAutoClose);
{$ELSE}
  Result := False;
{$IFEND}
end;

class procedure TGxMessageAutoClose.SetEnabled(const Value: Boolean; _IgnoreHints, _IgnoreWarnings: Boolean);
begin
{$IF declared(TMessageAutoClose)}
  if Value then begin
    if Assigned(TheMessageAutoClose) then begin
      TheMessageAutoClose.FIgnoreHints := _IgnoreHints;
      TheMessageAutoClose.FIgnoreWarnings := _IgnoreWarnings;
    end else
      TheMessageAutoClose := TMessageAutoClose.Create(_IgnoreHints, _IgnoreWarnings);
  end else
    FreeAndNil(TheMessageAutoClose);
{$IFEND}
end;

{$IF declared(TMessageAutoClose)}

{ TMessageAutoClose }

constructor TMessageAutoClose.Create(_IgnoreHints, _IgnoreWarnings: Boolean);
begin
  inherited Create;
  FIgnoreHints := _IgnoreHints;
  FIgnoreWarnings := _IgnoreWarnings;
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
begin
  if TComponent_FindComponent(_Form, GxMessageAutoCloseComponent, True, cmp) then begin
    // our AutoClose component already exists, nothing to do
    Exit; //==>
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

  cmp := TMessageAutoCloseComponent.Create(_Form, HintCount, WarningCount, ErrorCount, FIgnoreHints, FIgnoreWarnings);
  cmp.Name := GxMessageAutoCloseComponent;
end;
{$IFEND}

end.
