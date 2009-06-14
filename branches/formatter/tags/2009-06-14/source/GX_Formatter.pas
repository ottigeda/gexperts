// the code formatter expert as a regular expert
// (Used if you don't define GX_FORMATTER_IS_EDITOR_EXPERT.)
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_Formatter;

{$I GX_CondDefine.inc}

interface

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  SysUtils,
  Classes,
  Menus,
  GX_Experts,
  GX_ConfigurationInfo,
  GX_CodeFormatterExpert;

type
  TGxCodeFormatterExpert = class(TGX_Expert)
  private
    FExpert: TCodeFormatterExpert;
  protected
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
  end;

{ TCodeFormatterExpert }

procedure TGxCodeFormatterExpert.Click(Sender: TObject);
begin
  FExpert.Execute;
end;

procedure TGxCodeFormatterExpert.Configure;
begin
  FExpert.Configure;
end;

constructor TGxCodeFormatterExpert.Create;
begin
  inherited Create;

  FExpert := TCodeFormatterExpert.Create;

  ShortCut := Menus.ShortCut(Word('F'), [ssCtrl, ssAlt]);
end;

destructor TGxCodeFormatterExpert.Destroy;
begin
  FreeAndNil(FExpert);
  inherited;
end;

function TGxCodeFormatterExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Code &Formatter';
begin
  Result := SMenuCaption;
end;

class function TGxCodeFormatterExpert.GetName: string;
begin
  Result := 'CodeFormatter';
end;

function TGxCodeFormatterExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TGxCodeFormatterExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

procedure TGxCodeFormatterExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FExpert.InternalLoadSettings(ConfigurationKey, Settings);
end;

procedure TGxCodeFormatterExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FExpert.InternalSaveSettings(ConfigurationKey, Settings);
end;

initialization
  RegisterGX_Expert(TGxCodeFormatterExpert);
end.

