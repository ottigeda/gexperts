unit GX_GrepNextItemExpert;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GX_Experts;

type
  TGrepNextItemExpert = class(TGX_Expert)
  protected
    procedure SetShortCut(Value: TShortCut); override;
  public
    constructor Create; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetActionCaption: string; override;
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    function GetHelpString: string; override;
    function HasMenuItem: Boolean; override;
    function CanHaveShortCut: Boolean; override;
  end;

implementation

uses
  Menus,
  GX_ActionBroker,
  GX_GrepExpert,
  GX_GrepResults,
  GX_GrepMenuEntry;

{ TGrepNextItemExpert }

function TGrepNextItemExpert.CanHaveShortCut: Boolean;
begin
  Result := True;
end;

class function TGrepNextItemExpert.ConfigurationKey: string;
begin
  Result := TGrepExpert.ConfigurationKey + 'Next';
end;

function TGrepNextItemExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TGrepNextItemExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

constructor TGrepNextItemExpert.Create;
begin
  inherited;
  FActionInt := GxActionBroker.RequestAction(GetActionName, GetBitmap);
  FActionInt.OnExecute := Self.Execute;
  FActionInt.Caption := GetActionCaption;
end;

procedure TGrepNextItemExpert.Execute(Sender: TObject);
begin
  if not Assigned(fmGrepResults) then
    Exit; //==>

  if fmGrepResults.actListSelectNext.Enabled then begin
    fmGrepResults.actListSelectNext.Execute;
    IncCallCount;
  end;
end;

function TGrepNextItemExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Grep Result &Next Item';
begin
  Result := SMenuCaption;
end;

function TGrepNextItemExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(VK_F8, [ssAlt]);
end;

function TGrepNextItemExpert.GetHelpString: string;
resourcestring
  SHelpString =
    '  Select next item in grep search results.';
begin
  Result := SHelpString;
end;

class function TGrepNextItemExpert.GetName: string;
begin
  Result := GrepNextItemName;
end;

procedure TGrepNextItemExpert.SetShortCut(Value: TShortCut);
begin
  inherited;
  if not Assigned(fmGrepResults) then
    Exit; //==>
  fmGrepResults.actListSelectNext.ShortCut := Value;
end;

initialization
  RegisterGX_Expert(TGrepNextItemExpert);
end.
