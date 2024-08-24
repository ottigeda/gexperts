unit GX_GrepPrevItemExpert;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GX_Experts;

type
  TGrepPrevItemExpert = class(TGX_Expert)
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

{ TGrepPrevItemExpert }

function TGrepPrevItemExpert.CanHaveShortCut: Boolean;
begin
  Result := True;
end;

class function TGrepPrevItemExpert.ConfigurationKey: string;
begin
  Result := TGrepExpert.ConfigurationKey + 'Prev';
end;

function TGrepPrevItemExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TGrepPrevItemExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

constructor TGrepPrevItemExpert.Create;
begin
  inherited;
  FActionInt := GxActionBroker.RequestAction(GetActionName, GetBitmap);
  FActionInt.OnExecute := Self.Execute;
  FActionInt.Caption := GetActionCaption;
end;

procedure TGrepPrevItemExpert.Execute(Sender: TObject);
begin
  if not Assigned(fmGrepResults) then
    Exit; //==>

  if fmGrepResults.actListSelectPrevious.Enabled then begin
    fmGrepResults.actListSelectPrevious.Execute;
    IncCallCount;
  end;
end;

function TGrepPrevItemExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Grep Result &Previous Item';
begin
  Result := SMenuCaption;
end;

function TGrepPrevItemExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(VK_F7, [ssAlt]);
end;

function TGrepPrevItemExpert.GetHelpString: string;
resourcestring
  SHelpString =
    '  Select previous item in grep search results.';
begin
  Result := SHelpString;
end;

class function TGrepPrevItemExpert.GetName: string;
begin
  Result := GrepPrevItemName;
end;

procedure TGrepPrevItemExpert.SetShortCut(Value: TShortCut);
begin
  inherited;
  if not Assigned(fmGrepResults) then
    Exit; //==>
  fmGrepResults.actListSelectPrevious.ShortCut := Value;
end;

initialization
  RegisterGX_Expert(TGrepPrevItemExpert);
end.
