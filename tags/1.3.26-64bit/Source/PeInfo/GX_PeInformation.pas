unit GX_PeInformation;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, SysUtils;

implementation

uses
  StrUtils, GX_GenericUtils, GX_Experts,
  GX_ConfigurationInfo;

type
  TPEInformationExpert = class(TGX_Expert)
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

var
  PeExpert: TPEInformationExpert;

{ TPEInformationExpert }

constructor TPEInformationExpert.Create;
begin
  inherited Create;
  PeExpert := Self;
end;

destructor TPEInformationExpert.Destroy;
begin
  PeExpert := nil;
  inherited;
end;

function TPEInformationExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'P&E Information';
begin
  Result := SMenuCaption;
end;

class function TPEInformationExpert.GetName: string;
begin
  Result := 'PEInformation';
end;

procedure TPEInformationExpert.Execute(Sender: TObject);
begin
  GXShellExecute(AddSlash(ConfigInfo.GExpertsPath) + 'GExpertsPeInformation.exe', '', True);

  IncCallCount;
end;

function TPEInformationExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TPEInformationExpert);
end.

