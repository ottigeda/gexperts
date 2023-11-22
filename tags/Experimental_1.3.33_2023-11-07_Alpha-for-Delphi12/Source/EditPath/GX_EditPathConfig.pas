unit GX_EditPathConfig;

{$I GX_CondDefine.inc}

interface

{.$DEFINE GX_DELPHI2009_UP}

{$IFDEF GX_DELPHI2009_UP}
// Only Delphi 2009 and up provide the necessary ToolsAPI funtionality

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Contnrs,
  GX_BaseForm;

type
  TConfigEntry = class
  private
    FConfigName: string;
    FPlatform: string;
    FPath: string;
    function GetConfigCaption: string;
    function GetPlatformCaption: string;
  public
    constructor Create(const _ConfigName, _Platform, _Path: string);
    function TextCaption: string;
    function ListCaption: string;
    property Path: string read FPath write FPath;
    property ConfigName: string read FConfigName write FConfigName;
    property Platform: string read FPlatform write FPlatform;
  end;

type
  Tf_EditPathConfig = class(TfmBaseForm)
    l_SelectConfig: TLabel;
    b_Ok: TButton;
    b_Cancel: TButton;
    lb_Target: TListBox;
  private
    procedure SetData(_ConfigList: TObjectList; const _Platform, _Config: string);
    procedure GetData(out _Platform, _Config: string);
  protected
  public
    class function Execute(_Owner: TWinControl; _ConfigList: TObjectList; var _Platform, _Config: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

{$ENDIF GX_DELPHI2009_UP}

implementation

{$IFDEF GX_DELPHI2009_UP}

{$R *.dfm}

uses
  u_dzVclUtils,
  GX_OtaUtils;

const
  CURRENT_CONFIG = '<Current Config>';

{ TfmBaseForm1 }

constructor Tf_EditPathConfig.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  TControl_SetMinConstraints(Self);
end;

class function Tf_EditPathConfig.Execute(_Owner: TWinControl; _ConfigList: TObjectList;
  var _Platform, _Config: string): Boolean;
var
  frm: Tf_EditPathConfig;
begin
  frm := Tf_EditPathConfig.Create(_Owner);
  try
    frm.SetData(_ConfigList, _Platform, _Config);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Platform, _Config);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_EditPathConfig.GetData(out _Platform, _Config: string);
var
  Idx: Integer;
  cfg: TConfigEntry;
begin
  _Platform := '';
  _Config := '';
  Idx := lb_Target.ItemIndex;
  if Idx >= 0 then begin
    cfg := lb_Target.Items.Objects[Idx] as TConfigEntry;
    if Assigned(cfg) then begin
      _Platform := cfg.Platform;
      _Config := cfg.ConfigName;
    end;
  end;
end;

procedure Tf_EditPathConfig.SetData(_ConfigList: TObjectList; const _Platform, _Config: string);
var
  i: Integer;
  cfg: TConfigEntry;
  ThisIndex: Integer;
begin
  lb_Target.Items.Clear;
  ThisIndex := lb_Target.Items.Add(CURRENT_CONFIG);
  lb_Target.ItemIndex := ThisIndex;
  for i := 0 to _ConfigList.Count - 1 do begin
    cfg := _ConfigList[i] as TConfigEntry;
    ThisIndex := lb_Target.Items.AddObject(cfg.ListCaption, cfg);
    if (cfg.Platform = _Platform) and (cfg.ConfigName = _Config) then
      lb_Target.ItemIndex := ThisIndex;
  end;
end;

{ TConfigEntry }

constructor TConfigEntry.Create(const _ConfigName, _Platform, _Path: string);
begin
  inherited Create;
  FConfigName := _ConfigName;
  FPlatform := _Platform;
  FPath := _Path;
end;

function TConfigEntry.GetConfigCaption: string;
begin
  if FConfigName = 'Base' then
    Result := sAllConfigurations
  else
    Result := FConfigName + sConfiguration;
end;

function TConfigEntry.GetPlatformCaption: string;
begin
  Result := GxOtaGetPlatformCaption(FPlatform);
end;

function TConfigEntry.TextCaption: string;
begin
{$IFDEF GX_DELPHIXE2_UP}
  Result := GetConfigCaption + ' - ' + GetPlatformCaption;
{$ELSE}
  Result := GetConfigCaption;
{$ENDIF}
end;

function TConfigEntry.ListCaption: string;
begin
  if FPlatform <> '' then
    Result := '    ' + GetPlatformCaption
  else
    Result := GetConfigCaption;
end;
{$ENDIF GX_DELPHI2009_UP}

end.

