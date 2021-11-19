unit GX_EditPath;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GX_BaseForm,
  SynMemo,
  StdCtrls,
  ExtCtrls;

type
  Tf_EditPath = class(TfmBaseForm)
    p_Memo: TPanel;
    b_Ok: TButton;
    b_Cancel: TButton;
  private
    FMemo: TSynMemo;
    procedure SetData(const _Path: string);
    procedure GetData(out _Path: string);
  public
    class function Execute(_Owner: TComponent; var _Path: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzTypes,
  u_dzVclUtils,
  u_dzStringUtils,
  u_dzStringArrayUtils,
  SynUnicode,
  GX_OtaUtils,
  GX_Experts;

{ Tf_EditPath }

class function Tf_EditPath.Execute(_Owner: TComponent; var _Path: string): Boolean;
var
  frm: Tf_EditPath;
begin
  frm := Tf_EditPath.Create(_Owner);
  try
    frm.SetData(_Path);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Path);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_EditPath.GetData(out _Path: string);
var
  i: Integer;
  Lines: TUnicodeStrings;
  s: string;
begin
  Lines := FMemo.Lines;
  _Path := '';
  for i := 0 to Lines.Count - 1 do begin
    s := Trim(Lines[i]);
    if s <> '' then
      _Path := _Path + ';' + s;
  end;
  _Path := Copy(_Path, 2);
end;

procedure Tf_EditPath.SetData(const _Path: string);
var
  Dirs: TStringArray;
  i: Integer;
  Lines: TUnicodeStrings;
begin
  Dirs := SplitString(_Path, [';']);
  Lines := FMemo.Lines;
  Lines.Clear;
  for i := Low(Dirs) to High(Dirs) do
    if Trim(Dirs[i]) <> '' then
      Lines.Add(Dirs[i]);
end;

constructor Tf_EditPath.Create(_Owner: TComponent);
begin
  inherited;
  TPanel_BevelNone([p_Memo]);
  FMemo := TSynMemo.Create(Self);
  FMemo.Parent := p_Memo;
  FMemo.Align := alClient;
end;

type
  TEditPathExpert = class(TGX_Expert)
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    // optional, but recommended
    function GetHelpString: string; override;
    // optional, defaults to true
    function HasConfigOptions: Boolean; override;
//    // optional if HasConfigOptions returns false
//    procedure Configure; override;
//    // Override to load any configuration settings
//    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
//    // Override to save any configuration settings
//    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure Execute(Sender: TObject); override;
  end;

{ TEditPathExpert }

constructor TEditPathExpert.Create;
begin
  inherited;

end;

destructor TEditPathExpert.Destroy;
begin

  inherited;
end;

procedure TEditPathExpert.Execute(Sender: TObject);
var
  Path: string;
begin
  if not GxOtaTryGetProjectSearchPath(Path) then
    Path := '';
  if Tf_EditPath.Execute(Application, Path) then
    GxOtaTrySetProjectSearchPath(Path);
end;

function TEditPathExpert.GetActionCaption: string;
begin
  Result := 'Edit &Path ...';
end;

function TEditPathExpert.GetHelpString: string;
begin
  Result := 'Needs a help string';
end;

function TEditPathExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TEditPathExpert);
end.

