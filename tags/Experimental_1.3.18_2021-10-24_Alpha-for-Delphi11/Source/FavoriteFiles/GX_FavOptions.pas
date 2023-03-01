unit GX_FavOptions;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Grids,
  GX_BaseForm;

type
  TfmFavOptions = class(TfmBaseForm)
    gbxFavOptions: TGroupBox;
    chkConfirmFolderDelete: TCheckBox;
    chkExpandAllOnLoad: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkHideOnExecute: TCheckBox;
    chkShowPreview: TCheckBox;
    chk_InsertFavMenu: TCheckBox;
    sg_FileFilters: TStringGrid;
    l_FileFilters: TLabel;
    b_Default: TButton;
    procedure b_DefaultClick(Sender: TObject);
    procedure sg_FileFiltersEnter(Sender: TObject);
    procedure sg_FileFiltersExit(Sender: TObject);
    procedure sg_FileFiltersKeyPress(Sender: TObject; var Key: Char);
  private
    procedure SetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu: Boolean;
      const _FileFilter: string);
    procedure GetData(out _FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu: Boolean;
      out _FileFilter: string);
    procedure SetFileFiter(const _FileFilter: string);
  public
    class function Execute(_Owner: TWinControl; var _FolderDelete, _ExpandAll: Boolean;
      var _ExecHide, _ShowPreview: Boolean;
      var _InsertFavMenu: Boolean;
      var _FileFilter: string): Boolean;
    class function GetDefaultFileFilter: string;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  u_dzStringArrayUtils,
  u_dzTypes,
  u_dzStringUtils,
  GX_GenericUtils;

{ TfmFavOptions }

class function TfmFavOptions.Execute(_Owner: TWinControl; var _FolderDelete, _ExpandAll: Boolean;
  var _ExecHide, _ShowPreview: Boolean;
  var _InsertFavMenu: Boolean;
  var _FileFilter: string): Boolean;
var
  frm: TfmFavOptions;
begin
  frm := TfmFavOptions.Create(nil);
  try
    frm.SetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu, _FileFilter);
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      frm.GetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu, _FileFilter);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

class function TfmFavOptions.GetDefaultFileFilter: string;

  procedure AppendMask(const _Description, _Mask: string);
  begin
    Result := Result + '|' + _Description + '|' + _Mask;
  end;

begin
  Result := '';
  AppendMask('Source Files', '*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.bdsproj;*.pas;*.cpp;*.hpp;*.c;*.h');
  AppendMask('Project Files', '*.dpr;*.bpr;*.dpk;*.bpk;*.bpg;*.bdsproj;*.bdsgroup;*.dproj;*.groupproj');
  AppendMask('Pascal Files', '*.pas;*.inc');
  AppendMask('Help Files', '*.chm;*.hlp');
  AppendMask('Graphics Files', '*.bmp;*.wmf;*.jpg;*.png;*.gif;*.ico');
  AppendMask('Text Files', '*.txt;*.me;*.asc;*.xml;*.iss');
  AppendMask('HTML Files', '*.html;*.htm');
  AppendMask('Executable Files', '*.exe');
  AppendMask('SQL Scripts', '*.sql');
  AppendMask('C/C++', '*.c;*.cpp;*.h;*.hpp');
  Result := Copy(Result, 2);
end;

constructor TfmFavOptions.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  TStringGrid_AssignRow(sg_FileFilters, 0, ['Description', 'Mask']);
  
{$IFNDEF GX_VER150_up} // Delphi 7
  chk_InsertFavMenu.Enabled := False;
  chk_InsertFavMenu.Caption := chk_InsertFavMenu.Caption + ' (not available in Delphi 6)';
{$ENDIF}

  InitDpiScaler;
end;

procedure TfmFavOptions.b_DefaultClick(Sender: TObject);
begin
  SetFileFiter(GetDefaultFileFilter);
end;

procedure TfmFavOptions.GetData(out _FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu: Boolean;
  out _FileFilter: string);
var
  Descriptions: TStrings;
  Masks: TStrings;
  i: Integer;
begin
  _FolderDelete := chkConfirmFolderDelete.Checked;
  _ExpandAll := chkExpandAllOnLoad.Checked;
  _ExecHide := chkHideOnExecute.Checked;
  _ShowPreview := chkShowPreview.Checked;
  _InsertFavMenu := chk_InsertFavMenu.Checked;

  _FileFilter := '';
  Descriptions := sg_FileFilters.Cols[0];
  Masks := sg_FileFilters.Cols[1];
  for i := 1 to Descriptions.Count - 1 do begin
    _FileFilter := _FileFilter + '|' + Descriptions[i] + ' (' + Masks[i] + ')|' + Masks[i];
  end;
  _FileFilter := Copy(_FileFilter, 2);
end;

procedure TfmFavOptions.SetFileFiter(const _FileFilter: string);
var
  Filter: string;
  Desc: string;
  Mask: string;
  p: Integer;
begin
  Filter := _FileFilter;
  TStringGrid_Clear(sg_FileFilters);
  while ExtractStr(Filter, '|', Desc) do begin
    p := Pos('(', Desc);
    if p > 0 then
      Desc := Trim(Copy(Desc, 1, p - 1));
    ExtractStr(Filter, '|', Mask);
    if Mask <> AllFilesWildCard then
      TStringGrid_AppendRow(sg_FileFilters, [Desc, Mask], True);
  end;
  TGrid_Resize(sg_FileFilters, [roUseGridWidth, roUseAllRows]);
end;

procedure TfmFavOptions.SetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu: Boolean;
  const _FileFilter: string);
begin
  chkConfirmFolderDelete.Checked := _FolderDelete;
  chkExpandAllOnLoad.Checked := _ExpandAll;
  chkHideOnExecute.Checked := _ExecHide;
  chkShowPreview.Checked := _ShowPreview;
  chk_InsertFavMenu.Checked := _InsertFavMenu;

  SetFileFiter(_FileFilter);
end;

procedure TfmFavOptions.sg_FileFiltersEnter(Sender: TObject);
begin
  TStringGrid_AppendRow(sg_FileFilters, True);
end;

procedure TfmFavOptions.sg_FileFiltersExit(Sender: TObject);
var
  r: Integer;
begin
  for r := TGrid_GetNonfixedRowCount(sg_FileFilters) downto sg_FileFilters.FixedRows + 1 do begin
    if TStringGrid_IsRowEmpty(sg_FileFilters, r) then
      TStringGrid_DeleteRow(sg_FileFilters, r);
  end;
end;

procedure TfmFavOptions.sg_FileFiltersKeyPress(Sender: TObject; var Key: Char);
begin
  TStringGrid_AppendRow(sg_FileFilters, True);
end;

end.
