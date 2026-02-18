unit GX_UsesExpertOptions;

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
  StdCtrls,
  ExtCtrls,
  GX_BaseForm,
  GX_UsesSortGroups;

type
  TFilterIdentifiersEnum = (fieStartOnly, fieAnywhere, fieStartFirst);

type
  TfmUsesExpertOptions = class(TfmBaseForm)
    chkReplaceFileUnit: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkReadMap: TCheckBox;
    chkParseAll: TCheckBox;
    chkDisableParserCache: TCheckBox;
    btnClearCache: TButton;
    rg_FilterIdentifiers: TRadioGroup;
    chkSearchPathFavorites: TCheckBox;
    chk_FastAdd: TCheckBox;
    chk_AutoSort: TCheckBox;
    b_Help: TButton;
    grp_SortSettings: TGroupBox;
    lb_Prefixes: TListBox;
    btnPrefixAdd: TButton;
    btnPrefixEdit: TButton;
    btnPrefixDelete: TButton;
    btnPrefixReset: TButton;
    btnPrefixUp: TButton;
    btnPrefixDown: TButton;
    rg_Separator: TRadioGroup;
    procedure btnClearCacheClick(Sender: TObject);
    procedure b_HelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnPrefixAddClick(Sender: TObject);
    procedure btnPrefixEditClick(Sender: TObject);
    procedure btnPrefixDeleteClick(Sender: TObject);
    procedure btnPrefixResetClick(Sender: TObject);
    procedure btnPrefixUpClick(Sender: TObject);
    procedure btnPrefixDownClick(Sender: TObject);
    procedure lb_PrefixesClick(Sender: TObject);
  private
    FCacheDir: string;
    procedure SetData(const _CanReplaceFindUseUnit: Boolean; const _CacheDir: string;
      const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
      _FilterIdentifiers: TFilterIdentifiersEnum; _FastAdd, _AutoSort: Boolean;
      _PrefixGroups: TUsesPrefixGroupList; _GroupSeparator: TGroupSeparator);
    procedure GetData(out _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
      out _FilterIdentifiers: TFilterIdentifiersEnum; out _FastAdd, _AutoSort: Boolean;
      _PrefixGroups: TUsesPrefixGroupList; out _GroupSeparator: TGroupSeparator);
    procedure UpdatePrefixButtons;
  public
    class function Execute(_Owner: TWinControl; _CanReplaceFindUseUnit: Boolean;
      const _CacheDir: string;
      var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
      var _FilterIdentifiers: TFilterIdentifiersEnum;
      var _FastAdd: Boolean;
      var _AutoSort: Boolean;
      _PrefixGroups: TUsesPrefixGroupList;
      var _GroupSeparator: TGroupSeparator): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzFileUtils,
  u_dzVclUtils,
  GX_MessageBox,
  GX_GxUtils;

{ TfmUsesExpertOptions }

class function TfmUsesExpertOptions.Execute(_Owner: TWinControl; _CanReplaceFindUseUnit: Boolean;
  const _CacheDir: string;
  var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
  var _FilterIdentifiers: TFilterIdentifiersEnum;
  var _FastAdd: Boolean;
  var _AutoSort: Boolean;
  _PrefixGroups: TUsesPrefixGroupList;
  var _GroupSeparator: TGroupSeparator): Boolean;
var
  frm: TfmUsesExpertOptions;
begin
  frm := TfmUsesExpertOptions.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_CanReplaceFindUseUnit, _CacheDir, _ReadMapFile, _ReplaceFileUseUnit, _ParseAll,
      _DisableCache, _SearchPathFavorites, _FilterIdentifiers, _FastAdd, _AutoSort,
      _PrefixGroups, _GroupSeparator);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites,
        _FilterIdentifiers, _FastAdd, _AutoSort, _PrefixGroups, _GroupSeparator);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmUsesExpertOptions.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

procedure TfmUsesExpertOptions.GetData(out _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean;
  out _DisableCache, _SearchPathFavorites: Boolean; out _FilterIdentifiers: TFilterIdentifiersEnum;
  out _FastAdd, _AutoSort: Boolean;
  _PrefixGroups: TUsesPrefixGroupList; out _GroupSeparator: TGroupSeparator);
var
  i: Integer;
begin
  _ReadMapFile := chkReadMap.Checked;
  _ReplaceFileUseUnit := chkReplaceFileUnit.Checked;
  _ParseAll := chkParseAll.Checked;
  _DisableCache := chkDisableParserCache.Checked;
  _SearchPathFavorites := chkSearchPathFavorites.Checked;
  _FilterIdentifiers := TFilterIdentifiersEnum(rg_FilterIdentifiers.ItemIndex);
  _FastAdd := chk_FastAdd.Checked;
  _AutoSort := chk_AutoSort.Checked;

  _PrefixGroups.Clear;
  for i := 0 to lb_Prefixes.Items.Count - 1 do
    _PrefixGroups.Add(lb_Prefixes.Items[i]);

  _GroupSeparator := TGroupSeparator(rg_Separator.ItemIndex);
end;

procedure TfmUsesExpertOptions.SetData(const _CanReplaceFindUseUnit: Boolean; const _CacheDir: string;
  const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
  _FilterIdentifiers: TFilterIdentifiersEnum; _FastAdd, _AutoSort: Boolean;
  _PrefixGroups: TUsesPrefixGroupList; _GroupSeparator: TGroupSeparator);
var
  i: Integer;
begin
  chkReplaceFileUnit.Enabled := _CanReplaceFindUseUnit;
  FCacheDir := _CacheDir;
  btnClearCache.Enabled := (FCacheDir <> '') and DirectoryExists(FCacheDir);
  chkDisableParserCache.Hint := FCacheDir;
  chkDisableParserCache.ShowHint := (FCacheDir <> '');
  chkReadMap.Checked := _ReadMapFile;
  chkReplaceFileUnit.Checked := _ReplaceFileUseUnit;
  chkParseAll.Checked := _ParseAll;
  chkDisableParserCache.Checked := _DisableCache;
  chkSearchPathFavorites.Checked := _SearchPathFavorites;
  rg_FilterIdentifiers.ItemIndex := Ord(_FilterIdentifiers);
  chk_FastAdd.Checked := _FastAdd;
  chk_AutoSort.Checked := _AutoSort;

  lb_Prefixes.Items.Clear;
  for i := 0 to _PrefixGroups.Count - 1 do
    lb_Prefixes.Items.Add(_PrefixGroups.Groups[i]);

  rg_Separator.ItemIndex := Ord(_GroupSeparator);
  UpdatePrefixButtons;
end;

{ TResetPrefixGroupsMessage }

type
  TResetPrefixGroupsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

function TResetPrefixGroupsMessage.GetMessage: string;
resourcestring
  SResetPrefixGroups =
    'This will reset the prefix groups to their default values.'#13#10
    + 'Do you really want to do that?';
begin
  Result := SResetPrefixGroups;
end;

{ TClearCacheMessage }

type
  TClearCacheMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

function TClearCacheMessage.GetMessage: string;
resourcestring
  SClearCache =
    'This will clear the cache of the units parser stored in. '#13#10
    + '%s'#13#10
    + 'It will automatically be recreated the next time you open the Uses Clause Manager. '
    + 'But it will take longer to rebuild it than to simply load it. '#13#10
    + 'Do you really want to do that?';
begin
  Result := Format(SClearCache, [FData]);
end;

{ TCacheClearedMessage }

type
  TCacheClearedMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

function TCacheClearedMessage.GetMessage: string;
resourcestring
  SCacheCleared =
    'The units parser cache has been cleared.';
begin
  Result := SCacheCleared;
end;

procedure TfmUsesExpertOptions.btnClearCacheClick(Sender: TObject);
begin
  if (FCacheDir = '') or not DirectoryExists(FCacheDir) then
    Exit; //==>
  if ShowGxMessageBox(TClearCacheMessage, FCacheDir) <> mrYes then
    Exit; //==>
  TFileSystem.DelDirTree(FCacheDir);
  btnClearCache.Enabled := False;
  ShowGxMessageBox(TCacheClearedMessage);
end;

procedure TfmUsesExpertOptions.b_HelpClick(Sender: TObject);
begin
  inherited;
  GxContextHelp(Self, 65);
end;

procedure TfmUsesExpertOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key=vk_F1 then b_HelpClick(nil);
end;

procedure TfmUsesExpertOptions.UpdatePrefixButtons;
var
  HasSelection: Boolean;
begin
  HasSelection := lb_Prefixes.ItemIndex >= 0;
  btnPrefixEdit.Enabled := HasSelection;
  btnPrefixDelete.Enabled := HasSelection;
  btnPrefixUp.Enabled := HasSelection and (lb_Prefixes.ItemIndex > 0);
  btnPrefixDown.Enabled := HasSelection and (lb_Prefixes.ItemIndex < lb_Prefixes.Items.Count - 1);
end;

procedure TfmUsesExpertOptions.lb_PrefixesClick(Sender: TObject);
begin
  UpdatePrefixButtons;
end;

procedure TfmUsesExpertOptions.btnPrefixAddClick(Sender: TObject);
var
  s: string;
begin
  s := '';
  if InputQuery('Add Prefix', 'Namespace prefix (e.g. Vcl.)', s) and (s <> '') then begin
    lb_Prefixes.Items.Add(s);
    lb_Prefixes.ItemIndex := lb_Prefixes.Items.Count - 1;
    UpdatePrefixButtons;
  end;
end;

procedure TfmUsesExpertOptions.btnPrefixEditClick(Sender: TObject);
var
  Idx: Integer;
  s: string;
begin
  Idx := lb_Prefixes.ItemIndex;
  if Idx < 0 then
    Exit;
  s := lb_Prefixes.Items[Idx];
  if InputQuery('Edit Prefix', 'Namespace prefix (e.g. Vcl.)', s) and (s <> '') then begin
    lb_Prefixes.Items[Idx] := s;
    lb_Prefixes.ItemIndex := Idx;
  end;
end;

procedure TfmUsesExpertOptions.btnPrefixDeleteClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lb_Prefixes.ItemIndex;
  if Idx < 0 then
    Exit;
  lb_Prefixes.Items.Delete(Idx);
  if Idx >= lb_Prefixes.Items.Count then
    Idx := lb_Prefixes.Items.Count - 1;
  lb_Prefixes.ItemIndex := Idx;
  UpdatePrefixButtons;
end;

procedure TfmUsesExpertOptions.btnPrefixResetClick(Sender: TObject);
var
  Defaults: TUsesPrefixGroupList;
  i: Integer;
begin
  if ShowGxMessageBox(TResetPrefixGroupsMessage, '', Self) <> mrYes then
    Exit;
  Defaults := TUsesPrefixGroupList.Create;
  try
    Defaults.LoadDefaults;
    lb_Prefixes.Items.Clear;
    for i := 0 to Defaults.Count - 1 do
      lb_Prefixes.Items.Add(Defaults.Groups[i]);
  finally
    FreeAndNil(Defaults);
  end;
  UpdatePrefixButtons;
end;

procedure TfmUsesExpertOptions.btnPrefixUpClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lb_Prefixes.ItemIndex;
  if Idx <= 0 then
    Exit;
  lb_Prefixes.Items.Move(Idx, Idx - 1);
  lb_Prefixes.ItemIndex := Idx - 1;
  UpdatePrefixButtons;
end;

procedure TfmUsesExpertOptions.btnPrefixDownClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lb_Prefixes.ItemIndex;
  if (Idx < 0) or (Idx >= lb_Prefixes.Items.Count - 1) then
    Exit;
  lb_Prefixes.Items.Move(Idx, Idx + 1);
  lb_Prefixes.ItemIndex := Idx + 1;
  UpdatePrefixButtons;
end;

end.
