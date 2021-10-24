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
  GX_BaseForm;

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
    procedure btnClearCacheClick(Sender: TObject);
  private
    FCacheDir: string;
    procedure SetData(const _CanReplaceFindUseUnit: Boolean; const _CacheDir: string;
      const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
      _FilterIdentifiers: TFilterIdentifiersEnum);
    procedure GetData(out _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
      out _FilterIdentifiers: TFilterIdentifiersEnum);
  public
    class function Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
      const _CacheDir: string;
      var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
      var _FilterIdentifiers: TFilterIdentifiersEnum): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzFileUtils,
  u_dzVclUtils,
  GX_MessageBox;

{ TfmUsesExpertOptions }

class function TfmUsesExpertOptions.Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
  const _CacheDir: string;
  var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
  var _FilterIdentifiers: TFilterIdentifiersEnum): Boolean;
var
  frm: TfmUsesExpertOptions;
begin
  frm := TfmUsesExpertOptions.Create(_Owner);
  try
    frm.SetData(_CanReplaceFindUseUnit, _CacheDir, _ReadMapFile, _ReplaceFileUseUnit, _ParseAll,
      _DisableCache, _SearchPathFavorites, _FilterIdentifiers);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites,
        _FilterIdentifiers);
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
  out _DisableCache, _SearchPathFavorites: Boolean; out _FilterIdentifiers: TFilterIdentifiersEnum);
begin
  _ReadMapFile := chkReadMap.Checked;
  _ReplaceFileUseUnit := chkReplaceFileUnit.Checked;
  _ParseAll := chkParseAll.Checked;
  _DisableCache := chkDisableParserCache.Checked;
  _SearchPathFavorites := chkSearchPathFavorites.Checked;
  _FilterIdentifiers := TFilterIdentifiersEnum(rg_FilterIdentifiers.ItemIndex);
end;

procedure TfmUsesExpertOptions.SetData(const _CanReplaceFindUseUnit: Boolean; const _CacheDir: string;
  const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache, _SearchPathFavorites: Boolean;
  _FilterIdentifiers: TFilterIdentifiersEnum);
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

end.
