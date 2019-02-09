unit GX_UsesExpertOptions;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TfmUsesExpertOptions = class(TForm)
    chkReplaceFileUnit: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkReadMap: TCheckBox;
    chkParseAll: TCheckBox;
    chkDisableParserCache: TCheckBox;
    btnClearCache: TButton;
    procedure btnClearCacheClick(Sender: TObject);
  private
    FCacheDir: string;
    procedure SetData(const _CanReplaceFindUseUnit: Boolean; const _CacheDir: string;
      const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache: Boolean);
    procedure GetData(out _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache: Boolean);
  public
    class function Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
      const _CacheDir: string;
      var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses
  GX_MessageBox, GX_dzFileUtils;

{ TfmUsesExpertOptions }

class function TfmUsesExpertOptions.Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
  const _CacheDir: string;
  var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache: Boolean): Boolean;
var
  frm: TfmUsesExpertOptions;
begin
  frm := TfmUsesExpertOptions.Create(_Owner);
  try
    frm.SetData(_CanReplaceFindUseUnit, _CacheDir, _ReadMapFile, _ReplaceFileUseUnit, _ParseAll,
      _DisableCache);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmUsesExpertOptions.GetData(out _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean;
  out _DisableCache: Boolean);
begin
  _ReadMapFile := chkReadMap.Checked;
  _ReplaceFileUseUnit := chkReplaceFileUnit.Checked;
  _ParseAll := chkParseAll.Checked;
  _DisableCache := chkDisableParserCache.Checked;
end;

procedure TfmUsesExpertOptions.SetData(const _CanReplaceFindUseUnit: Boolean; const _CacheDir: string;
  const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll, _DisableCache: Boolean);
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

