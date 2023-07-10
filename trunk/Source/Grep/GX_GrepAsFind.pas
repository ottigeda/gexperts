unit GX_GrepAsFind;

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
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  GX_BaseForm,
  GX_EventHook;

type
  TfmGrepAsFind = class(TfmBaseForm)
    pc_Main: TPageControl;
    ts_Find: TTabSheet;
    l_TextToFind: TLabel;
    cmb_TextToFind: TComboBox;
    grp_Options: TGroupBox;
    chk_CaseSensitive: TCheckBox;
    chk_WholeWords: TCheckBox;
    chk_RegExpr: TCheckBox;
    grp_Direction: TGroupBox;
    rb_DirectionForward: TRadioButton;
    rb_DirectionBackward: TRadioButton;
    grp_Scope: TGroupBox;
    rb_ScopeGlobal: TRadioButton;
    rb_ScopeSelected: TRadioButton;
    grp_Origin: TGroupBox;
    rb_OriginFromCursor: TRadioButton;
    rb_OriginEntireScope: TRadioButton;
    p_Bottom: TPanel;
    ts_FindInFiles: TTabSheet;
    cmb_FifTextToFind: TComboBox;
    l_FifTextToFind: TLabel;
    grp_FifOptions: TGroupBox;
    chk_FifRegExpr: TCheckBox;
    chk_FifWholeWords: TCheckBox;
    chk_FifCaseSensitive: TCheckBox;
    grp_FifWhere: TGroupBox;
    rb_FifAllFilesInProject: TRadioButton;
    rb_FifProjectGropu: TRadioButton;
    rb_FifAllOpen: TRadioButton;
    rb_FifDirectoris: TRadioButton;
    grp_FifDirOptions: TGroupBox;
    l_FifFileMask: TLabel;
    cmb_FifFileMask: TComboBox;
    b_FifBrowse: TButton;
    chk_FifIncludeSubDirs: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
    procedure b_FifBrowseClick(Sender: TObject);
    procedure b_OkClick(Sender: TObject);
  private
    procedure HandleDropFiles(_Sender: TObject; _Files: TStrings);
  public
    constructor Create(_Owner: TComponent); override;
    class function ExecuteFindFile(_Owner: TWinControl): Boolean;
  end;

implementation

{$R *.dfm}

uses
  FileCtrl,
  u_dzVclUtils,
  u_dzSelectDirectoryFix,
  GX_GrepExpert,
  GX_GrepBackend,
  ActnList;

{ TfmGrepAsFind }

procedure TfmGrepAsFind.b_OkClick(Sender: TObject);
begin
  gblGrepExpert.GrepCode := True;
  gblGrepExpert.GrepComments := True;
  gblGrepExpert.GrepStrings := True;
  gblGrepExpert.GrepFinalization := True;
  gblGrepExpert.GrepImplementation := True;
  gblGrepExpert.GrepInitialization := True;
  gblGrepExpert.GrepInterface := True;
  gblGrepExpert.GrepForms := False;
  gblGrepExpert.GrepFormsMultiline := False;
  gblGrepExpert.GrepFormsSpecialChars := False;
  gblGrepExpert.GrepSQLFiles := False;
  gblGrepExpert.GrepSaveOption := gsoNoSave;

  gblGrepExpert.GrepMinDepth := 0;
  gblGrepExpert.GrepMaxDepth := -1;
  gblGrepExpert.ExcludedDirsIsRegEx := False;
  gblGrepExpert.GrepUseMapFile := False;

  if pc_Main.ActivePage = ts_Find then begin
    gblGrepExpert.GrepCaseSensitive := chk_CaseSensitive.Checked;
    gblGrepExpert.GrepSub := False;
    gblGrepExpert.GrepWholeWord := chk_WholeWords.Checked;
    gblGrepExpert.GrepRegEx := chk_RegExpr.Checked;
    gblGrepExpert.GrepAction := gaCurrentOnlyGrep;
  end else begin
    gblGrepExpert.GrepCaseSensitive := chk_FifCaseSensitive.Checked;
    gblGrepExpert.GrepSub := chk_FifIncludeSubDirs.Checked;
    gblGrepExpert.GrepWholeWord := chk_FifWholeWords.Checked;
    gblGrepExpert.GrepRegEx := chk_FifRegExpr.Checked;
    if rb_FifAllOpen.Checked then
      gblGrepExpert.GrepAction := gaOpenFilesGrep
    else if rb_FifDirectoris.Checked then
      gblGrepExpert.GrepAction := gaDirGrep
    else if rb_FifProjectGropu.Checked then
      gblGrepExpert.GrepAction := gaProjGroupGrep
    else
      gblGrepExpert.GrepAction := gaProjGrep;
  end;
  ModalResult := mrOk;
end;

constructor TfmGrepAsFind.Create(_Owner: TComponent);
begin
  inherited;
  pc_Main.ActivePage := ts_Find;
  // the Find in files dialog still exists, so for now we don't replace it.
  // but turn hide that tab sheet.
  ts_FindInFiles.TabVisible := False;
  TWinControl_ActivateDropFiles(cmb_FifFileMask, HandleDropFiles)
end;

class function TfmGrepAsFind.ExecuteFindFile(_Owner: TWinControl): Boolean;
var
  frm: TfmGrepAsFind;
begin
  frm := TfmGrepAsFind.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    Result := (mrOk = frm.ShowModal);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmGrepAsFind.b_FifBrowseClick(Sender: TObject);
var
  Directory: string;
begin
  inherited;
  Directory := cmb_FifFileMask.Text;
  if SelectDirectory('Select folde to search', '', Directory, [sdNewUI], Self) then
    cmb_FifFileMask.Text := Directory;
end;

procedure TfmGrepAsFind.HandleDropFiles(_Sender: TObject; _Files: TStrings);
begin
  if _Files.Count > 0 then
    cmb_FifFileMask.Text := _Files[0];
end;

end.

