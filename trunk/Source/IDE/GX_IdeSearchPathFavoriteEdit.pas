unit GX_IdeSearchPathFavoriteEdit;

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
  Menus,
  ActnList,
  Actions,
  StdCtrls,
  ExtCtrls,
  SynMemo,
  GX_BaseForm;

type
  Tf_IdeSearchPathFavoriteEdit = class(TfmBaseForm)
    l_Name: TLabel;
    ed_Name: TEdit;
    l_Path: TLabel;
    b_OK: TButton;
    b_Cancel: TButton;
    b_Select: TButton;
    b_Help: TButton;
    b_MakeRelative: TButton;
    b_MakeAbsolute: TButton;
    b_PrependDots: TButton;
    b_RemoveDots: TButton;
    TheActionList: TActionList;
    act_MakeRelative: TAction;
    act_MakeAbsolute: TAction;
    act_PrependDots: TAction;
    act_RemoveDots: TAction;
    act_SelectDirectory: TAction;
    act_OK: TAction;
    act_Cancel: TAction;
    p_Memo: TPanel;
    pm_Memo: TPopupMenu;
    mi_MemoMoveUp: TMenuItem;
    mi_MemoMoveDown: TMenuItem;
    N3: TMenuItem;
    mi_MemoPrependDots: TMenuItem;
    mi_MemoRemoveDots: TMenuItem;
    mi_MemoMakeRelative: TMenuItem;
    mi_MemoMakeAbsolute: TMenuItem;
    procedure act_SelectDirectoryExecute(Sender: TObject);
    procedure b_HelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure act_OKExecute(Sender: TObject);
    procedure act_CancelExecute(Sender: TObject);
    procedure act_MakeRelativeExecute(Sender: TObject);
    procedure act_MakeAbsoluteExecute(Sender: TObject);
    procedure act_PrependDotsExecute(Sender: TObject);
    procedure act_RemoveDotsExecute(Sender: TObject);
    procedure b_CancelClick(Sender: TObject);
  private
    FOnCallHelpContext: TNotifyEvent;
    FMemo: TSynMemo;
    procedure GetData(out _Name, _Path: string);
    procedure SetData(const _Name, _Path: string; _OnCallHelpContext: TNotifyEvent = nil);
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
  protected
    procedure CreateWnd; override; // show sizeable form like a dialog
    procedure CreateParams(var Params: TCreateParams); override; // form without caption icon
  public
    class function Execute(_Owner: TComponent; var _Name, _Path: string; _OnCallHelpContext: TNotifyEvent = nil): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  SynEdit,
  GX_GenericUtils,
  GX_OtaUtils,
  GX_EnhancedEditor,
  GX_IdeUtils,
  GX_SynMemoUtils;

{ Tf_IdeSearchPathFavoriteEdit }

class function Tf_IdeSearchPathFavoriteEdit.Execute(_Owner: TComponent; var _Name, _Path: string; _OnCallHelpContext: TNotifyEvent = nil): Boolean;
var
  frm: Tf_IdeSearchPathFavoriteEdit;
begin
  frm := Tf_IdeSearchPathFavoriteEdit.Create(_Owner);
  try
    frm.SetData(_Name, _Path, _OnCallHelpContext);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Name, _Path);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_IdeSearchPathFavoriteEdit.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(self);

  FMemo := TSynMemo.Create(self);
  FMemo.Parent := p_Memo;
  FMemo.Align := alClient;
  FMemo.HideSelection := False;
  FMemo.WordWrap := False;
  TSynMemo_SetDelimiter(FMemo);

{$IFDEF GX_SUPPORTS_THEMING}
  FMemo.StyleElements := [];
  FMemo.ParentColor := False;
  FMemo.Color := GetIdeEditorBackgroundColor;
  FMemo.Font.Color := GetIdeEditorForegroundColor;
  FMemo.ActiveLineColor := GetIdeEditorLineHighlightColor;
{$ELSE}
  FMemo.ActiveLineColor := TGxEnhancedEditor.DefaultActiveLineColor;
{$ENDIF}
  FMemo.RightEdge := 0;
  FMemo.Gutter.Width := 0;
  FMemo.Options := FMemo.Options - [eoScrollPastEol, eoTabsToSpaces] + [eoHideShowScrollbars];
  FMemo.PopupMenu := pm_Memo;

  TWinControl_ActivateDropFiles(FMemo, HandleFilesDropped);

  InitDpiScaler;
end;

procedure Tf_IdeSearchPathFavoriteEdit.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  FMemo.Lines.AddStrings(_Files);
end;

procedure Tf_IdeSearchPathFavoriteEdit.GetData(out _Name, _Path: string);
begin
  _Name := ed_Name.Text;
  _Path := TSynMemo_GetDelimitedText(FMemo);
  _Path := AnsiDequotedStr(_Path, '"');
end;

procedure Tf_IdeSearchPathFavoriteEdit.SetData(const _Name, _Path: string; _OnCallHelpContext: TNotifyEvent = nil);
begin
  ed_Name.Text := _Name;
  TSynMemo_SetDelimitedText(FMemo, AnsiDequotedStr(_Path, '"'));
  FOnCallHelpContext := _OnCallHelpContext;

  b_Help.Visible := Assigned(_OnCallHelpContext);
  if not (b_Help.Visible) then
    b_Select.Left := b_Help.Left;

  if _Path = '' then
    Caption := Caption + ', Add'
  else
    Caption := Caption + ', Edit';
end;

procedure Tf_IdeSearchPathFavoriteEdit.act_CancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure Tf_IdeSearchPathFavoriteEdit.act_MakeAbsoluteExecute(Sender: TObject);
begin
  TSynMemoUtilMakePathsAbsolute.ProcessAllLines(FMemo);
end;

procedure Tf_IdeSearchPathFavoriteEdit.act_MakeRelativeExecute(Sender: TObject);
begin
  TSynMemoUtilMakePathsRelative.ProcessAllLines(FMemo);
end;

procedure Tf_IdeSearchPathFavoriteEdit.act_OKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure Tf_IdeSearchPathFavoriteEdit.act_PrependDotsExecute(Sender: TObject);
begin
  TSynMemoUtilPrependDots.ProcessSelectedLines(FMemo);
end;

procedure Tf_IdeSearchPathFavoriteEdit.act_RemoveDotsExecute(Sender: TObject);
begin
  TSynMemoUtilRemoveDots.ProcessSelectedLines(FMemo);
end;

procedure Tf_IdeSearchPathFavoriteEdit.act_SelectDirectoryExecute(Sender: TObject);
var
  Dir: string;
begin
  Dir := '';
  if GetDirectory('Select directory to add', Dir, self) then
    FMemo.Lines.Add(Dir);
end;

procedure Tf_IdeSearchPathFavoriteEdit.b_CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure Tf_IdeSearchPathFavoriteEdit.b_HelpClick(Sender: TObject);
begin
  inherited;
  FOnCallHelpContext(self);
end;

procedure Tf_IdeSearchPathFavoriteEdit.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = vk_F1 then
    b_HelpClick(nil);
end;

// show sizeable form like a dialog
// form without caption icon
procedure Tf_IdeSearchPathFavoriteEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure Tf_IdeSearchPathFavoriteEdit.CreateWnd;
begin
  inherited;
  SendMessage(Handle, WM_SETICON, 1, 0);
end;

end.

