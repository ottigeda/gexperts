// Dialog to select a macro template
unit GX_MacroSelect;

interface
uses
  Windows, Messages, SysUtils, Classes,
  Controls, Forms, ExtCtrls, StdCtrls, ComCtrls, CommCtrl,
  GX_MacroFile, GX_BaseForm;

type
  TfmMacroSelect = class(TfmBaseForm)
    pnlMain: TPanel;
    lvMacros: TListView;
    pnlHeader: TPanel;
    lblFilter: TLabel;
    tbEnter: TEdit;
    pnlButtonsRight: TPanel;
    btnConfiguration: TButton;
    procedure tbEnterChange(Sender: TObject);
    procedure lstMacrosDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbEnterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure btnConfigurationClick(Sender: TObject);
  private
    FMacroFile: TMacroFile;
    procedure SelectTemplate(Index: Integer);
    procedure LoadFormLayout;
    procedure SaveFormLayout;
    procedure SizeColumns;
    function WindowPosKey: string;
  public
    function GetSelectedMacroCode: Integer;
    procedure LoadTemplates(AMacroFile: TMacroFile; const Filter: string = '');
  end;

// Returns the index of the selected macro
function GetTemplateFromUser(const ATemplateName: string; MacroFile: TMacroFile): Integer;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  GX_MacroTemplatesExpert, GX_ConfigurationInfo, GX_GenericUtils;

function GetTemplateFromUser(const ATemplateName: string; MacroFile: TMacroFile): Integer;
var
  Int: IInterface;
  frm: TfmMacroSelect;
begin
  Assert(Assigned(MacroFile));

  // This buys (me) some time with adapting forms for High DPI by temporarily turning off
  // High DPI awareness. Works only for forms that are shown modally and don't
  // call into the IDE before closing.
  // All this is only necessary for Delphi 11 and later.
  // It does nothing for older Delphi versions.
  Int := TemporarilyDisableHighDpi;
  frm := TfmMacroSelect.Create(Application);
  try
    frm.TemporarilyDisableHighDpiInterface := Int;
    Int := nil;

    frm.LoadTemplates(MacroFile);
    frm.tbEnter.Text := ATemplateName;
    frm.tbEnter.SelectAll;
    if frm.ShowModal = mrOk then
      Result := frm.GetSelectedMacroCode
    else
      Result := -1;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmMacroSelect.SelectTemplate(Index: Integer);
begin
  lvMacros.Selected := lvMacros.Items[Index];
  lvMacros.ItemFocused := lvMacros.Selected;
  lvMacros.Selected.MakeVisible(False);
end;

procedure TfmMacroSelect.tbEnterChange(Sender: TObject);
begin
  LoadTemplates(GetExpandMacroExpert.MacroFile, tbEnter.Text);
end;

procedure TfmMacroSelect.lstMacrosDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfmMacroSelect.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ModalResult := mrOk;
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

function TfmMacroSelect.GetSelectedMacroCode: Integer;
var
  MacroName: string;
begin
  Result := -1;
  if lvMacros.Selected <> nil then
  begin
    MacroName := lvMacros.Selected.Caption;
    if Assigned(FMacroFile) then
      Result := FMacroFile.IndexOf(MacroName);
  end;
end;

procedure TfmMacroSelect.LoadTemplates(AMacroFile: TMacroFile; const Filter: string = '');

  procedure AddMacroToList(const AMacroName, AMacroDesc: string);
  var
    ListItem: TListItem;
  begin
    if (Filter = '')
      or StrContains(Filter, AMacroName, False) or StrContains(Filter, AMacroDesc, False)  then
    begin
      ListItem := lvMacros.Items.Add;
      ListItem.Caption := AMacroName;
      ListItem.SubItems.Add(AMacroDesc);
    end;
  end;

  procedure FocusAndSelectFirstItem;
  begin
    if lvMacros.Items.Count > 0 then
      SelectTemplate(0);
  end;

var
  i: Integer;
begin
  FMacroFile := AMacroFile;
  lvMacros.Items.BeginUpdate;
  try
    lvMacros.Items.Clear;
    for i := 0 to AMacroFile.MacroCount - 1 do
      AddMacroToList(AMacroFile.MacroItems[i].Name, AMacroFile.MacroItems[i].Desc);
    FocusAndSelectFirstItem;
    SizeColumns;
  finally
    lvMacros.Items.EndUpdate;
  end;
end;

procedure TfmMacroSelect.tbEnterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) or (Key = VK_UP) or
    (Key = VK_NEXT) or (Key = VK_PRIOR) then
  begin
    SendMessage(lvMacros.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmMacroSelect.LoadFormLayout;
var
  Settings: IExpertSettings;
begin
  // do not localize
  Settings := TMacroTemplatesExpert.GetSettings;
  Settings.LoadForm(WindowPosKey, Self, [fsSize]);
  Settings := Settings.Subkey(WindowPosKey);
  lvMacros.Columns[0].Width := Settings.ReadInteger('NameWidth', lvMacros.Columns[0].Width);
end;

procedure TfmMacroSelect.SaveFormLayout;
var
  Settings: IExpertSettings;
begin
  // do not localize
  Settings := TMacroTemplatesExpert.GetSettings;
  if WindowState = wsNormal then // save only if not maximized/minimized
    Settings.SaveForm(WindowPosKey, Self, [fsSize]);
  Settings := Settings.Subkey(WindowPosKey);
  Settings.WriteInteger('NameWidth', lvMacros.Columns[0].Width);
end;

procedure TfmMacroSelect.FormCreate(Sender: TObject);
begin
  LoadFormLayout;
end;

procedure TfmMacroSelect.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveFormLayout;
end;

procedure TfmMacroSelect.FormResize(Sender: TObject);
begin
  SizeColumns;
end;

procedure TfmMacroSelect.SizeColumns;
begin
  if lvMacros.Items.Count > 0 then
    ListView_SetColumnWidth(lvMacros.Handle, 1, ColumnTextWidth)
  else
    lvMacros.Columns[1].Width := 200;
end;

procedure TfmMacroSelect.btnConfigurationClick(Sender: TObject);
begin
  Assert(Assigned(FMacroFile));
  GetExpandMacroExpertReq.Configure;
  LoadTemplates(GetExpandMacroExpertReq.MacroFile);
end;

function TfmMacroSelect.WindowPosKey: string;
begin
  Result := 'SelectWindow';
end;

end.

