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
    edtFilter: TEdit;
    btnClear: TButton;
    pnlButtonsRight: TPanel;
    btnConfiguration: TButton;
    procedure edtFilterChange(Sender: TObject);
    procedure lstMacrosDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure btnConfigurationClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    FMacroFile: TMacroFile;
    procedure SelectTemplate(Index: Integer);
    procedure LoadFormLayout;
    procedure SaveFormLayout;
    procedure SizeColumns;
    function WindowPosKey: string;
  public
    ///<summary>
    /// Shows the dialog to select a macro template
    /// @Returns the index of the selected macro or -1 if the dialog was cancelled </summary>
    class function Execute(const ATemplateName: string; MacroFile: TMacroFile): Integer;
    constructor Create(_Owner: TComponent); override;
    function GetSelectedMacroCode: Integer;
    procedure LoadTemplates(AMacroFile: TMacroFile; const Filter: string = '');
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  u_dzVclUtils, u_dzStringUtils,
  GX_MacroTemplatesExpert, GX_ConfigurationInfo, GX_GenericUtils;

{ TfmMacroSelect }

class function TfmMacroSelect.Execute(const ATemplateName: string; MacroFile: TMacroFile): Integer;
var
  frm: TfmMacroSelect;
begin
  Assert(Assigned(MacroFile));

  frm := TfmMacroSelect.Create(Application);
  try
    frm.LoadTemplates(MacroFile);
    frm.edtFilter.Text := ATemplateName;
    frm.edtFilter.SelectAll;
    if frm.ShowModal = mrOk then
      Result := frm.GetSelectedMacroCode
    else
      Result := -1;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmMacroSelect.Create(_Owner: TComponent);
begin
  inherited;
  LoadFormLayout;

  InitDpiScaler;
end;

procedure TfmMacroSelect.SelectTemplate(Index: Integer);
begin
  lvMacros.Selected := lvMacros.Items[Index];
  lvMacros.ItemFocused := lvMacros.Selected;
  lvMacros.Selected.MakeVisible(False);
end;

procedure TfmMacroSelect.edtFilterChange(Sender: TObject);
begin
  LoadTemplates(GetExpandMacroExpert.MacroFile, edtFilter.Text);
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
var
  FirstMatch: Integer;

  procedure AddMacroToList(const AMacroName, AMacroDesc: string);
  var
    ListItem: TListItem;
  begin
    if (Filter = '') then begin
      FirstMatch := 0;
    end else if StrContains(Filter, AMacroName, False) or StrContains(Filter, AMacroDesc, False) then begin
      if StartsText(Filter, AMacroName) then begin
        if (FirstMatch = -1) or SameText(Filter, AMacroName) then begin
          FirstMatch := lvMacros.Items.Count;
        end;
      end;
    end else begin
      // no match, don't add
      Exit; //==>
    end;
    ListItem := lvMacros.Items.Add;
    ListItem.Caption := AMacroName;
    ListItem.SubItems.Add(AMacroDesc);
  end;

var
  i: Integer;
begin
  FMacroFile := AMacroFile;
  lvMacros.Items.BeginUpdate;
  try
    lvMacros.Items.Clear;
    FirstMatch := -1;
    for i := 0 to AMacroFile.MacroCount - 1 do
      AddMacroToList(AMacroFile.MacroItems[i].Name, AMacroFile.MacroItems[i].Desc);
    if FirstMatch <> -1 then
      SelectTemplate(FirstMatch);
    SizeColumns;
  finally
    lvMacros.Items.EndUpdate;
  end;
end;

procedure TfmMacroSelect.edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TfmMacroSelect.btnClearClick(Sender: TObject);
begin
  edtFilter.Text := '';
  edtFilter.SetFocus;
end;

procedure TfmMacroSelect.btnConfigurationClick(Sender: TObject);
begin
  Assert(Assigned(FMacroFile));
  GetExpandMacroExpertReq.Configure(Self);
  LoadTemplates(GetExpandMacroExpertReq.MacroFile);
end;

function TfmMacroSelect.WindowPosKey: string;
begin
  Result := 'SelectWindow';
end;

end.

