unit GX_EditorEnhancementsConfig;

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
  StdCtrls,
  ExtCtrls,
  GX_BaseForm;

type
  TfmEditorEnhancementsConfig = class(TfmBaseForm)
    gbxEditorToolBar: TGroupBox;
    chkEditorToolBar: TCheckBox;
    rgAlign: TRadioGroup;
    btnConfigureToolBar: TButton;
    gbxEditorTabs: TGroupBox;
    chkMultiLine: TCheckBox;
    chkHotTrack: TCheckBox;
    chkButtons: TCheckBox;
    chkEditTabButtonsFlat: TCheckBox;
    chkMiddleButtonClose: TCheckBox;
    lblHideNavBar: TLabel;
    chkHideNavbar: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
    b_Help: TButton;
    procedure b_HelpClick(Sender: TObject);
    procedure btnConfigureToolBarClick(Sender: TObject);
    procedure chkButtonsClick(Sender: TObject);
    procedure chkEditorToolBarClick(Sender: TObject);
  private
    FToolbarActions: TStrings;
    procedure SetData(_ToolbarVisible: boolean; _ToolbarPos: TAlign;
      _ToolbarActions: TStrings;
      _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat: boolean;
      _HideNavbar: boolean);
    procedure GetData(out _ToolbarVisible: boolean; out _ToolbarPos: TAlign;
      _ToolbarActions: TStrings;
      out _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat: boolean;
      out _HideNavbar: boolean);
  public
    class function Execute(_Owner: TWinControl;
      var _ToolbarVisible: boolean; var _ToolbarPos: TAlign; _ToolbarActions: TStrings;
      var _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat: boolean;
      var _HideNavbar: boolean): boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  GX_GxUtils,
  GX_IdeUtils,
  GX_ToolbarConfig;

{ TfmEditorEnhancementsConfig }

procedure TfmEditorEnhancementsConfig.btnConfigureToolBarClick(Sender: TObject);
begin
  TfmToolbarConfig.Execute(Self, FToolbarActions);
end;

procedure TfmEditorEnhancementsConfig.b_HelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 35)
end;

constructor TfmEditorEnhancementsConfig.Create(_Owner: TComponent);
begin
  inherited;

  FToolbarActions := TStringList.Create;

  gbxEditorTabs.Visible := RunningDelphi7OrLess;
{$IFNDEF GX_VER300_up} // RAD Studio 10 Seattle (24; BDS 17)
  chkHideNavbar.Visible := False;
{$ENDIF}
{$IFDEF GX_VER320_up} // RAD Studio 10.2 Tokyo (26; BDS 19)
  chkHideNavbar.Visible := False;
  lblHideNavBar.Visible := True;
{$ENDIF}
end;

destructor TfmEditorEnhancementsConfig.Destroy;
begin
  FreeAndNil(FToolbarActions);
  inherited;
end;

class function TfmEditorEnhancementsConfig.Execute(_Owner: TWinControl;
  var _ToolbarVisible: boolean; var _ToolbarPos: TAlign; _ToolbarActions: TStrings;
  var _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat: boolean;
  var _HideNavbar: boolean): boolean;
var
  frm: TfmEditorEnhancementsConfig;
begin
  frm := TfmEditorEnhancementsConfig.Create(_Owner);
  try
    frm.SetData(_ToolbarVisible, _ToolbarPos, _ToolbarActions,
      _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat,
      _HideNavbar);
    Result := (mrOk = frm.ShowModal);
    if Result then
      frm.GetData(_ToolbarVisible, _ToolbarPos, _ToolbarActions,
        _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat,
        _HideNavbar);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmEditorEnhancementsConfig.GetData(out _ToolbarVisible: boolean; out _ToolbarPos: TAlign;
  _ToolbarActions: TStrings;
  out _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat: boolean;
  out _HideNavbar: boolean);
begin
  _ToolbarVisible := chkEditorToolBar.Checked;
  case rgAlign.ItemIndex of
    1: _ToolbarPos := alBottom;
    2: _ToolbarPos := alLeft;
    3: _ToolbarPos := alRight;
  else
    _ToolbarPos := alTop;
  end;
  _ToolbarActions.Assign(FToolbarActions);

  _HotTrack := chkHotTrack.Checked;
  _MultiLine := chkMultiLine.Checked;
  _MiddleButtonClose := chkMiddleButtonClose.Checked;
  _Buttons := chkButtons.Checked;
  _ButtonsFlat := chkEditTabButtonsFlat.Checked;

  _HideNavbar := chkHideNavbar.Checked;
end;

procedure TfmEditorEnhancementsConfig.SetData(_ToolbarVisible: boolean; _ToolbarPos: TAlign;
  _ToolbarActions: TStrings;
  _HotTrack, _MultiLine, _MiddleButtonClose, _Buttons, _ButtonsFlat: boolean;
  _HideNavbar: boolean);
begin
  chkEditorToolBar.Checked := _ToolbarVisible;
  case _ToolbarPos of
    alLeft: rgAlign.ItemIndex := 2;
    alRight: rgAlign.ItemIndex := 3;
    alBottom: rgAlign.ItemIndex := 1;
  else
    rgAlign.ItemIndex := 0;
  end;
  FToolbarActions.Assign(_ToolbarActions);

  chkHotTrack.Checked := _HotTrack;
  chkMultiLine.Checked := _MultiLine;
  chkMiddleButtonClose.Checked := _MiddleButtonClose;
  chkButtons.Checked := _Buttons;
  chkEditTabButtonsFlat.Checked := _ButtonsFlat;

  chkHideNavbar.Checked := _HideNavbar;
end;

procedure TfmEditorEnhancementsConfig.chkButtonsClick(Sender: TObject);
var
  EnableState: boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
    (Sender as TCheckBox).Enabled;

  chkEditTabButtonsFlat.Enabled := EnableState;
end;

procedure TfmEditorEnhancementsConfig.chkEditorToolBarClick(Sender: TObject);
var
  EnableState: boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
    (Sender as TCheckBox).Enabled;

  rgAlign.Enabled := EnableState;
  btnConfigureToolBar.Enabled := EnableState;
end;

end.
