unit GX_ProcedureListOptions;

interface

uses
  Types, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls, GX_BaseForm,
  GX_ConfigurationInfo;

type
  TProcedureListOptions = class(TObject)
  private
    FAlignmentChanged: Boolean;
    FDialogFont: TFont;
    FCodeViewFont: TFont;
    FCodeViewAlignment: TAlign;
    FBoundsRect: TRect;
    FSortOnColumn: Integer;
    FCodeViewVisible: Boolean;
    FSearchAll: Boolean;
    FCodeViewWidth, FCodeViewHeight: Integer;
    FOptions: TProcedureListOptions;
    FObjectNameVisible: Boolean;
    FSearchClassName: Boolean;
    procedure SetBoundsRect(const AValue: TRect);
  public
    property AlignmentChanged: Boolean read FAlignmentChanged write FAlignmentChanged;
    property DialogFont: TFont read FDialogFont write FDialogFont;
    property CodeViewFont: TFont read FCodeViewFont write FCodeViewFont;
    property CodeViewAlignment: TAlign read FCodeViewAlignment write FCodeViewAlignment;
    property CodeViewVisible: Boolean read FCodeViewVisible write FCodeViewVisible;
    property CodeViewHeight: Integer read FCodeViewHeight write FCodeViewHeight;
    property CodeViewWidth: Integer read FCodeViewWidth write FCodeViewWidth;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property SortOnColumn: Integer read FSortOnColumn write FSortOnColumn;
    property SearchAll: Boolean read FSearchAll write FSearchAll;
    property SearchClassName: Boolean read FSearchClassName write FSearchClassName;
    property Options: TProcedureListOptions read FOptions write FOptions;
    property ObjectNameVisible: Boolean read FObjectNameVisible write FObjectNameVisible;
    procedure LoadSettings(_Settings: IExpertSettings);
    procedure SaveSettings(_Settings: IExpertSettings);

    constructor Create;
    destructor Destroy; override;
  end;

  TfmProcedureListOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbCodeView: TGroupBox;
    cbCVDock: TComboBox;
    lblDock: TLabel;
    grp_CvFont: TGroupBox;
    pnlCVFont: TPanel;
    btnChangeCodeViewFont: TButton;
    gbDialog: TGroupBox;
    grp_ListFont: TGroupBox;
    pnlDialogFont: TPanel;
    btnChgDialogFont: TButton;
    chkShowCodeView: TCheckBox;
    chkShowObjectName: TCheckBox;
    chkMatchAnywhere: TCheckBox;
    chkMatchClass: TCheckBox;
    procedure btnChgDialogFontClick(Sender: TObject);
    procedure btnChangeCodeViewFontClick(Sender: TObject);
    procedure cbCVDockChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FOptions: TProcedureListOptions;
    procedure SetCodeViewAlignment(Value: TAlign);
    function GetCodeViewAlignment: TAlign;
  public
    constructor Create(_Owner: TComponent); override;
    property Options: TProcedureListOptions read FOptions write FOptions;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Dialogs;

const
  CodeViewFontKey = 'CodeViewFont';
  DialogFontKey = 'DialogFont';

constructor TProcedureListOptions.Create;
begin
  FDialogFont := TFont.Create;
  FCodeViewFont := TFont.Create;
  FCodeViewFont.Name := 'Courier New';
  FCodeViewFont.Size := 8;
  AlignmentChanged := False;
  ObjectNameVisible := True;
  SortOnColumn := 1;
end;

destructor TProcedureListOptions.Destroy;
begin
  FreeAndNil(FDialogFont);
  FreeAndNil(FCodeViewFont);
  inherited;
end;

procedure TProcedureListOptions.LoadSettings(_Settings: IExpertSettings);

  function GetCodeViewAlignment(Value: string): TAlign;
  begin
    if Value = 'Top' then
      Result := alTop
    else if Value = 'Right' then
      Result := alRight
    else if Value = 'Left' then
      Result := alLeft
    else
      Result := alBottom;
  end;

begin
  // Do not localize any of the following lines
  FBoundsRect := _Settings.ReadBounds(Bounds(317, 279, 550, 500));
  FCodeViewVisible := _Settings.ReadBool('ShowProcedureBody', False);
  FCodeViewWidth := _Settings.ReadInteger('ProcedureWidth', 292);
  FCodeViewHeight := _Settings.ReadInteger('ProcedureHeight', 100);
  FCodeViewAlignment := GetCodeViewAlignment(_Settings.ReadString('ProcedureAlignment', 'Right'));
  FAlignmentChanged := _Settings.ReadBool('AlignmentChanged', False);
  FSortOnColumn := _Settings.ReadInteger('SortColumn', FSortOnColumn);
  _Settings.LoadFont(DialogFontKey, FDialogFont);
  _Settings.LoadFont(CodeViewFontKey, FCodeViewFont);
  FSearchAll := _Settings.ReadBool('SearchAll', True);
  FSearchClassName := _Settings.ReadBool('SearchClassName', True);
  FObjectNameVisible := _Settings.ReadBool('ShowObjectName', True);
end;

procedure TProcedureListOptions.SaveSettings(_Settings: IExpertSettings);

  function GetAlignmentString(Value: TAlign): string;
  begin
    case Value of
      alTop: Result := 'Top';
      alLeft: Result := 'Left';
      alRight: Result := 'Right';
      alBottom: Result := 'Bottom';
    else
      Result := 'Right';
    end;
  end;

begin
  // Do not localize any of the following lines
  _Settings.WriteBool('SearchAll', FSearchAll);
  _Settings.WriteBool('SearchClassName', FSearchClassName);
  _Settings.WriteBounds(BoundsRect);
  _Settings.WriteInteger('SortColumn', FSortOnColumn);
  _Settings.WriteInteger('ProcedureWidth', FCodeViewWidth);
  _Settings.WriteInteger('ProcedureHeight', FCodeViewHeight);
  _Settings.WriteString('ProcedureAlignment', GetAlignmentString(FCodeViewAlignment));
  _Settings.SaveFont(DialogFontKey, FDialogFont);
  _Settings.SaveFont(CodeViewFontKey, FCodeViewFont);
  _Settings.WriteBool('ShowProcedureBody', FCodeViewVisible);
  _Settings.WriteBool('AlignmentChanged', FAlignmentChanged);
  _Settings.WriteBool('ShowObjectName', FObjectNameVisible);
end;

procedure TProcedureListOptions.SetBoundsRect(const AValue: TRect);
begin
  FBoundsRect := AValue;
end;

{ TfmProcedureListOptions}

constructor TfmProcedureListOptions.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

procedure TfmProcedureListOptions.btnChgDialogFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font.Assign(pnlDialogFont.Font);
    if Execute then
      pnlDialogFont.Font.Assign(Font);
  finally
    Free;
  end;
end;

procedure TfmProcedureListOptions.btnChangeCodeViewFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Options := Options + [fdFixedPitchOnly];
    // Only show fixed font for source code
    Font.Assign(pnlCVFont.Font);
    if Execute then
      pnlCVFont.Font.Assign(Font);
  finally
    Free;
  end;
end;

function TfmProcedureListOptions.GetCodeViewAlignment: TAlign;
var
  sDock: string;
begin
  sDock := cbCVDock.Items[cbCVDock.ItemIndex];
  if sDock = 'Top' then
    Result := alTop
  else if sDock = 'Right' then
    Result := alRight
  else if sDock = 'Left' then
    Result := alLeft
  else
    Result := alBottom;
end;

procedure TfmProcedureListOptions.SetCodeViewAlignment(Value: TAlign);
begin
  case Value of
    alTop: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Top');
    alLeft: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Left');
    alRight: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Right');
    alBottom: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Bottom');
  else
    cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Right');
  end;
end;

procedure TfmProcedureListOptions.cbCVDockChange(Sender: TObject);
begin
  FOptions.AlignmentChanged := True;
end;

procedure TfmProcedureListOptions.FormShow(Sender: TObject);
begin
  FOptions.AlignmentChanged := False;
  chkShowCodeView.Checked := FOptions.CodeViewVisible;
  chkShowObjectName.Checked := FOptions.ObjectNameVisible;
  SetCodeViewAlignment(FOptions.CodeViewAlignment);
  pnlDialogFont.Font.Assign(FOptions.DialogFont);
  pnlCVFont.Font.Assign(FOptions.CodeViewFont);
  chkMatchAnywhere.Checked := FOptions.SearchAll;
  chkMatchClass.Checked := FOptions.SearchClassName;
end;

procedure TfmProcedureListOptions.btnOKClick(Sender: TObject);
begin
  FOptions.CodeViewVisible := chkShowCodeView.Checked;
  FOptions.ObjectNameVisible := chkShowObjectName.Checked;
  FOptions.CodeViewAlignment := GetCodeViewAlignment;
  FOptions.DialogFont.Assign(pnlDialogFont.Font);
  FOptions.CodeViewFont.Assign(pnlCVFont.Font);
  FOptions.SearchAll := chkMatchAnywhere.Checked;
  FOptions.SearchClassName := chkMatchClass.Checked;
end;

end.

