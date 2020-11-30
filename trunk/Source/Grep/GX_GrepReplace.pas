unit GX_GrepReplace;

interface

uses
  Classes, Controls, StdCtrls, Forms,
  GX_GrepExpert, GX_GrepBackend, GX_BaseForm;

type
  TfmGrepReplace = class(TfmBaseForm)
    lblWith: TLabel;
    cbReplace: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    lblIn: TLabel;
    lblInString: TLabel;
    lblReplace: TLabel;
    lblReplaceString: TLabel;
    chkUseRegEx: TCheckBox;
    procedure btnHelpClick(Sender: TObject);
  private
    FGrepExpert: TGrepExpert;
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    procedure SetSearchString(const Value: string);
    procedure SetReplaceInString(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RetrieveSettings(var Value: TGrepSettings);
    property GrepExpert: TGrepExpert read FGrepExpert;
    property SearchString: string write SetSearchString;
    property ReplaceInString: string write SetReplaceInString;
  end;

implementation

{$R *.dfm}

uses SysUtils,
  GX_GenericUtils, GX_GxUtils, GX_GrepResults;

constructor TfmGrepReplace.Create(AOwner: TComponent);
begin
  inherited;
  LoadFormSettings;
end;

destructor TfmGrepReplace.Destroy;
begin
  SaveFormSettings;
  inherited;
end;

procedure TfmGrepReplace.SaveFormSettings;
begin
  AddMRUString(cbReplace.Text, FGrepExpert.ReplaceList, False);
end;

procedure TfmGrepReplace.LoadFormSettings;
resourcestring
  SGrepNotActive = 'The Grep expert is not active';
begin
  if not Assigned(gblGrepExpert) then
    raise Exception.Create(SGrepNotActive);

  FGrepExpert := gblGrepExpert;
  cbReplace.Items.Assign(FGrepExpert.ReplaceList);
  chkUseRegEx.Checked := FGrepExpert.GrepRegEx;

  if cbReplace.Items.Count > 0 then
  begin
    cbReplace.Text := cbReplace.Items[0];
    cbReplace.SelectAll;
  end;
end;

procedure TfmGrepReplace.RetrieveSettings(var Value: TGrepSettings);
begin
  Value.Replace := cbReplace.Text;
  Value.RegEx := chkUseRegEx.Checked;
end;

procedure TfmGrepReplace.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 3);
end;

procedure TfmGrepReplace.SetSearchString(const Value: string);
begin
  lblReplaceString.Caption := Value;
end;

procedure TfmGrepReplace.SetReplaceInString(const Value: string);
begin
  lblInString.Caption := Value;
end;

end.

