unit GX_IdeSearchPathFavoriteEdit;

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
  StdCtrls,
  GX_BaseForm, ExtCtrls;

type
  Tf_IdeSearchPathFavoriteEdit = class(TfmBaseForm)
    l_Name: TLabel;
    ed_Name: TEdit;
    l_Path: TLabel;
    m_Path: TMemo;
    b_OK: TButton;
    b_Cancel: TButton;
    b_Select: TButton;
    imF1: TImage;
    procedure b_SelectClick(Sender: TObject);
    procedure imF1Click(Sender: TObject);
  private
    FOnCallHelpContext: TNotifyEvent;
    procedure GetData(out _Name, _Path: string);
    procedure SetData(const _Name, _Path: string; _OnCallHelpContext:TNotifyEvent=nil);
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
  public
    class function Execute(_Owner: TComponent; var _Name, _Path: string; _OnCallHelpContext:TNotifyEvent=nil): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  GX_GenericUtils;

{ Tf_IdeSearchPathFavoriteEdit }

//§B : Tf_IdeSearchPathFavoriteEdit.Execute
class function Tf_IdeSearchPathFavoriteEdit.Execute(_Owner: TComponent; var _Name, _Path: string; _OnCallHelpContext:TNotifyEvent=nil): Boolean;
var
  frm: Tf_IdeSearchPathFavoriteEdit;
begin
  frm := Tf_IdeSearchPathFavoriteEdit.Create(_Owner);
  try
    frm.SetData(_Name, _Path, _OnCallHelpContext);
    Result := (frm.ShowModal = mrok);
    if Result then
      frm.GetData(_Name, _Path);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_IdeSearchPathFavoriteEdit.b_SelectClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := '';
  if not GetDirectory('Select directory to add', Dir, Self) then
    Exit;
  m_Path.Lines.Add(Dir);
end;

constructor Tf_IdeSearchPathFavoriteEdit.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  m_Path.Lines.Delimiter := ';';
  TWinControl_ActivateDropFiles(m_Path, HandleFilesDropped);

  InitDpiScaler;
end;

procedure Tf_IdeSearchPathFavoriteEdit.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  m_Path.Lines.AddStrings(_Files);
end;

procedure Tf_IdeSearchPathFavoriteEdit.GetData(out _Name, _Path: string);
begin
  _Name := ed_Name.Text;
  _Path := AnsiDequotedStr(m_Path.Lines.DelimitedText, '"');
end;

procedure Tf_IdeSearchPathFavoriteEdit.SetData(const _Name, _Path: string; _OnCallHelpContext: TNotifyEvent=nil);
begin
  ed_Name.Text := _Name;
  m_Path.Lines.DelimitedText := AnsiDequotedStr(_Path, '"');
  FOnCallHelpContext := _OnCallHelpContext;

  if not(Assigned(_OnCallHelpContext)) then b_Select.Left := m_Path.Left;
  // imF1.Enabled := Assigned(_OnCallHelpContext);
end;

procedure Tf_IdeSearchPathFavoriteEdit.imF1Click(Sender: TObject);
begin
  inherited;
  FOnCallHelpContext(Self);
end;

end.
