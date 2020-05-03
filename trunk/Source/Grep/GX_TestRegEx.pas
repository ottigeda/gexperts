unit GX_TestRegEx;

{$I GX_CondDefine.inc}

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
  ExtCtrls,
  ComCtrls,
  SynRegExpr,
  GX_BaseForm;

type
  TfmTestRegEx = class(TfmBaseForm)
    ed_RegEx: TEdit;
    l_RegEx: TLabel;
    l_TestText: TLabel;
    b_OK: TButton;
    b_Cancel: TButton;
    tim_InputDelay: TTimer;
    re_Test: TRichEdit;
    chk_CaseSensitive: TCheckBox;
    procedure ed_RegExChange(Sender: TObject);
    procedure tim_InputDelayTimer(Sender: TObject);
    procedure chk_CaseSensitiveClick(Sender: TObject);
  private
    FRegEx: TRegExpr;
    procedure UpdateOutput;
  public
    class function Execute(_Owner: TComponent; var _RegEx: string; var _CaseSensitive: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfmTestRegEx }

class function TfmTestRegEx.Execute(_Owner: TComponent; var _RegEx: string; var _CaseSensitive: Boolean): Boolean;
var
  frm: TfmTestRegEx;
begin
  frm := TfmTestRegEx.Create(_Owner);
  try
    frm.ed_RegEx.Text := _RegEx;
    frm.chk_CaseSensitive.Checked := _CaseSensitive;
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      _RegEx := frm.ed_RegEx.Text;
      _CaseSensitive := frm.chk_CaseSensitive.Checked;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmTestRegEx.chk_CaseSensitiveClick(Sender: TObject);
begin
  UpdateOutput;
end;

constructor TfmTestRegEx.Create(_Owner: TComponent);
begin
  inherited;

  re_Test.Lines.Add('Some text goes here.');

  FRegEx := TRegExpr.Create;
end;

destructor TfmTestRegEx.Destroy;
begin
  FreeAndNil(FRegEx);
  inherited;
end;

procedure TfmTestRegEx.ed_RegExChange(Sender: TObject);
begin
  tim_InputDelay.Enabled := False;
  tim_InputDelay.Enabled := True;
end;

procedure TfmTestRegEx.tim_InputDelayTimer(Sender: TObject);
begin
  UpdateOutput;
end;

procedure TfmTestRegEx.UpdateOutput;
var
  Res: Boolean;
  i: Integer;
  CursorPos: Integer;
  StartOfLine: Integer;
begin
  tim_InputDelay.Enabled := False;

  re_Test.Lines.BeginUpdate;
  try
    CursorPos := re_Test.SelStart;
    re_Test.SelectAll;
    re_Test.SelAttributes.Color := clBlack;
    re_Test.SelAttributes.Style := [];
    FRegEx.ModifierI := not chk_CaseSensitive.Checked;
    FRegEx.Expression := ed_RegEx.Text;
    try
      FRegEx.Compile;
      for i := 0 to re_Test.Lines.Count - 1 do begin
        StartOfLine := re_Test.Perform(EM_LINEINDEX, i, 0);
        Res := FRegEx.Exec(re_Test.Lines[i]);
        while Res do begin
          re_Test.SelStart := StartOfLine + FRegEx.MatchPos[0] - 1;
          re_Test.SelLength := FRegEx.MatchLen[0];
//          re_Test.SelAttributes.Color := clRed;
          re_Test.SelAttributes.Style := [fsUnderline];
          Res := FRegEx.ExecNext;
        end;
      end;
    except
      // ignore
    end;
    re_Test.SelStart := CursorPos;
    re_Test.SelLength := 0;
  finally
    re_Test.Lines.EndUpdate;
  end;
end;

end.
