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
  GX_BaseForm,
  GX_RegExpr,
  GX_StringList;

type
  TfmTestRegEx = class(TfmBaseForm)
    ed_RegEx: TEdit;
    l_RegEx: TLabel;
    l_Matches: TLabel;
    b_Ok: TButton;
    b_Cancel: TButton;
    tim_InputDelay: TTimer;
    re_Test: TRichEdit;
    chk_CaseSensitive: TCheckBox;
    procedure ed_RegExChange(Sender: TObject);
    procedure tim_InputDelayTimer(Sender: TObject);
    procedure chk_CaseSensitiveClick(Sender: TObject);
  private
    FMatchColor: TColor;
    FRegEx: TRegExpr;
    FCurrentCode: TGxUnicodeStringList;
    procedure UpdateOutput;
  public
    class function Execute(_Owner: TComponent; _MatchFont: TFont; _MatchColor: TColor;
      var _RegEx: string; var _CaseSensitive: Boolean): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  u_dzVclUtils,
  GX_OtaUtils;

{ TfmTestRegEx }

class function TfmTestRegEx.Execute(_Owner: TComponent; _MatchFont: TFont; _MatchColor: TColor;
  var _RegEx: string; var _CaseSensitive: Boolean): Boolean;
var
  frm: TfmTestRegEx;
begin
  frm := TfmTestRegEx.Create(_Owner);
  try
    frm.FMatchColor := _MatchColor;
    frm.ed_RegEx.Font.Assign(_MatchFont);
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

function GetModuleDir: string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

constructor TfmTestRegEx.Create(_Owner: TComponent);
var
  s: string;
begin
  inherited;

  FRegEx := TRegExpr.Create;

  FCurrentCode := TGxUnicodeStringList.Create;
  if not Assigned(BorlandIDEServices) or not GxOtaGetActiveEditorText(FCurrentCode, False) then begin
    begin
      s := IncludeTrailingPathDelimiter(GetModuleDir) + 'preview.pas';
      if FileExists(s) then begin
        FCurrentCode.LoadFromFile(s);
      end
    end;
  end;

  InitDpiScaler;
end;

destructor TfmTestRegEx.Destroy;
begin
  FreeAndNil(FCurrentCode);
  FreeAndNil(FRegEx);
  inherited;
end;

procedure TfmTestRegEx.ed_RegExChange(Sender: TObject);
begin
  tim_InputDelay.Enabled := False;
  tim_InputDelay.Enabled := True; //FI:W508 Variable is assigned twice successively
end;

procedure TfmTestRegEx.tim_InputDelayTimer(Sender: TObject);
begin
  UpdateOutput;
end;

procedure TfmTestRegEx.UpdateOutput;
var
  Res: Boolean;
  i: Integer;
  StartOfLine: Integer;
  LineIdx: Integer;
begin
  tim_InputDelay.Enabled := False;

  re_Test.Lines.BeginUpdate;
  try
    re_Test.Clear;
    FRegEx.ModifierI := not chk_CaseSensitive.Checked;
    FRegEx.Expression := ed_RegEx.Text;
    try
      FRegEx.Compile;
      for i := 0 to FCurrentCode.Count - 1 do begin
        Res := FRegEx.Exec(FCurrentCode[i]);
        if Res then begin
          LineIdx := re_Test.Lines.Add(FCurrentCode[i]);
          StartOfLine := re_Test.Perform(EM_LINEINDEX, LineIdx, 0);
          repeat
            re_Test.SelStart := StartOfLine + FRegEx.MatchPos[0] - 1;
            re_Test.SelLength := FRegEx.MatchLen[0];

            re_Test.SelAttributes.Color := FMatchColor;
            re_Test.SelAttributes.Style := [fsBold];
          until not FRegEx.ExecNext;
        end;
      end;
    except
      // ignore
    end; //FI:W501 Empty except block
    re_Test.SelStart := 0;
    re_Test.SelLength := 0;
  finally
    re_Test.Lines.EndUpdate;
  end;
end;

end.
