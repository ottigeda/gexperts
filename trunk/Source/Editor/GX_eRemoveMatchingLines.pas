unit GX_eRemoveMatchingLines;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_BaseForm;

type
  ///<summary>
  /// This editor expert removes lines from the current editor that consist only
  /// of the configured text, optionally padded with spaces, or alternatively those
  /// which match a set of regular expressions.
  /// e.g. those pesky comments the IDE adds automatically
  ///    TForm1 = class(TForm)
  ///    private
  ///    { Private declarations }
  ///  public
  ///    { Public declarations }
  ///  end;
  TRemoveMatchingLinesExpert = class(TEditorExpert)
  private
    FMatches: TStringList;
    FRegEx: Boolean;
  public
    constructor Create; override;
    // optional if HasConfigOptions returns false
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    // optional, but recommended
    function GetHelpString: string; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  end;

type
  TfmRemoveMatchingLinesExpertConfig = class(TfmBaseForm)
    m_Lines: TMemo;
    b_Ok: TButton;
    b_Cancel: TButton;
    l_Lines: TLabel;
    b_Defaults: TButton;
    chk_RegEx: TCheckBox;
    procedure b_DefaultsClick(Sender: TObject);
  private
    class procedure SetDefault(_Matches: TStrings; _RegEx: Boolean);
    procedure SetData(_Matches: TStrings; _RegEx: Boolean);
    procedure GetData(_Matches: TStrings; out _RegEx: Boolean);
  public
    constructor Create(_Owner: TComponent); override;
    class function Execute(_Owner: TComponent; _Matches: TStrings; var _RegEx: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus,
  Registry,
  SynRegExpr,
  u_dzClassUtils,
  u_dzVclUtils,
  GX_OtaUtils,
  GX_StringList;

{ TRemoveMatchingLinesExpert }

procedure TRemoveMatchingLinesExpert.Configure;
begin
  if TfmRemoveMatchingLinesExpertConfig.Execute(nil, FMatches, FRegEx) then
    SaveSettings;
end;

constructor TRemoveMatchingLinesExpert.Create;
begin
  inherited Create;

  FMatches := TStringList.Create;

  FRegEx := False;
  TfmRemoveMatchingLinesExpertConfig.SetDefault(FMatches, FRegEx);

  // LoadSettings is called automatically for the expert upon creation.
end;

type
  TAbstractMatcher = class
  protected
    FMatches: TStringList;
    function LineMatches(const _Line: string): Boolean; virtual; abstract;
  public
    constructor Create(_Matches: TStrings);
    destructor Destroy; override;
  end;

{ TAbstractMatcher }

constructor TAbstractMatcher.Create(_Matches: TStrings);
begin
  inherited Create;
  FMatches := TStringList.Create;
  FMatches.Sorted := True;
  FMatches.Duplicates := dupIgnore;
  FMatches.AddStrings(_Matches);
end;

type
  TCompareMatcher = class(TAbstractMatcher)
  protected
    function LineMatches(const _Line: string): Boolean; override;
  end;

destructor TAbstractMatcher.Destroy;
begin
  FreeAndNil(FMatches);
  inherited;
end;

{ TCompareMatcher }

function TCompareMatcher.LineMatches(const _Line: string): Boolean;
var
  s: string;
  Idx: Integer;
begin
  s := Trim(_Line);
  Result := FMatches.Find(s, Idx);
end;

type
  TRegExprMatcher = class(TAbstractMatcher)
  protected
    function LineMatches(const _Line: string): Boolean; override;
  public
    constructor Create(_Matches: TStrings);
    destructor Destroy; override;
  end;

{ TRegExprMatcher }

constructor TRegExprMatcher.Create(_Matches: TStrings);
var
  i: Integer;
  re: TRegExpr;
begin
  inherited Create(_Matches);
  for i := 0 to FMatches.Count - 1 do begin
    re := TRegExpr.Create;
    re.Expression := FMatches[i];
    re.Compile;
    FMatches.Objects[i] := re;
  end;
end;

destructor TRegExprMatcher.Destroy;
begin
  TStrings_FreeAllObjects(FMatches);
  inherited;
end;

function TRegExprMatcher.LineMatches(const _Line: string): Boolean;
var
  re: TRegExpr;
  i: Integer;
  Len: Integer;
begin
  Len := Length(_Line);
  Result := False;
  for i := 0 to FMatches.Count - 1 do begin
    re := TRegExpr(FMatches.Objects[i]);
    Result := re.Exec(_Line);
    if Result then begin
      Result := (re.MatchPos[0] = 1) and (re.MatchLen[0] = Len);
    end;
    if Result then
      Exit; //==>
  end;
end;

{ TRemoveMatchingLinesExpert }

procedure TRemoveMatchingLinesExpert.Execute(Sender: TObject);
var
  Text: TGXUnicodeStringList;
  LineIdx: Integer;
  s: TGXUnicodeString;
  WasChanged: Boolean;
  Matcher: TAbstractMatcher;
begin
  Text := TGXUnicodeStringList.Create;
  try
    if not GxOtaGetActiveEditorText(Text, False) then
      Exit; //==>

    if FRegEx then
      Matcher := TRegExprMatcher.Create(FMatches)
    else
      Matcher := TCompareMatcher.Create(FMatches);

    WasChanged := False;
    for LineIdx := Text.Count - 1 downto 0 do begin
      s := Text[LineIdx];
      if Matcher.LineMatches(s) then begin
        Text.Delete(LineIdx);
        WasChanged := True;
      end;
    end;
    if WasChanged then
      GxOtaReplaceEditorTextWithUnicodeString(GxOtaGetCurrentSourceEditor, Text.Text);
  finally
    FreeAndNil(Text);
  end;
  IncCallCount;
end;

function TRemoveMatchingLinesExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Remove Matching Lines';
begin
  Result := SDisplayName;
end;

function TRemoveMatchingLinesExpert.GetHelpString: string;
resourcestring
  SRemoveLinesEditorExpertHelp = 'Removes lines from the current editor that consist only '
    + 'of the configured text, optionally padded with spaces, or those matching as set of '
    + 'regular expressions. '
    + 'Use case: Remove those stupid comments like { Private delcarations } the IDE adds automatically.';
begin
  Result := SRemoveLinesEditorExpertHelp;
end;

procedure TRemoveMatchingLinesExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);

  FMatches.Clear;
  _Settings.ReadStrings('Matches', FMatches);
  FRegEx := _Settings.ReadBool('RegEx', False);
  if FMatches.Count = 0 then
    TfmRemoveMatchingLinesExpertConfig.SetDefault(FMatches, FRegEx);
end;

procedure TRemoveMatchingLinesExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);

  _Settings.WriteStrings('Matches', FMatches);
  _Settings.WriteBool('RegEx', FRegEx);
end;

class procedure TfmRemoveMatchingLinesExpertConfig.SetDefault(_Matches: TStrings; _RegEx: Boolean);
begin
  _Matches.Clear;

  if _RegEx then begin
    // English
    _Matches.Add('\s*\{ (Private|Protected|Public|Published) declarations \}');
    // German
    _Matches.Add('\s*\{ (Private|Protected|Public|Published)-Deklarationen \}');
    // French
    _Matches.Add('\s*\{ D�clarations (priv�es|prot�g�es|publiques|publi�es) \}');
  end else begin
    // English
    _Matches.Add('{ Private declarations }');
    _Matches.Add('{ Protected declarations }');
    _Matches.Add('{ Public declarations }');
    _Matches.Add('{ Published declarations }');

    // French
    _Matches.Add('{ D�clarations priv�es }');
    _Matches.Add('{ D�clarations prot�g�es }');
    _Matches.Add('{ D�clarations publiques }');
    _Matches.Add('{ D�clarations publi�es }');

    // German
    _Matches.Add('{ Private-Deklarationen }');
    _Matches.Add('{ Protected-Deklarationen }');
    _Matches.Add('{ Public-Deklarationen }');
    _Matches.Add('{ Published-Deklarationen }');

    // todo: Japanese
    // (requires Ansi CodePage 932)
    //  _Matches.Add('{ Private ?? }');
    //  _Matches.Add('{ Protected ?? }');
    //  _Matches.Add('{ Public ?? }');
    //  _Matches.Add('{ Published ?? }');
    // The Ansi codes of the two characters are: 90 E9 8C BE
    // The UTF-8 codes are: E5 AE A3 E8 A
    // (from Uwe Raabe's answer to my question on Google+ )
    // https://plus.google.com/+ThomasMueller/posts/cYvgUAhdZUS
  end;
end;

class function TfmRemoveMatchingLinesExpertConfig.Execute(_Owner: TComponent; _Matches: TStrings;
  var _RegEx: Boolean): Boolean;
var
  frm: TfmRemoveMatchingLinesExpertConfig;
begin
  frm := TfmRemoveMatchingLinesExpertConfig.Create(nil);
  try
    frm.SetData(_Matches, _RegEx);
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      frm.GetData(_Matches, _RegEx);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmRemoveMatchingLinesExpertConfig.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);

  GxOtaGetEditorFont(m_Lines.Font);
end;

procedure TfmRemoveMatchingLinesExpertConfig.b_DefaultsClick(Sender: TObject);
begin
  if m_Lines.Lines.Count > 0 then
    if mrYes <> MessageDlg('Do you really want to overwrite the existing lines?', mtConfirmation, [mbYes, mbCancel], 0) then
      Exit; //==>

  SetDefault(m_Lines.Lines, chk_RegEx.Checked);
end;

procedure TfmRemoveMatchingLinesExpertConfig.GetData(_Matches: TStrings; out _RegEx: Boolean);
var
  i: Integer;
  s: string;
begin
  _Matches.Clear;
  for i := 0 to m_Lines.Lines.Count - 1 do begin
    s := Trim(m_Lines.Lines[i]);
    if s <> '' then
      _Matches.Add(s);
  end;
  _RegEx := chk_RegEx.Checked;
end;

procedure TfmRemoveMatchingLinesExpertConfig.SetData(_Matches: TStrings; _RegEx: Boolean);
begin
  m_Lines.Lines.Assign(_Matches);
  chk_RegEx.Checked := _RegEx;
end;

initialization
  RegisterEditorExpert(TRemoveMatchingLinesExpert);
end.
