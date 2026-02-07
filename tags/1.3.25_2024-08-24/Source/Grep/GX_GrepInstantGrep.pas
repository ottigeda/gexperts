unit GX_GrepInstantGrep;

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
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Buttons,
  ActnList,
  Actions,
  ToolsAPI,
  regexpr,
  GX_EventHook,
  GX_StringList,
  GX_SharedImages,
  GX_IdeDock;

type
  TfmGxInstantGrepForm = class(TfmIdeDockForm)
    TheActionList: TActionList;
    act_Refresh: TAction;
    act_Clear: TAction;
    l_RegEx: TLabel;
    ed_RegEx: TEdit;
    tim_InputDelay: TTimer;
    chk_CaseSensitive: TCheckBox;
    p_Results: TPanel;
    lb_Results: TListBox;
    l_PressEsc: TLabel;
    tim_EditorChanged: TTimer;
    sb_Refresh: TSpeedButton;
    sb_Clear: TSpeedButton;
    procedure tim_InputDelayTimer(Sender: TObject);
    procedure ed_RegExChange(Sender: TObject);
    procedure lb_ResultsDrawItem(_Control: TWinControl; _Index: Integer; _Rect: TRect;
      _State: TOwnerDrawState);
    procedure ed_RegExKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lb_ResultsDblClick(Sender: TObject);
    procedure lb_ResultsClick(Sender: TObject);
    procedure lb_ResultsKeyPress(Sender: TObject; var Key: Char);
    procedure ed_RegExKeyPress(Sender: TObject; var Key: Char);
    procedure tim_EditorChangedTimer(Sender: TObject);
    procedure chk_CaseSensitiveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure act_RefreshExecute(Sender: TObject);
    procedure act_ClearExecute(Sender: TObject);
  private
    FRegEx: TRegExpr;
    FCurrentCode: TGXUnicodeStringList;
    FCurrentFile: string;
    FOriginalEditPos: TOTAEditPos;
    procedure Clear;
    procedure UpdateOutput;
    function TryGetSelectedEditorPos(out _EditPos: TOTAEditPos): Boolean;
    procedure GoBack();
    procedure GoToCurrent;
    procedure SetListboxItemHeight;
    procedure LoadCurrentCode;
  protected
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); override;
{$ENDIF}
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  Math,
  u_dzVclUtils,
  u_dzClassUtils,
  GX_OtaUtils,
  GX_Experts,
  GX_GrepMenuEntry,
  GX_GenericUtils,
  GX_NTAEditServiceNotifier,
  GX_GExperts;

var
  fmGxInstantGrepForm: TfmGxInstantGrepForm = nil;

type
  TFileResult = class
  private
    Idx: Integer;
    MatchStart: Integer;
    MatchLen: Integer;
  public
    constructor Create(_Index, _Start, _Len: Integer);
  end;

{ TFileResult }

constructor TFileResult.Create(_Index, _Start, _Len: Integer);
begin
  inherited Create;
  Idx := _Index;
  MatchStart := _Start;
  MatchLen := _Len;
end;

{ TfmGxInstantGrepForm }

constructor TfmGxInstantGrepForm.Create(_Owner: TComponent);
begin
  inherited;

  p_Results.BevelOuter := bvNone;

  TControl_SetMinConstraints(Self);

  FRegEx := TRegExpr.Create;
  FCurrentCode := TGXUnicodeStringList.Create;

  LoadCurrentCode;
  SetListboxItemHeight;

  InitDpiScaler;
end;

destructor TfmGxInstantGrepForm.Destroy;
begin
  TStrings_FreeAllObjects(lb_Results.Items);
  FreeAndNil(FCurrentCode);
  inherited;
  fmGxInstantGrepForm := nil;
end;

procedure TfmGxInstantGrepForm.act_ClearExecute(Sender: TObject);
begin
  Clear;
end;

procedure TfmGxInstantGrepForm.act_RefreshExecute(Sender: TObject);
begin
  UpdateOutput;
end;

procedure TfmGxInstantGrepForm.Clear;
var
  Items: TStrings;
begin
  // clear results and RegEx phrase
  tim_EditorChanged.Enabled := False;
  tim_InputDelay.Enabled := False;
  ed_RegEx.Text := '';

  Items := lb_Results.Items;
  Items.BeginUpdate;
  try
    TStrings_FreeAllObjects(Items);
    Items.Clear;
  finally
    Items.EndUpdate;
  end;
end;

procedure TfmGxInstantGrepForm.LoadCurrentCode;

  function GetModuleDir: string;
  begin
    Result := ExtractFilePath(GetModuleName(HInstance));
  end;

resourcestring
  SCaption = '%s - Instant Grep';
var
  ISourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
begin
  if Assigned(BorlandIDEServices)
    and GxOtaTryGetCurrentSourceEditor(ISourceEditor)
    and GxOtaGetActiveEditorText(FCurrentCode, False) then begin
    FCurrentFile := ISourceEditor.FileName;
    if GxOtaTryGetTopMostEditView(ISourceEditor, EditView) then begin
      FOriginalEditPos := EditView.CursorPos;
    end else begin
      FOriginalEditPos.Col := -1;
      FOriginalEditPos.Line := -1;
    end;
  end else begin
    FCurrentFile := IncludeTrailingPathDelimiter(GetModuleDir) + 'preview.pas';
    if FileExists(FCurrentFile) then begin
      FCurrentCode.LoadFromFile(FCurrentFile);
    end;
    FOriginalEditPos.Col := -1;
    FOriginalEditPos.Line := -1;
  end;

  // modify caption to show current module name
  Caption := Format(SCaption, [ExtractFileName(FCurrentFile)]);
end;

procedure TfmGxInstantGrepForm.GoBack();
begin
  if FOriginalEditPos.Col >= 0 then begin
    GxOtaTryGotoEditPos(FOriginalEditPos);
  end;
  GxOtaFocusCurrentIDEEditControl;
end;

procedure TfmGxInstantGrepForm.GoToCurrent();
var
  EditPos: TOTAEditPos;
begin
  if TryGetSelectedEditorPos(EditPos) then begin
    GxOtaTryGotoEditPos(EditPos);
  end;
  GxOtaFocusCurrentIDEEditControl;
end;

function TfmGxInstantGrepForm.TryGetSelectedEditorPos(out _EditPos: TOTAEditPos): Boolean;
var
  Idx: Integer;
  Items: TStrings;
  Res: TFileResult;
begin
  Result := False;

  Idx := lb_Results.ItemIndex;
  Items := lb_Results.Items;
  if (Idx < 0) or (Idx >= Items.Count) then
    Exit; //==>

  Res := TFileResult(Items.Objects[Idx]);
  if not Assigned(Res) then
    Exit;

  _EditPos.Col := Res.MatchStart;
  _EditPos.Line := Res.Idx + 1;
  Result := True;
end;

procedure TfmGxInstantGrepForm.chk_CaseSensitiveClick(Sender: TObject);
begin
  inherited;
  tim_InputDelay.Enabled := False;
  UpdateOutput;
end;

procedure TfmGxInstantGrepForm.lb_ResultsClick(Sender: TObject);
var
  EditPos: TOTAEditPos;
begin
  if TryGetSelectedEditorPos(EditPos) then
    GxOtaTryGotoEditPos(EditPos);
end;

procedure TfmGxInstantGrepForm.lb_ResultsDblClick(Sender: TObject);
begin
  GoToCurrent();
end;

procedure TfmGxInstantGrepForm.lb_ResultsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    GoBack()
  else if Key = #13 then
    GoToCurrent();
end;

procedure TfmGxInstantGrepForm.lb_ResultsDrawItem(_Control: TWinControl; _Index: Integer; _Rect: TRect;
  _State: TOwnerDrawState);
resourcestring
  SLine = 'Line %d';
const
  TopOffset = 1;
var
  ListBox: TListBox absolute _Control;
  LbCanvas: TCanvas;
  Module: string;
  Res: TFileResult;
  LineHeight: Integer;

  procedure PaintFrame(_Rect: TRect);
  begin
    // all but the very first line should draw the upperframe line one pixel above to avoid thick horizontal lines
    if _Index > 0 then
      _Rect.Top := _Rect.Top - 1;

    LbCanvas.Brush.Color := clBtnShadow;
    LbCanvas.FrameRect(_Rect);
  end;

  procedure PaintGutter(_Rect: TRect);
  var
    i: Integer;
    Line: string;
    TextTop: Integer;
  begin
    LbCanvas.Brush.Color := clBtnFace;
    LbCanvas.FillRect(_Rect);

    TextTop := _Rect.Top + TopOffset;
    for i := -1 to 1 do begin
      if i = 0 then
        LbCanvas.Font.Color := clBtnText
      else
        LbCanvas.Font.Color := clGrayText;
      Line := Format('%d', [Res.Idx + 1 + i]);
      LbCanvas.TextOut(_Rect.Right - 1 - LbCanvas.TextWidth(Line) - FScaler.Calc(5), TextTop, Line);
      Inc(TextTop, LineHeight);
    end;

    LbCanvas.Pen.Color := clBtnShadow;
    LbCanvas.MoveTo(_Rect.Right - 1, _Rect.Top);
    LbCanvas.LineTo(_Rect.Right - 1, _Rect.Bottom);
  end;

  procedure PaintLines(_Rect: TRect);
  var
    TextTop: Integer;
    i: Integer;
    LineText: string;
    BGNormal: TColor;
    BGMatch: TColor;
    Idx: Integer;
  begin
    if odSelected in _State then begin
      BGNormal := clHighLight;
      LbCanvas.Font.Color := clHighLightText;
      BGMatch := BGNormal;
    end else if _Index = 0 then begin
      BGNormal := clBtnFace;
      LbCanvas.Font.Color := clWindowText;
      BGMatch := BGNormal;
    end else begin
      BGNormal := clWindow;
      LbCanvas.Font.Color := clWindowText;
      BGMatch := RGB(250, 255, 230);
    end;
    LbCanvas.Brush.Color := BGNormal;
    LbCanvas.FillRect(_Rect);

    TextTop := _Rect.Top + TopOffset;
    for i := -1 to 1 do begin
      Idx := Res.Idx + i;
      if (Idx >= 0) and (Idx < FCurrentCode.Count) then
        LineText := FCurrentCode[Idx]
      else
        LineText := '';
      if i = 0 then begin
        // line containing the match
        LbCanvas.Brush.Color := BGMatch;
        LbCanvas.FillRect(Rect(_Rect.Left, TextTop, _Rect.Right, TextTop + LineHeight));
      end else begin
        // line above or below the match
        LbCanvas.Brush.Color := BGNormal;
      end;

      LbCanvas.TextOut(_Rect.Left + 1, TextTop, LineText);

      Inc(TextTop, LineHeight);
    end;
  end;

var
  Items: TStrings;
  GutterWidth: Integer;
begin // TfmFastGrep.lb_ResultsDrawItem
  Items := ListBox.Items;
  Res := TFileResult(Items.Objects[_Index]);
  if Assigned(Res) then begin
    Module := Items[_Index];
    LbCanvas := ListBox.Canvas;
    LbCanvas.Font := ListBox.Font;
    LineHeight := LbCanvas.TextHeight('Mg');
    GutterWidth := LbCanvas.TextWidth(DupeString('9', Length(Format('%d', [FCurrentCode.Count])))) + FScaler.Calc(10);

    // draw surrounding frame
    PaintFrame(_Rect);
    PaintGutter(Rect(_Rect.Left + 1, _Rect.Top + 1, _Rect.Left + GutterWidth + 1, _Rect.Bottom - 1));
    PaintLines(Rect(_Rect.Left + GutterWidth + 2, _Rect.Top + 1, _Rect.Right - 1, _Rect.Bottom - 1));
  end;
end;

procedure TfmGxInstantGrepForm.SetListboxItemHeight;
var
  cnv: TCanvas;
begin
  cnv := lb_Results.Canvas;
  cnv.Font := lb_Results.Font;
  lb_Results.ItemHeight := (cnv.TextHeight('Mg') + 2) * 3;
end;

procedure TfmGxInstantGrepForm.ed_RegExChange(Sender: TObject);
begin
  TTimer_Restart(tim_InputDelay);
end;

procedure TfmGxInstantGrepForm.ed_RegExKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) and (Shift = []) then begin
    SendMessage(lb_Results.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmGxInstantGrepForm.ed_RegExKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    GoBack()
  else if Key = #13 then
    GoToCurrent();
end;

procedure TfmGxInstantGrepForm.FormActivate(Sender: TObject);
begin
  inherited;
  l_RegEx.Caption := 'Regular &Expression';
  chk_CaseSensitive.Caption := '&Case sensitive';
end;

procedure TfmGxInstantGrepForm.FormDeactivate(Sender: TObject);
begin
  inherited;
  l_RegEx.Caption := 'Regular Expression';
  chk_CaseSensitive.Caption := 'Case sensitive';
end;

procedure TfmGxInstantGrepForm.tim_EditorChangedTimer(Sender: TObject);
begin
  tim_EditorChanged.Enabled := False;
  LoadCurrentCode;
  UpdateOutput;
end;

procedure TfmGxInstantGrepForm.tim_InputDelayTimer(Sender: TObject);
begin
  tim_InputDelay.Enabled := False;
  UpdateOutput;
end;

procedure TfmGxInstantGrepForm.UpdateOutput;
var
  Res: Boolean;
  i: Integer;
  Items: TStrings;
  RegEx: string;
begin
  RegEx := ed_RegEx.Text;
  if RegEx = '' then
    Exit; //==>

  Items := lb_Results.Items;
  Items.BeginUpdate;
  try
    FRegEx.ModifierI := not chk_CaseSensitive.Checked;
    FRegEx.Expression := RegEx;
    try
      TStrings_FreeAllObjects(Items);
      Items.Clear;
      FRegEx.Compile;
      for i := 0 to FCurrentCode.Count - 1 do begin
        Res := FRegEx.Exec(FCurrentCode[i]);
        if Res then begin
          Items.AddObject(FCurrentFile, TFileResult.Create(i, FRegEx.MatchPos[0], FRegEx.MatchLen[0]));
        end;
      end;
      if Items.Count > 0 then begin
        Items.InsertObject(0, 'Original Position', TFileResult.Create(FOriginalEditPos.Line - 1, 0, 0));
        TListBox_SetItemIndex(lb_Results, 0, False);
      end else
        lb_Results.ItemIndex := -1;
    except
      // ignore
    end; //FI:W501 Empty except block
  finally
    Items.EndUpdate;
  end;
end;

{$IFDEF GX_IDE_IS_HIDPI_AWARE}
procedure TfmGxInstantGrepForm.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
var
  il: TImageList;
begin
  inherited;
  il := GExpertsInst.GetScaledSharedImages(_NewDpi);
  TheActionList.Images := il;
end;

procedure TfmGxInstantGrepForm.ArrangeControls;
begin
  inherited;
  SetListboxItemHeight;
end;
{$ENDIF}

type
  TGrepInstantSearchExpert = class(TGX_Expert)
  private
{$IFDEF GX_VER170_up}
    FNotifierIdx: Integer;
    procedure EditorViewActivated(_Sender: TObject; _EditView: IOTAEditView);
{$ENDIF}
  protected
    procedure SetActive(New: Boolean); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    // optional, but recommended
    function GetHelpString: string; override;
    // optional, defaults to true
    function HasConfigOptions: Boolean; override;
//    // optional if HasConfigOptions returns false
//    procedure Configure; override;
//    // Override to load any configuration settings
//    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
//    // Override to save any configuration settings
//    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure Execute(Sender: TObject); override;
  end;

{ TGrepInstantSearchExpert }

constructor TGrepInstantSearchExpert.Create;
begin
  inherited;

  // SetFormIcon(fmGxInstantGrepForm);
  IdeDockManager.RegisterDockableForm(TfmGxInstantGrepForm, fmGxInstantGrepForm, 'fmGxInstantGrepForm');

{$IFDEF GX_VER170_up}
  if Assigned(BorlandIDEServices) then begin
    FNotifierIdx := (BorlandIDEServices as IOTAEditorServices).AddNotifier(
      TGxNTAEditServiceNotifierActivate.Create(EditorViewActivated));
  end;
{$ENDIF}
end;

destructor TGrepInstantSearchExpert.Destroy;
begin
{$IFDEF GX_VER170_up}
  if FNotifierIdx <> 0 then
    (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FNotifierIdx);
{$ENDIF}

  // inherited Destroy sets Active to false which frees the form
  inherited;
end;

{$IFDEF GX_VER170_up}
procedure TGrepInstantSearchExpert.EditorViewActivated(_Sender: TObject; _EditView: IOTAEditView);
begin
  if Assigned(fmGxInstantGrepForm) then begin
    TTimer_Restart(fmGxInstantGrepForm.tim_EditorChanged);
  end;
end;
{$ENDIF}

procedure TGrepInstantSearchExpert.Execute(Sender: TObject);
begin
  if not Assigned(fmGxInstantGrepForm) then begin
    fmGxInstantGrepForm := TfmGxInstantGrepForm.Create(Application);
    SetFormIcon(fmGxInstantGrepForm);
  end;
  IdeDockManager.ShowForm(fmGxInstantGrepForm);
  EnsureFormVisible(fmGxInstantGrepForm);
  IncCallCount;
end;

function TGrepInstantSearchExpert.GetActionCaption: string;
begin
  Result := 'Instant Grep';
end;

function TGrepInstantSearchExpert.GetHelpString: string;
begin
  Result := 'Does a Grep search for the current editor file and gives a list of matches.'#13#10
    + 'Which can then be selected to position the cursor.';
end;

class function TGrepInstantSearchExpert.GetName: string;
begin
  Result := GrepFastSearchName;
end;

function TGrepInstantSearchExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TGrepInstantSearchExpert.SetActive(New: Boolean);
begin
  if New <> Active then begin
    inherited SetActive(New);
    if New then begin
      IdeDockManager.RegisterDockableForm(TfmGxInstantGrepForm, fmGxInstantGrepForm, 'fmGxInstantGrepForm');
    end else begin
      IdeDockManager.UnRegisterDockableForm(fmGxInstantGrepForm, 'fmGxInstantGrepForm');
      FreeAndNil(fmGxInstantGrepForm);
    end;
  end;
end;

initialization
  RegisterGX_Expert(TGrepInstantSearchExpert);
end.

