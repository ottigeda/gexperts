unit GX_GrepFastGrep;

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
  ToolsAPI,
  regexpr,
  GX_EventHook,
  GX_StringList,
  GX_IdeDock;

type
  TfmFastGrep = class(TfmIdeDockForm)
    l_RegEx: TLabel;
    ed_RegEx: TEdit;
    tim_InputDelay: TTimer;
    chk_CaseSensitive: TCheckBox;
    lb_Results: TListBox;
    procedure tim_InputDelayTimer(Sender: TObject);
    procedure ed_RegExChange(Sender: TObject);
    procedure lb_ResultsDrawItem(_Control: TWinControl; _Index: Integer; _Rect: TRect;
      _State: TOwnerDrawState);
    procedure ed_RegExKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lb_ResultsDblClick(Sender: TObject);
    procedure lb_ResultsClick(Sender: TObject);
    procedure lb_ResultsKeyPress(Sender: TObject; var Key: Char);
    procedure ed_RegExKeyPress(Sender: TObject; var Key: Char);
  private
    FRegEx: TRegExpr;
    FCurrentCode: TGXUnicodeStringList;
    FCurrentFile: string;
    FOriginalEditPos: TOTAEditPos;
    procedure UpdateOutput;
    function TryGetSelectedEditorPos(out _EditPos: TOTAEditPos): Boolean;
    procedure GoBackAndClose();
    procedure GoToCurrentAndClose;
    procedure SetListboxItemHeight;
  protected
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
    procedure ArrangeControls; override;
{$ENDIF}
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  u_dzClassUtils,
  GX_OtaUtils,
  GX_Experts,
  GX_GrepMenuEntry;

var
  fmFastGrep: TfmFastGrep = nil;

type
  TFileResult = class
  private
    Module: string;
    Idx: Integer;
    MatchStart: Integer;
    MatchLen: Integer;
  public
    constructor Create(const _Module: string; _Index, _Start, _Len: Integer);
  end;

{ TFileResult }

constructor TFileResult.Create(const _Module: string; _Index, _Start, _Len: Integer);
begin
  inherited Create;
  Module := _Module;
  Idx := _Index;
  MatchStart := _Start;
  MatchLen := _Len;
end;

{ TfmFastGrep }

constructor TfmFastGrep.Create(_Owner: TComponent);

  function GetModuleDir: string;
  begin
    Result := ExtractFilePath(GetModuleName(HInstance));
  end;

var
  ISourceEditor: IOTASourceEditor;
begin
  inherited;

  FRegEx := TRegExpr.Create;
  FCurrentCode := TGXUnicodeStringList.Create;

  if Assigned(BorlandIDEServices)
    and GxOtaTryGetCurrentSourceEditor(ISourceEditor)
    and GxOtaGetActiveEditorText(FCurrentCode, False) then begin
    FCurrentFile := ISourceEditor.FileName;
    FOriginalEditPos := GxOtaGetTopMostEditView.CursorPos;
  end else begin
    FCurrentFile := IncludeTrailingPathDelimiter(GetModuleDir) + 'preview.pas';
    if FileExists(FCurrentFile) then begin
      FCurrentCode.LoadFromFile(FCurrentFile);
    end;
    FOriginalEditPos.Col := -1;
    FOriginalEditPos.Line := -1;
  end;
  SetListboxItemHeight;

  InitDpiScaler;
end;

destructor TfmFastGrep.Destroy;
begin
  TStrings_FreeAllObjects(lb_Results.Items);
  FreeAndNil(FCurrentCode);
  inherited;
end;

procedure TfmFastGrep.GoBackAndClose();
begin
  if FOriginalEditPos.Col >= 0 then begin
    GxOtaGotoEditPos(FOriginalEditPos);
  end;
  GxOtaFocusCurrentIDEEditControl;
  fmFastGrep := nil;
  Release;
end;

procedure TfmFastGrep.GoToCurrentAndClose();
var
  EditPos: TOTAEditPos;
begin
  if TryGetSelectedEditorPos(EditPos) then begin
    GxOtaGotoEditPos(EditPos);
  end;
  GxOtaFocusCurrentIDEEditControl;
  fmFastGrep := nil;
  Release;
end;

function TfmFastGrep.TryGetSelectedEditorPos(out _EditPos: TOTAEditPos): Boolean;
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

procedure TfmFastGrep.lb_ResultsClick(Sender: TObject);
var
  EditPos: TOTAEditPos;
begin
  if TryGetSelectedEditorPos(EditPos) then
    GxOtaGotoEditPos(EditPos);
end;

procedure TfmFastGrep.lb_ResultsDblClick(Sender: TObject);
begin
  GoToCurrentAndClose();
end;

procedure TfmFastGrep.lb_ResultsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    GoBackAndClose()
  else if Key = #13 then
    GoToCurrentAndClose();
end;

procedure TfmFastGrep.lb_ResultsDrawItem(_Control: TWinControl; _Index: Integer; _Rect: TRect;
  _State: TOwnerDrawState);
resourcestring
  SLine = 'Line %d';
const
  TopOffset = 1;
var
  ListBox: TListBox absolute _Control;
  LbCanvas: TCanvas;
  Res: TFileResult;
  LineHeight: Integer;

  procedure PaintFileHeader(_Rect: TRect);
  var
    TextTop: Integer;
    TopColor: TColor;
    BottomColor: TColor;
    i: Integer;
    FileNameWidth: Integer;
    FileString: string;
    LineText: string;
  begin
    TextTop := _Rect.Top + TopOffset;
    TopColor := clBtnHighlight;
    BottomColor := clBtnShadow;

    LbCanvas.Brush.Color := clBtnFace;
    LbCanvas.Font.Color := clBtnText;
    LbCanvas.FillRect(_Rect);

    _Rect.Right := _Rect.Right + 2;
    if odSelected in _State then
      Frame3D(LbCanvas, _Rect, BottomColor, TopColor, 1)
    else
      Frame3D(LbCanvas, _Rect, TopColor, BottomColor, 1);

    i := LbCanvas.TextWidth('00');
    FileString := ExtractFileName(Res.Module);
    LbCanvas.TextOut(_Rect.Left, TextTop, FileString);
//    LbCanvas.TextOut(_Rect.Left + i + FScaler.Calc(8), TextTop, FileString);

    LineText := Format(SLine, [Res.Idx + 1]);

    FileNameWidth := LbCanvas.TextWidth(LineText) + FScaler.Calc(10);
    if (LbCanvas.TextWidth(FileString) + i + FScaler.Calc(10)) <= _Rect.Right - FileNameWidth then
      LbCanvas.TextOut(ListBox.ClientWidth - FileNameWidth, TextTop, LineText);
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
        // we already filled the whole area before the for loop
      end;
      LbCanvas.TextOut(_Rect.Left, TextTop, LineText);
      Inc(TextTop, LineHeight);
    end;
  end;

begin // TfmFastGrep.lb_ResultsDrawItem
  Res := TFileResult(ListBox.Items.Objects[_Index]);
  if Assigned(Res) then begin
    LbCanvas := ListBox.Canvas;
    LbCanvas.Font := ListBox.Font;
    LineHeight := LbCanvas.TextHeight('Mg');
    PaintFileHeader(Rect(_Rect.Left, _Rect.Top, _Rect.Right, _Rect.Top + LineHeight + 2));
    PaintLines(Rect(_Rect.Left, _Rect.Top + LineHeight + 2, _Rect.Right, _Rect.Bottom));
  end;
end;

procedure TfmFastGrep.SetListboxItemHeight;
var
  cnv: TCanvas;
begin
  cnv := lb_Results.Canvas;
  cnv.Font := lb_Results.Font;
  lb_Results.ItemHeight := (cnv.TextHeight('Mg') + 2) * 4;
end;

procedure TfmFastGrep.ed_RegExChange(Sender: TObject);
begin
  TTimer_Restart(tim_InputDelay);
end;

procedure TfmFastGrep.ed_RegExKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) and (Shift = []) then begin
    SendMessage(lb_Results.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmFastGrep.ed_RegExKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    GoBackAndClose()
  else if Key = #13 then
    GoToCurrentAndClose();
end;

procedure TfmFastGrep.tim_InputDelayTimer(Sender: TObject);
begin
  UpdateOutput;
  tim_InputDelay.Enabled := False;
end;

procedure TfmFastGrep.UpdateOutput;
var
  Res: Boolean;
  i: Integer;
  Items: TStrings;
begin
  Items := lb_Results.Items;
  Items.BeginUpdate;
  try
    FRegEx.ModifierI := not chk_CaseSensitive.Checked;
    FRegEx.Expression := ed_RegEx.Text;
    try
      TStrings_FreeAllObjects(Items);
      Items.Clear;
      FRegEx.Compile;
      for i := 0 to FCurrentCode.Count - 1 do begin
        Res := FRegEx.Exec(FCurrentCode[i]);
        if Res then begin
          Items.AddObject(FCurrentCode[i], TFileResult.Create(FCurrentFile, i, FRegEx.MatchPos[0], FRegEx.MatchLen[0]));
        end;
      end;
      if Items.Count > 0 then
        TListBox_SetItemIndex(lb_Results, 0, True)
      else
        lb_Results.ItemIndex := -1;
    except
      // ignore
    end; //FI:W501 Empty except block
  finally
    Items.EndUpdate;
  end;
end;

{$IFDEF GX_IDE_IS_HIDPI_AWARE}
procedure TfmFastGrep.ArrangeControls;
begin
  inherited;
  SetListboxItemHeight;
end;
{$ENDIF}

type
  TGrepFastSearchExpert = class(TGX_Expert)
  public
    class function GetName: string; override;
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

{ TGrepFastSearchExpert }

procedure TGrepFastSearchExpert.Execute(Sender: TObject);
begin
  if not Assigned(fmFastGrep) then
    fmFastGrep := TfmFastGrep.Create(Application);
  fmFastGrep.Show;
end;

function TGrepFastSearchExpert.GetActionCaption: string;
begin
  Result := 'Fast Grep';
end;

function TGrepFastSearchExpert.GetHelpString: string;
begin
  Result := 'Does a Grep search for the current editor file and gives a list of matches.'#13#10
    + 'Which can then be selected to position the cursor.';
end;

class function TGrepFastSearchExpert.GetName: string;
begin
  Result := GrepFastSearchName;
end;

function TGrepFastSearchExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TGrepFastSearchExpert);
end.

