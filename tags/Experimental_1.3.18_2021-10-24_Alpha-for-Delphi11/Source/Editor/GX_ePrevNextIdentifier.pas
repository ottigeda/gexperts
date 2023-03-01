// Current identifier location editor expert
// Contributed to GExperts by Max Vlasov <maksee@mail.ru>
// Additional edits by Erik Berry

unit GX_ePrevNextIdentifier;

{$I GX_CondDefine.inc}

interface

uses
  Classes, StdCtrls, Controls, Forms, GX_BaseForm;

type
  TfmPrevNextConfig = class(TfmBaseForm)
    gbxPrevNextOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkCenterMatch: TCheckBox;
    chk_Unfold: TCheckBox;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, ToolsAPI,
  GX_GenericUtils, GX_OtaUtils, GX_EditReader, GX_EditorExpert, GX_ConfigurationInfo;

type
  TViewChangeType = (vctScrollIfNeeded, vctScrollCenter);

  TBaseIdentExpert = class(TEditorExpert)
  protected
    FViewChangeType: TViewChangeType;
{$IFDEF GX_SUPPORTS_FOLDING}
    FUnfoldNearest: boolean;
{$ENDIF}
    function TryGetCurrentIdent(const Source: string; CurPos: Integer;
      var Pos, Len: Integer): boolean;
    function FindIdentAtPos(const Source: string; CurPos: Integer; Prev: boolean;
      var Pos: Integer; var Ident: string): boolean;
    procedure SetPosition(const _Src: string; _CharIdx: Integer);
    function FindIdentAction(const Source: string; Pos: Integer;
      var FoundPos: Integer; var Ident: string): Boolean; virtual; abstract;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    constructor Create; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    procedure Configure; override;
  end;

  TPreviousIdentExpert = class(TBaseIdentExpert)
  protected
    function FindIdentAction(const Source: string; Pos: Integer;
      var FoundPos: Integer; var Ident: string): Boolean; override;
  public
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
  end;

  TNextIdentExpert = class(TBaseIdentExpert)
  protected
    function FindIdentAction(const Source: string; Pos: Integer;
      var FoundPos: Integer; var Ident: string): Boolean; override;
  public
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
  end;

resourcestring
  SIdentHelpString =
    '  This expert detects the identifier under the cursor and ' +
    'allow you to quickly jump to the %s occurrence ' +
    'of that identifier in the same file.';

{ TBaseIdentExpert }

function TBaseIdentExpert.TryGetCurrentIdent(const Source: string; CurPos: Integer;
  var Pos, Len: Integer): Boolean;
begin
  Result := False;

  while CurPos >= 1 do
    if IsCharIdentifier(Source[CurPos]) then
    begin
      Dec(CurPos);
      Result := True;
    end
    else if (not Result) and (CurPos >= 2) then
      if IsCharIdentifier(Source[CurPos - 1]) then
      begin
        Dec(CurPos, 2);
        Result := True;
      end
      else
        Break
    else
      Break;

  if Result then
  begin
    Pos := CurPos + 1;
    Inc(CurPos, 2);
    while (CurPos >= 1) and (CurPos <= Length(Source)) do
      if IsCharIdentifier(Source[CurPos]) then
        Inc(CurPos)
      else
        Break;

    Len := CurPos - Pos;
  end;
end;

function TBaseIdentExpert.FindIdentAtPos(const Source: string; CurPos: Integer; Prev: Boolean;
  var Pos: Integer; var Ident: string): Boolean;
var
  StartPos: Integer;
  Id: string;
  Len: Integer;
begin
  Result := False;

  if TryGetCurrentIdent(Source, CurPos, StartPos, Len) then
  begin
    Id := Copy(Source, StartPos, Len);
    Result := FindTextIdent(Id, Source, StartPos, Prev, Pos);
    Ident := Id;
  end;
end;

procedure TBaseIdentExpert.SetPosition(const _Src: string; _CharIdx: Integer);
var
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
  LinePos: TPoint;
  EditView: IOTAEditView;
begin
  LinePos := CharPosToLinePos(_CharIdx, _Src);
  CharPos.Line := LinePos.Y;
  CharPos.CharIndex := LinePos.X - 1;
  EditView := GxOtaGetTopMostEditView;
  EditView.ConvertPos(False, EditPos, CharPos);

  case FViewChangeType of
    vctScrollCenter: GxOtaGotoEditPos(EditPos);
    vctScrollIfNeeded:
    begin
      EditView.CursorPos := EditPos;
      EditView.MoveViewToCursor;
      EditView.Paint;
    end;
  end;

{$IFDEF GX_SUPPORTS_FOLDING}
  // This raises an AV in boreditu.dll in Delphi 2007 on my GExperts development computer.
  // Note that this also happens if I use the editor popup menu, even if GExperts and CnPack
  // are disabled. Maybe there is something wrong with my installation?
  // * The AV doesn't happen with Delphi 10.2 but it doesn't always unfold the block and even
  //   if it does the cursor position is still wrong.
  // * The AV doesn't happen in Delphi 2006 but it doesn't unfold the block either.
  // -- 2020-03-07 twm
  if FUnfoldNearest then
    (EditView as IOTAElideActions).UnElideNearestBlock;
{$ENDIF}
end;

constructor TBaseIdentExpert.Create;
begin
  inherited;
  FViewChangeType := vctScrollIfNeeded;
{$IFDEF GX_SUPPORTS_FOLDING}
  FUnfoldNearest := True;
{$ENDIF}
end;

procedure TBaseIdentExpert.Execute;
var
  EditRead: TEditReader;
  SourceEditor: IOTASourceEditor;
  CharPos: TOTACharPos;
  CharIdx: Integer;
  Src: string;
  FoundPos: Integer;
  Ident: string;
begin
  if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
    Exit;

  EditRead := TEditReader.Create(SourceEditor.FileName);
  try
    Src := EditRead.GetText;
    CharPos := EditRead.GetCurrentCharPos;
  finally
    FreeAndNil(EditRead);
  end;

  Src := AdjustLineBreaks(Src, tlbsCRLF);
  CharIdx := LinePosToCharPos(Point(CharPos.CharIndex + 1, CharPos.Line), Src);

  if FindIdentAction(Src, CharIdx, FoundPos, Ident) then
    SetPosition(Src, FoundPos)
  else
    MessageBeep($FFFFFFFF);

  IncCallCount;
end;

function TBaseIdentExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TBaseIdentExpert.Configure;
var
  Dlg: TfmPrevNextConfig;
begin
  Dlg := TfmPrevNextConfig.Create(nil);
  try
    Dlg.chkCenterMatch.Checked := FViewChangeType = vctScrollCenter;
{$IFDEF GX_SUPPORTS_FOLDING}
    dlg.chk_Unfold.Checked := FUnfoldNearest;
{$ELSE}
    Dlg.chk_Unfold.Visible := False;
{$ENDIF}

    if Dlg.ShowModal = mrOk then
    begin
      if Dlg.chkCenterMatch.Checked then
        FViewChangeType := vctScrollCenter
      else
        FViewChangeType := vctScrollIfNeeded;
{$IFDEF GX_SUPPORTS_FOLDING}
      FUnfoldNearest := Dlg.chk_Unfold.Checked;
{$ENDIF}
      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TBaseIdentExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  // Do not localize any of the below items.
  FViewChangeType := TViewChangeType(_Settings.ReadEnumerated('ViewChangeType',
    TypeInfo(TViewChangeType), Ord(vctScrollCenter)));
{$IFDEF GX_SUPPORTS_FOLDING}
  FUnfoldNearest := _Settings.ReadBool('UnfoldNearest', True);
{$ENDIF}
end;

procedure TBaseIdentExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);
  // Do not localize any of the below items.
  _Settings.WriteEnumerated('ViewChangeType', TypeInfo(TViewChangeType), Ord(FViewChangeType));
{$IFDEF GX_SUPPORTS_FOLDING}
  _Settings.WriteBool('UnfoldNearest', FUnfoldNearest);
{$ENDIF}
end;

{ TPreviousIdentExpert }

function TPreviousIdentExpert.GetHelpString: string;
begin
  Result := Format(SIdentHelpString, ['previous']);
end;

function TPreviousIdentExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + VK_UP;
end;

function TPreviousIdentExpert.GetDisplayName: string;
begin
  Result := 'Previous Identifier Reference';
end;

class function TPreviousIdentExpert.GetName: string;
begin
  Result := 'PreviousIdent';
end;

class function TPreviousIdentExpert.ConfigurationKey: string;
begin
  Result := 'PrevIdentifier';
end;

function TPreviousIdentExpert.FindIdentAction(const Source: string; Pos: Integer; var FoundPos: Integer;
  var Ident: string): Boolean;
begin
  Result := FindIdentAtPos(Source, Pos, True, FoundPos, Ident);
end;

{ TNextIdentExpert }

class function TNextIdentExpert.ConfigurationKey: string;
begin
  Result := 'NextIdentifier';
end;

function TNextIdentExpert.FindIdentAction(const Source: string; Pos: Integer; var FoundPos: Integer;
  var Ident: string): Boolean;
begin
  Result := FindIdentAtPos(Source, Pos, False, FoundPos, Ident);
end;

function TNextIdentExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + VK_DOWN;
end;

function TNextIdentExpert.GetDisplayName: string;
begin
  Result := 'Next Identifier Reference';
end;

function TNextIdentExpert.GetHelpString: string;
begin
  Result := Format(SIdentHelpString, ['next']);
end;

class function TNextIdentExpert.GetName: string;
begin
  Result := 'NextIdent';
end;

{ TfmPrevNextConfig }

constructor TfmPrevNextConfig.Create(_Owner: TComponent);
begin
  inherited;

  InitDpiScaler;
end;

initialization
  RegisterEditorExpert(TPreviousIdentExpert);
  RegisterEditorExpert(TNextIdentExpert);

end.

