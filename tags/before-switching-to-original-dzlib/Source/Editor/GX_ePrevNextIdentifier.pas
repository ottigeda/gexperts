// Current identifier location editor expert
// Contributed to GExperts by Max Vlasov <maksee@mail.ru>
// Additional edits by Erik Berry

unit GX_ePrevNextIdentifier;

{$I GX_CondDefine.inc}

interface

uses
  Classes, StdCtrls, Controls, Forms, GX_BaseForm, GX_EditorExpert, GX_ConfigurationInfo;

type
  TViewChangeType = (vctScrollIfNeeded, vctScrollCenter);

  TBaseIdentExpert = class(TEditorExpert)
  private
    FSource: string;
    FPosition: Integer;
    procedure SetPosition(Value: Integer);
  protected
    // Source and Position are valid only inside InternalExecute
    property Source: string read FSource;
    property Position: Integer read FPosition write SetPosition;
    procedure InternalExecute; virtual; abstract;
  public
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    procedure Configure; override;
  end;

  TPreviousIdentExpert = class(TBaseIdentExpert)
  private
    Previous: Boolean;
  protected
    function FindIdentAction(const Source: string; Pos: Integer;
      var FoundPos: Integer; var Ident: string): Boolean;
    procedure InternalExecute; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
  end;

  TNextIdentExpert = class(TPreviousIdentExpert)
  public
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
  end;

  TfmPrevNextConfig = class(TfmBaseForm)
    gbxPrevNextOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkCenterMatch: TCheckBox;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, ToolsAPI,
  GX_GenericUtils, GX_OtaUtils, GX_EditReader;

resourcestring
  SIdentHelpString =
    '  This expert detects the identifier under the cursor and ' +
    'allow you to quickly jump to the %s occurrence ' +
    'of that identifier in the same file.';

var
  // This is *local* and used by both the prevident
  // and the nextident expert...
  ViewChangeType: TViewChangeType = vctScrollIfNeeded;

function CurrentIdent(const Source: string; CurPos: Integer;
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

function FindIdentAtPos(const Source: string; CurPos: Integer; Prev: Boolean;
  var Pos: Integer; var Ident: string): Boolean;
var
  StartPos: Integer;
  Id: string;
  Len: Integer;
begin
  Result := False;

  if CurrentIdent(Source, CurPos, StartPos, Len) then
  begin
    Id := Copy(Source, StartPos, Len);
    Result := FindTextIdent(Id, Source, StartPos, Prev, Pos);
    Ident := Id;
  end;
end;

{ TBaseIdentExpert }

procedure TBaseIdentExpert.SetPosition(Value: Integer);
var
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
  LinePos: TPoint;
  EditView: IOTAEditView;
begin
  FPosition := Value;
  LinePos := CharPosToLinePos(FPosition, FSource);
  CharPos.Line := LinePos.Y;
  CharPos.CharIndex := LinePos.X - 1;
  EditView := GxOtaGetTopMostEditView;
  EditView.ConvertPos(False, EditPos, CharPos);

  case ViewChangeType of
    vctScrollCenter: GxOtaGotoEditPos(EditPos);
    vctScrollIfNeeded:
    begin
      EditView.CursorPos := EditPos;
      EditView.MoveViewToCursor;
      EditView.Paint;
    end;
  end;
{$IFDEF GX_DELPHI2005_UP}
  (EditView as IOTAElideActions).UnElideNearestBlock;
{$ENDIF}
end;

procedure TBaseIdentExpert.Execute;
var
  EditRead: TEditReader;
  SourceEditor: IOTASourceEditor;
  CharPos: TOTACharPos;
begin
  FSource := '';
  FPosition := -1;
  if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
    Exit;

  EditRead := TEditReader.Create(SourceEditor.FileName);
  try
    FSource := EditRead.GetText;
    FSource := AdjustLineBreaks(FSource, tlbsCRLF);
    CharPos := EditRead.GetCurrentCharPos;
    FPosition := LinePosToCharPos(Point(CharPos.CharIndex + 1, CharPos.Line), FSource);
  finally
    FreeAndNil(EditRead);
  end;

  InternalExecute;

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
    Dlg.chkCenterMatch.Checked := ViewChangeType = vctScrollCenter;

    if Dlg.ShowModal = mrOk then
    begin
      if Dlg.chkCenterMatch.Checked then
        ViewChangeType := vctScrollCenter
      else
        ViewChangeType := vctScrollIfNeeded;

      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

{ TPreviousIdentExpert }

constructor TPreviousIdentExpert.Create;
begin
  inherited Create;
  Previous := True;
end;

procedure TPreviousIdentExpert.InternalExecute;
var
  FoundPos: Integer;
  Ident: string;
begin
  if FindIdentAction(Source, Position, FoundPos, Ident) then
    Position := FoundPos
  else
    MessageBeep($FFFFFFFF);
end;

function TPreviousIdentExpert.FindIdentAction(const Source: string; Pos: Integer;
  var FoundPos: Integer; var Ident: string): Boolean;
begin
  Result := FindIdentAtPos(Source, Pos, Previous, FoundPos, Ident);
end;

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

procedure TPreviousIdentExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  // Do not localize any of the below items.
  ViewChangeType := TViewChangeType(_Settings.ReadEnumerated('ViewChangeType', TypeInfo(TViewChangeType), Ord(vctScrollCenter)));
end;

procedure TPreviousIdentExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);
  // Do not localize any of the below items.
  _Settings.WriteEnumerated('ViewChangeType', TypeInfo(TViewChangeType), Ord(ViewChangeType));
end;

{ TNextIdentExpert }

class function TNextIdentExpert.ConfigurationKey: string;
begin
  Result := 'NextIdentifier';
end;

constructor TNextIdentExpert.Create;
begin
  inherited Create;
  Previous := False;
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

initialization
  RegisterEditorExpert(TPreviousIdentExpert);
  RegisterEditorExpert(TNextIdentExpert);

end.

