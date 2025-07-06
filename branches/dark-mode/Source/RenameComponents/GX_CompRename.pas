unit GX_CompRename;

(* Things left to consider:
 *  - Search a new component's ancestors to get an alternate rename rule
 *  - Remove timer hack somehow?
 *  - Allow explicit %d rename rules to apply before the rename dialog shows
 *)

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ToolsAPI, ComCtrls, Buttons,
  GX_Experts, GX_ConfigurationInfo, GX_EditorChangeServices, Contnrs, Messages,
  Types, GX_BaseForm, GX_CheckButton;

type
  TIsValidComponentName = function (const OldName, NewName: WideString; var Reason: WideString): Boolean of object;

  // Simple rename dialog that shows the old and new component name
  TfmCompRename = class(TfmBaseForm)
    lblOldName: TLabel;
    edtOldName: TEdit;
    lblNewName: TLabel;
    edtNewName: TEdit;
    p_Buttons: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    lblReason: TLabel;
    btnSettings: TButton;
    pc_Additional: TPageControl;
    ts_Align: TTabSheet;
    grp_Margins: TGroupBox;
    ed_MarginTop: TEdit;
    l_MarginTop: TLabel;
    ed_MarginLeft: TEdit;
    l_MarginLeft: TLabel;
    l_MarginRight: TLabel;
    ed_MarginRight: TEdit;
    ed_MarginBottom: TEdit;
    l_MarginBottom: TLabel;
    chk_WithMargins: TCheckBox;
    ts_Anchors: TTabSheet;
    b_AnchorLeft: TBitBtn;
    b_AnchorRight: TBitBtn;
    b_AnchorTop: TBitBtn;
    b_AnchorBottom: TBitBtn;
    b_AlignTop: TBitBtn;
    b_AlignLeft: TBitBtn;
    b_AlignRight: TBitBtn;
    b_AlignClient: TBitBtn;
    b_AlignBottom: TBitBtn;
    b_AlignNone: TBitBtn;
    b_AlignCustom: TBitBtn;
    b_Margins0: TButton;
    b_Margins3: TButton;
    b_Margins6: TButton;
    b_Margins8: TButton;
    procedure edtNewNameChange(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure pc_AdditionalChange(Sender: TObject);
    procedure b_AlignTopClick(Sender: TObject);
    procedure b_AlignLeftClick(Sender: TObject);
    procedure b_AlignClientClick(Sender: TObject);
    procedure b_AlignRightClick(Sender: TObject);
    procedure b_AlignBottomClick(Sender: TObject);
    procedure b_AlignNoneClick(Sender: TObject);
    procedure b_AlignCustomClick(Sender: TObject);
    procedure b_Margins0Click(Sender: TObject);
    procedure b_Margins3Click(Sender: TObject);
    procedure b_Margins6Click(Sender: TObject);
    procedure b_Margins8Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOnIsValidComponentName: TIsValidComponentName;
    FProperties: TObjectList;
    FAnchorButtons: array[TAnchorKind] of TGXCheckButton;
    FAlignButtons: array[TAlign] of TGXCheckButton;
    FComponentClassName: WideString;
    procedure InitializeForm;
    procedure InitOtherProps(const _Component: IOTAComponent; _OtherProps: TStrings;
      _UseRules: Boolean);
    procedure AddComponentProperty(const _Component: IOTAComponent; PropertyName: WideString; const Value: WideString);
    function GetComponentProperty(Index: Integer): WideString;
    procedure SetComponent(const _Component: IOTAComponent);
    procedure InitAlign(const _Component: IOTAComponent);
    procedure ApplyAlign(const _Component: IOTAComponent);
    procedure InitAnchors(const _Component: IOTAComponent);
    procedure ApplyAnchors(const _Component: IOTAComponent);
    procedure HandleAlignButtons(_Align: TAlign);
    procedure SetMargins(_Value: integer);
    procedure DialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
    procedure SetData(const _Component: IOTAComponent; _RenameRule: WideString;
      _OtherProps: TStrings; _UseRules: Boolean;
      _HandleIsValidComponentName: TIsValidComponentName);
    procedure GetData(const _Component: IOTAComponent; _OtherProps: TStrings);
    procedure SetRuleSelection(SelStart, SelEnd: Integer);
  protected
    procedure ArrangeControls; override;
  public
    class function Execute(_Owner: TWinControl; const _Component: IOTAComponent;
      _RenameRule: WideString; _OtherProps: TStrings; _UseRules: Boolean;
      _HandleIsValidComponentName: TIsValidComponentName): Boolean;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Menus, StrUtils, IniFiles, Graphics, TypInfo, Math,
  u_dzClassUtils, u_dzVclUtils, u_dzStringUtils, u_dzTypeInfoUtils,
  Themes,
  {$IFOPT D+}GX_DbugIntf, {$ENDIF}
  GX_CompRenameConfig, GX_OtaUtils, GX_GenericUtils, GX_IdeUtils;

type
  TRenameComponentsExpert = class;

  TCompRenameNotifier = class(TInterfacedObject, IGxEditorNotification)
  private
    FChangeServiceNotifierIndex: Integer;
    FClient: TRenameComponentsExpert;
  protected
    // IGxEditorNotification
    procedure NewModuleOpened(const Module: IOTAModule);
    procedure SourceEditorModified(const SourceEditor: IOTASourceEditor);
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
    procedure ComponentRenamed(const FormEditor: IOTAFormEditor;
      Component: IOTAComponent; const OldName, NewName: string);
    function EditorKeyPressed(const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
    function GetIndex: Integer;
  protected
    procedure Attach;
  public
    constructor Create(const Client: TRenameComponentsExpert);
    destructor Destroy; override;

    procedure Detach;
  end;

  TRenameComponentsExpert = class(TGX_Expert)
  private
    FCompRenameNotifier: TCompRenameNotifier;
    FRenameRuleListVcl: TStringList;
    FRenameRuleListFmx: TStringList;
    FShowDialog: Boolean;
    FAutoAddClasses: Boolean;
    FComponentNames: TStringList;
    FFormNames: TStringList;
    FTimer: TTimer;
    FFormEditor: IOTAFormEditor;
    FFormWidth: Integer;
    FFormHeight: Integer;
    function DoRename(const Component: IOTAComponent; UseRules: Boolean): Boolean;
    procedure HandleOnExport(_Sender: TObject; _RulesListVcl, _RulesListFmx: TStringList);
    procedure HandleOnImport(_Sender: TObject; const _fn: string;
      _RulesListVcl, _RulesListFmx: TStringList);
    function GetActiveRenameRuleList: TStringList;
    class function TryLoadRules(_Settings: IExpertSettings; const _Section: string;
      _TryRoot: Boolean; _RulesList: TStringList): Boolean;
    class procedure SaveRules(_Settings: IExpertSettings; const _Section: string;
      _RulesList: TStringList);
  protected
    procedure AddNewClass(const AClassName: WideString);
    procedure DoOnTimer(Sender: TObject);
    function GetClassRenameRule(const AClassName: WideString): WideString;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure ComponentRenamed(const FormEditor: IOTAFormEditor;
      Component: IOTAComponent; const OldName, NewName: WideString);
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
    function IsValidComponentName(const OldName, NewName: WideString; var Reason: WideString): Boolean;
    procedure AddNotifier;
    procedure RemoveNotifier;
    function IsDefaultComponentName(Component: IOTAComponent; const NewName: WideString): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure(_Owner: TWinControl); overload; override;
    procedure Configure(_Owner: TWinControl; const _Selected: string); reintroduce; overload;
    function GetActionCaption: string; override;
    function GetDefaultShortCut: TShortCut; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    procedure SetActive(New: Boolean); override;
    function IsDefaultActive: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
  end;

resourcestring
  SPropertyNotFound = 'Property not found';

var
  PrivateCompRenameExpert: TRenameComponentsExpert;

{ TfmCompRename }

class function TfmCompRename.Execute(_Owner: TWinControl; const _Component: IOTAComponent;
  _RenameRule: WideString; _OtherProps: TStrings; _UseRules: Boolean;
  _HandleIsValidComponentName: TIsValidComponentName): Boolean;
var
  frm: TfmCompRename;
begin
  frm := TfmCompRename.Create(_Owner);
  try
    frm.SetData(_Component, _RenameRule, _OtherProps, _UseRules, _HandleIsValidComponentName);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Component, _OtherProps);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmCompRename.SetData(const _Component: IOTAComponent;
  _RenameRule: WideString; _OtherProps: TStrings; _UseRules: Boolean;
  _HandleIsValidComponentName: TIsValidComponentName);
var
  Pipe1Pos: Integer;
  Pipe2Pos: Integer;
  CompName: WideString;
begin
  Pipe2Pos := -1;
  Pipe1Pos := Pos(WideString('|'), _RenameRule);
  if Pipe1Pos > 0 then begin
    System.Delete(_RenameRule, Pipe1Pos, 1);
    Pipe2Pos := LastCharPos(_RenameRule, '|');
    if (Pipe2Pos >= Pipe1Pos) then
      System.Delete(_RenameRule, Pipe2Pos, 1);
  end;

  FOnIsValidComponentName := _HandleIsValidComponentName;

  CompName := GxOtaGetComponentName(_Component);
  edtOldName.Text := CompName;

  SetComponent(_Component);

  if Assigned(_OtherProps) then begin
    InitOtherProps(_Component, _OtherProps, _UseRules)
  end;

  TControl_SetMinConstraints(Self);
  InitDpiScaler;

  if _UseRules then begin
    edtNewName.Text := _RenameRule;
    if Pipe1Pos > 0 then
      SetRuleSelection(Pipe1Pos - 1, Pipe2Pos - 1)
    else
      SetRuleSelection(Length(_RenameRule), Length(_RenameRule));
  end else begin
    edtNewName.Text := CompName;
    SetRuleSelection(0, Length(CompName));
  end;
end;

procedure TfmCompRename.GetData(const _Component: IOTAComponent; _OtherProps: TStrings);
var
  CompName: WideString;
  i: Integer;
  PropName: WideString;
begin
  CompName := Trim(edtNewName.Text);
  GxOtaSetComponentName(_Component, CompName);

  ApplyAlign(_Component);
  ApplyAnchors(_Component);

  if Assigned(_OtherProps) then begin
    for i := 0 to _OtherProps.Count - 1 do begin
      PropName := _OtherProps.Names[i];
      if PropName = '' then
        PropName := _OtherProps[i];
      if GxOtaPropertyExists(_Component, PropName) then
        GxOtaSetComponentPropertyAsString(_Component, PropName, GetComponentProperty(i));
    end;
  end;
end;

procedure TfmCompRename.FormShow(Sender: TObject);
begin
  inherited;
  ArrangeControls;
end;

procedure TfmCompRename.pc_AdditionalChange(Sender: TObject);
begin
  inherited;
  if pc_Additional.ActivePage = ts_Align then
    TWinControl_SetFocus(FAlignButtons[alClient])
  else if pc_Additional.ActivePage = ts_Anchors then
    TWinControl_SetFocus(FAnchorButtons[akTop]);
end;

procedure TfmCompRename.ApplyAlign(const _Component: IOTAComponent);
var
  BoolValue: LongBool;
  CompMargins: TObject;
  al: TAlign;
  IntValue: Integer;
begin
  if not GxOtaActiveDesignerIsVCL then begin
    // for whatever reason this currently does not work for FMX
    Exit; //==>
  end;

  if not ts_Align.TabVisible then
    Exit; //==>

  al := Low(TAlign);
  while al < High(TAlign) do begin
    if FAlignButtons[al].Down then
      Break;
    Inc(al);
  end;
  IntValue := Ord(al);
  _Component.SetPropByName('Align', IntValue);

  if not grp_Margins.Visible then
    Exit; //==>

  if chk_WithMargins.Visible then begin
    BoolValue := chk_WithMargins.Checked;
    _Component.SetPropByName('AlignWithMargins', BoolValue);
  end;
  if _Component.GetPropTypeByName('Margins') = tkClass then begin
    CompMargins := GetObjectProp((_Component as INTAComponent).GetComponent, 'Margins');
    if TryStrToInt(ed_MarginTop.Text, IntValue) then
      SetOrdProp(CompMargins, 'Top', IntValue);
    if TryStrToInt(ed_MarginLeft.Text, IntValue) then
      SetOrdProp(CompMargins, 'Left', IntValue);
    if TryStrToInt(ed_MarginRight.Text, IntValue) then
      SetOrdProp(CompMargins, 'Right', IntValue);
    if TryStrToInt(ed_MarginBottom.Text, IntValue) then
      SetOrdProp(CompMargins, 'Bottom', IntValue);
  end;
end;

procedure TfmCompRename.InitAlign(const _Component: IOTAComponent);
var
  HasAlign: Boolean;
  HasMargins: Boolean;
  HasAlignWithMargins: Boolean;
  BoolValue: LongBool;
  CompMargins: TObject;
  IntValue: Integer;
  AlignValue: TAlign;
  al: TAlign;
begin
  HasAlign := False;
  HasMargins := False;
  HasAlignWithMargins := False;
  try
    // for whatever reason this currently does not work for FMX
    if not GxOtaActiveDesignerIsVCL then
      Exit; //==>

    // GetPropValueByName expects 4 byte values for enums
    if not _Component.GetPropValueByName('Align', IntValue) then
      Exit; //==>

    if not InRange(IntValue, Ord(Low(TAlign)), Ord(High(TAlign))) then
      Exit; //==>

    HasAlign := True;
    AlignValue := TAlign(IntValue);
    for al := Low(TAlign) to High(TAlign) do
      FAlignButtons[al].Down := (al = AlignValue);

    if _Component.GetPropTypeByName('Margins') <> tkClass then
      Exit; //==>

    HasMargins := True;

    // GetPropValueByName expects 4 byte values for booleans too
    if _Component.GetPropValueByName('AlignWithMargins', BoolValue) then begin
      HasAlignWithMargins := True;
      chk_WithMargins.Checked := BoolValue;
    end;

    CompMargins := GetObjectProp((_Component as INTAComponent).GetComponent, 'Margins');
    IntValue := GetOrdProp(CompMargins, 'Top');
    ed_MarginTop.Text := IntToStr(IntValue);
    IntValue := GetOrdProp(CompMargins, 'Left');
    ed_MarginLeft.Text := IntToStr(IntValue);
    IntValue := GetOrdProp(CompMargins, 'Right');
    ed_MarginRight.Text := IntToStr(IntValue);
    IntValue := GetOrdProp(CompMargins, 'Bottom');
    ed_MarginBottom.Text := IntToStr(IntValue);
  finally
    ts_Align.TabVisible := HasAlign;
    grp_Margins.Visible := HasMargins;
    chk_WithMargins.Visible := HasAlignWithMargins;
  end;
end;

procedure TfmCompRename.ApplyAnchors(const _Component: IOTAComponent);
var
  IntValue: Integer;
  AnchorValue: TAnchors absolute IntValue; // not the other way round!
  ak: TAnchorKind;
begin
  if ts_Anchors.TabVisible then begin
    IntValue := 0;
    Assert(AnchorValue = []);
    for ak  := Low(TAnchorKind) to High(TAnchorKind) do begin
      if FAnchorButtons[ak].Down then
        Include(AnchorValue, ak);
    end;
    _Component.SetPropByName('Anchors', IntValue);
  end;
end;

procedure TfmCompRename.InitAnchors(const _Component: IOTAComponent);
var
  IntValue: Integer;
  AnchorValue: TAnchors absolute IntValue; // not the other way round!
  ak: TAnchorKind;
begin
  if not _Component.GetPropValueByName('Anchors', IntValue) then begin
    ts_Anchors.TabVisible := False;
  end else begin
    for ak := Low(TAnchorKind) to High(TAnchorKind) do
      FAnchorButtons[ak].Down := (ak in AnchorValue);
  end;
end;

procedure TfmCompRename.SetComponent(const _Component: IOTAComponent);
var
  h: Integer;
begin
  FComponentClassName := _Component.GetComponentType;
  InitAlign(_Component);
  InitAnchors(_Component);
  if not ts_Align.TabVisible and not ts_Anchors.TabVisible then begin
    h := pc_Additional.Height;
    p_Buttons.Height := p_Buttons.Height - h;
    Self.Height := Self.Height - h;
    pc_Additional.Visible := False;
  end;
end;

procedure TfmCompRename.InitOtherProps(const _Component: IOTAComponent; _OtherProps: TStrings;
  _UseRules: Boolean);
var
  i: Integer;
  UsePropValue: Boolean;
  PropName: string;
  PropValue: string;
begin
  for i := 0 to _OtherProps.Count - 1 do begin
    UsePropValue := False;
    PropName := _OtherProps.Names[i];
    if PropName = '' then
      PropName := _OtherProps[i];
    if not GxOtaPropertyExists(_Component, PropName) then begin
      AddComponentProperty(_Component, PropName, SPropertyNotFound);
    end else begin
      if _UseRules then begin
        PropValue := _OtherProps.Values[PropName];
        if PropValue <> '' then begin
          PropValue := AnsiDequotedStr(PropValue, #39);
          if (PropValue = #39#39) then
            PropValue := '';
          UsePropValue := True;
        end;
      end;
      if UsePropValue then
        AddComponentProperty(_Component, PropName, PropValue)
      else
        AddComponentProperty(_Component, PropName,
          GxOtaGetComponentPropertyAsString(_Component, PropName, True));
    end;
  end;
end;

procedure TfmCompRename.edtNewNameChange(Sender: TObject);
var
  LOldName: WideString;
  LNewName: WideString;
  Reason: WideString;
begin
  if Assigned(FOnIsValidComponentName) then
  begin
    LOldName := edtOldName.Text;
    LNewName := edtNewName.Text;
    btnOK.Enabled := FOnIsValidComponentName(LOldName, LNewName, Reason);
    if btnOK.Enabled then
      lblReason.Visible := False
    else
    begin
      lblReason.Caption := Reason;
      lblReason.Visible := True;
    end;
  end
  else
    btnOK.Enabled := IsValidIdent(edtNewName.Text);
end;

procedure TfmCompRename.SetRuleSelection(SelStart, SelEnd: Integer);
begin
  edtNewName.SelStart := SelStart;
  if SelEnd > SelStart then
    edtNewName.SelLength := SelEnd - SelStart;
end;

procedure TfmCompRename.AddComponentProperty(const _Component: IOTAComponent;
  PropertyName: WideString; const Value: WideString);

  function PropertyIsEnum(const _Component: IOTAComponent; _PropName: string;
    out _TypeInfo: PTypeInfo; out _Min, _Max: Integer): Boolean;
  var
    NativeComp: TComponent;
    TypeData: PTypeData;
    PropInfo: PPropInfo;
  begin
    Result := False;
    NativeComp := GxOtaGetNativeComponent(_Component);
    if Assigned(NativeComp) then begin
      PropInfo := GetPropInfo(NativeComp, PropertyName);
      if PropInfo.PropType^^.Kind = tkEnumeration then begin
        TypeData := GetTypeData(PropInfo^.PropType^);
        _Min := TypeData.MinValue;
        _Max := TypeData.MaxValue;
        _TypeInfo := PropInfo.PropType^;
        Result := True;
      end;
    end;
  end;

var
  Lbl: TLabel;
  Edit: TEdit;
  diff: Integer;
  TypeInfo: PTypeInfo;
  IsEnum: Boolean;
  MinValue: Integer;
  MaxValue: Integer;
  cmb: TComboBox;
  i: Integer;
  s: string;
begin
  PropertyName := Trim(PropertyName);
  if PropertyName = '' then
    Exit; //==>

  Lbl := TLabel.Create(Self);
  Lbl.Name := 'lblProperty_' + PropertyName;
  Lbl.Parent := Self;
  Lbl.Top := (FProperties.Count + 1) * (lblNewName.Top - lblOldName.Top) + lblNewName.Top;
  Lbl.Left := lblNewName.Left;
  Lbl.Caption := PropertyName;

  IsEnum := false;
  if Value = SPropertyNotFound then begin
    Lbl.Font.Color := GetThemedColor(clGrayText);
  end else begin
    IsEnum := PropertyIsEnum(_Component, PropertyName, TypeInfo, MinValue, MaxValue);
  end;

  if IsEnum then begin
    cmb := TComboBox.Create(Self);
    cmb.Name := 'cmbProperty_' + PropertyName;
    cmb.Parent := Self;
    cmb.Top := (FProperties.Count + 1) * (edtNewName.Top - edtOldName.Top) + edtNewName.Top;
    cmb.Left := edtNewName.Left;
    cmb.Width := edtNewName.Width;
    cmb.Style := csDropDownList;
    for i := MinValue to MaxValue do begin
      s := SafeGetEnumName(TypeInfo, i);
      cmb.Items.Add(s);
    end;
    cmb.TabOrder := FProperties.Count + 1 + edtNewName.TabOrder;
    TComboBox_SelectWithoutChangeEvent(cmb, Value, 0);
    FProperties.Add(cmb);
  end else begin
    Edit := TEdit.Create(Self);
    Edit.Name := 'edtProperty_' + PropertyName;
    Edit.Parent := Self;
    Edit.Top := (FProperties.Count + 1) * (edtNewName.Top - edtOldName.Top) + edtNewName.Top;
    Edit.Left := edtNewName.Left;
    Edit.Width := edtNewName.Width;
    Edit.Text := Value;
    Edit.TabOrder := FProperties.Count + 1 + edtNewName.TabOrder;
    FProperties.Add(Edit);

    if Value = SPropertyNotFound then begin
      Edit.ReadOnly := True;
      Edit.Color := GetThemedColor(clBtnFace);
      Edit.TabStop := False;
    end;
  end;

  diff := edtNewName.Top - edtOldName.Top;
  Height := Height + diff;
end;

function TfmCompRename.GetComponentProperty(Index: Integer): WideString;
var
  obj: TObject;
begin
  if (Index < 0) or (Index >= FProperties.Count) then
    raise Exception.Create('Invalid property index in TfmCompRename.GetComponentProperty');

  obj := FProperties[Index];
  if obj is TEdit then begin
    Result := TEdit(FProperties[Index]).Text;
  end else if obj is TComboBox then begin
    Result := TComboBox_GetSelectedDef(TComboBox(obj), '');
  end else
    raise Exception.Create('Invalid property value control in TfmCompRename.GetComponentProperty');
end;

constructor TfmCompRename.Create(Owner: TComponent);
begin
  inherited;
  ActiveControl := edtNewName;

  FProperties := TObjectList.Create(False);
  FAlignButtons[alTop] := TGXCheckButton.Clone(b_AlignTop);
  FAlignButtons[alLeft] := TGXCheckButton.Clone(b_AlignLeft);
  FAlignButtons[alClient] := TGXCheckButton.Clone(b_AlignClient);
  FAlignButtons[alRight] := TGXCheckButton.Clone(b_AlignRight);
  FAlignButtons[alBottom] := TGXCheckButton.Clone(b_AlignBottom);
  FAlignButtons[alNone] := TGXCheckButton.Clone(b_AlignNone);
  FAlignButtons[alCustom] := TGXCheckButton.Clone(b_AlignCustom);

  FAnchorButtons[akTop] := TGXCheckButton.Clone(b_AnchorTop);
  FAnchorButtons[akLeft] := TGXCheckButton.Clone(b_AnchorLeft);
  FAnchorButtons[akRight] := TGXCheckButton.Clone(b_AnchorRight);
  FAnchorButtons[akBottom] := TGXCheckButton.Clone(b_AnchorBottom);
  p_Buttons.BevelOuter := bvNone;

  InitializeForm;
end;

destructor TfmCompRename.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

procedure TfmCompRename.DialogKey(var Msg: TWMKey);
begin
  // make the selection of alignment and anchors via arrow keys more intuitive
  case Msg.CharCode of
    VK_DOWN: begin
        if ActiveControl = FAlignButtons[alTop] then
          FAlignButtons[alClient].SetFocus
        else if (ActiveControl = FAlignButtons[alClient])
             or (ActiveControl = FAlignButtons[alLeft])
             or (ActiveControl = FAlignButtons[alRight])
        then
          FAlignButtons[alBottom].SetFocus
        else if (ActiveControl = FAnchorButtons[akTop])
             or (ActiveControl = FAnchorButtons[akLeft])
             or (ActiveControl = FAnchorButtons[akRight])
        then
          FAnchorButtons[akBottom].SetFocus
        else
          inherited;
      end;
    VK_UP: begin
        if ActiveControl = FAlignButtons[alBottom] then
          FAlignButtons[alClient].SetFocus
        else if (ActiveControl = FAlignButtons[alClient])
             or (ActiveControl = FAlignButtons[alLeft])
             or (ActiveControl = FAlignButtons[alRight])
        then
          FAlignButtons[alTop].SetFocus
        else if (ActiveControl = FAlignButtons[alNone])
             or (ActiveControl = FAlignButtons[alCustom])
        then
          FAlignButtons[alBottom].SetFocus
        else if (ActiveControl = FAnchorButtons[akBottom])
             or (ActiveControl = FAnchorButtons[akLeft])
             or (ActiveControl = FAnchorButtons[akRight])
        then
          FAnchorButtons[akTop].SetFocus
        else
          inherited;
      end;
    VK_RIGHT: begin
        if ActiveControl = FAlignButtons[alLeft] then
          FAlignButtons[alClient].SetFocus
        else if ActiveControl = FAlignButtons[alClient] then
          FAlignButtons[alRight].SetFocus
        else if (ActiveControl = FAnchorButtons[akTop])
             or (ActiveControl = FAnchorButtons[akBottom])
             or (ActiveControl = FAnchorButtons[akLeft])
        then
          FAnchorButtons[akRight].SetFocus
        else
          inherited;
      end;
    VK_LEFT: begin
        if ActiveControl = FAlignButtons[alRight] then
          FAlignButtons[alClient].SetFocus
        else if ActiveControl = FAlignButtons[alClient] then
          FAlignButtons[alLeft].SetFocus
        else if (ActiveControl = FAnchorButtons[akTop])
             or (ActiveControl = FAnchorButtons[akBottom])
             or (ActiveControl = FAnchorButtons[akRight]) then
          FAnchorButtons[akLeft].SetFocus
        else
          inherited;
      end;
  else
    inherited;
  end;
end;

{ TCompRenameNotifier }

procedure TCompRenameNotifier.Attach;
begin
  Assert(FChangeServiceNotifierIndex = -1);
  FChangeServiceNotifierIndex := GxEditorChangeServices.AddNotifier(Self);
end;

procedure TCompRenameNotifier.ComponentRenamed(const FormEditor: IOTAFormEditor;
  Component: IOTAComponent; const OldName, NewName: string);
begin
  if Assigned(FClient) then
    FClient.ComponentRenamed(FormEditor, Component, OldName, NewName);
end;

constructor TCompRenameNotifier.Create(const Client: TRenameComponentsExpert);
begin
  inherited Create;

  if Assigned(Client) then
  begin
    FClient := Client;
    FChangeServiceNotifierIndex := -1;
    Attach;
  end;
end;

destructor TCompRenameNotifier.Destroy;
begin
  Detach;
  FClient := nil;
  inherited Destroy;
end;

procedure TCompRenameNotifier.Detach;
begin
  GxEditorChangeServices.RemoveNotifierIfNecessary(FChangeServiceNotifierIndex);
end;

function TCompRenameNotifier.EditorKeyPressed(
  const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
begin
  Result := False;
  // Nothing
end;

procedure TCompRenameNotifier.FormEditorModified(const FormEditor: IOTAFormEditor);
begin
  if Assigned(FClient) then
    FClient.FormEditorModified(FormEditor);
end;

function TCompRenameNotifier.GetIndex: Integer;
begin
  Result := FChangeServiceNotifierIndex;
end;

procedure TCompRenameNotifier.NewModuleOpened(const Module: IOTAModule);
begin
  // Nothing
end;

procedure TCompRenameNotifier.SourceEditorModified(const SourceEditor: IOTASourceEditor);
begin
  // Nothing
end;

{ TRenameComponentsExpert }

function  TRenameComponentsExpert.GetActiveRenameRuleList: TStringList;
begin
  if GxOtaActiveDesignerIsFMX then begin
    Result := FRenameRuleListFmx;
  end else begin
    // I can't be bothered to distinguish between VCL, CLX and dotNET, since only VCL has survived
    Result := FRenameRuleListVcl;
  end;
end;


procedure TRenameComponentsExpert.AddNewClass(const AClassName: WideString);
var
  RuleList: TStringList;
begin
  RuleList := GetActiveRenameRuleList;
  Assert(Assigned(RuleList));

  // Classname already in list?
  if RuleList.IndexOfName(AClassName) < 0 then
  begin
    // No -> add "TClass=Class" as new entry
    RuleList.Add(AClassName+'='+Copy(AClassName, 2));
    SaveSettings;
  end;
end;

procedure TRenameComponentsExpert.Execute(Sender: TObject);
var
  SelCount: Integer;
  CurrentComponent: IOTAComponent;
  i: Integer;
begin
  if not GxOtaFormEditorHasSelectedComponent then
    Configure(nil)
  else
  begin
    if not GxOtaTryGetCurrentFormEditor(FFormEditor) then
      Exit;
    try
      SelCount := FFormEditor.GetSelCount;
      for i := 0 to SelCount - 1 do begin
        CurrentComponent := FFormEditor.GetSelComponent(i);
        if not DoRename(CurrentComponent, False) then
          Break;
      end;
    finally
      FFormEditor := nil;
    end;
  end;

  IncCallCount;
end;

procedure TRenameComponentsExpert.Configure(_Owner: TWinControl);
begin
  Configure(_Owner, '');
end;

procedure TRenameComponentsExpert.Configure(_Owner: TWinControl; const _Selected: string);
begin
//    SetFormIcon(Dialog);
  FRenameRuleListVcl.Sort;
  FRenameRuleListFmx.Sort;
  if TfmCompRenameConfig.Execute(_Owner, HandleOnImport, HandleOnExport,
    FRenameRuleListVcl, FRenameRuleListFmx, FShowDialog, FAutoAddClasses,
    FFormWidth, FFormHeight, _Selected) then
    SaveSettings;
end;

procedure TRenameComponentsExpert.HandleOnImport(_Sender: TObject; const _fn: string;
  _RulesListVcl, _RulesListFmx: TStringList);
var
  GXSettings: TGExpertsSettings;
  Settings: IExpertSettings;
  ini: TMemIniFile;
begin
  {$IFOPT D+} SendDebugFmt('RenameComponents importing from file %s', [_fn]); {$ENDIF}
  if not FileExists(_fn) then begin
    {$IFOPT D+} SendDebug('File does not exist'); {$ENDIF}
    Exit; //==>
    end;


  GXSettings := nil;
  ini := TMemInifile.Create(_fn);
  try
    GXSettings := TGExpertsSettings.Create(ini, True);
    Ini := nil;
    Settings :=TExpertSettingsEx.Create(GXSettings, ConfigurationKey);
    GXSettings := nil;
    if not TryLoadRules(Settings, 'vcl', False, _RulesListVcl) then
      TryLoadRules(Settings, 'Items', False, _RulesListVcl);
    TryLoadRules(Settings, 'fmx', False, _RulesListFmx);
{$IFOPT D+}SendDebugFmt('RenameComponents imported %d vcl and %d fmx rules', [_RulesListVcl.Count, _RulesListFmx.Count]); {$ENDIF}
  finally
    FreeAndNil(GXSettings);
    FreeAndNil(ini);
  end;
end;

procedure TRenameComponentsExpert.HandleOnExport(_Sender: TObject; _RulesListVcl, _RulesListFmx: TStringList);
var
  GXSettings: TGExpertsSettings;
  Settings: IExpertSettings;
  ini: TMemIniFile;
  fn: string;
begin
  fn := 'GExperts_' + Self.GetName + '.ini';
  if not ShowSaveDialog('Select file to export to', 'ini', fn, 'INI files (*.ini)|*.ini') then
    Exit; //==>

  GXSettings := nil;
  ini := TMemIniFile.Create(fn);
  try
    GXSettings := TGExpertsSettings.Create(ini, False);
    Settings := TExpertSettingsEx.Create(GXSettings, ConfigurationKey);
    GXSettings := nil;
    SaveRules(Settings, 'vcl', _RulesListVcl);
    SaveRules(Settings, 'fmx', _RulesListFmx);
    ini.UpdateFile;
  finally
    FreeAndNil(GXSettings);
    FreeAndNil(ini);
  end;
end;

procedure TRenameComponentsExpert.ComponentRenamed(const FormEditor: IOTAFormEditor;
    Component: IOTAComponent; const OldName, NewName: WideString);
var
  RenameRuleList: TStringList;
begin
  // Bug: Delphi 8 can not set string properties on components
  if RunningDelphi8 then
    Exit;

  RenameRuleList := GetActiveRenameRuleList;
  if (not Assigned(RenameRuleList)) or (RenameRuleList.Count < 1) then
    Exit;
  if Active and Assigned(FormEditor) and (OldName = '') and (NewName > '') then
  begin
    // If the form being edited isn't the active designer, assume some
    // automated tool is doing the editing, and GExperts ignores the change
    if FormEditor.FileName <> GxOtaGetCurrentSourceFile then
      Exit;
    // Don't change the names of components that are not a default component
    // name based on the class.  This prevents renaming pasted components.
    if Assigned(Component) and IsDefaultComponentName(Component, NewName) then
    begin
      FComponentNames.Add(NewName);
      FFormNames.Add(FormEditor.FileName);
      FTimer.Enabled := True;
    end;
  end;
end;

constructor TRenameComponentsExpert.Create;
begin
  inherited Create;
  PrivateCompRenameExpert := Self;
  FComponentNames := TStringList.Create;
  FFormNames := TStringList.Create;
  FRenameRuleListVcl := TStringList.Create;
  FRenameRuleListFmx := TStringList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := DoOnTimer;
  AddNotifier;
end;

destructor TRenameComponentsExpert.Destroy;
begin
  PrivateCompRenameExpert := nil;
  RemoveNotifier;
  FreeAndNil(FTimer);

  TStrings_FreeWithObjects(FRenameRuleListVcl);
  TStrings_FreeWithObjects(FRenameRuleListFmx);

  FreeAndNil(FComponentNames);
  FreeAndNil(FFormNames);
  inherited;
end;

function TRenameComponentsExpert.DoRename(const Component: IOTAComponent; UseRules: Boolean): Boolean;
var
  i: Integer;
  PlaceHolderPos: Integer;
  ClassName: WideString;
  RenameRule: WideString;
  CompName: WideString;
  PropName: WideString;
  PropValue: String;
  SearchName: WideString;
  Reason: WideString;
  ShowDialog: Boolean;
  OtherProps: TStringList;
  Index: Integer;
  RenameRuleList: TStringList;
begin
  Assert(Assigned(Component));
  Assert(Assigned(FFormEditor));
  Result := True;

  CompName := GxOtaGetComponentName(Component);
  if CompName = '' then
    Exit; //==>

  // Get the component class name
  ClassName := Component.GetComponentType;

  // Get the desired rename rule for this class
  RenameRule := GetClassRenameRule(ClassName);
  if RenameRule = '' then
  begin
    if FAutoAddClasses then
    begin
      AddNewClass(ClassName);
      RenameRule := GetClassRenameRule(ClassName);
    end;
  end;

  PlaceHolderPos := Pos(WideString('%d'), RenameRule);
  if PlaceHolderPos > 0 then
    System.Delete(RenameRule, PlaceHolderPos, 2);

  ShowDialog := FShowDialog;
  if not UseRules and (Length(RenameRule) > 0) then
  begin
    ShowDialog := True;
    UseRules := IsDefaultComponentName(Component, CompName);
  end;

  RenameRuleList := GetActiveRenameRuleList;
  if not UseRules or (Length(RenameRule) > 0) then begin
    if ShowDialog or not UseRules then begin
      Index := RenameRuleList.IndexOfName(Component.GetComponentType);
      if Index <> -1 then begin
        OtherProps := RenameRuleList.Objects[Index] as TStringList;
      end else
        OtherProps := nil;
      Result := TfmCompRename.Execute(nil, Component, RenameRule, OtherProps, UseRules,
        IsValidComponentName);
    end else begin
      // Try to find a new name without user interaction
      CompName := '';

      if PlaceHolderPos > 0 then
        Insert('%d', RenameRule, PlaceHolderPos)
      else
        RenameRule := RenameRule + '%d';

      for i := 1 to 100 do
      begin
        SearchName := Format(RenameRule, [i]);
        if GxOtaComponentsAreEqual(Component, FFormEditor.FindComponent(SearchName)) then
          Exit; // The component already matches the rename rule's result

        if IsValidComponentName(CompName, SearchName, Reason) then
        begin
          CompName := SearchName;
          Break;
        end;
      end;
      if Length(CompName) > 0 then
        GxOtaSetComponentName(Component, CompName);

      if UseRules then
      begin
        Index := RenameRuleList.IndexOfName(Component.GetComponentType);
        if Index <> -1 then
        begin
          OtherProps := RenameRuleList.Objects[Index] as TStringList;
          if Assigned(OtherProps) then
          begin
            for i := 0 to OtherProps.Count - 1 do
            begin
              PropName := OtherProps.Names[i];
              if (PropName <> '') and GxOtaPropertyExists(Component, PropName) then
              begin
                PropValue := OtherProps.Values[PropName];
                if PropValue <> '' then
                begin
                  PropValue := AnsiDequotedStr(PropValue, #39);
                  if (PropValue= #39#39) then
                    PropValue := '';
                  GxOtaSetComponentPropertyAsString(Component, PropName, PropValue);
                end;
              end;
            end;
          end
        end;
      end;
    end;
  end
end;

procedure TRenameComponentsExpert.DoOnTimer(Sender: TObject);
var
  i: Integer;
  FormName: WideString;
  SearchName: WideString;
  Component: IOTAComponent;
begin
  FTimer.Enabled := False;
  try
    if (GxOtaGetCurrentFormEditor = nil) then
      Exit;
    Assert(FFormNames.Count = FComponentNames.Count);
    for i := 0 to FComponentNames.Count - 1 do
    begin
      // Try to locate the component with given name
      FormName := FFormNames[i];
      SearchName := FComponentNames[i];
      if (FormName = '') or (SearchName = '') then
        Break;
      FFormEditor := GxOtaGetFormEditorForFileName(FormName);
      if not Assigned(FFormEditor) then
        Break;

      Component := FFormEditor.FindComponent(SearchName);
      if Assigned(Component) then
      begin
        if GxOtaIsInheritedComponent(Component) then
          Break;
        if not DoRename(Component, True) then
          Break;
      end;
    end;
  finally
    FFormEditor := nil;
    Component := nil;
    FComponentNames.Clear;
    FFormNames.Clear;
  end;
end;

procedure TRenameComponentsExpert.FormEditorModified(const FormEditor: IOTAFormEditor);
begin
  // Nothing
end;

function TRenameComponentsExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Rename Components...';
begin
  Result := SMenuCaption;
end;

function TRenameComponentsExpert.GetClassRenameRule(const AClassName: WideString): WideString;
var
  RuleList: TStringList;
begin
  RuleList := GetActiveRenameRuleList;
  Assert(Assigned(RuleList));
  Result := RuleList.Values[AClassName];
end;

function TRenameComponentsExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(VK_F2, [ssShift]);
end;

class function TRenameComponentsExpert.GetName: string;
begin
  Result := COMP_RENAME_NAME;
end;

function TRenameComponentsExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TRenameComponentsExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

class function TRenameComponentsExpert.TryLoadRules(_Settings: IExpertSettings; const _Section: string;
  _TryRoot: Boolean; _RulesList: TStringList): Boolean;

  procedure ReadOtherProps(_Settings: IExpertSettings; const _Section: string; _TryRoot: Boolean;
    _RulesList: TStringList);
  var
    OtherProps: TStringList;
    i: Integer;
  begin
    OtherProps := nil;
    try
      for i := 0 to _RulesList.Count - 1 do begin
        if not Assigned(OtherProps) then
          OtherProps := TStringList.Create;
        _Settings.ReadStrings(_Section + _RulesList.Names[i], OtherProps);
        if _TryRoot and (OtherProps.Count = 0) then begin
          _Settings.ReadStrings(_RulesList.Names[i], OtherProps);
        end;
        if OtherProps.Count > 0 then begin
          _RulesList.Objects[i] := OtherProps;
          OtherProps := nil;
        end;
      end;
    finally
      FreeAndNil(OtherProps);
    end;
  end;

begin
  TStrings_FreeAllObjects(_RulesList).Clear;
  Result := _Settings.SectionExists(_Section);
  if Result then begin
    _Settings.ReadStrings(_Section, _RulesList);
    _RulesList.Sort;
    ReadOtherProps(_Settings, AddSlash(_Section), _TryRoot, _RulesList);
  end;
end;

procedure TRenameComponentsExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  FShowDialog := _Settings.ReadBool('ShowDialog', False);
  FAutoAddClasses := _Settings.ReadBool('AutoAdd', True);

  FFormWidth := _Settings.ReadInteger('Width', 0);
  FFormHeight := _Settings.ReadInteger('Height', 0);

  // new configuration under .\vcl
  if not TryLoadRules(_Settings, 'vcl', True, FRenameRuleListVcl) then begin
    // older configuration under .\Items
    TryLoadRules(_Settings, 'Items', True, FRenameRuleListVcl);
  end;

  TryLoadRules(_Settings, 'fmx', False, FRenameRuleListFmx);
end;

class procedure TRenameComponentsExpert.SaveRules(_Settings: IExpertSettings; const _Section: string; _RulesList: TStringList);
var
  i: Integer;
  SubSection: string;
  OtherProps: TStringList;
begin
  _Settings.WriteStrings(_Section, _RulesList);

  for i := 0 to _RulesList.Count - 1 do begin
    SubSection := AddSlash(_Section) + _RulesList.Names[i];
    OtherProps := _RulesList.Objects[i] as TStringList;
    if Assigned(OtherProps) then
      _Settings.WriteStrings(SubSection, OtherProps)
    else begin
      if _Settings.SectionExists(SubSection) then
        _Settings.EraseSection(SubSection);
    end;
  end;
end;

procedure TRenameComponentsExpert.InternalSaveSettings(_Settings: IExpertSettings);
var
  i: Integer;
  cnt: integer;
  Sections: TStringList;
  s: string;
begin
  inherited InternalSaveSettings(_Settings);
  Assert(Assigned(FRenameRuleListVcl));
  Assert(Assigned(FRenameRuleListFmx));

  // clean up older sub sections which were directly in the section.
  Sections := TStringList.Create;
  try
    _Settings.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
     s := sections[i];
      if not SameText(s, 'vcl') and not SameText(s, 'fmx') then
        _Settings.EraseSection(s);
    end;
  finally
    FreeAndNil(Sections);
  end;

  if _Settings.ValueExists('Count') then begin
    // clean up old entries that were directly in the section, we now write
    // to a subsection
    cnt := _Settings.ReadInteger('Count', 0);
    for i := 0 to cnt - 1 do begin
      _Settings.DeleteKey(Format('Item%d', [i]));
    end;
    _Settings.DeleteKey('Count');
  end;

  _Settings.WriteInteger('Width', FFormWidth);
  _Settings.WriteInteger('Height', FFormHeight);

  _Settings.WriteBool('ShowDialog', FShowDialog);
  _Settings.WriteBool('AutoAdd', FAutoAddClasses);

  SaveRules(_Settings, 'vcl', FRenameRuleListVcl);
  SaveRules(_Settings, 'fmx', FRenameRuleListFmx);
end;

function TRenameComponentsExpert.IsValidComponentName(const OldName, NewName: WideString; var Reason: WideString): Boolean;
resourcestring
  InalidIdent = 'Invalid identifier';
  DuplicateName = 'Duplicate name';
var
  FoundComponent: IOTAComponent;
  FoundName: WideString;
begin
  Reason := '';
  FoundName := '';
  Result := IsValidIdent(NewName);
  if not Result then
    Reason := InalidIdent;
  if Result and Assigned(FFormEditor) then
  begin
    FoundComponent := FFormEditor.FindComponent(NewName);
    if Assigned(FoundComponent) then
      FoundName := GxOtaGetComponentName(FoundComponent);
    Result := (not Assigned(FoundComponent)) or (FoundName = OldName);
    if not Result then
      Reason := DuplicateName;
  end;
end;

procedure TRenameComponentsExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      AddNotifier
    else
      RemoveNotifier;
  end;
end;

procedure TRenameComponentsExpert.AddNotifier;
begin
  if not Assigned(FCompRenameNotifier) then
    FCompRenameNotifier := TCompRenameNotifier.Create(Self);
end;

procedure TRenameComponentsExpert.RemoveNotifier;
begin
  if Assigned(FCompRenameNotifier) then
  begin
    FCompRenameNotifier.Detach;
    FCompRenameNotifier := nil;
  end;
end;

function TRenameComponentsExpert.IsDefaultComponentName(Component: IOTAComponent; const NewName: WideString): Boolean;
var
  Prefix: WideString;
  Suffix: WideString;
begin
  Assert(Assigned(Component));
  Result := False;
  Prefix := Component.GetComponentType;
  if (Length(Prefix) > 1) and (Prefix[1] = 'T') then
    Prefix := Copy(Prefix, 2);
  if not StartsText(Prefix, NewName) then
    Exit;
  Suffix := Copy(NewName, Length(Prefix) + 1);
  if StrToIntDef(Suffix, -999) <> -999 then
    Result := True;
end;

function TRenameComponentsExpert.IsDefaultActive: Boolean;
begin
  // IDE Bug: This expert does not work under Delphi 8
  Result := not RunningDelphi8;
end;

procedure TfmCompRename.InitializeForm;
begin
  SetModalFormPopupMode(Self);
  lblReason.Font.Color := clRed;
  if IsThemingEnabled
  then pc_Additional.Style := tsButtons
  else pc_Additional.Style := tsTabs;
end;

procedure TfmCompRename.ArrangeControls;
var
  Spacer : Integer;
  H, W   : Integer;
begin
  inherited;
  Spacer := FScaler.Calc(2);
  FAlignButtons[alTop].Anchors := [akLeft, akTop];

  FAlignButtons[alLeft].Anchors := [akLeft, akTop];
  FAlignButtons[alLeft].Left := FAlignButtons[alTop].Left;
  FAlignButtons[alLeft].Top  := FAlignButtons[alTop].Top + FAlignButtons[alTop].Height + Spacer;

  W := FAlignButtons[alTop].Width - (2 * FAlignButtons[alLeft].Width) - (2 * Spacer);
  H := FAlignButtons[alLeft].Height;

  FAlignButtons[alClient].Anchors := [akLeft, akTop];
  FAlignButtons[alClient].Top := FAlignButtons[alLeft].Top;
  FAlignButtons[alClient].Width := W;
  FAlignButtons[alClient].Left  := FAlignButtons[alLeft].Left + FAlignButtons[alLeft].Width + Spacer;
  FAlignButtons[alClient].Height := H;

  FAlignButtons[alRight].Anchors := [akLeft, akTop];
  FAlignButtons[alRight].Left  := FAlignButtons[alClient].Left + FAlignButtons[alClient].Width + Spacer;
  FAlignButtons[alRight].Top   := FAlignButtons[alLeft].Top;
  FAlignButtons[alRight].Width := FAlignButtons[alLeft].Width;

  FAlignButtons[alBottom].Anchors := [akLeft, akTop];
  FAlignButtons[alBottom].Top   := FAlignButtons[alClient].Top + FAlignButtons[alClient].Height + Spacer;
  FAlignButtons[alBottom].Left  := FAlignButtons[alTop].Left;
  FAlignButtons[alBottom].Width := FAlignButtons[alTop].Width;

  FAlignButtons[alNone].Anchors := [akLeft, akTop];
  FAlignButtons[alNone].Left    := FAlignButtons[alLeft].Left;
  FAlignButtons[alNone].Top     := FAlignButtons[alBottom].Top + FAlignButtons[alBottom].Height + (2 * Spacer);

  FAlignButtons[alCustom].Anchors := [akLeft, akTop];
  FAlignButtons[alCustom].Top     := FAlignButtons[alNone].Top;
  FAlignButtons[alCustom].Width   := FAlignButtons[alNone].Width;
  FAlignButtons[alCustom].Left    := FAlignButtons[alTop].Left + FAlignButtons[alTop].Width - FAlignButtons[alCustom].Width;
end;

procedure TfmCompRename.btnSettingsClick(Sender: TObject);
begin
  Assert(Assigned(PrivateCompRenameExpert));
  PrivateCompRenameExpert.Configure(Self, FComponentClassName);
end;

procedure TfmCompRename.HandleAlignButtons(_Align: TAlign);
var
  al: TAlign;
begin
  if FAlignButtons[_Align].Down then begin
    for al := Low(FAlignButtons) to High(FAlignButtons) do
      if al <> _Align then
        FAlignButtons[al].Down := False;
  end
  else
    FAlignButtons[_Align].Down := True;
end;

procedure TfmCompRename.b_AlignBottomClick(Sender: TObject);
begin
  HandleAlignButtons(alBottom);
end;

procedure TfmCompRename.b_AlignClientClick(Sender: TObject);
begin
  HandleAlignButtons(alClient);
end;

procedure TfmCompRename.b_AlignCustomClick(Sender: TObject);
begin
  HandleAlignButtons(alCustom);
end;

procedure TfmCompRename.b_AlignLeftClick(Sender: TObject);
begin
  HandleAlignButtons(alLeft);
end;

procedure TfmCompRename.b_AlignNoneClick(Sender: TObject);
begin
  HandleAlignButtons(alNone);
end;

procedure TfmCompRename.b_AlignRightClick(Sender: TObject);
begin
  HandleAlignButtons(alRight);
end;

procedure TfmCompRename.b_AlignTopClick(Sender: TObject);
begin
  HandleAlignButtons(alTop);
end;

procedure TfmCompRename.SetMargins(_Value: integer);
var
  s: string;
begin
  s := IntToStr(_Value);
  ed_MarginTop.Text := s;
  ed_MarginLeft.Text := s;
  ed_MarginRight.Text := s;
  ed_MarginBottom.Text := s;
end;

procedure TfmCompRename.b_Margins0Click(Sender: TObject);
begin
  SetMargins(0);
end;

procedure TfmCompRename.b_Margins3Click(Sender: TObject);
begin
  SetMargins(3);
end;

procedure TfmCompRename.b_Margins6Click(Sender: TObject);
begin
  SetMargins(6);
end;

procedure TfmCompRename.b_Margins8Click(Sender: TObject);
begin
  SetMargins(8);
end;

initialization
  RegisterGX_Expert(TRenameComponentsExpert);

end.

