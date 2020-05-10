unit GX_SelectComponents;

{$I GX_CondDefine.inc}

// TODO: Prevent selecting a VCL form + components, since it isn't actually possible

// todo: It's not possible to select the form (that is: not select any component) with this
// expert. Either find a way to implement this (the IDE's structure pane can do it)
// or remove the form from the tree view.

interface

uses
  Classes, Forms, Controls, ExtCtrls, ToolsAPI, ComCtrls, StdCtrls, Dialogs,
  ActnList, Actions, ImgList, Graphics, Buttons, GX_BaseForm,
  GX_GenericUtils;

type
  TComponentInfo = record
    rName: TGXUnicodeString;
    rType: TGXUnicodeString;
  end;

  TSelectComponentsForm = class(TfmBaseForm)
    TreeView: TTreeView;
    ActionList: TActionList;
    FindPanel: TPanel;
    SearchEdit: TEdit;
    StayOnTopCheckBox: TCheckBox;
    SelectAllButton: TBitBtn;
    SelectAllAction: TAction;
    BottomPanel: TPanel;
    ExactNameCheckBox: TCheckBox;
    ExactTypeCheckBox: TCheckBox;
    TreePanel: TPanel;
    ResizeButton: TBitBtn;
    ChangeModeAction: TAction;
    FilterLabel: TLabel;
    procedure TreeViewClick(aSender: TObject);
    procedure TreeViewKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure SearchEditChange(aSender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure StayOnTopCheckBoxClick(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var aKey: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure SearchEditKeyPress(Sender: TObject; var aKey: Char);
    procedure ExactCheckBoxClick(Sender: TObject);
    procedure ChangeModeActionExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ChangeModeActionHint(var HintStr: String; var CanShow: Boolean);
  private
    FIsActivaingForm: Boolean;
    FFilter: TComponentInfo;
    FMatchingNodes: TList;
    FCurrentNode: TTreeNode;
    FFormEditor: IOTAFormEditor;
    FMiniMode: Boolean;
    FStayOnTop: Boolean;
    FLastHeight: Integer;
    FLastComponentName: TGXUnicodeString;

    procedure Init;
    procedure FocusSearchEdit;
    procedure ChangeMode(const aMiniMode: Boolean);

    procedure ChildComponentCallback(aParam: Pointer; aComponent: IOTAComponent; var aResult: Boolean);
    procedure SelectCurrentComponent;
    procedure FillTreeView(const aFromComponent: IOTAComponent);

    procedure SelectComponentOnForm(const aName: TGXUnicodeString; const aAddToSelection: Boolean = False);

    procedure SetCurrentNode(const aNode: TTreeNode);
    procedure FindNextNode;
    procedure FindPrevNode;
    procedure FilterNodes(const aFilter: TComponentInfo);
    procedure SetStayOnTop(const aStayOnTop: Boolean);
  protected
    property CurrentNode: TTreeNode read FCurrentNode write SetCurrentNode;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;

    property MiniMode: Boolean read FMiniMode  write ChangeMode;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop;
    property LastHeight: Integer read FLastHeight;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Messages, TypInfo,
  u_dzStringUtils, u_dzVclUtils,
  GX_Experts, GX_OtaUtils, GX_SharedImages;

type
  TComponentSelectExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(aAction: TCustomAction); override;
  public
    class function GetName: string; override;

    function GetActionCaption: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

var
  TheForm: TSelectComponentsForm;

procedure GetInfo(const aTreeNode: TTreeNode; const aGetType: Boolean; var aInfo: TComponentInfo); overload;
var
  p: Integer;
begin
  aInfo.rName := aTreeNode.Text;
  p := Pos(' : ', aInfo.rName);

  if p > 0 then
  begin
    if aGetType then
      aInfo.rType := Copy(aInfo.rName, p + 3);
    aInfo.rName := Copy(aInfo.rName, 1, p - 1);
  end;
end;

function GetInfo(const aText: TGXUnicodeString): TComponentInfo; overload;
var
  p: Integer;
begin
  Result.rName := aText;
  p  := Pos(':', Result.rName);

  if p > 0 then
  begin
    Result.rType := Trim(Copy(Result.rName, p + 1));
    Result.rName := Trim(Copy(Result.rName, 1, p - 1));
  end;
end;

function FilterToText(const aFilter: TComponentInfo) : TGXUnicodeString;
begin
  Result := aFilter.rName;

  if aFilter.rType <> '' then
    Result := Result + ':' + aFilter.rType;
end;

{ TSelectComponentsForm }

constructor TSelectComponentsForm.Create(_Owner: TComponent);
begin
  inherited;
  FMatchingNodes := TList.Create;
end;

destructor TSelectComponentsForm.Destroy;
begin
  FreeAndNil(FMatchingNodes);
  inherited;
end;

procedure TSelectComponentsForm.SelectComponentOnForm(const aName: TGXUnicodeString;
  const aAddToSelection: Boolean);
var
  cmp: IOTAComponent;
begin
  cmp := FFormEditor.FindComponent(aName);
  TreeView.MultiSelect := aAddToSelection;

  if Assigned(cmp) then
  begin
    FLastComponentName := aName;
    cmp.Select(aAddToSelection);
  end;
end;

procedure TSelectComponentsForm.SelectCurrentComponent;
var
  Info: TComponentInfo;
begin
  if not FIsActivaingForm and Assigned(FFormEditor) and Assigned(TreeView.Selected) then
  begin
    GetInfo(TreeView.Selected, False, Info);
    SelectComponentOnForm(Info.rName);
  end;
end;

procedure TSelectComponentsForm.SelectAllActionExecute(Sender: TObject);
var
  Idx: Integer;
  Node: TTreeNode;
  Info: TComponentInfo;
begin
  for Idx := 0 to Pred(FMatchingNodes.Count) do
  begin
    Node := FMatchingNodes [Idx];
    GetInfo(Node, False, Info);
    SelectComponentOnForm(Info.rName, Idx > 0);
  end;

  TreeView.Select(FMatchingNodes);
end;

procedure TSelectComponentsForm.SetCurrentNode(const aNode: TTreeNode);
begin
  if FMatchingNodes.Count > 0 then
  begin
    FCurrentNode := aNode;
    if not Assigned(FCurrentNode) or (FMatchingNodes.IndexOf(FCurrentNode) < 0) then
      FCurrentNode := FMatchingNodes.First;
    TreeView.Select(FCurrentNode);
    SelectCurrentComponent;
  end
  else FCurrentNode := nil;

  if Assigned(FCurrentNode) then
    SearchEdit.Font.Color := clBlack
  else
    SearchEdit.Font.Color := clRed;
end;

procedure TSelectComponentsForm.ChangeMode(const aMiniMode: Boolean);
var
  BestFitHeight: Integer;
begin
  if aMiniMode = FMiniMode then
    Exit;

  FMiniMode := aMiniMode;

  if FMiniMode then
    FLastHeight := Height;

  TreePanel.Visible := not FMiniMode;

  if FMiniMode then begin
    ChangeModeAction.ImageIndex := ImageIndexExpand;
    // The above fails to change the image in D6 at least
    ResizeButton.Glyph.Assign(nil);
    GetSharedImageList.GetBitmap(ImageIndexExpand, ResizeButton.Glyph);
    BestFitHeight := Height - ClientHeight + FindPanel.Height;
    Constraints.MinHeight := BestFitHeight;
    Constraints.MaxHeight := BestFitHeight;
    ClientHeight := FindPanel.Height;
  end
  else
  begin
    Constraints.MinHeight := 175;
    Constraints.MaxHeight := 0;
    Height := FLastHeight;
    ChangeModeAction.ImageIndex := ImageIndexContract;
    ResizeButton.Glyph.Assign(nil);
    GetSharedImageList.GetBitmap(ImageIndexContract, ResizeButton.Glyph);
  end;

  FocusSearchEdit;
end;

procedure TSelectComponentsForm.ChangeModeActionExecute(Sender: TObject);
begin
  MiniMode := not MiniMode;
end;

procedure TSelectComponentsForm.ChildComponentCallback(aParam: Pointer;
  aComponent: IOTAComponent; var aResult: Boolean);
var
  Node: TTreeNode;
  CmpName: TGXUnicodeString;
begin
  CmpName := GxOtaGetComponentName(aComponent);
  Node := TreeView.Items.AddChildObject(TTreeNode(aParam), CmpName + ' : ' + aComponent.GetComponentType, nil);
  aComponent.GetChildren(Node, ChildComponentCallback);
  aResult := True;
end;

procedure TSelectComponentsForm.ExactCheckBoxClick(Sender: TObject);
begin
  FocusSearchEdit;
  FilterNodes(FFilter);
end;

procedure TSelectComponentsForm.FillTreeView(const aFromComponent: IOTAComponent);
begin
  if TreeView.Items.GetFirstNode <> nil then
    aFromComponent.GetChildren(TreeView.Items.GetFirstNode, ChildComponentCallback);
end;

procedure TSelectComponentsForm.FilterNodes(const aFilter: TComponentInfo);
var
  ByName: Boolean;
  ByType: Boolean;
  ExactName: Boolean;
  ExactType: Boolean;
  IsNameMatch: Boolean;
  IsTypeMatch: Boolean;
  NodeIdx: Integer;
  Node: TTreeNode;
  aInfo: TComponentInfo;
  Found: Boolean;
begin
  FMatchingNodes.Clear;

  TreeView.Items.BeginUpdate;
  try
    ByName := aFilter.rName <> '';
    ByType := aFilter.rType <> '';

    ExactName := ExactNameCheckBox.Checked;
    ExactType := ExactTypeCheckBox.Checked;

    for NodeIdx := 0 to Pred(TreeView.Items.Count) do
    begin
      Node := TreeView.Items [NodeIdx];
      GetInfo(Node, ByType, aInfo);

      IsNameMatch := ByName and
        (not ExactName and (Pos(UpperCase(aFilter.rName), UpperCase(aInfo.rName)) > 0) or
        (ExactName and SameText(aFilter.rName, aInfo.rName)));
                        
      IsTypeMatch := ByType and
        (not ExactType and (Pos(UpperCase(aFilter.rType), UpperCase(aInfo.rType)) > 0) or
        (ExactType and SameText(aFilter.rType, aInfo.rType)));

      Found := (ByName and not ByType and IsNameMatch) or
        (not ByName and ByType and IsTypeMatch) or
        (ByName and ByType and IsNameMatch and IsTypeMatch);

      if Found then
        FMatchingNodes.Add(Node);

      if Found then // Images disabled for now since D6 fails to show the right images, set StateIndex as well
        Node.ImageIndex := ImageIndexArrow
      else
        Node.ImageIndex := -1;
    end;
  finally
    TreeView.Items.EndUpdate;
  end;

  CurrentNode := CurrentNode;
end;

procedure TSelectComponentsForm.SearchEditChange(aSender: TObject);
begin
  FFilter := GetInfo(SearchEdit.Text);

  FilterNodes(FFilter);
end;

procedure TSelectComponentsForm.SearchEditKeyPress(Sender: TObject; var aKey: Char);
var
  IgnoreKey : Boolean;
begin
  IgnoreKey := True;

  case Ord(aKey) of
    VK_RETURN: SelectAllAction.Execute;
    VK_ESCAPE: ;
    VK_SPACE: SearchEdit.Clear;
    else
      IgnoreKey := False;
  end;

  if IgnoreKey then
    aKey := Chr(0);
end;

procedure TSelectComponentsForm.SearchEditKeyUp(Sender: TObject; var aKey: Word; Shift: TShiftState);
var
  IgnoreKey: Boolean;
begin
  IgnoreKey := True;

  case aKey of
    VK_UP:
      FindPrevNode;
    VK_DOWN:
      FindNextNode;
    else
      IgnoreKey := False;
  end;

  if IgnoreKey then
    aKey := 0;
end;

procedure TSelectComponentsForm.FindNextNode;
var
  Idx : Integer;
begin
  if FMatchingNodes.Count <= 0 then
  begin
    CurrentNode := nil;
    Exit;
  end;

  Idx := FMatchingNodes.IndexOf(CurrentNode);

  if (Idx > -1) and (Idx < Pred(FMatchingNodes.Count)) then
  begin
    Inc(Idx);
    CurrentNode := FMatchingNodes[Idx];
  end
  else
    CurrentNode := FMatchingNodes.First;
end;

procedure TSelectComponentsForm.FindPrevNode;
var
  Idx: Integer;
begin
  if FMatchingNodes.Count <= 0 then
  begin
    CurrentNode := nil;
    Exit;
  end;

  Idx := FMatchingNodes.IndexOf(CurrentNode);

  if (Idx > 0) and (Idx < FMatchingNodes.Count) then
  begin
    Dec(Idx);
    CurrentNode := FMatchingNodes[Idx];
  end
  else
    CurrentNode := FMatchingNodes.Last;
end;

procedure TSelectComponentsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(VK_ESCAPE) then
  begin
    Key := #0;
    Close;
  end;
end;

procedure TSelectComponentsForm.FocusSearchEdit;
begin
  TryFocusControl(SearchEdit);
end;

procedure TSelectComponentsForm.FormActivate(Sender: TObject);
var
  CmpName: TGXUnicodeString;
  Info: TComponentInfo;
  Idx: Integer;
  Node: TTreeNode;
begin
  FIsActivaingForm := True;
  try
    try
      Init;
      CmpName := FLastComponentName;

      FocusSearchEdit;
      SearchEdit.Text := FilterToText(FFilter);
      SearchEdit.SelectAll;
      SearchEditChange(SearchEdit);

      for Idx := 0 to Pred(TreeView.Items.Count) do
      begin
        Node := TreeView.Items[Idx];
        GetInfo(Node, False, Info);
        if SameText(CmpName, Info.rName) then
        begin
          CurrentNode := Node;
          Exit;
        end;
      end;
    except
    end;
  finally
    FIsActivaingForm := False;
  end;
end;

procedure TSelectComponentsForm.Init;
var
  ParentName: TGXUnicodeString;
  ParentType: TGXUnicodeString;
  Root: IOTAComponent;
begin
  // Even though we use Begin/EndUpdate for the Items, the tree view still flickers a lot.
  // To prevent this, we could use LockWindowUpdate, which prevents this but it is strongly
  // discouraged. Unfortunately TWinControl_Lock does not work on the treeview, but it works
  // on the panel which contains the treeview.
  TWinControl_Lock(TreePanel);
  TreeView.Items.BeginUpdate;
  try
    FMatchingNodes.Clear;

    SearchEdit.Enabled := False;
    TreeView.Items.Clear;

    if not GxOtaTryGetCurrentFormEditor(FFormEditor) then
      Abort;

    Root := FFormEditor.GetRootComponent;
    if not Assigned(Root) then
      Abort;

    ParentType := Root.GetComponentType;
    ParentName := GxOtaGetComponentName(Root);

    TreeView.Items.Add(nil, ParentName + ' : ' + ParentType);

    FillTreeView(Root);
    TreeView.FullExpand;
    TreeView.Selected := TreeView.Items.GetFirstNode;
    TreeView.Selected.MakeVisible;

    SearchEdit.Enabled := True;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TSelectComponentsForm.SetStayOnTop(const aStayOnTop : Boolean);
begin
  if aStayOnTop = FStayOnTop then
    Exit;

  FStayOnTop := aStayOnTop;

  if FStayOnTop then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;

  StayOnTopCheckBox.Checked := FStayOnTop;

  FocusSearchEdit;
end;

procedure TSelectComponentsForm.StayOnTopCheckBoxClick(Sender: TObject);
begin
  StayOnTop := StayOnTopCheckBox.Checked;
end;

procedure TSelectComponentsForm.TreeViewClick(aSender: TObject);
begin
  SelectCurrentComponent;
end;

procedure TSelectComponentsForm.TreeViewKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  SelectCurrentComponent;
end;

procedure TSelectComponentsForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  SelectAllAction.Enabled := FMatchingNodes.Count > 0;
  if SelectAllAction.Enabled then
    SelectAllAction.Caption := '&Select ' + IntToStr(FMatchingNodes.Count)
  else
    SelectAllAction.Caption := 'Select';
end;

procedure TSelectComponentsForm.ChangeModeActionHint(var HintStr: String; var CanShow: Boolean);
begin
  if FMiniMode then
    HintStr := 'Expand'
  else
    HintStr := 'Contract';
end;

{ TComponentSelectExpert }

procedure TComponentSelectExpert.UpdateAction(aAction: TCustomAction);
begin
  aAction.Enabled := GxOtaCurrentlyEditingForm;
end;

procedure TComponentSelectExpert.Execute(Sender: TObject);
begin
  if not Assigned(TheForm) then
    TheForm := TSelectComponentsForm.Create(nil);

  TheForm.Show;

  IncCallCount;
end;

function TComponentSelectExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Select Components...';
begin
  Result := SMenuCaption;
end;

class function TComponentSelectExpert.GetName: string;
begin
  Result := 'SelectComponents';
end;

function TComponentSelectExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TComponentSelectExpert);

finalization
  FreeAndNil(TheForm);

end.
