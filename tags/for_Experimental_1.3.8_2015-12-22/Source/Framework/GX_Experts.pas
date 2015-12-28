unit GX_Experts;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Graphics, Forms, ActnList, GX_Actions, GX_ConfigurationInfo, Menus;

type
  TGX_Expert = class(TObject)
  private
    FBitmap: TBitmap;
    FActive: Boolean;
    FShortCut: TShortCut;
    FAction: IGxAction;
    procedure SetShortCut(Value: TShortCut);
    function GetBitmap: TBitmap;
    procedure ActionOnUpdate(Sender: TObject);
  protected
    function GetExpertIndex: Integer;
    function BitmapFileName: string; virtual;
    procedure SetFormIcon(Form: TForm);
    procedure SetActive(New: Boolean); virtual;
    procedure UpdateAction(Action: TCustomAction); virtual;
    function HasSubmenuItems: Boolean; virtual; // Default = False
    // See LoadSettings
    procedure InternalLoadSettings(Settings: TGExpertsSettings); virtual;
    // See SaveSettings
    procedure InternalSaveSettings(Settings: TGExpertsSettings); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Information functions that need to be overriden
    // by each expert to provide required registration
    // information.
    // Caption of a menu item entry.
    function GetActionCaption: string; virtual; abstract;
    // Determine if the expert action is enabled
    function GetActionEnabled: Boolean; virtual;
    // Internal name of expert for expert identification.
    class function GetName: string; virtual; {$IFNDEF GX_BCB} abstract; {$ENDIF}
    // Subkey used to store configuration details in the registry
    class function ConfigurationKey: string; virtual;
    // Name of action to be created for expert; by default,
    // the action name is constructed from the expert's name.
    function GetActionName: string;
    // Name to be displayed for the expert in the GExperts
    // *configuration* dialog; this is a different entry than
    // the action caption (GetActionCaption)
    function GetDisplayName: string; virtual;
    // Various expert configuration items which
    // are only used during registration.
    function HasConfigOptions: Boolean; virtual; // Default = True
    function HasMenuItem: Boolean; virtual; // Default = True
    function HasDesignerMenuItem: Boolean; virtual; // Default = False
    procedure DoCreateSubMenuItems(MenuItem: TMenuItem);
    procedure CreateSubMenuItems(MenuItem: TMenuItem); virtual;
    function IsDefaultActive: Boolean; virtual; // Default = True
    // "Executes" the expert
    procedure Click(Sender: TObject); virtual; abstract;
    // Do any delayed setup after the IDE is done initializing
    procedure AfterIDEInitialized; virtual;
    // Various methods that will be called
    // at appropriate times
    procedure Configure; virtual;
    // Request to the expert to (re)load all its serialized state; calls InternalLoadSettings
    // Note: This is called *automatically* by the framework.
    procedure LoadSettings;
    // Saves expert data; calls InternalSaveSettings
    // NOT called automatically by the framework.
    procedure SaveSettings;
    // Update the action state
    procedure DoUpdateAction;

    // Various state information:
    // Is expert active?
    property Active: Boolean read FActive write SetActive;
    // Index of expert; used to determine a "historic"
    // menu item order in the GExperts menu item.
    property ExpertIndex: Integer read GetExpertIndex;
    // Bitmap associated with the expert; note that this
    // bitmap is loaded and unloaded on demand; you will
    // have to .Assign the bitmap to get a permanent copy.
    property Bitmap: Graphics.TBitmap read GetBitmap;
    // Keyboard shortcut associated with the expert
    property ShortCut: TShortCut read FShortCut write SetShortCut;
  end;

  TGX_ExpertClass = class of TGX_Expert;

var
  GX_ExpertList: TList = nil;
  ExpertIndexLookup: TStringList = nil;

procedure RegisterGX_Expert(AClass: TGX_ExpertClass);
function GetGX_ExpertClassByIndex(const Index: Integer): TGX_ExpertClass;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Dialogs,
  GX_MenuActions, GX_MessageBox, GX_IconMessageBox,
  GX_GxUtils, GX_OtaUtils, GX_GenericUtils;

{ TGX_Expert }

procedure TGX_Expert.ActionOnUpdate(Sender: TObject);
begin
  DoUpdateAction;
end;

procedure TGX_Expert.Configure;
resourcestring
  SNoConfigOptions = 'No configuration for this expert is available.';
begin
  MessageDlg(SNoConfigOptions, mtInformation, [mbOK], 0);
end;

// Note: Don't call LoadSettings in Create.  This is done for you
// when the expert is created.  See TGExperts.InstallAddIn.
constructor TGX_Expert.Create;
begin
  inherited Create;

  // Don't set Active to True.
  // Instead override IsDefaultActive and let LoadSettings do it
  FShortCut := 0;
end;

procedure TGX_Expert.CreateSubMenuItems(MenuItem: TMenuItem);
begin
  // Override to create any submenu items in the main menu
end;

destructor TGX_Expert.Destroy;
begin
  // Set active to False, this makes it possible to handle all creation and
  // destruction inside SetActive
  Active := False;

  FreeAndNil(FBitmap);

  inherited Destroy;
end;

function TGX_Expert.GetActionName: string;
begin
  // Default action name from expert name; do not localize.
  Result := 'GX_' + GetName + 'Action';
end;

function TGX_Expert.GetExpertIndex: Integer;
var
  Index: Integer;
begin
  Result := MaxInt - 10000;
  if ExpertIndexLookup.Find(ClassName, Index) then
    Result := Integer(ExpertIndexLookup.Objects[Index]);
end;

function TGX_Expert.GetBitmap: Graphics.TBitmap;
var
  BitmapFile: string;
begin
  if not Assigned(FBitmap) then
  begin
    // Locate an icon for the expert and load it
    // if it exists.
    BitmapFile := BitmapFileName;
    if BitmapFile <> '' then
    begin
      if not GxLoadBitmapForExpert(BitmapFile, FBitmap) then
      begin
        {$IFOPT D+} SendDebug('Missing bitmap ' + BitmapFile + ' for ' + Self.ClassName); {$ENDIF}
        ShowGxMessageBox(TShowMissingIconMessage, ChangeFileExt(BitmapFile, '.bmp'));
      end;
    end
    else
    begin
      {$IFOPT D+} SendDebugError('Bitmap name missing for expert ' + Self.ClassName); {$ENDIF D+}
    end;
  end;

  Result := FBitmap;
end;

function TGX_Expert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TGX_Expert.HasMenuItem: Boolean;
begin
  Result := True;
end;

function TGX_Expert.HasSubmenuItems: Boolean;
begin
  Result := False;
end;

function TGX_Expert.BitmapFileName: string;
begin
  Result := GetName;
end;

function TGX_Expert.IsDefaultActive: Boolean;
begin
  Result := True;
end;

procedure TGX_Expert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  // Empty base routine to allow the safe usage of "inherited" in descendants
end;

procedure TGX_Expert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  // Empty base routine to allow the safe usage of "inherited" in descendants
end;

const
  ShortCutIdent = 'ExpertShortcuts'; // Do not localize.
  EnabledIdent = 'EnabledExperts'; // Do not localize.

procedure TGX_Expert.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create;
  try
    // Do not put these two Settings.xxx lines in InternalLoadSettings,
    // since a descendant might forget to call 'inherited'
    ShortCut := Settings.ReadInteger(ShortCutIdent, GetName, ShortCut);
    Active := Settings.ReadBool(EnabledIdent, GetName, IsDefaultActive);

    InternalLoadSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGX_Expert.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create;
  try
    // Do not put these two Settings.xxx lines in InternalSaveSettings,
    // since a descendant might forget to call 'inherited'
    Settings.WriteBool(EnabledIdent, GetName, Active);
    Settings.WriteInteger(ShortCutIdent, GetName, ShortCut);

    InternalSaveSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGX_Expert.SetActive(New: Boolean);
begin
  if New = FActive then
    Exit;

  FActive := New;

  if HasMenuItem then
  begin
    if New and not IsStandAlone then
      FAction := GXMenuActionManager.RequestMenuExpertAction(Self)
    else
      FAction := nil;
  end;

  if Assigned(FAction) then
    FAction.OnUpdate := ActionOnUpdate;
end;

procedure TGX_Expert.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
  if Assigned(FAction) then
    FAction.ShortCut := FShortCut;
end;

{ Globals }

function GetGX_ExpertClass(const ClassName: string): TGX_ExpertClass;
var
  i: Integer;
begin
  Assert(GX_ExpertList <> nil, 'Uses clauses are out of order.  GX_ExpertList is nil!');

  for i := 0 to GX_ExpertList.Count - 1 do
  begin
    Result := GX_ExpertList[i];
    if Result.ClassNameIs(ClassName) then Exit;
  end;
  Result := nil;
end;

function GetGX_ExpertClassByIndex(const Index: Integer): TGX_ExpertClass;
begin
  Result := nil;
  if (Index >= 0) and (Index <= GX_ExpertList.Count - 1) then
    Result := GX_ExpertList[Index];
end;

procedure RegisterGX_Expert(AClass: TGX_ExpertClass);
var
  ExpertClassName: string;
begin
  ExpertClassName := AClass.ClassName;
  {$IFOPT D+} SendDebug('Registering expert: ' +  ExpertClassName); {$ENDIF D+}
  if GetGX_ExpertClass(ExpertClassName) <> nil then
  begin
    Assert(False, 'Duplicate call to RegisterGX_Expert for ' + ExpertClassName);
    Exit;
  end;
  GX_ExpertList.Add(AClass);
end;

procedure InitExpertIndexLookup;
const
  OldExpertOrder: array [0..29] of string = (
  'TProcedureExpert',
  'TExpertManagerExpert',
  'TGrepDlgExpert',
  'TGrepExpert',
  'TMsgExpExpert',
  'TBackupExpert',
  'TTabExpert',
  'TCleanExpert',
  'TClipExpert',
  'TFilesExpert',
  'TClassExpert',
  'TSourceExportExpert',
  'TCodeLibExpert',
  'TASCIIExpert',
  'TPEExpert',
  'TReplaceCompExpert',
  'TGridExpert',
  'TShortCutExpert',
  'TDependExpert',
  'TLayoutExpert',
  'TToDoExpert',
  'TCodeProofreaderExpert',
  'TProjOptionSetsExpert',
  'TCompsToCodeExpert',
  'TCompRenameExpert',
  'TCopyComponentNamesExpert',
  'TGxMenusForEditorExperts',
  'TMacroLibExpert',
  'TOpenFileExpert',
  'TFindCompRefWizard'
  );
var
  i: Integer;
begin
  Assert(not Assigned(ExpertIndexLookup));
  ExpertIndexLookup := TStringList.Create;
  for i := Low(OldExpertOrder) to High(OldExpertOrder) do
    ExpertIndexLookup.AddObject(OldExpertOrder[i], TObject(i));
  ExpertIndexLookup.Sorted := True;
end;

procedure TGX_Expert.SetFormIcon(Form: TForm);
begin
  Assert(Assigned(Form));
  if Assigned(Bitmap) then
    ConvertBitmapToIcon(Bitmap, Form.Icon);
end;

function TGX_Expert.HasDesignerMenuItem: Boolean;
begin
  Result := False;
end;

function TGX_Expert.GetActionEnabled: Boolean;
begin
  Result := FAction.GetEnabled;
end;

procedure TGX_Expert.DoCreateSubMenuItems(MenuItem: TMenuItem);
begin
  if HasSubMenuItems then
    if Assigned(MenuItem) and (MenuItem.Count = 0) then
      CreateSubMenuItems(MenuItem);
end;

procedure TGX_Expert.DoUpdateAction;
begin
  UpdateAction(FAction.GetAction);
end;

function TGX_Expert.GetDisplayName: string;
begin
  Result := StringReplace(GetActionCaption, '...', '', [rfReplaceAll]);
  Result := StripHotkey(Result);
end;

class function TGX_Expert.ConfigurationKey: string;
begin
  Result := GetName;
end;

{$IFDEF GX_BCB}
class function TGX_Expert.GetName: string;
begin
  Result := ClassName;
end;
{$ENDIF}

procedure TGX_Expert.UpdateAction(Action: TCustomAction);
begin
  // Update Enabled, Visible, Caption, etc.
end;

procedure TGX_Expert.AfterIDEInitialized;
begin
  // Do any delayed setup here that needs some later-created IDE items
end;

initialization
  GX_ExpertList := TList.Create;
  InitExpertIndexLookup;

finalization
  FreeAndNil(GX_ExpertList);
  FreeAndNil(ExpertIndexLookup);

end.

