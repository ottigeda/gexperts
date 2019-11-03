unit GX_GExperts;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, ToolsAPI, Controls, ExtCtrls,
  GX_EditorExpertManager, GX_Experts;

type
  TGExperts = class(TNotifierObject, IOTANotifier, IOTAWizard)
  private
    FEditorExpertsManager: TGxEditorExpertManager;
    FExpertList: TList;
    FStartingUp: Boolean;
{$IFDEF STARTUP_LAYOUT_FIX_ENABLED}
    FLastDesktopName: string;
{$ENDIF}
    procedure InstallAddIn;
    function GetExpert(const Index: Integer): TGX_Expert;
    function GetExpertCount: Integer;
    procedure InitializeGExperts;
    procedure OnCloseMessageViewTimer(_Sender: TObject);
  protected
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadEditorExperts;
    procedure FreeEditorExperts;

    function FindExpert(const ExpertName: string; out Idx: Integer): boolean;

    property EditorExpertManager: TGxEditorExpertManager read FEditorExpertsManager;
    property ExpertList[const Index: Integer]: TGX_Expert read GetExpert;
    property ExpertCount: Integer read GetExpertCount;
    property StartingUp: Boolean read FStartingUp;
    procedure RefreshExpertShortCuts;
    procedure ShowConfigurationForm;
    procedure DoAfterIDEInitialized(Sender: TObject);

    function GetSharedImages: TImageList;
    function GetExpertList: TList;
    procedure TimedCloseMessageView;
  end;

///<summary>
/// @param CheckValid, if true, raises an exceptoin if the instance is NIL </summary>
function GExpertsInst(CheckValid: Boolean = False): TGExperts;
procedure ShowGXAboutForm;
procedure ShowGXConfigurationForm;
procedure InitSharedResources;
procedure FreeSharedResources;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Dialogs, Forms,
  GX_GenericUtils, GX_GetIdeVersion, GX_About, GX_MenuActions, GX_MessageBox,
  GX_ConfigurationInfo, GX_Configure, GX_KbdShortCutBroker, GX_SharedImages,
  GX_IdeUtils, GX_IdeEnhance, GX_EditorChangeServices, GX_ToolbarDropDown,
  GX_TimedCallback;

type
  TUnsupportedIDEMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

var
  FPrivateGExpertsInst: TGExperts = nil;
  SharedImages: TdmSharedImages = nil;

function GExpertsInst(CheckValid: Boolean): TGExperts;
begin
  if CheckValid and (not Assigned(FPrivateGExpertsInst)) then
    raise Exception.Create('GExpertsInst is not a valid reference');
  Result := FPrivateGExpertsInst;
end;

procedure ShowGXAboutForm;
begin
  with gblAboutFormClass.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure ShowGXConfigurationForm;
begin
  with TfmConfiguration.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure InitSharedResources;
begin
  if not Assigned(SharedImages) then
    SharedImages := TdmSharedImages.Create(nil);
end;

procedure FreeSharedResources;
begin
  {$IFOPT D+} SendDebug('Freeing shared images'); {$ENDIF}
  FreeAndNil(SharedImages);
end;

{ TGExperts }

constructor TGExperts.Create;
begin
  {$IFOPT D+} SendDebug('TGExperts.Create'); {$ENDIF}
  inherited Create;
  FStartingUp := True;
  InitializeGExperts;

  TTimedCallback.Create(DoAfterIDEInitialized, 1200, True);

  gblAboutFormClass.AddToAboutDialog;
{$IFDEF STARTUP_LAYOUT_FIX_ENABLED}
  FLastDesktopName := GetIdeDesktopName;
  {$IFOPT D+} SendDebug('LastDesktopName:' + FLastDesktopName);  {$ENDIF}
{$ENDIF}
end;

procedure TGExperts.InitializeGExperts;
resourcestring
  SInitError = 'Initialization Error:' + sLineBreak;
begin
  FExpertList := TList.Create;

  FPrivateGExpertsInst := Self;
  InitSharedResources;

  // Create the action manager.
  {$IFOPT D+} SendDebug('Creating GXActionManager'); {$ENDIF}
  CreateGXMenuActionManager;
  try
    {$IFOPT D+} SendDebug('Installing AddIn'); {$ENDIF}
    InstallAddIn;
    {$IFOPT D+} SendDebug('Successfully installed AddIn'); {$ENDIF}
  except
    on E: Exception do
    begin
      GxLogException(E);
      MessageDlg(SInitError + E.Message, mtError, [mbOK], 0);

      { Swallow the exception; at least D5 and BCB5 are allergic to
        exceptions when loading *packages*. Better safe than sorry
        for DLLs. }
    end;
  end;
end;

destructor TGExperts.Destroy;
resourcestring
  SDestructionError = 'GExperts destruction error: ';
var
  i: Integer;
  ExpName : string;
begin
  try
    {$IFOPT D+} SendDebug('Destroying GExperts'); {$ENDIF}

    gblAboutFormClass.RemoveFromAboutDialog;
    GxKeyboardShortCutBroker.BeginUpdate;
    try
      try
        {$IFOPT D+} SendDebug('Destroying Experts'); {$ENDIF}
        if FExpertList <> nil then
        begin
          for i := 0 to FExpertList.Count - 1 do
          begin
            if ExpertList[i] <> nil then begin
              ExpName := ExpertList[i].GetName;
              UniqueString(ExpName);
              {$IFOPT D+}SendDebug('Destroying Expert: ' + ExpName); {$ENDIF}
              try
                ExpertList[i].Free;
                {$IFOPT D+}SendDebug('done Destroying Expert: ' + ExpName); {$ENDIF}
              except
                on E: Exception do
                begin
                  // Report the exception and continue to destroy the other experts
                  MessageDlg(Format('Error destroying expert %d - %s: %s', [i, ExpName, E.Message]), mtError, [mbOK], 0);
                  {$IFOPT D+} SendDebugError(Format('Error destroying expert %d - %s: %s', [i, ExpName, E.Message])); {$ENDIF}
                end;
              end;
            end;
          end;
          {$IFOPT D+} SendDebug('Done freeing experts'); {$ENDIF}
          FreeAndNil(FExpertList);
        end;
      finally
        // Release the editor expert manager and the editor experts
        {$IFOPT D+} SendDebug('Releasing editor expert manager'); {$ENDIF}
        FreeEditorExperts;
        FreeIdeEnhancements;
        ReleaseEditorChangeServices;
        FreeGXToolBarDropDowns;

        // Free the action manager and remove any registered keybindings
        {$IFOPT D+} SendDebug('Freeing Action manager'); {$ENDIF}
        FreeGXMenuActionManager;
        FreeSharedResources;
      end;
    finally
      GxKeyboardShortCutBroker.EndUpdate;
    end;

    FPrivateGExpertsInst := nil;
    inherited Destroy;
    {$IFOPT D+} SendDebug('done Destroying GExperts'); {$ENDIF}
  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebugError('TGExperts.Destroy Error ' + E.Message); {$ENDIF}
      GxLogAndShowException(E, SDestructionError);
      raise;
    end;
  end;
end;

procedure TGExperts.Execute;
begin //FI:W519
  // Do nothing. We install menu and other items to trigger actions.
end;

function TGExperts.FindExpert(const ExpertName: string; out Idx: Integer): boolean;
var
  i: Integer;
begin
  for i := 0 to ExpertCount - 1 do
  begin
    if SameText(ExpertList[i].GetName, ExpertName) then
    begin
      Idx := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TGExperts.FreeEditorExperts;
begin
  FreeAndNil(FEditorExpertsManager);
end;

function TGExperts.GetExpert(const Index: Integer): TGX_Expert;
begin
  Result := TGX_Expert(FExpertList.Items[Index]);
end;

function TGExperts.GetExpertCount: Integer;
begin
  Result := FExpertList.Count;
end;

function TGExperts.GetExpertList: TList;
begin
  Result := FExpertList;
end;

function TGExperts.GetIDString: string;
begin
  Result := 'GExperts.GExperts'; // Do not localize.
end;

function TGExperts.GetName: string;
begin
  Result := 'GExperts'; // Do not localize.
end;

function TGExperts.GetSharedImages: TImageList;
begin
  Result := nil;
  if Assigned(SharedImages) then
    Result := SharedImages.Images;
end;

function TGExperts.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TGExperts.InstallAddIn;
resourcestring
  SExpertCreationFailed = 'Expert "%s" could not be created.' + sLineBreak +
  'Reason: %s';
var
  Expert: TGX_Expert;
  ExpertClass: TGX_ExpertClass;
  i: Integer;
begin
  GxKeyboardShortCutBroker.BeginUpdate;
  try
    for i := 0 to GX_ExpertList.Count - 1 do
    begin
      ExpertClass := GetGX_ExpertClassByIndex(i);
      try
        Expert := ExpertClass.Create;
        FExpertList.Add(Expert);

        Expert.LoadSettings;
      except
        on E: Exception do
        begin
          MessageDlg(Format(SExpertCreationFailed, [ExpertClass.ClassName, E.Message]), mtError, [mbOK], 0);
          // Eat the exception and load other experts (is this safe?)
        end;
      end;
    end;

    if ConfigInfo.EditorExpertsEnabled then
      LoadEditorExperts;

    IdeEnhancements.Initialize;
  finally
    GxKeyboardShortCutBroker.EndUpdate;
  end;

  ShowGxMessageBox(TUnsupportedIDEMessage);
end;

procedure TGExperts.LoadEditorExperts;
begin
  FEditorExpertsManager := TGxEditorExpertManager.Create;
end;

procedure TGExperts.RefreshExpertShortCuts;
var
  i: Integer;
  Expert: TGX_Expert;
begin
  for i := 0 to GetExpertCount - 1 do
  begin
    Expert := GetExpert(i);
    Expert.ShortCut := Expert.ShortCut; //FI:W503 - Assignment has side effects
  end;
end;

procedure TGExperts.ShowConfigurationForm;
begin
  with TfmConfiguration.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TGExperts.DoAfterIDEInitialized(Sender: TObject);
var
  i: Integer;
{$IFDEF STARTUP_LAYOUT_FIX_ENABLED}
  s: string;
{$ENDIF}
begin
  FStartingUp := False;
  for i := 0 to FExpertList.Count - 1 do
    ExpertList[i].AfterIDEInitialized;
  if RunningDelphi8OrGreater then
    GxKeyboardShortCutBroker.DoUpdateKeyBindings;
  GXMenuActionManager.ArrangeMenuItems;
  GXMenuActionManager.MoveMainMenuItems;
{$IFDEF STARTUP_LAYOUT_FIX_ENABLED}
  // se also GX_EditorChangeServices
  if ConfigInfo.GetForceDesktopOnStartup then begin
    s := ConfigInfo.GetForcedStartupDesktop;
    if s = '' then
      s := FLastDesktopName;
    SetIdeDesktop(s);
  end;
{$ENDIF}
end;

function FindClassForm(const AClassName: string): TForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i].ClassNameIs(AClassName) then begin
      Result := Screen.Forms[i];
      Break;
    end;
end;

procedure TGExperts.TimedCloseMessageView;
var
  TheTimer: TTimer;
begin
  TheTimer :=  TTimer.Create(nil);
  TheTimer.OnTimer := OnCloseMessageViewTimer;
end;

procedure TGExperts.OnCloseMessageViewTimer(_Sender: TObject);
var
  Timer: TTimer;
  MessageViewForm: TForm;
begin
  try
    Timer := _Sender as TTimer;
    Timer.Enabled := False;
    FreeAndNil(Timer);
    MessageViewForm := FindClassForm('TMsgWindow');
    if MessageViewForm = nil then // otherwise TMessageViewForm is used
      MessageViewForm := FindClassForm('TMessageViewForm');
    if MessageViewForm = nil then
      Exit; //==>
    MessageViewForm.Hide;
  except
    on E: Exception do begin
{$IFOPT D+}SendDebugError(E.Message + ' in TGExperts.OnCloseMessageViewTimer');
{$ENDIF}
    end;
  end;
end;

{ TUnsupportedIDEMessage }

function TUnsupportedIDEMessage.GetMessage: string;
resourcestring
  SBadIDEVersion =
    'You are currently using an outdated version of this IDE that has ' +
    'patches or update packs available from Embarcadero.  GExperts might work, but is ' +
    'unsupported running under your IDE.  We recommend you upgrade ' +
    'using the downloads available on the Embarcadero web site: '+
    'http://cc.embarcadero.com/myreg';
begin
  Result := SBadIDEVersion;
end;

function TUnsupportedIDEMessage.ShouldShow: Boolean;
begin
  Result := (GetBorlandIdeVersion in [
      // List IDEs here that have OTA/IDE bugs that bother GExperts
      ideD600, ideD601R, ideD601F,
      ideD800, ideD801,
      ideRS2010, ideRS2010U1 // Keyboard macro streaming broken
    ]);
end;

initialization

end.

