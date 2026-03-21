unit GX_GExpertsLightMain;

interface

uses
  SysUtils,
  StrUtils,
  Windows,
  Classes,
  Forms,
  ActnList,
  Menus,
  Controls,
  Registry,
  ToolsAPI,
  GX_DuplicateDllHandler,
  GX_CodeFormatterExpert;

type
  TGExpertsLight = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  private
    FActionList: TActionList;
    FKeyboardBindingIdx: Integer;
    FFormatter: TCodeFormatterExpert;
    FMainMenuItem: TMenuItem;
    procedure HandleFormat(_Sender: TObject);
    procedure HandleConfigure(_Sender: TObject);
    procedure LoadSettings;
  protected // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  protected // IOTAMenuWizard
    function GetMenuText: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

// This function needs to be interface-visible, otherwise
// C++Builder 5 complains about a missing EXTDEF symbol
// for this export.  Do not call this function yourself.
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;

exports
  InitWizard Name WizardEntryPoint;

implementation

uses
  Dialogs,
  u_dzVclUtils,
//  w_dzDialog,
  GX_GxUtils,
  DbugIntf,
  GX_DummyWizard,
  GX_IdeUtils,
  GX_About,
  GX_ConfigurationInfo;

{ TGExpertsLight }

constructor TGExpertsLight.Create;
var
  NTAServices: INTAServices;
  act: TAction;
begin
  inherited;

  FFormatter := TCodeFormatterExpert.Create;

  LoadSettings;

  FActionList := TActionList.Create(nil);

//  FKeyboardBindingIdx := (BorlandIDEServices as  IOTAKeyboardServices).AddKeyboardBinding(Self);

  NTAServices := BorlandIDEServices as INTAServices;

  FMainMenuItem := TMenuItem.Create(nil);
  FMainMenuItem.Caption := 'GE&xperts Light';
  act := TActionlist_Append(FActionList, '&Format Code', HandleFormat, ShortCut(Ord('D'), [ssCtrl]));
  act.SecondaryShortCuts.Add('Shift+Ctrl+D');
  TMenuItem_AppendSubmenuItem(FMainMenuItem, act);
  act := TActionlist_Append(FActionList, '&Configure Code Formatter', HandleConfigure);
  TMenuItem_AppendSubmenuItem(FMainMenuItem, act);
  NTAServices.MainMenu.Items.Add(FMainMenuItem);

  TfmAbout.AddToAboutDialog;
end;

destructor TGExpertsLight.Destroy;
begin
  TfmAbout.RemoveFromAboutDialog;

  FreeAndNil(FFormatter);
  if FKeyboardBindingIdx <> -1 then
    (BorlandIDEServices as IOTAKeyboardServices).RemoveKeyboardBinding(FKeyboardBindingIdx);
  inherited;
end;

procedure TGExpertsLight.LoadSettings;
var
  Settings: IExpertSettings;
begin
  Settings := ConfigInfo.GetExpertSettings('EditorExperts' + PathDelim + 'CodeFormatter');
  FFormatter.InternalLoadSettings(Settings);
end;

procedure TGExpertsLight.HandleConfigure(_Sender: TObject);
var
  Settings: IExpertSettings;
begin
  FFormatter.Configure(nil);

  Settings := ConfigInfo.GetExpertSettings('EditorExperts' + PathDelim + 'CodeFormatter');
  FFormatter.InternalSaveSettings(Settings);
end;

procedure TGExpertsLight.HandleFormat(_Sender: TObject);
//var
//  frm: Tf_dzDialog;
begin
  if not FFormatter.Execute then
    Exit; //==>

//  frm := Tf_dzDialog.Create(nil);
//  try
//    frm.ShowDontShowAgain := True;
//    frm.ShowDetailButton := False;
//    frm.SetVisibleButtons([dbeOK]);
//    frm.SetFocusButton(0);
//    frm.Caption := 'GExperts Light - Code Formatter';
//    frm.UserMessage := 'Your code has been formatted. To undo, press Ctrl+Z.';
//    frm.OptionDescription := '';
//    frm.DialogType := mtInformation;
//    // center the form first, so the DPI gets set correctly
//    TForm_CenterOn(frm, nil);
//    frm.GenerateControls;
//    TForm_CenterOn(frm, nil);
//    frm.ShowModal;
//    FDontShowAgain := frm.ShowDontShowAgain;
//  finally
//    FreeAndNil(frm);
//  end;
end;

function TGExpertsLight.GetName: string;
begin
  Result := 'GExperts.experts.light';
end;

function TGExpertsLight.GetIDString: string;
begin
  Result := GetName;
end;

procedure TGExpertsLight.Execute;
begin
  FFormatter.Configure(nil);
end;

function TGExpertsLight.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TGExpertsLight.GetMenuText: string;
begin
  Result := 'Configure GExperts Code Formatter';
end;

const
  InvalidIndex = -1;

var
  gblExpertIndex: Integer = InvalidIndex;

  { Remove the wizard from the IDE. }
procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if gblExpertIndex <> InvalidIndex then begin
    Assert(Assigned(BorlandIDEServices));

    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));

    WizardServices.RemoveWizard(gblExpertIndex);

    gblExpertIndex := InvalidIndex;
  end;
end;

{ Register the wizard. }
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean;
var
  WizardServices: IOTAWizardServices;
begin
  Result := (BorlandIDEServices <> nil);

  if Result and (not BuiltWithPackages) then begin
    Result := False;
    ShowNoPackagesError;
  end;

  if not Result then
    Exit; //==>

  Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);

  // Second check that the GExperts DLL is not loaded twice in the same IDE instance.
  // The first check in CreateInstanceMutexes only works if two different GExperts DLLs are
  // loaded into the IDE. If the same one is loaded twice, the initialization section
  // (and thus CreateInstanceMutexes) is only called once.
  // So we now do the same GExpertsDLLMarker trick again, with a different name this time
  if CheckAndCreateDllMarkerComponent('GExpertsDllMarker2') = nil then begin
    // OK, it is indeed the second (or third ...) call to InitWizard in the same DLL
    // simply return true and exit.
    // todo: Maybe we should display an error message?
    Result := True;
    Exit; //==>
  end;

  Terminate := FinalizeWizard;

  WizardServices := BorlandIDEServices as IOTAWizardServices;
  Assert(Assigned(WizardServices));

  if IsDuplicateGExpertsDll() then begin
    // register a dummy wizard so we can fail gracefully
    gblExpertIndex := WizardServices.AddWizard(TDummyWizard.Create as
      IOTAWizard);
{$IFOPT D+}SendDebugFmt('GExperts is already active, added dummy expert with index %d', [gblExpertIndex]); {$ENDIF}
  end else begin
    gblExpertIndex := WizardServices.AddWizard(TGExpertsLight.Create as
      IOTAWizard);
{$IFOPT D+}SendDebugFmt('Expert added with index %d', [gblExpertIndex]); {$ENDIF}
  end;

  // some code on the Internet checks for <> 0 here, but this seems to be wrong.
  // I have definitely seen AddWizard return 0 and the Expert was active.
  // Unfortunately there seems to be no official documentation on the return code's value.
  Result := (gblExpertIndex >= 0);
end;

end.

