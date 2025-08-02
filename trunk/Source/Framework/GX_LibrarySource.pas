unit GX_LibrarySource;

{$I GX_CondDefine.inc}

{$IFDEF BCB}
  {$OBJEXPORTALL ON}
{$ENDIF BCB}

interface

uses
  Classes,
  ToolsAPI, // Errors here indicate that you didn't link to the DesignIde package
  GX_DuplicateDllHandler;

// This function needs to be interface-visible, otherwise
// C++Builder 5 complains about a missing EXTDEF symbol
// for this export.  Do not call this function yourself.
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;

implementation

uses
  Windows, Forms, GX_GxUtils, GX_OtaUtils, GX_VerDepConst,
  GX_DbugIntf,  GX_GExperts, GX_CodeLib, GX_GrepExpert, GX_ExpertManager,
  GX_DummyWizard, GX_IdeUtils;

const
  InvalidIndex = -1;

var
  FExpertIndex: Integer = InvalidIndex;

{ Remove the wizard from the IDE. }
procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if FExpertIndex <> InvalidIndex then
  begin
    Assert(Assigned(BorlandIDEServices));

    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));

    WizardServices.RemoveWizard(FExpertIndex);

    FExpertIndex := InvalidIndex;
  end;
end;

{ Register the wizard. }
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  Result := (BorlandIDEServices <> nil);

  if Result and (not BuiltWithPackages) then
  begin
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
  if  CheckAndCreateDllMarkerComponent('GExpertsDllMarker2') = nil then begin
    // OK, it is indeed the second (or third ...) call to InitWizard in the same DLL
    // simply return true and exit.
    // todo: Maybe we should display an error message?
    Result := True;
    Exit; //==>
  end;

  Terminate := FinalizeWizard;

  WizardServices := BorlandIDEServices as IOTAWizardServices;
  Assert(Assigned(WizardServices));

  if IsDuplicateGExpertsDll then begin
    // register a dummy wizard so we can fail gracefully
    FExpertIndex := WizardServices.AddWizard(TDummyWizard.Create as IOTAWizard);
{$IFOPT D+}SendDebugFmt('GExperts is already active, added dummy expert with index %d', [FExpertIndex]); {$ENDIF}
  end else begin
    FExpertIndex := WizardServices.AddWizard(TGExperts.Create as IOTAWizard);
{$IFOPT D+}SendDebugFmt('Expert added with index %d', [FExpertIndex]); {$ENDIF}
  end;

  // some code on the Internet checks for <> 0 here, but this seems to be wrong.
  // I have definitely seen AddWizard return 0 and the Expert was active.
  // Unfortunately there seems to be no official documentation on the return code's value.
  Result := (FExpertIndex >= 0);
end;

exports
{$IFDEF GX_BCB}
  InitWizard name WizardEntryPoint;
{$ELSE}
  InitWizard name WizardEntryPoint,
  ShowExpertManager,
  InstallGExperts,
  RemoveGExperts,
  ShowCodeLib,
  ShowGrep,
  ShowGrepEx;
{$ENDIF GX_BCB}

end.
