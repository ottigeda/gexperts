unit GX_DuplicateDllHandler;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms;

// For this check to work, this unit's initialization code must be executed before
// any unit that calls this function. Since calling this function is only possible
// if this unit is in the other unit's uses list, that should always be the case.
function IsDuplicateGExpertsDll: Boolean;

///<summary>
/// Checks if a component with the given name already exists. If not, create it.
/// @param MarkerName is the component name to check for and create.
/// @returns NIL if the comopnent already exists
///          otherwise the newly created component </summary>
function CheckAndCreateDllMarkerComponent(const _MarkerName: string): TComponent;

implementation

uses
  GX_MessageBox,
  GX_OtaUtils,
  GX_VerDepConst;

// ---------------------------------------------

type
  TGxMultipleInstancesMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TGxMultipleInstancesMessage }

function TGxMultipleInstancesMessage.GetMessage: string;
resourcestring
  SMultipleGExpertsInstances =
    'Multiple instances of this version of GExperts are currently running.'#13#10 +
    'This is usually caused by multiple instances of the same IDE running at once (check in the Task Manager if you don''t see them all).'#13#10 +
    'Please be aware that the last IDE instance to save any settings ' +
    'will overwrite any previously saved settings from other instances.';
begin
  Result := SMultipleGExpertsInstances;
end;

function CheckAndCreateDllMarkerComponent(const _MarkerName: string): TComponent;
begin
  if Application.FindComponent(_MarkerName) <> nil then begin
    // Another instance of GExperts has already been loaded.
    Result := nil;
    Exit;
  end;

  Result := TComponent.Create(Application);
  Result.Name := _MarkerName;
end;

var
  // used to detect duplicate GExperts.dlls loaded into the same IDE instance
  GExpertsDllMarker: TComponent = nil;

  // used to detect duplicate GExperts.dlls loaded into different IDE instances
  GXGeneralMutex: THandle;
  GXVersionMutex: THandle;

function IsDuplicateGExpertsDll: Boolean;
begin
  Result := (GExpertsDllMarker = nil);
end;

procedure CreateInstanceMutexes;
begin
  // First, check that the GExperts DLL is not loaded twice in the same IDE instance.
  // This would play havoc with many of the functions.
  // Unfortunately this only works if two different DLLs are loaded into the IDE.
  // If the same one is loaded twice, the initialization (and thus CreateInstanceMutexes)
  // is only called once. So we need another way of preventing this.
  // But nothing is preventing us from doing the GExpertsDLLMarker trick twice, with
  // a different name in InitWizard above.
  GExpertsDllMarker := CheckAndCreateDllMarkerComponent('GExpertsDllMarker');
  if GExpertsDllMarker = nil then begin
    // Another instance of GExperts has already been loaded.
    // -> Fail silently and do not create an instance of TGExperts in InitWizard.
    Exit;
  end;

  // This mutex signals that at least one copy of GExperts is running
  // The installer uses this to determine if it should allow installation
  // (See AppMutex in GExperts.iss)
  GXGeneralMutex := CreateMutex(nil, False, 'GExperts.Addin.For.Borland.IDEs');

  GXVersionMutex := CreateMutex(nil, False, PChar('GExperts.Addin.For.Borland.IDEs.Under.' +
    GxOtaGetIDEProductIdentifier + MajorVersionNumberChar));
  if (GXVersionMutex <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then begin
    // The mutex already exists, so there is a good chance that a copy
    // of this version of GExperts is already loaded into memory.
    ShowGxMessageBox(TGxMultipleInstancesMessage);
  end;
end;

procedure DestroyInstanceMutexes;
begin
  if GXGeneralMutex <> 0 then
    CloseHandle(GXGeneralMutex);
  if GXVersionMutex <> 0 then
    CloseHandle(GXVersionMutex);
end;

initialization
  CreateInstanceMutexes;

finalization
  DestroyInstanceMutexes;

end.
