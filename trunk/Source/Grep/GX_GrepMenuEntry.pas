{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T+,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR ON}
{$WARN CVT_WIDENING_STRING_LOST ON}
{$WARN NON_PORTABLE_TYPECAST ON}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}
{$WARN IMMUTABLE_STRINGS OFF}
unit GX_GrepMenuEntry;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Menus,
  ActnList,
  Controls,
  GX_Experts,
  GX_EventHook,
  GX_Logging;

const
  // do not translate
  GrepSearchName = 'GrepSearch';
  GrepResultsName = 'GrepResults';
  GrepNextItemName = 'GrepNextItem';
  GrepPrevItemName = 'GrepPrevItem';

type
  TGrepMenuEntryExpert = class(TGX_Expert)
  private
    FReplaceFind: Boolean;
    FLogger: IGxLogger;
    FSearchFindOrig: TNotifyEvent;
    FSearchAgainOrig: TNotifyEvent;
    procedure PopulatePopupMenu(const PopupMenu: TPopupMenu);
    procedure HandleSearchFindExecute(_Sender: TObject);
    procedure HandleSearchAgainExecute(_Sender: TObject);
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    function SupportsSubmenu: Boolean;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    function HasConfigOptions: Boolean; override;
    procedure Configure(_Owner: TWinControl); override;
    function HasSubmenuItems: Boolean; override;
    function HasCallCount: Boolean; override;
    procedure CreateSubMenuItems(MenuItem: TMenuItem); override;
    procedure Execute(Sender: TObject); override;
    procedure ShowPopup(Sender: TObject);
  end;

var
  gblGrepMenuEntryExpert: TGrepMenuEntryExpert = nil;

implementation

uses
  Types,
  ComCtrls,
  Forms,
  GX_OtaUtils,
  GX_GExperts,
  GX_ActionBroker,
  GX_IdeUtils,
  GX_GrepMenuConfig,
  GX_GrepAsFind;

{ TGrepMenuEntryExpert }

var
  InternalPopupMenu: TPopupMenu;

function GetInternalPopupMenu: TPopupMenu;
const
  MenuName = 'GxMenusForEditorExpertsInternalPopupMenu';
begin
  if not Assigned(InternalPopupMenu) then begin
    InternalPopupMenu := NewPopupMenu(nil, MenuName, paCenter, False, []);
{$IFNDEF GX_ICONS_IN_POPUP_MENUS_ARE_BROKEN}
    InternalPopupMenu.Images := GxOtaGetIdeImageList;
{$ENDIF}
  end;

  Result := InternalPopupMenu;
end;

procedure ReleaseInternalPopupMenu;
begin
  FreeAndNil(InternalPopupMenu);
end;

const
  NilMethod: TMethod = (
    Code: nil;
    Data: nil;
    );

function FindAction(const _Name: string): TAction;
var
  AppBuilder: TForm;
begin
  AppBuilder := TForm(Application.FindComponent('AppBuilder'));
  if not Assigned(AppBuilder) then
    Result := nil
  else begin
    Result := TAction(AppBuilder.FindComponent(_Name));
  end;
end;

function GetActionOnExecute(const _ActionName: string): TNotifyEvent;
var
  Action: TAction;
begin
  Action := FindAction(_ActionName);
  if not Assigned(Action) then
    Result := TNotifyEvent(NilMethod)
  else begin
    Result := Action.OnExecute;
  end;
end;

procedure SetActionOnExecute(const _ActionName: string; _Value: TNotifyEvent);
var
  Action: TAction;
begin
  Action := FindAction(_ActionName);
  if Assigned(Action) then
    Action.OnExecute := TNotifyEvent(_Value);
end;

procedure TGrepMenuEntryExpert.Configure(_Owner: TWinControl);
begin
  if not Tf_GrepMenuConfig.Execute(_Owner, FReplaceFind) then
    Exit; //==>

  if FReplaceFind then begin
    if not Assigned(FSearchFindOrig) then begin
      FLogger.Info('Configure: Installing Find hook');
      FSearchFindOrig := GetActionOnExecute('SearchFindCommand');
      SetActionOnExecute('SearchFindCommand', HandleSearchFindExecute);
    end;
    if not Assigned(FSearchAgainOrig) then begin
      FLogger.Info('Configure: Installing FindAgain hook');
      FSearchAgainOrig := GetActionOnExecute('SearchAgainCommand');
      SetActionOnExecute('SearchAgainCommand', HandleSearchAgainExecute)
    end;
  end else begin
    if Assigned(FSearchFindOrig) then begin
      FLogger.Info('Configure: Removing Find hook');
      SetActionOnExecute('SearchFindCommand', FSearchFindOrig);
      FSearchFindOrig := nil;
    end;
    if Assigned(FSearchAgainOrig) then begin
      FLogger.Info('Configure: Removing FindAgain hook');
      SetActionOnExecute('SearchAgainCommand', FSearchAgainOrig);
      FSearchAgainOrig := nil;
    end;
  end;
end;

constructor TGrepMenuEntryExpert.Create;
begin
  inherited;
  gblGrepMenuEntryExpert := Self;
  FLogger := CreateModuleLogger('GrepMenuExpert');
end;

destructor TGrepMenuEntryExpert.Destroy;
begin
  if Assigned(FSearchFindOrig) then
    SetActionOnExecute('SearchFindCommand', FSearchFindOrig);
  FSearchFindOrig := nil;

  if Assigned(FSearchAgainOrig) then
    SetActionOnExecute('SearchAgainCommand', FSearchAgainOrig);
  FSearchAgainOrig := nil;

  gblGrepMenuEntryExpert := nil;
  ReleaseInternalPopupMenu;
  inherited;
end;

procedure TGrepMenuEntryExpert.HandleSearchFindExecute(_Sender: TObject);
begin
  FLogger.Info('HandleSearchFindExecute');
  TfmGrepAsFind.ExecuteFindFile(nil);
//  TNotifyEvent(FSearchFindHook.OrigEvent)(_Sender);
end;

procedure TGrepMenuEntryExpert.HandleSearchAgainExecute(_Sender: TObject);
begin
  FLogger.Info('HandleSearchAgainExecute');
  FSearchAgainOrig(_Sender);
end;

procedure TGrepMenuEntryExpert.CreateSubMenuItems(MenuItem: TMenuItem);
var
  GExpertsInstance: TGExperts;

  procedure HandleExpert(const _ExpertName: string);
  var
    Expert: TGX_Expert;
    ExpertMenuEntry: TMenuItem;
  begin
    if not GExpertsInstance.FindExpert(_ExpertName, Expert) then
      Exit; //==>
    if not Expert.Active then
      Exit; //==>

    ExpertMenuEntry := TMenuItem.Create(MenuItem);
    if Expert.HasMenuItem then begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsMenuAction(Expert.GetActionName)
    end else begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsAction(Expert.GetActionName);
    end;
    MenuItem.Add(ExpertMenuEntry);
  end;

begin
  inherited;
  Assert(Assigned(MenuItem));
  MenuItem.Clear;

  GExpertsInstance := GExpertsInst;
  Assert(Assigned(GExpertsInstance));

  HandleExpert(GrepSearchName);
  HandleExpert(GrepResultsName);
  HandleExpert(GrepPrevItemName);
  HandleExpert(GrepNextItemName);
end;

procedure TGrepMenuEntryExpert.Execute(Sender: TObject);
var
  IsToolButton: Boolean;
begin
  IsToolButton := Assigned(Sender) and (Sender is TCustomAction)
    and (TCustomAction(Sender).ActionComponent is TToolButton);
  if SupportsSubmenu and (not IsToolButton) and Assigned(Sender) then begin
    // The submenu items perform all actions
  end else begin
    ShowPopup(Sender);
  end;
end;

function TGrepMenuEntryExpert.GetActionCaption: string;
begin
  Result := '&Grep';
end;

class function TGrepMenuEntryExpert.GetName: string;
begin
  Result := 'GrepMenuEntry';
end;

function TGrepMenuEntryExpert.HasCallCount: Boolean;
begin
  Result := False;
end;

function TGrepMenuEntryExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TGrepMenuEntryExpert.HasSubmenuItems: Boolean;
begin
  Result := True;
end;

procedure TGrepMenuEntryExpert.PopulatePopupMenu(const PopupMenu: TPopupMenu);
var
  GExpertsInstance: TGExperts;

  procedure HandleExpert(const _ExpertName: string);
  var
    Expert: TGX_Expert;
    ExpertMenuEntry: TMenuItem;
  begin
    if not GExpertsInstance.FindExpert(_ExpertName, Expert) then
      Exit; //==>
    if not Expert.Active then
      Exit; //==>

    ExpertMenuEntry := TMenuItem.Create(PopupMenu);
    if Expert.HasMenuItem then begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsMenuAction(Expert.GetActionName)
    end else begin
      ExpertMenuEntry.Action := GxActionBroker.FindGExpertsAction(Expert.GetActionName);
    end;
    PopupMenu.Items.Add(ExpertMenuEntry);
  end;

begin
  GExpertsInstance := GExpertsInst;
  Assert(Assigned(GExpertsInstance));

  PopupMenu.Items.Clear;
  HandleExpert(GrepSearchName);
  HandleExpert(GrepResultsName);
  HandleExpert(GrepPrevItemName);
  HandleExpert(GrepNextItemName);
end;

procedure TGrepMenuEntryExpert.ShowPopup(Sender: TObject);
var
  MousePosition: TPoint;
  APopupMenu: TPopupMenu;
begin
  MousePosition := Mouse.CursorPos;
  APopupMenu := GetInternalPopupMenu;
  Assert(Assigned(APopupMenu));
  PopulatePopupMenu(APopupMenu);

  APopupMenu.Popup(MousePosition.X, MousePosition.Y);
end;

function TGrepMenuEntryExpert.SupportsSubmenu: Boolean;
begin
  // The Delphi 7- IDEs seem to clear out the submenu item shortcuts
  Result := RunningDelphi8OrGreater;
  if Result then begin
    // on small screens the sub menu cannot be displayed properly
    Result := Assigned(Screen.ActiveForm) and Assigned(Screen.ActiveForm.Monitor)
      and (Screen.ActiveForm.Monitor.Height >= 1024);
  end;
end;

procedure TGrepMenuEntryExpert.UpdateAction(Action: TCustomAction);
var
  b: Boolean;
  GExpertsInstance: TGExperts;
begin
  GExpertsInstance := GExpertsInst(True);
  b := False;
  if GExpertsInstance.IsExpertActive(GrepSearchName) then
    b := True;
  if GExpertsInstance.IsExpertActive(GrepResultsName) then
    b := True;
  if GExpertsInstance.IsExpertActive(GrepNextItemName) then
    b := True;
  if GExpertsInstance.IsExpertActive(GrepPrevItemName) then
    b := True;

  Action.Enabled := b;
  // todo: Is this a good idea? Shouldn't the action always be visible?
  Action.Visible := Action.Enabled;
end;

initialization
  RegisterGX_Expert(TGrepMenuEntryExpert);

end.

