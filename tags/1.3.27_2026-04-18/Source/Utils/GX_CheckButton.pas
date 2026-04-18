unit GX_CheckButton;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  Controls,
  StdCtrls,
  Themes;

type
  TGXCheckButton = class(TCustomCheckBox)
  private
    FSavedOnClick: TNotifyEvent;
    FData: Pointer;
{$IFDEF GX_SUPPORTS_THEMING}
    class constructor Create;
    class destructor Destroy;
{$ENDIF}    
    function GetDownState: Boolean;
    procedure SetDownState(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Caption;
    property Data: Pointer read FData write FData;
    property Down: Boolean read GetDownState write SetDownState;
    property SavedOnClick: TNotifyEvent read FSavedOnClick write FSavedOnClick;
    constructor Clone(var AControl);
  end;

  TGXCheckButtonGroup = class(TComponent)
  private
    FButtonList : TList;
    FAllowAllUp : Boolean;
    FOnClick    : TNotifyEvent;
  protected
    procedure AnyButtonClicked(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(var AControl): TGXCheckButton; overload;
    function Add(var AControl; AData: Integer): TGXCheckButton; overload;
    function Add(var AControl; AData: Pointer): TGXCheckButton; overload;
    function GetButtonData(ndx: Integer): Pointer;
    function IsDown(ndx: Integer): Boolean; overload;
    function IsDown(ABtn: TGXCheckButton): Boolean; overload;
    function TryGetSelected(var Index: Integer): Boolean;
    procedure SetButtonState(ndx: Integer; ADown: Boolean); overload;
    procedure SetButtonState(ABtn: TGXCheckButton; ADown: Boolean); overload;
    procedure SetDown(AData: Integer);
  end;

implementation

uses
  Types,
  Windows,
  SysUtils,
  Graphics;

{$IFDEF GX_SUPPORTS_THEMING}
type
  TGXCheckButtonStyleHook = class(TButtonStyleHook)
  protected
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
  end;

{ TGXCheckButtonStyleHook }

procedure TGXCheckButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
var
  LStyle  : TCustomStyleServices;
  Details : TThemedElementDetails;
  LControl: TGXCheckButton;
  R       : TRect;
begin
  LStyle := StyleServices;
  if not LStyle.Available then
  begin
    inherited;
    Exit;
  end;

  if Control is TGXCheckButton then
  begin
    LControl := TGXCheckButton(Control);
    if LControl.Checked
    then Details := LStyle.GetElementDetails(tbPushButtonPressed)
    else Details := LStyle.GetElementDetails(tbPushButtonNormal);

    R := LControl.ClientRect;
    LStyle.DrawElement(ACanvas.Handle, Details, R, nil);
    LStyle.DrawText(ACanvas.Handle, Details, LControl.Caption, R, [tfCenter, tfVerticalCenter]);
    if Focused then
    begin
      R.Inflate(-2, -2);
      DrawFocusRect(ACanvas.Handle, R);
    end;
  end;
end;
{$ENDIF}

{ TGXCheckButton }

type
  TWinControlAccess = class(TWinControl);

{$IFDEF GX_SUPPORTS_THEMING}
class constructor TGXCheckButton.Create;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TGXCheckButton, TCheckBoxStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TGXCheckButton, TGXCheckButtonStyleHook);
end;

class destructor TGXCheckButton.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TGXCheckButton, TGXCheckButtonStyleHook);
end;
{$ENDIF}

constructor TGXCheckButton.Clone(var AControl);
var
  ASource: TWinControl;
begin
  if not (TObject(Pointer(AControl)) is TWinControl) then
    raise EComponentError.Create('Invalid source control');

  FSavedOnClick := nil;
  ASource := TWinControl(AControl);

  Create(ASource.Owner);
  Parent := ASource.Parent;
  BoundsRect := ASource.BoundsRect;
  Anchors    := ASource.Anchors;
{$IF Declared(TMargins)}
  Margins    := ASource.Margins;
  AlignWithMargins := ASource.AlignWithMargins;
{$IFEND}
  Caption    := TWinControlAccess(ASource).Caption;
  Enabled    := ASource.Enabled;
  Visible    := ASource.Visible;
  TabStop    := ASource.TabStop;
  TabOrder   := ASource.TabOrder;
  Name       := ASource.Name + '_GX';
  OnClick    := TWinControlAccess(ASource).OnClick;

  FreeAndNil(ASource);
  Pointer(AControl) := nil;
end;

function TGXCheckButton.GetDownState: Boolean;
begin
  Result := Checked;
end;

procedure TGXCheckButton.SetDownState(const Value: Boolean);
var
  SavedClicksDisabled : Boolean;
begin
  SavedClicksDisabled := ClicksDisabled;
  ClicksDisabled := True;

  SetChecked(Value);

  ClicksDisabled := SavedClicksDisabled;
end;

procedure TGXCheckButton.CreateParams(var Params: TCreateParams);
// Make the checkbox look like a button.
begin
  inherited;
  Params.Style := Params.Style + BS_PUSHLIKE;
end;

{ TGXCheckButtonGroup }

procedure TGXCheckButtonGroup.AnyButtonClicked(Sender: TObject);
var
  AButton : TGXCheckButton;
  LButton : TGXCheckButton;
  i       : Integer;
begin
  if Sender is TGXCheckButton then
  begin
    AButton := TGXCheckButton(Sender);
    // set all other button to "up"
    for i := 0 to FButtonList.Count-1 do
    begin
      LButton := TGXCheckButton(FButtonList.Items[i]);
      if LButton <> AButton then
      begin
        LButton.Down := False;
      end;
    end;
    if (not AButton.Down) and (not FAllowAllUp) then
    begin
      AButton.Down := True;
    end;

    if Assigned(FOnClick) then FOnClick(AButton);
    if Assigned(AButton.FSavedOnClick) then
      AButton.FSavedOnClick(AButton);
  end; // if Sender is TGXCheckButton
end;

constructor TGXCheckButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonList := TList.Create;
  FAllowAllUp := True;
end;

destructor TGXCheckButtonGroup.Destroy;
begin
  FButtonList.Free;
  inherited;
end;

function TGXCheckButtonGroup.GetButtonData(ndx: Integer): Pointer;
begin
  Result := nil;
  if ndx >= 0 then
    Result := TGXCheckButton(FButtonList.Items[ndx]).Data;
end;

function TGXCheckButtonGroup.IsDown(ndx: Integer): Boolean;
var
  LButton : TGXCheckButton;
begin
  LButton := TGXCheckButton(FButtonList[ndx]);
  Result := LButton.Down;
end;

function TGXCheckButtonGroup.IsDown(ABtn: TGXCheckButton): Boolean;
var
  ndx : Integer;
begin
  Result := False;
  ndx := FButtonList.IndexOf(ABtn);
  if ndx >= 0 then
    Result := ABtn.Down;
end;

procedure TGXCheckButtonGroup.Notification(AComponent: TComponent; Operation: TOperation);
var
  ndx : Integer;
begin
  if (AComponent is TGXCheckButton) and (Operation = opRemove) then
  begin
    ndx := FButtonList.IndexOf(AComponent);
    if ndx >= 0 then
    begin
      FButtonList.Items[ndx] := nil;
      FButtonList.Delete(ndx);
    end;
  end;
  inherited;
end;

procedure TGXCheckButtonGroup.SetButtonState(ndx: Integer; ADown: Boolean);
var
  LButton : TGXCheckButton;
  i       : Integer;
begin
  LButton := TGXCheckButton(FButtonList.Items[ndx]);
  if Assigned(LButton) then
  begin
    if ADown then
    begin
      for i := 0 to FButtonList.Count-1 do
        TGXCheckButton(FButtonList.Items[i]).Down := False;
      TGXCheckButton(FButtonList.Items[ndx]).Down := True;
    end
    else begin
      for i := 0 to FButtonList.Count-1 do
        TGXCheckButton(FButtonList.Items[i]).Down := False;
      if FAllowAllUp then
        TGXCheckButton(FButtonList.Items[ndx]).Down := True
      else
        TGXCheckButton(FButtonList.Items[ndx]).Down := True;
    end;
    LButton.Down := ADown;
  end;
end;

procedure TGXCheckButtonGroup.SetButtonState(ABtn: TGXCheckButton; ADown: Boolean);
var
  ndx : Integer;
begin
  ndx := FButtonList.IndexOf(ABtn);
  if ndx >= 0 then
    SetButtonState(ndx, ADown);
end;

procedure TGXCheckButtonGroup.SetDown(AData: Integer);
var
  i       : Integer;
  LButton : TGXCheckButton;
begin
  for i := 0 to FButtonList.Count-1 do
  begin
    LButton := TGXCheckButton(FButtonList.Items[i]);
    if LButton.Data = Pointer(AData) then
      LButton.Down := True
    else
      LButton.Down := False;
  end;
end;

function TGXCheckButtonGroup.TryGetSelected(var Index: Integer): Boolean;
var
  i       : Integer;
  LButton : TGXCheckButton;
begin
  Result := False;
  for i := 0 to FButtonList.Count-1 do
  begin
    LButton := TGXCheckButton(FButtonList.Items[i]);
    if LButton.Down then
    begin
      Index := i;
      Result := True;
      Break;
    end;
  end;
end { TryGetSelected };

function TGXCheckButtonGroup.Add(var AControl; AData: Pointer): TGXCheckButton;
var
  ASource : TWinControl;
  AButton : TGXCheckButton;
begin
  if not (TObject(Pointer(AControl)) is TWinControl) then
    raise EComponentError.Create('Invalid source control');

  ASource := TWinControl(AControl);
  AButton := TGXCheckButton.Clone(ASource);
  AButton.Data := AData;
  AButton.SavedOnClick := AButton.OnClick;
  AButton.OnClick      := Self.AnyButtonClicked;

  FButtonList.Add(AButton);
  Result := AButton;
end;

function TGXCheckButtonGroup.Add(var AControl; AData: Integer): TGXCheckButton;
begin
  Result := Add(AControl, Pointer(AData));
end;

function TGXCheckButtonGroup.Add(var AControl): TGXCheckButton;
begin
  Result := Add(AControl, nil);
end;

end.
