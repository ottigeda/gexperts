unit GX_CheckButton;

{ This unit provides:
  - TGXCheckButton: A CheckBox-like component, which looks like a TButton;
    similar to TSpeedButton, but with keyboard focus control.
  - TGXCheckButtonGroup: Group TGXCheckButtons to behave like a RadioGroup.
  - TGXCheckButtonStyleHook for themed drawing.
}

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
  Graphics,
  GX_IdeUtils
  ;

{$IFDEF GX_SUPPORTS_THEMING}
type
  TGXCheckButtonStyleHook = class(TButtonStyleHook)
  protected
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
  end;

{ TGXCheckButtonStyleHook }

const
  Pattern : array[0..7] of Word = (
    $5555, $AAAA, $5555, $AAAA
   ,$5555, $AAAA, $5555, $AAAA
  );

procedure TGXCheckButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
var
  LStyle  : TCustomStyleServices;
  Details : TThemedElementDetails;
  LControl: TGXCheckButton;
  R       : TRect;

  procedure GXDrawFocusRect(DC: HDC; R: TRect);
  var
    hbmpPattern : HBITMAP;
    hbrPattern  : HBRUSH;
    hbrOriginal : HBRUSH;
    cx, cy      : Integer;
    nSaved      : Integer;
  begin
    nSaved := SaveDC(DC);
    hbmpPattern := CreateBitmap(8, 8, 1, 1, @Pattern);
    hbrPattern  := CreatePatternBrush(hbmpPattern);
    hbrOriginal := SelectObject(DC, hbrPattern);

    SetBkColor(DC,   ColorToRGB(GetThemedColor(clWindow)));
    SetTextColor(DC, ColorToRGB(GetThemedColor(clWindowText)));

    SystemParametersInfo(SPI_GETFOCUSBORDERWIDTH, 0, cx, 0);
    SystemParametersInfo(SPI_GETFOCUSBORDERHEIGHT, 0, cy, 0);

    PatBlt(DC, R.Left      , R.Top        , R.Width, cy , PATINVERT);  // top
    PatBlt(DC, R.Left      , R.Bottom - cy, R.Width, cy , PATINVERT);  // bottom
    PatBlt(DC, R.Left      , R.Top    + cy, cx     , R.Height - (cy * 2), PATINVERT);  // left
    PatBlt(DC, R.Right - cx, R.Top    + cy, cx     , R.Height - (cy * 2), PATINVERT);  // right

    SelectObject(DC, hbrOriginal);
    DeleteObject(hbrPattern);
    DeleteObject(hbmpPattern);

    RestoreDC(DC, nSaved);
  end;

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

    if LControl.Checked then
      Details := LStyle.GetElementDetails(tbPushButtonPressed)
    else
      Details := LStyle.GetElementDetails(tbPushButtonNormal);

    R := LControl.ClientRect;
    LStyle.DrawElement(ACanvas.Handle, Details, R, nil);
    LStyle.DrawText(ACanvas.Handle, Details, LControl.Caption, R, [tfCenter, tfVerticalCenter]);

    if Focused then
    begin
      R.Inflate(-2, -2); // for better visibility

      // DrawFocusRect(ACanvas.Handle, R);      // respects SPI_GETFOCUSBORDERWIDTH/SPI_GETFOCUSBORDERHEIGHT
      // DrawStyleFocusRect(ACanvas.Handle, R); // only 1 pixel; ignores system settings
      GXDrawFocusRect(ACanvas.Handle, R); // own implementation
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
  Margins    := ASource.Margins;
  AlignWithMargins := ASource.AlignWithMargins;
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

    if AButton.Down then
    begin
    if Assigned(AButton.FSavedOnClick) then
        AButton.FSavedOnClick(AButton)
      else
        if Assigned(FOnClick) then
          FOnClick(AButton);
    end;
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
  LOnClick: TNotifyEvent;
begin
  if not (TObject(Pointer(AControl)) is TWinControl) then
    raise EComponentError.Create('Invalid source control');

  ASource := TWinControl(AControl);
  LOnClick := TWinControlAccess(ASource).OnClick;

  AButton := TGXCheckButton.Clone(ASource);
  AButton.Data := AData;
  AButton.SavedOnClick := LOnClick;
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
