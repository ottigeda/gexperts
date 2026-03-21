unit GX_MessageBox;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, Types, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, AppEvnts,
  Messages,
  w_dzDialog, u_dzDpiScaleUtils, u_dzVclUtils;

type
  TGxMsgBoxAdaptor = class(TObject)
  protected
    // General optional data passed into ShowGxMessageBox that can be
    // used to customize the error message, etc.
    FData: string;
    // Returns the message text shown in the message text
    function GetMessage: string; virtual; abstract;
    // Returns caption of the message box
    function GetCaption: string; virtual;
    // Returns the set of buttons that the message box
    // should feature.
    function GetButtons: TMsgDlgButtons; virtual;
    // Button to be used as the default
    function GetDefaultButton: TMsgDlgBtn; virtual;
    // Return a unique identifier for this dialog; this
    // is used for storage in the registry.
    // By default, this is Self.ClassName
    function GetUniqueIdentifier: string;
    // Mark this message box as suppressed; by default,
    // state is stored in the registry.
    procedure DoPermanentlySuppress;
    // Returns whether this message box is permanently
    // suppressed.
    function IsPermanentlySuppressed: Boolean;
    // Returns whether the message box should be shown.
    function ShouldShow: Boolean; virtual;
    function AllowSuppress: Boolean; virtual;
  public
    class function ConfigurationKey: string;
  end;

  TGxErrorBoxAdaptor = class(TGxMsgBoxAdaptor)
  protected
    function GetButtons: TMsgDlgButtons; override;
    function AllowSuppress: Boolean; override;
    function GetMessage: string; override;
  end;

  TGxQuestionBoxAdaptor = class(TGxMsgBoxAdaptor)
  protected
    function GetButtons: TMsgDlgButtons; override;
    function GetDefaultButton: TMsgDlgBtn; override;
  end;

type
  TfmGxMessageBox = class(Tf_dzDialog)
  protected
    procedure DoShow; override;
    procedure Loaded; override;
  protected
    FScaler: TFormDpiScaler;
    procedure WMDpiChanged(var _Msg: TWMDpi); message WM_DPICHANGED;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect); virtual;
    procedure ArrangeControls; virtual;
  protected
    procedure InitDpiScaler;
  public
    destructor Destroy; override;
    procedure GenerateControls; override;
  end;

  TGxMsgBoxAdaptorClass = class of TGxMsgBoxAdaptor;

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass; const Data: string = '';
  _Owner: TWinControl = nil): TModalResult;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Consts,
  GX_GxUtils,
  GX_ConfigurationInfo;

const
  MsgDlgResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry,
    mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0 {$IFDEF GX_VER200_up}, mrClose{$ENDIF});

  MsgDlgButtonCaptions: array[TMsgDlgBtn] of string = (
   SMsgDlgYes, SNoButton, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort, SMsgDlgRetry,
   SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll,
   SMsgDlgYesToAll, SMsgDlgHelp {$IFDEF GX_VER200_up}, SMsgDlgClose{$ENDIF});

  MsgDlgBtn2dzDialogButtonEnum: array[TMsgDlgBtn] of TDialogButtonEnum = (
    dbeYes, dbeNo, dbeOK, dbeCancel, dbeAbort, dbeRetry, dbeIgnore, dbeAll, dbeNoToAll,
    dbeYesToAll, dbeHelp{$IFDEF GX_VER200_up}, dbeOK{$ENDIF});

function MsgDlgButtons2DialogButtonArr(_Buttons:TMsgDlgButtons):TDialogButtonArr;
var
  btn: TMsgDlgBtn;
  cnt: integer;
begin
cnt := 0;
  SetLength(Result, 0);
  for btn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do begin
    if btn in _Buttons then begin
      SetLength(Result, cnt + 1);
      Result[cnt] := MsgDlgBtn2dzDialogButtonEnum[btn];
      Inc(cnt);
    end;
  end;
end;

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass; const Data: string;
  _Owner: TWinControl): TModalResult;
var
  Adaptor: TGxMsgBoxAdaptor;
  frm: TfmGxMessageBox;
  Buttons: TDialogButtonArr;
  DialogButtons: TMsgDlgButtons;
begin
  Adaptor := AdaptorClass.Create;
  try
    Adaptor.FData := Data;
    Result := MsgDlgResults[Adaptor.GetDefaultButton];
    if Adaptor.IsPermanentlySuppressed then
      Exit; //==>

    if Adaptor.ShouldShow then begin
      frm := TfmGxMessageBox.Create(_Owner);
      try
        GxSetDefaultFont(frm);
        frm.ShowDontShowAgain := Adaptor.AllowSuppress;
        frm.ShowDetailButton := False;
        DialogButtons := Adaptor.GetButtons;
        Buttons := MsgDlgButtons2DialogButtonArr(DialogButtons);
        frm.SetVisibleButtons(Buttons);
        frm.SetFocusButton(0);
        frm.Caption := Adaptor.GetCaption;
        frm.UserMessage := Adaptor.GetMessage;
        frm.OptionDescription := '';
        frm.DialogType := mtInformation;
        // center the form first, so the DPI gets set correctly
        TForm_CenterOn(frm, _Owner);
        frm.GenerateControls;
        TForm_CenterOn(frm, _Owner);
        Result := frm.ShowModal;
        if Adaptor.AllowSuppress and frm.DontShowAgainValue then
          Adaptor.DoPermanentlySuppress;
      finally
        FreeAndNil(frm);
      end;
    end;
  finally
    FreeAndNil(Adaptor);
  end;
end;

{ TGxMsgBoxAdaptor }

function TGxMsgBoxAdaptor.GetCaption: string;
begin
  Result := 'GExperts Message';
end;

function TGxMsgBoxAdaptor.GetUniqueIdentifier: string;
begin
  Result := Self.ClassName;
end;

procedure TGxMsgBoxAdaptor.DoPermanentlySuppress;
var
  Settings: IExpertSettings;
begin
  if not AllowSuppress then
    Exit;
  Settings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  Settings.WriteBool(Self.ClassName, True); // Do not localize
end;

function TGxMsgBoxAdaptor.IsPermanentlySuppressed: Boolean;
var
  Settings: IExpertSettings;
begin
  Settings := ConfigInfo.GetExpertSettings(ConfigurationKey);
  Result := Settings.ReadBool(Self.ClassName, False); // Do not localize
end;

function TGxMsgBoxAdaptor.ShouldShow: Boolean;
begin
  Result := True;
end;

function TGxMsgBoxAdaptor.GetButtons: TMsgDlgButtons;
begin
  Result := [mbOK];
end;

function TGxMsgBoxAdaptor.GetDefaultButton: TMsgDlgBtn;
const
  DefaultButton = mbOK;
begin
  Result := DefaultButton;

{$IFOPT D+}
  if not (DefaultButton in GetButtons) then
    SendDebugError('Message box "' + Self.ClassName + '" has a default button that is not available');
{$ENDIF D+}
end;

function TGxMsgBoxAdaptor.AllowSuppress: Boolean;
begin
  Result := True;
end;

class function TGxMsgBoxAdaptor.ConfigurationKey: string;
begin
  Result := 'Misc' + PathDelim + 'SuppressedMessages';
end;

{ TGxErrorBoxAdaptor }

function TGxErrorBoxAdaptor.AllowSuppress: Boolean;
begin
  Result := False;
end;

function TGxErrorBoxAdaptor.GetButtons: TMsgDlgButtons;
begin
{$IF declared(mbClose)}
  Result := [mbClose];
{$ELSE}
  Result := [mbOK];
{$IFEND}
end;

function TGxErrorBoxAdaptor.GetMessage: string;
begin
  Result := FData;
end;

{ TGxQuestionBoxAdaptor }

function TGxQuestionBoxAdaptor.GetButtons: TMsgDlgButtons;
begin
  Result := [mbYes, mbNo];
end;

function TGxQuestionBoxAdaptor.GetDefaultButton: TMsgDlgBtn;
begin
  Result := mbYes;
end;

{ TfmGxMessageBox }

procedure TfmGxMessageBox.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
begin
  if Assigned(FScaler) then begin
    FScaler.ApplyDpi(_NewDpi, _NewBounds);
  end;
  ArrangeControls;
{$IFDEF GX_IDE_IS_HIDPI_AWARE}
  FCurrentPPI := _NewDpi;
{$ENDIF}
end;

procedure TfmGxMessageBox.ArrangeControls;
begin
// inherited?
end;

destructor TfmGxMessageBox.Destroy;
begin
  FreeAndNil(FScaler);
  inherited;
end;

procedure TfmGxMessageBox.DoShow;
begin
  ApplyDpi(TScreen_GetDpiForForm(Self), nil);
  inherited;
end;

procedure TfmGxMessageBox.GenerateControls;
begin
  inherited;
  InitDpiScaler;
end;

procedure TfmGxMessageBox.InitDpiScaler;
var
  DpiForForm: Integer;
begin
  FScaler := TFormDpiScaler.Create(Self);
  DpiForForm := TScreen_GetDpiForForm(Self);
  ApplyDpi(DpiForForm, nil);
end;

procedure TfmGxMessageBox.Loaded;
begin
  Scaled := False;
  inherited;
end;

procedure TfmGxMessageBox.WMDpiChanged(var _Msg: TWMDpi);
begin
  inherited;
{$IFDEF GX_TWMDPI_HAS_SCALLEDRECT_TYPO}
  ApplyDpi(_Msg.YDpi, _Msg.ScalledRect);
{$ELSE}
  ApplyDpi(_Msg.YDpi, _Msg.ScaledRect);
{$ENDIF}
  _Msg.Result := 0;
end;

end.
