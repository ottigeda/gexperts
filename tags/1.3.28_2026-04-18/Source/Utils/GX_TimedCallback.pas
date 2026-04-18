unit GX_TimedCallback;

interface

uses
  SysUtils,
  Classes,
  ExtCtrls;

type
  TTimedCallback = class(TObject)
  private
    FTimer: TTimer;
    FCallback: TNotifyEvent;
    FFreeAfterCallback: Boolean;
    procedure HandleTimer(Sender: TObject);
  public
    constructor Create(_CallBack: TNotifyEvent; _DelayMS: Integer; _FreeAfterCallback: Boolean);
    destructor Destroy; override;
    procedure Reset;
  end;

implementation

uses
  u_dzVclUtils;

{ TTimedCallback }

constructor TTimedCallback.Create(_CallBack: TNotifyEvent; _DelayMS: Integer;
  _FreeAfterCallback: Boolean);
begin
  inherited Create;

  Assert(Assigned(_Callback));

  FCallback := _CallBack;
  FFreeAfterCallback := _FreeAfterCallback;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
  FTimer.Interval := _DelayMS;
  FTimer.Enabled := True;
end;

destructor TTimedCallback.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

procedure TTimedCallback.HandleTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  if Assigned(FCallback) then
    FCallback(Self);
  if FFreeAfterCallback then
    Free; //FI:W515 Suspicious FREE call
end;

procedure TTimedCallback.Reset;
begin
  TTimer_Restart(FTimer);
end;

end.

