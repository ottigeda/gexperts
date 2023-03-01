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
    constructor Create(CallBack: TNotifyEvent; _DelayMS: integer; _FreeAfterCallback: boolean);
    destructor Destroy; override;
    procedure Reset;
  end;

implementation

{ TTimedCallback }

constructor TTimedCallback.Create(CallBack: TNotifyEvent; _DelayMS: integer;
  _FreeAfterCallback: boolean);
begin
  inherited Create;

  Assert(Assigned(Callback));

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
  FTimer.Interval := _DelayMS;
  FTimer.Enabled := True;

  FCallback := CallBack;

  FFreeAfterCallback := _FreeAfterCallback;
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
  FTimer.Enabled := False;
  FTimer.Enabled := True; // FI:W508 Variable is assigned twice successively
end;

end.

