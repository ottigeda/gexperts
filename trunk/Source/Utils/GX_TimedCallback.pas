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
    FFreeAfterCallback: boolean;
    procedure HandleTimer(Sender: TObject);
  public
    constructor Create(CallBack: TNotifyEvent; _DelayMS: integer; _FreeAfterCallback: boolean);
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

procedure TTimedCallback.HandleTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  FreeAndNil(FTimer);
  if Assigned(FCallback) then
    FCallback(Self);
  if FFreeAfterCallback then
    Free;
end;

end.

