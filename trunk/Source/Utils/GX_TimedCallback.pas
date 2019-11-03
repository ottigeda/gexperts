unit GX_TimedCallback;

interface

uses
  SysUtils,
  Classes,
  ExtCtrls;

type
  TTimedCallback = class(TObject)
  private
    FInitTimer: TTimer;
    FCallback: TNotifyEvent;
    FInitCount: Integer;
    procedure OnInitTimer(Sender: TObject);
  public
    constructor Create(CallBack: TNotifyEvent);
  end;

implementation

{ TTimedCallback }

constructor TTimedCallback.Create(CallBack: TNotifyEvent);
begin
  inherited Create;
  Assert(Assigned(Callback));
  FInitTimer := TTimer.Create(nil);
  FInitTimer.Enabled := False;
  FInitTimer.OnTimer := OnInitTimer;
  FInitTimer.Interval := 400;
  FInitTimer.Enabled := True;
  FInitCount := 0;
  FCallback := CallBack;
end;

procedure TTimedCallback.OnInitTimer(Sender: TObject);
begin
  Inc(FInitCount);
  if (FInitCount >= 4) then
  begin
    FInitTimer.Enabled := False;
    FreeAndNil(FInitTimer);
    if Assigned(FCallback) then
      FCallback(Self);
  end;
end;

end.

