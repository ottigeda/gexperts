unit testfile_FinalMethod;

interface

type
  TMyThread = class(TThread)
  protected
    procedure Execute; override; final;
    procedure doExecute; virtual; abstract;
  end;

implementation

procedure TMyThread.Execute;
begin
end;

end.