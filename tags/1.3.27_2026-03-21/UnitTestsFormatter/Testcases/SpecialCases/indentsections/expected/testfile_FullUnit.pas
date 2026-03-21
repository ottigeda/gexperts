unit testfile_FullUnit;

interface

  uses
    SysUtils,
    Classes;

  type
    TMyRecord = record
      Name: string;
      Value: Integer;
    end;

    TMyClass = class
    private
      FData: TMyRecord;
    public
      constructor Create;
      destructor Destroy; override;
      property Data: TMyRecord read FData write FData;
    end;

  var
    GlobalInstance: TMyClass;

implementation

  uses
    Windows,
    Messages;

  { TMyClass }

  constructor TMyClass.Create;
  begin
    inherited;
    FData.Name := '';
    FData.Value := 0;
  end;

  destructor TMyClass.Destroy;
  begin
    inherited;
  end;

initialization
  GlobalInstance := TMyClass.Create;

finalization
  GlobalInstance.Free;

end.
