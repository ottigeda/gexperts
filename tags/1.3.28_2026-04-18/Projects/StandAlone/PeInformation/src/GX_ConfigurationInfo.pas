unit GX_ConfigurationInfo;

interface

type
  IConfigInfo = interface(IUnknown) //FI:W523 - we don't need a GUID
  end;

function ConfigInfo: IConfigInfo;

implementation

type
  TConfigInfo = class(TInterfacedObject, IConfigInfo)

  end;

function ConfigInfo: IConfigInfo;
begin
  Result := TConfigInfo.Create;
end;

end.
