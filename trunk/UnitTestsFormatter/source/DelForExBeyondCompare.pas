unit DelForExBeyondCompare;

interface

function GetBeyondCompareExePath: string;

implementation

uses
  Windows,
  SysUtils,
  Registry;

function TryReadFromReg(_HKey: HKEY; const _Key, _Value: string; out _ExePath: string): Boolean;
var
  reg: TRegistry;
begin
  Result := False;
  reg := TRegistry.Create;
  try
    reg.RootKey := _HKey;
    if reg.OpenKeyReadOnly(_Key) then begin
      try
        if reg.ValueExists(_Value) then begin
          _ExePath := reg.ReadString(_Value);
          Result := True;
        end;
      finally
        reg.CloseKey;
      end;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function TryReadScooter(out _ExePath: string): Boolean;
const
  REG_KEY = '\Software\Scooter Software\Beyond Compare';
  REG_VALUE = 'ExePath';
begin
  Result := TryReadFromReg(HKEY_LOCAL_MACHINE, REG_KEY, REG_VALUE, _ExePath);
  if not Result then
    Result := TryReadFromReg(HKEY_CURRENT_USER, REG_KEY, REG_VALUE, _ExePath);
end;

function TryReadIntelliPoint(out _ExePath: string): Boolean;
const
  REG_KEY = '\Software\Microsoft\IntelliPoint\AppSpecific\BCompareLite.exe';
  REG_VALUE = 'Path';
begin
  Result := TryReadFromReg(HKEY_CURRENT_USER, REG_KEY, REG_VALUE, _ExePath)
end;

function GetBeyondCompareExePath: string;
begin
  if not TryReadScooter(Result) then
    if not TryReadIntelliPoint(Result) then
      Result := 'C:\Program Files (x86)\Beyond Compare 2\bc2.exe';
end;

end.
