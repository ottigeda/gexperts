unit u_dzCriticalSection;

{.$DEFINE debug_Crit_Sect}

interface

uses
  Windows,
  SyncObjs;

type
  TdzCriticalSection = class(TCriticalSection)
  private
{$IFDEF debug_Crit_Sect}
    FLockCount: Integer;
    FOwner: Integer;
{$ENDIF debug_Crit_Sect}
  public
    class function NewInstance: TObject; override;
{$IFDEF debug_Crit_Sect}
    procedure Acquire; override;
    procedure Release; override;
{$ENDIF debug_Crit_Sect}
  end;

implementation

function GetCacheLineSize: Integer;
var
  ProcInfo: PSystemLogicalProcessorInformation;
  CurInfo: PSystemLogicalProcessorInformation;
  Len: DWORD;
begin
  Len := 0;
  if (GetProcAddress(GetModuleHandle(kernel32), 'GetLogicalProcessorInformation') <> nil) and
    not GetLogicalProcessorInformation(nil, Len) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then begin
    GetMem(ProcInfo, Len);
    try
      GetLogicalProcessorInformation(ProcInfo, Len);
      CurInfo := ProcInfo;
      while Len > 0 do begin
        if (CurInfo.Relationship = RelationCache) and (CurInfo.Cache.Level = 1) then begin
          Result := CurInfo.Cache.LineSize;
          Exit;
        end;
        Inc(CurInfo);
        Dec(Len, SizeOf(CurInfo^));
      end;
    finally
      FreeMem(ProcInfo);
    end;
  end;
  Result := 64;
end;

var
  CacheLineSize: Integer;

{ TdzCriticalSection }

class function TdzCriticalSection.NewInstance: TObject;
// see
// http://delphitools.info/2011/11/30/fixing-tcriticalsection/
// for an explanation why this could speed up execution on multi core systems
var
  InstSize: Integer;
begin
  InstSize := InstanceSize;
  if InstSize < CacheLineSize then
    InstSize := CacheLineSize;
  Result := InitInstance(GetMemory(InstSize));
end;

{$IFDEF debug_Crit_Sect}

procedure TdzCriticalSection.Acquire;
begin
  InterlockedIncrement(FLockCount);
  inherited;
  FOwner := GetCurrentThreadId;
end;

procedure TdzCriticalSection.Release;
begin
  inherited;
  if InterlockedDecrement(FLockCount) < 0 then
    Assert(FLockCount < 10);
end;
{$ENDIF debug_Crit_Sect}

initialization
  CacheLineSize := GetCacheLineSize;
end.

