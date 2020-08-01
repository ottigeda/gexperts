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

uses
  u_dzMiscUtils;

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

{$IF not declared(PSystemLogicalProcessorInformation)}
{$ALIGN ON}
{$MINENUMSIZE 4}

type
  _PROCESSOR_CACHE_TYPE = (CacheUnified { = 0}, CacheInstruction { = 1}, CacheData { = 2}, CacheTrace { = 3});
  PROCESSOR_CACHE_TYPE = _PROCESSOR_CACHE_TYPE;
  TProcessorCacheType = PROCESSOR_CACHE_TYPE;
type
  TCacheDescriptor = record
    Level: BYTE;
    Associativity: BYTE;
    LineSize: WORD;
    Size: DWORD;
    _Type: PROCESSOR_CACHE_TYPE;
  end;

type
  TLogicalProcessorRelationship = (RelationProcessorCore { = 0},
    RelationNumaNode { = 1},
    RelationCache { = 2},
    RelationProcessorPackage { = 3},
    RelationGroup { = 4}, RelationAll = $FFFF);

type
  TSystemLogicalProcessorInformation = record
    ProcessorMask: ULONG_PTR;
    Relationship: TLogicalProcessorRelationship;
    case Integer of
      0: (Flags: BYTE); // ProcessorCore
      1: (NodeNumber: DWORD); // NumaNode
      2: (Cache: TCacheDescriptor); //Cache
      3: (Reserved: array[0..1] of ULONGLONG);
  end;
  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;

function GetLogicalProcessorInformation(Buffer: PSystemLogicalProcessorInformation; var ReturnedLength: DWORD): BOOL; stdcall;
  external kernel32 name 'GetLogicalProcessorInformation';
{$IFEND}

function GetCacheLineSize: Integer;
var
  ProcInfo: PSystemLogicalProcessorInformation;
  CurInfo: PSystemLogicalProcessorInformation;
  Len: DWORD;
  Err: DWORD;
begin
  Result := 64;

  Len := 0;
  if not GetLogicalProcessorInformation(nil, Len) then begin
    Err := GetLastError;
    if Err = ERROR_INSUFFICIENT_BUFFER then begin
      GetMem(ProcInfo, Len);
      try
        if GetLogicalProcessorInformation(ProcInfo, Len) then begin
          // it should not be possible that the second call still returns, but ...
          CurInfo := ProcInfo;
          while Len > 0 do begin
            if (CurInfo.Relationship = RelationCache) and (CurInfo.Cache.Level = 1) then begin
              Result := CurInfo.Cache.LineSize;
              Exit;
            end;
            Inc(CurInfo);
            Dec(Len, SizeOf(CurInfo^));
          end;
        end;
      finally
        FreeMem(ProcInfo);
      end;
    end;
  end;
end;

initialization
  CacheLineSize := GetCacheLineSize;
end.

