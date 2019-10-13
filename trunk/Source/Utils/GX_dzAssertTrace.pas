unit GX_dzAssertTrace;

interface

uses
  SysUtils;

var
  gblAssertTraceOn: Boolean = False;

implementation

uses
  GX_DbugIntf;

{$IFDEF ASSERT_TRACING}

{$MESSAGE warn 'Assert tracing is turned on, this will impact performance!'}

procedure DebugAssertLine(const _Message, _Filename: string; _LineNumber: Integer; _ErrorAddr: Pointer);
begin
  if gblAssertTraceOn then
    SendDebug(ChangeFileExt(ExtractFileName(_Filename), '') + ':' + IntToStr(_LineNumber) + ' ' + _Message);
end;

initialization
  AssertErrorProc := DebugAssertLine;
{$ENDIF}
end.
