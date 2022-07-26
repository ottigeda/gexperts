unit GX_Debug;

{$I GX_CondDefine.inc}

// if GX_DEBUGLOG is not defined (which is the default), this unit only
// exports GxDebugShowWarning

// GxDebugShowWarning will do nothing if GX_DEBUG_NO_WARNINGS is defined

interface

uses SysUtils;

{$IFDEF GX_DEBUGLOG}
procedure GxAddToDebugLog(const Msg: string);
procedure GxAddExceptionToLog(const E: Exception; const Msg: string);
{$ENDIF}

procedure GxDebugShowWarning(const _Msg: string);

implementation

uses
  Windows
{$IFNDEF GX_DEBUGLOG}
;

procedure GxDebugShowWarning(const _Msg: string);
begin
{$IFNDEF GX_DEBUG_NO_WARNINGS}
  MessageBox(0, PChar(_msg), 'GExperts Debug Warning', MB_OK or MB_ICONHAND);
{$ENDIF}
end;


{$ELSE}
, Classes,
  // This requires the JCL 1.11: http://delphi-jedi.org/CODELIBJCL
  // And a detailed map file (see the linker options in the IDE)
  JclDebug, JclHookExcept;

var
  DebugData: TStringList = nil;
  DebugFile: string = '';

procedure LoadDebugFile;
var
  Buf: array[0..MAX_PATH + 1] of Char;
begin
  if DebugData = nil then
  begin
    DebugData := TStringList.Create;
    SetString(DebugFile, Buf, GetModuleFileName(HInstance, Buf, SizeOf(Buf)));
    DebugFile := DebugFile + '.debuglog';
    if FileExists(DebugFile) then
      DebugData.LoadFromFile(DebugFile);
  end;
end;

procedure GxExceptionNotify(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  try
    LoadDebugFile;
    if ExceptObj is Exception then
    begin
      DebugData.Add(Format('Exception %s "%s" at: %s', [ExceptObj.ClassName,
        Exception(ExceptObj).Message, DateTimeToStr(Now)]));
    end
    else
      DebugData.Add('Exception at: ' + DateTimeToStr(Now));
    if JclLastExceptStackList <> nil then
      JclLastExceptStackList.AddToStrings(DebugData);
    DebugData.Add('');
    DebugData.SaveToFile(DebugFile);
  except
    // Swallow exceptions
  end;
end;

// Do we need a more performance conscious way to save the log
// without waiting until we unload.  Keep the file open for append?
// A method that supports multiple clients writing to the log file?
// As-is it allows us to debug some machine hangs, though
procedure GxAddToDebugLog(const Msg: string);
begin
  try
    LoadDebugFile;
    DebugData.Text := DebugData.Text + Msg;
    DebugData.SaveToFile(DebugFile);
  except
    // Swallow exceptions
  end;
end;

procedure GxAddExceptionToLog(const E: Exception; const Msg: string);
begin
  try
    LoadDebugFile;
    DebugData.Add(Msg +': '+ E.ClassName +': '+ E.Message);
    if JclLastExceptStackList <> nil then
      JclLastExceptStackList.AddToStrings(DebugData);
    DebugData.SaveToFile(DebugFile);
  except
    // Swallow exceptions
  end;
end;

initialization
  JclStartExceptionTracking;
  JclAddExceptNotifier(GxExceptionNotify);

finalization
  JclStopExceptionTracking;
{$ENDIF}

end.
