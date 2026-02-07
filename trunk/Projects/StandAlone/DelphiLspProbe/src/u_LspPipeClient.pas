unit u_LspPipeClient;

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  TLspPipeClient = class
  private
    FStdinRead, FStdinWrite: THandle;
    FStdoutRead, FStdoutWrite: THandle;
    FStderrRead, FStderrWrite: THandle;
    FProcessHandle: THandle;
    FProcessId: DWORD;
    FExePath: string;
    procedure CreatePipes;
    procedure ClosePipeHandles;
  public
    constructor Create(const AExePath: string);
    destructor Destroy; override;
    function Start: Boolean;
    procedure Stop;
    procedure SendMessage(const AJson: string);
    function ReadMessage: string;
    function ReadStderr: string;
    property ExePath: string read FExePath;
    property ProcessId: DWORD read FProcessId;
  end;

  ELspPipeError = class(Exception);

implementation

{ TLspPipeClient }

constructor TLspPipeClient.Create(const AExePath: string);
begin
  inherited Create;
  FExePath := AExePath;
  FStdinRead := INVALID_HANDLE_VALUE;
  FStdinWrite := INVALID_HANDLE_VALUE;
  FStdoutRead := INVALID_HANDLE_VALUE;
  FStdoutWrite := INVALID_HANDLE_VALUE;
  FStderrRead := INVALID_HANDLE_VALUE;
  FStderrWrite := INVALID_HANDLE_VALUE;
  FProcessHandle := INVALID_HANDLE_VALUE;
  FProcessId := 0;
end;

destructor TLspPipeClient.Destroy;
begin
  Stop;
  inherited;
end;

procedure TLspPipeClient.CreatePipes;
var
  SecurityAttr: TSecurityAttributes;
begin
  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := True;
  SecurityAttr.lpSecurityDescriptor := nil;

  // Create stdin pipe
  if not CreatePipe(FStdinRead, FStdinWrite, @SecurityAttr, 0) then
    raise ELspPipeError.Create('Failed to create stdin pipe');
  // Make write handle non-inheritable
  SetHandleInformation(FStdinWrite, HANDLE_FLAG_INHERIT, 0);

  // Create stdout pipe
  if not CreatePipe(FStdoutRead, FStdoutWrite, @SecurityAttr, 0) then
    raise ELspPipeError.Create('Failed to create stdout pipe');
  // Make read handle non-inheritable
  SetHandleInformation(FStdoutRead, HANDLE_FLAG_INHERIT, 0);

  // Create stderr pipe
  if not CreatePipe(FStderrRead, FStderrWrite, @SecurityAttr, 0) then
    raise ELspPipeError.Create('Failed to create stderr pipe');
  // Make read handle non-inheritable
  SetHandleInformation(FStderrRead, HANDLE_FLAG_INHERIT, 0);
end;

procedure TLspPipeClient.ClosePipeHandles;
begin
  if FStdinRead <> INVALID_HANDLE_VALUE then
    CloseHandle(FStdinRead);
  if FStdinWrite <> INVALID_HANDLE_VALUE then
    CloseHandle(FStdinWrite);
  if FStdoutRead <> INVALID_HANDLE_VALUE then
    CloseHandle(FStdoutRead);
  if FStdoutWrite <> INVALID_HANDLE_VALUE then
    CloseHandle(FStdoutWrite);
  if FStderrRead <> INVALID_HANDLE_VALUE then
    CloseHandle(FStderrRead);
  if FStderrWrite <> INVALID_HANDLE_VALUE then
    CloseHandle(FStderrWrite);

  FStdinRead := INVALID_HANDLE_VALUE;
  FStdinWrite := INVALID_HANDLE_VALUE;
  FStdoutRead := INVALID_HANDLE_VALUE;
  FStdoutWrite := INVALID_HANDLE_VALUE;
  FStderrRead := INVALID_HANDLE_VALUE;
  FStderrWrite := INVALID_HANDLE_VALUE;
end;

function TLspPipeClient.Start: Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CreationFlags: DWORD;
begin
  if not FileExists(FExePath) then
    raise ELspPipeError.CreateFmt('LSP executable not found: %s', [FExePath]);

  CreatePipes;

  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := FStdinRead;
  StartupInfo.hStdOutput := FStdoutWrite;
  StartupInfo.hStdError := FStderrWrite;

  ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));

  CreationFlags := CREATE_NO_WINDOW;

  if not CreateProcess(
    PChar(FExePath),
    nil,
    nil,
    nil,
    True,  // Inherit handles
    CreationFlags,
    nil,
    PChar(ExtractFilePath(FExePath)),
    StartupInfo,
    ProcessInfo) then
  begin
    ClosePipeHandles;
    raise ELspPipeError.CreateFmt('Failed to start LSP process: %s (Error %d)',
      [FExePath, GetLastError]);
  end;

  FProcessHandle := ProcessInfo.hProcess;
  FProcessId := ProcessInfo.dwProcessId;
  CloseHandle(ProcessInfo.hThread);

  // Close the child's ends of the pipes
  CloseHandle(FStdinRead);
  FStdinRead := INVALID_HANDLE_VALUE;
  CloseHandle(FStdoutWrite);
  FStdoutWrite := INVALID_HANDLE_VALUE;
  CloseHandle(FStderrWrite);
  FStderrWrite := INVALID_HANDLE_VALUE;

  Result := True;
end;

procedure TLspPipeClient.Stop;
begin
  if FProcessHandle <> INVALID_HANDLE_VALUE then
  begin
    TerminateProcess(FProcessHandle, 0);
    CloseHandle(FProcessHandle);
    FProcessHandle := INVALID_HANDLE_VALUE;
  end;
  ClosePipeHandles;
  FProcessId := 0;
end;

procedure TLspPipeClient.SendMessage(const AJson: string);
var
  Header: AnsiString;
  Content: UTF8String;
  Written: DWORD;
begin
  Content := UTF8Encode(AJson);
  Header := AnsiString(Format('Content-Length: %d'#13#10#13#10, [Length(Content)]));

  if not WriteFile(FStdinWrite, Header[1], Length(Header), Written, nil) then
    raise ELspPipeError.Create('Failed to write header to stdin pipe');

  if not WriteFile(FStdinWrite, Content[1], Length(Content), Written, nil) then
    raise ELspPipeError.Create('Failed to write content to stdin pipe');
end;

function TLspPipeClient.ReadMessage: string;
var
  HeaderLine: AnsiString;
  Ch: AnsiChar;
  BytesRead: DWORD;
  ContentLength: Integer;
  Content: UTF8String;
  TotalRead: Integer;
  Available: DWORD;
  Timeout: Cardinal;
begin
  Result := '';
  ContentLength := -1;
  Timeout := GetTickCount + 30000; // 30 second timeout

  // Read headers (line by line until empty line)
  while True do
  begin
    HeaderLine := '';
    // Read character by character until CRLF
    while True do
    begin
      // Check for timeout
      if GetTickCount > Timeout then
        raise ELspPipeError.Create('Timeout waiting for LSP response');

      // Check if data is available
      if not PeekNamedPipe(FStdoutRead, nil, 0, nil, @Available, nil) then
        raise ELspPipeError.Create('Failed to peek stdout pipe');

      if Available = 0 then
      begin
        Sleep(10);
        Continue;
      end;

      if not ReadFile(FStdoutRead, Ch, 1, BytesRead, nil) or (BytesRead = 0) then
        raise ELspPipeError.Create('Failed to read from stdout pipe');

      if Ch = #13 then
        Continue;  // Skip CR
      if Ch = #10 then
        Break;     // End of line

      HeaderLine := HeaderLine + Ch;
    end;

    // Empty line signals end of headers
    if HeaderLine = '' then
      Break;

    // Parse Content-Length header
    if Pos('Content-Length:', string(HeaderLine)) = 1 then
      ContentLength := StrToIntDef(Trim(string(Copy(HeaderLine, 16, MaxInt))), -1);
  end;

  if ContentLength <= 0 then
    raise ELspPipeError.Create('Invalid or missing Content-Length header');

  // Read the content
  SetLength(Content, ContentLength);
  TotalRead := 0;
  while TotalRead < ContentLength do
  begin
    if GetTickCount > Timeout then
      raise ELspPipeError.Create('Timeout reading LSP content');

    if not ReadFile(FStdoutRead, Content[TotalRead + 1],
      ContentLength - TotalRead, BytesRead, nil) then
      raise ELspPipeError.Create('Failed to read content from stdout pipe');

    if BytesRead = 0 then
    begin
      Sleep(10);
      Continue;
    end;

    Inc(TotalRead, BytesRead);
  end;

  Result := UTF8ToString(Content);
end;

function TLspPipeClient.ReadStderr: string;
var
  Buffer: array[0..4095] of AnsiChar;
  BytesRead, Available: DWORD;
  Output: AnsiString;
begin
  Result := '';
  Output := '';

  while PeekNamedPipe(FStderrRead, nil, 0, nil, @Available, nil) and (Available > 0) do
  begin
    if ReadFile(FStderrRead, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) and (BytesRead > 0) then
    begin
      Buffer[BytesRead] := #0;
      Output := Output + Buffer;
    end
    else
      Break;
  end;

  Result := string(Output);
end;

end.
