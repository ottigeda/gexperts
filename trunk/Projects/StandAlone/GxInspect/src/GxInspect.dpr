program GxInspect;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
  DefaultPort = 3580;
  PortFileName = 'GxInspect.port';
  INVALID_SOCKET_VALUE = Integer(not 0);
  SOCKET_ERROR_VALUE = -1;

type
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..256] of AnsiChar;
    szSystemStatus: array[0..128] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  end;

  TSockAddrIn = packed record
    sin_family: Word;
    sin_port: Word;
    sin_addr: Cardinal;
    sin_zero: array[0..7] of AnsiChar;
  end;

const
  AF_INET = 2;
  SOCK_STREAM = 1;
  IPPROTO_TCP = 6;
  INADDR_LOOPBACK = $7F000001;

function WSAStartup(wVersionRequired: Word; var lpWSAData: TWSAData): Integer; stdcall; external 'ws2_32.dll';
function WSACleanup: Integer; stdcall; external 'ws2_32.dll';
function ws_socket(af, type_, protocol: Integer): Integer; stdcall; external 'ws2_32.dll' name 'socket';
function ws_connect(s: Integer; var name: TSockAddrIn; namelen: Integer): Integer; stdcall; external 'ws2_32.dll' name 'connect';
function ws_closesocket(s: Integer): Integer; stdcall; external 'ws2_32.dll' name 'closesocket';
function ws_send(s: Integer; const buf; len, flags: Integer): Integer; stdcall; external 'ws2_32.dll' name 'send';
function ws_recv(s: Integer; var buf; len, flags: Integer): Integer; stdcall; external 'ws2_32.dll' name 'recv';
function ws_htons(hostshort: Word): Word; stdcall; external 'ws2_32.dll' name 'htons';
function ws_htonl(hostlong: Cardinal): Cardinal; stdcall; external 'ws2_32.dll' name 'htonl';

var
  Port: Integer;
  PortExplicit: Boolean;
  CommandLine: string;

function GetPortFilePath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + PortFileName;
end;

function ReadPortFromFile: Integer;
var
  F: TextFile;
  S: string;
begin
  Result := 0;
  if not FileExists(GetPortFilePath) then
    Exit;
  AssignFile(F, GetPortFilePath);
  try
    Reset(F);
    try
      if not Eof(F) then begin
        ReadLn(F, S);
        S := Trim(S);
        Result := StrToIntDef(S, 0);
      end;
    finally
      CloseFile(F);
    end;
  except
    Result := 0;
  end;
end;

procedure WritePortFile(APort: Integer);
var
  F: TextFile;
begin
  AssignFile(F, GetPortFilePath);
  try
    Rewrite(F);
    try
      WriteLn(F, APort);
    finally
      CloseFile(F);
    end;
    WriteLn('Enabled inspection server on port ', APort);
    WriteLn('File: ', GetPortFilePath);
    WriteLn('Restart the Delphi IDE to activate.');
  except
    on E: Exception do begin
      WriteLn('ERROR: Could not write port file: ', E.Message);
      Halt(1);
    end;
  end;
end;

procedure DeletePortFile;
begin
  if FileExists(GetPortFilePath) then begin
    if DeleteFile(GetPortFilePath) then
      WriteLn('Disabled inspection server.')
    else begin
      WriteLn('ERROR: Could not delete ', GetPortFilePath);
      Halt(1);
    end;
  end else
    WriteLn('Inspection server was not enabled.');
end;

procedure ShowHelp;
begin
  WriteLn('GxInspect - Query the GExperts UI inspection server');
  WriteLn;
  WriteLn('Usage: GxInspect [options] <command> [args...]');
  WriteLn('  -p <port>    Server port (default: ', DefaultPort, ')');
  WriteLn;
  WriteLn('Commands:');
  WriteLn('  ENABLE [port]                            Create port file to enable server');
  WriteLn('  DISABLE                                  Remove port file to disable server');
  WriteLn('  FORMS                                    List all forms');
  WriteLn('  CONTROLS <form-id>                       List controls');
  WriteLn('  SCREENSHOT <form-id> <filepath>          Save screenshot');
  WriteLn('  BOUNDS <form-id>                         Form geometry');
  WriteLn('  CHECK <form-id>                          Overlap/clipping check');
  WriteLn('  EXPERTS                                  List all experts');
  WriteLn('  EXECUTE <expert-name>                    Execute an expert');
  WriteLn('  CONFIGURE <expert-name>                  Open expert config');
  WriteLn('  SETBOUNDS <form-id> <L> <T> <W> <H>     Move/resize a form');
  WriteLn('  CLOSE <form-id>                          Close a form');
  WriteLn('  CLICK <form-id> <control-name>           Click a control');
  WriteLn('  GETPROP <form-id> <control> <property>   Get a property');
  WriteLn('  SETPROP <form-id> <ctrl> <prop> <value>  Set a property');
  WriteLn('  QUIT                                     Close the IDE');
  WriteLn('  VERSION                                  IDE and GExperts version');
  WriteLn('  PING                                     Test connection');
  WriteLn;
  WriteLn('The ENABLE command creates a file (', PortFileName, ') next to this executable.');
  WriteLn('GExperts reads it on IDE startup to activate the inspection server.');
end;

procedure ParseArgs;
var
  i: Integer;
  Arg: string;
  Parts: string;
begin
  Port := DefaultPort;
  PortExplicit := False;
  Parts := '';
  i := 1;
  while i <= ParamCount do begin
    Arg := ParamStr(i);
    if (Arg = '-p') or (Arg = '--port') then begin
      Inc(i);
      if i <= ParamCount then begin
        Port := StrToIntDef(ParamStr(i), DefaultPort);
        PortExplicit := True;
      end;
    end else if (Arg = '--help') or (Arg = '-h') or (Arg = '/?') then begin
      ShowHelp;
      Halt(0);
    end else begin
      if Parts <> '' then
        Parts := Parts + ' ';
      Parts := Parts + Arg;
    end;
    Inc(i);
  end;
  CommandLine := Parts;
end;

procedure SendAll(Sock: Integer; const S: AnsiString);
var
  Sent, Total, Ret: Integer;
begin
  Total := Length(S);
  Sent := 0;
  while Sent < Total do begin
    Ret := ws_send(Sock, S[Sent + 1], Total - Sent, 0);
    if Ret <= 0 then begin
      WriteLn('ERROR: send failed');
      Halt(1);
    end;
    Inc(Sent, Ret);
  end;
end;

function RecvLine(Sock: Integer; out Line: string): Boolean;
var
  Ch: AnsiChar;
  Ret: Integer;
begin
  Result := False;
  Line := '';
  while True do begin
    Ret := ws_recv(Sock, Ch, 1, 0);
    if Ret <= 0 then
      Exit;
    if Ch = #10 then begin
      Result := True;
      Exit;
    end;
    if Ch <> #13 then
      Line := Line + Char(Ch);
  end;
end;

var
  WsaData: TWSAData;
  Sock: Integer;
  Addr: TSockAddrIn;
  Line: string;
  HadOutput: Boolean;
  UpperCmd: string;
  FilePort: Integer;
begin
  ParseArgs;
  if CommandLine = '' then begin
    ShowHelp;
    Halt(0);
  end;

  UpperCmd := UpperCase(CommandLine);

  // Handle ENABLE command: create the port file
  if (UpperCmd = 'ENABLE') or (Copy(UpperCmd, 1, 7) = 'ENABLE ') then begin
    if Length(UpperCmd) > 7 then
      Port := StrToIntDef(Trim(Copy(CommandLine, 8, MaxInt)), DefaultPort);
    WritePortFile(Port);
    Halt(0);
  end;

  // Handle DISABLE command: delete the port file
  if UpperCmd = 'DISABLE' then begin
    DeletePortFile;
    Halt(0);
  end;

  // For all other commands, read port from file if -p was not passed
  FilePort := ReadPortFromFile;
  if (FilePort > 0) and not PortExplicit then
    Port := FilePort;

  if WSAStartup($0202, WsaData) <> 0 then begin
    WriteLn('ERROR: WSAStartup failed');
    Halt(1);
  end;
  try
    Sock := ws_socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if Sock = INVALID_SOCKET_VALUE then begin
      WriteLn('ERROR: Could not create socket');
      Halt(1);
    end;
    try
      FillChar(Addr, SizeOf(Addr), 0);
      Addr.sin_family := AF_INET;
      Addr.sin_port := ws_htons(Word(Port));
      Addr.sin_addr := ws_htonl(INADDR_LOOPBACK);

      if ws_connect(Sock, Addr, SizeOf(Addr)) = SOCKET_ERROR_VALUE then begin
        WriteLn('ERROR: Could not connect to inspection server on port ', Port);
        if FilePort = 0 then
          WriteLn('No port file found. Run "GxInspect ENABLE" first, then restart the IDE.')
        else
          WriteLn('Make sure the Delphi IDE was started after running "GxInspect ENABLE".');
        Halt(1);
      end;

      // Send the command
      SendAll(Sock, AnsiString(CommandLine + #13#10));

      // Read response until END line
      HadOutput := False;
      while RecvLine(Sock, Line) do begin
        if Line = 'END' then
          Break;
        WriteLn(Line);
        HadOutput := True;
      end;

      if not HadOutput then begin
        WriteLn('ERROR: No response from server');
        Halt(1);
      end;
    finally
      ws_closesocket(Sock);
    end;
  finally
    WSACleanup;
  end;
end.
