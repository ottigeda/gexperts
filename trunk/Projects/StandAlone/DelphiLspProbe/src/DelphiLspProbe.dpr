program DelphiLspProbe;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  SysUtils,
  Classes,
  Registry,
  StrUtils,
  u_LspPipeClient in 'u_LspPipeClient.pas',
  u_LspProtocol in 'u_LspProtocol.pas';

function FindDelphiLspFromRegistry: string;
const
  BDSKey = 'Software\Embarcadero\BDS';
var
  Reg: TRegistry;
  Versions: TStringList;
  I: Integer;
  RootDir, LspPath: string;
  BestVersion: Double;
  Version: Double;
begin
  Result := '';
  BestVersion := 0;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKeyReadOnly(BDSKey) then
      Exit;

    Versions := TStringList.Create;
    try
      Reg.GetKeyNames(Versions);
      Reg.CloseKey;

      for I := 0 to Versions.Count - 1 do
      begin
        if Reg.OpenKeyReadOnly(BDSKey + '\' + Versions[I]) then
        begin
          if Reg.ValueExists('RootDir') then
          begin
            RootDir := Reg.ReadString('RootDir');
            LspPath := IncludeTrailingPathDelimiter(RootDir) + 'bin\delphilsp.exe';
            if FileExists(LspPath) then
            begin
              Version := StrToFloatDef(Versions[I], 0);
              if Version > BestVersion then
              begin
                BestVersion := Version;
                Result := LspPath;
              end;
            end;
          end;
          Reg.CloseKey;
        end;
      end;
    finally
      Versions.Free;
    end;
  finally
    Reg.Free;
  end;
end;

procedure PrintUsage;
begin
  WriteLn('Usage: DelphiLspProbe [path-to-delphilsp.exe]');
  WriteLn;
  WriteLn('If no path is provided, the tool will search the Windows Registry');
  WriteLn('for the latest installed Delphi version.');
end;

procedure Main;
var
  LspPath: string;
  Client: TLspPipeClient;
  Protocol: TLspProtocol;
  Request, Response: string;
  StdErr: string;
begin
  WriteLn('DelphiLspProbe - Probing DelphiLSP capabilities');
  WriteLn('================================================');
  WriteLn;

  // Get LSP path from command line or registry
  if ParamCount >= 1 then
  begin
    LspPath := ParamStr(1);
    if not FileExists(LspPath) then
    begin
      WriteLn('Error: File not found: ' + LspPath);
      ExitCode := 1;
      Exit;
    end;
  end
  else
  begin
    WriteLn('Searching registry for DelphiLSP...');
    LspPath := FindDelphiLspFromRegistry;
    if LspPath = '' then
    begin
      WriteLn('Error: Could not find delphilsp.exe in registry.');
      WriteLn;
      PrintUsage;
      ExitCode := 1;
      Exit;
    end;
  end;

  WriteLn('Launching: ' + LspPath);
  WriteLn;

  Client := TLspPipeClient.Create(LspPath);
  Protocol := TLspProtocol.Create;
  try
    try
      if not Client.Start then
      begin
        WriteLn('Error: Failed to start LSP process');
        ExitCode := 1;
        Exit;
      end;

      // Send initialize request
      WriteLn('Sending initialize request...');
      Request := Protocol.BuildInitializeRequest(GetCurrentProcessId);
      Client.SendMessage(Request);

      // Read initialize response
      Response := Client.ReadMessage;
      Protocol.ParseAndPrintCapabilities(Response);
      Protocol.SaveCapabilitiesToFile(Response, 'delphilsp_capabilities.json');

      // Send initialized notification
      WriteLn;
      WriteLn('Sending initialized notification...');
      Client.SendMessage(Protocol.BuildInitializedNotification);

      // Give it a moment to process
      Sleep(500);

      // Check stderr for any messages
      StdErr := Client.ReadStderr;
      if StdErr <> '' then
      begin
        WriteLn;
        WriteLn('Server stderr output:');
        WriteLn(StdErr);
      end;

      // Send shutdown request
      WriteLn('Sending shutdown request...');
      Client.SendMessage(Protocol.BuildShutdownRequest);

      try
        Response := Client.ReadMessage;
        // Shutdown response received
      except
        // Ignore errors reading shutdown response
      end;

      // Send exit notification
      WriteLn('Sending exit notification...');
      Client.SendMessage(Protocol.BuildExitNotification);

      WriteLn;
      WriteLn('Done.');

    except
      on E: ELspPipeError do
      begin
        WriteLn('LSP Error: ' + E.Message);
        StdErr := Client.ReadStderr;
        if StdErr <> '' then
        begin
          WriteLn;
          WriteLn('Server stderr:');
          WriteLn(StdErr);
        end;
        ExitCode := 1;
      end;
      on E: Exception do
      begin
        WriteLn('Error: ' + E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Client.Free;
    Protocol.Free;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ' + E.Message);
      ExitCode := 1;
    end;
  end;

  // Wait for key if running interactively
  if IsDebuggerPresent then
  begin
    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
  end;
end.
