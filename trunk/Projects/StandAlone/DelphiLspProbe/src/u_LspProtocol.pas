unit u_LspProtocol;

interface

uses
  SysUtils,
  Classes,
  StrUtils,
  System.JSON;

type
  TLspProtocol = class
  private
    FNextId: Integer;
    class function FormatJson(AJson: TJSONValue): string; static;
  public
    constructor Create;
    function BuildInitializeRequest(AProcessId: Cardinal): string;
    function BuildInitializedNotification: string;
    function BuildShutdownRequest: string;
    function BuildExitNotification: string;
    procedure ParseAndPrintCapabilities(const AJsonResponse: string);
    procedure SaveCapabilitiesToFile(const AJsonResponse, AFileName: string);
  end;

implementation

{ TLspProtocol }

constructor TLspProtocol.Create;
begin
  inherited;
  FNextId := 1;
end;

class function TLspProtocol.FormatJson(AJson: TJSONValue): string;
begin
  Result := AJson.Format(2);
end;

function TLspProtocol.BuildInitializeRequest(AProcessId: Cardinal): string;
var
  Root, Params, ClientInfo, Capabilities, InitOptions: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('jsonrpc', '2.0');
    Root.AddPair('id', TJSONNumber.Create(FNextId));
    Inc(FNextId);
    Root.AddPair('method', 'initialize');

    Params := TJSONObject.Create;
    Params.AddPair('processId', TJSONNumber.Create(Integer(AProcessId)));

    ClientInfo := TJSONObject.Create;
    ClientInfo.AddPair('name', 'DelphiLspProbe');
    ClientInfo.AddPair('version', '1.0');
    Params.AddPair('clientInfo', ClientInfo);

    Params.AddPair('rootUri', TJSONNull.Create);

    // Empty capabilities - we just want to see what the server offers
    Capabilities := TJSONObject.Create;
    Params.AddPair('capabilities', Capabilities);

    // Initialization options for DelphiLSP
    InitOptions := TJSONObject.Create;
    InitOptions.AddPair('serverType', 'controller');
    InitOptions.AddPair('agentCount', TJSONNumber.Create(1));
    Params.AddPair('initializationOptions', InitOptions);

    Root.AddPair('params', Params);

    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

function TLspProtocol.BuildInitializedNotification: string;
var
  Root, Params: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('jsonrpc', '2.0');
    Root.AddPair('method', 'initialized');
    Params := TJSONObject.Create;
    Root.AddPair('params', Params);
    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

function TLspProtocol.BuildShutdownRequest: string;
var
  Root: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('jsonrpc', '2.0');
    Root.AddPair('id', TJSONNumber.Create(FNextId));
    Inc(FNextId);
    Root.AddPair('method', 'shutdown');
    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

function TLspProtocol.BuildExitNotification: string;
var
  Root: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('jsonrpc', '2.0');
    Root.AddPair('method', 'exit');
    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

procedure TLspProtocol.ParseAndPrintCapabilities(const AJsonResponse: string);

  function GetCapabilityStatus(ACaps: TJSONObject; const AName: string): string;
  var
    Value: TJSONValue;
  begin
    Value := ACaps.FindValue(AName);
    if Value = nil then
      Result := 'no'
    else if Value is TJSONBool then
    begin
      if TJSONBool(Value).AsBoolean then
        Result := 'YES'
      else
        Result := 'no';
    end
    else if Value is TJSONObject then
      Result := 'YES (options)'
    else
      Result := 'YES';
  end;

var
  Root, ResultObj, Caps, ServerInfo: TJSONObject;
  ServerName, ServerVersion: string;
  CapPair: TJSONPair;
  AllCaps: TStringList;
  I: Integer;
  HighlightCaps: array of string;
begin
  HighlightCaps := ['completionProvider', 'hoverProvider', 'signatureHelpProvider',
    'declarationProvider', 'definitionProvider', 'typeDefinitionProvider',
    'implementationProvider', 'referencesProvider', 'documentHighlightProvider',
    'documentSymbolProvider', 'codeActionProvider', 'codeLensProvider',
    'documentFormattingProvider', 'documentRangeFormattingProvider',
    'renameProvider', 'foldingRangeProvider', 'executeCommandProvider',
    'workspaceSymbolProvider', 'semanticTokensProvider'];

  Root := TJSONObject.ParseJSONValue(AJsonResponse) as TJSONObject;
  if Root = nil then
  begin
    WriteLn('Error: Failed to parse JSON response');
    Exit;
  end;

  try
    // Check for error
    if Root.FindValue('error') <> nil then
    begin
      WriteLn('Error from server:');
      WriteLn(FormatJson(Root.FindValue('error')));
      Exit;
    end;

    ResultObj := Root.FindValue('result') as TJSONObject;
    if ResultObj = nil then
    begin
      WriteLn('Error: No result in response');
      Exit;
    end;

    // Get server info
    ServerInfo := ResultObj.FindValue('serverInfo') as TJSONObject;
    if ServerInfo <> nil then
    begin
      ServerName := '';
      ServerVersion := '';
      if ServerInfo.FindValue('name') <> nil then
        ServerName := ServerInfo.FindValue('name').Value;
      if ServerInfo.FindValue('version') <> nil then
        ServerVersion := ServerInfo.FindValue('version').Value;
      WriteLn;
      WriteLn(Format('Server: %s v%s', [ServerName, ServerVersion]));
    end;

    Caps := ResultObj.FindValue('capabilities') as TJSONObject;
    if Caps = nil then
    begin
      WriteLn('Error: No capabilities in response');
      Exit;
    end;

    WriteLn;
    WriteLn('Capabilities:');
    WriteLn(StringOfChar('-', 50));

    // Print highlighted capabilities first
    for I := 0 to High(HighlightCaps) do
      WriteLn(Format('  %-35s %s', [HighlightCaps[I] + ':', GetCapabilityStatus(Caps, HighlightCaps[I])]));

    // Collect all other capabilities
    AllCaps := TStringList.Create;
    try
      AllCaps.Sorted := True;
      for CapPair in Caps do
      begin
        // Skip already printed ones
        if not MatchStr(CapPair.JsonString.Value, HighlightCaps) then
          AllCaps.Add(CapPair.JsonString.Value);
      end;

      if AllCaps.Count > 0 then
      begin
        WriteLn;
        WriteLn('Other capabilities:');
        for I := 0 to AllCaps.Count - 1 do
          WriteLn(Format('  %-35s %s', [AllCaps[I] + ':', GetCapabilityStatus(Caps, AllCaps[I])]));
      end;
    finally
      AllCaps.Free;
    end;

  finally
    Root.Free;
  end;
end;

procedure TLspProtocol.SaveCapabilitiesToFile(const AJsonResponse, AFileName: string);
var
  Root, ResultObj, Caps: TJSONObject;
  FileContent: TStringList;
begin
  Root := TJSONObject.ParseJSONValue(AJsonResponse) as TJSONObject;
  if Root = nil then
    Exit;

  try
    ResultObj := Root.FindValue('result') as TJSONObject;
    if ResultObj = nil then
      Exit;

    Caps := ResultObj.FindValue('capabilities') as TJSONObject;
    if Caps = nil then
      Exit;

    FileContent := TStringList.Create;
    try
      FileContent.Text := FormatJson(Caps);
      FileContent.SaveToFile(AFileName, TEncoding.UTF8);
      WriteLn;
      WriteLn('Raw capabilities JSON saved to: ' + AFileName);
    finally
      FileContent.Free;
    end;
  finally
    Root.Free;
  end;
end;

end.
