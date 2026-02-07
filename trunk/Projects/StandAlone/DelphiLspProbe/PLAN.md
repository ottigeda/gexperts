# DelphiLspProbe - Plan

## Goal

Build a simple Delphi console application that launches `delphilsp.exe`, sends an LSP `initialize` request, and prints the server's reported capabilities. This tells us definitively whether DelphiLSP supports rename, code actions, formatting, or other refactoring-related features.

## Why

Embarcadero removed refactoring from Delphi 13. We want to know if the LSP server still has those capabilities hidden, or if they were stripped out entirely. Documentation is incomplete - only probing the actual server will give a definitive answer.

## Approach

A minimal console app (no GUI) that:

1. Finds `delphilsp.exe` from the Delphi installation (registry lookup)
2. Launches it as a child process with stdin/stdout pipes
3. Sends an LSP `initialize` request with basic client capabilities
4. Reads and parses the `initialize` response
5. Prints the `serverCapabilities` in a readable format, highlighting:
   - `renameProvider`
   - `codeActionProvider`
   - `documentFormattingProvider`
   - `completionProvider`
   - `definitionProvider`
   - `referencesProvider`
   - Any other capabilities present
6. Sends `initialized` notification, then `shutdown` + `exit`

## Files to Create

```
Projects/StandAlone/DelphiLspProbe/
  src/
    DelphiLspProbe.dpr           - Main program
    DelphiLspProbe.dproj         - Delphi project file (Delphi 12+)
    u_LspPipeClient.pas          - Pipe communication with child process
    u_LspProtocol.pas            - JSON-RPC message building/parsing
  __Build_Project.cmd            - Build script
```

## Implementation Details

### u_LspPipeClient.pas
- `TLspPipeClient` class
- Creates anonymous pipes for stdin/stdout/stderr
- Launches `delphilsp.exe` via `CreateProcess` with redirected handles
- `SendMessage(JsonString)` - prepends `Content-Length` header and writes to stdin pipe
- `ReadMessage(): string` - reads `Content-Length` header from stdout, then reads the JSON payload
- Synchronous (blocking) communication is fine for this probe tool

### u_LspProtocol.pas
- `BuildInitializeRequest(): string` - returns the JSON for an initialize request
- `BuildInitializedNotification(): string`
- `BuildShutdownRequest(): string`
- `BuildExitNotification(): string`
- `ParseCapabilities(JsonResponse: string)` - extracts and prints server capabilities
- Uses `System.JSON` (available from Delphi 10+)

### DelphiLspProbe.dpr
- Console application (`{$APPTYPE CONSOLE}`)
- Finds delphilsp.exe path from registry or command-line argument
- Creates pipe client, sends initialize, prints capabilities, shuts down cleanly

### Initialize Request (JSON-RPC)

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "processId": <our PID>,
    "clientInfo": { "name": "DelphiLspProbe", "version": "1.0" },
    "rootUri": null,
    "capabilities": {},
    "initializationOptions": {
      "serverType": "controller",
      "agentCount": 1
    }
  }
}
```

## Expected Output

```
DelphiLspProbe - Probing DelphiLSP capabilities
================================================
Launching: C:\Delphi\13\bin\delphilsp.exe
Sending initialize request...

Server: DelphiLSP v<version>

Capabilities:
  completionProvider:          YES
  hoverProvider:               YES
  definitionProvider:          YES
  declarationProvider:         YES
  implementationProvider:      YES
  referencesProvider:          YES
  signatureHelpProvider:       YES
  documentFormattingProvider:  no
  renameProvider:              no
  codeActionProvider:          no
  ...

Raw capabilities JSON saved to: delphilsp_capabilities.json
```

## References

- [ExploreDelphiLSP](https://github.com/CWBudde/ExploreDelphiLSP) - Proof-of-concept Delphi app, MIT licensed, shows pipe communication pattern
- [LSP-Pascal-Library](https://github.com/rickard67/LSP-Pascal-Library) - Full Delphi LSP client library, Delphi 10.4+
- [LSP Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
