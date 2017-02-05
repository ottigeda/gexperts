{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_Base.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JeExpert_Base;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  Dialogs,
  Registry,
  ToolsAPI,
  Menus,
  ComCtrls,
  ExtCtrls,
  JeExpert_Types,
  JeExpert_Strings,
  JeExpert_Editor,
  JeExpert_FormEditor,
  JeExpert_Exported,
  JeExpert_PropertyEditors,
  JeExpert_CompoEditors;

type
 TJeBaseExpert = class(TIExpert)
 private
   FHandle: THandle;
   FExpertDllEntry:TExpertDllEntry;
   FDelphiApis: TExportedApis;
   function GetComponent(Name: string):TComponent;
   procedure FormClose(Sender: TObject; var Action: TCloseAction);
 public
   constructor Create;
   destructor Destroy;override;
   function GetName:string;override;stdcall;
   function GetAuthor:string;override;stdcall;
   function GetStyle:TExpertstyle;override;stdcall;
   function GetIDstring:string;override;stdcall;
 end;

var
 FDllFunctions: TDllFunctions;

implementation

{$I Versions.inc}



{*****************************************************************}
constructor TJeBaseExpert.Create;
var
 Options: TMemoryStream;
begin
  {$IFDEF DEBUG}ShowMessage('1. Initializing Expert');{$ENDIF}
  if ToolServices = nil then
    Raise Exception.Create('Unable to find ToolServices -> No way of working correctly !!!');

  //Load Library
  {$IFDEF DEBUG}ShowMessage('2. Loading Library');{$ENDIF}
  FHandle := LoadLibrary(PChar(RC_ExpertDllName));
  if FHandle<>0 then
  begin
    {$IFDEF DEBUG}ShowMessage('3. Retrieving Entry Point');{$ENDIF}
    @FExpertDllEntry:=GetProcAddress(FHandle,'ExpertDllEntry');
    if Assigned(FExpertDllEntry) then
    begin
      {$IFDEF DEBUG}ShowMessage('4. Initializing Dll');{$ENDIF}
      FDelphiApis := TExportedApis.Create;
      FDllFunctions := FExpertDllEntry(FDelphiApis);
      if FDllFunctions.Version <> ExpertVersion then
        ShowMessage(RC_ExpertDllBadVersion)
      else
      begin
        //Launch dll
        {$IFDEF DEBUG}ShowMessage('5. Loading Options');{$ENDIF}
        Options := TMemoryStream.Create;
        FDelphiApis.GetOptions(TStream(Options));
        Options.Position:=0;
        {$IFDEF DEBUG}ShowMessage('6. Giving options to DLL');{$ENDIF}
        FDllFunctions.Initialize(Options);
        Options.Free;

        TForm(GetComponent('AppBuilder')).OnClose := FormClose;
        {$IFDEF DEBUG}ShowMessage('7. Registering Property Editors');{$ENDIF}
        RegisterEditors;
        {$IFDEF DEBUG}ShowMessage('8. Registering Component Editors');{$ENDIF}
        RegisterComponentEditors;
      end;
    end
    else
      ShowMessage(RC_ExpertDllCorrupted);
  end
  else
    ShowMessage(RC_ExpertDllFailed);
end;
{*****************************************************************}
destructor TJeBaseExpert.Destroy;
var
 Options:TMemoryStream;
begin
  try
    if FHandle<>0 then
    begin
      if Assigned(FExpertDllEntry) then
      begin
        //unload the dll
        Options := TMemoryStream.Create;
        FDllFunctions.Finalize(TStream(Options));
        FDelphiApis.SetOptions(TStream(Options));
        Options.Free;

        FDelphiApis.Free;
      end;
      FreeLibrary(FHandle);
      TForm(GetComponent('AppBuilder')).OnClose := nil;
    end;
  finally
    inherited;
  end;
end;
{*****************************************************************}
procedure TJeBaseExpert.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  try
    FDelphiApis.Free;
  except
  end;
end;
{*****************************************************************}
function TJeBaseExpert.GetAuthor: string;
begin
  result := 'Sébastien Buysse';
end;
{*****************************************************************}
function TJeBaseExpert.GetComponent(Name: string): TComponent;

  procedure GetIt(Parent: TComponent);
  var
   i:integer;
  begin
    result := Parent.FindComponent(Name);
    if result=nil then
    begin
      i:=0;
      while (i<Parent.ComponentCount) and (result=nil) do
      begin
        GetIt(Parent.Components[i]);
        inc(i);
      end;
    end;
  end;

begin
  GetIt(Application);
end;
{*****************************************************************}
function TJeBaseExpert.GetIDstring: string;
begin
  result := 'JeBaseExpert_ID';
end;
{*****************************************************************}
function TJeBaseExpert.GetName: string;
begin
  result := 'JeBaseExpert';
end;
{*****************************************************************}
function TJeBaseExpert.GetStyle: TExpertstyle;
begin
  result := esAddin;
end;
{*****************************************************************}

end.

