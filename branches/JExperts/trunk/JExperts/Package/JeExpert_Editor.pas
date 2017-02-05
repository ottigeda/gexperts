{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_Editor.PAS, released on 2001-02-28.

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

unit JeExpert_Editor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Forms, Dialogs, Registry, ExptIntf, EditIntf, ToolIntf;

type
 TJeEditor = class(TObject)
 private
   FModule:TIModuleInterface;
   FEdit:TIEditorInterface;
 public
   Constructor Create(Source:String);
   Destructor Destroy;override;

   function GetTextSelection(var Text:string):boolean;
   function InsertText(Text:string):boolean;
   function GetWholeText(var Text:string):boolean;
   function SetWholeText(Text: string):boolean;
   function DeleteSelected:boolean;
   function GetNumberOfLines:integer;
   function SetLine(Line: integer):boolean;
   function GetCurrentLine:integer;
 end;

implementation

{*****************************************************************}
constructor TJeEditor.Create;
begin
  FModule:=nil;
  FEdit:=nil;

  FModule:=ToolServices.GetModuleInterface(Source);
  if FModule=nil then exit;
  FEdit:=FModule.GetEditorInterface;
end;
{*****************************************************************}
function TJeEditor.DeleteSelected: boolean;
var
 before,after:integer;
 Viewer:TIEditView;
begin
  result:=false;
  if FEdit=nil then exit;

  with FEdit.CreateUndoableWriter do
  begin
    if FEdit.GetViewCount=0 then
    begin
      Free;
      Exit;
    end;
    Viewer:=FEdit.GetView(0);
    before:=Viewer.CharPosToPos(FEdit.BlockStart);
    after:=Viewer.CharPosToPos(FEdit.BlockAfter);
    CopyTo(before);
    DeleteTo(After);
    Viewer.Free;
    Free;
    result:=True;
  end;
  FModule.ShowSource;
end;
{*****************************************************************}
destructor TJeEditor.Destroy;
begin
  if FEdit<>nil then FEdit.Free;
  if FModule<>nil then FModule.Free;
  inherited;
end;
{*****************************************************************}
function TJeEditor.GetCurrentLine: integer;
begin
  result:=-1;
  if FEdit=nil then exit;
  result:=FEdit.GetBlockStart.Line
end;
{*****************************************************************}
function TJeEditor.GetNumberOfLines: integer;
begin
  result:=-1;
  if FEdit=nil then exit;
  result:=FEdit.LinesInBuffer;
end;
{*****************************************************************}
function TJeEditor.GetTextSelection(var Text: string): boolean;
var
 before,after:integer;
 Viewer:TIEditView;
 FReader:TIEditReader;
begin
  result:=false;
  if FEdit=nil then exit;

  Viewer:=FEdit.GetView(0);
  if Viewer=nil then
  begin
    FEdit.Free;
    FModule.Free;
    exit;
  end;
  before:=Viewer.CharPosToPos(FEdit.BlockStart);
  after:=Viewer.CharPosToPos(FEdit.BlockAfter);
  Viewer.Free;

  FReader:=FEdit.CreateReader;
  if FReader=nil then
  begin
    FEdit.Free;
    FModule.Free;
    exit;
  end;
  SetLength(Text,after-before);
  FReader.GetText(before,PChar(Text),after-before);
  FReader.Free;

  result:=True;
end;
{*****************************************************************}
function TJeEditor.GetWholeText(var Text: string): boolean;
var
 FReader:TIEditReader;
 ln,pos:integer;
 st:array [0..255] of char;
begin
  result:=false;
  if FEdit=nil then exit;

  FReader:=FEdit.CreateReader;
  if FReader=nil then
  begin
    FEdit.Free;
    FModule.Free;
    Exit;
  end;

  Text:='';ln:=1;pos:=0;
  while ln>0 do
  begin
    ln:=FReader.GetText(pos,st,255);
    st[ln]:=#0;
    Text:=Text+st;
    inc(pos,ln);
  end;

  FReader.Free;

  result:=True;
end;
{*****************************************************************}
function TJeEditor.InsertText(Text: string): boolean;
var
 after:integer;
 Viewer:TIEditView;
 FWriter:TIEditWriter;
 chartmp:TCharPos;
 tmpPos:TEditPos;
begin
  result:=false;
  if FEdit=nil then exit;

  Viewer:=FEdit.GetView(0);
  if Viewer=nil then
  begin
    FEdit.Free;
    FModule.Free;
    exit;
  end;
  tmpPos:=Viewer.CursorPos;
  Viewer.ConvertPos(true,tmpPos,chartmp);
  after:=Viewer.CharPosToPos(chartmp);
  Viewer.Free;

  FWriter:=FEdit.CreateUndoableWriter;
  if FWriter=nil then
  begin
    FEdit.Free;
    FModule.Free;
    Exit;
  end;
  FWriter.CopyTo(after);
  FWriter.Insert(PChar(Text));

  FModule.ShowSource;
  FWriter.Free;

  result:=True;
end;
{*****************************************************************}
function TJeEditor.SetLine(Line: integer): boolean;
var
 View:TIEditView;
 NewPos:TEditPos;
begin
  result:=false;
  if FEdit=nil then exit;

  View:=FEdit.GetView(0);
  if View=nil then
  begin
    FEdit.Free;
    FModule.Free;
    exit;
  end;

  NewPos.Line:=Line;
  NewPos.Col:=1;
  View.CursorPos:=NewPos;
  View.TopPos:=NewPos;

  FModule.ShowSource;
  View.Free;
end;
{*****************************************************************}
function TJeEditor.SetWholeText(Text: string): boolean;
var
 Writer:TIEditWriter;
begin
  result:=false;
  if FEdit=nil then exit;

  Writer:=FEdit.CreateUndoableWriter;
  if Writer=nil then
  begin
    FEdit.Free;
    FModule.Free;
    exit;
  end;

  Writer.CopyTo(0);
  Writer.DeleteTo(MAXINT);

  FModule.ShowSource;
  Writer.Free;

  result:=InsertText(Text);
end;
{*****************************************************************}
end.
