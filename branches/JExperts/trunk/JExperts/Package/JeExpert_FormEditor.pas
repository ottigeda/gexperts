{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_FormEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2002-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JeExpert_FormEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Forms, Dialogs, Registry, ExptIntf, EditIntf, ToolIntf;

type
 TJeFormEditor = class(TObject)
 private
   form:TIComponentInterface;
   ti:TIFormInterface;
   tis:TIModuleInterface;
 public
   Constructor Create(Source:String);
   Destructor Destroy;override;

   function GetMainForm: TForm;
   function GetComponent(index:integer):TIComponentInterface;
   function GetComponentCount:integer;
   function GetSelectedCount:integer;
   function GetSelected(index:integer):TIComponentInterface;
   function AddComponent(Parent:TIComponentInterface;ClassType:string;Left,Top,Width,Heigth:integer):TIComponentInterface;
 end;

implementation

{*****************************************************************}
function TJeFormEditor.AddComponent(Parent: TIComponentInterface;
  ClassType: string; Left, Top, Width,
  Heigth: integer): TIComponentInterface;
begin
  if ti=nil then
    result:=nil
  else
    result:=ti.CreateComponent(Parent,ClassType,Left,Top,Width,Heigth);
end;
{*****************************************************************}
constructor TJeFormEditor.Create(Source:string);
begin
  tis := ToolServices.GetModuleInterface(Source);
  if tis<>nil then
  begin
    ti := tis.GetFormInterface;
    if ti<>nil then
      form := ti.GetFormComponent;
  end;
end;
{*****************************************************************}
destructor TJeFormEditor.Destroy;
begin
  if form<>nil then form.Free;
  if ti<>nil then ti.Free;
  if tis<>nil then tis.Free;
  inherited;
end;
{*****************************************************************}
function TJeFormEditor.GetComponent(index: integer): TIComponentInterface;
begin
  if form<>nil then
    result:=form.GetComponent(index)
  else
    result:=nil;
end;
{*****************************************************************}
function TJeFormEditor.GetComponentCount: integer;
begin
  if form<>nil then
    result:=form.GetComponentCount
  else
    result:=0;
end;
{*****************************************************************}
function TJeFormEditor.GetMainForm: TForm;
begin
  if form<>nil then
    result:=TForm(form.GetComponentHandle)
  else
    result:=nil;
end;
{*****************************************************************}
function TJeFormEditor.GetSelected(index: integer): TIComponentInterface;
begin
  if ti<>nil then
    result:=ti.GetSelComponent(index)
  else
    result:=nil;
end;
{*****************************************************************}
function TJeFormEditor.GetSelectedCount: integer;
begin
  if ti<>nil then
    result:=ti.GetSelCount
  else
    result:=0;
end;
{*****************************************************************}
end.
