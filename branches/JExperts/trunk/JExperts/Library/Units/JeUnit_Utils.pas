{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeUnit_Utils.PAS, released on 2001-02-28.

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

unit JeUnit_Utils;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Activex, ImgList, Registry;

function GenerateGUID: string;
function AutoSaveToInteger(Value: string): integer;

//LookTag -> Do not delete those with a tag not null
procedure MakeMenuEmpty(var Menu: TPopupMenu; LookTag: boolean = false);
procedure MakeItemEmpty(Item: TMenuItem; LookTag: boolean = false);

implementation

{*********************************************************************}

function GenerateGUID: string;
var
  Guid: TGuid;
  Str: array[0..255] of WideChar;
begin
  CoCreateGuid(Guid);
  StringFromGUID2(Guid, Str, 255);
  result := WideCharToString(Str);
end;
{*********************************************************************}

procedure MakeItemEmpty(Item: TMenuItem; LookTag: boolean);
var
  i: integer;
begin
  for i := Item.Count - 1 downto 0 do
    if (not LookTag) or (Item.Items[i].Tag = 0) then
    begin
      MakeItemEmpty(Item.Items[i], LookTag);
      Item.Items[i].Free;
    end;
end;
{*********************************************************************}

procedure MakeMenuEmpty(var Menu: TPopupMenu; LookTag: boolean);
var
  i: integer;
begin
  for i := Menu.Items.Count - 1 downto 0 do
    if (not LookTag) or (Menu.Items[i].Tag = 0) then
    begin
      MakeItemEmpty(Menu.Items[i], LookTag);
      Menu.Items[i].Free;
    end;
end;
{*********************************************************************}

function AutoSaveToInteger(Value: string): integer;
begin
  Value := Trim(Value);
  if Value = 'Save all files' then
    result := 0
  else if Value = 'Save current file' then
    result := 1
  else if Value = 'Save all files if existing physically' then
    result := 2
  else if Value = 'Save current file if existing physically' then
    result := 3
  else if Value = 'Just remind' then
    result := 4
  else
    result := -1;
end;
{*********************************************************************}
end.
