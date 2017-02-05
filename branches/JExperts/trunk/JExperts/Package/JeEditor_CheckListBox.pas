{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeEditor_CheckListBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2002-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit JeEditor_CheckListBox;

interface

{$I JEDI.inc}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ExptIntf,
  Dialogs,
  EditIntf,
  ToolIntf,
  TypInfo,
  DesignEditors,
  DesignIntf,
  Contnrs;

type
 TJeCheckListBoxEditor = class(TDefaultEditor)
 private
   procedure TestProp(const PropertyEditor:IProperty);
 public
   function GetVerbCount : integer;override;
   function GetVerb(Index:integer) : string;override;
   procedure ExecuteVerb(Index:integer);override;
   procedure Edit; override;
 end;


implementation

uses
 checklst;

{*************************************************}
procedure TJeCheckListBoxEditor.Edit;
begin
  ExecuteVerb(1);
end;
{*************************************************}
procedure TJeCheckListBoxEditor.ExecuteVerb(Index: integer);
var
 Compos:  IDesignerSelections;
begin
  with Component as TCheckListBox do
    case Index of
      0 : Items.Clear;
      1 : begin
            Compos := TDesignerSelections.Create;
            Compos.Add(Component);
            GetComponentProperties(Compos,[tkClass],Designer,TestProp);
         end;
    end;
  Designer.Modified;
end;
{*************************************************}
function TJeCheckListBoxEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0 : result:='Clear';
    1 : result:='Edit...';
  end;
end;
{*************************************************}
function TJeCheckListBoxEditor.GetVerbCount: integer;
begin
  result := 2;
end;
{*************************************************}
procedure TJeCheckListBoxEditor.TestProp(const PropertyEditor: IProperty);
var
 prop: string;
begin
  prop := PropertyEditor.GetName;
  if CompareText(prop,'ITEMS')=0 then
     PropertyEditor.Edit;
end;

end.
