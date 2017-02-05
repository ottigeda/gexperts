{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeEditor_Cursor.PAS, released on 2001-02-28.

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

unit JeEditor_Cursor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExptIntf,
  EditIntf, ToolIntf
      {$IFDEF DELPHI5} ,DsgnIntf {$ENDIF} {$IFDEF DELPHI6} ,DesignEditors, DesignIntf {$ENDIF}
;

type
 TJeCursorEditor = class(TClassProperty)
 public
   function GetAttributes : TPropertyAttributes; override;
   function GetValue : string; override;
   procedure GetValues(Proc: TGetStrProc); override;
   procedure SetValue(const Value: string); override;
   procedure Edit; override;
 end;

implementation

uses
 JeExpert_Base;

var
 C_Cursor : array[0..22] of string = ('crDefault', 'crNone', 'crArrow',
      'crCross', 'crIBeam', 'crSize', 'crSizeNESW', 'crSizeNS', 'crSizeNWSE',
      'crSizeWE', 'crUpArrow', 'crHourGlass', 'crDrag', 'crNoDrop', 'crHSplit',
      'crVSplit', 'crMultiDrag', 'crSQLWait', 'crNo', 'crAppStart','crHelp',
      'crHandPoint', 'crSizeAll');

{*************************************************}
function TJeCursorEditor.GetAttributes : TPropertyAttributes;
begin
  result:=[paMultiSelect, paDialog, paValueList ,paSortList];
end;
{*************************************************}
function TJeCursorEditor.GetValue : string;
var
 i: integer;
begin
  i := Abs(GetOrdValue);
  if i>22 then result := ''
  else result := C_Cursor[i];
end;
{*************************************************}
procedure TJeCursorEditor.SetValue(const Value: string);
var
 i: integer;
begin
  for i:=0 to 22 do
  begin
    if UpperCase(C_Cursor[i])=UpperCase(Value) then
    begin
      SetOrdValue(-i);
      exit;
    end;
  end;
end;
{*************************************************}
procedure TJeCursorEditor.GetValues(Proc: TGetStrProc);
var
 i: integer;
begin
  for i:=0 to 22 do
    Proc(C_Cursor[i]);
end;
{*************************************************}
procedure TJeCursorEditor.Edit;
begin
  SetOrdValue(FDllFunctions.EditorCursor(GetOrdValue));
end;
{*************************************************}
end.

