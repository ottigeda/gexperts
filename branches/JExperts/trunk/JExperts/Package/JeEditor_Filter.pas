{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeEditor_Filter.PAS, released on 2001-02-28.

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

unit JeEditor_Filter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExptIntf,
  EditIntf, ToolIntf
      {$IFDEF DELPHI5} ,DsgnIntf {$ENDIF} {$IFDEF DELPHI6} ,DesignEditors, DesignIntf {$ENDIF}
;  

type
 TJeFilterEditor = class(TClassProperty)
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

{*************************************************}
function TJeFilterEditor.GetAttributes : TPropertyAttributes;
begin
  result:=[paMultiSelect, paDialog, paSortList];
end;
{*************************************************}
function TJeFilterEditor.GetValue : string;
begin
  result:=GetStrValue;
end;
{*************************************************}
procedure TJeFilterEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;
{*************************************************}
procedure TJeFilterEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue('Edit...');
end;
{*************************************************}
procedure TJeFilterEditor.Edit;
begin
  SetStrValue(FDllFunctions.EditorFilters(GetStrValue));
end;
{*************************************************}
end.
