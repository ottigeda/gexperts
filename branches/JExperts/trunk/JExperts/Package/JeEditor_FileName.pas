{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeEditor_FileName.PAS, released on 2001-02-28.

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

unit JeEditor_FileName;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExptIntf,
  EditIntf, ToolIntf,  dialogs,
 {$IFDEF DELPHI5} DsgnIntf {$ENDIF} {$IFDEF DELPHI6} DesignEditors, DesignIntf {$ENDIF}
  ;

type
  TJeFileChooseProperty = class(TClassProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

implementation

{*************************************************}
function TJeFileChooseProperty.GetAttributes : TPropertyAttributes;
begin
  result:=[paMultiSelect, paDialog, paSortList];
end;
{*************************************************}
function TJeFileChooseProperty.GetValue : string;
begin
  result:=GetStrValue;
end;
{*************************************************}
procedure TJeFileChooseProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;
{*************************************************}
procedure TJeFileChooseProperty.GetValues(Proc: TGetStrProc);
begin
  SetStrValue('Choose a file ...');
end;
{*************************************************}
procedure TJeFileChooseProperty.Edit;
begin
  with TOpendialog.Create(nil) do
  begin
    Filter:='All files (*.*)|*.*|Executables (*.exe)|*.exe|Text Files (*.txt)|*.txt|Pascal units (*.pas)|*.pas|C/Cpp Files (*.c;*.cpp)|*.c;*.cpp';
    if Execute then
      SetStrValue(FileName);
    Free;
  end;
end;
{*************************************************}

end.
