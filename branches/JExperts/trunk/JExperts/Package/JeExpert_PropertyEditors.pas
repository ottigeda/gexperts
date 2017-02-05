{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_PropertyEditors.PAS, released on 2001-02-28.

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

unit JeExpert_PropertyEditors;

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
  Buttons,
  DesignIntf;

procedure RegisterEditors;

implementation

uses
  JeEditor_Multiline, JeEditor_FileName, JeEditor_HelpFile, JeEditor_Time,
  JeEditor_Date, JeEditor_DateTime, JeEditor_Cursor, JeEditor_Filter;

{*************************************************}
procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'Hint',TJeHintEditor);
  RegisterPropertyEditor(TypeInfo(TCaption),TPersistent,'Caption',TJeHintEditor);

  RegisterPropertyEditor(TypeInfo(TFileName),nil,'',TJeFileChooseProperty);
  RegisterPropertyEditor(TypeInfo(string),TPersistent,'HelpFile',TJeHelpChooseEditor);
  RegisterPropertyEditor(TypeInfo(TTime),TPersistent,'',TJeTimeEditor);
  RegisterPropertyEditor(TypeInfo(TDate),TPersistent,'',TJeDateEditor);
  RegisterPropertyEditor(TypeInfo(TDateTime),TPersistent,'',TJeTimeDateEditor);
  RegisterPropertyEditor(TypeInfo(TCursor),TPersistent,'',TJeCursorEditor);

  RegisterPropertyEditor(TypeInfo(string),TPersistent,'Filter',TJeFilterEditor);
  RegisterPropertyEditor(TypeInfo(string),TOpenDialog,'Filter',TJeFilterEditor);
  RegisterPropertyEditor(TypeInfo(string),TSaveDialog,'Filter',TJeFilterEditor);
end;
{*************************************************}
end.

