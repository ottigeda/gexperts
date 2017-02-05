{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormCursorEditor.PAS, released on 2001-02-28.

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

unit JeFormCursorEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvButton, Buttons, JvSpeedButton, JvExControls;

type
  TfoCursorEditor = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Panel1: TPanel;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    procedure ListBox1Click(Sender: TObject);
  private
  public
  end;

const
  IDC_NODROP = PChar(32767);
  IDC_DRAG = PChar(32766);
  IDC_HSPLIT = PChar(32765);
  IDC_VSPLIT = PChar(32764);
  IDC_MULTIDRAG = PChar(32763);
  IDC_SQLWAIT = PChar(32762);
  IDC_HANDPT = PChar(32761);

implementation

{$R *.DFM}

{*********************************************************************}

procedure TfoCursorEditor.ListBox1Click(Sender: TObject);
begin
  Panel1.Cursor := TCursor(-Listbox1.ItemIndex);
end;
{*********************************************************************}
end.
