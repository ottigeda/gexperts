{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormSeparators.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2000202-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JeFormSeparators;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, JvSpeedButton, StdCtrls, ExtCtrls,
  JvDialogs, JvExStdCtrls, JvListBox, JvGroupBox, JvExControls;

type
  TfoSeparator = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    JvGroupBox1: TJvGroupBox;
    JvListbox1: TJvListbox;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    JvSpeedButton3: TJvSpeedButton;
    JvSpeedButton4: TJvSpeedButton;
    JvSpeedButton5: TJvSpeedButton;
    JvSpeedButton6: TJvSpeedButton;
    OpenDialog1: TJvOpenDialog;
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure JvSpeedButton2Click(Sender: TObject);
    procedure JvSpeedButton3Click(Sender: TObject);
    procedure JvSpeedButton4Click(Sender: TObject);
    procedure JvSpeedButton5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{************************************************************}

procedure TfoSeparator.JvSpeedButton1Click(Sender: TObject);
var
  st: string;
begin
  st := '';
  if InputQuery('Type your separator', 'Text : ', st) then
    self.JvListBox1.Items.add(st);
end;
{************************************************************}

procedure TfoSeparator.JvSpeedButton2Click(Sender: TObject);
begin
  self.JvListBox1.DeleteSelected;
end;
{************************************************************}

procedure TfoSeparator.JvSpeedButton3Click(Sender: TObject);
begin
  self.JvListBox1.MoveSelectedUp;
end;
{************************************************************}

procedure TfoSeparator.JvSpeedButton4Click(Sender: TObject);
begin
  self.JvListBox1.MoveSelectedDown;
end;
{************************************************************}

procedure TfoSeparator.JvSpeedButton5Click(Sender: TObject);
begin
  Form.FSeparators.Text := self.JvListbox1.Items.Text;
  Form.SaveSeparators;
  Form.LoadSeparators;
end;
{************************************************************}

procedure TfoSeparator.FormCreate(Sender: TObject);
begin
  self.JvListBox1.Items.Text := Form.FSeparators.Text;
end;
{************************************************************}
end.
