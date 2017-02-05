{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormTransHeader.PAS, released on 2001-02-28.

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

unit JeFormTransHeader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JeClass_TransHeader, JeExpert_Strings,
  Buttons, JvSpeedButton, StdCtrls, ActnList, ExtCtrls, JvDialogs, Mask, JvExMask,
  JvToolEdit, JvExControls, JvBaseDlg;

type
  TfoTransHeader = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    ActionList1: TActionList;
    CheckAll: TAction;
    CheckNone: TAction;
    Label1: TLabel;
    JvFileNameBox1: TJvFileNameEdit;
    ListBox1: TListBox;
    JvSaveDialog1: TJvSaveDialog;
    CheckBox1: TCheckBox;
    procedure JvSpeedButton1Click(Sender: TObject);
  private
    procedure HeaderInfo(Sender: TObject; Text: string);
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}
{***************************************************}

procedure TfoTransHeader.JvSpeedButton1Click(Sender: TObject);
begin
  if JvFileNameBox1.FileName = '' then
  begin
    Form.ToStatus(RCE_NoFile);
    Exit;
  end;
  if not (JvSaveDialog1.Execute) then
    Exit;

  ListBox1.Clear;
  GroupBox3.Visible := true;
  with TJeHeaderTranslator.Create(nil) do
  begin
    Prepro := CheckBox1.Checked;
    OnInfo := HeaderInfo;
    Translatefile(JvFileNameBox1.FileName, JvSaveDialog1.FileName);
    Free;
  end;
  GroupBox3.Visible := false;
end;
{***************************************************}

procedure TfoTransHeader.HeaderInfo(Sender: TObject; Text: string);
begin
  ListBox1.ItemIndex := ListBox1.Items.Add(Text);
end;
{***************************************************}
end.
