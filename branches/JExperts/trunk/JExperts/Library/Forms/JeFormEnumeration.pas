{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormEnumeration.PAS, released on 2001-02-28.

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

unit JeFormEnumeration;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvButton, ExtCtrls, JvListView, Buttons,
  JvSpeedButton, JeExpert_Strings, FileCtrl, ActnList,
  JvDialogs, JvExComCtrls, JvExControls;

type
  TfoEnumeration = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    JvSpeedButton3: TJvSpeedButton;
    JvSpeedButton4: TJvSpeedButton;
    JvSpeedButton5: TJvSpeedButton;
    JvSpeedButton6: TJvSpeedButton;
    JvListView1: TJvListView;
    Label2: TLabel;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    ActionList1: TActionList;
    Insert: TAction;
    Delete: TAction;
    OpenDialog1: TJvOpenDialog;
    SaveDialog1: TJvSaveDialog;
    procedure CheckBox1Click(Sender: TObject);
    procedure JvSpeedButton4Click(Sender: TObject);
    procedure JvSpeedButton3Click(Sender: TObject);
    procedure JvSpeedButton5Click(Sender: TObject);
    procedure JvSpeedButton6Click(Sender: TObject);
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{*********************************************************************}

procedure TfoEnumeration.CheckBox1Click(Sender: TObject);
begin
  self.Edit2.Enabled := self.CheckBox1.Checked;
end;
{*********************************************************************}

procedure TfoEnumeration.JvSpeedButton4Click(Sender: TObject);
begin
  JvListView1.DeleteSelected;
end;
{*********************************************************************}

procedure TfoEnumeration.JvSpeedButton3Click(Sender: TObject);
var
  t: TListItem;
begin
  t := JvListView1.Items.Add;
  t.Caption := 'New_' + IntToStr(JvListView1.Items.Count);
  JvListView1.UnselectAll;
  t.MakeVisible(false);
  t.Selected := true;
end;
{*********************************************************************}

procedure TfoEnumeration.JvSpeedButton1Click(Sender: TObject);
var
  st: string;
  i: integer;

  function BlankString(Count: integer): string;
  begin
    result := Format('%' + IntToStr(Count) + 's', [' ']);
  end;

begin
  //Test for bad inputs
  if Trim(Edit1.Text) = '' then
  begin
    Beep;
    Form.ToStatus(RCE_NoEnumName);
    Edit1.SetFocus;
    Exit;
  end;
  if (Trim(Edit2.Text) = '') and (CheckBox1.Checked) then
  begin
    Beep;
    Form.ToStatus(RCE_NoSetName);
    Edit2.SetFocus;
    Exit;
  end;
  if JvListView1.Items.Count = 0 then
  begin
    Beep;
    Form.ToStatus(RCE_NoSetItems);
    JvListView1.SetFocus;
    Exit;
  end;

  //Generate the code
  with TStringList.Create do
  begin
    Add('');

    st := ' ' + Edit1.Text + ' = (';
    for i := 0 to JvListView1.Items.Count - 1 do
    begin
      if Length(st) >= 75 then
      begin
        Add(st);
        st := BlankString(Length(Edit1.Text) + 5);
      end;
      st := st + JvListView1.Items[i].Caption;
      if i <> JvListView1.Items.Count - 1 then
        st := st + ', ';
    end;
    st := st + ');';
    Add(st);
    if CheckBox1.Checked then
      Add(' ' + Edit2.Text + ' = set of ' + Edit1.Text + ';');

    Add('');
    DelphiApis.EditorAddText(Text);
    Free;
    ModalResult := mrOk;
  end;
end;
{*********************************************************************}

procedure TfoEnumeration.JvSpeedButton5Click(Sender: TObject);
var
  i: integer;
begin
  if not OpenDialog1.Execute then
    exit;
  if OpenDialog1.FilterIndex = 1 then
  begin
// todo: enable
//    with JvXmlParser1 do
//    begin
//      LoadFromFile(OpenDialog1.FileName);
//      if Root.TagName <> 'ENUMERATION' then
//        exit;
//      for i := 0 to Root.Count - 1 do
//        with JvListView1.Items.Add do
//          Caption := Root.Elements[i].Text;
//    end;
  end
  else
  begin
    with TStringList.Create do
    begin
      LoadFromFile(OpenDialog1.FileName);
      for i := 0 to Count - 1 do
        with JvListView1.Items.Add do
          Caption := Strings[i];
      Free;
    end;
  end;
end;
{*********************************************************************}

procedure TfoEnumeration.JvSpeedButton6Click(Sender: TObject);
var
  i: integer;
begin
  if not SaveDialog1.Execute then
    exit;
// todo: enable
//  with JvXmlParser1 do
//  begin
//    Root.Clear;
//    Root.TagName := 'ENUMERATION';
//    for i := 0 to JvListView1.Items.Count - 1 do
//      Root.Add('ITEM', JvListView1.Items[i].Caption);
//    SaveToFile(SaveDialog1.FileName);
//  end;
end;
{*********************************************************************}

procedure TfoEnumeration.FormCreate(Sender: TObject);
begin
  if DirectoryExists(DelphiApis.PackPath) then
  begin
    ForceDirectories(DelphiApis.PackPath + 'User\Enums');
    OpenDialog1.InitialDir := DelphiApis.PackPath + 'User\Enums';
    SaveDialog1.InitialDir := DelphiApis.PackPath + 'User\Enums';
  end;
end;
{*********************************************************************}
end.
