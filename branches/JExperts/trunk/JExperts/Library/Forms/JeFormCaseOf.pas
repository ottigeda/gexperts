{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormCaseOf.PAS, released on 2001-02-28.

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

unit JeFormCaseOf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, StdCtrls, JvButton, ComCtrls, Buttons,
  JvSpeedButton, JvListView, JeExpert_Strings, FileCtrl,
  ActnList, JvExComCtrls, JvExControls, JvDialogs;

type
  TfoCaseOf = class(TForm)
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
    PopupMenu1: TPopupMenu;
    CheckBox1: TCheckBox;
    SingleLineofCode1: TMenuItem;
    MultipleLinesofCode1: TMenuItem;
    ActionList1: TActionList;
    Insert: TAction;
    Delete: TAction;
    OpenDialog1: TJvOpenDialog;
    SaveDialog1: TJvSaveDialog;
    procedure SingleLineofCode1Click(Sender: TObject);
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

{************************************************************}

procedure TfoCaseOf.SingleLineofCode1Click(Sender: TObject);
const
  CS_LinesTxt: array[0..1] of string = ('Single Line', 'Multiple Lines');
var
  i: integer;
begin
  for i := 0 to JvListView1.Items.Count - 1 do
  begin
    if JvListView1.Items[i].Selected = false then
      Continue;
    JvListView1.Items[i].ImageIndex := (Sender as TMenuItem).Tag;
    JvListView1.Items[i].SubItems[0] := CS_LinesTxt[JvListView1.Items[i].ImageIndex];
  end;
end;
{************************************************************}

procedure TfoCaseOf.JvSpeedButton4Click(Sender: TObject);
begin
  JvListView1.DeleteSelected;
end;
{************************************************************}

procedure TfoCaseOf.JvSpeedButton3Click(Sender: TObject);
begin
  if JvSpeedButton3.Tag = 0 then
  begin
    Form.ToStatus(RCH_RightClickCode);
    JvSpeedButton3.Tag := 1;
  end;

  with JvListView1.Items.Add do
  begin
    Caption := 'New_' + IntToStr(JvListView1.Items.Count);
    SubItems.Add('Single Line');
    JvListView1.UnselectAll;
    MakeVisible(false);
    Selected := true;
  end;
end;
{************************************************************}

procedure TfoCaseOf.JvSpeedButton5Click(Sender: TObject);
const
  CS_LinesTxt: array[0..1] of string = ('Single Line', 'Multiple Lines');
type
  TCaseRecord = record
    Name: string[255];
    Value: integer; //0 = simple instruction; 1 = Multiple instruction
  end;
var
  i: integer;
  fich: file of TCaseRecord;
  rec: TCaseRecord;
begin
  if not OpenDialog1.Execute then
    exit;
  if OpenDialog1.FilterIndex = 2 then
  begin
    try
      AssignFile(fich, OpenDialog1.FileName);
      Reset(fich);
      while not (eof(fich)) do
      begin
        Read(fich, rec);
        with JvListView1.Items.Add do
        begin
          Caption := rec.name;
          if rec.Value > 1 then
            rec.Value := 1;
          ImageIndex := rec.Value;
          SubItems.Add(CS_LinesTxt[rec.Value]);
        end;
      end;
      CloseFile(fich);
    except
    end;
  end
  else
  begin
// todo: enable
//    with JvXmlParser1 do
//    begin
//      LoadFromFile(OpenDialog1.FileName);
//      if Root.TagName = 'CASE' then
//      begin
//        for i := 0 to Root.Count - 1 do
//          with Root[i] do
//            with JvListView1.Items.Add do
//            try
//              Caption := Values['NAME'];
//              ImageIndex := StrToIntDef(Values['TYPE'], 0);
//              SubItems.Add(CS_LinesTxt[ImageIndex]);
//            except
//              Free;
//            end;
//      end
//      else
//        Form.ToStatus(RCE_InvalidCase);
//    end;
  end;
end;
{************************************************************}

procedure TfoCaseOf.JvSpeedButton6Click(Sender: TObject);
var
  i: integer;
begin
  if SaveDialog1.Execute = false then
    exit;

//  with JvXmlParser1 do
//  begin
//    Root.Clear;
//    Root.TagName := 'CASE';
//    for i := 0 to JvListView1.Items.Count - 1 do
//      with Root.Add('ITEM') do
//      begin
//        Add('NAME', JvListView1.Items[i].Caption);
//        Add('TYPE', IntToStr(JvListView1.Items[i].ImageIndex));
//      end;
//    SaveToFile(SaveDialog1.FileName);
//  end;
end;
{************************************************************}

procedure TfoCaseOf.JvSpeedButton1Click(Sender: TObject);
var
  i: integer;
begin
  //Test for bad inputs
  if Trim(Edit1.Text) = '' then
  begin
    Beep;
    Form.ToStatus(RCE_NoVariable);
    Edit1.SetFocus;
    Exit;
  end;
  if JvListView1.Items.Count = 0 then
  begin
    Beep;
    Form.ToStatus(RCE_NoCaseItems);
    JvListView1.SetFocus;
    Exit;
  end;

  //Generate the code
  with TStringList.Create do
  begin
    Add('');

    Add('  case ' + Edit1.Text + ' of ');
    for i := 0 to JvListView1.Items.Count - 1 do
      if JvListView1.Items[i].ImageIndex = 0 then
        Add('   ' + JvListView1.Items[i].Caption + ' : ;')
      else
      begin
        Add('   ' + JvListView1.Items[i].Caption + ' : ');
        Add('       begin');
        Add('         //Place your code here');
        Add('       end;');
      end;
    if CheckBox1.Checked then
    begin
      Add('   else');
      Add('     begin');
      Add('       //If the value is not one of the other''s');
      Add('     end;');
    end;
    Add('  end;');

    Add('');

    DelphiApis.EditorAddText(Text);
    Free;
    ModalResult := mrOk;
  end;
end;
{************************************************************}

procedure TfoCaseOf.FormCreate(Sender: TObject);
begin
  if DirectoryExists(DelphiApis.PackPath) then
  begin
    ForceDirectories(DelphiApis.PackPath + 'User\Case');
    OpenDialog1.InitialDir := DelphiApis.PackPath + 'User\Case';
    SaveDialog1.InitialDir := DelphiApis.PackPath + 'User\Case';
  end;
end;
{************************************************************}
end.
