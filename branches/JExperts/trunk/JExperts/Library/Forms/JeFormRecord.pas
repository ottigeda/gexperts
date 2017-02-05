{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormRecord.PAS, released on 2001-02-28.

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

unit JeFormRecord;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvButton, ExtCtrls, Buttons, JvSpeedButton, ComCtrls, JeExpert_Strings,
  JvListView, Menus, FileCtrl, ActnList,
  JvDialogs, JvExComCtrls, JvExControls;

type
  TfoRecord = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    JvListView1: TJvListView;
    JvSpeedButton3: TJvSpeedButton;
    JvSpeedButton4: TJvSpeedButton;
    JvSpeedButton5: TJvSpeedButton;
    JvSpeedButton6: TJvSpeedButton;
    PopupMenu1: TPopupMenu;
    Boolean1: TMenuItem;
    Byte1: TMenuItem;
    Char1: TMenuItem;
    Integer1: TMenuItem;
    LongInt1: TMenuItem;
    LonwWord1: TMenuItem;
    ShortInt1: TMenuItem;
    SmallInt1: TMenuItem;
    Word1: TMenuItem;
    Int641: TMenuItem;
    File1: TMenuItem;
    String1: TMenuItem;
    AnsiString1: TMenuItem;
    WideString1: TMenuItem;
    Custom1: TMenuItem;
    ActionList1: TActionList;
    Insert: TAction;
    Delete: TAction;
    OpenDialog1: TJvOpenDialog;
    SaveDialog1: TJvSaveDialog;
    procedure JvSpeedButton4Click(Sender: TObject);
    procedure JvSpeedButton3Click(Sender: TObject);
    procedure WideString1Click(Sender: TObject);
    procedure Custom1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure JvSpeedButton5Click(Sender: TObject);
    procedure JvSpeedButton6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetType(Value: string);
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.dfm}

{*********************************************************************}

procedure TfoRecord.JvSpeedButton4Click(Sender: TObject);
begin
  JvListView1.DeleteSelected;
end;
{*********************************************************************}

procedure TfoRecord.JvSpeedButton3Click(Sender: TObject);
var
  t: TListItem;
begin
  t := JvListView1.Items.Add;
  t.Caption := 'New_' + IntToStr(JvListView1.Items.Count);
  t.SubItems.Add('Integer');
  JvListView1.UnselectAll;
  t.MakeVisible(false);
  t.Selected := true;
  with JvSpeedButton3 do
    if Tag = 0 then
    begin
      Form.ToStatus(RCH_RightClickType);
      Tag := 1;
    end;
end;
{*********************************************************************}

procedure TfoRecord.WideString1Click(Sender: TObject);
begin
  SetType((Sender as TMenuItem).Caption);
end;
{*********************************************************************}

procedure TfoRecord.Custom1Click(Sender: TObject);
var
  st: string;
begin
  st := '';
  if InputQuery('Type', 'Enter your type', st) then
    SetType(st);
end;
{*********************************************************************}

procedure TfoRecord.PopupMenu1Popup(Sender: TObject);
var
  i: integer;
begin
  with (Sender as TPopupMenu) do
    for i := 0 to Items.Count - 1 do
      Items[i].Enabled := JvListView1.SelCount > 0;
end;
{*********************************************************************}

procedure TfoRecord.SetType(Value: string);
var
  i: integer;
begin
  for i := 0 to JvListView1.Items.Count - 1 do
    with JvListView1.Items[i] do
      if Selected then
        SubItems[0] := Value;
end;
{*********************************************************************}

procedure TfoRecord.JvSpeedButton1Click(Sender: TObject);
var
  i: integer;
begin
  //Test for bad inputs
  if Trim(Edit1.Text) = '' then
  begin
    Beep;
    Form.ToStatus(RCE_NoRecordName);
    Edit1.SetFocus;
    Exit;
  end;
  if JvListView1.Items.Count = 0 then
  begin
    Beep;
    Form.ToStatus(RCE_NoRecordItems);
    JvListView1.SetFocus;
    Exit;
  end;

  //Generate the code
  with TStringList.Create do
  begin
    Add('');
    Add(self.Edit1.Text + ' = record');
    for i := 0 to JvListView1.Items.Count - 1 do
      with JvListView1.Items[i] do
        Add('  ' + Caption + ': ' + SubItems[0] + ';');
    Add('end;');
    Add('');
    DelphiApis.EditorAddText(Text);
    Free;
    ModalResult := mrOk;
  end;
end;
{*********************************************************************}

procedure TfoRecord.JvSpeedButton5Click(Sender: TObject);
type
  TRecordType = record
    name: string[255];
    ctype: integer;
    ctypename: string[255];
  end;
var
  i: integer;
  fich: file of TRecordType;
  rec: TRecordType;
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
          case rec.ctype of
            0: SubItems.Add('Byte');
            1: SubItems.Add('Word');
            2: SubItems.Add('ShortInt');
            3: SubItems.Add('SmallInt');
            4: SubItems.Add('Cardinal');
            5: SubItems.Add('integer');
            6: SubItems.Add('Longint');
            7: SubItems.Add('LongWord');
            8: SubItems.Add('Int64');
            9: SubItems.Add('Char');
            10: SubItems.Add('Boolean');
            11: SubItems.Add('File');
            12: SubItems.Add('string');
            13: SubItems.Add('Pchar');
            14: SubItems.Add('Shortstring');
            15: SubItems.Add('Ansistring');
            16: SubItems.Add('Widestring');
            17: SubItems.Add(rec.ctypename);
          end;
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
//      if Root.TagName = 'RECORD' then
//      begin
//        for i := 0 to Root.Count - 1 do
//          with Root[i] do
//            with JvListView1.Items.Add do
//            try
//              Caption := Values['CAPTION'];
//              SubItems.Add(Values['TYPE']);
//            except
//              Free;
//            end;
//      end
//      else
//        Form.ToStatus(RCE_InvalidRecord);
//    end;
  end;
end;
{*********************************************************************}

procedure TfoRecord.JvSpeedButton6Click(Sender: TObject);
var
  i: integer;
begin
  if not SaveDialog1.Execute then
    exit;

// todo: enable
//  with JvXmlParser1 do
//  begin
//    Root.Clear;
//    Root.TagName := 'RECORD';
//    for i := 0 to JvListView1.Items.Count - 1 do
//      with Root.Add('ITEM') do
//      begin
//        Add('CAPTION', JvListView1.Items[i].Caption);
//        Add('TYPE', JvListView1.Items[i].SubItems[0]);
//      end;
//    SaveToFile(SaveDialog1.FileName);
//  end;
end;
{*********************************************************************}

procedure TfoRecord.FormCreate(Sender: TObject);
begin
  if DirectoryExists(DelphiApis.PackPath) then
  begin
    ForceDirectories(DelphiApis.PackPath + 'User\Record');
    OpenDialog1.InitialDir := DelphiApis.PackPath + 'User\Record';
    SaveDialog1.InitialDir := DelphiApis.PackPath + 'User\Record';
  end;
end;
{*********************************************************************}
end.
