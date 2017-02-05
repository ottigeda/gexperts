{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormLexical.PAS, released on 2001-02-28.

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

unit JeFormLexical;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus, clipbrd, JvEdit,
  ToolWin, ActnList, ImgList, JvExStdCtrls;

type
  TfoLexical = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Search1: TMenuItem;
    N1: TMenuItem;
    Quit1: TMenuItem;
    Edit1: TMenuItem;
    Copyfunctiondeclaration1: TMenuItem;
    Copyfunctionname1: TMenuItem;
    Copyfunctionhelp1: TMenuItem;
    FindDialog1: TFindDialog;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Memo1: TMemo;
    Panel5: TPanel;
    JvEdit1: TJvEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ActionList1: TActionList;
    Search: TAction;
    Quit: TAction;
    CopyDeclaration: TAction;
    CopyName: TAction;
    CopyHelp: TAction;
    ImageList1: TImageList;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FindDialog1Find(Sender: TObject);
    procedure SearchExecute(Sender: TObject);
    procedure CopyHelpExecute(Sender: TObject);
    procedure CopyNameExecute(Sender: TObject);
    procedure CopyDeclarationExecute(Sender: TObject);
    procedure QuitExecute(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure JvEdit1Change(Sender: TObject);
  private
  public
    LastItem: TTreeNode;
    LastText: string;
  end;

var
  foLexical: TfoLexical = nil;

implementation

{$R *.DFM}

{************************************************************}

procedure TfoLexical.FormCreate(Sender: TObject);
var
  i, j: integer;
// todo: enable
//  Elem, Elem2: TJeXmlElement;
  Node, Node2: TTreeNode;
  Stream: TResourceStream;
begin
  foLexical := self;

  Stream := TResourceStream.Create(HInstance, 'Jv_LEXICAL', RT_RCDATA);
// todo: enable
//  self.JvXmlParser1.LoadFromStream(Stream);
  Stream.Free;

// todo: enable
//  for i := 0 to self.JvXmlParser1.Root.Count - 1 do
//  begin
//    Elem := self.JvXmlParser1.Root[i];
//    Node := self.TreeView1.Items.AddChild(nil, Elem.Values['TEXT']);
//    for j := 0 to Elem.Count - 1 do
//      if Elem[j].TagName = 'NODE' then
//      begin
//        Elem2 := Elem[j];
//        Node2 := self.TreeView1.Items.AddChild(Node, Elem2.Values['TEXT']);
//        Node2.Data := Elem2;
//      end;
//  end;
end;
{************************************************************}

procedure TfoLexical.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := caFree;
  foLexical := nil;
end;
{************************************************************}

procedure TfoLexical.FindDialog1Find(Sender: TObject);
var
  found: boolean;
begin
  if (LastText <> self.FindDialog1.FindText) then
  begin
    LastText := self.FindDialog1.FindText;
    LastItem := self.TreeView1.Items.GetFirstNode;
  end;

  found := false;
// todo: enable
//  while (LastItem <> nil) and (not found) do
//  begin
//    if LastItem.HasChildren = false then
//      if (pos(UpperCase(LastText), UpperCase(TJeXmlElement(LastItem.Data).Values['DESCRIPTION'])) <> 0) then
//        found := true;
//    if not found then
//      LastItem := LastItem.GetNext;
//  end;

  if found then
  begin
    self.TreeView1.Selected := LastItem;
    self.TreeView1Change(self.TreeView1, LastItem);
    self.Memo1.SelStart := Pos(UpperCase(LastText), UpperCase(self.Memo1.Lines.Text)) - 1;
    self.Memo1.SelLength := Length(LastText);

    LastItem := LastItem.GetNext;
  end
  else
    beep;
end;
{************************************************************}

procedure TfoLexical.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  //Change edit and memo
  if not (Node.HasChildren) then
  begin
// todo: enable
//    self.Memo1.Lines.Text := TJeXmlElement(Node.Data).Values['DESCRIPTION'];
//    self.JvEdit1.Text := TJeXmlElement(Node.Data).Values['PROTO'];
  end;
end;
{************************************************************}

procedure TfoLexical.SearchExecute(Sender: TObject);
begin
  LastItem := self.TreeView1.Items.GetFirstNode;
  LastText := '';
  self.FindDialog1.Execute;
end;
{************************************************************}

procedure TfoLexical.CopyHelpExecute(Sender: TObject);
var
  st: string;
begin
  st := self.Memo1.Lines.Text;
  ClipBoard.SetTextBuf(PChar(st));
end;
{************************************************************}

procedure TfoLexical.CopyNameExecute(Sender: TObject);
var
  nom: string;
  i, j: integer;
begin
  nom := trim(self.JvEdit1.text);
  nom := trim(Copy(nom, pos(' ', nom) + 1, length(nom)));

  i := length(nom);
  j := 1;
  while (j < Length(nom)) and (not (nom[j] in [' ', '(', ';', ':'])) do
    inc(j);
  if (j < Length(nom)) then
    i := j;
  nom := Copy(nom, 1, i - 1);

  ClipBoard.SetTextBuf(PChar(nom));
end;
{************************************************************}

procedure TfoLexical.CopyDeclarationExecute(Sender: TObject);
var
  st: string;
begin
  st := self.JvEdit1.Text;
  ClipBoard.SetTextBuf(PChar(st));
end;
{************************************************************}

procedure TfoLexical.QuitExecute(Sender: TObject);
begin
  Close;
end;
{************************************************************}

procedure TfoLexical.Memo1Change(Sender: TObject);
begin
  CopyHelp.Enabled := Memo1.Lines.Count > 0;
end;
{************************************************************}

procedure TfoLexical.JvEdit1Change(Sender: TObject);
begin
  CopyDeclaration.Enabled := JvEdit1.Text <> '';
  CopyName.Enabled := CopyDeclaration.Enabled;
end;
{************************************************************}
end.
