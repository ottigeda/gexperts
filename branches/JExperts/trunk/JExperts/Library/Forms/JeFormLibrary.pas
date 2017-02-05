{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormLibrary.PAS, released on 2001-02-28.

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

unit JeFormLibrary;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ComCtrls, ExtCtrls, ActnList, ToolWin,
  ImgList, FileCtrl, JvExComCtrls, JvComCtrls;

type
  TfoLibrary = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    JvTreeView1: TJvTreeView;
    Splitter1: TSplitter;
    Memo1: TMemo;
    Quit1: TMenuItem;
    ActionList1: TActionList;
    Quit: TAction;
    AddCategory: TAction;
    Items1: TMenuItem;
    AddItem: TAction;
    Delete: TAction;
    AddCategory1: TMenuItem;
    AddItem1: TMenuItem;
    N2: TMenuItem;
    Delete1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ImageList1: TImageList;
    procedure FormShow(Sender: TObject);
    procedure Reset;
    procedure QuitExecute(Sender: TObject);
    procedure AddCategoryExecute(Sender: TObject);
    procedure AddItemExecute(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure JvTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Memo1Change(Sender: TObject);
  private
    procedure AddItems(Value: TTreeNode);
    function GetPathFor(Node: TTreeNode): string;
  public
  end;

implementation

uses
  JeFormPopup;

resourcestring
  CT_FilesLibrary = '.SRC';

{$R *.DFM}

  {*********************************************************************}

procedure TfoLibrary.FormShow(Sender: TObject);
begin
  Reset;
end;
{*********************************************************************}

procedure TfoLibrary.Reset;
begin
  JvTreeView1.Items.Clear;
  with TStringList.Create do
  begin
    Text := Form.GetLibraryAreas;
    while Count > 0 do
    begin
      AddItems(JvTreeView1.Items.Add(nil, Strings[0]));
      Delete(0);
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoLibrary.QuitExecute(Sender: TObject);
begin
  Close;
end;
{*********************************************************************}

procedure TfoLibrary.AddCategoryExecute(Sender: TObject);
var
  st: string;

  function NotExist(Value: string): boolean;
  var
    Item: TTreeNode;
  begin
    Value := UpperCase(Value);
    Item := JvTreeView1.TopItem;
    result := true;
    while Item <> nil do
    begin
      result := result and (UpperCase(Item.Text) <> Value);
      Item := Item.getNextSibling;
    end;
  end;

begin
  st := '';
  if InputQuery('Add Category', 'Enter the name of the category', st) then
    if NotExist(st) then
    begin
      JvTreeView1.Items.Add(nil, st);
      ForceDirectories(DelphiApis.PackPath + 'Library\' + st);
    end;
end;
{*********************************************************************}

procedure TfoLibrary.AddItems(Value: TTreeNode);
var
  Path: string;
  SearchRec: TSearchRec;
  res: integer;
begin
  Path := DelphiApis.PackPath + 'Library\' + Value.Text + '\';
  res := FindFirst(Path + '*.*', faAnyFile, SearchRec);
  while res = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and (faDirectory and SearchRec.Attr = 0) then
      if UpperCase(ExtractFileExt(SearchRec.Name)) = CT_FilesLibrary then
        JvTreeView1.Items.AddChild(Value, ChangeFileExt(SearchRec.Name, ''));
    res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;
{*********************************************************************}

procedure TfoLibrary.AddItemExecute(Sender: TObject);
var
  st: string;
  Node: TTreeNode;

  function NotExists(Value: string): boolean;
  var
    Item: TTreeNode;
  begin
    Value := UpperCase(Value);
    Item := Node;
    result := true;
    while Item <> nil do
    begin
      result := result and (UpperCase(Item.Text) <> Value);
      Item := Item.getNextSibling;
    end;
  end;

begin
  st := '';
  Node := JvTreeView1.Selected;
  if Node.Parent <> nil then
    Node := Node.Parent;
  if InputQuery('Add Item', 'Enter the name of the item', st) then
    if NotExists(st) then
    begin
      Node := JvTreeView1.Items.AddChild(Node, st);
      with TStringList.Create do
      begin
        SaveToFile(GetPathFor(Node));
        Free;
      end;
      JvTreeView1.Selected := Node;
    end;
end;
{*********************************************************************}

procedure TfoLibrary.DeleteExecute(Sender: TObject);
var
  Child: TTreeNode;
begin
  if JvTreeView1.Selected = nil then
  begin
    Beep;
    Exit;
  end;
  with CreateMessageDialog('Do you really want to delete the item ?', mtConfirmation, [mbYes, mbNo, mbCancel]) do
  try
    Position := poScreenCenter;
    if (ShowModal = mrYes) then
    begin
      if JvTreeView1.Selected.Parent = nil then
      begin
        Child := JvTreeView1.Selected.getFirstChild;
        while Child <> nil do
        begin
          try
            DeleteFile(GetPathFor(Child));
          except
          end;
          Child := Child.getNextSibling;
        end;
        try
          RemoveDirectory(PChar(GetPathFor(JvTreeView1.Selected)));
        except
        end;
      end
      else
      try
        Deletefile(GetPathFor(JvTreeView1.Selected));
      except
      end;
      JvTreeView1.Selected.Delete;
    end;
  finally
    Free;
  end;
end;
{*********************************************************************}

procedure TfoLibrary.JvTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if (Node = nil) or (Node.Parent = nil) then
  begin
    Memo1.Clear;
    Memo1.Enabled := false;
  end
  else
  begin
    Memo1.Enabled := true;
    try
      Memo1.OnChange := nil;
      Memo1.Lines.LoadFromFile(GetPathFor(Node));
      Memo1.OnChange := Memo1Change;
    except
      Memo1.Clear;
    end;
  end;
end;
{*********************************************************************}

function TfoLibrary.GetPathFor(Node: TTreeNode): string;
begin
  result := DelphiApis.PackPath + 'Library\';
  if Node.Parent = nil then
    result := result + Node.Text + '\'
  else
    result := result + Node.Parent.Text + '\' + Node.Text + CT_FilesLibrary;
end;
{*********************************************************************}

procedure TfoLibrary.Memo1Change(Sender: TObject);
begin
  if JvTreeView1.Selected <> nil then
    Memo1.Lines.SaveToFile(GetPathFor(JvTreeView1.Selected));
end;
{*********************************************************************}
end.
