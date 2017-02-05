{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormRuntime.PAS, released on 2001-02-28.

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

unit JeFormRuntime;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvButton, Buttons, JvSpeedButton, Menus, ExtCtrls,
  ImgList, ComCtrls, ToolWin, ActnList;

type
  TfoRuntime = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Search1: TMenuItem;
    N1: TMenuItem;
    Quit1: TMenuItem;
    FindDialog1: TFindDialog;
    Label1: TLabel;
    ComboBox1: TComboBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    Search: TAction;
    Quit: TAction;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FindDialog1Show(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FNextElm: integer;
    FLastSearch: string;
  public
  end;

var
  foRuntime: TfoRuntime = nil;

implementation

{$R *.DFM}

{************************************************************}

procedure TfoRuntime.ComboBox1Change(Sender: TObject);
begin
  if self.ComboBox1.Text = '' then
    exit;
// todo: enable
//  with self.JvXmlParser1.Root.ElementNamed[self.ComboBox1.Text] do
//  begin
//    Edit1.Text := Values['TYPE'];
//    Edit2.Text := Values['NAME'];
//    Memo1.Lines.Text := Values['TXT'];
//  end;
end;
{************************************************************}

procedure TfoRuntime.FormCreate(Sender: TObject);
var
  Stream: TResourceStream;
begin
  foRuntime := self;
  Stream := TResourceStream.Create(HInstance, 'Jv_RUNTIME', RT_RCDATA);
// todo: enable
//  self.JvXmlParser1.LoadFromStream(Stream);
//  self.ComboBox1.Items.Text := self.JvXmlParser1.Root.GetElementList;
  Stream.Free;
end;
{************************************************************}

procedure TfoRuntime.Quit1Click(Sender: TObject);
begin
  self.Close;
end;
{************************************************************}

procedure TfoRuntime.Search1Click(Sender: TObject);
begin
  if self.FindDialog1.Execute then
  begin

  end;
end;
{************************************************************}

procedure TfoRuntime.FindDialog1Find(Sender: TObject);
begin
  if FLastSearch <> FindDialog1.FindText then
  begin
    FNextElm := 0;
    FLastSearch := FindDialog1.FindText;
  end;

// todo: enable
//  while (FNextElm < self.JvXmlParser1.Root.Count) and
//    (Pos(FindDialog1.FindText, self.JvXmlParser1.Root.Elements[FNextElm].Values['TXT']) = 0) do
//    inc(FNextElm);
//
//  if (FNextElm < self.JvXmlParser1.Root.Count) then
//  begin
//    self.ComboBox1.ItemIndex := FNextElm;
//    self.ComboBox1Change(self.ComboBox1);
//    self.Memo1.SelStart := Pos(FLastSearch, self.Memo1.Lines.Text) - 1;
//    self.Memo1.SelLength := Length(FLastSearch);
//  end
//  else
//    beep;

  inc(FNextElm);
end;
{************************************************************}

procedure TfoRuntime.FindDialog1Show(Sender: TObject);
begin
  FNextElm := 0;
  FLastSearch := '';
end;
{************************************************************}

procedure TfoRuntime.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := caFree;
  foRuntime := nil;
end;
{************************************************************}
end.
