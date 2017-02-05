{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormHint.PAS, released on 2001-02-28.

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

unit JeFormHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JvButton, JvMemo, ComCtrls, Buttons, ToolWin, Menus,
  JvSpeedButton, ActnList, ImgList, JvDialogs, JvExControls;

type
  TfoHintForm = class(TForm)
    Panel1: TPanel;
    JvGroupBox1: TGroupBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    Panel2: TPanel;
    ToolButton2: TToolButton;
    Memo: TRichEdit;
    PopupMenu1: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N1: TMenuItem;
    SelectAll1: TMenuItem;
    Undo1: TMenuItem;
    N2: TMenuItem;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    ActionList1: TActionList;
    Undo: TAction;
    Cut: TAction;
    Copy: TAction;
    Paste: TAction;
    SelectAll: TAction;
    Print: TAction;
    New: TAction;
    SaveToFile: TAction;
    OpenFile: TAction;
    Escape: TAction;
    ToolButton3: TToolButton;
    ImageList1: TImageList;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton7: TToolButton;
    OpenDialog1: TJvOpenDialog;
    SaveDialog1: TJvSaveDialog;
    procedure Memo1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoSelectionChange(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure UndoExecute(Sender: TObject);
    procedure CutExecute(Sender: TObject);
    procedure CopyExecute(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
    procedure NewExecute(Sender: TObject);
    procedure SaveToFileExecute(Sender: TObject);
    procedure OpenFileExecute(Sender: TObject);
    procedure EscapeExecute(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

{***************************************************}

procedure TfoHintForm.Memo1Change(Sender: TObject);
var
  j: integer;
begin
  JvGroupBox1.Caption := '[ ' + IntToStr(Memo.Lines.count) + ' line';
  if Memo.Lines.count > 1 then
    JvGroupBox1.Caption := JvGroupBox1.Caption + 's';
  JvGroupBox1.Caption := JvGroupBox1.Caption + ', ';
  j := length(Memo.Text);
  JvGroupbox1.Caption := JvGroupBox1.Caption + IntToStr(j) + ' char';
  if j > 1 then
    JvGroupBox1.Caption := JvGroupBox1.Caption + 's';
  JvGroupBox1.Caption := JvGroupBox1.Caption + ' ]';
  MemoSelectionChange(Memo);
end;
{***************************************************}

procedure TfoHintForm.FormShow(Sender: TObject);
begin
  Memo1Change(Sender);
end;
{***************************************************}

procedure TfoHintForm.MemoSelectionChange(Sender: TObject);
begin
  Copy.Enabled := Memo.SelLength > 0;
  Cut.Enabled := Memo.SelLength > 0;
  Undo.Enabled := Memo.CanUndo;
end;
{***************************************************}

procedure TfoHintForm.Panel1Resize(Sender: TObject);
begin
  JvSpeedButton1.Left := (Panel1.Width - ((JvSpeedButton2.Left + JvSpeedButton2.Width) - JvSpeedButton1.Left)) div 2;
  JvSpeedButton2.Left := JvSpeedButton1.Left + 124;
end;
{***************************************************}

procedure TfoHintForm.SelectAllExecute(Sender: TObject);
begin
  Memo.SelectAll;
end;
{***************************************************}

procedure TfoHintForm.UndoExecute(Sender: TObject);
begin
  Memo.Undo;
end;
{***************************************************}

procedure TfoHintForm.CutExecute(Sender: TObject);
begin
  Memo.CutToClipboard;
end;
{***************************************************}

procedure TfoHintForm.CopyExecute(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;
{***************************************************}

procedure TfoHintForm.PasteExecute(Sender: TObject);
begin
  Memo.PasteFromClipboard;
end;
{***************************************************}

procedure TfoHintForm.PrintExecute(Sender: TObject);
begin
  Memo.Print(Caption);
end;
{***************************************************}

procedure TfoHintForm.NewExecute(Sender: TObject);
begin
  Memo.Clear;
  Memo1Change(Sender);
end;
{***************************************************}

procedure TfoHintForm.SaveToFileExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Memo.lines.SaveToFile(SaveDialog1.FileName);
end;
{***************************************************}

procedure TfoHintForm.OpenFileExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Memo.Lines.LoadFromFile(opendialog1.FileName);
end;
{***************************************************}

procedure TfoHintForm.EscapeExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{***************************************************}
end.
