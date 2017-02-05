{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormDirectoryCleaner.PAS, released on 2001-02-28.

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

unit JeFormDirectoryCleaner;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons,
  JvSpeedButton, CheckLst, JvCheckListBox, JeExpert_Strings, 
  JvLogFile, ActnList, Menus, JvComponent, JvSearchFiles, JvComponentBase, JvExCheckLst, Mask,
  JvExMask, JvToolEdit, JvExControls;

type
  TfoDirCleaner = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    JvDirectoryBox1: TJvDirectoryEdit;
    GroupBox3: TGroupBox;
    JvSpeedButton3: TJvSpeedButton;
    JvSpeedButton4: TJvSpeedButton;
    JvCheckListBox1: TJvCheckListBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Animate1: TAnimate;
    StaticText1: TStaticText;
    ActionList1: TActionList;
    PopupMenu1: TPopupMenu;
    JvLogFile1: TJvLogFile;
    CheckAll: TAction;
    CheckNone: TAction;
    CheckAll1: TMenuItem;
    CheckNone1: TMenuItem;
    CheckBox1: TCheckBox;
    JvSearchFile1: TJvSearchFiles;
    procedure JvSpeedButton3Click(Sender: TObject);
    procedure JvSpeedButton4Click(Sender: TObject);
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure JvSearchFile1Found(Sender: TObject; Path: string);
    procedure JvSearchFile1ChangedDir(Sender: TObject; Directory: string);
    procedure JvSearchFile1Ended(Sender: TObject);
    procedure JvSearchFile1Start(Sender: TObject);
    procedure CheckAllExecute(Sender: TObject);
    procedure CheckNoneExecute(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  JeFormPopup;

{*********************************************************************}

procedure TfoDirCleaner.JvSpeedButton3Click(Sender: TObject);
var
  st: string;
begin
  st := '.xxx';
  if InputQuery('Extension', 'Enter an extension', st) then
    JvCheckListBox1.Items.Add(st);
end;
{*********************************************************************}

procedure TfoDirCleaner.JvSpeedButton4Click(Sender: TObject);
begin
  JvCheckListBox1.DeleteSelected;
end;
{*********************************************************************}

procedure TfoDirCleaner.JvSpeedButton1Click(Sender: TObject);
begin
  if JvDirectoryBox1.Directory = '' then
  begin
    Form.ToStatus(RCE_NoDirectory);
    JvDirectoryBox1.SetFocus;
    Exit;
  end;
  if JvCheckListBox1.GetChecked.Count = 0 then
  begin
    Form.ToStatus(RCE_AtLeastOneExt);
    JvCheckListBox1.SetFocus;
    Exit;
  end;

  (Sender as TJvSpeedButton).Enabled := false;
// todo: enable
//  JvSearchFile1.Recursive := CheckBox1.Checked;
//  JvSearchFile1.Execute(JvDirectoryBox1.Directory);
end;
{*********************************************************************}

procedure TfoDirCleaner.JvSearchFile1Found(Sender: TObject; Path: string);
var
  st: string;
  i, j: integer;
begin
  st := UpperCase(ExtractFileExt(Path));
  j := -1;
  for i := 0 to JvCheckListBox1.Items.Count - 1 do
    if UpperCase(JvCheckListBox1.Items[i]) = st then
      j := i;
  if j <> -1 then
  begin
    StaticText1.Caption := Path;
    try
      if DeleteFile(Path) then
        JvLogFile1.Add(DateTimeToStr(Now), 'DELETE', 'File ' + Path + ' deleted.')
      else
        JvLogFile1.Add(DateTimeToStr(Now), 'ERROR', 'File ' + Path + ' skipped.');
    except
      JvLogFile1.Add(DateTimeToStr(Now), 'ERROR', 'Failed for file ' + Path);
    end;
  end;
end;
{*********************************************************************}

procedure TfoDirCleaner.JvSearchFile1ChangedDir(Sender: TObject;
  Directory: string);
begin
  JvLogFile1.Add(DateTimeToStr(Now), 'DIRECTORY', Directory);
end;
{*********************************************************************}

procedure TfoDirCleaner.JvSearchFile1Ended(Sender: TObject);
begin
  GroupBox2.Visible := false;
  JvLogFile1.Add('END');
  JvLogFile1.ShowLog('Cleaner Log');
  ModalResult := mrOk;
end;
{*********************************************************************}

procedure TfoDirCleaner.JvSearchFile1Start(Sender: TObject);
begin
  JvLogFile1.Clear;
  GroupBox2.Visible := true;
  Animate1.Active := true;
  JvLogFile1.Add('BEGIN');
  JvLogFile1.Add(DateTimeToStr(Now), 'START DIR', JvDirectoryBox1.Directory);
end;
{*********************************************************************}

procedure TfoDirCleaner.CheckAllExecute(Sender: TObject);
begin
  JvCheckListBox1.CheckAll;
end;
{*********************************************************************}

procedure TfoDirCleaner.CheckNoneExecute(Sender: TObject);
begin
  JvCheckListBox1.UnCheckAll;
end;
{*********************************************************************}
end.
