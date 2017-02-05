{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormAddLibrary.PAS, released on 2001-02-28.

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

unit JeFormAddLibrary;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, JvSpeedButton, JeExpert_Strings, FileCtrl, JvExControls;

type
  TfoAddCodeLibrary = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    ComboBox1: TComboBox;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    Label3: TLabel;
    Edit1: TEdit;
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
  public
  end;

implementation

uses
  JeFormPopup;

resourcestring
  CT_FilesLibrary = '.SRC';

{$R *.DFM}

  {*********************************************************************}

procedure TfoAddCodeLibrary.JvSpeedButton1Click(Sender: TObject);
var
  FileName: string;
begin
  if (Abs(ComboBox1.ItemIndex) = 1) then
  begin
    Form.ToStatus(RCE_MustSelectArea);
    ComboBox1.SetFocus;
    Exit;
  end;
  if Trim(Edit1.Text) = '' then
  begin
    Form.ToStatus(RCE_CodeNoName);
    Edit1.SetFocus;
    Exit;
  end;
  if Trim(Memo1.Text) = '' then
  begin
    Form.ToStatus(RCE_NoCodeInMemo);
    Memo1.SetFocus;
    Exit;
  end;

  FileName := DelphiApis.PackPath + 'Library\' + ComboBox1.items[ComboBox1.ItemIndex] + '\';
  ForceDirectories(FileName);
  FileName := FileNAme + Edit1.Text + CT_FilesLibrary;
  try
    Memo1.Lines.SaveToFile(FileName);
    ModalResult := mrOk;
  except
    Form.ToStatus(RCE_InvalidFileName);
    Edit1.SetFocus;
  end;
end;
{*********************************************************************}

procedure TfoAddCodeLibrary.FormShow(Sender: TObject);
var
  st: string;
begin
  if DelphiApis.EditorIsTextSelected then
  begin
    DelphiApis.EditorGetTextSelection(st);
    Memo1.Text := st;
  end
  else
    Form.ToStatus(RCH_NoSelection);
  ComboBox1.Items.Text := Form.GetLibraryAreas;
  ComboBox1.Items.Insert(0, '--------------------------------------------------------');
  ComboBox1.Items.Insert(0, 'New Area...');
end;
{*********************************************************************}

procedure TfoAddCodeLibrary.ComboBox1Change(Sender: TObject);
var
  st: string;
begin
  st := 'Area Name';
  if ComboBox1.ItemIndex = 0 then
    if InputQuery('New Area', 'Enter Area Name', st) then
    begin
      ComboBox1.Items[0] := st;
      ComboBox1.ItemIndex := 0;
    end;
end;
{*********************************************************************}
end.
