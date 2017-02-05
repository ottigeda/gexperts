{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormBatch.PAS, released on 2001-02-28.

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

unit JeFormBatch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, JvSpeedButton, StdCtrls, ComCtrls, ExtCtrls,
  JeExpert_Strings, JvComponent, JvComponentBase, JvSearchFiles, Mask,
  JvExMask, JvToolEdit, JvExControls;

type
  TfoBatch = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    JvDirectoryBox1: TJvDirectoryEdit;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Animate1: TAnimate;
    ListBox1: TListBox;
    StaticText1: TStaticText;
    CheckBox1: TCheckBox;
    JvSearchFile1: TJvSearchFiles;
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure JvSearchFile1Ended(Sender: TObject);
    procedure JvSearchFile1Found(Sender: TObject; Path: string);
  private
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{***************************************************}

procedure TfoBatch.JvSpeedButton1Click(Sender: TObject);
begin
  //Go!
  if self.JvDirectoryBox1.Directory = '' then
  begin
    Beep;
    Form.ToStatus(RCE_NoDirectory);
    JvDirectoryBox1.SetFocus;
    Exit;
  end;

  self.JvSpeedButton1.Enabled := false;
  self.JvSpeedButton2.Enabled := false;
  self.ListBox1.Clear;

  self.Animate1.Active := true;
  self.Label3.Caption := RC_StatSearchingFiles;
  Form.ToStatus(RC_StatSearchingFiles);
  self.StaticText1.Caption := '';
  self.GroupBox1.Enabled := false;
  self.GroupBox2.Visible := true;

// todo: enable
//  self.JvSearchFile1.Mask := '*' + DelphiApis.GetPrjExtension;
//  self.JvSearchFile1.Recursive := self.CheckBox1.Checked;
  Application.ProcessMessages;
//  self.JvSearchFile1.Execute(self.JvDirectoryBox1.Directory);
end;
{***************************************************}

procedure TfoBatch.JvSearchFile1Ended(Sender: TObject);
var
  i: integer;
begin
  Form.ToStatus(RC_StatCompiling);
  self.Label3.Caption := RC_StatCompiling;
  self.StaticText1.Caption := '';

  for i := 0 to self.ListBox1.Items.Count - 1 do
  begin
    self.ListBox1.TopIndex := i;
    self.ListBox1.ItemIndex := i;
    Application.ProcessMessages;
    DelphiApis.CompileProject(self.ListBox1.Items[i]);
  end;

  self.JvSpeedButton1.Enabled := true;
  self.JvSpeedButton2.Enabled := true;
  self.GroupBox1.Enabled := true;
  self.GroupBox2.Visible := false;
  ModalResult := mrOk;
end;
{***************************************************}

procedure TfoBatch.JvSearchFile1Found(Sender: TObject; Path: string);
begin
  self.StaticText1.Caption := 'Found ' + ExtractFileName(Path);
  self.ListBox1.TopIndex := self.ListBox1.Items.Add(Path);
  Application.ProcessMessages;
end;
{***************************************************}
end.
