{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormReport.PAS, released on 2001-02-28.

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

unit JeFormReport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Buttons, JvSpeedButton, ExtCtrls, StdCtrls,
  JeExpert_Strings, JvComponent, JvLabel, JvExControls;

type
  TfoBugReport = class(TForm)
    JvSurfTo1: TJvLabel;
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    StaticText1: TStaticText;
    TabSheet2: TTabSheet;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    JvSpeedButton3: TJvSpeedButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure JvSpeedButton3Click(Sender: TObject);
  private
    procedure SendMail(Sender: TObject);
  public
  end;

implementation

{$R *.DFM}

uses
  JeFormPopup;

{*********************************************************************}

procedure TfoBugReport.JvSpeedButton1Click(Sender: TObject);
begin
  self.PageControl1.ActivePage := self.TabSheet2;
  with (Sender as TJvSpeedButton) do
  begin
    Caption := '&Send';
    OnClick := SendMail;
  end;
  self.JvSpeedButton3.Enabled := true;
end;
{*********************************************************************}

procedure TfoBugReport.SendMail(Sender: TObject);
begin
  Form.SendMail(RC_SebEmail, RC_SubjectEmail, 'Expert Name : ' + self.Edit1.Text + RC_CRLF + self.Memo1.Lines.Text);
  Form.ToStatus(RCH_ThanksEmail);
  self.Close;
end;
{*********************************************************************}

procedure TfoBugReport.JvSpeedButton3Click(Sender: TObject);
begin
  self.JvSpeedButton3.Enabled := false;
  self.JvSpeedButton1.Caption := '&Next';
  self.JvSpeedButton1.OnClick := JvSpeedButton1Click;
  self.PageControl1.ActivePage := self.TabSheet1;
end;
{*********************************************************************}
end.
