{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormAbout.PAS, released on 2001-02-28.

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

unit JeFormAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, jpeg, JvScrollText, Buttons, JvSpeedButton, 
  JvLabel, JeExpert_Strings, ActnList, JvExControls;

type
  TfoAbout = class(TForm)
    Label1: TLabel;
    JvScrollText1: TJvScrollText;
    JvSpeedButton1: TJvSpeedButton;
    ActionList1: TActionList;
      Register: TAction;
    JvSpeedButton2: TJvSpeedButton;
    lblCopyRight: TLabel;
    Bevel2: TBevel;
    lblRights: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    JvHotLink1: TJvLabel;
    lblVisitJedi: TLabel;
    Image1: TImage;
    Image3: TImage;
    lblJvHotLink2: TJvLabel;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RegisterExecute(Sender: TObject);
  private
  public
  end;

implementation

uses
  JeFormPopup, JeFormRegister;

{$R *.DFM}

{************************************************************}

procedure TfoAbout.FormCreate(Sender: TObject);
begin
  {  if DllFunctions.Options.Regged then
      Caption := Caption+' - Registered to '+DllFunctions.Options.UserName
    else
      Caption := Caption+' - Private use only';
    StaticText1.Caption := StaticText1.Caption+' '+DelphiApis.PackVersion;
    JvMailTo1.MailDestinator := RC_SebEmail;
    Form.ToStatus(RC_ThanksUsing);}
end;
{************************************************************}

procedure TfoAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ord(Key) = VK_ESCAPE then
    self.ModalResult := mrCancel;
end;
{************************************************************}

procedure TfoAbout.RegisterExecute(Sender: TObject);
begin
  {  if DllFunctions.Options.Regged then
      Exit;
    with TfoRegister.Create(nil) do
    begin
      if ShowModal=mrOk then
        if DllFunctions.Options.IsSerialOk(Edit1.Text,Edit2.Text) then
        begin
          DllFunctions.Options.UserName := Edit1.Text;
          DllFunctions.Options.Serial := Edit2.Text;
          Form.SaveOptions;
          ShowMessage('Thanks for registering. Advertisments are now disabled');
        end;
      Free;
    end;}
end;
{************************************************************}
end.
