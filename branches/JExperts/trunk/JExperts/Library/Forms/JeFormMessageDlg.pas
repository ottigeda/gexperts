{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormMessageDlg.PAS, released on 2001-02-28.

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

unit JeFormMessageDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, ImgList, StdCtrls, ExtCtrls, ComCtrls, Buttons, JvSpeedButton,
  JeExpert_Strings, JvExControls;

type
  TfoMessageDialog = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    RadioGroup1: TRadioGroup;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    RadioGroup2: TRadioGroup;
    StaticText3: TStaticText;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    StaticText4: TStaticText;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    RadioGroup3: TRadioGroup;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    GroupBox3: TGroupBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    Image1: TImage;
    Memo1: TMemo;
    NextButton: TJvSpeedButton;
    CancelButton: TJvSpeedButton;
    PrevButton: TJvSpeedButton;
    TabSheet7: TTabSheet;
    StaticText7: TStaticText;
    Panel1: TPanel;
    Preview: TImage;
    ImageList1: TImageList;
    JvSpeedButton1: TJvSpeedButton;
    RadioGroup4: TRadioGroup;
    CheckBox5: TCheckBox;
    SpinEdit1: TSpinEdit;
    procedure NextButtonClick(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{****************************************************}

procedure TfoMessageDialog.NextButtonClick(Sender: TObject);
const
  ADlgType: array[0..4] of string = ('mtConfirmation', 'mtCustom', 'mtError', 'mtInformation', 'mtWarning');
  Btns: array[0..10] of string = ('mbYes', 'mbNo', 'mbOK', 'mbCancel', 'mbAbort', 'mbRetry', 'mbIgnore',
    'mbAll', 'mbYesToAll', 'mbNoToAll', 'mbHelp');
  BtnsRes: array[0..9] of string = ('mrYes', 'mrNo', 'mrOK', 'mrCancel', 'mrAbort', 'mrRetry', 'mrIgnore',
    'mrAll', 'mrYesToAll', 'mrNoToAll');
var
  DlgType, Msg, Buttons: string;
  i, j: integer;
begin
  case self.PageControl1.ActivePage.PageIndex of
    4:
      begin
        if self.RadioGroup3.ItemIndex = 0 then
        begin
          self.NextButton.Caption := '&Generate';
          self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex + 2];
          exit;
        end;
      end;
    5:
      begin
        j := 0;
        for i := 0 to self.ComponentCount - 1 do
          if (self.Components[i] is TCheckBox) then
            with TCheckBox(self.Components[i]) do
              if (Parent = self.GroupBox3) and (Checked) then
                inc(j);
        if j <> 0 then
          self.NextButton.Caption := '&Generate'
        else
        begin
          Beep;
          Form.ToStatus(RCE_AtLeastOneBtn);
          Exit;
        end;
      end;
    6:
      begin
        DlgType := ADlgType[self.RadioGroup1.ItemIndex];
        Msg := StringReplace(self.Memo1.Lines.Text, '''', '''''', [rfReplaceAll]);
        Msg := StringReplace(Msg, RC_CRLF, '''+' + RC_CRLFStr + '+''', [rfReplaceAll]);
        Buttons := '';
        for i := 0 to self.ComponentCount - 1 do
          if (self.Components[i] is TCheckBox) then
            with TCheckBox(self.Components[i]) do
              if (Parent = self.GroupBox1) and (Checked) then
                Buttons := Buttons + Btns[Tag] + ',';
        if Buttons <> '' then
          Buttons := Copy(Buttons, 1, Length(Buttons) - 1);
        with TStringList.Create do
        begin
          Add('with CreateMessageDialog(''' + Msg + ''', ' + DlgType + ', [' + Buttons + ']) do');
          Add('  try');
          if self.SpinEdit1.Value <> 0 then
            Add('    HelpContext := ' + IntToStr(self.SpinEdit1.Value) + ';');
          if self.RadioGroup4.ItemIndex = 1 then
            Add('    Position := poScreenCenter;');
          case self.RadioGroup3.ItemIndex of
            0: Add('    ShowModal;');
            1:
              begin
                Buttons := '';
                for i := 0 to self.ComponentCount - 1 do
                  if (self.Components[i] is TCheckBox) then
                    with TCheckBox(self.Components[i]) do
                      if (Parent = self.GroupBox3) and (Checked) then
                        Buttons := Buttons + BtnsRes[Tag] + ',';
                if Buttons <> '' then
                  Buttons := Copy(Buttons, 1, Length(Buttons) - 1);
                if Pos(',', Buttons) = 0 then
                  Add('    if (ShowModal = ' + Buttons + ') then')
                else
                  Add('    if (ShowModal in [' + Buttons + ']) then');
                Add('    begin');
                Add('    ');
                Add('    end');
                Add('    else');
                Add('    begin');
                Add('    ');
                Add('    end;');
              end;
            2:
              begin
                Add('    case ShowModal of');
                for i := 0 to self.ComponentCount - 1 do
                  if (self.Components[i] is TCheckBox) then
                    with TCheckBox(self.Components[i]) do
                      if (Parent = self.GroupBox3) and (Checked) then
                        Add('      ' + BtnsRes[Tag] + ' : ;');
                Add('    end;');
              end;
          end;
          Add('  finally');
          Add('    Free;');
          Add('  end;');
          DelphiApis.EditorAddText(Text);
          Free;
        end;
        ModalResult := mrOk;
        exit;
      end;
  end;
  self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex + 1];
  self.PrevButton.Enabled := self.PageControl1.ActivePage.PageIndex <> 0;
end;
{****************************************************}

procedure TfoMessageDialog.PrevButtonClick(Sender: TObject);
begin
  self.NextButton.Caption := '&Next';
  case self.PageControl1.ActivePage.PageIndex of
    6:
      begin
        if self.RadioGroup3.ItemIndex = 0 then
        begin
          self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex - 2];
          exit;
        end;
      end;
  end;
  self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex - 1];
  self.PrevButton.Enabled := self.PageControl1.ActivePage.PageIndex <> 0;
end;
{****************************************************}

procedure TfoMessageDialog.RadioGroup1Click(Sender: TObject);
begin
  self.Preview.Picture.Assign(nil);
  if self.RadioGroup1.ItemIndex = 0 then
    self.ImageList1.GetBitmap(0, self.Preview.Picture.Bitmap)
  else if self.RadioGroup1.ItemIndex > 1 then
    self.ImageList1.GetBitmap(self.RadioGroup1.ItemIndex - 1, self.Preview.Picture.Bitmap);
end;
{****************************************************}

procedure TfoMessageDialog.FormCreate(Sender: TObject);
begin
  self.ImageList1.GetBitmap(0, self.Preview.Picture.Bitmap);
end;
{****************************************************}

procedure TfoMessageDialog.JvSpeedButton1Click(Sender: TObject);
const
  ADlgType: array[0..4] of TMsgDlgType = (mtConfirmation, mtCustom, mtError, mtInformation, mtWarning);
  Btns: array[0..10] of TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbYesToAll, mbNoToAll, mbHelp);
var
  Msg: string;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  i: integer;
begin
  Msg := self.Memo1.Lines.Text;
  DlgType := ADlgType[self.RadioGroup1.ItemIndex];
  Buttons := [];
  for i := 0 to self.ComponentCount - 1 do
    if (self.Components[i] is TCheckBox) then
      with TCheckBox(self.Components[i]) do
        if (Parent = self.GroupBox1) and (Checked) then
          Buttons := Buttons + [Btns[Tag]];
  MessageDlg(Msg, DlgType, Buttons, 0);
end;
{****************************************************}

procedure TfoMessageDialog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case ord(Key) of
    VK_ESCAPE: CancelButton.Click;
  end;
end;
{****************************************************}
end.
