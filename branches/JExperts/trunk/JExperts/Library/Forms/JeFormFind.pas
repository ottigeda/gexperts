{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormFind.PAS, released on 2001-02-28.

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

unit JeFormFind;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvButton, ExtCtrls, ComCtrls, Buttons, JvSpeedButton,
  JeExpert_Strings, JvExControls;

type
  TfoFindFirst = class(TForm)
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    StaticText7: TStaticText;
    TabSheet2: TTabSheet;
    RadioGroup1: TRadioGroup;
    TabSheet3: TTabSheet;
    GroupBox4: TGroupBox;
    CheckBox8: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    StaticText1: TStaticText;
    GroupBox5: TGroupBox;
    Edit5: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    NextButton: TJvSpeedButton;
    CancelButton: TJvSpeedButton;
    PrevButton: TJvSpeedButton;
    TabSheet4: TTabSheet;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    procedure PrevButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
  private
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{*********************************************************************}

procedure TfoFindFirst.PrevButtonClick(Sender: TObject);
begin
  self.NextButton.Caption := '&Next';
  case self.PageControl1.ActivePage.PageIndex of
    3:
      begin
        if self.RadioGroup1.ItemIndex = 0 then
        begin
          self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex - 2];
          exit;
        end;
      end;
  end;
  self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex - 1];
  self.PrevButton.Enabled := self.PageControl1.ActivePage.PageIndex <> 0;
end;
{*********************************************************************}

procedure TfoFindFirst.NextButtonClick(Sender: TObject);
const
  AAttributes: array[1..7] of string = ('faReadOnly', 'faHidden', 'faSysFile', 'faAnyFile', 'faVolumeID',
    'faDirectory', 'faArchive');
var
  i: integer;
  st: string;
begin
  case self.PageControl1.ActivePage.PageIndex of
    1:
      begin
        self.NextButton.Caption := '&Generate';
        if self.RadioGroup1.ItemIndex = 1 then
        begin
          self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex + 2];
          exit;
        end;
      end;
    2:
      begin
        //Generate in a procedure
        if Edit1.Text = '' then
        begin
          Beep;
          Form.ToStatus(RCE_NoProcName);
          Edit1.SetFocus;
          Exit;
        end;
        with TStringList.Create do
        begin
          Add('');

          Add('procedure ' + Edit1.Text + '(Directory: string);');
          Add('var');
          Add(' SearchRec: TSearchRec;');
          Add(' Res: integer;');
          Add('begin');
          Add('  if (Directory<>'''') and (Directory[length(Directory)]<>''\'') then');
          Add('    Directory := Directory+''\'';');
          st := '  Res := FindFirst(Directory+''' + Edit5.Text + ''', ';
          for i := 0 to self.ComponentCount - 1 do
          begin
            if self.Components[i] is TCheckBox then
              with (self.Components[i] as TCheckBox) do
                if Checked and (Tag <> 0) then
                begin
                  st := st + AAttributes[Tag] + '+';
                  if Length(st) > 80 then
                  begin
                    st := Copy(st, 1, Length(st) - 1);
                    Add(st);
                    st := '           +';
                  end;
                end;
          end;
          st := Copy(st, 1, Length(st) - 1) + ', SearchRec);';
          Add(st);
          Add('  while (Res=0) do');
          Add('  begin');
          Add('    if (SearchRec.Name<>''.'') and (SearchRec.Name<>''..'') then');
          Add('    begin');
          Add('      if (SearchRec.Attr and faDirectory = 0) then');
          Add('      begin');
          Add('        //Place your code here !');
          Add('        //FileName is SearchRec.Name');
          Add('        //Full Path is Directory+SearchRec.Name');
          if CheckBox1.Checked then
          begin
            Add('      end');
            Add('      else ');
            Add('         ' + Edit1.Text + '(Directory+SearchRec.Name+''\'');');
          end
          else
            Add('      end;');
          Add('    end;');
          Add('    Res := FindNext(SearchRec);');
          Add('  end;');
          Add('  FindClose(SearchRec);');
          Add('end;');
          Add('');
          DelphiApis.EditorAddText(Text);
          Free;
        end;
        ModalResult := mrOk;
        Exit;
      end;
    3:
      begin
        //Generate in current code
        if (Edit2.Text = '') or (Edit3.Text = '') or (Edit4.Text = '') then
        begin
          Beep;
          Form.ToStatus(RCE_NoOptions);
          if Edit2.Text = '' then
            Edit2.SetFocus
          else if Edit3.Text = '' then
            Edit3.SetFocus
          else
            Edit4.SetFocus;
          Exit;
        end;
        with TStringList.Create do
        begin
          Add('');
          Add('  if (' + Edit2.Text + '<>'''') and (' + Edit2.Text + '[Length(' + Edit2.Text + ')]<>''\'') then');
          Add('    ' + Edit2.Text + ' := ' + Edit2.Text + '+''\'';');
          st := '  ' + Edit4.Text + ' := FindFirst(' + Edit2.Text + '+''' + Edit5.Text + ''', ';
          for i := 0 to self.ComponentCount - 1 do
          begin
            if self.Components[i] is TCheckBox then
              with (self.Components[i] as TCheckBox) do
                if (Checked) and (Tag <> 0) then
                begin
                  st := st + AAttributes[Tag] + '+';
                  if Length(st) > 80 then
                  begin
                    st := Copy(st, 1, Length(st) - 1);
                    Add(st);
                    st := '           +';
                  end;
                end;
          end;
          st := Copy(st, 1, Length(st) - 1) + ', ' + Edit3.Text + ');';
          Add(st);
          Add('  while ' + Edit4.Text + '=0 do');
          Add('  begin');
          Add('    if (' + Edit3.Text + '.Name<>''.'') and (' + Edit3.Text + '.Name<>''..'') then');
          Add('    begin');
          Add('        //Place your code here !');
          Add('        //FileName is ' + Edit3.Text + '.Name');
          Add('        //Full Path is ' + Edit2.Text + '+' + Edit3.Text + '.Name');
          Add('    end;');
          Add('    ' + Edit4.Text + ' := FindNext(' + Edit3.Text + ');');
          Add('  end;');
          Add('  FindClose(' + Edit3.Text + ');');
          Add('');
          DelphiApis.EditorAddText(Text);
          Free;
        end;
        ModalResult := mrOk;
        Exit;
      end;
  end;
  self.PageControl1.ActivePage := self.PageControl1.Pages[self.PageControl1.ActivePage.PageIndex + 1];
  self.PrevButton.Enabled := self.PageControl1.ActivePage.PageIndex <> 0;
end;
{*********************************************************************}
end.
