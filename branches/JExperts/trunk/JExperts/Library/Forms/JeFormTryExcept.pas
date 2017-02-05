{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormTryExcept.PAS, released on 2001-02-28.

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

unit JeFormTryExcept;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, JvButton, ExtCtrls, ComCtrls, Buttons, JvSpeedButton,
  JvListView, JeExpert_Strings, JvExComCtrls, JvExControls;

type
  TfoTryExcept = class(TForm)
    Image1: TImage;
    PopupMenu1: TPopupMenu;
    OkButton: TJvSpeedButton;
    CancelButton: TJvSpeedButton;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    GroupBox1: TGroupBox;
    JvListView1: TJvListView;
    SingleLine1: TMenuItem;
    MultipleLineofCode1: TMenuItem;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SingleLine1Click(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  JeFormPopup;

{************************************************************}

procedure TfoTryExcept.RadioGroup1Click(Sender: TObject);
begin
  self.GroupBox1.Enabled := self.RadioGroup1.ItemIndex = 0;
  self.JvListView1.Enabled := self.RadioGroup1.ItemIndex = 0;
end;
{************************************************************}

procedure TfoTryExcept.FormCreate(Sender: TObject);
var
  Stream: TResourceStream;
  i: integer;
begin
  Stream := TResourceStream.Create(hInstance, 'Jv_EXCEPTIONS', RT_RCDATA);
  with TStringList.Create do
  begin
    LoadFromStream(Stream);
    Sort;
    for i := 0 to Count - 1 do
      with JvListView1.Items.Add do
      begin
        Caption := Strings[i];
        ImageIndex := 0;
        SubItems.Add('Single Line');
      end;
    Free;
  end;
  Stream.Free;
  Form.ToStatus(RCH_RightClickCode);
end;
{************************************************************}

procedure TfoTryExcept.SingleLine1Click(Sender: TObject);
const
  CS_LinesTxt: array[0..1] of string = ('Single Line', 'Multiple Lines');
var
  i: integer;
begin
  for i := 0 to JvListView1.Items.Count - 1 do
  begin
    if JvListView1.Items[i].Selected = false then
      Continue;
    JvListView1.Items[i].ImageIndex := (Sender as TMenuItem).Tag;
    JvListView1.Items[i].SubItems[0] := CS_LinesTxt[JvListView1.Items[i].ImageIndex];
  end;
end;
{************************************************************}

procedure TfoTryExcept.OkButtonClick(Sender: TObject);
var
  i: integer;
begin
  with TStringList.Create do
  begin
    Add('');

    Add('  try');
    Add('    //Place your code here ');
    if RadioGroup1.ItemIndex = 0 then
    begin
      Add('  except');
      for i := 0 to JvListView1.Items.Count - 1 do
        if JvListView1.Items[i].Checked then
        begin
          if JvListView1.Items[i].ImageIndex = 0 then
            Add('    on ' + JvListView1.Items[i].Caption + ' do ;')
          else
          begin
            Add('    on ' + JvListView1.Items[i].Caption + ' do');
            Add('      begin');
            Add('        //Place your code here');
            Add('      end;');
          end;
        end;
    end
    else
    begin
      Add('  finally');
      Add('    //This code is always executed !');
    end;
    Add('  end;');

    Add('');
    DelphiApis.EditorAddText(Text);
    Free;
  end;
end;
{************************************************************}
end.
