{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormBitBtn.PAS, released on 2001-02-28.

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

unit JeFormBitBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, Buttons, JvSpeedButton, JeExpert_Types,
  JvComponent, JvComponentBase, JvSearchFiles, JvExControls;

type
  TfoBitBtn = class(TForm)
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    SpinEdit4: TSpinEdit;
    CheckBox3: TCheckBox;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    ComboBox1: TComboBox;
    TabSheet2: TTabSheet;
    ScrollBox1: TScrollBox;
    GroupBox1: TGroupBox;
    JvSearchFile1: TJvSearchFiles;
    SpeedButton1: TSpeedButton;
    PreviewButton: TBitBtn;
    Label9: TLabel;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label10: TLabel;
    procedure FormShow(Sender: TObject);
    procedure JvSearchFile1Start(Sender: TObject);
    procedure JvSearchFile1Found(Sender: TObject; Path: string);
    procedure GlyphClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    LastBtn: TSpeedButton;
    MaxHeight: Integer;
    Loading: Boolean;
    procedure ReplaceButton;
  public
    procedure LoadFromRecord(Value: TBitBtnRecord);
    function SaveToRecord: TBitBtnRecord;
  end;

implementation

uses
  JeFormPopup;

const
  CS_Layout: array[0..3] of TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop,
    blGlyphBottom);
  CS_Kind: array[0..10] of TBitBtnKind = (bkCustom, bkOk, bkCancel, bkHelp, bkYes, bkNo,
    bkClose, bkAbort, bkRetry, bkIgnore, bkAll);
  CS_Modal: array[0..10] of TModalResult = (mrNone, mrOk, mrCancel, mrAbort,
    mrRetry, mrIgnore, mrYes, mrNo, mrAll, mrNoToAll, mrYesToAll);

{$R *.DFM}

  {*********************************************************************}

procedure TfoBitBtn.FormShow(Sender: TObject);
var
  Path: string;
begin
  ReplaceButton;
  Path := DelphiApis.PackPath + 'Images\Buttons\';
// todo: enable
//  JvSearchFile1.Execute(Path);
  MaxHeight := 0;
end;
{*********************************************************************}

procedure TfoBitBtn.JvSearchFile1Start(Sender: TObject);
begin
  LastBtn := SpeedButton1;
end;
{*********************************************************************}

procedure TfoBitBtn.JvSearchFile1Found(Sender: TObject; Path: string);
var
  Btn: TSpeedButton;
begin
  Btn := TSpeedButton.Create(ScrollBox1);
  with Btn do
  begin
    Parent := ScrollBox1;
    AutoSize := true;
    Flat := true;
    ShowHint := true;
    Hint := ChangeFileExt(ExtractFileName(Path), '');
    try
      Glyph.LoadFromFile(Path);
      if Glyph.Width > Glyph.Height then
        NumGlyphs := 2;
    except
      Free;
      Exit;
    end;
    if LastBtn.Left + LastBtn.Width + 2 * Width > ScrollBox1.Width then
    begin
      Left := 0;
      Top := LastBtn.Top + MaxHeight;
      MaxHeight := 0;
    end
    else
    begin
      Left := LastBtn.Left + LastBtn.Width;
      Top := LastBtn.Top;
    end;
    LastBtn := Btn;
    Btn.OnClick := GlyphClick;
    if Height > MaxHeight then
      MaxHeight := Height;
  end;
end;
{*********************************************************************}

procedure TfoBitBtn.GlyphClick(Sender: TObject);
begin
  with Sender as TSpeedButton do
  begin
    PreviewButton.Glyph.Assign(Glyph);
    PreviewButton.NumGlyphs := NumGlyphs;
    SpinEdit2.Value := NumGlyphs;
  end;
  ReplaceButton;
end;
{*********************************************************************}

procedure TfoBitBtn.ReplaceButton;
begin
  PreviewButton.Left := (GroupBox1.ClientWidth - PreviewButton.Width) div 2;
  PreviewButton.Top := (GroupBox1.ClientHeight - PreviewButton.Height) div 2;
end;
{*********************************************************************}

procedure TfoBitBtn.CheckBox1Click(Sender: TObject);
begin
  if Loading then
    Exit;
  with PreviewButton do
  begin
    Enabled := CheckBox1.Checked;
    Cancel := CheckBox2.Checked;
    Default := CheckBox3.Checked;
    Width := SpinEdit6.Value;
    Height := SpinEdit5.Value;
    Margin := SpinEdit4.Value;
    NumGlyphs := SpinEdit2.Value;
    Spacing := SpinEdit1.Value;
    Caption := Edit1.Text;
    Layout := CS_Layout[ComboBox1.ItemIndex];
    Kind := CS_Kind[ComboBox2.ItemIndex];
    ModalResult := CS_Modal[ComboBox3.ItemIndex];
  end;
end;
{*********************************************************************}

procedure TfoBitBtn.LoadFromRecord(Value: TBitBtnRecord);
var
  Stream: TStream;
  i: Integer;
begin
  Loading := true;
  with Value do
  begin
    CheckBox1.Checked := Enabled;
    CheckBox2.Checked := Cancel;
    CheckBox3.Checked := Default;
    Edit1.Text := Caption;
    SpinEdit6.Value := Width;
    SpinEdit5.Value := Height;
    SpinEdit4.Value := Margin;
    SpinEdit2.Value := NumGlyphs;
    SpinEdit1.Value := Spacing;
    for i := 0 to 3 do
      if CS_Layout[i] = Layout then
        ComboBox1.ItemIndex := i;
    for i := 0 to 10 do
      if CS_Kind[i] = Kind then
        ComboBox2.ItemIndex := i;
    for i := 0 to 10 do
      if CS_Modal[i] = ModalResult then
        ComboBox3.ItemIndex := i;
    if Glyph = nil then
      PreviewButton.Glyph.Assign(nil)
    else
    begin
      Stream := TMemoryStream.Create;
      Glyph.SaveToStream(Stream);
      Stream.Position := 0;
      PreviewButton.Glyph := TBitmap.Create;
      PreviewButton.Glyph.LoadFromStream(Stream);
      Stream.Free;
    end;
  end;
  Loading := false;
  CheckBox1Click(nil);
end;
{*********************************************************************}

function TfoBitBtn.SaveToRecord: TBitBtnRecord;
var
  Stream: TStream;
begin
  with result do
  begin
    Default := PreviewButton.Default;
    Cancel := PreviewButton.Cancel;
    Enabled := PreviewButton.Enabled;
    Width := PreviewButton.Width;
    Height := PreviewButton.Height;
    Kind := PreviewButton.Kind;
    ModalResult := PreviewButton.ModalResult;
    Margin := PreviewButton.Margin;
    NumGlyphs := PreviewButton.NumGlyphs;
    Layout := PreviewButton.Layout;
    Spacing := PreviewButton.Spacing;
    Caption := PreviewButton.Caption;
    if PreviewButton.Glyph = nil then
      Glyph := nil
    else
    begin
      Stream := TMemoryStream.Create;
      PreviewButton.Glyph.SaveToStream(Stream);
      Stream.Position := 0;
      Glyph := TBitmap.Create;
      Glyph.LoadFromStream(Stream);
      Stream.Free;
    end;
  end;
end;
{*********************************************************************}

procedure TfoBitBtn.FormCreate(Sender: TObject);
begin
  Loading := false;
end;
{*********************************************************************}
end.
