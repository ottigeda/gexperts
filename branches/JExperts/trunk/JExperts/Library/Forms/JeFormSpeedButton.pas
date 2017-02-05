{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormSpeedButton.PAS, released on 2001-02-28.

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

unit JeFormSpeedButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, JvSpeedButton, Spin, 
  JeExpert_Types, JvComponent, JvComponentBase, JvSearchFiles, JvExControls;

type
  TfoSpeedButton = class(TForm)
    PageControl1: TPageControl;
    GroupBox1: TGroupBox;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PreviewButton: TJvSpeedButton;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    GroupBox3: TGroupBox;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Label3: TLabel;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox1: TComboBox;
    Label6: TLabel;
    SpinEdit4: TSpinEdit;
    CheckBox3: TCheckBox;
    ScrollBox1: TScrollBox;
    JvSearchFile1: TJvSearchFiles;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    SpeedButton1: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure JvSearchFile1Found(Sender: TObject; Path: string);
    procedure JvSearchFile1Start(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure GlyphClick(Sender: TObject);
  private
    LastBtn: TSpeedButton;
    MaxHeight: Integer;
    procedure ReplaceButton;
  public
    procedure LoadFromRecord(Value: TSpeedRecord);
    function SaveToRecord: TSpeedRecord;
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{*********************************************************************}

procedure TfoSpeedButton.FormShow(Sender: TObject);
var
  Path: string;
begin
  ReplaceButton;
  Path := DelphiApis.PackPath + 'Images\Buttons\';
// todo: enable
//  JvSearchFile1.Execute(Path);
end;
{*********************************************************************}

procedure TfoSpeedButton.JvSearchFile1Found(Sender: TObject; Path: string);
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

procedure TfoSpeedButton.JvSearchFile1Start(Sender: TObject);
begin
  LastBtn := SpeedButton1;
end;
{*********************************************************************}

procedure TfoSpeedButton.GlyphClick(Sender: TObject);
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

procedure TfoSpeedButton.CheckBox1Click(Sender: TObject);
begin
  PreviewButton.Flat := CheckBox1.Checked;
  PreviewButton.Enabled := CheckBox2.Checked;
  PreviewButton.Transparent := CheckBox3.Checked;
  PreviewButton.Caption := Edit1.Text;
  PreviewButton.GroupIndex := SpinEdit3.Value;
  PreviewButton.Margin := SpinEdit4.Value;
  PreviewButton.Spacing := SpinEdit1.Value;
  PreviewButton.NumGlyphs := SpinEdit2.Value;
  PreviewButton.Width := SpinEdit6.Value;
  PreviewButton.Height := SpinEdit5.Value;
  case ComboBox1.ItemIndex of
    0: PreviewButton.Layout := blGlyphLeft;
    1: PreviewButton.Layout := blGlyphRight;
    2: PreviewButton.Layout := blGlyphTop;
    3: PreviewButton.Layout := blGlyphBottom;
  end;
  ReplaceButton;
end;
{*********************************************************************}

procedure TfoSpeedButton.ReplaceButton;
begin
  PreviewButton.Left := (GroupBox1.ClientWidth - PreviewButton.Width) div 2;
  PreviewButton.Top := (GroupBox1.ClientHeight - PreviewButton.Height) div 2;
end;
{*********************************************************************}

procedure TfoSpeedButton.LoadFromRecord(Value: TSpeedRecord);
var
  Stream: TStream;
begin
  CheckBox1.Checked := Value.Flat;
  CheckBox2.Checked := Value.Enabled;
  CheckBox3.Checked := Value.Transparent;
  Edit1.Text := Value.Caption;
  SpinEdit3.Value := Value.GroupIndex;
  SpinEdit4.Value := Value.Margin;
  SpinEdit1.Value := Value.Spacing;
  SpinEdit2.Value := Value.NumGlyphs;
  SpinEdit6.Value := Value.Width;
  SpinEdit5.Value := Value.Height;
  case Value.Layout of
    blGlyphLeft: ComboBox1.ItemIndex := 0;
    blGlyphRight: ComboBox1.ItemIndex := 1;
    blGlyphTop: ComboBox1.ItemIndex := 2;
    blGlyphBottom: ComboBox1.ItemIndex := 3;
  end;
  if Value.Glyph = nil then
    PreviewButton.Glyph.Assign(nil)
  else
  begin
    Stream := TMemoryStream.Create;
    Value.Glyph.SaveToStream(Stream);
    Stream.Position := 0;
    PreviewButton.Glyph := TBitmap.Create;
    PreviewButton.Glyph.LoadFromStream(Stream);
    Stream.Free;
  end;
  CheckBox1Click(nil);
end;
{*********************************************************************}

function TfoSpeedButton.SaveToRecord: TSpeedRecord;
var
  Stream: TStream;
begin
  result.Flat := CheckBox1.Checked;
  result.Enabled := CheckBox2.Checked;
  result.Transparent := CheckBox3.Checked;
  result.Caption := Edit1.Text;
  result.GroupIndex := SpinEdit3.Value;
  result.Margin := SpinEdit4.Value;
  result.Spacing := SpinEdit1.Value;
  result.NumGlyphs := SpinEdit2.Value;
  result.Width := SpinEdit6.Value;
  result.Height := SpinEdit5.Value;
  case ComboBox1.ItemIndex of
    0: result.Layout := blGlyphLeft;
    1: result.Layout := blGlyphRight;
    2: result.Layout := blGlyphTop;
    3: result.Layout := blGlyphBottom;
  end;
  if PreviewButton.Glyph = nil then
    result.Glyph := nil
  else
  begin
    Stream := TMemoryStream.Create;
    PreviewButton.Glyph.SaveToStream(Stream);
    Stream.Position := 0;
    result.Glyph := TBitmap.Create;
    result.Glyph.LoadFromStream(Stream);
    Stream.Free;
  end;
end;
{*********************************************************************}
end.
