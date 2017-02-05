{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormOptions.PAS, released on 2001-02-28.

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

unit JeFormOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvSpeedButton, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, Spin, JeClass_Options,
  JvLabel, jpeg, JvExStdCtrls, JvCombobox, JvColorCombo, JvExControls;

type
  TfoOptions = class(TForm)
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    GroupBox4: TGroupBox;
    CheckBox3: TCheckBox;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    GroupBox5: TGroupBox;
    Label2: TLabel;
    ComboBox1: TComboBox;
    CheckBox4: TCheckBox;
    Label4: TLabel;
    ComboBox3: TComboBox;
    CheckBox5: TCheckBox;
    TabSheet4: TTabSheet;
    GroupBox6: TGroupBox;
    CheckBox7: TCheckBox;
    NextButton: TJvSpeedButton;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    JvFontBox1: TJvFontComboBox;
    CheckBox12: TCheckBox;
    CheckBox6: TCheckBox;
    Image1: TImage;
    Image3: TImage;
    lblJvHotLink2: TJvLabel;
    CheckBox13: TCheckBox;
    procedure CheckBox3Click(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
  private
  public
    procedure LoadOptions(Options: TExpertOptions);
    procedure SaveOptions(var Options: TExpertOptions);
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{*********************************************************************}

procedure TfoOptions.LoadOptions(Options: TExpertOptions);

  procedure SetValue(Value: string; Combo: TComboBox);
  var
    i: integer;
  begin
    i := Combo.Items.IndexOf(Value);
    if i = -1 then
      i := 0;
    Combo.ItemIndex := i;
  end;

begin
  CheckBox1.Checked := Options.ToolBarCodeGenerator;
  CheckBox2.Checked := Options.ToolBarSnippets;
  CheckBox12.Checked := Options.HintPanelVisible;
  CheckBox13.checked:= Options.PlaceJxMainMenuInToolsMenu;

  CheckBox3.Checked := Options.AutomaticSave;
  SpinEdit1.Value := Options.AutomaticTime;
  CheckBox5.Checked := Options.AutomaticConfirm;
  SetValue(Options.AutomaticAction, self.ComboBox3);

  SetValue(Options.Disposition, ComboBox1);
  CheckBox4.Checked := Options.DispositionPreserve;
  CheckBox6.Checked := Options.TipsAtStartup;
  CheckBox7.Checked := Options.SayWithAgent;

  CheckBox8.Checked := Options.PaletteMultiline;
  CheckBox9.Checked := Options.PaletteHotTrack;
  CheckBox10.Checked := Options.PaletteAutoSelect;
  CheckBox11.Checked := Options.PaletteAutoScroll;
  ComboBox5.ItemIndex := Options.PalettePosition;
  ComboBox4.ItemIndex := Options.PaletteStyle;
// todo: enable
//  JvFontBox1.Value.Name := Options.PaletteFont;

end;
{*********************************************************************}

procedure TfoOptions.SaveOptions(var Options: TExpertOptions);
begin
  Options.ToolBarCodeGenerator := CheckBox1.Checked;
  Options.ToolBarSnippets := CheckBox2.Checked;
  Options.HintPanelVisible := CheckBox12.Checked;
  Options.PlaceJxMainMenuInToolsMenu:=CheckBox13.checked ;

  Options.AutomaticSave := CheckBox3.Checked;
  Options.AutomaticTime := SpinEdit1.Value;
  Options.AutomaticConfirm := CheckBox5.Checked;
  Options.AutomaticAction := ComboBox3.Text;

  Options.Disposition := ComboBox1.Text;
  Options.DispositionPreserve := CheckBox4.Checked;

  Options.TipsAtStartup := CheckBox6.Checked;
  Options.SayWithAgent := CheckBox7.Checked;

  Options.PaletteMultiline := CheckBox8.Checked;
  Options.PaletteHotTrack := CheckBox9.Checked;
  Options.PaletteAutoSelect := CheckBox10.Checked;
  Options.PaletteAutoScroll := CheckBox11.Checked;
  Options.PalettePosition := ComboBox5.ItemIndex;
  Options.PaletteStyle := ComboBox4.ItemIndex;
// todo: enable
//  Options.PaletteFont := JvFontBox1.Value.Name;
end;
{*********************************************************************}

procedure TfoOptions.CheckBox3Click(Sender: TObject);
begin
  self.SpinEdit1.Enabled := self.CheckBox3.Checked;
  self.CheckBox5.Enabled := self.CheckBox3.Checked;
  self.ComboBox3.Enabled := self.CheckBox3.Checked;
end;
{*********************************************************************}

procedure TfoOptions.NextButtonClick(Sender: TObject);
begin
// todo: enable
//  try
//    with TAgent.Create(nil) do
//    begin
//      Connected := true;
//      ControlInterfacE.PropertySheet.Set_Visible(True);
//      Connected := false;
//    end;
//  except
//    Beep;
//  end;
end;
{*********************************************************************}
end.
