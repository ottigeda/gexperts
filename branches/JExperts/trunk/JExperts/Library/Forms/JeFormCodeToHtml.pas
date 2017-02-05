{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormCodeToHtml.PAS, released on 2001-02-28.

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

unit JeFormCodeToHtml;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, JvSpeedButton, JvExControls;

type
  TfoCodeToHtml = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

implementation

uses
  JeClass_CodeToHtml, JeFormPopup;

{$R *.DFM}

{*********************************************************************}

procedure TfoCodeToHtml.JvSpeedButton1Click(Sender: TObject);
var
  st: string;
begin
  JvSpeedButton1.Enabled := false;
  JvSpeedButton2.Enabled := false;

  if self.SaveDialog1.Execute then
    with TJeCodeToHtml.Create do
      with TStringList.Create do
      begin
        if self.RadioGroup1.ItemIndex = 0 then
          DelphiApis.EditorGetWholeText(st)
        else
          DelphiApis.EditorGetTextSelection(st);
        ColorOptions := DelphiApis.EditorGetColorScheme;
        Text := CodeToHtml(st);
        SaveToFile(self.SaveDialog1.FileName);
        Free;
      end;
end;
{*********************************************************************}

procedure TfoCodeToHtml.FormShow(Sender: TObject);
begin
  if DelphiApis.EditorIsTextSelected then
    self.RadioGroup1.ItemIndex := 1;
end;
{*********************************************************************}
end.
