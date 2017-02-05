{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormASCII.PAS, released on 2001-02-28.

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

unit JeFormASCII;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, JvExControls, JvSpeedButton;

type
  TfoAscii = class(TForm)
    FontDialog1: TFontDialog;
    Image1: TImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Edit1: TEdit;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    JvSpeedButton3: TJvSpeedButton;
    procedure JvBitBtn1Click(Sender: TObject);
    procedure initall(fon: Tfont);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure JvSpeedButton1Click(Sender: TObject);
  private
  public
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{*****************************************************************************}

procedure TfoAscii.initall(fon: Tfont);
var
  i, j: integer;
begin
  self.Edit1.Font := fon;
  self.Edit1.Font.size := 14;
  j := 0;
  for i := 0 to self.ComponentCount - 1 do
    if self.Components[i] is TLabel then
    begin
      (self.Components[i] as TLabel).Caption := chr(j);
      (self.Components[i] as TLabel).Font := fon;
      (self.components[i] as TLabel).Hint := 'Character #' + IntToStr(j) + ' (click to add to EditBox)';
      inc(j);
    end;
end;
{*****************************************************************************}

procedure TfoAscii.JvBitBtn1Click(Sender: TObject);
begin
  if self.FontDialog1.Execute then
    initall(self.FontDialog1.Font);
end;
{*****************************************************************************}

procedure TfoAscii.FormCreate(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to 14 do
  begin
    if i = 14 then
      for j := 0 to 3 do
      begin
        with TLabel.Create(self) do
        begin
          Parent := self.tabsheet1;
          Left := 6 + i * 18;
          Top := 38 + j * 14;
          AutoSize := false;
          Width := 17;
          Height := 13;
          OnClick := Label1Click;
          ShowHint := true;
        end;
      end
    else
      for j := 0 to 17 do
      begin
        with TLabel.Create(self) do
        begin
          Parent := self.tabsheet1;
          Left := 6 + i * 18;
          Top := 38 + j * 14;
          AutoSize := false;
          Width := 17;
          Height := 13;
          OnClick := Label1Click;
          ShowHint := true;
        end;
      end;
  end;
  initall(self.FontDialog1.font);
end;
{*****************************************************************************}

procedure TfoAscii.Label1Click(Sender: TObject);
begin
  self.edit1.text := self.edit1.text + (Sender as tlabel).caption;
  self.edit1.SelLength := 0;
  self.edit1.SelStart := length(self.edit1.text) - 1;
end;
{*****************************************************************************}

procedure TfoAscii.JvSpeedButton1Click(Sender: TObject);
begin
  DelphiApis.EditorAddText(Edit1.Text);
end;
{*****************************************************************************}
end.
