{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormFavorites.PAS, released on 2001-02-28.

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

unit JeFormFavorites;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, JvSpeedButton,
  JvDialogs, JvExStdCtrls, JvListBox, JvGroupBox, JvExControls;

type
  TfoFavorites = class(TForm)
    Image1: TImage;
    JvSpeedButton5: TJvSpeedButton;
    JvSpeedButton6: TJvSpeedButton;
    Panel1: TPanel;
    JvGroupBox1: TJvGroupBox;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    JvSpeedButton3: TJvSpeedButton;
    JvSpeedButton4: TJvSpeedButton;
    JvListbox1: TJvListbox;
    OpenDialog1: TJvOpenDialog;
    procedure JvSpeedButton4Click(Sender: TObject);
    procedure JvSpeedButton3Click(Sender: TObject);
    procedure JvSpeedButton2Click(Sender: TObject);
    procedure JvSpeedButton1Click(Sender: TObject);
    procedure JvSpeedButton5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

  TFavoriteData = class
    Name: string;
    Path: string;
  end;

implementation

uses
  JeFormPopup;

{$R *.DFM}

{************************************************************}

procedure TfoFavorites.JvSpeedButton4Click(Sender: TObject);
begin
  self.JvListBox1.MoveSelectedDown;
end;
{************************************************************}

procedure TfoFavorites.JvSpeedButton3Click(Sender: TObject);
begin
  self.JvListBox1.MoveSelectedUp;
end;
{************************************************************}

procedure TfoFavorites.JvSpeedButton2Click(Sender: TObject);
begin
  self.JvListBox1.DeleteSelected;
end;
{************************************************************}

procedure TfoFavorites.JvSpeedButton1Click(Sender: TObject);
var
  i: integer;
  FavoriteData: TFavoriteData;
  st: string;
begin
  if self.OpenDialog1.Execute then
  begin
    for i := 0 to self.OpenDialog1.Files.Count - 1 do
    begin
      FavoriteData := TFavoriteData.Create;
      FavoriteData.Path := self.OpenDialog1.Files[i];
      st := FavoriteData.Path;
      if not InputQuery('Specify Name', 'Enter a name for this entry', st) then
        exit;
      FavoriteData.Name := st;
      self.JvListBox1.Items.AddObject(st, FavoriteData);
    end;
  end;
end;
{************************************************************}

procedure TfoFavorites.JvSpeedButton5Click(Sender: TObject);
var
  i: integer;
begin
  Form.FFavorites.Clear;
  for i := 0 to JvListBox1.Items.Count - 1 do
    with TFavoriteData(JvListBox1.Items.Objects[i]) do
      Form.FFavorites.Add(Name + '|' + Path);
  Form.SaveFavorites;
  Form.LoadFavorites;
end;
{************************************************************}

procedure TfoFavorites.FormShow(Sender: TObject);
var
  i, j: integer;
  FavoriteData: TFavoriteData;
  st: string;
begin
  for i := 0 to Form.FFavorites.Count - 1 do
    with Form.FFavorites do
    begin
      FavoriteData := TFavoriteData.Create;
      st := Strings[i];
      if pos('|', st) <> 0 then
      begin
        j := LastDelimiter('|', st);
        FavoriteData.Name := Copy(st, 1, j - 1);
        FavoriteData.Path := Copy(st, j + 1, Length(st));
      end
      else
      begin
        Caption := st;
        Hint := st;
      end;
      self.JvListBox1.Items.AddObject(FavoriteData.Name, FavoriteData);
    end;
end;
{************************************************************}
end.
