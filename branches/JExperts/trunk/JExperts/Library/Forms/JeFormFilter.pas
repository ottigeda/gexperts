{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormFilter.PAS, released on 2001-02-28.

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

unit JeFormFilter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, JvCombobox, JvSpeedButton, Registry, Buttons,
  JvDialogs, JvExStdCtrls, JvGroupBox, JvExControls;

type
  TfoFilter = class(TForm)
    JvGroupBox1: TJvGroupBox;
    stringGrid1: TStringGrid;
    JvCombobox1: TJvCombobox;
    NextButton: TJvSpeedButton;
    CancelButton: TJvSpeedButton;
    PrevButton: TJvSpeedButton;
    OpenDialog1: TJvOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure JvCombobox1Change(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
  private
  public
    procedure LoadFromStr(Value: string);
    function SaveToStr: string;
  end;

implementation

{$R *.DFM}

uses
  JeFormPopup;

{************************************************************}

procedure TfoFilter.LoadFromStr(Value: string);
var
  st: string;
  i, j, k: integer;
begin
  for i := 0 to StringGrid1.ColCount - 1 do
    for j := 0 to StringGrid1.RowCount - 1 do
      StringGrid1.Cells[i, j] := '';

  k := 0;
  i := pos('|', Value);
  while (i <> 0) do
  begin
    st := Copy(Value, 1, i - 1);
    Value := Copy(Value, i + 1, length(Value));
    j := pos('|', Value);
    self.stringGrid1.Cells[0, k] := st;
    if (j = 0) then
      st := Value
    else
    begin
      st := Copy(Value, 1, j - 1);
      Value := Copy(Value, j + 1, length(Value));
    end;
    self.stringGrid1.Cells[1, k] := st;
    inc(k);
    i := pos('|', Value);
  end;
end;
{************************************************************}

function TfoFilter.SaveToStr: string;
var
  i: integer;
  st1, st2: string;
begin
  result := '';
  for i := 0 to 49 do
  begin
    st1 := self.stringGrid1.Cells[0, i];
    st2 := self.stringGrid1.Cells[1, i];
    if (st1 <> '') and (st2 <> '') then
    begin
      if result <> '' then
        result := result + '|';
      result := result + st1 + '|' + st2
    end;
  end;
end;
{************************************************************}

procedure TfoFilter.FormShow(Sender: TObject);
begin
  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);
    if ValueExists('Filters') then
      JvComboBox1.Items.Text := ReadString('Filters');
    Free;
  end;
end;
{************************************************************}

procedure TfoFilter.JvCombobox1Change(Sender: TObject);
begin
  if (JvCombobox1.ItemIndex <> -1) then
    LoadFromStr(JvCombobox1.Items[JvCombobox1.ItemIndex]);
end;
{************************************************************}

procedure TfoFilter.NextButtonClick(Sender: TObject);
var
  st: string;
  i: integer;
begin
  st := SaveToStr;
  i := JvComboBox1.Items.IndexOf(st);
  if i <> -1 then
    JvComboBox1.Items.Delete(i);
  JvComboBox1.Items.Insert(0, st);
  if JvComboBox1.Items.Count > 10 then
    JvComboBox1.Items.Delete(10);

  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);
    WriteString('Filters', JvComboBox1.Items.Text);
    Free;
  end;
end;
{************************************************************}

procedure TfoFilter.PrevButtonClick(Sender: TObject);
begin
  self.OpenDialog1.Filter := SaveToStr;
  self.OpenDialog1.Execute;
end;
{************************************************************}
end.
