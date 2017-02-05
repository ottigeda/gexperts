{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormComponentSearch.PAS, released on 2001-02-28.

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

unit JeFormComponentSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvListView, JeExpert_Types, ExtCtrls, JeExpert_Strings, JvExComCtrls;

type
  TfoCompoSearch = class(TForm)
    JvListView1: TJvListView;
    Label1: TLabel;
    Edit1: TEdit;
    Timer1: TTimer;
    procedure JvListView1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    CompoList: TStringList;
  end;

implementation

{$R *.DFM}

uses
  JeFormPopup;

{*********************************************************************}

procedure TfoCompoSearch.JvListView1DblClick(Sender: TObject);
var
  Parent: TBaseCompo;
begin
  Parent.Internal := nil;
  if JvListView1.Selected <> nil then
  begin
    if DelphiApis.FormStartWorking then
      DelphiApis.CompoAdd(Parent, JvListView1.Selected.Caption, -1, -1, -1, -1);
    DelphiApis.FormEndWorking;
  end;
end;
{*********************************************************************}

procedure TfoCompoSearch.Timer1Timer(Sender: TObject);
var
  i: integer;
  st: string;
begin
  Timer1.Enabled := false;
  JvListView1.Items.BeginUpdate;
  JvListView1.Items.Clear;
  JvListView1.Cursor := crHourGlass;
  if Trim(Edit1.Text) = '' then
  begin
    for i := 0 to CompoList.Count - 1 do
      with JvListView1.Items.Add do
        Caption := CompoList[i];
  end
  else
  begin
    st := UpperCase(Edit1.Text);
    for i := 0 to CompoList.Count - 1 do
      if Pos(st, UpperCase(CompoList[i])) <> 0 then
        with JvListView1.Items.Add do
          Caption := CompoList[i];
  end;
  JvListView1.Cursor := crDefault;
  JvListView1.Items.EndUpdate;
end;
{*********************************************************************}

procedure TfoCompoSearch.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  //Restart timer
  Timer1.Enabled := false;
  Timer1.Enabled := true;
end;
{*********************************************************************}

procedure TfoCompoSearch.FormCreate(Sender: TObject);
var
  i: integer;
  st: string;
begin
  CompoList := TStringList.Create;
  CompoList.Sorted := true;

  with TStringList.Create do
  begin
    Text := DelphiApis.PaletteGetModules;
    for i := 0 to Count - 1 do
    begin
      st := DelphiApis.PaletteGetComponents(Strings[i]);
      CompoList.Text := CompoList.Text + st;
    end;
    Free;
  end;
  Timer1Timer(nil);
  Form.ToStatus(RCH_DoubleClickAdd);
end;
{*********************************************************************}

procedure TfoCompoSearch.FormDestroy(Sender: TObject);
begin
  CompoList.Free;
end;
{*********************************************************************}
end.
