{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormSnippet.PAS, released on 2001-02-28.

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

unit JeFormSnippet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, StdCtrls, Buttons, JvPrint, JeExpert_Strings, ExtCtrls,
  JvComponent, JvDialogs, JvComponentBase;

type
  TfoSnippet = class(TForm)
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    JvPrint1: TJvPrint;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Panel1: TPanel;
    Memo1: TMemo;
    SaveDialog1: TJvSaveDialog;
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    LastFile: string;
  public
    procedure LoadFromFile(FileName: string);
  end;

var
  foSnippet: TfoSnippet = nil;

implementation

{$R *.DFM}

{*********************************************************************}

procedure TfoSnippet.SpeedButton7Click(Sender: TObject);
begin
  JvPrint1.Print(TStringList(Memo1.Lines));
end;
{*********************************************************************}

procedure TfoSnippet.SpeedButton4Click(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;
{*********************************************************************}

procedure TfoSnippet.SpeedButton1Click(Sender: TObject);
var
  Stream: TResourceStream;
  st: string;
begin
  if not (SaveDialog1.Execute) then
    exit;
  if SaveDialog1.FilterIndex = 2 then
    Memo1.Lines.SaveToFile(SaveDialog1.FileName)
  else
    with TStringList.Create do
    begin
      Stream := TResourceStream.Create(hInstance, 'Jv_HTML_SNIP', RT_RCDATA);
      LoadFromStream(Stream);
      Stream.Free;

      Text := StringReplace(Text, '%TITLE%', ChangeFileExt(ExtractFileName(LastFile), ''), [rfReplaceAll,
        rfIgnoreCase]);
      st := '   <B>File</B> : ' + ChangeFileExt(ExtractFileName(LastFile), '') + RC_CRLF;
      st := st + '   <BR><B>Date</B> : ' + FormatDateTime('dddd d mmmm yyyy', Date) + RC_CRLF;
      Text := StringReplace(Text, '%INFORMATIONS%', st, [rfReplaceAll, rfIgnoreCase]);
      Text := StringReplace(Text, '%DATAS%', Memo1.Lines.Text, [rfReplaceAll, rfIgnoreCase]);

      SaveToFile(SaveDialog1.FileName);
      Free;
    end;
end;
{*********************************************************************}

procedure TfoSnippet.LoadFromFile(FileName: string);
begin
  Memo1.Lines.LoadFromFile(FileName);
  LastFile := FileName;
  Caption := 'Snippet - ' + ChangeFileExt(ExtractFileName(LastFile), '');
end;
{*********************************************************************}

procedure TfoSnippet.FormCreate(Sender: TObject);
begin
  foSnippet := self;
end;
{*********************************************************************}

procedure TfoSnippet.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  foSnippet := nil;
end;
{*********************************************************************}
end.
