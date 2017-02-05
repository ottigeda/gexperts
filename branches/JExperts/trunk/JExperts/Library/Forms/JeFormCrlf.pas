{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormCrlf.PAS, released on 2001-02-28.

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

unit JeFormCrlf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, JvSpeedButton,
  JeExpert_Strings, Mask, JvExMask, JvToolEdit, JvExControls;

type
  TfoCrlf = class(TForm)
    Image1: TImage;
    JvSpeedButton1: TJvSpeedButton;
    JvSpeedButton2: TJvSpeedButton;
    Panel1: TPanel;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    JvFileNameBox1: TJvFileNameEdit;
    procedure JvSpeedButton1Click(Sender: TObject);
  private
    procedure CorrectCRLF(FileName: string);
  public
  end;

implementation

{$R *.DFM}

uses
  JeFormPopup;

{************************************************************}

procedure TfoCrlf.CorrectCRLF(FileName: string);
var
  src: TFileStream;
  dest: TMemoryStream;
  buf: array[0..2048] of byte;
  buf2: array[0..4096] of byte;
  i, j, k: integer;
begin
  src := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  src.Position := 0;
  dest := TMemoryStream.Create;

  while src.position < src.Size do
  begin
    i := src.Read(buf, sizeof(buf));
    k := 0;
    for j := 0 to i - 1 do
    begin
      if buf[j] = 13 then
      begin
        buf2[k] := 13;
        inc(k);
        buf2[k] := 10;
        inc(k);
      end
      else if buf[j] <> 10 then
      begin
        buf2[k] := buf[j];
        inc(k);
      end;
    end;
    dest.Write(buf2, k);
    Application.ProcessMessages;
  end;
  src.free;

  src := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  dest.Position := 0;
  src.CopyFrom(dest, dest.size);
  src.free;
  dest.free;
end;
{************************************************************}

procedure TfoCrlf.JvSpeedButton1Click(Sender: TObject);
var
  i: integer;
begin
// todo: enable
//  if self.JvFileNameBox1.Files.Count = 0 then
//  begin
//    Beep;
//    Form.ToStatus(RCE_AtLeastOneFile);
//    JvFileNameBox1.SetFocus;
//    Exit;
//  end;
//
//  for i := 0 to self.JvFileNameBox1.Files.Count - 1 do
//    CorrectCRLF(self.JvFileNameBox1.Files[i]);
  ModalResult := mrOk;
end;
{************************************************************}
end.
