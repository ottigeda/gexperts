{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeEditor_BitBtn.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit JeEditor_BitBtn;

interface

{$I JEDI.inc}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  stdctrls,
  ExtCtrls,
  Dialogs,
  EditIntf,
  ToolIntf,
  TypInfo,
  ExptIntf,
  JeExpert_Types,
  Buttons,
  DesignEditors,
  DesignIntf;

type
  TJeBitBtnEditor = class(TDefaultEditor)
  public
    function GetVerbCount : integer;override;
    function GetVerb(Index:integer) : string;override;
    procedure ExecuteVerb(Index:integer);override;
    procedure Edit;override;
  end;

implementation

uses
 JeExpert_Base;

{*************************************************}
procedure TJeBitBtnEditor.Edit;
begin
  ExecuteVerb(0);
end;
{*************************************************}
procedure TJeBitBtnEditor.ExecuteVerb(Index: integer);
var
 BitBtnRecord: TBitBtnRecord;
 Stream: TStream;
begin
  with Component as TBitBtn do
    case Index of
      0 : begin
            BitBtnRecord.Width := Width;
            BitBtnRecord.Height := Height;
            BitBtnRecord.Enabled := Enabled;
            BitBtnRecord.Caption := Caption;
            BitBtnRecord.Margin := Margin;
            BitBtnRecord.Spacing := Spacing;
            BitBtnRecord.NumGlyphs := NumGlyphs;
            BitBtnRecord.Layout := Layout;
            BitBtnRecord.Default := Default;
            BitBtnRecord.Cancel := Cancel;
            BitBtnRecord.Kind := Kind;
            BitBtnRecord.ModalResult := ModalResult;

            if Glyph=nil then
              BitBtnRecord.Glyph := nil
            else
            begin
              Stream := TMemoryStream.Create;
              Glyph.SaveToStream(Stream);
              Stream.Position := 0;
              BitBtnRecord.Glyph := TBitmap.Create;
              BitBtnRecord.Glyph.LoadFromStream(Stream);
              Stream.Free;
            end;

            BitBtnRecord := FDllFunctions.EditorBitBtn(BitBtnRecord);

            Width := BitBtnRecord.Width;
            Height := BitBtnRecord.Height;
            Enabled := BitBtnRecord.Enabled;
            Caption := BitBtnRecord.Caption;
            Margin := BitBtnRecord.Margin;
            Spacing := BitBtnRecord.Spacing;
            NumGlyphs := BitBtnRecord.NumGlyphs;
            Layout := BitBtnRecord.Layout;
            Default := BitBtnRecord.Default;
            Kind := BitBtnRecord.Kind;
            ModalResult := BitBtnRecord.ModalResult;
            Cancel := BitBtnRecord.Cancel;

            if BitBtnRecord.Glyph=nil then
              Glyph.Assign(nil)
            else
            begin
              Stream := TMemoryStream.Create;
              BitBtnRecord.Glyph.SaveToStream(Stream);
              if Glyph=nil then
                Glyph := TBitmap.Create;
              Stream.Position := 0;
              Glyph.LoadFromStream(Stream);
              Stream.Free;
            end;
          end;
      2:  Kind := bkOk;
      3:  Kind := bkCancel;
      4:  Kind := bkYes;
      5:  Kind := bkNo;
      6:  Kind := bkHelp;
      7:  Kind := bkClose;
      8:  Kind := bkAbort;
      9:  Kind := bkRetry;
      10:  Kind := bkIgnore;
      11:  Kind := bkAll;
    end;
  Designer.Modified;
end;
{*************************************************}
function TJeBitBtnEditor.GetVerb(Index: integer): string;
begin
  case Index of
     0: result := 'Edit...';
     1: result := '-';
     2: result := 'Ok';
     3: result := 'Cancel';
     4: result := 'Yes';
     5: result := 'No';
     6: result := 'Help';
     7: result := 'Close';
     8: result := 'Abort';
     9: result := 'Retry';
     10: result := 'Ignore';
     11: result := 'All';
  end;
end;
{*************************************************}
function TJeBitBtnEditor.GetVerbCount: integer;
begin
  result := 12;
end;
{*************************************************}
end.
