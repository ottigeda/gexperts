{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeEditor_SpeedButton.PAS, released on 2001-02-28.

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

unit JeEditor_SpeedButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,  stdctrls,
  extctrls, Dialogs, EditIntf, ToolIntf, TypInfo, ExptIntf, JeExpert_Types,
  Buttons,DesignEditors, DesignIntf;

type
  TJeSpeedButtonEditor = class(TDefaultEditor)
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
procedure TJeSpeedButtonEditor.Edit;
begin
  ExecuteVerb(0);
end;
{*************************************************}
procedure TJeSpeedButtonEditor.ExecuteVerb(Index: integer);
var
 SpeedRecord: TSpeedRecord;
 Stream: TStream;
begin
  with Component as TSpeedButton do
    case Index of
      0 : begin
            SpeedRecord.Width := Width;
            SpeedRecord.Height := Height;
            SpeedRecord.Flat := Flat;
            SpeedRecord.Enabled := Enabled;
            SpeedRecord.Transparent := Transparent;
            SpeedRecord.Caption := Caption;
            SpeedRecord.GroupIndex := GroupIndex;
            SpeedRecord.Margin := Margin;
            SpeedRecord.Spacing := Spacing;
            SpeedRecord.NumGlyphs := NumGlyphs;
            SpeedRecord.Layout := Layout;

            if Glyph=nil then
              SpeedRecord.Glyph := nil
            else
            begin
              Stream := TMemoryStream.Create;
              Glyph.SaveToStream(Stream);
              Stream.Position := 0;
              SpeedRecord.Glyph := TBitmap.Create;
              SpeedRecord.Glyph.LoadFromStream(Stream);
              Stream.Free;
            end;

            SpeedRecord := FDllFunctions.EditorSpeedButton(SpeedRecord);

            Width := SpeedRecord.Width;
            Height := SpeedRecord.Height;
            Flat := SpeedRecord.Flat;
            Enabled := SpeedRecord.Enabled;
            Transparent := SpeedRecord.Transparent;
            Caption := SpeedRecord.Caption;
            GroupIndex := SpeedRecord.GroupIndex;
            Margin := SpeedRecord.Margin;
            Spacing := SpeedRecord.Spacing;
            NumGlyphs := SpeedRecord.NumGlyphs;
            Layout := SpeedRecord.Layout;

            if SpeedRecord.Glyph=nil then
              Glyph.Assign(nil)
            else
            begin
              Stream := TMemoryStream.Create;
              SpeedRecord.Glyph.SaveToStream(Stream);
              if Glyph=nil then
                Glyph := TBitmap.Create;
              Stream.Position := 0;
              Glyph.LoadFromStream(Stream);
              Stream.Free;
            end;
          end;
      2: begin
           Width := 75;
           Height := 25;
         end;
      3: begin
           Width := 23;
           Height := 22;
         end;
      4: begin
           Width := 16;
           Height := 16;
         end;
      5: begin
           Width := 32;
           Height := 32;
         end;
    end;
  Designer.Modified;
end;
{*************************************************}
function TJeSpeedButtonEditor.GetVerb(Index: integer): string;
begin
  case Index of
     0: result := 'Edit...';
     1: result := '-';
     2: result := 'Set Size to 75*25';
     3: result := 'Set Size to 23*22';
     4: result := 'Set Size to 16*16';
     5: result := 'Set Size to 32*32';
  end;
end;
{*************************************************}
function TJeSpeedButtonEditor.GetVerbCount: integer;
begin
  result := 6;
end;
{*************************************************}
end.
