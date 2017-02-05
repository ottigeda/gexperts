{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeClass_Options.PAS, released on 2001-02-28.

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

{$H-}
unit JeClass_Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JeExpert_Types;

type
  TRegExpertOptions = record
    Version: Double;
    UserName, Serial: string;
    ToolBarCodeGenerator, ToolBarSnippets: boolean;
    AutomaticSave: boolean;
    AutomaticTime: cardinal;
    AutomaticConfirm: boolean;
    AutomaticAction: string;
    Disposition: string;
    DispositionPreserve: boolean;
    ToolBar1Left, ToolBar1Top, ToolBar2Left, ToolBar2Top: integer;
    TipsAtStartup: boolean;
    SayAgent: boolean;
    PaletteMultiline, PaletteHotTrack, PaletteAutoScroll, PaletteAutoSelect: boolean;
    PaletteFont: string;
    PaletteStyle, PalettePosition: Integer;
    HintPanelVisible: Boolean;
    PlaceJxMainMenuInToolsMenu:Boolean;
  end;

  TExpertOptions = class(TPersistent)
  private
    FOptions: TRegExpertOptions;
    function GetRegged: boolean;
  protected
  public
    function IsSerialOk(UserName, Serial: string): boolean;
    procedure SetOptions(Stream: TStream);
    procedure GetOptions(var Stream: TStream);
    procedure CompleteOldVersion;
  published
    property Version: double read FOptions.Version;

    property UserName: string read FOptions.UserName write FOptions.UserName;
    property Serial: string read FOptions.Serial write FOptions.Serial;
    property Regged: boolean read GetRegged;
    property ToolBarCodeGenerator: boolean read FOptions.ToolBarCodeGenerator write FOptions.ToolBarCodeGenerator;
    property ToolBarSnippets: boolean read FOptions.ToolBarSnippets write FOptions.ToolBarSnippets;
    property HintPanelVisible: Boolean read FOptions.HintPanelVisible write FOptions.HintPanelVisible;
    property PlaceJxMainMenuInToolsMenu: Boolean read FOptions.PlaceJxMainMenuInToolsMenu write FOptions.PlaceJxMainMenuInToolsMenu;

    property AutomaticSave: boolean read FOptions.AutomaticSave write FOptions.AutomaticSave;
    property AutomaticTime: cardinal read FOptions.AutomaticTime write FOptions.AutomaticTime;
    property AutomaticConfirm: boolean read FOptions.AutomaticConfirm write FOptions.AutomaticConfirm;
    property AutomaticAction: string read FOptions.AutomaticAction write FOptions.AutomaticAction;

    property Disposition: string read FOptions.Disposition write FOptions.Disposition;
    property DispositionPreserve: boolean read FOptions.DispositionPreserve write FOptions.DispositionPreserve;

    property ToolBar1Left: integer read FOptions.ToolBar1Left write FOptions.ToolBar1Left;
    property ToolBar1Top: integer read FOptions.ToolBar1Top write FOptions.ToolBar1Top;
    property ToolBar2Left: integer read FOptions.ToolBar2Left write FOptions.ToolBar2LEft;
    property ToolBar2Top: integer read FOptions.ToolBar2Top write FOptions.ToolBar2Top;

    property TipsAtStartup: boolean read FOptions.TipsAtStartup write FOptions.TipsAtStartup;
    property SayWithAgent: boolean read FOptions.SayAgent write FOptions.SayAgent;

    property PaletteMultiline: boolean read FOptions.PaletteMultiline write FOptions.PaletteMultiline;
    property PaletteHotTrack: boolean read FOptions.PaletteHotTrack write FOptions.PaletteHotTrack;
    property PaletteAutoScroll: boolean read FOptions.PaletteAutoScroll write FOptions.PaletteAutoScroll;
    property PaletteAutoSelect: boolean read FOptions.PaletteAutoSelect write FOptions.PaletteAutoSelect;
    property PaletteFont: string read FOptions.PaletteFont write FOptions.PaletteFont;
    property PaletteStyle: integer read FOptions.PaletteStyle write FOptions.PaletteStyle;
    property PalettePosition: integer read FOptions.PalettePosition write FOptions.PalettePosition;
  end;

implementation

{*********************************************************************}

procedure TExpertOptions.CompleteOldVersion;
begin
  if Version = 0 then
  begin
    //Complete all properties
    UserName := '';
    Serial := '';

    ToolBarCodeGenerator := true;
    ToolBarSnippets := true;
    HintPanelVisible := true;
    PlaceJxMainMenuInToolsMenu:=False;

    AutomaticSave := false;
    AutomaticTime := 5;
    AutomaticConfirm := false;
    AutomaticAction := '';

    Disposition := '';
    DispositionPreserve := true;

    ToolBar1Left := 11;
    ToolBar1Top := 80;
    ToolBar2Left := 11;
    ToolBar2Top := 106;

    TipsAtStartup := true;
    SayWithAgent := false;

    PaletteMultiline := true;
    PaletteHotTrack := true;
    PaletteFont := '';
    PaletteStyle := 0;
    PalettePosition := 0;
  end;
  FOptions.Version := ExpertVersion;
end;
{*********************************************************************}

procedure TExpertOptions.GetOptions(var Stream: TStream);
begin
  Stream.Write(FOptions, sizeof(FOptions));
end;
{*********************************************************************}

function TExpertOptions.GetRegged: boolean;
begin
  result := IsSerialOk(UserName, Serial);
end;
{*********************************************************************}

function TExpertOptions.IsSerialOk(UserName, Serial: string): boolean;

  function Generate(Value: string): string;
  var
    i, j: Integer;
  begin
    j := 0;
    for i := 1 to Length(Value) do
      j := j + ord(Value[i]);
    i := Length(Value);
    result := 'JExperts-' + IntToStr(i * 4) + '-' + IntToStr((j * 12) div 5) + '-' + IntToStr(i * (j - 1)) + '-' +
      (IntToStr(round(pi * j / (i + 1))));
  end;

begin
  result := false;
  if (Length(UserName) < 4) or (Length(Serial) = 0) then
    Exit;
  result := Serial = Generate(UserName);
end;
{*********************************************************************}

procedure TExpertOptions.SetOptions(Stream: TStream);
begin
  ZeroMemory(@FOptions, SizeOf(FOptions));
  Stream.Read(FOptions, SizeOf(FOptions));
  if Version < ExpertVersion then
    CompleteOldVersion;
end;
{*********************************************************************}
{$H+}
end.
