{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_DllExported.PAS, released on 2001-02-28.

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

unit JeExpert_DllExported;

interface

uses
  Windows, Messages, Dialogs, Classes, Menus, ComCtrls, Graphics, Controls,
  Forms, JeExpert_Types, JeClass_Options, JeExpert_Strings, SysUtils;

type
  TDllExported = class(TDllFunctions)
  private
    function GetButton(ButtonTag: integer): TToolButton;
  public
    FOptions: TExpertOptions; //Must be public for the FormPopup
    FProjectsOpened: TStringList;
    constructor Create;
    destructor Destroy; override;

    //Internal functions
    procedure TryLoadingDefDisposition;

    //Exported functions
    function Initialize(Options: TStream): boolean; override;
    function Finalize(var Options: TStream): boolean; override;
    function Version: double; override;
    procedure ExecuteItem(ItemName: string); override;
    procedure ExecuteButton(ButtonTag: integer); override;
    procedure DropDown(ButtonTag: integer; Point: TPoint); override;
    procedure PopupMenu(ButtonTag: integer; Point: TPoint); override;
    procedure CompilerNotification(Event: TNotificationType; FileName: string); override;

    //Editors
    function EditorMultiline(Value: string): string; override;
    function EditorTime(Value: TTime): TTime; override;
    function EditorDate(Value: TDate): TDate; override;
    function EditorDateTime(Value: TDateTime): TDateTime; override;
    function EditorFilters(Value: string): string; override;
    function EditorCursor(Value: TCursor): TCursor; override;
    function EditorSpeedButton(Value: TSpeedRecord): TSpeedRecord; override;
    function EditorBitBtn(Value: TBitBtnRecord): TBitBtnRecord; override;
    property
      Options: TExpertOptions read FOptions write FOptions;
  end;

implementation

uses
  JeFormPopup, JeFormHint, JeFormTime, JeFormDate, JeFormCursorEditor, JeFormDateTime,
  JeFormFilter, JeFormSpeedButton, JeFormBitBtn
  ,ToolsApi;

{*********************************************************************}

procedure TDllExported.CompilerNotification(Event: TNotificationType;
  FileName: string);
var
  data: TDataDateTime;
  i: integer;
  LoggerRecord: TLoggerRecord;
begin
  case Event of
    ntProjectOpened:
      begin
        data := TDataDateTime.Create;
        data.Value := Now;
        FProjectsOpened.AddObject(FileName, TObject(data));
        TryLoadingDefDisposition;
      end;
    ntFileClosing:
      begin
        i := FProjectsOpened.IndexOf(FileName);
        if i <> -1 then
          CompilerNotification(ntProjectClosing, FileName);
      end;
    ntProjectClosing:
      begin
        i := FProjectsOpened.IndexOf(FileName);
        if i = -1 then
        begin
          if FProjectsOpened.Count > 0 then
            i := 0
          else
            exit;
        end;

        try
          FileName := ChangeFileExt(FProjectsOpened[i], '.btl');
          LoggerRecord.TimeStarted := TDataDateTime(FProjectsOpened.Objects[i]).Value;
          LoggerRecord.TimeFinished := Now;
          if FileExists(FileName) then
            with TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite) do
            begin
              Position := Size;
              Write(LoggerRecord, SizeOf(LoggerRecord));
              Free;
            end
          else
            with TFileStream.Create(FileName, fmCreate or fmShareDenyWrite) do
            begin
              Write(LoggerRecord, SizeOf(LoggerRecord));
              Free;
            end;
        except
        end;

        FProjectsOpened.Delete(i);
      end;
    ntProjectOpening, ntFileOpened, ntFileOpening, ntWindowOpened:
      TryLoadingDefDisposition;
  end;
end;
{*********************************************************************}

constructor TDllExported.Create;
begin
  FOptions := TExpertOptions.Create;
  Form := TfoPopup.Create(nil);
  FProjectsOpened := TStringList.Create;
end;
{*********************************************************************}

destructor TDllExported.Destroy;
begin
  FOptions.Free;
  Form.Free;
  FProjectsOpened.Free;
  inherited;
end;
{*********************************************************************}

procedure TDllExported.DropDown(ButtonTag: integer; Point: TPoint);
var
  Button: TToolButton;
begin
  Button := GetButton(ButtonTag);
  if Button <> nil then
    Button.DropdownMenu.Popup(Point.X, Point.Y);
end;
{*********************************************************************}

function TDllExported.EditorBitBtn(Value: TBitBtnRecord): TBitBtnRecord;
begin
  with TfoBitBtn.Create(nil) do
  begin
    LoadFromRecord(Value);
    Form.InitForm(TForm(JvSpeedButton1.Parent));
    if ShowModal = mrOk then
      result := SaveToRecord
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

function TDllExported.EditorCursor(Value: TCursor): TCursor;
begin
  with TfoCursorEditor.Create(nil) do
  begin
    ListBox1.ItemIndex := Abs(Value);
    ListBox1Click(ListBox1);
    Form.InitForm(TForm(GroupBox1.Parent));
    if ShowModal = mrOk then
      result := -ListBox1.ItemIndex
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

function TDllExported.EditorDate(Value: TDate): TDate;
begin
  with TfoDate.Create(nil) do
  begin
    MonthCalendar1.Date := Value;
    Form.InitForm(TForm(GroupBox1.Parent));
    if ShowModal = mrOk then
      result := MonthCalendar1.Date
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

function TDllExported.EditorDateTime(Value: TDateTime): TDateTime;
begin
  with TfoDateTime.Create(nil) do
  begin
    DateTimePicker1.Date := Int(Value);
    DateTimePicker2.Time := Value - Int(Value);
    Form.InitForm(TForm(DateTimePicker1.Parent));
    if ShowModal = mrOk then
      result := DateTimePicker1.Date + DateTimePicker2.Time
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

function TDllExported.EditorFilters(Value: string): string;
begin
  with TfoFilter.Create(nil) do
  begin
    LoadFromStr(Value);
    Form.InitForm(TForm(JvGroupBox1.Parent));
    if ShowModal = mrOk then
      result := SaveToStr
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

function TDllExported.EditorMultiline(Value: string): string;
begin
  with TfoHintForm.Create(nil) do
  begin
    Memo.Text := Value;
    Form.InitForm(TForm(Panel1));
    if ShowModal = mrOk then
      result := Memo.Text
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

function TDllExported.EditorSpeedButton(Value: TSpeedRecord): TSpeedRecord;
begin
  with TfoSpeedButton.Create(nil) do
  begin
    LoadFromRecord(Value);
    Form.InitForm(TForm(JvSpeedButton1.Parent));
    if ShowModal = mrOk then
      result := SaveToRecord
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

function TDllExported.EditorTime(Value: TTime): TTime;
begin
  with TfoTime.Create(nil) do
  begin
    DateTimePicker1.Time := Value;
    Form.InitForm(TForm(GroupBox1.Parent));
    if ShowModal = mrOk then
      result := DateTimePicker1.Time
    else
      result := Value;
    Free;
  end;
end;
{*********************************************************************}

procedure TDllExported.ExecuteButton(ButtonTag: integer);
var
  Button: TToolButton;
begin
  Button := GetButton(ButtonTag);
  if Button <> nil then
    Button.Click;
end;
{*********************************************************************}

procedure TDllExported.ExecuteItem(ItemName: string);
var
  found: boolean;

  procedure FindItem(Item: TMenuItem);
  var
    i: integer;
  begin
    i := 0;
    while (i < Item.Count) and not (found) do
      if Item[i].Name = ItemName then
      begin
        Item[i].Click;
        found := true;
        Exit;
      end
      else
      begin
        FindItem(Item[i]);
        inc(i);
      end;
  end;

begin
  found := false;
  FindItem(Form.ExpertPopup.Items);
end;
{*********************************************************************}

function TDllExported.Finalize(var Options: TStream): boolean;
begin
  FOptions.GetOptions(Options);
  DelphiApis.RemoveMenu(RC_MenuName);
  DelphiApis.RemoveToolBar(Form.ToolBarGenerators.Name);
  DelphiApis.RemoveToolBar(Form.ToolBarSnippets.Name);
  result := true;
end;
{*********************************************************************}

function TDllExported.GetButton(ButtonTag: integer): TToolButton;
var
  i: integer;
begin
  for i := 0 to Form.ToolBarGenerators.ButtonCount - 1 do
    if Form.ToolBarGenerators.Buttons[i].Tag = ButtonTag then
    begin
      result := Form.ToolBarGenerators.Buttons[i];
      Exit;
    end;
  for i := 0 to Form.ToolBarSnippets.ButtonCount - 1 do
    if Form.ToolBarSnippets.Buttons[i].Tag = ButtonTag then
    begin
      result := Form.ToolBarSnippets.Buttons[i];
      Exit;
    end;
  result := nil;
end;
{*********************************************************************}

function TDllExported.Initialize(Options: TStream): boolean;
var
  MainMenu: TMainMenu;
  Temp, ToolsMenuItem: TMenuItem;
  MenuItemName:string;

begin
  Form.Timer1.Enabled := true;
  Application.Handle := DelphiApis.ApplicationHandle;
  FOptions.SetOptions(Options);
//-------------------------------
{  if FOptions.PlaceJxMainMenuInToolsMenu then
  begin
  DelphiApis.AddMenu(RC_MenuName, 'ToolsToolsCommand', Form.ExpertPopup, Form.MenuPopup);
  end
  else}
//-------------------------------
  DelphiApis.AddMenu(RC_MenuName, 'HelpMenu', Form.ExpertPopup, Form.MenuPopup);

  Form.LoadSeparators;
  Form.LoadFavorites;

  result := true;
end;
{*********************************************************************}

procedure TDllExported.PopupMenu(ButtonTag: integer; Point: TPoint);
var
  Button: TToolButton;
begin
  Button := GetButton(ButtonTag);
  if Button <> nil then
    Button.PopupMenu.Popup(Point.X, Point.Y);
end;
{*********************************************************************}

procedure TDllExported.TryLoadingDefDisposition;
begin
  if Options.HintPanelVisible then
    DelphiApis.ShowHintBar;
  if Options.DispositionPreserve then
    Form.LoadLayout(Options.Disposition);
end;
{*********************************************************************}

function TDllExported.Version: double;
begin
  result := ExpertVersion;
end;
{*********************************************************************}
end.
