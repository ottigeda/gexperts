{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormTodo.PAS, released on 2001-02-28.

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

unit JeFormTodo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ComCtrls, JvListView, ExtCtrls,
  Grids, ImgList, JeExpert_Strings, ToolWin, ActnList,
  JvDialogs, JvExComCtrls;

type
  TfoToDo = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    Export1: TMenuItem;
    N1: TMenuItem;
    Quit1: TMenuItem;
    Items1: TMenuItem;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    JvListView2: TJvListView;
    Edit1: TMenuItem;
    JvListView1: TJvListView;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    N2: TMenuItem;
    ActionList1: TActionList;
    Save: TAction;
    Export: TAction;
    Quit: TAction;
    Add: TAction;
    Edit: TAction;
    Delete: TAction;
    ImageList2: TImageList;
    SaveDialog1: TJvSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Save1Click(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure JvListView1DblClick(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Items1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Export1Click(Sender: TObject);
  private
    function CurrentListView: TJvListView;
    procedure SaveAsHtml(Path: string);
    procedure SaveAsText(Path: string);
    procedure SaveAsCsv(Path: string);
  public
    PathProject, PathUnit: string;
    Modified: boolean;
  end;

var
  foToDo: TfoToDo = nil;

implementation

uses
  JeFormPopup, JeFormToDo_Add;

{$R *.DFM}

{************************************************************}

procedure TfoToDo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := caFree;
  foToDo := nil;
end;
{************************************************************}

procedure TfoToDo.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;
  if not Modified then
    exit;

  with CreateMessageDialog('Do you want to save changes ?', mtConfirmation, [mbYes, mbNo, mbCancel]) do
  try
    Position := poScreenCenter;
    case ShowModal of
      mrYes: Save1Click(nil);
      mrCancel: CanClose := False;
    end;
  finally
    Free;
  end;
end;
{************************************************************}

procedure TfoToDo.Save1Click(Sender: TObject);

  procedure SaveToDo(Path: string; ListView: TJvListView; ThreeLines: boolean);
  var
    i: integer;
  begin
// todo: enable
//    with JvXmlParser1 do
//    begin
//      Root.Clear;
//      Root.TagName := 'ToDoList';
//      for i := 0 to ListView.Items.Count - 1 do
//        with ListView.Items[i] do
//        begin
//          with Root.Add('ELEM') do
//          begin
//            Add('Status', IntToStr(StateIndex));
//            if ThreeLines then
//            begin
//              Add('Line', SubItems[0]);
//              Add('Text', SubItems[1]);
//            end
//            else
//              Add('Text', SubItems[0]);
//          end;
//        end;
//      if (Root.Count = 0) and (FileExists(Path)) then
//        DeleteFile(Path)
//      else
//        SaveToFile(Path);
//    end;
  end;

begin
  if self.TabSheet1.TabVisible then
    SaveToDo(PathProject, JvListView1, false);
  SaveToDo(PathUnit, JvListView2, true);
  Modified := false;
end;
{************************************************************}

procedure TfoToDo.Quit1Click(Sender: TObject);
begin
  self.Close;
end;
{************************************************************}

procedure TfoToDo.Add1Click(Sender: TObject);
var
  t: TListItem;
begin
  with TfoToDoAdd.Create(nil) do
  begin
    ComboBox1.ItemIndex := 0;
    if PageControl1.ActivePage = TabSheet1 then
    begin
      SpinEdit1.Value := -1;
      SpinEdit1.Enabled := false;
    end
    else
      SpinEdit1.Value := DelphiApis.EditorGetCurrentLine;

    if ShowModal = mrOk then
    begin
      t := CurrentListView.Items.Add;
      if self.PageControl1.ActivePage = self.TabSheet2 then
        t.SubItems.Add(IntToStr(SpinEdit1.Value));
      t.SubItems.Add(Edit1.Text);
      t.StateIndex := ComboBox1.ItemIndex;
      t.ImageIndex := -1;
      Modified := true;
    end;
    Free;
  end;
end;
{************************************************************}

procedure TfoToDo.JvListView1DblClick(Sender: TObject);
begin
  with TJvListView(Sender) do
  begin
    if Selected = nil then
      exit;
    with Selected do
      if SubItems.Count > 0 then
        DelphiApis.EditorSetLine(StrToIntDef(SubItems[0], 0));
  end;
end;
{************************************************************}

procedure TfoToDo.Delete1Click(Sender: TObject);
begin
  CurrentListView.DeleteSelected;
  Modified := true;
end;
{************************************************************}

procedure TfoToDo.FormShow(Sender: TObject);

  procedure LoadToDo(Path: string; ListView: TJvListView; HasLine: boolean);
  var
    i: integer;
    t: TListItem;
  begin
    ListView.Items.Clear;
    if not FileExists(Path) then
      exit;
// todo: enable
//    with JvXmlParser1 do
//    begin
//      LoadFromFile(Path);
//      if Root.TagName <> 'ToDoList' then
//        exit;
//      for i := 0 to Root.Count - 1 do
//      begin
//        t := ListView.Items.Add;
//        try
//          t.Caption := '';
//          t.StateIndex := StrToInt(Root[i].Values['Status']);
//          t.ImageIndex := -1;
//          if HasLine then
//            t.SubItems.Add(Root[i].Values['Line']);
//          t.SubItems.Add(Root[i].Values['Text']);
//        except
//          t.Free;
//        end;
//      end;
//    end;
  end;

begin
  DelphiApis.EditorGetCurrentProject(PathProject);
  DelphiApis.EditorGetCurrentUnit(PathUnit);
  self.TabSheet1.Caption := 'Project - ' + ChangeFileExt(ExtractFileName(PathProject), '');
  self.TabSheet2.Caption := 'Unit - ' + ChangeFileExt(ExtractFileName(PathUnit), '');
  PathProject := ChangeFileExt(PathProject, '.tdo');
  PathUnit := ChangeFileExt(PathUnit, '.tdo');

  if PathProject = PathUnit then
  begin
    self.TabSheet1.TabVisible := false;
    self.PageControl1.ActivePage := self.TabSheet2;
  end;
  Modified := false;

  LoadToDo(PathProject, JvListView1, false);
  LoadToDo(PathUnit, JvListView2, true);
end;
{************************************************************}

procedure TfoToDo.Edit1Click(Sender: TObject);
var
  t: TListItem;
begin
  if CurrentListView.Selected = nil then
    exit;

  with TfoToDoAdd.Create(nil) do
  begin
    ComboBox1.ItemIndex := 0;
    t := CurrentListView.Selected;
    ComboBox1.ItemIndex := t.StateIndex;
    if PageControl1.ActivePage = TabSheet1 then
    begin
      SpinEdit1.Value := -1;
      SpinEdit1.Enabled := false;
      Edit1.Text := t.SubItems[0];
    end
    else
    begin
      SpinEdit1.Value := StrToInt(t.SubItems[0]);
      Edit1.Text := t.SubItems[1];
    end;

    if ShowModal = mrOk then
    begin
      t.SubItems.Clear;
      if self.PageControl1.ActivePage = self.TabSheet2 then
        t.SubItems.Add(IntToStr(SpinEdit1.Value));
      t.StateIndex := ComboBox1.ItemIndex;
      t.SubItems.Add(Edit1.Text);
      Modified := true;
    end;
    Free;
  end;
end;
{************************************************************}

procedure TfoToDo.Items1Click(Sender: TObject);
begin
  Edit1.Enabled := CurrentListView.Selected <> nil;
end;
{************************************************************}

function TfoToDo.CurrentListView: TJvListView;
begin
  if self.PageControl1.ActivePage = self.TabSheet1 then
    result := JvListView1
  else
    result := JvListView2;
end;
{************************************************************}

procedure TfoToDo.FormCreate(Sender: TObject);
begin
  if foToDo <> nil then
  begin
    foToDo.Save1Click(foToDo.Save1);
    foToDo.Close;
  end;
  foToDo := self;
end;
{************************************************************}

procedure TfoToDo.Export1Click(Sender: TObject);
begin
  if not (self.SaveDialog1.Execute) then
    exit;
  case SaveDialog1.FilterIndex of
    1: SaveAsHtml(self.SaveDialog1.FileName);
    2: SaveAsText(self.SaveDialog1.FileName);
    3: SaveAsCsv(self.SaveDialog1.FileName);
  end;
end;
{************************************************************}

procedure TfoToDo.SaveAsHtml(Path: string);
var
  Stream: TResourceStream;
  FileName: string;
  st: string;
  i, j: integer;
begin
  Stream := TResourceStream.Create(hInstance, 'Jv_HTML_TODO', RT_RCDATA);
  with TStringList.Create do
  begin
    //Load the default informations
    LoadFromStream(Stream);

    //Generate the page
    j := 0;
    if self.PageControl1.ActivePage = self.TabSheet1 then
    begin
      FileName := PathProject;
      st := '  <TR><TD WIDTH=15% ALIGN=CENTER><B>Status</B></TD> <TD ALIGN=CENTER><B>Text</B></TD> </TR>' + RC_CRLF;
      with JvListView1 do
        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].StateIndex = 0 then
            st := st + '  <TR><TD BGCOLOR=RED ></TD>'
          else
          begin
            st := st + '  <TR><TD BGCOLOR=GREEN></TD>';
            inc(j);
          end;
// todo: enable
//          st := st + '<TD>' + StringToHtml(Items[i].SubItems[0]) + '</TD> </TR>' + RC_CRLF;
        end;
    end
    else
    begin
      FileName := PathUnit;
      st := '  <TR><TD WIDTH=15% ALIGN=CENTER><B>Status</B></TD> <TD WIDTH=15% ALIGN=CENTER><B>Line</B></TD> <TD><B>Text</B></TD> </TR>'
        + RC_CRLF;
      with JvListView2 do
        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].StateIndex = 0 then
            st := st + '  <TR><TD BGCOLOR=RED></TD>'
          else
          begin
            st := st + '  <TR><TD BGCOLOR=GREEN></TD>';
            inc(j);
          end;
// todo: enable
//          st := st + '<TD ALIGN=CENTER>' + StringToHtml(Items[i].SubItems[0]) + '</TD><TD>' +
//            StringToHtml(Items[i].SubItems[1]) + '</TD> </TR>' + RC_CRLF;
        end;
    end;
    Text := StringReplace(Text, '%DATAS%', st, [rfIgnoreCase, rfReplaceAll]);
    Text := StringReplace(Text, '%TITLE%', 'To Do - ' + FileName, [rfIgnoreCase, rfReplaceAll]);

    st := '   <B>File</B> : ' + FileName + RC_CRLF;
    st := st + '   <BR><B>Date</B> : ' + FormatDateTime('dddd d mmmm yyyy', Date) + RC_CRLF;
    st := st + '   <BR><B>Number of Items</B> : ' + IntToStr(CurrentListView.Items.Count) + RC_CRLF;
    st := st + '   <BR><B>Items finished</B> : ' + IntToStr(j) + RC_CRLF;
    Text := StringReplace(Text, '%INFORMATIONS%', st, [rfIgnoreCase, rfReplaceAll]);

    //Save it
    SaveToFile(Path);
    Free;
  end;
  Stream.Free;
end;
{************************************************************}

procedure TfoToDo.SaveAsCsv(Path: string);
var
  FileName: string;
  st: string;
  i, j: integer;
begin
  with TStringList.Create do
  begin
    //Generate the page
    j := 0;
    if self.PageControl1.ActivePage = self.TabSheet1 then
    begin
      FileName := PathProject;
      Add('Status;Text');
      with JvListView1 do
        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].StateIndex = 0 then
            st := 'To Do'
          else
          begin
            st := 'Finished';
            inc(j);
          end;
          Add(st + ';"' + StringReplace(Items[i].SubItems[0], '"', '""', [rfReplaceAll]) + '"');
        end;
    end
    else
    begin
      FileName := PathUnit;
      Add('Status;Line;Text');
      with JvListView2 do
        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].StateIndex = 0 then
            st := 'To Do'
          else
          begin
            st := 'Finished';
            inc(j);
          end;
          Add(st + ';' + Items[i].SubItems[0] + ';"' + StringReplace(Items[i].SubItems[1], '"', '""', [rfReplaceAll]) +
            '"');
        end;
    end;

    Insert(0, '');
    Insert(0, 'Items finished  : ' + IntToStr(j));
    Insert(0, 'Number of Items : ' + IntToStr(CurrentListView.Items.Count));
    Insert(0, 'Date : ' + FormatDateTime('dddd d mmmm yyyy', Date));
    Insert(0, 'File : ' + FileName);

    //Save it
    SaveToFile(Path);
    Free;
  end;
end;
{************************************************************}

procedure TfoToDo.SaveAsText(Path: string);
var
  Stream: TResourceStream;
  FileName: string;
  st: string;
  i, j: integer;
begin
  Stream := TResourceStream.Create(hInstance, 'Jv_TXT_TODO', RT_RCDATA);
  with TStringList.Create do
  begin
    //Load the default informations
    LoadFromStream(Stream);

    //Generate the page
    j := 0;
    if self.PageControl1.ActivePage = self.TabSheet1 then
    begin
      FileName := PathProject;
      st := '  Status    Text' + RC_CRLF;
      with JvListView1 do
        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].StateIndex = 0 then
            st := st + '  To Do   '
          else
          begin
            st := st + ' Finished ';
            inc(j);
          end;
// todo: enable
//          st := st + StringToHtml(Items[i].SubItems[0]) + RC_CRLF;
        end;
    end
    else
    begin
      FileName := PathUnit;
      st := '  Status    Line  Text' + RC_CRLF;
      with JvListView2 do
        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].StateIndex = 0 then
            st := st + '  To Do   '
          else
          begin
            st := st + ' Finished ';
            inc(j);
          end;
          st := st + Format('%5s  ', [Items[i].SubItems[0]]) + Items[i].SubItems[1] + RC_CRLF;
        end;
    end;
    Text := StringReplace(Text, '%DATAS%', st, [rfIgnoreCase, rfReplaceAll]);

    st := '   File : ' + FileName + RC_CRLF;
    st := st + '   Date : ' + FormatDateTime('dddd d mmmm yyyy', Date) + RC_CRLF;
    st := st + '   Number of Items : ' + IntToStr(CurrentListView.Items.Count) + RC_CRLF;
    st := st + '   Items finished  : ' + IntToStr(j) + RC_CRLF;
    Text := StringReplace(Text, '%INFORMATIONS%', st, [rfIgnoreCase, rfReplaceAll]);

    //Save it
    SaveToFile(Path);
    Free;
  end;
  Stream.Free;
end;
{************************************************************}
end.
