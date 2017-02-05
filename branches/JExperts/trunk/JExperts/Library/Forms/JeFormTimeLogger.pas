{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormTimeLogger.PAS, released on 2001-02-28.

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

unit JeFormTimeLogger;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, JeExpert_Strings, JeExpert_Types,
  ToolWin, ImgList, ActnList, JvDialogs;

type
  TfoTimeLogger = class(TForm)
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Quit1: TMenuItem;
    Saveas1: TMenuItem;
    StatusBar1: TStatusBar;
    Panel3: TPanel;
    Panel2: TPanel;
    ListView1: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    Open: TAction;
      Export: TAction;
    Quit: TAction;
    OpenDialog1: TJvOpenDialog;
    SaveDialog1: TJvSaveDialog;
    procedure Quit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure OpenFileName(FileName: string);
    function GetDuration(Value: TDateTime): string;
    procedure UpdateTotal;
  public
    LastTotal: TDateTime;
    LastPartialTotal: TDateTime;
    LastFile: TFileName;
    LastStarted: TDateTime;
  end;

implementation

{$R *.DFM}

uses
  JeFormPopup;

{*********************************************************************}

procedure TfoTimeLogger.Quit1Click(Sender: TObject);
begin
  self.Close;
end;
{*********************************************************************}

procedure TfoTimeLogger.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Text := DllFunctions.FProjectsOpened.Text;
  if ComboBox1.Items.Count > 0 then
  begin
    ComboBox1.ItemIndex := 0;
    if Assigned(ComboBox1.OnChange) then
      ComboBox1.OnChange(ComboBox1);
  end;
end;
{*********************************************************************}

procedure TfoTimeLogger.Open1Click(Sender: TObject);
begin
  if self.OpenDialog1.Execute then
  begin
    OpenFileName(self.OpenDialog1.FileName);
    LastFile := StringReplace(self.OpenDialog1.FileName, '.bpl', DelphiApis.GetPrjExtension, [rfIgnoreCase]);
  end;
end;
{*********************************************************************}

procedure TfoTimeLogger.OpenFileName(FileName: string);
var
  rec: TLoggerRecord;
  Total: TDateTime;
begin
  Timer1.Enabled := false;
  ListView1.Items.BeginUpdate;
  ListView1.Items.Clear;
  FileName := ChangeFileExt(FileName, '.btl');
  Total := EncodeTime(0, 0, 0, 0);
  try
    if FileExists(FileName) then
      with TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite) do
      try
        while Position < Size do
        begin
          Read(rec, SizeOf(rec));
          with ListView1.Items.Add do
          begin
            Caption := DateTimeToStr(rec.TimeStarted);
            SubItems.Add(DateTimeToStr(rec.TimeFinished));
            SubItems.Add(GetDuration(rec.TimeFinished - rec.TimeStarted));

            Total := Total + (rec.TimeFinished - rec.TimeStarted);
          end;
        end;
      finally
        Free;
      end;

    LastTotal := Total;
    LastPartialTotal := Total;
    UpdateTotal;
  except
    ListView1.Items.Clear;
  end;
  ListView1.Items.EndUpdate;
end;
{*********************************************************************}

function TfoTimeLogger.GetDuration(Value: TDateTime): string;
var
  Days: integer;
begin
  if Value > 1 then
  begin
    Days := Round(Int(Value));
    result := IntToStr(Days) + ' day';
    if Days > 1 then
      result := result + 's '
    else
      result := result + ' ';
  end
  else
    result := '';

  result := result + FormatDateTime('h:nn:ss', Value);
end;
{*********************************************************************}

procedure TfoTimeLogger.ComboBox1Change(Sender: TObject);
var
  i: Integer;
begin
  OpenFileName(ComboBox1.Items[ComboBox1.ItemIndex]);
  LastFile := ComboBox1.Items[ComboBox1.ItemIndex];
  Timer1.Enabled := true;
  with ListView1.Items.Add do
  begin
    i := DllFunctions.FProjectsOpened.IndexOf(LastFile);
    LastStarted := 0;
    if i <> -1 then
    begin
      LastStarted := TDataDateTime(DllFunctions.FProjectsOpened.Objects[i]).Value;
      Caption := DateTimeToStr(LastStarted);
      SubItems.Add(DateTimeToStr(Now));
      SubItems.Add(GetDuration(Now - LastStarted));
      LastTotal := LastPartialTotal + (Now - LastStarted);
      UpdateTotal;
    end
    else
      Free;
  end;
end;
{*********************************************************************}

procedure TfoTimeLogger.UpdateTotal;
begin
  StatusBar1.Panels[0].Text := IntToStr(ListView1.Items.Count) + ' Edition(s)';
  StatusBar1.Panels[1].Text := 'Total : ' + GetDuration(LastTotal);
  StatusBar1.Panels[2].Text := 'Average : ' + GetDuration(LastTotal / ListView1.Items.Count);
end;
{*********************************************************************}

procedure TfoTimeLogger.Saveas1Click(Sender: TObject);
var
  Stream: TResourceStream;

  procedure SaveToHtml(FileName: string);
  var
    i: integer;
    st: string;
  begin
    Stream := TResourceStream.Create(hInstance, 'Jv_HTML_LOGGER', RT_RCDATA);
    with TStringList.Create do
    begin
      LoadFromStream(Stream);
      Text := StringReplace(Text, '%PROJECT%', LastFile, [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%DATE%', FormatDateTime('dddd d mmmm yyyy', Date), [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%COUNT%', IntToStr(ListView1.Items.Count), [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%TOTAL%', GetDuration(LastTotal), [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%AVERAGE%', GetDuration(LastTotal / ListView1.Items.Count), [rfIgnoreCase,
        rfReplaceAll]);
      st := '';
      for i := 0 to ListView1.Items.Count - 1 do
        with ListView1.Items[i] do
          st := st + '<TR><TD>' + Caption + '</TD><TD>' + SubItems[0] + '</TD><TD>' + SubItems[1] + '</TD></TR>' +
            RC_CRLF;
      Text := StringReplace(Text, '%DATAS%', st, [rfIgnoreCase, rfReplaceAll]);

      SaveToFile(FileName);
      Free;
    end;
    Stream.Free;
  end;

  procedure SaveToCSV(FileName: string);
  var
    i: integer;
  begin
    with TStringList.Create do
    begin
      Add('Project : ' + LastFile);
      Add('Date : ' + FormatDateTime('dddd d mmmm yyyy', Date));
      Add('Number of editions : ' + IntToStr(ListView1.Items.Count));
      Add('Total time : ' + GetDuration(LastTotal));
      Add('Average : ' + GetDuration(LastTotal / ListView1.Items.Count));
      Add('');
      Add('Started at;Ended at;Duration');
      for i := 0 to ListView1.Items.Count - 1 do
        with ListView1.Items[i] do
          Add(Caption + ';' + SubItems[0] + ';' + SubItems[1]);
      SaveToFile(FileName);
      Free;
    end;
  end;

  procedure SaveToTxt(FileName: string);
  var
    i: integer;
    st: string;
  begin
    Stream := TResourceStream.Create(hInstance, 'Jv_TXT_LOGGER', RT_RCDATA);
    with TStringList.Create do
    begin
      LoadFromStream(Stream);
      Text := StringReplace(Text, '%PROJECT%', LastFile, [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%DATE%', FormatDateTime('dddd d mmmm yyyy', Date), [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%COUNT%', IntToStr(ListView1.Items.Count), [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%TOTAL%', GetDuration(LastTotal), [rfIgnoreCase, rfReplaceAll]);
      Text := StringReplace(Text, '%AVERAGE%', GetDuration(LastTotal / ListView1.Items.Count), [rfIgnoreCase,
        rfReplaceAll]);
      st := '';
      for i := 0 to ListView1.Items.Count - 1 do
        with ListView1.Items[i] do
          st := st + Format('  %10s   ', [Caption]) + Format('  %10s   ', [SubItems[0]]) + SubItems[1] + RC_CRLF;
      Text := StringReplace(Text, '%DATAS%', st, [rfIgnoreCase, rfReplaceAll]);

      SaveToFile(FileName);
      Free;
    end;
    Stream.Free;
  end;

begin
  if not (SaveDialog1.Execute) then
    exit;

  case SaveDialog1.FilterIndex of
    1: SaveToHtml(SaveDialog1.FileName);
    2: SaveToCSV(SaveDialog1.FileName);
    3: SaveToTxt(SaveDialog1.FileName);
  end;
end;
{*********************************************************************}

procedure TfoTimeLogger.Timer1Timer(Sender: TObject);
begin
  if (LastStarted = 0) or (ListView1.Items.Count = 0) then
    exit;
  with ListView1.Items[ListView1.Items.Count - 1] do
  begin
    SubItems[0] := DateTimeToStr(Now);
    SubItems[1] := GetDuration(Now - LastStarted);
    LastTotal := LastPartialTotal + (Now - LastStarted);
  end;

  UpdateTotal;
end;
{*********************************************************************}
end.
