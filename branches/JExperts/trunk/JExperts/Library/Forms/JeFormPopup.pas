{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeFormPopup.PAS, released on 2001-02-28.

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

unit JeFormPopup;

interface

uses
  //General units
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ShellApi, StdCtrls, ExtCtrls, Activex, ToolWin, ComCtrls,
  ImgList, Registry, OleCtrls, Mapi, JvTipOfDay,
  JvExControls, JvBaseDlg, JvLabel,
  ActnList,jpeg,

  //Experts units
  JeUnit_Utils, JeExpert_Types, JeExpert_Strings, JeClass_Options, JeExpert_DllExported;

type
  TfoPopup = class(TForm)
    ExpertPopup: TPopupMenu;
    JvWebPages1: TMenuItem;
    JvOurPages1: TMenuItem;
    JvOfficialBorlandRelatedSites1: TMenuItem;
    JvDelphiComponents1: TMenuItem;
    DelphiTips1: TMenuItem;
    JvBuyPinSoftware1: TMenuItem;
    JvN3: TMenuItem;
    JvEsiStudentsFrench1: TMenuItem;
    JvWebCamViewer1: TMenuItem;
    JvBorlandInternational1: TMenuItem;
    JvN4: TMenuItem;
    JvBorlandDelphi1: TMenuItem;
    JvBorlandCommunity1: TMenuItem;
    JvDelphiSuperPage1: TMenuItem;
    JvTorry1: TMenuItem;
    JvDelphiPages1: TMenuItem;
    JvDelphi30001: TMenuItem;
    JvUndu1: TMenuItem;
    JvHelpMakers1: TMenuItem;
    JvN2: TMenuItem;
    JvAbout1: TMenuItem;
    ImageDlgs: TImage;
    JvOptions1: TMenuItem;
    JvUtils1: TMenuItem;
    JvCodeGenerators1: TMenuItem;
    JvCodeManipulators1: TMenuItem;
    JvAsciiTable1: TMenuItem;
    JvN1: TMenuItem;
    JvCodeManipulators2: TMenuItem;
    JvBatchCompiler1: TMenuItem;
    JvBugReport1: TMenuItem;
    JvImportCHeader1: TMenuItem;
    JvFormTools1: TMenuItem;
    JvMethodsLexical1: TMenuItem;
    JvRuntimeErrors1: TMenuItem;
    JvInsertGUID1: TMenuItem;
    JvExecuteProject1: TMenuItem;
    JvComments1: TMenuItem;
    JvToggleComment1: TMenuItem;
    JvUncommentSelection1: TMenuItem;
    JvCommentSelection1: TMenuItem;
    JvCorrectCarriageReturns1: TMenuItem;
    JvConvertToHtml2: TMenuItem;
    JvMessageDialog1: TMenuItem;
    JvInsertPanel1: TMenuItem;
    ToolBarGenerators: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ImageList1: TImageList;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ImageList2: TImageList;
    ToolBarSnippets: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    Timer1: TTimer;
    ToolButton15: TToolButton;
    ToolBarsPopup: TPopupMenu;
    CodeGenerator1: TMenuItem;
    CodeSnippets1: TMenuItem;
    N2: TMenuItem;
    JvToolsToolBars1: TMenuItem;
    SnippetsMenu: TPopupMenu;
    SeparatorsMenu: TPopupMenu;
    FavoritesMenu: TPopupMenu;
    ComponentsMenu: TPopupMenu;
    PaletteMenu: TPopupMenu;
    AddCurrentUnit1: TMenuItem;
    AddCurrentProject1: TMenuItem;
    N3: TMenuItem;
    SaveTimer: TTimer;
    JvToDoList1: TMenuItem;
    JvTipsOfDay1: TJvTipOfDay;
    JvRecord1: TMenuItem;
    JvUnion1: TMenuItem;
    JvEnumerations1: TMenuItem;
    ToolButton16: TToolButton;
    LibraryMenu: TPopupMenu;
    AddCode1: TMenuItem;
    N4: TMenuItem;
    JvFindFirst1: TMenuItem;
    JvExceptionHandler1: TMenuItem;
    JvCaseOf1: TMenuItem;
    JvComponentSearch1: TMenuItem;
    JvTimeLogger1: TMenuItem;
    JvDirectoryCleaner1: TMenuItem;
    JvInsertDate1: TMenuItem;
    JvInsertTime1: TMenuItem;
    JvCaseChanger1: TMenuItem;
    JvForceUpperCase1: TMenuItem;
    JvForceLowerCase1: TMenuItem;
    JvInvertCase1: TMenuItem;
    JvOpenProjectDirectory1: TMenuItem;
    JvLayout1: TMenuItem;
    JvLoad1: TMenuItem;
    JvSave1: TMenuItem;
    JvN6: TMenuItem;
    JvManage1: TMenuItem;
    Image3: TImage;
    lblJvHotLink2: TJvLabel;
    JEDIonSourceForge1: TMenuItem;
    JEDICodeLibraryJCL1: TMenuItem;
    procedure JvSebastienBuysse1Click(Sender: TObject); virtual;
    procedure JvBuyPinSoftware1Click(Sender: TObject); virtual;
    procedure JvAbout1Click(Sender: TObject); virtual;
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormDestroy(Sender: TObject); virtual;
    procedure JvOptions1Click(Sender: TObject); virtual;
    procedure JvAsciiTable1Click(Sender: TObject); virtual;
    procedure JvInsertGUID1Click(Sender: TObject); virtual;
    procedure JvExecuteProject1Click(Sender: TObject); virtual;
    procedure JvToggleComment1Click(Sender: TObject); virtual;
    procedure JvUncommentSelection1Click(Sender: TObject); virtual;
    procedure JvCommentSelection1Click(Sender: TObject); virtual;
    procedure JvRuntimeErrors1Click(Sender: TObject); virtual;
    procedure JvMethodsLexical1Click(Sender: TObject); virtual;
    procedure JvBatchCompiler1Click(Sender: TObject); virtual;
    procedure JvBugReport1Click(Sender: TObject); virtual;
    procedure JvCorrectCarriageReturns1Click(Sender: TObject); virtual;
    procedure JvConvertToHtml2Click(Sender: TObject); virtual;
    procedure JvMessageDialog1Click(Sender: TObject); virtual;
    procedure JvImportCHeader1Click(Sender: TObject); virtual;
    procedure JvInsertPanel1Click(Sender: TObject); virtual;
    procedure Timer1Timer(Sender: TObject); virtual;
    procedure ToolButton11Click(Sender: TObject); virtual;
    procedure ToolBarsPopupPopup(Sender: TObject); virtual;
    procedure CodeGenerator1Click(Sender: TObject); virtual;
    procedure CodeSnippets1Click(Sender: TObject); virtual;
    procedure ToolButton12Click(Sender: TObject); virtual;
    procedure AddCurrentUnit1Click(Sender: TObject); virtual;
    procedure FavoritesMenuPopup(Sender: TObject); virtual;
    procedure AddCurrentProject1Click(Sender: TObject); virtual;
    procedure PaletteMenuPopup(Sender: TObject); virtual;
    procedure ComponentsMenuPopup(Sender: TObject); virtual;
    procedure SaveTimerTimer(Sender: TObject); virtual;
    procedure JvToDoList1Click(Sender: TObject); virtual;
    procedure JvRecord1Click(Sender: TObject); virtual;
    procedure JvUnion1Click(Sender: TObject); virtual;
    procedure JvEnumerations1Click(Sender: TObject); virtual;
    procedure Agent1BalloonHide(Sender: TObject;
      const CharacterID: WideString); virtual;
    procedure SnippetsMenuPopup(Sender: TObject); virtual;
    procedure JvFindFirst1Click(Sender: TObject); virtual;
    procedure JvExceptionHandler1Click(Sender: TObject); virtual;
    procedure JvCaseOf1Click(Sender: TObject); virtual;
    procedure JvEricFrancois1Click(Sender: TObject);
    procedure JvComponentSearch1Click(Sender: TObject);
    procedure LibraryMenuPopup(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
    procedure AddCode1Click(Sender: TObject);
    procedure JvTimeLogger1Click(Sender: TObject);
    procedure JvDirectoryCleaner1Click(Sender: TObject);
    procedure JvInsertDate1Click(Sender: TObject);
    procedure JvInsertTime1Click(Sender: TObject);
    procedure JvBrunoPinto1Click(Sender: TObject);
    procedure JvForceUpperCase1Click(Sender: TObject);
    procedure JvForceLowerCase1Click(Sender: TObject);
    procedure JvInvertCase1Click(Sender: TObject);
    procedure JvOpenProjectDirectory1Click(Sender: TObject);
    procedure JvLoad1Click(Sender: TObject);
    procedure JvSave1Click(Sender: TObject);
    procedure JvManage1Click(Sender: TObject);
  private
    procedure CreateForm(FormName: string; Line1: string = ''; Line2: string = ''; Line3: string = '');
    procedure SendEmail(Destination, Subject: string);
    procedure OnSeparatorClick(Sender: TObject); virtual;
    procedure OnFavoriteClick(Sender: TObject); virtual;
    procedure OnTabClick(Sender: TObject); virtual;
    procedure OnCompoClick(Sender: TObject); virtual;
    function IsCode: boolean;
  public
    FSeparators: TStringList;
    FFavorites: TStringList;
    FLayouts: TStringList;

    procedure ApplyOptions;
    procedure SaveOptions;

    procedure LoadSeparators;
    procedure SaveSeparators;

    procedure LoadFavorites;
    procedure SaveFavorites;

    procedure LoadLayouts;
    procedure LoadLayout(Value: string);
    procedure SaveLayouts;

    procedure ShowTips;
    procedure AddSnippetsDirectory(Directory: string; Item: TMenuItem);
    procedure AddLibraryDirectory(Directory: string; Item: TMenuItem);
    procedure SnippetClick(Sender: TObject); virtual;
    procedure SnippetDirClick(Sender: TObject); virtual;
    procedure MenuPopup(Sender: TObject); virtual;
    procedure LibraryDirClick(Sender: TObject); virtual;
    procedure CodeLibraryClick(Sender: TObject); virtual;
    procedure InitForm(Form: TForm; Img: TImage = nil; Line1: string = ''; Line2: string = ''; Line3: string = '');
    function GetLibraryAreas: string;
  published
    procedure ToStatus(Text: string; IsHint: boolean = false);
    procedure SendMail(MailTo: string; Subject: string; Content: string);
  end;

var
  Form: TfoPopup;
  DelphiApis: TDelphiApis;
  DllFunctions: TDllExported;

implementation

uses
  JeFormAbout, JeFormOptions, JeFormASCII, JeFormRuntime, JeFormLexical,
  JeFormBatch, JeFormReport, JeFormCrlf, JeFormCodeToHtml, JeFormMessageDlg,
  JeFormSeparators, JeFormFavorites, JeFormAutoSave, JeFormTodo,
  JeFormRecord, JeFormUnion, JeFormEnumeration, JeFormSnippet, JeFormFind,
  JeFormTryExcept, JeFormCaseOf, JeFormComponentSearch, JeFormTimeLogger,
  JeFormDirectoryCleaner, JeFormAddLibrary, JeFormLibrary, JeFormTransHeader,
  JeFormLayout, JeFormLayoutManage;

{$R *.DFM}

resourcestring
  CT_FilesLibrary = '.SRC';
  CT_FilesSnippets = '.COD;*.TEM;';
  CT_EmptyMenu = '<Empty>';
  CT_AgentDefChar = 'Merlin';

  CT_LayoutSaveTitle = 'Save layout';
  CT_LayoutSaveCaption = 'Enter the name for this layout';

  {*********************************************************************}

procedure TfoPopup.CreateForm(FormName, Line1, Line2, Line3: string);
var
  Form: TForm;
  Image: TImage;
begin
  Form := TForm(TComponentClass(FindClass(FormName)).Create(nil));
  if Form <> nil then
  begin
    Image := TImage(Form.FindComponent('Image1'));
    InitForm(Form, Image, Line1, Line2, Line3);
    Form.ShowModal;
    Form.Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.ToStatus(Text: string; IsHint: boolean);
begin
  //Test if loaded
  if Timer1.Tag = 0 then
    exit;

  //Say it if specified in options and msAgent there
  if (DllFunctions.Options.SayWithAgent) and not (IsHint) then
  begin
    try
// todo: enable
//      with TAgent.Create(nil) do
//      begin
//        OnBalloonHide := Agent1BalloonHide;
//        Connected := true;
//        Characters.Load(CT_AgentDefChar, CT_AgentDefChar + '.acs');
//        with Characters.Character(CT_AgentDefChar) do
//        begin
//          { TODO :  didn't work on Win2k, maybe on Win95 }
//          //        LanguageID := LANG_ENGLISH;
//
//          Show(true);
//          MoveTo(Screen.Width - (Width + 20), Screen.Height - (Height + 20), 0);
//          Speak(Text, '');
//        end;
//      end;
    except
    end;
  end;

  //Show the hint (so, it's visible for non agent compatible systems)
  if DllFunctions.Options.HintPanelVisible then
    DelphiApis.SetHintBar(Text);
end;
{*********************************************************************}

procedure TfoPopup.JvSebastienBuysse1Click(Sender: TObject);
begin
  SendEmail(RC_SebEmail, RC_SubjectEmail);
end;
{*********************************************************************}

procedure TfoPopup.JvBrunoPinto1Click(Sender: TObject);
begin
  SendEmail(RC_BrunoEmail, RC_SubjectEmail);
end;
{*********************************************************************}

procedure TfoPopup.JvEricFrancois1Click(Sender: TObject);
begin
  SendEmail(RC_EricEmail, RC_SubjectEmail);
end;
{*********************************************************************}

procedure TfoPopup.SendEmail(Destination, Subject: string);
var
  Tmp: array[0..255] of Char;
begin
  StrPCopy(Tmp, 'Mailto:' + Destination + '?Subject=' + Subject);
  ToStatus(RC_StatEmail);
  ShellExecute(0, nil, Tmp, nil, nil, SW_NORMAL);
end;
{*********************************************************************}

procedure TfoPopup.JvBuyPinSoftware1Click(Sender: TObject);
var
  Tmp: array[0..255] of Char;
  Hint: string;
begin
  Hint := (Sender as TMenuItem).Hint;
  StrPCopy(Tmp, Hint);
  ToStatus(RC_StatSurfing + Hint);
  ShellExecute(0, nil, Tmp, nil, nil, SW_NORMAL);
end;
{*********************************************************************}

procedure TfoPopup.JvAbout1Click(Sender: TObject);
begin
  //Not basic procedure as there is no banner here
  with TfoAbout.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.FormCreate(Sender: TObject);
begin
  RegisterClasses([TfoCodeToHtml, TfoCrlf, TfoBugReport, TfoBatch,
    TfoASCII, TfoMessageDialog, TfoSeparator, TfoTransHeader,
      TfoFavorites, TfoRecord, TfoUnion, TfoEnumeration, TfoFindFirst,
      TfoTryExcept, TfoCaseOf, TfoCompoSearch, TfoDirCleaner,
      TfoAddCodeLibrary]);
  FSeparators := TStringList.Create;
  FFavorites := TStringList.Create;
  FLayouts := TStringList.Create;

  LoadSeparators;
  LoadLayouts;
end;
{*********************************************************************}

procedure TfoPopup.FormDestroy(Sender: TObject);
begin
  SaveTimer.Enabled := false;
  Timer1.Enabled := false;
  SaveOptions;
  SaveLayouts;
  FSeparators.Free;
  FFavorites.Free;
  FLayouts.Free;
end;
{*********************************************************************}

procedure TfoPopup.JvOptions1Click(Sender: TObject);
begin
  //Not basic procedure as FOptions is private
  with TfoOptions.Create(nil) do
  begin
    ComboBox1.Items.Text := FLayouts.Text;
    ComboBox1.Items.Insert(0, '<Default>');
    LoadOptions(DllFunctions.Options);
    InitForm(TForm(Image1.Parent), Image1, 'OPTIONS');
    if ShowModal = mrOk then
    begin
      SaveOptions(DllFunctions.FOptions);
      self.SaveOptions;
      ApplyOptions;
      ToStatus(RC_StatSaveOptions);
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.InitForm(Form: TForm; Img: TImage; Line1: string;
  Line2: string; Line3: string);
const
  Area1: TRect = (Left: 2; Top: 35; Right: 32; Bottom: 250);
  Area2: TRect = (Left: 35; Top: 35; Right: 65; Bottom: 250);
  Area3: TRect = (Left: 68; Top: 35; Right: 98; Bottom: 250);
  FontColor: TColor = clYellow;
  BgColor: TColor = clBlack;
var
  Stream: TStream;
  Fo: TForm;
  Max: Integer;

  procedure WriteLine(Area: TRect; Text: string);
  var
    i: integer;
    arHeight: integer;
    bmp: TBitmap;
  begin
    if Text = '' then
      exit;
    with TLabel.Create(nil) do
    begin
      Parent := self;
      Caption := '';
      Alignment := taCenter;
      for i := 1 to length(Text) do
        if i = length(Text) then
          Caption := Caption + Text[i]
        else
          Caption := Caption + Text[i] + #10;
      Color := BgColor;
      Font.Color := FontColor;
      Font.Name := 'Arial';
      Font.Size := 6;
      while (Width < (Area.Right - Area.Left)) and (Height < (Area.Bottom - Area.Top)) and (Font.Size <= Max) do
        Font.Size := Font.Size + 1;
      Font.Size := Font.Size - 1;
      Max := Font.Size;
      Transparent := false;

      arHeight := Area.Bottom - Area.Top;
      if Height < arHeight then
        Area.Top := Area.Top + ((arHeight - Height) div 2);
      Area.Right := Area.Left + Width;
      Area.Bottom := Area.Top + Height;

      bmp := TBitmap.Create;
      with bmp do
      begin
        Width := Area.Right;
        Height := Area.Bottom;
        Canvas.Brush.Color := clBlack;
        Canvas.FillRect(Rect(0, 0, Width, Height));
        PaintTo(Canvas.Handle, Area.Left, Area.Top);
        Img.Picture.Bitmap.Canvas.CopyMode := SRCPAINT;
        Area.Left := Area.Left + 1;
        Area.Top := Area.Top + 1;
        Img.Picture.Bitmap.Canvas.CopyRect(Area, bmp.Canvas, Area);
        Free;
      end;

      Free;
    end;
  end;

begin
  // MB added:  and (ImageDlgs.Picture.Graphic<>nil)
  if (Img <> nil) and (ImageDlgs.Picture.Graphic <> nil) then
  begin
// todo: enable
//    TJvPcx(ImageDlgs.Picture.Graphic).AssignTo(Img.Picture.Bitmap);
    Max := 50;
    WriteLine(Area1, Line1);
    WriteLine(Area2, Line2);
    WriteLine(Area3, Line3);
    Img.Invalidate;
  end;

  //Load icon
  Fo := TForm(GetParentForm(Form));
  if Fo <> nil then
  begin
    Stream := TResourceStream.Create(hInstance, 'Jv_ICON', RT_RCDATA);
    Fo.Icon.LoadFromStream(Stream);
    Fo.ShowHint := True;
    Stream.Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.JvImportCHeader1Click(Sender: TObject);
begin
  CreateForm('TfoTransHeader', 'HEADER', 'TRANSLATOR');
end;
{*********************************************************************}

procedure TfoPopup.JvAsciiTable1Click(Sender: TObject);
begin
  CreateForm('TfoASCII', 'ASCII', 'TABLE');
end;
{*********************************************************************}

procedure TfoPopup.ToolButton11Click(Sender: TObject);
begin
  CreateForm('TfoSeparator', 'SEPARATORS', 'EDITOR');
  SaveSeparators;
  LoadSeparators;
end;
{*********************************************************************}

procedure TfoPopup.ToolButton12Click(Sender: TObject);
begin
  CreateForm('TfoFavorites', 'FAVORITES', 'EDITOR');
end;
{*********************************************************************}

procedure TfoPopup.JvBatchCompiler1Click(Sender: TObject);
begin
  CreateForm('TfoBatch', 'BATCH', 'COMPILER');
end;
{*********************************************************************}

procedure TfoPopup.JvBugReport1Click(Sender: TObject);
begin
  CreateForm('TfoBugReport', 'JvG', 'REPORT');
end;
{*********************************************************************}

procedure TfoPopup.JvCorrectCarriageReturns1Click(Sender: TObject);
begin
  CreateForm('TfoCrlf', 'CARRIAGE', 'RETURN', 'CORRECTER');
end;
{*********************************************************************}

procedure TfoPopup.JvConvertToHtml2Click(Sender: TObject);
begin
  CreateForm('TfoCodeToHtml', 'CODE', 'TO', 'HTML');
end;
{*********************************************************************}

procedure TfoPopup.JvMessageDialog1Click(Sender: TObject);
begin
  if IsCode then
    CreateForm('TfoMessageDialog', 'MESSAGE', 'DIALOG');
end;
{*********************************************************************}

procedure TfoPopup.JvRecord1Click(Sender: TObject);
begin
  if IsCode then
    CreateForm('TfoRecord', 'RECORD', 'EDITOR');
end;
{*********************************************************************}

procedure TfoPopup.JvUnion1Click(Sender: TObject);
begin
  if IsCode then
    CreateForm('TfoUnion', 'UNION', 'EDITOR');
end;
{*********************************************************************}

procedure TfoPopup.JvEnumerations1Click(Sender: TObject);
begin
  if IsCode then
    CreateForm('TfoEnumeration', 'ENUMERATION', 'EDITOR');
end;
{*********************************************************************}

procedure TfoPopup.JvFindFirst1Click(Sender: TObject);
begin
  if IsCode then
    CreateForm('TfoFindFirst', 'FINDFIRST', 'FINDNEXT', 'FINDCLOSE');
end;
{*********************************************************************}

procedure TfoPopup.JvExceptionHandler1Click(Sender: TObject);
begin
  if IsCode then
    CreateForm('TfoTryExcept', 'EXCEPTION', 'HANDLER');
end;
{*********************************************************************}

procedure TfoPopup.JvCaseOf1Click(Sender: TObject);
begin
  if IsCode then
    CreateForm('TfoCaseOf', 'CASE OF');
end;
{*********************************************************************}

procedure TfoPopup.JvComponentSearch1Click(Sender: TObject);
begin
  CreateForm('TfoCompoSearch');
end;
{*********************************************************************}

procedure TfoPopup.JvDirectoryCleaner1Click(Sender: TObject);
begin
  CreateForm('TfoDirCleaner', 'DIRECTORY', 'CLEANER');
end;
{*********************************************************************}

procedure TfoPopup.ToolButton16Click(Sender: TObject);
begin
  with TfoLibrary.Create(nil) do
  begin
    InitForm(TForm(Panel2));
    ShowModal;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.AddCode1Click(Sender: TObject);
begin
  CreateForm('TfoAddCodeLibrary', 'ADD CODE', 'TO', 'LIBRARY');
end;
{*********************************************************************}

procedure TfoPopup.JvManage1Click(Sender: TObject);
var
  i: Integer;
begin
  with TfoLayoutManage.Create(nil) do
  begin
    ListBox1.Items.Text := FLayouts.Text;
    InitForm(TForm(Image1.Parent), Image1, 'LAYOUTS', 'MANAGER');
    if ShowModal = mrOk then
    begin
      for i := FLayouts.Count - 1 downto 0 do
        if ListBox1.Items.IndexOf(FLayouts[i]) = -1 then
          FLayouts.Delete(i);
      SaveLayouts;
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.ApplyOptions;
var
  i: integer;
  Options: TPaletteOptions;
begin
  //Show or hide toolbars
  if DllFunctions.Options.ToolBarCodeGenerator then
  begin
    if DelphiApis.AddToolBar(DllFunctions.Options.ToolBar1Left, DllFunctions.Options.ToolBar1Top, ToolBarGenerators,
      TImageList(ToolBarGenerators.Images), ToolBarGenerators.ButtonWidth) then
      for i := 0 to ToolBarGenerators.ButtonCount - 1 do
        with ToolBarGenerators.Buttons[i] do
          DelphiApis.AddButton(ToolBarGenerators.Name, Hint, Style, Tag, PopupMenu <> nil, DropDownMenu <> nil, @OnClick
            <> nil);
  end
  else
    DelphiApis.RemoveToolBar(self.ToolBarGenerators.Name);
  if DllFunctions.Options.ToolBarSnippets then
  begin
    if DelphiApis.AddToolBar(DllFunctions.Options.ToolBar2Left, DllFunctions.Options.ToolBar2Top, ToolBarSnippets,
      TImageList(ToolBarSnippets.Images), ToolBarSnippets.ButtonWidth) then
      for i := 0 to ToolBarSnippets.ButtonCount - 1 do
        with ToolBarSnippets.Buttons[i] do
          DelphiApis.AddButton(ToolBarSnippets.Name, Hint, Style, Tag, PopupMenu <> nil, DropDownMenu <> nil, @OnClick
            <> nil);
  end
  else
    DelphiApis.RemoveToolBar(self.ToolBarSnippets.Name);

  //Launch or stop the save reminder
  SaveTimer.Enabled := DllFunctions.Options.AutomaticSave and (SaveTimer.Tag = 0);
  SaveTimer.Interval := DllFunctions.Options.AutomaticTime * 60000;

  //Set palette options
  Options.Multiline := DllFunctions.Options.PaletteMultiline;
  Options.HotTrack := DllFunctions.Options.PaletteHotTrack;
  Options.AutoScroll := DllFunctions.Options.PaletteAutoScroll;
  Options.AutoSelect := DllFunctions.Options.PaletteAutoSelect;
  Options.TabPosition := DllFunctions.Options.PalettePosition;
  Options.TabStyle := DllFunctions.Options.PaletteStyle;
  Options.TabFont := DllFunctions.Options.PaletteFont;
  DelphiApis.SetToolBarOptions(Options);

  //Show or hide the hint panel
  if DllFunctions.Options.HintPanelVisible then
    DelphiApis.ShowHintBar
  else
    DelphiApis.HideHintBar;

  //Load disposition
  LoadLayout(DllFunctions.Options.Disposition);
end;
{*********************************************************************}

procedure TfoPopup.JvInsertGUID1Click(Sender: TObject);
begin
  DelphiApis.EditorAddText('''' + GenerateGUID + '''');
  ToStatus(RC_StatAddedGuid);
end;
{*********************************************************************}

procedure TfoPopup.MenuPopup(Sender: TObject);
var
  st: string;
  bool: boolean;

  procedure SetMenuItemsEnabled(Value: boolean; Items: array of TMenuItem);
  var
    i: integer;
  begin
    for i := Low(Items) to High(Items) do
    begin
      Items[i].Enabled := Value;
      DelphiApis.UpdateMenuItem(Items[i]);
    end;
  end;

begin
  //Those who needs sources
  SetMenuItemsEnabled(DelphiApis.EditorIsCode, [JvInsertGUID1, JvToggleComment1,
    JvUncommentSelection1, JvCommentSelection1, JvComments1,
      JvConvertToHtml2, JvMessageDialog1, JvEnumerations1, JvRecord1,
      JvUnion1, JvCaseOf1, JvFindFirst1, JvExceptionHandler1,
      JvInsertDate1, JvInsertTime1, JvForceUpperCase1, JvForceLowerCase1,
      JvInvertCase1]);

  //Those who needs form
  { TODO : It's broken in D6, after fixed, remove it }
//mb
  SetMenuItemsEnabled(DelphiApis.EditorIsDfm, [JvInsertPanel1]);

  //Specific ones
  bool := (DelphiApis.EditorGetCurrentProject(st)) and (FileExists(st));
  bool := bool and ((DelphiApis.EditorGetCurrentUnit(st)) and (FileExists(st)));
  SetMenuItemsEnabled(bool, [JvToDoList1, JvOpenProjectDirectory1]);
  SetMenuItemsEnabled((DelphiApis.EditorGetCurrentProject(st)) and (FileExists(ChangeFileExt(st, '.exe'))),
    [JvExecuteProject1]);
end;
{*********************************************************************}

procedure TfoPopup.JvExecuteProject1Click(Sender: TObject);
var
  st: string;
begin
  if DelphiApis.EditorGetCurrentProject(st) then
  begin
    st := ChangeFileExt(st, '.exe');
    ShellExecute(0, 'open', PChar(st), nil, nil, SW_SHOW);
  end;
end;
{*********************************************************************}

procedure TfoPopup.JvToggleComment1Click(Sender: TObject);
var
  i: integer;
  st: string;
begin
  with TStringList.Create do
  begin
    if DelphiApis.EditorGetTextSelection(st) then
    begin
      Text := st;
      for i := 0 to Count - 1 do
        if pos('//', Strings[i]) = 1 then
          Strings[i] := Copy(Strings[i], 3, Length(Strings[i]))
        else
          Strings[i] := '//' + Strings[i];
      DelphiApis.EditorAddText(Text);
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.JvUncommentSelection1Click(Sender: TObject);
var
  i: integer;
  st: string;
begin
  with TStringList.Create do
  begin
    if DelphiApis.EditorGetTextSelection(st) then
    begin
      Text := st;
      for i := 0 to Count - 1 do
        if pos('//', Strings[i]) = 1 then
          Strings[i] := Copy(Strings[i], 3, Length(Strings[i]));
      DelphiApis.EditorAddText(Text);
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.JvCommentSelection1Click(Sender: TObject);
begin
  JvUncommentSelection1Click(Sender);
  JvToggleComment1Click(Sender);
end;
{*********************************************************************}

procedure TfoPopup.JvRuntimeErrors1Click(Sender: TObject);
begin
  if foRuntime <> nil then
    foRuntime.BringToFront
  else
    with TfoRuntime.Create(nil) do
    begin
      InitForm(TForm(ComboBox1.Parent), nil);
      Show;
    end;
end;
{*********************************************************************}

procedure TfoPopup.JvMethodsLexical1Click(Sender: TObject);
begin
  if foLexical <> nil then
    foLexical.BringToFront
  else
    with TfoLexical.Create(nil) do
    begin
      InitForm(TForm(Panel2), nil);
      Show;
    end;
end;
{*********************************************************************}

procedure TfoPopup.JvInsertPanel1Click(Sender: TObject);
var
  i: integer;
  Bounds: TRect;
  Panel: TWinControl;
  Control: TControl;
  Base: TBaseCompo;
  Compos: TList;
begin
  DelphiApis.FormStartWorking;

  if DelphiApis.FormGetSelectedCount = 0 then
  begin
    ToStatus(RC_StatNoCompoSelected);
    Beep;
  end
  else
  begin
    Base.Internal := nil;
    if DelphiApis.FormGetSelectedCount = 1 then
    begin
      if DelphiApis.FormGetSelected(0).Component.Name <> DelphiApis.FormGetForm.Name then
      begin
        with TControl(DelphiApis.FormGetSelected(0).Component) do
        begin
          Parent := TWinControl(DelphiApis.CompoAdd(Base, 'TPanel', Left, Top, Width, Height).Component);
          Left := 0;
          Top := 0;
          ToStatus(RC_StatParentAdded);
        end;
      end
      else
        ToStatus(RC_StatNoCompoSelected);
    end
    else
    begin
      Bounds := Rect(0, 0, 0, 0);
      Compos := TList.Create;
      for i := 0 to DelphiApis.FormGetSelectedCount - 1 do
      begin
        UnionRect(Bounds, Bounds, TControl(DelphiApis.FormGetSelected(i).Component).BoundsRect);
        Compos.Add(DelphiApis.FormGetSelected(i).Component);
      end;
      Panel := TWinControl(DelphiApis.CompoAdd(Base, 'TPanel', Bounds.Left, Bounds.Top, Bounds.Right - Bounds.Left,
        Bounds.Bottom - Bounds.Top).Component);
      for i := 0 to Compos.Count - 1 do
      begin
        Control := TControl(Compos[i]);
        Control.Parent := Panel;
        Control.Left := Control.Left - Panel.Left;
        Control.Top := Control.Top - Panel.Top;
      end;
      Compos.Free;
    end;
  end;

  DelphiApis.FormEndWorking;
end;
{*********************************************************************}

procedure TfoPopup.SaveOptions;
var
  Stream: TStream;
  p: TPoint;
begin
  p := DelphiApis.PositionToolBar(self.ToolBarGenerators.Name);
  if p.x <> -1 then
  begin
    DllFunctions.Options.ToolBar1Left := p.x;
    DllFunctions.Options.ToolBar1Top := p.y;
  end;
  p := DelphiApis.PositionToolBar(self.ToolBarSnippets.Name);
  if p.x <> -1 then
  begin
    DllFunctions.Options.ToolBar2Left := p.x;
    DllFunctions.Options.ToolBar2Top := p.y;
  end;

  Stream := TMemoryStream.Create;
  DllFunctions.Options.GetOptions(Stream);
  Stream.Position := 0;
  DelphiApis.SetOptions(Stream);
  Stream.Free;
end;
{*********************************************************************}

procedure TfoPopup.Timer1Timer(Sender: TObject);
begin
  if DelphiApis.CompilerLoaded then
  begin
    (Sender as TTimer).Enabled := false;
    if DllFunctions.Options.TipsAtStartup then
      Form.ShowTips;
    ApplyOptions;
    Timer1.Tag := 1;

    Randomize;
    case Random(10) of
      0: ToStatus('Welcome on board !');
      1: ToStatus('Hello, we are the ' + FormatDateTime('dddddd', Now));
      2: ToStatus('Hello you! Have a good day today!');
      3: ToStatus('Greetings from the BuyPin Team!');
      4: ToStatus('Thanks for using our product, we hope you are enjoying it !');
      5: ToStatus('Helloooooooo');
      6: ToStatus('Welcome!');
      7: ToStatus('Hi! Have a nice coding moment!');
      8: ToStatus('Enjoy our product...');
      9: ToStatus('Have a good day today!');
    end;
  end;
end;
{*********************************************************************}

procedure TfoPopup.LoadSeparators;
var
  Stream: TResourceStream;
  i: integer;
  it: TMenuItem;
begin
  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);
    if ValueExists('Separators') then
      FSeparators.Text := ReadString('Separators')
    else
    begin
      //Load defaults separators
      Stream := TResourceStream.Create(hInstance, 'Jv_SEP', RT_RCDATA);
      FSeparators.LoadFromStream(Stream);
      Stream.Free;
    end;
    Free;
  end;

  MakeMenuEmpty(self.SeparatorsMenu);
  if FSeparators.Count = 0 then
  begin
    it := TMenuItem.Create(self.SeparatorsMenu);
    it.Caption := CT_EmptyMenu;
    it.Enabled := false;
    ;
    self.SeparatorsMenu.Items.Add(it);
  end
  else
    for i := 0 to FSeparators.Count - 1 do
    begin
      it := TMenuItem.Create(self.SeparatorsMenu);
      with it do
      begin
        Caption := FSeparators[i];
        OnClick := OnSeparatorClick;
      end;
      self.SeparatorsMenu.Items.Add(it);
    end;
end;
{*********************************************************************}

procedure TfoPopup.SaveSeparators;
begin
  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);
    WriteString('Separators', FSeparators.Text);
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.OnSeparatorClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    DelphiApis.EditorAddText(Caption + RC_CRLF);
end;
{*********************************************************************}

procedure TfoPopup.ToolBarsPopupPopup(Sender: TObject);
begin
  self.CodeGenerator1.Checked := DllFunctions.Options.ToolBarCodeGenerator;
  self.CodeSnippets1.Checked := DllFunctions.Options.ToolBarSnippets;
end;
{*********************************************************************}

procedure TfoPopup.CodeGenerator1Click(Sender: TObject);
begin
  DllFunctions.Options.ToolBarCodeGenerator := not (DllFunctions.Options.ToolBarCodeGenerator);
  with (Sender as TMenuItem) do
    Checked := not (Checked);
  ApplyOptions;
end;
{*********************************************************************}

procedure TfoPopup.CodeSnippets1Click(Sender: TObject);
begin
  DllFunctions.Options.ToolBarSnippets := not (DllFunctions.Options.ToolBarSnippets);
  with (Sender as TMenuItem) do
    Checked := not (Checked);
  ApplyOptions;
end;
{*********************************************************************}

procedure TfoPopup.LoadFavorites;
var
  i, j: integer;
  it: TMenuItem;
  st: string;
begin
  FFavorites.Clear;
  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);
    if ValueExists('Favorites') then
      FFavorites.Text := ReadString('Favorites');
    for i := FFavorites.Count - 1 downto 0 do
      if Trim(FFavorites[i]) = '' then
        FFavorites.Delete(i);
    Free;
  end;

  MakeMenuEmpty(self.FavoritesMenu, true);
  if FFavorites.Count = 0 then
  begin
    it := TMenuItem.Create(self.FavoritesMenu);
    it.Caption := CT_EmptyMenu;
    it.Enabled := false;
    self.FavoritesMenu.Items.Add(it);
  end
  else
    for i := 0 to FFavorites.Count - 1 do
    begin
      it := TMenuItem.Create(self.FavoritesMenu);
      with it do
      begin
        st := FFavorites[i];
        if pos('|', st) <> 0 then
        begin
          j := LastDelimiter('|', st);
          Caption := Copy(st, 1, j - 1);
          Hint := Copy(st, j + 1, Length(st));
        end
        else
        begin
          Caption := st;
          Hint := st;
        end;
        OnClick := OnFavoriteClick;
      end;
      self.FavoritesMenu.Items.Add(it);
    end;
end;
{*********************************************************************}

procedure TfoPopup.SaveFavorites;
begin
  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);
    WriteString('Favorites', FFavorites.Text);
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.OnFavoriteClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    if FileExists(Hint) then
      DelphiApis.ProjectOpenFile(Hint)
end;
{*********************************************************************}

procedure TfoPopup.AddCurrentUnit1Click(Sender: TObject);
var
  st, st2: string;
begin
  DelphiApis.EditorGetCurrentUnit(st);
  st2 := st;
  if not InputQuery('Specify Name', 'Enter a name for this entry', st) then
    exit;
  FFavorites.Add(st + '|' + st2);
  SaveFavorites;
  LoadFavorites;
end;
{*********************************************************************}

procedure TfoPopup.FavoritesMenuPopup(Sender: TObject);
var
  i: integer;
begin
  //Disable dead files
  with FavoritesMenu do
    for i := 0 to Items.Count - 1 do
      Items[i].Enabled := (Items[i].Tag = 1) or (FileExists(Items[i].Hint));
end;
{*********************************************************************}

procedure TfoPopup.AddCurrentProject1Click(Sender: TObject);
var
  st, st2: string;
begin
  DelphiApis.EditorGetCurrentProject(st);
  st2 := st;
  if not InputQuery('Specify Name', 'Enter a name for this entry', st) then
    exit;
  FFavorites.Add(st + '|' + st2);
  SaveFavorites;
  LoadFavorites;
end;
{*********************************************************************}

procedure TfoPopup.PaletteMenuPopup(Sender: TObject);
var
  i: integer;
  it: TMenuItem;
begin
  MakeMenuEmpty(PaletteMenu);
  with TStringList.Create do
  begin
    Text := DelphiApis.PaletteGetTabs;
    Sort;
    for i := 0 to Count - 1 do
    begin
      it := TMenuItem.Create(PaletteMenu);
      it.Caption := Strings[i];
      it.Hint := Strings[i];
      it.OnClick := OnTabClick;
      if (i mod 30 = 0) and (i <> 0) then
        it.Break := mbBarBreak;
      PaletteMenu.Items.Add(it);
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.OnTabClick(Sender: TObject);
begin
  with TMenuItem(Sender) do
    DelphiApis.PaletteJumpToTab(Hint);
end;
{*********************************************************************}

procedure TfoPopup.ComponentsMenuPopup(Sender: TObject);
var
  i: integer;
  it: TMenuItem;

  procedure AddComponents(ModuleName: string; Menu: TMenuItem);
  var
    i: integer;
    it: TMenuItem;
  begin
    with TStringList.Create do
    begin
      Text := DelphiApis.PaletteGetComponents(ModuleName);
      Sort;
      for i := 0 to Count - 1 do
      begin
        it := TMenuItem.Create(ComponentsMenu);
        it.Caption := Strings[i];
        it.Hint := Strings[i];
        it.OnClick := OnCompoClick;
        if (i mod 30 = 0) and (i <> 0) then
          it.Break := mbBarBreak;
        Menu.Add(it);
      end;
      if Count = 0 then
        Menu.Free;
      Free;
    end;
  end;

begin
  MakeMenuEmpty(ComponentsMenu);
  with TStringList.Create do
  begin
    Text := DelphiApis.PaletteGetModules;
    Sort;
    for i := 0 to Count - 1 do
    begin
      it := TMenuItem.Create(ComponentsMenu);
      it.Caption := Strings[i];
      if (i mod 20 = 0) and (i <> 0) then
        it.Break := mbBarBreak;
      ComponentsMenu.Items.Add(it);
      AddComponents(Strings[i], it);
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.OnCompoClick(Sender: TObject);
var
  Base: TBaseCompo;
begin
  Base.Internal := nil;
  with TMenuItem(Sender) do
  begin
    DelphiApis.FormStartWorking;
    DelphiApis.CompoAdd(Base, Hint, -1, -1, -1, -1);
    DelphiApis.FormEndWorking;
  end;
end;
{*********************************************************************}

procedure TfoPopup.SaveTimerTimer(Sender: TObject);
var
  i, j: integer;
  st: string;
begin
  if (DllFunctions.Options.AutomaticSave = false) or (not (DelphiApis.IsSomethingModified)) then
    exit;

  j := AutoSaveToInteger(DllFunctions.Options.AutomaticAction);
  if (DllFunctions.Options.AutomaticConfirm = false) or (j = 4) then
  begin
    with TfoAutoSave.Create(nil) do
    begin
      if (j = 1) or (j = 3) then
        ComboBox1.ItemIndex := 1
      else
        ComboBox1.ItemIndex := 0;
      if ShowModal = mrOk then
      begin
        j := ComboBox1.ItemIndex;
        case j of
          2:
            begin
              Free;
              Exit;
            end;
          3:
            begin
              SaveTimer.Tag := 1;
              SaveTimer.Enabled := false;
              Free;
              Exit;
            end;
          4:
            begin
              DllFunctions.Options.AutomaticSave := false;
              SaveTimer.Enabled := false;
              Free;
              Exit;
            end;
        end;
      end;
      Free;
    end;
  end;
  case j of
    0:
      begin
        DelphiApis.CallMenuItem('FileSaveAllItem');
        ToStatus(RC_StatSaveAllFiles);
      end;
    1:
      begin
        DelphiApis.CallMenuItem('FileSaveItem');
        ToStatus(RC_StatSaveFile);
      end;
    2: with TStringList.Create do
      begin
        Text := DelphiApis.GetAllOpenedFiles;
        for i := 0 to Count - 1 do
          if not (FileExists(Strings[i])) then
            exit;
        DelphiApis.CallMenuItem('FileSaveAllItem');
        ToStatus(RC_StatSaveAllFiles);
      end;
    3:
      begin
        DelphiApis.EditorGetCurrentUnit(st);
        if FileExists(st) then
        begin
          DelphiApis.CallMenuItem('FileSaveItem');
          ToStatus(RC_StatSaveFile);
        end;
      end;
  end;
end;
{*********************************************************************}

procedure TfoPopup.JvToDoList1Click(Sender: TObject);
begin
  with TfoToDo.Create(nil) do
  begin
    InitForm(TForm(Panel1), nil);
    Show;
  end;
end;
{*********************************************************************}

procedure TfoPopup.ShowTips;
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(hInstance, 'Jv_TIPS', RT_RCDATA);
// todo: enable
//  JvTipsOfDay1.Hints.LoadFromStream(Stream);
//  JvTipsOfDay1.Execute;
//  DllFunctions.Options.TipsAtStartup := JvTipsOfDay1.GetShowHintOnStartup;
  Stream.Free;
end;
{*********************************************************************}

procedure TfoPopup.Agent1BalloonHide(Sender: TObject;
  const CharacterID: WideString);
begin
// todo: enable
//  with (Sender as TAgent) do
//  begin
//    if Characters.Get_Item(CT_AgentDefChar) <> nil then
//      with Characters.Get_Item(CT_AgentDefChar) do
//      begin
//        Characters.Unload(CT_AgentDefChar);
//        Connected := false;
//      end;
//    Free;
//  end;
end;
{*********************************************************************}

procedure TfoPopup.SnippetsMenuPopup(Sender: TObject);
begin
  AddSnippetsDirectory(DelphiApis.PackPath + 'Snippets\', self.SnippetsMenu.Items);
end;
{*********************************************************************}

procedure TfoPopup.AddSnippetsDirectory(Directory: string;
  Item: TMenuItem);
var
  t: TSearchRec;
  res: integer;
  st: string;
  it, it2: TMenuItem;
  Dirs, Files: TStringList;
  i: integer;
begin
  MakeItemEmpty(Item, true);

  if (Directory <> '') and (Directory[length(Directory)] <> '\') then
    Directory := Directory + '\';
  res := FindFirst(Directory + '*.*', faAnyFile, t);
  Dirs := TStringList.Create;
  Files := TStringList.Create;
  Dirs.Sorted := true;
  Files.Sorted := true;

  while res = 0 do
  begin
    if (faDirectory and t.Attr) = faDirectory then
    begin
      if (t.Name <> '.') and (t.Name <> '..') then
        Dirs.Add(t.Name);
    end
    else
    begin
      st := UpperCase(ExtractFileExt(t.Name));
      if pos(st, CT_FilesSnippets) <> 0 then
        Files.Add(t.Name);
    end;
    res := FindNext(t);
  end;

  if Dirs.Count + Files.Count > 0 then
    if Item.Count = 1 then
      Item[0].Visible := false;

  for i := 0 to Dirs.Count - 1 do
  begin
    it := TMenuItem.Create(Item);
    with it do
    begin
      Caption := Dirs[i];
      Hint := Directory + Dirs[i] + '\';
      OnClick := SnippetDirClick;

      it2 := TMenuItem.Create(it);
      it2.Caption := CT_EmptyMenu;
      it2.Enabled := false;
      it2.Tag := 1;
      Add(it2);
    end;
    Item.Add(it);
  end;

  for i := 0 to Files.Count - 1 do
  begin
    it := TMenuItem.Create(Item);
    with it do
    begin
      Caption := ChangeFileExt(Files[i], '');
      Hint := Directory + Files[i];
      OnClick := SnippetClick;
    end;
    Item.Add(it);
  end;

  FindClose(t);
  Dirs.Free;
  Files.Free;
end;
{*********************************************************************}

procedure TfoPopup.SnippetClick(Sender: TObject);
type
  TParams = record
    ParamName: string;
    Value: string;
  end;
var
  st, st2: string;
  i, j: integer;
  w: DWord;
  Params: array[0..200] of TParams;
  p: array[0..255] of char;
begin
  with Sender as TMenuItem do
  begin
    st := UpperCase(ExtractFileExt(Hint));
    if st = '.TEM' then
    begin
      if foSnippet = nil then
      begin
        TfoSnippet.Create(nil);
        InitForm(TForm(foSnippet.Panel1));
      end;
      foSnippet.LoadFromFile(Hint);
      foSnippet.Show;
      foSnippet.BringToFront;
    end
    else if st = '.COD' then
    begin
      j := 4;
      Params[0].ParamName := '%NOW%';
      Params[0].Value := DateTimeToStr(Now);
      Params[1].ParamName := '%DATE%';
      Params[1].Value := DateToStr(Now);
      Params[2].ParamName := '%TIME%';
      Params[2].Value := TimeToStr(Now);
      Params[3].ParamName := '%AUTHOR%';
      w := 255;
      WNetGetUser(nil, p, w);
      p[w] := #0;
      Params[3].Value := p;

      with TStringList.Create do
      begin
        LoadFromFile(Hint);
        i := 0;
        while (i < Count) and (UpperCase(Strings[i]) <> '<CODE>') do
        begin
          st := Strings[i];
          if (pos('%', st) <> 0) and (pos('=', st) <> 0) then
          begin
            Params[j].ParamName := Copy(st, 1, pos('=', st) - 1);
            st := Copy(st, pos('=', st) + 1, Length(st));
            st2 := '';
            if InputQuery('Value required', st, st2) then
              Params[j].Value := st2
            else
            begin
              Free;
              Exit;
            end;
            inc(j);
          end;
          inc(i);
        end;

        inc(i);
        st := RC_CRLF;
        while (i < Count) and (UpperCase(Strings[i]) <> '</CODE>') do
        begin
          st := st + Strings[i] + RC_CRLF;
          inc(i);
        end;

        for i := 0 to j - 1 do
          st := StringReplace(st, Params[i].ParamName, Params[i].Value, [rfIgnoreCase, rfReplaceAll]);

        DelphiApis.EditorAddText(st);

        Free;
      end;
    end;
  end;
end;
{*********************************************************************}

procedure TfoPopup.SnippetDirClick(Sender: TObject);
begin
  AddSnippetsDirectory((Sender as TMenuItem).Hint, Sender as TMenuItem);
end;
{*********************************************************************}

procedure TfoPopup.SendMail(MailTo, Subject, Content: string);
var
  Msg: TMapiMessage;
  Recipient: TMapiRecipDesc;
begin
  ZeroMemory(@Recipient, sizeof(Recipient));
  Recipient.ulRecipClass := MAPI_TO;
  Recipient.lpszName := 'Sebastien Buysse';
  Recipient.lpszAddress := 'SMTP:sbuysse@buypin.com';

  ZeroMemory(@Msg, SizeOf(Msg));
  Msg.lpszSubject := PChar(Subject);
  Msg.lpszNoteText := PChar(Content);
  Msg.nRecipCount := 1;
  Msg.lpRecips := @Recipient;
  Msg.nFileCount := 0;

  MapiSendMail(0, Application.Handle, Msg, 0, 0);
end;
{*********************************************************************}

procedure TfoPopup.AddLibraryDirectory(Directory: string; Item: TMenuItem);
var
  t: TSearchRec;
  res: integer;
  st: string;
  it, it2: TMenuItem;
  Dirs, Files: TStringList;
  i: integer;
begin
  MakeItemEmpty(Item, true);

  if (Directory <> '') and (Directory[length(Directory)] <> '\') then
    Directory := Directory + '\';
  res := FindFirst(Directory + '*.*', faAnyFile, t);
  Dirs := TStringList.Create;
  Files := TStringList.Create;
  Dirs.Sorted := true;
  Files.Sorted := true;

  while res = 0 do
  begin
    if (faDirectory and t.Attr) = faDirectory then
    begin
      if (t.Name <> '.') and (t.Name <> '..') then
        Dirs.Add(t.Name);
    end
    else
    begin
      st := UpperCase(ExtractFileExt(t.Name));
      if pos(st, CT_FilesLibrary) <> 0 then
        Files.Add(t.Name);
    end;
    res := FindNext(t);
  end;

  if Dirs.Count + Files.Count > 0 then
    if Item.Count = 1 then
      Item[0].Visible := false;

  for i := 0 to Dirs.Count - 1 do
  begin
    it := TMenuItem.Create(Item);
    with it do
    begin
      Caption := Dirs[i];
      Hint := Directory + Dirs[i] + '\';
      OnClick := LibraryDirClick;

      it2 := TMenuItem.Create(it);
      it2.Caption := CT_EmptyMenu;
      it2.Enabled := false;
      it2.Tag := 1;
      Add(it2);
    end;
    Item.Add(it);
  end;

  for i := 0 to Files.Count - 1 do
  begin
    it := TMenuItem.Create(Item);
    with it do
    begin
      Caption := ChangeFileExt(Files[i], '');
      Hint := Directory + Files[i];
      OnClick := CodeLibraryClick;
    end;
    Item.Add(it);
  end;

  FindClose(t);
  Dirs.Free;
  Files.Free;
end;
{*********************************************************************}

procedure TfoPopup.LibraryMenuPopup(Sender: TObject);
begin
  AddLibraryDirectory(DelphiApis.PackPath + 'Library\', self.LibraryMenu.Items);
end;
{*********************************************************************}

procedure TfoPopup.CodeLibraryClick(Sender: TObject);
begin
  with TStringList.Create do
  begin
    LoadFromFile((Sender as TMenuItem).Hint);
    Add('');
    Insert(0, '');
    DelphiApis.EditorAddText(Text);
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.LibraryDirClick(Sender: TObject);
begin
  AddLibraryDirectory((Sender as TMenuItem).Hint, Sender as TMenuItem);
end;
{*********************************************************************}

procedure TfoPopup.JvTimeLogger1Click(Sender: TObject);
begin
  with TfoTimeLogger.Create(nil) do
  begin
    InitForm(TForm(Panel2), nil);
    ShowModal;
  end;
end;
{*********************************************************************}

function TfoPopup.GetLibraryAreas: string;
var
  t: TSearchRec;
  res: integer;
  Dirs: TStringList;
  Directory: string;
begin
  Directory := DelphiApis.PackPath + 'Library\';
  res := FindFirst(Directory + '*.*', faAnyFile, t);
  Dirs := TStringList.Create;
  Dirs.Sorted := true;

  while res = 0 do
  begin
    if (faDirectory and t.Attr) = faDirectory then
      if (t.Name <> '.') and (t.Name <> '..') then
        Dirs.Add(t.Name);
    res := FindNext(t);
  end;

  FindClose(t);
  result := Dirs.Text;
  Dirs.Free;
end;
{*********************************************************************}

function TfoPopup.IsCode: boolean;
begin
  //Test if it's code and tell it to the user if not
  result := DelphiApis.EditorIsCode;
  if not result then
    ToStatus(RCE_MustBeCode);
end;
{*********************************************************************}

procedure TfoPopup.JvInsertDate1Click(Sender: TObject);
begin
  DelphiApis.EditorAddText(DateToStr(Date));
end;
{*********************************************************************}

procedure TfoPopup.JvInsertTime1Click(Sender: TObject);
begin
  DelphiApis.EditorAddText(TimeToStr(Time));
end;
{*********************************************************************}

procedure TfoPopup.JvForceUpperCase1Click(Sender: TObject);
var
  st: string;
begin
  if DelphiApis.EditorGetTextSelection(st) then
    DelphiApis.EditorAddText(UpperCase(st));
end;
{*********************************************************************}

procedure TfoPopup.JvForceLowerCase1Click(Sender: TObject);
var
  st: string;
begin
  if DelphiApis.EditorGetTextSelection(st) then
    DelphiApis.EditorAddText(LowerCase(st));
end;
{*********************************************************************}

procedure TfoPopup.JvInvertCase1Click(Sender: TObject);
var
  st: string;
  i: integer;
begin
  if DelphiApis.EditorGetTextSelection(st) then
  begin
    for i := 1 to Length(st) do
      if (st[i] in ['a'..'z']) then
        st[i] := UpCase(st[i])
      else
        st[i] := LowerCase(st[i])[1];
    DelphiApis.EditorAddText(UpperCase(st));
  end;
end;
{*********************************************************************}

procedure TfoPopup.JvOpenProjectDirectory1Click(Sender: TObject);
var
  st: string;
begin
  if DelphiApis.EditorGetCurrentProject(st) then
  begin
    st := ExtractFilePath(st);
    ShellExecute(0, 'open', PChar(st), nil, nil, SW_SHOW);
  end;
end;
{*********************************************************************}

procedure TfoPopup.LoadLayout(Value: string);
var
  i: Integer;
  Layout: TLayoutRecord;

  procedure AssignLayout(CLayout: TLayoutClass; var Layout: TLayoutRecord);
  begin
    with Layout do
    begin
      Left := CLayout.Left;
      Top := CLayout.Top;
      Width := CLayout.Width;
      Height := CLayout.Height;
      WindowState := CLayout.WindowState;
      Floating := CLayout.Floating;
      DockSite := CLayout.DockSite;
    end;
  end;

begin
  i := FLayouts.IndexOf(Value);
  if i <> -1 then
    with TLayoutsClass(FLayouts.Objects[i]) do
    begin
      AssignLayout(MainForm, Layout);
      DelphiApis.SetMainFormLayout(Layout);

      AssignLayout(EditorForm, Layout);
      DelphiApis.SetEditorLayout(Layout);

      AssignLayout(InspectorForm, Layout);
      DelphiApis.SetInspectorLayout(Layout);
    end;
end;
{*********************************************************************}

procedure TfoPopup.JvLoad1Click(Sender: TObject);
begin
  with TfoLayoutChooser.Create(nil) do
  begin
    ComboBox1.Items.Text := FLayouts.Text;
    if ComboBox1.Items.Count > 0 then
      ComboBox1.ItemIndex := 0;
    if ShowModal = mrOk then
      LoadLayout(ComboBox1.Text);
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.LoadLayouts;
var
  i: integer;
  Stream: TMemoryStream;
  buf: array[0..64000] of byte;
  Layout: TLayoutRecord2;
  CLayout: TLayoutsClass;

  procedure AssignLayout(var CLayout: TLayoutClass; Layout: TLayoutRecord2);
  begin
    with CLayout do
    begin
      Left := Layout.Left;
      Top := Layout.Top;
      Width := Layout.Width;
      Height := Layout.Height;
      WindowState := Layout.WindowState;
      Floating := Layout.Floating;
      DockSite := Layout.DockSite;
    end;
  end;

begin
  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);
    if ValueExists('Layouts') then
    begin
      Stream := TMemoryStream.Create;
      i := ReadBinaryData('Layouts', buf, sizeof(buf));
      Stream.Write(buf, i);
      Stream.Position := 0;

      while Stream.Position < Stream.Size do
      begin
        CLayout := TLayoutsClass.Create;

        Stream.Read(Layout, SizeOf(Layout));
        CLayout.MainForm := TLayoutClass.Create;
        AssignLayout(CLayout.MainForm, Layout);

        Stream.Read(Layout, SizeOf(Layout));
        CLayout.EditorForm := TLayoutClass.Create;
        AssignLayout(CLayout.EditorForm, Layout);

        Stream.Read(Layout, SizeOf(Layout));
        CLayout.InspectorForm := TLayoutClass.Create;
        AssignLayout(CLayout.InspectorForm, Layout);

        FLayouts.AddObject(Layout.Name, CLayout);
      end;

      Stream.Free;
    end;
    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.SaveLayouts;
var
  i: integer;
  Stream: TMemoryStream;
  buf: array[0..64000] of byte;
  Layout: TLayoutRecord2;

  procedure AssignLayout(CLayout: TLayoutClass; var Layout: TLayoutRecord2);
  begin
    with Layout do
    begin
      Left := CLayout.Left;
      Top := CLayout.Top;
      Width := CLayout.Width;
      Height := CLayout.Height;
      WindowState := CLayout.WindowState;
      Floating := CLayout.Floating;
      DockSite := CLayout.DockSite;
    end;
  end;

begin
  with TRegistry.Create do
  begin
    OpenKey(DelphiApis.RegistryPath, true);

    Stream := TMemoryStream.Create;
    for i := 0 to FLayouts.Count - 1 do
    begin
      AssignLayout(TLayoutsClass(FLayouts.Objects[i]).MainForm, Layout);
      Layout.Name := FLayouts.Strings[i];
      Stream.Write(Layout, SizeOf(Layout));

      AssignLayout(TLayoutsClass(FLayouts.Objects[i]).EditorForm, Layout);
      Layout.Name := FLayouts.Strings[i];
      Stream.Write(Layout, SizeOf(Layout));

      AssignLayout(TLayoutsClass(FLayouts.Objects[i]).InspectorForm, Layout);
      Layout.Name := FLayouts.Strings[i];
      Stream.Write(Layout, SizeOf(Layout));
    end;
    Stream.Position := 0;
    i := Stream.Read(buf, Stream.Size);
    WriteBinaryData('Layouts', buf, i);
    Stream.Free;

    Free;
  end;
end;
{*********************************************************************}

procedure TfoPopup.JvSave1Click(Sender: TObject);
var
  st: string;
  Layout: TLayoutRecord;
  CLayout: TLayoutsClass;

  procedure AssignLayout(var CLayout: TLayoutClass; Layout: TLayoutRecord);
  begin
    with CLayout do
    begin
      Left := Layout.Left;
      Top := Layout.Top;
      Width := Layout.Width;
      Height := Layout.Height;
      WindowState := Layout.WindowState;
      Floating := Layout.Floating;
      DockSite := Layout.DockSite;
    end;
  end;

begin
  if InputQuery(CT_LayoutSaveTitle, CT_LayoutSaveCaption, st) then
  begin
    if FLayouts.IndexOf(st) <> -1 then
    begin
      ToStatus(RCE_LayoutExists);
      Exit;
    end;
    CLayout := TLayoutsClass.Create;

    Layout := DelphiApis.GetMainFormLayout;
    CLayout.MainForm := TLayoutClass.Create;
    AssignLayout(CLayout.MainForm, Layout);

    Layout := DelphiApis.GetInspectorLayout;
    CLayout.InspectorForm := TLayoutClass.Create;
    AssignLayout(CLayout.InspectorForm, Layout);

    Layout := DelphiApis.GetEditorLayout;
    CLayout.EditorForm := TLayoutClass.Create;
    AssignLayout(CLayout.EditorForm, Layout);

    FLayouts.AddObject(st, CLayout);

    SaveLayouts;
  end;
end;
{*********************************************************************}
end.
