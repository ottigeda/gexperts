{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_Exported.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JeExpert_Exported;

// {.$I Versions.inc}
{.$DEFINE DEBUG}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Forms, Dialogs, Registry, ToolsAPI, Menus, ComCtrls,
  ExtCtrls, JeExpert_Types, JeExpert_Strings, JeExpert_Editor, JeExpert_FormEditor;

type
  TNotifierSpy = class(TInterfacedObject, IOTAIDENotifier)
  protected
    { This procedure is called immediately after the item is successfully saved.
      This is not called for IOTAWizards }
    procedure AfterSave;
    { This function is called immediately before the item is saved. This is not
      called for IOTAWizard }
    procedure BeforeSave;
    { The associated item is being destroyed so all references should be dropped.
      Exceptions are ignored. }
    procedure Destroyed;
    { This associated item was modified in some way. This is not called for
      IOTAWizards }
    procedure Modified;
    { This procedure is called for many various file operations within the
      IDE }
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    { This function is called immediately before the compiler is invoked.
      Set Cancel to True to cancel the compile }
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    { This procedure is called immediately following a compile.  Succeeded
      will be true if the compile was successful }
    procedure AfterCompile(Succeeded: Boolean); overload;
  public
    destructor Destroy; override;
  end;

 TExportedApis = class(TDelphiApis)
 private
   FMenuList: TStringList;
   FToolBarList: TStringList;
   FFormEditor : TJeFormEditor;
   FPopup: TPopupMenu;
   FNotifierSpy: TNotifierSpy;
   FStatusInstalled: Boolean;
 protected
   function GetComponent(Name: string): TComponent;overload;
   function GetComponent(Name: string;Parent: string):TComponent;overload;
   function GetCurrentSource:string;
   procedure ExecuteAndWait(FileName: string; visibility: integer;WorkDir:string);
   procedure TabResize(Sender: TObject);
   procedure FormComplete(var Base:TBaseCompo);
   procedure ExecuteItem(Sender: TIMenuItemIntf);

   procedure ToolButtonClick(Sender: TObject);
   procedure ButtonPopup(Sender: TObject);
   procedure ButtonDropDown(Sender: TObject);
   procedure ToolButtonForceDrop(Sender: TObject);
   procedure ApplicationHint(Sender: TObject);
 public
   constructor Create;
   destructor Destroy;override;

   function GetOptions(var Options: TStream):boolean;override;
   function SetOptions(Options: TStream):boolean;override;
   function GetHint(Sender: TObject):string;override;
   function CompilerLoaded:boolean;override;

   procedure AddMenu(MenuName: string; InsertBefore: string; Menu: TPopupMenu; OnPopup:TNotifyEvent = nil);override;
   procedure RemoveMenu(MenuName: string);override;

   function AddToolBar(FLeft,FTop: integer;ToolBar: TToolbar;ImagesLst: TImageList;ButtonWth: integer):boolean;override;
   procedure RemoveToolBar(ToolBarName: string);override;
   function PositionToolBar(ToolBarName: string): TPoint;override;
   procedure AddButton(ToolBarName: string;Hint: string;Style: TToolButtonStyle;Tag: integer;
             PopupMenu: boolean; DropDownMenu: boolean;OnClick: boolean);override;

   procedure GetCompilerVersion(var Compiler: byte; var  Version:double);override;
   procedure UpdateMenuItem(Item: TMenuItem);override;
   function CallMenuItem(ItemName: string):boolean;override;

   function EditorGetTextSelection(var Text:string):boolean;override;
   function EditorAddText(Text: string):boolean;override;
   function EditorInsertText(Text: string):boolean;override;
   function EditorGetWholeText(var Text:string):boolean;override;
   function EditorSetWholeText(Text: string):boolean;override;
   function EditorDeleteSelected:boolean;override;
   function EditorGetNumberOfLines:integer;override;
   function EditorGetCurrentLine : integer;override;
   function EditorSetLine(Line: integer): boolean;override;
   function EditorIsCode:boolean;override;
   function EditorIsDfm:boolean;override;
   function EditorGetCurrentUnit(var UnitName:string):boolean;override;
   function EditorGetCurrentProject(var ProjectName:string):boolean;override;
   function EditorIsTextSelected : boolean;override;
   function EditorGetColorScheme: TJeCodeColor;override;

   function ProjectClose:boolean;override;
   function ProjectOpen(ProjectName: string):boolean;override;
   function ProjectOpenFile(FileName: string):boolean;override;
   function ProjectSave:boolean;override;
   function ProjectCloseFile(FileName: string):boolean;override;
   function ProjectSaveFile(FileName: string):boolean;override;

   function GetBatchCompiler: string;override;
   function GetPrjExtension: string;override;
   function GetIncludePaths : string;override;
   function CompileProject(Project:String;LogResult:boolean = false): boolean;override;
   function IsSomethingModified:boolean;override;
   function GetAllOpenedFiles :string;override;

   function FormGetForm: TForm;override;
   function FormGetSelected  (Index:integer):TBaseCompo;override;
   function FormGetSelectedCount : integer;override;
   function FormStartWorking : boolean;override;
   function FormEndWorking : boolean;override;
   function CompoAdd(Parent: TBaseCompo;CompoType: string;Left,Top,Width,Heigth:integer):TBaseCompo;override;

   function PaletteGetTabs: string;override;
   procedure PaletteJumpToTab(TabName: string);override;
   function PaletteGetModules: string;override;
   function PaletteGetComponents(ModuleName:string):string;override;
   procedure SetToolBarOptions(Options: TPaletteOptions);override;

   function Title:string;override;
   function PackVersion:string;override;
   function RegistryPath:string;override;
   function ApplicationHandle:THandle;override;
   function PackPath:string;override;

   procedure ShowHintBar;override;
   procedure HideHintBar;override;
   procedure SetHintBar(Value: string);override;

   function GetMainFormLayout: TLayoutRecord;override;
   function GetInspectorLayout: TLayoutRecord;override;
   function GetEditorLayout: TLayoutRecord;override;
   function GetGeneralLayout(Form: TForm): TLayoutRecord;
   procedure SetMainFormLayout(Value: TLayoutRecord);override;
   procedure SetInspectorLayout(Value: TLayoutRecord);override;
   procedure SetEditorLayout(Value: TLayoutRecord);override;
   procedure SetGeneralLayout(Form: TForm; Value: TLayoutRecord);
 end;

 function MyHook(code: integer; wparam: WPARAM; lparam: LPARAM): LRESULT stdcall;

implementation

uses
  JeExpert_Base, ExptIntf
//  ,ToolsAPI
  ;

var
  FHook: hHook;
  FCalls: Integer;

{*****************************************************************}
procedure TExportedApis.AddButton(ToolBarName: string;Hint: string;Style: TToolButtonStyle;Tag: integer;
         PopupMenu: boolean; DropDownMenu: boolean;OnClick: boolean);

var
 i:integer;
 Bar: TToolBar;
 Button: TToolButton;
begin
  i:=FToolBarList.IndexOf(ToolBarName);
  if i<>-1 then
  begin
    Bar := TToolBar(FToolBarList.Objects[i]);
    Button := TToolButton.Create(Bar);

    Button.Top := 0;
    if Bar.ButtonCount<1 then
      Button.Left := 0
    else
    begin
      if Bar.ButtonCount=1 then
        Bar.Buttons[0].Width := Bar.Buttons[0].Width*2;
      Button.Left := (Bar.Buttons[Bar.ButtonCount-1].Left)+(Bar.Buttons[Bar.ButtonCount-1].Width);
    end;
    Button.Parent := Bar;
    Button.Caption := '';
    Button.Style := Style;
    Button.Hint := Hint;
    Button.ShowHint := True;
    Button.Tag := Tag;
    if PopupMenu then Button.PopupMenu := FPopup;
    if DropDownMenu then
    begin
      Button.DropDownMenu := TPopupMenu.Create(Button);
      Button.DropDownMenu.Tag := Tag;
      Button.DropDownMenu.OnPopup := ButtonDropDown;
    end;
    Button.AutoSize := true;
    if OnClick then
      Button.OnClick := ToolButtonClick
    else
      Button.OnClick := ToolButtonForceDrop;

    {$IFDEF D4}
    if (Button.Left+Button.Width>Bar.Constraints.MinWidth) then
       Bar.Constraints.MinWidth := Button.Left + Button.Width;
    Bar.Width := Bar.Width*2;
    {$ENDIF}
    for i:=0 to Bar.ButtonCount-1 do
      Bar.Buttons[i].ImageIndex := i;
  end;
end;
{*****************************************************************}
procedure TExportedApis.AddMenu(MenuName, InsertBefore: string;
  Menu: TPopupMenu; OnPopup: TNotifyEvent);
var
  MainMenu : TIMainMenuIntf;
  ExpertMenu : TIMenuItemIntf;
  NewMenu : TIMenuItemIntf;
  i : integer;

  procedure AddToMenu(index:integer;Menu:TIMenuItemIntf; Item:TMenuItem);
  var
   i:integer;
   ThisItem:TIMenuItemIntf;
   t:TMenuItem;
   stream:TStream;

    function GetFlags:TIMenuFlags;
    begin
      result := [mfVisible];
      if Item.Enabled then result := result+[mfEnabled];
      if Item.Checked then result := result + [mfChecked];
      if Item.Break = mbBreak then result := result + [mfBreak];
      if Item.Break = mbBarBreak then result := result + [mfBarBreak];
      if Item.RadioItem then result := result + [mfRadioItem];
    end;

  begin
    //Verify name unicity
    if GetComponent(Item.Name)<>nil then
       Item.Name:='GENERATED_'+IntToStr(Application.ComponentCount+1);

    //Add it
    ThisItem := Menu.InsertItem(index,Item.Caption,Item.Name,Item.Hint,
                                Item.ShortCut,Item.HelpContext,Item.GroupIndex,
                                GetFlags,ExecuteItem);

    //Add the image if any
    if (Item.Bitmap<>nil)and(Item.Bitmap.Width>0) then
    begin
      t:=TMenuItem(GetComponent(Item.Name));
      if t<>nil then
      begin
        //Small trick to copy it (Assign makes an exception!)
        t.Bitmap := TBitmap.Create;
        stream := TMemoryStream.Create;
        Item.Bitmap.SaveToStream(stream);
        stream.Position:=0;
        t.Bitmap.LoadFromStream(stream);
        stream.Free;
      end;
    end;

    //Add submenus
    for i:=0 to Item.Count-1 do
      AddToMenu(i,ThisItem,Item.Items[i]);
  end;

begin
  {$IFDEF DEBUG}ShowMessage('A. Adding Menu (Start) - '+MenuName);{$ENDIF}

  //Getting place to start
  MainMenu := ToolServices.GetMainMenu;

  if MainMenu = nil then exit;

  ExpertMenu := MainMenu.FindMenuItem(InsertBefore);
  if ExpertMenu = nil then
  begin
    ShowMessage(RC_BadMenuName);
    exit;
  end;
  i := ExpertMenu.GetIndex;
  ExpertMenu := ExpertMenu.GetParent;
  if ExpertMenu = nil then exit;

  //Adding the menu entry
  NewMenu := ExpertMenu.InsertItem(i,MenuName,Menu.Name,'',0,0,0,[mfEnabled,mfVisible],TIMenuClickEvent(OnPopup));
  FMenuList.Add(MenuName);
  for i:=0 to Menu.Items.Count-1 do
    AddToMenu(i,NewMenu,Menu.Items[i]);
  MainMenu.Free;
  {$IFDEF DEBUG}ShowMessage('A. Adding Menu (End) - '+MenuName);{$ENDIF}
end;
{*****************************************************************}
function TExportedApis.AddToolBar(FLeft, FTop: integer;
  ToolBar: TToolbar;ImagesLst: TImageList; ButtonWth: integer):boolean;
var
 FToolBar:TToolBar;
 i:integer;
 Tab: TTabControl;
 bmp: TBitmap;

 function Max(A,B:integer):integer;
 begin
   if A>B then result:=A else result:=B;
 end;

begin
  {$IFDEF DEBUG}ShowMessage('B. Adding Toolbar (Start) - '+ToolBar.Name);{$ENDIF}
  result := false;
  i:=FToolBarList.IndexOf(ToolBar.Name);
  if i<>-1 then exit;

  //Check Height of the Compiler Form
  Tab:=TTabControl(GetComponent('TabControl','AppBuilder'));
  if Tab<>nil then
  begin
    Tab.Constraints.MinHeight:=Max(ToolBar.Height+ToolBar.Top,Tab.Constraints.MinHeight);
    Tab.Constraints.MaxHeight:=Max(ToolBar.Height+ToolBar.Top,Tab.Constraints.MinHeight);
  end;

  //Add the toolbar
  FToolBar := TToolBar.Create(Application);
  FToolBar.Left := FLeft;
  FToolBar.Top := FTop;
  with FToolBar do
  begin
    Align := alTop;
    AutoSize := true;
    Flat := true;
    Parent := TControlBar(GetComponent('ControlBar1','AppBuilder'));
    Left := FLeft;
    Top := FTop;
    BorderWidth := 0;
    EdgeBorders := [];
    ShowHint := True;
    Wrapable := False;
    DragMode := dmAutomatic;
    {$IFDEF D4}
    ButtonWidth := ButtonWth;
    {$ENDIF}

    Images := TImageList.Create(Application);
    Images.Width := 16;
    Images.Height := 16;
    bmp := TBitmap.Create;
    for i:=0 to ImagesLst.Count-1 do
    begin
      ImagesLst.GetBitmap(i, bmp);
      Images.Add(bmp,nil);
    end;
    bmp.Free;

    OnClick := ToolBar.OnClick;
    OnDblClick := ToolBar.OnDblClick;
    OnEndDrag := ToolBar.OnEndDrag;
    OnStartDrag := ToolBar.OnStartDrag;
    OnDragDrop := ToolBar.OnDragDrop;
    OnDragOver := ToolBar.OnDragOver;
    OnDockDrop := ToolBar.OnDockDrop;
    OnDockOver := ToolBar.OnDockOver;
    OnEnter := ToolBar.OnEnter;
    OnExit := ToolBar.OnExit;
    OnGetSiteInfo := ToolBar.OnGetSiteInfo;
    OnMouseDown := ToolBar.OnMouseDown;
    OnMouseMove := ToolBar.OnMouseMove;
    OnMouseUp := ToolBar.OnMouseUp;
    OnResize := ToolBar.OnResize;
    OnUnDock := ToolBar.OnUnDock;
    result := true;
  end;
  FToolBar.Left := FLeft;
  FToolBar.Top := FTop;
  FToolBarList.AddObject(ToolBar.Name,TObject(FToolBar));
  {$IFDEF DEBUG}ShowMessage('B. Adding Toolbar (End) - '+ToolBar.Name);{$ENDIF}
end;
{*****************************************************************}
function TExportedApis.ApplicationHandle: THandle;
begin
  result := Application.Handle;
end;
{*****************************************************************}
procedure TExportedApis.ApplicationHint(Sender: TObject);
begin
  SetHintBar(GetLongHint(Application.Hint));
end;
{*****************************************************************}
procedure TExportedApis.ButtonDropDown(Sender: TObject);
var
 i: integer;
begin
  with Sender as TPopupMenu do
   if PopupComponent is TToolBar then
     with PopupComponent as TToolBar do
     begin
       for i:=0 to ButtonCount-1 do
         Buttons[i].Down := false;
       for i:=0 to ButtonCount-1 do
         if Buttons[i].Tag = ((Sender as TPopupMenu).Tag) then
           with Buttons[i] do
             FDllFunctions.DropDown(Tag,ClientToScreen(Point(0,Height)));
     end;
end;
{*****************************************************************}
procedure TExportedApis.ButtonPopup(Sender: TObject);
begin
  with Sender as TPopupMenu do
   if PopupComponent is TToolButton then
     with PopupComponent as TToolButton do
       FDllFunctions.PopupMenu(Tag,ClientToScreen(Point(0,Height)));
end;
{*****************************************************************}
function TExportedApis.CallMenuItem(ItemName: string): boolean;
var
 tm:TMenuItem;
begin
  tm := TMenuItem(GetComponent(ItemName,'AppBuilder'));
  if (tm<>nil)and (tm.enabled) then
  begin
    result := true;
    tm.Click;
  end
  else
    result:=false;
end;
{*****************************************************************}
function TExportedApis.CompileProject(Project: String;
  LogResult: boolean): boolean;
var
 st,Directory,Params:string;
begin
  Directory:=ExtractFilePath(Project);
  Directory:=Copy(Directory,1,Length(Directory)-1);
  Params:=' "'+Project+'" -u"'+Directory+';'+GetIncludePaths+'"';
  if LogResult then
    Params:=Params+' >"'+ChangeFileExt(Project,'.log"');

  st:='"'+GetBatchCompiler+'"'+Params;
  ExecuteAndWait(st,SW_HIDE,Directory);

  result:=true;
end;
{*****************************************************************}
function TExportedApis.CompilerLoaded: boolean;
var
 w: TWinControl;
begin
  w:=TWinControl(GetComponent('AppBuilder'));
  result:=(w<>nil) and (w.Visible);
end;
{*****************************************************************}
function TExportedApis.CompoAdd(Parent: TBaseCompo; CompoType: string;
  Left, Top, Width, Heigth: integer): TBaseCompo;
begin
  result.Internal:=FFormEditor.AddComponent(Parent.Internal,CompoType,Left,Top,Width,Heigth);
  FormComplete(result);
end;
{*****************************************************************}
constructor TExportedApis.Create;
var
 Tab: TTabControl;
 i: Integer;
begin
  //Initialize variables
  FMenuList := TStringList.Create;
  FToolBarList := TStringList.Create;
  FPopup := TPopupMenu.Create(Application);
  FPopup.OnPopup := ButtonPopup;
  FStatusInstalled := false;
  Application.OnHint := ApplicationHint;

  //Spy the resize of the Component Palette
  Tab:=TTabControl(GetComponent('TabControl','AppBuilder'));
  if Tab<>nil then
    Tab.OnResize := TabResize;

  //Add the notifier to spy modifications
  {$WARNINGS OFF}
  FNotifierSpy := TNotifierSpy.Create;
  {$WARNINGS ON}
  ToolServices.AddNotifier(FNotifierSpy);

  i := 0;
  FHook := SetWindowsHookEx(WH_CALLWNDPROC,MyHook,i,
    GetCurrentThreadID);
end;
{*****************************************************************}
destructor TExportedApis.Destroy;
var
 Tab: TTabControl;
begin
  if not (csDestroying in Application.ComponentState) then
  begin
    //Remove events in the Application
    Application.OnHint := nil;

    //Remove the spy on the component palette
    Tab:=TTabControl(GetComponent('TabControl','AppBuilder'));
    if Tab<>nil then
      Tab.OnResize := nil;
      
    //Remove the notifier
    ToolServices.RemoveNotifier(FNotifierSpy);
    FNotifierSpy.Free;
    UnhookWindowsHookEx(FHook);
  end;

  //Destroy variables
  FMenuList.Free;
  FToolBarList.Free;
  FPopup.Free;

  inherited;
end;
{*****************************************************************}
function TExportedApis.EditorAddText(Text: string): boolean;
begin
  result:=EditorDeleteSelected and EditorInsertText(Text);
end;
{*****************************************************************}
function TExportedApis.EditorDeleteSelected: boolean;
begin
  result:=false;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=DeleteSelected;
      Free;
    end;
end;
{*****************************************************************}
function TExportedApis.EditorGetColorScheme: TJeCodeColor;

 procedure LoadDefaults;
 begin
   with result do
   begin
     Plain.Style:=[];
     Plain.Color:=TColor(0);
     Plain.BGColor:=TColor(15);
     Comment:=Plain;Reserved:=Plain;Identifier:=Plain;Symbol:=Plain;String_:=Plain;Number:=Plain;Assembler_:=Plain;

     Comment.Color:=TColor(8);
     Reserved.Style:=[fsBold]
   end;
 end;

 function LoadIt(Key:string):TJeFontAttributes;
 begin
   with TRegistry.Create do
   begin
     OpenKey(ToolServices.GetBaseRegistryKey,false);
     OpenKey('Editor\Highlight\'+Key,false);
     result.Style:=[];
     if ValueExists('Foreground Color') then
       result.Color:=TColor(ReadInteger('Foreground Color'))
     else
       result.Color:=TColor(0);
     if (ValueExists('Italic')) and (ReadString('Italic')='True') then
       result.Style:=result.Style+[fsItalic];
     if (ValueExists('Bold')) and (ReadString('Bold')='True') then
       result.Style:=result.Style+[fsBold];
     if (ValueExists('Underline')) and (ReadString('Underline')='True') then
       result.Style:=result.Style+[fsUnderline];
     if (ValueExists('Background Color'))and(ValueExists('Default Background'))and
       (ReadString('Default Background')='False') then
       result.BGColor:=TColor(ReadInteger('Background Color'))
     else
       result.BGColor:=TColor(-1);
     Free;
   end;
 end;

begin
  LoadDefaults;

  result.Plain:=LoadIt('Plain text');
  result.Comment:=LoadIt('Comment');
  result.Reserved:=LoadIt('Reserved Word');
  result.Identifier:=LoadIt('Identifier');
  result.Symbol:=LoadIt('Symbol');
  result.String_:=LoadIt('String');
  result.Number:=LoadIt('Number');
  result.Assembler_:=LoadIt('Assembler');
end;
{*****************************************************************}
function TExportedApis.EditorGetCurrentLine: integer;
begin
  result:=-1;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=GetCurrentLine;
      Free;
    end;
end;
{*****************************************************************}
function TExportedApis.EditorGetCurrentProject(
  var ProjectName: string): boolean;
begin
  ProjectName := ToolServices.GetProjectName;
  result := true;
end;
{*****************************************************************}
function TExportedApis.EditorGetCurrentUnit(var UnitName: string): boolean;
begin
  UnitName := GetCurrentSource;
  result := true;
end;
{*****************************************************************}
function TExportedApis.EditorGetNumberOfLines: integer;
begin
  result:=-1;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=GetNumberOfLines;
      Free;
    end;
end;
{*****************************************************************}
function TExportedApis.EditorGetTextSelection(var Text: string): boolean;
begin
  result:=false;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=GetTextSelection(Text);
      Free;
    end;
end;
{*****************************************************************}
function TExportedApis.EditorGetWholeText(var Text: string): boolean;
begin
  result:=false;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=GetWholeText(Text);
      Free;
    end;
end;
{*****************************************************************}
function TExportedApis.EditorInsertText(Text: string): boolean;
begin
  result:=false;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=InsertText(Text);
      Free;
    end;
end;
{*****************************************************************}
function TExportedApis.EditorIsCode: boolean;
var
 FModule:TIModuleInterface;
 FEdit:TIEditorInterface;
begin
  result:=false;
  FModule:=ToolServices.GetModuleInterface(GetCurrentSource);
  if FModule<>nil then
  begin
    FEdit:=FModule.GetEditorInterface;
    if FEdit<>nil then
    begin
      result := FEdit.GetViewCount>0;
      FEdit.free;
    end;
    FModule.Free;
  end;
end;
{*****************************************************************}
function TExportedApis.EditorIsDfm: boolean;
var
 form:TIComponentInterface;
 ti:TIFormInterface;
 tis:TIModuleInterface;
// MB
 CurrentSource: string;
begin
  result := false;

// MB
CurrentSource:= GetCurrentSource;
//  Debugger.LogMsg (CurrentSource);
// showMessage(CurrentSource);

  tis := ToolServices.GetModuleInterface(CurrentSource);

  if tis<>nil then
  begin
    ti := tis.GetFormInterface;
    if ti<>nil then
    begin
      form := ti.GetFormComponent;
      if form<>nil then
      begin
        result := true;
        form.free;
      end;
      ti.free;
    end;
    tis.free;
  end;
end;
{*****************************************************************}
function TExportedApis.EditorIsTextSelected: boolean;
var
 st:string;
begin
  EditorGetTextSelection(st);
  result:=Length(st)>0;
end;
{*****************************************************************}
function TExportedApis.EditorSetLine(Line: integer): boolean;
begin
  result:=false;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=SetLine(Line);
      Free;
    end;
end;
{*****************************************************************}
function TExportedApis.EditorSetWholeText(Text: string): boolean;
begin
  result:=false;
  if EditorIsCode then
    with TJeEditor.Create(GetCurrentSource) do
    begin
      result:=SetWholeText(Text);
      Free;
    end;
end;
{*****************************************************************}
procedure TExportedApis.ExecuteAndWait(FileName: string;
  visibility: integer; WorkDir: string);
var
 zAppName:array[0..512] of char;
 zCurDir:array[0..255] of char;
 StartupInfo:TStartupInfo;
 ProcessInfo:TProcessInformation;
begin
  StrPCopy(zAppName,FileName);
  StrPCopy(zCurDir,WorkDir);
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not(CreateProcess(nil,zAppName,nil,nil,false,Create_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
     nil,ZCurDir,StartupInfo,ProcessInfo)) then
  WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
end;
{*****************************************************************}
procedure TExportedApis.ExecuteItem(Sender: TIMenuItemIntf);
begin
  if Sender.GetItemCount=0 then
    FDllFunctions.ExecuteItem(Sender.GetName);
end;
{*****************************************************************}
procedure TExportedApis.FormComplete(var Base: TBaseCompo);
begin
  if Base.Internal<>nil then
  begin
    Base.Component:=TComponent(TIComponentInterface(Base.Internal).GetComponentHandle);
    Base.Name:=Base.Component.Name;
    Base.ClassType:=Base.Component.ClassType;
  end;
end;
{*****************************************************************}
function TExportedApis.FormEndWorking: boolean;
begin
  if FFormEditor<>nil then FFormEditor.Free;
  FFormEditor:=nil;
  result:=true;
end;
{*****************************************************************}
function TExportedApis.FormGetForm: TForm;
begin
  result:=FFormEditor.GetMainForm;
end;
{*****************************************************************}
function TExportedApis.FormGetSelected(Index: integer): TBaseCompo;
begin
  result.Internal:=FFormEditor.GetSelected(Index);
  FormComplete(result);
end;
{*****************************************************************}
function TExportedApis.FormGetSelectedCount: integer;
begin
  result:=FFormEditor.GetSelectedCount;
end;
{*****************************************************************}
function TExportedApis.FormStartWorking: boolean;
begin
  FFormEditor:=TJeFormEditor.Create(GetCurrentSource);
  result:=FFormEditor<>nil;
end;
{*****************************************************************}
function TExportedApis.GetAllOpenedFiles: string;
var
 i: integer;
 st: string;
begin
  with TStringList.Create do
  begin
    for i:=0 to ToolServices.GetUnitCount-1 do
    begin
      st := ToolServices.GetUnitName(i);
      if pos(UpperCase(ExtractFileExt(st)),RC_FileExtensions)<>0 then
        Add(st);
    end;
    result := Text;
    Free;
  end;
end;
{*****************************************************************}
function TExportedApis.GetBatchCompiler: string;
begin
  result:=ExtractFilePath(Application.ExeName)+RC_BatchCompiler;
end;
{*****************************************************************}
procedure TExportedApis.GetCompilerVersion(var Compiler: byte;
  var Version: double);
begin
  {$IFDEF D4}
   Compiler := COMP_DELPHI;
   Version  := 4;
  {$ENDIF}

  {$IFDEF D5}
   Compiler := COMP_DELPHI;
   Version  := 5;
  {$ENDIF}
end;
{*****************************************************************}
function TExportedApis.GetComponent(Name: string): TComponent;

  procedure GetIt(Parent: TComponent);
  var
   i:integer;
  begin
    result := Parent.FindComponent(Name);
    if result=nil then
    begin
      i:=0;
      while (i<Parent.ComponentCount) and (result=nil) do
      begin
        GetIt(Parent.Components[i]);
        inc(i);
      end;
    end;
  end;

begin
  GetIt(Application);
end;
{*****************************************************************}
function TExportedApis.GetComponent(Name, Parent: string): TComponent;
var
 ParComp:TComponent;
begin
  ParComp := Application.FindComponent(Parent);
  if ParComp<>nil then
    result := ParComp.FindComponent(Name)
  else
    result := nil;
end;
{*****************************************************************}
function TExportedApis.GetCurrentSource: string;
begin
  result:=ToolServices.GetCurrentFile;
  if Pos(UpperCase(ExtractFileExt(result)),RC_SourceExtension)=0 then
    result:=ChangeFileExt(result,RC_SourceDefExt);
end;
{*****************************************************************}
function TExportedApis.GetEditorLayout: TLayoutRecord;
begin
  result := GetGeneralLayout(TForm(GetComponent('EditWindow_0')));
end;
{*****************************************************************}
function TExportedApis.GetGeneralLayout(Form: TForm): TLayoutRecord;
begin
  if Form=nil then
   with result do
   begin
     Left := -1;
     Top := -1;
     Width := 0;
     Height := 0;
     WindowState := wsNormal;
     Floating := true;
     DockSite := '';
   end;
  with result do
  begin
    Left := Form.Left;
    Top := Form.Top;
    Width := Form.Width;
    Height := Form.Height;
    WindowState := Form.WindowState;
    Floating := Form.HostDockSite=nil;
    if Form.HostDockSite<>nil then
      DockSite := Form.HostDockSite.Name
    else
      DockSite := '';
  end;
end;
{*****************************************************************}
function TExportedApis.GetHint(Sender: TObject): string;
begin
  //Get hint from an item of the menu
  result:=TIMenuItemIntf(Sender).GetHint;
end;
{*****************************************************************}
function TExportedApis.GetIncludePaths: string;
var
 st:string;
begin
  with TRegistry.Create do
  begin
    OpenKey(ToolServices.GetBaseRegistryKey,false);
    OpenKey('Library',false);
    if ValueExists('Search Path') then
      result:=ReadString('Search Path')
    else
      result:='';

    st:=ExtractFilePath(GetBatchCompiler);
    st:=ExtractFilePath(Copy(st,1,Length(st)-1));
    st:=Copy(st,1,Length(st)-1);
    result:=StringReplace(result,RC_PathPseudo,st,[rfReplaceAll,rfIgnoreCase]);

    Free;
  end;
end;
{*****************************************************************}
function TExportedApis.GetInspectorLayout: TLayoutRecord;
begin
  result := GetGeneralLayout(TForm(GetComponent('PropertyInspector')));
end;
{*****************************************************************}
function TExportedApis.GetMainFormLayout: TLayoutRecord;
begin
  result := GetGeneralLayout(TForm(GetComponent('AppBuilder')));
end;
{*****************************************************************}
function TExportedApis.GetOptions(var Options: TStream): boolean;
var
 Buffer:array [0..10000] of byte;
 cnt:integer;
begin
  try
    ZeroMemory(@Buffer,SizeOf(Buffer));
    with TRegistry.Create do
    begin
      OpenKey(RC_RegistryPath,true);
      cnt := ReadBinaryData(RC_ExpertOptions,Buffer,sizeof(Buffer));
      Options.Write(Buffer,cnt);
      Free;
    end;
    result := true;
  except
    result := false;
  end;
end;
{*****************************************************************}
function TExportedApis.GetPrjExtension: string;
begin
  result:=RC_PrjExtension;
end;
{*****************************************************************}
procedure TExportedApis.HideHintBar;
var
 StatusBar: TStatusBar;
begin
  if not FStatusInstalled then
    Exit;
  StatusBar := TStatusBar(GetComponent('statusbar','EditWindow_0'));
  if (StatusBar=nil) or (StatusBar.Panels.Count<=3) then
    Exit;
  StatusBar.Panels[3].Free;
  FStatusInstalled := false;
end;
{*****************************************************************}
function TExportedApis.IsSomethingModified: boolean;
begin
  result := TMenuItem(GetComponent('FileSaveAllItem','AppBuilder')).Enabled;
end;
{*****************************************************************}
function TExportedApis.PackPath: string;
begin
  with TRegistry.Create do
  begin
    OpenKey(RC_RegistryPath,true);
    if ValueExists('Path') then
    begin
      result := ReadString('Path');
      if (Length(Result)>0) and (Result[Length(Result)]<>'\') then
        result := result+'\';
    end
    else
      result := '';
    Free;
  end;
end;
{*****************************************************************}
function TExportedApis.PackVersion: string;
begin
  result := RC_PackVersion;
end;
{*****************************************************************}
function TExportedApis.PaletteGetComponents(ModuleName: string): string;
var
 i,j: integer;
begin
  with TStringList.Create do
  begin
    j:=-1;
    i:=0;

    while i<ToolServices.GetModuleCount do
      if ToolServices.GetModuleName(i)=ModuleName then
      begin
        j:=i;i:=MAXINT;
      end
      else inc(i);

    if j<>-1 then
      for i:=0 to ToolServices.GetComponentCount(j)-1 do
        Add(ToolServices.GetComponentName(j,i));
    result := Text;
    Free;
  end;
end;
{*****************************************************************}
function TExportedApis.PaletteGetModules: string;
var
 i: integer;
begin
  with TStringList.Create do
  begin
    for i:=0 to ToolServices.GetModuleCount-1 do
      Add(ToolServices.GetModuleName(i));
    result := Text;
    Free;
  end;
end;
{*****************************************************************}
function TExportedApis.PaletteGetTabs: string;
var
 Tab: TTabControl;
begin
  Tab:=TTabControl(GetComponent('TabControl','AppBuilder'));
  if Tab<>nil then
    result := Tab.Tabs.Text;
end;
{*****************************************************************}
procedure TExportedApis.PaletteJumpToTab(TabName: string);
var
 Tab: TTabControl;
 i: integer;
begin
  Tab:=TTabControl(GetComponent('TabControl','AppBuilder'));
  if Tab<>nil then
  begin
    i := Tab.Tabs.IndexOf(TabName);
    if i<>-1 then
    begin
      Tab.TabIndex := i;
      if Assigned(Tab.OnChange) then
        Tab.OnChange(Tab);
    end;
  end;
end;
{*****************************************************************}
function TExportedApis.PositionToolBar(ToolBarName: string): TPoint;
var
 i:integer;
begin
  i:=FToolBarList.IndexOf(ToolBarName);
  result.x := -1;
  if i<>-1 then
   with TToolBar(FToolBarList.Objects[i]) do
    result:=Point(Left,Top);
end;
{*****************************************************************}
function TExportedApis.ProjectClose: boolean;
begin
  result := ToolServices.CloseProject;
end;
{*****************************************************************}
function TExportedApis.ProjectCloseFile(FileName: string): boolean;
begin
  result := ToolServices.CloseFile(FileName);
end;
{*****************************************************************}
function TExportedApis.ProjectOpen(ProjectName: string): boolean;
begin
  result := ToolServices.OpenProject(ProjectName);
end;
{*****************************************************************}
function TExportedApis.ProjectOpenFile(FileName: string): boolean;
begin
  result := ToolServices.OpenFile(FileName);
end;
{*****************************************************************}
function TExportedApis.ProjectSave: boolean;
begin
  result := ToolServices.SaveProject;
end;
{*****************************************************************}
function TExportedApis.ProjectSaveFile(FileName: string): boolean;
begin
  result := ToolServices.SaveFile(FileName);
end;
{*****************************************************************}
function TExportedApis.RegistryPath: string;
begin
  result := RC_RegistryPath+RC_SpecificPath;
end;
{*****************************************************************}
procedure TExportedApis.RemoveMenu(MenuName: string);
var
 i:integer;
 MainMenu : TIMainMenuIntf;
 ExpertMenu : TIMenuItemIntf;

 procedure DeleteIt(Menu: TIMenuItemIntf);
 var
  i:integer;
 begin
   for i:=0 to Menu.GetItemCount-1 do
     DeleteIt(Menu.GetItem(0));
   Menu.DestroyMenuItem;
 end;

begin
  i := FMenuList.IndexOf(MenuName);
  if (i<>-1) then
  begin
    MainMenu := ToolServices.GetMainMenu;
    if MainMenu = nil then exit;
    ExpertMenu := MainMenu.FindMenuItem(MenuName);
    if ExpertMenu<>nil then
      DeleteIt(ExpertMenu);
  end;
end;
{*****************************************************************}
procedure TExportedApis.RemoveToolBar(ToolBarName: string);
var
 i:integer;
begin
  i:=FToolBarList.IndexOf(ToolBarName);
  if i<>-1 then
   with TToolBar(FToolBarList.Objects[i]) do
   begin
     Parent:=nil;
     Free;
     FToolBarList.Delete(i);
   end;
end;
{*****************************************************************}
procedure TExportedApis.SetEditorLayout(Value: TLayoutRecord);
begin
  SetGeneralLayout(TForm(GetComponent('EditWindow_0')),Value);
end;
{*****************************************************************}
procedure TExportedApis.SetGeneralLayout(Form: TForm;
  Value: TLayoutRecord);
begin
  if Form=nil then
    Exit;
  with Form do
  begin
    Left := Value.Left;
    Top := Value.Top;
    Width := Value.Width;
    Height := Value.Height;
    WindowState := Value.WindowState;
    Form.ManualDock(TWinControl(GetComponent(Value.DockSite,'EditWindow_0')),nil,alClient);
    Left := Value.Left;
    Top := Value.Top;
    Width := Value.Width;
    Height := Value.Height;
    WindowState := Value.WindowState;
  end;
end;
{*****************************************************************}
procedure TExportedApis.SetHintBar(Value: string);
var
 StatusBar: TStatusBar;
begin
  if not FStatusInstalled then
    Exit;
  StatusBar := TStatusBar(GetComponent('statusbar','EditWindow_0'));
  if (StatusBar=nil) then
    Exit;
  //Check if not disappeared!
  if (StatusBar.Panels.Count<=3) then
    ShowHintBar;
  StatusBar.Panels[3].Text := Value;
end;
{*****************************************************************}
procedure TExportedApis.SetInspectorLayout(Value: TLayoutRecord);
begin
  SetGeneralLayout(TForm(GetComponent('PropertyInspector')),Value);
end;
{*****************************************************************}
procedure TExportedApis.SetMainFormLayout(Value: TLayoutRecord);
begin
  SetGeneralLayout(TForm(GetComponent('AppBuilder')),Value);
end;
{*****************************************************************}
function TExportedApis.SetOptions(Options: TStream): boolean;
var
 Buffer:array [0..10000] of byte;
begin
  try
    Options.Position := 0;
    with TRegistry.Create do
    begin
      OpenKey(RC_RegistryPath,true);
      Options.Read(Buffer,Options.Size);
      WriteBinaryData(RC_ExpertOptions,Buffer,Options.Size);
      Free;
    end;
    result := true;
  except
    result := false;
  end;
end;
{*****************************************************************}
procedure TExportedApis.SetToolBarOptions(Options: TPaletteOptions);
var
 Tab: TTabControl;
begin
  Tab:=TTabControl(GetComponent('TabControl','AppBuilder'));
  if Tab<>nil then
  begin
    Tab.MultiLine := Options.Multiline;
    Tab.HotTrack := Options.HotTrack;
    case Options.TabPosition of
      0 : Tab.TabPosition := tpTop;
      1 : Tab.TabPosition := tpBottom;
      2 : Tab.TabPosition := tpLeft;
      3 : Tab.TabPosition := tpRight;
    end;
    case Options.TabStyle of
      0 : Tab.Style := tsTabs;
      1 : Tab.Style := tsButtons;
      2 : Tab.Style := tsFlatButtons;
    end;
    if Trim(Options.TabFont)<>'' then
      Tab.Font.Name := Options.TabFont;
    with TRegistry.Create do
    begin
      OpenKey(ToolServices.GetBaseRegistryKey,false);
      OpenKey('Extras',true);
      if Options.AutoScroll then
         WriteString('AutoPaletteScroll','1')
      else
         WriteString('AutoPaletteScroll','0');
      if Options.AutoSelect then
         WriteString('AutoPaletteSelect','1')
      else
         WriteString('AutoPaletteSelect','0');
      Free;
    end;
  end;
end;
{*****************************************************************}
procedure TExportedApis.ShowHintBar;
var
 StatusBar: TStatusBar;
begin
  StatusBar := TStatusBar(GetComponent('statusbar','EditWindow_0'));
  if (StatusBar=nil) or (StatusBar.Panels.Count>3) then
    Exit;
  StatusBar.Panels[2].Width:=70;
  with StatusBar.Panels.Add do
  begin
    Width := 0;
    Style := psText;
    Bevel := pbNone;
    Text := 'JExperts Hint Viewer';
    FStatusInstalled := true;
  end;
end;
{*****************************************************************}
procedure TExportedApis.TabResize(Sender: TObject);
var
 FHeight: integer;
begin
  if Sender is TTabControl then
   with (Sender as TTabControl) do
   begin
     FHeight:=Height-(DisplayRect.Bottom-DisplayRect.top)+30;
     Constraints.MinHeight:=FHeight;
     (Parent as TWinControl).Constraints.MaxHeight:=FHeight;
   end;
end;
{*****************************************************************}
function TExportedApis.Title: string;
begin
  result := RC_PackTitle;
end;
{*****************************************************************}
procedure TExportedApis.ToolButtonClick(Sender: TObject);
begin
  FDllFunctions.ExecuteButton((Sender as TToolButton).Tag);
end;
{*****************************************************************}
procedure TExportedApis.ToolButtonForceDrop(Sender: TObject);
begin
  with Sender as TToolButton do
   FDllFunctions.DropDown(Tag,ClientToScreen(Point(0,Height)));
end;
{*****************************************************************}
procedure TExportedApis.UpdateMenuItem(Item: TMenuItem);
var
 it:TMenuItem;
begin
  it:=TMenuItem(GetComponent(Item.Name));
  if it=nil then exit;
  it.Caption := Item.Caption;
  it.Enabled := Item.Enabled;
  it.Break := Item.Break;
  it.Checked := Item.Checked;
  it.GroupIndex := Item.GroupIndex;
  it.ShortCut := Item.ShortCut;
  it.OnClick := Item.OnClick;
end;
{*****************************************************************}



{*****************************************************************}
function MyHook(code: integer; wparam: WPARAM;lparam: LPARAM): LRESULT;
begin
  if (FCalls=0) then
  begin
    inc(FCalls);
    with PCWPStruct(lParam)^ do
      if ((Message = WM_PARENTNOTIFY) and (wParam = WM_CREATE) )
        or (Message=WM_SHOWWINDOW) then
        FDllFunctions.CompilerNotification(ntWindowOpened,'');
    dec(FCalls);
  end;
  result := CallNextHookEx(FHook, Code, wParam, lParam);
end;
{*****************************************************************}


{ TNotifierSpy }

{*****************************************************************}
destructor TNotifierSpy.Destroy;
begin
  inherited;
end;
{*****************************************************************}
procedure TNotifierSpy.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);

  function ConvertCode(NotifyCode: TFileNotification): TNotificationType;
  begin
    case NotifyCode of
     fnFileOpening : result := ntFileOpening;
     fnFileOpened : result := ntFileOpened;
     fnFileClosing : result := ntFileClosing;
     fnProjectOpening : result := ntProjectOpening;
     fnProjectOpened : result := ntProjectOpened;
     fnProjectClosing : result := ntProjectClosing;
     fnAddedToProject : result := ntAddedToProject;
     fnRemovedFromProject : result := ntRemovedFromProject;
     fnDefaultDesktopLoad : result := ntDefaultDesktopLoad;
     fnDefaultDesktopSave : result := ntDefaultDesktopSave;
     fnProjectDesktopLoad : result := ntProjectDesktopLoad;
     fnprojectDesktopSave : result := ntprojectDesktopSave;
     fnPackageInstalled : result := ntPackageInstalled;
     fnPackageUninstalled : result := ntPackageUninstalled;
     else result := ntFileOpening;
    end;
  end;

begin
  FDllFunctions.CompilerNotification(ConvertCode(NotifyCode),FileName);
end;
{*****************************************************************}
end.
