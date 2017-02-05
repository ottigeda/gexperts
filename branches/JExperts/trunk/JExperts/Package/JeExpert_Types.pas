{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_Types.PAS, released on 2001-02-28.

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

unit JeExpert_Types;

interface

uses
  Windows, Messages, Dialogs, Classes, Menus, ComCtrls, Graphics, Controls,
  Forms, Buttons;

const
  ExpertVersion = 1.0;
  ExpertVersionInt = 1;

  COMP_DELPHI = $01;
  COMP_BUILDER = $02;
  COMP_KYLIX = $03;

type
  //-----------------------------------------------------------------
  //Records used for the code to html
  //-----------------------------------------------------------------
  TJeFontAttributes = record
    Style: TFontStyles;
    Color: TColor;
    BGColor: TColor;
  end;

  TJeCodeColor = record
    Plain: TJeFontAttributes;
    Comment: TJeFontAttributes;
    Reserved: TJeFontAttributes;
    Identifier: TJeFontAttributes;
    Symbol: TJeFontAttributes;
    String_: TJeFontAttributes;
    Number: TJeFontAttributes;
    Assembler_: TJeFontAttributes;
  end;

  //-----------------------------------------------------------------
  //Record used in form manipulations
  //-----------------------------------------------------------------
  TBaseCompo = record
    Internal: pointer;
    Name: string;
    ClassType: TClass;
    Component: TComponent;
  end;

  //-----------------------------------------------------------------
  //Record used in time logger
  //-----------------------------------------------------------------
  TLoggerRecord = record
    TimeStarted: TDateTime;
    TimeFinished: TDateTime;
  end;
  TDataDateTime = class
    Value: TDateTime;
  end;

  //-----------------------------------------------------------------
  //record used in the palette options
  //-----------------------------------------------------------------
  TPaletteOptions = record
    Multiline: boolean;
    HotTrack: boolean;
    AutoScroll: boolean;
    AutoSelect: boolean;
    TabPosition: Integer;
    TabStyle: Integer;
    TabFont: string;
  end;

  //-----------------------------------------------------------------
  //Record used in the SpeedButton editor
  //-----------------------------------------------------------------
  TSpeedRecord = record
    Width, Height: Integer;
    Flat, Enabled, Transparent: boolean;
    Caption: string;
    GroupIndex, Margin, Spacing, NumGlyphs: Integer;
    Layout: TButtonLayout;
    Glyph: TBitmap;
  end;

  //-----------------------------------------------------------------
  //Record used in the BitBtn editor
  //-----------------------------------------------------------------
  TBitBtnRecord = record
    Width, Height: Integer;
    Default, Cancel, Enabled: boolean;
    Caption: string;
    Margin, Spacing, NumGlyphs: Integer;
    Layout: TButtonLayout;
    Kind: TBitBtnKind;
    ModalResult: TModalResult;
    Glyph: TBitmap;
  end;

  //-----------------------------------------------------------------
  //Record used in the layouts
  //-----------------------------------------------------------------
  TLayoutRecord = record
    Left, Top, Width, Height: Integer;
    WindowState: TWindowState;
    Floating: Boolean;
    DockSite: string;
  end;
  TLayoutRecord2 = record
    Name: string[255];
    Left, Top, Width, Height: Integer;
    WindowState: TWindowState;
    Floating: Boolean;
    DockSite: string[255];
  end;
  TLayoutClass = class
  public
    Left, Top, Width, Height: Integer;
    WindowState: TWindowState;
    Floating: Boolean;
    DockSite: string;
  end;
  TLayoutsClass = class
  public
    MainForm, EditorForm, InspectorForm: TLayoutClass;
  end;

  //-----------------------------------------------------------------
  //Set used in the notifications
  //-----------------------------------------------------------------
  TNotificationType = (ntFileOpening, ntFileOpened, ntFileClosing,
    ntProjectOpening, ntProjectOpened, ntProjectClosing, ntAddedToProject,
    ntRemovedFromProject, ntDefaultDesktopLoad, ntDefaultDesktopSave,
    ntProjectDesktopLoad, ntProjectDesktopSave, ntPackageInstalled,
    ntPackageUninstalled, ntWindowOpened);

  //-----------------------------------------------------------------
  //Api's exported by the expert
  //-----------------------------------------------------------------
  TDelphiApis = class
  public
    function CompilerLoaded: boolean; virtual; abstract;

    //Menu related functions
    procedure AddMenu(MenuName: string; InsertBefore: string; Menu: TPopupMenu; OnPopup: TNotifyEvent = nil); virtual;
      abstract;
    procedure RemoveMenu(MenuName: string); virtual; abstract;
    procedure UpdateMenuItem(Item: TMenuItem); virtual; abstract;

    //Toolbar related functions
    function AddToolBar(Left, Top: integer; ToolBar: TToolbar; ImagesLst: TImageList; ButtonWidth: integer): boolean;
      virtual; abstract;
    procedure RemoveToolBar(ToolBarName: string); virtual; abstract;
    function PositionToolBar(ToolBarName: string): TPoint; virtual; abstract;
    procedure AddButton(ToolBarName: string; Hint: string; Style: TToolButtonStyle; Tag: integer;
      PopupMenu: boolean; DropDownMenu: boolean; OnClick: boolean); virtual; abstract;

    //Pack utility functions
    function GetOptions(var Options: TStream): boolean; virtual; abstract;
    function SetOptions(Options: TStream): boolean; virtual; abstract;
    function GetHint(Sender: TObject): string; virtual; abstract;

    //Delphi utility functions
    procedure GetCompilerVersion(var Compiler: byte; var Version: double); virtual; abstract;
    function CallMenuItem(ItemName: string): boolean; virtual; abstract;

    function EditorGetTextSelection(var Text: string): boolean; virtual; abstract;
    function EditorAddText(Text: string): boolean; virtual; abstract;
    function EditorInsertText(Text: string): boolean; virtual; abstract;
    function EditorGetWholeText(var Text: string): boolean; virtual; abstract;
    function EditorSetWholeText(Text: string): boolean; virtual; abstract;
    function EditorDeleteSelected: boolean; virtual; abstract;
    function EditorGetNumberOfLines: integer; virtual; abstract;
    function EditorGetCurrentLine: integer; virtual; abstract;
    function EditorSetLine(Line: integer): boolean; virtual; abstract;
    function EditorIsCode: boolean; virtual; abstract;
    function EditorIsDfm: boolean; virtual; abstract;
    function EditorGetCurrentUnit(var UnitName: string): boolean; virtual; abstract;
    function EditorGetCurrentProject(var ProjectName: string): boolean; virtual; abstract;
    function EditorIsTextSelected: boolean; virtual; abstract;
    function EditorGetColorScheme: TJeCodeColor; virtual; abstract;

    function ProjectClose: boolean; virtual; abstract;
    function ProjectOpen(ProjectName: string): boolean; virtual; abstract;
    function ProjectOpenFile(FileName: string): boolean; virtual; abstract;
    function ProjectSave: boolean; virtual; abstract;
    function ProjectSaveFile(FileName: string): boolean; virtual; abstract;
    function ProjectCloseFile(FileName: string): boolean; virtual; abstract;

    //Compiler related
    function GetBatchCompiler: string; virtual; abstract;
    function GetPrjExtension: string; virtual; abstract;
    function GetIncludePaths: string; virtual; abstract;
    function CompileProject(Project: string; LogResult: boolean = false): boolean; virtual; abstract;
    function IsSomethingModified: boolean; virtual; abstract;
    function GetAllOpenedFiles: string; virtual; abstract;

    //Form related
    function FormStartWorking: boolean; virtual; abstract;
    function FormEndWorking: boolean; virtual; abstract;
    function FormGetForm: TForm; virtual; abstract;
    function FormGetSelected(Index: Integer): TBaseCompo; virtual; abstract;
    function FormGetSelectedCount: Integer; virtual; abstract;
    function CompoAdd(Parent: TBaseCompo; CompoType: string; Left, Top, Width,
      Heigth: integer): TBaseCompo; virtual; abstract;

    //Palette and components related
    function PaletteGetTabs: string; virtual; abstract;
    procedure PaletteJumpToTab(TabName: string); virtual; abstract;
    function PaletteGetModules: string; virtual; abstract;
    function PaletteGetComponents(ModuleName: string): string; virtual; abstract;
    procedure SetToolBarOptions(Options: TPaletteOptions); virtual; abstract;

    function Title: string; virtual; abstract;
    function PackVersion: string; virtual; abstract;
    function RegistryPath: string; virtual; abstract;
    function ApplicationHandle: THandle; virtual; abstract;
    function PackPath: string; virtual; abstract;

    //Hint bar related
    procedure ShowHintBar; virtual; abstract;
    procedure HideHintBar; virtual; abstract;
    procedure SetHintBar(Value: string); virtual; abstract;

    //Layout related
    function GetMainFormLayout: TLayoutRecord; virtual; abstract;
    procedure SetMainFormLayout(Value: TLayoutRecord); virtual; abstract;

    function GetEditorLayout: TLayoutRecord; virtual; abstract;
    procedure SetEditorLayout(Value: TLayoutRecord); virtual; abstract;

    function GetInspectorLayout: TLayoutRecord; virtual; abstract;
    procedure SetInspectorLayout(Value: TLayoutRecord); virtual; abstract;
  end;

  //-----------------------------------------------------------------
  //Functions available in the dll
  //-----------------------------------------------------------------
  TDllFunctions = class
  public
    function Initialize(Options: TStream): boolean; virtual; abstract;
    function Finalize(var Options: TStream): boolean; virtual; abstract;
    function Version: double; virtual; abstract;

    procedure ExecuteItem(ItemName: string); virtual; abstract;
    procedure ExecuteButton(ButtonTag: integer); virtual; abstract;
    procedure DropDown(ButtonTag: integer; Point: TPoint); virtual; abstract;
    procedure PopupMenu(ButtonTag: integer; Point: TPoint); virtual; abstract;
    procedure CompilerNotification(Event: TNotificationType; FileName: string); virtual; abstract;

    //Editors
    function EditorMultiline(Value: string): string; virtual; abstract;
    function EditorTime(Value: TTime): TTime; virtual; abstract;
    function EditorDate(Value: TDate): TDate; virtual; abstract;
    function EditorDateTime(Value: TDateTime): TDateTime; virtual; abstract;
    function EditorFilters(Value: string): string; virtual; abstract;
    function EditorCursor(Value: TCursor): TCursor; virtual; abstract;
    function EditorSpeedButton(Value: TSpeedRecord): TSpeedRecord; virtual; abstract;
    function EditorBitBtn(Value: TBitBtnRecord): TBitBtnRecord; virtual; abstract;
  end;

  //-----------------------------------------------------------------
  //Functions exported in the dll
  //-----------------------------------------------------------------
  TExpertDllEntry = function(DelphiApis: TDelphiApis): TDllFunctions; stdcall;

implementation

end.
