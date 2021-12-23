object fmMacroLibrary: TfmMacroLibrary
  Left = 326
  Top = 252
  ActiveControl = lvMacros
  AutoScroll = False
  Caption = 'Keyboard Macro Library'
  ClientHeight = 218
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 0
    Top = 148
    Width = 314
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
  end
  object lvMacros: TListView
    Left = 0
    Top = 22
    Width = 314
    Height = 126
    Align = alClient
    Columns = <
      item
        Caption = 'Macro Name'
        MaxWidth = 1000
        MinWidth = 120
        Width = 165
      end
      item
        Caption = 'Recorded'
        MaxWidth = 145
        MinWidth = 145
        Width = 145
      end>
    ColumnClick = False
    HideSelection = False
    RowSelect = True
    ParentShowHint = False
    PopupMenu = mnuMacroPop
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvMacrosChange
    OnDblClick = lvMacrosDblClick
    OnEdited = lvMacrosEdited
    OnInfoTip = lvMacrosInfoTip
    OnKeyDown = lvMacrosKeyDown
    OnKeyPress = lvMacrosKeyPress
  end
  object Toolbar: TToolBar
    Left = 0
    Top = 0
    Width = 314
    Height = 22
    AutoSize = True
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Wrapable = False
    object tbnRecord: TToolButton
      Left = 0
      Top = 0
      Action = actRecord
      AllowAllUp = True
      Style = tbsCheck
    end
    object tbnPlayback: TToolButton
      Left = 23
      Top = 0
      Action = actPlayback
    end
    object tbnSep1: TToolButton
      Left = 46
      Top = 0
      Width = 8
      Caption = 'tbnSep1'
      ImageIndex = 71
      Style = tbsSeparator
    end
    object tbnClear: TToolButton
      Left = 54
      Top = 0
      Action = actEditClear
    end
    object tbnDelete: TToolButton
      Left = 77
      Top = 0
      Action = actEditDelete
    end
    object tbEdit: TToolButton
      Left = 100
      Top = 0
      Action = actEditMacro
    end
    object tbnSep2: TToolButton
      Left = 123
      Top = 0
      Width = 8
      Caption = 'tbnSep2'
      ImageIndex = 69
      Style = tbsSeparator
    end
    object tbnCopy: TToolButton
      Left = 131
      Top = 0
      Action = actEditCopy
    end
    object tbnSep3: TToolButton
      Left = 154
      Top = 0
      Width = 8
      Caption = 'tbnSep3'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object tbnSave: TToolButton
      Left = 162
      Top = 0
      Action = actFileSave
    end
    object tbnLoad: TToolButton
      Left = 185
      Top = 0
      Action = actFileLoad
    end
    object tbnSep4: TToolButton
      Left = 208
      Top = 0
      Width = 8
      Caption = 'tbnSep4'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object btnSuspend: TToolButton
      Left = 216
      Top = 0
      Action = actEditSuspend
      AllowAllUp = True
      Style = tbsCheck
    end
    object tbnSep5: TToolButton
      Left = 239
      Top = 0
      Width = 8
      Caption = 'tbnSep5'
      ImageIndex = 69
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 247
      Top = 0
      Action = actHelp
    end
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 152
    Width = 314
    Height = 66
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    object lblMacroDesc: TLabel
      Left = 0
      Top = 0
      Width = 314
      Height = 14
      Align = alTop
      Caption = '  Macro Description'
      FocusControl = mmoMacroDescription
    end
    object mmoMacroDescription: TMemo
      Left = 0
      Top = 14
      Width = 314
      Height = 52
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = mmoMacroDescriptionChange
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 72
    Top = 48
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy selected macro for playback'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
    object actEditClear: TAction
      Category = 'Edit'
      Caption = 'C&lear'
      Hint = 'Clear macro library entries...'
      ImageIndex = 44
      ShortCut = 16460
      OnExecute = actEditClearExecute
    end
    object actViewToolbar: TAction
      Category = 'View'
      Caption = 'Show Toolbar'
      Hint = 'Show toolbar'
      OnExecute = actViewToolbarExecute
    end
    object actEditDelete: TAction
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete selected macro'
      ImageIndex = 11
      ShortCut = 16430
      OnExecute = actEditDeleteExecute
    end
    object actViewDescription: TAction
      Category = 'View'
      Caption = 'Show Description'
      Hint = 'Show description'
      OnExecute = actViewDescriptionExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = 'Save Macro...'
      Hint = 'Save macro...'
      ImageIndex = 31
      ShortCut = 16467
      OnExecute = actFileSaveExecute
    end
    object actFileLoad: TAction
      Category = 'File'
      Caption = 'Load Macro...'
      Hint = 'Load macro...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = actFileLoadExecute
    end
    object actEditSuspend: TAction
      Category = 'Edit'
      Caption = '&Suspend'
      Hint = 'Suspend adding macros to library'
      ImageIndex = 68
      OnExecute = actEditSuspendExecute
    end
    object actRecord: TAction
      Category = 'Edit'
      Caption = '&Record macro'
      Hint = 'Record new macro'
      ImageIndex = 32
      OnExecute = actRecordExecute
    end
    object actPlayback: TAction
      Category = 'Edit'
      Caption = '&Playback macro'
      Hint = 'Playback selected macro'
      ImageIndex = 43
      OnExecute = actPlaybackExecute
    end
    object actHelp: TAction
      Caption = 'Help'
      Hint = 'Help'
      ImageIndex = 0
      OnExecute = actHelpExecute
    end
    object actEditRename: TAction
      Category = 'Edit'
      Caption = 'Rename'
      Hint = 'Rename macro'
      ImageIndex = 38
      ShortCut = 113
      OnExecute = actEditRenameExecute
    end
    object actPromptForName: TAction
      Category = 'View'
      Caption = 'Auto-Prompt for Name'
      Hint = 'Automatically prompt for a name when stopping recording'
      OnExecute = actPromptForNameExecute
    end
    object actEditMacro: TAction
      Category = 'Edit'
      Caption = 'Edit'
      Hint = 'Edit'
      ImageIndex = 38
      ShortCut = 8305
      OnExecute = actEditMacroExecute
    end
    object actFileClose: TAction
      Category = 'File'
      Caption = 'Close'
      ShortCut = 27
      OnExecute = actFileCloseExecute
    end
  end
  object mnuMacroPop: TPopupMenu
    Left = 72
    Top = 96
    object mitClear: TMenuItem
      Action = actEditClear
    end
    object mitCopy: TMenuItem
      Action = actEditCopy
    end
    object mitDelete: TMenuItem
      Action = actEditDelete
    end
    object miShowContent: TMenuItem
      Action = actEditMacro
    end
    object mitRename: TMenuItem
      Action = actEditRename
    end
    object mitSep1: TMenuItem
      Caption = '-'
    end
    object mitLoadmacro: TMenuItem
      Action = actFileLoad
    end
    object mitSavemacro: TMenuItem
      Action = actFileSave
    end
    object mitSep2: TMenuItem
      Caption = '-'
    end
    object mitSuspend: TMenuItem
      Action = actEditSuspend
      RadioItem = True
    end
    object mitSep3: TMenuItem
      Caption = '-'
    end
    object mitShowToolbar: TMenuItem
      Action = actViewToolbar
    end
    object mitShowDescription: TMenuItem
      Action = actViewDescription
    end
    object mitSep5: TMenuItem
      Caption = '-'
    end
    object mitPromptforName: TMenuItem
      Action = actPromptForName
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'bkm'
    Filter = 'GExperts Editor Macros (*.gxm)|*.gxm|Any File (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Load Keyboard Macro'
    Left = 136
    Top = 48
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'gxm'
    Filter = 'GExperts Editor Macros (*.gxm)|*.gxm|Any File (*.*)|*.*'
    Title = 'Save Keyboard Macro'
    Left = 136
    Top = 96
  end
end
