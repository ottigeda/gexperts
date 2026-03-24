object fmVerticalTabs: TfmVerticalTabs
  Left = 268
  Top = 191
  AutoScroll = False
  Caption = 'Vertical Tabs'
  ClientHeight = 400
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  PopupMenu = Popup
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object lbFiles: TListBox
    Left = 0
    Top = 22
    Width = 200
    Height = 359
    Align = alClient
    ItemHeight = 14
    PopupMenu = Popup
    TabOrder = 0
    OnClick = lbFilesClick
    OnMouseMove = lbFilesMouseMove
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 200
    Height = 22
    AutoSize = True
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object tbnRefresh: TToolButton
      Left = 0
      Top = 0
      Action = actRefresh
    end
    object tbnSep1: TToolButton
      Left = 23
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnSortAlpha: TToolButton
      Left = 31
      Top = 0
      Action = actSortAlpha
      Style = tbsCheck
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 381
    Width = 200
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object Popup: TPopupMenu
    Images = dmSharedImages.Images
    Left = 80
    Top = 120
    object mitGotoFile: TMenuItem
      Action = actGotoFile
      Default = True
    end
    object mitSep1: TMenuItem
      Caption = '-'
    end
    object mitCloseFile: TMenuItem
      Action = actCloseFile
    end
    object mitCloseAllOther: TMenuItem
      Action = actCloseAllOther
    end
    object mitSep2: TMenuItem
      Caption = '-'
    end
    object mitCopyPath: TMenuItem
      Action = actCopyPath
    end
    object mitOpenFolder: TMenuItem
      Action = actOpenFolder
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    Left = 80
    Top = 180
    object actRefresh: TAction
      Caption = '&Refresh'
      Hint = 'Refresh file list'
      ImageIndex = 39
      ShortCut = 116
      OnExecute = actRefreshExecute
    end
    object actSortAlpha: TAction
      Caption = 'Sort &Alphabetically'
      Hint = 'Toggle alphabetical sorting'
      ImageIndex = 93
      OnExecute = actSortAlphaExecute
    end
    object actGotoFile: TAction
      Caption = '&Go to File'
      Hint = 'Switch to selected file'
      OnExecute = actGotoFileExecute
    end
    object actCloseFile: TAction
      Caption = '&Close File'
      Hint = 'Close selected file'
      OnExecute = actCloseFileExecute
    end
    object actCloseAllOther: TAction
      Caption = 'Close &All Other Files'
      Hint = 'Close all files except selected'
      OnExecute = actCloseAllOtherExecute
    end
    object actCopyPath: TAction
      Caption = 'Copy &Path'
      Hint = 'Copy full path to clipboard'
      OnExecute = actCopyPathExecute
    end
    object actOpenFolder: TAction
      Caption = 'Open Containing &Folder'
      Hint = 'Open containing folder in Explorer'
      OnExecute = actOpenFolderExecute
    end
  end
end
