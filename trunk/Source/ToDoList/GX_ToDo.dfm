object fmToDo: TfmToDo
  Left = 268
  Top = 191
  ActiveControl = edtFilterTodoList
  AutoScroll = False
  Caption = 'To Do List'
  ClientHeight = 229
  ClientWidth = 665
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object StatusBar: TStatusBar
    Left = 0
    Top = 210
    Width = 665
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object lvTodo: TListView
    Left = 0
    Top = 22
    Width = 665
    Height = 188
    Align = alClient
    Columns = <
      item
        Caption = '!'
        Width = 20
      end
      item
        Caption = 'Category'
        Width = 80
      end
      item
        Caption = 'Owner'
        Width = 70
      end
      item
        Caption = 'Description'
        Width = 270
      end
      item
        Caption = 'File'
        Width = 170
      end
      item
        Caption = 'Line'
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = Popup
    SmallImages = dmSharedImages.Images
    SortType = stData
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = lvTodoChange
    OnColumnClick = lvTodoColumnClick
    OnCompare = lvTodoCompare
    OnCustomDrawItem = lvTodoCustomDrawItem
    OnDblClick = actEditGotoExecute
    OnEditing = lvTodoEditing
    OnKeyDown = FormKeyDown
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 665
    Height = 22
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Wrapable = False
    object tbnRefresh: TToolButton
      Left = 0
      Top = 0
      Action = actFileRefresh
    end
    object tbnSep1: TToolButton
      Left = 23
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnGoto: TToolButton
      Left = 31
      Top = 0
      Action = actEditGoto
    end
    object tbnSep2: TToolButton
      Left = 54
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbnPrint: TToolButton
      Left = 62
      Top = 0
      Action = actFilePrint
    end
    object tbnSep3: TToolButton
      Left = 85
      Top = 0
      Width = 8
      ImageIndex = 3
      Style = tbsSeparator
    end
    object tbnConfigure: TToolButton
      Left = 93
      Top = 0
      Action = actOptionsConfigure
    end
    object tbnSep4: TToolButton
      Left = 116
      Top = 0
      Width = 8
      ImageIndex = 4
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 124
      Top = 0
      Action = actHelpHelp
    end
    object tbnSep5: TToolButton
      Left = 147
      Top = 0
      Width = 8
      Caption = 'tbnSep5'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object edtFilterTodoList: TEdit
      Left = 155
      Top = 0
      Width = 121
      Height = 22
      Hint = 'Enter a filter text and then press the Return/Enter key.'
      Margins.Left = 0
      Margins.Top = 2
      Margins.Right = 0
      Margins.Bottom = 2
      Constraints.MinHeight = 22
      TabOrder = 0
      OnChange = edtFilterTodoListChange
      OnKeyDown = edtFilterTodoListKeyDown
      OnKeyPress = edtFilterTodoListKeyPress
    end
  end
  object Popup: TPopupMenu
    Images = dmSharedImages.Images
    Left = 72
    Top = 56
    object mitGoto: TMenuItem
      Action = actEditGoto
      Default = True
    end
    object mitRefresh: TMenuItem
      Action = actFileRefresh
    end
    object mitSep1: TMenuItem
      Caption = '-'
    end
    object mitCopyToClipboard: TMenuItem
      Action = actEditCopy
    end
    object mitSep2: TMenuItem
      Caption = '-'
    end
    object mitPrint: TMenuItem
      Action = actFilePrint
    end
    object mitConfigure: TMenuItem
      Action = actOptionsConfigure
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    Left = 72
    Top = 112
    object actFileRefresh: TAction
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Refresh to do items'
      ImageIndex = 39
      ShortCut = 116
      OnExecute = actFileRefreshExecute
    end
    object actEditGoto: TAction
      Category = 'Edit'
      Caption = '&Goto'
      Hint = 'Goto source line'
      ImageIndex = 27
      ShortCut = 13
      OnExecute = actEditGotoExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print to do items'
      ImageIndex = 3
      ShortCut = 16464
      OnExecute = actFilePrintExecute
      OnUpdate = actFilePrintUpdate
    end
    object actOptionsConfigure: TAction
      Category = 'Options'
      Caption = '&Options...'
      Hint = 'Options...'
      ImageIndex = 17
      OnExecute = actOptionsConfigureExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 0
      OnExecute = actHelpHelpExecute
    end
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Copy All'
      Hint = 'Copy all'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
  end
  object tim_Filter: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tim_FilterTimer
    Left = 328
    Top = 120
  end
end
