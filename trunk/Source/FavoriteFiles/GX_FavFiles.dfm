object fmFavFiles: TfmFavFiles
  Left = 402
  Top = 273
  AutoScroll = False
  Caption = 'Favorite Files'
  ClientHeight = 342
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 14
  object splTreeView: TSplitter
    Left = 121
    Top = 22
    Width = 3
    Height = 301
    Cursor = crHSplit
  end
  object tvFolders: TTreeView
    Left = 0
    Top = 22
    Width = 121
    Height = 301
    Align = alLeft
    DragMode = dmAutomatic
    HideSelection = False
    Images = ilFolders
    Indent = 19
    PopupMenu = pmuFolders
    SortType = stText
    TabOrder = 0
    ToolTips = False
    OnChange = tvFoldersChange
    OnChanging = tvFoldersChanging
    OnDragDrop = tvFoldersDragDrop
    OnDragOver = tvFoldersDragOver
    OnEdited = tvFoldersEdited
    OnEndDrag = tvFoldersEndDrag
    OnKeyUp = tvFoldersKeyUp
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 323
    Width = 495
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object pnlFiles: TPanel
    Left = 124
    Top = 22
    Width = 371
    Height = 301
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object splFileView: TSplitter
      Left = 0
      Top = 151
      Width = 371
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object ListView: TListView
      Left = 0
      Top = 0
      Width = 371
      Height = 151
      Align = alClient
      Columns = <
        item
          Caption = 'File'
          Width = -1
          WidthType = (
            -1)
        end
        item
          Caption = 'File Name'
        end
        item
          Caption = 'Description'
          Width = 400
        end
        item
          Caption = 'Execute'
        end>
      ColumnClick = False
      DragMode = dmAutomatic
      HideSelection = False
      IconOptions.AutoArrange = True
      LargeImages = ilSysLarge
      MultiSelect = True
      RowSelect = True
      PopupMenu = pmuFiles
      SmallImages = ilSystem
      SortType = stText
      TabOrder = 0
      OnChange = ListViewChange
      OnDblClick = actFileExecuteExecute
      OnEdited = ListViewEdited
      OnDragOver = ListViewDragOver
    end
    object pnlFileView: TPanel
      Left = 0
      Top = 154
      Width = 371
      Height = 147
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'This editor is created at runtime'
      ParentColor = True
      TabOrder = 1
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 495
    Height = 22
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object tbnFileNewFile: TToolButton
      Left = 0
      Top = 0
      Action = actFileNewFile
    end
    object tbnFileNewFolder: TToolButton
      Left = 23
      Top = 0
      Action = actFileNewFolder
    end
    object tbnFileDelete: TToolButton
      Left = 46
      Top = 0
      Action = actFileDelete
    end
    object tbnSep1: TToolButton
      Left = 69
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbnNavLevelUp: TToolButton
      Left = 77
      Top = 0
      Action = actNavLevelUp
    end
    object tbnSep2: TToolButton
      Left = 100
      Top = 0
      Width = 8
      ImageIndex = 3
      Style = tbsSeparator
    end
    object tbnFileProperties: TToolButton
      Left = 108
      Top = 0
      Action = actFileProperties
    end
    object tbnSep3: TToolButton
      Left = 131
      Top = 0
      Width = 8
      ImageIndex = 4
      Style = tbsSeparator
    end
    object tbnNavExpand: TToolButton
      Left = 139
      Top = 0
      Action = actNavExpand
    end
    object tbnNavContract: TToolButton
      Left = 162
      Top = 0
      Action = actNavContract
    end
    object tbnSep4: TToolButton
      Left = 185
      Top = 0
      Width = 8
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbnHelpHelp: TToolButton
      Left = 193
      Top = 0
      Action = actHelpHelp
    end
  end
  object MainMenu: TMainMenu
    Images = dmSharedImages.Images
    Left = 16
    Top = 32
    object mitFile: TMenuItem
      Caption = '&File'
      object mitNewFile: TMenuItem
        Action = actFileNewFile
      end
      object mitNewFolder: TMenuItem
        Action = actFileNewFolder
      end
      object mitFileDelete: TMenuItem
        Action = actFileDelete
      end
      object mitFileSep1: TMenuItem
        Caption = '-'
      end
      object mitFileProperties: TMenuItem
        Action = actFileProperties
      end
      object mitFileSep2: TMenuItem
        Caption = '-'
      end
      object mitFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object mitOptions: TMenuItem
      Caption = '&Options'
      object mitOptionsOptions: TMenuItem
        Action = actOptionsOptions
      end
    end
    object mitHelp: TMenuItem
      Caption = '&Help'
      object mitHelpHelp: TMenuItem
        Action = actHelpHelp
      end
      object mitHelpContents: TMenuItem
        Action = actHelpContents
      end
      object mitHelpSep1: TMenuItem
        Caption = '-'
      end
      object mitHelpAbout: TMenuItem
        Action = actHelpAbout
      end
    end
  end
  object ilFolders: TImageList
    Left = 16
    Top = 80
    Bitmap = {
      494C01010A000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001001000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001863104200000000
      0000000000000000000000000000000000000000000000001863104200000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018630000000000000000
      0000000000000000000000000000000000000000000018630000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010420000E07F00000000
      0000000000000000104210421042104200000000000010420000E07F00000000
      0000000000000000104210421042000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001F000040000000000000E07F0000
      000000000000000000001863E07F104200001F00004000000000000018630000
      00000000000000000000E07F1042000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001F000040000000000000E07F
      E07FE07F000000000000E07F18631042000000001F000040000000000000E07F
      E07FE07F00000000000018630000104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000010421F000000000000000000
      000000001042000000001863E07F10420000000010421F000000000000000000
      0000000010420000000010420000104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7F1F0000400000E07F
      E07FE07F000000000040E07F1863104200001042FF7F18631F00004000001863
      E07FE07F00000000004000001042104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7FE07F1F0000400000
      0000000000401863E07F1863E07F104200001042FF7FFF7FFF7F1F0000400000
      000000000040FF7F104200001863104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7F1863E07F1F000000
      000000401863E07F1863E07F186310420000104210421042104210421F000000
      000000401042104210421042E07F104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1042000000001042FF7FE07F1863E07F1863
      E07F1863FF7FFF7FFF7FFF7FFF7F104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000010421863E07F1863E07F1863
      E07F1863104210421042104210421042000000001042FF7F1863E07F1863E07F
      1863FF7F10421042104210421042104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010421863E07F1863E07F
      186310420000000000000000000000000000000000001042FF7FFF7FFF7FFF7F
      FF7F104200000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042104210421042
      1042000000000000000000000000000000000000000000001042104210421042
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7F1042104210421042
      1042104210420000000000000000000000000000FF7F10421042104210421042
      1042104200000000000000000000000000000000104210421042104210421042
      1042104210421042104210421042104200000000000010421042104210421042
      10421042104210421042104210420000000000000000FF7F186318631863007C
      1863186310420000000000000000000000000000FF7F186318631863007C1863
      18631042000000000000000000000000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F10420000000000001042FF7FE07F1863E07F
      1863E07F1863E07F1863E07F10420000000000000000FF7F18631863007C007C
      1863186310420000000000000000000000000000FF7F18631863007C007C1863
      186310420000000000000000000000000000000010420000000000000000E07F
      1863E07F1863E07F1863E07F1863104200000000104200000000000000001863
      E07F1863E07F1863E07F186300001042000000000000FF7F1863007C007C007C
      007C007C10420000104210421042104200000000FF7F1863007C007C007C007C
      007C10420000104210421042104200000000000010420000E07F000000000000
      00000000E07F1863E07F1863E07F10420000000010420000E07F000000000000
      000000001863E07F1863104200001042000000000000FF7F1863007C007C007C
      007C007C10420000E07F1863E07F104200000000FF7F1863007C007C007C007C
      007C10420000E07F1863E07F10420000000000001042FF7F0000E07F00000000
      000000000000E07F1863E07F1863104200001042FF7F18630000E07F00000000
      0000000000001863E07F000010421042000000000000FF7F18631863007C007C
      18631863104200001863E07F1863104200000000FF7F18631863007C007C1863
      1863104200001863E07F186300001042000000001042FF7FE07F0000E07F0000
      0000000000000000E07F1863E07F104200001042FF7FFF7FFF7F0000E07F0000
      00000000000000001042000018631042000000000000FF7F186318631863007C
      1863186310420000E07F1863E07F104200000000FF7F186318631863007C1863
      186310420000E07F1863104200001042000000001042FF7F18630000E07FE07F
      00000000000000001863E07F18631042000010421042104210420000E07FE07F
      000000000000000010421042E07F1042000000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7F104200001863E07F1863104200000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F104200001863E07F000010421042000000001042FF7FE07F18630000E07F
      E07F00000000000000001863E07F1042000000001042FF7F1863E07F0000E07F
      E07F0000000000000000E07F1863104200000000000000000000000000000000
      0000000000000000E07F1863E07F104200000000000000000000000000000000
      000000000000FF7F1042000018631042000000001042FF7FFF7FFF7FFF7F0000
      0000E07F0000000000000000FF7F1042000000001042FF7FE07F1863E07F0000
      0000E07F0000000000000000FF7F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200001042104210421042104210421042
      104210421042104210421042E07F10420000000010421863E07F1863E07F1863
      E07F0000000000001863104200000000000000001042FF7F1863E07F1863E07F
      18630000000000001863104200000000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200000000000010421863E07F1863E07F
      186310420000000018630000000000000000000000001042FF7FFF7FFF7FFF7F
      FF7F1042000000001863000000000000000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1042000000001042FF7FE07F1863E07F1863
      E07F1863FF7FFF7FFF7FFF7FFF7F104200000000000000001042104210421042
      104200000000000000000000007C004000000000000000001042104210421042
      104200000000000000000000007C00400000000010421863E07F1863E07F1863
      E07F1863104210421042104210421042000000001042FF7F1863E07F1863E07F
      1863FF7F10421042104210421042104200000000000000000000000000000000
      0000000000000000000000000040004000400000000000000000000000000000
      0000000000000000000000000040004000400000000010421863E07F1863E07F
      186310420000000000000000000000000000000000001042FF7FFF7FFF7FFF7F
      FF7F104200000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000040007C0000000000000000000000000000
      00000000000000000000000000000040007C0000000000001042104210421042
      1042000000000000000000000000000000000000000000001042104210421042
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07FFF7FE07FFF7FE07F
      FF7FE07FFF7FE07F0000000000000000000000000000E07FFF7FE07FFF7FE07F
      FF7FE07FFF7FE07F000000000000000000000000104210421042104210421042
      1042104210421042104210421042104200000000000010421042104210421042
      104210421042104210421042104200000000000000000000E07F10421042E07F
      10421042E07F000000000000000000000000000000000000E07F10421042E07F
      10421042E07F00000000000000000000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F10420000000000001042FF7FE07F1863E07F
      1863E07F1863E07F1863E07F104200000000000000000000FF7F10421042FF7F
      10421042FF7F000000000000000000000000000000000000FF7F10421042FF7F
      10421042FF7F00000000000000000000000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F18631042000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863000010420000000010420000E07F10421042E07F
      10421042E07F000010421042104210420000000010420000E07F10421042E07F
      10421042E07F00001042104210420000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F186310420000104200000000104200001042104210421042
      1042104210420000E07F1863E07F104200000000104200001042104210421042
      10421042104200001863E07F10420000000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200001042FF7F1863E07F1863E07F1863
      E07F1863E07F1863E07F000010421042000000000000FF7FFF7FE07FE07FE07F
      E07FE07FE07FE07F0000E07F18631042000000000000FF7FFF7FE07FE07FE07F
      E07FE07FE07FE07F0000186300001042000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F104200001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F104200001863104200000000104200000000FF7FFF7FE07F
      E07FE07F00000000E07F1863E07F104200000000104200000000FF7FFF7FE07F
      E07FE07F000000001863104200001042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200001042104210421042104210421042
      104210421042104210421042E07F1042000000001042FF7F186300000000FF7F
      000000001042E07F1042E07F18631042000010421042FF7F186300000000FF7F
      0000000010421863E07F000010421042000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F18631042000000001042FF7FE07F1863E07F0000
      E07F1042E07F1042E07F1863E07F1042000010421042FF7FFF7FFF7FFF7F0000
      FF7FFF7FFF7FFF7F1042000018631042000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1042000000001042FF7FE07F1863E07F1863
      E07F1863FF7FFF7FFF7FFF7FFF7F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200001042104210421042104210421042
      104210421042104210421042E07F10420000000010421863E07F1863E07F1863
      E07F1863104210421042104210421042000000001042FF7F1863E07F1863E07F
      1863FF7F104210421042104210421042000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200000000000010421863E07F1863E07F
      186310420000000000000000000000000000000000001042FF7FFF7FFF7FFF7F
      FF7F1042000000000000000000000000000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1042000000001042FF7FE07F1863E07F1863
      E07F1863FF7FFF7FFF7FFF7FFF7F104200000000000000001042104210421042
      1042000000000000000000000000000000000000000000001042104210421042
      104200000000000000000000000000000000000010421863E07F1863E07F1863
      E07F1863104210421042104210421042000000001042FF7F1863E07F1863E07F
      1863FF7F10421042104210421042104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010421863E07F1863E07F
      186310420000000000000000000000000000000000001042FF7FFF7FFF7FFF7F
      FF7F104200000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042104210421042
      1042000000000000000000000000000000000000000000001042104210421042
      104200000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000F9FFF9FF00000000E0FFE0FF00000000
      807F807F00000000000000000000000000000000000000000000000000000000
      8000800000000000800080000000000080000000000000008000000000000000
      8000000000000000800080000000000080008000000000008001800100000000
      C07FC07F00000000E0FFE0FF00000000FFFFFFFF801F003FC000E000801F003F
      8000C000801F003F8000C0008000000084008400800000008080808080000000
      84400440800000008200020080000000818001808000000080C080C080000000
      80508050800000008001800180008000C040C04080008000E0E0E0E080018001
      FFF0FFF0C07FC07FFFF8FFF8E0FFE0FFFFFFFFFF800F800FC000E000800F800F
      8000C000C01FC01F8000C000C000C00080008000800080008000800080008000
      8000000080008000800000008000800080000000800000008000800080000000
      80008000800000008001800180008000C07FC07F80008000E0FFE0FF80018001
      FFFFFFFFC07FC07FFFFFFFFFE0FFE0FF00000000000000000000000000000000
      000000000000}
  end
  object dlgGetFiles: TOpenDialog
    FilterIndex = 0
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Select File'
    Left = 16
    Top = 176
  end
  object pmuFolders: TPopupMenu
    Images = dmSharedImages.Images
    Left = 16
    Top = 128
    object mitTreeNewFolder: TMenuItem
      Action = actFileNewFolder
    end
    object mitTreeDeleteFolder: TMenuItem
      Action = actFileDelete
    end
    object mitTreeSep1: TMenuItem
      Caption = '-'
    end
    object mitMoveUp: TMenuItem
      Action = actFileMoveUp
    end
    object mitMoveDown: TMenuItem
      Action = actFileMoveDown
    end
    object mitTreeSep2: TMenuItem
      Caption = '-'
    end
    object mitTreeProperties: TMenuItem
      Action = actFileProperties
    end
  end
  object pmuFiles: TPopupMenu
    Images = dmSharedImages.Images
    Left = 144
    Top = 80
    object mitFExecute: TMenuItem
      Action = actFileExecute
      Default = True
    end
    object mitCSep0: TMenuItem
      Caption = '-'
    end
    object mitFNewFile: TMenuItem
      Action = actFileNewFile
    end
    object mitFDelete: TMenuItem
      Action = actFileDelete
    end
    object mitFRename: TMenuItem
      Action = actFileRename
    end
    object mitCSep1: TMenuItem
      Caption = '-'
    end
    object mitFView: TMenuItem
      Caption = '&View'
      OnClick = mitFViewClick
      object mitViewLarge: TMenuItem
        Action = actViewLargeIcons
        RadioItem = True
      end
      object mitViewSmall: TMenuItem
        Tag = 1
        Action = actViewSmallIcons
        RadioItem = True
      end
      object mitViewList: TMenuItem
        Tag = 2
        Action = actViewList
        RadioItem = True
      end
      object mitViewDetails: TMenuItem
        Tag = 3
        Action = actViewDetails
        RadioItem = True
      end
    end
    object mitCSep2: TMenuItem
      Caption = '-'
    end
    object mitSelectAll: TMenuItem
      Action = actFileSelectAll
    end
    object mitCSep3: TMenuItem
      Caption = '-'
    end
    object mitFProperties: TMenuItem
      Action = actFileProperties
    end
  end
  object ilSystem: TImageList
    ShareImages = True
    Left = 144
    Top = 32
  end
  object ilSysLarge: TImageList
    Height = 32
    ShareImages = True
    Width = 32
    Left = 208
    Top = 32
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 72
    Top = 32
    object actOptionsOptions: TAction
      Category = 'Options'
      Caption = '&Options...'
      Hint = 'Options...'
      ImageIndex = 17
      ShortCut = 16463
      OnExecute = actOptionsOptionsExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit'
      ImageIndex = 8
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actFileProperties: TAction
      Category = 'File'
      Caption = '&Properties...'
      Hint = 'Properties...'
      ImageIndex = 35
      ShortCut = 32781
      OnExecute = actFilePropertiesExecute
    end
    object actFileDelete: TAction
      Category = 'File'
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 11
      ShortCut = 46
      OnExecute = actFileDeleteExecute
    end
    object actFileNewFile: TAction
      Category = 'File'
      Caption = 'New F&ile...'
      Hint = 'New file...'
      ImageIndex = 10
      ShortCut = 45
      OnExecute = actFileNewFileExecute
    end
    object actFileNewFolder: TAction
      Category = 'File'
      Caption = 'New F&older...'
      Hint = 'New folder'
      ImageIndex = 9
      ShortCut = 16460
      OnExecute = actFileNewFolderExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 0
      ShortCut = 112
      OnExecute = actHelpHelpExecute
    end
    object actHelpContents: TAction
      Category = 'Help'
      Caption = '&Contents'
      Hint = 'Contents'
      OnExecute = actHelpContentsExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      Hint = 'About...'
      ImageIndex = 16
      OnExecute = actHelpAboutExecute
    end
    object actNavExpand: TAction
      Category = 'Navigation'
      Caption = 'Expand All'
      Hint = 'Expand all'
      ImageIndex = 12
      OnExecute = actNavExpandExecute
    end
    object actNavContract: TAction
      Category = 'Navigation'
      Caption = 'Contract All'
      Hint = 'Contract all'
      ImageIndex = 13
      OnExecute = actNavContractExecute
    end
    object actNavLevelUp: TAction
      Category = 'Navigation'
      Caption = 'Level Up'
      Hint = 'Level up'
      ImageIndex = 37
      OnExecute = actNavLevelUpExecute
    end
    object actViewLargeIcons: TAction
      Category = 'View'
      Caption = '&Large Icons'
      Hint = 'Large icons'
      OnExecute = actViewLargeIconsExecute
    end
    object actViewSmallIcons: TAction
      Category = 'View'
      Caption = '&Small Icons'
      Hint = 'Small icons'
      OnExecute = actViewSmallIconsExecute
    end
    object actViewList: TAction
      Category = 'View'
      Caption = '&List'
      Hint = 'List view'
      OnExecute = actViewListExecute
    end
    object actViewDetails: TAction
      Category = 'View'
      Caption = '&Details'
      Hint = 'Details view'
      OnExecute = actViewDetailsExecute
    end
    object actFileExecute: TAction
      Category = 'Execute'
      Caption = '&Execute'
      Hint = 'Execute file'
      ImageIndex = 36
      ShortCut = 13
      OnExecute = actFileExecuteExecute
    end
    object actFileRename: TAction
      Category = 'File'
      Caption = 'Rename'
      ImageIndex = 38
      ShortCut = 113
      OnExecute = actFileRenameExecute
    end
    object actFileSelectAll: TAction
      Category = 'File'
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = actFileSelectAllExecute
    end
    object actFileMoveUp: TAction
      Category = 'File'
      Caption = 'Move Up'
      ShortCut = 16422
      OnExecute = actFileMoveUpExecute
    end
    object actFileMoveDown: TAction
      Category = 'File'
      Caption = 'Move Down'
      ShortCut = 16424
      OnExecute = actFileMoveDownExecute
    end
  end
end
