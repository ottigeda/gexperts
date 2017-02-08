object foLexical: TfoLexical
  Left = 381
  Top = 264
  Caption = 'Methods lexical'
  ClientHeight = 328
  ClientWidth = 592
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 575
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 22
    Height = 306
  end
  object Panel1: TPanel
    Left = 0
    Top = 22
    Width = 185
    Height = 306
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object TreeView1: TTreeView
      Left = 0
      Top = 0
      Width = 185
      Height = 306
      Align = alClient
      HideSelection = False
      HotTrack = True
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnChange = TreeView1Change
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 328
    Width = 592
    Height = 0
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 188
    Top = 22
    Width = 404
    Height = 306
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 2
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 404
      Height = 281
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel4'
      TabOrder = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 404
        Height = 281
        Align = alClient
        HideSelection = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        WantReturns = False
        OnChange = Memo1Change
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 281
      Width = 404
      Height = 25
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'Panel5'
      TabOrder = 1
      object JvEdit1: TJvEdit
        Left = 0
        Top = 0
        Width = 404
        Height = 25
        Align = alClient
        HideSelection = False
        ReadOnly = True
        TabOrder = 0
        OnChange = JvEdit1Change
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 592
    Height = 22
    AutoSize = True
    Caption = 'ToolBar1'
    Images = ImageList1
    TabOrder = 3
    object ToolButton6: TToolButton
      Left = 0
      Top = 0
      Action = Quit
    end
    object ToolButton7: TToolButton
      Left = 23
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButton1: TToolButton
      Left = 31
      Top = 0
      Action = Search
    end
    object ToolButton2: TToolButton
      Left = 54
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 62
      Top = 0
      Action = CopyDeclaration
    end
    object ToolButton4: TToolButton
      Left = 85
      Top = 0
      Action = CopyHelp
    end
    object ToolButton5: TToolButton
      Left = 108
      Top = 0
      Action = CopyName
    end
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 48
    Top = 66
    object File1: TMenuItem
      Caption = '&File'
      object Search1: TMenuItem
        Action = Search
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Quit1: TMenuItem
        Action = Quit
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Copyfunctiondeclaration1: TMenuItem
        Action = CopyDeclaration
      end
      object Copyfunctionname1: TMenuItem
        Action = CopyName
      end
      object Copyfunctionhelp1: TMenuItem
        Action = CopyHelp
      end
    end
  end
  object FindDialog1: TFindDialog
    Options = [frDown, frHideMatchCase, frHideWholeWord, frHideUpDown, frMatchCase, frDisableMatchCase, frDisableUpDown, frDisableWholeWord]
    OnFind = FindDialog1Find
    Left = 78
    Top = 66
  end
  object ActionList1: TActionList
    Left = 70
    Top = 121
    object Search: TAction
      Caption = '&Search...'
      Hint = 'Search'
      ImageIndex = 1
      ShortCut = 16454
      OnExecute = SearchExecute
    end
    object Quit: TAction
      Caption = '&Quit'
      Hint = 'Quit'
      ImageIndex = 0
      ShortCut = 32883
      OnExecute = QuitExecute
    end
    object CopyDeclaration: TAction
      Caption = 'Copy Function &Declaration'
      Hint = 'Copy Function Declaration'
      ImageIndex = 4
      ShortCut = 16433
      OnExecute = CopyDeclarationExecute
    end
    object CopyName: TAction
      Caption = 'Copy Function &Name'
      Hint = 'Copy Function Name'
      ImageIndex = 2
      ShortCut = 16434
      OnExecute = CopyNameExecute
    end
    object CopyHelp: TAction
      Caption = 'Copy Function &Help'
      Hint = 'Copy Function Help'
      ImageIndex = 3
      ShortCut = 16435
      OnExecute = CopyHelpExecute
    end
  end
  object ImageList1: TImageList
    Left = 40
    Top = 121
    Bitmap = {
      494C0101050009000C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B0000000000000000007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FF00
      0000FF000000FF000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00007B7B7B007B7B7B0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF0000000000000000000000
      00007B7B7B0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF0000000000
      0000FFFFFF000000000000000000FF000000FFFFFF0000000000000000000000
      00000000FF0000000000000000007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF0000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF0000000000
      00000000000000000000FFFFFF00FF000000FFFFFF00000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF0000000000
      0000FFFFFF0000000000FF000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FF00
      0000FF000000FF000000FFFFFF00FFFFFF000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000BDBD
      BD00FFFFFF0000000000FFFFFF000000000000000000000000007B7B7B000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
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
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF000000000000000000000000000000FF000000
      FF0000000000000000007B7B7B00000000007B7B7B00000000000000FF000000
      FF000000FF00000000000000000000000000FFFF0000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF0000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000000000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B0000000000000000007B7B7B00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B0000000000000000007B7B7B0000000000000000000000FF00000000000000
      FF000000FF000000FF007B7B7B00000000007B7B7B0000000000000000000000
      00000000FF000000FF000000000000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF0000000000000000000000
      00007B7B7B007B7B7B0000FFFF000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF0000000000000000000000
      00007B7B7B007B7B7B0000FFFF00000000000000FF000000FF00000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF0000000000000000000000
      00007B7B7B0000FFFF0000FFFF000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF0000000000000000000000
      00007B7B7B0000FFFF0000FFFF00000000000000FF000000FF00000000000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF0000000000000000000000FF000000FF0000000000FFFFFF00FF0000000000
      0000FFFFFF00FF00000000000000FF000000FFFFFF0000000000000000000000
      00000000FF0000000000000000007B7B7B0000000000FFFFFF00FF0000000000
      0000FFFFFF000000000000000000FF000000FFFFFF0000000000000000000000
      00000000FF0000000000000000007B7B7B000000FF000000FF00000000000000
      000000000000000000007B7B7B00000000007B7B7B0000000000000000000000
      0000000000000000FF000000FF0000000000FFFF00000000000000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF0000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FF000000FFFFFF00FF000000FFFFFF0000000000000000000000
      FF000000FF000000FF00000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF0000000000000000000000
      FF000000FF000000FF0000000000000000000000FF000000FF00000000000000
      000000000000000000000000840000000000000084000000FF00000000000000
      0000000000000000FF000000FF000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF0000000000
      0000FF00000000000000FFFFFF00FF000000FFFFFF00000000000000FF000000
      FF000000FF000000FF000000FF000000000000000000FFFFFF00FF000000FF00
      0000FF000000FF000000FF000000FF000000FFFFFF00000000000000FF000000
      FF000000FF000000FF000000FF00000000000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FF000000FFFFFF00FFFFFF00FF000000FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF0000000000FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF00000000000000FF000000FF000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FF00
      0000FFFFFF000000000000000000FF0000000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000FFFFFF00FF0000000000
      0000FFFFFF000000000000000000FF0000000000000000000000000000000000
      FF000000FF000000FF000000000000000000000000000000FF000000FF000000
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF0000000000FFFFFF00FF0000000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000FFFFFF00FF000000FFFF
      FF00FFFFFF0000000000FFFFFF00FF0000000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000000000000000FF000000
      FF000000FF00000000007B7B7B00000000007B7B7B0000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000BDBD
      BD00FFFFFF0000000000FFFFFF000000000000000000000000007B7B7B000000
      FF000000FF000000FF00000000000000000000000000FFFFFF0000000000BDBD
      BD00FFFFFF0000000000FFFFFF000000000000000000000000007B7B7B000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FC00000000000000FC00000000000000
      FC00000000000000FC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000230000000000000001000000000000
      00000000000000000023000000000000006300000000000000C3000000000000
      010700000000000003FF000000000000FFFFFFFFFC00FC00F83FFFF8FC00FC00
      E00F20F8FC00FC00CC47007FFC00FC008463007C00000000A073003C00000000
      31F9000F0000000038F90004000000003C79000C002300233C3901FF00010001
      3C19E3FC000000009C0BFFFC002300238C43FFFF00630063C467FFF800C300C3
      E00FFFF801070107F83FFFFF03FF03FF00000000000000000000000000000000
      000000000000}
  end
end