object foDirCleaner: TfoDirCleaner
  Left = 409
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Directory Cleaner'
  ClientHeight = 366
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 6
    Top = 8
    Width = 100
    Height = 350
    AutoSize = True
  end
  object JvSpeedButton1: TJvSpeedButton
    Left = 152
    Top = 330
    Width = 77
    Height = 25
    Caption = '&Run'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    OnClick = JvSpeedButton1Click
  end
  object JvSpeedButton2: TJvSpeedButton
    Left = 286
    Top = 330
    Width = 77
    Height = 25
    Caption = '&Cancel'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 2
  end
  object Panel1: TPanel
    Left = 114
    Top = 8
    Width = 285
    Height = 305
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 6
      Top = 4
      Width = 271
      Height = 67
      TabOrder = 0
      object Label2: TLabel
        Left = 8
        Top = 18
        Width = 42
        Height = 13
        Caption = 'Directory'
      end
      object JvDirectoryBox1: TJvDirectoryEdit
        Left = 62
        Top = 14
        Width = 200
        Height = 21
        DialogKind = dkWin32
        DialogOptions = []
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 62
        Top = 40
        Width = 147
        Height = 17
        Caption = 'Search all &SubDirectories'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object GroupBox3: TGroupBox
      Left = 6
      Top = 76
      Width = 271
      Height = 147
      TabOrder = 1
      object JvSpeedButton3: TJvSpeedButton
        Left = 186
        Top = 10
        Width = 77
        Height = 25
        Caption = '&Insert'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton3Click
      end
      object JvSpeedButton4: TJvSpeedButton
        Left = 186
        Top = 42
        Width = 77
        Height = 25
        Caption = '&Delete'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton4Click
      end
      object JvCheckListBox1: TJvCheckListBox
        Left = 4
        Top = 10
        Width = 175
        Height = 129
        Columns = 3
        ItemHeight = 13
        Items.Strings = (
          '.~pas'
          '.~dfm'
          '.~dpk'
          '.~cpp'
          '.~h'
          '.~hpp'
          '.~bpg'
          '.~dsk'
          '.map'
          '.bak'
          '.tmp'
          '.dcu'
          '.dof'
          '.cfg'
          '.mps'
          '.btl'
          '.obj'
          '.exe'
          '.dll'
          '.---'
          '.old')
        TabOrder = 0
      end
    end
    object GroupBox2: TGroupBox
      Left = 6
      Top = 226
      Width = 271
      Height = 67
      TabOrder = 2
      Visible = False
      object Label3: TLabel
        Left = 66
        Top = 18
        Width = 57
        Height = 13
        Caption = 'Searching...'
      end
      object Animate1: TAnimate
        Left = 8
        Top = 14
        Width = 47
        Height = 45
        AutoSize = False
        CommonAVI = aviFindFile
        StopFrame = 8
      end
      object StaticText1: TStaticText
        Left = 78
        Top = 34
        Width = 183
        Height = 25
        AutoSize = False
        TabOrder = 1
      end
    end
  end
  object ActionList1: TActionList
    Left = 40
    Top = 40
    object CheckAll: TAction
      Caption = 'Check &All'
      Hint = 'Check All'
      ShortCut = 16449
      OnExecute = CheckAllExecute
    end
    object CheckNone: TAction
      Caption = 'Check &None'
      Hint = 'Check None'
      ShortCut = 16469
      OnExecute = CheckNoneExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 40
    Top = 70
    object CheckAll1: TMenuItem
      Action = CheckAll
    end
    object CheckNone1: TMenuItem
      Action = CheckNone
    end
  end
  object JvLogFile1: TJvLogFile
    Left = 40
    Top = 98
  end
  object JvSearchFile1: TJvSearchFiles
    OnBeginScanDir = JvSearchFile1ChangedDir
    OnFindFile = JvSearchFile1Found
    Left = 40
    Top = 126
  end
end
