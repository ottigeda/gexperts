object foTransHeader: TfoTransHeader
  Left = 399
  Top = 553
  BorderStyle = bsDialog
  Caption = 'C++ Header Translator'
  ClientHeight = 366
  ClientWidth = 406
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
    Caption = '&Translate'
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
      Height = 57
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 54
        Height = 13
        Caption = 'Header File'
      end
      object JvFileNameBox1: TJvFilenameEdit
        Left = 70
        Top = 12
        Width = 193
        Height = 21
        DialogOptions = [ofHideReadOnly, ofEnableSizing]
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 70
        Top = 34
        Width = 195
        Height = 17
        Caption = 'Translate Pre Processor Commands'
        TabOrder = 1
      end
    end
    object GroupBox3: TGroupBox
      Left = 6
      Top = 64
      Width = 271
      Height = 225
      TabOrder = 1
      Visible = False
      object ListBox1: TListBox
        Left = 6
        Top = 12
        Width = 257
        Height = 207
        ItemHeight = 13
        TabOrder = 0
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
    end
    object CheckNone: TAction
      Caption = 'Check &None'
      Hint = 'Check None'
      ShortCut = 16469
    end
  end
  object JvSaveDialog1: TJvSaveDialog
    DefaultExt = 'pas'
    Filter = 'Pascal Unit (*.pas)|*.pas'
    Height = 0
    Width = 0
    Left = 42
    Top = 102
  end
end
