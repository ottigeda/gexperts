object foBatch: TfoBatch
  Left = 375
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Batch Compiler'
  ClientHeight = 364
  ClientWidth = 407
  Color = clBtnFace
  Constraints.MinHeight = 312
  Constraints.MinWidth = 360
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
    Width = 289
    Height = 305
    TabOrder = 0
    object Label1: TLabel
      Left = 6
      Top = 6
      Width = 264
      Height = 52
      Caption = 
        'Use this expert to compile a lot of project contained in a '#13#10'dir' +
        'ectory and his subdirectories. You just have to select '#13#10'the dir' +
        'ectory and then, it will compile every project '#13#10'file found.'
    end
    object GroupBox1: TGroupBox
      Left = 6
      Top = 66
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
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 62
        Top = 42
        Width = 147
        Height = 17
        Caption = 'Search all SubDirectories'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object GroupBox2: TGroupBox
      Left = 6
      Top = 136
      Width = 271
      Height = 159
      TabOrder = 1
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
        Width = 16
        Height = 16
        CommonAVI = aviFindFile
        StopFrame = 8
      end
      object ListBox1: TListBox
        Left = 8
        Top = 68
        Width = 253
        Height = 81
        ItemHeight = 13
        TabOrder = 1
      end
      object StaticText1: TStaticText
        Left = 78
        Top = 34
        Width = 183
        Height = 25
        AutoSize = False
        TabOrder = 2
      end
    end
  end
  object JvSearchFile1: TJvSearchFiles
    OnFindFile = JvSearchFile1Found
    Left = 28
    Top = 30
  end
end
