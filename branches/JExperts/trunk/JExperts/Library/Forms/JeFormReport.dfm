object foBugReport: TfoBugReport
  Left = 283
  Top = 214
  BorderStyle = bsDialog
  Caption = 'Bug Report'
  ClientHeight = 367
  ClientWidth = 440
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
    Left = 232
    Top = 326
    Width = 77
    Height = 25
    Caption = '&Next'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    OnClick = JvSpeedButton1Click
  end
  object JvSpeedButton2: TJvSpeedButton
    Left = 342
    Top = 326
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
  object JvSpeedButton3: TJvSpeedButton
    Left = 122
    Top = 326
    Width = 77
    Height = 25
    Caption = '&Previous'
    Enabled = False
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    OnClick = JvSpeedButton3Click
  end
  object JvSurfTo1: TJvLabel
    Left = 66
    Top = 90
    Width = 5
    Height = 13
    Transparent = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object PageControl1: TPageControl
    Left = 114
    Top = 8
    Width = 319
    Height = 303
    ActivePage = TabSheet1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object TabSheet1: TTabSheet
      TabVisible = False
      object StaticText1: TStaticText
        Left = 4
        Top = 4
        Width = 271
        Height = 119
        AutoSize = False
        Caption = 
          'Use this expert to report a bug, to submit an idea for a '#13#10'futur' +
          'e expert or to submit possible ameliorations to the '#13#10'current ex' +
          'perts.'#13#10#13#10'Use and abuse this experts. Help me creating the best ' +
          #13#10'expert package ever made.'#13#10#13#10'Thank you !!!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      ImageIndex = 1
      TabVisible = False
      object GroupBox1: TGroupBox
        Left = 6
        Top = 6
        Width = 301
        Height = 45
        TabOrder = 0
        object Label1: TLabel
          Left = 12
          Top = 18
          Width = 61
          Height = 13
          Caption = 'Expert Name'
        end
        object Edit1: TEdit
          Left = 80
          Top = 14
          Width = 213
          Height = 21
          TabOrder = 0
        end
      end
      object GroupBox2: TGroupBox
        Left = 6
        Top = 56
        Width = 301
        Height = 209
        TabOrder = 1
        object Memo1: TMemo
          Left = 8
          Top = 16
          Width = 285
          Height = 183
          TabOrder = 0
        end
      end
    end
  end
end
