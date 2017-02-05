object foAscii: TfoAscii
  Left = 249
  Top = 135
  BorderStyle = bsDialog
  Caption = 'ASCII Character Table'
  ClientHeight = 374
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
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
    Left = 150
    Top = 330
    Width = 77
    Height = 25
    Caption = '&Ok'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 1
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
  object PageControl1: TPageControl
    Left = 114
    Top = 8
    Width = 332
    Height = 309
    ActivePage = TabSheet1
    MultiLine = True
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      TabVisible = False
      object JvSpeedButton3: TJvSpeedButton
        Left = 2
        Top = 2
        Width = 77
        Height = 25
        Caption = '&Font...'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvBitBtn1Click
      end
      object Edit1: TEdit
        Left = 86
        Top = 4
        Width = 185
        Height = 21
        Hint = 'Click on characters to add to this box'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Terminal'
    Font.Style = []
    MinFontSize = 1
    MaxFontSize = 10
    Options = [fdEffects, fdLimitSize]
    Left = 20
    Top = 30
  end
end
