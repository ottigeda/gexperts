object foLayoutManage: TfoLayoutManage
  Left = 372
  Top = 313
  BorderStyle = bsDialog
  Caption = 'Layout Manager'
  ClientHeight = 364
  ClientWidth = 418
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
    Width = 297
    Height = 305
    TabOrder = 0
    object JvSpeedButton3: TJvSpeedButton
      Left = 210
      Top = 8
      Width = 77
      Height = 25
      Caption = '&Delete'
      Flat = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ModalResult = 1
      OnClick = JvSpeedButton3Click
    end
    object JvSpeedButton4: TJvSpeedButton
      Left = 210
      Top = 44
      Width = 77
      Height = 25
      Caption = '&Clear'
      Flat = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ModalResult = 1
      OnClick = JvSpeedButton4Click
    end
    object Listbox1: TJvListBox
      Left = 8
      Top = 8
      Width = 191
      Height = 291
      ItemHeight = 13
      Background.FillMode = bfmTile
      Background.Visible = False
      TabOrder = 0
    end
  end
end
