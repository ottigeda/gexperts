object foTime: TfoTime
  Left = 527
  Top = 406
  BorderStyle = bsDialog
  Caption = 'Time Editor'
  ClientHeight = 110
  ClientWidth = 322
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
  object JvSpeedButton1: TJvSpeedButton
    Left = 59
    Top = 79
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
    Left = 186
    Top = 79
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
  object GroupBox1: TGroupBox
    Left = 6
    Top = 2
    Width = 307
    Height = 67
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 16
      Width = 90
      Height = 13
      Caption = 'Please enter a time'
    end
    object DateTimePicker1: TDateTimePicker
      Left = 30
      Top = 34
      Width = 265
      Height = 21
      Date = 36751.749809976800000000
      Time = 36751.749809976800000000
      Kind = dtkTime
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
    end
  end
end
