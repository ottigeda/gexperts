object foDate: TfoDate
  Left = 524
  Top = 339
  BorderStyle = bsDialog
  Caption = 'Date Editor'
  ClientHeight = 252
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
    Left = 57
    Top = 215
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
    Left = 184
    Top = 215
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
    Height = 203
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 16
      Width = 92
      Height = 13
      Caption = 'Please enter a date'
    end
    object MonthCalendar1: TMonthCalendar
      Left = 58
      Top = 36
      Width = 190
      Height = 153
      Date = 36751.529160300920000000
      TabOrder = 0
    end
  end
end
