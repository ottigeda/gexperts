object foDateTime: TfoDateTime
  Left = 626
  Top = 300
  BorderStyle = bsDialog
  Caption = 'Time Date Editor'
  ClientHeight = 130
  ClientWidth = 293
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
  object Label1: TLabel
    Left = 4
    Top = 12
    Width = 144
    Height = 13
    Caption = 'Please enter a time and a date'
  end
  object JvSpeedButton1: TJvSpeedButton
    Left = 44
    Top = 99
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
    Left = 171
    Top = 99
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
  object DateTimePicker1: TDateTimePicker
    Left = 24
    Top = 36
    Width = 251
    Height = 21
    Date = 36230.911923958300000000
    Time = 36230.911923958300000000
    Kind = dtkTime
    MaxDate = 402132.911923958000000000
    TabOrder = 0
  end
  object DateTimePicker2: TDateTimePicker
    Left = 24
    Top = 60
    Width = 251
    Height = 21
    Date = 36230.911923958300000000
    Time = 36230.911923958300000000
    DateFormat = dfLong
    MaxDate = 2958465.911923960000000000
    MinDate = 0.911923958454281000
    TabOrder = 1
  end
end
