object foRegister: TfoRegister
  Left = 394
  Top = 451
  BorderStyle = bsDialog
  Caption = 'Enter your Name and Serial'
  ClientHeight = 93
  ClientWidth = 299
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
    Left = 8
    Top = 10
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 10
    Top = 32
    Width = 26
    Height = 13
    Caption = 'Serial'
  end
  object JvSpeedButton1: TJvSpeedButton
    Left = 76
    Top = 61
    Width = 75
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
    Left = 180
    Top = 61
    Width = 75
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
  object Edit1: TEdit
    Left = 48
    Top = 4
    Width = 249
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 48
    Top = 28
    Width = 249
    Height = 21
    TabOrder = 1
  end
end
