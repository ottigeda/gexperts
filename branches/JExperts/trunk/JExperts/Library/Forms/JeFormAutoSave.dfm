object foAutoSave: TfoAutoSave
  Left = 292
  Top = 250
  BorderStyle = bsDialog
  Caption = 'Automatic Save'
  ClientHeight = 104
  ClientWidth = 231
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
    Left = 10
    Top = 6
    Width = 212
    Height = 26
    Caption = 
      'You have not saved your files for a long time.'#13#10'What do you want' +
      ' to do ?'
  end
  object Label2: TLabel
    Left = 12
    Top = 44
    Width = 30
    Height = 13
    Caption = 'Action'
  end
  object JvSpeedButton1: TJvSpeedButton
    Left = 31
    Top = 71
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
    Left = 123
    Top = 71
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
  object ComboBox1: TComboBox
    Left = 52
    Top = 40
    Width = 169
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'Save all files'
      'Save current file'
      'Remind me later'
      'Do not bother me this session'
      'Never bother me again')
  end
  object Timer1: TTimer
    Interval = 30000
    OnTimer = Timer1Timer
    Left = 22
    Top = 12
  end
end
