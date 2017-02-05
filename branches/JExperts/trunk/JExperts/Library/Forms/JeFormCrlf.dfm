object foCrlf: TfoCrlf
  Left = 343
  Top = 266
  BorderStyle = bsDialog
  Caption = 'Carriage Return Correcter'
  ClientHeight = 365
  ClientWidth = 410
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
    Caption = '&Ok'
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
      Width = 268
      Height = 52
      Caption = 
        'Use this expert to correct the carriage return problems'#13#10'encount' +
        'ered with some files edited with another program.'#13#10'Sometimes, th' +
        'e compiler gets crazy with those files'#13#10'and see errors at a bad ' +
        'line number... '
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 70
      Width = 271
      Height = 47
      TabOrder = 0
      object Label2: TLabel
        Left = 12
        Top = 18
        Width = 69
        Height = 13
        Caption = 'Files to correct'
      end
      object JvFileNameBox1: TJvFilenameEdit
        Left = 90
        Top = 16
        Width = 171
        Height = 21
        Filter = 
          'Pascal Units (*.pas)|*.pas|C/CPP Source (*.c;*.cpp)|*.c;*.cpp|Al' +
          'l Files (*.*)|*.*'
        DialogOptions = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
        TabOrder = 0
      end
    end
  end
end
