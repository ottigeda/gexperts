object foCursorEditor: TfoCursorEditor
  Left = 748
  Top = 351
  BorderStyle = bsDialog
  Caption = 'Cursor editor'
  ClientHeight = 232
  ClientWidth = 308
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
    Left = 52
    Top = 199
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
    Left = 179
    Top = 199
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
    Top = 6
    Width = 293
    Height = 181
    TabOrder = 0
    object ListBox1: TListBox
      Left = 12
      Top = 14
      Width = 147
      Height = 155
      ItemHeight = 13
      Items.Strings = (
        'crDefault'
        'crNone'
        'crArrow'
        'crCross'
        'crIBeam'
        'crSize'
        'crSizeNESW'
        'crSizeNS'
        'crSizeNWSE'
        'crSizeWE'
        'crUpArrow'
        'crHourGlass'
        'crDrag'
        'crNoDrop'
        'crHSplit'
        'crVSplit'
        'crMultiDrag'
        'crSQLWait'
        'crNo'
        'crAppStart'
        'crHelp'
        'crHandPoint'
        'crSizeAll')
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object Panel1: TPanel
      Left = 170
      Top = 14
      Width = 109
      Height = 155
      Caption = 'Preview'
      TabOrder = 1
    end
  end
end
