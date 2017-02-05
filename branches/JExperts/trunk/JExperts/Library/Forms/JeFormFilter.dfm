object foFilter: TfoFilter
  Left = 452
  Top = 369
  BorderStyle = bsDialog
  Caption = 'Filter Editor'
  ClientHeight = 274
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object NextButton: TJvSpeedButton
    Left = 226
    Top = 239
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
    OnClick = NextButtonClick
  end
  object CancelButton: TJvSpeedButton
    Left = 319
    Top = 239
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
  object PrevButton: TJvSpeedButton
    Left = 8
    Top = 239
    Width = 77
    Height = 25
    Caption = '&Test'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    OnClick = PrevButtonClick
  end
  object JvGroupBox1: TJvGroupBox
    Left = 8
    Top = 6
    Width = 389
    Height = 225
    TabOrder = 0
    object StringGrid1: TStringGrid
      Left = 12
      Top = 14
      Width = 367
      Height = 177
      ColCount = 2
      DefaultColWidth = 170
      DefaultRowHeight = 15
      FixedCols = 0
      RowCount = 50
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object JvCombobox1: TJvComboBox
      Left = 12
      Top = 196
      Width = 367
      Height = 22
      Style = csDropDownList
      TabOrder = 1
      OnChange = JvCombobox1Change
    end
  end
  object OpenDialog1: TJvOpenDialog
    Height = 0
    Width = 0
    Left = 134
    Top = 58
  end
end
