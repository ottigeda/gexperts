object fmAsciiChart: TfmAsciiChart
  Left = 422
  Top = 177
  Caption = 'ASCII Chart'
  ClientHeight = 395
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnConstrainedResize = FormConstrainedResize
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object pb_Grid: TPaintBox
    Left = 0
    Top = 24
    Width = 542
    Height = 371
    Align = alClient
    OnMouseMove = pb_GridMouseMove
    OnMouseUp = pb_GridMouseUp
    OnPaint = pb_GridPaint
  end
  object p_ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 542
    Height = 24
    Align = alTop
    TabOrder = 0
    object sb_Low: TSpeedButton
      Left = 0
      Top = 1
      Width = 41
      Height = 22
      GroupIndex = 1
      Down = True
      Caption = 'Low'
      Flat = True
      OnClick = actCharLowExecute
    end
    object sb_High: TSpeedButton
      Left = 42
      Top = 2
      Width = 41
      Height = 22
      GroupIndex = 1
      Caption = 'High'
      Flat = True
      OnClick = actCharHighExecute
    end
    object sb_Dec: TSpeedButton
      Left = 90
      Top = 2
      Width = 41
      Height = 22
      GroupIndex = 2
      Down = True
      Caption = 'Dec'
      Flat = True
      OnClick = actCharDecExecute
    end
    object sb_Hex: TSpeedButton
      Left = 130
      Top = 2
      Width = 41
      Height = 22
      GroupIndex = 2
      Caption = 'Hex'
      Flat = True
      OnClick = actCharHexExecute
    end
    object cbxFontName: TComboBox
      Left = 176
      Top = 0
      Width = 175
      Height = 22
      Hint = 'Character Font'
      Style = csDropDownList
      DropDownCount = 20
      ItemHeight = 14
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = cbxFontNameChange
      OnEnter = cbxFontNameEnter
    end
    object edFontSize: TEdit
      Left = 352
      Top = 0
      Width = 57
      Height = 22
      MaxLength = 2
      TabOrder = 1
      Text = '9'
      OnChange = edFontSizeChange
    end
    object updFontSize: TUpDown
      Left = 409
      Top = 0
      Width = 16
      Height = 22
      Associate = edFontSize
      Min = 6
      Max = 20
      Position = 9
      TabOrder = 2
      OnClick = updFontSizeClick
    end
    object eChars: TEdit
      Left = 432
      Top = 0
      Width = 85
      Height = 22
      TabOrder = 3
    end
    object btnClear: TButton
      Left = 517
      Top = 0
      Width = 22
      Height = 22
      Caption = 'X'
      TabOrder = 4
      OnClick = btnClearClick
    end
  end
  object pmContext: TPopupMenu
    AutoPopup = False
    Left = 72
    Top = 88
    object mitShowLowCharacters: TMenuItem
      Action = actCharLow
      GroupIndex = 1
      RadioItem = True
    end
    object mitShowHighCharacters: TMenuItem
      Action = actCharHigh
      GroupIndex = 1
      RadioItem = True
    end
    object mitSep1: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mitCharAsDec: TMenuItem
      Action = actCharDec
      GroupIndex = 2
      RadioItem = True
    end
    object mitCharAsHex: TMenuItem
      Action = actCharHex
      GroupIndex = 2
      RadioItem = True
    end
    object mitSep2: TMenuItem
      Caption = '-'
      GroupIndex = 2
    end
    object mitFontSize8: TMenuItem
      Tag = 8
      Action = actFontSize8
      GroupIndex = 3
      RadioItem = True
    end
    object mitFontSize10: TMenuItem
      Tag = 10
      Action = actFontSize10
      GroupIndex = 3
      RadioItem = True
    end
    object mitFontSize12: TMenuItem
      Tag = 12
      Action = actFontSize12
      GroupIndex = 3
      RadioItem = True
    end
    object mitSep3: TMenuItem
      Caption = '-'
      GroupIndex = 3
    end
    object mitShowHints: TMenuItem
      Action = actShowHints
      GroupIndex = 3
    end
    object mitSep4: TMenuItem
      Caption = '-'
      GroupIndex = 3
    end
    object mitHelp: TMenuItem
      Action = actHelpHelp
      GroupIndex = 3
    end
    object mitAbout: TMenuItem
      Action = actHelpAbout
      GroupIndex = 3
    end
  end
  object HintTimer: TTimer
    Interval = 3000
    OnTimer = HintTimerTimer
    Left = 16
    Top = 88
  end
  object Actions: TActionList
    Left = 128
    Top = 120
    object actCharLow: TAction
      Category = 'HighLow'
      Caption = 'Show Characters &0-127'
      GroupIndex = 1
      ImageIndex = 3
      ShortCut = 16460
      OnExecute = actCharLowExecute
      OnUpdate = actCharLowUpdate
    end
    object actCharHigh: TAction
      Category = 'HighLow'
      Caption = 'Show Characters &128-255'
      GroupIndex = 1
      ImageIndex = 2
      ShortCut = 16456
      OnExecute = actCharHighExecute
      OnUpdate = actCharHighUpdate
    end
    object actCharDec: TAction
      Category = 'HexDec'
      Caption = 'Character Values as &Decimal'
      GroupIndex = 2
      ImageIndex = 0
      ShortCut = 24644
      OnExecute = actCharDecExecute
      OnUpdate = actCharDecUpdate
    end
    object actCharHex: TAction
      Category = 'HexDec'
      Caption = 'Character Values as &Hexadecimal'
      GroupIndex = 2
      ImageIndex = 1
      ShortCut = 24648
      OnExecute = actCharHexExecute
      OnUpdate = actCharHexUpdate
    end
    object actFontSize8: TAction
      Category = 'FontSize'
      Caption = 'Font Size: 8'
      ShortCut = 16440
      OnExecute = actFontSize8Execute
      OnUpdate = actGenericFontSizeUpdate
    end
    object actFontSize10: TAction
      Category = 'FontSize'
      Caption = 'Font Size: 10'
      ShortCut = 16432
      OnExecute = actFontSize10Execute
      OnUpdate = actGenericFontSizeUpdate
    end
    object actFontSize12: TAction
      Category = 'FontSize'
      Caption = 'Font Size: 12'
      ShortCut = 16434
      OnExecute = actFontSize12Execute
      OnUpdate = actGenericFontSizeUpdate
    end
    object actShowHints: TAction
      Category = 'Options'
      Caption = 'Show Hi&nts'
      OnExecute = actShowHintsExecute
      OnUpdate = actShowHintsUpdate
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      ImageIndex = 4
      ShortCut = 112
      OnExecute = actHelpHelpExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      OnExecute = actHelpAboutExecute
    end
  end
end
