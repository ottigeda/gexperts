inherited fmGxFastGrepForm: TfmGxFastGrepForm
  Left = 0
  Top = 0
  ActiveControl = ed_RegEx
  Caption = 'Fast Grep'
  ClientHeight = 405
  ClientWidth = 433
  PixelsPerInch = 96
  TextHeight = 14
  object l_RegEx: TLabel
    Left = 8
    Top = 9
    Width = 101
    Height = 14
    Caption = 'Regular &Expression'
    FocusControl = ed_RegEx
  end
  object l_PressEsc: TLabel
    Left = 160
    Top = 48
    Width = 273
    Height = 33
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Press Esc to return to the original position.'
    WordWrap = True
  end
  object ed_RegEx: TEdit
    Left = 8
    Top = 24
    Width = 417
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ed_RegExChange
    OnKeyDown = ed_RegExKeyDown
    OnKeyPress = ed_RegExKeyPress
  end
  object chk_CaseSensitive: TCheckBox
    Left = 8
    Top = 56
    Width = 145
    Height = 17
    Caption = '&Case sensitive'
    TabOrder = 1
  end
  object lb_Results: TListBox
    Left = 8
    Top = 80
    Width = 417
    Height = 321
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 64
    TabOrder = 2
    OnClick = lb_ResultsClick
    OnDblClick = lb_ResultsDblClick
    OnDrawItem = lb_ResultsDrawItem
    OnKeyPress = lb_ResultsKeyPress
  end
  object tim_InputDelay: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tim_InputDelayTimer
    Left = 264
    Top = 200
  end
  object tim_EditorChanged: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tim_EditorChangedTimer
    Left = 176
    Top = 216
  end
end
