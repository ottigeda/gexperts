inherited fmGxInstantGrepForm: TfmGxInstantGrepForm
  Left = 0
  Top = 0
  ActiveControl = ed_RegEx
  Caption = 'Instant Grep'
  ClientHeight = 405
  ClientWidth = 433
  OnActivate = FormActivate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 14
  object l_RegEx: TLabel
    Left = 8
    Top = 9
    Width = 101
    Height = 14
    Caption = 'Regular Expression'
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
    Caption = 'Case sensitive'
    TabOrder = 1
    OnClick = chk_CaseSensitiveClick
  end
  object p_Results: TPanel
    Left = 8
    Top = 80
    Width = 417
    Height = 321
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object lb_Results: TListBox
      Left = 1
      Top = 1
      Width = 415
      Height = 319
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 64
      TabOrder = 0
      OnClick = lb_ResultsClick
      OnDblClick = lb_ResultsDblClick
      OnDrawItem = lb_ResultsDrawItem
      OnKeyPress = lb_ResultsKeyPress
    end
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
