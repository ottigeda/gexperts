inherited f_Goto: Tf_Goto
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Go to'
  ClientHeight = 219
  ClientWidth = 217
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object l_LineNumber: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Line Number'
  end
  object cmb_LineNumber: TComboBox
    Left = 8
    Top = 24
    Width = 201
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnKeyDown = cmb_LineNumberKeyDown
  end
  object lb_UnitPositions: TListBox
    Left = 8
    Top = 56
    Width = 201
    Height = 33
    ItemHeight = 13
    TabOrder = 1
    OnClick = lb_UnitPositionsClick
    OnDblClick = lb_UnitPositionsDblClick
  end
  object b_OK: TButton
    Left = 56
    Top = 184
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 136
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
