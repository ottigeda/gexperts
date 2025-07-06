inherited f_Goto: Tf_Goto
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Go to'
  ClientHeight = 219
  ClientWidth = 217
  ParentFont = False
  Font.Height = -13
  Font.Name = 'MS Shell Dlg 2'
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object l_LineNumber: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 16
    Caption = 'Line Number'
  end
  object cmb_LineNumber: TComboBox
    Left = 8
    Top = 27
    Width = 201
    Height = 24
    TabOrder = 0
    OnChange = cmb_LineNumberChange
    OnKeyDown = cmb_LineNumberKeyDown
  end
  object lb_UnitPositions: TListBox
    Left = 8
    Top = 57
    Width = 201
    Height = 123
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnClick = lb_UnitPositionsClick
    OnDblClick = lb_UnitPositionsDblClick
  end
  object b_OK: TButton
    Left = 53
    Top = 186
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 134
    Top = 186
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
