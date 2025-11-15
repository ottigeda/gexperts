inherited f_AutoSwitchDestkopConfig: Tf_AutoSwitchDestkopConfig
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Auto Switch Desktop configuration'
  ClientHeight = 186
  ClientWidth = 249
  StyleElements = [seFont, seClient, seBorder]
  TextHeight = 15
  object l_FormDesktop: TLabel
    Left = 10
    Top = 14
    Width = 141
    Height = 15
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Desktop for &Form Designer'
  end
  object l_CodeDesktop: TLabel
    Left = 10
    Top = 78
    Width = 126
    Height = 15
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Desktop for &Code Editor'
  end
  object cmb_FormDesktop: TComboBox
    Left = 10
    Top = 38
    Width = 231
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
  end
  object cmb_CodeDesktop: TComboBox
    Left = 10
    Top = 102
    Width = 231
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 1
  end
  object b_OK: TButton
    Left = 42
    Top = 142
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 146
    Top = 142
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
