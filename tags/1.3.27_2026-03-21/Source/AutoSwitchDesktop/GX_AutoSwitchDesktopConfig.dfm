inherited f_AutoSwitchDestkopConfig: Tf_AutoSwitchDestkopConfig
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Auto Switch Desktop configuration'
  ClientHeight = 137
  ClientWidth = 249
  PixelsPerInch = 96
  TextHeight = 13
  object l_FormDesktop: TLabel
    Left = 8
    Top = 8
    Width = 128
    Height = 13
    Caption = 'Desktop for &Form Designer'
  end
  object l_CodeDesktop: TLabel
    Left = 8
    Top = 56
    Width = 115
    Height = 13
    Caption = 'Desktop for &Code Editor'
  end
  object cmb_FormDesktop: TComboBox
    Left = 8
    Top = 24
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object cmb_CodeDesktop: TComboBox
    Left = 8
    Top = 72
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object b_OK: TButton
    Left = 88
    Top = 104
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 168
    Top = 104
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
