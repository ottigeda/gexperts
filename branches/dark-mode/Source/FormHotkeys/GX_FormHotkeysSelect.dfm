inherited fmFormHotkeysSelect: TfmFormHotkeysSelect
  ActiveControl = hk_Notkey
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select New Shortcut'
  ClientHeight = 89
  ClientWidth = 169
  PixelsPerInch = 96
  TextHeight = 13
  object l_HotkeyFor: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 13
    Caption = 'Hotkey for %s'
  end
  object b_OK: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object b_Cancel: TButton
    Left = 88
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object hk_Notkey: THotKey
    Left = 8
    Top = 24
    Width = 121
    Height = 19
    HotKey = 32833
    TabOrder = 2
  end
end
