inherited f_GrepMenuConfig: Tf_GrepMenuConfig
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Configuration'
  ClientHeight = 67
  ClientWidth = 386
  PixelsPerInch = 96
  TextHeight = 13
  object chk_ReplaceFind: TCheckBox
    Left = 8
    Top = 8
    Width = 369
    Height = 17
    Caption = 'Replace Search -> Find command (not yet fully implemented)'
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 224
    Top = 32
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 302
    Top = 32
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
