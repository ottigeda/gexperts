inherited f_GrepMenuConfig: Tf_GrepMenuConfig
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Configuration'
  ClientHeight = 97
  ClientWidth = 289
  PixelsPerInch = 96
  TextHeight = 13
  object chk_ReplaceFind: TCheckBox
    Left = 8
    Top = 8
    Width = 273
    Height = 17
    Caption = 'Replace Search -> Find command'
    TabOrder = 0
  end
  object chk_ReplaceFindInFiles: TCheckBox
    Left = 8
    Top = 32
    Width = 273
    Height = 17
    Caption = 'Replace Search -> Find in Files command'
    TabOrder = 1
  end
  object b_OK: TButton
    Left = 128
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 206
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
