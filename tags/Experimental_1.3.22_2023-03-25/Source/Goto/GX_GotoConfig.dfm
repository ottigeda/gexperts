inherited f_GotoConfig: Tf_GotoConfig
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Go to configuration'
  ClientHeight = 65
  ClientWidth = 329
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object chk_ReplaceSearchGoto: TCheckBox
    Left = 8
    Top = 8
    Width = 313
    Height = 17
    Caption = 'Replace Search -> Go to Line Number command'
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 168
    Top = 32
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 248
    Top = 32
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
