inherited fmGrepOptions: TfmGrepOptions
  Left = 294
  Top = 263
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Search Options'
  ClientHeight = 75
  ClientWidth = 377
  ParentFont = False
  Font.Charset = ANSI_CHARSET
  Font.Height = -12
  Position = poOwnerFormCenter
  DesignSize = (
    377
    75)
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 216
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 296
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object chkUseCurrentIdent: TCheckBox
    Left = 8
    Top = 8
    Width = 361
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use &current editor identifier as the default search string'
    TabOrder = 0
  end
end
