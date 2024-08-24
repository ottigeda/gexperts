object fmPasteAsConfig: TfmPasteAsConfig
  Left = 426
  Top = 292
  ActiveControl = cbPasteAsType
  BorderStyle = bsDialog
  Caption = 'PasteAs Configuration'
  ClientHeight = 209
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    313
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxPasteAsOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 297
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'PasteAs Options'
    TabOrder = 0
    object lblMaxEntries: TLabel
      Left = 16
      Top = 24
      Width = 70
      Height = 13
      Alignment = taRightJustify
      Caption = 'Paste as type:'
    end
    object chkCreateQuotedStrings: TCheckBox
      Left = 16
      Top = 64
      Width = 250
      Height = 25
      Caption = 'Create quoted strings'
      TabOrder = 1
    end
    object cbPasteAsType: TComboBox
      Left = 16
      Top = 40
      Width = 170
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object chkAddExtraSpaceAtTheEnd: TCheckBox
      Left = 16
      Top = 88
      Width = 250
      Height = 17
      Caption = 'Add extra space at the end'
      TabOrder = 2
    end
    object chkShowOptions: TCheckBox
      Left = 16
      Top = 128
      Width = 250
      Height = 17
      Caption = 'Show options dialog for each paste'
      TabOrder = 3
    end
  end
  object btnOK: TButton
    Left = 152
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 232
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
