object fmMacroTemplateEdit: TfmMacroTemplateEdit
  Left = 374
  Top = 371
  BorderStyle = bsDialog
  Caption = 'Macro Template'
  ClientHeight = 193
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object lblName: TLabel
    Left = 8
    Top = 8
    Width = 84
    Height = 14
    Alignment = taRightJustify
    Caption = 'Keyword/&Name'
    FocusControl = edtName
  end
  object lblDescription: TLabel
    Left = 8
    Top = 56
    Width = 60
    Height = 14
    Alignment = taRightJustify
    Caption = '&Description'
    FocusControl = edtDescription
  end
  object lblShortcut: TLabel
    Left = 168
    Top = 104
    Width = 48
    Height = 14
    Alignment = taRightJustify
    Caption = '&Shortcut'
    FocusControl = edtShortCut
  end
  object lblInsertPos: TLabel
    Left = 8
    Top = 104
    Width = 78
    Height = 14
    Alignment = taRightJustify
    Caption = '&Insert Position'
    FocusControl = cbxInsertPos
  end
  object edtName: TEdit
    Left = 8
    Top = 24
    Width = 153
    Height = 22
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 8
    Top = 72
    Width = 313
    Height = 22
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 168
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 248
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object cbxInsertPos: TComboBox
    Left = 8
    Top = 120
    Width = 153
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 2
    Items.Strings = (
      'Cursor Position'
      'Beginning of File'
      'Beginning of Line')
  end
  object edtShortCut: THotKey
    Left = 168
    Top = 120
    Width = 153
    Height = 22
    HotKey = 32833
    InvalidKeys = [hcNone, hcShift]
    Modifiers = [hkAlt]
    TabOrder = 3
  end
end
