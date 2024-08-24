object fmClipboardOptions: TfmClipboardOptions
  Left = 294
  Top = 198
  BorderStyle = bsDialog
  Caption = 'Clipboard History Options'
  ClientHeight = 226
  ClientWidth = 305
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
  object lblMaxEntries: TLabel
    Left = 8
    Top = 8
    Width = 91
    Height = 14
    Caption = '&Maximum entries'
    FocusControl = edtMaxClip
  end
  object lbFont: TLabel
    Left = 8
    Top = 112
    Width = 25
    Height = 14
    Caption = '&Font'
    FocusControl = cbPreviewFont
  end
  object btnOK: TButton
    Left = 144
    Top = 192
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 224
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object chkAutoClose: TCheckBox
    Left = 8
    Top = 80
    Width = 271
    Height = 25
    Caption = 'Auto-cl&ose window after copy'
    TabOrder = 2
  end
  object chkAutoStart: TCheckBox
    Left = 8
    Top = 56
    Width = 271
    Height = 25
    Caption = 'St&art clipboard capture on startup'
    TabOrder = 1
  end
  object edtMaxClip: TEdit
    Left = 8
    Top = 24
    Width = 59
    Height = 22
    TabOrder = 0
  end
  object cbPreviewFont: TComboBox
    Left = 8
    Top = 128
    Width = 201
    Height = 22
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = cbPreviewFontChange
  end
  object edPreviewFont: TEdit
    Left = 216
    Top = 128
    Width = 33
    Height = 22
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '1'
    OnChange = edPreviewFontChange
  end
  object udPreviewFont: TUpDown
    Left = 249
    Top = 128
    Width = 17
    Height = 22
    Anchors = [akTop, akRight]
    Associate = edPreviewFont
    Min = 1
    Position = 1
    TabOrder = 5
  end
  object btnPreviewFont: TButton
    Left = 272
    Top = 128
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 6
    OnClick = btnPreviewFontClick
  end
  object txtPreview: TStaticText
    Left = 8
    Top = 160
    Width = 253
    Height = 18
    Caption = 'The quick brown fox jumps over the lazy dog'
    TabOrder = 9
  end
  object b_Defaults: TButton
    Left = 8
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Defaults'
    TabOrder = 10
    OnClick = b_DefaultsClick
  end
end
