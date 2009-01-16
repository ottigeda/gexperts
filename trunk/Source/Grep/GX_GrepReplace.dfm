object fmGrepReplace: TfmGrepReplace
  Left = 378
  Top = 227
  BorderStyle = bsDialog
  Caption = 'Replace Matches'
  ClientHeight = 138
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    396
    138)
  PixelsPerInch = 96
  TextHeight = 14
  object lblWith: TLabel
    Left = 9
    Top = 33
    Width = 26
    Height = 14
    Caption = '&With'
    FocusControl = cbReplace
  end
  object lblIn: TLabel
    Left = 9
    Top = 57
    Width = 11
    Height = 14
    Caption = 'In'
  end
  object lblInString: TLabel
    Left = 61
    Top = 57
    Width = 328
    Height = 42
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    ShowAccelChar = False
    WordWrap = True
  end
  object lblReplace: TLabel
    Left = 9
    Top = 8
    Width = 42
    Height = 14
    Caption = 'Replace'
  end
  object lblReplaceString: TLabel
    Left = 61
    Top = 8
    Width = 4
    Height = 14
    ShowAccelChar = False
  end
  object cbReplace: TComboBox
    Left = 61
    Top = 30
    Width = 328
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 155
    Top = 105
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 234
    Top = 105
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnHelp: TButton
    Left = 313
    Top = 105
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
end
