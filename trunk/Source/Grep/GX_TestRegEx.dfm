object fmTestRegEx: TfmTestRegEx
  Left = 0
  Top = 0
  Width = 553
  Height = 424
  BorderIcons = [biMaximize]
  Caption = 'Test Regular Expression'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    537
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object l_RegEx: TLabel
    Left = 8
    Top = 8
    Width = 92
    Height = 13
    Caption = 'Regular Expression'
  end
  object l_TestText: TLabel
    Left = 8
    Top = 88
    Width = 46
    Height = 13
    Caption = 'Test Text'
  end
  object ed_RegEx: TEdit
    Left = 8
    Top = 24
    Width = 521
    Height = 21
    TabOrder = 0
    OnChange = ed_RegExChange
  end
  object b_OK: TButton
    Left = 376
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object b_Cancel: TButton
    Left = 456
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object re_Test: TRichEdit
    Left = 8
    Top = 104
    Width = 521
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object chk_CaseSensitive: TCheckBox
    Left = 8
    Top = 56
    Width = 145
    Height = 17
    Caption = 'Case sensitive'
    TabOrder = 1
    OnClick = chk_CaseSensitiveClick
  end
  object tim_InputDelay: TTimer
    Interval = 500
    OnTimer = tim_InputDelayTimer
    Left = 264
    Top = 200
  end
end
