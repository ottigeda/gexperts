inherited fmGxEditExceptionNotification: TfmGxEditExceptionNotification
  BorderIcons = [biSystemMenu]
  Caption = 'Exception Notification'
  ClientHeight = 281
  ClientWidth = 546
  PixelsPerInch = 96
  TextHeight = 13
  object l_Exception: TLabel
    Left = 8
    Top = 8
    Width = 77
    Height = 13
    Caption = 'Exception Name'
  end
  object l_Message: TLabel
    Left = 8
    Top = 56
    Width = 195
    Height = 13
    Caption = 'Exception Message (Regular Expression)'
  end
  object l_Matches: TLabel
    Left = 8
    Top = 104
    Width = 171
    Height = 13
    Caption = 'Matches for the Regular Expression'
  end
  object cmb_Exception: TComboBox
    Left = 8
    Top = 24
    Width = 281
    Height = 21
    ItemHeight = 0
    TabOrder = 0
  end
  object ed_Message: TEdit
    Left = 8
    Top = 72
    Width = 529
    Height = 21
    TabOrder = 1
    OnChange = ed_MessageChange
  end
  object re_Test: TRichEdit
    Left = 8
    Top = 120
    Width = 529
    Height = 57
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      
        'type any text you want to match here (e.g. the full message of t' +
        'he exception you want to catch)')
    TabOrder = 2
  end
  object b_OK: TButton
    Left = 384
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object b_Cancel: TButton
    Left = 464
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object rg_Action: TRadioGroup
    Left = 8
    Top = 192
    Width = 361
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Action'
    Items.Strings = (
      'Disabled (no action)'
      'Ignore Exception'
      'Break on Exception')
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object tim_InputDelay: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tim_InputDelayTimer
    Left = 264
    Top = 200
  end
end
