inherited fmGxEditExceptionNotification: TfmGxEditExceptionNotification
  BorderIcons = [biSystemMenu]
  Caption = 'Exception Notification'
  ClientHeight = 313
  ClientWidth = 546
  PixelsPerInch = 96
  TextHeight = 13
  object l_Exception: TLabel
    Left = 296
    Top = 8
    Width = 77
    Height = 13
    Caption = 'Exception Name'
  end
  object l_Message: TLabel
    Left = 8
    Top = 80
    Width = 195
    Height = 13
    Caption = 'Exception Message (Regular Expression)'
  end
  object l_Matches: TLabel
    Left = 8
    Top = 128
    Width = 171
    Height = 13
    Caption = 'Matches for the Regular Expression'
  end
  object l_Project: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Project'
  end
  object cmb_Exception: TComboBox
    Left = 296
    Top = 24
    Width = 241
    Height = 21
    ItemHeight = 0
    TabOrder = 4
  end
  object ed_Message: TEdit
    Left = 8
    Top = 96
    Width = 529
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = ed_MessageChange
  end
  object re_Test: TRichEdit
    Left = 8
    Top = 144
    Width = 529
    Height = 65
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      
        'type any text you want to match here (e.g. the full message of t' +
        'he exception you want to catch)')
    TabOrder = 6
  end
  object b_OK: TButton
    Left = 384
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object b_Cancel: TButton
    Left = 464
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object rg_Action: TRadioGroup
    Left = 8
    Top = 224
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
    TabOrder = 7
  end
  object ed_Project: TEdit
    Left = 8
    Top = 24
    Width = 153
    Height = 21
    TabOrder = 0
  end
  object b_ProjectAny: TButton
    Left = 168
    Top = 16
    Width = 113
    Height = 25
    Caption = '<- Any'
    TabOrder = 2
    OnClick = b_ProjectAnyClick
  end
  object b_ProjectSession: TButton
    Left = 168
    Top = 48
    Width = 113
    Height = 25
    Caption = '<- Current Session'
    TabOrder = 3
    OnClick = b_ProjectSessionClick
  end
  object b_ProjectName: TButton
    Left = 8
    Top = 48
    Width = 153
    Height = 25
    Caption = '^- <project name goes here>'
    TabOrder = 1
    OnClick = b_ProjectNameClick
  end
  object tim_InputDelay: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tim_InputDelayTimer
    Left = 264
    Top = 200
  end
end
