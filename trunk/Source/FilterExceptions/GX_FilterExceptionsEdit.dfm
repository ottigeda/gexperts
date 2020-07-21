inherited fmGxFilterExceptionsEdit: TfmGxFilterExceptionsEdit
  BorderIcons = [biSystemMenu]
  Caption = 'Exception Filter'
  ClientHeight = 337
  ClientWidth = 505
  PixelsPerInch = 96
  TextHeight = 13
  object b_OK: TButton
    Left = 328
    Top = 304
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object b_Cancel: TButton
    Left = 416
    Top = 304
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object rg_Action: TRadioGroup
    Left = 8
    Top = 248
    Width = 313
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Action'
    Items.Strings = (
      'Ignore Exception'
      'Break on Exception'
      'Disabled (no action)')
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object grp_Project: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 81
    Caption = 'Project (regex)'
    TabOrder = 0
    object ed_Project: TEdit
      Left = 8
      Top = 18
      Width = 153
      Height = 21
      TabOrder = 0
    end
    object b_ProjectName: TButton
      Left = 8
      Top = 48
      Width = 153
      Height = 25
      Caption = '<project name goes here>'
      TabOrder = 1
      OnClick = b_ProjectNameClick
    end
    object b_ProjectSession: TButton
      Left = 168
      Top = 48
      Width = 113
      Height = 25
      Caption = 'Current Session'
      TabOrder = 2
      OnClick = b_ProjectSessionClick
    end
    object b_ProjectAny: TButton
      Left = 168
      Top = 16
      Width = 113
      Height = 25
      Caption = 'Any Project'
      TabOrder = 3
      OnClick = b_ProjectAnyClick
    end
  end
  object grp_ExceptionClass: TGroupBox
    Left = 312
    Top = 8
    Width = 185
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Exception Class (regex)'
    TabOrder = 1
    object cmb_Exception: TComboBox
      Left = 8
      Top = 18
      Width = 153
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
    object b_ExceptionCurrent: TButton
      Left = 8
      Top = 48
      Width = 153
      Height = 25
      Caption = '<current name goes here>'
      TabOrder = 1
      OnClick = b_ExceptionCurrentClick
    end
  end
  object grp_Message: TGroupBox
    Left = 8
    Top = 104
    Width = 489
    Height = 136
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Exception Message (regex)'
    TabOrder = 2
    DesignSize = (
      489
      136)
    object l_Matches: TLabel
      Left = 8
      Top = 48
      Width = 171
      Height = 13
      Caption = 'Matches for the Regular Expression'
    end
    object ed_Message: TEdit
      Left = 8
      Top = 16
      Width = 473
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = ed_MessageChange
    end
    object re_Test: TRichEdit
      Left = 8
      Top = 64
      Width = 473
      Height = 64
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        
          'type any text you want to match here (e.g. the full message of t' +
          'he exception you want to '
        'catch)')
      TabOrder = 1
      OnChange = re_TestChange
    end
  end
  object b_AllInCurrentSession: TButton
    Left = 328
    Top = 272
    Width = 169
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ignore All in Current Session'
    TabOrder = 4
    OnClick = b_AllInCurrentSessionClick
  end
  object tim_InputDelay: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tim_InputDelayTimer
    Left = 264
    Top = 200
  end
end
