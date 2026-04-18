object fmReplaceComp: TfmReplaceComp
  Left = 291
  Top = 311
  BorderStyle = bsDialog
  Caption = 'Replace Components'
  ClientHeight = 233
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    485
    233)
  TextHeight = 14
  object lblSeach: TLabel
    Left = 79
    Top = 16
    Width = 22
    Height = 14
    Alignment = taRightJustify
    Caption = '&Find'
    FocusControl = cbSearch
  end
  object lblReplace: TLabel
    Left = 33
    Top = 40
    Width = 70
    Height = 14
    Alignment = taRightJustify
    Caption = '&Replace with'
    FocusControl = cbReplace
  end
  object btnOK: TButton
    Left = 154
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 235
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnHelp: TButton
    Left = 397
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 7
    OnClick = btnHelpClick
  end
  object cbSearch: TComboBox
    Left = 111
    Top = 12
    Width = 314
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 18
    Sorted = True
    TabOrder = 0
    OnChange = cbSearchChange
    OnKeyDown = cbSearchKeyDown
  end
  object cbReplace: TComboBox
    Left = 111
    Top = 36
    Width = 314
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 18
    ItemHeight = 14
    Sorted = True
    TabOrder = 1
    OnChange = cbSearchChange
    OnKeyDown = cbSearchKeyDown
  end
  object gbxScope: TGroupBox
    Left = 188
    Top = 67
    Width = 285
    Height = 127
    Anchors = [akTop]
    Caption = 'Scope'
    TabOrder = 3
    DesignSize = (
      285
      127)
    object rbAllOnCurrentForm: TRadioButton
      Left = 8
      Top = 51
      Width = 271
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All components on the &current form'
      TabOrder = 1
    end
    object rbAllInProject: TRadioButton
      Left = 8
      Top = 79
      Width = 271
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All components on all forms in the &project'
      TabOrder = 2
    end
    object rbSelectedOnCurrentForm: TRadioButton
      Left = 8
      Top = 23
      Width = 271
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Selected components on the current form'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
  end
  object gbxOptions: TGroupBox
    Left = 12
    Top = 67
    Width = 165
    Height = 127
    Caption = '&Options'
    TabOrder = 2
    object chkIgnoreErrors: TCheckBox
      Left = 11
      Top = 23
      Width = 150
      Height = 17
      Caption = 'Ignore errors'
      TabOrder = 0
    end
    object chkLogChanges: TCheckBox
      Left = 11
      Top = 43
      Width = 150
      Height = 17
      Caption = 'Log changes'
      TabOrder = 1
    end
    object chkLogValues: TCheckBox
      Left = 23
      Top = 63
      Width = 140
      Height = 17
      Caption = 'Log all values'
      TabOrder = 2
    end
    object chkOverwriteLog: TCheckBox
      Left = 23
      Top = 83
      Width = 140
      Height = 17
      Caption = 'Overwrite old log'
      TabOrder = 3
    end
    object chkShowLogWin: TCheckBox
      Left = 23
      Top = 103
      Width = 140
      Height = 17
      Caption = 'Show log window'
      TabOrder = 4
    end
  end
  object btnSettings: TButton
    Left = 316
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'S&ettings...'
    TabOrder = 6
    OnClick = btnSettingsClick
  end
end
