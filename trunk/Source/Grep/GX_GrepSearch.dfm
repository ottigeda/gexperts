object fmGrepSearch: TfmGrepSearch
  Left = 311
  Top = 189
  BorderStyle = bsDialog
  Caption = 'Grep Search'
  ClientHeight = 588
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    489
    588)
  PixelsPerInch = 96
  TextHeight = 14
  object lblFind: TLabel
    Left = 14
    Top = 12
    Width = 66
    Height = 14
    Alignment = taRightJustify
    Caption = '&Text to find'
    FocusControl = cbText
  end
  object cbText: TComboBox
    Left = 85
    Top = 8
    Width = 397
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ItemHeight = 14
    TabOrder = 0
    OnKeyDown = ComboKeyDown
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 40
    Width = 233
    Height = 169
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      233
      169)
    object cbCaseSensitive: TCheckBox
      Left = 10
      Top = 18
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object cbForms: TCheckBox
      Left = 10
      Top = 58
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search for&m files'
      TabOrder = 2
    end
    object cbFormsMultiline: TCheckBox
      Left = 24
      Top = 80
      Width = 97
      Height = 17
      Hint = 
        'Concatenate multiple lines to one line before searching (experim' +
        'ental)'
      Caption = 'Multiline'
      TabOrder = 3
    end
    object cbFormsSpecialChars: TCheckBox
      Left = 128
      Top = 80
      Width = 97
      Height = 17
      Hint = 
        'Convert special characters from #NNN notation to characters befo' +
        're searching (experimental)'
      Caption = 'Special chars'
      TabOrder = 4
    end
    object cbWholeWord: TCheckBox
      Left = 10
      Top = 38
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Whole word'
      TabOrder = 1
    end
    object cbRegEx: TCheckBox
      Left = 10
      Top = 122
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Regular e&xpression'
      TabOrder = 6
    end
    object cbSQLFiles: TCheckBox
      Left = 10
      Top = 102
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search S&QL files'
      TabOrder = 5
    end
  end
  object gbxWhere: TGroupBox
    Left = 248
    Top = 40
    Width = 233
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Files'
    TabOrder = 2
    DesignSize = (
      233
      169)
    object chk_UseMapFile: TCheckBox
      Left = 24
      Top = 80
      Width = 201
      Height = 17
      Hint = 
        'Parses the .map file to get the unit names of all units used in ' +
        'the project.'#13#10'You must enable generating a map file with publics' +
        ' or a detailed map file in Linker Options.'
      Caption = 'Parse map file'
      TabOrder = 3
    end
    object rbAllProjFiles: TRadioButton
      Left = 10
      Top = 58
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All files in &project'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rbDirectoriesClick
    end
    object rbOpenFiles: TRadioButton
      Left = 10
      Top = 102
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Open project files'
      TabOrder = 4
      OnClick = rbDirectoriesClick
    end
    object rbDirectories: TRadioButton
      Left = 10
      Top = 122
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Directories'
      TabOrder = 5
      OnClick = rbDirectoriesClick
    end
    object rbCurrentOnly: TRadioButton
      Left = 10
      Top = 18
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Current &file'
      TabOrder = 0
      OnClick = rbDirectoriesClick
    end
    object rbAllProjGroupFiles: TRadioButton
      Left = 10
      Top = 38
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All files in project &group'
      TabOrder = 1
      OnClick = rbDirectoriesClick
    end
    object rbResults: TRadioButton
      Left = 10
      Top = 143
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Pre&vious search result files'
      TabOrder = 6
      OnClick = rbDirectoriesClick
    end
    object txt_NoMapFile: TStaticText
      Left = 40
      Top = 80
      Width = 116
      Height = 18
      Hint = 
        'Parses the .map file to get the unit names of all units used in ' +
        'the project.'#13#10'You must enable generating a map file with publics' +
        ' or a detailed map file in Linker Options.'
      Margins.Bottom = 0
      Caption = 'Map file not available'
      TabOrder = 7
      Visible = False
    end
  end
  object gbxContentTypes: TGroupBox
    Left = 8
    Top = 216
    Width = 233
    Height = 105
    Caption = 'Delphi Code Content Types'
    TabOrder = 3
    DesignSize = (
      233
      105)
    object cbGrepCode: TCheckBox
      Left = 10
      Top = 18
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Code'
      TabOrder = 0
      OnClick = cbGrepCodeClick
    end
    object cbGrepStrings: TCheckBox
      Left = 10
      Top = 38
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Strings'
      TabOrder = 1
      OnClick = cbGrepStringsClick
    end
    object cbGrepComments: TCheckBox
      Left = 10
      Top = 58
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Comments'
      TabOrder = 2
      OnClick = cbGrepCommentsClick
    end
    object btnGrepAll: TButton
      Left = 182
      Top = 72
      Width = 43
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'All'
      TabOrder = 3
      OnClick = btnGrepAllClick
    end
  end
  object gbxUnitSections: TGroupBox
    Left = 248
    Top = 216
    Width = 233
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delphi Code Sections'
    TabOrder = 4
    DesignSize = (
      233
      105)
    object cbSectionInterface: TCheckBox
      Left = 10
      Top = 18
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Interface'
      TabOrder = 0
      OnClick = cbSectionInterfaceClick
    end
    object cbSectionImplementation: TCheckBox
      Left = 10
      Top = 38
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Implementation'
      TabOrder = 1
      OnClick = cbSectionImplementationClick
    end
    object cbSectionInitialization: TCheckBox
      Left = 10
      Top = 58
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Initialization'
      TabOrder = 2
      OnClick = cbSectionInitializationClick
    end
    object cbSectionFinalization: TCheckBox
      Left = 10
      Top = 78
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Finalization'
      TabOrder = 3
      OnClick = cbSectionFinalizationClick
    end
    object btnSectionAll: TButton
      Left = 182
      Top = 72
      Width = 43
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'All'
      TabOrder = 4
      OnClick = btnSectionAllClick
    end
  end
  object gbxDirectories: TGroupBox
    Left = 8
    Top = 324
    Width = 474
    Height = 133
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directory Search'
    TabOrder = 5
    DesignSize = (
      474
      133)
    object lblMasks: TLabel
      Left = 25
      Top = 84
      Width = 53
      Height = 14
      Hint = 'Separate multiple file masks by semicolon'
      Alignment = taRightJustify
      Caption = 'File mas&ks'
      FocusControl = cbMasks
    end
    object lblDirectory: TLabel
      Left = 21
      Top = 26
      Width = 57
      Height = 14
      Hint = 'Separate multiple directories by semicolon'
      Alignment = taRightJustify
      Caption = 'Di&rectories'
      FocusControl = cbDirectory
    end
    object lblExcludeDirs: TLabel
      Left = 13
      Top = 55
      Width = 65
      Height = 14
      Hint = 'Separate multiple directories by semicolon'
      Alignment = taRightJustify
      Caption = 'Exclude Dirs'
      FocusControl = cbExcludedDirs
    end
    object cbMasks: TComboBox
      Left = 84
      Top = 80
      Width = 355
      Height = 22
      Hint = 'Separate multiple file masks by semicolon'
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 3
      OnKeyDown = ComboKeyDown
    end
    object cbInclude: TCheckBox
      Left = 84
      Top = 106
      Width = 379
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search su&bdirectories'
      TabOrder = 4
    end
    object cbDirectory: TComboBox
      Left = 84
      Top = 22
      Width = 355
      Height = 22
      Hint = 'Separate multiple directories by semicolon'
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 0
      OnDropDown = cbDirectoryDropDown
      OnKeyDown = ComboKeyDown
    end
    object btnBrowse: TButton
      Left = 440
      Top = 22
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object cbExcludedDirs: TComboBox
      Left = 84
      Top = 51
      Width = 355
      Height = 22
      Hint = 'Separate multiple directories by semicolon'
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 2
      OnDropDown = cbExcludedDirsDropDown
      OnKeyDown = ComboKeyDown
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 553
    Width = 489
    Height = 35
    Align = alBottom
    TabOrder = 7
    DesignSize = (
      489
      35)
    object btnOK: TButton
      Left = 244
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 324
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnHelp: TButton
      Left = 404
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Help'
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnSearch: TButton
      Left = 89
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Search'
      TabOrder = 1
      Visible = False
      OnClick = btnOKClick
    end
    object btnOptions: TButton
      Left = 8
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Options'
      TabOrder = 0
      OnClick = btnOptionsClick
    end
  end
  object rgSaveOption: TRadioGroup
    Left = 8
    Top = 464
    Width = 474
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Save Search History'
    ItemIndex = 1
    Items.Strings = (
      'Save parameters and results'
      'Only save parameters'
      'Do not save')
    ParentShowHint = False
    ShowHint = False
    TabOrder = 6
  end
  object timHintTimer: TTimer
    Interval = 5000
    OnTimer = timHintTimerTimer
    Left = 176
    Top = 136
  end
end
