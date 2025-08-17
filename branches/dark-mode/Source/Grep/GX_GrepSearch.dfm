object fmGrepSearch: TfmGrepSearch
  Left = 311
  Top = 189
  BorderStyle = bsDialog
  Caption = 'Grep Search'
  ClientHeight = 580
  ClientWidth = 679
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  DesignSize = (
    679
    580)
  PixelsPerInch = 96
  TextHeight = 15
  object lblFind: TLabel
    Left = 21
    Top = 12
    Width = 59
    Height = 15
    Alignment = taRightJustify
    Caption = '&Text to find'
    FocusControl = cbText
  end
  object cbText: TComboBox
    Left = 85
    Top = 8
    Width = 579
    Height = 23
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
    Height = 193
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      233
      193)
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
    object b_TestRegEx: TButton
      Left = 24
      Top = 144
      Width = 75
      Height = 25
      Caption = 'Test ...'
      TabOrder = 7
      OnClick = b_TestRegExClick
    end
  end
  object gbxWhere: TGroupBox
    Left = 248
    Top = 40
    Width = 233
    Height = 193
    Caption = 'Files'
    TabOrder = 2
    DesignSize = (
      233
      193)
    object chk_UseMapFile: TCheckBox
      Left = 24
      Top = 79
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
      TabOrder = 5
      OnClick = rbDirectoriesClick
    end
    object rbDirectories: TRadioButton
      Left = 10
      Top = 122
      Width = 216
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Directories'
      TabOrder = 6
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
      TabOrder = 7
      OnClick = rbDirectoriesClick
    end
    object txt_NoMapFile: TStaticText
      Left = 43
      Top = 79
      Width = 117
      Height = 19
      Hint = 
        'Parses the .map file to get the unit names of all units used in ' +
        'the project.'#13#10'You must enable generating a map file with publics' +
        ' or a detailed map file in Linker Options.'
      Caption = 'Map file not available'
      TabOrder = 4
      Visible = False
    end
  end
  object gbxContentTypes: TGroupBox
    Left = 488
    Top = 40
    Width = 175
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delphi Code Content Types'
    TabOrder = 3
    DesignSize = (
      175
      81)
    object cbGrepCode: TCheckBox
      Left = 10
      Top = 18
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Code'
      TabOrder = 0
      OnClick = cbGrepCodeClick
    end
    object cbGrepStrings: TCheckBox
      Left = 10
      Top = 38
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Strings'
      TabOrder = 1
      OnClick = cbGrepStringsClick
    end
    object cbGrepComments: TCheckBox
      Left = 10
      Top = 58
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Comments'
      TabOrder = 2
      OnClick = cbGrepCommentsClick
    end
    object btnGrepAll: TButton
      Left = 124
      Top = 48
      Width = 43
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'All'
      TabOrder = 3
      OnClick = btnGrepAllClick
    end
  end
  object gbxUnitSections: TGroupBox
    Left = 488
    Top = 128
    Width = 175
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delphi Code Sections'
    TabOrder = 4
    DesignSize = (
      175
      105)
    object cbSectionInterface: TCheckBox
      Left = 10
      Top = 18
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Interface'
      TabOrder = 0
      OnClick = cbSectionInterfaceClick
    end
    object cbSectionImplementation: TCheckBox
      Left = 10
      Top = 38
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Implementation'
      TabOrder = 1
      OnClick = cbSectionImplementationClick
    end
    object cbSectionInitialization: TCheckBox
      Left = 10
      Top = 58
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Initialization'
      TabOrder = 2
      OnClick = cbSectionInitializationClick
    end
    object cbSectionFinalization: TCheckBox
      Left = 10
      Top = 78
      Width = 158
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Finalization'
      TabOrder = 3
      OnClick = cbSectionFinalizationClick
    end
    object btnSectionAll: TButton
      Left = 124
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
    Top = 240
    Width = 656
    Height = 217
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directory Search'
    TabOrder = 5
    DesignSize = (
      656
      217)
    object lblMasks: TLabel
      Left = 8
      Top = 72
      Width = 183
      Height = 15
      Caption = 'File mas&ks (separate by semicolon)'
      FocusControl = cbMasks
    end
    object lblDirectory: TLabel
      Left = 8
      Top = 24
      Width = 185
      Height = 15
      Caption = 'Di&rectories (separate by semicolon)'
      FocusControl = cbDirectory
    end
    object lblExcludeDirs: TLabel
      Left = 24
      Top = 168
      Width = 192
      Height = 15
      Caption = 'Exclude Dirs (separate by semicolon)'
      FocusControl = cbExcludedDirs
    end
    object l_MinMaxDepth: TLabel
      Left = 176
      Top = 121
      Width = 89
      Height = 15
      Caption = 'Min / Max Depth'
    end
    object cbMasks: TComboBox
      Left = 8
      Top = 88
      Width = 607
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      TabOrder = 2
      OnKeyDown = ComboKeyDown
    end
    object chk_SubDirRegex: TCheckBox
      Left = 240
      Top = 168
      Width = 149
      Height = 17
      Caption = 'Regular expression'
      TabOrder = 7
      OnClick = chk_SubDirRegexClick
    end
    object cbInclude: TCheckBox
      Left = 8
      Top = 128
      Width = 139
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search su&bdirectories'
      TabOrder = 3
      OnClick = cbIncludeClick
    end
    object cbDirectory: TComboBox
      Left = 8
      Top = 40
      Width = 607
      Height = 23
      Hint = 'Separate multiple directories by semicolon'
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      TabOrder = 0
      OnDropDown = cbDirectoryDropDown
      OnKeyDown = ComboKeyDown
    end
    object btnBrowse: TButton
      Left = 622
      Top = 39
      Width = 25
      Height = 25
      Hint = 'Select Directory'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object cbExcludedDirs: TComboBox
      Left = 24
      Top = 184
      Width = 591
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      TabOrder = 6
      OnDropDown = cbExcludedDirsDropDown
      OnKeyDown = ComboKeyDown
    end
    object ed_MinDepth: TEdit
      Left = 176
      Top = 136
      Width = 41
      Height = 23
      Enabled = False
      TabOrder = 4
      Text = '0'
      OnChange = ed_MinDepthChange
    end
    object ed_MaxDepth: TEdit
      Left = 224
      Top = 136
      Width = 41
      Height = 23
      Hint = 'empty for no depth limit'
      Enabled = False
      TabOrder = 5
      Text = '0'
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 545
    Width = 679
    Height = 35
    Align = alBottom
    TabOrder = 7
    DesignSize = (
      679
      35)
    object btnOK: TButton
      Left = 426
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
      Left = 506
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
      Left = 586
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
    Width = 656
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
end
