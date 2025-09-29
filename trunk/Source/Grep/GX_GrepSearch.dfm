object fmGrepSearch: TfmGrepSearch
  Left = 311
  Top = 189
  BorderStyle = bsDialog
  Caption = 'Grep Search'
  ClientHeight = 588
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  DesignSize = (
    689
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
    Width = 597
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ItemHeight = 14
    TabOrder = 0
    OnKeyDown = ComboKeyDown
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 37
    Width = 231
    Height = 199
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      231
      199)
    object cbCaseSensitive: TCheckBox
      Left = 8
      Top = 23
      Width = 214
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object cbForms: TCheckBox
      Left = 8
      Top = 63
      Width = 214
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search for&m files'
      TabOrder = 2
    end
    object cbFormsMultiline: TCheckBox
      Left = 22
      Top = 83
      Width = 97
      Height = 17
      Hint = 
        'Concatenate multiple lines to one line before searching (experim' +
        'ental)'
      Caption = 'Multiline'
      TabOrder = 3
    end
    object cbFormsSpecialChars: TCheckBox
      Left = 126
      Top = 83
      Width = 97
      Height = 17
      Hint = 
        'Convert special characters from #NNN notation to characters befo' +
        're searching (experimental)'
      Caption = 'Special chars'
      TabOrder = 4
    end
    object cbWholeWord: TCheckBox
      Left = 8
      Top = 43
      Width = 214
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Whole word'
      TabOrder = 1
    end
    object cbRegEx: TCheckBox
      Left = 8
      Top = 125
      Width = 214
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Regular e&xpression'
      TabOrder = 6
    end
    object cbSQLFiles: TCheckBox
      Left = 8
      Top = 105
      Width = 214
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search S&QL files'
      TabOrder = 5
    end
    object b_TestRegEx: TButton
      Left = 26
      Top = 149
      Width = 75
      Height = 25
      Caption = 'Test ...'
      TabOrder = 7
      OnClick = b_TestRegExClick
    end
  end
  object gbxWhere: TGroupBox
    Left = 245
    Top = 37
    Width = 217
    Height = 199
    Caption = 'Files'
    TabOrder = 2
    DesignSize = (
      217
      199)
    object chk_UseMapFile: TCheckBox
      Left = 22
      Top = 85
      Width = 187
      Height = 17
      Hint = 
        'Parses the .map file to get the unit names of all units used in ' +
        'the project.'#13#10'You must enable generating a map file with publics' +
        ' or a detailed map file in Linker Options.'
      Caption = 'Parse map file'
      TabOrder = 3
    end
    object rbAllProjFiles: TRadioButton
      Left = 8
      Top = 63
      Width = 200
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All files in &project'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rbDirectoriesClick
    end
    object rbOpenFiles: TRadioButton
      Left = 8
      Top = 105
      Width = 200
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Open project files'
      TabOrder = 5
      OnClick = rbDirectoriesClick
    end
    object rbDirectories: TRadioButton
      Left = 8
      Top = 125
      Width = 200
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Directories'
      TabOrder = 6
      OnClick = rbDirectoriesClick
    end
    object rbCurrentOnly: TRadioButton
      Left = 8
      Top = 23
      Width = 200
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Current &file'
      TabOrder = 0
      OnClick = rbDirectoriesClick
    end
    object rbAllProjGroupFiles: TRadioButton
      Left = 8
      Top = 43
      Width = 200
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All files in project &group'
      TabOrder = 1
      OnClick = rbDirectoriesClick
    end
    object rbResults: TRadioButton
      Left = 8
      Top = 146
      Width = 200
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Pre&vious search result files'
      TabOrder = 7
      OnClick = rbDirectoriesClick
    end
    object txt_NoMapFile: TStaticText
      Left = 43
      Top = 86
      Width = 116
      Height = 18
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
    Left = 470
    Top = 37
    Width = 211
    Height = 84
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delphi Code Content Types'
    TabOrder = 3
    DesignSize = (
      211
      84)
    object cbGrepCode: TCheckBox
      Left = 10
      Top = 23
      Width = 144
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Code'
      TabOrder = 0
      OnClick = cbGrepCodeClick
    end
    object cbGrepStrings: TCheckBox
      Left = 10
      Top = 43
      Width = 144
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Strings'
      TabOrder = 1
      OnClick = cbGrepStringsClick
    end
    object cbGrepComments: TCheckBox
      Left = 10
      Top = 63
      Width = 144
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Comments'
      TabOrder = 2
      OnClick = cbGrepCommentsClick
    end
    object btnGrepAll: TButton
      Left = 160
      Top = 54
      Width = 43
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'All'
      TabOrder = 3
      OnClick = btnGrepAllClick
    end
  end
  object gbxUnitSections: TGroupBox
    Left = 470
    Top = 128
    Width = 211
    Height = 108
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delphi Code Sections'
    TabOrder = 4
    DesignSize = (
      211
      108)
    object cbSectionInterface: TCheckBox
      Left = 10
      Top = 23
      Width = 145
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Interface'
      TabOrder = 0
      OnClick = cbSectionInterfaceClick
    end
    object cbSectionImplementation: TCheckBox
      Left = 10
      Top = 43
      Width = 145
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Implementation'
      TabOrder = 1
      OnClick = cbSectionImplementationClick
    end
    object cbSectionInitialization: TCheckBox
      Left = 10
      Top = 63
      Width = 144
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Initialization'
      TabOrder = 2
      OnClick = cbSectionInitializationClick
    end
    object cbSectionFinalization: TCheckBox
      Left = 10
      Top = 83
      Width = 144
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Finalization'
      TabOrder = 3
      OnClick = cbSectionFinalizationClick
    end
    object btnSectionAll: TButton
      Left = 160
      Top = 77
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
    Width = 674
    Height = 215
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directory Search'
    TabOrder = 5
    DesignSize = (
      674
      215)
    object lblMasks: TLabel
      Left = 10
      Top = 69
      Width = 188
      Height = 14
      Caption = 'File mas&ks (separate by semicolon)'
      FocusControl = cbMasks
    end
    object lblDirectory: TLabel
      Left = 10
      Top = 23
      Width = 192
      Height = 14
      Caption = 'Di&rectories (separate by semicolon)'
      FocusControl = cbDirectory
    end
    object lblExcludeDirs: TLabel
      Left = 24
      Top = 161
      Width = 200
      Height = 14
      Caption = 'Exclude Dirs (separate by semicolon)'
      FocusControl = cbExcludedDirs
    end
    object l_MinMaxDepth: TLabel
      Left = 176
      Top = 117
      Width = 90
      Height = 14
      Caption = 'Min / Max Depth'
    end
    object cbMasks: TComboBox
      Left = 10
      Top = 86
      Width = 623
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 2
      OnKeyDown = ComboKeyDown
    end
    object chk_SubDirRegex: TCheckBox
      Left = 240
      Top = 160
      Width = 149
      Height = 19
      Caption = 'Regular expression'
      TabOrder = 7
      OnClick = chk_SubDirRegexClick
    end
    object cbInclude: TCheckBox
      Left = 10
      Top = 121
      Width = 157
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search su&bdirectories'
      TabOrder = 3
      OnClick = cbIncludeClick
    end
    object cbDirectory: TComboBox
      Left = 10
      Top = 40
      Width = 623
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
      Left = 640
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
      Top = 179
      Width = 609
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 6
      OnDropDown = cbExcludedDirsDropDown
      OnKeyDown = ComboKeyDown
    end
    object ed_MinDepth: TEdit
      Left = 176
      Top = 131
      Width = 41
      Height = 22
      Enabled = False
      TabOrder = 4
      Text = '0'
      OnChange = ed_MinDepthChange
    end
    object ed_MaxDepth: TEdit
      Left = 224
      Top = 131
      Width = 41
      Height = 22
      Hint = 'empty for no depth limit'
      Enabled = False
      TabOrder = 5
      Text = '0'
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 551
    Width = 689
    Height = 37
    Align = alBottom
    TabOrder = 7
    DesignSize = (
      689
      37)
    object btnOK: TButton
      Left = 444
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 524
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnHelp: TButton
      Left = 604
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Help'
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnSearch: TButton
      Left = 89
      Top = 3
      Width = 75
      Height = 25
      Caption = 'Search'
      TabOrder = 1
      Visible = False
      OnClick = btnOKClick
    end
    object btnOptions: TButton
      Left = 8
      Top = 3
      Width = 75
      Height = 25
      Caption = 'Options'
      TabOrder = 0
      OnClick = btnOptionsClick
    end
  end
  object rgSaveOption: TRadioGroup
    Left = 8
    Top = 460
    Width = 674
    Height = 87
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
