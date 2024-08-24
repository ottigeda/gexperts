inherited fmProjDependOptions: TfmProjDependOptions
  BorderStyle = bsDialog
  Caption = 'Project Dependencies Configuration'
  ClientHeight = 233
  ClientWidth = 545
  PixelsPerInch = 96
  TextHeight = 13
  object l_ScanEntireUnitBla: TLabel
    Left = 32
    Top = 24
    Width = 505
    Height = 41
    AutoSize = False
    Caption = 
      '(This is slower, but will also find additional, unusual uses cla' +
      'uses that have been conditionally defined.)'
    WordWrap = True
  end
  object l_ScanBrowsingPath: TLabel
    Left = 32
    Top = 152
    Width = 505
    Height = 41
    AutoSize = False
    Caption = 
      '(This will find sources for units that are only included as DCU ' +
      'files in the library path.)'
    WordWrap = True
  end
  object l_SearchLibraryPath: TLabel
    Left = 32
    Top = 88
    Width = 505
    Height = 41
    AutoSize = False
    Caption = '(This will find sources for units in the library path.)'
    WordWrap = True
  end
  object chk_ScanEntireUnit: TCheckBox
    Left = 8
    Top = 8
    Width = 529
    Height = 17
    Caption = 'Scan entire unit'
    TabOrder = 0
  end
  object chk_SearchBrowsingPath: TCheckBox
    Left = 8
    Top = 136
    Width = 529
    Height = 17
    Caption = 'Also search Browsing Path'
    TabOrder = 1
  end
  object b_OK: TButton
    Left = 384
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 464
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object chk_SearchLibraryPath: TCheckBox
    Left = 8
    Top = 72
    Width = 529
    Height = 17
    Caption = 'Search Library Path'
    TabOrder = 4
  end
end
