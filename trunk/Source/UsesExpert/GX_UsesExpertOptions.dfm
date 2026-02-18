inherited fmUsesExpertOptions: TfmUsesExpertOptions
  Left = 338
  Top = 241
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Uses Clause Manager, Configuration'
  ClientHeight = 354
  ClientWidth = 329
  ParentFont = True
  KeyPreview = True
  OnKeyDown = FormKeyDown
  DesignSize = (
    329
    354)
  PixelsPerInch = 96
  TextHeight = 13
  object chkReadMap: TCheckBox
    Left = 8
    Top = 8
    Width = 313
    Height = 17
    Hint = 
      'Requires that you turn on map file generation in the project'#39's l' +
      'inker options.'
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Read project units from map file rather than .dpr'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object chkReplaceFileUnit: TCheckBox
    Left = 8
    Top = 32
    Width = 313
    Height = 17
    Hint = 
      'If enabled, the menu entry File -> Use Unit will call the GExper' +
      'ts Uses Clause Manager'
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replace IDE File, Use Unit feature'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object chkParseAll: TCheckBox
    Left = 8
    Top = 56
    Width = 273
    Height = 17
    Caption = 'Parse all units, not just the Favorites'
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 164
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object btnCancel: TButton
    Left = 246
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object chkDisableParserCache: TCheckBox
    Left = 24
    Top = 80
    Width = 257
    Height = 17
    Caption = 'Disable Parser Cache'
    TabOrder = 3
  end
  object btnClearCache: TButton
    Left = 40
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Clear Cache'
    TabOrder = 4
    OnClick = btnClearCacheClick
  end
  object rg_FilterIdentifiers: TRadioGroup
    Left = 8
    Top = 168
    Width = 313
    Height = 81
    Caption = ' Show identifiers matching search string ... '
    Items.Strings = (
      'at start only'
      'anywhere'
      'at start first then anywhere')
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
  end
  object chkSearchPathFavorites: TCheckBox
    Left = 8
    Top = 136
    Width = 321
    Height = 17
    Caption = 'Also search the favorites configured for the search path'
    TabOrder = 5
  end
  object chk_FastAdd: TCheckBox
    Left = 8
    Top = 264
    Width = 313
    Height = 17
    Hint = 'Pressing Enter adds to current uses list and exits the dialog.'
    Caption = 'Default to Fast Add'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
  end
  object chk_AutoSort: TCheckBox
    Left = 8
    Top = 288
    Width = 313
    Height = 17
    Hint = 'When closing with OK, sort and group the uses clauses by namespace prefix.'
    Caption = 'Default to sorting and grouping the uses clauses'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
  end
  object b_Help: TButton
    Left = 8
    Top = 320
    Width = 75
    Height = 25
    Hint = 'Help (F1)'
    Caption = 'Help'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
    OnClick = b_HelpClick
  end
end
