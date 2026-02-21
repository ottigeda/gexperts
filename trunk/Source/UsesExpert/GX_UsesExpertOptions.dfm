inherited fmUsesExpertOptions: TfmUsesExpertOptions
  Left = 338
  Top = 241
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Uses Clause Manager, Configuration'
  ClientHeight = 593
  ClientWidth = 329
  ParentFont = True
  KeyPreview = True
  OnKeyDown = FormKeyDown
  DesignSize = (
    329
    593)
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
    Left = 145
    Top = 559
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 10
  end
  object btnCancel: TButton
    Left = 236
    Top = 559
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 11
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
    Width = 85
    Height = 25
    Caption = 'Clear Cache'
    TabOrder = 4
    OnClick = btnClearCacheClick
  end
  object rg_FilterIdentifiers: TRadioGroup
    Left = 8
    Top = 168
    Width = 313
    Height = 68
    Anchors = [akLeft, akTop, akRight]
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
    Width = 313
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Also search the favorites configured for the search path'
    TabOrder = 5
  end
  object chk_FastAdd: TCheckBox
    Left = 8
    Top = 251
    Width = 313
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Hint = 'Pressing Enter adds to current uses list and exits the dialog.'
    Caption = 'Default to Fast Add'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
  end
  object chk_AutoSort: TCheckBox
    Left = 8
    Top = 275
    Width = 313
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Hint = 'When closing with OK, sort and group the uses clauses by namespace prefix.'
    Caption = 'Default to sorting and grouping the uses clauses'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
  end
  object grp_SortSettings: TGroupBox
    Left = 8
    Top = 299
    Width = 313
    Height = 252
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Sorting and Grouping '
    TabOrder = 9
    object lb_Prefixes: TListBox
      Left = 8
      Top = 18
      Width = 204
      Height = 120
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnClick = lb_PrefixesClick
    end
    object btnPrefixAdd: TButton
      Left = 220
      Top = 18
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Add...'
      TabOrder = 1
      OnClick = btnPrefixAddClick
    end
    object btnPrefixEdit: TButton
      Left = 220
      Top = 46
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Edit...'
      TabOrder = 2
      OnClick = btnPrefixEditClick
    end
    object btnPrefixDelete: TButton
      Left = 220
      Top = 74
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      TabOrder = 3
      OnClick = btnPrefixDeleteClick
    end
    object btnPrefixUp: TButton
      Left = 220
      Top = 106
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Up'
      TabOrder = 4
      OnClick = btnPrefixUpClick
    end
    object btnPrefixDown: TButton
      Left = 220
      Top = 134
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Down'
      TabOrder = 5
      OnClick = btnPrefixDownClick
    end
    object btnPrefixReset: TButton
      Left = 8
      Top = 144
      Width = 120
      Height = 25
      Caption = 'Reset to Defaults'
      TabOrder = 6
      OnClick = btnPrefixResetClick
    end
    object rg_Separator: TRadioGroup
      Left = 8
      Top = 176
      Width = 297
      Height = 68
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Group separator '
      Items.Strings = (
        'None'
        'Blank line'
        'Comment (e.g. { Vcl })')
      TabOrder = 7
    end
  end
  object b_Help: TButton
    Left = 8
    Top = 559
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Hint = 'Help (F1)'
    Caption = 'Help'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    OnClick = b_HelpClick
  end
end
