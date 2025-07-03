inherited fmUsesExpertOptions: TfmUsesExpertOptions
  Left = 2441
  Top = 213
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Uses Clause Manager Configuration'
  ClientHeight = 330
  ClientWidth = 329
  ParentFont = False
  Font.Name = 'Tahoma'
  DesignSize = (
    329
    330)
  PixelsPerInch = 96
  TextHeight = 13
  object imHelp: TImage
    Left = 11
    Top = 302
    Width = 16
    Height = 16
    Picture.Data = {
      07544269746D617036030000424D360300000000000036000000280000001000
      0000100000000100180000000000000300000000000000000000000000000000
      0000FF00FFFF00FFFF00FFFF00FF9C9C9C6B6B6B5252524A4A4A4A4A4A4A4A4A
      5252529C9C9CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADE7CE
      C6EFDED6F7E7D6F7E7D6EFDED6EFDED6CEBDB55A5A5A7373739C9C9CFF00FFFF
      00FFFF00FFFF00FFCEC6BDF7E7D6F7F7F7E7E7E7CEB5A5D6AD94DEC6BDEFF7F7
      F7EFEFEFDED68C847B6B6B6B9C9C9CFF00FFFF00FFE7D6CEF7E7DEF7FFFFCE9C
      84B54A10BD6339D6AD9CC65A21BD5221D6AD9CF7FFFFF7DED67B736B737373FF
      00FFFF00FFEFDED6F7FFFFC67B5ABD4A10C65218C6A594FFFFFFDE9473BD4A10
      B54A10CE9C84F7FFFFEFDED65A5A5A9C9C9CF7E7E7F7F7F7D6AD94BD4A10CE63
      31CE6329CE6B39DE8C6BCE6329CE6331C65A29B54A10DEC6BDF7EFE7A59C9463
      6363F7EFE7F7F7F7C66331C65A29CE6B31CE5A21CE8C6BF7E7DECE6B39C65A21
      CE6331C65218C67B52F7FFFFDECEC6525252F7EFE7EFDED6C65A21CE6331CE63
      31CE5A21C6846BFFFFFFEFAD94C64A10CE6331CE6329C65A29F7EFEFEFDED652
      5252F7EFE7F7DECECE5A21CE6331CE6331CE6329C65221CEB5A5FFFFFFE79C7B
      C65218CE6329C65A29F7EFEFEFDED6525252F7EFE7F7EFE7DE6B31D66B31CE63
      29C65A21C65218BD4A10DECEC6FFFFFFD67342CE5A21CE6B39FFF7F7EFDED66B
      6B6BF7E7E7FFFFFFF79C6BE76329CE8C6BEFE7DED67B52BD3100D69C7BFFFFFF
      DE8C63CE5218E79C73FFFFFFDEC6BDADADADF7EFE7F7EFEFFFEFDEFF8C4ADE84
      5AEFFFFFFFFFFFE7BDA5F7FFFFEFFFFFE77339E77339FFEFEFF7E7DEA59C94FF
      00FFFF00FFEFDED6FFFFFFFFEFCEFFB573EFAD84EFE7DEEFF7F7EFE7DEF7A57B
      FF8C4AFFDECEFFFFFFEFDED6CECEC6FF00FFFF00FFF7EFEFEFDED6FFFFFFFFFF
      FFFFF7C6FFDEADFFCE94FFCE94FFD6ADFFF7F7FFFFFFEFDED6CECEC6FF00FFFF
      00FFFF00FFFF00FFF7EFEFEFDED6F7E7E7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      EFE7DEEFDED6DEDEDEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFF7EFE7F7F7
      EFF7EFEFF7EFEFF7EFEFF7EFEFF7EFEFF7EFEFF7EFE7FF00FFFF00FFFF00FFFF
      00FF}
    Transparent = True
    OnClick = imHelpClick
  end
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
    Left = 160
    Top = 296
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object btnCancel: TButton
    Left = 240
    Top = 296
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
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
    Caption = 'Show identifiers matching search string ...'
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
end
