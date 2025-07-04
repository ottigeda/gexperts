inherited foBookmarksEdit: TfoBookmarksEdit
  Left = 888
  Top = 33
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Editor Bookmarks, Edit'
  ClientHeight = 127
  ClientWidth = 256
  Constraints.MaxHeight = 159
  Constraints.MaxWidth = 266
  Constraints.MinHeight = 159
  Constraints.MinWidth = 266
  ParentFont = False
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object laBookmarkIdx: TLabel
    Left = 160
    Top = 47
    Width = 48
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Bookmark'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object laModule: TLabel
    Left = 8
    Top = 4
    Width = 35
    Height = 13
    Caption = 'Module'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object laLine: TLabel
    Left = 8
    Top = 47
    Width = 20
    Height = 13
    Caption = 'Line'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object imF1: TImage
    Left = 8
    Top = 99
    Width = 16
    Height = 16
    Hint = 'Help (F1)'
    Anchors = [akLeft, akBottom]
    ParentShowHint = False
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
    ShowHint = True
    Transparent = True
    OnClick = imF1Click
  end
  object edLine: TEdit
    Left = 8
    Top = 63
    Width = 144
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnChange = edLineChange
  end
  object coBookmarkIdx: TComboBox
    Left = 160
    Top = 63
    Width = 88
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    DropDownCount = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 4
    OnChange = coBookmarkIdxChange
  end
  object coModule: TComboBox
    Left = 8
    Top = 20
    Width = 240
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 2
    OnChange = coModuleChange
  end
  object buOk: TButton
    Left = 30
    Top = 94
    Width = 149
    Height = 25
    Hint = 'Save bookmark and close form (Return)'
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Save'
    Default = True
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object buCancel: TButton
    Left = 188
    Top = 94
    Width = 60
    Height = 25
    Hint = 'Close form (Escape)'
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
end
