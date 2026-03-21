inherited foBookmarksEdit: TfoBookmarksEdit
  Left = 1356
  Top = 178
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
    TabOrder = 4
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
    TabOrder = 5
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
    TabOrder = 3
    OnChange = coModuleChange
  end
  object buOk: TButton
    Left = 75
    Top = 94
    Width = 106
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
  object buHelp: TButton
    Left = 8
    Top = 94
    Width = 60
    Height = 25
    Hint = 'Help (F1)'
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = buHelpClick
  end
end
