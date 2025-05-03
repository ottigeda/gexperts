inherited foBookmarksEdit: TfoBookmarksEdit
  Left = 341
  Top = 434
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Bookmark'
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
    Left = 6
    Top = 99
    Width = 16
    Height = 16
    Hint = 'Help (F1)'
    Anchors = [akLeft, akBottom]
    ParentShowHint = False
    Picture.Data = {
      07544269746D6170D2000000424DD20000000000000052000000280000001000
      000010000000010004000000000080000000C40E0000C40E0000070000000700
      0000FFFF010001010100FFFFFF008181810081010100FF010100C1C1C1000000
      0000000100000000000000110000000033333121300000011111122113000012
      2222222221300012322222542130001245222254213000124522225421300012
      4555225421300012444425542130001245226454213000124555224421300012
      344422242130001222222222210000011111111110000000000000000000}
    ShowHint = True
    Transparent = True
    OnClick = imF1Click
  end
  object imF1NA: TImage
    Left = 14
    Top = 91
    Width = 16
    Height = 16
    Hint = 'Help (F1)'
    Anchors = [akLeft, akBottom]
    ParentShowHint = False
    Picture.Data = {
      07544269746D6170C2000000424DC20000000000000042000000280000001000
      000010000000010004000000000080000000C40E0000C40E0000030000000300
      0000FFFF010081818100F1FBFF00000000000002000000000000001200000000
      2222210122000001111110011200001200000000012000120120001201200012
      0120001201200012012200120120001201110012012000120120001201200012
      0122011201200012011100100120001200000000012000122222222221000001
      1111111110000000000000000000}
    ShowHint = True
    Transparent = True
    Visible = False
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
    Left = 28
    Top = 94
    Width = 151
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
