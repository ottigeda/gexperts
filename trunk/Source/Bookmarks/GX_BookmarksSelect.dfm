inherited foBookmarksSelect: TfoBookmarksSelect
  Left = 240
  Top = 240
  Anchors = [akTop, akRight]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select function or procedure'
  ClientHeight = 428
  ClientWidth = 466
  Constraints.MaxHeight = 900
  Constraints.MaxWidth = 640
  Constraints.MinHeight = 280
  Constraints.MinWidth = 320
  ParentFont = False
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object laModule: TLabel
    Left = 9
    Top = 9
    Width = 400
    Height = 13
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'NT_History.pas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object imF1: TImage
    Left = 385
    Top = 4
    Width = 16
    Height = 16
    Hint = 'Help (F1)'
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
    Left = 393
    Top = 11
    Width = 16
    Height = 16
    Hint = 'Help (F1)'
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
  object buSelect: TButton
    Left = 7
    Top = 375
    Width = 384
    Height = 25
    Hint = 'Set bookmark and close form (Return)'
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Select'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object buCancel: TButton
    Left = 399
    Top = 375
    Width = 59
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
    TabOrder = 2
  end
  object lbFuncsProcs: TListBox
    Left = 8
    Top = 31
    Width = 450
    Height = 336
    Style = lbOwnerDrawFixed
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ItemHeight = 15
    Items.Strings = (
      '1'
      '1'
      '2'
      '2'
      '3'
      '3'
      '4'
      '4'
      '5'
      '5'
      '6'
      '6'
      '7'
      '7'
      '8'
      '8'
      '9'
      '9'
      '0'
      '0'
      '1'
      '1'
      '2'
      '2'
      '3'
      '3'
      '4'
      '4'
      '5'
      '5'
      '6'
      '6')
    ParentFont = False
    TabOrder = 0
    OnClick = lbFuncsProcsClick
    OnDblClick = lbFuncsProcsDblClick
    OnDrawItem = lbFuncsProcsDrawItem
  end
  object SBar: TStatusBar
    Left = 0
    Top = 406
    Width = 466
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Panels = <
      item
        Bevel = pbNone
        Width = 20
      end
      item
        Text = ' No functions or procedures found'
        Width = 281
      end
      item
        Alignment = taCenter
        Text = '>wwwwwwww<'
        Width = 90
      end
      item
        Text = 'Sort name'
        Width = 92
      end>
    ParentShowHint = False
    ShowHint = True
    UseSystemFont = False
    OnResize = SBarResize
  end
  object ToolBar: TToolBar
    Left = 435
    Top = 5
    Width = 29
    Height = 26
    Align = alCustom
    Anchors = [akTop, akRight]
    Caption = 'ToolBar'
    Color = clSilver
    EdgeBorders = []
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 4
    object tbSort: TToolButton
      Left = 0
      Top = 0
      Hint = 'Toggle sort mode  Line-Type-Name'
      Caption = 'tbSort'
      ImageIndex = 0
      ParentShowHint = False
      ShowHint = True
      OnClick = tbSortClick
    end
  end
  object paNoMatch: TPanel
    Left = 131
    Top = 166
    Width = 212
    Height = 60
    Anchors = []
    BevelOuter = bvNone
    Caption = 'No match, reset mask'
    Color = clYellow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    Visible = False
  end
end
