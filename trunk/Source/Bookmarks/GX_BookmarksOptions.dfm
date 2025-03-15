inherited foBookmarksOptions: TfoBookmarksOptions
  Left = 1213
  Top = 213
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 292
  ClientWidth = 260
  Constraints.MinHeight = 324
  Constraints.MinWidth = 270
  ParentFont = False
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imF1: TImage
    Left = 6
    Top = 264
    Width = 16
    Height = 16
    Hint = 'Help (F1)'
    Anchors = [akLeft, akRight, akBottom]
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
  object buSave: TButton
    Left = 28
    Top = 259
    Width = 156
    Height = 25
    Hint = 'Save options and close form (Return)'
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Save'
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
    TabOrder = 0
  end
  object buCancel: TButton
    Left = 192
    Top = 259
    Width = 60
    Height = 25
    Hint = 'Close form (Escape)'
    Anchors = [akLeft, akRight, akBottom]
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
  object cbUseOldEditForm: TCheckBox
    Left = 8
    Top = 7
    Width = 100
    Height = 17
    Caption = 'use old edit form'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object cbEnlargedLineDisplay: TCheckBox
    Left = 8
    Top = 28
    Width = 120
    Height = 17
    Caption = 'enlarged line display'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object cbRemoveLeadingBlanks: TCheckBox
    Left = 8
    Top = 49
    Width = 130
    Height = 17
    Caption = 'remove leading blanks'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object paColorBase: TPanel
    Left = 0
    Top = 110
    Width = 260
    Height = 145
    BevelOuter = bvNone
    Caption = 'paColorBase'
    Color = clAqua
    TabOrder = 5
    DesignSize = (
      260
      145)
    object laListboxColors1: TLabel
      Left = 8
      Top = 3
      Width = 170
      Height = 29
      AutoSize = False
      Caption = 'Listbox background color and bookmark highlighting ...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object laListboxColors2: TLabel
      Left = 8
      Top = 91
      Width = 166
      Height = 26
      AutoSize = False
      Caption = 'Background and font color of bookmark header ...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object paLBcolorBkg: TPanel
      Left = 8
      Top = 33
      Width = 244
      Height = 48
      Hint = 'Select color with DblClick xxx'
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      Caption = 'paLBcolorBkg'
      Color = clSilver
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnDblClick = paLBcolorBkDblClick
      OnMouseDown = paLBcolorBkMouseDown
    end
    object paLBcolorBMline: TPanel
      Left = 3
      Top = 49
      Width = 254
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvLowered
      Color = clYellow
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnDblClick = paLBcolorBMlineDblClick
      OnMouseDown = paLBcolorBMlineMouseDown
    end
    object paHeaderBkColor: TPanel
      Left = 8
      Top = 121
      Width = 244
      Height = 20
      Hint = 'Select color with DblClick xxx'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnDblClick = paHeaderXXXDblClick
      OnMouseDown = paHeaderXXXColorMouseDown
      object laHeaderFontColor: TLabel
        Left = 79
        Top = 3
        Width = 87
        Height = 13
        Alignment = taCenter
        Caption = 'FuncProcName'
        OnDblClick = paHeaderXXXDblClick
        OnMouseDown = paHeaderXXXColorMouseDown
      end
    end
  end
  object cbHideClassName: TCheckBox
    Left = 8
    Top = 70
    Width = 205
    Height = 17
    Caption = 'hide class name in bookmarks header'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object cbHideLBFocus: TCheckBox
    Left = 8
    Top = 91
    Width = 105
    Height = 17
    Caption = 'hide listbox focus'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object ColorDialog: TColorDialog
    OnShow = ColorDialogShow
    Options = [cdFullOpen]
    Left = 181
    Top = 13
  end
end
