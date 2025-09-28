inherited foBookmarksOptions: TfoBookmarksOptions
  Left = 281
  Top = 540
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Editor Bookmarks, Configuration'
  ClientHeight = 316
  ClientWidth = 260
  ParentFont = False
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object buSave: TButton
    Left = 75
    Top = 283
    Width = 110
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
    Top = 283
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
    TabOrder = 3
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
    TabOrder = 4
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
    TabOrder = 5
  end
  object paColorBase: TPanel
    Left = 0
    Top = 134
    Width = 260
    Height = 145
    BevelOuter = bvNone
    TabOrder = 6
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
      Tag = 1
      Left = 8
      Top = 33
      Width = 244
      Height = 48
      Hint = 'Select color with DblClick xxx'
      Anchors = [akLeft, akTop, akRight]
      Caption = 'paLBcolorBkg'
      Color = clSilver
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnDblClick = paLBcolorBkDblClick
      OnMouseDown = paLBcolorBkMouseDown
    end
    object paLBcolorBMline: TPanel
      Tag = 1
      Left = 3
      Top = 49
      Width = 254
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Color = clYellow
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnDblClick = paLBcolorBMlineDblClick
      OnMouseDown = paLBcolorBMlineMouseDown
    end
    object paHeaderBkColor: TPanel
      Tag = 1
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
    TabOrder = 7
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
    TabOrder = 8
  end
  object cbDenyDoubleBookmarks: TCheckBox
    Left = 8
    Top = 112
    Width = 136
    Height = 17
    Caption = 'deny double bookmarks'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
  end
  object buHelp: TButton
    Left = 8
    Top = 283
    Width = 60
    Height = 25
    Hint = 'Help (F1)'
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
  object ColorDialog: TColorDialog
    OnShow = ColorDialogShow
    Options = [cdFullOpen]
    Left = 180
    Top = 13
  end
end
