object fmFavOptions: TfmFavOptions
  Left = 279
  Top = 208
  Caption = 'Favorite Files Options'
  ClientHeight = 386
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object l_FileFilters: TLabel
    Left = 8
    Top = 160
    Width = 52
    Height = 14
    Caption = 'File Filters'
  end
  object gbxFavOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 489
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Favorite Files Dialog Options'
    TabOrder = 0
    object chkConfirmFolderDelete: TCheckBox
      Left = 10
      Top = 21
      Width = 471
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Confirm on folder delete'
      TabOrder = 0
    end
    object chkExpandAllOnLoad: TCheckBox
      Left = 10
      Top = 39
      Width = 471
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Expand all on load'
      TabOrder = 1
    end
    object chkHideOnExecute: TCheckBox
      Left = 10
      Top = 58
      Width = 471
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Hide window when executing a file'
      TabOrder = 2
    end
    object chkShowPreview: TCheckBox
      Left = 10
      Top = 76
      Width = 471
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Show file preview pane'
      TabOrder = 3
    end
  end
  object btnOK: TButton
    Left = 344
    Top = 352
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 424
    Top = 352
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chk_InsertFavMenu: TCheckBox
    Left = 16
    Top = 128
    Width = 473
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Insert a Favorites Entry in the File Menu'
    TabOrder = 1
  end
  object sg_FileFilters: TStringGrid
    Left = 8
    Top = 176
    Width = 489
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultColWidth = 50
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 2
    OnEnter = sg_FileFiltersEnter
    OnExit = sg_FileFiltersExit
    OnKeyPress = sg_FileFiltersKeyPress
  end
  object b_Default: TButton
    Left = 8
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Default'
    TabOrder = 3
    OnClick = b_DefaultClick
  end
end
