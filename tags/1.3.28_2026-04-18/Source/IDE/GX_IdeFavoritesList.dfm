inherited f_GxIdeFavoritesList: Tf_GxIdeFavoritesList
  Left = 2360
  Top = 276
  BorderIcons = [biSystemMenu]
  Caption = 'Favorite Search Paths'
  ClientHeight = 313
  ClientWidth = 417
  ParentFont = False
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object b_Close: TButton
    Left = 336
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 6
  end
  object lv_Favorites: TListView
    Left = 8
    Top = 8
    Width = 321
    Height = 297
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Value'
        Width = 200
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lv_FavoritesDblClick
  end
  object b_Add: TButton
    Left = 336
    Top = 24
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add ...'
    TabOrder = 1
    OnClick = b_AddClick
  end
  object b_Edit: TButton
    Left = 336
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Edit ...'
    Default = True
    TabOrder = 2
    OnClick = b_EditClick
  end
  object b_Delete: TButton
    Left = 336
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Delete'
    TabOrder = 3
    OnClick = b_DeleteClick
  end
  object b_Help: TButton
    Left = 336
    Top = 280
    Width = 75
    Height = 25
    Hint = 'Help (F1)'
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Help'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = b_HelpClick
  end
  object b_Export: TButton
    Left = 336
    Top = 136
    Width = 75
    Height = 25
    Caption = 'E&xport ...'
    TabOrder = 4
    OnClick = b_ExportClick
  end
  object b_Import: TButton
    Left = 336
    Top = 168
    Width = 75
    Height = 25
    Caption = '&Import ...'
    TabOrder = 5
    OnClick = b_ImportClick
  end
end
