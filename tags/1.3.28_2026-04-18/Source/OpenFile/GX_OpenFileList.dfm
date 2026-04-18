object frOpenFileList: TfrOpenFileList
  Left = 0
  Top = 0
  Width = 334
  Height = 215
  TabOrder = 0
  object pnlList: TPanel
    Left = 0
    Top = 0
    Width = 334
    Height = 182
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    FullRepaint = False
    TabOrder = 0
    object ListView: TListView
      Left = 3
      Top = 3
      Width = 328
      Height = 176
      Align = alClient
      Columns = <>
      DragMode = dmAutomatic
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 182
    Width = 334
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnAddToFavorites: TButton
      Left = 4
      Top = 3
      Width = 135
      Height = 25
      Caption = '&Add to Favorites'
      TabOrder = 0
    end
  end
end
