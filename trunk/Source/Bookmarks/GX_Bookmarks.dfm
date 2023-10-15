inherited fmGxBookmarksForm: TfmGxBookmarksForm
  Caption = 'Bookmarks'
  PixelsPerInch = 96
  TextHeight = 14
  object p_Bookmarks: TPanel
    Left = 0
    Top = 0
    Width = 304
    Height = 204
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    TabOrder = 0
    object lb_Bookmarks: TListBox
      Left = 1
      Top = 1
      Width = 302
      Height = 202
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 64
      PopupMenu = pm_Bookmarks
      TabOrder = 0
      OnDblClick = lb_BookmarksDblClick
      OnDrawItem = lb_BookmarksDrawItem
      OnKeyPress = lb_BookmarksKeyPress
    end
  end
  object tim_Update: TTimer
    OnTimer = tim_UpdateTimer
    Left = 144
    Top = 104
  end
  object pm_Bookmarks: TPopupMenu
    Left = 232
    Top = 104
    object mi_Edit: TMenuItem
      Caption = 'Edit...'
      OnClick = mi_EditClick
    end
    object mi_Add: TMenuItem
      Caption = 'Add...'
      OnClick = mi_AddClick
    end
    object mi_Delete: TMenuItem
      Caption = 'Delete'
      OnClick = mi_DeleteClick
    end
    object mi_DeleteAll: TMenuItem
      Caption = 'Delete All'
      ShortCut = 16452
      OnClick = mi_DeleteAllClick
    end
  end
end
