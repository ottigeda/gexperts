inherited f_IdeSearchPathFavoriteEdit: Tf_IdeSearchPathFavoriteEdit
  Left = 100
  Top = 100
  BorderIcons = [biSystemMenu]
  Caption = 'Favorite Search Path'
  ClientHeight = 233
  ClientWidth = 465
  ParentFont = False
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  DesignSize = (
    465
    233)
  PixelsPerInch = 96
  TextHeight = 13
  object l_Name: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object l_Path: TLabel
    Left = 8
    Top = 56
    Width = 22
    Height = 13
    Caption = 'Path'
  end
  object ed_Name: TEdit
    Left = 8
    Top = 24
    Width = 449
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 304
    Top = 200
    Width = 75
    Height = 25
    Action = act_OK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 7
  end
  object b_Cancel: TButton
    Left = 384
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 8
    OnClick = b_CancelClick
  end
  object b_Select: TButton
    Left = 120
    Top = 200
    Width = 105
    Height = 25
    Action = act_SelectDirectory
    Anchors = [akLeft, akBottom]
    TabOrder = 6
  end
  object b_Help: TButton
    Left = 8
    Top = 200
    Width = 75
    Height = 25
    Hint = 'Help (F1)'
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    OnClick = b_HelpClick
  end
  object b_MakeRelative: TButton
    Left = 368
    Top = 56
    Width = 88
    Height = 25
    Action = act_MakeRelative
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object b_MakeAbsolute: TButton
    Left = 368
    Top = 88
    Width = 88
    Height = 25
    Action = act_MakeAbsolute
    Anchors = [akRight, akBottom]
    TabOrder = 3
  end
  object b_PrependDots: TButton
    Left = 368
    Top = 128
    Width = 88
    Height = 25
    Action = act_PrependDots
    Anchors = [akRight, akBottom]
    TabOrder = 4
  end
  object b_RemoveDots: TButton
    Left = 368
    Top = 160
    Width = 88
    Height = 25
    Action = act_RemoveDots
    Anchors = [akRight, akBottom]
    TabOrder = 5
  end
  object p_Memo: TPanel
    Left = 8
    Top = 72
    Width = 353
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Memo created at runtime'
    TabOrder = 1
  end
  object TheActionList: TActionList
    Left = 280
    Top = 124
    object act_MakeRelative: TAction
      Caption = 'Make &Relative'
      OnExecute = act_MakeRelativeExecute
    end
    object act_MakeAbsolute: TAction
      Caption = 'Make &Absolute'
      OnExecute = act_MakeAbsoluteExecute
    end
    object act_PrependDots: TAction
      Caption = '&Prepend ..\'
      OnExecute = act_PrependDotsExecute
    end
    object act_RemoveDots: TAction
      Caption = 'Remove ..\'
      OnExecute = act_RemoveDotsExecute
    end
    object act_SelectDirectory: TAction
      Caption = '&Select directory'
      OnExecute = act_SelectDirectoryExecute
    end
    object act_OK: TAction
      Caption = '&OK'
      ShortCut = 16397
      OnExecute = act_OkExecute
    end
    object act_Cancel: TAction
      Caption = '&Cancel'
      ShortCut = 27
      OnExecute = act_CancelExecute
    end
  end
  object pm_Memo: TPopupMenu
    Left = 280
    Top = 61
    object mi_MemoMoveUp: TMenuItem
      Caption = 'Move Up'
    end
    object mi_MemoMoveDown: TMenuItem
      Caption = 'Move Down'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mi_MemoPrependDots: TMenuItem
      Action = act_PrependDots
    end
    object mi_MemoRemoveDots: TMenuItem
      Action = act_RemoveDots
    end
    object mi_MemoMakeRelative: TMenuItem
      Action = act_MakeRelative
    end
    object mi_MemoMakeAbsolute: TMenuItem
      Action = act_MakeAbsolute
    end
  end
end
