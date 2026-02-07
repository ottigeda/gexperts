inherited f_EditPath: Tf_EditPath
  Left = 2386
  Top = 230
  BorderIcons = [biSystemMenu]
  Caption = 'Edit Unit Search Path'
  ClientHeight = 352
  ClientWidth = 627
  KeyPreview = True
  Position = poDesigned
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object spVer: TSplitter
    Left = 200
    Top = 0
    Width = 8
    Height = 316
    Hint = 'Toggle MinSize / Center / MaxSize with DblClick'
    MinSize = 200
    ResizeStyle = rsUpdate
  end
  object p_BottomButtons: TPanel
    Left = 0
    Top = 316
    Width = 627
    Height = 36
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      627
      36)
    object b_Cancel: TButton
      Left = 551
      Top = 6
      Width = 70
      Height = 24
      Action = act_Cancel
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 1
    end
    object b_OK: TButton
      Left = 475
      Top = 6
      Width = 70
      Height = 24
      Action = act_OK
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 0
    end
    object b_Help: TButton
      Left = 6
      Top = 6
      Width = 70
      Height = 24
      Hint = 'Help (F1)'
      Caption = 'Help'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = b_HelpClick
    end
  end
  object p_Client: TPanel
    Left = 208
    Top = 0
    Width = 419
    Height = 316
    Align = alClient
    TabOrder = 0
    object spHor: TSplitter
      Left = 1
      Top = 201
      Width = 417
      Height = 8
      Cursor = crVSplit
      Hint = 'Toggle MinSize / Center / MaxSize with DblClick'
      Align = alTop
      MinSize = 200
      ResizeStyle = rsUpdate
    end
    object p_ClientTop: TPanel
      Left = 1
      Top = 1
      Width = 417
      Height = 200
      Align = alTop
      Caption = 'Memo goes here'
      TabOrder = 0
      object p_MemoButtons: TPanel
        Left = 316
        Top = 21
        Width = 100
        Height = 178
        Align = alRight
        TabOrder = 0
        DesignSize = (
          100
          178)
        object sb_MoveUp: TSpeedButton
          Left = 6
          Top = 2
          Width = 25
          Height = 23
          Action = act_MoveUp
          Glyph.Data = {
            B6080000424DB608000000000000360400002800000030000000180000000100
            0800000000008004000000000000000000000001000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
            A6000020400000206000002080000020A0000020C0000020E000004000000040
            20000040400000406000004080000040A0000040C0000040E000006000000060
            20000060400000606000006080000060A0000060C0000060E000008000000080
            20000080400000806000008080000080A0000080C0000080E00000A0000000A0
            200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
            200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
            200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
            20004000400040006000400080004000A0004000C0004000E000402000004020
            20004020400040206000402080004020A0004020C0004020E000404000004040
            20004040400040406000404080004040A0004040C0004040E000406000004060
            20004060400040606000406080004060A0004060C0004060E000408000004080
            20004080400040806000408080004080A0004080C0004080E00040A0000040A0
            200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
            200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
            200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
            20008000400080006000800080008000A0008000C0008000E000802000008020
            20008020400080206000802080008020A0008020C0008020E000804000008040
            20008040400080406000804080008040A0008040C0008040E000806000008060
            20008060400080606000806080008060A0008060C0008060E000808000008080
            20008080400080806000808080008080A0008080C0008080E00080A0000080A0
            200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
            200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
            200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
            2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
            2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
            2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
            2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
            2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
            2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
            2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4EFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEF0404040404A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFC04A4EFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFFCFCFCFC04A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFC04A4EFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFFCFCFCFC04A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFC04A4A4A4EFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFFCFCFCFCFCFCFCFCFCEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4A4A4
            A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFCFCFCFCEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFA4A4A4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFFCFCFCFCFCEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFA4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFFCEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4EFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
            EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          Spacing = 0
        end
        object sb_MoveDown: TSpeedButton
          Left = 6
          Top = 25
          Width = 25
          Height = 23
          Action = act_MoveDown
          Glyph.Data = {
            86020000424D8602000000000000460000002800000030000000180000000100
            04000000000040020000C40E0000C40E00000400000004000000C0A0E000FF00
            0000800000008080800000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000003
            0000000000000000000000000000000000000000000000023000000000000000
            0000003000000000000000000000001123000000000000000000033300000000
            0000000000000111123000000000000000003333300000000000000000001111
            1123000000000000000333333300000000000000000111111222300000000000
            0033333333300000000000000000011112300000000000000000333330000000
            0000000000000111123000000000000000003333300000000000000000000111
            1230000000000000000033333000000000000000000001111230000000000000
            0000333330000000000000000000011112300000000000000000333330000000
            0000000000000111120000000000000000003333300000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          Spacing = 0
        end
        object b_Favorites: TButton
          Left = 69
          Top = 2
          Width = 25
          Height = 23
          Hint = 'Favorites'
          Caption = '&Fav'
          ParentShowHint = False
          PopupMenu = pm_Favorites
          ShowHint = True
          TabOrder = 0
          OnClick = b_FavoritesClick
        end
        object b_MakeRelative: TButton
          Left = 6
          Top = 66
          Width = 88
          Height = 24
          Action = act_MakeRelative
          Anchors = [akRight, akBottom]
          TabOrder = 1
        end
        object b_MakeAbsolute: TButton
          Left = 6
          Top = 92
          Width = 88
          Height = 24
          Action = act_MakeAbsolute
          Anchors = [akRight, akBottom]
          TabOrder = 2
        end
        object b_PrependDots: TButton
          Left = 6
          Top = 122
          Width = 88
          Height = 24
          Action = act_PrependDots
          Anchors = [akRight, akBottom]
          TabOrder = 3
        end
        object b_RemoveDots: TButton
          Left = 6
          Top = 149
          Width = 88
          Height = 24
          Action = act_RemoveDots
          Anchors = [akRight, akBottom]
          TabOrder = 4
        end
      end
      object p_MemoCaption: TPanel
        Left = 1
        Top = 1
        Width = 415
        Height = 20
        Align = alTop
        TabOrder = 1
        object l_UnitSearchPath: TLabel
          Left = 5
          Top = 2
          Width = 288
          Height = 15
          Caption = 'Unit &Search Path (drop a directory or file here to add it)'
        end
      end
    end
    object p_ClientBottom: TPanel
      Left = 1
      Top = 209
      Width = 417
      Height = 106
      Align = alClient
      TabOrder = 1
      object lb_Inherited: TListBox
        Left = 1
        Top = 19
        Width = 415
        Height = 86
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
      object p_InheritedCaption: TPanel
        Left = 1
        Top = 1
        Width = 415
        Height = 18
        Align = alTop
        TabOrder = 1
        object l_Inherited: TLabel
          Left = 5
          Top = 2
          Width = 47
          Height = 15
          Caption = 'Inherited'
        end
      end
    end
  end
  object p_Left: TPanel
    Left = 0
    Top = 0
    Width = 200
    Height = 316
    Align = alLeft
    TabOrder = 2
    object lb_Target: TListBox
      Left = 1
      Top = 21
      Width = 198
      Height = 294
      Hint = 'Alt+Up/Down to move between entries'
      Align = alClient
      ExtendedSelect = False
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = lb_TargetClick
    end
    object p_TargetCaption: TPanel
      Left = 1
      Top = 1
      Width = 198
      Height = 20
      Align = alTop
      TabOrder = 1
      object l_Target: TLabel
        Left = 5
        Top = 2
        Width = 33
        Height = 15
        Caption = '&Target'
      end
    end
  end
  object TheActionList: TActionList
    Left = 96
    Top = 52
    object act_MoveUp: TAction
      Hint = 'Move Up'
      ShortCut = 16422
      OnExecute = act_MoveUpExecute
    end
    object act_MoveDown: TAction
      Hint = 'Move Down'
      ShortCut = 16424
      OnExecute = act_MoveDownExecute
    end
    object act_MoveToBase: TAction
      Caption = 'Move to base target'
      OnExecute = act_MoveToBaseExecute
    end
    object act_PrevTarget: TAction
      Caption = 'Previous Target'
      ShortCut = 32806
      OnExecute = act_PrevTargetExecute
    end
    object act_NextTarget: TAction
      Caption = 'Next Target'
      ShortCut = 32808
      OnExecute = act_NextTargetExecute
    end
    object act_OK: TAction
      Caption = '&OK'
      ShortCut = 16397
      OnExecute = act_OKExecute
    end
    object act_Cancel: TAction
      Caption = '&Cancel'
      ShortCut = 27
      OnExecute = act_CancelExecute
    end
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
  end
  object pm_Favorites: TPopupMenu
    Left = 468
    Top = 46
  end
  object pm_Memo: TPopupMenu
    Left = 280
    Top = 61
    object mi_MemoMoveUp: TMenuItem
      Action = act_MoveUp
      Caption = 'Move Up'
    end
    object mi_MemoMoveDown: TMenuItem
      Action = act_MoveDown
      Caption = 'Move Down'
    end
    object mi_MemoMovetobasetarget: TMenuItem
      Action = act_MoveToBase
      ShortCut = 49218
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
    object N2: TMenuItem
      Caption = '-'
    end
    object mi_MemoPrevoiusTarget: TMenuItem
      Action = act_PrevTarget
    end
    object mi_MemoNextTarget: TMenuItem
      Action = act_NextTarget
    end
  end
end
