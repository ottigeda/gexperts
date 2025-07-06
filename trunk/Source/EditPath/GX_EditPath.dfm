inherited f_EditPath: Tf_EditPath
  Left = 1075
  Top = 77
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Edit Unit Search Path'
  ClientHeight = 271
  ClientWidth = 569
  Position = poDesigned
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object spl_Horizontal: TSplitter
    Left = 217
    Top = 0
    Width = 5
    Height = 230
  end
  object p_BottomButtons: TPanel
    Left = 0
    Top = 230
    Width = 569
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      569
      41)
    object imF1: TImage
      Left = 7
      Top = 12
      Width = 16
      Height = 16
      Picture.Data = {
        07544269746D617036030000424D360300000000000036000000280000001000
        0000100000000100180000000000000300000000000000000000000000000000
        0000FF00FFFF00FFFF00FFFF00FF9C9C9C6B6B6B5252524A4A4A4A4A4A4A4A4A
        5252529C9C9CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFADADADE7CE
        C6EFDED6F7E7D6F7E7D6EFDED6EFDED6CEBDB55A5A5A7373739C9C9CFF00FFFF
        00FFFF00FFFF00FFCEC6BDF7E7D6F7F7F7E7E7E7CEB5A5D6AD94DEC6BDEFF7F7
        F7EFEFEFDED68C847B6B6B6B9C9C9CFF00FFFF00FFE7D6CEF7E7DEF7FFFFCE9C
        84B54A10BD6339D6AD9CC65A21BD5221D6AD9CF7FFFFF7DED67B736B737373FF
        00FFFF00FFEFDED6F7FFFFC67B5ABD4A10C65218C6A594FFFFFFDE9473BD4A10
        B54A10CE9C84F7FFFFEFDED65A5A5A9C9C9CF7E7E7F7F7F7D6AD94BD4A10CE63
        31CE6329CE6B39DE8C6BCE6329CE6331C65A29B54A10DEC6BDF7EFE7A59C9463
        6363F7EFE7F7F7F7C66331C65A29CE6B31CE5A21CE8C6BF7E7DECE6B39C65A21
        CE6331C65218C67B52F7FFFFDECEC6525252F7EFE7EFDED6C65A21CE6331CE63
        31CE5A21C6846BFFFFFFEFAD94C64A10CE6331CE6329C65A29F7EFEFEFDED652
        5252F7EFE7F7DECECE5A21CE6331CE6331CE6329C65221CEB5A5FFFFFFE79C7B
        C65218CE6329C65A29F7EFEFEFDED6525252F7EFE7F7EFE7DE6B31D66B31CE63
        29C65A21C65218BD4A10DECEC6FFFFFFD67342CE5A21CE6B39FFF7F7EFDED66B
        6B6BF7E7E7FFFFFFF79C6BE76329CE8C6BEFE7DED67B52BD3100D69C7BFFFFFF
        DE8C63CE5218E79C73FFFFFFDEC6BDADADADF7EFE7F7EFEFFFEFDEFF8C4ADE84
        5AEFFFFFFFFFFFE7BDA5F7FFFFEFFFFFE77339E77339FFEFEFF7E7DEA59C94FF
        00FFFF00FFEFDED6FFFFFFFFEFCEFFB573EFAD84EFE7DEEFF7F7EFE7DEF7A57B
        FF8C4AFFDECEFFFFFFEFDED6CECEC6FF00FFFF00FFF7EFEFEFDED6FFFFFFFFFF
        FFFFF7C6FFDEADFFCE94FFCE94FFD6ADFFF7F7FFFFFFEFDED6CECEC6FF00FFFF
        00FFFF00FFFF00FFF7EFEFEFDED6F7E7E7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        EFE7DEEFDED6DEDEDEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFF7EFE7F7F7
        EFF7EFEFF7EFEFF7EFEFF7EFEFF7EFEFF7EFEFF7EFE7FF00FFFF00FFFF00FFFF
        00FF}
      Transparent = True
      OnClick = imF1Click
    end
    object b_Cancel: TButton
      Left = 493
      Top = 8
      Width = 70
      Height = 25
      Action = act_Cancel
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 5
    end
    object b_OK: TButton
      Left = 417
      Top = 8
      Width = 70
      Height = 25
      Action = act_OK
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 4
    end
    object b_MakeRelative: TButton
      Left = 37
      Top = 8
      Width = 88
      Height = 25
      Action = act_MakeRelative
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object b_MakeAbsolute: TButton
      Left = 132
      Top = 8
      Width = 88
      Height = 25
      Action = act_MakeAbsolute
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
    object b_PrependDots: TButton
      Left = 226
      Top = 8
      Width = 78
      Height = 25
      Action = act_PrependDots
      Anchors = [akTop, akRight]
      TabOrder = 2
    end
    object b_RemoveDots: TButton
      Left = 311
      Top = 8
      Width = 78
      Height = 25
      Action = act_RemoveDots
      Anchors = [akTop, akRight]
      TabOrder = 3
    end
  end
  object p_Client: TPanel
    Left = 222
    Top = 0
    Width = 347
    Height = 230
    Align = alClient
    TabOrder = 0
    object spl_Vertical: TSplitter
      Left = 1
      Top = 125
      Width = 345
      Height = 5
      Cursor = crVSplit
      Align = alBottom
    end
    object p_ClientClient: TPanel
      Left = 1
      Top = 1
      Width = 345
      Height = 124
      Align = alClient
      Caption = 'Memo goes here'
      TabOrder = 0
      object p_RightButtons: TPanel
        Left = 303
        Top = 17
        Width = 41
        Height = 106
        Align = alRight
        TabOrder = 0
        DesignSize = (
          41
          106)
        object sb_MoveUp: TSpeedButton
          Left = 8
          Top = 40
          Width = 25
          Height = 23
          Action = act_MoveUp
          Anchors = [akLeft, akBottom]
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
          Left = 8
          Top = 72
          Width = 25
          Height = 23
          Action = act_MoveDown
          Anchors = [akLeft, akBottom]
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
          Left = 8
          Top = 8
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
      end
      object p_MemoCaption: TPanel
        Left = 1
        Top = 1
        Width = 343
        Height = 16
        Align = alTop
        TabOrder = 1
        object l_UnitSearchPath: TLabel
          Left = 8
          Top = 0
          Width = 256
          Height = 13
          Caption = 'Unit &Search Path (drop a directory or file here to add it)'
        end
      end
    end
    object p_ClientBottom: TPanel
      Left = 1
      Top = 130
      Width = 345
      Height = 99
      Align = alBottom
      TabOrder = 1
      OnCanResize = p_ClientBottomCanResize
      object lb_Inherited: TListBox
        Left = 1
        Top = 17
        Width = 343
        Height = 81
        Align = alClient
        Anchors = [akLeft, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
      object p_InheritedCaption: TPanel
        Left = 1
        Top = 1
        Width = 343
        Height = 16
        Align = alTop
        TabOrder = 1
        object l_Inherited: TLabel
          Left = 8
          Top = 0
          Width = 41
          Height = 13
          Caption = 'Inherited'
        end
      end
    end
  end
  object p_Left: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 230
    Align = alLeft
    TabOrder = 2
    OnCanResize = p_LeftCanResize
    object lb_Target: TListBox
      Left = 1
      Top = 17
      Width = 215
      Height = 212
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
      Width = 215
      Height = 16
      Align = alTop
      TabOrder = 1
      object l_Target: TLabel
        Left = 8
        Top = 0
        Width = 31
        Height = 13
        Caption = '&Target'
      end
    end
  end
  object TheActionList: TActionList
    Left = 96
    Top = 53
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
    Left = 469
    Top = 46
  end
  object pm_Memo: TPopupMenu
    Left = 376
    Top = 151
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
