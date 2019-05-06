inherited fmEConvertStrings: TfmEConvertStrings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Convert Strings'
  ClientHeight = 481
  ClientWidth = 689
  ParentFont = False
  OnResize = FormResize
  DesignSize = (
    689
    481)
  PixelsPerInch = 96
  TextHeight = 13
  object l_Input: TLabel
    Left = 8
    Top = 8
    Width = 26
    Height = 13
    Caption = 'Input'
    FocusControl = m_Input
  end
  object l_Output: TLabel
    Left = 440
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Output'
    FocusControl = m_Output
  end
  object l_Prefix: TLabel
    Left = 264
    Top = 216
    Width = 82
    Height = 13
    Caption = 'Prefix for all lines'
    FocusControl = ed_Prefix
    OnClick = b_CopyToClipboardClick
  end
  object l_Suffix: TLabel
    Left = 264
    Top = 320
    Width = 82
    Height = 13
    Caption = 'Suffix for all lines'
  end
  object m_Input: TMemo
    Left = 8
    Top = 24
    Width = 241
    Height = 409
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 18
    WordWrap = False
    OnChange = m_InputChange
  end
  object m_Output: TMemo
    Left = 440
    Top = 24
    Width = 241
    Height = 409
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 17
    WordWrap = False
  end
  object chk_QuoteStrings: TCheckBox
    Left = 264
    Top = 160
    Width = 161
    Height = 17
    Caption = '&Quote strings'
    TabOrder = 6
    OnClick = chk_QuoteStringsClick
  end
  object chk_AppendSpace: TCheckBox
    Left = 264
    Top = 184
    Width = 161
    Height = 17
    Caption = '&Append space'
    TabOrder = 7
    OnClick = chk_AppendSpaceClick
  end
  object b_CopyToClipboard: TButton
    Left = 352
    Top = 448
    Width = 121
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Copy To Clip&board'
    TabOrder = 14
    OnClick = b_CopyToClipboardClick
  end
  object b_Insert: TButton
    Left = 480
    Top = 448
    Width = 121
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'I&nsert and Close'
    TabOrder = 15
    OnClick = b_InsertClick
  end
  object b_Close: TButton
    Left = 608
    Top = 448
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 16
  end
  object chk_ExtractRaw: TCheckBox
    Left = 264
    Top = 64
    Width = 161
    Height = 17
    Caption = '&Extract Raw Strings'
    TabOrder = 2
    OnClick = chk_ExtractRawClick
  end
  object ed_Prefix: TEdit
    Left = 264
    Top = 232
    Width = 161
    Height = 21
    TabOrder = 8
    OnChange = ed_PrefixChange
  end
  object b_PasteFromClipboard: TButton
    Left = 8
    Top = 448
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Paste &From Clipboard'
    TabOrder = 0
    OnClick = b_PasteFromClipboardClick
  end
  object chk_TrimLeft: TCheckBox
    Left = 264
    Top = 88
    Width = 161
    Height = 17
    Caption = 'Trim &Left'
    TabOrder = 3
    OnClick = chk_TrimLeftClick
  end
  object chk_TrimRight: TCheckBox
    Left = 264
    Top = 112
    Width = 161
    Height = 17
    Caption = 'Trim &Right'
    TabOrder = 4
    OnClick = chk_TrimRightClick
  end
  object chk_Indent: TCheckBox
    Left = 264
    Top = 136
    Width = 161
    Height = 17
    Caption = '&Keep Indent'
    TabOrder = 5
    OnClick = chk_IndentClick
  end
  object b_Favorites: TButton
    Left = 264
    Top = 24
    Width = 161
    Height = 25
    Action = act_Favorites
    Anchors = [akLeft, akBottom]
    TabOrder = 1
  end
  object ed_PrefixFirst: TEdit
    Left = 264
    Top = 280
    Width = 161
    Height = 21
    TabOrder = 10
    OnChange = ed_PrefixFirstChange
  end
  object chk_PrefixFirst: TCheckBox
    Left = 264
    Top = 264
    Width = 161
    Height = 17
    Margins.Bottom = 0
    Caption = 'Same Prefix for first line'
    TabOrder = 9
    OnClick = chk_PrefixFirstClick
  end
  object ed_Suffix: TEdit
    Left = 264
    Top = 336
    Width = 161
    Height = 21
    TabOrder = 11
    OnChange = ed_SuffixChange
  end
  object chk_SuffixLast: TCheckBox
    Left = 264
    Top = 368
    Width = 161
    Height = 17
    Margins.Bottom = 0
    Caption = 'Same suffix for last line'
    TabOrder = 12
    OnClick = chk_SuffixLastClick
  end
  object ed_SuffixLast: TEdit
    Left = 264
    Top = 384
    Width = 161
    Height = 21
    TabOrder = 13
    OnChange = ed_SuffixLastChange
  end
  object pm_Favorites: TPopupMenu
    OnPopup = pm_FavoritesPopup
    Left = 384
    Top = 16
    object mi_FavoritesSaveAs: TMenuItem
      Caption = 'Save as ...'
      OnClick = mi_FavoritesSaveAsClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mi_Opendirectory: TMenuItem
      Caption = 'Open directory'
      OnClick = mi_OpendirectoryClick
    end
  end
  object TheActionList: TActionList
    Left = 184
    Top = 440
    object act_Favorites: TAction
      Caption = 'Favorites (F10) >'
      ShortCut = 121
      OnExecute = act_FavoritesExecute
    end
  end
end
