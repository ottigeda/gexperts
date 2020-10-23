inherited frmSortOptions: TfrmSortOptions
  BorderStyle = bsDialog
  Caption = 'Sort Options'
  ClientHeight = 249
  ClientWidth = 217
  PixelsPerInch = 96
  TextHeight = 13
  object grpSortOrder: TGroupBox
    Left = 8
    Top = 8
    Width = 201
    Height = 201
    Caption = 'Sort order for prefixes'
    TabOrder = 0
    object lbxGroupOrder: TListBox
      Left = 8
      Top = 24
      Width = 153
      Height = 169
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbxGroupOrderClick
    end
    object btnUp: TBitBtn
      Left = 168
      Top = 64
      Width = 25
      Height = 25
      Hint = 'Move up'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnUpClick
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
    end
    object btnDown: TBitBtn
      Left = 168
      Top = 96
      Width = 25
      Height = 25
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnDownClick
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
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4EFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEF04A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4EFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFC04A4A4EFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFA4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFFCFCFCFC04A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFCFCFC04A4A4EFEFEFEF
        EFEFEFEFEFEFEFEFEFEFA4A4A4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFFCFCFCFCFCFC040404EFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4A4A4
        A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFC04A4EFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFFCFCFCFC04A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFC04A4EFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFFCFCFCFC04A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFCFCFCFC04A4EFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFFCFCFCFC04EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFA4A4A4A4A4
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF
        EFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEF}
      NumGlyphs = 2
    end
  end
  object btnOK: TButton
    Left = 40
    Top = 216
    Width = 81
    Height = 26
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 128
    Top = 216
    Width = 81
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
