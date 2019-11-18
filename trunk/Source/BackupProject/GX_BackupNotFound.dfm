inherited fmBackupNotFound: TfmBackupNotFound
  ActiveControl = b_OK
  BorderIcons = [biSystemMenu]
  Caption = 'Backup Project'
  ClientHeight = 301
  ClientWidth = 624
  ParentFont = False
  Font.Height = -12
  PixelsPerInch = 96
  TextHeight = 14
  object l_NotFound: TLabel
    Left = 8
    Top = 8
    Width = 323
    Height = 14
    Caption = 'The following included files could not be found for backup:'
  end
  object m_NotFound: TMemo
    Left = 8
    Top = 32
    Width = 608
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 535
    Top = 268
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = b_OKClick
  end
end
