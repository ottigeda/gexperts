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
  object l_Note: TLabel
    Left = 8
    Top = 264
    Width = 521
    Height = 33
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Note: "Search Library path for included files" is turned off. Yo' +
      'u might want to turn it on in the expert'#39's configuration.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object m_NotFound: TMemo
    Left = 8
    Top = 32
    Width = 608
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 535
    Top = 268
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = b_OKClick
  end
end
