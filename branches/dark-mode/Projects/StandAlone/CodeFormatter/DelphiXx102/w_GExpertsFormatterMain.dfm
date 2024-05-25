object f_GExpertsFormatterMain: Tf_GExpertsFormatterMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'GExperts Source Code Formatter'
  ClientHeight = 308
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    617
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object l_FilesToFormat: TLabel
    Left = 8
    Top = 8
    Width = 253
    Height = 13
    Caption = 'Files to format (you can drop them onto the window)'
  end
  object b_SelectFile: TButton
    Left = 584
    Top = 22
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = b_SelectFileClick
  end
  object b_Format: TButton
    Left = 456
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Format'
    Default = True
    Enabled = False
    TabOrder = 2
    OnClick = b_FormatClick
  end
  object b_Exit: TButton
    Left = 536
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Exit'
    TabOrder = 3
    OnClick = b_ExitClick
  end
  object b_Settings: TButton
    Left = 88
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Settings ...'
    TabOrder = 4
    OnClick = b_SettingsClick
  end
  object b_About: TButton
    Left = 8
    Top = 256
    Width = 75
    Height = 25
    Caption = 'About ...'
    TabOrder = 5
    OnClick = b_AboutClick
  end
  object TheStatusBar: TStatusBar
    Left = 0
    Top = 289
    Width = 617
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Select at least one file to format or drag one onto this window.'
  end
  object m_FilesToFormat: TMemo
    Left = 8
    Top = 24
    Width = 569
    Height = 225
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    WordWrap = False
    OnChange = m_FilesToFormatChange
  end
  object od_File: TOpenDialog
    DefaultExt = '.pas'
    Filter = 'Delphi Sourcecode (*.pas)|*.pas|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Select file'
    Left = 238
    Top = 44
  end
end
