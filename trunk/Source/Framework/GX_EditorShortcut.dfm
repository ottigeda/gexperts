object fmEditorShortcut: TfmEditorShortcut
  Left = 304
  Top = 219
  BorderStyle = bsDialog
  Caption = 'Define Shortcut'
  ClientHeight = 129
  ClientWidth = 228
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object gbxShortcut: TGroupBox
    Left = 9
    Top = 8
    Width = 211
    Height = 81
    Caption = 'Editor Expert Name'
    TabOrder = 0
    object lblShortCut: TLabel
      Left = 9
      Top = 24
      Width = 48
      Height = 14
      Caption = '&Shortcut'
      FocusControl = hkyShortCut
    end
    object hkyShortCut: THotKey
      Left = 9
      Top = 47
      Width = 189
      Height = 22
      HotKey = 32833
      InvalidKeys = [hcNone, hcShift]
      Modifiers = [hkAlt]
      TabOrder = 0
    end
  end
  object btnCancel: TButton
    Left = 145
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 60
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
