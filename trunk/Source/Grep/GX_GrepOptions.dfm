object fmGrepOptions: TfmGrepOptions
  Left = 294
  Top = 263
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Search Options'
  ClientHeight = 177
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    401
    177)
  PixelsPerInch = 96
  TextHeight = 14
  object l_ExternalEditor: TLabel
    Left = 8
    Top = 48
    Width = 218
    Height = 14
    Caption = 'External Editor (for stand alone version)'
  end
  object l_Parameters: TLabel
    Left = 8
    Top = 96
    Width = 61
    Height = 14
    Caption = 'Parameters'
  end
  object btnOK: TButton
    Left = 240
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 320
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkUseCurrentIdent: TCheckBox
    Left = 8
    Top = 16
    Width = 385
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use &current editor identifier as the default search string'
    TabOrder = 0
  end
  object ed_ExternalEditor: TEdit
    Left = 8
    Top = 64
    Width = 353
    Height = 22
    TabOrder = 1
  end
  object b_Select: TButton
    Left = 368
    Top = 63
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = b_SelectClick
  end
  object ed_Parameters: TEdit
    Left = 8
    Top = 112
    Width = 361
    Height = 22
    TabOrder = 3
  end
  object b_Parameters: TButton
    Left = 8
    Top = 144
    Width = 113
    Height = 25
    Caption = 'Parameters'
    TabOrder = 6
  end
  object pm_Parameters: TPopupMenu
    Left = 192
    Top = 88
    object mi_File: TMenuItem
      Caption = '{FILE}'
      OnClick = mi_FileClick
    end
    object mi_Line: TMenuItem
      Caption = '{LINE}'
      OnClick = mi_LineClick
    end
    object mi_Column: TMenuItem
      Caption = '{COLUMN}'
      OnClick = mi_ColumnClick
    end
  end
end
