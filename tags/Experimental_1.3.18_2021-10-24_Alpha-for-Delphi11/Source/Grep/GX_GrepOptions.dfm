object fmGrepOptions: TfmGrepOptions
  Left = 294
  Top = 263
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Search Options'
  ClientHeight = 177
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  DesignSize = (
    529
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
    Left = 368
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 448
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
    Width = 513
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use &current editor identifier as the default search string'
    TabOrder = 0
  end
  object ed_ExternalEditor: TEdit
    Left = 8
    Top = 64
    Width = 481
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object b_Select: TButton
    Left = 496
    Top = 64
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = b_SelectClick
  end
  object ed_Parameters: TEdit
    Left = 8
    Top = 112
    Width = 513
    Height = 22
    Anchors = [akLeft, akTop, akRight]
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
