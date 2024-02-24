object fmGxExplicitFilter: TfmGxExplicitFilter
  Left = 381
  Top = 212
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Filter Explicit Properties'
  ClientHeight = 145
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 120
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 200
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chk_WriteExplicitLeft: TCheckBox
    Left = 16
    Top = 8
    Width = 257
    Height = 17
    Caption = 'Write ExplicitLeft'
    TabOrder = 0
  end
  object chk_WriteExplicitTop: TCheckBox
    Left = 16
    Top = 32
    Width = 257
    Height = 17
    Caption = 'Write ExplicitTop'
    TabOrder = 1
  end
  object chk_WriteExplicitWidth: TCheckBox
    Left = 16
    Top = 56
    Width = 257
    Height = 17
    Caption = 'Write ExplicitWidth'
    TabOrder = 2
  end
  object chk_WriteExplicitHeight: TCheckBox
    Left = 16
    Top = 80
    Width = 257
    Height = 17
    Caption = 'Write ExplicitHeight'
    TabOrder = 3
  end
end
