object fmCleanDirectoriesOptions: TfmCleanDirectoriesOptions
  Left = 551
  Top = 313
  BorderStyle = bsDialog
  Caption = 'Clean Directories Configuration'
  ClientHeight = 81
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    353
    81)
  PixelsPerInch = 96
  TextHeight = 15
  object chk_IncludeBinary: TCheckBox
    Left = 8
    Top = 16
    Width = 345
    Height = 17
    Caption = 'Include the project'#39's unit output and executable directory'
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 179
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 267
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
