object fmGrepOptions: TfmGrepOptions
  Left = 294
  Top = 263
  BorderStyle = bsDialog
  Caption = 'Grep Search Options'
  ClientHeight = 137
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
    137)
  PixelsPerInch = 96
  TextHeight = 14
  object gbxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 385
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Grep Options'
    TabOrder = 0
    DesignSize = (
      385
      89)
    object chkGrepUseCurrentIdent: TCheckBox
      Left = 8
      Top = 24
      Width = 369
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use &current editor identifier as the default search string'
      TabOrder = 0
    end
    object chkUseMapFile: TCheckBox
      Left = 8
      Top = 48
      Width = 369
      Height = 17
      Caption = 'Use map file instead of dpr file for project search'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 240
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 320
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
