object fmPrevNextConfig: TfmPrevNextConfig
  Left = 311
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Prev/Next Identifier Options'
  ClientHeight = 137
  ClientWidth = 312
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
    312
    137)
  PixelsPerInch = 96
  TextHeight = 14
  object gbxPrevNextOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 297
    Height = 86
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Prev/Next Identifier Options'
    TabOrder = 0
    object chkCenterMatch: TCheckBox
      Left = 16
      Top = 32
      Width = 265
      Height = 17
      Caption = 'Center found identifiers in the editor'
      TabOrder = 0
    end
    object chk_Unfold: TCheckBox
      Left = 16
      Top = 56
      Width = 265
      Height = 17
      Caption = 'Call "unfold nearest" at position'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 148
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 228
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
