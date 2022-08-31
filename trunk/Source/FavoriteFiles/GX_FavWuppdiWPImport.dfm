object fmFavWuppdiWPImport: TfmFavWuppdiWPImport
  Left = 315
  Top = 223
  Caption = 'WuppdiWP Import'
  ClientHeight = 137
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    441
    137)
  PixelsPerInch = 96
  TextHeight = 14
  object lblWuppdiWPFilename: TLabel
    Left = 8
    Top = 8
    Width = 155
    Height = 14
    Alignment = taRightJustify
    Caption = 'WuppdiWP &configuration file'
    FocusControl = edtWuppdiWPFilename
  end
  object lblSubfolderName: TLabel
    Left = 8
    Top = 56
    Width = 110
    Height = 14
    Alignment = taRightJustify
    Caption = 'Fav. &subfolder name'
    FocusControl = edtWuppdiWPFilename
  end
  object btnCancel: TButton
    Left = 360
    Top = 104
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnOK: TButton
    Left = 280
    Top = 104
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
  end
  object edtWuppdiWPFilename: TEdit
    Left = 8
    Top = 24
    Width = 393
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edtWuppdiWPFilenameChange
  end
  object btWuppdiWPFilenameSelect: TButton
    Left = 408
    Top = 24
    Width = 22
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btWuppdiWPFilenameSelectClick
  end
  object edtSubfolderName: TEdit
    Left = 8
    Top = 72
    Width = 169
    Height = 22
    Hint = 'Set this to empty to override the favorite-root'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
end
