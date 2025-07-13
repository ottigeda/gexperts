inherited f_EditPathConfig: Tf_EditPathConfig
  Left = 2269
  Top = 237
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Unit Search Path, Configuration'
  ClientHeight = 276
  ClientWidth = 281
  KeyPreview = True
  OldCreateOrder = True
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object l_SelectConfig: TLabel
    Left = 8
    Top = 8
    Width = 100
    Height = 13
    Caption = 'Select Default Config'
  end
  object b_OK: TButton
    Left = 128
    Top = 245
    Width = 70
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 203
    Top = 245
    Width = 70
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object lb_Target: TListBox
    Left = 8
    Top = 24
    Width = 265
    Height = 214
    Hint = 'Alt+Up/Down to move between entries'
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExtendedSelect = False
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object b_Help: TButton
    Left = 8
    Top = 245
    Width = 70
    Height = 24
    Hint = 'Help (F1)'
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = b_HelpClick
  end
end
