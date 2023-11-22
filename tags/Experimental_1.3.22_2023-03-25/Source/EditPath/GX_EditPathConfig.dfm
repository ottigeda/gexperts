inherited f_EditPathConfig: Tf_EditPathConfig
  BorderIcons = [biSystemMenu]
  Caption = 'Edit Unit Sarch Path Config'
  ClientHeight = 369
  ClientWidth = 281
  OldCreateOrder = True
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
    Left = 104
    Top = 336
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 192
    Top = 336
    Width = 81
    Height = 25
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
    Height = 298
    Hint = 'Alt+Up/Down to move between entries'
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExtendedSelect = False
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
end
