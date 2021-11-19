inherited f_EditPath: Tf_EditPath
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Path Editor'
  ClientHeight = 257
  ClientWidth = 529
  PixelsPerInch = 96
  TextHeight = 13
  object p_Memo: TPanel
    Left = 8
    Top = 16
    Width = 513
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Memo goes here'
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 360
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 440
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
