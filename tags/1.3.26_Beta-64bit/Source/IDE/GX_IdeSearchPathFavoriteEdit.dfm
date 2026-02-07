inherited f_IdeSearchPathFavoriteEdit: Tf_IdeSearchPathFavoriteEdit
  Left = 100
  Top = 100
  BorderIcons = [biSystemMenu]
  Caption = 'Favorite Search Path'
  ClientHeight = 209
  ClientWidth = 417
  ParentFont = False
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object l_Name: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object l_Path: TLabel
    Left = 8
    Top = 56
    Width = 22
    Height = 13
    Caption = 'Path'
  end
  object ed_Name: TEdit
    Left = 8
    Top = 24
    Width = 401
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object m_Path: TMemo
    Left = 8
    Top = 72
    Width = 401
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object b_OK: TButton
    Left = 266
    Top = 177
    Width = 70
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object b_Cancel: TButton
    Left = 341
    Top = 177
    Width = 70
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object b_Select: TButton
    Left = 122
    Top = 177
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Select directory'
    TabOrder = 2
    OnClick = b_SelectClick
  end
  object b_Help: TButton
    Left = 8
    Top = 177
    Width = 70
    Height = 25
    Hint = 'Help (F1)'
    Caption = 'Help'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = b_HelpClick
  end
end
