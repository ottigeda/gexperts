object fmConfigureWarning: TfmConfigureWarning
  Left = 500
  Top = 265
  ActiveControl = ed_Filter
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Insert WARN Directive'
  ClientHeight = 305
  ClientWidth = 418
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
    418
    305)
  PixelsPerInch = 96
  TextHeight = 14
  object l_Filter: TLabel
    Left = 8
    Top = 8
    Width = 26
    Height = 14
    Caption = 'Fi&lter'
    FocusControl = ed_Filter
  end
  object lb_Warn: TListBox
    Left = 8
    Top = 48
    Width = 393
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 14
    TabOrder = 1
  end
  object b_ON: TButton
    Left = 8
    Top = 272
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&ON'
    ModalResult = 1
    TabOrder = 2
    OnClick = b_ONClick
  end
  object b_OFF: TButton
    Left = 88
    Top = 272
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'O&FF'
    ModalResult = 1
    TabOrder = 3
    OnClick = b_OFFClick
  end
  object b_Default: TButton
    Left = 168
    Top = 272
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&DEFAULT'
    ModalResult = 1
    TabOrder = 4
    OnClick = b_DefaultClick
  end
  object b_Cancel: TButton
    Left = 328
    Top = 272
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object ed_Filter: TEdit
    Left = 8
    Top = 24
    Width = 393
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ed_FilterChange
    OnKeyDown = ed_FilterKeyDown
  end
  object b_ERROR: TButton
    Left = 248
    Top = 272
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&ERROR'
    ModalResult = 1
    TabOrder = 5
    OnClick = b_ERRORClick
  end
end
