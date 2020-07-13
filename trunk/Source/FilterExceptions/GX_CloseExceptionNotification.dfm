object fmGxCloseExceptionNotificationExpert: TfmGxCloseExceptionNotificationExpert
  Left = 381
  Top = 212
  Width = 640
  Height = 320
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Close Exception Notification'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnDblClick = FormDblClick
  OnResize = FormResize
  DesignSize = (
    624
    281)
  PixelsPerInch = 96
  TextHeight = 14
  object l_Exceptions: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 14
    Caption = 'Exceptions'
  end
  object sg_Exceptions: TStringGrid
    Left = 8
    Top = 24
    Width = 608
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    DefaultColWidth = 150
    DefaultRowHeight = 20
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    TabOrder = 0
    OnDblClick = sg_ExceptionsDblClick
  end
  object b_OK: TButton
    Left = 463
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 543
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object b_Add: TButton
    Left = 8
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add ...'
    TabOrder = 3
    OnClick = b_AddClick
  end
  object b_Edit: TButton
    Left = 88
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Edit ...'
    TabOrder = 4
    OnClick = b_EditClick
  end
  object b_Delete: TButton
    Left = 168
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete ...'
    TabOrder = 5
    OnClick = b_DeleteClick
  end
end
