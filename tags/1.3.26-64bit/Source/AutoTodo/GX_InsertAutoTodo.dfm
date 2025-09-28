object fmInsertAutoTodoForm: TfmInsertAutoTodoForm
  Left = 381
  Top = 212
  BorderIcons = [biSystemMenu]
  Caption = 'Comment Empty Code Blocks'
  ClientHeight = 209
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    614
    209)
  PixelsPerInch = 96
  TextHeight = 14
  object lblUsername: TLabel
    Left = 8
    Top = 8
    Width = 54
    Height = 14
    Caption = 'Username'
  end
  object lblTextToInsert: TLabel
    Left = 8
    Top = 56
    Width = 239
    Height = 14
    Caption = 'Text to insert (include comment delimiters)'
  end
  object lblStarWindows: TLabel
    Left = 261
    Top = 27
    Width = 230
    Height = 14
    Caption = '(Enter '#39'*'#39' to use the Windows username)'
  end
  object edtUsername: TEdit
    Left = 8
    Top = 24
    Width = 249
    Height = 22
    TabOrder = 0
  end
  object btnLoadDetault: TButton
    Left = 8
    Top = 144
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load Default'
    TabOrder = 2
    OnClick = btnLoadDetaultClick
  end
  object btnInsertPlaceholder: TButton
    Left = 112
    Top = 144
    Width = 115
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Insert Placeholder'
    TabOrder = 3
  end
  object mmoTextToInsert: TMemo
    Left = 8
    Top = 72
    Width = 596
    Height = 67
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 176
    Width = 614
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      614
      33)
    object chkShowDoneDialog: TCheckBox
      Left = 8
      Top = 5
      Width = 249
      Height = 17
      Caption = 'Show done dialog'
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 425
      Top = 0
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 521
      Top = 0
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
  object pmuPlaceholders: TPopupMenu
    Left = 568
    Top = 16
  end
end
