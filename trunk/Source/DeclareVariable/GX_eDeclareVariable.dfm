object fmDeclareVariable: TfmDeclareVariable
  Left = 311
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Declare Variable'
  ClientHeight = 340
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    420
    340)
  PixelsPerInch = 96
  TextHeight = 14
  object lblIdentifier: TLabel
    Left = 16
    Top = 16
    Width = 52
    Height = 14
    Caption = 'Identifier:'
  end
  object lblIdentifierValue: TLabel
    Left = 120
    Top = 16
    Width = 283
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblExisting: TLabel
    Left = 16
    Top = 40
    Width = 387
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    WordWrap = True
  end
  object lblDeclareAs: TLabel
    Left = 16
    Top = 64
    Width = 61
    Height = 14
    Caption = 'Declare as:'
  end
  object rbLocalVar: TRadioButton
    Left = 120
    Top = 63
    Width = 100
    Height = 17
    Caption = 'Local &variable'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object rbField: TRadioButton
    Left = 224
    Top = 63
    Width = 60
    Height = 17
    Caption = '&Field'
    TabOrder = 1
  end
  object rbProperty: TRadioButton
    Left = 300
    Top = 63
    Width = 80
    Height = 17
    Caption = '&Property'
    TabOrder = 2
  end
  object lblType: TLabel
    Left = 16
    Top = 96
    Width = 30
    Height = 14
    Caption = '&Type:'
    FocusControl = cmbType
  end
  object cmbType: TComboBox
    Left = 120
    Top = 93
    Width = 283
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object lblTypeHint: TLabel
    Left = 120
    Top = 117
    Width = 283
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object lblInsertAt: TLabel
    Left = 16
    Top = 128
    Width = 50
    Height = 14
    Caption = '&Insert at:'
    FocusControl = cmbInsertAt
  end
  object cmbInsertAt: TComboBox
    Left = 120
    Top = 125
    Width = 283
    Height = 22
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object lblRead: TLabel
    Left = 16
    Top = 164
    Width = 79
    Height = 14
    Caption = '&Read specifier:'
    FocusControl = edtRead
    Visible = False
  end
  object edtRead: TEdit
    Left = 120
    Top = 161
    Width = 283
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Visible = False
  end
  object lblWrite: TLabel
    Left = 16
    Top = 196
    Width = 82
    Height = 14
    Caption = '&Write specifier:'
    FocusControl = edtWrite
    Visible = False
  end
  object edtWrite: TEdit
    Left = 120
    Top = 193
    Width = 283
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    Visible = False
  end
  object lblWarning: TLabel
    Left = 16
    Top = 268
    Width = 387
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
    WordWrap = True
  end
  object btnOK: TButton
    Left = 247
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 328
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
end
