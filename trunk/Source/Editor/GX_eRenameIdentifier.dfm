object fmRenameIdentifier: TfmRenameIdentifier
  Left = 311
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Rename Identifier'
  ClientHeight = 227
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    370
    227)
  PixelsPerInch = 96
  TextHeight = 14
  object lblCurrentName: TLabel
    Left = 16
    Top = 16
    Width = 80
    Height = 14
    Caption = 'Current name:'
  end
  object lblCurrentNameValue: TLabel
    Left = 112
    Top = 16
    Width = 241
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
  object lblScope: TLabel
    Left = 16
    Top = 40
    Width = 37
    Height = 14
    Caption = 'Scope:'
  end
  object lblScopeValue: TLabel
    Left = 112
    Top = 40
    Width = 241
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = '...'
  end
  object lblOccurrences: TLabel
    Left = 16
    Top = 64
    Width = 337
    Height = 14
    Anchors = [akLeft, akTop, akRight]
    Caption = '0 occurrences will be renamed'
  end
  object lblComponentWarning: TLabel
    Left = 16
    Top = 88
    Width = 337
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'This will also rename the component in the form designer.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    WordWrap = True
  end
  object lblNewName: TLabel
    Left = 16
    Top = 122
    Width = 60
    Height = 14
    Caption = 'New name:'
  end
  object edtNewName: TEdit
    Left = 16
    Top = 142
    Width = 337
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 197
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 278
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
