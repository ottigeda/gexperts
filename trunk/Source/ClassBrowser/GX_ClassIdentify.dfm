object fmClassIdentify: TfmClassIdentify
  Left = 255
  Top = 176
  BorderStyle = bsDialog
  Caption = 'Enter Class Identifier'
  ClientHeight = 193
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object gbxIdentifier: TGroupBox
    Left = 8
    Top = 8
    Width = 305
    Height = 145
    Caption = 'Class Set Identifier'
    TabOrder = 0
    object lblNotes: TLabel
      Left = 9
      Top = 22
      Width = 290
      Height = 59
      AutoSize = False
      Caption = 
        'Each set of classes requires that a unique identifier be entered' +
        '.  For example, VCL classes might use the identifier "VCL".'
      WordWrap = True
    end
    object lblIdentifier: TLabel
      Left = 10
      Top = 94
      Width = 49
      Height = 14
      Alignment = taRightJustify
      Caption = '&Identifier'
      FocusControl = edtID
    end
    object edtID: TEdit
      Left = 67
      Top = 91
      Width = 229
      Height = 22
      TabOrder = 0
      OnChange = edtIDChange
    end
    object chk_ScanRecursively: TCheckBox
      Left = 8
      Top = 120
      Width = 289
      Height = 17
      Caption = 'Scan Subdirectories'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 160
    Top = 160
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 240
    Top = 160
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
