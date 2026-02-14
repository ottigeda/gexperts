object fmReverseOptions: TfmReverseOptions
  Left = 268
  Top = 200
  BorderStyle = bsSizeable
  Caption = 'Reverse Statement Configuration'
  ClientHeight = 510
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OnResize = FormResize
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    800
    510)
  PixelsPerInch = 96
  TextHeight = 14
  object lblFormat: TLabel
    Left = 8
    Top = 8
    Width = 784
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Each row defines one reversal rule. Use $1, $2, ... for backreferences in the replacement.'
    WordWrap = True
  end
  object sgRules: TStringGrid
    Left = 8
    Top = 40
    Width = 696
    Height = 312
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    OnDblClick = sgRulesDblClick
  end
  object pnlButtons: TPanel
    Left = 712
    Top = 40
    Width = 80
    Height = 312
    Anchors = [akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    object btnAdd: TButton
      Left = 0
      Top = 0
      Width = 80
      Height = 25
      Caption = '&Add'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnEdit: TButton
      Left = 0
      Top = 32
      Width = 80
      Height = 25
      Caption = '&Edit'
      TabOrder = 1
      OnClick = btnEditClick
    end
    object btnDelete: TButton
      Left = 0
      Top = 64
      Width = 80
      Height = 25
      Caption = '&Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnReset: TButton
      Left = 0
      Top = 96
      Width = 80
      Height = 25
      Caption = '&Reset'
      TabOrder = 3
      OnClick = btnResetClick
    end
  end
  object lblError: TLabel
    Left = 8
    Top = 358
    Width = 784
    Height = 14
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object gbTest: TGroupBox
    Left = 8
    Top = 370
    Width = 784
    Height = 104
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Test'
    TabOrder = 2
    object lblTestInput: TLabel
      Left = 8
      Top = 18
      Width = 28
      Height = 14
      Caption = 'Input'
    end
    object edTestInput: TEdit
      Left = 8
      Top = 36
      Width = 768
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edTestInputChange
    end
    object lblTestResult: TLabel
      Left = 8
      Top = 64
      Width = 32
      Height = 14
      Caption = 'Result'
    end
    object edTestResult: TEdit
      Left = 8
      Top = 82
      Width = 768
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 620
    Top = 478
    Width = 80
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 712
    Top = 478
    Width = 80
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object tmrTest: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrTestTimer
  end
end
