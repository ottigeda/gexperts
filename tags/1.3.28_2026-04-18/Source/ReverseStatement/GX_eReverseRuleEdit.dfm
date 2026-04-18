object fmReverseRuleEdit: TfmReverseRuleEdit
  Left = 268
  Top = 200
  BorderStyle = bsDialog
  Caption = 'Edit Reversal Rule'
  ClientHeight = 290
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    700
    290)
  PixelsPerInch = 96
  TextHeight = 14
  object lblMatch: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 14
    Caption = 'Match pattern'
  end
  object edMatch: TEdit
    Left = 8
    Top = 26
    Width = 684
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edMatchChange
  end
  object lblReplace: TLabel
    Left = 8
    Top = 56
    Width = 70
    Height = 14
    Caption = 'Replacement'
  end
  object edReplace: TEdit
    Left = 8
    Top = 74
    Width = 684
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = edReplaceChange
  end
  object lblError: TLabel
    Left = 8
    Top = 106
    Width = 684
    Height = 14
    Anchors = [akLeft, akTop, akRight]
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
    Top = 128
    Width = 684
    Height = 110
    Anchors = [akLeft, akTop, akRight]
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
      Width = 668
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
      Width = 668
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 516
    Top = 256
    Width = 80
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 608
    Top = 256
    Width = 80
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object tmrValidate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrValidateTimer
  end
end
