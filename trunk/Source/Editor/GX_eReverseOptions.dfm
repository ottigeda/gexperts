object fmReverseOptions: TfmReverseOptions
  Left = 268
  Top = 200
  BorderStyle = bsSizeable
  Caption = 'Reverse Statement Configuration'
  ClientHeight = 470
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    600
    470)
  PixelsPerInch = 96
  TextHeight = 14
  object lblFormat: TLabel
    Left = 8
    Top = 8
    Width = 584
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Caption =
      'Regex reversal rules (paired lines: match pattern, then replace' +
      'ment template).'#13#10'Lines starting with # are comments. Use $1, $2' +
      ', ... for backreferences.'
    WordWrap = True
  end
  object pnlMemo: TPanel
    Left = 8
    Top = 42
    Width = 584
    Height = 310
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
  end
  object lblError: TLabel
    Left = 8
    Top = 358
    Width = 584
    Height = 14
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblTestInput: TLabel
    Left = 8
    Top = 380
    Width = 50
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'Test input:'
  end
  object edTestInput: TEdit
    Left = 70
    Top = 377
    Width = 434
    Height = 22
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object btnTest: TButton
    Left = 512
    Top = 376
    Width = 80
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = '&Test'
    TabOrder = 2
    OnClick = btnTestClick
  end
  object lblTestResult: TLabel
    Left = 8
    Top = 406
    Width = 56
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'Test result:'
  end
  object edTestResult: TEdit
    Left = 70
    Top = 403
    Width = 522
    Height = 22
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 420
    Top = 437
    Width = 80
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 512
    Top = 437
    Width = 80
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
