inherited fmCleanDirectoriesOptions: TfmCleanDirectoriesOptions
  BorderStyle = bsDialog
  Caption = 'Clean Directories Configuration'
  ClientHeight = 81
  ClientWidth = 361
  TextHeight = 15
  object chk_IncludeBinary: TCheckBox
    Left = 8
    Top = 16
    Width = 345
    Height = 17
    Caption = 'Include the project'#39's unit output and executable directory'
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 192
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 278
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
