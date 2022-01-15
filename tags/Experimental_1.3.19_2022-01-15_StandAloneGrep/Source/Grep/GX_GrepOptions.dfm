object fmGrepOptions: TfmGrepOptions
  Left = 294
  Top = 263
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Search Options'
  ClientHeight = 377
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  DesignSize = (
    529
    377)
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 360
    Top = 344
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 440
    Top = 344
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object chkUseCurrentIdent: TCheckBox
    Left = 8
    Top = 16
    Width = 513
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use &current editor identifier as the default search string'
    TabOrder = 0
  end
  object grp_StandAlone: TGroupBox
    Left = 8
    Top = 48
    Width = 513
    Height = 281
    Caption = 'Stand Alone Options'
    TabOrder = 1
    object grp_WindowsExplorer: TGroupBox
      Left = 16
      Top = 184
      Width = 481
      Height = 81
      Caption = 'Windows Explorer Context Menu'
      TabOrder = 1
      object chk_ExplorerMenuBackground: TCheckBox
        Left = 8
        Top = 24
        Width = 465
        Height = 17
        Caption = 'Tree view and background'
        TabOrder = 0
      end
      object chk_ExplorerMenuFolder: TCheckBox
        Left = 8
        Top = 48
        Width = 465
        Height = 17
        Caption = 'Folder entries'
        TabOrder = 1
      end
    end
    object grp_ExternalEditor: TGroupBox
      Left = 16
      Top = 24
      Width = 481
      Height = 153
      Caption = 'External Editor'
      TabOrder = 0
      DesignSize = (
        481
        153)
      object l_ExternalEditorParameters: TLabel
        Left = 8
        Top = 72
        Width = 61
        Height = 14
        Caption = 'Parameters'
      end
      object l_ExternalEditorExe: TLabel
        Left = 8
        Top = 24
        Width = 60
        Height = 14
        Caption = 'Executable'
      end
      object b_ExternalEditorParameters: TButton
        Left = 8
        Top = 120
        Width = 113
        Height = 25
        Caption = 'Parameters'
        TabOrder = 3
      end
      object ed_ExternalEditorParameters: TEdit
        Left = 8
        Top = 88
        Width = 465
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object ed_ExternalEditorExe: TEdit
        Left = 8
        Top = 40
        Width = 433
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object b_ExternalEditorExe: TButton
        Left = 448
        Top = 39
        Width = 25
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = b_ExternalEditorExeClick
      end
    end
  end
  object pm_Parameters: TPopupMenu
    Left = 192
    Top = 88
    object mi_File: TMenuItem
      Caption = '{FILE}'
      OnClick = mi_FileClick
    end
    object mi_Line: TMenuItem
      Caption = '{LINE}'
      OnClick = mi_LineClick
    end
    object mi_Column: TMenuItem
      Caption = '{COLUMN}'
      OnClick = mi_ColumnClick
    end
  end
end
