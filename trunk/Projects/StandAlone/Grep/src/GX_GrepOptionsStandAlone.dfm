object fmGrepGrepOptionsStandAlone: TfmGrepGrepOptionsStandAlone
  Left = 294
  Top = 263
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Stand Alone Options'
  ClientHeight = 430
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 336
    Top = 360
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 416
    Top = 360
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object grp_WindowsExplorer: TGroupBox
    Left = 8
    Top = 264
    Width = 481
    Height = 81
    Caption = 'File Explorer Context Menu'
    TabOrder = 1
    object chk_ExplorerMenuBackground: TCheckBox
      Left = 8
      Top = 24
      Width = 465
      Height = 17
      Caption = 'Add entry to tree view and background'
      TabOrder = 0
    end
    object chk_ExplorerMenuFolder: TCheckBox
      Left = 8
      Top = 48
      Width = 465
      Height = 17
      Caption = 'Add entry to folders'
      TabOrder = 1
    end
  end
  object grp_ExternalEditor: TGroupBox
    Left = 8
    Top = 8
    Width = 481
    Height = 233
    Caption = 'External Editor'
    TabOrder = 0
    DesignSize = (
      481
      233)
    object l_ExternalEditorParameters: TLabel
      Left = 8
      Top = 144
      Width = 61
      Height = 14
      Caption = 'Parameters'
    end
    object l_ExternalEditorExe: TLabel
      Left = 8
      Top = 96
      Width = 60
      Height = 14
      Caption = 'Executable'
    end
    object b_ExternalEditorParameters: TButton
      Left = 8
      Top = 192
      Width = 113
      Height = 25
      Caption = 'Parameters'
      TabOrder = 3
    end
    object ed_ExternalEditorParameters: TEdit
      Left = 8
      Top = 160
      Width = 465
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object ed_ExternalEditorExe: TEdit
      Left = 8
      Top = 112
      Width = 433
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object b_ExternalEditorExe: TButton
      Left = 448
      Top = 111
      Width = 25
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = b_ExternalEditorExeClick
    end
    object grp_Presets: TGroupBox
      Left = 16
      Top = 24
      Width = 449
      Height = 57
      Caption = 'Presets'
      TabOrder = 4
      object b_Notepad: TButton
        Left = 248
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Notepad'
        TabOrder = 0
        OnClick = b_NotepadClick
      end
      object b_NotepadPP: TButton
        Left = 8
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Notepad++'
        TabOrder = 1
        OnClick = b_NotepadPPClick
      end
      object b_Notepad2: TButton
        Left = 88
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Notepad2'
        TabOrder = 2
        OnClick = b_Notepad2Click
      end
      object b_Associated: TButton
        Left = 328
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Default'
        TabOrder = 3
      end
      object b_ProgrammersNotepad: TButton
        Left = 168
        Top = 24
        Width = 75
        Height = 25
        Caption = 'pn'
        TabOrder = 4
        OnClick = b_ProgrammersNotepadClick
      end
    end
  end
  object pm_Parameters: TPopupMenu
    Left = 184
    Top = 184
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
