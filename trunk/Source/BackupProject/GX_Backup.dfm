object fmBackup: TfmBackup
  Left = 318
  Top = 175
  Width = 772
  Height = 539
  BorderIcons = [biSystemMenu]
  Caption = 'Backup Project'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Scaled = False
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object pnlButtons: TPanel
    Left = 0
    Top = 462
    Width = 756
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 490
      Top = 0
      Width = 266
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object btnBackup: TButton
        Left = 18
        Top = 3
        Width = 75
        Height = 26
        Caption = '&Backup'
        Default = True
        TabOrder = 0
        OnClick = btnBackupClick
      end
      object btnCancel: TButton
        Left = 102
        Top = 3
        Width = 75
        Height = 26
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 185
        Top = 3
        Width = 75
        Height = 26
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
  end
  object pnlFiles: TPanel
    Left = 0
    Top = 0
    Width = 756
    Height = 462
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    FullRepaint = False
    TabOrder = 0
    object gbxFiles: TGroupBox
      Left = 7
      Top = 7
      Width = 742
      Height = 448
      Align = alClient
      Caption = 'Files'
      TabOrder = 0
      object pnlFileList: TPanel
        Left = 2
        Top = 16
        Width = 738
        Height = 394
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 6
        FullRepaint = False
        TabOrder = 0
        object tvFiles: TListView
          Left = 6
          Top = 6
          Width = 726
          Height = 382
          Align = alClient
          Columns = <>
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnKeyDown = tvFilesKeyDown
        end
      end
      object pnlFileButtons: TPanel
        Left = 2
        Top = 410
        Width = 738
        Height = 36
        Align = alBottom
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        DesignSize = (
          738
          36)
        object pnlFileButtonsRight: TPanel
          Left = 642
          Top = 0
          Width = 96
          Height = 36
          Align = alRight
          BevelOuter = bvNone
          FullRepaint = False
          TabOrder = 2
          object btnOptions: TButton
            Left = 14
            Top = 2
            Width = 75
            Height = 26
            Caption = '&Options...'
            TabOrder = 0
            OnClick = btnOptionsClick
          end
        end
        object btnRemove: TButton
          Left = 89
          Top = 2
          Width = 75
          Height = 26
          Anchors = [akLeft, akBottom]
          Caption = '&Remove'
          TabOrder = 1
          OnClick = btnRemoveClick
        end
        object btnAdd: TButton
          Left = 6
          Top = 2
          Width = 75
          Height = 26
          Anchors = [akLeft, akBottom]
          Caption = '&Add...'
          TabOrder = 0
          OnClick = btnAddClick
        end
      end
    end
  end
end
