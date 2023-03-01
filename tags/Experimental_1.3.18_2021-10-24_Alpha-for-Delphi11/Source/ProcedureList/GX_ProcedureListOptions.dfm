object fmProcedureListOptions: TfmProcedureListOptions
  Left = 410
  Top = 219
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Procedure List Configuration'
  ClientHeight = 194
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 400
    Top = 160
    Width = 75
    Height = 26
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 488
    Top = 160
    Width = 75
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object gbCodeView: TGroupBox
    Left = 288
    Top = 8
    Width = 273
    Height = 145
    Caption = 'Code View'
    TabOrder = 1
    object lblDock: TLabel
      Left = 16
      Top = 50
      Width = 27
      Height = 14
      Caption = 'Dock'
      FocusControl = cbCVDock
    end
    object cbCVDock: TComboBox
      Left = 64
      Top = 46
      Width = 145
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 1
      OnChange = cbCVDockChange
      Items.Strings = (
        'Top'
        'Left'
        'Right'
        'Bottom')
    end
    object grp_CvFont: TGroupBox
      Left = 8
      Top = 88
      Width = 257
      Height = 49
      Caption = 'Font'
      TabOrder = 2
      object pnlCVFont: TPanel
        Left = 8
        Top = 16
        Width = 145
        Height = 23
        Caption = 'AaBbYyZz'
        TabOrder = 1
      end
      object btnChangeCodeViewFont: TButton
        Left = 160
        Top = 15
        Width = 85
        Height = 25
        Caption = 'Change ...'
        TabOrder = 0
        OnClick = btnChangeCodeViewFontClick
      end
    end
    object chkShowCodeView: TCheckBox
      Left = 16
      Top = 22
      Width = 113
      Height = 17
      Caption = 'Show code view'
      TabOrder = 0
    end
  end
  object gbDialog: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 145
    Caption = 'Procedure List'
    TabOrder = 0
    object grp_ListFont: TGroupBox
      Left = 8
      Top = 88
      Width = 257
      Height = 49
      Caption = 'Font'
      TabOrder = 3
      object pnlDialogFont: TPanel
        Left = 8
        Top = 16
        Width = 145
        Height = 25
        Caption = 'AaBbYyZz'
        TabOrder = 1
      end
      object btnChgDialogFont: TButton
        Left = 160
        Top = 16
        Width = 85
        Height = 25
        Caption = 'Change ...'
        TabOrder = 0
        OnClick = btnChgDialogFontClick
      end
    end
    object chkShowObjectName: TCheckBox
      Left = 16
      Top = 22
      Width = 246
      Height = 17
      Caption = 'Show object names'
      TabOrder = 0
    end
    object chkMatchAnywhere: TCheckBox
      Left = 16
      Top = 65
      Width = 246
      Height = 17
      Caption = 'Match anywhere in the name'
      TabOrder = 2
    end
    object chkMatchClass: TCheckBox
      Left = 16
      Top = 43
      Width = 246
      Height = 17
      Caption = 'Match in both class and method names'
      TabOrder = 1
    end
  end
end
