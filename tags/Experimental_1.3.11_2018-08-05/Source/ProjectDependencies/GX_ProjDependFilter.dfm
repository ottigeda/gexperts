object fmProjDependFilter: TfmProjDependFilter
  Left = 333
  Top = 160
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Unit Filter'
  ClientHeight = 262
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlFooter: TPanel
    Left = 0
    Top = 230
    Width = 205
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 23
      Top = 0
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 106
      Top = 0
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlFilters: TPanel
    Left = 0
    Top = 0
    Width = 205
    Height = 230
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlListFooter: TPanel
      Left = 0
      Top = 189
      Width = 205
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnAdd: TButton
        Left = 143
        Top = 5
        Width = 49
        Height = 25
        Caption = '&Add'
        TabOrder = 1
        OnClick = btnAddClick
      end
      object edtUnitName: TEdit
        Left = 8
        Top = 7
        Width = 128
        Height = 22
        TabOrder = 0
        OnChange = edtUnitNameChange
      end
    end
    object pnlUnits: TPanel
      Left = 0
      Top = 0
      Width = 205
      Height = 189
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 0
      object lbFilter: TListBox
        Left = 6
        Top = 23
        Width = 193
        Height = 160
        Align = alClient
        ItemHeight = 14
        MultiSelect = True
        PopupMenu = mnuPopup
        Sorted = True
        TabOrder = 1
      end
      object pnlUnitsHeader: TPanel
        Left = 6
        Top = 6
        Width = 193
        Height = 17
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Units To Hide'
        TabOrder = 0
      end
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Pascal Files (*.pas)|*.pas'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Filter Files'
    Left = 52
    Top = 32
  end
  object mnuPopup: TPopupMenu
    OnPopup = mnuPopupPopup
    Left = 16
    Top = 32
    object mitAddFiles: TMenuItem
      Caption = 'Add Files...'
      ShortCut = 45
      OnClick = mitAddFilesClick
    end
    object mitDeleteFiles: TMenuItem
      Caption = 'Delete Selected...'
      ShortCut = 46
      OnClick = mitDeleteFilesClick
    end
    object mitClear: TMenuItem
      Caption = '&Clear List...'
      ShortCut = 16430
      OnClick = mitClearClick
    end
    object mitSep1: TMenuItem
      Caption = '-'
    end
    object mitSave: TMenuItem
      Caption = 'Save...'
      ShortCut = 16467
      OnClick = mitSaveClick
    end
    object mitLoad: TMenuItem
      Caption = 'Load...'
      ShortCut = 16463
      OnClick = mitLoadClick
    end
  end
  object dlgFilterOpen: TOpenDialog
    DefaultExt = 'flt'
    Filter = 'Filter Files (*.flt)|*.flt'
    Title = 'Load Filter'
    Left = 88
    Top = 32
  end
  object dlgFilterSave: TSaveDialog
    DefaultExt = 'flt'
    Filter = 'Filter Files (*.flt)|*.flt'
    Title = 'Save Filter'
    Left = 124
    Top = 32
  end
end
