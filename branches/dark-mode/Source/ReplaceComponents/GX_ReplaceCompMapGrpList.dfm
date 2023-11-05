object fmReplaceCompMapGrpList: TfmReplaceCompMapGrpList
  Left = 279
  Top = 250
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Mapping Groups'
  ClientHeight = 286
  ClientWidth = 355
  Color = clBtnFace
  Constraints.MinWidth = 265
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 355
    Height = 250
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 261
      Top = 3
      Width = 91
      Height = 244
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object btnAdd: TButton
        Left = 7
        Top = 11
        Width = 79
        Height = 25
        Caption = 'Add...'
        TabOrder = 0
        OnClick = btnAddClick
      end
      object btnEdit: TButton
        Left = 7
        Top = 40
        Width = 79
        Height = 25
        Caption = 'Edit...'
        TabOrder = 1
        OnClick = btnEditClick
      end
      object btnDelete: TButton
        Left = 7
        Top = 69
        Width = 79
        Height = 25
        Caption = 'Delete...'
        TabOrder = 2
        OnClick = btnDeleteClick
      end
      object btnExport: TButton
        Left = 7
        Top = 127
        Width = 79
        Height = 25
        Caption = 'Export...'
        TabOrder = 4
        OnClick = btnExportClick
      end
      object btnImport: TButton
        Left = 7
        Top = 98
        Width = 79
        Height = 25
        Caption = 'Import...'
        TabOrder = 3
        OnClick = btnImportClick
      end
      object btnClear: TButton
        Left = 7
        Top = 156
        Width = 79
        Height = 25
        Caption = 'Clear...'
        TabOrder = 5
        OnClick = btnClearClick
      end
    end
    object pnlList: TPanel
      Left = 3
      Top = 3
      Width = 258
      Height = 244
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lbxGroups: TListBox
        Left = 3
        Top = 3
        Width = 252
        Height = 238
        Align = alClient
        Constraints.MinHeight = 180
        Constraints.MinWidth = 127
        ItemHeight = 14
        MultiSelect = True
        TabOrder = 0
        OnClick = lbxGroupsClick
        OnDblClick = lbxGroupsDblClick
      end
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 250
    Width = 355
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 31
    Constraints.MinWidth = 237
    FullRepaint = False
    TabOrder = 1
    object pnlFooterButtons: TPanel
      Left = 91
      Top = 0
      Width = 264
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object btnOk: TButton
        Left = 96
        Top = 4
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object btnCancel: TButton
        Left = 180
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object dlgGetImportFile: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select an import file'
    Left = 48
    Top = 144
  end
  object dlgGetExportFile: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Select a file write the the exported data to'
    Left = 136
    Top = 144
  end
end
