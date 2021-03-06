object fmUsesManager: TfmUsesManager
  Left = 311
  Top = 202
  ActiveControl = edtFilter
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Uses Clause Manager'
  ClientHeight = 478
  ClientWidth = 595
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 560
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 266
    Top = 0
    Width = 3
    Height = 441
    Cursor = crHSplit
  end
  object pnlUnits: TPanel
    Left = 269
    Top = 0
    Width = 326
    Height = 441
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 1
    object pcUnits: TPageControl
      Left = 6
      Top = 47
      Width = 314
      Height = 388
      ActivePage = tabSearchPath
      Align = alClient
      TabIndex = 0
      TabOrder = 1
      object tabSearchPath: TTabSheet
        Caption = '&Search Path'
        ImageIndex = 3
        object pnlSearchPathFooter: TPanel
          Left = 0
          Top = 325
          Width = 306
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnSearchPathAddToIntf: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnSearchPathAddToImpl: TButton
            Left = 152
            Top = 4
            Width = 150
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
        object pnlSearchPath: TPanel
          Left = 0
          Top = 0
          Width = 306
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxSearchPath: TListBox
            Left = 3
            Top = 3
            Width = 300
            Height = 319
            Align = alClient
            Color = clBtnFace
            DragMode = dmAutomatic
            Enabled = False
            ItemHeight = 14
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
      end
      object tabProject: TTabSheet
        Caption = '&Project'
        object pnlProject: TPanel
          Left = 0
          Top = 0
          Width = 306
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxProject: TListBox
            Left = 3
            Top = 3
            Width = 300
            Height = 319
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 14
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
        object pnlProjFooter: TPanel
          Left = 0
          Top = 325
          Width = 306
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnProjectAddToInterface: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnProjectAddToImplementation: TButton
            Left = 152
            Top = 4
            Width = 150
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
      end
      object tabCommon: TTabSheet
        Caption = '&VCL/RTL'
        ImageIndex = 1
        object pnlCommon: TPanel
          Left = 0
          Top = 0
          Width = 306
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxCommon: TListBox
            Left = 3
            Top = 3
            Width = 300
            Height = 319
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 14
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
        object pnlCommonFooter: TPanel
          Left = 0
          Top = 325
          Width = 306
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCommonAddToInterface: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnCommonAddToImplementation: TButton
            Left = 152
            Top = 4
            Width = 150
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
      end
      object tabFavorite: TTabSheet
        Caption = '&Favorite'
        ImageIndex = 2
        object pnlFavorite: TPanel
          Left = 0
          Top = 0
          Width = 306
          Height = 296
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxFavorite: TListBox
            Left = 3
            Top = 3
            Width = 300
            Height = 290
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 14
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
        object pnlFavFooter: TPanel
          Left = 0
          Top = 296
          Width = 306
          Height = 63
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnFavoriteAddToInterface: TButton
            Left = 4
            Top = 33
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 2
          end
          object btnFavoriteAddToImplementation: TButton
            Left = 152
            Top = 33
            Width = 150
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 3
          end
          object btnFavoriteAddToFavorites: TButton
            Left = 152
            Top = 3
            Width = 150
            Height = 25
            Action = actFavAdd
            TabOrder = 1
          end
          object btnFavoriteDeleteFromFavorites: TButton
            Left = 4
            Top = 3
            Width = 140
            Height = 25
            Action = actFavDelete
            TabOrder = 0
          end
        end
      end
    end
    object pnlAvailableHeader: TPanel
      Left = 6
      Top = 6
      Width = 314
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      DesignSize = (
        314
        41)
      object lblFilter: TLabel
        Left = 17
        Top = 20
        Width = 26
        Height = 14
        Caption = 'Filte&r'
        FocusControl = edtFilter
      end
      object edtFilter: TEdit
        Left = 48
        Top = 16
        Width = 250
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = edtFilterChange
        OnKeyDown = edtFilterKeyDown
      end
      object lblUnits: TPanel
        Left = 0
        Top = 0
        Width = 314
        Height = 15
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        Caption = 'Available Units'
        ParentColor = True
        TabOrder = 0
      end
    end
  end
  object pnlUses: TPanel
    Left = 0
    Top = 0
    Width = 266
    Height = 441
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 0
    object pcUses: TPageControl
      Left = 6
      Top = 21
      Width = 254
      Height = 414
      ActivePage = tabInterface
      Align = alClient
      TabIndex = 0
      TabOrder = 1
      object tabInterface: TTabSheet
        Caption = 'I&nterface'
        object pnlInterface: TPanel
          Left = 0
          Top = 0
          Width = 246
          Height = 351
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxInterface: TListBox
            Left = 3
            Top = 3
            Width = 240
            Height = 345
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 14
            MultiSelect = True
            PopupMenu = pmuUses
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxInterfaceDblClick
            OnDragDrop = lbxInterfaceDragDrop
            OnDragOver = lbxUsedDragOver
          end
        end
        object pnlIntfFooter: TPanel
          Left = 0
          Top = 351
          Width = 246
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnIntfDelete: TButton
            Left = 4
            Top = 4
            Width = 75
            Height = 25
            Action = actUsesDelete
            TabOrder = 0
          end
          object btnIntfMoveToImpl: TButton
            Left = 88
            Top = 4
            Width = 152
            Height = 25
            Action = actIntfMoveToImpl
            TabOrder = 1
          end
        end
      end
      object tabImplementation: TTabSheet
        Caption = 'I&mplementation'
        ImageIndex = 1
        object pnlImplementation: TPanel
          Left = 0
          Top = 0
          Width = 246
          Height = 351
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxImplementation: TListBox
            Left = 3
            Top = 3
            Width = 240
            Height = 345
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 14
            MultiSelect = True
            PopupMenu = pmuUses
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxImplementationDblClick
            OnDragDrop = lbxImplementationDragDrop
            OnDragOver = lbxUsedDragOver
          end
        end
        object pnlImplFooter: TPanel
          Left = 0
          Top = 351
          Width = 246
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnImplDelete: TButton
            Left = 4
            Top = 4
            Width = 75
            Height = 25
            Action = actUsesDelete
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object btnImplMoveToIntf: TButton
            Left = 88
            Top = 4
            Width = 152
            Height = 25
            Action = actImplMoveToIntf
            TabOrder = 1
          end
        end
      end
    end
    object lblUses: TPanel
      Left = 6
      Top = 6
      Width = 254
      Height = 15
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Caption = 'Used Units '
      ParentColor = True
      TabOrder = 0
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 441
    Width = 595
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    DesignSize = (
      595
      37)
    object chkSingleActionMode: TCheckBox
      Left = 8
      Top = 9
      Width = 233
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Single action/quic&k add mode'
      TabOrder = 0
    end
    object pnlButtonsRight: TPanel
      Left = 272
      Top = 0
      Width = 323
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btnCancel: TButton
        Left = 241
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnOK: TButton
        Left = 157
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnOpen: TButton
        Left = 8
        Top = 4
        Width = 75
        Height = 25
        Action = actOpenUnit
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
    end
  end
  object pmuUses: TPopupMenu
    Left = 104
    Top = 56
    object mitInterfaceDelete: TMenuItem
      Action = actUsesDelete
      Default = True
    end
    object mitUsesMoveToInterface: TMenuItem
      Action = actImplMoveToIntf
    end
    object mitUsesMoveToImplementation: TMenuItem
      Action = actIntfMoveToImpl
    end
    object mitIntfSep1: TMenuItem
      Caption = '-'
    end
    object mitUsesOpenUnit: TMenuItem
      Action = actOpenUnit
    end
    object mitUsesAddToFavorites: TMenuItem
      Action = actUsesAddToFavorites
    end
  end
  object pmuAvail: TPopupMenu
    OnPopup = pmuAvailPopup
    Left = 296
    Top = 80
    object mitAvailAddToIntf: TMenuItem
      Action = actAvailAddToIntf
    end
    object mitAvailAddToImpl: TMenuItem
      Action = actAvailAddToImpl
    end
    object mitAvailSep1: TMenuItem
      Caption = '-'
    end
    object mitAvailOpenUnit: TMenuItem
      Action = actOpenUnit
    end
    object mitAvailSep2: TMenuItem
      Caption = '-'
    end
    object mitAvailAddToFav: TMenuItem
      Action = actFavAdd
    end
    object mitAvailDelFromFav: TMenuItem
      Action = actFavDelete
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Delphi Units (*.pas, *.dcu)|*.dcu;*.pas|All Files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 354
    Top = 80
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 42
    Top = 56
    object actUsesDelete: TAction
      Caption = '&Delete'
      Hint = 'Delete unit from uses list (Double Click)'
      ImageIndex = 42
      ShortCut = 46
      OnExecute = acUsesDeleteExecute
    end
    object actIntfMoveToImpl: TAction
      Caption = 'Move to &Implementation'
      ImageIndex = 36
      ShortCut = 16464
      OnExecute = actIntfMoveToImplExecute
    end
    object actImplMoveToIntf: TAction
      Caption = 'Move to In&terface'
      ImageIndex = 36
      ShortCut = 16468
      OnExecute = actImplMoveToIntfExecute
    end
    object actFavDelete: TAction
      Caption = 'D&elete from Favorites'
      ImageIndex = 42
      ShortCut = 46
      OnExecute = actFavDeleteExecute
    end
    object actFavAdd: TAction
      Caption = '&Add to Favorites...'
      ImageIndex = 41
      ShortCut = 45
      OnExecute = actFavAddExecute
    end
    object actAvailAddToIntf: TAction
      Caption = 'Add to Interfa&ce'
      ShortCut = 16468
      OnExecute = actAvailAddToIntfExecute
    end
    object actAvailAddToImpl: TAction
      Caption = 'Add to Imp&lementation'
      ShortCut = 16464
      OnExecute = actAvailAddToImplExecute
    end
    object actOpenUnit: TAction
      Caption = 'Open Unit'
      Hint = 'Open selected unit (Ctrl+Double Click)'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = actOpenUnitExecute
    end
    object actUsesAddToFavorites: TAction
      Caption = 'Add to Favorites'
      OnExecute = actUsesAddToFavoritesExecute
    end
  end
end
