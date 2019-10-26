object fmUsesManager: TfmUsesManager
  Left = 311
  Top = 202
  ActiveControl = edtUnitFilter
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Uses Clause Manager'
  ClientHeight = 490
  ClientWidth = 922
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 297
    Top = 0
    Height = 453
    OnMoved = SplitterMoved
  end
  object pnlUnits: TPanel
    Left = 300
    Top = 0
    Width = 622
    Height = 453
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 1
    object pcUnits: TPageControl
      Left = 6
      Top = 47
      Width = 610
      Height = 400
      ActivePage = tabIdentifiers
      Align = alClient
      TabOrder = 1
      OnChange = pcUnitsChange
      OnResize = pcUnitsResize
      object tabSearchPath: TTabSheet
        Caption = '&Search Path'
        ImageIndex = 3
        object pnlSearchPathFooter: TPanel
          Left = 0
          Top = 340
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnSearchPathAddToIntf: TButton
            Left = 8
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnSearchPathAddToImpl: TButton
            Left = 160
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnAddSearchPathlToFavorites: TButton
            Left = 312
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddAllToFav
            TabOrder = 2
          end
        end
        object pnlSearchPath: TPanel
          Left = 0
          Top = 0
          Width = 602
          Height = 340
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_SearchPath: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 334
            Align = alClient
            Color = clBtnFace
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            Enabled = False
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pmuAvail
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
      end
      object tabProject: TTabSheet
        Caption = '&Project'
        object pnlProject: TPanel
          Left = 0
          Top = 0
          Width = 602
          Height = 340
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Project: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 334
            Align = alClient
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pmuAvail
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
        object pnlProjFooter: TPanel
          Left = 0
          Top = 340
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCopySaveProjectListMenu: TSpeedButton
            Left = 568
            Top = 0
            Width = 25
            Height = 25
            Glyph.Data = {
              AE000000424DAE00000000000000360000002800000008000000050000000100
              18000000000078000000C30E0000C30E00000000000000000000FF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF
              00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00FF000000
              000000000000000000000000000000FF00FF}
            Layout = blGlyphBottom
            Margin = 6
            Spacing = 0
            OnClick = btnCopySaveProjectListMenuClick
          end
          object btnProjectAddToInterface: TButton
            Left = 8
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnProjectAddToImplementation: TButton
            Left = 158
            Top = 0
            Width = 154
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnAddProjectToFavorites: TButton
            Left = 317
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddAllToFav
            TabOrder = 2
          end
          object btnCopySaveCurrentList: TButton
            Left = 467
            Top = 0
            Width = 104
            Height = 25
            Hint = 'Copy the list of Project units to the clipboard.'
            Caption = 'Copy/Save List'
            TabOrder = 3
            OnClick = btnCopySaveProjectListClick
          end
        end
      end
      object tabCommon: TTabSheet
        Caption = '&VCL/RTL'
        ImageIndex = 1
        object pnlCommon: TPanel
          Left = 0
          Top = 0
          Width = 602
          Height = 340
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Common: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 334
            Align = alClient
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pmuAvail
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
        object pnlCommonFooter: TPanel
          Left = 0
          Top = 340
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCommonAddToInterface: TButton
            Left = 8
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnCommonAddToImplementation: TButton
            Left = 160
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnAddRtlToFavorites: TButton
            Left = 312
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddAllToFav
            TabOrder = 2
          end
        end
      end
      object tabFavorite: TTabSheet
        Caption = '&Favorite'
        ImageIndex = 2
        object pnlFavorite: TPanel
          Left = 0
          Top = 0
          Width = 602
          Height = 340
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Favorite: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 334
            Align = alClient
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pm_Favorite
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
        object pnlFavFooter: TPanel
          Left = 0
          Top = 340
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnFavoriteAddToInterface: TButton
            Left = 8
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnFavoriteAddToImplementation: TButton
            Left = 160
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnFavoriteAddToFavorites: TButton
            Left = 312
            Top = 0
            Width = 73
            Height = 25
            Action = actFavAddUnit
            TabOrder = 2
          end
          object btnFavoriteDeleteFromFavorites: TButton
            Left = 392
            Top = 0
            Width = 73
            Height = 25
            Action = actFavDelUnit
            TabOrder = 3
          end
        end
      end
      object tabIdentifiers: TTabSheet
        Caption = 'Identifiers'
        ImageIndex = 4
        object sg_Identifiers: TStringGrid
          Left = 0
          Top = 0
          Width = 602
          Height = 337
          Align = alClient
          ColCount = 2
          DefaultColWidth = 150
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect]
          TabOrder = 0
          OnDrawCell = sg_AvailDrawCell
          OnMouseDown = sg_MouseDownForDragging
        end
        object pnlIdentifiersFooter: TPanel
          Left = 0
          Top = 337
          Width = 602
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnIdentifiersAddToIntf: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnIdentifiersAddToImpl: TButton
            Left = 152
            Top = 4
            Width = 157
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object pnlMatchIdentifier: TPanel
            Left = 313
            Top = 2
            Width = 255
            Height = 30
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 2
            object rbMatchAnyware: TRadioButton
              Left = 4
              Top = 6
              Width = 125
              Height = 17
              Caption = 'Match anywhere'
              Checked = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              TabStop = True
              OnClick = rbMatchAnywareClick
            end
            object rbMatchAtStart: TRadioButton
              Left = 138
              Top = 6
              Width = 113
              Height = 17
              Caption = 'Match at start'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              OnClick = rbMatchAtStartClick
            end
          end
        end
        object pnlIdentifiersProgress: TPanel
          Left = 152
          Top = 160
          Width = 193
          Height = 113
          TabOrder = 2
          Visible = False
          object lblIdentifiers: TLabel
            Left = 16
            Top = 88
            Width = 117
            Height = 14
            Caption = 'Identifiers found: %d'
          end
          object lblUnitsParsed: TLabel
            Left = 16
            Top = 40
            Width = 94
            Height = 14
            Caption = 'Units parsed: %d'
          end
          object lblUnitsLoaded: TLabel
            Left = 16
            Top = 64
            Width = 94
            Height = 14
            Caption = 'Units loaded: %d'
          end
          object lblUnitsFound: TLabel
            Left = 16
            Top = 16
            Width = 90
            Height = 14
            Caption = 'Units found: %d'
          end
        end
      end
    end
    object pnlAvailableHeader: TPanel
      Left = 6
      Top = 6
      Width = 610
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      OnResize = pnlAvailableHeaderResize
      DesignSize = (
        610
        41)
      object edtUnitFilter: TEdit
        Left = 200
        Top = 16
        Width = 408
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = edtUnitFilterChange
        OnEnter = edtUnitFilterEnter
        OnExit = edtUnitFilterExit
        OnKeyDown = edtUnitFilterKeyDown
      end
      object lblUnits: TPanel
        Left = 0
        Top = 0
        Width = 610
        Height = 15
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Available Units'
        ParentColor = True
        TabOrder = 0
        object lblFilter: TLabel
          Left = 1
          Top = 0
          Width = 26
          Height = 14
          Caption = 'Filte&r'
          FocusControl = edtUnitFilter
        end
      end
      object edtIdentifierFilter: TEdit
        Left = 0
        Top = 16
        Width = 367
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Visible = False
        OnChange = edtIdentifierFilterChange
        OnEnter = edtIdentifierFilterEnter
        OnExit = edtIdentifierFilterExit
        OnKeyDown = edtIdentifierFilterKeyDown
      end
    end
  end
  object pnlUses: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 453
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 0
    OnResize = pnlUsesResize
    object p_Interface: TPanel
      Left = 6
      Top = 21
      Width = 139
      Height = 359
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      OnResize = pcUsesResize
      object p_InterfaceTitle: TPanel
        Left = 0
        Top = 0
        Width = 139
        Height = 15
        Align = alTop
        BevelOuter = bvNone
        Caption = 'I&nterface'
        ParentColor = True
        TabOrder = 1
      end
      object sg_Interface: TStringGrid
        Left = 0
        Top = 15
        Width = 139
        Height = 344
        Align = alClient
        ColCount = 1
        DefaultColWidth = 100
        DefaultRowHeight = 16
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
        PopupMenu = pm_Intf
        TabOrder = 0
        OnDblClick = sg_InterfaceDblClick
        OnDragDrop = sg_InterfaceDragDrop
        OnDragOver = sg_InterfaceDragOver
        OnDrawCell = sg_UsedDrawCell
        OnMouseDown = sg_MouseDownForDragging
      end
    end
    object p_Implementation: TPanel
      Left = 145
      Top = 21
      Width = 146
      Height = 359
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 3
      OnResize = pcUsesResize
      object p_ImplementationTitle: TPanel
        Left = 0
        Top = 0
        Width = 146
        Height = 15
        Align = alTop
        BevelOuter = bvNone
        Caption = 'I&mplementation'
        ParentColor = True
        TabOrder = 1
      end
      object sg_Implementation: TStringGrid
        Left = 0
        Top = 15
        Width = 146
        Height = 344
        Align = alClient
        ColCount = 1
        DefaultColWidth = 100
        DefaultRowHeight = 16
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
        PopupMenu = pm_Impl
        TabOrder = 0
        OnDblClick = sg_ImplementationDblClick
        OnDragDrop = sg_ImplementationDragDrop
        OnDragOver = sg_ImplementationDragOver
        OnDrawCell = sg_UsedDrawCell
        OnMouseDown = sg_MouseDownForDragging
      end
    end
    object lblUses: TPanel
      Left = 6
      Top = 6
      Width = 285
      Height = 15
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Used Units '
      ParentColor = True
      TabOrder = 0
    end
    object pnlUsesBottom: TPanel
      Left = 6
      Top = 380
      Width = 285
      Height = 67
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      OnResize = pnlUsesBottomResize
      object btnAddDots: TButton
        Left = 0
        Top = 36
        Width = 137
        Height = 25
        Caption = 'Add Unit Prefixes'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        OnClick = btnAddDotsClick
      end
      object btnRemoveDots: TButton
        Left = 144
        Top = 36
        Width = 137
        Height = 25
        Caption = 'Remove Unit Prefixes'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        OnClick = btnRemoveDotsClick
      end
      object b_DeleteFromIntf: TButton
        Left = 0
        Top = 8
        Width = 65
        Height = 25
        Action = actIntfDelete
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object b_DeleteFromImpl: TButton
        Left = 216
        Top = 8
        Width = 65
        Height = 25
        Action = actImplDelete
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object b_MoveToImpl: TButton
        Left = 72
        Top = 8
        Width = 65
        Height = 25
        Action = actIntfMove
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object b_MoveToIntf: TButton
        Left = 144
        Top = 8
        Width = 65
        Height = 25
        Action = actImplMove
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 453
    Width = 922
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    object pnlButtonsRight: TPanel
      Left = 567
      Top = 0
      Width = 355
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnCancel: TButton
        Left = 273
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnOK: TButton
        Left = 189
        Top = 4
        Width = 75
        Height = 25
        Action = actOK
        Default = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object btnOpen: TButton
        Left = 72
        Top = 4
        Width = 107
        Height = 25
        Action = actOpenUnit
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  object pm_Intf: TPopupMenu
    Left = 104
    Top = 56
    object m_IntfDelete: TMenuItem
      Action = actIntfDelete
      Default = True
    end
    object m_IntfMove: TMenuItem
      Action = actIntfMove
    end
    object m_IntfSep1: TMenuItem
      Caption = '-'
    end
    object m_IntfOpenUnit: TMenuItem
      Action = actOpenUnit
    end
    object m_IntfAddToFavorites: TMenuItem
      Action = actIntfAddToFavorites
    end
    object m_IntfSep2: TMenuItem
      Caption = '-'
    end
    object m_IntfUnalias: TMenuItem
      Action = actUnAlias
    end
  end
  object pm_Impl: TPopupMenu
    Left = 200
    Top = 56
    object m_ImplDelete: TMenuItem
      Action = actImplDelete
      Default = True
    end
    object m_ImplMove: TMenuItem
      Action = actImplMove
    end
    object m_ImplSep: TMenuItem
      Caption = '-'
    end
    object m_ImplOpenUnit: TMenuItem
      Action = actOpenUnit
    end
    object m_ImplAddToFavorites: TMenuItem
      Action = actImplAddToFavorites
    end
    object m_ImplSep2: TMenuItem
      Caption = '-'
    end
    object m_ImplUnAlias: TMenuItem
      Action = actUnAlias
    end
  end
  object pmuAvail: TPopupMenu
    Left = 456
    Top = 112
    object mitAvailAddToUses: TMenuItem
      Action = actAvailAddToImpl
      Default = True
    end
    object mi_AvailAddToIntf: TMenuItem
      Action = actAvailAddToIntf
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
      Action = actAvailAddToFav
    end
    object mitAvailDelFromFav: TMenuItem
      Action = actFavDelUnit
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
    object actIntfDelete: TAction
      Category = 'Intf'
      Caption = 'Delete'
      Hint = 'Delete from Interface'
      OnExecute = actIntfDeleteExecute
    end
    object actImplDelete: TAction
      Category = 'Impl'
      Caption = 'Delete'
      Hint = 'Delete from Implementation'
      OnExecute = actImplDeleteExecute
    end
    object actIntfMove: TAction
      Category = 'Intf'
      Caption = 'Move ->'
      Hint = 'Move to Implementation'
      OnExecute = actIntfMoveExecute
    end
    object actImplMove: TAction
      Category = 'Impl'
      Caption = '<- Move'
      Hint = 'Move to Interface'
      OnExecute = actImplMoveExecute
    end
    object actFavDelUnit: TAction
      Category = 'Fav'
      Caption = 'D&elete Unit'
      ImageIndex = 42
      OnExecute = actFavDelUnitExecute
    end
    object actAvailAddToFav: TAction
      Category = 'Avail'
      Caption = '&Add to Favorites...'
      ImageIndex = 41
      OnExecute = actAvailAddToFavExecute
    end
    object actAvailAddToIntf: TAction
      Category = 'Avail'
      Caption = 'Add to Interfa&ce'
      OnExecute = actAvailAddToIntfExecute
    end
    object actAvailAddToImpl: TAction
      Category = 'Avail'
      Caption = 'Add to Imp&lementation'
      OnExecute = actAvailAddToImplExecute
    end
    object actOpenUnit: TAction
      Caption = 'Open Unit'
      Hint = 'Open selected unit (Ctrl+Double Click)'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = actOpenUnitExecute
    end
    object actIntfAddToFavorites: TAction
      Category = 'Intf'
      Caption = 'Add to Favorites'
      OnExecute = actIntfAddToFavoritesExecute
    end
    object actImplAddToFavorites: TAction
      Category = 'Impl'
      Caption = 'Add to Favorites'
      OnExecute = actImplAddToFavoritesExecute
    end
    object actUnAlias: TAction
      Caption = 'Unalias ...'
      OnExecute = actIntfUnAliasExecute
    end
    object actOK: TAction
      Caption = 'OK'
      Hint = 'OK'
      ShortCut = 16397
      OnExecute = actOKExecute
    end
    object actFocusInterface: TAction
      Caption = 'Focus Interface'
      ShortCut = 32846
      OnExecute = actFocusInterfaceExecute
    end
    object actFocusImplementation: TAction
      Caption = 'Focus Imlementation'
      ShortCut = 32845
      OnExecute = actFocusImplementationExecute
    end
    object actFavAddUnit: TAction
      Category = 'Fav'
      Caption = 'Add Unit ...'
      OnExecute = actFavAddUnitExecute
    end
    object actAvailAddAllToFav: TAction
      Category = 'Avail'
      Caption = 'Add all to Favorite'
      OnExecute = actAvailAddAllToFavExecute
    end
  end
  object tim_Progress: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tim_ProgressTimer
    Left = 376
    Top = 248
  end
  object pm_Favorite: TPopupMenu
    Left = 560
    Top = 112
    object mi_FavAddToImpl: TMenuItem
      Action = actAvailAddToImpl
      Default = True
    end
    object mi_FavAddtoIntf: TMenuItem
      Action = actAvailAddToIntf
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object OpenUnit1: TMenuItem
      Action = actOpenUnit
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mi_FavAddUnit: TMenuItem
      Action = actFavAddUnit
    end
    object mi_FavDelUnit: TMenuItem
      Action = actFavDelUnit
    end
  end
  object pmCopySaveProjectList: TPopupMenu
    Images = ImageList1
    Left = 488
    Top = 240
    object mCopytoClipboard1: TMenuItem
      Caption = 'Copy the List of Project Units to the Clipboard   '
      ImageIndex = 0
      OnClick = mCopytoClipboard1Click
    end
    object Save1: TMenuItem
      Caption = 'Save the List of Project Units to Disk...'
      ImageIndex = 1
      OnClick = Save1Click
    end
  end
  object ImageList1: TImageList
    Left = 600
    Top = 240
    Bitmap = {
      494C010102000800080010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000090909000777777007777770077777700777777007777
      770077777700777777007777770090909000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB0077777700B881A9009E5189009D5188009D51
      88009D5188009D5188009D5188009D5188009D5188009D5188009D5188009D51
      88009E518900B073A000000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000077777700FBFBFB00B0773C00B0773C00B0773C00B077
      3C00B0773C00B0773C00FBFBFB00777777009E5289009D518800F4EFF300FBFB
      FB00F8F7F800F3F0F200F6F5F600FBFBFB00FBFBFB00FBFBFB00FBFBFB00F5F0
      F3009E5189009E528900000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00F8F8F800CECCCC00EBEBEB00F2F2F200F8F8F800FBFBFB00FBFBFB00FBFB
      FB009E5189009D518800000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B9B9B900A7A7A700A7A7A700A7A7
      A700A7A7A7000000000077777700FBFBFB00B0773C00B0773C00B0773C00B077
      3C00B0773C00B0773C00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00F8F8F800E7E7E700706D6C00B1AFAE00000000FFF5F5F500FBFBFB00FBFB
      FB009E5189009D518800000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00FAFAFA00F6F6F600AAA9A800BCBABA008F8D8C00000000FFF5F5F500FBFB
      FB009E5189009D518800000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00D7A56E00D7A5
      6E00D7A56E000000000077777700FBFBFB00B0773C00B0773C00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00FBFBFB00F9F9F900000000FF8E8B8B006F6C6B008B888800000000FFF3F2
      F3009E5189009D518800000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB008C8C
      8C007777770077777700777777007E7E7E009D5188009D518800F5F0F400FBFB
      FB00FBFBFB00FBFBFB00F6F4F500000000FF858282006F6C6B008C8988000000
      00FFB989AC009D518800000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00D7A56E00D7A5
      6E00D7A56E000000000077777700FBFBFB00B0773C00B0773C00FBFBFB007777
      7700FBFBFB00DDDDDD0082828200D2D2D2009D5188009D5188009D5188009D51
      88009D5188009D5188009D518800C092B300000000FF858282006F6C6B008C88
      8800000000FFB887AA00000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB007777
      7700D8D8D80082828200D8D8D800000000009D5188009D5188009D5188009D51
      88009D5188009D5188009D5188009D518800C195B500000000FF8C8988006F6C
      6B008B888800000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00D7A56E00D7A5
      6E00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB007777
      770082828200DCDCDC0000000000000000009D5188009D5188009D518800EFE6
      ED00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00ECECEC008B88
      88006F6C6B008B888700000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000091919100777777007777770077777700777777008686
      8600E2E2E2000000000000000000000000009D5188009D5188009D518800FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00726F6E00726F6E00FBFBFB00FBFBFB000000
      00FF858182006F6C6B0098969600000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00D7A56E00D7A5
      6E00FBFBFB00E5E5E50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009D5188009D5188009D518800FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00726F6E00726F6E00FBFBFB00FBFBFB00C195
      B500000000FF928E8F00BCBBBA00928F8F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00A7A7A700E5E5E500AEAEAE00E4E4E40000000000000000000000
      0000000000000000000000000000000000009F538A009D5188009D518800FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB009E51
      8900C091B300000000FF89858400A9A7A7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00A7A7A700AEAEAE00E8E8E8000000000000000000000000000000
      000000000000000000000000000000000000B67DA6009E5189009D5188009D51
      88009D5188009D5188009D5188009D5188009D5188009D5188009D5188009F55
      8B00DABED200000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B9B9B900A7A7A700A7A7A700A7A7
      A700A7A7A700B1B1B100EBEBEB00000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FC00FFFF00000000FC00000300000000
      FC00000300000000FC0000030000000004000083000000000400004300000000
      040002230000000004000113000000000400008B000000000401004700000000
      0403000300000000040700110000000003FF000800000000007F000400000000
      00FF00070000000001FFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object dlgSave: TSaveDialog
    Left = 672
    Top = 240
  end
end
