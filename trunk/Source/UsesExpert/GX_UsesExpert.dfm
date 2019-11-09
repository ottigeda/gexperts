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
    Height = 434
    OnMoved = SplitterMoved
  end
  object pnlUnits: TPanel
    Left = 300
    Top = 0
    Width = 622
    Height = 434
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 1
    object pcUnits: TPageControl
      Left = 6
      Top = 47
      Width = 610
      Height = 381
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
          Top = 321
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnSearchPathAddToIntf: TButton
            Left = 4
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnSearchPathAddToImpl: TButton
            Left = 154
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnAddSearchPathlToFavorites: TButton
            Left = 304
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
          Height = 321
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_SearchPath: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 315
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
            OnSelectCell = sg_SearchPathSelectCell
          end
        end
      end
      object tabProject: TTabSheet
        Caption = '&Project'
        object pnlProject: TPanel
          Left = 0
          Top = 0
          Width = 602
          Height = 321
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Project: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 282
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
            OnSelectCell = sg_ProjectSelectCell
          end
          object p_NoMapFile: TPanel
            Left = 3
            Top = 285
            Width = 596
            Height = 33
            Align = alBottom
            BevelOuter = bvNone
            Color = clYellow
            TabOrder = 1
            Visible = False
            DesignSize = (
              596
              33)
            object l_NoMapFile: TLabel
              Left = 0
              Top = 0
              Width = 596
              Height = 33
              Align = alClient
              Caption = 
                'No .map file was found, reading units from .dpr file. Enable map' +
                ' file generation in the project'#39's Linker options.'
              WordWrap = True
            end
            object b_NoMapFileClose: TButton
              Left = 571
              Top = 0
              Width = 25
              Height = 33
              Anchors = [akTop, akRight, akBottom]
              Caption = 'X'
              TabOrder = 0
              OnClick = b_NoMapFileCloseClick
            end
          end
        end
        object pnlProjFooter: TPanel
          Left = 0
          Top = 321
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCopySaveProjectListMenu: TSpeedButton
            Left = 574
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
            Left = 4
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnProjectAddToImplementation: TButton
            Left = 154
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnAddProjectToFavorites: TButton
            Left = 304
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddAllToFav
            TabOrder = 2
          end
          object btnCopySaveCurrentList: TButton
            Left = 454
            Top = 0
            Width = 123
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
          Height = 321
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Common: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 315
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
            OnSelectCell = sg_CommonSelectCell
          end
        end
        object pnlCommonFooter: TPanel
          Left = 0
          Top = 321
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCommonAddToInterface: TButton
            Left = 4
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnCommonAddToImplementation: TButton
            Left = 154
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnAddRtlToFavorites: TButton
            Left = 304
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
          Height = 321
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Favorite: TStringGrid
            Left = 3
            Top = 3
            Width = 596
            Height = 315
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
            OnSelectCell = sg_FavoriteSelectCell
          end
        end
        object pnlFavFooter: TPanel
          Left = 0
          Top = 321
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnFavoriteAddToInterface: TButton
            Left = 4
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnFavoriteAddToImplementation: TButton
            Left = 154
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnFavoriteAddToFavorites: TButton
            Left = 304
            Top = 0
            Width = 145
            Height = 25
            Action = actFavAddUnit
            TabOrder = 2
          end
          object btnFavoriteDeleteFromFavorites: TButton
            Left = 454
            Top = 0
            Width = 145
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
          Height = 321
          Align = alClient
          ColCount = 2
          DefaultColWidth = 150
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect]
          PopupMenu = pmuAvail
          TabOrder = 0
          OnDblClick = lbxAvailDblClick
          OnDrawCell = sg_AvailDrawCell
          OnMouseDown = sg_MouseDownForDragging
          OnSelectCell = sg_IdentifiersSelectCell
        end
        object pnlIdentifiersFooter: TPanel
          Left = 0
          Top = 321
          Width = 602
          Height = 31
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnIdentifiersAddToIntf: TButton
            Left = 4
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnIdentifiersAddToImpl: TButton
            Left = 154
            Top = 0
            Width = 145
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object pnlMatchIdentifier: TPanel
            Left = 304
            Top = 0
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
              Caption = 'Match &anywhere'
              Checked = True
              TabOrder = 0
              TabStop = True
              OnClick = rbMatchAnywareClick
            end
            object rbMatchAtStart: TRadioButton
              Left = 138
              Top = 6
              Width = 113
              Height = 17
              Caption = 'Match at s&tart'
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
    Height = 434
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
      Height = 340
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
        Height = 325
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
      Height = 340
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
        Height = 325
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
      Top = 361
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
    Top = 434
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
  object sbUCM: TStatusBar
    Left = 0
    Top = 471
    Width = 922
    Height = 19
    Hint = 
      'DOUBLE-click on this Status Bar to copy the text displayed on th' +
      'e Status Bar to the Clipboard.'#13#10'Or RIGHT-click on the Status Bar' +
      ' to show a popup menu with more options.'
    Panels = <>
    ParentShowHint = False
    PopupMenu = pmUCMStatusBar
    ShowHint = True
    SimplePanel = True
    OnDblClick = sbUCMDblClick
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
    Images = il_MenuIcons
    OnPopup = pmuAvailPopup
    Left = 456
    Top = 112
    object mitAvailAddToUses: TMenuItem
      Action = actAvailAddToImpl
      Default = True
      ImageIndex = 6
    end
    object mi_AvailAddToIntf: TMenuItem
      Action = actAvailAddToIntf
      ImageIndex = 7
    end
    object mitAvailSep1: TMenuItem
      Caption = '-'
    end
    object mitAvailOpenUnit: TMenuItem
      Action = actOpenUnit
      ImageIndex = 4
    end
    object mitAvailSep2: TMenuItem
      Caption = '-'
    end
    object mitAvailAddToFav: TMenuItem
      Action = actAvailAddToFav
      ImageIndex = 5
    end
    object mitAvailSep3: TMenuItem
      Caption = '-'
    end
    object mCopyThisIdentifierToTheClipboard: TMenuItem
      Caption = 'Copy this Identifier to the Clipboard'
      ImageIndex = 0
      OnClick = mCopyThisIdentifierToTheClipboardClick
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
    OnPopup = pm_FavoritePopup
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
    object mi_FavOpenUnit: TMenuItem
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
    Images = il_MenuIcons
    Left = 488
    Top = 240
    object mi_CopyProjectListToClipboard: TMenuItem
      Caption = 'Copy the List of Project Units to the Clipboard   '
      ImageIndex = 0
      OnClick = mi_CopyProjectListToClipboardClick
    end
    object mi_SaveProjectList: TMenuItem
      Caption = 'Save the List of Project Units to Disk ...'
      ImageIndex = 1
      OnClick = mi_SaveProjectListClick
    end
  end
  object il_MenuIcons: TImageList
    Left = 600
    Top = 240
    Bitmap = {
      494C010108003800080010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C9E4F50061B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E400BDDEF300000000000000000000000000BACCA3008BA9
      61007D9F4F008BA96100BACCA300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BACCA3008BA9
      61007D9F4F008BA96100BACCA300000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FFBACCA3008BA9
      61007D9F4F008BA96100BACCA300000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FFB1D8F10062B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E500A6D3EF00000000000000000097B172007C9D4C007C9D
      4C007C9D4C007C9D4C007C9D4C0097B1720000000000858280006F6C6B006F6C
      6B006F6C6B006F6C6B006F6C6B008E8B8B000000000097B172007C9D4C007C9D
      4C007C9D4C007C9D4C007C9D4C0097B17200000000000000000000000000CFAE
      8B00CEAB8800000000000000000000000000000000FF97B172007C9D4C007C9D
      4C007C9D4C007C9D4C007C9D4C0097B17200000000FF6F6E6C00000000FF5957
      560059575600000000FF59575600000000FFA3D1EF0062B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E5009ACDED0000000000BACCA3007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C00BBCCA400EEF0EB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FAFAFA006F6C6B00BACCA3007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C00BACCA30000000000D0AF8D00B077
      3D00B0773D00CFAD8B000000000000000000BACCA3007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C00B9CAA300000000FF000000FFDFD3
      C800000000FF000000FF000000FF5957560096CBED0062B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E5008FC8EC00000000008BA962007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C008BAA630000000000FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB006F6C6B008BA962007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C008BA9620000000000B5824E00B077
      3D00B0773D00B0773D00D0AF8D00000000008BA962007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C008BA96200000000FFD5B89A00B077
      3D00D2B39400000000FF000000FF000000FF87C4EA0062B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E50084C2E900000000007D9F4F007C9D4C00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB007B9E4C007D9F4F0000000000B1B0B000B1B0
      B000B1B0B000D8D8D800FBFBFB006F6C6B007D9F4F007C9D4C00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB007B9E4C007D9F4F00000000007B6E6300AB77
      4100B0773D00B0773D00B0773D00D2B292007D9F4F007C9D4C00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB007B9E4C007D9F4F00000000FFBA8E6100B177
      3D00B1773D00D3B49500000000FF595756007ABEE80062B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E50078BDE800000000008BA962007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C008BAA630000000000FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB006F6C6B008BA962007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C008BA962000000000000000000EDE4
      DA00B5804A00B0773D00B0773D00CDAA86008BA962007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C008BA96200000000FF746C6700A776
      4600B1773D00B1773D00E1D6CC00595756006CB7E60062B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E5006DB7E60000000000BACCA3007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C00BECFA800E1E3DD00B1B0B000B1B0
      B000B1B0B000D8D8D800FBFBFB006F6C6B00BACCA3007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C00BACCA30000000000000000000000
      0000EBDDD900B57F4A00CCA7820000000000BACCA3007C9D4C007C9D4C007C9D
      4C00FBFBFB007B9E4C007C9D4C007C9D4C00BACCA300000000FF000000FFF1EB
      EA00BB895D00D2B29200000000FF000000FF6EB8E70062B2E50062B2E50062B2
      E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2E50062B2
      E50062B2E50062B2E50071B9E600000000000000000097B172007C9D4C007C9D
      4C007C9D4C007C9D4C007C9D4C0098B2730000000000F8F8F800FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB006F6C6B000000000097B172007C9D4C007C9D
      4C007C9D4C007C9D4C007C9D4C0097B17200000000000000000000000000BA85
      AB00B983A600EFE5E5000000000000000000000000FF97B172007C9D4C007C9D
      4C007C9D4C007C9D4C007C9D4C0097B17200000000FF000000FFE7D5E200A55E
      9200E0CAD200000000FF000000FF59575600000000009AC0D20099BFD10099BF
      D10099BFD10099BFD10099BFD10099BFD10099BFD10099BFD10099BFD10099BF
      D10099BFD1009BC1D20000000000000000000000000000000000BDCCA7008CAA
      63007D9F4F008CAA6300C0D0AA0000000000C7C8C600B1B0B000B1B0B000B1B0
      B000B1B0B000D8D8D800FBFBFB006F6C6B000000000000000000B9CDAA008BAA
      63007D9F4F008BAA6300B9CDA800000000009898940000000000BB87AD009E52
      89009E528900B985AB000000000000000000000000FF000000FFBACCA3008BA9
      63007D9F4F008BAA6300B9CCA800000000FF98979400000000FFA65F93009E51
      8900A1588D00E5D2E000000000FF59575600000000002A7492002A7492002A74
      92002A7492002A7492002A7492002A7492002A7492002A7492002A7492002A74
      92002A7492002C74940000000000000000000000000085828000EFF1ED000000
      00000000000000000000F3F5F000F9F9F900FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB006F6C6B0000000000A4D2EF00C2DFED000000
      00000000000000000000C2DFEC00BDD8E8006F6C6B0000000000AA6C99009E52
      89009E5289009E528900BB87AC0000000000000000FF000000FF000000FF0000
      00FF000000FF000000FFC2DFEC0098B2C2006F6C6B009A9697008F7586009C52
      88009E528900A1598D00E6DAE300000000FF000000002A7492002A7492002A74
      92002A7492002A7492002A7492006A9DB20093B8C70093B8C80093B8C80093B8
      C80093B8C800A7C4D1000000000000000000000000006F6C6B00FBFBFB00CACA
      CA007777770077777700CACACA00DFDFDF00B1B0B000B1B0B000B1B0B000B1B0
      B000B1B0B000D8D8D800FBFBFB006F6C6B0000000000ACD5F00063B3E60063B3
      E60063B3E60063B3E60063B3E60063B2E5006A8CA2006F6C6B00756A6F009755
      84009E5289009E5289009E528900BD8AAF00000000FF59575600000000FF89C6
      EB0063B4E60063B4E60063B4E60064B3E4009CBACD00D1D7DB00D3D1D200D0BB
      CB00A0558C00AF719E00000000FF59575600000000003C809C002A7492002A74
      92002A7492002C74930086AFC100000000000000000000000000000000000000
      000000000000000000000000000000000000000000006F6C6B00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB006F6C6B000000000000000000AAD5F00063B3
      E60063B3E60063B3E60063B3E60063B3E60064B4E600BBD7E900000000000000
      0000A96797009E5289009E528900CDA6C300000000FF59575600000000FF0000
      00FF87C5EB0063B4E60063B4E60063B4E60064B4E500CCE5F600000000FF0000
      00FFE4D0DF00000000FF000000FF595756000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BCCDA4007C9D4C007C9D4C007C9D4C00000000006F6C6B00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB006F6C6B00000000000000000000000000A7D4
      F00063B3E60063B3E60063B3E60063B3E60063B3E60063B4E600D9ECF8000000
      000000000000A9679700CBA3C00000000000000000FF000000FF000000FF0000
      00FF000000FF87C4EB0063B3E60063B3E60077BCE800000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DAE3CD0083A357007C9D4C0000000000B0773D00B0773D00B077
      3D00B0773D00B0773D00B0773D00B0773D00B0773D00B0773D00B0773D00B077
      3D00B0773D00B0773D00B0773D00B0773D000000000000000000000000000000
      0000A6D3F00063B3E60063B3E60063B3E60063B3E600A5D2EF00000000000000
      000000000000000000000000000000000000000000FF59575600000000FF0000
      00FF000000FF000000FF84C3EA0075BCE800000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF595756000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D4DFC60087A65D00DCE4D0007C9D4C0000000000B0773D00B0773D00B077
      3D00B0773D00B0773D00B0773D00B0773D00B0773D00B0773D00B0773D00B077
      3D00B0773D00B0773D00B0773D00B0773D000000000000000000000000000000
      000000000000A5D2EF0063B3E60063B3E600A2D1EF0000000000000000000000
      000000000000000000000000000000000000000000FF59575600000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF595756000000000000000000000000000000
      000000000000000000000000000000000000000000007C9D4C007C9D4C007C9D
      4C008FAC6700E7EDDF0000000000BCCDA50000000000C1956800B0773D00B077
      3D00B0773D00B0773D00B0773D00B0773D00B0773D00B0773D00B0773D00B077
      3D00B0773D00B0773D00B0773D00C19568000000000000000000000000000000
      00000000000000000000A2D1EF00A0D0EF000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF595756005957
      5600000000FF5957560059575600000000FF5957560059575600000000FF5957
      560059575600000000FF59575600000000FF0000000000000000000000000000
      0000000000000000000090909000777777007777770077777700777777007777
      7700777777007777770077777700909090000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006B8FE500537FE200537FE200537F
      E200537FE200537FE2006B8FE500000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D2E8F6007ABEE90063B3
      E60063B3E60063B3E60063B3E60063B3E6006FB9E700D0E5F400BFC1C300716E
      6D008784840000000000A8A8A800A3A3A3000000000000000000000000000000
      0000000000000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB0077777700B881A9009E5189009D5188009D51
      88009D5188009D5188009D5188009D5188009D5188009D5188009D5188009D51
      88009E518900B073A0000000000000000000537FE20000000000000000000000
      00000000000000000000537FE200000000007777770077777700777777007777
      7700777777007777770077777700919191008FC8EC006BB7E700C4E2F40063B3
      E60063B3E60063B3E60063B3E6006EB9E600BFDBED0000000000000000000000
      000000000000A7A7A70077777700B0B0B0000000000000000000000000000000
      0000000000000000000077777700FBFBFB00B0773C00B0773C00B0773C00B077
      3C00B0773C00B0773C00FBFBFB00777777009E5289009D518800F4EFF300FBFB
      FB00F8F7F800F3F0F200F6F5F600FBFBFB00FBFBFB00FBFBFB00FBFBFB00F5F0
      F3009E5189009E5289000000000000000000537FE20000000000000000000000
      00000000000000000000537FE20000000000FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB0077777700B4DAF200B0D8F100DAECF80063B3
      E60093CAEC00FAFAFB00F9FAFA00F1F4F600C7CBCD0083848400777777008181
      81009898980077777700B1B0B000EBEBEB000000000000000000000000000000
      0000000000000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00F8F8F800CECCCC00EBEBEB00F2F2F200F8F8F800FBFBFB00FBFBFB00FBFB
      FB009E5189009D5188000000000000000000537FE20000000000000000000000
      00000000000000000000537FE20000000000ECE0D400B0773D00B0773D00B077
      3D00B0773D00B0773D00FBFBFB00777777000000000000000000C5E2F40063B3
      E60063B3E60063B3E600C1DDEF00CFD1D2007B7B7B00C7C7C700EAEAEA00CCCC
      CC007F7F7F009E9E9E0000000000817F7E00B9B9B900A7A7A700A7A7A700A7A7
      A700A7A7A7000000000077777700FBFBFB00B0773C00B0773C00B0773C00B077
      3C00B0773C00B0773C00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00F8F8F800E7E7E700706D6C00B1AFAE0000000000F5F5F500FBFBFB00FBFB
      FB009E5189009D5188000000000000000000537FE2000000000000000000688D
      E400537FE200537FE200557FE10000000000FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB0077777700000000000000000077BCE80093CA
      EC00FAFAFB00FBFBFB00000000008B8C8C00C1C1C10000000000000000000000
      0000CACACA008787870000000000706D6C00A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00FAFAFA00F6F6F600AAA9A800BCBABA008F8D8C0000000000F5F5F500FBFB
      FB009E5189009D5188000000000000000000537FE2000000000000000000537F
      E200E7ECF900678DE400BAC8ED00DADCE200ECE0D400B0773D00B0773D00B077
      3D00B0773D00B0773D00FBFBFB007777770000000000B5DAF20063B3E60063B3
      E60063B3E60063B3E6000000000079797900DEDEDE0000000000000000000000
      0000E6E6E600767676000000000074717000A7A7A700FBFBFB00D7A56E00D7A5
      6E00D7A56E000000000077777700FBFBFB00B0773C00B0773C00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00777777009D5188009D518800FBFBFB00FBFB
      FB00FBFBFB00F9F9F900000000008E8B8B006F6C6B008B88880000000000F3F2
      F3009E5189009D5188000000000000000000537FE2000000000000000000537F
      E200688DE400BCCBF200000000007E7E7E00FBFBFB00FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB0077777700000000006CB7E60063B3E60063B3
      E60063B3E60063B3E600EAF1F6008E8F8F00BBBBBB0000000000000000000000
      0000C4C4C4008A8A8A00000000006F6C6B00A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB008C8C
      8C007777770077777700777777007E7E7E009D5188009D518800F5F0F400FBFB
      FB00FBFBFB00FBFBFB00F6F4F50000000000858282006F6C6B008C8988000000
      0000B989AC009D51880000000000000000006B8FE500537FE200537FE2005881
      E200C2CDEC00F0F0F4000000000077777700ECE0D400B0773D00DDC5AF008C8C
      8C007777770077777700777777007F7F7F00C2E1F40063B3E60063B3E6009FD0
      EF00B0D8F100B0D8F200E1EEF700D0D5D70079797900BBBBBB00DEDEDE00C0C0
      C0007B7B7B00D4D4D400F2F2F2006F6C6B00A7A7A700FBFBFB00D7A56E00D7A5
      6E00D7A56E000000000077777700FBFBFB00B0773C00B0773C00FBFBFB007777
      7700FBFBFB00DDDDDD0082828200D2D2D2009D5188009D5188009D5188009D51
      88009D5188009D5188009D518800C092B30000000000858282006F6C6B008C88
      880000000000B887AA0000000000000000000000000000000000000000000000
      0000ECE2DA00D8AB76000000000077777700FBFBFB00FBFBFB00FBFBFB007777
      7700FBFBFB00DDDDDD0083838300D2D2D200BDDEF30063B3E6007CBFE900BCDE
      F30075BCE80076BCE90078BEE700DCEAF300CFD5D9008D8E8F00797979008E8E
      8E00D7D7D700F6F6F600FAFAFA006F6C6B00A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB007777
      7700D8D8D80082828200D8D8D800000000009D5188009D5188009D5188009D51
      88009D5188009D5188009D5188009D518800C195B500000000008C8988006F6C
      6B008B88880000000000000000000000000000000000A8A8A800FBFBFB00FBFB
      FB00FBFBFB00FBFBFB000000000077777700ECE0D400B0773D00DDC5AF007777
      7700D8D8D80082828200D8D8D80000000000000000009ACDEE00CCE5F5006BB7
      E70063B3E60063B3E60063B3E60069B5E600BDDCEF0000000000000000000000
      0000F7F7F700FAFAFA00FBFBFB006F6C6B00A7A7A700FBFBFB00D7A56E00D7A5
      6E00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB00FBFBFB007777
      770082828200DCDCDC0000000000000000009D5188009D5188009D518800EFE6
      ED00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00ECECEC008B88
      88006F6C6B008B888700000000000000000000000000A8A8A800FBFBFB00D7A6
      6E00D7A66E00FBFBFB000000000077777700FBFBFB00FBFBFB00FBFBFB007777
      770083838300DDDDDD0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7F9FB00FAFAFA00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB006F6C6B00A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB000000000091919100777777007777770077777700777777008686
      8600E2E2E2000000000000000000000000009D5188009D5188009D518800FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00726F6E00726F6E00FBFBFB00FBFBFB000000
      0000858182006F6C6B00989696000000000000000000A8A8A800FBFBFB00FBFB
      FB00FBFBFB00FBFBFB0000000000929292007777770077777700777777008787
      8700E2E2E20000000000000000000000000000000000000000006F6C6B00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00928F
      8F006F6C6B006F6C6B006F6C6B0077747400A7A7A700FBFBFB00D7A56E00D7A5
      6E00FBFBFB00E5E5E50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009D5188009D5188009D518800FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00726F6E00726F6E00FBFBFB00FBFBFB00C195
      B50000000000928E8F00BCBBBA00928F8F0000000000A8A8A800FBFBFB00D7A6
      6E00D7A66E00FBFBFB00E5E5E500000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006F6C6B00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB006F6C
      6B00FAFAFA00E8E8E700807D7C00C6C5C500A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00A7A7A700E5E5E500AEAEAE00E4E4E40000000000000000000000
      0000000000000000000000000000000000009F538A009D5188009D518800FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB009E51
      8900C091B3000000000089858400A9A7A70000000000A8A8A800FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00A8A8A800E5E5E500AFAFAF00E5E5E500000000000000
      00000000000000000000000000000000000000000000000000006F6C6B00FBFB
      FB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB006F6C
      6B00E8E8E800807E7D00C4C4C20000000000A7A7A700FBFBFB00FBFBFB00FBFB
      FB00FBFBFB00A7A7A700AEAEAE00E8E8E8000000000000000000000000000000
      000000000000000000000000000000000000B67DA6009E5189009D5188009D51
      88009D5188009D5188009D5188009D5188009D5188009D5188009D5188009F55
      8B00DABED20000000000000000000000000000000000A8A8A800FBFBFB00FBFB
      FB00FBFBFB00FBFBFB00A8A8A800AEAEAE00E8E8E80000000000000000000000
      0000000000000000000000000000000000000000000000000000706C6B00FAFA
      FA00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB00FBFBFB006F6C
      6B00817E7D00C3C3C3000000000000000000B9B9B900A7A7A700A7A7A700A7A7
      A700A7A7A700B1B1B100EBEBEB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B9B9B900A8A8A800A8A8
      A800A8A8A800A8A8A800B1B1B100EBEBEB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000959393006F6C
      6B006F6C6B006F6C6B006F6C6B006F6C6B006F6C6B006F6C6B006F6C6B007774
      7300C5C4C400000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000001C1FFC1FFC1FF0001808080E780A5
      000100000043006E000100400041004700010040004000420001004000600040
      00010000007100630001808080E380C68003C100C143C14280039C009C41FC01
      800380008000A00281FF8000C030B036FFF08000E019F87FFFF88000F03FBCFE
      FFF08000F87FBFFEFF828000FCFFC925FC00FFFF01FF8004FC0000037D000078
      FC0000037D000000FC0000037D00C002040000836100C2720400004360008272
      040002236200807204000113020000000400008BF20000000401004782018070
      040300038203FF80040700118207C00003FF000881FFC000007F0004803FC001
      00FF0007807FC00301FFFFFF80FFC00700000000000000000000000000000000
      000000000000}
  end
  object dlgSave: TSaveDialog
    Left = 672
    Top = 240
  end
  object pmUCMStatusBar: TPopupMenu
    Images = il_MenuIcons
    OnPopup = pmUCMStatusBarPopup
    Left = 424
    Top = 432
    object mCopyThisTextToTheClipboard: TMenuItem
      Caption = 'Copy this TEXT to the clipboard'
      Default = True
      ImageIndex = 0
      OnClick = mCopyThisTextToTheClipboardClick
    end
    object mCopyThisFileToTheClipboard: TMenuItem
      Caption = 'Copy this FILE to the clipboard'
      ImageIndex = 2
      OnClick = mCopyThisFileToTheClipboardClick
    end
    object mShowThisFileInWindowsExplorer: TMenuItem
      Caption = 'Open Location of this File'
      ImageIndex = 3
      OnClick = mShowThisFileInWindowsExplorerClick
    end
  end
end
