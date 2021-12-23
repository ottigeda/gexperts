object fmConfiguration: TfmConfiguration
  Left = 411
  Top = 164
  BorderIcons = [biSystemMenu]
  Caption = 'GExperts Configuration'
  ClientHeight = 621
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Scaled = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 587
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    FullRepaint = False
    TabOrder = 0
    object pcConfig: TPageControl
      Left = 7
      Top = 7
      Width = 770
      Height = 573
      ActivePage = tshExperts
      Align = alClient
      HotTrack = True
      MultiLine = True
      TabOrder = 0
      OnChange = pcConfigChange
      object tshExperts: TTabSheet
        Caption = 'Experts'
      end
      object tshEditorExperts: TTabSheet
        Caption = 'Editor Experts'
        ImageIndex = 7
      end
      object tshGeneral: TTabSheet
        Caption = 'General'
        object pnlGeneral: TPanel
          Left = 0
          Top = 0
          Width = 762
          Height = 544
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 8
          TabOrder = 0
          object gbxLocations: TGroupBox
            Left = 8
            Top = 8
            Width = 746
            Height = 225
            Align = alTop
            Caption = 'File Locations'
            TabOrder = 0
            DesignSize = (
              746
              225)
            object lblVCL: TLabel
              Left = 12
              Top = 24
              Width = 113
              Height = 14
              Caption = '&VCL source directory'
              FocusControl = edVCLPath
            end
            object lblConfig: TLabel
              Left = 12
              Top = 72
              Width = 146
              Height = 14
              Caption = '&GExperts storage directory'
              FocusControl = edConfigPath
            end
            object lblHelp: TLabel
              Left = 12
              Top = 168
              Width = 43
              Height = 14
              Caption = 'Help &file'
              FocusControl = edHelpFile
            end
            object sbVCLDir: TButton
              Left = 716
              Top = 40
              Width = 22
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 1
              OnClick = sbVCLDirClick
            end
            object sbConfigDir: TButton
              Left = 716
              Top = 88
              Width = 22
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 3
              OnClick = sbConfigDirClick
            end
            object sbHelpFile: TButton
              Left = 716
              Top = 184
              Width = 22
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 7
              OnClick = sbHelpFileClick
            end
            object edVCLPath: TEdit
              Left = 12
              Top = 40
              Width = 701
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object edConfigPath: TEdit
              Left = 12
              Top = 88
              Width = 701
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
            end
            object edHelpFile: TEdit
              Left = 12
              Top = 184
              Width = 701
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 6
            end
            object lblCachingPath: TLabel
              Left = 12
              Top = 120
              Width = 146
              Height = 14
              Caption = 'GExperts caching directory'
              FocusControl = edCachingPath
            end
            object edCachingPath: TEdit
              Left = 12
              Top = 136
              Width = 701
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 4
            end
            object bCachingPath: TButton
              Left = 716
              Top = 136
              Width = 22
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 5
              OnClick = bCachingPathClick
            end
          end
          object gbxCustomFont: TGroupBox
            Left = 8
            Top = 239
            Width = 746
            Height = 65
            Align = alTop
            Caption = 'User Interface'
            TabOrder = 1
            object chkUseCustomFont: TCheckBox
              Left = 16
              Top = 24
              Width = 217
              Height = 24
              Caption = 'Use custom UI font'
              TabOrder = 0
            end
            object btnCustomFont: TButton
              Left = 238
              Top = 24
              Width = 79
              Height = 25
              Caption = 'Font...'
              TabOrder = 1
              OnClick = btnCustomFontClick
            end
          end
          object pnlGeneralSpacer: TPanel
            Left = 8
            Top = 233
            Width = 746
            Height = 6
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
          end
        end
      end
      object tshIDE: TTabSheet
        Caption = 'IDE'
        object gbxIDEMenu: TGroupBox
          Left = 8
          Top = 8
          Width = 433
          Height = 129
          Caption = '&Menu'
          TabOrder = 0
          object chkAlphabetizeMenu: TCheckBox
            Left = 8
            Top = 24
            Width = 423
            Height = 24
            Caption = 'Alphabetize the GExperts menu items'
            TabOrder = 0
          end
          object chkPlaceGxMainMenuInToolsMenu: TCheckBox
            Left = 8
            Top = 48
            Width = 423
            Height = 24
            Caption = 'Place GExperts menu under Tools'
            TabOrder = 1
          end
          object chkHideWindowMenu: TCheckBox
            Left = 8
            Top = 72
            Width = 423
            Height = 24
            Caption = 'Hide Window menu'
            TabOrder = 2
          end
          object chkMoveComponentMenu: TCheckBox
            Left = 8
            Top = 96
            Width = 423
            Height = 24
            Caption = 'Move the Component menu to Tools'
            TabOrder = 3
          end
        end
        object gbxIDEForms: TGroupBox
          Left = 8
          Top = 139
          Width = 745
          Height = 398
          Caption = 'IDE'
          TabOrder = 2
          object l_ForceDestkopAV: TLabel
            Left = 176
            Top = 316
            Width = 288
            Height = 14
            Caption = 'Warning: Can cause Access Violations on IDE startup'
            Font.Charset = ANSI_CHARSET
            Font.Color = clRed
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object chkEnhanceDialogs: TCheckBox
            Left = 8
            Top = 24
            Width = 729
            Height = 24
            Hint = 
              'Enhance IDE options dialogs to allow resizing, remember position' +
              's, increase'#13'combobox DropDownCounts, resizable picture open dial' +
              'ogs, collapse Together'#13'options tree node, etc.  (Some enhancemen' +
              'ts require BDS 2006 or later)'
            Caption = 'Enhance IDE dialogs (and fix form positioning bugs)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = chkEnhanceDialogsClick
          end
          object chkEnhanceSearchPaths: TCheckBox
            Left = 8
            Top = 96
            Width = 729
            Height = 24
            Caption = 
              'Enable drag && drop, autocomplete and other enhancement for sear' +
              'ch paths'
            TabOrder = 3
          end
          object chkEnhanceToolProperties: TCheckBox
            Left = 8
            Top = 120
            Width = 729
            Height = 24
            Caption = 'Enhance Tools menu Tool Properties dialog'
            TabOrder = 4
          end
          object chkAllowResize: TCheckBox
            Left = 24
            Top = 48
            Width = 713
            Height = 24
            Caption = 'Allow resize and remember size'
            TabOrder = 1
          end
          object chkRememberPosition: TCheckBox
            Left = 48
            Top = 72
            Width = 689
            Height = 24
            Caption = 'Also remember position'
            TabOrder = 2
          end
          object chkEnhanceInstallPackages: TCheckBox
            Left = 8
            Top = 144
            Width = 729
            Height = 24
            Caption = 'Enhance Install Packages dialog with ... Explorer button'
            TabOrder = 5
          end
          object chkEnhanceBuildEventsDialog: TCheckBox
            Left = 8
            Top = 216
            Width = 729
            Height = 24
            Caption = 'Enhance Build Events dialog with Favourites'
            TabOrder = 8
          end
          object chkEnhanceApplicationSettingsDialog: TCheckBox
            Left = 8
            Top = 168
            Width = 729
            Height = 24
            Caption = 'Enhance Application Settings dialog with version button'
            TabOrder = 6
          end
          object chkAutoCloseMessage: TCheckBox
            Left = 8
            Top = 240
            Width = 729
            Height = 24
            Caption = 'Automatically close message window after successful compile'
            TabOrder = 9
          end
          object chkAutoCloseIgnoreHints: TCheckBox
            Left = 32
            Top = 264
            Width = 281
            Height = 24
            Caption = 'Even if there are hints'
            TabOrder = 10
          end
          object chkAutoCloseIgnoreWarnings: TCheckBox
            Left = 312
            Top = 264
            Width = 425
            Height = 24
            Caption = 'Even if there are warnings'
            TabOrder = 11
          end
          object chkForceStartupDesktop: TCheckBox
            Left = 8
            Top = 288
            Width = 729
            Height = 24
            Caption = 'Force desktop on startup (leave empty for last selected)'
            TabOrder = 12
          end
          object cbxDesktop: TComboBox
            Left = 24
            Top = 312
            Width = 145
            Height = 22
            ItemHeight = 14
            TabOrder = 13
          end
          object chkEnhanceDockForms: TCheckBox
            Left = 8
            Top = 192
            Width = 729
            Height = 24
            Caption = 'Enhance Dock Forms to allow minimize and Win+arrow positioning'
            TabOrder = 7
          end
          object chk_FontForNewForms: TCheckBox
            Left = 8
            Top = 344
            Width = 729
            Height = 24
            Caption = 'Custom Font for new forms (sets DefFontData)'
            TabOrder = 14
            Visible = False
          end
          object b_CustomFontForNewForms: TButton
            Left = 24
            Top = 360
            Width = 79
            Height = 25
            Caption = 'Font...'
            TabOrder = 15
            Visible = False
            OnClick = b_CustomFontForNewFormsClick
          end
        end
        object gbxObjectInspector: TGroupBox
          Left = 448
          Top = 8
          Width = 305
          Height = 129
          Caption = 'Object Inspector'
          TabOrder = 1
          object chkOIFontNames: TCheckBox
            Left = 8
            Top = 24
            Width = 295
            Height = 24
            Caption = 'Show font names using the font'
            TabOrder = 0
          end
          object chkOIHideHotCmds: TCheckBox
            Left = 8
            Top = 48
            Width = 295
            Height = 24
            Caption = 'Hide Quick Action Panel'
            TabOrder = 1
          end
          object chkOIHideDescPane: TCheckBox
            Left = 8
            Top = 72
            Width = 295
            Height = 24
            Caption = 'Hide Description Panel'
            TabOrder = 2
          end
        end
      end
      object tshOldIdes: TTabSheet
        Caption = 'Delphi 6/7'
        ImageIndex = 8
        object gbxTabDockHost: TGroupBox
          Left = 448
          Top = 8
          Width = 305
          Height = 105
          Caption = 'Tab Dock &Hosts'
          TabOrder = 1
          object chkMultiLineTabDockHost: TCheckBox
            Left = 8
            Top = 24
            Width = 295
            Height = 24
            Caption = 'Enable multiline tabs for docked forms'
            TabOrder = 0
            OnClick = chkMultiLineTabDockHostClick
          end
          object chkDefaultMultiLineTabDockHost: TCheckBox
            Left = 24
            Top = 48
            Width = 279
            Height = 24
            Caption = 'Default to multiline tabs'
            TabOrder = 1
          end
        end
        object gbxCompPalette: TGroupBox
          Left = 8
          Top = 8
          Width = 433
          Height = 193
          Caption = 'Component &Palette'
          TabOrder = 0
          object chkCPMultiLine: TCheckBox
            Left = 8
            Top = 16
            Width = 423
            Height = 24
            Caption = 'Multiline tabs'
            TabOrder = 0
            OnClick = chkCPMultiLineClick
          end
          object chkCPAsButtons: TCheckBox
            Left = 8
            Top = 88
            Width = 423
            Height = 24
            Caption = 'Show tabs as buttons'
            TabOrder = 3
            OnClick = chkCPAsButtonsClick
          end
          object chkCPTabsInPopup: TCheckBox
            Left = 8
            Top = 136
            Width = 423
            Height = 24
            Caption = 'Add popup menu/button with tab names'
            TabOrder = 5
            OnClick = chkCPTabsInPopupClick
          end
          object chkCPFlat: TCheckBox
            Left = 24
            Top = 112
            Width = 407
            Height = 24
            Caption = 'Flat buttons'
            TabOrder = 4
          end
          object chkCPTabsInPopupAlphaSort: TCheckBox
            Left = 24
            Top = 160
            Width = 407
            Height = 24
            Caption = 'Show tab names in alphabetical order'
            TabOrder = 6
          end
          object chkCPScrollOpposite: TCheckBox
            Left = 24
            Top = 40
            Width = 407
            Height = 24
            Caption = 'Scroll &opposite'
            TabOrder = 1
          end
          object chkCPRaggedRight: TCheckBox
            Left = 24
            Top = 64
            Width = 407
            Height = 24
            Caption = 'Ragged &right'
            TabOrder = 2
          end
        end
      end
      object tshEditor: TTabSheet
        Caption = 'Code Editor'
        object lblHideNavBar: TLabel
          Left = 248
          Top = 48
          Width = 513
          Height = 113
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'You can hide the navigation bar in the Tools -> Options dialog'#13#10 +
            'See Editor Options -> Display -> Show Navigation Toolbar'
          Visible = False
          WordWrap = True
        end
        object gbxEditorTabs: TGroupBox
          Left = 8
          Top = 216
          Width = 225
          Height = 153
          Caption = 'Editor T&abs (Delphi 6 and 7)'
          TabOrder = 1
          object chkMultiLine: TCheckBox
            Left = 8
            Top = 24
            Width = 210
            Height = 24
            Caption = 'Multiline'
            TabOrder = 0
          end
          object chkHotTrack: TCheckBox
            Left = 8
            Top = 72
            Width = 210
            Height = 24
            Caption = 'Hot tracking'
            TabOrder = 2
          end
          object chkButtons: TCheckBox
            Left = 8
            Top = 96
            Width = 210
            Height = 24
            Caption = 'Button style'
            TabOrder = 3
            OnClick = chkButtonsClick
          end
          object chkEditTabButtonsFlat: TCheckBox
            Left = 24
            Top = 120
            Width = 194
            Height = 24
            Caption = 'Flat buttons'
            Enabled = False
            TabOrder = 4
          end
          object chkMiddleButtonClose: TCheckBox
            Left = 8
            Top = 48
            Width = 210
            Height = 24
            Caption = 'Middle mouse button closes tab'
            TabOrder = 1
          end
        end
        object gbxEditorToolBar: TGroupBox
          Left = 8
          Top = 10
          Width = 225
          Height = 199
          Caption = 'Editor &Toolbar'
          TabOrder = 0
          object chkEditorToolBar: TCheckBox
            Left = 8
            Top = 24
            Width = 193
            Height = 24
            Caption = 'Enable editor toolbar'
            TabOrder = 0
            OnClick = chkEditorToolBarClick
          end
          object rgAlign: TRadioGroup
            Left = 24
            Top = 48
            Width = 177
            Height = 105
            Caption = 'Align'
            Items.Strings = (
              'Top'
              'Bottom'
              'Left'
              'Right')
            TabOrder = 1
          end
          object btnConfigureToolBar: TButton
            Left = 24
            Top = 162
            Width = 177
            Height = 25
            Caption = 'Configure Toolbar...'
            TabOrder = 2
            OnClick = btnConfigureToolBarClick
          end
        end
        object chkDisableEDTEnhancements: TCheckBox
          Left = 248
          Top = 24
          Width = 513
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Caption = '&Disable all editor enhancements'
          TabOrder = 3
          OnClick = chkDisableEDTEnhancementsClick
        end
        object chkHideNavbar: TCheckBox
          Left = 248
          Top = 48
          Width = 513
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Hide Navigation Bar (Delphi 10 and 10.1)'
          TabOrder = 2
        end
      end
      object tshSuppressedMessages: TTabSheet
        Caption = 'Suppressed Messages'
        ImageIndex = 6
        DesignSize = (
          762
          544)
        object gbSuppressedMessages: TGroupBox
          Left = 8
          Top = 8
          Width = 433
          Height = 608
          Anchors = [akLeft, akTop, akBottom]
          Caption = 'Suppressed Messages'
          TabOrder = 0
          DesignSize = (
            433
            608)
          object lbSuppressedMesages: TListBox
            Left = 8
            Top = 24
            Width = 321
            Height = 576
            Anchors = [akLeft, akTop, akBottom]
            ItemHeight = 14
            TabOrder = 0
          end
          object btnDeleteSuppressedMessage: TButton
            Left = 344
            Top = 24
            Width = 75
            Height = 25
            Caption = 'Delete'
            TabOrder = 1
            OnClick = btnDeleteSuppressedMessageClick
          end
          object btnClearSuppressedMessages: TButton
            Left = 344
            Top = 56
            Width = 75
            Height = 25
            Caption = 'Clear'
            TabOrder = 2
            OnClick = btnClearSuppressedMessagesClick
          end
        end
      end
      object tshDebug: TTabSheet
        Caption = 'Debug'
        ImageIndex = 6
        object chkEditorKeyTracing: TCheckBox
          Left = 16
          Top = 16
          Width = 233
          Height = 17
          Caption = 'Enable editor key tracing messages'
          TabOrder = 0
          OnClick = chkEditorKeyTracingClick
        end
        object btnEnumerateModules: TButton
          Left = 16
          Top = 39
          Width = 137
          Height = 25
          Caption = 'Enumerate Modules'
          TabOrder = 1
          OnClick = btnEnumerateModulesClick
        end
        object btnEumerateActions: TButton
          Left = 16
          Top = 103
          Width = 137
          Height = 25
          Caption = 'Enumerate Actions'
          TabOrder = 3
          OnClick = btnEumerateActionsClick
        end
        object btnGetFonts: TButton
          Left = 16
          Top = 135
          Width = 137
          Height = 25
          Caption = 'IDE Fonts'
          TabOrder = 4
          OnClick = btnGetFontsClick
        end
        object btnAppBuilder: TButton
          Left = 16
          Top = 167
          Width = 137
          Height = 25
          Caption = 'Application Window'
          TabOrder = 5
          OnClick = btnAppBuilderClick
        end
        object gbxFonts: TGroupBox
          Left = 16
          Top = 207
          Width = 321
          Height = 81
          Caption = 'Custom &Fonts'
          TabOrder = 6
          object btnOIFont: TButton
            Left = 24
            Top = 22
            Width = 119
            Height = 26
            Caption = 'Object Inspector'
            TabOrder = 0
            OnClick = btnFontClick
          end
          object btnCPFont: TButton
            Left = 172
            Top = 22
            Width = 124
            Height = 26
            Caption = 'Component Palette'
            TabOrder = 2
            OnClick = btnFontClick
          end
          object chkOIFontEnabled: TCheckBox
            Left = 55
            Top = 51
            Width = 89
            Height = 17
            Caption = 'Enable'
            TabOrder = 1
            OnClick = chkFontEnabledClick
          end
          object chkCPFontEnabled: TCheckBox
            Left = 203
            Top = 51
            Width = 97
            Height = 17
            Caption = 'Enable'
            TabOrder = 3
            OnClick = chkFontEnabledClick
          end
        end
        object gbxFileSaving: TGroupBox
          Left = 18
          Top = 301
          Width = 191
          Height = 70
          Caption = 'File &Saving'
          TabOrder = 7
          Visible = False
          object lblEvery: TLabel
            Left = 28
            Top = 44
            Width = 30
            Height = 14
            Alignment = taRightJustify
            Caption = 'Every'
            FocusControl = edtMinutes
          end
          object lblMinutes: TLabel
            Left = 121
            Top = 44
            Width = 53
            Height = 14
            Caption = 'minute(s)'
          end
          object chkAutoSave: TCheckBox
            Left = 12
            Top = 20
            Width = 157
            Height = 24
            Caption = 'Enable auto save of files'
            TabOrder = 0
            OnClick = chkAutoSaveClick
          end
          object edtMinutes: TEdit
            Left = 64
            Top = 40
            Width = 38
            Height = 22
            TabOrder = 1
            Text = '1'
          end
          object udMinutes: TUpDown
            Left = 102
            Top = 40
            Width = 16
            Height = 22
            Associate = edtMinutes
            Min = 1
            Max = 1000
            Position = 1
            TabOrder = 2
          end
        end
        object btnEditView: TButton
          Left = 16
          Top = 71
          Width = 137
          Height = 25
          Caption = 'Edit View'
          TabOrder = 2
          OnClick = btnEditViewClick
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 587
    Width = 784
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 497
      Top = 0
      Width = 287
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      DesignSize = (
        287
        34)
      object btnOK: TButton
        Left = 48
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 128
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 208
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
    object btnImport: TButton
      Left = 216
      Top = 1
      Width = 75
      Height = 25
      Caption = 'Import ...'
      TabOrder = 3
      Visible = False
    end
    object btnExport: TButton
      Left = 136
      Top = 1
      Width = 75
      Height = 25
      Caption = 'Export ...'
      TabOrder = 2
      Visible = False
      OnClick = btnExportClick
    end
    object btnUsage: TButton
      Left = 8
      Top = 0
      Width = 121
      Height = 25
      Caption = 'Usage (%d) ...'
      TabOrder = 1
      OnClick = btnUsageClick
    end
  end
  object dlgUIFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 6
    MaxFontSize = 24
    Options = []
    Left = 476
    Top = 356
  end
  object dlgFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 6
    MaxFontSize = 24
    Options = []
    Left = 476
    Top = 388
  end
end
