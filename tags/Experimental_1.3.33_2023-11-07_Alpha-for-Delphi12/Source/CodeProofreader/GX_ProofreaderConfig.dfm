object fmProofreaderConfig: TfmProofreaderConfig
  Left = 279
  Top = 203
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Code Proofreader'
  ClientHeight = 490
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 30
    Width = 523
    Height = 460
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object pnlButtons: TPanel
      Left = 6
      Top = 421
      Width = 511
      Height = 33
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object pnlButtonsRight: TPanel
        Left = 214
        Top = 0
        Width = 297
        Height = 33
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object btnOK: TButton
          Left = 50
          Top = 7
          Width = 75
          Height = 25
          Caption = 'OK'
          Default = True
          ModalResult = 1
          TabOrder = 0
          OnClick = btnOKClick
        end
        object btnCancel: TButton
          Left = 136
          Top = 7
          Width = 75
          Height = 25
          Cancel = True
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 1
        end
        object btnHelp: TButton
          Left = 222
          Top = 7
          Width = 75
          Height = 25
          Caption = '&Help'
          TabOrder = 2
          OnClick = btnHelpClick
        end
      end
    end
    object Pages: TPageControl
      Tag = 1
      Left = 6
      Top = 6
      Width = 511
      Height = 415
      ActivePage = tabCommon
      Align = alClient
      MultiLine = True
      TabOrder = 0
      OnChange = PagesChange
      object tabCommon: TTabSheet
        Caption = '&Settings'
        ImageIndex = 3
        DesignSize = (
          503
          386)
        object lblCustomBeep: TLabel
          Left = 22
          Top = 29
          Width = 266
          Height = 14
          Caption = 'Custom &beep file name (empty = default beep):'
          FocusControl = eCustomBeep
        end
        object cbBeep: TCheckBox
          Left = 4
          Top = 4
          Width = 175
          Height = 17
          Anchors = [akTop, akRight]
          Caption = '&Beep when correcting'
          TabOrder = 0
          OnClick = cbBeepClick
        end
        object eCustomBeep: TEdit
          Left = 22
          Top = 48
          Width = 395
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Text = 'eCustomBeep'
        end
        object btnSelectFile: TButton
          Left = 423
          Top = 48
          Width = 75
          Height = 22
          Anchors = [akTop, akRight]
          Caption = 'Browse...'
          TabOrder = 3
          OnClick = btnSelectFileClick
        end
        object btnTestBeep: TButton
          Left = 423
          Top = 21
          Width = 75
          Height = 25
          Action = actTestBeep
          TabOrder = 1
        end
      end
      object tabReplacement: TTabSheet
        Caption = '&AutoCorrect'
        object pnlReplacement: TPanel
          Left = 0
          Top = 0
          Width = 503
          Height = 386
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 0
          object lvReplacement: TListView
            Left = 4
            Top = 32
            Width = 495
            Height = 350
            Align = alClient
            Columns = <
              item
                Caption = 'Replace Text'
                Width = 134
              end
              item
                Caption = 'Location'
                Width = 128
              end
              item
                AutoSize = True
                Caption = 'Replace With'
              end>
            ColumnClick = False
            HideSelection = False
            MultiSelect = True
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            PopupMenu = pmList
            TabOrder = 0
            ViewStyle = vsReport
            OnData = lvReplacementData
            OnDblClick = lvReplacementDblClick
          end
          object pnlACHeader: TPanel
            Left = 4
            Top = 4
            Width = 495
            Height = 28
            Align = alTop
            BevelOuter = bvNone
            BorderWidth = 5
            TabOrder = 1
            object tbrReplacement: TToolBar
              Left = 1
              Top = 3
              Width = 69
              Height = 23
              Align = alNone
              ButtonHeight = 23
              DisabledImages = dmSharedImages.DisabledImages
              EdgeBorders = []
              Flat = True
              Images = dmSharedImages.Images
              TabOrder = 0
              object tbnReplacementInsert: TToolButton
                Left = 0
                Top = 0
                Action = actListInsert
                AutoSize = True
              end
              object tbnReplacementEdit: TToolButton
                Left = 23
                Top = 0
                Action = actListEdit
                AutoSize = True
              end
              object tbnReplacementDelete: TToolButton
                Left = 46
                Top = 0
                Action = actListDelete
                AutoSize = True
              end
            end
            object cbReplacerActive: TCheckBox
              Left = 101
              Top = 6
              Width = 171
              Height = 18
              Caption = 'E&nable AutoCorrect'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
        end
      end
      object tabDictionary: TTabSheet
        Tag = 1
        Caption = '&Dictionary'
        object pnlDictionary: TPanel
          Left = 0
          Top = 0
          Width = 503
          Height = 386
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 0
          object gbxWords: TGroupBox
            Left = 4
            Top = 4
            Width = 194
            Height = 378
            Align = alClient
            Caption = 'Dictionary Words'
            TabOrder = 0
            object pnlWords: TPanel
              Left = 2
              Top = 16
              Width = 190
              Height = 360
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 4
              TabOrder = 0
              object tbrDictionary: TToolBar
                Left = 4
                Top = 4
                Width = 182
                Height = 24
                DisabledImages = dmSharedImages.DisabledImages
                EdgeBorders = []
                Flat = True
                Images = dmSharedImages.Images
                TabOrder = 0
                object tbnDictionaryInsert: TToolButton
                  Left = 0
                  Top = 0
                  Action = actListInsert
                end
                object tbnDictionaryEdit: TToolButton
                  Left = 23
                  Top = 0
                  Action = actListEdit
                end
                object tbnDictionaryDelete: TToolButton
                  Left = 46
                  Top = 0
                  Action = actListDelete
                end
                object tbnDictionarySep: TToolButton
                  Left = 69
                  Top = 0
                  Width = 8
                  ImageIndex = 13
                  Style = tbsSeparator
                end
                object tbnDictionaryExport: TToolButton
                  Left = 77
                  Top = 0
                  Action = actExportWords
                end
                object tbnDictionaryImport: TToolButton
                  Left = 100
                  Top = 0
                  Action = actImportWords
                end
              end
              object lvDictionary: TListView
                Left = 4
                Top = 28
                Width = 182
                Height = 328
                Align = alClient
                Columns = <
                  item
                    AutoSize = True
                    Caption = 'Words'
                  end>
                ColumnClick = False
                HideSelection = False
                MultiSelect = True
                OwnerData = True
                ReadOnly = True
                RowSelect = True
                PopupMenu = pmList
                ShowColumnHeaders = False
                TabOrder = 1
                ViewStyle = vsReport
                OnData = lvDictionaryData
                OnDataFind = lvDictionaryDataFind
                OnDblClick = lvDictionaryDblClick
              end
            end
          end
          object pnlDictOptions: TPanel
            Left = 198
            Top = 4
            Width = 301
            Height = 378
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              301
              378)
            object gbReplaceIf: TGroupBox
              Left = 8
              Top = 48
              Width = 293
              Height = 147
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Replace When'
              TabOrder = 2
              object cbOneCharIncorrect: TCheckBox
                Left = 8
                Top = 37
                Width = 262
                Height = 17
                Caption = 'One character is wrong'
                TabOrder = 1
                OnClick = cbOneCharIncorrectClick
              end
              object cbAllowOneCharacterMissing: TCheckBox
                Left = 8
                Top = 81
                Width = 262
                Height = 17
                Caption = 'One character is missing'
                TabOrder = 3
              end
              object cbAllowExtraChar: TCheckBox
                Left = 8
                Top = 101
                Width = 262
                Height = 17
                Caption = 'An extra character is inserted'
                TabOrder = 4
              end
              object cbCaseDiffer: TCheckBox
                Left = 8
                Top = 17
                Width = 262
                Height = 17
                Caption = 'Word case is different'
                TabOrder = 0
              end
              object cbAllowSwitchedChars: TCheckBox
                Left = 8
                Top = 121
                Width = 262
                Height = 17
                Caption = 'Two characters are switched'
                TabOrder = 5
              end
              object cbMustBeNearbyLetter: TCheckBox
                Left = 21
                Top = 59
                Width = 249
                Height = 17
                Caption = 'Character must be nearby correct key'
                TabOrder = 2
              end
            end
            object cbFirstCharMustBeCorrect: TCheckBox
              Left = 8
              Top = 202
              Width = 291
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Don'#39't replace if the first character is different'
              TabOrder = 3
            end
            object cbEnableDictionary: TCheckBox
              Tag = 2
              Left = 8
              Top = 4
              Width = 291
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Enable dictionary replacement'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbEnableDicitionaryClick
            end
            object cbEnableCompiler: TCheckBox
              Left = 8
              Top = 24
              Width = 291
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Enable compiler-assisted replacement'
              TabOrder = 1
              OnClick = cbEnableCompilerClick
            end
          end
        end
      end
      object tabHistory: TTabSheet
        Tag = 2
        Caption = '&Correction History'
        object pnlHistory: TPanel
          Left = 0
          Top = 0
          Width = 503
          Height = 386
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 0
          object lvHistory: TListView
            Left = 4
            Top = 4
            Width = 495
            Height = 347
            Align = alClient
            Columns = <
              item
                AutoSize = True
                Caption = 'Correction'
              end
              item
                Caption = 'Time'
                Width = 90
              end>
            ColumnClick = False
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            PopupMenu = pmHistory
            TabOrder = 0
            ViewStyle = vsReport
          end
          object pnlHistoryButtons: TPanel
            Left = 4
            Top = 351
            Width = 495
            Height = 31
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              495
              31)
            object btnDisableRule: TButton
              Left = 327
              Top = 5
              Width = 168
              Height = 26
              Action = actDisableRule
              Anchors = [akRight, akBottom]
              TabOrder = 0
            end
          end
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 523
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblRules: TLabel
      Left = 14
      Top = 9
      Width = 47
      Height = 14
      Alignment = taRightJustify
      Caption = '&Rules for'
      FocusControl = cbLanguage
    end
    object cbLanguage: TComboBox
      Left = 68
      Top = 6
      Width = 185
      Height = 22
      Style = csDropDownList
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 0
      OnChange = cbLanguageChange
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 120
    Top = 200
    object actListInsert: TAction
      Category = 'List'
      Caption = '&Insert'
      Hint = 'Insert'
      ImageIndex = 41
      ShortCut = 16429
      OnExecute = actListInsertExecute
    end
    object actListEdit: TAction
      Category = 'List'
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 38
      ShortCut = 113
      OnExecute = actListEditExecute
    end
    object actListDelete: TAction
      Category = 'List'
      Caption = 'De&lete'
      Hint = 'Delete'
      ImageIndex = 42
      ShortCut = 16430
      OnExecute = actListDeleteExecute
    end
    object actDisableRule: TAction
      Category = 'List'
      Caption = 'Di&sable Correction Rule'
      Hint = 'Disable Correction Rule'
      OnExecute = actDisableRuleExecute
    end
    object actImportWords: TAction
      Category = 'List'
      Caption = 'Import Word List'
      Hint = 'Import Word List'
      ImageIndex = 1
      OnExecute = actImportWordsExecute
    end
    object actExportWords: TAction
      Category = 'List'
      Caption = 'Export Word List'
      Hint = 'Export Word List'
      ImageIndex = 31
      OnExecute = actExportWordsExecute
    end
    object actTestBeep: TAction
      Category = 'Common'
      Caption = 'Test'
      ImageIndex = 43
      OnExecute = actTestBeepExecute
    end
  end
  object pmList: TPopupMenu
    Images = dmSharedImages.Images
    Left = 48
    Top = 200
    object pmiListInsert: TMenuItem
      Action = actListInsert
    end
    object pmiListEdit: TMenuItem
      Action = actListEdit
    end
    object pmiListDelete: TMenuItem
      Action = actListDelete
    end
  end
  object pmHistory: TPopupMenu
    Left = 186
    Top = 199
    object pmiDisableRule: TMenuItem
      Action = actDisableRule
    end
  end
end
