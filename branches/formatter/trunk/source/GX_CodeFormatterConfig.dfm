object fmCodeFormatterConfig: TfmCodeFormatterConfig
  Left = 339
  Top = 175
  HelpContext = 100
  Caption = 'Delphi Code Formatter Configuration'
  ClientHeight = 428
  ClientWidth = 478
  Color = clBtnFace
  Constraints.MinHeight = 375
  Constraints.MinWidth = 478
  ParentFont = True
  OldCreateOrder = True
  PopupMenu = pm_Extra
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pc_Main: TPageControl
    Left = 0
    Top = 0
    Width = 478
    Height = 387
    ActivePage = ts_Capitalization
    Align = alClient
    TabOrder = 0
    object ts_Indent: TTabSheet
      Caption = 'Indent'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object l_SpacesPerIndent: TLabel
        Left = 8
        Top = 22
        Width = 88
        Height = 13
        Margins.Bottom = 0
        Caption = 'Spaces per Indent'
      end
      object SpacePerIndentEdit: TEdit
        Left = 104
        Top = 19
        Width = 59
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object SpacePerIndentUpDown: TUpDown
        Left = 163
        Top = 19
        Width = 11
        Height = 21
        Associate = SpacePerIndentEdit
        Max = 10
        TabOrder = 1
      end
      object IndentCommentsCheck: TCheckBox
        Left = 240
        Top = 42
        Width = 170
        Height = 19
        Caption = 'Indent comments'
        TabOrder = 4
      end
      object IndentCompDirectivesCheck: TCheckBox
        Left = 240
        Top = 64
        Width = 170
        Height = 20
        Caption = 'Indent compiler directives'
        TabOrder = 5
      end
      object NoIndentElseIfCheck: TCheckBox
        Left = 240
        Top = 16
        Width = 170
        Height = 27
        Caption = 'Never indent else if'
        TabOrder = 3
      end
      object grp_ExtraIndentBefore: TGroupBox
        Left = 8
        Top = 53
        Width = 193
        Height = 129
        Caption = 'Extra Indent Before'
        TabOrder = 2
        object chk_IndentCaseElse: TCheckBox
          Left = 16
          Top = 96
          Width = 172
          Height = 20
          Caption = 'else in a case block'
          TabOrder = 3
        end
        object chk_IndentTryElse: TCheckBox
          Left = 16
          Top = 72
          Width = 172
          Height = 21
          Caption = 'else in a try block'
          TabOrder = 2
        end
        object chk_IndentTry: TCheckBox
          Left = 16
          Top = 48
          Width = 172
          Height = 20
          Caption = 'try'
          TabOrder = 1
        end
        object chk_IndentBegin: TCheckBox
          Left = 16
          Top = 24
          Width = 172
          Height = 20
          Caption = 'begin'
          TabOrder = 0
        end
      end
    end
    object ts_Spacing: TTabSheet
      Caption = 'Spacing'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        470
        359)
      object grid_Spacing: TStringGrid
        Left = 8
        Top = 8
        Width = 453
        Height = 338
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 3
        DefaultColWidth = 100
        DefaultRowHeight = 16
        FixedCols = 0
        RowCount = 15
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goEditing]
        TabOrder = 0
        ColWidths = (
          187
          119
          108)
      end
    end
    object ts_LineBreaks: TTabSheet
      Caption = 'Line Breaks'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object l_BeginStyle: TLabel
        Left = 240
        Top = 104
        Width = 52
        Height = 13
        Margins.Bottom = 0
        Caption = 'Begin style'
      end
      object l_WrapAtPosition: TLabel
        Left = 248
        Top = 200
        Width = 51
        Height = 13
        Margins.Bottom = 0
        Caption = 'At position'
      end
      object l_TryStyle: TLabel
        Left = 240
        Top = 136
        Width = 42
        Height = 13
        Margins.Bottom = 0
        Caption = 'Try style'
      end
      object FeedRoundBeginCombo: TComboBox
        Left = 299
        Top = 100
        Width = 163
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 2
        Items.Strings = (
          'Unchanged'
          'Hanging begin'
          'Break before and after begin')
      end
      object WrapLinesCheck: TCheckBox
        Left = 240
        Top = 176
        Width = 143
        Height = 23
        Caption = 'Wrap long lines'
        TabOrder = 4
      end
      object WrapPositionEdit: TEdit
        Left = 312
        Top = 196
        Width = 41
        Height = 21
        TabOrder = 5
        Text = '0'
      end
      object WrapPositionUpDown: TUpDown
        Left = 353
        Top = 196
        Width = 12
        Height = 21
        Associate = WrapPositionEdit
        Max = 254
        TabOrder = 6
      end
      object grp_AlwaysBreakLine: TGroupBox
        Left = 8
        Top = 7
        Width = 217
        Height = 249
        Caption = 'Always Break Line'
        TabOrder = 0
        object FeedAfterVarCheck: TCheckBox
          Left = 16
          Top = 22
          Width = 190
          Height = 24
          Caption = 'After "var", "type" etc.'
          TabOrder = 0
        end
        object FeedBeforeEndCheck: TCheckBox
          Left = 16
          Top = 46
          Width = 190
          Height = 23
          Caption = 'Before "end"'
          TabOrder = 1
        end
        object FeedAfterSemiColonCheck: TCheckBox
          Left = 16
          Top = 70
          Width = 190
          Height = 23
          Caption = 'After semicolon (except directives)'
          TabOrder = 2
        end
        object FeedElseIfCheck: TCheckBox
          Left = 16
          Top = 94
          Width = 190
          Height = 23
          Caption = 'Between else and if'
          TabOrder = 3
        end
        object FeedAfterThenCheck: TCheckBox
          Left = 16
          Top = 118
          Width = 190
          Height = 24
          Caption = 'After "then","else","do",":"'
          TabOrder = 4
          OnClick = FeedAfterThenCheckClick
        end
        object ExceptSingleCheck: TCheckBox
          Left = 32
          Top = 142
          Width = 180
          Height = 20
          Caption = 'Except single lines'
          TabOrder = 5
        end
        object NoFeedBeforeThenCheck: TCheckBox
          Left = 16
          Top = 166
          Width = 190
          Height = 24
          Caption = 'Never before "then", "do"'
          TabOrder = 6
        end
        object FeedEachUnitCheck: TCheckBox
          Left = 16
          Top = 190
          Width = 190
          Height = 25
          Caption = 'Between every unit in "uses"'
          TabOrder = 7
        end
        object RemoveDoubleBlankCheck: TCheckBox
          Left = 16
          Top = 214
          Width = 190
          Height = 23
          Caption = 'Remove double blank lines'
          TabOrder = 8
        end
      end
      object grp_ForceBlankLineBetween: TGroupBox
        Left = 240
        Top = 8
        Width = 221
        Height = 77
        Caption = 'Force a Blank Line Between'
        TabOrder = 1
        object BlankProcCheck: TCheckBox
          Left = 12
          Top = 20
          Width = 185
          Height = 23
          Caption = 'Main procedures/functions'
          TabOrder = 0
        end
        object BlankSubProcCheck: TCheckBox
          Left = 12
          Top = 44
          Width = 185
          Height = 24
          Caption = 'Local procedures/functions'
          TabOrder = 1
        end
      end
      object FeedRoundTryCombo: TComboBox
        Left = 299
        Top = 132
        Width = 163
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 3
        Items.Strings = (
          'Unchanged'
          'Hanging try'
          'Break before and after try')
      end
    end
    object ts_Capitalization: TTabSheet
      Caption = 'Capitalization'
      DesignSize = (
        470
        359)
      object l_Capitalize: TLabel
        Left = 8
        Top = 74
        Width = 46
        Height = 13
        Margins.Bottom = 0
        Caption = 'Capitalize'
      end
      object l_ReservedWords: TLabel
        Left = 8
        Top = 18
        Width = 78
        Height = 13
        Margins.Bottom = 0
        Caption = 'Reserved words'
      end
      object l_StandardDirectives: TLabel
        Left = 8
        Top = 42
        Width = 93
        Height = 13
        Margins.Bottom = 0
        Caption = 'Standard directives'
      end
      object UpperCompDirectivesCheck: TCheckBox
        Left = 136
        Top = 68
        Width = 200
        Height = 25
        Caption = 'Compiler directives'
        TabOrder = 2
      end
      object UpperNumbersCheck: TCheckBox
        Left = 136
        Top = 90
        Width = 200
        Height = 25
        Caption = 'Hex numbers'
        TabOrder = 3
      end
      object ReservedCaseCombo: TComboBox
        Left = 136
        Top = 14
        Width = 118
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Lower case'
          'Upper case'
          'Only first up'
          'Unchanged')
      end
      object StandDirectivesCombo: TComboBox
        Left = 136
        Top = 38
        Width = 118
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          'Lower case'
          'Upper case'
          'Only first up'
          'Unchanged')
      end
      object EditButton: TButton
        Left = 392
        Top = 276
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Edit ...'
        TabOrder = 7
        OnClick = EditButtonClick
      end
      object rb_CapitalizationInRegistry: TRadioButton
        Left = 16
        Top = 280
        Width = 238
        Height = 17
        Caption = 'Stored in Registry'
        Checked = True
        TabOrder = 5
        TabStop = True
        OnClick = rb_CapitalizationInRegistryClick
      end
      object rb_CapitalizationInFile: TRadioButton
        Left = 16
        Top = 303
        Width = 238
        Height = 17
        Caption = 'Stored in File'
        TabOrder = 6
        OnClick = rb_CapitalizationInFileClick
      end
      object ed_CapitalizationFile: TEdit
        Left = 32
        Top = 326
        Width = 354
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 8
      end
      object b_CapitalizationSelect: TButton
        Left = 392
        Top = 324
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Select ...'
        Enabled = False
        TabOrder = 9
        OnClick = b_CapitalizationSelectClick
      end
      object rg_Capitalization: TRadioGroup
        Left = 8
        Top = 121
        Width = 246
        Height = 134
        Caption = 'User defined capitalization'
        Items.Strings = (
          'Do not use list'
          'Add new words only'
          'Use list'
          'Use list (except standard directives)'
          'Add and use'
          'Add and use (except standard directives)')
        TabOrder = 4
        TabStop = True
      end
    end
    object ts_Align: TTabSheet
      Caption = 'Align'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object l_AlignComentsAtPosition: TLabel
        Left = 12
        Top = 35
        Width = 51
        Height = 13
        Margins.Bottom = 0
        Caption = 'At position'
      end
      object l_AlignVarAtPosition: TLabel
        Left = 12
        Top = 115
        Width = 51
        Height = 13
        Margins.Bottom = 0
        Caption = 'At position'
      end
      object AlignCommentsCheck: TCheckBox
        Left = 12
        Top = 11
        Width = 339
        Height = 20
        Caption = 'Align simple comments after code'
        TabOrder = 0
      end
      object AlignCommentPosEdit: TEdit
        Left = 140
        Top = 31
        Width = 63
        Height = 21
        TabOrder = 1
        Text = '0'
      end
      object AlignCommentPosUpDown: TUpDown
        Left = 203
        Top = 31
        Width = 10
        Height = 21
        Associate = AlignCommentPosEdit
        TabOrder = 2
      end
      object AlignVarCheck: TCheckBox
        Left = 12
        Top = 91
        Width = 339
        Height = 20
        Caption = 'Align var/const statements'
        TabOrder = 3
      end
      object AlignVarPosEdit: TEdit
        Left = 140
        Top = 111
        Width = 63
        Height = 21
        TabOrder = 4
        Text = '0'
      end
      object AlignVarPosUpDown: TUpDown
        Left = 203
        Top = 111
        Width = 10
        Height = 21
        Associate = AlignVarPosEdit
        TabOrder = 5
      end
    end
    object ts_Misc: TTabSheet
      Caption = 'Misc.'
      object chk_ShowDone: TCheckBox
        Left = 16
        Top = 200
        Width = 377
        Height = 17
        Caption = 'Show confirmation dialog when complete'
        TabOrder = 2
      end
      object grp_ConfigPrecedence: TGroupBox
        Left = 8
        Top = 88
        Width = 281
        Height = 97
        Caption = 'Configuration Precedence'
        TabOrder = 1
        object lb_Precedence: TListBox
          Left = 8
          Top = 24
          Width = 193
          Height = 65
          ItemHeight = 13
          TabOrder = 0
          OnClick = lb_PrecedenceClick
        end
        object b_PrecedenceUp: TButton
          Left = 208
          Top = 24
          Width = 65
          Height = 25
          Caption = 'Move Up'
          TabOrder = 1
          OnClick = b_PrecedenceUpClick
        end
        object b_PrecedenceDown: TButton
          Left = 208
          Top = 56
          Width = 67
          Height = 25
          Caption = 'Move Down'
          TabOrder = 2
          OnClick = b_PrecedenceDownClick
        end
      end
      object grp_DirectivesPreventFormatting: TGroupBox
        Left = 8
        Top = 8
        Width = 281
        Height = 73
        Margins.Bottom = 0
        Caption = 'Directives in source to prevent formatting:'
        TabOrder = 0
        object l_MiscStart: TLabel
          Left = 8
          Top = 24
          Width = 24
          Height = 13
          Margins.Bottom = 0
          Caption = 'Start'
        end
        object l_MiscEnd: TLabel
          Left = 144
          Top = 24
          Width = 18
          Height = 13
          Margins.Bottom = 0
          Caption = 'End'
        end
        object StartCommentOutEdit: TEdit
          Left = 8
          Top = 40
          Width = 129
          Height = 21
          MaxLength = 20
          TabOrder = 0
        end
        object EndCommentOutEdit: TEdit
          Left = 144
          Top = 40
          Width = 129
          Height = 21
          MaxLength = 20
          TabOrder = 1
        end
      end
    end
    object ts_Preview: TTabSheet
      Caption = 'Preview'
      ImageIndex = 6
      OnResize = ts_PreviewResize
      OnShow = ts_PreviewShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        470
        359)
      object l_Before: TLabel
        Left = 6
        Top = 0
        Width = 32
        Height = 13
        Margins.Bottom = 0
        Caption = 'Before'
      end
      object l_After: TLabel
        Left = 252
        Top = 0
        Width = 22
        Height = 13
        Margins.Bottom = 0
        Caption = 'After'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object m_PreviewBefore: TMemo
        Left = 6
        Top = 16
        Width = 233
        Height = 305
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        OnChange = ts_PreviewShow
        OnClick = m_PreviewBeforeClick
        OnEnter = m_PreviewBeforeClick
        OnKeyDown = m_PreviewBeforeKeyDown
        OnKeyPress = m_PreviewBeforeKeyPress
        OnKeyUp = m_PreviewBeforeKeyDown
        OnMouseDown = m_PreviewBeforeMouseDown
      end
      object m_PreviewAfter: TMemo
        Left = 249
        Top = 16
        Width = 214
        Height = 305
        Anchors = [akLeft, akTop, akRight]
        Color = clAqua
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object p_Botton: TPanel
    Left = 0
    Top = 387
    Width = 478
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      478
      41)
    object b_Help: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 0
      OnClick = b_HelpClick
    end
    object b_Ok: TButton
      Left = 316
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object b_Cancel: TButton
      Left = 396
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object b_Tools: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Hint = 
        'You can add {GXFormatter.config=<name>} as the first line to a u' +
        'nit to force a configuration for that particular unit.'
      Caption = 'Tools >'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = b_ToolsClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Any file(*.*)|*.*|Text file (*.txt)|*.txt'
    Left = 581
    Top = 7
  end
  object pm_Extra: TPopupMenu
    Left = 400
    Top = 64
    object mi_ResetTo: TMenuItem
      Caption = 'Reset to'
      object mi_ResetToDefault: TMenuItem
        Caption = '<default>'
        OnClick = mi_ResetToDefaultClick
      end
    end
    object mi_Import: TMenuItem
      Caption = 'Import ...'
      OnClick = mi_ImportClick
    end
    object mi_Export: TMenuItem
      Caption = 'Export ...'
      OnClick = mi_ExportClick
    end
  end
  object od_Import: TOpenDialog
    Filter = 'INI-Files (*.ini)|*.ini|all files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 400
    Top = 176
  end
  object sd_Export: TSaveDialog
    Filter = 'INI-Files (*.ini)|*.ini|all files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing, ofDontAddToRecent]
    Left = 400
    Top = 120
  end
  object od_CapitalizationFile: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'text files (*.txt)|*.txt|all files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 288
    Top = 344
  end
end
