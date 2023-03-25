inherited fmCodeFormatterConfig: TfmCodeFormatterConfig
  Left = 532
  HelpContext = 100
  Caption = 'Delphi Code Formatter Configuration'
  ClientHeight = 530
  ClientWidth = 1015
  OldCreateOrder = False
  PopupMenu = pm_Extra
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object p_Main: TPanel
    Left = 0
    Top = 0
    Width = 1015
    Height = 496
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    TabOrder = 0
    object pc_Main: TPageControl
      Left = 7
      Top = 7
      Width = 434
      Height = 482
      ActivePage = ts_Indent
      Align = alLeft
      TabOrder = 0
      OnChange = UpdatePreview
      object ts_Indent: TTabSheet
        Caption = 'Indent'
        object l_SpacesPerIndent: TLabel
          Left = 16
          Top = 8
          Width = 88
          Height = 13
          Caption = 'Spaces per Indent'
        end
        object ed_SpacePerIndent: TEdit
          Left = 16
          Top = 24
          Width = 65
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = UpdatePreview
        end
        object ud_SpacePerIndent: TUpDown
          Left = 81
          Top = 24
          Width = 16
          Height = 21
          Associate = ed_SpacePerIndent
          Min = 0
          Max = 10
          Position = 0
          TabOrder = 1
          Wrap = False
        end
        object chk_IndentComments: TCheckBox
          Left = 216
          Top = 103
          Width = 238
          Height = 14
          Caption = 'Indent comments'
          TabOrder = 5
          OnClick = UpdatePreview
        end
        object chk_IndentCompDirectives: TCheckBox
          Left = 216
          Top = 123
          Width = 238
          Height = 14
          Caption = 'Indent compiler directives'
          TabOrder = 6
          OnClick = UpdatePreview
        end
        object chk_NoIndentElseIf: TCheckBox
          Left = 216
          Top = 64
          Width = 238
          Height = 14
          Caption = 'Never indent else if'
          TabOrder = 3
          OnClick = UpdatePreview
        end
        object chk_NoIndentUsesComma: TCheckBox
          Left = 216
          Top = 142
          Width = 238
          Height = 14
          Hint = 
            'Do not ident units in the uses block if the line starts with a c' +
            'omma.'#13#10'Note: This disables Line Breaks after each unit uses list' +
            '.'
          Caption = 'Do not indent uses starting with ,'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = UpdatePreview
        end
        object grp_ExtraIndentBefore: TGroupBox
          Left = 8
          Top = 56
          Width = 193
          Height = 121
          Caption = 'Extra Indent Before'
          TabOrder = 2
          object chk_IndentCaseElse: TCheckBox
            Left = 8
            Top = 96
            Width = 140
            Height = 14
            Caption = 'else in a case block'
            TabOrder = 3
            OnClick = UpdatePreview
          end
          object chk_IndentTryElse: TCheckBox
            Left = 8
            Top = 72
            Width = 140
            Height = 13
            Caption = 'else in a try block'
            TabOrder = 2
            OnClick = UpdatePreview
          end
          object chk_IndentTry: TCheckBox
            Left = 8
            Top = 48
            Width = 140
            Height = 14
            Caption = 'try'
            TabOrder = 1
            OnClick = UpdatePreview
          end
          object chk_IndentBegin: TCheckBox
            Left = 8
            Top = 24
            Width = 140
            Height = 13
            Caption = 'begin'
            TabOrder = 0
            OnClick = UpdatePreview
          end
        end
        object chk_NoIndentVarDecl: TCheckBox
          Left = 216
          Top = 84
          Width = 238
          Height = 14
          Caption = 'Never indent var declaration'
          TabOrder = 4
          OnClick = UpdatePreview
        end
      end
      object ts_Spacing: TTabSheet
        Caption = 'Spacing'
        object grid_Spacing: TStringGrid
          Left = 0
          Top = 0
          Width = 426
          Height = 454
          Align = alClient
          ColCount = 3
          DefaultColWidth = 100
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 15
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goEditing]
          TabOrder = 0
          ColWidths = (
            170
            120
            110)
        end
      end
      object ts_LineBreaks: TTabSheet
        Caption = 'Line Breaks'
        object l_BeginStyle: TLabel
          Left = 240
          Top = 96
          Width = 52
          Height = 13
          Caption = 'Begin style'
        end
        object l_WrapAtPosition: TLabel
          Left = 248
          Top = 216
          Width = 51
          Height = 13
          Caption = 'At position'
        end
        object l_TryStyle: TLabel
          Left = 240
          Top = 144
          Width = 42
          Height = 13
          Caption = 'Try style'
        end
        object cmb_FeedRoundBegin: TComboBox
          Left = 240
          Top = 112
          Width = 185
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = UpdatePreview
          Items.Strings = (
            'Unchanged'
            'Hanging begin'
            'Break before and after begin')
        end
        object chk_WrapLines: TCheckBox
          Left = 240
          Top = 192
          Width = 167
          Height = 18
          Caption = 'Wrap long lines'
          TabOrder = 4
          OnClick = UpdatePreview
        end
        object ed_WrapPosition: TEdit
          Left = 264
          Top = 232
          Width = 33
          Height = 21
          TabOrder = 5
          Text = '0'
          OnChange = UpdatePreview
        end
        object ud_WrapPosition: TUpDown
          Left = 297
          Top = 232
          Width = 13
          Height = 21
          Associate = ed_WrapPosition
          Min = 0
          Max = 254
          Position = 0
          TabOrder = 6
          Wrap = False
        end
        object grp_AlwaysBreakLine: TGroupBox
          Left = 8
          Top = 8
          Width = 217
          Height = 314
          Caption = 'Always Break Line'
          TabOrder = 0
          object chk_FeedAfterVar: TCheckBox
            Left = 8
            Top = 24
            Width = 201
            Height = 13
            Caption = 'After "var", "type" etc.'
            TabOrder = 0
            OnClick = UpdatePreview
          end
          object chk_FeedBeforeEnd: TCheckBox
            Left = 8
            Top = 72
            Width = 201
            Height = 13
            Caption = 'Before "end"'
            TabOrder = 2
            OnClick = UpdatePreview
          end
          object chk_FeedAfterSemiColon: TCheckBox
            Left = 8
            Top = 96
            Width = 201
            Height = 14
            Caption = 'After semicolon (except directives)'
            TabOrder = 3
            OnClick = UpdatePreview
          end
          object chk_FeedElseIf: TCheckBox
            Left = 8
            Top = 120
            Width = 201
            Height = 13
            Caption = 'Between else and if'
            TabOrder = 4
            OnClick = UpdatePreview
          end
          object chk_FeedAfterThen: TCheckBox
            Left = 8
            Top = 144
            Width = 201
            Height = 14
            Caption = 'After "then","else","do",":"'
            TabOrder = 5
            OnClick = chk_FeedAfterThenClick
          end
          object chk_ExceptSingle: TCheckBox
            Left = 16
            Top = 168
            Width = 193
            Height = 13
            Caption = 'Except single lines'
            TabOrder = 6
            OnClick = UpdatePreview
          end
          object chk_NoFeedBeforeThen: TCheckBox
            Left = 8
            Top = 192
            Width = 201
            Height = 14
            Caption = 'Never before "then", "do"'
            TabOrder = 7
            OnClick = UpdatePreview
          end
          object chk_FeedAfterUses: TCheckBox
            Left = 8
            Top = 216
            Width = 201
            Height = 13
            Caption = 'After "uses"'
            TabOrder = 8
            OnClick = UpdatePreview
          end
          object chk_FeedEachUnit: TCheckBox
            Left = 8
            Top = 240
            Width = 201
            Height = 13
            Caption = 'Between every unit in "uses"'
            TabOrder = 9
            OnClick = chk_FeedEachUnitClick
          end
          object chk_FeedEachUnitBeforeComma: TCheckBox
            Left = 16
            Top = 264
            Width = 168
            Height = 13
            Caption = 'Before the comma'
            TabOrder = 10
            OnClick = UpdatePreview
          end
          object chk_RemoveDoubleBlank: TCheckBox
            Left = 8
            Top = 288
            Width = 201
            Height = 14
            Caption = 'Remove double blank lines'
            TabOrder = 11
            OnClick = UpdatePreview
          end
          object chk_FeedBeforeElse: TCheckBox
            Left = 8
            Top = 48
            Width = 201
            Height = 14
            Caption = 'Before "else"'
            TabOrder = 1
            OnClick = UpdatePreview
          end
        end
        object grp_ForceBlankLineBetween: TGroupBox
          Left = 232
          Top = 8
          Width = 193
          Height = 74
          Caption = 'Force a Blank Line Between'
          TabOrder = 1
          object chk_BlankProc: TCheckBox
            Left = 8
            Top = 24
            Width = 177
            Height = 13
            Caption = 'Main procedures/functions'
            TabOrder = 0
            OnClick = UpdatePreview
          end
          object chk_BlankSubProc: TCheckBox
            Left = 8
            Top = 48
            Width = 177
            Height = 14
            Caption = 'Local procedures/functions'
            TabOrder = 1
            OnClick = UpdatePreview
          end
        end
        object cmb_FeedRoundTry: TComboBox
          Left = 240
          Top = 160
          Width = 185
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          OnChange = UpdatePreview
          Items.Strings = (
            'Unchanged'
            'Hanging try'
            'Break before and after try')
        end
      end
      object ts_Capitalization: TTabSheet
        Caption = 'Capitalization'
        DesignSize = (
          426
          454)
        object l_Capitalize: TLabel
          Left = 16
          Top = 8
          Width = 46
          Height = 13
          Caption = 'Capitalize'
        end
        object l_ReservedWords: TLabel
          Left = 16
          Top = 48
          Width = 78
          Height = 13
          Caption = 'Reserved words'
        end
        object l_StandardDirectives: TLabel
          Left = 144
          Top = 48
          Width = 93
          Height = 13
          Caption = 'Standard directives'
        end
        object l_Identifiers: TLabel
          Left = 272
          Top = 48
          Width = 49
          Height = 13
          Caption = 'Identifiers'
        end
        object l_CapitalizationInFile: TLabel
          Left = 16
          Top = 248
          Width = 126
          Height = 13
          Caption = 'Capitalization stored in file'
        end
        object chk_UpperCompDirectives: TCheckBox
          Left = 16
          Top = 24
          Width = 118
          Height = 20
          Caption = 'Compiler directives'
          TabOrder = 0
          OnClick = UpdatePreview
        end
        object chk_UpperNumbers: TCheckBox
          Left = 136
          Top = 24
          Width = 117
          Height = 20
          Caption = 'Hex numbers'
          TabOrder = 1
          OnClick = UpdatePreview
        end
        object cmb_ReservedCase: TComboBox
          Left = 16
          Top = 64
          Width = 118
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = UpdatePreview
          Items.Strings = (
            'Lower case'
            'Upper case'
            'Only first up'
            'Unchanged')
        end
        object cmb_StandDirectives: TComboBox
          Left = 144
          Top = 64
          Width = 117
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          OnChange = UpdatePreview
          Items.Strings = (
            'Lower case'
            'Upper case'
            'Only first up'
            'Unchanged')
        end
        object b_EditCapitalization: TButton
          Left = 347
          Top = 232
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Edit ...'
          TabOrder = 6
          OnClick = b_EditCapitalizationClick
        end
        object ed_CapitalizationFile: TEdit
          Left = 16
          Top = 264
          Width = 323
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
        end
        object b_CapitalizationSelect: TButton
          Left = 347
          Top = 262
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Select ...'
          TabOrder = 8
          OnClick = b_CapitalizationSelectClick
        end
        object rg_Capitalization: TRadioGroup
          Left = 8
          Top = 96
          Width = 385
          Height = 129
          Caption = 'User Defined Capitalization'
          Items.Strings = (
            'Do not use list'
            'Add new words only'
            'Use list'
            'Use list (except standard directives)'
            'Add and use'
            'Add and use (except standard directives)')
          TabOrder = 5
          OnClick = UpdatePreview
        end
        object cmb_IdentifiersCase: TComboBox
          Left = 272
          Top = 64
          Width = 118
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
          OnChange = UpdatePreview
          Items.Strings = (
            'Lower case'
            'Upper case'
            'Only first up'
            'Unchanged'
            'Use first occurence')
        end
      end
      object ts_Align: TTabSheet
        Caption = 'Align'
        object l_AlignComentsAtPosition: TLabel
          Left = 40
          Top = 40
          Width = 51
          Height = 13
          Caption = 'At position'
        end
        object l_AlignVarAtPosition: TLabel
          Left = 40
          Top = 136
          Width = 51
          Height = 13
          Caption = 'At position'
        end
        object chk_AlignComments: TCheckBox
          Left = 16
          Top = 16
          Width = 275
          Height = 14
          Caption = 'Align simple comments after code'
          TabOrder = 0
          OnClick = UpdatePreview
        end
        object ed_AlignCommentPos: TEdit
          Left = 40
          Top = 56
          Width = 51
          Height = 21
          TabOrder = 1
          Text = '0'
          OnChange = UpdatePreview
        end
        object ud_AlignCommentPos: TUpDown
          Left = 91
          Top = 48
          Width = 13
          Height = 21
          Associate = ed_AlignCommentPos
          TabOrder = 2
        end
        object chk_AlignConst: TCheckBox
          Left = 16
          Top = 88
          Width = 275
          Height = 14
          Caption = 'Align const statements and resource strings'
          TabOrder = 3
          OnClick = UpdatePreview
        end
        object chk_AlignVar: TCheckBox
          Left = 16
          Top = 112
          Width = 275
          Height = 14
          Caption = 'Align var statements'
          TabOrder = 4
          OnClick = UpdatePreview
        end
        object ed_AlignVarPos: TEdit
          Left = 40
          Top = 152
          Width = 51
          Height = 21
          TabOrder = 5
          Text = '0'
          OnChange = UpdatePreview
        end
        object ud_AlignVarPos: TUpDown
          Left = 91
          Top = 168
          Width = 13
          Height = 21
          Associate = ed_AlignVarPos
          TabOrder = 6
        end
      end
      object ts_Misc: TTabSheet
        Caption = 'Misc.'
        object grp_ConfigPrecedence: TGroupBox
          Left = 7
          Top = 72
          Width = 274
          Height = 97
          Caption = 'Configuration Precedence'
          TabOrder = 1
          object lb_Precedence: TListBox
            Left = 7
            Top = 20
            Width = 178
            Height = 69
            ItemHeight = 13
            TabOrder = 0
            OnClick = lb_PrecedenceClick
          end
          object b_PrecedenceUp: TButton
            Left = 193
            Top = 20
            Width = 75
            Height = 25
            Caption = 'Move Up'
            TabOrder = 1
            OnClick = b_PrecedenceUpClick
          end
          object b_PrecedenceDown: TButton
            Left = 193
            Top = 54
            Width = 75
            Height = 25
            Caption = 'Move Down'
            TabOrder = 2
            OnClick = b_PrecedenceDownClick
          end
        end
        object grp_DirectivesPreventFormatting: TGroupBox
          Left = 7
          Top = 7
          Width = 250
          Height = 59
          Caption = 'Source Directives to Prevent Formatting'
          TabOrder = 0
          object l_MiscStart: TLabel
            Left = 7
            Top = 20
            Width = 24
            Height = 13
            Caption = 'Start'
          end
          object l_MiscEnd: TLabel
            Left = 124
            Top = 20
            Width = 18
            Height = 13
            Caption = 'End'
          end
          object ed_StartComment: TEdit
            Left = 3
            Top = 33
            Width = 108
            Height = 21
            MaxLength = 20
            TabOrder = 0
            OnChange = UpdatePreview
          end
          object ed_EndCommentOut: TEdit
            Left = 124
            Top = 33
            Width = 108
            Height = 21
            MaxLength = 20
            TabOrder = 1
            OnChange = UpdatePreview
          end
        end
      end
    end
    object p_Preview: TPanel
      Left = 441
      Top = 7
      Width = 567
      Height = 482
      Align = alClient
      TabOrder = 1
      OnResize = p_PreviewResize
      object l_Before: TLabel
        Left = 24
        Top = 1
        Width = 32
        Height = 13
        Caption = 'Before'
      end
      object l_After: TLabel
        Left = 202
        Top = 1
        Width = 22
        Height = 13
        Caption = 'After'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
  end
  object p_Botton: TPanel
    Left = 0
    Top = 496
    Width = 1015
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      1015
      34)
    object b_Help: TButton
      Left = 8
      Top = 1
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 0
      Visible = False
      OnClick = b_HelpClick
    end
    object b_Ok: TButton
      Left = 853
      Top = 1
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object b_Cancel: TButton
      Left = 934
      Top = 1
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
      Top = 1
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
  object pm_Extra: TPopupMenu
    Left = 328
    Top = 40
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
end
