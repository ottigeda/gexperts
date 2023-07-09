inherited fmGrepAsFind: TfmGrepAsFind
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'GExperts Find Text'
  ClientHeight = 405
  ClientWidth = 433
  PixelsPerInch = 96
  TextHeight = 13
  object pc_Main: TPageControl
    Left = 0
    Top = 0
    Width = 433
    Height = 363
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = ts_Find
    Align = alClient
    TabOrder = 0
    object ts_Find: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Find'
      object l_TextToFind: TLabel
        Left = 8
        Top = 8
        Width = 56
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '&Text to find'
      end
      object cmb_TextToFind: TComboBox
        Left = 8
        Top = 24
        Width = 409
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
      end
      object grp_Options: TGroupBox
        Left = 8
        Top = 56
        Width = 199
        Height = 97
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Options'
        TabOrder = 1
        object chk_CaseSensitive: TCheckBox
          Left = 8
          Top = 24
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Case sensitive'
          TabOrder = 0
        end
        object chk_WholeWords: TCheckBox
          Left = 8
          Top = 48
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Whole words only'
          TabOrder = 1
        end
        object chk_RegExpr: TCheckBox
          Left = 8
          Top = 72
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Regular expresssions'
          TabOrder = 2
        end
      end
      object grp_Direction: TGroupBox
        Left = 216
        Top = 56
        Width = 199
        Height = 97
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Direction'
        TabOrder = 2
        object rb_DirectionForward: TRadioButton
          Left = 8
          Top = 24
          Width = 187
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Forwar&d'
          TabOrder = 0
        end
        object rb_DirectionBackward: TRadioButton
          Left = 8
          Top = 48
          Width = 187
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Backward'
          TabOrder = 1
        end
      end
      object grp_Scope: TGroupBox
        Left = 8
        Top = 168
        Width = 199
        Height = 73
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Scope'
        TabOrder = 3
        object rb_ScopeGlobal: TRadioButton
          Left = 8
          Top = 24
          Width = 183
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Global'
          TabOrder = 0
        end
        object rb_ScopeSelected: TRadioButton
          Left = 8
          Top = 48
          Width = 183
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Selected text'
          TabOrder = 1
        end
      end
      object grp_Origin: TGroupBox
        Left = 216
        Top = 168
        Width = 199
        Height = 73
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Origin'
        TabOrder = 4
        object rb_OriginFromCursor: TRadioButton
          Left = 8
          Top = 24
          Width = 185
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&From cursor'
          TabOrder = 0
        end
        object rb_OriginEntireScope: TRadioButton
          Left = 8
          Top = 48
          Width = 185
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Entire scope'
          TabOrder = 1
        end
      end
    end
    object ts_FindInFiles: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Find in Files'
      ImageIndex = 1
      object l_FifTextToFind: TLabel
        Left = 8
        Top = 8
        Width = 56
        Height = 13
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '&Text to find'
      end
      object cmb_FifTextToFind: TComboBox
        Left = 8
        Top = 24
        Width = 409
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
      end
      object grp_FifOptions: TGroupBox
        Left = 8
        Top = 56
        Width = 409
        Height = 73
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Options'
        TabOrder = 1
        object chk_FifRegExpr: TCheckBox
          Left = 8
          Top = 48
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Regular expresssions'
          TabOrder = 0
        end
        object chk_FifWholeWords: TCheckBox
          Left = 208
          Top = 24
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Whole words only'
          TabOrder = 1
        end
        object chk_FifCaseSensitive: TCheckBox
          Left = 8
          Top = 24
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Case sensitive'
          TabOrder = 2
        end
      end
      object grp_FifWhere: TGroupBox
        Left = 8
        Top = 144
        Width = 409
        Height = 73
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Where'
        TabOrder = 2
        object rb_FifAllFilesInProject: TRadioButton
          Left = 8
          Top = 24
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Search all files in &project'
          TabOrder = 0
        end
        object rb_FifProjectGropu: TRadioButton
          Left = 216
          Top = 24
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Search all files in project &group'
          TabOrder = 1
        end
        object rb_FifAllOpen: TRadioButton
          Left = 8
          Top = 48
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Search all &open files'
          TabOrder = 2
        end
        object rb_FifDirectoris: TRadioButton
          Left = 216
          Top = 48
          Width = 185
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Search in &directories'
          TabOrder = 3
        end
      end
      object grp_FifDirOptions: TGroupBox
        Left = 8
        Top = 232
        Width = 409
        Height = 97
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Search Directory Options'
        TabOrder = 3
        object l_FifFileMask: TLabel
          Left = 8
          Top = 24
          Width = 43
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'File &mask'
        end
        object cmb_FifFileMask: TComboBox
          Left = 8
          Top = 40
          Width = 305
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 0
        end
        object b_FifBrowse: TButton
          Left = 320
          Top = 38
          Width = 75
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Browse'
          TabOrder = 1
          OnClick = b_FifBrowseClick
        end
        object chk_FifIncludeSubDirs: TCheckBox
          Left = 8
          Top = 72
          Width = 305
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Include &subdirectories'
          TabOrder = 2
        end
      end
    end
  end
  object p_Bottom: TPanel
    Left = 0
    Top = 363
    Width = 433
    Height = 42
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      433
      42)
    object b_Ok: TButton
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = b_OkClick
    end
    object b_Cancel: TButton
      Left = 352
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
