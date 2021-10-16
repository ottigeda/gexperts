object fmClassOptions: TfmClassOptions
  Left = 274
  Top = 170
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Class Browser Options'
  ClientHeight = 385
  ClientWidth = 423
  Color = clBtnFace
  Constraints.MinHeight = 365
  Constraints.MinWidth = 360
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 423
    Height = 351
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object pcClassOptions: TPageControl
      Left = 6
      Top = 6
      Width = 411
      Height = 339
      ActivePage = tshGeneric
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tshGeneric: TTabSheet
        Caption = 'Display'
        DesignSize = (
          403
          310)
        object gbxFonts: TGroupBox
          Left = 8
          Top = 8
          Width = 387
          Height = 169
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Fonts'
          TabOrder = 0
          DesignSize = (
            387
            169)
          object lblTreeViewFont: TLabel
            Left = 8
            Top = 24
            Width = 53
            Height = 14
            Alignment = taRightJustify
            Caption = '&Tree font'
            FocusControl = cbTreeView
          end
          object lblListViewFont: TLabel
            Left = 8
            Top = 72
            Width = 45
            Height = 14
            Alignment = taRightJustify
            Caption = '&List font'
            FocusControl = cbListView
          end
          object lblEditorFont: TLabel
            Left = 8
            Top = 120
            Width = 59
            Height = 14
            Alignment = taRightJustify
            Caption = '&Editor font'
            FocusControl = cbEditor
          end
          object cbTreeView: TComboBox
            Left = 8
            Top = 40
            Width = 233
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 14
            TabOrder = 0
          end
          object cbListView: TComboBox
            Left = 8
            Top = 88
            Width = 233
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 14
            TabOrder = 4
          end
          object cbEditor: TComboBox
            Left = 8
            Top = 136
            Width = 233
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 14
            TabOrder = 8
          end
          object sTreeView: TEdit
            Left = 248
            Top = 40
            Width = 33
            Height = 22
            Anchors = [akTop, akRight]
            TabOrder = 1
            Text = '1'
          end
          object sListView: TEdit
            Left = 248
            Top = 88
            Width = 35
            Height = 22
            Anchors = [akTop, akRight]
            TabOrder = 5
            Text = '1'
          end
          object sEditor: TEdit
            Left = 248
            Top = 136
            Width = 35
            Height = 22
            Anchors = [akTop, akRight]
            TabOrder = 9
            Text = '1'
          end
          object udTree: TUpDown
            Left = 281
            Top = 40
            Width = 17
            Height = 22
            Anchors = [akTop, akRight]
            Associate = sTreeView
            Min = 1
            Position = 1
            TabOrder = 2
          end
          object udList: TUpDown
            Left = 283
            Top = 88
            Width = 16
            Height = 22
            Anchors = [akTop, akRight]
            Associate = sListView
            Min = 1
            Position = 1
            TabOrder = 6
          end
          object udEditor: TUpDown
            Left = 283
            Top = 136
            Width = 16
            Height = 22
            Anchors = [akTop, akRight]
            Associate = sEditor
            Min = 1
            Position = 1
            TabOrder = 10
          end
          object b_TreeFont: TButton
            Left = 304
            Top = 39
            Width = 25
            Height = 25
            Caption = '...'
            TabOrder = 3
            OnClick = b_TreeFontClick
          end
          object b_ListFont: TButton
            Left = 304
            Top = 87
            Width = 25
            Height = 25
            Caption = '...'
            TabOrder = 7
            OnClick = b_ListFontClick
          end
          object b_EditorFont: TButton
            Left = 304
            Top = 135
            Width = 25
            Height = 25
            Caption = '...'
            TabOrder = 11
            OnClick = b_EditorFontClick
          end
        end
        object cbAutoHide: TCheckBox
          Left = 10
          Top = 184
          Width = 359
          Height = 25
          Caption = '&Auto-hide window when jumping to class/member'
          TabOrder = 1
        end
      end
      object tshFilters: TTabSheet
        Caption = 'Configuration'
        DesignSize = (
          403
          310)
        object gbxFilters: TGroupBox
          Left = 8
          Top = 8
          Width = 387
          Height = 113
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Default Filters'
          TabOrder = 0
          object cbConstants: TCheckBox
            Left = 8
            Top = 22
            Width = 169
            Height = 17
            Caption = 'Constants'
            TabOrder = 0
          end
          object cbMethods: TCheckBox
            Tag = 1
            Left = 8
            Top = 38
            Width = 169
            Height = 17
            Caption = 'Methods'
            TabOrder = 1
          end
          object cbTypes: TCheckBox
            Tag = 2
            Left = 8
            Top = 54
            Width = 169
            Height = 17
            Caption = 'Types'
            TabOrder = 2
          end
          object cbVariables: TCheckBox
            Tag = 3
            Left = 8
            Top = 70
            Width = 169
            Height = 17
            Caption = 'Variables'
            TabOrder = 3
          end
          object cbProperties: TCheckBox
            Tag = 4
            Left = 8
            Top = 86
            Width = 169
            Height = 17
            Caption = 'Properties'
            TabOrder = 4
          end
          object cbPrivate: TCheckBox
            Tag = 5
            Left = 179
            Top = 22
            Width = 135
            Height = 17
            Caption = 'Private'
            TabOrder = 5
          end
          object cbProtected: TCheckBox
            Tag = 6
            Left = 179
            Top = 38
            Width = 135
            Height = 17
            Caption = 'Protected'
            TabOrder = 6
          end
          object cbPublic: TCheckBox
            Tag = 7
            Left = 179
            Top = 54
            Width = 135
            Height = 17
            Caption = 'Public'
            TabOrder = 7
          end
          object cbPublished: TCheckBox
            Tag = 8
            Left = 179
            Top = 70
            Width = 135
            Height = 17
            Caption = 'Published'
            TabOrder = 8
          end
        end
        object gbxDiagram: TGroupBox
          Left = 8
          Top = 128
          Width = 387
          Height = 65
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Diagram Options'
          TabOrder = 1
          object cbTop: TCheckBox
            Left = 8
            Top = 22
            Width = 305
            Height = 17
            Caption = 'Show most primitive class at top of diagram'
            TabOrder = 0
          end
          object cbStayInPackage: TCheckBox
            Left = 8
            Top = 38
            Width = 305
            Height = 17
            Caption = 'Stay in source folder'
            TabOrder = 1
          end
        end
        object gbxSearch: TGroupBox
          Left = 8
          Top = 200
          Width = 387
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Parse Options'
          TabOrder = 2
          object cbParseRecursing: TCheckBox
            Left = 8
            Top = 22
            Width = 305
            Height = 17
            Caption = 'Parsing looks for files recursively in subfolders'
            TabOrder = 0
          end
        end
      end
      object tshPrinting: TTabSheet
        Caption = 'Printing'
        ImageIndex = 2
        DesignSize = (
          403
          310)
        object gbxPrintingFont: TGroupBox
          Left = 8
          Top = 8
          Width = 387
          Height = 73
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Font for Class Hierarchy'
          TabOrder = 0
          DesignSize = (
            387
            73)
          object lblFont: TLabel
            Left = 8
            Top = 24
            Width = 25
            Height = 14
            Caption = 'Font'
            FocusControl = cbReportFont
          end
          object lblFontSize: TLabel
            Left = 226
            Top = 24
            Width = 21
            Height = 14
            Caption = 'Size'
            FocusControl = spnFontSize
          end
          object cbReportFont: TComboBox
            Left = 8
            Top = 40
            Width = 209
            Height = 22
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 14
            TabOrder = 0
          end
          object spnFontSize: TEdit
            Left = 224
            Top = 40
            Width = 65
            Height = 22
            TabOrder = 1
            Text = '1'
          end
          object udReportFontSize: TUpDown
            Left = 285
            Top = 40
            Width = 16
            Height = 22
            Associate = spnFontSize
            Min = 1
            Position = 1
            TabOrder = 2
          end
        end
        object gbxPrintingBox: TGroupBox
          Left = 8
          Top = 88
          Width = 387
          Height = 129
          Caption = 'Boxes for Class Hierarchy'
          TabOrder = 1
          object lblBoxSize: TLabel
            Left = 8
            Top = 24
            Width = 94
            Height = 14
            Caption = 'Size in characters'
            FocusControl = spnBoxSize
          end
          object lblBoxSpacing: TLabel
            Left = 8
            Top = 76
            Width = 88
            Height = 14
            Caption = 'Spacing in pixels'
            FocusControl = spnBoxSpacing
          end
          object spnBoxSize: TEdit
            Left = 8
            Top = 40
            Width = 45
            Height = 22
            TabOrder = 0
            Text = '1'
          end
          object udReportBoxSize: TUpDown
            Left = 53
            Top = 40
            Width = 16
            Height = 22
            Associate = spnBoxSize
            Min = 1
            Position = 1
            TabOrder = 1
          end
          object spnBoxSpacing: TEdit
            Left = 8
            Top = 96
            Width = 45
            Height = 22
            TabOrder = 2
            Text = '1'
          end
          object udReportBoxSpacing: TUpDown
            Left = 53
            Top = 96
            Width = 16
            Height = 22
            Associate = spnBoxSpacing
            Min = 1
            Position = 1
            TabOrder = 3
          end
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 351
    Width = 423
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 238
      Top = 0
      Width = 185
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TButton
        Left = 18
        Top = 2
        Width = 75
        Height = 26
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object btnCancel: TButton
        Left = 103
        Top = 2
        Width = 75
        Height = 26
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
