object fmOpenFileConfig: TfmOpenFileConfig
  Left = 332
  Top = 206
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Open File Configuration'
  ClientHeight = 369
  ClientWidth = 475
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 15
  object pnlButtons: TPanel
    Left = 0
    Top = 336
    Width = 475
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 174
      Top = 0
      Width = 301
      Height = 33
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnHelp: TButton
        Left = 220
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Help'
        ModalResult = 2
        TabOrder = 2
        OnClick = btnHelpClick
      end
      object btnCancel: TButton
        Left = 139
        Top = 2
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnOK: TButton
        Left = 57
        Top = 2
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = btnOKClick
      end
    end
  end
  object pnlConfig: TPanel
    Left = 0
    Top = 0
    Width = 475
    Height = 336
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object pcConfigPages: TPageControl
      Left = 6
      Top = 6
      Width = 463
      Height = 324
      ActivePage = tsTypes
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tsTypes: TTabSheet
        Caption = 'File Groups'
        DesignSize = (
          455
          294)
        object lblExtension: TLabel
          Left = 180
          Top = 13
          Width = 76
          Height = 15
          Alignment = taRightJustify
          Caption = 'File extensions'
          FocusControl = edtExtension
        end
        object lblMaxRecentFiles: TLabel
          Left = 174
          Top = 45
          Width = 82
          Height = 15
          Alignment = taRightJustify
          Caption = 'Max recent files'
          FocusControl = edtMaxRecentFiles
        end
        object gbxCustomDirectory: TGroupBox
          Left = 173
          Top = 69
          Width = 273
          Height = 219
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Custom Search Path'
          TabOrder = 5
          DesignSize = (
            273
            219)
          object btnDirectory: TButton
            Left = 241
            Top = 159
            Width = 22
            Height = 20
            Anchors = [akRight, akBottom]
            Caption = '...'
            TabStop = False
            OnClick = btnDirectoryClick
          end
          object lbxDirectoryList: TListBox
            Left = 10
            Top = 66
            Width = 253
            Height = 88
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 15
            TabOrder = 2
            OnClick = lbxDirectoryListClick
          end
          object btnDirectoryAdd: TButton
            Left = 90
            Top = 186
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Add'
            TabOrder = 5
            OnClick = btnDirectoryAddClick
          end
          object btnDirectoryDelete: TButton
            Left = 170
            Top = 186
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Delete'
            TabOrder = 6
            OnClick = btnDirectoryDeleteClick
          end
          object chkRecursive: TCheckBox
            Left = 12
            Top = 45
            Width = 250
            Height = 17
            Caption = 'Include subdirectories'
            TabOrder = 1
          end
          object btnDirectoryReplace: TButton
            Left = 10
            Top = 186
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Replace'
            TabOrder = 4
            OnClick = btnDirectoryReplaceClick
          end
          object edtDirectory: TEdit
            Left = 10
            Top = 159
            Width = 232
            Height = 23
            Anchors = [akLeft, akRight, akBottom]
            TabOrder = 3
          end
          object chkCustomDirectoryList: TCheckBox
            Left = 12
            Top = 24
            Width = 250
            Height = 17
            Caption = 'Add custom directories to search path'
            TabOrder = 0
            OnClick = chkCustomDirectoryListClick
          end
        end
        object lbxTypeList: TListBox
          Left = 8
          Top = 9
          Width = 153
          Height = 248
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 15
          Sorted = True
          TabOrder = 0
          OnClick = lbxTypeListClick
        end
        object edtExtension: TEdit
          Left = 265
          Top = 9
          Width = 181
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
        end
        object btnTypeAdd: TButton
          Left = 8
          Top = 263
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Add'
          TabOrder = 1
          OnClick = btnTypeAddClick
        end
        object btnTypeDelete: TButton
          Left = 87
          Top = 263
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnTypeDeleteClick
        end
        object edtMaxRecentFiles: TEdit
          Left = 265
          Top = 40
          Width = 56
          Height = 23
          TabOrder = 4
          Text = '20'
        end
      end
      object tsIDEOptions: TTabSheet
        Caption = 'Settings'
        ImageIndex = 1
        object gbxIDEMenuItems: TGroupBox
          Left = 8
          Top = 137
          Width = 323
          Height = 152
          Caption = 'IDE Menu Item Overrides'
          TabOrder = 1
          object lblMenuItem: TLabel
            Left = 20
            Top = 24
            Width = 78
            Height = 15
            Caption = 'IDE Menu Item'
          end
          object lblFileGroup: TLabel
            Left = 137
            Top = 24
            Width = 95
            Height = 15
            Caption = 'Default File Group'
            FocusControl = cbxViewUnitType
          end
          object bvlRow: TBevel
            Left = 16
            Top = 42
            Width = 295
            Height = 5
            Shape = bsTopLine
          end
          object chkOverrideViewUnit: TCheckBox
            Left = 20
            Top = 52
            Width = 110
            Height = 17
            Caption = 'View units'
            TabOrder = 0
          end
          object chkOverrideViewForm: TCheckBox
            Left = 20
            Top = 84
            Width = 110
            Height = 17
            Caption = 'View forms'
            TabOrder = 2
          end
          object cbxViewUnitType: TComboBox
            Left = 136
            Top = 50
            Width = 160
            Height = 23
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 1
          end
          object cbxViewFormType: TComboBox
            Left = 136
            Top = 82
            Width = 160
            Height = 23
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 3
          end
          object chkOverrideOpenProject: TCheckBox
            Left = 20
            Top = 116
            Width = 110
            Height = 17
            Caption = 'Open project'
            TabOrder = 4
          end
          object cbxOpenProjectType: TComboBox
            Left = 136
            Top = 114
            Width = 160
            Height = 23
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 5
          end
        end
        object gbxGeneralSettings: TGroupBox
          Left = 8
          Top = 4
          Width = 323
          Height = 127
          Caption = 'General Settings'
          TabOrder = 0
          object lblDefault: TLabel
            Left = 20
            Top = 52
            Width = 92
            Height = 15
            Caption = 'Default file group'
            FocusControl = cbxDefaultFileTypes
          end
          object cbxDefaultFileTypes: TComboBox
            Left = 136
            Top = 50
            Width = 160
            Height = 23
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 1
          end
          object chkMatchAnywhere: TCheckBox
            Left = 20
            Top = 24
            Width = 301
            Height = 17
            Caption = 'Match typed string anywhere in the file names'
            TabOrder = 0
          end
          object chkMapTab: TCheckBox
            Left = 20
            Top = 88
            Width = 301
            Height = 17
            Caption = 'Show Map tab with files from project'#39's .map file'
            TabOrder = 2
          end
        end
      end
    end
  end
end
