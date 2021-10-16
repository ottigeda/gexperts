object fmReplaceCompMapList: TfmReplaceCompMapList
  Left = 283
  Top = 224
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Mapping Definition List'
  ClientHeight = 176
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 461
    Height = 34
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblGroup: TLabel
      Left = 17
      Top = 12
      Width = 33
      Height = 14
      Alignment = taRightJustify
      Caption = 'Group'
      FocusControl = comGroupName
    end
    object comGroupName: TComboBox
      Left = 56
      Top = 8
      Width = 145
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 0
      OnChange = comGroupNameChange
    end
    object tbrGroups: TToolBar
      Left = 203
      Top = 8
      Width = 23
      Height = 22
      Align = alNone
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object tbnGroups: TToolButton
        Left = 0
        Top = 0
        Action = actOpenGroupList
      end
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 34
    Width = 461
    Height = 142
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object pnlMappings: TPanel
      Left = 3
      Top = 25
      Width = 455
      Height = 114
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lvMapItems: TListView
        Left = 3
        Top = 3
        Width = 449
        Height = 108
        Align = alClient
        Columns = <
          item
            Caption = 'Group'
            Width = 125
          end
          item
            Caption = 'Source'
            Width = 160
          end
          item
            Caption = 'Destination'
            Width = 160
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = lvMapItemsClick
        OnColumnClick = lvMapItemsColumnClick
        OnDblClick = lvMapItemsDblClick
      end
    end
    object tbrReplacement: TToolBar
      Left = 3
      Top = 3
      Width = 455
      Height = 22
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      EdgeBorders = []
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object pnlToolSep: TPanel
        Left = 0
        Top = 0
        Width = 5
        Height = 22
        BevelOuter = bvNone
        TabOrder = 0
      end
      object tbnAdd: TToolButton
        Left = 5
        Top = 0
        Action = actAdd
      end
      object tbnEdit: TToolButton
        Left = 28
        Top = 0
        Action = actEdit
      end
      object tbnDelete: TToolButton
        Left = 51
        Top = 0
        Action = actDelete
      end
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    Left = 112
    Top = 114
    object actAdd: TAction
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 41
      OnExecute = actAddExecute
    end
    object actEdit: TAction
      Caption = 'Edit'
      Hint = 'Edit'
      ImageIndex = 38
      OnExecute = actEditExecute
    end
    object actDelete: TAction
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 42
      OnExecute = actDeleteExecute
    end
    object actOpenGroupList: TAction
      Hint = 'Open group list'
      ImageIndex = 67
      OnExecute = btnOpenGroupListClick
    end
  end
end
