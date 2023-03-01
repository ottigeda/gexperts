inherited f_EditPath: Tf_EditPath
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Edit Path'
  ClientHeight = 357
  ClientWidth = 529
  PixelsPerInch = 96
  TextHeight = 13
  object p_Top: TPanel
    Left = 0
    Top = 0
    Width = 529
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      529
      41)
    object l_Target: TLabel
      Left = 8
      Top = 12
      Width = 32
      Height = 13
      Caption = '&Target'
      FocusControl = cmb_Target
    end
    object cmb_Target: TComboBox
      Left = 64
      Top = 8
      Width = 457
      Height = 22
      Style = csOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 0
      OnChange = cmb_TargetChange
      OnDrawItem = cmb_TargetDrawItem
    end
  end
  object p_Bottom: TPanel
    Left = 0
    Top = 316
    Width = 529
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      529
      41)
    object b_Cancel: TButton
      Left = 448
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object b_OK: TButton
      Left = 368
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = b_OKClick
    end
  end
  object p_Main: TPanel
    Left = 0
    Top = 41
    Width = 529
    Height = 275
    Align = alClient
    TabOrder = 1
    object spl_Vertical: TSplitter
      Left = 1
      Top = 149
      Width = 527
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object p_Memo: TPanel
      Left = 1
      Top = 1
      Width = 527
      Height = 148
      Align = alClient
      Caption = 'Memo goes here'
      TabOrder = 0
    end
    object lb_Iherited: TListBox
      Left = 1
      Top = 152
      Width = 527
      Height = 122
      Align = alBottom
      ItemHeight = 13
      TabOrder = 1
    end
  end
end
