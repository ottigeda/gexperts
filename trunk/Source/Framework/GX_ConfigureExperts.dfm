object frConfigureExperts: TfrConfigureExperts
  Left = 0
  Top = 0
  Width = 640
  Height = 223
  TabOrder = 0
  OnMouseWheelDown = FrameMouseWheelDown
  OnMouseWheelUp = FrameMouseWheelUp
  OnResize = FrameResize
  object pnlExpertsFilter: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblFilter: TLabel
      Left = 19
      Top = 14
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = '&Filter'
      FocusControl = edtFilter
    end
    object edtFilter: TEdit
      Left = 50
      Top = 10
      Width = 111
      Height = 21
      TabOrder = 0
      OnChange = edtFilterChange
      OnKeyDown = edtFilterKeyDown
    end
    object btnEnableAll: TButton
      Left = 190
      Top = 7
      Width = 102
      Height = 25
      Caption = 'Enable All'
      TabOrder = 2
      OnClick = btnEnableAllClick
    end
    object btnDisableAll: TButton
      Left = 299
      Top = 7
      Width = 102
      Height = 25
      Caption = 'Disable All'
      TabOrder = 3
      OnClick = btnDisableAllClick
    end
    object btnClear: TButton
      Left = 161
      Top = 9
      Width = 21
      Height = 21
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnClearClick
    end
    object btnClearAll: TButton
      Left = 406
      Top = 7
      Width = 80
      Height = 25
      Hint = 'Clears all keyboard shortcuts'
      Caption = 'Clear All'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnClearAllClick
    end
    object btnSetAllDefault: TButton
      Left = 488
      Top = 7
      Width = 118
      Height = 25
      Hint = 'Sets all keyboard shortcuts to the default'
      Caption = 'Load Defaults'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnSetAllDefaultClick
    end
  end
  object sbxExperts: TScrollBox
    Left = 0
    Top = 41
    Width = 607
    Height = 182
    VertScrollBar.Increment = 40
    VertScrollBar.Range = 920
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    TabOrder = 1
    object pnlExpertLayout: TPanel
      Left = 0
      Top = 0
      Width = 585
      Height = 40
      TabOrder = 0
      object imgExpert: TImage
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        Center = True
        Proportional = True
        Transparent = True
      end
      object chkExpert: TCheckBox
        Left = 40
        Top = 12
        Width = 222
        Height = 17
        Caption = 'Set Component Properties'
        TabOrder = 0
      end
      object edtExpert: THotKey
        Left = 267
        Top = 10
        Width = 126
        Height = 22
        HotKey = 32833
        TabOrder = 1
      end
      object btnExpert: TButton
        Left = 479
        Top = 8
        Width = 102
        Height = 25
        Caption = 'Configure...'
        TabOrder = 3
      end
      object btnDefault: TButton
        Left = 403
        Top = 8
        Width = 72
        Height = 25
        Caption = 'Default'
        TabOrder = 2
      end
    end
  end
end
