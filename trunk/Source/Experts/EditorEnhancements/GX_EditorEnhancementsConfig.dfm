inherited fmEditorEnhancementsConfig: TfmEditorEnhancementsConfig
  Caption = 'Code Editor Enhancements Configuration'
  ClientHeight = 370
  ClientWidth = 649
  PixelsPerInch = 96
  TextHeight = 13
  object lblHideNavBar: TLabel
    Left = 256
    Top = 16
    Width = 381
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'You can hide the navigation bar in the Tools -> Options dialog'#13#10 +
      'See Editor Options -> Display -> Show Navigation Toolbar'
    Visible = False
    WordWrap = True
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
      Enabled = False
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
      Enabled = False
      TabOrder = 2
      OnClick = btnConfigureToolBarClick
    end
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
  object chkHideNavbar: TCheckBox
    Left = 256
    Top = 16
    Width = 384
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Hide Navigation Bar (Delphi 10 and 10.1)'
    TabOrder = 2
  end
  object b_OK: TButton
    Left = 488
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object b_Cancel: TButton
    Left = 568
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object b_Help: TButton
    Left = 240
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 5
    OnClick = b_HelpClick
  end
end
