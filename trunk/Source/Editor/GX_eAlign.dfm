object fmAlign: TfmAlign
  Left = 459
  Top = 281
  ActiveControl = lstTokens
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Align Lines'
  ClientHeight = 256
  ClientWidth = 181
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 135
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    181
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object lblToken: TLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = '&Align on token:'
    FocusControl = lstTokens
  end
  object btnOK: TButton
    Left = 8
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 97
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object lstTokens: TListBox
    Left = 8
    Top = 25
    Width = 163
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    PopupMenu = pmuTokens
    TabOrder = 0
    OnDblClick = lstTokensDblClick
  end
  object cbxMode: TComboBox
    Left = 8
    Top = 198
    Width = 164
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = 'Align at rightmost token'
    Items.Strings = (
      'Align at rightmost token'
      'Align at first token')
  end
  object pmuTokens: TPopupMenu
    Left = 16
    Top = 32
    object mitConfiguration: TMenuItem
      Caption = 'Configuration...'
      OnClick = mitConfigurationClick
    end
  end
end
