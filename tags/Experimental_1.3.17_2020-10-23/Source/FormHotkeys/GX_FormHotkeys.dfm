object f_FormHotkeys: Tf_FormHotkeys
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Form Hotkeys'
  ClientHeight = 329
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    633
    329)
  PixelsPerInch = 96
  TextHeight = 13
  object l_HotAndAcceleratorKeys: TLabel
    Left = 8
    Top = 8
    Width = 126
    Height = 13
    Caption = '&Hot- and Accelerator Keys'
  end
  object TheGrid: TStringGrid
    Left = 8
    Top = 24
    Width = 617
    Height = 297
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 6
    DefaultColWidth = 50
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    OnDrawCell = TheGridDrawCell
  end
  object b_somebutton: TButton
    Left = 336
    Top = 0
    Width = 75
    Height = 25
    Action = act_
    TabOrder = 1
  end
  object TheActionList: TActionList
    Left = 312
    Top = 176
    object act_: TAction
      Caption = '&Action1'
      ShortCut = 16450
      SecondaryShortCuts.Strings = (
        'Alt+Ctrl+X'
        'Shift+Alt+Ctrl+B')
    end
  end
  object pm_: TPopupMenu
    Left = 408
    Top = 136
    object SomeMenuItem1: TMenuItem
      Action = act_
    end
  end
end
