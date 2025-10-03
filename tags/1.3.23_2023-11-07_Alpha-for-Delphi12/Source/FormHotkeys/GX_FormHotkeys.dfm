object f_FormHotkeys: Tf_FormHotkeys
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Form Hotkeys'
  ClientHeight = 329
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object TheGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 784
    Height = 329
    Align = alClient
    ColCount = 7
    DefaultColWidth = 50
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    PopupMenu = pm_Grid
    TabOrder = 0
    OnDrawCell = TheGridDrawCell
  end
  object TheActionList: TActionList
    Left = 312
    Top = 176
    object act_AssignShortcut: TAction
      Caption = '&Assign Shortcut'
      ShortCut = 16449
      OnExecute = act_AssignShortcutExecute
    end
    object act_GotoAction: TAction
      Caption = '&Goto Action'
      ShortCut = 16455
      OnExecute = act_GotoActionExecute
    end
  end
  object pm_Grid: TPopupMenu
    OnPopup = pm_GridPopup
    Left = 408
    Top = 136
    object pm_AssingShortcut: TMenuItem
      Action = act_AssignShortcut
    end
    object mi_GotoAction: TMenuItem
      Action = act_GotoAction
    end
  end
end
