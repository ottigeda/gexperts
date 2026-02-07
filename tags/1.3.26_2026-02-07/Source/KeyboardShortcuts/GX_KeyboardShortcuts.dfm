inherited fmGxKeyboardShortcuts: TfmGxKeyboardShortcuts
  Left = 381
  Top = 212
  Width = 605
  Height = 507
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Actions with Keyboard Shortcuts'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 14
  object sg_Actions: TStringGrid
    Left = 0
    Top = 0
    Width = 304
    Height = 201
    Align = alClient
    ColCount = 4
    DefaultColWidth = 50
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    OnDrawCell = sg_ActionsDrawCell
    OnMouseUp = sg_ActionsMouseUp
    OnMouseWheelDown = sg_ActionsMouseWheelDown
    OnMouseWheelUp = sg_ActionsMouseWheelUp
  end
  object TheActionList: TActionList
    Left = 320
    Top = 280
    object act_Close: TAction
      Caption = 'Close'
      ShortCut = 27
      OnExecute = act_CloseExecute
    end
  end
end
