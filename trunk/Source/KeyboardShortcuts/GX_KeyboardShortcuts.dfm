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
  OldCreateOrder = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 14
  object sg_Actions: TStringGrid
    Left = 0
    Top = 0
    Width = 589
    Height = 468
    Align = alClient
    ColCount = 4
    DefaultColWidth = 50
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    OnDrawCell = sg_ActionsDrawCell
    OnMouseUp = sg_ActionsMouseUp
    OnMouseWheelDown = sg_ActionsMouseWheelDown
    OnMouseWheelUp = sg_ActionsMouseWheelUp
  end
end
