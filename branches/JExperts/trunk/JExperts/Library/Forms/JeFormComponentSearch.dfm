object foCompoSearch: TfoCompoSearch
  Left = 516
  Top = 388
  BorderStyle = bsDialog
  Caption = 'Component Finder'
  ClientHeight = 237
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 12
    Width = 49
    Height = 13
    Caption = 'Search for'
  end
  object JvListView1: TJvListView
    Left = 4
    Top = 36
    Width = 193
    Height = 193
    Hint = 'Double click to add to your form'
    Columns = <
      item
        Caption = 'Name'
        Width = 160
      end>
    FlatScrollBars = True
    HotTrack = True
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = JvListView1DblClick
    ColumnsOrder = '0=160'
    Groups = <>
    ExtendedColumns = <
      item
      end>
  end
  object Edit1: TEdit
    Left = 58
    Top = 8
    Width = 139
    Height = 21
    TabOrder = 1
    OnKeyPress = Edit1KeyPress
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 36
    Top = 38
  end
end
