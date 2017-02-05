object foTryExcept: TfoTryExcept
  Left = 360
  Top = 396
  BorderStyle = bsDialog
  Caption = 'Exceptions'
  ClientHeight = 363
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 6
    Top = 8
    Width = 100
    Height = 350
    AutoSize = True
  end
  object OkButton: TJvSpeedButton
    Left = 160
    Top = 330
    Width = 77
    Height = 25
    Caption = '&Ok'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TJvSpeedButton
    Left = 296
    Top = 330
    Width = 77
    Height = 25
    Caption = '&Cancel'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 2
  end
  object Panel1: TPanel
    Left = 114
    Top = 8
    Width = 303
    Height = 303
    TabOrder = 0
    object RadioGroup1: TRadioGroup
      Left = 6
      Top = 8
      Width = 289
      Height = 61
      ItemIndex = 0
      Items.Strings = (
        'Try..Except..End'
        'Try..Finally..End')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object GroupBox1: TGroupBox
      Left = 6
      Top = 74
      Width = 289
      Height = 219
      TabOrder = 1
      object JvListView1: TJvListView
        Left = 5
        Top = 11
        Width = 276
        Height = 200
        Hint = 'Right-click to change the code implementation'
        Checkboxes = True
        Columns = <
          item
            Caption = 'Exception'
            Width = 150
          end
          item
            Caption = 'Code'
            Width = 100
          end>
        FlatScrollBars = True
        GridLines = True
        HotTrack = True
        MultiSelect = True
        RowSelect = True
        PopupMenu = PopupMenu1
        TabOrder = 0
        ViewStyle = vsReport
        ColumnsOrder = '0=150,1=100'
        Groups = <>
        ExtendedColumns = <
          item
          end
          item
          end>
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 46
    Top = 156
    object SingleLine1: TMenuItem
      Caption = 'Single Line of Code'
      OnClick = SingleLine1Click
    end
    object MultipleLineofCode1: TMenuItem
      Tag = 1
      Caption = 'Multiple Line of Code'
      OnClick = SingleLine1Click
    end
  end
end
