object foToDoAdd: TfoToDoAdd
  Left = 545
  Top = 825
  BorderStyle = bsDialog
  Caption = 'Add an item'
  ClientHeight = 112
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object JvSpeedButton2: TJvSpeedButton
    Left = 200
    Top = 80
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
  object JvSpeedButton1: TJvSpeedButton
    Left = 84
    Top = 80
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
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 2
    Width = 353
    Height = 69
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 20
      Height = 13
      Caption = 'Line'
    end
    object Label2: TLabel
      Left = 8
      Top = 42
      Width = 21
      Height = 13
      Caption = 'Text'
    end
    object Label3: TLabel
      Left = 168
      Top = 16
      Width = 30
      Height = 13
      Caption = 'Status'
    end
    object SpinEdit1: TSpinEdit
      Left = 44
      Top = 12
      Width = 105
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object Edit1: TEdit
      Left = 44
      Top = 38
      Width = 301
      Height = 21
      TabOrder = 0
      OnKeyPress = Edit1KeyPress
    end
    object ComboBox1: TComboBox
      Left = 204
      Top = 12
      Width = 141
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'To Do'
        'Finished')
    end
  end
end
