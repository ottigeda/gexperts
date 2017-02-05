object foCaseOf: TfoCaseOf
  Left = 416
  Top = 350
  BorderStyle = bsDialog
  Caption = 'Case of'
  ClientHeight = 363
  ClientWidth = 417
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
  object JvSpeedButton1: TJvSpeedButton
    Left = 156
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
    OnClick = JvSpeedButton1Click
  end
  object JvSpeedButton2: TJvSpeedButton
    Left = 280
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
    Width = 291
    Height = 309
    Caption = 'Panel1'
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 6
      Top = 6
      Width = 279
      Height = 61
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 18
        Width = 38
        Height = 13
        Caption = 'Variable'
      end
      object Edit1: TEdit
        Left = 54
        Top = 14
        Width = 217
        Height = 21
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 54
        Top = 40
        Width = 183
        Height = 17
        Caption = 'Include a ELSE statement'
        TabOrder = 1
      end
    end
    object GroupBox2: TGroupBox
      Left = 6
      Top = 68
      Width = 279
      Height = 235
      TabOrder = 1
      object JvSpeedButton3: TJvSpeedButton
        Left = 4
        Top = 12
        Width = 63
        Height = 25
        Caption = '&Insert'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton3Click
      end
      object JvSpeedButton4: TJvSpeedButton
        Left = 71
        Top = 12
        Width = 63
        Height = 25
        Caption = '&Delete'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton4Click
      end
      object JvSpeedButton5: TJvSpeedButton
        Left = 139
        Top = 12
        Width = 63
        Height = 25
        Caption = '&Load'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton5Click
      end
      object JvSpeedButton6: TJvSpeedButton
        Left = 206
        Top = 12
        Width = 63
        Height = 25
        Caption = '&Save'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton6Click
      end
      object JvListView1: TJvListView
        Left = 4
        Top = 42
        Width = 267
        Height = 187
        Hint = 'Right-click to change the code implementation'
        Columns = <
          item
            Caption = 'Name'
            Width = 150
          end
          item
            Caption = 'Code'
            Width = 90
          end>
        FlatScrollBars = True
        GridLines = True
        HotTrack = True
        MultiSelect = True
        RowSelect = True
        PopupMenu = PopupMenu1
        TabOrder = 0
        ViewStyle = vsReport
        ColumnsOrder = '0=150,1=90'
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
    Left = 182
    Top = 228
    object SingleLineofCode1: TMenuItem
      Caption = 'Single Line of Code'
      OnClick = SingleLineofCode1Click
    end
    object MultipleLinesofCode1: TMenuItem
      Tag = 1
      Caption = 'Multiple Lines of Code'
      OnClick = SingleLineofCode1Click
    end
  end
  object ActionList1: TActionList
    Left = 46
    Top = 140
    object Insert: TAction
      Caption = 'Insert'
      ShortCut = 45
      OnExecute = JvSpeedButton3Click
    end
    object Delete: TAction
      Caption = 'Delete'
      ShortCut = 46
      OnExecute = JvSpeedButton4Click
    end
  end
  object OpenDialog1: TJvOpenDialog
    Height = 0
    Width = 0
    Left = 200
    Top = 184
  end
  object SaveDialog1: TJvSaveDialog
    Height = 0
    Width = 0
    Left = 208
    Top = 192
  end
end
