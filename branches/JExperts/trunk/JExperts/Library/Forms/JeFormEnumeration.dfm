object foEnumeration: TfoEnumeration
  Left = 378
  Top = 312
  BorderStyle = bsDialog
  Caption = 'Enumeration Editor'
  ClientHeight = 367
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
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
      Height = 93
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 18
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Label2: TLabel
        Left = 8
        Top = 62
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Edit1: TEdit
        Left = 42
        Top = 14
        Width = 229
        Height = 21
        Ctl3D = True
        MaxLength = 40
        ParentCtl3D = False
        TabOrder = 0
        Text = 'TYourEnumeration'
      end
      object Edit2: TEdit
        Left = 42
        Top = 58
        Width = 229
        Height = 21
        Ctl3D = True
        Enabled = False
        ParentCtl3D = False
        TabOrder = 2
        Text = 'TYourSet'
      end
      object CheckBox1: TCheckBox
        Left = 42
        Top = 38
        Width = 223
        Height = 17
        Caption = 'Generate a Set'
        TabOrder = 1
        OnClick = CheckBox1Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 6
      Top = 102
      Width = 279
      Height = 199
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
        Width = 269
        Height = 149
        Columns = <
          item
            Caption = 'Name'
            Width = 245
          end>
        FlatScrollBars = True
        GridLines = True
        HotTrack = True
        MultiSelect = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        ColumnsOrder = '0=245'
        Groups = <>
        ExtendedColumns = <
          item
          end>
      end
    end
  end
  object ActionList1: TActionList
    Left = 44
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
    DefaultExt = 'enm'
    Filter = 'Enum Files v2.0 (*.uen)|*.uen|Enum Files  v1.0 (*.enm)|*.enm'
    Height = 0
    Width = 0
    Left = 42
    Top = 62
  end
  object SaveDialog1: TJvSaveDialog
    DefaultExt = 'enm'
    Filter = 'Enum Files v2.0 (*.uen)|*.uen'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Height = 0
    Width = 0
    Left = 42
    Top = 86
  end
end
