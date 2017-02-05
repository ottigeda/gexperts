object foRecord: TfoRecord
  Left = 680
  Top = 203
  BorderStyle = bsDialog
  Caption = 'Record Editor'
  ClientHeight = 364
  ClientWidth = 413
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
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 6
      Top = 6
      Width = 279
      Height = 47
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 18
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
        ParentCtl3D = False
        TabOrder = 0
        Text = 'TYourRecord'
      end
    end
    object GroupBox2: TGroupBox
      Left = 6
      Top = 56
      Width = 279
      Height = 245
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
        Height = 195
        Hint = 'Right-click to change the type'
        Columns = <
          item
            Caption = 'Name'
          end
          item
            Caption = 'Type'
            Width = 190
          end>
        FlatScrollBars = True
        GridLines = True
        HotTrack = True
        MultiSelect = True
        RowSelect = True
        PopupMenu = PopupMenu1
        TabOrder = 0
        ViewStyle = vsReport
        ColumnsOrder = '0=50,1=190'
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
    OnPopup = PopupMenu1Popup
    Left = 222
    Top = 182
    object AnsiString1: TMenuItem
      Caption = 'AnsiString'
      OnClick = WideString1Click
    end
    object Boolean1: TMenuItem
      Caption = 'Boolean'
      OnClick = WideString1Click
    end
    object Byte1: TMenuItem
      Caption = 'Byte'
      OnClick = WideString1Click
    end
    object Char1: TMenuItem
      Caption = 'Char'
      OnClick = WideString1Click
    end
    object Custom1: TMenuItem
      Caption = 'Custom...'
      OnClick = Custom1Click
    end
    object File1: TMenuItem
      Caption = 'File'
      OnClick = WideString1Click
    end
    object Int641: TMenuItem
      Caption = 'Int64'
      OnClick = WideString1Click
    end
    object Integer1: TMenuItem
      Caption = 'Integer'
      OnClick = WideString1Click
    end
    object LongInt1: TMenuItem
      Caption = 'LongInt'
      OnClick = WideString1Click
    end
    object LonwWord1: TMenuItem
      Caption = 'LongWord'
      OnClick = WideString1Click
    end
    object ShortInt1: TMenuItem
      Caption = 'ShortInt'
      OnClick = WideString1Click
    end
    object SmallInt1: TMenuItem
      Caption = 'SmallInt'
      OnClick = WideString1Click
    end
    object String1: TMenuItem
      Caption = 'String'
      OnClick = WideString1Click
    end
    object Word1: TMenuItem
      Caption = 'Word'
      OnClick = WideString1Click
    end
    object WideString1: TMenuItem
      Caption = 'WideString'
      OnClick = WideString1Click
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
    DefaultExt = 'rec'
    Filter = 'Record Files v2.0 (*.urc)|*.urc|Record Files v1.0 (*.rec)|*.rec'
    Height = 0
    Width = 0
    Left = 46
    Top = 56
  end
  object SaveDialog1: TJvSaveDialog
    DefaultExt = 'rec'
    Filter = 'Record Files v2.0 (*.urc)|*.urc'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Height = 0
    Width = 0
    Left = 46
    Top = 86
  end
end
