object foSeparator: TfoSeparator
  Left = 259
  Top = 272
  BorderStyle = bsDialog
  Caption = 'Separators editor'
  ClientHeight = 365
  ClientWidth = 556
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
  object JvSpeedButton5: TJvSpeedButton
    Left = 230
    Top = 328
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
    OnClick = JvSpeedButton5Click
  end
  object JvSpeedButton6: TJvSpeedButton
    Left = 366
    Top = 328
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
    Left = 118
    Top = 8
    Width = 433
    Height = 307
    TabOrder = 0
    object JvGroupBox1: TJvGroupBox
      Left = 8
      Top = 8
      Width = 419
      Height = 287
      TabOrder = 0
      object JvSpeedButton1: TJvSpeedButton
        Left = 8
        Top = 20
        Width = 77
        Height = 25
        Caption = '&Add...'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton1Click
      end
      object JvSpeedButton2: TJvSpeedButton
        Left = 115
        Top = 20
        Width = 77
        Height = 25
        Caption = '&Remove'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton2Click
      end
      object JvSpeedButton3: TJvSpeedButton
        Left = 223
        Top = 20
        Width = 77
        Height = 25
        Caption = '&Move Up'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton3Click
      end
      object JvSpeedButton4: TJvSpeedButton
        Left = 330
        Top = 20
        Width = 77
        Height = 25
        Caption = '&Move Down'
        Flat = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        OnClick = JvSpeedButton4Click
      end
      object JvListbox1: TJvListBox
        Left = 6
        Top = 52
        Width = 403
        Height = 227
        ItemHeight = 13
        Background.FillMode = bfmTile
        Background.Visible = False
        MultiSelect = True
        TabOrder = 0
      end
    end
  end
  object OpenDialog1: TJvOpenDialog
    Filter = 
      'Unit Files (*.pas)|*.pas|Project Files (*.dpr)|*.dpr|Project and' +
      ' Units |*.dpr;*.pas|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Height = 0
    Width = 0
    Left = 24
    Top = 66
  end
end
