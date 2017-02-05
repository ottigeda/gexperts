object foAddCodeLibrary: TfoAddCodeLibrary
  Left = 727
  Top = 313
  BorderStyle = bsDialog
  Caption = 'Add Code To Library'
  ClientHeight = 365
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
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
    Left = 150
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
    Left = 286
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
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 241
      Height = 78
      Caption = 
        'Use this expert to add some lines of code to the '#13#10'code library.' +
        ' The selected code of your unit, if'#13#10'any, has been extracted and' +
        ' you can now edit it.'#13#10#13#10'You also have to choose the area in wic' +
        'h the code'#13#10'will be stored.'
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 86
      Width = 275
      Height = 69
      TabOrder = 0
      object Label2: TLabel
        Left = 10
        Top = 18
        Width = 22
        Height = 13
        Caption = 'Area'
      end
      object Label3: TLabel
        Left = 8
        Top = 44
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object ComboBox1: TComboBox
        Left = 42
        Top = 14
        Width = 225
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = ComboBox1Change
      end
      object Edit1: TEdit
        Left = 42
        Top = 40
        Width = 225
        Height = 21
        TabOrder = 1
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 156
      Width = 275
      Height = 147
      TabOrder = 1
      object Memo1: TMemo
        Left = 6
        Top = 12
        Width = 265
        Height = 129
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
