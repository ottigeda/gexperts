object foCodeToHtml: TfoCodeToHtml
  Left = 147
  Top = 172
  BorderStyle = bsDialog
  Caption = 'Code To Html'
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
    ModalResult = 1
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
      Left = 10
      Top = 10
      Width = 269
      Height = 117
      Caption = 
        'Use this expert to transform your code into html. This is '#13#10'very' +
        ' usefull to publish sources on your website, or in a '#13#10'forum.'#13#10#13 +
        #10'Colors used for the code are the colors your are currently '#13#10'us' +
        'ing.'#13#10#13#10'"All code" will save the whole code of the current unit.' +
        #13#10'"Selected code" will save the selected code only.'
    end
    object RadioGroup1: TRadioGroup
      Left = 10
      Top = 138
      Width = 265
      Height = 57
      ItemIndex = 0
      Items.Strings = (
        'All code'
        'Selected code')
      TabOrder = 0
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.html'
    Filter = 'Html Files (*.htm;*.html)|*.htm;*.html|All Files (*.*)|*.*'
    Left = 58
    Top = 44
  end
end
