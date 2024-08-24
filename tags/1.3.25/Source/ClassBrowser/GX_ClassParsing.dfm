object fmClassParsing: TfmClassParsing
  Left = 299
  Top = 253
  BorderStyle = bsDialog
  Caption = 'Parsing classes...'
  ClientHeight = 97
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object lblParsing: TLabel
    Left = 91
    Top = 16
    Width = 406
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Parsing classes, please wait...'
    WordWrap = True
  end
  object aniFlashlight: TAnimate
    Left = 5
    Top = 8
    Width = 80
    Height = 50
    CommonAVI = aviFindFolder
    StopFrame = 29
  end
  object Progress: TProgressBar
    Left = 24
    Top = 64
    Width = 457
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
end
