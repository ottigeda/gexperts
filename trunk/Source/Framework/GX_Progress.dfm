object fmProgress: TfmProgress
  Left = 357
  Top = 310
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 37
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object Progress: TProgressBar
    Left = 8
    Top = 8
    Width = 257
    Height = 25
    Min = 0
    Max = 100
    TabOrder = 0
  end
end
