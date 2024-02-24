inherited fmAboutExperimental: TfmAboutExperimental
  Caption = 'About GExperts - Experimental'
  ClientHeight = 330
  PixelsPerInch = 96
  TextHeight = 14
  inherited lblWebPage: TLabel
    Top = 136
  end
  inherited lblProjectLeader: TLabel
    Left = 272
    Top = 160
  end
  inherited lblContributors: TLabel
    Left = 256
    Top = 192
  end
  inherited lblErik: TLabel
    Top = 160
    OnClick = nil
  end
  inherited lblWebSite: TLabel
    Top = 135
  end
  inherited lblPreRelease1: TLabel
    Caption = 'Experimental'
    Font.Color = clRed
    ParentFont = False
    Visible = True
  end
  inherited lblPreRelease2: TLabel
    Caption = 'Experimental'
    Font.Color = clRed
    ParentFont = False
    Visible = True
  end
  object l_CreatedBy: TLabel [9]
    Left = 224
    Top = 64
    Width = 385
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Experimental version created by Thomas M'#252'ller'
  end
  object l_DummzeuchDe: TLabel [10]
    Left = 224
    Top = 88
    Width = 385
    Height = 14
    Alignment = taCenter
    AutoSize = False
    Caption = 'https://gexperts.dummzeuch.de'
  end
  object l_Formatter: TLabel [11]
    Left = 224
    Top = 112
    Width = 385
    Height = 14
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    Caption = 'Code formatter based on DelForExp by Egbert van Nes'
  end
  inherited mmoBuildDetails: TMemo
    Left = 240
    Top = 256
    Width = 89
    Height = 16
    Lines.Strings = (
      'invisible')
  end
  inherited btnClose: TButton
    Left = 488
    Top = 296
  end
  inherited pnlLogo: TPanel
    Top = 26
    inherited imgLogo: TImage
      Left = 2
      Top = 0
    end
  end
  inherited btnEmail: TButton
    Left = 240
    Top = 296
    Width = 241
  end
  inherited mmoContributors: TMemo
    Left = 368
    Top = 192
    Width = 225
    Height = 97
  end
  inherited tim_Scroll: TTimer
    Left = 104
  end
end
