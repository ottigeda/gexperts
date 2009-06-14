inherited fmAboutExperimental: TfmAboutExperimental
  Caption = 'About GExperts - Experimental'
  ClientHeight = 305
  PixelsPerInch = 96
  TextHeight = 14
  inherited lblWebPage: TLabel
    Top = 111
  end
  inherited lblProjectLeader: TLabel
    Top = 131
  end
  inherited lblContributors: TLabel
    Top = 153
  end
  inherited lblErik: TLabel
    Top = 131
  end
  inherited lblWebSite: TLabel
    Top = 111
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
    Top = 79
    Width = 385
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    Caption = 'http://www.dummzeuch.de'
    OnClick = lblWebPageClick
  end
  inherited btnClose: TButton
    Top = 270
  end
  inherited pnlLogo: TPanel
    Top = 26
    inherited imgLogo: TImage
      Left = 2
      Top = 0
    end
  end
  inherited btnEmail: TButton
    Top = 270
    Visible = False
  end
  inherited mmoContributors: TMemo
    Top = 152
    Height = 105
  end
end
