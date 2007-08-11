inherited fmAboutExperimental: TfmAboutExperimental
  Caption = 'About GExperts - Experimental'
  ClientHeight = 305
  ExplicitWidth = 572
  ExplicitHeight = 330
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblWebPage: TLabel
    Top = 111
    ExplicitTop = 111
  end
  inherited lblProjectLeader: TLabel
    Top = 131
    ExplicitTop = 131
  end
  inherited lblContributors: TLabel
    Top = 153
    ExplicitTop = 153
  end
  inherited lblErik: TLabel
    Top = 131
    ExplicitTop = 131
  end
  inherited lblWebSite: TLabel
    Top = 111
    ExplicitTop = 111
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
    Left = 216
    Top = 64
    Width = 345
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AutoSize = False
    Caption = 'Experimental version created by Thomas M'#252'ller'
  end
  object l_DummzeuchDe: TLabel [10]
    Left = 216
    Top = 79
    Width = 345
    Height = 13
    Cursor = crHandPoint
    Margins.Bottom = 0
    Alignment = taCenter
    AutoSize = False
    Caption = 'http://www.dummzeuch.de'
    OnClick = lblWebPageClick
  end
  inherited pnlContributors: TPanel
    Top = 153
    ExplicitTop = 153
    inherited lbxContributors: TListBox
      ExplicitTop = -8
    end
  end
  inherited btnClose: TButton
    Top = 270
    ExplicitTop = 270
  end
  inherited pnlLogo: TPanel
    Top = 26
    ExplicitTop = 26
    inherited imgLogo: TImage
      Left = 2
      Top = 0
      ExplicitLeft = 2
      ExplicitTop = 0
    end
  end
  inherited btnEmail: TButton
    Top = 270
    ExplicitTop = 270
  end
end
