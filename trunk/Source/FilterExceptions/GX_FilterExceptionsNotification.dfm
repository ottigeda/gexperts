inherited fmExceptionNotification: TfmExceptionNotification
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'GExperts Debugger Exception Notification'
  ClientHeight = 121
  ClientWidth = 584
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object l_Message: TLabel
    Left = 8
    Top = 8
    Width = 568
    Height = 73
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Message goes here'
    WordWrap = True
  end
  object b_Break: TButton
    Left = 423
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Break'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object b_Continue: TButton
    Left = 503
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Continue'
    ModalResult = 5
    TabOrder = 1
  end
  object b_Ignore: TButton
    Left = 8
    Top = 88
    Width = 75
    Height = 25
    Action = act_Ignore
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object TheActionList: TActionList
    Left = 288
    Top = 64
    object act_Ignore: TAction
      Caption = '&Filter ...'
    end
    object act_CopyToClipboard: TAction
      Caption = 'Copy to Clipboard'
      ShortCut = 16451
    end
  end
end
