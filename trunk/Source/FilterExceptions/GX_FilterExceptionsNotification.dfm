inherited fmExceptionNotification: TfmExceptionNotification
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'GExperts Debugger Exception Notification'
  ClientHeight = 121
  ClientWidth = 585
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object l_Message: TLabel
    Left = 8
    Top = 8
    Width = 569
    Height = 73
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Message goes here'
    WordWrap = True
  end
  object b_Break: TButton
    Left = 424
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
    Left = 504
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
    Action = act_Filter
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object b_AllThisSession: TButton
    Left = 88
    Top = 88
    Width = 169
    Height = 25
    Action = act_IgnoreAll
    TabOrder = 3
  end
  object TheActionList: TActionList
    Left = 288
    Top = 32
    object act_Filter: TAction
      Caption = '&Filter ...'
      Hint = 'Create a new filter based on this exception'
      OnExecute = act_FilterExecute
    end
    object act_CopyToClipboard: TAction
      Caption = 'Copy to Clipboard'
      ShortCut = 16451
      OnExecute = act_CopyToClipboardExecute
    end
    object act_IgnoreAll: TAction
      Caption = 'Ignore &All this Session'
      Hint = 'Ignore all exceptions for the rest of this debugger session.'
      OnExecute = act_IgnoreAllExecute
    end
  end
end
