inherited fmConfigureIfDef: TfmConfigureIfDef
  Anchors = [akRight, akBottom]
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Insert IFDEF / IFNDEF'
  ClientHeight = 393
  ClientWidth = 385
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pc_IfClasses: TPageControl
    Left = 0
    Top = 0
    Width = 385
    Height = 352
    Align = alClient
    TabOrder = 0
    OnChange = pc_IfClassesChange
  end
  object p_Bottom: TPanel
    Left = 0
    Top = 352
    Width = 385
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      385
      41)
    object b_OK: TButton
      Left = 222
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object b_Cancel: TButton
      Left = 302
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
