object fmGxDockForm: TfmGxDockForm
  Left = 381
  Top = 212
  Caption = 'GExperts Dock Window'
  ClientHeight = 582
  ClientWidth = 409
  Color = clBtnFace
  UseDockManager = True
  DockSite = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnDockDrop = FormDockDrop
  PixelsPerInch = 96
  TextHeight = 14
  object l_Warning: TLabel
    AlignWithMargins = True
    Left = 16
    Top = 3
    Width = 377
    Height = 576
    Margins.Left = 16
    Margins.Right = 16
    Align = alClient
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Warning! This feature is still very experimental. If you dock an' +
      'y form here, you might not be able to undock it again.'#13#10'If that ' +
      'happens, restore one of your saved desktops or restart the IDE.'
    Layout = tlCenter
    WordWrap = True
  end
end
