object fmBaseForm: TfmBaseForm
  Left = 300
  Top = 220
  AutoScroll = False
  Caption = 'GExperts'
  ClientHeight = 541
  ClientWidth = 653
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  TextHeight = 13
  object TheApplicationEvents: TApplicationEvents
    OnShowHint = TheApplicationEventsShowHint
    Left = 72
    Top = 88
  end
end
