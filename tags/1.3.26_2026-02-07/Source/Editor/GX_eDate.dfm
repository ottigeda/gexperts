inherited fmDateFormat: TfmDateFormat
  Left = 606
  Top = 312
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Insert Date/Time Configuration'
  ClientHeight = 90
  ClientWidth = 226
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblFormat: TLabel
    Left = 8
    Top = 8
    Width = 82
    Height = 13
    Caption = 'Date/time &format'
    FocusControl = cbFormat
  end
  object cbFormat: TComboBox
    Left = 8
    Top = 24
    Width = 209
    Height = 21
    DropDownCount = 18
    TabOrder = 0
    Text = 'cbFormat'
    Items.Strings = (
      'dddd, mmmm d, yyyy'
      'dddd, mmmm d'
      'mmmm d'
      'mmmm d, yyyy'
      'dd/mm/yy'
      'mm/dd/yy'
      'dd-mm-yy'
      'mm-dd-yy'
      'dddd, mmmm d, yyyy  h:mm am/pm'
      'dddd, mmmm d h:mm am/pm'
      'mmmm d h:mm am/pm'
      'mmmm d, yyyy h:mm am/pm'
      'dd/mm/yy h:mm am/pm'
      'mm/dd/yy h:mm am/pm'
      'dd-mm-yy h:mm am/pm'
      'mm-dd-yy h:mm am/pm'
      'dddd, mmmm d, yyyy h:mm'
      'dddd, mmmm d h:mm'
      'mmmm d h:mm'
      'mmmm d, yyyy h:mm'
      'dd/mm/yy h:mm'
      'mm/dd/yy h:mm'
      'dd-mm-yy h:mm'
      'mm-dd-yy h:mm')
  end
  object btnOK: TButton
    Left = 64
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 144
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
