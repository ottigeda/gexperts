inherited fmDateFormat: TfmDateFormat
  Left = 285
  Top = 201
  BorderStyle = bsDialog
  Caption = 'Insert Date/Time Configuration'
  ClientHeight = 74
  ClientWidth = 337
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object lblFormat: TLabel
    Left = 20
    Top = 13
    Width = 92
    Height = 15
    Alignment = taRightJustify
    Caption = 'Date/time &format'
    FocusControl = cbFormat
  end
  object cbFormat: TComboBox
    Left = 120
    Top = 10
    Width = 209
    Height = 23
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
    Left = 168
    Top = 41
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 252
    Top = 41
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
