inherited fmFavFolderProperties: TfmFavFolderProperties
  Caption = 'Folder Properties'
  PixelsPerInch = 96
  ClientHeight = 135
  ClientWidth = 295
  ParentFont = True
  TextHeight = 15
  inherited gbxNewFolder: TGroupBox
    Height = 88
    Caption = ' Folder Properties '
    inherited lblFolderName: TLabel
      Left = 15
      Top = 23
      Width = 66
      Height = 15
    end
    inherited lblFolderType: TLabel
      Left = 22
      Top = 55
      Width = 59
      Height = 15
    end
    inherited edtFolderName: TEdit
      Height = 23
    end
    inherited cbxFolderType: TComboBox
      Top = 51
    end
  end
  inherited btnCancel: TButton
    Left = 212
    Top = 102
  end
  inherited btnOK: TButton
    Left = 132
    Top = 102
  end
end
