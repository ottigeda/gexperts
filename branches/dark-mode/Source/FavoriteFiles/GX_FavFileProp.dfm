object fmFavFileProp: TfmFavFileProp
  Left = 288
  Top = 192
  BorderStyle = bsDialog
  Caption = 'File Properties'
  ClientHeight = 272
  ClientWidth = 464
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  DesignSize = (
    464
    272)
  PixelsPerInch = 96
  TextHeight = 14
  object pgeProperties: TPageControl
    Left = 8
    Top = 8
    Width = 443
    Height = 225
    ActivePage = tabProperties
    Anchors = [akLeft, akTop, akRight]
    MultiLine = True
    TabIndex = 0
    TabOrder = 0
    TabStop = False
    object tabProperties: TTabSheet
      Caption = 'Properties'
      DesignSize = (
        435
        195)
      object lblFile: TLabel
        Left = 70
        Top = 13
        Width = 18
        Height = 15
        Alignment = taRightJustify
        Caption = '&File'
        FocusControl = edtFilename
      end
      object lblName: TLabel
        Left = 56
        Top = 78
        Width = 32
        Height = 15
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = edtName
      end
      object lblDescription: TLabel
        Left = 28
        Top = 107
        Width = 60
        Height = 15
        Alignment = taRightJustify
        Caption = '&Description'
        FocusControl = edtDescription
      end
      object imgFileIcon: TImage
        Left = 96
        Top = 37
        Width = 32
        Height = 32
        Center = True
        Transparent = True
      end
      object lblIcon: TLabel
        Left = 65
        Top = 45
        Width = 23
        Height = 15
        Alignment = taRightJustify
        Caption = 'Icon'
      end
      object lblExecuteType: TLabel
        Left = 21
        Top = 136
        Width = 67
        Height = 15
        Alignment = taRightJustify
        Caption = 'Execute &type'
        FocusControl = cbxExecuteType
      end
      object lblExecuteUsing: TLabel
        Left = 15
        Top = 165
        Width = 73
        Height = 15
        Alignment = taRightJustify
        Caption = 'Execute &using'
        FocusControl = edtExecuteUsing
      end
      object sbnFile: TButton
        Left = 408
        Top = 7
        Width = 20
        Height = 20
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = sbnFileClick
      end
      object sbnExecute: TButton
        Left = 408
        Top = 161
        Width = 20
        Height = 20
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 6
        OnClick = sbnExecuteClick
      end
      object edtName: TEdit
        Left = 96
        Top = 75
        Width = 312
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object edtDescription: TEdit
        Left = 96
        Top = 104
        Width = 312
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object cbxExecuteType: TComboBox
        Left = 96
        Top = 133
        Width = 145
        Height = 23
        Style = csDropDownList
        TabOrder = 4
        OnClick = cbxExecuteTypeClick
        Items.Strings = (
          'Load In IDE'
          'Shell Execute'
          'Custom'
          'Source Project')
      end
      object edtFilename: TEdit
        Left = 96
        Top = 8
        Width = 312
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = edtFilenameExit
      end
      object edtExecuteUsing: TEdit
        Left = 96
        Top = 162
        Width = 312
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 5
      end
    end
  end
  object btnCancel: TButton
    Left = 376
    Top = 239
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 295
    Top = 239
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
