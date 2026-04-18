object fmCodeOptions: TfmCodeOptions
  Left = 442
  Top = 167
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Code Librarian Options'
  ClientHeight = 393
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 176
    Top = 360
    Width = 73
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 256
    Top = 360
    Width = 73
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object pgeCodeOpt: TPageControl
    Left = 8
    Top = 8
    Width = 321
    Height = 337
    ActivePage = tabFonts
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabFonts: TTabSheet
      Caption = 'Display'
      object grp_Fonts: TGroupBox
        Left = 8
        Top = 176
        Width = 297
        Height = 121
        Caption = 'Fonts'
        TabOrder = 1
        object lblTreeView: TLabel
          Left = 8
          Top = 24
          Width = 51
          Height = 14
          Caption = 'Treeview'
        end
        object lblEditor: TLabel
          Left = 8
          Top = 72
          Width = 32
          Height = 14
          Caption = 'Editor'
        end
        object lblSize: TLabel
          Left = 232
          Top = 21
          Width = 41
          Height = 17
          AutoSize = False
          Caption = 'Size'
        end
        object fcTreeview: TComboBox
          Left = 8
          Top = 40
          Width = 205
          Height = 22
          Style = csDropDownList
          DropDownCount = 18
          ItemHeight = 14
          TabOrder = 0
        end
        object fcEditor: TComboBox
          Left = 8
          Top = 88
          Width = 205
          Height = 22
          Style = csDropDownList
          DropDownCount = 18
          ItemHeight = 14
          TabOrder = 3
        end
        object udTreeview: TUpDown
          Left = 273
          Top = 40
          Width = 16
          Height = 22
          Associate = eTreeview
          Min = 4
          Max = 24
          Position = 8
          TabOrder = 2
        end
        object udEditor: TUpDown
          Left = 273
          Top = 88
          Width = 16
          Height = 22
          Associate = eEditor
          Min = 4
          Max = 24
          Position = 8
          TabOrder = 4
        end
        object eTreeview: TEdit
          Left = 232
          Top = 40
          Width = 41
          Height = 22
          TabOrder = 1
          Text = '8'
          OnKeyPress = eNumericKeyPress
        end
        object eEditor: TEdit
          Left = 232
          Top = 88
          Width = 41
          Height = 22
          TabOrder = 5
          Text = '8'
        end
      end
      object grp_Layout: TGroupBox
        Left = 8
        Top = 8
        Width = 297
        Height = 161
        Caption = 'Layout'
        TabOrder = 0
        object pnlSideSide: TPanel
          Left = 30
          Top = 64
          Width = 105
          Height = 89
          BevelOuter = bvLowered
          Color = clWindow
          TabOrder = 2
          object shpLeft: TShape
            Left = 8
            Top = 8
            Width = 41
            Height = 73
          end
          object shpRight: TShape
            Left = 56
            Top = 8
            Width = 41
            Height = 73
          end
        end
        object pnlTopBottom: TPanel
          Left = 168
          Top = 64
          Width = 105
          Height = 89
          BevelOuter = bvLowered
          Color = clWindow
          TabOrder = 3
          object shpTop: TShape
            Left = 8
            Top = 8
            Width = 89
            Height = 33
          end
          object shpBottom: TShape
            Left = 8
            Top = 48
            Width = 89
            Height = 33
          end
        end
        object rbTop: TRadioButton
          Left = 152
          Top = 32
          Width = 137
          Height = 17
          Caption = 'Top to bottom'
          TabOrder = 1
        end
        object rbSide: TRadioButton
          Left = 14
          Top = 32
          Width = 137
          Height = 17
          Caption = 'Side by side'
          TabOrder = 0
        end
      end
    end
    object tabPaths: TTabSheet
      Caption = 'Storage'
      object lblStoragePath: TLabel
        Left = 8
        Top = 16
        Width = 173
        Height = 14
        Caption = 'Code Librarian Storage Location'
      end
      object sbBrowse: TButton
        Left = 284
        Top = 32
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = sbBrowseClick
      end
      object edPath: TEdit
        Left = 8
        Top = 32
        Width = 275
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
  end
end
