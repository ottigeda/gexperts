object foSpeedButton: TfoSpeedButton
  Left = 389
  Top = 310
  BorderStyle = bsDialog
  Caption = 'SpeedButton Editor'
  ClientHeight = 299
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object JvSpeedButton1: TJvSpeedButton
    Left = 34
    Top = 264
    Width = 77
    Height = 25
    Caption = '&Ok'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 1
  end
  object JvSpeedButton2: TJvSpeedButton
    Left = 158
    Top = 264
    Width = 77
    Height = 25
    Caption = '&Cancel'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 2
  end
  object PageControl1: TPageControl
    Left = 4
    Top = 6
    Width = 251
    Height = 247
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Options'
      object GroupBox2: TGroupBox
        Left = 2
        Top = 0
        Width = 239
        Height = 121
        Caption = '[ General Properties ]'
        TabOrder = 0
        object Label3: TLabel
          Left = 10
          Top = 70
          Width = 55
          Height = 13
          Caption = 'GroupIndex'
        end
        object Label4: TLabel
          Left = 20
          Top = 44
          Width = 36
          Height = 13
          Caption = 'Caption'
        end
        object Label6: TLabel
          Left = 20
          Top = 96
          Width = 32
          Height = 13
          Caption = 'Margin'
        end
        object Label7: TLabel
          Left = 132
          Top = 70
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object Label8: TLabel
          Left = 130
          Top = 96
          Width = 31
          Height = 13
          Caption = 'Height'
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 18
          Width = 67
          Height = 17
          Caption = 'Flat'
          TabOrder = 0
          OnClick = CheckBox1Click
        end
        object SpinEdit3: TSpinEdit
          Left = 74
          Top = 66
          Width = 45
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = CheckBox1Click
        end
        object CheckBox2: TCheckBox
          Left = 78
          Top = 18
          Width = 69
          Height = 17
          Caption = 'Enabled'
          TabOrder = 1
          OnClick = CheckBox1Click
        end
        object Edit1: TEdit
          Left = 74
          Top = 40
          Width = 155
          Height = 21
          TabOrder = 3
          OnChange = CheckBox1Click
        end
        object SpinEdit4: TSpinEdit
          Left = 74
          Top = 92
          Width = 45
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = CheckBox1Click
        end
        object CheckBox3: TCheckBox
          Left = 152
          Top = 18
          Width = 81
          Height = 17
          Caption = 'Transparent'
          TabOrder = 2
          OnClick = CheckBox1Click
        end
        object SpinEdit5: TSpinEdit
          Left = 182
          Top = 92
          Width = 45
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 7
          Value = 0
          OnChange = CheckBox1Click
        end
        object SpinEdit6: TSpinEdit
          Left = 182
          Top = 66
          Width = 45
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 6
          Value = 0
          OnChange = CheckBox1Click
        end
      end
      object GroupBox3: TGroupBox
        Left = 2
        Top = 122
        Width = 239
        Height = 95
        Caption = '[ Glyph Options ]'
        TabOrder = 1
        object Label1: TLabel
          Left = 16
          Top = 20
          Width = 39
          Height = 13
          Caption = 'Spacing'
        end
        object Label2: TLabel
          Left = 10
          Top = 44
          Width = 54
          Height = 13
          Caption = 'NumGlyphs'
        end
        object Label5: TLabel
          Left = 20
          Top = 72
          Width = 32
          Height = 13
          Caption = 'Layout'
        end
        object SpinEdit1: TSpinEdit
          Left = 74
          Top = 16
          Width = 155
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = CheckBox1Click
        end
        object SpinEdit2: TSpinEdit
          Left = 74
          Top = 40
          Width = 155
          Height = 22
          MaxValue = 4
          MinValue = 1
          TabOrder = 1
          Value = 1
          OnChange = CheckBox1Click
        end
        object ComboBox1: TComboBox
          Left = 74
          Top = 66
          Width = 155
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = CheckBox1Click
          Items.Strings = (
            'blGlyphLeft'
            'blGlyphRight'
            'blGlyphTop'
            'blGlyphBottom')
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Glyph'
      ImageIndex = 1
      object ScrollBox1: TScrollBox
        Left = 1
        Top = 0
        Width = 239
        Height = 215
        TabOrder = 0
        object SpeedButton1: TSpeedButton
          Left = 0
          Top = 0
          Width = 23
          Height = 22
          Hint = 'Empty Image'
          Flat = True
          OnClick = GlyphClick
        end
      end
    end
  end
  object GroupBox1: TGroupBox
    Left = 260
    Top = 6
    Width = 239
    Height = 247
    Caption = '[ Preview ]'
    TabOrder = 1
    object PreviewButton: TJvSpeedButton
      Left = 81
      Top = 113
      Width = 77
      Height = 25
      Caption = '&Ok'
      Flat = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      NumGlyphs = 2
    end
  end
  object JvSearchFile1: TJvSearchFiles
    OnFindFile = JvSearchFile1Found
    Left = 360
    Top = 20
  end
end
