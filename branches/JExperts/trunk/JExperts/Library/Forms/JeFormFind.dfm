object foFindFirst: TfoFindFirst
  Left = 356
  Top = 405
  BorderStyle = bsDialog
  Caption = 'FindFirst - FindNext - FindClose'
  ClientHeight = 368
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 10
    Width = 100
    Height = 350
    AutoSize = True
  end
  object NextButton: TJvSpeedButton
    Left = 228
    Top = 326
    Width = 77
    Height = 25
    Caption = '&Next'
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    OnClick = NextButtonClick
  end
  object CancelButton: TJvSpeedButton
    Left = 338
    Top = 326
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
  object PrevButton: TJvSpeedButton
    Left = 118
    Top = 326
    Width = 77
    Height = 25
    Caption = '&Previous'
    Enabled = False
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    OnClick = PrevButtonClick
  end
  object PageControl1: TPageControl
    Left = 116
    Top = 10
    Width = 303
    Height = 303
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      ParentShowHint = False
      ShowHint = True
      TabVisible = False
      object StaticText7: TStaticText
        Left = 4
        Top = 8
        Width = 275
        Height = 39
        AutoSize = False
        Caption = 
          'This expert will help you building a findfirst, findnext, '#13#10'find' +
          'close module. It can be either in a separated '#13#10'procedure or dir' +
          'ectly in your code.'
        TabOrder = 0
      end
      object RadioGroup1: TRadioGroup
        Left = 4
        Top = 54
        Width = 285
        Height = 75
        ItemIndex = 0
        Items.Strings = (
          'Generate in a procedure'
          'Generate in current code')
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      ImageIndex = 1
      TabVisible = False
      object GroupBox4: TGroupBox
        Left = 4
        Top = 44
        Width = 285
        Height = 119
        Caption = '[ Attributes ]'
        TabOrder = 0
        object CheckBox8: TCheckBox
          Tag = 4
          Left = 12
          Top = 94
          Width = 97
          Height = 17
          Caption = 'Any file'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object CheckBox5: TCheckBox
          Tag = 5
          Left = 138
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Volume ID files'
          TabOrder = 4
        end
        object CheckBox6: TCheckBox
          Tag = 6
          Left = 138
          Top = 43
          Width = 97
          Height = 17
          Caption = 'Directory files'
          TabOrder = 5
        end
        object CheckBox7: TCheckBox
          Tag = 7
          Left = 138
          Top = 68
          Width = 97
          Height = 17
          Caption = 'Archive files'
          TabOrder = 6
        end
        object CheckBox4: TCheckBox
          Tag = 3
          Left = 12
          Top = 68
          Width = 97
          Height = 17
          Caption = 'System files'
          TabOrder = 2
        end
        object CheckBox3: TCheckBox
          Tag = 2
          Left = 12
          Top = 43
          Width = 97
          Height = 17
          Caption = 'Hidden files'
          TabOrder = 1
        end
        object CheckBox2: TCheckBox
          Tag = 1
          Left = 12
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Read Only files'
          TabOrder = 0
        end
      end
      object StaticText1: TStaticText
        Left = 4
        Top = 8
        Width = 275
        Height = 31
        AutoSize = False
        Caption = 
          'Now, you have to choose the attributes for '#13#10'the files you want ' +
          'to search and the mask.'
        TabOrder = 2
      end
      object GroupBox5: TGroupBox
        Left = 4
        Top = 166
        Width = 285
        Height = 49
        Caption = '[ File mask ]'
        TabOrder = 1
        object Edit5: TEdit
          Left = 8
          Top = 18
          Width = 265
          Height = 21
          TabOrder = 0
          Text = '*.*'
        end
      end
    end
    object TabSheet3: TTabSheet
      ImageIndex = 2
      TabVisible = False
      object GroupBox2: TGroupBox
        Left = 4
        Top = 6
        Width = 285
        Height = 87
        Caption = '[ Procedure Options ]'
        TabOrder = 0
        object Label1: TLabel
          Left = 10
          Top = 26
          Width = 78
          Height = 13
          Caption = 'Procedure name'
        end
        object Edit1: TEdit
          Left = 94
          Top = 22
          Width = 181
          Height = 21
          TabOrder = 0
          Text = 'FindProc'
        end
        object CheckBox1: TCheckBox
          Left = 12
          Top = 52
          Width = 201
          Height = 17
          Caption = 'Search in directory recursively'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
      end
    end
    object TabSheet4: TTabSheet
      ImageIndex = 3
      TabVisible = False
      object GroupBox3: TGroupBox
        Left = 4
        Top = 6
        Width = 287
        Height = 91
        Caption = '[ Code Options (name of variables) ]'
        TabOrder = 0
        object Label2: TLabel
          Left = 8
          Top = 22
          Width = 114
          Height = 13
          Caption = 'Variable containing path'
        end
        object Label3: TLabel
          Left = 8
          Top = 44
          Width = 101
          Height = 13
          Caption = 'TSearchRec variable'
        end
        object Label4: TLabel
          Left = 8
          Top = 66
          Width = 85
          Height = 13
          Caption = 'Temporaty integer'
        end
        object Edit2: TEdit
          Left = 128
          Top = 18
          Width = 151
          Height = 21
          TabOrder = 0
        end
        object Edit3: TEdit
          Left = 128
          Top = 40
          Width = 151
          Height = 21
          TabOrder = 1
        end
        object Edit4: TEdit
          Left = 128
          Top = 62
          Width = 151
          Height = 21
          TabOrder = 2
        end
      end
    end
  end
end
