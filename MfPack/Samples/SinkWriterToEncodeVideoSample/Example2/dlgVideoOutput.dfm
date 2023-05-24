object dlgVideoSetttings: TdlgVideoSetttings
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Video output settings'
  ClientHeight = 264
  ClientWidth = 297
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 3
    Top = 8
    Width = 286
    Height = 225
    ParentShowHint = False
    Shape = bsFrame
    ShowHint = False
  end
  object Label1: TLabel
    Left = 22
    Top = 18
    Width = 71
    Height = 13
    Caption = 'Output Format'
  end
  object Label2: TLabel
    Left = 114
    Top = 18
    Width = 68
    Height = 13
    Caption = 'Encoding type'
  end
  object Label3: TLabel
    Left = 24
    Top = 62
    Width = 76
    Height = 13
    Caption = 'Video dimension'
  end
  object Label4: TLabel
    Left = 24
    Top = 147
    Width = 85
    Height = 19
    Hint = 'Buffer latency. By default this is 10 milliseconds.'
    AutoSize = False
    Caption = 'Buffer latency'
  end
  object Label5: TLabel
    Left = 24
    Top = 104
    Width = 59
    Height = 19
    Hint = 'Frames Per Second'
    AutoSize = False
    Caption = 'Frame rate'
    ParentShowHint = False
    ShowHint = True
  end
  object Label6: TLabel
    Left = 100
    Top = 107
    Width = 127
    Height = 19
    Hint = 'Frames Per Second'
    AutoSize = False
    Caption = 'Bit rate (bits per second)'
    ParentShowHint = False
    ShowHint = True
  end
  object Label7: TLabel
    Left = 100
    Top = 147
    Width = 82
    Height = 21
    Hint = 'Time in 100 nanoseconds units.'
    AutoSize = False
    Caption = 'Frame time'
  end
  object OKBtn: TButton
    Left = 8
    Top = 235
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 89
    Top = 235
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbxOutputFormat: TComboBox
    Left = 22
    Top = 34
    Width = 78
    Height = 21
    TabOrder = 2
    Text = 'MP4'
    OnCloseUp = cbxOutputFormatCloseUp
    Items.Strings = (
      'MP4'
      'WMV'
      'AVI')
  end
  object cbxEncodingFormat: TComboBox
    Left = 114
    Top = 34
    Width = 151
    Height = 21
    TabOrder = 3
    OnCloseUp = cbxEncodingFormatCloseUp
    Items.Strings = (
      '-')
  end
  object cbxDimensions: TComboBox
    Left = 24
    Top = 80
    Width = 151
    Height = 21
    TabOrder = 4
    OnCloseUp = cbxDimensionsCloseUp
  end
  object edLatency: TEdit
    Left = 24
    Top = 163
    Width = 48
    Height = 21
    Hint = 'Buffer latency. By default this is 10 milliseconds.'
    NumbersOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    Text = '10'
    OnChange = edLatencyChange
  end
  object cbSaveResizedBitmap: TCheckBox
    Left = 24
    Top = 201
    Width = 223
    Height = 16
    Caption = 'Save resized source bitmap(s) to file(s).'
    TabOrder = 6
    OnClick = cbSaveResizedBitmapClick
  end
  object edFps: TEdit
    Left = 24
    Top = 119
    Width = 48
    Height = 21
    Hint = 
      'FrameRate in FPS. The less movement the less FPS are needed. Def' +
      'ault is 12 FPS for animations.'
    NumbersOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    Text = '12'
    OnChange = edFpsChange
    OnEnter = edFpsEnter
  end
  object edBitRate: TEdit
    Left = 100
    Top = 119
    Width = 59
    Height = 21
    Hint = 'BitRate: Default is 800000 bits per second.'
    NumbersOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    Text = '8000000'
    OnChange = edBitRateChange
  end
  object edFrameTimeUnits: TEdit
    Left = 100
    Top = 163
    Width = 48
    Height = 21
    Hint = 'Time in 100 nanoseconds units. (minimum is 1 ms (10000 units))'
    NumbersOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    Text = '10000'
    OnChange = edFrameTimeUnitsChange
  end
end
