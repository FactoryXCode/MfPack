object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 354
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 74
    Top = 270
    Width = 431
    Height = 22
    Alignment = taCenter
    AutoSize = False
    Caption = 'Click '#39'Execute'#39' to run the sample..'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 2
    Top = 74
    Width = 587
    Height = 22
    Alignment = taCenter
    AutoSize = False
    Caption = 'Choose the output format'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 2
    Top = 136
    Width = 587
    Height = 26
    Alignment = taCenter
    AutoSize = False
    Caption = 'Choose a video dimension'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 182
    Top = 223
    Width = 141
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Video length in seconds:'
  end
  object Label4: TLabel
    Left = 0
    Top = 0
    Width = 587
    Height = 22
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Choose a bitmapfile'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 12
  end
  object butExecute: TButton
    Left = 244
    Top = 310
    Width = 95
    Height = 36
    Caption = 'Execute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = butExecuteClick
  end
  object cbxOutputFormat: TComboBox
    Left = 182
    Top = 98
    Width = 73
    Height = 21
    TabOrder = 0
    Text = 'MP4'
    OnCloseUp = cbxOutputFormatCloseUp
    Items.Strings = (
      'MP4'
      'WMV'
      'AVI')
  end
  object cbxEncodingFormat: TComboBox
    Left = 261
    Top = 98
    Width = 145
    Height = 21
    TabOrder = 1
    OnCloseUp = cbxEncodingFormatCloseUp
    Items.Strings = (
      '-')
  end
  object CheckBox1: TCheckBox
    Left = 181
    Top = 247
    Width = 223
    Height = 18
    Caption = 'Save resized source bitmap to file.'
    TabOrder = 4
  end
  object lbxVideoDimensions: TListBox
    Left = 182
    Top = 165
    Width = 224
    Height = 54
    ItemHeight = 13
    Items.Strings = (
      'SD  480p (480 x 640)'
      'HD  720p (1280 x 720)'
      'Full HD 1080p (1920 x 1080)')
    TabOrder = 2
    OnClick = lbxVideoDimensionsClick
  end
  object edVideoLength: TEdit
    Left = 324
    Top = 220
    Width = 80
    Height = 26
    TabOrder = 3
    Text = '20'
    OnChange = edVideoLengthChange
  end
  object butChooseBmpFile: TButton
    Left = 248
    Top = 26
    Width = 95
    Height = 36
    Caption = 'Choose BMP'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = butChooseBmpFileClick
  end
  object odlgBitmapFile: TOpenDialog
    Filter = '24bit Bitmap|*.bmp'
    Options = [ofReadOnly, ofEnableSizing]
    Left = 48
    Top = 20
  end
end
