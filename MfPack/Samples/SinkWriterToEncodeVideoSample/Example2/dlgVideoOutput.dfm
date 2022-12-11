object dlgVideoSetttings: TdlgVideoSetttings
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Video output settings'
  ClientHeight = 219
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
    Height = 169
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
    Top = 116
    Width = 53
    Height = 18
    AutoSize = False
    Caption = 'Duration:'
  end
  object OKBtn: TButton
    Left = 8
    Top = 186
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 89
    Top = 187
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
    Left = 22
    Top = 81
    Width = 151
    Height = 21
    TabOrder = 4
    OnCloseUp = cbxDimensionsCloseUp
    Items.Strings = (
      'SD  480p (480 x 640)'
      'HD  720p (1280 x 720)'
      'Full HD 1080p (1920 x 1080)')
  end
  object edVideoLength: TEdit
    Left = 73
    Top = 113
    Width = 48
    Height = 21
    Hint = 'Duration in seconds'
    NumbersOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    Text = '20'
    OnChange = edVideoLengthChange
  end
  object cbSaveResizedBitmap: TCheckBox
    Left = 22
    Top = 147
    Width = 223
    Height = 18
    Caption = 'Save resized source bitmap to file.'
    TabOrder = 6
    OnClick = cbSaveResizedBitmapClick
  end
end
