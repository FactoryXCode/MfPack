object dlgMfCaptureToFile: TdlgMfCaptureToFile
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 180
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 26
    Top = 27
    Width = 65
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Device'
  end
  object Label2: TLabel
    Left = 26
    Top = 62
    Width = 65
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Capture File'
  end
  object edOutputFile: TEdit
    Left = 103
    Top = 59
    Width = 309
    Height = 21
    AutoSize = False
    TabOrder = 0
    Text = 'capture.mp4'
  end
  object cbDeviceList: TComboBox
    Left = 104
    Top = 24
    Width = 308
    Height = 21
    TabOrder = 1
  end
  object rbMP4: TRadioButton
    Left = 42
    Top = 115
    Width = 105
    Height = 17
    Caption = 'MP4'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = rbMP4Click
  end
  object rbWMV: TRadioButton
    Left = 42
    Top = 155
    Width = 105
    Height = 17
    Caption = 'WMV'
    TabOrder = 3
    OnClick = rbWMVClick
  end
  object butCapture: TButton
    Left = 242
    Top = 139
    Width = 105
    Height = 33
    Caption = 'Start Capture'
    TabOrder = 4
    OnClick = butCaptureClick
  end
end
