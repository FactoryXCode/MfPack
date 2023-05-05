object frmLoopBackCapture: TfrmLoopBackCapture
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Loopback Capture Sample'
  ClientHeight = 438
  ClientWidth = 472
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblMsg: TLabel
    Left = 0
    Top = 414
    Width = 472
    Height = 24
    Align = alBottom
    AutoSize = False
    Caption = 'Start Capture'
    Layout = tlCenter
    ExplicitLeft = 8
    ExplicitTop = 460
    ExplicitWidth = 451
  end
  object Bevel1: TBevel
    Left = 0
    Top = 413
    Width = 472
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 434
  end
  object Panel3: TPanel
    Left = 8
    Top = 215
    Width = 454
    Height = 70
    Hint = 
      'The capture buffersize depending on the audiodevice specs and so' +
      'urce latency '
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Capture Processing Interval'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    VerticalAlignment = taAlignTop
    object lblDevicePeriod: TLabel
      Left = 9
      Top = 26
      Width = 147
      Height = 13
      Caption = 'Device Period (10 MilliSeconds)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object tbDevicePeriod: TTrackBar
      Left = 0
      Top = 39
      Width = 455
      Height = 25
      Max = 100
      Position = 10
      TabOrder = 0
      OnChange = tbDevicePeriodChange
    end
  end
  object butStart: TButton
    Left = 8
    Top = 380
    Width = 85
    Height = 27
    Caption = 'Start Capture'
    TabOrder = 0
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 99
    Top = 381
    Width = 85
    Height = 27
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object butPlayData: TButton
    Left = 190
    Top = 381
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = butPlayDataClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 33
    Width = 454
    Height = 72
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '   AudioEndpoint'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    VerticalAlignment = taAlignTop
    object rbRenderingDevice: TRadioButton
      Left = 8
      Top = 22
      Width = 145
      Height = 17
      Hint = 'Render'
      Caption = 'Audio Rendering Stream.'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = True
    end
    object rbCaptureDevice: TRadioButton
      Left = 8
      Top = 45
      Width = 145
      Height = 17
      Hint = 'Capture'
      Caption = 'Audio Capture Stream.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 111
    Width = 455
    Height = 98
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Data-flow Direction'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    VerticalAlignment = taAlignTop
    object rbConsole: TRadioButton
      Left = 8
      Top = 23
      Width = 315
      Height = 17
      Hint = 'Console'
      ParentCustomHint = False
      Caption = 'Games, system notification sounds and voice commands.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object rbMultimedia: TRadioButton
      Left = 8
      Top = 46
      Width = 315
      Height = 17
      Hint = 'Multimedia'
      Caption = 'Music, movies, narration and live music recording.'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TabStop = True
    end
    object rbCommunications: TRadioButton
      Left = 8
      Top = 69
      Width = 315
      Height = 17
      Hint = 'Communications'
      Caption = 'Voice communications (talking to another person).'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
  end
  object cbxStayOnTop: TCheckBox
    Left = 16
    Top = 8
    Width = 189
    Height = 15
    Caption = 'Stay On Top'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 5
    OnClick = cbxStayOnTopClick
  end
  object butShowdlgDevices: TButton
    Left = 365
    Top = 5
    Width = 97
    Height = 25
    Caption = 'Show Devices'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = butShowdlgDevicesClick
  end
  object Panel4: TPanel
    Left = 8
    Top = 293
    Width = 454
    Height = 81
    BevelOuter = bvLowered
    TabOrder = 8
    object Label1: TLabel
      Left = 10
      Top = 32
      Width = 79
      Height = 13
      Hint = 'Enter a file name without extension.'
      AutoSize = False
      Caption = 'FileName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblFileExt: TLabel
      Left = 274
      Top = 51
      Width = 50
      Height = 21
      Hint = 'Enter a file name without extension.'
      AutoSize = False
      Caption = '.wav'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = False
    end
    object edFileName: TEdit
      Left = 9
      Top = 51
      Width = 265
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = 'loopback-capture'
    end
    object cbxDontOverWrite: TCheckBox
      Left = 9
      Top = 11
      Width = 189
      Height = 15
      Hint = 'Do not overwrite files with the same name.'
      Caption = 'Don'#39't Overwrite Excisting Files'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
    end
  end
end
