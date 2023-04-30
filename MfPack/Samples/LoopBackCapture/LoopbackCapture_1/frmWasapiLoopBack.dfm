object frmLoopBackCapture: TfrmLoopBackCapture
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Loopback Capture Sample'
  ClientHeight = 464
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
  object Bevel1: TBevel
    Left = 8
    Top = 316
    Width = 454
    Height = 85
  end
  object Label1: TLabel
    Left = 13
    Top = 351
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
    Left = 280
    Top = 370
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
    Left = 13
    Top = 370
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
    TabOrder = 2
    Text = 'loopback-capture'
  end
  object Panel3: TPanel
    Left = 8
    Top = 214
    Width = 454
    Height = 97
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
    TabOrder = 10
    VerticalAlignment = taAlignTop
    object rbAvBufSize: TRadioButton
      Left = 9
      Top = 47
      Width = 434
      Height = 17
      Hint = 'Best for capturing streams like YouTube'
      ParentCustomHint = False
      Caption = 
        'Average processing interval (Best for capturing low latency  str' +
        'eams or fast internet)'
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
    object rbDefBufSize: TRadioButton
      Left = 9
      Top = 24
      Width = 402
      Height = 17
      Hint = 'Best for capturing local streams'
      Caption = 'Default processing interval (Best for capturing local streams)'
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
    object rbMinBufSize: TRadioButton
      Left = 9
      Top = 70
      Width = 433
      Height = 17
      Hint = 'Best for capturing low latency streams or slow internet.'
      Caption = 
        'Minimum processing interval (Best for capturing high latency str' +
        'eams or slow internet)'
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
  object butStart: TButton
    Left = 8
    Top = 407
    Width = 85
    Height = 27
    Caption = 'Start Capture'
    TabOrder = 0
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 99
    Top = 408
    Width = 85
    Height = 27
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 1
    OnClick = butStopClick
  end
  object sbMsg: TStatusBar
    Left = 0
    Top = 440
    Width = 472
    Height = 24
    DoubleBuffered = True
    Panels = <>
    ParentDoubleBuffered = False
    SimplePanel = True
    SimpleText = 'Start Capture'
    ExplicitLeft = 2
    ExplicitTop = 433
  end
  object butPlayData: TButton
    Left = 190
    Top = 408
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = butPlayDataClick
  end
  object cbxDontOverWrite: TCheckBox
    Left = 13
    Top = 330
    Width = 189
    Height = 15
    Hint = 'Do not overwrite files with the same name.'
    Caption = 'Don'#39't Overwrite Excisting Files'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 8
    Top = 34
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
    TabOrder = 5
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
    TabOrder = 6
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
    TabOrder = 7
    OnClick = cbxStayOnTopClick
  end
  object Button2: TButton
    Left = 365
    Top = 7
    Width = 97
    Height = 22
    Caption = 'Show Devices'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = Button2Click
  end
end
