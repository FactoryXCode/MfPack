object frmLoopBackCapture: TfrmLoopBackCapture
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Loopback Capture Sample 1'
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
    Left = 0
    Top = 439
    Width = 472
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 434
  end
  object lblStatus: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 443
    Width = 466
    Height = 18
    Align = alBottom
    AutoSize = False
    Caption = 'Start Capture'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
    ExplicitTop = 424
  end
  object Panel3: TPanel
    Left = 10
    Top = 219
    Width = 454
    Height = 91
    Hint = 
      'The capture buffersize depending on the audiodevice specs and so' +
      'urce latency '
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Capture Buffer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    VerticalAlignment = taAlignTop
    object lblBufferDuration: TLabel
      Left = 8
      Top = 46
      Width = 195
      Height = 13
      Caption = 'Capture buffer duration : 10 milliseconds'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblCaptureBufferDuration: TLabel
      Left = 114
      Top = 25
      Width = 216
      Height = 13
      Caption = 'Auto capture buffer duration: 10 milliseconds'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object tbBufferDuration: TTrackBar
      Left = 1
      Top = 65
      Width = 452
      Height = 25
      Align = alBottom
      Max = 30
      Position = 5
      TabOrder = 0
      OnChange = tbBufferDurationChange
      ExplicitTop = 64
    end
    object cbxAutoBufferSize: TCheckBox
      Left = 8
      Top = 24
      Width = 100
      Height = 16
      Caption = 'Auto buffer size.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = cbxAutoBufferSizeClick
    end
  end
  object butStart: TButton
    Left = 8
    Top = 406
    Width = 85
    Height = 27
    Caption = 'Start capture'
    TabOrder = 6
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 99
    Top = 406
    Width = 85
    Height = 27
    Caption = 'Stop capture'
    Enabled = False
    TabOrder = 7
    OnClick = butStopClick
  end
  object butPlayData: TButton
    Left = 190
    Top = 406
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = butPlayDataClick
  end
  object Panel1: TPanel
    Left = 10
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
    TabOrder = 2
    VerticalAlignment = taAlignTop
    object rbRenderingDevice: TRadioButton
      Left = 6
      Top = 22
      Width = 145
      Height = 17
      Hint = 'Render'
      Caption = 'Audio rendering stream.'
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
      Left = 6
      Top = 45
      Width = 145
      Height = 17
      Hint = 'Capture'
      Caption = 'Audio capture stream.'
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
    TabOrder = 3
    VerticalAlignment = taAlignTop
    object rbConsole: TRadioButton
      Left = 8
      Top = 24
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
      OnMouseUp = rbConsoleMouseUp
    end
    object rbMultimedia: TRadioButton
      Left = 9
      Top = 47
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
      OnMouseUp = rbConsoleMouseUp
    end
    object rbCommunications: TRadioButton
      Left = 8
      Top = 70
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
      OnMouseUp = rbConsoleMouseUp
    end
  end
  object cbxStayOnTop: TCheckBox
    Left = 17
    Top = 8
    Width = 189
    Height = 15
    Caption = 'Stay On Top'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    OnClick = cbxStayOnTopClick
  end
  object butShowdlgDevices: TButton
    Left = 364
    Top = 3
    Width = 97
    Height = 25
    Caption = 'Show Devices'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = butShowdlgDevicesClick
  end
  object Panel4: TPanel
    Left = 8
    Top = 319
    Width = 454
    Height = 81
    BevelOuter = bvLowered
    TabOrder = 5
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
      Left = 405
      Top = 51
      Width = 44
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
      Left = 8
      Top = 51
      Width = 395
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
      TabOrder = 1
      Text = 'loopback-capture'
      OnKeyUp = edFileNameKeyUp
    end
    object cbxDontOverWrite: TCheckBox
      Left = 9
      Top = 11
      Width = 189
      Height = 15
      Hint = 'Do not overwrite files with the same name.'
      Caption = 'Don'#39't overwrite excisting files'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 0
    end
  end
  object butResetEngine: TButton
    Left = 384
    Top = 406
    Width = 80
    Height = 27
    Hint = 'Reset the engine when having issues.'
    Caption = 'Reset engine'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    OnClick = butResetEngineClick
  end
end
