object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LoopBackCapture Sample 2'
  ClientHeight = 427
  ClientWidth = 459
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
  object Bevel2: TBevel
    Left = 0
    Top = 0
    Width = 459
    Height = 163
    Align = alTop
    ExplicitTop = -1
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 15
    Top = 35
    Width = 92
    Height = 17
    Hint = 
      'Obtain the process ID for the process tree you wish to capture o' +
      'r exclude from capture.'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Process ID (PID):'
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 15
    Top = 58
    Width = 92
    Height = 17
    Hint = 
      'Obtain the process ID for the process tree you wish to capture o' +
      'r exclude from capture.'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Process Name:'
  end
  object lblMsg: TLabel
    AlignWithMargins = True
    Left = 6
    Top = 400
    Width = 450
    Height = 24
    Margins.Left = 6
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
    ExplicitTop = 452
    ExplicitWidth = 453
  end
  object Bevel1: TBevel
    Left = 0
    Top = 395
    Width = 459
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 445
    ExplicitWidth = 462
  end
  object Bevel3: TBevel
    Left = 0
    Top = 396
    Width = 459
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 442
    ExplicitWidth = 462
  end
  object Panel3: TPanel
    Left = 0
    Top = 163
    Width = 459
    Height = 117
    Hint = 
      'The capture buffersize depending on the audiodevice specs and so' +
      'urce latency '
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Buffers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
    VerticalAlignment = taAlignTop
    object Label4: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 42
      Width = 44
      Height = 17
      Hint = 
        'The bitrate is determinated by the hardware. Otherwise the bitra' +
        'te will be the one choosen.'
      AutoSize = False
      Caption = 'Bitrate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblCaptureBufferDuration: TLabel
      Left = 10
      Top = 20
      Width = 269
      Height = 13
      AutoSize = False
      Caption = 'Capture Buffer Duration: 0 ms.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object rb441b16: TRadioButton
      Left = 4
      Top = 58
      Width = 100
      Height = 13
      Hint = '44.100 kHz at 16 bits'
      Caption = '44.1 kHz 16 bit'
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
    object rb48b24: TRadioButton
      Left = 4
      Top = 77
      Width = 100
      Height = 13
      Hint = '48.000 kHz at 24 bits'
      Caption = '48 kHz 24 bit'
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
    object rb48b32: TRadioButton
      Left = 110
      Top = 77
      Width = 100
      Height = 13
      Hint = '48.000 kHz at 32 bits'
      Caption = '48 kHz 32 bit'
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
    object rb96b24: TRadioButton
      Left = 4
      Top = 96
      Width = 100
      Height = 13
      Hint = '96.000 kHz at 24 bits'
      Caption = '96 kHz 24 bit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object rb96b32: TRadioButton
      Left = 110
      Top = 96
      Width = 100
      Height = 13
      Hint = '96.000 kHz at 32 bits'
      Caption = '96 kHz 32 bit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
  end
  object edPID: TEdit
    Left = 113
    Top = 31
    Width = 106
    Height = 21
    Hint = 'Enter a numeric value!'
    NumbersOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = '0'
    OnKeyUp = edPIDKeyUp
  end
  object rb2: TRadioButton
    Left = 22
    Top = 118
    Width = 291
    Height = 17
    Caption = 'Capture audio from process ID (PID)  and its children'
    TabOrder = 1
  end
  object rb1: TRadioButton
    Left = 22
    Top = 137
    Width = 385
    Height = 17
    Caption = 
      'Capture audio from all processes except process ID (PID) and its' +
      ' children'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object butGetPID: TButton
    Left = 20
    Top = 84
    Width = 97
    Height = 24
    Hint = 'Get the Program ID of this application.'
    Caption = 'Get this PID'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = butGetPIDClick
  end
  object butShowProcesses: TButton
    Left = 122
    Top = 84
    Width = 97
    Height = 24
    Hint = 'Pick a Program ID from a running process.'
    Caption = 'Choose a process'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = butShowProcessesClick
  end
  object edProcName: TEdit
    Left = 113
    Top = 55
    Width = 286
    Height = 21
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 5
    Text = 'Unknown'
    OnKeyUp = edPIDKeyUp
  end
  object cbxStayOnTop: TCheckBox
    Left = 20
    Top = 8
    Width = 96
    Height = 15
    Caption = 'Stay On Top'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 6
    OnClick = cbxStayOnTopClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 280
    Width = 459
    Height = 75
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 7
    ExplicitTop = 323
    object Label1: TLabel
      Left = 22
      Top = 45
      Width = 79
      Height = 13
      Hint = 'Enter a file name without extension.'
      AutoSize = False
      Caption = 'FileName:'
      ParentShowHint = False
      ShowHint = True
    end
    object lblFileExt: TLabel
      Left = 423
      Top = 43
      Width = 27
      Height = 16
      Hint = 'Enter a file name without extension.'
      Caption = '.wav'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = False
    end
    object edFileName: TEdit
      Left = 74
      Top = 43
      Width = 349
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
      OnKeyUp = edFileNameKeyUp
    end
    object cbxDontOverWrite: TCheckBox
      Left = 22
      Top = 11
      Width = 185
      Height = 15
      Hint = 'Do not overwrite files with the same name.'
      Caption = 'Don'#39't overwrite excisting  files.'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object butStart: TButton
    Left = 8
    Top = 361
    Width = 85
    Height = 27
    Caption = 'Start Capture'
    TabOrder = 8
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 97
    Top = 361
    Width = 85
    Height = 27
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 9
    OnClick = butStopClick
  end
  object butPlayData: TButton
    Left = 187
    Top = 361
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = butPlayDataClick
  end
end
