object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LoopBackCapture Sample 2'
  ClientHeight = 428
  ClientWidth = 482
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 13
  object Bevel2: TBevel
    Left = 0
    Top = 0
    Width = 482
    Height = 163
    Align = alTop
    ExplicitTop = -1
    ExplicitWidth = 459
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
    Top = 401
    Width = 473
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
    Top = 396
    Width = 482
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 445
    ExplicitWidth = 462
  end
  object Bevel3: TBevel
    Left = 0
    Top = 397
    Width = 482
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 442
    ExplicitWidth = 462
  end
  object Panel3: TPanel
    Left = 0
    Top = 163
    Width = 482
    Height = 120
    Hint = 
      'The capture buffersize depending on the audiodevice specs and so' +
      'urce latency '
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Capture Buffer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
    VerticalAlignment = taAlignTop
    ExplicitWidth = 478
    object Label4: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 77
      Width = 39
      Height = 13
      Hint = 
        'The bitrate is determinated by the hardware. Otherwise the bitra' +
        'te will be the one choosen.'
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
    object Label5: TLabel
      Left = 7
      Top = 51
      Width = 102
      Height = 13
      Hint = 'Enter a file name without extension.'
      Alignment = taRightJustify
      Caption = 'Buffer Duration (ms):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblCaptureBufferDuration: TLabel
      Left = 109
      Top = 24
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
    object lblBufferDuration: TLabel
      Left = 178
      Top = 52
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
    object sedBufferSize: TSpinEdit
      Left = 115
      Top = 48
      Width = 52
      Height = 22
      MaxValue = 1000
      MinValue = 0
      TabOrder = 0
      Value = 10
    end
    object cbxAutoBufferSize: TCheckBox
      Left = 7
      Top = 23
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
    end
    object cbxWavFormats: TComboBox
      Left = 6
      Top = 93
      Width = 144
      Height = 21
      ItemIndex = 0
      TabOrder = 2
      Text = '44.1 kHz 16 bit'
      Items.Strings = (
        '44.1 kHz 16 bit'
        '48 kHz 24 bit'
        '48 kHz 32 bit'
        '96 kHz 24 bit'
        '96 kHz 32 bit')
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
    Top = 283
    Width = 482
    Height = 75
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 7
    ExplicitWidth = 478
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
      Left = 422
      Top = 40
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
      Left = 71
      Top = 40
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
    Left = 4
    Top = 363
    Width = 85
    Height = 27
    Caption = 'Start Capture'
    TabOrder = 8
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 95
    Top = 363
    Width = 85
    Height = 27
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 9
    OnClick = butStopClick
  end
  object butPlayData: TButton
    Left = 185
    Top = 363
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
