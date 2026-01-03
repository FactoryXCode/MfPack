object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LoopBackCapture Sample 2'
  ClientHeight = 687
  ClientWidth = 706
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 144
  TextHeight = 21
  object Bevel2: TBevel
    Left = 0
    Top = 0
    Width = 706
    Height = 245
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    ExplicitWidth = 723
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 23
    Top = 53
    Width = 138
    Height = 25
    Hint = 
      'Obtain the process ID for the process tree you wish to capture o' +
      'r exclude from capture.'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Process ID (PID):'
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 23
    Top = 87
    Width = 138
    Height = 26
    Hint = 
      'Obtain the process ID for the process tree you wish to capture o' +
      'r exclude from capture.'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Process Name:'
  end
  object lblMsg: TLabel
    AlignWithMargins = True
    Left = 9
    Top = 646
    Width = 692
    Height = 36
    Margins.Left = 9
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    AutoSize = False
    Caption = 'Start Capture'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
    ExplicitTop = 602
    ExplicitWidth = 710
  end
  object Bevel1: TBevel
    Left = 0
    Top = 638
    Width = 706
    Height = 2
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 594
    ExplicitWidth = 723
  end
  object Bevel3: TBevel
    Left = 0
    Top = 640
    Width = 706
    Height = 1
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 596
    ExplicitWidth = 723
  end
  object Panel3: TPanel
    Left = 0
    Top = 245
    Width = 706
    Height = 180
    Hint = 
      'The capture buffersize depending on the audiodevice specs and so' +
      'urce latency '
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Capture Buffer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
    VerticalAlignment = taAlignTop
    ExplicitWidth = 723
    object Label4: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 116
      Width = 58
      Height = 21
      Hint = 
        'The bitrate is determinated by the hardware. Otherwise the bitra' +
        'te will be the one choosen.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Bitrate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblCaptureBufferDuration: TLabel
      Left = 164
      Top = 36
      Width = 334
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Auto capture buffer duration: 10 milliseconds'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object lblBufferDuration: TLabel
      Left = 105
      Top = 78
      Width = 301
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Capture buffer duration : 10 milliseconds'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 538
      Top = 77
      Width = 60
      Height = 22
      Hint = 'Latency in milliseconds.'
      Caption = 'Latency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object sedBufferSize: TSpinEdit
      Left = 11
      Top = 72
      Width = 78
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      MaxValue = 1000
      MinValue = 0
      TabOrder = 0
      Value = 10
    end
    object cbxAutoBufferSize: TCheckBox
      Left = 11
      Top = 35
      Width = 150
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Auto buffer size.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cbxWavFormats: TComboBox
      Left = 9
      Top = 140
      Width = 216
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
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
    object spedLatency: TSpinEdit
      Left = 604
      Top = 74
      Width = 71
      Height = 33
      Hint = 'Latency in milliseconds.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      MaxValue = 1000
      MinValue = 0
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Value = 10
    end
  end
  object edPID: TEdit
    Left = 170
    Top = 47
    Width = 159
    Height = 29
    Hint = 'Enter a numeric value!'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    NumbersOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = '0'
    OnKeyUp = edPIDKeyUp
  end
  object rb2: TRadioButton
    Left = 33
    Top = 177
    Width = 437
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Capture audio from process ID (PID)  and its children'
    TabOrder = 1
  end
  object rb1: TRadioButton
    Left = 33
    Top = 206
    Width = 578
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 
      'Capture audio from all processes except process ID (PID) and its' +
      ' children'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object butGetPID: TButton
    Left = 30
    Top = 126
    Width = 146
    Height = 36
    Hint = 'Get the Program ID of this application.'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Get this PID'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = butGetPIDClick
  end
  object butShowProcesses: TButton
    Left = 183
    Top = 126
    Width = 146
    Height = 36
    Hint = 'Pick a Program ID from a running process.'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Choose a process'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = butShowProcessesClick
  end
  object edProcName: TEdit
    Left = 170
    Top = 83
    Width = 429
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 5
    Text = 'Unknown'
    OnKeyUp = edPIDKeyUp
  end
  object cbxStayOnTop: TCheckBox
    Left = 30
    Top = 12
    Width = 144
    Height = 23
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Stay On Top'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
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
    Top = 425
    Width = 706
    Height = 152
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 7
    ExplicitWidth = 697
    object Label1: TLabel
      Left = 9
      Top = 76
      Width = 97
      Height = 19
      Hint = 'Enter a file name without extension.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'FileName:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object lblFileExt: TLabel
      Left = 633
      Top = 106
      Width = 42
      Height = 24
      Hint = 'Enter a file name without extension.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '.wav'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = False
    end
    object lblOutputFmt: TLabel
      Left = 11
      Top = 12
      Width = 123
      Height = 21
      AutoSize = False
      Caption = 'Output format'
    end
    object edFileName: TEdit
      Left = 11
      Top = 106
      Width = 619
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = 'loopback-capture'
      OnKeyUp = edFileNameKeyUp
    end
    object cbxDontOverWrite: TCheckBox
      Left = 407
      Top = 44
      Width = 273
      Height = 22
      Hint = 'Do not overwrite files with the same name.'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Don'#39't overwrite excisting  files.'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
    end
    object cbxOutputFormat: TComboBox
      Left = 11
      Top = 39
      Width = 160
      Height = 29
      ItemIndex = 0
      TabOrder = 2
      Text = 'WAV (Lossless)'
      OnChange = cbxOutputFormatChange
      Items.Strings = (
        'WAV (Lossless)'
        'FLAC (Lossless)')
    end
  end
  object butStart: TButton
    Left = 6
    Top = 587
    Width = 128
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Start Capture'
    TabOrder = 8
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 143
    Top = 587
    Width = 127
    Height = 40
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 9
    OnClick = butStopClick
  end
  object butPlayData: TButton
    Left = 278
    Top = 587
    Width = 120
    Height = 40
    Hint = 'Play recorded data.'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = butPlayDataClick
  end
  object dlgSave: TSaveDialog
    Left = 623
    Top = 6
  end
end
