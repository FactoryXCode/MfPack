object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LoopBackCapture Sample 2'
  ClientHeight = 431
  ClientWidth = 462
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
    Width = 462
    Height = 175
    Align = alTop
    ExplicitTop = 2
    ExplicitWidth = 418
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 15
    Top = 44
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
    Top = 68
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
    Left = 0
    Top = 406
    Width = 462
    Height = 24
    Margins.Left = 6
    Align = alBottom
    AutoSize = False
    Caption = 'Start Capture'
    Layout = tlCenter
    ExplicitTop = 362
    ExplicitWidth = 418
  end
  object Bevel1: TBevel
    Left = 0
    Top = 430
    Width = 462
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitLeft = -54
    ExplicitTop = 387
    ExplicitWidth = 472
  end
  object Bevel3: TBevel
    Left = 0
    Top = 405
    Width = 462
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 359
    ExplicitWidth = 418
  end
  object edPID: TEdit
    Left = 113
    Top = 41
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
    Top = 127
    Width = 291
    Height = 17
    Caption = 'Capture audio from process ID (PID)  and its children'
    TabOrder = 1
  end
  object rb1: TRadioButton
    Left = 22
    Top = 148
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
    Left = 22
    Top = 92
    Width = 97
    Height = 22
    Hint = 'Get the Program ID of this application.'
    Caption = 'Get PID'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = butGetPIDClick
  end
  object Button1: TButton
    Left = 124
    Top = 92
    Width = 97
    Height = 22
    Hint = 'Pick a Program ID from a running process.'
    Caption = 'Show Processes'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = Button1Click
  end
  object edProcName: TEdit
    Left = 113
    Top = 65
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
    Left = 22
    Top = 15
    Width = 79
    Height = 15
    Caption = 'Stay On Top'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 6
    OnClick = cbxStayOnTopClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 289
    Width = 462
    Height = 76
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 7
    ExplicitTop = 257
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
      Left = 278
      Top = 42
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
      Left = 72
      Top = 42
      Width = 205
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
    Left = 6
    Top = 372
    Width = 85
    Height = 27
    Caption = 'Start Capture'
    TabOrder = 8
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 97
    Top = 372
    Width = 85
    Height = 27
    Caption = 'Stop Capture'
    Enabled = False
    TabOrder = 9
    OnClick = butStopClick
  end
  object butPlayData: TButton
    Left = 187
    Top = 372
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
  object Panel3: TPanel
    Left = 0
    Top = 175
    Width = 462
    Height = 114
    Hint = 
      'The capture buffersize depending on the audiodevice specs and so' +
      'urce latency '
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Capture Processing Interval'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
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
    object Label4: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 73
      Width = 44
      Height = 17
      AutoSize = False
      Caption = 'Bitrate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
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
    object rb44: TRadioButton
      Left = 6
      Top = 90
      Width = 65
      Height = 13
      Hint = '44.100 kHz at 16 bits'
      Caption = '44.1 kHz'
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
    object rb48: TRadioButton
      Left = 77
      Top = 90
      Width = 65
      Height = 13
      Hint = '48.000 kHz at 24 bits'
      Caption = '48 kHz'
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
end
