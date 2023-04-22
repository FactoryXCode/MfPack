object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'LoopBackCapture Sample 2'
  ClientHeight = 357
  ClientWidth = 428
  Color = clBtnFace
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
    Left = 8
    Top = 8
    Width = 405
    Height = 193
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 15
    Top = 70
    Width = 92
    Height = 17
    Hint = 
      'Obtain the process ID for the process tree you wish to capture o' +
      'r exclude from capture.'#10'You can use Task Manager or the tlist pr' +
      'ogram to get this ID. Run the sample with the process ID, the'#10'de' +
      'sired capture mode (including the process tree or excluding it),' +
      ' and the output WAV file.'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Process ID (PID):'
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 15
    Top = 92
    Width = 92
    Height = 17
    Hint = 
      'Obtain the process ID for the process tree you wish to capture o' +
      'r exclude from capture.'#10'You can use Task Manager or the tlist pr' +
      'ogram to get this ID. Run the sample with the process ID, the'#10'de' +
      'sired capture mode (including the process tree or excluding it),' +
      ' and the output WAV file.'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Process Name:'
  end
  object sbMsg: TStatusBar
    Left = 0
    Top = 333
    Width = 428
    Height = 24
    DoubleBuffered = True
    Panels = <>
    ParentDoubleBuffered = False
    SimplePanel = True
    SimpleText = 'Start Capture'
  end
  object cbxDontOverWrite: TCheckBox
    Left = 22
    Top = 16
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
  object edPID: TEdit
    Left = 113
    Top = 67
    Width = 106
    Height = 21
    Hint = 'Enter a numeric value!'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = '0'
    OnKeyUp = edPIDKeyUp
  end
  object rb2: TRadioButton
    Left = 22
    Top = 149
    Width = 291
    Height = 17
    Caption = 'Capture audio from process ID (PID)  and its children'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object rb1: TRadioButton
    Left = 22
    Top = 172
    Width = 385
    Height = 17
    Caption = 
      'Capture audio from all processes except process ID (PID) and its' +
      ' children'
    TabOrder = 4
  end
  object butGetPID: TButton
    Left = 19
    Top = 116
    Width = 97
    Height = 21
    Hint = 'Get the Program ID of this application.'
    Caption = 'Get PID'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = butGetPIDClick
  end
  object Button1: TButton
    Left = 122
    Top = 116
    Width = 97
    Height = 22
    Hint = 'Pick a Program ID from a running process.'
    Caption = 'Show Processes'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = Button1Click
  end
  object edProcName: TEdit
    Left = 113
    Top = 89
    Width = 286
    Height = 21
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 7
    Text = 'Unknown'
    OnKeyUp = edPIDKeyUp
  end
  object cbxStayOnTop: TCheckBox
    Left = 22
    Top = 37
    Width = 91
    Height = 15
    Caption = 'Stay On Top'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 8
    OnClick = cbxStayOnTopClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 206
    Width = 405
    Height = 121
    BevelOuter = bvLowered
    TabOrder = 9
    object Label1: TLabel
      Left = 11
      Top = 42
      Width = 79
      Height = 13
      Hint = 'Enter a file name without extension.'
      AutoSize = False
      Caption = 'FileName:'
      ParentShowHint = False
      ShowHint = True
    end
    object lblFileExt: TLabel
      Left = 267
      Top = 39
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
    object Label4: TLabel
      AlignWithMargins = True
      Left = 11
      Top = 10
      Width = 44
      Height = 17
      Hint = 
        'Obtain the process ID for the process tree you wish to capture o' +
        'r exclude from capture.'#10'You can use Task Manager or the tlist pr' +
        'ogram to get this ID. Run the sample with the process ID, the'#10'de' +
        'sired capture mode (including the process tree or excluding it),' +
        ' and the output WAV file.'
      AutoSize = False
      Caption = 'Bitrate:'
    end
    object butStart: TButton
      Left = 11
      Top = 84
      Width = 85
      Height = 27
      Caption = 'Start Capture'
      TabOrder = 0
      OnClick = butStartClick
    end
    object butStop: TButton
      Left = 99
      Top = 84
      Width = 85
      Height = 27
      Caption = 'Stop Capture'
      Enabled = False
      TabOrder = 1
      OnClick = butStopClick
    end
    object edFileName: TEdit
      Left = 61
      Top = 39
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
      TabOrder = 2
      Text = 'loopback-capture'
      OnKeyUp = edFileNameKeyUp
    end
    object butPlayData: TButton
      Left = 186
      Top = 84
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
    object rb44: TRadioButton
      Left = 53
      Top = 12
      Width = 65
      Height = 13
      Hint = '44.100 kHz at 16 bits'
      Caption = '44.1 kHz'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      TabStop = True
    end
    object rb48: TRadioButton
      Left = 124
      Top = 12
      Width = 65
      Height = 13
      Hint = '48.000 kHz at 24 bits'
      Caption = '48 kHz'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
  end
end
