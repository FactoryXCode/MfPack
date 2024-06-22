object frmLoopBackCapture: TfrmLoopBackCapture
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Loopback Capture Sample 1'
  ClientHeight = 376
  ClientWidth = 472
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 351
    Width = 472
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 439
  end
  object lblStatus: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 355
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
    ExplicitTop = 443
  end
  object Panel3: TPanel
    Left = 0
    Top = 149
    Width = 472
    Height = 82
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
    TabOrder = 2
    VerticalAlignment = taAlignTop
    ExplicitTop = 170
    object lblBufferDuration: TLabel
      Left = 65
      Top = 51
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
      TabOrder = 0
      OnClick = cbxAutoBufferSizeClick
    end
    object sedBufferSize: TSpinEdit
      Left = 7
      Top = 48
      Width = 52
      Height = 22
      MaxValue = 1000
      MinValue = 0
      TabOrder = 1
      Value = 10
    end
  end
  object butStart: TButton
    Left = 8
    Top = 318
    Width = 85
    Height = 27
    Caption = 'Start capture'
    TabOrder = 4
    OnClick = butStartClick
  end
  object butStop: TButton
    Left = 99
    Top = 318
    Width = 85
    Height = 27
    Caption = 'Stop capture'
    Enabled = False
    TabOrder = 5
    OnClick = butStopClick
  end
  object butPlayData: TButton
    Left = 190
    Top = 318
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = butPlayDataClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 51
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '   AudioEndpoint'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    VerticalAlignment = taAlignTop
    object rbRenderingDevice: TRadioButton
      Left = 6
      Top = 22
      Width = 145
      Height = 17
      Hint = 'Render'
      Caption = 'Audio rendering EndPoint.'
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
      Left = 168
      Top = 22
      Width = 145
      Height = 17
      Hint = 'Capture'
      Caption = 'Audio capture EndPoint.'
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
    object butShowdlgDevices: TButton
      Left = 367
      Top = 18
      Width = 97
      Height = 25
      Caption = 'Select Device'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = butShowdlgDevicesClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 51
    Width = 472
    Height = 98
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Data-flow Direction'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    VerticalAlignment = taAlignTop
    ExplicitTop = 72
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
      Left = 8
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
  object Panel4: TPanel
    Left = 0
    Top = 231
    Width = 472
    Height = 81
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 3
    ExplicitTop = 252
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
    object cbxStayOnTop: TCheckBox
      Left = 186
      Top = 11
      Width = 106
      Height = 15
      Caption = 'Stay On Top'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
      OnClick = cbxStayOnTopClick
    end
  end
  object butResetEngine: TButton
    Left = 384
    Top = 318
    Width = 80
    Height = 27
    Hint = 'Reset the engine when having issues.'
    Caption = 'Reset engine'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = butResetEngineClick
  end
end
