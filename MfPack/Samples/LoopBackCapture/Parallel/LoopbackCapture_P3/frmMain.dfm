object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WAS Loopback Capture Sample 3'
  ClientHeight = 433
  ClientWidth = 450
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
    Top = 408
    Width = 450
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 439
    ExplicitWidth = 472
  end
  object lblStatus: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 412
    Width = 444
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
    ExplicitLeft = 8
    ExplicitTop = 443
    ExplicitWidth = 465
  end
  object Panel3: TPanel
    Left = 0
    Top = 213
    Width = 450
    Height = 76
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
    TabOrder = 1
    VerticalAlignment = taAlignTop
    object lblBufferDuration: TLabel
      Left = 69
      Top = 50
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
    object Label2: TLabel
      Left = 326
      Top = 50
      Width = 38
      Height = 13
      Hint = 'Latency in milliseconds.'
      Caption = 'Latency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
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
    object spedLatency: TSpinEdit
      Left = 370
      Top = 47
      Width = 51
      Height = 22
      Hint = 'Latency in milliseconds.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      MaxValue = 1000
      MinValue = 0
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Value = 10
    end
    object sedBufferSize: TSpinEdit
      Left = 8
      Top = 47
      Width = 52
      Height = 22
      MaxValue = 1000
      MinValue = 0
      TabOrder = 2
      Value = 10
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 213
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  AudioEndpoint'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    VerticalAlignment = taAlignTop
    object Label4: TLabel
      Left = 7
      Top = 68
      Width = 25
      Height = 13
      Caption = 'Role'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 7
      Top = 150
      Width = 77
      Height = 13
      Caption = 'Other options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object rbConsole: TRadioButton
      Left = 16
      Top = 83
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
      Left = 16
      Top = 104
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
      Left = 16
      Top = 125
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
    object cbxDisableMmcss: TCheckBox
      Left = 16
      Top = 169
      Width = 169
      Height = 14
      Caption = 'Disable the use of MMCSS.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object Panel2: TPanel
      Left = 4
      Top = 25
      Width = 452
      Height = 43
      Alignment = taLeftJustify
      BevelOuter = bvNone
      TabOrder = 4
      object Label3: TLabel
        Left = 3
        Top = 0
        Width = 53
        Height = 13
        Caption = 'Data flow'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rbRenderingDevice: TRadioButton
        Left = 12
        Top = 18
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
        Left = 172
        Top = 18
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
      object butShowdlgDevices: TButton
        Left = 341
        Top = 14
        Width = 97
        Height = 25
        Caption = 'Show Devices'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = butShowdlgDevicesClick
      end
    end
    object cbxEnableStreamSwitch: TCheckBox
      Left = 234
      Top = 169
      Width = 194
      Height = 14
      Caption = 'Enable stream switch.'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 5
    end
    object cbxUsePCMAudioFmt: TCheckBox
      Left = 16
      Top = 189
      Width = 177
      Height = 14
      Hint = 'Will be set in Create.'
      Caption = 'Use PCM audio output format.'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 6
    end
    object cbxStayOnTop: TCheckBox
      Left = 234
      Top = 189
      Width = 95
      Height = 15
      Caption = 'Stay On Top'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 7
      OnClick = cbxStayOnTopClick
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 289
    Width = 450
    Height = 79
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 2
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
      Left = 9
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
      Text = 'WAS-capture'
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
  object butStartStop: TButton
    Left = 7
    Top = 375
    Width = 85
    Height = 27
    Caption = 'Start capture'
    TabOrder = 3
    OnClick = butStartStopClick
  end
  object butPlayData: TButton
    Left = 98
    Top = 375
    Width = 80
    Height = 27
    Hint = 'Play recorded data.'
    Caption = 'Play data'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = butPlayDataClick
  end
  object butResetEngine: TButton
    Left = 362
    Top = 375
    Width = 80
    Height = 27
    Hint = 'Reset the engine when having issues.'
    Caption = 'Reset engine'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = butResetEngineClick
  end
end
