object frmCapture: TfrmCapture
  Left = 0
  Top = 0
  Caption = 'GPU Desktop Capture Sample 2'
  ClientHeight = 1179
  ClientWidth = 1486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 23
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1486
    Height = 157
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1459
    object Bevel3: TBevel
      Left = 913
      Top = 12
      Width = 546
      Height = 136
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object Bevel2: TBevel
      Left = 484
      Top = 11
      Width = 426
      Height = 136
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object Bevel1: TBevel
      Left = 6
      Top = 11
      Width = 475
      Height = 136
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object lblMonitor: TLabel
      Left = 522
      Top = 26
      Width = 65
      Height = 23
      Alignment = taRightJustify
      Caption = 'Monitor:'
    end
    object lblOutput: TLabel
      Left = 980
      Top = 26
      Width = 59
      Height = 23
      Alignment = taRightJustify
      Caption = 'Output:'
    end
    object lblAudio: TLabel
      Left = 23
      Top = 105
      Width = 102
      Height = 23
      Caption = 'Audio device:'
    end
    object lblResolution: TLabel
      Left = 474
      Top = 69
      Width = 113
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Resolution:'
    end
    object lblFrameRate: TLabel
      Left = 474
      Top = 105
      Width = 113
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Frame rate:'
    end
    object lblAudioBitrate: TLabel
      Left = 1184
      Top = 73
      Width = 119
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Audio bitrate:'
    end
    object lblAudioCodec: TLabel
      Left = 920
      Top = 73
      Width = 119
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Audio codec:'
    end
    object cbxMonitor: TComboBox
      Left = 597
      Top = 23
      Width = 300
      Height = 31
      Style = csDropDownList
      TabOrder = 0
    end
    object edtOutput: TEdit
      Left = 1049
      Top = 23
      Width = 335
      Height = 31
      TabOrder = 1
      Text = 'capture_output.mp4'
    end
    object btnBrowse: TButton
      Left = 1393
      Top = 22
      Width = 40
      Height = 36
      Caption = '...'
      TabOrder = 2
      OnClick = btnBrowseClick
    end
    object cbxAudioDevice: TComboBox
      Left = 135
      Top = 102
      Width = 333
      Height = 31
      Style = csDropDownList
      TabOrder = 3
    end
    object cbxResolutions: TComboBox
      Left = 597
      Top = 66
      Width = 300
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 4
      Text = 'Full HD (1920 x 1080)'
      Items.Strings = (
        'Preview window size'
        '720p (1280 x 720)'
        'Full HD (1920 x 1080)'
        
          '1080p+ or WUXGA (Widescreen Ultra Extended Graphics Array) (1920' +
          ' x 1200)'
        '2K (2560 x 1440)'
        '4K (3840 x 2160)')
    end
    object cbxFrameRate: TComboBox
      Left = 597
      Top = 102
      Width = 95
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 1
      TabOrder = 5
      Text = '60 Hz'
      Items.Strings = (
        '30 Hz'
        '60 Hz')
    end
    object rbRecVideoAndAudio: TRadioButton
      Left = 24
      Top = 16
      Width = 229
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Record Video and Audio'
      Checked = True
      TabOrder = 6
      TabStop = True
      OnClick = rbRecVideoAndAudioClick
    end
    object rbRecVideo: TRadioButton
      Left = 279
      Top = 16
      Width = 155
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Record Video'
      TabOrder = 7
      OnClick = rbRecVideoClick
    end
    object rbRecAudio: TRadioButton
      Left = 24
      Top = 58
      Width = 142
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Record Audio'
      TabOrder = 8
      OnClick = rbRecAudioClick
    end
    object cbxAudioBitrate: TComboBox
      Left = 1313
      Top = 70
      Width = 120
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 9
      Text = '160 kbps'
      Items.Strings = (
        '96 kbps'
        '128 kbps'
        '160 kbps'
        '192 kbps'
        '256 kbps')
    end
    object cbxAudioCodec: TComboBox
      Left = 1049
      Top = 70
      Width = 120
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 0
      TabOrder = 10
      Text = 'AAC'
      OnChange = cbxAudioCodecChange
      OnCloseUp = cbxAudioCodecCloseUp
      Items.Strings = (
        'AAC'
        'FLAC')
    end
    object cbxAudioFormat: TComboBox
      Left = 169
      Top = 54
      Width = 113
      Height = 31
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Enabled = False
      ItemIndex = 0
      TabOrder = 11
      Text = 'WAV'
      Items.Strings = (
        'WAV'
        'FLAC')
    end
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 157
    Width = 1486
    Height = 731
    Align = alClient
    BevelOuter = bvNone
    Color = 6656
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 1459
    ExplicitHeight = 725
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 888
    Width = 1486
    Height = 291
    Align = alBottom
    BevelOuter = bvNone
    Color = clGray
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 2
    ExplicitWidth = 1459
    object Bevel5: TBevel
      Left = 9
      Top = 3
      Width = 499
      Height = 82
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object Bevel4: TBevel
      Left = 748
      Top = 3
      Width = 732
      Height = 82
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object lblAudioStateCaption: TLabel
      Left = 1218
      Top = 13
      Width = 55
      Height = 23
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Audio:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblAudioState: TLabel
      Left = 1276
      Top = 13
      Width = 70
      Height = 23
      Caption = 'Disabled'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblFPS: TLabel
      Left = 772
      Top = 51
      Width = 49
      Height = 23
      Caption = 'FPS: 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblStatus: TLabel
      Left = 831
      Top = 13
      Width = 30
      Height = 23
      Caption = 'Idle'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblModeCaption: TLabel
      Left = 962
      Top = 13
      Width = 55
      Height = 23
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblMode: TLabel
      Left = 1021
      Top = 13
      Width = 116
      Height = 23
      Caption = 'Video + Audio'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblRecTimeCaption: TLabel
      Left = 962
      Top = 51
      Width = 55
      Height = 23
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Time:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblRecTime: TLabel
      Left = 1021
      Top = 51
      Width = 70
      Height = 23
      Caption = '00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMoneyGreen
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 772
      Top = 13
      Width = 55
      Height = 23
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Status:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel6: TBevel
      Left = 513
      Top = 3
      Width = 230
      Height = 82
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
    end
    object mmoLog: TMemo
      Left = 0
      Top = 87
      Width = 1486
      Height = 204
      Align = alBottom
      Color = 6656
      DoubleBuffered = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentDoubleBuffered = False
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitTop = 192
      ExplicitWidth = 1459
    end
    object butStart: TButton
      Left = 27
      Top = 26
      Width = 113
      Height = 37
      Caption = 'Start'
      TabOrder = 1
      OnClick = butStartClick
    end
    object butStop: TButton
      Left = 146
      Top = 25
      Width = 113
      Height = 37
      Caption = 'Stop'
      Enabled = False
      TabOrder = 2
      OnClick = butStopClick
    end
    object cbxKeepOnTop: TCheckBox
      Left = 533
      Top = 44
      Width = 187
      Height = 41
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Keep on top'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 3
      OnClick = cbxKeepOnTopClick
    end
    object cbxHotKeys: TCheckBox
      Left = 533
      Top = 5
      Width = 174
      Height = 41
      Hint = 
        'F9  - Start recording'#13#10'F10 - Stop recording'#13#10'F11 - Show/Hide win' +
        'dow'
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Enable Hotkeys'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = cbxHotKeysClick
    end
    object butPlayOutput: TButton
      Left = 265
      Top = 25
      Width = 113
      Height = 37
      Caption = 'Play output'
      Enabled = False
      TabOrder = 5
      OnClick = butPlayOutputClick
    end
  end
  object tmrGUI: TTimer
    Enabled = False
    Interval = 250
    OnTimer = tmrGUITimer
    Left = 1386
    Top = 180
  end
end
