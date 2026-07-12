object frmCapture: TfrmCapture
  Left = 0
  Top = 0
  Caption = 'GPU Desktop Capture Sample 2'
  ClientHeight = 666
  ClientWidth = 907
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 907
    Height = 89
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 2
    object Bevel3: TBevel
      Left = 592
      Top = 7
      Width = 313
      Height = 77
    end
    object Bevel2: TBevel
      Left = 275
      Top = 7
      Width = 311
      Height = 77
    end
    object Bevel1: TBevel
      Left = 3
      Top = 6
      Width = 269
      Height = 77
    end
    object lblMonitor: TLabel
      Left = 300
      Top = 15
      Width = 45
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      Caption = 'Monitor:'
    end
    object lblOutput: TLabel
      Left = 624
      Top = 16
      Width = 41
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      Caption = 'Output:'
    end
    object lblAudio: TLabel
      Left = 10
      Top = 65
      Width = 69
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Audio device:'
    end
    object lblResolution: TLabel
      Left = 281
      Top = 39
      Width = 64
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Resolution:'
    end
    object lblFrameRate: TLabel
      Left = 281
      Top = 65
      Width = 64
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Frame rate:'
    end
    object lblAudioBitrate: TLabel
      Left = 745
      Top = 42
      Width = 71
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Audio bitrate:'
    end
    object lblAudioCodec: TLabel
      Left = 598
      Top = 41
      Width = 67
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Audio codec:'
    end
    object cbxMonitor: TComboBox
      Left = 350
      Top = 13
      Width = 170
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 0
    end
    object edtOutput: TEdit
      Left = 671
      Top = 13
      Width = 199
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 1
      Text = 'capture_output.mp4'
    end
    object btnBrowse: TButton
      Left = 874
      Top = 14
      Width = 23
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '...'
      TabOrder = 2
      OnClick = btnBrowseClick
    end
    object cbxAudioDevice: TComboBox
      Left = 83
      Top = 62
      Width = 182
      Height = 21
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 3
    end
    object cbxResolutions: TComboBox
      Left = 350
      Top = 37
      Width = 170
      Height = 21
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
      Left = 350
      Top = 62
      Width = 54
      Height = 21
      ItemIndex = 1
      TabOrder = 5
      Text = '60 Hz'
      Items.Strings = (
        '30 Hz'
        '60 Hz')
    end
    object rbRecVideoAndAudio: TRadioButton
      Left = 14
      Top = 11
      Width = 148
      Height = 14
      Caption = 'Record Video and Audio'
      Checked = True
      TabOrder = 6
      TabStop = True
      OnClick = rbRecVideoAndAudioClick
    end
    object rbRecVideo: TRadioButton
      Left = 169
      Top = 11
      Width = 87
      Height = 14
      Caption = 'Record Video'
      TabOrder = 7
      OnClick = rbRecVideoClick
    end
    object rbRecAudio: TRadioButton
      Left = 14
      Top = 33
      Width = 89
      Height = 13
      Caption = 'Record Audio'
      TabOrder = 8
      OnClick = rbRecAudioClick
    end
    object cbxAudioBitrate: TComboBox
      Left = 822
      Top = 39
      Width = 77
      Height = 21
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
      Left = 671
      Top = 39
      Width = 68
      Height = 21
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
      Left = 106
      Top = 31
      Width = 63
      Height = 21
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
    Top = 89
    Width = 907
    Height = 526
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BevelOuter = bvNone
    Color = 6656
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 878
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 615
    Width = 907
    Height = 51
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    BevelOuter = bvNone
    Color = clGray
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 2
    ExplicitWidth = 878
    object pnlControls: TPanel
      Left = 0
      Top = 0
      Width = 907
      Height = 51
      Align = alBottom
      BevelOuter = bvNone
      ParentShowHint = False
      ShowCaption = False
      ShowHint = False
      TabOrder = 0
      ExplicitTop = 2
      ExplicitWidth = 878
      object Bevel4: TBevel
        Left = 427
        Top = 5
        Width = 476
        Height = 46
      end
      object Bevel5: TBevel
        Left = 5
        Top = 2
        Width = 224
        Height = 46
      end
      object lblAudioStateCaption: TLabel
        Left = 707
        Top = 7
        Width = 31
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Audio:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblAudioState: TLabel
        Left = 742
        Top = 7
        Width = 151
        Height = 34
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        AutoSize = False
        Caption = 'Disabled'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMoneyGreen
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object lblFPS: TLabel
        Left = 449
        Top = 29
        Width = 25
        Height = 12
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        Caption = 'FPS: 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblStatus: TLabel
        Left = 471
        Top = 7
        Width = 17
        Height = 12
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        Caption = 'Idle'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMoneyGreen
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblModeCaption: TLabel
        Left = 556
        Top = 7
        Width = 31
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Mode:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblMode: TLabel
        Left = 590
        Top = 7
        Width = 62
        Height = 12
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Video + Audio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMoneyGreen
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblRecTimeCaption: TLabel
        Left = 556
        Top = 29
        Width = 31
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Time:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblRecTime: TLabel
        Left = 590
        Top = 29
        Width = 34
        Height = 12
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = '00:00:00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMoneyGreen
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label1: TLabel
        Left = 436
        Top = 7
        Width = 31
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Status:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel6: TBevel
        Left = 232
        Top = 2
        Width = 195
        Height = 46
      end
      object butStart: TButton
        Left = 15
        Top = 14
        Width = 64
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Start'
        TabOrder = 0
        OnClick = butStartClick
      end
      object butStop: TButton
        Left = 83
        Top = 14
        Width = 63
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Stop'
        Enabled = False
        TabOrder = 1
        OnClick = butStopClick
      end
      object cbxKeepOnTop: TCheckBox
        Left = 239
        Top = 25
        Width = 90
        Height = 19
        Caption = 'Keep on top'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        State = cbChecked
        TabOrder = 2
        OnClick = cbxKeepOnTopClick
      end
      object cbxHotKeys: TCheckBox
        Left = 239
        Top = 5
        Width = 90
        Height = 18
        Hint = 'Write a new Log_yyyymmdd_hhnnss.txt file for each recording'
        Caption = 'Enable Hotkeys'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbxHotKeysClick
      end
      object butPlayOutput: TButton
        Left = 150
        Top = 14
        Width = 64
        Height = 21
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Play output'
        Enabled = False
        TabOrder = 4
        OnClick = butPlayOutputClick
      end
      object cbxEnableLogging: TCheckBox
        Left = 333
        Top = 5
        Width = 88
        Height = 18
        Hint = 'Write a new Log_yyyymmdd_hhnnss.txt file for each recording'
        Caption = 'Enable logging'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 5
        OnClick = cbxEnableLoggingClick
      end
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
