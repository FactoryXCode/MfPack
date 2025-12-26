object frmCapture: TfrmCapture
  Left = 0
  Top = 0
  Caption = 'GPU Desktop Capture Sample 2'
  ClientHeight = 1119
  ClientWidth = 1459
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
    Width = 1459
    Height = 157
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
      OnChange = cbxAudioFormatChange
      Items.Strings = (
        'WAV'
        'FLAC')
    end
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 157
    Width = 1459
    Height = 689
    Align = alClient
    BevelOuter = bvNone
    Color = 6656
    ParentBackground = False
    TabOrder = 1
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 846
    Width = 1459
    Height = 273
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblFPS: TLabel
      Left = 450
      Top = 24
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
      Left = 252
      Top = 25
      Width = 30
      Height = 23
      Caption = 'Idle'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object mmoLog: TMemo
      Left = 0
      Top = 69
      Width = 1459
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
    end
    object btnStart: TButton
      Left = 4
      Top = 19
      Width = 113
      Height = 37
      Caption = 'Start'
      TabOrder = 1
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 123
      Top = 18
      Width = 113
      Height = 37
      Caption = 'Stop'
      Enabled = False
      TabOrder = 2
      OnClick = btnStopClick
    end
  end
end
