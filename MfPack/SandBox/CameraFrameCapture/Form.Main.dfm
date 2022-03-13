object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Media Foundation Camera Capture Demo'
  ClientHeight = 743
  ClientWidth = 1205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnShow = HandlFormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1205
    Height = 161
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1205
      161)
    object lblSelectDevice: TLabel
      Left = 16
      Top = 13
      Width = 72
      Height = 15
      Caption = 'Select Device:'
    end
    object lblResoltution: TLabel
      Left = 448
      Top = 10
      Width = 59
      Height = 15
      Caption = 'Resolution:'
    end
    object lblLog: TLabel
      Left = 16
      Top = 72
      Width = 23
      Height = 15
      Caption = 'Log:'
    end
    object cbxCaptureDevices: TComboBox
      Left = 94
      Top = 8
      Width = 259
      Height = 23
      DropDownCount = 30
      TabOrder = 0
      OnChange = HandleSelectedDeviceChange
    end
    object cbxResolution: TComboBox
      Left = 513
      Top = 8
      Width = 360
      Height = 23
      Style = csDropDownList
      TabOrder = 1
      OnChange = cbxResolutionChange
    end
    object btnRefreshDevices: TButton
      Left = 359
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = btnRefreshDevicesClick
    end
    object memLog: TMemo
      Left = 94
      Top = 37
      Width = 1069
      Height = 79
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object btnClearLog: TButton
      Left = 1088
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Clear Log'
      TabOrder = 4
      OnClick = btnClearLogClick
    end
    object btnCaptureFrame: TButton
      Left = 647
      Top = 122
      Width = 91
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Capture Frame'
      TabOrder = 5
      OnClick = btnCaptureFrameClick
    end
    object btnSaveImage: TButton
      Left = 1000
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save Image'
      TabOrder = 6
      OnClick = HandleSaveImageClick
    end
    object btnStartBurst: TButton
      Left = 753
      Top = 122
      Width = 113
      Height = 25
      Caption = 'Start Burst Capture'
      TabOrder = 7
      OnClick = HandleStartBurstCapture
    end
    object btnStopBurst: TButton
      Left = 877
      Top = 122
      Width = 113
      Height = 25
      Caption = 'Stop Burst Capture'
      TabOrder = 8
      OnClick = HandleStopBurstCapture
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 161
    Width = 1205
    Height = 582
    Align = alClient
    TabOrder = 1
    object grpVideoPreview: TGroupBox
      Left = 1
      Top = 1
      Width = 607
      Height = 580
      Align = alClient
      Caption = ' Capture Preview '
      DefaultHeaderFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clBlack
      HeaderFont.Height = 18
      HeaderFont.Name = 'Segoe UI'
      HeaderFont.Style = []
      HeaderFont.Quality = fqClearTypeNatural
      ParentFont = False
      TabOrder = 0
      object pnlVideo: TPanel
        Left = 2
        Top = 17
        Width = 603
        Height = 561
        Align = alClient
        BevelOuter = bvNone
        Color = clBlack
        DoubleBuffered = True
        ParentBackground = False
        ParentDoubleBuffered = False
        TabOrder = 0
        OnResize = pnlVideoResize
      end
    end
    object grpFrameCapture: TGroupBox
      Left = 608
      Top = 1
      Width = 596
      Height = 580
      Align = alRight
      Caption = ' Frame Capture '
      DefaultHeaderFont = False
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = 18
      HeaderFont.Name = 'Segoe UI'
      HeaderFont.Style = []
      HeaderFont.Quality = fqClearTypeNatural
      TabOrder = 1
      object pnlFrameCapture: TPanel
        Left = 2
        Top = 17
        Width = 592
        Height = 561
        Align = alClient
        Caption = 'No Image. Waiting frame capture...'
        Color = clMedGray
        ParentBackground = False
        TabOrder = 0
        object picFrame: TImage
          Left = 1
          Top = 1
          Width = 590
          Height = 559
          Align = alClient
          AutoSize = True
          Center = True
          Proportional = True
          ExplicitLeft = 0
          ExplicitTop = 5
          ExplicitWidth = 618
          ExplicitHeight = 584
        end
      end
    end
  end
  object sdSaveFrame: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 
      'BMP image (*.bmp)|*.bmp|PNG image (*.png)|*.png|JPEG image (*.jp' +
      'g, *.jpeg)|*.jpg'
    Left = 32
    Top = 328
  end
end
