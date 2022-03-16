object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Media Foundation Camera Capture Demo'
  ClientHeight = 723
  ClientWidth = 1205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  Menu = MainMenu1
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
      Top = 73
      Width = 23
      Height = 15
      Caption = 'Log:'
    end
    object lblSupported: TLabel
      Left = 814
      Top = 13
      Width = 22
      Height = 15
      Caption = 'N/A'
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
      Width = 288
      Height = 23
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
      Width = 1091
      Height = 79
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object btnClearLog: TButton
      Left = 1110
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Clear Log'
      TabOrder = 4
      OnClick = btnClearLogClick
    end
    object btnCaptureFrame: TButton
      Left = 783
      Top = 122
      Width = 91
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Capture Frame'
      TabOrder = 5
      OnClick = btnCaptureFrameClick
    end
    object btnSaveImage: TButton
      Left = 896
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save Image'
      TabOrder = 6
      OnClick = HandleSaveImageClick
    end
    object btnStartBurst: TButton
      Left = 94
      Top = 122
      Width = 113
      Height = 25
      Caption = 'Start Burst Capture'
      TabOrder = 7
      OnClick = HandleStartBurstCapture
    end
    object btnStopBurst: TButton
      Left = 229
      Top = 122
      Width = 113
      Height = 25
      Anchors = [akLeft]
      Caption = 'Stop Burst Capture'
      TabOrder = 8
      OnClick = HandleStopBurstCapture
    end
    object btnCopyLog: TButton
      Left = 1022
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Copy Log'
      TabOrder = 9
      OnClick = HandleCopyLog
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 161
    Width = 1205
    Height = 562
    Align = alClient
    TabOrder = 1
    object spResize: TSplitter
      Left = 605
      Top = 1
      Height = 560
      Align = alRight
      ResizeStyle = rsUpdate
      ExplicitLeft = 570
      ExplicitTop = 206
    end
    object grpVideoPreview: TGroupBox
      Left = 1
      Top = 1
      Width = 604
      Height = 560
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
      ExplicitWidth = 607
      object pnlVideo: TPanel
        Left = 2
        Top = 17
        Width = 600
        Height = 541
        Align = alClient
        BevelOuter = bvNone
        Color = clBlack
        DoubleBuffered = True
        ParentBackground = False
        ParentDoubleBuffered = False
        TabOrder = 0
        OnResize = pnlVideoResize
        ExplicitWidth = 603
      end
    end
    object grpFrameCapture: TGroupBox
      Left = 608
      Top = 1
      Width = 596
      Height = 560
      Align = alRight
      Caption = ' Frame Capture '
      Color = clBtnFace
      DefaultHeaderFont = False
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = 18
      HeaderFont.Name = 'Segoe UI'
      HeaderFont.Style = []
      HeaderFont.Quality = fqClearTypeNatural
      ParentBackground = False
      ParentColor = False
      TabOrder = 1
      object tcCapture: TPageControl
        Left = 2
        Top = 17
        Width = 592
        Height = 541
        ActivePage = tbFrame
        Align = alClient
        TabOrder = 0
        object tbFrame: TTabSheet
          Caption = 'Frame'
          object pnlFrameCapture: TPanel
            Left = 0
            Top = 0
            Width = 584
            Height = 511
            Align = alClient
            Caption = 'No Image. Waiting frame capture...'
            Color = clMedGray
            DoubleBuffered = False
            ParentBackground = False
            ParentDoubleBuffered = False
            TabOrder = 0
            ExplicitLeft = 328
            ExplicitTop = 288
            ExplicitWidth = 256
            ExplicitHeight = 223
            object picFrame: TImage
              Left = 1
              Top = 1
              Width = 582
              Height = 509
              Align = alClient
              AutoSize = True
              Center = True
              Proportional = True
              ExplicitLeft = 408
              ExplicitTop = 344
              ExplicitWidth = 170
              ExplicitHeight = 157
            end
          end
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
  object MainMenu1: TMainMenu
    Left = 336
    Top = 401
    object File1: TMenuItem
      Caption = 'File'
      object mnEdit: TMenuItem
        Caption = 'Exit'
        OnClick = OnExit
      end
    end
    object mnLogLevel: TMenuItem
      Caption = 'Log Level'
      object mnDebugLevel: TMenuItem
        Caption = 'Debug'
        OnClick = HandleLogLevelChange
      end
      object mnInfoLevel: TMenuItem
        Tag = 1
        Caption = 'Info'
        OnClick = HandleLogLevelChange
      end
      object mnWarningLevel: TMenuItem
        Tag = 2
        Caption = 'Warning'
        OnClick = HandleLogLevelChange
      end
      object mnErrorLevel: TMenuItem
        Tag = 3
        Caption = 'Error'
        OnClick = HandleLogLevelChange
      end
    end
  end
end
