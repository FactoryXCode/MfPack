object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Media Foundation Camera Capture Demo'
  ClientHeight = 751
  ClientWidth = 1213
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = HandlFormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pcSetup: TPageControl
    Left = 0
    Top = 0
    Width = 1213
    Height = 201
    ActivePage = tsAdvanced
    Align = alTop
    TabOrder = 0
    object tsSetup: TTabSheet
      Caption = 'Setup'
      DesignSize = (
        1205
        171)
      object lblLog: TLabel
        Left = 16
        Top = 73
        Width = 23
        Height = 15
        Caption = 'Log:'
      end
      object lblMethod: TLabel
        Left = 16
        Top = 140
        Width = 45
        Height = 15
        Caption = 'Method:'
      end
      object lblResoltution: TLabel
        Left = 448
        Top = 10
        Width = 59
        Height = 15
        Caption = 'Resolution:'
      end
      object lblSelectDevice: TLabel
        Left = 16
        Top = 13
        Width = 72
        Height = 15
        Caption = 'Select Device:'
      end
      object lblSupported: TLabel
        Left = 814
        Top = 13
        Width = 22
        Height = 15
        Caption = 'N/A'
      end
      object btnCaptureFrame: TButton
        Left = 831
        Top = 129
        Width = 91
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Capture Frame'
        TabOrder = 0
        OnClick = btnCaptureFrameClick
      end
      object btnClearLog: TButton
        Left = 1118
        Top = 130
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Clear Log'
        TabOrder = 1
        OnClick = btnClearLogClick
      end
      object btnCopyLog: TButton
        Left = 1030
        Top = 130
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Copy Log'
        TabOrder = 2
        OnClick = HandleCopyLog
      end
      object btnRefreshDevices: TButton
        Left = 359
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Refresh'
        TabOrder = 3
        OnClick = btnRefreshDevicesClick
      end
      object btnSaveImage: TButton
        Left = 949
        Top = 129
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Save Image'
        TabOrder = 4
        OnClick = HandleSaveImageClick
      end
      object btnStartBurst: TButton
        Left = 577
        Top = 129
        Width = 113
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Start Burst Capture'
        TabOrder = 5
        OnClick = HandleStartBurstCapture
      end
      object btnStopBurst: TButton
        Left = 704
        Top = 129
        Width = 113
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Stop Burst Capture'
        TabOrder = 6
        OnClick = HandleStopBurstCapture
      end
      object cboMethod: TComboBox
        Left = 94
        Top = 132
        Width = 105
        Height = 23
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 7
        Text = 'Synchronous'
        OnChange = HandleMethodChanged
        Items.Strings = (
          'Synchronous'
          'Asynchronous')
      end
      object cbxCaptureDevices: TComboBox
        Left = 94
        Top = 8
        Width = 259
        Height = 23
        DropDownCount = 30
        TabOrder = 8
        OnChange = HandleSelectedDeviceChange
      end
      object cbxResolution: TComboBox
        Left = 513
        Top = 8
        Width = 288
        Height = 23
        TabOrder = 9
        OnChange = cbxResolutionChange
      end
      object memLog: TMemo
        Left = 94
        Top = 37
        Width = 1099
        Height = 86
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 10
      end
    end
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object lblPreviewType: TLabel
        Left = 11
        Top = 24
        Width = 92
        Height = 15
        Caption = 'Capture Preview: '
      end
      object Label1: TLabel
        Left = 11
        Top = 55
        Width = 122
        Height = 15
        Caption = 'Direct2D render mode: '
      end
      object lblSeconds: TLabel
        Left = 194
        Top = 83
        Width = 43
        Height = 15
        Caption = 'seconds'
      end
      object Label2: TLabel
        Left = 11
        Top = 83
        Width = 78
        Height = 15
        Caption = 'Burst duration:'
      end
      object cbxPreviewType: TComboBox
        Left = 139
        Top = 20
        Width = 145
        Height = 23
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 0
        Text = 'Normal'
        OnChange = HandlePreviewTypeChange
        Items.Strings = (
          'None'
          'Normal')
      end
      object cbxRenderMode: TComboBox
        Left = 139
        Top = 49
        Width = 145
        Height = 23
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 1
        Text = 'Software'
        OnChange = HandleRenderModeChange
        Items.Strings = (
          'Default'
          'Hardware'
          'Software')
      end
      object cbxDuration: TComboBox
        Left = 139
        Top = 78
        Width = 49
        Height = 23
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = '10'
        Items.Strings = (
          '10'
          '30'
          '60'
          '90'
          '120')
      end
    end
  end
  object tcCapture: TPageControl
    Left = 0
    Top = 201
    Width = 1213
    Height = 550
    ActivePage = tsFrame
    Align = alClient
    TabOrder = 1
    ExplicitTop = 300
    ExplicitHeight = 451
    object tsFrame: TTabSheet
      Caption = 'Frame'
      object pnlFrameCapture: TPanel
        Left = 0
        Top = 0
        Width = 1205
        Height = 520
        Align = alClient
        Caption = 'No Image. Waiting frame capture...'
        Color = clMedGray
        DoubleBuffered = False
        ParentBackground = False
        ParentDoubleBuffered = False
        TabOrder = 0
        ExplicitWidth = 1203
        ExplicitHeight = 719
        object pbCapture: TPaintBox
          Left = 1
          Top = 1
          Width = 1203
          Height = 518
          Align = alClient
          OnPaint = HandleCapturePaint
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 1193
          ExplicitHeight = 528
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
