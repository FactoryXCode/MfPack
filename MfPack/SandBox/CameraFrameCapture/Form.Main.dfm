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
  object Label5: TLabel
    Left = 464
    Top = 58
    Width = 508
    Height = 15
    Caption = 
      'Calculate the maximum frame rate currently readable from the cam' +
      'era at the selected resolution'
  end
  object pcSetup: TPageControl
    Left = 0
    Top = 0
    Width = 1213
    Height = 201
    ActivePage = tsSetup
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
        Left = 480
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
      object Label4: TLabel
        Left = 522
        Top = 139
        Width = 261
        Height = 15
        Caption = '(Full resolution image). Preview below is reduced.'
      end
      object btnCaptureFrame: TButton
        Left = 341
        Top = 131
        Width = 91
        Height = 25
        Caption = 'Capture Frame'
        TabOrder = 0
        OnClick = btnCaptureFrameClick
      end
      object btnClearLog: TButton
        Left = 1118
        Top = 129
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Clear Log'
        TabOrder = 1
        OnClick = btnClearLogClick
      end
      object btnCopyLog: TButton
        Left = 1030
        Top = 129
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
        Left = 442
        Top = 131
        Width = 75
        Height = 25
        Caption = 'Save Image'
        TabOrder = 4
        OnClick = HandleSaveImageClick
      end
      object btnToggleBurst: TButton
        Left = 216
        Top = 131
        Width = 113
        Height = 25
        Caption = 'Start Burst Capture'
        TabOrder = 5
        OnClick = HandleToggleBurst
      end
      object cboMethod: TComboBox
        Left = 94
        Top = 132
        Width = 105
        Height = 23
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
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
        TabOrder = 7
        OnChange = HandleSelectedDeviceChange
      end
      object cbxResolution: TComboBox
        Left = 545
        Top = 8
        Width = 288
        Height = 23
        TabOrder = 8
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
        TabOrder = 9
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
      object lblResolution: TLabel
        Left = 11
        Top = 116
        Width = 86
        Height = 15
        Caption = 'Resolution filter:'
      end
      object Label3: TLabel
        Left = 194
        Top = 116
        Width = 74
        Height = 15
        Caption = 'fps and above'
      end
      object lblMaxDesc: TLabel
        Left = 392
        Top = 50
        Width = 457
        Height = 15
        Caption = 
          'Estimate the maximum frame rate readable from the camera at the ' +
          'selected resolution.'
      end
      object lblMaxDesc2: TLabel
        Left = 392
        Top = 92
        Width = 448
        Height = 15
        Caption = 
          'Limiting factors include: CPU, GPU and for many web cameras - lo' +
          'w light conditions.'
      end
      object lblCurrentMethod: TLabel
        Left = 535
        Top = 29
        Width = 115
        Height = 15
        Caption = 'Capture method: N/A'
      end
      object lblMaxDesc1: TLabel
        Left = 392
        Top = 70
        Width = 237
        Height = 15
        Caption = 'This excludes time taken to return the image.'
      end
      object cbxPreviewType: TComboBox
        Left = 139
        Top = 20
        Width = 180
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
        Width = 180
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
      object cbxFrameRateMin: TComboBox
        Left = 139
        Top = 111
        Width = 49
        Height = 23
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 3
        Text = '24'
        OnChange = HandleMinimumFrameRateChange
        Items.Strings = (
          '10'
          '24'
          '29'
          '30'
          '60')
      end
      object btnCalculateMax: TButton
        Left = 392
        Top = 19
        Width = 129
        Height = 25
        Caption = 'Calculate Frame Rate'
        TabOrder = 4
        OnClick = HandleCalculateMax
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
