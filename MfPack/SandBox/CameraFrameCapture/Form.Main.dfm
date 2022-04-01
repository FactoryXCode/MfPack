object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Media Foundation Camera Capture Demo'
  ClientHeight = 777
  ClientWidth = 1212
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
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
    Width = 1212
    Height = 201
    ActivePage = tsSetup
    Align = alTop
    TabOrder = 0
    object tsSetup: TTabSheet
      Caption = 'Setup'
      DesignSize = (
        1204
        171)
      object lblLog: TLabel
        Left = 16
        Top = 73
        Width = 23
        Height = 15
        Caption = 'Log:'
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
      object lblMethod: TLabel
        Left = 14
        Top = 140
        Width = 45
        Height = 15
        Caption = 'Method:'
      end
      object lblSeconds: TLabel
        Left = 648
        Top = 141
        Width = 43
        Height = 15
        Caption = 'seconds'
      end
      object lblLogLevel: TLabel
        Left = 707
        Top = 141
        Width = 53
        Height = 15
        Caption = 'Log Level:'
      end
      object lblFPSDesc: TLabel
        Left = 929
        Top = 13
        Width = 74
        Height = 15
        Caption = 'fps and above'
      end
      object Label2: TLabel
        Left = 839
        Top = 13
        Width = 29
        Height = 15
        Caption = 'Show'
      end
      object btnClearLog: TButton
        Left = 1123
        Top = 130
        Width = 70
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Clear Log'
        TabOrder = 0
        OnClick = btnClearLogClick
      end
      object btnCopyLog: TButton
        Left = 1035
        Top = 130
        Width = 70
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Copy Log'
        TabOrder = 1
        OnClick = HandleCopyLog
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
      object cbxCaptureDevices: TComboBox
        Left = 94
        Top = 8
        Width = 259
        Height = 23
        DropDownCount = 30
        TabOrder = 3
        OnChange = HandleSelectedDeviceChange
      end
      object cbxResolution: TComboBox
        Left = 545
        Top = 8
        Width = 288
        Height = 23
        TabOrder = 4
        OnChange = cbxResolutionChange
      end
      object memLog: TMemo
        Left = 94
        Top = 37
        Width = 1098
        Height = 86
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 5
      end
      object cboMethod: TComboBox
        Left = 94
        Top = 133
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
      object btnToggleBurst: TButton
        Left = 472
        Top = 132
        Width = 113
        Height = 25
        Caption = 'Start Burst Capture'
        TabOrder = 7
        OnClick = HandleToggleBurst
      end
      object btnCaptureFrame: TButton
        Left = 349
        Top = 132
        Width = 105
        Height = 25
        Caption = 'Single Capture'
        TabOrder = 8
        OnClick = HandleCaptureFrame
      end
      object btnSaveImage: TButton
        Left = 944
        Top = 130
        Width = 70
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Save Image'
        TabOrder = 9
        OnClick = HandleSaveImageClick
      end
      object chkDisplayPreview: TCheckBox
        Left = 218
        Top = 137
        Width = 104
        Height = 17
        Caption = 'Display Preview'
        Checked = True
        State = cbChecked
        TabOrder = 10
        OnClick = HandlePreviewChange
      end
      object cbxDuration: TComboBox
        Left = 591
        Top = 133
        Width = 49
        Height = 23
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 11
        Text = '10'
        Items.Strings = (
          '10'
          '30'
          '60'
          '90'
          '120')
      end
      object cboLogLevel: TComboBox
        Left = 766
        Top = 133
        Width = 99
        Height = 23
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 12
        Text = 'Debug'
        OnChange = HandleLogLevelChange
        Items.Strings = (
          'Debug1'
          'Debug'
          'Info'
          'Warning'
          'Error')
      end
      object cbxFrameRateMin: TComboBox
        Left = 874
        Top = 8
        Width = 49
        Height = 23
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 13
        Text = '24'
        OnChange = HandleMinimumFrameRateChange
        Items.Strings = (
          '10'
          '24'
          '29'
          '30'
          '60')
      end
    end
    object tsDiagnostics: TTabSheet
      Caption = 'Diagnostics'
      ImageIndex = 2
      object lblCurrentMethod: TLabel
        Left = 159
        Top = 53
        Width = 115
        Height = 15
        Caption = 'Capture method: N/A'
      end
      object lblMaxDesc: TLabel
        Left = 16
        Top = 82
        Width = 457
        Height = 15
        Caption = 
          'Estimate the maximum frame rate readable from the camera at the ' +
          'selected resolution.'
      end
      object lblMaxDesc1: TLabel
        Left = 479
        Top = 82
        Width = 237
        Height = 15
        Caption = 'This excludes time taken to return the image.'
      end
      object lblMaxDesc2: TLabel
        Left = 16
        Top = 103
        Width = 448
        Height = 15
        Caption = 
          'Limiting factors include: CPU, GPU and for many web cameras - lo' +
          'w light conditions.'
      end
      object lblMaxTitle: TLabel
        Left = 16
        Top = 23
        Width = 511
        Height = 15
        Caption = 
          'Calculate the maximum frame rate currently readable from the cam' +
          'era at the selected resolution.'
      end
      object btnCalculateMax: TButton
        Left = 16
        Top = 46
        Width = 129
        Height = 25
        Caption = 'Calculate Frame Rate'
        TabOrder = 0
        OnClick = HandleCalculateMax
      end
    end
  end
  object tcCapture: TPageControl
    Left = 0
    Top = 201
    Width = 1212
    Height = 576
    ActivePage = tsFrame
    Align = alClient
    TabOrder = 1
    object tsFrame: TTabSheet
      Caption = 'Frame'
      object pbCapture: TPaintBox
        Left = 0
        Top = 0
        Width = 1204
        Height = 546
        Align = alClient
        OnPaint = HandleCapturePaint
        ExplicitLeft = 240
        ExplicitTop = 168
        ExplicitWidth = 105
        ExplicitHeight = 105
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
